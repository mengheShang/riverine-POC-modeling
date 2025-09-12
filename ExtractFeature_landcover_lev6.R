library(raster)
library(sf)
library(sp)
library(dplyr)

# 加载文件
stations <- read.csv("D:\\POC research\\data\\Sites position.csv")
coordinates(stations) <- ~Longitude + Latitude
proj4string(stations) <- CRS("+proj=longlat +datum=WGS84")
watershed_raster <- raster("D:\\POC research\\data\\HydroBasins_lev6\\HydroBasin_lev6_raster.tif")

# 获取所有landcover文件路径
landcover_files <- list.files(path = "D:\\POC research\\data\\pco2Data\\land_covernew", pattern = "_1\\.tif$", full.names = TRUE)
landcover_stack <- stack(landcover_files)

# 加载1985年的landcover数据
landcover_1985 <- raster("D:\\POC research\\data\\pco2Data\\land_covernew\\1985_1.tif")

# 将1985-1989年的栅格对象添加到栈中
landcover_stack <- stack(landcover_1985, landcover_1985, landcover_1985, landcover_1985, landcover_stack)

# 统一范围、坐标系和分辨率
common_extent <- extent(
  max(extent(watershed_raster)@xmin, extent(landcover_stack)@xmin),
  min(extent(watershed_raster)@xmax, extent(landcover_stack)@xmax),
  max(extent(watershed_raster)@ymin, extent(landcover_stack)@ymin),
  min(extent(watershed_raster)@ymax, extent(landcover_stack)@ymax)
)

watershed_raster <- crop(watershed_raster, common_extent)
landcover_stack <- crop(landcover_stack, common_extent)

watershed_raster <- resample(watershed_raster, landcover_stack, method = "ngb")

# 创建一个函数来找到最近的非空栅格值
# 获取栅格的行列号
get_row_col <- function(raster, cell) {
  row_col <- rowColFromCell(raster, cell)
  return(list(row = row_col[1], col = row_col[2]))
}

# 获取相邻单元
get_adjacent_cells <- function(raster, cell, radius) {
  dim_raster <- dim(raster)  # 获取栅格的维度（行和列数）
  row_col <- get_row_col(raster, cell)  # 获取当前单元的行和列号
  row <- row_col$row
  col <- row_col$col
  
  adjacent_cells <- c()  # 初始化相邻单元的列表
  
  for (i in -radius:radius) {
    for (j in -radius:radius) {
      if (i == 0 && j == 0) next  # 跳过中心单元
      new_row <- row + i  # 计算新的行号
      new_col <- col + j  # 计算新的列号
      # 检查新的行和列号是否在栅格范围内
      if (new_row > 0 && new_row <= dim_raster[1] && new_col > 0 && new_col <= dim_raster[2]) {
        new_cell <- cellFromRowCol(raster, new_row, new_col)  # 获取新的单元号
        adjacent_cells <- c(adjacent_cells, new_cell)  # 添加到相邻单元的列表中
      }
    }
  }
  
  return(adjacent_cells)
}

# 寻找最近非空栅格值
find_nearest_non_na <- function(raster, point) {
  cell <- cellFromXY(raster, coordinates(point))
  if (!is.na(raster[cell])) {
    return(cell)
  }
  
  radius <- 1
  while (TRUE) {
    cells <- get_adjacent_cells(raster, cell, radius)
    non_na_cells <- cells[!is.na(raster[cells])]
    if (length(non_na_cells) > 0) {
      return(non_na_cells[1])
    }
    radius <- radius + 1
  }
}

# 提取信息的函数
process_stations <- function(start_index, end_index) {
  results <- data.frame(Station = integer(), Time = integer(), Landcover = integer(), Percentage = numeric())
  
  for (i in start_index:end_index) {
    lon <- stations$Longitude[i]
    lat <- stations$Latitude[i]
    
    print(paste("Processing station:", i, "Longitude:", lon, "Latitude:", lat))
    
    # 创建一个点
    station_point <- SpatialPoints(coords = matrix(c(lon, lat), ncol = 2), proj4string = crs(watershed_raster))
    
    # 找到站点所在的栅格点
    cell <- cellFromXY(watershed_raster, coordinates(station_point))
    watershed_id <- watershed_raster[cell]
    
    # 如果站点所在栅格点没有有效id，寻找最近的非空栅格点
    if (is.na(watershed_id) || watershed_id == 0) {
      print(paste("Station", i, "has no valid id at the cell, finding nearest non-NA cell."))
      cell <- find_nearest_non_na(watershed_raster, station_point)
      watershed_id <- watershed_raster[cell]
    }
    
    print(paste("Watershed ID:", watershed_id))
    
    if (!is.na(watershed_id) && watershed_id != 0) {
      # 找到所有属于这个流域ID的栅格点
      mask <- watershed_raster[] == watershed_id
      indices <- which(mask)
      
      print(paste("Indices in watershed:", length(indices)))
      
      # 提取landcover值
      lc_values <- extract(landcover_stack, indices, df = TRUE)
      
      if (!all(is.na(lc_values))) {
        # 手动设置年份信息
        years <- 1985:2022
        
        for (j in 1:(ncol(lc_values) - 1)) {  # 对每个年份
          lc_year <- lc_values[, j + 1]  # 排除第一列索引
          
          # 计算出现最多的landcover类型及其所占百分比
          lc_table <- table(lc_year)
          most_frequent_lc <- as.integer(names(lc_table)[which.max(lc_table)])
          percentage <- max(lc_table) / sum(lc_table) * 100
          
          # 添加到临时数据框
          temp_result <- data.frame(Station = stations$ID[i],
                                    Time = years[j],
                                    Landcover = most_frequent_lc,
                                    Percentage = percentage)
          
          # 合并结果
          results <- rbind(results, temp_result)
          
          print(paste("Current results for station:", i, "Year:", years[j], "Landcover:", most_frequent_lc, "Percentage:", percentage))
        }
      }
    } else {
      print(paste("Invalid watershed_id for station:", i))
    }
  }
  
  return(results)
}

# 处理一部分站点的函数
process_batch <- function(start_index, batch_size) {
  end_index <- min(start_index + batch_size - 1, nrow(stations))
  batch_results <- process_stations(start_index, end_index)
  file_path <- "D:\\POC research\\data\\pco2Data_HydroBasins\\landcover_6.csv"
  write.csv(batch_results, file_path, row.names = FALSE)
  return(batch_results)
}

# 设置每次处理的站点数量
batch_size <- 211
# 处理第一批站点
start_index <- 1
# 调用函数处理一部分站点
final_results <- process_batch(start_index, batch_size)

# 打印当前处理的结果
print(final_results)
