library(raster)
library(sf)
library(sp)
library(dplyr)

# 加载文件
watershed_raster <- raster("D:\\POC research\\data\\HydroBasins_lev6\\HydroBasin_lev6_raster.tif")
npp_files <- list.files(path = "D:\\POC research\\data\\pco2Data\\npp", pattern = "_new\\.tif$", full.names = TRUE)
npp_stack <- stack(npp_files)
stations <- read.csv("D:\\POC research\\data\\Sites position.csv")
coordinates(stations) <- ~Longitude + Latitude
proj4string(stations) <- CRS("+proj=longlat +datum=WGS84")

# 统一范围、坐标系和分辨率
common_extent <- extent(
  max(extent(watershed_raster)@xmin, extent(npp_stack)@xmin),
  min(extent(watershed_raster)@xmax, extent(npp_stack)@xmax),
  max(extent(watershed_raster)@ymin, extent(npp_stack)@ymin),
  min(extent(watershed_raster)@ymax, extent(npp_stack)@ymax)
)

watershed_raster <- crop(watershed_raster, common_extent)
npp_stack <- crop(npp_stack, common_extent)

watershed_raster <- resample(watershed_raster, npp_stack, method = "ngb")

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

# 处理一批站点的函数
process_stations <- function(start_index, end_index) {
  results <- data.frame()
  
  for (i in start_index:end_index) {
    lon <- stations$Longitude[i]
    lat <- stations$Latitude[i]
    
    print(i)
    
    # 创建一个点
    station_point <- SpatialPoints(coords = matrix(c(lon, lat), ncol = 2), proj4string = crs(watershed_raster))
    
    # 找到最近的栅格点
    nearest_cell <- find_nearest_non_na(watershed_raster, station_point)
    watershed_id <- watershed_raster[nearest_cell]
    
    print(watershed_id)
    
    if (!is.na(watershed_id)) {
      # 找到所有属于这个流域ID的栅格点
      mask <- watershed_raster[] == watershed_id
      indices <- which(mask)
      
      print(indices)
      
      # 提取NPP值
      npp_values <- extract(npp_stack, indices, df = TRUE)
      
      if (!all(is.na(npp_values))) {
        # 计算平均NPP
        npp <- colMeans(npp_values[, -1], na.rm = TRUE)  # 排除第一列索引
        
        print(npp)
        
        # 手动设置年份信息
        years <- 2001:2022
        
        if (length(npp) > 0) {
          # 创建临时数据框
          temp_result <- data.frame(Station = stations$ID[i],
                                    Time = years,
                                    NPP = npp)
          
          # 合并结果
          results <- rbind(results, temp_result)
        }
      }
    }
  }
  
  return(results)
}

# 处理一部分站点
process_batch <- function(start_index, batch_size) {
  end_index <- min(start_index + batch_size - 1, nrow(stations))
  batch_results <- process_stations(start_index, end_index)
  
  # 如果结果文件存在，则读取现有结果
  file_path <- "D:\\POC research\\data\\pco2Data_HydroBasins\\npp_6.csv"
  if (file.exists(file_path)) {
    existing_results <- read.csv(file_path)
    # 覆盖现有数据
    for (i in 1:nrow(batch_results)) {
      station_id <- batch_results$Station[i]
      existing_results <- existing_results[existing_results$Station != station_id, ]
    }
    combined_results <- rbind(existing_results, batch_results)
  } else {
    combined_results <- batch_results
  }
  
  # 输出合并结果为CSV文件
  write.csv(combined_results, file_path, row.names = FALSE)
  
  # 返回当前处理的结果
  return(batch_results)
}

# 设置每次处理的站点数量
batch_size <- 211
# 处理第一批站点
start_index <- 1

# 调用处理函数处理一部分站点
results <- process_batch(start_index, batch_size)

# 打印当前处理的结果
print(results)

