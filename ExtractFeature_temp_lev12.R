library(raster)
library(ncdf4)
library(sf)
library(sp)
library(dplyr)

# 加载文件
watershed_raster <- raster("D:\\POC research\\data\\HydroBasins_lev12\\HydroBasins_lev12_raster.tif")
nc_file <- "D:\\POC research\\data\\pco2Data\\ERA5-Land-monthly_China_1950_2022.nc"
stations <- read.csv("D:\\POC research\\data\\Sites position.csv")
coordinates(stations) <- ~Longitude + Latitude
proj4string(stations) <- CRS("+proj=longlat +datum=WGS84")

# 读取NetCDF文件
nc_data <- nc_open(nc_file)
temperature_raw <- ncvar_get(nc_data, "t2m")  
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")
time <- ncvar_get(nc_data, "time")  # 时间单位是小时，从1900-01-01 00:00:00开始
scale_factor <- ncatt_get(nc_data, "t2m", "scale_factor")$value
add_offset <- ncatt_get(nc_data, "t2m", "add_offset")$value
fill_value <- ncatt_get(nc_data, "t2m", "_FillValue")$value
nc_close(nc_data)

# 转换时间为日期
time_dates <- as.POSIXct(time * 3600, origin="1900-01-01", tz="GMT")

# 应用缩放因子和偏移量
temperature <- temperature_raw
temperature[temperature_raw == fill_value] <- NA  # 使用 NA 替换缺失值
#temperature1 <- temperature * scale_factor + add_offset  # 应用缩放因子和偏移量
temperature <- temperature - 273.15  # 转换为摄氏度

# 将降水数据转换为栅格堆栈
temp_stack <- stack()
for (i in 1:dim(temperature)[3]) {
  cat("Processing layer", i, "of", dim(temperature)[3], "\n")
  r <- raster(temperature[,,i], xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84"))
  temp_stack <- addLayer(temp_stack, r)
}

# 统一范围、坐标系和分辨率
common_extent <- extent(
  max(extent(watershed_raster)@xmin, extent(temp_stack)@xmin),
  min(extent(watershed_raster)@xmax, extent(temp_stack)@xmax),
  max(extent(watershed_raster)@ymin, extent(temp_stack)@ymin),
  min(extent(watershed_raster)@ymax, extent(temp_stack)@ymax)
)

watershed_raster <- crop(watershed_raster, common_extent)
temp_stack <- crop(temp_stack, common_extent)

watershed_raster <- resample(watershed_raster, temp_stack, method = "ngb")

# 创建寻找最近非空栅格值的函数
get_row_col <- function(raster, cell) {
  row_col <- rowColFromCell(raster, cell)
  return(list(row = row_col[1], col = row_col[2]))
}

get_adjacent_cells <- function(raster, cell, radius) {
  dim_raster <- dim(raster)  # 获取栅格的维度（行和列数）
  row_col <- get_row_col(raster, cell)  # 获取当前单元的行和列号
  row <- row_col$row
  col <- row_col$col
  
  adjacent_cells <- c()  # 初始化相邻单元的列表
  
  for (i in -radius:radius) {
    for (j in -radius:radius) {
      if (i == 0 && j == 0) next  # 跳过中心单元
      new_row <- max(1, min(row + i, dim_raster[1]))
      new_col <- max(1, min(col + j, dim_raster[2]))
      new_cell <- cellFromRowCol(raster, new_row, new_col)  # 获取新的单元号
      adjacent_cells <- c(adjacent_cells, new_cell)  # 添加到相邻单元的列表中
    }
  }
  return(adjacent_cells)
}

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

# 创建寻找最近非空temp的函数
find_non_na_temp_values <- function(watershed_raster, temp_stack, point) {
  nearest_cell <- find_nearest_non_na(watershed_raster, point)
  watershed_id <- watershed_raster[nearest_cell]
  
  mask <- watershed_raster[] == watershed_id
  indices <- which(mask)
  
  temp_values <- extract(temp_stack, indices, df = TRUE)
  
  # 如果temp值为空，扩大搜索范围
  radius <- 1
  while (all(is.na(temp_values))) {
    radius <- radius + 1
    cells <- get_adjacent_cells(watershed_raster, nearest_cell, radius)
    for (cell in cells) {
      new_watershed_id <- watershed_raster[cell]
      if (!is.na(new_watershed_id) && new_watershed_id != watershed_id) {
        mask <- watershed_raster[] == new_watershed_id
        indices <- which(mask)
        new_temp_values <- extract(temp_stack, indices, df = TRUE)
        if (!all(is.na(new_temp_values))) {
          temp_values <- new_temp_values
          break
        }
      }
    }
  }
  
  return(temp_values)
}

# 创建处理一批站点的函数
process_stations <- function(start_index, end_index) {
  results <- data.frame()
  
  for (i in start_index:end_index) {
    lon <- stations$Longitude[i]
    lat <- stations$Latitude[i]
    
    print(i)
    
    station_point <- SpatialPoints(coords = matrix(c(lon, lat), ncol = 2), proj4string = crs(watershed_raster))
    
    temp_values <- find_non_na_temp_values(watershed_raster, temp_stack, station_point)
    temp <- colMeans(temp_values[, -1], na.rm = TRUE)
    
    print(temp)
    
    time <- time_dates
    if (length(temp) > 0) {
      temp_result <- data.frame(Station = stations$ID[i],
                                Time = time,
                                Temperature = temp)
      
      results <- rbind(results, temp_result)
    }
  }
  
  return(results)
}

# 创建输出处理结果的函数
process_batch <- function(start_index, batch_size) {
  end_index <- min(start_index + batch_size - 1, nrow(stations))
  batch_results <- process_stations(start_index, end_index)
  
  file_path <- "D:\\POC research\\data\\pco2Data_HydroBasins\\temp_12.csv"
  write.csv(batch_results, file_path, row.names = FALSE)
  
  return(batch_results)
}

# 处理一批信息
batch_size <- 211
start_index <- 1

results <- process_batch(start_index, batch_size)

print(results)