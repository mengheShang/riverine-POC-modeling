library(raster)
library(ncdf4)
library(sf)
library(sp)
library(dplyr)

# 加载文件
watershed_raster <- raster("D:\\POC research\\data\\1_HydroBasins_lev6\\HydroBasin_lev6_raster.tif")
nc_file <- "D:\\POC research\\data\\2_pco2Data\\ERA5-Land-monthly_China_1950_2022.nc"
stations <- read.csv("D:\\POC research\\data\\3_POC records\\Sampling_sites.csv")
coordinates(stations) <- ~Longitude + Latitude
proj4string(stations) <- CRS("+proj=longlat +datum=WGS84")

# 读取NetCDF文件
nc_data <- nc_open(nc_file)
precipitation_raw <- ncvar_get(nc_data, "tp")  
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")
time <- ncvar_get(nc_data, "time")  # 时间单位是小时，从1900-01-01 00:00:00开始
scale_factor <- ncatt_get(nc_data, "tp", "scale_factor")$value
add_offset <- ncatt_get(nc_data, "tp", "add_offset")$value
fill_value <- ncatt_get(nc_data, "tp", "_FillValue")$value
nc_close(nc_data)

# 转换时间为日期
time_dates <- as.POSIXct(time * 3600, origin="1900-01-01", tz="GMT")

# 应用缩放因子和偏移量
precipitation <- precipitation_raw
precipitation[precipitation_raw == fill_value] <- NA  # 使用 NA 替换缺失值
precipitation <- precipitation * 1000 # 单位转换

# 计算每个月的天数
days_in_month <- function(date) {
  y <- format(date, "%Y")
  m <- format(date, "%m")
  next_month <- as.Date(paste0(y, "-", as.integer(m) %% 12 + 1, "-01"))
  days <- as.integer(format(next_month - 1, "%d"))
  return(days)
}
days_in_months <- sapply(time_dates, days_in_month)

# 乘以每个月的天数
for (i in seq_along(time_dates)) {
  month_days <- days_in_months[i]
  precipitation[,,i] <- precipitation[,,i] * month_days
}

# 将降水数据转换为栅格堆栈
precip_stack <- stack()
for (i in 1:dim(precipitation)[3]) {
  cat("Processing layer", i, "of", dim(precipitation)[3], "\n")
  r <- raster(precipitation[,,i], xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84"))
  precip_stack <- addLayer(precip_stack, r)
}

# 统一范围、坐标系和分辨率
common_extent <- extent(
  max(extent(watershed_raster)@xmin, extent(precip_stack)@xmin),
  min(extent(watershed_raster)@xmax, extent(precip_stack)@xmax),
  max(extent(watershed_raster)@ymin, extent(precip_stack)@ymin),
  min(extent(watershed_raster)@ymax, extent(precip_stack)@ymax)
)

watershed_raster <- crop(watershed_raster, common_extent)
precip_stack <- crop(precip_stack, common_extent)

watershed_raster <- resample(watershed_raster, precip_stack, method = "ngb")

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

# 创建寻找最近非空precip的函数
# 获取非空precip的函数
find_non_na_precip_values <- function(watershed_raster, precip_stack, point) {
  nearest_cell <- find_nearest_non_na(watershed_raster, point)
  watershed_id <- watershed_raster[nearest_cell]
  
  mask <- watershed_raster[] == watershed_id
  indices <- which(mask)
  
  precip_values <- extract(precip_stack, indices, df = TRUE)
  
  # 如果precip值为空，扩大搜索范围
  radius <- 1
  while (all(is.na(precip_values))) {
    radius <- radius + 1
    cells <- get_adjacent_cells(watershed_raster, nearest_cell, radius)
    for (cell in cells) {
      new_watershed_id <- watershed_raster[cell]
      if (!is.na(new_watershed_id) && new_watershed_id != watershed_id) {
        mask <- watershed_raster[] == new_watershed_id
        indices <- which(mask)
        new_precip_values <- extract(precip_stack, indices, df = TRUE)
        if (!all(is.na(new_precip_values))) {
          precip_values <- new_precip_values
          break
        }
      }
    }
  }
  
  return(precip_values)
}

# 创建处理一批站点的函数
process_stations <- function(start_index, end_index) {
  results <- data.frame()
  
  for (i in start_index:end_index) {
    lon <- stations$Longitude[i]
    lat <- stations$Latitude[i]
    
    print(i)
    
    station_point <- SpatialPoints(coords = matrix(c(lon, lat), ncol = 2), proj4string = crs(watershed_raster))
    
    cell <- cellFromXY(watershed_raster, coordinates(station_point))
    watershed_id <- watershed_raster[cell]
    
    if (is.na(watershed_id) || watershed_id == 0) {
      print(paste("Station", i, "has no valid id at the cell, finding nearest non-NA cell."))
      cell <- find_nearest_non_na(watershed_raster, station_point)
      watershed_id <- watershed_raster[cell]
    }
    
    print(paste("Watershed ID:", watershed_id))
    
    if (!is.na(watershed_id)) {
      mask <- watershed_raster[] == watershed_id
      indices <- which(mask)
      
      print(indices)
      
      precip_values <- extract(precip_stack, indices, df = TRUE)
      
      if (!all(is.na(precip_values))) {
        precip <- colMeans(precip_values[, -1], na.rm = TRUE)
        
        print(precip)
        
        time <- time_dates
        
        if (length(precip) > 0) {
          temp_result <- data.frame(Station = stations$ID[i],
                                    Time = time,
                                    Precipitation = precip)
          
          results <- rbind(results, temp_result)
        }
      }
    }
  }
  
  return(results)
}

# 创建输出处理结果的函数
process_batch <- function(start_index, batch_size) {
  end_index <- min(start_index + batch_size - 1, nrow(stations))
  batch_results <- process_stations(start_index, end_index)
  
  file_path <- "D:\\POC research\\data\\pco2Data_HydroBasins\\precip_6.csv"
  write.csv(batch_results, file_path, row.names = FALSE)
  
  return(batch_results)
}

batch_size <- 211
start_index <- 1

results <- process_batch(start_index, batch_size)

print(results)