#######
library(terra)
library(raster)
library(ncdf4)
library(sp)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "D:/POC research/data/3_POC records/monthly_records.xlsx"
basin_tiff_dir <- "D:/POC research/data/1_DEM_watershed/watershed_2"
nc_file <- "D:/POC research/data/2_pco2Data/ERA5-Land-monthly_China_1950_2022.nc"
output_path <- "D:/POC research/data/2_inputData_watershed/precipitation_single_station.csv"

# 读取站点数据
stations <- read_excel(stations_path)
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

# 提取并计算流域内降水的平均值
extract_basin_precip_single_station <- function(station_id, specified_time, lon, lat, precipitation, time_dates, basin_tiff_dir, stations) {
  
  # 获取对应站点数据
  station <- stations[stations$ID == station_id,]
  
  # 转换指定的采样时间为日期格式
  specified_time <- as.Date(specified_time)
  
  # 根据指定时间找到对应的时间索引
  year_month <- format(specified_time, "%Y-%m")
  time_index <- which(format(time_dates, "%Y-%m") == year_month)
  
  cat("Processing station:", station_id, "for time:", year_month, "\n")
  
  if (length(time_index) == 0) {
    stop(paste("No precipitation data available for the date", year_month))
  }
  
  # 提取该时间点的降水数据
  precip_layer <- precipitation[,,time_index]
  
  # 创建一个栅格对象
  precip_rast <- rast(precip_layer, extent = ext(c(min(lon), max(lon), min(lat), max(lat))), crs = "+proj=longlat +datum=WGS84")
  
  # 读取站点对应的流域TIF文件
  basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))
  
  if (!file.exists(basin_tiff_file)) {
    stop(paste("TIF file for station ID", station_id, "not found."))
  }
  
  basin_tiff <- rast(basin_tiff_file)
  
  # 裁剪降水栅格到流域范围并计算流域内的降水平均值
  precip_rast <- project(precip_rast, crs(basin_tiff))
  precip_rast <- resample(precip_rast, basin_tiff, method = "near")
  precip_clipped <- crop(precip_rast, basin_tiff)
  basin_tiff <- crop(basin_tiff, precip_clipped)
  
  precip_masked <- mask(precip_clipped, basin_tiff)
  
  # 绘制流域TIF和掩膜后的降水栅格
  par(mfrow = c(1, 2))  # 设置画布为两列
  plot(basin_tiff, main = paste("Basin for Station", station_id))
  plot(precip_clipped, main = paste("Precipitation for", station_id))
  
  print(ext(precip_clipped))
  print(ext(basin_tiff))
  print(crs(precip_clipped))
  print(crs(basin_tiff))
  print(res(precip_clipped))
  print(res(basin_tiff))
  
  print(global(basin_tiff, "sum", na.rm = TRUE))
  print(global(precip_masked, "sum", na.rm = TRUE))
  # 计算平均降水量
  average_val <- global(precip_masked, "mean", na.rm = TRUE)$mean
  
  return(data.frame(ID = station_id, Sampling_time = year_month, precip = average_val))
}

# 使用指定的站点ID和采样时间进行提取
station_id <- 5  # 修改为你想要提取的站点ID
specified_time <- "2008-08-01"  # 指定采样时间

# 提取单个站点的流域降水数据
precip_single_station <- extract_basin_precip_single_station(station_id, specified_time, lon, lat, precipitation, time_dates, basin_tiff_dir, stations)

# 查看结果
print(precip_single_station)

# 写入csv文件
write_csv(precip_single_station, output_path)

#######
# 加载必要的库
library(terra)
library(ncdf4)

# 输入文件路径
nc_file <- "D:\\POC research\\data\\2_pco2Data\\ERA5-Land-monthly_China_1950_2022.nc"
output_tiff_path <- "D:/POC research/data/2_pco2Data/precipitation_200808.tif"

# 读取NetCDF文件
nc_data <- nc_open(nc_file)
precipitation_raw <- ncvar_get(nc_data, "tp")  # 降水量
lon <- ncvar_get(nc_data, "longitude")  # 经度
lat <- ncvar_get(nc_data, "latitude")  # 纬度
time <- ncvar_get(nc_data, "time")  # 时间，单位是小时
scale_factor <- ncatt_get(nc_data, "tp", "scale_factor")$value  # 缩放因子
add_offset <- ncatt_get(nc_data, "tp", "add_offset")$value  # 偏移量
fill_value <- ncatt_get(nc_data, "tp", "_FillValue")$value  # 缺失值
nc_close(nc_data)

# 转换时间为日期
time_dates <- as.POSIXct(time * 3600, origin = "1900-01-01", tz = "GMT")

# 应用缩放因子和偏移量
precipitation <- precipitation_raw
precipitation[precipitation_raw == fill_value] <- NA  # 替换缺失值为 NA
precipitation <- precipitation * 1000  # 假设单位转换为毫米

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

precipitation <- aperm(precipitation, c(2, 1, 3))
  
# 指定要提取的年份和月份
year_to_extract <- 2008
month_to_extract <- 8
target_year_month <- paste0(year_to_extract, "-", sprintf("%02d", month_to_extract))

# 找到对应的年月索引
time_index <- which(format(time_dates, "%Y-%m") == target_year_month)

# 提取该月份的降水数据
precip_layer <- precipitation[,,time_index]

# 创建栅格对象
precip_rast <- rast(precip_layer, 
                    extent = ext(c(min(lon), max(lon), min(lat), max(lat))),
                    crs = "+proj=longlat +datum=WGS84")

# 绘制栅格图
plot(precip_rast)

# 保存为TIFF文件
writeRaster(precip_rast, output_tiff_path, overwrite = TRUE)
