######

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
output_path <- "D:/POC research/data/2_inputData_watershed/temperature_single_station.csv"

# 读取站点数据
stations <- read_excel(stations_path)
coordinates(stations) <- ~Longitude + Latitude
proj4string(stations) <- CRS("+proj=longlat +datum=WGS84")

# 读取NetCDF文件
nc_data <- nc_open(nc_file)
temperature_raw <- ncvar_get(nc_data, "tp")  
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
temperature <- temperature_raw
temperature[temperature_raw == fill_value] <- NA  # 使用 NA 替换缺失值
temperature <- temperature * 1000 # 单位转换

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
  temperature[,,i] <- temperature[,,i] * month_days
}

# 提取并计算流域内温度的平均值
extract_basin_temp_single_station <- function(station_id, lon, lat, temperature, time_dates, basin_tiff_dir, stations) {
  
  # 获取对应站点数据
  station <- stations[stations$ID == station_id,]
  
  # 根据采样时间找到对应的时间索引
  year_month <- format(as.Date(station$Sampling_time), "%Y-%m")
  time_index <- which(format(time_dates, "%Y-%m") == year_month)
  
  cat("Processing station:", station_id, "for time:", year_month, "\n")
  
  if (length(time_index) == 0) {
    stop(paste("No temperature data available for the date", year_month))
  }
  
  # 提取该时间点的温度数据
  temp_layer <- temperature[,,time_index]
  
  # 创建一个栅格对象
  temp_rast <- rast(temp_layer, extent = ext(c(min(lon), max(lon), min(lat), max(lat))), crs = "+proj=longlat +datum=WGS84")
  
  # 读取站点对应的流域TIF文件
  basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))
  
  if (!file.exists(basin_tiff_file)) {
    stop(paste("TIF file for station ID", station_id, "not found."))
  }
  
  basin_tiff <- rast(basin_tiff_file)
  
  # 裁剪温度栅格到流域范围并计算流域内的温度平均值
  basin_tiff <- project(basin_tiff, crs(temp_rast))
  temp_rast <- resample(temp_rast, basin_tiff, method = "near")
  temp_clipped <- crop(temp_rast, basin_tiff)
  basin_tiff <- crop(basin_tiff, temp_rast)
  
  temp_masked <- mask(temp_clipped, basin_tiff)
  
  # 计算平均温度量
  average_val <- global(temp_masked, "mean", na.rm = TRUE)$mean
  
  return(data.frame(ID = station_id, Sampling_time = year_month, temp = average_val))
}

# 使用指定的站点ID
station_id <- 68  # 修改为你想要提取的站点ID

# 提取单个站点的流域温度数据
temp_single_station <- extract_basin_temp_single_station(station_id, lon, lat, temperature, time_dates, basin_tiff_dir, stations)

# 查看结果
print(temp_single_station)

# 写入csv文件
write_csv(temp_single_station, output_path)


######
# 加载必要的库
library(terra)
library(ncdf4)

# 输入文件路径
nc_file <- "D:/POC research/data/2_pco2Data/ERA5-Land-monthly_China_1950_2022.nc"
output_tiff_path <- "D:/POC research/data/2_pco2Data/temperature_200808.tif"

# 读取NetCDF文件
nc_data <- nc_open(nc_file)
temperature_raw <- ncvar_get(nc_data, "tp")  # 温度
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
temperature <- temperature_raw
temperature[temperature_raw == fill_value] <- NA  # 替换缺失值为 NA
temperature <- temperature * 1000  # 假设单位转换为毫米

# 定义计算每个月天数的函数
days_in_month <- function(date) {
  y <- format(date, "%Y")
  m <- format(date, "%m")
  next_month <- as.Date(paste0(y, "-", as.integer(m) %% 12 + 1, "-01"))
  days <- as.integer(format(next_month - 1, "%d"))
  return(days)
}

# 获取年月索引
year_month <- format(time_dates, "%Y-%m")
unique_year_month <- unique(year_month)

# 逐月累积温度数据
monthly_temperature <- array(NA, dim = c(dim(temperature_raw)[1], dim(temperature_raw)[2], length(unique_year_month)))

for (i in seq_along(unique_year_month)) {
  # 获取当前月份的所有时间索引
  month_indices <- which(year_month == unique_year_month[i])
  
  # 计算该月的天数
  days <- days_in_month(time_dates[month_indices[1]])
  
  # 累积该月的温度并乘以天数
  monthly_temperature[,,i] <- apply(temperature[,,month_indices], c(1, 2), sum) * days
}

# 指定要提取的年份和月份
year_to_extract <- 2008
month_to_extract <- 8
target_year_month <- paste0(year_to_extract, "-", sprintf("%02d", month_to_extract))

# 找到对应的年月索引
time_index <- which(unique_year_month == target_year_month)

if (length(time_index) == 0) {
  stop(paste("No temperature data available for", target_year_month))
}

# 提取该月份的温度数据
precip_layer <- monthly_temperature[,,time_index]

# 创建栅格对象
precip_rast <- rast(precip_layer, 
                    extent = ext(c(min(lon), max(lon), min(lat), max(lat))),
                    crs = "+proj=longlat +datum=WGS84")

# 保存为TIFF文件
writeRaster(precip_rast, output_tiff_path, format = "GTiff", overwrite = TRUE)

# 输出完成信息
cat("temperature data for", year_to_extract, "-", month_to_extract, "saved as", output_tiff_path, "\n")
