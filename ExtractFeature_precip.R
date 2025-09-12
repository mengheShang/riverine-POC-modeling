library(terra)
library(raster)
library(ncdf4)
library(sp)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m"
nc_file <- "E:/POC research/data/2_pco2Data/ERA5-Land-monthly_China_1950_2022.nc"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/prec.csv"

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

# 维度调换
precipitation <- aperm(precipitation, c(2, 1, 3))

# 定义一个函数来提取并计算流域内prec的平均值
extract_basin_precip <- function(station, lon, lat, precipitation, time_dates, basin_tiff_file) {
  
  # 根据采样时间找到对应的时间索引
  year_month <- format(as.Date(station$Sampling_time), "%Y-%m")
  time_index <- which(format(time_dates, "%Y-%m") == year_month)
  
  cat("prcessing station:", station$ID, "time", year_month,"\n")
  
  if (length(time_index) == 0) {
    stop(paste("No precipitation data available for the date", year_month))
  }
  
  # 提取该时间点的prec数据
  precip_layer <- precipitation[,,time_index]
  
  # 创建一个栅格对象
  precip_rast <- rast(precip_layer, extent = ext(c(min(lon), max(lon), min(lat), max(lat))), crs = "+proj=longlat +datum=WGS84")
  plot(precip_rast)
  
  # 读取站点对应的流域TIF文件
  basin_tiff <- rast(basin_tiff_file)
  
  # 裁剪温度栅格到流域范围并计算流域内的prec平均值
  basin_tiff <- project(basin_tiff, crs(precip_rast))
  precip_rast <- resample(precip_rast, basin_tiff, method = "near")
  precip_clipped <- crop(precip_rast, basin_tiff)
  basin_tiff <- crop(basin_tiff, precip_rast)
  print(ext(precip_clipped))
  print(ext(basin_tiff))
  plot(precip_clipped)
  
  # 动态生成文件名
#  output_tiff_filename <- paste0("E:/POC research/data/2_pco2Data/precipitation_", station$ID, "_", year_month, ".tif")
#  writeRaster(precip_rast, output_tiff_filename, overwrite = TRUE)
  
  precip_masked <- mask(precip_clipped, basin_tiff)
  plot(precip_masked, main = paste("Precipitation for station ID:", station$ID, "on", year_month))
  
  # 计算平均值
  average_val <- global(precip_masked, "mean", na.rm = TRUE)$mean

  return(average_val)
}

# 初始化一个结果数据框
precip_results <- data.frame()
# 对所有站点计算prec平均值
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if(station_id < 864)next
  sampling_time <- as.Date(stations$Sampling_time[i])
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  
  if (file.exists(basin_tiff_file)) {
    station_precip <- extract_basin_precip(stations[i,], lon, lat, precipitation, time_dates, basin_tiff_file)
    station_results <- data.frame(
      ID = station_id,
      Sampling_time = format(sampling_time, "%Y-%m"),
      Precipitation = station_precip
    )
    precip_results <- bind_rows(precip_results, station_results)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
  gc()  # 垃圾回收，释放内存
}

# 查看结果
print(precip_results)

# 写入csv文件
write_csv(precip_results, output_path)
