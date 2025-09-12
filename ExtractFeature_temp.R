library(terra)
library(ncdf4)
library(sp)
library(readxl)
library(readr)
library(dplyr)

# 输入文件路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m"
nc_file <- "E:/POC research/data/2_pco2Data/ERA5-Land-monthly_China_1950_2022.nc"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/temp.csv"

# 读取站点数据
stations <- read_excel(stations_path)
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
temperature <- temperature - 273.15  # 转换为摄氏度

temperature <- aperm(temperature, c(2, 1, 3))

# 定义一个函数来提取并计算流域内温度的平均值
extract_basin_temp <- function(station, lon, lat, temperature, time_dates, basin_tiff_file) {
  
  # 根据采样时间找到对应的时间索引
  year_month <- format(as.Date(station$Sampling_time), "%Y-%m")
  time_index <- which(format(time_dates, "%Y-%m") == year_month)
  
  cat("Processing station:", station$ID, "time", year_month,"\n")
  
  if (length(time_index) == 0) {
    stop(paste("No temperature data available for the date", year_month))
  }
  
  # 提取该时间点的温度数据
  temp_layer <- temperature[,,time_index]
  
  # 创建一个栅格对象
  temp_rast <- rast(temp_layer, extent = ext(c(min(lon), max(lon), min(lat), max(lat))), crs = "+proj=longlat +datum=WGS84")
  plot(temp_rast)
  
  # 读取站点对应的流域TIF文件
  basin_tiff <- rast(basin_tiff_file)
  
  # 裁剪温度栅格到流域范围并计算流域内的温度平均值
  basin_tiff <- project(basin_tiff, crs(temp_rast))
  temp_rast <- resample(temp_rast, basin_tiff, method = "near")
  temp_clipped <- crop(temp_rast, basin_tiff)
  basin_tiff <- crop(basin_tiff, temp_rast)
  
  print(ext(temp_clipped))
  print(ext(basin_tiff))
  plot(temp_clipped)
  
  temp_masked <- mask(temp_clipped, basin_tiff)
  plot(temp_masked)
  
  # 计算平均值
  average_val <- global(temp_masked, "mean", na.rm = TRUE)$mean
  
  return(average_val)
}

# 初始化一个结果数据框
temp_results <- data.frame()

# 对所有站点计算温度平均值
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if (station_id < 864 ) next
  sampling_time <- as.Date(stations$Sampling_time[i])
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  
  if (file.exists(basin_tiff_file)) {
    station_temp <- extract_basin_temp(stations[i,], lon, lat, temperature, time_dates, basin_tiff_file)
    station_results <- data.frame(
      ID = station_id,
      Sampling_time = format(sampling_time, "%Y-%m"),
      Temperature = station_temp
    )
    temp_results <- bind_rows(temp_results, station_results)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
  gc()  # 垃圾回收，释放内存
}

# 查看结果
print(temp_results)

# 写入csv文件
write_csv(temp_results, output_path)
