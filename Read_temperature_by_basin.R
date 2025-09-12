library(ncdf4)
library(raster)
library(sf)

# 读取温度数据
nc_file <- "D:\\POC research\\data\\pco2Data\\ERA5-Land-monthly_China_1950_2022.nc"
basin_path <- "D:\\POC research\\data\\basin\\8basin.shp"
output_path <- "D:\\POC research\\data\\pco2Data_basin\\temperature.csv"

# 提取温度数据及其坐标
nc_data <- nc_open(nc_file)
temperature <- ncvar_get(nc_data, "t2m") 
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")
time <- ncvar_get(nc_data, "time")
fill_value <- ncatt_get(nc_data, "t2m", "_FillValue")$value
nc_close(nc_data)

# 转换时间为日期
time_dates <- as.POSIXct(time * 3600, origin="1900-01-01", tz="GMT")
date_labels <- format(time_dates, "%Y-%m")

# 创建一个 raster brick 对象
r <- brick(nc_file, varname = "t2m")
# 替换缺失值
r[r == fill_value] <- NA
# 转换为摄氏度
r <- r - 273.15

# 读取流域数据
watersheds <- st_read(basin_path)

# 确保坐标系一致
crs(r) <- st_crs(watersheds)$proj4string

# 初始化数据框存储结果
temperature_df <- data.frame(Watershed = watersheds$NAME)
# 初始化每个时间点的列为NA
for (i in 1:length(date_labels)) {
  temperature_df[paste0(date_labels[i])] <- NA  
}

# 定义按流域提取温度数据的函数
extract_temperature_by_watershed <- function(raster_data, watershed) {
  masked_raster <- mask(raster_data, as(watershed, "Spatial"))
  mean_temp <- cellStats(masked_raster, stat = 'mean', na.rm = TRUE)
  return(mean_temp)
}

# 遍历每个流域和时间点，提取温度数据
for (i in 1:nrow(watersheds)) {
  watershed <- watersheds[i, ]
  watershed_name <- as.character(watershed$NAME)  
  for (j in 1:length(time_dates)) {
    raster_layer <- r[[j]]  # 获取每个时间点的栅格层
    mean_temp <- extract_temperature_by_watershed(raster_layer, watershed)
    temperature_df[i, paste0(date_labels[j])] <- mean_temp
  }
}

print(temperature_df)

# 保存结果到 CSV 文件
write.csv(temperature_df, output_path, row.names = FALSE)