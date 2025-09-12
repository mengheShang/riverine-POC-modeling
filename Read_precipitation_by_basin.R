library(ncdf4)
library(raster)
library(sf)

# 读取数据
nc_file <- "D:\\POC research\\data\\pco2Data\\ERA5-Land-monthly_China_1950_2022.nc"
basin_path <- "D:\\POC research\\data\\basin\\8basin.shp"
output_path <- "D:\\POC research\\data\\pco2Data_basin\\precipitation.csv"

# 提取降水数据及其坐标
nc_data <- nc_open(nc_file)
lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude")
time <- ncvar_get(nc_data, "time")
fill_value <- ncatt_get(nc_data, "tp", "_FillValue")$value
nc_close(nc_data)

# 转换时间为日期
time_dates <- as.POSIXct(time * 3600, origin="1900-01-01", tz="GMT")
date_labels <- format(time_dates, "%Y-%m")

# 创建一个 raster brick 对象
r <- brick(nc_file, varname = "tp")

r[r == fill_value] <- NA  # 使用 NA 替换缺失值
# 定义转换单位和按月天数调整的函数
convert_precipitation <- function(r, time_dates) {
  days_in_month <- function(date) {
    y <- format(date, "%Y")
    m <- format(date, "%m")
    next_month <- as.Date(paste0(y, "-", as.integer(m) %% 12 + 1, "-01"))
    days <- as.integer(format(next_month - 1, "%d"))
    return(days)
  }
  days_in_months <- sapply(time_dates, days_in_month)
  
  for (i in seq_along(time_dates)) {
    month_days <- days_in_months[i]
    r[[i]] <- calc(r[[i]], fun = function(x) x * month_days * 1000)  # 单位转换并乘以每个月的天数
  }
  return(r)
}

# 转换单位并按月天数调整
r <- convert_precipitation(r, time_dates)

# 读取流域数据
watersheds <- st_read(basin_path)

# 确保坐标系一致
crs(r) <- st_crs(watersheds)$proj4string

# 初始化数据框存储结果
precipitation_df <- data.frame(Watershed = watersheds$NAME)

for (i in 1:length(date_labels)) {
  precipitation_df[paste0(date_labels[i])] <- NA  # 初始化每个时间点的列为NA
}

# 定义按流域提取数据的函数
extract_precipitation_by_watershed <- function(raster_data, watershed) {
  masked_raster <- mask(raster_data, as(watershed, "Spatial"))
  mean_precipitation <- cellStats(masked_raster, stat = 'mean', na.rm = TRUE)
  return(mean_precipitation)
}

# 遍历每个流域和时间点，提取数据
for (i in 1:nrow(watersheds)) {
  watershed <- watersheds[i, ]
  watershed_name <- as.character(watershed$NAME)  
  for (j in 1:length(time_dates)) {
    raster_layer <- r[[j]]  # 获取每个时间点的栅格层
    mean_precipitation <- extract_precipitation_by_watershed(raster_layer, watershed)
    precipitation_df[i, paste0(date_labels[j])] <- mean_precipitation
  }
}

print(precipitation_df)

# 保存结果到 CSV 文件
write.csv(precipitation_df, output_path, row.names = FALSE)