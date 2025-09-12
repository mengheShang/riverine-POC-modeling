# 加载必要的库
library(ncdf4)
library(readxl)
library(geosphere)
library(readr)

# 设置输入输出路径
sites_excel <- "D:/POC research/data/Sites position.xlsx"
nc_file <- "D:/POC research/data/pco2Data/ERA5-Land-monthly_China_1950_2022.nc"
output_csv <- "D:/POC research/data/variation/precipitation.csv"

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
#precipitation <- precipitation * scale_factor + add_offset  # 应用缩放因子和偏移量
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

#print(days_in_months)

# 乘以每个月的天数
for (i in seq_along(time_dates)) {
  month_days <- days_in_months[i]
  precipitation[,,i] <- precipitation[,,i] * month_days
}

# 读取站点数据
sites <- read_excel(sites_excel)
site_coords <- cbind(sites$Longitude, sites$Latitude)

# 创建网格点矩阵
grid_matrix <- expand.grid(lon = lon, lat = lat)

# 初始化存储结构
date_labels <- format(time_dates, "\"%Y-%m\"")
precipitation_df <- data.frame(Station_ID = sites$ID)
for (i in 1:length(date_labels)) {
  precipitation_df[paste0(date_labels[i])] <- NA  # 初始化每个时间点的列为NA
}

# 计算每个站点的最近网格点并提取时间序列
for (i in seq_along(sites$ID)) {
  # 计算距离并找到最近的网格点
  distances <- geosphere::distHaversine(matrix(site_coords[i,], nrow = 1), grid_matrix)
  nearest_index <- which.min(distances)
  
  # 计算对应的lon和lat索引
  nearest_lon_index <- (nearest_index - 1) %% length(lon) + 1
  nearest_lat_index <- (nearest_index - 1) %/% length(lon) + 1
  
  # 提取降水时间序列
  precip_series <- precipitation[nearest_lon_index, nearest_lat_index, ]
  
  # 如果存在NA值，则寻找最近的非NA值
  if (any(is.na(precip_series))) {
    # 按距离排序
    ordered_indices <- order(distances)
    for (j in ordered_indices) {
      lon_index <- (j - 1) %% length(lon) + 1
      lat_index <- (j - 1) %/% length(lon) + 1
      candidate_series <- precipitation[lon_index, lat_index, ]
      if (!any(is.na(candidate_series))) {
        precip_series <- candidate_series
        break
      }
    }
  }
  
  # 存储到列表中
  precipitation_df[i, 2:ncol(precipitation_df)] <- precip_series
}

# 输出结果
print(precipitation_df)

# 将结果写入CSV文件
write_csv(precipitation_df, output_csv)
