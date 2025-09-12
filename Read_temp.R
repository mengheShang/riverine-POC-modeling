# 加载必要的库
library(ncdf4)
library(readxl)
library(geosphere)
library(readr)

# 设置输入输出路径
sites_excel <- "D:/POC research/data/Sites position.xlsx"
nc_file <- "D:/POC research/data/pco2Data/ERA5-Land-monthly_China_1950_2022.nc"
output_excel <- "D:/POC research/data/pco2Data_sites/temperature.csv"

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
temperature2 <- temperature - 273.15  # 转换为摄氏度

# 读取站点数据
sites <- read_excel(sites_excel)
site_coords <- cbind(sites$Longitude, sites$Latitude)

# 创建网格点矩阵
grid_matrix <- expand.grid(lon = lon, lat = lat)

# 初始化存储结构
date_labels <- format(time_dates, "\"%Y-%m\"")
temperature_df <- data.frame(Station_ID = sites$ID)
for (i in 1:length(date_labels)) {
  temperature_df[paste0(date_labels[i])] <- NA  # 初始化每个时间点的列为NA
}

# 计算每个站点的最近网格点并提取时间序列
for (i in seq_along(sites$ID)) {
  # 计算距离并找到最近的网格点
  distances <- geosphere::distHaversine(matrix(site_coords[i,], nrow = 1), grid_matrix)
  nearest_index <- which.min(distances)
  
  # 计算对应的lon和lat索引
  nearest_lon_index <- (nearest_index - 1) %% length(lon) + 1
  nearest_lat_index <- (nearest_index - 1) %/% length(lon) + 1
  
  # 提取气温时间序列
  temp_series <- temperature2[nearest_lon_index, nearest_lat_index, ]
  
  # 如果存在NA值，则寻找最近的非NA值
  if (any(is.na(temp_series))) {
    # 按距离排序
    ordered_indices <- order(distances)
    for (j in ordered_indices) {
      lon_index <- (j - 1) %% length(lon) + 1
      lat_index <- (j - 1) %/% length(lon) + 1
      candidate_series <- temperature[lon_index, lat_index, ]
      if (!any(is.na(candidate_series))) {
        temp_series <- candidate_series
        break
      }
    }
  }
  
  # 存储到列表中
  temperature_df[i, 2:ncol(temperature_df)] <- temp_series
}

# 输出结果
print(temperature_df)

# 将结果写入Excel文件
write_csv(temperature_df, output_excel)

