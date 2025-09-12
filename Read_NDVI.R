library(raster)

# 读取包含时间信息和NDVI数据的TIF文件
tif_file <- "D:\\POC research\\data\\MODIS_indices\\MODIS_Indices-0000007936-0000007936.tif"
raster_stack <- stack(tif_file)

# 检查栅格文件的元数据
print(raster_stack)

# 假设时间信息波段索引为13，NDVI波段索引为14
time_band <- raster_stack[[13]]
ndvi_band <- raster_stack[[14]]

# 提取时间信息和NDVI值
time_values <- getValues(time_band)
ndvi_values <- getValues(ndvi_band)

# 只输出非NA的栅格点
non_na_indices <- which(!is.na(time_values) & !is.na(ndvi_values))
non_na_time_values <- time_values[non_na_indices]
non_na_ndvi_values <- ndvi_values[non_na_indices]

# 打印非NA的时间信息和NDVI值
print(non_na_time_values)
print(non_na_ndvi_values)

# 检查前几行非NA数据
print(head(non_na_time_values))
print(head(non_na_ndvi_values))

# 可视化非NA的NDVI时间序列
ndvi_ts <- data.frame(Date = non_na_time_values, NDVI = non_na_ndvi_values)

# 打印非NA的时间序列对象
print(ndvi_ts)

print(class(time_values))
print(summary(time_values))

# 将DOY转换为日期，假设年份为一个特定的年份，例如2000年
dates <- as.Date(non_na_time_values - 1, origin="2000-01-01")

# 创建时间序列对象
ndvi_ts <- data.frame(Date = dates, NDVI = non_na_ndvi_values)

# 打印非NA的时间序列对象
print(ndvi_ts)