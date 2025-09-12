# ---- 分时间 ----

# 加载必要的库
library(ncdf4)
library(dplyr)
library(lubridate)
library(readxl)
library(writexl)
library(ggplot2)

# 读取nc文件
nc_file <- "E:\\POC research\\data\\2_Discharge and Sediment\\WaterGAP\\watergap_22d_gswp3-w5e5_histsoc_dis_monthly_1901_2019.nc4"
nc_data <- nc_open(nc_file)

# 获取nc文件中的变量
lon <- ncvar_get(nc_data, "lon")  # 经度
lat <- ncvar_get(nc_data, "lat")  # 纬度
runoff <- ncvar_get(nc_data, "dis")  # 径流量
time <- ncvar_get(nc_data, "time")  # 时间

# 检查时间单位
time_units <- ncatt_get(nc_data, "time", "units")
print(time_units$value)  # 显示时间单位

# 将时间变量转换为日期格式（单位为"months since 1901-01-01"）
origin <- as.Date("1901-01-01")  
time_dates <- origin %m+% months(time)

# 关闭nc文件
nc_close(nc_data)

# 读取站点表格
stations <- read_excel("E:\\POC research\\data\\3_POC records\\original_records_correct.xlsx")

# 确保站点表格中有经纬度和日期列，并添加Year和Month列
stations <- stations %>%
  mutate(Date = as.Date(Sampling_Time),
         Year = year(Date),
         Month = month(Date))

# 按 ID、Longitude、Latitude、Year 和 Month 聚合，计算每月的平均值
monthly_stations <- stations %>%
  group_by(ID, Longitude, Latitude, Year, Month) %>%
  summarise(`Water discharge2 (m3/s)` = mean(`Water discharge2 (m3/s)`, na.rm = TRUE))

# 创建新的日期列（每月的第一天）
monthly_stations <- monthly_stations %>%
  mutate(Date = make_date(Year, Month, 1))

# 创建一个函数来找到最近的经纬度索引
find_nearest_index <- function(value, vector) {
  return(which.min(abs(vector - value)))
}

# 补全缺失的径流量数据
monthly_stations <- monthly_stations %>%
  rowwise() %>%
  mutate(
    Lon_Index = find_nearest_index(Longitude, lon),
    Lat_Index = find_nearest_index(Latitude, lat),
    Time_Index = find_nearest_index(Date, time_dates),
    Model_Discharge = runoff[Lon_Index, Lat_Index, Time_Index]  # 获取模型数据
  ) %>%
  ungroup()
#  select(-Lon_Index, -Lat_Index, -Time_Index)

# 检查数据匹配情况
head(monthly_stations)

# 在评估模型拟合优度之前，过滤掉模型中小于100的值
comparison_data <- monthly_stations %>%
  filter(Model_Discharge >= 100) %>%
  filter(!is.na(`Water discharge2 (m3/s)`) & !is.na(Model_Discharge))

# 计算拟合优度的函数
calculate_statistics <- function(observed, predicted) {
  # 计算 R²
  r_squared <- cor(observed, predicted)^2
  
  # 计算 RMSE
  rmse <- sqrt(mean((observed - predicted)^2))
  
  # 计算 MAE
  mae <- mean(abs(observed - predicted))
  
  return(list(R2 = r_squared, RMSE = rmse, MAE = mae))
}

# 对实测数据与模型数据进行拟合优度计算
stats <- calculate_statistics(comparison_data$`Water discharge2 (m3/s)`, comparison_data$Model_Discharge)

# 输出统计结果
print(paste("R²: ", stats$R2))
print(paste("RMSE: ", stats$RMSE))
print(paste("MAE: ", stats$MAE))

# 添加季节信息
comparison_data <- comparison_data %>%
  mutate(Season = case_when(
    Month %in% c(12, 1, 2) ~ "Winter",  # 冬季
    Month %in% c(3, 4, 5) ~ "Spring",   # 春季
    Month %in% c(6, 7, 8) ~ "Summer",   # 夏季
    Month %in% c(9, 10, 11) ~ "Autumn"  # 秋季
  ))

# 绘制散点图，实测值在x轴，模型值在y轴，使用颜色区分季节
ggplot(comparison_data, aes(x = `Water discharge2 (m3/s)`, y = Model_Discharge, color = Season)) +
  geom_point(size = 3, alpha = 0.6) +  # 绘制散点
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 1:1 标准线
  theme_minimal() +
  labs(
    title = "Water Discharge of WaterGAP",
    x = "Observed Water Discharge (m3/s)",
    y = "Modelled Water Discharge (m3/s)"
  ) +
  scale_fill_brewer(palette = "Set1") +  
  # 添加拟合优度信息
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1,
           label = paste("R² = ", round(stats$R2, 3), "\nRMSE = ", round(stats$RMSE, 2), "\nMAE = ", round(stats$MAE, 2)),
           size = 5, color = "black") +
  # 调整坐标轴范围以确保显示完整
  coord_fixed(ratio = 1)

# ---- 分流域 ----

library(sf)# 加载必要的库
library(raster)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(sf)

# 读取流域的tif文件
basin_raster <- raster("D:\\POC research\\data\\1_basin\\8basin_ID.tif")

# 获取tif中的流域ID和流域名称的对应关系
basin_id_to_name <- data.frame(
  OBJECTID = 1:8, 
  NAME = c("Yangtze River Basin", "Southwest International River Basin", "Yellow River Basin", 
           "Huaihe Basin", "Haihe Basin", "Southeastern Coastal River Basin", 
           "Northeast River Basin", "Pearl River Basin")
)

# 读取nc文件
nc_file <- "D:\\POC research\\data\\2_Discharge and Sediment\\WaterGAP\\watergap_22d_gswp3-w5e5_histsoc_dis_monthly_1901_2019.nc4"
nc_data <- nc_open(nc_file)

# 获取nc文件中的变量
lon <- ncvar_get(nc_data, "lon")  # 经度
lat <- ncvar_get(nc_data, "lat")  # 纬度
runoff <- ncvar_get(nc_data, "dis")  # 径流量
time <- ncvar_get(nc_data, "time")  # 时间

# 检查时间单位
time_units <- ncatt_get(nc_data, "time", "units")
print(time_units$value)  # 显示时间单位

# 将时间变量转换为日期格式（单位为"months since 1901-01-01"）
origin <- as.Date("1901-01-01")  
time_dates <- origin %m+% months(time)

# 关闭nc文件
nc_close(nc_data)

# 读取站点表格
stations <- read_excel("D:\\POC research\\data\\3_POC records\\original_records.xlsx")

# 确保站点表格中有经纬度和日期列，并添加Year和Month列
stations <- stations %>%
  mutate(Date = as.Date(Sampling_Time),
         Year = year(Date),
         Month = month(Date))

# 按 ID、Longitude、Latitude、Year 和 Month 聚合，计算每月的平均值
monthly_stations <- stations %>%
  group_by(ID, Longitude, Latitude, Year, Month) %>%
  summarise(`Water discharge2 (m3/s)` = mean(`Water discharge2 (m3/s)`, na.rm = TRUE))

# 创建新的日期列（每月的第一天）
monthly_stations <- monthly_stations %>%
  mutate(Date = make_date(Year, Month, 1))

# 创建一个函数来找到最近的经纬度索引
find_nearest_index <- function(value, vector) {
  return(which.min(abs(vector - value)))
}

# 补全缺失的径流量数据，找到与模型数据的匹配
monthly_stations <- monthly_stations %>%
  rowwise() %>%
  mutate(
    Lon_Index = find_nearest_index(Longitude, lon),
    Lat_Index = find_nearest_index(Latitude, lat),
    Time_Index = find_nearest_index(Date, time_dates),
    Model_Discharge = runoff[Lon_Index, Lat_Index, Time_Index]  # 获取模型数据
  ) %>%
  ungroup() # 取消 rowwise，转换为正常数据框

# 提取站点的流域ID
coordinates <- data.frame(lon = monthly_stations$Longitude, lat = monthly_stations$Latitude)
station_points <- SpatialPoints(coordinates, proj4string = CRS(projection(basin_raster)))

# 从 tif 文件中提取每个站点的流域 ID，使用 nearest 方法找到最近的栅格值
monthly_stations$Basin_ID <- extract(basin_raster, station_points, method = "simple", buffer = 5000, fun = function(x) x[which.min(is.na(x))])

unmatched_points <- monthly_stations %>%
  filter(is.na(Basin_ID))

print(unmatched_points)

# 将流域ID映射到流域名称
monthly_stations <- monthly_stations %>%
  left_join(basin_id_to_name, by = c("Basin_ID" = "OBJECTID"))

# 过滤掉没有实测数据或模型数据的行
comparison_data <- monthly_stations %>%
  filter(!is.na(`Water discharge2 (m3/s)`) & !is.na(Model_Discharge)) %>%
  filter(Model_Discharge >= 100)

# 计算拟合优度的函数
calculate_statistics <- function(observed, predicted) {
  r_squared <- cor(observed, predicted)^2
  rmse <- sqrt(mean((observed - predicted)^2))
  mae <- mean(abs(observed - predicted))
  return(list(R2 = r_squared, RMSE = rmse, MAE = mae))
}

# 计算拟合优度
stats <- calculate_statistics(comparison_data$`Water discharge2 (m3/s)`, comparison_data$Model_Discharge)

# 绘制散点图，实测值在x轴，模型值在y轴，使用颜色区分流域
ggplot(comparison_data, aes(x = `Water discharge2 (m3/s)`, y = Model_Discharge, color = NAME)) +
  geom_point(size = 3, alpha = 0.6) +  # 绘制散点
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 1:1 标准线
  theme_minimal() +
  labs(
    title = "Observed vs. Modelled Water Discharge by Basin",
    x = "Observed Water Discharge (m3/s)",
    y = "Modelled Water Discharge (m3/s)",
    color = "Basin Name"  # 图例中的标题
  ) +
  scale_color_brewer(palette = "Set1") +  # 使用设定的颜色
  # 添加拟合优度信息
  annotate("text", x = Inf, y = -Inf, hjust = 1, vjust = -1,
           label = paste("R² = ", round(stats$R2, 3), "\nRMSE = ", round(stats$RMSE, 2), "\nMAE = ", round(stats$MAE, 2)),
           size = 5, color = "black") +
  # 调整坐标轴比例确保1:1线可视
  coord_fixed(ratio = 1)
