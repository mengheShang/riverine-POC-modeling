# 加载必要的库
library(ncdf4)
library(dplyr)
library(lubridate)
library(readxl)
library(writexl)
library(sf)
library(ggplot2)

# 读取 GRFR nc 文件
nc_file <- "E:\\POC research\\data\\2_Discharge and Sediment\\GRFR\\output_pfaf_04_1979-2019.nc"
nc_data <- nc_open(nc_file)

# 获取时间维度信息
time <- ncvar_get(nc_data, "time")
origin <- as.Date("1979-01-01")  # 假设时间起点为1979-01-01
time_dates <- origin + days(time)

# 读取站点表格
stations <- read_excel("E:\\POC research\\data\\3_POC records\\original_records_correct.xlsx")

# 确保站点表格中有经纬度和日期列，并添加 Year 和 Month 列
stations <- stations %>%
  mutate(Date = as.Date(Sampling_Time),
         Year = year(Date),
         Month = month(Date))

# 读取流域 shp 文件，获取河段及其 rivid
shp_file <- "E:\\POC research\\data\\2_Discharge and Sediment\\MERIT_Basins_v0.7\\level_01_v0.7\\pfaf_04_riv_3sMERIT.shp"
river_shp <- st_read(shp_file)

# 将站点表格转换为空间点对象
stations_sf <- st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = st_crs(river_shp))

# 进行空间匹配，找到每个站点最近的河流段（nearest feature）
nearest_indices <- st_nearest_feature(stations_sf, river_shp)

# 获取NetCDF中所有的rivid列表
nc_rivids <- ncvar_get(nc_data, "rivid")  

# 创建一个函数来找到最近的时间索引
find_nearest_time_index <- function(date, time_dates) {
  which.min(abs(time_dates - date))
}

# 初始化 Model_Discharge 列
stations$Model_Discharge <- NA

# 为每条记录单独读取流量数据
for (i in 1:nrow(stations)) {
  # 获取当前记录的日期和 rivid
  current_date <- stations$Date[i]
  current_rivid <- stations$rivid[i]
  river_index <- which(nc_rivids == current_rivid)
  
  # 找到最近的时间索引
  time_index <- find_nearest_time_index(current_date, time_dates)
  
  # 只读取当前 rivid 和时间索引的 Qout 值
  model_discharge <- ncvar_get(nc_data, "Qout", 
                               start = c(river_index, time_index),
                               count = c(1, 1))
  
  # 将读取到的模型流量数据存入对应位置
  stations$Model_Discharge[i] <- model_discharge
}

# 关闭 nc 文件
nc_close(nc_data)

# 检查数据匹配情况
head(stations)

# 计算拟合优度等统计数据
comparison_data <- stations %>%
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

# 绘制散点图，实测值在 x 轴，模型值在 y 轴，使用颜色区分季节
ggplot(comparison_data, aes(x = `Water discharge2 (m3/s)`, y = Model_Discharge, color = Season)) +
  geom_point(size = 3, alpha = 0.6) +  # 绘制散点
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # 1:1 标准线
  theme_minimal() +
  labs(
    title = "Water Discharge of GRFR Model",
    x = "Observed Water Discharge (m3/s)",
    y = "Modelled Water Discharge (m3/s)"
  ) +
  scale_color_brewer(palette = "Set2") +  # 使用颜色区分季节
  # 添加拟合优度信息
  annotate("text", x = max(comparison_data$`Water discharge2 (m3/s)`) * 0.7, 
           y = max(comparison_data$Model_Discharge) * 0.5,
           label = paste("R² = ", round(stats$R2, 3), "\nRMSE = ", round(stats$RMSE, 2), "\nMAE = ", round(stats$MAE, 2)),
           color = "black") +
  coord_fixed(ratio = 1)
