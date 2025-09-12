#####
# 浓度变化
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)

basin_path <- "D:\\POC research\\data\\basin\\8basin.shp"
poc_path <- "D:\\POC research\\data\\POC records\\all records.xlsx"

# 读取流域 shapefile
watershed_shp <- st_read(basin_path)

# 修复无效的几何图形
watershed_shp <- st_make_valid(watershed_shp)

# 读取 POC 数据
poc_data <- read_excel(poc_path)

# 去除经纬度缺失值的行
poc_data <- poc_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))
# 去除浓度缺失的行
poc_data <- poc_data %>%
  filter(!is.na(`POC (mg/L)`))

# 将 POC 数据转换为 sf 对象
poc_sf <- st_as_sf(poc_data, coords = c("Longitude", "Latitude"), crs = 4326)

# 将日期列转换为日期格式
poc_sf$date <- ymd(poc_sf$Parsed_Sampling_Time)

# 提取月份信息
poc_sf$month <- month(poc_sf$date)

# 按流域和月份计算 POC 浓度的平均值、标准误及数据量
poc_avg <- poc_sf %>%
  st_join(watershed_shp, join = st_within) %>%
  filter(`POC (mg/L)` > 0) %>%
  group_by(NAME, month) %>%
  summarize(avg_poc = mean(`POC (mg/L)`, na.rm = TRUE),
            se_poc = sd(`POC (mg/L)`, na.rm = TRUE) / sqrt(n()),
            data_count = n())

# 绘制 POC 浓度的月际变化并显示数据量和误差线段
ggplot(poc_avg, aes(x = factor(month), y = avg_poc, group = NAME, color = NAME)) +
  geom_line() +
  geom_point(aes(size = data_count)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Monthly Variation of POC Concentration by Watershed",
       x = "Month",
       y = "Average POC Concentration(mg/L)",
       size = "Data Count") +  # 图例中添加数据量说明
  scale_y_continuous(limits = c(0, 80), 
                     breaks = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80), 
                     labels = c(0, 5, 10, 15, 20, 30, 40, 50, 60, 70, 80),
                     trans = scales::trans_new("squish", 
                                               transform = function(x) ifelse(x <= 20, x, 20 + (x - 20) / 3),
                                               inverse = function(x) ifelse(x <= 20, x, 20 + (x - 20) * 3))) +  # 自定义纵轴比例
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")



#####
# 通量变化
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(lubridate)

basin_path <- "D:\\POC research\\data\\basin\\8basin.shp"
poc_path <- "D:\\POC research\\data\\POC records\\all records.xlsx"

# 读取流域 shapefile
watershed_shp <- st_read(basin_path)

# 修复无效的几何图形
watershed_shp <- st_make_valid(watershed_shp)

# 读取 POC 数据
poc_data <- read_excel(poc_path)

# 去除经纬度缺失值的行
poc_data <- poc_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))
# 去除通量缺失的行
poc_data <- poc_data %>%
  filter(!is.na(`POC flux(g/s)`))

# 将 POC 数据转换为 sf 对象
poc_sf <- st_as_sf(poc_data, coords = c("Longitude", "Latitude"), crs = 4326)

# 将日期列转换为日期格式
poc_sf$date <- ymd(poc_sf$Parsed_Sampling_Time)

# 提取月份信息
poc_sf$month <- month(poc_sf$date)

# 检查空间连接结果
joined_data <- st_join(poc_sf, watershed_shp, join = st_within)
# 查看连接结果中NA的情况
na_data <- joined_data %>%
  filter(is.na(NAME))
print(na_data)  # 打印NA的情况，查看原因

# 按流域和月份计算 POC 浓度的平均值、标准误及数据量
poc_avg <- poc_sf %>%
  st_join(watershed_shp, join = st_within) %>%
  filter(`POC flux(g/s)` > 0) %>%
  group_by(NAME, month) %>%
  summarize(avg_poc = mean(`POC flux(g/s)`, na.rm = TRUE),
            se_poc = sd(`POC flux(g/s)`, na.rm = TRUE) / sqrt(n()),
            data_count = n())

# 绘制 POC 浓度的月际变化并显示数据量和误差线段
ggplot(poc_avg, aes(x = factor(month), y = avg_poc, group = NAME, color = NAME)) +
  geom_line() +
  geom_point(aes(size = data_count)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Monthly Variation of POC Concentration by Watershed",
       x = "Month",
       y = "Average POC Flux(g/s)",
       size = "Data Count") +  # 图例中添加数据量说明
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")