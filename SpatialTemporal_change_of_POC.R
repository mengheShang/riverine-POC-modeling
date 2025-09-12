#####
###时间变化
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)

# 读取数据
poc_data <- read_excel("D:\\POC research\\data\\POC records\\all records.xlsx")
poc_data <- read_excel("D:\\POC research\\data\\POC records\\records_Huanghe.xlsx")
# 将采样时间转换为日期时间格式
poc_data$sample_time <- as.Date(poc_data$Sampling_Time, format = "%Y-%m-%d")


##季节变化
# 根据采样时间添加季节列
poc_data$season <- ifelse(month(poc_data$sample_time) %in% c(3, 4, 5), "Spring",
                      ifelse(month(poc_data$sample_time) %in% c(6, 7, 8), "Summer",
                             ifelse(month(poc_data$sample_time) %in% c(9, 10, 11), "Autumn", "Winter")))

# 按季节分组并计算POC浓度的平均值
seasonal_avg_poc <- poc_data %>%
  filter(`POC (mg/L)` != 0) %>%
  group_by(season) %>%
  summarise(avg_poc = mean(`POC (mg/L)`, na.rm = TRUE)) %>%
  #arrange(match(season, c("Spring", "Summer", "Autumn", "Winter")))

ggplot(seasonal_avg_poc, aes(x = season, y = avg_poc)) +
  geom_line(color = "skyblue", size = 1) +
  geom_point(color = "blue", size = 0.5) +
  labs(title = "Seasonal Variation of POC Concentration",
       x = "Season", 
       y = "Average POC Concentration(mg/L)") +
  theme(plot.title = element_text(hjust = 0.5))
        
#####
##逐月变化
# 浓度
poc_data$YearMonth <- floor_date(poc_data$sample_time, "month")

monthly_avg_poc <- poc_data %>%
  group_by(YearMonth) %>%
  summarise(avg_poc = mean(`POC (mg/L)`, na.rm = TRUE),
            se_poc = sd(`POC (mg/L)`, na.rm = TRUE) / sqrt(n()),
            data_count = sum(!is.na(`POC (mg/L)`)))

ggplot(monthly_avg_poc, aes(x = YearMonth, y = avg_poc)) +
  geom_line(color = "blue", size = 1) +  # 使用线图表示变化
  geom_point(color = "red", aes(size = data_count)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Monthly Variation of POC Concentration",
       x = "Month",
       y = "Average POC Concentration (mg/L)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 通量
poc_data$YearMonth <- floor_date(poc_data$sample_time, "month")

monthly_avg_poc <- poc_data %>%
  group_by(YearMonth) %>%
  filter(`POC flux(g/s)` != 0) %>%
  summarise(avg_poc = mean(`POC flux(g/s)`, na.rm = TRUE),
            se_poc = sd(`POC flux(g/s)`, na.rm = TRUE) / sqrt(n()),
            data_count = sum(!is.na(`POC flux(g/s)`)))

ggplot(monthly_avg_poc, aes(x = YearMonth, y = avg_poc)) +
  geom_line(color = "blue", size = 1) +  # 使用线图表示变化
  geom_point(color = "red", aes(size = data_count)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Monthly Variation of POC Flux",
       x = "Month",
       y = "Average POC Flux (g/s)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####
##年均月变化
# 浓度
poc_data$Month <- month(poc_data$sample_time, label = TRUE, abbr = TRUE)  # 提取月份并使用缩写

monthly_avg_poc2 <- poc_data %>%
  group_by(Month) %>%
  filter(`POC (mg/L)` != 0) %>%
  summarise(avg_poc = mean(`POC (mg/L)`, na.rm = TRUE),
            se_poc = sd(`POC (mg/L)`, na.rm = TRUE) / sqrt(n()),
            data_count = sum(!is.na(`POC (mg/L)`)))  # 计算平均值，忽略缺失数据

summary(monthly_avg_poc2$data_count)
breaks_manual <- c(0, 20, 25, 30, Inf)
labels_manual <- c("<20", "20-25", "25-30", ">30")
monthly_avg_poc2$count_Breaks <- cut(
  monthly_avg_poc2$data_count,
  breaks = breaks_manual,
  include.lowest = TRUE,
  labels = labels_manual)
size_mapping <- c("<20" = 1.5, "20-25" = 2.5, "25-30" = 3, ">30" = 4)

ggplot(monthly_avg_poc2, aes(x = Month, y = avg_poc)) +
  geom_line(color = "blue", size = 1) +  # 使用线图表示变化
  geom_point(color = "red", aes(size = count_Breaks)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Average POC Concentration by Month Across Years",
       x = "Month",
       y = "Average POC Concentration (mg/L)") +
  scale_size_manual(values = size_mapping) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 倾斜X轴标签以避免重叠

# 通量
poc_data$Month <- month(poc_data$sample_time, label = TRUE, abbr = TRUE)  # 提取月份并使用缩写

monthly_avg_poc2 <- poc_data %>%
  group_by(Month) %>%
  filter(`POC flux(g/s)` != 0) %>%
  summarise(avg_poc = mean(`POC flux(g/s)`, na.rm = TRUE),
            se_poc = sd(`POC flux(g/s)`, na.rm = TRUE) / sqrt(n()),
            data_count = sum(!is.na(`POC flux(g/s)`)))  # 计算平均值，忽略缺失数据

summary(monthly_avg_poc2$data_count)
breaks_manual <- c(0, 20, 25, 30, Inf)
labels_manual <- c("<20", "20-25", "25-30", ">30")
monthly_avg_poc2$count_Breaks <- cut(
  monthly_avg_poc2$data_count,
  breaks = breaks_manual,
  include.lowest = TRUE,
  labels = labels_manual)
size_mapping <- c("<20" = 1.5, "20-25" = 2.5, "25-30" = 3, ">30" = 4)

ggplot(monthly_avg_poc2, aes(x = Month, y = avg_poc)) +
  geom_line(color = "blue", size = 1) +  # 使用线图表示变化
  geom_point(color = "red", aes(size = count_Breaks)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Average POC Flux by Month Across Years",
       x = "Month",
       y = "Average POC Flux (g/s)", 
       size = "Data Count") +
  scale_size_manual(values = size_mapping) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 倾斜X轴标签以避免重叠



#####
###空间分布
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(sf)
library(cowplot)

# 设置地图shapefile的路径
china_map_path <- "D:/POC research/data/Chinamap_beta/China_Land.shp" 
nineline_path <- "D:/POC research/data/Chinamap_beta/South_China_Sea.shp"
Basin_path <- "D:/POC research/data/basin/8basin.shp"
RiverStem_path <- "D:/POC research/data/river/stem.shp"
RiverTri_path <- "D:/POC research/data/river/tributaries.shp"

# 读取shapefile
china_map <- st_read(china_map_path)
nineline <- st_read(nineline_path)
basin <- st_read(Basin_path)
stem_river <- st_read(RiverStem_path)
tri_river <- st_read(RiverTri_path)

# 将shapefile转换到WGS 84的坐标系
china_map_wgs84 <- st_transform(china_map, crs = 4326)
nineline_wgs84 <- st_transform(nineline, crs = 4326)
basin_wgs84 <- st_transform(basin, crs = 4326)
stem_river_wgs84 <- st_transform(stem_river, crs = 4326)
tri_river_wgs84 <- st_transform(tri_river, crs = 4326)

# 读取POC数据
poc_data <- read_excel("D:\\POC research\\data\\POC records\\all records.xlsx")
# 将所有的空值和0转换为NA
poc_data <- poc_data %>%
  mutate(across(where(is.character), ~ na_if(., ""))) %>%
  mutate(across(where(is.numeric), ~ na_if(., 0)))
# 去除经纬度缺失值和POC通量为0的行
poc_data <- poc_data %>%
  filter(!is.na(Longitude) & !is.na(Latitude))
# 去除浓度缺失的行
poc_data <- poc_data %>%
  filter(!is.na(`POC (mg/L)`))
# 去除通量缺失的行
poc_data <- poc_data %>%
  filter(!is.na(`POC flux(g/s)`))
# 计算相同位置数据的均值
poc_data <- poc_data %>%
  group_by(Longitude, Latitude) %>%
  summarize(`POC (mg/L)` = mean(`POC (mg/L)`, na.rm = TRUE),
            `POC flux(g/s)` = mean(`POC flux(g/s)`, na.rm = TRUE))

# 将散点数据转换为sf对象
poc_data_sf <- st_as_sf(poc_data, coords = c("Longitude", "Latitude"), crs = 4326)

#####
## 浓度分段
summary(poc_data_sf$`POC (mg/L)`)
breaks_manual <- c(0, 1, 5, 10, Inf)
labels_manual <- c("<1", "1-5", "5-10", ">10")
poc_data_sf$POC_Breaks <- cut(
  poc_data_sf$`POC (mg/L)`,
  breaks = breaks_manual,
  include.lowest = TRUE,
  labels = labels_manual
)
# 绘制浓度分布
# 创建共用地图基础
base_map <- ggplot() +
  geom_sf(data = china_map_wgs84, fill = "ivory", color = "black", alpha = 1) +
  geom_sf(data = nineline_wgs84, fill = "ivory", color = "black") +
  geom_sf(data = basin_wgs84, fill = NA, color = "black") +
  geom_sf(data = tri_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.4) +
  geom_sf(data = stem_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.7) +
  geom_sf(data = poc_data_sf, 
          aes(color = POC_Breaks)) +
  scale_color_manual(values = c("yellow", "orange", "red", "darkred")) +
  theme_minimal(base_size = 12)
# 主地图
main_map <- base_map +
  labs(title = "Measured POC Spatial Distribution", x = "Longitude", y = "Latitude",
       color = "POC(mg/L)") +
  coord_sf(xlim = c(70, 135), ylim = c(15, 55)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.background = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))
# 子地图
south_sea_map <- base_map +
  coord_sf(xlim = c(107, 122.8), ylim = c(2.218933, 24.385950)) +
  guides(size = "none", color = "none") +  # 关闭图例的显示
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())
# 定义南海诸岛子图的位置
xpos <- 0.692 
ypos <- 0.144  
width <- 0.15  # 子图的宽度
height <- 0.15 # 子图的高度
# 使用cowplot将南海诸岛地图放置在主图的右下角
final_plot <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(south_sea_map, x = xpos, y = ypos, width = width, height = height)
# 绘制最终地图
print(final_plot)

#####
## 通量分段
summary(poc_data_sf$`POC flux(g/s)`)
breaks_manual <- c(0, 500, 2500, 10000, Inf)
labels_manual <- c("<500", "500-2500", "2500-10000", ">10000")
poc_data_sf$Flux_Breaks <- cut(
  poc_data_sf$`POC flux(g/s)`,
  breaks = breaks_manual,
  include.lowest = TRUE,
  labels = labels_manual
)
# 绘制通量分布
# 创建共用地图基础
base_map <- ggplot() +
  geom_sf(data = china_map_wgs84, fill = "ivory", color = "black", alpha = 1) +
  geom_sf(data = nineline_wgs84, fill = "ivory", color = "black") +
  geom_sf(data = basin_wgs84, fill = NA, color = "black") +
  geom_sf(data = tri_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.4) +
  geom_sf(data = stem_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.7) +
  geom_sf(data = poc_data_sf, 
          aes(color = Flux_Breaks)) +
  scale_color_manual(values = c("yellow", "orange", "red", "darkred")) +
  theme_minimal(base_size = 12)
# 主地图
main_map <- base_map +
  labs(title = "Measured POC Spatial Distribution", x = "Longitude", y = "Latitude",
       color = "POC flux(g/s)") +
  coord_sf(xlim = c(70, 135), ylim = c(15, 55)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.background = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))
# 子地图
south_sea_map <- base_map +
  coord_sf(xlim = c(107, 122.8), ylim = c(2.218933, 24.385950)) +
  guides(size = "none", color = "none") +  # 关闭图例的显示
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())
# 定义南海诸岛子图的位置
xpos <- 0.638 
ypos <- 0.173  
width <- 0.15  # 子图的宽度
height <- 0.15 # 子图的高度
# 使用cowplot将南海诸岛地图放置在主图的右下角
final_plot <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(south_sea_map, x = xpos, y = ypos, width = width, height = height)
# 绘制最终地图
print(final_plot)

#####
## 同时绘制浓度和通量分布
# 创建共用地图基础
base_map <- ggplot() +
  geom_sf(data = china_map_wgs84, fill = "ivory", color = "black", alpha = 1) +
  geom_sf(data = nineline_wgs84, fill = "ivory", color = "black") +
  geom_sf(data = basin_wgs84, fill = NA, color = "black") +
  geom_sf(data = tri_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.4) +
  geom_sf(data = stem_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.7) +
  geom_sf(data = poc_data_sf, 
          aes(size = Flux_Breaks,
              color = POC_Breaks), 
          shape = 21, lwd = 50) +
  scale_size_manual(values = c(1, 2, 3, 4)) +
  scale_color_manual(values = c("yellow", "orange", "red", "darkred")) +
  theme_minimal(base_size = 12)

# 主地图
main_map <- base_map +
  labs(title = "Measured POC Spatial Distribution", x = "Longitude", y = "Latitude",
       size = "POC flux(g/s)", color = "POC(mg/L)") +
  coord_sf(xlim = c(70, 135), ylim = c(15, 55)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.background = element_blank(),
        legend.position = "right",
        legend.box = "vertical",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))

# 子地图
south_sea_map <- base_map +
  coord_sf(xlim = c(107, 122.8), ylim = c(2.218933, 24.385950)) +
  guides(size = "none", color = "none") +  # 关闭图例的显示
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

# 定义南海诸岛子图的位置
xpos <- 0.637 
ypos <- 0.173  
width <- 0.15  # 子图的宽度
height <- 0.15 # 子图的高度

# 使用cowplot将南海诸岛地图放置在主图的右下角
final_plot <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(south_sea_map, x = xpos, y = ypos, width = width, height = height)

# 绘制最终地图
print(final_plot)