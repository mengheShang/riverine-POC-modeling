
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(cowplot)

# 读取数据
excel_path <- "D:\\POC research\\data\\POC records\\records.xlsx"
data <- read_excel(excel_path)

# 转换为Date对象
data$Parsed_Sampling_Time <- as.Date(data$Parsed_Sampling_Time)

# 为采样次数创建分段
data$Sampling_Count_Breaks <- cut(
  data$Total_Sampling_Count,
  breaks = c(-Inf, 10, 20, 30, Inf),  # 定义4个区间
  labels = c("<11", "11-20", "21-30", ">30")
)

# 为采样时间创建分段
data$Year <- year(data$Parsed_Sampling_Time)
data$Sampling_Time_Breaks <- cut(
  data$Year,
  breaks = c(-Inf, 2000, 2005, 2010, 2015, Inf),  # 定义5个区间
  labels = c("Before 2000", "2000-2005", "2005-2010", "2010-2015", "After 2016")
)

# 设置地图shapefile的路径
china_map_path <- "D:/POC research/data/Chinamap_beta/China_Land.shp" 
nineline_path <- "D:/POC research/data/Chinamap_beta/South_China_Sea.shp"
Basin_path <- "D:/POC research/data/basin/basin_full.shp"
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

# 将散点数据转换为sf对象
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

# 创建共用地图基础
base_map <- ggplot() +
  geom_sf(data = basin_wgs84, fill = "ivory", color = "grey") +
  geom_sf(data = china_map_wgs84, fill = NA, color = "black", alpha = 1) +
  geom_sf(data = nineline_wgs84, fill = "ivory", color = "black") +
  geom_sf(data = tri_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.4) +
  geom_sf(data = stem_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.7) +
  geom_sf(data = data_sf, aes(size = Sampling_Count_Breaks, color = Sampling_Time_Breaks), 
          shape = 21, lwd = 10) +
  scale_size_manual(values = c(1, 2, 3, 4)) +
  scale_color_manual(values = c("purple", "red", "#0072B2", "#009E73", "brown")) +
  theme_minimal(base_size = 12)

# 主地图
main_map <- base_map +
  labs(title = "Riverine POC Sampling Sites", x = "Longitude", y = "Latitude",
       size = "Sampling Count", color = "Sampling Time") +
  coord_sf(xlim = c(70, 135), ylim = c(15, 55)) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.background = element_blank(),
        legend.position = "right")

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
xpos <- 0.607 
ypos <- 0.19  
width <- 0.15  # 子图的宽度
height <- 0.15 # 子图的高度

# 使用cowplot将南海诸岛地图放置在主图的右下角
final_plot <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(south_sea_map, x = xpos, y = ypos, width = width, height = height)

# 绘制最终地图
print(final_plot)