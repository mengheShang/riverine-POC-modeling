# 加载必要的包
library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(cowplot)

setwd("E:\\POC research\\data")

# 读取数据
excel_path <- "2_inputData_watershed\\input_new.xlsx"
data <- read_excel(excel_path)

# 去除所有含有NA的行
data_clean <- data %>%
  filter_all(all_vars(!is.na(.)))

# 为年份创建分段（可选）
data_clean$Year_Breaks <- cut(
  data_clean$Year,
  breaks = c(-Inf, 2000, 2005, 2010, 2015, Inf),
  labels = c("Before 2000", "2000-2005", "2005-2010", "2010-2015", "After 2015")
)

# 设置地图shapefile的路径
bou_path <- "POC_basic data\\国家基础地理数据400万\\bou1_4l.shp" 
Basin_path <- "POC_basic data\\9basins\\liuyu.shp"
RiverStem_path <- "POC_basic data\\国家基础地理数据400万\\hyd1_4l.shp"
RiverTri_path <- "POC_basic data\\国家基础地理数据400万\\hyd2_4l.shp"

# 读取shapefile
bou <- st_read(bou_path)
basin <- st_read(Basin_path)
stem_river <- st_read(RiverStem_path)
tri_river <- st_read(RiverTri_path)

std_crs <- 4326

# 将所有 shapefile 转换为 WGS84 坐标系
bou_proj <- st_transform(bou, crs = std_crs)
basin_proj <- st_transform(basin, crs = std_crs)
stem_river_proj <- st_transform(stem_river, crs = std_crs)
tri_river_proj <- st_transform(tri_river, crs = std_crs)

# 将散点数据转换为sf对象
data_sf <- st_as_sf(data_clean, coords = c("Longitude", "Latitude"), crs = std_crs)

# 修复无效的几何对象
bou <- st_make_valid(bou_proj)
basin <- st_make_valid(basin_proj)
stem_river <- st_make_valid(stem_river_proj)
tri_river <- st_make_valid(tri_river_proj)

base_map_main <- ggplot() +
  geom_sf(data = bou, fill = "ivory", color = "black", linewidth = 0.8) +
  geom_sf(data = basin, fill = "ivory", color = "black") +
  geom_sf(data = tri_river, fill = NA, color = "lightblue", linewidth = 0.4) +
  geom_sf(data = stem_river, fill = NA, color = "lightblue", linewidth = 0.7) +
  geom_sf(data = data_sf, aes(color = Year_Breaks), size = 2, alpha = 0.6) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

# 定义主图和子图的坐标范围
main_xlim <- c(70, 140)
main_ylim <- c(15, 55)
south_sea_xlim <- c(105, 125)
south_sea_ylim <- c(0, 25)

# 裁剪主图和子图
main_map <- base_map_main +
  coord_sf(xlim = main_xlim, ylim = main_ylim, expand = FALSE) +
  labs(title = "Riverine POC Sampling Sites",
       x = "Longitude",
       y = "Latitude",
       color = "Sampling Year") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        plot.background = element_blank(),
        legend.position = "right")

# 创建南海子图
south_sea_map <- base_map_main +
  coord_sf(xlim = south_sea_xlim, ylim = south_sea_ylim, expand = FALSE) +
  guides(color = "none") +
  theme(plot.margin = margin(0, 0, 0, 0), 
        plot.background = element_rect(fill = "white", colour = NA),
        legend.position = "none", 
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

library(grid)

# 将南海子图转换为 grob
south_sea_grob <- ggplotGrob(south_sea_map)

# 计算子图在主图中的位置 (右下角)
submap_xmin <- main_xlim[2] - diff(south_sea_xlim) * 0.8
submap_xmax <- main_xlim[2]
submap_ymin <- main_ylim[1]
submap_ymax <- main_ylim[1] + diff(south_sea_ylim) * 0.8

# 创建最终地图
final_map <- main_map +
  annotation_custom(
    grob = south_sea_grob,
    xmin = submap_xmin,
    xmax = submap_xmax,
    ymin = submap_ymin,
    ymax = submap_ymax
  ) 

# 显示最终地图
print(final_map)