

library(readxl)
library(dplyr)
library(ggplot2)
library(sf)
library(lubridate)
library(cowplot)

# 读取数据
excel_path <- "D:\\POC research\\data\\POC records\\records.xlsx"
data <- read_excel(excel_path)

# 计算每个站点的总采样次数并合并回原始数据集
#data <- data %>%
#  mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude)) %>%
#  group_by(Station_ID) %>%
#  summarise(Total_Sampling_Count = n(), .groups = 'drop') %>%
#  left_join(data, by = "Station_ID")

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

# 创建WGS 84的bbox
#bbox_mainland_wgs84 <- st_bbox(c(xmin = 70,xmax = 130, ymin = 15,ymax = 55), crs = st_crs(4326))
#bbox_island_wgs84 <- st_bbox(c(xmin = 107.913113, xmax = 122.740601, ymin = 2.218933, ymax = 24.385950), crs = st_crs(4326))

#china_map_wgs84 <- st_make_valid(china_map_wgs84)
#basin_wgs84 <- st_make_valid(basin_wgs84)
#stem_river_wgs84 <- st_make_valid(stem_river_wgs84)
#tri_river_wgs84 <- st_make_valid(tri_river_wgs84)
#nineline_wgs84 <- st_make_valid(nineline_wgs84)

#main_land <- st_crop(china_map_wgs84, bbox_mainland_wgs84)
#basin <- st_crop(basin_wgs84, bbox_mainland_wgs84)
#stemriver <- st_crop(stem_river_wgs84, bbox_mainland_wgs84)
#tririver <- st_crop(tri_river_wgs84, bbox_mainland_wgs84)
#nineline_big
#southsea_islands <- st_crop(china_map_wgs84, bbox_island_wgs84)
#nineline_small <- st_crop(nineline_wgs84, bbox_island_wgs84)


# 将bbox转换到Krasovsky_1940_Albers坐标系
#bbox_mainland_krasovsky <- st_transform(st_as_sfc(bbox_mainland_wgs84), crs = st_crs(china_map))
#bbox_island_krasovsky <- st_transform(st_as_sfc(bbox_island_wgs84), crs = st_crs(china_map))

# 使用转换后的bbox来裁剪china_map
#main_land <- st_crop(china_map, bbox_mainland_krasovsky)
#south_sea_islands <- st_crop(china_map, bbox_island_krasovsky)

# 使用转换后的bbox来裁剪nineline图层
#nineline_big <- st_crop(nineline_tran, bbox_mainland_krasovsky)
#nineline_small <- st_crop(nineline_tran, bbox_island_krasovsky)

# 使用转换后的bbox来裁剪basin和river
#basin <- st_crop(basin_tran, bbox_mainland_krasovsky)
#stemriver <- st_crop(stemriver_tran, bbox_mainland_krasovsky)
#tririver <- st_crop(tririver_tran, bbox_mainland_krasovsky)






# 将散点数据转换为sf对象
data_sf <- st_as_sf(data, coords = c("Longitude", "Latitude"), crs = 4326)

# 转换散点数据到WGS 84的投影坐标系统
#data_sf <- st_transform(data_sf, crs = st_crs(china_map))
data_sf <- st_transform(data_sf, crs = 4326)

# 检查Parsed_Sampling_Time列中哪些值是NA
#na_rows <- which(is.na(data$Parsed_Sampling_Time))
# 打印有NA值的行的Station_ID
# na_station_ids <- data$Station_ID[na_rows]
# print(na_station_ids)



# 绘制主要地图
main_map <- 
  ggplot() +
  
  geom_sf(data = basin_wgs84, fill = NA, color = "black") +
  geom_sf(data = china_map_wgs84, fill = "ivory", color = "black", alpha = 0.7) +
  
  geom_sf(data = nineline_wgs84, fill = "ivory", color = "black") +
  geom_sf(data = tri_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.4) +
  geom_sf(data = stem_river_wgs84, fill = NA, color = "lightblue", linewidth = 0.7) +
  
  geom_sf(data = data_sf, aes(size = Sampling_Count_Breaks, color = Sampling_Time_Breaks), 
          shape = 21, lwd = 10) +
  scale_size_manual(values = c(1, 2, 3, 4)) + # 根据实际需要调整大小
  scale_color_manual(values = c("purple", "red", "orange", "chartreuse", "darkgreen")) + 
  labs(title = "Riverine POC Sampling Sites", x = "Longitude", y = "Latitude",
       size = "Sampling Count", color = "Sampling Time") +
  
  coord_sf(xlim = c(70, 135), ylim = c(15, 55)) +
  
  theme_minimal(base_size = 12) + 
  theme(plot.title = element_text(hjust = 0.5), # 标题居中
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1), # 明确设置边框
        plot.background = element_blank(), # 保持背景透明
        legend.position = "right") # 调整图例位置

# 创建子地图
south_sea_map <- 
  ggplot() +
  geom_sf(data = china_map_wgs84, fill = "ivory", color = "black") +
  geom_sf(data = nineline_wgs84, fill = "ivory", color = "black") +
  

  coord_sf(xlim = c(107, 122.8), ylim = c(2.218933, 24.385950)) +
  
  theme_void() +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())
 
#nineline_map <- 
#  ggplot() +
#  geom_sf(data = nineline_small, fill = "ivory", color = "black") +
 
#  theme_void() +
#  theme(plot.background = element_blank(),
#        panel.grid = element_blank(),
#        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
#        axis.text = element_blank(),
#        axis.title = element_blank(),
#        axis.ticks = element_blank(),
#        axis.line = element_blank())

# 定义南海诸岛子图的位置
xpos <- 0.623 # 从右边开始
ypos <- 0.161 # 从底部开始 
width <- 0.15  # 子图的宽度
height <- 0.15 # 子图的高度

# 使用cowplot将南海诸岛地图放置在主图的右下角
final_plot <- ggdraw() +
  draw_plot(main_map) +
  draw_plot(south_sea_map, x = xpos, y = ypos, width = width, height = height)

# 绘制最终地图
print(final_plot)
