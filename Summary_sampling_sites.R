# 加载所需的包
library(readxl)
library(sf)
library(dplyr)
library(openxlsx)

site_path1 <- "D:\\POC research\\data\\2_inputData_watershed\\input.xlsx"

# 读取Excel文件
sample_data <- read_excel(site_path1)

sample_data <- sample_data %>%
  filter_all(all_vars(!is.na(.)))

# 将经纬度转换为空间数据对象
sample_data_sf <- st_as_sf(sample_data, coords = c("Longitude", "Latitude"), crs = 4326)

# 读取流域shapefile
shapefile <- st_read("D:\\POC research\\data\\1_basin\\8basin.shp")

# 修复shapefile中的无效几何体
shapefile <- st_make_valid(shapefile)

# 使用sf包中的函数将站点数据与流域数据进行空间连接
joined_data <- st_join(sample_data_sf, shapefile, join = st_within)

# 找出未匹配的采样点
unmatched_points <- joined_data %>% filter(is.na(NAME))

# 找到未匹配点的最近流域
nearest_features <- st_nearest_feature(unmatched_points, shapefile)

# 获取最近流域的名称
nearest_names <- shapefile$NAME[nearest_features]

# 将未匹配点的流域名称替换为最近流域的名称
unmatched_points$NAME <- nearest_names

# 将替换后的点重新加入原始数据
corrected_data <- rbind(joined_data %>% filter(!is.na(NAME)), unmatched_points)

# 排除经纬度和采样时间都相同的记录
unique_data <- corrected_data %>% distinct(geometry, .keep_all = TRUE)

# 统计各流域总采样数量
total_samples <- corrected_data %>%
  group_by(NAME) %>%
  summarise(总采样数量 = n(), .groups = 'drop')

# 统计各流域有效采样数量（经纬度、采样时间都不同）
valid_samples <- unique_data %>%
  group_by(NAME) %>%
  summarise(有效采样数量 = n(), .groups = 'drop')

# 统计各流域采样站点数量（经纬度不同）
unique_sites <- corrected_data %>%
  distinct(geometry, .keep_all = TRUE) %>%
  group_by(NAME) %>%
  summarise(采样站点数量 = n(), .groups = 'drop')

print(total_samples)
print(valid_samples)
print(unique_sites)

total <- st_set_geometry(total_samples, NULL)
valid <- st_set_geometry(valid_samples, NULL)
unique <- st_set_geometry(unique_sites, NULL)

# 合并汇总结果
summary_table <- total %>%
  left_join(valid, by = "NAME") %>%
  left_join(unique, by = "NAME")

# 打印汇总表格
print(summary_table)


######
library(ggplot2)
library(reshape2)

colnames(summary_table) <- c("NAME", "Total_Samples", "Effective_Samples", "Sampling_Sites")
summary_table$NAME <- c("Northeast River Basin", 
                        "Southeast Coastal River Basin", 
                        "Hai River Basin", 
                        "Huai River Basin", 
                        "Pearl River Basin", 
                        "Yangtze River Basin", 
                        "Yellow River Basin")

summary_table$Effective_Samples <- NULL
summary_table_long <- melt(summary_table, id.vars = "NAME", variable.name = "Type", value.name = "Count")

# 绘制柱状图
ggplot(summary_table_long, aes(x = NAME, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Total Samples and Sampling Sites by River Basin",
       x = "River Basin", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Total_Samples" = "#1f77b4", "Sampling_Sites" = "#4f7942"))  # 使用自定义的颜色