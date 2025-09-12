library(readxl)
library(dplyr)
library(ggplot2)

if (!require("rnaturalearth")) install.packages("rnaturalearth")
if (!require("rnaturalearthdata")) install.packages("rnaturalearthdata")
library(rnaturalearth)
library(rnaturalearthdata)

# 读取Excel文件
excel_path <- "C:/Users/smh/POC research/POC records/records.xlsx"
data <- read_excel(excel_path)

# 转换Sampling_Time为日期格式
parse_mixed_dates <- function(date_string) {
  date <- NA
  # 尝试解析 DD/MM/YY 格式
  if (grepl("^\\d{2}/\\d{2}/\\d{2}$", date_string)) {
    date <- tryCatch(as.Date(date_string, format = "%d/%m/%y"), error = function(e) NA)
  }
  # 尝试解析 DD/MM/YYYY 格式
  if (is.na(date) && grepl("^\\d{2}/\\d{2}/\\d{4}$", date_string)) {
    date <- tryCatch(as.Date(date_string, format = "%d/%m/%Y"), error = function(e) NA)
  }
  # 处理 “月份. 年份” 格式，如 "Jul. 2005"
  if (is.na(date) && grepl("^[A-Za-z]+\\. \\d{4}$", date_string)) {
    date_string <- paste("01", gsub("\\.", "", date_string))
    date <- tryCatch(as.Date(date_string, format = "%d %b %Y"), error = function(e) NA)
  }
  return(date)
}

# 读取数据
excel_path <- "C:/Users/smh/POC research/POC records/records.xlsx" 
data <- read_excel(excel_path) 

# 转换为字符型以避免非字符串输入
data$Sampling_Time <- as.character(data$Sampling_Time)

# 应用解析函数
data$Parsed_Sampling_Time <- sapply(data$Sampling_Time, parse_mixed_dates)

# 将数字转换回日期格式
data$Parsed_Sampling_Time <- as.Date(data$Parsed_Sampling_Time, origin = "1970-01-01")


# 计算每个站点的总采样次数
sampling_counts <- data %>%
  group_by(Station_ID) %>%
  summarise(Total_Sampling_Count = n())

# 将总采样次数合并回原始数据集
data <- data %>%
  left_join(sampling_counts, by = "Station_ID")

# 获取中国地图数据
china_map <- ne_countries(country = "china", returnclass = "sf")

# 检查是否有NA值
sum(is.na(data$Parsed_Sampling_Time))

# 使用ggplot2绘制地图

ggplot() +
  geom_sf(data = china_map, fill = "antiquewhite", color = "black") + # 绘制中国地图
  geom_point(data = data, aes(x = Longitude, y = Latitude, size = Total_Sampling_Count, color = Parsed_Sampling_Time),
             alpha = 0.7) + # 添加散点图
  scale_size_continuous(name = "总采样次数") +
  scale_color_date(name = "采样时间", 
                   low = "blue", 
                   high = "red", 
                   ) +
  labs(title = "水文站点分布", x = "经度", y = "纬度") +
  theme_minimal() +
  coord_sf(xlim = c(73, 135), ylim = c(18, 54), expand = FALSE) # 限制坐标范围以专注于中国


# 保存图像到文件
ggsave("Station_Sampling_Map.png", width = 10, height = 8, dpi = 300)