
library(terra)
library(readxl)
library(sp)
library(readr)
#####
# 设置输入输出路径
sites_path <- "D:/POC research/data/POC records/all records.xlsx"
basin_path <- "D:/POC research/data/basin/8basin_ID.tif"

# 读取站点数据
sites <- read_excel(sites_path)
# 去除经纬度缺失值的行
sites <- sites %>%
  filter(!is.na(Longitude) & !is.na(Latitude))
coordinates(sites) <- ~Longitude + Latitude
proj4string(sites) <- CRS("+proj=longlat +datum=WGS84")

# 读取流域数据
basin_raster <- rast(basin_path)

# 创建寻找最近非空栅格值的函数
get_row_col <- function(raster, cell) {
  row_col <- rowColFromCell(raster, cell)
  return(list(row = row_col[1], col = row_col[2]))
}

get_adjacent_cells <- function(raster, cell, radius) {
  dim_raster <- dim(raster)  # 获取栅格的维度（行和列数）
  row_col <- get_row_col(raster, cell)  # 获取当前单元的行和列号
  row <- row_col$row
  col <- row_col$col
  
  adjacent_cells <- c()  # 初始化相邻单元的列表
  
  for (i in -radius:radius) {
    for (j in -radius:radius) {
      if (i == 0 && j == 0) next  # 跳过中心单元
      new_row <- max(1, min(row + i, dim_raster[1]))
      new_col <- max(1, min(col + j, dim_raster[2]))
      new_cell <- cellFromRowCol(raster, new_row, new_col)  # 获取新的单元号
      adjacent_cells <- c(adjacent_cells, new_cell)  # 添加到相邻单元的列表中
    }
  }
  return(adjacent_cells)
}

find_nearest_non_na <- function(raster, point) {
  cell <- cellFromXY(raster, coordinates(point))
  if (!is.na(values(raster)[cell])) {
    return(values(raster)[cell])
  }
  
  radius <- 1
  while (TRUE) {
    cells <- get_adjacent_cells(raster, cell, radius)
    non_na_cells <- cells[!is.na(values(raster)[cells])]
    if (length(non_na_cells) > 0) {
      return(values(raster)[non_na_cells[1]])
    }
    radius <- radius + 1
  }
}

# 提取每个站点的流域ID
basin_ids <- vector(length = nrow(sites))

for (i in 1:nrow(sites)) {
  point <- sites[i, ]
  basin_id <- find_nearest_non_na(basin_raster, point)
  
  # Debugging: Print the site index and basin_id
  print(paste("Site index:", i, "Basin ID:", basin_id))
  
  # Ensure basin_id is correctly retrieved
  if (!is.na(basin_id)) {
    basin_ids[i] <- basin_id
  } else {
    basin_ids[i] <- NA  # Handle NA values appropriately
  }
}

# 将流域ID添加到站点数据中
sites <- as.data.frame(sites)
sites$Basin_ID <- basin_ids

# 将日期列转换为日期格式
sites$date <- ymd(sites$Parsed_Sampling_Time)
# 提取月份信息
sites$month <- month(sites$date)

#####
## 年平均通量
# 按流域ID和月份计算POC通量的平均值、标准误及数据量
poc_avg <- sites %>%
  filter(`POC flux(g/s)` > 0) %>%
  group_by(Basin_ID, month) %>%
  summarize(avg_poc = mean(`POC flux(g/s)`, na.rm = TRUE),
            se_poc = sd(`POC flux(g/s)`, na.rm = TRUE) / sqrt(n()),
            data_count = sum(!is.na(`POC flux(g/s)`)))

# 采样次数范围
summary(poc_avg$data_count)
breaks_manual <- c(0, 5, 10, 15, Inf)
labels_manual <- c("<5", "5-10", "10-15", ">15")
poc_avg$count_Breaks <- cut(
  poc_avg$data_count,
  breaks = breaks_manual,
  include.lowest = TRUE,
  labels = labels_manual)
size_mapping <- c("<5" = 1, "5-10" = 2, "10-15" = 3, ">15" = 4)

# 创建流域ID和名称的映射
basin_names <- c(
  "1" = "长江流域",
  "2" = "西南国际河流流域",
  "3" = "黄河流域",
  "4" = "淮河流域",
  "5" = "海河流域",
  "6" = "东南沿海诸河流域",
  "7" = "东北诸河流域",
  "8" = "珠江流域"
)
poc_avg$Basin_Name <- basin_names[as.character(poc_avg$Basin_ID)]

# 定义颜色映射
color_mapping <- c(
  "长江流域" = "purple",
  "黄河流域" = "#F0E442",
  "淮河流域" = "#009E73",
  "海河流域" = "darkgreen",
  "东南沿海诸河流域" = "#0072B2",
  "东北诸河流域" = "brown",
  "珠江流域" = "#CC79A7"
)
# 绘制POC通量的月际变化并显示数据量和误差线段
ggplot(poc_avg, aes(x = factor(month), y = avg_poc, group = Basin_Name, color = as.factor(Basin_Name))) +
  geom_line() +
  geom_point(aes(size = count_Breaks)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Monthly Variation of POC Flux by Basin",
       x = "Month",
       y = "Average POC flux(g/s)",
       size = "Data Count",
       color = "Basin ID") +  # 图例中添加流域ID说明
  scale_color_manual(values = color_mapping) +
  scale_size_manual(values = size_mapping) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  guides(color = guide_legend(order = 1),
         size = guide_legend(order = 2)) +  # 固定图例的顺序
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")

# 单独绘制
basin_list <- unique(poc_avg$Basin_Name)
for (basin in basin_list) {
  plot_data <- poc_avg %>% filter(Basin_Name == basin)
  
  p <- ggplot(plot_data, aes(x = factor(month), y = avg_poc, group = Basin_Name, color = Basin_Name)) +
    geom_line() +
    geom_point(aes(size = count_Breaks)) +  # 使用点的大小表示数据量
    geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
    labs(title = paste("Monthly Variation of POC Flux in", basin),
         x = "Month",
         y = "Average POC flux(g/s)",
         size = "Data Count",
         color = "Basin Name") +  # 图例中添加流域ID说明
    scale_color_manual(values = color_mapping) +  # 使用固定的颜色映射
    scale_size_manual(values = size_mapping) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    guides(color = guide_legend(order = 1),
           size = guide_legend(order = 2)) +  # 固定图例的顺序
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = "right")
  
  print(p)
}

#####
# 逐月通量
path <- "D:/POC research/data/POC records/sites_with_basin_id.csv"
sites <- read_csv(path)
sites$date <- as.Date(sites$date)
sites$YearMonth <- floor_date(sites$date, "month")

# 按流域ID、年份和月份计算POC通量的平均值、标准误及数据量
poc_avg <- sites %>%
  filter(`POC flux(g/s)` > 0) %>%
  group_by(Basin_ID, YearMonth) %>%
  summarize(avg_poc = mean(`POC flux(g/s)`, na.rm = TRUE),
            se_poc = sd(`POC flux(g/s)`, na.rm = TRUE) / sqrt(n()),
            data_count = sum(!is.na(`POC flux(g/s)`)))

# 采样次数范围
summary(poc_avg$data_count)
breaks_manual <- c(0, 1.5, 2.5, 3.5, Inf)
labels_manual <- c("≤1", "2", "3", "≥4")
poc_avg$count_Breaks <- cut(
  poc_avg$data_count,
  breaks = breaks_manual,
  include.lowest = TRUE,
  labels = labels_manual)
size_mapping <- c("≤1" = 1, "2" = 2, "3" = 3, "≥4" = 4)

# 创建流域ID和名称的映射
basin_names <- c(
  "1" = "长江流域",
  "2" = "西南国际河流流域",
  "3" = "黄河流域",
  "4" = "淮河流域",
  "5" = "海河流域",
  "6" = "东南沿海诸河流域",
  "7" = "东北诸河流域",
  "8" = "珠江流域"
)
poc_avg$Basin_Name <- unname(basin_names[as.character(poc_avg$Basin_ID)])

# 定义颜色映射
color_mapping <- c(
  "长江流域" = "purple",
  "黄河流域" = "#F0E442",
  "淮河流域" = "#009E73",
  "海河流域" = "darkgreen",
  "东南沿海诸河流域" = "#0072B2",
  "东北诸河流域" = "brown",
  "珠江流域" = "#CC79A7"
)

# 绘制POC通量的月际变化并显示数据量和误差线段
ggplot(poc_avg, aes(x = factor(YearMonth), y = avg_poc, group = Basin_Name, color = factor(Basin_Name))) +
  geom_line() +
  geom_point(aes(size = count_Breaks)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Monthly Variation of POC Flux by Basin",
       x = "Month",
       y = "Average POC flux(g/s)",
       size = "Data Count",
       color = "Basin Name") +
  scale_color_manual(values = color_mapping) +
  scale_size_manual(values = size_mapping) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  guides(color = guide_legend(order = 1),
         size = guide_legend(order = 2)) +  # 固定图例的顺序
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "right")

# 单独绘制每个流域的逐年POC通量变化
basin_list <- unique(poc_avg$Basin_Name)
for (basin in basin_list) {
  plot_data <- poc_avg %>% filter(Basin_Name == basin)

  p <- ggplot(plot_data, aes(x = factor(YearMonth), y = avg_poc, group = Basin_Name, color = factor(Basin_Name))) +
    geom_line() +
    geom_point(aes(size = count_Breaks)) +  # 使用点的大小表示数据量
    geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
    labs(title = paste("Monthly Variation of POC Flux in", basin), 
         x = "Month",
         y = "Average POC flux(g/s)",
         size = "Data Count",
         color = "Basin Name") +
    scale_color_manual(values = color_mapping) +
    scale_size_manual(values = size_mapping) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    guides(color = guide_legend(order = 1),
           size = guide_legend(order = 2)) +  # 固定图例的顺序
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  print(p)
}
    

#####
# 年平均浓度
path <- "D:/POC research/data/POC records/sites_with_basin_id.csv"
sites <- read_csv(path)

# 按流域ID和月份计算POC浓度的平均值、标准误及数据量
poc_avg <- sites %>%
  filter(`POC (mg/L)` > 0) %>%
  group_by(Basin_ID, month) %>%
  summarize(avg_poc = mean(`POC (mg/L)`, na.rm = TRUE),
            se_poc = sd(`POC (mg/L)`, na.rm = TRUE) / sqrt(n()),
            data_count = sum(!is.na(`POC (mg/L)`)))

summary(poc_avg$data_count)
breaks_manual <- c(0, 10, 15, 20, Inf)
labels_manual <- c("<10", "10-15", "15-20", ">20")
poc_avg$count_Breaks <- cut(
  poc_avg$data_count,
  breaks = breaks_manual,
  include.lowest = TRUE,
  labels = labels_manual)
size_mapping <- c("<10" = 1, "10-15" = 2, "15-20" = 3, ">20" = 4)

# 创建流域ID和名称的映射
basin_names <- c(
  "1" = "长江流域",
  "2" = "西南国际河流流域",
  "3" = "黄河流域",
  "4" = "淮河流域",
  "5" = "海河流域",
  "6" = "东南沿海诸河流域",
  "7" = "东北诸河流域",
  "8" = "珠江流域"
)
poc_avg$Basin_Name <- basin_names[as.character(poc_avg$Basin_ID)]

# 定义颜色映射
color_mapping <- c(
  "长江流域" = "purple",
  "黄河流域" = "#F0E442",
  "淮河流域" = "#009E73",
  "海河流域" = "darkgreen",
  "东南沿海诸河流域" = "#0072B2",
  "东北诸河流域" = "brown",
  "珠江流域" = "#CC79A7"
)

# 绘制POC浓度的月际变化并显示数据量和误差线段
ggplot(poc_avg, aes(x = factor(month), y = avg_poc, group = Basin_Name, color = as.factor(Basin_Name))) +
  geom_line() +
  geom_point(aes(size = count_Breaks)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Monthly Variation of POC Concentration by Basin",
       x = "Month",
       y = "Average POC (mg/L)",
       size = "Data Count",
       color = "Basin ID") +  # 图例中添加流域ID说明
  scale_color_manual(values = color_mapping) +
  scale_size_manual(values = size_mapping) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  guides(color = guide_legend(order = 1),
         size = guide_legend(order = 2)) +  # 固定图例的顺序
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right")

# 单独绘制
basin_list <- unique(poc_avg$Basin_Name)
for (basin in basin_list) {
  plot_data <- poc_avg %>% filter(Basin_Name == basin)
  
  p <- ggplot(plot_data, aes(x = factor(month), y = avg_poc, group = Basin_Name, color = Basin_Name)) +
    geom_line() +
    geom_point(aes(size = count_Breaks)) +  # 使用点的大小表示数据量
    geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
    labs(title = paste("Monthly Variation of POC Concentration in", basin),
         x = "Month",
         y = "Average POC (mg/L)",
         size = "Data Count",
         color = "Basin Name") +  # 图例中添加流域ID说明
    scale_color_manual(values = color_mapping) +  # 使用固定的颜色映射
    scale_size_manual(values = size_mapping) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
    guides(color = guide_legend(order = 1),
           size = guide_legend(order = 2)) +  # 固定图例的顺序
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 11),
          legend.position = "right")
  
  print(p)
}
#####
# 逐月浓度
path <- "D:/POC research/data/POC records/sites_with_basin_id.csv"
sites <- read_csv(path)
sites$date <- as.Date(sites$date)
sites$YearMonth <- floor_date(sites$date, "month")

# 按流域ID、年份和月份计算POC通量的平均值、标准误及数据量
poc_avg <- sites %>%
  filter(`POC (mg/L)` > 0) %>%
  group_by(Basin_ID, YearMonth) %>%
  summarize(avg_poc = mean(`POC (mg/L)`, na.rm = TRUE),
            se_poc = sd(`POC (mg/L)`, na.rm = TRUE) / sqrt(n()),
            data_count = sum(!is.na(`POC (mg/L)`)))

# 采样次数范围
summary(poc_avg$data_count)
breaks_manual <- c(0, 3, 5, 10, Inf)
labels_manual <- c("<3", "3-5", "5-10", ">10")
poc_avg$count_Breaks <- cut(
  poc_avg$data_count,
  breaks = breaks_manual,
  include.lowest = TRUE,
  labels = labels_manual)
size_mapping <- c("<3" = 1, "3-5" = 2, "5-10" = 3, ">10" = 4)

# 创建流域ID和名称的映射
basin_names <- c(
  "1" = "长江流域",
  "2" = "西南国际河流流域",
  "3" = "黄河流域",
  "4" = "淮河流域",
  "5" = "海河流域",
  "6" = "东南沿海诸河流域",
  "7" = "东北诸河流域",
  "8" = "珠江流域"
)
poc_avg$Basin_Name <- unname(basin_names[as.character(poc_avg$Basin_ID)])

# 定义颜色映射
color_mapping <- c(
  "长江流域" = "purple",
  "黄河流域" = "#F0E442",
  "淮河流域" = "#009E73",
  "海河流域" = "darkgreen",
  "东南沿海诸河流域" = "#0072B2",
  "东北诸河流域" = "brown",
  "珠江流域" = "#CC79A7"
)

# 绘制POC通量的月际变化并显示数据量和误差线段
ggplot(poc_avg, aes(x = factor(YearMonth), y = avg_poc, group = Basin_Name, color = factor(Basin_Name))) +
  geom_line() +
  geom_point(aes(size = count_Breaks)) +  # 使用点的大小表示数据量
  geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
  labs(title = "Monthly Variation of POC Flux by Basin",
       x = "Month",
       y = "Average POC (mg/L)",
       size = "Data Count",
       color = "Basin Name") +
  scale_color_manual(values = color_mapping) +
  scale_size_manual(values = size_mapping) +
  guides(color = guide_legend(order = 1),
         size = guide_legend(order = 2)) +  # 固定图例的顺序
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "right")

# 单独绘制每个流域的逐年POC通量变化
basin_list <- unique(poc_avg$Basin_Name)
for (basin in basin_list) {
  plot_data <- poc_avg %>% filter(Basin_Name == basin)
  
  p <- ggplot(plot_data, aes(x = factor(YearMonth), y = avg_poc, group = Basin_Name, color = factor(Basin_Name))) +
    geom_line() +
    geom_point(aes(size = count_Breaks)) +  # 使用点的大小表示数据量
    geom_errorbar(aes(ymin = avg_poc - se_poc, ymax = avg_poc + se_poc), width = 0.2) +  # 添加误差线段
    labs(title = paste("Monthly Variation of POC Flux in", basin), 
         x = "Month",
         y = "Average POC (mg/L)",
         size = "Data Count",
         color = "Basin Name") +
    scale_color_manual(values = color_mapping) +
    scale_size_manual(values = size_mapping) +
    guides(color = guide_legend(order = 1),
           size = guide_legend(order = 2)) +  # 固定图例的顺序
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")
  
  print(p)
}