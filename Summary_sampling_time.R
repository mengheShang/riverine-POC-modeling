library(dplyr)
library(readxl)
library(ggplot2)
library(sf)

# 文件地址
tif_folder <- "E:\\POC research\\data\\1_DEM_watershed/watershed"
data_path <- "E:\\POC research\\data\\3_POC records\\filled_records_WaterGAP_add.xlsx"
df <- read_excel(data_path)


basins <- st_read("E:/POC research/data/POC_basic data/9basins/liuyu.shp")
if (any(!st_is_valid(basins))) {
  basins <- st_make_valid(basins)
}

coords <- df %>% select(Latitude, Longitude)
points_sf <- st_as_sf(coords, coords = c("Longitude", "Latitude"), crs = st_crs(basins))
matched_data <- st_join(points_sf, basins, join = st_within)
if (any(is.na(matched_data$W1102WB0_2))) {
  unmatched_points <- matched_data %>% filter(is.na(W1102WB0_2))
  nearest_features <- st_nearest_feature(unmatched_points, basins)
  matched_data$W1102WB0_2[is.na(matched_data$W1102WB0_2)] <- basins$W1102WB0_2[nearest_features]
}
df <- cbind(df, Basin_Name = matched_data$W1102WB0_2)

# Sampling year ####
# Remove rows with NA values
df_cleaned <- na.omit(df)

# Group by 'Sampling_year' and count the number of samples per year
sampling_count_per_year <- as.data.frame(table(df_cleaned$Sampling_year))
colnames(sampling_count_per_year) <- c("Sampling_year", "Sample_Count")

# Convert Sampling_year to numeric (if not already)
sampling_count_per_year$Sampling_year <- as.numeric(as.character(sampling_count_per_year$Sampling_year))

# Plot the results
ggplot(sampling_count_per_year, aes(x = Sampling_year, y = Sample_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Sampling Year", y = "Number of Samples", title = "Number of Samples per Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5) )  +
  scale_x_continuous(breaks = seq(min(sampling_count_per_year$Sampling_year), 
                                  max(sampling_count_per_year$Sampling_year), by = 1))

# Sampling days in one month ####
library(lubridate)

filter_tif_ids <- function(data) {
  # 获取TIF文件ID列表
  tif_files <- list.files(
    path = tif_folder,
    pattern = "\\.tif$",
    ignore.case = TRUE  # 处理大小写不敏感
  )
  tif_ids <- tools::file_path_sans_ext(tif_files)
  
  # 执行过滤
  filtered <- data %>%
    filter(ID %in% tif_ids)%>%
    arrange(ID)

  return(filtered)
}

df <- df %>%
  filter(
    !is.na(`Water discharge2 (m3/s)`),
    !is.na(`POC (mg/L)`)
  )

# 执行第二部分过滤
df <- filter_tif_ids(df)

# 提取年份和月份，并统计每个采样点每月的记录数,计算每个站点每月的统计量
monthly_stats <- df %>%
  mutate(
    Sampling_Time = as.Date(Sampling_Time),
    year = year(Sampling_Time),
    month = month(Sampling_Time)
  ) %>%
  group_by(ID, year, month) %>%
  filter(n() > 5) %>%  # 只保留每月记录数>1的组
  summarise(
    record_count = n(),
    sampling_days = n_distinct(day(Sampling_Time)),
    dates = unique(day(Sampling_Time)) %>% sort() %>% paste(collapse = ", "),
    # 计算均值和标准差
    mean_discharge = mean(`Water discharge2 (m3/s)`, na.rm = TRUE),
    sd_discharge = sd(`Water discharge2 (m3/s)`, na.rm = TRUE),
    mean_POC = mean(`POC (mg/L)`, na.rm = TRUE),
    sd_POC = sd(`POC (mg/L)`, na.rm = TRUE),
    .groups = "drop"
  )

# 统计不同采样天数（sampling_days）出现的频次
freq_table <- monthly_stats %>%
  count(sampling_days, name = "frequency") %>%
  arrange(sampling_days)  # 按天数排序

# 查看统计表
print(freq_table)

library(ggplot2)

# 绘制条形图（适用于离散天数）
ggplot(freq_table, aes(x = factor(sampling_days), y = frequency)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
  geom_text(aes(label = frequency), vjust = -0.5, size = 4) +  # 添加频次标签
  labs(
    title = "频数分布：每月采样天数",
    x = "每月采样天数（天）",
    y = "频次"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # 标题居中
# ####
# 继续您的代码...

# 筛选出月内有多天采样的记录
multi_day_samples <- monthly_stats %>% 
  filter(sampling_days > 2) %>% 
  select(ID, year, month)

# 获取这些记录的原始数据
multi_day_data <- df %>%
  mutate(
    Sampling_Time = as.Date(Sampling_Time),
    year = year(Sampling_Time),
    month = month(Sampling_Time)
  ) %>%
  inner_join(multi_day_samples, by = c("ID", "year", "month")) %>%
  mutate(
    date_label = format(Sampling_Time, "%Y-%m-%d"),  # 用于标注的具体日期
    site_label = paste(ID, Basin_Name, sep = " - ")   # 组合站点ID和流域名称
  )

# 检查是否有数据
if(nrow(multi_day_data) == 0) {
  stop("没有找到月内多天采样的记录")
}

# 为每个站点创建时间序列图
plots <- multi_day_data %>%
  group_by(ID, Basin_Name) %>%
  group_map(~ {
    site_data <- .x
    site_name <- unique(site_data$site_label)
    
    ggplot(site_data, aes(x = Sampling_Time, y = `POC (mg/L)`)) +
      geom_line(color = "blue", alpha = 0.5) +
      geom_point(aes(color = factor(day(Sampling_Time))), size = 3) +
      geom_text(aes(label = format(Sampling_Time, "%d")), 
                hjust = -0.5, vjust = 0.5, size = 3) +
      scale_color_discrete(name = "采样日") +
      labs(
        title = paste("POC浓度时间序列 -", site_name),
        subtitle = "标注数字为当月采样日",
        x = "采样时间",
        y = "POC浓度 (mg/L)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom"
      )
  })

# 显示所有图形
library(purrr)
walk(plots, print)