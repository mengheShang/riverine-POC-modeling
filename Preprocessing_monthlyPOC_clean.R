library(writexl)
library(readxl)
library(readr)
library(dplyr)
library(lubridate)
library(openxlsx)

# ==================== 第一部分：计算月平均值 ====================
setwd("E:/POC research/data/3_POC records")

# 文件路径
input_file <- "filled_records_GRADES.xlsx"
interim_file <- "monthly_records_GRADES.xlsx"
final_output <- "clean_records_GRADES.xlsx"
tif_folder <- file.path("E:/POC research/data/1_DEM_watershed/watershed_new_500m")

# 数据预处理流程
process_monthly_data <- function() {
  # 读取原始数据
  sites <- read_excel(input_file)
  
  # 数据清洗与转换
  sites_clean <- sites %>%
    filter(
      !is.na(`Water_discharge(m3/s)`),
      !is.na(`POC(mg/L)`)
    ) %>%
    mutate(
      Sampling_Time = as.Date(Sampling_Time),
      Sampling_time = floor_date(Sampling_Time, "month")
    )
  
  # 计算月平均值，同时统计模型填充占比（可选）
  monthly_means <- sites_clean %>%
    group_by(Sampling_time, Latitude, Longitude) %>%
    summarize(
      across(
        c(`Water_discharge(m3/s)`, `POC(mg/L)`, `POC_flux(g/s)`),
        ~ mean(., na.rm = TRUE)
      ),
      Discharge_filled_ratio = mean(as.logical(Discharge_filled_by_model), na.rm = TRUE),  # 平均填充比例
      .groups = "drop"
    ) %>%
    mutate(
      Discharge_filled_by_model = Discharge_filled_ratio > 0  # 如果该月有任何一个记录是模型填充，则标记为 TRUE
    ) %>%
    select(-Discharge_filled_ratio)
  
  # 合并数据：保留ID信息
  result <- sites_clean %>%
    select(ID, Sampling_time, Latitude, Longitude) %>%
    distinct() %>%
    right_join(monthly_means, by = c("Sampling_time", "Latitude", "Longitude")) %>%
    group_by(Sampling_time, Latitude, Longitude) %>%
    slice_min(ID) %>%
    ungroup() %>%
    mutate(
      `POC_flux(g/s)` = ifelse(
        `POC_flux(g/s)` == 0 | is.na(`POC_flux(g/s)`),
        `POC(mg/L)` * `Water_discharge(m3/s)` / 1000,
        `POC_flux(g/s)`
      )
    )
  
  # 写入中间文件
  write_xlsx(result, interim_file)
  return(result)
}

# ==================== 第二部分：TIF ID过滤 ====================
filter_tif_ids <- function(data) {
  tif_files <- list.files(
    path = tif_folder,
    pattern = "\\.tif$",
    ignore.case = TRUE
  )
  tif_ids <- tools::file_path_sans_ext(tif_files)
  
  filtered <- data %>%
    filter(ID %in% tif_ids) %>%
    arrange(ID)
  
  # 写入最终文件
  write.xlsx(filtered, final_output, rowNames = FALSE)
  return(filtered)
}

# ==================== 执行主流程 ====================
monthly_data <- process_monthly_data()
final_data <- filter_tif_ids(monthly_data)

cat("处理完成！\n",
    "中间文件保存至:", file.path(getwd(), interim_file), "\n",
    "最终结果保存至:", file.path(getwd(), final_output))

