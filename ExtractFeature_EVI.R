# 加载必要的库
library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "D:/POC research/data/3_POC records/monthly_records.xlsx"
evi_dir <- "D:\\POC research\\data\\2_MODIS_indices\\EVI"
basin_tiff_dir <- "D:/POC research/data/1_DEM_watershed/watershed_2"
output_path <- "D:/POC research/data/2_inputData_watershed/EVI_summary.csv"

# 读取站点数据
stations <- read_excel(stations_path)

# 获取所有evi文件
evi_files <- list.files(path = evi_dir, pattern = "*.tif$", full.names = TRUE)

# 定义提取年份和月份的函数
extract_year_month <- function(file_name) {
  parts <- strsplit(basename(file_name), "_")[[1]]
  year <- as.numeric(parts[3])
  month <- as.numeric(parts[4])
  return(c(year, month))
}

# 定义月份的名称列表
month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# 定义四季月份对应的索引
season_indices <- list(
  spring = c(3, 4, 5),  # 春季：3月、4月、5月
  summer = c(6, 7, 8),  # 夏季：6月、7月、8月
  autumn = c(9, 10, 11),# 秋季：9月、10月、11月
  winter = c(12, 1, 2)  # 冬季：12月、1月、2月
)

# 对所有evi栅格进行预处理（投影、分辨率、边界对齐）
preprocess_evi_files <- function(evi_file, reference_raster) {
  evi_rast <- rast(evi_file)
  
  # 对evi值除以10000以还原实际值
  evi_rast <- evi_rast / 10000
  
  # 对齐投影、分辨率和边界
  aligned_evi <- project(evi_rast, crs(reference_raster))
  aligned_evi <- resample(aligned_evi, reference_raster, method = "near")
  cropped_evi <- crop(aligned_evi, ext(reference_raster))
  
  return(cropped_evi)
}

# 对齐evi栅格的函数：当栅格与参考栅格的边界不一致时执行投影、重采样和裁剪操作
align_raster_to_reference <- function(input_raster, reference_raster) {
  if (!compareGeom(input_raster, reference_raster, stopOnError = FALSE)) {
    aligned_raster <- project(input_raster, crs(reference_raster))
    aligned_raster <- resample(aligned_raster, reference_raster, method = "near")
    cropped_raster <- crop(aligned_raster, ext(reference_raster))
    return(cropped_raster)
  } else {
    return(input_raster)
  }
}

# 对所有evi文件进行预处理并存储
preprocessed_evi_list <- lapply(evi_files, preprocess_evi_files, reference_raster = rast(evi_files[1]))

# 计算evi变量的函数，并加载相应的流域TIF文件
calculate_evi_for_station <- function(station_id, sampling_year) {
  cat(sprintf("Processing station %s for year %d\n", station_id, sampling_year))
  
  # 动态加载与站点ID相对应的流域TIF文件
  basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))
  if (!file.exists(basin_tiff_file)) {
    warning(sprintf("Basin TIF file for station %s not found!", station_id))
    return(NULL)
  }
  basin_tiff <- rast(basin_tiff_file)
  
  # 初始化结果数据框，确保至少有一行
  evi_variables <- data.frame(matrix(nrow = 1, ncol = 0))
  
  # 初始化存储每个季节evi值的列表
  season_evi <- list(
    spring = numeric(0),
    summer = numeric(0),
    autumn = numeric(0),
    winter = numeric(0)
  )
  
  # 遍历evi文件，并筛选出该采样年份的数据
  for (i in seq_along(evi_files)) {
    year_month <- extract_year_month(evi_files[i])
    year <- year_month[1]
    month <- year_month[2]
    
    if (year != sampling_year) {
      next  # 跳过非采样年份的数据
    }
    
    # 检查并输出 month 和 month_name
    if (month >= 1 && month <= 12) {
      month_name <- month_names[month]
    } else {
      next
    }
    
    # 提取该月的 evi 栅格
    evi_rast <- preprocessed_evi_list[[i]]
    
    # 对齐evi栅格到参考流域栅格
    evi_rast <- align_raster_to_reference(evi_rast, basin_tiff)
    
    # 使用流域掩膜裁剪evi栅格
    masked_evi <- mask(evi_rast, basin_tiff)
    plot(masked_evi)
    
    # 检查masked_evi是否为空或无效
    if (is.null(masked_evi) || ncell(masked_evi) == 0) {
      evi_variables[[month_name]] <- NA
      next
    }
    
    # 计算evi均值
    evi_mean <- global(masked_evi, "mean", na.rm = TRUE)$mean
    if (!is.na(evi_mean) && !is.null(evi_mean)) {
      # 存储每月 evi 值
      evi_variables[[month_name]] <- evi_mean
      
      # 根据月份添加到对应季节
      if (month %in% season_indices$spring) {
        season_evi$spring <- c(season_evi$spring, evi_mean)
      } else if (month %in% season_indices$summer) {
        season_evi$summer <- c(season_evi$summer, evi_mean)
      } else if (month %in% season_indices$autumn) {
        season_evi$autumn <- c(season_evi$autumn, evi_mean)
      } else if (month %in% season_indices$winter) {
        season_evi$winter <- c(season_evi$winter, evi_mean)
      }
    } else {
      evi_variables[[month_name]] <- NA
    }
  }
  
  # 计算每个季节的evi均值
  evi_variables$spring_evi <- if (length(season_evi$spring) > 0) mean(season_evi$spring, na.rm = TRUE) else NA
  evi_variables$summer_evi <- if (length(season_evi$summer) > 0) mean(season_evi$summer, na.rm = TRUE) else NA
  evi_variables$autumn_evi <- if (length(season_evi$autumn) > 0) mean(season_evi$autumn, na.rm = TRUE) else NA
  evi_variables$winter_evi <- if (length(season_evi$winter) > 0) mean(season_evi$winter, na.rm = TRUE) else NA
  
  cat(sprintf("Finished station %s for year %d\n", station_id, sampling_year))
  return(evi_variables)
}

# 初始化一个结果数据框
evi_results <- data.frame()

# 对所有站点逐行计算evi变量
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  sampling_time <- as.Date(stations$Sampling_time[i])
  sampling_year <- as.numeric(format(sampling_time, "%Y"))
  
  # 计算evi变量
  evi_variables <- calculate_evi_for_station(station_id, sampling_year)
  if (!is.null(evi_variables)) {
    station_results <- data.frame(ID = station_id, evi_variables)
    evi_results <- bind_rows(evi_results, station_results)
  }
  gc()
}

# 查看结果
print(evi_results)

# 写入csv文件
write_csv(evi_results, output_path)
