# 加载必要的库
library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
ndvi_dir <- "E:\\POC research\\data\\2_MODIS_indices\\NDVI"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_2"
output_path <- "E:/POC research/data/2_inputData_watershed/NDVI_summary.csv"

# 读取站点数据
stations <- read_excel(stations_path)

# 获取所有NDVI文件
ndvi_files <- list.files(path = ndvi_dir, pattern = "*.tif$", full.names = TRUE)

# 定义提取年份和月份的函数
extract_year_month <- function(file_name) {
  parts <- strsplit(basename(file_name), "_")[[1]]
  year <- as.numeric(parts[3])
  month <- as.numeric(parts[4])
  return(c(year, month))
}

# 对齐NDVI栅格的函数：当栅格与参考栅格的边界不一致时执行投影、重采样和裁剪操作
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

# 计算NDVI变量的函数，并加载相应的流域TIF文件
calculate_ndvi_for_station <- function(station_id, sampling_time) {
  sampling_year <- as.numeric(format(sampling_time, "%Y"))
  cat(sprintf("Processing station %s for year %d\n", station_id, sampling_year))
  
  # 动态加载与站点ID相对应的流域TIF文件
  basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))
  if (!file.exists(basin_tiff_file)) {
    warning(sprintf("Basin TIF file for station %s not found!", station_id))
    return(NULL)
  }
  basin_tiff <- rast(basin_tiff_file)
  
  # 初始化结果数据框
  ndvi_variables <- data.frame(
    ID = station_id,
    NDVI = NA,
    NDVI_year = NA,
    NDVI_back1 = NA,
    NDVI_back2 = NA,
    NDVI_back3 = NA,
    sum_NDVI3 = NA
  )
  
  # 提取采样年份和前三年的NDVI数据
  years_of_interest <- c(sampling_year, sampling_year - 1, sampling_year - 2, sampling_year - 3)
  ndvi_values <- list()
  
  for (year in years_of_interest) {
    # 初始化该年份的NDVI值向量
    year_ndvi <- numeric(0)
    
    # 遍历NDVI文件，筛选出该年份的数据
    for (i in seq_along(ndvi_files)) {
      year_month <- extract_year_month(ndvi_files[i])
      file_year <- year_month[1]
      
      if (file_year != year) {
        next  # 跳过非目标年份的数据
      }
      
      # 加载并处理NDVI栅格
      ndvi_rast <- rast(ndvi_files[i]) / 10000  # 直接加载并除以10000
      
      # 对齐NDVI栅格到参考流域栅格
      ndvi_rast <- align_raster_to_reference(ndvi_rast, basin_tiff)
      
      # 使用流域掩膜裁剪NDVI栅格
      masked_ndvi <- mask(ndvi_rast, basin_tiff)
      
      # 计算NDVI均值
      ndvi_mean <- global(masked_ndvi, "mean", na.rm = TRUE)$mean
      if (!is.na(ndvi_mean)) {
        year_ndvi <- c(year_ndvi, ndvi_mean)
      }
      
      # 清理临时栅格对象
      rm(ndvi_rast, masked_ndvi)
      gc()
    }
    
    # 计算该年份的平均NDVI
    if (length(year_ndvi) > 0) {
      ndvi_values[[as.character(year)]] <- mean(year_ndvi, na.rm = TRUE)
    } else {
      ndvi_values[[as.character(year)]] <- NA
    }
    
    # 清理临时变量
    rm(year_ndvi)
    gc()
  }
  
  # 填充结果数据框
  if (as.character(sampling_year) %in% names(ndvi_values)) {
    ndvi_variables$NDVI <- ndvi_values[[as.character(sampling_year)]]
    ndvi_variables$NDVI_year <- ndvi_values[[as.character(sampling_year)]]
  }
  
  if (as.character(sampling_year - 1) %in% names(ndvi_values)) {
    ndvi_variables$NDVI_back1 <- ndvi_values[[as.character(sampling_year - 1)]]
  }
  
  if (as.character(sampling_year - 2) %in% names(ndvi_values)) {
    ndvi_variables$NDVI_back2 <- ndvi_values[[as.character(sampling_year - 2)]]
  }
  
  if (as.character(sampling_year - 3) %in% names(ndvi_values)) {
    ndvi_variables$NDVI_back3 <- ndvi_values[[as.character(sampling_year - 3)]]
  }
  
  # 计算过去三年NDVI的和
  if (!is.na(ndvi_variables$NDVI_back1) && !is.na(ndvi_variables$NDVI_back2) && !is.na(ndvi_variables$NDVI_back3)) {
    ndvi_variables$sum_NDVI3 <- sum(c(ndvi_variables$NDVI_back1, ndvi_variables$NDVI_back2, ndvi_variables$NDVI_back3), na.rm = TRUE)
  }
  
  # 清理流域栅格对象
  rm(basin_tiff)
  gc()
  
  cat(sprintf("Finished station %s for year %d\n", station_id, sampling_year))
  return(ndvi_variables)
}

# 初始化一个结果数据框
ndvi_results <- data.frame()

# 对所有站点逐行计算NDVI变量
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  Sampling_time <- as.Date(stations$Sampling_time[i])
  
  # 计算NDVI变量
  ndvi_variables <- calculate_ndvi_for_station(station_id, Sampling_time)
  if (!is.null(ndvi_variables)) {
    ndvi_results <- bind_rows(ndvi_results, ndvi_variables)
  }
  
  # 显式清理内存
  gc()
  
  # 每处理5个站点后强制清理内存
  if (i %% 5 == 0) {
    gc(reset = TRUE)
    cat(sprintf("Memory cleanup after processing %d stations\n", i))
  }
}

# 查看结果
print(ndvi_results)

# 写入csv文件
write_csv(ndvi_results, output_path)
