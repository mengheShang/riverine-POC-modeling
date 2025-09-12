library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_2"
ndvi_yearly_dir <- "E:/POC research/data/2_MODIS_indices/NDVI_yearly"
output_path <- "E:/POC research/data/2_inputData_watershed/NDVI_summary.csv"

# 读取站点数据
stations <- read_excel(stations_path)

# 加载每年 NDVI 文件，并按年份命名
ndvi_files <- list.files(ndvi_yearly_dir, pattern = "NDVI_\\d{4}\\.tif$", full.names = TRUE)
names(ndvi_files) <- gsub(".*NDVI_(\\d{4})\\.tif$", "\\1", ndvi_files)

# 对齐栅格函数
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

# 主函数：提取过去三年 NDVI
calculate_ndvi_for_station <- function(station_id, sampling_time) {
  sampling_year <- as.numeric(format(sampling_time, "%Y"))
  cat(sprintf("Processing station %s for year %d\n", station_id, sampling_year))
  
  basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))
  if (!file.exists(basin_tiff_file)) {
    warning(sprintf("Basin TIF file for station %s not found!", station_id))
    return(NULL)
  }
  basin_tiff <- rast(basin_tiff_file)
  
  ndvi_variables <- data.frame(
    ID = station_id,
    NDVI_1back = NA,
    NDVI_2back = NA,
    NDVI_3back = NA,
    sum_NDVI3 = NA
  )
  
  years_of_interest <- c(sampling_year - 1, sampling_year - 2, sampling_year - 3)
  ndvi_values <- list()
  
  for (year in years_of_interest) {
    file_path <- ndvi_files[as.character(year)]
    if (is.na(file_path) || !file.exists(file_path)) next
    
    ndvi_rast <- rast(file_path)
    ndvi_rast <- align_raster_to_reference(ndvi_rast, basin_tiff)
    masked_ndvi <- mask(ndvi_rast, basin_tiff)
    
    plot(masked_ndvi)
    
    ndvi_mean <- global(masked_ndvi, "mean", na.rm = TRUE)$mean
    ndvi_values[[as.character(year)]] <- ndvi_mean
    
    rm(ndvi_rast, masked_ndvi)
    gc()
  }
  
  if (!is.null(ndvi_values[[as.character(sampling_year - 1)]]))
    ndvi_variables$NDVI_1back <- ndvi_values[[as.character(sampling_year - 1)]]
  if (!is.null(ndvi_values[[as.character(sampling_year - 2)]]))
    ndvi_variables$NDVI_2back <- ndvi_values[[as.character(sampling_year - 2)]]
  if (!is.null(ndvi_values[[as.character(sampling_year - 3)]]))
    ndvi_variables$NDVI_3back <- ndvi_values[[as.character(sampling_year - 3)]]
  
  # 求和
  ndvi_variables$sum_NDVI3 <- sum(unlist(ndvi_values), na.rm = TRUE)
  
  rm(basin_tiff)
  gc()
  
  cat(sprintf("Finished station %s\n", station_id))
  return(ndvi_variables)
}

# 主循环
ndvi_results <- data.frame()

for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  sampling_time <- as.Date(stations$Sampling_time[i])
  
  ndvi_variables <- calculate_ndvi_for_station(station_id, sampling_time)
  if (!is.null(ndvi_variables)) {
    ndvi_results <- bind_rows(ndvi_results, ndvi_variables)
  }
  
  gc()
  if (i %% 5 == 0) {
    gc(reset = TRUE)
    cat(sprintf("Memory cleanup after %d loops\n", i))
  }
}

# 保存结果
write_csv(ndvi_results, output_path)


## 转化为年尺度 ####
library(terra)

# NDVI 原始月度文件夹
ndvi_dir <- "E:/POC research/data/2_MODIS_indices/NDVI"
ndvi_files <- list.files(ndvi_dir, pattern = "\\.tif$", full.names = TRUE)

# 提取年份函数
extract_year_month <- function(file_name) {
  parts <- strsplit(basename(file_name), "_")[[1]]
  year <- as.numeric(parts[3])
  month <- as.numeric(parts[4])
  return(c(year, month))
}

# 按年份分组
ndvi_by_year <- split(ndvi_files, sapply(ndvi_files, \(f) extract_year_month(f)[1]))

# 年均输出目录
ndvi_yearly_dir <- "E:/POC research/data/2_MODIS_indices/NDVI_yearly"
dir.create(ndvi_yearly_dir, showWarnings = FALSE)

# 循环生成每年的年均 NDVI 栅格
for (yr in names(ndvi_by_year)) {
  files <- ndvi_by_year[[yr]]
  cat("Processing year:", yr, "\n")
  
  rst_list <- lapply(files, \(f) rast(f) / 10000)
  rst_stack <- rast(rst_list)
  
  yearly_mean <- app(rst_stack, mean, na.rm = TRUE)
  out_file <- file.path(ndvi_yearly_dir, paste0("NDVI_", yr, ".tif"))
  writeRaster(yearly_mean, out_file, overwrite = TRUE)
  
  rm(rst_list, rst_stack, yearly_mean)
  gc()
  terra::tmpFiles(remove = TRUE)
}

