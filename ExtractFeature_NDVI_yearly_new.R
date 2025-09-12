library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m"
ndvi_yearly_dir <- "E:/POC research/data/2_MODIS_indices/NDVI_yearly"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/NDVI.csv"

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

# 主函数：提取当年 NDVI
calculate_ndvi_for_station <- function(station_id, sampling_time) {
  sampling_year <- as.numeric(format(sampling_time, "%Y"))
  cat(sprintf("Processing station %s for year %d\n", station_id, sampling_year))
  
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  if (!file.exists(basin_tiff_file)) {
    warning(sprintf("Basin TIF file for station %s not found!", station_id))
    return(NULL)
  }
  basin_tiff <- rast(basin_tiff_file)
  
  ndvi_variables <- data.frame(
    ID = station_id,
    Sampling_time = stations$Sampling_time[i], 
    NDVI = NA
  )
  
  if (sampling_year > 1999){
    file_path <- ndvi_files[as.character(sampling_year)]
    
    ndvi_rast <- rast(file_path)
    ndvi_rast <- align_raster_to_reference(ndvi_rast, basin_tiff)
    masked_ndvi <- mask(ndvi_rast, basin_tiff)
    plot(masked_ndvi)
    ndvi_variables$NDVI <- global(masked_ndvi, "mean", na.rm = TRUE)$mean
  } else {
    cat(sprintf("No NDVI data available for station %s in year %d\n", station_id, sampling_year))
  }
  
  rm(basin_tiff)
  gc()
  
  cat(sprintf("Finished station %s\n", station_id))
  return(ndvi_variables)
}

# 主循环
ndvi_results <- data.frame()

for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if(station_id<864) next
  sampling_time <- as.Date(stations$Sampling_time[i])
  
  ndvi_variables <- calculate_ndvi_for_station(station_id, sampling_time)
  if (!is.null(ndvi_variables)) {
    ndvi_results <- bind_rows(ndvi_results, ndvi_variables)
  }
  
  gc()
}

# 保存结果
write_csv(ndvi_results, output_path)
