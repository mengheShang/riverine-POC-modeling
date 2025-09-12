# 加载必要的库
library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
data_dir <- "E:/POC research/data/2_pco2Data/npp"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/NPP.csv"

# 读取站点数据
stations <- read_excel(stations_path)

# 获取所有NPP tiff文件的列表
tiff_files <- list.files(path = data_dir, pattern = "*_new.tif$", full.names = TRUE)
years <- seq(2001, 2001 + length(tiff_files) - 1)  # NPP数据年份范围是2001-2022

# 读取所有NPP TIF文件并存储在列表中
npp_list <- lapply(tiff_files, rast)

# 读取任意一个流域TIF文件作为参考
basin_tiff <- rast("E:\\POC research\\data\\1_DEM_watershed\\watershed_new_500m\\1.tif")

# 定义对齐和裁剪的函数
align_extent <- function(raster1, raster2) {
  if (!ext(raster1) == ext(raster2)) {
    message("Extents do not match. Re-clipping the raster.")
    # 裁剪和重采样raster2，使其与raster1对齐
    raster2 <- project(raster2, crs(raster1))
    raster2 <- resample(raster2, raster1, method = "near")
    raster2 <- crop(raster2, ext(raster1))
  }
  return(raster2)
}

# 对NPP栅格进行预处理，确保其投影、分辨率和范围与流域栅格一致
npp_list <- lapply(seq_along(npp_list), function(i) {
  npp_data <- npp_list[[i]]
  cat(sprintf("Processing raster %d/%d...\n", i, length(npp_list)))
  
  # 对齐NPP栅格到流域栅格
  aligned_npp <- align_extent(basin_tiff, npp_data)
  cat("Done with raster ", i, "\n\n")
  
  return(aligned_npp)
})


# 定义一个函数来计算每个站点的NPP相关变量
calculate_npp_variables <- function(station_id, basin_tiff_file, npp_list, sampling_year) {
  # 打印进度
  cat(sprintf("Processing station %s, year %d\n", station_id, sampling_year))
  
  npp_variables <- data.frame(Sampling_time = stations$Sampling_time[i], 
                              NPP = NA)
  
  # 读取站点对应的流域TIF文件
  basin_tiff <- rast(basin_tiff_file)
  
  # 检查采样年份是否在2001-2022年之间
  if (sampling_year %in% years) {
    npp_year_idx <- which(years == sampling_year)
    npp <- npp_list[[npp_year_idx]]
    
    npp <- align_extent(basin_tiff, npp)
    npp <- mask(npp, basin_tiff)
    npp_variables$NPP <- global(npp, "mean", na.rm=TRUE)$mean
  } else {
    cat(sprintf("No NPP data available for station %s in year %d\n", station_id, sampling_year))
  }
  
  return(npp_variables)
}

# 初始化一个结果数据框
npp_results <- data.frame()

# 对所有站点计算NPP变量
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if(station_id<864) next
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  sampling_time <- as.Date(stations$Sampling_time[i])
  sampling_year <- as.numeric(format(sampling_time, "%Y"))
  
  if (file.exists(basin_tiff_file)) {
    npp_variables <- calculate_npp_variables(station_id, basin_tiff_file, npp_list, sampling_year)
    npp_variables$ID <- station_id
    npp_results <- bind_rows(npp_results, npp_variables)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
}

npp_results <- npp_results %>%
  select(ID, Sampling_time, NPP)

# 查看结果
print(npp_results)

# 写入csv文件
write_csv(npp_results, output_path)