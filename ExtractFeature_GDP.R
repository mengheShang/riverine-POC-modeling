library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
gdp_data_dir <- "E:\\POC research\\data\\2_GDP"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/GDP.csv"

# 读取站点数据
stations <- read_excel(stations_path)

# 获取所有GDP tiff文件的列表
tiff_files <- list.files(path = gdp_data_dir, pattern = "\\.tif$", full.names = TRUE)

# 确定年份范围，从1990到2015
years <- 1990:2015

# 读取所有GDP TIF文件并存储在列表中
gdp_list <- lapply(tiff_files, rast)

print(tiff_files)
print(length(years))

# 确保tiff_files的长度与years一致
if (length(tiff_files) != length(years)) {
  stop("The length of tiff_files is not equal to the length of years.")
}

# 读取任意一个流域TIF文件作为参考
basin_tiff <- rast("E:\\POC research\\data\\1_DEM_watershed\\watershed_new_500m\\1.tif")

# 定义一个函数来检查和对齐范围
align_extent <- function(raster1, raster2) {
  if (!ext(raster1) == ext(raster2)) {
    message("Extents do not match. Re-clipping the raster.")
    raster2 <- project(raster2, crs(raster1))
    raster2 <- resample(raster2, raster1, method = "near")
    raster2 <- crop(raster2, ext(raster1))
  }
  return(raster2)
}

gdp_list <- lapply(seq_along(gdp_list), function(i) {
  gdp_data <- gdp_list[[i]]
  cat(sprintf("Processing raster %d/%d...\n", i, length(gdp_list)))
  gdp_data <- project(gdp_data, crs(basin_tiff))
  cropped_data <- crop(gdp_data, basin_tiff)
  resampled_data <- resample(cropped_data, basin_tiff, method = "near")
  cat("Done with raster ", i, "\n\n")
  return(resampled_data)
})

# 定义一个函数来计算每个站点的流域GDP
calculate_gdp <- function(station_id, basin_tiff_file, gdp_list, sampling_year) {
  # 打印进度
  cat(sprintf("Processing station %s, year %d\n", station_id, sampling_year))
  
  # 找到与采样年份相对应的TIF文件
  if (sampling_year > 2015) {
    sampling_year = 2015
  }
  year_index <- which(years == sampling_year)
  if (length(year_index) == 0) {
    stop(paste("No GDP data available for the year", sampling_year))
  }
  
  gdp_data <- gdp_list[[year_index]]
  
  # 读取站点对应的流域TIF文件
  basin_tiff <- rast(basin_tiff_file)
  
  gdp_data_1 <- align_extent(basin_tiff, gdp_data)
  gdp_data <- mask(gdp_data_1, basin_tiff)
  
  plot(gdp_data)
  
  # 计算流域GDP
  total_gdp <- global(gdp_data, "sum", na.rm = TRUE)
  
  cat(sprintf("Finished station %s, year %d\n", station_id, sampling_year))
  
  return(total_gdp)
}

# 初始化一个结果数据框
gdp_results <- data.frame()

# 对所有站点计算GDP
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if (station_id < 864) next
  sampling_time <- as.Date(stations$Sampling_time[i])
  sampling_year <- as.numeric(format(sampling_time, "%Y"))
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  
  if (file.exists(basin_tiff_file)) {
    total_gdp <- calculate_gdp(station_id, basin_tiff_file, gdp_list, sampling_year)
    station_results <- data.frame(ID = station_id, Sampling_time = stations$Sampling_time[i], GDP = total_gdp[1,1])
    gdp_results <- bind_rows(gdp_results, station_results)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
}

# 查看结果
print(gdp_results)

# 写入csv文件
write_csv(gdp_results, output_path)