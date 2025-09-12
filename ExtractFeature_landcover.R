library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
data_dir <- "E:/POC research/data/2_pco2Data/land_covernew"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/landcover.csv"

# 读取站点数据
stations <- read_excel(stations_path)

# 获取所有landcover tiff文件的列表
tiff_files <- list.files(path = data_dir, pattern = "*_1.tif$", full.names = TRUE)

# 找到1985年的TIF文件
file_1985 <- grep("1985_1.tif$", tiff_files, value = TRUE)

# 将1985年的TIF文件复制4次，以便生成1985-1989五年的数据
tiff_files <- c(rep(file_1985, 5), tiff_files[-grep("1985_1.tif$", tiff_files)])

# 确定年份范围，从1985到相应的年份
years <- 1985:(1985 + length(tiff_files) - 1)

# 读取所有landcover TIF文件并存储在列表中
landcover_list <- lapply(tiff_files, rast)

# 确保tiff_files的长度与years一致
if (length(tiff_files) != length(years)) {
  stop("The length of tiff_files is not equal to the length of years.")
}

# 读取任意一个流域TIF文件作为参考
basin_tiff <- rast("E:\\POC research\\data\\1_DEM_watershed\\watershed_new_500m\\1.tif")

landcover_list <- lapply(seq_along(landcover_list), function(i) {
  landcover_data <- landcover_list[[i]]
  
  cat(sprintf("Processing raster %d/%d...\n", i, length(landcover_list)))
  # 统一坐标系并重投影
  cat("Projecting to match CRS and resolution...\n")
  landcover_data <- project(landcover_data, crs(basin_tiff))
  # 裁剪
  cat("Cropping raster...\n")
  cropped_data <- crop(landcover_data, basin_tiff)
  # 重采样
  cat("Resampling raster...\n")
  resampled_data <- resample(cropped_data, basin_tiff, method = "near")
  
  cat("Done with raster ", i, "\n\n")
  return(resampled_data)
})

# Function to check and align extents
align_extent <- function(raster1, raster2) {
  if (!ext(raster1) == ext(raster2)) {
    message("Extents do not match. Re-clipping the raster.")
    raster2 <- project(raster2, crs(raster1))
    raster2 <- resample(raster2, raster1, method = "near")
    raster2 <- crop(raster2, ext(raster1))
  }
  return(raster2)
}

# 定义一个函数来计算每个站点的土地覆盖类型变量
calculate_landcover_variables <- function(station_id, basin_tiff_file, landcover_list, sampling_year) {
  # 打印进度
  cat(sprintf("Processing station %s, year %d\n", station_id, sampling_year))
  
  # 初始化数据框存储landcover变量
  landcover_variables <- data.frame(Cropland = NA, Forest = NA, Shrub = NA, Grassland = NA, Water = NA, 
                                    Snow_Ice = NA, Barren = NA, Urban = NA, Wetland = NA)
  
  # 找到与采样年份相对应的TIF文件
  year_index <- which(years == sampling_year)
  if (length(year_index) == 0) {
    stop(paste("No landcover data available for the year", sampling_year))
  }
  
  landcover_data <- landcover_list[[year_index]]
  
  # 读取站点对应的流域TIF文件
  basin_tiff <- rast(basin_tiff_file)
  
  landcover_data_1 <- align_extent(basin_tiff, landcover_data)
  landcover_data <- mask(landcover_data_1, basin_tiff)
  plot(landcover_data_1)
  plot(landcover_data)
  
  # 计算各类landcover的比例
  landcover_variables$`Cropland` <- global(landcover_data == 1, "mean", na.rm = TRUE)$mean
  landcover_variables$`Forest` <- global(landcover_data == 2, "mean", na.rm = TRUE)$mean
  landcover_variables$`Shrub` <- global(landcover_data == 3, "mean", na.rm = TRUE)$mean
  landcover_variables$`Grassland` <- global(landcover_data == 4, "mean", na.rm = TRUE)$mean
  landcover_variables$`Water` <- global(landcover_data == 5, "mean", na.rm = TRUE)$mean
  landcover_variables$`Snow_Ice` <- global(landcover_data == 6, "mean", na.rm = TRUE)$mean
  landcover_variables$`Barren` <- global(landcover_data == 7, "mean", na.rm = TRUE)$mean
  landcover_variables$`Urban` <- global(landcover_data == 8, "mean", na.rm = TRUE)$mean
  landcover_variables$`Wetland` <- global(landcover_data == 9, "mean", na.rm = TRUE)$mean
  
  cat(sprintf("Finished station %s, year %d\n", station_id, sampling_year))
  
  return(landcover_variables)
}

# 初始化一个结果数据框
landcover_results <- data.frame()

# 对所有站点计算landcover变量
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if(station_id < 864) next
  sampling_time <- as.Date(stations$Sampling_time[i])
  sampling_year <- as.numeric(format(sampling_time, "%Y"))
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  
  if (file.exists(basin_tiff_file)) {
    landcover_variables <- calculate_landcover_variables(station_id, basin_tiff_file, landcover_list, sampling_year)
    station_results <- data.frame(ID = station_id, year = sampling_year, landcover_variables)
    landcover_results <- bind_rows(landcover_results, station_results)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
}

# 查看结果
print(landcover_results)

# 写入csv文件
write_csv(landcover_results, output_path)
