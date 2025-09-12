library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "D:/POC research/data/3_POC records/monthly_records.xlsx"
data_dir <- "D:/POC research/data/2_pco2Data/land_covernew"
basin_tiff_dir <- "D:/POC research/data/1_DEM_watershed/watershed_2"

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
basin_tiff <- rast("D:\\POC research\\data\\1_DEM_watershed\\watershed_1\\_1.tif")

# 对指定的单个采样点及采样时间提取相应landcover变量
calculate_single_station_landcover <- function(station_id, sampling_year, basin_tiff_file, landcover_list) {
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
  
  # 对齐栅格数据
  landcover_data <- project(landcover_data, crs(basin_tiff))
  cropped_data <- crop(landcover_data, basin_tiff)
  resampled_data <- resample(cropped_data, basin_tiff, method = "near")
  landcover_data <- mask(resampled_data, basin_tiff)
  
  # 确保landcover_data有值后再计算各类比例
  if (!is.null(global(landcover_data, "sum", na.rm = TRUE)$sum)) {
    landcover_variables$`Cropland%` <- global(landcover_data == 1, "mean", na.rm = TRUE)$mean * 100
    landcover_variables$`Forest%` <- global(landcover_data == 2, "mean", na.rm = TRUE)$mean * 100
    landcover_variables$`Shrub%` <- global(landcover_data == 3, "mean", na.rm = TRUE)$mean * 100
    landcover_variables$`Grassland%` <- global(landcover_data == 4, "mean", na.rm = TRUE)$mean * 100
    landcover_variables$`Water%` <- global(landcover_data == 5, "mean", na.rm = TRUE)$mean * 100
    landcover_variables$`Snow_Ice%` <- global(landcover_data == 6, "mean", na.rm = TRUE)$mean * 100
    landcover_variables$`Barren%` <- global(landcover_data == 7, "mean", na.rm = TRUE)$mean * 100
    landcover_variables$`Urban%` <- global(landcover_data == 8, "mean", na.rm = TRUE)$mean * 100
    landcover_variables$`Wetland%` <- global(landcover_data == 9, "mean", na.rm = TRUE)$mean * 100
  } else {
    warning(paste("No valid landcover data for station", station_id, "in year", sampling_year))
  }
  
  cat(sprintf("Finished processing station %s, year %d\n", station_id, sampling_year))
  
  return(landcover_variables)
}


# 提取特定采样点和采样时间的landcover变量
station_id <- "74"  # 替换为你的站点ID
sampling_time <- as.Date("2005-02-01")  # 替换为你的采样时间
sampling_year <- as.numeric(format(sampling_time, "%Y"))
basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))

if (file.exists(basin_tiff_file)) {
  landcover_variables <- calculate_single_station_landcover(station_id, sampling_year, basin_tiff_file, landcover_list)
  print(landcover_variables)
} else {
  warning(paste("TIF file for station ID", station_id, "not found."))
}
