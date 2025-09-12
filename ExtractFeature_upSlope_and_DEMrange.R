library(terra)
library(dplyr)
library(readxl)
library(writexl)

# 输入文件路径
dem_file <- "E:\\POC research\\data\\1_DEM_watershed\\SRTM/SRTM_dem_90m.tif"
stations_path <- "E:\\POC research\\data\\3_POC records\\clean_records.xlsx"
basin_tiff_dir <- "E:\\POC research\\data\\1_DEM_watershed\\watershed_new_500m\\"
output_path <- "E:\\POC research\\data\\2_inputData_watershed\\Features/upSlope_elevRange.csv"

# 读取DEM文件
dem <- rast(dem_file)
res_ratio <- 500 / 90  
dem <- aggregate(dem, fact = res_ratio, fun = "mean")

# 计算坡度（以度为单位）
slope <- terrain(dem, v = "slope", unit = "degrees")

# 读取采样点数据
stations <- read_excel(stations_path) %>%
  distinct(ID, Longitude, Latitude)

# 函数：提取流域内平均坡度和高程范围
calculate_upstream_features <- function(station_id, basin_tiff_file) {
  # 打印进度
  cat(sprintf("Processing station %s\n", station_id))
  
  # 读取站点对应的流域TIF文件
  basin_tiff <- rast(basin_tiff_file)
  
  # 确保流域栅格和DEM/坡度栅格的CRS和分辨率匹配
  basin_tiff <- resample(basin_tiff, dem, method = "near")
  
  # 将 NoData 值 255 设置为 NA
  basin_tiff[basin_tiff == 255] <- NA
  
  # 将DEM和坡度栅格掩膜为流域区域
  dem_masked <- mask(dem, basin_tiff)
  slope_masked <- mask(slope, basin_tiff)
  
  plot(dem_masked)
  plot(slope_masked)
  
  # 计算流域内平均坡度
  mean_slope <- global(slope_masked, "mean", na.rm = TRUE)$mean
  
  # 计算流域内高程范围（最大高差）
  max_elev <- global(dem_masked, "max", na.rm = TRUE)$max
  min_elev <- global(dem_masked, "min", na.rm = TRUE)$min
  elev_range <- max_elev - min_elev
  
  return(data.frame(ID = station_id, 
                    upSlope = mean_slope,
                    DEM_range = elev_range))
}

# 初始化一个结果数据框
results <- data.frame(ID = integer(), 
                      upSlope = numeric(),
                      DEM_range = numeric())

# 对所有站点计算流域特征
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if(station_id<864)next
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  
  if (file.exists(basin_tiff_file)) {
    station_results <- calculate_upstream_features(station_id, basin_tiff_file)
    results <- bind_rows(results, station_results)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
  gc()  # 垃圾回收，释放内存
}

# 查看结果
print(results)

# 保存结果到Excel文件
write_csv(results, output_path)
