library(sf)
library(raster)
library(dplyr)
library(tidyr)
library(sp)
library(foreach)
library(doParallel)

basin_path <- "D:\\POC research\\data\\basin\\8basin.shp"
tiff_dir <- "D:\\POC research\\data\\pco2Data\\land_covernew"
output_path <- "D:\\POC research\\data\\pco2Data_basin\\land_cover.csv"

# 加载流域shapefile
basin <- st_read(basin_path)

# 获取landcover tif文件列表
landcover_files <- list.files(tiff_dir, pattern = "*.tif$", full.names = TRUE)

# 转换shapefile的坐标系为EPSG:4326
basin <- st_transform(basin, crs = 4326)
basin_sp <- as(basin, "Spatial")

# 提前计算流域的bounding boxes以减少掩膜操作的范围
basin_bbox <- lapply(1:length(basin_sp), function(i) {
  extent(basin_sp[i, ])
})

# 定义优化后的函数
extract_landcover_by_basin <- function(landcover_file, basin_sp, basin_bbox) {
  # 加载landcover tif文件
  landcover <- raster(landcover_file)
  
  # 创建一个空的数据框用于存储结果
  results <- data.frame()
  
  # 循环遍历每个流域
  for (i in 1:length(basin_sp)) {
    # 提取当前流域的几何和bounding box
    basin_geom <- basin_sp[i, ]
    bbox <- basin_bbox[[i]]
    
    # 裁剪landcover栅格以减少掩膜操作的范围
    cropped_landcover <- crop(landcover, bbox)
    
    # 使用流域几何来掩膜landcover栅格
    masked_landcover <- mask(cropped_landcover, basin_geom)
    
    # 计算每种landcover类型的像素数量
    landcover_values <- freq(masked_landcover, useNA = "no")
    
    # 创建一个数据框包含当前流域的结果
    basin_id <- i
    year <- as.numeric(sub(".*/(\\d{4}).*", "\\1", landcover_file))
    temp_df <- data.frame(Year = year, Basin_ID = basin_id, landcover_values)
    
    # 合并结果
    results <- bind_rows(results, temp_df)
  }
  
  return(results)
}

# 遍历每个年份的landcover文件并提取数据
all_results <- lapply(landcover_files, function(file) {
  extract_landcover_by_basin(file, basin_sp, basin_bbox)
})

# 将所有结果合并成一个数据框
all_results <- bind_rows(all_results)

# 将结果存储为CSV文件
write.csv(all_results, output_path, row.names = FALSE)
