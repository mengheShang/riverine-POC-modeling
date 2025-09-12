library(terra)
library(dplyr)
library(readr)

# 输入文件路径
station_id <- 74  # 这里可以方便地修改为需要处理的站点ID
clay_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__CLAY.tif"
sand_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__SAND.tif"
silt_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__SILT.tif"
organic_carbon_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__ORG_CARBON.tif"
root_depth_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__ROOT_DEPTH.tif"
ph_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__PH_WATER.tif"
basin_tiff_file <- paste0("D:\\POC research\\data\\1_DEM_watershed\\watershed_2\\_", station_id, ".tif")
output_file <- paste0("D:/POC research/data/2_inputData_watershed/soil_summary_station_", station_id, ".csv")

# 读取栅格数据
clay_rast <- rast(clay_tiff_file)
sand_rast <- rast(sand_tiff_file)
silt_rast <- rast(silt_tiff_file)
organic_carbon_rast <- rast(organic_carbon_tiff_file)
root_depth_rast <- rast(root_depth_tiff_file)
ph_rast <- rast(ph_tiff_file)

# 定义函数
has_values <- function(rast) {
  !is.na(global(rast, "sum", na.rm = TRUE)$sum)
}

convert_root_depth_raster <- function(root_depth_rast) {
  rcl_shallower <- matrix(c(1, 0, 2, 50, 3, 100, 4, 150), ncol = 2, byrow = TRUE)
  rcl_deeper <- matrix(c(1, 50, 2, 100, 3, 150, 4, 200), ncol = 2, byrow = TRUE)
  
  shallower_depth_rast <- classify(root_depth_rast, rcl_shallower, others = NA)
  deeper_depth_rast <- classify(root_depth_rast, rcl_deeper, others = NA)
  midpoint_depth_rast <- (shallower_depth_rast + deeper_depth_rast) / 2
  
  return(list(shallower = shallower_depth_rast, deeper = deeper_depth_rast, midpoint = midpoint_depth_rast))
}

prepare_raster <- function(target_rast, reference_rast) {
  if (!ext(target_rast) == ext(reference_rast)) {
    message("Extents do not match. Re-clipping the raster.")
    target_rast <- project(target_rast, crs(reference_rast))
    target_rast <- resample(target_rast, reference_rast, method = "near")
    target_rast <- crop(target_rast, ext(reference_rast))
  }
  return(target_rast)
}

# 读取站点的流域TIF文件
basin_tiff <- rast(basin_tiff_file)
basin_tiff[basin_tiff == 255] <- NA  # 将 NoData 值 255 设置为 NA

# 重投影并对齐栅格
clay <- prepare_raster(clay_rast, basin_tiff)
sand <- prepare_raster(sand_rast, basin_tiff)
silt <- prepare_raster(silt_rast, basin_tiff)
OC <- prepare_raster(organic_carbon_rast, basin_tiff)
root_depth <- prepare_raster(root_depth_rast, basin_tiff)
ph <- prepare_raster(ph_rast, basin_tiff)

# 转换根深度等级栅格
root_depth_rasters <- convert_root_depth_raster(root_depth)

# 应用掩膜
clay <- mask(clay, basin_tiff)
sand <- mask(sand, basin_tiff)
silt <- mask(silt, basin_tiff)
OC <- mask(OC, basin_tiff)
root_depth_rasters$shallower <- mask(root_depth_rasters$shallower, basin_tiff)
root_depth_rasters$deeper <- mask(root_depth_rasters$deeper, basin_tiff)
root_depth_rasters$midpoint <- mask(root_depth_rasters$midpoint, basin_tiff)
ph <- mask(ph, basin_tiff)

# 计算并输出土壤相关变量
soil_variables <- data.frame(
  ID = station_id,
  `Clay(%weight)` = global(clay, "mean", na.rm = TRUE)$mean,
  `Sand(%weight)` = global(sand, "mean", na.rm = TRUE)$mean,
  `Silt(%weight)` = global(silt, "mean", na.rm = TRUE)$mean,
  `Organic_Carbon(%weight)` = global(OC, "mean", na.rm = TRUE)$mean,
  `pH_water` = global(ph, "mean", na.rm = TRUE)$mean,
  `Shallower_Depth(cm)` = global(root_depth_rasters$shallower, "mean", na.rm = TRUE)$mean,
  `Deeper_Depth(cm)` = global(root_depth_rasters$deeper, "mean", na.rm = TRUE)$mean,
  `Midpoint_Depth(cm)` = global(root_depth_rasters$midpoint, "mean", na.rm = TRUE)$mean
)

# 输出结果
print(soil_variables)

# 保存结果到CSV文件
write_csv(soil_variables, output_file)
