# 加载必要的库
library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
cfactor_tiff_file <- "E:\\POC research\\data\\2_GloSEM_25km\\Data_25km/RUSLE_CFactor_yr2012_v1.1_25km.tif"
kfactor_tiff_file <- "E:\\POC research\\data\\2_GloSEM_25km\\Data_25km/RUSLE_KFactor_v1.1_25km.tif"
lsfactor_tiff_file <- "E:\\POC research\\data\\2_GloSEM_25km\\Data_25km/RUSLE_LSFactor_v1.1_25km.tif"
rfactor_tiff_file <- "E:\\POC research\\data\\2_GloSEM_25km\\Data_25km/RUSLE_RFactor_v1.1_25km.tif"
soilloss_2001_tiff_file <- "E:\\POC research\\data\\2_GloSEM_25km\\Data_25km/RUSLE_SoilLoss_v1.1_yr2001_25km.tif"
soilloss_2012_tiff_file <- "E:\\POC research\\data\\2_GloSEM_25km\\Data_25km/RUSLE_SoilLoss_v1.1_yr2012_25km.tif"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m/"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/soilloss.csv"

# 读取站点数据
stations <- read_excel(stations_path)

# 读取RUSLE相关因子的栅格数据
cfactor_rast <- rast(cfactor_tiff_file)
kfactor_rast <- rast(kfactor_tiff_file)
lsfactor_rast <- rast(lsfactor_tiff_file)
rfactor_rast <- rast(rfactor_tiff_file)
soilloss_2001_rast <- rast(soilloss_2001_tiff_file)
soilloss_2012_rast <- rast(soilloss_2012_tiff_file)

# 准备函数
# 检查栅格是否有值的函数
has_values <- function(rast) {
  !is.na(global(rast, "sum", na.rm = TRUE)$sum)
}

# 定义一个函数来检查、重投影、对齐栅格并将负值转换为NA
prepare_raster <- function(target_rast, reference_rast) {
  if (!ext(target_rast) == ext(reference_rast)) {
    message("Extents do not match. Re-clipping the raster.")
    target_rast <- project(target_rast, crs(reference_rast))
    target_rast <- resample(target_rast, reference_rast, method = "near")
    target_rast <- crop(target_rast, ext(reference_rast))
  }
  return(target_rast)
}

basin_tiff <- rast(file.path(basin_tiff_dir, "1.tif"))
cfactor <- prepare_raster(cfactor_rast, basin_tiff)
kfactor <- prepare_raster(kfactor_rast, basin_tiff)
lsfactor <- prepare_raster(lsfactor_rast, basin_tiff)
rfactor <- prepare_raster(rfactor_rast, basin_tiff)
soilloss_2001 <- prepare_raster(soilloss_2001_rast, basin_tiff)
soilloss_2012 <- prepare_raster(soilloss_2012_rast, basin_tiff)

# 定义一个函数来计算每个站点的土壤相关变量
calculate_soil_variables <- function(station_id, sampling_year, basin_tiff_file) {
  soil_variables <- data.frame(ID = station_id)
  
  # 打印进度
  cat(sprintf("Processing station %s, year %d\n", station_id, sampling_year))
  
  # 读取站点对应的流域TIF文件
  basin_tiff <- rast(basin_tiff_file)
  
  if (!has_values(basin_tiff)) {
    warning(sprintf("No valid values found for basin tiff at station %s", station_id))
    return(soil_variables)
  }
  
  # 对齐并掩膜土壤侵蚀相关栅格
  cfactor <- mask(prepare_raster(cfactor, basin_tiff), basin_tiff)
  kfactor <- mask(prepare_raster(kfactor, basin_tiff), basin_tiff)
  lsfactor <- mask(prepare_raster(lsfactor, basin_tiff), basin_tiff)
  rfactor <- mask(prepare_raster(rfactor, basin_tiff), basin_tiff)
  
  # 根据采样年份选择土壤流失数据
  if (sampling_year < 2012) {
    soilloss <- mask(prepare_raster(soilloss_2001, basin_tiff), basin_tiff)
    soil_variables$`SoilLoss` <- global(soilloss, "mean", na.rm = TRUE)$mean
  } else {
    soilloss <- mask(prepare_raster(soilloss_2012, basin_tiff), basin_tiff)
    soil_variables$`SoilLoss` <- global(soilloss, "mean", na.rm = TRUE)$mean
  }
  
  # 提取其他土壤侵蚀相关变量的平均值并附加单位
  soil_variables$`C_factor` <- global(cfactor, "mean", na.rm = TRUE)$mean
  soil_variables$`K_factor` <- global(kfactor, "mean", na.rm = TRUE)$mean
  soil_variables$`LS_factor` <- global(lsfactor, "mean", na.rm = TRUE)$mean
  soil_variables$`R_factor` <- global(rfactor, "mean", na.rm = TRUE)$mean
  
  return(soil_variables)
}

# 初始化一个结果数据框
soil_results <- data.frame()

# 对所有站点计算土壤侵蚀变量
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if(station_id < 864) next
  samplingtime <- as.Date(stations$Sampling_time)
  Sampling_years <- as.numeric(format(samplingtime, "%Y"))
  sampling_year <- Sampling_years[i]  # 从站点数据表中提取采样年份
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  
  if (file.exists(basin_tiff_file)) {
    soil_variables <- calculate_soil_variables(station_id, sampling_year, basin_tiff_file)
    soil_results <- bind_rows(soil_results, soil_variables)
  } else {
    warning(sprintf("TIF file for station ID %s not found.", station_id))
  }
  gc()  # 垃圾回收，释放内存
}

# 查看结果
print(soil_results)
# 写入CSV文件
write_csv(soil_results, output_path)
