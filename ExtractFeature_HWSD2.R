library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "D:/POC research/data/3_POC records/monthly_records.xlsx"
clay_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__CLAY.tif"
sand_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__SAND.tif"   # 新增
silt_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__SILT.tif"   # 新增
organic_carbon_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__ORG_CARBON.tif"
root_depth_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__ROOT_DEPTH.tif"
ph_tiff_file <- "D:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__PH_WATER.tif"
basin_tiff_dir <- "D:\\POC research\\data\\1_DEM_watershed\\watershed_2\\"
output_path <- "D:/POC research/data/2_inputData_watershed/soil_summary.csv"

# 读取站点数据
stations <- read_excel(stations_path)

# 读取栅格数据
clay_rast <- rast(clay_tiff_file)
sand_rast <- rast(sand_tiff_file)   
silt_rast <- rast(silt_tiff_file)   
organic_carbon_rast <- rast(organic_carbon_tiff_file)
root_depth_rast <- rast(root_depth_tiff_file)
ph_rast <- rast(ph_tiff_file)

## 准备函数
  # 检查栅格是否有值的函数
  has_values <- function(rast) {
    !is.na(global(rast, "sum", na.rm = TRUE)$sum)
  }

  # 将根深度等级转换为根深度栅格（上限、下限、中值）的函数
  convert_root_depth_raster <- function(root_depth_rast) {
    
    # 定义分类规则，适用于所有分类
    rcl_shallower <- matrix(c(1, 0, 2, 50, 3, 100, 4, 150), ncol = 2, byrow = TRUE)
    rcl_deeper <- matrix(c(1, 50, 2, 100, 3, 150, 4, 200), ncol = 2, byrow = TRUE)
    
    # 应用分类
    shallower_depth_rast <- classify(root_depth_rast, rcl_shallower, others = NA)
    deeper_depth_rast <- classify(root_depth_rast, rcl_deeper, others = NA)
    
    # 计算中值
    midpoint_depth_rast <- (shallower_depth_rast + deeper_depth_rast) / 2
    
    return(list(shallower = shallower_depth_rast, deeper = deeper_depth_rast, midpoint = midpoint_depth_rast))
  }

  # 定义一个函数来检查、重投影、对齐栅格并将负值转换为NA
  prepare_raster <- function(target_rast, reference_rast) {
    if (!ext(target_rast) == ext(reference_rast)) {
      message("Extents do not match. Re-clipping the raster.")
      target_rast <- project(target_rast, crs(reference_rast))
      target_rast <- resample(target_rast, reference_rast, method = "near")
      target_rast <- crop(target_rast, ext(reference_rast))
    }
    
    # 将负值转换为NA
    target_rast[target_rast < 0] <- NA
    
    return(target_rast)
  }
  
## 预处理
basin_tiff_file <- "D:\\POC research\\data\\1_DEM_watershed\\watershed_2\\_1.tif"
basin_tiff <- rast(basin_tiff_file)
# 重投影并对齐栅格
clay <- prepare_raster(clay_rast, basin_tiff)
sand <- prepare_raster(sand_rast, basin_tiff)   
silt <- prepare_raster(silt_rast, basin_tiff)   
OC <- prepare_raster(organic_carbon_rast, basin_tiff)
root_depth <- prepare_raster(root_depth_rast, basin_tiff)
ph <- prepare_raster(ph_rast, basin_tiff)

# 定义一个函数来计算每个站点的土壤相关变量
calculate_soil_variables <- function(station_id, basin_tiff_file) {
  soil_variables <- data.frame(ID = station_id)
  
  # 打印进度
  cat(sprintf("Processing station %s\n", station_id))
  
  # 读取站点对应的流域TIF文件
  basin_tiff <- rast(basin_tiff_file)
  
  cat("Basin Tiff has values: ", has_values(basin_tiff), "\n")
  
  # 将 NoData 值 255 设置为 NA
  basin_tiff[basin_tiff == 255] <- NA
  
  # 对齐栅格
  clay <- prepare_raster(clay, basin_tiff)
  sand <- prepare_raster(sand, basin_tiff)   
  silt <- prepare_raster(silt, basin_tiff)   
  OC <- prepare_raster(OC, basin_tiff)
  root_depth <- prepare_raster(root_depth, basin_tiff)
  ph <- prepare_raster(ph, basin_tiff)
  
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
  
  # 提取各类土壤数据
  soil_variables$`Clay(%weight)` <- global(clay, "mean", na.rm = TRUE)$mean
  soil_variables$`Sand(%weight)` <- global(sand, "mean", na.rm = TRUE)$mean   
  soil_variables$`Silt(%weight)` <- global(silt, "mean", na.rm = TRUE)$mean   
  soil_variables$`Organic_Carbon(g/kg)` <- global(OC, "mean", na.rm = TRUE)$mean
  soil_variables$`pH_water` <- global(ph, "mean", na.rm = TRUE)$mean
  
  # 提取根深度的平均值
  soil_variables$`Shallower_Depth(cm)` <- global(root_depth_rasters$shallower, "mean", na.rm = TRUE)$mean
  soil_variables$`Deeper_Depth(cm)` <- global(root_depth_rasters$deeper, "mean", na.rm = TRUE)$mean
  soil_variables$`Midpoint_Depth(cm)` <- global(root_depth_rasters$midpoint, "mean", na.rm = TRUE)$mean
  
  return(soil_variables)
}

# 初始化一个结果数据框
soil_results <- data.frame()

# 对所有站点计算土壤变量
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))
  
  if (file.exists(basin_tiff_file)) {
    soil_variables <- calculate_soil_variables(station_id, basin_tiff_file)
    soil_results <- bind_rows(soil_results, soil_variables)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
  gc()  # 垃圾回收，释放内存
}

# 查看结果
print(soil_results)

# 写入csv文件
write_csv(soil_results, output_path)