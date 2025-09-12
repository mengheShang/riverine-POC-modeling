library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
npp_data_dir <- "E:/POC research/data/2_pco2Data/npp"
landcover_data_dir <- "E:/POC research/data/2_pco2Data/land_covernew"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_2"
output_path <- "E:/POC research/data/2_inputData_watershed/npp_land_var.csv"

# 读取站点数据
stations <- read_excel(stations_path)

# 获取所有NPP和landcover TIF文件的列表
npp_tiff_files <- list.files(path = npp_data_dir, pattern = "*_new.tif$", full.names = TRUE)
landcover_tiff_files <- list.files(path = landcover_data_dir, pattern = "*_1.tif$", full.names = TRUE)

years <- seq(2001, 2001 + length(npp_tiff_files) - 1)  # NPP数据年份范围是2001-2022

# 读取所有NPP和landcover TIF文件并存储在列表中
npp_list <- lapply(npp_tiff_files, rast)
landcover_list <- lapply(landcover_tiff_files, rast)

# 读取任意一个流域TIF文件作为参考
basin_tiff <- rast("E:/POC research/data/1_DEM_watershed/watershed_2/_1.tif")

# 设定等面积投影（示例：Lambert Azimuthal Equal Area; 请根据实际区域更换）
my_equal_area_crs <- "+proj=aea +lat_0=30 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# 将参考的 basin_tiff 也投影到等面积坐标
basin_tiff <- project(basin_tiff, my_equal_area_crs, method = "near")

# 读取并投影 NPP、Landcover 栅格至等面积坐标系
npp_list <- lapply(npp_tiff_files, function(f) {
  r_raw <- rast(f)
  # 连续型数据(NPP) 用双线性插值
  project(r_raw, my_equal_area_crs, method = "bilinear")
})

landcover_list <- lapply(landcover_tiff_files, function(f) {
  r_raw <- rast(f)
  # 分类数据(Landcover) 用邻近插值
  project(r_raw, my_equal_area_crs, method = "near")
})

# 对齐和裁剪函数 
align_extent <- function(raster1, raster2) {
  if (!terra::same.crs(raster1, raster2)) {
    raster2 <- terra::project(raster2, terra::crs(raster1))
  }
  if (!terra::compareGeom(raster1, raster2, 
                          crs = FALSE,  
                          ext = FALSE,   
                          res = TRUE,     
                          stopOnError = FALSE)) {
    raster2 <- terra::resample(raster2, raster1, method = "near")
  }
  if (!terra::ext(raster1) == terra::ext(raster2)) {
    raster2 <- terra::crop(raster2, e1)
  }
  return(raster2)
}

# 处理landcover和NPP栅格，确保其投影、分辨率和范围与流域栅格一致
npp_list <- lapply(npp_list, function(npp_data) align_extent(basin_tiff, npp_data))
landcover_list <- lapply(landcover_list, function(landcover_data) align_extent(basin_tiff, landcover_data))

# 定义提取变量的函数
calculate_npp_landcover_variables <- function(station_id, basin_tiff_file, 
                                              npp_list, landcover_list, 
                                              sampling_year) {
  # 打印进度
  cat(sprintf("Processing station %s, year %d\n", station_id, sampling_year))
  
  # 使用 local() 隔离临时变量
  npp_variables <- local({
    # 初始化结果
    result <- data.frame(
      ID = station_id,
      Sampling_year = sampling_year,
      NPP_1back = NA, NPP_2back = NA, NPP_3back = NA,
      sum_NPP3 = NA,
      Wood_NPP = NA, Crop_NPP = NA, Grass_NPP = NA,
      Water_NPP = NA, Barren_NPP = NA, Wetland_NPP = NA
    )
    
    # 读取流域栅格（显式声明为临时变量）
    basin_tiff <- rast(basin_tiff_file)
    on.exit(rm(basin_tiff), add = TRUE)  # 确保退出时清理
    
    if (sampling_year %in% years) {
      npp_year_idx <- which(years == sampling_year)
      
      # 提取前3年的NPP数据（动态处理）
      npp_1 <- if (sampling_year - 1 %in% years) {
        npp <- npp_list[[which(years == sampling_year - 1)]]
        npp <- align_extent(basin_tiff, npp)
        r <- mask(npp, basin_tiff)
        result$NPP_1back <- global(r, "mean", na.rm = TRUE)$mean
        r
      } else NULL
      on.exit(if(exists("npp_1")) rm(npp_1), add = TRUE)
      
      npp_2 <- if (sampling_year - 2 %in% years) {
        npp <- npp_list[[which(years == sampling_year - 2)]]
        npp <- align_extent(basin_tiff, npp)
        r <- mask(npp, basin_tiff)
        result$NPP_2back <- global(r, "mean", na.rm = TRUE)$mean
        r
      } else NULL
      on.exit(if(exists("npp_2")) rm(npp_2), add = TRUE)
      
      npp_3 <- if (sampling_year - 3 %in% years) {
        npp <- npp_list[[which(years == sampling_year - 3)]]
        npp <- align_extent(basin_tiff, npp)
        r <- mask(npp, basin_tiff)
        result$NPP_3back <- global(r, "mean", na.rm = TRUE)$mean
        r
      } else NULL
      on.exit(if(exists("npp_3")) rm(npp_3), add = TRUE)
      
      # 检查是否所有必要年份数据都存在
      if (is.null(npp_1) || is.null(npp_2) || is.null(npp_3)) {
        cat(sprintf("Missing NPP data for station %s in year %d or previous years. Skipping.\n",
                    station_id, sampling_year))
        return(result)
      }
      
      # 计算3年总和
      sum_npp_3yrs <- npp_1 + npp_2 + npp_3
      result$sum_NPP3 <- global(sum_npp_3yrs, "mean", na.rm = TRUE)$mean
      on.exit(rm(sum_npp_3yrs), add = TRUE)
      
      # 处理Landcover数据
      landcover_data <- align_extent(basin_tiff, landcover_list[[npp_year_idx]])
      landcover_data <- mask(landcover_data, basin_tiff)
      landcover_data[landcover_data %in% c(6, 8)] <- NA  # 去除无效类别
      on.exit(rm(landcover_data), add = TRUE)
      
      # 定义计算函数（局部函数，无需清理）
      calc_lc_npp_total <- function(r_npp, r_lc, classes) {
        masked_npp <- mask(r_npp, r_lc %in% classes, maskvalues = FALSE)
        npp_in_kgC <- masked_npp * cellSize(masked_npp)
        global(npp_in_kgC, "sum", na.rm = TRUE)$sum
      }
      
      # 计算各土地类型NPP总量
      result$Wood_NPP <- calc_lc_npp_total(sum_npp_3yrs, landcover_data, c(2, 3))
      result$Crop_NPP <- calc_lc_npp_total(sum_npp_3yrs, landcover_data, 1)
      result$Grass_NPP <- calc_lc_npp_total(sum_npp_3yrs, landcover_data, 4)
      result$Water_NPP <- calc_lc_npp_total(sum_npp_3yrs, landcover_data, 5)
      result$Barren_NPP <- calc_lc_npp_total(sum_npp_3yrs, landcover_data, 7)
      result$Wetland_NPP <- calc_lc_npp_total(sum_npp_3yrs, landcover_data, 9)
    } else {
      cat(sprintf("No NPP data available for station %s in year %d\n", station_id, sampling_year))
    }
    
    # 返回结果（local()的返回值）
    result
  })
  
  # 强制清理terra临时文件
  terra::tmpFiles(remove = TRUE)
  gc(full = TRUE)
  
  return(npp_variables)
}

# 初始化一个结果数据框
npp_landcover_results <- data.frame()

# 对所有站点计算NPP和landcover变量
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))
  sampling_time <- as.Date(stations$Sampling_time[i])
  sampling_year <- as.numeric(format(sampling_time, "%Y"))
  
  if (file.exists(basin_tiff_file)) {
    npp_landcover_variables <- calculate_npp_landcover_variables(station_id, basin_tiff_file, npp_list, landcover_list, sampling_year)
    npp_landcover_results <- bind_rows(npp_landcover_results, npp_landcover_variables)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
  gc()
}

# 查看结果
print(npp_landcover_results)

# 写入csv文件
write_csv(npp_landcover_results, output_path)


#####

# 函数：提取特定站点和采样时间的NPP变量
extract_npp_variables <- function(station_id, sampling_time, npp_list, landcover_list, basin_tiff_dir, years) {
  # 将采样时间转换为年份
  sampling_year <- as.numeric(format(as.Date(sampling_time), "%Y"))
  
  # 获取站点对应的流域TIF文件路径
  basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))
  
  if (!file.exists(basin_tiff_file)) {
    stop(paste("TIF file for station ID", station_id, "not found."))
  }
  
  # 调用计算函数来提取NPP和landcover变量
  npp_variables <- calculate_npp_landcover_variables(station_id, basin_tiff_file, npp_list, landcover_list, sampling_year)
  
  # 返回结果
  return(npp_variables)
}

# 使用示例
station_id <- "65"  # 示例站点ID
sampling_time <- "2003-07-15"  # 示例采样时间

# 调用函数提取NPP变量
npp_result <- extract_npp_variables(station_id, sampling_time, npp_list, landcover_list, basin_tiff_dir, years)

# 查看提取的结果
print(npp_result)
