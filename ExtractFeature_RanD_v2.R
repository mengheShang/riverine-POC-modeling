library(terra)
library(sf)
library(dplyr)
library(readxl)
library(readr)

# 输入文件路径
stations_path <- "E:\\POC research\\data\\3_POC records\\clean_records.xlsx"
reservoirs_dams_file <- "E:/POC research/data/2_Reservoirs and Dams/China_Dams.shp"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/RanD.csv"

# 读取站点数据和水库大坝数据
stations <- read_excel(stations_path)
reservoirs_dams <- st_read(reservoirs_dams_file)

# 转化函数
replace_neg99_with_na_sf <- function(sf_obj) {
  attributes_df <- st_drop_geometry(sf_obj)
  attributes_df[] <- lapply(attributes_df, function(x) {
    if (is.numeric(x)) {
      x[x == -99] <- NA
    }
    return(x)
  })
  sf_obj <- st_as_sf(cbind(attributes_df, st_geometry(sf_obj)))
  return(sf_obj)
}

# 对 sf 对象进行替换操作
reservoirs_dams <- replace_neg99_with_na_sf(reservoirs_dams)

# 计算汇总统计特征的函数
calculate_summary_statistics <- function(reservoirs_dams) {
  summary_stats <- reservoirs_dams %>%
    st_drop_geometry() %>%
    summarize(
      Num_Dams = n(),
      Avg_DamHeight_m = mean(DAM_HGT_M, na.rm = TRUE),  # 平均大坝高度
      Max_DamHeight_m = max(DAM_HGT_M, na.rm = TRUE),   # 最大大坝高度
      Avg_CatchmentArea_km2 = mean(CATCH_SKM, na.rm = TRUE),  # 平均集水面积
      Sum_CatchmentArea_km2 = sum(CATCH_SKM, na.rm = TRUE),   # 总集水面积
      Avg_Depth_m = mean(DEPTH_M, na.rm = TRUE),              # 平均深度
      Sum_Depth_m = sum(DEPTH_M, na.rm = TRUE),               # 总深度
      Avg_Capacity_Mm3 = mean(CAP_MCM, na.rm = TRUE),   # 平均水库容量
      Sum_Capacity_Mm3 = sum(CAP_MCM, na.rm = TRUE),    # 总水库容量
      Avg_Discharge_ls = mean(DIS_AVG_LS, na.rm = TRUE),      # 平均流量
      Sum_Discharge_ls = sum(DIS_AVG_LS, na.rm = TRUE),       # 总流量
      Avg_DOR_pc = mean(DOR_PC, na.rm = TRUE)                 # 调节度指数
    )
  return(summary_stats)
}

# 计算最近大坝特征的函数
calculate_nearest_dam_features <- function(station_point, reservoirs_dams) {
  distances <- st_distance(station_point, reservoirs_dams)
  nearest_dam <- reservoirs_dams[which.min(distances), ]
  
  nearest_dam_features <- nearest_dam %>%
    st_drop_geometry() %>%
    dplyr::select(
      Nearest_Dam_Height_m = DAM_HGT_M,        # 大坝高度
      Nearest_CatchmentArea_km2 = CATCH_SKM,   # 集水面积
      Nearest_Depth_m = DEPTH_M,               # 平均深度
      Nearest_Capacity_Mm3 = CAP_REP,      # 水库容量
      Nearest_Discharge_ls = DIS_AVG_LS,       # 平均流量
      Nearest_DOR_pc = DOR_PC                  # 调节度指数
    ) %>%
    mutate(
      Distance_to_Nearest_Dam_km = as.numeric(min(distances)) / 1000
    )
  return(nearest_dam_features)
}

# 计算时序特征的函数
calculate_temporal_features <- function(reservoirs_dams, sampling_year) {
  temporal_features <- reservoirs_dams %>%
    st_drop_geometry() %>%  # 排除几何信息
    summarize(
      Num_Dams_10yr = sum(YEAR >= (sampling_year - 10), na.rm = TRUE),
      Num_Dams_20yr = sum(YEAR >= (sampling_year - 20), na.rm = TRUE),
      Num_Dams_30yr = sum(YEAR >= (sampling_year - 30), na.rm = TRUE),
      Num_Dams_40yr = sum(YEAR >= (sampling_year - 40), na.rm = TRUE),
      Num_Dams_50yr = sum(YEAR >= (sampling_year - 50), na.rm = TRUE)
    )
  return(temporal_features)
}

# 处理所有站点的函数
process_stations <- function() {
  results <- data.frame()  # 初始化结果数据框
  
  for (i in 1:nrow(stations)) {
    id <- stations$ID[i]
    if (id<864) next
    lon <- stations$Longitude[i]
    lat <- stations$Latitude[i]
    sampling_year <- as.numeric(format(as.Date(stations$`Sampling_time`[i]), "%Y"))
    
    print(id)
    print(paste("Longitude:", lon))
    print(paste("Latitude:", lat))
    print(paste("Sampling Year:", sampling_year))
    
    # 创建一个点
    station_point <- st_sfc(st_point(c(lon, lat)), crs = st_crs(reservoirs_dams))
    
    # 查找站点对应的流域
    basin_tiff_file <- file.path(basin_tiff_dir, paste0(stations$ID[i], ".tif"))
    
    if (file.exists(basin_tiff_file)) {
      basin_tiff <- rast(basin_tiff_file)
      crs(basin_tiff) <- crs(reservoirs_dams)
      
      # 转换为 sf 对象
      basin_geom_sf <- st_as_sf(as.polygons(basin_tiff, dissolve = TRUE))
      
      # 使用流域数据进行空间查询
      reservoirs_dams_in_basin <- reservoirs_dams[st_intersects(reservoirs_dams, basin_geom_sf, sparse = FALSE), ]
      
      if (nrow(reservoirs_dams_in_basin) > 0) {
        # 计算统计特征、最近大坝特征和时序特征
        summary_stats <- calculate_summary_statistics(reservoirs_dams_in_basin)
        nearest_dam_features <- calculate_nearest_dam_features(station_point, reservoirs_dams_in_basin)
        temporal_features <- calculate_temporal_features(reservoirs_dams_in_basin, sampling_year)
      } else {
        # 如果没有水库数据，用 NA 填充
        summary_stats <- data.frame(Num_Dams = 0,
                                    Avg_DamHeight_m = 0,
                                    Max_DamHeight_m = 0,
                                    Avg_CatchmentArea_km2 = 0,
                                    Sum_CatchmentArea_km2 = 0,
                                    Avg_Depth_m = 0,
                                    Sum_Depth_m = 0,
                                    Avg_Capacity_Mm3 = 0,
                                    Sum_Capacity_Mm3 = 0,
                                    Avg_Discharge_ls = 0,
                                    Sum_Discharge_ls = 0,
                                    Avg_DOR_pc = 0)
        
        nearest_dam_features <- data.frame(Nearest_Dam_Height_m = 0,
                                           Nearest_CatchmentArea_km2 = 0,
                                           Nearest_Depth_m = 0,
                                           Nearest_Capacity_Mm3 = 0,
                                           Nearest_Discharge_ls = 0,
                                           Nearest_DOR_pc = 0,
                                           Distance_to_Nearest_Dam_km = 0)
        
        temporal_features <- data.frame(Num_Dams_10yr = 0,
                                        Num_Dams_20yr = 0,
                                        Num_Dams_30yr = 0,
                                        Num_Dams_40yr = 0,
                                        Num_Dams_50yr = 0)
      }
      
      # 合并特征，确保没有几何信息的列
      station_features <- data.frame(
        Station_ID = as.integer(stations$ID[i]),
        Sampling_time = stations$`Sampling_time`[i], 
        summary_stats,
        nearest_dam_features,
        temporal_features
      )
      
      # # 将 summary_stats、nearest_dam_features 和 temporal_features 展开为列
      # station_features <- cbind(station_features, summary_stats, nearest_dam_features, temporal_features)
      
      # 确保结果数据框的结构保持一致
      results <- bind_rows(results, station_features)
      
    } else {
      warning(paste("TIF file for station ID", stations$ID[i], "not found."))
    }
  }
  
  return(results)
}

# 处理所有站点
results <- process_stations()

# 输出合并结果为CSV文件
write_csv(results, output_path)

# 打印当前处理的结果
print(results)
