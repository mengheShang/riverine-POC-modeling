library(sf)
library(sp)
library(dplyr)

# 加载站点和水库大坝的矢量数据
stations <- read.csv("D:\\POC research\\data\\Sites position.csv")
coordinates(stations) <- ~Longitude + Latitude
proj4string(stations) <- CRS("+proj=longlat +datum=WGS84")

reservoirs_dams <- st_read("D:\\POC research\\data\\Reservoirs and Dams\\China_Dams.shp")

# 函数：寻找最近的水库或大坝
find_nearest_reservoir_dam <- function(station, reservoirs_dams) {
  distances <- st_distance(station, reservoirs_dams)
  nearest_index <- which.min(distances)
  nearest_reservoir_dam <- reservoirs_dams[nearest_index, ]
  return(nearest_reservoir_dam)
}

# 函数：将-99替换为NA
replace_neg99_with_na <- function(df) {
  df[df == -99] <- NA
  return(df)
}

# 处理一批站点的函数
process_stations <- function(start_index, end_index) {
  results <- data.frame()
  
  for (i in start_index:end_index) {
    lon <- stations$Longitude[i]
    lat <- stations$Latitude[i]
    
    print(i)
    print(paste("Longitude:", lon))
    print(paste("Latitude:", lat))
    
    # 创建一个点
    station_point <- st_sfc(st_point(c(lon, lat)), crs = st_crs(reservoirs_dams))
    
    # 查找最近的水库或大坝
    nearest_reservoir_dam <- find_nearest_reservoir_dam(station_point, reservoirs_dams)
    
    # 提取相关信息，仅保留指定的列
    reservoir_dam_info <- nearest_reservoir_dam %>%
      st_drop_geometry() %>%
      select(RanD_ID = GRAND_ID,
             ReservoirName = RES_NAME,
             DamName = DAM_NAME,
             River = RIVER,
             MainBasin = MAIN_BASIN,
             SubBasin = SUB_BASIN,
             Year = YEAR,
             DamHeight_m = DAM_HGT_M,
             DamLength_m = DAM_LEN_M,
             Area_km2 = AREA_SKM,
             ReportedCapacity_mcm = CAP_REP,
             Depth_m = DEPTH_M,
             AverageDischarge_m3s = DIS_AVG_LS,
             CatchmentArea_km2 = CATCH_SKM) %>%
      mutate(Station_ID = stations$ID[i], 
             Station_Longitude = lon, 
             Station_Latitude = lat) %>%
      replace_neg99_with_na() %>%
      select(Station_ID, Station_Longitude, Station_Latitude, everything())  # 将Station_ID移到第一列
    
    # 合并结果
    results <- rbind(results, reservoir_dam_info)
  }
  
  return(results)
}

# 处理一部分站点
process_batch <- function(start_index, batch_size) {
  end_index <- min(start_index + batch_size - 1, nrow(stations))
  batch_results <- process_stations(start_index, end_index)
  
  # 读取结果地址
  file_path <- "D:\\POC research\\data\\pco2Data_sites\\Reservoirs_Dams_Info.csv"
  
  # 输出合并结果为CSV文件
  write.csv(batch_results, file_path, row.names = FALSE)
  
  # 返回当前处理的结果
  return(batch_results)
}

# 设置每次处理的站点数量
batch_size <- 211
# 处理第一批站点
start_index <- 1

# 调用处理函数处理一部分站点
results <- process_batch(start_index, batch_size)

# 打印当前处理的结果
print(results)
