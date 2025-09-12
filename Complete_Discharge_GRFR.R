# 加载必要的库
library(ncdf4)
library(dplyr)
library(lubridate)
library(readxl)
library(writexl)
library(sf)
library(stringr)

# 读取 GRFR nc 文件
nc_file <- "E:\\POC research\\data\\2_Discharge and Sediment\\GRFR\\output_pfaf_04_1979-2019.nc"
nc_data <- nc_open(nc_file)

# 获取所有时间点
time <- ncvar_get(nc_data, "time")
origin <- as.Date("1979-01-01")  # 时间从 1979-01-01 开始
time_dates <- origin + days(time)

# 读取站点表格
stations <- read_excel("E:\\POC research\\data\\3_POC records\\original_records_correct.xlsx")

# 确保站点表格中有日期列
stations$Date <- as.Date(stations$Sampling_Time)
names(stations)[names(stations) == "Water discharge2 (m3/s)"] <- "Water_discharge(m3/s)"

stations$Discharge_filled_by_model <- FALSE

# 读取 shapefile 文件，获取河流段及其 rivid
shp_file <- "E:\\POC research\\data\\2_Discharge and Sediment\\MERIT_Basins_v0.7\\level_01_v0.7\\pfaf_04_riv_3sMERIT.shp"
river_shp <- st_read(shp_file)

# 将站点表格转换为空间点对象
stations_sf <- st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = st_crs(river_shp))

# 找到最近河段的 rivid 值
stations$station_rivid <- river_shp$COMID[st_nearest_feature(stations_sf, river_shp)]

#####
# nearest_index <- st_nearest_feature(stations_sf, river_shp)
# stations$station_rivid <- river_shp$COMID[nearest_index]
# matched_rivers <- river_shp[nearest_index, ]
# 
# # 给河流线段添加站点 ID
# matched_rivers$StationID <- stations$ID  
# 
# # 只保留 COMID 和 StationID 这两个属性
# matched_rivers_out <- matched_rivers %>% 
#   dplyr::select(COMID, StationID, geometry)
# 
# # 保存为 shapefile
# st_write(matched_rivers_out, "E:\\POC research\\data\\2_Discharge and Sediment\\matched_rivers_MERIT.shp", 
#          delete_dsn = TRUE)
#####

# 从 NetCDF 中获取全部 rivid，并匹配索引
nc_rivids <- ncvar_get(nc_data, "rivid")
stations$nc_index <- match(stations$station_rivid, nc_rivids)

# 创建一个函数来找到最近的时间索引
find_nearest_time_index <- function(date, time_dates) {
  return(which.min(abs(time_dates - date)))
}

# 循环每一条记录
count = 0
for (i in 1:nrow(stations)) {
  # 获取当前记录的日期和对应的 NetCDF 索引
  current_date <- stations$Date[i]
  current_nc_index <- stations$nc_index[i]
  
  # 如果索引为空，跳过该条
  if (is.na(current_nc_index)) next
  
  # 找到最近的时间索引
  time_index <- find_nearest_time_index(current_date, time_dates)
  
  # 读取当前站点对应时间点的流量数据
  discharge_value <- ncvar_get(nc_data, "Qout", start = c(current_nc_index, time_index), count = c(1, 1))
  
  # 将读取到的流量值补充到相应位置
  if (is.na(stations$`Water_discharge(m3/s)`[i])) {
    count <- count+1
    stations$`Water_discharge(m3/s)`[i] <- discharge_value
    stations$`POC flux(g/s)`[i] <- discharge_value * stations$`POC (mg/L)`[i] 
    stations$Discharge_filled_by_model[i] <- TRUE 
  }
}

# 关闭 nc 文件
nc_close(nc_data)
print(count)
# 保存补全后的表格
write_xlsx(stations, "E:\\POC research\\data\\3_POC records\\filled_records_GRFR_add.xlsx")
