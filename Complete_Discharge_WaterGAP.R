# 加载必要的库
library(ncdf4)
library(dplyr)
library(lubridate)
library(readxl)
library(writexl)

# 读取nc文件
nc_file <- "E:\\POC research\\data\\2_Discharge and Sediment\\WaterGAP\\watergap_22d_gswp3-w5e5_histsoc_dis_monthly_1901_2019.nc4"
nc_data <- nc_open(nc_file)

# 获取nc文件中的变量（假设时间变量名为"StdTime"，经度变量名为"lon"，纬度变量名为"lat"，径流量变量名为"dis"）
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat")
dis <- ncvar_get(nc_data, "dis")
time <- ncvar_get(nc_data, "time")

# 将时间变量转换为日期格式
origin <- as.Date("1901-01-01")  # 时间从1901-01-01开始
time_dates <- origin %m+% months(time)

# 关闭nc文件
nc_close(nc_data)

# 去除模型径流量小于100的值
dis[dis < 100] <- NA

# 读取站点表格
stations <- read_excel("E:\\POC research\\data\\3_POC records\\original_records_correct.xlsx")

# 确保站点表格中有经纬度和日期列
stations$Date <- as.Date(stations$Sampling_Time)

# 创建一个函数来找到最近的经纬度索引
find_nearest_index <- function(value, vector) {
  return(which.min(abs(vector - value)))
}

# 填补缺失的径流量数据
stations <- stations %>%
  rowwise() %>%
  mutate(
    Lon_Index = find_nearest_index(Longitude, lon),
    Lat_Index = find_nearest_index(Latitude, lat),
    Time_Index = find_nearest_index(Date, time_dates),
    `Water discharge2 (m3/s)` = ifelse(is.na(`Water discharge2 (m3/s)`),
                                       dis[Lon_Index, Lat_Index, Time_Index],
                                       `Water discharge2 (m3/s)`),
    `POC flux(g/s)` = ifelse(is.na(`POC flux(g/s)`) | (`POC flux(g/s)` == 0),
                             `Water discharge2 (m3/s)` * `POC (mg/L)` / 1000,
                             `POC flux(g/s)`)
    
  ) %>%
  select(-Lon_Index, -Lat_Index, -Time_Index)

# 保存补全后的表格
write_xlsx(stations, "E:\\POC research\\data\\3_POC records\\filled_records_WaterGAP_add.xlsx")