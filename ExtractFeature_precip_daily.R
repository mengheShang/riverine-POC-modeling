library(terra)
library(ncdf4)
library(dplyr)
library(readxl)
library(readr)
library(sp)
library(lubridate)

# 输入文件路径
stations_path <- "D:/POC research/data/3_POC records/monthly_records.xlsx"
basin_tiff_dir <- "D:/POC research/data/1_DEM_watershed/watershed_2"
nc_files <- list.files("E:/POC research/data/2_pco2Data/ERA5Land_daily/", pattern = "\\.nc$", full.names = TRUE)
output_path <- "D:/POC research/data/2_inputData_watershed/precipitation_variables.csv"

# 读取站点数据
stations <- read_excel(stations_path)
coordinates(stations) <- ~Longitude + Latitude
proj4string(stations) <- CRS("+proj=longlat +datum=WGS84")

# 函数：选择合适的NetCDF文件
select_nc_file <- function(sampling_time, nc_files) {
  year <- as.integer(format(sampling_time, "%Y"))
  if (year >= 1985 & year <= 1990) {
    return(grep("1985-1990", nc_files, value = TRUE))
  } else if (year >= 1991 & year <= 2000) {
    return(grep("1991-2000", nc_files, value = TRUE))
  } else if (year >= 2001 & year <= 2009) {
    return(grep("2001-2009", nc_files, value = TRUE))
  } else if (year >= 2010 & year <= 2015) {
    return(grep("2010-2015", nc_files, value = TRUE))
  } else if (year >= 2016 & year <= 2024) {
    return(grep("2016-2024", nc_files, value = TRUE))
  }
}

# 函数：根据文件的起始时间转换时间
convert_time_to_date <- function(time_var) {
  time_dates <- as.POSIXct(time_var, origin = "1970-01-01", tz = "GMT")
  time_dates <- time_dates - as.difftime(1, units = "days")  # 将每日00点的降水分配到前一天
  return(time_dates)
}

# 函数：提取降水变量
extract_basin_precip <- function(station, nc_file, basin_tiff_file) {
  # 打开 NetCDF 文件
  nc_data <- nc_open(nc_file)
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  time <- ncvar_get(nc_data, "valid_time")  
  fill_value <- ncatt_get(nc_data, "tp", "_FillValue")$value
  
  # 转换时间
  time_dates <- convert_time_to_date(time)
  
  # 找到采样时间对应的时间索引，以及前一个月的索引
  sampling_date <- as.Date(station$Sampling_time)
  year_month <- format(sampling_date, "%Y-%m")
  prev_month_date <- as.Date(paste0(format(sampling_date - months(1), "%Y-%m"), "-01"))
  months_to_extract <- c(prev_month_date, as.Date(paste0(year_month, "-01")))
  
  # 获取需要的日期范围
  start_date <- prev_month_date
  end_date <- as.Date(paste0(format(sampling_date + months(1), "%Y-%m"), "-01")) - 1
  date_range <- seq.Date(start_date, end_date, by = "day")
  
  # 找到对应的时间索引
  time_index <- which(as.Date(time_dates) >= start_date & as.Date(time_dates) <= end_date)
  
  # 分块提取降水数据（提取采样当月和前一个月的降水）
  precipitation_raw <- ncvar_get(nc_data, "tp", start = c(1, 1, time_index[1]), count = c(-1, -1, length(time_index)))
  precipitation_raw[precipitation_raw == fill_value] <- NA  # 使用 NA 替换缺失值
  precipitation <- precipitation_raw * 1000  # 单位转换为毫米
  precipitation <- aperm(precipitation, c(2, 1, 3))  # 维度调换
  
  nc_close(nc_data)
  
  # 初始化向量来存储每日的流域平均降水量
  daily_mean_precip <- numeric(length(time_index))  # 存储每一天的平均降水量
  
  # 获取对应的日期
  selected_dates <- as.Date(time_dates[time_index])
  
  # 读取流域TIF文件并重采样到降水数据的分辨率
  basin_tiff <- rast(basin_tiff_file)
  basin_tiff <- project(basin_tiff, crs(rast(precipitation[,,1], extent = ext(c(min(lon), max(lon), min(lat), max(lat))), crs = "+proj=longlat +datum=WGS84")))
  
  # 遍历所有时间层，计算每日的流域平均降水量
  for (t in 1:dim(precipitation)[3]) {
    daily_rast <- rast(precipitation[,,t], extent = ext(c(min(lon), max(lon), min(lat), max(lat))), crs = "+proj=longlat +datum=WGS84")
    daily_rast <- resample(daily_rast, basin_tiff, method = "near")
    daily_rast_masked <- mask(crop(daily_rast, basin_tiff), basin_tiff)
    
    plot(daily_rast)
    plot(daily_rast_masked)
    
    # 计算每日的流域平均降水量
    daily_mean_precip[t] <- global(daily_rast_masked, "mean", na.rm = TRUE)$mean
    
    rm(daily_rast, daily_rast_masked)
    gc()
  }
  
  # 使用日期过滤来区分前一个月和当前月的数据
  prev_month_mask <- format(selected_dates, "%Y-%m") == format(prev_month_date, "%Y-%m")
  current_month_mask <- format(selected_dates, "%Y-%m") == year_month
  
  # 分别获取前一个月和当前月的降水数据
  prev_month_precip <- daily_mean_precip[prev_month_mask]
  current_month_precip <- daily_mean_precip[current_month_mask]
  
  # 计算所需的降水变量（基于当前月的数据）
  max_daily_precip <- max(current_month_precip, na.rm = TRUE)  # 当前月最大日降水量
  mean_precip <- mean(current_month_precip, na.rm = TRUE)  # 当前月平均降水量
  rainy_days <- sum(current_month_precip > 1, na.rm = TRUE)  # 当前月降水天数
  # 计算当前月连续降水天数
  rle_current <- rle(current_month_precip > 1)
  consecutive_rainy_days <- ifelse(any(rle_current$values), max(rle_current$lengths[rle_current$values], na.rm = TRUE), 0)
  extreme_rain_days_50mm <- sum(current_month_precip > 50, na.rm = TRUE)  # 当前月极端降水天数
  # 计算当前月降水量的95%百分位
  percentile_95 <- quantile(current_month_precip, 0.95, na.rm = TRUE)
  # 统计超过95%百分位的极端降水天数
  extreme_rain_days_95p <- sum(current_month_precip > percentile_95, na.rm = TRUE)
  
  # 计算前一个月的累计降水量
  cumulative_precip_prev_month <- sum(prev_month_precip, na.rm = TRUE)
  
  # 释放内存
  rm(precipitation_raw, precipitation, basin_tiff)
  gc()
  
  return(data.frame(
    ID = station$ID,
    Sampling_time = year_month,
    max_daily_precip = max_daily_precip,
    mean_daily_precip = mean_precip,
    rainy_days = rainy_days,
    consecutive_rainy_days = consecutive_rainy_days,
    extreme_rain_days_50mm = extreme_rain_days_50mm,
    extreme_rain_days_95p = extreme_rain_days_95p, 
    precip_prev_month = cumulative_precip_prev_month
  ))
}


# 初始化结果数据框
precip_results <- data.frame()

# 对所有站点进行计算
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  sampling_time <- as.Date(stations$Sampling_time[i])
  basin_tiff_file <- file.path(basin_tiff_dir, paste0("_", station_id, ".tif"))
  
  # 输出当前处理的站点信息
  cat("Processing station ID:", station_id, "Sampling time:", format(sampling_time, "%Y-%m"), "\n")
  
  # 选择正确的 NetCDF 文件
  nc_file <- select_nc_file(sampling_time, nc_files)
  
  if (file.exists(basin_tiff_file) & length(nc_file) > 0) {
    station_precip <- extract_basin_precip(stations[i,], nc_file, basin_tiff_file)
    precip_results <- bind_rows(precip_results, station_precip)
  } else {
    warning(paste("TIF file or NetCDF file for station ID", station_id, "not found."))
  }
  gc()
}

# 查看结果
print(precip_results)

# 写入 CSV 文件
write_csv(precip_results, output_path)
