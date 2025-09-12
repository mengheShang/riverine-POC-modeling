library(terra)
library(readxl)
library(purrr)
library(dplyr)
library(readr)

tif_directory <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m"  
input_file <- "E:\\POC research\\data\\3_POC records\\clean_records.xlsx"

sampling_data <- read_excel(input_file) %>%
  distinct(ID, Longitude, Latitude)

get_watershed_area <- function(tif_file) {
  raster_layer <- rast(tif_file)
  message("Processing file: ", tif_file)

  total_area_km2 <- expanse(raster_layer, unit = "km", transform = TRUE)
  print(total_area_km2[1,2])
  return(total_area_km2[1,2])
}

# 根据采样数据中的站点ID查找对应的流域TIF文件并计算面积
calculate_areas_for_sampling_data <- function(sampling_data) {
  results <- data.frame(ID = sampling_data$ID, Area = NA)  # 初始化结果数据框
  
  for (i in 1:nrow(sampling_data)) {
    station_id <- sampling_data$ID[i]
    if (station_id < 864)next
    # 构建与站点ID相对应的TIF文件名（假设文件名格式为 "ID.tif"）
    tif_file <- file.path(tif_directory, paste0(station_id, ".tif"))
    
    if (file.exists(tif_file)) {
      # 计算流域面积
      area_km2 <- get_watershed_area(tif_file)
      results$Area[i] <- area_km2
    } else {
      warning(sprintf("TIF file for station ID %s not found!", station_id))
    }
  }
  
  return(results)
}

# Step 7: 计算所有站点的流域面积
watershed_areas <- calculate_areas_for_sampling_data(sampling_data)
# Step 8: 输出结果
print(watershed_areas)

# Step 9: 将结果保存为CSV文件
output_file <- "E:/POC research/data/2_inputData_watershed/Features/area.csv"
write_csv(watershed_areas, file = output_file)
