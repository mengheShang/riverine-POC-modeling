library(terra)   # 栅格
library(sf)      # 矢量
library(dplyr)   # 数据操作

setwd("E:/POC research/data")

# 读取小流域多边形
basins <- st_read("POC_basic data/POC_watershed/China-basin1.shp")

# 读取流量累积栅格
flow_accum <- rast("5_Prediction/Spatial/FlowAcc_1km.tif")

# 读取河网
river <- st_read("2_Discharge and Sediment/MERIT_Basins_v0.7/level_01_v0.7/pfaf_04_riv_3sMERIT.shp")

# 投影对齐
if (st_crs(basins) != st_crs(river)) {
  river <- st_transform(river, st_crs(basins))
}
if (crs(flow_accum) != st_crs(basins)$wkt) {
  flow_accum <- project(flow_accum, st_crs(basins)$wkt)
}

# 存储结果
results_list <- list()

for (i in 1:nrow(basins)) {
  basin <- basins[i, ]
  basin_id <- as.character(basin$CHINA_B1_)
  
  # 获取该流域内的河网线段
  river_in_basin <- st_intersection(river, basin)
  
  # 如果该流域没有河网，跳过
  if (nrow(river_in_basin) == 0) {
    message("No river segments in basin ID: ", basin_id)
    next
  }
  
  # 转成 terra 矢量
  river_vect <- vect(river_in_basin)
  
  # 用河网掩膜 flow accumulation
  flow_cropped <- crop(flow_accum, river_vect, mask = TRUE)
  
  # 如果全部是 NA，跳过
  if (all(is.na(values(flow_cropped)))) {
    message("All NA values for basin ID: ", basin_id)
    next
  }
  
  # 找最大值
  max_value <- global(flow_cropped, fun = "max", na.rm = TRUE)$max
  max_pos <- where.max(flow_cropped)
  
  if (nrow(max_pos) > 1) {
    message("Multiple max points in basin ID: ", basin_id)
  }
  
  # 获取坐标
  max_coords <- xyFromCell(flow_cropped, max_pos[, "cell"])
  
  # 保存为 sf 点
  points_sf <- st_as_sf(
    tibble(
      ID = basin_id,
      max_flow = max_value,
      x = max_coords[, 1],
      y = max_coords[, 2]
    ),
    coords = c("x", "y"),
    crs = st_crs(basins)
  )
  
  results_list[[i]] <- points_sf
}

# 合并
result <- bind_rows(results_list)

# 绘图
plot(result)

# 保存为 Shapefile
st_write(result, "5_Prediction/Spatial/Points_1.shp", delete_layer = TRUE)

#####
library(terra)   # 栅格
library(sf)      # 矢量
library(dplyr)   # 数据操作

setwd("E:/POC research/data")

# 读取小流域多边形
basins <- st_read("POC_basic data/POC_watershed/China-basin1.shp")

# 读取流量累积栅格
flow_accum <- rast("5_Prediction/Spatial/FlowAcc_1km.tif")

if (crs(flow_accum) != st_crs(basins)$wkt) {
  flow_accum <- project(flow_accum, st_crs(basins)$wkt)
}
# 读取河网栅格
river_raster <- rast("2_Discharge and Sediment/MERIT_Basins_v0.7/pfaf_04_riv_3sMERIT_raster.tif")

if (crs(river_raster) != crs(flow_accum)) {
  river_raster <- project(river_raster, flow_accum)
}

# 存储结果
results_list <- list()

for (i in 1:nrow(basins)) {
  basin <- basins[i, ]
  basin_id <- as.character(basin$CHINA_B1_)
  
  # 将流域转成 terra 矢量
  basin_vect <- vect(basin)
  
  # 裁剪河网栅格到流域范围
  river_crop <- crop(river_raster, basin_vect, mask = TRUE)
  
  # 如果该流域没有河网像元，跳过
  if (all(is.na(values(river_crop)))) {
    message("No river pixels in basin ID: ", basin_id)
    next
  }
  
  # 用河网掩膜 flow_accum
  flow_cropped <- mask(crop(flow_accum, basin_vect), river_crop)
  
  # 如果全部是 NA，跳过
  if (all(is.na(values(flow_cropped)))) {
    message("All NA values for basin ID: ", basin_id)
    next
  }
  
  # 找最大值
  max_value <- global(flow_cropped, fun = "max", na.rm = TRUE)$max
  max_pos <- where.max(flow_cropped)
  
  if (nrow(max_pos) > 1) {
    message("Multiple max points in basin ID: ", basin_id)
  }
  
  # 获取坐标
  max_coords <- xyFromCell(flow_cropped, max_pos[, "cell"])
  
  # 保存为 sf 点
  points_sf <- st_as_sf(
    tibble(
      ID = basin_id,
      max_flow = max_value,
      x = max_coords[, 1],
      y = max_coords[, 2]
    ),
    coords = c("x", "y"),
    crs = st_crs(basins)
  )
  
  results_list[[i]] <- points_sf
}

# 合并
result <- bind_rows(results_list)

# 绘图
plot(result)

# 保存为 Shapefile
st_write(result, "5_Prediction/Spatial/Points_2.shp", delete_layer = TRUE)
