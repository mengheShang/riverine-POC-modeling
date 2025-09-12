library(terra)   # 处理栅格数据
library(sf)      # 处理矢量数据
library(dplyr)   # 数据框操作
setwd("E:/POC research/data")
# 读取小流域多边形（矢量）
basins <- st_read("E:/POC research/data/POC_basic data/POC_watershed/China-basin1_Project.shp")

# 读取流量累积栅格
flow_accum <- rast("E:/POC research/data/5_Prediction/Spatial/SRTM_flowacc_90m.tif")

# 确保坐标系一致
st_crs(basins) == crs(flow_accum) 
flow_accum <- project(flow_accum, st_crs(basins)$wkt)

plot(flow_accum, col = terrain.colors(50))
plot(st_geometry(basins), add = TRUE, border = "red")

# 方法：按小流域裁剪栅格 -> 找最大值 -> 记录坐标
# result <- basins %>%
#   group_by(`CHINA_B1_`) %>%
#   group_map(~ {
#     basin_mask <- vect(.x)
#     flow_cropped <- crop(flow_accum, basin_mask, mask = TRUE)
#     
#     max_value <- global(flow_cropped, fun = "max", na.rm = TRUE)$max
#     max_pos <- where.max(flow_cropped)
#     
#     if (length(max_pos) > 1) {
#       ID = as.character(.x[["CHINA_B1_"]][1]) 
#       print(ID)
#       message("Multiple max points in basin ID: ", ID)
#     }
#     
#     max_coords <- xyFromCell(flow_cropped, max_pos[, "cell"])
#     
#     # 返回多点几何
#     tibble(
#       ID = .x$CHINA_B1_,
#       max_flow = max_value,
#       geometry = st_sfc(st_multipoint(max_coords), crs = st_crs(basins))  # 使用st_multipoint
#     )
#   }) %>%
#   bind_rows() %>%
#   st_as_sf()

results_list <- list()

for (i in 1:nrow(basins)) {
  basin <- basins[i, ]
  basin_id <- as.character(basin$CHINA_B1_)
  
  basin_mask <- vect(basin)
  flow_cropped <- crop(flow_accum, basin_mask, mask = TRUE)
  
  max_value <- global(flow_cropped, fun = "max", na.rm = TRUE)$max
  max_pos <- where.max(flow_cropped)
  
  if (nrow(max_pos) > 1) {
    message("Multiple max points in basin ID: ", basin_id)
  }
  
  max_coords <- xyFromCell(flow_cropped, max_pos[, "cell"])

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

result <- bind_rows(results_list)

plot(result)

# 保存为Shapefile
st_write(result, "5_Prediction/Spatial/Points.shp", delete_layer = TRUE)
