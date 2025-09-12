# combine dem ####

library(whitebox)
library(terra)
library(sf)
library(tidyverse)

# # 设置工作路径
# data_path <- "D:/POC/data/SRTM"
# 
# # 获取该路径下所有.tif文件
# dem_files <- list.files(path = data_path, 
#                         pattern = "\\.tif$", 
#                         full.names = TRUE)
# 
# # 检查找到的文件
# print(paste("找到", length(dem_files), "个DEM文件:"))
# print(dem_files)
# 
# # 读取所有DEM文件
# dem_list <- lapply(dem_files, rast)
# 
# # 检查所有DEM的CRS是否一致
# crs_list <- lapply(dem_list, crs)
# if(length(unique(crs_list)) > 1) {
#   warning("DEM文件的坐标系统不一致，需要先统一CRS！")
#   # 如果需要，可以在这里添加CRS转换代码
# }
# 
# # 拼接DEM (使用terra包的mosaic函数)
# merged_dem <- do.call(mosaic, c(dem_list, fun = "mean"))
# 
# # 保存合并后的DEM
# output_file <- file.path(data_path, "SRTM_dem_90m.tif")
# writeRaster(merged_dem, output_file, overwrite = TRUE)
# 
# # 可视化检查
# plot(merged_dem, main = "拼接后的SRTM DEM (90m)")
# print(paste("拼接完成，结果已保存到:", output_file))
# 
# # 显示拼接后DEM的基本信息
# print("拼接后DEM的信息:")
# print(merged_dem)

# clip dem into small tiles ####
library(terra); library(whitebox)

setwd("D:/POC/data/SRTM")
dem_file <- "dem_tile_28.tif"
dem <- rast(dem_file)
plot(dem)

n_rows <- 10; n_cols <- 10
e <- ext(dem)
x_range <- seq(e$xmin, e$xmax, length.out = n_cols + 1)
y_range <- seq(e$ymin, e$ymax, length.out = n_rows + 1)

exts <- list()
for (i in seq_len(n_cols)) {
  for (j in seq_len(n_rows)) {
    xi_min <- x_range[i]; xi_max <- x_range[i+1]
    yj_min <- y_range[j]; yj_max <- y_range[j+1]
    exts[[length(exts) + 1]] <- ext(xi_min, xi_max, yj_min, yj_max)
  }
}
print(exts)

basin <- st_read("D:\\POC\\data\\basic_prediction\\9大流域片\\liuyu.shp")
basin <- st_transform(basin, crs(dem))
dem <- crop(dem, basin)
plot(dem)

for (i in seq_along(exts)) {
  tile_ext <- exts[[i]]
  dem_tile <- crop(dem, tile_ext)
  tile_name <- sprintf("dem_tile_28_%02d.tif", i)
  writeRaster(dem_tile, tile_name, overwrite = TRUE)
  print(paste(i,"finished"))
}

# dem to flowacc ####
# library(whitebox)
# wb <- "E:/POC research/data/1_DEM_watershed/SRTM_China_new.tif"
# dem_filled <- "E:/POC research/data/1_DEM_watershed/SRTM_China_DEM_filled.tif"
# dp <- "E:/POC research/data/1_DEM_watershed/flowdir.tif"
# fa <- "E:/POC research/data/1_DEM_watershed/FlowAcc_1km.tif"
# 
# wbt_fill_depressions(dem = wb, output = dem_filled)
# wbt_d8_pointer(dem = dem_filled, output = dp)
# wbt_d8_flow_accumulation(input = dp, output = fa, pntr = TRUE)

# dem to stream ####
for (i in 1:100) {
  print(paste("Start tile", i))

  wb <- sprintf("dem_tile_%02d.tif", i)
  dem_tile <- rast(wb)
  plot(dem_tile)
  
  fl <- sprintf("filled_%02d.tif", i)
  dp <- sprintf("flowdir_%02d.tif", i)
  fa <- sprintf("flowacc_%02d.tif", i)
  st <- sprintf("streams_%02d.tif", i)
  
  print(paste("start fill dem"))
  wbt_fill_depressions(dem = wb, output = fl)
  print(paste("filled: ", i))
  wbt_d8_pointer(dem = fl, output = dp)
  print(paste("flowdir: ",i))
  wbt_d8_flow_accumulation(input = dp, output = fa, pntr = TRUE)
  print(paste("flowacc: ", i))
  
  # flowac -> streams
  thr <- 1000
  fa_r <- rast(fa)
  writeRaster(fa_r > thr, st, overwrite = TRUE)
  gc()
}

# combine flowacc ####
data_path <- "E:/POC research/data/1_DEM_watershed/SRTM"
flowacc_files <- list.files(path = data_path,
                            pattern = "^flowacc_.*\\.tif$",
                            full.names = TRUE,
                            ignore.case = TRUE)

# 检查找到的文件
print(paste("找到", length(flowacc_files), "个文件:"))
print(flowacc_files)
flowacc_list <- lapply(flowacc_files, rast)

crs_list <- lapply(flowacc_list, crs)
if(length(unique(crs_list)) > 1) {
  warning("flowacc文件的坐标系统不一致，需要先统一CRS！")
}

# 拼接 (使用terra包的mosaic函数)
merged_flowacc <- do.call(mosaic, c(flowacc_list, fun = "max"))
# 保存
output_file <- file.path("E:/POC research/data/1_DEM_watershed/SRTM_flowacc_90m.tif")
writeRaster(merged_flowacc, output_file, overwrite = TRUE)

plot(merged_flowacc, main = "拼接flowacc")
print(paste("拼接完成，结果已保存到:", output_file))

# clip and mask 
library(sf)
flowacc <- rast("E:/POC research/data/1_DEM_watershed/SRTM_flowacc_90m.tif")
plot(flowacc)
basin <- st_read("D:\\POC\\data\\basic_prediction\\9大流域片\\liuyu.shp")
basin <- st_transform(basin, crs(flowacc))
flowacc_clipped <- mask(crop(flowacc, basin), basin)
plot(flowacc_clipped)
output_file <- file.path("E:/POC research/data/1_DEM_watershed/SRTM_flowacc_90m_China.tif")
writeRaster(flowacc_clipped, output_file, overwrite = TRUE)

# combine streams ####
data_path <- "E:/POC research/data/1_DEM_watershed/SRTM"
stream_files <- list.files(path = data_path,
                        pattern = "^streams_.*\\.tif$",
                        full.names = TRUE,
                        ignore.case = TRUE)

# 检查找到的文件
print(paste("找到", length(stream_files), "个文件:"))
print(stream_files)
stream_list <- lapply(stream_files, rast)

crs_list <- lapply(stream_list, crs)
if(length(unique(crs_list)) > 1) {
  warning("stream文件的坐标系统不一致，需要先统一CRS！")
}

# 拼接 (使用terra包的mosaic函数)
merged_stream <- do.call(mosaic, c(stream_list, fun = "max"))
merged_stream[merged_stream == 0] <- NA
# 保存
output_file <- file.path("E:/POC research/data/1_DEM_watershed/SRTM_stream_90m_new.tif")
basin <- st_read("D:\\POC\\data\\basic_prediction\\9大流域片\\liuyu.shp")
basin <- st_transform(basin, crs(merged_stream))
stream <- mask(crop(merged_stream, basin), basin)
writeRaster(stream, output_file, overwrite = TRUE)

plot(stream, main = "拼接stream")
print(paste("拼接完成，结果已保存到:", output_file))

# combine flowdir ####
data_path <- "D:/POC/data/SRTM"
flowdir_files <- list.files(path = data_path,
                           pattern = "^flowdir_.*\\.tif$",
                           full.names = TRUE,
                           ignore.case = TRUE)

# 检查找到的文件
print(paste("找到", length(flowdir_files), "个文件:"))
print(flowdir_files)
flowdir_list <- lapply(flowdir_files, rast)

crs_list <- lapply(flowdir_list, crs)
if(length(unique(crs_list)) > 1) {
  warning("flowdir文件的坐标系统不一致，需要先统一CRS！")
}

# 拼接 (使用terra包的mosaic函数)
merged_flowdir <- do.call(mosaic, c(flowdir_list, fun = "max"))
# 保存
output_file <- file.path("E:/POC research/data/1_DEM_watershed/SRTM_flowdir_90m.tif")
writeRaster(merged_flowdir, output_file, overwrite = TRUE)

plot(merged_flowdir, main = "拼接flowdir")
print(paste("拼接完成，结果已保存到:", output_file))

# clip and mask 
library(sf)
flowdir <- rast("E:/POC research/data/1_DEM_watershed/SRTM_flowdir_90m.tif")
plot(flowdir)
basin <- st_read("D:\\POC\\data\\basic_prediction\\9大流域片\\liuyu.shp")
basin <- st_transform(basin, crs(flowdir))
flowdir_clipped <- mask(crop(flowdir, basin), basin)
plot(flowdir_clipped)
output_file <- file.path("E:/POC research/data/1_DEM_watershed/SRTM_flowdir_90m_China.tif")
writeRaster(flowdir_clipped, output_file, overwrite = TRUE)

# watershed ####
pour_points <- st_read("D:/POC/data/SRTM/yellowriver_points.shp")
for (i in 1:nrow(pour_points)) {

  pt <- pour_points[i, ]
  pt_id <- as.character(pt$ID)  # 假设你的字段名为 ID，如果不是请替换
  
  cat("start station ID:", pt_id, "\n")

  pt_file <- sprintf("tmp_pour_point_%s.shp", pt_id)
  snapped_file <- sprintf("snapped_pour_point_%s.shp", pt_id)
  watershed_file <- sprintf("watershed_%s.tif", pt_id)

  st_write(pt, pt_file, delete_dsn = TRUE, quiet = TRUE)
  
  wbt_jenson_snap_pour_points(
    pour_pts = pt_file,
    streams = "E:/POC research/data/1_DEM_watershed/SRTM_stream_90m_China.tif",
    output = snapped_file,
    snap_dist = 100
  )
  
  wbt_watershed(
    d8_pntr = "E:/POC research/data/1_DEM_watershed/SRTM_flowdir_90m_China.tif",
    pour_pts = snapped_file,
    output = watershed_file
  )
  watershed <- rast(watershed_file)
  plot(watershed)
}

# get watershed with buffer crop ####
library(sf)
library(terra)
library(whitebox)

setwd("E:/POC research/data/5_prediction")
streams_rast <- rast("../1_DEM_watershed/SRTM_stream_90m_new.tif")
flowdir_rast <- rast("../1_DEM_watershed/SRTM/SRTM_flowdir_90m_China.tif")

pour_points <- st_read("../1_DEM_watershed/sites_shp/sampling_sites.shp") 
study_area_poly <- st_read("D:/POC/data/basic_prediction/9大流域片/liuyu.shp") 
basins <- st_read("../1_basin/8basin.shp")

# 分块处理函数
process_watershed_by_point <- function(pt, pt_id, buffer_dist_km = 1000) {
  # 创建临时文件路径
  pt_file <- paste0("tmp_pour_point_", pt_id, ".shp")
  snapped_file <- paste0("snapped_pour_point_", pt_id, ".shp")
  watershed_file <- paste0("watershed_", pt_id, ".tif")

  # 保存单点文件
  st_write(st_zm(pt), pt_file, delete_dsn = TRUE, quiet = TRUE)
  
  pt_buffered <- st_buffer(pt, dist = buffer_dist_km * 1000) %>% 
    st_intersection(study_area_poly)
  
  if (nrow(pt_buffered) == 0) {
    # 直接使用点位置（不缓冲）
    pt_buffered <- pt
    cat("Point", pt_id, "buffer completely outside study area. Using point location only.\n")
  }
  
  bbox <- st_bbox(pt_buffered)
  
  # 使用terra裁剪
  cropped_streams <- paste0("cropped_streams_", pt_id, ".tif")
  cropped_flowdir <- paste0("cropped_flowdir_", pt_id, ".tif")
  
  cat("Processing stream rast:", pt_id, "\n")
  crop(streams_rast, ext(bbox), filename = cropped_streams, overwrite = TRUE)
  cat("Processing flowdir rast:", pt_id, "\n")
  crop(flowdir_rast, ext(bbox), filename = cropped_flowdir, overwrite = TRUE)
  
  # 执行核心操作
  wbt_jenson_snap_pour_points(
    pour_pts = pt_file,
    streams = cropped_streams,
    output = snapped_file,
    snap_dist = 100
  )
  
  wbt_watershed(
    d8_pntr = cropped_flowdir,
    pour_pts = snapped_file,
    output = watershed_file
  )
  
  # 清理临时文件
  file.remove(cropped_streams, cropped_flowdir)
  
  return (snapped_file)
}

snapped_points_list <- list()

# 主循环
for (i in 1:nrow(pour_points)) {
  if (i>20) next
  pt <- pour_points[i, ]
  pt_id <- pt$ID
  
  if (st_intersects(pt, st_as_sfc(st_bbox(streams_rast)), sparse = FALSE)[1,1]) {
    cat("Processing point", pt_id, "\n")
    snapped_file <- process_watershed_by_point(pt, pt_id)
    snapped_sf <- st_read(snapped_file, quiet = TRUE)
    snapped_points_list[[length(snapped_points_list) + 1]] <- snapped_sf
  } else {
    cat("Point", pt_id, "outside extent. Skipping.\n")
  }
}


# get watershed with basin crop ####
library(sf)
library(terra)
library(whitebox)

# Set working directory
setwd("E:/POC research/data/1_DEM_watershed")

# Input raster paths
streams_rast <- rast("streams_yellowriver.tif")
flowdir_rast <- rast("flowdir_864.tif")

# Read point and basin data
pour_points <- st_read("yellowriver_points_snapped.shp")
basins <- st_read("../1_basin/8basin.shp") |>
  st_make_valid()

# Ensure matching CRS
pour_points <- st_transform(pour_points, st_crs(basins))

# Loop through each point
for (i in 1:nrow(pour_points)) {
  if (i!= 38) next
  pt <- pour_points[i, ]
  pt_id <- pt$ID
  
  # Find the basin the point falls into
  basin_match <- basins[st_intersects(pt, basins, sparse = FALSE)[1, ], ]
  
  # Skip if not in any basin
  if (nrow(basin_match) == 0) {
    cat("Point", pt_id, "not in any basin. Skipped.\n")
    next
  }
  
  cat("Processing point", pt_id, "\n")
  
  # Convert to SpatVector for raster masking
  basin_vect <- vect(basin_match)
  
  # Crop stream and flowdir using the matched basin
  cropped_streams <- crop(streams_rast, basin_vect, mask = TRUE)
  cropped_flowdir <- crop(flowdir_rast, basin_vect, mask = TRUE)
  
  # Save cropped rasters as temp files
  stream_file <- file.path("temp", paste0("stream_", pt_id, ".tif"))
  flowdir_file <- file.path("temp", paste0("flowdir_", pt_id, ".tif"))
  writeRaster(cropped_streams, stream_file, overwrite = TRUE)
  writeRaster(cropped_flowdir, flowdir_file, overwrite = TRUE)
  
  # Write pour point shapefile
  pt_file <- file.path("temp", paste0("point_", pt_id, ".shp"))
  st_write(st_zm(pt), pt_file, delete_dsn = TRUE, quiet = TRUE)
  
  # Define output file paths
  snapped_file <- file.path("temp", paste0("snapped_", pt_id, ".shp"))
  watershed_file <- file.path("temp", paste0("watershed_", pt_id, ".tif"))
  
  # Snap pour point
  wbt_jenson_snap_pour_points(
    pour_pts = snapped_file,
    streams = stream_file,
    output = snapped_file,
    snap_dist = 100
  )
  
  # Delineate watershed using snapped point and local flow direction raster
  wbt_watershed(
    d8_pntr = flowdir_file,
    pour_pts = pt_file,
    output = watershed_file
  )
}

# clip to yellowriver ####
library(terra)
library(sf)
library(whitebox)
setwd("E:/POC research/data/1_DEM_watershed/SRTM")
basins <- st_read("../../1_basin/8basin.shp") |>
  st_make_valid()
yellow_basin <- basins[basins$NAME == "黄河流域", ]

dem_raw <- rast("../temp/burned_dem_yellowriver.tif")

yellow_basin_vect <- vect(yellow_basin)

# 裁剪并掩膜
dem_crop <- crop(dem_raw, yellow_basin_vect)
dem_mask <- mask(dem_crop, yellow_basin_vect)

# 保存为黄河流域DEM
writeRaster(dem_mask, "../temp/burned_dem_yellowriver.tif", overwrite = TRUE)


wb <- file.path("burned_dem_yellowriver.tif")
fl <- file.path("filled_dem_yellowriver.tif")
dp <- file.path("flowdir_yellowriver.tif")
fa <- file.path("flowacc_yellowriver.tif")
st <- file.path("streams_yellowriver.tif")

wbt_fill_depressions(dem = wb, output = fl)
wbt_d8_pointer(dem = fl, output = dp)
wbt_d8_flow_accumulation(input = dp, output = fa, pntr = TRUE)

# flowac -> streams
thr <- 1000
fa_r <- rast(fa)
streams_r <- classify(fa_r, rcl = matrix(c(-Inf, thr, NA,
                                           thr, Inf, 1), ncol = 3, byrow = TRUE))

writeRaster(streams_r, st, overwrite = TRUE)

# burning streams ####
library(whitebox)

setwd("E:/POC research/data/1_DEM_watershed/temp")
dem <- "../SRTM/dem_yellowriver.tif"
streams <- "E:/POC research/data/POC_basic data/国家基础地理数据400万/hyd2_4l.shp"
burned_dem <- "burn_dem_yellowriver.tif"

wbt_fill_burn(
  dem = dem,
  streams = streams,
  output = burned_dem
)