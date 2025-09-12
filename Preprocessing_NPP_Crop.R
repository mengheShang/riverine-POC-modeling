library(sf)
library(terra)

# 定义数据目录和文件列表
data_dir <- "D:/POC research/data/pco2Data/npp"
shp_file <- "D:/POC research/data/basin/8basin.shp"
output_dir <- "D:/POC research/data/pco2Data/npp_clipped"  # 新的输出目录

# 读取流域shp文件
basin <- st_read(shp_file)

# 获取所有tiff文件的列表
tiff_files <- list.files(path = data_dir, pattern = "*.tif$", full.names = TRUE)

# 定义一个函数来裁剪和处理每个tiff文件
process_tiff <- function(tiff_file, basin, output_dir) {
  # 读取tiff文件
  npp_data <- rast(tiff_file)
  
  # 将shapefile转换为terra对象
  basin_terra <- vect(basin)
  
  # 裁剪tiff文件
  npp_clipped <- crop(npp_data, basin_terra)
  npp_clipped <- mask(npp_clipped, basin_terra)
  
  # 确保没有NA值
  values(npp_clipped)[is.na(values(npp_clipped))] <- 0
  
  # 保存裁剪后的tiff文件到新的输出目录
  output_file <- file.path(output_dir, basename(sub("_new.tif", "_clipped.tif", tiff_file)))
  writeRaster(npp_clipped, filename = output_file, overwrite = TRUE)
}

# 对所有tiff文件进行处理并存储到新的输出目录
lapply(tiff_files, process_tiff, basin, output_dir)
