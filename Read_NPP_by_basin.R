# 加载必要的R包
library(raster)
library(sf)
library(terra)
library(readr)

# 定义数据目录和文件列表
data_dir <- "D:/POC research/data/pco2Data/npp"
shp_file <- "D:/POC research/data/basin/8basin.shp"
output_path <- "D:/POC research/data/pco2Data_basin/npp_mean.csv"

# 读取流域shp文件
basin <- st_read(shp_file)

# 获取所有tiff文件的列表
tiff_files <- list.files(path = data_dir, pattern = "*.tif$", full.names = TRUE)

# 定义一个函数来计算每个流域的NPP平均值
calculate_mean_npp <- function(tiff_file, basin) {
  # 读取tiff文件
  npp_data <- rast(tiff_file)
  
  # 将shapefile转换为terra对象
  basin_terra <- vect(basin)
  
  # 初始化一个向量来存储每个流域的平均NPP值
  mean_values <- numeric(nrow(basin))
  
  # 逐个流域计算平均值
  for (i in 1:nrow(basin)) {
    single_basin <- basin_terra[i, ]
    npp_clipped <- crop(npp_data, ext(single_basin))
    npp_clipped <- mask(npp_clipped, single_basin)
    
    # 计算平均值并存储
    mean_val <- global(npp_clipped, "mean", na.rm = TRUE)$mean
    mean_values[i] <- mean_val
  }
  
  # 创建结果数据框
  result <- data.frame(basin_id = 1:nrow(basin), mean_npp = mean_values)
  return(result)
}

# 对所有tiff文件计算平均值
results <- lapply(tiff_files, calculate_mean_npp, basin)

# 合并所有年份的结果
combined_results <- do.call(cbind, lapply(results, function(x) x$mean_npp))
combined_results <- data.frame(basin_id = 1:nrow(basin), combined_results)

# 设置列名
colnames(combined_results) <- c("basin_id", paste0(2001:2022))

# 查看结果
print(combined_results)

# 写入csv文件
write_csv(combined_results, output_path)