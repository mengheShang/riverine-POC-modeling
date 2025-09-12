# 加载必要的库
library(raster)
library(readxl)
library(writexl)

# 设置输入输出路径
base_path <- "D:\\POC research\\data\\pco2Data\\land_covernew\\"
sites_path <- "D:/POC research/data/Sites position.xlsx"
output_path <- "D:/POC research/data/variation/land_use.xlsx"

# 读取站点数据
sites <- read_excel(sites_path)
site_coords <- cbind(sites$Longitude, sites$Latitude)

# 初始化存储结果的DataFrame
years <- 1985:2022
land_use_data <- matrix(nrow = nrow(sites), ncol = length(years), dimnames = list(sites$ID, as.character(years)))

# 循环处理每一年的土地利用数据
for (year in years) {
  tiff_path <- sprintf("%s%d_1.tif", base_path, year)
  
  # 检查文件是否存在
  if (!file.exists(tiff_path)) {
    message("Warning: File does not exist and will be skipped: ", tiff_path)
    next  # 跳过当前循环的剩余部分
  }
  
  # 读取土地利用TIFF文件
  land_use_raster <- raster(tiff_path)
  
  # 构建经纬度坐标
  #res_values <- res(land_use_raster)  # 获取每个像素的分辨率
  #x_min <- extent(land_use_raster)[1] + res_values[1] / 2
  #y_max <- extent(land_use_raster)[4] - res_values[2] / 2
  
  # 创建网格坐标矩阵
  #cols <- ncol(land_use_raster)
  #rows <- nrow(land_use_raster)
  #grid_coords <- expand.grid(x = seq(x_min, x_min + (cols-1) * res_values[1], by = res_values[1]),
  #                           y = seq(y_max, y_max - (rows-1) * res_values[2], by = -res_values[2]))
  
  # 使用最临近法提取每个站点的土地利用信息
  extracted_values <- extract(land_use_raster, site_coords, method = "simple", cellnumbers = TRUE)
  closest_cells <- extracted_values[,1, drop = FALSE]
  land_use_data[, as.character(year)] <- land_use_raster[closest_cells]
}

# 创建最终的DataFrame
final_data <- data.frame(Station_ID = rownames(land_use_data), land_use_data)

# 输出结果
print(final_data)

# 写入Excel文件
write_xlsx(final_data, output_path)
