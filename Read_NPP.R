# 加载必要的库
library(terra)
library(readxl)

# 设置输入输出路径
base_path <- "D:\\POC research\\data\\pco2Data\\npp\\"
sites_path <- "D:/POC research/data/Sites position.xlsx"
output_path <- "D:/POC research/data/pco2Data_sites/npp.csv"

# 读取站点数据
sites <- read_excel(sites_path)
site_coords <- cbind(sites$Longitude, sites$Latitude)

# 初始化存储结果的DataFrame
years <- 2001:2022
npp_data <- matrix(nrow = nrow(sites), ncol = length(years), dimnames = list(sites$ID, as.character(years)))

# 循环处理每一年的NPP数据
for (year in years) {
  tiff_path <- sprintf("%s%d_new.tif", base_path, year)
  
  # 使用 terra 读取 raster 文件
  npp_raster <- rast(tiff_path)
  
  # 使用更大的窗口进行掩膜操作
  npp_raster_filled <- terra::focal(npp_raster, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE, pad = TRUE)
  
  # 多次应用掩膜操作
  for (i in 1:3) {
    npp_raster_filled <- terra::focal(npp_raster_filled, w = matrix(1, 5, 5), fun = mean, na.rm = TRUE, pad = TRUE)
  }
  
  # 使用 terra 包的 extract 函数提取每个站点的土地利用信息
  extracted_values <- extract(npp_raster_filled, site_coords)
  
  # 提取唯一的一列
  npp_year_data <- extracted_values[, 1]
  
  # 存储提取值到结果数据框
  npp_data[, as.character(year)] <- npp_year_data
}

# 创建最终的DataFrame
final_data <- data.frame(Station_ID = rownames(npp_data), npp_data)

# 输出结果
print(final_data)

# 保存结果到CSV文件
write.csv(final_data, output_path, row.names = FALSE)

