# 加载必要的库
library(terra)
library(readxl)
library(writexl)
library(dplyr)

# 输入文件路径
dem_file <- "E:/POC research/data/1_DEM_watershed/SRTM/SRTM_dem_90m.tif"
stations_path <- "E:\\POC research\\data\\3_POC records\\original_records_correct.xlsx"

# 读取数字高程模型 (DEM)
dem <- rast(dem_file)

# 将 DEM 中的 -9999 值替换为 NA
# dem[dem == -9999] <- NA
res_ratio <- 500 / 90  
dem <- aggregate(dem, fact = res_ratio, fun = "mean")  # 使用均值聚合

# 读取采样点数据
stations <- read_excel(stations_path) %>%
  distinct(ID, Longitude, Latitude)

# 将采样点转换为空间对象
stations_sp <- vect(stations, geom = c("Longitude", "Latitude"), crs = crs(dem))

# 计算坡度栅格
slope <- terrain(dem, v = "slope", unit = "degrees")

slope_values <- rep(NA, nrow(stations))
# # 对每个采样点计算坡度
# for (i in 1:nrow(stations)) {
#   point <- stations_sp[i, ]
#   cell <- cellFromXY(dem, crds(point))  # 获取点对应的像元位置
#   
#   neighbors <- adjacent(dem, cells = cell, directions = 8, pairs = FALSE)  # 获取周围8个像元
#   
#   # 检查周围8个像元是否都有非空值
#   if (all(!is.na(values(dem)[neighbors]))) {
#     # 如果周围8个像元都非空，提取坡度值
#     slope_values[i] <- extract(slope, point)[, 2]
#   } else {
#     # 如果有空值，找到最近的、周围8个像元都非空的点
#     valid_point_found <- FALSE
#     search_radius <- 1
#     while (!valid_point_found) {
#       nearby_cells <- adjacent(dem, cells = cell, directions = 8 * search_radius, pairs = FALSE, include = TRUE)
#       valid_cells <- nearby_cells[apply(values(dem)[adjacent(dem, cells = nearby_cells, directions = 8, pairs = FALSE)], 1, function(x) all(!is.na(x)))]
#       if (length(valid_cells) > 0) {
#         valid_point_found <- TRUE
#         cell <- valid_cells[1]
#         slope_values[i] <- extract(slope, xyFromCell(dem, cell))[, 2]
#       }
#       search_radius <- search_radius + 1
#     }
#   }
# }
# 
# # 将坡度数据添加到采样点数据框中
# stations$slope <- slope_values
# 
# # 查看结果
# print(stations)


slope_values <- extract(slope, stations_sp)[, 2]
stations$slope <- slope_values
# 保存结果
write_xlsx(stations, "E:\\POC research\\data\\2_inputData_watershed\\Features.xlsx")

