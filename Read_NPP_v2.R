library(terra)
library(readxl)
library(sp)

# 设置输入输出路径
base_path <- "D:\\POC research\\data\\pco2Data\\npp\\"
sites_path <- "D:/POC research/data/Sites position.xlsx"
output_path <- "D:/POC research/data/pco2Data_sites/npp.csv"

# 读取站点数据
sites <- read_excel(sites_path)
coordinates(sites) <- ~Longitude + Latitude
proj4string(sites) <- CRS("+proj=longlat +datum=WGS84")

# 初始化存储结果的DataFrame
years <- 2001:2022
npp_data <- matrix(nrow = nrow(sites), ncol = length(years), dimnames = list(sites$ID, as.character(years)))

# 创建寻找最近非空栅格值的函数
get_row_col <- function(raster, cell) {
  row_col <- rowColFromCell(raster, cell)
  return(list(row = row_col[1], col = row_col[2]))
}

get_adjacent_cells <- function(raster, cell, radius) {
  dim_raster <- dim(raster)  # 获取栅格的维度（行和列数）
  row_col <- get_row_col(raster, cell)  # 获取当前单元的行和列号
  row <- row_col$row
  col <- row_col$col
  
  adjacent_cells <- c()  # 初始化相邻单元的列表
  
  for (i in -radius:radius) {
    for (j in -radius:radius) {
      if (i == 0 && j == 0) next  # 跳过中心单元
      new_row <- max(1, min(row + i, dim_raster[1]))
      new_col <- max(1, min(col + j, dim_raster[2]))
      new_cell <- cellFromRowCol(raster, new_row, new_col)  # 获取新的单元号
      adjacent_cells <- c(adjacent_cells, new_cell)  # 添加到相邻单元的列表中
    }
  }
  return(adjacent_cells)
}

find_nearest_non_na <- function(raster, point) {
  cell <- cellFromXY(raster, coordinates(point))
  if (!is.na(values(raster)[cell])) {
    return(values(raster)[cell])
  }
  
  radius <- 1
  while (TRUE) {
    cells <- get_adjacent_cells(raster, cell, radius)
    non_na_cells <- cells[!is.na(values(raster)[cells])]
    if (length(non_na_cells) > 0) {
      return(values(raster)[non_na_cells[1]])
    }
    radius <- radius + 1
  }
}

# 循环处理每一年的NPP数据
for (year in years) {
  tiff_path <- sprintf("%s%d_new.tif", base_path, year)
  
  # 使用 terra 读取 raster 文件
  npp_raster <- rast(tiff_path)
  
  for (i in 1:nrow(sites)) {
    point <- sites[i, ]
    npp_value <- find_nearest_non_na(npp_raster, point)
    
    # Debugging: Print the year, site index, and npp_value
    print(paste("Year:", year, "Site index:", i, "NPP value:", npp_value))
    
    # Ensure npp_value is correctly retrieved
    if (!is.na(npp_value)) {
      npp_data[i, as.character(year)] <- npp_value
    } else {
      npp_data[i, as.character(year)] <- NA  # Handle NA values appropriately
    }
  }
}

# 创建最终的DataFrame
final_data <- data.frame(Station_ID = rownames(npp_data), npp_data)

# 输出结果
write.csv(final_data, output_path, row.names = FALSE)
print(final_data)


