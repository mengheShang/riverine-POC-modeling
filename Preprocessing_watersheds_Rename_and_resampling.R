#copy and rename####
setwd("E:\\POC research\\data\\1_DEM_watershed\\watershed_new_500m")
files <- list.files(pattern = "^_")
file.rename(from = files,
            to = sub("^_", "", files))

# 设置源文件夹和目标文件夹路径
source_folder <- "E:\\POC research\\data\\1_DEM_watershed\\watershed"
target_folder <- "E:\\POC research\\data\\1_DEM_watershed\\watershed_new"  

# 获取源文件夹中所有.tif文件
tif_files <- list.files(path = source_folder, 
                        pattern = "\\.tif$", 
                        ignore.case = TRUE, 
                        full.names = TRUE)

# 提取文件名中的数字部分并筛选小于864的文件
files_to_copy <- sapply(tif_files, function(file) {
  # 从文件名中提取数字部分
  file_name <- basename(file)
  num_part <- as.numeric(gsub("[^0-9]", "", file_name))
  
  # 检查是否是有效的数字且小于864
  if (!is.na(num_part) && num_part < 864) {
    return(file)
  } else {
    return(NA)
  }
})

# 复制文件到目标文件夹
file.copy(files_to_copy, file.path(target_folder, basename(files_to_copy)))

#resample####
library(terra)

# 设置路径
input_folder <- "E:\\POC research\\data\\1_DEM_watershed\\watershed_new" 
output_folder <- "E:\\POC research\\data\\1_DEM_watershed\\watershed_new_500m"  
target_resolution <- 500  # 目标分辨率(米)

# 确保输出文件夹存在
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

# 获取所有TIFF文件
tif_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE, ignore.case = TRUE)

# 定义重采样函数
resample_to_500m <- function(input_file, output_folder) {
  # 读取栅格
  r <- rast(input_file)
  
  # 获取原始分辨率
  original_res <- res(r)[1]
  
  # 检查是否需要处理
  if (original_res == target_resolution) {
    message("文件 ", basename(input_file), " 已经是500米分辨率，跳过处理")
    return(NULL)
  }
  
  # 根据分辨率差异选择处理方法
  if (original_res < target_resolution) {
    # 如果原始分辨率更高(如250m→500m)，使用聚合
    fact <- target_resolution / original_res
    if (fact <= 0) {
      stop("计算出的聚合系数无效: ", fact)
    }
    r_mean <- aggregate(r, fact = fact, fun = "mean")
    r_out <- round(r_mean) 
  } else {
    # 如果原始分辨率更低(如1000m→500m)，使用重采样
    template <- rast(ext(r), resolution = target_resolution, crs = crs(r))
    r_out <- resample(r, template, method = "near")
  }
  
  # 构建输出文件名
  output_file <- file.path(output_folder, basename(input_file))
  
  # 写入文件
  writeRaster(r_out, filename = output_file, overwrite = TRUE)
  
  return(output_file)
}

# 对所有文件应用重采样
results <- lapply(tif_files, function(x) {
  tryCatch({
    resample_to_500m(x, output_folder)
  }, error = function(e) {
    message("处理文件 ", basename(x), " 时出错: ", e$message)
    return(NULL)
  })
})

sum(sapply(results, is.null))