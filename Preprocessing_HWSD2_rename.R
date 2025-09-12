# 设置文件目录
directory <- "D:/POC research/data/HWSD2/HWSD2_D1"

# 获取目录中的所有文件
files <- list.files(directory, full.names = TRUE)

# 遍历文件并重命名
for (file in files) {
  # 提取文件名
  filename <- basename(file)
  
  # 检查文件名是否包含"_D1_"
  if (grepl("_D1_", filename)) {
    # 生成新文件名
    new_filename <- gsub("_D1_", "_", filename)
    
    # 生成新文件的完整路径
    new_file <- file.path(directory, new_filename)
    
    # 重命名文件
    file.rename(file, new_file)
    cat("Renamed:", file, "to", new_file, "\n")
  }
}

cat("Batch renaming completed.\n")
