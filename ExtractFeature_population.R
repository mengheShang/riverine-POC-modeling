library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径 ----------------------------------------------------------------
setwd("E:/POC research/data")  
stations_path <- "./3_POC records/clean_records.xlsx"
pop_base_dir <- "./2_Population"
basin_tiff_dir <- "./1_DEM_watershed/watershed_new_500m"
output_path <- "./2_inputData_watershed/Features/population.csv"

# 读取站点数据 ----------------------------------------------------------------
stations <- read_excel(stations_path)

# 读取参考流域栅格 ----------------------------------------------------
target_crs <- "+proj=aea +lat_1=25 +lat_2=47 +lat_0=30 +lon_0=105 +x_0=0 +y_0=0 +datum=WGS84 +units=m"
basin_ref <- rast("./1_DEM_watershed/watershed_new_500m/1.tif") 
basin_ref_proj <- project(basin_ref, target_crs, method = "near")

# prepare basins
basin_paths <- list.files(basin_tiff_dir, pattern = "\\.tif$", full.names = TRUE)
basin_ids   <- gsub("^([0-9]+)\\.tif$", "\\1", basename(basin_paths))

basins <- setNames(
  lapply(basin_paths, function(path) {
    bm <- rast(path)
    bm_proj <- project(bm, target_crs, method = "near")
    return(bm_proj)
  }),
  basin_ids
)


# 定义目标年份范围 -----------------------------------------------------------
target_years <- 1990:2020

# 生成ADF文件夹路径映射 ------------------------------------------------------
adf_dirs <- file.path(pop_base_dir, paste0("pop", target_years, "/tpop", target_years))
existing_years <- target_years[file.exists(adf_dirs)]
adf_dirs <- adf_dirs[file.exists(adf_dirs)]

if (length(adf_dirs) == 0) {
  stop("No valid ADF directories found in: ", pop_base_dir)
}

# 函数：处理人口栅格数据 ----------------------------------------------------------
process_pop <- function(adf_dir, template) {
  pop_rast <- rast(file.path(adf_dir, "w001001.adf"))
  
  nodata_val <- -2147483648 
  pop_rast[pop_rast == nodata_val] <- NA
  
  pop_proj <- project(pop_rast, template, method = "bilinear")
  if (!compareGeom(pop_proj, template)) {
    pop_proj <- resample(pop_proj, template)
  }
  pop_proj <- crop(pop_proj, template)
  
  names(pop_proj) <- basename(dirname(adf_dir)) %>% 
    gsub("pop", "", .)
  
  return(pop_proj)
}

# 批量执行：预处理人口数据（对齐边界、分辨率、投影到参考流域栅格）
pop_list <- lapply(adf_dirs, process_pop, template = basin_ref_proj)
names(pop_list) <- sapply(pop_list, names)

# 核心计算函数 ------------------------------------------------------------
calculate_density <- function(station_id, year) {
  tryCatch({
    # 提取预处理后的流域掩膜和人口栅格
    basin_clean <- basins[[as.character(station_id)]]
    pop_layer   <- pop_list[[as.character(year)]]
    
    plot(basin_clean, pop_layer)
    
    # 检查是否为空
    if (is.null(basin_clean) || is.null(pop_layer)) return(NA_real_)
    if (all(is.na(values(basin_clean)))) return(NA_real_)
    
    if (!compareGeom(pop_layer, basin_clean)) {
      pop_layer <- crop(pop_layer, basin_clean)
      pop_layer <- resample(pop_layer, basin_clean, method = "near")
    }
    
    # 计算每个像元面积（单位 km²）
    cell_area <- terra::cellSize(pop_layer, unit = "km")
    masked_area <- mask(cell_area, basin_clean)
    
    # 人口密度 = 总人数 / 面积
    pop_masked <- mask(pop_layer, basin_clean)
    dens <- pop_masked / masked_area
    val  <- global(dens, "mean", na.rm = TRUE)[1,1]
    print(val)
    val <- if (nrow(val) == 0 || is.na(val$mean)) NA_real_ else val$mean
    
    # 内存清理
    rm(pop_layer, basin_clean, pop_masked, cell_area, masked_area, dens)
    gc()
    
    return(val)
  }, error = function(e) {
    message("Error for station ", station_id, " year ", year, ": ", e$message)
    return(NA_real_)
  })
}
# 主处理流程 ----------------------------------------------------------------
results <- stations %>%
  mutate(Sampling_year = as.numeric(format(Sampling_time, "%Y"))) %>%
  rowwise() %>%
  mutate(
    Pop_density = if (Sampling_year >= 1990 | Sampling_year <= 2020) {
      calculate_density(ID, Sampling_year)
    } else {
      NA_real_
    }
  ) %>%
  select(ID, Sampling_time, Pop_density)

# 结果输出 ------------------------------------------------------------------
write_csv(results, output_path)