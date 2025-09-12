library(terra)
library(readxl)
library(dplyr)
library(readr)

# 输入文件路径 ----------------------------------------------------------------
setwd("E:/POC research/data")  
stations_path <- "./3_POC records/clean_records.xlsx"
pop_base_dir <- "./2_Population"
basin_tiff_dir <- "./1_DEM_watershed/watershed_new_500m"
output_path <- "./2_inputData_watershed/Features/pop.csv"

# 读取站点数据 ----------------------------------------------------------------
stations <- read_excel(stations_path)

# 读取参考流域栅格（保留原始地理坐标系） --------------------------------------
basin_ref <- rast("./1_DEM_watershed/watershed_new_500m/1.tif")

# prepare basins（不投影） ---------------------------------------------------
basin_paths <- list.files(basin_tiff_dir, pattern = "\\.tif$", full.names = TRUE)
basin_ids   <- gsub("^([0-9]+)\\.tif$", "\\1", basename(basin_paths))

basins <- setNames(
  lapply(basin_paths, function(path) {
    rast(path)  # 不再 project
  }),
  basin_ids
)

# 定义目标年份范围 -----------------------------------------------------------
target_years <- 1990:2020
adf_dirs <- file.path(pop_base_dir, paste0("pop", target_years, "/tpop", target_years))
existing_years <- target_years[file.exists(adf_dirs)]
adf_dirs <- adf_dirs[file.exists(adf_dirs)]

if (length(adf_dirs) == 0) {
  stop("No valid ADF directories found in: ", pop_base_dir)
}

# 函数：处理人口栅格数据（保留原始坐标系） ------------------------------------
process_pop <- function(adf_dir, template) {
  pop_rast <- rast(file.path(adf_dir, "w001001.adf"))
  nodata_val <- -2147483648 
  pop_rast[pop_rast == nodata_val] <- NA

  if(crs(pop_rast) !=  crs(template)){
    pop_rast <- project(pop_rast, template)
  }
  
  pop_rast <- crop(pop_rast, template)
  pop_rast <- resample(pop_rast, template, method = "near")

  names(pop_rast) <- basename(dirname(adf_dir)) %>% gsub("pop", "", .)
  return(pop_rast)
}

# 批量处理人口数据 -----------------------------------------------------------
pop_list <- lapply(adf_dirs, process_pop, template = basin_ref)
names(pop_list) <- sapply(pop_list, names)

# 核心计算函数（不做投影，直接在 WGS84 计算面积） ------------------------------
calculate_density <- function(station_id, year) {
  tryCatch({
    basin_clean <- basins[[as.character(station_id)]]
    pop_layer   <- pop_list[[as.character(year)]]
    
    if (is.null(basin_clean) || is.null(pop_layer)) return(NA_real_)
    if (all(is.na(values(basin_clean)))) return(NA_real_)
    
    if(crs(pop_layer) !=  crs(basin_clean)){
      pop_layer <- project(pop_layer, basin_clean)
    }
    
    if (!is.null(intersect(ext(pop_layer), ext(basin_clean)))) {
      pop_layer <- crop(pop_layer, basin_clean)
      pop_layer <- resample(pop_layer, basin_clean, method = "near")
    }
    
    # 在地理坐标系下直接计算面积（terra 自动考虑球面）
    cell_area <- terra::cellSize(pop_layer, unit = "km")
    masked_area <- mask(cell_area, basin_clean)
    
    pop_masked <- mask(pop_layer, basin_clean)
    dens <- pop_masked / masked_area
    val  <- global(dens, "mean", na.rm = TRUE)[1, 1]
    
    val <- if (is.na(val)) NA_real_ else val
    return(val)
  }, error = function(e) {
    message("Error for station ", station_id, " year ", year, ": ", e$message)
    return(NA_real_)
  })
}

# 主处理流程 ----------------------------------------------------------------
results <- stations %>%
  filter(ID >= 864) %>%
  mutate(Sampling_year = as.numeric(format(Sampling_time, "%Y"))) %>%
  rowwise() %>%
  mutate(
    Pop_density = if (Sampling_year >= 1990 & Sampling_year <= 2020) {
      calculate_density(ID, Sampling_year)
    } else {
      NA_real_
    }
  ) %>%
  ungroup() %>%
  dplyr::select(ID, Sampling_time, Pop_density)

# 输出结果 ------------------------------------------------------------------
write_csv(results, output_path)
