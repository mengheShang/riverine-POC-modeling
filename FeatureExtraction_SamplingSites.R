# Packages ####
library(terra)
library(readxl)
library(dplyr)
library(purrr)
library(readr)
library(writexl)
library(tidyr)
library(ncdf4)
library(sp)
library(sf)

# Directory ####
setwd("E:/POC research/data")
paths <- list(
  stations = "3_POC records/clean_records.xlsx",
  basin_dir = "1_DEM_watershed/watershed_2",
  output      = "2_inputData_watershed/outlets_variables.xlsx",
  
  # 各变量数据路径
  dem      = "1_DEM_watershed/SRTM_China_new.tif",
  rh_file          = "2_Litter_Rh/Rh_RF_ensemble_mean_1982_2018.tif",
  litter_file      = "2_Litter_Rh/litter_360_720.tif",
  soil = list(
    clay            = "2_HWSD2/HWSD2_D1/HWSD2_D1__CLAY.tif",
    sand            = "2_HWSD2/HWSD2_D1/HWSD2_D1__SAND.tif",
    silt            = "2_HWSD2/HWSD2_D1/HWSD2_D1__SILT.tif",
    organic_carbon  = "2_HWSD2/HWSD2_D1/HWSD2_D1__ORG_CARBON.tif",
    root_depth      = "2_HWSD2/HWSD2_D1/HWSD2_D1__ROOT_DEPTH.tif",
    ph              = "2_HWSD2/HWSD2_D1/HWSD2_D1__PH_WATER.tif"
  ),
  rusle_c          = "2_GloSEM_25km/Data_25km/RUSLE_CFactor_yr2012_v1.1_25km.tif",
  rusle_k          = "2_GloSEM_25km/Data_25km/RUSLE_KFactor_v1.1_25km.tif",
  rusle_ls         = "2_GloSEM_25km/Data_25km/RUSLE_LSFactor_v1.1_25km.tif",
  rusle_r          = "2_GloSEM_25km/Data_25km/RUSLE_RFactor_v1.1_25km.tif",
  soilloss_2001    = "2_GloSEM_25km/Data_25km/RUSLE_SoilLoss_v1.1_yr2001_25km.tif",
  soilloss_2012    = "2_GloSEM_25km/Data_25km/RUSLE_SoilLoss_v1.1_yr2012_25km.tif",
  reservoirs_dams  = "2_Reservoirs and Dams/China_Dams.shp",
  
  gdp_dir          = "2_GDP",
  precipitation_nc = "2_pco2Data/ERA5-Land-monthly_China_1950_2022.nc",
  temperature_nc   = "2_pco2Data/ERA5-Land-monthly_China_1950_2022.nc",
  npp_dir          = "2_pco2Data/npp",
  landcover_dir    = "2_pco2Data/land_covernew"
)

# General Functions ####
align_rasters <- function(target, reference) {
  # 将栅格校正到与reference一致的投影、分辨率、范围
  if (!ext(target) == ext(reference)) {
    target <- project(target, crs(reference)) %>%
      resample(reference, method = "near") %>%
      crop(ext(reference))
  }
  return(target)
}

safe_read <- function(path, fun = rast) {
  # 安全读取文件的函数，若不存在则返回NULL
  if (file.exists(path)) return(fun(path))
  warning(paste("文件不存在:", path))
  return(NULL)
}

# 读取入海口及时间数据 
cat("\n==== 正在加载入海口与时间数据 ====\n")

# 创建包含所有入海口（ID, Long, Lat）和时间组合的时序表
outlets_data <- read_excel(paths$stations) %>%
  mutate(
    Sampling_time  = as.Date(Sampling_time),
    Sampling_year  = as.numeric(format(Sampling_time, "%Y")),
    Sampling_month = as.numeric(format(Sampling_time, "%m"))
  )

# 加载基准流域栅格
basin_ref <- safe_read(file.path(paths$basin_dir, "_1.tif"))

# 读取DEM并建立参考栅格
dem        <- safe_read(paths$dem) %>% subst(-9999, NA)

# 1.Constant Factors ####

## A. Slope ####
cat("\n==== 计算坡度 Slope ====\n")

# 计算地形坡度
slope <- terrain(dem, v = "slope", unit = "degrees")

# 定义坡度提取函数（考虑周围像元无效值时向外扩展）
extract_slope <- function(point) {
  cell <- cellFromXY(dem, crds(point))
  if (is.na(cell)) return(NA_real_)
  
  neighbors <- adjacent(dem, cells = cell, directions = 8, pairs = FALSE)
  
  # 若周围8个像元值全有效，则直接提取
  if (all(!is.na(values(dem)[neighbors]))) {
    return(extract(slope, point)[, 2])
  } else {
    # 否则向外扩展搜索
    search_radius <- 1
    max_radius    <- 10
    
    while (search_radius <= max_radius) {
      nearby_cells <- tryCatch({
        adjacent(dem, cells = cell, 
                 directions = 8 * search_radius, 
                 pairs = FALSE, 
                 include = TRUE)
      }, error = function(e) NA)
      
      if (!all(is.na(nearby_cells))) {
        valid_cells <- nearby_cells[
          apply(adjacent(dem, cells = nearby_cells, directions = 8, pairs = FALSE), 
                1, function(x) all(!is.na(values(dem)[x])))
        ]
        
        if (length(valid_cells) > 0) {
          # 选取第一个(最近的一个)
          return(extract(slope, xyFromCell(dem, valid_cells[1]))[, 2])
        }
      }
      search_radius <- search_radius + 1
    }
    return(NA_real_)
  }
}

# 提取每个入海口位置的 Slope（不与时间做笛卡尔积）
unique_outlets <- outlets_data %>%
  distinct(ID, Longitude, Latitude)

outlets_sp <- vect(
  unique_outlets,
  geom = c("Longitude", "Latitude"),
  crs  = crs(dem)
)

unique_slopes <- unique_outlets %>%
  mutate(
    Slope = map_dbl(1:nrow(outlets_sp), ~ extract_slope(outlets_sp[.x, ]))
  ) %>%
  select(ID, Slope)

## B. upSlope ####
cat("\n==== 计算上游平均坡度 upSlope ====\n")

calculate_upstream_slope <- function(outlets_id) {
  cat(sprintf("Processing outlet ID: %s\n", outlets_id))
  basin_file <- file.path(paths$basin_dir, paste0(outlets_id, ".tif"))
  if (!file.exists(basin_file)) {
    warning(paste("流域文件不存在:", basin_file))
    return(NA_real_)
  }
  
  basin <- safe_read(basin_file) %>%
    project(crs(slope)) %>%
    resample(slope) %>%
    subst(255, NA)
  
  slope_masked <- mask(slope, basin)
  global(slope_masked, "mean", na.rm = TRUE)$mean
}

unique_upslopes <- unique_outlets %>%
  mutate(
    upSlope = map_dbl(ID, calculate_upstream_slope)
  ) %>%
  select(ID, upSlope)

rm(dem,slope, extract_slope,calculate_upstream_slope)
gc()
## C. WatershedArea ####
cat("\n==== 计算流域面积 WatershedArea ====\n")

calculate_watershed_area <- function(basin_file) {
  basin <- safe_read(basin_file)
  if (is.null(basin)) return(NA_real_)
  
  basin %>% 
    subst(255, NA) %>% 
    expanse(unit = "km") %>% 
    sum()
}

unique_areas <- unique_outlets %>%
  mutate(
    WatershedArea = map_dbl(ID, ~ {
      basin_file <- file.path(paths$basin_dir, paste0(.x, ".tif"))
      cat(sprintf("计算入海口 %s 的流域面积\n", .x))
      calculate_watershed_area(basin_file)
    })
  ) %>%
  select(ID, WatershedArea)

rm(calculate_watershed_area, basin_file)
gc()

## D. SoilSource_HWSD2 ####
cat("\n==== 计算土壤数据 (HWSD2) ====\n")

# 先将各土壤变量对齐到 basin_ref
soil_rasters <- map(paths$soil, safe_read) %>%
  map(~ align_rasters(.x, basin_ref) %>% subst(-9999, NA))

calculate_soil_vars <- function(outlets_id) {
  basin_file <- file.path(paths$basin_dir, paste0(outlets_id, ".tif"))
  if (!file.exists(basin_file)) {
    return(tibble(
      ID              = outlets_id,
      clay            = NA_real_,
      sand            = NA_real_,
      silt            = NA_real_,
      organic_carbon  = NA_real_,
      root_depth      = NA_real_,
      ph              = NA_real_
    ))
  }
  
  basin <- safe_read(basin_file) %>% subst(255, NA)
  
  # 对每个soil_raster进行掩膜并求平均
  soil_vars <- imap_dfc(soil_rasters, ~{
    masked <- mask(.x, basin)
    val    <- global(masked, "mean", na.rm = TRUE)$mean
    set_names(list(val), .y)
  })
  
  bind_cols(ID = outlets_id, soil_vars)
}

unique_soils <- unique_outlets %>%
  mutate(soil_data = map(ID, calculate_soil_vars)) %>%
  tidyr::unnest(soil_data) %>%
  select(ID, clay, sand, silt, organic_carbon, root_depth, ph)

rm(soil_rasters, calculate_soil_vars)
gc()

## E. SoilSource_Litter ####
cat("\n==== 计算 Rh (土壤呼吸) 与 Litter (枯落物) ====\n")

# 1) 读入多波段，再对时间维度做平均
rh_rast      <- safe_read(paths$rh_file)
litter_rast  <- safe_read(paths$litter_file)

# 对所有 band 求平均
rh_mean     <- mean(rh_rast, na.rm = TRUE)
litter_mean <- mean(litter_rast, na.rm = TRUE)

# 对每个 ID 进行掩膜并算平均值
calculate_rh_litter <- function(outlets_id) {
  basin_file <- file.path(paths$basin_dir, paste0(outlets_id, ".tif"))
  if (!file.exists(basin_file)) {
    warning(paste("流域文件不存在:", basin_file))
    return(tibble(ID = outlets_id, Rh = NA_real_, Litter = NA_real_))
  }
  
  # 读取并掩膜
  basin <- safe_read(basin_file)
  basin[basin == 255] <- NA
  
  # 对齐(若投影/分辨率不同，可使用 align_rasters)
  rh_aligned     <- align_rasters(rh_mean, basin)
  litter_aligned <- align_rasters(litter_mean, basin)
  
  rh_masked     <- mask(rh_aligned, basin)
  litter_masked <- mask(litter_aligned, basin)
  
  tibble(
    ID     = outlets_id,
    Rh     = global(rh_masked, "mean", na.rm = TRUE)$mean,
    Litter = global(litter_masked, "mean", na.rm = TRUE)$mean
  )
}

unique_rh_litter <- unique_outlets %>%
  mutate(rh_litter_data = map(ID, calculate_rh_litter)) %>%
  unnest(rh_litter_data) %>%
  select(ID, Rh, Litter)

rm(rh_rast, litter_rast, rh_mean, litter_mean, calculate_rh_litter)
gc()

## F. RUSLE Factor ####

cat("\n==== 计算 RUSLE C/K/LS/R 因子 (静态) ====\n")

# 读取 RUSLE C/K/LS/R 栅格，并对齐到 basin_ref
rusle_c_rast  <- safe_read(paths$rusle_c)
rusle_k_rast  <- safe_read(paths$rusle_k)
rusle_ls_rast <- safe_read(paths$rusle_ls)
rusle_r_rast  <- safe_read(paths$rusle_r)

rusle_c_25km  <- align_rasters(rusle_c_rast,  basin_ref)
rusle_k_25km  <- align_rasters(rusle_k_rast,  basin_ref)
rusle_ls_25km <- align_rasters(rusle_ls_rast, basin_ref)
rusle_r_25km  <- align_rasters(rusle_r_rast,  basin_ref)

# 定义函数：对每个ID的流域掩膜后，计算C/K/LS/R四个因子的流域平均值
calculate_rusle_factors <- function(outlet_id) {
  basin_file <- file.path(paths$basin_dir, paste0(outlet_id, ".tif"))
  if (!file.exists(basin_file)) {
    warning(sprintf("流域文件不存在: %s", basin_file))
    return(tibble(
      ID        = outlet_id,
      C_factor  = NA_real_,
      K_factor  = NA_real_,
      LS_factor = NA_real_,
      R_factor  = NA_real_
    ))
  }
  basin <- safe_read(basin_file) %>% subst(255, NA)
  
  c_masked  <- mask(rusle_c_25km,  basin)
  k_masked  <- mask(rusle_k_25km,  basin)
  ls_masked <- mask(rusle_ls_25km, basin)
  r_masked  <- mask(rusle_r_25km,  basin)
  
  tibble(
    ID        = outlet_id,
    C_factor  = global(c_masked,  "mean", na.rm=TRUE)$mean,
    K_factor  = global(k_masked,  "mean", na.rm=TRUE)$mean,
    LS_factor = global(ls_masked, "mean", na.rm=TRUE)$mean,
    R_factor  = global(r_masked,  "mean", na.rm=TRUE)$mean
  )
}

# 对每个 ID 计算 RUSLE 因子
unique_rusle <- unique_outlets %>%
  mutate(rusle_factors = map(ID, calculate_rusle_factors)) %>%
  unnest(rusle_factors) %>%
  select(ID, C_factor, K_factor, LS_factor, R_factor)

rm(
  rusle_c_rast, rusle_k_rast, rusle_ls_rast, rusle_r_rast,
  rusle_c_25km, rusle_k_25km, rusle_ls_25km, rusle_r_25km,
  calculate_rusle_factors
)
gc()

## G. ReservoirsAndDams (Static) ####
cat("\n==== 提取水库/大坝 静态特征 (Summary & Nearest Dam) ====\n")

# 读取 & 预处理
reservoirs_dams <- st_read(paths$reservoirs_dams)
replace_neg99_with_na_sf <- function(sf_obj) {
  attrs <- st_drop_geometry(sf_obj)
  attrs[] <- lapply(attrs, function(x) {
    if (is.numeric(x)) x[x == -99] <- NA
    x
  })
  st_as_sf(cbind(attrs, st_geometry(sf_obj)))
}
reservoirs_dams <- replace_neg99_with_na_sf(reservoirs_dams)

# 汇总特征
calculate_summary_statistics <- function(dams_sub) {
  dams_sub %>%
    st_drop_geometry() %>%
    summarize(
      Num_Dams               = n(),
      Avg_DamHeight_m        = mean(DAM_HGT_M, na.rm = TRUE),
      Max_DamHeight_m        = max(DAM_HGT_M, na.rm = TRUE),
      Avg_Capacity_Mm3       = mean(CAP_MCM, na.rm = TRUE),
      Sum_Capacity_Mm3       = sum(CAP_MCM, na.rm = TRUE),
      # ... 其余字段 ...
      Avg_DOR_pc             = mean(DOR_PC, na.rm = TRUE)
    )
}

# 最近大坝
calculate_nearest_dam_features <- function(station_point, dams_sub) {
  distances   <- st_distance(station_point, dams_sub)
  nearest_dam <- dams_sub[which.min(distances), ]
  
  nearest_dam %>%
    st_drop_geometry() %>%
    dplyr::select(
      Nearest_Dam_Height_m       = DAM_HGT_M,
      Nearest_Dam_Capacity_Mm3   = CAP_REP,
      # ... 其余字段 ...
      Nearest_DOR_pc             = DOR_PC
    ) %>%
    mutate(
      Distance_to_Nearest_Dam_km = as.numeric(min(distances))/1000
    )
}

# 对每个 ID 仅做一次：找出流域内所有大坝并计算 Summary + Nearest
process_reservoirs_static <- function() {
  # 如果只需要 ID/Longitude/Latitude 就用 unique_outlets
  results_static <- data.frame()
  
  for (i in seq_len(nrow(unique_outlets))) {
    outlet_id <- unique_outlets$ID[i]
    lon       <- unique_outlets$Longitude[i]
    lat       <- unique_outlets$Latitude[i]
    
    basin_file <- file.path(paths$basin_dir, paste0(outlet_id, ".tif"))
    if (!file.exists(basin_file)) {
      warning(sprintf("No basin TIF: ID=%s", outlet_id))
      next
    }
    basin_tiff <- rast(basin_file)
    crs(basin_tiff) <- crs(reservoirs_dams)  # 保持原逻辑
    
    # 转成 sf
    basin_geom_sf <- st_as_sf(as.polygons(basin_tiff, dissolve = TRUE))
    dams_in_basin <- reservoirs_dams[ st_intersects(reservoirs_dams, basin_geom_sf, sparse=FALSE), ]
    
    if (nrow(dams_in_basin) > 0) {
      summary_stats <- calculate_summary_statistics(dams_in_basin)
      
      # 最近大坝
      station_pt <- st_sfc(st_point(c(lon, lat)), crs=st_crs(reservoirs_dams))
      nearest_dam_features <- calculate_nearest_dam_features(station_pt, dams_in_basin)
    } else {
      # 无大坝
      summary_stats <- data.frame(Num_Dams=0, Avg_DamHeight_m=NA, Max_DamHeight_m=NA,
                                  Avg_Capacity_Mm3=NA, Sum_Capacity_Mm3=NA,
                                  Avg_DOR_pc=NA)
      nearest_dam_features <- data.frame(
        Nearest_Dam_Height_m=NA, Nearest_Dam_Capacity_Mm3=NA,
        Nearest_DOR_pc=NA, Distance_to_Nearest_Dam_km=NA
      )
    }
    
    # 合并
    row_out <- data.frame(ID=outlet_id, Lon=lon, Lat=lat)
    row_out <- cbind(row_out, summary_stats, nearest_dam_features)
    results_static <- bind_rows(results_static, row_out)
  }
  return(results_static)
}

unique_reservoirs_static <- process_reservoirs_static()

rm(
  reservoirs_dams,
  replace_neg99_with_na_sf,
  calculate_summary_statistics,
  calculate_nearest_dam_features,
  process_reservoirs_static
)
gc()

# combine constant results ####
static_data <- list(
  unique_slopes,
  unique_upslopes,
  unique_areas,
  unique_soils,
  unique_rh_litter,
  unique_rusle,
  unique_reservoirs_static  
) %>%
  reduce(full_join, by = "ID")

# 2.Global Change Factors ####

## (1) Precipitation ####
cat("\n==== 准备并计算降水 Precipitation ====\n")

prepare_precipitation_data <- function(nc_path) {
  nc_data <- nc_open(nc_path)
  precipitation_raw <- ncvar_get(nc_data, "tp")
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  time <- ncvar_get(nc_data, "time")
  fill_value <- ncatt_get(nc_data, "tp", "_FillValue")$value
  nc_close(nc_data)
  
  # 替换填充值，转为 mm
  precipitation <- precipitation_raw
  precipitation[precipitation_raw == fill_value] <- NA
  precipitation <- precipitation * 1000
  
  # 时间处理
  time_dates <- as.POSIXct(time * 3600, origin = "1900-01-01", tz = "GMT")
  
  # 计算该月天数并乘以日均降水
  days_in_month <- function(date) {
    y <- format(date, "%Y") 
    m <- format(date, "%m") 
    # 下个月1号
    next_month <- as.Date(paste0(y, "-", as.integer(m) %% 12 + 1, "-01"))
    # 当月天数
    as.integer(format(next_month - 1, "%d"))
  }
  
  days_in_months <- sapply(time_dates, days_in_month)
  precipitation <- sweep(precipitation, 3, days_in_months, `*`)
  
  # 调整维度顺序 (lon, lat, time) -> (lat, lon, time)，然后再改为 (lon, lat, time) 需要根据具体数据检查
  # 下面假设原始数据是 (lon, lat, time)，如有需要可根据实际情况调换
  precipitation <- aperm(precipitation, c(2, 1, 3))
  
  list(
    data = precipitation,
    lon  = lon,
    lat  = lat,
    time = time_dates
  )
}

precip_data <- prepare_precipitation_data(paths$precipitation_nc)

calculate_precipitation <- function(outlets_id, sampling_time) {
  year_month  <- format(as.Date(sampling_time), "%Y-%m")
  time_index  <- which(format(precip_data$time, "%Y-%m") == year_month)
  
  if (length(time_index) == 0) {
    warning(paste("No precipitation data for", year_month))
    return(NA_real_)
  }
  
  basin_file <- file.path(paths$basin_dir, paste0(outlets_id, ".tif"))
  if (!file.exists(basin_file)) {
    warning(paste("流域文件不存在:", basin_file))
    return(NA_real_)
  }
  
  # 构建栅格
  precip_rast <- rast(
    precip_data$data[,,time_index],
    extent = ext(range(precip_data$lon), range(precip_data$lat)),
    crs    = "+proj=longlat +datum=WGS84"
  )
  
  basin <- safe_read(basin_file) %>% 
    project(crs(precip_rast)) %>%
    subst(255, NA)
  
  precip_masked <- mask(
    resample(precip_rast, basin, method = "near"),
    basin
  )
  global(precip_masked, "mean", na.rm = TRUE)$mean
}

precip_results <- outlets_data %>%
  select(ID, Sampling_time) %>%
  mutate(
    Precipitation = map2_dbl(ID, Sampling_time, ~ {
      cat(sprintf("计算降水: ID=%s, Date=%s\n", .x, format(as.Date(.y), "%Y-%m")))
      calculate_precipitation(.x, .y)
    })
  )

rm(precip_data, prepare_precipitation_data, calculate_precipitation)
gc()

## (2) Temperature ####
cat("\n==== 准备并计算温度 Temperature ====\n")

prepare_temperature_data <- function(nc_path) {
  nc_data <- nc_open(nc_path)
  temperature_raw <- ncvar_get(nc_data, "t2m")
  lon <- ncvar_get(nc_data, "longitude")
  lat <- ncvar_get(nc_data, "latitude")
  time <- ncvar_get(nc_data, "time")
  fill_value <- ncatt_get(nc_data, "t2m", "_FillValue")$value
  nc_close(nc_data)
  
  temperature <- temperature_raw
  temperature[temperature_raw == fill_value] <- NA
  # 转为摄氏度
  temperature <- temperature - 273.15
  
  time_dates <- as.POSIXct(time * 3600, origin = "1900-01-01", tz = "GMT")
  
  # 调整维度顺序 (与降水类似)
  temperature <- aperm(temperature, c(2, 1, 3))
  
  list(
    data = temperature,
    lon  = lon,
    lat  = lat,
    time = time_dates
  )
}

temp_data <- prepare_temperature_data(paths$temperature_nc)

calculate_temperature <- function(outlets_id, sampling_time) {
  year_month <- format(as.Date(sampling_time), "%Y-%m")
  time_index <- which(format(temp_data$time, "%Y-%m") == year_month)
  
  if (length(time_index) == 0) {
    warning(paste("No temperature data for", year_month))
    return(NA_real_)
  }
  
  basin_file <- file.path(paths$basin_dir, paste0(outlets_id, ".tif"))
  if (!file.exists(basin_file)) {
    warning(paste("流域文件不存在:", basin_file))
    return(NA_real_)
  }
  
  temp_rast <- rast(
    temp_data$data[,,time_index],
    extent = ext(range(temp_data$lon), range(temp_data$lat)),
    crs    = "+proj=longlat +datum=WGS84"
  )
  
  basin <- safe_read(basin_file) %>%
    project(crs(temp_rast)) %>%
    subst(255, NA)
  
  temp_masked <- mask(
    resample(temp_rast, basin, method = "near"),
    basin
  )
  global(temp_masked, "mean", na.rm = TRUE)$mean
}

temp_results <- outlets_data %>%
  select(ID, Sampling_time) %>%
  mutate(
    Temperature = map2_dbl(ID, Sampling_time, ~ {
      cat(sprintf("计算温度: ID=%s, Date=%s\n", .x, format(as.Date(.y), "%Y-%m")))
      calculate_temperature(.x, .y)
    })
  )

rm(temp_data, prepare_temperature_data, calculate_temperature)
gc()

## (3) PopulationDensity ####

## (4) GDP ####
cat("\n==== 读取并计算GDP ====\n")

tiff_files <- list.files(paths$gdp_dir, pattern = "\\.tif$", full.names = TRUE)
years      <- 1990:2015

gdp_list <- map(tiff_files, ~{
  rast(.x) %>%
    project(crs(basin_ref)) %>%
    crop(ext(basin_ref)) %>%
    resample(basin_ref, method = "near")
})

calculate_gdp <- function(outlets_id, sampling_time) {
  sampling_year <- as.numeric(format(as.Date(sampling_time), "%Y"))
  cat(sprintf("计算GDP: ID=%s, Year=%d\n", outlets_id, sampling_year))
  
  # 超过2015则没有数据
  if (sampling_year > 2015) return(NA_real_)
  
  basin_file <- file.path(paths$basin_dir, paste0(outlets_id, ".tif"))
  if (!file.exists(basin_file)) {
    warning(paste("流域文件不存在:", basin_file))
    return(NA_real_)
  }
  
  year_index <- which(years == sampling_year)
  if (length(year_index) == 0) return(NA_real_) # 无对应年份
  
  basin <- safe_read(basin_file) %>% subst(255, NA)
  
  masked_gdp <- mask(gdp_list[[year_index]], basin)
  global(masked_gdp, "sum", na.rm = TRUE)$sum
}

gdp_results <- outlets_data %>%
  select(ID, Sampling_time) %>%
  mutate(
    GDP = map2_dbl(ID, Sampling_time, calculate_gdp)
  )

rm(tiff_files, years, gdp_list, calculate_gdp)
gc()

## (5) NPP ####
cat("\n==== 读取并计算 NPP 动态变量 ====\n")

# 获取并读取所有 NPP TIF 文件
npp_tiff_files <- list.files(
  path = paths$npp_dir, 
  pattern = "*_new.tif$", 
  full.names = TRUE
)

# 假设 NPP 数据年份为 2001-2022 (与文件数相匹配)
# years_npp <- seq(2001, 2022)
npp_years <- seq(2001, 2001 + length(npp_tiff_files) - 1)

cat(sprintf("NPP 数据年份范围：%d - %d\n", min(npp_years), max(npp_years)))

# 读取所有 TIF 并投影对齐到 basin_ref
npp_list <- lapply(seq_along(npp_tiff_files), function(i) {
  r <- rast(npp_tiff_files[[i]])
  cat(sprintf("Processing NPP raster %d/%d...\n", i, length(npp_tiff_files)))
  r_aligned <- align_rasters(r, basin_ref)
  cat("Done with raster", i, "\n")
  return(r_aligned)
})

# 定义计算函数：给定 (ID, Sampling_time) 计算 NPP_1back, 2back, 3back, sum_NPP3
calculate_npp_for_station <- function(outlet_id, sampling_time) {
  sampling_year <- as.numeric(format(as.Date(sampling_time), "%Y"))
  cat(sprintf("Calculating NPP for ID=%s, Year=%d\n", outlet_id, sampling_year))
  
  # 流域文件
  basin_file <- file.path(paths$basin_dir, paste0(outlet_id, ".tif"))
  if (!file.exists(basin_file)) {
    warning(sprintf("No basin file for ID=%s", outlet_id))
    return(tibble(
      ID = outlet_id,
      Sampling_time = sampling_time,
      NPP_1back = NA_real_, 
      NPP_2back = NA_real_,
      NPP_3back = NA_real_,
      sum_NPP3  = NA_real_
    ))
  }
  
  # 若年份不在 2001~(2001+length-1) 之列，则无数据
  if (! (sampling_year %in% npp_years)) {
    return(tibble(
      ID = outlet_id,
      Sampling_time = sampling_time,
      NPP_1back = NA_real_, 
      NPP_2back = NA_real_,
      NPP_3back = NA_real_,
      sum_NPP3  = NA_real_
    ))
  }
  
  # 读取流域栅格并掩膜
  basin_tiff <- safe_read(basin_file, rast) %>% subst(255, NA)
  
  # 找到年份索引
  idx_y  <- which(npp_years == sampling_year)
  idx_1  <- which(npp_years == (sampling_year - 1))
  idx_2  <- which(npp_years == (sampling_year - 2))
  idx_3  <- which(npp_years == (sampling_year - 3))
  
  # 依次读取并计算
  get_mean_npp <- function(idx) {
    if (length(idx) == 0) return(NA_real_)
    m <- mask(npp_list[[idx]], basin_tiff)
    global(m, "mean", na.rm=TRUE)$mean
  }
  
  npp_1back <- get_mean_npp(idx_1)
  npp_2back <- get_mean_npp(idx_2)
  npp_3back <- get_mean_npp(idx_3)
  
  tibble(
    ID         = outlet_id,
    Sampling_time = sampling_time,
    NPP_1back  = npp_1back,
    NPP_2back  = npp_2back,
    NPP_3back  = npp_3back,
    sum_NPP3   = sum(c(npp_1back, npp_2back, npp_3back), na.rm=TRUE)
  )
}

# 对 outlets_data 每行计算
npp_results <- outlets_data %>%
  select(ID, Sampling_time) %>%
  mutate(
    NPP_values = map2(ID, Sampling_time, calculate_npp_for_station)
  ) %>%
  unnest(NPP_values)

rm(npp_list, npp_years, npp_tiff_files, calculate_npp_for_station)
gc()

## (6) Landcover ####
cat("\n==== 读取并计算 Landcover ====\n")

# 与GDP等类似：先预处理(读入所有年份栅格)、再按站点+时间提取

# -- A) 预处理：读取所有 TIF 文件并建立与年份对应关系
prepare_landcover_data <- function(landcover_dir) {
  # 获取所有landcover tiff文件列表
  tiff_files <- list.files(path = landcover_dir, pattern = "_1.tif$", full.names = TRUE)
  file_1985 <- grep("1985_1.tif$", tiff_files, value = TRUE)
  if (length(file_1985) > 0) {
    # 将1985年的TIF文件复制4次，生成1985-1989五年的数据
    tiff_files <- c(rep(file_1985, 5), tiff_files[-grep("1985_1.tif$", tiff_files)])
  }
  
  years_lc <- 1985:(1985 + length(tiff_files) - 1)
  
  # 读取所有landcover TIF文件并统一投影、裁剪、重采样到 basin_ref
  landcover_list <- lapply(seq_along(tiff_files), function(i) {
    cat(sprintf("Reading Landcover raster %d/%d...\n", i, length(tiff_files)))
    r <- rast(tiff_files[[i]])
    # 投影、裁剪、重采样
    r <- project(r, crs(basin_ref))
    r <- crop(r, basin_ref)
    r <- resample(r, basin_ref, method = "near")
    return(r)
  })
  
  if (length(landcover_list) != length(years_lc)) {
    stop("landcover_list 与 years_lc 长度不匹配，请检查。")
  }
  
  list(
    data   = landcover_list,
    years  = years_lc
  )
}

landcover_data <- prepare_landcover_data(paths$landcover_dir)

# -- B) 计算某一站点某年 Landcover 覆盖率
calculate_landcover <- function(outlets_id, sampling_time) {
  sampling_year <- as.numeric(format(as.Date(sampling_time), "%Y"))
  cat(sprintf("计算 Landcover: ID=%s, Year=%d\n", outlets_id, sampling_year))
  
  # 在 years_lc 中找对应
  year_index <- which(landcover_data$years == sampling_year)
  if (length(year_index) == 0) {
    # 如果没有对应年份，则返回 NA
    return(tibble(
      Cropland  = NA_real_,
      Forest    = NA_real_,
      Shrub     = NA_real_,
      Grassland = NA_real_,
      Water     = NA_real_,
      Snow_Ice  = NA_real_,
      Barren    = NA_real_,
      Urban     = NA_real_,
      Wetland   = NA_real_
    ))
  }
  
  # 读取流域栅格
  basin_file <- file.path(paths$basin_dir, paste0(outlets_id, ".tif"))
  if (!file.exists(basin_file)) {
    warning(paste("流域文件不存在:", basin_file))
    return(tibble(
      Cropland  = NA_real_,
      Forest    = NA_real_,
      Shrub     = NA_real_,
      Grassland = NA_real_,
      Water     = NA_real_,
      Snow_Ice  = NA_real_,
      Barren    = NA_real_,
      Urban     = NA_real_,
      Wetland   = NA_real_
    ))
  }
  basin <- safe_read(basin_file) %>% subst(255, NA)
  
  # 掩膜
  r_lc <- mask(
    resample(landcover_data$data[[year_index]], basin, method="near"),
    basin
  )
  
  # 计算各类占比：1=Crop,2=Forest,3=Shrub,4=Grassland,5=Water,6=SnowIce,7=Barren,8=Urban,9=Wetland
  
  tibble(
    Cropland   = global(r_lc == 1, "mean", na.rm = TRUE)$mean,
    Forest     = global(r_lc == 2, "mean", na.rm = TRUE)$mean,
    Shrub      = global(r_lc == 3, "mean", na.rm = TRUE)$mean,
    Grassland  = global(r_lc == 4, "mean", na.rm = TRUE)$mean,
    Water      = global(r_lc == 5, "mean", na.rm = TRUE)$mean,
    Snow_Ice   = global(r_lc == 6, "mean", na.rm = TRUE)$mean,
    Barren     = global(r_lc == 7, "mean", na.rm = TRUE)$mean,
    Urban      = global(r_lc == 8, "mean", na.rm = TRUE)$mean,
    Wetland    = global(r_lc == 9, "mean", na.rm = TRUE)$mean
  )
}

# -- C) 生成结果表
landcover_results <- outlets_data %>%
  select(ID, Sampling_time) %>%
  mutate(
    landcover = map2(ID, Sampling_time, calculate_landcover)
  ) %>%
  unnest(landcover) 

rm(landcover_data, prepare_landcover_data, calculate_landcover)
gc()

## (7) SoilLoss ####
cat("\n==== 基于年份动态计算 SoilLoss (2001/2012) ====\n")

# 读取 2001/2012 SoilLoss 栅格，并对齐
soilloss_2001_rast <- safe_read(paths$soilloss_2001)
soilloss_2012_rast <- safe_read(paths$soilloss_2012)
soilloss_2001_25km <- align_rasters(soilloss_2001_rast, basin_ref)
soilloss_2012_25km <- align_rasters(soilloss_2012_rast, basin_ref)

# 根据采样年份: <2012 用2001, 否则用2012
calculate_soilloss <- function(outlet_id, sampling_time) {
  sampling_year <- as.numeric(format(as.Date(sampling_time), "%Y"))
  cat(sprintf("计算 SoilLoss: ID=%s, Year=%d\n", outlet_id, sampling_year))
  
  basin_file <- file.path(paths$basin_dir, paste0(outlet_id, ".tif"))
  if (!file.exists(basin_file)) {
    warning(sprintf("流域文件不存在: %s", basin_file))
    return(NA_real_)
  }
  basin <- safe_read(basin_file) %>% subst(255, NA)
  
  if (sampling_year < 2012) {
    masked_2001 <- mask(soilloss_2001_25km, basin)
    global(masked_2001, "mean", na.rm=TRUE)$mean
  } else {
    masked_2012 <- mask(soilloss_2012_25km, basin)
    global(masked_2012, "mean", na.rm=TRUE)$mean
  }
}

# 构造 SoilLoss 动态结果表
soilloss_results <- outlets_data %>%
  select(ID, Sampling_time, Sampling_year) %>%
  mutate(
    SoilLoss = map2_dbl(ID, Sampling_time, calculate_soilloss)
  )

rm(
  soilloss_2001_rast, soilloss_2012_rast,
  soilloss_2001_25km, soilloss_2012_25km,
  calculate_soilloss
)
gc()

## (8) ReservoirDams_Temporal ####
cat("\n==== 根据采样年份动态计算大坝时序特征 ====\n")

# 只计算“近10/20/30/40/50年大坝数”等依赖 sampling_year 的指标
calculate_temporal_features <- function(dams_sub, sampling_year) {
  dams_sub %>%
    st_drop_geometry() %>%
    summarize(
      Num_Dams_Recent10Years = sum(YEAR >= (sampling_year - 10), na.rm = TRUE),
      Num_Dams_Recent20Years = sum(YEAR >= (sampling_year - 20), na.rm = TRUE),
      Num_Dams_Recent30Years = sum(YEAR >= (sampling_year - 30), na.rm = TRUE),
      Num_Dams_Recent40Years = sum(YEAR >= (sampling_year - 40), na.rm = TRUE),
      Num_Dams_Recent50Years = sum(YEAR >= (sampling_year - 50), na.rm = TRUE)
    )
}

# 建立一个函数，对 outlets_data 里的每一行 (ID, Sampling_time) 做处理
process_reservoirs_temporal <- function() {
  all_rows <- data.frame()
  
  for (j in seq_len(nrow(outlets_data))) {
    outlet_id     <- outlets_data$ID[j]
    sampling_year <- outlets_data$Sampling_year[j]
    
    basin_file <- file.path(paths$basin_dir, paste0(outlet_id, ".tif"))
    if (!file.exists(basin_file)) {
      # 无流域文件
      row_fail <- data.frame(
        ID=outlet_id, Sampling_time=outlets_data$Sampling_time[j],
        Num_Dams_Recent10Years=NA, Num_Dams_Recent20Years=NA,
        Num_Dams_Recent30Years=NA, Num_Dams_Recent40Years=NA,
        Num_Dams_Recent50Years=NA
      )
      all_rows <- bind_rows(all_rows, row_fail)
      next
    }
    basin_tiff <- rast(basin_file)
    crs(basin_tiff) <- crs(reservoirs_dams)
    
    basin_geom_sf <- st_as_sf(as.polygons(basin_tiff, dissolve=TRUE))
    dams_in_basin <- reservoirs_dams[st_intersects(reservoirs_dams, basin_geom_sf, sparse=FALSE), ]
    
    if (nrow(dams_in_basin) > 0) {
      tf <- calculate_temporal_features(dams_in_basin, sampling_year)
    } else {
      tf <- data.frame(Num_Dams_Recent10Years=0, Num_Dams_Recent20Years=0,
                       Num_Dams_Recent30Years=0, Num_Dams_Recent40Years=0,
                       Num_Dams_Recent50Years=0)
    }
    
    # 合并
    row_out <- cbind(
      data.frame(ID=outlet_id, Sampling_time=outlets_data$Sampling_time[j]),
      tf
    )
    all_rows <- bind_rows(all_rows, row_out)
  }
  
  return(all_rows)
}

resdam_temporal_results <- process_reservoirs_temporal()

rm(
  reservoirs_dams,
  process_reservoirs_temporal
)
gc()

# Combine Results ####

cat("\n==== 合并动态与静态数据 ====\n")

# 1) 将动态部分合并到一起
dynamic_data <- list(
  precip_results,
  temp_results,
  
  gdp_results,
  npp_results,
  landcover_results,
  soilloss_results,
  resdam_temporal_results
) %>%
  reduce(full_join, by = c("ID", "Sampling_time"))

# 2) 与时序表(含经纬度、年、月)合并
final_data_with_dynamic <- outlets_data %>%
  left_join(dynamic_data, by = c("ID", "Sampling_time"))

# 3) 再与静态数据合并（按照 ID）
final_data <- final_data_with_dynamic %>%
  left_join(static_data, by = "ID")

# 4) 重新排列列顺序
final_data <- final_data %>%
  select(
    ID,
    Sampling_year,
    Sampling_month,
    Longitude,
    Latitude,
    everything(),
    -Sampling_time
  )

write_xlsx(final_data, paths$output)
cat("处理完成！结果已保存至:", paths$output, "\n")