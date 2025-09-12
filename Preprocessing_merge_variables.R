library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(writexl)
library(purrr)
library(stringr)
library(lubridate)

cat("==== 1. 设置路径 ====\n")
data_dir <- "E:/POC research/data/2_inputData_watershed/Features"
output_file <- "E:/POC research/data/2_inputData_watershed/Input_features_864.xlsx"

# ==== 2. 读取静态因子 ====

unique_upslopes      <- read_csv(file.path(data_dir, "upSlope_elevRange.csv"))
unique_slopes        <- read_csv(file.path(data_dir, "slope.csv")) |>
  filter(ID %in% unique_upslopes$ID)
unique_areas         <- read_csv(file.path(data_dir, "area.csv"))
unique_soils         <- read_csv(file.path(data_dir, "soilsource.csv"))
unique_litter        <- read_csv(file.path(data_dir, "litter.csv"))
unique_litho         <- read_csv(file.path(data_dir, "lithology.csv"))

# === 合并静态因子 ===
static_vars <- list(
  unique_slopes,
  unique_upslopes,
  unique_areas,
  unique_soils,
  unique_litter,
  unique_litho
) %>% reduce(left_join, by = "ID") %>%
  mutate(across(where(is.numeric), ~ replace(.x, is.na(.x) | is.infinite(.x), 0)))

cat("Finished with Static Features\n")

# ==== 3. 读取动态因子 ====
ndvi     <- read_csv(file.path(data_dir, "NDVI.csv"))
prec     <- read_csv(file.path(data_dir, "prec.csv"))
temp     <- read_csv(file.path(data_dir, "temp.csv"))
npp      <- read_csv(file.path(data_dir, "NPP.csv"))
landcover<- read_csv(file.path(data_dir, "landcover.csv"))
soilloss <- read_csv(file.path(data_dir, "soilloss.csv"))
gdp      <- read_csv(file.path(data_dir, "gdp.csv"))
pop      <- read_csv(file.path(data_dir, "pop.csv"))
resdam   <- read_csv(file.path(data_dir, "RanD.csv")) |>
  mutate(across(-Sampling_time, ~ ifelse(is.na(.) | . == "", 0, .)))

temporal_vars <- list(
  ndvi,
  prec,
  temp,
  npp,
  landcover,
  soilloss,
  gdp,
  pop,
  resdam
) %>% reduce(left_join, by = c("ID", "Sampling_time"))

cat("Finished with temporal features\n")

# ==== 4. 构建 ID × 时间 组合 ====

basin_time <- temporal_vars %>% 
  distinct(ID, Sampling_time)

point_basin <- static_vars %>%
  distinct(ID)

point_time_grid <- point_basin %>% 
  inner_join(basin_time, by = "ID")

# ==== 5. 合并静态 + 动态 ====
point_timeseries <- point_time_grid %>%
  left_join(static_vars,   by = c("ID")) %>%
  left_join(temporal_vars, by = c("ID", "Sampling_time")) 

# ====
poc_records <- read_excel("E:\\POC research\\data\\3_POC records\\clean_records_GRADES.xlsx")
# point_timeseries <- read_excel("E:/POC research/data/2_inputData_watershed/Input_features.xlsx")

final_data <- point_timeseries %>%
  left_join(
    poc_records %>% 
      select(ID, Sampling_time, `Water_discharge(m3/s)`, `POC(mg/L)`, `POC_flux(g/s)`),
    by = c("ID", "Sampling_time")
  ) %>%
  mutate(
    Year = year(Sampling_time),  # 提取年份
    Month = month(Sampling_time) # 提取月份
  ) %>%
  select(-Sampling_time) %>%
  relocate(ID, Year, Month, Longitude, Latitude) %>%
  relocate(`Water_discharge(m3/s)`, `POC(mg/L)`, `POC_flux(g/s)`, .after = last_col())

# ==== 6. 导出结果 ====
write_xlsx(final_data, output_file)
