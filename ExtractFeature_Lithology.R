library(terra)
library(readxl)
library(dplyr)
library(readr)

stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
litho_tif_file <- "E:/POC research/data/2_Lithology/dztp_08022024_ras.tif"  
litho_mapping_file <- "E:/POC research/data/2_Lithology/DIC_lithology_en.xlsx"  
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/lithology.csv"

stations <- read_excel(stations_path) |>
  distinct(ID, Longitude, Latitude)

litho_map <- read_excel(litho_mapping_file, range = cell_cols(1:2)) %>%
  distinct() %>%
  filter(!is.na(Code) & !is.na(Lithology))

litho_types <- setNames(litho_map$Code, litho_map$Lithology)

litho_raster <- rast(litho_tif_file)

basin <- rast(file.path(basin_tiff_dir, "1.tif"))
litho_raster <- project(litho_raster, crs(basin))

calculate_lithology_variables <- function(station_id, basin_tiff_file, litho_raster) {
  cat(sprintf("Processing station %s\n", station_id))
  
  litho_vars <- setNames(rep(NA, length(litho_types)), names(litho_types))
  
  if (!file.exists(basin_tiff_file)) {
    warning(paste("Missing basin TIFF for station:", station_id))
    return(as.data.frame(t(litho_vars)))
  }
  
  basin <- rast(basin_tiff_file)
  litho_crop <- crop(litho_raster, basin)
  litho_masked <- mask(resample(litho_crop, basin, method = "near"), basin)
  
  # 提取每个岩性比例
  for (litho_name in names(litho_types)) {
    litho_code <- litho_types[[litho_name]]
    litho_vars[[litho_name]] <- global(litho_masked == litho_code, "mean", na.rm = TRUE)$mean
  }
  
  return(as.data.frame(t(litho_vars)))
}

lithology_results <- data.frame()
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if (station_id <864) next
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  
  litho_vars <- calculate_lithology_variables(station_id, basin_tiff_file, litho_raster)
  litho_vars$ID <- station_id
  lithology_results <- bind_rows(lithology_results, litho_vars) |>
    select(-`None data`,	-`water`,	-`Unassigned/Unknown`)
}

write_csv(lithology_results, output_path)
