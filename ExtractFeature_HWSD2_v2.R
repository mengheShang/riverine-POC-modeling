library(terra)
library(readxl)
library(dplyr)
library(readr)

# Define file paths
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
clay_tiff_file <- "E:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__CLAY.tif"
sand_tiff_file <- "E:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__SAND.tif"
silt_tiff_file <- "E:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__SILT.tif"
organic_carbon_tiff_file <- "E:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__ORG_CARBON.tif"
root_depth_tiff_file <- "E:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__ROOT_DEPTH.tif"
ph_tiff_file <- "E:\\POC research\\data\\2_HWSD2\\HWSD2_D1\\HWSD2_D1__PH_WATER.tif"
basin_tiff_dir <- "E:\\POC research\\data\\1_DEM_watershed\\watershed_new_500m\\"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/soilsource.csv"

# Read station data
stations <- read_excel(stations_path) |>
  distinct(ID, Longitude, Latitude)

# Read raster data
clay_rast <- rast(clay_tiff_file)
sand_rast <- rast(sand_tiff_file)
silt_rast <- rast(silt_tiff_file)
organic_carbon_rast <- rast(organic_carbon_tiff_file)
root_depth_rast <- rast(root_depth_tiff_file)
ph_rast <- rast(ph_tiff_file)

## Function to prepare raster by reprojecting, aligning, and setting negative values to NA
prepare_raster <- function(target_rast, reference_rast) {
  if (!ext(target_rast) == ext(reference_rast)) {
    message("Extents do not match. Re-clipping the raster.")
    target_rast <- project(target_rast, crs(reference_rast))
    target_rast <- resample(target_rast, reference_rast, method = "near")
    target_rast <- crop(target_rast, ext(reference_rast))
  }
  
  # Convert negative values to NA
  target_rast[target_rast < 0] <- NA
  
  return(target_rast)
}

## Preprocessing
basin_tiff_file <- "E:\\POC research\\data\\1_DEM_watershed\\watershed_new_500m\\1.tif"
basin_tiff <- rast(basin_tiff_file)

# Align rasters with basin_tiff
clay <- prepare_raster(clay_rast, basin_tiff)
sand <- prepare_raster(sand_rast, basin_tiff)
silt <- prepare_raster(silt_rast, basin_tiff)
OC <- prepare_raster(organic_carbon_rast, basin_tiff)
root_depth <- prepare_raster(root_depth_rast, basin_tiff)
ph <- prepare_raster(ph_rast, basin_tiff)

# Define function to calculate soil variables for each station
calculate_soil_variables <- function(station_id, basin_tiff_file) {
  soil_variables <- data.frame(ID = station_id)
  
  # Print progress
  cat(sprintf("Processing station %s\n", station_id))
  
  # Read basin TIF file for the station's watershed
  basin_tiff <- rast(basin_tiff_file)
  
  # Set NoData value to NA for basin raster
  basin_tiff[basin_tiff == 255] <- NA
  
  # Align rasters to basin extent
  clay <- prepare_raster(clay, basin_tiff)
  sand <- prepare_raster(sand, basin_tiff)
  silt <- prepare_raster(silt, basin_tiff)
  OC <- prepare_raster(OC, basin_tiff)
  root_depth <- prepare_raster(root_depth, basin_tiff)
  ph <- prepare_raster(ph, basin_tiff)
  plot(OC)
  
  # Mask each raster by basin area
  clay <- mask(clay, basin_tiff)
  sand <- mask(sand, basin_tiff)
  silt <- mask(silt, basin_tiff)
  OC <- mask(OC, basin_tiff)
  root_depth <- mask(root_depth, basin_tiff)
  ph <- mask(ph, basin_tiff)
  plot(OC)
  
  # Extract soil data
  soil_variables$`Clay` <- global(clay, "mean", na.rm = TRUE)$mean
  soil_variables$`Sand` <- global(sand, "mean", na.rm = TRUE)$mean
  soil_variables$`Silt` <- global(silt, "mean", na.rm = TRUE)$mean
  soil_variables$`SOC` <- global(OC, "mean", na.rm = TRUE)$mean
  soil_variables$`pH_soil` <- global(ph, "mean", na.rm = TRUE)$mean
  
  # Use root depth class values directly to calculate the mean root depth class
  soil_variables$`Root_Depth` <- global(root_depth, "mean", na.rm = TRUE)$mean
  
  return(soil_variables)
}

# Initialize results dataframe
soil_results <- data.frame()

# Process each station
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if (station_id < 864) next
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  
  if (file.exists(basin_tiff_file)) {
    soil_variables <- calculate_soil_variables(station_id, basin_tiff_file)
    soil_results <- bind_rows(soil_results, soil_variables)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
  gc()  # Garbage collection to free memory
}

# View and save results
print(soil_results)
write_csv(soil_results, output_path)
