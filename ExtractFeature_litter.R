library(terra)
library(dplyr)
library(readr)
library(readxl)

# Define file paths
rh_tiff_file <- "E:/POC research/data/2_Litter_Rh/Rh_RF_ensemble_mean_1982_2018.tif"
litter_tiff_file <- "E:/POC research/data/2_Litter_Rh/litter_360_720.tif"
stations_path <- "E:/POC research/data/3_POC records/clean_records.xlsx"
basin_tiff_dir <- "E:/POC research/data/1_DEM_watershed/watershed_new_500m/"
output_path <- "E:/POC research/data/2_inputData_watershed/Features/litter.csv"

# Read station data
stations <- read_excel(stations_path) %>%
  distinct(ID, Longitude, Latitude)

# Function to align rasters
align_extent <- function(raster1, raster2) {
  # Ensure same CRS
  if (!terra::same.crs(raster1, raster2)) {
    raster2 <- terra::project(raster2, terra::crs(raster1))
  }
  
  # Check geometry and resample if needed
  if (!terra::compareGeom(raster1, raster2, 
                          crs = FALSE,  
                          ext = FALSE,   
                          res = TRUE,     
                          stopOnError = FALSE)) {
    raster2 <- terra::resample(raster2, raster1, method = "near")
  }
  
  # Crop to same extent if needed
  if (!all(terra::ext(raster1) == terra::ext(raster2))) {
    raster2 <- terra::crop(raster2, terra::ext(raster1))
  }
  
  return(raster2)
}

# Read and preprocess base rasters
rh_rast <- rast(rh_tiff_file)
litter_rast <- rast(litter_tiff_file)

# Calculate mean across all bands (years) for each variable
rh_mean <- mean(rh_rast, na.rm = TRUE)
litter_mean <- mean(litter_rast, na.rm = TRUE)

# Define function to process each station's watershed
calculate_watershed_variables <- function(station_id, basin_tiff_file, 
                                          rh_raster, litter_raster) {
  watershed_data <- data.frame(ID = station_id)
  
  # Print progress
  cat(sprintf("Processing station %s\n", station_id))
  
  # Load the basin TIF file for the station's watershed
  basin_tiff <- rast(basin_tiff_file)
  
  # Set NoData value to NA for basin raster
  basin_tiff[basin_tiff == 255] <- NA
  
  # Align and process Rh data
  rh_aligned <- align_extent(basin_tiff, rh_raster)
  rh_masked <- mask(rh_aligned, basin_tiff)
  watershed_data$`Rh(g C/m2/yr)` <- global(rh_masked, "mean", na.rm = TRUE)$mean
  
  # Align and process Litter data
  litter_aligned <- align_extent(basin_tiff, litter_raster)
  litter_masked <- mask(litter_aligned, basin_tiff)
  watershed_data$`Litter` <- global(litter_masked, "mean", na.rm = TRUE)$mean
  
  return(watershed_data)
}

# Initialize dataframe for results
watershed_results <- data.frame()

# Process each station
for (i in 1:nrow(stations)) {
  station_id <- stations$ID[i]
  if(station_id<864)next
  basin_tiff_file <- file.path(basin_tiff_dir, paste0(station_id, ".tif"))
  
  if (file.exists(basin_tiff_file)) {
    watershed_vars <- calculate_watershed_variables(station_id, basin_tiff_file, 
                                                    rh_mean, litter_mean)
    watershed_results <- bind_rows(watershed_results, watershed_vars)
  } else {
    warning(paste("TIF file for station ID", station_id, "not found."))
  }
  gc()  # Garbage collection to free up memory
}

# View and save results
print(watershed_results)
write_csv(watershed_results, output_path)
