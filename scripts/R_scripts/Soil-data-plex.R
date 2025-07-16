
library(geodata)
library(terra)

# Create output folder
dir.create("soil_data", showWarnings = FALSE)

# Variables of interest and depths
variables <- c("phh2o", "clay", "silt", "sand", "soc")
depths <- c(5, 15, 30)

# Initialize list to store results
mean_layers <- list()

# Loop through variables
for (var in variables) {
  message(paste("Processing:", var))
  
  # List to store rasters for this variable
  depth_rasters <- list()
  
  # Loop through each depth
  for (d in depths) {
    message(paste("  Downloading depth:", d))
    
    # Download raster for variable and depth
    r <- soil_world(var = var, depth = d, path = "soil_data")
    
    # Keep the mean (usually first layer)
    r_mean <- r[[1]]
    names(r_mean) <- paste0(var, "_", d)
    depth_rasters[[d]] <- r_mean
  }
  
  # Stack and calculate mean across 3 depths
  depth_stack <- rast(depth_rasters)
  depth_mean <- mean(depth_stack, na.rm = TRUE)
  names(depth_mean) <- paste0(var, "_mean_0_30cm")
  
  # Store in list
  mean_layers[[var]] <- depth_mean
  
  # Optional: Save to file
  writeRaster(depth_mean, filename = paste0("soil_data/", var, "_mean_0_30cm.tif"), overwrite = TRUE)
}

# Stack all mean layers together
soil_stack <- rast(mean_layers)

# Optional: Save full stack
writeRaster(soil_stack, "soil_data/soil_stack_mean_0_30cm.tif", overwrite = TRUE)

# Quick plot
plot(soil_stack)
