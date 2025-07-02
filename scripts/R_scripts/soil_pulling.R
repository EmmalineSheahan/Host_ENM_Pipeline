# pulling vrt's down from soil grids first seeing as the process is deeply memory 
# intensive

library(geodata)
library(sf)
library(terra)
library(gdalUtilities)
library(rnaturalearthhires)
library(ggplot2)
library(tidyterra)
library(viridis)

# creating target crs
target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

# reading in raster for projection purposes
proj_ras <- rast('./env_data/climate/wc2.1_2.5m/wc2.1_2.5m_bio_1.tif')

# retrieving soil variables (ph, carbon, sand, cation exchange capacity) from soil grids
igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
sg_url <- "/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

wanted_soilvars <- c("phh2o", "sand", "soc", "cec")
soil_levels <- c("0-5", "5-15", "15-30")
soil_ras_list <- vector("list", length = 4)
for (i in seq_along(wanted_soilvars)) {
  for (j in seq_along(soil_levels)) {
    gdal_translate(paste0(sg_url, wanted_soilvars[i], '/', wanted_soilvars[i], '_',
                          soil_levels[j],
                          'cm_mean.vrt'),
                   paste0("./env_data/soils/", wanted_soilvars[i], "_",
                          soil_levels[j], "cm_mean.tif"))
  }
  soil_ras5 <- rast(paste0('./env_data/soils/', wanted_soilvars[i], '_0-5cm_mean.tif'))
  crs(soil_ras5) <- igh
  soil_ras15 <- rast(paste0('./env_data/soils/', wanted_soilvars[i], '_5-15cm_mean.tif'))
  crs(soil_ras15) <- igh
  soil_ras30 <- rast(paste0('./env_data/soils/', wanted_soilvars[i], '_15-30cm_mean.tif'))
  crs(soil_ras30) <- igh
  soil_ras5 <- project(soil_ras5, proj_ras)
  soil_ras15 <- project(soil_ras15, proj_ras)
  soil_ras30 <- project(soil_ras30, proj_ras)
  soilstack <- c(soil_ras5, soil_ras15, soil_ras30)
  soil_ras_list[[i]] <- terra::app(soilstack, mean)
}
all_soils <- rast(soil_ras_list)

writeRaster(all_soils, filename = './env_data/soils/all_soils.tif',
            overwrite = T)