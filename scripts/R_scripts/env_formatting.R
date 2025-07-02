# retrieving and formatting environmental data

install.packages("geodata")
install.packages("gdalUtilities")

library(geodata)
library(sf)
library(terra)
library(gdalUtilities)
library(rnaturalearthhires)
library(ggplot2)
library(tidyterra)
library(viridis)

# creating land
target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

# pulling down wanted variables from worldclim
all_vars <- worldclim_global(var = "bio", res = 2.5, path = './env_data')

# getting needed variables and fixing units
wanted_vars <- c(1, 2, 3, 12, 15)
bio_vars <- all_vars[[wanted_vars]]
for (i in 1:3) {
  values(bio_vars[[i]]) <- values(bio_vars[[i]])/10
}

# re-naming
new_names <- c("Annual_Mean_Temperature", "Mean_Diurnal_Range", "Isothermality", 
               "Total_Annual_Precipitation", "Precipitation_Seasonality")
names(bio_vars) <- new_names

# cloud cover
cloud_cover <- rast('./env_data/MODCF_meanannual.tif')
cloud_cover <- project(cloud_cover, bio_vars[[1]])
cloud_cover <- mask(cloud_cover, land)
values(cloud_cover) <- values(cloud_cover)*0.01

# aridity index
aridity_index <- rast('./env_data/Global-AI_ET0_annual_v3/Global-AI_ET0_v3_annual/ai_v3_yr.tif')
aridity_index <- project(aridity_index, bio_vars[[1]])
aridity_index <- mask(aridity_index, land)
values(aridity_index) <- values(aridity_index)*0.0001

# combining with biovars
all_envs <- c(bio_vars, cloud_cover, aridity_index)
names(all_envs) <- c("Annual_Mean_Temperature", "Mean_Diurnal_Range", "Isothermality", 
                     "Total_Annual_Precipitation", "Precipitation_Seasonality", "Cloud_Cover",
                     "Aridity_Index")

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
  soil_ras5 <- rast(paste0('./env_data/soils/', wanted_soilvars[i], '_0-5cm_mean.vrt'),
                    drivers = "OGR_VRT")
  crs(soil_ras5) <- igh
  soil_ras15 <- rast(paste0('./env_data/soils/', wanted_soilvars[i], '_5-15cm_mean.vrt'))
  crs(soil_ras15) <- igh
  soil_ras30 <- rast(paste0('./env_data/soils/', wanted_soilvars[i], '_15-30cm_mean.vrt'))
  crs(soil_ras30) <- igh
  soil_ras5 <- project(soil_ras5, all_envs[1])
  soil_ras15 <- project(soil_ras15, all_envs[1])
  soil_ras30 <- project(soil_ras30, all_envs[1])
  soilstack <- c(soil_ras5, soil_ras15, soil_ras30)
  soil_ras_list[[i]] <- terra::app(soilstack, mean)
}
all_soils <- rast(soil_ras_list)

# combining with other env variables
all_envs <- c(all_envs, all_soils)
names(all_envs) <- c("Annual_Mean_Temperature", "Mean_Diurnal_Range", "Isothermality", 
                     "Total_Annual_Precipitation", "Precipitation_Seasonality", "Cloud_Cover",
                     "Aridity_Index", "Soil_pH", "Sand", "Soil_Organic_Carbon",
                     "Cation_Exchange_Capacity")
unit_list <- c("°C", "°C", "°C", "mm", "mm", "% Cloudy Days", "Aridity Index", "pH", 
               "g/kg", "dg/kg", "mmol(c)/kg")

# writing to file
writeRaster(all_envs, filename = './env_data/all_envs.tif', 
            filetype = "GTiff", overwrite = T)

pdf('./plots/Environmental_Variables.pdf')
for (i in 1:dim(all_envs)[3]) {
     p <- ggplot() +
       geom_spatraster(data = all_envs[[i]]) +
       geom_sf(data = land, fill = NA, col = "black") +
       coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = F) +
       scale_fill_gradient2(high = "yellow", mid = "#3DED97", low = "#8969CD",
                            midpoint = max(values(all_envs[[i]]), na.rm = T)/2,
                       na.value = "transparent") +
       theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
       panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
       ggtitle(gsub("_", " ", names(all_envs)[i])) +
       labs(fill = unit_list[i])
 print(p)
}
dev.off()
