# retrieving and formatting environmental data

library(geodata)
library(sf)
library(terra)
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

unit_list <- c("°C", "°C", "°C", "mm", "mm", "% Cloudy Days", "Aridity Index")

# retrieving soil variables (ph, carbon, sand, calcium) from soil grids
# use geodata package


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
