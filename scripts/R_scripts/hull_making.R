# making alpha hulls for species with too few coordinates to model

library(dplyr)
library(sf)
library(terra)
library(ggplot2)
library(rnaturalearthhires)
library(rangeBuilder)
library(tidyterra)

# creating directories
dir.create('./results/hulls')
dir.create('./plots/hulls')

# reading in hull species list
hullsp <- read.csv('./base_data/nat_hull_species_with_4_to_7_coords.csv')
hull_names <- hullsp$Species

# adding in Aegilops mutica, seeing as its ENM failed
hull_names <- c(hull_names, "Aegilops_mutica")

# reading in environmental raster stack to use as template
envs <- rast('./env_data/all_envs.tif')

# creating land
target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

for (i in seq_along(hull_names)) {
  
  # reading in occurrences
  occs <- read.csv(paste0('./occ_data/', hull_names[i], '.csv'))
  
  # building buffer
  rangebuilder_malfunction <- integer()
  sf_use_s2(F)
  tryCatch(
    {
    hull <- getDynamicAlphaHull(occs[,2:3], 
                              fraction = 1, partCount = 1, 
                              initialAlpha = 40, clipToCoast = "terrestrial",
                              coordHeaders = c("X", 
                                               "Y"), 
                              buff = 100000)
    }, 
    error=function(err){
      message('On iteration ',i, ' there was an error: ',err)
      rangebuilder_malfunction <<-c(rangebuilder_malfunction,i)
    }
  )
  
  if(i %in% rangebuilder_malfunction) {
    temp_spec_sf <- st_as_sf(occs[,2:3], 
                             coords = c("decimalLongitude", "decimalLatitude"),
                             crs = target_crs)
    temp_spec_sf <- st_make_valid(temp_spec_sf)
    temp_alph <- st_convex_hull(x = st_union(temp_spec_sf))
    hull <- st_make_valid(temp_alph)
  }
  
  # building threshold raster
  hull_thresh <- rast(crs = target_crs, extent = ext(envs), resolution = res(envs)[1],
                      vals = 1)
  hull_thresh_final <- mask(hull_thresh, as_spatvector(hull[[1]]))
  hull_thresh_final <- mask(hull_thresh_final, land)
  
  # write to file
  writeRaster(hull_thresh_final, filename = paste0('./results/hulls/', hull_names[i], 
                                                   '.tif'), overwrite = T)
  
  # creating plot for checking
  pdf(paste0('./plots/hulls/', hull_names[i], '.pdf'))
  p <- ggplot() +
    geom_sf(data = land, fill = "darkgrey") +
    geom_spatraster(data = hull_thresh_final, show.legend = F) +
    scale_fill_gradient(high = "#522081", low = "white",
                        na.value = "transparent") +
    geom_point(data = occs, aes(x = decimalLongitude, y = decimalLatitude,
                                        fill = species), 
               colour = "black", fill = "#FFBF00", size = 3, pch = 21) +
    coord_sf(xlim = c((min(occs$decimalLongitude)-10), 
                      (max(occs$decimalLongitude)+10)),
             ylim = c((min(occs$decimalLatitude)-10),
                      (max(occs$decimalLatitude)+10)), expand = F) +
    ggtitle(paste0("Alpha hull for ", gsub('_', ' ', hull_names[i]))) +
    theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "aliceblue"),
          axis.title = element_blank())
  print(p)
  dev.off()
}
