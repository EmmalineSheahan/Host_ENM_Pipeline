# creating final spatraster stack of all thresholded natural range models

library(terra)
library(sf)
library(rnaturalearthhires)
library(ggplot2)
library(tidyterra)

target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

# modelled species
model_stack <- rast(paste0('./results/nat_SDM_thresh_rasters/', 
            list.files('./results/nat_SDM_thresh_rasters')))

#hull species
hullsp <- read.csv('./base_data/nat_hull_species_with_4_to_7_coords.csv')
hull_names <- hullsp$Species
hull_names <- c(hull_names, "Aegilops_mutica")

hull_stack <- rast(paste0('./results/hulls/', 
                          list.files('./results/hulls')))
names(hull_stack) <- hull_names

# combining stacks
full_stack <- c(model_stack, hull_stack)

# writing to file
writeRaster(full_stack, filename = './results/nat_complete_thresh_stack.tif', 
            overwrite = T)

# making richness raster
full_richness <- app(full_stack, fun = "sum", na.rm = T)

# plotting richness
pdf('./plots/nat_full_richness.pdf')
  p <- ggplot() +
    geom_sf(data = land, fill = "grey", col = "black") +
    geom_spatraster(data = full_richness) +
    coord_sf(xlim = c(-180, 180), ylim = c(-90, 90), expand = F) +
    scale_fill_gradient2(high = "yellow", mid = "#3DED97", low = "#601f9e",
                         midpoint = max(values(full_richness), na.rm = T)/2,
                         na.value = "transparent") +
    theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "aliceblue"),
          axis.title = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    ggtitle("Natural Range Taxa Species Richness") +
    labs(fill = "Species Richness")
  print(p)
dev.off()