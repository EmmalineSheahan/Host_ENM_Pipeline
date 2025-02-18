# plotting all cleaned occurrence records native and introduced for final checking

library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(sf)

# creating land
target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

# reading in needed lists
host_list <- list.files('./occ_data')
introduced_list <- list.files('./int_data')

host_names <- gsub(".csv", "", host_list)
host_nats <- list.files('./nat_ranges')
host_nats <- gsub('.shp', '', host_nats)
host_nats <- gsub('.shx', '', host_nats)
host_nats <- gsub('.dbf', '', host_nats)
host_nats <- gsub('.prj', '', host_nats)
host_nats <- unique(host_nats)

int_names <- gsub(".csv", "", introduced_list)
int_nats <- list.files('./int_ranges')
int_nats <- gsub('.shp', '', int_nats)
int_nats <- gsub('.shx', '', int_nats)
int_nats <- gsub('.dbf', '', int_nats)
int_nats <- gsub('.prj', '', int_nats)
int_nats <- unique(int_nats)

# plotting cleaned native
pdf('./plots/All_cleaned_occs_native.pdf')

for(i in seq_along(host_list)) {
  thinned_data <- read.csv(paste0('./occ_data/', host_list[i]))
  if(host_names[i] %in% host_nats) {
    natrange <- st_read(paste0('./nat_ranges/', host_names[i], 
                                           '.shp'))
    natrange <- st_set_crs(natrange, target_crs)
    natrange <- st_set_crs(natrange, target_crs)
    p <- ggplot() +
      geom_sf(data = land, fill = "darkgrey") +
      geom_sf(data = natrange, fill = "#643B9F") +
      geom_point(data = thinned_data, aes(x = decimalLongitude, y = decimalLatitude,
                                        fill = species), 
               colour = "black", fill = "#FFBF00", size = 3, pch = 21) +
      coord_sf(xlim = c((min(thinned_data$decimalLongitude)-10), 
                      (max(thinned_data$decimalLongitude)+10)),
             ylim = c((min(thinned_data$decimalLatitude)-10),
                      (max(thinned_data$decimalLatitude)+10)), expand = F) +
      ggtitle(paste0("Cleaned Occurrence Records for ", gsub('_', ' ', host_names[i]))) +
      theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "aliceblue"),
          axis.title = element_blank())
  } else {
    p <- ggplot() +
      geom_sf(data = land, fill = "darkgrey") +
      geom_point(data = thinned_data, aes(x = decimalLongitude, y = decimalLatitude,
                                        fill = species), 
               fill = "#FFBF00", colour = "black", size = 3, pch = 21) +
      coord_sf(xlim = c((min(thinned_data$decimalLongitude)-10), 
                      (max(thinned_data$decimalLongitude)+10)),
             ylim = c((min(thinned_data$decimalLatitude)-10),
                      (max(thinned_data$decimalLatitude)+10)), expand = F) +
      ggtitle(paste0("Cleaned Occurrence Records for ", gsub('_', ' ', host_names[i]))) +
      theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "aliceblue"),
          axis.title = element_blank())
  }
  print(p)
}
dev.off()

# plotting cleaned introduced
pdf('./plots/All_cleaned_occs_introduced.pdf')

for(i in seq_along(introduced_list)) {
  thinned_data <- read.csv(paste0('./int_data/', introduced_list[i]))
  if(int_names[i] %in% int_nats) {
    intrange <- st_read(paste0('./int_ranges/', int_names[i], 
                               '.shp'))
    intrange <- st_set_crs(intrange, target_crs)
    intrange <- st_set_crs(intrange, target_crs)
    p <- ggplot() +
      geom_sf(data = land, fill = "darkgrey") +
      geom_sf(data = intrange, fill = "#643B9F") +
      geom_point(data = thinned_data, aes(x = decimalLongitude, y = decimalLatitude,
                                          fill = species), 
                 colour = "black", fill = "#FFBF00", size = 3, pch = 21) +
      coord_sf(xlim = c((min(thinned_data$decimalLongitude)-10), 
                        (max(thinned_data$decimalLongitude)+10)),
               ylim = c((min(thinned_data$decimalLatitude)-10),
                        (max(thinned_data$decimalLatitude)+10)), expand = F) +
      ggtitle(paste0("Cleaned Occurrence Records for ", gsub('_', ' ', int_names[i]))) +
      theme(panel.grid.major = element_blank(), 
            panel.background = element_rect(fill = "aliceblue"),
            axis.title = element_blank())
  } else {
    p <- ggplot() +
      geom_sf(data = land, fill = "darkgrey") +
      geom_point(data = thinned_data, aes(x = decimalLongitude, y = decimalLatitude,
                                          fill = species), 
                 fill = "#FFBF00", colour = "black", size = 3, pch = 21) +
      coord_sf(xlim = c((min(thinned_data$decimalLongitude)-10), 
                        (max(thinned_data$decimalLongitude)+10)),
               ylim = c((min(thinned_data$decimalLatitude)-10),
                        (max(thinned_data$decimalLatitude)+10)), expand = F) +
      ggtitle(paste0("Cleaned Occurrence Records for ", gsub('_', ' ', int_names[i]))) +
      theme(panel.grid.major = element_blank(), 
            panel.background = element_rect(fill = "aliceblue"),
            axis.title = element_blank())
  }
  print(p)
}
dev.off()