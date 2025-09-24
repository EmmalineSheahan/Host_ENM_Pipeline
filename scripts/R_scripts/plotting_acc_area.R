# plotting accessible areas nat and int
library(ggplot2)
library(tidyterra)
library(rnaturalearthhires)
library(dplyr)
library(terra)
library(sf)

# creating land
target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

# creating plot directories
dir.create('./plots/nat_acc_areas')
dir.create('./plots/int_acc_areas')

# creating species lists
nat_list_final <- gsub('.shx', '', list.files('./nat_acc_area'))
nat_list_final <- gsub('.shp', '', nat_list_final)
nat_list_final <- gsub('.prj', '', nat_list_final)
nat_list_final <- gsub('.dbf', '', nat_list_final)
nat_list_final <- unique(nat_list_final)

int_list_final <- gsub('.shx', '', list.files('./int_acc_area'))
int_list_final <- gsub('.shp', '', int_list_final)
int_list_final <- gsub('.prj', '', int_list_final)
int_list_final <- gsub('.dbf', '', int_list_final)
int_list_final <- unique(int_list_final)

# create plots to ensure accessible areas are accurate, nat
for(i in seq_along(nat_list_final)) {
  
  want_shape <- st_read(dsn = './nat_acc_area', layer = nat_list_final[i])
  temp_spec_mat <- read.csv(paste0('./occ_data/', nat_list_final[i], '.csv'))
  
  p <- ggplot() +
    geom_sf(data = land, fill = "darkgrey") +
    geom_sf(data = want_shape, fill = "lightgreen") +
    geom_point(data = temp_spec_mat, aes(x = decimalLongitude, y = decimalLatitude,
                                       fill = species), 
             colour = "black", fill = "#FFBF00", size = 2, pch = 21) +
    coord_sf(xlim = c(((ext(want_shape)[1])-10), 
                    ((ext(want_shape)[2])+10)),
           ylim = c(((ext(want_shape)[3])-10),
                    ((ext(want_shape)[4])+10)), expand = F) +
    ggtitle(paste0("Accessible Area for ", gsub("_", ' ', nat_list_final[i]))) +
    theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank())
  pdf(paste0('./plots/nat_acc_areas/', nat_list_final[i], "_nat_acc_area.pdf"))
  print(p)
  dev.off()
}

# create plots to ensure accessible areas are accurate, int
for(i in seq_along(int_list_final)) {
  
  want_shape <- st_read(dsn = './int_acc_area', layer = int_list_final[i])
  temp_spec_mat <- read.csv(paste0('./int_data/', int_list_final[i], '.csv'))
  
  p <- ggplot() +
    geom_sf(data = land, fill = "darkgrey") +
    geom_sf(data = want_shape, fill = "lightgreen") +
    geom_point(data = temp_spec_mat, aes(x = decimalLongitude, y = decimalLatitude,
                                         fill = species), 
               colour = "black", fill = "#FFBF00", size = 2, pch = 21) +
    coord_sf(xlim = c(((ext(want_shape)[1])-10), 
                      ((ext(want_shape)[2])+10)),
             ylim = c(((ext(want_shape)[3])-10),
                      ((ext(want_shape)[4])+10)), expand = F) +
    ggtitle(paste0("Accessible Area for ", gsub("_", ' ', int_list_final[i]))) +
    theme(panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "aliceblue"),
          axis.title = element_blank())
  pdf(paste0('./plots/int_acc_areas/', int_list_final[i], "_int_acc_area.pdf"))
  print(p)
  dev.off()
}