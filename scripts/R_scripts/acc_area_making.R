# creating accessible area polygons per species for native group and 
# introduced group

library(rangeBuilder)
library(alphahull)
library(sf)
library(dplyr)
library(terra)
library(tidyterra)
library(ENMeval)
library(rnaturalearthhires)

# creating species list
nat_list <- list.files('./occ_data')
int_list <- list.files('./int_data')
nat_names <- gsub('.csv', '', nat_list)
int_names <- gsub('.csv', '', int_list)

# checking how many records exist for each species
records_amount_nat <- vector(length = length(nat_list))
for (i in seq_along(nat_list)) {
  t <- read.csv(paste0('./occ_data/', nat_list[i]))
  records_amount_nat[i] <- nrow(t)
}

records_amount_int <- vector(length = length(int_list))
for (i in seq_along(int_list)) {
  t <- read.csv(paste0('./int_data/', int_list[i]))
  records_amount_int[i] <- nrow(t)
}

# we'll have to drop species with too few records
drop_species_nat <- nat_names[which(records_amount_nat < 4)]
amount_of_records_drop <- records_amount_nat[which(records_amount_nat < 4)]
drop_species_df <- data.frame(drop_species_nat, amount_of_records_drop)
colnames(drop_species_df) <- c("Species", "Amount_of_Records")
write.csv(drop_species_df, file = './base_data/nat_species_with_under_4_coords.csv')

drop_species_int <- int_names[which(records_amount_int < 4)]
amount_of_records_drop <- records_amount_int[which(records_amount_int < 4)]
drop_species_df_int <- data.frame(drop_species_int, amount_of_records_drop)
colnames(drop_species_df_int) <- c("Species", "Amount_of_Records")
write.csv(drop_species_df_int, file = './base_data/int_species_with_under_4_coords.csv')

# for species with 4-7 points, we can make alpha hulls around them for richness
# these will have to be vetted by hand
nat_list_new <- nat_list[-which(records_amount_nat < 4)]
nat_names_new <- nat_names[-which(records_amount_nat < 4)]
records_amount_nat_new <- vector(length = length(nat_list_new))
for (i in seq_along(nat_list_new)) {
  t <- read.csv(paste0('./occ_data/', nat_list_new[i]))
  records_amount_nat_new[i] <- nrow(t)
}

hull_species_nat <- nat_names_new[which(records_amount_nat_new < 8)]
records_hull_nat <- records_amount_nat_new[which(records_amount_nat_new < 8)]
hull_species_df_nat <- data.frame(hull_species_nat, records_hull_nat)
colnames(hull_species_df_nat) <- c("Species", "Amount_of_Records")
write.csv(hull_species_df_nat, 
          file = './base_data/nat_hull_species_with_4_to_7_coords.csv')

int_list_new <- int_list[-which(records_amount_int < 4)]
int_names_new <- int_names[-which(records_amount_int < 4)]
records_amount_int_new <- vector(length = length(int_list_new))
for (i in seq_along(int_list_new)) {
  t <- read.csv(paste0('./int_data/', int_list_new[i]))
  records_amount_int_new[i] <- nrow(t)
}

hull_species_int <- int_names_new[which(records_amount_int_new < 8)]
records_hull_int <- records_amount_int_new[which(records_amount_int_new < 8)]
hull_species_df_int <- data.frame(hull_species_int, records_hull_int)
colnames(hull_species_df_int) <- c("Species", "Amount_of_Records")
write.csv(hull_species_df_int, 
          file = './base_data/int_hull_species_with_4_to_7_coords.csv')

# writing the final lists of species with 8 or greater cleaned occurrences for modelling
nat_list_final <- nat_list_new[-which(records_amount_nat_new < 8)]
nat_names_final <- nat_names_new[-which(records_amount_nat_new < 8)]
nat_titles_final <- gsub('_', ' ', nat_names_final)
write.table(nat_names_final, file = './base_data/nat_modelled_species_list.txt', 
            row.names = F,
            col.names = F)

int_list_final <- int_list_new[-which(records_amount_int_new < 8)]
int_names_final <- int_names_new[-which(records_amount_int_new < 8)]
int_titles_final <- gsub('_', ' ', int_names_final)
write.table(int_names_final, file = './base_data/int_modelled_species_list.txt', 
            row.names = F,
            col.names = F)

# creating land
target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

# creating directories for accessible area shapefiles
dir.create('./nat_acc_area')
dir.create('./int_acc_area')

# creating various accessible areas of buffer distances 3, 4, 5, and 6 times larger
# than the area of the coordinates, and then running initial models to select the best
# buffer for each species via AIC
buff_props <- c(3, 4, 5, 6)

# pulling in environmental data
all_envs <- rast('./env_data/all_envs.tif')

# creating accessible area polygons, nat species
# for some reason there's an inexplicable error occurring in getDynamicAlphaHull
# even though there are enough coordinates for the function to work,
# i'm inserting a tryCatch for now which will store the species it's refusing
# to work on
alphahull_malfunction <- integer()

pdf('./plots/nat_accessible_areas_modelled_species.pdf')
for (i in seq_along(nat_list_final)) {
  tryCatch(
    {
      # creating the different buffers
      temp_spec_mat <- read.csv(paste0('./occ_data/', nat_list_final[i]))
      temp_spec_mat <- temp_spec_mat[,2:4]
      temp_spec_coords <- temp_spec_mat[,1:2]
      temp_alph <- getDynamicAlphaHull(temp_spec_coords, fraction = 1, partCount = 1, 
                                       initialAlpha = 40, clipToCoast = "terrestrial",
                                       coordHeaders = c("decimalLongitude", 
                                                        "decimalLatitude"), 
                                       buff = 100000)
      shape_list <- vector("list", length = length(buff_props))
      for (j in seq_along(buff_props)) {
        buffDist <- (sqrt(buff_props[j]*expanse(as_spatvector(temp_alph[[1]]), 
                                                unit = "m")) - 
                       sqrt(expanse(as_spatvector(temp_alph[[1]]), unit = "m")))/2
        newshape <- buffer(as_spatvector(temp_alph[[1]]), width = buffDist)
        shape_new_clipped <- intersect(newshape, as_spatvector(land))
        shape_new_clipped <- terra::aggregate(shape_new_clipped)
        if(expanse(shape_new_clipped) > (expanse(aggregate(as_spatvector(land)))*0.8)) {
          shape_list[[j]] <- NA
        } else {
          shape_list[[j]] <- shape_new_clipped
        }
      }
      
      # j loop to crop rasters to each polygon, run ENMeval for each polygon, and store
      # model results for each polygon
      model_results <- vector(length = length(shape_list))
      for (j in 1:length(shape_list)) {
        if(is.logical(shape_list[[j]])) {
          model_results[j] <- NA
        } else {
          cropped_envs <- crop(all_envs, shape_list[[j]])
          clipped_envs <- mask(cropped_envs, shape_list[[j]])
          simple_model <- ENMevaluate(occs = temp_spec_mat[,1:2], envs = clipped_envs, 
                                      algorithm = "maxnet",
                                      tune.args = list(fc = c("L","LQ"), rm = 1:2),
                                      partitions = "none", n.bg = 10000)
          mod_res <- min(eval.results(simple_model)$AICc)
          model_results[j] <- mod_res
        }
      }
      
      # find which polygon produced the lowest AIC
      want_shape_num <- which(model_results == min(model_results, na.rm = T))
      want_shape <- shape_list[[want_shape_num]]
      
      # create plots to ensure accessible areas are accurate
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
        ggtitle(paste0("Accessible Area for ", nat_titles_final[i])) +
        theme(panel.grid.major = element_blank(), 
              panel.background = element_rect(fill = "aliceblue"),
              axis.title = element_blank())
      print(p)
        
      
      # write that polygon to the acc area directory
      st_write(want_shape, dsn = './nat_acc_area', layer = nat_names_final[i], 
               driver = "ESRI Shapefile",
               append = F)
    }, 
    error=function(err){
      message('On iteration ',i, ' there was an error: ',err)
      alphahull_malfunction <<-c(alphahull_malfunction,i)
    }
  )
}
dev.off()

malfunctioned_species_nat <- nat_list_final[alphahull_malfunction]
write.table(malfunctioned_species_nat, 
            file = './base_data/nat_malfunctioned_species_list.txt', 
            row.names = F,
            col.names = F)

# creating accessible area polygons, int species
print("beginning int species")

alphahull_malfunction <- integer()

pdf('./plots/int_accessible_areas_modelled_species.pdf')
for (i in seq_along(int_list_final)) {
  tryCatch(
    {
      # creating the different buffers
      temp_spec_mat <- read.csv(paste0('./int_data/', int_list_final[i]))
      temp_spec_mat <- temp_spec_mat[,2:4]
      temp_spec_coords <- temp_spec_mat[,1:2]
      temp_alph <- getDynamicAlphaHull(temp_spec_coords, fraction = 1, partCount = 1, 
                                       initialAlpha = 40, clipToCoast = "terrestrial",
                                       coordHeaders = c("decimalLongitude", 
                                                        "decimalLatitude"), 
                                       buff = 100000)
      shape_list <- vector("list", length = length(buff_props))
      for (j in seq_along(buff_props)) {
        buffDist <- (sqrt(buff_props[j]*expanse(as_spatvector(temp_alph[[1]]), 
                                                unit = "m")) - 
                       sqrt(expanse(as_spatvector(temp_alph[[1]]), unit = "m")))/2
        newshape <- buffer(as_spatvector(temp_alph[[1]]), width = buffDist)
        shape_new_clipped <- intersect(newshape, as_spatvector(land))
        shape_new_clipped <- terra::aggregate(shape_new_clipped)
        if(expanse(shape_new_clipped) > (expanse(aggregate(as_spatvector(land)))*0.8)) {
          shape_list[[j]] <- NA
        } else {
          shape_list[[j]] <- shape_new_clipped
        }
      }
      
      # j loop to crop rasters to each polygon, run ENMeval for each polygon, and store
      # model results for each polygon
      model_results <- vector(length = length(shape_list))
      for (j in 1:length(shape_list)) {
        if(is.logical(shape_list[[j]])) {
          model_results[j] <- NA
        } else {
          cropped_envs <- crop(all_envs, shape_list[[j]])
          clipped_envs <- mask(cropped_envs, shape_list[[j]])
          simple_model <- ENMevaluate(occs = temp_spec_mat[,1:2], envs = clipped_envs, 
                                      algorithm = "maxnet",
                                      tune.args = list(fc = c("L","LQ"), rm = 1:2),
                                      partitions = "none", n.bg = 10000)
          mod_res <- min(eval.results(simple_model)$AICc)
          model_results[j] <- mod_res
        }
      }
      
      # find which polygon produced the lowest AIC
      want_shape_num <- which(model_results == min(model_results, na.rm = T))
      want_shape <- shape_list[[want_shape_num]]
      
      # create plots to ensure accessible areas are accurate
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
        ggtitle(paste0("Accessible Area for ", int_titles_final[i])) +
        theme(panel.grid.major = element_blank(), 
              panel.background = element_rect(fill = "aliceblue"),
              axis.title = element_blank())
      print(p)
      
      
      # write that polygon to the acc area directory
      st_write(want_shape, dsn = './int_acc_area', layer = int_names_final[i], 
               driver = "ESRI Shapefile",
               append = F)
    }, 
    error=function(err){
      message('On iteration ',i, ' there was an error: ',err)
      alphahull_malfunction <<-c(alphahull_malfunction,i)
    }
  )
}
dev.off()

malfunctioned_species_int <- int_list_final[alphahull_malfunction]
write.table(malfunctioned_species_int, 
            file = './base_data/int_malfunctioned_species_list.txt', 
            row.names = F,
            col.names = F)

