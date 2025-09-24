# creating accessible area polygons per species for native group

library(rangeBuilder)
library(alphahull)
library(sf)
library(dplyr)
library(terra)
library(tidyterra)
library(ENMeval)
library(rnaturalearthhires)

# setting up array
start_num <- as.numeric(Sys.getenv("START_NUM"))
task_id <- as.numeric(start_num)
part <- paste0("part", task_id)

# creating land
target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

# reading in species lists
nat_list_final <- read.table('./base_data/nat_modelled_species_list.txt')
nat_list_final <- nat_list_final$V1

# creating directories for accessible area shapefiles
dir.create('./nat_acc_area')

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

  tryCatch(
    {
      # creating the different buffers
      temp_spec_mat <- read.csv(paste0('./occ_data/', nat_list_final[task_id], '.csv'))
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
        landmask <- terra::aggregate(as_spatvector(land))
        shape_new_clipped <- terra::intersect(newshape, landmask)
        if(is.list(shape_new_clipped)) {
          shape_new_clipped <- shape_new_clipped[[1]]
        }
        wholemap <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90, crs = target_crs)
        wholemap_area <- expanse(wholemap)[1,2]*0.8
        if(expanse(shape_new_clipped) > wholemap_area) {
          shape_list[[j]] <- NA
        } else {
          shape_list[[j]] <- shape_new_clipped
        }
      }
      
      print(paste0("buffers created for species ", task_id))
      
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
                                      tune.args = list(fc = c("L", "Q"), rm = c(1:2)),
                                      partitions = "none", n.bg = 10000)
          mod_res <- min(eval.results(simple_model)$AICc)
          model_results[j] <- mod_res
        }
      }
      
      print(paste0("models ran for species ", task_id))
      
      # find which polygon produced the lowest AIC
      want_shape_num <- which(model_results == min(model_results, na.rm = T))
      want_shape <- shape_list[[want_shape_num]]
        
      # write that polygon to the acc area directory
      st_write(st_as_sf(want_shape), dsn = './nat_acc_area', 
               layer = nat_list_final[task_id], 
               driver = "ESRI Shapefile",
               append = F)
      
      print(paste0("acc area file created for species ", task_id))
      
    }, 
    error=function(err){
      message('On iteration ',task_id, ' there was an error: ',err)
      alphahull_malfunction <<-c(alphahull_malfunction,task_id)
    }
  )

malfunctioned_species_nat <- nat_list_final[alphahull_malfunction]
write.table(malfunctioned_species_nat, 
            file = './base_data/nat_malfunctioned_species_list.txt', 
            row.names = F,
            col.names = F,
            append = T)

# get correct malfunctioned species list
compsp <- list.files('./nat_acc_area')
compsp <- gsub('.shp', '', compsp)
compsp <- gsub('.shx', '', compsp)
compsp <- gsub('.prj', '', compsp)
compsp <- gsub('.dbf', '', compsp)
compsp <- unique(compsp)
malfunctioned_species_nat <- nat_list_final[which(!(nat_list_final %in% compsp))]

