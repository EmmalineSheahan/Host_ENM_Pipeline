# creating accessible area polygons per species for introduced group

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
landmask <- terra::aggregate(as_spatvector(land))

# reading in species lists
int_list_final <- read.table('./base_data/int_modelled_species_list.txt')
int_list_final <- int_list_final$V1

# creating directories for accessible area shapefiles
dir.create('./int_acc_area')

# creating various accessible areas of buffer distances 3, 4, 5, and 6 times larger
# than the area of the coordinates, and then running initial models to select the best
# buffer for each species via AIC
buff_props <- c(3, 4, 5, 6)

# pulling in environmental data
all_envs <- rast('./env_data/all_envs.tif')

# creating accessible area polygons nat species
drawqgis <- integer()

  tryCatch(
    {
      # creating the different buffers
      temp_spec_mat <- read.csv(paste0('./int_data/', int_list_final[task_id], '.csv'))
      temp_spec_mat <- temp_spec_mat[,2:4]
      temp_spec_sf <- st_as_sf(temp_spec_mat, 
                               coords = c("decimalLongitude", "decimalLatitude"),
                               crs = target_crs)
      temp_spec_sf <- st_make_valid(temp_spec_sf)
      
      # using getDynamicAlphaHull with s2 turned on first
      rangebuilder_malfunction_on <- integer()
      tryCatch(
        {
          temp_alph <- getDynamicAlphaHull(data.frame(st_coordinates(temp_spec_sf)), 
                                           fraction = 1, partCount = 1, 
                                           initialAlpha = 40, 
                                           clipToCoast = "terrestrial",
                                           coordHeaders = c("X", 
                                                            "Y"), 
                                           buff = 100000)
          temp_alph <- temp_alph[[1]]
        }, 
        error=function(err){
          message('On iteration ',task_id, ' there was an error: ',err)
          rangebuilder_malfunction_on <<-c(rangebuilder_malfunction,task_id)
        }
      )
      
      if(task_id %in% rangebuilder_malfunction_on) {
        rangebuilder_malfunction <- integer()
        sf_use_s2(F)
        tryCatch(
          {
            temp_alph <- getDynamicAlphaHull(data.frame(st_coordinates(temp_spec_sf)), 
                                           fraction = 1, partCount = 1, 
                                           initialAlpha = 40, 
                                           clipToCoast = "terrestrial",
                                           coordHeaders = c("X", 
                                                            "Y"), 
                                           buff = 100000)
            temp_alph <- temp_alph[[1]]
          }, 
          error=function(err){
            message('On iteration ',task_id, ' there was an error: ',err)
            rangebuilder_malfunction <<-c(rangebuilder_malfunction,task_id)
          }
       )
      
        # using st_convex_hull when rangebuilder doesn't work
        if(i %in% rangebuilder_malfunction) {
          temp_alph <- st_convex_hull(x = st_union(temp_spec_sf))
          temp_alph <- st_make_valid(temp_alph)
        }
      }
      
      # creating buffered shapes
      shape_list <- vector("list", length = length(buff_props))
      for (j in seq_along(buff_props)) {
        buffDist <- (sqrt(buff_props[j]*expanse(as_spatvector(temp_alph), 
                                                unit = "m")) - 
                       sqrt(expanse(as_spatvector(temp_alph), unit = "m")))/2
        newshape <- buffer(as_spatvector(temp_alph), width = buffDist)
        shape_new_clipped <- terra::intersect(newshape, landmask)
        if(is.list(shape_new_clipped)) {
          shape_new_clipped <- shape_new_clipped[[1]]
        }
        wholemap <- rast(xmin = -180, xmax = 180, ymin = -90, ymax = 90, 
                         crs = target_crs)
        wholemap_area <- expanse(wholemap)[1,2]*0.8
        if(expanse(shape_new_clipped) > wholemap_area) {
          shape_list[[j]] <- NA
        } else {
          shape_list[[j]] <- shape_new_clipped
        }
      }
      
      print(paste0("buffers created for species ", int_list_final[task_id]))
      
      # j loop to crop rasters to each polygon, run ENMeval for each polygon, and store
      # model results for each polygon
      model_results <- vector(length = length(shape_list))
      model_results2 <- vector(length = length(shape_list))
      for (j in 1:length(shape_list)) {
        if(is.logical(shape_list[[j]])) {
          model_results[j] <- NA
        } else {
          cropped_envs <- crop(all_envs, shape_list[[j]])
          clipped_envs <- mask(cropped_envs, shape_list[[j]])
          tryCatch(
            {
              simple_model <- ENMevaluate(occs = temp_spec_mat[,1:2], 
                                          envs = clipped_envs, 
                                          algorithm = "maxnet",
                                          tune.args = list(fc = c("L"), rm = c(1)),
                                          partitions = "none", n.bg = 10000)
            }, 
            error=function(err){
              message('On iteration ',j, ' there was an error: ',err)
            }
          )
          mod_res <- min(eval.results(simple_model)$AICc) 
          if(is.na(mod_res)) {
            mod_res2 <- max(eval.results(simple_model)$auc.train)
            model_results2[j] <- mod_res2
          }
          model_results[j] <- mod_res
        }
      }
      
      print(paste0("models ran for species ", int_list_final[task_id]))
      
      # find which polygon produced the lowest AIC
      if(all(is.na(model_results))) {
        want_shape_num <- which(model_results2 == max(model_results2, na.rm = T))
        want_shape <- shape_list[[want_shape_num[1]]]
      } else {
        want_shape_num <- which(model_results == min(model_results, na.rm = T))
        want_shape <- shape_list[[want_shape_num[1]]]
      }
      
      # which buffer was it
      needed_buf <- buff_props[want_shape_num]
      
      # write that polygon to the acc area directory
      st_write(st_as_sf(want_shape), dsn = './int_acc_area', 
               layer = int_list_final[task_id], 
               driver = "ESRI Shapefile",
               append = F)
      
      print(paste0("acc area file created for species ", int_list_final[task_id]))
      
    }, 
    error=function(err){
      message('On iteration ',task_id, ' there was an error: ',err)
      drawqgis <<- c(drawqgis, task_id)
    }
  )


drawthese <- int_list_final[drawqgis]
write.table(drawthese, 
            file = './base_data/int_qgis_list.txt', 
            row.names = F,
            col.names = F,
            append = T)