# maxent modelling of introduced species
# one set will be projected onto the introduced range, another set will be projected
# onto the combined introduced and natural range. natural range only is being produced
# in the nat_SDM script.

library(sf)
library(terra)
library(tidyterra)
library(dplyr)
library(predicts)
library(ENMeval)
library(geodata)
library(usdm)
library(ggplot2)

source('./scripts/var_remove_and_select.R')

# setting up array
start_num <- as.numeric(Sys.getenv("START_NUM"))
task_id <- as.numeric(start_num)
part <- paste0("part", task_id)

# creating land
target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

# reading in environmental data
envs <- rast('./env_data/all_envs.tif')

# creating species list
int_sp <- read.table('./base_data/int_modelled_species_list.txt')
int_sp <- int_sp$V1

# creating necessary directories
dir.create('./results/int_SDM_suit_rasters')
dir.create('./results/int_SDM_thresh_rasters')
dir.create('./results/int_SDM_parameters')
dir.create('./results/int_SDM_selected_variables')
dir.create('./results/int_SDM_TSS')
dir.create('./plots/occ_blocks_int')
dir.create('./plots/bg_blocks_int')
dir.create('./plots/int_SDM_suit')
dir.create('./plots/int_SDM_thresh')