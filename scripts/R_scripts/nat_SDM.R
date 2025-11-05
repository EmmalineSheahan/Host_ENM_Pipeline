# Maxent modelling of nat species

library(sf)
library(terra)
library(tidyterra)
library(dplyr)
library(predicts)
library(ENMeval)
library(geodata)
library(usdm)
library(ggplot2)

source('./scripts/R_scripts/var_remove_and_select.R')

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
nat_sp <- read.table('./base_data/nat_modelled_species_list.txt')
nat_sp <- nat_sp$V1

# creating necessary directories
dir.create('./results/nat_SDM_suit_rasters')
dir.create('./results/nat_SDM_thresh_rasters')
dir.create('./results/nat_SDM_parameters')
dir.create('./results/nat_SDM_selected_variables')
dir.create('./results/nat_SDM_TSS')
dir.create('./plots/occ_blocks')
dir.create('./plots/bg_blocks')
dir.create('./plots/nat_SDM_suit')
dir.create('./plots/nat_SDM_thresh')

# beginning array

# reading in occurrences
occs <- read.csv(paste0('./occ_data/', nat_sp[task_id], '.csv'))
occs <- occs[,-1]

# reading in accessible area
acc_area <- st_read(dsn = './nat_acc_area', layer = nat_sp[task_id])
acc_area <- st_set_crs(acc_area, target_crs)
acc_area <- as_spatvector(acc_area)

# cropping environmental variables to accessible area
envs_crop <- crop(envs, acc_area)
envs_mask <- mask(envs_crop, acc_area)

# creating background
acc_size <- length(which(!(is.na(values(envs_mask[[1]])))))
if(acc_size < 10000) {
  initial_try <- 5
  tt <- tryCatch(
    {
    bgs <- backgroundSample(mask = envs_mask, n = round((acc_size*0.9)), p = occs[,1:2],
                          excludep = T)
    }, error = function(w) w)
  while(is(tt, "error")) {
    tt <- tryCatch(
      {
        bgs <- backgroundSample(mask = envs_mask, n = round((acc_size*0.9)), 
                                p = occs[,1:2],
                                excludep = T)
      }, error = function(w) w)
    initial_try <- initial_try+1
  }
} else {
  initial_try <- 5
  tt <- tryCatch(
    {
      bgs <- backgroundSample(mask = envs_mask, n = 10000, p = occs[,1:2],
                              excludep = T, tryf = initial_try)
    }, warning = function(w) w)
  while(is(tt, "warning")) {
  tt <- tryCatch(
    {
    bgs <- backgroundSample(mask = envs_mask, n = 10000, p = occs[,1:2],
                          excludep = T, tryf = initial_try)
    }, warning = function(w) w)
  initial_try <- initial_try+1
  }
}
bgs <- data.frame(bgs)
colnames(bgs) <- colnames(occs[,1:2])

# variable selection
final_envs <- var_select(wanted_envs = envs_mask, wanted_occs = occs[,1:2],
                         wanted_bg = bgs)
writeRaster(final_envs, filename = paste0('./results/nat_SDM_selected_variables/',
                                          nat_sp[task_id], "_selected_vars.tif"),
            overwrite = T)

# model training and validation
# get block spatial partition
block_partition <- get.block(occs = occs[,1:2], bg = bgs, orientation = "lat_lon")

occs_block <- cbind(occs, block_partition$occs.grp)
colnames(occs_block) <- c("decimalLongitude", "decimalLatitude", "species",
                          "block")
occs_block$block <- as.factor(occs_block$block)
bgs_block <- cbind(bgs, block_partition$bg.grp)
colnames(bgs_block) <- c("decimalLongitude", "decimalLatitude",
                          "block")
bgs_block$block <- as.factor(bgs_block$block)

p <- ggplot() +
  geom_sf(data = land, fill = "darkgrey") +
  geom_spatraster(data = envs_mask[[1]], show.legend = F) +
  scale_fill_gradient2(high = "red", mid = "yellow", low = "white",
                       midpoint = max(values(envs_mask[[1]]), na.rm = T)/2,
                       na.value = "transparent") +
  geom_point(data = occs_block, aes(x = decimalLongitude, y = decimalLatitude,
                                    colour = block), size = 2, pch = 19) +
  scale_colour_manual(values = c("#A3D642", "#42D6C0", "#7642D6", "#D64258")) +
  coord_sf(xlim = c(((ext(envs_mask[[1]])[1])-10), 
                    ((ext(envs_mask[[1]])[2])+10)),
           ylim = c(((ext(envs_mask[[1]])[3])-10),
                    ((ext(envs_mask[[1]])[4])+10)), expand = F) +
  ggtitle(paste0("Occurrence Block Partition for ", gsub("_", ' ', nat_sp[task_id]))) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank())
pdf(paste0('./plots/occ_blocks/', nat_sp[task_id], "_occ_block_partition.pdf"))
print(p)
dev.off()

p <- ggplot() +
  geom_sf(data = land, fill = "darkgrey") +
  geom_spatraster(data = envs_mask[[1]], show.legend = F) +
  scale_fill_gradient2(high = "red", mid = "yellow", low = "white",
                       midpoint = max(values(envs_mask[[1]]), na.rm = T)/2,
                       na.value = "transparent") +
  geom_point(data = bgs_block, aes(x = decimalLongitude, y = decimalLatitude,
                                    colour = block), size = 2, pch = 19) +
  scale_colour_manual(values = c("#A3D642", "#42D6C0", "#7642D6", "#D64258")) +
  coord_sf(xlim = c(((ext(envs_mask[[1]])[1])-10), 
                    ((ext(envs_mask[[1]])[2])+10)),
           ylim = c(((ext(envs_mask[[1]])[3])-10),
                    ((ext(envs_mask[[1]])[4])+10)), expand = F) +
  ggtitle(paste0("Background Block Partition for ", gsub("_", ' ', nat_sp[task_id]))) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank())
pdf(paste0('./plots/bg_blocks/', nat_sp[task_id], "_bg_block_partition.pdf"))
print(p)
dev.off()

# train model
user.grp_block <- list(occs.grp = block_partition$occs.grp, 
                       bg.grp = block_partition$bg.grp)
modcatch <- tryCatch(
  {
  mod1 <- ENMevaluate(occs = occs[,1:2], envs = final_envs, bg = bgs,
                      algorithm = "maxnet",
                      tune.args = list(fc = c("L", "LQ", "LQH", "H", "LQHP"), 
                                       rm = 1:4),
                      partitions = 'user', user.grp = user.grp_block)
}, error=function(err) err)

if(is(modcatch, "error")) {
  mod1 <- ENMevaluate(occs = occs[,1:2], envs = final_envs, bg = bgs,
                      algorithm = "maxnet",
                      tune.args = list(fc = c("L", "LQ", "LQH"), 
                                       rm = 1:3),
                      partitions = 'user', user.grp = user.grp_block)
}

if (any(eval.results(mod1)$auc.val.avg) < 0.7) {
  select_this <- which(eval.results(mod1)$auc.val.avg == 
                         max(eval.results(mod1)$auc.val.avg, na.rm = T))
  if(length(select_this) > 1) {
    select_this <- select_this[1]
  }
  best_sdm <- mod1@predictions[[select_this]]
  best_sdm_tune <- mod1@tune.settings[select_this,]
} else {
  select_this <- which(eval.results(mod1)$AICc == min(eval.results(mod1)$AICc, 
                                                      na.rm = T))
  if(length(select_this) > 1) {
    select_this <- select_this[1]
  }
  best_sdm <- mod1@predictions[[select_this]]
  best_sdm_tune <- mod1@tune.settings[select_this,]
}
write.table(c(best_sdm_tune, names(final_envs)), 
            file = paste0('./results/nat_SDM_parameters/', 
                          nat_sp[task_id], '.txt'),
            row.names = F, col.names = F)
names(best_sdm) <- nat_sp[task_id]

# threshold model
percentiles <- c(0.99, 0.95, 0.9, 0.85, 0.8, 0.75)
presences <- occs[,1:2]
absences <- bgs
sdm_suit1 <- extract(best_sdm, presences)
sdm_suit <- sdm_suit1[,ncol(sdm_suit1)][order(sdm_suit1[,ncol(sdm_suit1)], 
                                na.last = NA,
                                decreasing = T)]

# testing different sensitivities
thresh_sdm_list <- vector("list", length = length(percentiles))
tss_list <- vector(length = length(percentiles))
spec_list <- vector(length = length(percentiles))
for (i in seq_along(percentiles)) {
  thresh <- round(length(sdm_suit)*percentiles[i])
  thresh_val <- sdm_suit[thresh]
  rclmat <- matrix(data = c(0, thresh_val, 0, 
                            thresh_val, 1, 1), nrow = 2, ncol = 3, byrow = T)
  thresholded_sdm <- classify(best_sdm, rcl = rclmat)
  thresh_sdm_list[[i]] <- thresholded_sdm
  real_abs <- extract(thresholded_sdm, absences)
  specificity <- length(which(real_abs[,2] == 0))/length(real_abs[,2])
  sensitivity <- percentiles[i]
  tss <- (sensitivity + (specificity)) - 1
  tss_list[i] <- tss
  spec_list[i] <- specificity
}
tss_df <- data.frame(percentiles, spec_list, tss_list)
colnames(tss_df) <- c("sensitivity", "specificity", "TSS")
write.csv(tss_df, file = paste0('./results/nat_SDM_TSS/', nat_sp[task_id], "_tss.csv"))

choose_final <- which(tss_list == max(tss_list))
final_sdm <- thresh_sdm_list[[choose_final]]
names(final_sdm) <- nat_sp[task_id]

# extending suitability and thresholded rasters to global extent
ext_suit <- extend(best_sdm, envs[[1]], snap = "out")
ext_suit <- crop(ext_suit, envs[[1]])
writeRaster(ext_suit, filename = paste0('./results/nat_SDM_suit_rasters/', 
                                        nat_sp[task_id], '.tif'),
            overwrite = T)

ext_thresh <- extend(final_sdm, envs[[1]], snap = "out")
ext_thresh <- crop(ext_thresh, envs[[1]])
writeRaster(ext_thresh, filename = paste0('./results/nat_SDM_thresh_rasters/', 
                                        nat_sp[task_id], '.tif'),
            overwrite = T)

# plotting suitability and thresholded rasters for manual checking
# suitability
pdf(paste0('./plots/nat_SDM_suit/', nat_sp[task_id], '_suitability.pdf'))
p <- ggplot() +
  geom_sf(data = land, fill = "darkgrey") +
  geom_spatraster(data = best_sdm) +
  scale_fill_gradient2(high = "yellow", mid = "#3DED97", low = "#8969CD",
                       midpoint = max(values(best_sdm), na.rm = T)/2,
                       na.value = "transparent", name = "Probability of Presence") +
  coord_sf(xlim = c(((ext(best_sdm)[1])-10), 
                    ((ext(best_sdm)[2])+10)),
           ylim = c(((ext(best_sdm)[3])-10),
                    ((ext(best_sdm)[4])+10)), expand = F) +
  ggtitle(paste0("Probability of Suitable Habitat for ", 
                 gsub("_", ' ', nat_sp[task_id]))) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        legend.title = element_text(size = 6),
        plot.title = element_text(size = 9),
        legend.text = element_text(size = 5),
        axis.text = element_text(size = 6))
print(p)
dev.off()

# threshold
pdf(paste0('./plots/nat_SDM_thresh/', nat_sp[task_id], '_threshold.pdf'))
p <- ggplot() +
  geom_sf(data = land, fill = "darkgrey") +
  geom_spatraster(data = final_sdm, show.legend = F) +
  scale_fill_gradient(high = "#FFC000", low = "transparent",
                       na.value = "transparent") +
  coord_sf(xlim = c(((ext(final_sdm)[1])-10), 
                    ((ext(final_sdm)[2])+10)),
           ylim = c(((ext(final_sdm)[3])-10),
                    ((ext(final_sdm)[4])+10)), expand = F) +
  ggtitle(paste0("Predicted Presence for ", 
                 gsub("_", ' ', nat_sp[task_id]))) +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"),
        axis.title = element_blank(),
        plot.title = element_text(size = 9),
        axis.text = element_text(size = 6))
print(p)
dev.off()

