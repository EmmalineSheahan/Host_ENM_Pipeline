# script to pull occurrences records down from databases, clean records, and thin records
library(rgbif)
library(ridigbio)
library(dplyr)
library(predicts)
library(CoordinateCleaner)
library(sf)
library(stringr)

# read in host pathogen range csv
host_data <- read.csv('./base_data/Wheat_pathogen_host_range.csv')

# get unique list of host species
host_list <- unique(host_data$Hosts)
two_names <- grep(' ', host_list)
host_list <- host_list[two_names]

# remove known crop species
crop_species <- read.csv('./base_data/crop-species-list.csv')
remcrop <- which(host_list %in% crop_species$sci_name)
host_list <- host_list[-remcrop]

# writing host_list to file
write.table(host_list, file = './base_data/host_list.txt')

# for loop to pull down, clean, and thin occurrences for each species

for (i in seq_along(host_list)) {
  
  # retrieving GBIF data
  gbif_data <- occ_data(scientificName = host_list[i], hasCoordinate = T,
                         coordinateUncertaintyInMeters = '0,10000', limit = 100000)
  gbif_data <- data.frame(gbif_data$data)
  gbif_data <- gbif_data %>% dplyr::select(scientificName, decimalLatitude, 
                                           decimalLongitude,
                                         country, stateProvince, eventDate)
  
  # adjusting eventDate to match format with idigbio
  gbif_data$eventDate <- gsub("T.*", '', gbif_data$eventDate)
  
  # retrieving idigbio data
  idigbio_data <- idig_search_records(rq = list("scientificname" = host_list[i], 
                                                "geopoint" = list("type" = "exists")), 
                                      fields = "all")
  idigbio_data <- idigbio_data %>% dplyr::select(scientificname, geopoint.lat, 
                                                 geopoint.lon,
                                                 country, stateprovince, datecollected, 
                                                 coordinateuncertainty)
  
  # removing points with too high a coordinate uncertainty
  remove_rows <- which(idigbio_data$coordinateuncertainty > 10000)
  idigbio_data <- idigbio_data[-remove_rows,]
  idigbio_data <- idigbio_data[,1:6]
  colnames(idigbio_data) <- colnames(gbif_data)
  
  # adjusting eventDate to match gbif
  idigbio_data$eventDate <- gsub("T.*", '', idigbio_data$eventDate)
  
  # merging data
  all_data <- merge(gbif_data, idigbio_data, all.x = T, all.y = T)
  
  # ensuring all coordinates are WGS84
  rem1 <- which(all_data$decimalLatitude < -90)
  rem2 <- which(all_data$decimalLatitude > 90)
  rem3 <- which(all_data$decimalLongitude < -180)
  rem4 <- which(all_data$decimalLongitude > 180)
  rem_rows <- c(rem1, rem2, rem3, rem4)
  if(length(rem_rows) > 0) {
    all_data <- all_data[-rem_rows,]
  }
  
  # removing duplicate records
  temp_data <- all_data %>% dplyr::select(decimalLatitude, decimalLongitude, eventDate)
  rem_dups <- which(duplicated(temp_data))
  if(length(rem_dups) > 0) {
    all_data <- all_data[-rem_dups,] 
  }
  
  # removing outside natural range
  
  # removing anything in the water
  
  # removing botanical gardens
  
  # removing outliars
  
  # spatial thinning
  
  
}
