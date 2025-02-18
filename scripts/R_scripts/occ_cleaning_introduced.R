# script to pull occurrence records down from databases, clean records, and thin records
library(rgbif)
library(ridigbio)
library(dplyr)
library(predicts)
library(CoordinateCleaner)
library(sf)
library(stringr)
library(spThin)
library(rnaturalearthhires)
library(wdpar)

# setting up array
start_num <- as.numeric(Sys.getenv("START_NUM"))
task_id <- as.numeric(start_num)
part <- paste0("part", task_id)

# creating land
target_crs <- "+proj=longlat +datum=WGS84"
land <- st_as_sf(rnaturalearthhires::countries10)[1] %>% st_set_crs(target_crs)

# reading in needed lists
introduced_list <- read.table('./base_data/introduced_list.txt')
introduced_list <- introduced_list$x
subsamp_list <- read.table('./base_data/final_finish_list.txt')
subsamp_list <- subsamp_list$x

# creating name lists used in for loop
int_names <- gsub(" ", "_", introduced_list)
int_nats <- list.files('./int_ranges')
int_nats <- gsub('.shp', '', int_nats)
int_nats <- gsub('.shx', '', int_nats)
int_nats <- gsub('.dbf', '', int_nats)
int_nats <- gsub('.prj', '', int_nats)
int_nats <- unique(int_nats)

# array to pull down, clean, and thin occurrences for each species

print("beginning array")

# retrieving GBIF data
gbif_data <- occ_data(scientificName = introduced_list[task_id], hasCoordinate = T,
                      coordinateUncertaintyInMeters = '0,10000', limit = 100000)
gbif_data <- data.frame(gbif_data$data)
if(nrow(gbif_data) > 0) {
  if(any(colnames(gbif_data) %in% "modified")) {
    gbif_data <- gbif_data %>% dplyr::select(scientificName, decimalLatitude, 
                                             decimalLongitude,
                                             country, modified)
  } else {
    gbif_data <- gbif_data %>% dplyr::select(scientificName, decimalLatitude, 
                                             decimalLongitude,
                                             country, eventDate)
  }
  
  colnames(gbif_data) <- c("scientificName", "decimalLatitude", 
                           "decimalLongitude",
                           "country", "eventDate")    
  print(paste0("Gbif data downloaded for iteration ", task_id))
  
  # adjusting eventDate to match format with idigbio
  gbif_data$eventDate <- gsub("T.*", '', gbif_data$eventDate)
  
} else {
  print(paste0("no gbif data for ", introduced_list[task_id]))
}

# retrieving idigbio data
idigbio_data <- idig_search_records(rq = list("scientificname" = introduced_list[task_id], 
                                              "geopoint" = list("type" = "exists")), 
                                    fields = "all")
if(nrow(idigbio_data) > 0) {
  idigbio_data <- idigbio_data %>% dplyr::select(scientificname, geopoint.lat, 
                                                 geopoint.lon,
                                                 country, datecollected, 
                                                 coordinateuncertainty)
  print(paste0("idigbio data downloaded for iteration ", task_id))
  
  # removing points with too high a coordinate uncertainty
  remove_rows <- which(idigbio_data$coordinateuncertainty > 10000)
  idigbio_data <- idigbio_data[-remove_rows,]
  idigbio_data <- idigbio_data[,1:5]
  colnames(idigbio_data) <- c("scientificName", "decimalLatitude", 
                              "decimalLongitude",
                              "country", "eventDate")
  
  # adjusting eventDate to match gbif
  idigbio_data$eventDate <- gsub("T.*", '', idigbio_data$eventDate)
  
} else {
  print(paste0("no idigbio data for ", introduced_list[task_id]))
}

# merging data

if(nrow(gbif_data) > 0 & nrow(idigbio_data) > 0) {
  all_data <- merge(gbif_data, idigbio_data, all.x = T, all.y = T)
} else if(nrow(gbif_data) > 0 & !(nrow(idigbio_data) > 0)) {
  all_data <- gbif_data
} else if(nrow(idigbio_data) > 0 & !(nrow(gbif_data) > 0)) {
  all_data <- idigbio_data
} else {
  all_data <- NA
  print(paste0("no records available for ", host_list[task_id]))
}

if(any(!(is.na(all_data)))) {
  
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
  
  # removing unnecessary columns
  all_data <- all_data[,2:3]
  
  print(paste0("beginning outside range exclusion on iteration ", task_id))
  
  # removing outside natural range
  species <- rep(introduced_list[task_id], times = nrow(all_data))
  all_data <- cbind(all_data, species)
  
  if(int_names[task_id] %in% int_nats) {
    natrange <- st_read(paste0('./int_ranges/', int_names[task_id], 
                               '.shp')) %>% st_set_crs(target_crs)
    natrange <- st_set_crs(natrange, target_crs)
    flag_range <- integer()
    tryCatch(
      {
        natrange <- st_repair_geometry(natrange)
        nat_data <- cc_iucn(x = all_data, range = natrange, buffer = 50000, 
                            lon = "decimalLongitude", lat = "decimalLatitude",
                            species = "species")
      },
      error=function(err){
        message('On iteration ',task_id, ' there was an error: ',err)
        flag_range <<-c(flag_range,task_id)
      }
    )
    if(length(flag_range) > 0) {
      flag_range2 <- integer()
      tryCatch(
        {
          natrange <- st_buffer(natrange, 0.0)
          nat_data <- cc_iucn(x = all_data, range = natrange, buffer = 50000, 
                              lon = "decimalLongitude", lat = "decimalLatitude",
                              species = "species")
        },
        error=function(err){
          message('On iteration ',task_id, ' there was an error: ',err)
          flag_range2 <<-c(flag_rang2,task_id)
        }
      )
      if(length(flag_range2) > 0) {
        nat_data <- NULL
      }
    }
  } else {
    nat_data <- NULL
  }
  
  if(is.null(nat_data)) {
    print("all coordinates are outside of introduced range")
  } else {
    
    # removing anything in the water
    nosea_data <- cc_sea(nat_data, lon = "decimalLongitude", lat = "decimalLatitude")
    
    if(nrow(nosea_data) == 0) {
      print("all data lost at sea")
    } else {
      
      # removing botanical gardens
      nobot_data <- cc_inst(nosea_data, lon = "decimalLongitude", lat = "decimalLatitude",
                            species = "species")
      
      if(nrow(nobot_data) == 0) {
        print("all data localities are institutions")
      } else {
        
        # removing outliers
        if(nrow(nobot_data) > 20) {
          noout_data <- cc_outl(nobot_data, lon = "decimalLongitude", 
                                lat = "decimalLatitude", species = "species", 
                                method = "distance", value = "clean", 
                                tdi = 1000)
        } else {
          noout_data <- nobot_data
        }
        
        if(introduced_list[task_id] %in% subsamp_list) {
          subsamp <- sample(x = 1:nrow(noout_data), size = (0.1*nrow(noout_data)))
          noout_data <- noout_data[subsamp,]
        }
        
        # spatial thinning
        thinned_data <- thin(loc.data = noout_data, lat.col = "decimalLatitude", 
                             long.col = "decimalLongitude", spec.col = "species", 
                             thin.par = 10,
                             reps = 10, 
                             write.files = F, locs.thinned.list.return = T)
        maxnumlist <- vector(length = length(thinned_data))
        for(j in 1:length(thinned_data)) {
          maxnumlist[j] <- nrow(thinned_data[[j]])
        }
        maxnum <- which(maxnumlist == max(maxnumlist))[1]
        thinned_data <- thinned_data[[maxnum]]
        species <- rep(introduced_list[task_id], times = nrow(thinned_data))
        thinned_data <- cbind(thinned_data, species)
        colnames(thinned_data) <- c("decimalLongitude", "decimalLatitude", "species")
        if(nrow(thinned_data) < 7) {
          thinned_data <- data.frame(noout_data$decimalLongitude, noout_data$decimalLatitude,
                                     noout_data$species)
          colnames(thinned_data) <- c("decimalLongitude", "decimalLatitude", "species")
        }
        
        # writing to file
        write.csv(thinned_data, file = paste0('./int_data/', int_names[task_id], '.csv'))
      }
    }
  } 
}
