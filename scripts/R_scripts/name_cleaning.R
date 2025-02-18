# creating a cleaned host list

library(dplyr)
library(stringr)

# read in host pathogen range csv
host_data <- read.csv('./base_data/Wheat_pathogen_host_range.csv')

# get unique list of host species
host_list <- unique(host_data$Hosts)
two_names <- grep(' ', host_list)
host_list <- host_list[two_names]

# getting count of sp.
sp_count <- grep('sp.', host_list)
sps <- host_list[sp_count]

# remove known crop species
crop_species <- read.csv('./base_data/crop-species-list.csv')
remcrop <- which(host_list %in% crop_species$sci_name)
host_list <- host_list[-remcrop]
host_list <- unique(host_list)

# read in wcvp dist and clean
# remove synonyms (rgbif pulls points for the accepted name and all of its synonyms)
wcvp_names <- read.table('./base_data/wcvp_names.csv', sep = "|", header = T,
                         na.strings = "", strip.white = F, fill = T, as.is = 1)
wcvp_dist <- read.table('./base_data/wcvp_distribution.csv', sep = "|", header = T)
wcvp_names$plant_name_id <- as.numeric(wcvp_names$plant_name_id)
wcvp_dist$plant_name_id <- as.numeric(wcvp_dist$plant_name_id)
wcvp <- left_join(wcvp_names, wcvp_dist, by = "plant_name_id", 
                  relationship = "many-to-many")
wanted_hosts <- which(wcvp$taxon_name %in% host_list)
wcvp_hosts <- wcvp[wanted_hosts,]

syn_names <- which(wcvp_hosts$taxon_status == "Synonym")
synonyms <- unique(wcvp_hosts$taxon_name[syn_names])

rem_hosts <- wcvp_hosts$taxon_name[syn_names]
del_hosts <- which(host_list %in% rem_hosts)
host_list_clean <- host_list[-del_hosts]

wcvp_hosts <- wcvp_hosts[-syn_names,]
wcvp_hosts <- wcvp_hosts %>% dplyr::select(taxon_name, continent, region, area, introduced,
                                           location_doubtful, geographic_area)


# writing final host list
write.table(host_list_clean, file = './base_data/host_list.txt')

# find introduced localities and species
rem_int <- which(wcvp_hosts$introduced == 1)
wcvp_int <- wcvp_hosts[rem_int,]
introduced_list <- unique(wcvp_int$taxon_name)
write.table(introduced_list, file = './base_data/introduced_list.txt')
write.csv(wcvp_int, file = './base_data/introduced_locality.csv')

wcvp_native <- wcvp_hosts[-rem_int,]

# writing native species locality to file
write.csv(wcvp_native, file = './base_data/native_locality.csv')
