# creating natural range polygons
library(terra)
library(sf)
library(rnaturalearth)
library(dplyr)

# creating nat_ranges directory for output files
dir.create('./nat_ranges')

# read in host list
host_list <- read.table('./base_data/host_list.txt')
host_list <- host_list[,1]

# read in wcvp dist and clean
wcvp_names <- read.table('./base_data/wcvp_names.csv', sep = "|", header = T,
                         na.strings = "", strip.white = F, fill = T, as.is = 1)
wcvp_dist <- read.table('./base_data/wcvp_distribution.csv', sep = "|", header = T)
wcvp_names$plant_name_id <- as.numeric(wcvp_names$plant_name_id)
wcvp_dist$plant_name_id <- as.numeric(wcvp_dist$plant_name_id)
wcvp <- left_join(wcvp_names, wcvp_dist, by = "plant_name_id", 
                  relationship = "many-to-many")
wanted_hosts <- which(wcvp$taxon_name %in% host_list)
wcvp_hosts <- wcvp[wanted_hosts,]
wcvp_hosts <- wcvp_hosts %>% dplyr::select(taxon_name, continent, region, area, introduced,
                                           location_doubtful, geographic_area)

# remove introduced localities
rem_int <- which(wcvp_hosts$introduced == 1)
wcvp_hosts <- wcvp_hosts[-rem_int,]

# renaming for already written code
wcvp_dist <- wcvp_hosts

# adjusting wcvp character strings to match rnaturalearth
wcvp_dist$area <- gsub('Northeastern ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Northwestern ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Southwestern ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Southeastern ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Northeast ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Northwest ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Southwest ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Southeast ', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' Northeast', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' Southeast', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' Northwest', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' Southwest', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' West-Central', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' South-Central', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' East-Central', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' North-Central', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' North', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' South', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' East', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' West', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' Central', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Western ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Northern ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Eastern ', '', wcvp_dist$area)
wcvp_dist$area <- gsub('Southern ', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' Inner', '', wcvp_dist$area)
wcvp_dist$area <- gsub(' Gulf', '', wcvp_dist$area)

# find working countries
locality <- unique(wcvp_dist$area)
flag <- integer()
for (i in 1:length(locality)) {
  tryCatch(
    {
      testing <- ne_countries(country = locality[i], returnclass = "sf", scale = 50)
    }, 
    error=function(err){
      message('On iteration ',i, ' there was an error: ',err)
      flag <<-c(flag,i)
    }
  )
}

problem_loc <- locality[flag]
working_countries <- locality[-flag]

still_problems <- problem_loc

# matching as many states to countries as possible
all_countries <- ne_countries(returnclass = "sf", scale = 50)
all_countries <- all_countries$sovereignt
for (i in seq_along(all_countries)) {
  allflag <- integer()
  for (j in 1:length(still_problems)) {
    tryCatch(
      {
        testing <- ne_states(country = all_countries[i], returnclass = "sf")[9]
        newtest <- testing %>% filter(name == still_problems[j])
        plot(newtest)
      }, 
      error=function(err){
        message('On iteration ',j, ' there was an error: ',err)
        allflag <<-c(allflag,j)
      }
    )
  }
  still_problems <- still_problems[allflag]
}

still_problems <- still_problems[order(still_problems)]

# manual fixing
wcvp_dist$area <- gsub("Aldabra", "Seychelles", wcvp_dist$area)
wcvp_dist$area <- gsub("Aleutian Is.", "Alaska", wcvp_dist$area)
wcvp_dist$area <- gsub("Andaman Is.", "Andaman and Nicobar", wcvp_dist$area)
wcvp_dist$area <- gsub("Ascension", "Saint Helena", wcvp_dist$area)
wcvp_dist$area <- gsub("Bahamas", "The Bahamas", wcvp_dist$area)
wcvp_dist$area <- gsub("Bismarck Archipelago", "Indonesia", wcvp_dist$area)
wcvp_dist$area <- gsub("Borneo", "Indonesia", wcvp_dist$area)
wcvp_dist$area <- gsub("Burkina", "Burkina Faso", wcvp_dist$area)
wcvp_dist$area <- gsub("Buryatiya", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("Canary Is.", "Spain", wcvp_dist$area)
wcvp_dist$area <- gsub("Cape Provinces", "South Africa", wcvp_dist$area)
wcvp_dist$area <- gsub("Caprivi Strip", "Namibia", wcvp_dist$area)
wcvp_dist$area <- gsub("Caroline Is.", "Federated States of Micronesia", wcvp_dist$area)
wcvp_dist$area <- gsub("Cayman Is.", "Cayman Islands", wcvp_dist$area)
wcvp_dist$area <- gsub("Central African Repu", "Central African Republic", wcvp_dist$area)
wcvp_dist$area <- gsub("Central European Rus", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("Chatham Is.", "New Zealand", wcvp_dist$area)
wcvp_dist$area <- gsub("Christmas I.", "Indian Ocean Territories", wcvp_dist$area)
wcvp_dist$area <- gsub("Congo", "Democratic Republic of the Congo", wcvp_dist$area)
wcvp_dist$area <- gsub("Corse", "France", wcvp_dist$area)
wcvp_dist$area <- gsub("Czechoslovakia", "Czech Republic", wcvp_dist$area)
wcvp_dist$area <- gsub("East European Russia", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("European R", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("Føroyar", "Faroe Islands", wcvp_dist$area)
wcvp_dist$area <- gsub("French Guiana", "Guyane française", wcvp_dist$area)
wcvp_dist$area <- gsub("Great Britain", "United Kingdom", wcvp_dist$area)
wcvp_dist$area <- gsub("Guinea-Bissau", "Guinea Bissau", wcvp_dist$area)
wcvp_dist$area <- gsub("Gulf of Guinea Is.", "Papua New Guinea", wcvp_dist$area)
wcvp_dist$area <- gsub("Inner Mongolia", "Mongolia", wcvp_dist$area)
wcvp_dist$area <- gsub("Jawa", "Indonesia", wcvp_dist$area)
wcvp_dist$area <- gsub("Juan Fernández Is.", "Chile", wcvp_dist$area)
wcvp_dist$area <- gsub("Kazan-retto", "Japan", wcvp_dist$area)
wcvp_dist$area <- gsub("Kermadec Is.", "New Zealand", wcvp_dist$area)
wcvp_dist$area <- gsub("Kirgizstan", "Kyrgyzstan", wcvp_dist$area)
wcvp_dist$area <- gsub("Kuril Is.", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("Labrador", "Canada", wcvp_dist$area)
wcvp_dist$area <- gsub("Laccadive Is.", "India", wcvp_dist$area)
wcvp_dist$area <- gsub("Lesser Sunda Is.", "Indonesia", wcvp_dist$area)
wcvp_dist$area <- gsub("Magadan", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("Malaya", "Singapore", wcvp_dist$area)
wcvp_dist$area <- gsub("Marianas", "Northern Mariana Islands", wcvp_dist$area)
wcvp_dist$area <- gsub("Marquesas", "Marquesas Islands", wcvp_dist$area)
wcvp_dist$area <- gsub("Mexican Pacific Is.", NA, wcvp_dist$area)
wcvp_dist$area <- gsub("Nansei-shoto", "Okinawa", wcvp_dist$area)
wcvp_dist$area <- gsub("Netherlands Antilles", "Aruba", wcvp_dist$area)
wcvp_dist$area <- gsub("New Guinea", "Papua New Guinea", wcvp_dist$area)
wcvp_dist$area <- gsub("New Wales", "New South Wales", wcvp_dist$area)
wcvp_dist$area <- gsub("Newfoundland", "Newfoundland and Labrador", wcvp_dist$area)
wcvp_dist$area <- gsub("Nicobar Is.", "Andaman and Nicobar", wcvp_dist$area)
wcvp_dist$area <- gsub("Norfolk Is.", "Norfolk Island", wcvp_dist$area)
wcvp_dist$area <- gsub("North Caucasus", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("North European Russi", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("Ogasawara-shoto", "Tokyo", wcvp_dist$area)
wcvp_dist$area <- gsub("Panamá", "Panama", wcvp_dist$area)
wcvp_dist$area <- gsub("Primorye", "Primor'ye", wcvp_dist$area)
wcvp_dist$area <- gsub("Prince Edward I.", "Prince Edward Islands", wcvp_dist$area)
wcvp_dist$area <- gsub("Provinces", NA, wcvp_dist$area)
wcvp_dist$area <- gsub("Réunion", "Reunion", wcvp_dist$area)
wcvp_dist$area <- gsub("Rhode I.", "Rhode Island", wcvp_dist$area)
wcvp_dist$area <- gsub("Sardegna", "Italy", wcvp_dist$area)
wcvp_dist$area <- gsub("Selvagens", "Portugal", wcvp_dist$area)
wcvp_dist$area <- gsub("Sicilia", "Italy", wcvp_dist$area)
wcvp_dist$area <- gsub("Sinai", "Egypt", wcvp_dist$area)
wcvp_dist$area <- gsub("Society Is.", "French Polynesia", wcvp_dist$area)
wcvp_dist$area <- gsub("Socotra", "Yemen", wcvp_dist$area)
wcvp_dist$area <- gsub("Solomon Is.", "Solomon Islands", wcvp_dist$area)
wcvp_dist$area <- gsub("South European Russi", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("Tadzhikistan", "Tajikistan", wcvp_dist$area)
wcvp_dist$area <- gsub("Tanzania", "United Republic of Tanzania", wcvp_dist$area)
wcvp_dist$area <- gsub("Territorie", NA, wcvp_dist$area)
wcvp_dist$area <- gsub("Territory", NA, wcvp_dist$area)
wcvp_dist$area <- gsub("Tibet", "China", wcvp_dist$area)
wcvp_dist$area <- gsub("Trinidad-Tobago", "Trinidad and Tobago", wcvp_dist$area)
wcvp_dist$area <- gsub("Tuamotu", "French Polynesia", wcvp_dist$area)
wcvp_dist$area <- gsub("Turkey-in-Europe", "Turkey", wcvp_dist$area)
wcvp_dist$area <- gsub("Turks-Caicos Is.", "Turks and Caicos Islands", wcvp_dist$area)
wcvp_dist$area <- gsub("Venezuelan Antilles", "Venezuela", wcvp_dist$area)
wcvp_dist$area <- gsub("West Siberia", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("Yakutskiya", "Russia", wcvp_dist$area)
wcvp_dist$area <- gsub("Zaïre", "Democratic Republic of the Congo", wcvp_dist$area)

multiproblem <- c("Baltic States", "Caribbean", "Central American Pac", "East Aegean Is.",
                  "East Himalaya", "Gulf States", "Korea", "Krym", "Lebanon-Syria",
                  "Leeward Is.", "Manchuria", "Sahara", "South China Sea",
                  "Sulawesi", "Sumatera", "Transcaucasus", "West Himalaya",
                  "Windward Is.", "Yugoslavia")
fix_problem <- list(c("Estonia", "Latvia", "Lithuania"),
                    c("Anguilla", "Antigua and Barbuda", "Aruba", "The Bahamas", 
                      "Virgin Islands", "Cayman Islands", "Cuba", "Dominica",
                      "Grenada", "Guadeloupe", "Haiti", "Jamaica", "Martinique", 
                      "Montserrat", "Saint Barthelemy", "Saint Kitts and Nevis"),
                    c("Costa Rica", "Columbia"), 
                    c("Greece", "Turkey"),
                    c("Nepal", "India", "Bhutan", "China", "Myanmar"),
                    c("Kuwait", "Bahrain", "Qatar", "Oman"),
                    c("North Korea", "South Korea"),
                    c("Russia", "Ukraine"),
                    c("Lebanon", "Syria"),
                    c("Antigua and Barbuda,", "Guadeloupe", "Montserrat", 
                      "Saint Kitts and Nevis", "Saint Martin", "Virgin Islands"),
                    c("China", "Russia"),
                    c("Egypt", "Tunisia", "Niger", "Mauritania", "Chad", "Libya",
                      "Mali", "Algeria", "Sudan", "Senegal"),
                    c("Philippines", "Vietnam", "Malaysia"),
                    c("Sulawesi Utara", "Sulawesi Barat", "Sulawesi Selatan", 
                      "Sulawesi Tenggara", "Sulawesi Tengah"),
                    c("Sumatera Barat", "Sumatera Utara", "Sumatera Selatan"),
                    c("Armenia", "Azerbaijan", "Georgia"),
                    c("India", "Pakistan"),
                    c("Dominica", "Grenada", "Martinique", "Saint Lucia", "Saint Vincent"),
                    c("Bosnia and Herzegovina", "Croatia", "Montenegro",
                      "Slovenia", "Macedonia"))


# loop to create natural range polygons for each species
host_names <- gsub(" ", "_", host_list)

all_states <- ne_states(returnclass = "sf")[9]
all_states_name <- all_states$name
for (i in seq_along(host_list)) {
  wanted_localities <- wcvp_dist %>% filter(taxon_name == host_list[i])
  if(nrow(wanted_localities) > 0) {
  wanted_localities <- wanted_localities$area
  
  # finding multi-problem strings
  prob_string <- which(wanted_localities %in% multiproblem)
  if(length(prob_string) > 0) {
    fixnum <- which(multiproblem %in% wanted_localities[prob_string])
    fix_country <- which(unlist(fix_problem[fixnum]) %in% all_countries)
    fix_state <- which(!(unlist(fix_problem[fixnum]) %in% all_countries))
    if(length(fix_country > 0)) {
      fix_poly_country <- ne_states(country = unlist(fix_problem[fixnum])[fix_country], 
                          returnclass = 'sf')[9]
    } else {
      fix_poly_country <- NA
    }
    if(length(fix_state > 0)) {
      fix_poly_state <- all_states %>% filter(name %in% 
                                                unlist(fix_problem[fixnum])[fix_state])
    } else {
      fix_poly_state <- NA
    }
  } 
  
  # making country polygons
  countries_want <- which(wanted_localities %in% all_countries)
  if(length(countries_want) > 0) {
    countries_sf <- ne_states(country = wanted_localities[countries_want], 
                            returnclass = 'sf')[9]
  } else {
    countries_sf <- NA
  }

  # making state polygons
  leftover <- wanted_localities[-countries_want]
  remlef <- which(leftover %in% multiproblem)
  leftover <- leftover[-remlef]
  if(length(leftover) > 0) {
    states_sf <- all_states %>% filter(name %in% leftover)
  } else {
    states_sf <- NA
  }
  
  # combining polygons
  full_list <- list(fix_poly_country, fix_poly_state, countries_sf, states_sf)
  drop <- which(is.na(full_list))
  if(length(drop) > 0) {
    full_list <- full_list[-drop]
  }
  final_poly <- do.call(rbind, full_list)
  
  # writing to file
  st_write(final_poly, dsn = './nat_ranges', layer = paste0(host_names[i], '.shp'),
           driver = "ESRI Shapefile",
           append = F)
  }
}
