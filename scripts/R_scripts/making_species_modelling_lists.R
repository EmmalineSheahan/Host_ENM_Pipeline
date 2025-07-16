# creating lists of species with enough points for modelling, species with enough points
# for buffering, and dropped species

library(dplyr)

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

