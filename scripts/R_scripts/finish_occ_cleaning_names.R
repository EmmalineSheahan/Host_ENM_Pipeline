# creating a list of names to complete the occurrence cleaning which stopped due to a lack of memory

library(dplyr)

finished_hosts <- list.files('./occ_data')
finished_hosts <- gsub("_", " ", finished_hosts)
finished_hosts <- gsub(".csv", "", finished_hosts)

host_list <- read.table('./base_data/host_list.txt')
host_list <- host_list$x

to_finish <- host_list[which(!(host_list %in% finished_hosts))]
write.table(to_finish, './base_data/to_finish_list.txt')

problem_hosts <- read.table('./base_data/problem_hosts_nat.txt')
problem_hosts_new <- vector(length = nrow(problem_hosts))
for(i in 1:nrow(problem_hosts)) {
  problem_hosts_new[i] <- paste0(problem_hosts[i,1], ' ', problem_hosts[i,2])
}

final_finish <- to_finish[which(to_finish %in% problem_hosts_new)]
write.table(final_finish, './base_data/final_finish_list.txt')

# final finish introduced
int_list <- read.table('./base_data/introduced_list.txt')
int_list <- int_list$x
int_done <- gsub('.csv', '', list.files('./int_data'))
int_done <- gsub('_', ' ', int_done)
int_finish <- int_list[which(!(int_list %in% int_done))]
write.table(int_finish, './base_data/int_finish.txt')

int_finish <- read.table('./base_data/int_finish.txt')
int_finish <- int_finish$x
new_fin <- c(33, 34, 36, 39, 12, 16, 23, 24, 26)
int_finish_new <- int_finish[new_fin]
write.table(int_finish_new, './base_data/int_finish_new.txt')
