# var_remove and var_select functions for automating variable selection process

# var_remove function for single iterative variable removal
# wanted_envs = the clipped environmental spatraster used in the model
# wanted_occs = the two column occurrence matrix
# wanted_bg = the two column background matrix
# returns the spatraster with the least important variable discarded
var_remove <- function(wanted_envs, wanted_occs, wanted_bg) {
  initial_mod <- MaxEnt(wanted_envs, p = wanted_occs, a = wanted_bg)
  initial_results <- data.frame(initial_mod@results)
  measure_name <- row.names(initial_results)
  perm_import <- data.frame(measure_name, 
                            initial_results[,1])
  colnames(perm_import) <- c("measure_name", "result")
  want_pull <- grep('.permutation.importance', perm_import$measure_name)
  final_perm_import <- perm_import[want_pull,]
  perm_remove <- which(final_perm_import$result == min(final_perm_import$result))
  if (length(perm_remove > 1)) {
    perm_remove <- perm_remove[1]
  }
  remove_this <- final_perm_import$measure_name[perm_remove]
  remove_this <- gsub('.permutation.importance', '', remove_this)
  
  # removing variable from analysis
  remove_env <- which(names(wanted_envs) %in% remove_this) 
  new_envs <- wanted_envs[[-remove_env]]
  
  return(new_envs)
}

# var_select function to iterate var_remove until all vifs are below 5
var_select <- function(wanted_envs, wanted_occs, wanted_bg) {
  
  final_env <- wanted_envs
  t <- max(usdm::vif(final_env)$VIF)
  
  while(t > 5) {
    final_env <- var_remove(wanted_envs = final_env, wanted_occs = wanted_occs,
                            wanted_bg = wanted_bg)
    if(dim(final_env)[3] < 3) {
      t <- 4
    } else {
      t <- max(usdm::vif(final_env)$VIF)
    }
  }
  
  return(final_env)
}