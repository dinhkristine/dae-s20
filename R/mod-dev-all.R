
xvars <- c("X_flag", "Y_flag", "temp", "fire_season", "hazard")

rp_par <- c(0.6, 0.7, 0.8, 0.9)

nrounds <- 10

fire <- as.data.frame(fire)


print_list <- list()

for (n_features in 1:5){
  results <- LoopAllVars(rp = rp_par,
                         nrounds = nrounds, 
                         list_of_xvars = xvars, 
                         number_of_xvars = n_features)
  print_list[[n_features]] <- results
}

iteration_log <- print_list %>% bind_rows()

write.csv(iteration_log, "data/iteration-log.CSV")
