

Funnn <- function(df, xvar){
  
  df %<>% select(large_fire, xvar)
  
  n <- floor(.6 * nrow(df))
  
  train_ind <- sample(seq_len(nrow(df)), size = n)
  
  train <- df[train_ind, ]
  
  test <- df[-train_ind, ]
  
  
  fit <- glm(formula = large_fire ~ .,
             family  = "binomial",
             data    = train)
  
  test$area_pred <- predict(fit, newdata = test, type = "response")
  test$percentile <- ntile(test$area_pred, 10)
  
  test_roc <- pROC::roc(response = test$large_fire, predictor = test$area_pred)
  test_auc <- as.data.frame(pROC::auc(test_roc))
  
  result <- data.frame(auc = test_auc, 
             xvar, 
             coef = fit$coefficients[[2]], 
             int_pvalue = broom::tidy(fit)$p.value[[1]], 
             coef_pvalue = broom::tidy(fit)$p.value[[2]])
  
  return(result)
}


xvars <- setdiff(colnames(fire), c("large_fire", "area", "month", "location"))

print_list <- list()

for (i in xvars){
  fit <- Funnn(data.frame(fire), i)
  
  print_list[[i]] <- fit 
}

a <-  print_list %>% bind_rows()




