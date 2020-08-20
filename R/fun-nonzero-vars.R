Funn2 <- function(df, xvar){
  
  df %<>% select(large_fire, xvar)
  
  fit <- glm(large_fire ~ ., data = df, family = "gaussian")
  
  data.frame(xvar = xvar, 
             coef = fit$coefficients[[2]], 
             p_value = broom::tidy(fit)$p.value[[2]], 
             mse = mean(fit$residuals^2))
  
}

xvars <- setdiff(colnames(fire_nonzero), c("large_fire", "area", "month", "location"))

print_list <- list()

for (i in xvars){
  fit <- Funn2(data.frame(fire_nonzero), i)
  
  print_list[[i]] <- fit 
}

a <-  print_list %>% bind_rows()


