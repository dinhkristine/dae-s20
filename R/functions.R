#' Explore variables function, returning plots of variables count and average proportion
#'
#' @param df dataframe
#' @param xvar independent variable 
#' @param count return histogram of count,default is TRUE
#'
#' @return
#' @export
#'
#' @examples
ExploreVariable <- function(df, xvar, count = TRUE){
  
  fire <- df
  
  group_df <- fire %>% 
    group_by(.dots = xvar) %>% 
    summarise(avg_prop_large_fire = mean(large_fire),
              count = n())
  
  ratio <- max(group_df$count) / max(group_df$avg_prop_large_fire)
  
  if(count == TRUE){
    p <- ggplot(group_df, aes_string(x = xvar, group = 1)) +
      geom_bar(aes(y = count), stat = "identity", fill = "#9aa1d9", col = "dark grey") +
      geom_point(aes(y = avg_prop_large_fire * ratio), size = 2, color = "black") + 
      geom_line(aes(y = avg_prop_large_fire * ratio), size = 1, color = "black") +
      scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Average Proportion of Large Fire"))
    
    y_lab <- "Number of Fire"
    
  } else {
    p <- ggplot(group_df, aes_string(x = xvar, group = 1)) +
      geom_point(aes(y = avg_prop_large_fire), size = 2, color = "black") + 
      geom_line(aes(y = avg_prop_large_fire), size = 1, color = "black") 
    
    y_lab <- "Average Proportion of Large Fire"
  }
  p +
    labs(y = y_lab) +
    theme_minimal()
}


#' Simple binomial regression to run bootstrap for variable selection 
#'
#' @param rp random partition 
#' @param xvars independent variable/s
#' @param nrounds number of times running the model
#'
#' @return
#' @export
#'
#' @examples
BinaryFit <- function(rp, xvars, nrounds){
  
  # select varaibles
  df  <- fire %>%  select(large_fire, xvars)
  
  all_auc <- c()
  all_aic <- c()
  all_iteration <- list()
  index <- 1
  
  for(each_rp in rp){
    for (i in 1:nrounds){
      # split train and test randomly based on proportion
      n <- floor(each_rp * nrow(df))
      
      train_ind <- sample(seq_len(nrow(df)), size = n)
      
      train <- df[train_ind, ]
      
      test <- df[-train_ind, ]
      
      # build model 
      fit <- glm(large_fire ~ ., data = train, family = "binomial")
      
      # predict 
      preds <- predict(fit, newdata = test, type = "response")
      
      test$preds <- preds
      
      # Validate AUC  
      test_roc <- pROC::roc(response = test$large_fire, predictor = test$preds)
      
      auc <- pROC::auc(test_roc)[[1]]
      
      all_auc <- c(all_auc, auc)
      all_aic <- c(all_aic, fit$aic)
    }
    # iteration log 
    all_iteration[[index]] <- data.frame(iteration_date = Sys.time(), 
                                         model_type = "glm",
                                         distibution = "binomial",
                                         random_partition = each_rp, 
                                         nrow_train = nrow(train), 
                                         nrow_test = nrow(test),
                                         nrounds = nrounds, 
                                         n_features = length(xvars),
                                         features = paste(xvars, collapse = ","), 
                                         AIC = mean(all_aic),
                                         auc = mean(all_auc))
    index <- index + 1
  }
  all_iteration %<>% bind_rows()
  
  return(all_iteration)
}

SafelyBinaryFit <- safely(BinaryFit)


#' Function to loop all the models through each variable 
#'
#' @param rp 
#' @param nrounds 
#' @param list_of_xvars 
#' @param number_of_xvars 
#'
#' @return
#' @export
#'
#' @examples
LoopAllVars <- function(rp, nrounds, list_of_xvars, number_of_xvars){
  # group variables
  vars <- combn(list_of_xvars, number_of_xvars)
  
  # how many group do we have?
  no_vars <- dim(vars)[[2]]
  
  # initiate print list and index 
  print_list <- list()
  k <- 1
  
  for (i in 1:no_vars) {
    iteration_log <- SafelyBinaryFit(rp, xvars = vars[,i], nrounds)
    print_list[[k]] <- iteration_log
    k <- k + 1
  }
  # map all separate log to bind them together into a dataframe 
  iteration_log <- map(print_list, "result") %>% rbind_list()
  
  return(iteration_log)
}


#' Summary statistics of variable 
#'
#' @param input variable 
#'
#' @return
#' @export
#'
#' @examples
SummaryTable <- function(input){
  
  var <- fire[, input][[1]]
  
  data.frame(Variable = input, 
             Min = min(var), 
             "Lower IQR" = quantile(var, .25)[[1]], 
             Median = median(var), 
             Mean = mean(var), 
             "Upper IQR" = quantile(var, .75)[[1]], 
             Max = max(var))
}
