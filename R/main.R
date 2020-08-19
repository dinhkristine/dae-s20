#### Packages #### 

library(tidyverse)
library(magrittr)
library(ggcorrplot)


#### Parameters ####

xvars <- c("X_flag", "fire_season", "temp", "RH", "rain", "hazard")

bar_color <- "#9aa1d9"

#### Functions ####

ExploreVariable <- function(xvar, count = TRUE){
  group_df <- fire %>% 
    group_by(.dots = xvar) %>% 
    summarise(avg_prop_large_fire = mean(large_fire),
              count = n())
  
  ratio <- max(group_df$count) / max(group_df$avg_prop_large_fire)
  
  if(count == TRUE){
    p <- ggplot(group_df, aes_string(x = xvar, group = 1)) +
      geom_bar(aes(y = count), stat = "identity", fill = bar_color, col = "dark grey") +
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


#### Load Data ####

fire <- read_csv("data/dae-2020-fall-data.csv")

fire %<>%
  mutate(large_fire = case_when(area == 0 ~ 0, TRUE ~ 1))

fire %<>%
  mutate(FFMC_bin = ntile(FFMC, 10), 
         DMC_bin = ntile(DMC, 10),
         DC_bin = ntile(DC, 10), 
         ISI_bin = ntile(ISI, 10), 
         temp_bin = ntile(temp, 10), 
         RH_bin = ntile(RH, 10))

fire %<>% mutate(rain = case_when(
  rain == 0 ~ 0, 
  TRUE~ 1))

fire$month %<>%
  factor(levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                    "jul", "aug", "sep", "oct", "nov", "dec"))

fire$day %<>% 
  factor(levels = c("sat", "sun", "mon", "tue", "wed", "thu", "fri"))

fire %<>% 
  mutate(location = paste(fire$X, fire$Y, sep = ","))

fire %<>% 
  mutate(Y_flag = case_when(
    Y %in% c(1,2,3,4) ~ "north", 
    TRUE ~ "south"), 
    X_flag = case_when(
      X %in% seq(1,5) ~ "west",
      TRUE ~ "east")) %>% 
  mutate(location_flag = paste(Y_flag, X_flag, sep = ","))


## 1 - Low 
## 2 - Moderate 
## 3 - High 
## 4 - Very High 
## 5 - Extreme
fire %<>% 
  mutate(
    FFMC_hazard = case_when(
      FFMC >= 0 & FFMC < 77 ~ 1, 
      FFMC >= 77 & FFMC < 85 ~ 2,
      FFMC >= 85 & FFMC < 89 ~ 3,
      FFMC >= 89 & FFMC < 92 ~ 4,
      FFMC >= 92 ~ 5
    ), 
    DMC_hazard = case_when(
      DMC >= 0 & DMC < 22 ~ 1,
      DMC >= 22 & DMC < 28 ~ 2,
      DMC >= 28 & DMC < 41 ~ 3,
      DMC >= 41 & DMC < 61 ~ 4,
      DMC >= 61 ~ 5
    ), 
    DC_hazard = case_when(
      DC >= 0 & DC < 80 ~ 1,
      DC >= 80 & DC < 190 ~ 2,
      DC >= 190 & DC < 300 ~ 3,
      DC >= 300 & DC < 425 ~ 4,
      DC >= 425 ~ 5,
    ), 
    ISI_hazard = case_when(
      ISI < 1.5 ~ 1,
      ISI >= 1.5 & ISI < 4.1 ~ 2,
      ISI >= 4.1 & ISI < 8.1 ~ 3,
      ISI >= 8.1 & ISI < 15 ~ 4,
      ISI >= 15 ~ 5,
    )
  )

fire %<>%
  mutate(hazard = select(., contains("_hazard")) %>% rowSums())

fire %<>%
  mutate(fire_season = case_when(
    month %in% c("jun", "jul", "aug", "sep", "oct") ~ 1, 
    TRUE ~ 0))


#### Data Exploratory Analysis ####

ExploreVariable("X")
ExploreVariable("X_flag")
ExploreVariable("Y")
ExploreVariable("Y_flag")
ExploreVariable("location") + theme(axis.text.x = element_text(angle = 90))
ExploreVariable("location_flag")
ExploreVariable("month")
ExploreVariable("fire_season")
ExploreVariable("day")
ExploreVariable("FFMC_bin", count = FALSE)
ExploreVariable("FFMC_hazard")
ExploreVariable("DMC_bin", count = FALSE)
ExploreVariable("DMC_hazard")
ExploreVariable("DC_bin", count = FALSE)
ExploreVariable("DC_hazard")
ExploreVariable("ISI_bin", count = FALSE)
ExploreVariable("ISI_hazard")
ExploreVariable("hazard")
ExploreVariable("temp_bin", count = FALSE)
ExploreVariable("RH_bin", count = FALSE)
ExploreVariable("wind")
ExploreVariable("rain")


#### Correlation #### 

corr_table <- fire %>% 
  select(FFMC, DMC, DC, ISI, temp, RH, wind, rain, large_fire) %>% 
  cor()

ggcorrplot(corr_table, hc.order = TRUE, type = "upper",
           outline.col = "white", 
           colors = c("blue", "white", "red"))


#### Model Development ####

fit <- glm(formula = large_fire ~ FFMC + DMC + DC + ISI + RH + rain + month,
           family  = "binomial",
           data    = fire)

fire$area_pred <- predict(fit, newdata = fire, type = "response")
fire$percentile <- ntile(fire$area_pred, 10)

em_pred_df <- fire %>% 
  group_by(percentile) %>% 
  summarise(em_prop = mean(large_fire), 
            pred_prop = mean(area_pred))

em_pred_df %>% 
  gather("key", "value", -percentile)
  
ggplot(a, aes(x = percentile)) + 
  geom_line(aes(y = em_prop), color = "red", size = 1) + 
  geom_line(aes(y = pred_prop), color = "blue", size = 1) + 
  geom_point(aes(y = em_prop), color = "red", size = 2) + 
  geom_point(aes(y = pred_prop), color = "blue", size = 2) + 
  theme_minimal()


test_roc <- pROC::roc(response = fire$large_fire, predictor = fire$area_pred)
test_auc <- as.data.frame(pROC::auc(test_roc))






rp_par <- c(0.6, 0.7, 0.8, 0.9)

nrounds <- 10

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

print_list <- list()

xvars <- setdiff(colnames(fire), c("area_pred", "percentile", "large_fire", "area"))

for (n_features in 1:28){
  results <- LoopAllVars(rp = rp_par,
                         nrounds = nrounds, 
                         list_of_xvars = xvars, 
                         number_of_xvars = n_features)
  print_list[[n_features]] <- results
}

iteration_log2 <- print_list %>% bind_rows()








