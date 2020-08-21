

#### Final Model for Binary #### 

n <- floor(.7 * nrow(fire))

train_ind <- sample(seq_len(nrow(fire)), size = n)

train <- fire[train_ind, ]

test <- fire[-train_ind, ]

fit <- glm(formula = large_fire ~ hazard + Y_flag,
           family  = "binomial",
           weights = fire_count,
           data    = train)

test$area_pred <- predict(fit, newdata = test, type = "response")
test$percentile <- ntile(test$area_pred, 10)

em_pred_df <- test %>% 
  group_by(percentile) %>% 
  summarise("Actual" = mean(large_fire), 
            "Predict" = mean(area_pred))

em_pred_df %<>% 
  gather("key", "value", -percentile)

ggplot(em_pred_df, aes(x = factor(percentile), y = value, color = key, group = key)) + 
  geom_line(size = 1) + 
  geom_point(size = 2) + 
  theme_minimal() + 
  labs(x = "Percentile", 
       y = "Average Proportion of of Large Fire") + 
  theme(legend.title = element_blank()) + 
  scale_color_manual(values = c("black", "#9aa1d9"))

aic1 <- fit$aic
test_roc <- pROC::roc(response = test$large_fire, predictor = test$area_pred)
test_auc1 <- as.data.frame(pROC::auc(test_roc))


tidy <- broom::tidy(fit) %>% 
  cbind(exp(confint(fit))) 

tidy %<>% mutate(OR = exp(fit$coefficients)) %>%  modify_if(is.numeric, round, 4) 
tidy

exp(tidy$estimate[[1]] + qnorm(c(0.025,0.975)) * tidy$estimate[[1]])
exp(tidy$estimate[[2]] + qnorm(c(0.025,0.975)) * tidy$estimate[[2]])
exp(tidy$estimate[[3]] + qnorm(c(0.025,0.975)) * tidy$estimate[[3]])

test_auc1
summary(fit)

aod::wald.test(b = coef(fit), Sigma = vcov(fit), Terms = 4)

tidy 
write_csv(tidy, "data/tidy-hazard-yflag.CSV")


test %>% 
  group_by(round(area_pred), large_fire) %>% 
  tally() %>% 
  
  tidy

test %<>% mutate(area_pred = case_when(area_pred < 0.5 ~ 0.5, TRUE ~ 1.1))

confusion_matrix <- ftable(test$large_fire, test$area_pred)
accuracy <- sum(diag(confusion_matrix))/nrow(test)*100
accuracy

#### Interaction ####

fit <- glm(formula = large_fire ~ hazard + Y_flag + hazard:Y_flag,
           family  = "binomial",
           weights = fire_count,
           data    = train)

test$area_pred <- predict(fit, newdata = test, type = "response")
test$percentile <- ntile(test$area_pred, 10)

test_roc <- pROC::roc(response = test$large_fire, predictor = test$area_pred)
test_auc2 <- as.data.frame(pROC::auc(test_roc))
aic2 <- fit$aic

test_auc1
test_auc2
aic1
aic2
