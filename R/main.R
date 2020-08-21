#### Packages #### 

library(tidyverse)
library(magrittr)
library(ggcorrplot)
library(kable)
library(kableExtra)

#### Functions ####

source("R/functions.R")


#### Load Data ####

fire <- read_csv("data/dae-2020-fall-data.csv")

# obs  

unique_location_time <- fire %>% 
  group_by(X, Y, month) %>% 
  tally()


fire %<>% 
  group_by(X, Y, month) %>% 
  summarise(FFMC = median(FFMC), 
            DMC = median(DMC), 
            DC = median(DC), 
            ISI = median(ISI),
            temp = median(temp), 
            RH = median(RH), 
            wind = median(wind), 
            rain = median(rain), 
            area = mean(area), 
            fire_count = n())

fire %<>%
  mutate(large_fire = case_when(area == 0 ~ 0, TRUE ~ 1))
  


fire %<>%
  mutate(FFMC_bin = ntile(FFMC, 10), 
         DMC_bin = ntile(DMC, 10),
         DC_bin = ntile(DC, 10), 
         ISI_bin = ntile(ISI, 10), 
         temp_bin = factor(ntile(temp, 10)), 
         RH_bin = ntile(RH, 10))

fire %<>% mutate(rain = case_when(
  rain == 0 ~ 0, 
  TRUE~ 1))

fire$month %<>%
  factor(levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                    "jul", "aug", "sep", "oct", "nov", "dec"))


fire %<>% 
  mutate(location = paste(X, Y, sep = ","))

fire %<>% 
  mutate(Y_flag = case_when(
    Y %in% c(1,2,3,4) ~ "North", 
    TRUE ~ "South"), 
    X_flag = case_when(
      X %in% seq(1,5) ~ "West",
      TRUE ~ "East")) %>% 
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

fire$hazard <- select(data.frame(fire), contains("_hazard")) %>% rowSums()

fire %<>%
  mutate(fire_season = case_when(
    month %in% c("jun", "jul", "aug", "sep", "oct") ~ 1, 
    TRUE ~ 0))

fire$fire_season %<>% as.factor()

fire %<>%
  mutate(hazard_bin = factor(ntile(hazard, 10)))


#### Data Exploratory Analysis ####



# ExploreVariable(fire, "X")
ExploreVariable(fire, "X_flag") + labs(x = "Horizontal Cardinal Direction")
# ExploreVariable(fire, "Y")
ExploreVariable(fire, "Y_flag") + labs(x = "Vertical Cardinal Direction")
# ExploreVariable(fire, "location") + theme(axis.text.x = element_text(angle = 90))
# ExploreVariable(fire, "location_flag", count = FALSE) + 
#   labs(x = "Ordinal Direction") + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
# ExploreVariable(fire, "month")
ExploreVariable(fire, "fire_season") + 
  labs(x = "Fire Season")

# ExploreVariable(fire, "day")
# ExploreVariable(fire, "FFMC_bin", count = FALSE)
# ExploreVariable(fire, "FFMC_hazard")
# ExploreVariable(fire, "DMC_bin", count = FALSE) + 
#   labs(x = "DMC")
# ExploreVariable(fire, "DMC_hazard")
# ExploreVariable(fire, "DC_bin", count = FALSE) + 
#   labs(x = "DC")
# ExploreVariable(fire, "DC_hazard")
ExploreVariable(fire, "ISI_bin", count = FALSE) +
  labs(x = "ISI")
# ExploreVariable(fire, "ISI_hazard")
# ExploreVariable(fire, "hazard")
ExploreVariable(fire, "hazard_bin", count = FALSE) + labs(x = "Hazard Level")
ExploreVariable(fire, "temp_bin", count = FALSE) + labs(x = "temp")
ExploreVariable(fire, "RH_bin", count = FALSE) + labs(x = "RH")
ExploreVariable(fire, "wind")
ExploreVariable(fire, "rain")
ExploreVariable(fire, "fire_count", count = FALSE) + labs(x = "Fire Frequency")


#### ####


fit_DMC <- glm(formula = large_fire ~ DMC, family  = "binomial", data = fire)
fit_FFMC <- glm(formula = large_fire ~ FFMC, family  = "binomial", data = fire)
fit_DC <- glm(formula = large_fire ~ DC, family  = "binomial", data = fire)
fit_ISI <- glm(formula = large_fire ~ ISI_hazard, family  = "binomial", data = fire)

#### Correlation #### 

corr_table <- fire %>%
  as.data.frame() %>% 
  select(FFMC, DMC, DC, ISI, temp, RH) %>% 
  cor()

ggcorrplot(corr_table, hc.order = TRUE, 
           outline.col = "white",
           colors = c("blue", "white", "red"))


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
  summarise(em_prop = mean(large_fire), 
            pred_prop = mean(area_pred))

ggplot(em_pred_df, aes(x = percentile)) + 
  geom_line(aes(y = em_prop), color = "red", size = 1) + 
  geom_line(aes(y = pred_prop), color = "blue", size = 1) + 
  geom_point(aes(y = em_prop), color = "red", size = 2) + 
  geom_point(aes(y = pred_prop), color = "blue", size = 2) + 
  theme_minimal()

aic1 <- fit$aic
test_roc <- pROC::roc(response = test$large_fire, predictor = test$area_pred)
test_auc1 <- as.data.frame(pROC::auc(test_roc))


broom::tidy(fit) %>% 
  cbind(confint(fit))

test_auc1

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

#### Diagnostic ####

fit <- glm(formula = large_fire ~ hazard + Y_flag,
           family  = "binomial",
           weights = fire_count,
           data    = train)

standard_res <- rstandard(fit, type = "pearson")

plot(standard_res)

car::vif(fit)

car::outlierTest(fit)
library(broom)

fit_df <- augment(fit) %>% mutate(ID = 1:nrow(.))

ggplot(fit_df, aes(x = ID, y = .std.resid)) + 
  geom_point(aes(color = factor(large_fire)), alpha = 0.5) + 
  theme_minimal()

plot(fit, which = 4, id.n = 3)


