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
  
print_summary_table <- list()
for (i in setdiff(colnames(fire), c("X", "Y", "month", "large_fire"))){
  sum_tab <- SummaryTable(i)
  print_summary_table[[i]] <- sum_tab
}

print_summary_table %<>% bind_rows() 
print_summary_table %<>% modify_if(is.numeric, round, 4)


write.csv(print_summary_table, "data/summary-table.CSV")

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

#### Diagnostic ####

fit <- glm(formula = large_fire ~ hazard + Y_flag,
           family  = "binomial",
           weights = fire_count,
           data    = train)

# standard_res <- rstandard(fit, type = "pearson")
# 
# plot(standard_res)
# 
# car::vif(fit)
# 
# car::outlierTest(fit)
# library(broom)
# 
# fit_df <- augment(fit) %>% mutate(ID = 1:nrow(.))
# 
# ggplot(fit_df, aes(x = ID, y = .std.resid)) + 
#   geom_point(aes(color = factor(large_fire)), alpha = 0.5) + 
#   theme_minimal()
# 
# plot(fit, which = 4, id.n = 3)


library(car)

outlierTest(fit) # Bonferonni p-value for most extreme obs


# Influential Observations
# Cook's D plot
# identify D values > 4/(n-k-1)
# cutoff <- 4/((nrow(fire)-length(fit$coefficients)-2))
# plot(fit, which=4, cook.levels=cutoff)
# Influence Plot
influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


diag <- augment(fit) %>% mutate(Index = 1:nrow(.))

diag %<>% 
  mutate(high_cooksd = case_when(
    .cooksd > .15 ~ 1, TRUE ~ 0), 
    col_stdresid = case_when(
      .std.resid > 0 ~ 1, 
      .std.resid < 0 ~ 0), 
    high_hat = case_when(
      .hat > .1 ~ 1, 
      TRUE ~ 0
    ))


ggplot(diag, aes(x = Index, y = .cooksd, color = factor(high_cooksd))) + 
  geom_point() +
  labs(y = "Cook's Distance") + 
  theme_minimal() + 
  scale_color_manual(values = c("#9aa1d9", "black")) + 
  theme(legend.position = "none") 

ggplot(diag, aes(x = Index, y = .std.resid, color = factor(col_stdresid))) + 
  geom_point() +
  labs(y = "Standard Residuals") + 
  theme_minimal() + 
  scale_color_manual(values = c("black", "#9aa1d9")) + 
  theme(legend.position = "none")

ggplot(diag, aes(x = Index, y = .hat, color = factor(high_hat))) + 
  geom_point() +
  labs(y = "Diagonal Hat Matrix") + 
  theme_minimal() + 
  scale_color_manual(values = c("#9aa1d9", "black")) + 
  theme(legend.position = "none")


library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit)


# Evaluate homoscedasticity
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)


# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?


inf_points <- (diag %>% filter(.std.resid > 2 | .std.resid < -2))$.rownames %>% as.numeric()

fire[inf_points, ] %>% as.data.frame() %>% dplyr::select(large_fire, hazard, Y_flag, fire_count) %>% 
  mutate(rownames = inf_points)

plot(diag$.resid, diag$large_fire)

