#### Model Development for non-zero model ####



# fire_nonzero %<>%
#   group_by(X, Y, month) %>%
#   summarise(FFMC = median(FFMC),
#             DMC = median(DMC),
#             DC = median(DC),
#             ISI = median(ISI),
#             temp = median(temp),
#             RH = median(RH),
#             wind = median(wind),
#             rain = median(rain),
#             area = mean(area),
#             fire_count = n())

fire_nonzero %<>% 
  mutate(large_fire = log(area))

fire_nonzero %<>%
  mutate(FFMC_bin = ntile(FFMC, 10),
         DMC_bin = ntile(DMC, 10),
         DC_bin = ntile(DC, 10),
         ISI_bin = ntile(ISI, 10),
         temp_bin = ntile(temp, 10),
         RH_bin = ntile(RH, 10),
         wind_bin = ntile(wind, 10))

fire_nonzero %<>% mutate(rain = case_when(
  rain == 0 ~ 0, 
  TRUE~ 1))

fire_nonzero$month %<>%
  factor(levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                    "jul", "aug", "sep", "oct", "nov", "dec"))

# fire_nonzero$day %<>% 
#   factor(levels = c("sat", "sun", "mon", "tue", "wed", "thu", "fri"))

fire_nonzero %<>% 
  mutate(location = paste(X, Y, sep = ","))

fire_nonzero %<>% 
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
fire_nonzero %<>% 
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

fire_nonzero$hazard <- select(data.frame(fire_nonzero), contains("_hazard")) %>% rowSums()


fire_nonzero %<>%
  mutate(fire_season = case_when(
    month %in% c("jun", "jul", "aug", "sep", "oct") ~ 1, 
    TRUE ~ 0))

fire_nonzero %<>% 
  mutate(wind_speed = case_when(
    wind < 4 ~ 1,
    wind >= 4 ~ 2
  ))


ExploreVariable(fire_nonzero, "X")
# ExploreVariable(fire_nonzero, "X_flag")
ExploreVariable(fire_nonzero, "Y")
ExploreVariable(fire_nonzero, "Y_flag")
ExploreVariable(fire_nonzero, "location") + theme(axis.text.x = element_text(angle = 90))
ExploreVariable(fire_nonzero, "location_flag")
ExploreVariable(fire_nonzero, "month")
ExploreVariable(fire_nonzero, "fire_season")
ExploreVariable(fire_nonzero, "day")
ExploreVariable(fire_nonzero, "FFMC_bin", count = FALSE)
ExploreVariable(fire_nonzero, "FFMC_hazard")
ExploreVariable(fire_nonzero, "DMC_bin", count = FALSE)
ExploreVariable(fire_nonzero, "DMC_hazard")
ExploreVariable(fire_nonzero, "DC_bin", count = FALSE)
ExploreVariable(fire_nonzero, "DC_hazard")
ExploreVariable(fire_nonzero, "ISI_bin", count = FALSE)
ExploreVariable(fire_nonzero, "ISI_hazard")
ExploreVariable(fire_nonzero, "hazard")
ExploreVariable(fire_nonzero, "temp_bin", count = FALSE)
ExploreVariable(fire_nonzero, "RH_bin", count = FALSE)
ExploreVariable(fire_nonzero, "wind")
ExploreVariable(fire_nonzero, "wind_speed")
ExploreVariable(fire_nonzero, "wind_bin", count = FALSE)
ExploreVariable(fire_nonzero, "rain")
# ExploreVariable(fire_nonzero, "fire_count")


fit <- lm(large_fire ~ fire_season + ISI_hazard + temp + wind_speed + rain, 
          data = fire_nonzero)

summary(fit)


fire_nonzero$preds <- predict(fit, newdata = fire_nonzero)

fire_nonzero$percentile <- ntile(fire_nonzero$preds, 10)

em_pred_df <- fire_nonzero %>% 
  group_by(percentile) %>% 
  summarise(em_prop = mean(large_fire), 
            pred_prop = mean(preds))

# em_pred_df %>% 
#   gather("key", "value", -percentile)

ggplot(em_pred_df, aes(x = percentile)) + 
  geom_line(aes(y = em_prop), color = "red", size = 1) + 
  geom_line(aes(y = pred_prop), color = "blue", size = 1) + 
  geom_point(aes(y = em_prop), color = "red", size = 2) + 
  geom_point(aes(y = pred_prop), color = "blue", size = 2) + 
  theme_minimal()




