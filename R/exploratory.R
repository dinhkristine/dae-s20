

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
