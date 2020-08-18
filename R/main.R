#### Packages #### 

library(tidyverse)
library(magrittr)
library(ggcorrplot)


#### Parameters ####

xvars <- c("X", "Y", "month", "day", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain")

bar_color <- "#9aa1d9"

#### Functions ####

ExploreVariable <- function(xvar, count = TRUE){
  group_df <- fire %>% 
    group_by(.dots = xvar) %>% 
    summarise(avg_log_area = mean(log_area),
              tot_log_area = sum(log_area),
              avg_bi_area = mean(bi_area),
              count = n())
  
  ratio <- max(group_df$count) / max(group_df$avg_log_area)
  
  if(count == TRUE){
    p <- ggplot(group_df, aes_string(x = xvar, group = 1)) +
      geom_bar(aes(y = count), stat = "identity", fill = bar_color, col = "dark grey") +
      geom_point(aes(y = avg_log_area * ratio), size = 2, color = "black") + 
      geom_line(aes(y = avg_log_area * ratio), size = 1, color = "black") +
      scale_y_continuous(sec.axis = sec_axis(~./ratio, name = "Average Burn Area"))
  } else {
    p <- ggplot(group_df, aes_string(x = xvar, group = 1)) +
      geom_point(aes(y = avg_log_area), size = 2, color = "black") + 
      geom_line(aes(y = avg_log_area), size = 1, color = "black")   
  }
  p +
    labs(y = "Count of Fire") +
    theme_minimal()
}


#### Load Data ####

fire <- read_csv("data/dae-2020-fall-data.csv")

fire %<>%
  mutate(log_area = log(area + 1), 
         bi_area = case_when(log_area == 0 ~ 0, TRUE ~ 1))

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
      X %in% seq(1,3) ~ "west",
      X %in% seq(4,6) ~ "central",
      TRUE ~ "east")) %>% 
  mutate(location_flag = paste(Y_flag, X_flag, sep = ","))


# fire %<>% 
#   mutate(
#     FFMC_hazard = case_when(
#       FFMC >= 0 & FFMC < 77 ~ "Low", 
#       FFMC >= 77 & FFMC < 85 ~ "Moderate",
#       FFMC >= 85 & FFMC < 89 ~ "High",
#       FFMC >= 89 & FFMC < 92 ~ "Very High",
#       FFMC >= 92 ~ "Extreme"
#   ), 
#   DMC_hazard = case_when(
#     DMC >= 0 & DMC < 22 ~ "Low",
#     DMC >= 22 & DMC < 28 ~ "Moderate",
#     DMC >= 28 & DMC < 41 ~ "High",
#     DMC >= 41 & DMC < 61 ~ "Very High",
#     DMC >= 61 ~ "Extreme"
#   ), 
#   DC_hazard = case_when(
#     DC >= 0 & DC < 80 ~ "Low",
#     DC >= 80 & DC < 190 ~ "Moderate",
#     DC >= 190 & DC < 300 ~ "High",
#     DC >= 300 & DC < 425 ~ "Very High",
#     DC >= 425 ~ "Extreme",
#   ), 
#   ISI_hazard = case_when(
#     ISI < 1.5 ~ "Low",
#     ISI >= 1.5 & ISI < 4.1 ~ "Moderate",
#     ISI >= 4.1 & ISI < 8.1 ~ "High",
#     ISI >= 8.1 & ISI < 15 ~ "Very High",
#     ISI >= 15 ~ "Extreme",
#   )
# )

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

# fire$FFMC_hazard %<>% factor(levels = c("Low", "Moderate", "High", "Very High", "Extreme"))

# fire %<>% 
#   modify_at(c("FFMC_hazard", "DMC_hazard", "DC_hazard", "ISI_hazard"), 
#             factor, 
#             levels = c("Low", "Moderate", "High", "Very High", "Extreme"))


#### Data Exploratory Analysis ####

ExploreVariable("X")
ExploreVariable("X_flag")
ExploreVariable("Y")
ExploreVariable("Y_flag")
ExploreVariable("location") + theme(axis.text.x = element_text(angle = 90))
ExploreVariable("location_flag")
ExploreVariable("month")
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
  select(FFMC, DMC, DC, ISI, temp, RH, wind, rain, area) %>% 
  cor()

ggcorrplot(corr_table, hc.order = TRUE, type = "upper",
           outline.col = "white")


#### Model Development ####

