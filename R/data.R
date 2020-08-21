#### Load data ####

fire <- read_csv("data/dae-2020-fall-data.csv")


#### Initial Data Explore ####

unique_location_time <- fire %>% 
  group_by(X, Y, month) %>% 
  tally()

# Summary statistics of each variable #

print_summary_table <- list()

for (i in setdiff(colnames(fire), c("X", "Y", "month", "large_fire", "day"))){
  sum_tab <- SummaryTable(i)
  print_summary_table[[i]] <- sum_tab
}

print_summary_table %<>% bind_rows() 

print_summary_table %<>% modify_if(is.numeric, round, 4)

# Save CSV to local 

write.csv(print_summary_table, "data/summary-table.CSV")


#### Group ####

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


#### Features Engineer ####

# create binary variable for area 0 = small area, 1 = large area with 100m^2 and bigger
fire %<>%
  mutate(large_fire = case_when(area == 0 ~ 0, TRUE ~ 1))

# create bins for indicies for visualization purposes 
fire %<>%
  mutate(FFMC_bin = ntile(FFMC, 10), 
         DMC_bin = ntile(DMC, 10),
         DC_bin = ntile(DC, 10), 
         ISI_bin = ntile(ISI, 10), 
         temp_bin = factor(ntile(temp, 10)), 
         RH_bin = ntile(RH, 10))

# create flag for rain 0 = no rain, 1 = rain
fire %<>% mutate(rain = case_when(
  rain == 0 ~ 0, 
  TRUE~ 1))

# create unique location X,Y
fire %<>% 
  mutate(location = paste(X, Y, sep = ","))

# coordinate direction North, South, East, and West
fire %<>% 
  mutate(Y_flag = case_when(
    Y %in% c(1,2,3,4) ~ "North", 
    TRUE ~ "South"), 
    X_flag = case_when(
      X %in% seq(1,5) ~ "West",
      TRUE ~ "East")) %>% 
  mutate(location_flag = paste(Y_flag, X_flag, sep = ","))

# Hazard level for each indices
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

# Total hazard level 
fire$hazard <- dplyr::select(data.frame(fire), contains("_hazard")) %>% rowSums()

# fire season 1 = Jun - oct, 0 = else 
fire %<>%
  mutate(fire_season = case_when(
    month %in% c("jun", "jul", "aug", "sep", "oct") ~ 1, 
    TRUE ~ 0))

#### Change variable class ####

fire$month %<>%
  factor(levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                    "jul", "aug", "sep", "oct", "nov", "dec"))

fire$fire_season %<>% as.factor()

fire %<>%
  mutate(hazard_bin = factor(ntile(hazard, 10)))
