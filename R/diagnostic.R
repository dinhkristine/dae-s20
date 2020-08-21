
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

