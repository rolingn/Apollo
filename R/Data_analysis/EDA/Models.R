library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggforce)
library(ggedit)
library(ggsci)
library(dplyr)
library(lubridate)
library(ggbreak)
library(ggthemes)
library(gglorenz)
library(ggpubr)
library(ggeffects)
library(lme4)
library(flexplot)
library(GGally)
library(lattice)
library(lmerTest)
library(mgcv)
library(mgcViz)
library(effects)
library(DHARMa)
library(emmeans)
library(sjPlot)

## Biomass und abundance mit site*elevation+(1|dayofyear) plots Nr1

## Anne suggestions --------------------------------------------------------
## Test model diagnostics - looks better
simulationOutput <- simulateResiduals(fittedModel = mlog_infection, plot = F)
plot(simulationOutput)

## Estimated marginal mean
#em <- as.data.frame(emmeans(m, ~ site*altitude, CI = T))
----------------------------------------------------------------------------

## Bei anova immer das komplexere Model an 2. Stelle setzen!!!
## If then the p value is significant, it means that the more complex model performs better

## Set working directory
setwd("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs")

## For all data gathering and transformations, see script Reading_and_processing_raw_csvs.R
#all_transects_all_data <- read.csv("all_transects_all_data.csv", sep = ",") # <- Uncomment if for some reason you want to work with the unfiltered dataset
all_transects_active <- read.csv("all_transects_simultaniously_active.csv", sep = ",")

all_transects_active$site <- as.factor(all_transects_active$site)
all_transects_all_data$site <- as.factor(all_transects_all_data$site)

## Summarizes the Sites into a smaller dataframe
#mean_detections_per_day <- all_transects_active %>% distinct(site, altitude, daily_detections_per_site)
detections_per_day <- all_transects_active %>% distinct(site, altitude, daily_detections, biomass_per_day, dayofyear)

#mean_detections_per_day$site <- as.factor(mean_detections_per_day$site)
detections_per_day$site <- as.factor(detections_per_day$site)

## Models ##
#plot(mean_detections_per_day$altitude, mean_detections_per_day$daily_detections_per_site)
#plot(mean_detections_per_day$site, mean_detections_per_day$daily_detections_per_site)
#ggpairs(mean_detections_per_day)


# Simple abundance model --------------------------------------------------
# m<-lm(Abundance~Gradient*Elevation)
m <-lm(daily_detections~site*altitude, detections_per_day)
car::Anova(m) # <- This function allows to check whether interaction is important without having to compare it to another model

m1 <-lm(daily_detections~altitude, detections_per_day)
anova(m1, m) # <- Keeping site improves the model -> original model is the best

summary(m1)
# (Intercept)                 16.24283   33.87752   0.479 0.631816 <- 16.24 detections at ref site (Jatzhorn) when altitude is 0
# siteMonstein               147.52686   42.52465   3.469 0.000566 *** <- detections at monstein are significantly higher than at Jatzhorn (by 147) when altitude is 0
#   siteWeissfluhjoch          -60.36856   45.21128  -1.335 0.182381 <- detections at Weissfluhjoch is lower, but not significantly
# altitude                     0.03234    0.01633   1.981 0.048096 * <- altitude has a slightly positive effect on detections at Jatzhorn (significant)
#   siteMonstein:altitude       -0.05492    0.02052  -2.676 0.007684 ** <- altitude has a negative effect at monstein compared to Jatzhorn (significant)
#   siteWeissfluhjoch:altitude   0.03266    0.02185   1.494 0.135693 <- The altitude effect at Weissfluhjoch is not significantly different than the one at the ref site (Jatzhorn)

#plot(m)
plot(allEffects(m))
plot(simulateResiduals(fittedModel = m, plot = F))





# Simple Biomass model ----------------------------------------------------
## Keep in mind: this model is calculated with the biomass of the individual insects and no aggregated value,
## meaning we basically model whether the insects themselves get heavier or lighter with altitude.
# m<-lm(Biomasse~Gradient*Elevation)
m <-lm(biomass~site*altitude, all_transects_active)
car::Anova(m) # <- both variables, as well as the interaction are significant, keep everything

summary(m)
# (Intercept)         -0.864803   0.419390  -2.062   0.0392 *   <- Biomass at Jatzhorn at altitude 0 is estimated at -0,86 (not relevant)
#   siteMonstein      -0.272620   0.215860  -1.263   0.2066     <- Biomass at Monstein does not significantly differ from the one at Jatzhorn
# siteWeissfluhjoch   -1.182295   0.208308  -5.676 1.39e-08 *** <- Biomass at Weissfluhjoch is significantly lower than at Jatzhorn
#   altitude           0.004745   0.000187  25.372  < 2e-16 *** <- Altitude has a significant positive effect on biomass

#plot(m)
plot(allEffects(m))
plot(simulateResiduals(fittedModel = m, plot = F))

# Daily Biomass model -------------------------------------------------------------------------
## Aggregating the biomass measurements into a single value per day and modeling -> not about individual biomass anymore :)
m <-lm(biomass_per_day~site*altitude, detections_per_day)
car::Anova(m) # <- both variables are significant, the interaction only marginally. Think about dropping the interaction.

m <-lm(biomass_per_day~site+altitude, detections_per_day) # Comment out when working with all data df
summary(m)


plot(allEffects(m))
plot(simulateResiduals(fittedModel = m, plot = F))


## Mixed effects models

# Base checks variance explained by dayofyear & gradient ------------------
## First, lets test how much variance in the data can be explained with the dayofyear and gradient
m<-lmer(daily_detections~1+(1|dayofyear), detections_per_day)
summary(m)
round((1895 / (1895 + 5781)) * 100, 2) ## Variation in daily detections explained by dayofyear: 24,69%
m<-lmer(daily_detections~1+(1|site), detections_per_day)
summary(m)
round((342.7 / (342.7 + 7522.5)) * 100, 2) ## Variation in daily detections explained by gradient: 4,36%
m<-lmer(biomass_per_day~1+(1|dayofyear), detections_per_day)
summary(m)
round((109807 / (109807 + 461849)) * 100, 2) ## Variation in biomass explained by dayofyear: 19,21%
m<-lmer(biomass_per_day~1+(1|site), detections_per_day)
summary(m)
round((11617 / (11617 + 563352)) * 100, 2) ## Variation in biomass explained by site: 2,02%

m<-lmer(daily_detections~1+(1|site)+(1|dayofyear), detections_per_day)
summary(m)
dotplot(ranef(m, condVar=TRUE))
plot(allEffects(m))



# Mixed effect for Abundance ----------------------------------------------
# m<-lmer(Abundance~Gradient*Elevation+(1|dayofyear)
## Rescaling the altitude variable if you want to avoid the warning
detections_per_day$altitude_scaled <- scale(detections_per_day$altitude)
m <-lmer(daily_detections~site*altitude+(1|dayofyear), detections_per_day)
car::Anova(m) # <- altitude not significant alone, but interaction is, so keep everything

m1 <-lm(daily_detections~site*altitude+dayofyear, detections_per_day)
anova(m, m1) # <- dayofyear should be included as random and not as fixed
m1 <-lmer(daily_detections~site*altitude_scaled+(1+altitude_scaled|dayofyear), detections_per_day)
anova(m, m1) # <- allowing the slopes to vary depending on the dayofyear significantly improves the model
m <-lmer(daily_detections~site*altitude_scaled+(1+altitude_scaled|dayofyear), detections_per_day)
## Checking if random effect is singular
isSingular(m, tol = 1e-4)
## Returns FALSE, which is good
summary(m)
plot(m)
estimates(m)
ggplot(detections_per_day, aes(altitude, daily_detections, colour = dayofyear, group = dayofyear)) +
  geom_point() +
  scale_y_log10() +
  geom_smooth(method = "lm", se = FALSE)
visualize(m, plot = "model", formula = daily_detections~altitude_scaled|dayofyear,
          sample = 10)
visualize(m, plot = "residuals") # <- Residuals seem a bit skewed

plot(allEffects(m))
plot(simulateResiduals(fittedModel = m, plot = F))



# Abundance squared model -------------------------------------------------
## m<-lm(Abundance~Gradient*Elevation*dayofyear^2)
m<-lm(daily_detections~site*altitude*poly(dayofyear, 2), detections_per_day)
m1<-lm(daily_detections~altitude*poly(dayofyear, 2), detections_per_day)
anova(m1, m) # <- site should be kept
m1<-lm(daily_detections~site+altitude*poly(dayofyear, 2), detections_per_day)
anova(m1, m) # <- interaction should be kept
m1<-lm(daily_detections~site*altitude, detections_per_day)
anova(m1, m) # <- poly(dayofyear, 2) should be kept
m1<-lm(daily_detections~site*altitude+poly(dayofyear, 2), detections_per_day)
anova(m1, m) # <- interaction should be kept
## How does this model (m) perform compared to the mixed effects model?
m1 <-lmer(daily_detections~site*altitude_scaled+(1+altitude_scaled|dayofyear), detections_per_day)
anova(m1, m) # <- mixed effects model is better
summary(m)



# Mixed effect for Biomass ------------------------------------------------
## m<-lmer(Biomass~Gradient*Elevation+(1|dayofyear)
m<-lmer(biomass_per_day~site*altitude+(1|dayofyear), detections_per_day)
car::Anova(m) # <- keep as it is
summary(m)
tab_model(m)
em <- as.data.frame(emmeans(m, ~ site*altitude, at = list(altitude = c(1600, 1700, 1900, 2500)), CI = T))
ggplot(em, aes(altitude, emmean, color = site)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = 0.3)

m1<-lmer(biomass_per_day~site+altitude+(1|dayofyear), detections_per_day)
anova(m1, m) # <- keep the interaction since it significantly improves the model
m1<-lmer(biomass_per_day~altitude+(1|dayofyear), detections_per_day)
anova(m1, m) # <- site variable cannot be dropped
## Comparing the mixed effects model to the fixed only effects model from earlier:
m1 <-lm(biomass_per_day~site+altitude, detections_per_day)
anova(m, m1) # <- m has lower AIC and BIC and p is highly significant, meaning the mixed effect term dayofyear should NOT be dropped
summary(m)
# (Intercept)                -553.9091   251.1274  470.3561  -2.206   0.0279 *  <- Biomass at Jatzhorn at altitude 0 (-553,9)
#   siteMonstein                788.5335   310.8261  440.7682   2.537   0.0115 * <- Monstein has a higher biomass as compared to Jatzhorn (slighly significant)
#   siteWeissfluhjoch          -413.4132   331.4424  448.2891  -1.247   0.2129 <- No significant difference at Weißfluhjoch  
# altitude                      0.6013     0.1196  442.5123   5.027 7.24e-07 *** <- Altitude has a highly significant positive effect (0,6mg increase with every meter) on biomass
#   siteMonstein:altitude        -0.2708     0.1498  439.3974  -1.808   0.0713 . <- Marginally significant difference in altitude effect at Monstein as compared to Jatzhorn
# siteWeissfluhjoch:altitude    0.1789     0.1598  443.8846   1.120   0.2635 <- No significant difference in altitude effect at Weissfluhjoch as compared to Jatzhorn



# Biomass squared model ---------------------------------------------------
## m<-lm(Biomass~Gradient*Elevation*dayofyear^2)
m<-lm(biomass_per_day~site*altitude*poly(dayofyear, 2), detections_per_day)
m1<-lm(biomass_per_day~site+altitude*poly(dayofyear, 2), detections_per_day)
anova(m1, m) # <- keep interaction
m1<-lm(biomass_per_day~altitude*poly(dayofyear, 2), detections_per_day)
anova(m1, m) # <- keep site
m1<-lm(biomass_per_day~site*altitude, detections_per_day)
anova(m1, m) # <- keep square term
summary(m)


# Diptera biomass models ------------------------------------------------
diptera_df <- subset(all_transects, order == "Diptera")
diptera_df <- diptera_df %>%
  group_by(site, position) %>%
  filter(dayofyear >= 205 & dayofyear <= 261) %>% ## <---- comment out if you want to work with all data
  mutate(mean_daily_detections = (n() / days_active)) %>%
  group_by(site, position, dayofyear) %>%
  mutate(daily_detections = n()) %>%
  ungroup()
diptera_df <- diptera_df %>%
  mutate(light = recode(light, "yes1" = "yes"))
diptera_df <- diptera_df %>% group_by(site, position, dayofyear) %>% mutate(biomass_per_day = (sum(biomass, na.rm = TRUE)))
diptera_per_day <- diptera_df %>% distinct(site, altitude, daily_detections, biomass_per_day, dayofyear, .keep_all = TRUE)
diptera_df$altitude_scaled <- scale(diptera_df$altitude)
diptera_per_day$altitude_scaled <- scale(diptera_per_day$altitude)

ggplot(diptera_df, aes(x = altitude, y = biomass, color = site)) +
  geom_point(alpha = 0.6, aes()) +  # Raw data points
  geom_smooth(method = "lm", se = TRUE) +  # Linear model fit with confidence interval
  scale_y_log10() +
  labs(x = "Altitude", y = "Biomass", color = "Site") +
  theme_minimal()

diptera_df$site <- as.factor(diptera_df$site)
diptera_per_day$site <- as.factor(diptera_per_day$site)
## Individual insect biomass
m <-lm(biomass~site*altitude, diptera_df)
m1 <-lm(biomass~site+altitude, diptera_df)
anova(m1, m) # <- interaction between site and altitude should be kept
m1 <-lm(biomass~site*altitude + light, diptera_df)
anova(m, m1) # <- including light as a fixed term significantly improves the model
m <-lm(biomass~site*altitude + light, diptera_df)
summary(m)
m1 <- lmer(biomass~site*altitude+light+(1|dayofyear), diptera_df)
anova(m1, m) # <- more complex with random effect is way better
m <- lmer(biomass~site*altitude+light+(1|dayofyear), diptera_df)
summary(m)
m1 <- lmer(biomass~site*altitude+(1|light)+(1|dayofyear), diptera_df)
m1 <- lmer(biomass~site*altitude+(1|light), diptera_df)
m2 <- lmer(biomass~site*altitude+(1|dayofyear), diptera_df)
anova(m2, m) # <- including light as random effect is not helping. m2 is the best model
m <- lmer(biomass~site*altitude+(1|dayofyear), diptera_df)
m1 <- lm(biomass~site*altitude+dayofyear, diptera_df)
anova(m, m1) # <- dayofyear as a random effect is much better than as a fixed term
m1 <- lmer(biomass~site*altitude+(1+site|dayofyear), diptera_df)
anova(m, m1) # <- allowing the slope of the fit to vary by site is significantly better 
m <- lmer(biomass~site*altitude_scaled+(1+site|dayofyear), diptera_df, control = lmerControl(optimizer = "bobyqa"))
m1 <- lmer(biomass~site*altitude+light+(1+site|dayofyear), diptera_df, control = lmerControl(optimizer = "bobyqa"))
m1 <- lmer(biomass~site*altitude_scaled+(1+altitude_scaled|dayofyear), diptera_df, control = lmerControl(optimizer = "bobyqa"))
anova(m, m1) # <- including light as a fixed parameter does not really improve the model -> leaving it out
m <- lmer(biomass~site*altitude+(1+site|dayofyear), diptera_df, control = lmerControl(optimizer = "bobyqa"))
summary(m)
## Biomass per day
m <-lm(biomass_per_day~site*altitude, diptera_per_day)
m1 <-lm(biomass_per_day~site+altitude, diptera_per_day)
anova(m1, m) # <- interaction between site and altitude should be kept
m1 <-lm(biomass_per_day~site*altitude + light, diptera_per_day)
anova(m, m1) # <- including light as a fixed term does not significantly improve the model
m1 <-lm(biomass_per_day~site*altitude*light, diptera_per_day)
anova(m, m1) # <- inlcuding light with interaction significantly improves the model
m <-lm(biomass_per_day~site*altitude*light, diptera_per_day)
m1 <-lm(biomass_per_day~altitude*light, diptera_per_day)
anova(m1, m) # <- keep the site variable
m1 <- lmer(biomass_per_day~site*altitude*light+(1|dayofyear), diptera_per_day)
anova(m1, m) # <- more complex with random effect is way better
m <- lmer(biomass_per_day~site*altitude*light+(1|dayofyear), diptera_per_day)
m1 <- lmer(biomass_per_day~site*altitude+(1|light)+(1|dayofyear), diptera_per_day)
anova(m, m1) # <- including light as random effect is not helping. better as fixed effect
m1 <- lmer(biomass_per_day~site*altitude+(1|dayofyear), diptera_per_day)
anova(m1, m) # <- keep light
summary(m)


# Lepidoptera biomass models --------------------------------------------
lepi_df <- subset(all_transects_active, order == "Lepidoptera")
lepi_df <- lepi_df %>%
  group_by(site, position) %>%
  filter(dayofyear >= 205 & dayofyear <= 261) %>% ## <---- comment out if you want to work with all data
  mutate(mean_daily_detections = (n() / days_active)) %>%
  group_by(site, position, dayofyear) %>%
  mutate(daily_detections = n()) %>%
  ungroup()
lepi_df <- lepi_df %>%
  mutate(light = recode(light, "yes1" = "yes"))
lepi_df <- lepi_df %>% group_by(site, position, dayofyear) %>% mutate(biomass_per_day = (sum(biomass, na.rm = TRUE)))
lepi_per_day <- lepi_df %>% distinct(site, altitude, daily_detections, biomass_per_day, dayofyear, .keep_all = TRUE)

ggplot(lepi_df, aes(x = altitude, y = biomass, color = site)) +
  geom_point(alpha = 0.6, aes()) +  # Raw data points
  geom_smooth(method = "lm", se = FALSE) +  # Linear model fit with confidence interval
  scale_y_log10() +
  labs(x = "Altitude", y = "Biomass", color = "Site") +
  theme_minimal()

lepi_df$site <- as.factor(lepi_df$site)
lepi_per_day$site <- as.factor(lepi_per_day$site)
## Biomass per individual insect
m <- lmer(biomass~site*altitude+(1|light)+(1|dayofyear), lepi_df)
m1 <- lmer(biomass~site*altitude+(1|light), lepi_df)
m <- lmer(biomass~site*altitude+(1|dayofyear), lepi_df)
anova(m1, m) # <- one random effect (light) is better than both and light is better than dayofyear
m <- lmer(biomass~site*altitude+(1|light), lepi_df)
m1 <- lm(biomass~site*altitude+light, lepi_df)
anova(m, m1) # <- light as a fixed term is much better than as a random term
m <- lm(biomass~site*altitude+site*light, lepi_df)
anova(m1, m) # <- light seems to have no interaction with site -> we can drop site
m <- lm(biomass~site*altitude+light, lepi_df)
m1 <- lmer(biomass~site*altitude+light+(1|dayofyear), lepi_df)
anova(m1, m) # <- including the dayofyear random effect is not very significantly better - leaving it out
m <- lm(biomass~site*altitude+light, lepi_df)
summary(m)
# (Intercept)                  -38.444467  10.446674  -3.680 0.000240 *** <- Biomass is negative at Jatzhorn at alt 0 an light off (can be ignored)
#   siteMonstein                57.458586  12.572243   4.570 5.18e-06 *** <- Monstein has a significantly higher biomass at alt 0 and light off than Jatzhorn
#   siteWeissfluhjoch           52.567874  15.052097   3.492 0.000489 *** <- Weissfluhjoch has a significantly higher biomass at alt 0 and light off than Jatzhorn
#   altitude                     0.023789   0.004557   5.220 1.98e-07 *** <- altitude has a significantly positive impact on biomass at Jatzhorn
#   lightyes                    20.880717   4.279240   4.880 1.15e-06 *** <- if the lights are on, biomass is significantly elevated by 20,88
#   siteMonstein:altitude       -0.023519   0.005644  -4.167 3.22e-05 *** <- the altitude effect significantly differs at Monstein as comapared to Jatzhorn. Effect is nearly 0
#   siteWeissfluhjoch:altitude  -0.023560   0.006657  -3.539 0.000411 *** <- the altitude effect significantly differs at Weissfluhjoch as comapared to Jatzhorn. Effect is nearly 0

## Biomass per day
m <- lm(biomass_per_day~site*altitude, lepi_per_day)
m1 <- lmer(biomass_per_day~site*altitude+(1|dayofyear), lepi_per_day)
anova(m1, m) # <- more complex with random effect is better
m <- lmer(biomass_per_day~site*altitude+(1|dayofyear), lepi_per_day)
m1 <- lmer(biomass_per_day~site+altitude+(1|dayofyear), lepi_per_day)
anova(m1, m) # <- the interaction between site and alt can be dropped
m <- lmer(biomass_per_day~site+altitude+(1|dayofyear), lepi_per_day)
m1 <- lmer(biomass_per_day~site+altitude+site*light+(1|dayofyear), lepi_per_day)
anova(m, m1) # <- include light
m <- lmer(biomass_per_day~site+altitude+site*light+(1|dayofyear), lepi_per_day)
m1 <- lmer(biomass_per_day~site+altitude+light+(1|dayofyear), lepi_per_day)
anova(m1, m) # <- the effect of light has no interaction with site -> dropping interaction
m <- lmer(biomass_per_day~site+altitude+light+(1|dayofyear), lepi_per_day)
summary(m)


# GAM Models --------------------------------------------------------------
detections_per_day$site <- as.factor(detections_per_day$site)
m <-gam(biomass_per_day~s(altitude, by = site, k = 8), data = detections_per_day)
## gam(biomass ~ site + s(dayofyear, by = site), data = data)# View summary of the modelsummary(m)
## predictionnew_data <- expand.grid(  dayofyear = seq(min(data$dayofyear), max(data$dayofyear), length.out = 100),  site = unique(data$site))# Predict values from the modelnew_data$predicted_biomass <- predict(m, new_data, type = "response")# Plotggplot(new_data, aes(x = dayofyear, y = predicted_biomass, color = site)) +  geom_line() +  labs(title = "GAM Predictions of Biomass Over Time",       x = "Day of Year", y = "Estimated Biomass") + theme_minimal()
summary(m)
m <- getViz(m)
for(i in 1:3) {
  o <- plot( sm(m, i) )
  print(o)
}
o + l_fitLine(colour = "red") + l_rug(mapping = aes(x=x, y=y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) + 
  l_points(shape = 19, size = 1, alpha = 0.1) + theme_classic()
