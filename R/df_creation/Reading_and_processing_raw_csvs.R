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


## Set working directory
setwd("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/base_results")


# CSV read in -------------------------------------------------------------
## Read in all the different results. Each site has its own csv
## At the moment the data is filtered to only include detections that were classified as Animalia - Insecta
Weiss_low_raw <- read.csv("all_analysis_results_383_Weiss_low.csv", sep = ";")
Weiss_low_raw$Hour <- as.integer(Weiss_low_raw$Hour)
## Exclude all measurements before the 17th of July, since for some reason this data appears in the dataframe even though the research period had not started yet..
Weiss_low_raw <- Weiss_low_raw %>%
  slice(782:n())
#Weiss_low <- subset(Weiss_low_raw, kingdom == "Animalia")
#Weiss_low <- subset(Weiss_low, class == "Insecta")

Weiss_mid_raw <- read.csv("all_analysis_results_384_Weiss_mid.csv", sep = ";")
Weiss_mid_raw$Hour <- as.integer(Weiss_mid_raw$Hour)
#Weiss_mid <- subset(Weiss_mid_raw, kingdom == "Animalia")
#Weiss_mid <- subset(Weiss_mid, class == "Insecta")

Weiss_up_raw <- read.csv("all_analysis_results_385_Weiss_up.csv", sep = ";")
Weiss_up_raw$Hour <- as.integer(Weiss_up_raw$Hour)
#Weiss_up <- subset(Weiss_up_raw, kingdom == "Animalia")
#Weiss_up <- subset(Weiss_up, class == "Insecta")

Jatz_low_raw <- read.csv("all_analysis_results_386_421_Jatz_low.csv", sep = ";")
Jatz_low_raw$Hour <- as.integer(Jatz_low_raw$Hour)
#Jatz_low <- subset(Jatz_low_raw, kingdom == "Animalia")
#Jatz_low <- subset(Jatz_low, class == "Insecta")

Jatz_mid_raw <- read.csv("all_analysis_results_387_Jatz_mid.csv", sep = ";")
Jatz_mid_raw$Hour <- as.integer(Jatz_mid_raw$Hour)
#Jatz_mid <- subset(Jatz_mid_raw, kingdom == "Animalia")
#Jatz_mid <- subset(Jatz_mid, class == "Insecta")

Jatz_up_raw <- read.csv("all_analysis_results_388_Jatz_up.csv", sep = ";")
Jatz_up_raw$Hour <- as.integer(Jatz_up_raw$Hour)
#Jatz_up <- subset(Jatz_up_raw, kingdom == "Animalia")
#Jatz_up <- subset(Jatz_up, class == "Insecta")

Mon_low_raw <- read.csv("all_analysis_results_389_Mon_low.csv", sep = ";")
Mon_low_raw$Hour <- as.integer(Mon_low_raw$Hour)
#Mon_low <- subset(Mon_low_raw, kingdom == "Animalia")
#Mon_low <- subset(Mon_low, class == "Insecta")

## The camera at the monstein mid elevation site was malfunctioning for the vast majority of the time. Data from this site is thus omitted at the moment.
# Mon_mid_raw <- read.csv("all_analysis_results_390_382_Mon_mid.csv", sep = ";")
# Mon_mid_raw$Hour <- as.integer(Mon_mid_raw$Hour)
# Mon_mid <- subset(Mon_mid_raw, kingdom == "Animalia")
# Mon_mid <- subset(Mon_mid, class == "Insecta")

Mon_up_raw <- read.csv("all_analysis_results_391_Mon_up.csv", sep = ";")
Mon_up_raw$Hour <- as.integer(Mon_up_raw$Hour)
#Mon_up <- subset(Mon_up_raw, kingdom == "Animalia")
#Mon_up <- subset(Mon_up, class == "Insecta")

## Combining the site results to transect results.
Weiss_transect <- rbind(Weiss_low_raw, Weiss_mid_raw, Weiss_up_raw)
Jatz_transect <- rbind(Jatz_low_raw, Jatz_mid_raw, Jatz_up_raw)
Monstein_transect <- rbind(Mon_low_raw, Mon_up_raw)

## Adding a column to the results that contains the time of the detection as a float number
Weiss_transect <- Weiss_transect %>%
  mutate(time = round(Hour + Minute / 60 + Second / 3600, 4))

Jatz_transect <- Jatz_transect %>%
  mutate(time = round(Hour + Minute / 60 + Second / 3600, 4))

Monstein_transect <- Monstein_transect %>%
  mutate(time = round(Hour + Minute / 60 + Second / 3600, 4))

## Combine the results of all 3 transects to a single dataframe
all_transects <- rbind(Weiss_transect, Jatz_transect, Monstein_transect)


# Data transformations ----------------------------------------------------
## Biomass regression formula
# log10	(body	mass)	=	a_region+ b_region	×	log10	(body	length)
# body length in [mm] and body mass in [mg]
a_region = -0.736
b_region = 2.191
all_transects$biomass <- 10^(a_region + b_region * log10(all_transects$Insect_length_total))

## Creating dayofyear and weekofyear column
all_transects$dayofyear <- yday(make_date(all_transects$Year, all_transects$Month, all_transects$Day))
# all_transects$weekofyear <- week(make_date(all_transects$Year, all_transects$Month, all_transects$Day))

write.csv(all_transects, "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/all_transects_raw.csv", row.names = FALSE)

## From Day 205 to Day 261 all cameras were simultaniously active
## Two dataframes are created: one with all data, and one only containing data from the period in which all cameras were simultaniously active

## 2 new columns:
## One that counts all the detections of each site and normalizes the values based on the days where the camera was active (mean daily detections for this site). 
## And one that counts the detections per day (daily_detections)
active_days_per_camera <- all_transects %>%
  group_by(site, position) %>%
  mutate(start = min(dayofyear), end = max(dayofyear)) %>%
  distinct(site, position, start, end)


all_transects_active <- all_transects %>%
  group_by(site, position) %>%
  mutate(daily_detections_per_site = (n() / days_active)) %>%
  group_by(site, position, dayofyear) %>%
  mutate(daily_detections = n()) %>%
  ungroup()

all_transects_active <- all_transects_active %>% group_by(site, position, dayofyear) %>% mutate(biomass_per_day = (sum(biomass, na.rm = TRUE)))

all_transects_active_simultaniously_active <- all_transects %>%
  group_by(site, position) %>%
  filter(dayofyear >= 205 & dayofyear <= 261) %>%
  mutate(daily_detections_per_site = (n() / (261-205+1))) %>%
  group_by(site, position, dayofyear) %>%
  mutate(daily_detections = n()) %>%
  ungroup()

all_transects_active_simultaniously_active <- all_transects_active_simultaniously_active %>% group_by(site, position, dayofyear) %>% mutate(biomass_per_day = (sum(biomass, na.rm = TRUE)))

write.csv(all_transects_active, "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/all_transects_all_data.csv", row.names = FALSE)
write.csv(all_transects_active_simultaniously_active, "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/all_transects_simultaniously_active.csv", row.names = FALSE)
