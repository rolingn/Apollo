library(dplyr)
library(lubridate)
library(emmeans)
library(tidyverse)
library(ggplot2)
setwd("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs")

create_order_df <- function(all_transects_raw, order_name, df_prefix){
  if (order_name != "others"){
    temp_order_df <- subset(all_transects_raw, order == order_name) 
  }
  else{
    temp_order_df <- subset(all_transects_raw, order != "Hymenoptera" & order != "Diptera" & order != "Lepidoptera")
  }
  temp_df <- temp_order_df %>%
    filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
    group_by(site, altitude) %>%
    filter(dayofyear >= 226 & dayofyear <= 261) %>% ## <---- comment out if you want to work with all data
    mutate(mean_daily_detections = (n() / (261-226+1))) %>%
    group_by(site, altitude, dayofyear) %>%
    mutate(daily_detections = n()) %>%
    ungroup() %>%
    mutate(light = recode(light, "yes1" = "yes")) %>%
    group_by(site, altitude, dayofyear) %>%
    mutate(biomass_per_day = (sum(biomass, na.rm = TRUE))) %>%
    distinct(site, altitude, dayofyear, daily_detections, biomass_per_day) %>%  # Get distinct combinations of site, altitude, dayofyear, daily_detections, biomass_per_day
    group_by(site, altitude) %>%  # Group by site and altitude
    complete(dayofyear = seq(226, 261, by = 1), 
             fill = list(daily_detections = 0, biomass_per_day = 0)) %>%  # Complete dayofyear within the min and max range and fill missing daily_detections and biomass_per_day with 0
    ungroup() %>%
    filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) # Deletes user specified rows that correspond to days at Jatzhorn lower, where the camera was not working. Does not make sense to set these values to 0...
  assign(paste0(df_prefix, "_per_day"), temp_df, envir = .GlobalEnv)
  
  temp_df <- temp_order_df %>%
    filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
    group_by(LED, site, altitude) %>%
    filter(dayofyear >= 226 & dayofyear <= 261) %>% ## <---- comment out if you want to work with all data
    mutate(mean_daily_detections = (n() / (261-226+1))) %>%
    group_by(LED, site, altitude, dayofyear) %>%
    mutate(detections = n()) %>%
    ungroup() %>%
    mutate(light = recode(light, "yes1" = "yes")) %>%
    group_by(LED, site, altitude, dayofyear) %>%
    mutate(biomass = (sum(biomass, na.rm = TRUE))) %>%
    distinct(LED, site, altitude, dayofyear, detections, biomass) %>%  # Get distinct combinations of site, altitude, dayofyear, daily_detections, biomass_per_day
    group_by(LED, site, altitude) %>%  # Group by site and altitude
    complete(dayofyear = seq(226, 261, by = 1), 
             fill = list(detections = 0, biomass = 0)) %>%  # Complete dayofyear within the min and max range and fill missing daily_detections and biomass_per_day with 0
    ungroup() %>%
    filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) # Deletes user specified rows that correspond to days at Jatzhorn lower, where the camera was not working. Does not make sense to set these values to 0...
  assign(paste0(df_prefix, "_d_n"), temp_df, envir = .GlobalEnv)
  
  temp_df <- temp_order_df %>%
    filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
    filter(dayofyear >= 226 & dayofyear <= 261) %>% ## <---- comment out if you want to work with all data
    group_by(site, altitude, position, Hour) %>%
    summarise(hourly_abundance = n(), hourly_biomass = sum(biomass, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    distinct(site, altitude, position, Hour, hourly_abundance, hourly_biomass, .keep_all = F) %>%  # Get distinct combinations of site, altitude, dayofyear, daily_detections, biomass_per_day
    group_by(site, altitude, position) %>%
    complete(Hour = seq(0, 23, by = 1), 
             fill = list(hourly_abundance = 0, hourly_biomass = 0)) %>%
    fill(position, .direction = "downup") %>%
    ungroup() %>%
    mutate(LED = if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off"))
  assign(paste0(df_prefix, "_per_hour"), temp_df, envir = .GlobalEnv)
}

create_rollsum_df <- function(filter, df_name){
  if (!is.na(filter)){
    temp_order_df <- subset(all_transects_active, order == filter) 
  }
  else{
    temp_order_df <- all_transects_active
  }
  per_hour_rollsum <- temp_order_df %>%
    filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
    group_by(site, altitude, position, Hour, Minute) %>%
    mutate(minute_abundance = n()) %>%
    ungroup() %>%
    distinct(site, altitude, position, Hour, Minute, minute_abundance, .keep_all = F) %>%  # Get distinct combinations of site, altitude, dayofyear, daily_detections, biomass_per_day
    group_by(site, altitude, position) %>%
    complete(Hour = seq(0, 23, by = 1), Minute = seq(0, 59, by = 1), 
             fill = list(minute_abundance = 0)) %>%
    fill(position, .direction = "downup") %>%
    group_by(site, altitude, position) %>%
    mutate(hourly_abundance = rollsum(minute_abundance, k = 60, fill = NA, align = "center")) %>%
    ungroup() %>%
    mutate(timeofday = Hour + Minute / 60)
  assign(paste0(df_name, "_rollsum"), per_hour_rollsum, envir = .GlobalEnv)
}

factor_wizzard <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])  # Correct way to reference column names
  }
  return(df)  # Return modified dataframe
}

# Reading in the data ------------------------------------------------------------------------
## For all data gathering and transformations, see script Reading_and_processing_raw_csvs.R
all_transects_active <- read.csv("all_transects_simultaniously_active.csv", sep = ",") %>%
  mutate(LED = if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off"))

all_transects_raw <- read.csv("all_transects_raw.csv", sep = ",") %>%
  mutate(LED = if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off"))

all_transects_active <- factor_wizzard(all_transects_active, c("site", "LED"))
all_transects_raw <- factor_wizzard(all_transects_raw, c("site", "LED"))

## Summarizes the Sites into a smaller dataframe
detections_per_day <- all_transects_active %>%
  filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
  filter(dayofyear>=226) %>% ## else, the starting dayofyear is 205 -> dont forget to edit the complete function below in this case
  distinct(site, altitude, dayofyear, daily_detections, biomass_per_day) %>%  # Get distinct combinations of site, altitude, dayofyear, daily_detections, biomass_per_day
  group_by(site, altitude) %>%  # Group by site and altitude
  complete(dayofyear = seq(226, 261, by = 1), 
           fill = list(daily_detections = 0, biomass_per_day = 0)) %>%  # Complete dayofyear within the min and max range and fill missing daily_detections and biomass_per_day with 0
  ungroup() %>%
  filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) # Deletes the rows that correspond to days at Jatzhorn lower, where the camera was not working. Does not make sense to set these values to 0...

dp_d_n <- all_transects_active %>% # detections per day and night
  filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
  filter(dayofyear >= 226) %>%
  group_by(LED, site, altitude, position, dayofyear) %>%
  summarise(detections = n(), biomass = sum(biomass, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct(LED, site, altitude, position, dayofyear, detections, biomass, .keep_all = F) %>%  # Get distinct combinations of site, altitude, dayofyear, daily_detections, biomass_per_day
  group_by(LED, site, altitude, position) %>%
  complete(dayofyear = seq(226, 261, by = 1), 
           fill = list(detections = 0, biomass = 0)) %>%
  fill(position, .direction = "downup") %>%
  ungroup() %>%
  filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231)))

per_hour <- all_transects_active %>%
  filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
  group_by(site, altitude, position, Hour) %>%
  summarise(hourly_abundance = n(), hourly_biomass = sum(biomass, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  distinct(site, altitude, position, Hour, hourly_abundance, hourly_biomass, .keep_all = F) %>%  # Get distinct combinations of site, altitude, dayofyear, daily_detections, biomass_per_day
  group_by(site, altitude, position) %>%
  complete(Hour = seq(0, 23, by = 1), 
           fill = list(hourly_abundance = 0, hourly_biomass = 0)) %>%
  fill(position, .direction = "downup") %>%
  ungroup() %>%filter(Hour != 0) %>% # Remove row of midnight, because sometimes light was switched on for a short duration after 12am
  mutate(LED = if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off"))

detections_per_day <- factor_wizzard(detections_per_day, c("site"))
per_hour <- factor_wizzard(per_hour, c("site", "position", "LED"))
dp_d_n <- factor_wizzard(dp_d_n, c("site", "position", "LED"))

## Create dataframes containing only Diptera, Lepidoptera and Hymenoptera:
create_order_df(all_transects_raw, "Lepidoptera", "lepi")
create_order_df(all_transects_raw, "Diptera", "diptera")
create_order_df(all_transects_raw, "Hymenoptera", "hyme")
create_order_df(all_transects_raw, "others", "others")

dusk_dawn_size <- all_transects_active %>%
  filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
  filter(dayofyear>=226) %>% 
  group_by(site, altitude) %>% 
  group_by(Hour %in% c(6,7), Hour %in% c(19, 20)) %>%
  mean(Insect_length_total)

dusk_dawn_size <- all_transects_active %>%
  filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
  filter(dayofyear >= 226) %>%
  mutate(
    time_period = case_when(
      Hour %in% c(6, 7) ~ "Dawn",
      Hour %in% c(19, 20) ~ "Dusk",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(time_period)) %>%
  group_by(site, altitude, time_period) %>%
  summarise(mean_insect_length = mean(Insect_length_total, na.rm = TRUE),
            sd = sd(Insect_length_total, na.rm = TRUE), .groups = "drop")


all_transects_active %>%
  filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
  filter(dayofyear >= 226) %>%
  mutate(
    time_period = case_when(
      Hour %in% c(6, 7) ~ "Dawn",
      Hour %in% c(19, 20) ~ "Dusk",
      Hour %in% c(8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18) ~ "Day",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(time_period)) %>%
  ggplot(aes(x = time_period, y = Insect_length_total)) +
  geom_boxplot() +
  facet_wrap(~ site + altitude) +
  theme_minimal() +
  labs(
    title = "Insect Lengths at Dawn and Dusk",
    x = "Time of Day",
    y = "Body Length"
  )

all_transects_active %>%
  filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>%
  filter(dayofyear >= 226) %>%
  ggplot(aes(x = time, y = Insect_length_total)) +
  geom_point(alpha = 0.05) +
  geom_smooth(color = "blue") + 
  facet_wrap(~ position) +
  xlim(5, 24)
