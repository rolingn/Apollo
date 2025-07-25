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
library(broom.mixed)
library(zoo)
library(psych)
library(MASS)
library(performance)

## Models included here:
## Abundances (detections per day) model
## Biomass (biomass per day) model
## Diptera Abundance Model
## Diptera Biomass Model
## Lepidoptera Abundance Model
## Lepitoptera Biomass Model
## Hymenoptera Abundance Model
## Hymenoptera Biomass Model

## Function definition:
# Creates the dataframes that contain only data from a specific order
## Outputs a _per_day (daily detections and biomass), 
## a per_hour (hourly detections and biomass), 
## and a _d_n (daily detections and biomass but for LED On/Off separately) df. 
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

## Function definition to create a comparison plot of model predictions with and without LED differentiation
build_master_plot <- function(df_without_LED, df_with_LED, order) {
  site_colors <- c("Jatzhorn" = "steelblue4",
                     "Monstein" = "darkolivegreen3",
                     "Weissfluhjoch" = "darkgoldenrod1")
  m <-lmer(sqrt(daily_detections)~site*altitude+(1|dayofyear), df_without_LED)
  em <- as.data.frame(emmeans(m, ~ site*altitude, at = list(altitude = altitude_seq), CI = T, type = "response"))
  abundance <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = .1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = "Elevation [m a.s.l.]", y = paste(order, "Abundance [n/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    scale_x_continuous(breaks = seq(1400, max(df_without_LED$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14))
  
  m <-lmer(sqrt(detections)~site*altitude*LED+(1|dayofyear), df_with_LED)
  em <- as.data.frame(emmeans(m, ~ site*altitude*LED, at = list(altitude = altitude_seq), CI = T, type = "response"))
  abundance_LED <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = .1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = "Elevation [m a.s.l.]", y = paste(order, "Abundance [n/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    #scale_x_continuous(breaks = seq(1400, max(detections_per_day$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14)) +
    facet_wrap(~LED)
  
  m <- lmer(sqrt(biomass_per_day)~site*altitude+(1|dayofyear), df_without_LED)
  em <- as.data.frame(emmeans(m, ~ site*altitude, at = list(altitude = altitude_seq), CI = T, type = "response"))
  biomass <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = 0.1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = "Elevation [m a.s.l.]", y = paste(order, "Biomass [mg/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    scale_x_continuous(breaks = seq(1400, max(df_without_LED$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.15), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14))
  
  m <- lmer(sqrt(biomass)~site*altitude*LED+(1|dayofyear), df_with_LED)
  em <- as.data.frame(emmeans(m, ~ site*altitude*LED, at = list(altitude = altitude_seq), CI = T, type = "response"))
  biomass_LED <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = 0.1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = "Elevation [m a.s.l.]", y = paste(order, "Biomass [mg/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    #scale_x_continuous(breaks = seq(1400, max(detections_per_day$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.15), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14)) +
    facet_wrap(~LED)
  
  abundance + biomass + abundance_LED + biomass_LED +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom",
          plot.tag.position = c(0.05, .97))
}

build_orders_plot <- function(df_with_LED_d, 
                              df_with_LED_h,
                              df_with_LED_l) {
  site_colors <- c("Jatzhorn" = "steelblue4",
                   "Monstein" = "darkolivegreen3",
                   "Weissfluhjoch" = "darkgoldenrod1")
  m <-lmer(sqrt(detections)~site*altitude*LED+(1|dayofyear), df_with_LED_d)
  em <- as.data.frame(emmeans(m, ~ site*altitude*LED, at = list(altitude = altitude_seq), CI = T, type = "response"))
  abundance_LED_d <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = .1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = NULL, y = paste("Diptera Abundance [n/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    #scale_x_continuous(breaks = seq(1400, max(detections_per_day$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14)) +
    facet_wrap(~LED)
  
  m <- lmer(sqrt(biomass)~site*altitude*LED+(1|dayofyear), df_with_LED_d)
  em <- as.data.frame(emmeans(m, ~ site*altitude*LED, at = list(altitude = altitude_seq), CI = T, type = "response"))
  biomass_LED_d <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = 0.1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = NULL, y = paste("Diptera Biomass [mg/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    #scale_x_continuous(breaks = seq(1400, max(detections_per_day$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.15), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14)) +
    facet_wrap(~LED)
  
  m <-lmer(sqrt(detections)~site*altitude*LED+(1|dayofyear), df_with_LED_h)
  em <- as.data.frame(emmeans(m, ~ site*altitude*LED, at = list(altitude = altitude_seq), CI = T, type = "response"))
  abundance_LED_h <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = .1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = NULL, y = paste("Hymenoptera Abundance [n/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    #scale_x_continuous(breaks = seq(1400, max(detections_per_day$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14)) +
    facet_wrap(~LED)
  
  m <- lmer(sqrt(biomass)~site*altitude*LED+(1|dayofyear), df_with_LED_h)
  em <- as.data.frame(emmeans(m, ~ site*altitude*LED, at = list(altitude = altitude_seq), CI = T, type = "response"))
  biomass_LED_h <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = 0.1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = NULL, y = paste("Hymenoptera Biomass [mg/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    #scale_x_continuous(breaks = seq(1400, max(detections_per_day$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.15), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14)) +
    facet_wrap(~LED)
  
  m <-lmer(sqrt(detections)~site*altitude*LED+(1|dayofyear), df_with_LED_l)
  em <- as.data.frame(emmeans(m, ~ site*altitude*LED, at = list(altitude = altitude_seq), CI = T, type = "response"))
  abundance_LED_l <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = .1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = "Elevation [m a.s.l.]", y = paste("Lepidoptera Abundance [n/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    #scale_x_continuous(breaks = seq(1400, max(detections_per_day$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.9), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14)) +
    facet_wrap(~LED)
  
  m <- lmer(sqrt(biomass)~site*altitude*LED+(1|dayofyear), df_with_LED_l)
  em <- as.data.frame(emmeans(m, ~ site*altitude*LED, at = list(altitude = altitude_seq), CI = T, type = "response"))
  biomass_LED_l <- ggplot(em, aes(altitude, response, color = site)) +
    geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = site), alpha = 0.1, color = NA) +
    geom_line(linewidth = .7) +
    labs(x = "Elevation [m a.s.l.]", y = paste("Lepidoptera Biomass [mg/day]"),
         colour = "Transect", fill = "Transect", linetype = "Transect") +
    #scale_x_continuous(breaks = seq(1400, max(detections_per_day$altitude), by = 200)) +
    #scale_colour_npg() +
    scale_color_manual(values = site_colors) +
    scale_fill_manual(values = site_colors) +
    theme_classic() +
    theme(legend.position = c(0.9, 0.15), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14)) +
    facet_wrap(~LED)
  
  abundance_LED_d + biomass_LED_d + 
    abundance_LED_h + biomass_LED_h + 
    abundance_LED_l + biomass_LED_l +
    plot_layout(ncol = 2) +
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "collect") & 
    theme(legend.position = "bottom",
          plot.tag.position = c(0.05, .97))
}

## Helper function to convert a list of variables into a factor
factor_wizzard <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])  # Correct way to reference column names
  }
  return(df)  # Return modified dataframe
}

## Set working directory
setwd("/github/RESULTS_2024/result_csvs")

# Reading in the data ------------------------------------------------------------------------
## For all data gathering and transformations, see script Reading_and_processing_raw_csvs.R
all_transects_active <- read.csv("all_transects_simultaniously_active.csv", sep = ",") %>%
  mutate(LED = if_else(light == "no", "LED Off",if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off")))

all_transects_raw <- read.csv("all_transects_raw.csv", sep = ",") %>%
  mutate(LED = if_else(light == "no", "LED Off",if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off")))

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

# Rolling sum (Minute steps) of hourly abundance
create_rollsum_df(filter = NA, df_name = "per_hour")

create_rollsum_df(filter = "Diptera", df_name = "diptera")
create_rollsum_df(filter = "Hymenoptera", df_name = "hyme")
create_rollsum_df(filter = "Lepidoptera", df_name = "lepi")

detections_per_day <- factor_wizzard(detections_per_day, c("site"))
per_hour <- factor_wizzard(per_hour, c("site", "position", "LED"))
dp_d_n <- factor_wizzard(dp_d_n, c("site", "position", "LED"))

## Create dataframes containing only Diptera, Lepidoptera and Hymenoptera:
create_order_df(all_transects_raw, "Lepidoptera", "lepi")
create_order_df(all_transects_raw, "Diptera", "diptera")
create_order_df(all_transects_raw, "Hymenoptera", "hyme")
create_order_df(all_transects_raw, "others", "others")

# The Models --------------------------------------------------------------------------
altitude_seq <- seq(min(all_transects_active$altitude), max(all_transects_active$altitude), by = 10)

## Checking for normality
shapiro.test(detections_per_day$daily_detections) ## Abundance data not normally distributed :(
shapiro.test(detections_per_day$biomass_per_day) ## Biomass data not normally distributed :(

detections_per_day$sqrt_daily_detections <- sqrt(detections_per_day$daily_detections)
car::qqPlot(detections_per_day$daily_detections) # Squareroot looks the best out of log and cube transormation
detections_per_day$sqrt_biomass_per_day <- sqrt(detections_per_day$biomass_per_day)
car::qqPlot(detections_per_day$sqrt_biomass_per_day) # Squareroot looks the best out of log and cube transormation
hist(detections_per_day$daily_detections, breaks = 30, main = "Histogram of Abundance", xlab = "Abundance")
hist(detections_per_day$sqrt_biomass_per_day, breaks = 30, main = "Histogram of Abundance", xlab = "Biomass")
dp_d_n$sqrt_detections <- sqrt(dp_d_n$detections)
dp_d_n$sqrt_biomass <- sqrt(dp_d_n$biomass)

shapiro.test(detections_per_day$sqrt_daily_detections)
shapiro.test(detections_per_day$sqrt_biomass_per_day)

## QQ Plots look good. Proceeding with squareroot transformed response variables


## Abundances (detections per day) model:
m <-lmer(sqrt(daily_detections)~site*altitude+(1|dayofyear), detections_per_day)
mLED <-lmer(sqrt(detections)~site*altitude*LED+(1|dayofyear), dp_d_n)

car::Anova(m)
summary(m)
tab_model(m)

car::Anova(mLED)
summary(mLED)
tab_model(mLED)


## Biomass (biomass per day) model:
m <- lmer(sqrt(biomass_per_day)~site*altitude+(1|dayofyear), detections_per_day)
mLED <- lmer(sqrt(biomass)~site*altitude*LED+(1|dayofyear), dp_d_n)

car::Anova(m)
summary(m)
tab_model(m)

car::Anova(mLED)
summary(mLED)
tab_model(mLED)


## Abundance and Biomass masterplot (publication plot):
build_master_plot(detections_per_day, dp_d_n, "Total")
ggsave("/Volumes/Apollo/Davos/Plots/Total_Abundance_Biomass_masterplot1.png", width = 10, height = 11, dpi = 300)
## If you want to view single plots:
abundance
abundance_LED
biomass
biomass_LED


## Diptera Abundance Model:
m <-lmer(sqrt(daily_detections)~site*altitude+(1|dayofyear), diptera_per_day)
mLED <- lmer(sqrt(detections)~site*altitude*LED+(1|dayofyear), diptera_d_n)

car::Anova(m)
summary(m)
tab_model(m)

car::Anova(mLED)
summary(mLED)
tab_model(mLED)

## Diptera Biomass Model:
m <-lmer(sqrt(biomass_per_day)~site*altitude+(1|dayofyear), diptera_per_day)
mLED <- lmer(sqrt(biomass)~site*altitude*LED+(1|dayofyear), diptera_d_n)

car::Anova(m)
summary(m)
tab_model(m)

car::Anova(mLED)
summary(mLED)
tab_model(mLED)


## Diptera Master Plot:
build_master_plot(diptera_per_day, diptera_d_n, "Diptera")
ggsave("/Volumes/Apollo/Davos/Plots/Diptera_Abundance_Biomass_masterplot.png", width = 10, height = 11, dpi = 300)
## If you want to view single plots:
abundance
abundance_LED
biomass
biomass_LED


## Hymenoptera Abundance Model:
m <-lmer(sqrt(daily_detections)~site*altitude+(1|dayofyear), hyme_per_day)
mLED <- lmer(sqrt(detections)~site*altitude*LED+(1|dayofyear), hyme_d_n)

car::Anova(m)
summary(m)
tab_model(m)

car::Anova(mLED)
summary(mLED)
tab_model(mLED)

## Hymenoptera Biomass Model:
m <-lmer(sqrt(biomass_per_day)~site*altitude+(1|dayofyear), hyme_per_day)
mLED <- lmer(sqrt(biomass)~site*altitude*LED+(1|dayofyear), hyme_d_n)

car::Anova(m)
summary(m)
tab_model(m)

car::Anova(mLED)
summary(mLED)
tab_model(mLED)


## Hymenoptera Master Plot:
build_master_plot(hyme_per_day, hyme_d_n, "Hymenoptera")
ggsave("/Volumes/Apollo/Davos/Plots/Hymenoptera_Abundance_Biomass_masterplot.png", width = 10, height = 11, dpi = 300)
## If you want to view single plots:
abundance
abundance_LED
biomass
biomass_LED


## Lepidoptera Abundance Model:
m <-lmer(sqrt(daily_detections)~site*altitude+(1|dayofyear), lepi_per_day)
mLED <- lmer(sqrt(detections)~site*altitude*LED+(1|dayofyear), lepi_d_n)

car::Anova(m)
summary(m)
tab_model(m)

car::Anova(mLED)
summary(mLED)
tab_model(mLED)

## Lepitoptera Biomass Model:
m <-lmer(sqrt(biomass_per_day)~site*altitude+(1|dayofyear), lepi_per_day)
mLED <- lmer(sqrt(biomass)~site*altitude*LED+(1|dayofyear), lepi_d_n)

car::Anova(m)
summary(m)
tab_model(m)

car::Anova(mLED)
summary(mLED)
tab_model(mLED)

## Lepidoptera Master Plot:
build_master_plot(lepi_per_day, lepi_d_n, "Lepidoptera")
ggsave("/Volumes/Apollo/Davos/Plots/Lepidoptera_Abundance_Biomass_masterplot.png", width = 10, height = 11, dpi = 300)
## If you want to view single plots:
abundance
abundance_LED
biomass
biomass_LED


## New order Master Plot:
build_orders_plot(diptera_d_n, hyme_d_n, lepi_d_n)
ggsave("/Volumes/Apollo/Davos/Plots/AllOrders_Abundance_Biomass_LED_masterplot.png", width = 10, height = 11, dpi = 300)

## all others Master Plot (out of interest):
build_master_plot(others_per_day, others_d_n, "Others")
#ggsave("/Volumes/Apollo/Davos/Plots/Others_Abundance_Biomass_masterplot.png", width = 10, height = 11, dpi = 300)
## If you want to view single plots:
abundance
abundance_LED
biomass
biomass_LED


## Diurnal plots
day <- filter(per_hour, Hour >= 5 & Hour < 21)

sunrises <- data.frame(
  site = c("Weissfluhjoch", "Weissfluhjoch", "Weissfluhjoch", 
           "Jatzhorn", "Jatzhorn", "Jatzhorn", 
           "Monstein", "Monstein", "Monstein"),
  position = c("Lower", "Mid", "Upper", 
           "Lower", "Mid", "Upper", 
           "Lower", "Mid", "Upper"),
  sunrise = c(6+46/60, 6+42/69, 6+36/60, 
              9+42/60, 8+14/60, 8+00/60, 
              10+00/60, NA, 6+44/60),
  sol_max = c(13+20/60, 13+20/60, 13+20/60,
              13+20/60, 13+20/60, 13+20/60,
              13+20/60, NA, 13+20/60),
  sunset = c(18+41/60, 18+46/60, 19+14/60,
              18+40/60, 20+06/60, 20+06/60,
              17+35/60, NA, 20+00/60))
dummy_row <- data.frame(
  Hour = NA, hourly_abundance = NA, position = "Mid", site = "Monstein")

day <- filter(per_hour_rollsum, Hour >= 5 & Hour < 21)
night <- filter(per_hour_rollsum, Hour >= 21 & Hour < 24)
day <- bind_rows(day, dummy_row)
night <- bind_rows(night, dummy_row)

ggplot(data = day, aes(timeofday, hourly_abundance)) +
  geom_vline(data = sunrises, aes(xintercept = sunrise, colour = "Sunrise @ site"), linewidth = 1) +
  geom_vline(data = sunrises, aes(xintercept = sol_max, colour = "Solar maximum"), linewidth = 1) +
  geom_vline(data = sunrises, aes(xintercept = sunset, colour = "Sunset @ site"), linewidth = 1) +
  geom_line(aes(colour = "All orders")) +
  geom_line(data = filter(diptera_rollsum, Hour >= 5 & Hour < 21), aes(timeofday, hourly_abundance, colour = "Diptera")) +
  geom_line(data = filter(hyme_rollsum, Hour >= 5 & Hour < 21), aes(timeofday, hourly_abundance, colour = "Hymenoptera")) +
  geom_line(data = filter(lepi_rollsum, Hour >= 5 & Hour < 21), aes(timeofday, hourly_abundance, colour = "Lepidoptera")) +
  facet_wrap(position~site, scale = "free_y") +
  labs(x = "Time of day", y = "Abundance [n/hour]", colour = "") +  # Fix legend title
  theme_clean() +
  theme(legend.position = c(0.5, 0.5), legend.background = element_rect(fill = "transparent"), text = element_text(size = 13)) +
  scale_color_manual(values = c("All orders" = "black",
                                "Diptera" = "red", 
                                "Hymenoptera" = "springgreen2", 
                                "Lepidoptera" = "maroon2",
                                "Sunrise @ site" = "gold1",
                                "Solar maximum" = "indianred2",
                                "Sunset @ site" = "steelblue2"))

ggplot(data = per_hour_rollsum, aes(timeofday, hourly_abundance)) +
  geom_vline(data = sunrises, aes(xintercept = sunrise, colour = "Sunrise @ site"), linewidth = 1) +
  geom_vline(data = sunrises, aes(xintercept = sol_max, colour = "Solar maximum"), linewidth = 1) +
  geom_vline(data = sunrises, aes(xintercept = sunset, colour = "Sunset @ site"), linewidth = 1) +
  geom_line(aes(colour = "All orders")) +
  geom_line(data = diptera_rollsum, aes(timeofday, hourly_abundance, colour = "Diptera")) +
  geom_line(data = hyme_rollsum, aes(timeofday, hourly_abundance, colour = "Hymenoptera")) +
  geom_line(data = lepi_rollsum, aes(timeofday, hourly_abundance, colour = "Lepidoptera")) +
  facet_wrap(position~site, scale = "free_y") +
  labs(x = "Time of day", y = "Abundance [n/hour]", colour = "") +  # Fix legend title
  theme_clean() +
  theme(legend.position = c(0.5, 0.5), legend.background = element_rect(fill = "transparent"), text = element_text(size = 13)) +
  scale_color_manual(values = c("All orders" = "black",
                                "Diptera" = "red", 
                                "Hymenoptera" = "springgreen2", 
                                "Lepidoptera" = "maroon2",
                                "Sunrise @ site" = "gold1",
                                "Solar maximum" = "indianred2",
                                "Sunset @ site" = "steelblue2"))

ggsave("/Volumes/Apollo/Davos/R/EDA_plots/Overall_Diurnal_combined_DayNight.png", width = 11, height = 9, dpi = 300)
