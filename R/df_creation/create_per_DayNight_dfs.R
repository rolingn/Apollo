library(tidyverse)
library(lubridate)
library(dplyr)

factor_wizzard <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])  # Correct way to reference column names
  }
  return(df)  # Return modified dataframe
}

create_per_DayNight_df <- function(all_transects_active, site, position) {
  date_seq <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  full_grid <- expand.grid(Date = date_seq, DayNight = c("Day", "Night")) %>%
    mutate(Month = month(Date), Day = day(Date))
  
  daynight_label <- function(hr) ifelse(hr >= 21, "Night", "Day")
  
  df <- all_transects_active %>% 
    filter(!(site == "Jatzhorn" & altitude == 1532 & dayofyear %in% c(212:221, 224, 228:231))) %>% 
    filter(site == {{ site }} & position == {{ position }}) %>% 
    group_by(Month, Day, Hour) %>% 
    summarise(hourly_abundance = n(),
              hourly_biomass   = sum(biomass, na.rm = TRUE),
              Diptera = sum(order == "Diptera", na.rm = TRUE),
              Hymenoptera = sum(order == "Hymenoptera", na.rm = TRUE),
              Lepidoptera = sum(order == "Lepidoptera", na.rm = TRUE),
              .groups = "drop") %>% 
    mutate(DayNight = daynight_label(Hour)) %>% 
    group_by(Month, Day, DayNight) %>% 
    summarise(abundance = sum(hourly_abundance),
              biomass   = sum(hourly_biomass),
              n_Diptera = sum(Diptera),
              n_Hymenoptera = sum(Hymenoptera),
              n_Lepidoptera = sum(Lepidoptera),
              .groups   = "drop")
  
  df <- full_grid %>% 
    left_join(df, by = c("Month", "Day", "DayNight")) %>% 
    mutate(abundance = replace_na(abundance, 0),
           biomass   = replace_na(biomass, 0),
           n_Diptera = replace_na(n_Diptera, 0),
           n_Hymenoptera = replace_na(n_Hymenoptera, 0),
           n_Lepidoptera = replace_na(n_Lepidoptera, 0),
           LED       = if_else(DayNight == "Night", "LED On", "LED Off"),
           timestamp = format(as.POSIXct(sprintf("2024-%02d-%02d %02d:00:00", 
                                                 Month, Day, if_else(DayNight == "Night", 21, 0))), 
                              "%Y-%m-%d %H:%M:%S")) %>%
    select(-Date)
  return(df)
}

force_led_off_before_date <- function(df, cutoff_date) {
  df <- df %>%
    mutate(
      LED = if_else(as.POSIXct(timestamp) < as.POSIXct(cutoff_date), "LED Off", LED)
    )
  return(df)
}

replace_zeros_with_na <- function(df, start_na, end_na) {
  df <- df %>%
    mutate(across(c(abundance, biomass),
                  ~ if_else(timestamp >= start_na & timestamp <= end_na, NA_real_, .x)))
  return(df)
}

setwd("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs")

all_transects_active <- read.csv("all_transects_simultaniously_active.csv", sep = ",") %>%
  mutate(LED = if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off")) %>%
  factor_wizzard(c("site", "LED"))

start_date <- as.POSIXct("2024-07-23 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2024-09-17 23:59:59", tz = "UTC")

per_day_WFJ_low <- create_per_DayNight_df(all_transects_active, "Weissfluhjoch", "Lower") %>% force_led_off_before_date("2024-08-13")
per_day_WFJ_mid <- create_per_DayNight_df(all_transects_active, "Weissfluhjoch", "Mid") %>% force_led_off_before_date("2024-08-13")
per_day_WFJ_up <- create_per_DayNight_df(all_transects_active, "Weissfluhjoch", "Upper") %>% force_led_off_before_date("2024-08-13")

per_day_JATZ_low <- create_per_DayNight_df(all_transects_active, "Jatzhorn", "Lower") %>% force_led_off_before_date("2024-08-09")
per_day_JATZ_low <- replace_zeros_with_na(per_day_JATZ_low, "2024-07-30 00:00:00", "2024-08-08 23:59:59")
per_day_JATZ_low <- replace_zeros_with_na(per_day_JATZ_low, "2024-08-10 21:00:00", "2024-08-11 22:59:59")
per_day_JATZ_low <- replace_zeros_with_na(per_day_JATZ_low, "2024-08-15 00:00:00", "2024-08-18 23:59:59")
per_day_JATZ_mid <- create_per_DayNight_df(all_transects_active, "Jatzhorn", "Mid") %>% force_led_off_before_date("2024-08-05")
per_day_JATZ_up <- create_per_DayNight_df(all_transects_active, "Jatzhorn", "Upper") %>% force_led_off_before_date("2024-08-05")

per_day_MON_low <- create_per_DayNight_df(all_transects_active, "Monstein", "Lower") %>% force_led_off_before_date("2024-08-05")
per_day_MON_up <- create_per_DayNight_df(all_transects_active, "Jatzhorn", "Upper") %>% force_led_off_before_date("2024-08-05")

setwd("/Volumes/Apollo/Davos/Data/CSV_tables/daily_per_site")

write.csv(per_day_WFJ_low, "weissfluhjoch_low.csv", row.names = FALSE)
write.csv(per_day_WFJ_mid, "weissfluhjoch_mid.csv", row.names = FALSE)
write.csv(per_day_WFJ_up, "weissfluhjoch_up.csv", row.names = FALSE)

write.csv(per_day_JATZ_low, "jatzhorn_low.csv", row.names = FALSE)
write.csv(per_day_JATZ_mid, "jatzhorn_mid.csv", row.names = FALSE)
write.csv(per_day_JATZ_up, "jatzhorn_up.csv", row.names = FALSE)

write.csv(per_day_MON_low, "monstein_low.csv", row.names = FALSE)
write.csv(per_day_MON_up, "monstein_up.csv", row.names = FALSE)
