library(dplyr)
library(tidyr)
library(lubridate)

factor_wizzard <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])  # Correct way to reference column names
  }
  return(df)  # Return modified dataframe
}


# Hourly ------------------------------------------------------------------


setwd("/Volumes/Apollo/Davos/Data/CSV_tables/smet_as_csv/200mGrid")
JATZ_low_weather <- read.csv("jatzhorn_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
JATZ_mid_weather <- read.csv("jatzhorn_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
JATZ_up_weather <- read.csv("jatzhorn_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))

MON_low_weather <- read.csv("monstein_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
MON_mid_weather <- read.csv("monstein_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
MON_up_weather <- read.csv("monstein_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))

WFJ_low_weather <- read.csv("weissfluhjoch_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
WFJ_mid_weather <- read.csv("weissfluhjoch_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
WFJ_up_weather <- read.csv("weissfluhjoch_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))

setwd("/Volumes/Apollo/Davos/Data/CSV_tables/smet_as_csv_rollmean3d/200mGrid")
JATZ_low_weather_rollmean <- read.csv("jatzhorn_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
JATZ_mid_weather_rollmean <- read.csv("jatzhorn_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
JATZ_up_weather_rollmean <- read.csv("jatzhorn_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))

MON_low_weather_rollmean <- read.csv("monstein_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
MON_mid_weather_rollmean <- read.csv("monstein_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
MON_up_weather_rollmean <- read.csv("monstein_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))

WFJ_low_weather_rollmean <- read.csv("weissfluhjoch_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
WFJ_mid_weather_rollmean <- read.csv("weissfluhjoch_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
WFJ_up_weather_rollmean <- read.csv("weissfluhjoch_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))

setwd("/Volumes/Apollo/Davos/Data/CSV_tables/hourly_per_site")
JATZ_low <- read.csv("jatzhorn_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
JATZ_mid <- read.csv("jatzhorn_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
JATZ_up <- read.csv("jatzhorn_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))

MON_low <- read.csv("monstein_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
#MON_mid <- read.csv("monstein_mid.csv", sep = ",") %>%
#  mutate(timestamp = as.POSIXct(timestamp))
MON_up <- read.csv("monstein_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))

WFJ_low <- read.csv("weissfluhjoch_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
WFJ_mid <- read.csv("weissfluhjoch_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
WFJ_up <- read.csv("weissfluhjoch_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))

JATZ_low <- cbind(JATZ_low_weather, JATZ_low[ , !(names(JATZ_low) %in% "timestamp")]) %>%
  mutate(site = "Jatzhorn", position = "Lower", Elevation = 1532)
JATZ_mid <- cbind(JATZ_mid_weather, JATZ_mid[ , !(names(JATZ_mid) %in% "timestamp")]) %>%
  mutate(site = "Jatzhorn", position = "Middle", Elevation = 2079)
JATZ_up <- cbind(JATZ_up_weather, JATZ_up[ , !(names(JATZ_up) %in% "timestamp")]) %>%
  mutate(site = "Jatzhorn", position = "Upper", Elevation = 2463)

MON_low <- cbind(MON_low_weather, MON_low[ , !(names(MON_low) %in% "timestamp")]) %>%
  mutate(site = "Monstein", position = "Lower", Elevation = 1427)
#MON_mid <- cbind(MON_mid_weather, MON_mid[ , !(names(MON_mid) %in% "timestamp")]) %>%
#  mutate(site = "Monstein", position = "Middle", Elevation = 1952)
MON_up <- cbind(MON_up_weather, MON_up[ , !(names(MON_up) %in% "timestamp")]) %>%
  mutate(site = "Monstein", position = "Upper", Elevation = 2632)

WFJ_low <- cbind(WFJ_low_weather, WFJ_low[ , !(names(WFJ_low) %in% "timestamp")]) %>%
  mutate(site = "Weissfluhjoch", position = "Lower", Elevation = 1563)
WFJ_mid <- cbind(WFJ_mid_weather, WFJ_mid[ , !(names(WFJ_mid) %in% "timestamp")]) %>%
  mutate(site = "Weissfluhjoch", position = "Middle", Elevation = 2102)
WFJ_up <- cbind(WFJ_up_weather, WFJ_up[ , !(names(WFJ_up) %in% "timestamp")]) %>%
  mutate(site = "Weissfluhjoch", position = "Upper", Elevation = 2540)

JATZ_low <- cbind(JATZ_low, JATZ_low_weather_rollmean[ , !(names(JATZ_low_weather_rollmean) %in% "timestamp_r3d")])
JATZ_mid <- cbind(JATZ_mid, JATZ_mid_weather_rollmean[ , !(names(JATZ_mid_weather_rollmean) %in% "timestamp_r3d")])
JATZ_up <- cbind(JATZ_up, JATZ_up_weather_rollmean[ , !(names(JATZ_up_weather_rollmean) %in% "timestamp_r3d")])

MON_low <- cbind(MON_low, MON_low_weather_rollmean[ , !(names(MON_low_weather_rollmean) %in% "timestamp_r3d")])
#MON_mid <- cbind(MON_mid, MON_mid_weather_rollmean[ , !(names(MON_mid_weather_rollmean) %in% "timestamp_r3d")])
MON_up <- cbind(MON_up, MON_up_weather_rollmean[ , !(names(MON_up_weather_rollmean) %in% "timestamp_r3d")])

WFJ_low <- cbind(WFJ_low, WFJ_low_weather_rollmean[ , !(names(WFJ_low_weather_rollmean) %in% "timestamp_r3d")])
WFJ_mid <- cbind(WFJ_mid, WFJ_mid_weather_rollmean[ , !(names(WFJ_mid_weather_rollmean) %in% "timestamp_r3d")])
WFJ_up <- cbind(WFJ_up, WFJ_up_weather_rollmean[ , !(names(WFJ_up_weather_rollmean) %in% "timestamp_r3d")])

df <- rbind(JATZ_low, JATZ_mid, JATZ_up,
            MON_low, MON_up,
            WFJ_low, WFJ_mid, WFJ_up) %>%
  select(-Month, -Day)%>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))
rm(JATZ_low_weather, JATZ_mid_weather, JATZ_up_weather,
   JATZ_low_weather_rollmean, JATZ_mid_weather_rollmean, JATZ_up_weather_rollmean,
   MON_low_weather, MON_mid_weather, MON_up_weather,
   MON_low_weather_rollmean, MON_mid_weather_rollmean, MON_up_weather_rollmean,
   WFJ_low_weather, WFJ_mid_weather, WFJ_up_weather,
   WFJ_low_weather_rollmean, WFJ_mid_weather_rollmean, WFJ_up_weather_rollmean)

df <- factor_wizzard(df, c("LED", "site", "position"))
write.csv(df, "/Volumes/Apollo/Davos/Data/CSV_tables/diopsis_and_weather_master_dfs/hourly.csv", row.names = FALSE)


# 3Hourly -----------------------------------------------------------------


setwd("/Volumes/Apollo/Davos/Data/CSV_tables/smet_as_csv_3hour/200mGrid")
JATZ_low_weather <- read.csv("jatzhorn_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
JATZ_mid_weather <- read.csv("jatzhorn_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
JATZ_up_weather <- read.csv("jatzhorn_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))

MON_low_weather <- read.csv("monstein_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
MON_mid_weather <- read.csv("monstein_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
MON_up_weather <- read.csv("monstein_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))

WFJ_low_weather <- read.csv("weissfluhjoch_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
WFJ_mid_weather <- read.csv("weissfluhjoch_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))
WFJ_up_weather <- read.csv("weissfluhjoch_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  mutate(day_of_year = yday(timestamp))

setwd("/Volumes/Apollo/Davos/Data/CSV_tables/smet_as_csv_3hour_rollmean3d/200mGrid")
JATZ_low_weather_rollmean <- read.csv("jatzhorn_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
JATZ_mid_weather_rollmean <- read.csv("jatzhorn_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
JATZ_up_weather_rollmean <- read.csv("jatzhorn_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))

MON_low_weather_rollmean <- read.csv("monstein_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
MON_mid_weather_rollmean <- read.csv("monstein_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
MON_up_weather_rollmean <- read.csv("monstein_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))

WFJ_low_weather_rollmean <- read.csv("weissfluhjoch_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
WFJ_mid_weather_rollmean <- read.csv("weissfluhjoch_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))
WFJ_up_weather_rollmean <- read.csv("weissfluhjoch_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  rename_with(~ paste0(., "_r3d"))

setwd("/Volumes/Apollo/Davos/Data/CSV_tables/3hourly_per_site")
JATZ_low <- read.csv("jatzhorn_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
JATZ_mid <- read.csv("jatzhorn_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
JATZ_up <- read.csv("jatzhorn_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))

MON_low <- read.csv("monstein_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
#MON_mid <- read.csv("monstein_mid.csv", sep = ",") %>%
#  mutate(timestamp = as.POSIXct(timestamp))
MON_up <- read.csv("monstein_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))

WFJ_low <- read.csv("weissfluhjoch_low.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
WFJ_mid <- read.csv("weissfluhjoch_mid.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))
WFJ_up <- read.csv("weissfluhjoch_up.csv", sep = ",") %>%
  mutate(timestamp = as.POSIXct(timestamp))

JATZ_low <- cbind(JATZ_low_weather, JATZ_low[ , !(names(JATZ_low) %in% "timestamp")]) %>%
  mutate(site = "Jatzhorn", position = "Lower", Elevation = 1532)
JATZ_mid <- cbind(JATZ_mid_weather, JATZ_mid[ , !(names(JATZ_mid) %in% "timestamp")]) %>%
  mutate(site = "Jatzhorn", position = "Middle", Elevation = 2079)
JATZ_up <- cbind(JATZ_up_weather, JATZ_up[ , !(names(JATZ_up) %in% "timestamp")]) %>%
  mutate(site = "Jatzhorn", position = "Upper", Elevation = 2463)

MON_low <- cbind(MON_low_weather, MON_low[ , !(names(MON_low) %in% "timestamp")]) %>%
  mutate(site = "Monstein", position = "Lower", Elevation = 1427)
#MON_mid <- cbind(MON_mid_weather, MON_mid[ , !(names(MON_mid) %in% "timestamp")]) %>%
#  mutate(site = "Monstein", position = "Middle", Elevation = 1952)
MON_up <- cbind(MON_up_weather, MON_up[ , !(names(MON_up) %in% "timestamp")]) %>%
  mutate(site = "Monstein", position = "Upper", Elevation = 2632)

WFJ_low <- cbind(WFJ_low_weather, WFJ_low[ , !(names(WFJ_low) %in% "timestamp")]) %>%
  mutate(site = "Weissfluhjoch", position = "Lower", Elevation = 1563)
WFJ_mid <- cbind(WFJ_mid_weather, WFJ_mid[ , !(names(WFJ_mid) %in% "timestamp")]) %>%
  mutate(site = "Weissfluhjoch", position = "Middle", Elevation = 2102)
WFJ_up <- cbind(WFJ_up_weather, WFJ_up[ , !(names(WFJ_up) %in% "timestamp")]) %>%
  mutate(site = "Weissfluhjoch", position = "Upper", Elevation = 2540)

JATZ_low <- cbind(JATZ_low, JATZ_low_weather_rollmean[ , !(names(JATZ_low_weather_rollmean) %in% "timestamp_r3d")])
JATZ_mid <- cbind(JATZ_mid, JATZ_mid_weather_rollmean[ , !(names(JATZ_mid_weather_rollmean) %in% "timestamp_r3d")])
JATZ_up <- cbind(JATZ_up, JATZ_up_weather_rollmean[ , !(names(JATZ_up_weather_rollmean) %in% "timestamp_r3d")])

MON_low <- cbind(MON_low, MON_low_weather_rollmean[ , !(names(MON_low_weather_rollmean) %in% "timestamp_r3d")])
#MON_mid <- cbind(MON_mid, MON_mid_weather_rollmean[ , !(names(MON_mid_weather_rollmean) %in% "timestamp_r3d")])
MON_up <- cbind(MON_up, MON_up_weather_rollmean[ , !(names(MON_up_weather_rollmean) %in% "timestamp_r3d")])

WFJ_low <- cbind(WFJ_low, WFJ_low_weather_rollmean[ , !(names(WFJ_low_weather_rollmean) %in% "timestamp_r3d")])
WFJ_mid <- cbind(WFJ_mid, WFJ_mid_weather_rollmean[ , !(names(WFJ_mid_weather_rollmean) %in% "timestamp_r3d")])
WFJ_up <- cbind(WFJ_up, WFJ_up_weather_rollmean[ , !(names(WFJ_up_weather_rollmean) %in% "timestamp_r3d")])

df_3h <- rbind(JATZ_low, JATZ_mid, JATZ_up,
            MON_low, MON_up,
            WFJ_low, WFJ_mid, WFJ_up) %>%
  select(-Month, -Day)%>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S"))
rm(JATZ_low_weather, JATZ_mid_weather, JATZ_up_weather,
   JATZ_low_weather_rollmean, JATZ_mid_weather_rollmean, JATZ_up_weather_rollmean,
   MON_low_weather, MON_mid_weather, MON_up_weather,
   MON_low_weather_rollmean, MON_mid_weather_rollmean, MON_up_weather_rollmean,
   WFJ_low_weather, WFJ_mid_weather, WFJ_up_weather,
   WFJ_low_weather_rollmean, WFJ_mid_weather_rollmean, WFJ_up_weather_rollmean)

df_3h <- factor_wizzard(df_3h, c("LED", "site", "position"))
write.csv(df_3h, "/Volumes/Apollo/Davos/Data/CSV_tables/diopsis_and_weather_master_dfs/3hourly.csv", row.names = FALSE)
rm(factor_wizzard,
   JATZ_low, JATZ_mid, JATZ_up,
   MON_low, MON_up,
   WFJ_low, WFJ_mid, WFJ_up)
