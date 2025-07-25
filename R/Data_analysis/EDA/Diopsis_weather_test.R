library(lubridate)
library(dplyr)
library(tidyr)

setwd("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Images_RAW_and_analyzed/")

weiss_low <- read.csv("383_Weiss_low/DIOPSIS-383/data_2024_DIOPSIS-383.csv", sep = ",")
weiss_low <- weiss_low %>%
  fill(temperature, .direction = "downup") %>%
  fill(light, .direction = "downup") %>%
  drop_na(timestamp) %>% 
  mutate(
    timestamp = ymd_hms(as.character(timestamp)),  # Ensure it's a datetime object
    month = month(timestamp),
    day = day(timestamp),
    hour = hour(timestamp),
    minute = minute(timestamp)
  )
weiss_low_mean <-  weiss_low %>%
  group_by(month, day) %>%
  summarise(mean_temperature = mean(temperature, na.rm = TRUE), .groups = "drop")

ggplot(weiss_low, aes(x = timestamp, y = temperature)) +
  geom_line() +
  scale_x_continuous(limits = c(ymd_hms("2024-08-13 00:00:00"), ymd_hms("2024-09-18 00:00:00")))

weiss_mid <- read.csv("384_Weiss_mid/DIOPSIS-384/data_2024_DIOPSIS-384.csv", sep = ",")
weiss_mid <- weiss_mid %>%
  fill(temperature, .direction = "downup") %>%
  fill(light, .direction = "downup") %>%
  drop_na(timestamp) %>% 
  mutate(
    timestamp = ymd_hms(as.character(timestamp)),  # Ensure it's a datetime object
    month = month(timestamp),
    day = day(timestamp),
    hour = hour(timestamp),
    minute = minute(timestamp)
  )

ggplot(weiss_mid, aes(x = timestamp, y = temperature)) +
  geom_line() +
  scale_x_continuous(limits = c(ymd_hms("2024-08-13 00:00:00"), ymd_hms("2024-09-18 00:00:00")))
