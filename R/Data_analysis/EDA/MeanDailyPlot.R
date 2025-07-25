library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(ggplot2)

factor_wizzard <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])  # Correct way to reference column names
  }
  return(df)  # Return modified dataframe
}

setwd("/Volumes/Apollo/Davos/Data/CSV_tables/diopsis_and_weather_master_dfs")
df <- read.csv("hourly.csv", sep = ",") %>%
  factor_wizzard(c("LED", "site", "position")) %>%
  mutate(timestamp = as.POSIXct(timestamp))
df_3h <- read.csv("3hourly.csv", sep = ",") %>%
  factor_wizzard(c("LED", "site", "position")) %>%
  mutate(timestamp = as.POSIXct(timestamp))
rm(factor_wizzard)

df <- df %>% filter(!is.na(hourly_abundance))
df_3h <- df_3h %>% filter(!is.na(hourly_abundance))

mean <- df %>% group_by(site, Hour) %>% summarise(mean_abundance = mean(hourly_abundance),
                                                  sd = sd(hourly_abundance))

ggplot(mean, aes(x = Hour, y = mean_abundance)) +
  geom_ribbon(aes(ymin = mean_abundance - sd, ymax = mean_abundance + sd), alpha = 0.2, fill = "blue") +
  geom_line(color = "blue", size = 1) +
  facet_wrap(~site) +
  coord_cartesian(ylim = c(0, 50))
