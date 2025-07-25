library(ggplot2)
library(readxl)
library(tidyverse)
library(patchwork)
library(ggpattern)
library(ggbreak)
library(ggpie)
library(lme4)
library(sjPlot)
library(emmeans)


## Biomass things
c_data <- read_excel("/Volumes/Apollo/Davos/Data/CSV_tables/Cedric_data/insect_data.xlsx", 
                     sheet = "Insect_finalbiomass") %>%
  filter(`Trap Type` == "Malaise") %>%
  slice(-c(1, 6, 7, 8, 13, 14, 15, 20, 21, 22, 27, 28, 29, 34, 35, 36, 41, 42))

c_data[c(1, 5, 9, 13, 17, 21), "Date start"] <- "23.07.2023"
c_data[c(1, 5, 9, 13, 17, 21), "Comments"] <- "Anteilig vom originalstart 19.07."
c_data[c(1, 5, 9, 13, 17, 21), "Days_Period"] <- 10
c_data[c(1, 5, 9, 13, 17, 21), "Biomass_g"] <- c_data[c(1, 5, 9, 13, 17, 21), "Biomass_g"]/14 * 10

c_data[c(12, 16, 20, 24), "Date end"] <- "12.09.2023"
c_data[c(12, 16, 20, 24), "Comments"] <- "Anteilig zum originalende 13.09."
c_data[c(12, 16, 20, 24), "Days_Period"] <- 13
c_data[c(12, 16, 20, 24), "Biomass_g"] <- c_data[c(12, 16, 20, 24), "Biomass_g"]/14 * 13

c_data_clean <- c_data %>%
  group_by(Site, Standort, `Date start`, `Date end`, Days_Period) %>%
  summarize(mean_biomass_malaise = mean(Biomass_g), mean_g_per_day_malaise = mean(g_per_day)) %>%
  mutate(`Date start` = dmy(`Date start`), `Date end`   = dmy(`Date end`))

n_data  <- read.csv("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/all_transects_simultaniously_active.csv", sep = ",") %>%
  mutate(LED = if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off")) %>%
  filter(site == "Jatzhorn") %>%
  filter(dayofyear <=256) %>%
  filter(!(position == "Lower" & dayofyear > 250))

n_data <- n_data %>%
  mutate(Date = as.Date(sprintf("2024-%02d-%02d", Month, Day)))

n_data <- n_data %>%
  mutate(
    TimeWindow = case_when(
      position == "Lower" & Date >= as.Date("2024-07-23") & Date < as.Date("2024-08-02") ~ "Window 1",
      position == "Lower" & Date >= as.Date("2024-08-02") & Date < as.Date("2024-08-16") ~ "Window 2",
      position == "Lower" & Date >= as.Date("2024-08-16") & Date < as.Date("2024-08-30") ~ "Window 3",
      position == "Lower" & Date >= as.Date("2024-08-30") & Date <= as.Date("2024-09-06") ~ "Window 4",
      
      position == "Mid" & Date >= as.Date("2024-07-23") & Date < as.Date("2024-08-02") ~ "Window 1",
      position == "Mid" & Date >= as.Date("2024-08-02") & Date < as.Date("2024-08-16") ~ "Window 2",
      position == "Mid" & Date >= as.Date("2024-08-16") & Date < as.Date("2024-08-30") ~ "Window 3",
      position == "Mid" & Date >= as.Date("2024-08-30") & Date <= as.Date("2024-09-12") ~ "Window 4",
      
      position == "Upper" & Date >= as.Date("2024-07-23") & Date < as.Date("2024-08-02") ~ "Window 1",
      position == "Upper" & Date >= as.Date("2024-08-02") & Date < as.Date("2024-08-16") ~ "Window 2",
      position == "Upper" & Date >= as.Date("2024-08-16") & Date < as.Date("2024-08-30") ~ "Window 3",
      position == "Upper" & Date >= as.Date("2024-08-30") & Date <= as.Date("2024-09-12") ~ "Window 4",
      
      TRUE ~ NA_character_
    )
  )
window_dates <- tibble::tibble(
  position = rep(c("Lower", "Mid", "Upper"), each = 4),
  TimeWindow = rep(c("Window 1", "Window 2", "Window 3", "Window 4"), times = 3),
  StartDate = as.Date(c(
    "2023-07-23", "2023-08-02", "2023-08-16", "2023-08-30",  # Lower
    "2023-07-23", "2023-08-02", "2023-08-16", "2023-08-30",  # Mid
    "2023-07-23", "2023-08-02", "2023-08-16", "2023-08-30"   # Upper
  )),
  EndDate = as.Date(c(
    "2023-08-02", "2023-08-16", "2023-08-30", "2023-09-06",  # Lower
    "2023-08-02", "2023-08-16", "2023-08-30", "2023-09-12",  # Mid
    "2023-08-02", "2023-08-16", "2023-08-30", "2023-09-12"   # Upper
  ))
)

# Step 3: Summarize biomass per position and time window
n_data_clean <- n_data %>%
  filter(!is.na(TimeWindow)) %>%
  group_by(position, TimeWindow) %>%
  summarise(biomass_diopsis = sum(biomass, na.rm = TRUE) / 1000, .groups = "drop") %>%
  left_join(window_dates, by = c("position", "TimeWindow"))%>%
  select(-TimeWindow) %>%
  mutate(position = recode(position,
                           "Lower" = "Buelen",
                           "Mid"   = "Clavadeler Alp",
                           "Upper" = "Jatzhorn"))

rm(window_dates)

df_biomasses_model <- c_data_clean %>%
  left_join(n_data_clean %>% 
              select(position, StartDate, biomass_diopsis),
            by = c("Site" = "position", "Date start" = "StartDate")) %>%
  mutate(Standort = recode(Standort,
                           "1600 m" = 1532,
                           "2100 m"   = 2079,
                           "2500 m" = 2463)) %>%
  pivot_longer(
    cols = c(mean_g_per_day_malaise, biomass_diopsis),
    names_to = "method",
    values_to = "daily_biomass"
  ) %>%
  mutate(
    method = recode(method,
                    "mean_g_per_day_malaise" = "malaise",
                    "biomass_diopsis" = "diopsis")
  ) %>%
  rename(Date_start = `Date start`)

rm(c_data, c_data_clean, n_data, n_data_clean)

# 215 is 02.08.
# 247 is 03.09.
# 256 is 12.09.
# 250 is 06.09.

lineweight <-  .8


## Abundance things
c_Abundance <- read_excel("/Volumes/Apollo/Davos/Data/CSV_tables/Cedric_data/Abundance.xls") %>%
  filter(Trap_Type == "Malaise") %>%
  select(-`Site ID`, -Trap_ID, -Sample_ID, -Trap_Type) %>%
  mutate(across(-all_of(c("Site", "Sampling Number", "Date_start", "Date_end", "Comments")), ~as.numeric(.))) %>%
  mutate(Total_Malaise = rowSums(across(-all_of(c("Site", "Sampling Number", "Date_start", "Date_end", "Comments"))))) %>%
  slice(-c(1, 5, 9)) %>%
  mutate(Hymenoptera_Malaise = rowSums(across(contains("Hymenoptera")), na.rm = TRUE)) %>%
  mutate(Diptera_Malaise = rowSums(across(contains("Diptera")), na.rm = TRUE)) %>%
  mutate(Lepidoptera_Malaise = rowSums(across(starts_with("Lepidoptera")), na.rm = TRUE))

# Shifting all time windows 4 days into the future because the c_data windows start 4 days earlier. I do this to not throw away one more data point
window_dates <- tibble::tibble(
  position = rep(c("Lower", "Mid", "Upper"), each = 3),
  TimeWindow = rep(c("2", "3", "4"), times = 3),
  StartDate = as.Date(c(
    "2023-07-23", "2023-08-06", "2023-08-20",  # Lower
    "2023-07-23", "2023-08-06", "2023-08-20",  # Mid
    "2023-07-23", "2023-08-06", "2023-08-20"   # Upper
  )),
  EndDate = as.Date(c(
    "2023-08-06", "2023-08-20", "2023-09-03",  # Lower
    "2023-08-06", "2023-08-20", "2023-09-03",  # Mid
    "2023-08-06", "2023-08-20", "2023-09-03"   # Upper
  ))
)

n_Abundance <- read.csv("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/all_transects_simultaniously_active.csv", sep = ",") %>%
  mutate(LED = if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off")) %>%
  filter(site == "Jatzhorn") %>%
  filter(dayofyear <=247) %>%
  group_by(position, Month, Day, daily_detections) %>%
  summarize(Diptera = sum(order == "Diptera", na.rm = TRUE),
    Hymenoptera = sum(order == "Hymenoptera", na.rm = TRUE),
    Lepidoptera = sum(order == "Lepidoptera", na.rm = TRUE),
    .groups = "drop") %>%
  mutate(Date = as.Date(sprintf("2024-%02d-%02d", Month, Day))) %>%
  mutate(
    TimeWindow = case_when(
      position == "Lower" & Date >= as.Date("2024-07-23") & Date < as.Date("2024-08-06") ~ "2",
      position == "Lower" & Date >= as.Date("2024-08-06") & Date < as.Date("2024-08-20") ~ "3",
      position == "Lower" & Date >= as.Date("2024-08-20") & Date < as.Date("2024-09-03") ~ "4",
      
      position == "Mid" & Date >= as.Date("2024-07-23") & Date < as.Date("2024-08-06") ~ "2",
      position == "Mid" & Date >= as.Date("2024-08-06") & Date < as.Date("2024-08-20") ~ "3",
      position == "Mid" & Date >= as.Date("2024-08-20") & Date < as.Date("2024-09-03") ~ "4",
      
      position == "Upper" & Date >= as.Date("2024-07-23") & Date < as.Date("2024-08-06") ~ "2",
      position == "Upper" & Date >= as.Date("2024-08-06") & Date < as.Date("2024-08-20") ~ "3",
      position == "Upper" & Date >= as.Date("2024-08-20") & Date < as.Date("2024-09-03") ~ "4",
      TRUE ~ NA_character_))  %>%
  filter(!is.na(TimeWindow)) %>%
  group_by(position, TimeWindow) %>%
  summarise(Total_Diopsis = sum(daily_detections, na.rm = TRUE),
            Diptera_Diopsis = sum(Diptera, na.rm = TRUE),
            Hymenoptera_Diopsis = sum(Hymenoptera, na.rm = TRUE),
            Lepidoptera_Diopsis = sum(Lepidoptera, na.rm = TRUE), .groups = "drop") %>%
  mutate(position = recode(position,
                           "Lower" = "Buelen",
                           "Mid"   = "Clavadeler Alp",
                           "Upper" = "Jatzhorn")) %>%
  mutate(TimeWindow = as.numeric(TimeWindow))

rm(window_dates)

df_abundances_model <- c_Abundance %>%
  select(Site, `Sampling Number`, Date_start, Date_end, Total_Malaise, Diptera_Malaise, Hymenoptera_Malaise, Lepidoptera_Malaise) %>%
  left_join(n_Abundance %>% 
              select(Total_Diopsis, Diptera_Diopsis, Hymenoptera_Diopsis, Lepidoptera_Diopsis, position, TimeWindow),
            by = c("Site" = "position", "Sampling Number" = "TimeWindow")) %>%
  select(-`Sampling Number`) %>%
  pivot_longer(
    cols = c(Total_Malaise, Total_Diopsis),
    names_to = "method",
    values_to = "Abundance"
  ) %>%
  mutate(
    method = recode(method,
                    "Total_Malaise" = "Malaise",
                    "Total_Diopsis" = "Diopsis")
  ) %>%
  mutate(
    Standort = case_when(
      Site == "Buelen" ~ 1532,
      Site == "Clavadeler Alp" ~ 2079,
      Site == "Jatzhorn" ~ 2463,
      TRUE ~ NA_integer_  # optional fallback
    )
  )

rm(c_Abundance, n_Abundance)


# Models ------------------------------------------------------------------

m <-lmer(sqrt(daily_biomass)~method*Standort+(1|Date_start), df_biomasses_model)
altitude_seq <- seq(1532, 2463, by = 10)
em <- as.data.frame(emmeans(m, ~ method*Standort, at = list(Standort = altitude_seq), CI = T, type = "response"))
biomass <- ggplot(em, aes(Standort, response)) +
  geom_point(data = df_biomasses_model, aes(Standort, daily_biomass, color = method)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = method), alpha = .1, color = NA) +
  geom_line(aes(color = method), linewidth = .7) +
  labs(x = "Elevation [m a.s.l.]", y = paste("Biomass [mg/day]"), linetype = "Method") +
  scale_x_continuous(breaks = seq(1500, 2463, by = 200)) +
  scale_color_manual(name = "Method",
    values = c(diopsis = "steelblue4", malaise = "gray10"),
    labels = c(diopsis = "Diopsis", malaise = "Malaise")
  ) +
  scale_fill_manual(name = "Method",
    values = c(diopsis = "steelblue4", malaise = "gray10"),
    labels = c(diopsis = "Diopsis", malaise = "Malaise")
  ) +
  theme_classic() +
  theme(legend.position = c(0.15, 0.9), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14))
biomass

m <-lmer(sqrt(Abundance)~method*Standort+(1|Date_start), df_abundances_model)
altitude_seq <- seq(1532, 2463, by = 10)
em <- as.data.frame(emmeans(m, ~ method*Standort, at = list(Standort = altitude_seq), CI = T, type = "response"))
abundance <- ggplot(em, aes(Standort, response/14)) +
  geom_point(data = df_abundances_model, aes(Standort, Abundance/14, color = method)) +
  geom_ribbon(aes(ymin = lower.CL/14, ymax = upper.CL/14, fill = method), alpha = .1, color = NA) +
  geom_line(aes(color = method), linewidth = .7) +
  labs(x = "Elevation [m a.s.l.]", y = paste("Abundance [n/day] (log scale)"), linetype = "Method") +
  scale_x_continuous(breaks = seq(1500, 2463, by = 200)) +
  scale_y_log10()+
  scale_color_manual(name = "Method",
                     values = c(Diopsis = "steelblue4", Malaise = "gray10")) +
  scale_fill_manual(name = "Method",
                    values = c(Diopsis = "steelblue4", Malaise = "gray10")) +
  theme_classic() +
  theme(legend.position = c(0.9, 0.9), legend.background = element_rect(fill = "transparent"), text = element_text(size = 14))
abundance

biomass + abundance + plot_layout(guides = "collect")
ggsave("/Volumes/Apollo/Davos/Plots/Cedric_comparison_model.png", width = 9, height = 6, dpi = 300)
