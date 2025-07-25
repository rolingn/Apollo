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

## Set working directory
setwd("~/RESULTS")


## Read in all the different results. Each site has its own csv
## At the moment the data is filtered to only include detections that were classified as Animalia - Insecta
Weiss_low_raw <- read.csv("all_analysis_results_383_Weiss_low.csv", sep = ";")
Weiss_low_raw$Hour <- as.integer(Weiss_low_raw$Hour)
Weiss_low <- subset(Weiss_low_raw, kingdom == "Animalia")
Weiss_low <- subset(Weiss_low, class == "Insecta")

Weiss_mid_raw <- read.csv("all_analysis_results_384_Weiss_mid.csv", sep = ";")
Weiss_mid_raw$Hour <- as.integer(Weiss_mid_raw$Hour)
Weiss_mid <- subset(Weiss_mid_raw, kingdom == "Animalia")
Weiss_mid <- subset(Weiss_mid, class == "Insecta")

Weiss_up_raw <- read.csv("all_analysis_results_385_Weiss_up.csv", sep = ";")
Weiss_up_raw$Hour <- as.integer(Weiss_up_raw$Hour)
Weiss_up <- subset(Weiss_up_raw, kingdom == "Animalia")
Weiss_up <- subset(Weiss_up, class == "Insecta")

Jatz_low_raw <- read.csv("all_analysis_results_386_421_Jatz_low.csv", sep = ";")
Jatz_low_raw$Hour <- as.integer(Jatz_low_raw$Hour)
Jatz_low <- subset(Jatz_low_raw, kingdom == "Animalia")
Jatz_low <- subset(Jatz_low, class == "Insecta")

Jatz_mid_raw <- read.csv("all_analysis_results_387_Jatz_mid.csv", sep = ";")
Jatz_mid_raw$Hour <- as.integer(Jatz_mid_raw$Hour)
Jatz_mid <- subset(Jatz_mid_raw, kingdom == "Animalia")
Jatz_mid <- subset(Jatz_mid, class == "Insecta")

Jatz_up_raw <- read.csv("all_analysis_results_388_Jatz_up.csv", sep = ";")
Jatz_up_raw$Hour <- as.integer(Jatz_up_raw$Hour)
Jatz_up <- subset(Jatz_up_raw, kingdom == "Animalia")
Jatz_up <- subset(Jatz_up, class == "Insecta")

Mon_low_raw <- read.csv("all_analysis_results_389_Mon_low.csv", sep = ";")
Mon_low_raw$Hour <- as.integer(Mon_low_raw$Hour)
Mon_low <- subset(Mon_low_raw, kingdom == "Animalia")
Mon_low <- subset(Mon_low, class == "Insecta")

## The camera at the monstein mid elevation site was malfunctioning for the vast majority of the time. Data from this site is thus omitted at the moment.
# Mon_mid_raw <- read.csv("all_analysis_results_390_382_Mon_mid.csv", sep = ";")
# Mon_mid_raw$Hour <- as.integer(Mon_mid_raw$Hour)
# Mon_mid <- subset(Mon_mid_raw, kingdom == "Animalia")
# Mon_mid <- subset(Mon_mid, class == "Insecta")

Mon_up_raw <- read.csv("all_analysis_results_391_Mon_up.csv", sep = ";")
Mon_up_raw$Hour <- as.integer(Mon_up_raw$Hour)
Mon_up <- subset(Mon_up_raw, kingdom == "Animalia")
Mon_up <- subset(Mon_up, class == "Insecta")

## Combining the site results to transect results.
Weiss_transect <- rbind(Weiss_low, Weiss_mid, Weiss_up)
Jatz_transect <- rbind(Jatz_low, Jatz_mid, Jatz_up)
Monstein_transect <- rbind(Mon_low, Mon_up)

## Adding a column to the results that contains the time of the detection as a float number
Weiss_transect <- Weiss_transect %>%
  mutate(time = round(Hour + Minute / 60 + Second / 3600, 4))

Jatz_transect <- Jatz_transect %>%
  mutate(time = round(Hour + Minute / 60 + Second / 3600, 4))

Monstein_transect <- Monstein_transect %>%
  mutate(time = round(Hour + Minute / 60 + Second / 3600, 4))

## Combine the results of all 3 transects to a single dataframe
all_transects <- rbind(Weiss_transect, Jatz_transect, Monstein_transect)
## Dataframe that counts all the detections of each site and normalizes the values based on the days where the camera was active. Results in a value for the mean detections per day for that camera.
all_transects_active <- all_transects %>% group_by(site, position) %>% mutate(norm_count_by_active = (n() / days_active) / n())
## Does the same as above, but this time around with the days where the camera had lights on. 
all_transects_light <- all_transects %>% filter(light== "yes") %>% group_by(site, position) %>% mutate(norm_count_by_light = (n() / days_with_light) / n())

## Calculates the percentage of the total detections that took place at each hour of the day. Does that separately for every site and for both months (August and September)
percent_per_Hour <- all_transects %>% group_by(Month, site, position, Hour) %>% filter(light == "yes") %>% summarise(Count = n(), altitude = first(altitude), .groups = 'drop') %>% complete(Month, site, position, Hour = full_seq(Hour, 1), fill = list(Count = 0)) %>% fill(altitude, .direction = "downup")
percent_per_Hour <- percent_per_Hour %>% group_by(Month, site, position) %>% mutate(Percent = (Count / sum(Count)) * 100)
## Also calculates the cumulative percentage
percent_per_Hour <- percent_per_Hour %>% arrange(Month, site, position, Hour) %>% mutate(CumulativePercent = cumsum(Percent))

## The same as above, but without distinguishing between the two different months
percent_per_Hour1 <- all_transects %>% group_by(site, position, Hour) %>% filter(light == "yes") %>% summarise(Count = n(), altitude = first(altitude), .groups = 'drop') %>% complete(site, position, Hour = full_seq(Hour, 1), fill = list(Count = 0)) %>% fill(altitude, .direction = "downup")
percent_per_Hour1 <- percent_per_Hour1 %>% group_by(site, position) %>% mutate(Percent = (Count / sum(Count)) * 100)
percent_per_Hour1 <- percent_per_Hour1 %>% arrange(site, position, Hour) %>% mutate(CumulativePercent = cumsum(Percent))

## The same as above, but without distinguishiung between neither site or month. Only the altitude level is distinguished.
percent_per_Hour_all <- all_transects %>% group_by(position, Hour) %>% summarise(Count = n(), altitude = first(altitude), .groups = 'drop') %>% complete(position, Hour = full_seq(Hour, 1), fill = list(Count = 0)) %>% fill(altitude, .direction = "downup")
percent_per_Hour_all <- percent_per_Hour_all %>% group_by(position) %>% mutate(Percent = (Count / sum(Count)) * 100)
percent_per_Hour_all <- percent_per_Hour_all %>% arrange(position, Hour) %>% mutate(CumulativePercent = cumsum(Percent))

## Figures out which were the orders that were detected the most often. The top 5 are saved in the top_orders variable
top_orders <- all_transects_active %>% count(order, sort = TRUE) %>% top_n(5, n) %>% pull(order)

## Takes the all_transect_aktive dataframe and sorts out all detections that do not belong to one of the top 5 orders as calculated above.
all_transects_active <- all_transects_active %>% mutate(order_grouped = ifelse(order %in% top_orders, order, 'Other'))

## Just a dummy dataframe that is used to plot 2 different lines in a facet_grid plot.
segment_data <- data.frame(
  Month = c(8, 9),  # Replace with your actual month names
  x = c(6, 7),
  y = c(0, 0),
  xend = c(23, 23),
  yend = c(100, 100)  # Different end points for each month
)

## Lorenz curves per transect, altitude class and month. A trend following the dashed line means that insects occurred evenly throughout the day. If it tends towards the bottom, insects appeared mainly during the evening and night hours.
## Faint yellow box marks time frame where the lights on the cameras were turned on.
ggplot(percent_per_Hour, aes(x = Hour, y = CumulativePercent, fill = site, colour = site)) +
  geom_rect(aes(xmin = 21, ymin = 0, xmax = 23, ymax = 100), alpha = .5, fill = "palegoldenrod", color = NA) +
  geom_segment(data = segment_data, aes(x = x, y = y, xend = xend, yend = yend), linetype = "dashed", color = "black", inherit.aes = FALSE) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the Day", y = "Cumulative Percentage", title = "Cumulative Percent of Detections by Hour") +
  facet_grid(Month~position) +
  scale_fill_observable() +
  theme_classic()

## Same as before, but only separated by transect and altitude class.
ggplot(percent_per_Hour1, aes(x = Hour, y = CumulativePercent, fill = site, colour = site)) +
  geom_rect(aes(xmin = 21, ymin = 0, xmax = 23, ymax = 100), alpha = .5, fill = "palegoldenrod", color = NA) +
  geom_segment(aes(x = 6.5, y = 0, xend = 23, yend = 100), linetype = "dashed", color = "black") +
  geom_line(size = 1.5) +
  labs(x = "Hour of the Day", y = "Cumulative Percentage", title = "Cumulative Percent of Detections by Hour") +
  facet_grid(.~position) +
  scale_fill_observable() +
  theme_classic()

## What percentage of the total amount of detections was detected at each hour. Separated by transect, altitude class and month. Faint yellow box marks time frame where the lights on the cameras were turned on.
ggplot(percent_per_Hour, aes(x = Hour, y = Percent, fill = site)) +
  geom_rect(aes(xmin = 20.5, ymin = 0, xmax = 23.5, ymax = 40), alpha = .5, fill = "palegoldenrod", color = NA) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Hour of the Day", y = "Percentage of Total Measurements", title = "Percent Distribution of Detections by Hour") +
  facet_grid(Month~position) +
  scale_fill_observable() +
  theme_classic()

## Same as before, but only separated by transect and altitude class.
ggplot(percent_per_Hour1, aes(x = Hour, y = Percent, fill = site)) +
  geom_rect(aes(xmin = 20.5, ymin = 0, xmax = 23.5, ymax = 40), alpha = .5, fill = "palegoldenrod", color = NA) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Hour of the Day", y = "Percentage of Total Measurements", title = "Percent Distribution of Detections by Hour") +
  facet_grid(.~position) +
  scale_fill_observable() +
  theme_classic()

## Defining a custom color palette
whole_day_palette <- c("black", "midnightblue", "deepskyblue4", "lightcyan1", "lightskyblue1", "darkorange", "#09003A")
## Percentual distribution of detections by hour of day. Color coded to reflect the color palette from following plots that also show some time of day data. 
ggplot(percent_per_Hour_all, aes(x = Hour, y = Percent, fill = Hour)) +
  geom_rect(aes(xmin = 20.5, ymin = 0, xmax = 23.5, ymax = 32), alpha = .5, fill = "khaki1", color = NA) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Time of Day", y = "Percentage of total Detection", title = "Distribution of Detections per Hour") +
  facet_grid(.~position) +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_fill_gradientn(colors = whole_day_palette, breaks = seq(0, 24, by = 2)) +
  guides(fill = guide_colorbar(reverse = TRUE)) +
  theme_classic() +
  theme(text = element_text(size = 16), legend.title = element_text(size = 12)) +
  theme(legend.position = "none")

ggsave("N:/alpenv/GOES/Projects/Diopsis Cameras/RESULTS_2024/Plots/Detections_per_hour.png", width = 14, height = 9, dpi = 400)

## Distribution of detections by day with a smoothing curve. Highest rate of detections took place in the second half of august. 
ggplot(all_transects %>% mutate(date = make_date(year = 2024, month = Month, day = Day)) %>% group_by(date, site) %>% summarize(detections_per_day = n(), .groups = 'drop')) +
  #geom_vline(aes(xintercept = as.Date("2024-08-18"))) +
  geom_smooth(aes(x = date, y = detections_per_day), method = "loess", span = 0.4, se = FALSE, colour = "black") +
  geom_line(aes(x = date, y = detections_per_day, colour = site), linewidth = 1.05) +
  theme_classic() +
  theme(text = element_text(size = 16), legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  labs(x = "Date", y = "Number of detections", colour = "Transect") +
  theme(legend.position = c(.9, 0.85)) +
  scale_colour_npg()
  
ggsave("N:/alpenv/GOES/Projects/Diopsis Cameras/RESULTS_2024/Plots/Temporal_distribution.png", width = 14, height = 9, dpi = 400)

## Number of detections (normalized as detections per day) by transect and altitude class. Color coded to represent the time of day at which the detections took place.
whole_day_palette <- c("black", "midnightblue", "deepskyblue4", "lightcyan1", "lightskyblue1", "darkorange", "#09003A")
ggplot(all_transects_active, aes(position, fill = time, group = time)) +
  geom_col(aes(y = norm_count_by_active)) +
  scale_fill_gradientn(colors = whole_day_palette, breaks = seq(0, 24, by = 2)) +
  facet_grid(.~site) +
  labs(x = "Altitude class", y = "Detections per Day", fill = "Time of Day") +
  theme_classic() +
  theme(legend.key.height = unit(5, "lines")) +
  theme(text = element_text(size = 16), legend.title = element_text(size = 12)) +
  scale_y_continuous(breaks = seq(0, 120, by = 20)) +
  guides(fill = guide_colorbar(reverse = TRUE))

ggsave("N:/alpenv/GOES/Projects/Diopsis Cameras/RESULTS_2024/Plots/Detections_per_Day_with_Time.png", width = 14, height = 9, dpi = 400)

## Same as before but the y values are notmalized to represent only the preriod where the cameras had the lights on.
ggplot(all_transects_light %>% filter(light == "yes"), aes(position, fill = time, group = time)) +
  geom_col(aes(y = norm_count_by_light)) +
  scale_fill_gradientn(colors = whole_day_palette, breaks = seq(0, 24, by = 2)) +
  facet_grid(.~site) +
  theme_classic() +
  theme(text = element_text(size = 16), legend.title = element_text(size = 12)) +
  labs(x = "Altitude class", y = "Detections per Day", fill = "Time of Day") +
  theme(legend.key.height = unit(5, "lines")) +
  guides(fill = guide_colorbar(reverse = TRUE))

## Same as before but again for the entire period where the camera was active. Color coded to represent the taxonomic distribution this time around. 
ggplot(all_transects_active, aes(position, fill = order_grouped, group = order_grouped)) +
  geom_col(aes(y = norm_count_by_active)) +
  facet_grid(.~site) +
  labs(x = "Altitude class", y = "Detections per Day", fill = "Taxonomic order") +
  theme_classic() +
  theme(legend.key.height = unit(2, "lines")) +
  scale_fill_npg() +
  theme(text = element_text(size = 16), legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  scale_y_continuous(breaks = seq(0, 120, by = 20))

ggsave("N:/alpenv/GOES/Projects/Diopsis Cameras/RESULTS_2024/Plots/Top_orders_per_position.png", width = 14, height = 9, dpi = 400)

## Calculates to which percentage to the different insect orders appear at each time of the day
percent_order_per_position <- all_transects_active %>% group_by(site, position, order_grouped) %>% summarise(Count = n())
percent_order_per_position <- percent_order_per_position %>% group_by(site, position) %>% mutate(Percent = (Count / sum(Count)) * 100)
percent_order_per_position <- percent_order_per_position %>% arrange(site, position) %>% mutate(CumulativePercent = cumsum(Percent))

## Percent distribution of orders at the different sites
ggplot(percent_order_per_position, aes(x = position, y = Percent, fill = order_grouped)) +
  geom_col() +
  scale_fill_npg() +
  theme_classic() +
  labs(x = "Altitude class", y = "Percent", fill = "Taxonomic order") +
  theme(legend.key.height = unit(2, "lines")) +
  theme(text = element_text(size = 16), legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  facet_wrap(.~site)

ggsave("N:/alpenv/GOES/Projects/Diopsis Cameras/RESULTS_2024/Plots/Top_orders_per_position_percent.png", width = 14, height = 9, dpi = 400)

## Which orders appear at which time of day. Only Weissfluhjoch transect. Only for period where the light on the camera was on.
ggplot(data = Weiss_transect %>% group_by(order, Hour) %>% filter(n() >= 20) %>% filter(light == "yes"), 
                   aes(Hour, colour = order, fill = order, group = order)) +
  geom_bar(position = "stack") +
  #scale_y_continuous(limits = c(0, 900)) +
  scale_fill_npg() +
  theme_classic() + 
  facet_grid(.~position) +
  scale_color_npg() +
  ggtitle("Weissfluhjoch")

## Which orders appear at which time of day. Only Jatzhorn transect. Only for period where the light on the camera was on.
ggplot(data = Jatz_transect %>% group_by(order, Hour) %>% filter(n() >= 40) %>% filter(light == "yes"), 
       aes(Hour, colour = order, fill = order, group = order)) +
  geom_bar(position = "stack") +
  #scale_y_continuous(limits = c(0, 900)) +
  scale_fill_npg() +
  theme_classic() + 
  facet_grid(.~position) +
  scale_color_npg() +
  ggtitle("Jatzhorn")

## Which orders appear at which time of day. Only Monstein transect. Only for period where the light on the camera was on.
ggplot(data = Monstein_transect %>% group_by(order, Hour) %>% filter(n() >= 20) %>% filter(light == "yes"), 
       aes(Hour, colour = order, fill = order, group = order)) +
  geom_bar(position = "stack") +
  #scale_y_continuous(limits = c(0, 900)) +
  scale_fill_npg() +
  theme_classic() + 
  facet_grid(.~position) +
  scale_color_npg() +
  ggtitle("Monstein")

## Which orders appear at which time of day. Only separated by altitude class.
ggplot(all_transects_active %>% group_by(order_grouped, Hour), 
       aes(Hour, fill = order_grouped, group = order_grouped)) +
  geom_rect(aes(xmin = 20.5, ymin = 0, xmax = 23.5, ymax = 68), alpha = .5, fill = "khaki1", color = NA) +
  geom_col(aes(y = norm_count_by_active), position = "stack") +
  scale_fill_npg() +
  theme_classic() + 
  theme(legend.key.height = unit(2, "lines")) +
  labs(x = "Time of Day", y = "Detections per Day", fill = "Taxonomic order") +
  theme(text = element_text(size = 16), legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  facet_grid(.~position) +
  theme(legend.position = c(.85, 0.75)) +
  ggtitle("Appearance by hour of day")

ggsave("N:/alpenv/GOES/Projects/Diopsis Cameras/RESULTS_2024/Plots/Orders_by_Hour.png", width = 14, height = 9, dpi = 400)

## Detections per hour of day for each transect separately.
ggplot(Weiss_transect %>% group_by(order) %>% filter(n() >= 30) %>% filter(light == "yes"), aes(Hour, colour = position)) +
  #geom_density(stat = "density", bw = .55)
  geom_freqpoly(binwidth = 1, linewidth = 2) +
  xlim(c(0, 23)) +
  scale_color_grey() +
  ggtitle("Weissfluhjoch")

ggplot(Jatz_transect %>% group_by(order) %>% filter(n() >= 30) %>% filter(light == "yes"), aes(Hour, colour = position)) +
  geom_freqpoly(binwidth = 1, linewidth = 2) +
  xlim(c(0, 23)) +
  scale_color_grey() +
  ggtitle("Jatzhorn")

ggplot(Monstein_transect %>% group_by(order) %>% filter(n() >= 30) %>% filter(light == "yes"), aes(Hour, colour = position)) +
  geom_freqpoly(binwidth = 1, linewidth = 2) +
  xlim(c(0, 23)) +
  scale_color_grey() +
  ggtitle("Monstein")


whole_day_palette <- c("black", "midnightblue", "deepskyblue4", "lightcyan1", "lightskyblue1", "darkorange", "#09003A")
# How many individuals of each order were detected at the different elevations - Color coded for the time at which they were detected. Only for period where the light on the camera was on. All transects plotted separately.
W_order_time <- ggplot(Weiss_transect %>% group_by(order) %>% filter(n() >= 50) %>% filter(light == "yes"), aes(order)) +
  geom_bar(aes(fill = time, group = time)) +
  facet_wrap(~position) +
  scale_fill_gradientn(colors = whole_day_palette, breaks = seq(0, 24, by = 2)) +
  theme_classic() + 
  ylim(c(0, 4800)) +
  ggtitle("Weissfluhjoch") +
  guides(fill = guide_colorbar(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.6),
        legend.key.height = unit(7, "lines"))

W_order_time

J_order_time <- ggplot(Jatz_transect %>% group_by(order) %>% filter(n() >= 50) %>% filter(light == "yes"), aes(order)) +
  geom_bar(aes(fill = time, group = time)) +
  facet_wrap(~position) +
  scale_fill_gradientn(colors = whole_day_palette, breaks = seq(0, 24, by = 2)) +
  theme_classic() +
  ggtitle("Jatzhorn") +
  ylim(c(0, 4800)) +
  guides(fill = guide_colorbar(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.6),
        legend.key.height = unit(7, "lines"))

J_order_time

M_order_time <- ggplot(Monstein_transect %>% group_by(order) %>% filter(n() >= 50) %>% filter(light == "yes"), aes(order)) +
  geom_bar(aes(fill = time, group = time)) +
  facet_wrap(~position) +
  scale_fill_gradientn(colors = whole_day_palette, breaks = seq(0, 24, by = 2)) +
  theme_classic() +
  ggtitle("Monstein") +
  ylim(c(0, 4800)) +
  guides(fill = guide_colorbar(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 15, hjust = 0.6),
        legend.key.height = unit(7, "lines"))

M_order_time

## Combine all three plots above
W_order_time / J_order_time / M_order_time + plot_layout(guides = "collect")

## Same as above, but with all transects from the beginning
order_time_all <- ggplot(all_transects %>% group_by(order, site, Hour) %>% filter(light == "yes" & order %in% top_orders), aes(order)) +
  geom_bar(aes(fill = time, group = time)) +
  facet_grid(site~position) +
  scale_fill_gradientn(colors = whole_day_palette, breaks = seq(0, 24, by = 2)) +
  theme_classic() +
  #ylim(c(0, 4800)) +
  guides(fill = guide_colorbar(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.6),
        legend.key.height = unit(7, "lines"))

order_time_all
  

# Visualization of the inference scores per order. These inference scores reflect the scores of a classification up to family-level. Scores for order level need to be read in from separate csv tables.
ggplot(data = Weiss_transect %>% group_by(order) %>% filter(n() >= 30) %>% filter(light == "yes"),
       aes(order, score)) +
  geom_boxplot(colour = "black", fill = "deepskyblue4") +
  facet_wrap(~position) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Visualization of the insect lengths per order
ggplot(data = Weiss_transect %>% group_by(order) %>% filter(n() >= 30) %>% filter(light == "yes"),
       aes(order, Insect_length_total)) +
  geom_boxplot(colour = "black", fill = "deepskyblue4") +
  facet_wrap(~position) +
  theme_light() +
  ylim(c(0, 23)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## Is there an altitude trend for the 5 most common orders? Trend curve adjustable in the poly() function. 2 for quadratic function e.g
altitude_trends_by_order <- all_transects_active %>% group_by(order, altitude, site, days_with_light) %>% filter(order %in% top_orders) %>% reframe(count = n() / days_active) %>% distinct(order, altitude, site, .keep_all = TRUE)
  
ggplot(data = altitude_trends_by_order, aes(x = altitude, y = count, colour = site, shape = site)) +
  geom_smooth(aes(x = altitude, y = count), method = "lm", formula = y ~ poly(x, 1), se = FALSE, inherit.aes = FALSE, color = "black") +
  geom_point(size = 3) +
  facet_wrap(~order, scales = "free_y") +
  theme_par() +
  theme(text = element_text(size = 16), legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
  labs(x = "Altitude", y = "Count per day", colour = "Transect", shape = "Transect") +
  scale_color_npg() +
  theme(legend.position = c(.85, 0.25))

ggsave("N:/alpenv/GOES/Projects/Diopsis Cameras/RESULTS_2024/Plots/Altitude_trends_orders_poly.png", width = 14, height = 9, dpi = 400)

