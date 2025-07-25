library(ggplot2)
library(tidyverse)
library(patchwork)
library(dplyr)
library(rphylopic)

## alles elevation!!

## Set working directory
setwd("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs")

# Reading in the data ------------------------------------------------------------------------
## For all data gathering and transformations, see script Reading_and_processing_raw_csvs.R

all_transects_raw <- read.csv("all_transects_raw.csv", sep = ",") %>%
  mutate(LED = if_else(Hour >= 21 & Hour <= 24, "LED On", "LED Off"))

top_orders <- df %>%
  count(Order, sort = TRUE) %>%
  mutate(Order_grouped = ifelse(row_number() <= 10, Order, "Others")) %>%
  group_by(Order_grouped) %>%
  summarise(Count = sum(n), .groups = "drop")

uuid <- get_uuid(name = "Diptera", n = 45)
diptera <- get_phylopic(uuid = uuid[45])

uuid <- get_uuid(name = "Coleoptera", n = 5)
coleoptera <- get_phylopic(uuid = uuid[5])

uuid <- get_uuid(name = "Hemiptera", n = 43)
hemiptera <- get_phylopic(uuid = uuid[43])

uuid <- get_uuid(name = "Hymenoptera", n = 1)
hymenoptera <- get_phylopic(uuid = uuid)

uuid <- get_uuid(name = "Lepidoptera", n = 29)
lepidoptera <- get_phylopic(uuid = uuid[29])

# Make it a pie chart
ggplot(all_transects_raw %>%
    count(order, sort = TRUE) %>%
    mutate(Order_grouped = ifelse(row_number() <= 5, order, "Others")) %>%
    group_by(Order_grouped) %>%
    summarise(Count = sum(n), .groups = "drop") %>%
    arrange(desc(Count)) %>%
    mutate(Prop = Count / sum(Count),
      Order_for_plot = factor(Order_grouped, levels = rev(Order_grouped)),
      Order_for_legend = factor(Order_grouped, levels = Order_grouped)),
  aes(x = "", y = Prop, fill = Order_for_plot)) +
  geom_col(width = 1) +
  geom_vline(xintercept = .5, color = "white", alpha = 1) +
  add_phylopic(img = diptera, x = 1.25, y = .27, height = .1, fill = "black", alpha = .7) +
  add_phylopic(img = coleoptera, x = 1.25, y = .61, height = .065, fill = "black", alpha = .7) +
  add_phylopic(img = hemiptera, x = 1.25, y = .85, height = .07, fill = "white", alpha = .5) +
  add_phylopic(img = hymenoptera, x = 1.25, y = .922, height = .07, fill = "white", alpha = .5) +
  add_phylopic(img = lepidoptera, x = 1.25, y = .978, height = .07, fill = "white", alpha = .5) +
  coord_polar(theta = "y") +
  scale_fill_viridis_d()+
  labs(fill = "") +
  theme_void() +
  theme(plot.title = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.51, .902),
        legend.justification = c("left", "top"),
        legend.background = element_rect(fill = "white", color = "white"))

