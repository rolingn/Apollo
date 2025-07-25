# Load necessary libraries
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggridges)
library(forcats)

# Step 1: Read the CSV file
setwd("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/all_crops/all_crops_by_camera")
data <- read.csv("click_data.csv")

head_diff <- data.frame(head_diff = data$head_length_labeled - data$head_length_predicted, part = "Head")
abdomen_diff <- data.frame(abdomen_diff = data$abdomen_length_labeled - data$abdomen_length_predicted, part = "Abdomen")
total_diff <- data.frame(total_diff = data$total_length_labeled - data$total_length_predicted, part = "Total")
total_diff_percent_of_body_size <- round(mean(abs((data$total_length_labeled - data$total_length_predicted) / data$total_length_labeled) * 100, na.rm = TRUE), 2)

combined_diff <- bind_rows(
  data.frame(diff = data$head_length_labeled - data$head_length_predicted, part = "Head"),
  data.frame(diff = data$abdomen_length_labeled - data$abdomen_length_predicted, part = "Abdomen"),
  data.frame(diff = data$total_length_labeled - data$total_length_predicted, part = "Total")
)

mean_error <- round(mean(total_diff$total_diff, na.rm = TRUE), 3)
mean_absollute_error <- round(mean(abs(total_diff$total_diff), na.rm = TRUE), 3)

## 95% der Messungen sind innerhalb des Fehlers, den 'percentiles' beinhaltet
percentiles1 <- quantile(total_diff$total_diff, probs = c(0.025, 0.975), na.rm = TRUE)

head_plot <- ggplot(head_diff) +
  geom_density(aes(head_diff))

abdomen_plot <- ggplot(abdomen_diff) +
  geom_density(aes(abdomen_diff))

total_plot <- ggplot(total_diff) +
  geom_density(aes(total_diff))

head_plot / abdomen_plot / total_plot + plot_layout(guides = "collect")

ggplot(combined_diff) +
  geom_density(aes(diff)) +
  facet_grid(part ~ .) +
  labs(x = "Error (mm)", y = "") +
  theme_classic() +
  scale_x_continuous(limits = c(-5, 5)) + 
  theme(strip.background = element_blank(),
        text = element_text(size = 15)) +
  theme(axis.text.y = element_blank(),                 # Remove y-axis numbers
        axis.ticks.y = element_blank())  

ggplot(combined_diff, aes(x = diff, y = fct_rev(part), fill = 0.5 - abs(0.5 - after_stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, scale = 5, colour = "gray80", linewidth = 1) +
  scale_fill_viridis_c(name = "Tail probability", direction = 1, option = "G") +
  stat_density_ridges(quantile_lines = TRUE, quantiles = c(0.025, 0.975), scale = 5, colour = "gray80", fill = NA,
                      linetype = 3, linewidth = .5) +
  labs(x = "Error (mm)", y = "") +
  theme_classic() +
  scale_x_continuous(limits = c(-5, 5),
                     breaks = seq(-5, 5, 1),    # Major ticks every 1 unit
                     minor_breaks = seq(-5, 5, 0.1)) + 
  theme(strip.background = element_blank(),
        text = element_text(size = 15),
        axis.ticks.y = element_blank())


percentiles <- combined_diff %>%
  group_by(part) %>%
  summarise(lower = quantile(diff, probs = 0.025, na.rm = TRUE),
            upper = quantile(diff, probs = 0.975, na.rm = TRUE))

# Create density data for each part
density_data <- combined_diff %>%
  group_by(part) %>%
  do({
    dens <- density(.$diff, na.rm = TRUE)
    means <- mean(.$diff, na.rm = TRUE)
    data.frame(x = dens$x, y = dens$y, part = unique(.$part), mean_error = unique(means))
  })

# Join density data with percentiles and define the filled region
density_data <- density_data %>%
  left_join(percentiles, by = "part") %>%
  mutate(fill = ifelse(x >= lower & x <= upper, y, NA))  # Highlight within 95% range

density_data <- density_data %>%
  group_by(part) %>%
  mutate(
    closest_fill = fill[which.min(abs(x - mean_error))]  # Get `fill` for closest `x`
  ) %>%
  ungroup()

# Plot
ggplot(combined_diff, aes(diff)) +
  geom_area(data = density_data, aes(x = x, y = fill), fill = "lightblue", alpha = 1) +
  geom_vline(data = percentiles, aes(xintercept = lower), linetype = 3) +
  geom_vline(data = percentiles, aes(xintercept = upper), linetype = 3) +
  geom_segment(data = density_data, aes(x = mean_error, xend = mean_error, y = 0, yend = closest_fill)) +
  geom_density() +
  facet_grid(part ~ .) +
  labs(x = "Error (mm)", y = "") +
  theme_classic() +
  scale_x_continuous(limits = c(-5, 5),
                     breaks = seq(-5, 5, 1),    # Major ticks every 1 unit
                     minor_breaks = seq(-5, 5, 0.1)) + 
  theme(strip.background = element_blank(),
        text = element_text(size = 15),
        axis.text.y = element_blank(),  # Remove y-axis numbers
        axis.ticks.y = element_blank())

ggsave("/Volumes/Apollo/Davos/Plots/Size_Errors.png", width = 8, height = 5, dpi = 300)
