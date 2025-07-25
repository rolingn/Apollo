# Load required packages
library(mgcv)
library(MASS)
library(tuneR)
library(ggplot2)
library(gganimate)
library(dplyr)
library(gifski)
library(av)

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
df <- df %>% filter(!is.na(hourly_abundance))

setwd("/Volumes/Apollo/Davos/R/Weather_Models/Hourly/Abundance")
mod <- readRDS("m_reduced2_no_s.rds")


# Generate 1 cycle of Hour data
samples_per_cycle <- 100  # for 440 Hz at 44100 Hz sampling rate
hour_grid <- seq(0, 23, length.out = samples_per_cycle)
newdata <- data.frame(
  Hour = hour_grid,
  ISWR = mean(df$ISWR, na.rm = TRUE),
  RH = mean(df$RH, na.rm = TRUE),
  P = mean(df$P, na.rm = TRUE),
  TA = mean(df$TA, na.rm = TRUE),
  VW = mean(df$VW, na.rm = TRUE),
  day_of_year = mean(df$day_of_year, na.rm = TRUE),
  LED = levels(df$LED)[1],
  site = levels(df$site)[1]
)

# Extract model matrix (basis) for s(Hour)
Xp_full <- predict(mod, newdata = newdata, type = "lpmatrix")
col_hour <- grepl("s\\(Hour\\)", colnames(Xp_full))
Xp_hour <- Xp_full[, col_hour]
beta <- coef(mod)
Vb <- vcov(mod)
beta_hour <- beta[col_hour]
Vb_hour <- Vb[col_hour, col_hour]

# Number of cycles (i.e., duration * pitch)
n_cycles <- 440 * 10  # 3 seconds of A4 tone

# Smooth modulation settings
step_scale <- 0.005  # smaller = slower/smoother morphing
chol_V <- chol(Vb_hour * step_scale)

# Initialize with mean coefficients
beta_walk <- matrix(NA, nrow = length(beta_hour), ncol = n_cycles)
beta_walk[, 1] <- beta_hour

# Random walk in coefficient space
for (i in 2:n_cycles) {
  eps <- t(chol_V) %*% rnorm(length(beta_hour))
  beta_walk[, i] <- beta_walk[, i - 1] + eps
}

# Generate waveforms for each cycle
samples <- apply(beta_walk, 2, function(b) Xp_hour %*% b)
waveform <- as.vector(samples)

# Normalize to [-1, 1]
waveform <- waveform / max(abs(waveform))

# Export to WAV
wave_obj <- Wave(left = as.integer(waveform * 32767), samp.rate = 44100, bit = 16)
writeWave(wave_obj, "/Users/Nils/Desktop/gam_hour_smoothmod_440hz.wav")

cat("Exported: gam_hour_smoothmod_440hz.wav\n")


# === Parameters ===
fps <- 30                    # Frames per second
duration_sec <- 10           # Duration of animation in seconds
n_frames <- fps * duration_sec

# === Step 1: Assume you already have these from your model ===
# - hour_grid: sequence of "hour" values (0 to 24)
# - Xp_hour: basis matrix for s(Hour)
# - beta_walk: matrix of modulated beta vectors (ncol = total audio cycles, e.g. 4400)

# Subset beta_walk to match animation frames
frame_indices <- round(seq(1, ncol(beta_walk), length.out = n_frames))
beta_anim <- beta_walk[, frame_indices]  # One coefficient set per frame

# Generate waveform samples for each frame
samples_anim <- apply(beta_anim, 2, function(b) {
  y <- Xp_hour %*% b
  y[length(y)] <- y[1]  # Ensure cyclic wraparound
  y
})

# === Step 2: Build animation-ready data frame ===
hour_grid <- seq(0, 24, length.out = samples_per_cycle + 1)[- (samples_per_cycle + 1)]

plot_data <- data.frame(
  Hour = rep(hour_grid, times = n_frames),
  amplitude = as.vector(samples_anim),
  frame = rep(1:n_frames, each = length(hour_grid))
)

# Normalize each frame's amplitude (optional but helpful)
plot_data <- plot_data %>%
  group_by(frame) %>%
  mutate(amplitude = amplitude / max(abs(amplitude))) %>%
  ungroup()

# === Step 3: Build the ggplot animation ===
p <- ggplot(plot_data, aes(x = Hour, y = amplitude, group = frame)) +
  geom_line(color = "purple", size = 1.2) +
  theme_minimal(base_size = 16) +
  labs(
    title = "Cyclic GAM Waveform Modulation — Frame {frame}",
    x = "Hour of Day", y = "Amplitude"
  ) +
  transition_manual(frame)

# === Step 4: Render and save as MP4 ===
animate(
  p, fps = fps, nframes = n_frames, width = 800, height = 400,
  renderer = av_renderer("/Users/Nils/Desktop/gam_waveform_real_time.mp4")
)

# Optional: To save as GIF instead
# animate(p, fps = fps, width = 800, height = 400, renderer = gifski_renderer("gam_waveform.gif"))