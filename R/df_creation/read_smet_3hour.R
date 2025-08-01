library(tidyverse)
library(lubridate)
library(dplyr)

read_smet <- function(file_path) {
  lines <- readLines(file_path)
  
  # Find the header and data section
  header_start <- grep("\\[HEADER\\]", lines)
  data_start <- grep("\\[DATA\\]", lines)
  
  # Extract and parse header
  header_lines <- lines[(header_start + 1):(data_start - 1)]
  header <- list()
  for (line in header_lines) {
    kv <- strsplit(line, "=")[[1]]
    if (length(kv) == 2) {
      key <- trimws(kv[1])
      value <- trimws(kv[2])
      header[[key]] <- value
    }
  }
  
  # Get column names from 'fields' entry
  fields <- unlist(strsplit(header[["fields"]], " "))
  colnames <- fields[fields != ""]
  
  # Read the data section
  data_lines <- lines[(data_start + 1):length(lines)]
  data <- read.table(text = paste(data_lines, collapse = "\n"), header = FALSE)
  colnames(data) <- colnames
  
  # Convert timestamp to POSIXct if present
  if ("timestamp" %in% colnames(data)) {
    data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  }
  
  return(list(header = header, data = data))
}

convert_kelvin_to_C <- function(dataframe) {
  temp_cols <- grep("^T", names(dataframe), value = TRUE)
  dataframe[temp_cols] <- lapply(dataframe[temp_cols], function(x) x - 273.15)
  return(dataframe)
}

start_date <- as.POSIXct("2024-07-23 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2024-09-17 23:59:59", tz = "UTC")

# Final Data
setwd("/Volumes/Apollo/Davos/Wetterdaten/Weather200mGrid/spatial_interpolations_Cams")

JATZ_low <- read_smet("Jatz_1.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

JATZ_mid <- read_smet("Jatz_2.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

JATZ_up <- read_smet("Jatz_3.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

MON_low <- read_smet("Mons_1.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

MON_mid <- read_smet("Mons_2.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

MON_up <- read_smet("Mons_3.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

WFJ_low <- read_smet("WFJ_1.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

WFJ_mid <- read_smet("WFJ_2.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

WFJ_up <- read_smet("WFJ_3.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

setwd("/Volumes/Apollo/Davos/Data/CSV_tables/smet_as_csv_3hour/200mGrid")
write.csv(JATZ_low, "jatzhorn_low.csv", row.names = FALSE)
write.csv(JATZ_mid, "jatzhorn_mid.csv", row.names = FALSE)
write.csv(JATZ_up, "jatzhorn_up.csv", row.names = FALSE)

write.csv(MON_low, "monstein_low.csv", row.names = FALSE)
write.csv(MON_mid, "monstein_mid.csv", row.names = FALSE)
write.csv(MON_up, "monstein_up.csv", row.names = FALSE)

write.csv(WFJ_low, "weissfluhjoch_low.csv", row.names = FALSE)
write.csv(WFJ_mid, "weissfluhjoch_mid.csv", row.names = FALSE)
write.csv(WFJ_up, "weissfluhjoch_up.csv", row.names = FALSE)


# Preliminary Data
setwd("/Volumes/Apollo/Davos/Wetterdaten/Rohdaten")

WFJ_low <- read_smet("SLF2.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(PSUM = if_else(PSUM == -999.0, 0, PSUM), timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  select(-DW, -HS, -TSS, -VW_MAX) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

WFJ_mid <- read_smet("WFJ2.smet")$data %>%
  filter(timestamp >= start_date & timestamp <= end_date) %>%
  convert_kelvin_to_C() %>%
  mutate(PSUM = if_else(PSUM == -999.0, 0, PSUM), timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")) %>%
  select(-DW, -HS, -TS1, -TS2, -TS3, -TSOIL10, -TSOIL30, -TSOIL50, -TSS, -VW_MAX) %>%
  mutate(timestamp = ymd_hms(timestamp),
         time_window = floor_date(timestamp, unit = "3 hours")) %>%
  group_by(time_window) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(timestamp = format(time_window, "%Y-%m-%d %H:%M:%S")) %>%
  select(-time_window)

setwd("/Volumes/Apollo/Davos/Data/CSV_tables/smet_as_csv_3hour/preliminary")
write.csv(WFJ_low, "weissfluhjoch_low.csv", row.names = FALSE)
write.csv(WFJ_mid, "weissfluhjoch_mid.csv", row.names = FALSE)
