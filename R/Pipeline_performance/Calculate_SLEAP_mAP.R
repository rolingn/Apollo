# Load necessary libraries
library(dplyr)

# Define the function to get precision at given recall thresholds
get_precision_at_recall_thresholds <- function(prec_recall_df, recall_thresholds) {
  
  # Create an empty vector to store precision values
  precision_values <- numeric(length(recall_thresholds))
  
  # Iterate over recall thresholds
  for (i in 1:length(recall_thresholds)) {
    # Find the rows where recall is <= current threshold
    valid_rows <- which(prec_recall_df$recall <= recall_thresholds[i])
    
    # If there are valid rows, get the precision corresponding to the maximum recall
    if (length(valid_rows) > 0) {
      # Select the row with the highest recall <= threshold
      max_recall_row <- valid_rows[which.max(prec_recall_df$recall[valid_rows])]
      
      # Store the corresponding precision value
      precision_values[i] <- prec_recall_df$precision[max_recall_row]
    } else {
      # If no valid rows, set precision to NA
      precision_values[i] <- NA
    }
  }
  
  # Create and return a data frame with recall thresholds and corresponding precision
  return(data.frame(
    recall_threshold = recall_thresholds,
    precision_at_recall = precision_values
  ))
}

# Step 1: Read the CSV file
setwd("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/all_crops/all_crops_by_camera")
data <- read.csv("click_data.csv")

data <- data %>%
  arrange(desc(OKS_score))
# Assuming your data is stored in a dataframe called 'data'
data <- data %>%
  mutate(FN = ifelse(!is.na(total_length_labeled) & (is.na(total_length_predicted) | total_length_predicted == 0), TRUE, FALSE))

relevant_data <- data[, c(16:16, 18:28)]
relevant_data[, 3:12] <- lapply(relevant_data[, 3:12], as.logical)

relevant_data <- relevant_data %>%
  mutate(cum_FN = cumsum(FN),
         cum_TP.5 = cumsum(TP.5),
         cum_TP.55 = cumsum(TP.55),
         cum_TP.6 = cumsum(TP.6),
         cum_TP.65 = cumsum(TP.65),
         cum_TP.7 = cumsum(TP.7),
         cum_TP.75 = cumsum(TP.75),
         cum_TP.8 = cumsum(TP.8),
         cum_TP.85 = cumsum(TP.85),
         cum_TP.9 = cumsum(TP.9),
         cum_TP.95 = cumsum(TP.95),
         cum_FP.5 = cumsum(!TP.5),
         cum_FP.55 = cumsum(!TP.55),
         cum_FP.6 = cumsum(!TP.6),
         cum_FP.65 = cumsum(!TP.65),
         cum_FP.7 = cumsum(!TP.7),
         cum_FP.75 = cumsum(!TP.75),
         cum_FP.8 = cumsum(!TP.8),
         cum_FP.85 = cumsum(!TP.85),
         cum_FP.9 = cumsum(!TP.9),
         cum_FP.95 = cumsum(!TP.95))

prec_recall.5 <- data.frame(
  precision = relevant_data$cum_TP.5 / (relevant_data$cum_TP.5 + relevant_data$cum_FP.5),
  recall = relevant_data$cum_TP.5 / (relevant_data$cum_TP.5 + relevant_data$cum_FN)
)
prec_recall.55 <- data.frame(
  precision = relevant_data$cum_TP.55 / (relevant_data$cum_TP.55 + relevant_data$cum_FP.55),
  recall = relevant_data$cum_TP.55 / (relevant_data$cum_TP.55 + relevant_data$cum_FN)
)
prec_recall.6 <- data.frame(
  precision = relevant_data$cum_TP.6 / (relevant_data$cum_TP.6 + relevant_data$cum_FP.6),
  recall = relevant_data$cum_TP.6 / (relevant_data$cum_TP.6 + relevant_data$cum_FN)
)
prec_recall.65 <- data.frame(
  precision = relevant_data$cum_TP.65 / (relevant_data$cum_TP.65 + relevant_data$cum_FP.65),
  recall = relevant_data$cum_TP.65 / (relevant_data$cum_TP.65 + relevant_data$cum_FN)
)
prec_recall.7 <- data.frame(
  precision = relevant_data$cum_TP.7 / (relevant_data$cum_TP.7 + relevant_data$cum_FP.7),
  recall = relevant_data$cum_TP.7 / (relevant_data$cum_TP.7 + relevant_data$cum_FN)
)
prec_recall.75 <- data.frame(
  precision = relevant_data$cum_TP.75 / (relevant_data$cum_TP.75 + relevant_data$cum_FP.75),
  recall = relevant_data$cum_TP.75 / (relevant_data$cum_TP.75 + relevant_data$cum_FN)
)
prec_recall.8 <- data.frame(
  precision = relevant_data$cum_TP.8 / (relevant_data$cum_TP.8 + relevant_data$cum_FP.8),
  recall = relevant_data$cum_TP.8 / (relevant_data$cum_TP.8 + relevant_data$cum_FN)
)
prec_recall.85 <- data.frame(
  precision = relevant_data$cum_TP.85 / (relevant_data$cum_TP.85 + relevant_data$cum_FP.85),
  recall = relevant_data$cum_TP.85 / (relevant_data$cum_TP.85 + relevant_data$cum_FN)
)
prec_recall.9 <- data.frame(
  precision = relevant_data$cum_TP.9 / (relevant_data$cum_TP.9 + relevant_data$cum_FP.9),
  recall = relevant_data$cum_TP.9 / (relevant_data$cum_TP.9 + relevant_data$cum_FN)
)
prec_recall.95 <- data.frame(
  precision = relevant_data$cum_TP.95 / (relevant_data$cum_TP.95 + relevant_data$cum_FP.95),
  recall = relevant_data$cum_TP.95 / (relevant_data$cum_TP.95 + relevant_data$cum_FN)
)

recall_thresholds <- seq(0, 1, length.out = 101)
prec_at_thresholds.5 <- get_precision_at_recall_thresholds(prec_recall.5, recall_thresholds)
prec_at_thresholds.55 <- get_precision_at_recall_thresholds(prec_recall.55, recall_thresholds)
prec_at_thresholds.6 <- get_precision_at_recall_thresholds(prec_recall.6, recall_thresholds)
prec_at_thresholds.65 <- get_precision_at_recall_thresholds(prec_recall.65, recall_thresholds)
prec_at_thresholds.7 <- get_precision_at_recall_thresholds(prec_recall.7, recall_thresholds)
prec_at_thresholds.75 <- get_precision_at_recall_thresholds(prec_recall.75, recall_thresholds)
prec_at_thresholds.8 <- get_precision_at_recall_thresholds(prec_recall.8, recall_thresholds)
prec_at_thresholds.85 <- get_precision_at_recall_thresholds(prec_recall.85, recall_thresholds)
prec_at_thresholds.9 <- get_precision_at_recall_thresholds(prec_recall.9, recall_thresholds)
prec_at_thresholds.95 <- get_precision_at_recall_thresholds(prec_recall.95, recall_thresholds)

average_precisions <- data.frame(
  AP.5 = mean(prec_at_thresholds.5$precision_at_recall, na.rm = TRUE),
  AP.55 = mean(prec_at_thresholds.55$precision_at_recall, na.rm = TRUE),
  AP.6 = mean(prec_at_thresholds.6$precision_at_recall, na.rm = TRUE),
  AP.65 = mean(prec_at_thresholds.65$precision_at_recall, na.rm = TRUE),
  AP.7 = mean(prec_at_thresholds.7$precision_at_recall, na.rm = TRUE),
  AP.75 = mean(prec_at_thresholds.75$precision_at_recall, na.rm = TRUE),
  AP.8 = mean(prec_at_thresholds.8$precision_at_recall, na.rm = TRUE),
  AP.85 = mean(prec_at_thresholds.85$precision_at_recall, na.rm = TRUE),
  AP.9 = mean(prec_at_thresholds.9$precision_at_recall, na.rm = TRUE),
  AP.95 = mean(prec_at_thresholds.95$precision_at_recall, na.rm = TRUE)
)

average_recalls <- data.frame(
  AR.5 = max(prec_recall.5$recall),
  AR.55 = max(prec_recall.55$recall),
  AR.6 = max(prec_recall.6$recall),
  AR.65 = max(prec_recall.65$recall),
  AR.7 = max(prec_recall.7$recall),
  AR.75 = max(prec_recall.75$recall),
  AR.8 = max(prec_recall.8$recall),
  AR.85 = max(prec_recall.85$recall),
  AR.9 = max(prec_recall.9$recall),
  AR.95 = max(prec_recall.95$recall)
)

mAP = round(mean(unlist(average_precisions)), 3) ## <------------ Hier is der wichtige Wert!!
mean_average_recall <- mean(unlist(average_recalls))

