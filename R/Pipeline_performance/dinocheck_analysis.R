library(dplyr)
library(stringr)

df <- read.csv("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/2000_raw_images_unsorted/filtered_dinocheck_data.csv")

mAP <- mean(df$AP)

TP <- sum(df$TP)
FP <- sum(df$FP)
FN <- sum(df$FN)

precision_025 <- TP / (TP + FP)
recall_025 <- TP / (TP + FN)

F1_025 <- (2 * precision_025 * recall_025) / (precision_025 + recall_025)
