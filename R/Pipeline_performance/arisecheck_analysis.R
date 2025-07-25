library(dplyr)
library(stringr)

df <- read.csv("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/Arise_results/arise_performance_results.csv")
df_02IoU <- read.csv("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/Arise_results/arise_performance_results_02IoU.csv")

TP <- sum(df$TP)
FP <- sum(df$FP)
FN <- sum(df$FN)

TGT <- TP + FN

precision <- round(TP / (TP + FP), 4)
recall <- round(TP / (TP + FN), 4)

F1 <- (2 * precision * recall) / (precision + recall)


# 0.2 IoU
TP_02IoU <- sum(df_02IoU$TP)
FP_02IoU <- sum(df_02IoU$FP)
FN_02IoU <- sum(df_02IoU$FN)

TGT_02IoU <- TP_02IoU + FN_02IoU

precision_02IoU <- round(TP_02IoU / (TP_02IoU + FP_02IoU), 4)
recall_02IoU <- round(TP_02IoU / (TP_02IoU + FN_02IoU), 4)

F1_02IoU <- (2 * precision_02IoU * recall_02IoU) / (precision_02IoU + recall_02IoU)
