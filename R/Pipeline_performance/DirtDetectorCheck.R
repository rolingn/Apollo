# Calculation of false alarm rate (FAR) and tracking detection rate (TDR)
# The values of the 10 inspected days are summed
TP <- 24 + 116 + 55 + 13 + 64 + 16 + 4 + 34 + 48 + 23
FP <- 4 + 25 + 47 + 2 + 30 + 11 + 2 + 10 + 15 + 4
TG <- 28 + 155 + 91 + 15 + 115 + 27 + 7 + 47 + 68 + 26

FAR = FP / (TP + FP)
TDR = TP / TG