## Insect script Davos ###


library(ggplot2)
library(cowplot)

setwd("C:/Users/kempelan/Desktop/Insects R")
data<-read.table("insect_data.txt", header=T, fill=TRUE,sep="\t", quote = "",dec=".")
str(data)

data$Trap_ID<-as.factor(data$Trap_ID)
data$Site<-as.factor(data$Site)
data$Trap_Type<-as.factor(data$Trap_Type)

data_M<-data[data$Trap_Type=="Malaise",]
data_P<-data[data$Trap_Type=="Pitfall",]

#Plot Traps along Sampling number
# Define custom colors for Trap_ID levels
custom_colors <- c("red", "red1", "grey", "grey", "blue", "blue") # Add more colors if needed

ggplot(data_M, aes(x = Sampling_number, y = g_per_day, color = Trap_ID)) +
  geom_point(size = 3.2) +
  labs(x = "Sampling Number", y = "g Insects per day") +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10, face = "bold")) +
  geom_line() +
  scale_color_manual(values = custom_colors)+
  theme_cowplot()

#Plot Traps along Sampling number
# Define custom colors for Trap_ID levels
custom_colors <- c("red", "#FFB6C1", "red", "#FFB6C1","grey", "darkgrey", "grey", "darkgrey","blue", "lightblue", "blue", "lightblue") # Add more colors if needed

ggplot(data_P, aes(x = Sampling_number, y = g_per_day, color = Trap_ID)) +
  geom_point(size = 3.2) +
  labs(x = "Sampling Number", y = "g Insects per day") +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 10, face = "bold")) +
  geom_line() +
  scale_color_manual(values = custom_colors)+
  theme_cowplot()

  
  
