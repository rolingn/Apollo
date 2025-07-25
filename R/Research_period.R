library(ggplot2)

setwd("/github")

camera_dates <- data.frame(
  Equipment = c("Jatzhorn lower", "Jatzhorn middle", "Jatzhorn upper",
                "Monstein lower", "Monstein middle", "Monstein upper",
                "Weissfluhjoch lower", "Weissfluhjoch middle", "Weissfluhjoch upper"),
  Start = as.Date(c("2024-07-08", "2024-07-08", "2024-07-09",
                    "2024-07-16", "2024-08-07", "2024-07-23",
                    "2024-07-17", "2024-07-22", "2024-07-11")),
  End = as.Date(c("2024-09-20", "2024-09-20", "2024-09-20",
                  "2024-09-25", "2024-09-25", "2024-09-25",
                  "2024-09-18", "2024-09-18", "2024-09-19")),
  Relevant = c("Yes", "Yes", "Yes",
               "Yes", "No", "Yes",
               "Yes", "Yes", "Yes")
)
camera_dates$ActiveDays <- as.numeric(camera_dates$End - camera_dates$Start)


ggplot(camera_dates, aes(y = Equipment, xmin = Start, xmax = End, color = Relevant)) +
  geom_rect(aes(ymin = -Inf, ymax = Inf, xmin = as.Date("2024-08-13"), xmax = as.Date("2024-09-18")), 
            color = NA, fill = "khaki1", alpha = .2)+
  geom_text(aes(x = as.Date("2024-07-23"), y = "Monstein middle"), label = "57 days active", angle = 90, 
            vjust = -0.7, size = 4, alpha = .3, nudge_y = .5, color = "khaki4") +
  geom_text(aes(x = as.Date("2024-08-31"), y = "Jatzhorn lower"), label = "36 days with light", angle = 0, 
                      vjust = 3.7, size = 4, alpha = .3, nudge_y = .5, color = "khaki4") +
  geom_errorbarh(height = 0.4) +  # Horizontal bars
  geom_text(aes(x = Start + (End - Start) / 2, label = paste(ActiveDays, "days")), 
            vjust = -0.5, size = 5) +  # Add text
  geom_vline(xintercept = as.Date("2024-07-23"), linetype = "dotted") +
  geom_vline(xintercept = as.Date("2024-09-18"), linetype = "dotted") +
  theme_classic() +
  labs(title = "", x = "Date", y = "Site") +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(size = 15),
        legend.position = "none") +
  scale_x_date(date_breaks = "1 week", date_labels = "%D") +
  scale_color_manual(values = c("No" = "gray80", "Yes" = "black"))

ggsave("Plots/Research_period.png", width = 11, height = 5, dpi = 300)
