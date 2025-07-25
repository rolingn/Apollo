library(dplyr)
library(stringr)

# 1. Read the CSV file into a data frame
df_nils <- read.csv("/Volumes/Apollo/Diopsis_Cameras/App_outputs/user_checked_predictions_10_02_25.csv")  # Replace with your actual file path

# 2. Extract the classes and orders from the 'filename' column
df_nils <- df_nils %>%
  mutate(class = str_extract(Filename, "(?<=/)[^/]+(?=/[^/]+/[^/]+$)")) %>%
  mutate(order = str_extract(Filename, "([^/]+)(?=/[^/]*$)"))

classes_to_keep <- df_nils %>%  # Classes that have at least one 'Correct' or 'Unsure' Response
  filter(Response %in% c("Correct", "Unsure")) %>%
  distinct(class) %>%  # Get unique class values
  pull(class)

classes_to_drop <- df_nils %>%  # All classes that only have 'Incorrect' or 'Dirt' as Response
  filter(!class %in% classes_to_keep) %>%
  distinct(class) %>%
  pull(class)

orders_to_keep <- df_nils %>%  # Orders that have at least one 'Correct' or 'Unsure' Response
  filter(Response %in% c("Correct", "Unsure")) %>%
  distinct(order) %>%  # Get unique order values
  pull(order)

orders_to_drop <- df_nils %>%  # All orders that only have 'Incorrect' or 'Dirt' as Response
  filter(!order %in% orders_to_keep) %>%
  distinct(order) %>%
  pull(order)

# 3. Count the occurrences of each class
class_counts_nils <- df_nils %>%
  count(class) %>%
  mutate(Percent = n / sum(n) * 100)

order_counts_nils <- df_nils %>%
  count(order) %>%
  mutate(Percent = n / sum(n) * 100)

response_percentage <- df_nils %>%
  count(Response) %>%
  mutate(Percent = n / sum(n) * 100)

response_percentage_class <- df_nils %>%
  filter(class %in% classes_to_keep) %>%
  group_by(class) %>%
  mutate(total_in_class = n()) %>%  # Get total count per class
  group_by(class, Response) %>%
  summarise(count = n(), total_in_class = first(total_in_class), .groups = 'drop') %>%
  mutate(percentage = (count / total_in_class) * 100)

response_percentage_order <- df_nils %>%
  filter(order %in% orders_to_keep) %>%
  group_by(order) %>%
  mutate(total_in_class = n()) %>%  # Get total count per class
  group_by(order, Response) %>%
  summarise(count = n(), total_in_class = first(total_in_class), .groups = 'drop') %>%
  mutate(percentage = (count / total_in_class) * 100)

# Actual Paper metrics
per_class_precision_class_filtered <- df_nils %>%
  group_by(class) %>%
  filter(Response %in% c("Correct", "Incorrect", "Unsure")) %>%
  mutate(total_in_class = n()) %>%  # Count total before filtering
  filter(Response == "Correct") %>%
  summarise(per_class_precision = n() / first(total_in_class))

per_class_precision_class <- df_nils %>%
  group_by(class) %>%
  filter(Response %in% c("Correct", "Incorrect", "Unsure")) %>%
  mutate(total_in_class = n()) %>%  # Count total before summarizing
  summarise(per_class_precision = sum(Response == "Correct") / first(total_in_class))

per_class_precision_order_filtered <- df_nils %>%
  group_by(order) %>%
  filter(Response %in% c("Correct", "Incorrect", "Unsure")) %>%
  mutate(total_in_order = n()) %>%  # Count total before filtering
  filter(Response == "Correct") %>%
  summarise(per_class_precision = n() / first(total_in_order))

per_class_precision_order <- df_nils %>%
  group_by(order) %>%
  filter(Response %in% c("Correct", "Incorrect", "Unsure")) %>%
  mutate(total_in_order = n()) %>%  # Count total before summarizing
  summarise(per_class_precision = sum(Response == "Correct") / first(total_in_order))


Diptera_in_total <- 28450 # Actual number of images classified as Diptera
labeled_as_dirt_diptera <- df_nils %>%
  filter(order == "Diptera") %>%
  summarise(dirt = sum(Response == "Dirt", na.rm = TRUE)) %>%
  pull(dirt)
sum_diptera_viewed <- order_counts_nils %>%
  filter(order == "Diptera") %>%
  pull(n)
dirt_percentage_diptera <- labeled_as_dirt_diptera / sum_diptera_viewed
# Subtracting the estimated number of images that only represent Dirt
Diptera_in_total <- Diptera_in_total - round((dirt_percentage_diptera * Diptera_in_total), 0)

Hemiptera_in_total <- 3911 #Actual number of images classified as Hemiptera
labeled_as_dirt_hemiptera <- df_nils %>%
  filter(order == "Hemiptera") %>%
  summarise(dirt = sum(Response == "Dirt", na.rm = TRUE)) %>%
  pull(dirt)
sum_hemiptera_viewed <- order_counts_nils %>%
  filter(order == "Hemiptera") %>%
  pull(n)
dirt_percentage_hemiptera <- labeled_as_dirt_hemiptera / sum_hemiptera_viewed
# Subtracting the estimated number of images that only represent Dirt
Hemiptera_in_total <- Hemiptera_in_total - round((Hemiptera_in_total * dirt_percentage_hemiptera), 0)

images_viewed_total <- nrow(df_nils)
diptera_precision_value <- per_class_precision_order %>%
  filter(order == "Diptera") %>%
  pull(per_class_precision)
hemiptera_precision_value <- per_class_precision_order %>%
  filter(order == "Hemiptera") %>%
  pull(per_class_precision)

estimated_TP_Diptera <- round(Diptera_in_total * diptera_precision_value, 0)
estimated_TP_Hemiptera <- round(Hemiptera_in_total * hemiptera_precision_value, 0)

TP_rest <- df_nils %>%
  filter(!order %in% c("Diptera", "Hemiptera")) %>%
  summarise(TP_rest = sum(Response == "Correct", na.rm = TRUE)) %>%
  pull(TP_rest)

TP_total <- estimated_TP_Diptera + estimated_TP_Hemiptera + TP_rest

labeled_as_dirt <- df_nils %>%
  summarise(dirt = sum(Response == "Dirt", na.rm = TRUE)) %>%
  pull(dirt)

dirt_percentage <- labeled_as_dirt / images_viewed_total

# Count all crops in the subfolders
root_folder <- "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/all_crops/by_class_order"
crops_total <- length(list.files(root_folder, pattern = "\\.(jpg|jpeg|png|tif|tiff|bmp|gif)$", 
                          recursive = TRUE, full.names = TRUE, ignore.case = TRUE))

estimated_dirt_total <- round(crops_total * dirt_percentage, 0)
crops_without_dirt <- crops_total - estimated_dirt_total # Excluding the images labeled as Dirt

top1_accuracy <- (TP_total / crops_without_dirt) * 100
