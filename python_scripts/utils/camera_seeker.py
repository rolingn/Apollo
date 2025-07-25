## This script is used to find out with which camera the 2000 randomly sampled images for the dinocheck were taken, since
## a clever scientist just sampled them without saving any reference to the original folder they came from.

import os
import csv
import re

photos_paths = ["/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Images_RAW_and_analyzed/421_Jatz_low/Jatzhorn_lower_analyzed/photos",
                "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Images_RAW_and_analyzed/391_Mon_up/DIOPSIS-391_analyzed/photos",
                "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Images_RAW_and_analyzed/389_Mon_low/DIOPSIS-389_analyzed/photos",
                "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Images_RAW_and_analyzed/388_Jatz_up/DIOPSIS-388_analyzed/photos",
                "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Images_RAW_and_analyzed/387_Jatz_mid/DIOPSIS-387_analyzed/photos",
                "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Images_RAW_and_analyzed/385_Weiss_up/DIOPSIS-385_analyzed/photos",
                "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Images_RAW_and_analyzed/384_Weiss_mid/DIOPSIS-384_analyzed/photos",
                "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Images_RAW_and_analyzed/383_Weiss_low/DIOPSIS-383_analyzed/photos",
                ]# Root folder containing subfolders
search_images_path = "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/2000_raw_images_unsorted"  # Folder with 2000 images to search for

# Define valid image extensions
image_extensions = {".jpg", ".jpeg", ".png", ".bmp", ".gif", ".tiff"}

# Get only image files from the 2000 images folder
search_images = {
    file for file in os.listdir(search_images_path)
    if os.path.splitext(file)[1].lower() in image_extensions  # Check file extension
}

found_images = {}

# Search in the existing folder structure
for photos_path in photos_paths:
    print(f"Searching in {photos_path}")
    for folder in os.listdir(photos_path):
        folder_path = os.path.join(photos_path, folder)
    
        if os.path.isdir(folder_path):  # Ensure it's a directory
            for file in os.listdir(folder_path):
                source_file = os.path.join(folder_path, file)
    
                if os.path.isfile(source_file) and file in search_images:
                    found_images[file] = source_file  # Store the full path

# ** Report Results **
found = 0
missing = 0
for img in search_images:
    if img in found_images:
        found += 1
    else:
        missing += 1
        
print(f"Found {found} images")

save_folder = "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/2000_raw_images_unsorted"  # Change this to your desired folder
csv_filename = os.path.join(save_folder, "original_paths.csv")

# Write to CSV file
with open(csv_filename, mode="w", newline="") as f:
    writer = csv.writer(f)
    
    # Write header row
    writer.writerow(["filename", "original_path", "camera"])
    
    # Write each found image and its path
    for img, path in found_images.items():
        # Extract camera number using regex
        match = re.search(r"Images_RAW_and_analyzed/(\d+)", path)
        camera_id = f"DIOPSIS-{match.group(1)}" if match else "Unknown"
        writer.writerow([img, path, camera_id])

print(f"Data saved to {csv_filename}")
        
        