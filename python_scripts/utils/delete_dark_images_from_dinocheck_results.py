#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb  3 17:30:08 2025

@author: Nils
"""

## Takes the output csv of the dinocheck and deletes all rows that corresponds to images that were categorized as too dark
## in the original pipeline. Creates a new csv that only contains images that are bright enough.

import os
import pandas as pd
from PIL import Image
import numpy as np

def image_to_dark_checker(image_path):
    """
    Takes the path of an input image and checks the brightness of the image.
    If the brightness is below 150, it returns True (meaning the image is too dark).
    If the brightness is above that value, it returns False.
    Dark images are moved to the 'dark_frames' subfolder.
    """
    try:
        img = Image.open(image_path).convert('L')  # Convert to grayscale
        numpy_image = np.array(img)  # Convert image to numpy array
        brightness = np.mean(numpy_image)  # Calculate mean brightness
        
        if brightness > 150:
            return False
        else:
            #dark_frames_folder = os.path.join(image_folder, "dark_frames")
            #os.makedirs(dark_frames_folder, exist_ok=True)
            #dark_frame_save_path = os.path.join(dark_frames_folder, image)
            #img.save(dark_frame_save_path)
            return True
    except Exception as e:
        print(f"Error processing {image_path}: {e}")
        return True  # Treat errors as if the image is dark to be safe

def filter_bright_images(csv_path, output_csv_path):
    """
    Reads a CSV file, checks image brightness, and creates a new CSV file
    excluding images that are too dark.
    """
    df = pd.read_csv(csv_path)

    # Ensure the 'image_path' column exists
    if 'image_path' not in df.columns:
        print("Error: CSV file must contain an 'image_path' column.")
        return

    filtered_rows = []
    
    for _, row in df.iterrows():
        full_image_path = row['image_path']
        # full_image_path = os.path.join(image_folder, image_path)  # Ensure full path
        
        if os.path.exists(full_image_path):  # Check if the file exists
            if not image_to_dark_checker(full_image_path):
                filtered_rows.append(row)  # Keep bright images
        else:
            print(f"Warning: Image file not found at {full_image_path}")

    # Create a new DataFrame with only bright images
    filtered_df = pd.DataFrame(filtered_rows)
    
    # Save to a new CSV file
    filtered_df.to_csv(output_csv_path, index=False)
    print(f"Filtered CSV saved to {output_csv_path}")

# Example usage:
filter_bright_images("/Volumes/T7_Shield/Diopsis_Cameras/RESULTS_2024/dinocheck_data.csv", 
                     "/Volumes/T7_Shield/Diopsis_Cameras/RESULTS_2024/2000_raw_images_unsorted/filtered_dinocheck_data.csv")





