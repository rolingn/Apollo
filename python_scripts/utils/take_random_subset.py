#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Jan 31 08:50:10 2025

@author: Nils
"""

import os
import random
import shutil

def copy_random_images(source_folder, destination_folder, n):
    # Ensure destination folder exists
    os.makedirs(destination_folder, exist_ok=True)

    # Get list of image files in the source folder
    images = [f for f in os.listdir(source_folder) if f.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp', '.gif'))]

    # Ensure n does not exceed the number of available images
    n = min(n, len(images))

    # Randomly select n images
    selected_images = random.sample(images, n)

    # Copy selected images to the destination folder
    for img in selected_images:
        src_path = os.path.join(source_folder, img)
        dst_path = os.path.join(destination_folder, img)
        shutil.copy(src_path, dst_path)  # copy2 preserves metadata

    print(f"Copied {n} images to {destination_folder}")

# Example usage:
source_folder = "/Volumes/T7_Shield/Diopsis_Cameras/RESULTS_2024/all_raw_images"
destination_folder = "/Volumes/T7_Shield/Diopsis_Cameras/RESULTS_2024/2000_raw_images"
n = 2000  # Number of images to copy

copy_random_images(source_folder, destination_folder, n)