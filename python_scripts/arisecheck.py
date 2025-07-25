### Script for calculating Precision, Recall, and F1_score for the results of the arise inference.

### We expect insight on the performance of the arise pipeline in detecting insects.

### User Manual:
### When launching with python arisecheck.py you will be prompted to select a folder of images.
### The folder must contain the images that were evaluated using the original dinocheck.py script.
### The images contained within the dinocheck.py results csv will appear on the screen if display_images = True.
### The GT bounding boxes will appear in green and the predictions (that match) at different thresholds in red.
### The code compares the GT and predictions and returns TP, FP and FN values
### to be able to calculate precision, recall and the F1 score later.
### This will allow us to compare the performance of the arise pipeline to ours.
### A csv with results will be saved into the input folder.

### Arise results are taken from csv files that have to be specified (arise_path variable).

import os
import random
from PIL import Image, ImageTk
from typing import List, Dict, Any


from tkinter import Tk, Canvas, filedialog
from PIL import Image, ImageDraw
import pandas as pd
import numpy as np
import time
import csv
import ast
from datetime import datetime
import pyautogui

    
    
results_dict = {}
current_rect = None
current_image_path = None
folder_path = None
images = []
canvas = None
root = None

FN_total = 0
not_analyzed_by_arise = 0
csv_header = ["filename", "camera", "precision", "recall", "F1_score", "TP", "FP", "FN"]

arise_path = "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/result_csvs/Arise_results/renamed"

# Hier steht noch arbeit an!!
def select_folder():
    """Prompt user to select a folder containing images."""
    global folder_path, images, image_folder_map, existing_data
    folder_path = filedialog.askdirectory(title="Select Folder")
    if folder_path:
        csv_file = os.path.join(folder_path, "filtered_dinocheck_data.csv")
        processed_images = set()

        # Load processed images if CSV exists
        if os.path.isfile(csv_file):
            existing_data = pd.read_csv(csv_file)
            existing_data['image_path'] = existing_data['image_path'].apply(lambda x: os.path.join(folder_path, os.path.basename(x)))
            processed_images = {
                os.path.join(folder_path, os.path.basename(image_path))
                for image_path in existing_data["image_path"].tolist()
            }

        # Recursively find all images in subfolders
        images = []
        image_folder_map = {}  # Map each image to its parent folder
        for root_dir, dirs, files in os.walk(folder_path):
            if 'visualized' in dirs:
                dirs.remove('visualized')
            for file in files:
                if file.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp', '.gif')):
                    full_path = os.path.join(root_dir, file)
                    images.append(full_path)
                    image_folder_map[full_path] = os.path.basename(root_dir)

        # Remove already processed images
        images = [img for img in images if img in processed_images]
        print(f"{len(images)} images left to process.")

        random.shuffle(images)  # Randomize image order
        show_first_image(display_images = True)
        
def show_first_image(display_images):
    """Display the next image randomly from the list."""
    global current_image_path, canvas, images, scale_factor, current_camera, filename, not_analyzed_by_arise
    img = Image.open(images[0]).convert("RGB")
    width, height = img.size
    scale_factor = 0.398538961038961
    if not images:
        print("No images to display.")
        root.destroy()
        return
    i = 0
    original_paths = pd.read_csv("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/2000_raw_images_unsorted/original_paths.csv")

    for image in images:
        current_image_path = image
        filename = os.path.basename(current_image_path)
        
        result = original_paths.loc[original_paths["filename"] == filename, "camera"]
        # Extract the value if it exists
        if not result.empty:
            current_camera = result.iloc[0]  # Get the first matching value
            #print("Current camera:", current_camera, "Current file:", filename)
        else:
            print(f"No matching image found in {original_paths}")
            continue

        arise_boxes = get_arise_predictions(current_image_path, current_camera)
        if arise_boxes.empty:
            print("Image somehow not analyzed by arise... Sry...")
            not_analyzed_by_arise +=1
            i += 1
            continue
        arise_boxes = transform_arise_boxes(arise_boxes, width, height)
        GT_boxes = get_gt_boxes(current_image_path = current_image_path)
    
        img = Image.open(current_image_path).convert("RGB")
        
        if display_images:
            width, height = img.size
            new_width = int(width * scale_factor)
            new_height = int(height * scale_factor)
            scaled_image = img.resize((new_width, new_height)) # Apply scale
            tk_image = ImageTk.PhotoImage(scaled_image)
        
            # Clear canvas and display the image
            canvas.delete("all")
            canvas.create_image(0, 0, anchor="nw", image=tk_image)
            canvas.image = tk_image
            draw_gt_boxes(canvas, GT_boxes, scale_factor, color="cyan", width=3)
            root.geometry(f"{new_width}x{new_height}")  # Resize window to image size
            root.update_idletasks()  # Force Tkinter to process GUI updates
            root.update()  
        threshold_calculations(GT_boxes, display_images, arise_boxes)
        i += 1
        print(f"Analyzed {i} of {len(images)} images.")
        save_to_csv()
        #save_canvas_as_image(canvas, filename)
        #break
    
    print(f"{not_analyzed_by_arise} out of {len(images)} images not analyzed by arise.")
    root.destroy()
    
def get_gt_boxes(current_image_path):
    gt_boxes = existing_data.loc[existing_data['image_path'] == current_image_path, 'GT_boxes']

    # Convert from string to list
    if not gt_boxes.empty:
        gt_boxes = gt_boxes.values[0]  # Extract value
        gt_boxes = ast.literal_eval(gt_boxes)  # Convert string representation of list to actual list
        return gt_boxes
    
    return []

def get_arise_predictions(current_image_path, current_camera):
    filename_without_ext = os.path.splitext(os.path.basename(current_image_path))[0]
    csv_path = os.path.join(arise_path, current_camera + ".csv")
    if os.path.isfile(csv_path):
        arise_dataframe = pd.read_csv(csv_path)
    else:
        print(f"Arise csv not found. Searched in {csv_path}")
        
    timestamp_dt = datetime.strptime(filename_without_ext, "%Y%m%d%H%M%S")
    # Format as "YYYY-MM-DD HH:MM:SS"
    formatted_timestamp = timestamp_dt.strftime("%Y-%m-%d %H:%M:%S")
    
    arise_boxes = arise_dataframe.loc[arise_dataframe['capture_on'] == formatted_timestamp, ['x1', 'x2', 'y1', 'y2']]
    #print(arise_boxes)
    # Convert from string to list
    if not arise_boxes.empty:
        return arise_boxes
    
    return arise_boxes

def transform_arise_boxes(arise_boxes, x_scale, y_scale):
    # Drop NaN rows completely
    arise_boxes = arise_boxes.dropna()

    # Return empty list if all values are NaN
    if arise_boxes.empty:
        return []

    # Extract columns as NumPy array
    box_array = arise_boxes[['x1', 'x2', 'y1', 'y2']].values

    # Multiply x values by x_scale, y values by y_scale
    box_array[:, [0, 1]] *= x_scale  # Scale x1 and x2
    box_array[:, [2, 3]] *= y_scale  # Scale y1 and y2
    
    box_array = np.round(box_array, 1)

    # Convert to list of lists
    return box_array.tolist()

def draw_gt_boxes(canvas, gt_boxes, scale_factor, color="cyan", width=2):
    """
    Draws rectangles for each box in GT_boxes on the given Tkinter canvas.
    
    :param canvas: Tkinter Canvas widget.
    :param gt_boxes: List of bounding boxes, where each box is [x1, y1, x2, y2].
    :param color: Outline color of the rectangles (default: "cyan").
    :param width: Line width of the rectangles (default: 2).
    """
    for box in gt_boxes:
        if len(box) == 4:  # Ensure valid box format [x1, y1, x2, y2]
            x1, y1, x2, y2 = box
            x1 *= scale_factor
            y1 *= scale_factor
            x2 *= scale_factor
            y2 *= scale_factor
            canvas.create_rectangle(x1, y1, x2, y2, outline=color, width=width)
            
def draw_detected_boxes(canvas, predicted_box, scale_factor, color="cyan", width=2, text = None):
    """
    Draws rectangles for each box in GT_boxes on the given Tkinter canvas.
    
    :param canvas: Tkinter Canvas widget.
    :param gt_boxes: List of bounding boxes, where each box is [x1, y1, x2, y2].
    :param color: Outline color of the rectangles (default: "cyan").
    :param width: Line width of the rectangles (default: 2).
    """
    x1 = predicted_box[0]
    y1 = predicted_box[2]
    x2 = predicted_box[1]
    y2 = predicted_box[3]
    x1 *= scale_factor
    y1 *= scale_factor
    x2 *= scale_factor
    y2 *= scale_factor
    canvas.create_rectangle(x1, y1, x2, y2, outline=color, width=width)
    if text is not None:
        canvas.create_text(round((x1 + x2) / 2, 0), y2 + 8, text=text, anchor="w", fill="black", font=("Arial", 10, "bold"))
    root.update_idletasks()  # Force Tkinter to process GUI updates
    root.update()
    
def threshold_calculations(GT_boxes, display_images, arise_boxes):
    #threshold = 0.25
    global results_dict, TP, FP, FN, FN_total
    results = {}
    
    TP = 0
    FP = 0
    matched_GT_boxes = set()  # To track matched GT boxes

    for detection in arise_boxes:
        best_IoU = 0
        best_GT_idx = None
        predicted_box = detection

        # Find the best match for this prediction
        #for i, GT_box in enumerate(click_data_scaled[current_image_path]):
        for i, GT_box in enumerate(GT_boxes):
            IoU = calculate_iou(GT_box, predicted_box)
            if IoU > best_IoU:
                best_IoU = IoU
                best_GT_idx = i

        # If best IoU is above threshold and GT is not matched yet, count as TP
        if best_IoU >= 0.5 and best_GT_idx not in matched_GT_boxes:
            TP += 1
            matched_GT_boxes.add(best_GT_idx)  # Mark GT as matched
            if display_images:
                draw_detected_boxes(canvas, predicted_box, scale_factor, color="green", width=2)
        else:
            FP += 1  # If no good match is found, count as FP
            draw_detected_boxes(canvas, predicted_box, scale_factor, color="orange", width=2, text = round(best_IoU, 2))
            

    precision = TP / (TP + FP) if (TP + FP) > 0 else 0.0
    results["precision"] = precision

    # FN: Unmatched GT boxes
    FN = len(GT_boxes) - len(matched_GT_boxes)
    FN_total += FN

    recall = TP / (TP + FN) if (TP + FN) > 0 else 0.0
    results["recall"] = recall

    if precision + recall > 0:
        f1_score = 2 * (precision * recall) / (precision + recall)
    else:
        f1_score = 0.0
    results["F1_score"] = f1_score
    results["TP"] = TP
    results["FP"] = FP
    results["FN"] = FN
    results["camera"] = current_camera
    
    results_dict[filename] = results.copy()
    
def calculate_iou(box1, box2):
    """
    Calculate the Intersection over Union (IoU) of two bounding boxes.
    
    Parameters:
        box1: list or tuple -> [xmin, ymin, xmax, ymax] for the first box
        box2: list or tuple -> [xmin, ymin, xmax, ymax] for the second box
    
    Returns:
        float: IoU value between 0 and 1
    """
    if box1 is None:
        return 0.0
    
    GT_xmin = min(box1[0], box1[2])
    GT_xmax = max(box1[0], box1[2])
    GT_ymin = min(box1[1], box1[3])
    GT_ymax = max(box1[1], box1[3])
    # Get coordinates for the intersection rectangle
    x_left = max(GT_xmin, box2[0])
    y_top = max(GT_ymin, box2[2])
    x_right = min(GT_xmax, box2[1])
    y_bottom = min(GT_ymax, box2[3])

    # Compute area of intersection
    if x_right < x_left or y_bottom < y_top:
        return 0.0  # No overlap

    intersection_area = (x_right - x_left) * (y_bottom - y_top)

    # Compute area of both bounding boxes
    box1_area = (box1[2] - box1[0]) * (box1[3] - box1[1])
    box2_area = (box2[1] - box2[0]) * (box2[3] - box2[2])

    # Compute IoU
    iou = intersection_area / float(box1_area + box2_area - intersection_area)
    return round(iou, 4)  # Return IoU rounded to 4 decimal places
            
def save_to_csv():
    """Save click data to a CSV file in the user-specified folder."""
    csv_file = os.path.join(folder_path, "arise_performance_results_02IoU.csv")

    # Open file in append mode if it exists
    with open(csv_file, mode= "w", newline="") as file:
        writer = csv.writer(file)

        # Write header if creating a new file
        writer.writerow(csv_header)

        # Write new data
        #for image_path, AP_value in ap_dict.items():
        for filename, values in results_dict.items():
            camera = values["camera"]
            precision = values["precision"]
            recall = values["recall"]
            F1 = values["F1_score"]
            TP = values["TP"]
            FP = values["FP"]
            FN = values["FN"]

            row = [filename, camera, precision, recall, F1, TP, FP, FN]
            writer.writerow(row)

def save_canvas_as_image(canvas, filename):
    path = "/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/Arise_performance_check_images"
    os.makedirs(path, exist_ok=True)
    save_path = os.path.join(path, filename)
    x = root.winfo_rootx() + canvas.winfo_x()
    y = root.winfo_rooty() + canvas.winfo_y()
    w = canvas.winfo_width()
    h = canvas.winfo_height()
    
    img = pyautogui.screenshot(region=(x, y, w, h))
    img = img.convert("RGB")
    img.save(save_path)
    #print(f"Canvas saved as {save_path}")

def main():
    """Main function to run the application."""
    global root, canvas
    root = Tk()
    root.title("Image Annotation Tool")
    
    canvas = Canvas(root)
    canvas.pack(fill="both", expand=True)

    # Bind events
    #canvas.bind("<Button-1>", record_click)  # Left-click for recording clicks
    #canvas.bind("<ButtonPress-1>", start_draw)
    #canvas.bind("<B1-Motion>", update_draw)
    #canvas.bind("<ButtonRelease-1>", end_draw)
    #root.bind("a", show_next_image) # Press "a" on the keyboard to advance to the next image
    #root.bind("<space>", skip_image)  # Spacebar for skipping images
    #root.bind("d", undo_last_click)  # 'd' key for undoing the last click

    # Select folder and start displaying images
    select_folder()

    root.mainloop()

if __name__ == "__main__":
    main()