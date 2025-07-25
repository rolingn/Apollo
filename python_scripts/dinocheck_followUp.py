### Script for calculating Precision, Recall, and F1_score for different algorithm thresholds. 

### We expect insight on which threshold would have been the best during inference, since 0.25 missed lots of insects.

### User Manual:
### When launching with python dinocheck.py you will be prompted to select a folder of images.
### The folder must contain the images that were evaluated using the original dinocheck.py script.
### The images contained within the dinocheck.py results csv will appear on the screen if display_images = True.
### The GT bounding boxes will appear in green and the predictions (that match) at different thresholds in green.
### The code compares the GT and predictions and returns TP, FP and FN values for all thresholds
### to be able to calculate precision, recall and F1 at different thresholds later.
### This will allow us to judge what threshold would work best on our data
### A csv with results will be saved into the input folder.

### Grounding DINO runs in the background and makes predictions.

import os
import random
from PIL import Image, ImageTk
import torch
from transformers import pipeline
from typing import List, Dict, Any


from tkinter import Tk, Canvas, filedialog
import pandas as pd
import numpy as np
import time
import csv
import ast
    
    
    
thresh_dict = {}
current_image_path = None
folder_path = None
images = []
canvas = None
root = None
#thresholds = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]
thresholds = np.arange(0, 1.01, 0.01).tolist()
csv_header = ["threshold",
              "precision", "recall", "F1_score", "TP", "FP", "FN"]


def load_grounding_dino_model():
    """
    Function to load the GroundingDINO model.
    """
    if torch.backends.mps.is_available():
        device = "cpu"  # Adjust for MPS compatibility
    elif torch.cuda.is_available():
        device = "cuda"
    else:
        device = "cpu"
    print("---------- GroundingDino runs on", device, "----------")
    detector_id = "IDEA-Research/grounding-dino-base"
    object_detector = pipeline(model=detector_id, task="zero-shot-object-detection", device=device)
    return object_detector

object_detector = load_grounding_dino_model()

def detect(object_detector, image: Image.Image, labels: List[str], threshold: float = 0.25) -> List[Dict[str, Any]]:
    """
    Use Grounding DINO to detect a set of labels in an image.
    """
    threshold = 0
    labels = [label if label.endswith(".") else label + "." for label in labels]
    results = object_detector(image, candidate_labels=labels, threshold=threshold)
    return results


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
        show_first_image(display_images = False)
        
def show_first_image(display_images):
    """Display the next image randomly from the list."""
    global current_image_path, canvas, images, scale_factor, detections
    scale_factor = 0.398538961038961
    if not images:
        print("No images to display.")
        root.destroy()
        return
    i = 0

    for image in images:
        #current_image_path = images.pop(0)
        current_image_path = image
        #parent_folder = image_folder_map[current_image_path]  # Get the parent folder of the image
        GT_boxes = get_gt_boxes(current_image_path = current_image_path)
    
        img = Image.open(current_image_path).convert("RGB")
        labels = ["insect"]  # Replace with relevant labels
        detections = detect(object_detector, img, labels)
        
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
        threshold_calculations(GT_boxes, display_images)
        i += 1
        print(f"Analyzed {i} of {len(images)} images.")
        save_to_csv()
        
    root.destroy()
    
def get_gt_boxes(current_image_path):
    gt_boxes = existing_data.loc[existing_data['image_path'] == current_image_path, 'GT_boxes']

    # Convert from string to list
    if not gt_boxes.empty:
        gt_boxes = gt_boxes.values[0]  # Extract value
        gt_boxes = ast.literal_eval(gt_boxes)  # Convert string representation of list to actual list
        return gt_boxes
    
    return []

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
            
def draw_detected_boxes(canvas, predicted_box, scale_factor, color="cyan", width=2):
    """
    Draws rectangles for each box in GT_boxes on the given Tkinter canvas.
    
    :param canvas: Tkinter Canvas widget.
    :param gt_boxes: List of bounding boxes, where each box is [x1, y1, x2, y2].
    :param color: Outline color of the rectangles (default: "cyan").
    :param width: Line width of the rectangles (default: 2).
    """
    x1 = predicted_box["xmin"]
    y1 = predicted_box["ymin"]
    x2 = predicted_box["xmax"]
    y2 = predicted_box["ymax"]
    x1 *= scale_factor
    y1 *= scale_factor
    x2 *= scale_factor
    y2 *= scale_factor
    canvas.create_rectangle(x1, y1, x2, y2, outline=color, width=width)
    root.update_idletasks()  # Force Tkinter to process GUI updates
    root.update()
    
def threshold_calculations(GT_boxes, display_images):
    #threshold = 0.25
    global thresh_dict
    results = {}

    for threshold in thresholds:
        detections_filtered = [box for box in detections if box['score'] >= threshold]
        
        if threshold in thresh_dict and thresh_dict[threshold]:
            previous_result = thresh_dict[threshold]
            previous_TP = previous_result["TP"]
            previous_FP = previous_result["FP"]
            previous_FN = previous_result["FN"]
            previous = True
        else:
            previous = False
    
        ### Precision calculation
        TP = 0
        FP = 0
        matched_GT_boxes = set()  # To track matched GT boxes
    
        for detection in detections_filtered:
            best_IoU = 0
            best_GT_idx = None
            predicted_box = detection["box"]
    
            # Find the best match for this prediction
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
                #draw_detected_boxes(canvas, predicted_box, scale_factor, color="orange", width=2)
                
        # Add previous TP FP and FN. Basically sums them over all images and only saves the total
        if previous:
            TP += previous_TP
            FP += previous_FP
    
        precision = TP / (TP + FP) if (TP + FP) > 0 else 0.0
        results["precision"] = precision
    
        # FN: Unmatched GT boxes
        FN = len(GT_boxes) - len(matched_GT_boxes)
        if previous:
            FN += previous_FN
    
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
        
        thresh_dict[threshold] = results.copy()
    
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
    x_left = max(GT_xmin, box2["xmin"])
    y_top = max(GT_ymin, box2["ymin"])
    x_right = min(GT_xmax, box2["xmax"])
    y_bottom = min(GT_ymax, box2["ymax"])

    # Compute area of intersection
    if x_right < x_left or y_bottom < y_top:
        return 0.0  # No overlap

    intersection_area = (x_right - x_left) * (y_bottom - y_top)

    # Compute area of both bounding boxes
    box1_area = (box1[2] - box1[0]) * (box1[3] - box1[1])
    box2_area = (box2["xmax"] - box2["xmin"]) * (box2["ymax"] - box2["ymin"])

    # Compute IoU
    iou = intersection_area / float(box1_area + box2_area - intersection_area)
    return round(iou, 4)  # Return IoU rounded to 4 decimal places
            
def save_to_csv():
    """Save click data to a CSV file in the user-specified folder."""
    csv_file = os.path.join(folder_path, "threshold_results_02IoU.csv")

    # Open file in append mode if it exists
    with open(csv_file, mode= "w", newline="") as file:
        writer = csv.writer(file)

        # Write header if creating a new file
        writer.writerow(csv_header)

        # Write new data
        for threshold, values in thresh_dict.items():
            precision = values["precision"]
            recall = values["recall"]
            F1 = values["F1_score"]
            TP = values["TP"]
            FP = values["FP"]
            FN = values["FN"]

            row = [threshold, precision, recall, F1, TP, FP, FN]
            writer.writerow(row)

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