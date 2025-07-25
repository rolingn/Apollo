### Script for calculating Precision, Recall and AP for groundingDINO. Also calculates Precision, Recall, and F1_score 
### for the 0.25 threshold that was used during inference. 

### User Manual:
### When launching with python dinocheck.py you will be prompted to select a folder of images to inspect.
### Choose the raw images from the camera.
### The images will appear in random order on the screen. The user can then click and drag on the image, to draw bounding
### boxes over the ground truth instances of insects.
### If a mistake was made, the last bounding box can be deleted by pressing "d" on the keyboard.
### Once all insects have been annotated, move on to the next image by pressing "a" (for advance)
### If you encounter an image with no insects to annotate, press "spacebar" to move on to the next image.
### A csv with results will be saved into the input folder. Ideally leave the csv there. This way the code will remember, which
### images have already been analyzed to not display them over and over if you relaunch the code later.

### Grounding DINO runs in the background and makes predictions.
### Precision, Recall and AP are calculated with the 11-point method.    

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
    
    
    
click_data = {}
click_data_scaled = {}
precisions_dict = {}
recalls_dict = {}
ap_dict = {}
thresh_025_dict = {}
current_rect = None
current_image_path = None
folder_path = None
images = []
canvas = None
root = None
thresholds = [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]
csv_header = ["image_path",
              "GT_boxes",
              "precision_0", "precision_01", "precision_02", "precision_03", "precision_04",
              "precision_05", "precision_06", "precision_07", "precision_08", "precision_09", "precision_1",
              "recall_0", "recall_01", "recall_02", "recall_03", "recall_04",
              "recall_05", "recall_06", "recall_07", "recall_08", "recall_09", "recall_1",
              "AP", 
              "precision_025", "recall_025", "F1_025"]


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
    global folder_path, images, image_folder_map
    folder_path = filedialog.askdirectory(title="Select Folder")
    if folder_path:
        csv_file = os.path.join(folder_path, "click_data.csv")
        processed_images = set()

        # Load processed images if CSV exists
        if os.path.isfile(csv_file):
            existing_data = pd.read_csv(csv_file)
            processed_images = set(existing_data["image_path"].tolist())
            #print(f"Loaded {len(processed_images)} processed images from existing CSV.")

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
        images = [img for img in images if img not in processed_images]
        print(f"{len(images)} images left to process.")

        random.shuffle(images)  # Randomize image order
        show_first_image()
        
def show_first_image():
    """Display the next image randomly from the list."""
    global current_image_path, canvas, images, scale_factor, detections
    if not images:
        print("No images to display.")
        root.destroy()
        return

    current_image_path = images.pop(0)
    #parent_folder = image_folder_map[current_image_path]  # Get the parent folder of the image

    img = Image.open(current_image_path).convert("RGB")
    labels = ["insect"]  # Replace with relevant labels
    detections = detect(object_detector, img, labels)
    
    width, height = img.size
    scale_factor = 0.398538961038961
    new_width = int(width * scale_factor)
    new_height = int(height * scale_factor)
    scaled_image = img.resize((new_width, new_height)) # Apply scale
    tk_image = ImageTk.PhotoImage(scaled_image)

    # Clear canvas and display the image
    canvas.delete("all")
    canvas.create_image(0, 0, anchor="nw", image=tk_image)
    canvas.image = tk_image
    root.geometry(f"{new_width}x{new_height}")  # Resize window to image size
    
def show_next_image(self):
    """Display the next image randomly from the list."""
    global current_image_path, canvas, images, scale_factor, detections
    
    #if current_image_path:
        #save_current_canvas()
    calculate_precisions()
    calculate_recalls()
    threshold_025_calculations()
    calculate_ap(precisions_dict[current_image_path], recalls_dict[current_image_path])
    print(f"ap: {ap_dict[current_image_path]}")
    if not images:
        print("No more images to display.")
        save_to_csv()
        root.quit()
        time.sleep(0.1)
        root.destroy()
        #os._exit(0)
        return
    
    save_to_csv()

    current_image_path = images.pop(0)

    img = Image.open(current_image_path).convert("RGB")
    labels = ["insect"]  # Replace with relevant labels
    detections = detect(object_detector, img, labels)
    
    width, height = img.size
    scale_factor = 0.398538961038961
    new_width = int(width * scale_factor)
    new_height = int(height * scale_factor)
    scaled_image = img.resize((new_width, new_height)) # Apply scale
    tk_image = ImageTk.PhotoImage(scaled_image)

    # Clear canvas and display the image
    canvas.delete("all")
    canvas.create_image(0, 0, anchor="nw", image=tk_image)
    canvas.image = tk_image
    root.geometry(f"{new_width}x{new_height}")  # Resize window to image size
    
def calculate_precisions():
    global precisions_dict
    precisions = {}

    for threshold in thresholds:
        detections_filtered = [box for box in detections if box['score'] >= threshold]

        TP = 0
        FP = 0
        matched_GT_boxes = set()  # To track matched GT boxes

        for detection in detections_filtered:
            best_IoU = 0
            best_GT_idx = None
            predicted_box = detection["box"]

            # Find the best match for this prediction
            for i, GT_box in enumerate(click_data_scaled[current_image_path]):
                IoU = calculate_iou(GT_box, predicted_box)
                if IoU > best_IoU:
                    best_IoU = IoU
                    best_GT_idx = i

            # If best IoU is above threshold and GT is not matched yet, count as TP
            if best_IoU >= 0.5 and best_GT_idx not in matched_GT_boxes:
                TP += 1
                matched_GT_boxes.add(best_GT_idx)  # Mark GT as matched
            else:
                FP += 1  # If no good match is found, count as FP

        #print(f"threshold: {threshold}")
        #print(f"TP: {TP}")
        #print(f"FP: {FP}")
        # Precision calculation (handle division by zero)
        if len(click_data_scaled[current_image_path]) == 0 and len(detections_filtered) == 0:
            precision = 1
        else:
            precision = TP / (TP + FP) if (TP + FP) > 0 else 0.0
        precisions[threshold] = precision
        
    precisions_dict[current_image_path] = precisions
    
def calculate_recalls():
    global recalls_dict
    recalls = {}

    for threshold in thresholds:
        detections_filtered = [box for box in detections if box['score'] >= threshold]

        TP = 0
        matched_GT_boxes = set()  # To track matched GT boxes

        # Check which GT boxes are matched
        for detection in detections_filtered:
            best_IoU = 0
            best_GT_idx = None
            predicted_box = detection["box"]
            

            for i, GT_box in enumerate(click_data_scaled[current_image_path]):
                IoU = calculate_iou(GT_box, predicted_box)
                if IoU > best_IoU:
                    best_IoU = IoU
                    best_GT_idx = i

            # If best IoU is above threshold and GT is not matched yet, count as TP
            if best_IoU >= 0.5 and best_GT_idx not in matched_GT_boxes:
                TP += 1
                matched_GT_boxes.add(best_GT_idx)  # Mark GT as matched

        # FN: Unmatched GT boxes
        FN = len(click_data_scaled[current_image_path]) - len(matched_GT_boxes)

        # Recall calculation (handle division by zero)
        if len(click_data_scaled[current_image_path]) == 0 and len(detections_filtered) == 0:
            recall = 1
        else:
            recall = TP / (TP + FN) if (TP + FN) > 0 else 0.0
        recalls[threshold] = recall

    recalls_dict[current_image_path] = recalls
    
def calculate_ap(precisions, recalls):
    """
    Computes the Average Precision (AP) for a single image.
    """
    global ap_dict
    # Convert dict to sorted lists
    recall_values = np.array([recalls[r] for r in sorted(recalls.keys())])  # Recall values corresponding to thresholds
    precision_values = np.array([precisions[r] for r in sorted(precisions.keys())])  # Precision in the same order

    ap = abs(np.trapezoid(precision_values, recall_values))

    ap_dict[current_image_path] = float(ap)
    
def threshold_025_calculations():
    threshold = 0.25
    global thresh_025_dict
    results = {}

    detections_filtered = [box for box in detections if box['score'] >= threshold]

    ### Precision calculation
    TP = 0
    FP = 0
    matched_GT_boxes = set()  # To track matched GT boxes

    for detection in detections_filtered:
        best_IoU = 0
        best_GT_idx = None
        predicted_box = detection["box"]

        # Find the best match for this prediction
        for i, GT_box in enumerate(click_data_scaled[current_image_path]):
            IoU = calculate_iou(GT_box, predicted_box)
            if IoU > best_IoU:
                best_IoU = IoU
                best_GT_idx = i

        # If best IoU is above threshold and GT is not matched yet, count as TP
        if best_IoU >= 0.5 and best_GT_idx not in matched_GT_boxes:
            TP += 1
            matched_GT_boxes.add(best_GT_idx)  # Mark GT as matched
        else:
            FP += 1  # If no good match is found, count as FP

    # Precision calculation (handle division by zero)
    if len(click_data_scaled[current_image_path]) == 0 and len(detections_filtered) == 0:
        precision = 1
    else:
        precision = TP / (TP + FP) if (TP + FP) > 0 else 0.0
    results["precision"] = precision
    
    ### Recall calculation
    TP = 0
    matched_GT_boxes = set()  # To track matched GT boxes

    # Check which GT boxes are matched
    for detection in detections_filtered:
        best_IoU = 0
        best_GT_idx = None
        predicted_box = detection["box"]
        

        for i, GT_box in enumerate(click_data_scaled[current_image_path]):
            IoU = calculate_iou(GT_box, predicted_box)
            if IoU > best_IoU:
                best_IoU = IoU
                best_GT_idx = i

        # If best IoU is above threshold and GT is not matched yet, count as TP
        if best_IoU >= 0.5 and best_GT_idx not in matched_GT_boxes:
            TP += 1
            matched_GT_boxes.add(best_GT_idx)  # Mark GT as matched

    # FN: Unmatched GT boxes
    FN = len(click_data_scaled[current_image_path]) - len(matched_GT_boxes)

    # Recall calculation (handle division by zero)
    if len(click_data_scaled[current_image_path]) == 0 and len(detections_filtered) == 0:
        recall = 1
    else:
        recall = TP / (TP + FN) if (TP + FN) > 0 else 0.0
    results["recall"] = recall

    if precision + recall > 0:
        f1_score = 2 * (precision * recall) / (precision + recall)
    else:
        f1_score = 0.0
    results["F1_score"] = f1_score
    
    thresh_025_dict[current_image_path] = results
    
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

def save_current_canvas():
    """Save the current canvas content as a .jpg file."""
    if not current_image_path:
        print("No image to save.")
        return

    # Export the canvas content to a postscript file
    ps_file = "temp_canvas.ps"
    canvas.postscript(file=ps_file, colormode='color')

    # Convert the postscript file to a .jpg image using Pillow
    save_dir = os.path.join(folder_path, "visualized")
    os.makedirs(save_dir, exist_ok=True)  # Create directory if it doesn't exist
    save_path = os.path.join(save_dir, os.path.basename(current_image_path))

    # Open and save the postscript as a .jpg
    with Image.open(ps_file) as img:
        img.convert("RGB").save(save_path, "JPEG")

    # Cleanup temporary file
    os.remove(ps_file)
    print(f"Saved the annotated crop into subfolder visualized")
        
def start_draw(event):
    """Start recording bounding box on mouse press."""
    global start_x, start_y, current_rect
    start_x, start_y = event.x, event.y
    current_rect = None

def update_draw(event):
    """Update rectangle as the mouse is dragged."""
    global current_rect
    if start_x is not None and start_y is not None:
        if current_rect:
            canvas.delete(current_rect)
        current_rect = canvas.create_rectangle(start_x, start_y, event.x, event.y, outline="cyan", width=2)

def end_draw(event):
    """Record bounding box when mouse is released."""
    global click_data, click_data_scaled, current_rect

    x1, y1 = start_x, start_y
    x2, y2 = event.x, event.y

    # Ensure coordinates are sorted correctly (top-left to bottom-right)
    x1, x2 = sorted([x1, x2])
    y1, y2 = sorted([y1, y2])

    # Scale coordinates
    x1_scaled, y1_scaled = round(x1 / scale_factor, 1), round(y1 / scale_factor, 1)
    x2_scaled, y2_scaled = round(x2 / scale_factor, 1), round(y2 / scale_factor, 1)

    #print(f"Bounding box recorded: ({x1_scaled}, {y1_scaled}), ({x2_scaled}, {y2_scaled})")

    # Store the bounding box data
    if current_image_path not in click_data:
        click_data[current_image_path] = []
        click_data_scaled[current_image_path] = []

    click_data[current_image_path].append([x1, y1, x2, y2])
    GT_xmin = min(x1_scaled, x2_scaled)
    GT_xmax = max(x1_scaled, x2_scaled)
    GT_ymin = min(y1_scaled, y2_scaled)
    GT_ymax = max(y1_scaled, y2_scaled)
    click_data_scaled[current_image_path].append([GT_xmin, GT_ymin, GT_xmax, GT_ymax])

def draw_scaled_dot(x, y, col, size):
    """Draw a green dot on the canvas at the scaled coordinates."""
    dot_size = size * scale_factor  # Adjust dot size for scaling
    canvas.create_oval(
        x - dot_size, y - dot_size, x + dot_size, y + dot_size,
        fill=col, outline=col
    )

def undo_last_click(event=None):
    """Undo the last recorded click."""
    global click_data, click_data_scaled
    if current_image_path in click_data and click_data[current_image_path]:
        removed_click = click_data[current_image_path].pop()
        removed_click = click_data_scaled[current_image_path].pop()
        print(f"Last click undone: {removed_click}")
        show_image_with_clicks()

def skip_image(self, event=None):
    """Skip to the next image and log an entry in the output CSV."""
    global current_image_path, click_data, click_data_scaled
    
    click_data[current_image_path] = []
    click_data_scaled[current_image_path] = []
    
    calculate_precisions()
    calculate_recalls()
    threshold_025_calculations()
    calculate_ap(precisions_dict[current_image_path], recalls_dict[current_image_path])
    
    print(f"ap: {ap_dict[current_image_path]}")

    save_to_csv() # Save Results into CSV
    show_next_image(self) # Move to the next image

def show_image_with_clicks():
    """Redraw the image with previously recorded clicks."""
    pil_image = Image.open(current_image_path)
    width, height = pil_image.size
    scale_factor = 0.398538961038961
    new_width = int(width * scale_factor)
    new_height = int(height * scale_factor)
    scaled_image = pil_image.resize((new_width, new_height)) # Apply scale
    tk_image = ImageTk.PhotoImage(scaled_image)

    canvas.delete("all")
    canvas.create_image(0, 0, anchor="nw", image=tk_image)
    canvas.image = tk_image

    # Redraw clicks as green dots
    if current_image_path in click_data:
        for x1, y1, x2, y2 in click_data[current_image_path]:
            canvas.create_rectangle(x1, y1, x2, y2, outline="cyan", width=2)
            #draw_scaled_dot(x, y, col = "cyan", size = 5)
            
def save_to_csv():
    """Save click data to a CSV file in the user-specified folder."""
    csv_file = os.path.join(folder_path, "click_data.csv")
    file_exists = os.path.isfile(csv_file)

    # Load existing data if CSV exists
    processed_images = set()
    if file_exists:
        existing_data = pd.read_csv(csv_file)
        processed_images = set(existing_data["image_path"].tolist())
        #print(f"Loaded {len(processed_images)} processed images from existing CSV.")

    # Open file in append mode if it exists
    with open(csv_file, mode="a" if file_exists else "w", newline="") as file:
        writer = csv.writer(file)

        # Write header if creating a new file
        if not file_exists:
            writer.writerow(csv_header)

        # Write new data
        for image_path, AP_value in ap_dict.items():
            if image_path in processed_images:
                continue
            GT_bounding_boxes = click_data_scaled.get(image_path) # [xmin, ymin, xmax, ymax]
            precision_values = precisions_dict.get(image_path)
            recall_values = recalls_dict.get(image_path)
            thresh_025_values = thresh_025_dict.get(image_path)

            row = [image_path]
            row.extend([GT_bounding_boxes])
            row.extend([precision_values[0], precision_values[0.1], precision_values[0.2], precision_values[0.3], 
                        precision_values[0.4], precision_values[0.5], precision_values[0.6], precision_values[0.7], 
                        precision_values[0.8], precision_values[0.9], precision_values[1]])
            row.extend([recall_values[0], recall_values[0.1], recall_values[0.2], recall_values[0.3], 
                        recall_values[0.4], recall_values[0.5], recall_values[0.6], recall_values[0.7], 
                        recall_values[0.8], recall_values[0.9], recall_values[1]])
            row.extend([AP_value])
            row.extend([thresh_025_values["precision"], thresh_025_values["recall"], thresh_025_values["F1_score"]])

            writer.writerow(row)

    print("Data saved to csv file (main directory).")

def main():
    """Main function to run the application."""
    global root, canvas
    root = Tk()
    root.title("Image Annotation Tool")
    
    canvas = Canvas(root)
    canvas.pack(fill="both", expand=True)

    # Bind events
    #canvas.bind("<Button-1>", record_click)  # Left-click for recording clicks
    canvas.bind("<ButtonPress-1>", start_draw)
    canvas.bind("<B1-Motion>", update_draw)
    canvas.bind("<ButtonRelease-1>", end_draw)
    root.bind("a", show_next_image) # Press "a" on the keyboard to advance to the next image
    root.bind("<space>", skip_image)  # Spacebar for skipping images
    root.bind("d", undo_last_click)  # 'd' key for undoing the last click

    # Select folder and start displaying images
    select_folder()

    root.mainloop()

if __name__ == "__main__":
    main()