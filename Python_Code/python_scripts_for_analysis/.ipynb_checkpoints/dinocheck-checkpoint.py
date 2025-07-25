### Script for generating TP, TN, FP and FN values for the grounding DINO predictions

import os
import random
import tkinter as tk
from tkinter import filedialog
from PIL import Image, ImageTk, ImageDraw
import csv
import torch
from transformers import pipeline
from typing import List, Dict, Any

# DetectionResult placeholder (you'll need to define or import it in practice)
class DetectionResult:
    def __init__(self, box):
        self.box = box

    @staticmethod
    def from_dict(result):
        box = result["box"]
        return DetectionResult(box=BoundingBox(xyxy=box["xyxy"]))

class BoundingBox:
    def __init__(self, xyxy):
        self.xyxy = xyxy

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

def detect(object_detector, image: Image.Image, labels: List[str], threshold: float = 0.25) -> List[Dict[str, Any]]:
    """
    Use Grounding DINO to detect a set of labels in an image.
    """
    labels = [label if label.endswith(".") else label + "." for label in labels]
    results = object_detector(image, candidate_labels=labels, threshold=threshold)
    return results

class ImageLabelingApp:
    def __init__(self, root):
        self.root = root
        self.root.title("Image Labeling App")
        
        # Variables
        self.image_folder = filedialog.askdirectory(title="Select Image Folder")
        self.csv_file = os.path.join(self.image_folder, "labels.csv")
        self.image_paths = self.load_images()
        random.shuffle(self.image_paths)
        self.current_index = 0
        self.current_label = None
        self.labels_data = { "TP": 0, "TN": 0, "FP": 0, "FN": 0 }

        if not self.image_paths:
            self.end_app()
            return

        self.init_csv()
        
        # Load Grounding DINO model
        self.object_detector = load_grounding_dino_model()

        # UI Setup
        self.image_label = tk.Label(self.root)
        self.image_label.pack()

        self.value_entry = tk.Entry(self.root)
        self.value_entry.pack()
        self.value_entry.bind("<Return>", self.save_and_next)

        self.buttons_frame = tk.Frame(self.root)
        self.buttons_frame.pack()

        self.buttons = {}
        for label in ["TP", "TN", "FP", "FN"]:
            button = tk.Button(self.buttons_frame, text=label, command=lambda l=label: self.select_label(l))
            button.pack(side="left", padx=5, pady=5)
            self.buttons[label] = button

        # Display the first image
        self.display_image()

    def load_images(self):
        """Load image paths from the selected folder and exclude already labeled images."""
        supported_extensions = (".png", ".jpg", ".jpeg", ".bmp", ".gif")
        all_images = [
            os.path.join(self.image_folder, f) 
            for f in os.listdir(self.image_folder) 
            if f.lower().endswith(supported_extensions)
        ]
        labeled_images = self.get_labeled_images()
        return [img for img in all_images if img not in labeled_images]

    def get_labeled_images(self):
        """Retrieve a list of already labeled image paths from the CSV file."""
        if not os.path.exists(self.csv_file):
            return []
        with open(self.csv_file, mode="r", newline="") as file:
            reader = csv.reader(file)
            next(reader, None)  # Skip header
            return [row[0] for row in reader]

    def init_csv(self):
        """Initialize the CSV file with headers if it doesn't exist."""
        if not os.path.exists(self.csv_file):
            with open(self.csv_file, mode="w", newline="") as file:
                writer = csv.writer(file)
                writer.writerow(["Image Path", "TP", "TN", "FP", "FN"])

    def display_image(self):
        """Display the current image with bounding boxes."""
        if self.current_index >= len(self.image_paths):
            self.end_app()
            return

        image_path = self.image_paths[self.current_index]
        img = Image.open(image_path).convert("RGB")
        
        # Detect objects and draw bounding boxes
        labels = ["insect"]  # Replace with relevant labels
        detections = detect(self.object_detector, img, labels)
        draw = ImageDraw.Draw(img)
        for detection in detections:
            box = detection["box"]
            #print(box)
            box['xyxy'] = [box['xmin'], box['ymin'], box['xmax'], box['ymax']]
            xyxy = box["xyxy"]
            draw.rectangle(xyxy, outline="red", width=3)
            draw.text((xyxy[0], xyxy[1]), detection["label"], fill="red")

        img = img.resize((1093, 821), Image.Resampling.LANCZOS)
        img = ImageTk.PhotoImage(img)

        self.image_label.img = img  # Keep a reference to prevent garbage collection
        self.image_label.config(image=img)  # Update the label to show the new image

    def select_label(self, label):
        """Handle button click and focus entry box."""
        self.save_value()  # Save the value for the previously selected label
        self.current_label = label
        self.value_entry.delete(0, tk.END)  # Clear the entry box
        self.value_entry.focus()

    def save_value(self):
        """Save the value for the currently selected label."""
        if self.current_label:
            value = self.value_entry.get()
            if value.isdigit():
                self.labels_data[self.current_label] = int(value)
            self.value_entry.delete(0, tk.END)

    def save_and_next(self, event):
        """Save all data for the current image and move to the next image."""
        self.save_value()

        # Write the data for the current image to the CSV file
        if self.current_index < len(self.image_paths):
            image_path = self.image_paths[self.current_index]
            with open(self.csv_file, mode="a", newline="") as file:
                writer = csv.writer(file)
                writer.writerow([
                    image_path,
                    self.labels_data["TP"], 
                    self.labels_data["TN"], 
                    self.labels_data["FP"], 
                    self.labels_data["FN"]
                ])

        # Reset for the next image
        self.labels_data = { "TP": 0, "TN": 0, "FP": 0, "FN": 0 }
        self.value_entry.delete(0, tk.END)
        self.current_label = None
        self.current_index += 1  # Move to the next image
        self.display_image()  # Display the next image

    def end_app(self):
        """Close the app when all images are processed."""
        for widget in self.root.winfo_children():
            widget.destroy()  # Remove all widgets from the root window
    
        # Show a closing message
        closing_message = tk.Label(self.root, text="All images have been labeled!", font=("Arial", 30))
        closing_message.pack(pady=100, padx=100)

        # Automatically close after 2 seconds
        self.root.after(4000, self.root.destroy)

if __name__ == "__main__":
    root = tk.Tk()
    app = ImageLabelingApp(root)
    root.mainloop()