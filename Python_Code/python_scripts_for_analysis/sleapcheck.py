### Script for generating ground truths for the performace metric of SLEAP. Input a folder containing subfolders for each camera. Update the scale_dict variable in this script to reflect the camera names and pixel scales of the cameras you are using. The subfolders contained in the main folder should have the same names as the entries in this dictionary. 

### The code lets SLEAP run in the background to generate predicted keypoints. The user can then click on the image to generate ground truths. Images are always annotated from head to abdomen (3 clicks). An OKS (object keypoint similarity) score is calculated. This is used in further calculations to get to a mAP score for the performance of SLEAP on the data you used.


from decimal import Decimal, ROUND_HALF_UP
from tkinter import Tk, Canvas, filedialog
import matplotlib.pyplot as plt
from PIL import Image, ImageTk
import pandas as pd
import numpy as np
import ipywidgets
import random
import sleap
import glob
import math
import json
import sys
import csv
import re
import os


# Global variables
click_data = {}
skeleton_values_dict = {}
lengths_predicted_dict = {}
current_clicks_dict = {}
manual_sizes_dict = {}
current_clicks = []
current_image_path = None
folder_path = None
images = []
canvas = None
root = None
displ_scale_factor = 2  # Scale factor for image display
uncertainty_factor = 0.025
scale_dict = {
    "382": 92,
    "383": 92,
    "384": 92,
    "385": 90,
    "387": 90,
    "388": 91,
    "389": 94,
    "391": 89,
    "421": 92
}
thresholds = [0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95]

def fetch_most_recent_model(dir_to_fetch):
    models = [f for f in glob.glob(os.path.join(dir_to_fetch, '*')) if os.path.isdir(f)]
    models.sort(key=lambda x: os.path.getmtime(x), reverse=True)
    most_recent_model = models[0] if models else None
    return most_recent_model

def calculate_distance(x1, y1, x2, y2):
    distance = math.sqrt((x2 - x1)**2 + (y2 - y1)**2)
    if not math.isnan(distance):
        return int(math.sqrt((x2 - x1)**2 + (y2 - y1)**2))
    else:
        return None
    
def calculate_body_length_mm(coordinates, pixel_scale, predicted):
    if coordinates is None:
        results = {
            'Head_length_predicted': None,
            'Abdomen_length_predicted': None,
            'insect_length_total_predicted': None
        }
        return results
    head_x, head_y = coordinates[0]
    thorax_x, thorax_y = coordinates[1]
    abdomen_x, abdomen_y = coordinates[2]
    
    # Calculate distances
    distance_head_thorax = calculate_distance(head_x, head_y, thorax_x, thorax_y)
    distance_thorax_abdomen = calculate_distance(thorax_x, thorax_y, abdomen_x, abdomen_y)

    try:
        distance_head_thorax = round((distance_head_thorax / pixel_scale) * 10, 2)
    except TypeError:
        distance_head_thorax = None
    try:
        distance_thorax_abdomen = round((distance_thorax_abdomen / pixel_scale) * 10, 2)
    except TypeError:
        distance_thorax_abdomen = None
    
    try:
        length_total = round(distance_head_thorax + distance_thorax_abdomen, 2)
    except TypeError:
        length_total = None
    # Store the distances in a dictionary
    if predicted:
        results = {
            'Head_length_predicted': distance_head_thorax,
            'Abdomen_length_predicted': distance_thorax_abdomen,
            'insect_length_total_predicted': length_total
        }
    else:
        results = {
            'Head_length_labeled': distance_head_thorax,
            'Abdomen_length_labeled': distance_thorax_abdomen,
            'insect_length_total_labeled': length_total
        }
    
    return results

def get_local_env_path():
    envs_path = sys.prefix
    envs_path = os.path.abspath(os.path.join(envs_path, os.pardir))
    #print(f"The envs directory is: {envs_path}")
    return envs_path

envs_path = get_local_env_path()
models_folder = os.path.join(envs_path, "sleap", "models")
most_recent_model = fetch_most_recent_model(models_folder)
predictor = sleap.load_model([most_recent_model], batch_size=16)

def predict_body_length(current_image_path, predictor, pixel_scale):
    #length_results = []
    global skeleton_values_dict, lengths_predicted_dict, skeleton_values
    try:
        img_arg = current_image_path
        img = sleap.load_video(img_arg)

        predictions = predictor.predict(img)

        #print(f"Length prediction Laenge = {len(predictions)}\npredictions = {predictions}")
        prediction = predictions[0]
            
        if prediction.instances:
            #predictions[0].plot(scale=1)
            #plt.savefig(save_path, format='jpg')
            #print(f"prediction = {prediction.instances}")
            skeleton_values = prediction[0].numpy()
            skeleton_values = np.round(skeleton_values, decimals=1)
            skeleton_values_dict[current_image_path] = skeleton_values
            #print(f"skeleton_values: {skeleton_values}")
            lengths_predicted = calculate_body_length_mm(skeleton_values, pixel_scale, predicted = True)
            lengths_predicted_dict[current_image_path] = lengths_predicted
        else:
            lengths = None
            lengths_predicted = None
            skeleton_values_dict[current_image_path] = None
        ran_clean = True
    except Exception as e:
        ran_clean = False
        raise RuntimeError(f"An error occurred while analyzing image {image}. Length measurement halted.\nThe Error: {e}")
    
    return lengths_predicted

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
        show_next_image()

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
    
def center_window_on_screen(window, width, height):
    """
    Center the Tkinter window on the monitor.
    :param window: The Tkinter root window or Toplevel window.
    :param width: Width of the window to display.
    :param height: Height of the window to display.
    """
    screen_width = window.winfo_screenwidth()  # Width of the screen
    screen_height = window.winfo_screenheight()  # Height of the screen

    # Calculate x and y positions for the window
    x = (screen_width // 2) - (width // 2)
    y = (screen_height // 2) - (height // 2)

    # Set the window's position
    window.geometry(f"{width}x{height}+{x}+{y}")
        
def show_next_image():
    """Display the next image randomly from the list."""
    global current_image_path, canvas, images, scale_factor
    if not images:
        print("No more images to display.")
        save_to_csv()
        root.destroy()
        return
    
    if current_image_path:
        save_current_canvas()

    current_image_path = images.pop(0)
    parent_folder = image_folder_map[current_image_path]  # Get the parent folder of the image
    scale_factor = scale_dict.get(parent_folder, 90)  # Default scale factor if not in dictionary

    lengths = predict_body_length(current_image_path, predictor, scale_factor)

    pil_image = Image.open(current_image_path)
    width, height = pil_image.size
    scaled_image = pil_image.resize((width * displ_scale_factor, height * displ_scale_factor))  # Apply scale
    tk_image = ImageTk.PhotoImage(scaled_image)

    # Clear canvas and display the image
    canvas.delete("all")
    canvas.create_image(0, 0, anchor="nw", image=tk_image)
    canvas.image = tk_image
    #root.geometry(f"{scaled_image.width}x{scaled_image.height}")  # Resize window to image size
    center_window_on_screen(root, scaled_image.width, scaled_image.height)
    cam_and_image = os.path.join(*current_image_path.split(os.sep)[-2:])
    print(f"Displaying: {cam_and_image} (Scale Factor: {scale_factor})")

def record_click(event):
    """Record the coordinates of a click on the current image and show a green dot."""
    global click_data, current_clicks_dict, manual_sizes_dict
    # Scale click coordinates back to original size
    x, y = event.x / displ_scale_factor, event.y / displ_scale_factor
    x = round(x, 1)
    y = round(y, 1)
    if current_image_path not in click_data:
        click_data[current_image_path] = []

    # Record the click if less than 3 clicks are recorded
    if len(click_data[current_image_path]) < 3:
        click_data[current_image_path].append([x, y])
        print(f"Click recorded: {x}, {y}")
        predicted_coords = skeleton_values[len(click_data[current_image_path])-1]
        print(f"predicted_coords: {predicted_coords[0]}, {predicted_coords[1]}")

        # Draw a small green dot at the scaled location
        canvas.create_line(
            event.x, event.y,  # Coordinates of the green dot
            predicted_coords[0]*displ_scale_factor, predicted_coords[1]*displ_scale_factor,  # Coordinates of the cyan dot
            fill="white",  # Line color
            width=2  # Line thickness
        )
        draw_scaled_dot(predicted_coords[0]*displ_scale_factor, predicted_coords[1]*displ_scale_factor, col = "orange", size = 1)
        draw_scaled_dot(event.x, event.y, col = "cyan", size = 1.5)

        # Automatically move to the next image after 3 clicks
        if len(click_data[current_image_path]) == 3:
            predicted_coords = skeleton_values[len(click_data[current_image_path])-1]
            draw_scaled_dot(predicted_coords[0]*displ_scale_factor, predicted_coords[1]*displ_scale_factor, col = "orange", size = 1)
            draw_scaled_dot(event.x, event.y, col = "cyan", size = 1.5)
            current_clicks = click_data[current_image_path]
            current_clicks = np.array(current_clicks)
            current_clicks_dict[current_image_path] = current_clicks
            manual_sizes = calculate_body_length_mm(current_clicks, scale_factor, predicted = False)
            manual_sizes_dict[current_image_path] = manual_sizes
            save_to_csv()
            show_next_image()

def draw_scaled_dot(x, y, col, size):
    """Draw a green dot on the canvas at the scaled coordinates."""
    dot_size = size * displ_scale_factor  # Adjust dot size for scaling
    canvas.create_oval(
        x - dot_size, y - dot_size, x + dot_size, y + dot_size,
        fill=col, outline=col
    )

def undo_last_click(event=None):
    """Undo the last recorded click."""
    global click_data
    if current_image_path in click_data and click_data[current_image_path]:
        removed_click = click_data[current_image_path].pop()
        print(f"Last click undone: {removed_click}")
        show_image_with_clicks()

def skip_image(event=None):
    """Skip to the next image and log an entry in the output CSV."""
    global current_image_path
    
    #for i in range(3):
    #    coords = skeleton_values[i]
    #    draw_scaled_dot(coords[0]*displ_scale_factor, coords[1]*displ_scale_factor, col = "orange", size = 1)

    # Path to the CSV file
    csv_file = os.path.join(folder_path, "click_data.csv")

    # Load existing column names if the file exists, or define them manually
    if os.path.isfile(csv_file):
        with open(csv_file, mode='r') as file:
            reader = csv.DictReader(file)
            fieldnames = reader.fieldnames  # Use existing column names
    else:
        # Define the full column structure here (replace with your actual column names)
        fieldnames = ["image_path", "image_width", "image_height", 
                             "head_labeled", "thorax_labeled", "abdomen_labeled",
                             "head_predicted", "thorax_predicted", "abdomen_predicted",
                             "head_length_labeled", "abdomen_length_labeled", "total_length_labeled",
                             "head_length_predicted", "abdomen_length_predicted", "total_length_predicted",
                             "OKS_score",
                             "FP", "FN",
                             "TP.5", "TP.55", "TP.6", "TP.65", "TP.7", "TP.75", "TP.8", "TP.85", "TP.9", "TP.95",
                             "bb_min_x", "bb_min_y", "bb_max_x", "bb_max_y", "bb_area", "pixel_scale"]

    # Create a blank row with all columns empty except for `image_path`, `OKS_score`, and the algorithm predictions
    skeleton_values = skeleton_values_dict.get(current_image_path)
    lengths_predicted = lengths_predicted_dict.get(current_image_path)
    FP = skeleton_values is not None
    skipped_entry = {col: "" for col in fieldnames}
    skipped_entry["image_path"] = current_image_path
    skipped_entry["OKS_score"] = 0  # Set OKS score to 0 for skipped images
    if skeleton_values is not None:
        skipped_entry["head_predicted"] = skeleton_values[0]
        skipped_entry["thorax_predicted"] = skeleton_values[1]
        skipped_entry["abdomen_predicted"] = skeleton_values[2]
        skipped_entry["head_length_predicted"] = lengths_predicted['Head_length_predicted']
        skipped_entry["abdomen_length_predicted"] = lengths_predicted['Abdomen_length_predicted']
        skipped_entry["total_length_predicted"] = lengths_predicted['insect_length_total_predicted']
    skipped_entry["FP"] = FP
    skipped_entry["FN"] = "False"
    skipped_entry["TP.5"] = "False"
    skipped_entry["TP.55"] = "False"
    skipped_entry["TP.6"] = "False"
    skipped_entry["TP.65"] = "False"
    skipped_entry["TP.7"] = "False"
    skipped_entry["TP.75"] = "False"
    skipped_entry["TP.8"] = "False"
    skipped_entry["TP.85"] = "False"
    skipped_entry["TP.9"] = "False"
    skipped_entry["TP.95"] = "False"
    skipped_entry["pixel_scale"] = scale_factor

    # Append the skipped entry to the CSV
    write_header = not os.path.isfile(csv_file)  # Write header only if file doesn't exist
    with open(csv_file, mode='a', newline='') as file:
        writer = csv.DictWriter(file, fieldnames=fieldnames)
        if write_header:
            writer.writeheader()  # Write the header only once
        writer.writerow(skipped_entry)

    print(f"Skipped image logged: {skipped_entry}")
    
    # Move to the next image
    show_next_image()

def show_image_with_clicks():
    """Redraw the image with previously recorded clicks."""
    pil_image = Image.open(current_image_path)
    width, height = pil_image.size
    scaled_image = pil_image.resize((width * displ_scale_factor, height * displ_scale_factor))
    tk_image = ImageTk.PhotoImage(scaled_image)

    canvas.delete("all")
    canvas.create_image(0, 0, anchor="nw", image=tk_image)
    canvas.image = tk_image

    # Redraw clicks as green dots
    if current_image_path in click_data:
        for x, y in click_data[current_image_path]:
            draw_scaled_dot(x * displ_scale_factor, y * displ_scale_factor)


def edit_keypoints(kpts):
    # Convert input to numpy array (assuming kpts are of shape (N, 2) with only x and y values)
    kpts = np.array(kpts).reshape(-1, 2)
    
    # Initialize visibility as 1 (visible) for all keypoints
    vi = np.ones(len(kpts))

    # Check if any keypoint has NaN in the x or y value, and set visibility to 0 if so
    for i, kp in enumerate(kpts):
        if np.isnan(kp[0]) or np.isnan(kp[1]):
            vi[i] = 0  # Set visibility to 0 if either x or y is NaN

    return kpts, vi

def OKS(kpts_labeled, kpts_predicted, uncertainty_factor, bb_area):

    kpts1, vi1 = edit_keypoints(kpts_labeled)
    kpts2, vi2 = edit_keypoints(kpts_predicted)

    if np.shape(kpts1) != np.shape(kpts2):
        print(kpts1, kpts2)
        print(np.shape(kpts1), np.shape(kpts2))
        raise ValueError("not same size")
    
    k = uncertainty_factor
    s = bb_area

    d = np.linalg.norm(kpts1 - kpts2, ord=2, axis=1)
    v = np.ones(len(d))

    for part in range(len(d)):
        if vi1[part] == 0 or vi2[part] == 0:
            d[part] = 0
            v[part] = 0
    
    if np.sum(v)!=0:
        #print(f"d: {d}")
        #print(f"k: {k}")
        #print(f"v: {v}")
        #print(f"s: {s}")
        OKS = (np.sum([(np.exp((-d[i]**2)/(2*(s**2)*(k**2))))*v[i] for i in range(len(d))])/3) ## Die 3 hier sagt, dass es 3 Körperteile gibt
    else:
        OKS = 0
    OKS = float(Decimal(str(OKS)).quantize(Decimal('0.000001'), rounding=ROUND_HALF_UP))

    return OKS
            
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
            writer.writerow(["image_path", "image_width", "image_height", 
                             "head_labeled", "thorax_labeled", "abdomen_labeled",
                             "head_predicted", "thorax_predicted", "abdomen_predicted",
                             "head_length_labeled", "abdomen_length_labeled", "total_length_labeled",
                             "head_length_predicted", "abdomen_length_predicted", "total_length_predicted",
                             "OKS_score",
                             "FP", "FN",
                             "TP.5", "TP.55", "TP.6", "TP.65", "TP.7", "TP.75", "TP.8", "TP.85", "TP.9", "TP.95",
                             "bb_min_x", "bb_min_y", "bb_max_x", "bb_max_y", "bb_area", "pixel_scale"])

        # Write new data
        for image_path, clicks in click_data.items():
            if image_path in processed_images:
                # cam_and_image = os.path.join(*image_path.split(os.sep)[-2:])
                # print(f"Skipping already processed image: {cam_and_image}")
                continue

            # Get original dimensions
            pil_image = Image.open(image_path)
            width, height = pil_image.width, pil_image.height
            
            # Calculate bounding box
            if clicks:
                min_x = min(point[0] for point in clicks)
                min_y = min(point[1] for point in clicks)
                max_x = max(point[0] for point in clicks)
                max_y = max(point[1] for point in clicks)
                bb_area = round((max_x - min_x) * (max_y - min_y), 2)
            else:
                min_x = min_y = max_x = max_y = bb_area = None

            # Prepare row
            current_clicks = current_clicks_dict.get(image_path)
            skeleton_values = skeleton_values_dict.get(image_path)
            lengths_predicted = lengths_predicted_dict.get(image_path)
            manual_sizes = manual_sizes_dict.get(image_path)
            oks = OKS(current_clicks, skeleton_values, uncertainty_factor, bb_area)
            #oks = round(oks, 3)
            print(f"OKS: {oks}")
            skeleton_empty = skeleton_values is None
            if skeleton_empty:
                FN = True
            else:
                FN = False
            
            TP = {f"TP_{int(th * 100)}": False for th in thresholds}
            for th in thresholds:
                if oks > th:
                    TP[f"TP_{int(th * 100)}"] = True

            row = [image_path, width, height]
            row.extend([current_clicks[0], current_clicks[1], current_clicks[2]])
            row.extend([skeleton_values[0], skeleton_values[1], skeleton_values[2]])
            row.extend([manual_sizes['Head_length_labeled'], manual_sizes['Abdomen_length_labeled'], manual_sizes['insect_length_total_labeled']])
            row.extend([lengths_predicted['Head_length_predicted'], lengths_predicted['Abdomen_length_predicted'], lengths_predicted['insect_length_total_predicted']])
            row.extend([oks])
            row.extend(["False"])
            row.extend([FN])
            row.extend([TP['TP_50'], TP['TP_55'], TP['TP_60'], TP['TP_65'], TP['TP_70'], TP['TP_75'], TP['TP_80'], TP['TP_85'], TP['TP_90'], TP['TP_95']])
            row.extend([min_x, min_y, max_x, max_y, bb_area, scale_factor])

            writer.writerow(row)

    print(f"Data saved to csv file (main directory).")

def main():
    """Main function to run the application."""
    global root, canvas
    root = Tk()
    root.title("Image Annotation Tool")
    
    canvas = Canvas(root)
    canvas.pack(fill="both", expand=True)

    # Bind events
    canvas.bind("<Button-1>", record_click)  # Left-click for recording clicks
    root.bind("<space>", skip_image)  # Spacebar for skipping images
    root.bind("d", undo_last_click)  # 'd' key for undoing the last click

    # Select folder and start displaying images
    select_folder()

    root.mainloop()

if __name__ == "__main__":
    main()