## Functions to perform the measurement of the body lenght of the insects in the cropped images previously created by GroundingDino and the rest of the pipeline.


import matplotlib.pyplot as plt
from PIL import Image
import ipywidgets
import sleap
import glob
import math
import json
import sys
import csv
import re
import os


## Extracts 'Year', 'Month', 'Day', 'Hour', 'Minute' and 'Second' from the name of the image file and returns the values.
def extract_datetime(filename):
    # Use regular expression to extract the datetime part from the filename
    match = re.match(r"(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})_crop_\d+\.jpg", filename)
    if match:
        year, month, day, hour, minute, second = match.groups()
        return int(year), int(month), int(day), int(hour), int(minute), int(second)
    else:
        raise ValueError("Filename does not match the expected pattern")

## Looks for the most recent model in the folder that is provided to it. This is handy if you trained multiple models and saved them all to the same folder. 
def fetch_most_recent_model(dir_to_fetch):
    models = [f for f in glob.glob(os.path.join(dir_to_fetch, '*')) if os.path.isdir(f)]
    models.sort(key=lambda x: os.path.getmtime(x), reverse=True)
    most_recent_model = models[0] if models else None
    return most_recent_model

## Takes an input path and reduces it by n levels. 
## e.g: reduce_path(C:\Users\Pictures\Moths, 1) would return C:\Users\Pictures
def reduce_path(path, levels):
    for _ in range(levels):
        path = os.path.dirname(path)
    return path

# Function to calculate the Euclidean distance between two points
def calculate_distance(x1, y1, x2, y2):
    distance = math.sqrt((x2 - x1)**2 + (y2 - y1)**2)
    if not math.isnan(distance):
        return int(math.sqrt((x2 - x1)**2 + (y2 - y1)**2))
    else:
        return None

## Calculates the lenght (in pixel) of the 2 segments making up each insect.
## Adds the lenght of the thorax and the abdomen for a total insect lenght.
## Returns the "Head_lenght", the "Abdomen_length" and the "insect_length_total"
## If either the thorax, or the abdomen lenght could not be recognized by the algorithm in the image, the total lenght is set to None.
def calculate_body_length_px(coordinates):
    head_x, head_y = coordinates[0]
    thorax_x, thorax_y = coordinates[1]
    abdomen_x, abdomen_y = coordinates[2]
    
    # Calculate distances
    distance_head_thorax = calculate_distance(head_x, head_y, thorax_x, thorax_y)
    distance_thorax_abdomen = calculate_distance(thorax_x, thorax_y, abdomen_x, abdomen_y)
    
    try:
        length_total = distance_head_thorax + distance_thorax_abdomen
    except TypeError:
        length_total = None
    # Store the distances in a dictionary
    results = {
        'Head_length': distance_head_thorax,
        'Abdomen_length': distance_thorax_abdomen,
        'insect_length_total': length_total
    }
    
    return results

## Fetches the path where all the environments are stored on the current machine.
def get_local_env_path():
    envs_path = sys.prefix
    envs_path = os.path.abspath(os.path.join(envs_path, os.pardir))
    print(f"The envs directory is: {envs_path}")
    return envs_path

## Fetch the environment path of the current machine:
envs_path = get_local_env_path()

## The function that brings it all together and does the inference.
## By default the function expects the model to be located in the "sleap" env folder in a subfolder called "models"
## Function also saves an annotated version of the cropped image in the "cropped_and_annotated" folder of the current day. This annotated image has the predicted segments of the insect body on it and will later also receive a text specifying the taxonomic classification and the length as a number. If the algorithm failed in creating a lenght measurement, the original crop is saved instead.
## Cropped images need to be in .jpg format.
## Print functions here will not appear in the main pipeline unless predict_body_length encounters an error. 
## Inference results also saved as "body_length_results.csv" in the results folder of the day.
def predict_body_length(cropped_images_folder, pixel_scale, models_folder = os.path.join(envs_path, "sleap", "models")):
    images = os.listdir(cropped_images_folder)
    images = sorted(images)
    most_recent_model = fetch_most_recent_model(models_folder)
    predictor = sleap.load_model([most_recent_model], batch_size=16)
    length_results = []
    for image in images:
        if not image.endswith('.jpg'):
            continue
        try:
            print(f"Measuring {image}")
            year, month, day, hour, minute, second = extract_datetime(image)
            img_arg = os.path.join(cropped_images_folder, image)
            root_folder = reduce_path(cropped_images_folder, levels = 1)
            save_path = os.path.join(root_folder, "cropped_and_annotated")
            os.makedirs(save_path, exist_ok=True)
            save_path = os.path.join(save_path, image)
            img = sleap.load_video(img_arg)

            predictions = predictor.predict(img)

            print(f"Length prediction Laenge = {len(predictions)}\npredictions = {predictions}")
            prediction = predictions[0]
            
            if prediction.instances:
                predictions[0].plot(scale=1)
                plt.savefig(save_path, format='jpg')
                print(f"prediction = {prediction.instances}")
                skeleton_values = prediction[0].numpy()
                lengths = calculate_body_length_px(skeleton_values)
            else:
                img = Image.open(img_arg)
                img.save(save_path)
                lengths = None
            print("Model used:", most_recent_model)
            print(root_folder)
            print(lengths)
            if lengths is not None:
                length_results.append([image, year, month, day, hour, minute, second,
                                   "{:.2f}".format(lengths['Head_length']/pixel_scale) if lengths['Head_length'] is not None else None, 
                                   "{:.2f}".format(lengths['Abdomen_length']/pixel_scale) if lengths['Abdomen_length'] is not None else None, 
                                   "{:.2f}".format(lengths['insect_length_total']/pixel_scale) if lengths['insect_length_total'] is not None else None])
            else:
                length_results.append([image, year, month, day, hour, minute, second, None, None, None])
            ran_clean = True
        except Exception as e:
            ran_clean = False
            raise RuntimeError(f"An error occurred while analyzing image {image}. Length measurement halted.\nThe Error: {e}")
            
    length_csv_file_path = os.path.join(root_folder, "results", "body_length_results.csv")
    with open(length_csv_file_path, mode='w', newline='') as file:
        writer = csv.writer(file)
        ## Columns of the results csv:
        writer.writerow(['Filename', 'Year', 'Month', 'Day', 'Hour', 'Minute', 'Second', 'Head_length', 'Abdomen_length', 'Insect_length_total'])
        writer.writerows(length_results)
    
    return ran_clean

## The wrapper that allows the code to be run from a terminal command line. Essential for the integration with the rest of the pipeline!
if __name__ == "__main__":
    import sys
    cropped_images_folder_path = sys.argv[1]
    pixel_scale = float(sys.argv[2])
    ran_clean = predict_body_length(cropped_images_folder_path, pixel_scale)
    print(json.dumps(ran_clean))