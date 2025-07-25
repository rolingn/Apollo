'''
Key functions that allow the main pipeline to work properly. Not many (if any) useless functions contained here.
'''

from transformers import AutoModelForMaskGeneration, AutoProcessor, pipeline
from PIL import Image, ImageDraw, ImageFont, PngImagePlugin
from typing import Any, List, Dict, Optional, Union, Tuple
from collections import OrderedDict, defaultdict
#from bioclip import TreeOfLifeClassifier, Rank
import plotly.graph_objects as go
from dataclasses import dataclass
import matplotlib.pyplot as plt
import plotly.express as px
from enum import Enum
import pandas as pd
import numpy as np
import warnings
import requests
import random
import torch
import glob
import json
import sys
import cv2
import re
import os

## Custom functions import from classes. Not sure if this is still needed but to lazy to check.
from FunktionenZumImportieren.classes import *


def load_image(image_str: str) -> Image.Image:
    if image_str.startswith("http"):
        image = Image.open(requests.get(image_str, stream=True).raw).convert("RGB")
    else:
        image = Image.open(image_str).convert("RGB")

    return image

def get_boxes(results: DetectionResult) -> List[List[List[float]]]:
    '''
    Takes the DetectionResults from GroundingDino as an input and outputs the bounding boxes.
    '''
    boxes = []
    for result in results:
        xyxy = result.box.xyxy
        boxes.append(xyxy)

    return [boxes]

def load_grounding_dino_model():
    '''
    Function to load the GroundingDino model. Model has never been used with cuda so far. Running it with mps on a Mac resulted in hugely different detection
    results for some reason (basically no detections at all). CPU is totally fine though on newer Macs (also with M processor).
    '''
    if torch.backends.mps.is_available():
        #device = "mps"
        device = "cpu"
    elif torch.cuda.is_available():
        device = "cuda"
    else:
        device = "cpu"
    print("---------- GroundingDino runs on", device, "----------")
    detector_id = "IDEA-Research/grounding-dino-base"
    object_detector = pipeline(model=detector_id, task="zero-shot-object-detection", device=device)
    return object_detector

def detect(
    object_detector,
    image: Image.Image,
    labels: List[str],
    threshold: float = 0.3
) -> List[Dict[str, Any]]:
    """
    Use Grounding DINO to detect a set of labels in an image in a zero-shot fashion.
    The GroundingDino detector. All important settings can also be changed right from the main pipeline (mainly threshold and labels).
    """

    labels = [label if label.endswith(".") else label+"." for label in labels]

    results = object_detector(image,  candidate_labels=labels, threshold=threshold)
    results = [DetectionResult.from_dict(result) for result in results]

    return results

def BioClip_inference(classifier, crops_for_BioClip, rank_string, certainty_threshold = 0.25):
    '''
    The function that facilitates the BioClip inference. BioClip is used to classify the cropped images taxonomically. 
    
    The cerainty_threshold specifies what the minimum difference in score between the first and the second prediction needs to be for this rank to be taken
    as the result for this picture. A higher threshold means that the first prediction is much more certain than the second one, thus standing for a 
    more trustworthy prediction. certainty_threshold only takes effect if rank_string = None
    '''

    ranks = ["species", "genus", "family", "order", "class", "phylum", "kingdom"]
    no_insect = 0
    free_rank = False
    
    if rank_string == "kingdom":
        rank = Rank.KINGDOM
    elif rank_string == "phylum":
        rank = Rank.PHYLUM
    elif rank_string == "class":
        rank = Rank.CLASS
    elif rank_string == "order":
        rank = Rank.ORDER
    elif rank_string == "family":
        rank = Rank.FAMILY
    elif rank_string == "genus":
        rank = Rank.GENUS
    elif rank_string == "species":
        rank = Rank.SPECIES
    elif rank_string is None:
        free_rank = True
    else:
        raise ValueError("Rank variable needs to be set to either kingdom, phylum, class, order, family, genus, species or None")

    if not free_rank:
        BioClip_predictions = classifier.predict(crops_for_BioClip, rank)
        print(f"Classified all images up to {rank_string}-level")
        return BioClip_predictions
    else:
        rank_tracker = OrderedDict((rank, 0) for rank in ["species", "genus", "family", "order", "class", "phylum", "kingdom"])
        predictions = []
        for crop in crops_for_BioClip:
            if os.path.isdir(crop):
                print("Path does not point to a file..")
                continue
            for rank_str in ranks:
                rank = getattr(Rank, rank_str.upper())
                BioClip_predictions = classifier.predict(crop, rank)
                #print(BioClip_predictions)
                score_diff = BioClip_predictions[0]['score'] - BioClip_predictions[1]['score']
                if score_diff >= certainty_threshold:
                    predictions.extend(BioClip_predictions)
                    rank_tracker[rank_str] += 1
                    #print(rank, score_diff)
                    break
                elif rank_str == "kingdom" and score_diff < certainty_threshold:
                    empty_entry = {'file_name': crop,
                                 'kingdom': '', 'phylum': '', 'class': '', 'order': '',
                                 'family': '', 'genus': '', 'species_epithet': '', 'species': '',
                                 'common_name': '', 'score': ''
                                }
                    predictions.append(empty_entry)
                    no_insect += 1
                    break
        print("Number of crops classified to the taxonomic ranks:")
        for rank, count in rank_tracker.items():
            if count > 0:
                print(f"{rank}: {count}")
        if no_insect > 0:
            print(f"no insect: {no_insect}")
        return predictions

def image_to_dark_checker(image_path, image_folder, image):
    '''
    Takes the path of an input image and checks the brightness of the image. If the brightness is below 150, it will return True (meaning image is to dark).
    If the brightness is above that value it will return False.

    This function is used in the main pipeline to avoid analyzing lots of pictures that are essentially black. 
    
    The pictures that are deemed to dark are saved in the 'dark_frames' subfolder for traceback.
    '''
    img = Image.open(image_path).convert('L')  # convert image to grayscale
    numpy_image = np.array(img)  # convert image to numpy array
    brightness = np.mean(numpy_image)  # calculate mean of the array
    if brightness > 150:
        return False
    else:
        dark_frames_folder = os.path.join(image_folder, "dark_frames")
        os.makedirs(dark_frames_folder, exist_ok=True)
        dark_frame_save_path = os.path.join(dark_frames_folder, image)
        img.save(dark_frame_save_path)
        return True

def annotate_classifications(classification_results_csv, body_length_csv, cropped_images_folder, image_folder, annotation_algorithm, output_folder = "cropped_and_annotated", pixel_scale = 2):
    '''
    This rather chunky function annotates the cropped images with:
    - The result from the classification (highest taxonomic name) and classsification score
    - The length of the insect in either mm or px. If SLEAP was only able to measure a part of the insect, this will also be stated (e.g head only)

    For a quick overview, the annotation also includes a border around the image. If the border is white, the classification score is high, 
    if it is a deep and saturated pink, the classification score was rather low.

    Saves the images in the 'cropped_and_annotated' folder. 

    If multiple algorithms were selected for classification, the algorithm that is used for the annotation can be selected in the 
    main pipeline (annotation algorithm).
    '''
    if classification_results_csv is not None:
        for index, row in classification_results_csv.iterrows():
            # Get the filename, class name, and detection probability
            no_class = False
            if annotation_algorithm == "BioClip":
                file_path = row['file_name']
                filename = os.path.basename(file_path)
                #class_index = classification_results_csv.columns.get_loc('score') - 1
                #class_name = row.iloc[class_index]
                highest_rank = row['highest_classification_rank']
                if not pd.isna(highest_rank):
                    class_name = row[highest_rank]
                    detection_probability = row['score']
                    difference_to_second_guess = row['score_diff']
                else:
                    class_name = ':('
                    detection_probability = 0
                    no_class = True
            elif annotation_algorithm == "ApolloNet":
                filename = row['Filename']
                class_name = row['Best_Class']
                detection_probability = row['Best_Score']
            elif annotation_algorithm == "InsectDetect":
                filename = row['img_name']
                class_name = row['top1']
                detection_probability = row['top1_prob']
                
            lengths = pd.read_csv(body_length_csv)
            insect_length_total = lengths.loc[index, 'Insect_length_total']
            length_head = lengths.loc[index, 'Head_length']
            length_abdomen = lengths.loc[index, 'Abdomen_length']
            head = False
            abdomen = False
            if not np.isnan(insect_length_total):
                annotation_length = insect_length_total
                not_entire_length = False
            elif not np.isnan(length_head):
                annotation_length = length_head
                not_entire_length = True
                head = True
            elif not np.isnan(length_abdomen):
                annotation_length = length_abdomen
                not_entire_length = True
                abdomen = True
            else:
                not_entire_length = True
                no_length = True

            # Annotates px values if the pixel_scale was set to 10 in the main pipeline (is later transformed by dividing by 10, so here we ask for '1')
            if pixel_scale == 1:
                if not not_entire_length:
                    text = f"{class_name} {round(detection_probability, 2)}\n{annotation_length}px"
                elif head:
                    text = f"{class_name} {round(detection_probability, 2)}\n{annotation_length}px only Head"
                    head = False
                elif abdomen:
                    text = f"{class_name} {round(detection_probability, 2)}\n{annotation_length}px only Abdomen"
                    abdomen = False
                elif no_length:
                    text = f"{class_name} {round(detection_probability, 2)}\nNo length"
                    no_length = False
            ## annotation in millimeter if pixel_scale has any other value
            else:
                if not not_entire_length:
                    text = f"{class_name} {round(detection_probability, 2)}\n{annotation_length} mm"
                elif head:
                    text = f"{class_name} {round(detection_probability, 2)}\n{annotation_length} mm only Head"
                    head = False
                elif abdomen:
                    text = f"{class_name} {round(detection_probability, 2)}\n{annotation_length} mm only Abdomen"
                    abdomen = False
                elif no_length:
                    text = f"{class_name} {round(detection_probability, 2)}\nNo length"
                    no_length = False
    
            image_path = os.path.join(cropped_images_folder, filename)
            image = Image.open(image_path)
            width, height = image.size
    
            font_size = 16
            
            try:
                font = ImageFont.truetype("arial.ttf", font_size)
            except OSError:
                font = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial.ttf", font_size)
    
            # Calculate the size of the text box and adjust font size if necessary
            draw = ImageDraw.Draw(image)
            text_bbox = draw.textbbox((0, 0), text, font=font)
            text_width = text_bbox[2] - text_bbox[0]
            text_height = text_bbox[3] - text_bbox[1]
    
            while text_width > image.width - 12:  # Adjust font size if text exceeds image width
                font_size -= 1
                try:
                    font = ImageFont.truetype("arial.ttf", font_size)
                except OSError:
                    font = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial.ttf", font_size)
                text_bbox = draw.textbbox((0, 0), text, font=font)
                text_width = text_bbox[2] - text_bbox[0]
                text_height = text_bbox[3] - text_bbox[1]
    
            # Create a new image with extra space for the text
            new_image_height = image.height + text_height + 20  # Adding some padding
            new_image = Image.new('RGB', (image.width, new_image_height), (255, 255, 255))
            new_image.paste(image, (0, 0))
    
            # Draw the text on the new image
            draw = ImageDraw.Draw(new_image)

            if annotation_algorithm == "BioClip" and not no_class:
                rectangle_coords = [(0, 0), (width - 1, height - 1)]
                draw.rectangle(rectangle_coords, 
                               outline = (255, int(255*difference_to_second_guess), 255),
                               width=3)
            
            text_position = (5, image.height + 10)  # Positioning the text below the image with some padding
            if not not_entire_length:
                draw.text(text_position, text, fill="green", font=font)
            else:
                draw.text(text_position, text, fill="purple", font=font)
    
            save_path = os.path.join(image_folder, output_folder)
            os.makedirs(save_path, exist_ok=True)
            save_path = os.path.join(save_path, filename)
    
            new_image.save(save_path)

def is_image_readable(image_path):
    '''
    Function to check wether an image is corrupted and thus not readable. This function is key in order to keep the pipeline from crashing
    everytime it encounters a corrupted image - and these do appear with DIOPSIS cameras. If the pipeline encounters a corrupted file, it
    will notice the user with print statements.
    '''
    try:
        with Image.open(image_path).convert('L') as img:
            img.verify()  # Verify the image file
        return True
    except (IOError, SyntaxError, OSError) as e:
        print(f"Image is truncated or corrupted: {image_path} - {e}")
        print("Skipping this image.")
        return False

def custom_warning_format(message, category, filename, lineno, file=None, line=None):
    '''
    Defining a custom warning format. Not sure if it is still being used though...
    '''
    return f"{message}\n"

def should_image_be_skipped(start_image, start_processing, image_arg, image, image_folder):
    '''
    Checking wether the current path really points to a file and if the file is a .jpg.
    Consequently, all images that are not jpg are skipped. Could maybe also be adapted to work with other filetypes, but no guarantee that the rest
    of the pipeline will be able to handle it.
    Also checks wether the image is readable using the function above.
    '''
    skip = False
    if not start_image == '':
        if image == start_image:
            start_processing = True
        if not start_processing:
            print("Skipping file", image, "because you specified it... Starting at", start_image)
            skip = True
            return skip, start_processing
    if os.path.isdir(image_arg):
        warnings.warn("Path does not point to a file:" + str(image_arg))
        skip = True
        return skip, start_processing
    if not image.endswith('.jpg'):
        warnings.warn("File does not seem to be a .jpg. Skipping this file: " + str(image_arg), UserWarning)
        skip = True
        return skip, start_processing
    if not is_image_readable(image_arg):
        skip = True
        return skip, start_processing
    image_to_dark = image_to_dark_checker(image_arg, image_folder, image)
    if image_to_dark:
        #print("The current image is to dark. I will skip it in the analysis but save it in the dark_frames folder for traceback...")
        skip = True
        return skip, start_processing

    return skip, start_processing

def get_local_env_path():
    '''
    Fetches the path where the environments are stored on this machine. Makes running the pipeline on differen machines a lot easier.
    '''
    envs_path = sys.prefix
    envs_path = os.path.abspath(os.path.join(envs_path, os.pardir))
    print(f"The envs directory is: {envs_path}")
    return envs_path

def extract_datetime(filename):
    '''
    Extracts the datetime part from the filename. Year, Month, Day, Hour, Minute and Second
    '''
    match = re.match(r"(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})_crop_\d+\.jpg", filename)
    if match:
        year, month, day, hour, minute, second = match.groups()
        return int(year), int(month), int(day), int(hour), int(minute), int(second)
    else:
        raise ValueError(f"Filename {filename} does not match the expected pattern")

def process_BioClip_predictions(BioClip_redictions, image_folder, rank_string, output_filename = "BioClip_classification_results"):
    '''
    Processes the results from the BioClip prediction and outputs a results csv to the result subfolder of each day.
    Filename is 'BioClip_classification_results.csv' by default. 

    WORD OF CAUTION: The 'score' value of the BioClip inference will only reflect the certainty for the taxonomic rank up to which the classification went.
                    That means that if you classified up to species level for example, and later in the analysis go up to family level, the score will
                    no longer reflect how certain BioClip is. The score always reflects the certainty that is achieved for the taxonomic rank that is
                    stated in the 'highest_classification_rank' column of the results csv. If you first classified to species, but then later decide to switch
                    your analysis to family, you need to run the inference again (ideally with the 'BioClip_only' notebook) in order to obtain the right
                    score values for that taxonomic rank. Otherwise, the 'score' value might appear either way to high or way to low.
    '''
    ranks = ["species", "genus", "family", "order", "class", "phylum", "kingdom"]
    df = pd.DataFrame(BioClip_redictions)
    df_sorted = df.sort_values(by=['file_name', 'score'], ascending=[True, False])
    columns = [col for col in df_sorted.columns if col != "score"] + ["score"]
    df_sorted = df[columns]

    results = []
    
    # Iterate over each group
    for file_path, group in df_sorted.groupby('file_name'):
        top_row = group.iloc[0].to_dict()  # Convert the first row to a dictionary
        top_score = top_row['score']
    
        file_name = os.path.basename(file_path)
        year, month, day, hour, minute, second = extract_datetime(file_name)
        
        if len(group) > 1:
            second_row = group.iloc[1].to_dict()
            second_top_score = second_row['score']
            score_diff = abs(top_score - second_top_score)
    
            if not rank_string:
                for rank in ranks:
                    if rank in top_row and not pd.isna(top_row[rank]):
                        up_to_rank = rank
                        second_classification = second_row[rank]
                        break
            else:
                up_to_rank = rank_string
                second_classification = second_row[rank_string]

        else:
            up_to_rank = ''
            second_classification = ''
            second_top_score = ''
            score_diff = ''

                    
        # Add additional variables to the dictionary
        top_row['highest_classification_rank'] = up_to_rank
        top_row['taxonomic_name_2'] = second_classification
        top_row['score_2'] = second_top_score
        top_row['score_diff'] = score_diff
        top_row['year'] = year
        top_row['month'] = month
        top_row['day'] = day
        top_row['hour'] = hour
        top_row['minute'] = minute
        top_row['second'] = second
        
        results.append(top_row)
    
    results_df = pd.DataFrame(results)
    csv_out_path = os.path.join(image_folder, "results")
    os.makedirs(csv_out_path, exist_ok=True)
    csv_out_path = os.path.join(csv_out_path, f"{output_filename}.csv")
    results_df.to_csv(csv_out_path, index=False)
    
    return results_df
    

def get_classification_results_csv(image_folder, annotation_algorithm, BioClip_filename = "BioClip_classification_results"):
    '''
    Function that fetches the results csv from the classification algorithms. Only searches for the result of the algorithm that was used for annotation.
    '''
    if annotation_algorithm == "InsectDetect":
        try:
            results_csv = pd.read_csv(os.path.join(image_folder, "results", "results", "classification_results.csv"))
        except FileNotFoundError:
            print("No InsectDetect results found.")
            results_csv = None
    elif annotation_algorithm == "ApolloNet":
        try:
            results_csv = pd.read_csv(os.path.join(image_folder, "results", "ApolloNet_classification_results.csv"))
        except FileNotFoundError:
            print("No Apollo results found.")
            results_csv = None
    elif annotation_algorithm == "BioClip":
        try:
            results_csv = pd.read_csv(os.path.join(image_folder, "results", f"{BioClip_filename}.csv"))
        except FileNotFoundError:
            print("No BioClip results found.")
            results_csv = None
    else:
        print(f"No classification results fetched. Make sure the 'annotation_algorithm' variable is set correctly. It should be one of 'InsectDetect', 'ApolloNet', or 'BioClip'. Variable is currently {annotation_algorithm}")
    return results_csv

def merge_result_csvs(image_folder):
    '''
    A function that takes the results csvs from all the algorithms and merges them into one nice results csv called 'all_results.csv'.
    Function is only used to merge the results from within one day.
    '''
    results_folder = os.path.join(image_folder, "results")
    csv_out_path = os.path.join(results_folder, "all_results.csv")
    
    InsectDetect_results = os.path.join(results_folder, "results", "classification_results.csv")
    if os.path.exists(InsectDetect_results):
        InsectDetect = True
        InsectDetect_results = pd.read_csv(InsectDetect_results)
        InsectDetect_best_class = InsectDetect_results['top1']
        InsectDetect_best_prob = InsectDetect_results['top1_prob']
    else:
        InsectDetect = False

    ApolloNet_results = os.path.join(results_folder, "ApolloNet_classification_results.csv")
    if os.path.exists(ApolloNet_results):
        ApolloNet = True
        ApolloNet_results = pd.read_csv(ApolloNet_results)
        ApolloNet_best_class = ApolloNet_results['Best_Class']
        ApolloNet_best_score = ApolloNet_results['Best_Score']
    else:
        ApolloNet = False

    BioClip_results = os.path.join(results_folder, "BioClip_classification_results.csv")
    if os.path.exists(BioClip_results):
        BioClip = True
        BioClip_results = pd.read_csv(BioClip_results)
        paths = BioClip_results['file_name']
        BioClip_data = BioClip_results.loc[:, 'kingdom':'score_diff']
    else:
        BioClip = False

    length_results = os.path.join(results_folder, "body_length_results.csv")
    if os.path.exists(length_results):
        length = True
        length_results = pd.read_csv(length_results)
    else:
        length = False

    if length:
        results = length_results
        if BioClip:
            results = pd.concat([results, BioClip_data], axis = 1)
        if ApolloNet:
            results['ApolloNet_best_class'] = ApolloNet_best_class
            results['ApolloNet_best_score'] = ApolloNet_best_score
        if InsectDetect:
            results['InsectDetect_best_class'] = InsectDetect_best_class
            results['InsectDetect_best_prob'] = InsectDetect_best_prob
        if BioClip:
            results['path'] = paths

        results.to_csv(csv_out_path, index=False)
    
    else:
        raise ValueError("Did not find any length results. Need those to create a master results csv file")
    
def are_crops_available(image_folder):
    '''
    Checks wether there are any crops in the 'cropped' folder of the current day. If no crops can be found it means that no insects were detected
    on the images of that particular day. This means that some steps can be skipped in the main pipeline (all classification and length measurement).
    '''
    crops_available = True
    crops_folder = os.path.join(image_folder, "cropped")
    crops_filepaths = glob.glob(os.path.join(crops_folder, '*'))
    crops_filepaths = sorted([item for item in crops_filepaths if os.path.isfile(item) and not item.startswith('.')])
    if len(crops_filepaths) == 0:
        crops_available = False
        print("No crops available for further analysis")
    return crops_available, crops_folder, crops_filepaths

def ask_for_pixel_scale():
    '''
    This function asks for a user input to define the scale of the pixels of the currently analyzed cameras.

    The pixel scale of the camera and the images should be measured in advance and then input as number of pixel per cm.

    This value is the divided by 10 to calculate insect lengths in mm rather than in cm.
    '''
    pixel_scale = int(input("Give the pixel scale of the current camera (in pixel per cm):"))
    pixel_scale /= 10
    return pixel_scale

def merge_all_results(DIOPSIS_folder, results_filename = "all_results", out_filename = "all_analysis_results"):
    '''
    A function that merges all the single 'all_results.csv' (created with the 'merge_result_csvs' function above) from each day where the camera
    produced pictures into one results file named 'all_analysis_results.csv' by default. 

    This file will appear at the topmost level of the camera file system (the place where also the 'photos' folder lives).

    This is the file that is probably the most relevant for all further analysis.
    '''
    image_folders = [folder for folder in os.listdir(os.path.join(DIOPSIS_folder, "photos")) if not folder.startswith('.')]
    image_folders = sorted(image_folders)
    all_results = pd.DataFrame()
    first = True
    for folder in image_folders:
        csv_path = os.path.join(DIOPSIS_folder, "photos", folder, "results", f"{results_filename}.csv") 
        try:
            if first:
                csv = pd.read_csv(csv_path)
            else:
                csv = pd.read_csv(csv_path, header=0)
            first = False
        except FileNotFoundError:
            print(f"No results for day {folder}")
            continue
        all_results = pd.concat([all_results, csv], ignore_index=True)
    csv_out_path = os.path.join(DIOPSIS_folder, f"{out_filename}.csv")
    all_results.to_csv(csv_out_path, index=False)
