## Deprecated version of the key functions for the main pipeline. Some of these functions were used in earlier versions of the pipeline (for example the rudimentary insect tracker). Currently 'helper_funktionen_clean' is used.

## This file also contains very old functions that were never used and came from the InsectSAM project I believe.

## This file is not maintained!


from transformers import AutoModelForMaskGeneration, AutoProcessor, pipeline
from PIL import Image, ImageDraw, ImageFont, PngImagePlugin
from typing import Any, List, Dict, Optional, Union, Tuple
from collections import OrderedDict, defaultdict
from FunktionenZumImportieren.classes import *
from bioclip import TreeOfLifeClassifier, Rank
from dataclasses import dataclass
import plotly.graph_objects as go
import matplotlib.pyplot as plt
import plotly.express as px
from enum import Enum
import pandas as pd
import numpy as np
import requests
import warnings
import random
import torch
import json
import sys
import cv2
import re
import os


def annotate(image: Union[Image.Image, np.ndarray], detection_results: List[DetectionResult]) -> np.ndarray:
    # Convert PIL Image to OpenCV format
    image_cv2 = np.array(image) if isinstance(image, Image.Image) else image
    image_cv2 = cv2.cvtColor(image_cv2, cv2.COLOR_RGB2BGR)

    # Iterate over detections and add bounding boxes and masks
    for detection in detection_results:
        label = detection.label
        score = detection.score
        box = detection.box
        mask = detection.mask

        # Sample a random color for each detection
        color = np.random.randint(0, 256, size=3)

        # Draw bounding box
        cv2.rectangle(image_cv2, (box.xmin, box.ymin), (box.xmax, box.ymax), color.tolist(), 2)
        cv2.putText(image_cv2, f'{label}: {score:.2f}', (box.xmin, box.ymin - 10), cv2.FONT_HERSHEY_SIMPLEX, 0.5, color.tolist(), 2)

        # If mask is available, apply it
        if mask is not None:
            # Convert mask to uint8
            mask_uint8 = (mask * 255).astype(np.uint8)
            contours, _ = cv2.findContours(mask_uint8, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
            cv2.drawContours(image_cv2, contours, -1, color.tolist(), 2)

    return cv2.cvtColor(image_cv2, cv2.COLOR_BGR2RGB)

def plot_detections(
    image: Union[Image.Image, np.ndarray],
    detections: List[DetectionResult],
    save_name: Optional[str] = None
) -> None:
    annotated_image = annotate(image, detections)
    plt.imshow(annotated_image)
    plt.axis('off')
    if save_name:
        plt.savefig(save_name, bbox_inches='tight')
    plt.show()

def random_named_css_colors(num_colors: int) -> List[str]:
    """
    Returns a list of randomly selected named CSS colors.

    Args:
    - num_colors (int): Number of random colors to generate.

    Returns:
    - list: List of randomly selected named CSS colors.
    """
    # List of named CSS colors
    named_css_colors = [
        'aliceblue', 'antiquewhite', 'aqua', 'aquamarine', 'azure', 'beige', 'bisque', 'black', 'blanchedalmond',
        'blue', 'blueviolet', 'brown', 'burlywood', 'cadetblue', 'chartreuse', 'chocolate', 'coral', 'cornflowerblue',
        'cornsilk', 'crimson', 'cyan', 'darkblue', 'darkcyan', 'darkgoldenrod', 'darkgray', 'darkgreen', 'darkgrey',
        'darkkhaki', 'darkmagenta', 'darkolivegreen', 'darkorange', 'darkorchid', 'darkred', 'darksalmon', 'darkseagreen',
        'darkslateblue', 'darkslategray', 'darkslategrey', 'darkturquoise', 'darkviolet', 'deeppink', 'deepskyblue',
        'dimgray', 'dimgrey', 'dodgerblue', 'firebrick', 'floralwhite', 'forestgreen', 'fuchsia', 'gainsboro', 'ghostwhite',
        'gold', 'goldenrod', 'gray', 'green', 'greenyellow', 'grey', 'honeydew', 'hotpink', 'indianred', 'indigo', 'ivory',
        'khaki', 'lavender', 'lavenderblush', 'lawngreen', 'lemonchiffon', 'lightblue', 'lightcoral', 'lightcyan', 'lightgoldenrodyellow',
        'lightgray', 'lightgreen', 'lightgrey', 'lightpink', 'lightsalmon', 'lightseagreen', 'lightskyblue', 'lightslategray',
        'lightslategrey', 'lightsteelblue', 'lightyellow', 'lime', 'limegreen', 'linen', 'magenta', 'maroon', 'mediumaquamarine',
        'mediumblue', 'mediumorchid', 'mediumpurple', 'mediumseagreen', 'mediumslateblue', 'mediumspringgreen', 'mediumturquoise',
        'mediumvioletred', 'midnightblue', 'mintcream', 'mistyrose', 'moccasin', 'navajowhite', 'navy', 'oldlace', 'olive',
        'olivedrab', 'orange', 'orangered', 'orchid', 'palegoldenrod', 'palegreen', 'paleturquoise', 'palevioletred', 'papayawhip',
        'peachpuff', 'peru', 'pink', 'plum', 'powderblue', 'purple', 'rebeccapurple', 'red', 'rosybrown', 'royalblue', 'saddlebrown',
        'salmon', 'sandybrown', 'seagreen', 'seashell', 'sienna', 'silver', 'skyblue', 'slateblue', 'slategray', 'slategrey',
        'snow', 'springgreen', 'steelblue', 'tan', 'teal', 'thistle', 'tomato', 'turquoise', 'violet', 'wheat', 'white',
        'whitesmoke', 'yellow', 'yellowgreen'
    ]

    # Sample random named CSS colors
    return random.sample(named_css_colors, min(num_colors, len(named_css_colors)))

def plot_detections_plotly(
    image: np.ndarray,
    detections: List[DetectionResult],
    class_colors: Optional[Dict[str, str]] = None
) -> None:
    # If class_colors is not provided, generate random colors for each class
    if class_colors is None:
        num_detections = len(detections)
        colors = random_named_css_colors(num_detections)
        class_colors = {}
        for i in range(num_detections):
            class_colors[i] = colors[i]


    fig = px.imshow(image)

    # Add bounding boxes
    shapes = []
    annotations = []
    for idx, detection in enumerate(detections):
        label = detection.label
        box = detection.box
        score = detection.score
        mask = detection.mask

        polygon = mask_to_polygon(mask)

        fig.add_trace(go.Scatter(
            x=[point[0] for point in polygon] + [polygon[0][0]],
            y=[point[1] for point in polygon] + [polygon[0][1]],
            mode='lines',
            line=dict(color=class_colors[idx], width=2),
            fill='toself',
            name=f"{label}: {score:.2f}"
        ))

        xmin, ymin, xmax, ymax = box.xyxy
        shape = [
            dict(
                type="rect",
                xref="x", yref="y",
                x0=xmin, y0=ymin,
                x1=xmax, y1=ymax,
                line=dict(color=class_colors[idx])
            )
        ]
        annotation = [
            dict(
                x=(xmin+xmax) // 2, y=(ymin+ymax) // 2,
                xref="x", yref="y",
                text=f"{label}: {score:.2f}",
            )
        ]

        shapes.append(shape)
        annotations.append(annotation)

    # Update layout
    button_shapes = [dict(label="None",method="relayout",args=["shapes", []])]
    button_shapes = button_shapes + [
        dict(label=f"Detection {idx+1}",method="relayout",args=["shapes", shape]) for idx, shape in enumerate(shapes)
    ]
    button_shapes = button_shapes + [dict(label="All", method="relayout", args=["shapes", sum(shapes, [])])]

    fig.update_layout(
        xaxis=dict(visible=False),
        yaxis=dict(visible=False),
        # margin=dict(l=0, r=0, t=0, b=0),
        showlegend=True,
        updatemenus=[
            dict(
                type="buttons",
                direction="up",
                buttons=button_shapes
            )
        ],
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.02,
            xanchor="right",
            x=1
        )
    )

    # Show plot
    fig.show()

def mask_to_polygon(mask: np.ndarray) -> List[List[int]]:
    # Find contours in the binary mask
    contours, _ = cv2.findContours(mask.astype(np.uint8), cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)

    # Find the contour with the largest area
    largest_contour = max(contours, key=cv2.contourArea)

    # Extract the vertices of the contour
    polygon = largest_contour.reshape(-1, 2).tolist()

    return polygon

def polygon_to_mask(polygon: List[Tuple[int, int]], image_shape: Tuple[int, int]) -> np.ndarray:
    """
    Convert a polygon to a segmentation mask.

    Args:
    - polygon (list): List of (x, y) coordinates representing the vertices of the polygon.
    - image_shape (tuple): Shape of the image (height, width) for the mask.

    Returns:
    - np.ndarray: Segmentation mask with the polygon filled.
    """
    # Create an empty mask
    mask = np.zeros(image_shape, dtype=np.uint8)

    # Convert polygon to an array of points
    pts = np.array(polygon, dtype=np.int32)

    # Fill the polygon with white color (255)
    cv2.fillPoly(mask, [pts], color=(255,))

    return mask

def load_image(image_str: str) -> Image.Image:
    if image_str.startswith("http"):
        image = Image.open(requests.get(image_str, stream=True).raw).convert("RGB")
    else:
        image = Image.open(image_str).convert("RGB")

    return image

def get_boxes(results: DetectionResult) -> List[List[List[float]]]:
    boxes = []
    for result in results:
        xyxy = result.box.xyxy
        boxes.append(xyxy)

    return [boxes]

def refine_masks(masks: torch.BoolTensor, polygon_refinement: bool = False) -> List[np.ndarray]:
    masks = masks.cpu().float()
    masks = masks.permute(0, 2, 3, 1)
    masks = masks.mean(axis=-1)
    masks = (masks > 0).int()
    masks = masks.numpy().astype(np.uint8)
    masks = list(masks)

    if polygon_refinement:
        for idx, mask in enumerate(masks):
            shape = mask.shape
            polygon = mask_to_polygon(mask)
            mask = polygon_to_mask(polygon, shape)
            masks[idx] = mask

    return masks

def extract_insect_masks(image: np.ndarray, detections: List[DetectionResult]) -> List[np.ndarray]:
    insect_masks = []
    for detection in detections:
        mask = detection.mask
        if mask is not None:
            insect_masks.append(mask)
    return insect_masks

def put_masks_on_yellow_background(image_shape: Tuple[int, int], masks: List[np.ndarray]) -> np.ndarray:
    yellow_background = np.full((image_shape[0], image_shape[1], 3), (0, 255, 255), dtype=np.uint8)
    for mask in masks:
        # Create a mask with 3 channels (RGB) for compatibility with the background image
        mask_rgb = cv2.cvtColor(mask, cv2.COLOR_GRAY2RGB)
        # Apply the mask to each channel of the yellow background using bitwise_or
        for c in range(3):
            yellow_background[:,:,c] = cv2.bitwise_or(yellow_background[:,:,c], mask_rgb[:,:,c])
    return yellow_background

def mask_to_min_max(mask):
    """Convert mask to min and max coordinates of the bounding box"""
    y, x = np.where(mask)
    xmin, xmax = x.min(), x.max()
    ymin, ymax = y.min(), y.max()
    return xmin, ymin, xmax, ymax

def extract_and_paste_insect(original_image, detection, background):
    """
    Extracts the insect from the original image using the mask
    and pastes it on the provided background.
    """
    mask = detection.mask
    xmin, ymin, xmax, ymax = mask_to_min_max(mask)

    # Crop insect from original image using the mask
    insect_crop = original_image[ymin:ymax, xmin:xmax]
    mask_crop = mask[ymin:ymax, xmin:xmax]

    # Extract insect using the mask
    insect = cv2.bitwise_and(insect_crop, insect_crop, mask=mask_crop)

    # Get mask coordinates on the background
    x_offset, y_offset = detection.box.xmin, detection.box.ymin
    x_end, y_end = x_offset + insect.shape[1], y_offset + insect.shape[0]

    # Create an inverse mask for background
    inverse_mask = cv2.bitwise_not(mask_crop)

    # Prepare the area on the background to accept the insect
    bg_region = background[y_offset:y_end, x_offset:x_end]
    bg_ready = cv2.bitwise_and(bg_region, bg_region, mask=inverse_mask)

    # Combine the insect with the prepared area
    combined = cv2.add(insect, bg_ready)

    # Paste the insect onto the background
    background[y_offset:y_end, x_offset:x_end] = combined

def create_yellow_background_with_insects(image, detections):
    """
    Creates a yellow background and pastes all detected insects on it.
    """
    # Create a plain yellow background
    yellow_background = np.full_like(image, (0, 255, 255), dtype=np.uint8)

    # Extract and paste each insect on the background
    for detection in detections:
        if detection.mask is not None:
            extract_and_paste_insect(image, detection, yellow_background)

    return yellow_background

def draw_classification_boxes(image_with_insects, detections):
    """
    Draws the classification bounding boxes and labels on the image with insects.
    """
    for detection in detections:
        label = detection.label
        score = detection.score
        box = detection.box

        # Sample a random color for each detection
        color = np.random.randint(0, 256, size=3).tolist()
        if os.path.isfile(image_with_insects):
            image_with_insects = cv2.imread(image_with_insects)
        # Draw bounding box
        cv2.rectangle(image_with_insects, (box.xmin, box.ymin), (box.xmax, box.ymax), color, 2)

        # Calculate the width and height of the text box
        (text_width, text_height), baseline = cv2.getTextSize(f"{label}: {score:.2f}", cv2.FONT_HERSHEY_SIMPLEX, 0.5, 2)

        # Draw the text background rectangle
        cv2.rectangle(
            image_with_insects,
            (box.xmin, box.ymin - text_height - baseline),
            (box.xmin + text_width, box.ymin),
            color,
            thickness=cv2.FILLED
        )

        # Put the text (label: score) on the image
        cv2.putText(
            image_with_insects,
            f"{label}: {score:.2f}",
            (box.xmin, box.ymin - baseline),
            cv2.FONT_HERSHEY_SIMPLEX,
            0.5,
            (255, 255, 255),
            2
        )

    return image_with_insects

def load_grounding_dino_model():
    #device = "cuda" if torch.cuda.is_available() else "cpu"
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
    """

    labels = [label if label.endswith(".") else label+"." for label in labels]

    results = object_detector(image,  candidate_labels=labels, threshold=threshold)
    results = [DetectionResult.from_dict(result) for result in results]

    return results

def BioClip_inference(crops_for_BioClip, rank_string, certainty_threshold = 0.25):
    '''
    The cerainty_threshold specifies what the minimum difference in score between the first and the second prediction needs to be for this rank to be taken
    as the result for this picture. A higher threshold means that the first prediction is much more certain than the second one, thus standing for a 
    more trustworthy prediction. certainty_threshold only takes effect if rank_string = None
    '''
    classifier = TreeOfLifeClassifier()
    ranks = ["species", "genus", "family", "order", "class", "phylum", "kingdom"]
    
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
        raise ValueError("Rank variable needs to be set to either kingdom, phylum, class, order, family, genus or species")

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
                score_diff = BioClip_predictions[0]['score'] - BioClip_predictions[1]['score']
                if score_diff >= certainty_threshold:
                    predictions.extend(BioClip_predictions)
                    rank_tracker[rank_str] += 1
                    #print(rank, score_diff)
                    break
                elif rank == "kingdom" and score_diff < certainty_threshold:
                    predictions.extend(BioClip_predictions)
                    rank_tracker[rank_str] += 1
                    break
        print("Number of crops classified to the taxonomic ranks:")
        for rank, count in rank_tracker.items():
            if count > 0:
                print(f"{rank}: {count}")
        return predictions
            

def crop_image(image_path, detections, save_path, buffer, image_format, detection_nr):
    #print("Saving images for", len(detections), "detections. Buffer = ", buffer)
    pot_faulty = False
    if image_format == "web":
        img = Image.open(requests.get(image_url, stream=True).raw)
    else:
        img = Image.open(image_path)
    width, height = img.size
    to_big_of_a_crop_threshold = 200
    to_big_of_a_crop_threshold_x = width * 0.5
    to_big_of_a_crop_threshold_y = height * 0.5
    detections_temp = detections
    for detection in detections_temp:
        # Open the image
        box = detection.box
        # Checken, ob der buffer die Grenzen von den Bilddimensionen sprengt. Falls ja: nur bis zum Bildrand croppen
        crop_xmin = box.xmin - buffer if box.xmin - buffer >= 0 else 0
        crop_ymin = box.ymin - buffer if box.ymin - buffer >= 0 else 0
        crop_xmax = box.xmax + buffer if box.xmax + buffer <= width else width
        crop_ymax = box.ymax + buffer if box.ymax + buffer <= height else height

        crop_width_check = abs(width - (crop_xmax - crop_xmin)) > to_big_of_a_crop_threshold_x
        crop_height_check = abs(height - (crop_ymax - crop_ymin)) > to_big_of_a_crop_threshold_y

        if crop_xmin != box.xmin - buffer:
            print(f"Crop {detection_nr} exceeds image on the LEFT side. Only cropping to the edge of the image. Insect might be cut off...")
        if crop_xmax != box.xmax + buffer:
            print(f"Crop {detection_nr} exceeds image on the RIGHT side. Only cropping to the edge of the image. Insect might be cut off...")
        if crop_ymin != box.ymin - buffer:
            print(f"Crop {detection_nr} exceeds image on the TOP. Only cropping to the edge of the image. Insect might be cut off...")
        if crop_ymax != box.ymax + buffer:
            print(f"Crop {detection_nr} exceeds image on the BOTTOM. Only cropping to the edge of the image. Insect might be cut off...")

        cropped_img = img.crop((crop_xmin, crop_ymin, crop_xmax, crop_ymax))
        dir_name, file_name = os.path.split(image_path)
        base_name, ext = os.path.splitext(file_name)
        
        if crop_width_check and crop_height_check:
            # Crop the image
            if save_path == "":
                # Add "_cropped" and an ascending number to the filename
                # out_path = os.path.join(dir_name, f"{base_name}_cropped_{temp}_rgb{ext}")
                out_path = os.path.join(dir_name, f"{base_name}_cropped_{detection_nr}{ext}")
            else:
                # out_path = os.path.join(save_path, f"{base_name}_cropped_{temp}_rgb{ext}")
                out_path = os.path.join(save_path, f"{base_name}_cropped_{detection_nr}{ext}")

            print("Detection", detection_nr, "was saved :)")
        else:
            potentionally_faulty = os.path.join(save_path, "potentionally_faulty")
            os.makedirs(potentionally_faulty, exist_ok=True)
            out_path = os.path.join(potentionally_faulty, f"{base_name}_cropped_{detection_nr}{ext}")
            print("Crop to big. Width:", crop_xmax - crop_xmin, "--- Height:", crop_ymax - crop_ymin, "Detections seems to be faulty. Saving in potentionally_faulty folder..")
            pot_faulty = True
        
        # Save the cropped image
        cropped_img.save(out_path)
        return pot_faulty, out_path

def are_box_centers_similar_x(curr_cen, prev_cen, threshold):
    """
    Überprüft, ob zwei Boxen ähnlich sind, basierend auf einem gegebenen Schwellenwert.

    Parameter:
    box1, box2 (BoundingBox): Die Boxen zum Vergleich.
    threshold (float): Der Schwellenwert für die Ähnlichkeit.

    Rückgabe:
    bool: True, wenn die Boxen ähnlich sind, sonst False.
    """
    if abs(curr_cen['x'] - prev_cen['x']) > threshold:
        #print("Distance x:", curr_cen['x'] - prev_cen['x'])
        return False
    else:
        #print("Centers in x dimension are to similar. Distance =", abs(box1_center - box2_center))
        #print("Cropped detection not saved!")
        return True

def are_box_centers_similar_y(curr_cen, prev_cen, threshold):
    """
    Überprüft, ob zwei Boxen ähnlich sind, basierend auf einem gegebenen Schwellenwert.

    Parameter:
    box1, box2 (BoundingBox): Die Boxen zum Vergleich.
    threshold (float): Der Schwellenwert für die Ähnlichkeit.

    Rückgabe:
    bool: True, wenn die Boxen ähnlich sind, sonst False.
    """
    if abs(curr_cen['y'] - prev_cen['y']) > threshold:
        #print("Distance y:", curr_cen['y'] - prev_cen['y'])
        return False
    else:
        #print("Centers in y dimension are to similar. Distance =", abs(box1_center - box2_center))
        #print("Cropped detection not saved!")
        return True

def draw_detections_on_images(image_path, current_center, previous_centers, similarity_found, similarity_threshold, save_path, pot_faulty, draw_layer):
    # Open an image file
    img = Image.open(image_path)
    draw = ImageDraw.Draw(draw_layer)
    one_red = False
    red = (173, 96, 71)
    redd = (255, 0, 0)
    bgreen = (71, 173, 147)
    if not pot_faulty:
        for prev_center in previous_centers:
            distance_x = abs(current_center['x'] - prev_center['x'])
            distance_y = abs(current_center['y'] - prev_center['y'])
            if not distance_x > similarity_threshold and not distance_y > similarity_threshold:
                line_color = red
                one_red = True
            else:
                line_color = bgreen
            draw.line([(current_center['x'], current_center['y']), (prev_center['x'], prev_center['y'])], fill=line_color, width=2)
        if len(previous_centers) == 0:
            line_color = (0, 255, 0)
            draw.ellipse((current_center['x'] - 10, current_center['y'] - 10, current_center['x'] + 10, current_center['y'] + 10), fill=line_color)
        elif one_red:
            draw.ellipse((current_center['x'] - 10, current_center['y'] - 10, current_center['x'] + 10, current_center['y'] + 10), fill=redd)
        else:
            draw.ellipse((current_center['x'] - 10, current_center['y'] - 10, current_center['x'] + 10, current_center['y'] + 10), fill=line_color)
    else:
        for prev_center in previous_centers:
            line_color = (100, 100, 100)
            draw.line([(current_center['x'], current_center['y']), (prev_center['x'], prev_center['y'])], fill=line_color, width=2)
            # draw.ellipse((current_center['x'] - 10, current_center['y'] - 10, current_center['x'] + 10, current_center['y'] + 10), fill=line_color)

    img = Image.alpha_composite(img.convert('RGBA'), draw_layer)
    img = img.convert('RGB')
    img.save(save_path)
    return draw_layer

def hugging_insects_check(detections, similarity_threshold):
    for detection in detections:
        for det in detections:
            if det is detection:
                continue
            det1 = detection.center
            det2 = det.center
            distance_x = abs(det1['x'] - det2['x'])
            distance_y = abs(det1['y'] - det2['y'])
            if distance_x >= similarity_threshold and distance_y >= similarity_threshold:
                alt = similarity_threshold
                hugging_insects_check = True
                similarity_threshold = min(distance_x, distance_y) - 5
                print("Two insects are to close to each other in the current image. Original similarity_threshold of", alt, "was adapted. The new temporary value is", similarity_threshold)
                return hugging_insects_check, similarity_threshold
            else:
                return hugging_insects_check, similarity_threshold

def image_to_dark_checker(image_path, image_folder, image):
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

def annotate_classifications(classification_results_csv, body_length_csv, cropped_images_folder, image_folder, pixel_scale, annotation_algorithm):
    if classification_results_csv is not None:
        for index, row in classification_results_csv.iterrows():
            # Get the filename, class name, and detection probability
            if annotation_algorithm == "BioClip":
                file_path = row['file_name']
                filename = os.path.basename(file_path)
                #class_index = classification_results_csv.columns.get_loc('score') - 1
                #class_name = row.iloc[class_index]
                highest_rank = row['highest_classification_rank']
                class_name = row[highest_rank]
                detection_probability = row['score']
                difference_to_second_guess = row['score_diff']
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
                #print("drin total!!")
                annotation_length = insect_length_total
                not_entire_length = False
            elif not np.isnan(length_head):
                #print("drin Head!!")
                annotation_length = length_head
                not_entire_length = True
                head = True
            elif not np.isnan(length_abdomen):
                #print("drin Abdomen!!")
                annotation_length = length_abdomen
                not_entire_length = True
                abdomen = True
            else:
                not_entire_length = True
                no_length = True
    
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
    
            # Open the corresponding image file using PIL
            image_path = os.path.join(cropped_images_folder, filename)
            image = Image.open(image_path)
            width, height = image.size
    
            # Define the initial font size
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

            if annotation_algorithm == "BioClip":
                rectangle_coords = [(0, 0), (width - 1, height - 1)]
                draw.rectangle(rectangle_coords, 
                               #outline = (int(255*difference_to_second_guess), int(255*difference_to_second_guess), int(255*difference_to_second_guess)),
                               outline = (255, int(255*difference_to_second_guess), 255),
                               width=3)
            
            text_position = (5, image.height + 10)  # Positioning the text below the image with some padding
            if not not_entire_length:
                draw.text(text_position, text, fill="green", font=font)
            else:
                draw.text(text_position, text, fill="purple", font=font)
    
            save_path = os.path.join(image_folder, "cropped_and_annotated")
            os.makedirs(save_path, exist_ok=True)
            save_path = os.path.join(save_path, filename)
    
            new_image.save(save_path)

def annotate_classifications_old(with_metadata, classification_results_csv, body_length_csv, cropped_images_folder, image_folder, pixel_scale, Apollo_results):
    if classification_results_csv is None:
        only_Apollo = True
        both = False
        classification_results_csv = Apollo_results
    elif Apollo_results is None:
        only_Apollo = False
        both = False
    else:
        both = True
        only_Apollo = False
        
    for index, row in classification_results_csv.iterrows():
        # Get the filename, class name, and detection probability
        if not only_Apollo:
            filename = row['img_name']
            class_name = row['top1']
            detection_probability = row['top1_prob']
        else:
            filename = row['Filename']
            class_name = row['Best_Class']
            detection_probability = row['Best_Score']
        lengths = pd.read_csv(body_length_csv)
        insect_length_total = lengths.loc[index, 'Insect_length_total']
        length_head = lengths.loc[index, 'Head_length']
        length_abdomen = lengths.loc[index, 'Abdomen_length']
        if both:
            Apollo_class = Apollo_results.loc[index, 'Best_Class']
        #print(f"total = {insect_length_total}\nhead = {length_head}\nabdomen = {length_abdomen}")
        head = False
        abdomen = False
        if not np.isnan(insect_length_total):
            #print("drin total!!")
            annotation_length = insect_length_total
            not_entire_length = False
        elif not np.isnan(length_head):
            #print("drin Head!!")
            annotation_length = length_head
            not_entire_length = True
            head = True
        elif not np.isnan(length_abdomen):
            #print("drin Abdomen!!")
            annotation_length = length_abdomen
            not_entire_length = True
            abdomen = True
        else:
            not_entire_length = True
            no_length = True

        # Define the text
        #text = f"{class_name}\n{detection_probability}"
        ## annotation in pixel if pixel_scale == 1
        if pixel_scale == 1 and both:
            if not not_entire_length:
                text = f"{class_name}\n{Apollo_class}\n{annotation_length}px"
            elif head:
                text = f"{class_name}\n{Apollo_class}\n{annotation_length}px only Head"
                head = False
            elif abdomen:
                text = f"{class_name}\n{Apollo_class}\n{annotation_length}px only Abdomen"
                abdomen = False
            elif no_length:
                text = f"{class_name}\n{Apollo_class}\nNo length"
                no_length = False
        if pixel_scale == 1 and not both:
            if not not_entire_length:
                text = f"{class_name}\n{annotation_length}px"
            elif head:
                text = f"{class_name}\n{annotation_length}px only Head"
                head = False
            elif abdomen:
                text = f"{class_name}\n{annotation_length}px only Abdomen"
                abdomen = False
            elif no_length:
                text = f"{class_name}\nNo length"
                no_length = False
        ## annotation in millimeter if pixel_scale has any other value
        elif both:
            if not not_entire_length:
                text = f"{class_name}\n{Apollo_class}\n{annotation_length} mm"
            elif head:
                text = f"{class_name}\n{Apollo_class}\n{annotation_length} mm only Head"
                head = False
            elif abdomen:
                text = f"{class_name}\n{Apollo_class}\n{annotation_length} mm only Abdomen"
                abdomen = False
            elif no_length:
                text = f"{class_name}\n{Apollo_class}\nNo length"
                no_length = False
        else:
            if not not_entire_length:
                text = f"{class_name}\n{annotation_length} mm"
            elif head:
                text = f"{class_name}\n{annotation_length} mm only Head"
                head = False
            elif abdomen:
                text = f"{class_name}\n{annotation_length} mm only Abdomen"
                abdomen = False
            elif no_length:
                text = f"{class_name}\nNo length"
                no_length = False

        # Open the corresponding image file using PIL
        image_path = os.path.join(cropped_images_folder, filename)
        image = Image.open(image_path)

        # Define the initial font size
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
            font = ImageFont.truetype("arial.ttf", font_size)
            text_bbox = draw.textbbox((0, 0), text, font=font)
            text_width = text_bbox[2] - text_bbox[0]
            text_height = text_bbox[3] - text_bbox[1]

        # Create a new image with extra space for the text
        new_image_height = image.height + text_height + 20  # Adding some padding
        new_image = Image.new('RGB', (image.width, new_image_height), (255, 255, 255))
        new_image.paste(image, (0, 0))

        # Draw the text on the new image
        draw = ImageDraw.Draw(new_image)
        text_position = (5, image.height + 10)  # Positioning the text below the image with some padding
        if not not_entire_length:
            draw.text(text_position, text, fill="green", font=font)
        else:
            draw.text(text_position, text, fill="purple", font=font)

        # Create a metadata dictionary
        metadata = PngImagePlugin.PngInfo()

        # Add the class name and detection probability to the metadata
        metadata.add_text('detection_class', class_name)
        metadata.add_text('detection_probability', str(detection_probability))

        save_path = os.path.join(image_folder, "cropped_and_annotated")
        os.makedirs(save_path, exist_ok=True)
        save_as_png = with_metadata
        if save_as_png:
            filename_png = os.path.splitext(filename)[0] + '.png'
            save_path = os.path.join(save_path, filename_png)
        else:
            save_path = os.path.join(save_path, filename)
        # Save the image with the new metadata
        new_image.save(save_path, pnginfo=metadata)

def is_image_readable(image_path):
    try:
        with Image.open(image_path).convert('L') as img:
            img.verify()  # Verify the image file
        return True
    except (IOError, SyntaxError, OSError) as e:
        print(f"Image is truncated or corrupted: {image_path} - {e}")
        print("Skipping this image.")
        return False

def json_result_extraction(result, filename):
    json_pattern = re.compile(r'\{.*?\}')
    match = json_pattern.search(result.stdout)
    if match:
        json_str = match.group(0)
        try:
            lengths = json.loads(json_str)
        except json.JSONDecodeError as e:
            print(f"Error decoding JSON for image: {filename}, error: {e}")
            return None
    else:
        print(f"No JSON found in output for image: {filename}")
        return None
    return lengths

def custom_warning_format(message, category, filename, lineno, file=None, line=None):
    return f"{message}\n"

def should_image_be_skipped(start_image, start_processing, image_arg, image, image_folder):
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
        print("The current image is to dark. I will skip it in the analysis but save it in the dark_frames folder for traceback...")
        skip = True
        return skip, start_processing

    return skip, start_processing

def get_local_env_path():
    envs_path = sys.prefix
    envs_path = os.path.abspath(os.path.join(envs_path, os.pardir))
    print(f"The envs directory is: {envs_path}")
    return envs_path

def extract_datetime(filename):
    # Use regular expression to extract the datetime part from the filename
    match = re.match(r"(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})_crop_\d+\.jpg", filename)
    if match:
        year, month, day, hour, minute, second = match.groups()
        return int(year), int(month), int(day), int(hour), int(minute), int(second)
    else:
        raise ValueError(f"Filename {filename} does not match the expected pattern")

def process_BioClip_predictions(BioClip_redictions, image_folder, rank_string):
    ranks = ["species", "genus", "family", "order", "class", "phylum", "kingdom"]
    df = pd.DataFrame(BioClip_redictions)
    df_sorted = df.sort_values(by=['file_name', 'score'], ascending=[True, False])
    columns = [col for col in df_sorted.columns if col != "score"] + ["score"]
    df_sorted = df[columns]

    results = []
    
    # Iterate over each group
    for file_path, group in df_sorted.groupby('file_name'):
        top_row = group.iloc[0].to_dict()  # Convert the first row to a dictionary
        second_row = group.iloc[1].to_dict()
        top_score = top_row['score']
    
        file_name = os.path.basename(file_path)
        year, month, day, hour, minute, second = extract_datetime(file_name)
        
        if len(group) > 1:
            second_top_score = second_row['score']
            score_diff = abs(top_score - second_top_score)
        else:
            second_top_score = None
            score_diff = None

        if not rank_string:
            for rank in ranks:
                if not pd.isna(top_row[rank]):
                    up_to_rank = rank
                    second_classification = second_row[rank]
                    break
        else:
            up_to_rank = rank_string
            second_classification = second_row[rank_string]

                    
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
    csv_out_path = os.path.join(csv_out_path, "BioClip_classification_results.csv")
    results_df.to_csv(csv_out_path, index=False)
    
    return results_df

def get_classification_results_csv(image_folder, annotation_algorithm):
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
            results_csv = pd.read_csv(os.path.join(image_folder, "results", "BioClip_classification_results.csv"))
        except FileNotFoundError:
            print("No BioClip results found.")
            results_csv = None
    else:
        print(f"No classification results fetched. Make sure the 'annotation_algorithm' variable is set correctly. It should be one of 'InsectDetect', 'ApolloNet', or 'BioClip'. Variable is currently {annotation_algorithm}")
    return results_csv