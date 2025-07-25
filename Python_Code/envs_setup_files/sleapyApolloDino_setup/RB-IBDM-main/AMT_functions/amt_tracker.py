'''
This script contains all the functions that enable the AMT tracker in the main pipeline.
Functions modified from https://stangeia.hobern.net/autonomous-moth-trap-image-pipeline/
'''


from PIL import Image, ImageDraw, ImageFont, PngImagePlugin
from scipy.optimize import linear_sum_assignment
from AMT_functions.colors import reportcolors
from abc import ABC, abstractmethod
import matplotlib.pyplot as plt
import math as math
import numpy as np
import requests
import shutil
import json
import time
import cv2
import os

env_path = os.environ['CONDA_PREFIX']
config_path = os.path.join(env_path, 'RB-IBDM-main', 'AMT_functions', 'AMT_config.json')
with open(config_path, 'r') as file:
    config = json.load(file)
markchanged = config['markchanged']

def plot_tracks_and_detections(image_array, new_tracks, image):
    if len(new_tracks)>0:
        image_folder = new_tracks[0]["image_folder"]
        annotation_out_folder = os.path.join(image_folder, "detection_drawings")
        os.makedirs(annotation_out_folder, exist_ok=True)
        annotation_out_path = os.path.join(annotation_out_folder, image)
        
        # Convert image array to BGR format if it's in RGB
        image_bgr = cv2.cvtColor(image_array, cv2.COLOR_RGB2BGR)
        
        for track in new_tracks:
            x_center = track['xcenter']
            y_center = track['ycenter']
            width = track['crop'].shape[1]
            height = track['crop'].shape[0]
            #print(f"Bin drin. Track: {track}")
            if 'saved' in track and track['saved'] == False:
                color = (0, 0, 255)  # Red
            elif 'sharper' in track and track["sharper"] == True:
                color = (255, 255, 255)  # White
            else:
                color = (0, 255, 0)  # Green
            
            # Draw rectangle
            top_left = (x_center - width // 2, y_center - height // 2)
            bottom_right = (x_center + width // 2, y_center + height // 2)
            cv2.rectangle(image_bgr, top_left, bottom_right, color, 2)
            
            # Add text
            cv2.putText(image_bgr, str(track['trackid']), (int(track['xcenter'] - 15), int(track['ycenter'] - height/2 - 20)), 
                        cv2.FONT_HERSHEY_SIMPLEX, 1.5, color, 2, cv2.LINE_AA)
            if 'cost' in track and track['cost'] is not None:
                cv2.putText(image_bgr, f"{track['cost']:.2f}", (int(track['xcenter'] - 50), int(track['ycenter'] + height/2 + 50)), 
                            cv2.FONT_HERSHEY_SIMPLEX, 1.5, color, 2, cv2.LINE_AA)
            if track['age'] != 0:
                cv2.putText(image_bgr, str(track['age']), (int(track['xcenter'] - width/2 - 40), int(track['ycenter'] + 20)), 
                            cv2.FONT_HERSHEY_SIMPLEX, 1.5, color, 2, cv2.LINE_AA)
        
        # Save the annotated image
        cv2.imwrite(annotation_out_path, image_bgr)

def get_binary_image(image, thresh = 20, kernel = 7, rate = 1):
    bsmog2 = cv2.createBackgroundSubtractorMOG2()
    image = bsmog2.apply(image, learningRate=rate)
    image[image > thresh] = 254
    image[image <= thresh] = 1
    image[image == 254] = 0
    image[image == 1] = 255
    kernel = np.ones((kernel, kernel), np.uint8)
    image = cv2.morphologyEx(image, cv2.MORPH_CLOSE, kernel)
    image = cv2.morphologyEx(image, cv2.MORPH_OPEN, kernel)
    binary = image.copy()
    return binary, kernel

def simple_crop(image_array, box, buffer):
    height, width = image_array.shape[:2]
    
    crop_xmin = box.xmin - buffer if box.xmin - buffer >= 0 else 0
    crop_ymin = box.ymin - buffer if box.ymin - buffer >= 0 else 0
    crop_xmax = box.xmax + buffer if box.xmax + buffer <= width else width
    crop_ymax = box.ymax + buffer if box.ymax + buffer <= height else height
    
    crop = image_array[crop_ymin:crop_ymax, crop_xmin:crop_xmax]
    return crop

def convert_bounding_boxes(detections, image, image_array, image_folder, buffer):
    blobs = []
    height, width = image_array.shape[:2]
    to_big_of_a_crop_threshold_x = width * 0.5
    to_big_of_a_crop_threshold_y = height * 0.5
    crop_folder = os.path.join(image_folder, "cropped")
    os.makedirs(crop_folder, exist_ok=True)
    for idx, det in enumerate(detections):
        pot_faulty = False
        box = det.box
        x, y, w, h = box.xmin, box.ymin, box.xmax - box.xmin, box.ymax - box.ymin
        # crop = image_array[y:y+h, x:x+w]
        crop = simple_crop(image_array, box, buffer)
        binary, kernel = get_binary_image(crop)
        mask = cv2.morphologyEx(binary, cv2.MORPH_DILATE, kernel)

        crop_width_check = w > to_big_of_a_crop_threshold_x
        crop_height_check = h > to_big_of_a_crop_threshold_y
        if crop_width_check or crop_height_check:
            pot_faulty = True
        
        center = det.center
        blob = {
            "id": idx,  # Assign a unique ID to each blob
            "x": x,
            "y": y,
            "w": w,
            "h": h,
            "xcenter": center['x'],
            "ycenter": center['y'],
            "size": w * h,
            "colors": reportcolors(crop, mask),  # Assuming color info is not available in DetectionResult
            "age": 0,
            "direction": "",  # Assuming direction info is not available in DetectionResult
            "score": det.score,  # Including the detection score if needed
            "label": det.label,  # Including the label if needed
            "filename": image,
            "changed": False,
            "blobimage": image_array,
            "crop": crop,
            "sharpness": 0,
            #"sharpest_crop_path": "",
            "image_folder": image_folder,
            "crop_folder": crop_folder,
            "cost": None,
            "weights": "",
            "trackid": None,
            "pot_faulty": pot_faulty,
            "saved": False,
            "sharper": False
        }
        blobs.append(blob)
    return blobs

def getdirection(a, b):
    ax, ay = a["xcenter"], a["ycenter"]
    bx, by = b["xcenter"], b["ycenter"]
    x = bx - ax
    y = by - ay
    if (x**2) + (y**2) > 100:
        return math.atan2(y, x) / math.pi * 180
    else:
        return ""

def find_nearest_available_trackid(start_id, vergeben):
    vergeben_set = set(vergeben)  # Convert list to set for faster lookups
    offset = 0

    while True:
        # Check positive direction
        candidate_id = start_id + offset
        if candidate_id not in vergeben_set:
            return candidate_id

        # Check negative direction
        if start_id - offset >= 0:
            candidate_id = start_id - offset
            if candidate_id not in vergeben_set:
                return candidate_id

        # Increment offset for next iteration
        offset += 1

def calculate_sharpness(image):
    # Convert the image to grayscale
    image = cv2.cvtColor(image, cv2.COLOR_RGB2BGR)
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    
    # Compute the Laplacian of the image
    laplacian = cv2.Laplacian(gray, cv2.CV_64F)
    
    # Calculate the variance of the Laplacian
    variance = laplacian.var()
    
    return variance

def save_crop(blob, track):
    if False: ###### Müsste vor dem Aktivieren noch aktualisiert werden
        if crop_xmin != box.xmin - buffer:
            print(f"Crop {detection_nr} exceeds image on the LEFT side. Only cropping to the edge of the image. Insect might be cut off...")
        if crop_xmax != box.xmax + buffer:
            print(f"Crop {detection_nr} exceeds image on the RIGHT side. Only cropping to the edge of the image. Insect might be cut off...")
        if crop_ymin != box.ymin - buffer:
            print(f"Crop {detection_nr} exceeds image on the TOP. Only cropping to the edge of the image. Insect might be cut off...")
        if crop_ymax != box.ymax + buffer:
            print(f"Crop {detection_nr} exceeds image on the BOTTOM. Only cropping to the edge of the image. Insect might be cut off...")

    cropped_img = blob["crop"]
    save_path = blob["crop_folder"]
    base_name, ext = os.path.splitext(blob["filename"])
        
    if not blob["pot_faulty"]:
        out_path = os.path.join(save_path, f"{base_name}_crop_{blob["id"]}{ext}")
    else:
        potentionally_faulty = os.path.join(save_path, "potentionally_faulty")
        os.makedirs(potentionally_faulty, exist_ok=True)
        out_path = os.path.join(potentionally_faulty, f"{base_name}_crop_{blob["id"]}{ext}")
        #print("Crop to big. Width:", blob["w"], "--- Height:", blob["h"], "Detections seems to be faulty. Saving in potentionally_faulty folder..")
        
    # Save the cropped image
    cropped_img = Image.fromarray(cropped_img)
    cropped_img.save(out_path)
    blob["sharpest_crop_path"] = out_path
    blob["saved"] = True
    return out_path


def overwrite_crop_if_sharper(blob, track, first_pass = False):
    if first_pass:
        out_path = save_crop(blob, track)
        track["sharpness"] = calculate_sharpness(blob["crop"])
    else:
        if calculate_sharpness(blob["crop"]) > track["sharpness"]:
            blob["sharpness"] = calculate_sharpness(blob["crop"]) ######## ------------------ hier war track
            if "sharpest_crop_path" in track:
                old_file_path = track["sharpest_crop_path"]
            else:
                print("No path for previous sharpest image found. Something might be off...")
                old_file_path = None
            out_path = save_crop(blob, track)
            if "match_found" in blob:
                blob["sharper"] = True
            if old_file_path is not None and os.path.exists(old_file_path):
                _ , file_name_new = os.path.split(out_path)
                _ , file_name_old = os.path.split(old_file_path)
                os.remove(old_file_path)
                #print(f"{file_name_old} has been replaced by a sharper image: {file_name_new}. trackid = {blob["trackid"]}")
            elif old_file_path is None:
                print("No old file to replace")
            elif not os.path.exists(old_file_path):
                print(f"File {track["sharpest_crop_path"]} does not exist.")
            else:
                print("Unknown error while trying to delete old, unsharper image.")
        else:
            new_sharpness = calculate_sharpness(blob["crop"])
            old_sharpness = track["sharpness"]
            blob["sharpest_crop_path"] = track["sharpest_crop_path"]
            blob["saved"] = False
            blob["sharper"] = False
            #print(f"New crop not sharper. Old: {old_sharpness:.3f} -- New: {new_sharpness:.3f}")

class Scale(ABC):
    def __init__(self):
        self.weight = 1

    @abstractmethod
    def measurecost(self, a, b):
        pass

    @abstractmethod
    def getcode(self):
        pass

    def setweight(self, conf):
        if "weight" in conf:
            self.weight = conf["weight"]

    def getweight(self):
        return self.weight


class AMTSizeScale(Scale):
    def measurecost(self, a, b):
        asize = a["size"]
        bsize = b["size"]
        if asize > bsize:
            asize, bsize = bsize, asize
        if asize == 0:
            return self.weight
        ratio = bsize / asize
        if ratio > 4:
            return self.weight
        return self.weight * (ratio - 1) / 3

    def getcode(self):
        return "S"


class AMTDistanceScale(Scale):
    def measurecost(self, a, b):
        ax, ay = a["xcenter"], a["ycenter"]
        bx, by = b["xcenter"], b["ycenter"]
        distance = math.sqrt((bx - ax) ** 2 + (by - ay) ** 2)

        if distance < 25:
            penaltydistance = 0
        elif (
            bx > a["x"]
            and bx < a["x"] + a["w"]
            and by > a["y"]
            and by < a["y"] + a["h"]
        ):
            penaltydistance = 0.01
        elif distance < 100:
            penaltydistance = 0.01
        elif distance < 250:
            penaltydistance = 0.02
        else:
            # Distance as fraction of possible distance - in range 0.0-1.0
            penaltydistance = distance / 4102

        return self.weight * penaltydistance

    def getcode(self):
        return "D"


class AMTColorScale(Scale):
    def __init__(self):
        self.colors = ["R", "G", "B", "C", "M", "Y", "W", "K"]

    def measurecost(self, a, b):
        distance = 0
        acolors = a["colors"]
        bcolors = b["colors"]
        for c in self.colors:
            if (c in acolors) != (c in bcolors):
                distance += 1
        return self.weight * distance / len(self.colors)

    def getcode(self):
        return "C"


class AMTAgeScale(Scale):
    def measurecost(self, a, b):
        if a["age"] > 5:
            return self.weight
        else:
            return self.weight * a["age"] / 5

    def getcode(self):
        return "A"


class AMTDirectionScale(Scale):
    def measurecost(self, a, b):
        if a["direction"] == "":
            return 0

        direction = getdirection(a, b)

        if direction == "":
            return 0

        difference = a["direction"] - direction
        if difference < -180:
            difference += 360
        elif difference > 180:
            difference -= 360

        return self.weight * abs(difference) / 180

    def getcode(self):
        return "B"


class AMTScaleFactory:
    def create(self, conf):
        classname = conf["class"]
        if classname == "AMTSizeScale":
            scale = AMTSizeScale()
        elif classname == "AMTDistanceScale":
            scale = AMTDistanceScale()
        elif classname == "AMTDirectionScale":
            scale = AMTDirectionScale()
        elif classname == "AMTAgeScale":
            scale = AMTAgeScale()
        elif classname == "AMTColorScale":
            scale = AMTColorScale()
        else:
            print("No Scale class " + classname + " defined")
            return None
        scale.setweight(conf)
        return scale


class AMTTracker:
    def __init__(self, conf):
        self.savedois = []
        self.conf = conf["tracker"]
        self.scales = []
        self.totalweight = 0
        factory = AMTScaleFactory()
        for s in self.conf["scales"]:
            if s["class"] is not None:
                scale = factory.create(s)
                self.scales.append(scale)
                self.totalweight += scale.getweight()
        self.maxage = self.conf["maxage"]
        self.cost_threshold = self.conf["cost_threshold"]

    def blobsmatch(self, blob1, blob2):
        # For this purpose, a match is considered good if the centroids of the
        # blobs are in the innnermost 10% of the other blob and if the sizes are
        # within 20%.

        x1 = blob1["x"]
        y1 = blob1["y"]
        w1 = blob1["w"]
        h1 = blob1["h"]
        x2 = blob2["x"]
        y2 = blob2["y"]
        w2 = blob2["w"]
        h2 = blob2["h"]

        cx1 = blob1["xcenter"]
        cy1 = blob1["ycenter"]
        cx2 = blob2["xcenter"]
        cy2 = blob2["ycenter"]

        size1 = w1 * h1
        size2 = w2 * h2

        if (size2 > size1 and size2 > size1 * 1.2) or (
            size1 > size2 and size1 > size2 * 1.2
        ):
            return False

        if (
            cx1 < x2 + (w2 * 0.4)
            or cx1 > x2 + (w2 * 0.6)
            or cy1 < y2 + (h2 * 0.4)
            or cy1 > y2 + (h2 * 0.56)
        ):
            return False

        if (
            cx2 < x1 + (w1 * 0.4)
            or cx2 > x1 + (w1 * 0.6)
            or cy2 < y1 + (h1 * 0.4)
            or cy2 > y1 + (h1 * 0.6)
        ):
            return False

        return True

    def compare(self, a, b):
        comparison = 0
        cstring = ""
        for s in self.scales:
            distance = s.measurecost(a, b)
            comparison += distance**2
            sstring = s.getcode() + ":" + str(distance)
            if len(sstring) > 7:
                sstring = sstring[0:7]
            if len(cstring) > 0:
                cstring += ";" + sstring
            else:
                cstring = sstring
        return math.sqrt(comparison / self.totalweight), cstring

    def managetracks(self, tracks, blobs, first_pass):
        newtracks = []
        unpopped_tracks = tracks.copy()
        vergeben = []
        t = 0
        self.cost_threshold = 0.33
        if not first_pass:
            while t < len(tracks):
                track = tracks[t]
                b = 0
                found = False
                while not found and b < len(blobs):
                    blob = blobs[b]
                    if self.blobsmatch(track, blob):
                        found = True
                        blob["trackid"] = track["trackid"]
                        vergeben.append(track["trackid"])
                        blob["cost"] = 0
                        blob["weights"] = "HighOverlap"
                        blob["match_found"] = True
                        overwrite_crop_if_sharper(blob, track)
                        newtracks.append(blobs[b])
                        blobs.pop(b)
                        tracks.pop(t)
                    else:
                        b += 1
                if not found:
                    # Leave this track for weighted comparisons
                    t += 1
                else:
                    # tracks is now one shorter so try the same index
                    pass
    
            if len(blobs) > 0 and len(tracks) > 0:
                max_size = len(tracks) if len(tracks) > len(blobs) else len(blobs)
                costs = np.full((max_size, max_size), 8.1)
    
                cstrings = []
                for t in range(len(tracks)):
                    cstringrow = []
                    cstrings.append(cstringrow)
                    for b in range(len(blobs)):
                        costs[t][b], cstring = self.compare(tracks[t], blobs[b])
                        cstringrow.append(cstring)
    
                if False:  # Show cost matrix
                    colnames = "Blobs"
                    sep = ": "
                    for b in blobs:
                        colnames += sep + str(b["id"])
                        sep = ", "
                    print(colnames)
                    for t in range(len(tracks)):
                        print(str(tracks[t]["trackid"]) + ": " + str(costs[t]))
                    # print(str(len(tracks)) + " / " + str(len(blobs)) + " / " + str(costs))
    
                track_ind, blob_ind = linear_sum_assignment(costs)
    
                for i in range(len(track_ind)):
                    if costs[track_ind[i]][blob_ind[i]] < self.cost_threshold:
                        track = tracks[track_ind[i]]
                        blob = blobs[blob_ind[i]]
                        blob["trackid"] = track["trackid"]
                        vergeben.append(track["trackid"])
                        blob["direction"] = getdirection(track, blob)
                        blob["age"] = 0
                        blob["delay"] = track["age"]
                        blob["cost"] = costs[track_ind[i]][blob_ind[i]]
                        blob["weights"] = cstrings[track_ind[i]][blob_ind[i]]
                        blob["match_found"] = True
                        overwrite_crop_if_sharper(blob, track)
                        newtracks.append(blob)
                        tracks[track_ind[i]] = None
                        blobs[blob_ind[i]] = None
    
            # At this point, newtracks contains all the blobs that match, tracks
            # contains other blobs from the last image, and blobs contains the
            # currently unmatched blobs.
    
            t = 0
            while t < len(tracks):
                track = tracks[t]
                if track is not None and track["age"] < 3:
                    track["age"] = track["age"] + 1
                    vergeben.append(track["trackid"])
                    track["saved"] = False
                    newtracks.append(track)
                    tracks.pop(t)
                else:
                    t += 1

        # All remaining blobs are new tracks
        for blob in blobs:
            if blob is not None:
                custom_id = find_nearest_available_trackid(blob["id"], vergeben)
                blob["trackid"] = custom_id
                for track in unpopped_tracks:
                    if track["trackid"] == blob["trackid"]:
                        matched_track = track
                        overwrite_crop_if_sharper(blob, matched_track, first_pass)
                        break
                else:
                    out_path = save_crop(blob, blob)
                    _ , new_blob_filename = os.path.split(out_path)
                    #print(f"Did not find any matching tracks. Blob seems to be new. blobid: {blob["trackid"]} -- Saved as {new_blob_filename}")
                vergeben.append(custom_id)
                blob["changed"] = True
                if not first_pass:
                    newtracks.append(blob)
        #print(f"Vergeben: {sorted(vergeben)}")
        # Remaining tracks are now dead
        if first_pass:
            newtracks.extend(tracks)
        sum_saved = 0
        sum_sharper = 0
        sum_new = 0
        id_of_saved = []
        for track in newtracks:
            if "saved" in track and track["saved"] == True:
                sum_saved += 1
                if "sharper" in track and track["sharper"] == True:
                    sum_sharper += 1
            sum_new = sum_saved - sum_sharper
        #print(f"Saved {sum_saved} of these detections. {sum_new} were new and {sum_sharper} were sharper than a previous crop.")
        #print(f"Saved {sum_saved} detections. "
        #  f"{sum_new} {'was' if sum_new == 1 else 'were'} new and "
        #  f"{sum_sharper} {'was' if sum_sharper == 1 else 'were'} sharper than a previous crop.")
        return newtracks, tracks