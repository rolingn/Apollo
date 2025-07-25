## The code that runs the custom trained model to detect faulty crops output from the GroundingDino inference. 
## Classifies the cropped images into one of the following 2 classes: "no_insect" and "insect"
## Moves all the crops that were identified as "no_insect" to the "potentially_faulty" folder of the specific day
## No reuslts csv is saved.


from tensorflow.keras.applications.resnet50 import preprocess_input, ResNet50
from tensorflow.keras.layers import Dense, GlobalAveragePooling2D, Dropout
from tensorflow.keras.preprocessing.image import ImageDataGenerator
from tensorflow.keras.callbacks import EarlyStopping, ModelCheckpoint
from tensorflow.keras.preprocessing import image
from tensorflow.keras.models import Model
import matplotlib.image as mpimg
import matplotlib.pyplot as plt
import tensorflow as tf
import numpy as np
import random
import shutil
import glob
import csv
import sys
import re
import os

tf.config.set_visible_devices([], 'GPU')


## Extracts 'Year', 'Month', 'Day', 'Hour', 'Minute' and 'Second' from the name of the image file and returns the values.
def extract_datetime(filename):
    # Use regular expression to extract the datetime part from the filename
    match = re.match(r"(\d{4})(\d{2})(\d{2})(\d{2})(\d{2})(\d{2})_crop_\d+\.jpg", filename)
    if match:
        year, month, day, hour, minute, second = match.groups()
        return int(year), int(month), int(day), int(hour), int(minute), int(second)
    else:
        raise ValueError(f"Filename {filename} does not match the expected pattern")

## Looks for the most recent model in the folder that is provided to it. This is handy if you trained multiple models and saved them all to the same folder. 
def fetch_most_recent_model(dir_to_fetch):
    models = [f for f in glob.glob(os.path.join(dir_to_fetch, '*')) if os.path.isdir(f) or f.endswith('.h5')]
    models.sort(key=lambda x: os.path.getmtime(x), reverse=True)
    most_recent_model = models[0] if models else None
    return most_recent_model

## Takes an input path and reduces it by n levels. 
## e.g: reduce_path(C:\Users\Pictures\Moths, 1) would return C:\Users\Pictures
def reduce_path(path, levels):
    for _ in range(levels):
        path = os.path.dirname(path)
    return path

## Loads an image and preprocesses it so that the algorithm can work with it.
def load_and_preprocess_image(img_path, target_size):
    img = image.load_img(img_path, target_size=target_size)
    img_array = image.img_to_array(img)
    img_array = np.expand_dims(img_array, axis=0)  # Add batch dimension
    img_array = preprocess_input(img_array)  # Use the same preprocessing function
    return img_array

## Fetches the path where all the environments are stored on the current machine.
def get_local_env_path():
    envs_path = sys.prefix
    envs_path = os.path.abspath(os.path.join(envs_path, os.pardir))
    print(f"The envs directory is: {envs_path}")
    return envs_path

## Fetches local environment path and searches for the most recent model in the environment "ApolloNet" in the subfolders "Intro-to-CV-for-Ecologists-main" and "model_weights_dirt".
## Your model should be saved in such a folder in order for the code to work. 
## If you saved your model differently, you need to adapt the second line below accordingly.
envs_path = get_local_env_path()
most_recent_model = fetch_most_recent_model(os.path.join(envs_path, "ApolloNet", "Intro-to-CV-for-Ecologists-main", "model_weights_dirt"))

## Loads and compiles the model
model = tf.keras.models.load_model(most_recent_model)
model.compile(optimizer = 'adam', loss = 'categorical_crossentropy', metrics = ['accuracy'])

## Defines the dimensions of the input image, the batch size and the seed_value. 
## The seed_value is important to guarantee that the code runs the same everytime it is executed.
img_height, img_width = (224,224)
batch_size = 128
seed_value= 321

os.environ['PYTHONHASHSEED']=str(seed_value)
random.seed(seed_value)
np.random.seed(seed_value)
tf.random.set_seed(seed_value)

## The actual function that performs the classification.
## Moves the crops that were classified as "no_insect" to the "potentially_faulty" folder.
## Also counts how many crops where detected as "no_insect" and subsequently moved to the different folder.
def predict_batch(folder):
    images = []
    count = 0
    files = sorted([f for f in os.listdir(folder) if os.path.isfile(os.path.join(folder, f)) and not f.startswith('.')])
    potentionally_faulty_folder = os.path.join(folder, "potentionally_faulty")
    os.makedirs(potentionally_faulty_folder, exist_ok=True)
    for filename in files:
        img_path = os.path.join(folder, filename)
        img_array = load_and_preprocess_image(img_path, target_size=(img_height, img_width))
        images.append(img_array)
    images = np.vstack(images)
    
    preds = model.predict(images)
    
    preds = np.array(preds)
    max_indices = np.argmax(preds, axis=1)
    result = np.where(max_indices == 1, "no_insect", "insect")
    
    for i, value in enumerate(result):
        if value == "no_insect":
            count += 1
            filename = files[i]
            current_location = os.path.join(folder, filename)
            move_destination = os.path.join(potentionally_faulty_folder, filename)
            shutil.move(current_location, move_destination)
    print(f"HERE {count} ")
    
## Also an inference function but without batch processing. Runs a bit slower.
## Not sure if its still up to date. This function is inactive in the current version of the code. 
def predict(folder):
    images = os.listdir(folder)
    images = sorted(images)
    potentionally_faulty_folder = os.path.join(folder, "potentionally_faulty")
    os.makedirs(potentionally_faulty_folder, exist_ok=True)
    class_names = ["insect", "no_insect"]
    results = []
    
    for img in images:
        if not img.endswith('.jpg'):
            continue
        # year, month, day, hour, minute, second = extract_datetime(img)
        img_path = os.path.join(folder, img)
        img_array = load_and_preprocess_image(img_path, target_size=(img_height, img_width))
        preds = model.predict(img_array)
        predicted_index = np.argmax(preds, axis=1)[0]
        pred_class = class_names[predicted_index]
        if pred_class == "no_insect":
            move_destination = os.path.join(potentionally_faulty_folder, img)
            shutil.move(img_path, move_destination)

## The wrapper that allows the code to be run from a terminal command line. Essential for the integration with the rest of the pipeline!
if __name__ == "__main__":
    import sys
    folder_path = sys.argv[1]
    predict_batch(folder = folder_path)
    print("Done predicting")
    # print(json.dumps(lengths))