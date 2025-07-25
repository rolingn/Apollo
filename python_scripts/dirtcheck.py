## Some code that can be used to conveniently check predictions from a classifier. Results are saved into the input directory. The code will then open each image in the folder one by one in random order and display it. 

## At each image the program expects you to press either of the following keys:
## '+': Meaning the prediction is correct
## '-': Meaning the prediction is incorrect
## 'u': For all cases that are unclear
## 'escape': To stop the program without having to go through all the images. Results will still be saved.

## It's recommended to take the cropped_and_annotated folder as input so you are able to see what the classifier predicted.

## Command line prompt is structured as follows:
## python check_util.py
## then enter or drag and drop image source folder


import os
import csv
import cv2
import random

def load_existing_responses(csv_path):
    """Load filenames and responses from the existing CSV, if it exists."""
    existing_responses = []
    if os.path.exists(csv_path):
        with open(csv_path, 'r') as csvfile:
            reader = csv.reader(csvfile)
            next(reader, None)  # Skip the header
            for row in reader:
                if row:  # Avoid empty rows
                    existing_responses.append((row[0], row[1]))
    return existing_responses

def display_images_and_collect_responses(folder_path, existing_files):
    """Display images and collect responses for files not already processed."""
    responses = []
    image_files = [f for f in os.listdir(folder_path) if f.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp', '.gif'))]
    
    # Filter out images already in the CSV
    new_files = [f for f in image_files if f not in existing_files]
    random.shuffle(new_files)  # Randomize the order of the new images

    for image_file in new_files:
        image_path = os.path.join(folder_path, image_file)
        image = cv2.imread(image_path)

        # Get screen dimensions (static or dynamically adjusted)
        screen_width = 1920  # Adjust as needed
        screen_height = 1080  # Adjust as needed
        window_name = 'Image'
        cv2.imshow(window_name, image)

        # Get image dimensions and calculate window position
        image_height, image_width = image.shape[:2]
        x_pos = (screen_width - image_width) // 2
        y_pos = (screen_height - image_height) // 2
        cv2.moveWindow(window_name, x_pos, y_pos)

        key = cv2.waitKey(0)

        if key == ord('+'):
            response = 'Correct'
        elif key == ord('-'):
            response = 'Incorrect'
        elif key == ord('u'):
            response = 'Unsure'
        elif key == 27:  # 27 is the ASCII value for the Escape key
            cv2.destroyAllWindows()
            break
        else:
            response = 'unknown response'

        responses.append((image_file, response))
        cv2.destroyAllWindows()

    return responses

def save_responses_to_csv(responses, csv_path):
    """Append new responses to the CSV file."""
    file_exists = os.path.exists(csv_path)
    with open(csv_path, 'a', newline='') as csvfile:
        writer = csv.writer(csvfile)
        if not file_exists:
            writer.writerow(['Filename', 'Response'])  # Write header if file doesn't exist
        writer.writerows(responses)

def calculate_statistics_from_csv(csv_path):
    """Calculate statistics from the entire CSV file."""
    responses = load_existing_responses(csv_path)
    total = len(responses)
    counts = {'Correct': 0, 'Incorrect': 0, 'Unsure': 0, 'unknown response': 0}

    for _, response in responses:
        if response in counts:
            counts[response] += 1

    stats = {key: (value / total) * 100 for key, value in counts.items()} if total > 0 else counts
    return counts, stats

def save_counts_to_csv(counts, output_counts_csv_path):
    """Save category counts to a separate CSV file."""
    with open(output_counts_csv_path, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Category', 'Count'])
        for category, count in counts.items():
            writer.writerow([category, count])

def main():
    # Get the folder paths from user input
    input_folder = input("Enter the source folder containing images: ").strip()
    output_folder = input_folder

    # Ensure output folder exists
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    output_csv_filename = 'user_checked_predictions.csv'
    output_counts_csv_filename = 'prediction_statistics.csv'

    output_csv_path = os.path.join(output_folder, output_csv_filename)
    output_counts_csv_path = os.path.join(output_folder, output_counts_csv_filename)

    # Load existing responses if the CSV already exists
    existing_responses = load_existing_responses(output_csv_path)
    existing_files = {filename for filename, _ in existing_responses}

    # Process new images and generate responses
    new_responses = display_images_and_collect_responses(input_folder, existing_files)
    save_responses_to_csv(new_responses, output_csv_path)

    # Calculate cumulative statistics and save to CSV
    counts, stats = calculate_statistics_from_csv(output_csv_path)
    save_counts_to_csv(counts, output_counts_csv_path)

    print("Statistics:")
    for category, percentage in stats.items():
        print(f"{category}: {percentage:.2f}%")

if __name__ == "__main__":
    main()