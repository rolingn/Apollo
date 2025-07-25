## Some code that can be used to conveniently check the predictions from the insect classifier. Type the prompt in the command line specifying the folder you want to check and the folder where the results should be saved to. The code will then open each image in the folder one by one and display it. 

## At each image the program expects you to press either of the following keys:
## '+': Meaning the prediction is correct
## '-': Meaning the prediction is incorrect
## '/': For all cases that are unclear
## 'escape': To stop the program without having to go through all the images. Results will still be saved.

## It's recommended to take the cropped_and_annotated folder as input so you are able to see what the classifier predicted.

## Command line prompt is structured as follows:
## python path\to\the\script\user_check_predictions.py "path_to_the_images" "path/you/want/to/save/the/results/csvs/to"
## Example use:
## python C:\Users\rolingni\AppData\Local\anaconda3\envs\InsectDetectSAM\RB-IBDM-main\utils\user_check_predictions.py "N:\alpenv\GOES\Projects\Diopsis Cameras\RESULTS_2024\all_crops\Insecta\Orthoptera" "C:\Users\rolingni\OneDrive - Eidg. Forschungsanstalt WSL\Bilder"


import os
import csv
import cv2
import argparse

def display_images_and_collect_responses(folder_path):
    responses = []
    image_files = [f for f in os.listdir(folder_path) if f.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp', '.gif'))]

    for image_file in image_files:
        image_path = os.path.join(folder_path, image_file)
        image = cv2.imread(image_path)
        cv2.imshow('Image', image)
        key = cv2.waitKey(0)

        if key == ord('+'):
            response = 'Correct'
        elif key == ord('-'):
            response = 'Incorrect'
        elif key == ord('/'):
            response = 'Unsure'
        elif key == 27:  # 27 is the ASCII value for the Escape key
            cv2.destroyAllWindows()
            break
        else:
            response = 'unknown response'

        responses.append((image_file, response))
        cv2.destroyAllWindows()

    return responses

def save_responses_to_csv(responses, output_csv_path):
    with open(output_csv_path, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Filename', 'Response'])
        writer.writerows(responses)

def calculate_statistics(responses):
    total = len(responses)
    counts = {'Correct': 0, 'Incorrect': 0, 'Unsure': 0, 'unknown response': 0}

    for _, response in responses:
        if response in counts:
            counts[response] += 1

    stats = {key: (value / total) * 100 for key, value in counts.items()}
    return counts, stats

def save_counts_to_csv(counts, output_counts_csv_path):
    with open(output_counts_csv_path, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Category', 'Count'])
        for category, count in counts.items():
            writer.writerow([category, count])

def main():
    parser = argparse.ArgumentParser(description='Process images and collect user responses.')
    parser.add_argument('input_folder', type=str, help='Path to the folder containing images')
    parser.add_argument('csv_output_folder', type=str, help='Path to the folder where the result csvs should be saved to')
    args = parser.parse_args()

    output_csv_filename = 'user_checked_predictions.csv'
    output_counts_csv_filename = 'prediction_statistics.csv'

    output_csv_path = os.path.join(args.csv_output_folder, output_csv_filename)
    output_counts_csv_path = os.path.join(args.csv_output_folder, output_counts_csv_filename)

    responses = display_images_and_collect_responses(args.input_folder)
    save_responses_to_csv(responses, output_csv_path)

    counts, stats = calculate_statistics(responses)
    save_counts_to_csv(counts, output_counts_csv_path)

    print("Statistics:")
    for category, percentage in stats.items():
        print(f"{category}: {percentage:.2f}%")

if __name__ == "__main__":
    main()