'''
This code is useful for screening and simultaniously organizing images.

Provide a folder from which images are taken and displayed one by one. 
The images are then moved to one of two folders depending on whether you press 'spacebar' or 'enter'. 
The two destination folders also need to be specified.
Input image format needs to be either png, jpg, jpeg, bmp or gif

Terminal prompt structure looks like this:
python path_to_script\util_for_sorting_pictures.py path_to_your_folder spacebar_folder enter_folder

Example prompt could look like this:
python ApolloNet\Intro-to-CV-for-Ecologists-main\util_for_sorting_pictures.py C:\Users\rolingni\Desktop\all_cropped C:\Users\rolingni\Desktop\training_set\no_fly C:\Users\rolingni\Desktop\training_set\fly

To run, write prompt in terminal window and press enter. 
'''



import os
import cv2
import shutil
import argparse


def display_images_and_collect_responses(folder_path, spacebar_folder, enter_folder):
    image_files = [f for f in os.listdir(folder_path) if f.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp', '.gif'))]

    for image_file in image_files:
        image_path = os.path.join(folder_path, image_file)
        image = cv2.imread(image_path)
        cv2.imshow('Image', image)
        key = cv2.waitKey(0)

        if key == ord(' '):  # Spacebar
            destination_folder = spacebar_folder
        elif key == ord('\r'):  # Enter key
            destination_folder = enter_folder
        else:
            destination_folder = None

        if destination_folder:
            destination_path = os.path.join(destination_folder, image_file)
            shutil.move(image_path, destination_path)

        cv2.destroyAllWindows()
        

def main():
    parser = argparse.ArgumentParser(description='Process images and move them based on user input.')
    parser.add_argument('input_folder', type=str, help='Path to the folder containing images')
    parser.add_argument('spacebar_folder', type=str, help='Path to the folder for images moved with spacebar')
    parser.add_argument('enter_folder', type=str, help='Path to the folder for images moved with enter key')
    args = parser.parse_args()

    os.makedirs(args.spacebar_folder, exist_ok=True)
    os.makedirs(args.enter_folder, exist_ok=True)

    display_images_and_collect_responses(args.input_folder, args.spacebar_folder, args.enter_folder)
    

if __name__ == "__main__":
    main()