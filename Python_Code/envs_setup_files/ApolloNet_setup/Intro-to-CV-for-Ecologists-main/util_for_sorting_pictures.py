#This code is useful for screening and simultaniously organizing images.

#Provide a folder from which images are taken and displayed one by one. 
#The images are then moved to one of two folders depending on whether you press 'spacebar' or 'enter'. 
#The two destination folders also need to be specified.
#Input image format needs to be either png, jpg, jpeg, bmp or gif

#To run, write prompt in terminal window and press enter. 

#Press escape to exit the program



import os
import cv2
import shutil
import random


def display_images_and_collect_responses(folder_path, spacebar_folder, enter_folder):
    # Get a list of image files in the folder
    image_files = [f for f in os.listdir(folder_path) if f.lower().endswith(('.png', '.jpg', '.jpeg', '.bmp', '.gif'))]

    # Shuffle the list to display images in random order
    random.shuffle(image_files)

    for image_file in image_files:
        image_path = os.path.join(folder_path, image_file)
        image = cv2.imread(image_path)

        if image is None:
            print(f"Could not read {image_file}, skipping...")
            continue

        cv2.imshow('Image', image)
        key = cv2.waitKey(0)

        # Handle key presses
        if key == 27:  # Escape key
            print("Exiting the program.")
            break
        elif key == ord(' '):  # Spacebar
            destination_folder = spacebar_folder
        elif key == ord('\r'):  # Enter key
            destination_folder = enter_folder
        else:
            destination_folder = None

        if destination_folder:
            destination_path = os.path.join(destination_folder, image_file)
            shutil.copy(image_path, destination_path)  # Use shutil.copy to copy files
            print(f"Copied {image_file} to {destination_folder}")

        cv2.destroyAllWindows()

    # Close any remaining OpenCV windows
    cv2.destroyAllWindows()

if __name__ == "__main__":
    source_path = input("Enter the source folder: ")
    spacebar_path = input("Enter the spacebar folder: ")
    enter_path = input("Enter the enter folder: ")
    display_images_and_collect_responses(source_path, spacebar_path, enter_path)