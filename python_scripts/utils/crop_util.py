### Script for gathering all the cropped images from all the different folders

import os
import shutil

def copy_faulty_images(source_folder, destination_folder):
    """
    Copies images from subfolders named 'potentially_faulty' into a destination folder.
    
    Parameters:
        source_folder (str): The root folder containing the 'photos' subfolder.
        destination_folder (str): The folder where images from 'potentially_faulty' will be copied.
    """
    # Ensure the destination folder exists
    os.makedirs(destination_folder, exist_ok=True)

    # Construct the path to the 'photos' subfolder
    photos_path = os.path.join(source_folder, 'photos')
    print(photos_path)
    
    if not os.path.exists(photos_path):
        print(f"The folder 'photos' does not exist in {source_folder}.")
        return
    '''
    # Iterate through all subfolders in 'photos'
    for folder in os.listdir(photos_path):
        folder_path = os.path.join(photos_path, folder)
        #print(f"Jetzt in {folder_path}.")
        
        if os.path.isdir(folder_path):
            # Path to the 'cropped' folder
            cropped_path = os.path.join(folder_path, 'cropped')
            #print(f"Jetzt in {cropped_path}.")

            if os.path.isdir(cropped_path):
                    # Copy all images from 'potentially_faulty' to the destination folder
                for file in os.listdir(cropped_path):
                    source_file = os.path.join(cropped_path, file)
                        
                    if os.path.isfile(source_file):
                        shutil.copy(source_file, destination_folder)
                        print(f"Copied {source_file}")
    '''
    ### Gather all the raw images
    for folder in os.listdir(photos_path):
        folder_path = os.path.join(photos_path, folder)
        #print(f"Jetzt in {folder_path}.")
        
        if os.path.isdir(folder_path):
            for file in os.listdir(folder_path):
                source_file = os.path.join(folder_path, file)
                    
                if os.path.isfile(source_file):
                    shutil.copy(source_file, destination_folder)
                    print(f"Copied {source_file}")

if __name__ == "__main__":
    source = input("Enter the source folder: ")
    destination = input("Enter the destination folder: ")
    copy_faulty_images(source, destination)