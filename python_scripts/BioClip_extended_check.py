import pandas as pd
import random
import tkinter as tk
from tkinter import filedialog
import cv2
import os

def select_csv_file():
    root = tk.Tk()
    root.withdraw()
    file_path = filedialog.askopenfilename(filetypes=[("CSV files", "*.csv")])
    return file_path

def extract_order(filename):
    parts = filename.split("/")
    for i, part in enumerate(parts):
        if part == "images" and i + 2 < len(parts):
            return parts[i + 2]  # Extract the order name dynamically
    return "Unknown"

def load_existing_results(output_path):
    if os.path.exists(output_path):
        return pd.read_csv(output_path)
    return pd.DataFrame(columns=['Filename'])

def process_csv(file_path):
    df = pd.read_csv(file_path)
    
    # Define output path
    output_path = os.path.join("/Volumes/Apollo/Diopsis_Cameras/RESULTS_2024/BioClip_GT_results", "User_checked_GT.csv")
    
    # Load existing results to avoid duplicate checking
    existing_results = load_existing_results(output_path)
    already_checked = set(existing_results['Filename'])
    amount_checked = len(already_checked)
    
    # Filter rows where 'Response' is 'Incorrect' or 'Unsure' and not already checked
    #filtered_df = df[df['Response'].isin(['Incorrect', 'Unsure']) & ~df['Filename'].isin(already_checked)]
    #filtered_df_base = df[df['Response'].isin(['Incorrect', 'Unsure'])]
    filtered_df_base = df[df['Response'].isin(['Incorrect'])]
    filtered_df = filtered_df_base[~filtered_df_base['Filename'].isin(already_checked)]
    len_filtered_df = len(filtered_df)
    print(f"{len(filtered_df_base) - len_filtered_df} images already checked -> {100 - round(amount_checked/len(filtered_df_base), 2)} % remaining")
    
    if filtered_df.empty:
        print("No new matching rows found.")
        return
    
    # Shuffle the filtered rows
    filtered_df = filtered_df.sample(frac=1).reset_index(drop=True)
    
    output_data = []
    
    for _, row in filtered_df.iterrows():
        filename = row['Filename']
        amount_checked += 1
        
        # Extract the required part of the path
        if "/images/" in filename:
            extracted_path = filename.split("/images/")[-1]
            extracted_path = "images/" + extracted_path
        else:
            print("Invalid filename format.")
            continue
        
        display_filename = os.path.join("/Volumes/Apollo/Diopsis_Cameras/App", extracted_path)
        #print(display_filename)
        
        # Extract predicted order
        predicted_order = extract_order(filename)
        #print(predicted_order)
        
        # Display the image
        image = cv2.imread(display_filename)
        if image is None:
            print(f"Error loading image: {display_filename}")
            continue
        
        cv2.imshow("Image", image)
        key = cv2.waitKey(0) & 0xFF
        cv2.destroyAllWindows()
        
        # Check for escape key press to terminate
        if key == 27:  # Escape key
            print("Process terminated by user.")
            cv2.destroyAllWindows()
            return
        
        # Assign GT order based on key press
        gt_order = None
        if key == ord('d'):
            gt_order = 'Diptera'
        elif key == ord('h'):
            gt_order = 'Hymenoptera'
        elif key == ord('l'):
            gt_order = 'Lepidoptera'
        else:
            gt_order = 'Other or Unknown'
            #print("Invalid key pressed, skipping.")
            #continue
        
        # Store the result
        output_data.append ({
            'Filename': filename,
            'Response': row['Response'],
            'Predicted order': predicted_order,
            'GT order': gt_order
        })
    
        # Save to CSV
        if output_data:
            output_df = pd.DataFrame(output_data)
            output_df.to_csv(output_path, mode='a', index=False, header=not os.path.exists(output_path))
            if len_filtered_df != 0:
                print(f"{100 - round(amount_checked/len(filtered_df_base), 2)} % remaining")
        else:
            print("No new data to save.")
        
        # Ensure all OpenCV windows are closed before exiting
        cv2.destroyAllWindows()

if __name__ == "__main__":
    file_path = select_csv_file()
    if file_path:
        process_csv(file_path)
    else:
        print("No file selected.")
