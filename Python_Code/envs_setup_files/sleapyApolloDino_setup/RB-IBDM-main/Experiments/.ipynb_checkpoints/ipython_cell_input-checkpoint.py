## Not guaranteed, that this code works properly. Check results carefully after use.

def a(get_values, labels, threshold, buffer, pixel_scale, image_folder):
    start_time = time.time()
    
    settings = get_values()
    start_processing = False
    pixel_scale /= 10
    blobs = []
    trails = {}
    tracker = AMTTracker(config)
    object_detector = load_grounding_dino_model()
    first_pass = True
    images = os.listdir(image_folder)
    images = sorted(images)
    
    for image in tqdm(images):
        print("Analyzing image", image)
        image_arg = os.path.join(image_folder, image)
        skip, start_processing = should_image_be_skipped(settings.start_image, start_processing, image_arg, image, image_folder)
        if skip:
            continue
        detections = detect(
            object_detector,
            image=image_arg,
            labels=labels,
            threshold=threshold
        )
        print("Nr. of detections:", len(detections))
        image_array = Image.open(image_arg)
        image_array = np.array(image_array)
        blobs = convert_bounding_boxes(detections, image, image_array, image_folder, buffer)
        if first_pass:
            tracker.savedois = blobs
        new_tracks, _ = tracker.managetracks(tracker.savedois, blobs, first_pass)
        tracker.savedois = new_tracks
        first_pass = False
    
        if settings.save_visualisation:
            plot_tracks_and_detections(image_array, new_tracks, image)
    
    print("--------")
    
    ## Classifying the cropped images
    print("Done detecting all the insects and saving cropped versions.")
    
    print("Double-checking for faulty detections...")
    Apollo_input_path = os.path.join(image_folder, "cropped")
    Apollo_script_path = os.path.join(envs_path, "ApolloNet", "Intro-to-CV-for-Ecologists-main", "inference_dirt_classifier.py")
    Apollo_command = f'conda run -n ApolloNet python "{Apollo_script_path}" "{Apollo_input_path}"'
    Apollo = subprocess.run(Apollo_command, shell=True, capture_output=True, text=True)
    #print(Apollo.stdout)
    if Apollo.returncode != 0:
        print(Apollo.stderr)
    else:
        matches = re.search(r"HERE (\d+)", Apollo.stdout)
        matches = int(matches.group(1))
        if matches > 0:
            print(f"Found {matches} potentially faulty crops. Moved them to the potentially_faulty folder.\n--------")
        else:
            print("All crops seem to be insects :)\n--------")
    
    
    print("Classifying crops now.")        
    
    if settings.InsectDetect:
        print("InsectDetect classifier running...")
        os.chdir(os.path.join(envs_path, "InsectDetectSAM", "yolov5-cls"))
        print("Working directory is now:", os.getcwd())
        InsectDetect_input_path = os.path.join(image_folder, "cropped")
        !python classify\predict.py --project {image_folder} --name results --source {InsectDetect_input_path} --weights "insect-detect-ml-main/models/efficientnet-b0_imgsz128.onnx" --img 128 --sort-top1 --sort-prob --concat-csv
        print("--------")
    
    if settings.ApolloNet:
        print("Performing ApolloNet Classification :)")
        Apollo_input_path = os.path.join(image_folder, "cropped")
        Apollo_script_path = os.path.join(envs_path, "ApolloNet", "Intro-to-CV-for-Ecologists-main", "inference.py")
        Apollo_command = f'conda run -n ApolloNet python "{Apollo_script_path}" "{Apollo_input_path}"'
        Apollo = subprocess.run(Apollo_command, shell=True, capture_output=True, text=True)
        if Apollo.returncode != 0:
            print(Apollo.stderr)
        else:
            print("ApolloNet ran clean\n--------")
    
    if settings.BioClip:
        print("The tree of life is growing: running BioClip algorithm for image classification now...")
        BioClip_input_path = os.path.join(image_folder, "cropped")
        crops_for_BioClip = glob.glob(os.path.join(BioClip_input_path, '*'))
        crops_for_BioClip = sorted([item for item in crops_for_BioClip if os.path.isfile(item)])
        BioClip_predictions = BioClip_inference(crops_for_BioClip, settings.rank, certainty_threshold = 0.45)
        if len(BioClip_predictions) > 0:
            clean_predictions = process_BioClip_predictions(BioClip_predictions, image_folder, settings.rank)
        print("--------")
        
    
    print("Done classifying the insects. Measuring body lengths now...")
    length_input_path = os.path.join(image_folder, "cropped")
    length_script_path = os.path.join(envs_path, "sleap", "body_length_inference_folder.py")
    command = f'conda run -n sleap python "{length_script_path}" "{length_input_path}" "{pixel_scale}"'
    ran_clean = subprocess.run(command, shell=True, capture_output=True, text=True)
    if ran_clean.returncode != 0:
        print(f"stdout = {ran_clean.stdout}")
        traceback_index = ran_clean.stderr.find("Traceback")
        print(ran_clean.stderr[traceback_index:])
    else:
        print("Length measurements ran clean\n--------")
    
    merge_result_csvs(image_folder)
        
    
    print("Done measuring. Annotating all results onto cropped images now...")
    results_csv = get_classification_results_csv(image_folder, settings.annotation_algorithm)
    input_folder = os.path.join(image_folder, "cropped_and_annotated")
    length_csv_file_path = os.path.join(image_folder, "results", "body_length_results.csv")
    
    if results_csv is not None:
        annotate_classifications(classification_results_csv = results_csv, body_length_csv = length_csv_file_path,
                                 cropped_images_folder = input_folder, image_folder = image_folder, pixel_scale = pixel_scale,
                                annotation_algorithm = settings.annotation_algorithm)
    
    print("--------")
    
    end_time = time.time()
    elapsed_time = end_time - start_time
    
    print(f"Done with everything! No length Errors occured :)\nElapsed time: {elapsed_time/60:.2f} minutes \nTime per Image: {elapsed_time/len(images):.2f} seconds")
    return end_time

end_time = a(get_values, labels, threshold, buffer, pixel_scale, image_folder)
