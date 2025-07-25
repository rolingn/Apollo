This environment is used to run the SLEAP algorithm: https://sleap.ai/tutorials/initial-training.html

To run the SLEAP GUI:
- Open terminal window.
- activate the SLEAP environment by running: conda activate sleap
- navigate to the environment folder e.g: cd C:\Users\rolingni\AppData\Local\anaconda3\envs\sleap
- open the GUI by typing "sleap-label" and enter.

There are comprehensive tutorials on how to use the GUI on the SLEAP website. Basic steps:
- Load images into the program
- Load the skeleton. Skeleton files are included in the env directory ("SimpleFlySkeleton_bodyLength.json" for body length and "SimpleSkeleton_bodyWidth.json" for body width measurements (not integrated into the pipeline at the moment))
- Run the inference from the menu at the top using one of the existing models in the "models" folder of the environment path
- Correct the predictions using the GUI
- Run training with corrected predictions, making sure "resume training" is checked. This way, the "old" model is taken as the starting point and improved through the new training data you just genarated.
- Make sure that the images are converted to black and white before training and inference since the base architecture of the already trained models where built using this setting. Errors might appear otherwise.
- Models should be automatically saved.

Folders and files relevant to the pipeline or the training process:
- models
- models_width
- body_lenght_inference_folder.py
- SimpleFlySkeleton_bodyLength.json
- SimpleSkeleton_bodyWidth.json