# Intro-to-CV-for-Ecologists
This repository is meant to be used with the worked example from "A gentle introduction to computer vision-based specimen classification in ecological datasets" from the Journal of Animal Ecology.

For data preparation, model training, and evaluation of the tabular models (XGBoost, LightGBM, and Random Forests), see 'R-Code.ipynb'. 
For data preparation, model training, and evaluation of the direct-from-image models (ResNet-50, VGG-16, and custom CNN), see 'Python-Code.ipynb'.
To see the required versions of Python, R, and associated packages/libraries, see the requirements text files in the 'Code' directory.


---> This repository was used to train 2 custom neural networks that are used in the Apollo pipeline:
- Rougly classifying the images into 5 custom classes (not very sophisticated)
- Dirt-detector. Detecting dirt and other unwanted crops - basically correcting any errors that might have been made by GroundingDino in the first place. Could be impoved by running training again with more training data that is now available.

The folders "Code" and "Data" are not relevant to the Pipeline. 