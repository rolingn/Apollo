### The name of this environment should not be changed upon setup for the pipeline to work correctly

## Creating the sleap environment:

## First option:
conda env create -f sleap_environment.yml


## Second option:

# Conda command for Mac machines:
conda create -y -n sleap -c conda-forge -c anaconda -c sleap sleap=1.3.3

# Conda command for Windows machines:
conda create -y -n sleap -c conda-forge -c nvidia -c sleap -c anaconda sleap=1.3.3


# Activate the environment after creating it:
conda activate sleap


# Navigate to the environment folder with:
# Mac:
cd $CONDA_PREFIX
# Windows:
cd %CONDA_PREFIX%


# Find this folder and move the contents of the sleap_setup folder to the sleap environment folder. Printing the following in the terminal might help finding the sleap environment folder:
# Mac:
echo $CONDA_PREFIX
# Windows:
echo %CONDA_PREFIX%


# Install the requirements:
conda install pip
pip install -r requirements.txt


# The setup should be complete. You can close the terminal. Additional 'pip install' calls might be necessary if packages are missing.