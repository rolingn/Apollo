### the name of this environment should not be changed upon setup for the pipeline to work correctly

## Creating the ApolloNet environment:

## First option:
conda env create -f ApolloNet_environment.yml


## Second option:

conda create -n ApolloNet python == 3.9.19


# Activate the environment after creating it:
conda activate ApolloNet


# Navigate to the environment folder with:
# Mac:
cd $CONDA_PREFIX
# Windows:
cd %CONDA_PREFIX%


# Find this folder and move the contents of the ApolloNet_setup folder to the ApolloNet environment folder. Printing the following in the terminal might help finding the ApolloNet environment folder:
# Mac:
echo $CONDA_PREFIX
# Windows:
echo %CONDA_PREFIX%


# Install the requirements:
conda install pip
pip install -r requirements.txt


# The setup should be complete. You can close the terminal. Additional 'pip install' calls might be necessary if packages are missing.