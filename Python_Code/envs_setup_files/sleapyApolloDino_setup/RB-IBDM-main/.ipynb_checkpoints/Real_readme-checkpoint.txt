This environment (sleapyApolloDino) is where the main pipeline lives.

Pipeline Notebooks:
- Pipeline.ipynb
- Pipeline_batch.ipynb

Pipeline can be used to perform insect detection, classification and measurement on a single folder of pictures

Pipeline_batch is meant to be used on the whole camera folder from a DIOPSIS camera. The file structure looks like this:
- DIOPSIS-382 containing a folder called "photos" which contains multiple folders for each single day.
The batch pipeline also creates a single results csv for all the days of that camera in the end.

Essential folders that make the pipeline work:
- FunktionenZumImportieren
- AMT_functions
- The two other environments (sleap and ApolloNet) need to be set up properly

The InsectSAM folder is a relict of the project where this code is faintly based on (InsectSAM: https://github.com/martintomov/RB-IBDM)
As far as I remeber, only the use of GroundingDino remains the same as in the original project.