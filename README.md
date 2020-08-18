
# Changes in the EU fisheries quota allocation under climate change

This repository is intended to support the project *Changes in the EU fisheries quota allocation under climate change*

# Authors 

Elena Ojea <sup>1</sup>, Alex Tidd <sup>1</sup> & Juliano Palacios-Abrantes<sup>2</sup>, 

1. Future Oceans Lab, Universidad de Vigo, Vigo, Spain

2. Institute for the Oceans and Fisheries, University of British Columbia, Vancouver, Canada.

## Files and folders organization:

In this repository you will find all of the code related to the project. **Note** that due to the large volume of the (raw) data used in the analysis, we will not able to upload all data to *GitHub*. Please email the corresponding authors to discuss ways to share the data.

### Folders

- **data (external)**: Folder where processed data is stored. Data folder is sub-divided in `spatial`; everything related to spatial analysis (e.g. shapefiles),`tacs`; everything related to ICES and EU tac system (e.g. current TAC allocation), `species`; data related to the specific species analyzed, `dbem`; processed outputs from the DBEM model.

**Note:**  Data will not be located here as it exceeds GitHub's size cap


- **figures**: Contains the *Exploratory* and *Selected* project figures (Part of *Results*)

- **functions**: Folder with the functions needed for the analysis. Function folder is sub-divided in `analysis` and `results` to spread functions needed in each section. 

- **tables**: Contains project's tables (Part of *Results*)

- **references**: Documents needed for the references of the manuscript including the reference list

- **results**: Intended to have the analyzed data of the project, e.g. the final results.

- **scripts**: Scripts for analysis a manuscript

### Files

- **analysis.RMD**: This script has the main analysis of the project

- **manuscript_draft.RMD**: Manuscript draft for project.

# Guidelines

## Data
- Locate data according to sub-folders
- If data is included, updated `data_readme.md`
- All data should be named in low caps and underscores (e.g. `some_data.csv`)
- All variables within data will be cleaned to lower caps and underscore with `janitor::clean_names()`

## Scripts

- Identify yourself with initials (e.g. `#JEPA`) in parts of the script when modifying it
-Use `<-` to name variables not `=`
- Keep all functions and variables names with first capital and then low caps and underscores (e.g. `tac_future <- function()`), this is to differentiate from variables within tables)
