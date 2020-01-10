## Build Data for Coal-to-Natural Gas Transition Analysis

Underlying code to build the data sets used in www.github.com/jaburney/naturalgastransition, which replicates the analysis in the paper: "The downstream air pollution impacts of the transition from coal to natural gas in the United States," J. Burney, published in *Nature Sustainability* (2020). [https://doi.org/10.1038/s41893-019-0453-5]

### Setup & Organization

Due to size considerations :thinking: raw underlying data are hosted elsewhere. Please see the accompanying website for the paper (www.jaburney.net/coal-to-natural-gas-transition) to download four zipped data files (AdminPlantsCropsMortality.zip, Aerosols.zip, SO2NO2O3.zip, EPA AQ Report.zip). Unzip these within the **L0 Input Data/** repository directory.

This repository should then have the following structure:

* **L0 Input Data/** contains a set of unzipped data folders (sources are described in the paper, linked above):

   Administrative (GADM)/   
   Aerosols (MODIS and OMI)/   
   Crops (USDA NASS)/   
   EPA AQ Report/   
   Formaldehyde (OMI)/   
   Mortality (NVSS)/   
   NO2 (DOMINO)/   
   Ozone - Satellite (Multi-Sensor)/   
   PM2p5 (Van Donkelaar)/   
   Power Plants (EPA Acid Rain Program Monthly)/   
   SO2 (OMI)/   
   Surface Reflectance Data/   
   
* **L1 Data Processing Code/** contains a set of scripts that process the input data minimally; outputs are stored in the created directory **L1 Processed Data/**:

   Global Processing Info.R   
   Process EPA AQ Report Data.R   
   Process EPA Power Plant Data.R   
   Process MODIS AOD Data.R   
   Process NASS Area Data.R   
   Process NO2 Data.R   
   Process NVSS Mortality Data.R   
   Process OMI HCHO Data.R   
   Process OMI O3 Data.R   
   Process OMI SO2 Data.R   
   Process OMI SSA Data.R   
   Process PM2p5 Data.R   
   Process USDA NASS Crop Data.R   

* **L2 Data Processing Code/** contains a set of scripts that process the intermediate files from **L1 Processed Data/** and stores final data sets in the created directories **L2 Processed Data/** and **Final Data Sets for Analysis/**:

   Aggregate Rasters to Counties.R   
   Clean EPA Power Plant Data.R   
   Extract Raster Data.R   
   Generate Full County Data.R   
   Generate Neighbors.R   
   Process Data for RF Calculations.R   
   Reshape and Merge all Data.R   

* **Build Replication Data.R** calls all of the scripts above in a reasonable order and contains descriptions of what each does. I strongly suggest reading through it first and running piecemeal, as opposed to in one go: some of the steps take a long time to run and may not be necessary, depending on your objectives. The script begins by creating **L1 Processed Data/** **L2 Processed Data/** and **Final Data Sets for Analysis/** to hold outputs.

The final step to prepare the data for replication analysis involves selecting the correct data sets and placing them in a **Data/** directory for analysis, as described in the main replication repository (link above).

### Requirements

Code was written in R version 3.6.1, and requires a number of additional packages (latest versions can be installed with the following):

```R
install.packages(c('tidyverse','foreign','zoo','data.table','sp','rgdal','raster','spdep','rgeos','GISTools','rworldmap','Matrix.utils','velox','ncdf4','lfe','stargazer','RColorBrewer','Cairo','ggpubr'), dependencies = T)
``` 

### Contact

Jen Burney, jburney@ucsd.edu

More information about this project can be found at www.jaburney.net/coal-to-natural-gas-transition

