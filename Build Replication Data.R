# --------------------------------------------------------------------------------------------
# Builds Data for Replication of:
# "The downstream air pollution impacts of the transition from 
#        coal to natural gas in the United States"
# Nature Sustainability, January 2020 
# doi: 10.1038/s41893-019-0453-5
# --------------------------------------------------------------------------------------------
# J. Burney (jburney@ucsd.edu)
# --------------------------------------------------------------------------------------------

# This code builds data to be used for replication analysis in paper above.
# Replication code repository is www.github.io/jaburney/naturalgastransition 
# Assumption is that this file is run from a directory containing the Data/ directory tree.
# This file references only down the directory tree.

dir.create("L1 Processed Data/")
dir.create("L2 Processed Data/")
dir.create("Final Data Sets for Analysis/")

# L0 Input Data/ contains the raw data source files from various sources, described in paper.
# L1 Data Processing Code/ contains scripts to load, aggregate, minimally clean L0 data
#    Outputs are saved in L1 Processed Data/
# L2 Data Processing Code/ contains scripts to aggregate, match, extract L1 data
#    Outputs are saved in L2 Processed Data/ and Final Data Sets for Analysis/

# To run replication code at the link above, you will need to ensure you have the correct
# directory structure to access the files, as noted in the instructions - this will just mean
# moving some files produced here into a Data/ folder as described.

# --------------------------------------------------------------------------------------------
# Required packages
# --------------------------------------------------------------------------------------------

# basic data handling
library(foreign)
library(tidyverse)
library(zoo)
library(data.table)

# spatial stuff
library(sp)
library(rgdal)
library(raster)
library(spdep)
library(rgeos)
library(GISTools)
library(rworldmap)
library(Matrix.utils)
library(velox)
library(ncdf4)

# regressions
library(lfe)
library(stargazer)

# plotting
library(RColorBrewer)
library(Cairo)
library(ggpubr)

# --------------------------------------------------------------------------------------------
# Level 1 Data Processing
# --------------------------------------------------------------------------------------------
# These files take all data in native formats from their original sources and do a few things:
# a. for rasters - read in, trim to common extent, align
# b. for plant data - read in, concatenate, clean
# c. for county data - read in, clean county names
#
# Note - these can be extremely slow in R on a personal machine, so only re-do if necessary
# --------------------------------------------------------------------------------------------

# The following scripts...
# "Data/L1 Data Processing Code/Global Processing Info.R"
# "Data/L1 Data Processing Code/Process EPA Power Plant Data.R"
# "Data/L1 Data Processing Code/Process MODIS AOD Data.R"
# "Data/L1 Data Processing Code/Process OMI SSA Data.R"
# "Data/L1 Data Processing Code/Process NO2 Data.R"
# "Data/L1 Data Processing Code/Process OMI HCHO Data.R"
# "Data/L1 Data Processing Code/Process OMI O3 Data.R"
# "Data/L1 Data Processing Code/Process OMI SO2 Data.R"
# "Data/L1 Data Processing Code/Process PM2p5 Data.R"
# "Data/L1 Data Processing Code/Process EPA AQ Report Data.R"
# "Data/L1 Data Processing Code/Process USDA NASS Crop Data.R"
# "Data/L1 Data Processing Code/Process NASS Area Data.R"
# "Data/L1 Data Processing Code/Process NVSS Mortality Data.R"

# Take input (raw) data and produce the following data files:
# "L1 Processed Data/CropAreas.Rdata"
# "L1 Processed Data/EPA AMPD Cleaned 2005-2016.Rdata"
# "L1 Processed Data/EPA Annual Surface Ozone 0.25deg 2005-2016.Rdata"
# "L1 Processed Data/EPA Annual Surface PM 0.25deg 2005-2016.Rdata"
# "L1 Processed Data/Spatial Info.Rdata"
# "L1 Processed Data/USA County Crop Yields 2005-2016.RData"
# "L1 Processed Data/USA County Mortality 2005-2016.RData"
# "L1 Processed Data/USA MODIS AOD 2005-2016.Rdata"
# "L1 Processed Data/USA NO2 2005-2016.Rdata"
# "L1 Processed Data/USA OMI HCHO 2005-2016.Rdata"
# "L1 Processed Data/USA OMI O3 2005-2016.Rdata"
# "L1 Processed Data/USA OMI SO2 2005-2016.Rdata"
# "L1 Processed Data/USA OMI SSA 2005-2016.Rdata"
# "L1 Processed Data/USA PM2.5 2005-2016.Rdata"

# Warning: SLOW!

source("Data/L1 Data Processing Code/Global Processing Info.R")
source("Data/L1 Data Processing Code/Process EPA Power Plant Data.R")
source("Data/L1 Data Processing Code/Process MODIS AOD Data.R")
source("Data/L1 Data Processing Code/Process OMI SSA Data.R")
source("Data/L1 Data Processing Code/Process NO2 Data.R")
source("Data/L1 Data Processing Code/Process OMI O3 Data.R")
source("Data/L1 Data Processing Code/Process OMI SO2 Data.R")
source("Data/L1 Data Processing Code/Process PM2p5 Data.R")
source("Data/L1 Data Processing Code/Process EPA AQ Report Data.R")
source("Data/L1 Data Processing Code/Process USDA NASS Crop Data.R")
source("Data/L1 Data Processing Code/Process NASS Area Data.R")
source("Data/L1 Data Processing Code/Process NVSS Mortality Data.R")

# --------------------------------------------------------------------------------------------
# Level 2 Data Processing
# --------------------------------------------------------------------------------------------
# These files take Level 1 processed files and merge/clean them to generate the final data
# sets used for analysis.
# --------------------------------------------------------------------------------------------

## First - at the plant location

# (1) Create power plant panel, code fuel types and switches, get station locations, etc.
# Output: Data/L2 Processed Data/Cleaned Power Plant and Spatial Data 2005-2016.Rdata
source("Data/L2 Data Processing Code/Clean EPA Power Plant Data.R")

# (2) Extract PM and atmospheric data around power plants (for plant location level analysis)
# Output: Data/L2 Processed Data/Extracted Monthly and Annual Atmospheric Data.Rdata
#         Data/L2 Processed Data/Extracted All Points Atmospheric Data.Rdata
#         Data/L2 Processed Data/Satellite and Surface Rasters.Rdata
source("Data/L2 Data Processing Code/Extract Raster Data.R")

# (3) Merge plant level data with surrounding county-level information
# Output: Data/Final Data Sets for Analysis/FullPanel_AllPoints_Annual.Rdata
#         Data/Final Data Sets for Analysis/FullPanel_Counties_Annual.Rdata
#         Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Annual.Rdata
#         Data/Final Data Sets for Analysis/FullPanel_PlantLocations_Monthly.Rdata
source("Data/L2 Data Processing Code/Reshape and Merge all Data.R")


## Second - at the county level

# 4. Aggregate raster environmental data to county level averages
# Output: Data/L2 Processed Data/County Environmental Data Extracted from Rasters.Rdata
source("Data/L2 Data Processing Code/Aggregate Rasters to Counties.R")

# 5. Calculate and summarize information from plant units within distance bands of each county
# Output: Data/L2 Processed Data/County Data with Plant Neighbors.Rdata
source("Data/L2 Data Processing Code/Generate Neighbors.R")

# 6. Merge neighbor set with county and power plant data
# Output: Data/Final Data Sets for Analysis/FullPanel_CountiesWithPlantNeighbors_Annual.Rdata
source("Data/L2 Data Processing Code/Generate Full County Data.R")

## 7. Prep AOD and Albedo data for RF Calculations
# Output: Data/L2 Processed Data/RF Calculations Data.Rdata
source("Data/L2 Data Processing Code/Process Data for RF Calculations.R")
