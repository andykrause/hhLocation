# Household age and metropolitan location

This repository contains the necessary code to reproduce the analysis of household location across 50 metropolitan regions as discussed in the paper entitled: "Fancy Title Here", by Anonymous Real Estate Author (anonymous for peer review effects).

## Documentation

### Introduction

This document explains the process for reproducing the data and analysis described in the paper entitled ``Fancy Title Here''.  There are three key steps in reproducing the data and analysis:

1. Download all code and data files from this repository at: [http://www.github.com/AnonREAuthor/hhLocation](http://www.github.com/AnonREAuthor/hhLocation "Git")

2. Open the code in R and change the directory paths to your desired paths (more on this later)

3. Execute the **hhLocAnalysis.R** script.  Note:  this may take a few hours as the raw data is downloaded the first time you run the script.

NOTE THAT A FOURTH IS REQUIRED IF YOU WANT TO SKIP THE TIMELY DATA BUILDING PROCESSES AND GO STRAIGHT TO THE ANALYSIS.  Will probably have to host the cleaned data file at dataverse....

### Downloading Code and Data

All code used the create this analysis, including raw data, cleaned data and final analysis are available at [http://www.github.com/AnonREAuthor/hhLocation](http://www.github.com/AnonREAuthor/hhLocation "Git").  This complete data provenance is recorded in R (version 3.1.1) and was built using the RStudio IDE.  There are four separate files in this repository.  The first, **hhLocAnalysis.R:**, is the main script and is the only one that needs to be updated and executed.  A description of each is below.

1. **hhLocAnalysis.R:**  The main script which controls the data cleaning, analysis and plotting.
2. **buildCBSAData.R:**  A set of functions to download and prepare the necessary CBSA information.
3. **buildHHData.R:** A set of functions for downloading and preparing the necessary census SF1 data.
4. **hhLocFunctions.R:** A set of functions for analyzing and plotting the household location data and results.

Along with this code files are three small data files in .csv form.  The first is required to run the analysis in any form, while the second two are necessary only to recreate the analysis exactly as first performed.  Note that removing the second and third file but keeping the `nbrCBSA` parameter at a value of **50** will create the same results. 

1. **statelist.csv:**  Simple list of all 50 states with their abbreviations.
2. **studyCBSAlist.csv:**  A list of the 50 most populous CBSAs in the United States.
3. **studyCitylist:** A list of all cities (subcenters) that are named within the most populous 50 CBSAs.

### Before running the code

#### R Libraries

Ensure that the following R libraries are installed: `ggplot2`, `plyr`, `dplyr`, `geosphere` and `stringr`.  You can check them with the `library` commands as shown below.  Missing libraries can be downloaded and installed with `install.packages('ggplot2')`, for example. 

    library(ggplot2) 
    library(plyr)
    library(dplyr)
    library(geosphere)
    library(stringr)

#### Your local directories

