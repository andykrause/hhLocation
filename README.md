# Household age and metropolitan location

This repository contains the necessary code to reproduce the analysis of household location across 50 metropolitan regions as discussed in the paper entitled: "Fancy Title Here", by Anonymous Real Estate Author (anonymous for peer review effects).

## Documentation

### Introduction

This document explains the process for reproducing the data and analysis described in the paper entitled ``Fancy Title Here''.  There are three key steps in reproducing the data and analysis:

1. Download all raw data files according to the instruction below from the www.dataverge.org hosted site. [ http://dx.doi.org/10.7910/DVN/RHJCNC]( http://dx.doi.org/10.7910/DVN/RHJCNC "DOI")

2. Download all code files from this repository at: [http://www.github.com/AnonREAuthor/hhLocation](http://www.github.com/AnonREAuthor/hhLocation "Git")

 **processOverview.png** 
 
### Downloading Data Files

The data files used in this analysis are hosted on the Harvard Dataverse Network and can be accessed online at [ http://dx.doi.org/10.7910/DVN/RHJCNC]( http://dx.doi.org/10.7910/DVN/RHJCNC "DOI"). Navigate to the 'DATA & ANALYSIS' tab and download all files in the 'Raw Assessor Data' and 'Raw GIS Data' sections.  Save the files to a suitable working directory.  This working directory will be used in the data preparation and analysis code that follows.  Users without a current Dataverse account will need to create a free account in order to access the data.

1. hhLocAnalysis.R:  The main script which controls the data cleaning, analysis and plotting.
2. buildCBSAData.R:  A set of functions to download and prepare the necessary CBSA information.
3. buildHHData.R: A set of functions for downloading and preparing the necessary census SF1 data.
4. hhLocFunctions.R: A set of functions for analyzing and plotting the household location data and results.

`install.packages()` 

    library(RODBC)
    library(RSQLite)
    library(DBI)
    library(stringr)
    library(maptools)
    library(plyr)
    library(sp)
    library(spdep)
    library(rgeos)
    library(spam)
    library(colorRamps)
    library(RColorBrewer)



