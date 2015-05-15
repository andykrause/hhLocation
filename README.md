# Household age and metropolitan location

This repository contains the necessary code to reproduce the analysis of household location across 35 metropolitan regions as discussed in the paper entitled: "Fancy Title Here", by Anonymous Real Estate Author (anonymous for peer review effects).

## Documentation

### Introduction

This document explains the process for reproducing the data and analysis described in the paper entitled ``Fancy Title Here''.  There are three key steps in reproducing the data and analysis:

1. Download all raw data files according to the instruction below from the www.dataverge.org hosted site. [ http://dx.doi.org/10.7910/DVN/RHJCNC]( http://dx.doi.org/10.7910/DVN/RHJCNC "DOI")

2. Download all code files from this repository at: [http://www.github.com/AnonREAuthor/hhLocation](http://www.github.com/AnonREAuthor/hhLocation "Git")

 **processOverview.png** 
 
### Downloading Data Files

The data files used in this analysis are hosted on the Harvard Dataverse Network and can be accessed online at [ http://dx.doi.org/10.7910/DVN/RHJCNC]( http://dx.doi.org/10.7910/DVN/RHJCNC "DOI"). Navigate to the 'DATA & ANALYSIS' tab and download all files in the 'Raw Assessor Data' and 'Raw GIS Data' sections.  Save the files to a suitable working directory.  This working directory will be used in the data preparation and analysis code that follows.  Users without a current Dataverse account will need to create a free account in order to access the data.

1. dataPrepCode.R:  The script to clean and prepare the raw data.
2. basicConversionTools.R:  A set of basic tools for converting data formats.
3. kingDataDevelop.R: A set of functions for dealing specific with King County data.
4. kingBuildSales.R: A set of function for dealing specically with King County Sales data.
5. dataAnalysisCode.R: The script that generates the results as discussed in the paper.
6. spatEconTools.R: A set of helper functions for working with spatial econometric models.


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



