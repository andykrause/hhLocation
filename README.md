# Household age and metropolitan location

This repository contains the necessary code to reproduce the analysis of household location across 50 metropolitan regions as discussed in the paper entitled: "Fancy Title Here", by Anonymous Real Estate Authors (anonymous for peer review effects).

## Documentation

### Introduction

This document explains the process for reproducing the data and analysis described in the paper entitled ``Fancy Title Here''.  There are three key steps in reproducing the data and analysis:

1. Download all code and data files from this repository at: [http://www.github.com/AnonREAuthor/hhLocation](http://www.github.com/AnonREAuthor/hhLocation "Git")

2. Open the code in R and change the directory paths to your desired paths (more on this later)

3. Execute the **hhLocAnalysis.R** script.  Note:  this may take a few hours as the raw data is downloaded the first time you run the script.

NOTE: Data at two intermediate steps may be downloaded from DATAVERSE WEBSITE.  This allows the user to skip the very lengthly and data storage heavy data compilation and initial cleaning and compiling steps (if so desired).  The two intermediate datasets are described below.

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

#### Intermediate Data

To save time, the user may download intermediate datasets -- prepared data (predData.csv) and cleaned data (cleanData.csv) from the DATAVERSE site.  The prepared data included compiled household age information on all census blocks in the 50 largest CBSAs.  The cleaned data has removed census blocks without households, fixed a number of city names and rescaled the distances.  Instructions on how to replicate the analysis with either of the intermediate datasets is described below. 

### Before running the code

#### R Libraries

Ensure that the following R libraries are installed: `ggplot2`, `plyr`, `dplyr`, `geosphere` and `stringr`.  You can check them with the `library` commands as shown below.  Missing libraries can be downloaded and installed with `install.packages('ggplot2')`, for example. 

    library(ggplot2)
    library(reshape)
    library(plyr)
    library(dplyr)
    library(geosphere)
    library(stringr)

#### Analysis parameters

Six parameters control the depth and type of analyses that will be performed.  **reBuildData** determines whether or not the user intends to download all of the raw data directly from the census and completely recreate the analysis from scratch.  Users who have NOT downloaded one of the two intermediate datasets must set this parameter to TRUE. **reCleanData** determines whether or not the prepared data will be cleaned. Users who have downloaded the **cleanData.csv** may set this to FALSE and use the downloaded dataset.  The **reScaleDists** parameter allows users to change the scaling of the distance variables.  If the user is recreating the data from the beginning (not using intermediate datasets) then setting **reScaleDists** to FALSE will use all census block groups with households regardless of their distance from the centers or subcenters in the CBSA.  Setting this parameter to TRUE will scale the distances by the lesser of the maximum distance in each CBSA region or by the global **maxDist** parameter (in miles).  In the paper we use a maximum distance of 60 miles.  If the user has opted to use the clean data intermediate dataset then the data is already scaled to the 60 mile distance and this parameter can be set to FALSE.

The **nbrCBSA** parameter determines how many CBSAs to analyze.  The count is done from the most populous down to the least populous.  A value of 50 is used in the analysis described in the paper.  A user wishing to change this value will have to recreate the data from scratch (set **reBuildData**, **reCleanData** and **reScaleDists** to TRUE).  Greatly increasing this value may greatly lengthen run-times and, depending on your computer memory, may crash the analysis. Finally, the **verbose** parameter defines whether or not the data-building and analytical functions will write their progress to the screen.  The defaults for the six parameters used in the paper are shown below. 

    reBuildData <- TRUE
    reCleanData <- TRUE
    reScaleDists <- TRUE
    nbrCBSA <- 50
    maxDist <- 60
    verbose <- TRUE

#### Directory Paths

Five directory and file paths must be set prior to running the analysis:

1. **dataDir**:  Directory where the raw census data will be stored.  Must have at least 12GB of free space.  If the user is utilizing an intermediate dataset, then this directory can be set to NULL.
2. **codeDir**:  Directory where the code files downloaded from Github are located.
3. **rawDataFile**:  Location to where the prepared data file (.csv) will be written.  If the user is utilizing the intermediate prepared data file (downloaded) then this path will point to that file.
4. **cleanDataFile**: Location to where the clean data file (.csv) will be written. If the user is utilizing the intermediate cleaned data file (downloaded) then this path will point to that file.
5. **figurePath**: Directory where the figures will be written.

Examples are shown below:

    dataDir <- 'c:/data/usa'
    codeDir <- 'c:/code/hhlocation'
    rawDataFile <- 'c:/code/hhlocation/data/hhdata.csv' 
    cleanDataFile <- 'c:/code/hhlocation/data/cleandata.csv'  
    figurePath <- 'c:/code/hhlocation/results'

#### Source files

For the final preliminary step the additional code files (functions) are sourced or loaded into memory.

    source(paste0(codeDir, '/buildHHData.R'))
    source(paste0(codeDir, '/buildCBSAData.R'))
    source(paste0(codeDir, '/hhLocFunctions.R'))

### Loading the Data

We begin by setting a number of the parameter into global parameters -- i.e. saving them to the global environment in R. 

     assign('gv', list(dataDir=dataDir,
                       codeDir=codeDir,
                       verbose=verbose,
                       nbrCBSA=nbrCBSA,
                       maxDist=maxDist
                       ))

Next, we move on to building the raw data from scratch (if the **reBuildData** parameter is set to TRUE).  A warning is provided letting the user know that this is a very timely operation.

     if(reBuildData){
       cat('\n\nWARNING:  This process may take more than an hour or two depending',
           'on your current data, internet connection and processing speeds\n\n')

Next, the code directory is checked to ensure that the list of cities and CBSAs is present. If this is the first time running the code, these may not be there (unless downloaded from the Github repository).  If they present, the two .csv files are read into memory and saved in the list called 'cbsa'. If they are not present, the list of cities and CBSAs are built using the **buildCBSAData()** function.  Note that if the files exist but the lenght of the CBSA list does not match the desired number of CBSAs (**nbrCBSA** parameter) the list will also be re-constructed.

     if(file.exists(paste0(codeDir, '/studyCityList.csv')) & 
          file.exists(paste0(codeDir, '/studyCBSAList.csv'))){

        cbsa <- list(cbsaList=read.csv(paste0(codeDir, '/studyCBSAList.csv')),
                     cityList=read.csv(paste0(codeDir, '/studyCityList.csv')))
      
        # If files exist but are not the correct size
        if(nrow(cbsa$cbsaList) != nbrCBSA){
          cbsa <- buildCBSAData(nbrCBSA=nbrCBSA, dataDir=dataDir)          
        }
      
      } else {
        cbsa <- buildCBSAData(nbrCBSA=nbrCBSA, dataDir=dataDir)    
      }
  
####  Building household data  

Next, the household data is constructed using the **buildHHData()** function.  This function downloaded, extracts, combines and prepares the data for each county located in one of the top **nbrCBSA** CBSAs (50 in this example).  More information on subfunctions and comments indicating the specific operations taken are available in the **buildHHData.R** file.
  
      hhData <- buildHHData(cbsaObj=cbsa, dataDir=dataDir, codeDir=codeDir,
                            outputPath=rawDataFile, returnData=TRUE)
     
If the user is utilizing intermediate data, the basic CBSA data must still be loaded.  This occurs in the FALSE portion of the `if(reBuildData)` code.
   
      } else {
    
        cbsa <- list(cbsaList=read.csv(paste0(codeDir, '/studyCBSAList.csv')),
                     cityList=read.csv(paste0(codeDir, '/studyCityList.csv')))
    
Finally, if the user has elected NOT to rebuild the data but TO re-clean the data then the intermediate prepared data is loaded at this time.

     if(reCleanData) hhData <- read.csv(rawDataFile, header=TRUE)
   
     } # closes if(reBuildData)

#### Data Cleaning

This section of code clean and finalizes the data for analysis.  If the user has downloaded the **cleanData.csv** file and has set the **reCleanData** parameter to FALSE then this section will be skipped as it is unnecessary.

     if(reCleanData){

We begin by removing all block that have no recorded households in them.

     xData <- subset(hhData, hhSum > 0) 

Next, the `hhSum` field is renamed to `total` 
  
     colnames(xData)[which(colnames(xData) == 'hhSum')] <- 'total'

We then extract the main, central city from each CBSA name.  This string value is added to the `cityName` field in the dataset.

     # Find dash
     dashLoc <- as.data.frame(str_locate(cbsa$cbsaList$city, "-"))$end
     dashLoc[is.na(dashLoc)] <- 100
     dashLoc <- dashLoc - 1

     # Extract first city name
     getName <- function(x, end){substr(x, 1, end)}
     cbsa$cbsaList$centralCity <- unlist(mapply(getName, 
                                               x=as.list(cbsa$cbsaList$city), 
                                               end=dashLoc))

     xData$cityName <- cbsa$cbsaList$centralCity[match(xData$cbsa, cbsa$cbsaList$code)]

For cities which share the first three letters (used in later graphical presentations), unique two letter codes are applied.

     xData$cityName[xData$cityName == 'New Orleans'] <- "NO"
     xData$cityName[xData$cityName == 'New York'] <- "NY"
     xData$cityName[xData$cityName == 'San Francisco'] <- "SF"
     xData$cityName[xData$cityName == 'San Diego'] <- "SD"

If the data is being built from scratch or if the distances in the intermediate files are being rescaled (``reScaleDists == TRUE``) then the distances are re-calculated.
  
     if(reScaleDists){
    
First, we build a list of all city and subcenter names

       cNames <- names(table(xData$subName))

Then a blank field to capture the scaled distance is added.

       xData$distScaled <- 0
    
For each city center or subcenter the scaled distance from each block centroid to the center/subcenter is then calculated and saled in the **distScaled** field. 

       # Loop through cities
       for(iName in 1:length(cNames)){
        
          # Create reference to all data points in city 'iName'
          cityInd <- which(xData$subName == cNames[iName])
        
          xData$distScaled[cityInd] <- round(scaleHHDist(xData$subDists[cityInd],
                                                 maxDist=gv$maxDist), 2)
       } # Ends for loop

If any distanced over 1 remain in the data (block greater then **maxDist** miles from the center/subcenter) then these are removed. 

       # Remove all those over 1
       xData <- subset(xData, distScaled <= 1)
    
      } # Ends if(reScaleDist)

Finally, the cleanded data is written the the **cleanDataFile** and some of the intermittance files are cleaned from the memory to free up space for future operations.

     ## Write out cleaned data

     write.csv(xData, cleanDataFile, row.names=FALSE)

     ## Remove raw data and clean memory

     rm(hhData)
     gc()

     } else {
   
If the distances are not being recalculated (and the user has downloaded the intermediate clean data file), then the data is simply read into memory.

     xData <- read.csv(cleanDataFile, header=T)
     }

### Data Analysis 

In this section we analyze the **hhData** using a set of custom function developed and described in the ``hhLocFunctions.R`` file. 

#### All Metro Analysis 
 
We begin by calculating the location quotients for all age groups across the combined set of census block in our study (all 50 CBSAs) using the **buildLQData()** function. The *logScale* argument determines the scaling on the X-axis, with a value of less than one essentially exaggerating the area near the CBD for visualization purposes. The *smoothLines* argument tells the function not to not create an additional smoothing step.  
 
      allData <- buildLQData(xData, metroName='All', logScale=5/9, smoothLines=FALSE)
  
Next, the **buildAgeLQPlot()** function takes the output from the previous step and plots it (on a ggPlot canvas).  

      allPlot <- buildAgeLQPlot(allData$plotData, title='All Metros', colorByAge=F,
                                addPOINT=TRUE, addS=TRUE, addLM=TRUE)

Finally, we export the plots to a .jpg file format
  
      jpeg(paste0(figurePath, "/All.jpg"), res=400, width=2500, height=3500)
         allPlot    
      dev.off(which=dev.cur())  
      
#### Metro-specific examples

We then create metro-specific analysis here, using Chicago and New York as examples. 
  
##### Chicago
  
     chiData <- buildLQData(xData, metroName='Chicago', logScale=5/9)
     chiPlot <- buildAgeLQPlot(chiData$plotData, title='Chicago Metro', colorByAge=F)
     jpeg(paste0(figurePath, "/Chicago.jpg"), res=400, width=2500, height=3500)
      chiPlot    
     dev.off(which=dev.cur())  

##### New York

     nyData <- buildLQData(xData, metroName='NY', logScale=5/9)
     nyPlot <- buildAgeLQPlot(nyData$plotData, title='New York Metro', colorByAge=F)
     jpeg(paste0(figurePath, "/NewYork.jpg"), res=400, width=2500, height=3500)
       nyPlot    
     dev.off(which=dev.cur())  
      

### Age-specific Analysis

We also compare a single age cohort (a set of individuals within a defined ten-year period) across all metropolitan regions.  The visualization of this analysis is shown via sparklines.

First, location quotients for all ages across all metro regions are calculated (NOTE: this can be a lengthly process)

     metroNames <- names(table(xData$cityName))
     metroData <- lapply(metroNames, buildLQData, yData=xData, logScale=5/9)

We then extract data from a single age cohort from all regions -- in this case from the set of households whose householder is between the ages of 15 to 24.

     age15Data <- lapply(metroData, stripAgeData, ageFactor=1)
     names(age15Data) <- metroNames

This data is then plotted used the **citySparkLines()** function.  

     jpeg(paste0(figurePath, "/Age15_24.jpg"), res=400, width=2500, height=3500)
       citySparkLines(age15Data, ncol=3, textSize=6, lineWidth=1.3, lineColor='navy',
                      plotTitle = 'Households Ages 15 - 24')
     dev.off(which=dev.cur())

We then do the same for the 75 to 84 cohort and the 85+ cohort.

     # Ages 75 to 84
     age75Data <- lapply(metroData, stripAgeData, ageFactor=7)
     names(age75Data) <- metroNames
     jpeg(paste0(figurePath, "/Age75_84.jpg"), res=400, width=2500, height=3500)
     citySparkLines(age75Data, ncol=3, textSize=6, lineWidth=1.3, lineColor='navy',
                    plotTitle = 'Households Ages 75 - 84')
     dev.off(which=dev.cur())

     # Age 85+
     age85Data <- lapply(metroData, stripAgeData, ageFactor=8)
     names(age85Data) <- metroNames
     jpeg(paste0(figurePath, "/Age85+.jpg"), res=400, width=2500, height=3500)
     citySparkLines(age85Data, ncol=3, textSize=6, lineWidth=1.3, lineColor='navy',
                    plotTitle = 'Households Ages 85+')
     dev.off(which=dev.cur())

# ABTIN CAN YOU DESCRIBE WHAT IS GOING ON HERE?

 ### Overlapping SEs

  d1 <- allData$plotData
  overlap <- ggplot(d1, aes(x=x,y=y, color=HouseholdAge)) + stat_smooth()+ ylab("Location Quotient") +
     xlab("Distance from CBD") +geom_point(position = "jitter", alpha = 0.3) 

  jpeg(paste0(figurePath, "/overlap.jpg"), res=400, width=6500, height=3500)
    overlap    
  dev.off(which=dev.cur())  

### Overlapping LMs 

  overlap2 <- ggplot(d1, aes(x=x,y=y, color=HouseholdAge)) 
  #overlap2 <- overlap2 + geom_point(alpha = 0.3)
  overlap2 <- overlap2 + stat_smooth(method="lm", geom="smooth", formula= y~x, size=.8, se=TRUE) + ylab("Location Quotient") +
    xlab("Distance from CBD") 
  
  jpeg(paste0(figurePath, "/overlapLM.jpg"), res=400, width=6500, height=3500)
    overlap2    
  dev.off(which=dev.cur()) 

