##########################################################################################
#                                                                                        #
#  Code to prep data for analysis  (loading and clipping taks a long time  )             #
#                                                                                        #
##########################################################################################

### Preliminary commands -----------------------------------------------------------------

 ## Load Libraries

  library(ggplot2)
  library(reshape2)
  library(R.utils)
  library(plyr)
  library(dplyr)
  library(geosphere)
  library(stringr)

 ## Set parameters

  # Required
  reBuildData <- FALSE
  reCleanData <- FALSE
  
  # Optional 
  reScaleDists <- TRUE
  nbrCBSA <- 50
  maxDist <- 60
  verbose <- TRUE

 ## Set patha

  dataDir <- 'c:/data/usa'
  codeDir <- 'c:/Code/research/hhLocation'
  rawDataFile <- 'c:/temp/hhdata.csv'       # where raw file is located data file
  cleanDataFile <- 'c:/temp/cleandata.csv'   # where to store clean data file
  figurePath <- 'c:/Dropbox/Research/PopDistPaper2/graphs'

 ## Source files

  source(paste0(codeDir, '/buildHHData.R'))
  source(paste0(codeDir, '/buildCBSAData.R'))
  source(paste0(codeDir, '/hhLocFunctions.R'))
  source(paste0(codeDir, '/hhPred.R'))
  

##########################################################################################
### Loading Data -------------------------------------------------------------------------

 ## Convert paramters to a global variable list
  
  assign('gv', list(dataDir=dataDir,
                    codeDir=codeDir,
                    verbose=verbose,
                    nbrCBSA=nbrCBSA,
                    maxDist=maxDist))

 ## If data is to be rebuilt

  if(reBuildData){
   
   # Give warning on time
    cat('\n\nWARNING:  This process may take more than an hour or two depending on your',
        'current data, internet connection and processing speeds\n\n')

   # Check for a complete list of CBSAs.  If not, build it
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
   
   hhData <- buildHHData(cbsaObj=cbsa, dataDir=dataDir, codeDir=codeDir,
                         outputPath=rawDataFile, returnData=TRUE)
     
  ## Future spots for building the actual SF1 census data
   
  } else {
   
 ## If using existing data
  
  # Load CBSA data
  cbsa <- list(cbsaList=read.csv(paste0(codeDir, '/studyCBSAList.csv')),
               cityList=read.csv(paste0(codeDir, '/studyCityList.csv')))
    
  # Load in prepared data (if it needs to be recleaned)
  
  if(reCleanData) hhData <- read.csv(rawDataFile, header=TRUE)
   
 }

##########################################################################################
### Preparing Data for Analysis ----------------------------------------------------------

 if(reCleanData){

 ## Remove Rural Blocks and those with no population
  
   xData <- subset(hhData, hhSum > 0)

 ## Fix anmes
  
   colnames(xData)[which(colnames(xData) == 'hhSum')] <- 'total'

 ## Add main city name to xData
  
   # Find dash
   dashLoc <- as.data.frame(str_locate(cbsa$cbsaList$city, "-"))$end
   dashLoc[is.na(dashLoc)] <- 100
   dashLoc <- dashLoc - 1

   # Extract first city name
   getName <- function(x, end){substr(x, 1, end)}
   cbsa$cbsaList$centralCity <- unlist(mapply(getName, 
                                             x=as.list(cbsa$cbsaList$city), 
                                             end=dashLoc))

   # add to xData
   xData$cityName <- cbsa$cbsaList$centralCity[match(xData$cbsa, cbsa$cbsaList$code)]

   # Fix names that look same in first three letters
   xData$cityName[xData$cityName == 'New Orleans'] <- "NO"
   xData$cityName[xData$cityName == 'New York'] <- "NY"
   xData$cityName[xData$cityName == 'San Francisco'] <- "SF"
   xData$cityName[xData$cityName == 'San Diego'] <- "SD"

  ## Rescale distances
  
   if(reScaleDists){
    
     # Build list of names
     cNames <- names(table(xData$subName))
    
     # Add field to Data
     xData$distScaled <- 0
    
     # Loop through cities
     for(iName in 1:length(cNames)){
        
        # Create reference to all data points in city 'iName'
        cityInd <- which(xData$subName == cNames[iName])
        
        xData$distScaled[cityInd] <- round(scaleHHDist(xData$subDists[cityInd],
                                                 maxDist=gv$maxDist), 2)
     } # Ends for loop
    
    # Remove all those over 1
     xData <- subset(xData, distScaled <= 1)
    
   } # Ends if(reScaleDist

  ## Write out cleaned data

   write.csv(xData, cleanDataFile, row.names=FALSE)

  ## Remove raw data and clean memory

   rm(hhData)
   gc()

 } else {
   
  ## If not recleaning data
   
   xData <- read.csv(cleanDataFile, header=T)
   
 }
 
##########################################################################################
### Data Analysis ------------------------------------------------------------------------

### All Metros ---------------------------------------------------------------------------
 
 ## Build data and plots

  allData <- buildLQData(xData, metroName='All', logScale=5/9, smoothLines=FALSE)
  allPlot <- buildAgeLQPlot(allData$plotData, title='50 Largest Metros', colorByAge=F,
                          addPOINT=TRUE, addS=TRUE, addLM=TRUE)

 ## Export Plots

  jpeg(paste0(figurePath, "/All.jpg"), res=400, width=2500, height=3500)
    allPlot    
  dev.off(which=dev.cur())  

### Create metro examples  ---------------------------------------------------------------

 ## Build data and plots
  
  # Chicago
  chiData <- buildLQData(xData, metroName='Chicago', logScale=5/9)
  chiPlot <- buildAgeLQPlot(chiData$plotData, title='Chicago Metro', colorByAge=F)
  jpeg(paste0(figurePath, "/Chicago.jpg"), res=400, width=2500, height=3500)
    chiPlot    
  dev.off(which=dev.cur())  

  # New York
  nyData <- buildLQData(xData, metroName='NY', logScale=5/9)
  nyPlot <- buildAgeLQPlot(nyData$plotData, title='New York Metro', colorByAge=F)
  jpeg(paste0(figurePath, "/NewYork.jpg"), res=400, width=2500, height=3500)
    nyPlot    
  dev.off(which=dev.cur())  

### Compare by age group -----------------------------------------------------------------

 ## Extra data for all metros

  metroNames <- names(table(xData$cityName))
  metroData <- lapply(metroNames, buildLQData, yData=xData, logScale=5/9)

## Extract age specific data 15-24

  # Age 15-24
  age15Data <- lapply(metroData, stripAgeData, ageFactor=1)
  names(age15Data) <- metroNames

  # Plot 
  jpeg(paste0(figurePath, "/Age15_24.jpg"), res=400, width=2500, height=3500)
    citySparkLines(age15Data, ncol=3, textSize=6, lineWidth=1.3, lineColor='navy',
               plotTitle = 'Households Ages 15 - 24')
  dev.off(which=dev.cur())

  # Age 75-24
  age75Data <- lapply(metroData, stripAgeData, ageFactor=7)
  names(age75Data) <- metroNames

  # Plot 
  jpeg(paste0(figurePath, "/Age75_84.jpg"), res=400, width=2500, height=3500)
  citySparkLines(age75Data, ncol=3, textSize=6, lineWidth=.5, lineColor='navy',
               plotTitle = 'Households Ages 75 - 84')
  dev.off(which=dev.cur())

## Extract age specific data 85+

  # Age 85+
  age85Data <- lapply(metroData, stripAgeData, ageFactor=8)
  names(age85Data) <- metroNames

  # Plot 
  jpeg(paste0(figurePath, "/Age85+.jpg"), res=400, width=2500, height=3500)
    citySparkLines(age85Data, ncol=3, textSize=6, lineWidth=.5, lineColor='navy',
               plotTitle = 'Households Ages 85+')
  dev.off(which=dev.cur())

### Overlapping SEs ---------------------------------------------------------------------

  d1 <- allData$plotData
  overlap <- ggplot(d1, aes(x=x,y=y, color=HouseholdAge)) + stat_smooth()+ ylab("Location Quotient") +
     xlab("Distance from CBD") +geom_point(position = "jitter", alpha = 0.3) 

  jpeg(paste0(figurePath, "/overlap.jpg"), res=400, width=6500, height=3500)
    overlap    
  dev.off(which=dev.cur())  

### Overlapping LMs ---------------------------------------------------------------------

  overlap2 <- ggplot(d1, aes(x=x,y=y, color=HouseholdAge)) 
  #overlap2 <- overlap2 + geom_point(alpha = 0.3)
  overlap2 <- overlap2 + stat_smooth(method="lm", geom="smooth", formula= y~x, size=.8, se=TRUE) + ylab("Location Quotient") +
    xlab("Distance from CBD") 
  
  jpeg(paste0(figurePath, "/overlapLM.jpg"), res=400, width=6500, height=3500)
    overlap2    
  dev.off(which=dev.cur()) 
  
###
  
pred.res <- hhPrediction(xData, 'All')

