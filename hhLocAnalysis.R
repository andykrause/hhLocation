##########################################################################################
#                                                                                        #
#  Code to prep data for analysis  (loading and clipping taks a long time  )             #
#                                                                                        #
##########################################################################################

### Preliminary commands -----------------------------------------------------------------

 ## Load Libraries

  library(ggplot2)
  library(reshape)
  library(R.utils)
  library(plyr)
  library(dplyr)

 ## Parameters

  # Operational Parameters
  reBuildData <- FALSE  ## Do you want to rebuild the data??

  # Plotting parameters
  wMult <- 240 
  sFactor <- .02
  wCol <- c("chartreuse4", "darkorange", "darkorange4", "firebrick1", "gold",
            "deepskyblue1","dodgerblue3","darkorchid")

 ## Load data and helper files 

  homePath <- ifelse(System$getHostname() != 'ANDYKRAUSEPC',
                     "/Users/hestiri/Dropbox/papers/PopDistPaper2",
                     "c:/dropbox//research//popdistpaper2")

  # Source custom functions

  source(paste0(homePath, '/code/hhlocation/hhLocFunctions.R'))

  #  Load in Basic Data  (will automatically choose path based on computer)

  if(reBuildData) source(paste0(homePath, "/code/hhlocation/hhDataPrep.R"))
  xData <- read.csv(file.path(homePath, '/data/cleanData.csv'))

  # Fix names
  xData$NameAlt <- as.character(xData$NameAlt)
  xData$NameAlt[xData$NameAlt == 'New Orleans'] <- "NO"
  xData$NameAlt[xData$NameAlt == 'New York'] <- "NY"
  xData$NameAlt[xData$NameAlt == 'San Diego'] <- "SD"
  xData$NameAlt[xData$NameAlt == 'San Francisco'] <- "SF"

### Create all Metros Plot ---------------------------------------------------------------

 ## Build data and plots

  allData <- buildLQData(xData, metroName='All', logScale=5/9, smoothLines=FALSE)
  allPlot <- buildAgeLQPlot(allData$plotData, title='All Metros', colorByAge=F,
                            addSmoothing=FALSE)

 ## Export Plots
  
  jpeg(paste0(homePath, "/graphs/All.jpg"), res=400, width=2500, height=3500)
    allPlot    
  dev.off(which=dev.cur())  


### Create metro examples  ---------------------------------------------------------------

 ## Build data and plots
 
  # Boston
  bosData <- buildLQData(xData, metroName='Boston', logScale=5/9)
  bosPlot <- buildAgeLQPlot(bosData$plotData, title='Boston Metro', colorByAge=F)

  # Seattle
  seaData <- buildLQData(xData, metroName='Seattle', logScale=5/9)
  seaPlot <- buildAgeLQPlot(seaData$plotData, title='Seattle Metro', colorByAge=F)

 ## Export Plots

  jpeg(paste0(homePath, "/graphs/Boston.jpg"), res=400, width=2500, height=3500)
    bosPlot    
  dev.off(which=dev.cur())  

  jpeg(paste0(homePath, "/graphs/Seatte.jpg"), res=400, width=2500, height=3500)
    seaPlot    
  dev.off(which=dev.cur())  

### Compare by age group -----------------------------------------------------------------

 ## Extra data for all metros

  metroNames <- names(table(xData$NameAlt))

  metroData <- lapply(metroNames, buildLQData, yData=xData, logScale=5/9)

 ## Extract age specific data 15-24

  # Age 15-24
   age15Data <- lapply(metroData, stripAgeData, ageFactor=1)
   names(age15Data) <- metroNames

  # Plot 
  jpeg(paste0(homePath, "/graphs/Age15_24.jpg"), res=400, width=2500, height=3500)
    citySparkLines(age15Data, ncol=3, textSize=6, lineWidth=1.3, lineColor='navy',
                   plotTitle = 'Households Ages 15 - 24')
  dev.off(which=dev.cur())

  # Age 75-24
  age75Data <- lapply(metroData, stripAgeData, ageFactor=7)
  names(age75Data) <- metroNames

  # Plot 
  jpeg(paste0(homePath, "/graphs/Age175_84.jpg"), res=400, width=2500, height=3500)
    citySparkLines(age75Data, ncol=3, textSize=6, lineWidth=1.3, lineColor='navy',
               plotTitle = 'Households Ages 75 - 84')
  dev.off(which=dev.cur())

 ## Extract age specific data 85+

  # Age 85+
  age85Data <- lapply(metroData, stripAgeData, ageFactor=8)
  names(age85Data) <- metroNames

  # Plot 
  jpeg(paste0(homePath, "/graphs/Age85+.jpg"), res=400, width=2500, height=3500)
    citySparkLines(age85Data, ncol=3, textSize=6, lineWidth=1.3, lineColor='navy',
                   plotTitle = 'Households Ages 85+')
  dev.off(which=dev.cur())


### Overlapping SEs

  d1 <- allData$plotData
  overlap <- ggplot(d1, aes(x=x,y=y, color=HouseholdAge)) + geom_line(size=1) + stat_smooth()
  jpeg(paste0(homePath, "/graphs/overlap.jpg"), res=400, width=6500, height=3500)
    overlap    
  dev.off(which=dev.cur())  



