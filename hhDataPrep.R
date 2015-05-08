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

 # Load in Basic Data  (will automatically choose path based on computer)

  dataPath <- ifelse(System$getHostname() != 'ANDYKRAUSEPC',
                     "/Users/hestiri/Dropbox/papers/PopDistPaper2/Data/",
                     "c:/dropbox//research//popdistpaper2/data/")

  allRaw <- read.csv(paste0(dataPath, "newdata2015.csv"),
                     header=T, stringsAsFactors=FALSE)

### Data Cleaning process   --------------------------------------------------------------

 ## Remove Rural Blocks and those with no population
  
  urbData <- allRaw[allRaw$URB == 1 & allRaw$Tot_HHLD != 0, ]
  
 ## Trim to necessary fields

  necFields <- c("G_ID", "pop_15_24", "pop_25_34", "pop_35_44", "pop_45_54", "pop_55_64",
                 "pop_65_74","pop_75_84", "pop_85.","Tot_HHLD", "Name_Alt",
                 "Raw_Distance")

  urbData <- urbData[ ,necFields]

  ## Remove the six problem cities

  probCities <- c("Hartford", "Providence", "Las Vegas", "Louisville",
                  "Phoenix", "San Antonio")

  inProb <- which(as.character(urbData$Name_Alt) %in% probCities)

  if(length(inProb) > 0) urbData <- urbData[-inProb, ]
  
 ## Recalculate normalized distance
  
  # Build function to calc norm dist
  normDist <- function(city, data){
    cData <- data[data$Name_Alt == city, ]
    cData$distNorm <- cData$Raw_Distance / max(cData$Raw_Distance)
    return(cData)
  }  

  # Calc normalized distance
  cNames <- names(table(urbData$Name_Alt))
  xData <- lapply(cNames, normDist, data=urbData)
  xData <- rbind.fill(xData)
  
  # Round and convert to miles

  xData$distNorm <- round(xData$distNorm, 2)
  xData$distMiles <- xData$distNorm / 5280

 ## Take underscore out of names

  names(xData) <- gsub("_", "", names(xData))

 ## Write out cleaned data

  write.csv(xData, paste0(dataPath, "cleanData.csv"), row.names=FALSE)

##########################################################################################

