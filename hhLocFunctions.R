##########################################################################################
#
#  This file contains the custom functions necessary for the Household Location Study
#  by (Names withheld for Peer Review)
#
##########################################################################################

### Distance based location quotient functions -------------------------------------------
  #  Calculates the proportion of the whole for one field compared to the
  #  Proportion of the whole for another field over space

  distLQ <- function(field1,      # "Numerattor" field
                     xData,       # Data.frame containing fields and locations 
                     field2,      # "Denominator" field
                     xLocation    # Field containing the locational indicators
                     ){
    
    # Isolate the various fields
    f1Data <- xData[ ,field1]
    f2Data <- xData[ ,field2]
    lData <- xData[ ,xLocation]
    
    # Calculate its location quotient
    (tapply(f1Data, lData, sum) / tapply(f2Data, lData, sum)/
       (sum(f1Data) / sum(f2Data))) 
  }

### Function that creates location quotient data ----------------------------------------

buildLQData <- function(yData,             # Data.frame of all data
                        metroName='All',   # Name of Metro Area
                        logScale=1         # Amount to scale X axis by
                        ){
  
  # Select out specific data
  if(metroName == "All"){
    mData <- yData
  } else {
    mData <- subset(yData, NameAlt==metroName)
  }
  
  # Extract age groups
  pNames <- names(mData)[grep("pop", names(mData))]
  
  # Calculate initial LQs
  locQs <- lapply(pNames, distLQ, xData=mData, field2='TotHHLD', xLocation='distNorm')
  
  # Build lines
  lqLines <- lapply(locQs, lowess, f=sFactor)
  
  # Set up as logaritmic type scale
  Xs <- (1:length(locQs[[1]])) ^ (logScale)
  Xl <- length(Xs)
  
  # Build plotting data
  plotData <- rbind.fill(lapply(lqLines,as.data.frame))
  plotData$tX <- rep(Xs, length(lqLines))
  plotData$HouseholdAge <- c(rep("Ages 15-24",length(lqLines[[1]]$y)),
                             rep("Ages 25-34",length(lqLines[[1]]$y)),
                             rep("Ages 35-44",length(lqLines[[1]]$y)),
                             rep("Ages 45-54",length(lqLines[[1]]$y)),
                             rep("Ages 55-64",length(lqLines[[1]]$y)),
                             rep("Ages 65-74",length(lqLines[[1]]$y)),
                             rep("Ages 75-84",length(lqLines[[1]]$y)),
                             rep("Ages 85+",length(lqLines[[1]]$y)))
  
  # Return data to function
  return(list(plotData = plotData,
              LQs = locQs,
              lqLines = lqLines))
}

### Build ggPlot function  -----------------------------------------------------------------

buildAgeLQPlot <- function(xData,                # a plotData object from buildLQData
                           titleName="",         # Title of the plot
                           colorByAge=FALSE      # Color the lines by age group?
                           ){
  
  # Create Base Plot
  if(colorByAge) {
    xData$hhColor <- xData$HouseholdAge
  } else {
    xData$hhColor <- 1
  }
  
  # Create Base Plot
  basePlot <- ggplot(xData, aes(x=tX,y=y, color=hhColor)) +
    
  
  # Add Lines
  geom_line() + 
    
  # Add Smoothing Line  
  stat_smooth(level=.9999, size=1)
  
  # Create 4x2 facets
  finalPlot <- basePlot + facet_wrap(~HouseholdAge, ncol=2) +
    
  # Add Axis  
  ylab("Location Quotient") +
  xlab("Distance from CBD") +
    
  # Add Title  
  ggtitle(titleName) +
  theme(plot.title = element_text(size=20, face="bold"),
        legend.position = 'none')
  
  # Return to Function
  return(finalPlot)
}

