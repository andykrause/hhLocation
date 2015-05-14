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
                        logScale=1,        # Amount to scale X axis by
                        smoothLines=TRUE,  # Should lines be smoothed?
                        smoothFactor=.1    # Smoothing factor (0 to 1)
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
  if(smoothLines){
     lqLines <- lapply(locQs, lowess, f=smoothFactor)
  } else {
     lqLines <- locQs
     for(lql in 1:length(locQs)){
       lqLines[[lql]] <- list(x=1:length(locQs[[lql]]),
                              y=as.numeric(locQs[[lql]]))
     }
  }
  
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
                           colorByAge=FALSE,     # Color the lines by age group?
                           addSmoothing=TRUE     # Add SE and Smoothed Line?
                           ){
  
  # Create Base Plot
  if(colorByAge) {
    xData$hhColor <- xData$HouseholdAge
  } else {
    xData$hhColor <- 1
  }
  
  # Create Base Plot
  basePlot <- ggplot(xData, aes(x=tX,y=y, color=hhColor)) +
  geom_line(size=1)
    
  # Add Lines
  if(addSmoothing){
    basePlot <- basePlot + stat_smooth(level=.9999, size=.8)
  }
  
  # Create 4x2 facets
  finalPlot <- basePlot + facet_wrap(~HouseholdAge, ncol=2) +
    
  # Add Axis  
  ylab("Location Quotient") +
  xlab("Distance from CBD") +
    
  # Add Title  
  ggtitle(titleName) +
  theme(plot.title = element_text(size=20, face="bold"),
        legend.position = 'none',
        panel.background =  element_rect(fill = "grey95", colour = NA))
  
  # Return to Function
  return(finalPlot)
}

### Extracts age data for a specfic age group --------------------------------------------

stripAgeData <- function(lqObj,            # Location Quotient Object from buildLQData
                         ageFactor         # Which age group (1-8)
){
  # Isolate plot data
  pData <- lqObj$plotData
  
  # Convert age names to factors
  pData$ageFact <- as.numeric(as.factor(pData$HouseholdAge))
  
  # Extract specific age group
  ppData <- subset(pData, ageFact == ageFactor) 
  
  # Return Data
  return(ppData)
}

### Creates city specific sparklines for a given age cohort ------------------------------

citySparkLines <- function(ageObj,            # Age specific object from StripAgeData()
                           ncol=1,            # # of columns
                           mPar=list(top=.125,
                                     bottom=.125,
                                     left=.1,
                                     right=.1,
                                     name=.2),  # Plot margin parameters
                           smoothLQs = TRUE,    # Use smoothed lines?
                           textSize=8,          # Size of annotation text
                           lineWidth=1.5,       # Size of spark line width
                           lineColor='black',   # Sparkline color
                           plotTitle = ''       # Plot Title
                           ){
  
  ## Calculate rows ,columns
  modu <- length(ageObj) %% ncol
  nrow <- length(ageObj) %/% ncol + ifelse(modu > 0, 1, 0)
  rMinus <- (nrow * ncol) - length(ageObj)
  
  offsets <- data.frame(X = sort(rep((ncol-1) :0, nrow)),
                        Y = rep((nrow-1) :0, ncol))
  if(modu > 0) offsets <- offsets[-((nrow(offsets)-rMinus+1):nrow(offsets)),]
  
  offsetList <- list()
  for(i in 1:nrow(offsets)){offsetList[[i]] <- offsets[i,]}
  
  ## Create scaled X,Ys
  xyScaled <- lapply(ageObj, scaleLocs, mPar=mPar)
  
  ## Located X,Ys
  xyLocated <- mapply(placeLocs, scaleData=xyScaled, offsets=offsetList,
                      MoreArgs=list(mPar=mPar)) 
  
  if(smoothLQs) xyLocated <- lapply(lapply(xyLocated, lowess, f=.2), as.data.frame)
  
  ## Build initial plot
  c4 <- data.frame(X=c(0, ncol, 0, ncol),
                   Y=c(0, 0, nrow, nrow))
  blankPlot <- ggplot(c4, aes(x=X,y=Y)) + geom_point(color='white')
  
  ## Add Lines
  
  for(i in 1: length(ageObj)){
    blankPlot <-  blankPlot + geom_line(data=xyLocated[[i]], aes(x=x, y=y), size=lineWidth,
                                        color=lineColor)  
  }
  
  ## Add City Names
  
  for(i in 1:length(ageObj)){
    blankPlot <-  blankPlot + annotate("text", 
                                       x = offsets$X[i] + mPar$left,
                                       y = offsets$Y[i] + .5,
                                       label = substr(names(ageObj)[[i]],1,3),
                                       size=textSize) 
  }
  
  ## Clean up for output
  
  finalPlot <- blankPlot + theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    plot.title = element_text(size=20, face="bold"),
    panel.background =  element_rect(fill = "grey95", colour = NA))
  
  ## Add Title
  
  finalPlot <- finalPlot + ggtitle(plotTitle)
  
  ## Return Values
  return(finalPlot)
  
}

### Helper function that scales the individual lines to fix inside the 'box' -------------

scaleLocs <- function(xyData, mPar){
  xAdj <- 1 - mPar$left - mPar$right - mPar$name
  yAdj <- 1 - mPar$top - mPar$bottom
  xScaled <- xyData$tX / max(xyData$tX)
  yScaled <- xyData$y / max(xyData$y)
  xy <- data.frame(x = xScaled * xAdj,
                   y = yScaled * yAdj)
  return(xy)
}

### Helper function that locates each of the spark lines on the larger plot --------------

placeLocs <- function(scaleData, offsets, mPar){
  xPlaced <- scaleData$x + mPar$left + mPar$name + offsets$X
  yPlaced <- scaleData$y + mPar$bottom + offsets$Y
  xy <- data.frame(x = xPlaced,
                   y = yPlaced)
  return(list(xy))
}




