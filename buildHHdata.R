
### FUNCTION USE EXAMPLE ###

if(F){
  
  # Set parameters
  dataDir <- 'd:/data/usa'
  codeDir <- 'c:/Dropbox/Research/PopDistPaper2/Code/hhLocation'
  cityListPath <- 'C:/Dropbox/Research/PopDistPaper2/Code/hhLocation/cityxylist.csv'
  outputPath <- 'c:/temp/cleandataexample.csv'

  # Source Files
  source(paste0(codeDir, '/buildHHData.R'))
  
  # Load Libraries
  library(plyr)
  library(dplyr)
  library(stringr)
  library(geosphere)
  
  # Set up list of cities to build data for  
  cityList <- read.csv(cityListPath, stringsAsFactors=FALSE) 
  quickTest <- cityList[c(51, 57:60, 65, 66), ]  # Select Seattle, Richmond and Salt Lake City 
  
  # Create Data
  createHHLocData(cityList=quickTest, dataDir=dataDir,
                  codeDir=codeDir, outputPath=outputPath, fullClean=TRUE)
  
}

### MASTERFUNCTION #######################################################################

### Function that controls all aspects of building the HH Data ---------------------------

createHHLocData <- function(cityList,                   # List of cities to analyze
                            dataDir,                    # Main directory
                            codeDir,                    # Directory where code is
                            outputPath,                 # where to output clean data
                            verbose=TRUE,               # Provide status updates
                            fullClean=TRUE              # Remove ZIPs after use
                            ){
 
 ## Set libraries
  
  require(stringr)
  require(plyr)
  require(dplyr)
  require(geosphere)
  
 ## Set global variables
  
  gv <- list(dataDir=dataDir,
             codeDir=codeDir,
             verbose=verbose,
             fullClean=fullClean)
  assign('gv', gv, envir = .GlobalEnv)
  
 ## Confirm that necessary raw data is present, if not acquire it
  
  confirmRawData(cityList)
  
  # Read in all state data
  dataList <- list()
  if(verbose) cat('\nMerging Geographic data to P22 data......')
  dataList <- lapply(as.list(dataStates), 
                     mergeGeoP22, 
                     cbsaList=cbsaList)
 
  # Merge all data
  dataList <- rbind.fill(dataList)
  if(verbose) cat('\nCombining merged data into single data frame.....')
  
  # Separate by CBSA code
  dataList <- split(dataList, as.character(dataList$cbsa))
  
  # Calc distances to each center (what to do about multiples?)
  if(verbose) cat('\nCalculating Distances to Center Points...')
  dataList <- lapply(dataList, hhDistCalc, 
                     cityList=cityList, cbsaList=cbsaList)
 
  # Write out as clean, final data
  cleanData <- rbind.fill(dataList)
  write.csv(cleanData, outputPath, row.names=F)
  if(verbose) cat('\nPrepared data written to', outputPath)
 
}

### confirms all raw census data in in place, if not it gets it --------------------------

confirmRawData <- function(cityList,                # List of cities to analyze
                           dataDir=gv$dataDir,      # Location of data
                           codeDir=gv$codeDir,      # Location of code
                           verbose=gv$verbose       # Show helpful commments?
                           ){
    
 ## Find the states that are needed
  
  # Read in State List
  stateList <- read.csv(paste0(codeDir, '/stateList.csv'), stringsAsFactors=FALSE)
  
  # Send this list to the global environment
  assign("stateList", stateList, envir = .GlobalEnv)
    
  # Read in CBSA List
  loadCBSACodes()
  
  # Turn city DF into a row-wise list for lapply()
  cityListX <- list()
  for(i in 1:nrow(cityList)){cityListX[[i]] <- cityList[i, ]}
  
  # Develop the cbsa codes
  cbsaList <- rbind.fill(lapply(cityListX, matchCityCBSA, cbsaCodes=cbsaCodes))
  
  # Send this list to the global environment
  assign("cbsaList", cbsaList, envir = .GlobalEnv)
  
  # Create a list of all states from which we'll need data
  dataStates <- sort(unique(unlist(lapply(cbsaList$stateNames, buildDataStates))))
  if(verbose) cat('\nDeveloping list of states...Checking for raw data')

 ## Check to see which data is missing   
  
  # convert to lower
  dataStates <- tolower(dataStates)
  
  # Send this list to the global environment
  assign("dataStates", dataStates, envir = .GlobalEnv)
  
  # check if data exists
  dataStatus <- unlist(lapply(as.list(dataStates), checkStateData))
  missingData <- dataStates[which(!dataStatus)]
  cat(missingMessage(missingData))  
  
  ## Get remainder of missing data
  
  if(length(missingData) >= 1){
    for(mD in 1:length(missingData)){
      
      # Find out full state name
      mdState <- stateList$state[which(stateList$stateAbbr == toupper(missingData[mD]))]
      if(verbose) cat("Gathering data for", mdState)
      
      # Get Data
      buildSF1Data(state=mdState, stAbbr=missingData[mD])
    }
  }
  
  ## Give notice
  
  if(verbose) cat('\n\n***Data acquisition complete.***\n\n')
  
}   

### Loads in the CBSA data and codes -----------------------------------------------------

loadCBSACodes <- function(dataDir=gv$dataDir,      # Main data directory
                          verbose=gv$verbose       # Give progress?
                          ){
  
  # Check to see if raw data exists
  fetchCBSACodes()
  
  # Load data into memory
  cbsaPath <- paste0(dataDir, '/censusinfo/cbsainfo/cbsacodes.csv')
  cbsa <- read.csv(cbsaPath)
  if(verbose) cat('CBSA codes loaded.\n')
  
  # Send this list to the global environment
  assign("cbsaCodes", cbsa, envir = .GlobalEnv)

}

### Builds and cleans CBSA code file from census -----------------------------------------

fetchCBSACodes <- function(dataDir=gv$dataDir,       # Main directory for all census data
                           verbose=gv$verbose        # Provide status reports?
                           ){
  
  ## Create Dir if not there
  
  censPath <- paste0(dataDir, '/censusInfo')
  dir.create(censPath, showWarnings = FALSE)
  cbsaPath <- paste0(censPath, '/cbsaInfo')
  dir.create(cbsaPath, showWarnings = FALSE)
  
  # Create file location
  dlPath <- paste0(cbsaPath, '/cbsacodes.csv')
  
  if(!file.exists(dlPath)){
    
    ## Download File
    
    downPath <- paste0('https://www.census.gov/popest/data/metro/totals/2011/tables/',
                       'CBSA-EST2011-01.csv')
    
    download.file(url=downPath, destfile=dlPath)
    if(verbose) cat('CBSA codes successfully downloaded.\n')
    
    ## Read in CBSA codes
    
    # Read in
    cbsa <- read.csv(dlPath, header=F, stringsAsFactors=FALSE)
    
    # Remove excess rows and columns
    cbsa <- cbsa[-c(1:6, 980:985), 1:4]
    names(cbsa) <- c('code', 'subcode','name', 'pop2010')
    
    # Separate City and State
    commaLoc <- str_locate(cbsa$name, ',')
    cbsa$city <- substr(cbsa$name, 1, commaLoc[, 1] - 1)
    cbsa$ST <- substr(cbsa$name, commaLoc[, 1] + 1, 100)
    cbsa$name <- NULL
    
    ## Write file back out
    
    write.csv(cbsa, dlPath, row.names=F)
    if(verbose) cat('CBSA codes clean and written out to.', dlPath, '\n')
    
  } else {
    
    if(verbose) cat('CBSA file already exists.\n')
    
  }
} # closes function

### Match the city list to its corresponding MSA codes -----------------------------------

matchCityCBSA <- function(iCity,            # Individual City names
                          cbsaCodes          # Full CBSA data object
                          ){
  
  ## Match based on City Name
  
  cityLoc <- as.data.frame(str_locate(cbsaCodes$city, iCity$name))
  cityMatch <- which(!is.na(cityLoc$start))
  
  ## Generate Codes
  
  # If no city name match
  if(length(cityMatch) == 0){
    cityCode <- 'No Matching City'
    stateNames <- NA
  }
  
  # If only one city name match
  if(length(cityMatch) == 1){
    cityCode <- cbsaCodes$code[cityMatch]
    stateNames <- cbsaCodes$ST[cityMatch]
  }
  
  # If more than one city name match
  if(length(cityMatch) > 1){
    
    # Multiple states found
    stateLoc <- as.data.frame(str_locate(cbsaCodes$ST, iCity$state))
    stateMatch <- which(!is.na(stateLoc$start))
    cityStateMatch <- cityMatch[cityMatch %in% stateMatch]
    
    # If one state matches
    if(length(cityStateMatch) == 1){
      cityCode <- cbsaCodes$code[cityStateMatch]
      stateNames <- cbsaCodes$ST[cityStateMatch]
    } else {
      
      # If duplicate is a sub-MSA
      if(length(table(cbsaCodes$code[cityStateMatch])) == 1){
        cityCode <- cbsaCodes$code[cityStateMatch[1]]
        stateNames <- cbsaCodes$ST[cityStateMatch[1]]
      } else {  
        
        # If no states match
        cityCode <- 'Invalid City State Combination'
        stateNames <- NA
      }
    }
  } # closes if(length(cityMatch) > 1) condition
  
  ## Return city code
  
  return(data.frame(cbsaCode = cityCode,
                    stateNames = stateNames))
} 

### Build a list of all states from which data is needed due to cityList -----------------

buildDataStates <- function(stObj            # A state code from the cbsaCodes obj
                            ){
  
  # Set up blank object
  iST <- NULL
  
  # Remove any spaces
  stObj <- str_replace_all(stObj, " ", "")
  
  # Find '-'s indicates multi-state CBSA
  dashLoc <- as.data.frame(str_locate_all(stObj, "-"))$start
  
  # If single state CBSA
  if(length(dashLoc) == 0){
    iST <- stObj
    
  # If Multistate CSBA, then pull apart object  
  } else {
    dashLoc <- c(1, dashLoc, nchar(stObj))
    for(i in 1:(length(dashLoc) - 1)){
      iST <- c(iST, str_replace_all(substr(stObj, dashLoc[i], dashLoc[i + 1]), "-", ""))
    }
  }
  
  ## Return object  
  return(iST)
}

### Checks to see which of the data are present, return list of missing ------------------

checkStateData <- function(iState,                # State abbrev to check for data
                           dataDir=gv$dataDir     # Main directory
                           ){
  
  # Set initial Condition
  dataPresent <- TRUE
  
  # Test for geo data
  if(!file.exists(paste0(dataDir, '/', iState, '/census/2010/sf1/geo.csv'))){
    dataPresent <- FALSE
  }
  
  # Test for P22Data
  if(!file.exists(paste0(dataDir, '/', iState, '/census/2010/sf1/p22Data.csv'))){
    dataPresent <- FALSE
  }
  
  # Return finding
  return(dataPresent)
}  

### Produces message giving state of data acquisition needs ------------------------------

missingMessage <- function(missingData            # Vector of states with missing data
                           ){
  
  # Build Message
  if(length(missingData) > 0){
    mdMessage <- "\n\nData is missing from: "
    for(i in 1:length(missingData)){
      if(i != length(missingData)){
        mdMessage <- paste0(mdMessage, 
                            stateList$state[which(stateList$stateAbbr == 
                                                    toupper(missingData[i]))], ', ')
      } else {
        mdMessage <- paste0(mdMessage, 'and ', 
                            stateList$state[which(stateList$stateAbbr == 
                                                    toupper(missingData[i]))])
      }
    }
    
    mdMessage <- paste0(mdMessage, '\n\n***Missing data will be acquired now.',
                        'This may take awhile.***\n')
  } else {
    mdMessage <- "\n\nAll data is present and data preparation can proceed"
  }
  
  # Return message
  return(mdMessage)
}   

### Build the necessary data (geo and P22) for HH x Age analysis -------------------------

buildSF1Data <- function(state,                  # Full state name (capitalized 1st let.)
                         stAbbr,                 # 2 letter state abbrev.
                         dataDir=gv$dataDir,     # Base directory to build state data in
                         verbose=gv$verbose,     # Show comments and completion notices?
                         fullClean=gv$fullClean  # Delete ZIP file after use?
){
  
 ## Obtain raw data
  
  # Force to lower
  stAbbr <- tolower(stAbbr)
  
  # Download file if necessary
  if(!file.exists(paste0(dataDir, '/', stAbbr, '/census/2010/sf1/raw/', stAbbr,
                         'geo2010.sf1'))){
    downloadSF1(state, stAbbr)
  } else {
    if(verbose) cat (state, 'raw data files already exist.\n')
  }
  
  # Build file location for future use
  fileDir <- paste0(dataDir, '/', stAbbr, '/census/2010/sf1')
  
  ## Build out Geographic Data
  
  if(!file.exists(paste0(fileDir, '/geo.csv'))){
    
    # Extract Geographic Data
    geoData <- extractGeoData(fileDir, stAbbr)
    if(verbose) cat(state, 'geography generated.\n')
    
    # Write out geoData
    write.csv(geoData, paste0(fileDir, '/geo.csv'), row.names=F)
    if(verbose) cat(state, 'geography written to file.\n')
    
  } else {
    
    if(verbose) cat (state, 'finalized geo file already exists.\n')  
    
  }
  
  ## Household Data
  
  if(!file.exists(paste0(fileDir, '/p22Data.csv'))){
    
    # Extract Household Data
    hhData <- extractHHData(fileDir, stAbbr)
    if(verbose) cat(state, 'household data (P22) generated.\n')
    
    # Write out Household
    write.csv(hhData, paste0(fileDir, '/p22Data.csv'), row.names=F)
    if(verbose) cat(state, 'household data written to file.\n')
    
  } else {
    
    if(verbose) cat (state, 'finalized household data (P22) file already exists.\n')  
    
  }
} # closes function 

### Builds dir structure, downloads and unpacks necessary sf1 data for HHAge analysis

downloadSF1 <- function(state,                   # full state name
                        stAbbr,                  # 2 letter state abbr
                        dataDir=gv$dataDir,      # Main directory destination 
                        verbose=gv$verbose,      # Show messages?
                        cleanUp=gv$fullClean     # Remove .zip after extraction
                        ){
 
 ## Set up locations
  
  # Force state abbreviation to lower
  stAbbr <- tolower(stAbbr)
  
  # Fix spaces in state names
  state <- str_replace_all(state, ' ', '_')
  
  # Create directory locations
  if(verbose) cat('...building directory structure...\n')
  dirPath <- paste0(dataDir, '/', stAbbr)
  dir.create(dirPath, showWarnings = FALSE)
  dirPath <- paste0(dirPath, '/census')
  dir.create(dirPath, showWarnings = FALSE)
  dirPath <- paste0(dirPath, '/2010')
  dir.create(dirPath, showWarnings = FALSE)
  dirPath <- paste0(dirPath, '/sf1')
  dir.create(dirPath, showWarnings = FALSE)
  dirPathRaw <- paste0(dirPath, '/raw')
  dir.create(dirPathRaw, showWarnings = FALSE)
  
 ## Download Census Zip File
  
  # Build path to file on census FTP
  downPath <- paste0('http://www2.census.gov/census_2010/04-Summary_File_1/',
                     state, '/', stAbbr, '2010.sf1.zip')
  
  # Create name for local zip
  destPath <- paste0(dirPathRaw, '/sf1.zip' )
  
  # Download file
  download.file(url=downPath, destfile=destPath)
  if(verbose) cat(state, 'raw zip file successfully downloaded.\n')
  
 ## Unpack zip file              
  
  unzip(destPath, exdir=dirPathRaw,
        files = c(paste0(stAbbr, 'geo2010.sf1'),
                  paste0(stAbbr, '000052010.sf1')))
  if(verbose) cat('2', state, 'files successfully unpacked.\n')
  
 ## Cleanup (remove .zip to save space)
  
  if(cleanUp){
    file.remove(destPath)
    if(verbose) cat(state, 'zip file successfully deleted.\n')    
  }
} # closes function

### Extracts necessary geographic identifiers from geo data + writes to file -------------

extractGeoData <- function(fileDir,              # Directory where geodata will live
                           stAbbr                # 2 letter state abbreviation
                           ){
  
  ## Read in Raw data  
  geo <- read.csv(paste0(fileDir, '/raw/', stAbbr, 'geo2010.sf1'), header=F,
                  stringsAsFactors=FALSE)
  
  ## Prepare data
  
  # Name Field
  names(geo) <- 'main'
  
  # Build geo IDs from main field
  geo$geoLevel <- substr(geo$main, 9, 11)
  geo$geoID <- substr(geo$main, 19, 67) 
  geo$sfID <- substr(geo$geoID, 1, 7)
  geo$blockID <- ifelse(geo$geoLevel == '101', substr(geo$geoID, 36, 49), 0)
  
  # Trim down to make more managable
  geo$remain <- substr(geo$main, 68, 1000)
  
  # Pull out Lat Long and CBSA designation
  geo$latlong <- substr(geo$remain,270, 294)
  geo$lat <- as.numeric(substr(geo$latlong, 2, 9))
  geo$long <- -as.numeric(substr(geo$latlong, 13, 23))
  geo$cbsa <- substr(geo$remain, 46, 50)
  
  ## Trim to final dataset  
  
  geoOut <- geo[ ,c('geoLevel', 'sfID', 'blockID', 'cbsa', 'lat', 'long')]
  
  ## Return
  
  return(geoOut)
  
} # closes function

### Extracts necessary hh x age (P22) data + writes to file ------------------------------

extractHHData <- function(fileDir,               # Directory where p22data will live
                          stAbbr                 # 2 letter state abbreviation
                          ){ 
  
  ## Read in raw data
  
  hhData <- read.csv(paste0(fileDir, '/raw/', stAbbr, '000052010.sf1'), 
                     header=F, stringsAsFactors=FALSE)
  
  ## Trim to necessary fields  
  
  hhData <- hhData[ ,c(5, 123:142)]
  
  ## Merge family and non-family households
  
  hhFinalData <- hhData[ ,1:11]
  hhFinalData[ ,2:11] <- hhData[ ,2:11] + hhData[ ,12:21] 
  
   # Deal with 55-59/60-64 issues
  hhExtra <- hhFinalData[,7] + hhFinalData[,8]
  hhFinalData[,7] <- hhExtra
  hhFinalData <- hhFinalData[,-8]
  
  ## Rename fields
  
  names(hhFinalData) <- c('sfID', 'hhSum','hh1524','hh2534','hh3544','hh4554','hh5564',
                     'hh6574','hh7584','hh85p')
  ## Return data
  
  return(hhFinalData)
  
} # closes function

### Trims to block level geographies, merges GEO and P22 Data ----------------------------

mergeGeoP22 <- function(iState,                  # 2 letter state abbreviation code
                        cbsaList                # Full list of cbsaCodes
                        ){
  
  # Force to lower
  iState <- tolower(iState)
  
  # Read in Geo Data  
  geoData <- read.csv(paste0(gv$dataDir, '/', iState, '/census/2010/sf1/geo.csv'),
                      header=T, stringsAsFactors=FALSE)
  
  # Read in household data
  hhData <- read.csv(paste0(gv$dataDir, '/', iState, '/census/2010/sf1/p22Data.csv'),
                     header=T, stringsAsFactors=FALSE)  
  
  # Match all CBSA codes in that state
  stateMatch <- as.data.frame(str_locate(cbsaList$stateNames, 
                                         toupper(iState)))$start
  iCbsa <- cbsaList$cbsaCode[which(!is.na(stateMatch))]
  
  ## Limit Geo list
  
  # By block level only
  blockGeos <- subset(geoData, geoLevel == '101')
  
  # By CBSA 
  blockGeos <- subset(blockGeos, cbsa != "")
  blockGeos <- subset(blockGeos, cbsa %in% iCbsa)
  
  ## Join to HH Data
  
  # Convert sfID to numeric
  blockGeos$sfID <- as.numeric(blockGeos$sfID)
  blockHHData <- merge(blockGeos, hhData, by='sfID')
  
  ## Return
  return(blockHHData)
  
}


### Calculated distances for each CBSA data group ----------------------------------------

hhDistCalc <- function(dataObj,               # A DF of data for one CBSA
                       cityList,              # Full list of cities
                       cbsaList               # Full List of CBSA Codes
                       ){
  
  # Ensure that city names are in lower case
  names(cityList) <- tolower(names(cityList))
  
  # Grab city which matches the dataObj
  iC <- which(cbsaList$cbsaCode == dataObj$cbsa[1])
  
  if(length(iC) == 1){
    
    # Select City
    iCity <- cityList[iC[1], ]
  
    # Calculate distances (in Miles)
    dists <- sqrt(((dataObj$lat - iCity$lat)^2) + ((dataObj$long) - iCity$long)^2)*69
  
    # Add Distances and city name to dataObj
    dataObj$dists <- dists
    dataObj$subDists <- dists
    dataObj$cityName <- iCity$name
    dataObj$subName <- 'none'
  
  } else {
    
    # Build Empty matrix
    distMatrix <- matrix(ncol=length(iC), nrow=nrow(dataObj))
    
    # Calc all distances
    for(dM in 1:length(iC)){
      iCity <- cityList[iC[dM],]
      distMatrix[,dM] <- dists <- sqrt(((dataObj$lat - iCity$lat)^2) + 
                                     ((dataObj$long) - iCity$long)^2)*69
      
    }
    distMatrix <- as.data.frame(distMatrix)
    colnames(distMatrix) <- cityList$subcentername[iC]
    
    # Assign values
    dataObj$dists <- distMatrix$none
    dMins <- apply(distMatrix,1,function(x) which(x==min(x)))
    ordMatrix <- apply(distMatrix, 1, sort)
    dataObj$subDists <- ordMatrix[1,]
    dataObj$cityName <- iCity$name[1]
    dataObj$subName <- names(distMatrix)[dMins]
  }  
    
  # Return data Obj
  return(dataObj)
  
}





