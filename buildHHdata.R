
### MAIN FUNCTION USE EXAMPLE ###

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
  
  # Set up list of cities to build data for  
  cityList <- read.csv(cityListPath, stringsAsFactors=FALSE) 
  quickTest <- cityList[c(15, 44, 50), ]  # Select Seattle, Richmond and Salt Lake City 
  
  # Create Data
  createHHLocData(dataDir, codeDir, quickTest, outputPath=outputPath)
  
}

### Build the necessary data (geo and P22) for HH x Age analysis -------------------------

buildSF1Data <- function(mainDir,                # Base directory to build state data in
                         state,                  # Full state name (capitalized 1st let.)
                         stAbbr,                 # 2 letter state abbrev.
                         verbose=TRUE,           # Show comments and completion notices?
                         fullClean=FALSE         # Delete ZIP file after use?
){
  
  require(stringr)
  
  ## Obtain raw data
  
  # Force to lower
  stAbbr <- tolower(stAbbr)
  
  # Download file if necessary
  if(!file.exists(paste0(mainDir, '/', stAbbr, '/census/2010/sf1/raw/', stAbbr,
                         'geo2010.sf1'))){
    downloadSF1(mainDir, state, stAbbr, verbose)
  } else {
    if(verbose) cat (state, 'raw data files already exist.\n')
  }
  
  # Build file location for future use
  fileDir <- paste0(mainDir, '/', stAbbr, '/census/2010/sf1')
  
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

downloadSF1 <- function(mainDir,                 # Main directory destination 
                        state,                   # full state name
                        stAbbr,                  # 2 letter state abbr
                        verbose=TRUE,            # Show messages?
                        cleanUp=TRUE             # Remove .zip after extraction
                        ){
 
 ## Set up locations
  
  # Force state abbreviation to lower
  stAbbr <- tolower(stAbbr)
  
  # Fix spaces in state names
  state <- str_replace_all(state, ' ', '_')
  
  # Create directory locations
  if(verbose) cat('...building directory structure...\n')
  dirPath <- paste0(mainDir, '/', stAbbr)
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

### Builds and cleans CBSA code file from census -----------------------------------------

getMSACodes <- function(mainDir,                 # Main directory for all census data
                        verbose=TRUE             # Provide status reports?
                        ){
  
  ## Create Dir if not there
  censPath <- paste0(mainDir, '/censusInfo')
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
                          cbsaData          # Full CBSA data object
){
  
  ## Match based on City Name
  
  cityLoc <- as.data.frame(str_locate(cbsaData$city, iCity$City))
  cityMatch <- which(!is.na(cityLoc$start))
  
  ## Generate Codes
  
  # If no city name match
  if(length(cityMatch) == 0){
    cityCode <- 'No Matching City'
    stateNames <- NA
  }
  
  # If only one city name match
  if(length(cityMatch) == 1){
    cityCode <- cbsaData$code[cityMatch]
    stateNames <- cbsaData$ST[cityMatch]
  }
  
  # If more than one city name match
  if(length(cityMatch) > 1){
    
    # Multiple states found
    stateLoc <- as.data.frame(str_locate(cbsaData$ST, iCity$ST))
    stateMatch <- which(!is.na(stateLoc$start))
    cityStateMatch <- cityMatch[cityMatch %in% stateMatch]
    
    # If one state matches
    if(length(cityStateMatch) == 1){
      cityCode <- cbsaData$code[cityStateMatch]
      stateNames <- cbsaData$ST[cityStateMatch]
    } else {
      
      # If duplicate is a sub-MSA
      if(length(table(cbsaData$code[cityStateMatch])) == 1){
        cityCode <- cbsaData$code[cityStateMatch[1]]
        stateNames <- cbsaData$ST[cityStateMatch[1]]
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

makeDataStates <- function(stObj            # A state code from the cbsaCodes obj
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
    dashLoc <- c(1, dashLoc)
    for(i in 1:(length(dashLoc) - 1)){
      iST <- c(iST, str_replace_all(substr(stObj, dashLoc[i], dashLoc[i + 1]), "-", ""))
    }
  }
  
  ## Return object  
  return(iST)
}


### confirms all raw census data in in place, if not it gets it --------------------------

confirmRawData <- function(cityList,                # List of cities to analyze
                           mainDir,                 # Main directory location
                           codeDir,                 # Location of code
                           verbose=TRUE             # Show helpful commments?
){
  
  require(stringr)
  require(plyr)
  require(dplyr)
  
  ## Find the states that are needed
  
  # Read in State List
  stateList <- read.csv(paste0(codeDir, '/stateList.csv'), stringsAsFactors=FALSE)
  
  # Read in CBSA List
  cbsaList <- loadCBSAInfo(mainDir)
  
  # Turn city DF into a row-wise list for lapply()
  cityListX <- list()
  for(i in 1:nrow(cityList)){cityListX[[i]] <- cityList[i,]}
  
  # Develop the cbsa codes
  cbsaCodes <- rbind.fill(lapply(cityListX, matchCityCBSA, cbsaData=cbsaList))
  
  # Send this list to the global environment
  assign("cbsaCodes", cbsaCodes, envir = .GlobalEnv)
  
  # Create a list of all states from which we'll need data
  dataStates <- sort(unique(unlist(lapply(cbsaCodes$stateNames, makeDataStates))))
  
  ## Check to see which data is missing  
  
  # convert to lower
  dataStates <- tolower(dataStates)
  
  # Send this list to the global environment
  assign("listOfStates", dataStates, envir = .GlobalEnv)
  
  # check if data exists
  dataStatus <- unlist(lapply(as.list(dataStates), checkStateData, mainDir=mainDir))
  missingData <- dataStates[which(!dataStatus)]
  cat(missingMessage(missingData, stateList))  
  
  ## Get remainder of missing data
  
  if(length(missingData) >= 1){
    for(mD in 1:length(missingData)){
      
      # Find out full state name
      mdState <- stateList$state[which(stateList$stateAbbr == toupper(missingData[mD]))]
      if(verbose) cat("Gathering data for", mdState)
      
      # Get Data
      buildSF1Data(mainDir=mainDir, state=mdState, missingData[mD])
    }
  }
  
  ## Give notice
  
  if(verbose) cat('\n\n***Data acquisition complete.***\n\n')
  
}   

### Checks to see which of the data are present, return list of missing ------------------

checkStateData <- function(iState,                # State abbrev to check for data
                           mainDir                # Main directory
){
  
  # Set initial Condition
  dataPresent <- TRUE
  
  # Test for geo data
  if(!file.exists(paste0(mainDir, '/', iState, '/census/2010/sf1/geo.csv'))){
    dataPresent <- FALSE
  }
  
  # Test for P22Data
  if(!file.exists(paste0(mainDir, '/', iState, '/census/2010/sf1/p22Data.csv'))){
    dataPresent <- FALSE
  }
  
  # Return finding
  return(dataPresent)
}  

### Produces message giving state of data acquisition needs ------------------------------

missingMessage <- function(missingData,           # Vector of states with missing data
                           stateList              # Full list of states and abbrevs
){
  
  # Build Message
  if(length(missingData) > 0){
    mdMessage <- "Data is missing from: "
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
    mdMessage <- "All data is present and analysis can proceed"
  }
  
  # Return message
  return(mdMessage)
}   

### Loads in the CBSA data and codes -----------------------------------------------------

loadCBSAInfo <- function(mainDir,                 # Main directory
                         verbose=TRUE             # Give progress?
){
  cbsaPath <- paste0(mainDir, '/censusinfo/cbsainfo/cbsacodes.csv')
  cbsa <- read.csv(cbsaPath)
  if(verbose) cat('CBSA codes loaded.\n')
  
  return(cbsa)
}

### MASTERFUNCTION #######################################################################
### Function that controls all aspects of building the HH Data ---------------------------

createHHLocData <- function(mainDir,                    # Main directory
                            codeDir,                    # Directory where code is
                            cityList,                   # list of cities to analyze
                            outputPath,                 # where to output clean data
                            verbose=TRUE                # Provide status updates
){
  
  # Get all raw data set
  confirmRawData(cityList, mainDir, codeDir, verbose=TRUE)
  
  # Read in all state data
  dataList <- list()
  
  dataList <- lapply(as.list(listOfStates), 
                     mergeGeoP22, 
                     cbsaCodes=cbsaCodes, 
                     mainDir=mainDir)
  
  # Merge all data
  dataList <- rbind.fill(dataList)
  
  # Separate by CBSA code
  dataList <- split(dataList, as.character(dataList$cbsa))
  
  # Calc distances to each center (what to do about multiples?)
  dataList <- lapply(dataList, hhDistCalc, cityList=quickTest,
                     cbsaCodes=cbsaCodes)
  
  # Write out as clean, final data
  cleanData <- rbind.fill(dataList)
  write.csv(cleanData, outputPath, row.names=F)
  
  
}
##########################################################################################

### Trims to block level geographies, merges GEO and P22 Data ----------------------------

mergeGeoP22 <- function(iState,              # 2 letter state abbreviation code
                        cbsaCodes,           # Full list of cbsaCodes
                        mainDir              # Main directory
){
  
  # Force to lower
  iState <- tolower(iState)
  
  # Read in Geo Data  
  geoData <- read.csv(paste0(mainDir, '/', iState, '/census/2010/sf1/geo.csv'),
                      header=T)
  
  # Read in household data
  hhData <- read.csv(paste0(mainDir, '/', iState, '/census/2010/sf1/p22Data.csv'),
                     header=T)  
  
  # Match all CBSA codes in that state
  stateMatch <- as.data.frame(str_locate(cbsaCodes$stateNames, 
                                         toupper(iState)))$start
  cbsaList <- cbsaCodes$cbsaCode[which(!is.na(stateMatch))]
  
  ## Limit Geo list
  
  # By block level only
  blockGeos <- subset(geoData, geoLevel == '101')
  
  # By CBSA 
  blockGeos <- subset(blockGeos, cbsa != "")
  blockGeos <- subset(blockGeos, cbsa %in% cbsaList)
  
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
                       cbsaCodes              # Full List of CBSA Codes
){
  
  # Ensure that city names are in lower case
  names(cityList) <- tolower(names(cityList))
  
  # Grab city which matches the dataObj
  iC <- which(cbsaCodes$cbsaCode == dataObj$cbsa[1])
  iCity <- cityList[iC[1], ]
  
  # Calculate distances (in Miles)
  dists <- sqrt(((dataObj$lat - iCity$lat)^2) + ((dataObj$long) - iCity$long)^2)*69
  
  # Add Distances and city name to dataObj
  dataObj$dists <- dists
  dataObj$cityName <- iCity$city
  
  # Return data Obj
  return(dataObj)
  
}





