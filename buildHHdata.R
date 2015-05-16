
### MAIN FUNCTION USE EXAMPLE ###
if(F){
  dPath <- 'd:/data/usa'
  buildSF1Data(dPath, 'Washington', 'WA')
  
  getMSACodes(dPath)
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
  
  ## Rename fields
  
  names(hhData) <- c('sfID', 'hh1524','hh2534','hh3544','hh4554','hh5564',
                     'hh6574','hh7584','hh85p')
  ## Return data
  
  return(hhData)
  
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
    
    write.csv(cbsa, dlPath)
    if(verbose) cat('CBSA codes clean and written out to.', dlPath, '\n')
    
  } else {
    
    if(verbose) cat('CBSA file already exists.\n')
    
  }
} # closes function



