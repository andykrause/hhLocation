### Master function that builds all CBSA data --------------------------------------------

buildCBSAData <- function(nbrCBSA,                  # Nbr of top CBSAs to consider
                          dataDir=dataDir,          # Data directory
                          codeDir=codeDir,          # Code directory
                          verbose=TRUE              # Show progress comments  
){
  
  ## Get the CBSA list off of the web (or your local drive if done already)
  
  cbsa <- fetchCBSACodes(dataDir=dataDir, verbose=verbose, retCBSA=TRUE)
  
  ## Pull out top X number of metro areas by population
  
  cbsaOnly <- subset(cbsa, is.na(subcode) | subcode=="")
  cbsaTop <- cbsaOnly[1:nbrCBSA, ]
  
  ## Convert a full list of CBSA and their sub units
  
  cbsaList <- cbsa[which(cbsa$code %in% cbsaTop$code), ]
  
  ## Calculate the X,Y points from Google Maps (takes awhile)
  
  cityList <- findCBSACenters(cbsaList, badList=c('Northern New Jersey',
                                                  'Suffolk', 'Nassau', 'Long Island'))
  
  # Isolate those that matched
  resCheck <- which(unlist(lapply(cityList, class))=='data.frame')  
  cityListX <- rbind.fill(cityList[resCheck])
  cityListX$city <- gsub("[.]", "", cityListX$city)
  
  ## Remove dupicates
  cityState <- paste0(cityListX$city, ".", cityList$state)
  cityListX <- cityListX[!duplicated(cityState), ]
  
  ## Write out
  
  write.csv(cityListX, paste0(gv$codeDir, '/studyCityList.csv'), row.names=FALSE)
  write.csv(cbsaTop, paste0(gv$codeDir, '/studyCBSAList.csv'), row.names=FALSE)
  
  ## Return values
  
  return(list(cityList=cityListX,
              cbsaList=cbsaTop))
}   

### Download the full CBSA list ----------------------------------------------------------

fetchCBSACodes <- function(dataDir=gv$dataDir,       # Main directory for all census data
                           verbose=gv$verbose,       # Provide status reports?
                           retCBSA=FALSE             # Return values to function call?
){
  
  ## Create Dir if not there
  
  censPath <- paste0(dataDir, '/censusInfo')
  dir.create(censPath, showWarnings = FALSE)
  cbsaPath <- paste0(censPath, '/cbsaInfo')
  dir.create(cbsaPath, showWarnings = FALSE)
  
  # Create file location
  dlPath <- paste0(cbsaPath, '/cbsaCodes.csv')
  
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
    
    # Convert population to a number
    cbsa$pop2010 <- as.numeric(str_replace_all(cbsa$pop2010, ',', ''))
    
    # Order by decending population
    cbsa <- cbsa[order(-cbsa$pop2010), ]
    
    # Remove those with no names
    cbsa <- subset(cbsa, !is.na(city))
    
    ## Write file back out
    
    write.csv(cbsa, dlPath, row.names=F)
    if(verbose) cat('CBSA codes cleaned and written out to.', dlPath, '\n')
    
  } else {
    
    if(verbose) cat('CBSA file already exists. Reading into memory.\n')
    cbsa <- read.csv(dlPath, header=T, stringsAsFactors=FALSE)
    
  }
  
  ## Return to function  
  
  if(retCBSA) return(cbsa)
  
} # closes function

### Find the lat long for each of the centers/subcenters ---------------------------------

findCBSACenters <- function(cbsaList,                 # List of choses CBSAs to analyze
                            badList=c('Northern New Jersey',
                                      'Suffolk', 'Nassau',
                                      'Long Island'))
  {
  
  
  ##  clean up entries in the CBSA List
  cbsaList$city <- gsub('--', '-', cbsaList$city)
  cbsaList$city <- gsub('/', '-', cbsaList$city)
  cbsaList$city <- gsub('St.', 'St', cbsaList$city)
  
  cbsaList$ST <- gsub(' ', '', cbsaList$ST)
  
  cityList <- list()
  
  for(i in 1:nrow(cbsaList)){
    
    iCBSA <- cbsaList[i, ]
    
    iCity <- unlist(strsplit(iCBSA$city, '-'))
    iCity <- iCity[!iCity %in% badList]
    
    mainID <- which(substr(iCity, 1, 1) == '.')
    if(length(mainID) !=0 & length(iCity) > 1){
      mainCity <- str_replace_all(iCity[mainID], "[.]", "")
      iCity <- iCity[-mainID]
    } else {
      mainCity <- iCity[1]
    }
    
    iCounty <- grep('County', iCity)
    if(length(iCounty) > 0) iCity <- iCity[-iCounty]
    iST <- unlist(strsplit(iCBSA$ST, '-'))
    
    if(length(iCity) > 0){
      
      tempCities <- list()
      
      for(j in 1:length(iCity)){
        if(j == 1 & (is.na(iCBSA$subcode) || iCBSA$subcode == '')){
          cat('\n\nObtaining Latitude and Longitude for', iCity[j], ',', iST[1])
          iXY <- getGoogleLatLong(iCity[j], iST[1])
          tempCities[[j]] <- data.frame(cbsa=iCBSA$code,
                                        city=iCity[j],
                                        state = iST[j],
                                        long=iXY$long,
                                        lat=iXY$lat)
        } else {
          if(length(iST) == 1){
            iXY <- getGoogleLatLong(iCity[j], iST[1])
            cat('\n\nObtaining Latitude and Longitude for', iCity[j], ',', iST[1])
            tempCities[[j]] <- data.frame(cbsa=iCBSA$code,
                                          city=iCity[j],
                                          state=iST,
                                          long=iXY$long,
                                          lat=iXY$lat)    
          } else {
            
            stRes <- list()
            states <- NULL
            for(k in 1:length(iST)){
              stRes[[k]] <- try(getGoogleLatLong(iCity[j], iST[k]), silent=TRUE)
              cat('\n\nObtaining Latitude and Longitude for', iCity[j], ',', iST[k])
              states <- c(states, iST[k])
            }
            sClass <- unlist(lapply(stRes, class))
            yesResults <- sClass!='try-error'
            sNA <- !unlist(lapply(lapply(stRes, is.na), function(x) x[1]))
            states <- states[yesResults & sNA]
            stRes <- rbind.fill(stRes[yesResults & sNA])
            
            if(length(unique(stRes$long)) == 1){
              iXY <- stRes[1,]
              closest <- 1
              
            } else {
              mainXY <- getGoogleLatLong(mainCity, iST[1])
              mDists <- distHaversine(p1=c(mainXY$long, mainXY$lat),
                                      p2=cbind(stRes$long, stRes$lat))
              closest <- which(mDists == min(mDists))[1]
              iXY <- stRes[closest,]
            }
            tempCities[[j]] <- data.frame(cbsa=iCBSA$code,
                                          city=iCity[j],
                                          state=states[closest],
                                          long=iXY$long,
                                          lat=iXY$lat)     
          }
        }   
      }
      
      cityList[[i]] <- rbind.fill(tempCities)
      Sys.sleep(15)
      
    } else {
      cityList[[i]] <- 'No city'
    }
  }
  return(cityList)
}

### Get Lat and Long for city from Google Maps -------------------------------------------

getGoogleLatLong <- function(city, ST){
  
  # Set Libraries
  require(stringr)
  
  ## Pull down information from google maps
  
  gCode <- try(readLines(paste0('https://www.google.com.au/maps/place/',
                                city, ',+', ST, ',+USA/'))[10], silent=T)
  
  # If lat long was found
  
  if(class(gCode) != 'try-error'){
    
    # Find city in code
    gLoc <- as.data.frame(str_locate(gCode, city))$end
    trimCode <- substr(gCode, gLoc, gLoc + 70)
    
    # Trim code down to correct area
    tLoc <- as.data.frame(str_locate(trimCode, 'null,null,'))$end
    ttCode <- substr(trimCode, tLoc + 1, tLoc + 28)
    
    # Find start and stop of coordinates
    findComma <- as.data.frame(str_locate(ttCode, ','))$end
    findEnd <- as.data.frame(str_locate(ttCode, "[]]"))$end
    if(is.na(findEnd)) findEnd <- nchar(ttCode)
    
    # Extract coordinates
    lat <- as.numeric(substr(ttCode, 1, findComma - 1))
    long <- as.numeric(substr(ttCode, findComma + 1, findEnd - 1))
    
    # Return values
    return(data.frame(long=long, lat=lat))
    
  } else {
    
    # If lat long not found
    return(NA)
    
  } 
}

