# Household age and metropolitan location

This repository contains the necessary code to reproduce the analysis of household location across 35 metropolitan regions as discussed in the paper entitled: "Fancy Title Here", by Anonymous Real Estate Author (anonymous for peer review effects).

## Documentation

### Introduction

This document explains the process for reproducing the data and analysis described in the paper entitled ``Fancy Title Here''.  There are three key steps in reproducing the data and analysis:

1. Download all raw data files according to the instruction below from the www.dataverge.org hosted site. [ http://dx.doi.org/10.7910/DVN/RHJCNC]( http://dx.doi.org/10.7910/DVN/RHJCNC "DOI")
2. Download all code files from this repository at: [http://www.github.com/AnonREAuthor/ReproducibleRealEstate](http://www.github.com/AnonREAuthor/ReproducibleRealEstate "Git")
3.  Execute the code using R version 3.1.1 (R Core Team 2015), as decribed below. 

Each step in explained below, including a 'chunk'-by-'chunk' explanation (literate programming) of the code used to prepare and analyze the data. The **processOverview.png** file shows a schematic of the data, operations and results of this analysis.   

### Downloading Data Files

The data files used in this analysis are hosted on the Harvard Dataverse Network and can be accessed online at [ http://dx.doi.org/10.7910/DVN/RHJCNC]( http://dx.doi.org/10.7910/DVN/RHJCNC "DOI"). Navigate to the 'DATA & ANALYSIS' tab and download all files in the 'Raw Assessor Data' and 'Raw GIS Data' sections.  Save the files to a suitable working directory.  This working directory will be used in the data preparation and analysis code that follows.  Users without a current Dataverse account will need to create a free account in order to access the data.

The raw files are also available at [http://info.kingcounty.gov/assessor/DataDownload/default.aspx](http://info.kingcounty.gov/assessor/DataDownload/default.aspx "Assessor") and [http://www5.kingcounty.gov/gisdataportal/Default.aspx](http://www5.kingcounty.gov/gisdataportal/Default.aspx "GIS").  As these files are updated weekly, downloading the latest data and running the code that follows on it will produce slightly different results due to changes in the assessor's data since January 5th, 2015 (the first weekly update that contained the 2014 year end data), the date of the archived data that is available on Dataverse.

### Downloading Code

The code necessary to reproduce this analysis is located at [http://www.github.com/AnonREAuthor/ReproducibleRealEstate](http://www.github.com/AnonREAuthor/ReproducibleRealEstate "Github").  All coding is done in R, using version 3.1.1. R can be downloaded at [http://cran.r-project.org/bin/windows/base/](http://cran.r-project.org/bin/windows/base/ "R").  The necessary libraries are listed at the beginning of the **dataPrepCode.R** file.  There are five files that need to be downloaded:

1. dataPrepCode.R:  The script to clean and prepare the raw data.
2. basicConversionTools.R:  A set of basic tools for converting data formats.
3. kingDataDevelop.R: A set of functions for dealing specific with King County data.
4. kingBuildSales.R: A set of function for dealing specically with King County Sales data.
5. dataAnalysisCode.R: The script that generates the results as discussed in the paper.
6. spatEconTools.R: A set of helper functions for working with spatial econometric models.

The downloaded code can be saved in the same directory as the data or in a separate directory.  The user will have the option of specifying separate data and code directories in the analysis the follows.

NOTE:  User wishing to avoid the lengthy data preparation stage of the analysis can skip directly to the data analysis by using the **cleansales.csv** file and the **dataAnalysisCode.R**.   

### Data Preparation and Analysis

Two of the five code files need to be executing in R by the user.  First the data preparation code (**dataPrepCode.R**) followed by the actual analysis (**dataAnalysisCode.R**).  The data preparation code will take up to 30 minutes to run, depending on your process speed and alloted memory size.

#### Data Preparation: **dataPrepCode.R**

This section explains each `chunk' of data preparation code.  Further explanation of the individual functions can be found in the comments in the individual code files (**basicConversionTools.R**, **kingDataDevelop.R** and **kingBuildSales.R**)

#### Initial Commands

First, the necessary libraries are loaded.  Users may need to first install the individual libraries using the `install.packages()` command.  


    library(RODBC)
    library(RSQLite)
    library(DBI)
    library(stringr)
    library(maptools)
    library(plyr)
    library(sp)
    library(spdep)
    library(rgeos)
    library(spam)
    library(colorRamps)
    library(RColorBrewer)

Next, the paths to the downloaded code and data are set.  These can be identical.  This is the only part of the code that they user will be required to change.  Prior to executing the **dataPrepCode.R** file, enter the path to the downloaded code and data files in the place of *your/code/path* and *your/data/path*.  Note the use of UNIX forward slashes to separate directories.

    codePath <- 'your/code/path'   # ex. 'c:/temp/
    dataPath <- 'your/data/path' 

The three files containing functions to be used in the code preparation are then read into memory with the `source()` command.

    source(file.path(codePath, '/basicConversionTools.R'))
    source(file.path(codePath, '/kingDataDevelop.R'))
    source(file.path(codePath, '/kingBuildSales.R'))

In the final initial step, parameters for preparation script are set.  The first parameter -- **convertData** -- set whether or not the lengthy data conversion (.csv to .db) process should be undertaken.  The first time this code is run, the parameter should be left as TRUE.  Subsequent runs in which the user may want to change some of the individual data preparation parameters (such as which sales instrument to exclude) can be expedited by changing **convertData** to FALSE.  The **studyYear** parameter indicates that the preparation should be done on 2014 data.  If a user where to download new 2015 data and attempt to run this code on that data, this parameter should be changed to 2015.  Attempts to reproduce the results should not alter these two parameters. 

    convertData <- TRUE
    studyYear <- 2014
  
#### Data Conversion

In this set of steps, the raw .csv files are converted to .db SQLite file formats.  This process is accomplished with the `convertCSVtoSQLite()` function, a custom-function annotated in the **basicConversionTools.R** file.  To begin, the two tabular assessor files with information on the parcel and the residential building are converted.

    convertCSVtoSQLite(dataPathCurrent = dataPath,
                   dataPathNew = dataPath,
                   newFileName = 'KingData2014.db',
                   fileNames=c('Extr_Parcel.csv', 'Extr_ResBldg.csv'),
                   tableNames = c('Parcel2014', 'ResBldg2014'),
                   overWrite=TRUE)  

  
Next, the assessed value history file is converted.  Because this file contains assessed value histories back into the mid-1990s, a second command (`kngBuildAssdVal()`) isolated the year 2015 values (expressing the property values as of the end of the year 2014).

    convertCSVtoSQLite(dataPathCurrent=dataPath,
                       dataPathNew = dataPath,
                       newFileName = 'KingValueHistory.db',
                       fileNames=c('EXTR_ValueHistory_V.csv'),
                       tableNames = c('ValueHistory'),
                       overWrite=TRUE)
  
    kngBuildAssdVal(avYears=studyYear + 1,  
                    assdValDB = file.path(dataPath, 'kingvaluehistory.db'),
                    overWrite=TRUE)

And then the sales file is converted.

     convertCSVtoSQLite(dataPathCurrent=dataPath,
                        dataPathNew = dataPath,
                        newFileName = 'KingSales.db',
                        fileNames=c('EXTR_RPSale.csv'),
                        tableNames = c('AllSales'),
                        overWrite=TRUE)

#### Initial Sales Cleaning

Next, an initial set of filters are applied to the sales data to remove obvious non-arms length transactions.  This process all adds a unique identifier to the sales data and performs a number of other basic data managemnt calculations.  A specific list of tasks is below.  The `kngSCleanSales()` function is described in more detail in the **kingBuildSales.R** file.

**Specific Tasks**

1. Remove sales with no PIN number or no sales price
2. Remove sales missing sales date
3. Trim sales to desired date range (by year)
4. Add a custom PIN number (manage leading 0's)
5. Analyze total transactions per parcel, trim those with too many (suspect sales)
6. Label sales with multiple parcels in the transaction
7. Add unique IDs at the transaction and the parcel-transaction level
8. Remove sales with bad instrument, reason or warning codes
9. Write the data to a SQLite file

.

      kngSCleanSales(saleYears = studyYear,
                    transLimit = 10,
                    salesDB = file.path(dataPath, 'kingsales.db'),
                    trimList=list(SaleReason=2:19,
                                  SaleInstrument=c(0, 1, 4:28),
                                  SaleWarning=paste0(" ", c(1:2, 5:9, 11:14, 18:23, 
                                                         25, 27, 31:33, 37, 39,
                                                         43, 46, 48, 49,
                                                         50:53, 59, 61,
                                                         63, 64, 66),
                                                  " ")),
                                 overWrite=TRUE,
                                 verbose=FALSE
                                 )


#### Labeling Sales

Next we label the sales with their land use and record type. All sales not single family residential in use are removed (including apartments and condominiums).  This filtered data is then written to the **labeledSales** table in location given in the **salesDB** database.  More details on the `kngSLabelSales()` and its sub-ordinate functions can be found in the **kingBuildSales.R** file.

  kngSLabelSales(saleYears=2014, 
                   salesDB=file.path(dataPath, 'kingsales.db'),
                   overWrite=TRUE,
                   verbose=FALSE)

Instances of multiple parcel sales are removed with the `kngSConfirmLabels()` function.  As are unnecessary fields.  More details on the `kngSConfirmLabels()` can be found in the **kingBuildSales.R** file.

  kngSConfirmLabels(salesDB=file.path(dataPath, 'kingsales.db'),
                       latestYear=2014,
                       verbose=TRUE,
                       overWrite=TRUE)

#### Append Data to Sales

In this last step of data preparation additional data such as parcel, residential building, coordinate and assessed value data is appended to the sales transaction observations.

First, the parcel data and the residential building data are attached with the `kngSSplitAttachSales()` function.More details on the `kngSLabelSales()` and its sub-ordinate functions can be found in the **kingBuildSales.R** file.


    kngSSplitAttachSales(salesDB=file.path(dataPath, 'kingsales.db'),
                       dataDir=dataPath,
                       verbose=TRUE,
                       overWrite=TRUE)

Then the 2015 assessed values are attached with the `kngSAttachAssdValues()` function.More details on the `kngSAttachAssdValues()` and its sub-ordinate functions can be found in the **kingBuildSales.R** file.

  xSales <-  kngSAttachAssdValues(salesDB=file.path(dataPath, 'kingsales.db'),
                                  dataDir=dataPath,
                                  dataYear=studyYear,
                                  verbose=TRUE,
                                  overWrite=TRUE)


Then the 2015 assessed values are attached with the `kngSAttachXYs()` function.More details on the `kngSAttachXYs()` and its sub-ordinate functions can be found in the **kingBuildSales.R** file.

    xSales <- kngSAttachXYs(xSales,
                          latlongFile = file.path(dataPath, 'parcelpoints2014.shp'),
                          verbose=FALSE)

#### Write out Clean Sales File

The final step of the initial data cleaning process writes the sales data to a .csv file for later analysis.

    write.csv(xSales,
              file=file.path(dataPath, 'cleansales.csv'),
              row.names=FALSE)

### Data Analysis: **dataAnalysisCode.R**

This section explains each `chunk' of data analysis code.  Further explanation of the individual functions can be found in the comments in the individual code files **spatEconTools.R**.

#### Initial Commands

First, the paths to the downloaded code and data are set.  These can be identical.  This is the only part of the code that they user will be **required** to change.  Prior to executing the **dataAnalysisCode.R** file, enter the path to the downloaded code and data files in the place of 'your/code/path' and 'your/data/path'.  Note the use of UNIX forward slashes to separate directories. 

   codePath <- 'your/code/path'  # ex.  'c:/temp/
   dataPath <- 'your/data/path' 

A set of spatial econometric helper functions (found in the **spatEconTools.R** files) that are used in the empirical analysis are then read into memory with the `source()` command.

   source(file.path(codePath, 'spatEconTools.R'))

#### Load and Clean Data

We begin the analysis by loading the data from the .csv file created in the earlier section.  This data is saved in the **kcSales** object.

    kcSales <- read.csv(file.path(dataPath, 'cleanSales.csv'), header=T)

The previous data cleaning exercise removed observations (transactions) based primarily on their transaction characterstics.  The next set of criteria, cleans by characteristics of the properties themselves.  The goal here is to remove transaction of properties that are not single family structures -- with a single living unit and habitable residential building -- at the time of sale.

We begin by removing all transactions of homes with more than one habitable, residential dwelling. 141 obsevations were removed.

  
    kcSales <- subset(kcSales, nbrBldgs == 1)

Next, we remove all sales of properties not having an assessors' use of code of 2 (single family detached home) or 29 (townhouse). 604 observations removed.


    kcSales <- subset(kcSales, PresentUse == 2 | PresentUse == 29)


All sales of homes with more than one living unit (duplexes, triplexes and quadplexes) are removed. 41 observations removed.


    kcSales <- subset(kcSales, NbrLivingUnits == 1)

Finally, observations with an improved assessed value of \$0 are removed as well.  This is due to the fact that some of these may represent land sales or sales of partially finished homes that were not properly labeled in the transactional data. 1,572 observations removed. 
  
    kcSales <- subset(kcSales, ImpsVal != 0)

After this initial data cleaning to eliminate likely non-single family residential structures, 20,530 observations remained out of an initial 22,847.

#### Transform fields

In this section of code, a number of variables from the raw data are transformed for statistical modeling purposes.

We begin by creating a variable indicating the month of sale from the original full sale date field.

  
    kcSales$Month <- as.numeric(substr(kcSales$DocumentDate,1,2))

Next, a binary waterfront location variable is created from the original WFNTLOCATION field which was an ordinal variable denoting location quality.

    kcSales$WFNT <- ifelse(kcSales$WfntLocation > 0, 1, 0)

The baths, half baths and three-quarter baths are then summed into a unified baths variable.

    kcSales$Baths <- (kcSales$BathFullCount + kcSales$Bath3qtrCount * .75 +
                    kcSales$BathHalfCount * .5)  

Similarly, all four types of fireplaces are summed into a combined fireplace field.


    kcSales$Fireplaces <- (kcSales$FpSingleStory + kcSales$FpMultiStory +
                         kcSales$FpFreestanding + kcSales$FpAdditional)
                         
Next, we create a binary variable indicating whether or not the home is a townhome.


    kcSales$Townhome <- ifelse(kcSales$PresentUse == 29, 1, 0)
    
Finally, a number new, scaled variables are created in so that resulting model coefficients are of an appropriate scale.

    kcSales$lotAcres <- kcSales$SqFtLot / 43560
    kcSales$homeSize <- kcSales$SqFtTotLiving / 1000
    kcSales$Age <- 2014 - kcSales$YrBuilt

#### Variable of Interest: VIEW

View quality is the variable of interest in this study.  The existing King County Assessors View fields are very dissaggregated, representing ten different view types and five different view scores.  Below these are compbined into three major view catagories and a category specific view score -- maximum of individual scores within that category -- is created.

First, we convert the three mountain views into a single binary variable representing at least one mountain view. The maximum mountain view score is then computed and added to the VIEWMOUNTSCORE variable.


    kcSales$viewMount <- ifelse(rowSums(cbind(kcSales$MtRainier, kcSales$Olympics,
                                            kcSales$Cascades)) > 0, 1, 0)

    kcSales$viewMountScore <- apply(cbind(kcSales$MtRainier, kcSales$Olympics,
                                        kcSales$Cascades), 1, max)

Next, the same process is applied to view of the three major waterbodies in the area -- The Puget Sound, Lake Washington and Lake Sammamish.

    kcSales$viewWater <- ifelse(rowSums(cbind(kcSales$PugetSound, kcSales$LakeWashington,
                                            kcSales$LakeSammamish)) > 0, 1, 0)

    kcSales$viewWaterScore <- apply(cbind(kcSales$PugetSound, kcSales$LakeWashington,
                                        kcSales$LakeSammamish), 1, max)

And, then it is applied to the remaining set of views -- Territorial, Seattle Skyline, Small waterbody and other -- referred to collectively as `Other'.


 
    kcSales$viewOther <- ifelse(rowSums(cbind(kcSales$Territorial, kcSales$SeattleSkyline,
                                            kcSales$SmallLakeRiverCreek, 
                                            kcSales$OtherView)) > 0, 1, 0)

    kcSales$viewOtherScore <- apply(cbind(kcSales$Territorial, kcSales$SeattleSkyline,
                                        kcSales$SmallLakeRiverCreek, 
                                        kcSales$OtherView), 1, max)

Finally, we develop a binary variable that notes whether or not a property has multiple views in the three categories created above.  The total view score, accomplished by summing all view scores across the original ten view catogories is also created.


    kcSales$viewMult <- ifelse(rowSums(cbind(kcSales$viewMount, kcSales$viewWater,
                                           kcSales$viewOther)) > 0, 1, 0)

    kcSales$viewTotalScore <- rowSums(kcSales[,which(names(kcSales) == 'MtRainier'):
                                              which(names(kcSales) == 'OtherView')])

#### Filter outliers

Having creating all the variable, we now remove a number of observations that have characteristics that render the property unlikely to be representative of a traditional, single family home sale -- the focus of this analysis.

First we filter out all sales with more than two acres of land.  The rationale for this filter is than many properties with large acreage are either development sites and have additional value in the excess land or are agricultural properties located on the far fringes of the urban area.  The two acre cutoff is based on prevailing land use regulations and removes 497 (2.4%) observations from the data set.  


    trimSales <- subset(kcSales, SqFtLot < (43560 * 2))

Next, homes with eight or more bedrooms are filtered out.  These properties represented either very large, high end properties or special use homes, both of which are unrepresentative of our intended sample of traditional single family residential sales.  The cutoff of eight is based on a natural break determined by exploratory data analysis.  14 observations (0.06%) are removed with this filter.

    trimSales <- subset(trimSales, Bedrooms < 8)

The final filter removes home with less than 500 or more than 8,000 square feet of living space.  Again, these are removed as they are likely not traditional single family homes.  These cutoffs are based on natural breaks in the data and remove 32 observations (0.16%).

    trimSales <- subset(trimSales, SqFtTotLiving >= 500 & SqFtTotLiving <= 8000)

After the three filtering activities, 19,987 observations remain.

#### Model Estimation

We begin by developing a regression model using the trimmed set of sales observations.  The model is specified in a semi-log format in which the natural log of the sales price is the dependent variable.

    modBase <- lm(log(SalePrice) ~ as.factor(Month) + lotAcres + WFNT + BldgGrade + 
                  homeSize + Baths + Age + Fireplaces + Townhome + 
                  viewMount + viewWater + viewOther, data=trimSales)

We then test for spatial autocorrelation in the residuals of the model  To do so, we first construct a spatial point data frame using the `SpatialPointsDataFrame()` function from the **sp** package.

    salesSP <- SpatialPointsDataFrame(coords=cbind(trimSales$X, trimSales$Y),
                                       data=trimSales)
                                       
Then, the custom `createSWM()` function -- see the **spatEcon.R** file -- is used to build a distance weighted, spatial weigths matrix including the 10 nearest points. The nugget of 25 feet ensures that multiple sales of a single property do not exert an overly large impact in the distance weighting function.

    swmAll10 <- createSWM(salesSP, 10, nugget=25)

Using the spatial weights matrix and the spatial points data.frame, we test for spatial autocorrelation in error terms using a Moran's I test.

    miAll <- moran.test(modBase$resid, swmAll10, zero.policy=TRUE)

As the Moran's I test shows a very high level of spatial autocorrelation, we then use a robust LaGrange multiplier test to determine if the predominant form of the spatial dependence in the dataset is in the form of spatial lag or spatial error.

    lmAll <- lm.LMtests(modBase, swmAll10, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
 
Finding that the dependence is overwhelming represented in the error term, we then esitmate a spatial error model using the  spatial weights matrix specified above.
 
    modSEM <- errorsarlm(as.formula(modBase), data=salesSP@data, swmAll10, method="spam", 
                       zero.policy=TRUE)

Checking the diagnostics from the spatial error model show it to have greatly improved r-squared, standard error and AIC values as compared to the basic OLS model.

    summary(modBase)$r.squared
    calcPseudoR2(modSEM)

    summary(modBase)$sigma
    sqrt(modSEM$s2)  

    AIC(modBase)
    AIC(modSEM)

Finally, we also test an additional specification of the models that evaluates the impact of the different view scores levels for each of the three view types.  As before, we also run a sptaial error model to correct for the spatial autocorrelation in the residuals.  

    modBaseSc <- lm(log(SalePrice) ~ as.factor(Month) + lotAcres + WFNT + BldgGrade + 
                  homeSize + Baths +
                  Age + Fireplaces + Townhome + 
                  as.factor(viewMountScore) + 
                  as.factor(viewWaterScore) + 
                  as.factor(viewOtherScore), data=trimSales)

    modSEMSc <- errorsarlm(as.formula(modBaseSc), data=salesSP@data, swmAll10,
                           method="spam", zero.policy=TRUE)

As before, the spatial error model vastly outperforms the OLS specification.

    summary(modBaseSc)$r.squared
    calcPseudoR2(modSEMSc)

    summary(modBaseSc)$sigma
    sqrt(modSEMSc$s2)  

    AIC(modBaseSc)
    AIC(modSEMSc)





