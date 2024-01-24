##########################################################################
##########################################################################
## Script Name: hydroRealism_marketModel.R
## Purpose of Script: This script preforms the market model fitting and 
## prediction of amount of traded water rights for the Hydro Realism 
## analysis. This is done with data from West Water Research (WWR) to
## inform the economic model. Specifically, the WWR data was used to 
## determine the average price per write as well as the trading 
## relationships modeled, which we limited to historical trades from 
## the Waterlitix dataset. 
##    One of the major challenges with using Waterlitix is that the
## water rights trades were summarized in the geographic unit of trading
## regions, which were determined by WWR. For some states, these were the
## same set of boundaries we used for the Water Management Areas (WMA) for
## that state. In other cases they did not. For some states, the trading
## regions were political boundaries, such as counties - which do not
## cleanly align with delineated watersheds. In order to preform the 
## market modeling with the Waterlitix, we had to recalculate the cumulative
## curve from each WMA into a curve for each trading region 
## (convertWMAVolToTrdRegions function). Then the market model could be
## fit, and a prediction for the results of trading were calculated. 
## Finally, the new predicted trade region results were then then 
## "dispersed" to the WMAs based on the percentage amount that each WMA
## contributed to the trade region (applyTrdRegionsToWMAs function). 
##    The market model itself uses the point expansion method to calculate
## an equilibrium between quantities and prices to find a state where net
## benefits are maximized. Within this script, this is referred to as the
## fitting process and is managed with the econModelStates function. It 
## should be noted that the function is robust enough to handle different
## supply and demand sectors; however, we were primarily interested in
## water rights moving from the agriculture sector (supply) to the urban 
## sector (demand). Output positive values from this process indicate 
## supply "selling" to demand and negative values represent supply buying 
## from demand.
##    Elasticities at the state level were calculated from Waterlitix in
## a separate script. For now the values are hard coded inputs, but it is 
## planned to revise this in future updates. 
##    One final note should be stated. As described in the paper, the 
## market model was ran under various scenarios. This script is designed
## of capable of running each of the described scenarios, and has some
## remnant of code of other tested hypothetical scenarios. With this in 
## mind, while the script can be loaded and from the first to last line,
## that is not specifically how the script SHOULD be ran. Any user should 
## find and make sure they fully understand the `markAnalysisMode` variable
## and its potential options as it will have a profound effect on the 
## required data inputs as well as general run time.
##
## Special Requirements: Cumulated water rights; Elasticity values - from WWR; 
## Prices for each modeling sector to pay - from WWR; spatial WMA data;
## spatial WWR water trade regions
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 4/5/2021
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 1/24/2024
##
## Copyright (c) 2024 The Pennsylvania State University
##
##########################################################################
##########################################################################
options(stringsAsFactors=F, scipen=999)

##setting the paths of various directories
##local machine
#docDir <- "/Users/mdl5548/Documents/"
#projCodeDir <- paste0(docDir, "GitHub/waterRightsCumulationCurves/")
#gdrBase <- "/Users/mdl5548/Library/CloudStorage/GoogleDrive-mdl5548@psu.edu/Shared drives/PCHES_Project1.2/Water rights project/Water institutions/Data/"
#projGdrDir <- paste0(gdrBase, "waterRightsCumulations/")
#wwrDir <- paste0(projGdrDir, "wwrData/")
#cumuDir <- paste0(projGdrDir, "output/")
#stateDataDir <- paste0(projGdrDir, "inputData/")
#outDir <- paste0(cumuDir, "curveFitting/")
##parallels machine
#docDir <-
#projCodeDir <-
#gdrBase <-
#projGdrDir <-
#wwrDir <-
#cumuDir <-
#stateDataDir <-
#outDir <-
##RC machine
docDir <- "/storage/group/pches/default/users/mdl5548/"
projCodeDir <- "/codeDir/"
#gdrBase <-
projGdrDir <- "/dataDir/"
wwrDir <- paste0(projGdrDir, "wwrData/")
cumuDir <- paste0(projGdrDir, "output/")
stateDataDir <- paste0(projGdrDir, "inputData/")
outDir <- paste0(cumuDir, "curveFitting/")



##scenerio Dir
#scenDir <- paste0(wwrDir, "withStorage_wmaVol/")
scenDir <- paste0(wwrDir, "withoutStorage_RegionVol/")

##read in libraries
.libPaths("/Rlib")
library(BBmisc)
library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library(reshape2)
library(usdata)

##source custom functions
source(paste0(projCodeDir, "hydroRealism_marketModel_CustomFunctions.R"))

##read in the water rights cumulation data - from storage file
load(paste0(cumuDir, "reorganizedData/statesCalcCumulative.RData"))

##An object to allow the user to specify which market model analysis to
##run. There are three possible options: 'basic', 'cutoffs', and/or 
##'sgmaCutoffs'. At least one option must be input, but all three may be
##included. The 'basic' market model is the original in which a certain 
##percentage of water rights were traded from the agriculture sector to 
##the urban sector. Both of the 'cutoffs' options instead trade the oldest
##rights first until either the predicted amount of water has been traded,
##or the cutoff date (year) has been reached. To tie these options back
##to the paper, 'basic' is Scenario 1, 'cutoffs' is Scenario 2, and 
##'sgmaCutoffs' is Scenario 2a.
markAnalysisMode <- c("basic") #c("basic", "cutoffs", "sgmaCutoffs")

##Projection used to calculate shape areas
projForAreaCalc <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

##########################################################################
##read in WWR derived data
priceFiles <- list.files(paste0(wwrDir, "marketTrdSummaries/"), "_updatedVals", full.names=T)

##Programmatically list the states to preform the market model for
##If one would instead only like to run a subset of states, selected states
##can be manually input in the `stAbbr` object
fileNames <- sapply(strsplit(priceFiles, "/"), "[[", length(strsplit(priceFiles[1],"/")[[1]]))
##Get the state abbreviation and state name from given abbreviations
stAbbr <- sapply(strsplit(fileNames, "Ave"), "[[", 1)
stateNames <- tolower(abbr2state(stAbbr))

##Read in the relationship tables to determine which trades to consider in modeling, 
##for restricting to actual trading relationships
##These relationships are based on the WWR trading data
##surface water
findSWTrdRels <- list.files(paste0(wwrDir, "marketTrdSummaries/"), "MarketSWVol", full.names=T)
findSWTrdRels <- findSWTrdRels[sapply(paste0(stAbbr, "Region"), grep, x=findSWTrdRels)]
swHistTrdRelations <- lapply(findSWTrdRels, read.csv)
##groundwater
findGWTrdRels <- list.files(paste0(wwrDir, "marketTrdSummaries/"), "MarketGWVol", full.names=T)
findGWTrdRels <- findGWTrdRels[sapply(paste0(stAbbr, "Region"), grep, x=findGWTrdRels)]
gwHistTrdRelations <- lapply(findGWTrdRels, read.csv)

##Read in WMA layer, to be able to calculate the area difference between WMAs and trading regions
wmasLayer <- readOGR(dsn=paste0(stateDataDir, "allStateWMAs/"), layer="simpleWMAs")
##preform some minor clean-up in the names of several WMAs. This should be moved to preprocessing
##for V2
wmasLayer$basinName[wmasLayer$state=="Colorado" & wmasLayer$basinNum==71] <- "West Dolores Creek"
wmasLayer$basinName[wmasLayer$state=="Nevada"] <- paste(wmasLayer$basinName[wmasLayer$state=="Nevada"], wmasLayer$basinNum[wmasLayer$state=="Nevada"], sep="_")
wmasLayer$basinName[wmasLayer$state=="Montana"] <- paste(wmasLayer$basinName[wmasLayer$state=="Montana"], wmasLayer$basinNum[wmasLayer$state=="Montana"], sep="_")
##Arizona groundwater WMA layer
azGrdWMAs <- readOGR(dsn=paste0(stateDataDir, "allStateWMAs/"), layer="azGroundWMAs")

##Read in US county layer - as WWR listed the trading regions for several states as
##the county boundaries.
usCountiesLayer <- readOGR(dsn=paste0(docDir, "backgroundGISData/countyBoundaryRef/cb_2018_us_county_500k"), layer="cb_2018_us_county_500k")
usCountiesLayer <- spTransform(usCountiesLayer, wmasLayer@proj4string@projargs)

##Read in layer with WMA name conversion provided by UNH, for modified water rights output file names
##This is required as several states have similar enough WMA identifiers so that those 
##identifiers would not be unique on their own. However, WBM requires that each WMA have its own
##numeric only identifier. Due to this, UNH developed a WBM acceptable identifier and it is used
##here to name the output files.
namConvData <- read.csv(paste0(projGdrDir, "unhNamingConvention/simpleWMAs_v2_ID.csv"))

##!!!OPTIONAL!!!
##Read in date cutoff table - required for the 'earlyPrioDates' scenario, or when the 
##market model should trade the water in the earliest priority years first. Anything prior
##to the cutoff year should be considered up for trade and anything after should not be touched.
##If no cutoff is provided, the default is the last year of the analysis, which is set several
##lines below.
if("cutoffs" %in% markAnalysisMode){
  wmaCutOffDates <- read.csv(paste0(projGdrDir, "wma_cutoff_dates_WatInst_noMarketMod_v25.csv"))
  wmaCutOffDates$Cutoff_Date <- trunc(wmaCutOffDates$Cutoff_Date)
  ##Merge the cutoff dates with the name conversions, to be able to be used later in the script
  datesWithIDs <- merge(x=namConvData, y=wmaCutOffDates, by.x="ID", by.y="WMA", all.x=T)
  ##If a cutoff date isn't provided, default to the last date
  datesWithIDs$Cutoff_Date[which(is.na(datesWithIDs$Cutoff_Date)==T)] <- 2100
}
if("sgmaCutoffs" %in% markAnalysisMode){
  wmaCutOffDatesSMGA <- read.csv(paste0(projGdrDir, "wma_cutoff_dates_WatInst_noMarketMod_sgma_v25.csv"))
  wmaCutOffDatesSMGA$Cutoff_Date <- trunc(wmaCutOffDatesSMGA$Cutoff_Date)
  ##Merge the cutoff dates with the name conversions, to be able to be used later in the script
  datesWithIDsSMGA <- merge(x=namConvData, y=wmaCutOffDatesSMGA, by.x="ID", by.y="WMA", all.x=T)
  ##If a cutoff date isn't provided, default to the last date
  datesWithIDsSMGA$Cutoff_Date[which(is.na(datesWithIDsSMGA$Cutoff_Date)==T)] <- 2100
}


##Set up objects in order to be populated with data from the various states
##Essentially loading in the state trade regions data, if it has not already 
##been done and assigning elasticity values
stateRegSpData <- list()
agrElast <- rep(NA, length(stAbbr))
urbElast <- rep(NA, length(stAbbr))
stSurRights <- list()
stGrdRights <- list()
statePricesByReg <- list()
##read in California trade regions
if("ca" %in% stAbbr){
  caliHydroReg <- readOGR(dsn=paste0(stateDataDir, "california/Hydrologic_Regions (mdl5548@psu.edu)/"), layer="Hydrologic_Regions")
  ##dissolve features by basin name
  caliFields <- caliHydroReg@data[,c("HR_NAME")]
  dissolveCali <- unionSpatialPolygons(caliHydroReg, caliHydroReg$HR_NAME)
  ##add attribute table to the dissolved features
  pid <- sapply(slot(dissolveCali,"polygons"),function(x){slot(x,"ID")})
  matchBasinID <- match(pid, caliFields)
  disCaliDF <- data.frame(basinName=pid, state="California", row.names=pid)
  caliHydroReg <- SpatialPolygonsDataFrame(dissolveCali, data=disCaliDF)
  stateRegSpData[[which(stAbbr=="ca")]] <- caliHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="ca")]] <-  read.csv(priceFiles[grep("caAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="ca")] <- -0.329
  urbElast[which(stAbbr=="ca")] <- -1.207
}
##read in Colorado trade regions
if("co" %in% stAbbr){
  coloHydroReg <- wmasLayer[wmasLayer$state=="Colorado",]
  coloHydroReg$basinName[grep(" - ", coloHydroReg$basinName)] <- sapply(strsplit(coloHydroReg$basinName[grep(" - ", coloHydroReg$basinName)], " - "), "[[", 2)
  stateRegSpData[[which(stAbbr=="co")]] <- coloHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="co")]] <-  read.csv(priceFiles[grep("coAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="co")] <- -0.579
  urbElast[which(stAbbr=="co")] <- -2.389
}
##read in Arizona trade regions
if("az" %in% stAbbr){
  boundsToKeep <- c("SANTA CRUZ AMA", "JOSEPH CITY INA", "PRESCOTT AMA", "PHOENIX AMA", "HARQUAHALA INA", "PINAL AMA", "TUCSON AMA", "DOUGLAS INA")
  azGrdWMAs$basinName[-c(which(azGrdWMAs$basinName %in% boundsToKeep))] <- "OUTSIDE AMA / INA"
  
  dissAZ <- unionSpatialPolygons(azGrdWMAs, azGrdWMAs$basinName)
  pid <- sapply(slot(dissAZ,"polygons"),function(x){slot(x,"ID")})
  dissAZDF <- data.frame(Name=pid, row.names=pid)
  arizHydroReg <- SpatialPolygonsDataFrame(dissAZ, data=dissAZDF)
  names(arizHydroReg) <- "basinName"
  stateRegSpData[[which(stAbbr=="az")]] <- arizHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="az")]] <-  read.csv(priceFiles[grep("azAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="az")] <- -0.564
  urbElast[which(stAbbr=="az")] <- -2.017
}
##read in Idaho trade regions
if("id" %in% stAbbr){
  idahoHydroReg <- wmasLayer[wmasLayer$state=="Idaho",]
  idahoHydroReg$basinName <- idahoHydroReg$basinNum
  stateRegSpData[[which(stAbbr=="id")]] <- idahoHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="id")]] <-  read.csv(priceFiles[grep("idAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="id")] <- -0.437
  urbElast[which(stAbbr=="id")] <- -0.82
}
##read in Montana trade regions
if("mt" %in% stAbbr){
  montHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="30",]
  montHydroReg$basinName <- montHydroReg$NAME
  stateRegSpData[[which(stAbbr=="mt")]] <- montHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="mt")]] <-  read.csv(priceFiles[grep("mtAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="mt")] <- -0.437
  urbElast[which(stAbbr=="mt")] <- -0.82
}
##read in New Mexico trade regions
if("nm" %in% stAbbr){
  nemexHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="35",]
  nemexHydroReg$basinName <- nemexHydroReg$NAME
  nemexHydroReg$basinName[grep("ñ", nemexHydroReg$basinName)] <- "Dona Ana"
  stateRegSpData[[which(stAbbr=="nm")]] <- nemexHydroReg
  stateNames[which(stAbbr=="nm")] <- "newMexico"
  ##state region prices
  statePricesByReg[[which(stAbbr=="nm")]] <-  read.csv(priceFiles[grep("nmAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="nm")] <- -0.564
  urbElast[which(stAbbr=="nm")] <- -2.017
}
##read in Nevada trade regions
if("nv" %in% stAbbr){
  nevadHydroReg <- wmasLayer[wmasLayer$state=="Nevada",]
  nevadHydroReg$basinName <- nevadHydroReg$basinNum
  ##add the leading 0s to the row and column names
  swHistTrdRelations[[which(stAbbr=="nv")]]$BuyerRegion[which(nchar(swHistTrdRelations[[which(stAbbr=="nv")]]$BuyerRegion)==2)] <- paste0("0", swHistTrdRelations[[which(stAbbr=="nv")]]$BuyerRegion[which(nchar(swHistTrdRelations[[which(stAbbr=="nv")]]$BuyerRegion)==2)])
  swHistTrdRelations[[which(stAbbr=="nv")]]$SellerRegion[which(nchar(swHistTrdRelations[[which(stAbbr=="nv")]]$SellerRegion)==2)] <- paste0("0", swHistTrdRelations[[which(stAbbr=="nv")]]$SellerRegion[which(nchar(swHistTrdRelations[[which(stAbbr=="nv")]]$SellerRegion)==2)])
  stateRegSpData[[which(stAbbr=="nv")]] <- nevadHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="nv")]] <-  read.csv(priceFiles[grep("nvAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="nv")] <- -0.579
  urbElast[which(stAbbr=="nv")] <- -2.389
}
##read in Oregon trade regions
if("or" %in% stAbbr){
  oregHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="41",]
  oregHydroReg$basinName <- toupper(oregHydroReg$NAME)
  stateRegSpData[[which(stAbbr=="or")]] <- oregHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="or")]] <-  read.csv(priceFiles[grep("orAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="or")] <- -0.437
  urbElast[which(stAbbr=="or")] <- -0.82
}
##read in Utah trade regions
if("ut" %in% stAbbr){
  utahHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="49",]
  utahHydroReg$basinName <- toupper(utahHydroReg$NAME)
  stateRegSpData[[which(stAbbr=="ut")]] <- utahHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="ut")]] <-  read.csv(priceFiles[grep("utAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="ut")] <- -0.579
  urbElast[which(stAbbr=="ut")] <- -2.389
}
##read in Washington trade regions
if("wa" %in% stAbbr){
  washHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="53",]
  washHydroReg$basinName <- washHydroReg$NAME
  stateRegSpData[[which(stAbbr=="wa")]] <- washHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="wa")]] <-  read.csv(priceFiles[grep("waAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="wa")] <- -0.437
  urbElast[which(stAbbr=="wa")] <- -0.82
}
##read in Wyoming trade regions
if("wy" %in% stAbbr){
  wyomHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="56",]
  wyomHydroReg$basinName <- wyomHydroReg$NAME
  stateRegSpData[[which(stAbbr=="wy")]] <- wyomHydroReg
  ##state region prices
  statePricesByReg[[which(stAbbr=="wy")]] <-  read.csv(priceFiles[grep("wyAve", priceFiles)])
  ##set the state elasticities
  agrElast[which(stAbbr=="wy")] <- -0.437
  urbElast[which(stAbbr=="wy")] <- -0.82
}

##Place the WMA spatial layers in a list, to be able to include the 
##AZ groundwater boundaries
wmasData <- list(wmasLayer, azGrdWMAs)


##fitting the market model and modifying the cumulative curves for the "basic" scenario,
##in which data from all years is available to be traded
##this is equivalent to Scenario 1 in the paper
if("basic" %in% markAnalysisMode){
  ##aggregate the WMAS volumes into regions, to create region volumes
  stateRegVol_basic <- mapply(convertWMAVolToTrdRegions, st=stateNames, stPrices=statePricesByReg, stateHydroReg=stateRegSpData, MoreArgs=list(surRights=wmasRightsTotsSur, grdRights=wmasRightsTotsGrd, wmaLayer=wmasData, areaProj=projForAreaCalc), SIMPLIFY=F)
  ##write out percent of each WMA within each Trade Region
  stateRegWMAPer <- lapply(stateRegVol_basic, function(lst){lst[[2]]})
  ##write out the pre economic modeling trade region volumes and prices
  stateRegVol_basic <- lapply(stateRegVol_basic, function(lst){lst[[1]]})
  
  ##preform the market model fitting, to determine who should trade with who and what amount of water rights should be traded
  stEconModels_basic <- mapply(econModelStates, spReg=stateRegVol_basic, swTrdRels=swHistTrdRelations, gwTrdRels=gwHistTrdRelations, agElast=agrElast, urbElast=urbElast, MoreArgs=list(trdType="historical"), SIMPLIFY=F)
  ##write out fitted market model results, for future referencing
  mapply(writeEconModelResults, modResults=stEconModels_basic, st=stAbbr, wDir=paste0(scenDir, "fullCumuCurves/"))
  
  ##Modify the original water right cumulative curves based on the amount of 
  ##water rights traded. This is done by reverse calculating the amount of 
  ##water rights each WMA contributed to a given trade region and modifying 
  ##the rights gained or lost by that amount.
  modWaterRights_basic <- mapply(applyTrdRegionsToWMAs, st=stateNames, modeledEconVals=stEconModels_basic, spReg=stateRegVol_basic, wmaInReg=stateRegWMAPer,
                                  MoreArgs=list(surRights=wmasRightsTotsSur, grdRights=wmasRightsTotsGrd, wmaLayer=wmasData, areaProj=projForAreaCalc, 
                                                tableWriteDir=paste0(cumuDir, "sectorCumuSummaries"), nameConvTab=namConvData, 
                                                typeApplicaiton=c("earlyPrioDates", "allCurveYrs"), surYrCutTab=wmaCutOffDates))
}

##fitting the market model and modifying the cumulative curves for the "cutoffs" scenario,
##in which the data for some years for some WMA's is limited by a cutoff to signify
##the potential of running out of water
##this is equivalent to Scenario 2 in the paper
if("cutoffs" %in% markAnalysisMode){
  ##aggregate the WMAS volumes into regions, to create region volumes - cutoff dates
  stateRegVol_cutoffs <- mapply(convertWMAVolToTrdRegions, st=stateNames, stPrices=statePricesByReg, stateHydroReg=stateRegSpData, MoreArgs=list(surRights=wmasRightsTotsSur, grdRights=wmasRightsTotsGrd, wmaLayer=wmasData, areaProj=projForAreaCalc, whichYear="cutoff", cutOffYrs=datesWithIDs), SIMPLIFY=F)
  ##write out percent of each WMA within each Trade Region
  stateRegWMAPer <- lapply(stateRegVol_cutoffs, function(lst){lst[[2]]})
  ##write out the pre economic modeling trade region volumes and prices
  stateRegVol_cutoffs <- lapply(stateRegVol_cutoffs, function(lst){lst[[1]]})
  
  ##preform the market model fitting, to determine who should trade with who and what amount of water rights should be traded
  stEconModels_cutoffs <- mapply(econModelStates, spReg=stateRegVol_cutoffs, swTrdRels=swHistTrdRelations, gwTrdRels=gwHistTrdRelations, agElast=agrElast, urbElast=urbElast, MoreArgs=list(trdType="historical"), SIMPLIFY=F)
  ##write out fitted market model results, for future referencing
  mapply(writeEconModelResults, modResults=stEconModels_cutoffs, st=stAbbr, wDir=paste0(scenDir, "waterRunsOutCutoffs_WatInst/"))
  
  ##Modify the original water right cumulative curves based on the amount of 
  ##water rights traded. This is done by reverse calculating the amount of 
  ##water rights each WMA contributed to a given trade region and modifying 
  ##the rights gained or lost by that amount.
  modWaterRights_cutoff <- mapply(applyTrdRegionsToWMAs, st=stateNames, modeledEconVals=stEconModels_cutoffs, spReg=stateRegVol_cutoffs, wmaInReg=stateRegWMAPer,
                                  MoreArgs=list(surRights=wmasRightsTotsSur, grdRights=wmasRightsTotsGrd, wmaLayer=wmasData, areaProj=projForAreaCalc, 
                                                tableWriteDir=paste0(cumuDir, "sectorCumuSummaries"), nameConvTab=namConvData, 
                                                typeApplicaiton=c("earlyPrioDates", "waterRunsOutCutoffs_WatInst"), surYrCutTab=wmaCutOffDates))
}

##fitting the market model and modifying the cumulative curves for the "sgmaCutoffs" scenario,
##in which the data for some years for some WMA's is limited by a cutoff to signify
##the potential of running out of water. The cutoff used based on the hypothetical of CA's
##SGMA rules being adapted throughout the whole Western Region.
##this is equivalent to Scenario 2a in the paper
if("sgmaCutoffs" %in% markAnalysisMode){
  ##aggregate the WMAS volumes into regions, to create region volumes - SMGA cutoff dates
  stateRegVol_cutsgma <- mapply(convertWMAVolToTrdRegions, st=stateNames, stPrices=statePricesByReg, stateHydroReg=stateRegSpData, MoreArgs=list(surRights=wmasRightsTotsSur, grdRights=wmasRightsTotsGrd, wmaLayer=wmasData, areaProj=projForAreaCalc, whichYear="cutoff", cutOffYrs=datesWithIDsSMGA), SIMPLIFY=F)
  ##write out percent of each WMA within each Trade Region
  stateRegWMAPer <- lapply(stateRegVol_cutsgma, function(lst){lst[[2]]})
  ##write out the pre economic modeling trade region volumes and prices
  stateRegVol_cutsgma <- lapply(stateRegVol_cutsgma, function(lst){lst[[1]]})
  
  ##preform the market model fitting, to determine who should trade with who and what amount of water rights should be traded
  stEconModels_cutsgma <- mapply(econModelStates, spReg=stateRegVol_cutsgma, swTrdRels=swHistTrdRelations, gwTrdRels=gwHistTrdRelations, agElast=agrElast, urbElast=urbElast, MoreArgs=list(trdType="historical"), SIMPLIFY=F)
  ##write out fitted market model results, for future referencing
  mapply(writeEconModelResults, modResults=stEconModels_cutsgma, st=stAbbr, wDir=paste0(scenDir, "waterRunsOutCutoffs_WatInst_SMGA/"))
  
  ##Modify the original water right cumulative curves based on the amount of 
  ##water rights traded. This is done by reverse calculating the amount of 
  ##water rights each WMA contributed to a given trade region and modifying 
  ##the rights gained or lost by that amount. 
  modWaterRights_sgmaCut <- mapply(applyTrdRegionsToWMAs, st=stateNames, modeledEconVals=stEconModels_cutsgma, spReg=stateRegVol_cutsgma, wmaInReg=stateRegWMAPer,
                                    MoreArgs=list(surRights=wmasRightsTotsSur, grdRights=wmasRightsTotsGrd, wmaLayer=wmasData, areaProj=projForAreaCalc, 
                                                  tableWriteDir=paste0(cumuDir, "sectorCumuSummaries"), nameConvTab=namConvData, 
                                                  typeApplicaiton=c("earlyPrioDates", "waterRunsOutCutoffs_WatInst_SMGA"), surYrCutTab=wmaCutOffDatesSMGA))
}


