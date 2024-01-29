##########################################################################
##########################################################################
## Script Name: 2_DemandCurves_simulated_trades.R
## Purpose of the script: This script is built on the market model script written by Matt Lisk 
## in the marketModeling script. It shares some portion of codes in the hydroRealism_marketModel.R
## script, and it is modified to calculate the welfare gains using the amount of water rights 
## traded from the Waterlitix data. Specifically, it uses demand curves of water rights estimated
## by the water market and the amount of water rights would be traded in each WMAs under the equilibrium
## condition to calculate the welfare gains from water rights trading. The outputs of this script
## are a list of excel files that will be used in the 4_WelfareCalculation.R file to calculate the 
## welfare gains by WMAs.
##
##
## Special Requirements: Cumulated water rights; Elasticity values - from WWR; 
## Prices for each modeling sector to pay - from WWR; spatial WMA data;
## spatial WWR water trade regions
##
## Author: Jiameng Zheng
## Email: jiamengz@illinois.edu
##
##
## 
##
##########################################################################
##########################################################################
rm(list=ls()) 
options(stringsAsFactors=F, scipen=999)

##setting the paths of various directories
docDir <- "C:/Users/jiamengz/OneDrive/PCHES/"


projCodeDir <- paste0(docDir, "waterRightsCumulationCurves-master/")
gdrBase <- "C:/Users/jiamengz/OneDrive/PCHES/data/welfare_calculation/Data/"


projGdrDir <- paste0(gdrBase, "waterRightsCumulations/")
wwrDir <- paste0(projGdrDir, "wwrData/")
cumuDir <- paste0(projGdrDir, "output/")
stateDataDir <- paste0(projGdrDir, "inputData/")
outDir <- paste0(cumuDir, "curveFitting/")

##scenerio Dir
#scenDir <- paste0(wwrDir, "withStorage_wmaVol/")
scenDir <- paste0(wwrDir, "withoutStorage_RegionVol/")

##read in libraries
library(BBmisc)
library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library(ggplot2)
library(ggfortify)
library(ggthemes)
library(classInt)
library(reshape2)
library(usdata)
library(parallel)


##source custom functions
source(paste0(docDir, "commonFunctions.R"))
source(paste0(projCodeDir, "WMAAggFunctions_welfare.R"))

##read in the water rights cumulation data - from storage file
load(paste0(cumuDir, "reorganizedData/statesCalcCumulative.RData"))

projForAreaCalc <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

##########################################################################
##read in WWR derived data
priceFiles <- list.files(paste0(wwrDir, "marketTrdSummaries/"), "_updatedVals", full.names=T)
##if needed, remove files so as to not process state
#priceFiles <- priceFiles[-c(1,4:11)]
#priceFiles <- priceFiles[-c(5)]
#priceFiles <- priceFiles[6]
#priceFiles <- priceFiles[c(6,7)]

fileNames <- sapply(strsplit(priceFiles, "/"), "[[", length(strsplit(priceFiles[1],"/")[[1]]))
stAbbr <- sapply(strsplit(fileNames, "Ave"), "[[", 1)
stateNames <- tolower(abbr2state(stAbbr))

##all state tables in one object
statePricesByReg <- lapply(priceFiles, read.csv)
##CA
#statePricesByReg <- read.csv(paste0(wwrDir, "marketTrdSummaries/caAvePriceToPayReg_updatedVals.csv"))
##CO
#statePricesByReg <- read.csv(paste0(wwrDir, "coAvePriceToPayReg_updatedVals.csv"))
##UT
#statePricesByReg <- read.csv(paste0(wwrDir, "utAvePriceToPayReg_updatedVals.csv"))

##read in the relationship tables to determine which trades to consider in modeling for restricting to actual trading relationships
findSWTrdRels <- list.files(paste0(wwrDir, "marketTrdSummaries/"), "MarketSWVol", full.names=T)
findSWTrdRels <- findSWTrdRels[sapply(paste0(stAbbr, "Region"), grep, x=findSWTrdRels)]
#swHistTrdRelations <- lapply(findSWTrdRels, createTrdRels)
swHistTrdRelations <- lapply(findSWTrdRels, read.csv)

findGWTrdRels <- list.files(paste0(wwrDir, "marketTrdSummaries/"), "MarketGWVol", full.names=T)
findGWTrdRels <- findGWTrdRels[sapply(paste0(stAbbr, "Region"), grep, x=findGWTrdRels)]
#gwHistTrdRelations <- lapply(findGWTrdRels, createTrdRels)
gwHistTrdRelations <- lapply(findGWTrdRels, read.csv)

##read in WMA layer
wmasLayer <- readOGR(dsn=paste0(stateDataDir, "allStateWMAs/simpleWMAs.shp"), layer="simpleWMAs")
wmasLayer$basinName[wmasLayer$state=="Colorado" & wmasLayer$basinNum==71] <- "West Dolores Creek"
wmasLayer$basinName[wmasLayer$state=="Nevada"] <- paste(wmasLayer$basinName[wmasLayer$state=="Nevada"], wmasLayer$basinNum[wmasLayer$state=="Nevada"], sep="_")
wmasLayer$basinName[wmasLayer$state=="Montana"] <- paste(wmasLayer$basinName[wmasLayer$state=="Montana"], wmasLayer$basinNum[wmasLayer$state=="Montana"], sep="_")
##Arizona groundwater WMA layer
azGrdWMAs <- readOGR(dsn=paste0(stateDataDir, "allStateWMAs/azGroundWMAs.shp"), layer="azGroundWMAs")

##read in US county layer
usCountiesLayer <- readOGR(dsn=paste0(gdrBase, "backgroundGISData/cb_2018_us_county_500k.shp"), layer="cb_2018_us_county_500k")
usCountiesLayer <- spTransform(usCountiesLayer, crs(wmasLayer))

##read in layer with WMA name conversion, for modified water rights output files
#namConvData <- readOGR(dsn=paste0(projGdrDir, "unhNamingConvention/simpleWMAs_ID/"), layer="simpleWMAs_ID")
#namConvData <- namConvData@data
namConvData <- read.csv(paste0(projGdrDir, "unhNamingConvention/simpleWMAs_v2_ID.csv"))

##read in cutoff table - for the date to add to urban using the 'earlyPrioDates' scenerio
wmaCutOffDates <- read.csv(paste0(projGdrDir, "wma_cutoff_dates_v12.csv"))
#wmaCutOffDates <- read.csv(paste0(projGdrDir, "wma_cutoff_dates_noMarketMod_v22.csv"))
#wmaCutOffDatesSMGA <- read.csv(paste0(projGdrDir, "wma_cutoff_dates_noMarketMod_sgma_v22.csv"))
wmaCutOffDates$Cutoff_Date <- trunc(wmaCutOffDates$Cutoff_Date)
#wmaCutOffDatesSMGA$Cutoff_Date <- trunc(wmaCutOffDatesSMGA$Cutoff_Date)


##merge the cutoff dates with the name conversions, but be able to be used later in the script
#datesWithIDs <- merge(x=namConvData, y=wmaCutOffDates, by.x="ID", by.y="WMA", all.x=T)
#datesWithIDsSMGA <- merge(x=namConvData, y=wmaCutOffDatesSMGA, by.x="ID", by.y="WMA", all.x=T)
##if a cutoff date isn't provided, use the last date
#datesWithIDs$Cutoff_Date[which(is.na(datesWithIDs$Cutoff_Date)==T)] <- 2100
#datesWithIDsSMGA$Cutoff_Date[which(is.na(datesWithIDsSMGA$Cutoff_Date)==T)] <- 2100

stateRegSpData <- list()
agrElast <- rep(NA, length(stAbbr))
urbElast <- rep(NA, length(stAbbr))
stSurRights <- list()
stGrdRights <- list()
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
  ##set the state elasticities
  agrElast[which(stAbbr=="ca")] <- -0.329
  urbElast[which(stAbbr=="ca")] <- -1.207
  ##Nick Hagerty elasticities
  #agrElast[which(stAbbr=="ca")] <- -0.1
  #urbElast[which(stAbbr=="ca")] <- -0.23
}
if("co" %in% stAbbr){
  coloHydroReg <- wmasLayer[wmasLayer$state=="Colorado",]
  #coloHydroReg$basinName[coloHydroReg$basinNum==71] <- "West Dolores Creek"
  coloHydroReg$basinName[grep(" - ", coloHydroReg$basinName)] <- sapply(strsplit(coloHydroReg$basinName[grep(" - ", coloHydroReg$basinName)], " - "), "[[", 2)
  stateRegSpData[[which(stAbbr=="co")]] <- coloHydroReg
  ##set the state elasticities
  agrElast[which(stAbbr=="co")] <- -0.579
  urbElast[which(stAbbr=="co")] <- -2.389
}
if("az" %in% stAbbr){
  boundsToKeep <- c("SANTA CRUZ AMA", "JOSEPH CITY INA", "PRESCOTT AMA", "PHOENIX AMA", "HARQUAHALA INA", "PINAL AMA", "TUCSON AMA", "DOUGLAS INA")
  azGrdWMAs$basinName[-c(which(azGrdWMAs$basinName %in% boundsToKeep))] <- "OUTSIDE AMA / INA"
  
  dissAZ <- unionSpatialPolygons(azGrdWMAs, azGrdWMAs$basinName)
  pid <- sapply(slot(dissAZ,"polygons"),function(x){slot(x,"ID")})
  dissAZDF <- data.frame(Name=pid, row.names=pid)
  arizHydroReg <- SpatialPolygonsDataFrame(dissAZ, data=dissAZDF)
  names(arizHydroReg) <- "basinName"
  stateRegSpData[[which(stAbbr=="az")]] <- arizHydroReg
  ##set the state elasticities
  agrElast[which(stAbbr=="az")] <- -0.564
  urbElast[which(stAbbr=="az")] <- -2.017
}
if("id" %in% stAbbr){
  idahoHydroReg <- wmasLayer[wmasLayer$state=="Idaho",]
  idahoHydroReg$basinName <- idahoHydroReg$basinNum
  stateRegSpData[[which(stAbbr=="id")]] <- idahoHydroReg
  ##set the state elasticities
  agrElast[which(stAbbr=="id")] <- -0.437
  urbElast[which(stAbbr=="id")] <- -0.82
}
if("mt" %in% stAbbr){
  montHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="30",]
  montHydroReg$basinName <- montHydroReg$NAME
  stateRegSpData[[which(stAbbr=="mt")]] <- montHydroReg
  ##set the state elasticities
  agrElast[which(stAbbr=="mt")] <- -0.437
  urbElast[which(stAbbr=="mt")] <- -0.82
}
if("nm" %in% stAbbr){
  nemexHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="35",]
  nemexHydroReg$basinName <- nemexHydroReg$NAME
  nemexHydroReg$basinName[grep("ñ", nemexHydroReg$basinName)] <- "Dona Ana"
  stateRegSpData[[which(stAbbr=="nm")]] <- nemexHydroReg
  stateNames[which(stAbbr=="nm")] <- "newMexico"
  ##set the state elasticities
  agrElast[which(stAbbr=="nm")] <- -0.564
  urbElast[which(stAbbr=="nm")] <- -2.017
}
if("nv" %in% stAbbr){
  nevadHydroReg <- wmasLayer[wmasLayer$state=="Nevada",]
  nevadHydroReg$basinName <- nevadHydroReg$basinNum
  ##add the leading 0s to the row and column names
  #rowNotLongEnough <- which(nchar(rownames(swHistTrdRelations[[which(stAbbr=="nv")]]))<3)
  swHistTrdRelations[[which(stAbbr=="nv")]]$BuyerRegion[which(nchar(swHistTrdRelations[[which(stAbbr=="nv")]]$BuyerRegion)==2)] <- paste0("0", swHistTrdRelations[[which(stAbbr=="nv")]]$BuyerRegion[which(nchar(swHistTrdRelations[[which(stAbbr=="nv")]]$BuyerRegion)==2)])
  swHistTrdRelations[[which(stAbbr=="nv")]]$SellerRegion[which(nchar(swHistTrdRelations[[which(stAbbr=="nv")]]$SellerRegion)==2)] <- paste0("0", swHistTrdRelations[[which(stAbbr=="nv")]]$SellerRegion[which(nchar(swHistTrdRelations[[which(stAbbr=="nv")]]$SellerRegion)==2)])
  stateRegSpData[[which(stAbbr=="nv")]] <- nevadHydroReg
  ##set the state elasticities
  agrElast[which(stAbbr=="nv")] <- -0.579
  urbElast[which(stAbbr=="nv")] <- -2.389
}
if("or" %in% stAbbr){
  oregHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="41",]
  oregHydroReg$basinName <- toupper(oregHydroReg$NAME)
  stateRegSpData[[which(stAbbr=="or")]] <- oregHydroReg
  ##set the state elasticities
  agrElast[which(stAbbr=="or")] <- -0.437
  urbElast[which(stAbbr=="or")] <- -0.82
}
#if("tx" %in% stAbbr){
#  texasHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="48",]
#  agrElast <- c(agrElast, -0.564)
#  urbElast <- c(urbElast, -2.017)
#}
if("ut" %in% stAbbr){
  utahHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="49",]
  utahHydroReg$basinName <- toupper(utahHydroReg$NAME)
  stateRegSpData[[which(stAbbr=="ut")]] <- utahHydroReg
  ##set the state elasticities
  agrElast[which(stAbbr=="ut")] <- -0.579
  urbElast[which(stAbbr=="ut")] <- -2.389
}
if("wa" %in% stAbbr){
  washHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="53",]
  washHydroReg$basinName <- washHydroReg$NAME
  stateRegSpData[[which(stAbbr=="wa")]] <- washHydroReg
  ##set the state elasticities
  agrElast[which(stAbbr=="wa")] <- -0.437
  urbElast[which(stAbbr=="wa")] <- -0.82
}
if("wy" %in% stAbbr){
  wyomHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="56",]
  wyomHydroReg$basinName <- wyomHydroReg$NAME
  stateRegSpData[[which(stAbbr=="wy")]] <- wyomHydroReg
  ##set the state elasticities
  agrElast[which(stAbbr=="wy")] <- -0.437
  urbElast[which(stAbbr=="wy")] <- -0.82
}

##list the spatial layers, to be able to include the az groundwater boundaries
wmasData <- list(wmasLayer, azGrdWMAs)

##aggregate the WMAS volumes into regions, to create region volumes
#stateRegVol <- convertWRVolToTrdRegions("california", statePricesByReg, caliHydroReg, wmasRightsTotsSur, wmasRightsTotsGrd, wmasData, projForAreaCalc)
stateRegVol <- mapply(convertWMAVolToTrdRegions, st=stateNames, stPrices=statePricesByReg, stateHydroReg=stateRegSpData, MoreArgs=list(surRights=wmasRightsTotsSur, grdRights=wmasRightsTotsGrd, wmaLayer=wmasData, areaProj=projForAreaCalc), SIMPLIFY=F)
stateRegWMAPer <- lapply(stateRegVol, function(lst){lst[[2]]})
stateRegVol <- lapply(stateRegVol, function(lst){lst[[1]]})


#cores <- min(c(6, length(stAbbr)))
stEconModels <- mapply(econModelStates, spReg=stateRegVol, swTrdRels=swHistTrdRelations, gwTrdRels=gwHistTrdRelations, agElast=agrElast, urbElast=urbElast, MoreArgs=list(trdType="historical"), SIMPLIFY=F)

#stEconModels <- mcmapply(econModelStates, spReg=stateRegVol, swTrdRels=swHistTrdRelations, gwTrdRels=gwHistTrdRelations, agElast=agrElast, urbElast=urbElast, MoreArgs=list(trdType="historical"), SIMPLIFY=F, mc.cores=cores)
##write out results based on actual trading relationships from WWR data
mapply(writeEconModelResults, modResults=stEconModels, st=stAbbr, wDir=scenDir)

