####################################################################################
####################################################################################
## Script Name: 1_WelfareCaculation_actual_trades.R##
## Purpose of the script: This script is built on the market model script written by Matt Lisk 
## in the marketModeling script. It shares some portion of codes in the hydroRealism_marketModel.R
## script, and it is modified to calculate the welfare gains using the amount of water rights 
## traded from the Waterlitix data. Specifically, it uses demand curves of water rights estimated
## by the water market and the amount of water rights traded by each WMAs to calculate the welfare
## gains from water rights trading. The output of this script is an excel file of the amount of welfare
## gains by WMAs. 
##
##
##
## Special Requirements: Cumulated water rights; Elasticity values - from WWR; 
## Prices for each modeling sector to pay - from WWR; spatial WMA data;
## spatial WWR water trade regions; actual amount of water rights traded
##
## Author: Jiameng Zheng
## Email: jiamengz@illinois.edu
## 
##
##
######################################################################################
######################################################################################



options(stringsAsFactors=F, scipen=999)

##setting the paths of various directories
docDir <-"C:/Users/jiamengz/OneDrive/PCHES/"

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
library(dplyr)
library(sf)
library(usdata)
library(ggplot2)
library(maps)
library(ggnewscale)
library(rnaturalearth)
library("ggrepel")
#dDir <- "C:/Users/jiamengz/OneDrive/PCHES/data/GIS files"
#ne_download(type="populated_places", category="cultural", scale="small", destdir=dDir)


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
#statePricesByReg <- read.csv(paste0(wwrDir, "caAvePriceToPayReg_updatedVals.csv"))
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
#mcmapply() #is a thing
##write out the pre economic modeling trade region volumes and prices
#stRegVolNames <- paste0(scenDir, stAbbr, "TrdRegVolandPrices.csv")
#mapply(write.csv, stateRegVol, stRegVolNames, MoreArgs=list(row.names=F))


#cores <- min(c(6, length(stAbbr)))
stEconModels <- mapply(econModelStates, spReg=stateRegVol, swTrdRels=swHistTrdRelations, gwTrdRels=gwHistTrdRelations, agElast=agrElast, urbElast=urbElast, MoreArgs=list(trdType="historical"), SIMPLIFY=F)

#stEconModels <- mcmapply(econModelStates, spReg=stateRegVol, swTrdRels=swHistTrdRelations, gwTrdRels=gwHistTrdRelations, agElast=agrElast, urbElast=urbElast, MoreArgs=list(trdType="historical"), SIMPLIFY=F, mc.cores=cores)
##write out results based on actual trading relationships from WWR data
mapply(writeEconModelResults, modResults=stEconModels, st=stAbbr, wDir=scenDir)


##########################################################################
##read in actual trade data
getRegNames <- lapply(stateRegSpData, function(spD){return(spD$basinName)})
swHistTrdVals <- mapply(createActualTrdVals, findSWTrdRels, allRegNames=getRegNames, SIMPLIFY=F)
gwHistTrdVals <- mapply(createActualTrdVals, findGWTrdRels, allRegNames=getRegNames, SIMPLIFY=F)

############################################################################################
## read in K and P from fitEconCurves step 

summary_df <- as.data.frame(matrix(ncol=9, nrow = 0))
colnames(summary_df) <- c("State", "Region", "SW_rowSum", "SW_colSum", "SW_allSum","GW_rowSum","GW_colSum","GW_allSum", "Trd_allSum")

for (st_tmp in stAbbr) {
  
#st_tmp <- 'nv'
  print(st_tmp)
SWagrK <- read.csv(paste0(scenDir, paste(st_tmp,"MarketTrades_SWagrK.csv",sep="")), row.names=1)
SWurbK <- read.csv(paste0(scenDir, paste(st_tmp,"MarketTrades_SWurbK.csv",sep="")), row.names=1)

SWurbP <- read.csv(paste0(scenDir, paste(st_tmp,"MarketTrades_SWurbP.csv",sep="")), row.names=1)

GWagrK <- read.csv(paste0(scenDir, paste(st_tmp,"MarketTrades_GWagrK.csv",sep="")), row.names=1)
GWurbK <- read.csv(paste0(scenDir, paste(st_tmp,"MarketTrades_GWurbK.csv",sep="")), row.names=1)

GWurbP <- read.csv(paste0(scenDir, paste(st_tmp,"MarketTrades_GWurbP.csv",sep="")), row.names=1)

  k <- which(stAbbr == st_tmp)
  print(k)
  swTrdVals <- as.data.frame(swHistTrdVals[[k]])
  gwTrdVals <- as.data.frame(gwHistTrdVals[[k]])
  Qbar <- as.data.frame(stateRegVol[[k]])
  agElast <- agrElast[[k]]
  urElast <- urbElast[[k]]
  agrQuant <- Qbar$SWAgrBefore
  urbQuant <- Qbar$SWUrbBefore
  agrQuantG <- Qbar$GWAgrBefore
  urbQuantG <- Qbar$GWUrbBefore
  
  swTrdVals[is.na(swTrdVals)] <- 0
  gwTrdVals[is.na(gwTrdVals)] <- 0

  TrdVals <- swTrdVals + gwTrdVals
  pricebar <- statePricesByReg[[k]]

##################  clean more of water quantity from water exchange ##################
  spReg <- as.data.frame(stateRegVol[[k]]) 
  outMatSWAgVol <- matrix(data=NA, nrow=length(spReg$basinName), ncol=length(spReg$basinName))
  colnames(outMatSWAgVol) <- spReg$basinName
  rownames(outMatSWAgVol) <- spReg$basinName
  
  outMatGWAgVol <- matrix(data=NA, nrow=length(spReg$basinName), ncol=length(spReg$basinName))
  colnames(outMatGWAgVol) <- spReg$basinName
  rownames(outMatGWAgVol) <- spReg$basinName
    #trdType <- "historical"
  #agElast <- agrElast[n]
  #urbElast <- urbElast[n]
  swTrdRels <- swHistTrdRelations[[k]]
  gwTrdRels <- gwHistTrdRelations[[k]]

  
  swsellerNamelist<-unique(swTrdRels$SellerRegion)
  
  for (sellerName in swsellerNamelist) {

  #sellerName <- "Sacramento River"
  #sapply(unique(c(swTrdRels$SellerRegion,gwTrdRels$SellerRegion)), function(sellerName){
    sellerInd <- which(spReg$basinName==sellerName)
  ##identify the trade relationships that the seller has historically had
  ##from all trades, isolate those where water was specifically being traded to trade partner's urban sectors
  swHistTrds <- swTrdRels[swTrdRels$SellerRegion==sellerName & swTrdRels$agSoldToUrb>0,]
  ##ID the trade partners that are not self
  swHistTrdPart <- swHistTrds$BuyerRegion

  ##checking to see if trade went both ways
  ##if so, only model the larger trading relationship
  swHistTrdPart <- sapply(swHistTrdPart, function(trdPart){trdPartRecs<-swTrdRels[swTrdRels$SellerRegion==trdPart & swTrdRels$agSoldToUrb>0,]
  if(sellerName %in% trdPartRecs$BuyerRegion){
    if(trdPartRecs[trdPartRecs$BuyerRegion==sellerName,"agSoldToUrb"] > swHistTrds[swHistTrds$BuyerRegion==trdPart,"agSoldToUrb"]){
      return(trdPart)
    }else{
      if(trdPart==sellerName){
        return(trdPart)
      }else{
        return(NA)
      }
    }
  }else{
    return(trdPart)
  }})
 
  ##if needed, remove NAs from the checking of two way trading
  if(TRUE %in% unique(is.na(swHistTrdPart))){
    swHistTrdPart <- swHistTrdPart[-c(which(is.na(swHistTrdPart)==T))]
  }
  regionSWAgVol  <- rep(NA, length(spReg$basinName))
  swBuyersInd <- sapply(swHistTrdPart, function(x){which(spReg$basinName==x)})

  ##extract the original water rights into a vector to preform the economic modeling
  if(length(swBuyersInd)>0 & length(sellerInd)>0){
  regionSWAgVol[swBuyersInd] <- spReg$SWAgrBefore[sellerInd]
  #regionSWAgPr[swBuyersInd] <- spReg$swAvePrAgrPay[sellerInd]
  outMatSWAgVol[sellerInd,] <- regionSWAgVol

  }

   
  }

agquant <- as.data.frame(outMatSWAgVol)
if(all(is.na(agquant) == TRUE )) {
  agrQuant <- rowSums(is.na(agquant) == ncol(agquant))
  #agrQuantG <- as.data.frame(agrQuantG)
} else {
agrQuant <- apply(agquant, 1, function(x) unique(na.omit(x)))
agrQuant[lengths(agrQuant)==0] <- 0
#agrQuant2 <- data.frame(matrix(unlist(agrQuant), nrow=length(agrQuant)))
agrQuant <- as.data.frame(agrQuant)
agrQuant <- t(agrQuant)
rownames(agrQuant) <- gsub("[.]", " ", rownames(agrQuant))
#agrQuant <- as.data.frame(agrQuant)
#names(agrQuant)[names(agrQuant) == 'V1'] <- "agrQuant"
}


gwsellerNamelist<-unique(gwTrdRels$SellerRegion)
for (sellerName in gwsellerNamelist) {
  
  #sellerName <- "Sacramento River"
  #sapply(unique(c(swTrdRels$SellerRegion,gwTrdRels$SellerRegion)), function(sellerName){
  sellerInd <- which(spReg$basinName==sellerName)
  ##identify the trade relationships that the seller has historically had
  ##from all trades, isolate those where water was specifically being traded to trade partner's urban sectors
  gwHistTrds <- gwTrdRels[gwTrdRels$SellerRegion==sellerName & gwTrdRels$agSoldToUrb>0,]
  ##ID the trade partners that are not self
  gwHistTrdPart <- gwHistTrds$BuyerRegion

  ##checking to see if trade went both ways
  ##if so, only model the larger trading relationship

  gwHistTrdPart <- sapply(gwHistTrdPart, function(trdPart){trdPartRecs<-gwTrdRels[gwTrdRels$SellerRegion==trdPart & gwTrdRels$agSoldToUrb>0,]
  if(sellerName %in% trdPartRecs$BuyerRegion){
    if(trdPartRecs[trdPartRecs$BuyerRegion==sellerName,"agSoldToUrb"] > gwHistTrds[gwHistTrds$BuyerRegion==trdPart,"agSoldToUrb"]){
      return(trdPart)
    }else{
      if(trdPart==sellerName){
        return(trdPart)
      }else{
        return(NA)
      }
    }
  }else{
    return(trdPart)
  }})
  ##if needed, remove NAs from the checking of two way trading
 
  if(TRUE %in% unique(is.na(gwHistTrdPart))){
    gwHistTrdPart <- gwHistTrdPart[-c(which(is.na(gwHistTrdPart)==T))]
  }
  regionGWAgVol <- regionGWAgPr <- rep(NA, length(spReg$basinName))
  gwBuyersInd <- sapply(gwHistTrdPart, function(x){which(spReg$basinName==x)})
  # 
  ##extract the original water rights into a vector to preform the economic modeling

  
  if(length(gwBuyersInd)>0 & length(sellerInd)>0){
    regionGWAgVol[gwBuyersInd] <- spReg$GWAgrBefore[sellerInd]
    outMatGWAgVol[sellerInd,] <- regionGWAgVol
  }
  
}
agquantg <- as.data.frame(outMatGWAgVol)

if(all(is.na(agquantg) == TRUE )) {
  agrQuantG <- rowSums(is.na(agquantg) == ncol(agquantg))
  #agrQuantG <- as.data.frame(agrQuantG)
} else {
  agrQuantG <- apply(agquantg, 1, function(x) unique(na.omit(x)))
  agrQuantG[lengths(agrQuantG)==0] <- 0
  #agrQuant2 <- data.frame(matrix(unlist(agrQuant), nrow=length(agrQuant)))
  agrQuantG <- as.data.frame(agrQuantG)
  agrQuantG <- t(agrQuantG)
  rownames(agrQuantG) <- gsub("[.]", " ", rownames(agrQuantG))
}



##################   calculate welfare of surface water ####################
  # If the amount traded is higher than the amount agr has, set the amount traded as what the agr has #
  swTrdVals[agrQuant < swTrdVals] <- swTrdVals[agrQuant < swTrdVals] + (agrQuant - swTrdVals)[agrQuant < swTrdVals]
  Qagr <- agrQuant - swTrdVals
  Qagr <- mutate_all(Qagr, function (x) pmax(x,0))
  Qurb <- as.data.frame(t(urbQuant + t(swTrdVals)))
  # <- urbQuant + swTrdVals
  
  agrQuant_df <- data.frame(matrix(ncol = ncol(swTrdVals), nrow = nrow(swTrdVals)))
  colnames(agrQuant_df) <- colnames(swTrdVals)
  rownames(agrQuant_df) <- rownames(swTrdVals)
  
  for(i in 1:ncol(agrQuant_df)) {
    agrQuant_df[,i] <- agrQuant
  }
  
  urbQuant_df <- data.frame(matrix(ncol = ncol(swTrdVals), nrow = nrow(swTrdVals)))
  colnames(urbQuant_df) <- colnames(swTrdVals)
  rownames(urbQuant_df) <- rownames(swTrdVals)
  
  for(i in 1:nrow(urbQuant_df)) {
    urbQuant_df[i,] <- Qbar$SWUrbBefore
  }
  
  SWurbK[is.na(SWurbK)] <- 0
  SWagrK[is.na(SWagrK)] <-0
  SWurbP[is.na(SWurbP)] <-0
  

  my_func1 <- function(i, SWurbK, urElast) {
    if (SWurbK == 0) {
      NA
    } else {
      (i/SWurbK)^(1/urElast)
    }
  }
  
  integrate_func1 <- function(SWurbK, urElast, urbQuant_df, Qurb, SWurbP) {
    if (is.na(SWurbK) || is.na(urElast) || is.na(urbQuant_df) || is.na(Qurb)){
      return(NA)
    } else {
      if (SWurbK == 0) {
        return(NA)
      } else if (urbQuant_df == 0) {
        return(NA) 
      } else if (SWurbP == 0) {
        return(NA) 
      } else {
      integrate(my_func1, lower=urbQuant_df, upper=Qurb, SWurbK=SWurbK, urElast=urElast)$value
    }
  }
} 

  
  vintegrate_func1 <- Vectorize(integrate_func1)

  
  welUrb <- data.frame(matrix(nrow=nrow(swTrdVals), ncol=ncol(swTrdVals)))
  colnames(welUrb) <- colnames(swTrdVals)
  rownames(welUrb) <- rownames(swTrdVals)  
  for (i in 1:nrow(welUrb)){
    for (j in 1:ncol(welUrb)){
      welUrb[i,j] <- integrate_func1(SWurbK[i,j], urElast, urbQuant_df[i,j], Qurb[i,j], SWurbP[i,j])
    }
  }
 
  my_func2 <- function(i, SWagrK, agElast) {
    if (SWagrK == 0) {
      NA
    } else {
      (i/SWagrK)^(1/agElast)
    }
  }
  integrate_func2 <- function(SWagrK, agElast, agrQuant_df, Qagr, SWurbP) {
    if (is.na(SWagrK) || is.na(agElast) || is.na(agrQuant_df) || is.na(Qagr)){
      return(NA)
    } else {
      if (SWagrK == 0) {
        return(NA)
      } else if (agrQuant_df == 0) {
        return(NA) 
      } else if (Qagr == 0) {
        return(NA)
      } else if (SWurbP == 0) {
        return(NA)
      } else {
      integrate(my_func2, lower=agrQuant_df, upper=Qagr, SWagrK=SWagrK, agElast=agElast)$value
    }
  }
}

  vintegrate_func2 <- Vectorize(integrate_func2)
  welAgr <- data.frame(matrix(nrow=nrow(swTrdVals), ncol=ncol(swTrdVals)))
  colnames(welAgr) <- colnames(swTrdVals)
  rownames(welAgr) <- rownames(swTrdVals)  
  for (i in 1:nrow(welAgr)){
    for (j in 1:ncol(welAgr)){
      #print(SWagrK[i,j])
      #print(agrQuant_df[i,j])
      #print(Qagr[i,j])
      welAgr[i,j] <- integrate_func2(SWagrK[i,j], agElast, agrQuant_df[i,j], Qagr[i,j], SWurbP[i,j])
    }
  }
  
  squareUrb <- (Qurb - urbQuant_df)*SWurbP
  
  SWagr <- mutate_all(welAgr, function (x) as.numeric(as.character(x)))
  SWagr <- mutate_all(SWagr, function (x) pmin(x, 0))
  #SWagr_sum <- sum(SWagr, na.rm=TRUE)
  
  SWurb <- mutate_all(welUrb, function (x) as.numeric(as.character(x)))
  SWurb <- mutate_all(SWurb, function (x) pmax(x, 0))
  #SWurb_sum <- sum(SWurb, na.rm=TRUE)
  SWurb[is.na(SWurb)] <-0
  SWagr[is.na(SWagr)] <-0
  SW_dff <- SWurb + SWagr
  
  SW_urbsquare <- mutate_all(squareUrb, function (x) pmax(x,0))
  
  # this step calculates consumer surplus, and set the CS = 0 if the CS is negative #
  SW_CS <- SWurb - SW_urbsquare
  SW_CS <- mutate_all(SW_CS, function (x) pmax(x, 0))
  
  # this step calculates producer surplus, and set the PS = 0 if the CS is negative #
  SW_PS <- SW_dff - SW_CS
  SW_PS <- mutate_all(SW_PS, function (x) pmax(x, 0))
  
  SW_colSum <- colSums(SW_CS, na.rm=TRUE)
  SW_rowSum <- rowSums(SW_PS, na.rm=TRUE)
  SW_colSum <- pmax(SW_colSum,0)
  SW_rowSum <- pmax(SW_rowSum,0)
  SW_allSum <- SW_colSum + SW_rowSum
  
  
  
  #SW_colSum_df = data.frame(region=names(SW_colSum), SW_colSum=SW_colSum, row.names=NULL)
  #SW_rowSum_df = data.frame(region=names(SW_rowSum), SW_rowSum=SW_rowSum, row.names=NULL)
  #SW_allSum_df = data.frame(region=names(SW_allSum), SW_allSum=SW_allSum, row.names=NULL)
  SW_colSum_df = as.data.frame(SW_colSum)
  rownames(SW_colSum_df) <- NULL
  SW_colSum_df$Region = names(SW_rowSum)
  SW_rowSum_df = as.data.frame(SW_rowSum)
  rownames(SW_rowSum_df) <- NULL
  SW_rowSum_df$Region = names(SW_rowSum)
  SW_allSum_df = as.data.frame(SW_allSum)
  rownames(SW_allSum_df) <- NULL
  SW_allSum_df$Region = names(SW_rowSum)
  
  tmp_SW_df = merge(merge(SW_rowSum_df,SW_colSum_df,by='Region'), SW_allSum_df, by='Region')
  
  
 ########## work on GW ################ 
  
  #if (st_tmp == "ca") {
   # tmp_GW_df <- data.frame(matrix(ncol = 4, nrow = nrow(gwTrdVals)))
  #  colnames(tmp_GW_df) <- c("Region", "GW_rowSum","GW_colSum","GW_allSum")
  #  tmp_GW_df$Region <- tmp_SW_df$Region
  #  tmp_GW_df[is.na(tmp_GW_df)] <- 0
    
#  } else {
    
  # if trade more than agr has before, change the quantity to what ag has before 
  gwTrdVals[agrQuantG < gwTrdVals] <- gwTrdVals[agrQuantG < gwTrdVals] + (agrQuantG - gwTrdVals)[agrQuantG < gwTrdVals]
  
  #gwTrdVals[agrQuantG < gwTrdVals] <-agrQuantG[agrQuantG < gwTrdVals]
  
  QagrG <- agrQuantG - gwTrdVals 
  QagrG <- mutate_all(QagrG, function (x) pmax(x,0))
  QurbG <- as.data.frame(t(urbQuantG + t(gwTrdVals)))
  #QurbG <- urbQuantG + gwTrdVals
  
  agrQuantG_df <- data.frame(matrix(ncol = ncol(gwTrdVals), nrow = nrow(gwTrdVals)))
  colnames(agrQuantG_df) <- colnames(gwTrdVals)
  rownames(agrQuantG_df) <- rownames(gwTrdVals)
  
  for(i in 1:ncol(agrQuantG_df)) {
    agrQuantG_df[,i] <- agrQuantG
  }
  
  urbQuantG_df <- data.frame(matrix(ncol = ncol(gwTrdVals), nrow = nrow(gwTrdVals)))
  colnames(urbQuantG_df) <- colnames(gwTrdVals)
  rownames(urbQuantG_df) <- rownames(gwTrdVals)
  
  for(i in 1:nrow(urbQuantG_df)) {
    urbQuantG_df[i,] <- Qbar$GWUrbBefore
  }

  
  
  GWurbK[is.na(GWurbK)] <- 0
  GWagrK[is.na(GWagrK)] <-0
  GWurbP[is.na(GWurbP)] <-0
  

  my_func3 <- function(i, GWurbK, urElast) {
    if (GWurbK == 0) {
      NA
    } else {
      (i/GWurbK)^(1/urElast)
    }
  }
  
  
  integrate_func3 <- function(GWurbK, urElast, urbQuantG_df, QurbG, GWurbP) {
    if (is.na(GWurbK) || is.na(urElast) || is.na(urbQuantG_df) || is.na(QurbG)){
      return(NA)
    } else {
      if (GWurbK == 0){
        return(NA)
      } else if (urbQuantG_df == 0) {
        return(NA)
      } else if (GWurbP == 0) {
        return(NA)
      } else {
        integrate(my_func3, lower=urbQuantG_df, upper=QurbG, GWurbK=GWurbK, urElast=urElast)$value
      }
    }
  }
  vintegrate_func3 <- Vectorize(integrate_func3)
  
  
  welUrbG <- data.frame(matrix(nrow=nrow(gwTrdVals), ncol=ncol(gwTrdVals)))
  colnames(welUrbG) <- colnames(gwTrdVals)
  rownames(welUrbG) <- rownames(gwTrdVals)  
  for (i in 1:nrow(welUrbG)){
    for (j in 1:ncol(welUrbG)){
      welUrbG[i,j] <- integrate_func3(GWurbK[i,j], urElast, urbQuantG_df[i,j], QurbG[i,j], GWurbP[i,j])
    }
  }
  
  my_func4 <- function(i, GWagrK, agElast) {
    if (GWagrK == 0) {
      NA
    } else {
      (i/GWagrK)^(1/agElast)
    }
  }
  integrate_func4 <- function(GWagrK, agElast, agrQuantG_df, QagrG, GWurbP) {
    if (is.na(GWagrK) || is.na(agElast) || is.na(agrQuantG_df) || is.na(QagrG)){
      return(NA)
    } else {
      if (GWagrK == 0){
        return(NA)
      } else if (agrQuantG_df == 0) {
        return(NA)
      } else if (QagrG == 0) {
        return(NA)
      } else if (GWurbP == 0) {
        return(NA)
      } else {
        integrate(my_func4, lower=agrQuantG_df, upper=QagrG, GWagrK=GWagrK, agElast=agElast)$value
      }
    }
  }
  
  
  vintegrate_func4 <- Vectorize(integrate_func4)
  welAgrG <- data.frame(matrix(nrow=nrow(gwTrdVals), ncol=ncol(gwTrdVals)))
  colnames(welAgrG) <- colnames(gwTrdVals)
  rownames(welAgrG) <- rownames(gwTrdVals)  
  for (i in 1:nrow(welAgrG)){
    for (j in 1:ncol(welAgrG)){
      welAgrG[i,j] <- integrate_func4(GWagrK[i,j], agElast, agrQuantG_df[i,j], QagrG[i,j], GWurbP[i,j])
    }
  }
  
  squareUrbG <- (QurbG - urbQuantG_df)*GWurbP
  
  GWagr <- mutate_all(welAgrG, function (x) as.numeric(as.character(x)))
  GWagr <- mutate_all(GWagr, function (x) pmin(x, 0))
  #SWagr_sum <- sum(SWagr, na.rm=TRUE)
  
  GWurb <- mutate_all(welUrbG, function (x) as.numeric(as.character(x)))
  GWurb <- mutate_all(GWurb, function (x) pmax(x, 0))
  #SWurb_sum <- sum(SWurb, na.rm=TRUE)
  GWurb[is.na(GWurb)] <-0
  GWagr[is.na(GWagr)] <-0
  GW_dff <- GWurb + GWagr
  # this is the A+B part in the welfare graph#
  
  GW_urbsquare <- mutate_all(squareUrbG, function (x) pmax(x,0))
  
  GW_CS <- GWurb - GW_urbsquare
  GW_CS <- mutate_all(GW_CS, function (x) pmax(x, 0))
  # this is consumer surplus part #
  GW_PS <- GW_dff - GW_CS
  GW_PS <- mutate_all(GW_PS, function (x) pmax(x, 0))
  # this is the producer surplus part #
  
  
  GW_colSum <- colSums(GW_CS, na.rm=TRUE)
  GW_rowSum <- rowSums(GW_PS, na.rm=TRUE)
  GW_colSum <- pmax(GW_colSum,0)
  GW_rowSum <- pmax(GW_rowSum,0)
  GW_allSum <- GW_colSum + GW_rowSum
  
  GW_colSum_df = as.data.frame(GW_colSum)
  rownames(GW_colSum_df) <- NULL
  GW_colSum_df$Region = names(SW_rowSum)
  GW_rowSum_df = as.data.frame(GW_rowSum)
  rownames(GW_rowSum_df) <- NULL
  GW_rowSum_df$Region = names(SW_rowSum)
  GW_allSum_df = as.data.frame(GW_allSum)
  rownames(GW_allSum_df) <- NULL
  GW_allSum_df$Region = names(SW_rowSum)
  
  Trd_colSum <- colSums(TrdVals, na.rm=TRUE)
  Trd_rowSum <- rowSums(TrdVals, na.rm=TRUE)
  Trd_allSum <- Trd_colSum + Trd_rowSum
  Trd_allSum_df = as.data.frame(Trd_allSum)
  Trd_allSum_df$Region = names(Trd_allSum)
  tmp_GW_df = merge(merge(GW_rowSum_df,GW_colSum_df,by='Region'), GW_allSum_df, by='Region')
  #}
  tmp_df = merge(tmp_SW_df, tmp_GW_df, by='Region') # combine GW and SW temporary dataframe
  tmp_df$State = st_tmp # add state column

  
  tmp_df <- tmp_df[, c(8,1,2,3,4,5,6,7)] # reorder the column
  
  # append to the dataframe
  #summary_df[nrow(summary_df)+1, ]=c(st_tmp, SWagr_sum, SWurb_sum, SW_diff, GWagr_sum, GWurb_sum, GW_diff, welfare_combine)
  tmp_df2 <- merge(tmp_df, Trd_allSum_df, by="Region")
  summary_df <- rbind(summary_df, tmp_df2)

  
}
  
summary_df$welfare_combined <- summary_df$SW_allSum+summary_df$GW_allSum
summary_df$state_name <- toupper(summary_df$State)
summary_df$state_name<- state.name[match(summary_df$state_name, state.abb)] 
summary_df$welfare <- summary_df$welfare_combined * 60 * 60 * 6 * 100

summary_df$welfare[summary_df$welfare == 0] <- NA
summary_df$welfare[summary_df$welfare < 0] <- NA
boundary <- st_read("C:/Users/jiamengz/OneDrive/PCHES/data/welfare_calculation/Data/backgroundGISData/WWRtradeBounds.shp")
boundary <- st_transform(boundary, crs = 4269)
df_spjoin <- merge(boundary, summary_df, by.x="basinName", by.y="Region", all.x=TRUE)
df_spjoin$welfare <- as.numeric(df_spjoin$welfare)
df_spjoin$logwel <- log(df_spjoin$welfare)
df_spjoin$welinmill <- df_spjoin$welfare/1000000
#welfare_old <- readxl::read_xlsx("C:/Users/jiamengz/OneDrive/PCHES/data/welfare_calculation/Data/welfare_summary_actual_trades_df_ml.xlsx")
#df_spjoin <- merge(df_spjoin, welfare_old, by.x=c("State", "basinName"), by.y=c("State", "Region"))

df_spjoin$Noapplicable <- ifelse(df_spjoin$Trd_allSum==0, 0, 1)
df_spjoin$Noapplicable[df_spjoin$welfare_combined != 0] <- 0
df_spjoin$Noapplicable[df_spjoin$Noapplicable==0] <- NA
df_spjoin$Noapplicable <- as.factor(df_spjoin$Noapplicable)
df_spjoin$ps <-df_spjoin$SW_rowSum + df_spjoin$GW_rowSum
df_spjoin$ps <- df_spjoin$ps * 60 * 60 * 6 * 100
df_spjoin$ps[df_spjoin$ps == 0] <- NA
df_spjoin$ps[df_spjoin$ps < 0] <- NA
df_spjoin$psinmill <- df_spjoin$ps/1000000
df_spjoin$cs <-df_spjoin$SW_colSum+ df_spjoin$GW_colSum
df_spjoin$cs <- df_spjoin$cs * 60 * 60 * 6 * 100
df_spjoin$cs[df_spjoin$cs == 0] <- NA
df_spjoin$cs[df_spjoin$cs < 0] <- NA
df_spjoin$csinmill <- df_spjoin$cs/1000000

city <- st_read("C:/Users/jiamengz/OneDrive/PCHES/data/GIS files/ne_10m_populated_places.shp")
bigCities <- c( "San Francisco", "Los Angeles", "San Diego", "Portland", "Seattle", "Spokane", "Boise", "Idaho Falls", "Denver", "Colorado Springs", "Phoenix", "Tucson", "Las Vegas", "Reno", "Salt Lake City", "Provo","Albuquerque", "Las Cruces", "Missoula", "Billings", "Cheyenne", "Casper")
bigcity <- subset(city, NAME %in% bigCities & SOV0NAME == "United States" & ADM1NAME != "Maine"  )
bigcity <- bigcity[!(bigcity$NAME == "Las Vegas" & bigcity$ADM1NAME == "New Mexico" ),]
bigcity <- st_as_sf(bigcity, coords = c("LONGITUDE", "LATITUDE"), crs = 4269)
bigcity <- st_transform(bigcity, crs = 4269)
save(df_spjoin, bigcity,  file = "C:/Users/jiamengz/OneDrive/PCHES/data/welfare_calculation/Data/welfare_data_actual.rdata")


writexl::write_xlsx(summary_df, "C:/Users/jiamengz/OneDrive/PCHES/data/welfare_calculation/Data/welfare_summary_actual_trades_df_ml.xlsx" )
