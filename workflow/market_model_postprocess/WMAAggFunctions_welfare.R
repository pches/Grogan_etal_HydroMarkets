##########################################################################
##########################################################################
## Script Name: WMAAggFunctions_welfare.R
## Purpose of Script: A file which holds the custom function written 
## specifically for this project. It is built on the hyrdoRealism_market
## Model_customFuctions.R. This file is called by almost all of the 
## other script files for one function or another. It is modified to export 
## parameters estimated by the Market Model to calculate welfare gains in
## later scripts.
##
## Special Requirements: None - this is meant to be called by other scripts.
##
## Author: Matthew D. Lisk, Jiameng Zheng
## Email: mdl5548@psu.edu, jiamengz@illinois.edu
## Date Created: 11/12/2019
##
## Last Moddified By: Jiameng Zheng
## Editors Email: Author
## Date Last Edited: 2/15/2023
##
##
##########################################################################
##########################################################################
##custom function for PCHES WMA Aggregations
##########################################################################
##########################################################################
readInCaliPODRecs <- function(caliFile, byUse=T){
  #################
  #caliFile <- filesByUse[1]
  #byUse <- T
  #################
  
  sheets <- 1:4 ##1-WaterRights basic information, 2-ApplicationInfo, 3-PointsOfDiversion, 4-BeneficialUses 
  fileSheets <- lapply(sheets, function(sh){read.xls(caliFile, sheet=sh)})
  
  recordsBase <- fileSheets[[3]][,c("Application.Number", "POD.ID", "Latitude", "Longitude", "POD.Direct.Diversion.Rate", "DD.Unit", "Source", "Watershed")]
  getDate <- merge(x=recordsBase, y=fileSheets[[1]][,c("Application.Number", "Status.Date")], by="Application.Number")
  finishTouch <- merge(x=getDate, y=fileSheets[[4]][,c("Application.Number", "Beneficial.Use")], by="Application.Number")
  
  return(finishTouch)
}
##########################################################################
##########################################################################
reorgCaliRecs <- function(countyRecs, useRecs, entityRecs, wShedRecs){
  #################
  #wUse <- uniWaterUses[11]
  #wUse <- uniWaterUses[3]
  #countyRecs <- h2oDivCountyRecs[h2oDivCountyRecs$waterUse==wUse,]
  #useRecs <- h2oDivUseRecs[h2oDivUseRecs$waterUse==wUse,]
  #entityRecs <- h2oDivEntityRecs[h2oDivEntityRecs$waterUse==wUse,]
  #wShedRecs <- h2oDivWatershedRecs[h2oDivWatershedRecs$waterUse==wUse,]
  #################
  ##objects which hold valid values for various fields
  validCFSUnits <- c("Acre-feet", "Acre-feet per Year", "Cubic Feet per Second", "	Gallons", "Gallons per Day", "Gallons per Minute")
  ##keep only those records with a valid unit type, as unable to properly convert
  valCntCFSUnit <- countyRecs[countyRecs$CFS_units %in% validCFSUnits,]
  valUseCFSUnit <- useRecs[useRecs$CFS_units %in% validCFSUnits,]
  valEntCFSUnit <- entityRecs[entityRecs$CFS_units %in% validCFSUnits,]
  valWsdCFSUnit <- wShedRecs[wShedRecs$CFS_units %in% validCFSUnits,]
  
  ##combine records by county and records by water use
  commonRecs1 <- valCntCFSUnit[valCntCFSUnit$waterRightID %in% valUseCFSUnit$waterRightID,]
  notInUse <- valCntCFSUnit[!(valCntCFSUnit$waterRightID %in% valUseCFSUnit$waterRightID),]
  notInCounty <- valUseCFSUnit[!(valUseCFSUnit$waterRightID %in% valCntCFSUnit$waterRightID),]
  combineRecs1 <- rbind.data.frame(commonRecs1, notInUse, notInCounty)
  
  ##combine the records from the previous combination and records by entity
  notInCombine1 <- valEntCFSUnit[!(valEntCFSUnit$waterRightID %in% combineRecs1$waterRightID),]
  combineRecs2 <- rbind.data.frame(combineRecs1, notInCombine1)
  
  ##combine the records from the previous combination and records by watersheds
  notInCombine2 <- valWsdCFSUnit[!(valWsdCFSUnit$waterRightID %in% combineRecs2$waterRightID),]
  fullyCombinedRecs <- rbind.data.frame(combineRecs2, notInCombine2)
  
  ##remove records without a stated basin
  fullyCombinedRecs <- fullyCombinedRecs[is.na(fullyCombinedRecs$basinName)==F,]
  fullyCombinedRecs <- fullyCombinedRecs[fullyCombinedRecs$basinName!="",]
  ##remove records without a stated source
  fullyCombinedRecs <- fullyCombinedRecs[is.na(fullyCombinedRecs$source)==F,]
  ##remove records without a priority date
  fullyCombinedRecs <- fullyCombinedRecs[is.na(fullyCombinedRecs$priorityDate)==F,]
  fullyCombinedRecs <- fullyCombinedRecs[fullyCombinedRecs$priorityDate!="",]

  ##convert all flow values to CFS
  fullyCombinedRecs$CFS <- as.numeric(fullyCombinedRecs$CFS)
  cfsUnits <- unique(fullyCombinedRecs$CFS_units)
  if(length(cfsUnits)>1 & cfsUnits[1]!="Cubic Feet per Second"){
    if("Acre-feet per Year" %in% cfsUnits){
      afyIndex <- which(fullyCombinedRecs$CFS_units=="Acre-feet per Year")
      fullyCombinedRecs$CFS[afyIndex] <- acreFtYr2ft3Sec(fullyCombinedRecs$CFS[afyIndex])
    }
    if("Gallons per Day" %in% cfsUnits){
      gpdIndex <- which(fullyCombinedRecs$CFS_units=="Gallons per Day")
      fullyCombinedRecs$CFS[gpdIndex] <- galDay2ft3Sec(fullyCombinedRecs$CFS[gpdIndex])
    }
    if("Gallons per Minute" %in% cfsUnits){
      gpmIndex <- which(fullyCombinedRecs$CFS_units=="Gallons per Minute")
      fullyCombinedRecs$CFS[gpmIndex] <- conv_unit(fullyCombinedRecs$CFS[gpmIndex], "gal_per_min", "ft3_per_sec")
    }
    if("Gallons" %in% cfsUnits){  ##from examining a few documents, assume per year
      gIndex <- which(fullyCombinedRecs$CFS_units=="Gallons")
      fullyCombinedRecs$CFS[gIndex] <- conv_unit(fullyCombinedRecs$CFS[gIndex], "us_gal", "ft3") / 365.25
    }
    if("Acre-feet" %in% cfsUnits){  ##from examining a few documents, assume per year
      afIndex <- which(fullyCombinedRecs$CFS_units=="Acre-feet")
      fullyCombinedRecs$CFS[afIndex] <- acreFtYr2ft3Sec(fullyCombinedRecs$CFS[afIndex])
    }
  }

  ##reclassify sources
  gwInd <- lapply(c("GROUNDWATER", "Groundwater", "GROUND WATER"), grep, x=fullyCombinedRecs$source)
  fullyCombinedRecs$source <- "SURFACE WATER"
  fullyCombinedRecs$source[unlist(gwInd)] <- "GROUND WATER"

  ##reformat date colume, to be in same format as Idaho year/month/day
  fullyCombinedRecs$priorityDate <- format(as.POSIXct(fullyCombinedRecs$priorityDate, format="%m/%d/%Y"), format="%Y/%m/%d")

  ##remove columns no longer needed
  fullyCombinedRecs <- fullyCombinedRecs[,-c(which(colnames(fullyCombinedRecs)=="CFS_units"))]

  ##make sure the coordinates are numeric
  fullyCombinedRecs$latitude <- as.numeric(fullyCombinedRecs$latitude)
  fullyCombinedRecs$longitude <- as.numeric(fullyCombinedRecs$longitude)
  
  return(fullyCombinedRecs)
}
##########################################################################
##########################################################################
##a custom function to merge data frames
multiFullMerge <- function(x, y, by){
  outFrame <- merge(x,y,by="year",all=T)
  return(outFrame)
}
##########################################################################
##########################################################################
testDate <- function(inDate){
  #################
  #inDate <- jjj[1960]
  #################
  
  return(tryCatch(year(inDate), error=function(e) NULL))
}
##########################################################################
##########################################################################
# ##a function to extract data by sector for each 
# bySector <- function(sectorVars, wmaData){
#   #################
#   sectorVars <- sects[1]  ##the sector conversion table for a specific sector
#   wmaData <- wmaTab
#   #################
#   #print(sectorVars)
#   #wmaData <- as.data.frame(wmaData)
#   ##Subset industry and make it its own data frame with the priority date in the first column and 
#   ##the Overall Max CFS in the second
#   sectorRecs <- wmaData[grepl(sectorVars, as.character(wmaData$waterUse)),]
#   #sectorRecs <- wmaData[wmaData$waterUse==sectorVars,]
#   
#   ##If the sector is absent from a given WMA, this if statement will create an empty, similarly formatted, data frame 
#   ##with dates and a 0 cuml cfs amount.
#   if(nrow(sectorRecs)<1){
#     sectorRecs <- data.frame(priorityDate=seq(as.Date('1950/1/1'), as.Date('1999/1/1'), by="year"), CFS=0)
#   }
#   
#   ##Sort sector according to priority date.
#   sectorRecs <- sectorRecs[order(ymd(sectorRecs$priorityDate), decreasing=F),]
#   ##Save sector
#   #file.name <- paste(out.dir, "WMA", w, "/RIGHTS_SECTOR_", sec.categories[m], "_WMA_", w, "_", source.nm, ".csv", sep = "")
#   #write.table(sector, file.name, row.names = F, sep = ",")
#   #print(file.name)
#   
#   ##aggregate cfs by year
#   cfsYrTotal <- aggregate(data=sectorRecs, CFS~year(priorityDate), FUN=sum)
#   colnames(cfsYrTotal) <- c("year","cfs")   #unique(sectorVars$sector)
#   ##Save sec.by.year
#   #file.name <- paste(out.dir, "WMA", w, "/CFS_PER_YR_", sec.categories[m], "_WMA_", w, "_", source.nm, ".csv", sep = "")
#   #write.table(sec.by.year.csv, file.name, row.names = F, sep = ",")
#   #print(file.name)
#   
#   ##cumulative sum of water use...?
#   cfsCumuSum <- data.frame(year=cfsYrTotal$year, cumCFS=cumsum(cfsYrTotal$cfs))
#   colnames(cfsCumuSum)[2] <- sectorVars
#   #colnames(cfsCumuSum)[2] <- unique(sectorVars$sector)
#   ##Save sec.cuml
#   #file.name <- paste(out.dir, "WMA", w, "/CUMU_AMT_PER_YR_", sec.categories[m], "_WMA_", w, "_", source.nm, ".csv", sep = "")
#   #write.table(sec.cuml, file.name, row.names = F, sep = ",")
#   #print(file.name)
#   
#   ##return the relevent varialbes
#   return(cfsCumuSum)
# }
##########################################################################
##########################################################################
##This function subsets rights based on WMA, water source, sector, and saves 4 types of intermediate output. 1) Rights in each subset sector,
##ordered by priority date, will be saved under "RIGHTS_SECTOR_Sector_WMA_Id_Source.csv." 2) The cfs of each year will be saved under "CFS_PER_YEAR_Sector_WMA_Id_Source.csv."
##3) The sum of cfs over time will be saved under "CUMM_AMT_PER_YR_Sector_WMA_Id_Source.csv." 4) A data frame of all sector's cumulative cfs over time will
##be saved as "ALL_SEC_CUML_WMA_Id_Source.csv."
calcWMASectorVars <- function(rights, state, wmaID, startDate, endDate){
  #################
  #n <- 4
  #rights <- rightsByState_surface[[n]]  ##is the dataframe input.
  #state <- states[n]
  #wmaID <- wmaIDByState[[n]]  ##a vector of numeric administrative basin identifying numbers. E.g. WMA 11, WMA 37, WMA 98.
  #startDate <- allYrMin
  #endDate <- 2100
  #################

  ##Create a vector of unique sector variables
  sectCats <- c("irrigation", "domestic", "livestock", "fish", "industrial", "environmental", "other")
  
  ##calculate the cumulative total by wma, by sector, and by year, the beginning 3/4ths of sector.calc
  if(state=="arizona"){
    ##currently only sorts by surface basins, may need to recalculate base on ground water basins
    rightsByWMABySect <- pblapply(wmaID, function(x,tab,sects){wmaTab<-tab[(tab$surBasinNum==x & is.na(tab$surBasinNum)==F),];
                                                                ##create an output object data frame in which to organize the cumulative totals for water rights
                                                                rightsByWMA <- data.frame(Year=startDate:endDate, Irrigation=NA, Domestic=NA, Livestock=NA, 
                                                                                            Fish=NA, Industrial=NA, Environmental=NA, Other=NA)
                                                                
                                                                ##Fill out the the WMA data frame with the water rights from that WMA
                                                                for(sect in sectCats){
                                                                  ##Subset industry and make it its own data frame with the priority date in the first column and 
                                                                  ##the Overall Max CFS in the second
                                                                  sectorRecs <- wmaTab[grepl(sect, as.character(wmaTab$waterUse)),]
                                                                  if(nrow(sectorRecs)>0){
                                                                    ##Sort sector according to priority date.
                                                                    sectorRecs <- sectorRecs[order(ymd(sectorRecs$priorityDate), decreasing=F),]
                                                                    ##aggregate cfs by year
                                                                    cfsYrTotal <- aggregate(data=sectorRecs, CFS~year(priorityDate), FUN=sum)
                                                                    ##if any priority years are unrealistic, then set them to the binding years
                                                                    cfsYrTotal$`year(priorityDate)`[cfsYrTotal$`year(priorityDate)`>endDate]<-endDate
                                                                    ##preform the cumulation of each sector
                                                                    cfsCumuSum <- data.frame(year=cfsYrTotal$`year(priorityDate)`, cumuCFS=cumsum(cfsYrTotal$CFS))
                                                                    ##add the CFS aggregation to the output cumulation table
                                                                    rightsByWMA[which(rightsByWMA$Year %in% cfsCumuSum$year), which(colnames(rightsByWMA)==capitalizeStrings(sect))] <- cfsCumuSum$cumuCFS
                                                                  }
                                                                }
                                                                ##for each NA, replace with previous non-NA value
                                                                rightsByWMA[1,is.na(rightsByWMA[1,])]<-0;
                                                                fillTab<-na.locf(rightsByWMA);
                                                                return(fillTab)}, tab=rights, sects=sectCats)  #sects=sectClasses)
  }else{
    rightsByWMABySect <- pblapply(wmaID, function(x,tab,sects){wmaTab<-tab[(tab$basinNum==x & is.na(tab$basinNum)==F),];
                                                                ##create an output object data frame in which to organize the cumulative totals for water rights
                                                                rightsByWMA <- data.frame(Year=startDate:endDate, Irrigation=NA, Domestic=NA, Livestock=NA, 
                                                                                           Fish=NA, Industrial=NA, Environmental=NA, Other=NA)
                                                                
                                                                ##Fill out the the WMA data frame with the water rights from that WMA
                                                                for(sect in sectCats){
                                                                  ##Subset industry and make it its own data frame with the priority date in the first column and 
                                                                  ##the Overall Max CFS in the second
                                                                  sectorRecs <- wmaTab[grepl(sect, as.character(wmaTab$waterUse)),]
                                                                  if(nrow(sectorRecs)>0){
                                                                    ##Sort sector according to priority date.
                                                                    sectorRecs <- sectorRecs[order(ymd(sectorRecs$priorityDate), decreasing=F),]
                                                                    ##aggregate cfs by year
                                                                    cfsYrTotal <- aggregate(data=sectorRecs, CFS~year(priorityDate), FUN=sum)
                                                                    ##if any priority years are unrealistic, then set them to the binding years
                                                                    cfsYrTotal$`year(priorityDate)`[cfsYrTotal$`year(priorityDate)`>endDate]<-endDate
                                                                    ##preform the cumulation of each sector
                                                                    cfsCumuSum <- data.frame(year=cfsYrTotal$`year(priorityDate)`, cumuCFS=cumsum(cfsYrTotal$CFS))
                                                                    ##add the CFS aggregation to the output cumulation table
                                                                    rightsByWMA[which(rightsByWMA$Year %in% cfsCumuSum$year), which(colnames(rightsByWMA)==capitalizeStrings(sect))] <- cfsCumuSum$cumuCFS
                                                                  }
                                                                }
                                                                ##for each NA, replace with previous non-NA value
                                                                rightsByWMA[1,is.na(rightsByWMA[1,])]<-0;
                                                                fillTab<-na.locf(rightsByWMA);
                                                                return(fillTab)}, tab=rights, sects=sectCats)
  }
  #x<-1; x<-48; x<-8; x<-"001"; x<-"1_2; x<-"RG"; x<-"17"

  ##total cumulative water use by WMA, equivelent to the first section of vol.by.sector 
  wmaRightsTotal <- lapply(rightsByWMABySect, function(datTab){totTab<-data.frame(Year=datTab$Year,CUML=rowSums(datTab[,2:ncol(datTab)]),datTab[,2:ncol(datTab)]);
                                                        return(totTab)})
  #write.csv(all.cuml, file = paste(out.dir, "WMA", w, "/ALL_CUML_WMA_", w, "_", source.nm, ".csv", sep = ""))
  
  ##calculates percentage 
  wmaRightsPer <- lapply(wmaRightsTotal, function(datTab){perTab<-cbind.data.frame(datTab$Year,datTab$CUML,(datTab[,3:ncol(datTab)]/datTab$CUML)*100);
                                                      colnames(perTab)<-colnames(datTab);
                                                      ##convert the nans to 0s
                                                      perTab[is.na(perTab)]<-0;
                                                      return(perTab)})
  #write.csv(all.percent, file = paste(out.dir, "WMA", w, "/ALL_PERCENT_WMA_", w, "_", source.nm, ".csv", sep = ""), row.names = F)
  ##objects to be returned
  names(wmaRightsTotal) <- names(wmaRightsPer) <- wmaID
  return(list(wmaRightsTotal,wmaRightsPer))
}
##########################################################################
##########################################################################
##This function calculates the cumulative AFA sum of all rights with a volume amount appropriated.
calcWaterVol <- function(rightsTab, wmaID){
  #################
  #rightsTab <- fullRights
  #wmaID <- wmaIDByState[[1]]
  #################
  
  if("volume" %in% names(rightsTab)){
    ##extract date and volume data from the full data table
    volByWMA <- lapply(wmaID, function(x,tab,sects){wmaTab<-tab[tab$basinNum==x,];
                                                    volTab<-wmaTab[which(is.na(wmaTab$volume)==F),c("priorityDate", "volume")];
                                                    datOrd<-volTab[order(ymd(volTab[,"priorityDate"]),decreasing=F),];
                                                    datOrd[,"volume"] <- as.numeric(datOrd[,"volume"]);
                                                    return(datOrd)}, tab=rightsTab)
    #file.name <- paste(out.dir, "WMA", w, "/RIGHTS_VOLUME_WMA_", w, ".csv", sep = "")
    #write.table(volume, file.name, row.names = F, sep = ",")
    #print(file.name)
    
    ##aggregate cfs volume by year
    aggVolByYr <- lapply(volByWMA, function(xTab){aggTab<-aggregate(data=xTab, volume~year(ymd(priorityDate)),FUN=sum);
                                                  colnames(aggTab)<-c("Year","AFA");
                                                  return(aggTab)})
    #file.name <- paste(out.dir, "WMA", w, "/VOL_PER_YR_WMA_", w, ".csv", sep = "")
    #write.table(vol.by.year.csv, file.name, row.names = F, sep = ",")
    #print(file.name)
    
    ##cumulative sum over years
    afaCumuSums <- lapply(aggVolByYr, function(xTab){data.frame(Year=xTab$Year, AfaSumCumu=cumsum(xTab$AFA))})
    names(afaCumuSums) <- wmaID
    #file.name <- paste(out.dir, "WMA", w, "/CUMU_VOL_WMA_", w, ".csv", sep = "")
    #write.table(vol.cuml, file.name, row.names = F, sep = ",")
    #print(file.name)
    
    return(afaCumuSums)
  }else{
    return(NULL)
  }
}
##########################################################################
##########################################################################
##a function to plot the administrative WMAs for each state
plotAdminBounds <- function(adminBounds, plotLoc, state, boundLab, pixX, pixY){
  #################
  #adminBounds <- wmaBounds  ##the administrative boundaries to plot
  #boundLab <- wma
  #plotLoc <- plotsOut  ##the major location for which plots will be written out to
  #state <- st
  #pixX <- 900  ##the x of the output image, in pixels
  #pixY <- 1200  ##the y of the output image, in pixels
  #pixX <- 1200
  #pixY <- 900
  #################
  
  ##sets up output file structure
  stBndPltOut <- paste0(plotLoc,state,"/adminBounds/")
  if(dir.exists(stBndPltOut)==F){dir.create(stBndPltOut,recursive=T)}
  
  ##setting up the ggplot base - all state admin bounds plotted blank
  stateBase <- ggplot(adminBounds) + aes(long,lat,group=group) + geom_polygon(fill=NA,color="#969696") + theme_tufte()
  
  ##plots each individual boundary plot
  pblapply(adminBounds$basinNum, function(x){png(paste0(stBndPltOut,state,"_",boundLab,x,".png"), width=pixX, height=pixY);
                                              print(stateBase + geom_polygon(data=adminBounds[adminBounds$basinNum==x,],fill="blue",color="black"));
                                              dev.off()})                                    
}
##########################################################################
##########################################################################
##a function to map the point of use spatial data
cyclePlots <- function(admBound, waterUse, pntDiv, plotLocPoU, plotLocPoD, state, lab, passX, passY){
  #################
  #x<-30
  #x<-"7551-7557-7558"
  #admBound <- wmaBounds[wmaBounds$basinNum==x,]
  #lab <- wma
  #waterUse <- wrUse[wrUse$basinNum==x,]
  #pntDiv <- wrDiv[wrDiv$basinNum==x,]
  #plotLocPoU <- stPoUPltOuts
  #plotLocPoD <- stPoDPltOuts
  #state <- st
  #passX <- pixX
  #passY <- pixY
  #################
  
  polyBase <- ggplot(admBound) + aes(long,lat,group=group) + geom_polygon(fill=NA,color="#000000") + theme_tufte()
  ptBase <- ggplot(admBound) + aes(long,lat) + geom_polygon(fill=NA,color="#000000") + theme_tufte()
  adminID <- unique(admBound$basinNum)
  
  ###########################
  
  if(class(waterUse)=="numeric"){
    ##plot the points of use (all) for each wma
    png(paste0(plotLocPoU[1],state,"_",lab,adminID,"_PoUall.png"), width=passX, height=passY)
    print(polyBase + geom_polygon(data=waterUse,fill="#00ffff96",color="#969696"))
    dev.off()
    
    ##plot the points of use (ground water) for each wma
    png(paste0(plotLocPoU[2],state,"_",lab,adminID,"_PoUgrd.png"), width=passX, height=passY)
    if("GROUND WATER" %in% waterUse$source){
      print(polyBase + geom_polygon(data=waterUse[waterUse$source=="GROUND WATER",],fill="#65432196",color="#969696"))
    }else{
      print(polyBase)
    }
    dev.off()
    
    ##plots the points of use (surface water) for each wma
    png(paste0(plotLocPoU[3],state,"_",lab,adminID,"_PoUsur.png"), width=passX, height=passY)
    if("SURFACE WATER" %in% waterUse$source){
      print(polyBase + geom_polygon(data=waterUse[waterUse$source=="SURFACE WATER",],fill="#0000ff96",color="#969696"))
    }else{
      print(polyBase)
    }
    dev.off()
  }
  
  ###########################
  
  ##plot the points of diversion (all) for each wma
  png(paste0(plotLocPoD[1],state,"_",lab,adminID,"_PoDall.png"), width=passX, height=passY)
  print(autoplot(pntDiv,p=ptBase,colour="#00ffff96"))
  dev.off()
  
  ##plot the points of diversion (ground water) for each wma
  png(paste0(plotLocPoD[2],state,"_",lab,adminID,"_PoDgrd.png"), width=passX, height=passY)
  if("GROUND WATER" %in% pntDiv$source){
    print(autoplot(pntDiv[pntDiv$source=="GROUND WATER",],p=ptBase,colour="#65432196"))
  }else{
    print(polyBase)
  }
  dev.off()
  
  ##plots the points of diversion (surface water) for each wma
  png(paste0(plotLocPoD[3],state,"_",lab,adminID,"_PoDsur.png"), width=passX, height=passY)
  if("SURFACE WATER" %in% pntDiv$source){
    print(autoplot(pntDiv[pntDiv$source=="SURFACE WATER",],p=ptBase,colour="#0000ff96"))
  }else{
    print(polyBase)
  }
  dev.off()
}
##########################################################################
##########################################################################
##a function to map the point of use spatial data
plotPointOfUseandDiv <- function(adminBounds, waterUShp, waterDShp, plotLoc, state, boundLab, pixX, pixY){
  #################
  #adminBounds <- wmaBounds
  #boundLab <- wma
  #waterUShp <- wrUse
  #waterDShp <- wrDiv
  #plotLoc <- plotsOut
  #state <- st
  #pixX <- 1200  ##the x of the output image, in pixels
  #pixY <- 1200  ##the y of the output image, in pixels
  #################
  
  ##sets up output file structure
  sources <- c("allSources","ground", "surface")
  stPoUPltOuts <- paste0(plotLoc,state,"/",sources,"/pointOfUse/")
  sapply(stPoUPltOuts,function(x){if(dir.exists(x)==F){dir.create(x,recursive=T)}})
  stPoDPltOuts <- paste0(plotLoc,state,"/",sources,"/pointOfDiv/")
  sapply(stPoDPltOuts,function(x){if(dir.exists(x)==F){dir.create(x,recursive=T)}})
  
  ##hands data to plotting code wrapper
  if(class(waterUShp)=="numeric"){
    ##if water use is missing
    pblapply(adminBounds$basinNum, function(x){cyclePlots(adminBounds[adminBounds$basinNum==x,], NULL, 
                                                          waterDShp[waterDShp$basinNum==x,], stPoUPltOuts, stPoDPltOuts, state, boundLab, pixX, pixY)})
  }else{
    ##as expected
    pblapply(adminBounds$basinNum, function(x){cyclePlots(adminBounds[adminBounds$basinNum==x,], waterUShp[waterUShp$basinNum==x,], 
                                                          waterDShp[waterDShp$basinNum==x,], stPoUPltOuts, stPoDPltOuts, state, boundLab, pixX, pixY)})
  }
}
##########################################################################
##########################################################################
##a function to auto select the colors to create the correct color gradient for ggplot2
selectCols <- function(availDat){
  #################
  #availDat <- colnames(tab)[-c(1,ncol(tab))]
  #################
  colorGrad <- vector()
  
  if("Irrigation" %in% availDat){
    colorGrad <- c(colorGrad, "#bebebe")
  }
  if("Domestic" %in% availDat){
    colorGrad <- c(colorGrad, "#0000ff")
  }
  if("Livestock" %in% availDat){
    colorGrad <- c(colorGrad, "#ff0000")
  }
  if("Fish" %in% availDat){
    colorGrad <- c(colorGrad, "#006400")
  }
  if("Industrial" %in% availDat){
    colorGrad <- c(colorGrad, "#551a8b")
  }
  if("Environmental" %in% availDat){
    colorGrad <- c(colorGrad, "#00ff00")
  }
  if("Other" %in% availDat){
    colorGrad <- c(colorGrad, "#ffa500")
  }
  
  if(length(colorGrad)==1){
    colorGrad <- c(colorGrad, colorGrad, colorGrad)
  }
  
  return(colorGrad)
}
##########################################################################
##########################################################################
##plot the cumulative amount for different sectors, over time, in a given adminstrative basin.
##as well as the total CFS
plotCumuLines <- function(wmaCumuDat, plotLoc, state, boundLab, source, pixX, pixY){
  #################
  #wmaCumuDat <- wrtg
  #plotLoc <- plotsOut
  #state <- st
  #boundLab <- wma
  #source <- "ground"
  #pixX <- 900  ##the x of the output image, in pixels
  #pixY <- 800  ##the y of the output image, in pixels
  #################
  
  ##sets up output file structure
  cumuSectRightsPltOut <- paste0(plotLoc,state,"/",source,"/cumuRightsBySector/")
  if(dir.exists(cumuSectRightsPltOut)==F){dir.create(cumuSectRightsPltOut,recursive=T)}
  
  ##cfs by sector line plots
  lapply(1:length(wmaCumuDat),function(x){chkCols<-colSums(wmaCumuDat[[x]]);
                                  if(length(which(chkCols==0))>0){
                                    subTab<-wmaCumuDat[[x]][,-c(which(chkCols==0))];  ##only keep columns with data, defined as having a sum greater than 0
                                  }else{subTab<-wmaCumuDat[[x]]}
                                  if(class(subTab)=="data.frame"){
                                    plotCols<-selectCols(colnames(subTab)[-c(1:2)]);
                                    chzTab<-melt(subTab[,-c(which(colnames(subTab)=="CUML"))],id.vars="Year");
                                    png(paste0(cumuSectRightsPltOut,state,"_",boundLab,names(wmaCumuDat)[x],"_cumuRightsBySector.png"), width=pixX, height=pixY)
                                    print(ggplot(chzTab, aes(Year, value, group=variable)) + geom_line(aes(color=variable),size=2) + theme_tufte() +
                                          scale_color_manual(values=plotCols, name="Sector") + labs(x="Year", y="Cumulative CFS") +
                                            theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text.x=element_text(size=16),
                                                  axis.text.y=element_text(size=16), legend.title=element_text(size=20,hjust=0.5), legend.text=element_text(size=20)))
                                    dev.off()}})
  
  ##total cfs plot
  lapply(1:length(wmaCumuDat),function(x){totCols<-c(which(colnames(wmaCumuDat[[x]])%in%c("Year","CUML")));  ##when all tables are formatted the same, as they should be
                                    subTab<-wmaCumuDat[[x]][,totCols];
                                    png(paste0(cumuSectRightsPltOut,state,"_",boundLab,names(wmaCumuDat)[x],"_cumuRightsTotalCFS.png"), width=pixX, height=pixY)
                                    print(ggplot(subTab, aes(Year, CUML)) + geom_line(size=2) + theme_tufte() + labs(x="Year", y="Cumulative CFS") +
                                            theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text.x=element_text(size=16),
                                                  axis.text.y=element_text(size=16), legend.title=element_text(size=20), legend.text=element_text(size=20))) 
                                    dev.off()})
}
##########################################################################
##########################################################################
##a function to plot the percent volume used for each sector, by wma
plotCumuStacked <- function(wmaCumuDat, plotLoc, state, boundLab, source, pixX, pixY){
  #################
  #wmaCumuDat <- wrpg
  #plotLoc <- plotsOut
  #state <- st
  #source <- "ground"
  #pixX <- 800  ##the x of the output image, in pixels
  #pixY <- 900  ##the y of the output image, in pixels
  #################
  
  ##sets up output file structure
  cumuSectPerPltOut <- paste0(plotLoc,state,"/",source,"/cumuPercentBySector/")
  if(dir.exists(cumuSectPerPltOut)==F){dir.create(cumuSectPerPltOut,recursive=T)}
  
  lapply(1:length(wmaCumuDat),function(x){chkCols<-colSums(wmaCumuDat[[x]]);
                                  if(length(which(chkCols==0))>0){
                                    subTab<-wmaCumuDat[[x]][,-c(which(chkCols==0))];  ##only keep columns with data, defined as having a sum greater than 0
                                  }else{subTab<-wmaCumuDat[[x]]}
                                  if(class(subTab)=="data.frame"){
                                    plotCols<-selectCols(colnames(subTab)[-c(1:2)]);
                                    chzTab<-melt(subTab[,-c(which(colnames(subTab)=="CUML"))],id.vars="Year");
                                    #chzTab$variable<-factor(chzTab$variable,levels=rev(levels(chzTab$variable)));
                                    #chzTab$variable<-factor(capitalizeStrings(as.character(chzTab$variable)),levels=rev(capitalizeStrings(as.character(levels(chzTab$variable)))));
                                    png(paste0(cumuSectPerPltOut,state,"_",boundLab,names(wmaCumuDat)[x],"_cumuRightsStacked.png"), width=pixX, height=pixY)
                                    print(ggplot(chzTab, aes(Year, value, group=variable)) + geom_area(aes(fill=variable)) + theme_tufte() +
                                          scale_fill_manual(values=plotCols, name="Sector") + labs(x="Year", y="% Total Flow Allocation") +
                                          theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text.x=element_text(size=16),
                                                axis.text.y=element_text(size=16), legend.title=element_text(size=20,hjust=0.5), legend.text=element_text(size=20)))
                                    dev.off()}})
}
##########################################################################
##########################################################################
##a function to plot cumulative AFA volume by wma
plotCumuAFA <- function(wmaCumuDat, plotLoc, state, boundLab, pixX, pixY){
  #################
  #wmaCumuDat <- idahoWMAVolSums
  #plotLoc <- plotsOut
  #state <- "idaho"
  #pixX <- 900  ##the x of the output image, in pixels
  #pixY <- 800  ##the y of the output image, in pixels
  #################
  
  ##sets up output file structure
  afaVolPltOut <- paste0(plotLoc,state,"/cumuAFAVolume/")
  if(dir.exists(afaVolPltOut)==F){dir.create(afaVolPltOut,recursive=T)}

  lapply(1:length(wmaCumuDat),function(x){png(paste0(afaVolPltOut,state,"_",boundLab,names(wmaCumuDat)[x],"_cumuRightsTotalAFA.png"), width=pixX, height=pixY)
                                          print(ggplot(wmaCumuDat[[x]], aes(Year, AfaSumCumu)) + geom_line(size=2) + theme_tufte() + labs(x="Year", y="Cumulative AFA", fill="Sector") +
                                                  theme(axis.title.x=element_text(size=20), axis.title.y=element_text(size=20), axis.text.x=element_text(size=16),
                                                        axis.text.y=element_text(size=16), legend.title=element_text(size=20), legend.text=element_text(size=20)))
                                          dev.off()})
}
##########################################################################
##########################################################################
# ##a function to assign huc 8 watersheds to a single US county, based on area
# assignHuc2County <- function(hucRecs, hucGeo, usCounts){
#   #################
#   #hucRecs <- counts2Hucs[[645]]
#   #hucGeo <- westHucs[645,]
#   #usCounts <- projWCs[hucRecs,]
#   #hucRecs <- counts2Hucs[[605]]
#   #hucGeo <- westHucs[605,]
#   #usCounts <- projWCs[hucRecs,]
#   #################
#   
#   hucID <- hucGeo$HUC8
#   
#   if(length(hucRecs)>1){
#     
#     ##as long as all counties are within research boundaries
#     ##crop counties by watershed
#     cropCounts <- crop(usCounts, hucGeo)
#     ##determine which county contains the majority of the huc
#     cropArea <- gArea(cropCounts, byid=T)
#     whichCounty <- which.max(cropArea)
#     #assignedState <- cropCounts$STATEFP[whichCounty]
#     #assignedCounty <- cropCounts$COUNTYFP[whichCounty]
#     assignedFIPS <- cropCounts$fullFIPS[whichCounty]
#     
#   }else if(length(hucRecs)==1){
#     
#     #assignedState <- usCounts$STATEFP
#     #assignedCounty <- usCounts$COUNTYFP
#     assignedFIPS <- usCounts$fullFIPS
#     
#   }else{
#     break("issues occured")
#   }
#   
#   return(c(hucID, assignedFIPS))
# }
##########################################################################
##########################################################################
customOverlay <- function(isoPoly, rastPts){
  #################
  #isoPoly<-projWCs[x,]
  #isoPoly<-projWMAs[x,]
  #rastPts<-projMapMIrADRast
  #################
  
  expPolyBnd<-extent(isoPoly)+c(-50,50,-50,50);
  subsetCells<-rastPts[which(rastPts$x>=expPolyBnd@xmin & rastPts$x<=expPolyBnd@xmax & rastPts$y>=expPolyBnd@ymin & rastPts$y<=expPolyBnd@ymax),];
  if(nrow(subsetCells)>0){
    coCellInter<-subsetCells[isoPoly,]
    ##id cell number, then find cell number index in full cell dataframe
    fullIndex<-which(rastPts$cells %in% coCellInter$cells);
  }else{
    fullIndex<-NULL
  }
  return(fullIndex)
}
##########################################################################
##########################################################################
galDay2ft3Sec <- function(val){
  #################
  #val <- 1
  #################
  ft3Sec <- val * 0.00000154723
  return(ft3Sec)
}
##########################################################################
##########################################################################
acreFtYr2ft3Sec <- function(val){
  #################
  #val <- 1
  #################
  ft3Sec <- val * 0.00138036
  return(ft3Sec)
}
##########################################################################
##########################################################################
##extract the water use and CSF from each record
exAZSurData <- function(useRec, useCats, useUnits){
  ###############
  #useRec <- azSurFillings$USES[2]
  #useRec <- azSurFillings$USES[4]
  #useRec <- azSurFillings$USES[17]
  #useRec <- azSurFillings$USES[851]
  #useRec <- azSurFillings$USES[1451]
  #useRec <- azSurFillings$USES[2800]
  #tempUses <- azSurFillings$USES
  #tempUses <- tempUses[-grep("STOCK", tempUses)]
  #tempUses <- tempUses[-grep("IRRIGATION", tempUses)]
  #tempUses <- tempUses[-grep("WILDLIFE", tempUses)]
  #tempUses <- tempUses[-grep("DOMESTIC", tempUses)]
  #tempUses <- tempUses[-grep("MINING", tempUses)]
  #tempUses <- tempUses[-grep("MUNICIPAL", tempUses)]
  #tempUses <- tempUses[-grep("POWER", tempUses)]
  #tempUses <- tempUses[-grep("RECREATION", tempUses)]
  #tempUses <- tempUses[-grep("INDUSTRIAL", tempUses)]
  #tempUses <- tempUses[-grep("OTHER", tempUses)]
  #tempUses <- tempUses[-grep("COMMERCIAL", tempUses)]
  #useRec <- tempUses[77]
  #useRec <- tempUses[37]
  #useRec <- tempUses[3]
  #useCats <- azSurUseCats
  #useUnits <- azSurUnits
  ###############
  ##if COWS / HORSES is available, convert to STOCK
  if(grepl("COWS / HORSES", useRec)){
    useRec <- gsub("COWS / HORSES", "COWS", useRec)
    useCats <- c(useCats, "COWS")
  }
  
  ##determine which water uses are available
  splitRec <- strsplit(useRec, " ")[[1]]
  #catsInRec <- useCats[which(useCats %in% splitRec)]
  catsInRec <- names(sort(unlist(sapply(useCats, grep, x=splitRec))))
  
  ##get the unit used in the record
  unitPos <- which(splitRec %in% useUnits)
  if(length(unitPos)==0){
    ##if no units, no water use
    setOutput <- data.frame(origWaterUse=NA, CFS=NA)
    return(setOutput)
  }
  
  h2oUse <- splitRec[unitPos-1]
  if(is.null(catsInRec)==T & length(h2oUse)>=1){
    catsInRec <- "OTHER"
  }else if(grepl("###############", useRec)){
    ##remove hashtag amounts
    setOutput <- data.frame(origWaterUse=NA, CFS=NA)
    return(setOutput)
  }
  numH2OUse <- as.numeric(gsub(",", "", h2oUse))
  
  if(length(numH2OUse)>length(catsInRec)){
    numH2OUse <- numH2OUse[1:length(catsInRec)]
    unitPos <- unitPos[1:length(catsInRec)]
  }
  
  ##convert water use to CFS
  unitText <- splitRec[unitPos]
  outCFS <- sapply(1:length(unitText), function(n){utxt<-unitText[n];
                                                    if(utxt=="AFA"){
                                                      return(acreFtYr2ft3Sec(numH2OUse[n]))
                                                    }else if(utxt=="AFT"){  ##Assumes that AFT is the same as AFA
                                                      return(acreFtYr2ft3Sec(numH2OUse[n]))
                                                    }else if(utxt=="AF"){  ##Assumes that AF is the same as AFA
                                                      return(acreFtYr2ft3Sec(numH2OUse[n]))
                                                    }else if(utxt=="GPA"){
                                                      return(conv_unit(numH2OUse[n], "us_gal", "ft3") / 365.25)
                                                    }else if(utxt=="GAL"){  ##Assumes that GAL is the same as GPA
                                                      return(conv_unit(numH2OUse[n], "us_gal", "ft3") / 365.25)  
                                                    }else if(utxt=="CFS"){
                                                      return(numH2OUse[n])
                                                    }else if(utxt=="CFT"){  ##Cubic foot total for annual use
                                                      return(numH2OUse[n] / 31556952)
                                                    }else if(utxt=="MIT"){  ##Assumes that MIT is the same as AMI per year
                                                      return(numH2OUse[n] * 0.025)
                                                    }else if(utxt=="MIA"){  ##Assumes that MIA is the same as AMI per year
                                                      return(numH2OUse[n] * 0.025)
                                                    }else if(utxt=="MNT"){  ##Unknown unit, however all values are 0 
                                                      return(0)  
                                                    }})
  ##if needed, transform cows into stock
  if("COWS" %in% catsInRec){
    if("STOCK" %in% catsInRec ){
      whichCOWS <- grep("COWS",catsInRec)
      if(length(catsInRec)==length(outCFS)){
        outCFS[grep("STOCK",catsInRec)] <- sum(c(outCFS[grep("STOCK",catsInRec)], outCFS[whichCOWS]))
        ##remove cows, as no longer needed
        catsInRec <- catsInRec[-whichCOWS]
        outCFS <- outCFS[-whichCOWS]
      }else{
        ##remove cows, as no longer needed
        catsInRec <- catsInRec[-whichCOWS]
      }
    }else{
      ##convert cows to stock
      catsInRec[grep("COWS",catsInRec)] <- "STOCK"
    }
  }
  
  ##set up the output based on the number of input CFS values
  ##Assigns all water use to the use category with the most use. If its not broken down
  ##to different water amounts, assumes that the first category
  setOutput <- data.frame(origWaterUse=catsInRec[which.max(outCFS)], CFS=sum(outCFS))
  return(setOutput)
}
##########################################################################
##########################################################################
# calculateCounty2HUC <- function(county, hucData, waterUse){
#   #################
#   #county <- projWCs$fullFIPS[6]
#   #hucData <- as.data.frame(westHucs[westHucs$fullFIPS==county,])
#   #waterUse <- usgsWaterUse[usgsWaterUse$FIPS==county,]
#   #################
#   
#   if(nrow(hucData)>0){
#     retOb <- data.frame(HUC8=hucData$HUC8)
#     
#     if("tot_irri" %in% colnames(hucData)){
#       ##total the irrigation area of the hucs
#       hucIrrAreaTot <- sum(hucData$tot_irri)
#       
#       if(hucIrrAreaTot>0){
#         hucPercUse <- milGalDay2ft3Sec(waterUse$IR.WFrTo) * (hucData$tot_irri / hucIrrAreaTot)
#       }else{
#         hucPercUse <- 0
#       }
#       retOb <- cbind.data.frame(retOb, hucPercUse)
#     }
#     
#     return(retOb)
#   }else{
#     return(NULL)
#   }
# }
##########################################################################
##########################################################################
calculateUSGSVars <- function(cellTab, usgsCoVals){
  #################
  #coInd<-5
  #cellTab <- as.data.frame(hydeDataCells[cellsInCounts[[coInd]],])  ##table of the cells associated with county id
  #cellTab <- as.data.frame(irrCells250[cellsInCounts250[[coInd]],])
  #usgsCoVals <- as.data.frame(rastProjWCs[coInd,])  ##the usgs county data, singular county
  #################
  if(nrow(cellTab)>0){
    ##set up output object
    outFrame <- data.frame(cell=cellTab$cells)
    
    ##HYDE irrigation
    if("tot_irri" %in% colnames(cellTab)){
      ##get total of the HYDE cells within the county
      hydeIrrTot <- sum(cellTab$tot_irri)
      
      # if(usgsIrrUse>0 & hydeIrrTot==0){
      #   ##when the county as water, but the HYDE cells total to zero, division by zero everything to NA
      #   ##as alternative, divide county water value into the count of cells
      #   outFrame$usgsModIrr <- usgsIrrUse / nrow(cellTab)
      # }else if(usgsIrrUse==0 & hydeIrrTot==0){
      #   outFrame$usgsModIrr <- 0
      # }else{
      #   ##as expected
      #   outFrame$usgsModIrr <- usgsIrrUse * (cellTab$tot_irri / hydeIrrTot)
      # }
      if(hydeIrrTot==0 ){
        outFrame$hyde_IR_WSWFr <- 0
        outFrame$hyde_IR_WGWFr <- 0
      }else{
        outFrame$hyde_IR_WSWFr <- galDay2ft3Sec(usgsCoVals$IR.WSWFr*1000000) * (cellTab$tot_irri / hydeIrrTot)
        outFrame$hyde_IR_WGWFr <- galDay2ft3Sec(usgsCoVals$IR.WGWFr*1000000) * (cellTab$tot_irri / hydeIrrTot)
      }
    }
    
    ##MIrAD irrigation
    if("miradIrr" %in% colnames(cellTab)){
      ##get total of the HYDE cells within the county
      miradIrrTot <- sum(cellTab$miradIrr)
      
      # if(usgsIrrUse>0 & hydeIrrTot==0){
      #   ##when the county as water, but the HYDE cells total to zero, division by zero everything to NA
      #   ##as alternative, divide county water value into the count of cells
      #   outFrame$usgsModIrr <- usgsIrrUse / nrow(cellTab)
      # }else if(usgsIrrUse==0 & hydeIrrTot==0){
      #   outFrame$usgsModIrr <- 0
      # }else{
      #   ##as expected
      #   outFrame$usgsModIrr <- usgsIrrUse * (cellTab$cropland / hydeIrrTot)
      # }
      if(miradIrrTot==0){
        outFrame$mirad_IR_WSWFr <- 0
        outFrame$mirad_IR_WGWFr <- 0
      }else{
        outFrame$mirad_IR_WSWFr <- galDay2ft3Sec(usgsCoVals$IR.WSWFr*1000000) / nrow(cellTab)
        outFrame$mirad_IR_WGWFr <- galDay2ft3Sec(usgsCoVals$IR.WGWFr*1000000) / nrow(cellTab)
      }  
    }
    
    ##HYDE domestic and industrial
    if("popc" %in% colnames(cellTab)){
      ##get population total of the HYDe cells without county
      hydePopTot <- sum(cellTab$popc)
      
      # if(usgsPopUse>0 & hydePopTot==0){
      #   print("have cases of exception 1")
      # }else if(usgsPopUse==0 & hydePopTot==0){
      #   print("have cases of exception 2")
      # }else{
      #   ##as expected
      #   outFrame$usgsModPop <- usgsPopUse * (cellTab$popc / hydePopTot)
      # }
      if(hydePopTot==0){
        outFrame$usgsModSWDom <- 0
        outFrame$usgsModGWDom <- 0
        outFrame$usgsModSWInd <- 0
        outFrame$usgsModGWInd <- 0
      }else{
        usgsPopSWUse <- galDay2ft3Sec(usgsCoVals$PS.WSWFr*1000000) + galDay2ft3Sec(usgsCoVals$DO.WSWFr*1000000)
        usgsPopGWUse <- galDay2ft3Sec(usgsCoVals$PS.WGWFr*1000000) + galDay2ft3Sec(usgsCoVals$DO.WGWFr*1000000)
        usgsIndSWUse <- galDay2ft3Sec(usgsCoVals$IN.WSWFr*1000000) + galDay2ft3Sec(usgsCoVals$MI.WSWFr*1000000) + galDay2ft3Sec(usgsCoVals$PT.WSWFr*1000000)
        usgsIndGWUse <- galDay2ft3Sec(usgsCoVals$IN.WGWFr*1000000) + galDay2ft3Sec(usgsCoVals$MI.WGWFr*1000000) + galDay2ft3Sec(usgsCoVals$PT.WGWFr*1000000)
        
        outFrame$hyde_DOPS_WSWFr <- usgsPopSWUse * (cellTab$popc / hydePopTot)
        outFrame$hyde_DOPS_WGWFr <- usgsPopGWUse * (cellTab$popc / hydePopTot)
        outFrame$hyde_INMIPT_WSWFr <- usgsPopSWUse * (cellTab$popc / hydePopTot)
        outFrame$hyde_INMIPT_WGWFr <- usgsPopGWUse * (cellTab$popc / hydePopTot)
      }
    }
  }else{
    outFrame<- NULL
  }
  
  return(outFrame)
}
##########################################################################
##########################################################################
#convertRasterToPts <- function(inRast, inPoly){
#  #################
#  #inRast <- nlcd2016
#  #inPoly <- caliWMAs[1,]
#  #################
#  
#  cropRast <- mask(crop(inRast, inPoly), inPoly)
#  rastPts <- rasterToPoints(cropRast, spatial=T)
#  exRastPts <- raster::extract(cropRast, rastPts, cellnumbers=T, sp=T)
#  return(exRastPts)
#}
##########################################################################
##########################################################################
evalFunction <- function(Q, agrQnt, agrK, agrE, urbQnt, urbK, urbE){
  #################
  #agrQnt <- agrQuant
  #agrK <- agrK
  #agrE <- agrElast
  #urbQnt <- urbQuant
  #urbK <- urbK
  #urbE <- urbElast
  #################
  print(Q) 
  #print(agrQnt) 
  #print(agrK)
  #print(agrE)
  #print(urbQnt)
  #print(urbK)
  #print(urbE)
  findQ <- ((1/urbK) * Q)^(1/urbE) - ((1/agrK) * (agrQnt + urbQnt - Q))^(1/agrE)
  return(findQ)
}
##########################################################################
##########################################################################
fitEconCurves <- function(agrQuant, agrElast, agrPrice, urbQuant, urbElast, urbPrice, curve="li"){
  #################
  #agrQuant <- 518868.426332586  ##agriculture quantity
  #agrQuant <- waterRightRecsIndust$SWAgrBefore#[6]
  #agrQuant <- waterBeforeByReg$SWAgrBefore[10]
  #agrElast <- -0.432 ##agriculture elasticity
  #agrPrice <- waterRightRecsIndust$sw_agrLeasePrice#[6]  ##agriculture price, seller
  #agrPrice <- waterBeforeByReg$swAvePrAgrPay[10]
  #agrPrice <- 391.70
  #urbQuant <- 182480.620170195  ##urban quantity
  #urbQuant <- waterRightRecsIndust$SWUrbBefore#[6]
  #urbQuant <- waterBeforeByReg$SWUrbBefore[5]
  #urbElast <- -0.946  ##urban elasticity
  #urbPrice <- waterRightRecsIndust$sw_urbLeasePrice#[6]  ##urban price, buyer
  #urbPrice <- waterBeforeByReg$swAvePrUrbPay[5]
  #urbPrice <- 576.74
  #curve <- "nl"  ##li for linear, nl for non-linear
  #################
  if(curve=="li"){
    ##B column
    b4 <- (agrElast * agrQuant) / agrPrice
    b5 <- agrQuant - b4 * agrPrice
    b6 <- 1 / b4
    b7 <- (b5 / b4) * -1
    
    ##C column
    c4 <- (urbElast * urbQuant) / urbPrice
    c5 <- urbQuant - c4 * urbPrice
    c6 <- 1 / c4
    c7 <- (c5 / c4) * -1
    
    ##E column
    e9 <- (c6*-1) + b6
    e10 <- b6 * (agrQuant + urbQuant)
    e11 <- c7 - b7 - e10
    
    urbMarketVal <- e11 / e9
    agrMarketVal <- agrQuant + urbQuant - urbMarketVal
    negAgr <- which(agrMarketVal<0)
    agrMarketVal[negAgr] <- 0
    urbMarketVal[negAgr] <- agrQuant[negAgr] + urbQuant[negAgr]
    negUrb <- which(urbMarketVal<0)
    urbMarketVal[negUrb] <- 0
    agrMarketVal[negUrb] <- agrQuant[negUrb] + urbQuant[negUrb]
    
    perDiffAgr <- (agrMarketVal / agrQuant) - 1
    perDiffAgr[which(perDiffAgr>0)] <- 0
    perDiffUrb <- (urbMarketVal / urbQuant) - 1
    perDiffUrb[which(perDiffUrb<0)] <- 0
    
    totTradeVol <- urbMarketVal-urbQuant
    pctTradeVol <- 100 * totTradeVol / (agrQuant+urbQuant)
 
    urbMarketP <- urbMarketVal
    agrMarketP <- agrMarketVal/agrK
    
    welUrb <- integrate(function(i) (i/3)^(1/2), urbQuant, urbMarketVal)$value
    welAgr <- integrate(function(i) (i/3)^(1/2), agrQuant, agrMarketVal)$value
    
    squareUrb <- urbMarketP
    agrK <- agrQuant
    urbK <- urbQuant
    
    
     }else if(curve=="nl"){
    agrK <- agrQuant / (agrPrice^agrElast)
    urbK <- urbQuant / (urbPrice^urbElast)
    x <- 1
    urbMarketVal <- sapply(1:length(urbQuant), function(x){if( agrQuant[x]==0 | is.na(agrQuant[x])==T | is.na(urbQuant[x])==T | is.na(agrK[x])==T | is.na(urbK[x])==T){
                                                            retVal<-NA
                                                          }else{
                                                            if(urbQuant[x]==0){
                                                              urbQuant[x] <- 0.00001
                                                              urbK[x] <- urbQuant[x] / (urbPrice[x]^urbElast)
                                                            }
                                                            retVal<-tryCatch(uniroot(evalFunction, interval=c(urbQuant[x],urbQuant[x]+agrQuant[x]), agrQnt=agrQuant[x], agrK=agrK[x], agrE=agrElast, urbQnt=urbQuant[x], urbK=urbK[x], urbE=urbElast, extendInt="yes")$root, error=function(e){warning(conditionMessage(e)); NA})
                                                          }
                                                          return(retVal)})
    agrMarketVal <- agrQuant + urbQuant - urbMarketVal
    
    urbMarketP <- (urbMarketVal/urbK)^(1/urbElast)
    agrMarketP <- (agrMarketVal/agrK)^(1/agrElast)
    
    perDiffAgr <- (agrMarketVal / agrQuant) - 1
    perDiffUrb <- (urbMarketVal / urbQuant) - 1
    
    totTradeVol <- urbMarketVal-urbQuant
    pctTradeVol <- 100 * (totTradeVol / (agrQuant+urbQuant))
  
    
    #xx <- seq(urbQuant, urbMarketVal, by=1.0)
    #yy <- sapply(xx, function(i) (i/urbK)^(1/urbElast))
    
    #AUC(xx, yy)
    my_func1 <- function(i, urbK, urbElast) (i/urbK)^(1/urbElast)
    integrate_func1 <- function(urbK, urbElast, urbQuant, urbMarketVal) {
      if (is.na(urbK) || is.na(urbElast) || is.na(urbQuant) || is.na(urbMarketVal)){
        return(NA)
      } else {
        integrate(my_func1, lower=urbQuant, upper=urbMarketVal, urbK=urbK, urbElast=urbElast)$value
      }
      }
    vintegrate_func1 <- Vectorize(integrate_func1)
    welUrb <- vintegrate_func1(urbK, urbElast, urbQuant, urbMarketVal)
    
    my_func2 <- function(i, agrK, agrElast) (i/agrK)^(1/agrElast)
    integrate_func2 <- function(agrK, agrElast, agrQuant, agrMarketVal) {
      if (is.na(agrK) || is.na(agrElast) || is.na(agrQuant) || is.na(agrMarketVal)){
        return(NA)
      } else {
        integrate(my_func2, lower=agrQuant, upper=agrMarketVal, agrK=agrK, agrElast=agrElast)$value
      }
    }
    vintegrate_func2 <- Vectorize(integrate_func2)
    welAgr <- vintegrate_func2(agrK, agrElast, agrQuant, agrMarketVal)
    
    squareUrb <- (urbMarketVal - urbQuant)*urbMarketP

  
    
    #welUrb <- integrate(function(i) (i/urbK)^(1/urbElast), urbQuant, urbMarketVal)$value
    #welAgr <- integrate(function(i) (i/agrK)^(1/agrElast), agrQuant, agrMarketVal)$value

  }
  
  #curveFitted <- data.frame(AgrAfter=agrMarketVal, UrbAfter=urbMarketVal, perDiffAgr=perDiffAgr, perDiffUrb=perDiffUrb, totTradeVol=abs(totTradeVol), pctTradeVol=abs(pctTradeVol))
  curveFitted <- data.frame( welAgr = welAgr, welUrb = welUrb, agrMarketVal=agrMarketVal, urbMarketVal=urbMarketVal, perDiffAgr=perDiffAgr, perDiffUrb=perDiffUrb, totTradeVol=totTradeVol, pctTradeVol=pctTradeVol, agrMarketP =agrMarketP, urbMarketP = urbMarketP, squareUrb = squareUrb, agrK = agrK, urbK = urbK )
  return(curveFitted)
}
##########################################################################
##########################################################################
convertWMAVolToTrdRegions <- function(st, stPrices, stateHydroReg, surRights, grdRights, wmaLayer, areaProj, whichYear="last", cutOffYrs=NULL){
  #################
  #n <- 3
  #st <- stateNames[n]
  #stPrices <- statePricesByReg[[n]]
  #stateHydroReg <- stateRegSpData[[n]]
  #surRights <- wmasRightsTotsSur
  #grdRights <- wmasRightsTotsGrd
  #whichYear <- "last"  ##options: last, cutoff
  #wmaLayer <- wmasData
  #areaProj <- projForAreaCalc
  #cutOffYrs <- NULL
  #cutOffYrs <- datesWithIDs
  ################
  print(st)
  ##convert the prices in AFY to CFS
  stPrices$swAvePrAgrPay <- acreFtYr2ft3Sec(stPrices$swAvePrAgrPay)
  stPrices$gwAvePrAgrPay <- acreFtYr2ft3Sec(stPrices$gwAvePrAgrPay)
  stPrices$swAvePrUrbPay <- acreFtYr2ft3Sec(stPrices$swAvePrUrbPay)
  stPrices$gwAvePrUrbPay <- acreFtYr2ft3Sec(stPrices$gwAvePrUrbPay)
  
  stInd <- which(states==st)
  stSur <- surRights[[stInd]]
  stGrd <- grdRights[[stInd]]
  
  ##remove WMAs which are not to be included in the analysis
  ##these are "WMAs" which do not have geographies, or which fall within geographies of other larger WMAs
  badInd <- NA
  if(st=="idaho"){
    badInd <- which(names(stSur)%in%c("1","2","3"))  ##removes 1, 2, and 3
  }else if(st=="newMexico"){
    badInd <- which(names(stSur)%in%c("LWD","SD","SP"))
  }else if(st=="california"){
    badInd <- which(names(stSur)=="4406")
  }else if(st=="utah"){
    badInd <- which(names(stSur)=="00")
  }
  stSur[c(badInd)] <- NULL
  stGrd[c(badInd)] <- NULL
  #names(modWRs) <- stWMAS$basinNum
  stSur[sapply(stSur, is.null)] <- NULL
  stGrd[sapply(stGrd, is.null)] <- NULL
  
  # if(st=="arizona"){
  #   ##set up the water rights data, to get the correct volume of water available to all parties
  #   ##urban is a combination of domestic and industrial
  #   exSurLastYrs <- lapply(1:length(stSur),function(wmaInd){lastSurRec<-stSur[[wmaInd]][nrow(stSur[[wmaInd]]),c("Year","Irrigation","Domestic","Industrial")];
  #                                                       #lastGrdRec<-stGrd[[wmaInd]][nrow(stGrd[[wmaInd]]),c("Year","Irrigation","Domestic","Industrial")];
  #                                                       #combineWater<-data.frame(surfaceYr=lastSurRec$Year, groundYr=lastGrdRec$Year, SWAgrBefore=lastSurRec$Irrigation, GWAgrBefore=lastGrdRec$Irrigation, SWUrbBefore=sum(c(lastSurRec$Domestic,lastSurRec$Industrial),na.rm=T), GWUrbBefore=sum(c(lastGrdRec$Domestic,lastGrdRec$Industrial),na.rm=T));
  #                                                       combineWater<-data.frame(surfaceYr=lastSurRec$Year, SWAgrBefore=lastSurRec$Irrigation, SWUrbBefore=sum(c(lastSurRec$Domestic,lastSurRec$Industrial),na.rm=T))
  #                                                       return(combineWater)})
  #   surLastCombineYrs <- do.call(rbind.data.frame, exSurLastYrs)
  #   surWaterRightRecsIndust <- data.frame(state=capitalizeStrings(states[stInd]), basinNum=names(stSur), uniID=paste0(capitalizeStrings(states[stInd]),"_",names(stSur)), lastSurfaceYr=surLastCombineYrs$surfaceYr, SWAgrBefore=surLastCombineYrs$SWAgrBefore, SWUrbBefore=surLastCombineYrs$SWUrbBefore)
  #   exGrdLastYrs <- lapply(1:length(stGrd),function(wmaInd){lastGrdRec<-stGrd[[wmaInd]][nrow(stGrd[[wmaInd]]),c("Year","Irrigation","Domestic","Industrial")];
  #                                                       #combineWater<-data.frame(surfaceYr=lastSurRec$Year, groundYr=lastGrdRec$Year, SWAgrBefore=lastSurRec$Irrigation, GWAgrBefore=lastGrdRec$Irrigation, SWUrbBefore=sum(c(lastSurRec$Domestic,lastSurRec$Industrial),na.rm=T), GWUrbBefore=sum(c(lastGrdRec$Domestic,lastGrdRec$Industrial),na.rm=T));
  #                                                       combineWater<-data.frame(groundYr=lastGrdRec$Year, GWAgrBefore=lastGrdRec$Irrigation, GWUrbBefore=sum(c(lastGrdRec$Domestic,lastGrdRec$Industrial),na.rm=T))
  #                                                       return(combineWater)})
  #   grdLastCombineYrs <- do.call(rbind.data.frame, exGrdLastYrs)
  #   grdWaterRightRecsIndust <- data.frame(state=capitalizeStrings(states[stInd]), basinNum=names(stGrd), uniID=paste0(capitalizeStrings(states[stInd]),"_",names(stGrd)), lastSurfaceYr=grdLastCombineYrs$groundYr, GWAgrBefore=grdLastCombineYrs$GWAgrBefore, GWUrbBefore=grdLastCombineYrs$GWUrbBefore)
  #   
  #   
  #   
  #   
  #   ##get the state WMA data
  #   surWMALayer <- wmaLayer[[1]]
  #   stSurWMAS <- surWMALayer[tolower(surWMALayer$state)==tolower(st),]
  #   stGrdWMAS <- wmaLayer[[2]]
  #   ##calculate the area of the WMAS, then add it to the original WMA layer 
  #   stSurWMAArea <- calcArea(stSurWMAS, areaProj, "area")
  #   stSurWMAS$area <- stSurWMAArea$area
  #   stGrdWMAArea <- calcArea(stGrdWMAS, areaProj, "area")
  #   stGrdWMAS$area <- stGrdWMAArea$area
  #   
  #   #bnam <- stateHydroReg$basinName[4]
  #   waterBeforeByReg <- lapply(stateHydroReg$basinName, function(bnam){whichInd<-which(stateHydroReg$basinName==bnam);
  #                                                                     selectBasin<-stateHydroReg[whichInd,];
  #                                                                     ##crop the WMAs - surface
  #                                                                     surCroppedWMAs<-crop(stSurWMAArea,selectBasin);
  #                                                                     surCroppedAreas<-calcArea(surCroppedWMAs, areaProj, "area");
  #                                                                     surCroppedWMAs$cropArea<-surCroppedAreas$area;
  #                                                                     ##percent area of the WMA within the region
  #                                                                     surCroppedWMAs$perArea<-surCroppedWMAs$cropArea/surCroppedWMAs$area;
  #                                                                     ##crop the WMAs - ground
  #                                                                     grdCroppedWMAs<-crop(stGrdWMAArea,selectBasin);
  #                                                                     grdCroppedAreas<-calcArea(grdCroppedWMAs, areaProj, "area");
  #                                                                     grdCroppedWMAs$cropArea<-grdCroppedAreas$area;
  #                                                                     ##percent area of the WMA within the region
  #                                                                     grdCroppedWMAs$perArea<-grdCroppedWMAs$cropArea/grdCroppedWMAs$area;
  #   
  #                                                                     ##join the spatial data with the water rights data, only keeping those WMAs in the cropped area
  #                                                                     surMergedWMAs<-merge(x=surCroppedWMAs,y=waterRightRecsIndust,by=c("basinNum","uniID"));
  #                                                                     grdMergedWMAs<-merge(x=grdCroppedWMAs,y=waterRightRecsIndust,by=c("basinNum","uniID"));
  #                                                                     ##summarize water amounts by the select region
  #                                                                     ##first, modify the water amount by area
  #                                                                     selectBasin$SWAgrBefore<-sum(surMergedWMAs$SWAgrBefore*surMergedWMAs$perArea,na.rm=T);
  #                                                                     selectBasin$GWAgrBefore<-sum(grdMergedWMAs$GWAgrBefore*grdMergedWMAs$perArea,na.rm=T);
  #                                                                     selectBasin$SWUrbBefore<-sum(surMergedWMAs$SWUrbBefore*surMergedWMAs$perArea,na.rm=T);
  #                                                                     selectBasin$GWUrbBefore<-sum(grdMergedWMAs$GWUrbBefore*grdMergedWMAs$perArea,na.rm=T);
  #                                                                     return(selectBasin)})
  # }else{
  ##set up the water rights data, to get the correct volume of water available to all parties
  ##urban is a combination of domestic and industrial
  if(whichYear=="last"){
    exYrs <- lapply(1:length(stSur),function(wmaInd){lastSurRec<-stSur[[wmaInd]][nrow(stSur[[wmaInd]]),c("Year","Irrigation","Domestic","Industrial")];
    lastGrdRec<-stGrd[[wmaInd]][nrow(stGrd[[wmaInd]]),c("Year","Irrigation","Domestic","Industrial")];
    combineWater<-data.frame(surfaceYr=lastSurRec$Year, groundYr=lastGrdRec$Year, SWAgrBefore=lastSurRec$Irrigation, GWAgrBefore=lastGrdRec$Irrigation, SWUrbBefore=sum(c(lastSurRec$Domestic,lastSurRec$Industrial),na.rm=T), GWUrbBefore=sum(c(lastGrdRec$Domestic,lastGrdRec$Industrial),na.rm=T));
    return(combineWater)})
  }else if(whichYear=="cutoff"){
    if(is.null(cutOffYrs)){
      stop("If 'cutoff' used, a valid list of years need to be provided by 'cutOffYrs'.")
    }
    stCutOffYrs <- cutOffYrs[tolower(cutOffYrs$state)==tolower(st),]
    #stCutOffYrs <- stCutOffYrs[stCutOffYrs$basinNum%in%stateHydroReg$basinNum,]
    exYrs <- lapply(1:length(stSur),function(wmaInd){wmaCutOffYr<-stCutOffYrs$Cutoff_Date[which(sub("^0+","",stCutOffYrs$basinNum)%in%sub("^0+","",names(stSur[wmaInd])))];
    lastSurRec<-stSur[[wmaInd]][which(stSur[[wmaInd]]$Year==wmaCutOffYr),c("Year","Irrigation","Domestic","Industrial")];
    lastGrdRec<-stGrd[[wmaInd]][which(stSur[[wmaInd]]$Year==wmaCutOffYr),c("Year","Irrigation","Domestic","Industrial")];
    combineWater<-data.frame(surfaceYr=lastSurRec$Year, groundYr=lastGrdRec$Year, SWAgrBefore=lastSurRec$Irrigation, GWAgrBefore=lastGrdRec$Irrigation, SWUrbBefore=sum(c(lastSurRec$Domestic,lastSurRec$Industrial),na.rm=T), GWUrbBefore=sum(c(lastGrdRec$Domestic,lastGrdRec$Industrial),na.rm=T));
    return(combineWater)})
  }else{
    stop("Type of year not available.")
  }
  ##combine all WMA data into one object
  combineYrs <- do.call(rbind.data.frame, exYrs)
  waterRightRecsIndust <- data.frame(state=capitalizeStrings(states[stInd]), basinNum=names(stSur), uniID=paste0(capitalizeStrings(states[stInd]),"_",names(stSur)), lastSurfaceYr=combineYrs$surfaceYr, lastGroundYr=combineYrs$groundYr, SWAgrBefore=combineYrs$SWAgrBefore, GWAgrBefore=combineYrs$GWAgrBefore, SWUrbBefore=combineYrs$SWUrbBefore, GWUrbBefore=combineYrs$GWUrbBefore)
  
  ##get the state WMA data
  mainWMALayer <- wmaLayer[[1]]
  stWMAS <- mainWMALayer[tolower(mainWMALayer$state)==tolower(st),]
  ##calculate the area of the WMAS, then add it to the original WMA layer 
  stWMAArea <- calcArea(stWMAS, areaProj, "area")
  stWMAS$area <- stWMAArea$area
  
  ##set up a matrix of WWR trade regions by WMAs, to store of water rights that went into the WWRs
  holdWMAPer <- matrix(nrow=nrow(stWMAS), ncol=nrow(stateHydroReg))
  colnames(holdWMAPer) <- stateHydroReg$basinName
  rownames(holdWMAPer) <- stWMAS$basinNum
  
  #bnam <- stateHydroReg$basinName[8]
  ##begin filling out the percentage of WMA in each WWR
  sapply(stateHydroReg$basinName, function(bnam){whichInd<-which(stateHydroReg$basinName==bnam);
  selectBasin<-stateHydroReg[whichInd,];
  ##crop the WMAs
  croppedWMAs<-crop(stWMAArea,selectBasin);
  croppedAreas<-calcArea(croppedWMAs, areaProj, "area");
  croppedWMAs$cropArea<-croppedAreas$area;
  ##percent area of the WMA within the region
  croppedWMAs$perArea<-croppedWMAs$cropArea/croppedWMAs$area;
  ##adjust unrealistic percents, comes from rounding errors when calculating area
  croppedWMAs$perArea[which(croppedWMAs$perArea > 1)] <- 1
  ##add percent area to the holdWMAPer table
  holdWMAPer[sapply(croppedWMAs$basinNum, function(wmaInd){which(rownames(holdWMAPer) %in% wmaInd)}),whichInd] <<- croppedWMAs$perArea})
  # }
  
  chkPerTots <- rowSums(holdWMAPer, na.rm=T)
  #lessThan1Maxes <- as.numeric(apply(holdWMAPer[chkPerTots<1,], 1, which.max))
  lessThan1WMAs <- which(chkPerTots<1)
  if(length(lessThan1WMAs)==1){
    lessThan1Maxes <- as.numeric(which.max(holdWMAPer[lessThan1WMAs,]))
  }else{
    lessThan1Maxes <- as.numeric(apply(holdWMAPer[lessThan1WMAs,], 1, which.max))
  }
  ##make sure that all values add up to 1 on the percents of area used
  ##if less than 1, add to the WWR with the greatest area until equals 1
  mapply(function(nr,nc){holdWMAPer[nr,nc]<<-holdWMAPer[nr,nc]+(1-sum(holdWMAPer[nr,],na.rm=T))}, nr=lessThan1WMAs, nc=lessThan1Maxes)
  
  chkPerTots <- rowSums(holdWMAPer, na.rm=T)
  #greaterThan1Maxes <- as.numeric(apply(holdWMAPer[chkPerTots>1,], 1, which.max))
  greaterThan1WMAs <- which(chkPerTots>1)
  if(length(greaterThan1WMAs)==1){
    greaterThan1Maxes <- as.numeric(which.max(holdWMAPer[greaterThan1WMAs,]))
  }else{
    greaterThan1Maxes <- as.numeric(apply(holdWMAPer[greaterThan1WMAs,], 1, which.max))
  }
  
  ##if greater than 1, subtract from the WWR with the greatest area until equals 1
  mapply(function(nr,nc){holdWMAPer[nr,nc]<<-holdWMAPer[nr,nc]-(sum(holdWMAPer[nr,],na.rm=T)-1)}, nr=greaterThan1WMAs, nc=greaterThan1Maxes)
  
  #bnam <- stateHydroReg$basinName[4]
  #bnam <- "Boulder Creek"
  waterBeforeByReg <- lapply(stateHydroReg$basinName, function(bnam){whichInd<-which(stateHydroReg$basinName==bnam);
  #print(bnam)
  ##join the spatial data with the water rights data, only keeping those WMAs in the cropped area
  selectBasin<-stateHydroReg[whichInd,];
  
  ##isolate which WMAs were previously determined to be within the region
  basinWMAs <- holdWMAPer[,whichInd]
  ##clean out the NAs
  bNAInd <- which(is.na(basinWMAs)==F)
  basinWMAs <- basinWMAs[bNAInd]
  ##get the water rights amounts for each WMA within the WWR trading region
  #wmaNam<-names(basinWMAs)[1]
  getWMAIndex <- sapply(names(basinWMAs), function(wmaNam){wmaInd<-which(waterRightRecsIndust$basinNum %in% wmaNam)
  if(length(wmaInd)>0){
    return(wmaInd)
  }else{
    return(NA)
  }})
  ##remove NAs if needed
  naInd <- which(is.na(getWMAIndex)==F)  ##this assumes that there is at least one non-NA value
  if(length(naInd)>=1){
    getWMAIndex <- getWMAIndex[naInd]
    basinWMAs <- basinWMAs[naInd]
    
    wwrWMAs <- waterRightRecsIndust[getWMAIndex,]
    #wwrWMAs <- waterRightRecsIndust[waterRightRecsIndust$basinNum %in% names(basinWMAs),]
    
    ##summarize water amounts by the select region
    ##first, modify the water amount by area
    selectBasin$SWAgrBefore<-sum(wwrWMAs$SWAgrBefore*basinWMAs,na.rm=T);
    selectBasin$GWAgrBefore<-sum(wwrWMAs$GWAgrBefore*basinWMAs,na.rm=T);
    selectBasin$SWUrbBefore<-sum(wwrWMAs$SWUrbBefore*basinWMAs,na.rm=T);
    selectBasin$GWUrbBefore<-sum(wwrWMAs$GWUrbBefore*basinWMAs,na.rm=T);
  }else{
    ##happens in situations when the trade region == the WMA boundary, and there are no water right records for that WMA
    selectBasin$SWAgrBefore<-NA;
    selectBasin$GWAgrBefore<-NA;
    selectBasin$SWUrbBefore<-NA;
    selectBasin$GWUrbBefore<-NA;
  }
  return(selectBasin)
  
  })
  
  waterBeforeByReg <- do.call(rbind, waterBeforeByReg)
  waterBeforeByReg <- merge(x=waterBeforeByReg, y=stPrices, by="basinName")
  
  return(list(waterBeforeByReg,holdWMAPer))
}
##########################################################################
##########################################################################
applyTrdRegionsToWMAs <- function(st, modeledEconVals, spReg, wmaInReg, typeApplicaiton, surRights, grdRights, wmaLayer, areaProj, includeNegs=F, tableWriteDir, nameConvTab, surYrCutTab=NULL){
  #################
  #n <- 3
  #st <- stateNames[n]
  #modeledEconVals <- stEconModels[[n]]
  #spReg <- stateRegVol[[n]]
  #wmaInReg <- stateRegWMAPer[[n]]
  #typeApplicaiton <- c("earlyPrioDates", "waterRunsOutCutoffs")  ##curveShift, curveShift0, earlyPrioDates
  #surRights <- wmasRightsTotsSur
  #grdRights <- wmasRightsTotsGrd
  #wmaLayer <- wmasData
  #areaProj <- projForAreaCalc
  #includeNegs <- F
  #tableWriteDir <- paste0(cumuDir, "sectorCumuSummaries")
  #nameConvTab <- namConvData
  #surYrCutTab <- NULL
  #################
  print(st)
  if(typeApplicaiton[1]=="curveShift"){
    tableWriteDirSW <- paste0(tableWriteDir, "/", st, "/surfaceWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_noScen/")
    tableWriteDirGW <- paste0(tableWriteDir, "/", st, "/groundWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_noScen/")
  }else if(typeApplicaiton[1]=="curveShift0"){
    tableWriteDirSW <- paste0(tableWriteDir, "/", st, "/surfaceWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_noPerChanges/")
    tableWriteDirGW <- paste0(tableWriteDir, "/", st, "/groundWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_noPerChanges/")
  }else if(typeApplicaiton[1]=="earlyPrioDates"){
    ##checks to see if surYrCutTab has been included
    #if(is.null(surYrCutTab)==T){
    #  stop("When 'earlyPrioDates is selected, 'surYrCutTab' needs to be supplied.")
    #}
    tableWriteDirSW <- paste0(tableWriteDir, "/", st, "/surfaceWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_trdOldRights/")
    tableWriteDirGW <- paste0(tableWriteDir, "/", st, "/groundWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_trdOldRights/")
    #tableWriteDirSW <- paste0(tableWriteDir, "/", st, "_Hagerty/surfaceWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_trdOldRights/")
    #tableWriteDirGW <- paste0(tableWriteDir, "/", st, "_Hagerty/groundWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_trdOldRights/")
  }else{
    stop("Not recognized processing type 1.")
  }
  ##add type two type of model run - which years used.
  tableWriteDirSW <- paste0(tableWriteDirSW, typeApplicaiton[2], "/")
  tableWriteDirGW <- paste0(tableWriteDirGW, typeApplicaiton[2], "/")
  ##make sure that these directories exist
  if(dir.exists(tableWriteDirSW)==F){dir.create(tableWriteDirSW, recursive=T)}
  if(dir.exists(tableWriteDirGW)==F){dir.create(tableWriteDirGW, recursive=T)}
  
  ##assign warket results to their own object
  swVol <- modeledEconVals[[1]]
  gwVol <- modeledEconVals[[3]]
  
  ##set up the water rights data, to get the correct volume of water available to all parties
  ##urban is a combination of domestic and industrial
  stInd <- which(states==st)
  stSur <- surRights[[stInd]]
  stGrd <- grdRights[[stInd]]
  stSur <- stSurOrg <- lapply(stSur, function(x){x$Urban<-x$Domestic+x$Industrial;
  x$UrbanPerDom<-x$Domestic/x$Urban;
  x$UrbanPerInd<-x$Industrial/x$Urban;
  x[is.na(x)]<-0;
  return(x)})
  stGrd <- stGrdOrg <- lapply(stGrd, function(x){x$Urban<-x$Domestic+x$Industrial;
  x$UrbanPerDom<-x$Domestic/x$Urban;
  x$UrbanPerInd<-x$Industrial/x$Urban;
  x[is.na(x)]<-0;
  return(x)})
  
  ##whether or not to include negative values
  ##in this case, negative values represent the region buying more ag water than selling
  if(includeNegs==F){
    swVol[swVol<0] <- 0
    gwVol[gwVol<0] <- 0
  }
  
  ##summarize the market analysis output
  #trdsInBasin <- sapply(1:nrow(swVol), function(ind){val<-swVol[ind,ind];swVol[ind,ind]<<-NA;return(val)})
  swSoldVol <- rowSums(swVol, na.rm=T)
  swBuyVol <- colSums(swVol, na.rm=T)
  gwSoldVol <- rowSums(gwVol, na.rm=T)
  gwBuyVol <- colSums(gwVol, na.rm=T)
  
  
  ##check the ag rights sold, to make sure that they do not exceed the total originally available to the WWR trade region
  swCheckAgSold <- swSoldVol - spReg$SWAgrBefore
  gwCheckAgSold <- gwSoldVol - spReg$GWAgrBefore
  ##check for negatives, or those trade regions which have sold more ag water rights than they began with
  swWhichNeg <- which(swCheckAgSold > 0)
  gwWhichNeg <- which(gwCheckAgSold > 0)
  if(length(swWhichNeg)>0){
    ##modify the amount of water rights bough by urban to reflect that too many rights were purchased from ag
    #negWWR<-swWhichNeg[1]
    sapply(swWhichNeg, function(negWWR){swVol[negWWR,] <<- swVol[negWWR,] - ((swCheckAgSold[swWhichNeg] / swSoldVol[swWhichNeg]) * swVol[negWWR,])
    })
    swSoldVol <- rowSums(swVol, na.rm=T)
    swBuyVol <- colSums(swVol, na.rm=T)
  }
  if(length(gwWhichNeg)>0){
    ##modify the amount of water rights bough by urban to reflect that too many rights were purchased from ag
    #negWWR<-swWhichNeg[1]
    sapply(gwWhichNeg, function(negWWR){gwVol[negWWR,] <<- gwVol[negWWR,] - ((gwCheckAgSold[gwWhichNeg] / gwSoldVol[gwWhichNeg]) * gwVol[negWWR,])
    })
    gwSoldVol <- rowSums(gwVol, na.rm=T)
    gwBuyVol <- colSums(gwVol, na.rm=T)
  }
  
  ##get the state WMA data
  mainWMALayer <- wmaLayer[[1]]
  stWMAS <- mainWMALayer[tolower(mainWMALayer$state)==tolower(st),]
  stWMAS$basinName[which(is.na(stWMAS$basinName)==T)]<-stWMAS$basinNum[which(is.na(stWMAS$basinName)==T)]
  
  ##for each wma, check which region(s) it is in, and modify curves based on the percent change
  #bnam <- stWMAS$basinName[stWMAS$basinName=="ESTERO BAY"]
  #bnam <- stWMAS$basinName[stWMAS$basinName=="BODEGA"]
  #bnam <- stWMAS$basinName[stWMAS$basinName=="22"]
  #bnam <- stWMAS$basinName[5]
  #bnam <- stWMAS$basinName[stWMAS$basinName=="CARMEL RIVER"]
  #bnam <- stWMAS$basinName[stWMAS$basinName=="Cache La Poudre River"]
  #bnam <- stWMAS$basinName[stWMAS$basinName=="Upper Yakima"]
  #bnam <- stWMAS$basinName[stWMAS$basinName=="Arkansas - Fowler to Las Animas"]
  bnam <- stWMAS$basinName[stWMAS$basinName=="South Platte - Cheesman to Denver Gage"]
  #bnam <- stWMAS$basinName[stWMAS$basinName=="LOWER GILA RIVER"]
  #bnam <- stWMAS$basinName[stWMAS$basinName=="Upper Skagit"]
  #for(bnam in stWMAS$basinName[stWMAS$basinName=="South Platte - Cheesman to Denver Gage"]){whichInd<-which(stWMAS$basinName==bnam)
  for(bnam in stWMAS$basinName){whichInd<-which(stWMAS$basinName==bnam)
  print(bnam)
  selectBasin<-stWMAS[whichInd,];
  ##find which WWR trade regions the WMA contributed to, 
  ##or which WWR regions am I a part of
  perWMAinWWRs <- wmaInReg[rownames(wmaInReg)==selectBasin$basinNum,]
  ##percent area the WMA is within each WWR trade region,
  ##or how much of me is in each WWR region
  perWMAinWWRs <- perWMAinWWRs[is.na(perWMAinWWRs)==F]
  ##a flag to keep track of whether or not the water right curves have been / should be moddified
  #wmaCurveShiftFlag <- 1
  
  if(length(perWMAinWWRs)<1){
    #wmaCurveShiftFlag <- 0
    #return(NULL)
  }else{
    idRegs <- spReg[sapply(names(perWMAinWWRs),function(nam){which(spReg$basinName==nam)}),]
    if(selectBasin$basinNum %in% names(stSur)){
      ##get the water right values
      selectBasinSWRights<-stSur[[which(names(stSur)==selectBasin$basinNum)]]
      selectBasinGWRights<-stGrd[[which(names(stGrd)==selectBasin$basinNum)]]
      ##create copies of the water right curves in order to modify them but compare to originals
      selectBasinSWRightsMod<-selectBasinSWRights
      selectBasinGWRightsMod<-selectBasinGWRights
      
      ##disaggregate the water rights from the WWR trade regions to the selected WMA
      ##surface water agriculture
      if(sum(swSoldVol[which(names(swSoldVol) %in% idRegs$basinName)]) > 0){
        swAgIntoWWR <- selectBasinSWRights$Irrigation[nrow(selectBasinSWRights)] * perWMAinWWRs
        WMANewSWAgRights <- sum(c(swAgIntoWWR - (swAgIntoWWR / idRegs$SWAgrBefore) * swSoldVol[which(names(swSoldVol) %in% idRegs$basinName)]))
      }else{
        WMANewSWAgRights <- selectBasinSWRights$Irrigation[nrow(selectBasinSWRights)]
      }
      ##surface water urban
      if(sum(swBuyVol[which(names(swBuyVol) %in% idRegs$basinName)]) > 0){
        swUrbIntoWWR <- selectBasinSWRights$Urban[nrow(selectBasinSWRights)] * perWMAinWWRs
        WMANewSWUrbRights <- sum(c(swUrbIntoWWR + (swUrbIntoWWR / idRegs$SWUrbBefore) * swBuyVol[which(names(swBuyVol) %in% idRegs$basinName)]))
      }else{
        WMANewSWUrbRights <- selectBasinSWRights$Urban[nrow(selectBasinSWRights)]
      }
      ##groundwater agriculture
      if(sum(gwSoldVol[which(names(gwSoldVol) %in% idRegs$basinName)]) > 0){
        gwAgIntoWWR <- selectBasinGWRights$Irrigation[nrow(selectBasinGWRights)] * perWMAinWWRs
        WMANewGWAgRights <- sum(c(gwAgIntoWWR - (gwAgIntoWWR / idRegs$GWAgrBefore) * gwSoldVol[which(names(gwSoldVol) %in% idRegs$basinName)]))
      }else{
        WMANewGWAgRights <- selectBasinGWRights$Irrigation[nrow(selectBasinGWRights)]
      }
      ##groundwater urban
      if(sum(gwBuyVol[which(names(gwBuyVol) %in% idRegs$basinName)]) > 0){
        gwUrbIntoWWR <- selectBasinGWRights$Urban[nrow(selectBasinGWRights)] * perWMAinWWRs
        WMANewGWUrbRights <- sum(c(gwUrbIntoWWR + (gwUrbIntoWWR / idRegs$GWUrbBefore) * gwBuyVol[which(names(gwBuyVol) %in% idRegs$basinName)]))
      }else{
        WMANewGWUrbRights <- selectBasinGWRights$Urban[nrow(selectBasinGWRights)]
      }
      
      ##create the percent used to modify WMA curves
      if(typeApplicaiton[1]=="curveShift" | typeApplicaiton[1]=="earlyPrioDates"){
        if(selectBasinSWRights$Irrigation[nrow(selectBasinSWRights)]==0){
          swPerAgAdjust <- 0
        }else{
          swPerAgAdjust <- WMANewSWAgRights / selectBasinSWRights$Irrigation[nrow(selectBasinSWRights)]
        }
        if(selectBasinSWRights$Urban[nrow(selectBasinSWRights)]==0){
          swPerUrbAdjust <- 0
        }else{
          swPerUrbAdjust <- WMANewSWUrbRights / selectBasinSWRights$Urban[nrow(selectBasinSWRights)]
        }
        if(selectBasinGWRights$Irrigation[nrow(selectBasinGWRights)]==0){
          gwPerAgAdjust <- 0
        }else{
          gwPerAgAdjust <- WMANewGWAgRights / selectBasinGWRights$Irrigation[nrow(selectBasinGWRights)]
        }
        if(selectBasinGWRights$Urban[nrow(selectBasinGWRights)]==0){
          gwPerUrbAdjust <- 0
        }else{
          gwPerUrbAdjust <- WMANewGWUrbRights / selectBasinGWRights$Urban[nrow(selectBasinGWRights)]
        }
      }else if(typeApplicaiton=="curveShift0"){
        ##a value of 1 means that the WMA curves should not change
        swPerAgAdjust <- 1
        swPerUrbAdjust <- 1
        gwPerAgAdjust <- 1
        gwPerUrbAdjust <- 1
      }else{
        ##do nothing for now
      }
      
      ##fill out the results on the output tables - surface water
      selectBasinSWRightsMod$Irrigation <- selectBasinSWRightsMod$Irrigation * swPerAgAdjust
      selectBasinSWRightsMod$Domestic <- selectBasinSWRights$Urban * swPerUrbAdjust * selectBasinSWRights$UrbanPerDom
      selectBasinSWRightsMod$Industrial <- selectBasinSWRights$Urban * swPerUrbAdjust * selectBasinSWRights$UrbanPerInd
      
      ##fill out the results on the output tables - groundwater
      selectBasinGWRightsMod$Irrigation <- selectBasinGWRightsMod$Irrigation * gwPerAgAdjust
      selectBasinGWRightsMod$Domestic <- selectBasinGWRights$Urban * gwPerUrbAdjust * selectBasinGWRights$UrbanPerDom
      selectBasinGWRightsMod$Industrial <- selectBasinGWRights$Urban * gwPerUrbAdjust * selectBasinGWRights$UrbanPerInd
      
      ##remake the curve total column
      selectBasinSWRightsMod$CUML<-rowSums(selectBasinSWRightsMod[,which(colnames(selectBasinSWRightsMod)=="Irrigation"):which(colnames(selectBasinSWRightsMod)=="Other")])
      selectBasinGWRightsMod$CUML<-rowSums(selectBasinGWRightsMod[,which(colnames(selectBasinGWRightsMod)=="Irrigation"):which(colnames(selectBasinGWRightsMod)=="Other")])
      
      if(typeApplicaiton[1]=="curveShift" | typeApplicaiton[1]=="curveShift0"){
        selectBasinSWperMod<-selectBasinSWRightsMod
        selectBasinGWperMod<-selectBasinGWRightsMod
      }else if(typeApplicaiton[1]=="earlyPrioDates"){
        ##modify the original water rights data by trading the earliest rights
        selectBasinSWperMod<-stSur[[which(names(stSur)==selectBasin$basinNum)]]
        selectBasinGWperMod<-stGrd[[which(names(stGrd)==selectBasin$basinNum)]]
        
        ##check to make sure that the agricultural CFS have been modified
        ##if they have not, set the variables to reflect to not change further
        surChngChk <- unique(round(selectBasinSWperMod$Irrigation,6)==round(selectBasinSWRightsMod$Irrigation,6))
        if((length(surChngChk)==1 & surChngChk[1]==TRUE) | is.na(surChngChk[1])==TRUE){
          surChanged <- 0
          surIrrTotal <- surUrbAdd <- 0
        }else{
          surChanged <- 1
          #surIrrTotal <- surUrbAdd <- sum(selectBasinSWRightsMod$Irrigation)
          surIrrTotal <- surUrbAdd <- selectBasinSWRightsMod$Irrigation[nrow(selectBasinSWRightsMod)]
        }
        ##check to make sure that the agricultural CFS have been modified
        ##if they have not, set the variables to reflect to not change further
        grdChngChk <- unique(round(selectBasinGWperMod$Irrigation,6)==round(selectBasinGWRightsMod$Irrigation,6))
        if((length(grdChngChk)==1 & grdChngChk[1]==TRUE) | is.na(grdChngChk[1])==TRUE){
          grdChanged <- 0
          grdIrrTotal <- grdUrbAdd <- 0
        }else{
          grdChanged <- 1
          #grdIrrTotal <- grdUrbAdd <- sum(selectBasinGWRightsMod$Irrigation)
          grdIrrTotal <- grdUrbAdd <- selectBasinGWRightsMod$Irrigation[nrow(selectBasinGWRightsMod)]
        }
        
        ##surface water
        ##determine the WMAs in which my (selected WMA) water is going to
        ##first, the WWR trading regions I am a part of are already included
        ##in perWMAinWWRs
        ##Use perWMAinWWRs to determine the WWR regions my region(s) traded with
        swRegionVals <- c()  ##used to determine how much water each trade region should get
        swValsWithinRegions <- list()  ##used to determine how much water each WMA within a trade region should get
        for(myTrdReg in names(perWMAinWWRs)){
          mySWRegTrds <- swVol[which(rownames(swVol) %in% myTrdReg),]  ##the WWR regions the selected region has as trading partners
          mySWRegTrds <- mySWRegTrds[is.na(mySWRegTrds)==F]  ##removing NAs
          mySWRegTrds <- mySWRegTrds[mySWRegTrds>0]  ##to simpligy processing, only continue with regions that have a trade value greater than 0
          
          if(length(mySWRegTrds)>0){
            ##an object to catch the WMA data for all WMAs from all trading region partners
            exWMATotalRights <- list()
            perWMAinReg <- list()
            ##an object to hold the trade region totals
            regTotal <- c()
            prevUrban <- c()
            for(mySWRegTrdName in names(mySWRegTrds)){
              ##now, with the trading regions that my trading region sold to, get the WMAs which are a part of buying trade region
              wmasOfTrdReg <- wmaInReg[,which(colnames(wmaInReg)==mySWRegTrdName)]  ##the WMAs of the trade region my trade region is trading with
              ##collect the names of the WMAS in which the selected WMA traded with, 
              ##based on the the WWR trade regions they are a part of
              wmasOfTrdReg[is.na(wmasOfTrdReg)] <- 0 
              wmasOfTrdReg <- wmasOfTrdReg[wmasOfTrdReg>0]  ##this are the WMAS in the selected trading region, and the percent contributing rights from each WMA
            
              ##double check and make sure that the WMA has a water rights table
              ##for certain states, like California, in which due to various reasons the WMA does not have rights
              wmasOfTrdReg <- wmasOfTrdReg[which(names(wmasOfTrdReg) %in% names(stSur))]
              perWMAinReg[[length(perWMAinReg)+1]] <- wmasOfTrdReg            
              ##calculate the amount of water rights each WMA contributed to the region
              exWMATotalRights[[length(exWMATotalRights)+1]] <- sapply(names(wmasOfTrdReg), function(x){stSurOrg[[which(names(stSurOrg)==x)]]$Urban[nrow(stSurOrg[[which(names(stSurOrg)==x)]])]})
              ##total water rights contributed to the region
              regTotal[length(regTotal)+1] <- sum(exWMATotalRights[[length(exWMATotalRights)]])
              #wmaPerOfReg <- (exWMATotalRights * wmasOfTrdReg) / spReg$SWUrbBefore[spReg$basinName==mySWRegTrdName]
              prevUrban[length(prevUrban)+1] <- spReg$SWUrbBefore[spReg$basinName==mySWRegTrdName]
            }
            exWMATotalRights <- unlist(exWMATotalRights)
            perWMAinReg <- unlist(perWMAinReg)
            
            ##add per WMA values to the WMAs for the trade region
            swValsWithinRegions[[length(swValsWithinRegions)+1]] <- (exWMATotalRights * perWMAinReg) / sum((exWMATotalRights * perWMAinReg))
            names(swValsWithinRegions)[length(swValsWithinRegions)] <- myTrdReg
            
            ##add total CFS value to the swRegionVals vector
            swRegionVals[length(swRegionVals)+1] <- mySWRegTrds[which(names(mySWRegTrds)==mySWRegTrdName)] #/ sum(mySWRegTrds)
            names(swRegionVals)[length(swRegionVals)] <- myTrdReg
        }
        swRegionVals <- swRegionVals / sum(swRegionVals)
        
        
        ##ground water
        gwRegionVals <- c()  ##used to determine how much water each trade region should get
        gwValsWithinRegions <- list()  ##used to determine how much water each WMA within a trade region should get
        for(myTrdReg in names(perWMAinWWRs)){
          myGWRegTrds <- gwVol[which(rownames(gwVol) %in% myTrdReg),]  ##the WWR regions the selected region has as trading partners
          myGWRegTrds <- myGWRegTrds[is.na(myGWRegTrds)==F]  ##removing NAs
          myGWRegTrds <- myGWRegTrds[myGWRegTrds>0]  ##to simpligy processing, only continue with regions that have a trade value greater than 0
          
          if(length(myGWRegTrds)>0){
            ##an object to catch the WMA data for all WMAs from all trading region partners
            exWMATotalRights <- list()
            perWMAinReg <- list()
            ##an object to hold the trade region totals
            regTotal <- c()
            prevUrban <- c()
            for(myGWRegTrdName in names(myGWRegTrds)){
              ##now, with the trading regions that my trading region sold to, get the WMAs which are a part of buying trade region
              wmasOfTrdReg <- wmaInReg[,which(colnames(wmaInReg)==myGWRegTrdName)]  ##the WMAs of the trade region my trade region is trading with
              ##collect the names of the WMAS in which the selected WMA traded with, 
              ##based on the the WWR trade regions they are a part of
              wmasOfTrdReg[is.na(wmasOfTrdReg)] <- 0 
              wmasOfTrdReg <- wmasOfTrdReg[wmasOfTrdReg>0]  ##this are the WMAS in the selected trading region, and the percent contributing rights from each WMA
              
              ##double check and make sure that the WMA has a water rights table
              ##for certain states, like California, in which due to various reasons the WMA does not have rights
              wmasOfTrdReg <- wmasOfTrdReg[which(names(wmasOfTrdReg) %in% names(stGrd))]
              perWMAinReg[[length(perWMAinReg)+1]] <- wmasOfTrdReg            
              ##calculate the amount of water rights each WMA contributed to the region
              exWMATotalRights[[length(exWMATotalRights)+1]] <- sapply(names(wmasOfTrdReg), function(x){stGrdOrg[[which(names(stGrdOrg)==x)]]$Urban[nrow(stGrdOrg[[which(names(stGrdOrg)==x)]])]})
              ##total water rights contributed to the region
              regTotal[length(regTotal)+1] <- sum(exWMATotalRights[[length(exWMATotalRights)]])
              #wmaPerOfReg <- (exWMATotalRights * wmasOfTrdReg) / spReg$SWUrbBefore[spReg$basinName==mySWRegTrdName]
              prevUrban[length(prevUrban)+1] <- spReg$GWUrbBefore[spReg$basinName==myGWRegTrdName]
            }
            exWMATotalRights <- unlist(exWMATotalRights)
            perWMAinReg <- unlist(perWMAinReg)
            
            ##add per WMA values to the WMAs for the trade region
            gwValsWithinRegions[[length(gwValsWithinRegions)+1]] <- (exWMATotalRights * perWMAinReg) / sum((exWMATotalRights * perWMAinReg))
            names(gwValsWithinRegions)[length(gwValsWithinRegions)] <- myTrdReg
            
            ##add total CFS value to the swRegionVals vector
            gwRegionVals[length(gwRegionVals)+1] <- myGWRegTrds[which(names(myGWRegTrds)==myGWRegTrdName)] #/ sum(mySWRegTrds)
            names(gwRegionVals)[length(gwRegionVals)] <- myTrdReg
          }
        }
        gwRegionVals <- gwRegionVals / sum(gwRegionVals)
        
        ##decrease surface water rights irrigation based on the amount from the shifted curves
        ##first, find the first year of agriculture rights, so as to not process a long series
        ##of years with 0 data ofr most WMAs
        surRightsRow<-min(which(selectBasinSWperMod$Irrigation>0))-1
        if(length(surRightsRow)==0 | is.infinite(surRightsRow)==T){
          surRightsRow<-1884
        }
        grdRightsRow<-min(which(selectBasinGWperMod$Irrigation>0))-1
        if(length(grdRightsRow)==0 | is.infinite(grdRightsRow)==T){
          grdRightsRow<-1884
        }
        
        while(surIrrTotal>0){
          surRightsRow<-surRightsRow+1
          yrAmount<-selectBasinSWperMod[surRightsRow,"Irrigation"]
          if(is.na(yrAmount)==T & surRightsRow>=nrow(selectBasinSWperMod)){
            ##for outlying cases where a very small amount of water is left in surIrrTotal,
            ##but the number of rows of selectBasinSWperMod has been exceeded
            surIrrTotal<-0
            selectBasinSWperMod[nrow(selectBasinSWperMod),"Irrigation"]<-0
            wwrRights <- 0
          }else if(yrAmount>=surIrrTotal){
            selectBasinSWperMod$Irrigation[surRightsRow:nrow(selectBasinSWperMod)] <- selectBasinSWperMod$Irrigation[surRightsRow:nrow(selectBasinSWperMod)] - surIrrTotal
            ##first, amount of water per WWR trade region
            wwrRights <- surIrrTotal * swRegionVals
            
            ##if yrAmount is greather than surIrrTotal, subtract surIrrTotal. Should end with 0
            surIrrTotal<-surIrrTotal-surIrrTotal
          }else{
            ##subtract ag water rights from the selected WMA
            selectBasinSWperMod$Irrigation[surRightsRow:nrow(selectBasinSWperMod)] <- selectBasinSWperMod$Irrigation[surRightsRow:nrow(selectBasinSWperMod)] - yrAmount
            ##if yrAmount is less than surIrrTotal, subtract yrAmount - the typical condition in the middle of processing
            surIrrTotal<-surIrrTotal-yrAmount
            
            ##first, amount of water per WWR trade region
            wwrRights <- yrAmount * swRegionVals
          }
          
          if(length(swRegionVals)>0){
            for(wwrInd in 1:length(swRegionVals)){
              ##how much rights each WMA in the WWR trade region should get
              regWMA <- swValsWithinRegions[[wwrInd]] * wwrRights[wwrInd]
              ##now, cycle though each WMA and add the amount of water rights urban portions should receive
              for(wmaInd in 1:length(regWMA)){
                ##get the table for the appropriate WMA
                wmaWaterRights<-stSur[[which(names(stSur)==names(regWMA)[wmaInd])]]
                wmaFirstUrbanYr<-min(which(wmaWaterRights$Urban>0))  ##get the first urban year for the WMA, in case needed for water right years before urban is available
                if(wmaFirstUrbanYr>surRightsRow){
                  wmaWaterRights$Domestic[surRightsRow:nrow(wmaWaterRights)] <- sapply(surRightsRow:nrow(wmaWaterRights), function(x){sum(c(wmaWaterRights$Domestic[x], (regWMA[wmaInd] * wmaWaterRights$UrbanPerDom[wmaFirstUrbanYr])), na.rm=T)})
                  wmaWaterRights$Industrial[surRightsRow:nrow(wmaWaterRights)] <- sapply(surRightsRow:nrow(wmaWaterRights), function(x){sum(c(wmaWaterRights$Industrial[x], (regWMA[wmaInd] * wmaWaterRights$UrbanPerInd[wmaFirstUrbanYr])), na.rm=T)})
                }else{
                  wmaWaterRights$Domestic[surRightsRow:nrow(wmaWaterRights)] <- sapply(surRightsRow:nrow(wmaWaterRights), function(x){sum(c(wmaWaterRights$Domestic[x], (regWMA[wmaInd] * wmaWaterRights$UrbanPerDom[surRightsRow])), na.rm=T)})
                  wmaWaterRights$Industrial[surRightsRow:nrow(wmaWaterRights)] <- sapply(surRightsRow:nrow(wmaWaterRights), function(x){sum(c(wmaWaterRights$Industrial[x], (regWMA[wmaInd] * wmaWaterRights$UrbanPerInd[surRightsRow])), na.rm=T)})
                }
                ##assigning the modified urban values to back to the WMA list
                stSur[[which(names(stSur)==names(regWMA)[wmaInd])]] <- wmaWaterRights
              }
            }
          }              
        }
        ##assign the new ag values back to the original WMA table
        ##new copy as original copy may have been modified by added urban water rights
        stSur[[which(names(stSur)==selectBasin$basinNum)]]$Irrigation <- selectBasinSWperMod$Irrigation
        
        while(grdIrrTotal>0){
          #print(paste0("WMA ", selectBasin$basinNum, " grdIrrTotal loc3: ", grdIrrTotal))
          grdRightsRow<-grdRightsRow+1
          yrAmount<-selectBasinGWperMod[grdRightsRow,"Irrigation"]
          if(is.na(yrAmount)==T & grdRightsRow>=nrow(selectBasinGWperMod)){
            ##for outlying cases where a very small amount of water is left in surIrrTotal,
            ##but the number of rows of selectBasinSWperMod has been exceeded
            grdIrrTotal<-0
            selectBasinGWperMod[nrow(selectBasinGWperMod),"Irrigation"]<-0
            wwrRights <- 0
          }else if(yrAmount>=grdIrrTotal){
            selectBasinGWperMod$Irrigation[grdRightsRow:nrow(selectBasinGWperMod)] <- selectBasinGWperMod$Irrigation[grdRightsRow:nrow(selectBasinGWperMod)] - grdIrrTotal
            ##if yrAmount is greather than surIrrTotal, subtract surIrrTotal. Should end with 0
            ##first, amount of water per WWR trade region
            wwrRights <- grdIrrTotal * gwRegionVals
            
            grdIrrTotal<-grdIrrTotal-grdIrrTotal
            
          }else{
            ##if yrAmount is less than surIrrTotal, subtract yrAmoun
            selectBasinGWperMod$Irrigation[grdRightsRow:nrow(selectBasinGWperMod)] <- selectBasinGWperMod$Irrigation[grdRightsRow:nrow(selectBasinGWperMod)] - yrAmount            
            grdIrrTotal<-grdIrrTotal-yrAmount
            
            ##add urban water rights to the WMAs of the trade regions our trade region traded with
            ##first, amount of water per WWR trade region
            wwrRights <- yrAmount * gwRegionVals
          }
          
          if(length(gwRegionVals)>0){
            for(wwrInd in 1:length(gwRegionVals)){
              ##how much rights each WMA in the WWR trade region should get
              regWMA <- gwValsWithinRegions[[wwrInd]] * wwrRights[wwrInd] 
              ##now, cycle though each WMA and add the amount of water rights urban portions should recieve
              for(wmaInd in 1:length(regWMA)){
                ##get the table for the appropriate WMA
                wmaWaterRights<-stGrd[[which(names(stGrd)==names(regWMA)[wmaInd])]]
                wmaFirstUrbanYr<-min(which(wmaWaterRights$Urban>0))  ##get the first urban year for the WMA, in case needed for water right years before urban is available
                if(wmaFirstUrbanYr>grdRightsRow){
                  wmaWaterRights$Domestic[grdRightsRow:nrow(wmaWaterRights)] <- sapply(grdRightsRow:nrow(wmaWaterRights), function(x){sum(c(wmaWaterRights$Domestic[x], (regWMA[wmaInd] * wmaWaterRights$UrbanPerDom[wmaFirstUrbanYr])), na.rm=T)})
                  wmaWaterRights$Industrial[grdRightsRow:nrow(wmaWaterRights)] <- sapply(grdRightsRow:nrow(wmaWaterRights), function(x){sum(c(wmaWaterRights$Industrial[x], (regWMA[wmaInd] * wmaWaterRights$UrbanPerInd[wmaFirstUrbanYr])), na.rm=T)})
                }else{
                  wmaWaterRights$Domestic[grdRightsRow:nrow(wmaWaterRights)] <- sapply(grdRightsRow:nrow(wmaWaterRights), function(x){sum(c(wmaWaterRights$Domestic[x], (regWMA[wmaInd] * wmaWaterRights$UrbanPerDom[grdRightsRow])), na.rm=T)})
                  wmaWaterRights$Industrial[grdRightsRow:nrow(wmaWaterRights)] <- sapply(grdRightsRow:nrow(wmaWaterRights), function(x){sum(c(wmaWaterRights$Industrial[x], (regWMA[wmaInd] * wmaWaterRights$UrbanPerInd[grdRightsRow])), na.rm=T)})
                }
                ##assigning the modified urban values to back to the WMA list
                stGrd[[which(names(stGrd)==names(regWMA)[wmaInd])]] <- wmaWaterRights
              }
            }
          }
        }
        ##assign the new ag values back to the original WMA table
        ##new copy as original copy may have been modified by added urban water rights
        stGrd[[which(names(stGrd)==selectBasin$basinNum)]]$Irrigation <- selectBasinGWperMod$Irrigation
      }}}}
  
  ##remove WMAs which are not to be included in the analysis
  ##these are "WMAs" which do not have geographies, or which fall within geographies of other larger WMAs
  badInd <- NA
  if(st=="idaho"){
    badInd <- which(names(stSur)%in%c("1","2","3"))  ##removes 1, 2, and 3
  }else if(st=="newMexico"){
    badInd <- which(names(stSur)%in%c("LWD","SD","SP"))
  }else if(st=="california"){
    badInd <- which(names(stSur)=="4406")
  }else if(st=="utah"){
    badInd <- which(names(stSur)=="00")
  }
  stSur[c(badInd)] <- NULL
  stGrd[c(badInd)] <- NULL
  stSurOrg[c(badInd)] <- NULL
  stGrdOrg[c(badInd)] <- NULL
  #names(modWRs) <- stWMAS$basinNum
  stSur[sapply(stSur, is.null)] <- NULL
  stGrd[sapply(stGrd, is.null)] <- NULL
  stSurOrg[sapply(stSur, is.null)] <- NULL
  stGrdOrg[sapply(stGrd, is.null)] <- NULL
  
  
  stConvTab <- nameConvTab[nameConvTab$state==capitalizeStrings(st),]
  modWRs <- mapply(function(sw,swo,gw,nam,st,convTab){##create percentages from the new CUML totals
    sw <- sw[,1:which(colnames(sw)=="Other")]
    sw$CUML<-rowSums(sw[,which(colnames(sw)=="Irrigation"):which(colnames(sw)=="Other")])
    
    sw[is.na(sw)==T] <- 0
    orgTab <- stSurOrg[[which(names(stSurOrg)==nam)]]
    if(sw$Irrigation[nrow(sw)] > orgTab$Irrigation[nrow(orgTab)]){print(nam)}
    if(sw$Domestic[nrow(sw)] < orgTab$Domestic[nrow(orgTab)]){print(nam)}
    if(sw$Industrial[nrow(sw)] < orgTab$Industrial[nrow(orgTab)]){print(nam)}
    
    sw$Irrigation<-sw$Irrigation/sw$CUML*100
    sw$Domestic<-sw$Domestic/sw$CUML*100
    sw$Livestock<-sw$Livestock/sw$CUML*100
    sw$Fish<-sw$Fish/sw$CUML*100
    sw$Industrial<-sw$Industrial/sw$CUML*100
    sw$Environmental<-sw$Environmental/sw$CUML*100
    sw$Other<-sw$Other/sw$CUML*100
    sw[is.na(sw)==T] <- 0
    
    gw <- gw[,1:which(colnames(gw)=="Other")]
    gw$CUML<-rowSums(gw[,which(colnames(gw)=="Irrigation"):which(colnames(gw)=="Other")])
    gw$Irrigation<-gw$Irrigation/gw$CUML*100
    gw$Domestic<-gw$Domestic/gw$CUML*100
    gw$Livestock<-gw$Livestock/gw$CUML*100
    gw$Fish<-gw$Fish/gw$CUML*100
    gw$Industrial<-gw$Industrial/gw$CUML*100
    gw$Environmental<-gw$Environmental/gw$CUML*100
    gw$Other<-gw$Other/gw$CUML*100
    gw[is.na(gw)==T] <- 0
    
    lstToReturn<-list(sw,gw)
    unhNam <- convTab$ID[which(convTab$basinNum==sub("^0+", "", nam))]
    if(length(unhNam)==0){
      unhNam <- convTab$ID[which(convTab$basinNum==nam)]
    }
    #print(paste0("original name: ", nam, "; new name: ", unhNam))
    
    names(lstToReturn)<-c(unhNam,unhNam)
    return(lstToReturn)}, sw=stSur, swo=stSurOrg, gw=stGrd, nam=names(stSur), st=st, 
    MoreArgs=list(convTab=stConvTab), SIMPLIFY=F)
  
  # print(st)
  # fff <- sapply(stSurOrg, function(lst){return(lst$CUML[nrow(lst)])})
  # print(length(fff))
  # fff2 <- sum(fff)
  # print(paste0("original water volume: ", fff2))
  # lll <- sapply(modWRs, function(lst){return(lst[[1]]$CUML[nrow(lst[[1]])])})  ##original format
  # print(length(lll))
  # lll2 <- sum(lll)
  # print(paste0("modeled water volume: ", lll2))
  # 
  # bbb <- sapply(stGrdOrg, function(lst){return(lst$CUML[nrow(lst)])})
  # print(length(bbb))
  # bbb2 <- sum(bbb)
  # print(paste0("original water volume: ", bbb2))
  # ddd <- sapply(modWRs, function(lst){return(lst[[2]]$CUML[nrow(lst[[2]])])})  ##original format
  # print(length(ddd))
  # ddd2 <- sum(ddd)
  # print(paste0("modeled water volume: ", ddd2))
  
  ##write out the results
  lapply(modWRs, function(lst){write.csv(lst[[1]], paste0(tableWriteDirSW, "WMA_", names(lst[1]), "_SW.csv"), row.names=F)
    write.csv(lst[[2]], paste0(tableWriteDirGW, "WMA_", names(lst[2]), "_GW.csv"), row.names=F)})
  
  return(modWRs)
}
}
##########################################################################
##########################################################################
econModelStates <- function(spReg, trdType, agElast, urbElast, swTrdRels=NULL, gwTrdRels=NULL){
  #################
  #n <- 1
  #spReg <- stateRegVol[[n]]
  #trdType <- "historical"
  #agElast <- agrElast[n]
  #urbElast <- urbElast[n]
  #swTrdRels <- swHistTrdRelations[[n]]
  #gwTrdRels <- gwHistTrdRelations[[n]]
  #################
  #print(spReg$basinName)
  ##set up output objects
  outMatSWTotVol <- matrix(data=NA, nrow=length(spReg$basinName), ncol=length(spReg$basinName))
  colnames(outMatSWTotVol) <- spReg$basinName
  rownames(outMatSWTotVol) <- spReg$basinName
  outMatSWTotPct <- outMatSWTotVol
  outMatGWTotVol <- outMatSWTotVol
  outMatGWTotPct <- outMatSWTotVol
  outMatSWWelfareAg <- outMatSWTotVol
  outMatSWWelfareUrb <- outMatSWTotVol
  outMatGWWelfareAg <- outMatSWTotVol
  outMatGWWelfareUrb <- outMatSWTotVol
  outMatSWAgrP <- outMatSWTotVol
  outMatSWUrbP <- outMatSWTotVol
  outMatGWAgrP <- outMatSWTotVol
  outMatGWUrbP <- outMatSWTotVol
  outMatSWsqUrb <- outMatSWTotVol
  outMatGWsqUrb <- outMatGWTotVol
  outMatSWagrK <- outMatSWTotVol
  outMatSWurbK <- outMatSWTotVol
  outMatGWagrK <- outMatSWTotVol
  outMatGWurbK <- outMatSWTotVol
  
  #fullTrdRels <- outMatSWTotVol
  
  
  ##run the economic curve fitting models for surface water1
  #sellerName<-unique(gwTrdRels$SellerRegion)[7]
  #sellerName<-unique(swTrdRels$SellerRegion)[5]
  #sellerName<-unique(swTrdRels$SellerRegion)[2]
  if(trdType=="historical"){
    sapply(unique(c(swTrdRels$SellerRegion,gwTrdRels$SellerRegion)), function(sellerName){sellerInd <- which(spReg$basinName==sellerName)
                                                      ##identify the trade relationships that the seller has historically had
                                                      ##from all trades, isolate those where water was specifically being traded to trade partner's urban sectors
                                                      swHistTrds <- swTrdRels[swTrdRels$SellerRegion==sellerName & swTrdRels$agSoldToUrb>0,]
                                                      gwHistTrds <- gwTrdRels[gwTrdRels$SellerRegion==sellerName & gwTrdRels$agSoldToUrb>0,]
                                                      ##ID the trade partners that are not self
                                                      swHistTrdPart <- swHistTrds$BuyerRegion
                                                      #swHistTrdPart <- swHistTrdPart[-c(which(swHistTrdPart==sellerName))]
                                                      gwHistTrdPart <- gwHistTrds$BuyerRegion
                                                      #gwHistTrdPart <- gwHistTrdPart[-c(which(gwHistTrdPart==sellerName))]
                                                      
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
                                                      if(TRUE %in% unique(is.na(swHistTrdPart))){
                                                        swHistTrdPart <- swHistTrdPart[-c(which(is.na(swHistTrdPart)==T))]
                                                      }
                                                      regionSWAgVol <- regionSWAgPr <- rep(NA, length(spReg$basinName))
                                                      swBuyersInd <- sapply(swHistTrdPart, function(x){which(spReg$basinName==x)})
                                                      if(TRUE %in% unique(is.na(gwHistTrdPart))){
                                                        gwHistTrdPart <- gwHistTrdPart[-c(which(is.na(gwHistTrdPart)==T))]
                                                      }
                                                      regionGWAgVol <- regionGWAgPr <- rep(NA, length(spReg$basinName))
                                                      gwBuyersInd <- sapply(gwHistTrdPart, function(x){which(spReg$basinName==x)})
                                                    
                                                      ##extract the original water rights into a vector to preform the economic modeling
                                                      if(length(swBuyersInd)>0 & length(sellerInd)>0){ 
                                                        regionSWAgVol[swBuyersInd] <- spReg$SWAgrBefore[sellerInd]
                                                        regionSWAgPr[swBuyersInd] <- spReg$swAvePrAgrPay[sellerInd]
                                                        
                                                        ##preforms the economic curve fitting
                                                        fitSellerTrdsSW <- fitEconCurves(regionSWAgVol, agElast, regionSWAgPr, spReg$SWUrbBefore, urbElast, spReg$swAvePrUrbPay, curve="nl")
                                                        
                                                        ##fill the output objects with the economic modeling results
                                                        outMatSWTotVol[sellerInd,] <<- fitSellerTrdsSW$totTradeVol
                                                        outMatSWTotPct[sellerInd,] <<- fitSellerTrdsSW$pctTradeVol
                                                        outMatSWWelfareAg[sellerInd,] <<- fitSellerTrdsSW$welAgr
                                                        outMatSWWelfareUrb[sellerInd,] <<- fitSellerTrdsSW$welUrb
                                                        outMatSWAgrP[sellerInd,] <<- fitSellerTrdsSW$agrMarketP
                                                        outMatSWUrbP[sellerInd,] <<- fitSellerTrdsSW$urbMarketP
                                                        outMatSWsqUrb[sellerInd,] <<- fitSellerTrdsSW$squareUrb
                                                        outMatSWagrK[sellerInd,] <<- fitSellerTrdsSW$agrK
                                                        outMatSWurbK[sellerInd,] <<- fitSellerTrdsSW$urbK
                                  
                                                      }
                                                      if(length(gwBuyersInd)>0 & length(sellerInd)>0){
                                                        regionGWAgVol[gwBuyersInd] <- spReg$GWAgrBefore[sellerInd]
                                                        regionGWAgPr[gwBuyersInd] <- spReg$gwAvePrAgrPay[sellerInd]
                                                        
                                                        ##preforms the economic curve fitting
                                                        fitSellerTrdsGW <- fitEconCurves(regionGWAgVol, agElast, regionGWAgPr, spReg$GWUrbBefore, urbElast, spReg$gwAvePrUrbPay, curve="nl")
                                                        
                                                        ##fill the output objects with the economic modeling results
                                                        outMatGWTotVol[sellerInd,] <<- fitSellerTrdsGW$totTradeVol
                                                        outMatGWTotPct[sellerInd,] <<- fitSellerTrdsGW$pctTradeVol
                                                        outMatGWWelfareAg[sellerInd,] <<- fitSellerTrdsGW$welAgr
                                                        outMatGWWelfareUrb[sellerInd,] <<- fitSellerTrdsGW$welUrb
                                                        outMatGWAgrP[sellerInd,] <<- fitSellerTrdsGW$agrMarketP
                                                        outMatGWUrbP[sellerInd,] <<- fitSellerTrdsGW$urbMarketP
                                                        outMatGWsqUrb[sellerInd,] <<- fitSellerTrdsGW$squareUrb
                                                        outMatGWagrK[sellerInd,] <<- fitSellerTrdsGW$agrK
                                                        outMatGWurbK[sellerInd,] <<- fitSellerTrdsGW$urbK
                                                      }
    })
  }else if(trdType=="state"){
    sapply(spReg$basinName, function(sellerName){sellerInd <- which(spReg$basinName==sellerName)
                                                      regionSWAgVol <- rep(spReg$SWAgrBefore[sellerInd], nrow(spReg))
                                                      regionSWAgPr <- rep(spReg$swAvePrAgrPay[sellerInd], nrow(spReg))
                                                      regionGWAgVol <- rep(spReg$GWAgrBefore[sellerInd], nrow(spReg))
                                                      regionGWAgPr <- rep(spReg$gwAvePrAgrPay[sellerInd], nrow(spReg))
                                                      
    
                                                      ##preforms the economic curve fitting
                                                      fitSellerTrdsSW <- fitEconCurves(regionSWAgVol, agElast, regionSWAgPr, spReg$SWUrbBefore, urbElast, spReg$swAvePrUrbPay, curve="nl")
                                                      fitSellerTrdsGW <- fitEconCurves(regionGWAgVol, agElast, regionGWAgPr, spReg$GWUrbBefore, urbElast, spReg$gwAvePrUrbPay, curve="nl")
                                                      
                                                      ##fill the output objects with the economic modeling results
                                                      outMatSWTotVol[sellerInd,] <<- fitSellerTrdsSW$totTradeVol
                                                      outMatSWTotPct[sellerInd,] <<- fitSellerTrdsSW$pctTradeVol
                                                      outMatGWTotVol[sellerInd,] <<- fitSellerTrdsGW$totTradeVol
                                                      outMatGWTotPct[sellerInd,] <<- fitSellerTrdsGW$pctTradeVol
                                                      outMatGWWelfareAg[sellerInd,] <<- fitSellerTrdsGW$welAgr
                                                      outMatGWWelfareUrb[sellerInd,] <<- fitSellerTrdsGW$welUrb
                                                      outMatSWWelfareAg[sellerInd,] <<- fitSellerTrdsSW$welAgr
                                                      outMatGWAgrP[sellerInd,] <<- fitSellerTrdsGW$agrMarketP
                                                      outMatGWUrbP[sellerInd,] <<- fitSellerTrdsGW$urbMarketP
                                                      outMatSWAgrP[sellerInd,] <<- fitSellerTrdsSW$agrMarketP
                                                      outMatSWUrbP[sellerInd,] <<- fitSellerTrdsSW$urbMarketP
                                                      outMatGWsqUrb[sellerInd,] <<- fitSellerTrdsGW$squareUrb
                                                      outMatSWsqUrb[sellerInd,] <<- fitSellerTrdsSW$squareUrb
                                                      outMatSWagrK[sellerInd,] <<- fitSellerTrdsSW$agrK
                                                      outMatSWurbK[sellerInd,] <<- fitSellerTrdsSW$urbK
                                                      outMatGWagrK[sellerInd,] <<- fitSellerTrdsGW$agrK
                                                      outMatGWurbK[sellerInd,] <<- fitSellerTrdsGW$urbK
                                                      
    })
  }else{
    stop("Not a valid modeling type")
  }
  
  return(list(outMatSWTotVol, outMatSWTotPct, outMatGWTotVol, outMatGWTotPct, outMatSWWelfareAg, outMatSWWelfareUrb, outMatGWWelfareAg, outMatGWWelfareUrb,outMatGWAgrP, outMatGWUrbP, outMatSWAgrP, outMatSWUrbP, outMatGWsqUrb, outMatSWsqUrb, outMatSWagrK, outMatSWurbK, outMatGWagrK, outMatGWurbK ))
}
##########################################################################
##########################################################################
writeEconModelResults <- function(modResults, st, wDir){
  #################
  #modResults <- stEconModels
  #st <- stAbbr
  #wDir <- scenDir
  #################
  
  write.csv(modResults[[1]], paste0(wDir, st, "MarketTrades_SWTrdVol.csv"))
  write.csv(modResults[[2]], paste0(wDir, st, "MarketTrades_SWTrdPct.csv"))
  write.csv(modResults[[3]], paste0(wDir, st, "MarketTrades_GWTrdVol.csv"))
  write.csv(modResults[[4]], paste0(wDir, st, "MarketTrades_GWTrdPct.csv"))
  write.csv(modResults[[5]], paste0(wDir, st, "MarketTrades_SWWelfareAg.csv"))
  write.csv(modResults[[6]], paste0(wDir, st, "MarketTrades_SWWelfareUrb.csv"))
  write.csv(modResults[[7]], paste0(wDir, st, "MarketTrades_GWWelfareAg.csv"))
  write.csv(modResults[[8]], paste0(wDir, st, "MarketTrades_GWWelfareUrb.csv"))
  write.csv(modResults[[9]], paste0(wDir, st, "MarketTrades_GWAgrP.csv"))
  write.csv(modResults[[10]], paste0(wDir, st, "MarketTrades_GWUrbP.csv"))
  write.csv(modResults[[11]], paste0(wDir, st, "MarketTrades_SWAgrP.csv"))
  write.csv(modResults[[12]], paste0(wDir, st, "MarketTrades_SWUrbP.csv"))
  write.csv(modResults[[13]], paste0(wDir, st, "MarketTrades_GWUrbSquare.csv"))
  write.csv(modResults[[14]], paste0(wDir, st, "MarketTrades_SWUrbSquare.csv"))
  write.csv(modResults[[15]], paste0(wDir, st, "MarketTrades_SWagrK.csv"))
  write.csv(modResults[[16]], paste0(wDir, st, "MarketTrades_SWurbK.csv"))
  write.csv(modResults[[17]], paste0(wDir, st, "MarketTrades_GWagrK.csv"))
  write.csv(modResults[[18]], paste0(wDir, st, "MarketTrades_GWurbK.csv"))
  
}
##########################################################################
##########################################################################
createTrdRels <- function(fileNam){
  #################
  #fileNam <- findSWTrdRels[1]
  ################
  inTab <- read.csv(fileNam)
  subTab <- inTab[,c("BuyerRegion", "SellerRegion")]
  subTab <- subTab[which(is.na(subTab$BuyerRegion)==F & is.na(subTab$SellerRegion)==F),]
  subTab$val <- 1
  
  aggTab <- dcast(subTab, SellerRegion~BuyerRegion, sum)
  aggTab[aggTab==0] <- NA
  
  rownames(aggTab) <- aggTab$SellerRegion
  aggTab[,which("SellerRegion" %in% colnames(aggTab))] <- NULL
  
  return(aggTab)
}
##########################################################################
##########################################################################
createActualTrdVals <- function(fileNam, allRegNames){
  #################
  #fileNam <- findGWTrdRels[which(stAbbr=="wy")]
  #allRegNames <- getRegNames[[which(stAbbr=="wy")]]
  #################
  actTrdStruct <- matrix(data=NA, nrow=length(allRegNames), ncol=length(allRegNames))
  colnames(actTrdStruct) <- allRegNames
  rownames(actTrdStruct) <- allRegNames
  
  inTab <- read.csv(fileNam)
  subTab <- inTab[,c("BuyerRegion", "SellerRegion", "urbBuyFromAg")]
  subTab$urbBuyFromAg <- acreFtYr2ft3Sec(subTab$urbBuyFromAg)
  ##organize the trade data into matrix of actual trade volumes
  actualTotVol <- acast(subTab, SellerRegion~BuyerRegion, mean)
  actualTotVol[is.nan(actualTotVol)] <- NA
  
  #sellerName<-rownames(actualTotVol)[1]
  #runL <- l
  sapply(rownames(actualTotVol), function(sellerName){sellerTrdInd <- which(rownames(actualTotVol)==sellerName)
                                                    sellerInd <- which(allRegNames==sellerName)
  
                                                    ##identify the trade relationships that the seller has
                                                    sellerTrdRels <- rep(NA, length(allRegNames))
                                                    sellerFromTrdRels <- actualTotVol[sellerTrdInd,]
                                                    #sellerTrdRels[which(allRegNames %in% colnames(sellerFromTrdRels)[which(is.na(sellerFromTrdRels)==F)])] <- 1
                                                    if(nrow(actualTotVol)==1 & ncol(actualTotVol)==1){
                                                      buyerInds <- which(allRegNames %in% colnames(actualTotVol))
                                                    }else{
                                                      buyerInds <- which(allRegNames %in% names(sellerFromTrdRels)[which(is.na(sellerFromTrdRels)==F)])
                                                    }
                                                    sellerTrdRels[buyerInds] <- sellerFromTrdRels[which(is.na(sellerFromTrdRels)==F)]
                                                    
                                                    #return(sellerTrdRels)
                                                    actTrdStruct[sellerInd,] <<- sellerTrdRels
                                                  })
  return(actTrdStruct)
}
##########################################################################
##########################################################################

##########################################################################
##########################################################################
#gainRamp- lighter to darker
#lossRamp- darker to lighter
plotEconModels <- function(outModVolumes, typeTrans, includeNegs=F, spatialPolys, gainRamp=c("#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061"), zeroRamp="#f7f7f7", lossRamp=c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7"), 
                           plotBrks, plotNetBrks, legendTitle, legendLoc, legBoxOrient, legendDir="vertical", titleSize=52, legendTxt=48, legTitleSize=40, fileName, 
                           fileWidth=1984, fileHeight=3850, plotTitles){
  #################
  #outModVolumes <- stEconModels[[which(stAbbr=="wa")]][[3]]
  #outModVolumes <- swHistTrdVals[[which(stAbbr=="ca")]]
  #typeTrans <- "Groundwater"
  #includeNegs <- F
  #spatialPolys <- stateRegSpData[[which(stAbbr=="ca")]]
  #gainRamp <- c("#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
  #zeroRamp <- "#f7f7f7"
  #lossRamp <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3")
  #plotBrks <- c(0, 13, 36, 64, 94, 294, 366)
  #plotNetBrks <- c(-366, -294, -94, -64, -36, -13, 0, 13, 36, 64, 94, 294, 366)
  #legendTitle <- "Volume (CFS)"
  #legendTitle <- "Percent (%)"
  #legendLoc <- "right"
  #legBoxOrient <- "vertical"
  #legendDir <- "vertical"
  #titleSize <- 44
  #legendTxt <- 30
  #legTitleSize <- 32
  #fileName <- paste0(scenDir, "caSurMarketActualTrades_Volume_2.png")
  #fileWidth <- 1984
  #fileHeight <- 3850
  #plotTitles <- tallTitles52
  #################
  
  if(includeNegs==F){
    outModVolumes[outModVolumes<0] <- 0
  }
  #themeRamp <- colorRampPalette(themeRamp)
  gainRamp <- colorRampPalette(gainRamp)
  zeroRamp <- colorRampPalette(zeroRamp)
  lossRamp <- colorRampPalette(lossRamp)
  
  ##summarize the market analysis output
  trdsInBasin <- sapply(1:nrow(outModVolumes), function(ind){val<-outModVolumes[ind,ind];outModVolumes[ind,ind]<<-NA;return(val)})
  soldVol <- rowSums(outModVolumes, na.rm=T)
  buyVol <- colSums(outModVolumes, na.rm=T)
  
  marketData <- data.frame(basinName=names(soldVol), 
                           transAsBuyerVol=buyVol, transAsSellerVol=soldVol, transWithinSelfVol=trdsInBasin)
  marketData[marketData==0] <- NA
  ##calculate the net gain or loss of water
  buyerVol <- marketData$transAsBuyerVol
  buyerNA <- which(is.na(buyerVol)==T)
  buyerVol[buyerNA] <- 0
  sellerVol <- marketData$transAsSellerVol
  sellerNA <- which(is.na(sellerVol)==T)
  sellerVol[sellerNA] <- 0
  marketData$netWaterGain <- buyerVol - sellerVol
  marketData$netWaterGain[sellerNA[match(buyerNA,sellerNA)]] <- NA
  #write.csv(marketData, paste0(scenDir, "caSWMarketModel_plotValues_Volume.csv"), row.names=F)
  
  ##merging the spatial and the market data together
  spatialMarket <- merge(x=spatialPolys, y=marketData, by="basinName")
  
  #sur <- classIntervals(c(spatialMarket$transAsBuyerVol, spatialMarket$transAsSellerVol, spatialMarket$transWithinSelfVol), style="jenks")
  #surN <- classIntervals(abs(c(spatialMarket$netWaterGain)), style="jenks")
  #classIntervals(c(spatialMarket$transAsBuyerVol, spatialMarket$transAsSellerVol, spatialMarket$transWithinSelfVol, abs(spatialMarket$netWaterGain)), style="jenks")
  
  
  ##preform the classification for the plot
  buyBrksLabs <- sapply(1:(length(plotBrks)-1), function(idx){paste0(round(plotBrks[idx],1), " - ", round(plotBrks[idx+1],1))})
  sellBrks <- rev(plotBrks) * -1
  sellBrksLabs <- sapply(1:(length(sellBrks)-1), function(idx){paste0(round(sellBrks[idx],1), " - ", round(sellBrks[idx+1],1))})
  netLabs <- sapply(1:(length(plotNetBrks)-1), function(idx){paste0(round(plotNetBrks[idx],1), " - ", round(plotNetBrks[idx+1],1))})
  spatialMarket$buyerBrks <- cut(spatialMarket$transAsBuyerVol, breaks=plotBrks, include.lowest=T, labels=buyBrksLabs)
  #spatialMarket$sellerBrks <- cut(spatialMarket$transAsSellerVol, breaks=plotBrks, include.lowest=T, labels=buyBrksLabs)
  spatialMarket$sellerBrks <- cut((spatialMarket$transAsSellerVol*-1), breaks=sellBrks, include.lowest=T, labels=sellBrksLabs)
  spatialMarket$selfBrks <- cut(spatialMarket$transWithinSelfVol, breaks=plotBrks, include.lowest=T, labels=buyBrksLabs)
  spatialMarket$netBrks <- cut(spatialMarket$netWaterGain, breaks=plotNetBrks, include.lowest=T, labels=netLabs)
  
  ################################
  
  brkScale <- levels(spatialMarket$buyerBrks)
  ##transform from spatial polygons data frame, to a tipple
  sweepSpatial <- broom::tidy(spatialMarket)
  ##re-adding the color catagories
  data2Map <- data.frame(id=row.names(spatialMarket), mapData=spatialMarket$buyerBrks)
  ggPlotReadyPolys <- plyr::join(sweepSpatial, data2Map, by="id")
  ##make plot
  buyerMap <- ggplot(ggPlotReadyPolys) + aes(long,lat,group=group) + geom_polygon(aes(fill=mapData), color="#969696") + theme_tufte() +
      scale_fill_manual(name=legendTitle, values=rev(gainRamp(length(plotBrks)-1)), breaks=rev(brkScale), labels=rev(brkScale), drop=F, na.value="grey50") +
      theme(legend.position=legendLoc[[1]], legend.title=element_text(size=legTitleSize), legend.text=element_text(size=legendTxt), title=element_text(size=titleSize,face="bold"), legend.direction=legendDir, axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())+
      ggtitle(paste0(typeTrans, plotTitles[1]))
  
  brkScale <- levels(spatialMarket$sellerBrks)
  ##transform from spatial polygons data frame, to a tipple
  sweepSpatial <- broom::tidy(spatialMarket)
  ##re-adding the color catagories
  data2Map <- data.frame(id=row.names(spatialMarket), mapData=spatialMarket$sellerBrks)
  ggPlotReadyPolys <- plyr::join(sweepSpatial, data2Map, by="id")
  ##make plot
  sellerMap <- ggplot(ggPlotReadyPolys) + aes(long,lat,group=group) + geom_polygon(aes(fill=mapData), color="#969696") + theme_tufte() +
      scale_fill_manual(name=legendTitle, values=rev(lossRamp(length(sellBrks)-1)), breaks=rev(brkScale), labels=rev(brkScale), drop=F, na.value="grey50") +
      theme(legend.position=legendLoc[[1]], legend.title=element_text(size=legTitleSize), legend.text=element_text(size=legendTxt), title=element_text(size=titleSize,face="bold"), legend.direction=legendDir, axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())+
      ggtitle(paste0(typeTrans, plotTitles[2]))
  
  brkScale <- levels(spatialMarket$selfBrks)
  ##transform from spatial polygons data frame, to a tipple
  sweepSpatial <- broom::tidy(spatialMarket)
  ##re-adding the color catagories
  data2Map <- data.frame(id=row.names(spatialMarket), mapData=spatialMarket$selfBrks)
  ggPlotReadyPolys <- plyr::join(sweepSpatial, data2Map, by="id")
  ##make plot
  selfMap <- ggplot(ggPlotReadyPolys) + aes(long,lat,group=group) + geom_polygon(aes(fill=mapData), color="#969696") + theme_tufte() +
      scale_fill_manual(name=legendTitle, values=rev(gainRamp(length(plotBrks)-1)), breaks=rev(brkScale), labels=rev(brkScale), drop=F, na.value="grey50") +
      theme(legend.position=legendLoc[[1]], legend.title=element_text(size=legTitleSize), legend.text=element_text(size=legendTxt), title=element_text(size=titleSize,face="bold"), legend.direction=legendDir, axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())+
      ggtitle(paste0(typeTrans, plotTitles[3]))
  
  brkScale <- levels(spatialMarket$netBrks)
  ##transform from spatial polygons data frame, to a tipple
  sweepSpatial <- broom::tidy(spatialMarket)
  ##re-adding the color catagories
  data2Map <- data.frame(id=row.names(spatialMarket), mapData=spatialMarket$netBrks)
  ggPlotReadyPolys <- plyr::join(sweepSpatial, data2Map, by="id")
  ##make plot
  #splitPt <- which(netLabs=="0 - 0") 
  splitPt <- grep(" - 0", netLabs)
  netMap <- ggplot(ggPlotReadyPolys) + aes(long,lat,group=group) + geom_polygon(aes(fill=mapData), color="#969696") + theme_tufte() +
      #scale_fill_manual(name=legendTitle, values=c(rev(gainRamp(splitPt-1)), zeroRamp(1), rev(lossRamp(splitPt-1))), breaks=rev(brkScale), labels=rev(brkScale), drop=F, na.value="grey50") +
      scale_fill_manual(name=legendTitle, values=c(rev(gainRamp(splitPt)), rev(lossRamp(splitPt))), breaks=rev(brkScale), labels=rev(brkScale), drop=F, na.value="grey50") +  
      theme(legend.position=legendLoc[[1]], legend.title=element_text(size=legTitleSize), legend.text=element_text(size=legendTxt), title=element_text(size=titleSize,face="bold"), legend.direction=legendDir, axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank())+
      ggtitle(paste0(typeTrans, plotTitles[4]))
  
  
  ##write out the maps
  png(fileName, width=fileWidth, height=fileHeight)
  print(multiplot(buyerMap, sellerMap, 
                  netMap, selfMap, 
                  layout=matrix(c(1,2,
                                  3,4),ncol=2, byrow=T)))
  dev.off()
}
##########################################################################
##########################################################################

##########################################################################
##########################################################################









