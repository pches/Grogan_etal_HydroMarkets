##########################################################################
##########################################################################
## Script Name: hydroRealism_marketModel_CustomFunctions.R
## Purpose of Script: A file which holds the custom function written 
## specifically to assist with preforming the market model portion of the
## Hydro Realism analysis.
##
## Special Requirements: None - this is meant to be called by another scripts.
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 11/12/2019
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 1/24/2024
##
## Copyright (c) 2024 The Pennsylvania State University
##
##########################################################################
##########################################################################
##custom function for PCHES Hydro Realism Market Model
##########################################################################
##########################################################################
##A function to convert acre feet per year to cubic feet per second
acreFtYr2ft3Sec <- function(val){
  #################
  #val <- 1
  #################
  ft3Sec <- val * 0.00138036
  return(ft3Sec)
}
##########################################################################
##########################################################################
##A function to calculate the area of spatial polygons based on a given 
##projection
calcArea <- function(polys, areaProj, areaVarName="area"){
  #################
  #polys <- stWMAS
  #areaProj <- areaProj
  #areaVarName <- "area"
  #################
  
  spTransPolys <- spTransform(polys, areaProj)
  polys[[areaVarName]] <- area(spTransPolys) / 1000000  ##assumes the areaProj produces sq m, transforms into km
  
  return(polys)
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
##This function is the point expansion equation, and therefor the heart of 
##the market model analysis. The input variable names come are related 
##to the variables found in figure S5 in the paper. This function is used
##to find the maximally beneficial state between water right quantity and
##price. 
##
##It is worth nothing that while the variable names assume agriculture
##and urban economic sectors, values can be of any sector.
evalFunction <- function(Q, agrQnt, agrK, agrE, urbQnt, urbK, urbE){
  #################
  #agrQnt <- agrQuant
  #agrK <- agrK
  #agrE <- agrElast
  #urbQnt <- urbQuant
  #urbK <- urbK
  #urbE <- urbElast
  #Q=lower
  #agrQnt=agrQuant[x]
  #agrK=agrK[x]
  #agrE=agrElast
  #urbQnt=urbQuant[x]
  #urbK=urbK[x]
  #urbK=0
  #urbK=9
  #urbE=urbElast
  #Q=c(urbQuant[x],urbQuant[x]+agrQuant[x])
  #urbQnt=0
  #################
  
  findQ <- ((1/urbK) * Q)^(1/urbE) - ((1/agrK) * (agrQnt + urbQnt - Q))^(1/agrE)
  return(findQ)
}
##########################################################################
##########################################################################
##A function to manage the fitting of the market model. For the Hydro 
##Realism paper, only results from running this function with a non-linear
##curve were used.
fitEconCurves <- function(agrQuant, agrElast, agrPrice, urbQuant, urbElast, urbPrice, curve="li"){
  #################
  #agrQuant <- regionSWAgVol ##agriculture quantity
  #agrPrice <- waterRightRecsIndust$sw_agrLeasePrice#[6]  ##agriculture price, seller
  #urbQuant <- spReg$SWUrbBefore ##urban quantity
  #urbPrice <- waterRightRecsIndust$sw_urbLeasePrice#[6]  ##urban price, buyer
  #agrElast <- agElast ##agriculture elasticity
  #urbElast <- urbElast  ##urban elasticity
  #curve <- "nl"  ##li for linear, nl for non-linear
  #################
  ##If the user selects "li", run the market model using linear curves
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
  ##if the user selects "nl", run the market model using non-linear curves
  ##This uses the `evalFunction` function to fit the market model.
  }else if(curve=="nl"){
    agrK <- agrQuant / (agrPrice^agrElast)
    urbK <- urbQuant / (urbPrice^urbElast)
    
    urbMarketVal <- sapply(1:length(urbQuant), function(x){#print(x)
                                                          if(agrQuant[x]==0 | is.na(agrQuant[x])==T | is.na(urbQuant[x])==T | is.na(agrK[x])==T | is.na(urbK[x])==T){
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
    
    perDiffAgr <- (agrMarketVal / agrQuant) - 1
    perDiffUrb <- (urbMarketVal / urbQuant) - 1
    
    ##determine total trade volume 
    totTradeVol <- urbMarketVal-urbQuant
    ##determine the percentage of traded volume
    pctTradeVol <- 100 * (totTradeVol / (agrQuant+urbQuant))
  }
  ##organize all output as a data frame
  curveFitted <- data.frame(AgrAfter=agrMarketVal, UrbAfter=urbMarketVal, perDiffAgr=perDiffAgr, perDiffUrb=perDiffUrb, totTradeVol=totTradeVol, pctTradeVol=pctTradeVol)
  return(curveFitted)
}
##########################################################################
##########################################################################
##This function takes the cumulative curves of the various WMAs and 
##calculates a cumulative curve for the trade regions. This is done by 
##first identifying the amount of overlap between WMAs and trading regions
##by area and also percent area. Then, cycling though each trade region,
##the relevant WMAs are identified and their data extracted by percent
##area and summed into the cumulative curve for the trade region. 
##
##To facilitate understanding, two examples will be presented. In the first 
##example, let it be assumed that trade region A (trA) is comprised
##of two WMAs, WMA1 and WMA2. For year X, there are 10 CFS of water rights
##in WMA1 for agriculture and 5 CSF of water rights for WMA2. WMA1 
##comprises 20% of trA, and WMA2 fills out the remaining 80%. So, the 
##trA's water rights for year X would be: (10*.2) + (5*.8) = 6 CFS. 
##
##For the second example, let us reuse trA and WMA1 from the previous 
##example. Let it be assumed that in looking at WMA1 closer, it is 
##found that it is actually overlaps three trade regions: trA from the
##previous example as well as trade regions B and C (trB and trC, 
##respectively). It is already known that WMA1 "supplies" 2 of its 10
##CSF to trA. The percent area overlap with the other trade regions are
##70% with trB and 10% with trC. This means that 7 CFS would go to trB
##and 1 CFS to trC. 
##
##In short, a trade region can be comprised of multiple WMAs and a single
##WMA my "supply" multiple trade regions. The percent area overlap between
##trading regions and WMAs needed to be kept track of, as it is the same 
##ratio which is used to modify the WMA cumulative curves after the 
##market model analysis
convertWMAVolToTrdRegions <- function(st, stPrices, stateHydroReg, surRights, grdRights, wmaLayer, areaProj, whichYear="last", cutOffYrs=NULL){
  #################
  #n <- 2
  #st <- stateNames[n]
  #stPrices <- statePricesByReg[[n]]
  #stateHydroReg <- stateRegSpData[[n]]
  #surRights <- wmasRightsTotsSur
  #grdRights <- wmasRightsTotsGrd
  #whichYear <- "cutoff"  ##options: last, cutoff
  #wmaLayer <- wmasData
  #areaProj <- projForAreaCalc
  #cutOffYrs <- datesWithIDs #<- NULL
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
    
  ##begin filling out the percentage of WMA in each WWR
  sapply(stateHydroReg$basinName, function(bnam){whichInd<-which(stateHydroReg$basinName==bnam);
                                               selectBasin<-stateHydroReg[whichInd,];
                                               ##crop the WMAs
                                               croppedWMAs<-raster::crop(stWMAArea,selectBasin);
                                               croppedAreas<-calcArea(croppedWMAs, areaProj, "area");
                                               croppedWMAs$cropArea<-croppedAreas$area;
                                               ##percent area of the WMA within the region
                                               croppedWMAs$perArea<-croppedWMAs$cropArea/croppedWMAs$area;
                                               ##adjust unrealistic percents, comes from rounding errors when calculating area
                                               croppedWMAs$perArea[which(croppedWMAs$perArea > 1)] <- 1
                                               ##add percent area to the holdWMAPer table
                                               holdWMAPer[sapply(croppedWMAs$basinNum, function(wmaInd){which(rownames(holdWMAPer) %in% wmaInd)}),whichInd] <<- croppedWMAs$perArea})

  
  chkPerTots <- rowSums(holdWMAPer, na.rm=T)
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
  greaterThan1WMAs <- which(chkPerTots>1)
  if(length(greaterThan1WMAs)==1){
    greaterThan1Maxes <- as.numeric(which.max(holdWMAPer[greaterThan1WMAs,]))
  }else{
    greaterThan1Maxes <- as.numeric(apply(holdWMAPer[greaterThan1WMAs,], 1, which.max))
  }
  
  ##if greater than 1, subtract from the WWR with the greatest area until equals 1
  mapply(function(nr,nc){holdWMAPer[nr,nc]<<-holdWMAPer[nr,nc]-(sum(holdWMAPer[nr,],na.rm=T)-1)}, nr=greaterThan1WMAs, nc=greaterThan1Maxes)
  

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
  
  ##waterBeforeByReg = the object which stores the amount of water available to each trading region pre-market model
  ##holdWMAPer = the object which stores the percent area overlap between trading regions and WMAs.
  return(list(waterBeforeByReg,holdWMAPer))
}
##########################################################################
##########################################################################
##Modifies the original WMA cumulative curves based on the amount of ag 
##right lost and urban rights gained from the market modeling preformed
##on the trading regions. This is essentially the reverse of 
##`convertWMAVolToTrdRegions`, and uses the percent area overlap values
##created in that function. 
##
##It should be noted that while this function does return the modified
##WMA cumulative curves, it also writes them to disk once they have
##been calculated. This is due to the amount of time this step can take
##to run and the possibility of the process being interrupted. 
applyTrdRegionsToWMAs <- function(st, modeledEconVals, spReg, wmaInReg, typeApplicaiton, surRights, grdRights, wmaLayer, areaProj, includeNegs=F, tableWriteDir, nameConvTab, surYrCutTab=NULL){
  #################
  #n <- 2
  #st <- stateNames[n]
  #modeledEconVals <- stEconModels[[n]]
  #spReg <- stateRegVol[[n]]
  #wmaInReg <- stateRegWMAPer[[n]]
  #typeApplicaiton <- c("earlyPrioDates", "waterRunsOutCutoffs_WatInst")  ##curveShift, curveShift0, earlyPrioDates
  #surRights <- wmasRightsTotsSur
  #grdRights <- wmasRightsTotsGrd
  #wmaLayer <- wmasData
  #areaProj <- projForAreaCalc
  #includeNegs <- F
  #tableWriteDir <- paste0(cumuDir, "sectorCumuSummaries")
  #nameConvTab <- namConvData
  #surYrCutTab <- wmaCutOffDates
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
    tableWriteDirSW <- paste0(tableWriteDir, "/", st, "/surfaceWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_trdOldRights/")
    tableWriteDirGW <- paste0(tableWriteDir, "/", st, "/groundWater/percentages/marketModified_stps_nonL_wInd/wwrPrices_trdOldRights/")
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
    sapply(swWhichNeg, function(negWWR){swVol[negWWR,] <<- swVol[negWWR,] - ((swCheckAgSold[swWhichNeg] / swSoldVol[swWhichNeg]) * swVol[negWWR,])
                                        })
    swSoldVol <- rowSums(swVol, na.rm=T)
    swBuyVol <- colSums(swVol, na.rm=T)
  }
  if(length(gwWhichNeg)>0){
    ##modify the amount of water rights bough by urban to reflect that too many rights were purchased from ag
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
                                            if(surRightsRow>nrow(wmaWaterRights)){
                                              
                                            }else if(wmaFirstUrbanYr>surRightsRow){
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
                                            if(grdRightsRow>nrow(wmaWaterRights)){
                                              
                                            }else if(wmaFirstUrbanYr>grdRightsRow){
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

  
  ##write out the results
  lapply(modWRs, function(lst){write.csv(lst[[1]], paste0(tableWriteDirSW, "WMA_", names(lst[1]), "_SW.csv"), row.names=F)
                              write.csv(lst[[2]], paste0(tableWriteDirGW, "WMA_", names(lst[2]), "_GW.csv"), row.names=F)})
  
  return(modWRs)
}
##########################################################################
##########################################################################
##Another function to facilitate the market modeling process. While 
##`evalFunction` is the market model equation and `fitEconCurves` manages
##the setup of the market model between two trade regions, this function
##manages the identification of which trade regions are allowed to be 
##modeled. For the trading type (trdType) "state", this function assumes
##a hypothetical in which each trading region within a state are allowed
##to trade with each other, irrespective of any other consideration. 
##
##The "historical" trdType adds several layers of restriction on which
##trading regions may trade with each other, within a given state. The 
##first restriction is whether or not there is a historical basis for the
##trading relationship. For the Hydro Realism paper, we used the trading
##relationships found from within the Waterlitix dataset to define what
##was historical. However, any set of relationships, realistic or 
##hypothetical could be input. The second restriction is that trades could
##only occur in one direction. This was cause by a select few cases where
##it was found that the ag sector of one WMA was selling to the urban 
##sector of another, and also the ag sector of the second WMA was selling
##to the urban sector of the first. In these cases, we only kept the 
##trade which had the greater volume, but subtracted the lesser volume
##from the available amount of water. 
econModelStates <- function(spReg, trdType, agElast, urbElast, swTrdRels=NULL, gwTrdRels=NULL){
  #################
  #n <- 2
  #spReg <- stateRegVol[[n]] ##volume of available water for each trading region within the state
  #trdType <- "historical"  ##"state" or "historical"
  #agElast <- agrElast[n] ##state level agriculture elasticity
  #urbElast <- urbElast[n]  ##state level urban elasticity
  #swTrdRels <- swHistTrdRelations[[n]]  ##optionally provided surface water trading relationships
  #gwTrdRels <- gwHistTrdRelations[[n]]  ##optionally provided groundwater trading relationships
  #################
  #print(spReg$basinName)
  ##set up output objects
  outMatSWTotVol <- matrix(data=NA, nrow=length(spReg$basinName), ncol=length(spReg$basinName))
  colnames(outMatSWTotVol) <- spReg$basinName
  rownames(outMatSWTotVol) <- spReg$basinName
  outMatSWTotPct <- outMatSWTotVol
  outMatGWTotVol <- outMatSWTotVol
  outMatGWTotPct <- outMatSWTotVol
  ##run the economic curve fitting models for surface water1
  if(trdType=="historical"){
    sapply(unique(c(swTrdRels$SellerRegion,gwTrdRels$SellerRegion)), function(sellerName){sellerInd <- which(spReg$basinName==sellerName)
                                                      ##identify the trade relationships that the seller has historically had
                                                      ##from all trades, isolate those where water was specifically being traded to trade partner's urban sectors
                                                      #print(sellerName)
                                                      swHistTrds <- swTrdRels[swTrdRels$SellerRegion==sellerName & swTrdRels$agSoldToUrb>0,]
                                                      gwHistTrds <- gwTrdRels[gwTrdRels$SellerRegion==sellerName & gwTrdRels$agSoldToUrb>0,]
                                                      ##ID the trade partners that are not self
                                                      swHistTrdPart <- swHistTrds$BuyerRegion
                                                      gwHistTrdPart <- gwHistTrds$BuyerRegion

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
                                                        
                                                        fitSellerTrdsSW <- fitEconCurves(regionSWAgVol, agElast, regionSWAgPr, spReg$SWUrbBefore, urbElast, spReg$swAvePrUrbPay, curve="nl")

                                                        ##fill the output objects with the economic modeling results
                                                        outMatSWTotVol[sellerInd,] <<- fitSellerTrdsSW$totTradeVol
                                                        outMatSWTotPct[sellerInd,] <<- fitSellerTrdsSW$pctTradeVol
                                                      }
                                                      if(length(gwBuyersInd)>0 & length(sellerInd)>0){
                                                        regionGWAgVol[gwBuyersInd] <- spReg$GWAgrBefore[sellerInd]
                                                        regionGWAgPr[gwBuyersInd] <- spReg$gwAvePrAgrPay[sellerInd]

                                                        ##preforms the economic curve fitting
                                                        fitSellerTrdsGW <- fitEconCurves(regionGWAgVol, agElast, regionGWAgPr, spReg$GWUrbBefore, urbElast, spReg$gwAvePrUrbPay, curve="nl")

                                                        ##fill the output objects with the economic modeling results
                                                        outMatGWTotVol[sellerInd,] <<- fitSellerTrdsGW$totTradeVol
                                                        outMatGWTotPct[sellerInd,] <<- fitSellerTrdsGW$pctTradeVol
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
    })
  }else{
    stop("Not a valid modeling type")
  }

  return(list(outMatSWTotVol, outMatSWTotPct, outMatGWTotVol, outMatGWTotPct))
}
##########################################################################
##########################################################################
##A function to write out the results of the market model analysis at the
##trade region level. This is done mostly to have a text version of the
##trade region cumulative curves for easy of access, and in case the 
##script is interrupted. This also allows for the four files to be
##written for one line of code in the main script.
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
}
##########################################################################
##########################################################################


