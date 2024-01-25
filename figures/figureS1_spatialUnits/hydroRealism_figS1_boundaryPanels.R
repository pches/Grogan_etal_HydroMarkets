##########################################################################
##########################################################################
## Script Name: hydroRealism_figS1_boundaryPanels.R
## Purpose of Script: This script creates the S1 figure in the Hydro 
## Realism paper. The figure is a small collection of maps within three 
## panels, labeled A, B, and C. Panel A is an establishing panel, showing
## the 11 Western States whose water rights were used within the study. 
## Panel B shows the WMAs used for the study, and Panel C shows the the
## trade region boundaries. The main purpose of this figure is to 
## succinctly show the the geographic differences between the WMAs and the
## trade regions. 
##
## Special Requirements: A series of spatial boundary polygons. More 
## specifically the boundaries of the continental 48 United States, the 
## boundaries of the WMAs, and the boundaries of the trade regions defined
## by Waterlitix. 
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 4/5/2021
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 1/25/2024
##
## Copyright (c) 2024 The Pennsylvania State University
##
##########################################################################
##########################################################################
options(stringsAsFactors=F, scipen=999)

##local machine
#docDir <- "/Users/mdl5548/Documents/"
#gdrBase <- "/Users/mdl5548/Library/CloudStorage/GoogleDrive-mdl5548@psu.edu/Shared drives/PCHES_Project1.2/Water rights project/Water institutions/Data/"
#projGdrDir <- paste0(gdrBase, "waterRightsCumulations/")
#stateDataDir <- paste0(projGdrDir, "inputData/")
##RC machine
docDir <- "/storage/group/pches/default/users/mdl5548/"
projGdrDir <- "/dataDir/"
stateDataDir <- paste0(projGdrDir, "inputData/")

##read in libraries
.libPaths("/Rlib")
library(rgeos)
library(rgdal)
library(maptools)
library(classInt)
library(ggplot2)
library(ggfortify)
library(ggthemes)
library(ggsn) 
library(ggpattern)

##Projection, in other scripts used to calculate shape areas
##For this script, used for panel A, for a most estetically 
##pleasing figure
projForAreaCalc <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

##########################################################################
##########################################################################
##reading in the boundary datasets from their various sources
##WMA boundaries
wmasLayer <- readOGR(dsn=paste0(stateDataDir, "allStateWMAs/"), layer="simpleWMAs")

##US county boundaries - as some trade regions are county boundaries
usCountiesLayer <- readOGR(dsn=paste0(docDir, "backgroundGISData/countyBoundaryRef/cb_2018_us_county_500k"), layer="cb_2018_us_county_500k")
usCountiesLayer <- spTransform(usCountiesLayer, wmasLayer@proj4string@projargs)

##US state boundaries, for reference
usStatesLayer <- readOGR(dsn=paste0(docDir, "backgroundGISData/stateBoundaryRef/cb_2018_us_state_500k"), layer="cb_2018_us_state_500k")
usStatesLayer <- spTransform(usStatesLayer, wmasLayer@proj4string@projargs)
subStates <- usStatesLayer[usStatesLayer$STUSPS=="CA" | usStatesLayer$STUSPS=="OR" | usStatesLayer$STUSPS=="WA" | usStatesLayer$STUSPS=="ID" | usStatesLayer$STUSPS=="MT" |
                             usStatesLayer$STUSPS=="CO" | usStatesLayer$STUSPS=="AZ" | usStatesLayer$STUSPS=="NV" | usStatesLayer$STUSPS=="UT" | usStatesLayer$STUSPS=="WY" |
                             usStatesLayer$STUSPS=="NM",]

##read in and organize the various sources that comprise the boundaries 
##used for the trade regions
##CA
caliHydroReg <- readOGR(dsn=paste0(stateDataDir, "california/Hydrologic_Regions (mdl5548@psu.edu)/"), layer="Hydrologic_Regions")
##dissolve features by basin name
caliFields <- caliHydroReg@data[,c("HR_NAME")]
dissolveCali <- unionSpatialPolygons(caliHydroReg, caliHydroReg$HR_NAME)
##add attribute table to the dissolved features
pid <- sapply(slot(dissolveCali,"polygons"),function(x){slot(x,"ID")})
matchBasinID <- match(pid, caliFields)
disCaliDF <- data.frame(basinName=pid, state="California", row.names=pid)
caliHydroReg <- SpatialPolygonsDataFrame(dissolveCali, data=disCaliDF)
##CO
coloHydroReg <- wmasLayer[wmasLayer$state=="Colorado",]
coloHydroReg$basinName[grep(" - ", coloHydroReg$basinName)] <- sapply(strsplit(coloHydroReg$basinName[grep(" - ", coloHydroReg$basinName)], " - "), "[[", 2)
##WA
washHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="53",]
washHydroReg$basinName <- washHydroReg$NAME
##OR
oregHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="41",]
oregHydroReg$basinName <- toupper(oregHydroReg$NAME)
##ID
idahoHydroReg <- wmasLayer[wmasLayer$state=="Idaho",]
idahoHydroReg$basinName <- idahoHydroReg$basinNum
##MT
montHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="30",]
montHydroReg$basinName <- montHydroReg$NAME
##NV
nevadHydroReg <- wmasLayer[wmasLayer$state=="Nevada",]
nevadHydroReg$basinName <- nevadHydroReg$basinNum
##UT
utahHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="49",]
utahHydroReg$basinName <- toupper(utahHydroReg$NAME)
##WY
wyomHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="56",]
wyomHydroReg$basinName <- wyomHydroReg$NAME
##AZ
azGrdWMAs <- readOGR(dsn=paste0(stateDataDir, "allStateWMAs/"), layer="azGroundWMAs")
boundsToKeep <- c("SANTA CRUZ AMA", "JOSEPH CITY INA", "PRESCOTT AMA", "PHOENIX AMA", "HARQUAHALA INA", "PINAL AMA", "TUCSON AMA", "DOUGLAS INA")
azGrdWMAs$basinName[-c(which(azGrdWMAs$basinName %in% boundsToKeep))] <- "OUTSIDE AMA / INA"
dissAZ <- unionSpatialPolygons(azGrdWMAs, azGrdWMAs$basinName)
pid <- sapply(slot(dissAZ,"polygons"),function(x){slot(x,"ID")})
dissAZDF <- data.frame(Name=pid, row.names=pid)
arizHydroReg <- SpatialPolygonsDataFrame(dissAZ, data=dissAZDF)
names(arizHydroReg) <- "basinName"
##NM
nemexHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="35",]
nemexHydroReg$basinName <- nemexHydroReg$NAME
nemexHydroReg$basinName[grep("ñ", nemexHydroReg$basinName)] <- "Dona Ana"

##reorganize the trading regions into a single layer
tradeRegions <- rbind(arizHydroReg, caliHydroReg[,"basinName"], coloHydroReg[,"basinName"], idahoHydroReg[,"basinName"], montHydroReg[,"basinName"],
                      nemexHydroReg[,"basinName"], nevadHydroReg[,"basinName"], oregHydroReg[,"basinName"], utahHydroReg[,"basinName"],
                      washHydroReg[,"basinName"], wyomHydroReg[,"basinName"])

##reproject the US state layer
projUSStatesLayer <- spTransform(usStatesLayer, projForAreaCalc)
projUSStatesLayer <- projUSStatesLayer[-c(which(projUSStatesLayer$STUSPS %in% c("VI", "AS", "GU", "MP", "AK", "PR", "HI"))),]
projSubStates <- projUSStatesLayer[projUSStatesLayer$STUSPS=="CA" | projUSStatesLayer$STUSPS=="OR" | projUSStatesLayer$STUSPS=="WA" | projUSStatesLayer$STUSPS=="ID" | projUSStatesLayer$STUSPS=="MT" |
                                     projUSStatesLayer$STUSPS=="CO" | projUSStatesLayer$STUSPS=="AZ" | projUSStatesLayer$STUSPS=="NV" | projUSStatesLayer$STUSPS=="UT" | projUSStatesLayer$STUSPS=="WY" |
                                     projUSStatesLayer$STUSPS=="NM",]

dataFrame <- data.frame(lat=184363, lon=-684440, text="United/nStates")
spdf <- SpatialPointsDataFrame(coords=cbind(dataFrame$lat, dataFrame$lon), data=dataFrame, 
                               proj4string=CRS(projForAreaCalc))

##########################################################################
##First option, did not end up being used
# png("/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/borderMaps_opt1.png", width=1500, height=850)
# par(mfrow=c(1, 2), mar=c(1, 1, 1, 1))
# plot(wmasLayer)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2)
# plot(wmasLayer, add=T, col="white", border="#a6a6a6")
# plot(subStates, add=T, col="#00000000", border="black", lwd=2)
# 
# plot(tradeRegions)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2)
# plot(tradeRegions, add=T, col="white", border="#a6a6a6")
# plot(subStates, add=T, col="#00000000", border="black", lwd=2)
# dev.off()
##########################################################################
##Second option, did not end up being used
# png("/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/borderMaps_opt2.png", width=1500, height=850)
# par(mfrow=c(1, 2), mar=c(1, 1, 1, 1))
# plot(wmasLayer)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2)
# plot(wmasLayer, add=T, col="white", border="#a6a6a6")
# plot(subStates, add=T, col="#00000000", border="black", lwd=2)
# polygonsLabel(subStates, subStates$NAME, method=c("centroid", "inpolygon"), polypart="largest", cex=2.5)
# 
# plot(tradeRegions)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2)
# plot(tradeRegions, add=T, col="white", border="#a6a6a6")
# plot(subStates, add=T, col="#00000000", border="black", lwd=2)
# dev.off()
##########################################################################
##Version of the file used in paper as Figure S1
png("/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/borderMaps_opt3b.png", width=1500, height=1400)
layout(matrix(c(1,1,2,3), byrow=T, ncol=2))
par(mar=c(1, 1, 1, 1))
plot(projUSStatesLayer, col="#c4c4c4", border="#eaeaea", lwd=2)
plot(projSubStates, add=T, col="white", border="black", lwd=2)
polygonsLabel(projSubStates, projSubStates$NAME, method=c("centroid", "inpolygon"), polypart="largest", cex=2)
text(-1151905, -65540, "Idaho", cex=2, col="black")
text(-1445905, -402540, "Nevada", cex=2, col="black")
text(-1521905, 420000, "Washington", cex=2, col="black")
text(-1681905, -802540, "California", cex=2, col="black")

text(184363, -704440, "United\nStates", cex=4, col="#868686")
text(-1931905, -1554440, "A", cex=6, col="black")

plot(wmasLayer)
plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2)
plot(wmasLayer, add=T, col="white", border="#a6a6a6")
plot(subStates, add=T, col="#00000000", border="black", lwd=2)
text(-123, 33, "B", cex=6, col="black")

plot(tradeRegions)
plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2)
plot(tradeRegions, add=T, col="white", border="#a6a6a6")
plot(subStates, add=T, col="#00000000", border="black", lwd=2)
text(-123, 33, "C", cex=6, col="black")
dev.off()
##########################################################################


##Code used to create Figure 2 as part of the Lisk et al data paper

# require(terra)
# wmasLayer <- vect(wmasLayer)
# extWMAs <- ext(wmasLayer)
# ymin(extWMAs) <- 30.8
# xmax(extWMAs) <- -100.6
# 
# subStates <- vect(subStates)
# usStatesLayer <- vect(usStatesLayer)
# #usStatesLayer <- usStatesLayer[as.numeric(usStatesLayer$STATEFP)<60 & as.numeric(usStatesLayer$STATEFP)!=2 & as.numeric(usStatesLayer$STATEFP)!=15,]
# #usStatesLayerC <- crop(usStatesLayer, extWMAs)
# 
# 
# png("/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/borderMaps_onlyWMA.png", width=1500, height=1400)
# layout(matrix(c(1,2), byrow=T, ncol=1))
# par(mar=c(1, 1, 1, 1))
# plot(projUSStatesLayer, col="#c4c4c4", border="#eaeaea", lwd=2)
# plot(projSubStates, add=T, col="white", border="black", lwd=2)
# polygonsLabel(projSubStates, projSubStates$NAME, method=c("centroid", "inpolygon"), polypart="largest", cex=2)
# text(-1151905, -65540, "Idaho", cex=2, col="black")
# text(-1445905, -402540, "Nevada", cex=2, col="black")
# text(-1521905, 420000, "Washington", cex=2, col="black")
# text(-1681905, -802540, "California", cex=2, col="black")
# 
# text(184363, -704440, "United\nStates", cex=4, col="#868686")
# text(-1931905, -1554440, "A", cex=6, col="black")
# 
# plot(wmasLayer, axes=F, xlim=c(xmin(wmasLayer), xmax(extWMAs)), ylim=c(ymin(extWMAs), ymax(wmasLayer)))
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2)
# plot(wmasLayer, add=T, col="white", border="#a6a6a6")
# plot(subStates, add=T, col="#00000000", border="black", lwd=2)
# text(-123, 33, "B", cex=6, col="black")
# dev.off()
# 
# 
# 
# library(tidyterra)
# library(cowplot)
# projUSStatesLayerGG <- vect(projUSStatesLayer)
# projSubStatesGG <- vect(projSubStates)
# projSubStatesGG$NAME[3] <- "New\nMexico"
# usStatesLayerGG <- vect(usStatesLayer)
# 
# #layout(matrix(c(1,2), byrow=T, ncol=1))
# ##create a theme without ticks and tick labels
# blankTheme <- ggplot() + theme_tufte() + 
#   theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x=element_blank(), 
#         axis.text.y=element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), 
#         legend.position="none")
# 
# refMap <- blankTheme + geom_spatvector(data=projUSStatesLayerGG, color="white", fill="white", linewidth=6) +
#   geom_spatvector(data=projUSStatesLayerGG, color="#eaeaea", fill="#c4c4c4", linewidth=1.5) +
#   geom_spatvector(data=projSubStatesGG, color="black", fill="white", linewidth=1.5) +
#   geom_spatvector_text(data=projSubStatesGG, aes(label=NAME), color="black", size=8,
#                        nudge_x=c(60000,0,0,60000,0,0,0,0,0,0,0),
#                        nudge_y=c(-200000,-10000,0,-150000,0,0,0,150000,0,0,0)) +
#   annotate("text", x=184363, y=-704440, label="United\nStates", color="#868686", size=12)   
# 
# borderMap <- blankTheme + geom_spatvector(data=usStatesLayerGG, color="#eaeaea", fill="#c4c4c4", linewidth=2) +
#   geom_spatvector(data=wmasLayer, color="#a6a6a6", fill="white") +
#   geom_spatvector(data=subStates, color="black", fill="#00000000", linewidth=2) +
#   coord_sf(xlim=c(xmin(wmasLayer), xmax(extWMAs)-1), ylim=c(ymin(extWMAs)-10, ymax(wmasLayer)))
# 
# completeMap <- ggdraw() + draw_plot(borderMap) + 
#   draw_plot(refMap, x=0.1, y=-0.1, width=0.8, height=0.6)
# 
# 
# png("/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/borderMaps_onlyWMA_opt2.png", width=1500, height=1600)
# par(mar=c(1, 1, 1, 1))
# completeMap
# dev.off()