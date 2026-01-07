##########################################################################
##########################################################################
## Script Name: hydroRealism_fig2_boundaryPanels.R
## Purpose of Script: This script creates Figure 2 in the Hydro 
## Realism paper. The original version showed three panels, labeled A, B, and C. 
## Panel A is an establishing panel, showing the 11 Western States whose water 
## rights were used within the study. Panel B shows the WMAs used for the study,
## and Panel C shows the trade region boundaries. The main purpose of the 
## original figure was to succinctly show the the geographic differences between
## the WMAs and the trade regions. 
##
## For the version used to create Figure 2, two more panels were added. The 
## first shows natural hydrologic boundaries within the 11 states and highlights
## that the majority of the hydrologic boundaries cross state borders. The 
## second shows how connected the states are by Hydro-infrastructure and the 
## direction that water moves with said structures. 
##
## In addition, the previous Panel A had it's label removed (now referred to as
## First Panel), with the Second Panel begining the labels with 'a'. 
##
##
## Special Requirements: A series of spatial boundary polygons. More 
## specifically the boundaries of the continental 48 United States, the 
## boundaries of the WMAs, the boundaries of the trade regions defined
## by Waterlitix, the layer of line polygons representing the direction water
## is moved for the donor reservoirs, and the WECC clipped watersheds.
##
## Author: Matthew D. Lisk
## Email: mdl5548@psu.edu
## Date Created: 4/5/2021
##
## Last Moddified By: Author
## Editors Email: Author
## Date Last Edited: 1/7/2026
##
## Copyright (c) 2026 The Pennsylvania State University
##
##########################################################################
##########################################################################
options(stringsAsFactors=F, scipen=999)

##mdl5548's local machine
docDir <- "/Users/mdl5548/Documents/"
gdrBase <- "/Users/mdl5548/Library/CloudStorage/GoogleDrive-mdl5548@psu.edu/Shared drives/PCHES_Project1.2/Water rights project/Water institutions/Data/"
waterCumuDir <- paste0(gdrBase, "waterRightsCumulations/")
stateDataDir <- paste0(waterCumuDir, "inputData/")
##RC machine
#docDir <- "/storage/group/pches/default/users/mdl5548/"
#waterCumuDir <- "/dataDir/"
#stateDataDir <- paste0(waterCumuDir, "inputData/")

##read in libraries
#.libPaths("/Rlib")
library(terra)

##Projection
##For this script, used for panel A, for a most aesthetically 
##pleasing figure
projForAreaCalc <- crs("ESRI:102003")

##########################################################################
##########################################################################
##reading in the boundary datasets from their various sources
##WMA boundaries
wmasFile <- list.files(waterCumuDir, "simpleWMAs.shp$", full.names=T, recursive=T)
wmasLayer <- vect(wmasFile)

##US county boundaries - as some trade regions are county boundaries
##data from US 2018 Census
usCountiesFile <- list.files(paste0(docDir, "backgroundGISData/"), "cb_2018_us_county_500k.shp$", full.names=T, recursive=T)
usCountiesLayer <- vect(usCountiesFile)
usCountiesLayer <- project(usCountiesLayer, wmasLayer)

##US state boundaries, for reference
##data from US 2018 Census
usStatesFile <- list.files(paste0(docDir, "backgroundGISData/"), "cb_2018_us_state_500k.shp$", full.names=T, recursive=T)
usStatesLayer <- vect(usStatesFile)
usStatesLayer <- project(usStatesLayer, wmasLayer)
subStates <- usStatesLayer[usStatesLayer$STUSPS=="CA" | usStatesLayer$STUSPS=="OR" | usStatesLayer$STUSPS=="WA" | usStatesLayer$STUSPS=="ID" | usStatesLayer$STUSPS=="MT" |
                             usStatesLayer$STUSPS=="CO" | usStatesLayer$STUSPS=="AZ" | usStatesLayer$STUSPS=="NV" | usStatesLayer$STUSPS=="UT" | usStatesLayer$STUSPS=="WY" |
                             usStatesLayer$STUSPS=="NM",]

##Read in and organize the various sources that comprise the boundaries used
##for the trade regions. These boundaries are those used by West Water Research
##to organize thier trading data.
##CA - Hydrologic Regions - not WMAs
caliHydoRegFile <- list.files(waterCumuDir, "ca_Hydrologic_Regions.shp$", full.names=T, recursive=T)
caliHydroReg <- vect(caliHydoRegFile)
names(caliHydroReg)[2] <- "basinName"
##CO - WMAs
coloHydroReg <- wmasLayer[wmasLayer$state=="Colorado",]
coloHydroReg$basinName[grep(" - ", coloHydroReg$basinName)] <- sapply(strsplit(coloHydroReg$basinName[grep(" - ", coloHydroReg$basinName)], " - "), "[[", 2)
##WA - Counties
washHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="53",]
washHydroReg$basinName <- washHydroReg$NAME
##OR - Counties
oregHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="41",]
oregHydroReg$basinName <- toupper(oregHydroReg$NAME)
##ID - WMAs
idahoHydroReg <- wmasLayer[wmasLayer$state=="Idaho",]
idahoHydroReg$basinName <- idahoHydroReg$basinNum
##MT - Counties
montHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="30",]
montHydroReg$basinName <- montHydroReg$NAME
##NV - WMAs
nevadHydroReg <- wmasLayer[wmasLayer$state=="Nevada",]
nevadHydroReg$basinName <- nevadHydroReg$basinNum
##UT - Counties
utahHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="49",]
utahHydroReg$basinName <- toupper(utahHydroReg$NAME)
##WY - Counties
wyomHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="56",]
wyomHydroReg$basinName <- wyomHydroReg$NAME
##AZ - Modified ground water WMA's; specifically by dissolving the boundaries of
##the geometries outside of official AMAs and INAs into one polygon
azGrdWMAFile <- list.files(waterCumuDir, "azGroundWMAs.shp", full.names=T, recursive=T)
azGrdWMAs <- vect(azGrdWMAFile)
boundsToKeep <- c("SANTA CRUZ AMA", "JOSEPH CITY INA", "PRESCOTT AMA", "PHOENIX AMA", "HARQUAHALA INA", "PINAL AMA", "TUCSON AMA", "DOUGLAS INA")
azGrdWMAs$basinName[-c(which(azGrdWMAs$basinName %in% boundsToKeep))] <- "OUTSIDE AMA / INA"
arizHydroReg <- aggregate(azGrdWMAs, by="basinName", dissolve=T)
##NM - Counties
nemexHydroReg <- usCountiesLayer[usCountiesLayer$STATEFP=="35",]
nemexHydroReg$basinName <- nemexHydroReg$NAME
##"Correct" a county name to be able to match the name presented in the water
##rights data.
#nemexHydroReg$basinName[grep("ñ", nemexHydroReg$basinName)] <- "Dona Ana"

##Make the trading regions into a single layer
tradeRegions <- rbind(arizHydroReg[,"basinName"], caliHydroReg[,"basinName"], coloHydroReg[,"basinName"], idahoHydroReg[,"basinName"], montHydroReg[,"basinName"],
                      nemexHydroReg[,"basinName"], nevadHydroReg[,"basinName"], oregHydroReg[,"basinName"], utahHydroReg[,"basinName"], washHydroReg[,"basinName"],
                      wyomHydroReg[,"basinName"])

##Reproject the US state layer
projUSStatesLayer <- project(usStatesLayer, projForAreaCalc)
projUSStatesLayer <- projUSStatesLayer[-c(which(projUSStatesLayer$STUSPS %in% c("VI", "AS", "GU", "MP", "AK", "PR", "HI"))),]
projSubStates <- projUSStatesLayer[projUSStatesLayer$STUSPS=="CA" | projUSStatesLayer$STUSPS=="OR" | projUSStatesLayer$STUSPS=="WA" | projUSStatesLayer$STUSPS=="ID" | projUSStatesLayer$STUSPS=="MT" |
                                     projUSStatesLayer$STUSPS=="CO" | projUSStatesLayer$STUSPS=="AZ" | projUSStatesLayer$STUSPS=="NV" | projUSStatesLayer$STUSPS=="UT" | projUSStatesLayer$STUSPS=="WY" |
                                     projUSStatesLayer$STUSPS=="NM",]
##remove Names from state whose names will be manually placed
projSubStates$NAME[c(1,2,4,8)] <- ""

##########################################################################
##########################################################################
##Original figure used in a previous draft of the paper
# png("/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/borderMaps_opt3b_remake.png", width=1500, height=1400)
# layout(matrix(c(1,1,2,3), byrow=T, ncol=2))
# par(mar=c(1, 1, 1, 1))
# ##Panel A
# plot(projUSStatesLayer, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
# plot(projSubStates, add=T, col="white", border="black", lwd=2, axes=F)
# text(projSubStates, labels=projSubStates$NAME, inside=T, cex=2)
# text(-1480957, 825284.5, "Idaho", cex=2, col="black")
# text(-1768457, 505284.5, "Nevada", cex=2, col="black")
# text(-1835957, 1296284.5, "Washington", cex=2, col="black")
# text(-2015957, 78284.5, "California", cex=2, col="black")
# text(-110957, 135284.5, "United\nStates", cex=4, col="#868686")
# text(-2175957, -850284.5, "A", cex=6, col="black")
# 
# ##Panel B
# plot(wmasLayer, axes=F)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
# plot(wmasLayer, add=T, col="white", border="#a6a6a6", axes=F)
# plot(subStates, add=T, col="#00000000", border="black", lwd=2, axes=F)
# text(-123, 33, "B", cex=6, col="black")
# 
# ##Panel C
# plot(tradeRegions, axes=F)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
# plot(tradeRegions, add=T, col="white", border="#a6a6a6", axes=F)
# plot(subStates, add=T, col="#00000000", border="black", lwd=2, axes=F)
# text(-123, 33, "C", cex=6, col="black")
# dev.off()

##########################################################################
##########################################################################
##Read in the WECC Donor Reservoir file
donResFile <- list.files(paste0(docDir, "PCHES/panelData/"), "WECC_DonRes_Interstate.shp$", full.names=T, recursive=T)
donRes <- vect(donResFile)
donRes <- project(donRes, wmasLayer)
##Read in the WECC Watershed Clip file
wshedClipFile <- list.files(paste0(docDir, "PCHES/panelData/"), "WECC_Watershed_Clip_ID.shp$", full.names=T, recursive=T)
wshedClip <- vect(wshedClipFile)
wshedClip <- project(wshedClip, wmasLayer)
wshedClip$col <- "#ffffff"
wshedClip$border <- "#a6a6a6"

##Manually assigning colors for the Watershed Clip panel
modRecs <- c(6, 31, 784, 166, 35, 38, 358, 206, 420, 961, 276, 684, 1561, 1738, 1913, 204, 4338, 1355, 419, 1480, 1790, 2345, 
             2221, 4176, 2838, 3212, 4648, 1365, 4246, 2867, 4861)
findRecs <- sapply(modRecs, function(x){which(wshedClip$Basin_ID==x)})
wshedClip$col[findRecs] <- c("#00c1aa", "#c59900", "#bf80ff", "#00b8e5", "#00b820", "#00b4ef", "#ff65ae", "#afa100", "#ed8141", 
                             "#5bb300", "#9590ff", "#00b0f6", "#e7861b", "#da9100", "#f662df", "#ef65e9", "#8ab014", "#f27755",
                             "#00a5ff", "#01c19c", "#94a800", "#00a6fa", "#00bccf", "#ba9904", "#7897ff", "#19b46e", "#50a0ff",
                             "#00bbdb", "#4bbe18", "#23c267", "#dc9cde")
wshedClip$border[findRecs] <- "#ffffff"

##Convert the hydro-infrastructure data into start-end coordinates data frame,
##As this is easier to plot with arrowheads than as spatial lines
donResGeom <- geom(donRes)
evenGeom <- seq(2, nrow(donResGeom), by=2)
donResStartPts <- donResGeom[-evenGeom,c("geom", "x", "y")]
colnames(donResStartPts)[2:3] <- c("startX", "startY")
donResEndPts <- donResGeom[evenGeom,c("geom", "x", "y")]
colnames(donResEndPts)[2:3] <- c("endX", "endY")
donResReorg <- merge(x=donResStartPts, y=donResEndPts, by="geom")

##This version of the panels includes full CONUS in an Albers Equal Area 
##projection for the first panel. Originally the first panel was labeled 'a'. 
##It was later determined that the second panel should be 'a'. 
# png("/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/borderMaps_5panels_lowercase.png", width=1500, height=2100)
# layout(matrix(c(1,1,2,3,4,5), byrow=T, ncol=2))
# par(mar=c(1, 1, 1, 1))
# ##Panel A
# plot(projUSStatesLayer, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
# plot(projSubStates, add=T, col="white", border="black", lwd=2, axes=F)
# text(projSubStates, labels=projSubStates$NAME, inside=T, cex=2.5)
# text(-1480957, 825284.5, "Idaho", cex=2.5, col="black")
# text(-1768457, 505284.5, "Nevada", cex=2.5, col="black")
# text(-1835957, 1296284.5, "Washington", cex=2.5, col="black")
# text(-2015957, 78284.5, "California", cex=2.5, col="black")
# text(-110957, 135284.5, "United\nStates", cex=5, col="#868686")
# #text(-2175957, -850284.5, "A", cex=7, col="black")
# 
# ##Panel B
# plot(wmasLayer, axes=F)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
# plot(wmasLayer, add=T, col="white", border="#a6a6a6", axes=F)
# plot(subStates, add=T, col="#00000000", border="black", lwd=3, axes=F)
# text(-123, 33, "a", cex=7, col="black")
# 
# ##Panel C
# plot(tradeRegions, axes=F)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
# plot(tradeRegions, add=T, col="white", border="#a6a6a6", axes=F)
# plot(subStates, add=T, col="#00000000", border="black", lwd=3, axes=F)
# text(-123, 33, "b", cex=7, col="black")
# 
# ##Panel D
# plot(wshedClip, axes=F)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
# plot(wshedClip, add=T, col=wshedClip$col, border=wshedClip$border, axes=F)
# plot(subStates, add=T, col="#00000000", border="black", lwd=3, axes=F)
# text(-123, 33, "c", cex=7, col="black")
# 
# ##Panel E
# plot(subStates, axes=F)
# plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
# plot(subStates, add=T, col="#ffffff", border="black", lwd=3, axes=F)
# arrows(x0=donResReorg$startX, y0=donResReorg$startY, x1=donResReorg$endX, y1=donResReorg$endY, col="blue", lwd=2, length=0.15)
# text(-123, 33, "d", cex=7, col="black")
# dev.off()

##########################################################################

##The version used as Figure 2. This has the first panel focused on the 11 study
##states in the same WGS system as the other panels. 
subStates$NAME[c(1,8)] <- ""
png("/Users/mdl5548/Documents/GitHub/waterRightsCumulationCurves/borderMaps_5panels_noCONUS_lowercase.png", width=1500, height=2100)
layout(matrix(c(1,1,2,3,4,5), byrow=T, ncol=2))
par(mar=c(1, 1, 1, 1))

##First Panel
plot(subStates, axes=F)
plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
plot(subStates, add=T, col="#ffffff", border="black", lwd=3, axes=F)
text(subStates, labels=subStates$NAME, inside=T, cex=3)
text(-114.338115, 43.817005, "Idaho", cex=3, col="black")
text(-116.799052, 39.349501, "Nevada", cex=3, col="black")

##Panel a
plot(wmasLayer, axes=F)
plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
plot(wmasLayer, add=T, col="white", border="#a6a6a6", axes=F)
plot(subStates, add=T, col="#00000000", border="black", lwd=3, axes=F)
text(-123, 33, "a", cex=7, col="black")

##Panel b
plot(tradeRegions, axes=F)
plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
plot(tradeRegions, add=T, col="white", border="#a6a6a6", axes=F)
plot(subStates, add=T, col="#00000000", border="black", lwd=3, axes=F)
text(-123, 33, "b", cex=7, col="black")

##Panel c
plot(wshedClip, axes=F)
plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
plot(wshedClip, add=T, col=wshedClip$col, border=wshedClip$border, axes=F)
plot(subStates, add=T, col="#00000000", border="black", lwd=3, axes=F)
text(-123, 33, "c", cex=7, col="black")

##Panel d
plot(subStates, axes=F)
plot(usStatesLayer, add=T, col="#c4c4c4", border="#eaeaea", lwd=2, axes=F)
plot(subStates, add=T, col="#ffffff", border="black", lwd=3, axes=F)
arrows(x0=donResReorg$startX, y0=donResReorg$startY, x1=donResReorg$endX, y1=donResReorg$endY, col="blue", lwd=2, length=0.15)
text(-123, 33, "d", cex=7, col="black")
dev.off()


