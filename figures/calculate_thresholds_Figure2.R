# Code for "Bringing Hydrologic Realism to Water Markets" by Grogan et al. in review

# This script calculates the water right priority date for which earlier dates are likely to actually provide water
# This is the "wet vs paper right' threshold 
# This script makes Figure 2

#############################################################################################################
### Libraries and source code
library(raster)
library(rgdal)
library(rgeos)
library(sp)
library(rasterVis)
library(latticeExtra)

#############################################################################################################
### FUNCTION: Unmet demand calculation
#############################################################################################################

unmet.demand = function(unmet, wma.phys){
  unmet.delta = unmet
  if(length(unmet) > 1){
    for(i in 2:length(unmet)){
      if(wma.phys$jdate[i] - wma.phys$jdate[i-1] == 1){ # this is a consecutive day.  Calculate the delta
        if(unmet[i] > unmet[i-1]){
          unmet.delta[i] = unmet[i] - unmet[i-1]  # only calc a change if it goes up. Otherwise, all unmet counts
        }
      }
    }
  }
  unmet.delta
}

#############################################################################################################
### FUNCTION: calculate the weighted average annual threshold date based on daily water restrictions
#############################################################################################################

irr.weighted.date = function(fpath){  # f.path = file path to WBM output for each WMA
  
  # load file
  wma = read.delim(fpath, sep="\t")
  
  # number the days from 1 to num(days)
  jdate = seq(1,nrow(wma))
  wma = cbind(jdate, wma)
  
  # change cutoff date 0 to 1800
  wma$irr_SW_year[wma$irr_SW_year == 0] = 1800
  
  # days with physical limit cutoffs
  wma.phys = subset(wma, wma$irr_SW_lim == 1)
  
  # calculate unfulfilled demand
  unmet = wma.phys$irr_demand - (wma.phys$irr_AW_vol + wma.phys$irr_GW_vol + wma.phys$irr_SW_vol)
  
  # only count delta(unfulfilled demand) for consecutive days
  unmet.delta = unmet.demand(unmet, wma.phys)
  
  # remove 0s
  unmet.delta[unmet.delta == 0] = NA
  
  # weigh dates by unfulfilled demand (place more weight on dates when the restriction was stricter)
  if(sum(is.na(unmet.delta)) == length(unmet.delta)){
    coeff = rep(NA, length(unmet.delta))
  }else{
    coeff = unmet.delta/min(unmet.delta)
  }
  
  irr.date.weighted = sum(coeff * wma.phys$irr_SW_year)/sum(coeff)
}

#############################################################################################################
### Spatial input data
#############################################################################################################

# HarDWR - Water Management Area (WMA) Shapefiles, available from: https://data.msdlive.org/records/v5ree-qj344 
# DOI: https://doi.org/10.57931/2001052
wma.shp.id = readOGR(dsn = "stateWMAs/", layer = "stateWMAs") # for dsn argument, supply full path to the folder location

#############################################################################################################
### File names
#############################################################################################################

# list the model output names
mod.nm.list = c("Scenario2",    # Scenario 2: Water rights without trading, as simulated by WBM
                "Scenario2a"    # Scenario 2a: Water rights without trading, with groundwater constraint
)

# directory name where model output is stored
mod.dir = # model directory
  
# directory name for saving calculation results:
out.dir = # output directory

#############################################################################################################
### Calculate wet vs paper thresholds
#############################################################################################################

for(mod.name in mod.nm.list){
  
  # directory to all model output
  wma.dir = file.path(mod.dir, mod.nm, "WMA")
  wma.files = list.files(wma.dir, pattern="wma_", full.names = T)
  
  ### Identify which WMAs had irrigation water restrictions (physical limitations)
  irr.restrict = mat.or.vec(nr=0, nc=1)
  count = 1
  for(i in 1:length(wma.files)){
    wma = read.delim(wma.files[i], sep="\t")
    if(sum(wma$irr_SW_lim == 1) > 0){
      irr.restrict[count] = wma.files[i]
      count = count+1
    }
  }
  
  ### Call weighted date functions
  irr.value.dates = lapply(irr.restrict, irr.weighted.date)
  
  ### Connect values to WMAs
  irr.value.dates = unlist(irr.value.dates)
  irr.wma.dates = data.frame(matrix(nr=length(irr.restrict), nc=2))
  irr.wma.dates[,2] = irr.value.dates
  for(i in 1:length(irr.restrict)){
    file.nm = unlist(strsplit(irr.restrict[i], "/"))[12]
    wma.id = regmatches(file.nm, regexpr("(\\d+)", file.nm, perl = TRUE))
    irr.wma.dates[i,1] = wma.id
  }
  colnames(irr.wma.dates) = c("WMA", "Cutoff_Date")
  
  
  # save to file
  write.csv(irr.wma.dates, 
            paste(out.dir, "/wma_cutoff_dates_", mod.nm, ".csv", sep=""),
            row.names = F)
  
}  ### End of calculations

### NOTE: for reproducibility, the following files are available on MSDlive:
# wma_threshold_dates_Scenario2.csv      This is the output for Scenario 2, shown in Figure 2A
# wma_threshold_dates_Scenario2a.csv     This is the output for Scenario 2a, shown in Figure 2B


#############################################
#  Make Figure 2
#############################################

library(tidyr)
library(dplyr)
library(ggplot2)

# state shapefile is available from the US Census Bureau: https://www.census.gov/geographies/mapping-files/2015/geo/carto-boundary-file.html
states = readOGR('cb_2015_us_state_500k/', 'cb_2015_us_state_500k')
states.wecc = states[states$NAME == "Idaho" | 
                       states$NAME == "Washington" | 
                       states$NAME == "Oregon" | 
                       states$NAME == "Colorado" | 
                       states$NAME == "Arizona" |
                       states$NAME == "California" |
                       states$NAME == "Montana" |
                       states$NAME == "Nevada" |
                       states$NAME == "New Mexico" |
                       states$NAME == "Utah" |
                       states$NAME == "Wyoming",]


# Load threshold calculation data (saved on line 132 above, and provided for reproducibility)
noMM = read.csv("wma_threshold_dates_Scenario2.csv")      # data for Figure 2A
MM   = read.csv("wma_threshold_dates_Scenario2a.csv")     # data for Figure 2B

# calculate counts for bins
noMM.count = data.frame(cut_interval(x = noMM$Cutoff_Date, length=10) %>% table)
MM.count   = data.frame(cut_interval(x = MM$Cutoff_Date,   length=10) %>% table)

# merge counts into one data frame
colnames(noMM.count)[1] = colnames(MM.count)[1] = "bin"
cutoff.bins = merge(noMM.count, MM.count, "bin") 
colnames(cutoff.bins)[2:3] = c("B_Current", "B_SGMA")
cutoff.bins = cutoff.bins[order(cutoff.bins$bin),]
cutoff.bins$bin = seq(from = 1800, to = 2000, by = 10)

# Reformat: wide to long
cutoff.bins.long = gather(cutoff.bins, key = "Model_step", value = "WMA_count", B_Current:B_SGMA)
cutoff.bins.long = as_tibble(cutoff.bins.long)

# link WMA shapefile and cutoff date data
wma.shp.id$cutoff.b.curr = mat.or.vec(nc=1,nr=nrow(wma.shp.id@data))
wma.shp.id$cutoff.b.sgma = mat.or.vec(nc=1,nr=nrow(wma.shp.id@data))
for(i in 1:nrow(wma.shp.id@data)){
  if(wma.shp.id$ID[i] %in% noMM$WMA){
    wma.shp.id$cutoff.b.curr[i] = noMM$Cutoff_Date[which(noMM$WMA == wma.shp.id$ID[i])]
  }else{
    wma.shp.id$cutoff.b.curr[i] = NA
  }
  
  if(wma.shp.id$ID[i] %in% MM$WMA){
    wma.shp.id$cutoff.b.sgma[i] = MM$Cutoff_Date[which(MM$WMA == wma.shp.id$ID[i])]
  }else{
    wma.shp.id$cutoff.b.sgma[i] = NA
  }
  print(i)
}

# 1. rasterize shapefiles with values
r <- raster(ncol=1000, nrow=1000)
extent(r) <- extent(wma.shp.id)
wma.bcurr.r     = rasterize(wma.shp.id, r, 'cutoff.b.curr')
wma.bsgma.r     = rasterize(wma.shp.id, r, 'cutoff.b.sgma')


# make color palette
gp = colorRampPalette(brewer.pal(n=9, name='PRGn'))(1000)
bb = colorRampPalette(brewer.pal(n=9, name='BrBG'))(1000)
sp = colorRampPalette(brewer.pal(n=9, name='Spectral'))(1000)

my.brks = seq(1800, 2010, by=1)
myColorkey <- list(at=my.brks)


# Plot Figure 2A
png("Figure_2A.png", width=900, height=1000, res=130)
rasterVis::levelplot(wma.bcurr.r, 
          par.settings = list(axis.line = list(col = "transparent")),
          scales=list(draw=FALSE),
          xlab=NULL,
          ylab=NULL,
          margin=F, 
          #at = my.at,
          colorkey = myColorkey,
          col.regions = sp) +
  latticeExtra::layer(sp.polygons(wma.shp.id,  fill = 'transparent', border = 'grey', lwd=0.1)) +
  latticeExtra::layer(sp.polygons(states.wecc, fill = 'transparent', lwd=0.9)) 
dev.off()


# plot Figure 2B
png("Figure_2B.png", width=900, height=1000, res=130)
rasterVis::levelplot(wma.bsgma.r, 
          par.settings = list(axis.line = list(col = "transparent")),
          scales=list(draw=FALSE),
          xlab=NULL,
          ylab=NULL,
          margin=F, 
          #at = my.at,
          colorkey = myColorkey,
          col.regions = sp) +
  latticeExtra::layer(sp.polygons(wma.shp.id,  fill = 'transparent', border = 'grey', lwd=0.1)) +
  latticeExtra::layer(sp.polygons(states.wecc, fill = 'transparent', lwd=0.9)) 
dev.off()

# EOF
