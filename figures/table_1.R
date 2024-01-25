# Spatial aggregation of regional water use metrics
# Table 1

library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)


####################################################################################################################################
# Functions #
####################################################################################################################################

# load wbm data into a brick
wbm_load = function(path, varname, years = NA){
  # path     = character string; file path to model output
  # varname  = character string; variable name to load 
  # years    = vector or NA; 
  #            vector: sequence of years of model data to read if you don't want to read ALL files in file path
  
  if(sum(is.na(years)) == 1){
    file.list = list.files(path = path, full.names = T)
  }else{
    file.list.full = list.files(path = path, full.names = T)
    file.list = file.list.full[unlist(sapply(years, FUN = function(x) grep(pattern=paste("wbm_",x, sep=""), file.list.full)))]
  }
  
  wbm.brk = do.call(stack,
                    lapply(file.list, 
                           raster::brick, 
                           varname = varname)
  )
  return(wbm.brk)
}
####################################################################################################################################

# aggregate gridded raster or brick to a spatial shapefile
spatial_aggregation<-function(raster.data, shapefile, s = 1, cell.area = 1, weight = T, poly.out = T, na.rm=T){
  # raster.data = raster (or brick?) of data in mm
  # shapefile   = shapefile with polygons
  # s: set to 1 for sum over area, 0 for mean (mm) over area
  # cell.area   = raster of cell areas, used if sum=1.  If no input raster is provided, cell.area is calculted from raster.data
  #             NOTE: R function cell area results in 0.2% smaller cell area at equator than WBM-based 30-minute cell size
  # weight: if T, the mean value is the area-weighted mean of cells within a polygon
  # poly.out: if T, the output is a shapefile that includes the new, sum or mean value.  If F, output is just a list of values.
  
  if(s == 1){ # output sum (km3)
    if(length(cell.area)==1){
      #print("cell.area calculated from raster.data")
      cell.area<-raster::area(raster.data) # unit: km2.  
    }
    mm_to_km = 10^-6
    # raster.data[is.na(raster.data)]<-c(0)   # causes problems with bricks
    data.km3<-overlay(raster.data, cell.area, fun=function(x,y){return(mm_to_km * x * y)}) 
    data.out<-raster::extract(data.km3, shapefile, fun=sum, na.rm=T, sp=poly.out)
  }else{ # output mean (mm)
    data.out<-raster::extract(raster.data, shapefile, fun=mean, weights = weight, na.rm=T, sp=poly.out)
  }
  data.out
}

####################################################################################################################################

# calculate state and regional sums by aggregating WBM output, and saving .csv files of results
# this function saves intermediate steps to .csv files

calculate_state_and_region_sums = function(wbm.path, scenario){
  
  vars = c("indUseGross",
           "domUseGross",
           "irrigationGross",
           "irrigationExtra",
           "indUseEvap",
           "domUseEvap",
           "irrigationNet")
  
  for(v in vars){
    dat = wbm_load(file.path(wbm.path, "yearly", v), varname = v)
    
    dat = raster(file.path(wbm.path, "climatology", paste("wbm_", v, "_yc.nc", sep="")), varname = v)
    dat.yr = dat*365                                            # convert from average daily to total annual value
    dat.states = spatial_aggregation(dat.yr, states.wecc, s=1)  # returns state-level sums 
    dat.mean   = dat.states$layer                               # $layer is the average annual sum for each state
    
    dat.mean.sum = sum(as.numeric(dat.mean))                    # calculate regional sum (all states summed together)
    out.dat = as.data.frame(cbind(c(states.wecc$NAME,"Region"), c(as.numeric(dat.mean), dat.mean.sum)))
    
    colnames(out.dat) = c("State", paste(v, "mean", sep="_"))
    write.csv(out.dat, paste("results/Table_1/", v, "_", scenario, ".csv", sep=""), row.names=F)
    # print(v)
  }
  
  # combine industrial + domestic = urban
  ind.withdrawal = read.csv(paste("results/Table_1/indUseGross_", scenario, ".csv", sep=""))
  dom.withdrawal = read.csv(paste("results/Table_1/domUseGross_", scenario, ".csv", sep=""))
  
  ind.consume = read.csv(paste("results/Table_1/indUseEvap_", scenario, ".csv", sep=""))
  dom.consume = read.csv(paste("results/Table_1/domUseEvap_", scenario, ".csv", sep=""))
  
  urban.withdrawal = ind.withdrawal[,2] + dom.withdrawal[,2]
  urban.withdrawal = cbind(ind.withdrawal[,1], urban.withdrawal)
  colnames(urban.withdrawal)[1] = "State"
  
  urban.consume = ind.consume[,2] + dom.consume[,2]
  urban.consume = cbind(ind.consume[,1], urban.consume)
  colnames(urban.consume)[1] = "State"
  
  write.csv(urban.withdrawal, paste("results/Table_1/urban_withdrawal_",  scenario, ".csv", sep=""),  row.names=F)
  write.csv(urban.consume,    paste("results/Table_1/urban_consumption_", scenario, ".csv", sep=""), row.names=F)
  
}

####################################################################################################################################
# Load spatial shapefiles for states
####################################################################################################################################

# state shapefile is available from the US Census Bureau: https://www.census.gov/geographies/mapping-files/2015/geo/carto-boundary-file.html
states = readOGR('cb_2015_us_state_500k/', 'cb_2015_us_state_500k')
# states = readOGR("/net/nfs/squam/raid/userdata/dgrogan/data/map_data/cb_2015_us_state_500k/", "cb_2015_us_state_500k")

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

####################################################################################################################################
# File paths and scenario names
####################################################################################################################################

wbm.path.list = c("/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/WatInst_noPolicy_v25",
                  "/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/WatInst_noMarketMod_v25",
                  "/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/WatInst_noMarketMod_sgma_v25",
                  "/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/WatInst_waterMarket_v25",
                  "/net/nfs/squam/raid/data/WBM_TrANS/DOE/Water_Institutions/WatInst_waterMarket_sgma_v25")
                  
scenario.list = c("scenario1",   # Scenario 1:  Siloed hydrology model
                  "scenario2",   # Scenario 2:  Hydrology model with water rights but no water market
                  "scenario2a",  # Scenario 2a: Scenario 2 with groundwater constraint
                  "scenario3",   # Scenario 3:  Integrated hydrology and water market model
                  "scenario3a")  # Scenario 3a: Scenario 3 with groundwater constraint
                  


####################################################################################################################################
# Call state and regional sum function. This takes a few minutes
####################################################################################################################################

for(i in 1:5){
  calculate_state_and_region_sums(wbm.path.list[i], scenario.list[i])
  print(scenario.list[i])
}


####################################################################################################################################
# Gather regional total values from .csv files generated above to form Table 1
####################################################################################################################################

# set up table dimensions, column names, and row names
table_1 = as.data.frame(mat.or.vec(nr=9, nc=6))
colnames(table_1) = c("urban_withdrawal", "agriculture_withdrawal",
                      "groundwater_depletion_from_agriculture",
                      "urban_consumption", "agriculture_consumption", 
                      "total")
rownames(table_1) = c("scenario1", "scenario2", "scenario2a", "scenario3", "scenario3a",
                      "scen2_minus_scen1", "scen3_minus_scen2", "scen2a_minus_scen2", "scen3a_minus_scen3")

# select regional total data to fill in table
table_1[1,1] = read.csv("results/Table_1/urban_withdrawal_scenario1.csv")[12,2] 
table_1[1,2] = read.csv("results/Table_1/irrigationGross_scenario1.csv")[12,2]
table_1[1,3] = read.csv("results/Table_1/irrigationExtra_scenario1.csv")[12,2]
table_1[1,4] = read.csv("results/Table_1/urban_consumption_scenario1.csv")[12,2]
table_1[1,5] = read.csv("results/Table_1/irrigationNet_scenario1.csv")[12,2]

table_1[2,1] = read.csv("results/Table_1/urban_withdrawal_scenario2.csv")[12,2]
table_1[2,2] = read.csv("results/Table_1/irrigationGross_scenario2.csv")[12,2]
table_1[2,3] = read.csv("results/Table_1/irrigationExtra_scenario2.csv")[12,2]
table_1[2,4] = read.csv("results/Table_1/urban_consumption_scenario2.csv")[12,2]
table_1[2,5] = read.csv("results/Table_1/irrigationNet_scenario2.csv")[12,2]

table_1[3,1] = read.csv("results/Table_1/urban_withdrawal_scenario2a.csv")[12,2]
table_1[3,2] = read.csv("results/Table_1/irrigationGross_scenario2a.csv")[12,2]
table_1[3,3] = read.csv("results/Table_1/irrigationExtra_scenario2a.csv")[12,2]
table_1[3,4] = read.csv("results/Table_1/urban_consumption_scenario2a.csv")[12,2]
table_1[3,5] = read.csv("results/Table_1/irrigationNet_scenario2a.csv")[12,2]

table_1[4,1] = read.csv("results/Table_1/urban_withdrawal_scenario3.csv")[12,2]
table_1[4,2] = read.csv("results/Table_1/irrigationGross_scenario3.csv")[12,2]
table_1[4,3] = read.csv("results/Table_1/irrigationExtra_scenario3.csv")[12,2]
table_1[4,4] = read.csv("results/Table_1/urban_consumption_scenario3.csv")[12,2]
table_1[4,5] = read.csv("results/Table_1/irrigationNet_scenario3.csv")[12,2]

table_1[5,1] = read.csv("results/Table_1/urban_withdrawal_scenario3a.csv")[12,2]
table_1[5,2] = read.csv("results/Table_1/irrigationGross_scenario3a.csv")[12,2]
table_1[5,3] = read.csv("results/Table_1/irrigationExtra_scenario3a.csv")[12,2]
table_1[5,4] = read.csv("results/Table_1/urban_consumption_scenario3a.csv")[12,2]
table_1[5,5] = read.csv("results/Table_1/irrigationNet_scenario3a.csv")[12,2]

# calculate total consumption: urban + agriculture
table_1[,6] = rowSums(table_1[,4:5]) 

# calculate comparisons between scenarios
table_1[6,] = table_1[2,] - table_1[1,]  # scen2_minus_scen1
table_1[7,] = table_1[4,] - table_1[2,]  # scen3_minus_scen2
table_1[8,] = table_1[3,] - table_1[2,]  # scen2a_minus_scen2
table_1[9,] = table_1[5,] - table_1[4,]  # scen3a_minus_scen3

# report all numbers to 3 significant figures ((# This doesn't seem to transfer to .csv file results. Not sure why - still only report to 3 sigfig in final table in paper))
table_1 = signif(table_1, 3)

# save to file
write.csv(table_1, "results/Table_1/Table_1.csv")
