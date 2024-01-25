# welfare data maps

#############################################################################################################
### Libraries and source code
library(raster)
rasterOptions(tmpdir = "/net/usr/spool/")   # set alternative /tmp directory
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(rasterVis)
library(latticeExtra)
library(sf)

#############################################################################################################
### load data
#############################################################################################################

# Load state shapefile for mapping
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

# trade regions for mapping. This data is provided on MSDlive
trade.reg = readOGR("WWRTradeBounds/", "WWRtradeBounds")


### Welfare data
# actual trades
welfare_actual_file = load("data/welfare_data/welfare_data_actual.rdata")  # data provided on MSDlive
welfare_actual_sf   = get(welfare_actual_file[1])
welfare_actual_city = get(welfare_actual_file[2]) # locations of major cities. Use for context on panel A

# make a spatial dataframe for plotting
welfare_city_sp = as_Spatial(welfare_actual_city)

# simulated trades
welfare_sim_file = load("data/welfare_data/welfare_data_simulated.rdata")  # data provided  on MSDlive
welfare_sim_sf   = get(welfare_sim_file[1])

# simulated with wet/paper threshold 
welfare_sim_t_file = load("data/welfare_data/welfare_data_simulated_cutoffs.rdata")  # data provided on MSDlive
welfare_sim_t_sf   = get(welfare_sim_t_file[1])

# simulated with thresholds and SGMA
welfare_sim_sgma_file = load("data/welfare_data/welfare_data_simulated_cutoffs_SGMA.rdata")  # data provided on MSDlive
welfare_sim_sgma_sf   = get(welfare_sim_sgma_file[1])

# make a spatial dataframe for plotting
welfare_actual_sp = as_Spatial(welfare_actual_sf)


#############################################################################################################
### Calculate delta (%) between different scenarios
#############################################################################################################

# Delta: Difference between Scenario 1 (simulated trades) and Scenario 2: with and without the wet/paper thresholds
welfare_sim_and_thresh_delta_percent = 100*(as.numeric(welfare_sim_t_sf$welinmill) - as.numeric(welfare_sim_sf$welinmill))/as.numeric(welfare_sim_sf$welinmill)
welfare_sim_t_sf$delta_percent = welfare_sim_and_thresh_delta_percent
welfare_sim_and_thresh_delta = (as.numeric(welfare_sim_t_sf$welinmill) - as.numeric(welfare_sim_sf$welinmill))
welfare_sim_t_sf$delta_level = welfare_sim_and_thresh_delta

# Delta: Difference between Scenario 2 and Scenario 2b: Both with wet/paper threshold, but 2b adds the groundwater constraint
welfare_sim_thesh_and_sgw_delta_percent = 100*(as.numeric(welfare_sim_sgma_sf$welinmill) - as.numeric(welfare_sim_t_sf$welinmill))/as.numeric(welfare_sim_t_sf$welinmill)
welfare_sim_sgma_sf$delta_percent = welfare_sim_thesh_and_sgw_delta_percent


# link data to spatial dataframe
welfare_actual_sp = as_Spatial(welfare_actual_sf)
welfare_actual_and_sim_sp = welfare_actual_sp
welfare_actual_and_sim_sp$welfare_mil_actual = welfare_actual_and_sim$welfare_mil_actual
welfare_actual_and_sim_sp$welfare_mil_sim    = welfare_actual_and_sim$welfare_mil_sim
welfare_actual_and_sim_sp$delta_percent      = welfare_actual_and_sim$delta_percent

#############################################################################################################
### Rasterize shapefiles with values (this makes nice looking plots using rasterVis::levelplot)
#############################################################################################################

r <- raster(ncol=1000, nrow=1000)
extent(r) <- extent(welfare_actual_sf)
welfare_actual.r    = rasterize(welfare_actual_sf,    r, 'welinmill')
welfare_sim.r       = rasterize(welfare_sim_sf,       r, 'welinmill')

# rasterize deltas
delta_b_c = rasterize(welfare_sim_t_sf,    r, "delta_percent")
delta_c_d = rasterize(welfare_sim_sgma_sf, r, "delta_percent")

#############################################################################################################
### Plotting setup items
#############################################################################################################

# Identify No Data polygons. Make a separate shapefile for these so they can be plotted gray
welfare_actual_sp = as_Spatial(welfare_actual_sf)
welfare_actual_nodata = welfare_actual_sp[!is.na(welfare_actual_sp$Noapplicable),]

# output directory for map files
out.dir = # output directory

#############################################################################################################
### Make Maps, Figure 3A,B,C,D
#############################################################################################################

## Figure 3A
#  Actual trades
brks.a = seq(1, 125, by=1)
Colorkey.a <- list(at=brks.a)
bg = colorRampPalette(brewer.pal(n=9, name='BuGn'))(1000)

png(file.path(out.dir, "Figure3A.png"), width=900, height=1000, res=250)
rasterVis::levelplot(welfare_actual.r, 
                     par.settings = list(axis.line = list(col = "transparent")),
                     scales=list(draw=FALSE),
                     xlab=NULL,
                     ylab=NULL,
                     margin=F, 
                     colorkey = Colorkey.a,
                     col.regions = gn 
) +
  latticeExtra::layer(sp.polygons(trade.reg, fill = 'transparent', border = 'grey', lwd=0.1)) +
  latticeExtra::layer(sp.polygons(welfare_actual_nodata, fill = 'grey94', border='grey', lwd=0.1)) +
  latticeExtra::layer(sp.polygons(states.wecc,     fill = 'transparent',  lwd=0.9)) +
  latticeExtra::layer(sp.points(welfare_city_sp, pch=21, fill='black', col='lightgrey')) 
dev.off()


### Figure 3B
#   Simulated trades: Scenario 1
brks.b = seq(1, 7000, by=0.5)
Colorkey.b <- list(at=brks.b)
bl = colorRampPalette(brewer.pal(n=9, name='Blues'))(14000)

# saturate color scale at 7000 million
welfare_sim.r.s = welfare_sim.r
welfare_sim.r.s[welfare_sim.r.s > 7000] = 7000

png(file.path(out.dir, "Figure3B.png"), width=900, height=1000, res=250)
rasterVis::levelplot(welfare_sim.r.s,
                     par.settings = list(axis.line = list(col = "transparent")),
                     scales=list(draw=FALSE),
                     xlab=NULL,
                     ylab=NULL,
                     margin=F, 
                     colorkey = Colorkey.b,
                     col.regions = bl
) +
  latticeExtra::layer(sp.polygons(trade.reg, fill = 'transparent', border = 'grey', lwd=0.1)) +
  latticeExtra::layer(sp.polygons(welfare_actual_nodata, fill = 'grey94', border='grey', lwd=0.1)) +
  latticeExtra::layer(sp.polygons(states.wecc,     fill = 'transparent',  lwd=0.9)) 
dev.off()


### Figure 3C
#   % delta: from actual trades to simulated trades (positive means more welfare with simulated)

# Note: there is no white in the color palette for actual data. On this map, white = no trades. light yellow = $0 welfare gains
c1 = rev( c(rev(colorRampPalette(brewer.pal(9, "Blues"))(2)), colorRampPalette(brewer.pal(9, "YlOrRd"))(100)))
brks.a = seq(-100, 2, by=1)
Colorkey.a <- list(at=brks.a)

png(file.path(out.dir, "Figure3C.png"), width=900, height=1000, res=250)
rasterVis::levelplot(delta_b_c, 
          par.settings = list(axis.line = list(col = "transparent")),
          scales=list(draw=FALSE),
          xlab=NULL,
          ylab=NULL,
          margin=F, 
          colorkey = Colorkey.a,
          col.regions = c1
          ) +
  latticeExtra::layer(sp.polygons(trade.reg, fill = 'transparent', border = 'grey', lwd=0.1))+
  latticeExtra::layer(sp.polygons(welfare_actual_nodata, fill = 'grey94', border='grey', lwd=0.1)) +
  latticeExtra::layer(sp.polygons(states.wecc,     fill = 'transparent',  lwd=0.9)) 
dev.off()


### Figure 3D
#   % delta: change from C with groundwater constraint

c2 = rev( c(rev(colorRampPalette(brewer.pal(9, "Blues"))(40)), colorRampPalette(brewer.pal(9, "YlOrRd"))(100)))
brks.a = seq(-100, 40, by=1)
Colorkey.a <- list(at=brks.a)

png(file.path(out.dir, "Figure3D.png"), width=900, height=1000, res=250)
rasterVis::levelplot(delta_c_d, 
                     par.settings = list(axis.line = list(col = "transparent")),
                     scales=list(draw=FALSE),
                     xlab=NULL,
                     ylab=NULL,
                     margin=F, 
                     colorkey = Colorkey.a,
                     col.regions = c2
) +
  latticeExtra::layer(sp.polygons(trade.reg, fill = 'transparent', border = 'grey', lwd=0.1))+
  latticeExtra::layer(sp.polygons(welfare_actual_nodata, fill = 'grey94', border='grey', lwd=0.1)) +
  latticeExtra::layer(sp.polygons(states.wecc,     fill = 'transparent',  lwd=0.9)) 
dev.off()


#############################################################################################################
### Make bar plots: Figure 3E and 3F
#############################################################################################################
library(ggplot2)

### Figure 3E: Region-wide welfare gain (total)

## load regional summary data
dat = read.csv("data/welfare_region_totals.csv") # data provided in Github repo
colnames(dat) = c("Scenario", "Total_gain", "Agricultural_gain", "Urban_gain")
dat = dat[,c(-2)] # don't need this column
dat$Scenario = c("Actual", "1", "3", "3a")

ag.gain = dat[,1:2]
ag.gain$Sector = "Agriculture"
colnames(ag.gain)[2] = "Welfare_gain_MilUSD"

urb.gain = dat[,c(1,3)]
urb.gain$Sector = "Urban"
colnames(urb.gain)[2] = "Welfare_gain_MilUSD"

dat.long = rbind(ag.gain, urb.gain)
dat.long$Welfare_gain_BilUSD = dat.long$Welfare_gain_MilUSD/1e3

# set the order of the scenarios plotted
dat.long$Scenario = factor(dat.long$Scenario, levels = c("Actual", "1", "3", "3a"))

p1=ggplot() +
  geom_bar(data = dat.long,
           aes(x = Scenario, y = Welfare_gain_BilUSD, fill = Sector),
           stat="identity")+
  scale_fill_manual(values = c("cadetblue3", "bisque3"))+
  ylab("Welfare Gain (Billion $)") +
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  theme_bw(base_size = 26)

ggsave(filename = file.path(out.dir, "Figure3E.png"),
       plot = p1,
       dpi=300,
       width=9.5,
       height=6,
       units="in"
       )


### Figure 3F: state-by-state welfare gains, percent by sector

# load state-level data
dat = read.csv("data/welfare_gain_by_state_sector.csv") # data provided in GitHub repo

# remove states with no welfare gains
r = which(dat$state_name == "Wyoming" | dat$state_name == "Montana")
dat = dat[-r,]
colnames(dat)[5:6]=c("Agriculture", "Urban")

# Subset to results from Scenario 2
dat.hydro = dat[22:30,]

# Reformat to long dataframe for plotting
dat.hydro.long = gather(data = dat.hydro, key = "Sector", value = "gain", Agriculture:Urban)

# Make percent stacked barplot
dat.hydro.pct = subset(dat.hydro.long, dat.hydro.long$Sector %in% c("Agriculture", "Urban"))

p = ggplot(dat.hydro.pct, aes(fill=Sector, y=gain, x=state_name)) +
  geom_bar(position="fill", stat="identity") +
  scale_fill_manual(values = c("cadetblue3", "bisque3")) +
  geom_hline(yintercept = 0.5, color = 'grey30') +
  xlab("State") +
  ylab("Share of Welfare Gains from Trade") +
  theme_bw(base_size = 28) +
  theme(legend.position='bottom') +
  scale_x_discrete(limits=rev) +
  scale_y_continuous(expand=c(0,0)) +
  coord_flip() 

ggsave(filename = file.path(out.dir, "Figure3F.png"),
       plot=p,
       width=9.5,
       height=9,
       units='in')

# EOF
