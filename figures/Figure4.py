#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
import matplotlib.pyplot as plt
import matplotlib as mpl
import numpy as np
import pandas as pd
from itertools import product
import xarray as xr
from io import StringIO
from collections import defaultdict


# In[2]:


pd.set_option("display.width",None)
pd.set_option("display.max_columns",None)
pd.set_option("display.max_colwidth",80)
pd.set_option("display.max_rows",None)
pd.set_option("display.precision",6)
pd.options.display.float_format = '{:.4f}'.format


# In[3]:


mask_file= # path to WBM network mask file.  # (Add these to msdlive)
wma_map_file= # path to raster of WMA basinNum at model resolution 
fldr= # path to root folder of water rights cumulative curves.  
bl='noMarketMod' # Folder name assumed for the baseline cumulative curves
                 #  these curves are available at https://data.msdlive.org/records/y76hr-dvn55
mts=['agTrdsOldRights/market_ugw/','agTrdsOldRights/market_sgma/']
                 # Folder names assumed for two water market scenarios
                 #  these curves are available at https://data.msdlive.org/records/<fill-in>
whichSgma=[0,1]
delta_urb={}
delta_urb[0]={}
delta_urb[1]={}
for sg,fn in product(whichSgma,os.listdir(fldr+bl+'/')):
    mt=mts[sg]
    if not fn.endswith('_SW.csv'):
        continue
    df1=pd.read_csv(fldr+bl+'/'+fn,index_col=0,parse_dates=True,date_format="%Y-%m-%d")
    df1g=pd.read_csv(fldr+bl+'/'+fn.replace("SW","GW"),index_col=0,parse_dates=True,date_format="%Y-%m-%d").fillna(0)
    urb1=(df1["CUML"].values*0.01*(df1["Irrigation"].values))[-1]
    if int(fn.split('_')[1]) not in range(645,831):
        urb1+=(df1g["CUML"].values*0.01*(df1g["Irrigation"].values))[-1]
        
    if not os.path.exists(fldr+mt+'/'+fn):
        print("{} in noMarket, but no in Market!".format(fn))
        urb2=np.nan
    else:
        df2=pd.read_csv(fldr+mt+'/'+fn,index_col=0,parse_dates=True,date_format="%Y-%m-%d")
        df2g=pd.read_csv(fldr+mt+'/'+fn.replace("SW","GW"),index_col=0,parse_dates=True,date_format="%Y-%m-%d").fillna(0)
        urb2=(df2["CUML"].values*0.01*(df2["Irrigation"].values))[-1]
        if int(fn.split('_')[1]) not in range(645,831):
            urb2+=(df2g["CUML"].values*0.01*(df2g["Irrigation"].values))[-1]
    
    delta_urb[sg][int(fn.split('_')[1])]={"rights":urb1,"market":urb2}


# In[4]:


# We assume that irrigation rights represent an instanteous allotment applied only during active irrigation.
#  In contrast, the urban rights they are converted would be applied constantly.  To equalize, we 
#  provide a coarse estimate of the uniform fraction of the year that irrigation would be active 
#  ( 4 hours per day and 50 days of the year)
chi = 50/365.256 * 4/24.
df=chi * (0.3048 **3) * 86400 * pd.concat(dict([(mts[x].split('/')[1],pd.DataFrame.from_dict(delta_urb[x],orient="index")) for x in whichSgma]))*365.256/1e9
# Calculate market modifications to the allocations to irrigation
df["diff"]=(df["rights"]-df["market"]).clip(lower=0)
df = df.sort_index()


# In[5]:


# WBM hydrologic model output
version="v25"
scenarios=["noMarketMod","waterMarket"]
sgmas=["","_sgma"]
cell_area_file = "../cell_area/full_cell_area.nc"  # add to msd-live.
folder= # Root folder for WBM model outputs
        #  these folders can be found at data.msdlive.org/records/<fill-in>


# In[6]:

# Define variable names
VAR={"Unsustainable Ground Water":"irrigationExtra",
     "Irrigation Withdrawal":"irrigationGross",
    "GrossDom":"domUseGross","GrossInd":"indUseGross",
    "Irrigation Consumption":"irrigationNet","NetDom":"domUseEvap",
    "NetInd":"indUseEvap"
   } 
DERVAR={"Unsustainable Ground Water":"Unsustainable Ground Water",
       "Irrigation Withdrawal":"Irrigation Withdrawal",
       "Urban Withdrawal":["GrossDom","GrossInd"],
       "Irrigation Consumption":"Irrigation Consumption",
       "Urban Consumption":["NetDom","NetInd"]
       }


# In[7]:


# Summarize model output
def create_dataset(fldr,var=VAR,dervar=DERVAR):
    # Open the raw data
    rdss={}
    for k,wbm_nm in var.items():
        rdss[k]=xr.open_dataset(fldr+"wbm_{}_yc.nc".format(wbm_nm))[wbm_nm].rename(k)
    # Create the derived arrays
    dss={}
    for k,d in dervar.items():
        if type(d) == list:
            dss[k]=rdss[d[0]].rename(k)
            for idd in d[1:]:
                dss[k]+=rdss[idd].values
        else:
            dss[k]=rdss[d]
    for k in dervar.keys():
        dss[k].attrs.update({'long_name':k})
    
    ### Define a specific area file
    area=xr.open_dataset(fldr+cell_area_file)["cell_area"].squeeze(dim="time")
    dss['area']=area.interp_like(dss[k])
    return xr.Dataset(dss,coords=dss[k].coords)


# In[8]:


# Gather model outputs for each scenario
dss=dict()
for sgma, scen in product(sgmas,scenarios):
    fldr=folder+"WatInst_{}{}_{}/climatology/".format(scen,sgma,version)
    var=dict(VAR)
    dervar=dict(DERVAR)
    dss["{}{}".format(scen,sgma)]=create_dataset(fldr,var=var,dervar=dervar).squeeze(dim="time")
    


# In[9]:


# Define masks and aggregation arrays
mask = xr.open_dataset(mask_file)["Band1"].fillna(0).interp_like(dss[list(dss.keys())[0]]).round(0).astype(int)
wmas=xr.open_dataset(wma_map_file)["Band1"].fillna(0)
wmas=wmas.interp_like(dss[list(dss.keys())[0]]).fillna(0).round(0).astype(int)
wma_ids=np.unique(wmas.values)
wma_ids=wma_ids[wma_ids != 0] # WMA 0 is outside the WMA domain


# In[10]:


# Tabulate model output by WMA
data=dict([(mts[x].split('/')[1],{}) for x in whichSgma])
for sgma,wma in product(whichSgma,wma_ids):
    sgma=mts[sgma].split('/')[1]
    data[sgma][wma]=dict()
    
    if 'ugw' in sgma:
        s_out,s_in=(2*[["noMarketMod","waterMarket"]])
    elif "sgma" in sgma:
        s_out,s_in=([["noMarketMod","waterMarket"],["noMarketMod_sgma","waterMarket_sgma"]])
    else:
        raise "un-identified sgma status"
    for so,s in zip(s_out,s_in):
        data[sgma][wma][so]=dict()
        for k in list(dervar.keys()):
            tmp=(1e-6 * 365.256 * dss[s][k] * dss[s]["area"]).where(wmas== wma,0) # km3/y
            data[sgma][wma][so][k]=tmp.sum().values
    data[sgma][wma]=pd.DataFrame.from_dict(data[sgma][wma]).T
for sgma in whichSgma:
    sgma=mts[sgma].split('/')[1]
    data[sgma]=pd.concat(data[sgma]).astype(float)
data=pd.concat(data).astype(float)


# In[11]:


# Integrate WBM model output with water right allocation changes
gross=data["Irrigation Withdrawal"].unstack().sort_index()
gross_rights=gross['noMarketMod']
gross_market=gross['waterMarket']
gross = (gross_rights - gross_market)/gross_rights
df['delta_gross']=gross.clip(lower=0)
df['irr_gross'] = gross_market
df['pre_gross'] = gross_rights
irr_consump_diff=gross_market-gross_rights
df['irr_consump_diff']=irr_consump_diff


# In[12]:


# Define scatter plot sizing functions
c,a,b=50.,0.9,2.0
sizer = lambda X: (c*X)**a + b
invs  = lambda X: (np.clip(X-b,0,None)**(1/a))/c
s=sizer(df['irr_gross'].clip(upper=4))


# In[13]:


# Generate Figure 4 of Grogan et al.

fig,axs=plt.subplots(1,2,figsize=(5.5,3.0),dpi=600,sharey='row')
for ax,sgma in zip(axs,whichSgma):
    sgma=mts[sgma].split('/')[1]
    idx=(df.xs(sgma,level=0)['diff'])>1e-14 # Neglect WMAs with negligible trading

    # Calculate the total number of WMAs with less than 2% relative changes in both 
    #  irrigation and trading - these plot visually as 0% change
    nz=np.array([a+b==2 for a,b in list(zip(x<2,y>-2))]).sum()
    # Plot line of correspondence
    ax.plot([0,100],[0,-100],'b--',lw=0.7,zorder=1)
    ax.plot([0,100],[0,0],'k--',lw=0.7,zorder=1)

    # Plot the individual WMAs
    art=ax.scatter(x=x,y=y,s=s.xs(sgma,level=0).loc[idx],edgecolors='k',facecolor='#159441',
                   marker='o',linewidth=0.6,alpha=0.6,zorder=10) #c=c.loc[idx],cmap='Greys'

    # Calculate region-wide averages weighted by total irrigation in the WMA
    x=100*(df.xs(sgma,level=0)['irr_gross'].loc[idx] * 
           (df.xs(sgma,level=0)['diff'] / df.xs(sgma,level=0)['rights']).loc[idx]).sum() / \
            df.xs(sgma,level=0)['irr_gross'].loc[idx].sum()
        
    y=-100* (df.xs(sgma,level=0)["irr_gross"] * 
             df.xs(sgma,level=0)['delta_gross']).loc[idx].sum() / \
             df.xs(sgma,level=0)['irr_gross'].loc[idx].sum()
    # and plot regional averages    
    art1=ax.plot(x,y,marker='s',markersize=7,color='#E9270D',mew=0,label='US West',alpha=0.7,zorder=20)

    # Label and legend
    if "ugw" in sgma:
        ax.set_ylabel("Change in irrigation withdrawal (%)")

    irrs=[0.0,0.05,0.1,0.2]    
    custom=[mpl.lines.Line2D([],[],marker="o",markeredgecolor='k',ls="none",
                             markerfacecolor="#159441",linewidth=0.6,alpha=0.6,markersize=sizer(x)) for x in irrs]
    ax.set_ylim((-100,10))
    if "ugw" in sgma:
        ax.legend(custom,irrs,loc=3,title="Irrigation ($km^3\ y^{-1}$)",
                  bbox_to_anchor=[0.06,0.03,0.35,0.35],ncol=2,title_fontsize=8,fontsize=7,frameon=True)
        ax.text(-3,4.5,"A   without GW constraint",fontsize=8)
    else:
        ax.text(-3,4.5,"B   with GW constraint",fontsize=8)

fig.subplots_adjust(wspace=0.024)
fig.text(0.5,0.0,'Irrigation rights traded away (%)',ha='center')
plt.show()

