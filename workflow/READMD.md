# Workflow
A collection of scripts for reproducing the analysis.

## Elasticity Calculations


## Market Modeling: marketModeling/hydroRealism_marketModel.R
This script preforms the market model fitting and prediction of amount of traded water rights for the Hydro Realism analysis. This is done with data from West Water Research (WWR) to inform the market model. The market model uses the point expansion method to calculate an equilibrium between quantities and prices to find a state where net benefits are maximized.

The general workflow of the model is to read in the HarDWR cumulative curves (https://data.msdlive.org/records/y76hr-dvn55), aggregate the WMA data into trade regions, "fit" the market model to determine the amount of water rights that should be traded, and modify the original WMA cumulative curves based on the trade region model results. The modified WMA cumulative curves are then ready to be input into WBM for the hydrologic analysis. As the relationship between WMAs and trade regions is based on area overlap, polygons of these boundaries are also required. The source of the spatial polygon layers are the same as those identified and used for the creation of Figure S1.

The market model is / can be ran under various scenarios, and `hydroRealism_MarketModel.R` is capable of running any of them based on user input. The script requires a companion script, `hydroRealism_marketModel_CustomFunctions.R`, which contains a series of custom written functions for the analysis. Due some of the required R packages having been retired since the code was first written, a Singularity container has been created in order to preform this analysis. The definition file for said container, `waterRightAnalysis.def` is included to allow others access to the market modeling script without needing to completely replicate the retired environment on their own.

## WBM
