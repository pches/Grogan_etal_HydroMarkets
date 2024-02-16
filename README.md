# Code for: Bringing Hydrologic Realism to Water Markets by Grogan et al.

This code was written by: Danielle Grogan, Matt Lisk, Shan Zuidema, and Jiameng Zheng.  
This repository contains all the code to run the Water Market model, post-process the market model output, and post-process hydrologic model output described in the associated paper. This paper is submitted; if it is published, a full citation and link will be added here.

Current paper citation:
Grogan D., Lisk M., Zuidema S., Zheng J., Fisher-Vanden K., Lammers R., Olmstead S., Fowler L., and Prusevich A. Bringing Hydrologic Realism to Water Markets. _submitted_

Input and output data from this repo's code can be found here:

Input Water Rights data: Lisk, M., Grogan, D., Zuidema, S., Caccese, R., Peklak, D., Zheng, J., Fisher-Vanden, K., Lammers, R., Olmstead, S., & Fowler, L. (2023). Harmonized Database of Western U.S. Water Rights (HarDWR) (Version v1) [Data set]. MSD-LIVE Data Repository. https://doi.org/10.57931/2205619

Input data, intermediate processing steps, and output data useful for reproducing figures and tables: Grogan, D., Lisk, M., Zuidema, S., Zheng, J., Fisher-Vanden, K., Lammers, R., Olmstead, S., Fowler, L., & Prusevich, A. (2024). Data for Grogan et al. "Bringing Hydrologic Realism to Water Markets" (Version v1) [Data set]. MSD-LIVE Data Repository. https://doi.org/10.57931/2283495

The hydrologic model described in Bringing Hydrologic Realism to Water Markets is a branch of the University of New Hampshire Water Balance Model, WBM. The main branch of WBM is available here: https://github.com/wsag/WBM, DOI 10.5281/zenodo.6263096. The water rights module described in the paper is available upon request from the authors, and will soon be downloadable from https://license.unh.edu/products/Software 

Contact info: danielle.grogan@unh.edu

## System requirements:
This code is written in standard R and Python coding languages. No special hardware or system requirements necessary. The R code requires use of the raster package for some processing. All processing can be done on a regular desktop computer.  


## How to use the data Data for Grogan et al. "Bringing Hydrologic Realism to Water Markets" (Version v1) 
The code provided here can be used on the data available at https://doi.org/10.57931/2283495 to reproduce the figures and tables in the paper. Here we describe each dataset and which piece of code they are used with:

Folder: marketTrdSummaries/  Description: Files in this folder are used as input to code 1_WelfareCalculation_actual_trades.R. They summarize historical water right trade transactions in each state.

File Name: welfare_gain_by_state_sector.csv; Description: Welfare gains by state and sector, as shown in Figure 3F. Used in code Figure3.R and produced (as a .xlsx file) by code 2_DemandCurves_simulated_trades.R

File Name: welfare_data_actual.rdata; Description: welfare gains by WMA from actual historical trades, as shown in Figure 3A. This data is the output of code 1_WelfareCalculation_actual_trades.R

File Name: welfare_summary_simulated.xlsx; Description: Welfare gains by state as simulated by the market model in Scenario 1. Produced by code 2_DemandCurves_simulated_trades.R, and used in code 4_WelfareCalculation.R. 

File Name: welfare_summary_cutoffs.xlsx; Description: Welfare gains by state as simulated by the market model in Scenario 2. Produced by code 3_DemandCurves_simulated_trades_cutoffs.R, and used in code 4_WelfareCalculation.R. 

File Name: welfare_summary_cutoffs_SGMS.xlsx; Description: Welfare gains by state as simulated by the market model in Scenario 2a. Produced by code 3_DemandCurves_simulated_trades_cutoffs.R, and used in code 4_WelfareCalculation.R. 

File Name: welfare_data_actual.rdata; Description: Spatial data, actual historical welfare gains by WMA as shown in Figure 3A. Produced by code 4_WelfareCalculation.R and used by code Figure3.R.

File Name: welfare_data_simulated.rdata; Description: Spatial data, simulated Scenario 1 welfare gains by WMA as shown in Figure 3B. Produced by code 4_WelfareCalculation.R and used by code Figure3.R.

File Name: welfare_data_simulated_cutoffs.rdata; Description: Spatial data, simulated Scenario 2 welfare gains by WMA. Produced by code 4_WelfareCalculation.R and used by code Figure3.R.

File Name: welfare_data_simulated_cutoffs_SGMA.rdata; Description: Spatial data, simulated Scenario 2a welfare gains by WMA. Produced by code 4_WelfareCalculation.R and used by code Figure3.R.

File Name: wma_thresold_dates_Scenario2(a).csv; Description: Wet vs. paper right threshold dates for each WMA. Shown in Figure 2A,B. Produced and used by code calculate_thresolds_Figure2.R

File Name: WWRTradeBounds (directory); Description: Trade boundary shapefile required to reproduce Figure 3A-D. Used in code Figure3.R

File Name: welfare_region_totals.csv; Description: Welfare gains for the entire study region, as shown in Figure 3E. Used in code Figure3.R

File Name: Welfare_gain_by_state_sector.csv; Description: Welfare gains by state and sector, as shown in Figure 3F. Used in code Figure3.R and produced (as a .xlsx file) by code 2_DemandCurves_simulated_trades.R

File Name: WECC_MERIT_5min_v3b_mask.nc; Description: Gridded file that identified which land grid cells are in the WBM model domain, used for processing in code Figure4.py

WBM output files: scenario[x]_wbm_output.zip, where [x] is one of 1, 2, 2a, 3, and 3a. These output files are used to produce Table 1 by code table_1.R.  These files are large gridded netcdf files, and table_1.R makes use of the R raster package. This is the only processing step that likely takes more than a few minutes on a standard desktop computer.

File Name: Table_1.csv; Description: All data in Table 1, reproducible from WBM output files using code table_1.R
