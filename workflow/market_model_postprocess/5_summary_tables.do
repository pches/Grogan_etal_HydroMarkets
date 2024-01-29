***************************************************************
***************************************************************
** Description: This script cleans up the welfare calculations done by R 
** to generate the welfare gains by sector and by state data that is used
** in Panel F of Figure 3, and Table S6.
**
****************************************************************
****************************************************************

*------------------ clean data to generate data for Panel F in Figure 3---------------------------*

** Panel A: get the summary statistics of actual trades **
clear all

import excel "C:\Users\jiamengz\Desktop\welfare_code_data\data\welfare_summary_actual_trades.xlsx", sheet("Sheet1") firstrow

replace welfare = 0 if welfare <0
gen ps = SW_rowSum + GW_rowSum
gen cs = SW_colSum + GW_colSum 
gen prod_surplus = ps * 60*60*6*100
gen cons_surplus = cs *60*60*6*100

gen welinmill = welfare/1000000
gen psinmill = prod_surplus/1000000
gen csinmill = cons_surplus/1000000

keep welinmill psinmill csinmill state
collapse (sum) welinmill psinmill csinmill, by(state)
gen scenario = "A"
tempfile data1
save `data1'


clear

** Panel B: Get the summary statistics of state without cutoffs ****
import excel "C:\Users\jiamengz\Desktop\welfare_code_data\data\welfare_summary_simulated.xlsx", sheet("Sheet1") firstrow

collapse (sum) welfare_combined welfare ps cs psinmill csinmill, by(state)
gen welinmill = welfare/1000000

keep welinmill psinmill csinmill state
gen scenario ="B"
tempfile data2
save `data2'



** Panel C: Get the summary statistics of state with cutoffs ****
clear 
import excel "C:\Users\jiamengz\Desktop\welfare_code_data\data\welfare_summary_cutoffs.xlsx", sheet("Sheet1") firstrow

collapse (sum) welfare_combined welfare ps cs psinmill csinmill, by(state)

gen welinmill = welfare/1000000
*collapse (sum) welfare psinmill csinmill
keep welinmill psinmill csinmill state
gen scenario = "C"
tempfile data3
save `data3'


clear
** Get the summary statistics of state with cutoffs + SGMA ****

*clear all
import excel "C:\Users\jiamengz\Desktop\welfare_code_data\data\welfare_summary_cutoffs_SGMA.xlsx", sheet("Sheet1") firstrow


collapse (sum) welfare_combined welfare ps cs psinmill csinmill, by(state)


gen welinmill = welfare/1000000
keep welinmill psinmill csinmill state
gen scenario = "D"
tempfile data4
save `data4'

use `data1', clear
append using `data2'
append using `data3'
append using `data4'

rename psinmill ag_gain
rename csinmill urb_gain
gen pct_ag = ag_gain/welinmill
gen pct_urb = urb_gain/welinmill

save "C:\Users\jiamengz\Desktop\welfare_code_data\data\welfare_gain_by_state_sector.dta", replace


*------------- generate table S6-----------------------*

collapse (sum) ag_gain urb_gain welinmill, by(scenario)


save "C:\Users\jiamengz\Desktop\welfare_code_data\data\welfare_gain_by_sector.dta", replace

