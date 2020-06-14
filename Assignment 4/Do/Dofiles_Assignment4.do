*-----------------------------*
*-----Dofile Assingment 4-----*
*-----------------------------*
*----Author: Lewis Polo-------*
*--------Uniandes-------------*


clear all

set more off, permanently
set matsize 5000

global data "/Users/lwenrique/Documents/GitHub/RDD/Assignment 4/Data"
global regressions "/Users/lwenrique/Documents/GitHub/RDD/Assignment 4/Regressions"
global tables "/Users/lwenrique/Documents/GitHub/RDD/Assignment 4/Tables"
global figures "/Users/lwenrique/Documents/GitHub/RDD/Assignment 4/Figures"
*=========Point 1=========*

use https://github.com/scunning1975/causal-inference-class/raw/master/hansen_dwi, clear
save "$data/Hansen_dwi.dta", replace

cd "$data"

use "Hansen_dwi.dta", clear

*------------------------------------*
*-------------Point 3----------------*
*-----Creating the Dummy variable----*
*------------------------------------*

gen DUI=.

replace DUI=1 if bac1>=0.08 
replace DUI=0 if bac1<0.08

tab DUI
tabout DUI using "$tables/tab_dui.xls", replace
*------------------------------------*
*-------------Point 4----------------*
*------------------------------------*

*-------Checking manipulation at cuttoff--------*

net install rddensity, from(https://sites.google.com/site/rdpackages/rddensity/stata) replace

rddensity bac1, all c(0.08) plot graph_options(graphregion(color(white)) xtitle("BAC") ytitle("Density") legend(off))
graph export "$figures/Plot_Manipulacion.png", as(png) replace

*-----Replicating Figure 1-----------*
histogram bac1, color(gray%20) xlabel(0(0.1)0.4) xvarformat(%2.1f) 			 ///
ylabel(0(500)2000, angle(0)) graphregion(color(white)) yvarformat(%9.0gc)	 ///
xline(0.08 0.15, lcol(gray) lpattern(line)  lwidth(thin)) bin(800)			 /// 
xtitle("BAC") frequency				
graph export "$figures/Figure1.png", as(png) replace
graph export "$figures/Figure1.pdf", as(pdf) replace

*------------------------------------*
*-------------Point 5----------------*
*------------------------------------*

*---------Replicating Table 2-----------*
*-------------PANEL A-------------------*

gen bac1_c=bac1-0.08
gen bac1_c2=(bac1-0.08)^2


foreach var of varlist male white aged acc {
reg `var' DUI bac1_c i.DUI#c.bac1_c if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_`var'
}
esttab model_male model_white model_aged model_acc							///
using "$regressions/table2.xls", replace keep(DUI)							///
b(3) se(3) star(* 0.10 * 0.05 ** 0.01) nonum nonotes nolines compress obslast	///
stats(ymean N, label("Mean" "Observations") ///
fmt(3 3 0)) rename(Male White Age Accident)

	
*------------------------------------*
*-------------Point 6----------------*
*------------------------------------*

*---------Replicating Figure 2-----------*
*-------------PANEL A-D -----------------*

ssc install cmogram

*-------Linear
foreach var of varlist male white acc aged {

cmogram `var' bac1 if bac1<=0.2, cut(0.08) scatter line(0.08 0.15) lfitci 

graph export "$figures/Figure2_`var'_linear.png", as(png) replace
graph export "$figures/Figure2_`var'_linear.pdf", as(pdf) replace

}

*-------Quadratic

foreach var of varlist male white acc aged {

cmogram `var' bac1 if bac1<=0.2, cut(0.08) scatter line(0.08 0.15) qfitci 

graph export "$figures/Figure2_`var'_quadratic.png", as(png) replace
graph export "$figures/Figure2_`var'_quadratic.pdf", as(pdf) replace

}
*------------------------------------*
*-------------Point 7----------------*
*------------------------------------*

*======Panel A=========*
reg recidivism DUI bac1_c if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_1A

reg recidivism DUI bac1_c DUI#c.bac1_c if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_2A

reg recidivism DUI bac1_c bac1_c2 i.DUI#c.bac1_c i.DUI#c.bac1_c2 if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_3A

reg recidivism DUI bac1_c male white acc aged if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_4A

reg recidivism DUI bac1_c DUI#c.bac1_c male white acc aged if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_5A

reg recidivism DUI bac1_c bac1_c2 i.DUI#c.bac1_c i.DUI#c.bac1_c2 male white acc aged if (bac1>=0.03 & bac1<=0.13), r
estadd ysumm
estimates store model_6A


esttab model_1A model_2A model_3A model_4A model_5A model_6A							///
using "$regressions/table3_A.xls", replace keep(DUI)							///
b(3) se(3) star(* 0.10 * 0.05 ** 0.01) nonum nonotes nolines compress obslast	///
stats(ymean N, label("Mean" "Observations") ///
fmt(3 3 0)) 

*=======Panel B=======*
reg recidivism DUI bac1_c if (bac1>=0.055 & bac1<=0.105), r
estadd ysumm
estimates store model_1B

reg recidivism DUI bac1_c DUI#c.bac1_c if (bac1>=0.055 & bac1<=0.105), r
estadd ysumm
estimates store model_2B

reg recidivism DUI bac1_c bac1_c2 i.DUI#c.bac1_c i.DUI#c.bac1_c2 if (bac1>=0.055 & bac1<=0.105), r
estadd ysumm
estimates store model_3B

reg recidivism DUI bac1_c male white acc aged if (bac1>=0.055 & bac1<=0.105), r
estadd ysumm
estimates store model_4B

reg recidivism DUI bac1_c DUI#c.bac1_c male white acc aged if (bac1>=0.055 & bac1<=0.105), r
estadd ysumm
estimates store model_5B

reg recidivism DUI bac1_c bac1_c2 i.DUI#c.bac1_c i.DUI#c.bac1_c2 male white acc aged if (bac1>=0.055 & bac1<=0.105), r
estadd ysumm
estimates store model_6B


esttab model_1B model_2B model_3B model_4B model_5B model_6B							///
using "$regressions/table3_B.xls", replace keep(DUI)							///
b(3) se(3) star(* 0.10 * 0.05 ** 0.01) nonum nonotes nolines compress obslast	///
stats(ymean N, label("Mean" "Observations") ///
fmt(3 3 0)) 


*------------------------------------*
*-------------Point 8----------------*
*------------------------------------*

*============Figure 3===========*

*-----Point A--------*

cmogram recidivism bac1 if bac1<=0.15, cut(0.08) scatter line(0.08 0.15) lfitci
graph export "$figures/Figure3_A.png", as(png) replace
graph export "$figures/Figure3_A.pdf", as(pdf) replace

*-----Point B--------*

cmogram recidivism bac1 if bac1<=0.15, cut(0.08) scatter line(0.08 0.15) qfitci
graph export "$figures/Figure3_B.png", as(png) replace
graph export "$figures/Figure3_B.pdf", as(pdf) replace












