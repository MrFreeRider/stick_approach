********************************************************************************
*** Paper: El niño is coming...
*** Data adjustment and initial analysis.
*** Author: Jose David Lopez-Rivas  
********************************************************************************

clear all
cd"~/Documents/Research/Stick approach/Paper"
use "~/Documents/GitHub/stick_approach/data_merge.dta", clear

**********************************************************
** Time indicator variable (monthly and bi-monthly)
**********************************************************

// Generating a variable for the timing of the intervention, where 0 
// indicates the beginning of the policy in July 2014.

rename time period
bys id_wss: generate t=_n
gen time = t-45
label var time "Time period"

gen tbi=0
forval j = 1(1)72{
	replace tbi= `j' if t <= (`j'*2) & t > (`j'*2)-2
}

gen timebi=0
replace timebi = tbi-23
label var timebi "Time period"

// Generating a post-intervention indicator variable. There are three moments 
// where the regulator intervened. These are: July 2014=0, October 2014=3, 
// and May 2015=10.

gen post = 0
replace post = 1  if time >= 0
label var post "After"


**********************************************************
*** Treatment variable  (varying over time)
**********************************************************
// Fines = Dynamic treatment indicator variable

// Departments affected by the water fine program over time. 
// 1st Moment => "Resolución CRA 695 de 12 de agosto de 2014"  by the  Atlantico, Bolivar, Boyaca, 
// Caldas, Cesar, La Guajira, Magdalena, Norte de santander, Quindio, 
// Risaralda, Santander, y Tolima.  

gen fines = 0
label variable fines "WSS is treated"

replace fines = 1 if id_dpto == 8   & time >=0
replace fines = 1 if id_dpto == 13 & time >=0
replace fines = 1 if id_dpto == 15 & time >=0
replace fines = 1 if id_dpto == 17 & time >=0
replace fines = 1 if id_dpto == 20 & time >=0
replace fines = 1 if id_dpto == 44 & time >=0
replace fines = 1 if id_dpto == 47 & time >=0
replace fines = 1 if id_dpto == 54 & time >=0
replace fines = 1 if id_dpto == 63 & time >=0
replace fines = 1 if id_dpto == 66 & time >=0
replace fines = 1 if id_dpto == 68 & time >=0
replace fines = 1 if id_dpto == 73 & time >=0

// 2nd Moment => "Resolucion CRA 699 de 21 de octubre de 2014", in Atlantico & Bolivar the regulation
// was lifted. 

replace fines = 0 if id_dpto == 8  & time >= 2
replace fines = 0 if id_dpto == 13 & time >= 2


// 3rd Moment => "Resolucion 714 de 13 de mayo de 2015". Only Magdalena, La Guajira & Cesar 
// remained under regulation.

replace fines = 0 if id_dpto == 15 & time >=8
replace fines = 0 if id_dpto == 17 & time >=8
replace fines = 0 if id_dpto == 54 & time >=8
replace fines = 0 if id_dpto == 63 & time >=8
replace fines = 0 if id_dpto == 66 & time >=8
replace fines = 0 if id_dpto == 68 & time >=8
replace fines = 0 if id_dpto == 73 & time >=8


// Static treatment indicator for each moment.

forvalues i=1/3{
	rename distance_`i' dist_border_`i'
	rename dist_treated_`i' dist_treat_border_`i'
	rename treated_`i' treat_border`i'
	gen treated_`i' = treat_border`i'
	replace treated_`i' = 0 if treat_border`i' == -1
}

***************************************************************
** Percentage of WSS income by consumption
***************************************************************

gen bill_per = (bill_cons/bill_total)*100

***************************************************************
** Adjusting panel bi-monthly
***************************************************************

global varsum cons subsidy contribution rainfall bill_cons  
global varmean temperature users ne1 ne2 ne3 ne4 ne5 ne6 nt bill_per

foreach v in $varsum{
	bys wss tbi: egen `v'bi = sum(`v')
	replace `v'bi =. if `v' == .
	drop `v'
	rename `v'bi `v'
}
foreach v in $varmean{
	bys wss tbi: egen `v'bi = mean(`v')
	replace `v'bi =. if `v' == .
	replace `v'bi =round(`v'bi, 1)
	drop `v'
	rename `v'bi `v' 
}

quietly by wss tbi:  gen dup_bi = cond(_N==1,0,_n)
drop if dup_bi==2
drop if tbi > 30

drop t time
rename tbi t
rename timebi time

drop bill_total ih ig cons_total dup_bi

sort wss time


**********************************************************
** Dependent variable: Consumption per user
**********************************************************

// Dependent variable with no adjustment in cubic meters per household 
// (Monyhly consumption divided into number of users reported). I call 
// this variable "cons_po" statintg for consumption per households (original).
 
gen cons_po=cons/users
label var  cons_po "m3/user"

** Dealing with atypical reports in consumption and users.

// Consumption and users original distribution.
/*
qnorm cons, ytitle(Cubic meters) msize(medium) mcolor(navy%30) msymbol(o) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/qnorm_cons.pdf", replace
window manage close graph
qnorm users, ytitle("Users (#)") msize(medium) mcolor(navy%30) msymbol(o) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/qnorm_users.pdf", replace
window manage close graph
*/
// Checking for jumps in the reports month by month. Variables: consumtion
// and number of users. To do this, I create a variable that counts the number 
// of digits each month by WSS and compare this with its minimun number of 
// digits. Those reports with an increment in one digit in a particular month
// are converted to missing. A new variable is created with the sufix 'fix.' 
 
global var users cons bill_cons

foreach d in  $var {
	gen `d'_fix=`d'
	tostring `d', gen(`d'_st) force
	gen counter_`d'=strlen(`d'_st) 
	replace counter_`d'=. if `d'==.

	bys wss: egen mean_counter_`d'=mean(counter_`d')
	gen change_count_`d'=counter_`d'-mean_counter_`d'

	bys wss: egen min_counter_`d'=min(counter_`d')

	gen diff_low_`d'= counter_`d' - min_counter_`d' // actual digits minus minimun

	replace `d'_fix=. if  diff_low_`d'>1 // if the # digits higher in two. 


}

** Figures
// Scatter plot between consumption and number of digits above the minimum level. 
/* 
twoway (scatter cons diff_low_cons) , ytitle(Cubic meters) xtitle(Change in number of digits) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_counter.pdf", replace
window manage close graph
// Scatter plot between users and number of digits above the minimum level.  
twoway (scatter users diff_low_users) , ytitle(Users (#)) xtitle(Change in number of digits) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/users_counter.pdf", replace
window manage close graph
// Scatter plot between consumption and users original variables.
scatter cons users, ytitle(Cubic meters) xtitle("Users (#)") xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_users_before.pdf", replace
window manage close graph
// Scatter plot between consumption and users original variables.
scatter cons_fix  users_fix , ytitle(Cubic meters) xtitle("Users (#)") xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_users_after.pdf", replace
window manage close graph
*/

foreach d in  $var {
	drop `d'_st counter_`d' mean_counter_`d' change_count_`d' min_counter_`d' diff_low_`d'
}
	


***************************************************
** Consumption per user per month.
***************************************************

// Generating consumption per user with the adjusted consumption and users variables: "cons_pf."

gen cons_pf=cons_fix/users_fix
label var cons_pf "m3/user"


// The next adjustment is a checking on the number of digits and the
// interquartile range process for extreme values. First, I replace as missing data those reports with more than 2 digits, which is atypical for residential water consumption.Second, I determined interquartile range for each WSS, and allow an upper limit of 2.5, replace as missing all values above this. 
 
gen cons_ph=cons_pf

gen cons_p_round = round(cons_ph)
tostring cons_p_round, gen(cons_p_st)
gen counter_cons_p=strlen(cons_p_st) 
replace counter_cons_p=. if cons_p_round==.
replace cons_ph=. if counter_cons_p>2

bys wss: egen p25 = pctile(cons_pf), p(25)
bys wss: egen p75 = pctile(cons_pf), p(75)
gen int_quart = p75 - p25
replace cons_ph = . if int_quart == 0

gen in_fen = int_quart*2.5

gen low_limit = p25-in_fen
gen up_limit = p75+in_fen

replace cons_ph =. if cons_ph >= up_limit

drop cons_p_round cons_p_st counter_cons_p p25 p75 int_quart in_fen low_limit up_limit

label var  cons_ph "m3/user"

// Even though, we adjusted the variables separately, it can be noted that in 
// the distribution of "cons_pf" there are still outliers that could affect the 
// results. Therefore, I use five adjustments for robustness purpose. I cut the 
// tails of the distribtuion at 1%, 5%, 10% and 25%. I will estimate the 
// regressions with this later.

gen cons_phcut1=cons_ph
sum cons_phcut1, detail
replace cons_phcut1=. if cons_phcut1>`r(p99)'

gen cons_phcut5=cons_ph
sum cons_phcut5, detail
replace cons_phcut5=. if cons_phcut5>`r(p95)'

gen cons_phcut10=cons_ph
sum cons_phcut10, detail
replace cons_phcut10=. if cons_phcut10>`r(p90)'

gen cons_phcut25=cons_ph
sum cons_phcut25, detail
replace cons_phcut25=. if cons_phcut25>`r(p75)'


** Water consumption is normalized by dividing it into the average control group consumption at baseline and multiplying by 100.
egen cons_cont=mean(cons_ph), by(post treated_1)
replace cons_cont=. if post==1
replace cons_cont=. if treated_1==0
replace cons_cont = cons_cont[2] if missing(cons_cont)
label var cons_cont "Average control at baseline"

sort wss t
gen cons_adj = (cons_ph/cons_cont)*100
label var cons_adj "Normalized consumption"

// Distribution of consumption per user: original, fixed, and adjusments.
/*
// Distribution with original consumption and users.

qnorm cons_po, color(maroon%20) ytitle(cubic meters/user) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_hh_original.pdf", replace

// Distribution after first adjustment. 
qnorm cons_pf, color(maroon%20) ytitle(cubic meters/user) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_hh_fixed.pdf", replace
 
// Distribution after the second adjustment. 
qnorm cons_ph, color(maroon%20) ytitle(cubic meters/user) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_hh_fixed_intrang.pdf", replace

// Distribution after cutting the right-tail.
qnorm cons_phcut1, color(maroon%20) ytitle(cubic meters/user) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_hh_fixed1.pdf", replace

qnorm cons_phcut5, color(maroon%20) ytitle(cubic meters/user) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_hh_fixed5.pdf", replace

qnorm cons_phcut10, color(maroon%20) ytitle(cubic meters/user) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_hh_fixed10.pdf", replace

qnorm cons_phcut25, color(maroon%20) ytitle(cubic meters/user) xsize(4.6)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/cons_hh_fixed25.pdf", replace


label var cons_phcut1  "m3/user"
label var cons_phcut5  "m3/user"
label var cons_phcut10  "m3/user"
label var cons_phcut25  "m3/user"

*/


**********************************************************
*** Distance to the artifitial border Variables.
**********************************************************

// Distance to the border over time. There are three distance 
// measures according to each moment of the policy. Here I determine a dynamic
// variable called "dist_border" in kilometers. 

gen dist_border = dist_border_1 
replace dist_border = dist_border_2 if  time >= 1
replace dist_border = dist_border_3 if  time >= 4 

// Distance, treatment and time indicator "distance_treated. distance_treated# 
// is a variable that indicates treatment for each moment of the policy.
// Positive values indicate that the WSS is treated. 

gen dist_treat_border = dist_treat_border_1
replace dist_treat_border_1= dist_treat_border_2 if time >= 1
replace dist_treat_border_1= dist_treat_border_3 if time >= 4


label var dist_border "Distance to the border (km)"
label var dist_treat_border "Distance to the border (km)"
label var dist_mag_km "Distance to Magdalena river (km)"

**********************************************************
*** Geographic units index 
**********************************************************

// Department and municipality identificator.
egen department = group(dpto)
egen municipality = group(mpio)
egen month_dpto= group(month  department)
egen mpio_dpto= group(municipality  department)

label var department "Department" 
label var municipality "Municipality" 
label var month_dpto "Month & Department"
label var mpio_dpto "Municipality & Department"

drop dpto mpio
rename department dpto
rename municipality mpio

**********************************************************
*** Variables at the organization level. 
**********************************************************

// Combining original data base of water consumption and number of users 
// (by SES) with variables about the organizations that manage the aqueducts. 
// The source of the incormation for both datasets is SUPERSERVICIOS.

merge m:1 id_wss using "~/Documents/Research/Natural Capital & Water/more tree better water/organizations.dta"

replace org_type=type_org if  org_type==.
drop type_org order_org clasif_org
rename  org_type type_org
rename  org_loc loc_org
rename org_state state_org
rename  org_year_start start_org

drop if _merge==2
drop _merge

// Type of Provider variables. There are four types: stated based, local governmets community based, private based.

tab type_org, gen (typeorg)
label var typeorg1 "State-owned company"
label var typeorg2 "Local Government"
label var typeorg3 "Community-based"
label var typeorg4 "Private Corporation"
label var typeorg5 "Private use"

label var n_sub "Subsidized users (\%)"

**********************************************************
** Monetary variables conversion
**********************************************************

// Converting variables in colombian pesos (COP) into US dollars (USD)
gen bill_cons1=(bill_cons/3249.75)
drop bill_cons
rename bill_cons1 bill_cons
label var bill_cons "Aquedutct Income (000 USD)"

gen bill_cons1=(bill_cons_fix/3249.75)
drop bill_cons_fix
rename bill_cons1 bill_cons_fix
label var bill_cons_fix "Aquedutct Income (000 USD)"

// Water rate
gen bill_po = bill_cons/cons
label var  bill_po "USD/m3/User"
gen bill_pf = bill_cons_fix/cons_fix
label var  bill_pf  "USD/m3/User"

// Number of digits checking and Interquartile range extreme values

gen bill_ph=bill_pf
label var  bill_ph "USD/m3/User"

gen bill_p_round = round(bill_ph)
tostring bill_p_round, gen(bill_p_st)
gen counter_bill_p=strlen(bill_p_st) 
replace counter_bill_p=. if bill_p_round==.
replace bill_ph=. if counter_bill_p>2

bys wss: egen p25_bill_ph = pctile(bill_ph), p(25)
bys wss: egen p75_bill_ph = pctile(bill_ph), p(75)
gen int_quart_bill_ph = p75_bill_ph - p25_bill_ph
replace bill_ph = . if int_quart_bill_ph == 0

gen in_fen_bill_ph=int_quart_bill_ph*2.5
gen low_limit_bill_ph=p25-in_fen_bill_ph
gen up_limit_bill_ph=p75+in_fen_bill_ph
replace bill_ph=. if bill_ph>=up_limit_bill_ph

sum bill_ph, detail
replace bill_ph=. if bill_ph>`r(p99)'

drop bill_p_round bill_p_st counter_bill_p p75_bill_ph p25_bill_ph int_quart_bill_ph in_fen_bill_ph low_limit_bill_ph up_limit_bill_ph

**********************************************************
** Group Income-Heterogeneity
**********************************************************

***  Herfindahl-Hirschman Index calculation

forval i=1/6{
	gen ne`i'per = (ne`i'/nt)
}
gen hhi = (1-(ne1per^2 + ne2per^2 + ne3per^2 + ne4per^2 + ne5per^2 + ne6per^2))

/*
preserve
**** HHI
local bin = 20
local normal_begin = 0.15
local normal_end = 1
local range ra(`normal_begin' `normal_end') 
local colors navy maroon black 
local call 

levelsof treated_1 
tokenize "`r(levels)'" 
local nlevels : word count `r(levels)' 

forval j = 1/`nlevels' { 
    su hhi if treated_1 == ``j'' 
    scalar mu`j' = r(mean)
    scalar sd`j' = r(sd)

    local color : word `j' of `colors' 
    local call `call' || histogram hhi if treated_1 == ``j'', bcolor(`color'%45) bin(`bin')
    local call `call' || function normalden(x, mu`j', sd`j') , `range' lcolor(`color'%90) 
} 
twoway `call'   ///
legend(order(3 "Affected" 1 "Not Affected") position(1) row(1) ring(0) size(medium))  ytitle(Density) xsize(8)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/hh_index_hist.pdf", replace


qplot hhi, over(treated_1) trscale(invnormal(@)) xtitle(standard normal) legend(order(2 "Affected" 1 "Not Affected") ring(0) pos(11) col(2) size(medium)) yla(, ang(h))  ytitle(Herfindahl-Hirschman Index)  xtitle(Standard Normal) xsize(8)
graph export "~/Dropbox/Aplicaciones/Overleaf/Stick approach Paper/Figures/hh_index_qplot.pdf", replace

*/


*************************************************************

*** Adjusting Melgar: This municipality appear as non-treated_1
// even if when it's located in a treated department.   

replace treated_1 = 1 if id_mpio== 73449
replace treated_2 = 1 if id_mpio== 73449

replace dist_treat_border_1 = dist_treat_border_1*-1  if id_mpio== 73449
replace dist_treat_border_2 = dist_treat_border_2*-1  if id_mpio== 73449


*******************************************
** Window of time indicators
*******************************************
gen sample_time = 0
replace sample_time = 1 if time  >= -8 & time < 8
label var sample_time"16 months"

*************************************************************
*** Municipal Data from CEDE municipality panel (UNIANDES)
*************************************************************
merge m:m id_mpio year using "var_cede.dta"
drop if _merge !=3
drop _merge

label var pobl_urb "Urban Population" 
label var pobl_tot "Total Population" 
label var indrural "Rurality Index"
label var areaoficialkm2 "Municipality area (km2)"
label var altura "Altitude (m)"
label var nbi "UBN Index"
label var IPM "MP Index"
label var y_corr "Municipality Income (COP)"
label var TMI "IMR"
label var tacued "Aqueduct access (\%)"
label var taseo "Sanitation access (\%)"
label var talcan "Sewage access (\%)"
label var dismdo "Dist. to regional market (km)"

gen y_corr_usd=y_corr/3249.75
label var y_corr_usd "Municipality income (USD)"

replace altura=2522 if altura==25221

global cede pobl_urb pobl_tot indrural areaoficialkm2 altura dismdo nbi IPM y_corr TMI tacued taseo talcan y_corr_usd



***************************************************************
** Panel
***************************************************************

egen td=group(timead dpto)
egen tm=group(timead mpio)


egen mt=group( mpio timead)
egen dt=group( dpto timead)



global var_count cons_adj rainfall temperature users_fix hhi n_sub bill_per

foreach v in $var_count{
	gen count_`v' = 1 if `v' == . & count_obs==16
	replace count_`v' = 0 if count_`v' ==. & count_obs==16
}

gen count = count_cons_adj +  count_rainfall + count_temperature +count_users_fix+  count_hhi + count_n_sub + count_bill_per

bys wss: egen count_ob=count(time)

egen my=group(month year)

xtset wss t

keep if count==0 // Strong balanced panel

keep if count_ob==16 // 8 periods pre and 8 post

eststo clear
global controlvar rainfall temperature users_fix hhi n_sub bill_per
xtreg cons_ph  fines  i.t $controlvar dist_border , fe

xtreg cons_ph  fines  i.month i.year $controlvar dist_border if type_org==1, fe

xtreg cons_ph  i.treated_1##i.t  i.month i.year $controlvar dist_border , fe

coefplot, drop(rainfall temperature bill_pf _cons users_fix 1.fines ) 
 
***************************************************************
** Summary Stats and differences in mean
***************************************************************

drop objectid subsidy contribution 

replace rainfall = . if rainfall== 0
gen year_start = 2014-start_org

// Saving dataset average at baseline
preserve
keep if time < 0
global prevars wss dist_border_1 dist_border_2 dist_border_3 temperature rainfall hhi  n_sub dist_mag_km treated_1 cons_po users_fix cons_fix cons_pf cons_phcut1 cons_phcut5 cons_phcut10 cons_ph year_start type_org loc_org typeorg1 typeorg2 typeorg3 typeorg4 typeorg5  bill_ph pobl_urb pobl_tot indrural areaoficialkm2 altura dismdo nbi IPM TMI tacued taseo talcan y_corr_usd

keep $prevars time

foreach v in $prevars{
	rename `v' `v'm
	bys wss: egen `v'=mean(`v'm)
	drop `v'm
}

keep if time==-1
drop time 
export delimited using "~/Documents/GitHub/stick_approach/data_elnino_pre.csv", nolabel replace
restore

//do "summary_statistics.do"


*********************************************
***  Saving dataset
*********************************************

drop $cede 

sort wss time
export delimited using "~/Documents/GitHub/stick_approach/data_elnino_panel.csv", nolabel replace


*****************************************************
*** Distribution of variables by treatment status
*****************************************************


/*

*********************************************
***  water consumption over time by groups
*********************************************


preserve
local varname cons_ph
local group  treated_1
local time time
collapse (mean) y = `varname' (semean) se_y = `varname', by(`group' `time')
gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y
sort `group'
twoway  (rarea yu  yl time if treated_1 == 0   & time<0, color(navy%30) lpattern(solid) lcolor(navy%30) mcolor(%30)) (line y time if treated_1 == 0 & time<0, lcolor(navy) lpattern(solid))  (scatter y time  if treated_1 == 0 &  time<0, msymbol(d) mcolor(navy) msize(vsmall)) ///
(rarea yu  yl time if treated_1 == 1  &  time<0, color(maroon%30) lpattern(solid) lcolor(maroon%30) )  (line y time if treated_1 == 1   &  time<0, lcolor(maroon) lpattern(solid)) (scatter y time  if treated_1 == 1  &  time<0, msymbol(s) mcolor(maroon) msize(vsmall)),  ///
ytitle(Cubic meters) xtitle(Time) ///
legend(order(1 "95% CI Control" 3 "Mean Control" 4 "95% CI Treated Once" 6 "Mean Treated Once") position(1) row(2) ring(0))
//graph export cons_group_over_time.tif, replace
restore

**** Rainfall
preserve
local varname rainfall
local group  treated_1
local time time
collapse (mean) y = `varname' (semean) se_y = `varname', by(`group' `time')
gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y
sort `group'
twoway  (rarea yu  yl time if treated == 0 & time<0, color(navy%30) lpattern(solid) lcolor(navy%30) mcolor(%30)) (line y time if treated == 0 & time<0, lcolor(navy) lpattern(solid))  (scatter y time  if treated == 0  & time<0, msymbol(d) mcolor(navy) msize(vsmall)) ///
(rarea yu  yl time if treated == 1  & time<0, color(maroon%30) lpattern(solid) lcolor(maroon%30) )  (line y time if treated == 1  & time<0, lcolor(maroon) lpattern(solid)) (scatter y time  if treated == 1 & time<0 , msymbol(s) mcolor(maroon) msize(vsmall)),  ///
ytitle(mm/month) xtitle(Time) ///
xline(0) xline(3) xline(10) ///
legend(order(1 "95% CI Control" 3 "Mean Control" 4 "95% CI Treated Once" 6 "Mean Treated Once") position(1) row(2) ring(0))
//graph export rainfall_ group_over_time.tif, replace
restore

**** Temperature
preserve
local varname temperature
local group  treated_1
local time time
collapse (mean) y = `varname' (semean) se_y = `varname', by(`group' `time')
gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y
sort `group'
twoway  (rarea yu  yl time if treated == 0  & time<0, color(navy%30) lpattern(solid) lcolor(navy%30) mcolor(%30)) (line y time if treated == 0 & time<0, lcolor(navy) lpattern(solid))  (scatter y time  if treated == 0  & time<0, msymbol(d) mcolor(navy) msize(vsmall)) ///
(rarea yu  yl time if treated == 1  & time<0, color(maroon%30) lpattern(solid) lcolor(maroon%30) )  (line y time if treated == 1  & time<0, lcolor(maroon) lpattern(solid)) (scatter y time  if treated == 1  & time<0 , msymbol(s) mcolor(maroon) msize(vsmall)),  ///
ytitle(ºC/month) xtitle(Time) ///
xline(0) xline(3) xline(10) ///
legend(order(1 "95% CI Control" 3 "Mean Control" 4 "95% CI Treated Once" 6 "Mean Treated Once") position(1) row(2) ring(0))
//graph export temperature_ group_over_time.tif, replace
restore

*/

*******************************************
** Graphical analysis at the border
*******************************************

* I expected that the water consumption decrease at the cutoff because of the fines policy.
/*
*** Aggregate sample
twoway (scatter cons_ph dist_treated_1 if time<0, msymbol(dh) msize(small) ///
 mcolor(grey%20)) ///
 (lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1<0, lcolor(navy) ///
 lwidth(0.3) lpattern(solid) ) ///
 (lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1>0, lcolor(maroon) ///
 lwidth(0.3) lpattern(solid) ) , ///
 legend(order(1 "Consumption" 2 "Fit Control" 3 "Fit Treated") ///
 pos(1) col(1) ring(0)) ///
 ytitle(m3/user)  ///
 title(Panel A: Aggregate Sample (Linear),  pos(11) size(medium))  ///
 xline(0, lcolor(black) lpattern(dash))
 //graph export discon_overall.tif, replace
 
 twoway (scatter cons_ph dist_treated_1 if time>=0, msymbol(dh) msize(small) mcolor(grey%20)) ///
 (qfit cons_ph dist_treated_1 if time>=0 & dist_treated_1<0, lcolor(navy) lwidth(0.3) lpattern(solid) ) (qfit cons_ph dist_treated_1 if time>=0 & dist_treated_1>0, lcolor(maroon) lwidth(0.3) lpattern(solid) ) , legend(order(1 "Consumption" 2 "Fit Control" 3 "Fit Treated") pos(1) col(1) ring(0)) ytitle(Cubic meters)  title(Panel A: Aggregate Sample (Quadratic),  pos(11) size(medium)) xline(0, lcolor(black) lpattern(dash))
 graph export discon_overall_quadratic.tif, replace
 **
 
***  First Quartile
 twoway (scatter cons_ph dist_treated_1 if time>=0 & dist_treated_1>-25 & dist_treated_1<25, msymbol(dh) msize(small) mcolor(grey%20)) ///
(lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1<0 & dist_treated_1>-25, lcolor(navy) lwidth(0.3) lpattern(solid)) ///
(lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1>0 & dist_treated_1<25, lcolor(maroon) lwidth(0.3) lpattern(solid) ) , legend(order(1 "Consumption" 2 "Fit Control" 3 "Fit Treated") pos(1) col(1) ring(0)) ytitle(Cubic meters)  title(Panel B: Bandwith 25 kilometers (Linear), pos(11) size(medium)) xline(0, lcolor(black) lpattern(dash))
 graph export discon_km_Q1.tif, replace
 
  twoway (scatter cons_ph dist_treated_1 if time>=0 & dist_treated_1>-25 & dist_treated_1<25, msymbol(dh) msize(small) mcolor(grey%20)) ///
(qfit cons_ph dist_treated_1 if time>=0 & dist_treated_1<0 & dist_treated_1>-25, lcolor(navy) lwidth(0.3) lpattern(solid)) ///
(qfit cons_ph dist_treated_1 if time>=0 & dist_treated_1>0 & dist_treated_1<25, lcolor(maroon) lwidth(0.3) lpattern(solid) ) , legend(order(1 "Consumption" 2 "Fit Control" 3 "Fit Treated") pos(1) col(1) ring(0)) ytitle(Cubic meters)  title(Panel B: Bandwith 25 kilometers (Quadratic), pos(11) size(medium)) xline(0, lcolor(black) lpattern(dash))
 graph export discon_km_Q1_quadratic.tif, replace
 **
***Second Quartile
twoway (scatter cons_ph dist_treated_1 if time>=0 & dist_treated_1>-50 & dist_treated_1<50, msymbol(dh) msize(small) mcolor(grey%50)) ///
(lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1<0 & dist_treated_1>-50, lcolor(navy) lwidth(0.3) lpattern(solid) ) ///
(lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1>0 & dist_treated_1<50, lcolor(maroon) lwidth(0.3) lpattern(solid) ) , legend(order(1 "Consumption" 2 "Fit Control" 3 "Fit Treated") pos(1) col(1) ring(0)) ytitle(Cubic meters) title(Panel C:  Bandwith 50 kilometers (Linear),  pos(11) size(medium)) xline(0, lcolor(black) lpattern(dash))
 graph export discon_km_Q2.tif, replace
 
 twoway (scatter cons_ph dist_treated_1 if time>=0 & dist_treated_1>-50 & dist_treated_1<50, msymbol(dh) msize(small) mcolor(grey%50)) ///
(qfit cons_ph dist_treated_1 if time>=0 & dist_treated_1<0 & dist_treated_1>-50, lcolor(navy) lwidth(0.3) lpattern(solid) ) ///
(qfit cons_ph dist_treated_1 if time>=0 & dist_treated_1>0 & dist_treated_1<50, lcolor(maroon) lwidth(0.3) lpattern(solid) ) , legend(order(1 "Consumption" 2 "Fit Control" 3 "Fit Treated") pos(1) col(1) ring(0)) ytitle(Cubic meters) title(Panel C: Bandwith 50 kilometers (Quadratic),  pos(11) size(medium)) xline(0, lcolor(black) lpattern(dash))
 graph export discon_km_Q2_quadratic.tif, replace
 **

 *** Third Quartile
twoway (scatter cons_ph dist_treated_1 if time>=0 & dist_treated_1>-90 & dist_treated_1<90, msymbol(dh) msize(small) mcolor(grey%50)) ///
(lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1<0 & dist_treated_1>-90, lcolor(navy) lwidth(0.3) lpattern(solid) ) ///
(lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1>0 & dist_treated_1<90, lcolor(maroon) lwidth(0.3) lpattern(solid) ) , legend(order(1 "Consumption" 2 "Fit Control" 3 "Fit Treated") pos(1) col(1) ring(0)) ytitle(Cubic meters) title(Panel D: Bandwith 90 kilometers (Linear), pos(11) size(medium) ) xline(0, lcolor(black) lpattern(dash))
 graph export discon_km_Q3.tif, replace
 
 twoway (scatter cons_ph dist_treated_1 if time>=0 & dist_treated_1>-90 & dist_treated_1<90, msymbol(dh) msize(small) mcolor(grey%50)) ///
(qfit cons_ph dist_treated_1 if time>=0 & dist_treated_1<0 & dist_treated_1>-90, lcolor(navy) lwidth(0.3) lpattern(solid) ) ///
(qfit cons_ph dist_treated_1 if time>=0 & dist_treated_1>0 & dist_treated_1<90, lcolor(maroon) lwidth(0.3) lpattern(solid) ) , legend(order(1 "Consumption" 2 "Fit Control" 3 "Fit Treated") pos(1) col(1) ring(0)) ytitle(Cubic meters) title(Panel D:  Bandwith 90 kilometers (Quadratic), pos(11) size(medium) ) xline(0, lcolor(black) lpattern(dash))
 graph export discon_km_Q3quadratic.tif, replace
 
 **
 */
 
 ** Differences in observable characteristics
 ** By treatment status
 **
global  control_initial  temperature users_fix  contribution subsidy  bill_cons n_sub  i.month i.year
global cede pobl_tot  indrural  y_corr tacued taseo talcan TMI

/*
global vars cons_fix users_fix bill_cons ne1per ne2per ne3per ne4per ne5per ne6per   typeorg1 typeorg2 typeorg3 typeorg4 rural_org  subsidy n_sub contribution ig ih  temperature rainfall  pobl_tot pobl_urb indrural areaoficialkm2 altura dismdo nbi IPM y_corr TMI tacued taseo talcan 

label var ig "Gini index"
label var ih "Herfindahl-Hirshman index"
label var cons "Consumption (Group)"
 
sort id_wss time

eststo clear
forvalues d=25(25)250 {
estpost ttest $vars $cede if time==-1 & distance_1<=`d' , by(treated) unequal
eststo  km`d'
}

esttab km25 km50 km75 km100 km125 km150 km175 km200 km225 km250  using diff_mean_border.tex, replace label  nonumbers  cell(" b(label(Diff.) star fmt(a2)) ") mtitle star(* 0.10 ** 0.05 *** 0.01) booktabs gaps compress noobs 

*/
 
*******************************************
** Common trends assumption
*******************************************
//do "common_trends_check.do"


** Measure of users variable by type of organization
gen users_pub= users_fix if  type_org==1
gen users_loc= users_fix if  type_org==2
gen users_com= users_fix if  type_org==3
gen users_pri= users_fix if  type_org==4


su users_pub, detail
gen sam_users_pub=0
replace  sam_users_pub= 1 if users_pub<=`r(p95)'
replace sam_users_pub= 0 if users_pub<=`r(p5)'
gen users_pub_cut = users_pub if sam_users_pub

su users_loc, detail
gen sam_users_loc=0
replace  sam_users_loc= 1 if users_loc<=`r(p95)'
replace sam_users_loc= 0 if users_loc<=`r(p5)'
gen users_loc_cut = users_loc if sam_users_loc

su users_com, detail
gen sam_users_com=0
replace  sam_users_com= 1 if users_com<=`r(p95)'
replace sam_users_com= 0 if users_com<=`r(p5)'
gen users_com_cut = users_com if sam_users_com

su users_pri, detail
gen sam_users_pri=0
replace  sam_users_pri= 1 if users_pri<=`r(p95)'
replace sam_users_pri= 0 if users_pri<=`r(p5)'
gen users_pri_cut = users_pri if sam_users_pri


*******************************************
** Regressions
*******************************************

//do " reg_initial.do"

******************************************

