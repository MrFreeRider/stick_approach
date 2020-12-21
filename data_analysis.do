***********************************************************************************
*** Paper: Applying the stick to excessive water use: Does the group size matters?
*** Initial analysis of data and adjusment
*** Author: Jose David Lopez-Rivas  
*** Doctoral Student in Economics
*** Universidad de los Andes
***********************************************************************************

clear all
cd"/Users/mrfreerider/Documents/Research/Stick approach/Paper"
use "/Users/mrfreerider/Documents/Research/Stick approach/Paper/data_merge.dta", clear


merge m:1 id_wss using "/Users/mrfreerider/Documents/Research/Natural Capital & Water/more tree better water/organizations.dta"
replace org_type=type_org if  org_type==.
drop type_org order_org clasif_org
rename  org_type type_org
rename  org_loc loc_org
rename org_state state_org
rename  org_year_start start_org
drop if _merge==2
drop _merge



** Consumo HH inicial (no adjustment)
gen cons_po=cons/users
label var  cons_po "Water consumption  (m3/HH)"


*** Generating Time Indicator variable normalized
rename time period
label var period "Time"
bys id_wss: generate time=_n
gen time_ad = time-43
label var time_ad "Time"
drop time
rename time_ad time
label var time "Time"


*** Generating post-intervention Indicator variable 
gen post = 0
replace post = 1  if time >= 0
label var post "After"
gen post1 = 0
replace post1=1 if time >= 0
label var post1 "After moment 1"
gen post2 = post1
replace post2=0 if time <= 3
label var post2 "After moment 2"
gen post3 = post1
replace post3=0 if time  <= 10
label var post3 "After moment 3"

label var post "After"
label var post1 "Moment 1"
label var post2 "Moment 2"
label var post3  "Moment 3"


**********************************************************
*** Treatment and distance  variable
***  Departments affected by water fine program over time
*** Moment 1 = Resolucion. CRA 605/2014  by the  Atlantico, Bolivar, Boyaca, Caldas, Cesar, La Guajira, Magdalena, Norte de santander, Quindio, Risaralda, Santander, y Tolima
*** Moment 2 =  res. CRA 699 10/2014, atlantico & bolivar removed
*** Moment 3 = res.CRA 714 05/2015, magdalena, la guajira & cesar remained

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

gen distance= distance_1 
label var distance "Distance (Km)"

**res. CRA 699 10/2014, atlantico & bolivar removed
replace fines = 0 if id_dpto == 8   & time >=3
replace fines = 0 if id_dpto == 13 & time>=3

replace distance=distance_2 if  time >=3

**res.CRA 714 05/2015, magdalena, la guajira & cesar remained
replace fines = 0 if id_dpto == 15 & time >=10
replace fines = 0 if id_dpto == 17 & time >=10
replace fines = 0 if id_dpto == 54 & time >=10
replace fines = 0 if id_dpto == 63 & time >=10
replace fines = 0 if id_dpto == 66 & time >=10
replace fines = 0 if id_dpto == 68 & time >=10
replace fines = 0 if id_dpto == 73 & time >=10

replace distance=distance_3 if  time >=10



gen distance_treated = dist_treated_1
replace distance_treated= dist_treated_2 if time >=3
replace distance_treated= dist_treated_3 if time >=10 


egen distance_quart= cut(distance), group(4)
gen distance_band = 50 * floor(distance/50) 
label var distance_band "Distance ranges"



*** Geographic units
egen department = group(dpto)
label var department "Department" 
egen municipality = group(mpio)
label var municipality "Municipality" 

*** Type of Provider
tab type_org, gen (typeorg)
label var typeorg1 "Public corporation"
label var typeorg2 "Local government"
label var typeorg3 " Community organization"
label var typeorg4 " Private Corporation"

*** Rural Provider
gen rural_org =1 if loc_org==0
replace rural_org = 0 if rural_org==.
label var rural_org "Located in a rural area"

*** % of users by SES
gen ne1per=(ne1/nt)*100
label var ne1per "Users in SES 1 (\%)"

gen ne2per=(ne2/nt)*100
label var ne2per "Users in SES 2 (\%)"

gen ne3per=(ne3/nt)*100
label var ne3per "Users in SES 3 (\%)"

gen ne4per=(ne4/nt)*100
label var ne4per "Users in SES 4 (\%)"

gen ne5per=(ne5/nt)*100
label var ne5per "Users in SES 5 (\%)"

gen ne6per=(ne6/nt)*100
label var ne6per "Users in SES 6 (\%)"

**
** Changing treated 
replace treated_1 =0 if treated_1 ==-1
replace treated_2 =0 if treated_2 ==-1
replace treated_3 =0 if treated_3 ==-1

gen treatment=treated_1 
replace treatment=treated_2 if time >=3
replace treatment=treated_3 if time >=10


*** Treated overall variable
gen treated = 0
replace treated = 1 if id_dpto == 8   
replace treated = 1 if id_dpto == 13 
replace treated = 1 if id_dpto == 15
replace treated = 1 if id_dpto == 17 
replace treated = 1 if id_dpto == 20
replace treated = 1 if id_dpto == 44
replace treated = 1 if id_dpto == 47
replace treated = 1 if id_dpto == 54
replace treated = 1 if id_dpto == 63
replace treated = 1 if id_dpto == 66
replace treated = 1 if id_dpto == 68
replace treated = 1 if id_dpto == 73

label var treated "Treated once"

*** Subsidy/contribution ratio
gen cont_sub=contribution/subsidy
replace  cont_sub=0 if contribution==. & subsidy>0
label var  cont_sub "Ratio contribution/subsidies"

*** label variables
label var ne1 "Users SES 1"
label var ne2 "Users SES 2"
label var ne3 "Users SES 3"
label var ne4 "Users SES 4"
label var ne5 "Users SES 5"
label var ne6 "Users SES 6"
label var n_sub "\% of users subsidized"
label var dist_mag_km "Distance to Magdalena River"

** deleting the outliers
gen bill_cons1=(bill_cons/3249.75)
drop bill_cons
rename bill_cons1 bill_cons
label var bill_cons "Income by consumption  (USD)"
sum bill_total, detail
replace bill_total=. if  bill_total==`r(max)'
gen bill_total1=(bill_total/3249.75)
drop bill_total
rename bill_total1 bill_total
label var bill_total "Total income (USD)"




gen contribution1= (contribution/3249.75)
drop contribution
rename contribution1 contribution
label var contribution "Contributions (USD)"

gen subsidy1= (subsidy/3249.75)
drop subsidy
rename subsidy1 subsidy
label var subsidy "Subsidies (USD)"



** Deleting atypical reports
global var  users cons
foreach d in  $var {
gen `d'_fix=`d'
tostring `d', gen(`d'_st)
gen counter_`d'=strlen(`d'_st) 
replace counter_`d'=. if `d'==.

bys wss: egen mean_counter_`d'=mean(counter_`d')
gen change_count_`d'=counter_`d'-mean_counter_`d'
bys wss: egen min_counter_`d'=min(counter_`d')
bys wss: egen max_counter_`d'=max(counter_`d')
gen diff_low_`d'=counter_`d'-min_counter_`d'
gen diff_up_`d'=max_counter_`d'-counter_`d'
replace `d'_fix=. if  diff_low_`d'>1
}



** Consumption HH without atypical reports
gen cons_pf=cons_fix/users_fix
//hist cons_pf, freq
//graph export cons_hh_fix.tif, replace

** Deleting atypical data in consumtion HH
gen cons_phcut1=cons_pf
sum cons_phcut1, detail
replace cons_phcut1=. if cons_phcut1>`r(p99)'
replace cons_phcut1=. if cons_phcut1<`r(p1)'

gen cons_phcut5=cons_pf
sum cons_phcut5, detail
replace cons_phcut5=. if cons_phcut5>`r(p95)'
replace cons_phcut5=. if cons_phcut5<`r(p5)'

gen cons_phcut10=cons_pf
sum cons_phcut10, detail
replace cons_phcut10=. if cons_phcut10>`r(p90)'
replace cons_phcut10=. if cons_phcut10<`r(p10)'

gen cons_phcut25=cons_pf
sum cons_phcut25, detail
replace cons_phcut25=. if cons_phcut25>`r(p75)'
replace cons_phcut25=. if cons_phcut25<`r(p25)'



gen bill_po=bill_cons/cons
label var  bill_po "Water rate  (\$/m3/HH)"
gen bill_ph=bill_cons/cons_fix
label var  bill_ph  "Water rate  (\$/m3/HH)"



/*
gen cons_p_round = round(cons_pf)
tostring cons_p_round, gen(cons_p_st)
gen counter_cons_p=strlen(cons_p_st) 
replace counter_cons_p=. if cons_p_round==.
replace cons_ph=. if counter_cons_p>2

bys wss: egen p25=pctile(cons_ph), p(25)
bys wss:  egen p75=pctile(cons_ph), p(75)
gen int_quart=p75 - p25
replace cons_ph=. if int_quart==0
gen in_fen=int_quart*1.5
gen low_limit=p25-in_fen
gen up_limit=p25+in_fen
replace cons_ph=. if cons_ph<=low_limit
drop cons_p_round cons_p_st p25 p75 int_quart in_fen


*** Dispersion cons fixed first stage and users to inspect visually the outliers
*** An error in the report with atypical 
scatter  cons_pf users if counter_cons_p==1 & cons < 10000
graph export cons_fix_1digits.png, replace
scatter  cons_pf users if counter_cons_p==2 
graph export cons_fix_2digits.png, replace
scatter  cons_pf users if counter_cons_p==3 & users <4000
graph export cons_fix_3digits.png, replace
scatter  cons_pf users if counter_cons_p==4 
graph export cons_fix_4digits.png, replace
scatter  cons_pf users if counter_cons_p==5 
graph export cons_fix_5digits.png, replace
scatter  cons_pf users if counter_cons_p==6 
graph export cons_fix_6digits.png, replace
*/



**Histogram of  three measures
su cons_po, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist cons_pf, xline( `m') percent  title(Panel A: Water consumption per user - Original report, pos(11) size(medium)) xtitle(Cubic meters)) 

su cons_pf, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist cons_pf, xline( `m') percent  title(Panel A: Water consumption per user - Original report, pos(11) size(medium)) xtitle(Cubic meters)) 
graph export cons_hh_original.tif , replace


su cons_phcut1, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist cons_phcut1, xline( `m') percent  title(Distribution of consumption - Cut at 1%, pos(11) size(medium)) xtitle(Water consumption (m3)))
graph export cons_hh_fixed1.tif , replace

su cons_phcut5, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist cons_phcut5,  xscale(range(5 45)) xline( `m') percent  title(Panel B: Water consumption per user - - Cut at 5%, pos(11) size(medium)) xtitle(Cubic meters))
graph export cons_hh_fixed5.tif , replace

su cons_phcut10, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist cons_phcut10, xline( `m') percent  title(Distribution of consumption - Cut at 10%, pos(11) size(medium)) xtitle(Water consumption (m3)) )
graph export cons_hh_fixed10.tif , replace

su cons_phcut25, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist cons_phcut25, xline( `m') percent  title(Distribution of consumption - Cut at 25%, pos(11) size(medium)) xtitle(Water consumption (m3)) )
graph export cons_hh_fixed25.tif , replace




label var cons_phcut5  "m3/user"
label var cons_pf  "m3/user"
label var cons_po  "m3/user"

*************************************************************

*** Adjusting Melgar  
replace treated_1 = 1 if id_mpio== 73449
replace treated_2 = 1 if id_mpio== 73449
replace dist_treated_1 = dist_treated_1*-1  if id_mpio== 73449
replace dist_treated_2 = dist_treated_2*-1  if id_mpio== 73449


*************************************************************
*** Merging CEDE Municipal Data
merge m:m id_mpio year using "var_cede.dta"
drop if _merge !=3
drop _merge

label var pobl_urb "Urban Population" 
label var pobl_tot "Total Population" 
label var indrural "Rurality Index"
label var areaoficialkm2 "Municipality area (km2)"
label var altura "Altitude (m.a.s.l.)"
label var nbi "Unsatisfied basic needs index"
label var IPM "Poverty Index"
label var y_corr "Municipality income"
label var TMI "Infant mortality rate"
label var tacued "Aqueduct coverage (\%)"
label var taseo "Sanitation coverage (\%)"
label var talcan "Sewage coverage (\%)"
label var dismdo "Distance to reg. market (km)"

gen y_corr_usd=y_corr/3249.75
label var y_corr_usd "Municipality income (USD)"

global allcede pobl_tot indrural areaoficialkm2 altura  nbi IPM y_corr_usd dismdo tacued taseo talcan TMI
*** To check the post variable are well determined
/*
preserve 
bys time: egen mean_post=mean(post1)
twoway line mean_post time if time>-24
graph export check_post1.png, replace
window manage close graph
bys time: egen mean_post2=mean(post2)
twoway line mean_post2 time if time>-24
graph export check_post2.png, replace
window manage close graph
bys time: egen mean_post3=mean(post3)
twoway line mean_post3 time if time>-24
graph export check_post3.png, replace
window manage close graph
restore
*/

*** scatter of distribution over time
/*
preserve 
twoway scatter bill_cons time, xsc(r(0 1))
graph export distribution_bill_cons.tif, replace
window manage close graph

twoway scatter bill_total time 
graph export distribution_bill_total.tif, replace
window manage close graph

twoway scatter contribution time 
graph export distribution_contribution.tif, replace
window manage close graph

twoway scatter subsidy time 
graph export distribution_subsidy.tif, replace
window manage close graph

twoway scatter cons_ph time 
graph export distribution_cons.tif, replace
window manage close graph

twoway scatter users time 
graph export distribution_users.tif, replace
window manage close graph
restore
*/
*****



***************************************************************
** Summary Stats and differences in mean
***************************************************************
//do "summary_statistics.do"

*******************************************
***Window of time
*******************************************
gen sample_time_1 = 0
replace sample_time_1 = 1 if time  >= -12 & time <= 12
label var sample_time_1"One year after/before"

gen sample_time_2 = 0
replace sample_time_2 = 1 if time  >= -24 & time <= 24
label var sample_time_2 "Two year after/before"

gen sample_time_0 = 0
replace sample_time_0 = 1 if time  >= -24
label var sample_time_0 "All observations"




*********************************************
***  Kernel density of distribution
*********************************************
/*
local bin = 20
local normal_begin = 0
local normal_end = 0.75
local range ra(`normal_begin' `normal_end') 
local colors maroon navy black 
local call 

levelsof treated 
tokenize "`r(levels)'" 
local nlevels : word count `r(levels)' 

forval j = 1/`nlevels' { 
    su ig if treated == ``j'' 
    scalar mu`j' = r(mean)
    scalar sd`j' = r(sd)

    local color : word `j' of `colors' 
    local call `call' || histogram ig if treated == ``j'', bcolor(`color'%45) bin(`bin')
    local call `call' || function normalden(x, mu`j', sd`j') , `range' lcolor(`color'%90) 
} 
twoway `call'   ///
legend(order(3 "Affected" 1 "Not Affected") position(1) row(1) ring(0) size(medium))  ytitle(Density) xtitle() title(Panel A: Gini Index, pos(11) size(large))
graph export gini_index_hist.tif, replace

qplot ig, over(treated) trscale(invnormal(@)) xtitle(standard normal) ytitle(Index) legend(order(2 "Treated" 1 "Control") ring(0) pos(11) col(1) size(medium)) yla(, ang(h))  title(Panel A: Gini Index, pos(11) size(large))
graph export gini_index_qplot.tif, replace


**** HHI
local bin = 20
local normal_begin = 0.15
local normal_end = 1
local range ra(`normal_begin' `normal_end') 
local colors maroon navy black 
local call 

levelsof treated 
tokenize "`r(levels)'" 
local nlevels : word count `r(levels)' 

forval j = 1/`nlevels' { 
    su ih if treated == ``j'' 
    scalar mu`j' = r(mean)
    scalar sd`j' = r(sd)

    local color : word `j' of `colors' 
    local call `call' || histogram ih if treated == ``j'', bcolor(`color'%45) bin(`bin')
    local call `call' || function normalden(x, mu`j', sd`j') , `range' lcolor(`color'%90) 
} 
twoway `call'   ///
legend(order(3 "Affected" 1 "Not Affected") position(1) row(1) ring(0) size(medium))  ytitle(Density) xtitle() title(Panel B: Herfindahl-Hirschman Index, pos(11) size(large))
graph export hh_index_hist.tif, replace

qplot ih, over(treated) trscale(invnormal(@)) xtitle(standard normal) ytitle(Index) legend(order(2 "Treated" 1 "Control") ring(0) pos(11) col(1) size(medium)) yla(, ang(h))  title(Panel B: Herfindahl-Hirshman Index, pos(11) size(large))
graph export hh_index_qplot.tif, replace

*/


/*

*********************************************
***  water consumption over time by groups
*********************************************


preserve
local varname cons_phcut5
local group  treated
local time time
collapse (mean) y = `varname' (semean) se_y = `varname', by(`group' `time')
gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y
sort `group'
twoway  (rarea yu  yl time if treated == 0  & time<0, color(navy%30) lpattern(solid) lcolor(navy%30) mcolor(%30)) (line y time if treated == 0 & time<0, lcolor(navy) lpattern(solid))  (scatter y time  if treated == 0 &  time<0, msymbol(d) mcolor(navy) msize(vsmall)) ///
(rarea yu  yl time if treated == 1  &  time<0, color(maroon%30) lpattern(solid) lcolor(maroon%30) )  (line y time if treated == 1   &  time<0, lcolor(maroon) lpattern(solid)) (scatter y time  if treated == 1  &  time<0, msymbol(s) mcolor(maroon) msize(vsmall)),  ///
ytitle(Cubic meters) xtitle(Time) ///
legend(order(1 "95% CI Control" 3 "Mean Control" 4 "95% CI Treated Once" 6 "Mean Treated Once") position(1) row(2) ring(0))
graph export cons_group_over_time.tif, replace
restore

**** Rainfall
preserve
local varname rainfall
local group  treated
local time time
collapse (mean) y = `varname' (semean) se_y = `varname', by(`group' `time')
gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y
sort `group'
twoway  (rarea yu  yl time if treated == 0  & time>=-24  & time<0, color(navy%30) lpattern(solid) lcolor(navy%30) mcolor(%30)) (line y time if treated == 0 & time>=-24  & time<0, lcolor(navy) lpattern(solid))  (scatter y time  if treated == 0 &  time>=-24  & time<0, msymbol(d) mcolor(navy) msize(vsmall)) ///
(rarea yu  yl time if treated == 1  &  time>=-24   & time<0, color(maroon%30) lpattern(solid) lcolor(maroon%30) )  (line y time if treated == 1   &  time>=-24  & time<0, lcolor(maroon) lpattern(solid)) (scatter y time  if treated == 1  &  time>=-24  & time<0 , msymbol(s) mcolor(maroon) msize(vsmall)),  ///
ytitle(mm/month) xtitle(Time) ///
xline(0) xline(3) xline(10) ///
legend(order(1 "95% CI Control" 3 "Mean Control" 4 "95% CI Treated Once" 6 "Mean Treated Once") position(1) row(2) ring(0))
graph export rainfall_ group_over_time.tif, replace
restore

**** Temperature
preserve
local varname temperature
local group  treated
local time time
collapse (mean) y = `varname' (semean) se_y = `varname', by(`group' `time')
gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y
sort `group'
twoway  (rarea yu  yl time if treated == 0  & time>=-24  & time<0, color(navy%30) lpattern(solid) lcolor(navy%30) mcolor(%30)) (line y time if treated == 0 & time>=-24  & time<0, lcolor(navy) lpattern(solid))  (scatter y time  if treated == 0 &  time>=-24  & time<0, msymbol(d) mcolor(navy) msize(vsmall)) ///
(rarea yu  yl time if treated == 1  &  time>=-24   & time<0, color(maroon%30) lpattern(solid) lcolor(maroon%30) )  (line y time if treated == 1   &  time>=-24  & time<0, lcolor(maroon) lpattern(solid)) (scatter y time  if treated == 1  &  time>=-24  & time<0 , msymbol(s) mcolor(maroon) msize(vsmall)),  ///
ytitle(ºC/month) xtitle(Time) ///
xline(0) xline(3) xline(10) ///
legend(order(1 "95% CI Control" 3 "Mean Control" 4 "95% CI Treated Once" 6 "Mean Treated Once") position(1) row(2) ring(0))
graph export temperature_ group_over_time.tif, replace
restore


preserve
local varname cons_ph
local group  fines
local time time
local distance distance_band
collapse (mean) y = `varname' (semean) se_y = `varname',  by(`group' `time' `distance')
gen yu = y + 1.96*se_y
gen yl = y - 1.96*se_y
sort `group'
twoway  (rarea yu  yl time if fines == 0  & time>=-24, color(navy%30) lpattern(solid) lcolor(navy%30) mcolor(%30)) (line y time if fines == 0 & time>=-24 & distance<=400, lcolor(navy) lpattern(solid))  (scatter y time  if fines == 0 &  time>=-24, msymbol(d) mcolor(navy) msize(vsmall)) || /// 
(rarea yu  yl time if fines == 1  &  time>=-24, color(maroon%30) lpattern(solid) lcolor(maroon%30) )  (line y time if fines == 1   &  time>=-24 , lcolor(maroon) lpattern(solid)) (scatter y time  if fines == 1  &  time>=-24 , msymbol(s) mcolor(maroon) msize(vsmall)),  by(distance_band) ///
ytitle(Cubic meters) xtitle(Time) ///
xline(0) xline(3) xline(10) ///
legend(order(1 "95% CI Control" 3 "Mean Control" 4 "95% CI Treated Once" 6 "Mean Treated Once") position(6) row(1) )
graph export cons_group_over_time_fines.tif, replace
restore

*/

*******************************************
** Graphical analysis at the cut off
*******************************************
* I expected that the water consumption decrease at the cutoff because of the fines policy.
/*
*** Aggregate sample
twoway (scatter cons_ph dist_treated_1 if time>=0, msymbol(dh) msize(small) mcolor(grey%20)) ///
 (lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1<0, lcolor(navy) lwidth(0.3) lpattern(solid) ) (lfit cons_ph dist_treated_1 if time>=0 & dist_treated_1>0, lcolor(maroon) lwidth(0.3) lpattern(solid) ) , legend(order(1 "Consumption" 2 "Fit Control" 3 "Fit Treated") pos(1) col(1) ring(0)) ytitle(m3/user)  title(Panel A: Aggregate Sample (Linear),  pos(11) size(medium))  xline(0, lcolor(black) lpattern(dash))
 graph export discon_overall.tif, replace
 
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

replace treated_1 =0 if treated_1 ==-1
replace treated_2 =0 if treated_2 ==-1
replace treated_3 =0 if treated_3 ==-1

*******************************************
** Regressions Fines on WC
*******************************************

egen month_dpto= group(month  department)
gen dist_sqrt= distance^2
gen dist_cub=distance^3

*** HH index 
gen ih1=(10000-(ne1per^2 + ne2per^2 + ne3per^2 + ne4per^2 + ne5per^2 + ne6per^2))/10000


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





//do " reg_initial.do"

******************************************

