***********************************************************************************
*** Paper: Applying the stick to excessive water use: Does the group size matters?
*** Regressions Fines and Group Size and Heterogeneity
*** Author: Jose David Lopez-Rivas  
*** Doctoral Student in Economics
*** Universidad de los Andes
***********************************************************************************


 ** Global of covariates
global controlvar1   rainfall temperature  bill_ph  i.period 

** Full Sample
eststo clear
xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.users_fix 
matrix A = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln A= beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo A

** Plot margins
margins, dydx(fines) at(users_fix=(0(120000)1200000))
marginsplot, yline(0, lpattern(1) lcolor(red)) xtitle(Group-size) recast(connected) plotopts(msize(vsmall)  msymbol(s) ) recastci(rarea) ciopt(color(navy%20) lcolor(%10))  xlabel( , labsize(vsmall))  title(Panel B: Average treatment cross-effect, pos(11) size(normal))
graph export effect_all_gs.tif, replace
window manage close graph

*******************************************
** Public Corporations
xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1  if type_org==1, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.users_fix 
matrix B = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln B = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo B


** Plot margins
su users_pub
margins, dydx(fines) at(users_fix=(0(120000)1200000))
marginsplot, yline(0, lpattern(1) lcolor(red)) xtitle(Group-size) recast(connected) plotopts(msize(vsmall)  msymbol(s) ) recastci(rarea) ciopt(color(navy%20) lcolor(%10)) xlabel( , labsize(vsmall))  title(Panel A: Average treatment cross-effect in Public corporations, pos(11) size(normal))
graph export effect_public_gs.tif, replace
window manage close graph

** local governments
xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1  if type_org==2, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.users_fix 
matrix C = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln C = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo C

su users_loc
margins, dydx(fines) at(users_fix=(0(850)17000))
eststo Cplot
marginsplot, yline(0, lpattern(1) lcolor(red)) xtitle(Group-size) recast(connected) plotopts(msize(vsmall)  msymbol(s) ) recastci(rarea) ciopt(color(navy%20) lcolor(%10)) xlabel( , labsize(vsmall))  title(Panel B: Average treatment cross-effect in Local-goverments providers, pos(11) size(normal))
graph export effect_loc_gs.tif, replace
window manage close graph

** Community
xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1  if type_org==3, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.users_fix 
matrix D = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln D= beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo D

su users_com
margins, dydx(fines) at(users_fix=(0(600)12000))
marginsplot, yline(0, lpattern(1) lcolor(red)) xtitle(Group-size) recast(connected) plotopts(msize(vsmall)  msymbol(s) ) recastci(rarea) ciopt(color(navy%20) lcolor(%10)) xlabel( , labsize(vsmall))  title(Panel C: Average treatment cross-effect in Community organizations, pos(11) size(normal))
graph export effect_com_gs.tif, replace
window manage close graph

** Private
xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1  if type_org==4, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.users_fix 
matrix E = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln E = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo E

su users_pri
margins, dydx(fines) at(users_fix=(0(130000)1300000))
marginsplot, yline(0, lpattern(1) lcolor(red)) xtitle(Group-size) recast(connected) plotopts(msize(vsmall)  msymbol(s) ) recastci(rarea) ciopt(color(navy%20) lcolor(%10)) xlabel( , labsize(vsmall)) title(Panel D: Average treatment cross-effect in Private corporations, pos(11) size(normal))
graph export effect_pri_gs.tif, replace
window manage close graph


*** Table of results
esttab  A B C D E   using  reg_users_effect.tex, replace keep(1.fines users_fix  1.fines#c.users_fix   _cons ) coeflabel(1.fines "Fines" users_fix "Group-size" 1.fines#c.users_fix "Fines X Group-size"  _cons "Constant")  stats(timefe wssfe climfe N_g N vce, labels("Time FE" "WSS FE"  "Climate Controls" "WSS" "Observations") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  booktabs gap compress   mlabels(none) ///
mgroups("All" "Public Corp." "Local Government" "Community" "Private Corp.", pattern( 1 1  1  1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))      nomtitle nobaselevels


*** Interaction Effects
matrix coef=[A\B\C\D\E ]
matrix rown coef =  "All" "Public Corp." "Local Government" "Community" "Private Corp."
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,8]) msymbol(d)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) lcolor(*.1  *.2  *.8) ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(solid) lcolor(red%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(10) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Direct"   ) pos(1) row(1) ring(0)  size(small)) xtitle(" ") ytitle(Cubic meters) title(Panel A: cross-effect of fines and group-size, pos(11) size(normal))
graph export effect_group_size.tif, replace




*******
*** Adjusting users distribution
eststo clear
xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1  if type_org==1 & sam_users_pub , fe  vce(cluster month_dpto) nonest
eststo A
su users_pub_cut
margins, dydx(fines) at(users_fix=(0(4500)90000))
marginsplot, yline(0, lpattern(1) lcolor(red)) xtitle(Group-size) recast(connected) plotopts(msize(vsmall)  msymbol(s) ) recastci(rarea) ciopt(color(navy%20) lcolor(%10)) xlabel( , labsize(vsmall))  title(Panel A: Average treatment cross-effect in Public corporations, pos(11) size(normal))
graph export effect_public_gs_sample.tif, replace


xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1  if type_org==2 & sam_users_loc , fe  vce(cluster month_dpto) nonest
eststo B
su users_loc_cut
margins, dydx(fines) at(users_fix=(0(250)2500))
marginsplot, yline(0, lpattern(1) lcolor(red)) xtitle(Group-size) recast(connected) plotopts(msize(vsmall)  msymbol(s) ) recastci(rarea) ciopt(color(navy%20) lcolor(%10)) xlabel( , labsize(vsmall))  title(Panel B: Average treatment cross-effect in Local-goverments providers, pos(11) size(normal))
graph export effect_loc_gs_sample.tif, replace


xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1  if type_org==3 & sam_users_com , fe  vce(cluster month_dpto) nonest
eststo C
su users_com_cut
margins, dydx(fines) at(users_fix=(0(250)2500))
marginsplot, yline(0, lpattern(1) lcolor(red)) xtitle(Group-size) recast(connected) plotopts(msize(vsmall)  msymbol(s) ) recastci(rarea) ciopt(color(navy%20) lcolor(%10)) xlabel( , labsize(vsmall))  title(Panel C: Average treatment cross-effect in Community organizations, pos(11) size(normal))
graph export effect_com_gs_sample.tif, replace

xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1  if type_org==4 & sam_users_pri , fe  vce(cluster month_dpto) nonest
eststo D
su users_pri_cut
margins, dydx(fines) at(users_fix=(0(400)40000))
marginsplot, yline(0, lpattern(1) lcolor(red)) xtitle(Group-size) recast(connected) plotopts(msize(vsmall)  msymbol(s) ) recastci(rarea) ciopt(color(navy%20) lcolor(%10)) xlabel( , labsize(vsmall)) title(Panel D: Average treatment cross-effect in Private corporations, pos(11) size(normal))
graph export effect_pri_gs_sample.tif, replace

*** Table of results
esttab  A B C D    using  reg_users_effect_cut.tex, replace keep(1.fines users_fix  1.fines#c.users_fix   _cons ) coeflabel(1.fines "Fines" users_fix "Group-size"  1.fines#c.users_fix "Fines X Group-size"  _cons "Constant")  stats(timefe wssfe climfe N_g N vce, labels("Time FE" "WSS FE"  "Climate Controls" "WSS" "Observations") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  booktabs gap compress   mlabels(none) ///
mgroups( "Public Corp." "Local Government" "Community" "Private Corp.", pattern(  1  1  1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))      nomtitle nobaselevels



*** Effects By Type of organizations
**** Border
eststo clear
forvalues i=1/4{
forvalues d = 25(25)250{
xtreg cons_phcut5  i.fines##c.users_fix  i.period $controlvar1 if distance<=`d' & type_org==`i' , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.users_fix 
matrix  coef`i'`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln coef`i'`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo coef`i'`d' 
}
}

forvalues i=1/4{
matrix coef=[coef`i'25\coef`i'50\coef`i'75\coef`i'100\coef`i'125\ coef`i'150 \coef`i'175 \coef`i'200 \coef`i'225\coef`i'250]
matrix rown coef =  "25" "50" "75" "100" "125" "150" "175" "200" "225" "250"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,8]) msymbol(d)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) lcolor(*.1  *.2  *.4) ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(dash) lcolor(black%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(10) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Direct"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border) ytitle(Cubic meters) title(Effect of fines and Group-size, pos(11) size(large))
graph export effect_gs_border_km_`i'.tif, replace
}





******************************************
** Fines and Gini Index
*****************************************
eststo clear
xtreg cons_phcut5  i.fines##c.ig  i.period $controlvar  , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ig 
matrix A = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln A= beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo A

xtreg cons_phcut5  i.fines##c.ig  i.period $controlvar  if type_org==1 , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ig 
matrix B = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln B = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo B

xtreg cons_phcut5  i.fines##c.ig  i.period $controlvar if type_org==2 , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ig 
matrix C = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln C = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo C

xtreg cons_phcut5  i.fines##c.ig  i.period $controlvar if type_org==3 , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ig 
matrix D = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln D = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo D


xtreg cons_phcut5  i.fines##c.ig  i.period $controlvar  if type_org==4, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ig 
matrix E = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln E = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo E


esttab  A B C  D E  using  reg_gini_effect.tex, keep(1.fines ig 1.fines#c.ig    _cons ) coeflabel(1.fines  "Fines" ig "Gini Index" 1.fines#c.ig     "Fines  X Gini Index"   _cons "Constant") stats(timefe wssfe climfe N_g N vce, labels("Time FE" "WSS FE"  "Climate Controls" "WSS" "Observations") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  replace booktabs gap compress   mlabels(none) ///
mgroups("All" "Public Corp." "Local Government" "Community" "Private Corp.", pattern( 1 1  1  1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))      nomtitle nobaselevels



matrix coef=[A\B\C\D\E ]
matrix rown coef = "All" "Public" "Local Government" "Community" "Private"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,8]) msymbol(d)  msize(small) recast(scatter) ciopts(recast(rcap, rcap, rcap) lcolor(*.1  *.2  *.4) ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(dash) lcolor(black%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(10) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Direct"   ) pos(1) row(1) ring(0)  size(small)) xtitle() ytitle(Cubic meters) title(Effect of the fines and Gini Index, pos(11) size(medium))
graph export effect_gini.tif, replace

***********************************************
**** HHI
***********************************************
xtreg cons_phcut5  i.fines##c.ih1  i.period $controlvar , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ih 1
matrix A1 = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln A1= beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo A

xtreg cons_phcut5  i.fines##c.ih1  i.period $controlvar  if type_org==1 , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ih1
matrix B1 = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln B1 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo B

xtreg cons_phcut5  i.fines##c.ih1  i.period $controlvar if type_org==2 , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ih1 
matrix C1 = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln C1 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo C

xtreg cons_phcut5  i.fines##c.ih1  i.period $controlvar if type_org==3 , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ih1
matrix D1 = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln D1 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo D


xtreg cons_phcut5  i.fines##c.ih1  i.period $controlvar  if type_org==4, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.fines +  1.fines#c.ih1
matrix E1= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln E1 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo E


esttab  A B C  D E  using  reg_hhi_effect.tex, keep(1.fines ih1 1.fines#c.ih1    _cons ) coeflabel(1.fines  "Fines" ih1 "HH Index" 1.fines#c.ih1     "Fines  X HH Index"   _cons "Constant") stats(timefe wssfe climfe N_g N vce, labels("Time FE" "WSS FE"  "Climate Controls" "WSS" "Observations") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  replace booktabs gap compress   mlabels(none) ///
mgroups("All" "Public Corp." "Local Government" "Community" "Private Corp.", pattern( 1 1  1  1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))      nomtitle nobaselevels


matrix coef1=[A1\B1\C1\D1\E1 ]
matrix rown coef1 =  "All" "Public" "Local Government" "Community" "Private"

coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,8]) msymbol(D)  msize(vsmall) recast(scatter) ciopts(recast(rcap, rcap, rcap) color(black%5  black%10 black%15)   ) levels(99, 95, 90)),  bylabel(Panel A: Gini Index) || (matrix (coef1[,1]), ci((coef1[,2] coef1[,3]) (coef1[,4] coef1[,5]) (coef1[,6] coef1[,7])) aux(coef1[,8]) msymbol(D)  msize(vsmall) recast(scatter) ciopts(recast(rcap, rcap, rcap) color(black%5  black%10 black%15)   ) levels(99, 95, 90)),   bylabel(Panel B: HH Index)  ///
xline(0, lpattern(1) lcolor(red%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(10) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Direct"   ) pos(1) row(1) ring(0)  size(small)) ytitle() xtitle(Cubic meters per month) 
graph export effect_hhi.tif, replace


su ih1 if type_org==1, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist ih1 if type_org==1, xline( `m') percent title(Panel A: HHI Public corporations, pos(11) size(medium)) xtitle(Cubic meters)) 
graph export hhi_public.tif, replace

su ih1 if type_org==2, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist ih1 if type_org==2, xline( `m') percent title(Panel A: HHI Public corporations, pos(11) size(medium)) xtitle(Cubic meters)) 
graph export hhi_local.tif, replace

su ih1 if type_org==3, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist ih1 if type_org==3, xline( `m') percent title(Panel A: HHI Public corporations, pos(11) size(medium)) xtitle(Cubic meters)) 
graph export hhi_community.tif, replace

su ih1 if type_org==4, detail
local m=r(mean)
local sd=r(sd)
local low = `m'-`sd'
local high=`m'+`sd'
twoway (hist ih1 if type_org==4, xline( `m') percent title(Panel A: HHI Public corporations, pos(11) size(medium)) xtitle(Cubic meters)) 
graph export hhi_private.tif, replace

*-----------------------------------------------------------------------
** HHI, Size and fines
xtreg cons_phcut5 i.fines##c.users_fix##c.ih1  i.period $controlvar1 , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix + 1.fines#c.ih1 +  1.fines#c.users_fix#c.ih
matrix coef1= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef1 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg1

xtreg cons_phcut5 i.fines##c.users_fix##c.ih1 i.period $controlvar1 if type_org==1, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix + 1.fines#c.ih1 +  1.fines#c.users_fix#c.ih
matrix coef2= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef2 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg2

xtreg cons_phcut5 i.fines##c.users_fix##c.ih1  i.period $controlvar1  if type_org==2, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix + 1.fines#c.ih1 +  1.fines#c.users_fix#c.ih
matrix coef3= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef3 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg3

xtreg cons_phcut5 i.fines##c.users_fix##c.ih1  i.period $controlvar1  if type_org==3,  fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix + 1.fines#c.ih1 +  1.fines#c.users_fix#c.ih
matrix coef4= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef4 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg4

xtreg cons_phcut5 i.fines##c.users_fix##c.ih1  i.period $controlvar1 if type_org==4, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix + 1.fines#c.ih1 +  1.fines#c.users_fix#c.ih
matrix coef5= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef5 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg5

esttab reg1 reg2 reg3 reg4 reg5  using  reg_treat_effect_combined.tex, keep(1.fines users_fix ih 1.fines#c.users_fix  1.fines#c.ih  1.fines#c.users_fix    1.fines#c.users_fix#c.ih   _cons) coeflabel( 1.fines "Fines" users_fix "Group-size" ih "HHI" 1.fines#c.users_fix  "Fines X Group-size"  1.fines#c.ih "Fines X HHI" 1.fines#c.users_fix#c.ih  "Fines  X Group-size X HHI "   _cons "Constant") stats(wssfe timefe control N_g N vce, labels("WSS FE"  "Time FE"  "Controls" "\# WSS" "Obs.") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  mgroups("All" "Public Corp." "Local Government" "Community" "Private Corp.", pattern( 1  1  1  1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))   replace booktabs gap compress nomtitle

matrix coef=[coef1\coef2\coef3\coef4\coef5]
matrix rown coef =  "All" "Public Corp." "Local-Government" "Community"  "Private Corp."
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,8]) msymbol(D)  msize(vsmall) recast(scatter) ciopts(recast(rcap, rcap, rcap) color(black%5  black%10 black%15)  ))   , vertical  ///
yline(0, lpattern(dash) lcolor(black%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(12) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Estimate"   ) pos(1) row(1) ring(0)  size(small)) xtitle() ytitle(Cubic meters) title( Effect of Fines X Size X Heterogeneity, pos(11) size(medium))
graph export effect_size_heterogeneity.tif, replace


