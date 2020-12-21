

xtreg cons_po  post fines  users_fix  bill_cons  distance dist_sqrt  dist_cub  i.month i.year i.period , fe vce(cluster month_dpto) nonest


xtreg cons_po  post fines  distance dist_sqrt  dist_cub i.month i.year i.period , fe vce(cluster month_dpto) nonest



distance dist_sqrt   dist_cub


xtreg cons_ph  post fines temperature rainfall $cede  i.month i.year i.period, fe vce(cluster month_dpto) nonest  // coef .1670056 pval 0.049 

xtreg cons_ph  post fines  distance dist_sqrt i.month i.year i.period, fe  // coef .1670056 pval 0.054 

 




forvalues d=0/2 {
eststo clear
xtreg cons_p post fines  if sample_time_`d', fe  
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom 1.fines
matrix A = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln A= beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo A

xtreg cons_p treated##post  $control_initial $cede if sample_time_`d', fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.post#1.treated
matrix B = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln B = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo B

xtreg cons_p treated##post  if distance<=5 & sample_time_`d' , fe  
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom 1.post#1.treated
matrix C = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln C = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo C
xtreg cons_p treated##post   $control_initial  $cede  if dist_mag_km<=5  & sample_time_`d', fe vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.post#1.treated
matrix D = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln D = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo D

xtreg cons_p treated##post   if dist_mag_km<=10 & sample_time_`d', fe 
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom 1.post#1.treated
matrix E = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln E = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo E
xtreg cons_p treated##post  $control_initial $cede  if dist_mag_km<=10 & sample_time_`d', fe vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.post#1.treated
matrix F = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln F= beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo F

xtreg cons_p treated##post    if dist_mag_km<=20  & sample_time_`d' , fe 
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom 1.post#1.treated
matrix G = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln G = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo G
xtreg cons_p treated##post  $control_initial $cede  if dist_mag_km<=20 & sample_time_`d', fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.post#1.treated
matrix H = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln H = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo H

xtreg cons_p treated##post   if dist_mag_km<=30 & sample_time_`d' , fe 
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom 1.post#1.treated
matrix I = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln I = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo I
xtreg cons_p treated##post  $control_initial $cede   if dist_mag_km<=30 & sample_time_`d', fe vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.post#1.treated
matrix J = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln J = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo J
xtreg cons_p treated##post    if dist_mag_km<=50  & sample_time_`d', fe  
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom 1.post#1.treated
matrix K = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln K = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo K
xtreg cons_p treated##post  $control_initial $cede  if dist_mag_km<=50 & sample_time_`d', fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.post#1.treated
matrix L = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln L = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo L

esttab B D F H J L using  reg_treat_effect_time_`d'.tex, keep(1.post 1.treated#1.post  _cons) coeflabel(1.post  "After" 1.treated#1.post "Treated once X After"   _cons "Constant") stats(wssfe timefe control N_g N vce, labels("WSS FE"  "Time FE"  "Controls" "\# WSS" "Obs.") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  replace booktabs gap compress mtitle("All" "<5km" "<10km" "<20km" "<30km" "<50km" "All" "<5km" "<10km" "<20km" "<30km" "<50km" )
}



eststo clear
forvalues j=0/2 {
forvalues d = 25(25)250{
xtreg cons_ph   post##treated $control_initial $cede if dist_mag_km<=`d' & sample_time_`j', fe  vce(cluster month_dpto) nonest 
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.post#1.treated
matrix  coef`j'`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln coef`j'`d' = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo coef`j'`d' 
}
}


forvalues j=0/2 {
matrix coef=[coef`j'25\coef`j'50\coef`j'75\coef`j'100\coef`j'125\ coef`j'150 \coef`j'175 \coef`j'200 \coef`j'225\coef`j'250]
matrix rown coef =  "5" "50" "75" "100" "125" "150" "175" "200" "225" "250" 
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] dir[,7])) aux(coef[,8]) msymbol(d)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) lcolor(*.1  *.2  *.4) ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(dash) lcolor(black%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(10) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Direct"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border (km)) ytitle(Cubic meters) title(Effect of fines, pos(11) size(large))
graph export effect_border_km_time_`j'.tif, replace
}



coef`j'55\coef`j'60\coef`j'65\coef`j'70\coef`j'75\coef`j'80]

"55" "60" "65" "70" "75" "80"

** Effects By Type of organizations
*** Public
eststo clear
forvalues j=0/2 {
forvalues d = 15(5)80{
xtreg cons_p i.treated##i.post $control_initial $cede  if distance_1<=`d'  & sample_time_`j' & type_org==1, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.post#1.treated
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln coef`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo coef`d' 
}
}

forvalues j=0/2 {
matrix coef=[coef`j'5\coef`j'10\coef`j'15\coef`j'20\coef`j'25\ coef`j'30 \coef`j'35 \coef`j'40 \coef`j'45\coef`j'50\coef`j'55\coef`j'60\coef`j'65\coef`j'70\coef`j'75\coef`j'80]
matrix rown coef =  "5" "10" "15" "20" "25" "30" "35" "40" "45" "50" "55" "60" "65" "70" "75" "80"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] dir[,7])) aux(coef[,8]) msymbol(d)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) lcolor(*.1  *.2  *.4) ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(dash) lcolor(black%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(10) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Direct"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border(km)) ytitle(Cubic meters) title(Panel A: Effect of fines in Public Corporations, pos(11) size(medium))
graph export effect_border_km_public_time_`j'.tif, replace
}


*** Local
eststo clear
forvalues d = 5(5)80{
xtreg cons_p i.treated##i.post $control_initial $cede  if distance_1<=`d'  & type_org==2, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.post#1.treated
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln coef`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo coef`d' 
}

matrix coef=[coef5\coef10\coef15\coef20\coef25\ coef30 \coef35 \coef40 \coef45\coef50\coef55\coef60\coef65\coef70\coef75\coef80]
matrix rown coef ="5km" "10km" "15km" "20km" "25km" "30km" "35km" "40km" "45km" "50km" "55km" "60km" "65km" "<70km" "75km" "80km"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] dir[,7])) aux(coef[,8]) msymbol(d)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) lcolor(*.1  *.2  *.4) ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(dash) lcolor(black%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(10) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Direct"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border) ytitle(Cubic meters) title(Panel B: Effect of fines in Local Governments, pos(11) size(medium))
graph export effect_border_km_local.tif, replace

*** Community
forvalues d = 5(5)80{
xtreg cons_p i.treated##i.post $control_initial $cede  if distance_1<=`d'  & type_org==3, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.post#1.treated
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln coef`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo coef`d' 
}

matrix coef=[coef5\coef10\coef15\coef20\coef25\ coef30 \coef35 \coef40 \coef45\coef50\coef55\coef60\coef65\coef70\coef75\coef80]
matrix rown coef =   "5km" "10km" "15km" "20km" "25km" "30km" "35km" "40km" "45km" "50km" "55km" "60km" "65km" "<70km" "75km" "80km"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] dir[,7])) aux(coef[,8]) msymbol(d)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) lcolor(*.1  *.2  *.4) ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(dash) lcolor(black%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(10) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Direct"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border) ytitle(Cubic meters) title(Panel C: Effect of fines in Community Organizations, pos(11) size(medium))
graph export effect_border_km_community.tif, replace

*** Private
forvalues d = 5(5)80{
xtreg cons_p i.treated##i.post  $control_initial $cede  if distance_1<=`d'  & type_org==4, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom 1.post#1.treated
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln coef`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo coef`d' 
}

matrix coef=[coef5\coef10\coef15\coef20\coef25\ coef30 \coef35 \coef40 \coef45\coef50\coef55\coef60\coef65\coef70\coef75\coef80]
matrix rown coef =   "5km" "10km" "15km" "20km" "25km" "30km" "35km" "40km" "45km" "50km" "55km" "60km" "65km" "<70km" "75km" "80km"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] dir[,7])) aux(coef[,8]) msymbol(d)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) lcolor(*.1  *.2  *.4) ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(dash) lcolor(black%90) lwidth(0.1))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(10) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Direct"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border) ytitle(Cubic meters) title(Panel D: Effect of fines in Private Corporations, pos(11) size(medium))
graph export effect_border_km_private.tif, replace



xtreg cons_p  1.treated#c.distance_1##i.post  $control_initial $cede  if distance_1<=100, fe  vce(cluster month_dpto) nonest


