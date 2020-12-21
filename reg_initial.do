***********************************************************************************
*** Paper: Applying the stick to excessive water use: Does the group size matters?
*** Regressions fines and cross-effects
*** Author: Jose David Lopez-Rivas  
*** Doctoral Student in Economics
*** Universidad de los Andes
***********************************************************************************


xtreg cons_po   fines, fe //15.73396   0.121 
xtreg cons_pf   fines, fe //  .712656     0.893
xtreg cons_phcut1   fines, fe //  .6218116       0.222 
xtreg cons_phcut5   fines, fe //  -.2807794      0.000 
xtreg cons_phcut10   fines, fe //  -.151109        0.000 
xtreg cons_phcut25   fines, fe // -.0813964     0.024 


xtreg cons_po   fines rainfall , fe  //  20.39644     0.048
xtreg cons_pf   fines rainfall , fe //     1.100522  0.089
xtreg cons_phcut1   fines rainfall , fe //  .9158227       0.222 
xtreg cons_phcut5   fines rainfall , fe //  -.3718582  0.000 
xtreg cons_phcut10   fines rainfall , fe // -.2221066 0.000
xtreg cons_phcut25   fines rainfall , fe //  -.1238652   0.001 


xtreg cons_po   fines rainfall temperature , fe  //  26.67311      0.006
xtreg cons_pf   fines rainfall temperature , fe //    1.401464    0.652 
xtreg cons_phcut1   fines rainfall  temperature, fe //  .8218619        0.222 
xtreg cons_phcut5   fines rainfall  temperature, fe //  -.5450941    0.000 
xtreg cons_phcut10   fines rainfall temperature, fe // -.3649308 0.000
xtreg cons_phcut25   fines rainfall temperature, fe // -.2790769    0.001 

xtreg cons_po   fines rainfall temperature  users_fix, fe  //  .7709843      0.835 
xtreg cons_pf   fines rainfall temperature  users_fix , fe //    1.574694      0.612 
xtreg cons_phcut1   fines rainfall  temperature  users_fix , fe //  .928252     0.168  
xtreg cons_phcut5   fines rainfall  temperature  users_fix, fe //  -.538783   0.000 
xtreg cons_phcut10   fines rainfall temperature  users_fix, fe //  -.3579367 0.000
xtreg cons_phcut25   fines rainfall temperature  users_fix, fe // -.258755    0.00 

xtreg cons_po   fines rainfall temperature  users_fix bill_ph, fe  // 1.501341      0.635 
xtreg cons_pf   fines rainfall temperature  users_fix bill_ph, fe //     1.501341      0.632 
xtreg cons_phcut1   fines rainfall  temperature  users_fix bill_ph , fe //  .8841663   0.198  
xtreg cons_phcut5   fines rainfall  temperature  users_fix bill_ph, fe //  -.5140663    0.000 
xtreg cons_phcut10   fines rainfall temperature  users_fix bill_ph, fe //  -.3195223  0.000
xtreg cons_phcut25   fines rainfall temperature  users_fix bill_ph, fe // -.2344885     0.00

xtreg cons_po   fines rainfall temperature  users_fix bill_ph i.period, fe  // -.4012615 0.911 
xtreg cons_pf   fines rainfall temperature  users_fix bill_ph  i.period , fe //     -.4012615  0.911
xtreg cons_phcut1   fines rainfall  temperature  users_fix bill_ph  i.period , fe //   1.462963    0.063 
xtreg cons_phcut5   fines rainfall  temperature  users_fix bill_ph  i.period, fe //  -.1994144      0.002
xtreg cons_phcut10   fines rainfall temperature  users_fix bill_ph  i.period, fe //   -.0258273    0.616 
xtreg cons_phcut25   fines rainfall temperature  users_fix bill_ph  i.period, fe // .07906      0.089 


xtreg cons_po   fines  i.period, fe  // 14.64734  0.911 
xtreg cons_pf   fines  i.period , fe //      -.983842  0.866 
xtreg cons_phcut1   fines i.period , fe //   1.049066      0.062 
xtreg cons_phcut5   fines  i.period, fe //  .0600572       0.252 
xtreg cons_phcut10   fines  i.period, fe //   .1354409    0.001  
xtreg cons_phcut25   fines  i.period, fe // .163257     0.000


xtreg cons_po   fines rainfall temperature  users_fix bill_ph i.period, fe vce(cluster month_dpto) nonest // -.4012615  0.914 
xtreg cons_pf   fines rainfall temperature  users_fix bill_ph  i.period , fe vce(cluster month_dpto) nonest //     -.4012615   0.914 
xtreg cons_phcut1   fines rainfall  temperature  users_fix bill_ph  i.period , fe vce(cluster month_dpto) nonest //  1.462963    0.001
xtreg cons_phcut5   fines rainfall  temperature  users_fix bill_ph  i.period, fe vce(cluster month_dpto) nonest  //  -.1994144     0.024 
xtreg cons_phcut10   fines rainfall temperature  users_fix bill_ph  i.period, fe vce(cluster month_dpto) nonest  //   -.0258273    0.719 
xtreg cons_phcut25   fines rainfall temperature  users_fix bill_ph i.period, fe vce(cluster month_dpto) nonest  // .07906      0.175


*** Number Users 
bys fines: egen users_affected=sum(users_fix)
bys fines: egen users_affected1=sum(users_fix) if type_org==1
bys fines: egen users_affected2=sum(users_fix) if type_org==2
bys fines: egen users_affected3=sum(users_fix) if type_org==3
bys fines: egen users_affected4=sum(users_fix) if type_org==4


** Covariates for regressions
global controlvar rainfall temperature  users_fix bill_ph  

** Regressions
eststo clear
xtreg cons_phcut5   fines,  fe  //All  No adjusted
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom fines
matrix A = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln A= beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo A

xtreg cons_phcut5  fines  i.period $controlvar , fe  vce(cluster month_dpto) nonest // All Adjusted
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix B = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln B = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo B

xtreg cons_phcut5   fines  if type_org==1  , fe  //Public No adjusted
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom fines
matrix C = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln C = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo C

xtreg cons_phcut5   fines  i.period $controlvar  if type_org==1  , fe vce(cluster month_dpto) nonest //Public Adjusted
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix D = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln D = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo D

xtreg cons_phcut5   fines   if type_org==2 , fe //Local No adjusted
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom fines
matrix E = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln E = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo E

xtreg cons_phcut5   fines   i.period $controlvar if type_org==2 , fe vce(cluster month_dpto) nonest  //Local Adjusted
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix F = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln F= beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo F

xtreg cons_phcut5   fines    if type_org==3 , fe  //Community No adjusted
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom fines
matrix G = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln G = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo G

xtreg cons_phcut5   fines  i.period $controlvar  if type_org==3, fe  vce(cluster month_dpto) nonest //Community Adjusted
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix H = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln H = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo H

xtreg cons_phcut5   fines if type_org==4, fe //private No adjusted
estadd local wssfe Y
estadd local  timefe N
estadd local control N
lincom fines
matrix I = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln I = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo I

xtreg cons_phcut5   fines  i.period $controlvar   if type_org==4 , fe vce(cluster month_dpto) nonest //private Adjusted
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix J = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln J = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo J

esttab A B  D  F  H J  using  reg_treat_effect.tex, keep(fines _cons) coeflabel( fines "Fines "   _cons "Constant") stats(wssfe timefe control N_g N vce, labels("WSS FE"  "Time FE"  "Controls" "\# WSS" "Obs.") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  mgroups("All" "Public Corp." "Local Government" "Community" "Private Corp.", pattern( 1 0 1  1  1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))   replace booktabs gap compress nomtitle


**************************************************************************
*** Effects over time
**************************************************************************
eststo clear
 xtreg cons_phcut5 post i.treatment##i.period $controlvar  , fe
forvalues d=641/675{
lincom 1.treatment#`d'.period
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln coef`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo coef`d' 
}

matrix coef=[  coef641\coef642\coef643\coef644\coef645\coef646\coef647\coef648\coef649\coef650\ coef651\coef652\coef653\coef654\coef655\coef656\coef657\coef658\coef659\coef660\ coef661\coef662\coef663\coef664\coef665\coef666\coef667\coef668\coef669\coef670\ coef671\coef672\coef673\coef674\coef675 ]
matrix rown coef=  "-12" "-11" "-10" "-9" "-8" "-7" "-6" "-5" "-4" "-3" "-2""-1" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12" "13" "14" "15" "16" "17" "18" "19" "20" "21"  "24"  

coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,8]) msymbol(S) msize(vsmall) lpattern(4) lwidth(0.1) lcolor(black%80) recast(connected)  ciopts(recast(rcap, rcap, rcap) color(%10 %20  %30)) levels(99, 95, 90)), vertical  ///
yline(0, lpattern(1) lcolor(red%90) lwidth(0.1))  ///
mlabel( cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(6) mlabsize(*.9) ///
legend(order(1 "99%"  2 "95%" 3 "90%" 4 "Effect"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Time) ytitle(Cubic meters) ///
title(Panel A: Effect of fines over time, pos(11) size(medium)) ///
xline(13) xline(16) xline(26)

graph export effect_over_time.tif, replace


**************************************************************************
**Robustness Analyses
**************************************************************************

global cut 1 5 10 
eststo clear
foreach d in $cut{
xtreg cons_phcut`d' fines  i.period $controlvar , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix coef`d'= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d' = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'

xtreg cons_phcut`d' fines  i.period $controlvar if type_org==1, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix coef`d'_1= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_1 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_1

xtreg cons_phcut`d' fines  i.period $controlvar if type_org==2, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix coef`d'_2= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_2 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_2

xtreg cons_phcut`d' fines  i.period $controlvar if type_org==3, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix coef`d'_3= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_3 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_3

xtreg cons_phcut`d' fines  i.period $controlvar if type_org==4, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix coef`d'_4= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_4 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_4


}


matrix coef=[coef1\coef5\coef10]
matrix rown coef =  "1%" "5%" "10%" 

matrix coef_1=[coef1_1\coef5_1\coef10_1]
matrix rown coef_1 =  "1%" "5%" "10%"  

matrix coef_2=[coef1_2\coef5_2\coef10_2]
matrix rown coef_2 =  "1%" "5%" "10%"  

matrix coef_3=[coef1_3\coef5_3\coef10_3]
matrix rown coef_3 =  "1%" "5%" "10%" 

matrix coef_4=[coef1_4\coef5_4\coef10_4]
matrix rown coef_4 =  "1%" "5%" "10%"  


coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,8]) msymbol(i)  msize(small)  recast(bar)  barwidth(0.15)  ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) )citop  )  ///
(matrix (coef_1[,1]), ci((coef_1[,2] coef_1[,3]) (coef_1[,4] coef_1[,5]) (coef_1[,6] coef_1[,7])) aux(coef_1[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15)  ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop )  ///
 (matrix (coef_2[,1]), ci((coef_2[,2] coef_2[,3]) (coef_2[,4] coef_2[,5]) (coef_2[,6] coef_2[,7])) aux(coef_2[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15) ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop) ///
  (matrix (coef_3[,1]), ci((coef_3[,2] coef_3[,3]) (coef_3[,4] coef_3[,5]) (coef_3[,6] coef_3[,7])) aux(coef_3[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15) ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop ) ///
   (matrix (coef_4[,1]), ci((coef_4[,2] coef_4[,3]) (coef_4[,4] coef_4[,5]) (coef_4[,6] coef_4[,7])) aux(coef_4[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15) ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop ), vertical  ///
yline(0, lpattern(dash) lcolor(red%90) lwidth(0.05))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(0) mlabsize(*.9) mcolor(white) ///
legend(order(1 "All" 5 "Public Corp."  9 "Local-Government"  13 "Community" 17 "Private Corp."  2  "99%"  3 "95%" 4 "90%"   ) pos(1) row(2) ring(0)  size(vsmall)) ytitle(Cubic meters) title(Effects of fines in adjusted outcome distributions., pos(11) size(normal))
graph export effect_robust.tif, replace


*******
global cut 1 5 10 
eststo clear
foreach d in $cut{
xtreg cons_phcut`d' i.fines##c.users_fix  i.period $controlvar , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix
matrix coef`d'= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d' = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'

xtreg cons_phcut`d' i.fines##c.users_fix  i.period $controlvar if type_org==1, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix
matrix coef`d'_1= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_1 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_1

xtreg cons_phcut`d' i.fines##c.users_fix  i.period $controlvar if type_org==2, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix
matrix coef`d'_2= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_2 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_2

xtreg cons_phcut`d' i.fines##c.users_fix  i.period $controlvar if type_org==3, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix
matrix coef`d'_3= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_3 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_3

xtreg cons_phcut`d' i.fines##c.users_fix  i.period $controlvar if type_org==4, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.users_fix
matrix coef`d'_4= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_4 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_4


}


matrix coef=[coef1\coef5\coef10]
matrix rown coef =  "1%" "5%" "10%" 

matrix coef_1=[coef1_1\coef5_1\coef10_1]
matrix rown coef_1 =  "1%" "5%" "10%" 

matrix coef_2=[coef1_2\coef5_2\coef10_2]
matrix rown coef_2 =  "1%" "5%" "10%" 

matrix coef_3=[coef1_3\coef5_3\coef10_3]
matrix rown coef_3 =  "1%" "5%" "10%" 

matrix coef_4=[coef1_4\coef5_4\coef10_4]
matrix rown coef_4 =  "1%" "5%" "10%"  


coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,8]) msymbol(i)  msize(small)  recast(bar)  barwidth(0.15)  ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) )citop  )  ///
(matrix (coef_1[,1]), ci((coef_1[,2] coef_1[,3]) (coef_1[,4] coef_1[,5]) (coef_1[,6] coef_1[,7])) aux(coef_1[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15)  ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop )  ///
 (matrix (coef_2[,1]), ci((coef_2[,2] coef_2[,3]) (coef_2[,4] coef_2[,5]) (coef_2[,6] coef_2[,7])) aux(coef_2[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15) ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop) ///
  (matrix (coef_3[,1]), ci((coef_3[,2] coef_3[,3]) (coef_3[,4] coef_3[,5]) (coef_3[,6] coef_3[,7])) aux(coef_3[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15) ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop ) ///
   (matrix (coef_4[,1]), ci((coef_4[,2] coef_4[,3]) (coef_4[,4] coef_4[,5]) (coef_4[,6] coef_4[,7])) aux(coef_4[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15) ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop ), vertical  ///
yline(0, lpattern(dash) lcolor(red%90) lwidth(0.05))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(0) mlabsize(*.9) mcolor(white) ///
legend(order(1 "All" 5 "Public Corp."  9 "Local-Government"  13 "Community" 17 "Private Corp."  2  "99%"  3 "95%" 4 "90%"   ) pos(1) row(2) ring(0)  size(vsmall)) ytitle(Cubic meters) title(Panel A: Effects of fines and users in adjusted outcome distributions, pos(11) size(normal))
graph export effect_robust_users.tif, replace

****
global cut 1 5 10 
eststo clear
foreach d in $cut{
xtreg cons_phcut`d' i.fines##c.ih1  i.period $controlvar , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.ih1
matrix coef`d'= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d' = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'

xtreg cons_phcut`d' i.fines##c.ih1  i.period $controlvar if type_org==1, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.ih1
matrix coef`d'_1= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_1 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_1

xtreg cons_phcut`d' i.fines##c.ih1  i.period $controlvar if type_org==2, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.ih1
matrix coef`d'_2= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_2 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_2

xtreg cons_phcut`d' i.fines##c.ih1  i.period $controlvar if type_org==3, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.ih1
matrix coef`d'_3= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_3 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_3

xtreg cons_phcut`d' i.fines##c.ih1  i.period $controlvar if type_org==4, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom 1.fines + 1.fines#c.ih1
matrix coef`d'_4= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d'_4 = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'_4


}


matrix coef=[coef1\coef5\coef10]
matrix rown coef =  "1%" "5%" "10%" 

matrix coef_1=[coef1_1\coef5_1\coef10_1]
matrix rown coef_1 =  "1%" "5%" "10%" 

matrix coef_2=[coef1_2\coef5_2\coef10_2]
matrix rown coef_2 =  "1%" "5%" "10%" 

matrix coef_3=[coef1_3\coef5_3\coef10_3]
matrix rown coef_3 =  "1%" "5%" "10%" 

matrix coef_4=[coef1_4\coef5_4\coef10_4]
matrix rown coef_4 =  "1%" "5%" "10%"  


coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,8]) msymbol(i)  msize(small)  recast(bar)  barwidth(0.15)  ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) )citop  )  ///
(matrix (coef_1[,1]), ci((coef_1[,2] coef_1[,3]) (coef_1[,4] coef_1[,5]) (coef_1[,6] coef_1[,7])) aux(coef_1[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15)  ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop )  ///
 (matrix (coef_2[,1]), ci((coef_2[,2] coef_2[,3]) (coef_2[,4] coef_2[,5]) (coef_2[,6] coef_2[,7])) aux(coef_2[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15) ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop) ///
  (matrix (coef_3[,1]), ci((coef_3[,2] coef_3[,3]) (coef_3[,4] coef_3[,5]) (coef_3[,6] coef_3[,7])) aux(coef_3[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15) ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop ) ///
   (matrix (coef_4[,1]), ci((coef_4[,2] coef_4[,3]) (coef_4[,4] coef_4[,5]) (coef_4[,6] coef_4[,7])) aux(coef_4[,8]) msymbol(i)  msize(small) recast(bar)  barwidth(0.15) ciopts(recast(rcap, rcap, rcap) color(black%10  black%20  black%30) ) citop ), vertical  ///
yline(0, lpattern(dash) lcolor(red%90) lwidth(0.05))  ///
mlabel(string(@b, "%5.2f") + cond(@aux<.01, "***", cond(@aux<.05, "**", cond(@aux<.1, "*", "")))) mlabpos(0) mlabsize(*.9) mcolor(white) ///
legend(order(1 "All" 5 "Public Corp."  9 "Local-Government"  13 "Community" 17 "Private Corp."  2  "99%"  3 "95%" 4 "90%"   ) pos(1) row(2) ring(0)  size(vsmall)) ytitle(Cubic meters) title(Panel B: Effects of fines and income heterogeneity in adjusted outcome distributions, pos(11) size(normal))
graph export effect_robust_ih.tif, replace


**************************************************************************
** No consumption in 20 cubic meters
**************************************************************************

gen cons_no_20=cons_phcut5
replace cons_no_20=. if cons_phcut5==20


eststo clear
xtreg cons_no_20  fines  i.period $controlvar , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix coefall= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coefall= beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo regall

forval d=1/4{

xtreg cons_no_20  fines  i.period $controlvar  if  type_org==`d' , fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local control Y
lincom fines
matrix coef`d'= (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))))
matrix coln  coef`d' = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval
eststo reg`d'
}

esttab regall reg1 reg2 reg3 reg4   using  reg_treat_effect_no20.tex, keep(fines        _cons) coeflabel( fines "WSS affected"   _cons "Constant") stats(wssfe timefe control N_g N vce, labels("WSS FE"  "Time FE"  "Controls" "\# WSS" "Obs.") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  mgroups("All" "Public Corp." "Local Government" "Community" "Private Corp.", pattern( 1 0 1  1  1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))   replace booktabs gap compress nomtitle


**************************************************************************
** Robust: Effects by distance
**************************************************************************
*** Fines on Cooperation
eststo clear
forvalues d = 10(10)200{
xtreg cons_phcut5 fines  i.period $controlvar if distance<=`d' , fe  vce(cluster month_dpto) nonest 
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom fines
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))), e(N_g))
matrix coln coef`d' = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval N
eststo coef`d' 
}

matrix coef=[coef10\ coef20 \coef30 \coef40 \coef50 \coef60 \ coef70 \coef80 \coef90 \coef100 \ coef110 \coef120 \coef130 \coef140 \coef150 \coef160 \coef170 \coef180\ coef190 \coef200]
matrix rown coef = "10" "20" "30" "40" "50" "60" "70" "80" "90" "100" "110" "120" "130" "140" "150" "160" "170" "180" "190" "200"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,9]) msymbol(o)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) color(black%15  black%20 black%25)   ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(1) lcolor(red%90) lwidth(0.1))  ///
mlabel("("+string(@aux, "%5.0f") + ")" ) mlabpos(2) mlabsize(*.8) mcolor(black)  ///
legend(order(1 "99%"  2 "95%" 3 "90%"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border (km)) ytitle(Cubic meters per month) ///
title( Effect of fines on consumption, pos(11) size(normal))
graph export effect_border_km.tif, replace

** Table
esttab coef10 coef20 coef30 coef40 coef50 coef60  coef70 coef80 coef90 coef100  coef110 coef120 coef130 coef140 coef150 coef160  using  reg_treat_effect_distance.tex, keep(fines _cons) coeflabel( fines "Fines"   _cons "Constant") stats(wssfe timefe control N_g N vce, labels("WSS FE"  "Time FE"  "Controls" "\# WSS" "Obs.") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  replace booktabs compress mtitle("<10" "<20" "<30" "<40" "<50" "<60" "<70" "<80" "<90" "<100" "<10" "<120" "<130" "<140" "<150" "<160"  ) 


*** Robust:  Effects By Type of organizations
*** Public
eststo clear
forvalues d =  10(10)200{
xtreg cons_phcut5 fines  $controlvar i.period if distance <=`d'  & type_org==1, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom fines
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))), e(N_g))
matrix coln coef`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval N
eststo coef`d' 
}


matrix coef=[coef10\ coef20 \coef30 \coef40 \coef50 \coef60 \ coef70 \coef80 \coef90 \coef100 \ coef110 \coef120 \coef130 \coef140 \coef150 \coef160 \coef170 \coef180\ coef190 \coef200]
matrix rown coef = "10" "20" "30" "40" "50" "60" "70" "80" "90" "100" "110" "120" "130" "140" "150" "160" "170" "180" "190" "200"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,9]) msymbol(o)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) color(black%15  black%20 black%25)   ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(1) lcolor(red%90) lwidth(0.1))  ///
mlabel("("+string(@aux, "%5.0f") + ")" ) mlabpos(2) mlabsize(*.8) mcolor(black)  ///
legend(order(1 "99%"  2 "95%" 3 "90%"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border (km)) ytitle(Cubic meters per month) ///
title( Panel A: Effect of fines in public corporation systems, pos(11) size(normal))
graph export effect_border_km_public.tif, replace

** Table
esttab coef10 coef20 coef30 coef40 coef50 coef60  coef70 coef80 coef90 coef100  coef110 coef120 coef130 coef140 coef150 coef160 using  reg_treat_effect_distance_pub.tex, keep(fines _cons) coeflabel( fines "Fines"   _cons "Constant") stats(wssfe timefe control N_g N vce, labels("WSS FE"  "Time FE"  "Controls" "\# WSS" "Obs.") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  replace booktabs compress mtitle("<10" "<20" "<30" "<40" "<50" "<60" "<70" "<80" "<90" "<100" "<10" "<120" "<130" "<140" "<150" "<160"  ) 


*** Local
eststo clear
forvalues d = 10(10)200{
xtreg cons_phcut5 fines i.period $controlvar if distance <=`d'  & type_org==2, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom fines
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))), e(N_g))
matrix coln coef`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval N
eststo coef`d' 
}

matrix coef=[coef10\ coef20 \coef30 \coef40 \coef50 \coef60 \ coef70 \coef80 \coef90 \coef100 \ coef110 \coef120 \coef130 \coef140 \coef150 \coef160 \coef170 \coef180\ coef190 \coef200]
matrix rown coef = "10" "20" "30" "40" "50" "60" "70" "80" "90" "100" "110" "120" "130" "140" "150" "160" "170" "180" "190" "200"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,9]) msymbol(o)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) color(black%15  black%20 black%25)   ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(1) lcolor(red%90) lwidth(0.1))  ///
mlabel("("+string(@aux, "%5.0f") + ")" ) mlabpos(2) mlabsize(*.8) mcolor(black)  ///
legend(order(1 "99%"  2 "95%" 3 "90%"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border (km)) ytitle(Cubic meters per month) ///
 title(Panel B: Effect of fines in local government systems, pos(11) size(normal))
graph export effect_border_km_local.tif, replace

** Table
esttab coef10 coef20 coef30 coef40 coef50 coef60  coef70 coef80 coef90 coef100  coef110 coef120 coef130 coef140 coef150 coef160  using  reg_treat_effect_distance_loc.tex, keep(fines _cons) coeflabel( fines "Fines"   _cons "Constant") stats(wssfe timefe control N_g N vce, labels("WSS FE"  "Time FE"  "Controls" "\# WSS" "Obs.") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  replace booktabs compress mtitle("<10" "<20" "<30" "<40" "<50" "<60" "<70" "<80" "<90" "<100" "<10" "<120" "<130" "<140" "<150" "<160" ) 


*** Community
eststo clear
forvalues d =10(10)200{
xtreg cons_phcut5 fines  $controlvar i.period if distance <=`d'  & type_org==3, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom fines
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))), e(N_g))
matrix coln coef`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval N
eststo coef`d' 
}

matrix coef=[coef10\ coef20 \coef30 \coef40 \coef50 \coef60 \ coef70 \coef80 \coef90 \coef100 \ coef110 \coef120 \coef130 \coef140 \coef150 \coef160 \coef170 \coef180\ coef190 \coef200]
matrix rown coef = "10" "20" "30" "40" "50" "60" "70" "80" "90" "100" "110" "120" "130" "140" "150" "160" "170" "180" "190" "200"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,9]) msymbol(o)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) color(black%15  black%20 black%25)   ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(1) lcolor(red%90) lwidth(0.1))  ///
mlabel("("+string(@aux, "%5.0f") + ")" ) mlabpos(2) mlabsize(*.8) mcolor(black)  ///
legend(order(1 "99%"  2 "95%" 3 "90%"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border (km)) ytitle(Cubic meters per month) ///
title(Panel C: Effect of fines in community systems , pos(11) size(normal))
graph export effect_border_km_community.tif, replace

** Table
esttab coef10 coef20 coef30 coef40 coef50 coef60  coef70 coef80 coef90 coef100  coef110 coef120 coef130 coef140 coef150 coef160   using  reg_treat_effect_distance_com.tex, keep(fines _cons) coeflabel( fines "Fines"   _cons "Constant") stats(wssfe timefe control N_g N vce, labels("WSS FE"  "Time FE"  "Controls" "\# WSS" "Obs.") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  replace booktabs compress mtitle("<10" "<20" "<30" "<40" "<50" "<60" "<70" "<80" "<90" "<100" "<10" "<120" "<130" "<140" "<150" "<160" ) 


*** Private
forvalues d = 10(10)200{
xtreg cons_phcut5 fines  $controlvar i.period if distance <=`d'  & type_org==4, fe  vce(cluster month_dpto) nonest
estadd local  timefe Y
estadd local wssfe Y
estadd local climfe Y
lincom  fines
matrix  coef`d' = (r(estimate), r(estimate)-(2.576* r(se)), r(estimate)+(2.576* r(se)),r(estimate)-(1.96* r(se)), r(estimate)+(1.96* r(se)) , r(estimate)-(1.645* r(se)), r(estimate)+(1.645* r(se)), 2*ttail(r(df),abs(r(estimate)/r(se))) , e(N_g))
matrix coln coef`d'  = beta liminf99 limsup99 liminf95 limsup95 liminf90 limsup90 pval N
eststo coef`d' 
}

matrix coef=[coef10\ coef20 \coef30 \coef40 \coef50 \coef60 \ coef70 \coef80 \coef90 \coef100 \ coef110 \coef120 \coef130 \coef140 \coef150 \coef160 \coef170 \coef180\ coef190 \coef200]
matrix rown coef = "10" "20" "30" "40" "50" "60" "70" "80" "90" "100" "110" "120" "130" "140" "150" "160" "170" "180" "190" "200"
coefplot (matrix (coef[,1]), ci((coef[,2] coef[,3]) (coef[,4] coef[,5]) (coef[,6] coef[,7])) aux(coef[,9]) msymbol(o)  msize(small) recast(connected) ciopts(recast(rcap, rcap, rcap) color(black%15  black%20 black%25)   ) levels(99, 95, 90))   , vertical  ///
yline(0, lpattern(1) lcolor(red%90) lwidth(0.1))  ///
mlabel("("+string(@aux, "%5.0f") + ")" ) mlabpos(2) mlabsize(*.8) mcolor(black)  ///
legend(order(1 "99%"  2 "95%" 3 "90%"   ) pos(1) row(1) ring(0)  size(small)) xtitle(Distance to border (km)) ytitle(Cubic meters per month) ///
title(Panel D: Effect of fines in private corporation systems, pos(11) size(normal))
graph export effect_border_km_private.tif, replace

** Table
esttab coef10 coef20 coef30 coef40 coef50 coef60  coef70 coef80 coef90 coef100  coef110 coef120 coef130 coef140 coef150 coef160  using  reg_treat_effect_distance_pri.tex, keep(fines _cons) coeflabel( fines "Fines"   _cons "Constant") stats(wssfe timefe control N_g N vce, labels("WSS FE"  "Time FE"  "Controls" "\# WSS" "Obs.") layout( @ @ @ @ @)) star(* 0.10 ** 0.05 *** 0.01)  replace booktabs compress mtitle("<10" "<20" "<30" "<40" "<50" "<60" "<70" "<80" "<90" "<100" "<10" "<120" "<130" "<140" "<150" "<160"  ) 

xtset id_wss time
xtgee cons_phcut5 fines, fe
