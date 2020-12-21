***********************************************************************************
*** Paper: Applying the stick to excessive water use: Does the group size matters?
*** Parallel trends checking
*** Author: Jose David Lopez-Rivas  
*** Doctoral Student in Economics
*** Universidad de los Andes
***********************************************************************************

preserve
bys id_wss: generate time1=_n
eststo clear
forvalues d=1(1)43{
gen paralelas_treated=(treated==1 & time1>=`d')
gen paralelas_control=(treated==0 & time1>=`d')

areg cons_phcut5  paralelas_treated  i.period if time1<=43, absorb(id_wss)
estimates store consumo_`d'
drop paralelas*
}
/* Matrices con coeficientes*/
estimates table consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43, keep (paralelas_treated) star
matrix treated_coef=r(coef)

/*Los nombres de las columnas y filas*/
matrix parallel_trend_treat= J(43,2,.)
matrix rown parallel_trend_treat= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln parallel_trend_treat= Coef sd 

/* Matrices para de coeficientes para tendecias paralelas*/
forv d=1(1)43{
matrix parallel_trend_treat [`d', 1]=treated_coef[1,2*`d'-1]
matrix parallel_trend_treat [`d', 2]=treated_coef[1,2*`d']
}
/* Matrices de intervalos de confianza - Nombres*/
matrix treat_99= J(43,2,.)
matrix rown treat_99= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_99= LowerLimit UpperLimit
matrix treat_95= J(43,2,.)
matrix rown treat_95= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_95= LowerLimit UpperLimit
matrix treat_90= J(43,2,.)
matrix rown treat_90=consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_90= LowerLimit UpperLimit

/* Matrices de intervalos de confianza - Intervalos*/
forv d=1(1)43{
matrix treat_99[`d',1]= (parallel_trend_treat [`d', 1])-(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_99[`d',2]= (parallel_trend_treat [`d', 1])+(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',1]= (parallel_trend_treat [`d', 1])-(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',2]= (parallel_trend_treat [`d', 1])+(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',1]= (parallel_trend_treat [`d', 1])-(1.64*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',2]= (parallel_trend_treat [`d', 1])+(1.64*(sqrt(parallel_trend_treat [`d', 2])))
}
/*Todos los intervalos resumidos en una matriz - para graficar*/
matrix ci= [ treat_99, treat_95, treat_90 ]

/* Grafica de coeficientes*/
coefplot (matrix (parallel_trend_treat[,1]), ci((treat_99[,1] treat_99[,2])(treat_95[,1] treat_95[,2])(treat_90[,1] treat_90[,2]))), recast(connected) lwidth(*1) lcolor(black) msymbol(oh) msize(small) ciopts(recast(rarea, rarea, rarea) color(%10 %15  %20)) levels(99, 95, 90) vertical yline(0, lpattern(dash) lcolor(red%70)) ///
groups (headroom consumo_1 consumo_12="2011" consumo_13 consumo_24="2012" consumo_25 consumo_36="2013" consumo_37 consumo_43="2014", nogap) coeflabel(consumo_1="1" consumo_2="2" consumo_3="3" consumo_4="4" consumo_5="5" consumo_6="6" consumo_7="7" consumo_8="8" consumo_9="9" consumo_10="10"  consumo_11="11" consumo_12="12" consumo_13="1" consumo_14="2" consumo_15="3" consumo_16="4"consumo_17="5"consumo_18="6" consumo_19="7" consumo_20="8"  consumo_21="9" consumo_22 ="10" consumo_23="11" consumo_24="12" consumo_25="1" consumo_26 ="2"consumo_27="3" consumo_28="4" consumo_29="5"consumo_30="6" consumo_31="7" consumo_32="8" consumo_33="9" consumo_34="10" consumo_35="11" consumo_36="12" consumo_37="1" consumo_38="2" consumo_39="3" consumo_40="4" consumo_41="5" consumo_42="6" consumo_43="7", labsize(vsmall))  /// 
legend (order(1 "99% CI" 2 "95% CI" 3 "90% CI") pos(1) row(1) ring(0)) ytitle("Cubic meters")   title(Fasification Test, pos(11) size(large))
//graph export overall_test.tif, replace
restore

*******************************************
** 10 kilometers
preserve
bys id_wss: generate time1=_n
eststo clear
forvalues d=1(1)43{
gen paralelas_treated=(treated==1 & time1>=`d')
gen paralelas_control=(treated==0 & time1>=`d')

areg cons_phcut5  paralelas_treated  i.period if time1<=43 & distance_1<=10, absorb(id_wss)
estimates store consumo_`d'
drop paralelas*
}
/* Matrices con coeficientes*/
estimates table consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43, keep (paralelas_treated) star
matrix treated_coef=r(coef)

/*Los nombres de las columnas y filas*/
matrix parallel_trend_treat= J(43,2,.)
matrix rown parallel_trend_treat= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln parallel_trend_treat= Coef sd 

/* Matrices para de coeficientes para tendecias paralelas*/
forv d=1(1)43{
matrix parallel_trend_treat [`d', 1]=treated_coef[1,2*`d'-1]
matrix parallel_trend_treat [`d', 2]=treated_coef[1,2*`d']
}
/* Matrices de intervalos de confianza - Nombres*/
matrix treat_99= J(43,2,.)
matrix rown treat_99= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_99= LowerLimit UpperLimit
matrix treat_95= J(43,2,.)
matrix rown treat_95= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_95= LowerLimit UpperLimit
matrix treat_90= J(43,2,.)
matrix rown treat_90=consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_90= LowerLimit UpperLimit

/* Matrices de intervalos de confianza - Intervalos*/
forv d=1(1)43{
matrix treat_99[`d',1]= (parallel_trend_treat [`d', 1])-(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_99[`d',2]= (parallel_trend_treat [`d', 1])+(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',1]= (parallel_trend_treat [`d', 1])-(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',2]= (parallel_trend_treat [`d', 1])+(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',1]= (parallel_trend_treat [`d', 1])-(1.64*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',2]= (parallel_trend_treat [`d', 1])+(1.64*(sqrt(parallel_trend_treat [`d', 2])))
}
/*Todos los intervalos resumidos en una matriz - para graficar*/
matrix ci= [ treat_99, treat_95, treat_90 ]

/* Grafica de coeficientes*/
coefplot (matrix (parallel_trend_treat[,1]), ci((treat_99[,1] treat_99[,2])(treat_95[,1] treat_95[,2])(treat_90[,1] treat_90[,2]))), recast(connected) lwidth(*1) lcolor(black) msymbol(oh) msize(small) ciopts(recast(rarea, rarea, rarea) color(%10 %15  %20)) levels(99, 95, 90) vertical yline(0, lpattern(dash) lcolor(red%70)) ///
groups (headroom consumo_1 consumo_12="2011" consumo_13 consumo_24="2012" consumo_25 consumo_36="2013" consumo_37 consumo_43="2014", nogap) coeflabel(consumo_1="1" consumo_2="2" consumo_3="3" consumo_4="4" consumo_5="5" consumo_6="6" consumo_7="7" consumo_8="8" consumo_9="9" consumo_10="10"  consumo_11="11" consumo_12="12" consumo_13="1" consumo_14="2" consumo_15="3" consumo_16="4"consumo_17="5"consumo_18="6" consumo_19="7" consumo_20="8"  consumo_21="9" consumo_22 ="10" consumo_23="11" consumo_24="12" consumo_25="1" consumo_26 ="2"consumo_27="3" consumo_28="4" consumo_29="5"consumo_30="6" consumo_31="7" consumo_32="8" consumo_33="9" consumo_34="10" consumo_35="11" consumo_36="12" consumo_37="1" consumo_38="2" consumo_39="3" consumo_40="4" consumo_41="5" consumo_42="6" consumo_43="7", labsize(vsmall))  /// 
legend (order(1 "99% CI" 2 "95% CI" 3 "90% CI") pos(1) row(1) ring(0)) ytitle("Cubic meters")   title(Panel A: 10 kilometers Fasification Test, pos(11) size(large))
//graph export 5km_test.tif, replace
window manage close graph
restore


*******************************************
** 20 kilometers
preserve
bys id_wss: generate time1=_n
eststo clear
forvalues d=1(1)43{
gen paralelas_treated=(treated==1 & time1>=`d')
gen paralelas_control=(treated==0 & time1>=`d')

areg cons_phcut5  paralelas_treated $control_initial   i.period if time1<=43 & distance_1<=20, absorb(id_wss)
estimates store consumo_`d'
drop paralelas*
}
/* Matrices con coeficientes*/
estimates table consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43, keep (paralelas_treated) star
matrix treated_coef=r(coef)

/*Los nombres de las columnas y filas*/
matrix parallel_trend_treat= J(43,2,.)
matrix rown parallel_trend_treat= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln parallel_trend_treat= Coef sd 

/* Matrices para de coeficientes para tendecias paralelas*/
forv d=1(1)43{
matrix parallel_trend_treat [`d', 1]=treated_coef[1,2*`d'-1]
matrix parallel_trend_treat [`d', 2]=treated_coef[1,2*`d']
}
/* Matrices de intervalos de confianza - Nombres*/
matrix treat_99= J(43,2,.)
matrix rown treat_99= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_99= LowerLimit UpperLimit
matrix treat_95= J(43,2,.)
matrix rown treat_95= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_95= LowerLimit UpperLimit
matrix treat_90= J(43,2,.)
matrix rown treat_90=consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_90= LowerLimit UpperLimit

/* Matrices de intervalos de confianza - Intervalos*/
forv d=1(1)43{
matrix treat_99[`d',1]= (parallel_trend_treat [`d', 1])-(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_99[`d',2]= (parallel_trend_treat [`d', 1])+(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',1]= (parallel_trend_treat [`d', 1])-(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',2]= (parallel_trend_treat [`d', 1])+(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',1]= (parallel_trend_treat [`d', 1])-(1.64*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',2]= (parallel_trend_treat [`d', 1])+(1.64*(sqrt(parallel_trend_treat [`d', 2])))
}
/*Todos los intervalos resumidos en una matriz - para graficar*/
matrix ci= [ treat_99, treat_95, treat_90 ]

/* Grafica de coeficientes*/
coefplot (matrix (parallel_trend_treat[,1]), ci((treat_99[,1] treat_99[,2])(treat_95[,1] treat_95[,2])(treat_90[,1] treat_90[,2]))), recast(connected) lwidth(*1) lcolor(black) msymbol(oh) msize(small) ciopts(recast(rarea, rarea, rarea) color(%10 %15  %20)) levels(99, 95, 90) vertical yline(0, lpattern(dash) lcolor(red%70)) ///
groups (headroom consumo_1 consumo_12="2011" consumo_13 consumo_24="2012" consumo_25 consumo_36="2013" consumo_37 consumo_43="2014", nogap) coeflabel(consumo_1="1" consumo_2="2" consumo_3="3" consumo_4="4" consumo_5="5" consumo_6="6" consumo_7="7" consumo_8="8" consumo_9="9" consumo_10="10"  consumo_11="11" consumo_12="12" consumo_13="1" consumo_14="2" consumo_15="3" consumo_16="4"consumo_17="5"consumo_18="6" consumo_19="7" consumo_20="8"  consumo_21="9" consumo_22 ="10" consumo_23="11" consumo_24="12" consumo_25="1" consumo_26 ="2"consumo_27="3" consumo_28="4" consumo_29="5"consumo_30="6" consumo_31="7" consumo_32="8" consumo_33="9" consumo_34="10" consumo_35="11" consumo_36="12" consumo_37="1" consumo_38="2" consumo_39="3" consumo_40="4" consumo_41="5" consumo_42="6" consumo_43="7", labsize(vsmall))  /// 
legend (order(1 "99% CI" 2 "95% CI" 3 "90% CI") pos(1) row(1) ring(0)) ytitle("Cubic meters")   title(Panel B: 20 kilometers Fasification Test, pos(11) size(large))
//graph export 10km_test.tif, replace
//window manage close graph
restore



*******************************************
** 30 kilometers
preserve
bys id_wss: generate time1=_n
eststo clear
forvalues d=1(1)43{
gen paralelas_treated=(treated==1 & time1>=`d')
gen paralelas_control=(treated==0 & time1>=`d')

areg cons_phcut5  paralelas_treated  $control_initial  i.period  if time1<=43 & distance_1<=30, absorb(id_wss)
estimates store consumo_`d'
drop paralelas*
}
/* Matrices con coeficientes*/
estimates table consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43, keep (paralelas_treated) star
matrix treated_coef=r(coef)

/*Los nombres de las columnas y filas*/
matrix parallel_trend_treat= J(43,2,.)
matrix rown parallel_trend_treat= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln parallel_trend_treat= Coef sd 

/* Matrices para de coeficientes para tendecias paralelas*/
forv d=1(1)43{
matrix parallel_trend_treat [`d', 1]=treated_coef[1,2*`d'-1]
matrix parallel_trend_treat [`d', 2]=treated_coef[1,2*`d']
}
/* Matrices de intervalos de confianza - Nombres*/
matrix treat_99= J(43,2,.)
matrix rown treat_99= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_99= LowerLimit UpperLimit
matrix treat_95= J(43,2,.)
matrix rown treat_95= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_95= LowerLimit UpperLimit
matrix treat_90= J(43,2,.)
matrix rown treat_90=consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_90= LowerLimit UpperLimit

/* Matrices de intervalos de confianza - Intervalos*/
forv d=1(1)43{
matrix treat_99[`d',1]= (parallel_trend_treat [`d', 1])-(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_99[`d',2]= (parallel_trend_treat [`d', 1])+(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',1]= (parallel_trend_treat [`d', 1])-(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',2]= (parallel_trend_treat [`d', 1])+(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',1]= (parallel_trend_treat [`d', 1])-(1.64*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',2]= (parallel_trend_treat [`d', 1])+(1.64*(sqrt(parallel_trend_treat [`d', 2])))
}
/*Todos los intervalos resumidos en una matriz - para graficar*/
matrix ci= [ treat_99, treat_95, treat_90 ]

/* Grafica de coeficientes*/
coefplot (matrix (parallel_trend_treat[,1]), ci((treat_99[,1] treat_99[,2])(treat_95[,1] treat_95[,2])(treat_90[,1] treat_90[,2]))), recast(connected) lwidth(*1) lcolor(black) msymbol(oh) msize(small) ciopts(recast(rarea, rarea, rarea) color(%10 %15  %20)) levels(99, 95, 90) vertical yline(0, lpattern(dash) lcolor(red%70)) ///
groups (headroom consumo_1 consumo_12="2011" consumo_13 consumo_24="2012" consumo_25 consumo_36="2013" consumo_37 consumo_43="2014", nogap) coeflabel(consumo_1="1" consumo_2="2" consumo_3="3" consumo_4="4" consumo_5="5" consumo_6="6" consumo_7="7" consumo_8="8" consumo_9="9" consumo_10="10"  consumo_11="11" consumo_12="12" consumo_13="1" consumo_14="2" consumo_15="3" consumo_16="4"consumo_17="5"consumo_18="6" consumo_19="7" consumo_20="8"  consumo_21="9" consumo_22 ="10" consumo_23="11" consumo_24="12" consumo_25="1" consumo_26 ="2"consumo_27="3" consumo_28="4" consumo_29="5"consumo_30="6" consumo_31="7" consumo_32="8" consumo_33="9" consumo_34="10" consumo_35="11" consumo_36="12" consumo_37="1" consumo_38="2" consumo_39="3" consumo_40="4" consumo_41="5" consumo_42="6" consumo_43="7", labsize(vsmall))  /// 
legend (order(1 "99% CI" 2 "95% CI" 3 "90% CI") pos(1) row(1) ring(0)) ytitle("Cubic meters")   title(Panel C: 30 kilometers Fasification Test, pos(11) size(large))
//graph export 15km_test.tif, replace
//window manage close graph
restore


*******************************************
** 100 kilometers
preserve
bys id_wss: generate time1=_n
eststo clear
forvalues d=1(1)43{
gen paralelas_treated=(treated==1 & time1>=`d')
gen paralelas_control=(treated==0 & time1>=`d')

areg cons_phcut5  paralelas_treated $control   i.period if time1<=43 & distance_1<=50, absorb(id_wss)
estimates store consumo_`d'
drop paralelas*
}
/* Matrices con coeficientes*/
estimates table consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43, keep (paralelas_treated) star
matrix treated_coef=r(coef)

/*Los nombres de las columnas y filas*/
matrix parallel_trend_treat= J(43,2,.)
matrix rown parallel_trend_treat= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln parallel_trend_treat= Coef sd 

/* Matrices para de coeficientes para tendecias paralelas*/
forv d=1(1)43{
matrix parallel_trend_treat [`d', 1]=treated_coef[1,2*`d'-1]
matrix parallel_trend_treat [`d', 2]=treated_coef[1,2*`d']
}
/* Matrices de intervalos de confianza - Nombres*/
matrix treat_99= J(43,2,.)
matrix rown treat_99= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_99= LowerLimit UpperLimit
matrix treat_95= J(43,2,.)
matrix rown treat_95= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_95= LowerLimit UpperLimit
matrix treat_90= J(43,2,.)
matrix rown treat_90=consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_90= LowerLimit UpperLimit

/* Matrices de intervalos de confianza - Intervalos*/
forv d=1(1)43{
matrix treat_99[`d',1]= (parallel_trend_treat [`d', 1])-(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_99[`d',2]= (parallel_trend_treat [`d', 1])+(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',1]= (parallel_trend_treat [`d', 1])-(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',2]= (parallel_trend_treat [`d', 1])+(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',1]= (parallel_trend_treat [`d', 1])-(1.64*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',2]= (parallel_trend_treat [`d', 1])+(1.64*(sqrt(parallel_trend_treat [`d', 2])))
}
/*Todos los intervalos resumidos en una matriz - para graficar*/
matrix ci= [ treat_99, treat_95, treat_90 ]

/* Grafica de coeficientes*/
coefplot (matrix (parallel_trend_treat[,1]), ci((treat_99[,1] treat_99[,2])(treat_95[,1] treat_95[,2])(treat_90[,1] treat_90[,2]))), recast(connected) lwidth(*1) lcolor(black) msymbol(oh) msize(small) ciopts(recast(rarea, rarea, rarea) color(%10 %15  %20)) levels(99, 95, 90) vertical yline(0, lpattern(dash) lcolor(red%70)) ///
groups (headroom consumo_1 consumo_12="2011" consumo_13 consumo_24="2012" consumo_25 consumo_36="2013" consumo_37 consumo_43="2014", nogap) coeflabel(consumo_1="1" consumo_2="2" consumo_3="3" consumo_4="4" consumo_5="5" consumo_6="6" consumo_7="7" consumo_8="8" consumo_9="9" consumo_10="10"  consumo_11="11" consumo_12="12" consumo_13="1" consumo_14="2" consumo_15="3" consumo_16="4"consumo_17="5"consumo_18="6" consumo_19="7" consumo_20="8"  consumo_21="9" consumo_22 ="10" consumo_23="11" consumo_24="12" consumo_25="1" consumo_26 ="2"consumo_27="3" consumo_28="4" consumo_29="5"consumo_30="6" consumo_31="7" consumo_32="8" consumo_33="9" consumo_34="10" consumo_35="11" consumo_36="12" consumo_37="1" consumo_38="2" consumo_39="3" consumo_40="4" consumo_41="5" consumo_42="6" consumo_43="7", labsize(vsmall))  /// 
legend (order(1 "99% CI" 2 "95% CI" 3 "90% CI") pos(1) row(1) ring(0)) ytitle("Cubic meters")   title(Panel E: 50 kilometers Fasification Test, pos(11) size(large))
graph export 30km_test.tif, replace
window manage close graph
restore

** 50 kilometers
preserve
bys id_wss: generate time1=_n
eststo clear
forvalues d=1(1)43{
gen paralelas_treated=(treated==1 & time1>=`d')
gen paralelas_control=(treated==0 & time1>=`d')

areg cons_phcut5  paralelas_treated $control_initial  i.period  if time1<=43 & distance_1<=100, absorb(id_wss)
estimates store consumo_`d'
drop paralelas*
}
/* Matrices con coeficientes*/
estimates table consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43, keep (paralelas_treated) star
matrix treated_coef=r(coef)

/*Los nombres de las columnas y filas*/
matrix parallel_trend_treat= J(43,2,.)
matrix rown parallel_trend_treat= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln parallel_trend_treat= Coef sd 

/* Matrices para de coeficientes para tendecias paralelas*/
forv d=1(1)43{
matrix parallel_trend_treat [`d', 1]=treated_coef[1,2*`d'-1]
matrix parallel_trend_treat [`d', 2]=treated_coef[1,2*`d']
}
/* Matrices de intervalos de confianza - Nombres*/
matrix treat_99= J(43,2,.)
matrix rown treat_99= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_99= LowerLimit UpperLimit
matrix treat_95= J(43,2,.)
matrix rown treat_95= consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_95= LowerLimit UpperLimit
matrix treat_90= J(43,2,.)
matrix rown treat_90=consumo_1 consumo_2 consumo_3 consumo_4 consumo_5 consumo_6 consumo_7 consumo_8 consumo_9 consumo_10 consumo_11 consumo_12 consumo_13 consumo_14 consumo_15 consumo_16 consumo_17 consumo_18 consumo_19 consumo_20 consumo_21 consumo_22 consumo_23 consumo_24 consumo_25 consumo_26 consumo_27 consumo_28 consumo_29 consumo_30 consumo_31 consumo_32 consumo_33 consumo_34 consumo_35 consumo_36 consumo_37 consumo_38 consumo_39 consumo_40 consumo_41 consumo_42 consumo_43
matrix coln treat_90= LowerLimit UpperLimit

/* Matrices de intervalos de confianza - Intervalos*/
forv d=1(1)43{
matrix treat_99[`d',1]= (parallel_trend_treat [`d', 1])-(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_99[`d',2]= (parallel_trend_treat [`d', 1])+(2.58*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',1]= (parallel_trend_treat [`d', 1])-(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_95[`d',2]= (parallel_trend_treat [`d', 1])+(1.96*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',1]= (parallel_trend_treat [`d', 1])-(1.64*(sqrt(parallel_trend_treat [`d', 2])))
matrix treat_90[`d',2]= (parallel_trend_treat [`d', 1])+(1.64*(sqrt(parallel_trend_treat [`d', 2])))
}
/*Todos los intervalos resumidos en una matriz - para graficar*/
matrix ci= [ treat_99, treat_95, treat_90 ]

/* Grafica de coeficientes*/
coefplot (matrix (parallel_trend_treat[,1]), ci((treat_99[,1] treat_99[,2])(treat_95[,1] treat_95[,2])(treat_90[,1] treat_90[,2]))), recast(connected) lwidth(*1) lcolor(black) msymbol(oh) msize(small) ciopts(recast(rarea, rarea, rarea) color(%10 %15  %20)) levels(99, 95, 90) vertical yline(0, lpattern(dash) lcolor(red%70)) ///
groups (headroom consumo_1 consumo_12="2011" consumo_13 consumo_24="2012" consumo_25 consumo_36="2013" consumo_37 consumo_43="2014", nogap) coeflabel(consumo_1="1" consumo_2="2" consumo_3="3" consumo_4="4" consumo_5="5" consumo_6="6" consumo_7="7" consumo_8="8" consumo_9="9" consumo_10="10"  consumo_11="11" consumo_12="12" consumo_13="1" consumo_14="2" consumo_15="3" consumo_16="4"consumo_17="5"consumo_18="6" consumo_19="7" consumo_20="8"  consumo_21="9" consumo_22 ="10" consumo_23="11" consumo_24="12" consumo_25="1" consumo_26 ="2"consumo_27="3" consumo_28="4" consumo_29="5"consumo_30="6" consumo_31="7" consumo_32="8" consumo_33="9" consumo_34="10" consumo_35="11" consumo_36="12" consumo_37="1" consumo_38="2" consumo_39="3" consumo_40="4" consumo_41="5" consumo_42="6" consumo_43="7", labsize(vsmall))  /// 
legend (order(1 "99% CI" 2 "95% CI" 3 "90% CI") pos(1) row(1) ring(0)) ytitle("Cubic meters")   title(Panel D: 100 kilometers Fasification Test, pos(11) size(large))
//graph export 50km_test.tif, replace
//window manage close graph
restore

