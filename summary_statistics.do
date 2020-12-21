**********************************************************************
**Summarizing 
**********************************************************************

global vars users_fix  bill_ph bill_total n_sub cont_sub  loc_org typeorg1 typeorg2 typeorg3 typeorg4 temperature rainfall 

label var ih "Herfindahl Index"
label var ig "Gini Index"
label var  users_fix "Users (\#)"
label var bill_ph "Water rate (USD/m3)"
label var bill_ph "WSS Total Income (USD)"
label var subsidy  "Subisidies (USD)  " 
label var n_sub  "Users subsidized (\%)  " 
label var contribution  "Contributions (USD)" 
label var typeorg1 "Public corporation"
label var typeorg2  "Local government"
label var typeorg3  "Community" 
label var typeorg4 "Private Corporation"
label var rural_org  "Is in a rural area"
label var temperature  "Temperature (ºC/month) "
label var rainfall "Rainfall (mm/month) " 


eststo clear
estpost su $vars $allcede department municipality wss   if time<0
esttab  using summary_stat.tex,cells("mean(label(Mean) fmt(a2)) sd(label(Std. Deviation) fmt(a2)) min(label(Min.)  fmt(a2)) max(label(Max.)  fmt(a2)) count(label(Obs.) fmt(a2))") booktabs label replace nonumber noobs unstack 


eststo clear
estpost su $vars $allcede department municipality wss   if time<0 & type_org==1
eststo A
estpost su $vars $allcede department municipality wss   if time<0 & type_org==2
eststo B
estpost su $vars $allcede department municipality wss   if time<0 & type_org==3
eststo C
estpost su $vars $allcede department municipality wss   if time<0 & type_org==4
eststo D
esttab  using summary_stat_type_org.tex,cells("mean(label(Mean) fmt(a2)) sd(label(SD) fmt(a2)) count(label(Obs.) fmt(a2))") booktabs label replace nonumber noobs  mgroups("Public Corporation" "Local Governments" "Community" "Private Corporation", pattern( 1 1  1  1 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))   nomtitle


** By treatment status
sort id_wss time
eststo clear
estpost ttest $vars $allcede if time<0  , by(treated) unequal
eststo A
estpost ttest $vars $allcede if time<0 & type_org ==1, by(treated) unequal
eststo B
estpost ttest $vars $allcede if time<0 & type_org ==2, by(treated) unequal
eststo C
estpost ttest $vars $allcede if time<0 & type_org ==3, by(treated) unequal
eststo D
estpost ttest $vars $allcede if time<0 & type_org ==4, by(treated) unequal
eststo E

esttab  A B C D E using differences_mean_treatment.tex, replace label nomtitle nonumbers ///
cell(" t(fmt(a2)) b(label(Diff.) star fmt(a2))") ///
booktabs gaps compress noobs mgroups("All" "Public Corporation" "Local Governments" "Community" "Private Corporation", pattern( 1 1  1  1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))   nomtitle


** By treatment status
sort id_wss time
eststo clear
estpost ttest $vars $allcede if time<0 & distance_1<=10 , by(treated) unequal
eststo A
estpost ttest $vars $allcede if time<0  & distance_1<=20, by(treated) unequal
eststo B
estpost ttest $vars $allcede if time<0 & distance_1<=30, by(treated) unequal
eststo C
estpost ttest $vars $allcede if time<0 & distance_1<=40, by(treated) unequal
eststo D

esttab  A B C D  using differences_mean_distance.tex, replace label nomtitle nonumbers ///
cell(" t(fmt(a2)) b(label(Diff.) star fmt(a2))") ///
booktabs gaps compress noobs mgroups("10km" "20km" "30km" "40km", pattern( 1  1  1  1 ) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))   nomtitle






** Normalized differences

stddiff  $vars $allcede if time<0 , by(treated)  abs
matrix a= r(output)
matrix rown a = ""
stddiff  $vars $allcede if time<0 & distance<=5 , by(treated) abs
matrix b= r(output)

stddiff  $vars $allcede if time<0 & distance<=10 , by(treated) abs
matrix c= r(output)

stddiff  $vars $allcede if time<0 & distance<=20, by(treated) abs
matrix d= r(output)

stddiff  $vars $allcede if time<0 & distance<=30, by(treated) abs
matrix e= r(output)


matrix A =  a,b,c,d,e 

matrix rown A =  "Users (\#)"  "Water rate (USD/m3) " "Subisidies (USD)  "  "Users subsidized (\%)  "  "Contributions (USD)" "Public corporation" "Local government" "Community" "Private Corporation" "Is in a rural area" "Temperature (ºC/month) " "Rainfall (mm/month) " "Total Population" "Rural Index" "Municipal Area (Km2)" "Altitude (m)" "Unsatisfied Basic Need Index" "Multidimensional Poverty Index" "Municipal Income (USD)" "Distance to regional market (km)" "Aqueduct coverage (\%)  " "Sanitation coverage (\%) " "Sewage coverage (\%)  " "Infant mortality Index"
matrix coln A = "Mean" "SD" "Mean" "SD " "Std Diff"   "Mean " "SD " "Mean " "SD" "Std Diff"   "Mean" "SD" "Mean" "SD" "Std Diff"   "Mean" "SD" "Mean" "SD" "Std Diff"  

esttab  matrix(A, fmt("2"))  using stdiff_all.tex, replace  label booktabs gaps compress noobs  nomtitle  mgroups( "5 km" "10 km" "20 km" "30 km", pattern(  1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0  0 1 0 0 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))  




