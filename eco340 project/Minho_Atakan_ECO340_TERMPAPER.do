clear all
set more off 
cd "C:\Users\paul\Desktop\eco340"
use census0611_new


*separating education variable into four different indicator variables 
gen educ0 = educ if(educ==0)
gen educ1 = educ if(educ==1)
gen educ2 = educ if(educ==2)
gen educ3 = educ if(educ==3)
gen educ4 = educ if(educ==4)

replace educ0 = 1 if(educ0==0)
replace educ0 = 0 if(educ0==. & educ != .)
replace educ1 = 0 if(educ1==. & educ != .)
replace educ2 = 1 if(educ2==2)
replace educ2 = 0 if(educ2==. & educ != .)
replace educ3 = 1 if(educ3==3)
replace educ3 = 0 if(educ3==. & educ != .)
replace educ4 = 1 if(educ4==4)
replace educ4 = 0 if(educ4==. & educ != .)
 

gen age1 = agegrp if(agegrp==8 | agegrp==9 | agegrp==10)
replace age1 = 1 if(age1 != .)
replace age1 = 0 if(age1==. & agegrp != .)
gen age2 = agegrp if(agegrp==11 | agegrp==12 | agegrp==13)
replace age2 = 1 if(age2 ! =.)
replace age2 = 0 if(age2==. & agegrp != .)
gen age3 = agegrp if(agegrp==14 | agegrp==15 | agegrp==16)
replace age3 = 1 if(age3 != .)
replace age3 = 0 if(age3==. & agegrp != .)


*cohort 1 indicates those from developed countries, and cohort 2 indicates those from developing countries

gen coh1 = pob if(pob== 1 | pob== 2 | pob== 7 | pob== 8 | pob== 9 | pob== 11 | pob== 12 | pob== 20 | pob== 27)
replace coh1 = 1 if(coh1 !=.)
replace coh1 = 0 if(coh1 ==. & pob != .)
gen coh2 = pob if(coh1== 0)
replace coh2 = 1 if(coh2 !=.)
replace coh2 = 0 if(coh2 ==. & pob != .)


gen cnda = pob if(pob==1)
replace cnda = 0 if(cnda ==. & pob != .)


gen se_inc = cow if(cow==3 | cow==4)
replace se_inc = 1 if(se_inc != .)
replace se_inc = 0 if(se_inc == . & cow != .)
gen se_uninc = cow if(cow==5 | cow==6)
replace se_uninc = 1 if(se_uninc != .)
replace se_uninc = 0 if(se_uninc == . & cow != .)
gen emp = cow if(cow==1)
replace emp = 0 if(emp==. & cow !=.)

gen selfemp = se_inc + se_uninc

*mar=1 if legally married or living in common law,
*mar=0 if never legally married, separted, divorced, or widowed. 
gen mar = marsth if(marsth == 2 | marsth == 3)
replace mar = 1 if(mar != .)
replace mar = 0 if(mar == . & marsth !=.)
gen unmar = marsth if(mar == 0)
replace unmar = 1 if(unmar != .)
replace unmar = 0 if(unmar == . & marsth !=.)

gen hhsze = hhsize if(hhsize != 8)

gen female = sex if(sex == 1)
replace female = 0 if (sex==2)

gen lnwage = log(wages)

*did not include wage due to possible issue of reserve casaulity 

gen pkid = 1 if(pkid0_1 == 1 |pkid2_5 == 1 | pkid6_14 == 1 | pkid15_24 == 1 | pkid25==1)
replace pkid = 0 if(pkid==. & (pkid0_1 != . | pkid2_5 != . | pkid6_14 != . | pkid15_24 != . | pkid25 != .))


*if one of official languages of canada is spoken at home
gen hloff = 1 if(hlaen == 1 | hlafr == 1)
replace hloff = 0 if(hloff == . & hlaen != . )
*non-permanent residents counted as immigrants
gen minority = vismin if(vismin != 13)
replace minority = 1 if(minority != .)
replace minority = 0 if(minority == . & vismin != .)

gen int1 = female*coh2

gen fem_imm = female if(cnda == 0)
replace fem_imm = 0 if(pob==1)

*se rate in developing: 0.497513101
*se rate in dveloped: 0.542613969

logit selfemp educ1 educ2 educ3 educ4 age2 age3 mar ysm hhsze coh2 female int1 pkid hloff prihm i.efdecile invst, r

margin, dydx(coh2) atmeans
margin, dydx(int1) atmeans

logit se_inc educ1 educ2 educ3 educ4 age2 age3 mar ysm hhsze coh2 female int1 pkid hloff prihm i.efdecile invst, r

margin, dydx(coh2) atmeans
margin, dydx(int1) atmeans

*-0.0051153       

logit se_uninc educ1 educ2 educ3 educ4 age2 age3 mar ysm hhsze coh2 female int1 pkid hloff prihm i.efdecile invst, r 

margin, dydx(coh2) atmeans
margin, dydx(int1) atmeans

*-0.031101









