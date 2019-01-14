**a
***Import data and merge two datasets:
import sasxport OHX_D.XPT,clear
save OHX_D
import sasxport DEMO_D.XPT
save DEMO_D
merge 1:1 seqn using OHX_D

**b
***Drop missing values:
drop if missing(ohx04htc)
drop if missing(ridagemn)
***Drop and replace values:
drop if ohx04htc == 9
replace ohx04htc = 2 if ohx04htc == 5
replace ohx04htc = 0 if ohx04htc == 2 | ohx04htc == 4
***Do logit regression for these two variables:
logit ohx04htc ridagemn
***Calculate top_25, top_50, and top_75 of individuals lose their primary upper right 2nd bicuspid:
gen pop_25 = trunc(((-log((1-0.25)/0.25))-8.359362)/(-0.0696778))
gen pop_50 = trunc(((-log((1-0.50)/0.25))-8.359362)/(-0.0696778))
gen pop_75 = trunc(((-log((1-0.75)/0.25))-8.359362)/(-0.0696778))
***Choose the range of representative age values:
replace pop_25 = ceil(((-log((1-0.25)/0.25))-8.359362)/(-0.0696778)/12)
replace pop_75 = floor(((-log((1-0.75)/0.75))-8.359362)/(-0.0696778)/12)


**c
**1
logit ohx04htc ridagemn
***Show its BIC value:
estat ic
***Do logit regression for three variables:
logit ohx04htc ridagemn i.riagendr
***Show its BIC value:
estat ic
***Consider models with smaller BIC is better

**2
replace ridreth1 = 2 if ridreth1 == 5
gen race1 = ridreth1
gen race2 = ridreth1
gen race4 = ridreth1
replace race1 = 0 if race1 == 2 | race1 == 3 | race1 == 4
replace race1 = 1 if race1 == 1
replace race2 = 0 if race2 == 1 | race2 == 3 | race2 == 4
replace race2 = 1 if race2 == 2
replace race4 = 0 if race4 == 1 | race4 == 2 | race4 == 3
replace race4 = 1 if race4 == 4 
***Do logit regression:
logit ohx04htc ridagemn i.race1
estat ic 
logit ohx04htc ridagemn i.race4
estat ic
logit ohx04htc ridagemn i.race4 i.race2
estat ic
**3
logit ohx04htc ridagemn i.race4 indfmpir
estat ic
***The final model is logit ohx04htc ridagemn i.race4 indfmpir, BIC=1468.503

svyset sdmvpsu [pweight=wtmec2yr], strata(sdmvstra) vce(linearized)
svy: logit ohx04htc ridagemn i.race4 indfmpir
***The result of svy logit shows differences compared with results on logit.
***Since svy logit uses not OLS, but weighted least squares, the result shows different estimates' values.





**d
***Calculate adjusted predctions at the mean:
gen age_year = trunc(ridagemn/12)
quietly logit ohx04htc age_year i.race4 indfmpir 
margins, at(age_year=(8(1)11)) atmeans
marginsplot

***Calculate marginal effects at the mean:
margins, dydx(race4) at(age_year=(8(1)11)) atmeans
margins, dydx(indfmpir) at(age_year=(8(1)11)) atmeans
marginsplot

***Average marginal effect
margins, dydx(race4) at(age_year=(8(1)11)) vsquish
margins, dydx(indfmpir) at(age_year=(8(1)11)) vsquish
marginsplot
