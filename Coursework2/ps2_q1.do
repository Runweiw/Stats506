import delimited recs2015_public_v3.csv,clear
**1
gen electricity_usage = kwh*nweight
**generate a new variable and use kwh*nweight as its value
egen suma0 = sum(electricity_usage)
**check electricity_usage
foreach i of num 1/96{
**for each
gen x1`i' = kwh*(brrwt`i'-nweight)
egen suma`i' = sum(x1`i')
replace suma`i' = (suma`i')^2
**use egen to do sum of coloumn
}
egen std_error_electricity = rowtotal(suma1-suma96)
replace std_error_electricity = (std_error_electricity/24)^0.5
preserve 
keep suma0 std_error_electricity
restore


gen natural_gas_usage = cufeetng*nweight
egen sumb0 = sum(natural_gas_usage)
foreach i of num 1/96{
gen x2`i' = cufeetng*(brrwt`i'-nweight)
egen sumb`i' = sum(x2`i')
replace sumb`i' = (sumb`i')^2
}
egen std_error_gas = rowtotal(sumb1-sumb96)
replace std_error_gas = (std_error_gas/24)^0.5
preserve 
keep sumb0 std_error_gas
restore


gen propane_usage = gallonlp*nweight
egen sumc0 = sum(propane_usage)
foreach i of num 1/96{
gen x3`i' = gallonlp*(brrwt`i'-nweight)
egen sumc`i' = sum(x3`i')
replace sumc`i' = (sumc`i')^2
}
egen std_error_propane = rowtotal(sumc1-sumc96)
replace std_error_propane = (std_error_propane/24)^0.5
preserve 
keep sumc0 std_error_propane
restore


gen fuel_oil_usage = gallonfo*nweight
egen sumd0 = sum(fuel_oil_usage)
foreach i of num 1/96{
gen x4`i' = gallonfo*(brrwt`i'-nweight)
egen sumd`i' = sum(x4`i')
replace sumd`i' = (sumd`i')^2
}
egen std_error_oil = rowtotal(sumd1-sumd96)
replace std_error_oil = (std_error_oil/24)^0.5


collapse(mean) suma0 std_error_electricity sumb0 std_error_gas sumc0 std_error_propane sumd0 std_error_oil
export delimited using recs2015_usage.csv
