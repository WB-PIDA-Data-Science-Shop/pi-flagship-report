********************************************************************************
*Authro: Awais // 11-21--2023 and amended later 
********************************************************************************
* Note regarding firm_level data: This economy was included in the Eurostat List of offshore financial centres.				
** For Canada, only the data for Quebec and Ontario were included.				
*** For China, only the data for Beijing and Shanghai were included. Access here: https://www.worldbank.org/en/programs/entrepreneurship			
********************************************************************************
version 17
clear all
set more off
*Create a project directory
cd "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\Overview_Data_Analysis\data_firm_level_in_progress {not done}"
cap log close

clear
* Import the worksheet `Sheet1` from the Excel file `my_excel_file.xlsx`
import excel "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\Overview_Data_Analysis\data_firm_level_in_progress {not done}\New_firms_Historical_data.xlsx", sheet("new_lics")
*rename 
rename	A	Economy
rename	B	country_code
rename	C	Adult_population
rename	D	Year
rename	E	Number_of_NEW_llc 
rename	F	New_business_density_rate


* Example dataset with country names containing asterisks
input str50 country_name
"Antigua and Barbuda*"
"Bahrain*"
"Bahrain*"
"Barbados*"
"Belize*"
"Canada**"
"China***"
"Cook Islands*"
"Dominica*"
"Grenada*"
"Hong Kong SAR, China*"
"Isle of Man*"
"Liberia*"
"Liechtenstein*"
"Mauritius*"
"Philippines*"
"Seychelles*"
"Singapore*"
"St. Lucia*"
"St. Vincent and the Grenadines*"
"Vanuatu*"
end

* Remove asterisks from country names
gen cleaned_country_name = subinstr(Economy, "*", "", .)

drop Economy 
rename cleaned_country_name  country
drop country_name

// dropping 1st line
drop if _n == 1
destring country, replace
destring Adult_population, replace
destring Year, replace
destring Number_of_NEW_llc, replace
destring New_business_density_rate, replace
encode country_code, gen(country_num)
drop country_code
rename country_num country_code
ren *,lower
merge m:1 country using "gdppercap_constant2015", force
drop if _merge==1
drop if _merge==2
drop _merge
drop region_v1 


********************************************************************************
//regression: *examine how the new business density rate is influenced by GDP per capita 
/// and the number of new businesses per population, /// so I set the new business density rate ///
/// as the dependent variable (DV) and the other two variables as independent variables (IVs)       ///
********************************************************************************

** Declare the panel structure
encode country, generate(country_v1)
sort country_v1 year
xtset country_v1 year

*a. Fixed Effects (FE) Panel Regression: un-observed hetereogenity conrolled by FE by across the countries over time.
xtreg new_business_density_rate log_ny_gdp_mktp_cd  number_of_new_llc, fe

//results : shows a significant relationship between the number of new LLCs and the DV ( measure of new business density), controlling for country-specific unobserved factors. The high rho value indicates that much of the variation in your dependent variable is due to differences across countries....

//Coefficients: log_ny_gdp_mktp_cd: Omitted (likely due to multicollinearity or because it's a time-invariant variable in a fixed-effects model).
//number_of_new_llc: The coefficient is .0000322 with a standard error of 1.93e-06. The t-statistic is 16.71 and the p-value is 0.000, indicating this variable is statistically significant and implies that for each unit increase in number_of_new_llc, the dependent variable (new_business_den~e) increases by 0.0000322 units, holding other factors constant._cons (Constant): The coefficient for the constant is 2.575924.


* DV : show cty and year fixed effect  more robust
reg new_business_density_rate i.country_v1 i.year log_ny_gdp_mktp_cd  number_of_new_llc

*clustered standard errors at the coutry level. This is robust model -
reg new_business_density_rate i.country_v1 i.year log_ny_gdp_mktp_cd  number_of_new_llc, cl(country_v1)

**************************************************************************************************************

































//storing the estimates 
estimates store cl_model
ssc install coefplot
* Retrieve the stored estimates
estimates use cl_model
* Create a coefficient plot: search vertical and horizental 
coefplot cl_model, keep(number_of_new_llc) // I can customize the plot further as needed
*to do: change the new llc var.











**************************************************
*b. Random Effects (RE) Panel Regression:
xtreg new_business_density_rate log_ny_gdp_mktp_cd  number_of_new_llc, re
*c. Pooled OLS (if you assume no fixed or random effects):
xtreg new_business_density_rate log_ny_gdp_mktp_cd  number_of_new_llc, pa

*******************************************************************************

//org_cap
use  closeness_to_frontier_Dynamic_V2, clear
ren *, lower
order country_name region incomegroup lendingcategory  country_code country_group  variable value_0_1
gen group = missing(region)
lab var group "these group so does not reflect in region category "
gen dummy= 1
replace dummy =0 if value_0_1==.
egen n_ctys = total(dummy == 1), by(variable)

sort variable
drop if group==1
lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
sort country_group economy variable
gen lowercase_var = lower(variable)
drop variable 
rename lowercase_var variable
//dec_mkg_prcss
drop if n_ctys<100
rename organizational_capacity org_cap
rename decision_making_processes dec_mkg_prcss
*replace org_cap=0 if org_cap==.
keep if org_cap==1
egen avg_org_cap = mean(value_0_1), by (org_cap country_code region year)
contract country_code country_name year region avg_org_cap
drop if missing(avg_org_cap)
drop _freq
sort region year
keep if inlist(year, 2014, 2016, 2018, 2020, 2022)
**************
*merge
merge 1:1 country_code region year using x5
sort region year
drop if avg_org_cap & avg_dec_mkg_prcss==.
order country_code country_name region  year avg_dec_mkg_prcss avg_org_cap
drop _merge
drop if inlist(country_code, "TUV", "NRU", "KIR", "NHL", "FSM") |  ///
inlist(country_code, "STP", "VCT", "MAC", "TCA", "SXM", "LCA", "CPV")
sort year region
save dynamic_ctf, replace
