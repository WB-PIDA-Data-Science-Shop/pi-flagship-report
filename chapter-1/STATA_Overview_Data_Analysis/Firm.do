********************************************************************************
*Authro: Awais // 11-21--2023
*		
* This economy was included in the Eurostat List of offshore financial centres.				
** For Canada, only the data for Quebec and Ontario were included.				
*** For China, only the data for Beijing and Shanghai were included				
********************************************************************************
version 17
clear all
set more off
*Create a project directory
cd "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\data on firm density and new firm density"
cap log close

clear
* Import the worksheet `Sheet1` from the Excel file `my_excel_file.xlsx`
import excel "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\data on firm density and new firm density\New_firms_Historical_data.xlsx", sheet("new_lics")
*rename 
rename	A	Economy
rename	B	Adult_population
rename	C	Year
rename	D	Number_of_NEW_llc 
rename	E	New_business_density_rate

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
* Display the cleaned country names
*list country_name cleaned_country_name

// dropping 1st line
drop if _n == 1
destring country, replace
destring Adult_population, replace
destring Year, replace
destring Number_of_NEW_llc, replace
destring New_business_density_rate, replace

// List of all variable names
ren *,lower 
merge m:1 country using "gdppercap_constant2015"
drop _merge
rename region_v1 region

order country country_code region year adult_population number_of_new_llc new_business_density_rate log_ny_gdp_mktp_cd
keep if inlist(year, 2014, 2016, 2018, 2020)

egen country_id = group(country)

*examine how the new business density rate is influenced by GDP per capita and the number of new businesses per population, so I set the new business density rate as the dependent variable (DV) and the other two variables as independent variables (IVs).

** Declare the panel structure
xtset country_id year
*a. Fixed Effects (FE) Panel Regression: un-observed hetereogenity conrolled by FE by across the countries over time.
*Estimate estimate and use Housemman estimate  
xtreg new_business_density_rate log_ny_gdp_mktp_cd  number_of_new_llc, fe
* depn var : show cty and year fixed effect  more robust
reg new_business_density_rate i.country_id i.year log_ny_gdp_mktp_cd  number_of_new_llc
*clustered standard errors at the coutry level. This is robust model - 
reg new_business_density_rate i.country_id i.year log_ny_gdp_mktp_cd  number_of_new_llc, cl(country_id)
estimates store cl_model
ssc install coefplot
* Retrieve the stored estimates
estimates use cl_model
* Create a coefficient plot: search vertical and horizental 
coefplot cl_model, keep(number_of_new_llc) // You can customize the plot further as needed
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


********************************************************************************
*// Load your dataset 
********************************************************************************
*World Goverance indicators 

ssc install wbopendata

wbopendata, language(en - English) country() topics() indicator(NY.GDP.PCAP.KD) clear

reshape long yr, i(countryname countrycode regionname indicatorname indicatorcode) j(time)

rename countrycode iso3code

replace indicatorcode = subinstr(indicatorcode, ".", "_", .)

reshape wide yr, i(iso3code countryname time) j(indicatorcode) string

keep countryname iso3code time yrNY_GDP_PCAP_KD regionname indicatorname
order countryname time iso3code  regionname indicatorname yrNY_GDP_PCAP_KD
rename yrNY_GDP_PCAP_KD NY_GDP_PCAP_KD
lab var NY_GDP_PCAP_KD "GDP per capita (constant 2015 US$), note:GDP per capita is gross domestic product divided by midyear population. GDP is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources. Data are in constant 2015 U.S. dollars"
