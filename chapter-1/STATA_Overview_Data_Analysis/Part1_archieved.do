********************************************************************************
*Authro: Awais // 11-10-2023
********************************************************************************
version 17
clear all
set more off
*Create a project directory
cd "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview"
cap log close

*using the data classification.xlsx(this is xls file) with comments from the team. 


clear
* Import the worksheet `Sheet1` from the Excel file `my_excel_file.xlsx`
import excel "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\data classification.xlsx", sheet("Date_11_9_2023")
*rename 
rename	A	variable
rename	B	Indicator
rename	C	indicator_order
rename	D	variable_legacy
rename	E	var_level
rename	F	family_var
rename	G	family_name
rename	H	family_order
rename	I	family_name_legacy
rename	J	Status
rename	K	reason_for_change
rename	L	Included_in_new_version
rename	M	Benchmarked_CTF
rename	N	Benc_Stat_family_aggr_download
rename	O	Benchmark_dynamic_indicator
rename	P	Benchmark_dyna_family_aggr
rename	Q	description
rename	R	Organizational_Capacity
rename	S	People
rename	T	Money
rename	U	Systems
rename	V	Decision_Making_processes
rename	W	PI_performance
rename	X	Comments

drop Y // comment from RR

// dropping 1st line

drop if _n == 1

// List of all variable names
local var_list variable Indicator indicator_order variable_legacy var_level family_var family_name family_name_legacy Status reason_for_change Included_in_new_version Benchmarked_CTF Benc_Stat_family_aggr_download Benc_Stat_family_aggr_download Benchmark_dynamic_indicator Benchmark_dyna_family_aggr description Organizational_Capacity People Money Systems Decision_Making_processes PI_performance Comments
 
// Loop through each variable in the list and destring it
foreach var of local var_list {
    destring `var', replace
}

keep variable Indicator Organizational_Capacity People Money Systems Decision_Making_processes PI_performance

destring , replace

gen lowercase_var = lower(variable)
drop variable
rename lowercase_var variable
save "temp_indicators", replace

*******************************************************************************
clear
use closeness_to_frontier_Static_Nov_11.dta, clear
ren *, upper
merge m:1 COUNTRY_CODE using "regional_classification.dta"
drop if _m == 2
drop _merge
rename FRASER_EFW_FREEDOM_OF_FOREIGNERS   FRASER_EFW
rename WB_WBL_ENTREPRENEURSHIP WB_WBL

 foreach varname in WB_GTMI_DCEI WJP_ROL_3_4 IBP_OBS_OBI WJP_ROL_3_1 WJP_ROL_3_2 VARS_TRANSP_AVG WB_WBL_SOCIAL VDEM_CORE_V2CLDISCW VDEM_CORE_V2CLDISCM WJP_ROL_4_6 WJP_ROL_4_4 WJP_ROL_4_5 VDEM_CORE_V2CLACFREE WJP_ROL_4_7 VDEM_CORE_V2DLENGAGE VDEM_CORE_V2CSREPRSS VDEM_CORE_V2CSEEORGS VDEM_CORE_V2X_CSPART RWB_PFI_INDEX VDEM_CORE_V2CACAMPS BS_BTI_Q2_3 FH_FIW_CL_RATING VARS_SOCIAL_AVG WDI_SESECTCAQZS WDI_SEPRMTCAQZS WDI_SEPRETCAQZS WDI_SESECENRLTCZS WDI_SEPRMENRLTCZS WDI_SPREGBRTHZS WDI_SHSTABRTCZS WDI_SHSTAANVCZS VDEM_CORE_V2PEAPSGEN VDEM_CORE_V2PEAPSECON VDEM_CORE_V2PEAPSPOL VARS_SERVICE_DELIVERY_AVG OECD_PMR_2018_2_2 OECD_PMR_2018_2_1 OECD_PMR_2018_1_4 OECD_PMR_2018_1_3 OECD_PMR_2018_1_2 OECD_PMR_2018_1_1 VDEM_CORE_V2X_GENDER VDEM_CORE_V2LGQUGEN VDEM_CORE_V2PEPWRGEN VDEM_CORE_V2PEPWRSOC VDEM_CORE_V2PEPWRSES FH_FIW_PR_RATING BS_BTI_Q2_1 VDEM_CORE_V2XLG_LEGCON BS_BTI_Q3_1 WJP_ROL_1 BS_BTI_SI VARS_POL_AVG RISE_EE_4_3 WB_PEFA_PI_2016_30 WB_PEFA_PI_2016_29 WB_PEFA_PI_2016_28 WB_PEFA_PI_2016_27 WB_PEFA_PI_2016_26 WB_PEFA_PI_2016_25 WB_PEFA_PI_2016_24 WB_PEFA_PI_2016_23 WB_PEFA_PI_2016_22 WB_PEFA_PI_2016_21 WB_PEFA_PI_2016_20 WB_PEFA_PI_2016_19 WB_PEFA_PI_2016_18 WB_PEFA_PI_2016_17 WB_PEFA_PI_2016_16 WB_PEFA_PI_2016_15 WB_PEFA_PI_2016_14 WB_PEFA_PI_2016_13 WB_PEFA_PI_2016_12 WB_PEFA_PI_2016_11 WB_PEFA_PI_2016_10 WB_PEFA_PI_2016_05 WB_GTMI_PFM_MIS BS_BTI_Q8_2 WB_DEBT_TRANSP_INDEX BS_BTI_Q8_1 VARS_PFM_AVG FRASER_EFW WB_GTMI_I_8 WB_WBL WB_GFDB_OI_01 WB_LPI_LP_LPI_CUST_XQ BS_BTI_Q7_2 WJP_ROL_6 VDEM_CORE_V2XCL_PRPTY VARS_MKT_AVG VDEM_CORE_V2CLACJSTW VDEM_CORE_V2CLACJSTM WJP_ROL_8_4 WJP_ROL_8_1 WJP_ROL_8_2 WJP_ROL_7_5 WJP_ROL_7_6 WJP_ROL_7_1 WJP_ROL_7_7 WJP_ROL_4_3 WJP_ROL_6_6 IDEA_GSOD_V_21_05 VDEM_CORE_V2JUNCIND WJP_ROL_2_2 VDEM_CORE_V2JUHCIND BS_BTI_Q3_2 VDEM_CORE_V2JUACCNT VARS_LEG_AVG WB_ASPIRE_ADEQUACY_BENEFITS WB_ASPIRE_COVERAGE WB_WBL_LABOR OECD_EPL_TEMPORARY OECD_EPL_REGULAR IDEA_GSOD_V_22_16 WJP_ROL_4_8 VDEM_CORE_V2PEASJSOECON VDEM_CORE_V2PEASJPOL VDEM_CORE_V2STCRITRECADM BS_BTI_Q15_1 VDEM_CORE_V2CLRSPCT VARS_HRM_AVG SPI_STD_AND_METHODS SPI_CENSUS_AND_SURVEY_INDEX WB_GTMI_PSDI WB_GTMI_GTEI WB_GTMI_CGSI VARS_DIGITAL_AVG RISE_RE_7 RISE_RE_4 RISE_RE_3 RISE_RE_2 RISE_RE_1 RISE_EE_9 RISE_EE_8 RISE_EE_7 RISE_EE_6 RISE_EE_5 RISE_EE_4 RISE_EE_3 RISE_EE_2 RISE_EE_1 BS_BTI_Q12_1 VARS_CLIMATE_AVG WJP_ROL_6_2 VDEM_CORE_V2EXBRIBE VDEM_CORE_V2X_CORR WJP_ROL_2 VARS_ANTICORRUPTION_AVG {

    rename `varname' Var`varname'

}

reshape long Var,  i(COUNTRY_NAME REGION INCOMEGROUP LENDINGCATEGORY) j(value) string

rename value Variable 
rename Var Value_0_1

save closeness_to_frontier_Static_long, replace
********************************************************************************
use closeness_to_frontier_Static_long, clear
ren *, lower
order country_name region incomegroup lendingcategory  country_code country_group  variable value_0_1
save closeness_to_frontier_Static_long_v1, replace
********************************************************************************
*merge
clear
use closeness_to_frontier_Static_long_v1, clear
gen lowercase_var = lower(variable)
drop variable 
rename lowercase_var variable
merge m:1 variable using "temp_indicators.dta"
drop if _m == 2
drop _merge
order country_name region incomegroup lendingcategory country_code country_group economy Indicator variable Organizational_Capacity Decision_Making_processes value_0_1 People Money Systems  PI_performance 

rename Organizational_Capacity org_cap
rename Decision_Making_processes dec_mkg_prcss
save closeness_to_frontier_Static_v2, replace

*********************************************************************************
//org_cap
* Step 1: Calculate n_orgcap
egen norgcap = total(org_cap), by(variable)
* Step 2: Filter the data and calculate org_cap related statistics
egen norgcap_notmissing = total(!missing(value)), by(country_code region lendingcategory)
egen avgorgcap = mean(value), by(country_code region lendingcategory)


//decision-making processes
* Step 1: Calculate n_dmp
egen n_dmp = total(dec_mkg_prcss), by(variable)
* Step 2: Filter the data and calculate org_cap related statistics
egen dec_mkg_prcss_notmissing = total(!missing(value)), by(country_code region lendingcategory)
egen avgdec_mkg_prcss = mean(value), by(country_code region lendingcategory)

// Define a Stata program for regional variation analysis
program regional_variation_custom
    version 16.0 // Specify the Stata version you are using
    
    // Filter the data where perc_orgcap > 0.5
    keep if norgcap > 0.5
    
    // Calculate the maximum and minimum values of norgcap within each region
    by region: egen max_var = max(norgcap), na
    by region: egen min_var = min(norgcap), na
    
    // Generate code_max and code_min using conditional statements
    gen code_max = . if missing(norgcap)
    gen code_min = . if missing(norgcap)
    by region: replace code_max = country_code if norgcap == max_var
    by region: replace code_min = country_code if norgcap == min_var
    
    // Fill missing values in code_max and code_min
    by region: replace code_max = code_max[_n-1] if missing(code_max)
    by region: replace code_min = code_min[_n+1] if missing(code_min)
    
    // Calculate the mean value of norgcap within each region
    by region: egen mean_var = mean(norgcap), na
    
    // Create a bar graph with error bars and labels
    twoway (bar mean_var region, barwidth(0.5) barcolor(gs12)) ///
           (rcap max_var min_var region, lcolor(black)) ///
           (scatteri code_max max_var region, mlabel(code_max)) ///
           (scatteri code_min min_var region, mlabel(code_min)), ///
           yscale(range(0 1)) ytitle("Value") legend(off) ///
           title("Your Title", size(small)) ///
           yline(0.2, lpattern(dash)) yline(0.4, lpattern(dash)) ///
           yline(0.6, lpattern(dash)) yline(0.8, lpattern(dash))
    
end

********************************************************************************
//Dynamic Data
********************************************************************************

clear
use closeness_to_frontier_Dynamic_Nov_11.dta, clear
ren *, upper
merge m:1 COUNTRY_CODE using "regional_classification.dta"
drop if _m == 2
drop _merge


 foreach varname in VARS_ANTICORRUPTION_AVG WJP_ROL_2 VDEM_CORE_V2X_CORR VDEM_CORE_V2EXBRIBE WJP_ROL_6_2 VARS_CLIMATE_AVG BS_BTI_Q12_1 RISE_EE_1 RISE_EE_2 RISE_EE_3 RISE_EE_4 RISE_EE_5 RISE_EE_6 RISE_EE_7 RISE_EE_8 RISE_EE_9 RISE_RE_1 RISE_RE_2 RISE_RE_3 RISE_RE_4 RISE_RE_7 WB_GTMI_CGSI WB_GTMI_GTEI WB_GTMI_PSDI SPI_CENSUS_AND_SURVEY_INDEX SPI_STD_AND_METHODS VARS_HRM_AVG VDEM_CORE_V2CLRSPCT BS_BTI_Q15_1 VDEM_CORE_V2STCRITRECADM VDEM_CORE_V2PEASJPOL VDEM_CORE_V2PEASJSOECON WJP_ROL_4_8 IDEA_GSOD_V_22_16 OECD_EPL_REGULAR OECD_EPL_TEMPORARY WB_WBL_LABOR WB_ASPIRE_COVERAGE WB_ASPIRE_ADEQUACY_BENEFITS VARS_LEG_AVG VDEM_CORE_V2JUACCNT BS_BTI_Q3_2 VDEM_CORE_V2JUHCIND WJP_ROL_2_2 VDEM_CORE_V2JUNCIND IDEA_GSOD_V_21_05 WJP_ROL_6_6 WJP_ROL_4_3 WJP_ROL_7_7 WJP_ROL_7_1 WJP_ROL_7_6 WJP_ROL_7_5 WJP_ROL_8_2 WJP_ROL_8_1 WJP_ROL_8_4 VDEM_CORE_V2CLACJSTM VDEM_CORE_V2CLACJSTW VARS_POL_AVG BS_BTI_SI WJP_ROL_1 BS_BTI_Q3_1 VDEM_CORE_V2XLG_LEGCON BS_BTI_Q2_1 FH_FIW_PR_RATING VDEM_CORE_V2PEPWRSES VDEM_CORE_V2PEPWRSOC VDEM_CORE_V2PEPWRGEN VDEM_CORE_V2LGQUGEN VDEM_CORE_V2X_GENDER VARS_SOCIAL_AVG FH_FIW_CL_RATING BS_BTI_Q2_3 VDEM_CORE_V2CACAMPS RWB_PFI_INDEX VDEM_CORE_V2X_CSPART VDEM_CORE_V2CSEEORGS VDEM_CORE_V2CSREPRSS VDEM_CORE_V2DLENGAGE WJP_ROL_4_7 VDEM_CORE_V2CLACFREE WJP_ROL_4_5 WJP_ROL_4_4 WJP_ROL_4_6 VDEM_CORE_V2CLDISCM VDEM_CORE_V2CLDISCW WB_WBL_SOCIAL VARS_TRANSP_AVG WJP_ROL_3_2 WJP_ROL_3_1 IBP_OBS_OBI WJP_ROL_3_4 WB_GTMI_DCEI {

    rename `varname' Var`varname'

}

*reshape 
	
reshape long Var, i(YEAR COUNTRY_NAME) j(Indicator ) string
rename Var Value_0_1
rename Indicator variable
gen lowercase_var = lower(variable)
drop variable 
rename lowercase_var variable
merge m:1 variable using "temp_indicators.dta"
drop if _m == 2
drop _merge
ren *, lower
order country_name region incomegroup lendingcategory  country_code country_group  variable value_0_1
save closeness_to_frontier_Dynamic_V2, replace

********************************************************************************
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
* Save the combined dataset
ren *, lower
keep if time >= 2013

// Replace ".." with missing values and convert to numeric
destring ny_gdp_pcap_kd, replace

// Select and rename columns
rename iso3code country_code

// Calculate the latest available year by country
egen latest_year = max(time) if !missing(ny_gdp_pcap_kd)

// Filter out rows with latest_year equals -Inf
drop if missing(latest_year)

// Filter rows where year is equal to the latest available year
keep if time == latest_year
// Keep only distinct values of country_code, year, and gdppercap_constant2015
*duplicates report country_code time
*drop if _N > 1
// Merge with the FLAG dataset
merge m:1 country_code using regional_classification
drop if _m == 2
drop _merge
save wdi_gdppercap_constant2015, replace

********************************************************************************
********************************************************************************


