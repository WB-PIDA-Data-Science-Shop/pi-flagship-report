********************************************************************************
*Authro: Awais // creatd 11-10-2023 and edited on 111-2023 after team's feedback on data analyis.
********************************************************************************
version 17
clear all
set more off
*Create a project directory
cd "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\Overview_Data_Analysis"
cap log close

*******************************************************************************
*Dynamic 
********************************************************************************
clear
use closeness_to_frontier_Dynamic_Nov_11.dta, clear
*og
egen missing_og=rownonmiss(vdem_core_v2peasjpol vdem_core_v2stcritrecadm vdem_core_v2peasjsoecon rise_ee_4 wjp_rol_7_5 wjp_rol_3_2 wjp_rol_7_6 wjp_rol_3_4 wjp_rol_3_1 ibp_obs_obi)
*tabulate  missing_og
egen og_11=rowmean(vdem_core_v2peasjpol vdem_core_v2stcritrecadm vdem_core_v2peasjsoecon rise_ee_4 wjp_rol_7_5 wjp_rol_3_2 wjp_rol_7_6 wjp_rol_3_4 wjp_rol_3_1 ibp_obs_obi) if missing==11

*dmp
egen missing_dmp=rownonmiss(vdem_core_v2xlg_legcon vdem_core_v2cldiscw vdem_core_v2clacfree vdem_core_v2pepwrsoc wjp_rol_4_5 wjp_rol_4_4 vdem_core_v2x_gender vdem_core_v2lgqugen wjp_rol_4_6 vdem_core_v2cacamps vdem_core_v2cldiscm vdem_core_v2dlengage vdem_core_v2csreprss wjp_rol_4_7 vdem_core_v2cseeorgs vdem_core_v2x_cspart wjp_rol_1 vdem_core_v2pepwrses rwb_pfi_index vdem_core_v2pepwrgen)

egen dmp_20=rowmean(vdem_core_v2xlg_legcon vdem_core_v2cldiscw vdem_core_v2clacfree vdem_core_v2pepwrsoc wjp_rol_4_5 wjp_rol_4_4 vdem_core_v2x_gender vdem_core_v2lgqugen wjp_rol_4_6 vdem_core_v2cacamps vdem_core_v2cldiscm vdem_core_v2dlengage vdem_core_v2csreprss wjp_rol_4_7 vdem_core_v2cseeorgs vdem_core_v2x_cspart wjp_rol_1 vdem_core_v2pepwrses rwb_pfi_index vdem_core_v2pepwrge) if missing_dmp==20

ren *, upper
merge m:1 COUNTRY_CODE using "regional_classification.dta"
drop if _m == 2
drop _merge

order COUNTRY_NAME COUNTRY_CODE COUNTRY_GROUP YEAR ECONOMY REGION INCOMEGROUP LENDINGCATEGORY
*drop if OG_11 ==. & DMP_20 == .

encode COUNTRY_NAME, generate(COUNTRY_NAME_numeric) label(COUNTRY_NAME)
encode COUNTRY_CODE, generate(COUNTRY_CODE_numeric) label(COUNTRY_CODE)
encode ECONOMY, generate(ECONOMY_numeric) label(ECONOMY_CODE)
encode INCOMEGROUP, generate(INCOMEGROUP_numeric) label(INCOMEGROUP_CODE)
encode REGION, generate(REGION_numeric) label(REGION_CODE)
encode LENDINGCATEGORY, generate(LENDINGCATEGORY_numeric) label(LENDINGCATEGORY_CODE)

drop COUNTRY_NAME
drop COUNTRY_CODE
drop ECONOMY
drop INCOMEGROUP
drop REGION
drop LENDINGCATEGORY

rename COUNTRY_NAME_numeric  COUNTRY_NAME
rename COUNTRY_CODE_numeric  COUNTRY_CODE
rename ECONOMY_numeric  ECONOMY
rename REGION_numeric  REGION
rename LENDINGCATEGORY_numeric  LENDINGCATEGORY

 foreach varname in COUNTRY_NAME COUNTRY_CODE YEAR VARS_ANTICORRUPTION_AVG WJP_ROL_2 VDEM_CORE_V2X_CORR VDEM_CORE_V2EXBRIBE WJP_ROL_6_2 VARS_CLIMATE_AVG BS_BTI_Q12_1 RISE_EE_1 RISE_EE_2 RISE_EE_3 RISE_EE_4 RISE_EE_5 RISE_EE_6 RISE_EE_7 RISE_EE_8 RISE_EE_9 RISE_RE_1 RISE_RE_2 RISE_RE_3 RISE_RE_4 RISE_RE_7 WB_GTMI_CGSI WB_GTMI_GTEI WB_GTMI_PSDI SPI_CENSUS_AND_SURVEY_INDEX SPI_STD_AND_METHODS VARS_HRM_AVG VDEM_CORE_V2CLRSPCT BS_BTI_Q15_1 VDEM_CORE_V2STCRITRECADM VDEM_CORE_V2PEASJPOL VDEM_CORE_V2PEASJSOECON WJP_ROL_4_8 IDEA_GSOD_V_22_16 OECD_EPL_REGULAR OECD_EPL_TEMPORARY WB_WBL_LABOR WB_ASPIRE_COVERAGE WB_ASPIRE_ADEQUACY_BENEFITS VARS_LEG_AVG VDEM_CORE_V2JUACCNT BS_BTI_Q3_2 VDEM_CORE_V2JUHCIND WJP_ROL_2_2 VDEM_CORE_V2JUNCIND IDEA_GSOD_V_21_05 WJP_ROL_6_6 WJP_ROL_4_3 WJP_ROL_7_7 WJP_ROL_7_1 WJP_ROL_7_6 WJP_ROL_7_5 WJP_ROL_8_2 WJP_ROL_8_1 WJP_ROL_8_4 VDEM_CORE_V2CLACJSTM VDEM_CORE_V2CLACJSTW VARS_POL_AVG BS_BTI_SI WJP_ROL_1 BS_BTI_Q3_1 VDEM_CORE_V2XLG_LEGCON BS_BTI_Q2_1 FH_FIW_PR_RATING VDEM_CORE_V2PEPWRSES VDEM_CORE_V2PEPWRSOC VDEM_CORE_V2PEPWRGEN VDEM_CORE_V2LGQUGEN VDEM_CORE_V2X_GENDER VARS_SOCIAL_AVG FH_FIW_CL_RATING BS_BTI_Q2_3 VDEM_CORE_V2CACAMPS RWB_PFI_INDEX VDEM_CORE_V2X_CSPART VDEM_CORE_V2CSEEORGS VDEM_CORE_V2CSREPRSS VDEM_CORE_V2DLENGAGE WJP_ROL_4_7 VDEM_CORE_V2CLACFREE WJP_ROL_4_5 WJP_ROL_4_4 WJP_ROL_4_6 VDEM_CORE_V2CLDISCM VDEM_CORE_V2CLDISCW WB_WBL_SOCIAL VARS_TRANSP_AVG WJP_ROL_3_2 WJP_ROL_3_1 IBP_OBS_OBI WJP_ROL_3_4 WB_GTMI_DCEI {

    rename `varname' Var`varname'

}

reshape long Var,  i(VarCOUNTRY_NAME VarCOUNTRY_CODE  VarYEAR) j(value) string

rename value Variable 
rename Var value_0_1
ren*, lower
rename varcountry_name country_name 
rename varcountry_code country_code 
rename varyear year
rename incomegroup_numeric incomegroup
order year economy country_name country_code incomegroup region lendingcategory

*these are regions (SSA under country_name)
drop if country_name==.
drop if variable =="YEAR"
drop if variable =="COUNTRY_CODE"
keep if inlist(year, 2014, 2016, 2018, 2020, 2022)
gen new_var = lower(variable)
drop if og_11 ==. & dmp_20 == .
drop if economy==.

drop variable
rename new_var  variable

sort variable
merge m:1 variable using "temp_indicators.dta"
drop if _m == 2
drop _merge
 
save closeness_to_frontier_dynamic_long_v3, replace


****************************************************************************************************************
*Dynamic: CTF : Decision Making Processes and Organizational Capacity  (avg) by regions and Global average 

****************************************************************************************************************
use closeness_to_frontier_dynamic_long_v3, clear
gen group = missing(region)
lab var group "these group so does not reflect in region category "
ren*, lower
gen dummy= 1
replace dummy =0 if value_0_1==.
egen n_ctys = total(dummy == 1), by(variable)
drop if group==1
lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
rename organizational_capacity org_cap
rename decision_making_processes dec_mkg_prcss
sort country_group economy variable
drop if n_ctys<100
keep if dec_mkg_prcss==1
egen avg_dec_mkg_prcss = mean(value_0_1), by (dec_mkg_prcss country_code region year)
contract country_code region year avg_dec_mkg_prcss
drop if missing(avg_dec_mkg_prcss)
drop _freq
sort region year
save temp_avg_dec_mkg_prcss, replace

//org_cap
use  closeness_to_frontier_dynamic_long_v3, clear
gen group = missing(region)
lab var group "these group so does not reflect in region category "
ren*, lower
gen dummy= 1
replace dummy =0 if value_0_1==.
egen n_ctys = total(dummy == 1), by(variable)
drop if group==1
lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
rename organizational_capacity org_cap
rename decision_making_processes dec_mkg_prcss
//dec_mkg_prcss
sort country_group economy variable
drop if n_ctys<100
keep if org_cap==1
egen avg_org_cap = mean(value_0_1), by (org_cap country_code region year)
contract country_code region year avg_org_cap
drop if missing(avg_org_cap)
drop _freq
sort region year
save temp_avg_org_cap, replace 

// Merge the two datasets
use temp_avg_org_cap.dta, clear
merge 1:1 country_code year using temp_avg_dec_mkg_prcss.dta
sort region year
drop _merge

//GDP 
merge m:1 country_code using "gdppercap_constant2015" 
drop if _m == 2
drop _merge

sort year region

order year country_code country region  avg_org_cap avg_dec_mkg_prcss log_ny_gdp_mktp_cd

drop if missing(log_ny_gdp_mktp_cd)

save og_dmp_gdp_dynmaic, replace 
// Export data to Excel
export excel using "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\CTF_static_data.xlsx", sheet("Dynamic_CTF") replace

*********************************************************************************************************************************************************************************************************
//Scatter plot vs Banks Goals/
//replace the instituiton and other variables to prepare the org_cap  log_ny_gdp_mktp_cd  etc to scater plot in R
*********************************************************************************************************************************************************************************************************
























******************************************************************************************************************************************************************************************************************
ddd
*below code is for Awais only
*******************************************************************************************************************************************************************************************************************
// Define a list of variables to process
local variables "avg_dec_mkg_prcss org_cap"

// Loop over each variable
foreach var in `variables' {
    use closeness_to_frontier_dynamic_long_v3, clear
    ren*, lower
    // Rest of the data preparation code
    order country_name region incomegroup lendingcategory  year country_code country_group  variable value_0_1
    gen group = missing(region)
    lab var group "these group so does not reflect in region category "
    gen dummy = 1
    replace dummy = 0 if value_0_1 == .
    egen n_ctys = total(dummy == 1), by(variable)
    drop if group == 1
    lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
    
    // Rename variables
    rename organizational_capacity org_cap
    rename decision_making_processes dec_mkg_prcss
    
    // Sort and filter data
    sort country_group economy variable
    drop if n_ctys < 100
    
    // Filter based on the specific variable
    if "`var'" == "avg_dec_mkg_prcss" {
        keep if dec_mkg_prcss == 1
        egen avg_var_dec_mkg = mean(value_0_1), by (dec_mkg_prcss country_code region year)
    }
    else if "`var'" == "org_cap" {
        keep if org_cap == 1
        egen avg_var_org_cap = mean(value_0_1), by (org_cap country_code region year)
    }
    
    contract country_code country_name year region avg_var
    drop if missing(avg_var)
    drop _freq
    sort region year
    
    // Save the result with a variable-specific filename
    local save_filename "`var'_data.dta"
    save `save_filename', replace
}

// Merge the two datasets
use avg_dec_mkg_prcss_data.dta, clear
merge 1:1 country_code year using avg_org_cap_data.dta
sort region year
drop _merge



// Define a list of variables to process
local variables "avg_dec_mkg_prcss org_cap"

// Loop over each variable
foreach var in `variables' {
    use closeness_to_frontier_dynamic_long, clear
    
    // Drop unwanted countries
  drop if inlist(country_code, "TUV", "NRU", "KIR", "NHL", "FSM") | ///
inlist(country_code, "STP", "VCT", "TCA", "SXM", "LCA", "CPV", "PYF", "ASM","MNP") | ///
inlist(country_code, "NCL","VGB","KNA","CYM","BMU", "MDG", "MHL")
    
    // Rest of the data preparation code
    order country_name region incomegroup lendingcategory  year country_code country_group  variable value_0_1
    gen group = missing(region)
    lab var group "these group so does not reflect in region category "
    gen dummy = 1
    replace dummy = 0 if value_0_1 == .
    egen n_ctys = total(dummy == 1), by(variable)
    drop if group == 1
    lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
    
    // Rename variables
    rename organizational_capacity org_cap
    rename decision_making_processes dec_mkg_prcss
    
    // Sort and filter data
    sort country_group economy variable
    drop if n_ctys < 100
    
    // Filter based on the specific variable
    if "`var'" == "avg_dec_mkg_prcss" {
        keep if dec_mkg_prcss == 1
        egen avg_var_dec_mkg = mean(value_0_1), by (dec_mkg_prcss country_code region year)
    }
    else if "`var'" == "org_cap" {
        keep if org_cap == 1
        egen avg_var_org_cap = mean(value_0_1), by (org_cap country_code region year)
    }
    
    contract country_code country_name year region avg_var
    drop if missing(avg_var)
    drop _freq
    sort region year
    keep if inlist(year, 2014, 2016, 2018, 2020, 2022)
    
    // Save the result with a variable-specific filename
    local save_filename "`var'_data.dta"
    save `save_filename', replace
}


// Merge the two datasets
use avg_dec_mkg_prcss_data.dta, clear
merge 1:1 country_code year using avg_org_cap_data.dta
sort region year
drop _merge

// Export data to Excel
export excel using "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\CTF_static_data.xlsx", sheet("Dynamic_CTF") replace



//sample Dynamic 
//closeness_to_frontier_dynamic_long_v3
use  closeness_to_frontier_Dynamic_V2, clear

drop if inlist(country_code, "TUV", "NRU", "KIR", "NHL", "FSM") |  ///

inlist(country_code, "STP", "VCT", "MAC", "TCA", "SXM", "LCA", "CPV") 

order country_name region incomegroup lendingcategory  country_code country_group  variable value_0_1
gen group = missing(region)
lab var group "these group so does not reflect in region category "
gen dummy= 1
replace dummy =0 if value_0_1==.
egen n_ctys = total(dummy == 1), by(variable)
drop if group==1
lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
rename organizational_capacity org_cap
rename decision_making_processes dec_mkg_prcss
sort country_group economy variable
drop if n_ctys<100
keep if dec_mkg_prcss==1
egen avg_dec_mkg_prcss = mean(value_0_1), by (dec_mkg_prcss country_code variable indicator region year)
contract country_code country_name year variable indicator region avg_dec_mkg_prcss
drop if missing(avg_dec_mkg_prcss)
drop _freq
sort region year
keep if inlist(year, 2014, 2016, 2018, 2020, 2022)
save x5, replace

//org_cap
use  closeness_to_frontier_Dynamic_V2, clear
drop if inlist(country_code, "TUV", "NRU", "KIR", "NHL", "FSM") |  ///
inlist(country_code, "STP", "VCT", "MAC", "TCA", "SXM", "LCA", "CPV")
order country_name region incomegroup lendingcategory  country_code country_group  variable value_0_1
gen group = missing(region)
lab var group "these group so does not reflect in region category "
gen dummy= 1
replace dummy =0 if value_0_1==.
egen n_ctys = total(dummy == 1), by(variable)
drop if group==1
lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
rename organizational_capacity org_cap
rename decision_making_processes dec_mkg_prcss
//dec_mkg_prcss
sort country_group economy variable
drop if n_ctys<100
keep if org_cap==1
egen avg_org_cap = mean(value_0_1), by (org_cap country_code country_code variable region year)
contract country_code country_name year region country_code variableavg_org_cap
drop if missing(avg_org_cap)
drop _freq
sort region year
keep if inlist(year, 2014, 2016, 2018, 2020, 2022)
*merge
merge 1:1 country_code region year using x5
sort region year
drop _merge
sort year region
save dynamic_ctf, replace


//Sample CTF-Static code:

use  CTF_Static_long_maindata, clear
ren *, lower
order country_name region incomegroup lendingcategory  country_code country_group  variable value_0_1
gen group = missing(region)
lab var group "these group so does not reflect in region category "
gen dummy= 1
replace dummy =0 if value_0_1==.
egen n_ctys = total(dummy == 1), by(variable)
***
sort variable
drop if group==1
lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
sort country_group economy variable

//dec_mkg_prcss
drop if n_ctys<100
*replace dec_mkg_prcss=0 if dec_mkg_prcss==.
keep if dec_mkg_prcss==1
egen avg_dec_mkg_prcss = mean(value_0_1), by (dec_mkg_prcss country_code region)
contract country_code country_name variable indicator region avg_dec_mkg_prcss
drop if missing(avg_dec_mkg_prcss)
drop _freq
save x_data, replace

//org_cap
use  CTF_Static_long_maindata, clear
ren *, lower
order country_name region incomegroup lendingcategory  country_code country_group  variable value_0_1
gen group = missing(region)
lab var group "these group so does not reflect in region category "
gen dummy= 1
replace dummy =0 if value_0_1==.
egen n_ctys = total(dummy == 1), by(variable)
***
sort variable
drop if group==1
lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
sort country_group economy variable
//dec_mkg_prcss
drop if n_ctys<100
*replace org_cap=0 if org_cap==.
keep if org_cap==1
egen avg_org_cap = mean(value_0_1), by (org_cap country_code region)
contract country_code country_name variable indicator region avg_org_cap
drop if missing(avg_org_cap)
**************
*merge
merge 1:1 country_code region using x_data
sort region 
order country_code country_name region avg_dec_mkg_prcss avg_org_cap
drop _merge

