********************************************************************************
*Authro: Awais // creatd 11-10-2023 and edited on 111-2023 after team's feedback on data analyis.
********************************************************************************
version 17
clear all
set more off
*Create a project directory
cd "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\Overview_Data_Analysis"
cap log close

*using the data classification.xlsx(this is xls file) with comments from the team. 

clear
* Import the worksheet `Sheet1` from the Excel file `my_excel_file.xlsx`
import excel "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\Overview_Data_Analysis\data classification.xlsx", sheet("Date_11_9_2023")
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
cd "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\Overview_Data_Analysis"
use closeness_to_frontier_Static_Nov_11.dta, clear
*static

*dmp
egen missing_dmp=rownonmiss (fh_fiw_pr_rating fh_fiw_cl_rating wb_wbl_social rwb_pfi_index vdem_core_v2x_cspart vdem_core_v2x_gender vdem_core_v2pepwrses vdem_core_v2cacamps vdem_core_v2csreprss vdem_core_v2dlengage vdem_core_v2cseeorgs vdem_core_v2cldiscm vdem_core_v2pepwrgen vdem_core_v2cldiscw vdem_core_v2pepwrsoc vdem_core_v2clacfree vdem_core_v2xlg_legcon vdem_core_v2lgqugen wjp_rol_1 wjp_rol_4_5 wjp_rol_4_7  wjp_rol_4_6 wjp_rol_4_4)

egen dmp_23=rowmean(fh_fiw_pr_rating fh_fiw_cl_rating wb_wbl_social rwb_pfi_index vdem_core_v2x_cspart vdem_core_v2x_gender vdem_core_v2pepwrses vdem_core_v2cacamps vdem_core_v2csreprss vdem_core_v2dlengage vdem_core_v2cseeorgs vdem_core_v2cldiscm vdem_core_v2pepwrgen vdem_core_v2cldiscw vdem_core_v2pepwrsoc vdem_core_v2clacfree vdem_core_v2xlg_legcon vdem_core_v2lgqugen wjp_rol_1 wjp_rol_4_5 wjp_rol_4_7  wjp_rol_4_6 wjp_rol_4_4) if missing==23

*og
egen missing_og=rownonmiss(wb_gtmi_gtei wb_gtmi_psdi wb_gtmi_i_8 wb_gtmi_dcei wb_gtmi_cgsi wb_gtmi_pfm_mis spi_census_and_survey_index spi_std_and_methods vdem_core_v2peasjpol vdem_core_v2stcritrecadm vdem_core_v2peasjsoecon wb_lpi_lp_lpi_cust_xq rise_ee_4_3 wjp_rol_7_5 wjp_rol_3_2 wjp_rol_7_6 wjp_rol_3_4 wjp_rol_3_1 ibp_obs_obi)

egen org_cap_19=rowmean(wb_gtmi_gtei wb_gtmi_psdi wb_gtmi_i_8 wb_gtmi_dcei wb_gtmi_cgsi wb_gtmi_pfm_mis spi_census_and_survey_index spi_std_and_methods vdem_core_v2peasjpol vdem_core_v2stcritrecadm vdem_core_v2peasjsoecon wb_lpi_lp_lpi_cust_xq rise_ee_4_3 wjp_rol_7_5 wjp_rol_3_2 wjp_rol_7_6 wjp_rol_3_4 wjp_rol_3_1 ibp_obs_obi) if missing_og==19

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
rename Var value_0_1

drop if MISSING_DMP == 0 & MISSING_OG == 0

save closeness_to_frontier_Static_long, replace






********************************************************************************
//STATIC-Analysis
********************************************************************************






use closeness_to_frontier_Static_long, clear
ren *, lower
order country_name region incomegroup lendingcategory  country_code country_group  variable value_0_1
gen lowercase_var = lower(variable)
drop variable 
rename lowercase_var variable
*temp_indicators are data classification.xlsx (note: org capacity, decision making, people, money and system)
merge m:1 variable using "temp_indicators.dta"
drop if _m == 2
drop _merge
encode region, gen(region_v1)
drop region
rename region_v1 region
****gdppercap_constant2015 (2022 - this is a clean file after removing missing gdp data of certain small islands etc)
*merge m:1 country_code using "gdppercap_constant2015.dta"
*drop if _m == 2
*drop _merge

order country_name incomegroup lendingcategory country_code country_group region

*these are regions: are under the country_name so we dropped them:Low income, Sub-Saharan Africa,,East Asia & Pacific,,Low & middle income,,European Union,Central Europe, and the Baltics,	High income	South Asia	Middle East & North Africa	North America	Latin America & Caribbean	Lower middle income	OECD members	Europe & Central AsiaArab World	Upper middle income	Middle income
drop if region==.

*Drop small islands from the dataset as discussed 

drop if inlist(country_code, "TUV", "NRU", "KIR", "NHL", "FSM") | ///
inlist(country_code, "STP", "VCT", "TCA", "SXM", "LCA", "CPV", "PYF", "ASM","MNP") | ///
inlist(country_code, "NCL","VGB","KNA","CYM","BMU", "MDG", "MHL")
*note: (1,628 observations deleted)

local var_list "lendingcategory economy variable incomegroup country_code country_name"

foreach var in `var_list' {
    encode `var', generate(str_var)
    drop `var'
    rename str_var `var'
	order country_name incomegroup country_code  variable region economy lendingcategory Indicator variable Organizational_Capacity People Money Systems Decision_Making_processes PI_performance
	
}

ren*, lower 
rename organizational_capacity org_cap
rename decision_making_processes  dec_mkg_prcss

////*below code is important we are dropping the variables that are missing(.) from the data, it will leave only data that is available.
*by country_name (country_code), sort: drop if sum(!mi(value_0_1)) == 0 


// List of sub-categories
local sub_categories "bs_bti_q12_1 bs_bti_q15_1 bs_bti_q2_1] bs_bti_q2_3 bs_bti_q3_1 bs_bti_q3_2 bs_bti_q8_1 bs_bti_q8_2 bs_bti_si fraser_efw vars_anticorruption_avg vars_climate_avg vars_digital_avg vars_hrm_avg vars_leg_avg vars_mkt_avg vars_pfm_avg vars_pol_avg vars_service_delivery_avg vars_social_avg vars_transp_avg wb_pefa_pi_2016_05 wb_pefa_pi_2016_10 wb_pefa_pi_2016_11 wb_pefa_pi_2016_12 wb_pefa_pi_2016_13 wb_pefa_pi_2016_14 wb_pefa_pi_2016_15 wb_pefa_pi_2016_16 wb_pefa_pi_2016_17 wb_pefa_pi_2016_18 wb_pefa_pi_2016_19 wb_pefa_pi_2016_20 wb_pefa_pi_2016_21 wb_pefa_pi_2016_22 wb_pefa_pi_2016_23 wb_pefa_pi_2016_24 wb_pefa_pi_2016_25 wb_pefa_pi_2016_26 wb_pefa_pi_2016_27 wb_pefa_pi_2016_28 wb_pefa_pi_2016_29 wb_pefa_pi_2016_30 wb_wbl wjp_rol_2 vdem_core_v2clacjstm vdem_core_v2clacjstw vdem_core_v2peapsgen vdem_core_v2peapspol vdem_core_v2peapsecon vdem_core_v2peasjpol vdem_core_v2peasjsoecon wb_aspire_adequacy_benefits wjp_rol_7_7	rise_re_4 wb_gfdb_oi_01	wdi_shstabrtczs	rise_ee_9 vdem_core_v2cseeorgs vdem_core_v2csreprss rise_re_7 spi_census_and_survey_index fh_fiw_cl_rating wjp_rol_7_6 wjp_rol_7_5 vdem_core_v2x_cspart	bs_bti_q7_2 wjp_rol_3_4 wdi_spregbrthzs wjp_rol_1 wb_gtmi_cgsi wjp_rol_8_2 wjp_rol_8_1 wjp_rol_8_4 vdem_core_v2stcritrecadm wb_gtmi_i_8	wb_debt_transp_index wb_gtmi_dcei oecd_pmr_2018_1_3 wjp_rol_4_3	wb_lpi_lp_lpi_cust_xq oecd_epl_regular oecd_epl_temporary rise_ee_2 rise_ee_8 vdem_core_v2dlengage vdem_core_v2exbribe wjp_rol_6_6 idea_gsod_v_21_05 rise_ee_6 wjp_rol_4_6 vdem_core_v2clacfree	wjp_rol_4_7 wjp_rol_4_5	vdem_core_v2cldiscm vdem_core_v2cldiscw	wjp_rol_4_4 wjp_rol_4_8	wb_gtmi_gtei oecd_pmr_2018_1_4 oecd_pmr_2018_1_2 wjp_rol_6_2 vdem_core_v2juhcind rise_ee_5 rise_ee_3 rise_ee_4 rise_re_3 vdem_core_v2juaccnt wjp_rol_2_2 rise_re_1 vdem_core_v2xlg_legcon vdem_core_v2lgqugen vdem_core_v2juncind rise_ee_7 rise_ee_1 ibp_obs_obi wb_gtmi_pfm_mis wjp_rol_7_1 rise_re_2 vdem_core_v2x_corr fh_fiw_pr_rating vdem_core_v2cacamps vdem_core_v2pepwrgen vdem_core_v2pepwrsoc vdem_core_v2pepwrses wdi_shstaanvczs rwb_pfi_index oecd_pmr_2018_2_1 vdem_core_v2xcl_prpty wb_gtmi_psdi rise_ee_4_3 wjp_rol_3_1 wdi_seprmenrltczs wdi_sesecenrltczs wjp_rol_6 wjp_rol_3_2 vdem_core_v2clrspct oecd_pmr_2018_1_1 wb_aspire_coverage spi_std_and_methods wdi_sepretcaqzs wdi_seprmtcaqzs wdi_sesectcaqzs oecd_pmr_2018_2_2 vdem_core_v2x_gender wb_wbl_labor wb_wbl_social idea_gsod_v_22_16"

// Initialize a flag variable
gen same_subcategories = 1

*note run this line seperately 
// Loop through each sub-category
foreach sub in `sub_categories'    {
    // Check if the sub-category varies by country
    qui tabulate `sub', by(country)
    qui replace same_subcategories = 0 if r(r2) > 1
}

// Check if any country has different sub-categories
count if same_subcategories == 0
// If any country has different sub-categories, the flag variable is set to 0. Finally, it counts how many countries have different sub-categories.
save CTF_Static_long_maindata, replace



********************************************************************************
//Bar chart and avg of organizational capacity and dec_mkg_prcss
********************************************************************************

use CTF_Static_long_maindata, clear
gen dummy = 1
replace dummy = 0 if value_0_1 == .
egen n_ctys = total(dummy == 1), by(variable)
lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable
keep if dummy == 1
drop if n_ctys < 100
//avg-org_cap
egen n_org_cap = total(org_cap), by(variable)
egen n_org_cap_notmissing = total(!missing(value)), by(country_name)
gen perc_org_cap = n_org_cap_notmissing /n_org_cap
sort n_org_cap  n_ctys
drop if missing(perc_org_cap)
sort country_code region

*plot-organizational capacity
histogram perc_org_cap, bin(60) frequency ///
    xtitle("Percent of 'organizational capacity' variables observed per country") ///
    ytitle("Frequency") ///
    title("organizational capacity (n_organizational capacity variables)") ///
    xlabel(0(0.1)1) ///
    ylabel(, noticks) ///
    graphregion(color(white))
	save temp_org_cap, clear , replace 
	
	
	
use CTF_Static_long_maindata, clear
gen dummy = 1
replace dummy = 0 if value_0_1 == .
egen n_ctys = total(dummy == 1), by(variable)
lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable
keep if dummy == 1
drop if n_ctys < 100
//avg-avg- dec_mkg_prcss
egen n_dec_mkg_prcss = total(dec_mkg_prcss), by(variable)
egen n_dec_mkg_prcss_notmissing = total(!missing(value)), by(country_name)
gen perc_dec_mkg_prcss = n_dec_mkg_prcss_notmissing /n_dec_mkg_prcss
sort n_dec_mkg_prcss  n_ctys
drop if missing(perc_dec_mkg_prcss)
sort country_code region

*plot-dec_mkg_prcss dec_mkg_prcss
histogram perc_dec_mkg_prcss, bin(60) frequency ///
    xtitle("Percent of'dec_mkg_prcss' variables observed per country") ///
    ytitle("Frequency") ///
    title("DMP(n_Decision Making processes variables)") ///
    xlabel(0(0.1)1) ///
    ylabel(, noticks) ///
    graphregion(color(white))
order  country_name incomegroup country_code region indicator variable org_cap value_0_1 dec_mkg_prcss n_dec_mkg_prcss n_dec_mkg_prcss_notmissing perc_dec_mkg_prcss


********************************************************************************
*Note: we can plot the same graph for the Peple, Money and Systems. Only need to  add  the var's (peple, money and system in the above code) 

********************************************************************************







********************************************************************************************************
*Static CTF : Decision Making Processes and Organizational Capacity  (avg) by regions and Global average 
*********************************************************************************************************
// List of variables to loop through
local var_list "dec_mkg_prcss org_cap"

foreach var in `var_list' {
    // Load the main dataset
    use CTF_Static_long_maindata, clear
    ren *, lower
    order country_name region incomegroup lendingcategory country_code country_group variable value_0_1
    gen group = missing(region)
    lab var group "these group so does not reflect in region category"
    gen dummy = 1
    replace dummy = 0 if value_0_1 == .
    egen n_ctys = total(dummy == 1), by(variable)
    
    // Sort and filter as needed
    sort variable
    drop if group == 1
    lab var n_ctys "shows the total number of countries where dummy is equal to 1 for each unique value of the variable variable"
    sort country_group economy variable
    
    // Filter by condition (e.g., n_ctys < 100)
    drop if n_ctys < 100
    
    // Perform additional operations specific to the variable
    if "`var'" == "dec_mkg_prcss" {
        // Example operations for dec_mkg_prcss
        *replace dec_mkg_prcss = 0 if dec_mkg_prcss == .
        keep if dec_mkg_prcss == 1
        egen avg_dec_mkg_prcss = mean(value_0_1), by(dec_mkg_prcss country_code region)
        contract country_code country_name region avg_dec_mkg_prcss
        drop if missing(avg_dec_mkg_prcss)
        save x_data, replace
    }
    else if "`var'" == "org_cap" {
        // Example operations for org_cap
        *replace org_cap = 0 if org_cap == .
        keep if org_cap == 1
        egen avg_org_cap = mean(value_0_1), by(org_cap country_code region)
        contract country_code country_name region avg_org_cap
        drop if missing(avg_org_cap)
    }
}

// Merge the results
merge 1:1 country_code region using x_data
sort region 
order country_code country_name region avg_dec_mkg_prcss avg_org_cap
drop _merge
drop _freq

// Export data to Excel
export excel using "C:\Users\wb543707\OneDrive - WBG\Pillars I-II-III\Part_1 Public institutions an Overview\CTF_static_data.xlsx", sheet("Static_CTF") replace



