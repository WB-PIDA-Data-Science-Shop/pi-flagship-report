*** Reproducability package

*** Set Directories [Note: c(pwd) referes to the directory where Do file is]
cd ..
global root "`c(pwd)'"

* Subfolders
global raw    "$root/Datain"
global clean  "$root/Output"
global dofile  "$root/Do"


* Check if wbopendata is installed 
local packages "wbopendata"

foreach p of local packages {
    capture which `p'
    if _rc {
        display as text "Installing `p' from SSC..."
        ssc install `p', replace
    }
    else {
        display as text "`p' already installed."
    }
}


*** 1) Figure 3.3 
*** Data sources: World Development Indicators, we get from here primarily income groups and country level data althrough in the end was not used

***Download WDI Data 
wbopendata, ///
    language(en) ///
    indicator(EG.ELC.ACCS.ZS; EG.ELC.RNEW.ZS;  EG.ELC.FOSL.ZS; ///
              EG.FEC.RNEW.ZS; EG.GDP.PUSE.KO.PP; ///
              IS.ROD.PAVE.ZS; IS.ROD.TOTL.KM; IS.AIR.PSGR; IS.SHP.GOOD.TU; ///
              IS.AIR.GOOD.MT.K1; IS.RRS.TOTL.KM;IS.SHP.GCNW.XQ;IT.NET.USER.ZS;NY.GDP.PCAP.PP.KD) ///
    clear long 
	
	drop if region == ""
	drop if regionname == "Aggregates"

*** Get latest available year of data 	
foreach x in eg_elc_accs_zs eg_elc_rnew_zs eg_elc_fosl_zs eg_fec_rnew_zs eg_gdp_puse_ko_pp is_rod_pave_zs is_rod_totl_km is_air_psgr is_shp_good_tu is_air_good_mt_k1 is_rrs_totl_km ny_gdp_pcap_pp_kd{
	by countrycode, sort: egen firsttime_`x' = min(cond(`x' != ., year, .))
	by countrycode: egen lasttime_`x' = max(cond(`x' != ., year, .))
	
	 gen byte first_`x' = year == firsttime_`x'
	gen byte last_`x' = year == lasttime_`x'
}	
	
	foreach x in eg_elc_accs_zs eg_elc_rnew_zs eg_elc_fosl_zs eg_fec_rnew_zs eg_gdp_puse_ko_pp is_rod_pave_zs is_rod_totl_km is_air_psgr is_shp_good_tu is_air_good_mt_k1 is_rrs_totl_km ny_gdp_pcap_pp_kd{
	preserve
    keep if last_`x' == 1
    gen varname = "`x'"
    gen value = `x'
    keep countrycode year value varname
    tempfile tmp_`x'
    save `tmp_`x'', replace
    restore
	}
	

*** Append and reshape latest avilable data per variable 	
clear
tempfile master tempfile
save `master', emptyok
foreach x in eg_elc_accs_zs eg_elc_rnew_zs eg_elc_fosl_zs eg_fec_rnew_zs ///
            eg_gdp_puse_ko_pp is_rod_pave_zs is_rod_totl_km is_air_psgr ///
            is_shp_good_tu is_air_good_mt_k1 is_rrs_totl_km ny_gdp_pcap_pp_kd{
    append using `tmp_`x''
}
	
reshape wide value year, i(countrycode) j(varname) string
ren value* *
label variable eg_elc_accs_zs       "Access to electricity (% of population)"
label variable eg_elc_fosl_zs       "Fossil fuel electricity output (% of total)"
label variable eg_elc_rnew_zs       "Renewable electricity output (% of total)"
label variable eg_fec_rnew_zs       "Renewable energy consumption (% of final energy)"
label variable eg_gdp_puse_ko_pp    "Energy use per $1,000 GDP (2017 PPP)"
label variable is_air_good_mt_k1    "Air freight (million ton-km)"
label variable is_air_psgr          "Air passengers carried"
label variable is_rod_pave_zs       "Paved roads (% of total roads)"
label variable is_rod_totl_km       "Total road network (km)"
label variable is_rrs_totl_km       "Rail lines (total route-km)"
label variable is_shp_good_tu       "Container port traffic (TEU)"
label var ny_gdp_pcap_pp_kd "GDP per capita, PPP (constant 2021 international $)"	


*** Store temp file 	
save "`tempfile'",replace 


***  Clean PMR
*** Data downlaoded from https://www.oecd.org/en/topics/product-market-regulation.html | https://www.oecd.org/content/dam/oecd/en/topics/policy-sub-issues/product-market-regulation/PMR_Sector_Indicator_2023-24_and_2018_March2026.xlsx

** Column names were modified to fit Stata format. 

*** Please note that the data has been updated since. We accessed the PMR on August 2025.

clear
import excel "$raw/Updated_PMR Sector Indicator_2023-24 and 2018.xlsx", sheet("PMR_Sector_Network_2023-24_clea") firstrow

*replace countryname = countryname+"*" if eca == 1
replace countryname = "Uzbekistan" if countryname == "Uzbekistan*"
replace countryname = "Peru" if countryname == "Peru*"
replace countryname = "Colombia" if countryname == "Colombia*"
replace countryname = "Malaysia*" if countryname == "Malaysia"

merge 1:m countrycode using "`tempfile'"	
	
*** 
keep if _merge==3

drop _merge 

gen Codecountry = countrycode 

*merge 1:1 Codecountry using "$raw/Final_dataset_filter.dta",gen(m2)
*keep if m2==3

save "`tempfile'",replace

*** Import ICT prices :https://www.itu.int/en/ITU-D/Statistics/Pages/ICTprices/default.aspx--> this was downloaded on June 12 2025.
clear 
import excel "$raw/ITU_ICTPriceBaskets_2008-2024.xlsx", sheet("economies_2008-2024") firstrow

*** reshape 
reshape long y_,i(countrycode Economy basket_combined_simplified Unit Code ITURegion LDC LLDC SIDS Income_2024) j(date)

gen varname = lower(Code)
replace varname = subinstr(varname, "$", "_usd", .)

keep countrycode date y_ varname
replace y_ = subinstr(y_,"NA","",.)
destring y_,replace
ren y_ data
ren date year 

reshape wide data,i(countrycode year) j(varname) string
ren data* *


label variable i271mb_ts_gni "Data-only mobile-broadband basket"
label variable i271mb_ts_ppp "Data-only mobile-broadband basket"
label variable i271mb_ts_usd "Data-only mobile-broadband basket"

label variable i154_fbb_ts_gni "Fixed-broadband basket"
label variable i154_fbb_ts_ppp "Fixed-broadband basket"
label variable i154_fbb_ts_usd "Fixed-broadband basket"

label variable i271mb_high_ts_gni "Mobile data and voice high-consumption basket"
label variable i271mb_high_ts_ppp "Mobile data and voice high-consumption basket"
label variable i271mb_high_ts_usd "Mobile data and voice high-consumption basket"

label variable i271mb_low_ts_gni "Mobile data and voice low-consumption basket"
label variable i271mb_low_ts_ppp "Mobile data and voice low-consumption basket"
label variable i271mb_low_ts_usd "Mobile data and voice low-consumption basket"

label variable i154_moblow_ts_gni "Mobile-cellular low-usage basket"
label variable i154_moblow_ts_ppp "Mobile-cellular low-usage basket"
label variable i154_moblow_ts_usd "Mobile-cellular low-usage basket"

keep if year == 2023


*** merge 
merge 1:1 countrycode using "`tempfile'",gen(m3)
keep if m3==3

*** Figure
scatter energy_electricity eg_elc_accs_zs if income != "HIC"
scatter e_communications_overall  i271mb_ts_gni
scatter e_communications_fixed_ecomm  i271mb_ts_gni
*scatter e_communications_mobile_ecomm i_Affordability
scatter e_communications_mobile_ecomm i271mb_ts_ppp 
scatter e_communications_mobile_ecomm i271mb_ts_usd

scatter i271mb_ts_ppp e_communications_mobile_ecomm  || lfit i271mb_ts_ppp e_communications_mobile_ecomm  




*** Option 
separate e_communications_mobile_ecomm,by(income)
label var e_communications_mobile_ecomm1 "HIC"
label var e_communications_mobile_ecomm2 "LMC"
label var e_communications_mobile_ecomm3 "UMC"


*** Seprate GNI price variable by income group 
separate i271mb_ts_gni,by(income)

*** Label newly created variables 
label var i271mb_ts_gni1 "HIC"
label var i271mb_ts_gni2 "LMC"
label var i271mb_ts_gni3 "UMC"

*** Drop Albania, outlier
drop if countrycode == "ALB"

*** Scatter 
scatter  i271mb_ts_gni1  e_communications_mobile_ecomm,  xlab(0(1)5) xtitle(Mobile e-communications (PMR)) ytitle(Data-only mobile broadband basket(% GNI p.c.)) || ///
scatter i271mb_ts_gni2 e_communications_mobile_ecomm,  || ///
scatter i271mb_ts_gni3 e_communications_mobile_ecomm,  || lfit  i271mb_ts_gni e_communications_mobile_ecomm,  legend(position(6) rows(1))
graph save "Graph" "$clean/Charts/Figure_3.3_comms_price.gph", replace
graph export "$clean/Charts/Figure_3.3_comms_price.png", as(png) name("Graph") replace
graph export "$clean/Charts/Figure_3.3_comms_price.eps", as(eps) name("Graph") replace


**** Figure 3.6 

*** clean GSR data 
* data downloaded from https://www.oecd.org/en/data/datasets/oecd-government-at-a-glance-database.html | https://www.oecd.org/content/dam/oecd/en/publications/support-materials/2025/03/the-2023-indicators-on-the-governance-of-sector-regulators_eb33f9bb/2023%20Indicators%20on%20the%20Governance%20of%20Sector%20Regulators%20(February%202025).xlsx

* data was downloaded on september 2025. Excel was cleaned such that it can be imported into STATA

**** GSR OECD
clear
import excel "$raw/GSR_Data.xlsx", sheet("Database_clean") firstrow
tempfile temp


*** store variable labels 
preserve
keep Questioncode Sector QuestionText_2023
duplicates drop Questioncode QuestionText_2023, force
replace Questioncode = subinstr(Questioncode, ".", "_", .)
replace Sector       = Sector + ": "

*---------------------------------------------
* Create a do-file with label + notes commands
*---------------------------------------------
file open mydo using "$dofile/2_label_vars.do", write replace
file write mydo "***************************************************" _n
file write mydo "* Automatically generated variable label commands *" _n
file write mydo "* Also overwrites NOTES with full question text   *" _n
file write mydo "***************************************************" _n
file write mydo "capture noisily {" _n

quietly {
    forvalues i = 1/`=_N' {
        local v = Questioncode[`i']
        local a = Sector[`i']
        local l = QuestionText_2023[`i']

        * Build full text "Sector: Question" and escape quotes
        local full     = strtrim(stritrim(`"`a'`l'"'))
        local full_esc = subinstr(`"`full'"', `"""', `""""', .)

        * 80-char cap for variable label (notes keep the full text)
        local lbl80 `"`full_esc'"'
        if strlen(`"`lbl80'"') > 80 local lbl80 = substr(`"`lbl80'"', 1, 80)

        file write mydo "* ---------- `v' ----------" _n
        file write mydo `"capture confirm variable `v'"' _n
        file write mydo `"if !_rc {"' _n
        file write mydo `"    label variable `v' "`lbl80'""' _n
        file write mydo `"    notes `v': "`full_esc'""' _n
        file write mydo `"}"' _n _n
    }
}

file write mydo "}" _n
file close mydo

* Optional: keep/export the mapping you used
keep Questioncode Sector QuestionText_2023
export excel using "$clean/Questions.xlsx", firstrow(variables) replace
restore

/*
preserve 
keep Questioncode Sector QuestionText_2023
duplicates drop Questioncode QuestionText_2023,force 
replace Questioncode = subinstr(Questioncode,".","_",.)
replace Sector = Sector + ": "

* Create a do-file with label commands
file open mydo using "$dofile/label_vars.do", write replace
file write mydo "***************************************************" _n
file write mydo "* Automatically generated variable label commands *" _n
file write mydo "***************************************************" _n

quietly {
    forvalues i = 1/`=_N' {
        local v = Questioncode[`i']
        local l = QuestionText_2023[`i']
		local a = Sector[`i']
        file write mydo `"label variable `v'  "`a'`l'""' _n
    }
}

quietly {
    forvalues i = 1/`=_N' {
        local v = Questioncode[`i']
        local l = QuestionText_2023[`i']
		local a = Sector[`i']
        file write mydo `"note `v'  "`a'`l'""' _n
    }
}


file close mydo
keep Questioncode Sector QuestionText_2023
export excel using "$clean/Questions.xlsx", firstrow(variables) replace
restore 
*/



*** drop second answers for brazil
drop reply_BRA2_2023 reply_BRA2_2018 Sector 

** reshape data
reshape long reply_,i(Questioncode  QuestionText_2023) j(country) string

*** convert points to blank
replace reply_ = subinstr(reply_,".","",.)
destring reply_,replace

*** split country 
split country,p("_")
drop country 

ren country1 countrycode 
ren country2 year 
destring year,replace

*** reshape again 
drop QuestionText_2023
ren reply_ x_
replace Questioncode = subinstr(Questioncode,".","_",.)

reshape wide x_,i(countrycode year) j(Questioncode) string
ren x_* *


save "`temp'",replace 


*** Import WDR
wbopendata, language(en - English) country() topics() indicator(NY.GDP.PCAP.PP.KD;NY.GNP.PCAP.CD) clear long
keep if year == 2023 | year == 2018
drop if regionname == "Aggregates"
drop if regionname == ""
ren ny_gdp_pcap_pp_kd gdp_pc_ppp
ren ny_gnp_pcap_cd gni_pc_current


*** merge with GDP per capita 
merge 1:m year countrycode using "`temp'"


*** Run do file for labels 
qui do "$dofile/2_label_vars.do"

*** drop those we dont have SGR for
keep if _merge==3

*** Save 
save "$clean/GSR_Database_Clean.dta",replace 



*** Draw charts 
clear
use "$clean/GSR_Database_Clean.dta",replace 
tempfile temp 

preserve 
encode countryname,gen(country_code)
tset country_code year 
foreach x in Q11b_b_9 Q1b_b_9 Q3ab_b_9 Q3bb_b_9 {
	encode `x',gen(cod_`x')
	gen new_cod_`x' = cod_`x'
}


gen diff = new_cod_Q1b_b_9 - L.new_cod_Q1b_b_9
gen x = D.new_cod_Q1b_b_9

restore 

keep if year == 2023

/*
1) Management
2) Info systems
3) Financial resources
*/

*** GDP per capita
*gen ln_gdp = ln(gdp_pc_ppp)
xtile quint = gni_pc_current,nq(3)

*gen quint = 1 if incomelevel== "UMC"
*replace quint = 2 if incomelevel== "HIC"


*** Financial resources 

/*
Is the source of the financial budget stated in the establishing legislation?
	Q2b_a_21
	Q3ab_a_21
	Q11b_a_21
	Q3bb_a_21
	Q1b_a_21

Is the regulator funded through fees, the national budget or a mix of both?
	Q3ab_a_22
	Q11b_a_22
	Q1b_a_22
	Q3bb_a_22
	Q2b_a_22


Which body is responsible for deciding the regulator's allocation of expenditures?
	Q2b_a_27
	Q3ab_a_27
	Q3bb_a_27
	Q1b_a_27

*/
drop  _merge

save "`temp'",replace 

*** merge pmr 
import excel "$raw/Updated_PMR Sector Indicator_2023-24 and 2018.xlsx", sheet("Clean_sector") firstrow clear
foreach x in overall_network energy_overall energy_electricity energy_naturalgas transport_overall transport_rail transport_air transport_road transport_water e_communications_overall e_communications_fixed_ecomm e_communications_mobile_ecomm{
	destring `x',replace
}

drop if Group == ""
	

merge 1:m countrycode using "`temp'"
	
	
keep if _merge == 3	
drop _merge 


xtile energy_quint = energy_overall,nq(3)
xtile comms_quint = e_communications_overall,nq(3)
xtile air_quint = transport_air,nq(3)
xtile rail_quint = transport_rail,nq(3)  

*** sanctions 
tab1 Q11b_c_12 Q1b_c_12 Q2b_c_12 Q3ab_c_12 Q3bb_c_12

*** keep variables 
local var_keep Q1b_a_22 Q2b_a_22 Q3ab_a_22 Q3bb_a_22 Q1b_a_21 Q2b_a_21 Q3ab_a_21 Q3bb_a_21 Q3bb_b_2 Q2b_b_2 Q1b_b_2 Q3ab_b_2   Q3bb_b_10_i Q1b_b_10_i Q2b_b_10_i Q3ab_b_10_i Q3ab_b_11a_i Q3bb_b_11a_i Q2b_b_11a_i Q1b_b_11a_i    Q1b_b_3 Q2b_b_3 Q3bb_b_3 Q3ab_b_3 Q3bb_b_9 Q3ab_b_9 Q1b_b_9  Q2b_b_9 Q3bb_c_3 Q2b_c_3 Q1b_c_3 Q3ab_c_3 Q11b_b_9 Q11b_a_22

keep `var_keep' energy_quint comms_quint air_quint rail_quint countrycode countryname quint

*** Tabs and fix 
codebook Q1b_a_21 Q2b_a_21 Q3ab_a_21 Q3bb_a_21

codebook Q1b_a_22 Q2b_a_22 Q3ab_a_22 Q3bb_a_22

codebook Q1b_b_9 Q2b_b_9 Q3ab_b_9 Q3bb_b_9


gen dummy = 0
replace dummy = 1 if inlist(Q1b_b_9, ///
    "no strategic objectives defined", ///
    "strategic objectives defined but not measured/reported on", ///
    "yes, internally/for internal use")

graph bar , over(dummy) over(quint) asyvars stack percentage ///
    legend(position(6) col(1) region(lstyle(none))) blabel(bar, format(%4.1f) position(center))

tab1  Q1b_b_9 Q3ab_b_9 Q3bb_b_9		

*** Tables: 1) strategy
preserve 

keep countryname Q1b_b_9 Q3ab_b_9 Q3bb_b_9	Q2b_b_9 Q11b_b_9

reshape long Q,i(countryname) j(sector) string

replace sector = "Energy" if sector == "1b_b_9"
replace sector = "Rail" if sector == "3ab_b_9"
replace sector = "E_Comms" if sector == "2b_b_9"
replace sector = "Air" if sector == "3bb_b_9"

gen x = 1

collapse (sum) x,by(sector Q)
replace Q = "Missing" if Q == ""

reshape wide x,i(Q) j(sector) string
drop if Q == "Missing"

*** order 
gen order = 0
replace order = 1 if Q == "no strategic objectives defined"
replace order = 2 if Q == "strategic objectives defined but not measured/reported on"
replace order = 3 if Q == "yes, internally/for internal use"
replace order = 4 if Q == "yes, information published on website"
replace order = 5 if Q == "yes, information reported to government ministry/parliament (accountable body)"


sort order
	
restore	
	
*** FIn mechanisms
preserve 
keep countryname Q3bb_a_22 Q2b_a_22 Q1b_a_22 Q3ab_a_22

reshape long Q,i(countryname) j(sector) string

replace sector = "Energy" if sector == "1b_a_22"
replace sector = "Rail" if sector == "3ab_a_22"
replace sector = "E_Comms" if sector == "2b_a_22"
replace sector = "Air" if sector == "3bb_a_22"

gen x = 1

collapse (sum) x,by(sector Q)
replace Q = "Missing" if Q == ""

reshape wide x,i(Q) j(sector) string
drop if Q == "Missing"

*** order 
gen order = 0
replace order = 1 if Q == "national budget"
replace order = 2 if Q == "fees"
replace order = 3 if Q == "both"

sort order

restore 	
	

*** Fin mech legislation	
preserve 
keep countryname Q3bb_a_22 Q2b_a_22 Q1b_a_22 Q3ab_a_22

reshape long Q,i(countryname) j(sector) string

replace sector = "Energy" if sector == "1b_a_22"
replace sector = "Rail" if sector == "3ab_a_22"
replace sector = "E_Comms" if sector == "2b_a_22"
replace sector = "Air" if sector == "3bb_a_22"

gen x = 1

collapse (sum) x,by(sector Q)
replace Q = "Missing" if Q == ""

reshape wide x,i(Q) j(sector) string
drop if Q == "Missing"

*** order 
gen order = 0
replace order = 1 if Q == "national budget"
replace order = 2 if Q == "fees"
replace order = 3 if Q == "both"

sort order

restore 	

graph bar , over(Q2b_a_22) over(quint) asyvars stack percentage ///
    legend(position(6) col(1) region(lstyle(none))) ///
    blabel(bar, format(%4.1f) position(center)) ///
    note(`"Notes:(`x')`noteText'"') ///
    ytitle("Percent of countries") ///
    bar(1, color(eltblue)) bar(2, color(navy)) bar(3, color(emerald))
	

*** Energy
foreach x in  Q1b_a_22 {
	local noteText : char `x'[note1]
graph bar , over(`x') over(quint) asyvars stack percentage ///
    legend(position(6) col(1) region(lstyle(none))) ///
    blabel(bar, format(%4.1f) position(center)) ///
    ytitle("Percent of countries") ///
    bar(1, color(eltblue)) ///
    bar(2, color(midblue)) ///
    bar(3, color(green))	
	graph export "$clean/Charts/Fig_3_6_Energy_Sources_graph_`x'.png", replace
	graph export "$clean/Charts/Fig_3_6_Energy_Sources_graph_`x'.eps", as(eps) replace

}				
			
*** E-comms 
foreach x in  Q2b_a_22  {
	local noteText : char `x'[note1]
	graph bar , over(`x') over(quint) asyvars stack percentage ///
    legend(position(6) col(1) region(lstyle(none))) ///
    blabel(bar, format(%4.1f) position(center)) ///
    ytitle("Percent of countries") ///
    bar(1, color(eltblue)) ///
    bar(2, color(midblue)) ///
    bar(3, color(green))	
	graph export "$clean/Charts/Fig_3_6_Comms_Sources_graph_`x'.png", replace
	graph export "$clean/Charts/Fig_3_6_Comms_Sources_graph_`x'.eps", as(eps) replace

}				
				

*** Rail 
foreach x in  Q3ab_a_22    {
	local noteText : char `x'[note1]
	graph bar , over(`x') over(quint) asyvars stack percentage ///
    legend(position(6) col(1) region(lstyle(none))) ///
    blabel(bar, format(%4.1f) position(center)) ///
    ytitle("Percent of countries") ///
    bar(1, color(eltblue)) ///
    bar(2, color(midblue)) ///
    bar(3, color(green))	
	graph export "$clean/Charts/Fig_3_6_Rail_Sources_graph_`x'.png", replace
	graph export "$clean/Charts/Fig_3_6_Rail_Sources_graph_`x'.eps", as(eps) replace

}				

*** Air 
foreach x in  Q3bb_a_22   {
	local noteText : char `x'[note1]
graph bar , over(`x') over(quint) asyvars stack percentage ///
    legend(position(6) col(1) region(lstyle(none))) ///
    blabel(bar, format(%4.1f) position(center)) ///
    ytitle("Percent of countries") ///
    bar(1, color(eltblue)) ///
    bar(2, color(midblue)) ///
    bar(3, color(green))	
	graph export "$clean/Charts/Fig_3_6_Air_Sources_graph_`x'.png", replace
	graph export "$clean/Charts/Fig_3_6_Air_Sources_graph_`x'.eps", as(eps) replace

}	
		
		
		
		
