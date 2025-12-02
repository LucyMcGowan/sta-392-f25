
#delimit;
clear; capture log close; macro drop _all; program drop _all; set more off; 
set type double; set varabbrev off;
#delimit cr

ssc install outreg2

* File Path
global PROJECT_DIR "/zfs/projects/faculty/malhotra-usda/jares_malhotra_dataverse"
global DATA "${PROJECT_DIR}/data"
global LOG_OUTDIR "${PROJECT_DIR}/programs/2_run_regressions/stata_logs"
cd "${PROJECT_DIR}/tables"

log using "${LOG_OUTDIR}/MFP Helpfulness Regressions.txt", text replace

* Read in dataset
use "${DATA}/input/Li et al (2023) Midwestern Farmer Survey Responses with Policy Outcome Estimates.dta", clear

* Create indicators for missing demographics. Use Multiple Imputation with PMM.
gen missing_gender = missing(female)

gen missing_education = missing(education_5_pt_scale)

gen missing_age = missing(age)

* This is where the error of the original code occured
gen missing_off_farm = missing(had_off_farm_income_2018)

gen missing_demographic = max(missing_gender, missing_education, missing_age, missing_off_farm)

* Create control for long-standing farm size.
gen log_total_acres_13_17 = log(total_acres_13_17)

* The following are from the original workflow; not used in Table 2.
gen arcsinh_corn_rev_18 = asinh(corn_rev_05_18_prod_18)
gen arcsinh_soy_rev_18 = asinh(soy_rev_05_18_prod_18)
gen arcsinh_corn_rev_18_100 = asinh(corn_rev_05_18_prod_18 / 100)
gen arcsinh_soy_rev_18_100 = asinh(soy_rev_05_18_prod_18 / 100)
gen arcsinh_corn_rev_18_1K = asinh(corn_rev_05_18_prod_18 / 1000)
gen arcsinh_soy_rev_18_1K = asinh(soy_rev_05_18_prod_18 / 1000)
gen arcsinh_corn_rev_18_10K = asinh(corn_rev_05_18_prod_18 / 10000)
gen arcsinh_soy_rev_18_10K = asinh(soy_rev_05_18_prod_18 / 10000)
gen arcsinh_corn_rev_18_100K = asinh(corn_rev_05_18_prod_18 / 100000)
gen arcsinh_soy_rev_18_100K = asinh(soy_rev_05_18_prod_18 / 100000)
gen arcsinh_corn_rev_18_1M = asinh(corn_rev_05_18_prod_18 / 1000000)
gen arcsinh_soy_rev_18_1M = asinh(soy_rev_05_18_prod_18 / 1000000)

* Get lagged inverse hyperbolic sine transformation crop measures.
gen arcsinh_corn_rev_13_17_100K = asinh(corn_rev_05_18_prod_13_17 / 100000)
gen arcsinh_soy_rev_13_17_100K = asinh(soy_rev_05_18_prod_13_17 / 100000)

* Binary treatment.
gen mfp_made_whole = MFP_net_damage_prod_18 > 0
gen mfp_made_whole_13_17 = MFP_net_damage_prod_13_17 > 0

* Generate state indicators.
gen iowa = (state_abbrev == "IA")
gen illinois = (state_abbrev == "IL")

save "${DATA}/output/Li et al (2023) Midwestern Farmer Survey Responses with Policy Outcome Estimates -- with Regression Fields.dta", replace


********************************************************************************
* Our replication with multiple imputation using predictive mean matching.
* female, education_5_pt_scale, age, and had_off_farm_income_2018
********************************************************************************

mi set mlong

mi register imputed MFP_helpful_4_pt_scale

* PMM with 20 imputations.
* Predictors include the outcome, policy outcome measures, farm size,
* livestock indicators, state indicators, and missingness indicators.
mi impute chained (pmm, knn(10)) MFP_helpful_4_pt_scale = MFP_net_damage_prod_18_p MFP_share_damage_prod_18 mfp_made_whole log_total_acres_13_17 hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018 iowa illinois missing_gender missing_education missing_age missing_off_farm female education_5_pt_scale age had_off_farm_income_2018, add(20) rseed(12345)


********************************************************************************
* For Table 2, we have:
* Four-level MFP perception
* Economic policy outcome treatments
* Standard controls
* Now estimated using MI with PMM-imputed demographics.
********************************************************************************

local demographic_vars = "female education_5_pt_scale age had_off_farm_income_2018 iowa illinois"
local livestock_vars = "hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018"
local controls = "log_total_acres_13_17 `demographic_vars' `livestock_vars'"
local missing_field_indicators = "missing_gender missing_age missing_off_farm"

* Table 2, column 1
* Range of MFP_net_damage_prod_18_p: 0.005 to 1
mi estimate, vce(robust): reg MFP_helpful_4_pt_scale MFP_net_damage_prod_18_p
outreg2 using "Unformatted Table 2.tex", dec(3) replace

* Table 2, column 2
mi estimate, vce(robust): reg MFP_helpful_4_pt_scale MFP_net_damage_prod_18_p `controls' `missing_field_indicators'
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table 2.tex", dec(3) append

* Table 2, column 3
* Range of MFP_share_damage_prod_18: 0.09 to 1.82
mi estimate, vce(robust): reg MFP_helpful_4_pt_scale MFP_share_damage_prod_18
outreg2 using "Unformatted Table 2.tex", dec(3) append

* Table 2, column 4
mi estimate, vce(robust): reg MFP_helpful_4_pt_scale MFP_share_damage_prod_18 `controls' `missing_field_indicators'
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table 2.tex", dec(3) append

* Table 2, column 5
* mfp_made_whole is binary (0 or 1)
mi estimate, vce(robust): reg MFP_helpful_4_pt_scale mfp_made_whole
outreg2 using "Unformatted Table 2.tex", dec(3) append

* Table 2, column 6
mi estimate, vce(robust): reg MFP_helpful_4_pt_scale mfp_made_whole `controls' `missing_field_indicators'
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table 2.tex", dec(3) append

log close
exit






















