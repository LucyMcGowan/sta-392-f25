/*******************************************************************************

Program:   "midwestern_farmer_survey_regressions.do"
Author:    Jake Jares
Date:      02/12/2024
Inputs:    Replication data from Li et al (2023) survey
           * "Li et al (2023) Midwestern Farmer Survey Responses with Policy Outcome Estimates.dta"

Ouputs:    8 unformatted regression tables (.tex file)
           * Regress 4-point MFP helpfulness on economic outcomes: "Unformatted Table 2.tex"
           * Regress 4-point MFP helpfulness on crop mix:          "Unformatted Table DM15.tex"
           * Regress binary MFP helpfulness on crop mix:           "Unformatted Table DM16.tex"
           * Regress binary MFP helpfulness on economic outcomes:  "Unformatted Table DM17.tex"
           * Show robustness to controlling for past plantings:    "Unformatted Table DM18.tex"
           * Regress tariff support on crop mix:                   "Unformatted Table DM19.tex"
           * Regress tariff support on economic outcomes:          "Unformatted Table DM20.tex"
           * Interaction with made whole status:                   "Unformatted Table DM21.tex"
           
           Dataset with created fields (used for generating Figure DM48)
           * "Li et al (2023) Midwestern Farmer Survey Responses with Policy Outcome Estimates -- with Regression Fields.dta"
           
Use the following syntax to run this on the Yens cluster.

    module load stata
    stata -b do midwestern_farmer_survey_regressions.do &

*******************************************************************************/


#delimit;
clear; capture log close; macro drop _all; program drop _all; set more off; 
set type double; set varabbrev off;
#delimit cr

ssc install outreg2

// CHANGE THE FOLLOWING FILEPATH TO REFLECT LOCATION OF REPLICATION FOLDER
// (Note that Windows machines use "\" characters for filepaths)
global PROJECT_DIR "/zfs/projects/faculty/malhotra-usda/jares_malhotra_dataverse"
global DATA "${PROJECT_DIR}/data"
global LOG_OUTDIR "${PROJECT_DIR}/programs/2_run_regressions/stata_logs"
cd "${PROJECT_DIR}/tables"

log using "${LOG_OUTDIR}/MFP Helpfulness Regressions.txt", text replace

// Read in dataset of farms that had *some* planted acreage
// in both 2013-2017 and 2018 (and reported perception of MFP).
use "${DATA}/input/Li et al (2023) Midwestern Farmer Survey Responses with Policy Outcome Estimates.dta", clear

// Create indicators for missing demographics
gen missing_gender = missing(female)
sum female
replace female = r(mean) if missing_gender

gen missing_education = missing(education_5_pt_scale)
sum education_5_pt_scale
replace education_5_pt_scale = r(mean) if missing_education

gen missing_age = missing(age)
sum age
replace age = r(mean) if missing_age

gen missing_off_farm = missing(had_off_farm_income_2018)
sum missing_off_farm
replace had_off_farm_income_2018 = r(mean) if missing_off_farm

// Verify that we actually only need three indicators to cover missingness status
assert missing_education if missing_age & missing_off_farm & missing_gender
gen missing_demographic = max(missing_gender, missing_education, missing_age, missing_off_farm)

// Create control for long-standing farm size
gen log_total_acres_13_17 = log(total_acres_13_17)

// Arcsinh transformation 
// Scaling is important: Aihounton and Henningsen (2021) propose
// that researchers try multiple scales (e.g. powers of 10)
// and choose the scale that maximizes R-squared
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

* Pick a reasonable scaling for the arcsinh transformation
reg MFP_helpful_4_pt_scale arcsinh_corn_rev_18 arcsinh_soy_rev_18, r
reg MFP_helpful_4_pt_scale arcsinh_corn_rev_18_100 arcsinh_soy_rev_18_100, r
reg MFP_helpful_4_pt_scale arcsinh_corn_rev_18_1K arcsinh_soy_rev_18_1K, r
reg MFP_helpful_4_pt_scale arcsinh_corn_rev_18_10K arcsinh_soy_rev_18_10K, r
reg MFP_helpful_4_pt_scale arcsinh_corn_rev_18_100K arcsinh_soy_rev_18_100K, r
reg MFP_helpful_4_pt_scale arcsinh_corn_rev_18_1M arcsinh_soy_rev_18_1M, r

* Verify that these are also optimal for the binary MFP outcome
reg MFP_helpful_binary arcsinh_corn_rev_18 arcsinh_soy_rev_18, r
reg MFP_helpful_binary arcsinh_corn_rev_18_100 arcsinh_soy_rev_18_100, r
reg MFP_helpful_binary arcsinh_corn_rev_18_1K arcsinh_soy_rev_18_1K, r
reg MFP_helpful_binary arcsinh_corn_rev_18_10K arcsinh_soy_rev_18_10K, r
reg MFP_helpful_binary arcsinh_corn_rev_18_100K arcsinh_soy_rev_18_100K, r
reg MFP_helpful_binary arcsinh_corn_rev_18_1M arcsinh_soy_rev_18_1M, r

* Get lagged inverse hyperbolic sine transformation crop measures
gen arcsinh_corn_rev_13_17_100K = asinh(corn_rev_05_18_prod_13_17 / 100000)
gen arcsinh_soy_rev_13_17_100K = asinh(soy_rev_05_18_prod_13_17 / 100000)

// Binary treatment
gen mfp_made_whole = MFP_net_damage_prod_18 > 0
gen mfp_made_whole_13_17 = MFP_net_damage_prod_13_17 > 0

* Generate state indicators
gen iowa = (state_abbrev == "IA")
gen illinois = (state_abbrev == "IL")
save "${DATA}/output/Li et al (2023) Midwestern Farmer Survey Responses with Policy Outcome Estimates -- with Regression Fields.dta", replace


********************************************************************************

/* TABLE 2 */
* Four-level MFP perception
* Economic policy outcome "treatments"
* Standard controls

local demographic_vars = "female education_5_pt_scale age had_off_farm_income_2018 iowa illinois"
local livestock_vars = "hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018"
local controls = "log_total_acres_13_17 `demographic_vars' `livestock_vars'"
local missing_field_indicators = "missing_gender missing_age missing_off_farm"

* Table 2, column 1
* Range of MFP_net_damage_prod_18_p: 0.005 to 1
reg MFP_helpful_4_pt_scale MFP_net_damage_prod_18_p, r
outreg2 using "Unformatted Table 2.tex", dec(3) replace

* Table 2, column 2
reg MFP_helpful_4_pt_scale MFP_net_damage_prod_18_p `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table 2.tex", dec(3) append

* Table 2, column 3
* Range of MFP_share_damage_prod_18: 0.09 to 1.82
reg MFP_helpful_4_pt_scale MFP_share_damage_prod_18, r
outreg2 using "Unformatted Table 2.tex", dec(3) append

* Table 2, column 4
reg MFP_helpful_4_pt_scale MFP_share_damage_prod_18 `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table 2.tex", dec(3) append

* Table 2, column 5
* mfp_made_whole is binary (0 or 1)
reg MFP_helpful_4_pt_scale mfp_made_whole, r
outreg2 using "Unformatted Table 2.tex", dec(3) append

* Table 2, column 6
reg MFP_helpful_4_pt_scale mfp_made_whole `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table 2.tex", dec(3) append

********************************************************************************

/* TABLE DM15 */
* Four-level MFP perception
* Crop portfolio "treatments"
* Standard controls

local demographic_vars = "female education_5_pt_scale age had_off_farm_income_2018 iowa illinois"
local livestock_vars = "hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018"
local controls = "log_total_acres_13_17 `demographic_vars' `livestock_vars'"
local missing_field_indicators = "missing_gender missing_age missing_off_farm"

* Table DM15, column 1
* Range of soy_acres_share_18: 0 to 1
reg MFP_helpful_4_pt_scale soy_acres_corn_soy_share_18, r
outreg2 using "Unformatted Table DM15.tex", dec(3) replace

* Table DM15, column 2
reg MFP_helpful_4_pt_scale soy_acres_corn_soy_share_18 `controls' `missing_field_indicators', r
outreg2 using "Unformatted Table DM15.tex", dec(3) append

* Report for table footnote: how many respondents have missing demographics
* imputed?
count if e(sample) & missing_demographic

* Table DM15, column 3
* Range of arcsinh_corn_rev_18_100K: 0 to 5.04 (std dev 0.715)
* Range of arcsinh_soy_rev_18_100K: 0 to 4.23 (std dev 0.707)
reg MFP_helpful_4_pt_scale arcsinh_corn_rev_18_100K arcsinh_soy_rev_18_100K, r
outreg2 using "Unformatted Table DM15.tex", dec(3) append

* Table DM15, column 4
reg MFP_helpful_4_pt_scale arcsinh_corn_rev_18_100K arcsinh_soy_rev_18_100K  `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM15.tex", dec(3) append

********************************************************************************

/* TABLE DM16 */
* Binary MFP perception
* Crop portfolio "treatments"
* Standard controls

local demographic_vars = "female education_5_pt_scale age had_off_farm_income_2018 iowa illinois"
local livestock_vars = "hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018"
local controls = "log_total_acres_13_17 `demographic_vars' `livestock_vars'"
local missing_field_indicators = "missing_gender missing_age missing_off_farm"

* Table DM16, column 1
* Range of soy_acres_share_18: 0 to 1
reg MFP_helpful_binary soy_acres_corn_soy_share_18, r
outreg2 using "Unformatted Table DM16.tex", dec(3) replace

* Table DM16, column 2
reg MFP_helpful_binary soy_acres_corn_soy_share_18 `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM16.tex", dec(3) append

* Table DM16, column 3
* Range of arcsinh_corn_rev_18_100K: 0 to 5.04 (std dev 0.715)
* Range of arcsinh_soy_rev_18_100K: 0 to 4.23 (std dev 0.707)
reg MFP_helpful_binary arcsinh_corn_rev_18_100K arcsinh_soy_rev_18_100K, r
outreg2 using "Unformatted Table DM16.tex", dec(3) append

* Table DM16, column 4
reg MFP_helpful_binary arcsinh_corn_rev_18_100K arcsinh_soy_rev_18_100K  `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM16.tex", dec(3) append


********************************************************************************

/* TABLE DM17 */
* Binary MFP perception
* Economic policy outcome "treatments"
* Standard controls

local demographic_vars = "female education_5_pt_scale age had_off_farm_income_2018 iowa illinois"
local livestock_vars = "hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018"
local controls = "log_total_acres_13_17 `demographic_vars' `livestock_vars'"
local missing_field_indicators = "missing_gender missing_age missing_off_farm"

* Table DM17, column 1
* Range of MFP_net_damage_prod_18_p: 0.005 to 1
reg MFP_helpful_binary MFP_net_damage_prod_18_p, r
outreg2 using "Unformatted Table DM17.tex", dec(3) replace

* Table DM17, column 2
reg MFP_helpful_binary MFP_net_damage_prod_18_p `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM17.tex", dec(3) append

* Table DM17, column 3
* Range of MFP_share_damage_prod_18: 0.09 to 1.82
reg MFP_helpful_binary MFP_share_damage_prod_18, r
outreg2 using "Unformatted Table DM17.tex", dec(3) append

* Table DM17, column 4
reg MFP_helpful_binary MFP_share_damage_prod_18 `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM17.tex", dec(3) append

* Table DM17, column 5
* mfp_made_whole is binary (0 or 1)
reg MFP_helpful_binary mfp_made_whole, r
outreg2 using "Unformatted Table DM17.tex", dec(3) append

* Table DM17, column 6
reg MFP_helpful_binary mfp_made_whole `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM17.tex", dec(3) append

********************************************************************************

/* TABLE DM18 */
* Four-level MFP perception
* Crop mix and policy outcome "treatments"
* Standard controls plus lagged "treatments"

local demographic_vars = "female education_5_pt_scale age had_off_farm_income_2018 iowa illinois"
local livestock_vars = "hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018"
local controls = "log_total_acres_13_17 `demographic_vars' `livestock_vars'"

* Table DM18, column 1
* Range of soy_acres_share_18: 0 to 1
reg MFP_helpful_4_pt_scale soy_acres_corn_soy_share_18 soy_acres_corn_soy_share_13_17 `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM18.tex", dec(3) replace

* Table DM18, column 2
* Range of arcsinh_corn_rev_18_100K: 0 to 5.04 (std dev 0.715)
* Range of arcsinh_soy_rev_18_100K: 0 to 4.23 (std dev 0.707)
reg MFP_helpful_4_pt_scale arcsinh_corn_rev_18_100K arcsinh_soy_rev_18_100K arcsinh_corn_rev_13_17_100K arcsinh_soy_rev_13_17_100K `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM18.tex", dec(3) append

* Table DM18, column 3
* Range of MFP_net_damage_prod_18_p: 0.005 to 1
reg MFP_helpful_4_pt_scale MFP_net_damage_prod_18_p MFP_net_damage_prod_13_17_p `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM18.tex", dec(3) append

* Table DM18, column 4
* Range of MFP_share_damage_prod_18: 0.09 to 1.82
reg MFP_helpful_4_pt_scale MFP_share_damage_prod_18 MFP_share_damage_prod_13_17 `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM18.tex", dec(3) append

* Table DM18, column 5
* mfp_made_whole is binary (0 or 1)
reg MFP_helpful_4_pt_scale mfp_made_whole mfp_made_whole_13_17 `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM18.tex", dec(3) append

********************************************************************************


/* TABLE DM19 */
* Five-level tariff support
* Crop portfolio "treatments"
* Standard controls

local demographic_vars = "female education_5_pt_scale age had_off_farm_income_2018 iowa illinois"
local livestock_vars = "hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018"
local controls = "log_total_acres_13_17 `demographic_vars' `livestock_vars'"
local missing_field_indicators = "missing_gender missing_age missing_off_farm"

* Table DM19, column 1
* Range of soy_acres_share_18: 0 to 1
reg support_raising_tariffs soy_acres_corn_soy_share_18, r
outreg2 using "Unformatted Table DM19.tex", dec(3) replace

* Table DM19, column 2
reg support_raising_tariffs soy_acres_corn_soy_share_18 `controls' `missing_field_indicators', r
outreg2 using "Unformatted Table DM19.tex", dec(3) append

* Report for table footnote: how many respondents have missing demographics
* imputed?
count if e(sample) & missing_demographic

* Table DM19, column 3
* Range of arcsinh_corn_rev_18_100K: 0 to 5.04 (std dev 0.715)
* Range of arcsinh_soy_rev_18_100K: 0 to 4.23 (std dev 0.707)
reg support_raising_tariffs arcsinh_corn_rev_18_100K arcsinh_soy_rev_18_100K, r
outreg2 using "Unformatted Table DM19.tex", dec(3) append

* Table DM19, column 4
reg support_raising_tariffs arcsinh_corn_rev_18_100K arcsinh_soy_rev_18_100K  `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM19.tex", dec(3) append

********************************************************************************

/* TABLE DM20 */
* Five-level tariff support
* Economic policy outcome "treatments"
* Standard controls

local demographic_vars = "female education_5_pt_scale age had_off_farm_income_2018 iowa illinois"
local livestock_vars = "hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018"
local controls = "log_total_acres_13_17 `demographic_vars' `livestock_vars'"
local missing_field_indicators = "missing_gender missing_age missing_off_farm"

* Table DM20, column 1
* Range of MFP_net_damage_prod_18_p: 0.005 to 1
reg support_raising_tariffs MFP_net_damage_prod_18_p, r
outreg2 using "Unformatted Table DM20.tex", dec(3) replace

* Table DM20, column 2
reg support_raising_tariffs MFP_net_damage_prod_18_p `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM20.tex", dec(3) append

* Table DM20, column 3
* Range of MFP_share_damage_prod_18: 0.09 to 1.82
reg support_raising_tariffs MFP_share_damage_prod_18, r
outreg2 using "Unformatted Table DM20.tex", dec(3) append

* Table DM20, column 4
reg support_raising_tariffs MFP_share_damage_prod_18 `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM20.tex", dec(3) append

* Table DM20, column 5
* mfp_made_whole is binary (0 or 1)
reg support_raising_tariffs mfp_made_whole, r
outreg2 using "Unformatted Table DM20.tex", dec(3) append

* Table DM20, column 6
reg support_raising_tariffs mfp_made_whole `controls' `missing_field_indicators', r
count if e(sample) & missing_demographic
outreg2 using "Unformatted Table DM20.tex", dec(3) append

********************************************************************************

/* TABLE DM21 */
* Four-level MFP perception
* Economic policy outcome "treatments"
* Standard controls
* Interaction term for positive / negative status ("loss aversion" / "prospect theory") -- PIECEWISE DISCONTINUOUS

gen MFP_net_damage_X_made_whole = MFP_net_damage_prod_18_p * mfp_made_whole
gen MFP_share_damage_X_made_whole = MFP_share_damage_prod_18 * mfp_made_whole

local demographic_vars = "female education_5_pt_scale age had_off_farm_income_2018 iowa illinois"
local livestock_vars = "hogs_2018 dairy_2018 beef_2018 poultry_2018 other_livestock_2018"
local controls = "log_total_acres_13_17 `demographic_vars' `livestock_vars'"
local missing_field_indicators = "missing_gender missing_age missing_off_farm"

* Table DM21, column 1
* Range of MFP_net_damage_prod_18_p: 0.005 to 1
reg MFP_helpful_4_pt_scale MFP_net_damage_prod_18_p MFP_net_damage_X_made_whole mfp_made_whole, r
outreg2 using "Unformatted Table DM21.tex", dec(3) replace
lincom MFP_net_damage_prod_18_p + MFP_net_damage_X_made_whole

* Table DM21, column 2
reg MFP_helpful_4_pt_scale MFP_net_damage_prod_18_p MFP_net_damage_X_made_whole mfp_made_whole `controls' `missing_field_indicators', r
outreg2 using "Unformatted Table DM21.tex", dec(3) append

* Table DM21, column 3
* Range of MFP_share_damage_prod_18: 0.09 to 1.82
reg MFP_helpful_4_pt_scale MFP_share_damage_prod_18 MFP_share_damage_X_made_whole mfp_made_whole, r
outreg2 using "Unformatted Table DM21.tex", dec(3) append

* Table DM21, column 4
reg MFP_helpful_4_pt_scale MFP_share_damage_prod_18 MFP_share_damage_X_made_whole mfp_made_whole `controls' `missing_field_indicators', r
outreg2 using "Unformatted Table DM21.tex", dec(3) append

log close
********************************************************************************
