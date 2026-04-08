
/* Dashboard path must be defined in your stata profile.do that is executed at startup. It should point to the the root of the dashboard repository */

/* packages

ssc install xsvmat
ssc install dsconcat 
*/


global project_path "$dashboard_path/stored_scripts" 
global input_data_dir "$dashboard_path/data/raw" 
global data_out_dir "$dashboard_path/data/main" 

* set a global seed #
global seed 03211990

* years/waves of MRIP data. 
global yr_wvs 20221 20222 20223 20224 20225 20226 ///
					 20231 20232 20233 20234 20235 20236  ///
					 20241 20242 20243 20244 20245 20246  ///
					 20251 20252 20253 20254 20255 20256
					 
global yearlist 2022 2023 2024 2025
global wavelist 1 2 3 4 5 6


do "$project_path\MRIP data wrapper.do"


*Set the estimation time period:
*Original periods used for Jan. 20th, 2025 RAP meeting 
/*
global FY2024 `"FY2024=1 if (year=="2024" & inlist(wave, "3", "4", "5", "6")) | (year=="2025" & inlist(wave,  "2"))"'
global FY2025_impute `"FY2025_impute=1 if (year=="2025" & inlist(wave, "3", "4")) | (year=="2024" & inlist(wave, "5", "6", "2"))"'
global FY2024_current `"FY2024_current=1 if (year=="2024" & inlist(wave, "3", "4"))"'
global FY2025_current `"FY2025_current=1 if (year=="2025" & inlist(wave, "3", "4"))"'
*/


*Updated periods for Jan. 28th, 2025 Council meeting 
global FY2024 `"FY2024=1 if (year=="2024" & inlist(wave, "3", "4", "5", "6")) | (year=="2025" & inlist(wave,  "2"))"'
global FY2025_impute `"FY2025_impute=1 if (year=="2025" & inlist(wave, "3", "4", "5")) | (year=="2024" & inlist(wave, "6", "2"))"'
global FY2024_current `"FY2024_current=1 if (year=="2024" & inlist(wave, "3", "4", "5"))"'
global FY2025_current `"FY2025_current=1 if (year=="2025" & inlist(wave, "3", "4", "5"))"'
global w52022 `"w52022=1 if (year=="2022" & inlist(wave, "5"))"'
global w52023 `"w52023=1 if (year=="2023" & inlist(wave, "5"))"'
global w52024 `"w52024=1 if (year=="2024" & inlist(wave, "5"))"'
global w52025 `"w52025=1 if (year=="2025" & inlist(wave, "5"))"'

global period_list  "FY2024 FY2025_impute FY2024_current FY2025_current w52022 w52023 w52024 w52025"
do "$project_path\directed trips.do"
do "$project_path\catch.do"


u  "$project_path\directed_trips.dta", clear
append using "$project_path\catch.dta"
replace area="old SNE" if area=="SNEold"
rename disp metric
gen order1 = 1 if area=="WGOM"
replace order1 = 2 if area=="SNE"
replace order1 = 3 if area=="old SNE"
replace order1 = 4 if area=="EGOM"

gen order2 = 1 if metric=="cod/haddock angler trips" | metric=="cod angler trips"
replace order2 = 2 if metric=="cod_keep"
replace order2 = 3 if metric=="cod_rel"
replace order2 = 4 if metric=="cod_cat"
replace order2 = 5 if metric=="hadd_keep"
replace order2 = 6 if metric=="hadd_rel"
replace order2 = 7 if metric=="hadd_cat"

/*
*catch in 521/526
preserve
keep if strmatch(metric, "*trips*")!=1
keep if inlist(area, "SNE")
drop pct* order*
ds area metric mode, not
renvarlab `r(varlist)', postfix(_SNE)
drop area
tempfile sne
save `sne', replace
restore 

keep if strmatch(metric, "*trips*")!=1
keep if inlist(area, "old SNE")
drop pct* order*
ds area metric mode, not
renvarlab `r(varlist)', postfix(_oldSNE)
drop area
merge 1:1  metric mode using `sne'

local values FY2024 FY2025_impute FY2024_current FY2025_current
foreach v of local values{
	gen `v'_521=`v'_oldSNE-`v'_SNE
}

keep metric mode *521
*/

*replace order2 = 8 if metric=="cod angler trips"

sort order1 order2 mode
drop if FY2024==0 & FY2025_impute==0 & FY2024_current==0 & FY2025_current==0 

ds order* area metric mode, not
format `r(varlist)' %12.0gc
levelsof area, local(areas)

* compute catch-per-trip
tempfile new
save `new', replace 

clear
tempfile master
save `master', emptyok

foreach a of local areas{

	u `new', clear 
	keep if area=="`a'"
	
	keep if strmatch(metric, "*trips*")==1
	drop pct* order*
	drop metric
	ds area mode, not
	renvarlab `r(varlist)', postfix(_trips)
	tempfile trips`a'
	save `trips`a'', replace
	
	u `new', clear 
	keep if area=="`a'"
	drop pct* order*

	merge m:1 area mode using `trips`a''
	
	append using `master'
	save `master', replace
	clear        

		
	
}

use `master', clear

preserve
keep if strmatch(metric, "*cat*")==1
local vars FY2024 FY2025_impute FY2024_current FY2025_current w52022 w52023 w52024 w52025
foreach v of local vars{
	gen `v'_catch_trip = `v'/`v'_trips
}

replace metric = metric+"_per_trip"
drop FY2024 FY2025_impute FY2024_current FY2025_current w52022 w52023 w52024 w52025
drop FY2024_trips FY2025_impute_trips FY2024_current_trips FY2025_current_trips w52022_trips w52023_trips w52024_trips w52025_trips
drop _merge
ds area metric mode, not
renvarlab `r(varlist)', postdrop(11)
tempfile catch_trip
save `catch_trip'
restore 

u `new', clear 
append using `catch_trip'

drop order*
gen order1 = 1 if area=="WGOM"
replace order1 = 2 if area=="SNE"
replace order1 = 3 if area=="old SNE"
replace order1 = 4 if area=="EGOM"

gen order2 = 1 if metric=="cod/haddock angler trips" | metric=="cod angler trips"
replace order2 = 2 if metric=="cod_keep"
replace order2 = 3 if metric=="cod_rel"
replace order2 = 4 if metric=="cod_cat"
replace order2 = 5 if metric=="cod_cat_per_trip"
replace order2 = 6 if metric=="hadd_keep"
replace order2 = 7 if metric=="hadd_rel"
replace order2 = 8 if metric=="hadd_cat"
replace order2 = 9 if metric=="hadd_cat_per_trip"
sort order1 order2 mode

ds order* area metric mode, not
format `r(varlist)' %12.0gc

replace pct_diffFY=((FY2025_impute- FY2024)/FY2024)*100
replace pct_diffcurrent=((FY2025_current -FY2024_current)/FY2024_current)*100

ds order* area metric mode, not
format `r(varlist)' %12.3gc

browse if mode!="total"

browse if mode=="total"
order area metric FY2024 FY2025_impute pct_diffFY FY2024_current FY2025_current  pct_diffcurrent

replace metric = "cod catch (numbers)" if metric =="cod_cat"
replace metric = "cod harvest (numbers)" if metric =="cod_keep"
replace metric = "cod discards (numbers)" if metric =="cod_rel"
replace metric = "avg. cod catch-per-trip (numbers)" if metric =="cod_cat_per_trip"


replace metric = "haddock catch (numbers)" if metric =="hadd_cat"
replace metric = "haddock harvest (numbers)" if metric =="hadd_keep"
replace metric = "haddock discards (numbers)" if metric =="hadd_rel"
replace metric = "avg. haddock catch-per-trip (numbers)" if metric =="hadd_cat_per_trip"


browse if mode=="total"
order area metric mode
sort order1 mode order2 


