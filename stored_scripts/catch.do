
****************************
* Catch of cod and haddock WGOM (model unit)
cd $input_data_cd

clear
mata: mata clear

tempfile tl1 cl1
dsconcat $triplist

sort year strat_id psu_id id_code
drop if strmatch(id_code, "*xx*")==1
duplicates drop 
save `tl1'
clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

replace var_id=strat_id if strmatch(var_id,"")

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3) nogenerate /*Keep all trips including catch==0*/
replace var_id=strat_id if strmatch(var_id,"")

* classify trips that I care about into the things I care about (caught or targeted sf/bsb) and things I don't care about "ZZ" 
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim1_common)," ","",.)

* We need to retain 1 observation for each strat_id, psu_id, and id_code
/* A.  Trip (Targeted or Caught) (fluke, sea bass, or scup) then it should be marked in the domain "_ATLCO"
   B.  Trip did not (Target or Caught) (fluke, sea bass, or scup) then it is marked in the the domain "ZZZZZ"
*/

gen common_dom="ZZ"
replace common_dom="ATLCO" if inlist(common, "atlanticcod") 
replace common_dom="ATLCO" if inlist(common, "haddock") 

replace common_dom="ATLCO"  if inlist(prim1_common, "atlanticcod") 
replace common_dom="ATLCO"  if inlist(prim1_common, "haddock") 

tostring wave, gen(w2)
tostring year, gen(year2)
gen st2 = string(st,"%02.0f")

gen state="MA" if st==25
replace state="MD" if st==24
replace state="RI" if st==44
replace state="CT" if st==9
replace state="NY" if st==36
replace state="NJ" if st==34
replace state="DE" if st==10
replace state="VA" if st==51
replace state="NC" if st==37
replace state="ME" if st==23
replace state="NH" if st==33

gen mode1="sh" if inlist(mode_fx, "1", "2", "3")
replace mode1="pr" if inlist(mode_fx, "7")
replace mode1="ch" if inlist(mode_fx, "5")
replace mode1="hd" if inlist(mode_fx, "4")

gen date=substr(id_code, 6,8)
gen month1=substr(date, 5, 2)
gen day1=substr(date, 7, 2)
drop if inlist(day1,"9x", "xx") 
destring day1, replace

keep if common_dom=="ATLCO"

*New MRIP site allocations
preserve 
import delimited using "$input_data_cd/MRIP_COD_ALL_SITE_LIST.csv", clear 
keep if inlist(state, "MA", "ME")
keep state intsite nmfs_stock_area nmfs_stat_area
sort intsite nmfs_stock_area  
keep nmfs_stock_area* intsite nmfs_stat_area state
duplicates drop
tempfile mrip_sites
save `mrip_sites', replace 
restore

merge m:1 intsite state using `mrip_sites',  keep(1 3)

tostring nmfs_stat_area, replace
replace nmfs_stat_area="SNE" if inlist(state, "CT", "RI", "NY", "NJ", "MD", "DE") 
replace nmfs_stat_area="NH" if inlist(state, "NH") 

/* generate the estimation strata - year, month, kind-of-day (weekend including fed holidays/weekday), mode (pr/fh)*/
gen my_dom_id_string=year2+"_"+ w2 +"_"+ nmfs_stat_area+"_"+mode1

* Define the list of species to process
local species "atlanticcod haddock"

* Loop over each species
foreach s of local species {

    * Create short species prefix (e.g., cod, hadd)
    local short = substr("`s'", 1, 4)
    if "`s'" == "atlanticcod" local short "cod"
    if "`s'" == "haddock"     local short "hadd"

    * Generate species-specific totals
    gen `short'_tot_cat = tot_cat if common == "`s'"
    egen sum_`short'_tot_cat = sum(`short'_tot_cat), by(strat_id psu_id id_code)

    gen `short'_harvest = landing if common == "`s'"
    egen sum_`short'_harvest = sum(`short'_harvest), by(strat_id psu_id id_code)

    gen `short'_releases = release if common == "`s'"
    egen sum_`short'_releases = sum(`short'_releases), by(strat_id psu_id id_code)
}

rename sum_cod_tot_cat cod_cat
rename sum_cod_harvest cod_keep
rename sum_cod_releases cod_rel
rename sum_hadd_tot_cat hadd_cat
rename sum_hadd_harvest hadd_keep
rename sum_hadd_releases hadd_rel

* Set a variable "no_dup"=0 if the record is "$my_common" catch and no_dup=1 otherwise
gen no_dup=0
replace no_dup=1 if  strmatch(common, "atlanticcod")==0
replace no_dup=1 if strmatch(common, "haddock")==0

/*
We sort on year, strat_id, psu_id, id_code, "no_dup", and "my_dom_id_string". For records with duplicate year, strat_id, psu_id, and id_codes, the first entry will be "my_common catch" if it exists.  These will all be have sp_dom "SF."  If there is no my_common catch, but the trip targeted (fluke, sea bass, or scup) or caught either species, the secondary sorting on "my_dom_id_string" ensures the trip is properly classified.

After sorting, we generate a count variable (count_obs1 from 1....n) and we keep only the "first" observations within each "year, strat_id, psu_id, and id_codes" group.
*/

bysort year strat_id psu_id id_code (my_dom_id_string no_dup): gen count_obs1=_n

keep if count_obs1==1 // This keeps only one record for trips with catch of multiple species. We have already computed catch of the species of interest above and saved these in a trip-row

order strat_id psu_id id_code no_dup my_dom_id_string count_obs1 common

svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

drop if wp_int==0
encode my_dom_id_string, gen(my_dom_id)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tempfile domains
save `domains', replace 
restore

tempfile basefile
save `basefile', replace 


* Create a postfile to collect results
tempfile results
postfile handle str15 varname str15 domain float total se ll95 ul95 using `results', replace

* Loop over variables
foreach var in cod_keep cod_rel cod_cat hadd_keep hadd_rel hadd_cat  {

    * Run svy mean for the variable by domain
    svy: total `var', over(my_dom_id)

    * Grab result matrix and domain labels
    matrix M = r(table)
    local colnames : colnames M

    * Loop over columns (domains)
    foreach col of local colnames {
        local m  = M[1, "`col'"]
        local se = M[2, "`col'"]
        local lb = M[5, "`col'"]
        local ub = M[6, "`col'"]

        post handle ("`var'") ("`col'") (`m') (`se') (`lb') (`ub')
    }
}

postclose handle

* Load results back into memory
use `results', clear

split domain, parse("@")
drop domain1
split domain2, parse(.)
split domain21, parse(b)

drop domain2 domain21 domain22 domain212
destring domain211, replace
rename domain211 my_dom_id
merge m:1 my_dom_id using `domains' 
sort varname  my_dom_id
keep varname total  my_dom_id_string

split my_dom_id_string, parse(_)

rename my_dom_id_string1 year
rename my_dom_id_string2 wave
rename my_dom_id_string3 area
rename my_dom_id_string4 mode
drop my_dom_id_string


reshape wide total, i(varname year wave mode) j(area) string
mvencode total*, mv(0) override

gen double totalWGOM = 0
foreach a in 513 514 515 521 526 NH{
    capture confirm variable total`a'
    if !_rc {
        replace totalWGOM = totalWGOM + total`a'
    }
}

replace totalSNE = totalSNE + total538

gen double totalSNEold = totalSNE
foreach a in 521 526 {
    capture confirm variable total`a'
    if !_rc {
        replace totalSNEold = totalSNEold + total`a'
    }
}

gen double totalEGM = 0
foreach a in 511 512{
    capture confirm variable total`a'
    if !_rc {
        replace totalEGM = totalEGM + total`a'
    }
}


keep totalWGOM totalSNE totalSNEold totalEGM varname year wave mode

/*
gen FY2024=1 if (year=="2024" & inlist(wave, "3", "4", "5", "6")) | (year=="2025" & inlist(wave,  "2"))
gen FY2025_impute=1 if (year=="2025" & inlist(wave, "3", "4")) | (year=="2024" & inlist(wave, "5", "6", "2"))
gen FY2024_current=1 if (year=="2024" & inlist(wave, "3", "4"))
gen FY2025_current=1 if (year=="2025" & inlist(wave, "3", "4"))
*/

gen $FY2024
gen $FY2025_impute
gen $FY2024_current
gen $FY2025_current

gen $w52022
gen $w52023
gen $w52024
gen $w52025

mvencode FY* w*, mv(0) override


preserve
keep if FY2025_impute==1 | FY2024==1
keep varname year wave mode totalWGOM FY2024 FY2025_impute
collapse (sum) totalWGOM, by(varname year wave mode FY2025_impute FY2024)
egen sum=sum(total), by(varname mode)
gen pct_of_total=total/sum
sort varname wave mode
drop sum
label variable pct_of_total "percent of mode-m's annual total"
save "$data_out_dir\wave_totals.dta", replace
restore 


preserve
clear 
set obs 3
gen mode="pr" if _n==1
replace mode="ch" if _n==2
replace mode="sh" if _n==3
replace mode="hd" if _n==4
tempfile modes
save `modes', replace 
restore 

tempfile base
save `base', replace 


mata: mata clear
clear
tempfile master
save `master', emptyok


local disps cod_cat cod_keep cod_rel hadd_cat hadd_keep hadd_rel
foreach d of local disps{

	u `base', clear 	
	keep if varname=="`d'"

	tempfile base2
	save `base2', replace

	local vars FY2024 FY2025_impute FY2024_current FY2025_current w52022 w52023 w52024 w52025
		foreach v of local vars{

		u `base2', clear 

		keep if `v'==1

		tempfile base3
		save `base3', replace

		local areas totalWGOM totalSNE totalSNEold totalEGM
			foreach a of local areas{
			u `base3', clear 
			collapse (sum) `a' , by(`v' mode)
			rename `a' total 
			merge 1:1 mode using `modes', nogen 
			su total
			local sumtotal=`r(sum)'

			set obs 5
			replace mode="total" if _n==5
			replace total =`sumtotal' if mode=="total"
			replace `v'=1 if mode=="total"
			tostring `v', replace
			replace `v'= "_`v'"
			reshape wide total  , i(mode) j(`v') string
			gen area="`a'"
			gen disp="`d'"

			append using `master'
			save `master', replace
			clear        

		}
	}
}

use `master', clear

mvencode total*, mv(0) override
replace area="EGOM" if area=="totalEGM"
replace area="SNE" if area=="totalSNE"
replace area="SNEold" if area=="totalSNEold"
replace area="WGOM" if area=="totalWGOM"

collapse (sum) total*, by(mode area disp)

gen pct_diffFY=((total_FY2025_impute- total_FY2024)/total_FY2024)*100
gen pct_diffcurrent=((total_FY2025_current -total_FY2024_current)/total_FY2024_current)*100

ds mode area disp, not
return list
local vars `r(varlist)'
foreach v of local vars{
	replace `v'=round(`v')
}

order area disp mode total_FY2024 total_FY2025_impute pct_diffFY total_FY2024_current total_FY2025_current pct_diffcurrent
sort area disp mode
rename disp disposition
renvarlab total*, predrop(6)
save "$data_out_dir\catch.dta", replace 
