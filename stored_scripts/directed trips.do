

****************************
* Directed trips for cod and haddock WGOM (model unit)
cd $input_data_cd

clear
tempfile tl1 cl1
dsconcat $triplist

/*dtrip will be used to estimate total directed trips*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "atlanticcod") 
replace dom_id="1" if strmatch(prim1_common, "atlanticcod") 

replace dom_id="1" if strmatch(common, "haddock") 
replace dom_id="1" if strmatch(prim1_common, "haddock") 

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

// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "atlanticcod", "haddock") 

mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")

keep if dom_id=="1"

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

replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

svy: total dtrip, over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

keep my b se  ll ul
gen pse=(se/b)*100

split my, parse(_)
rename my_dom_id_string1 year
rename my_dom_id_string2 wave
rename my_dom_id_string3 area
rename my_dom_id_string4 mode
drop my_dom_id_string
rename b dtrip
drop se ll ul pse

reshape wide dtrip , i(year wave mode) j(area) string
mvencode dtrip*, mv(0) override

gen double dtripWGOM = 0
foreach a in 513 514 515 521 526 NH{
    capture confirm variable dtrip`a'
    if !_rc {
        replace dtripWGOM = dtripWGOM + dtrip`a'
    }
}

gen double dtripEGM = 0
foreach a in 511 512{
    capture confirm variable dtrip`a'
    if !_rc {
        replace dtripEGM = dtripEGM + dtrip`a'
    }
}

/*
gen double dtrip521526 = 0
foreach a in 521 526{
    capture confirm variable dtrip`a'
    if !_rc {
        replace dtrip521526 = dtrip521526 + dtrip`a'
    }
}
*/


keep dtripWGOM dtripEGM  year wave mode
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
clear 
set obs 4
gen mode="pr" if _n==1
replace mode="ch" if _n==2
replace mode="sh" if _n==3
replace mode="hd" if _n==4
tempfile modes
save `modes', replace 
restore 

tempfile basefile
save `basefile', replace

mata: mata clear

clear
tempfile master
save `master', emptyok

local vars FY2024 FY2025_impute FY2024_current FY2025_current w52022 w52023 w52024 w52025
*local vars $period_list
foreach v of local vars{
	di "`v'"
	u `basefile', clear
	keep if `v'==1

	tempfile base3
	save `base3', replace
	
	local areas dtripWGOM dtripEGM 
		foreach a of local areas{
			
			u `base3', clear 
			
			collapse (sum) `a' , by(`v' mode)
			rename `a' dtrip 
			merge 1:1 mode using `modes', nogen 
			su dtrip
			local sum=`r(sum)'

			set obs 5
			replace mode="total" if _n==5
			replace dtrip =`sum' if mode=="total"
			replace `v'=1 if mode=="total"
			tostring `v', replace
			replace `v'= "_`v'"
			reshape wide dtrip  , i(mode) j(`v') string
			gen area="`a'"

			append using `master'
			save `master', replace
			clear        

		}

}
/*
collapse (sum) dtripWGOM, by(`v' mode)
rename dtrip dtrip 
merge 1:1 mode using `modes', nogen 
su dtrip
local sum=`r(sum)'
set obs 4
replace mode="total" if _n==4
replace dtrip=`sum' if mode=="total"
replace `v'=1 if mode=="total"
tostring `v', replace
replace `v'= "_`v'"
gen area="WGOM"
reshape wide dtrip, i(mode area) j(`v') string
append using `master'
save `master', replace
clear     
*/


use `master', clear
mvencode dtrip*, mv(0) override
collapse (sum) dtrip*, by(mode area)

gen pct_diffFY=((dtrip_FY2025_impute- dtrip_FY2024)/dtrip_FY2024)*100
gen pct_diffcurrent=((dtrip_FY2025_current -dtrip_FY2024_current)/dtrip_FY2024_current)*100

ds mode area, not
return list
local vars `r(varlist)'
foreach v of local vars{
	replace `v'=round(`v')
}

order mode dtrip_FY2024 dtrip_FY2025_impute pct_diffFY  dtrip_FY2024_current dtrip_FY2025_current pct_diffcurrent
replace area="EGOM" if area=="dtripEGM"
replace area="WGOM" if area=="dtripWGOM"

gen disposition="cod/haddock angler trips"
tempfile gom
save `gom', replace
  
  ****************************
* Directed COD trips cod in old SNE and new SNE
cd $input_data_cd

clear
tempfile tl1 cl1
dsconcat $triplist

/*dtrip will be used to estimate total directed trips*/
gen dtrip=1

sort year strat_id psu_id id_code
save `tl1'

clear

dsconcat $catchlist
sort year strat_id psu_id id_code
replace common=subinstr(lower(common)," ","",.)
save `cl1'

use `tl1'
merge 1:m year strat_id psu_id id_code using `cl1', keep(1 3)
replace common=subinstr(lower(common)," ","",.)
replace prim1_common=subinstr(lower(prim1_common)," ","",.)
replace prim2_common=subinstr(lower(prim2_common)," ","",.)

drop _merge
 
 /* classify trips into dom_id=1 (DOMAIN OF INTEREST) and dom_id=2 ('OTHER' DOMAIN). */
gen str1 dom_id="2"
replace dom_id="1" if strmatch(common, "atlanticcod") 
replace dom_id="1" if strmatch(prim1_common, "atlanticcod") 

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

// Deal with Group Catch: 
	// This bit of code generates a flag for each year-strat_id psu_id leader. (equal to the lowest of the dom_id)
	// Then it generates a flag for claim equal to the largest claim.  
	// Then it re-classifies the trip into dom_id=1 if that trip had catch of species in dom_id1 

replace claim=0 if claim==.

gen domain_claim=claim if inlist(common, "atlanticcod") 

mvencode domain_claim, mv(0) override

bysort strat_id psu_id leader (dom_id): gen gc_flag=dom_id[1]
bysort strat_id psu_id leader (domain_claim): gen claim_flag=domain_claim[_N]
replace dom_id="1" if strmatch(dom_id,"2") & claim_flag>0 & claim_flag!=. & strmatch(gc_flag,"1")

keep if dom_id=="1"

* generate estimation strata

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

replace my_dom_id_string=ltrim(rtrim(my_dom_id_string))

/* total with over(<overvar>) requires a numeric variable */
encode my_dom_id_string, gen(my_dom_id)

/* keep 1 observation per year-strat-psu-id_code. This will have dom_id=1 if it targeted or caught my_common1 or my_common2. Else it will be dom_id=2*/
bysort year wave strat_id psu_id id_code (dom_id): gen count_obs1=_n
keep if count_obs1==1


replace wp_int=0 if wp_int<=0
svyset psu_id [pweight= wp_int], strata(strat_id) singleunit(certainty)

preserve
keep my_dom_id my_dom_id_string
duplicates drop 
tostring my_dom_id, gen(my_dom_id2)
keep my_dom_id2 my_dom_id_string
tempfile domains
save `domains', replace 
restore

encode mode1, gen(mode2)

svy: total dtrip, over(my_dom_id)  

xsvmat, from(r(table)') rownames(rname) names(col) norestor
split rname, parse("@")
drop rname1
split rname2, parse(.)
drop rname2 rname22
rename rname21 my_dom_id2
merge 1:1 my_dom_id2 using `domains'
drop rname my_dom_id2 _merge 
order my_dom_id_string

keep my b se  ll ul
gen pse=(se/b)*100

split my, parse(_)
rename my_dom_id_string1 year
rename my_dom_id_string2 wave
rename my_dom_id_string3 area
rename my_dom_id_string4 mode
drop my_dom_id_string
rename b dtrip
drop se ll ul pse 

reshape wide dtrip, i(year wave mode) j(area) string
mvencode dtrip*, mv(0) override
replace dtripSNE = dtripSNE + dtrip538

gen double dtripSNEold = dtripSNE
foreach a in 521 526 {
    capture confirm variable dtrip`a'
    if !_rc {
        replace dtripSNEold = dtripSNEold + dtrip`a'
    }
}

keep dtripSNE dtripSNEold year wave mode


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


local vars FY2024 FY2025_impute FY2024_current FY2025_current w52022 w52023 w52024 w52025
foreach v of local vars{

u `base', clear 

keep if `v'==1

tempfile base2
save `base2', replace 

local areas dtripSNE dtripSNEold
foreach a of local areas{
	
u `base2', clear 

collapse (sum) `a' , by(`v' mode)
rename `a' dtrip 
merge 1:1 mode using `modes', nogen 
su dtrip
local sumdtrip=`r(sum)'

set obs 5
replace mode="total" if _n==5
replace dtrip =`sumdtrip' if mode=="total"
replace `v'=1 if mode=="total"
tostring `v', replace
replace `v'= "_`v'"
reshape wide dtrip  , i(mode) j(`v') string
gen area="`a'"

append using `master'
save `master', replace
clear        

}
}

use `master', clear
mvencode dtrip*, mv(0) override
replace area="SNE" if area=="dtripSNE"
replace area="old SNE" if area=="dtripSNEold"
collapse (sum) dtrip*, by(mode area )

gen pct_diffFY=((dtrip_FY2025_impute- dtrip_FY2024)/dtrip_FY2024)*100
gen pct_diffcurrent=((dtrip_FY2025_current -dtrip_FY2024_current)/dtrip_FY2024_current)*100

ds mode area, not
return list
local vars `r(varlist)'
foreach v of local vars{
	replace `v'=round(`v')
}
order area mode dtrip_FY2024 dtrip_FY2025_impute pct_diffFY dtrip_FY2024_current dtrip_FY2025_current pct_diffcurrent

gen disposition="cod angler trips"
append using `gom'
order area disp mode
renvarlab dtrip*, predrop(6)
save "$project_path\directed_trips.dta", replace 

  

