*********************************************************************************************
* Description: this program creates a dataset with mayors' profissional background and classify them as STEM or Non-STEM
**********************************************************************************************

**********************************************************************************************
* Initial Commands
**********************************************************************************************

cap log close                       /* closes open log files */
set more off                        /* tells Stata not to pause after each step of calculation */
clear                               /* clears current memory */
set memory 500m                     /* increases available memory */

global crosswalk_dir 					= "D:\clean_rais_software\data\input/sector_crosswalk"
global raw_dir 		 					= "D:\clean_rais_software\data\input\raw"
global RAIS_dir 						= "D:\clean_rais_software\data\input\RAIS_stata_files"
global work_dir 						= "C:\Users\gabri\OneDrive\Gabriel\Insper\Tese\Engenheiros\replication_code\rdd_when_science_strikes_back\2_get_rais_data"
global output_dir 						= "C:\Users\gabri\OneDrive\Gabriel\Insper\Tese\Engenheiros\replication_code\rdd_when_science_strikes_back\2_get_rais_data\output"
global create_electoral_data_dir 		= "C:\Users\gabri\OneDrive\Gabriel\Insper\Tese\Engenheiros\replication_code\rdd_when_science_strikes_back\1_create_electoral_data\output\data"

set seed 1234
cap mkdir "$output_dir"

**********************************************************************************************
* Main Code - collecting the data
**********************************************************************************************

foreach state in AC AL AP AM BA CE DF ES GO MA MT MS MG PA PB PR PE PI RJ RN RS RO RR SC SP SE TO {
forval year = 1995/2016 {
	
if !((`year' < 1989)&("`state'" == "TO")){

	* import
	
	if (`year' >= 2007)&(`year' < 2010){
		
		use cbo02 vinc_ativ educ using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear
		
		
		
		gen ano_nasc = substr(data_nasc,-4,4)
		destring ano_nasc, replace
		gen idade = `year' - ano_nasc

		merge m:1 CNAE1 using "$crosswalk_dir/crosswalk_CNAE1_CNAE2.dta"
		drop _merge
		
	}
	else if (`year' > 2010){
		
		use cbo02 vinc_ativ educ using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear
		
		
	}
	else if (`year' == 2006){
		
		use cbo02 vinc_ativ educ using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear
		
		
	}
	else if (`year' == 2010){
	
		use cbo02 vinc_ativ educ using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear
		
		
	}
	else if ((`year' >= 2003)&(`year' <= 2005)){
	
		use cbo02 vinc_ativ educ using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear
		
		
	}
	else if (`year' == 2002){
	
		use cbo02 vinc_ativ educ using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear	

	
	}
	else if ((`year' < 2002)&(`year' >= 1995)){
	
		use cbo94 vinc_ativ educ using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear	
		
		
		merge m:1 cbo94 using "$crosswalk_dir/crosswalk_cbo94_cbo02.dta"
		drop _merge
	
	}
	else if (`year' == 1994){
	
		use cbo94 vinc_ativ educ using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear	
		
	
	}
	else {
	
		use cbo94 vinc_ativ educ using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear	
		
	
	}
	
	cap gen cbo94 = ""
	cap gen cbo02 = ""

	}
		
	gen year 	= `year'
	gen state 	= "`state'"
	
	
	* keep only 2016 and 2020 candidates
	
	** create list of 2016 and 2020 candidates
	
	use "$create_electoral_data_dir\pis_hour_sample.dta", clear
	
	** merging
	
	merge n:1 cpf using "$create_electoral_data_dir\df_candidatos_allyears.dta" // created in create_pis_sample.do
	
	keep if _merge == 3
	drop _merge
	
	* drop PIS if multiple jobs

	duplicates tag PIS year, gen(duplicates)
	
	bys PIS year (duplicates): gen has_duplicates = 1 if duplicates[_N] > 0 
	
//	drop if has_duplicates == 1
//	
//	drop duplicates has_duplicates
	
	
		* yrs of education

		gen yrs_educ = .
		
		destring educ, force replace

		replace yrs_educ = 0 	if educ == 1
		replace yrs_educ = 2.5 	if educ == 2
		replace yrs_educ = 5 	if educ == 3
		replace yrs_educ = 7.5 	if educ == 4
		replace yrs_educ = 9 	if educ == 5
		replace yrs_educ = 11 	if educ == 6
		replace yrs_educ = 12 	if educ == 7
		replace yrs_educ = 14 	if educ == 8
		replace yrs_educ = 16 	if educ == 9
		replace yrs_educ = 18 	if educ == 10
		replace yrs_educ = 20 	if educ == 11
		
		* educational level
		
		gen I_hdropout 	= (educ <= 6)&(educ >= 1)
		gen I_hcomplete = (educ == 7)
		gen I_hmore 	= (educ > 7)&(!mi(educ))
		
		gen I_illiterate = (yrs_educ == 0)
		gen I_master 	 = (yrs_educ == 18)
		gen I_phd		 = (yrs_educ == 20)
		gen I_ugincomplete = (yrs_educ == 14)
		gen I_ugcomplete =	(yrs_educ == 16)
		gen I_gcomplete = (yrs_educ == 18)|(yrs_educ == 18)
		
		
		* saving
		
		save "$work_dir/`state'`year'_pis_RAIS.dta", replace
	
	}
	
}
}


**********************************************************************************************
* Append All Observations
**********************************************************************************************

clear

* Appending

foreach state in AC AL AP AM BA CE DF ES GO MA MT MS MG PA PB PR PE PI RJ RN RS RO RR SC SP SE TO {
forval year = 1985/2016 {

	cap append using "$work_dir/`state'`year'_pis_RAIS.dta", force


}
}

drop if mi(year)
drop if state == ""


* drop PIS if multiple jobs in different states

duplicates tag PIS year, gen(duplicates)

bys PIS year (duplicates): gen has_duplicates = 1 if duplicates[_N] > 0 

drop if has_duplicates == 1

drop duplicates has_duplicates

* saving

compress

save "$output_dir/pis_panel.dta", replace

* Removing temp data

local datafiles: dir "`workdir'" files "*.dta"

foreach datafile of local datafiles {
        rm `datafile'
}



