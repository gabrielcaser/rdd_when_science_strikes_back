*********************************************************************************************
* Description: this program creates a list o PIS
**********************************************************************************************

**********************************************************************************************
* Initial Commands
**********************************************************************************************

cap log close                       /* closes open log files */
set more off                        /* tells Stata not to pause after each step of calculation */
clear                               /* clears current memory */
set memory 500m                     /* increases available memory */
global RAIS_dir 		= "D:\clean_rais_software\data\input\RAIS_stata_files"
global work_dir 		= "C:\Users\RA_guest\Dropbox\Gus_Gabriel\softwares\worker_level\create_pis_panel_rais"
global output_dir 		= "C:\Users\RA_guest\Dropbox\Gus_Gabriel\softwares\worker_level\create_pis_panel_rais\output"


cap mkdir "$output_dir"

**********************************************************************************************
* Main Code - collecting the data
**********************************************************************************************

foreach state in AC AL AP AM BA CE DF ES GO MA MT MS MG PA PB PR PE PI RJ RN RS RO RR SC SP SE TO {
	
forval year = 1995/2016{
	
//if !((`year' < 1989)&("`state'" == "TO")){

	* import
	
		use PIS vinc_ativ horas using "$RAIS_dir/`state'/`state'`year'_comp.dta", clear
		
	* gen var
	
		gen year = `year'
		
		gen state 	= "`state'"
		
	* keeping only active workers

		keep if vinc_ativ == 1

	* drop part time

		drop if horas < 44
		
	* Keeping only PIS
	
		keep PIS
		
	* saving
		
	save "$work_dir/`state'`year'_pis_rais.dta", replace
		
	}
	

}


**********************************************************************************************
* Append All Observations
**********************************************************************************************

clear

* Appending

foreach state in AC AL AP AM BA CE DF ES GO MA MT MS MG PA PB PR PE PI RJ RN RS RO RR SC SP SE TO {
forval year = 1995/2016{

	cap append using "$work_dir/`state'`year'_pis_rais.dta", force
	cap rm "$work_dir/`state'`year'_pis_rais.dta"

}
}

bys PIS: gen nobs = _N

keep if nobs > 5

keep PIS

duplicates drop

* sampling and saving

sample 5

sort PIS 

drop if PIS == "00000000000"

save "$output_dir/pis_hour_sample.dta", replace

* removing temp data

foreach state in AC AL AP AM BA CE DF ES GO MA MT MS MG PA PB PR PE PI RJ RN RS RO RR SC SP SE TO {
forval year = 1995/2016{

	cap rm "$work_dir/`state'`year'_pis_rais.dta"

}
}
