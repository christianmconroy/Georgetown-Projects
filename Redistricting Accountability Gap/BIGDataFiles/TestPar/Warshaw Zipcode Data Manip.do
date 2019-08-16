* Narrow down to just Virginia 
use "C:\Users\chris\Documents\GeorgetownMPPMSFS\McCourtMPP\BIG Work\BIG Data Files\survey_idealpoints_withzips.dta", clear
gsort -zip
replace zip = "." if zip == "NA"
replace zip = "." if zip == "t0b3p"
replace zip = "." if zip == "R0M0J"
replace zip = "." if zip == "8503`"
drop if zip == "."

gen byte notnumeric = real(zip)==.
tab notnumeric

destring zip, gen(zip_num)

collapse (mean) zipideal=ideal_point zipidealse=ideal_point_se (count) numresp=respondent, by(zip_num)
keep if zip_num >= 20105 & zip_num <= 24657
describe


save "survey_idealpoints_withzips_va.dta", replace


