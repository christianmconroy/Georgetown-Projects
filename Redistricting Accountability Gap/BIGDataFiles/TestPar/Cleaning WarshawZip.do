/* Destringing Zip, Getting Rid of Non-Numeric, and Narrowing based on 
upper and lower bounds of VA data */ 
replace zip = "." if zip == "NA"
gen byte notnumeric = real(zip)==.
list if notnumeric == 1 & zip !="."
drop if zip == "t0b3p"
drop if zip == "R0M0J"
drop if zip == "8503`"
destring zip, gen(zipnum) 	
drop if zipnum == .

keep if zipnum >= 20105 & zipnum <= 24557
describe
sort zipnum
