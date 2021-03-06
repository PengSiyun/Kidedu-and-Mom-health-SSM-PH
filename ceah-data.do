*** Purpose: prepare data for analysis of children's educ and mother's health
*** Author: Siyun Peng
*** Date: October 26, 2018
*** Version 15

*** Note: original data file names: 
* mom dyad level T1 Full Sample with mortality 8-1-2016.sent to Peng 1-2-18
* mom re kid T1T2 for dropbox.n1514.MGJS.07-25-2016

***************************************************************
//	#1 Cleanning data
***************************************************************
*** Loading data
cd "C:\Users\bluep\Dropbox\peng\Academia\Work with Shawn\SSM\data&old code"
use "mom dyad level T1 Full Sample with mortality 8-1-2016.sent to Peng 1-2-18.dta",replace

*** ID variables
rename (LSUID kidid) (mid cid)

*** Children's characteristics
rename (sex) (cfem)
recode educat (9 888 9997 9999 = .) (1/5 = 0) (6 7 = 1), gen(cedu)
recode educat (9 888 9997 9999 = .) (1/2 = 0) (3/7 = 1), gen(cedu2)
recode educat (9 888 9997 9999 = .) (1 = 8) (2 = 10) (3 = 12) (4 5 = 14) ///
  (6 = 16) (7 = 19), gen(csch)
recode x141 (1 2 = 1) (3/6 = 0) (9 = .), gen(cmar)
recode within2 (888 9997 9998 9999 = .), gen(cliv)
recode freqsee freqtalk (7 = 0) (6 = 1) (5 = 2) (4 = 3) (3 = 4) (2 = 5) ///
  (1 = 6) (888 9997 9998 9999 = .), gen(csee ctlk)
recode x22a1_c x21a1_c (0 9 = .), gen(cstr cclo)
recode cstr (4/7=4), gen(cstr4)
recode cclo (1/4=1) (5=2) (6=3) (7=4), gen(cclo4)

*** Mother's characteristics
rename (numalive) (mnch)
recode f1 (1 = 1) (4 = 2) (5 = 3) (6=.) , gen(mmar1)
label define mmar1 1 "married" 2 "divorced" 3 "widowed"
label values mmar1 mmar1
gen mage1 = 2002 - g1y
recode mastrace (0 = 1) (1/23 = 0), gen(mwht)
recode g3 (9 = .) (1 2 = 0) (3 = 1) (4 5 = 2) (6 7 = 3), gen(medu)
label define medu 0 "less than high shool" 1 "high school degree" 2 "some college" 3 "college degree or higher"
label values medu medu

recode income2 (1 = .5) (2 = 1.5) (3 = 2.5) (4 = 3.5) (5 = 4.5) (6 = 6.25) ///
  (7 = 10) (9 = .), gen(minc1)

*** Mother's health (T1)
recode health1 (-3 = .), gen(srh1)
recode e8a e8b e8c e8d e8e e8f e8g (9 = .)
alpha e8a e8b e8c e8d e8e e8f e8g, gen(dep1)
replace dep1 = dep1 - 1

recode e2 (5 = 0), gen(adl1)

lab var mid   "mother id"
lab var cid   "child id"
lab var cfem  "child female"
lab var cedu  "child BA+"
lab var cedu2 "child high school+"
lab var csch  "child years schooling"
lab var cmar  "child married"
lab var cliv  "child live within 2 hours"
lab var csee  "child frequency visit"
lab var ctlk  "child frequency talk"
lab var cstr  "child strain"
lab var cclo  "child closeness"
lab var mnch  "mother # children"
lab var mage1 "mother age"
lab var mmar1 "mother marital status 1"
lab var mwht  "mother white"
lab var medu  "mother education"
lab var minc1 "mother household income 1"
lab var srh1  "mother srh 1"
lab var dep1  "mother depression 1 (a = 0.78)"
lab var adl1  "mother adl 1"

*** Keep analysis variables
order mid cid cfem cedu cedu2 csch cmar cliv csee ctlk cstr cclo mnch mage1 mmar1 ///
  cstr4 cclo4 mwht medu minc1 srh1 dep1 adl1
keep mid-adl1
tempfile d1
save `d1', replace


******* Merging T2 data ******
use "mom re kid T1T2 for dropbox.n1514.MGJS.07-25-2016.dta",replace

*** ID variables
rename (LSUID kidid) (mid cid)

*** Mother's health (T2)
rename (D2T2 momhealtht2) (adl2 srh2)
recode D15aT2 D15bT2 D15cT2 D15dT2 D15eT2 D15fT2 D15gT2 (9 = .)
alpha D15aT2 D15bT2 D15cT2 D15dT2 D15eT2 D15fT2 D15gT2, gen(dep2)
replace dep2 = dep2 -1
lab var mid   "mother id"
lab var cid   "child id"
lab var srh2  "mother srh 2"
lab var dep2  "mother depression 2 (a = 0.82)"
lab var adl2  "mother adl 2"

order mid cid srh2 dep2 adl2
keep mid-adl2

merge 1:1 mid cid using `d1'
drop if _merge == 1 // discard 5 mothers (24 children) in T2 but not in T1
gen t2 = ( _merge == 3 )
drop _merge

*** Generate children context measures
egen aedu  = mean(cedu), by(mid)
egen aedu2  = mean(cedu2), by(mid)
egen xedu  = max(cedu), by(mid)
egen xedu2  = max(cedu2), by(mid)
egen asch  = mean(csch), by(mid)
egen xsch  = max(csch), by(mid)

egen fmedu = mean(cedu) if cfem == 1, by(mid)
egen mledu = mean(cedu) if cfem == 0, by(mid)

foreach x in fem mar see tlk liv str clo str4 clo4 {
  egen a`x' = mean(c`x'), by(mid)
}

lab var t2    "in T2"
lab var aedu  "pr of children with BA+"
lab var aedu2 "pr of children with High school+"
lab var xedu  "at least one child with BA+"
lab var xedu2 "at least one child with High school+"
lab var asch  "avg children years of schooling"
lab var xsch  "max children years of schooling"
lab var fmedu "pr of daughters with BA+"
lab var mledu "pr of sons with BA+"
lab var afem  "pr of children female"
lab var amar  "pr of children married"
lab var asee  "avg frequency of visit"
lab var atlk  "avg frequenc of talk"
lab var aliv  "pr of children live within 2 hours"
lab var astr  "avg strain"
lab var aclo  "avg closeness"

*** Prepare mother data for analysis
egen pone = tag(mid)
keep if pone
order mid t2 mnch mage1 mmar1 minc1 mwht medu srh1 srh2 dep1 dep2 adl1 adl2 ///
  aedu aedu2 xedu xedu2 asch xsch fmedu mledu afem amar asee atlk aliv astr4 ///
  aclo4 astr aclo fem mar see tlk liv str clo str4 clo4
keep mid-clo4
keep if !mi(dep1, adl1)
keep if !mi(mwht, medu, astr, mmar1)

***************************************************************
//	#2 Auxilary analysis
***************************************************************
*** Running multiple imputation for missing wave 1 measures
mi set wide
mi reg imp minc1 mwht medu srh1 srh2 dep2 adl2 astr
mi imp chain (ologit) medu srh1 srh2 (logit) mwht adl2 (regress) minc1 dep2 ///
  astr = mnch mage1 i.mmar1 dep1 adl1 aedu xedu asch xsch afem amar asee    ///
  atlk aliv aclo, add(20) augment rseed(91169)
save ceah-mi-data, replace

**** Alternative data for kids level with relationship quality range 1-4
order mid t2 mnch mage1 mmar1 minc1 mwht medu srh1 srh2 dep1 dep2 adl1 adl2 ///
  aedu aedu2 xedu xedu2 asch xsch fmedu mledu afem amar asee atlk aliv astr4 ///
  aclo4 astr aclo cfem cmar csee ctlk cliv cstr cclo cstr4 cclo4
keep mid-cclo4
keep if !mi(dep1, adl1)
keep if !mi(mwht, medu, astr, mmar1)


