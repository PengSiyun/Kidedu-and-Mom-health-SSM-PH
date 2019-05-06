*** Purpose: analysis of children's education and mother's health
*** Author: Siyun Peng
*** Date: February 1, 2019

*** Set working directory and load data
cd "G:\Dropbox\Dropbox\peng\Academia\Share with Shawn"
use ceah-mi-data, replace


*** Descriptive statistics (Table 1)
tab mmar1, gen(mmr)
tab medu, gen(med)

foreach x of varlist dep1 dep2 adl1 adl2 mage1 mwht mmr1-mmr3 med1-med4 ///
  minc1 mnch afem amar aliv asee atlk astr aclo aedu asch xedu xsch {
  qui sum `x'
  dis "`x'   " %5.2f r(mean) "  " %5.2f r(sd) "  " %2.0f r(min) "-" %2.0f r(max)
}

***************************************************************
//	#1 Examining different measures of adult children's education (ACE)
***************************************************************


*************Correlations between 4 measures (Table A1)*************
corr aedu asch xedu xsch

*************4 measures predicting health*************

***without controls
*unstandarized (Table 2)
eststo clear
foreach x of varlist aedu asch xedu xsch {
  eststo d1`x': reg dep1 `x', vce(robust) 
  
  eststo a1`x': logit adl1 `x', vce(robust) 
 }

esttab d1aedu d1asch d1xedu d1xsch ///
  using ~/desktop/edudep.csv, replace ///
  b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap nomti ///
  title("Table 2a. Depressive symptoms and ACE with no control")
  
esttab a1aedu a1asch a1xedu a1xsch ///
  using ~/desktop/eduadl.csv, replace ///
  b(%5.2f) se(%5.2f) star pr2(%5.2f) aic(%5.2f) bic(%5.2f) nogap nomti ///
  eform title("Table 2b. Funtional limitations and ACE with no control")  

*standarized
eststo clear
foreach x in aedu asch xedu xsch {
  egen sd`x'=std(`x')
}

foreach x of varlist sdaedu sdasch sdxedu sdxsch {
  eststo d1`x': reg dep1 `x', vce(robust) 
  eststo d2`x': reg dep2 dep1 `x', vce(robust) 

  eststo a1`x': logit adl1 `x', vce(robust) 
  eststo a2`x': logit adl2 adl1 `x', vce(robust) 
}

esttab d1sdaedu d1sdasch d1sdxedu d1sdxsch d2sdaedu d2sdasch d2sdxedu d2sdxsch ///
  using ~/desktop/edudepsd.csv, replace ///
  b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap nomti ///
  title("Table 2a2. Depressive symptoms and ACE with no control")
  
esttab a1sdaedu a1sdasch a1sdxedu a1sdxsch a2sdaedu a2sdasch a2sdxedu a2sdxsch ///
  using ~/desktop/eduadlsd.csv, replace ///
  b(%5.2f) se(%5.2f) star pr2(%5.2f) aic(%5.2f) bic(%5.2f) nogap nomti ///
  eform title("Table 2b2. Funtional limitations and ACE with no control")  

***with controls
*unstandarized
eststo clear
foreach x of varlist sdaedu sdasch sdxedu sdxsch {
  eststo df1`x': reg dep1 mage1 mwht i.mmar1 i.medu mnch afem amar aliv ///
  asee atlk astr aclo `x' , vce(robust) 
  eststo df2`x': reg dep2 dep1 mage1 mwht i.mmar1 i.medu mnch afem amar aliv ///
  asee atlk astr aclo `x', vce(robust) 

  eststo af1`x': logit adl1 mage1 mwht i.mmar1 i.medu mnch afem amar aliv ///
  asee atlk astr aclo `x', vce(robust)
  eststo af2`x': logit adl2 adl1 mage1 mwht i.mmar1 i.medu mnch afem amar aliv ///
  asee atlk astr aclo `x', vce(robust)
}

esttab df1sdaedu df1sdasch df1sdxedu df1sdxsch df2sdaedu df2sdasch df2sdxedu ///
  df2sdxsch using ~/desktop/edudepfull.csv, ///
  replace b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap nomti ///
  title("Depressive symptoms and ACE")
  
esttab af1sdaedu af1sdasch af1sdxedu af1sdxsch af2sdaedu af2sdasch af2sdxedu ///
  af2sdxsch using ~/desktop/eduadlfull.csv, ///
  replace b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap nomti ///
  eform title("Funtional limitations and ACE")  



***************************************************************
//	#2 Using prop. of BA as IV
***************************************************************


*************Models for depressive symptoms and ADL (Table 3)*************
preserve
keep if !mi(mwht, medu, astr,mmar1)
eststo clear
qui eststo d1: reg dep1 aedu, vce(robust)
estimates store depm1
qui eststo d2: reg dep1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar, vce(robust)
estimates store depm2
qui eststo d3: reg dep1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv ///
  asee atlk astr aclo, vce(robust)
estimates store depm3

qui eststo a1: logit adl1 aedu, vce(robust)
estimates store adlm1
qui eststo a2: logit adl1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar, vce(robust)
estimates store adlm2
qui eststo a3: logit adl1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv ///
  asee atlk astr aclo, vce(robust)
estimates store adlm3
  
esttab d1 d2 d3 using ~/desktop/dep.csv, replace b(%5.2f) se(%5.2f) star r2(%5.2f) ///
  bic(%5.2f) nobase nogap nomti wide title("Table 3. Depressive symptoms and ACE")

esttab a1 a2 a3 using ~/desktop/adl.csv, replace eform b(%5.2f) se(%5.2f) star r2(%5.2f) ///
  bic(%5.2f) nobase nogap nomti wide title("Table 3. Functional limitations and ACE")

*************coefplot*************
tempfile c1 c2
coefplot depm1 depm2 depm3,	xline(0) ciop(col(black)) keep(aedu) ///
    saving(`c1') legend(off) coeflabels(aedu=" ") ///
	title("Depressive Symptoms") ///
	xtit("Effects of % children with BA")    
	
coefplot (adlm1,label("Base model")) (adlm2,label("Base model" "+ Controls")) ///
	(adlm3,label("Base model" "+ Controls" "+ Mother-Child" "   Relations")), ///
	xline(1) ciop(col(black)) keep(aedu) eform saving(`c2') ///
	title("Activity Limitation") ///
	xtit("OR of % children with BA") coeflabels(aedu=" ")   
graph combine "`c1'" "`c2'"
graph export ~/desktop/Fig2.jpg, replace

restore

*************Predicted values for depressive symptoms and ADL (Figure 2)*************
*Generate variables to make rugs on plot	
gen 	pipe = "|" if aedu != .	// What rug looks like
gen 	aedurand = aedu + rnormal(0,.005) // "jitter" age
gen 	where1 =  0.3				// Where on y-axis to put 1st rug
gen 	where2 =  0.2				// Where on y-axis to put 1st rug

tempfile g1 g2

qui reg dep1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk ///
  astr aclo, vce(robust)
margins , at(aedu = (0(0.1)1))
marginsplot , ytit("Predicted value of depressive symptoms") ylab(0(0.2)1, angle(h) ///
  grid gstyle(dot)) xlab(, grid gstyle(dot)) recastci(rarea)             ///
  ciopts(fintensity(30) lwidth(none)) title("Depressive Symptoms", size(medium))   ///
  xtit("Proportion of children with BA+") saving(`g1')                   ///
  addplot(scatter where1 aedurand, ms(none) mlabel(pipe) mlabcol(red) ///
  mlabpos(0) xlab(0(.1)1) ylab(0.3 (0.1) 0.9) legend(off))  
  
qui logit adl1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk ///
  astr aclo, vce(robust)
margins , at(aedu = (0(0.1)1))
marginsplot , ytit("Predicted probability of having any activity limitation")  ///
  ylab(0(0.1)0.6, angle(h) grid gstyle(dot)) xlab(, grid gstyle(dot))      ///
  recastci(rarea) ciopts(fintensity(30) lwidth(none)) saving(`g2')         ///
  title("Activity Limitation", size(medium)) xtit("Proportion of children with BA+") ///
  addplot(scatter where2 aedurand, ms(none) mlabel(pipe) mlabcol(red) ///
  mlabpos(0) xlab(0(.1)1) ylab(0.2 (0.1) 0.6) legend(off))
  
graph combine "`g1'" "`g2'"
graph export ~/desktop/Fig2.png, replace
  
*Histogram (Alternative to rugs)  
//addplot(hist aedu, fcol(none) yaxis(2) yscale(range(0 0.15) off alt axis(2)))           


***************************************************************
//	#3. Interactions
***************************************************************

eststo clear
local IVs aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk astr aclo
foreach x in c.aliv c.asee c.atlk c.astr c.aclo {
    qui eststo: reg dep1 `IVs' c.aedu#`x', vce(robust)
}
esttab , b(%9.3f) se(%9.3f) star nomti nogap                        ///
  keep(aedu c.aedu#c.aliv c.aedu#c.asee c.aedu#c.atlk      ///
	   c.aedu#c.astr c.aedu#c.aclo) title("Depressive symptoms interactions")

eststo clear
foreach x in i.medu i.mwht i.mmar1 c.mnch c.afem c.amar c.aliv c.asee c.atlk ///
  c.astr c.aclo {
    qui eststo: logit adl1 `IVs' c.aedu#`x', vce(robust)
}
esttab , b(%9.3f) se(%9.3f) star nomti nogap                        ///
  keep(aedu 1.medu#c.aedu 2.medu#c.aedu 3.medu#c.aedu 1.mwht#c.aedu ///
       2.mmar1#c.aedu 3.mmar1#c.aedu c.aedu#c.mnch c.aedu#c.afem    ///
	   c.aedu#c.amar c.aedu#c.aliv c.aedu#c.asee c.aedu#c.atlk      ///
	   c.aedu#c.astr c.aedu#c.aclo) title("Functional limitations interactions")

***************************************************************
//	#4 Auxiliary analyses
***************************************************************

* 1. Does including income matter? No -- estimates are not significantly
*    different than in reported analysis
mi est: reg dep1 aedu mage1 mwht i.mmar1 i.medu minc1, vce(robust)
mi est: logit adl1 aedu mage1 mwht i.mmar1 i.medu minc1, vce(robust)

* 2. What predicts average child education? white, marital status, and education
reg aedu mage1 mwht i.mmar1 i.medu, vce(robust)


* 3. individual kids interaction  
eststo clear
local IVs aedu mage1 mwht i.mmar1 i.medu mnch cfem cmar csee ctlk cliv cstr ///
      cclo
foreach x in c.cliv c.csee c.ctlk c.cstr c.cclo {
    qui eststo: reg dep1 `IVs' c.aedu#`x', vce(robust)
}
esttab , b(%9.3f) se(%9.3f) star nomti nogap                        ///
  keep(aedu c.aedu#c.cliv c.aedu#c.csee c.aedu#c.ctlk      ///
	   c.aedu#c.cstr c.aedu#c.cclo) title("Depressive symptoms interactions")

eststo clear
foreach x in c.cliv c.csee c.ctlk c.cstr c.cclo {
    qui eststo: logit adl1 `IVs' c.aedu#`x', vce(robust)
}
esttab , b(%9.3f) se(%9.3f) star nomti nogap                        ///
  keep(aedu c.aedu#c.cliv c.aedu#c.csee c.aedu#c.ctlk      ///
	   c.aedu#c.cstr c.aedu#c.cclo) title("Functional limitations interactions")

* 4. Collinearity diagnostics
qui reg dep1 aedu mage1 mwht i.mmar1 i.medu mnch afem amar aliv asee atlk ///
  astr aclo, vce(robust)
vif

* 5. Check whether ACE predicts improvement or decline in ADL
logit adl2 aedu mage1 mwht i.mmar1 i.medu mnch afem amar asee atlk aliv astr ///
  aclo aedu if adl1 == 0, vce(robust)
logit adl2 aedu mage1 mwht i.mmar1 i.medu mnch afem amar asee atlk aliv astr ///
  aclo aedu if adl1 == 1, vce(robust)
  
* 6. Check whether nonlinear relationship with sex composition
recode afem (0 = 0) (0.01/0.99 = 1) (1 = 2), gen(cfem)
reg dep1 aedu mage1 mwht i.mmar1 i.medu mnch i.cfem amar aliv asee atlk ///
  astr aclo, vce(robust)
logit adl1 aedu mage1 mwht i.mmar1 i.medu mnch i.cfem amar aliv asee atlk ///
  astr aclo, vce(robust)

   
* 7. Auxiliary descriptive statistics
corr dep1 dep2 adl1 adl2
corr dep1 adl1 aedu
corr mage1 mwhy mmr1-mmr3 med1-med3 minc1 
corr mnch afem amar aliv asee atlk aedu

recode aedu (0 = 0) (0.01/0.99 = 1) (1 = 2), gen(cedu)

table medu, c(mean dep1 mean adl1)
table cedu, c(mean dep1 mean adl1)

table medu if adl1 == 0, c(freq mean adl2)
table cedu if adl1 == 0, c(freq mean adl2)

table medu if adl1 == 1, c(freq mean adl2)
table cedu if adl1 == 1, c(freq mean adl2)


* 8. check for difference in highest educated son vs highest educated daughter
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch amar asee atlk aliv fmedu ///
  if cfem == 1, vce(robust)
  
reg dep1 mage1 mwht i.mmar1 i.medu minc1 mnch amar asee atlk aliv mledu ///
  if cfem == 1, vce(robust)

logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch amar asee atlk aliv fmedu ///
  if cfem == 1, vce(robust)

logit adl1 mage1 mwht i.mmar1 i.medu minc1 mnch amar asee atlk aliv mledu ///
  if cfem == 1, vce(robust)

  
  
  
* 9 Check high school vs. college
foreach x in aedu aedu2 xedu xedu2 {
eststo d1`x': reg dep1 `x', vce(robust) 
eststo d2`x': logit adl1 `x', vce(robust) 
}

esttab d1aedu d1aedu2 d1xedu d1xedu2 ///
  using ~/desktop/edudep.csv, replace ///
  b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap nomti ///
  title("Table 2a2. Depressive symptoms: high school versus college")

esttab d2aedu d2aedu2 d2xedu d2xedu2 ///
  using ~/desktop/eduadl.csv, replace ///
  b(%5.2f) se(%5.2f) star r2(%5.2f) aic(%5.2f) bic(%5.2f) nogap nomti ///
  title("Table 2a2. ADL: high school versus college")
