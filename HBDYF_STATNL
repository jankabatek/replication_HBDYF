*------------------------j.kabatek@unimelb.edu.au(c)---------------------------*
*                      Happy Birthday, You're Fired!                           *
*			                  J.Kabatek, ILR Review, 2021                            *
*							                    -(J<)-                                       *
*        DATA CONSTRUCTION, JOB SEPARATION MODELS & SELECT DESCRIPTIVES        *
*------------------------------------------------------------------------------*
* README:                                                                      *
* To operationalize the code, change the global macro MAIN_FOL (line 95) to    *
* your preferred project folder. The code uses an (optional) PLOTTABS package  *
* that can be downloaded from: https://github.com/jankabatek/statapack         *
* Note that the paths, names, and versions of raw CBS datasets used in this    *
* code may have changed. Please make sure that all the global dataset macros   *
* (lines 66-902) are up to date. Note that these changes may affect the exact  *
* numbers of observations in your final dataset.                               *
*                                                                              *
* The folder data_sets will be populated by datasets that are used             *
* within the code. No need to copy any raw data into the folder.               *
*                                                                              *
* THIS CODE RUNS WITH PROPRIETARY DATA PROVIDED BY STATISTICS NETHERLANDS      *
* Further information @ github.com/jankabatek/replication_HBDYF                *
*                                                                              *
* Please note that the entire code takes several hours to execute.             * 
*------------------------------------------------------------------------------* 
 
*------------------------------------------------------------------------------*
*                                                                              *
*                             P R E A M B L E                                  *					
*                                                                              *
*------------------------------------------------------------------------------*
** (1) Initialization ---------------------------------------------------------*
clear
set more off 
set seed 123		// Randomization seed  

*------------------------------------------------------------------------------*
** (2) Auxiliary commands -----------------------------------------------------*

capture program drop DESTRING
program define DESTRING
	syntax varlist(min=1), [typ(string asis)] 
	qui d  `varlist', varl  	 
	local varlist_full =  r(varlist)
	
	foreach var in `varlist_full' {
		cap confirm string variable `var'
		if !_rc {
				qui cap drop `var'_f 
				gen `typ' `var'_f = real(`var')
				qui drop `var'
				qui rename `var'_f `var'
		}
	}			
	if "`typ'" == "" {
		compress `varlist_full' //if type left not specified, compress as much as possible.
	}
end

*------------------------------------------------------------------------------*
** (3) Version macros ---------------------------------------------------------*

global VERSION = strofreal(month(date("$S_DATE", "DMY")),"%02.0f") + "_" + strofreal(day(date("$S_DATE", "DMY")),"%02.0f")	

*------------------------------------------------------------------------------*  
** (4) Dataset macros ---------------------------------------------------------*

global gbapers2016 "G:/Bevolking/GBAPERSOONTAB/2016/geconverteerde data/GBAPERSOONTAB 2016V1.dta" 

global data_sommen2006 "G:/Arbeid/BAANSOMMENTAB/2006/geconverteerde data/140930 BAANSOMMENTAB 2006V2.dta"
global data_sommen2007 "G:/Arbeid/BAANSOMMENTAB/2007/geconverteerde data/140930 BAANSOMMENTAB 2007V2.dta"
global data_sommen2008 "G:/Arbeid/BAANSOMMENTAB/2008/geconverteerde data/140930 BAANSOMMENTAB 2008V2.dta"
global data_sommen2009 "G:/Arbeid/BAANSOMMENTAB/2009/geconverteerde data/140930 BAANSOMMENTAB 2009V2.dta"
global data_sommen2010 "G:/Arbeid/BAANSOMMENTAB/2010/geconverteerde data/140930 BAANSOMMENTAB 2010V2.dta"
global data_sommen2011 "G:/Arbeid/BAANSOMMENTAB/2011/geconverteerde data/140930 BAANSOMMENTAB 2011V2.dta"
global data_sommen2012 "G:/Arbeid/BAANSOMMENTAB/2012/geconverteerde data/140930 BAANSOMMENTAB 2012V2.DTA"
global data_sommen2013 "G:/Arbeid/BAANSOMMENTAB/2013/geconverteerde data/BAANSOMMENTAB 2013V1.DTA"
global data_sommen2014 "G:/Arbeid/BAANSOMMENTAB/2014/geconverteerde data/BAANSOMMEN2014TABV1.DTA"
global data_sommen2015 "G:/Arbeid/BAANSOMMENTAB/2015/geconverteerde data/BAANSOMMEN2015TABV1.DTA" 
global data_sommen2016 "G:/Arbeid/BAANSOMMENTAB/geconverteerde data/BAANSOMMEN2016TABV1.dta" 

global data_kenmerk2006 "G:/Arbeid/BAANKENMERKENBUS/2006/geconverteerde data/140930 BAANKENMERKENBUS 2006V3.dta"
global data_kenmerk2007 "G:/Arbeid/BAANKENMERKENBUS/2007/geconverteerde data/140930 BAANKENMERKENBUS 2007V2.dta"
global data_kenmerk2008 "G:/Arbeid/BAANKENMERKENBUS/2008/geconverteerde data/140930 BAANKENMERKENBUS 2008V3.dta"
global data_kenmerk2009 "G:/Arbeid/BAANKENMERKENBUS/2009/geconverteerde data/140930 BAANKENMERKENBUS 2009V3.dta"
global data_kenmerk2010 "G:/Arbeid/BAANKENMERKENBUS/2010/geconverteerde data/140930 BAANKENMERKENBUS 2010V3.dta"
global data_kenmerk2011 "G:/Arbeid/BAANKENMERKENBUS/2011/geconverteerde data/140930 BAANKENMERKENBUS 2011V3.dta"
global data_kenmerk2012 "G:/Arbeid/BAANKENMERKENBUS/2012/geconverteerde data/140930 BAANKENMERKENBUS 2012V2.DTA" 
global data_kenmerk2013 "G:/Arbeid/BAANKENMERKENBUS/2013/geconverteerde data/BAANKENMERKENBUS2013V2.DTA"
global data_kenmerk2014 "G:/Arbeid/BAANKENMERKENBUS/2014/geconverteerde data/BAANKENMERKENBUS2014V1.DTA"
global data_kenmerk2015 "G:/Arbeid/BAANKENMERKENBUS/geconverteerde data/BAANKENMERKENBUS2015V2.dta"
global data_kenmerk2016 "G:/Arbeid/BAANKENMERKENBUS/geconverteerde data/BAANKENMERKENBUS2016V1.dta"  

*------------------------------------------------------------------------------* 
** (5) Folder macros ----------------------------------------------------------*

global MAIN_FOL "H:/MINWAGE/"
global DOFILES 	"${MAIN_FOL}/DOFILES"
global MISC 	"${MAIN_FOL}/MISC"
global DATA  	"${MAIN_FOL}/DATA"
global EST 		"${MAIN_FOL}/EST"
global LOG 		"${MAIN_FOL}/LOG"
global GRAPHS	"${MAIN_FOL}/GRAPHS"
global RES	 	"${MAIN_FOL}/RES"
*------------------------------------------------------------------------------* 
** (6) Initialization - project environment & logs ----------------------------*

cd $MAIN_FOL
for any DOFILES MISC DATA EST LOG GRAPHS RES: cap mkdir X
** load optional PLOTTABS visualization command
cap do "${MISC}/PLOTTABS.do"
cap log close
cap log using "${LOG}/log_${VERSION}" , replace
 
*------------------------------------------------------------------------------*
*                                                                              *
*                        R E P L I C A T I O N  C O D E                        *					
*                                                                              *
*------------------------------------------------------------------------------* 
** (a) Data generation ------------------------------------------------------- *

forvalues year = 2006/2015{  
 
	use "$gbapers2016", clear
	
	** restrict to people aged 14 - 26 in the given year:
	gen int BDyear 		= real(gbageboortejaar)
	gen byte age_eoy 	= `year' - BDyear
	label var age_eoy "Age at the end of given year"
	drop if age_eoy<14 | age_eoy>28
	
 	** define basic demographics:
	gen byte Generatie 	= real(gbageneratie)
	gen byte Gebmaand 	= real(gbageboortemaand)
	gen byte MF 		= real(gbageslacht)
	keep rinpersoon Generatie Gebmaand  MF age_eoy 
 
	** retrieve labor supply data
	merge 1:m rinpersoon using `"${data_sommen`year'}"', keep(match) ///
	keepusing(svdg blsv fiscloon kaldg deeltijdfactorbaanid baanid) gen(merge_BK_BS)
	
	merge 1:m baanid using `"${data_kenmerk`year'}"', keep(match master) /// 
	keepusing( baanid aanvangbaanid eindebaanid beidbaanid datumaanvangbaanid /// 
	datumeindebaanid soortbaanid arbeidsrelatiebaanid sectbaanid ) gen(merge_PT_BK)
	
	** sector labels
	label define sect_en 0 "0 Unknown                   " 1 "1 Agriculture               " 2 "2 Tobacco                   " 3 "3 Construction              " 4 "4 Dredging                  " 5 "5 Wood, brush & pckging     " 6 "6 Timber                    " 7 "7 Furniture                 " 8 "8 Wood, wholesale           " 9 "9 Print & graphics          " 10 "10 Metal industry           " 11 "11 Electronics              " 12 "12 Metal and engineering    " 13 "13 Bakeries                 " 14 "14 Sugar processing         " 15 "15 Butchers                 " 16 "16 Butchers other           " 17 "17 Retail                   " 18 "18 Cleaning                 " 19 "19 Supermarkets             " 20 "20 Port companies           " 21 "21 Ports - other            " 22 "22 Inland waterways         " 23 "23 Fisheries                " 24 "24 Merchants                " 25 "25 Transport - KLM          " 26 "26 Transport - Rail         " 27 "27 Transport - Post         " 28 "28 Taxi & Ambulance         " 29 "29 Public transport         " 30 "30 Private bus trans        " 31 "31 Other transit - people   " 32 "32 Other transit - goods    " 33 "33 Hospitality              " 34 "34 Hospitality - catering   " 35 "35 Health, ment.&soc.       " 36 "36 ? No descr. in the docs  " 38 "38 Banks                    " 39 "39 Insurance companies      " 40 "40 Publishers               " 41 "41 Wholesale 1              " 42 "42 Wholesale 2              " 43 "43 Business services 1      " 44 "44 Business services 2      " 45 "45 Business services 3      " 46 "46 Dairy                    " 47 "47 Textile                  " 48 "48 Stone, cement, glass     " 49 "49 Chemical industry        " 50 "50 Food industry            " 51 "51 General industry         " 52 "52 Lending companies        " 53 "53 Security firms           " 54 "54 Cultural institutions    " 55 "55 Other bussines and proff." 56 "56 Painting companies       " 57 "57 Plastering companies     " 58 "58 Roofing companies        " 59 "59 Mortar companies         " 60 "60 Stonemasons              " 61 "61 Gov - education & science" 62 "62 Gov - law & order        " 63 "63 Gov - defense            " 64 "64 Gov - munic. & provinces " 65 "65 Gov - public utilities   " 66 "66 Gov - other institutions " 67 "67 Work & (re)integration   " 68 "68 Rail construction        " 69 "69 Telecommunications       " 99 "99 Unknown                  " ,replace
	DESTRING sectbaanid 
	label values sectbaanid sect_en

	** daily wages
	gen dwage = fiscloon/kaldg/5*7
	
	** identify workers who only work as apprentices (sb==2)
	gen byte sb = 2*(soortbaanid=="2")
	bys rinpersoon baanid (datumaanvangbaanid aanvangbaanid): egen byte sbmax2=max(sb)

	** restrict to one person-job-spell observation
	by rinpersoon baanid datumaanvangbaanid (aanvangbaanid): keep if _n==1
	by rinpersoon baanid: gen int N_ireg_dates = _N
	by rinpersoon baanid: gen int n_ireg_dates = _n
	
	** identify multiple jobs per person, duplicities of promotions:
	sort rinpersoon beidbaanid datumaanvangbaanid
	by rinpersoon beidbaanid : gen NF_ireg_dates = _N
	by rinpersoon beidbaanid: gen nf_ireg_dates = _n 

	** months and days of job-spell start/end 
	gen aan_mx = substr(datumaanvangbaanid,5,2)
	gen aan_dx = substr(datumaanvangbaanid,7,2)
	gen ein_mx = substr(datumeindebaanid  ,5,2)
	gen ein_dx = substr(datumeindebaanid  ,7,2)
	gen yr     = substr(datumaanvangbaanid,1,4)
	
	DESTRING *_mx *_dx, typ(byte)
	DESTRING yr, typ(int)

	gen byte EIN_mx = ein_mx *(ein_mx <88) + 12*(ein_mx ==88)
	gen byte age_ev = age_eoy  - (Gebmaand > EIN_mx)
	
	** month-year indicators & spell duration (overall & within-year)
	gen int year = `year'
	gen fyear = year+(EIN_mx-1)/12
	gen ayear = yr  +(aan_mx-1)/12
	gen int spell = (year*12+(EIN_mx)) - (yr*12+(aan_mx-1))  //(fyear - ayear)*12 + 1
	gen ayear_cens = max(ayear,`year')
	gen int spell_cens = round( (fyear - ayear_cens)*12 + 1 ) 
	 
	** months till birthday at the end of the within-year job-spell
	gen byte tbd2 = (Gebmaand>EIN_mx)*(Gebmaand - EIN_mx) + (Gebmaand<=EIN_mx)*(Gebmaand - EIN_mx+12)

	** monthly age w/ monthly increments (at the end of the within-year job-spell & at begining of the spell)
	gen ageyr  = (12-tbd2 + 12*(age_ev))/12
	gen age_st = ageyr - spell/12

	** age at the start and tbd at the start:
	gen age_an = age_eoy - (Gebmaand > aan_mx) - (year - yr)
	gen byte tbda = (Gebmaand>aan_mx)*(Gebmaand - aan_mx) + (Gebmaand<=aan_mx)*(Gebmaand - aan_mx+12)
	gen tbda_yr = (12-tbda + (age_an-15)*12)/12 + 15

	** FT wage on a weekly basis:
	gen FTwage = dwage/deeltijdfactorbaanid
	gen FTwage2_blsv = blsv/kaldg/5*7/deeltijdfactorbaanid
	
	*****************************************
	
	** identify job-spells that did not end on 31 Dec. 
	gen byte event = (ein_mx>=1 & ein_mx<= 12)
	
	keep spell* arbeidsrelatiebaanid sbmax* event *_mx tbd2 Gebmaand age* 	///
	sect* beid*  fyear year MF rinpersoon baanid soortbaanid  FTwage* 		///
	deeltijdfactorbaanid Generatie *ireg_dates 

	** split job-spells into monthly records
	gen long id =_n
	stset (spell_cens), f(event) id(id)
	stsplit time, every(1)

	** recover time to birthday in the given month
	sort id time
	by id: gen CD = _N  - _t
	gen t_true  = spell - CD
	gen byte MND =  (CD/12 - floor(CD/12))*12
	gen byte TBD = tbd2 + MND
	replace TBD = TBD - 12 if TBD>12 

	** calendar months derived from the last month of observation
	gen byte month = EIN_mx - MND
	replace month = month + 12 if month <1

	** monthly age
	gen agem = age_ev * 12 + (12 - tbd2) - CD
	gen agem2 = agem^2
	replace ageyr = agem/12

	** duration event & log(duration)
	gen jobloss = event
	replace jobloss = 0 if jobloss==.
	gen lnj=log(_t) 
	
	** account for irregular job spells & promotions within the same firm
	gen byte jobloss3 = jobloss
	replace jobloss3 = 0 if event ==1 & n_ireg_dates != N_ireg_dates
	replace jobloss3 = 0 if event ==1 & nf_ireg_dates != NF_ireg_dates
	
	gen byte event2 = event
	replace event2 = 0 if event ==1 & n_ireg_dates != N_ireg_dates
	
	keep rinpersoon MF Generatie Gebmaand age_eoy baanid beidbaanid soortbaanid sectbaanid deeltijdfactorbaanid sbmax sbmax2 age_ev year spell_cens FTwage FTwage2_blsv event id _st _d _t _t0 time t_true TBD month agem agem2 jobloss ageyr jobloss3 event2 arbeidsrelatiebaanid
	drop if FTwage==. 
	
	save `"${DATA}/X_EMP_ST_`year'.dta"',replace
}

** account for irregular job spells across adjacent years:
forvalues year = 2006/2014{ 
use `"${DATA}/X_EMP_ST_`year'.dta"', clear

local yearplus = `year' +1
 
	append using `"${DATA}/X_EMP_ST_`yearplus'.dta"'
	cap drop TIME
	gen TIME = (year-2000)*12 + month 
	sort rinpersoon baanid TIME
	by rinpersoon baanid : replace jobloss3 = 0 if _n!=_N
	
	sort rinpersoon beidbaanid TIME
	by rinpersoon beidbaanid : replace jobloss3 = 0 if _n!=_N
	
	keep if year==`year'
	save `"${DATA}/X_EMP_ST_`year'.dta"', replace
	
}

** derive a single dataset combining all the waves: 
local varlist month year Gebmaand TBD baanid  jobloss3 agem ageyr sbmax2 t_true rinpersoon Generatie MF sectbaanid
use `varlist'  using  `"${DATA}/X_EMP_ST_2006.dta"', clear

drop if ageyr >26.1
drop if ageyr <14.5

forvalues year = 2007/2015{ 
	append using `"${DATA}/X_EMP_ST_`year'.dta"', keep(`varlist')
	drop if ageyr >26.1
	drop if ageyr <14.5
}

** another auxiliary time variable
gen int TIME = (year-2000)*12 + month 
sort baanid TIME t_true

** get rid of the remnants of partially-overlapping within-year job-spells
gen byte overlap = (baanid[_n] == baanid[_n+1]) & (TIME[_n] == TIME[_n+1])
drop if overlap==1
drop overlap

** calculate full job duration corresponding to the same baanid, excluding any intermittent OOLF periods
by baanid: gen int durdur = t_true[1] + _n - 1 
gen lnj = log(durdur)
drop t_true

** TBD auxiliary transformation 
gen byte TBD_trans = (-TBD +8)*(TBD<=7) + (-TBD + 12 + 8)*(TBD>7)
label define TBD_trans 1 " 7"  2 " 6" 3 " 5" 4 " 4" 5 " 3" 6 " 2"  7 " 1"  8 " 0"  9 "11"  10 "10" 11 " 9" 12 " 8", replace
label values  TBD_trans  TBD_trans
  
** a dummy marking the first month of the job  
gen byte AN=durdur==1

** apprentice dummy
gen byte apprentice = sbmax2 ==2 

sort rinpersoon TIME
by rinpersoon: gen durper = _n
compress

save `"${DATA}/X_EMP_ST_06_15.dta"', replace

forvalues year = 2006/2015 {
	cap rm `"${DATA}/X_EMP_ST_`year'.dta"'
}

*------------------------------------------------------------------------------*
** (b) Job Separation Models ------------------------------------------------- *

use TBD sectbaanid ageyr agem year month lnj apprentice Gebmaand jobloss* Generatie MF using    `"${DATA}/X_EMP_ST_06_15.dta"', clear
 
** drop buffer years
keep if year <2012

** principal sample selection
 
keep if ageyr >15.5 & ageyr<=23.5 & apprentice ==0

** additional covariates: age spline point
gen byte GT17 = (ageyr>17.5)

** additional covariates: birthday quarter dummy
gen byte Dx = TBD==12 | TBD<=2

** (2a) MAIN CLOGLOG REGRESSION MODEL 
global VARLIST_MAIN ibno7.TBD lnj i.GT17#(c.agem  c.agem#c.agem c.agem#c.agem#c.agem) i.month i.Gebmaand i.year
cloglog jobloss  ${VARLIST_MAIN}, eform robust

** (2b) age-specific regressions with a wedgeless quadratic age trend
forvalues i = 15/23{
	local j = `i' + 1
	cap cloglog jobloss  Dx lnj agem c.agem#c.agem i.month i.Gebmaand i.year if ageyr >=`i'.5 & ageyr<`j'.5 , eform robust
}	 

** (2c) GFC regression
gen byte CRIS = year ==2008 & month >6
replace CRIS = 1 if year ==2009
cap noi cloglog jobloss i.CRIS i.CRIS#i1.Dx lnj i.GT17#(c.agem c.agem#c.agem c.agem#c.agem#c.agem) i.month i.Gebmaand, eform robust

** (2d) heterogeneity regression
gen byte female = (MF==2)
gen byte Dx_f = Dx*female

gen byte immig1 = (Generatie==1)
gen byte immig2 = (Generatie==2)
gen byte Dx_i1  = Dx*immig1
gen byte Dx_i2  = Dx*immig2

gen sec_hor = (sectbaanid ==33)
gen sec_ret = (sectbaanid ==17)
gen sec_who = (sectbaanid ==19)
gen Dx_sec_hor = Dx*(sectbaanid ==33)
gen Dx_sec_ret = Dx*(sectbaanid ==17)
gen Dx_sec_who = Dx*(sectbaanid ==19)
 
cap noi cloglog jobloss Dx Dx_* female immig* sec_* lnj i.GT17#(c.agem c.agem#c.agem c.agem#c.agem#c.agem) i.month i.Gebmaand, eform

drop Dx_* sec_* 

** (2d) sector x time regressions
cap gen byte SEC_DISC = 1*(sectbaanid==17) + 2*(sectbaanid==19) + 3*(sectbaanid==33) ///
 + 4*(sectbaanid>60&sectbaanid<67) + 5*(sectbaanid==9 |sectbaanid==10 |sectbaanid==11 /// 
		|sectbaanid==21 |sectbaanid==23 |sectbaanid==24 |sectbaanid==25 |sectbaanid==26 ///
		|sectbaanid==28 |sectbaanid==29 |sectbaanid==30 |sectbaanid==38 |sectbaanid==39 ///
		|sectbaanid==40 |sectbaanid==43 |sectbaanid==44 |sectbaanid==53 |sectbaanid==49 ///
		|sectbaanid==60 |sectbaanid==68 |sectbaanid==69)  ///
 + 6*(sectbaanid==3 |sectbaanid==6 |sectbaanid==13 |sectbaanid==15 |sectbaanid==18 ///
		|sectbaanid==27 |sectbaanid==56 |sectbaanid==57 |sectbaanid==67)
replace SEC_DISC = 7 if SEC_DISC ==0
label define sect  1 "Retail" 2 "Supermarkets" 3 "Hospitality" 4 "Public sector" 5 "Unaffected industries" 6 "Heavily aff. industries" 7 "Rest", replace
label values SEC_DISC sect

mat COEF = [99 , 99 , 99 ]
preserve
forvalues year=2006/2011 {
	tempfile dat_`year'
	keep if year ==`year'
	save `dat_`year'', replace
	
	forvalues yearx = 0/1 { 
		forvalues i = 1/7{
			use `dat_`year'', clear
			keep if SEC ==`i' 
			cap noi cloglog jobloss  $VARLIST_MAIN, eform robust from(B_old_`i', skip)  iterate(10)
			mat B_old_`i' = e(b)
			mat COEF = [COEF \ `year', `i' , exp(B_old_`i'[1,1]) ]
		}
	}
	
	restore, preserve
}	

*------------------------------------------------------------------------------*
** (c) Select descriptives --------------------------------------------------- *

** Figure 5 (person-level work separation & work accession frequencies)
local Vlist agem rinpersoon sbmax2 jobloss3  year durdur AN TIME baanid
use `Vlist' using `"${DATA}/X_EMP_ST_06_15.dta"', clear
keep if year < 2013

** generate an auxiliary dataset of person-month records (regardless of work),
*  and merge it with the X_EMP work records
preserve

foreach i in 17 19 20 21 22 23 {
	global i = `i' 
	keep if  agem>=( (`i'-1.5)*12)    & agem<=   ( (`i'+1.5)*12)
	bys rinpersoon  agem (AN) : keep if _n==1 
	gen byte emp = 1

	tempfile aux_emp
	save `aux_emp', replace

	use rinpersoon gbageboortejaar gbageboortemaand gbageneratie gbageslacht using  "$gbapers2016", clear
	gen int BDyear = real(gbageboortejaar)

	keep if (BDyear>(2006 - (`i'+1.5) ))& (BDyear < (2012 - (`i'-1.5)))
	gen int m_BD = monthly( gbageboortejaar + "m" + gbageboortemaand, "YM")

	gen int AAN = max( m_BD+(`i'-1.5)*12 ,  monthly( "2006m01", "YM") )
	gen int EIN = min( m_BD+(`i'+1.5)*12 ,  monthly( "2012m12", "YM") )

	gen byte spell = EIN-AAN +1
	gen id = _n
	gen event = 0
	drop gba* BDyear 
	compress
	
	stset (spell), f(event) id(id)
	drop EIN id event   spell
	compress
	
	stsplit time, every(1)
	gen agem =( AAN + _t0) - m_BD
	gen ageyr = agem/12
	compress
	
	drop _*
	sum agem
	sum ageyr
	merge 1:1 rinpersoon agem using `aux_emp'
 
	replace emp = 0 if emp==. 
	bys rinpersoon (agem) : gen byte JL_ABS = emp[_n+1]==0 & emp[_n] ==1 if emp[_n+1] !=.
	by  rinpersoon (agem) : gen byte AN_ABS = emp==1 & emp[_n-1] ==0 if emp[_n-1] !=.
	
	keep if ageyr>=(`i'-1) & ageyr<(`i'+0.8)
	
	drop m_BD  AAN time agem _merge
	save `"${DATA}/del_`i'.dta"', replace
	restore, preserve
	
}
restore, not

tempfile aux
local Vlist2 ageyr emp JL_ABS AN_ABS 
 foreach i in 17 19 20 21 22 23 {
	global i = `i'
	use `Vlist2' using  `"${DATA}/del_`i'.dta"' if ageyr > $i - 0.5 &  ageyr<=  $i + 0.5 , clear
	gen byte TBD_trans = round( ((ageyr-${i})*12) + 6 )
	cap append using `aux'
	save `aux', replace
}
 
tab TBD_trans if AN_ABS ==1
tab TBD_trans if JL_ABS==1

** visualization (requires PLOTTABS routine - downloadable from github.com/jankabatek/statapack)
cap PLOTTABS TBD_trans if AN_ABS ==1 ,  clear options(`" name(FIG5,replace) xtitle("Months Remaining until Next Birthday", margin(small) ) ytitle("Avg. Job Accessions per Year") xline($i) ylabel(,nogrid) xsize(7) xlabel( 1 "5" 2 "4" 3 "3" 4 "2" 5 "1" 6 "0" 7 "11" 8 "10" 9 "9" 10 "8" 11 "7" 12 "6" ) "') gr(connect) div(42) pat

cap PLOTTABS TBD_trans if JL_ABS==1 ,  options(`" name(FIG5,replace) xtitle("Months Remaining until Next Birthday", margin(small) ) ytitle("Avg. Job Separations per Year") xline($i) ylabel(,nogrid) xsize(7) xlabel( 1 "5" 2 "4" 3 "3" 4 "2" 5 "1" 6 "0" 7 "11" 8 "10" 9 "9" 10 "8" 11 "7" 12 "6" ) legend(off) "') gr(connect) div(42) pat

log close
