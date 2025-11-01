' This program PROJECTS LFPRs for ages 55 to 100, SYOA

%afile = %0 

include get_lfpr_assumptions.prg

%nber_peaks = "1969q4 1973q4 1980q1 1990q3 2001q1 2007q4 2019q4"		' NBER business-cycle peak dates; these are used to create "deviation from peak" series, which are the ones used in estimation.
' NOTE: when the model was estimated last time (in 2009) some of these peaks may have been different. We used to use peaks different from NBER ones. (check especially the 1969 and the 1973 peaks).


' files that contains estimated equations for LFPRs for ages 55 to 74 by SYOA

%lfpr_eqs_5574 = "lfpr_eqns_5574" 		' this file contains estimated LFPR equations -- basic specification, and specification with LE
%lfpr_eqs_5574_path = @datapath + "\" + %lfpr_eqs_5574 + ".wf1"	' full path


' Data for exogenous variables
' need projected values for
' DI rates
' education
' marital status (MSshare)
' replacement ratio and earnings test series (from d-file)
' for ages 54 and younger we would also need RUs, but we don't need them for 55 to 74 ages.

' DI rates
%di_data = @wordq(%dfilelist,4) ' (e.g., "op1202o") 			' workfile that contgains the DI data needed for adjustments to LFPRs
%di_data_path = @datapath + "\" + %di_data + ".wf1"			' full path

' education
%ed_data = "eduscores_plus" 				' workfile that contains PROJECTED values for education score series
%ed_data_path = @datapath + "\" + %ed_data + ".wf1"	' full path

' from cpso_nilf file we need historical POP DATA
%nilf_data = "cpso_nilf" 						
%nilf_data_path = @datapath + "\" + %nilf_data + ".wf1"			' full path

' file with CNIpop data from Demo -- the program assumes this is a workfile saved in the default folder; the file should contain ALL data from demo, both .hist and .alt2 already concatenated into single series
%demo_data = "cnipopms_" + @wordq(%assumpt,1)
%demo_data_path = @datapath + "\" + %demo_data + ".wf1"			' full path


' Add factors -- these option regulate whether the addfactors from ADfile are aplied.
' When we run this "for real" both of these options will always be "Y" because the add factor file will contain the necessary series even if they are zeros
' Apply the individual addfactors p.._add?
%add = "Y"		' enter "Y" or "N" case sensitive
' Apply the second set of addfactors p.._add2?
%add2 = "Y"		' enter "Y" or "N" case sensitive
' If either of tyhe above is "Y", provide the source for the add factors file


' ** Output created by this program
%thisfile = "lfpr_proj_55100"
%outputpath = @datapath + "\"


' ****** END of Update section

   wfcreate(wf={%thisfile}, page=a) a 1901 2105
   pagecreate(page=q) q 1900Q2 2105Q4
   pagecreate(page=m) m 1900M2 2105M12
   pagecreate(page=vars) a 1901 2105


' define useful lists
%agesy = " 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74"			' age by single year

' define useful samples
' annual
wfselect {%thisfile}
pageselect a
smpl @all

sample hist @first {%yrlasthist}		' sample called 'hist' covers historical period
sample est 1994 {%yrlasthist}		' sample called 'est' was used for estimating the equations; we start it in 1994
sample mdl {%modelstartyr} @last	' sample called 'mdl' (for MODEL) covers the period for which we run the model; it can start right after the historical period, or at any date earlier than that.

' define useful samples
' quarterly
wfselect {%thisfile}
pageselect q
smpl @all

sample hist @first {%datelasthist}		' sample called 'hist' covers historical period
sample est 1994 {%datelasthist}			' sample called 'est' was used for estimating the equations; we start it in 1994
sample mdl {%modelstart} @last		' sample called 'mdl' (for MODEL) covers the period for which we run the model

%h1 = @otod(@dtoo(%datelasthist) +1) 	' quarter RIGHT AFTER the last historical quarter
sample proj {%h1} @last					' sample 'proj' for "projections" starts in the quarter RIGHT AFTER the last historical data


logmode l			' this progtram will display log messages
%msg = "Running LFPR_proj_5574.prg" 
logmsg {%msg}
logmsg 


' *** 1. Load LFPR equations for ages 55 to 74
wfselect {%thisfile}
pageselect q
smpl @all 

%msg = "Loading estimated equations for LFPRs, ages 55 to 74..."
logmsg {%msg}
%msg = " ... from " + %lfpr_eqs_5574 + ".wf1 ..."
logmsg {%msg}

wfopen {%lfpr_eqs_5574_path}
pageselect estimation
smpl @all
for %a {%agesy}
		copy {%lfpr_eqs_5574}::estimation\eq_pf{%a} {%thisfile}::q\eq_pf{%a} 				' copy the equations and RENAME them into eq_p{%s}{%a}; this name is used in the forecast command below.
		copy {%lfpr_eqs_5574}::estimation\eq_pm{%a} {%thisfile}::q\eq_pm{%a} 			' copy the equations and RENAME them into eq_p{%s}{%a}; this name is used in the forecast command below.																																					
next

' Also, need to copy:
' Coeffcients for rradj... and pot_et.. series; they are scalars named like rrcf/s/a/ and etcf.
for %a 62 63 64 65 66 67 68 69
	' load coeffcients for rradj...  series (these exist only for ages 62 to 69)
	copy {%lfpr_eqs_5574}::estimation\rrcfm{%a} {%thisfile}::q\*	
	copy {%lfpr_eqs_5574}::estimation\rrcff{%a} {%thisfile}::q\*
next
' load coeffcients for pot_et...  series (it does not differ by age or sex)
copy {%lfpr_eqs_5574}::estimation\etcf {%thisfile}::q\*

wfclose {%lfpr_eqs_5574}


%msg = "Done." 
logmsg {%msg}
logmsg

' ** 2. Create FE differentials
' For ages 1617 to 5054 these are created using the business-cycle equations.
' For ages 55 and older we do NOT incolude the business-cycle effect in the LFPR equations.
' Thereofore, all FE differentials are zero by definition.
' I create them here only becuase Firtran expects to have these series in the file.
%msg = "Creating FE differentials for ages 5559 6064 6569 7074 75o -- all are set to zero"
logmsg {%msg}

wfselect {%thisfile}
pageselect q
smpl @all 

for %s m f 
	for %a 5559 6064 6569 7074 75o
		series dp{%s}{%a}_fe = 0
	next
next


' ** 3. Load projected values for exogenous variables
%msg = "Loading projected data from external sources..."
logmsg {%msg}

' *DI data*
wfselect {%thisfile}
pageselect q
smpl @all

%msg = "..." + %di_data + "... "
logmsg {%msg}

' dbopen(type=aremos) %di_data_path
for %s f m 
	for %a 55 56 57 58 59 60 61
		copy {%di_data}::q\r{%s}{%a}di r{%s}{%a}di
	next
next
' close {%di_data}

' Create DI rates for ages 62 to74 by using lagged values of r..61di
wfselect {%thisfile}
pageselect q
smpl @all

for %s f m 
	series r{%s}62di = r{%s}61di(-4)
	series r{%s}63di = r{%s}61di(-8)
	series r{%s}64di = r{%s}61di(-12)
	series r{%s}65di = r{%s}61di(-16)
	series r{%s}66di = r{%s}61di(-20)
	series r{%s}67di = r{%s}61di(-24)
	series r{%s}68di = r{%s}61di(-28)
	series r{%s}69di = r{%s}61di(-32)
	series r{%s}70di = r{%s}61di(-36)
	series r{%s}71di = r{%s}61di(-40)
	series r{%s}72di = r{%s}61di(-44)
	series r{%s}73di = r{%s}61di(-48)
	series r{%s}74di = r{%s}61di(-52)
next

' make graphs showing these disability rates -- for checking
group di_males rm55di rm56di rm57di rm58di rm59di rm60di rm61di rm62di rm63di rm64di rm65di rm66di rm67di rm68di rm69di rm70di rm71di rm72di rm73di rm74di
group di_females rf55di rf56di rf57di rf58di rf59di rf60di rf61di rf62di rf63di rf64di rf65di rf66di rf67di rf68di rf69di rf70di rf71di rf72di rf73di rf74di

freeze(g_di_males) di_males.line
freeze(g_di_females) di_females.line


' *data from D-file* (rradj_... and pot_et_...)
wfselect {%thisfile}
pageselect q
smpl @all

%msg = "..." + %dfile + "... "
logmsg {%msg}

' dbopen(type=aremos) %dfile_path

for %a 62 63 64 65 66 67 68 69		
	copy {%dfile}::q\rradj_m{%a} rradj_m{%a}
	copy {%dfile}::q\rradj_f{%a} rradj_f{%a}
	copy {%dfile}::q\pot_et_txrt_{%a} pot_et_txrt_{%a}
next

for %s f m 
	for %a {%agesy}  
		copy {%dfile}::q\n{%s}{%a} n{%s}{%a}			' load QUARTERLY pop data from D-file
		'rename n{%s}{%a} n{%s}{%a}_dfile 	' rename to indicate source
	next
next


' *Education*
wfselect {%thisfile}
pageselect q
smpl @all

%msg = "...education... "
logmsg {%msg}

wfopen {%ed_data_path}
for %a {%agesy}
	for %s f m
		copy {%ed_data}::q\edscore{%s}{%a}_a {%thisfile}::q\edscore{%s}{%a} 	' copy edscore series and rename them to be edscorem65 (instead fo edscorem65_a)
	next
next
wfclose {%ed_data}

' *Mar Status*

%msg = "...Demo CNIpop by marital status... "
logmsg {%msg}

%msg = "..." + %demo_data + "... "
logmsg {%msg}

' Load CNIpop data provided by Demo; we use mid-year (Jul) data
wfopen {%demo_data}
pageselect jul
smpl @all

wfselect {%thisfile}
pageselect a			' Demo data for CNIpop is ONLY annual
smpl @all

for %a {%agesy}
	for %s m f
		copy {%demo_data}::jul\n{%s}{%a} {%thisfile}::a\n{%s}{%a}_jul 		' rename series with _jul in the name; this will indicate the data provided by Demo
		copy {%demo_data}::jul\n{%s}{%a}ms {%thisfile}::a\n{%s}{%a}ms_jul
		copy {%demo_data}::jul\n{%s}{%a}nm {%thisfile}::a\n{%s}{%a}nm_jul
		copy {%demo_data}::jul\n{%s}{%a}ma {%thisfile}::a\n{%s}{%a}ma_jul
	next
	copy {%demo_data}::jul\n{%a} {%thisfile}::a\n{%a}_jul
next

wfclose {%demo_data}

' load data from CPSO_NILF
%msg = "...CPSO_NILF data by marital status... "
logmsg {%msg}

%msg = "..." + %nilf_data + "... "
logmsg {%msg}

wfopen {%nilf_data_path}
pageselect a
smpl @all

wfselect {%thisfile}
pageselect a			' these data are annual only
smpl @all

' dbopen(type=aremos) %nilf_data_path
for %s f m 
	for %a {%agesy}  
		
		copy {%nilf_data}::a\n{%s}{%a}nm {%thisfile}::a\n{%s}{%a}nm_nilf
		copy {%nilf_data}::a\n{%s}{%a}ma {%thisfile}::a\n{%s}{%a}ma_nilf
		copy {%nilf_data}::a\n{%s}{%a}ms {%thisfile}::a\n{%s}{%a}ms_nilf
		
		copy {%nilf_data}::a\n{%s}{%a} {%thisfile}::a\n{%s}{%a}_nilf
	next
next
' close {%nilf_data}
wfclose {%nilf_data}

' *** Load add factors from the add factor file
wfselect {%thisfile}
pageselect q
smpl @all

if %add = "Y" then
	%msg = "...Add factors p..._add from " + %adfile + "... "
	logmsg {%msg}

	' dbopen(type=aremos) %adfile_path
	for %s m f 
		for %a {%agesy}
			copy {%adfile}::q\p{%s}{%a}_add p{%s}{%a}_add
		next
	next
	' close {%adfile}
endif

if %add2 = "Y" then
	%msg = "...Add factors p..._add2 from " + %adfile + "... "
	logmsg {%msg}

	' dbopen(type=aremos) %adfile_path
	for %s m f 
		for %a {%agesy}
			copy {%adfile}::q\p{%s}{%a}_add2 p{%s}{%a}_add2
		next
	next
	' close {%adfile}
endif

if %add = "N" then
	%msg = "...Add factors p..._add are set to ZERO ..."
	logmsg {%msg}

	for %s m f 
		for %a {%agesy}
			series p{%s}{%a}_add = 0
		next
	next
endif

if %add2 = "N" then
	%msg = "...Add factors p..._add are set to ZERO ..."
	logmsg {%msg}

	for %s m f 
		for %a {%agesy}
			series p{%s}{%a}_add2 = 0
		next
	next
endif

%msg = "Done loading data for exogenous variables."
logmsg {%msg}
logmsg


'***************************************************************
'*** 4. Create projections from the estimated equations ****
' ***************************************************************

'*** Preliminary step -- create MSshare

' Definition: MSshare_/s/a/ = n/s/a/ms / n/s/a/
' To create projected MS share we need projected  n/s/a/ms and n/s/a/ for SYOA ages 55 to 74.
' Here I create n/s/a/ms as follows:
' 	(1) using CPSO_NILF data (annual  and historical only), compute MSshare_/s/a/_hist = n/s/a/ms / n/s/a/
' 	(2) using Demo (Jul) data (annaul, both historical and projected), compute MSshare_/s/a/_jul = n/s/a/ms_jul / n/s/a/_jul
' 	(3) For historical period (period where both Demo data nd CPSO_NILF data are historical), compute the average deviation between MSshare_/s/a/_hist and MSshare_/s/a/_jul
' 	(4) Adjust MSshare_/s/a/_jul (both historical and projected) by the average deviation computed in (3), call it MSshare_/s/a/_jula
' 	(5) Create final MSshare_/s/a/ as follows:
' 			for historical period in CPSO_NILF, set MSshare_/s/a/ = MSshare_/s/a/_hist;
' 			for projected period (starting right after the historical in CPSO_NILF), set MSshare_/s/a/ = MSshare_/s/a/_jula
' 	(6) Convert the resulting annual MSshare_/s/a/ series to quarterly by Denton method.
' The resulting series MSshare_/s/a/ span historical and projected period and is used as input for the LFPR equations for ages 55 to 74.

' NOTE: 
' (1) 	CPSO_NILF data start in 1994; In current version CPSO_NILF data end in 2019. 
'		In the future, the end year will be a different (later) year. The program below accomodates this possibility automatically. 
' (2) Demo Jul data start in 2010; its last historical year is typically THREE years behind the TR year and ONE year behind the last historical for CPSO_NILF(e.g. for TR22, the last histrocal Year for Demo is 2019 and for CPSO_NiLF it is 2020).
' (3) I compute MSshare series in ANNUAL frequency and then convert MSshare to Quarterly by Denton method. 
' 		This might be slightly different from converting pop series from A to Q and them computing MSshare.

%msg = "Compute projected MSshare series for all ages."
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect a
smpl @all

' denote certain dates 
'%nilf_first = "2004" 	' first year when we overwrite historical totals in CPSO_NILF
%nilf_last = nm55ms_nilf.@last 		' YEAR of the last historical observation in CPSO_NILF file; here I assume that ALL n/s/a/ series in CPSO_NILF end on the same date
%demo_first = nm55ms_jul.@first 	' YEAR of the first non-NA obs in Demo CNIpop data; currently 2010, likely not to change
%demo_hist = @str(!TRyr - 3) 		' YEAR of the last HISTORICAL obs for Demo CNIpop data
%projyr = @str(@val(%nilf_last)+1) ' YEAr immediately after the %nilf_last

' 	(1) using CPSO_NILF data (annual and historical only), compute MSshare_/s/a/_hist = n/s/a/ms / n/s/a/
smpl @all
for %s m f 
	for %a {%agesy}
			series MSshare_{%s}{%a}_hist = n{%s}{%a}ms_nilf / n{%s}{%a}_nilf
	next
next

' 	(2) using Demo (Jul) data (annaul, both historical and projected), compute MSshare_/s/a/_jul = n/s/a/ms_jul / n/s/a/_jul
smpl @all
for %s m f 
	for %a {%agesy}
			series MSshare_{%s}{%a}_jul = n{%s}{%a}ms_jul / n{%s}{%a}_jul
	next
next

' 	(3) For historical period (period where both Demo data nd CPSO_NILF data are historical), compute the average deviation between MSshare_/s/a/_hist and MSshare_/s/a/_jul
smpl {%demo_first} {%demo_hist} 	' historical period -- when ALL series are historical
for %a {%agesy}
	for %s m f 
		genr dmsshare_{%s}{%a} = MSshare_{%s}{%a}_hist - MSshare_{%s}{%a}_jul		' compute the deviation over the historical period
		!msshare{%s}{%a}_adjs = @mean(dmsshare_{%s}{%a})								' set the adj factor to be constant and equal to the mean deviation over the historical period
	next
next

' 	(4) Adjust MSshare_/s/a/_jul (both historical and projected) by the average deviation computed in (3), call it MSshare_/s/a/_jula

smpl @all
for %a {%agesy}
	for %s m f 
		'series msshare{%s}{%a}_adj = !msshare{%s}{%a}_adjs
		series MSshare_{%s}{%a}_jula = MSshare_{%s}{%a}_jul + !msshare{%s}{%a}_adjs 
	next
next


' 	(5) Create final MSshare_/s/a/ as follows:
' 			for historical period in CPSO_NILF, set MSshare_/s/a/ = MSshare_/s/a/_hist;
' 			for projected period (starting right after the historical in CPSO_NILF), set MSshare_/s/a/ = MSshare_/s/a/_jula

for %a {%agesy}
	for %s m f
		smpl @first {%yrlasthist}  ' historical data for TR; it should contain historical data for CPSO_NILF and thus msshare
		series MSshare_{%s}{%a} = MSshare_{%s}{%a}_hist
		
		
		
		smpl {%projyr} @last
		series MSshare_{%s}{%a} = MSshare_{%s}{%a}_jula
		
		'make charts of MSshare LEVELs
		smpl 1990 @last
		group t MSshare_{%s}{%a}_hist MSshare_{%s}{%a}_jul MSshare_{%s}{%a}
		freeze(_gcmb_msshare{%s}{%a}) t.line  	' chart that shows the combined MSshare series
	next
next

' At this point we have ANNUAL MSshare series for all a-s groups

' 	(6) Convert the resulting annual MSshare_/s/a/ series to quarterly by Denton method.
wfselect {%thisfile}
pageselect q
smpl @all

for %a {%agesy}
	for %s m f
		copy(c="dentona") a\MSshare_{%s}{%a} q\MSshare_{%s}{%a}
	next
next

'delete series no longer needed
wfselect {%thisfile}
pageselect a
smpl @all
delete n*_nilf 

%msg = "MSshare series for all ages are created."
logmsg {%msg}
logmsg

'*** projected MSshare series are now done.
' ** !!! modifications for the use of the Demo data for CNIpop END here; the rest of the code is unchanged !!! ****


' *** Now create the projected LFPRs*****

%msg = "Creating projections for ages " + %agesy
logmsg {%msg}
logmsg

' ** Create projected LFPRs
wfselect {%thisfile}
pageselect q
smpl mdl				' period for which we run the model

for %s m f
  	for %a {%agesy}
  		eq_p{%s}{%a}.fit p{%s}{%a}_diadj_p			' create projected values for DI-adjusted LFPRs; these are the predicted values of the Dependent Variable from the estimated equations.
  		'eq_p{%s}{%a}_le.fit p{%s}{%a}_le_diadj_p	' same for equations with LE
  	next
next


' *** 5.  Translate the projected DI-adjusted LFPRs into "pure LFPRs" as follows:

' Assume: "trend LFPR" used in the DI-adjustment process in the projections will be equal to the projected LFPR itself.
' Thus:
' LFPR_diadj = LFPR + "trend LFPR" * DIrate
' assume: for projections "trend LFPR" = LFPR
' Therefore, 
' LFPR_diadj = LFPR + LFPR * DIrate
' LFPR = LFPR_diadj / (1+DIrate) 		
' use this expression to transform the  DI-adjusted LFPRs into "pure" LFPRs

wfselect {%thisfile}
pageselect q
smpl mdl

for %s m f
	for %a {%agesy}
		series p{%s}{%a}_p = p{%s}{%a}_diadj_p / (1 + r{%s}{%a}di)			
	next
next


' *** 6. Apply individual addfactors to p/s/a/_p and append the projected values to the observed historical LFPRs -- name the resul p/s/a/ 

' Load historical LFPRs

%msg = "Loading historical LFPRs for ages 55 to 79 from " + %dfile_path
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl @all

' ** Load the historical LFPRs for ages 55 and older by SYOA (Dfile has LFPRs for ages 55 to 79)
' dbopen(type=aremos) %dfile_path
for %s m f 
	for !a = 55 to 79
		copy {%dfile}::q\p{%s}{!a} p{%s}{!a}
	next
next
' also load LFPR for 80o
copy {%dfile}::q\pm80o pm80o
copy {%dfile}::q\pf80o pf80o
' close {%dfile}

' Apply individual addfactors to projected p..._p for ages 55 to 74. 
' AND create continuous series that combine HISTORICAL LFPRs with projected ones.
' For most ages, these addfactors will be zero, but they may be nonzero for several fo the series. 
' Series p/s/a/ already exist in workfile - they are equal to historical values and NAs everywehwre else.
' In this step we change the value of these series only in sample 'proj' where they currently have NAs. The new values in sample 'proj' will be p/s/a/_p + p/s/a/_add + p/s/a/_add2

%msg = "... Finalizing LFPRs for ages 55 to 74 by applying addfactors and appending to historical values ..."
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl proj			' the sample starts in the quarter RIGHT AFTER the last historical quarter							
for %s m f 
	for %a {%agesy}
		p{%s}{%a} = p{%s}{%a}_p + p{%s}{%a}_add + p{%s}{%a}_add2
	next
next

' *** 7. Create projected LFPRs for 5yr age groups (this is needed only for comparison in this file)
' FORTRAN -- this part isn't really needed because Fortran creates the 5yr age group LFPRs later in the process.
' Let's keep it around for now as it will provide a some useful checks for ages 55-74

wfselect {%thisfile}
pageselect q
smpl @all		' I do this for the entire sample, but we might want to do it for the 'model' period only

for %s m f 
	' 5559
	series n{%s}5559 = n{%s}55 + n{%s}56 + n{%s}57 + n{%s}58 + n{%s}59
	series p{%s}5559 = (p{%s}55_p * n{%s}55 + p{%s}56_p * n{%s}56 + p{%s}57_p * n{%s}57 + p{%s}58_p * n{%s}58 + p{%s}59_p * n{%s}59) / n{%s}5559
	'6064
	series n{%s}6064 = n{%s}60 + n{%s}61 + n{%s}62 + n{%s}63 + n{%s}64
	series p{%s}6064 = (p{%s}60_p * n{%s}60 + p{%s}61_p * n{%s}61 + p{%s}62_p * n{%s}62 + p{%s}63_p * n{%s}63 + p{%s}64_p * n{%s}64) / n{%s}6064
	' 6569
	series n{%s}6569 = n{%s}65 + n{%s}66 + n{%s}67 + n{%s}68 + n{%s}69
	series p{%s}6569 = (p{%s}65_p * n{%s}65 + p{%s}66_p * n{%s}66 + p{%s}67_p * n{%s}67 + p{%s}68_p * n{%s}68 + p{%s}69_p * n{%s}69) / n{%s}6569
	' 7074
	series n{%s}7074 = n{%s}70 + n{%s}71 + n{%s}72 + n{%s}73 + n{%s}74
	series p{%s}7074 = (p{%s}70_p * n{%s}70 + p{%s}71_p * n{%s}71 + p{%s}72_p * n{%s}72 + p{%s}73_p * n{%s}73 + p{%s}74_p * n{%s}74) / n{%s}7074
next


' *****************************************************************
' ********** Projected LFPRs for ages 75 to 100, SYOA ******
' ****************************************************************

%msg = "Projections for ages 75 and older..."
logmsg {%msg}
logmsg

' *** Step 1 *** 

'*** Define certain parameters (values for these may be changed in the futre)
!cm75 = 0.920 		' coefficicnt used in projecting LFPRs for M75 to M79
!cm80 = 0.965 		' coefficicnt used in projecting LFPRs for M80 to M100

!cf75 = 0.900 		' coefficicnt used in projecting LFPRs for F75 to F79
!cf80 = 0.965 		' coefficicnt used in projecting LFPRs for F80 to F100


' *** Step 2 ***
' Project LFPRS for ages 75 to 79

%msg = "... Projecting LFPRs for ages 75 to 79..."
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl @all

for %s m f 
	for %a 75 76 77 78 79
		%alag = @str(@val(%a)-1)
		smpl mdl						' project over the period for which we run the model
		series p{%s}{%a}_p = p{%s}{%alag}(-4) * !c{%s}75 + dp{%s}75o_fe 
		smpl proj						' for period right after the last historical, append the projected avlyues
		p{%s}{%a} = p{%s}{%a}_p
	next
next

' *** Step 3 ****
' Project LFPRS for ages 80 to 84

%msg = "... Projecting LFPRs for ages 80 to 84 (preliminary)..."
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl mdl 		' project over the period for which we run the model

' NOTE: 	in theory we can use this formula all the way to age 94. 
' 			This would require using values of pm79 and pf79 lagged back to 2004 (if we start running the model in 2019Q4)
' 			Historical values starting in 2004Q1 look good. Prior to 2004Q1 they look stange (piecewise linear, too artificial). This may be a reason why this formular was not applied to ages 85 and older in the old model.
' 		FOR NOW, I programmed this the way it is done in the "old model" -- use this formula for ages 80 to 84, and a different formular (see below) for ages 85 to 94.
!ct = 0
for %a 80 81 82 83 84 '85 86 87 88 89 90 91 92 93 94
	!ct = !ct +1
	!lag = 4 * !ct
	for %s m f
		series p{%s}{%a}_p = p{%s}79(-!lag) * !c{%s}80  ^ !ct  + dp{%s}75o_fe 
	next
next

' *** Step 4 ****
' Project LFPRS for ages 85 to 94

%msg = "... Projecting LFPRs for ages 85 to 94 (preliminary)..."
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl mdl 		' project over the period for which we run the model

' NOTE: 	FOR NOW, I programmed this the way it is done in the "old model" 
'			We may want to change it in the future
!ct = 0
for %a 85 86 87 88 89 90 91 92 93 94
	!lag1 = 24+ 4 * !ct
	!lag2 = !lag1 + 7
	!pw = !ct +6
	for %s m f
		series temp_sm = 0
		for !i=!lag1 to !lag2
			temp_sm = temp_sm + p{%s}79(-!i)
		next
		series p{%s}{%a}_p = ( temp_sm / 8 ) * !c{%s}80 ^ !pw + dp{%s}75o_fe
		delete temp_sm
	next
	!ct = !ct +1
next

' *** Step 6 ****
' Project LFPRS for ages 95 to 100

%msg = "... Projecting LFPRs for ages 95 to 100 (preliminary)..."
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl mdl 		' project LFPRs over the period for which we run the model

for %s m f 
	for %a 95 96 97 98 99 100
		%alag = @str(@val(%a)-1)
		series p{%s}{%a}_p = p{%s}{%alag}_p * !c{%s}80 + dp{%s}75o_fe 
	next
next

' *** Step 7 ****
' Adjust LFPRs for ages 80 to 11 from p/s/a/_p to p/s/a/

%msg = "... Finalizing LFPRs for ages 80 to 100 ..."
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl @all


' ** Load the historical and projected population for ages 80 to 100 by SYOA
' Note that this will REPLACE series n/s/a/ for ages 55 to 74 that I loaded eaerlier for computing the MSshare. This is OK, we no longer need those series. 
' QQQQQ Should I delete all the pop series that were used earlier prior to doing this step????
' dbopen(type=aremos) %dfile_path
for %s m f 
	for !a = 80 to 100
		copy {%dfile}::q\n{%s}{!a} n{%s}{!a}
	next
next
' close {%dfile}

' ** Compute LFPR for 80o (preliminary)
wfselect {%thisfile}
pageselect q
smpl @all

for %s m f
	series p{%s}80o_p =  (p{%s}80_p * n{%s}80 + _
								p{%s}81_p * n{%s}81 + _
								p{%s}82_p * n{%s}82 + _
								p{%s}83_p * n{%s}83 + _
								p{%s}84_p * n{%s}84 + _
								p{%s}85_p * n{%s}85 + _
								p{%s}86_p * n{%s}86 + _
								p{%s}87_p * n{%s}87 + _
								p{%s}88_p * n{%s}88 + _
								p{%s}89_p * n{%s}89 + _
								p{%s}90_p * n{%s}90 + _
								p{%s}91_p * n{%s}91 + _
								p{%s}92_p * n{%s}92 + _
								p{%s}93_p * n{%s}93 + _
								p{%s}94_p * n{%s}94 + _
								p{%s}95_p * n{%s}95 + _
								p{%s}96_p * n{%s}96 + _
								p{%s}97_p * n{%s}97 + _
								p{%s}98_p * n{%s}98 + _
								p{%s}99_p * n{%s}99 + _
								p{%s}100_p * n{%s}100 ) / (n{%s}80 + n{%s}81 + n{%s}82 + n{%s}83 + n{%s}84 + n{%s}85 + n{%s}86 + n{%s}87 + n{%s}88 + n{%s}89 + n{%s}90 + n{%s}91 + n{%s}92 + n{%s}93 + n{%s}94 + n{%s}95 + n{%s}96 + n{%s}97 + n{%s}98 + n{%s}99 + n{%s}100)
next

' ** Create 'final' LFPRs by using the ratio of LFPR for 80o
wfselect {%thisfile}
pageselect q
smpl proj  	' starting in the quarter right after the last historical data

'create continuous series fo rp/s/80o covering historical and projected.
for %s m f
	p{%s}80o = p{%s}80o_p
next

wfselect {%thisfile}
pageselect q
smpl mdl

' create 'final' LFPRs for ages 80 to 100
for %s m f
	for !a=80 to 100
		series p{%s}{!a} = p{%s}{!a}_p * (p{%s}80o / p{%s}80o_p)
	next
next

' **** ALL projected LFPRs have been created *****

%msg = "ALL projected LFPRs have been created"
logmsg {%msg}
logmsg


' *******make summary spool ******
wfselect {%thisfile}
pageselect q
smpl @all

spool _summary
string line1 = "This file was created on " + @date + " at " + @time + " by " + %usr
string line2 = "This file contains projected LFPRs for ages 55 to 100, SYOA." + @chr(13) + _
				" The projections are created starting in " + %modelstart + " (the model run), but in the final results the projected LFPRs are overwritten with the observed LFPRs for the historical period (up to and including " + %datelasthist + ")."

string line3 = " For ages 55 to 74, only one -- preferred -- version of projections is created, based on the estimated LFPR equatoins that include"  + @chr(13) + _
				 "  edscore, MSshare, (no time trend, no LE), loaded from " + %lfpr_eqs_5574_path  

string line4= "For ages 55 to 74, the projected LFPRs are based on the projected values for the exogenous variables loaded from:" + @chr(13) + _
				" (1) DI rates -- " + %di_data_path + @chr(13) + _
				" (2) Education (edscore) -- " + %ed_data_path + @chr(13) + _
				" (3) Marital status (MSshare) -- computed from population series in " + %demo_data_path + ". The projected values for MSshare series are created from the CNIpop data projections provided by the Demo group, adjusted (level-shifted) by the average deviation during the historical period (when both Demo data and CPSO_NILF data exist). Historical values for MSshare series are computed from data in CPSO_NILF file." + @chr(13) + _
				" (4) Replacement ratio and earnings test -- " + %dfile_path
				
if %add = "Y" then
	if %add2 = "Y" then
		string line5 = "Individual addfactors (p..._add) and overall addfactors (p..._add2) are applied to the projected LFPRs. The add factor series are loaded from " + %adfile_path
		else string line5 = "Individual addfactors (p..._add) ONLY are applied to the projected LFPRs. The add factor series are loaded from " + %adfile_path + ". The overall addfactors (p..._add2) are set to zero."
	endif
endif

if %add = "N" then
	if %add2 = "N" then
		string line5 = "All add factors are set to zero."
		else string line5 = "Individual addfactors are set to zero, while overall addfactors (p..._add2) are applied to the projected LFPRs. The add factor series are loaded from " + %adfile_path
	endif
endif

string line6= "For ages 55 to 74, TWO kinds of projected values are presented:" + @chr(13) + _
				 "(1) Projected value for the DI-adjusted LFPR -- named like pm62_diadj_p. " + @chr(13) + _
				 "(2) Projected value for the ''pure'' LFPR, named like pm62_p. This is obtained from the projected values for DI-adjusted LFPRs by ''un-doing'' the DI adjustment as described in the comments in the program code."

string line7= "For ages 75 and older, projected LFPRs are created based on the projected LFPRs for age 74 and age 79 (unchanged from the earlier version of the LFPR model). "+ @chr(13) + _
				"!!! This setup in the earlier model was likely influenced by the data limitations. Since we have longer historical data series, we can consider changing it. !!!"

string line8= "ALL projected values are located in page 'q'. " + @chr(13) + _
				"Projected values start in " + %h1 + " to match the projection period used in TR20" + %TR + "." + @chr(13) + _
				" In addition, using the projected LFPRs by SYOA, we compute projected LFPRs by 5yr age groups -- 5559, 6064, 6569, 7074. They are also shown in page 'q' and named like pm6064_p. "

string line9 = " Note that the new CNIpop data provided by the Demo group is used ONLY to compute the projected MSshare series. When using projected population (for example, in computing the LFPRs for 5yr age groups), we still use pop series from the DFILE." 				 

_summary.insert line1 line2 line3 line4 line5 line6 line7 line8 line9
'_summary.display

delete line*

%wfpath = %outputpath + %thisfile + ".wf1"
%msg = "Saving workfile to " + %wfpath + "  Any file with an identical filename is overwritten."
logmsg {%msg}
logmsg
wfsave(2) %wfpath ' saves the workfile

logmsg Finished lfpr_proj_55100
logmsg


