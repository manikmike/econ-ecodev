' This program PROJECTS LFPRs for age groups: 16-17, 18-19, 20-24, 25-29, ..., 50-54
' based on the LFPR equations estimated elsewhere

%afile = %0

include get_lfpr_assumptions.prg

%nber_peaks = "1969q4 1973q4 1980q1 1990q3 2001q1 2007q4 2019q4"		' NBER business-cycle peak dates; these are used to create "deviation from peak" series, which are the ones used in estimation.
' NOTE: when the model was estimated last time (in 2009) some of these peaks may have been different. We used to use peaks different from NBER ones. (check especially the 1969 and the 1973 peaks).

'*** Data sources

%di_data = @wordq(%dfilelist,4) ' (e.g., "op1202o") 	' workfile that contains the DI data needed for adjustments to LFPRs AND projected DI rates to build projections.
%di_data_path = @datapath + "\" + %di_data + ".wf1"			' full path

%ed_data = "eduscores_plus" 	' workfile that contgains edscore series created by Sven (we need these for f4549 and f5054)
%ed_data_path = @datapath + "\" + %ed_data + ".wf1"			' full path

%ru_proj = "ru_proj" 	' workfile that contains the projected RUs and RU_FEs
%ru_proj_path = @datapath + "\" + %ru_proj + ".wf1"		' full path

' Add factors
' Should we load the addfactors? (when we run this "for real" this will always be "Y" because the add factor file will contain the necessary series even if they are zeros)
' If so, enter "Y" and provide the path ro the %adfile
' If not, enter "N"
%add2 = "Y"


'*** Previously estimated equations

' BC equations
%bc_eqs = "lfpr_eqns_09" ' short name of WORKFILE containing the estimated equatiosn for LFPR business-cycle effects.
%bc_eqs_path = @datapath + "\" + %bc_eqs + ".wf1" 	' full path

' LFPR and related equations
%lfpr_eqs = "lfpr_eqns_1654"
%lfpr_eqs_path = @datapath + "\" + %lfpr_eqs + ".wf1"


' *** Output created by this program

%thisfile = "lfpr_proj_1654"			
%outputpath = @datapath + "\"

' ****** END of Update section

'wfcreate(wf={%thisfile}, page=q) q %datestart %projend
'pagecreate(page=a) a %yrstart %projendyr

   wfcreate(wf={%thisfile}, page=a) a 1901 2105
   pagecreate(page=q) q 1900Q2 2105Q4
   pagecreate(page=m) m 1900M2 2105M12
   pagecreate(page=vars) a 1901 2105

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
%msg = "Running lfpr_proj_1654.prg" 
logmsg {%msg}
logmsg 


' *** Load projected RUs 
' in THIS VERSION we load RUs fromthe projections created by the new model

wfselect {%thisfile}
pageselect q
smpl @all

%msg = "...RUs and RU_FEs from " + %re_proj_path + "... "
logmsg {%msg}

wfopen {%ru_proj_path}
pageselect q

for %s m f
	for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 
		copy {%ru_proj}::q\r{%s}{%a} {%thisfile}::q\
		copy {%ru_proj}::q\r{%s}{%a}_fe {%thisfile}::q\
	next
next
wfclose {%ru_proj}


' *** Load population series from D-file
' These are needed for various purposes -- to compute the POC indexes for some ages; to combined LFPRs into aggregate ones (such as create pm2024 from pm2024ms, pm2024ma, and pm2024nm)
wfselect {%thisfile}
pageselect q
smpl @all

%msg = "...population from " + %dfile + "... "
logmsg {%msg}

' dbopen(type=aremos) %dfile_path
	copy {%dfile}::q\nf1617nmc6u nf1617nmc6u
	copy {%dfile}::q\nf1617msc6u nf1617msc6u 
	copy {%dfile}::q\nf1617mac6u nf1617mac6u
	copy {%dfile}::q\nf1617 nf1617
	copy {%dfile}::q\nf1819nmc6u nf1819nmc6u
	copy {%dfile}::q\nf1819msc6u nf1819msc6u
	copy {%dfile}::q\nf1819mac6u nf1819mac6u
	copy {%dfile}::q\nf1819 nf1819
	
	for %a 2024 2529 3034 3539 4044 4549 5054
		copy {%dfile}::q\nm{%a}ms nm{%a}ms 
		copy {%dfile}::q\nm{%a}ma nm{%a}ma
		copy {%dfile}::q\nm{%a}nm nm{%a}nm
		copy {%dfile}::q\nm{%a} nm{%a}
	next
	for %a 2024 2529 3034 3539 4044
		copy {%dfile}::q\nf{%a}msc6u nf{%a}msc6u
		copy {%dfile}::q\nf{%a}msnc6 nf{%a}msnc6
		copy {%dfile}::q\nf{%a}mac6u nf{%a}mac6u
		copy {%dfile}::q\nf{%a}manc6 nf{%a}manc6
		copy {%dfile}::q\nf{%a}nmc6u nf{%a}nmc6u
		copy {%dfile}::q\nf{%a}nmnc6 nf{%a}nmnc6
		
		copy {%dfile}::q\nf{%a}ms nf{%a}ms
		copy {%dfile}::q\nf{%a}ma nf{%a}ma
		copy {%dfile}::q\nf{%a}nm nf{%a}nm
		copy {%dfile}::q\nf{%a} nf{%a}
	next
	for %a 4549 5054 
		copy {%dfile}::q\nf{%a}ms nf{%a}ms
		copy {%dfile}::q\nf{%a}ma nf{%a}ma
		copy {%dfile}::q\nf{%a}nm nf{%a}nm
		copy {%dfile}::q\nf{%a} nf{%a}
		
		copy {%dfile}::q\nf{%a}msc6u nf{%a}msc6u
		copy {%dfile}::q\nf{%a}mac6u nf{%a}mac6u
		copy {%dfile}::q\nf{%a}nmc6u nf{%a}nmc6u
	next
' close {%dfile}

' *** Load projected DI rates from op-file
wfselect {%thisfile}
pageselect q
smpl @all

%msg = "...DI rates from " + %di_data + "... "
logmsg {%msg}

' dbopen(type=aremos) %di_data_path
for %s m f 
	for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054
		copy {%di_data}::q\r{%s}{%a}di r{%s}{%a}di
	next
next
' close {%di_data}

' *** Load add factors from the add factor file
wfselect {%thisfile}
pageselect q
smpl @all

if %add2 = "Y" then
	%msg = "...Add factors from " + %adfile + "... "
	logmsg {%msg}

	'dbopen(type=aremos) %adfile_path
	for %s m f 
		for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054
			copy {%adfile}::q\p{%s}{%a}_add2 p{%s}{%a}_add2
		next
	next
	'close {%adfile}
endif

if %add2 = "N" then
	%msg = "...Add factors are set to ZERO ..."
	logmsg {%msg}

	for %s m f 
		for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054
			series p{%s}{%a}_add2 = 0
		next
	next
endif


' *** Load edscore created by Sven
wfselect {%thisfile}
pageselect q
smpl @all

%msg = "...Education scores from " + %ed_data + "... "
logmsg {%msg}

wfopen {%ed_data_path}
pageselect q

for %a 4549 5054
	copy {%ed_data}::q\edscoref{%a}_a {%thisfile}::q\edscoref{%a} 	' load edscoref/a/_a (the 0-4 index) and rename it edscoref/a/
next
wfclose {%ed_data}

%msg = "Done loading data for exogenous variables."
logmsg {%msg}
logmsg


' **** Load historical LFPRs so that we can eventually create a continuous series of LFPRs -- historical + projected

%msg = "Loading historical LFPRs for ages 1617 to 5054 " + %dfile_path
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl @all

' ** Load the historical LFPRs for ages 55 and older by SYOA (Dfile has LFPRs for ages 55 to 79)
' dbopen(type=aremos) %dfile_path
for %s m f 
	for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054
		copy {%dfile}::q\p{%s}{%a} p{%s}{%a}
	next
next

' close {%dfile}

' ***********************************************************
'**** Create projections from the estimated equations ****
' ************************************************************
%msg = "Creating projected LFPRs..." 
logmsg {%msg}
logmsg 

wfselect {%thisfile}
pageselect q
smpl @all

' *** Create trend variables for certain ages
' NOTE: these were simple sime trends in the historical period when we used them for estimating the equations. 
' Here we will phase them out in the future.


%msg = "Creating trend variables" 
logmsg {%msg}
logmsg 

' (!) create annual trend so that it looks like in Aremos (i.e. 99 = year 1999)
wfselect {%thisfile}
pageselect a
smpl @all

for %s m f
	for %a 1617 1819 2024 2529
		series tr_{%s}{%a} = @year - 1900
	next
next


'(2) make equivalent quaterly series
for %trends tr_m1617 tr_m1819 tr_f1617 tr_f1819 tr_m2024 tr_m2529 tr_f2024 tr_f2529
	copy(c=q) a\{%trends} q\*
next

' (3) phase out quarterly series
' Note that all dates below are hard-coded!
' Even when the last historical period moves beyond the dates used for the trend phase-out, the trends will remain as they are here..
' In other words, once last historical has moved past 2020Q4, NONE of these trends will have any effect


' For 1617 and 1819 -- trend would  by 0.2 in 2021q1, by 0.15 in 2021q2, etc. and becomes horizontal starting in 2021Q1
wfselect {%thisfile}
pageselect q
smpl @all

for %trends tr_m1617 tr_m1819 tr_f1617 tr_f1819
	!inc = 0.25
	for %d 2020Q1 2020Q2 2020Q3 2020Q4 
		!inc = !inc - 0.05
		smpl {%d} {%d}
		{%trends} = {%trends}(-1) + !inc
	next
	smpl 2021Q1 @last
	{%trends} = {%trends}(-1)
next
smpl @all

' For 2024, 2529 -- phase out the trend immediately:  trend is horizontal from the first quarter after the historical period

for %trends tr_m2024 tr_f2024 tr_m2529 tr_f2529
	smpl proj
	{%trends} = {%trends}(-1)
next
smpl @all

' (4) convert back to annual as average
wfselect {%thisfile}
pageselect a
smpl @all

delete tr_m1617 tr_m1819 tr_f1617 tr_f1819 tr_m2024 tr_m2529 	' delete the trends that already exists there; these continue upward indefinitely
for %trends tr_m1617 tr_m1819 tr_f1617 tr_f1819 tr_m2024 tr_m2529
	copy q\{%trends} a\*
next

	
' ** Create POC indexes using projected pop data from D-file
wfselect {%thisfile}
pageselect q
smpl @all

' Load series directly from D-file
copy {%dfile}::q\rf1617cu6 rf1617cu6
copy {%dfile}::q\rf1819cu6 rf1819cu6

for %a 4549 5054
	for %m ms ma nm
		copy {%dfile}::q\rf{%a}{%m}cu6 rf{%a}{%m}cu6
	next
next

' Old version, where we computed the series here
'for %a 1617 1819
'	series rf{%a}cu6 = (nf{%a}nmc6u + nf{%a}msc6u + nf{%a}mac6u) / nf{%a}
'next
'
'for %a 4549 5054
'	for %m ms ma nm
'		series rf{%a}{%m}cu6 = nf{%a}{%m}c6u / nf{%a}{%m}
'	next
'next

' *********** Load estimated equations **********
%msg = "Loading equations estimated elsewhere"
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl @all

' *** For BC effect
wfopen {%bc_eqs_path}
pageselect estimation			' we use equations named like eq_pm2024dpk3. Should we decide to use different equations, change the name in the COPY command below.
for %s m f 
	for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054
		copy {%bc_eqs}::estimation\eq_p{%s}{%a}dpk3 {%thisfile}::q\eq_p{%s}{%a}_bc 		' copy equations and RENAME them into eq_pm2024_bc. The code that follows uses these names.
	next
next
wfclose {%bc_eqs}

' For LFPRs and for RNL... ("out of LF" series; these are special for ages 1617 and 1819 only)
wfselect {%thisfile}
pageselect q
smpl @all

wfopen {%lfpr_eqs_path}

for %a 1617 1819
	' LFPR equations
	copy {%lfpr_eqs}::a\eq_pm{%a} {%thisfile}::q\eq_pm{%a} 		' copy equations named like eq_pm1617 and RENAME them into eq_pm1617, if needed. The code that follows uses these names.
	copy {%lfpr_eqs}::a\eq_pf{%a} {%thisfile}::q\eq_pf{%a}
	' RNL... equations, these vary by gender
	copy {%lfpr_eqs}::a\eq_rnlm{%a} {%thisfile}::q\eq_rnlm{%a} 			' copy equations named like eq_rnlm1617_bc and RENAME them into eq_rnlm1617, if needed
	copy {%lfpr_eqs}::a\eq_rnlf{%a}_h {%thisfile}::q\eq_rnlf{%a}_h 		' copy equations named like eq_rnlf1617_h_nobc and RENAME them into eq_rnlf1617_h, if needed
	copy {%lfpr_eqs}::a\eq_rnlf{%a}_so {%thisfile}::q\eq_rnlf{%a}_so 	' copy equations named like eq_rnlf1617_so_nobc and RENAME them into eq_rnlf1617_so, if needed
	' NOTE: these initial equation names, like eq_rnlf1617_h, might become different (say, we can decide to use equation eq_rnlf1617_h_bc instead) if we decide to use a different specification fo equation. 
	' 			This is why I rename them when I load them, so that the rest fo the code can rely on the sam equaton name even if we change it here in a later iteration of thsi program.
next

for %a 2024 2529 3034 3539 4044 4549 5054
	for %m ma ms nm
		copy {%lfpr_eqs}::a\eq_pm{%a}{%m} {%thisfile}::q\eq_pm{%a}{%m} 	' LFPR equations for MALES; 
	next
next

for %a 2024 2529 
	for %m ma ms nm
		copy {%lfpr_eqs}::a\eq_pf{%a}{%m}c6u_nosc {%thisfile}::q\eq_pf{%a}{%m}c6u 	' LFPR equations for FEMALES c6u; NOTE: these are the equations with NO SCHOOLING EFFECT (suffix _nosc)
		copy {%lfpr_eqs}::a\eq_pf{%a}{%m}nc6 {%thisfile}::q\eq_pf{%a}{%m}nc6 	' LFPR equations for FEMALES nc6; 
	next
next

for %a 3034 3539 4044 
	for %m ma ms nm
		copy {%lfpr_eqs}::a\eq_pf{%a}{%m}c6u {%thisfile}::q\eq_pf{%a}{%m}c6u 	' LFPR equations for FEMALES c6u; 
		copy {%lfpr_eqs}::a\eq_pf{%a}{%m}nc6 {%thisfile}::q\eq_pf{%a}{%m}nc6 	' LFPR equations for FEMALES nc6; 
	next
next

for %a 4549 5054
	for %m ma ms nm
		copy {%lfpr_eqs}::a\eq_pf{%a}{%m} {%thisfile}::q\eq_pf{%a}{%m} 	' LFPR equations for FEMALES; RENAME to eq_pf/age/ms/ 
	next
next

' also load coefficients from the estimated equations for schooling
for %s m f
	for %m ma ms nm
		copy {%lfpr_eqs}::a\sch_{%s}2024{%m} {%thisfile}::q\*
		copy {%lfpr_eqs}::a\sch_{%s}2529{%m} {%thisfile}::q\*
	next
next

wfclose {%lfpr_eqs}

' ******* Create the BC_effect series and FE differentials*****
%msg = "Creating the Business-Cycle effect series for all ages"
logmsg {%msg}
logmsg

' *** Create projected BC effect series named bc_/s/a/ using the estimated BC equations and the projected RUs loaded above
' These will be used as exogenous variables in projecting LFPRs
wfselect {%thisfile}
pageselect q
smpl @all

for %s m f 
	for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054
		series r{%s}{%a}_dpk3 = r{%s}{%a}	' This looks strange but there is a reason for it. 
														' The BC equatiosn have varibles named r/s/a/_dpk3 on RHS. 
														' To use the eq_.fit command (on next line) there MUST be series of this exact name.
														' We want to use the COEFs from the BC equations, but we intend to substitute the LEVEL of RU in place of r/s/a/_dpk3 (which was deviation from peak). This is how it is done in the "old" model as well.
														' This looks non-standard, but this IS the intention here.
		eq_p{%s}{%a}_bc.fit bc_{%s}{%a}
		
		delete r{%s}{%a}_dpk3 ' once done, we can delete r{%s}{%a}_dpk3
	next
next

' *** Create FE differentials
' FE differential for LFPRs is the difference between the BC effect evaluated at RU-FE and the BC effect evaluated at RU.
' BC effect evaluated at RU is computed above. We compute the rest here
' This is done only for the 'model' period

%msg = "Creating the FE differentials for all ages"
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl @all

for %s m f 
	for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054
		' BC effect evaluated at RU_FE
		series r{%s}{%a}_dpk3 = r{%s}{%a}_fe	' same process as described above for the BC effect, see notes above
		eq_p{%s}{%a}_bc.fit bc_{%s}{%a}_fe		' this creates the BC effect evaluated at RU_FE
		delete r{%s}{%a}_dpk3 						' once done, we can delete r{%s}{%a}_dpk3
		' FE differentials
		series dp{%s}{%a}_fe = bc_{%s}{%a}_fe - bc_{%s}{%a}
	next
next



'*********************************
'***** Ages 1617 and 1819*****
'*********************************
%msg = "Creating projections for ages 1617 and 1819"
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl @all

' NOTE: the equatiosn allow us to project the dependent variable, which id DI-adjusted LFPRs. 
' Here is how we Translate the projections for DI-adjusted LFPRs into "pure LFPRs"
' Assume: "trend LFPR" used in the DI-adjustment process in the projections will be equal to the projected LFPR itself.
' Thus:
' LFPR_diadj = LFPR + "trend LFPR" * DIrate
' assume: for projections "trend LFPR" = LFPR
' Therefore, 
' LFPR_diadj = LFPR + LFPR * DIrate
' LFPR = LFPR_diadj / (1+DIrate) 		
' we use this expression to transform the projected DI-adjusted LFPRs (like pm3034nm_diadj_p) into "pure" LFPRs (name it pm3034nm_p)


smpl mdl 	' period over which we run the model

for %a 1617 1819
	' males
	eq_rnlm{%a}.fit rnlm{%a}_nilf_fitted		' project values for rnlm/a/_nilf; the projected series MUST be named rnlm{%a}_nilf_fitted, b/c the LFPR equation that follows expects a series of this name.
	eq_pm{%a}.fit pm{%a}_diadj_p			' project DI-adjusted LFPRs and name them pm/a/_diadj_p
	series pm{%a}_p = pm{%a}_diadj_p / (1 + rm{%a}di) ' Convert DI-adjusted LFPRs to "pure" LFPRs
	
	'females
	eq_rnlf{%a}_h.fit rnlf{%a}_h_fitted		' project values for rnlf/a/_h  the projected series MUST be named rnlf{%a}_h_fitted
	eq_rnlf{%a}_so.fit rnlf{%a}_so_fitted	' project values for rnlf/a/_so  the projected series MUST be named rnlf{%a}_so_fitted
	eq_pf{%a}.fit pf{%a}_diadj_p				' project DI-adjusted LFPRs and name them pm/a/_diadj_p
	series pf{%a}_p = pf{%a}_diadj_p / (1 + rf{%a}di) ' Convert DI-adjusted LFPRs to "pure" LFPRs
next


'*********************************
'***** MALES 2024 to 5054 *****
'*********************************
%msg = "Creating projections for Males, ages 2024 to 5054"
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl mdl 	' period over which we run the model

for %a 2024 2529 3034 3539 4044 4549 5054
	for %m ms ma nm
		eq_pm{%a}{%m}.fit pm{%a}{%m}_diadj_p			' project DI-adjusted LFPRs and name them pm/a/ms/_diadj_p
		series pm{%a}{%m}_p = pm{%a}{%m}_diadj_p / (1 + rm{%a}di) ' Convert DI-adjusted LFPRs to "pure" LFPRs
	next
next

' combine LFPRs by MS into aggregated 5yr groups
for %a 2024 2529 3034 3539 4044 4549 5054
	series pm{%a}_p = (pm{%a}ms_p * nm{%a}ms + pm{%a}ma_p * nm{%a}ma + pm{%a}nm_p * nm{%a}nm) / nm{%a}
next

' ** Make 'final' LFPRs (series named like pm2024 instead fo pm2024_p) by
'	(1) for aggregate groups, appending pm..._p to the historical values of pm...,
'	(1.5) apply _add2 addfactors toa ll age groups
' 	(2) for pm... by marital status, scaling each series by the same scale as aggergae pm 
' This is the note from Fortran to expalin this step:
'         ! Some of Equations 149-176 are needed to rescale the
'         ! participation rates for the period starQtr to lastDataQtr
'         ! because historical data is not available by marital status 

' (1) agggregate 5yr groups -- append to historical data AND apply _add2 addfactors
wfselect {%thisfile}
pageselect q
smpl proj			' the sample starts in the quarter RIGHT AFTER the last historical quarter		
					
for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054
	pm{%a} = pm{%a}_p + pm{%a}_add2		
next

' (2) scale the LFPRs by marital status
wfselect {%thisfile}
pageselect q
smpl mdl			' period over which we run the model							
for %a 2024 2529 3034 3539 4044 4549 5054
	for %m ms ma nm
		series pm{%a}{%m} = pm{%a}{%m}_p * pm{%a} / pm{%a}_p
	next		
next


'**************************************
'***** FEMALES 2024 to 5054 *****
'**************************************
%msg = "Creating projections for Females, ages 2024 to 5054"
logmsg {%msg}
logmsg

wfselect {%thisfile}
pageselect q
smpl mdl 	' period over which we run the model

for %a 2024 2529 3034 3539 4044
	for %m ms ma nm
		eq_pf{%a}{%m}c6u.fit pf{%a}{%m}c6u_diadj_p			' project DI-adjusted LFPRs and name them pf/a/ms/c6u_diadj_p		
		series pf{%a}{%m}c6u_p = pf{%a}{%m}c6u_diadj_p / (1 + rf{%a}di) ' Convert DI-adjusted LFPRs to "pure" LFPRs
		
		eq_pf{%a}{%m}nc6.fit pf{%a}{%m}nc6_diadj_p			' same for NC6 category		
		series pf{%a}{%m}nc6_p = pf{%a}{%m}nc6_diadj_p / (1 + rf{%a}di) 
	next
next

for %a 4549 5054
	for %m ms ma nm
		eq_pf{%a}{%m}.fit pf{%a}{%m}_diadj_p			' these equations include education variable		
		series pf{%a}{%m}_p = pf{%a}{%m}_diadj_p / (1 + rf{%a}di) ' Convert DI-adjusted LFPRs to "pure" LFPRs
	next
next

' ** combine LFPRs by MS into aggregated 5yr groups
' 5yr groups by marital status
for %a 2024 2529 3034 3539 4044
	for %m ms ma nm
		series pf{%a}{%m}_p = (pf{%a}{%m}c6u_p * nf{%a}{%m}c6u + pf{%a}{%m}nc6_p * nf{%a}{%m}nc6) / nf{%a}{%m}
	next
next
' aggregate 5yr groups
for %a 2024 2529 3034 3539 4044
	series pf{%a}_p = (pf{%a}ms_p * nf{%a}ms + pf{%a}ma_p * nf{%a}ma + pf{%a}nm_p * nf{%a}nm) / nf{%a}
next

for %a 4549 5054
	series pf{%a}_p = (pf{%a}ms_p * nf{%a}ms + pf{%a}ma_p * nf{%a}ma + pf{%a}nm_p * nf{%a}nm) / nf{%a}
next

' ** Make 'final' LFPRs (series named like pf2024 instead fo pf2024_p) by
'	(1) for aggregate groups, appending pf..._p to the historical values of pf..., 
' 	(1.5) apply _add2 addfactors to all age groups
' 	(2) for pf... by marital status and by POC, scaling each series by the same scale as aggergae pf...
' 	SAME process as for males above 

' (1) agggregate 5yr groups -- append to historical data AND apply _add2 addfactor
wfselect {%thisfile}
pageselect q
smpl proj			' the sample starts in the quarter RIGHT AFTER the last historical quarter			
				
for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054
	pf{%a} = pf{%a}_p + pf{%a}_add2		
next

' (2) scale the LFPRs by marital status
wfselect {%thisfile}
pageselect q
smpl mdl			' period over which we run the model							
for %a 2024 2529 3034 3539 4044 
	for %m ms ma nm
		series pf{%a}{%m}nc6 = pf{%a}{%m}nc6_p * pf{%a} / pf{%a}_p
		series pf{%a}{%m}c6u = pf{%a}{%m}c6u_p * pf{%a} / pf{%a}_p
		series pf{%a}{%m} = pf{%a}{%m}_p * pf{%a} / pf{%a}_p
	next		
next

for %a 4549 5054
	for %m ms ma nm
		series pf{%a}{%m} = pf{%a}{%m}_p * pf{%a} / pf{%a}_p
	next		
next


'************************
' *******make summary spool ******
wfselect {%thisfile}
pageselect q
smpl @all

spool _summary
string line1 = "This file was created on " + @date + " at " + @time + " by " + %usr
string line2 = " This file shows the PROJECTED LFPRs created using the newly estimated LFPR equations for ages 1617 to 5054."
string line3 = " The projections are created using the following sources:" + @chr(13) + _
				" Estimated LFPR equations from " + %lfpr_eqs_path + @chr(13) + _
				" Estimated Business-cycle equations from " + %bc_eqs_path + @chr(13) + _
				" Historical and projected population from " + %dfile_path + @chr(13) + _
				" Historical and projected unemployment rates (and full-employment unempl. rates) from " + %ru_proj_path + @chr(13) + _
				" Historical and projected DI rates from " + %di_data_path + @chr(13) + _
				" Historical and projected Education scores from " + %ed_data_path + @chr(13) + _
				" The projections are created starting in " + %modelstart + " (the model run), but in the final results the projected LFPRs are overwritten with the observed LFPRs for the historical period (up to and including " + %datelasthist + ")."

if %add2 = "Y" then
	string line4 = "Add factors (p..._add2) are applied to the projected LFPRs. The add factor series are loaded from " + %adfile_path
endif

if %add2 = "N" then
	string line4 = "No p..._add2 aAdd factors are applied to the projected LFPRs. "
endif

				
string line5 = " The following equations include a time trend:" + @chr(13) + _
				" m1617 -- trend is phased out during 2020, and is horizontal starting in 2020Q4." + @chr(13) + _
				" m1819 -- trend is phased out during 2020, and is horizontal starting in 2020Q4" + @chr(13) + _
				" f1617 -- trend is phased out during 2020, and is horizontal starting in 2020Q4" + @chr(13) + _
				" f1819 -- trend is phased out during 2020, and is horizontal starting in 2020Q4" + @chr(13) + _
				" m2024 -- trend is stopped immediately, constant starting at the last historical value." + @chr(13) + _
				" m2529 -- trend is stopped immediately, constant starting at the last historical value." + @chr(13) + _
				" f2024 -- trend is stopped immediately, constant starting at the last historical value." + @chr(13) + _
				" f2529 -- trend is stopped immediately, constant starting at the last historical value.." + @chr(13) + _
				" NOTE: the phase out of all these trans is hard-coded. Even when the last historical data point moves, the trends will not change. This means that after 2019Q4 trends exert no influnce on ages 2024 and 2529, and after 2020Q4 they will exert not influence on ages 1617 and 1819. " 

string line6 = "This file present projections for DI-adjusted LFPRs (series named p..._diadj_p) and for ''pure'' LFPRs (series named p..._p):" + @chr(13) + _
				" The projected LFPRs (for pure LFPRs) are then appended to the historical LFPTs (loaded from TR20 D-file) -- the resulting series are name like pm3034 (without the _p suffix)" + @chr(13) + _
				" These continuous series (that combine historical LFPRs loaded from D-file and projecred LFPRs created here) are then compared to the projected LFPRs from TR20 loaded from " + %afile_path + @chr(13) + _
				" NOTE: the TR20 LFPRs loaded from the a-file are the FINAL LFPRs. Thus, for some ages (age 40 and older, I assume) they inlcude the LE adjustment. " + @chr(13) + _
				"             BUT LFPRs projected here do NOT include the LE adjustment. This makes them NOT directly comparable." + @chr(13) + _
				" 		    Therefore, on the charts for ages 4044, 4549, and 5054, I added a third line (named p..._p_tr20) that shows PRELIMINARY LFPRs from TR20 (they do not include the LE adjustment, but they also miss the scaling to historical data)"
				
string line7 = "The estimated equations project DI-adjusted LFPRs. The ''pure'' LFPrs are then obtained from the DI-adjusted ones as follows:" + @chr(13) + _
				" (1) Assume: in the projection period, the ''trend LFPR'' used in the DI-adjustment process is equal to the projected LFPR itself" + @chr(13) + _
				" (2) Recall: LFPR_diadj = LFPR + ''trend LFPR'' * DIrate" + @chr(13) + _
				" For projections, assume ''trend LFPR'' = LFPR " + @chr(13) + _
				" (3) Therefore, " + @chr(13) + _
				" LFPR_diadj = LFPR + LFPR * DIrate" + @chr(13) + _
				" LFPR = LFPR_diadj / (1 + DIrate)" + @chr(13) + _
				" I use this expression to transform the projected DI-adjusted LFPRs (like pm3034nm_diadj_p) into ''pure'' LFPRs (named pm3034nm_p) "


_summary.insert line1 line2 line3 line4 line5 line6 line7
'_summary.display

delete line*

%wfpath = %outputpath + %thisfile + ".wf1"
%msg = "Saving workfile to " + %wfpath + "  Any file with an identical filename is overwritten."
logmsg {%msg}
logmsg
wfsave(2) %wfpath ' saves the workfile

logmsg Finished lfpr_proj_1654
logmsg


