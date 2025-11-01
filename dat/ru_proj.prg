' This program projects RUs by age and sex using the RU equations estimated elsewhere

%afile = %0

include get_lfpr_assumptions.prg

'estimated equations
%ru_eqs = "ru_eqns" 	'short name of the workfile contaning estimated equations for RUs (they will be dru's, i.e. first differences)
%ru_eqs_path = @datapath + "\" + %ru_eqs + ".wf1"	  	' full path to the file





' *** Option for the ru_asa_adj addfactor
' *** Do we have values for ru_ada_adj addfactor?
' If so, enter "Y" below AND provide the path to the source file containing values for ru_asa_adj.
' If not, enter "N" below
%asa_adj_val = "Y" 		' enter "Y" or "N", case sensitive



' output created by this program
%thisfile = "ru_proj" 	' name of the file 
%output_path = @datapath + "\" + %thisfile + ".wf1"

' ******* END of update section

   wfcreate(wf={%thisfile}, page=a) a 1901 2105
   pagecreate(page=q) q 1900Q2 2105Q4
   pagecreate(page=m) m 1900M2 2105M12
   pagecreate(page=vars) a 1901 2105

' define useful samples
' quarterly
wfselect {%thisfile}
pageselect q
smpl @all

sample hist @first {%datelasthist}		' sample called 'hist' covers historical period
sample est 1994 {%datelasthist}			' sample called 'est' was used for estimating the equations; we start it in 1994
sample mdl {%modelstart} @last		' sample called 'mdl' (for MODEL) covers the period for which we run the model

%p1 = @otod(@dtoo(%datelasthist) +1) 	' quarter RIGHT AFTER the last historical quarter
sample proj {%p1} @last						' sample 'proj' for "projections" starts in the quarter RIGHT AFTER the last historical data


logmode l			' this progtram will display log messages
%msg = "Running ru_proj.prg" 
logmsg {%msg}
logmsg 


'*** 1. Copy in the estimated dRU equations.
'*** We will need the estimated coefficients from these.
%msg = "Loading RU equations from " + %ru_eqs_path
logmsg {%msg}
logmsg 

wfselect {%thisfile}
pageselect q
smpl @all

wfopen {%ru_eqs_path}
for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
   for %s m f
      copy {%ru_eqs}::q\eq_dr{%s}{%a}_2019ncf {%thisfile}::q\eq_dr{%s}{%a} 		' NOTE the name of the equations we are copying!!! 
      																								' This may be changes if we decide to use a sdifferent specification of equations.
      																								' When equations are copied, I rename them to eq_dr{%s}{%a}, so that the rest of the program will remain unchanged even if we later decide to load different equatiosn here.
   next
next
wfclose {%ru_eqs}


' *** 2. Load historical data
%msg = "Loading historical data"
logmsg {%msg}
logmsg 

wfselect {%thisfile}
pageselect q
smpl @all

' dbopen(type=aremos) {%dfile_path} 
' RUs
for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o 16o
   for %s m f
      copy {%dfile}::q\r{%s}{%a} r{%s}{%a}
   next
next
copy {%dfile}::q\r16o r16o

' BASE population and labor force series -- need these for computing ASA values
for %s m f 
	for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
      copy {%dfile}::q\n{%s}{%a}_by n{%s}{%a}_by
      copy {%dfile}::q\l{%s}{%a}_by l{%s}{%a}_by
	next
next

' Actual population and labor force series -- to compute 16o series
for %s m f 
	for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o 16o
		' fetch {%dfile}::n{%s}{%a}.q
      copy {%dfile}::q\n{%s}{%a} n{%s}{%a}
		' fetch {%dfile}::l{%s}{%a}.q
      copy {%dfile}::q\l{%s}{%a} l{%s}{%a}
	next
next
copy {%dfile}::q\n16o n16o
copy {%dfile}::q\l16o l16o

' close {%dfile}


' *** 3. Load exogenous variable -- RTP -- both historical and projected
%msg = "Loading RTP, historical and projected, from " + %afile_path
logmsg {%msg}
logmsg 

wfselect {%thisfile}
pageselect q
smpl @all

' dbopen(type=aremos) {%afile_path} 

' fetch {%afile}::rtp.q 
copy  {%afile}::q\rtp

' close {%afile}


'****************************************************************
'********* INITIAL RUN, assuming all addfactors = 0 ********
'****************************************************************
' will be denotes by '.1' after each step number, like 4.1, 5.1 etc.

%msg = "Starting the initial run -- all addafactors assumed to be zero"
logmsg {%msg}
logmsg 

'**** 4.1 Initialize all addfactors to zero
wfselect {%thisfile}
pageselect q
smpl @all

for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
  	for %s m f
     	 	series r{%s}{%a}_p_add = 0
     	next
next

series ru_asa_adj = 0



'**** 5.1 Create Projected RU values, preliminary --  r.._p***
%msg = "Creating Projected RU values, preliminary"
logmsg {%msg}
logmsg 

' transform addfactors to account for the way they are used in EViews vs Fortran
' In Fortran the addfactor are applied period by period, recursively. 
' Example: if rm1617_p_add = - 0.02, then it will be epplied as rm1617_p in 2020Q1 = rm1617_p(in 2019Q4) -0.02; then rm1617_p(in 2020Q2) - 0.02 , etc.
' In Eviews they will be applied as a complete series such as rm1617_p = rm1617_p - rm1617_p_add(entire series).
' Thefore, for EViews the addfactor series should be cumulated.
wfselect {%thisfile}
pageselect q
smpl @all
for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
  	for %s m f
     	 	series r{%s}{%a}_p_addc = @cumsum(r{%s}{%a}_p_add)		' cimulative addfactors; NOTE: at this point the addfactors are zero
     	next
next

' get predicted values from estimated equations
wfselect {%thisfile}
pageselect q
smpl proj			' This should be done over the "projection" sample, i.e. starting in the quarter right after the last historical

for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
  	for %s m f
     	 	eq_dr{%s}{%a}.forecast(f=actual) r{%s}{%a}_p		'create predicted values, name them r[sex][age]_p, such as rm2024_p, and append them to the historical values in a single series
     	 	r{%s}{%a}_p = r{%s}{%a}_p + r{%s}{%a}_p_addc	' apply the cumulativeaddfactors; NOTE: at this point the addfactors are zero
     	next
next

' Compute rum_asa_p ruf_asa_p ru_asa_p
wfselect {%thisfile}
pageselect q
smpl mdl			' 'model' sample

for %s f m 
	series l{%s}16o_by = l{%s}1617_by + l{%s}1819_by + l{%s}2024_by + l{%s}2529_by + l{%s}3034_by + l{%s}3539_by + l{%s}4044_by + l{%s}4549_by + l{%s}5054_by + l{%s}5559_by + l{%s}6064_by + l{%s}6569_by + l{%s}7074_by + l{%s}75o_by
next
series l16o_by = lm16o_by + lf16o_by

for %s f m
	series ru{%s}_asa_p =    (r{%s}1617_p * l{%s}1617_by + _
									r{%s}1819_p * l{%s}1819_by + _
									r{%s}2024_p * l{%s}2024_by + _
									r{%s}2529_p * l{%s}2529_by + _
									r{%s}3034_p * l{%s}3034_by + _
									r{%s}3539_p * l{%s}3539_by + _
									r{%s}4044_p * l{%s}4044_by + _
									r{%s}4549_p * l{%s}4549_by + _
									r{%s}5054_p * l{%s}5054_by + _
									r{%s}5559_p * l{%s}5559_by + _
									r{%s}6064_p * l{%s}6064_by + _
									r{%s}6569_p * l{%s}6569_by + _
									r{%s}7074_p * l{%s}7074_by + _
									r{%s}75o_p * l{%s}75o_by ) / (l{%s}16o_by)
next

series ru_asa_p = (ruf_asa_p * lf16o_by + rum_asa_p * lm16o_by) / l16o_by



'**** 6.1 Compute FE differentials ***
%msg = "Computing FE differential (dru_fe series) "
logmsg {%msg}
logmsg 

wfselect {%thisfile}
pageselect q
smpl mdl

' use estimated RU equations for this.
' We will create predicted values for the FIRST DIFFERENCE, not the level of ru
' NOTE: the estimated equations require a series named EXACTLY 'rtp' to be used as independent variable.
' For FE differentials, we want to use (1-rtp). 
' Need to redefine RTP series so that it becoems (1-rtp)

smpl @all
series rtp_orig = rtp 		' save the original rtp series so that we can restore it later
series rtp1 = 1 - rtp 		' this is deviation of rtp from 1 each period -- the values we need to use in creating dru_fe's
' we now need to create a series such that first difference of it will be equal to rtp1 -- because the RU equations use first differences of rtp as indepenedent variales; and we must name that series rtp
rtp = @cumsum(rtp1) 		' this is 'new rtp'; we must have a series named exactly 'rtp' to obtain the predicted values from the estimated dru equations. 
' check that thes alternative rtp is exactly what we need; the check below should be zero
series rtp1_ck = rtp1 - d(rtp)

' now make the dru_fe series
for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
  	for %s m f
     	 	eq_dr{%s}{%a}.forecast(d, f=na) dr{%s}{%a}_fe			' create predicted values for dru's, note the option (d) in parenthesis; and do NOT appent to any historical values
     	next
next

' restore rtp to the original series
rtp = rtp_orig
delete rtp_orig

%msg = "The INITIAL run is complete. "
logmsg {%msg}
logmsg 



' *****************************************************************
' ******* 7.1 Loading the addfactors ru_p_add *********************
' *****************************************************************
%msg = "Loading addfactors ru_p_add "
logmsg {%msg}
logmsg 

wfselect {%thisfile}
pageselect q
for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
	for %s m f 
		copy {%adfile}::add_q\r{%s}{%a}_p r{%s}{%a}_p_add 
	next
next

%msg = "All ru_p_add addfactors have been loaded."
logmsg {%msg}
logmsg 


'***********************************************************************************
'********* SECOND RUN, uses ru_p_add addfactors computed above ********
'***********************************************************************************
' will be denotes by '.2' after each step number, like 4.2, 5.2 etc.

%msg = "Starting the second run -- ru_p_add addfactors no longer zero"
logmsg {%msg}
logmsg 

'**** 4.2  -- not needed, no need to initialize addfactors
' Note that ru_asa_adj = 0 still

'**** 5.2 Create Projected RU values, preliminary --  r.._p***
%msg = "Second run -- Creating Projected RU values, preliminary"
logmsg {%msg}
logmsg 

' transform addfactors to account for the way they are used in EViews vs Fortran
' In Fortran the addfactor are applied period by period, recursively. 
' Example: if rm1617_p_add = - 0.02, then it will be epplied as rm1617_p in 2020Q1 = rm1617_p(in 2019Q4) -0.02; then rm1617_p(in 2020Q2) - 0.02 , etc.
' In Eviews they will be applied as a complete series such as rm1617_p = rm1617_p - rm1617_p_add(entire series).
' Thefore, for EViews the addfactor series should be cumulated.
wfselect {%thisfile}
pageselect q
smpl @all
for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
  	for %s m f
     	 	series r{%s}{%a}_p_addc = @cumsum(r{%s}{%a}_p_add)		' cimulative addfactors; NOTE: at this point the addfactors are NOT zero
     	next
next

' get predicted values from estimated equations
wfselect {%thisfile}
pageselect q
smpl proj			' This should be done over the "projection" sample, i.e. starting in the quarter right after the last historical

for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
  	for %s m f
     	 	eq_dr{%s}{%a}.forecast(f=actual) r{%s}{%a}_p		'create predicted values, name them r[sex][age]_p, such as rm2024_p, and append them to the historical values in a single series
     	 	r{%s}{%a}_p = r{%s}{%a}_p + r{%s}{%a}_p_addc	' apply the cumulative addfactors; NOTE: at this point the addfactors are NOT zero
     	next
next

' Compute rum_asa_p ruf_asa_p ru_asa_p

%msg = "Second run -- Computing ASA values, preliminary"
logmsg {%msg}
logmsg  

wfselect {%thisfile}
pageselect q
smpl mdl			' 'model' sample

for %s f m 
	series l{%s}16o_by = l{%s}1617_by + l{%s}1819_by + l{%s}2024_by + l{%s}2529_by + l{%s}3034_by + l{%s}3539_by + l{%s}4044_by + l{%s}4549_by + l{%s}5054_by + l{%s}5559_by + l{%s}6064_by + l{%s}6569_by + l{%s}7074_by + l{%s}75o_by
next
series l16o_by = lm16o_by + lf16o_by

for %s f m
	series ru{%s}_asa_p =    (r{%s}1617_p * l{%s}1617_by + _
									r{%s}1819_p * l{%s}1819_by + _
									r{%s}2024_p * l{%s}2024_by + _
									r{%s}2529_p * l{%s}2529_by + _
									r{%s}3034_p * l{%s}3034_by + _
									r{%s}3539_p * l{%s}3539_by + _
									r{%s}4044_p * l{%s}4044_by + _
									r{%s}4549_p * l{%s}4549_by + _
									r{%s}5054_p * l{%s}5054_by + _
									r{%s}5559_p * l{%s}5559_by + _
									r{%s}6064_p * l{%s}6064_by + _
									r{%s}6569_p * l{%s}6569_by + _
									r{%s}7074_p * l{%s}7074_by + _
									r{%s}75o_p * l{%s}75o_by ) / (l{%s}16o_by)
next

series ru_asa_p = (ruf_asa_p * lf16o_by + rum_asa_p * lm16o_by) / l16o_by

'**** 6.2 Compute FE differentials ***
' These are unchanged from step 6.1.


' **************************************************************
' **** 7.2 Load RU_ASA_ADJ aaddfactor, if available *******
' *************************************************************
' ru_asa_adj addfactors is created by professional judgement, based on the preliminary RUs computed above.
' for early/preliminary runs of the program, we may not have value sof ru_asa_adj available yet. In such cases it will be set to zero here
wfselect {%thisfile}
pageselect q
smpl @all
	
if %asa_adj_val = "Y" then
	' Load ru_asa_adj addfactor
	%msg = "Loading ru_asa_adj addfactor from " + %adfile_path
	logmsg {%msg}
	logmsg  
	
	' dbopen(type=aremos) {%adfile_path} 
	' fetch {%adfile}::ru_asa_adj.q 
	copy {%adfile}::q\ru_asa_adj ru_asa_adj 
	' close {%adfile}
endif

if %asa_adj_val = "N" then
	' Set ru_asa_adj addfactor to zero
	%msg = "NOTE: ru_asa_adj addfactor is set to zero" 
	logmsg {%msg}
	logmsg  
	
	series ru_asa_adj = 0
endif



' ***** 8. Create Projected RU values, final (no longer preliminary) ***
' This is done by applyign the ru_asa_adj addfactor

%msg = "Second Run -- Creating Projected RU values, Final (not preliminary)"
logmsg {%msg}
logmsg 

wfselect {%thisfile}
pageselect q
smpl proj 		' 'projection' sample

for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
  	for %s m f
     	 	r{%s}{%a} = r{%s}{%a}_p * (1+ ru_asa_adj/ru_asa_p)
     	next
next

'**** 9 Compute ASA values, final (i.e. not preliminary) ***
%msg = "Second Run -- Creating ASA values, final"
logmsg {%msg}
logmsg 

wfselect {%thisfile}
pageselect q
smpl mdl		' 'model' sample

for %s f m
	series ru{%s}_asa =    (r{%s}1617 * l{%s}1617_by + _
									r{%s}1819 * l{%s}1819_by + _
									r{%s}2024 * l{%s}2024_by + _
									r{%s}2529 * l{%s}2529_by + _
									r{%s}3034 * l{%s}3034_by + _
									r{%s}3539 * l{%s}3539_by + _
									r{%s}4044 * l{%s}4044_by + _
									r{%s}4549 * l{%s}4549_by + _
									r{%s}5054 * l{%s}5054_by + _
									r{%s}5559 * l{%s}5559_by + _
									r{%s}6064 * l{%s}6064_by + _
									r{%s}6569 * l{%s}6569_by + _
									r{%s}7074 * l{%s}7074_by + _
									r{%s}75o * l{%s}75o_by ) / (l{%s}16o_by)
next

series ru_asa = (ruf_asa * lf16o_by + rum_asa * lm16o_by) / l16o_by


'**** 10 Compute RU_FE's, final ***
%msg = "Compute ru_fe series, final"
logmsg {%msg}
logmsg 

wfselect {%thisfile}
pageselect q
smpl mdl

for %a 1617 1819 2024 2529 3034 3539 4044 4549 5054 5559 6064 6569 7074 75o
  	for %s m f
     	 	series r{%s}{%a}_fe = r{%s}{%a} + dr{%s}{%a}_fe
     	next
next

%msg = "DONE"
logmsg {%msg}
logmsg 

' ******************************************
'		make summary spool
wfselect {%thisfile}
pageselect q
smpl mdl

spool _summary
string line1 = "This file was created on " + @date + " at " + @time + " by " + %usr
string line2 = "The file contains the projected values fo RUs and RU_FEs by age and sex. "
string line3 = "The projections use the estimated coefficients from  equations that link RUs to RTP from" + %ru_eqs_path + @chr(13) + _
				" ru_p_add addfactors loaded from " + %adfile_path
if %asa_adj_val = "Y" then
	string line4 =	" ru_asa_adj addfactor loaded from " + %adfile_path
endif

if %asa_adj_val = "N" then
	string line4 =	" ru_asa_adj addfactor is set to zero."
endif			

_summary.insert line1 line2 line3 line4

delete line*

wfsave(2) %output_path ' saves the workfile

logmsg Finished ru_proj
logmsg


