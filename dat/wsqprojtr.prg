' This program creates HI Taxable Target using the new method developed in 2023, to be used for TR24 for the first time
' This program replaces wsQprojtr.prg that was used previously;
' I am keeping the name -- wsqprojtr.prg -- unchanghed to make it easy to integrate into the Econ model, but the program is entirely different.

' This program assumes the following files exist and are accessible in the default directory:
' wsqprojtr.wf1 that contains updated data and the relevant QCEW equations; this file is normally updated by running wsqdata.prg

' To run the program
' 		Make sure to include two input parameters, corresponding to WSD values for Q3 and Q4 for the target year.
'		For example:
		' exec .\wsqprojtr.prg 11245.906 11354.419

shell attrib -r wsqprojtr.wf1

wfopen wsqprojtr.wf1 
pageselect Annual
smpl @all

%datel = datel ' Year for which Taxable Targets are to be created 
%q2fin = q2fin ' value indicates whether we use PRELIM QCEW data for Q2 of the year (N) or FINAL one (Y)

logmode l
%msg = "Running wsQprojtr.prg" 
logmsg {%msg}
logmsg


' **** UPADATE entries in this section

' Do you want to save the file on this run? "Y" will OVERWRITE the existing wsqprojtr.wf1 file; "N" gives you an option to save it manually (possibly with a different name)
%sav = "Y" ' enter "Y" or "N" case sensitive (typically, use Y for runstream, N for testing/development)

'***** END of update section


wfselect wsqprojtr
pageselect Annual
pagestruct(end={%datel})

' *** Forecast values using the equations***
' This section estmates the HI taxable target
' It can be run at the same time as the data update step (set !dataload = 1 and !estrun = 1) or alone assuming that data update has already been done (set !dataload = 0 and !estrun = 1)
' During Econ TR run, ONLY this section is run; data load MUST be done separately from runstream.

	%msg = "Estimating the values for " + %datel
	logmsg {%msg}
	logmsg
	
	' Getting the values for WSD Q3 and Q4 that were passed to the program automatically
	wfselect wsqprojtr
	pageselect Annual
	smpl {%datel} {%datel} 	
	wsdnaq3 = @val(%0)	   ' series with these names aleady exists in the file, so should use same names  
	wsdnaq4 = @val(%1)
	
	' create series needed for forecasting 
	wfselect wsqprojtr
	pageselect Annual
	smpl @all
	
	' special difference series for qtr2
	series dqtr2 = qtr2 - qtr2f(-1)
	dqtr2.label(d) Difference between QCEW Q2 pre-release PRELIMINARY and QCEW Q2 pre-release FINAL in prior year
	dqtr2.label(u) $billions per period (NOT SAAR)
	
	' average of WSD Q3 and Q4
	smpl {%datel} {%datel}
	wsdq3 = wsdnaq3
	wsdq4 = wsdnaq4
	
	smpl @all
	series wsdq34 = (wsdq3 + wsdq4)/2
	wsdq34.label(d) NIPA wages (wsd) average of Q3 and Q4 of each year, SAAR, $billion
	
	
	' NOW do the actual forecasting
	wfselect wsqprojtr
	pageselect Annual
	smpl @all
	
	' round historical values of WSCAHI to the nearest $ million
	' since WSCAHI is currently saved in $ billion, this means rounding to 3 decimals
	wscahi = @round(wscahi,3)
	
	%fsample = %datel + " " + %datel 		' forecast sample, meaning, the latest year
	
	' depending on whether we have PRELIM of FINAL qtr2 values, we use different equations
	if %q2fin = "N" then
		eq1.forecast(e, g, forcsmpl=%fsample) qcew_an_f
	endif
	
	if %q2fin = "Y" then
		eq1_fin.forecast(e, g, forcsmpl=%fsample) qcew_an_f
	endif
	
	' Series qcew_an_f contains QCEW annual wages 
	' FORECASTED value for year=datel and 
	' HISTORICAL values for all prior years
	
	' Create the forecasted value for WSCAHI
	' by applying the gr rate of QCEW annual wages to WSCAHI value from prior year
	wfselect wsqprojtr
	pageselect Annual
	smpl {%datel} {%datel}
	
	wscahi = wscahi(-1) * (qcew_an_f / qcew_an_f(-1))
	
	if %q2fin = "N" then
		%note = "Value for " + %datel + " is forecast using equation eq1."
		else 
			%note = "Value for " + %datel + " is forecast using equation eq1_fin."
	endif
	wscahi.label(r) {%note}
	
	%msg = "WSCAHI " + %note
	logmsg {%msg}
	logmsg
	
	smpl {%datel} {%datel}
	wshiult = @round(wscahi / 1.005, 3) ' adjustment to synchronize level of covered wages with NIPA wages
										        ' assign the value to WSHIULT -- this is the name we use for the HI taxable target
                                       ' and round it to the nearest $ million (to 3 decimals, since wscahi is stored in $ billions)

	wfselect wsqprojtr
	pageselect Annual
	smpl {%datel} {%datel}
	
	group _g wshiult wsult941
	
	' Copy the relevant elements into page 'a' ; makes it easier for Fortran to get it
	for %ser wshiult _g
		copy(o) Annual\{%ser} a\*
	next
	
	wfselect wsqprojtr
	pageselect a
	smpl {%datel} {%datel}
	
	'_g.display
	
	%msg = "Estimation complete. See group _g for resulting taxable target values."
	logmsg {%msg}
	logmsg


'create a spool with summary info for the run
wfselect wsqprojtr
pageselect Annual
smpl {%datel} {%datel}

delete _summary 	' delete the prior spool

%usr = @env("USERNAME")
spool _summary
string line1 = "This file was created on " + @date + " at " + @time + " by " + %usr + " by running wsqprojtr.prg"
string line2
if %q2fin = "N" then
	line2 = "Value for WSCAHI in " + %datel + " is forecast using equation eq1."
else 
	line2 = "Value for WSCAHI in " + %datel + " is forecast using equation eq1_fin."
endif
string line3 = "Group _g in page 'a' contains the estimated taxable targets. " + @chr(13) + _
						" The taxable targets were estimated using the following values for WSD: " + @chr(13) + _
						%0 + " for " + %datel + "Q3" + @chr(13) + _
						%1 + " for " + %datel + "Q4"

_summary.insert line1 line2 line3

delete line* 

' save the file

if %sav = "N" then
	string sav = "The modified workfile has NOT been saved. If you intend to keep this version, please save the workfile manually. "
	sav.display
	logmsg DONE
	logmsg
endif

if %sav = "Y" then 
	wfsave(2) wsqprojtr
	logmsg DONE
	logmsg
	wfclose wsqprojtr
endif


