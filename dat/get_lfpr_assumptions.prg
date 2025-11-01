' This program derives the assumptions at runtime from the AFILE name
' when it is "included" in other programs where the %afile is already defined

wfselect {%afile}
pageselect vars
%assumpt = assumpt ' (e.g., "TR252 ""TR 2025 Alt II (TR252)"" E14A DTR252 ADTR252 ATR252 A 205Q4 S N")

'data for projections
' a-file -- for rtp (historical and projected)
%afile_path = @datapath + "\" + %afile + ".wf1"

' d-file -- for RUs (historical)
%dfile = @wordq(%assumpt,4)
%dfile_path = @datapath + "\" + + %dfile + ".wf1"
wfselect {%dfile}
pageselect vars
%dfilelist = dfile ' (e.g., "DTR252 CPSO68124 OP1252O OP1242O OP1242O EDMIL 205 205")

' If %asa_adj_val = "Y" below, provide the SOURCE for the adddactor
%adfile = @wordq(%assumpt,5) ' filename
%adfile_path = @datapath + "\" + %adfile + ".wf1"	' full patfh
wfselect {%adfile}
pageselect vars
%addlist = add  ' (e.g., "ADTR252 E14A DTR252 6 109Q1")



' ******** UPDATE inputs here *******
%usr = @env("username") 	' person running the program

%TR = @mid(@wordq(%assumpt,1),3,2) ' (e.g., "25")
!TRyr = @val("20" + %TR) ' (e.g., 2025)

%datestart = "1900Q2"				' date with quarter
%yrstart = "1901" 						'  same with year only

%datelasthist = @str(!TRyr - 1) + "Q3" ' (e.g., "2024Q3")				' last historical date with quarter
%yrlasthist = @str(!TRyr - 2) ' (e.g., "2023)"					' Last year with FULL year of historical data


%addlist5 = @wordq(%addlist,5) ' (e.g., "109Q1")
%modelstart = @str(1900 + @val(@left(%addlist5,3))) + @right(%addlist5,2) ' (e.g., "2009Q1")				'  the first quarter for which we run the model. In the "old model" this was 2007Q1.
											' NOTE: this does NOT have to be the quarter immediately following after %datelasthist. This cane ba ANY quarter in the hsitorical period or the quarter right after the historical period. 
											' To compare with TR20, I start this in 2019Q4. 
%modelstartyr = @left(%modelstart, 4)	' (e.g, "2009") same with year only
								
%assumpt8 = @wordq(%assumpt,8) ' (e.g., "205Q4")
%projend = @str(1900 + @val(@left(%assumpt8,3))) + @right(%assumpt8,2) ' (e.g., "2105Q4")					' last quarter of projections
%projendyr = @left(%projend, 4)	' (e.g., "2105") same with year only


