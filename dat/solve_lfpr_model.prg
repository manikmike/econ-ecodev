' This program opens the appropriate workfiles
' for a selected model solution and solves the EViews version
' of the labor force model in the same initial state
' as if it had been called by Fortran

exec .\modsel 1727 ' default is the 2025 Trustees Report Alt II

pageselect vars
%afile = @wordq(assumpt, 6)
%dfile = @wordq(assumpt, 4)
%otlfile = "otl_" + @right(%dfile, @len(%dfile)-1)
%adfile = @wordq(assumpt, 5)

wfopen {%dfile}.wf1
copy {%dfile}::vars\dfile work::vars\dfile
wfclose {%dfile}

wfopen {%adfile}.wf1
copy {%adfile}::vars\add work::vars\add
wfclose {%adfile}

%ofile1 = @wordq(dfile,3)
%ofile2 = @wordq(dfile,4)
%ofile3 = @left(%ofile1, @len(%ofile1)-1) + "r"
%ofile4 = "decp" + @left(%ofile1, @len(%ofile1)-1)
%mef = "mef"
%esf = "esf"
%bkdo1 = "bkdo1"
%bkdr1 = "bkdr1"
%cpso_nilf = "cpso_nilf"
%ce3750 = "ce3750"
%ssaprogdata = "ssaprogdata"
%cpsfile = @wordq(dfile,2)
%cnipopdata = "cnipopdata"

for %wf {%afile} {%dfile} {%otlfile} {%adfile} _
        {%ofile1} {%ofile2} {%ofile3} {%ofile4} _
        {%mef} {%esf} {%bkdo1} {%bkdr1} _
        {%ce3750} {%cpsfile} {%cnipopdata} {%cpso_nilf} _
        {%ssaprogdata}
   wfopen {%wf}
next

wfselect work
pageselect vars

exec .\lfpr_proj {%afile}

close @wf

for %wf lfpr_proj
   wfopen {%wf}
next
pageselect q


