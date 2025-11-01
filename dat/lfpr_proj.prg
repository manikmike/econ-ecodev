' This program run the EViews labor force project program
' and saves the solution to lfpr_proj.wf1

%afile = %0

wfcreate(wf=lfpr_proj, page=a) a 1901 2105
pagecreate(page=q) q 1900Q2 2105Q4
pagecreate(page=m) m 1900M2 2105M12
pagecreate(page=vars) a 1901 2105

copy {%afile}::vars\assumpt assumpt
%assumpt = assumpt
%dfile = @wordq(assumpt,4)
copy {%dfile}::vars\dfile dfile
%adfile = @wordq(assumpt,5)
copy {%adfile}::vars\add add

wfsave(2) lfpr_proj
wfclose lfpr_proj


%prgs = "ru_proj lfpr_proj_1654 lfpr_proj_55100"

for %prg {%prgs}
   call execprg(%prg, %afile)
next


subroutine execprg(string %prg, string %afile)

   exec .\{%prg}  %afile

   wfselect {%prg} ' The workfile and the program have the same name
   pageselect q
   smpl @all

   group g * not resid
   %solution = g.@members
   delete g
   
   wfopen lfpr_proj
   
   for %s {%solution}
      copy {%prg}::q\{%s} lfpr_proj::q\{%s}
   next

   wfsave(2) lfpr_proj
   wfclose lfpr_proj

   wfclose {%prg}

endsub


