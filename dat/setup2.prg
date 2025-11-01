'Create a default workfile named work like Aremos did with its work databank

'Prevent work.wf1 from being created more than once
'setmaxerrs 2
!ec = @errorcount
setmaxerrs (!ec + 2)
logmode -error
wfselect work
%error = @lasterrstr
!test = @instr(%error,"WFSELECT WORK")
logmode +error
if (!test <> 0) then
   wfcreate(wf=work, page=a) a 1901 2105
   pagecreate(page=q) q 1900Q2 2105Q4
   pagecreate(page=m) m 1900M2 2105M12
   pagecreate(page=vars) a 1901 2105
   smpl @all
endif
'setmaxerrs 1
!ec = @errorcount
setmaxerrs (!ec + 1)


