' This program allows the user to select from a list of MODEEM assumptions

exec .\setup2 ' modsel is in the current path

' Get the list of assumptions
pageselect vars

wfopen bkl
pageselect vars
copy bkl::vars\assumpt* work::vars\assumpt*
close bkl

svector descr
descr = @wsplit(assumpt2)
'Number each assumption
!n = @wcount(assumpt1)
for !i = 1 to !n
   descr(!i) = @str(!i) +") " + descr(!i)
next

scalar selnum = 1727 ' Default to most recent Alt 2

' Change selection to first program argument if not null
if (%0 <> "") then
   selnum = @val(%0)
endif

@uilist(selnum,"Please select the assumptions to use: ", @wjoin(descr))

delete descr

pageselect vars
string assumpt = ""
for !i = 1 to 10
   assumpt = assumpt + @wordq(assumpt{!i}, selnum) + " "
   delete assumpt{!i}
next


