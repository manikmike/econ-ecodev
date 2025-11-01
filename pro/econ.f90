program econ

   use EconMod
   include "OcactFortLib.inc"
   implicit none
   
   character (len=128) :: runNumStr
   integer :: runNum
   integer :: numargs
   
   numargs = iargc()
   if (numargs > 0) then
      call getarg(1, runNumStr)
      runNum = AscToInt(trim(runNumStr))
      call EconMain(runNum)
   else
      call EconMain()
   end if

end program econ