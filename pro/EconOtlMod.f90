module EconOtlMod

   use EconParMod
   use EconLibMod
   use EconModSolAVarMod ! uses EconModSol2VarMod and EconOtlVarMod
   use EconOtl1Mod
   use EconOtl1OutMod
   use EconOtl2Mod
   use EconOtl2OutMod
   use EconOtl3Mod
   use EconOtl3OutMod
   use ifport
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconOtlInit, EconOtl1Main, EconOtl2Main, EconOtl3Main
   
contains

!===============================================================================

   subroutine EconOtlInit()

      write(*,'(//a//)') "Initializing Otl, Please Wait..."

      call InitializeOtlVars()

   end subroutine EconOtlInit

!===============================================================================

end module EconOtlMod