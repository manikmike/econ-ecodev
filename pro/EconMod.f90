module EconMod

   use EconParMod
   use EconLibMod
   use EconModSol1Mod
   use EconModSol2Mod
   use EconModSolAMod
   use EconOtlMod
   use EconRevEarnMod
   use EconModTabMod
   use EconTrusteesReportTablesMod
   use EconModSol1VarMod ! for workfile variable names
   use ifwbase
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconMain
   
contains

!===============================================================================

   subroutine EconMain(assumptionNumber)
   
      integer, optional :: assumptionNumber
      integer :: i
      
      call ProfileInit()
      
      if (present(assumptionNumber)) then
         selnum = assumptionNumber
      end if

      call ProfileCheckpoint(1)
      call init()

      call ProfileCheckpoint(2)
      call EconModSol1Main()
      call ProfileCheckpoint(3)
      call EconModSol2Main()
      call ProfileCheckpoint(4)
      call EconOtlInit() ! Use values from an exisitng OTL file to initialize
      call ProfileCheckpoint(5)
      call EconOtl1Main()
      call EconModSolAMain()
      call EconOtl3Main()
      call EconOtl2Main()
      call EconOtl3Main()
      call EconModSolAMain()
      call EconOtl3Main()
      call EconOtl2Main()
      call EconOtl3Main()
      call EconModSolAMain()
      
      do i = 1, 5 ! Additional iterations for aiw_a and acwa_a convergence
         call EconOtl3Main()
         call EconOtl2Main()
         call EconOtl3Main()
         call EconModSolAMain()
      end do
           
      call ProfileCheckpoint(9)
      call EconRevEarnMain()
      call ProfileCheckpoint(10)
      call EconModTabMain()
      call EconTrusteesReportTablesMain()

      call ProfileCheckpoint(11)
      call wrapUp()
      call ProfileCheckpoint(12)


   end subroutine EconMain
   
!===============================================================================

  subroutine init()

     integer :: rv, len
     character (len=256) :: histExt
     character (len=256) :: ext
     character (len=128) :: selname   ! Name of model selected to solve

     call ProfileNameCheckpointTransition(1,  2,  "Initialization")
     call ProfileNameCheckpointTransition(2,  3,  "ModSol1")
     call ProfileNameCheckpointTransition(3,  4,  "ModSol2")
     call ProfileNameCheckpointTransition(4,  5,  "OtlInit")
     call ProfileNameCheckpointTransition(5,  6,  "Otl1")
     call ProfileNameCheckpointTransition(6,  7,  "ModSolA")
     call ProfileNameCheckpointTransition(7,  8,  "Otl3")
     call ProfileNameCheckpointTransition(8,  7,  "Otl2")
     call ProfileNameCheckpointTransition(7,  6,  "Otl3")
     call ProfileNameCheckpointTransition(6,  9,  "ModSolA")
     call ProfileNameCheckpointTransition(9,  10, "RevEarn")
     call ProfileNameCheckpointTransition(10, 11, "Miscellaneous")
     call ProfileNameCheckpointTransition(11, 12, "Wrapup")
     
     rv = getcwd(PRO_PATH)
     len = len_trim(PRO_PATH)
     OUT_PATH = trim(PRO_PATH(1:len-3))//"out"
     DAT_PATH = trim(PRO_PATH(1:len-3))//"dat"

     if (FORCE_ALT2) then
        selnum = ALT2_SELNUM
     else
        if (selnum == 0) call ModSel(selname, 2, selection=selnum)
     end if

     call GetAssumptions(assumpt, selnum)

     call FilesModBeginVerbose(20, trim(OUT_PATH) // "\log\econFilesUsed" // &
        trim(IntToAsc(selnum)) // ".txt")
     call FilesModSetMacro("OUT_PATH", trim(OUT_PATH))
     call FilesModSetMacro("DAT_PATH", trim(DAT_PATH))

     call GetParameters(selnum)

     ext = trim(FILE_EXTENSION)
     if (toLower(trim(ext)) == "alt2") then
        histExt = "hist"
        needHist = .true.
        needModData = .true.
     else if(toLower(trim(ext)) == "alt1" .or. &
        toLower(trim(ext)) == "alt3")  then
        histExt = "hist"
        needHist = .false.
        needModData = .true.
     else if(toLower(trim(ext)) == "alt4t" .or. &
        toLower(trim(ext)) == "alt4" .or. &
        toLower(trim(ext)) == "alt5t" .or. &
        toLower(trim(ext)) == "alt5")  then
        histExt = "hist"
        needHist = .true.
        needModData = .true.
        isBudgetRun = .true.
     else if (toLower(trim(ext)) == "nodacadapa") then
        histExt = trim("hist." // trim(ext))
        needHist = .true.
        needModData = .true.
     else if (toLower(trim(ext)) == "no_daca") then
        histExt = trim("hist." // trim(ext))
        needHist = .true.
        needModData = .true.
     else if (toLower(trim(ext)) == "ea2014") then
        histExt = trim("hist." // trim(ext))
        needHist = .true.
        needModData = .true.
     else if (toLower(trim(ext)) == "notaxlaw" .or. &
              toLower(trim(ext)) == "notaxlawdaca" .or. &
              toLower(trim(ext)) == "taxlaw" .or. &
              toLower(trim(ext)) == "taxlawdaca") then
        histExt = trim("hist." // trim(ext))
        needHist = .true.
        needModData = .true.
     else
        histExt = trim("hist." // trim(ext))
        needHist = .true.
        needModData = .false.
     end if
      
     call FilesModSetMacro("EXT",trim(ext))
     call FilesModSetMacro("TRYR",getcyr(TRYEAR,2))
      
     if (needHist) then
        ! remove macro tag only
        call FilesModSetMacro("HIST", "")
        call FilesModSetMacro("HISTEXT",trim(histExt))
     else
        ! remove extension macro and then entire line
        call FilesModSetMacro("HISTEXT","")
        call FilesModSetMacro("HIST", "NULL")
     end if

     if (needModData) then
        ! remove macro tag only
        call FilesModSetMacro("MODDATA", "")
        if (trim(ext) == "alt4t") then
           call FilesModSetMacro("ALT","msrbr")
        else if (trim(ext) == "alt4") then
           call FilesModSetMacro("ALT","msr")
        else if (trim(ext) == "alt5t") then
           call FilesModSetMacro("ALT",trim(getcyr(TRYEAR+2,2))//"br")
        else if (trim(ext) == "alt5") then
           call FilesModSetMacro("ALT",trim(getcyr(TRYEAR+2,2))//"b")
        else if (trim(ext) == "NoDACADAPA") then
           call FilesModSetMacro("ALT","nodd")
        else if (toLower(trim(ext)) == "ea2014") then
           call FilesModSetMacro("ALT","ea14")
        else if (toLower(trim(ext)) == "no_daca") then
           call FilesModSetMacro("ALT","nd12")
        else if (toLower(trim(ext)) == "notaxlaw") then
           call FilesModSetMacro("ALT","t0d0")
        else if (toLower(trim(ext)) == "notaxlawdaca") then
           call FilesModSetMacro("ALT","t0d1")
        else if (toLower(trim(ext)) == "taxlaw") then
           call FilesModSetMacro("ALT","t1d0")
        else if (toLower(trim(ext)) == "taxlawdaca") then
           call FilesModSetMacro("ALT","t1d1")
        else
           call FilesModSetMacro("ALT",trim(FILE_EXTENSION(4:4)))
        end if
     else
        ! remove alternative macro and then entire line
        call FilesModSetMacro("ALT","")
        call FilesModSetMacro("MODDATA", "NULL")
     end if

     call FilesModSetMacro("RDFILE",ToLower(trim(assumpt(6)(2:))))
     
     if (toLower(trim(ext)) == "alt2") then
        call FilesModSetMacro("ALT1", "NULL")
        call FilesModSetMacro("ALT2", "")
        call FilesModSetMacro("ALT3", "NULL")
     else if(toLower(trim(ext)) == "alt1") then
        call FilesModSetMacro("ALT1", "")
        call FilesModSetMacro("ALT2", "NULL")
        call FilesModSetMacro("ALT3", "NULL")
     else if(toLower(trim(ext)) == "alt3") then
        call FilesModSetMacro("ALT1", "NULL")
        call FilesModSetMacro("ALT2", "NULL")
        call FilesModSetMacro("ALT3", "")
     else
        call FilesModSetMacro("ALT1", "NULL")
        call FilesModSetMacro("ALT2", "NULL")
        call FilesModSetMacro("ALT3", "NULL")
     end if
     
     call OpenFiles(trim(DAT_PATH) // "\econFilesList.txt")
     
  end subroutine init

!===============================================================================

  function getcyr(iarg, len) result(cyr)

     integer :: iarg
     integer, optional :: len
     character (len=4) :: cyr

     cyr = trim(IntToAsc(iarg))

     if (present(len)) then

        select case (len)
        
           case (2)
              cyr = trim(cyr(3:4)) ! last two digits of year as a string
           case (4)
              cyr = trim(cyr(1:4)) ! all four digits of year as a string
           case default
              write(*,*) "Error: Length of year string must be 2 or 4."
              write(*,'(/a)') "Extension in the parameters file &
                   &does not match the input."
              write(*,'(a)') "Terminating..."
              stop
              
        end select
        
     else
        cyr = trim(cyr(3:4)) ! last two digits of year as a string
     end if

  end function getcyr
  
!===============================================================================

   subroutine wrapUp()
  
      call CloseWorkfile(AFILE, .false.)
      call CloseWorkfile(ADFILE, .false.)
      call CloseWorkfile(DFILE, .false.)
      call CloseWorkfile(OTLFILE, .false.)
      call CloseWorkfile(OFILE1, .false.)
      call CloseWorkfile(OFILE2, .false.)
      call CloseWorkfile(OFILE3, .false.)
      call CloseWorkfile(OFILE4, .false.)
      call CloseWorkfile(MEF, .false.)
      call CloseWorkfile(ESF, .false.)
      call CloseWorkfile(BKDO1, .false.)
      call CloseWorkfile(BKDR1, .false.)
      call CloseWorkfile(CPSO_NILF, .false.)
      call CloseWorkfile(CE3750, .false.)
      call CloseWorkfile(CPSFILE, .false.)
      call CloseWorkfile(CNIPOPDATA, .false.)

      call SetWorkfileVerboseMode(.false.) ! This will uninitialize COM Automation if needed
     
      call OpenFile(21, trim(OUT_PATH) // "\log\Times.txt", &
         "replace")
      call ProfileWriteCheckpointTransitionReport(21)
      call CloseFile(21)

      call ProfileWriteFullReport(21, &
         trim(OUT_PATH) // "\log\Prof.txt")
      
      call CloseAllFiles()

   end subroutine wrapUp

!===============================================================================

end module EconMod