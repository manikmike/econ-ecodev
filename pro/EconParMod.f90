module EconParMod

   use WorkfileInterface
   use ProfileMod
   use ifport, only: getcwd
   include "OcactFortLib.inc"
   implicit none
   public
  
   ! Trustees Report year
   integer, parameter :: TRYEAR = 2025
   integer, parameter :: TRYR = TRYEAR - 1900

   ! If true, forces intermediate assumptions solution
   logical, parameter :: FORCE_ALT2 = .false.
   integer, parameter :: ALT2_SELNUM = 1727
   
   ! These can't be parameter because they are read at runtime
   ! (e.g., from config file)
   logical :: LIFE_EXP_ADJUST
   real (kind=8) :: LIFE_EXP_FACTOR
   character (len=LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: EXOG_OUT_ASSUMPT
   character (len=LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: EXOG_IN_SSA
   character (len=256) :: OTL_FILE
   character (len=256) :: TE_FILE
   integer :: CPB_TARGET_YEAR
   character (len=256) :: PRO_PATH, DAT_PATH, OUT_PATH
   character (len=256) :: FILE_EXTENSION
   integer :: LAST_MODDATA_YEAR
   ! Set default values for the following parameters to ensure
   ! backwards compatibility with older config files (e.g., config1342.txt)
   integer :: OUTPUT_TRYEAR = TRYEAR
   integer :: OUTPUT_TRYR = TRYR
   ! This parameter calculates the last year of output for the files
   ! that are used by other groups within OCACT as inputs to their
   ! programs (i.e., down-the-line-files)
   integer :: OUTPUT_LAST_YEAR
   integer :: OUTPUT_LAST_YR
   ! Initialize to -1 here and then use default values for TR_TARGET
   ! if it is still set to -1 in EconRevEarnMod.
   ! Otherwise, use the value provided (as a percent) in the config file.
   real (kind=8) :: TAXABLE_RATIO = -1d0
   ! Default value set to "GDP17" after the 2023 BEA Comprehensive Revision
   character (len=256) :: REAL_GDP_SERIES = "GDP17"
   ! Targets for OASDI and HI taxable wages in year TRYEAR-1
   ! in billions of dollars
   ! Initialize to -1 here and call wsqprogtr.prg
   ! if they are still set to -1 in EcoModSolA and/or EconRevearnMod
   ! Otherwise, use the value provided (in billions) from the config file.
   real (kind=8) :: HI_TARGET = -1d0
   real (kind=8) :: OASDI_TARGET = -1d0
   ! Use labor force model defined in EViews program
   logical :: USE_EVIEWS_LFPR_MODEL
   ! Write solution output to EViews workfiles
   ! Debug Solution Configuration is required when writing solution to workfiles
   logical :: WORKFILE_EXPORT = .true. ! default to true for backwards compatibility
   ! The WORKFILE_EXPORT parameter must be consistent with the file
   ! E:\usr\Econ\include\CompileSeriesMaps.fh
   ! .false. requires COMPILE_SERIES_MAPS = 0
   ! .true.  requires COMPILE_SERIES_MAPS = 1

   logical :: needHist = .false.
   logical :: needModData = .false.
   logical :: isBudgetRun = .false.
  
  ! Base year for age-adjusting labor force
  integer, parameter :: LC_BASE_YEAR = 2020 ! HOLD CONSTANT UNTIL LC MODEL REESTIMATED
  integer, parameter :: LC_BASE_YR = LC_BASE_YEAR - 1900
  integer, parameter :: LC_BASE_QTR = 4 * LC_BASE_YR
  integer, parameter :: N_BASE_QTR = LC_BASE_QTR
  
  ! Maximum number of items in a list
  integer, parameter :: MAX_LEN = 10000
  
  ! Use for labor force participation rate, 
  ! civilian labor force, and employment (p, lc, and e)
  character (len=15), dimension(36) :: ageGrpLabel = &
    (/ "1617", "1819", "2024", "2529", "3034", &
       "3539", "4044", "4549", "5054", "5559", &
       "6064", "6569", "7074", "7579", "8084", &
       "85O",  "75O",  "2534", "3544", "4554", &
       "5564", "65O",  "70O",  "16O",  "6061", &
       "6264", "1619", "2044", "6579", "6574", &
       "80O",  "8589", "9094", "9599", "1519", &
       "95O" /)
       
  ! Use for unemployment only (ru):
  ! elements 14,15,17,23 are different from above
  character (len=15), dimension(36) :: ageGrpLabel2 = &
    (/ "1617", "1819", "2024", "2529", "3034", &
       "3539", "4044", "4549", "5054", "5559", &
       "6064", "6569", "7074", "75O",  "70O",  &
       "85O",  "NA",   "2534", "3544", "4554", &
       "5564", "65O",  "NA",   "16O",  "6061", &
       "6264", "1619", "2044", "6579", "6574", &
       "80O",  "8589", "9094", "9599", "1519", &
       "95O" /)

  character (len=15), dimension(3) :: marStatLabel = &
    (/ "NM", "MS", "MA" /)
    
  character (len=15), dimension(0:2) :: sexLabel = &
    (/ "", "M", "F" /)
  
contains

!===============================================================================

   subroutine GetParameters(selnum)
   
      integer :: selnum
      integer :: eof, eq
      character (len=256) :: line, param, val
      
      call OpenFile(0102, trim(DAT_PATH)// "\config" // & 
         trim(IntToAsc(selnum)) // ".txt", "old")
         
      do
         read(0102,'(a)',iostat=eof) line
         if (eof /= 0) exit
         eq = index(line, "=")
         param = trim(adjustl(line(1:eq-1)))
         val = trim(adjustl(line(eq+1:)))
         
         select case(trim(param))
                        
            case("LIFE_EXP_ADJUST")
               if (trim(val) == "true") then
                  LIFE_EXP_ADJUST = .true.
               else if(trim(val) == "false") then
                  LIFE_EXP_ADJUST = .false.
               else
                  write(6,'(a)') "Unknown Value for LIFE_EXP_ADJUST " // trim(val)
               end if
               
            case("LIFE_EXP_FACTOR")
               LIFE_EXP_FACTOR = AscToDoublePrec(trim(val))
               
            case("EXOG_OUT_ASSUMPT")
               call GetParameterAsList(EXOG_OUT_ASSUMPT, trim(val))
               
            case("EXOG_IN_SSA")
               call GetParameterAsList(EXOG_IN_SSA, trim(val))
               
            case("OTL_FILE")
               OTL_FILE = trim(val)
                            
            case("CPB_TARGET_YEAR")
               CPB_TARGET_YEAR = AscToInt(trim(val))
               
            case("FILE_EXTENSION")
               FILE_EXTENSION = trim(val)
               
            case("LAST_MODDATA_YEAR")
               LAST_MODDATA_YEAR = AscToInt(trim(val))
               
            case("OUTPUT_TRYEAR")
               OUTPUT_TRYEAR = AscToInt(trim(val))
               OUTPUT_TRYR = OUTPUT_TRYEAR - 1900
               ! Last year of output for down-the-line files
               OUTPUT_LAST_YEAR = int((OUTPUT_TRYEAR + 80) / 5) * 5
               OUTPUT_LAST_YR = OUTPUT_LAST_YEAR - 1900               
               
            case("TAXABLE_RATIO")
               TAXABLE_RATIO = AscToDoublePrec(trim(val))
               
            case("REAL_GDP_SERIES")
               REAL_GDP_SERIES = trim(val)
               
            case("OASDI_TARGET")
               OASDI_TARGET = AscToDoublePrec(trim(val))
               
            case("HI_TARGET")
               HI_TARGET = AscToDoublePrec(trim(val))
               
            case("USE_EVIEWS_LFPR_MODEL")
               if (trim(val) == "true") then
                  USE_EVIEWS_LFPR_MODEL = .true.
               else if(trim(val) == "false") then
                  USE_EVIEWS_LFPR_MODEL = .false.
               else
                  write(6,'(a)') "Unknown Value for USE_EVIEWS_LFPR_MODEL " // trim(val)
               end if

            case("WORKFILE_EXPORT")
               if (trim(val) == "true") then
                  WORKFILE_EXPORT = .true.
               else if(trim(val) == "false") then
                  WORKFILE_EXPORT = .false.
               else
                  write(6,'(a)') "Unknown Value for USE_EVIEWS_LFPR_MODEL " // trim(val)
               end if
               
            case default
               write(6,'(a)') "Unknown Parameter " // trim(param)
         end select
      end do
   
   end subroutine GetParameters

!=============================================================================== 

   subroutine GetParameterAsList(param, val)

      character (len=LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: param
      character (len=*) :: val
      integer :: comma, i, last, count
      
      if (trim(adjustl(val)) /= "") then
         comma = index(val, ",")
         last = 0
         count = 1
         do i = 1, len_trim(val)
            if (val(i:i) == ",") then
               param(count) = trim(adjustl(val(last+1:i-1)))
               last = i
               count = count + 1
            end if
         end do
         param(count) = trim(adjustl(val(last+1:i-1)))
         param(0) = IntToAsc(count)
      else
         param(0) = IntToAsc(0)
      end if
   
   end subroutine GetParameterAsList

!=============================================================================== 

end module EconParMod
