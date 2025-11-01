module EconModSol1Mod

   use EconParMod
   use EconLibMod
   use EconModSol1VarMod
   use EconModSol1OutMod
   use EconModSol1EquationsMod
   use EconModSol1EViewsMod
   use ifport, only: system
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSol1Main, selnum, assumpt
   
contains

!===============================================================================

   subroutine EconModSol1Main()

      call InitModSol1()

      call SolveModel()
      if (isBudgetRun) then
         call ComputePreliminaryLaborForce()
      end if
      if (LIFE_EXP_ADJUST) call AdjustLifeExpectancy(LIFE_EXP_FACTOR)
      call SolveCivilianLaborForce(1)
      call SolveCivilianLaborForce(2)
      if (.NOT. isBudgetRun) then
         call HandleAdjustmentFactors(1)
         call HandleAdjustmentFactors(2)
      end if
      call CalculateAggregateLaborForce()
      call AppendHistoricalQuarterlyData()
      call ConvertQuarterlyToAnnual()
      call AppendHistoricalAnnualData()
      call CalculateAggregateUnemploymentRate()
      call AppendHistoricalUnemploymentRate()
      call ConvertUnemploymentRateToAnnual()
      call CalculateEmployment()
      call CalculateValuesAtFullEmployment()
      call ConvertFullEmploymentToAnnual()
      call AgeSexAdjustSolution()
      !call ControlUnemploymentAtFullEmployment()
      call CalculateAverageGDP()
      call EconModSol1OutMain()
      call DisplayWarning()
            
      call Finish()
             
   end subroutine EconModSol1Main
   
!===============================================================================

   subroutine InitModSol1()

      character (len = LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: add
      integer :: i

      call AssignFileNames(assumpt, efileName, dfileName, adfileName, &
                           afileName, ofile1Name, ofile2Name)
      
      if (WORKFILE_EXPORT) then
         ! Make copies of the AFILE, DFILE, and OTLFILE if importing back to EViews
         ! These copies become outputs
         call CopyDataFilesForOutput()
      end if
      
      ! Next 3 files are inputs
      AFILE = OpenWorkfileVerbose(trim(afileName) // ".wf1")
      DFILE = OpenWorkfileVerbose(trim(dfileName) // ".wf1")
      OTLFILE = OpenWorkfileVerbose(trim(OTL_FILE) // ".wf1")
      
      ADFILE = OpenWorkfileVerbose(trim(adfileName) // ".wf1")

      ! Current TR Population
      OFILE1 = OpenWorkfileVerbose(trim(ofile1Name) // ".wf1")
      ! Previous year TR Population (for disability rates)
      OFILE2 = OpenWorkfileVerbose(trim(ofile2Name) // ".wf1")
      ! Current TR Population - "R" file
      i = len_trim(ofile1Name)
      ofile3Name = trim(ToLower(ofile1Name))
      ofile3Name(i:i) = "r"
      OFILE3 = OpenWorkfileVerbose(trim(ofile3Name) // ".wf1")
      ! Current December Population
      OFILE4 = OpenWorkfileVerbose("decp" // trim(ToLower(ofile1Name(1:i-1))) // ".wf1")

      MEF = OpenWorkfileVerbose("mef.wf1")
      ESF = OpenWorkfileVerbose("esf.wf1")
      BKDO1 = OpenWorkfileVerbose("bkdo1.wf1")
      BKDR1 = OpenWorkfileVerbose("bkdr1.wf1")
      CPSO_NILF = OpenWorkfileVerbose("cpso_nilf.wf1")

      CE3750 = OpenWorkfileVerbose("ce3750.wf1")
      
      CPSFILE = OpenWorkfileVerbose("cpso68124.wf1")
      CNIPOPDATA = OpenWorkfileVerbose("cnipopdata.wf1")

      call FetchList(ADFILE, "ADD", add)

      per1 = trim(add(5))
      per2 = trim(assumpt(8))
      ! per1a and per2a contain an annual index (1-MAX_YR)
      ! qtr1a and qtr2a contain the quarter (1-4)
      ! For the workfile module, sample(1:2) is represented
      ! using an index (eg - quarterly index (1-MAX_QTR))
      ! Convert these using per1a * 4 + (qtr1a - 1)
      ! and per2a * 4 + (qtr2a - 1)
      per1a = dateyear(trim(per1))
      per2a = dateyear(trim(per2))
      qtr1a = dateqtr(trim(per1))
      qtr2a = dateqtr(trim(per2))

      ! This is a quarterly model starting in (per1a : qtr1a)
      ! solved using <useactual=yes> option 
      ! which preserves historical data if present
      startQtr = per1a * 4 + (qtr1a - 1)
      endQtr = per2a * 4 + (qtr2a - 1)      
      
      startYr = per1a    
      endYr = per2a 

      call InitializeModSol1Vars()
      
   end subroutine InitModSol1

!===============================================================================
   
   subroutine CopyDataFilesForOutput()
   
      integer :: rv1, rv2, rv3
      
      write(*,*)
   
      write(*,'(a$)') "Copying file " // trim(afileName) // ".wf1"
      rv1 = system("copy " // trim(DAT_PATH) // "\" // trim(afileName) // ".wf1" // &
         " " // trim(OUT_PATH) // "\mul\" // trim(ToLower(trim(afileName))) // ".wf1 > NUL")
      if (rv1 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
      
      write(*,*)
      
      write(*,'(a$)') "Copying file " // trim(dfileName) // ".wf1"
      rv2 = system("copy " // trim(DAT_PATH) // "\" // trim(dfileName) // ".wf1" // &
         " " // trim(OUT_PATH) // "\mul\" // trim(ToLower(trim(dfileName))) // ".wf1 > NUL")
      if (rv2 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
      
      write(*,*)
      
      write(*,'(a$)') "Copying file " // trim(OTL_FILE) // ".wf1"
      rv3 = system("copy " // trim(DAT_PATH) // "\" // trim(OTL_FILE) // ".wf1" // &
         " " // trim(OUT_PATH) // "\mul\" // trim(ToLower(trim(OTL_FILE))) // ".wf1 > NUL")
      if (rv3 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
   
   end subroutine CopyDataFilesForOutput
   
!===============================================================================

   subroutine SolveModel()
   
      write(*,'(//a//)') "Solving ModSol1, Please Wait..."
      if (.NOT. USE_EVIEWS_LFPR_MODEL) then
         call EconModSol1EquationsMain()
      else
         call EconModSol1EViewsMain()
      end if
   
   end subroutine SolveModel

!===============================================================================
   
   subroutine ComputePreliminaryLaborForce()
   
      integer :: i, sex, ageGrp, age
   
      p(1:2,31,:) = p80o(1:2,:)
      do i = startQtr, endQtr
         lc(0,0,i) = 0d0
         do sex = 1, 2
            do ageGrp = 1, 9
               lc(0,0,i) = lc(0,0,i) + p(sex,ageGrp,i) * n(sex,ageGrp,i)
            end do
            do age = 55, 79
               lc(0,0,i) = lc(0,0,i) + psy(sex,age,i) * nisy(sex,age,i)
            end do
            ! 80 and over
            lc(0,0,i) = lc(0,0,i) + p(sex,31,i) * n(sex,31,i)
         end do
      end do
      p80o(1:2,:) = p(1:2,31,:) 
   
   end subroutine ComputePreliminaryLaborForce

!===============================================================================
   
   ! This subroutine has been replaced with a parameter in config###.txt
   subroutine LifeExpectancyAdjustment

      character (len = 128) :: lcadj, sela
      real (kind = 8) :: wgt
      
      do
         write(*,'(/////a/)') "Life Expectancy LFPR Addfactors"
         write(*,'(///a//)')  "Choose from the following menu."
         write(*,'(a//)')     "      Selection    Description"
         write(*,'(a)')       "         a           (a)djust LFPRs with addfactors"
         write(*,'(/a)')      "                OR"
         write(*,'(/a)')      "       'CR'       Carriage Return        skip adjustment";
         write(*,'(/a$)')     "Enter selection: "

         read(*,'(a)') lcadj
         if (trim(ToLower(lcadj)) == 'a') then
            exit
         else if (trim(ToLower(lcadj)) == '') then
            return
         else
            cycle
         end if
      end do  
      
      do
         write(*,'(/a)') "step1";
         write(*,'(//a)') "What is the weighting factor for the LFPR &
            &addfactors? (enter a value between 0 and 1.0)"
         write(*,'(/a$)') "Please enter a factor or 'q' to quit: "
         read(*,'(a)') sela
         if (trim(ToLower(sela)) == 'q') then
            call Abort()
         else if (trim(ToLower(sela)) == '') then
            cycle
         else
            wgt = AscToDoublePrec(trim(sela))
            if (wgt >= 0d0 .and. wgt <= 1d0) then
               call AdjustLifeExpectancy(wgt)
               return
            else
               cycle
            end if
         end if         
      end do
      
   end subroutine LifeExpectancyAdjustment

!===============================================================================

   subroutine AdjustLifeExpectancy(wgt)

      real (kind = 8) :: wgt
      integer :: i, sex, age, ageGrp
      integer :: le_qbeg
      real (kind=8), dimension(MAX_QTR) :: adj, lctemp
      
      ! Set start year to 2nd year following latest complete year 
      ! of historical data for annual LFPRs
      ! (Note that if set to 1st year, then adjusted LFPRs may 
      ! overide actual quarterly values.)
      lctemp = 0d0
      call FetchSeries(DFILE, "LC.Q", lctemp)
      ! The previous call fails for some reason, so call it again
      ! There may be an issue with thread synchronization with EViews wfselect command
      call FetchSeries(DFILE, "LC.Q", lctemp)
      lc(0,0,sample(1):startQtr-1) = lctemp(sample(1):startQtr-1)
      lastDataQtr = sample(2)
      le_qbeg = lastDataQtr + 1
      
      lctemp = 0d0
      call FetchSeries(DFILE, "LCM.Q", lctemp)
      lc(1,0,sample(1):startQtr-1) = lctemp(sample(1):startQtr-1)
      
      lctemp = 0d0
      call FetchSeries(DFILE, "LCF.Q", lctemp)
      lc(2,0,sample(1):startQtr-1) = lctemp(sample(1):startQtr-1)

      ! Apply adjustment to LFPRs
      do ageGrp = 3, 9
         do sex = 1, 2
            call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
               trim(ageGrpLabel(ageGrp)) // "ADJ.Q", adj)
            do i = le_qbeg, endQtr
               p(sex,ageGrp,i) = p(sex,ageGrp,i) + adj(i) * wgt
               pms(sex,ageGrp,1:3,i) = pms(sex,ageGrp,1:3,i) + adj(i) * wgt
               if (sex == 2 .and. ageGrp >= 3 .and. ageGrp <= 7) then
                  pmsc6u(sex,ageGrp,1:3,i) = pmsc6u(sex,ageGrp,1:3,i) + adj(i) * wgt
                  pmsnc6(sex,ageGrp,1:3,i) = pmsnc6(sex,ageGrp,1:3,i) + adj(i) * wgt
               end if
            end do
         end do
      end do
      
      do age = 55, 100
         do sex = 1, 2
            call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
               trim(IntToAsc(age)) // "ADJ.Q", adj)   
            do i = le_qbeg, endQtr
               psy(sex,age,i) = psy(sex,age,i) + adj(i) * wgt
            end do
         end do
      end do
      
      p(1:2,31,:) = p80o(1:2,:)
      do ageGrp = 31, 31
         do sex = 1, 2
            call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
               trim(ageGrpLabel(ageGrp)) // "ADJ.Q", adj)
            do i = le_qbeg, endQtr
               p(sex,ageGrp,i) = p(sex,ageGrp,i) + adj(i) * wgt
            end do
         end do
      end do
      p80o(1:2,:) = p(1:2,31,:)

   end subroutine AdjustLifeExpectancy
   
!===============================================================================

   subroutine SolveCivilianLaborForce(option)
   
      integer :: option
      integer :: i, age, sex, ageGrp, le_qbeg
      real (kind=8), dimension(MAX_QTR) :: lctemp, ntemp

      lctemp = 0d0
      call FetchSeries(DFILE, "LC.Q", lctemp)
      lc(0,0,sample(1):startQtr-1) = lctemp(sample(1):startQtr-1)
      lastDataQtr = sample(2)
      le_qbeg = lastDataQtr + 1
      
      if (option == 1) then
          do sex = 1, 2
             do ageGrp = 1, 9
                call FetchSeries(DFILE, "L" // trim(sexLabel(sex)) // &
                   trim(ageGrpLabel(ageGrp)) // ".Q", lc(sex,ageGrp,:))
                do i = le_qbeg, endQtr
                   lc(sex,ageGrp,i) = p(sex,ageGrp,i) * n(sex,ageGrp,i)
                   lc(sex,0,i) = lc(sex,0,i) + p(sex,ageGrp,i) * n(sex,ageGrp,i)
                end do
             end do
          end do
          
          do age = 55, 100
             do sex = 1, 2
                if (age < 80) then
                   lctemp = 0d0
                   call FetchSeries(DFILE, "L" // trim(sexLabel(sex)) // &
                      trim(IntToAsc(age)) // ".Q", lctemp)
                   lcsy(sex,age,sample(1):startQtr-1) = lctemp(sample(1):startQtr-1)
                end if                  
                do i = le_qbeg, endQtr
                   lcsy(sex,age,i) = psy(sex,age,i) * nisy(sex,age,i)
                   if (age < 80) lc(sex,0,i) = lc(sex,0,i) + psy(sex,age,i) * nisy(sex,age,i)
                end do
             end do
          end do
          
         do sex = 1, 2
             lctemp = 0d0
             call FetchSeries(DFILE, "L" // trim(sexLabel(sex)) // &
                trim(ageGrpLabel(31)) // ".Q", lctemp)
             lc(sex,31,sample(1):startQtr-1) = lctemp(sample(1):startQtr-1)
             
             lctemp = 0d0
             call FetchSeries(DFILE, "N" // trim(sexLabel(sex)) // &
                 trim(ageGrpLabel(31)) // ".Q", ntemp)
             n(sex,31,sample(1):startQtr-1) = ntemp(sample(1):startQtr-1)
         end do
             
         do i = le_qbeg, endQtr
            do sex = 1, 2
                lc(sex,31,i) = p(sex,31,i) * n(sex,31,i)
                lc(sex,0,i) = lc(sex,0,i) + p(sex,31,i) * n(sex,31,i)
            end do
            lc(0,0,i) = lc(1,0,i) + lc(2,0,i)
         end do
          
      else if (option == 2) then
      
         do i = lastDataQtr+1, endQtr
            do sex = 1, 2
               do age = 66, 72
                  lcsy(sex,age,i) = psy(sex,age,i) * nisy(sex,age,i)
               end do
            end do
         end do

         do i = lastDataQtr+1, endQtr
            do sex = 1, 2
               lc(sex,0,i) = 0d0
               do ageGrp = 1, 9
                  lc(sex,0,i) = lc(sex,0,i) + lc(sex,ageGrp,i) 
               end do
               do age = 55, 79
                  lc(sex,0,i) = lc(sex,0,i) + lcsy(sex,age,i)
               end do
               lc(sex,0,i) = lc(sex,0,i) + lc(sex,31,i)
            end do
            lc(0,0,i) = lc(1,0,i) + lc(2,0,i)            
         end do
      end if
   
   end subroutine SolveCivilianLaborForce

!===============================================================================

   subroutine HandleAdjustmentFactors(ct1)
   
      integer :: i, sex, ageGrp, age
      integer :: ct1
      integer :: ct3 = 0
      character (len = 2) :: l1
      integer, dimension(2) :: per
      integer :: perl1 = 0
      integer :: perl2 = 0
      integer :: per1x = 0
      integer :: per2x = 0
      integer :: s1 = 0
      integer :: s2 = 0
      integer :: a1 = 0
      integer :: a2 = 0
      
      integer :: qtrs_avg = 1 ! WE MAY WANT TO CHANGE THIS TO 4 OR SO TO SMOOTH OBSERVATION ERRORS
      real (kind=8), dimension(MAX_QTR) :: adj, ruadj
      real (kind=8) :: temp, pp80o
      real (kind=8), dimension(MAX_QTR) :: lastData
      
      character (len=LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: x

      ! Check for Adjustment Factors
      if(ct1 == 1) then
         l1 = "LC"
         tp(1,2:7,:) = p(1,2:7,:)
         tp_fe(1,2:7,:) = p_fe(1,2:7,:)
      else
         return
         l1 = "RU"
      end if
      
      ! write(*,'(///a/)') "Checking for Adjustment Factors for " // l1
      
      adj = 0d0
      x = ""
      call FetchSeries(ADFILE, l1 // ".ADJ", adj)
      if (sample(2) /= 0d0) then
         x(0) = "1"
         x(1) = l1
         perl1 = sample(1)
         perl2 = sample(2)
      end if

      ! write(*,'(a)') "per1 is (" // trim(IntToAsc(dateyear(per1)+1900)) // &
      !   "Q" // trim(IntToAsc(dateqtr(per1))) // "), per2 is (" // &
      !    trim(IntToAsc(dateyear(per2)+1900)) // "Q" // &
      !    trim(IntToAsc(dateqtr(per2))) //  ")"

      
      ! Compares solution period to range of adjustment factor so that
      ! an adjustment period, that is a subset of the solution period,
      ! can be set.
      ! Note: The adjustment period may be equal to or greater than the
      ! start of the solution period.  This permits adjustments to the
      ! actual historical data, if desired.
      if (x(0) /= "") then
         ! call FetchSeries(DFILE, "YEAR.Q", patYear) replaced by function patYear below
         sample(1) = 46
         sample(2) = per2a
         ! per1a and per2a contain an annual index (1-MAX_YR)
         ! s = solution period,  a = adjustment period
         s1 = patYear(per1a * 4 + (qtr1a - 1))
         s2 = patYear(per2a * 4 + (qtr2a - 1))
         a1 = patYear(perl1)
         a2 = patYear(perl2)
         ! no overlap of adjustment and solution periods
         if (a2 < s1 .or. a1 > s2) return
         ! last year of adjustment period is first year of solution period
         if (a2 == s1) then 
            per(1) = a2
            per(2) = min(a2+3, MAX_QTR)
         end if
         ! first year of adjustment period is last year of solution period
         if (a1 == s2) then 
            per(1) = a1
            per(2) = min(a1+3, MAX_QTR)
         end if
         ! Other possible overlap combinations
         if(a1 <= s1 .and. a2 <= s2) then
             per(1) = s1
             per(2) = a2
          else if (a1 <= s1 .and. a2 > s2) then
             per(1) = s1
             per(2) = s2
         else if (a1 > s1 .and. a2 <= s2) then 
             per(1) = a1
             per(2) = a2
         else ! (a1 > s1 .and. a2 > s2) then
             per(1) = a1
             per(2) = s2
         end if
         
         sample(1) = per(1) * 4
         sample(2) = per(2) * 4 + 3
         per1x = sample(1)
         per2x = sample(2)
         
         ! Checks for 1st Non-zero value for adjustment factor in period.
         ! If found, the adjustment period is shortened to exclude front loaded
         ! zeroes.
         ct3 = 0
         do i = per1x, per2x
            if (adj(i) /= 0) then
               sample(1) = i
               sample(2) = per2x
               ct3 = 1
               exit
            end if
         end do
         
         ! Notify operator of non-zero adjustment factor in the solution range
         if (ct3 == 1) then
            ! write(*,'(/////a/)') trim(x(1)) // " adjustment factor with " // &
            !    "non-zero values has been found"
            ! write(*,'(a)') "in the solution range." 

            ! Adjust variable and distribute by relative size
            if (ct1 == 1) then      !LC

               lcadj = 0d0
               lcadjsy = 0d0
               lcadj(0,0,:) = adj
               lcadjsy(0,0,:) = adj
               
               do sex = 1, 2
                  do ageGrp = 1, 9
                     lastData = 0d0
                     call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
                        trim(ageGrpLabel(ageGrp)) // ".Q", lastData)
                     p(sex,ageGrp,lastDataQtr-1:lastDataQtr) = lastData(lastDataQtr-1:lastDataQtr)
                  end do
               end do
 
               do sex = 1, 2
                  do age = 55, 79
                     lastData = 0d0
                     call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
                        trim(IntToAsc(age)) // ".Q", lastData)
                     psy(sex,age,lastDataQtr-1:lastDataQtr) = lastData(lastDataQtr-1:lastDataQtr)
                  end do
               end do
               
               do sex = 1, 2
                  do ageGrp = 31, 31 ! 80 and over
                     lastData = 0d0
                     call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
                        trim(ageGrpLabel(ageGrp)) // ".Q", lastData)
                     p(sex,ageGrp,lastDataQtr-1:lastDataQtr) = lastData(lastDataQtr-1:lastDataQtr)
                  end do
               end do
 
               do sex = 1, 2
                  do ageGrp = 1, 2
                     do i = lastDataQtr-qtrs_avg+1, lastDataQtr
                        lcadj(sex,ageGrp,i) = (p(sex,ageGrp,i) - p_p(sex,ageGrp,i) - p_add2(sex,ageGrp,i)) * n(sex,ageGrp,i)
                     end do
                  end do
                  do ageGrp = 3, 9
                     call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
                        trim(ageGrpLabel(ageGrp)) // "ADJ.Q", adj)
                     do i = lastDataQtr-qtrs_avg+1, lastDataQtr
                        lcadj(sex,ageGrp,i) = (p(sex,ageGrp,i) - p_p(sex,ageGrp,i) - p_add2(sex,ageGrp,i) - adj(i) * LIFE_EXP_FACTOR) * n(sex,ageGrp,i)
                     end do
                  end do
                  do age = 55, 79
                     call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
                        trim(IntToAsc(age)) // "ADJ.Q", adj)
                     do i = lastDataQtr-qtrs_avg+1, lastDataQtr
                        lcadjsy(sex,age,i) = (psy(sex,age,i) - psy_p(sex,age,i) - psy_add2(sex,age,i) - psy_add(sex,age,i) - adj(i) * LIFE_EXP_FACTOR) * nisy(sex,age,i)
                     end do
                  end do
                  ! 80 and over
                  call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // "80oADJ.Q", adj)
                  do i = lastDataQtr-qtrs_avg+1, lastDataQtr
                     pp80o = 0d0
                     do age = 80, 100
                         pp80o = pp80o + psy_p(sex,age,i) * nisy(sex,age,i)
                     end do
                     pp80o = pp80o / n(sex,31,i)
                     lcadj(sex,31,i) = (p(sex,31,i) - pp80o - adj(i) * LIFE_EXP_FACTOR) * n(sex,31,i)
                  end do
               end do
               do sex = 1, 2
                  
                  do ageGrp = 1, 9
                     temp = 0d0
                     do i = lastDataQtr-qtrs_avg+1, lastDataQtr
                        temp = temp + lcadj(sex,ageGrp,i)
                     end do
                     lcadj(sex,ageGrp,lastDataQtr) = temp / qtrs_avg
                  end do
                  
                  temp = 0d0
                  do i = lastDataQtr-qtrs_avg+1, lastDataQtr
                    temp = temp + lcadj(sex,31,i)
                  end do
                  lcadj(sex,31,lastDataQtr) = temp / qtrs_avg

                  do age = 55, 79
                     temp = 0d0
                     do i = lastDataQtr-qtrs_avg+1, lastDataQtr
                        temp = temp + lcadjsy(sex,age,i)
                     end do
                     lcadjsy(sex,age,lastDataQtr) = temp / qtrs_avg
                  end do
                  
               end do
               
               lcadj(0,24,lastDataQtr) = sum(lcadj(1:2,1:9,lastDataQtr)) + &
                                         sum(lcadj(1:2,31,lastDataQtr)) + &
                                         sum(lcadjsy(1:2,55:79,lastDataQtr))
                                         
               do i = lastDataQtr+1, endQtr
                  lcadj(0,24,i) = lcadj(0,24,lastDataQtr)
                  if (abs(lcadj(0,24,i)) > 1d-7) then
                     temp = lcadj(0,0,i) / lcadj(0,24,i)
                  else
                     temp = 0d0
                  end if
                  do sex = 1, 2
                     
                     do ageGrp = 1, 9
                        lcadj(sex,ageGrp,i) = lcadj(sex,ageGrp,lastDataQtr) * temp
                     end do
                     
                     lcadj(sex,31,i) = lcadj(sex,31,lastDataQtr) * temp
                     
                     do age = 55, 79
                        lcadjsy(sex,age,i) = lcadjsy(sex,age,lastDataQtr) * temp
                     end do
                     
                  end do
               end do
               
               ! Set adjustments in historical periods back to zero
               do i = lastDataQtr-qtrs_avg+1, lastDataQtr
                   lcadj(:,:,i) = 0d0
                   lcadjsy(:,:,i) = 0d0
               end do
               do sex = 1, 2
                  do i = startQtr, endQtr
                     do ageGrp = 1, 9
                        temp = 1 + (lcadj(sex,ageGrp,i) / n(sex,ageGrp,i)) / p(sex,ageGrp,i)
                        p(sex,ageGrp,i) = p(sex,ageGrp,i) * temp
                        if (ageGrp >= 3 .and. ageGrp <=9) then
                           pms(sex,ageGrp,1:3,i) = pms(sex,ageGrp,1:3,i) * temp
                           if (sex == 2 .and. ageGrp <=7) then
                              pmsc6u(sex,ageGrp,1:3,i) = pmsc6u(sex,ageGrp,1:3,i) * temp
                              pmsnc6(sex,ageGrp,1:3,i) = pmsnc6(sex,ageGrp,1:3,i) * temp
                           end if
                        end if
                     end do
                     do age = 55, 79
                        temp = 1 + (lcadjsy(sex,age,i) / nisy(sex,age,i)) / psy(sex,age,i)
                        psy(sex,age,i) = psy(sex,age,i) * temp
                     end do
                     ! 80 and over
                     temp = 1 + (lcadj(sex,31,i) / n(sex,31,i)) / p(sex,31,i)
                     p(sex,31,i) = p(sex,31,i) * temp
                     p80o(sex,i) = p(sex,31,i)
                     do age = 80, 100
                        psy(sex,age,i) = psy(sex,age,i) * temp
                     end do
                  end do
               end do
               
            else if (ct1 == 2)  then !RU
               ruadj = (ru(0,0,:) + adj) / (ru(0,0,:) + epsilon(0d0))
               do sex = 0, 2
                  do ageGrp = 0, 14
                     do i = startQtr, endQtr
                        if (ruadj(i) /= 0) ru(sex,ageGrp,i) = ru(sex,ageGrp,i) * ruadj(i)
                     end do
                  end do
               end do
            end if
         end if
      end if
      
   end subroutine HandleAdjustmentFactors
     
!===============================================================================

   double precision function patYear(qtr)
      ! This function replaces the series YEAR.Q from the DFILE
      integer :: qtr
      
      patYear = 0.25 * qtr - 0.375d0
      
   end function
   
!===============================================================================  

   subroutine CalculateAggregateLaborForce()
   
      integer :: i, sex, ageGrp, age, grp, lowAge, hiAge, lowGrp, hiGrp
      integer, dimension(2) :: per
      real (kind=8), dimension(MAX_QTR) :: lctemp, &
                                           ptemp
      integer, dimension(21) :: grpNum = (/ 10, 11, 12, 13, 14, 15, 32, 33, 34, &
         27, 18, 19, 20, 21, 30, 16, 31, 17, 23, 22, 24/)
      
      
      ! Compute LC from P and N
      do sex = 1, 2
        do i = 48*4, endQtr
           do ageGrp = 1, 9
              lc(sex,ageGrp,i) = p(sex,ageGrp,i) * n(sex,ageGrp,i)
           end do
           ageGrp = 31
              lc(sex,ageGrp,i) = p(sex,ageGrp,i) * n(sex,ageGrp,i)
           do age = 55, 100
              lcsy(sex,age,i) = psy(sex,age,i) * nisy(sex,age,i)
           end do
        end do
      end do
      
      ! Append LC historical data for 5-year age groups
      per(1) = 48 * 4
      do sex = 1, 2
         do ageGrp = 1, 9
            lctemp = 0d0
            call FetchSeries(DFILE, "L" // trim(sexLabel(sex)) // &
               trim(ageGrpLabel(ageGrp)) // ".Q", lctemp)
               per(2) = sample(2)
            lc(sex,ageGrp,per(1):per(2)) = lctemp(per(1):per(2))
         end do
         ageGrp = 31
         lctemp = 0d0
         call FetchSeries(DFILE, "L" // trim(sexLabel(sex)) // &
            trim(ageGrpLabel(ageGrp)) // ".Q", lctemp)
         per(2) = sample(2) 
         lc(sex,ageGrp,per(1):per(2)) = lctemp(per(1):per(2))                             
      end do
      do sex = 1, 2
         do age = 55, 79
            lctemp = 0d0
            call FetchSeries(DFILE, "L" // trim(sexLabel(sex)) // &
               trim(IntToAsc(age)) // ".Q", lctemp)
            lcsy(sex,age,48:sample(2)) = lctemp(48:sample(2))
         end do
      end do
      
      ! Append LFPR historical data for 5-year age groups
      per(1) = 48 * 4
      do sex = 1, 2
         do ageGrp = 1, 9
            ptemp = 0d0
            call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
               trim(ageGrpLabel(ageGrp)) // ".Q", ptemp)
               per(2) = sample(2)
            p(sex,ageGrp,per(1):per(2)) = ptemp(per(1):per(2))
         end do
         ageGrp = 31
         ptemp = 0d0
         call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
            trim(ageGrpLabel(ageGrp)) // ".Q", ptemp)
            per(2) = sample(2)
         p(sex,ageGrp,per(1):per(2)) = ptemp(per(1):per(2))
         p80o(sex,per(1):per(2)) = ptemp(per(1):per(2))
      end do
      do sex = 1, 2
         do age = 55, 79
            ptemp = 0d0
            call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
               trim(IntToAsc(age)) // ".Q", ptemp)
            psy(sex,age,48:sample(2)) = ptemp(48:sample(2))
         end do
      end do
      
      ! Aggregate LC age groups
      ! Compute totals by sex and age group
      ! array grpNum contains correct age group index
      per(1) = startQtr !48 * 4
      per(2) = endQtr
      
      do i = per(1), per(2)
         do sex = 1, 2
            do grp = 1, 9 ! 55-59, 60-64, ... 95-99
               lowAge = 55 + (grp - 1) * 5
               hiAge  = lowAge + 4
               lc(sex,grpNum(grp),i) = sum(lcsy(sex,lowAge:hiAge,i))
            end do
            lc(sex,grpNum(10),i) = sum(lc(sex,1:2,i)) ! 16-19
            do grp = 11, 15 ! 25-34, 35-44, 45-54, 55-64, 65-74
               lowGrp = 4 + (grp - 11) * 2
               hiGrp = lowGrp + 1
               lc(sex,grpNum(grp),i) = sum(lc(sex,lowGrp:hiGrp,i))
            end do
            lc(sex,grpNum(16),i) = sum(lc(sex,32:34,i)) + lcsy(sex,100,i) ! 85o
            ! Skipping 80o here since since it would use endogenous for 85o instead of historical
            ! 75o = 80o + 7579, 70o = 75o + 7074, 65o = 70o + 6569
            do grp = 18, 20 ! 75o, 70o, 65o
               ageGrp = 14 + (18 - grp)
               lc(sex,grpNum(grp),i) = lc(sex,grpNum(grp-1),i) + lc(sex,ageGrp,i)
            end do
            lc(sex,grpNum(21),i) = lc(sex,grpNum(20),i) + lc(sex,27,i) + &
               lc(sex,3,i) + sum(lc(sex,18:21,i)) ! 16o
            lc(sex,0,i) = lc(sex,grpNum(21),i)    ! lc{m,f} = l{m,f}16o      
         end do
      end do
      
      ! Aggregate LC
      lc(0,24,:) = lc(1,24,:)  + lc(2,24,:)
      lc(0,0,:) = lc(0,24,:)
      
      p(1:2,31,:) = p80o(1:2,:)
      ! Aggregate LFPR
      do sex = 1, 2
         do grp = 1, 21
            do i = per(1), per(2)
               p(sex,grpNum(grp),i) = lc(sex,grpNum(grp),i) / &
                  (n(sex,grpNum(grp),i) + epsilon(0d0))
            end do
         end do
      end do
      p80o(1:2,:) = p(1:2,31,:)
      
   end subroutine CalculateAggregateLaborForce
   
!===============================================================================

   subroutine AppendHistoricalQuarterlyData

      integer :: i, sex, grp
      integer, dimension(2) :: per
      real (kind=8), dimension(MAX_QTR) :: lctemp, ptemp
      integer, dimension(16) :: grpNum = (/ 10, 11, 12, 13, 14, &
         27, 18, 19, 20, 21, 30, 31, 17, 23, 22, 24 /)
         
      do sex = 1, 2
         do grp = 1, 16
            lctemp = 0
            call FetchSeries(DFILE, &
               "L"//trim(sexLabel(sex))//trim(ageGrpLabel(grpNum(grp)))//".Q", &
               lctemp)
            per(1) = 48 * 4
            per(2) = sample(2)
            do i = per(1), per(2)
               lc(sex,grpNum(grp),i) = lctemp(i)
            end do
         end do
      end do
      
      do sex = 1, 2
         do grp = 1, 16
            ptemp = 0
            call FetchSeries(DFILE, &
               "P"//trim(sexLabel(sex))//trim(ageGrpLabel(grpNum(grp)))//".Q", &
               ptemp)
            per(1) = 48 * 4
            per(2) = sample(2)
            do i = per(1), per(2)
               p(sex,grpNum(grp),i) = ptemp(i)
            end do
         end do
      end do
      p80o(1:2,:) = p(1:2,31,:)
      
      lc(1:2,0,:) = lc (1:2,24,:)
      lc(0,0,:) = lc(1,0,:) + lc(2,0,:)
      lc(0,24,:) = lc(1,24,:) + lc(2,24,:)
      p(0,24,:) = lc(0,0,:) / (n(1,24,:) + n(2,24,:) + epsilon(0d0))
         
   end subroutine AppendHistoricalQuarterlyData

!===============================================================================

   subroutine ConvertQuarterlyToAnnual()
   
      integer :: sex, age, ageGrp, marStat

      ! Labor Force Participation Rates
      !p(1:2,31,:) = p80o(1:2,:)
      p_p(1:2,31,:) = p80o_p(1:2,:)
      do sex = 1, 2
         do ageGrp = 1, 34
            if (ageGrp == 25 .or. ageGrp == 26 .or. &
                ageGrp == 28 .or. ageGrp == 29) cycle
            call collapse(p_a(sex,ageGrp,:), p(sex,ageGrp,:))
            call collapse(p_p_a(sex,ageGrp,:), p_p(sex,ageGrp,:))
            if (ageGrp >= 3) then
               do marStat = 1, 3
                  if (ageGrp <= 9) then
                     call collapse(pms_a(sex,ageGrp,marStat,:), &
                        pms(sex,ageGrp,marStat,:))
                     call collapse(pms_p_a(sex,ageGrp,marStat,:), &
                        pms_p(sex,ageGrp,marStat,:))
                  end if
                  if (sex == 2 .and. ageGrp <= 7) then
                     call collapse(pmsc6u_a(sex,ageGrp,marStat,:), &
                        pmsc6u(sex,ageGrp,marStat,:))
                     call collapse(pmsnc6_a(sex,ageGrp,marStat,:), &
                        pmsnc6(sex,ageGrp,marStat,:))
                     call collapse(pmsc6u_p_a(sex,ageGrp,marStat,:), &
                        pmsc6u_p(sex,ageGrp,marStat,:))
                     call collapse(pmsnc6_p_a(sex,ageGrp,marStat,:), &
                        pmsnc6_p(sex,ageGrp,marStat,:))
                  end if
               end do
            end if
         end do
         p80o_a(1:2,:) = p_a(1:2,31,:)
         p80o_p_a(1:2,:) = p_p_a(1:2,31,:)
         do age = 55, 100
            call collapse(psy_a(sex,age,:), psy(sex,age,:))
            call collapse(psy_p_a(sex,age,:), psy_p(sex,age,:))
         end do
      end do
      call collapse(p_a(0,24,:), p(0,24,:))
      call collapse(p_p_a(0,24,:), p_p(0,24,:))
      p_a(0,0,:) = p_a(0,24,:)
      p_p_a(0,0,:) = p_p_a(0,24,:)

      ! Civilian Labor Force
      do sex = 1, 2
         do ageGrp = 1, 34
            if (ageGrp == 25 .or. ageGrp == 26 .or. &
                ageGrp == 28 .or. ageGrp == 29) cycle
            call collapse(lc_a(sex,ageGrp,:), lc(sex,ageGrp,:))
         end do
         call collapse(lc_a(sex,0,:), lc(sex,0,:))
         call collapse(lc_p_a(sex,0,:), lc_p(sex,0,:))
      end do
      call collapse(lc_a(0,0,:), lc(0,0,:))
      call collapse(lc_p_a(0,0,:), lc_p(0,0,:))
      lc_a(0,24,:) = lc_a(0,0,:)
      
      do sex = 1, 2
         do age = 55, 100
            call collapse(lcsy_a(sex,age,:), lcsy(sex,age,:))
         end do
      end do
   
   end subroutine ConvertQuarterlyToAnnual

!===============================================================================

   subroutine AppendHistoricalAnnualData()
   
      integer, dimension(2) :: per
      integer :: sex, age, ageGrp
      real (kind=8), dimension(MAX_YR) :: lctemp, psytemp, ptemp
   
      per(1) = 48
      per(2) = per1a - 1
      do sex = 1, 2
         do ageGrp = 1, 31
            if (ageGrp == 15 .or. ageGrp == 16 .or. &
                ageGrp == 25 .or. ageGrp == 26 .or. &
                ageGrp == 28 .or. ageGrp == 29) cycle            
            call FetchSeries(DFILE, "L" // trim(sexLabel(sex)) // &
               trim(ageGrpLabel(ageGrp)) // ".A", lctemp)
            per(1) = sample(1)
            per(2) = sample(2)
            lc_a(sex,ageGrp,per(1):per(2)) = lctemp(per(1):per(2))
            call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
               trim(ageGrpLabel(ageGrp)) // ".A", ptemp)
            per(1) = sample(1)               
            per(2) = sample(2)
            p_a(sex,ageGrp,per(1):per(2)) = ptemp(per(1):per(2))
            if (ageGrp == 31) then
               p80o_a(sex,per(1):per(2)) = ptemp(per(1):per(2))
            end if
         end do
      end do
      
      do sex = 1, 2
         do age = 55, 79
            call FetchSeries(DFILE, "P" // trim(sexLabel(sex)) // &
               trim(IntToAsc(age)) // ".A", psytemp)
            per(1) = sample(1)               
            per(2) = sample(2)
            psy_a(sex,age,per(1):per(2)) = psytemp(per(1):per(2))
         end do
      end do
   
   end subroutine AppendHistoricalAnnualData

!===============================================================================

   subroutine CalculateAggregateUnemploymentRate()
   
      integer :: i, sex
   
      do i = 48*4, endQtr
         do sex = 1, 2
            ru(sex,27,i) = (ru(sex,1,i) * lc(sex,1,i) + ru(sex,2,i) * lc(sex,2,i)) / lc(sex,27,i)
            ru(sex,18,i) = (ru(sex,4,i) * lc(sex,4,i) + ru(sex,5,i) * lc(sex,5,i)) / lc(sex,18,i)
            ru(sex,19,i) = (ru(sex,6,i) * lc(sex,6,i) + ru(sex,7,i) * lc(sex,7,i)) / lc(sex,19,i)
            ru(sex,20,i) = (ru(sex,8,i) * lc(sex,8,i) + ru(sex,9,i) * lc(sex,9,i)) / lc(sex,20,i)
            ru(sex,21,i) = (ru(sex,10,i) * lc(sex,10,i) + ru(sex,11,i) * lc(sex,11,i)) / lc(sex,21,i)
            ru(sex,22,i) = (ru(sex,12,i) * lc(sex,12,i) + ru(sex,13,i) * lc(sex,13,i) &
               + ru(sex,14,i) * lc(sex,17,i)) / lc(sex,22,i)
            ru(sex,15,i) = (ru(sex,13,i) * lc(sex,13,i) + ru(sex,14,i) * lc(sex,17,i)) / lc(sex,23,i)
            ru(sex,24,i) = (ru(sex,27,i) * lc(sex,27,i) + ru(sex,3,i) * lc(sex,3,i) &
               + dot_product(ru(sex,18:22,i),lc(sex,18:22,i))) / lc(sex,24,i)
            ru(sex,0,i) = ru(sex,24,i)
         end do
         ru(0,24,i) = (ru(1,24,i) * lc(1,24,i) + ru(2,24,i) * lc(2,24,i)) / lc(0,24,i)
         ru(0,0,i) = ru(0,24,i)
      end do
      
   
   end subroutine CalculateAggregateUnemploymentRate

!===============================================================================

   subroutine AppendHistoricalUnemploymentRate()
   
      integer :: sex, grp
      integer, dimension(7) :: grpNum =(/ 27, 18, 19, 20, 21, 22, 24 /)
      real (kind = 8), dimension(MAX_QTR) :: rutemp
      
      do sex = 1, 2
         do grp = 1, 7
            call FetchSeries(DFILE, "R"//trim(sexLabel(sex))//trim(ageGrpLabel2(grpNum(grp)))//".Q", rutemp)
            ru(sex,grpNum(grp),48*4:sample(2)) = rutemp(48*4:sample(2))
         end do
         call FetchSeries(DFILE, "RU"//trim(sexLabel(sex))//".Q", rutemp)
         ru(sex,0,48*4:sample(2)) = rutemp(48*4:sample(2))
      end do
      
      call FetchSeries(DFILE, "R16O.Q", rutemp)
      ru(0,24,48*4:sample(2)) = rutemp(48*4:sample(2))

      call FetchSeries(DFILE, "RU.Q", rutemp)
      ru(0,0,48*4:sample(2)) = rutemp(48*4:sample(2))
   
   end subroutine AppendHistoricalUnemploymentRate
   
!=============================================================================== 

   subroutine ConvertUnemploymentRateToAnnual()
   
      integer :: i, sex, ageGrp
      real (kind=8), dimension(MAX_QTR) :: rutemp
      real (kind=8), dimension(MAX_YR) :: rutemp_a

      do sex = 1, 2
         do ageGrp = 1, 27
            if (ageGrp > 15 .and. ageGrp < 18 .or. ageGrp == 23 .or. &
               ageGrp == 25 .or. ageGrp == 26) cycle
            call collapse(ru_a(sex,ageGrp,:), ru(sex,ageGrp,:))
            call collapse(ru_p_a(sex,ageGrp,:), ru_p(sex,ageGrp,:))
            call FetchSeries(DFILE,"R" // trim(sexLabel(sex)) // &
               trim(ageGrpLabel2(ageGrp)) // ".Q", rutemp)
            do i = sample(1), sample(2)
               ru(sex,ageGrp,i) = rutemp(i)
            end do
            if (mod(sample(1),4) /= 0) ru_a(sex,ageGrp,sample(1)/4) = 0d0

            call FetchSeries(DFILE,"R" // trim(sexLabel(sex)) // &
               trim(ageGrpLabel2(ageGrp)) // ".A", rutemp_a)
            do i = sample(1), sample(2)
               ru_a(sex,ageGrp,i) = rutemp_a(i)
            end do


         end do
         call FetchSeries(DFILE,"RU" // trim(sexLabel(sex)) // ".Q", rutemp)
         do i = sample(1), sample(2)
            ru(sex,0,i) = rutemp(i)
         end do
         call collapse(ru_a(sex,0,:), ru(sex,0,:))
         if (mod(sample(1),4) /= 0) ru_a(sex,0,sample(1)/4) = 0d0
      end do
      
      call FetchSeries(DFILE,"RU.Q", rutemp)
      do i = sample(1), sample(2)
         ru(0,0,i) = rutemp(i)
      end do
      call collapse(ru_a(0,0,:), ru(0,0,:))
      if (mod(sample(1),4) /= 0) ru_a(0,0,sample(1)/4) = 0d0
      ru_a(0,24,:) = ru_a(0,0,:)      

   end subroutine ConvertUnemploymentRateToAnnual
      
!===============================================================================  

   subroutine CalculateEmployment()
   
      integer :: sex, ageGrp
      real (kind=8), dimension(MAX_QTR) :: etemp
      real (kind=8), dimension(MAX_YR) :: etemp_a
      
      ! Quarterly
      do sex = 1, 2
        do ageGrp = 1, 27
           if (ageGrp > 14 .and. ageGrp < 18 .or. ageGrp > 22 .and. ageGrp < 27) cycle
           if (ageGrp /= 14) then
              e(sex,ageGrp,:) = lc(sex,ageGrp,:) * (1d0 - ru(sex,ageGrp,:) / 100d0)
              call FetchSeries(DFILE, "E"//trim(sexLabel(sex))//trim(ageGrpLabel2(ageGrp))//".Q", etemp)
              e(sex,ageGrp,48*4:sample(2)) = etemp(48*4:sample(2))
           else
              e(sex,17,:) = lc(sex,17,:) * (1d0 - ru(sex,ageGrp,:) / 100d0)
              call FetchSeries(DFILE, "E"//trim(sexLabel(sex))//trim(ageGrpLabel(17))//".Q", etemp)
              e(sex,17,48*4:sample(2)) = etemp(48*4:sample(2))
           end if
        end do
        e(sex,24,:) = e(sex,27,:) + e(sex,3,:) + sum(e(sex,18:22,:),dim=1)
        call FetchSeries(DFILE, "E"//trim(sexLabel(sex))//"16O.Q", etemp)
        e(sex,24,48*4:sample(2)) = etemp(48*4:sample(2))
      end do
      
      e(1:2,23,:) = e(1:2,13,:) + e(1:2,17,:)
      
      e(0,24,:) = e(1,24,:) + e(2,24,:)
      call FetchSeries(DFILE, "E16O.Q", etemp)
      e(0,24,48*4:sample(2)) = etemp(48*4:sample(2))
      
      e(1,0,:) = e(1,24,:)
      e(2,0,:) = e(2,24,:)
      e(0,0,:) = e(1,0,:) + e(2,0,:)
      ee = e(0,0,:) + edmil
      
      hrs = ee * ahrs * 0.052
      prod =  gdpreal / hrs
      ! From: Sinclair, Sven. Sent: Wednesday, 19 May, 2021 17:26. Subject: RE: OASI Reconciliation - Age 62 Prevalence.
      ! gdp12 = (e+edmil)*ahrs*prod*0.052    
      
      ! Annual
      do sex = 1, 2
        do ageGrp = 1, 27
           if (ageGrp > 14 .and. ageGrp < 18 .or. ageGrp > 22 .and. ageGrp < 27) cycle
           if (ageGrp /= 14) then
              e_a(sex,ageGrp,:) = lc_a(sex,ageGrp,:) * (1d0 - ru_a(sex,ageGrp,:) / 100d0)
              call FetchSeries(DFILE, "E"//trim(sexLabel(sex))//trim(ageGrpLabel2(ageGrp))//".A", etemp_a)
              e_a(sex,ageGrp,48:sample(2)) = etemp_a(48:sample(2))
           else
              e_a(sex,17,:) = lc_a(sex,17,:) * (1d0 - ru_a(sex,ageGrp,:) / 100d0)
              call FetchSeries(DFILE, "E"//trim(sexLabel(sex))//trim(ageGrpLabel(17))//".A", etemp_a)
              e(sex,17,48:sample(2)) = etemp(48:sample(2))
           end if
        end do
        e_a(sex,24,:) = e_a(sex,27,:) + e_a(sex,3,:) + sum(e_a(sex,18:22,:),dim=1)
        call FetchSeries(DFILE, "E"//trim(sexLabel(sex))//"16O.A", etemp_a)
        e_a(sex,24,48:sample(2)) = etemp_a(48:sample(2))
      end do
      
      e_a(1:2,23,:) = e_a(1:2,13,:) + e_a(1:2,17,:)
      
      e_a(0,24,:) = e_a(1,24,:) + e_a(2,24,:)
      call FetchSeries(DFILE, "E16O.A", etemp_a)
      e_a(0,24,48:sample(2)) = etemp_a(48:sample(2))
      
      e_a(1,0,:) = e_a(1,24,:)
      e_a(2,0,:) = e_a(2,24,:)
      e_a(0,0,:) = e_a(1,0,:) + e_a(2,0,:)      
      ee_a = e_a(0,0,:) + edmil_a
      
      hrs_a = ee_a * ahrs_a * 0.052
      prod_a =  gdpreal_a / hrs_a
      ! From: Sinclair, Sven. Sent: Wednesday, 19 May, 2021 17:26. Subject: RE: OASI Reconciliation - Age 62 Prevalence.
      ! gdp12 = (e+edmil)*ahrs*prod*0.052
      
   end subroutine CalculateEmployment

!===============================================================================

   subroutine CalculateValuesAtFullEmployment()
   
      integer :: sex, ageGrp
      
      ! Labor Force Participation Rate
      do sex = 1, 2
         do ageGrp = 1, 13
            p_fe(sex,ageGrp,:) = p(sex,ageGrp,:) + dp_fe(sex,ageGrp,:)
            p_fe(sex,24,:) = p_fe(sex,24,:) + p_fe(sex,ageGrp,:) * n(sex,ageGrp,:) / &
               n(sex,24,:)
         end do
         p_fe(sex,17,:) = p(sex,17,:) + dp_fe(sex,17,:)
         p_fe(sex,24,:) = p_fe(sex,24,:) + p_fe(sex,17,:) * n(sex,17,:) / &
            n(sex,24,:)
         p_fe(0,24,:) = p_fe(0,24,:) + p_fe(sex,24,:) * n(sex,24,:) / (n(1,24,:) + n(2,24,:))
      end do
      
      ! Civilian Labor Force
      do sex = 1, 2
         do ageGrp = 1, 13
            lc_fe(sex,ageGrp,:) = p_fe(sex,ageGrp,:) * n(sex,ageGrp,:)
            lc_fe(sex,24,:) = lc_fe(sex,24,:) + lc_fe(sex,ageGrp,:)
         end do
         lc_fe(sex,17,:) = p_fe(sex,17,:) * n(sex,17,:)
         lc_fe(sex,24,:) = lc_fe(sex,24,:) + lc_fe(sex,17,:)
         lc_fe(sex,0,:) = lc_fe(sex,24,:)
         lc_fe(0,24,:) = lc_fe(0,24,:) + lc_fe(sex,24,:)
      end do
      lc_fe(0,0,:) = lc_fe(0,24,:)
      
      ! Unemployment Rate
      do sex = 1, 2
         do ageGrp = 1, 13
            ru_fe(sex,24,:) = ru_fe(sex,24,:) + ru_fe(sex,ageGrp,:) * lc_fe(sex,ageGrp,:) &
               / lc_fe(sex,24,:)
         end do
         ru_fe(sex,24,:) = ru_fe(sex,24,:) + ru_fe(sex,14,:) * lc_fe(sex,17,:) &
            / lc_fe(sex,24,:)
         ru_fe(0,24,:) = ru_fe(0,24,:) + ru_fe(sex,24,:) * lc_fe(sex,24,:) &
            / lc_fe(0,24,:)
         ru_fe(sex,0,:) = ru_fe(sex,24,:)
      end do
      ru_fe(0,0,:) = ru_fe(0,24,:)
      
      ! Employment
      do sex = 1, 2
         do ageGrp = 1, 13
            e_fe(sex,ageGrp,:) = lc_fe(sex,ageGrp,:) * &
               (1d0 - ru_fe(sex,ageGrp,:) / 100d0)
            e_fe(sex,24,:) = e_fe(sex,24,:) + e_fe(sex,ageGrp,:)
         end do
         e_fe(sex,17,:) = lc_fe(sex,17,:) * &
            (1d0 - ru_fe(sex,14,:) / 100d0)
         e_fe(sex,24,:) = e_fe(sex,24,:) + e_fe(sex,17,:)
         e_fe(sex,0,:) = e_fe(sex,24,:)
         e_fe(0,24,:) = e_fe(0,24,:) + e_fe(sex,24,:)
      end do
      e_fe(0,0,:) = e_fe(0,24,:)
   
   end subroutine CalculateValuesAtFullEmployment

!===============================================================================

   subroutine ConvertFullEmploymentToAnnual()
   
      integer :: sex, ageGrp
   
      do sex = 1, 2
         do ageGrp = 1, 13
            call collapse(dp_fe_a(sex,ageGrp,:), dp_fe(sex,ageGrp,:))
            call collapse(dru_fe_a(sex,ageGrp,:), dru_fe(sex,ageGrp,:))
            call collapse(p_fe_a(sex,ageGrp,:), p_fe(sex,ageGrp,:))
            call collapse(lc_fe_a(sex,ageGrp,:), lc_fe(sex,ageGrp,:))
            call collapse(ru_fe_a(sex,ageGrp,:), ru_fe(sex,ageGrp,:))
            call collapse(e_fe_a(sex,ageGrp,:), e_fe(sex,ageGrp,:))
         end do
         call collapse(dp_fe_a(sex,17,:), dp_fe(sex,17,:))
         call collapse(dru_fe_a(sex,14,:), dru_fe(sex,14,:))
         call collapse(p_fe_a(sex,17,:), p_fe(sex,17,:))
         call collapse(lc_fe_a(sex,17,:), lc_fe(sex,17,:))
         call collapse(ru_fe_a(sex,14,:), ru_fe(sex,14,:))
         call collapse(e_fe_a(sex,17,:), e_fe(sex,17,:))
      end do
      
      do sex = 0, 2
         do ageGrp = 0, 24, 24
            call collapse(dp_fe_a(sex,ageGrp,:), dp_fe(sex,ageGrp,:))
            call collapse(dru_fe_a(sex,ageGrp,:), dru_fe(sex,ageGrp,:))
            if (ageGrp /= 0) call collapse(p_fe_a(sex,ageGrp,:), p_fe(sex,ageGrp,:))
            call collapse(lc_fe_a(sex,ageGrp,:), lc_fe(sex,ageGrp,:))
            call collapse(ru_fe_a(sex,ageGrp,:), ru_fe(sex,ageGrp,:))
            call collapse(e_fe_a(sex,ageGrp,:), e_fe(sex,ageGrp,:))
         end do
      end do

   end subroutine ConvertFullEmploymentToAnnual

!===============================================================================

   subroutine AgeSexAdjustSolution()
   
      integer :: sex, ageGrp, per11, a, b
      real (kind=8), dimension(MAX_QTR) :: lctemp
      
      call FetchSeries(DFILE, "LM1617_BY.Q", lctemp)
      per11 = sample(1)
      a = per11 + 4
      b = endQtr
      
      ! Unemployment Rate
      ru_asa(0:2,0,a:b) = 0d0
      do sex = 1, 2
         do ageGrp = 1, 13
            ru_asa(sex,0,a:b) = ru_asa(sex,0,a:b) + ru(sex,ageGrp,a:b) * &
               lc_by(sex,ageGrp,a:b) / lc_by(sex,0,a:b)
         end do
         ru_asa(sex,0,a:b) = ru_asa(sex,0,a:b) + ru(sex,14,a:b) * &
            lc_by(sex,17,a:b) / lc_by(sex,0,a:b)
         ru_asa(0,0,a:b) = ru_asa(0,0,a:b) + ru_asa(sex,0,a:b) * &
            lc_by(sex,0,a:b) / lc_by(0,0,a:b)
      end do
      
      ! Labor Force Participation Rate
      p_asa(0:2,24,a:b) = 0d0
      do sex = 1, 2
         do ageGrp = 1, 13
            p_asa(sex,24,a:b) = p_asa(sex,24,a:b) + p(sex,ageGrp,a:b) * &
               n_by(sex,ageGrp,a:b) / n_by(sex,24,a:b)
         end do
         p_asa(sex,24,a:b) = p_asa(sex,24,a:b) + p(sex,17,a:b) * &
            n_by(sex,17,a:b) / n_by(sex,24,:)
         p_asa(0,24,a:b) = p_asa(0,24,a:b) + p_asa(sex,24,a:b) * &
            n_by(sex,24,a:b) / n_by(0,24,a:b)
      end do
      
      do sex = 0, 2
         call collapse(ru_asa_a(sex,0,:), ru_asa(sex,0,:))
         call collapse(ru_asa_p_a(sex,0,:), ru_asa_p(sex,0,:))
         call collapse(p_asa_a(sex,24,:), p_asa(sex,24,:))
      end do
   
   end subroutine AgeSexAdjustSolution
      
!===============================================================================

   subroutine CalculateAverageGDP()
   
      avg_gdp = gdp / (e(0,0,:) + edmil)
      avg_gdp_a = gdp_a / (e_a(0,0,:) + edmil_a)

      ru(1:2,15,:) = (lc(1:2,23,:) - e(1:2,23,:)) / (lc(1:2,23,:) + epsilon(0d0)) * 100d0
      ru_a(1:2,15,:) = (lc_a(1:2,23,:) - e_a(1:2,23,:)) / (lc_a(1:2,23,:) + epsilon(0d0))* 100d0
   
   end subroutine CalculateAverageGDP

!===============================================================================

   subroutine DisplayWarning()
   
      integer :: i, a, b
      
      write(*,'(/4a12/)') " ", "ru_asa_adj", "ru_asa_p", "ru_asa"
      
      a = per2a * 4 - 16
      b = per2a * 4 + 3
      
      do i = a, b
         if (mod(i,4) == 0) write(*,'(i12)') 1900 + i / 4
         write(*,'(a11,i1,3f18.12)') "Q", mod(i,4) + 1 , ru_asa_adj(i), &
            ru_asa(0,0,i) - ru_asa_adj(i), ru_asa(0,0,i)
      end do
      
      write(*,'(///a///)') "Warning - If TR run, operator may want to calculate &
         &value for RU_ASA_ADJ so that RU_ASA is at assumed level."
   
   end subroutine DisplayWarning

!===============================================================================

   subroutine Abort()
   
      write(*,'(/a/)') "ModSol1 procedure aborted"
      stop
      
   end subroutine Abort

!===============================================================================

   subroutine Finish()
    
      write(*,'(/a/)') "ModSol1 procedure finished"
      
   end subroutine Finish

!===============================================================================

end module EconModSol1Mod
