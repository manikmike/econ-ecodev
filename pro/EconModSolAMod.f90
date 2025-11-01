module EconModSolAMod

   use EconParMod
   use EconLibMod
   use EconModSolAVarMod
   use EconModSolAOutMod
   use EconModSolAHistOutMod
   use EconModSolAEquationsMod
   use EconMiscMod
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSolAMain
   
   ! Targeted growth rate for wsw_mef_a:
   ! 2023: W-2 data. TY 2023. September ("awi") MEF extract.

   real (kind = 8), dimension(123:123), parameter :: &
      MULT2_TE_TARGET = (/ 0.95d0 /)
       
   integer :: passNum = 0
   integer :: pereyte ! Ending year of total employment data

contains

!===============================================================================

   subroutine EconModSolAMain()

      call ProfileCheckpoint(6)
      passNum = passNum + 1
      call InitModSolA()
      call PreliminaryCalcsA()
      call SolveModSolA()
      call CreateAdditionalVarA()
      call CalculateTotalEmployed()
      call CalculateSelfEmployedOnly()
      if (passNum == 8) then
         call coveredworkers()
         call histlcanndata()
      end if
      call EconModSolAOutMain()
      call EconModSolAHistOutMain()
      call Finish()
             
   end subroutine EconModSolAMain

!===============================================================================

   subroutine InitModSolA()
   
      integer :: sex, ageGrp

      if (passNum == 1) then
         call InitializeModSolAVars()
         call GetTrend()
         call FetchSeries(DFILE, "CPB.A", cpb_a(:))
         mult2_te(1:2,1:12,:) = 1d0
         mult2_te(1:2,23,:) = 1d0
         if (HI_TARGET == -1d0) then
            call CalculateHITaxableTarget()
         end if
      else
         ! Only a few series need to be reinitialized
         call ReInitializeModSolAVars()
      end if
      
   end subroutine InitModSolA

!===============================================================================

   subroutine CalculateHITaxableTarget()
   
      integer :: rv
      real (kind = 8), dimension(MAX_YR) :: temp
      integer :: qtr3, qtr4
      character (len=32) :: arg1, arg2
      
      qtr3 = (TRYR-1) * 4 + 2
      qtr4 = qtr3 + 1
      
      arg1 = DoublePrecToAsc(wsd(qtr3),3)
      arg2 = DoublePrecToAsc(wsd(qtr4),3)
      
      rv = ExecuteProgram(trim(DAT_PATH)//"\wsqprojtr.prg "//trim(arg1)//" "//trim(arg2))
      if (rv /= 0) then
         write(*,'(a)') " Error: EViews program wsqprojtr terminated abnormally"
         call SetWorkfileVerboseMode(.false.)
         call CloseAllFiles()
         stop
      end if

      WSQPROJTR =  OpenWorkfileVerbose("wsqprojtr.wf1")
      temp = 0
      if (HI_TARGET == -1d0) then 
          call FetchSeries(WSQPROJTR, "WSHIULT", temp)
          HI_TARGET = temp(TRYR-1)
      end if
      ! Leave WSQPROJTR workfile so we can store OASDI_TARGET later on  
   
   end subroutine CalculateHITaxableTarget
      
!===============================================================================

   subroutine PreliminaryCalcsA()
   
      ! Numbers below correspond to comments in modsola.13c
      call EstimateHistoricalTotalEmployment()    ! 1
      call ProjectPrivateHouseholdWorkers()       ! 2
      call ProjectStudents()                      ! 3
      call ProjectElectionWorkers()               ! 4
      call CopyHistoricalTotals()                 ! 5
      call AproximationHHEmp1b()                  ! 6
      call CalculateHistoricalAWIData()
      call CalcAdjCPBDueOTL()
      
   end subroutine PreliminaryCalcsA

!===============================================================================

   subroutine EstimateHistoricalTotalEmployment()
   
      integer :: i, ageGrp, sex
      real (kind = 8), dimension(MAX_YR) :: he_m_temp
      
      ! 1) Estimated historical values for age-sex TE
      
      call FetchSeries(MEF, "HE_M.A", he_m_temp)
      pereyte = sample(2)
      
      do i = 88, pereyte
         do sex = 1, 2
            do ageGrp = 1, 12
               te_mn(sex,ageGrp,i) = te_rro_m(sex,ageGrp,i) + te_sloo_m(sex,ageGrp,i) + &
                  te_slos_m(sex,ageGrp,i) + te_sloe_m(sex,ageGrp,i) + te_ps_m(sex,ageGrp,i) + &
                  te_ph_m(sex,ageGrp,i) + teo_asf1(sex,ageGrp,i) + teo_asj1(sex,ageGrp,i) + &
                  teo_awj(sex,ageGrp,i) + teo_awh(sex,ageGrp,i)
               te_m(sex,ageGrp,i) = te_mn(sex,ageGrp,i) + he_m(sex,ageGrp,i)
               te_s(sex,ageGrp,i) = tel_so(sex,ageGrp,i) + teo_esf(sex,ageGrp,i)
               te_u(sex,ageGrp,i) = teo_und(sex,ageGrp,i)
               te_a(sex,ageGrp,i) = te_m(sex,ageGrp,i) + te_s(sex,ageGrp,i) + te_u(sex,ageGrp,i)
            end do
            ageGrp = 23
            te_mn(sex,ageGrp,i) = te_rro_m(sex,ageGrp,i) + te_sloo_m(sex,ageGrp,i) + &
               te_slos_m(sex,ageGrp,i) + te_sloe_m(sex,ageGrp,i) + te_ps_m(sex,ageGrp,i) + &
               te_ph_m(sex,ageGrp,i) + teo_asf1(sex,ageGrp,i) + teo_asj1(sex,ageGrp,i) + &
               teo_awj(sex,ageGrp,i) + teo_awh(sex,ageGrp,i)
            te_m(sex,ageGrp,i) = te_mn(sex,ageGrp,i) + he_m(sex,ageGrp,i)
            te_s(sex,ageGrp,i) = tel_so(sex,ageGrp,i) + teo_esf(sex,ageGrp,i)
            te_u(sex,ageGrp,i) = teo_und(sex,ageGrp,i)
            te_a(sex,ageGrp,i) = te_m(sex,ageGrp,i) + te_s(sex,ageGrp,i) + te_u(sex,ageGrp,i)

            he_m_1013(sex,i) = sum(he_m_sy(sex,10:13,i))
            he_m_1415(sex,i) = sum(he_m_sy(sex,14:15,i))
            te_a(sex,24,i) = sum(te_a(sex,1:12,i)) + te_a(sex,23,i)
            te_a(sex,0,i) = sum(he_m_sy(sex,0:9,i)) + he_m_1013(sex,i) + he_m_1415(sex,i) + te_a(sex,24,i)
         end do
         te_a(0,0,i) = sum(te_a(1:2,0,i))
      end do
    
   end subroutine EstimateHistoricalTotalEmployment

!===============================================================================
   
   subroutine ProjectPrivateHouseholdWorkers()
   
      integer :: i, sex, ageGrp, age
      integer, dimension(15) :: lowAge = (/ 16, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80 /)
      integer, dimension(15) :: hiAge =  (/ 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84 /)
      
      ! 2) Esimated Projected values for Private Household Workers
  
      ! 2a) Projected values for TE_PH_M by 5-year age groups
      
      do i = pereyte, endYr
         do sex = 1, 2
            enawph_a(sex,4,i) = enawph_a(sex,18,i) * e_a(sex,4,i) / &
               sum(e_a(sex,4:5,i))
            enawph_a(sex,5,i) = enawph_a(sex,18,i) * e_a(sex,5,i) / &
               sum(e_a(sex,4:5,i))
            enawph_a(sex,6,i) = enawph_a(sex,19,i) * e_a(sex,6,i) / &
               sum(e_a(sex,6:7,i))
            enawph_a(sex,7,i) = enawph_a(sex,19,i) * e_a(sex,7,i) / &
               sum(e_a(sex,6:7,i))
            enawph_a(sex,8,i) = enawph_a(sex,20,i) * e_a(sex,8,i) / &
               sum(e_a(sex,8:9,i))
            enawph_a(sex,9,i) = enawph_a(sex,20,i) * e_a(sex,9,i) / &
               sum(e_a(sex,8:9,i))
            enawph_a(sex,10,i) = enawph_a(sex,21,i) * e_a(sex,10,i) / &
               sum(e_a(sex,10:11,i))
            enawph_a(sex,11,i) = enawph_a(sex,21,i) * e_a(sex,11,i) / &
               sum(e_a(sex,10:11,i))
            enawph_a(sex,12,i) = enawph_a(sex,22,i) * e_a(sex,12,i) / &
               (e_a(sex,12,i) + e_a(sex,23,i))
            enawph_a(sex,23,i) = enawph_a(sex,22,i) * e_a(sex,23,i) / &
               (e_a(sex,12,i) + e_a(sex,23,i))
         end do
      end do
         
      ! 2b) Distribute projected values for 5-year age groups to single-year-of-age
         
      do sex = 1, 2
         do i = pereyte+1, endYr
            do ageGrp = 1, 12
               te_ph_m(sex,ageGrp,i) = te_ph_m(sex,ageGrp,i-1) * &
                  enawph_a(sex,ageGrp,i) / enawph_a(sex,ageGrp,i-1)
            end do
            te_ph_m(sex,23,i) = te_ph_m(sex,23,i-1) * &
               enawph_a(sex,23,i) / enawph_a(sex,23,i-1)
         end do
      end do
      
      te_ph_m_sy(1:2,0:100,pereyte+1:endYr) = 0d0
      
      do i = 88, endYr
         lm75ox(i) = sum(lcsy_a(1,75:100,i))
         lf75ox(i) = sum(lcsy_a(2,75:100,i))
      end do
      
      do i = pereyte+1, endYr
         do sex = 1, 2
            do ageGrp = 1, 9
               do age = lowAge(ageGrp), hiAge(ageGrp)
                  te_ph_m_sy(sex,age,i) = te_ph_m(sex,ageGrp,i) * nsy_a(sex,age,i) / nssa(sex,ageGrp,i)
               end do
            end do
            do ageGrp = 10, 12
               do age = lowAge(ageGrp), hiAge(ageGrp)
                  te_ph_m_sy(sex,age,i) = te_ph_m(sex,ageGrp,i) * lcsy_a(sex,age,i) / lc_a(sex,ageGrp,i)
               end do
            end do
         end do
      end do
      
      do i = pereyte+1, endYr
         do age = 70, 79
            te_ph_m_sy(1,age,i) = te_ph_m(1,23,i) * lcsy_a(1,age,i) / (lc_a(1,13,i) + lm75ox(i))
            te_ph_m_sy(2,age,i) = te_ph_m(2,23,i) * lcsy_a(2,age,i) / (lc_a(2,13,i) + lf75ox(i))
         end do
      end do
      
      do i = pereyte+1, endYr
         do age = 80, 100
            te_ph_m_sy(1,age,i) = te_ph_m(1,23,i) * lcsy_a(1,age,i) / (lc_a(1,13,i) + lm75ox(i))
            te_ph_m_sy(2,age,i) = te_ph_m(2,23,i) * lcsy_a(2,age,i) / (lc_a(2,13,i) + lf75ox(i))
         end do
      end do
      
      ! 2c) Aggregate
      
      do i = 88, endYr
         do sex = 1, 2
            
            te_ph_m_0t4(sex,i) = sum(te_ph_m_sy(sex,0:4,i))
            te_ph_m_5t9(sex,i) = sum(te_ph_m_sy(sex,5:9,i))
            te_ph_m_1014(sex,i) = sum(te_ph_m_sy(sex,10:14,i))
            
            do ageGrp = 1, 15 ! 16-17, 18-19, 20-24, 25-29, ..., 80-84
               te_ph_m(sex,ageGrp,i) = sum(te_ph_m_sy(sex,lowAge(ageGrp):hiAge(ageGrp),i))
            end do
            
            te_ph_m(sex,32,i) = sum(te_ph_m_sy(sex,85:89,i))
            te_ph_m(sex,33,i) = sum(te_ph_m_sy(sex,90:94,i))
            te_ph_m(sex,34,i) = sum(te_ph_m_sy(sex,95:99,i))
            
            te_ph_m(sex,27,i) = te_ph_m(sex,1,i) + te_ph_m(sex,2,i) ! 16-19
            te_ph_m(sex,25,i) = sum(te_ph_m_sy(sex,60:61,i))
            te_ph_m(sex,26,i) = te_ph_m(sex,11,i) - te_ph_m(sex,25,i) ! 62-64
            te_ph_m(sex,16,i) = te_ph_m(sex,32,i) + te_ph_m(sex,33,i) + te_ph_m(sex,34,i) + te_ph_m_sy(sex,100,i) ! 85o
            te_ph_m(sex,31,i) = te_ph_m(sex,16,i) + te_ph_m(sex,15,i) ! 80o
            te_ph_m(sex,17,i) = te_ph_m(sex,31,i) + te_ph_m(sex,14,i) ! 75o
            te_ph_m(sex,23,i) = te_ph_m(sex,17,i) + te_ph_m(sex,13,i) ! 70o
            te_ph_m(sex,22,i) = te_ph_m(sex,23,i) + te_ph_m(sex,12,i) ! 65o
            te_ph_m(sex,24,i) = sum(te_ph_m(sex,1:11,i)) + te_ph_m(sex,22,i) ! 16o
            
            te_ph_m_15u(sex,i) = te_ph_m_0t4(sex,i)  + te_ph_m_5t9(sex,i) + te_ph_m_1014(sex,i)
            te_ph_m(sex,0,i) = te_ph_m_15u(sex,i) + te_ph_m(sex,24,i)
            
         end do
         te_ph_m(0,0,i) = te_ph_m(1,0,i) + te_ph_m(2,0,i)
      end do
    
   end subroutine ProjectPrivateHouseholdWorkers
   
!===============================================================================
   
   subroutine ProjectStudents()
   
      integer :: i, sex, ageGrp, age
      integer, dimension(15) :: lowAge = (/ 16, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80 /)
      integer, dimension(15) :: hiAge =  (/ 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84 /)
      
      ! 3) Estimate projected values for age-sex S&L and Private students
      
      ! 3a) Copy each age/sex student groups from MEF file for single-year-of age (0 to 100o) and
      !     set projected values to zero.
   
      te_slos_m_sy(1:2,0:100,pereyte+1:endYr) = 0d0
      te_ps_m_sy(1:2,0:100,pereyte+1:endYr) = 0d0
      
      ! 3b) Set projected values for those age (18 to 34)
      
      do i = pereyte+1, endYr
         do sex = 1, 2
            do age = 18, 34
               te_slos_m_sy(sex,age,i) = te_slos_m_sy(sex,age,i-1) * nsy_a(sex,age,i) / nsy_a(sex,age,i-1)
               te_ps_m_sy(sex,age,i) = te_ps_m_sy(sex,age,i-1) * nsy_a(sex,age,i) / nsy_a(sex,age,i-1)
            end do
         end do
      end do
      
      ! 3c) Aggregate
      
      do i = 88, endYr
         do sex = 1, 2
            
            te_slos_m_0t4(sex,i) = sum(te_slos_m_sy(sex,0:4,i))
            te_slos_m_5t9(sex,i) = sum(te_slos_m_sy(sex,5:9,i))
            te_slos_m_1014(sex,i) = sum(te_slos_m_sy(sex,10:14,i))
            
            do ageGrp = 1, 15 ! 16-17, 18-19, 20-24, 25-29, ..., 80-84
               te_slos_m(sex,ageGrp,i) = sum(te_slos_m_sy(sex,lowAge(ageGrp):hiAge(ageGrp),i))
            end do
            
            te_slos_m(sex,32,i) = sum(te_slos_m_sy(sex,85:89,i))
            te_slos_m(sex,33,i) = sum(te_slos_m_sy(sex,90:94,i))
            te_slos_m(sex,34,i) = sum(te_slos_m_sy(sex,95:99,i))
            
            te_slos_m(sex,27,i) = te_slos_m(sex,1,i) + te_slos_m(sex,2,i) ! 16-19
            te_slos_m(sex,25,i) = sum(te_slos_m_sy(sex,60:61,i))
            te_slos_m(sex,26,i) = te_slos_m(sex,11,i) - te_slos_m(sex,25,i) ! 62-64
            te_slos_m(sex,16,i) = te_slos_m(sex,32,i) + te_slos_m(sex,33,i) + te_slos_m(sex,34,i) + te_slos_m_sy(sex,100,i) ! 85o
            te_slos_m(sex,31,i) = te_slos_m(sex,16,i) + te_slos_m(sex,15,i) ! 80o
            te_slos_m(sex,17,i) = te_slos_m(sex,31,i) + te_slos_m(sex,14,i) ! 75o
            te_slos_m(sex,23,i) = te_slos_m(sex,17,i) + te_slos_m(sex,13,i) ! 70o
            te_slos_m(sex,22,i) = te_slos_m(sex,23,i) + te_slos_m(sex,12,i) ! 65o
            te_slos_m(sex,24,i) = sum(te_slos_m(sex,1:11,i)) + te_slos_m(sex,22,i) ! 16o
            
            te_slos_m_15u(sex,i) = te_slos_m_0t4(sex,i)  + te_slos_m_5t9(sex,i) + te_slos_m_1014(sex,i)
            te_slos_m(sex,0,i) = te_slos_m_15u(sex,i) + te_slos_m(sex,24,i)
            
         end do
         te_slos_m(0,0,i) = te_slos_m(1,0,i) + te_slos_m(2,0,i)
      end do

      do i = 88, endYr
         do sex = 1, 2
            
            te_ps_m_0t4(sex,i) = sum(te_ps_m_sy(sex,0:4,i))
            te_ps_m_5t9(sex,i) = sum(te_ps_m_sy(sex,5:9,i))
            te_ps_m_1014(sex,i) = sum(te_ps_m_sy(sex,10:14,i))
            
            do ageGrp = 1, 15 ! 16-17, 18-19, 20-24, 25-29, ..., 80-84
               te_ps_m(sex,ageGrp,i) = sum(te_ps_m_sy(sex,lowAge(ageGrp):hiAge(ageGrp),i))
            end do
            
            te_ps_m(sex,32,i) = sum(te_ps_m_sy(sex,85:89,i))
            te_ps_m(sex,33,i) = sum(te_ps_m_sy(sex,90:94,i))
            te_ps_m(sex,34,i) = sum(te_ps_m_sy(sex,95:99,i))
            
            te_ps_m(sex,27,i) = te_ps_m(sex,1,i) + te_ps_m(sex,2,i) ! 16-19
            te_ps_m(sex,25,i) = sum(te_ps_m_sy(sex,60:61,i))
            te_ps_m(sex,26,i) = te_ps_m(sex,11,i) - te_ps_m(sex,25,i) ! 62-64
            te_ps_m(sex,16,i) = te_ps_m(sex,32,i) + te_ps_m(sex,33,i) + te_ps_m(sex,34,i) + te_ps_m_sy(sex,100,i) ! 85o
            te_ps_m(sex,31,i) = te_ps_m(sex,16,i) + te_ps_m(sex,15,i) ! 80o
            te_ps_m(sex,17,i) = te_ps_m(sex,31,i) + te_ps_m(sex,14,i) ! 75o
            te_ps_m(sex,23,i) = te_ps_m(sex,17,i) + te_ps_m(sex,13,i) ! 70o
            te_ps_m(sex,22,i) = te_ps_m(sex,23,i) + te_ps_m(sex,12,i) ! 65o
            te_ps_m(sex,24,i) = sum(te_ps_m(sex,1:11,i)) + te_ps_m(sex,22,i) ! 16o
            
            te_ps_m_15u(sex,i) = te_ps_m_0t4(sex,i)  + te_ps_m_5t9(sex,i) + te_ps_m_1014(sex,i)
            te_ps_m(sex,0,i) = te_ps_m_15u(sex,i) + te_ps_m(sex,24,i)
            
         end do
         te_ps_m(0,0,i) = te_ps_m(1,0,i) + te_ps_m(2,0,i)
      end do
    
   end subroutine ProjectStudents

!===============================================================================
   
   subroutine ProjectElectionWorkers()
   
      integer :: i, sex, ageGrp, age
      integer, dimension(15) :: lowAge = (/ 16, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80 /)
      integer, dimension(15) :: hiAge =  (/ 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84 /)
      
      ! 4) Estimate projected values for age-sex S&L and Election Workers
      
      ! 4a) Set aggregate values to grow with population age 16 and over
      
      do i = pereyte+1, endYr
         te_sloe_m(0,0,i) = te_sloe_m(0,0,i-1) * sum(nsy_a(1:2,16:100,i)) / sum(nsy_a(1:2,16:100,i-1))
      end do
      
      ! 4b) Distribute aggregate to single year of age

      ! 4b1) Copy each age/sex student groups from MEF file for single-year-of age (0 to 100o) and
      !      set projected values to zero.
      
      te_sloe_m_sy(1:2,0:100,pereyte+1:endYr) = 0d0
      
      ! 4b2) Estimate  age-sex distribution of the SSA area population age 60 to 79
      
      do i = 88, endYr
         n6079(i) = sum(nsy_a(1:2,60:79,i))
      end do
      
      do i = pereyte+1, endYr
         do sex = 1, 2
            do age = 60, 79
               te_sloe_m_sy(sex,age,i) = nsy_a(sex,age,i) / n6079(i) * te_sloe_m(0,0,i) 
            end do
         end do
      end do
      
      ! 4c) Aggregate
      
      do i = 88, endYr
         do sex = 1, 2
            
            te_sloe_m_0t4(sex,i) = sum(te_sloe_m_sy(sex,0:4,i))
            te_sloe_m_5t9(sex,i) = sum(te_sloe_m_sy(sex,5:9,i))
            te_sloe_m_1014(sex,i) = sum(te_sloe_m_sy(sex,10:14,i))
            
            do ageGrp = 1, 15 ! 16-17, 18-19, 20-24, 25-29, ..., 80-84
               te_sloe_m(sex,ageGrp,i) = sum(te_sloe_m_sy(sex,lowAge(ageGrp):hiAge(ageGrp),i))
            end do
            
            te_sloe_m(sex,32,i) = sum(te_sloe_m_sy(sex,85:89,i))
            te_sloe_m(sex,33,i) = sum(te_sloe_m_sy(sex,90:94,i))
            te_sloe_m(sex,34,i) = sum(te_sloe_m_sy(sex,95:99,i))
            
            te_sloe_m(sex,27,i) = te_sloe_m(sex,1,i) + te_sloe_m(sex,2,i) ! 16-19
            te_sloe_m(sex,25,i) = sum(te_sloe_m_sy(sex,60:61,i))
            te_sloe_m(sex,26,i) = te_sloe_m(sex,11,i) - te_sloe_m(sex,25,i) ! 62-64
            te_sloe_m(sex,16,i) = te_sloe_m(sex,32,i) + te_sloe_m(sex,33,i) + te_sloe_m(sex,34,i) + te_sloe_m_sy(sex,100,i) ! 85o
            te_sloe_m(sex,31,i) = te_sloe_m(sex,16,i) + te_sloe_m(sex,15,i) ! 80o
            te_sloe_m(sex,17,i) = te_sloe_m(sex,31,i) + te_sloe_m(sex,14,i) ! 75o
            te_sloe_m(sex,23,i) = te_sloe_m(sex,17,i) + te_sloe_m(sex,13,i) ! 70o
            te_sloe_m(sex,22,i) = te_sloe_m(sex,23,i) + te_sloe_m(sex,12,i) ! 65o
            te_sloe_m(sex,24,i) = sum(te_sloe_m(sex,1:11,i)) + te_sloe_m(sex,22,i) ! 16o
            
            te_sloe_m_15u(sex,i) = te_sloe_m_0t4(sex,i)  + te_sloe_m_5t9(sex,i) + te_sloe_m_1014(sex,i)
            te_sloe_m(sex,0,i) = te_sloe_m_15u(sex,i) + te_sloe_m(sex,24,i)
            
         end do
         te_sloe_m(0,0,i) = te_sloe_m(1,0,i) + te_sloe_m(2,0,i)
      end do
    
   end subroutine ProjectElectionWorkers

!===============================================================================
   
   subroutine CopyHistoricalTotals()
   
      integer :: i, sex
   
      ! 5) Copy misc.historical totals
      
      do i = 81, endYr
         
         te_u(0,0,i) = teo_und(0,0,i)
         te_s(0,0,i) = tel_so(0,0,i) + teo_esf(0,0,i)
         te_m(0,0,i) = te_a(0,0,i) - te_u(0,0,i) - te_s(0,0,i)
         te_mn(0,0,i) = te_rro_m(0,0,i) + te_sloo_m(0,0,i) + &
            te_slos_m(0,0,i) + te_sloe_m(0,0,i) + te_ps_m(0,0,i) + &
            te_ph_m(0,0,i) + teo_asf1(0,0,i) + teo_asj1(0,0,i) + &
            teo_awj(0,0,i) + teo_awh(0,0,i)
         
         tceahi_a(i) = he_m(0,0,i) + te_s(0,0,i)
         tcea_a(i) =ce_m(0,0,i) + te_s(0,0,i)
         wsw_hio_oth_se_a(i) = he_wosr_m(0,0,i)
         tefc_n_n_se_a(i) = he_wosf_m(0,0,i)
         tesl_n_n_hi_se_a(i) = he_wosl_m(0,0,i)
         wsw_hio_oth_a(i) = he_wor_m(0,0,i)
         
         seo_a(0,0,i) = ceso_m(0,0,i)
         cmb_a(i) = cesw_m(i)
         csw_a(i) = ces_m(i)
         wswa_a(i) = cew_m(0,0,i) + te_s(0,0,i)
         wswahi_a(i) = hew_m(0,0,i) + te_s(0,0,i)
         
         ! Note: 12/10/13
         ! We are adding CMB_OVER to the total of OASDI combination workers but not we are not adding any additional
         ! workers on an age-sex basis.
         
         cmb_tot_a(i) = cesw_m(i) + cmb_over(i)
         csw_tot_a(i) = seo_a(0,0,i) + cmb_tot_a(i)
         seo_hi_a(i) = heso_m(0,0,i)
         cmb_hi_a(i) =  csw_tot_a(i) - seo_hi_a(i)
         csw_hi_a(i) = seo_hi_a(i) + cmb_hi_a(i)
         wsw_mef_o_a(i) = cew_m(0,0,i)
         wsw_mef_a(i) = hew_m(0,0,i) + te_mn(0,0,i)
         
      end do
      
      do i = 81, endYr
         do sex = 1, 2
            he_m_1013(sex,i) = sum(he_m_sy(sex,10:13,i))
            he_m_1415(sex,i) = sum(he_m_sy(sex,14:15,i))
            he_m_15u(sex,i) = sum(he_m_sy(sex,0:9,i)) +  he_m_1013(sex,i) + he_m_1415(sex,i)
            ce_m_1013(sex,i) = sum(ce_m_sy(sex,10:13,i))
            ce_m_1415(sex,i) = sum(ce_m_sy(sex,14:15,i))
            ce_m_15u(sex,i) = sum(ce_m_sy(sex,0:9,i)) +  ce_m_1013(sex,i) + ce_m_1415(sex,i)
         end do
      end do

   end subroutine CopyHistoricalTotals

!===============================================================================
   
   subroutine AproximationHHEmp1b()
    
      integer :: i, sex
      
      ! 6) Approximation of HH Emp. Unpaid Family Workers by 5-year age groups
      !    for those aged 25 and over
   
      do i = startYr, endYr
         
         do sex = 1, 2
            enau_a(sex,27,i) = enau_a(sex,1,i) + enau_a(sex,2,i)
            enau_a(sex,4,i) = enau_a(sex,18,i) * e_a(sex,4,i) / e_a(sex,18,i)
            enau_a(sex,5,i) = enau_a(sex,18,i) * e_a(sex,5,i) / e_a(sex,18,i)
            enau_a(sex,6,i) = enau_a(sex,19,i) * e_a(sex,6,i) / e_a(sex,19,i)
            enau_a(sex,7,i) = enau_a(sex,19,i) * e_a(sex,7,i) / e_a(sex,19,i)
            enau_a(sex,8,i) = enau_a(sex,20,i) * e_a(sex,8,i) / e_a(sex,20,i)
            enau_a(sex,9,i) = enau_a(sex,20,i) * e_a(sex,9,i) / e_a(sex,20,i)
            enau_a(sex,10,i) = enau_a(sex,21,i) * e_a(sex,10,i) / e_a(sex,21,i)
            enau_a(sex,11,i) = enau_a(sex,21,i) * e_a(sex,11,i) / e_a(sex,21,i)
            enau_a(sex,12,i) = enau_a(sex,22,i) * e_a(sex,12,i) / e_a(sex,22,i)
            enau_a(sex,23,i) = enau_a(sex,22,i) * e_a(sex,23,i) / e_a(sex,22,i)

            eau_a(sex,27,i) = eau_a(sex,1,i) + eau_a(sex,2,i)
            eau_a(sex,4,i) = eau_a(sex,18,i) * e_a(sex,4,i) / e_a(sex,18,i)
            eau_a(sex,5,i) = eau_a(sex,18,i) * e_a(sex,5,i) / e_a(sex,18,i)
            eau_a(sex,6,i) = eau_a(sex,19,i) * e_a(sex,6,i) / e_a(sex,19,i)
            eau_a(sex,7,i) = eau_a(sex,19,i) * e_a(sex,7,i) / e_a(sex,19,i)
            eau_a(sex,8,i) = eau_a(sex,20,i) * e_a(sex,8,i) / e_a(sex,20,i)
            eau_a(sex,9,i) = eau_a(sex,20,i) * e_a(sex,9,i) / e_a(sex,20,i)
            eau_a(sex,10,i) = eau_a(sex,21,i) * e_a(sex,10,i) / e_a(sex,21,i)
            eau_a(sex,11,i) = eau_a(sex,21,i) * e_a(sex,11,i) / e_a(sex,21,i)
            eau_a(sex,12,i) = eau_a(sex,22,i) * e_a(sex,12,i) / e_a(sex,22,i)
            eau_a(sex,23,i) = eau_a(sex,22,i) * e_a(sex,23,i) / e_a(sex,22,i)

            enas_a(sex,27,i) = enas_a(sex,1,i) + enas_a(sex,2,i)
            enas_a(sex,4,i) = enas_a(sex,18,i) * e_a(sex,4,i) / e_a(sex,18,i)
            enas_a(sex,5,i) = enas_a(sex,18,i) * e_a(sex,5,i) / e_a(sex,18,i)
            enas_a(sex,6,i) = enas_a(sex,19,i) * e_a(sex,6,i) / e_a(sex,19,i)
            enas_a(sex,7,i) = enas_a(sex,19,i) * e_a(sex,7,i) / e_a(sex,19,i)
            enas_a(sex,8,i) = enas_a(sex,20,i) * e_a(sex,8,i) / e_a(sex,20,i)
            enas_a(sex,9,i) = enas_a(sex,20,i) * e_a(sex,9,i) / e_a(sex,20,i)
            enas_a(sex,10,i) = enas_a(sex,21,i) * e_a(sex,10,i) / e_a(sex,21,i)
            enas_a(sex,11,i) = enas_a(sex,21,i) * e_a(sex,11,i) / e_a(sex,21,i)
            enas_a(sex,12,i) = enas_a(sex,22,i) * e_a(sex,12,i) / e_a(sex,22,i)
            enas_a(sex,23,i) = enas_a(sex,22,i) * e_a(sex,23,i) / e_a(sex,22,i)

            eas_a(sex,27,i) = eas_a(sex,1,i) + eas_a(sex,2,i)
            eas_a(sex,4,i) = eas_a(sex,18,i) * e_a(sex,4,i) / e_a(sex,18,i)
            eas_a(sex,5,i) = eas_a(sex,18,i) * e_a(sex,5,i) / e_a(sex,18,i)
            eas_a(sex,6,i) = eas_a(sex,19,i) * e_a(sex,6,i) / e_a(sex,19,i)
            eas_a(sex,7,i) = eas_a(sex,19,i) * e_a(sex,7,i) / e_a(sex,19,i)
            eas_a(sex,8,i) = eas_a(sex,20,i) * e_a(sex,8,i) / e_a(sex,20,i)
            eas_a(sex,9,i) = eas_a(sex,20,i) * e_a(sex,9,i) / e_a(sex,20,i)
            eas_a(sex,10,i) = eas_a(sex,21,i) * e_a(sex,10,i) / e_a(sex,21,i)
            eas_a(sex,11,i) = eas_a(sex,21,i) * e_a(sex,11,i) / e_a(sex,21,i)
            eas_a(sex,12,i) = eas_a(sex,22,i) * e_a(sex,12,i) / e_a(sex,22,i)
            eas_a(sex,23,i) = eas_a(sex,22,i) * e_a(sex,23,i) / e_a(sex,22,i)
         end do
         
      end do
          
   end subroutine AproximationHHEmp1b      

!===============================================================================
   
   
   subroutine CalculateHistoricalAWIData()
   
      integer :: i
      
      do i = 81, TRYR - 3
         
         wsw_mef_a(i) = hew_m(0,0,i) + te_mn(0,0,i)

         ws_mef_a(i) = wscahi_a(i) - we_sf_a(i) + wsprrb_a(i) + wesl_n_nhi_a(i) + &
               (wesl_n_nhi_s_a(i) /tesl_n_s_a(i)) *  te_ps_m(0,0,i) + &
               (0.5d0 * 1.800d0 / 44.32167d0) * (aiw_a(i-1) / 1d3 * prod_a(i) / prod_a(i-1) * ahrs_a(i) / ahrs_a(i-1) * pgdp_a(i) / pgdp_a(i-1)) * te_ph_m(0,0,i) + &
               ws_eo_asf1(i) + ws_eo_asj1(i) + ws_eo_awj(i) + ws_eo_awh(i)
         
         aws_mef_a(i) = ws_mef_a(i) / wsw_mef_a(i)
         
      end do
   
   end subroutine CalculateHistoricalAWIData
   
!===============================================================================   

   subroutine CalcAdjCPBDueOTL()

      integer :: i, j, cpbper1a, cpbper1b, cpbper1a_lag, last_hist
      real (kind = 8) :: diff = 0d0
      real (kind = 8) :: diff_lag = 0d0
      real (kind = 8) :: modwscahi_a = 0d0
      ! HI_TARGET is updated by Tony based on WSD in the 4th quarter of TRYEAR-1
      ! It's value is now defined in the config####.txt file
      
      cpbper1a = CPB_TARGET_YEAR - 1900
      cpbper1b = cpbper1a + 1
      cpbper1a_lag = cpbper1a - 1
      last_hist = cpbper1a - 2
      
      cpb_a(cpbper1a_lag) = cpb_a(last_hist)
      
      ! Adjusts cpb in year before Tony's target so that model is consistent with Tony's estimate
      if (passNum > 1) then 
        modwscahi_a = wsca_a(cpbper1a_lag) + wefc_n_a(cpbper1a_lag) + wesl_n_hi_a(cpbper1a_lag) + &
                  wsca_hio_oth_a(cpbper1a_lag)
        diff_lag = wscahi_a(cpbper1a_lag) - modwscahi_a
        wspb_o_a(cpbper1a_lag) = wspb_o_a(cpbper1a_lag) + diff_lag
        cpb_a(cpbper1a_lag) = wspb_o_a(cpbper1a_lag)/wsdpb_a(cpbper1a_lag)
      endif
      
      ! Tony's target is used for cpb_a projection
      do i = cpbper1a, endYr
         if (passNum == 1) then
            tips_sr_a(i) = (0.000508328d0 * rtp_a(i) - 0.000481700d0) * gdp_a(i) * 1.26393d0 + tips_sr_add_a(i)
            wsdpb_a(i) = wsdp_a(i) - wsph_a(i) - wspf_a(i) - wsprrb_a(i) - tips_sr_a(i)
            cpb_a(i) = cpb_a(last_hist)
         end if
         wsdpb_xeo(i) = wsdpb_a(i) - ws_eo(i)
         if (i == cpbper1a) then
            if (passNum > 1) then
               diff = (HI_TARGET - wscahi_a(i))
               wspb_o_a(i) = wspb_o_a(i) + diff
               cpb_a(i) = wspb_o_a(i) / wsdpb_a(i)
            end if               
            wspb_o_xeo(i) = wsdpb_a(i) * cpb_a(i) - ws_eo_mefc(i) - ws_eo_esf(i)
            cpb_xeo(i) = wspb_o_xeo(i) / wsdpb_xeo(i)
            cpb_xeo(cpbper1a+1:126) = cpb_xeo(cpbper1a)
            do j = 127, 134
               cpb_xeo(j) = cpb_xeo(j-1) * 0.999
            end do
            cpb_xeo(135:endYr) = cpb_xeo(134)
         end if
         wspb_o_a(i) = cpb_xeo(i) * wsdpb_xeo(i) + ws_eo_mefc(i) + ws_eo_esf(i)
         cpb_a(i) = wspb_o_a(i) / wsdpb_a(i)
      end do
      
      !write(*,'(3f24.16)') cpb_a(cpbper1a_lag), modwscahi_a, wscahi_a(cpbper1a_lag), diff_lag
      !write(6,'(3f24.16)') HI_TARGET, wscahi_a(cpbper1a), diff
      

   end subroutine CalcAdjCPBDueOTL      

!===============================================================================

   subroutine SolveModSolA()
   
      integer :: i, yr, yr1, yr2
   
      write(*,'(//a,i0,a//)') "Solving ModSolA (pass ",passNum,"), Please Wait..."
      call EconModSolAEquationsMain()
      
      if (passNum < 8) then
         
         yr1 = lbound(MULT2_TE_TARGET, dim=1)
         yr2 = ubound(MULT2_TE_TARGET, dim=1)
         do i = 1, 8
            do yr = yr1, yr2
               mult2_te(1:2,1:12,yr) = mult2_te(1:2,1:12,yr) * &
                  (1 + (MULT2_TE_TARGET(yr) / 100d0 - (wsw_mef_a(yr) - wsw_mef_a(yr-1)) / wsw_mef_a(yr-1)))
               mult2_te(1:2,23,yr) = mult2_te(1:2,23,yr) * &
                  (1 + (MULT2_TE_TARGET(yr) / 100d0 - (wsw_mef_a(yr) - wsw_mef_a(yr-1)) / wsw_mef_a(yr-1)))
            end do
            call EconModSolAEquationsMain()
         end do

      end if
   
   end subroutine SolveModSolA

!===============================================================================

   subroutine CreateAdditionalVarA()
   
      integer :: i

! Please note that some variables are set to zero to match, but you might not want
! that.
   
      do i = 47, endYr 
         ena_a(i) = e_a(0,0,i) - ea_a(i)
         if (i >= 67) then
            enau_a(0,0,i) = enau_a(1,0,i) + enau_a(2,0,i) 
            enaw_a(i) = ena_a(i) - enas_a(0,0,i) - enau_a(0,0,i) 
            eau_a(0,0,i) = eau_a(1,0,i) + eau_a(2,0,i) 
            enawp_a(i) = enaw_a(i) - eggesl_a(i) - eggefc_a(i)
         else
            enau_a(0,0,i) = 0.0d0 
            enaw_a(i) = 0.0d0 
            eau_a(0,0,i) = 0.0d0 
         end if
         if (i == 105 .or. i == 106) then
            !enau_a(0,0,i) = 0.0d0 
            !enaw_a(i) = 0d0 
            !eau_a(0,0,i) = 0d0 
            !enawp_a(i) = 0d0
         end if
         if (i >= 48) then
            wsgge_a(i) = wsggesl_a(i) + wsggefc_a(i) + wsgfm_a(i)
         else
             wsgge_a(i) = 0.0d0
        end if
         if (i >= 71) then
            wsgca_a(i) = wsgslca_a(i) + wsgfca_a(i) + wsgmlc_a(i)
         end if
         y_a(i) = ynf_a(i) + yf_a(i)
         if (i >= 72) then         
            acwadiff_a(i) = (acwa_a(i) / acwa_a(i-1) - 1.0d0) * 100.0d0 - &
               (cpiwdec3_u_a(i) / cpiwdec3_u_a(i-1) - 1.0d0) * 100.0d0
         end if
         if (i >= 71) then         
            ws_n_nhi_a(i) = ws_a(i) - wscahi_a(i)
            wsp_n_nhi_a(i) = ws_n_nhi_a(i) - (wsgfm_a(i) - wsgmlc_a(i)) - &
               wesl_n_nhi_a(i)
            wspo_n_nhi_a(i) = wsp_n_nhi_a(i) - (wsph_a(i) -  &
               wsph_o_a(i)) - wsprr_a(i)
         end if
            
       end do
      
   end subroutine CreateAdditionalVarA

!===============================================================================
   
   subroutine CalculateTotalEmployed()
   
      integer :: i, sex, ageGrp
   
      do i =81, endYr
         do sex = 1, 2
            do ageGrp = 1, 12
               te_u(sex,ageGrp,i) = teo_und(sex,ageGrp,i)
               if (i >= pereyte+1) then
                  tel_so(sex,ageGrp,i) = tel_so(0,0,i) * &
                     (te_a(sex,ageGrp,i) - teo_esf(sex,ageGrp,i) - teo_und(sex,ageGrp,i)) / &
                     (te_a(1,24,i) + te_a(2,24,i) - teo_esf(1,24,i) - teo_esf(2,24,i) - teo_und(1,24,i) - teo_und(2,24,i))
               end if
               te_s(sex,ageGrp,i) = tel_so(sex,ageGrp,i) + teo_esf(sex,ageGrp,i)

               te_m(sex,ageGrp,i) = te_a(sex,ageGrp,i) - te_u(sex,ageGrp,i) - te_s(sex,ageGrp,i)
               te_mn(sex,ageGrp,i) = te_rro_m(sex,ageGrp,i) + te_sloo_m(sex,ageGrp,i) + &
                  te_slos_m(sex,ageGrp,i) + te_sloe_m(sex,ageGrp,i) + te_ps_m(sex,ageGrp,i) + &
                  te_ph_m(sex,ageGrp,i) + teo_asf1(sex,ageGrp,i) + teo_asj1(sex,ageGrp,i) + &
                  teo_awj(sex,ageGrp,i) + teo_awh(sex,ageGrp,i)
               if (i >= pereyte+1) then
                  he_m(sex,ageGrp,i) = te_m(sex,ageGrp,i) - te_mn(sex,ageGrp,i)

                  he_wol_m(sex,ageGrp,i) = he_wol_m(0,0,i) * he_m(sex,ageGrp,i) / (he_m(0,0,i) - (te_a(0,0,i) - te_a(1,24,i) - te_a(2,24,i)))
                  he_wor_m(sex,ageGrp,i) = 0d0 ! he_wor_m(0,0,i) * he_m(sex,ageGrp,i) / (he_m(0,0,i) - (te_a(0,0,i) - te_a(1,24,i) - te_a(2,24,i)))

                  he_wosl_m(sex,ageGrp,i) = he_wol_m(sex,ageGrp,i) * he_wosl_m(sex,ageGrp,i-1) / he_wol_m(sex,ageGrp,i-1)
                  he_wosr_m(sex,ageGrp,i) = he_wor_m(sex,ageGrp,i) * he_wosr_m(sex,ageGrp,i-1) / (he_wor_m(sex,ageGrp,i-1) + epsilon(0d0))
               end if
            end do
            ageGrp = 23
            te_u(sex,ageGrp,i) = teo_und(sex,ageGrp,i)
            if (i >= pereyte+1) then
               tel_so(sex,ageGrp,i) = tel_so(0,0,i) * &
                  (te_a(sex,ageGrp,i) - teo_esf(sex,ageGrp,i) - teo_und(sex,ageGrp,i)) / &
                  (te_a(1,24,i) + te_a(2,24,i) - teo_esf(1,24,i) - teo_esf(2,24,i) - teo_und(1,24,i) - teo_und(2,24,i))
            end if
            te_s(sex,ageGrp,i) = tel_so(sex,ageGrp,i) + teo_esf(sex,ageGrp,i)
            te_m(sex,ageGrp,i) = te_a(sex,ageGrp,i) - te_u(sex,ageGrp,i) - te_s(sex,ageGrp,i)
            te_mn(sex,ageGrp,i) = te_rro_m(sex,ageGrp,i) + te_sloo_m(sex,ageGrp,i) + &
               te_slos_m(sex,ageGrp,i) + te_sloe_m(sex,ageGrp,i) + te_ps_m(sex,ageGrp,i) + &
               te_ph_m(sex,ageGrp,i) + teo_asf1(sex,ageGrp,i) + teo_asj1(sex,ageGrp,i) + &
               teo_awj(sex,ageGrp,i) + teo_awh(sex,ageGrp,i)
            if (i >= pereyte+1) then
               he_m(sex,ageGrp,i) = te_m(sex,ageGrp,i) - te_mn(sex,ageGrp,i)

               he_wol_m(sex,ageGrp,i) = he_wol_m(0,0,i) * he_m(sex,ageGrp,i) / (he_m(0,0,i) - (te_a(0,0,i) - te_a(1,24,i) - te_a(2,24,i)))
               he_wor_m(sex,ageGrp,i) = 0d0 ! he_wor_m(0,0,i) * he_m(sex,ageGrp,i) / (he_m(0,0,i) - (te_a(0,0,i) - te_a(1,24,i) - te_a(2,24,i)))

               he_wosl_m(sex,ageGrp,i) = he_wol_m(sex,ageGrp,i) * he_wosl_m(sex,ageGrp,i-1) / he_wol_m(sex,ageGrp,i-1)
               he_wosr_m(sex,ageGrp,i) = he_wor_m(sex,ageGrp,i) * he_wosr_m(sex,ageGrp,i-1) / (he_wor_m(sex,ageGrp,i-1) + epsilon(0d0))
            end if
         end do
      end do
      
      ! Control the sum of the age-sex components for HE_WOSL_M and HE_WOSR_M to sum to total.
      
      do i = 81, endYr
         he_wosl_m(1,24,i) = sum(he_wosl_m(1,1:12,i)) + he_wosl_m(1,23,i)
         he_wosr_m(1,24,i) = sum(he_wosr_m(1,1:12,i)) + he_wosr_m(1,23,i)
         he_wosl_m(2,24,i) = sum(he_wosl_m(2,1:12,i)) + he_wosl_m(2,23,i)
         he_wosr_m(2,24,i) = sum(he_wosr_m(2,1:12,i)) + he_wosr_m(2,23,i)
      end do
      
      do i = pereyte+1, endYr
         do sex = 1, 2
            do ageGrp = 1, 12
               he_wosl_m(sex,ageGrp,i) = he_wosl_m(sex,ageGrp,i) * he_wosl_m(0,0,i) / ( he_wosl_m(1,24,i) +  he_wosl_m(2,24,i))
               he_wosr_m(sex,ageGrp,i) = he_wosr_m(sex,ageGrp,i) * he_wosr_m(0,0,i) / ( he_wosr_m(1,24,i) +  he_wosr_m(2,24,i) + epsilon(0d0))
            end do
            ageGrp = 23
            he_wosl_m(sex,ageGrp,i) = he_wosl_m(sex,ageGrp,i) * he_wosl_m(0,0,i) / ( he_wosl_m(1,24,i) +  he_wosl_m(2,24,i))
            he_wosr_m(sex,ageGrp,i) = he_wosr_m(sex,ageGrp,i) * he_wosr_m(0,0,i) / ( he_wosr_m(1,24,i) +  he_wosr_m(2,24,i) + epsilon(0d0))
         end do
      end do
      
      do i = pereyte+1, endYr
         do sex = 1, 2
            do ageGrp = 1, 12
               ce_m(sex,ageGrp,i) = he_m(sex,ageGrp,i) - &
                  ((he_wof_m(sex,ageGrp,i) + he_wol_m(sex,ageGrp,i) + he_wor_m(sex,ageGrp,i)) - &
                  (he_wosf_m(sex,ageGrp,i) + he_wosl_m(sex,ageGrp,i) + he_wosr_m(sex,ageGrp,i)))
            end do
            ageGrp = 23
            ce_m(sex,ageGrp,i) = he_m(sex,ageGrp,i) - &
               ((he_wof_m(sex,ageGrp,i) + he_wol_m(sex,ageGrp,i) + he_wor_m(sex,ageGrp,i)) - &
               (he_wosf_m(sex,ageGrp,i) + he_wosl_m(sex,ageGrp,i) + he_wosr_m(sex,ageGrp,i)))
         end do
      end do
      
      do i = 81, endYr
         do sex = 1, 2
            do ageGrp = 1, 12
               ce_a(sex,ageGrp,i) = ce_m(sex,ageGrp,i) + te_s(sex,ageGrp,i)
            end do
            ageGrp = 23
            ce_a(sex,ageGrp,i) = ce_m(sex,ageGrp,i) + te_s(sex,ageGrp,i)
         end do
      end do      

   end subroutine CalculateTotalEmployed

!===============================================================================   
   
   subroutine CalculateSelfEmployedOnly()
   
      integer :: i, sex, ageGrp
      real (kind = 8) :: ceso_m_p
    
      !   Calculation of SEO by age-sex

      ! The percentage of SE only workers under age 16 is based on the 1996
      !   CWHS count of these workers ratioed to the total 1996 historical value for SEO:
      !   1996 SEO = 8.42 million
      !   1996 SEO males under 16 = 10,000
      !   1996 SEO females under 16 = 6,500
      
      do i = pereyte+1, endYr
      
         ceso_m_sy(1,0,i) = ce_m_sy(1,0,i) * 0.0d0
         ceso_m_sy(1,1,i) = ce_m_sy(1,1,i) * (0.0d0 / 300.0d0)
         ceso_m_sy(1,2,i) = ce_m_sy(1,2,i) * 0.0d0
         ceso_m_sy(1,3,i) = ce_m_sy(1,3,i) * (0.0d0 / 800.0d0)
         ceso_m_sy(1,4,i) = ce_m_sy(1,4,i) * (0.0d0 / 700.0d0)
         ceso_m_sy(1,5,i) = ce_m_sy(1,5,i) * (0.0d0 / 400.0d0)
         ceso_m_sy(1,6,i) = ce_m_sy(1,6,i) * (0.0d0 / 900.0d0)
         ceso_m_sy(1,7,i) = ce_m_sy(1,7,i) * (100.0d0 / 4200.0d0)
         ceso_m_sy(1,8,i) = ce_m_sy(1,8,i) * (500.0d0 / 3800.0d0)
         ceso_m_sy(1,9,i) = ce_m_sy(1,9,i) * (400.0d0 / 6700.0d0)
         ceso_m_1013(1,i) = ce_m_1013(1,i) * (2200.0d0 / 70700.0d0)
         ceso_m_1415(1,i) = ce_m_1415(1,i) * (6800.0d0 / 453200.0d0)
         
         ceso_m_sy(2,0,i) = ce_m_sy(2,0,i) * 0.0d0
         ceso_m_sy(2,1,i) = ce_m_sy(2,1,i) * (0.0d0 / 100.0d0)
         ceso_m_sy(2,2,i) = ce_m_sy(2,2,i) * (0.0d0 / 200.0d0)
         ceso_m_sy(2,3,i) = ce_m_sy(2,3,i) * (0.0d0 / 300.0d0)
         ceso_m_sy(2,4,i) = ce_m_sy(2,4,i) * (0.0d0 / 300.0d0)
         ceso_m_sy(2,5,i) = ce_m_sy(2,5,i) * (0.0d0 / 200.0d0)
         ceso_m_sy(2,6,i) = ce_m_sy(2,6,i) * (0.0d0 / 500.0d0)
         ceso_m_sy(2,7,i) = ce_m_sy(2,7,i) * (400.0d0 / 4400.0d0)
         ceso_m_sy(2,8,i) = ce_m_sy(2,8,i) * (500.0d0 / 3700.0d0)
         ceso_m_sy(2,9,i) = ce_m_sy(2,9,i) * (100.0d0 / 3900.0d0)
         ceso_m_1013(2,i) = ce_m_1013(2,i) * (1600.0d0 / 51300.0d0)
         ceso_m_1415(2,i) = ce_m_1415(2,i) * (3900.0d0 / 390700.0d0)
         
         ceso_m_15u(1,i) = sum(ceso_m_sy(1,0:9,i)) + ceso_m_1013(1,i) + ceso_m_1415(1,i)
         ceso_m_15u(2,i) = sum(ceso_m_sy(2,0:9,i)) + ceso_m_1013(2,i) + ceso_m_1415(2,i)
         
         do sex = 1, 2
            do ageGrp = 1, 12
               ceso_m(sex,ageGrp,i) = (seo_a(0,0,i) - ceso_m_15u(1,i) - &
                 ceso_m_15u(2,i)) * (enas_a(sex,ageGrp,i) + eas_a(sex,ageGrp,i)) / &
                 (eas_a(0,0,i) + enas_a(0,0,i))
                !(sum(enas_a(1:2,1:12,i)) + sum(eas_a(1:2,1:12,i)) + sum(enas_a(1:2,23,i)) + sum(eas_a(1:2,23,i)))
            end do
            ceso_m(sex,23,i) = (seo_a(0,0,i) - ceso_m_15u(1,i) - ceso_m_15u(2,i)) * &
               (enas_a(sex,23,i) + eas_a(sex,23,i)) / (eas_a(0,0,i) + enas_a(0,0,i)) !(sum(enas_a(1:2,1:12,i)) + sum(eas_a(1:2,1:12,i)) + sum(enas_a(1:2,23,i)) + sum(eas_a(1:2,23,i)))
         end do
         
         ! Control age/sex groups to match total from model equations
         ceso_m_p = sum(ceso_m_15u(1:2,i)) + sum(ceso_m(1:2,1:12,i)) + sum(ceso_m(1:2,23,i))
         do sex = 1, 2
            ceso_m_15u(sex,i) = ceso_m_15u(sex,i) * seo_a(0,0,i) / ceso_m_p
            do ageGrp = 1, 12
               ceso_m(sex,ageGrp,i) = ceso_m(sex,ageGrp,i) * seo_a(0,0,i) / ceso_m_p
            end do
            ceso_m(sex,23,i) = ceso_m(sex,23,i) * seo_a(0,0,i) / ceso_m_p
         end do
         
         do sex = 1, 2
            do ageGrp = 1, 12
               cew_m(sex,ageGrp,i) = ce_m(sex,ageGrp,i) - ceso_m(sex,ageGrp,i)
            end do
            cew_m(sex,23,i) = ce_m(sex,23,i) - ceso_m(sex,23,i)
            cew_m_15u(sex,i) = ce_m_15u(sex,i) - ceso_m_15u(sex,i)
         
            ceso_m(sex,24,i) = sum(ceso_m(sex,1:12,i)) + ceso_m(sex,23,i)
            cew_m(sex,24,i) = sum(cew_m(sex,1:12,i)) + cew_m(sex,23,i)
            ceso_m(sex,27,i) = sum(ceso_m(sex,1:2,i))
            cew_m(sex,27,i) = sum(cew_m(sex,1:2,i))
         end do
         
         ceso_m(1,0,i) = ceso_m_15u(1,i) + ceso_m(1,24,i)
         ceso_m(2,0,i) = ceso_m_15u(2,i) + ceso_m(2,24,i)
         ceso_m(0,0,i) = ceso_m(1,0,i) + ceso_m(2,0,i)
         
         cew_m(1,0,i) = cew_m_15u(1,i) + cew_m(1,24,i)
         cew_m(2,0,i) = cew_m_15u(2,i) + cew_m(2,24,i)
         cew_m(0,0,i) = cew_m(1,0,i) + cew_m(2,0,i)

      end do
      
      do i = 81, endYr
         do sex = 1, 2
            ceso_m(sex,27,i) = sum(ceso_m(sex,1:2,i))
            cew_m(sex,27,i) = sum(cew_m(sex,1:2,i))
         end do
      end do
   
   end subroutine CalculateSelfEmployedOnly
   
!===============================================================================
   
   subroutine Finish()
    
      write(*,'(/a/)') "ModSolA procedure finished"
      
   end subroutine Finish      

!===============================================================================

end module EconModSolAMod