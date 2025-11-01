module EconModSol2EquationsMod

   use EconParMod
   use EconModSol2VarMod
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSol2EquationsMain
   
contains

!===============================================================================

   subroutine EconModSol2EquationsMain

      if (isBudgetRun) lastDataQtr = lastDataQtr - 1
   
      call SolveEmploymentNonAgriSelfRaw()            ! Equations   1- 23
      call SolveEmploymentNonAgriUnpaidRaw()          ! Equations  24- 42
      call SolveEmploymentAgriWageRaw()               ! Equations  43- 61
      call SolveEmploymentAgriUnpaidRaw()             ! Equations  62- 80
      call SolveEmploymentAgriSelfRaw()               ! Equations  81- 98
      call SolveEmploymentMales1665o()                ! Equations  99-155
      call SolveEmploymentFemales1665o()              ! Equations 156-227
      call SolveEmployementNonAgriWagePrivHouse       ! Equations 228-269
      call SolveEquations270_435()                    ! Equations 270-435
      
      if (isBudgetRun) lastDataQtr = lastDataQtr + 1

   end subroutine EconModSol2EquationsMain

!===============================================================================

   subroutine SolveEmploymentNonAgriSelfRaw()

      integer :: i
      
      ! Equations 1-23
      do i = startQtr+4, endQtr
        
         ! (2) EM1617NAS_R 
         enas_r(1,1,i) = (-0.23035d0 * rtp_1(i) + 0.24985d0) * e(1,1,i) + &
            enas_adj(1,1,i)
         
         ! (3) EF1617NAS_R 
         enas_r(2,1,i) = (0.12015d0 * rtp_1(i) - 0.10551d0) * e(2,1,i) + &
            enas_adj(2,1,i)
        
         ! (4) EF1819NAS_R 
         enas_r(2,2,i) = (0.11184d0 * rtp_1(i) - 0.10241d0) * e(2,2,i) + &
            enas_adj(2,2,i)
        
         ! (5) EF2024NAS_R 
         enas_r(2,3,i) = (0.08908d0 * rtp_1(i) - 0.07176d0) * e(2,3,i) + &
            enas_adj(2,3,i)
        
         ! (6) EF2534NAS_R 
         enas_r(2,18,i) = (0.00906d0 * rtp_1(i) + 0.03539d0) * e(2,18,i) + &
            enas_adj(2,18,i)
        
         ! (7) EF3544NAS_R 
         enas_r(2,19,i) =(-0.01869d0 * rtp_1(i) + 0.08087d0) * e(2,19,i) + &
            enas_adj(2,19,i)
        
         ! (8) EF4554NAS_R 
         enas_r(2,20,i) = (0.07232d0 * rtp_1(i) - 0.00701d0) * e(2,20,i) + &
            enas_adj(2,20,i)
        
         ! (9) EF5564NAS_R 
         enas_r(2,21,i) = (0.07872d0 * rtp_1(i) + 0.00466d0) * e(2,21,i) + &
            enas_adj(2,21,i)
        
         ! (10) EF65ONAS_R 
         enas_r(2,22,i) = (0.10940d0 * e(2,12,i) + 0.12265d0 * e(2,13,i) + &
            0.14137d0 * e(2,17,i)) + enas_adj(2,22,i)
        
         ! (11) EM1819NAS_R 
         enas_r(1,2,i) = (-0.05782d0 * rtp_1(i) + 0.07265d0) * e(1,2,i) + &
            enas_adj(1,2,i)
        
         ! (12) EM2024NAS_R 
         enas_r(1,3,i) = (-0.09206d0 * rtp_1(i) + 0.11567d0) * e(1,3,i) + &
            enas_adj(1,3,i)
        
         ! (13) EM2534NAS_R 
         enas_r(1,18,i) = (-0.09661d0 * rtp_1(i) + 0.14843d0) * e(1,18,i) + &
            enas_adj(1,18,i)
        
         ! (14) EM3544NAS_R 
         enas_r(1,19,i) =  (0.02739d0 * rtp_1(i) + 0.05236d0) * e(1,19,i) + &
            enas_adj(1,19,i)
        
         ! (15) EM4554NAS_R 
         enas_r(1,20,i) =  (0.06217d0 * rtp_1(i) + 0.03411d0) * e(1,20,i) + &
            enas_adj(1,20,i)
        
         ! (16) EM5564NAS_R 
         enas_r(1,21,i) = (-0.04776d0 * rtp_1(i) + 0.16626d0) * e(1,21,i) + &
            enas_adj(1,21,i)
        
         ! (17) EM65ONAS_R 
         enas_r(1,22,i) = (0.16527d0 * e(1,12,i) + 0.17798 * e(1,13,i) + &
            0.19058 * e(1,17,i)) + enas_adj(1,22,i)
        
         ! (18) ENAS_R 
         enas_r(0,0,i) = sum(enas_r(1:2,1:3,i)) +  sum(enas_r(1:2,18:22,i))
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         ! (19) GDPPFREAL 
         if (i >= lastDataQtr+1) then 
            if (long_range(i) == 0.0d0) then
               gdppfreal(i) = exp(-2.7555d0 + 0.0192d0 * year(i)) * n_ssa(i)  + gdppfreal_add(i)
            else
               gdppfreal(i) = gdppfreal(i-1) * gdpreal(i) / gdpreal(i-1)+ gdppfreal_add(i)
            end if

         ! (20) EA
            if (long_range(i) == 0.0d0) then
               ea(i) = gdppfreal(i) / exp(-0.85136d0 + 0.04661d0 * year(i)) + ea_add(i)
            else
               ea(i) = e(0,0,i) * ea(i-1) / e(0,0,i-1) + ea_add(i)
            end if
            
         ! (21) ENA 
            ena(i) = e(0,0,i) - ea(i)

         ! (22) ENAS
            if (long_range(i) == 0) then
               enas(0,0,i) = enas_r(0,0,i) + enas_add(0,0,i)
            else
               enas(0,0,i) = ena(i) * enas(0,0,i-1) / ena(i-1) + enas_add(0,0,i)
            end if

         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
 
         ! (23) EM1617NAS 
         enas(1,1,i) = enas_r(1,1,i) * enas(0,0,i) / enas_r(0,0,i)
   
      end do
   end subroutine SolveEmploymentNonAgriSelfRaw

!===============================================================================

   subroutine SolveEmploymentNonAgriUnpaidRaw()

      integer :: i
      
      ! Equations 24- 42
!      do i = max(startQtr, lastDataQtr+1), endQtr
      do i = startQtr+4, endQtr
      
         ! (24) EM1617NAU_R 
         enau_r(1,1,i) = 0.00028d0 * enas(0,0,i) + enau_adj(1,1,i) 
         
         ! (25) EF1617NAU_R 
         enau_r(2,1,i) = 0.00012d0 * enas(0,0,i) + enau_adj(2,1,i) 
         
         ! (26) EF1819NAU_R 
         enau_r(2,2,i) = 0.00025d0 * enas(0,0,i) + enau_adj(2,2,i) 
         
         ! (27) EF2024NAU_R 
         enau_r(2,3,i) = 0.00024d0 * enas(0,0,i) + enau_adj(2,3,i) 
         
         ! (28) EF2534NAU_R 
         enau_r(2,18,i) = 0.00117d0 * enas(0,0,i) + enau_adj(2,18,i)
         
         ! (29) EF3544NAU_R 
         enau_r(2,19,i) = 0.00218d0 * enas(0,0,i) + enau_adj(2,19,i)
         
         ! (30) EF4554NAU_R 
         enau_r(2,20,i) = 0.00226d0 * enas(0,0,i) + enau_adj(2,20,i)
         
         ! (31) EF5564NAU_R 
         enau_r(2,21,i) = 0.00083d0 * enas(0,0,i) + enau_adj(2,21,i)
         
         ! (32) EF65ONAU_R 
         enau_r(2,22,i) = (0.00027d0 + 0.00021d0 + 0.00008d0) * enas(0,0,i) + &
            enau_adj(2,22,i)
         
         ! (33) EM1819NAU_R 
         enau_r(1,2,i) = 0.00033d0 * enas(0,0,i) + enau_adj(1,2,i) 
         
         ! (34) EM2024NAU_R 
         enau_r(1,3,i) = 0.00050d0 * enas(0,0,i) + enau_adj(1,3,i) 
         
         ! (35) EM2534NAU_R 
         enau_r(1,18,i) = 0.00044d0 * enas(0,0,i) + enau_adj(1,18,i)
         
         ! (36) EM3544NAU_R 
         enau_r(1,19,i) = 0.00043d0 * enas(0,0,i) + enau_adj(1,19,i)
         
         ! (37) EM4554NAU_R 
         enau_r(1,20,i) = 0.00052d0 * enas(0,0,i) + enau_adj(1,20,i)
         
         ! (38) EM5564NAU_R 
         enau_r(1,21,i) = 0.00037d0 * enas(0,0,i) + enau_adj(1,21,i)
         
         ! (39) EM65ONAU_R 
         enau_r(1,22,i) = (0.00023d0 + 0.00010d0 + 0.00011d0) * enas(0,0,i) + &
            enau_adj(1,22,i)
         
         ! (40) ENAU_R 
         enau_r(0,0,i) = sum(enau_r(1:2,1:3,i)) + sum(enau_r(1:2,18:22,i))
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         ! (41) ENAU
         if (i >= lastDataQtr+1) then 
            enau(0,0,i) = enau_r(0,0,i)
         end if
         
          if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         ! (42) EM1617NAU 
         enau(1,1,i) = enau_r(1,1,i) * (enau(0,0,i) / enau_r(0,0,i))
      
      end do

   end subroutine SolveEmploymentNonAgriUnpaidRaw

!===============================================================================

   subroutine SolveEmploymentAgriWageRaw()

      integer :: i
      
      ! Equations 43- 61
!      do i = max(startQtr, lastDataQtr+1), endQtr
      do i = startQtr+4, endQtr
      
          if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         ! (43) EAW
         if (i >= lastDataQtr+1) then 
            if (long_range(i) == 0) then
               eaw(0,0,i) = ea(i) * (0.00893d0 * year(i) + 0.33159d0 * rtp(i) - &
                  0.67943d0) + eaw_add(0,0,i)
            else
               eaw(0,0,i) = ea(i) * eaw(0,0,i-1) / ea(i-1) + eaw_add(0,0,i)
            end if
         end if
         
          if (isBudgetRun) lastDataQtr = lastDataQtr - 1

         ! (44) EM1617AW_R 
         eaw_r(1,1,i) = max(0.0d0, eaw(0,0,i) * (-0.00594d0 - 0.09353d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 5.28754d0 * e(1,1,i) / e(0,0,i) + &
            0.08116d0) + eaw_adj(1,1,i))
         
         ! (45) EF1617AW_R 
         eaw_r(2,1,i) = max(0.0d0, eaw(0,0,i) * (-0.00055d0 - 0.05470d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 1.41760d0 * e(2,1,i) / e(0,0,i) + &
            0.04979d0) + eaw_adj(2,1,i))
         
         ! (46) EF1819AW_R 
         eaw_r(2,2,i) = max(0.0d0, eaw(0,0,i) * (0.00102d0 - 0.07375d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 0.78394d0 * e(2,2,i) / e(0,0,i) + &
            0.07226d0) + eaw_adj(2,2,i))
         
         ! (47) EF2024AW_R 
         eaw_r(2,3,i) = max(0.0d0, eaw(0,0,i) * (0.00112d0 - 0.05971d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 0.57256d0 * e(2,3,i) / e(0,0,i) + &
            0.05907d0) + eaw_adj(2,3,i))
         
         ! (48) EF2534AW_R 
         eaw_r(2,18,i) = max(0.0d0, eaw(0,0,i) * (0.00623d0 + 0.08868d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 1.00897d0 * e(2,18,i) / e(0,0,i) - &
            0.15142d0) + eaw_adj(2,18,i))
         
         ! (49) EF3544AW_R 
         eaw_r(2,19,i) = max(0.0d0, eaw(0,0,i) * (0.00687d0 - 0.00259d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 0.51319d0 * e(2,19,i) / e(0,0,i) - &
            0.00937d0) + eaw_adj(2,19,i))
         
         ! (50) EF4554AW_R 
         eaw_r(2,20,i) = max(0.0d0, eaw(0,0,i) * (0.00185d0 + 0.08747d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 0.28022d0 * e(2,20,i) / e(0,0,i) - &
            0.08053d0) + eaw_adj(2,20,i))
         
         ! (51) EF5564AW_R 
         eaw_r(2,21,i) = max(0.0d0, eaw(0,0,i) * (-0.00140d0 - 0.03001d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 - 0.59383d0 * e(2,21,i) / e(0,0,i) + &
            0.07088d0) + eaw_adj(2,21,i))
         
         ! (52) EF65OAW_R  
         eaw_r(2,22,i) = max(0.0d0, eaw(0,0,i) * (0.00096d0 + 0.06768d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 1.04213d0 * e(2,22,i) /  e(0,0,i) - &
            0.07359d0) + eaw_adj(2,22,i))
         
         ! (53) EM1819AW_R 
         eaw_r(1,2,i) = max(0.0d0, eaw(0,0,i) * (-0.00131d0 - 0.18120d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 3.87151d0 * e(1,2,i) / e(0,0,i) + &
            0.16636d0) + eaw_adj(1,2,i))
         
         ! (54) EM2024AW_R 
         eaw_r(1,3,i) = max(0.0d0, eaw(0,0,i) * (-0.00664d0 + 0.10493d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 2.00153d0 * e(1,3,i) / e(0,0,i) - &
            0.08191d0) + eaw_adj(1,3,i))
         
         ! (55) EM2534AW_R 
         eaw_r(1,18,i) = max(0.0d0, eaw(0,0,i) * (-0.02065d0 + 0.38358d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 - 0.98380d0 * e(1,18,i) / e(0,0,i) + &
            0.00751d0) + eaw_adj(1,18,i))
         
         ! (56) EM3544AW_R 
         eaw_r(1,19,i) = max(0.0d0, eaw(0,0,i) * (0.00402d0 - 0.15663d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 1.72119d0 * e(1,19,i) / e(0,0,i) + &
            0.05679d0) + eaw_adj(1,19,i))
         
         ! (57) EM4554AW_R 
         eaw_r(1,20,i) = max(0.0d0, eaw(0,0,i) * (0.00834d0 + 0.03746d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 0.46522d0 * e(1,20,i) / e(0,0,i) + &
            0.00144d0) + eaw_adj(1,20,i))
         
         ! (58) EM5564AW_R 
         eaw_r(1,21,i) = max(0.0d0, eaw(0,0,i) * (-0.00655d0 + 0.03521d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 0.46852d0 * e(1,21,i) / e(0,0,i) - &
            0.00037d0) + eaw_adj(1,21,i))
         
         ! (59) EM65OAW_R  
         eaw_r(1,22,i) = max(0.0d0, eaw(0,0,i) * (-0.00114d0 + 0.07640d0 * &
            (sum(rtp(i-2:i-1))) / 2.0d0 + 3.25911d0 * e(1,22,i)/  e(0,0,i) - &
            0.10058d0) + eaw_adj(1,22,i))
         
         ! (60) EAW_R 
         eaw_r(0,0,i) = sum(eaw_r(1:2,1:3,i)) + sum(eaw_r(1:2,18:22,i))
         
         ! (61) EM1617AW 
         eaw(1,1,i) = eaw_r(1,1,i) * eaw(0,0,i) / eaw_r(0,0,i)
      
      end do

   end subroutine SolveEmploymentAgriWageRaw

!===============================================================================

   subroutine SolveEmploymentAgriUnpaidRaw()

      integer :: i
      
      ! Equations 62- 80
!      do i = max(startQtr, lastDataQtr+1), endQtr
      do i = startQtr+4, endQtr
      
         ! (62) EM1617AS_R
         eas_r(1,1,i) = max(0.0d0, n(1,1,i) * (0.00528d0 + 0.00404d0) + &
            eas_adj(1,1,i))

!!!!! PLEASE NOTE eau_adj(1,1,i) IS AN ERR SHOULD BE eau_adj(2,1,i) !!!!!                  
         ! (63) EF1617AU_R 
         eau_r(2,1,i) = max(0.0d0, 0.006d0 + eau_adj(1,1,i))
        
         ! (64) EF1819AU_R 
         eau_r(2,2,i) = max(0.0d0, 0.005d0 + eau_adj(2,2,i))
        
         ! (65) EF2024AU_R 
         eau_r(2,3,i) = max(0.0d0, 0.005d0 + eau_adj(2,3,i))
        
         ! (66) EF2534AU_R 
         eau_r(2,18,i) = max(0.0d0, 0.002d0 + eau_adj(2,18,i))
        
         ! (67) EF3544AU_R 
         eau_r(2,19,i) = max(0.0d0, 0.002d0 + eau_adj(2,19,i))
        
         ! (68) EF4554AU_R 
         eau_r(2,20,i) = max(0.0d0, 0.001d0 + eau_adj(2,20,i))
        
         ! (69) EF5564AU_R 
         eau_r(2,21,i) = max(0.0d0, 0.001d0 + eau_adj(2,21,i))
        
         ! (70) EF65OAU_R  
         eau_r(2,22,i) = max(0.0d0, 0.002d0 + eau_adj(2,22,i))
        
         ! (71) EM1617AU_R 
         eau_r(1,1,i) = max(0.0d0, 0.002d0 + eau_adj(1,1,i))
        
         ! (72) EM1819AU_R 
         eau_r(1,2,i) = max(0.0d0, 0.001d0 + eau_adj(1,2,i))
        
         ! (73) EM2024AU_R 
         eau_r(1,3,i) = max(0.0d0, 0.001d0 + eau_adj(1,3,i))
        
         ! (74) EM2534AU_R 
         eau_r(1,18,i) = max(0.0d0, 0.003d0 + eau_adj(1,18,i))
        
         ! (75) EM3544AU_R 
         eau_r(1,19,i) = max(0.0d0, 0.004d0 + eau_adj(1,19,i))
        
         ! (76) EM4554AU_R 
         eau_r(1,20,i) = max(0.0d0, 0.005d0 + eau_adj(1,20,i))
        
         ! (77) EM5564AU_R 
         eau_r(1,21,i) = max(0.0d0, 0.003d0 + eau_adj(1,21,i))
        
         ! (78) EM65OAU_R  
         eau_r(1,22,i) = max(0.0d0, 0.001d0 + eau_adj(1,22,i))
        
         ! (79) EAU_R 
         eau_r(0,0,i) = sum(eau_r(1:2,1:3,i)) + sum(eau_r(1:2,18:22,i))

          if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         ! (80) EAU
         if (i >= lastDataQtr+1) then 
            if (long_range(i) == 0) then
               eau(0,0,i) = eau_r(0,0,i)
            else
               eau(0,0,i) = eau(0,0,i-1) / ea(i-1) * ea(i) 
            end if
         end if
         
          if (isBudgetRun) lastDataQtr = lastDataQtr - 1
     
      end do

   end subroutine SolveEmploymentAgriUnpaidRaw

!===============================================================================

   subroutine SolveEmploymentAgriSelfRaw()

      integer :: i
      
      ! Equations 81- 98
!      do i = max(startQtr, lastDataQtr+1), endQtr
      do i = startQtr+4, endQtr
         
          if (isBudgetRun) lastDataQtr = lastDataQtr + 1

         if (i >= lastDataQtr+1) then
         
            ! (81) EAS 
            eas(0,0,i) = ea(i) - eau(0,0,i) - eaw(0,0,i)
            
         end if
         
          if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         ! (82) EF1617AS_R 
         eas_r(2,1,i) = max(0.0d0, n(2,1,i) * (0.00181d0 + 0.00030d0) + &
            eas_adj(2,1,i))
         
         ! (83) EF1819AS_R 
         eas_r(2,2,i) = max(0.0d0, eas(1,2,i-1) * (-0.02393d0 + &
            0.63672d0 * sum(rtp(i-2:i-1))/2.0d0 + 0.98791d0 * e(2,2,i) / &
            e(1,2,i) - 4.43926d0) + eas_adj(2,2,i))
         
         ! (84) EF2024AS_R 
         eas_r(2,3,i) = max(0.0d0, eas(1,3,i-1) * (0.07353d0 - &
            0.40207d0 * sum(rtp(i-2:i-1))/2.0d0 + 0.57572d0 * e(2,3,i) / &
            e(1,3,i) - 0.01117d0) + eas_adj(2,3,i))
         
         ! (85) EF2534AS_R 
         eas_r(2,18,i) = max(0.0d0, eas(1,18,i-1) * (0.16575d0 + &
            0.16967d0 * sum(rtp(i-2:i-1))/2.0d0 + 0.55503d0 * e(2,18,i) / &
            e(1,18,i) - 0.43412d0) + eas_adj(2,18,i))
         
         ! (86) EF3544AS_R 
         eas_r(2,19,i) = max(0.0d0, eas(1,19,i-1) * (0.15848d0 + &
            0.37839d0 * sum(rtp(i-2:i-1))/2.0d0 + 0.37764d0 * e(2,19,i) / &
            e(1,19,i) - 0.45362d0) + eas_adj(2,19,i))
         
         ! (87) EF4554AS_R 
         eas_r(2,20,i) = max(0.0d0, eas(1,20,i-1) * (0.21947d0 + &
            0.29497d0 * sum(rtp(i-2:i-1))/2.0d0 + 0.58974d0 * e(2,20,i) / &
            e(1,20,i) - 0.51966d0) + eas_adj(2,20,i))
         
         ! (88) EF5564AS_R 
         eas_r(2,21,i) = max(0.0d0, eas(1,21,i-1) * (0.20892d0 + &
            0.36294d0 * sum(rtp(i-2:i-1))/2.0d0 + 0.65320d0 * e(2,21,i) / &
            e(1,21,i) - 0.66626d0) + eas_adj(2,21,i))
         
         ! (89) EF65OAS_R  
         eas_r(2,22,i) = max(0.0d0, eas(1,22,i-1) * (0.16242d0 + &
            0.54916d0 * sum(rtp(i-2:i-1))/2.0d0 + 0.06199d0 * e(2,22,i) / &
            e(1,22,i) - 0.47556d0) + eas_adj(2,22,i))
         
         ! (90) EM1819AS_R 
         eas_r(1,2,i) = max(0.0d0, n(1,2,i) * (0.00309d0 + 0.28448d0 * &
            ea(i) / sum(n(1:2,24,i)) - 0.00165d0) + eas_adj(1,2,i))
         
         ! (91) EM2024AS_R 
         eas_r(1,3,i) = max(0.0d0, n(1,3,i) * (-0.00181d0 + 0.97958d0 * &
            ea(i) / sum(n(1:2,24,i)) -  0.01093d0) + eas_adj(1,3,i))
         
         ! (92) EM2534AS_R 
         eas_r(1,18,i) = max(0.0d0, n(1,18,i) * (-0.00263d0 + 1.23186d0 * &
            ea(i) / sum(n(1:2,24,i)) - 0.01021d0) + eas_adj(1,18,i))
         
         ! (93) EM3544AS_R 
         eas_r(1,19,i) = max(0.0d0, n(1,19,i) * (-0.00151d0 + 1.66765d0 * &
            ea(i) / sum(n(1:2,24,i)) - 0.01450d0) + eas_adj(1,19,i))
         
         ! (94) EM4554AS_R 
         eas_r(1,20,i) = max(0.0d0, n(1,20,i) * (-0.00381d0 + 2.86654d0 * &
            ea(i) / sum(n(1:2,24,i)) - 0.03175d0) + eas_adj(1,20,i))
         
         ! (95) EM5564AS_R 
         eas_r(1,21,i) = max(0.0d0, n(1,21,i) * (-0.00460d0 + 2.78817d0 * &
            ea(i) / sum(n(1:2,24,i)) - 0.02398d0) + eas_adj(1,21,i))
         
         ! (96) EM65OAS_R  
         eas_r(1,22,i) = max(0.0d0, n(1,22,i) * (0.00079d0 + 1.76904d0 * &
            ea(i) / sum(n(1:2,24,i)) -  0.01437d0) + eas_adj(1,22,i))
         
         ! (97) EAS_R 
         eas_r(0,0,i) = sum(eas_r(1:2,1:3,i)) + sum(eas_r(1:2,18:22,i))
         
         ! (98) EM1617AS 
         eas(1,1,i) = eas_r(1,1,i) * (eas(0,0,i) / eas_r(0,0,i))
         
                  ! (99) EM1617AU 
         eau(1,1,i) = eau_r(1,1,i) * (eau(0,0,i) / eau_r(0,0,i))
                 
         ! (102) EM1819NAS 
         enas(1,2,i) = enas_r(1,2,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (103) EM1819NAU 
         enau(1,2,i) = enau_r(1,2,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (104) EM1819AW 
         eaw(1,2,i) = eaw_r(1,2,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (105) EM1819AS 
         if (i >= lastDataQtr-2) then 
            eas(1,2,i) = eas_r(1,2,i) * (eas(0,0,i) / eas_r(0,0,i))
         end if
         
         ! (106) EM1819AU 
         eau(1,2,i) = eau_r(1,2,i) * (eau(0,0,i) / eau_r(0,0,i))
               
         ! (109) EM2024NAS 
         enas(1,3,i) = enas_r(1,3,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (110) EM2024NAU 
         enau(1,3,i) = enau_r(1,3,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (111) EM2024AW 
         eaw(1,3,i) = eaw_r(1,3,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (112) EM2024AS 
         if (i >= lastDataQtr-2) then 
            eas(1,3,i) = eas_r(1,3,i) * (eas(0,0,i) / eas_r(0,0,i))
         end if
         
         ! (113) EM2024AU 
         eau(1,3,i) = eau_r(1,3,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (116) EM2534NAS 
         enas(1,18,i) = enas_r(1,18,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (117) EM2534NAU 
         enau(1,18,i) = enau_r(1,18,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (118) EM2534AW 
         eaw(1,18,i) = eaw_r(1,18,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (119) EM2534AS 
         if (i >= lastDataQtr-2) then 
            eas(1,18,i) = eas_r(1,18,i) * (eas(0,0,i) / eas_r(0,0,i))
         end if
         
         ! (120) EM2534AU 
         eau(1,18,i) = eau_r(1,18,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (123) EM3544NAS 
         enas(1,19,i) = enas_r(1,19,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (124) EM3544NAU 
         enau(1,19,i) = enau_r(1,19,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (125) EM3544AW 
         eaw(1,19,i) = eaw_r(1,19,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (126) EM3544AS 
         if (i >= lastDataQtr-2) then 
            eas(1,19,i) = eas_r(1,19,i) * (eas(0,0,i) / eas_r(0,0,i))
         end if
         
         ! (127) EM3544AU 
         eau(1,19,i) = eau_r(1,19,i) * (eau(0,0,i) / eau_r(0,0,i))
         
         ! (130) EM4554NAS 
         enas(1,20,i) = enas_r(1,20,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (131) EM4554NAU 
         enau(1,20,i) = enau_r(1,20,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (132) EM4554AW 
         eaw(1,20,i) = eaw_r(1,20,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (133) EM4554AS 
         if (i >= lastDataQtr-2) then 
            eas(1,20,i) = eas_r(1,20,i) * (eas(0,0,i) / eas_r(0,0,i))
         end if
         
         ! (134) EM4554AU 
         eau(1,20,i) = eau_r(1,20,i) * (eau(0,0,i) / eau_r(0,0,i))
         
         ! (137) EM5564NAS 
         enas(1,21,i) = enas_r(1,21,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (138) EM5564NAU 
         enau(1,21,i) = enau_r(1,21,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (139) EM5564AW 
         eaw(1,21,i) = eaw_r(1,21,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (140) EM5564AS 
         if (i >= lastDataQtr-2) then 
            eas(1,21,i) = eas_r(1,21,i) * (eas(0,0,i) / eas_r(0,0,i))
         end if
         
         ! (141) EM5564AU 
         eau(1,21,i) = eau_r(1,21,i) * (eau(0,0,i) / eau_r(0,0,i))
         
         ! (144) EM65ONAS 
         enas(1,22,i) = enas_r(1,22,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (145) EM65ONAU 
         enau(1,22,i) = enau_r(1,22,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (146) EM65OAW 
         eaw(1,22,i) = eaw_r(1,22,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (147) EM65OAS 
         if (i >=lastDataQtr-2) then 
            eas(1,22,i) = eas_r(1,22,i) * (eas(0,0,i) / eas_r(0,0,i))
         end if
      
      end do

   end subroutine SolveEmploymentAgriSelfRaw

!===============================================================================

   subroutine SolveEmploymentMales1665o()

      integer :: i
      
      ! Equations 99-155
!      do i = max(startQtr, lastDataQtr+1), endQtr
      do i = startQtr+4, endQtr



         ! (148) EM65OAU 
         eau(1,22,i) = eau_r(1,22,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (150) EMNAS 
         enas(1,0,i) = sum(enas(1,1:3,i)) + sum(enas(1,18:22,i))
         
         ! (151) EMNAU 
         enau(1,0,i) = sum(enau(1,1:3,i)) + sum(enau(1,18:22,i))
         
         ! (152) EMAW 
         eaw(1,0,i) = sum(eaw(1,1:3,i)) + sum(eaw(1,18:22,i))
         
         ! (153) EMAS 
         eas(1,0,i) = sum(eas(1,1:3,i)) + sum(eas(1,18:22,i))       
         
         ! (154) EMAU 
         eau(1,0,i) = sum(eau(1,1:3,i)) + sum(eau(1,18:22,i))
         
      end do

   end subroutine SolveEmploymentMales1665o

!===============================================================================

   subroutine SolveEmploymentFemales1665o()

      integer :: i
      
      ! Equations 156-227
!      do i = max(startQtr, lastDataQtr+1), endQtr
      do i = startQtr+4, endQtr
      
         ! (157) EF1617NAS 
         enas(2,1,i) = enas_r(2,1,i) * (enas(0,0,i) / enas_r(0,0,i))

         ! (158) EF1617NAU 
         enau(2,1,i) = enau_r(2,1,i) * (enau(0,0,i) / enau_r(0,0,i))

         ! (159) EF1617AW  
         eaw(2,1,i) = eaw_r(2,1,i) * (eaw(0,0,i) / eaw_r(0,0,i))

         ! (160) EF1617AS  
         eas(2,1,i) = eas_r(2,1,i) * (eas(0,0,i) / eas_r(0,0,i))

         ! (161) EF1617AU  
         eau(2,1,i)  = eau_r(2,1,i) * (eau(0,0,i) / eau_r(0,0,i))
         
         ! (164) EF1819NAS 
         enas(2,2,i) = enas_r(2,2,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (165) EF1819NAU 
         enau(2,2,i) = enau_r(2,2,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (166) EF1819AW  
         eaw(2,2,i) = eaw_r(2,2,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (167) EF1819AS  
         eas(2,2,i) = eas_r(2,2,i) * (eas(0,0,i) / eas_r(0,0,i))
         
         ! (168) EF1819AU  
         eau(2,2,i) = eau_r(2,2,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (171) EF2024NAS 
         enas(2,3,i)= enas_r(2,3,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (172) EF2024NAU 
         enau(2,3,i)= enau_r(2,3,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (173) EF2024AW  
         eaw(2,3,i) = eaw_r(2,3,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (174) EF2024AS  
         eas(2,3,i) = eas_r(2,3,i) * (eas(0,0,i) / eas_r(0,0,i))
         
         ! (175) EF2024AU  
         eau(2,3,i) = eau_r(2,3,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (178) EF2534NAS 
         enas(2,18,i)= enas_r(2,18,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (179) EF2534NAU 
         enau(2,18,i)= enau_r(2,18,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (180) EF2534AW   
         eaw(2,18,i) = eaw_r(2,18,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (181) EF2534AS   
         eas(2,18,i) = eas_r(2,18,i) * (eas(0,0,i) / eas_r(0,0,i))
         
         ! (182) EF2534AU   
         eau(2,18,i) = eau_r(2,18,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (185) EF3544NAS 
         enas(2,19,i)= enas_r(2,19,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (186) EF3544NAU 
         enau(2,19,i)= enau_r(2,19,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (187) EF3544AW  
         eaw(2,19,i) =  eaw_r(2,19,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (188) EF3544AS  
         eas(2,19,i) =  eas_r(2,19,i) * (eas(0,0,i) / eas_r(0,0,i))
         
         ! (189) EF3544AU  
         eau(2,19,i) =  eau_r(2,19,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (192) EF4554NAS 
         enas(2,20,i) = enas_r(2,20,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (193) EF4554NAU 
         enau(2,20,i) = enau_r(2,20,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (194) EF4554AW   
         eaw(2,20,i) = eaw_r(2,20,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (195) EF4554AS   
         eas(2,20,i) = eas_r(2,20,i) * (eas(0,0,i) / eas_r(0,0,i))
         
         ! (196) EF4554AU   
         eau(2,20,i) = eau_r(2,20,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (199) EF5564NAS 
         if (i >= lastDataQtr-2) then 
            enas(2,21,i)= enas_r(2,21,i) * (enas(0,0,i) / enas_r(0,0,i))
         end if
         
         ! (200) EF5564NAU 
         enau(2,21,i)= enau_r(2,21,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (201) EF5564AW   
         eaw(2,21,i) = eaw_r(2,21,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (202) EF5564AS   
         eas(2,21,i) = eas_r(2,21,i) * (eas(0,0,i) / eas_r(0,0,i))
         
         ! (203) EF5564AU   
         eau(2,21,i) = eau_r(2,21,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (206) EF65ONAS 
         enas(2,22,i)= enas_r(2,22,i) * (enas(0,0,i) / enas_r(0,0,i))
         
         ! (207) EF65ONAU 
         enau(2,22,i)= enau_r(2,22,i) * (enau(0,0,i) / enau_r(0,0,i))
         
         ! (208) EF65OAW  
         eaw(2,22,i) =  eaw_r(2,22,i) * (eaw(0,0,i) / eaw_r(0,0,i))
         
         ! (209) EF65OAS  
         eas(2,22,i) =  eas_r(2,22,i) * (eas(0,0,i) / eas_r(0,0,i))
         
         ! (210) EF65OAU  
         eau(2,22,i) =  eau_r(2,22,i) * (eau(0,0,i) / eau_r(0,0,i))
                  
         ! (212) EFNAS 
         enas(2,0,i) = sum(enas(2,1:3,i)) + sum(enas(2,18:22,i))
         
         ! (213) EFNAU 
         enau(2,0,i) = sum(enau(2,1:3,i)) + sum(enau(2,18:22,i))
         
         ! (214) EFAW  
         eaw(2,0,i) = sum(eaw(2,1:3,i)) + sum(eaw(2,18:22,i))
         
         ! (215) EFAS  
         eas(2,0,i) = sum(eas(2,1:3,i)) + sum(eas(2,18:22,i))
         
         ! (216) EFAU  
         eau(2,0,i) = sum(eau(2,1:3,i)) + sum(eau(2,18:22,i))
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         ! (223)PGDPAF
         if (i >= lastDataQtr+1) then 
            if (long_range(i) == 0) then
               pgdpaf(i) = pgdpaf(i-1) * ((pgdp(i) / pgdp(i-1))**4.0d0 - &
                  0.01d0)**0.25 + pgdpaf_add(i)
            else
               pgdpaf(i) = pgdpaf(i-1) * ((pgdp(i) / pgdp(i-1))**4.0d0)**0.25d0 + pgdpaf_add(i)
            end if
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
             
      end do

   end subroutine SolveEmploymentFemales1665o

!===============================================================================

   subroutine SolveEmployementNonAgriWagePrivHouse()

      integer :: i
      
      ! Equations 228-269
!      do i = max(startQtr, lastDataQtr+1), endQtr
      do i = startQtr+4, endQtr

      if (i >= lastDataQtr+2) then
      
         ! (228)DNEDMIL
         if ((edmil(i) - edmil(i-4)) < 0.0d0) then
            dnedmil(i) = edmil(i) - edmil(i-4)
         else
            dnedmil(i) = 0.0d0 
         end if
         
      end if
         
         ! (229) EF1617NAWPH_R  
         enawph_r(2,1,i) = max(0.001d0, -0.20802d0 * sum(rtp_1(i-3:i)) / &
            4.0d0 - 0.40988d0 * sum(rtp_5(i-3:i)) / 4.0d0 + 0.01015d0 + &
            61.2465d0 * 1.0d0 / year(i) - 0.00965d0 * minw(i) / cpiw_u(i) + &
            0.01561d0 * nu10(i) / n(2,1,i) - 0.13398d0) * e(2,1,i) + &
            enawph_adj(2,1,i)
         
         ! (230) EF1819NAWPH_R  
         enawph_r(2,2,i) = max(0.001d0, -0.03363d0 * sum(rtp_1(i-3:i)) / &
            4.0d0 - 0.12989d0 * sum(rtp_5(i-3:i)) / 4.0d0 - 0.00661d0 + &
            8.44701d0 * 1.0d0 / year(i) - 0.00539d0 * minw(i) / cpiw_u(i) + &
            0.00345d0 * nu10(i) / n(2,2,i) + 0.07597d0) * e(2,2,i) + &
            enawph_adj(2,2,i)
         
         ! (231) EF2024NAWPH_R  
         enawph_r(2,3,i) = max(0.001d0, -0.18707d0 * sum(rtp_1(i-19:i)) / &
            20.0d0 - 0.00223d0 + 2.12060d0 * 1.0d0 / year(i) + 0.00820d0 * &
            nu10(i) / n(2,3,i) + 0.14537d0) * e(2,3,i) + enawph_adj(2,3,i)
         
         ! (232) EF2534NAWPH_R 
         enawph_r(2,18,i) = max(0.001d0,  0.01874d0 * sum(rtp_1(i-3:i)) / &
            4.0d0 - 0.04167d0 * sum(rtp_5(i-19:i)) / 20.0d0 - 0.00090d0 + &
            1.55167d0 * 1.0d0 / year(i) + 0.01021d0 * nu10(i) / n(2,18,i) - &
            0.00170) * e(2,18,i) + enawph_adj(2,18,i)
         
         ! (233) EF3544NAWPH_R 
         enawph_r(2,19,i) = (0.00622d0 * sum(rtp_1(i-3:i)) / 4.0d0 - &
            0.06062d0 * sum(rtp_5(i-19:i)) / 20.0d0 + 0.00008d0 + 0.29372d0 * &
            (sum(enawph(2,18,i-47:i-36)/e(2,18,i-47:i-36)) / 12.0d0) + &
            0.06187d0) * e(2,19,i) + enawph_adj(2,19,i)
         
         ! (234) EF4554NAWPH_R 
         enawph_r(2,20,i) = (0.02788d0 * sum(rtp_1(i-3:i)) / 4.0d0 - &
            0.10996d0 * sum(rtp_5(i-19:i)) / 20.0d0 - 0.00349d0 + 0.53068d0 * &
            (sum(enawph(2,19,i-47:i-36)/e(2,19,i-47:i-36)) / 12.0d0) + &
            0.08883d0) * e(2,20,i) + enawph_adj(2,20,i)
         
         ! (235) EF5564NAWPH_R 
         enawph_r(2,21,i) = (0.05939d0 * sum(rtp_1(i-3:i)) / 4.0d0 - &
            0.10618d0 * sum(rtp_5(i-7:i)) / 8.0d0 -  0.00579d0 + 0.66195d0 * &
            (sum(enawph(2,20,i-47:i-36)/e(2,20,i-47:i-36)) / 12.0d0) + &
            0.05966d0) * e(2,21,i) + enawph_adj(2,21,i)
         
         ! (236) EF65ONAWPH_R  
         enawph_r(2,22,i) = (0.22642d0 * sum(rtp_1(i-3:i)) / 4.0d0 - &
            0.02069d0 + 0.33505d0 * (sum(enawph(2,21,i-47:i-36)) / 12.0d0) - &
            0.19707d0) + enawph_adj(2,22,i)
         
         ! (237) EM1617NAWPH_R  
         enawph_r(1,1,i) = max(0.001d0, -0.05284d0 * sum(rtp_1(i-3:i)) / &
            4.0d0 - 0.17833d0 * sum(rtp_5(i-3:i)) / 4.0d0 - 0.00768d0 + &
            9.19738d0 * 1.0d0 / year(i) - 0.00588d0 * minw(i) / cpiw_u(i) + &
            0.16862d0) * e(1,1,i) + enawph_adj(1,1,i)
         
         ! (238) EM1819NAWPH_R  
         enawph_r(1,2,i) = max(0.001d0, -0.07122d0 * sum(rtp_1(i-3:i)) / &
            4.0d0 - 0.03737d0 * sum(rtp_5(i-3:i)) / 4.0d0 - 0.00282d0 + &
            3.76796d0 * 1.0d0 / year(i) - 0.00499d0 * minw(i) / cpiw_u(i) + &
            0.08727d0) * e(1,2,i) + enawph_adj(1,2,i)
         
         ! (239) EM2024NAWPH_R  
         enawph_r(1,3,i) = max(0.001d0, -0.00450d0 * sum(rtp_1(i-3:i)) / &
            4.0d0 - 0.02345d0 * sum(rtp_5(i-3:i)) / 4.0d0 - 0.00113d0 - &
            0.00057d0 * minw(i) / cpiw_u(i) + 0.03265d0) * e(1,3,i) + &
            enawph_adj(1,3,i)
         
         ! (240) EM2534NAWPH_R 
         enawph_r(1,18,i) = max(0.001d0, -0.00490d0 * sum(rtp_5(i-3:i)) / &
            4.0d0 - 0.00054d0 - 0.00051d0 * minw(i) / cpiw_u(i) + 0.00789d0) * &
            e(1,18,i) + enawph_adj(1,18,i)
         
         ! (241) EM3544NAWPH_R 
         enawph_r(1,19,i) = (-0.00446d0 * sum(rtp_5(i-3:i)) / 4.0d0 - &
            0.00041d0 - 0.00053d0 * minw(i) / cpiw_u(i) + 0.00726d0) * &
            e(1,19,i) + enawph_adj(1,19,i)
         
         ! (242) EM4554NAWPH_R 
         enawph_r(1,20,i) = (-0.00039d0 + 0.00129d0) * e(1,20,i) + &
            enawph_adj(1,20,i)
         
         ! (243) EM5564NAWPH_R 
         enawph_r(1,21,i) = (-0.00015d0 + 0.00200d0) * e(1,21,i) + &
            enawph_adj(1,21,i)
         
         ! (244) EM65ONAWPH_R  
         enawph_r(1,22,i) = (-0.00679d0 + 0.64405d0 * &
            (sum(enawph(1,21,i-47:i-36)) / 12.0d0) + 0.00231d0) + &
            enawph_adj(1,22,i)
         
         ! (245) ENAWPH_R 
         enawph_r(0,0,i) = sum(enawph_r(1:2,1:3,i)) + sum(enawph_r(1:2,18:22,i))
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if (i >= lastDataQtr+1) then 
            ! (246) ENAWPH        
            if (long_range(i) == 0.0d0) then
               enawph(0,0,i) = enawph_r(0,0,i) + enawph_add(0,0,i)
            else 
               enawph(0,0,i) = enawph(0,0,i-1) * (e_fe(0,0,i) / e_fe(0,0,i-1))
            end if
            
         end if
         
          if (isBudgetRun) lastDataQtr = lastDataQtr - 1
            
         ! (247) EF1617NAWPH  
         enawph(2,1,i) = enawph_r(2,1,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         ! (248) EF1819NAWPH  
         enawph(2,2,i) =  enawph_r(2,2,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         ! (249) EF2024NAWPH  
         enawph(2,3,i) =  enawph_r(2,3,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         if(i >= lastDataQtr-10) then
            
            ! (250) EF2534NAWPH 
            enawph(2,18,i) = enawph_r(2,18,i) * (enawph(0,0,i) / enawph_r(0,0,i))

            ! (251) EF3544NAWPH 
            enawph(2,19,i) = enawph_r(2,19,i) * (enawph(0,0,i) / enawph_r(0,0,i))
            
            ! (252) EF4554NAWPH 
            enawph(2,20,i) = enawph_r(2,20,i) * (enawph(0,0,i) / enawph_r(0,0,i))
            
            ! (253) EF5564NAWPH 
            enawph(2,21,i) = enawph_r(2,21,i) * (enawph(0,0,i) / enawph_r(0,0,i))
            
         end if
            
            ! (254) EF65ONAWPH  
            enawph(2,22,i) = enawph_r(2,22,i) * (enawph(0,0,i) / enawph_r(0,0,i))

         
            ! (255) EFNAWPH 
            enawph(2,0,i) = sum(enawph(2,1:3,i)) + sum(enawph(2,18:22,i))
         

            
         if(i >= lastDataQtr+42) then

            ! (256) EGEFCPS
            if (long_range(i)==0.0d0) then
               egefcps(i) = egefcps(i-1) * 1.0075d0**0.25d0
            else 
               egefcps(i) = egefcps(i-1) * (e_fe(0,0,i) / e_fe(0,0,i-1))
            end if
            
         end if
            
         if (i >= lastDataQtr+42) then 
         
            ! (257) EGFC
               if (long_range(i)==0.0d0) then
                  egfc(i) = egfc(i-1) * 1.0094d0**0.25d0
               else 
                  egfc(i) = egfc(i-1) * (e_fe(0,0,i) / e_fe(0,0,i-1))
               end if
            
         end if
         

         
         
            
         if (i >= lastDataQtr+1) then
         
            ! (258) EGGEFC 
            eggefc(i) = egfc(i) + egefcps(i)
            
            ! (259) EGGESL
            if (long_range(i)==0.0d0) then
               eggesl(i) = eggesl(i-1) * (lc_fe(0,0,i) / lc_fe(0,0,i-4))**0.25d0
            else 
               eggesl(i) = eggesl(i-1) * (e_fe(0,0,i) / e_fe(0,0,i-1))
            end if
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         ! (260) EM1617NAWPH  
         enawph(1,1,i) =  enawph_r(1,1,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         ! (261) EM1819NAWPH  
         enawph(1,2,i) =  enawph_r(1,2,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         ! (262) EM2024NAWPH  
         enawph(1,3,i) =  enawph_r(1,3,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         ! (263) EM2534NAWPH 
         enawph(1,18,i) = enawph_r(1,18,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         ! (264) EM3544NAWPH 
         enawph(1,19,i) = enawph_r(1,19,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         ! (265) EM4554NAWPH 
         enawph(1,20,i) = enawph_r(1,20,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         if(i >= lastDataQtr-10) then
            
            ! (266) EM5564NAWPH 
            enawph(1,21,i) = enawph_r(1,21,i) * (enawph(0,0,i) / enawph_r(0,0,i))
            
         end if
            
         ! (267) EM65ONAWPH 
         enawph(1,22,i) = enawph_r(1,22,i) * (enawph(0,0,i) / enawph_r(0,0,i))
         
         ! (268) EMNAWPH 
         enawph(1,0,i) = sum(enawph(1,1:3,i)) + sum(enawph(1,18:22,i))
            
          if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if(i >= lastDataQtr+1) then

            ! (269) EP 
            ep(i) = e(0,0,i) - eggesl(i) - eggefc(i) - eas(0,0,i) - enas(0,0,i)

         end if
         
          if (isBudgetRun) lastDataQtr = lastDataQtr - 1

      end do

   end subroutine SolveEmployementNonAgriWagePrivHouse

!===============================================================================

   subroutine SolveEquations270_435()

      integer :: i
      
      ! Equations 270-435
!      do i = max(startQtr, lastDataQtr+1), endQtr
      do i = startQtr, endQtr
      
         ! (270) CRAZ1 
         if (i >= lastRaiseDataQtr+1) then 
            if (long_range(i) == 0.0d0) then 
               if (qtr(i) == 1.0d0) then
                  craz1(i) = (0.82429d0 * (awsp(i-6) / awsp(i-10) - 1.0d0) - &
                     0.005d0) 
               else 
                  craz1(i) = 0.0d0
               end if
            else 
               if (qtr(i) == 1.0d0) then 
                  craz1(i) = (awsp(i-6) / awsp(i-10) - 1.0d0) 
               else 
                  craz1(i) = 0.0d0
               end if
            endif
         end if
         
         ! (271) AWEFC_N 
         if (i >= lastDataQtr-6) then         
            if (long_range(i) == 0.0d0) then 
               awefc_n(i) = awefc_n(i-1) * (1.0d0 + 1.0d0 * craz1(i) + 0.00082d0) 
            else 
               awefc_n(i) = awefc_n(i-1) * avg_gdp(i) / avg_gdp(i-1) * &
                  (1.0d0 + ws_to_wss_d(i) / 100.0d0)**0.25d0
            end if
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1         
         
         if (i >= lastDataQtr+2) then
            ! (272) AWSPL 
            awspl(i) = sum(awsp(i-8:i-1)) / 8.0d0
         end if
         
         if (i >= lastDataQtr+1) then
            ! (273) AWSGGESL
            if (long_range(i) == 0.0d0) then 
               awsggesl(i) = awsggesl(i-1) * awspl(i) / awspl(i-1) 
            else 
               awsggesl(i) = awsggesl(i-1) * avg_gdp(i) / avg_gdp(i-1) * &
                  (1.0d0 + ws_to_wss_d(i) / 100.0d0)**0.25d0
            end if
            
            ! (274) WSGGESL 
            wsggesl(i) = awsggesl(i) * eggesl(i)
            
            ! (275) AWSGGEFC 
            if (long_range(i) == 0.0d0) then 
               awsggefc(i) = awsggefc(i-1) * (1.0d0 + 1.0d0 * craz1(i) + 0.0015d0) 
            else 
               awsggefc(i) = awsggefc(i-1) * avg_gdp(i) / avg_gdp(i-1) * &
                  (1.0d0 + ws_to_wss_d(i) / 100.0d0)**0.25d0
            end if
            
            ! (276) WSGGEFC 
            wsggefc(i) = awsggefc(i) * eggefc(i)
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
            
            ! (277) MRAZ
         if (i >= lastRaiseDataQtr+1) then 
            if (long_range(i) == 0.0d0) then 
               if (qtr(i) == 1.0d0) then
                  mraz(i) = 0.82429d0 * (awsp(i-6) / awsp(i-10) - 1.0d0) - 0.005d0
               else 
                  mraz(i) = 0.0d0
               end if
            else 
               if (qtr(i) == 1.0d0) then 
                  mraz(i) = awsp(i-6) / awsp(i-10) - 1.0d0
               else 
                  mraz(i) = 0.0d0
               end if
            end if 
         end if
            
          if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if (i >= lastDataQtr+1) then
            ! (278) AWSGFM 
            if (long_range(i) == 0.0d0) then 
               awsgfm(i) = awsgfm(i-1) * (1.0027d0 + 1.0d0 * mraz(i)) 
            else 
               awsgfm(i) = awsgfm(i-1) * avg_gdp(i) / avg_gdp(i-1) * &
                  (1.0d0 + ws_to_wss_d(i) / 100.0d0)**0.25d0
            end if 
         
            ! (279) WSGFM 
            wsgfm(i) = awsgfm(i) * (edmil(i) + edmil_r(i))

            ! (280) RWSSPBNFXGE 
            rwsspbnfxge(i) = 0.30026d0 * rtp(i-1) + 0.31936d0 + (0.5905 - (0.30026d0 * 1.0d0 + 0.31936d0)) + rwsspbnfxge_adj(i)

         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if (i >= 436) then
            
            ! (281) SOCF_UIFC 
            socf_uifc(i) = (-0.05934d0 * rtp(i) + 0.06165d0) * wsggefc(i)
            
         end if
         
         if (i >= lastDataQtr-2) then
            
            ! (282) SOCF_WC 
            socf_wc(i) = 0.0159d0 * wsggefc(i)
            
         end if
         
         if (i >= lastDataQtr-6) then
            
            ! (283) WEFC_N 
            wefc_n(i) = awefc_n(i) * tefc_n(i)
            
         end if
         
         if (i >= startQtr+4) then
         
            ! (284) OASDIFC_L 
            oasdifc_l(i) = (emptroasi(i) + emptrdi(i)) * 1.04d0 * &
               (wsggefc(i) - wefc_n(i)) * adj_fsa_fc(i)
        
            ! (285) HIFC_L 
            hifc_l(i) = emptrhi(i) * 1.055d0 * wsggefc(i) * adj_fsa_fc(i)
            
            ! (286) SOC_FC 
            soc_fc(i) = socf_uifc(i) + socf_wc(i) + oasdifc_l(i) + hifc_l(i)
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if (i >= lastDataQtr+1) then
            
            ! (287) CPIWMS 
            cpiwms(i) = cpiwms(i-1) * (1.0d0 + ((cpiw_u(i) / &
               cpiw_u(i-4))**0.25d0 - 1.0d0) * cpiwmswt(i))
               
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if (i >= 436) then
            
            ! (288) OLI_GHI_FC 
            oli_ghi_fc(i) = (oli_ghi_fc(i-1) / eggefc(i-1)) * cpiwms(i) / &
               cpiwms(i-1) * eggefc(i) * rgr_ghi(i)
               
         end if
         
         if (i >= lastDataQtr-2) then
            
            ! (289) OLI_GLI_FC 
            oli_gli_fc(i) = 2.0d0 * eggefc(i) * ((wsggefc(i) / eggefc(i)) + &
               2.0d0) * 0.075d0 * 26.0d0 / 1000.0d0
               
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if (i >= lastDataQtr+1) then
            
            ! (290) AWSGEFC 
            if (long_range(i)==0.0d0) then 
               awsgefc(i) = awsgefc(i-1) * (1.0d0 + 1.0d0 * craz1(i) + .0015d0)
            else 
               awsgefc(i) = awsgefc(i-1) * avg_gdp(i) / avg_gdp(i-1) * &
                  (1.0d0 + ws_to_wss_d(i) / 100.0d0)**0.25d0
            end if 
            
            ! (291) WSGEFC 
            wsgefc(i) = awsgefc(i) * egefcps(i)
            
            ! (292) WSGFC 
            wsgfc(i) = wsggefc(i) - wsgefc(i)
            
         end if
         
          if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if(i >= startQtr+4) then
            
            ! (293) OLI_CSRS1 
            oli_csrs1(i) = ((0.174d0 * wsgefc(i) + 0.07d0 * wsgfc(i)) / &
               wsggefc(i)) * wefc_n(i)
               
            ! (294) OLI_FERS1 
            oli_fers1(i) = 0.107d0 * (wsggefc(i) * 0.9d0 - wefc_n(i))
               
            ! (295) OLI_FERSFC 
            oli_fersfc(i) = 0.048d0 * (wsggefc(i) * 0.9d0 - wefc_n(i))
            
         end if
         
         if (i >= lastDataQtr-2) then
         
            ! (296) OLI_RETFC 
            oli_retfc(i) = oli_csrs1(i) + oli_fers1(i) + oli_fersfc(i) + &
               olif_retfco(i)
               
         end if
            
         if(i >= startQtr+4) then
            ! (297) OLI_FC 
            oli_fc(i) = (oli_ghi_fc(i) + oli_gli_fc(i) + oli_retfc(i))
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
            
         if (i >= lastDataQtr+1) then
         
            ! (298) RCWSF 
            rcwsf(i) = 1.0d0 + (soc_fc(i) + oli_fc(i)) / wsggefc(i) + rcwsf_add(i)
            
            ! (299) WSSGFC
            if (long_range(i) == 0.0d0) then 
               wssgfc(i) = rcwsf(i) * wsgfc(i) 
            else 
               wssgfc(i) = (wssgfc(i-1) / (eggefc(i-1) - egefcps(i-1))) * &
                  avg_gdp(i) / avg_gdp(i-1) * (eggefc(i) - egefcps(i))
            end if
            
            ! (300) WSSGGEFC 
            if (long_range(i)==0.0d0) then 
               wssggefc(i) = rcwsf(i) * wsggefc(i) 
            else 
               wssggefc(i) = (wssggefc(i-1) / eggefc(i-1)) * avg_gdp(i) / &
                  avg_gdp(i-1) * eggefc(i)
            end if
         
            ! (301) CFCGFC
            if (long_range(i)==0.0d0) then 
               cfcgfc(i) = wssgfc(i) * rcfcgfc(i) 
            else 
               cfcgfc(i) = cfcgfc(i-1) * wssggefc(i) / wssggefc(i-1)
            end if 
            
            ! (302) GDPGFC 
            gdpgfc(i) = wssgfc(i) + cfcgfc(i)
                  
            ! (303) WSSGEFC
            if (long_range(i)==0.0d0) then 
               wssgefc(i) = rcwsf(i) * wsgefc(i) 
            else 
               wssgefc(i) = wssgefc(i-1) / egefcps(i-1) * avg_gdp(i) / &
                  avg_gdp(i-1) * egefcps(i)
            end if
         
            ! (304) CFCGEFC 
            if (long_range(i)==0.0d0) then 
               cfcgefc(i) = wssgefc(i) * rcfcgefc(i) 
            else 
               cfcgefc(i) = cfcgefc(i-1) * wssggefc(i) / wssggefc(i-1)
            end if 
            
            ! (305) GDPGEFC 
            gdpgefc(i) = wssgefc(i) + cfcgefc(i)
            
            ! (306) GDPGGEFC 
            gdpggefc(i) = gdpgfc(i) + gdpgefc(i)
         
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if(i >= startQtr+4) then
         
            ! (307) OASDISL_L 
            oasdisl_l(i) = (emptroasi(i) + emptrdi(i)) * 0.978d0 * csla(i) * &
                wsggesl(i)
            
            ! (308) HISL_L 
            hisl_l(i) = emptrhi(i) * 1.0d0 * cslhi(i) * wsggesl(i)
            
         end if
         
         if (i >= 436) then 

            ! (309) SOC_UISL 
            soc_uisl(i) = (-0.02821d0 * sum(rtp_2(i-3:i)) / 4.0d0 + 0.03145d0) * &
               wsggesl(i)
               
         end if
         
         if (i >= lastDataQtr-2) then 
            
            ! (310) RSOCSL_WC 
            rsocsl_wc(i) = rsocsl_wc(i-1) - (rsocsl_wc(i-1) - 0.176d0) / 12.0d0
            
            ! (311) RWCWS 
            rwcws(i) = rwcws(i-1) - (rwcws(i-1) - 0.0144d0) / 12.0d0
            
            ! (312) SOC_WCSL 
            soc_wcsl(i) = rsocsl_wc(i) * rwcws(i) * wsggesl(i)
            
         end if
         
         if(i >= startQtr+4) then
         
            ! (313) SOC_SL 
            soc_sl(i) = oasdisl_l(i) + hisl_l(i) + soc_uisl(i) + soc_wcsl(i)
            
         end if
         
         if (i >= lastDataQtr-2) then
            
            ! (314) OLI_GLI_SL 
            oli_gli_sl(i) = 2.0d0 * eggesl(i) * ((wsggesl(i) / eggesl(i)) + &
               2.0d0) * 0.075d0 * 26.0d0 / 1000.0d0
               
         end if
         
         if (i >= 436) then
            
            ! (315) OLI_GHI_SL 
            oli_ghi_sl(i) = (oli_ghi_sl(i-1) / eggesl(i-1)) * cpiwms(i) / &
               cpiwms(i-1) * eggesl(i) * rgr_ghi(i)
               
         end if
         
         if (i >= lastDataQtr-2) then
            
            ! (316) OLI_WCSL 
            oli_wcsl(i) = (1.0d0 - rsocsl_wc(i)) * rwcws(i) * wsggesl(i)
            
            ! (317) OLI_RETSL 
            oli_retsl(i) = wsggesl(i) * (oli_retsl(i-1) / wsggesl(i-1))
            
         end if
         
         if(i >= startQtr+4) then
            
            ! (318) OLI_SL 
            oli_sl(i) = (oli_gli_sl(i) + oli_ghi_sl(i) + oli_wcsl(i) + &
               oli_retsl(i))
               
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
            
         if (i >= lastDataQtr+1) then
            
            ! (319) RCWSSL 
            rcwssl(i) = 1.0d0 + (soc_sl(i) + oli_sl(i)) / wsggesl(i) + rcwssl_add(i)
            
            ! (320) WSSGGESL
            if (long_range(i)==0.0d0) then
               wssggesl(i) = rcwssl(i) * wsggesl(i) 
            else 
               wssggesl(i) = wssggesl(i-1) / eggesl(i-1) * avg_gdp(i) / &
                  avg_gdp(i-1) * eggesl(i)
            end if
            
            ! (321) WSSGSL 
            wssgsl(i) = wssggesl(i) * wssgsl(i-1) / wssggesl(i-1)
            
            ! (322) CFCGSL
            if (long_range(i) == 0.0d0) then 
               cfcgsl(i) = wssgsl(i) * rcfcgsl(i) 
            else 
               cfcgsl(i) = cfcgsl(i-1) * wssggesl(i) / wssggesl(i-1)
            end if
            
            ! (323) GDPGSL 
            gdpgsl(i) = wssgsl(i) + cfcgsl(i)
            
            ! (324) WSSGESL  
            wssgesl(i) = wssggesl(i) - wssgsl(i)
            
            ! (325) CFCGESL
            if (long_range(i)==0.0d0) then
               cfcgesl(i) = wssgesl(i) * rcfcgesl(i) 
            else 
               cfcgesl(i) = cfcgesl(i-1) * wssggesl(i) / wssggesl(i-1)
            end if
            
            ! (326) GDPGESL 
            gdpgesl(i) = wssgesl(i) + cfcgesl(i)

            ! (327) GDPGGESL 
            gdpggesl(i) = gdpgsl(i) + gdpgesl(i)
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if(i >= lastDataQtr-2) then
            
            ! (328) OLI_RETFM 
            oli_retfm(i) = (oli_retfm(i-1) / wsgfm(i-1) - &
               (oli_retfm(i-1) / wsgfm(i-1) - 0.472d0) / 12.0d0) * wsgfm(i)
               
         end if
         
         if(i >= 436) then
            
            ! (329) SOCF_UIFM 
            socf_uifm(i) = max(0.001d0,(-0.05263d0 * (edmil(i) + edmil_r(i) - &
               (edmil(i-1) + edmil_r(i-1))) - 0.03079d0 * rtp(i) + 0.03310d0)) * &
               wsgfm(i)
               
         end if
         
         if (i >= lastDataQtr-2) then
            
            ! (330) SOCF_MIFM 
            socf_mifm(i) = 0.30d0 * cpiwms(i) * (edmil(i) + edmil_r(i))
         
         end if

         if(i >= startQtr+4) then
         
            ! (331) OASDIFM_L 
            oasdifm_l(i) = (emptroasi(i) + emptrdi(i)) * 0.9975d0 * &
                cml(i) * wsgfm(i)
            
            ! (332) HIFM_L 
            hifm_l(i) = emptrhi(i) * 1.0d0 * cml(i) * wsgfm(i)
            
            ! (333) SOC_FM 
            soc_fm(i) = socf_uifm(i) + socf_mifm(i) + oasdifm_l(i) + hifm_l(i)
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if (i >= lastDataQtr+1) then 

            ! (334) RCWSM 
            rcwsm(i) = (1.0d0 + (oli_retfm(i) + soc_fm(i)) / wsgfm(i)) + rcwsm_add(i)
         
            ! (335) WSSGFM  
            if (long_range(i) == 0.0d0) then 
               wssgfm(i) = rcwsm(i) * wsgfm(i) 
            else 
               wssgfm(i) = wssgfm(i-1) / edmil(i-1) * avg_gdp(i) / &
                  avg_gdp(i-1) * edmil(i)
            end if
    
            ! (336) CFCGFM 
            if (long_range(i) == 0.0d0) then 
               cfcgfm(i) = wssgfm(i) * rcfcgfm(i) 
            else 
               cfcgfm(i) = cfcgfm(i-1) * wssgfm(i) / wssgfm(i-1)
            end if
            
            ! (337) GDPGFM 
            gdpgfm(i) = wssgfm(i) + cfcgfm(i)
            
            ! (338) GDPGGE 
            gdpgge(i) = gdpggefc(i) + gdpggesl(i) + gdpgfm(i)
         
            ! (339) GDPPF 
            gdppf(i) = gdppfreal(i) * pgdpaf(i)

            ! (340) WSSPH 
            if (long_range(i) == 0.0d0) then 
               wssph(i) = (((wssph(i-1) / enawph(0,0,i-1)) / (sum(wssp(i-6:i-3) / &
                  ep(i-6:i-3)) / 4.0d0) - 0.41d0) * 0.875d0 + 0.41d0) * &
                  (sum(wssp(i-5:i-2) / ep(i-5:i-2)) / 4.0d0) * enawph(0,0,i) 
            else 
               wssph(i) = avg_gdp(i) / avg_gdp(i-1) * enawph(0,0,i) * &
                  wssph(i-1) / enawph(0,0,i-1)
            end if
            
            ! (341) OOH 
            ooh(i) = ooh(i-1) * (kgdpreal(i) * pgdp(i)) / (kgdpreal(i-1) * pgdp(i-1))
            
            ! (342) GDPPH 
            if (long_range(i) == 0.0d0) then 
               gdpph(i) = wssph(i) + ooh(i) 
            else 
               gdpph(i) = avg_gdp(i) / avg_gdp(i-1) * enawph(0,0,i) * &
                  gdpph(i-1) / enawph(0,0,i-1)
            end if
            
         end if
         
         
         if(i >= lastDataQtr+2) then
            
            ! (343) AWSSPL 
            awsspl(i) = sum(awssp(i-8:i-1)) / 8.0d0
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if (i >= lastDataQtr-2) then
            
            ! (344) AWSSPHS 
            if (long_range(i) == 0.0d0) then 
               awssphs(i) = awssphs(i-1) * awsspl(i) / awsspl(i-1) 
            else 
               awssphs(i) = awssphs(i-1) * avg_gdp(i) / avg_gdp(i-1)
            end if
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if (i >= lastDataQtr+1) then
            
            ! (345) EPHS_EST 
            if (long_range(i) == 0.0d0) then 
               ephs_est(i) = ephs_est(i-1) + 0.275d0 / 4 
            else 
               ephs_est(i) = ephs_est(i-1) * (e_fe(0,0,i) / e_fe(0,0,i-1))
            end if
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if(i >= lastDataQtr-2) then
            
            ! (346) WSSPHS 
            wssphs(i) = awssphs(i) * ephs_est(i)
            
            ! (347) AWSSPES 
            if (long_range(i) == 0.0d0) then 
               awsspes(i) = awsspes(i-1) * awsspl(i) / awsspl(i-1) 
            else 
               awsspes(i) = awsspes(i-1) * avg_gdp(i) / avg_gdp(i-1)
            end if
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if(i >= lastDataQtr+1) then
            
            ! (348) EPES_EST 
            if (long_range(i) == 0.0d0) then 
               epes_est(i) = epes_est(i-1) + 0.075d0 / 4 
            else 
               epes_est(i) = epes_est(i-1) * (e_fe(0,0,i) / e_fe(0,0,i-1))
            end if
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if(i >= lastDataQtr-2) then
            
            ! (349) WSSPES 
            wsspes(i) = awsspes(i) * epes_est(i)
    
            ! (350) AWSSPSS 
            if (long_range(i) == 0.0d0) then 
               awsspss(i) = awsspss(i-1) * awsspl(i) / awsspl(i-1) 
            else 
               awsspss(i) = awsspss(i-1) * avg_gdp(i) / avg_gdp(i-1)
            end if
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if(i >= lastDataQtr+1) then
            
            ! (351) EPSS_EST 
            if (long_range(i) == 0.0d0) then 
               epss_est(i) = epss_est(i-1) + 0.075d0 / 4 
            else 
               epss_est(i) = epss_est(i-1) * (e_fe(0,0,i) / e_fe(0,0,i-1))
            end if
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if(i >= lastDataQtr-2) then
            
            ! (352) WSSPSS 
            wsspss(i) = awsspss(i) * epss_est(i)
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if (i >= lastDataQtr+1) then
            
            ! (353) WSSPNI 
            wsspni(i) = wsspni(i-1) * (wssphs(i) + wsspes(i) + &
               wsspss(i)) / (wssphs(i-1) + wsspes(i-1) + wsspss(i-1))
            
            ! (354) GDPPNI  
            if (long_range(i) == 0.0d0) then 
               gdppni(i) = wsspni(i) / ((wsspni(i-1) / gdppni(i-1) - 0.866d0) * &
                  0.8d0 + 0.866d0) 
            else 
               gdppni(i) = wsspni(i) / 0.866d0
            end if
            
            ! (355) GDPPBNFXGE 
            gdppbnfxge(i) = gdp(i) - gdpgge(i) - gdppf(i) - gdpph(i) - gdppni(i)
            
            ! (356) ENAW 
            enaw(i) = ena(i) - enas(0,0,i) - enau(0,0,i)
            
            ! (357) ENAWPBXGE 
            enawpbxge(i) = enaw(i) - (enawph(0,0,i) + eggefc(i) + eggesl(i) + &
               wsspni(i) / (wssphs(i) + wsspes(i) + wsspss(i)) * (ephs_est(i) + &
               epes_est(i) + epss_est(i)))
            
            if (.not. isBudgetRun) then
            
               ! (358) YNF 
               ynf(i) = ynf(i-1) * (gdppbnfxge(i) / gdppbnfxge(i-1)) * &
                  (enas(0,0,i) / (enas(0,0,i) + enawpbxge(i))) / (enas(0,0,i-1) / &
                  (enas(0,0,i-1) + enawpbxge(i-1))) * ynf_mult(i)
               
            end if
            
            ! (359) WSSPBNFXGE 
            wsspbnfxge(i) = rwsspbnfxge(i) * (gdppbnfxge(i) - ynf(i)) + wsspbnfxge_add(i)
            
            ! (360) WSSPF
            if (long_range(i) == 0.0d0) then 
               wsspf(i) = eaw(0,0,i) * (sum(wssp(i-5:i-2) / ep(i-5:i-2)) / 4.0d0) * &
                  (3.15749d0 / (year(i) - 65.0d0) - 0.43419d0 * rtp(i) + 0.68725d0) + &
                  wsspf_add(i)
            else 
               wsspf(i) = wsspf(i-1) / eaw(0,0,i-1) * avg_gdp(i) / avg_gdp(i-1) * &
                  eaw(0,0,i)
            end if

            ! (361) WSSP  
            wssp(i) = wsspbnfxge(i) + wsspf(i) + wssph(i) + wsspni(i)

         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if (i >= lastDataQtr-2) then 

            ! (362) SOCF_RETRR 
            socf_retrr(i) = 0.20d0 * wsprrb(i)
            
            ! (363) OLI_GLI_P 
            oli_gli_p(i) = 0.0025d0 * ep(i) * awsp(i-1)
            
         end if
         
         if (i >= 436) then
            
            ! (364) OLI_GHI_P 
            oli_ghi_p(i) = oli_ghi_p(i-1) / ep(i-1) * cpiwms(i) / cpiwms(i-1) * &
               ep(i) * rgr_ghi(i)
         
         end if
         
         if(i >= startQtr+4) then
         
            ! (365) ROASDIP_L 
            roasdip_l(i) = (emptroasi(i) + emptrdi(i)) * txrp(i) * cp(i)
                 
            ! (366) RHIP_L 
            rhip_l(i) = emptrhi(i) * 1.0d0 * cp(i)
         
            ! (367) RSOC_UIP 
            rsoc_uip(i) = 0.00109d0 * sum(ru(0,0,i-5:i-2)) / 4.0d0 + &
               0.00045d0 * sum(ru(0,0,i-13:i-10)) / 4.0d0 + 0.00048d0 * &
               sum(ru(0,0,i-21:i-18)) / 4.0d0 - 0.00331d0
      
            ! (368) RSOC_WCP 
            rsoc_wcp(i) = rwcws(i) * rsocsl_wc(i)
            
      
            ! (369) RSOCF_PBG 
            rsocf_pbg(i) = 0.00022d0
         
            ! (370) ROLI_WCP 
            roli_wcp(i) = rwcws(i) * (1.0d0 - rsocsl_wc(i))
      
            ! (371) ROLI_SU 
            roli_su(i) = 0.0005d0
            
         end if
         
         if (i >= lastDataQtr-2) then 

            ! (372) ROLI_PPPS 
            roli_ppps(i) = max(roli_ppps(i-1), 0.00031d0 * year(i) + 0.00866d0)
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if(i >= lastDataQtr+1) then
            
            ! (373) RCWSP 
            rcwsp(i) = wssp(i) / (wssp(i) - socf_retrr(i) - oli_gli_p(i) - &
               oli_ghi_p(i)) * (1.0d0 + roasdip_l(i) + rhip_l(i) + rsoc_uip(i) + &
               rsoc_wcp(i) + rsocf_pbg(i) + roli_wcp(i) + roli_su(i) + roli_ppps(i)) + &
               rcwsp_add(i)
         
            ! (374) WSSGGE 
            wssgge(i) = wssggesl(i) + wssggefc(i) + wssgfm(i)
         
            ! (375) WSS 
            wss(i) = wssp(i) + wssgge(i)
         
            if (.not. isBudgetRun) then
               
               ! (376) WS 
               if (ws_to_wss_dyr(i) == 0.0d0) then 
                  ws(i) = wsggesl(i) + wsggefc(i) + wsgfm(i) + wssp(i) / rcwsp(i) 
               else 
                  ws(i) = wss(i) * ws(i-1) / wss(i-1) * (1.0d0 + ws_to_wss_d(i) / &
                     100.0d0)**0.25d0
               end if
            
            end if

            ! (377) AWSE 
            awse(i) = ws(i) / (e(0,0,i) + edmil(i) - eas(0,0,i) - enas(0,0,i))
         
            ! (378) AWSGFC 
            awsgfc(i) = wsgfc(i) / egfc(i)
         
            ! (379) WSP 
            wsp(i) = ws(i) - wsggesl(i) - wsggefc(i) - wsgfm(i)
         
            ! (380) AWSP 
            awsp(i) = wsp(i) / ep(i)
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1

         if(i >= lastDataQtr-2) then
                  
            ! (381) WSPF 
            if (long_range(i) == 0.0d0) then 
               wspf(i) = wsspf(i) * (sum(wsp(i-12:i-1) / wssp(i-12:i-1)) / &
                  12.0d0 + 0.015d0) 
            else 
               wspf(i) = (wspf(i-1) / wsspf(i-1)) * (wsp(i-1) / wssp(i-1)) / &
                  (wsp(i-2) / wssp(i-2)) * wsspf(i)
            end if
         
            ! (382) AWSPF 
            awspf(i) = wspf(i) / eaw(0,0,i)
                 
         end if

         if(i >= startQtr+4) then
         
            ! (383) WSPH 
            if (long_range(i) == 0.0d0) then 
               wsph(i) = wssph(i) / (1.0d0 + cph(i) * 1.0d0 * (emptroasi(i) + &
                  emptrdi(i) + emptrhi(i))) 
            else 
               wsph(i) = (awsph(i-1) * enawph(0,0,i-1) / wssph(i-1)) * &
                  (1.0d0 + ws_to_wss_d(i) / 100)**0.25d0 * wssph(i)
            end if
            
         end if
         
         if (i >= lastDataQtr-2) then 

            ! (384) AWSPH 
            awsph(i) = wsph(i) / enawph(0,0,i)
         
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if(i >= lastDataQtr+1) then

            ! (385) AWSSP 
            awssp(i) = wssp(i) / ep(i)
         
            ! (386) AWSUI 
            awsui(i) = (ws(i) - wsggefc(i) - wsgfm(i)) / (e(0,0,i) - &
               eggefc(i) - eas(0,0,i) - enas(0,0,i))
         
            ! (387) GDPGF 
            gdpgf(i) = gdpgfc(i) + gdpgfm(i)
         
            ! (388) GDPG 
            gdpg(i) = gdpgf(i) + gdpgsl(i)
         
            ! (389) GDPGE 
            gdpge(i) = gdpgefc(i) + gdpgesl(i)
         
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if (i >= lastDataQtr+6) then
         
           ! (390) TAXMAX  
            if (qtr(i) == 1.0d0) then 
               taxmax(i) = 300.0d0 * nint((sum(awse(i-8:i-5)) / 4.0d0) / &
                  (sum(awse(i-12:i-9)) / 4.0d0) * 1000.0d0 * taxmax(i-1) / &
                  300.0d0 + 0.5d0) / 1000.0d0 
            else 
               taxmax(i) = taxmax(i-1)
            end if
            
         end if

         if (i >= lastDataQtr-2) then 
            
             ! (391) WSPNI 
            if (long_range(i) == 0.0d0) then 
               wspni(i) = wsspni(i) * (wspni(i-1) / wsspni(i-1)) * &
                  ((wsp(i-1) / wssp(i-1)) / (wsp(i-9) / wssp(i-9)))**0.125d0 
            else 
               wspni(i) = wsspni(i) * (wspni(i-1) / wsspni(i-1)) * &
                  (1 + ws_to_wss_d(i) / 100)**0.25d0
            end if
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
         
         if(i >= lastDataQtr+1) then

            ! (392) WSSGF 
            wssgf(i) = wssgfc(i) + wssgfm(i)
         
            ! (393) WSSG 
            wssg(i) = wssgf(i) + wssgsl(i)
         
            ! (394) WSSGE 
            wssge(i) = wssgefc(i) + wssgesl(i)
         
            ! (395) AYF_K 
            ayf_k(i) = ((yf(i-1) / eas(0,0,i-1)) / (wsspf(i-1) / eaw(0,0,i-1)) - &
               5.0d0) * 0.8d0 + 5.0d0
            
            if (.not. isBudgetRun) then
            
               ! (396) YF 
               yf(i) = ayf_k(i) * (wsspf(i) / eaw(0,0,i)) * eas(0,0,i)
               
            end if
         
            ! (397) AWSSPBNFXGE 
            awsspbnfxge(i) = wsspbnfxge(i) / enawpbxge(i)
         
            ! (398) AWSSPF 
            awsspf(i) = wsspf(i) / eaw(0,0,i)
         
            ! (399) AYF 
            ayf(i) = yf(i) / eas(0,0,i)
          
            ! (400) AYNF 
            aynf(i) = ynf(i) / enas(0,0,i)
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if(i >= startQtr+4) then
         
            ! (401) AYNF_K 
            aynf_k(i) = ((ynf(i-1) / enas(0,0,i-1) / (wsspbnfxge(i-1) / &
               enawpbxge(i-1))) - 1.65d0) * 0.9d0 + 1.65d0
            
            ! (402) CR_UI 
            cr_ui(i) = 0.775d0
            
            ! (403) ENAWSPBXGE 
            enawspbxge(i) = enawpbxge(i) + enas(0,0,i)
            
         end if
         
         if (i >= lastDataQtr+2) then
            
            ! (404) TMAXUI_SL 
            tmaxui_sl(i) = tmaxui_sl(i-1) * awsui(i-1) / awsui(i-2)
            
         end if
         
         if(i >= startQtr+4) then
         
            ! (405) RELMAX_UI  
            relmax_ui(i) = tmaxui_sl(i) / awsui(i-1) / 1000.0d0
    
            ! (406) TRATIO_UI 
            tratio_ui(i) = 0.96996d0 * relmax_ui(i) - 0.13744d0 * &
               (sum(rtp(i-4:i-1)) / 4.0d0) + 0.10368d0 * &
               (sum(rtp(i-8:i-5)) / 4.0d0) + 0.04887d0
               
            ! (407) TRATE_UI 
            trate_ui(i) = 0.00143d0 * (sum(ru(0,0,i-8:i-5)) / 4.0d0) + &
               0.00128d0 * (sum(ru(0,0,i-12:i-9)) / 4.0d0) + 0.00057d0 * &
               (sum(ru(0,0,i-16:i-13)) / 4.0d0) + 0.00356d0
            
         end if
         
         if (i >= lastDataQtr-2) then 

            ! (408) RUIWS1 
            ruiws1(i) = cr_ui(i) * tratio_ui(i) * trate_ui(i)
            
            ! (409) RUIWS2 
            ruiws2(i) = 0.32476d0 * (sum(ruiws1(i-11:i-8) * (wsp(i-11:i-8) - &
               wsprrb(i-11:i-8) + wsggesl(i-11:i-8))) / 4.0d0) / &
               (wsp(i-1) - wsprrb(i-1) + wsggesl(i-1))
         
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1

         if(i >= lastDataQtr+1) then
         
            ! (410) WSD 
            wsd(i) = ws(i)
         
            ! (411) WSDP 
            wsdp(i) = wsd(i) - wsggesl(i) - wsggefc(i) - wsgfm(i)
            
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
         
         if(i >= startQtr+4) then
         
            ! (412) OLI_GGE 
            oli_gge(i) = oli_fc(i) + oli_sl(i) + oli_retfm(i)
            
         end if
            
         if (i >= lastDataQtr-2) then 

            ! (413) OLI_WCP 
            oli_wcp(i) = roli_wcp(i) * wsp(i)
            
            ! (414) OLI_SU 
            oli_su(i) = roli_su(i) * wsp(i)
            
            ! (415) OLI_PPPS 
            oli_ppps(i) = roli_ppps(i) * wsp(i)
            
         end if
         
         if(i >= startQtr+4) then
            
            ! (416) OLI_P 
            oli_p(i) = oli_wcp(i) + oli_su(i) + oli_ghi_p(i) + oli_gli_p(i) + &
               oli_ppps(i)
         
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
            
         if(i >= lastDataQtr+1) then
            
            ! (417) OLI  
            oli(i)= oli_gge(i) + oli_p(i)
         
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1

         if (i >= startQtr+4) then
         
            ! (418) SOC_GGE 
            soc_gge(i) = soc_fc(i) + soc_fm(i) + soc_sl(i)
            
         end if

         if (i >= 436) then
         
            ! (419) SOC_UIP 
            soc_uip(i) = rsoc_uip(i) * wsp(i)
            
         end if
         
         if(i >= lastDataQtr-2) then
         
            ! (420) SOC_WCP 
            soc_wcp(i) = rsoc_wcp(i) * wsp(i)
            
         end if
         
         if(i >= startQtr+4) then
            
            ! (421) OASDIP_L 
            oasdip_l(i) = roasdip_l(i) * wsp(i)
               
            ! (422) HIP_L 
            hip_l(i) = rhip_l(i) * wsp(i)
            
         end if
         
         if(i >= lastDataQtr-2) then
            
            ! (423) SOCF_PBG 
            socf_pbg(i) = rsocf_pbg(i) * wsp(i)
            
         end if
         
         if(i >= startQtr+4) then
            
            ! (424) SOC_P 
            soc_p(i) = soc_uip(i) + soc_wcp(i) + oasdip_l(i) + &
               hip_l(i) + socf_pbg(i) + socf_retrr(i)
               
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr + 1
            
         if(i >= lastDataQtr+1) then
         
            ! (425) SOC 
            soc(i) = soc_gge(i) + soc_p(i)
         
         end if
         
         if (isBudgetRun) lastDataQtr = lastDataQtr - 1
          
          if(i >= startQtr+4) then

            ! (426) OLI_PPS 
            oli_pps(i) = oli_ppps(i) + oli_retfc(i) + oli_retfm(i) + oli_retsl(i)
            
            ! (427) OLI_WC 
            oli_wc(i) = oli_wcp(i) + oli_wcsl(i)
            
            ! (428) OLI_GHI 
            oli_ghi(i) = oli_ghi_p(i) + oli_ghi_fc(i) + oli_ghi_sl(i)
            
            ! (429) OLI_GLI 
            oli_gli(i) = oli_gli_p(i) + oli_gli_fc(i) + oli_gli_sl(i)
         
            ! (430) SOCSL_WC 
            socsl_wc(i) = soc_wcsl(i) + soc_wcp(i)
            
            ! (431) SOCF_UIFED 
            socf_uifed(i) = socf_uifc(i) + socf_uifm(i)
            
            ! (432) SOCF_UIS 
            socf_uis(i) = (soc_uip(i) + soc_uisl(i)) * ruiws1(i) / &
                   (ruiws1(i) + ruiws2(i))
            
            ! (433) SOCF_UIF 
            socf_uif(i) = (soc_uip(i) + soc_uisl(i)) - socf_uis(i)
            
            ! (434) SOCF_OASDI 
            socf_oasdi(i) = oasdip_l(i) + oasdisl_l(i) + oasdifc_l(i) + &
                oasdifm_l(i)
            
            ! (435) SOCF_HI 
            socf_hi(i) = hip_l(i) + hisl_l(i) + hifc_l(i) + hifm_l(i)
         
         end if
   
      end do

   end subroutine SolveEquations270_435

!===============================================================================

end module EconModSol2EquationsMod