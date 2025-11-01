module EconModSol1EquationsMod

   use EconParMod
   use EconModSol1VarMod
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSol1EquationsMain
   
contains

!===============================================================================

   subroutine EconModSol1EquationsMain()
   
      logical, parameter :: fullEmployment = .true.
   
      call SolveUnemploymentRate()                    ! Equations   1- 62
      call SolveUnemploymentRate(fullEmployment)      ! Equations  63-118
      call SolveLaborForcePartRateMales1619()         ! Equations 119-122
      call SolveLaborForcePartRateMales2054()         ! Equations 123-178
      
      if (isBudgetRun) lastDataQtr = lastDataQtr - 1
      call SolveLaborForcePartRate5574()              ! Equations 179-248
      call SolveLaborForcePartRateMales75100()        ! Equations 249-303
      if (isBudgetRun) lastDataQtr = lastDataQtr + 1
      
      call SolveLaborForcePartRateFemales1619()       ! Equations 304-307
      call SolveLaborForcePartRateFemles2054()        ! Equations 308-423
      
      if (isBudgetRun) lastDataQtr = lastDataQtr - 1
      call SolveLaborForcePartRateFemales5574()       ! Equations 424-433
      call SolveLaborForcePartRate75100()             ! Equations 434-488
      if (isBudgetRun) lastDataQtr = lastDataQtr + 1
      
      call SolveLaborForcePartRate1674(fullEmployment)! Equations 489-514
      call SolveLaborForcePartRatePrelim()            ! Equations 515-520
      
   end subroutine EconModSol1EquationsMain
   
!===============================================================================

   subroutine SolveUnemploymentRate(fullEmployment)
   
      logical, optional :: fullEmployment
      integer :: i, sex, ageGrp
      
      if (.not. present(fullEmployment)) then

         ! Equations   1- 62
         do i = max(startQtr, lastDataQtr+1), endQtr

            ! (1) RM1617_P
            ru_p(1,1,i) = ru_p(1,1,i-1) + (-36.2076d0 * drtp(i) - &
               14.2816d0 * drtp_1(i) - 26.6756d0 * drtp_2(i) - &
               16.9202d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,1,i)
               
            ! (2) RM1819_P
            ru_p(1,2,i) = ru_p(1,2,i-1) + (-48.4227d0 * drtp(i) - &
               25.8766d0 * drtp_1(i) - 21.7466d0 * drtp_2(i) + &
                1.1551d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,2,i) 
                
            ! (3) RM2024_P 
            ru_p(1,3,i) = ru_p(1,3,i-1) + (-51.6518d0 * drtp(i) - &
               16.6465d0 * drtp_1(i) - 13.1350d0 * drtp_2(i) - &
               10.9309d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,3,i)
               
            ! (4) RM2529_P 
            ru_p(1,4,i) = ru_p(1,4,i-1) + (-37.9533d0 * drtp(i) - &
               17.3941d0 * drtp_1(i) - 14.9170d0 * drtp_2(i) - &
                7.0513d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,4,i)
                
            ! (5) RM3034_P 
            ru_p(1,5,i) = ru_p(1,5,i-1) + (-23.6417d0 * drtp(i) - &
               14.1284d0 * drtp_1(i) - 7.5008d0 * drtp_2(i) - &
                9.7232d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,5,i)
               
            ! (6) RM3539_P 
            ru_p(1,6,i) = ru_p(1,6,i-1) + (-27.6828d0 * drtp(i) - &
                5.4850d0 * drtp_1(i) - 10.8974d0 * drtp_2(i) - &
                9.8932d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,6,i)
                
            ! (7) RM4044_P 
            ru_p(1,7,i) = ru_p(1,7,i-1) + (-14.6558d0 * drtp(i) - &
               14.9735d0 * drtp_1(i) - 8.2594d0 * drtp_2(i) - &
                5.5023d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,7,i)
                
            ! (8) RM4549_P 
            ru_p(1,8,i) = ru_p(1,8,i-1) + (-20.7806d0 * drtp(i) - &
               11.5121d0 * drtp_1(i) - 9.9409d0 * drtp_2(i) + &
                1.5480d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,8,i)
                
            ! (9) RM5054_P 
            ru_p(1,9,i) = ru_p(1,9,i-1) + (-19.3341d0 * drtp(i) - &
                9.5336d0 * drtp_1(i) - 8.8784d0 * drtp_2(i) - &
                7.6218d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,9,i)

            ! (10) RM5559_P 
            ru_p(1,10,i) = ru_p(1,10,i-1) + (-25.9031d0 * drtp(i) - &
               11.4442d0 * drtp_1(i) - 4.5421d0 * drtp_2(i) + &
               0.55815d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,10,i)
   
            ! (11) RM6064_P 
            ru_p(1,11,i) = ru_p(1,11,i-1) + (1.3133d0 * drtp(i) - &
               12.9625d0 * drtp_1(i) - 2.4816d0 * drtp_2(i) - &
               14.4797d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,11,i)
               
            ! (12) RM6569_P 
            ru_p(1,12,i) = ru_p(1,12,i-1) + (-19.5151d0 * drtp(i) + &
                4.9785d0 * drtp_1(i) - 13.3449d0 * drtp_2(i) + &
                2.4706d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,12,i)
                
            ! (13) RM7074_P 
            ru_p(1,13,i) = ru_p(1,13,i-1) + (4.1938d0 * drtp(i) - &
                5.9012d0 * drtp_1(i) - 27.0406d0 * drtp_2(i) + &
                7.0400d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,13,i)
                
            ! (14) RM75O_P  
            ru_p(1,14,i) = ru_p(1,14,i-1) + (-12.1042d0 * drtp(i) - &
               15.6142d0 * drtp_1(i) + 7.06185d0 * drtp_2(i) - &
                2.5738d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(1,14,i)
                
            ! (15) RF1617_P 
            ru_p(2,1,i) = ru_p(2,1,i-1) + (-27.3243d0 * drtp(i) + &
               13.4173d0 * drtp_1(i) - 50.4583d0 * drtp_2(i) - &
               0.3678d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,1,i)
               
            ! (16) RF1819_P 
            ru_p(2,2,i) = ru_p(2,2,i-1) + (-42.6358d0 * drtp(i) - &
               13.6261d0 * drtp_1(i) + 9.5650d0 * drtp_2(i) - &
               31.4798d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,2,i)
               
            ! (17) RF2024_P 
            ru_p(2,3,i) = ru_p(2,3,i-1) + (-16.9400d0 * drtp(i) - &
               13.2669d0 * drtp_1(i) - 7.8323d0 * drtp_2(i) - &
                8.6887d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,3,i)
                
            ! (18) RF2529_P 
            ru_p(2,4,i) = ru_p(2,4,i-1) + (-15.5798d0 * drtp(i) - &
               11.9097d0 * drtp_1(i) - 9.8424d0 * drtp_2(i) - &
                2.7555d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,4,i)
                
            ! (19) RF3034_P 
            ru_p(2,5,i) = ru_p(2,5,i-1) + (-12.5396d0 * drtp(i) - &
                1.6601d0 * drtp_1(i) - 21.0289d0 * drtp_2(i) + &
                0.0881d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,5,i)
                
            ! (20) RF3539_P 
            ru_p(2,6,i) = ru_p(2,6,i-1) + (-21.9314d0 * drtp(i) - &
                3.0139d0 * drtp_1(i) - 7.8723d0 * drtp_2(i) - &
                6.4785d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,6,i)
                
            ! (21) RF4044_P 
            ru_p(2,7,i) = ru_p(2,7,i-1) + (-7.7893d0 * drtp(i) - &
                7.7152d0 * drtp_1(i) - 5.7849d0 * drtp_2(i) - &
                2.7298d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,7,i)
                
            ! (22) RF4549_P 
            ru_p(2,8,i) = ru_p(2,8,i-1) + (-7.8747d0 * drtp(i) - &
               12.5212d0 * drtp_1(i) + 3.56675d0 * drtp_2(i) - &
                5.4812d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,8,i)
                
            ! (23) RF5054_P 
            ru_p(2,9,i) = ru_p(2,9,i-1) + (-9.7818d0 * drtp(i) - &
                3.1242d0 * drtp_1(i) - 14.0327d0 * drtp_2(i) - &
                4.0364d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,9,i)
                
            ! (24) RF5559_P 
            ru_p(2,10,i) = ru_p(2,10,i-1) + (-2.4665d0 * drtp(i) - &
                4.8191d0 * drtp_1(i) - 11.4418d0 * drtp_2(i) - &
                3.5854d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,10,i)
                
            ! (25) RF6064_P 
            ru_p(2,11,i) = ru_p(2,11,i-1) + (-22.1139d0 * drtp(i) + &
                4.5539d0 * drtp_1(i) - 6.2406d0 * drtp_2(i) - &
                7.0337d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,11,i)
                
            ! (26) RF6569_P 
            ru_p(2,12,i) = ru_p(2,12,i-1) + (9.2541d0 * drtp(i) + &
                7.6281d0 * drtp_1(i) - 22.5230d0 * drtp_2(i) + &
                0.2738d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,12,i)
                
            ! (27) RF7074_P 
            ru_p(2,13,i) = ru_p(2,13,i-1) + (24.2237d0 * drtp(i) + &
                8.3386d0 * drtp_1(i) - 13.5317d0 * drtp_2(i) - &
                8.1855d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,13,i)
                
            ! (28) RF75O_P  
            ru_p(2,14,i) = ru_p(2,14,i-1) + (-28.8294d0 * drtp(i) + &
               55.5911d0 * drtp_1(i) - 31.0676d0 * drtp_2(i) - &
               15.8580d0 * drtp_3(i)) * 50.00d0 / 44.72d0 + ru_p_add(2,14,i)
               
         end do
               
         do i = startQtr, endQtr
         
            ! (29) RUM_ASA_P 
            ru_asa_p(1,0,i) = (ru_p(1,1,i) * lc_by(1,1,i) + &
               ru_p(1,2,i) * lc_by(1,2,i) + &
               ru_p(1,3,i) * lc_by(1,3,i) + &
               ru_p(1,4,i) * lc_by(1,4,i) + &
               ru_p(1,5,i) * lc_by(1,5,i) + &
               ru_p(1,6,i) * lc_by(1,6,i) + &
               ru_p(1,7,i) * lc_by(1,7,i) + & 
               ru_p(1,8,i) * lc_by(1,8,i) + &
               ru_p(1,9,i) * lc_by(1,9,i) + &
               ru_p(1,10,i) * lc_by(1,10,i) + &
               ru_p(1,11,i) * lc_by(1,11,i) + &
               ru_p(1,12,i) * lc_by(1,12,i) + &
               ru_p(1,13,i) * lc_by(1,13,i) + &
               ru_p(1,14,i) * lc_by(1,17,i)) / lc_by(1,0,i)

            ! (30) RUF_ASA_P 
            ru_asa_p(2,0,i) = (ru_p(2,1,i) * lc_by(2,1,i) + &
               ru_p(2,2,i) * lc_by(2,2,i) + &
               ru_p(2,3,i) * lc_by(2,3,i) + &
               ru_p(2,4,i) * lc_by(2,4,i) + &
               ru_p(2,5,i) * lc_by(2,5,i) + &
               ru_p(2,6,i) * lc_by(2,6,i) + &
               ru_p(2,7,i) * lc_by(2,7,i) + & 
               ru_p(2,8,i) * lc_by(2,8,i) + &
               ru_p(2,9,i) * lc_by(2,9,i) + &
               ru_p(2,10,i) * lc_by(2,10,i) + &
               ru_p(2,11,i) * lc_by(2,11,i) + &
               ru_p(2,12,i) * lc_by(2,12,i) + &
               ru_p(2,13,i) * lc_by(2,13,i) + &
               ru_p(2,14,i) * lc_by(2,17,i)) / lc_by(2,0,i)

            ! (31) RU_ASA_P 
            ru_asa_p(0,0,i) = (ru_asa_p(1,0,i) * lc_by(1,0,i) + &
               ru_asa_p(2,0,i) * lc_by(2,0,i)) / lc_by(0,0,i)
               
         end do
               
         do i = max(startQtr, lastDataQtr+1), endQtr               
               
            ! (32) - (59) RM1617 ... RF75O
            do sex = 1, 2
               do ageGrp = 1, 14
                  ru(sex,ageGrp,i) = ru_p(sex,ageGrp,i) * &
                     (1d0 + ru_asa_adj(i) / ru_asa_p(0,0,i))
               end do
            end do
            
         end do
               
         do i = startQtr, endQtr

            ! (60) RUM_ASA
            ru_asa(1,0,i) = (ru(1,1,i) * lc_by(1,1,i) + &
               ru(1,2,i) * lc_by(1,2,i) + &
               ru(1,3,i) * lc_by(1,3,i) + &
               ru(1,4,i) * lc_by(1,4,i) + &
               ru(1,5,i) * lc_by(1,5,i) + &
               ru(1,6,i) * lc_by(1,6,i) + &
               ru(1,7,i) * lc_by(1,7,i) + & 
               ru(1,8,i) * lc_by(1,8,i) + &
               ru(1,9,i) * lc_by(1,9,i) + &
               ru(1,10,i) * lc_by(1,10,i) + &
               ru(1,11,i) * lc_by(1,11,i) + &
               ru(1,12,i) * lc_by(1,12,i) + &
               ru(1,13,i) * lc_by(1,13,i) + &
               ru(1,14,i) * lc_by(1,17,i)) / lc_by(1,0,i)

            ! (61) RUF_ASA 
            ru_asa(2,0,i) = (ru(2,1,i) * lc_by(2,1,i) + &
               ru(2,2,i) * lc_by(2,2,i) + &
               ru(2,3,i) * lc_by(2,3,i) + &
               ru(2,4,i) * lc_by(2,4,i) + &
               ru(2,5,i) * lc_by(2,5,i) + &
               ru(2,6,i) * lc_by(2,6,i) + &
               ru(2,7,i) * lc_by(2,7,i) + & 
               ru(2,8,i) * lc_by(2,8,i) + &
               ru(2,9,i) * lc_by(2,9,i) + &
               ru(2,10,i) * lc_by(2,10,i) + &
               ru(2,11,i) * lc_by(2,11,i) + &
               ru(2,12,i) * lc_by(2,12,i) + &
               ru(2,13,i) * lc_by(2,13,i) + &
               ru(2,14,i) * lc_by(2,17,i)) / lc_by(2,0,i)
               
            ! (62) RU_ASA 
            ru_asa(0,0,i) = (ru_asa(1,0,i) * lc_by(1,0,i) + &
               ru_asa(2,0,i) * lc_by(2,0,i)) / lc_by(0,0,i)
               
         end do

      else
      
         ! Equations  63-90 are not needed because they can be derived
         ! if they are needed later
         ! For example, (63) drm1617_fe = (91) rm1617_fe - (32) rm1617
         !              (N) = (N+28) - (N-31)
         
         ! Equations  91-118
         do i = startQtr, endQtr
            ! (91) RM1617_FE 
            ru_fe(1,1,i) = ru(1,1,i) + (-36.2076d0 * (1 - rtp(i)) - &
               14.2816d0 * (1 - rtp_1(i)) - 26.6756d0 * (1 - rtp_2(i)) - &
               16.9202d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
          
            ! (92) RM1819_FE 
            ru_fe(1,2,i) = ru(1,2,i) + (-48.4227d0 * (1 - rtp(i)) - &
               25.8766d0 * (1 - rtp_1(i)) - 21.7466d0 * (1 - rtp_2(i)) + &
                1.1551d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0

            ! (93) RM2024_FE 
            ru_fe(1,3,i) = ru(1,3,i) + (-51.6518d0 * (1 - rtp(i)) - &
               16.6465d0 * (1 - rtp_1(i)) - 13.1350d0 * (1 - rtp_2(i)) - &
               10.9309d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (94) RM2529_FE 
            ru_fe(1,4,i) = ru(1,4,i) + (-37.9533d0 * (1 - rtp(i)) - &
               17.3941d0 * (1 - rtp_1(i)) - 14.9170d0 * (1 - rtp_2(i)) - &
                7.0513d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
                
            ! (95) RM3034_FE 
            ru_fe(1,5,i) = ru(1,5,i) + (-23.6417d0 * (1 - rtp(i)) - &
               14.1284d0 * (1 - rtp_1(i)) - 7.5008d0 * (1 - rtp_2(i)) - &
                9.7232d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
                
            ! (96) RM3539_FE 
            ru_fe(1,6,i) = ru(1,6,i) + (-27.6828d0 * (1 - rtp(i)) - &
               5.4850d0 * (1 - rtp_1(i)) - 10.8974d0 * (1 - rtp_2(i)) - &
               9.8932d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (97) RM4044_FE 
            ru_fe(1,7,i) = ru(1,7,i) + (-14.6558d0 * (1 - rtp(i)) - &
               14.9735d0 * (1 - rtp_1(i)) - 8.2594d0 * (1 - rtp_2(i)) - &
               5.5023d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0

            ! (98) RM4549_FE 
            ru_fe(1,8,i) = ru(1,8,i) + (-20.7806d0 * (1 - rtp(i)) - &
               11.5121d0 * (1 - rtp_1(i)) - 9.9409d0 * (1 - rtp_2(i)) + &
                1.5480d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
                
            ! (99) RM5054_FE 
            ru_fe(1,9,i) = ru(1,9,i) + (-19.3341d0 * (1 - rtp(i)) - &
               9.5336d0 * (1 - rtp_1(i)) - 8.8784d0 * (1 - rtp_2(i)) - &
               7.6218d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (100) RM5559_FE 
            ru_fe(1,10,i) = ru(1,10,i) + (-25.9031d0 * (1 - rtp(i)) - &
               11.4442d0 * (1 - rtp_1(i)) - 4.5421d0 * (1 - rtp_2(i)) + &
                0.55815d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
                
            ! (101) RM6064_FE 
            ru_fe(1,11,i) = ru(1,11,i) + (1.3133d0 * (1 - rtp(i)) - &
               12.9625d0 * (1 - rtp_1(i)) - 2.4816d0 * (1 - rtp_2(i)) - &
               14.4797d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (102) RM6569_FE 
            ru_fe(1,12,i) = ru(1,12,i) + (-19.5151d0 * (1 - rtp(i)) + &
               4.9785d0 * (1 - rtp_1(i)) - 13.3449d0 * (1 - rtp_2(i)) + &
               2.4706d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (103) RM7074_FE 
            ru_fe(1,13,i) = ru(1,13,i) + (4.1938d0 * (1 - rtp(i)) - &
               5.9012d0 * (1 - rtp_1(i)) - 27.0406d0 * (1 - rtp_2(i)) + &
               7.0400d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (104) RM75O_FE  
            ru_fe(1,14,i) = ru(1,14,i) + (-12.1042d0 * (1 - rtp(i)) - &
               15.6142d0 * (1 - rtp_1(i)) + 7.06185d0 * (1 - rtp_2(i)) - &
                2.5738d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0

            ! (105) RF1617_FE 
            ru_fe(2,1,i)  = ru(2,1,i) + (-27.3243d0 * (1 - rtp(i)) + &
               13.4173d0 * (1 - rtp_1(i)) - 50.4583d0 * (1 - rtp_2(i)) - &
                0.3678d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
                
            ! (106) RF1819_FE 
            ru_fe(2,2,i) = ru(2,2,i) + (-42.6358d0 * (1 - rtp(i)) - &
               13.6261d0 * (1 - rtp_1(i)) + 9.5650d0 * (1 - rtp_2(i)) - &
               31.4798d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (107) RF2024_FE 
            ru_fe(2,3,i) = ru(2,3,i) + (-16.9400d0 * (1 - rtp(i)) - &
               13.2669d0 * (1 - rtp_1(i)) - 7.8323d0 * (1 - rtp_2(i)) - &
                8.6887d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
                
            ! (108) RF2529_FE 
            ru_fe(2,4,i) = ru(2,4,i) + (-15.5798d0 * (1 - rtp(i)) - &
               11.9097d0 * (1 - rtp_1(i)) - 9.8424d0 * (1 - rtp_2(i)) - &
                2.7555d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
                
            ! (109) RF3034_FE 
            ru_fe(2,5,i) = ru(2,5,i) + (-12.5396d0 * (1 - rtp(i)) - &
               1.6601d0 * (1 - rtp_1(i)) - 21.0289d0 * (1 - rtp_2(i)) + &
               0.0881d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (110) RF3539_FE 
            ru_fe(2,6,i) = ru(2,6,i) + (-21.9314d0 * (1 - rtp(i)) - &
               3.0139d0 * (1 - rtp_1(i)) - 7.8723d0 * (1 - rtp_2(i)) - &
               6.4785d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (111) RF4044_FE 
            ru_fe(2,7,i) = ru(2,7,i) + (-7.7893d0 * (1 - rtp(i)) - &
               7.7152d0 * (1 - rtp_1(i)) - 5.7849d0 * (1 - rtp_2(i)) - &
               2.7298d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (112) RF4549_FE 
            ru_fe(2,8,i) = ru(2,8,i) + (-7.8747d0 * (1 - rtp(i)) - &
               12.5212d0 * (1 - rtp_1(i)) + 3.56675d0 * (1 - rtp_2(i)) - &
                5.4812d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
                
            ! (113) RF5054_FE 
            ru_fe(2,9,i) = ru(2,9,i) + (-9.7818d0 * (1 - rtp(i)) - &
               3.1242d0 * (1 - rtp_1(i)) - 14.0327d0 * (1 - rtp_2(i)) - &
               4.0364d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (114) RF5559_FE 
            ru_fe(2,10,i) = ru(2,10,i) + (-2.4665d0 * (1 - rtp(i)) - &
               4.8191d0 * (1 - rtp_1(i)) - 11.4418d0 * (1 - rtp_2(i)) - &
               3.5854d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (115) RF6064_FE 
            ru_fe(2,11,i) = ru(2,11,i) + (-22.1139d0 * (1 - rtp(i)) + &
               4.5539d0 * (1 - rtp_1(i)) - 6.2406d0 * (1 - rtp_2(i)) - &
               7.0337d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (116) RF6569_FE 
            ru_fe(2,12,i) = ru(2,12,i) + (9.2541d0 * (1 - rtp(i)) + &
               7.6281d0 * (1 - rtp_1(i)) - 22.5230d0 * (1 - rtp_2(i)) + &
               0.2738d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (117) RF7074_FE 
            ru_fe(2,13,i) = ru(2,13,i) + (24.2237d0 * (1 - rtp(i)) + &
               8.3386d0 * (1 - rtp_1(i)) - 13.5317d0 * (1 - rtp_2(i)) - &
               8.1855d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0
               
            ! (118) RF75O_FE  
            ru_fe(2,14,i) = ru(2,14,i) + (-28.8294d0 * (1 - rtp(i)) + &
               55.5911d0 * (1 - rtp_1(i)) - 31.0676d0 * (1 - rtp_2(i)) - &
               15.8580d0 * (1 - rtp_3(i))) * 50.00d0 / 44.72d0

            !(63 - 90)
            dru_fe(1:2,1:14,i) = ru_fe(1:2,1:14,i) - ru(1:2,1:14,i)

         end do

      end if
   
   
   end subroutine SolveUnemploymentRate
   
!===============================================================================
   
   subroutine SolveLaborForcePartRateMales1619()
   
      integer :: i
      
      ! Equations 119-122
      do i = startQtr, endQtr

         ! (119) PM1617_P 
         p_p(1,1,i) = 0.98298d0 - 0.000026d0 - 0.368046d0 * r_di(1,1,i) - &
            (-0.78720d0 + 0.01330d0 * tr_p(1,1,i) + 0.00158d0 * ru(1,1,i) + &
            0.00180d0 * ru(1,1,i-1) + 0.00115d0 * ru(1,1,i-2) + &
            0.00014d0 * ru(1,1,i-3) - 0.00072d0 * ru(1,1,i-4) - &
            0.00094d0 * ru(1,1,i-5))
   
         ! (120) PM1617 
         if (i > lastDataQtr) p(1,1,i) = p_p(1,1,i) + p_add2(1,1,i)

         ! (121) PM1819_P
         p_p(1,2,i) = 0.97979d0 - 0.000070d0 - 0.614469d0 * r_di(1,2,i) - &
            (-0.50476d0 + 0.00764d0 * tr_p(1,2,i) + &
            0.00626d0 * sum(ru(1,2,i-4:i)) / 5.0d0)
   
         ! (122) PM1819 
         if (i > lastDataQtr) p(1,2,i) = p_p(1,2,i) + p_add2(1,2,i)

      end do

   end subroutine SolveLaborForcePartRateMales1619
   
!===============================================================================
   
   subroutine SolveLaborForcePartRateMales2054()

      integer :: i
   
      ! Equations 123-178
      do i = startQtr, endQtr
      
         ! (123) PM2024NM_P 
         pms_p(1,3,1,i) = 1.04005d0 - 0.00523d0 - 0.000143d0 - 0.00225d0 * tr_p(1,3,i) - &
            0.00063d0 * ru(1,3,i) - 0.00077d0 * ru(1,3,i-1) - &
            0.00059d0 * ru(1,3,i-2) - 0.00027d0 * ru(1,3,i-3) + &
            0.00005d0 * ru(1,3,i-4) + 0.00020d0 * ru(1,3,i-5) - &
            0.788895d0 * r_di(1,3,i)

         ! (124) PM2024MS_P 
         pms_p(1,3,2,i) = 1.03184d0 - 0.00733d0 + 0.000543d0 - 0.00069d0 * tr_p(1,3,i) - &
            0.00063d0 * ru(1,3,i) - 0.00077d0 * ru(1,3,i-1) - &
            0.00059d0 * ru(1,3,i-2) - 0.00027d0 * ru(1,3,i-3) + &
            0.00005d0 * ru(1,3,i-4) + 0.00020d0 * ru(1,3,i-5) - &
            0.937270d0 * r_di(1,3,i)
            
         ! (125) PM2024MA_P 
         pms_p(1,3,3,i) = 1.14087d0 - 0.01412d0 + 0.000270d0 - 0.00232d0 * tr_p(1,3,i) - &
            0.00063d0 * ru(1,3,i) - 0.00077d0 * ru(1,3,i-1) - &
            0.00059d0 * ru(1,3,i-2) - 0.00027d0 * ru(1,3,i-3) + &
            0.00005d0 * ru(1,3,i-4) + 0.00020d0 * ru(1,3,i-5) - &
            0.878382d0 * r_di(1,3,i)
            
         ! (126) PM2024_P 
         p_p(1,3,i) = (pms_p(1,3,1,i) * nms(1,3,1,i) + &
            pms_p(1,3,2,i) * nms(1,3,2,i) + pms_p(1,3,3,i) * nms(1,3,3,i)) / n(1,3,i)

         ! (127) PM2529NM_P 
         pms_p(1,4,1,i) = 0.97919d0 - 0.00809d0 + 0.000424d0 - 0.00070d0 * tr_p(1,4,i) - &
            0.00028d0 * ru(1,4,i) - 0.00044d0 * ru(1,4,i-1) - &
            0.00050d0 * ru(1,4,i-2) - 0.00047d0 * ru(1,4,i-3) - &
            0.00037d0 * ru(1,4,i-4) - 0.00021d0 * ru(1,4,i-5) - &
            0.887239d0 * r_di(1,4,i)

         ! (128) PM2529MS_P 
         pms_p(1,4,2,i) = 1.00110d0 - 0.00498d0 + 0.001239d0 - 0.00025d0 * tr_p(1,4,i) - &
            0.00028d0 * ru(1,4,i) - 0.00044d0 * ru(1,4,i-1) - &
            0.00050d0 * ru(1,4,i-2) - 0.00047d0 * ru(1,4,i-3) - &
            0.00037d0 * ru(1,4,i-4) - 0.00021d0 * ru(1,4,i-5) - &
            0.956095d0 * r_di(1,4,i)
            
         ! (129) PM2529MA_P 
         pms_p(1,4,3,i) = 0.98602d0 - 0.00788d0 + 0.000733d0 - 0.00051d0 * tr_p(1,4,i) - &
            0.00028d0 * ru(1,4,i) - 0.00044d0 * ru(1,4,i-1) - &
            0.00050d0 * ru(1,4,i-2) - 0.00047d0 * ru(1,4,i-3) - &
            0.00037d0 * ru(1,4,i-4) - 0.00021d0 * ru(1,4,i-5) - &
            0.913332d0 * r_di(1,4,i)
            
         ! (130) PM2529_P 
         p_p(1,4,i) = (pms_p(1,4,1,i) * nms(1,4,1,i) + &
            pms_p(1,4,2,i) * nms(1,4,2,i) + pms_p(1,4,3,i) * nms(1,4,3,i)) / n(1,4,i)
            
         ! (131) PM3034NM_P 
         pms_p(1,5,1,i) = 0.90427d0 + 0.000567d0 - 0.00046d0 * ru(1,5,i) - &
            0.00061d0 * ru(1,5,i-1) - 0.00054d0 * ru(1,5,i-2) - &
            0.00036d0 * ru(1,5,i-3) - 0.00014d0 * ru(1,5,i-4) + &
            0.00001d0 * ru(1,5,i-5) - 0.883076d0 * r_di(1,5,i)
            
         ! (132) PM3034MS_P 
         pms_p(1,5,2,i) = 0.97120d0 + 0.001964d0 + 0.16457d0 * 1/(tr_p(1,5,i) - 85) - &
            0.00046d0 * ru(1,5,i) - 0.00061d0 * ru(1,5,i-1) - &
            0.00054d0 * ru(1,5,i-2) - 0.00036d0 * ru(1,5,i-3) - &
            0.00014d0 * ru(1,5,i-4) + 0.00001d0 * ru(1,5,i-5) - &
            0.961197 * r_di(1,5,i)
            
         ! (133) PM3034MA_P 
         pms_p(1,5,3,i) = 0.93933d0 + 0.001197d0 - 0.00046d0 * ru(1,5,i) - &
            0.00061d0 * ru(1,5,i-1) - 0.00054d0 * ru(1,5,i-2) - &
            0.00036d0 * ru(1,5,i-3) - 0.00014d0 * ru(1,5,i-4) + &
            0.00001d0 * ru(1,5,i-5) - 0.918267d0 * r_di(1,5,i)
            
         ! (134) PM3034_P 
         p_p(1,5,i) = (pms_p(1,5,1,i) * nms(1,5,1,i) + &
            pms_p(1,5,2,i) * nms(1,5,2,i) + pms_p(1,5,3,i) * nms(1,5,3,i)) / n(1,5,i)

         ! (135) PM3539NM_P 
         pms_p(1,6,1,i) = 0.86825d0 - 0.000105d0 - 0.00004d0 * ru(1,6,i) - &
            0.00010d0 * ru(1,6,i-1) - 0.00016d0 * ru(1,6,i-2) - &
            0.00021d0 * ru(1,6,i-3) - 0.00021d0 * ru(1,6,i-4) - &
            0.00015d0 * ru(1,6,i-5) - 0.846107d0 * r_di(1,6,i)
            
         ! (136) PM3539MS_P 
         pms_p(1,6,2,i) = 0.98068d0 + 0.002566d0 - 0.00004d0 * ru(1,6,i) - &
            0.00010d0 * ru(1,6,i-1) - 0.00016d0 * ru(1,6,i-2) - &
            0.00021d0 * ru(1,6,i-3) - 0.00021d0 * ru(1,6,i-4) - &
            0.00015d0 * ru(1,6,i-5) - 0.958567d0 * r_di(1,6,i)
            
         ! (137) PM3539MA_P 
         pms_p(1,6,3,i) = 0.92354d0 + 0.001209d0 - 0.00004d0 * ru(1,6,i) - &
            0.00010d0 * ru(1,6,i-1) - 0.00016d0 * ru(1,6,i-2) - &
            0.00021d0 * ru(1,6,i-3) - 0.00021d0 * ru(1,6,i-4) - &
            0.00015d0 * ru(1,6,i-5) - 0.901443d0 * r_di(1,6,i)

         ! (138) PM3539_P 
         p_p(1,6,i) = (pms_p(1,6,1,i) * nms(1,6,1,i) + &
            pms_p(1,6,2,i) * nms(1,6,2,i) + pms_p(1,6,3,i) * nms(1,6,3,i)) / n(1,6,i)

         ! (139) PM4044NM_P 
         pms_p(1,7,1,i) = 0.83977d0 - 0.001481d0 - 0.00057d0 * ru(1,7,i) - &
            0.00066d0 * ru(1,7,i-1) - 0.00044d0 * ru(1,7,i-2) - &
            0.00009d0 * ru(1,7,i-3) + 0.00022d0 * ru(1,7,i-4) + &
            0.00031d0 * ru(1,7,i-5) - 0.807518d0 * r_di(1,7,i)
            
         ! (140) PM4044MS_P 
         pms_p(1,7,2,i) = 0.98250d0 + 0.003434d0 - 0.00057d0 * ru(1,7,i) - &
            0.00066d0 * ru(1,7,i-1) - 0.00044d0 * ru(1,7,i-2) - &
            0.00009d0 * ru(1,7,i-3) + 0.00022d0 * ru(1,7,i-4) + &
            0.00031d0 * ru(1,7,i-5) - 0.950227d0 * r_di(1,7,i)
            
         ! (141) PM4044MA_P 
         pms_p(1,7,3,i) = 0.91512d0 + 0.001111d0 - 0.00057d0 * ru(1,7,i) - &
            0.00066d0 * ru(1,7,i-1) - 0.00044d0 * ru(1,7,i-2) - &
            0.00009d0 * ru(1,7,i-3) + 0.00022d0 * ru(1,7,i-4) + &
            0.00031d0 * ru(1,7,i-5) - 0.882777d0 * r_di(1,7,i)
            
         ! (142) PM4044_P 
         p_p(1,7,i) = (pms_p(1,7,1,i) * nms(1,7,1,i) + &
            pms_p(1,7,2,i) * nms(1,7,2,i) + pms_p(1,7,3,i) * nms(1,7,3,i)) / n(1,7,i)

         ! (143) PM4549NM_P 
         pms_p(1,8,1,i) = 0.81116d0 - 0.004005d0 - 0.00002d0 * ru(1,8,i) - &
            0.00016d0 * ru(1,8,i-1) - 0.00034d0 * ru(1,8,i-2) - &
            0.00049d0 * ru(1,8,i-3) - 0.00054d0 * ru(1,8,i-4) - &
            0.00040d0 * ru(1,8,i-5) - 0.765856d0 * r_di(1,8,i)
            
         ! (144) PM4549MS_P 
         pms_p(1,8,2,i) = 0.98115d0 + 0.004829d0 - 0.00002d0 * ru(1,8,i) - &
         0.00016d0 * ru(1,8,i-1) - 0.00034d0 * ru(1,8,i-2) - &
         0.00049d0 * ru(1,8,i-3) - 0.00054d0 * ru(1,8,i-4) - &
         0.00040d0 * ru(1,8,i-5) - 0.935691d0 * r_di(1,8,i)
         
         ! (145) PM4549MA_P 
         pms_p(1,8,3,i) = 0.89473d0 + 0.000334d0 - 0.00002d0 * ru(1,8,i) - &
            0.00016d0 * ru(1,8,i-1) - 0.00034d0 * ru(1,8,i-2) - &
            0.00049d0 * ru(1,8,i-3) - 0.00054d0 * ru(1,8,i-4) - &
            0.00040d0 * ru(1,8,i-5) - 0.849283d0 * r_di(1,8,i)
            
         ! (146) PM4549_P 
         p_p(1,8,i) = (pms_p(1,8,1,i) * nms(1,8,1,i) + &
            pms_p(1,8,2,i) * nms(1,8,2,i) + pms_p(1,8,3,i) * nms(1,8,3,i)) / n(1,8,i)

         ! (147) PM5054NM_P 
         pms_p(1,9,1,i) = 0.77540d0 - 0.010316d0 + 0.00112d0 * ru(1,9,i) + &
            0.00103d0 * ru(1,9,i-1) + 0.00023d0 * ru(1,9,i-2) - &
            0.00078d0 * ru(1,9,i-3) - 0.00149d0 * ru(1,9,i-4) - &
            0.00139d0 * ru(1,9,i-5) - 0.715161d0 * r_di(1,9,i)

         ! (148) PM5054MS_P 
         pms_p(1,9,2,i) = 0.94484d0 + 0.004732d0 + 0.00112d0 * ru(1,9,i) + &
            0.00103d0 * ru(1,9,i-1) + 0.00023d0 * ru(1,9,i-2) - &
            0.00078d0 * ru(1,9,i-3) - 0.00149d0 * ru(1,9,i-4) - &
            0.00139d0 * ru(1,9,i-5) + 0.09796d0 * (rf5054cu6(i) + & 
            rf5054c6o(i)) - 0.901430d0 * r_di(1,9,i)
            
         ! (149) PM5054MA_P 
         pms_p(1,9,3,i) = 0.84912d0 - 0.004345d0 + 0.00112d0 * ru(1,9,i) + &
            0.00103d0 * ru(1,9,i-1) + 0.00023d0 * ru(1,9,i-2) - &
            0.00078d0 * ru(1,9,i-3) - 0.00149d0 * ru(1,9,i-4) - &
            0.00139d0 * ru(1,9,i-5) - 0.789075d0 * r_di(1,9,i)
            
         ! (150) PM5054_P 
         p_p(1,9,i) = (pms_p(1,9,1,i) * nms(1,9,1,i) + &
            pms_p(1,9,2,i) * nms(1,9,2,i) + pms_p(1,9,3,i) * nms(1,9,3,i)) / n(1,9,i)

         ! Some of Equations 149-176 are needed to rescale the
         ! participation rates for the period starQtr to lastDataQtr
         ! because historical data is not available by marital status
         
         ! (151) PM2024 
         if (i > lastDataQtr) p(1,3,i) = p_p(1,3,i) + p_add2(1,3,i)
         
         ! (152) PM2024NM 
         pms(1,3,1,i) = pms_p(1,3,1,i) * p(1,3,i)  / p_p(1,3,i) 
         
         ! (153) PM2024MS 
         pms(1,3,2,i) = pms_p(1,3,2,i) * p(1,3,i)  / p_p(1,3,i) 
         
         ! (154) PM2024MA 
         pms(1,3,3,i) = pms_p(1,3,3,i) * p(1,3,i)  / p_p(1,3,i) 
         
         ! (155) PM2529 
         if (i > lastDataQtr) p(1,4,i) = p_p(1,4,i) + p_add2(1,4,i)
         
         ! (156) PM2529NM 
         pms(1,4,1,i) = pms_p(1,4,1,i) * p(1,4,i) / p_p(1,4,i)
         
         ! (157) PM2529MS 
         pms(1,4,2,i) = pms_p(1,4,2,i) * p(1,4,i) / p_p(1,4,i)
         
         ! (158) PM2529MA 
         pms(1,4,3,i) = pms_p(1,4,3,i) * p(1,4,i) / p_p(1,4,i)
         
         ! (159) PM3034 
         if (i > lastDataQtr) p(1,5,i) = p_p(1,5,i) + p_add2(1,5,i)
         
         ! (160) PM3034NM 
         pms(1,5,1,i) = pms_p(1,5,1,i) * p(1,5,i) / p_p(1,5,i)
         
         ! (161) PM3034MS 
         pms(1,5,2,i) = pms_p(1,5,2,i) * p(1,5,i) / p_p(1,5,i)
         
         ! (162) PM3034MA 
         pms(1,5,3,i) = pms_p(1,5,3,i) * p(1,5,i) / p_p(1,5,i)
         
         ! (163) PM3539 
         if (i > lastDataQtr) p(1,6,i) = p_p(1,6,i) + p_add2(1,6,i)
         
         ! (164) PM3539NM 
         pms(1,6,1,i) = pms_p(1,6,1,i) * p(1,6,i) / p_p(1,6,i)
         
         ! (165) PM3539MS 
         pms(1,6,2,i) = pms_p(1,6,2,i) * p(1,6,i) / p_p(1,6,i)
         
         ! (166) PM3539MA 
         pms(1,6,3,i) = pms_p(1,6,3,i) * p(1,6,i) / p_p(1,6,i)
         
         ! (167) PM4044 
         if (i > lastDataQtr) p(1,7,i) = p_p(1,7,i) + p_add2(1,7,i)
         
         ! (168) PM4044NM 
         pms(1,7,1,i) = pms_p(1,7,1,i) * p(1,7,i) / p_p(1,7,i)
         
         ! (169) PM4044MS 
         pms(1,7,2,i) = pms_p(1,7,2,i) * p(1,7,i) / p_p(1,7,i)
         
         ! (170) PM4044MA 
         pms(1,7,3,i) = pms_p(1,7,3,i) * p(1,7,i) / p_p(1,7,i)
         
         ! (171) PM4549 
         if (i > lastDataQtr) p(1,8,i) = p_p(1,8,i) + p_add2(1,8,i)
         
         ! (172) PM4549NM 
         pms(1,8,1,i) = pms_p(1,8,1,i) * p(1,8,i) / p_p(1,8,i)
         
         ! (173) PM4549MS 
         pms(1,8,2,i) = pms_p(1,8,2,i) * p(1,8,i) / p_p(1,8,i)
         
         ! (174) PM4549MA 
         pms(1,8,3,i) = pms_p(1,8,3,i) * p(1,8,i) / p_p(1,8,i)
         
         ! (175) PM5054 
         if (i > lastDataQtr) p(1,9,i) = p_p(1,9,i) + p_add2(1,9,i)
         
         ! (176) PM5054NM 
         pms(1,9,1,i) = pms_p(1,9,1,i) * p(1,9,i) / p_p(1,9,i)
         
         ! (177) PM5054MS 
         pms(1,9,2,i) = pms_p(1,9,2,i) * p(1,9,i) / p_p(1,9,i)
         
         ! (178) PM5054MA 
         pms(1,9,3,i) = pms_p(1,9,3,i) * p(1,9,i) / p_p(1,9,i)
         
      end do
   
   end subroutine SolveLaborForcePartRateMales2054
   
!===============================================================================
   
   subroutine SolveLaborForcePartRate5574()
   
      integer :: i

     ! Equations 179-248

      do i = startQtr, endQtr

         ! (179) PM55_P
         psy_p(1,55,i) = -0.76902d0 + 0.000142d0 - 0.818694d0 * rsy_di(1,55,i) &
            + psye_de(1,55,i) + psy_dm(1,55,i) + 0.00062d0 * ru(1,10,i) + &
            0.00041d0 * ru(1,10,i-1) - 0.00026d0 * ru(1,10,i-2) - &
            0.00101d0 * ru(1,10,i-3) - 0.00147d0 * ru(1,10,i-4) - &
            0.00126d0 * ru(1,10,i-5)

         ! (180) PM55 
         if (i > lastDataQtr) psy(1,55,i) = psy_p(1,55,i) + psy_add(1,55,i) + psy_add2(1,55,i)

         ! (181) PM56_P
         psy_p(1,56,i) = -0.76098d0 - 0.002143d0 - 0.798138d0 * rsy_di(1,56,i) &
            + psye_de(1,56,i) + psy_dm(1,56,i) + 0.00062d0 * ru(1,10,i) + &
            0.00041d0 * ru(1,10,i-1) - 0.00026d0 * ru(1,10,i-2) - &
            0.00101d0 * ru(1,10,i-3) - 0.00147d0 * ru(1,10,i-4) - &
            0.00126d0 * ru(1,10,i-5)
            
         ! (182) PM56 
         if (i > lastDataQtr) psy(1,56,i) = psy_p(1,56,i) + psy_add(1,56,i) + psy_add2(1,56,i)

         ! (183) PM57_P 
         psy_p(1,57,i) = -0.71065d0 - 0.004979d0 - 0.77622d0 * rsy_di(1,57,i) &
            + psye_de(1,57,i) + psy_dm(1,57,i) + 0.00062d0 * ru(1,10,i) + &
            0.00041d0 * ru(1,10,i-1) - 0.00026d0 * ru(1,10,i-2) - &
            0.00101d0 * ru(1,10,i-3) - 0.00147d0 * ru(1,10,i-4) - &
            0.00126d0 * ru(1,10,i-5)
            
         ! (184) PM57 
         if (i > lastDataQtr) psy(1,57,i) = psy_p(1,57,i) + psy_add(1,57,i) + psy_add2(1,57,i)

         ! (185) PM58_P
         psy_p(1,58,i) = -0.69412d0 - 0.00797d0 - 0.755977d0 * rsy_di(1,58,i) &
            + psye_de(1,58,i) + psy_dm(1,58,i) + 0.00062d0 * ru(1,10,i) + &
            0.00041d0 * ru(1,10,i-1) - 0.00026d0 * ru(1,10,i-2) - &
            0.00101d0 * ru(1,10,i-3) - 0.00147d0 * ru(1,10,i-4) - &
            0.00126d0 * ru(1,10,i-5)
            
         ! (186) PM58 
         if (i > lastDataQtr) psy(1,58,i) = psy_p(1,58,i) + psy_add(1,58,i) + psy_add2(1,58,i)

         ! (187) PM59_P
         psy_p(1,59,i) = -0.60153d0 - 0.012123d0 - 0.73072d0 * rsy_di(1,59,i) &
            + psye_de(1,59,i) + psy_dm(1,59,i) + 0.00062d0 * ru(1,10,i) + &
            0.00041d0 * ru(1,10,i-1) - 0.00026d0 * ru(1,10,i-2) - &
            0.00101d0 * ru(1,10,i-3) - 0.00147d0 * ru(1,10,i-4) - &
            0.00126d0 * ru(1,10,i-5)
            
         ! (188) PM59 
         if (i > lastDataQtr) psy(1,59,i) = psy_p(1,59,i) + psy_add(1,59,i) + psy_add2(1,59,i)

         ! (189) PF58_P 
         psy_p(2,58,i) = -0.604503d0 * rsy_di(2,58,i) - 0.023096d0 + &
            0.00064d0 * ru(2,10,i) + 0.00041d0 * ru(2,10,i-1) - &
            0.00029d0 * ru(2,10,i-2) - 0.00107d0 * ru(2,10,i-3) - &
            0.00155d0 * ru(2,10,i-4) - 0.00132d0 * ru(2,10,i-5) + &
            psye_de(2,58,i) + psy_dm(2,58,i) + 0.00362d0 * psy_coh48(58,i) - &
            0.83081d0

         ! (190) PF58 
         if (i > lastDataQtr) psy(2,58,i) =  psy_p(2,58,i) + psy_add(2,58,i) + psy_add2(2,58,i)

         ! (191) PM60_P
         psy_p(1,60,i) = -0.58858d0 -0.011072d0 + 0.000362d0 - 0.682175d0 * rsy_di(1,60,i) &
            + psye_de(1,60,i) + psy_dm(1,60,i) + 0.00203d0 * ru(1,11,i) + &
            0.0016d0 * ru(1,11,i-1) - 0.00021d0 * ru(1,11,i-2) - &
            0.00235d0 * ru(1,11,i-3) - 0.00374d0 * ru(1,11,i-4) - &
            0.00331d0 * ru(1,11,i-5) + 0.01566d0 * psy(2,58,i)

         ! (192) PM60 
         if (i > lastDataQtr) psy(1,60,i) = psy_p(1,60,i) + psy_add(1,60,i) + psy_add2(1,60,i)

         ! (193) PF59_P 
         psy_p(2,59,i) = -0.571801d0 * rsy_di(2,59,i) - 0.029463d0 + &
            0.00064d0 * ru(2,10,i) + 0.00041d0 * ru(2,10,i-1) - &
            0.00029d0 * ru(2,10,i-2) - 0.00107d0 * ru(2,10,i-3) - &
            0.00155d0 * ru(2,10,i-4) - 0.00132d0 * ru(2,10,i-5) + &
            psye_de(2,59,i) + psy_dm(2,59,i) + 0.0047d0 * psy_coh48(59,i) - &
            0.85665d0
            
         ! (194) PF59 
         if (i > lastDataQtr) psy(2,59,i) = psy_p(2,59,i) + psy_add(2,59,i) + psy_add2(2,59,i)

         ! (195) PM61_P
         psy_p(1,61,i) = -0.55646d0 - 0.019672d0 + 0.002517d0 - 0.647667d0 * rsy_di(1,61,i) &
            + psye_de(1,61,i) + psy_dm(1,61,i) + 0.00203d0 * ru(1,11,i) + &
            0.0016d0 * ru(1,11,i-1) - 0.00021d0 * ru(1,11,i-2) - &
            0.00235d0 * ru(1,11,i-3) - 0.00374d0 * ru(1,11,i-4) - &
            0.00331d0 * ru(1,11,i-5) + 0.08544d0 * psy(2,59,i)

         ! (196) PM61 
         if (i > lastDataQtr) psy(1,61,i) = psy_p(1,61,i) + psy_add(1,61,i) + psy_add2(1,61,i)

         ! (197) PF60_P 
         psy_p(2,60,i) = -0.526008d0 * rsy_di(2,60,i) - 0.033474d0 + &
            0.00141d0 * ru(2,11,i) + 0.00166d0 * ru(2,11,i-1) + &
            0.00116d0 * ru(2,11,i-2) + 0.00033d0 * ru(2,11,i-3) - &
            0.00041d0 * ru(2,11,i-4) - 0.00066d0 * ru(2,11,i-5) + &
            psye_de(2,60,i) + psy_dm(2,60,i) + 0.00819d0 * psy_coh48(60,i) - &
            1.18744d0
            
         ! (198) PF60 
         if (i > lastDataQtr) psy(2,60,i) = psy_p(2,60,i) + psy_add(2,60,i) + psy_add2(2,60,i)

         ! (199) PM62_P
         psy_p(1,62,i) = 0.26329d0 * psy(2,60,i) - 0.29161d0 - &
            0.544582d0 * rsy_di(1,61,i-4) + psye_de(1,62,i) + &
            psy_dm(1,62,i) - 0.056836d0 + 0.008814d0 + 0.00203d0 * ru(1,11,i) + &
            0.00160d0 * ru(1,11,i-1) - 0.00021d0 * ru(1,11,i-2) - &
            0.00235d0 * ru(1,11,i-3) - 0.00374d0 * ru(1,11,i-4) - &
            0.00331d0 * ru(1,11,i-5) - 0.60d0 * rradj_m62(i) - &
            0.02d0 * pot_et_txrt(62,i)
         
         ! (200) PM62 
         if (i > lastDataQtr) psy(1,62,i) = psy_p(1,62,i) + psy_add(1,62,i) + psy_add2(1,62,i)

         ! (201) PF61_P 
         psy_p(2,61,i) = -0.48775d0 * rsy_di(2,61,i) - 0.041036d0 + &
            0.00141d0 * ru(2,11,i) + 0.00166d0 * ru(2,11,i-1) + &
            0.00116d0 * ru(2,11,i-2) + 0.00033d0 * ru(2,11,i-3) - &
            0.00041d0 * ru(2,11,i-4) - 0.00066d0 * ru(2,11,i-5) + &
            psye_de(2,61,i) + psy_dm(2,61,i) + 0.0052d0 * psy_coh48(61,i) - &
            0.8785d0

         ! (202) PF61 
         if (i > lastDataQtr) psy(2,61,i) =  psy_p(2,61,i) + psy_add(2,61,i) + psy_add2(2,61,i)

         ! (203) PM63_P
         psy_p(1,63,i) = 0.40940d0 * psy(2,61,i) - 0.29495d0 - &
            0.480592d0 * rsy_di(1,61,i-8) + psye_de(1,63,i) + &
            psy_dm(1,63,i) - 0.085295d0 + 0.017121d0 + 0.00203d0 * ru(1,11,i) + &
            0.00160d0 * ru(1,11,i-1) - 0.00021d0 * ru(1,11,i-2) - &
            0.00235d0 * ru(1,11,i-3) - 0.00374d0 * ru(1,11,i-4) - &
            0.00331d0 * ru(1,11,i-5) - 0.55d0 * rradj_m63(i) - &
            0.02d0 * pot_et_txrt(63,i)

         ! (204) PM63 
         if (i > lastDataQtr) psy(1,63,i) = psy_p(1,63,i) + psy_add(1,63,i) + psy_add2(1,63,i)

         ! (205) PF62_P 
         psy_p(2,62,i) = -0.412411d0 * rsy_di(2,61,i-4) - 0.063463d0 + &
            0.00141d0 * ru(2,11,i) + 0.00166d0 * ru(2,11,i-1) + &
            0.00116d0 * ru(2,11,i-2) + 0.00033d0 * ru(2,11,i-3) - &
            0.00041d0 * ru(2,11,i-4) - 0.00066d0 * ru(2,11,i-5) + &
            psye_de(2,62,i) + psy_dm(2,62,i) + 0.00523d0 * psy_coh48(62,i) - &
            0.56287d0 - 0.51d0 * rradj_f62(i) - 0.02d0 * pot_et_txrt(62,i)
            
         ! (206) PF62 
         if (i > lastDataQtr) psy(2,62,i) = psy_p(2,62,i) + psy_add(2,62,i) + psy_add2(2,62,i)

         ! (207) PM64_P
         psy_p(1,64,i) = 0.47933d0 * psy(2,62,i) - 0.22665d0 - &
            0.435189d0 * rsy_di(1,61,i-12) + psye_de(1,64,i) + &
            psy_dm(1,64,i) - 0.113466d0 + 0.030849d0 + 0.00203d0 * ru(1,11,i) + &
            0.00160d0 * ru(1,11,i-1) - 0.00021d0 * ru(1,11,i-2) - &
            0.00235d0 * ru(1,11,i-3) - 0.00374d0 * ru(1,11,i-4) - &
            0.00331d0 * ru(1,11,i-5) - 0.50d0 * rradj_m64(i) - &
            0.02d0 * pot_et_txrt(64,i)

         ! (208) PM64 
         if (i > lastDataQtr) psy(1,64,i) = psy_p(1,64,i) + psy_add(1,64,i) + psy_add2(1,64,i)

         ! (209) PF63_P 
         psy_p(2,63,i) = -0.359185d0 * rsy_di(2,61,i-8) - 0.082104d0 + &
            0.00141d0 * ru(2,11,i) + 0.00166d0 * ru(2,11,i-1) + &
            0.00116d0 * ru(2,11,i-2) + 0.00033d0 * ru(2,11,i-3) - &
            0.00041d0 * ru(2,11,i-4) - 0.00066d0 * ru(2,11,i-5) + &
            psye_de(2,63,i) + psy_dm(2,63,i) + 0.00659d0 * psy_coh48(63,i) - &
            0.68289d0 - 0.4675d0 * rradj_f63(i) - 0.02d0 * pot_et_txrt(63,i)

         ! (210) PF63 
         if (i > lastDataQtr) psy(2,63,i) = psy_p(2,63,i) + psy_add(2,63,i) + psy_add2(2,63,i)

         ! (211) PM65_P
         psy_p(1,65,i) = 0.72722d0 * psy(2,63,i) - 0.35819d0 - &
            0.368105d0 * rsy_di(1,61,i-16) + psye_de(1,65,i) + &
            psy_dm(1,65,i) - 0.016932d0 + 0.059764d0 + 0.00067d0 * ru(1,12,i) + &
            0.00040d0 * ru(1,12,i-1) - 0.00040d0 * ru(1,12,i-2) - &
            0.00127d0 * ru(1,12,i-3) - 0.00178d0 * ru(1,12,i-4) - &
            0.00151d0 * ru(1,12,i-5) - 0.45d0 * rradj_m65(i) - &
            0.02d0 * pot_et_txrt(65,i)
            
         ! (212) PM65 
         if (i > lastDataQtr) psy(1,65,i) = psy_p(1,65,i) + psy_add(1,65,i) + psy_add2(1,65,i)

         ! (213) PF64_P 
         psy_p(2,64,i) = -0.323866d0 * rsy_di(2,61,i-12) - 0.094477d0 + &
            0.00141d0 * ru(2,11,i) + 0.00166d0 * ru(2,11,i-1) + &
            0.00116d0 * ru(2,11,i-2) + 0.00033d0 * ru(2,11,i-3) - &
            0.00041d0 * ru(2,11,i-4) - 0.00066d0 * ru(2,11,i-5) + &
            psye_de(2,64,i) + psy_dm(2,64,i) + 0.00745d0 * psy_coh48(64,i) - &
            0.72813d0 - 0.425d0 * rradj_f64(i) - 0.02d0 * pot_et_txrt(64,i)
            
         ! (214) PF64 
         if (i > lastDataQtr) psy(2,64,i) = psy_p(2,64,i) + psy_add(2,64,i) + psy_add2(2,64,i)

         ! (215) PM66_P
         psy_p(1,66,i) = 0.38684d0 * psy(2,64,i) - 0.20883d0 - &
            0.330352d0 * rsy_di(1,61,i-20) + psye_de(1,66,i) + &
            psy_dm(1,66,i) + 0.000194d0 + 0.036539d0 + 0.00067d0 * ru(1,12,i) + &
            0.00040d0 * ru(1,12,i-1) - 0.00040d0 * ru(1,12,i-2) - &
            0.00127d0 * ru(1,12,i-3) - 0.00178d0 * ru(1,12,i-4) - &
            0.00151d0 * ru(1,12,i-5) - 0.40d0 * rradj_m66(i) - &
            0.02d0 * pot_et_txrt(66,i)
            
         ! (216) PM66 
         if (i > lastDataQtr) psy(1,66,i) = psy_p(1,66,i) + psy_add(1,66,i) + psy_add2(1,66,i)

         ! (217) PF65_P 
         psy_p(2,65,i) = -0.256938d0 * rsy_di(2,61,i-16) + 0.035143d0 + &
            0.00029d0 * ru(2,12,i) + 0.00014d0 * ru(2,12,i-1) - &
            0.00023d0 * ru(2,12,i-2) - 0.00063d0 * ru(2,12,i-3) - &
            0.00086d0 * ru(2,12,i-4) - 0.00072d0 * ru(2,12,i-5) + &
            psye_de(2,65,i) + psy_dm(2,65,i) + 0.00348d0 * psy_coh48(65,i) - &
            0.3749d0 - 0.3825d0 * rradj_f65(i) - &
            0.02d0 * pot_et_txrt(65,i)
            
         ! (218) PF65 
         if (i > lastDataQtr) psy(2,65,i) = psy_p(2,65,i) + psy_add(2,65,i) + psy_add2(2,65,i)

         ! (219) PM67_P
         psy_p(1,67,i) = 0.35012d0 * psy(2,65,i) - 0.15975d0 - &
            0.294277d0 * rsy_di(1,61,i-24) + psye_de(1,67,i) + &
            psy_dm(1,67,i) + 0.042362d0 - 0.012475d0 + 0.00067d0 * ru(1,12,i) + &
            0.00040d0 * ru(1,12,i-1) - 0.00040d0 * ru(1,12,i-2) - &
            0.00127d0 * ru(1,12,i-3) - 0.00178d0 * ru(1,12,i-4) - &
            0.00151d0 * ru(1,12,i-5) - 0.35d0 * rradj_m67(i) - &
            0.02d0 * pot_et_txrt(67,i)
            
         ! (220) PM67 
         if (i > lastDataQtr) psy(1,67,i) = psy_p(1,67,i) + psy_add(1,67,i) + psy_add2(1,67,i)

         ! (221) PF66_P 
         psy_p(2,66,i) = -0.226911d0 * rsy_di(2,61,i-20) + 0.028402d0 + &
            0.00029d0 * ru(2,12,i) + 0.00014d0 * ru(2,12,i-1) - &
            0.00023d0 * ru(2,12,i-2) - 0.00063d0 * ru(2,12,i-3) - &
            0.00086d0 * ru(2,12,i-4) - 0.00072d0 * ru(2,12,i-5) + &
            psye_de(2,66,i) + psy_dm(2,66,i) + 0.00512d0 * psy_coh48(66,i) - &
            0.52851d0 - 0.34d0 * rradj_f66(i) - 0.02d0 * pot_et_txrt(66,i)

         ! (222) PF66 
         if (i > lastDataQtr) psy(2,66,i) = psy_p(2,66,i) + psy_add(2,66,i) + psy_add2(2,66,i)

         ! (223) PM68_P
         psy_p(1,68,i) = 0.95984d0 * psy(2,66,i) - 0.26305d0 - &
            0.269452d0 * rsy_di(1,61,i-28) + psye_de(1,68,i) + &
            psy_dm(1,68,i) + 0.057835d0 - 0.027492d0 + 0.00067d0 * ru(1,12,i) + &
            0.00040d0 * ru(1,12,i-1) - 0.00040d0 * ru(1,12,i-2) - &
            0.00127d0 * ru(1,12,i-3) - 0.00178d0 * ru(1,12,i-4) - &
            0.00151d0 * ru(1,12,i-5) - 0.30d0 * rradj_m68(i) - &
            0.02d0 * pot_et_txrt(68,i)

         ! (224) PM68 
         if (i > lastDataQtr) psy(1,68,i) = psy_p(1,68,i) + psy_add(1,68,i) + psy_add2(1,68,i)

         ! (225) PF67_P 
         psy_p(2,67,i) = 0.00029d0 * ru(2,12,i) + 0.00014d0 * ru(2,12,i-1) - &
            0.00023d0 * ru(2,12,i-2) - 0.00063d0 * ru(2,12,i-3) - &
            0.00086d0 * ru(2,12,i-4) - 0.00072d0 * ru(2,12,i-5) - &
            0.206835d0 * rsy_di(2,61,i-24) + .025203d0 + &
            psye_de(2,67,i) + psy_dm(2,67,i) + 0.00518d0 * psy_coh48(67,i) - &
            0.53735d0 - 0.2975d0 * rradj_f67(i) - &
            0.02d0 * pot_et_txrt(67,i)

         ! (226) PF67 
         if (i > lastDataQtr) psy(2,67,i) = psy_p(2,67,i) + psy_add(2,67,i) + psy_add2(2,67,i)

         ! (227) PM69_P
         psy_p(1,69,i) = 0.74113d0 * psy(2,67,i) - 0.22589d0 - &
            0.246975d0 * rsy_di(1,61,i-32) + psye_de(1,69,i) + &
            psy_dm(1,69,i) + 0.047260d0 - 0.018867d0 + 0.00067d0 * ru(1,12,i) + &
            0.00040d0 * ru(1,12,i-1) - 0.00040d0 * ru(1,12,i-2) - &
            0.00127d0 * ru(1,12,i-3) - 0.00178d0 * ru(1,12,i-4) - &
            0.00151d0 * ru(1,12,i-5) - 0.30d0 * rradj_m69(i) - &
            0.02d0 * pot_et_txrt(69,i)

         ! (228) PM69 
         if (i > lastDataQtr) psy(1,69,i) = psy_p(1,69,i) + psy_add(1,69,i) + psy_add2(1,69,i)

         ! (229) PF68_P 
         psy_p(2,68,i) = 0.00029d0 * ru(2,12,i) + 0.00014d0 * ru(2,12,i-1) - &
            0.00023d0 * ru(2,12,i-2) - 0.00063d0 * ru(2,12,i-3) - &
            0.00086d0 * ru(2,12,i-4) - 0.00072d0 * ru(2,12,i-5) - &
            0.180608d0 * rsy_di(2,61,i-28) + .020367d0 + &
            psye_de(2,68,i) + psy_dm(2,68,i) + 0.00400d0 * psy_coh48(68,i) - &
            0.41046d0 - 0.2550d0 * rradj_f68(i) - 0.02d0 * pot_et_txrt(68,i)

         ! (230) PF68 
         if (i > lastDataQtr) psy(2,68,i) = psy_p(2,68,i) + psy_add(2,68,i) + psy_add2(2,68,i)

         ! (231) PM70_P
         psy_p(1,70,i) = 0.46445d0 * psy(2,68,i) - 0.23451d0 - &
            0.220464 * rsy_di(1,61,i-36) + psye_de(1,70,i) + &
            psy_dm(1,70,i) + 0.037323d0 - 0.010528d0 - 0.00013d0 * ru(1,13,i) - &
            0.00016d0 * ru(1,13,i-1) - 0.00013d0 * ru(1,13,i-2) - &
            0.00006d0 * ru(1,13,i-3) + 0.00000d0 * ru(1,13,i-4) + &
            0.00003d0 * ru(1,13,i-5)

         ! (232) PM70 
         if (i > lastDataQtr) psy(1,70,i) = psy_p(1,70,i) + psy_add(1,70,i) + psy_add2(1,70,i)

         ! (233) PF69_P 
         psy_p(2,69,i) = 0.00029d0 * ru(2,12,i) + 0.00014d0 * ru(2,12,i-1) - &
            0.00023d0 * ru(2,12,i-2) - 0.00063d0 * ru(2,12,i-3) - &
            0.00086d0 * ru(2,12,i-4) - 0.00072d0 * ru(2,12,i-5) - &
            0.162386d0 * rsy_di(2,61,i-32) + 0.017869d0 +&
            psye_de(2,69,i) + psy_dm(2,69,i) + 0.00400d0 * psy_coh48(69,i) - &
            0.36706d0 - 0.2550d0 * rradj_f69(i) - 0.02d0 * pot_et_txrt(69,i)

         ! (234) PF69 
         if (i > lastDataQtr) psy(2,69,i) = psy_p(2,69,i) + psy_add(2,69,i) + psy_add2(2,69,i)

         ! (235) PM71_P
         psy_p(1,71,i) = 0.27684d0 * psy(2,69,i) - 0.20679d0 + psye_de(1,71,i) - &
            0.202537 * rsy_di(1,61,i-40) + &
            psy_dm(1,71,i) + 0.030867d0 - 0.005653d0 - 0.00013d0 * ru(1,13,i) - &
            0.00016d0 * ru(1,13,i-1) - 0.00013d0 * ru(1,13,i-2) - &
            0.00006d0 * ru(1,13,i-3) + 0.00000d0 * ru(1,13,i-4) + &
            0.00003d0 * ru(1,13,i-5)

         ! (236) PM71 
         if (i > lastDataQtr) psy(1,71,i) = psy_p(1,71,i) + psy_add(1,71,i) + psy_add2(1,71,i)

         ! (237) PF70_P 
         psy_p(2,70,i) = -0.00009d0 * ru(2,13,i) - 0.00028d0 * ru(2,13,i-1) - &
            0.00048d0 * ru(2,13,i-2) - 0.00063d0 * ru(2,13,i-3) - &
            0.00064d0 * ru(2,13,i-4) - 0.00046d0 * ru(2,13,i-5) -&
            0.136965 * rsy_di(2,61,i-36) + 0.015491d0 + &
            psye_de(2,70,i) + psy_dm(2,70,i) + 0.00424d0 * psy_coh48(70,i) - &
            0.49282d0

         ! (238) PF70 
         if (i > lastDataQtr) psy(2,70,i) = psy_p(2,70,i) + psy_add(2,70,i) + psy_add2(2,70,i)

         ! (239) PM72_P
         psy_p(1,72,i) = 0.77240d0 * psy(2,70,i) - 0.25289d0 - &
            0.186071d0 * rsy_di(1,61,i-44) + &
            psye_de(1,72,i) + psy_dm(1,72,i) + 0.040290d0 - &
            0.012330d0 - 0.00013d0 * ru(1,13,i) - &
            0.00016d0 * ru(1,13,i-1) - 0.00013d0 * ru(1,13,i-2) - &
            0.00006d0 * ru(1,13,i-3) + 0.00000d0 * ru(1,13,i-4) + &
            0.00003d0 * ru(1,13,i-5)

         ! (240) PM72 
         if (i > lastDataQtr) psy(1,72,i) = psy_p(1,72,i) + psy_add(1,72,i) + psy_add2(1,72,i)

         ! (241) PF71_P 
         psy_p(2,71,i) = -0.00009d0 * ru(2,13,i) - 0.00028d0 * ru(2,13,i-1) - &
            0.00048d0 * ru(2,13,i-2) - 0.00063d0 * ru(2,13,i-3) - &
            0.00064d0 * ru(2,13,i-4) - 0.00046d0 * ru(2,13,i-5) - &
            0.123245d0 * rsy_di(2,61,i-40) + 0.014381d0 + &
            psye_de(2,71,i) + psy_dm(2,71,i) + 0.003d0 * psy_coh48(71,i) - &
            0.3739d0

         ! (242) PF71 
         if (i > lastDataQtr) psy(2,71,i) = psy_p(2,71,i) + psy_add(2,71,i) + psy_add2(2,71,i)

         ! (243) PM73_P
         psy_p(1,73,i) = 0.65971d0 * psy(2,71,i) - 0.19394d0 - &
            0.167545d0 * rsy_di(1,61,i-48) + psye_de(1,73,i) + &
            psy_dm(1,73,i) + 0.036021d0 - 0.009631d0 - 0.00013d0 * ru(1,13,i) - &
            0.00016d0 * ru(1,13,i-1) - 0.00013d0 * ru(1,13,i-2) - &
            0.00006d0 * ru(1,13,i-3) + 0.00000d0 * ru(1,13,i-4) + &
            0.00003d0 * ru(1,13,i-5)

         ! (244) PM73 
         if (i > lastDataQtr) psy(1,73,i) = psy_p(1,73,i) + psy_add(1,73,i) + psy_add2(1,73,i)

         ! (245) PF72_P 
         psy_p(2,72,i) = -0.00009d0 * ru(2,13,i) - 0.00028d0 * ru(2,13,i-1) - &
            0.00048d0 * ru(2,13,i-2) - 0.00063d0 * ru(2,13,i-3) - &
            0.00064d0 * ru(2,13,i-4) - 0.00046d0 * ru(2,13,i-5) - &
            0.107978d0 * rsy_di(2,61,i-44) + 0.013644d0 + &
            psye_de(2,72,i) + psy_dm(2,72,i) + 0.00286d0 * psy_coh48(72,i) - &
            0.3467d0

         ! (246) PF72 
         if (i > lastDataQtr) psy(2,72,i) = psy_p(2,72,i) + psy_add(2,72,i) + psy_add2(2,72,i)

         ! (247) PM74_P
         psy_p(1,74,i) = 0.78464d0 * psy(2,72,i) - 0.17649d0  - &
            0.147134d0 * rsy_di(1,61,i-52) + &
            psye_de(1,74,i) + psy_dm(1,74,i) + 0.033451 - 0.010839d0 - &
            0.00013d0 * ru(1,13,i) - &
            0.00016d0 * ru(1,13,i-1) - 0.00013d0 * ru(1,13,i-2) - &
            0.00006d0 * ru(1,13,i-3) + 0.00000d0 * ru(1,13,i-4) + &
            0.00003d0 * ru(1,13,i-5)

         ! (248) PM74 
         if (i > lastDataQtr) psy(1,74,i) = psy_p(1,74,i) + psy_add(1,74,i) + psy_add2(1,74,i)
            
      end do
   
   end subroutine SolveLaborForcePartRate5574
   
!===============================================================================
   
   subroutine SolveLaborForcePartRateMales75100()
   
      integer :: i
      
      ! Equations 249-303
 
      do i = startQtr, endQtr

         ! (249) DPM75O_FE 
         dp_fe(1,17,i) = (-0.00043d0 * ru_fe(1,14,i) - &
            0.00051d0 * ru_fe(1,14,i-1) - 0.00036d0 * ru_fe(1,14,i-2) - &
            0.00010d0 * ru_fe(1,14,i-3) + 0.00013d0 * ru_fe(1,14,i-4) + &
            0.00021d0 * ru_fe(1,14,i-5)) - (-0.00043d0 * ru(1,14,i) - &
            0.00051d0 * ru(1,14,i-1) - 0.00036d0 * ru(1,14,i-2) - &
            0.00010d0 * ru(1,14,i-3) + 0.00013d0 * ru(1,14,i-4) + &
            0.00021d0 * ru(1,14,i-5))

         ! (250) PM75_P
         psy_p(1,75,i) = psy(1,74,i-4) * 0.920d0 + dp_fe(1,17,i)

         ! (251) PM75 
         if (i > lastDataQtr) psy(1,75,i) = psy_p(1,75,i)

         ! (252) PM76_P
         psy_p(1,76,i) = psy(1,75,i-4) * 0.920d0 + dp_fe(1,17,i)
         
         ! (253) PM76 
         if (i > lastDataQtr) psy(1,76,i) =  psy_p(1,76,i)

         ! (254) PM77_P
         psy_p(1,77,i) = psy(1,76,i-4) * 0.920d0 + dp_fe(1,17,i)

         ! (255) PM77 
         if (i > lastDataQtr) psy(1,77,i) =  psy_p(1,77,i)

         ! (256) PM78_P
         psy_p(1,78,i) = psy(1,77,i-4) * 0.920d0 + dp_fe(1,17,i)

         ! (257) PM78 
         if (i > lastDataQtr) psy(1,78,i) = psy_p(1,78,i)

         ! (258) PM79_P
         psy_p(1,79,i) = psy(1,78,i-4) * 0.920d0 + dp_fe(1,17,i)

         ! (259) PM79 
         if (i > lastDataQtr) psy(1,79,i) = psy_p(1,79,i)

         ! (260) PM80_P 
         psy_p(1,80,i) = psy(1,79,i-4) * 0.965d0**(1) + dp_fe(1,17,i)
         
         ! (261) PM81_P 
         psy_p(1,81,i) = psy(1,79,i-8) * 0.965d0**(2) + dp_fe(1,17,i)
         
         ! (262) PM82_P 
         psy_p(1,82,i) = psy(1,79,i-12) * 0.965d0**(3) + dp_fe(1,17,i)
         
         ! (263) PM83_P 
         psy_p(1,83,i) = psy(1,79,i-16) * 0.965d0**(4) + dp_fe(1,17,i)
         
         ! (264) PM84_P 
         psy_p(1,84,i) = psy(1,79,i-20) * 0.965d0**(5) + dp_fe(1,17,i)

         ! (265) PM85_P 
         psy_p(1,85,i) = (sum(psy(1,79,i-31:i-24)) / 8.0d0) * 0.965d0**(6) + &
           dp_fe(1,17,i)
           
         ! (266) PM86_P 
         psy_p(1,86,i) = (sum(psy(1,79,i-35:i-28)) / 8.0d0) * 0.965d0**(7) + &
            dp_fe(1,17,i)
            
         ! (267) PM87_P 
         psy_p(1,87,i) = (sum(psy(1,79,i-39:i-32)) / 8.0d0) * 0.965d0**(8) + &
            dp_fe(1,17,i)

         ! (268) PM88_P 
         psy_p(1,88,i) = (sum(psy(1,79,i-43:i-36)) / 8.0d0) * 0.965d0**(9) + &
            dp_fe(1,17,i)
            
         ! (269) PM89_P 
         psy_p(1,89,i) = (sum(psy(1,79,i-47:i-40)) / 8.0d0) * 0.965d0**(10) + &
            dp_fe(1,17,i)
            
         ! (270) PM90_P 
         psy_p(1,90,i) = (sum(psy(1,79,i-51:i-44)) / 8.0d0) * 0.965d0**(11) + &
            dp_fe(1,17,i)
            
         ! (271) PM91_P 
         psy_p(1,91,i) = (sum(psy(1,79,i-55:i-48)) / 8.0d0) * 0.965d0**(12) + &
            dp_fe(1,17,i)
            
         ! (272) PM92_P 
         psy_p(1,92,i) = (sum(psy(1,79,i-59:i-52)) / 8.0d0) * 0.965d0**(13) + &
            dp_fe(1,17,i)
            
         ! (273) PM93_P 
         psy_p(1,93,i) = (sum(psy(1,79,i-63:i-56)) / 8.0d0) * 0.965d0**(14) + &
            dp_fe(1,17,i)
            
         ! (274) PM94_P 
         psy_p(1,94,i) = (sum(psy(1,79,i-67:i-60)) / 8.0d0) * 0.965d0**(15) + &
            dp_fe(1,17,i)
     
         ! (275) PM95_P 
         psy_p(1,95,i) = psy_p(1,94,i) * 0.965d0 + dp_fe(1,17,i)
         
         ! (276) PM96_P 
         psy_p(1,96,i) = psy_p(1,95,i) * 0.965d0 + dp_fe(1,17,i)
         
         ! (277) PM97_P 
         psy_p(1,97,i) = psy_p(1,96,i) * 0.965d0 + dp_fe(1,17,i)
         
         ! (278) PM98_P 
         psy_p(1,98,i) = psy_p(1,97,i) * 0.965d0 + dp_fe(1,17,i)
         
         ! (279) PM99_P 
         psy_p(1,99,i) = psy_p(1,98,i) * 0.965d0 + dp_fe(1,17,i)
         
         ! (280) PM100_P 
         psy_p(1,100,i) = psy_p(1,99,i) * 0.965d0 + dp_fe(1,17,i)

         ! (281) PM80O_P 
         p80o_p(1,i) = (psy_p(1,80,i) * nisy(1,80,i) + psy_p(1,81,i) * nisy(1,81,i) + &
            psy_p(1,82,i) * nisy(1,82,i) + psy_p(1,83,i) * nisy(1,83,i) + &
            psy_p(1,84,i) * nisy(1,84,i) + psy_p(1,85,i) * nisy(1,85,i) + &
            psy_p(1,86,i) * nisy(1,86,i) + psy_p(1,87,i) * nisy(1,87,i) + &
            psy_p(1,88,i) * nisy(1,88,i) + psy_p(1,89,i) * nisy(1,89,i) + &
            psy_p(1,90,i) * nisy(1,90,i) + psy_p(1,91,i) * nisy(1,91,i) + &
            psy_p(1,92,i) * nisy(1,92,i) + psy_p(1,93,i) * nisy(1,93,i) + &
            psy_p(1,94,i) * nisy(1,94,i) + psy_p(1,95,i) * nisy(1,95,i) + &
            psy_p(1,96,i) * nisy(1,96,i) + psy_p(1,97,i) * nisy(1,97,i) + &
            psy_p(1,98,i) * nisy(1,98,i) + psy_p(1,99,i) * nisy(1,99,i) + &
            psy_p(1,100,i) * nisy(1,100,i)) / sum(nisy(1,80:100,i))

         ! (282) PM80O 
         if (i > lastDataQtr) p80o(1,i) = p80o_p(1,i)

         ! (283) PM80 
         psy(1,80,i) = psy_p(1,80,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (284) PM81 
         psy(1,81,i) = psy_p(1,81,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (285) PM82 
         psy(1,82,i) = psy_p(1,82,i) * p80o(1,i) / p80o_p(1,i)
          
         ! (286) PM83 
         psy(1,83,i) = psy_p(1,83,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (287) PM84 
         psy(1,84,i) = psy_p(1,84,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (288) PM85 
         psy(1,85,i) = psy_p(1,85,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (289) PM86 
         psy(1,86,i) = psy_p(1,86,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (290) PM87 
         psy(1,87,i) = psy_p(1,87,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (291) PM88 
         psy(1,88,i) = psy_p(1,88,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (292) PM89 
         psy(1,89,i) = psy_p(1,89,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (293) PM90 
         psy(1,90,i) = psy_p(1,90,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (294) PM91 
         psy(1,91,i) = psy_p(1,91,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (295) PM92 
         psy(1,92,i) = psy_p(1,92,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (296) PM93 
         psy(1,93,i) = psy_p(1,93,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (297) PM94 
         psy(1,94,i) = psy_p(1,94,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (298) PM95 
         psy(1,95,i) = psy_p(1,95,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (299) PM96 
         psy(1,96,i) = psy_p(1,96,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (300) PM97 
         psy(1,97,i) = psy_p(1,97,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (301) PM98 
         psy(1,98,i) = psy_p(1,98,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (302) PM99 
         psy(1,99,i) = psy_p(1,99,i) * p80o(1,i) / p80o_p(1,i)
         
         ! (303) PM100 
         psy(1,100,i) = psy_p(1,100,i) * p80o(1,i) / p80o_p(1,i)

      end do
  
   end subroutine SolveLaborForcePartRateMales75100
   
!===============================================================================
   
   subroutine SolveLaborForcePartRateFemales1619()

      integer :: i
   
      ! Equations 304-307

      do i = startQtr, endQtr

         ! (304) PF1617_P
         p_p(2,1,i) = 0.98681d0 - 0.000047d0 - (0.377204d0 * r_di(2,1,i)) - &
            (-0.00741d0 + 0.23393d0 * rf1617cu6(i) + &
            0.00051d0 * sum(ru(2,1,i-4:i)) / 5.0d0) - (-0.69608d0 + &
            0.01166d0 * tr_p(2,1,i) + 0.00616d0 * sum(ru(2,1,i-4:i)) / 5.0d0)

         ! (305) PF1617 
         if (i > lastDataQtr) p(2,1,i) = p_p(2,1,i) + p_add2(2,1,i)

         ! (306) PF1819_P 
         p_p(2,2,i) = 0.98200d0 - 0.000069d0 - (0.584930d0 * r_di(2,2,i)) - &
            (-0.00080d0 + 0.22814d0 * rf1819cu6(i) + &
            0.00318d0 * sum(ru(2,2,i-4:i)) / 5.0d0) - (-0.53433d0 + &
            0.00764d0 * tr_p(2,2,i) + 0.00667d0 * sum(ru(2,2,i-4:i)) / 5.0d0)

         ! (307) PF1819 
         if (i > lastDataQtr) p(2,2,i) = p_p(2,2,i) + p_add2(2,2,i)
            
      end do
   
   end subroutine SolveLaborForcePartRateFemales1619
   
!===============================================================================
   
   
   subroutine SolveLaborForcePartRateFemles2054()
   
      integer :: i

      ! Equations 308-423

      do i = startQtr, endQtr

         ! (308) PF2024NMC6U_P 
         pmsc6u_p(2,3,1,i) = (0.70868d0 -0.000477d0 - 0.00087d0 * ru(2,3,i) - &
            0.00099d0 * ru(2,3,i-1) - 0.00063d0 * ru(2,3,i-2) - &
            0.00007d0 * ru(2,3,i-3) + 0.00041d0 * ru(2,3,i-4) + &
            0.00052d0 * ru(2,3,i-5) - &
            0.661872d0 * r_di(2,3,i)) * 1.0160d0
            
         ! (309) PF2024NMNC6_P 
         pmsnc6_p(2,3,1,i) = (1.13654d0 -0.000206d0 - 0.00366d0 * tr_pf2024nmnc6(i) - &
            0.00087d0 * ru(2,3,i) - 0.00099d0 * ru(2,3,i-1) - &
            0.00063d0 * ru(2,3,i-2) - 0.00007d0 * ru(2,3,i-3) + &
            0.00041d0 * ru(2,3,i-4) + 0.00052d0 * ru(2,3,i-5) - &
            0.742995d0 * r_di(2,3,i)) * 1.0160d0
               
         ! (310) PF2024MSC6U_P 
         pmsc6u_p(2,3,2,i) = (0.69043d0 - 0.000935d0 + 0.000015d0 - 0.00087d0 * ru(2,3,i) - &
            0.00099d0 * ru(2,3,i-1) - 0.00063d0 * ru(2,3,i-2) - &
            0.00007d0 * ru(2,3,i-3) + 0.00041d0 * ru(2,3,i-4) + &
            0.00052d0 * ru(2,3,i-5) - 0.524765d0 * &
            r_di(2,3,i) - 0.1d0 * if2024msc6u(i)) * 1.0160d0
               
         ! (311) PF2024MSNC6_P 
         pmsnc6_p(2,3,2,i) = (0.79421d0 - 0.000075d0 - 0.00087d0 * ru(2,3,i) - &
            0.00099d0 * ru(2,3,i-1) - 0.00063d0 * ru(2,3,i-2) - &
            0.00007d0 * ru(2,3,i-3) + 0.00041d0 * ru(2,3,i-4) + &
            0.00052d0 * ru(2,3,i-5) - 0.781956d0 * &
            r_di(2,3,i)) * 1.0160d0
            
         ! (312) PF2024MAC6U_P 
         pmsc6u_p(2,3,3,i) = (0.95787d0 - 0.000403d0 + 0.000007d0 - 0.00087d0 * ru(2,3,i) - &
         0.00099d0 * ru(2,3,i-1) - 0.00063d0 * ru(2,3,i-2) - &
         0.00007d0 * ru(2,3,i-3) + 0.00041d0 * ru(2,3,i-4) + &
         0.00052d0 * ru(2,3,i-5) - 0.683999d0 * &
         r_di(2,3,i) - 0.16722d0 * if2024mac6u(i)) * 1.0160d0
         
         ! (313) PF2024MANC6_P 
         pmsnc6_p(2,3,3,i) = (0.75174d0 - 0.000258d0 + 0.000005d0 - 0.00087d0 * ru(2,3,i) - &
            0.00099d0 * ru(2,3,i-1) - 0.00063d0 * ru(2,3,i-2) - &
            0.00007d0 * ru(2,3,i-3) + 0.00041d0 * ru(2,3,i-4) + &
            0.00052d0 * ru(2,3,i-5) - 0.727427d0 * &
            r_di(2,3,i)) * 1.0160d0

         ! (314) PF2529NMC6U_P 
         pmsc6u_p(2,4,1,i) = (0.74861d0 - 0.000966d0 - 0.00056d0 * ru(2,4,i) - &
            0.00070d0 * ru(2,4,i-1) - 0.00057d0 * ru(2,4,i-2) - &
            0.00029d0 * ru(2,4,i-3) - 0.00002d0 * ru(2,4,i-4) + &
            0.00013d0 * ru(2,4,i-5) - 0.700572d0 * &
            r_di(2,4,i)) * 0.9981d0
            
         ! (315) PF2529NMNC6_P 
         pmsnc6_p(2,4,1,i) = (0.98148d0 + 0.000457d0 - 0.00111d0 * tr_pf2529nmnc6(i) - &
            0.00056d0 * ru(2,4,i) - 0.00070d0 * ru(2,4,i-1) - &
            0.00057d0 * ru(2,4,i-2) - 0.00029d0 * ru(2,4,i-3) - &
            0.00002d0 * ru(2,4,i-4) + 0.00013d0 * ru(2,4,i-5) - &
            0.853721d0 * r_di(2,4,i)) * 0.9981d0
            
         ! (316) PF2529MSC6U_P 
         pmsc6u_p(2,4,2,i) = (0.76218d0 - 0.001821d0 - 0.000004d0 - 0.00056d0 * ru(2,4,i) - &
            0.00070d0 * ru(2,4,i-1) - 0.00057d0 * ru(2,4,i-2) - &
            0.00029d0 * ru(2,4,i-3) - 0.00002d0 * ru(2,4,i-4) + &
            0.00013d0 * ru(2,4,i-5) - 0.608428d0 * &
            r_di(2,4,i) - 0.1d0 * if2529msc6u(i)) * 0.9981d0
            
         ! (317) PF2529MSNC6_P 
         pmsnc6_p(2,4,2,i) = (0.83502d0 + 0.000186d0 - 0.00056d0 * ru(2,4,i) - &
            0.00070d0 * ru(2,4,i-1) - 0.00057d0 * ru(2,4,i-2) - &
            0.00029d0 * ru(2,4,i-3) - 0.00002d0 * ru(2,4,i-4) + &
            0.00013d0 * ru(2,4,i-5) - 0.824520d0 * &
            r_di(2,4,i)) * 0.9981d0
            
         ! (318) PF2529MAC6U_P 
         pmsc6u_p(2,4,3,i) = (0.90653d0 - 0.000676d0 - 0.00056d0 * ru(2,4,i) - &
            0.00070d0 * ru(2,4,i-1) - 0.00057d0 * ru(2,4,i-2) - &
            0.00029d0 * ru(2,4,i-3) - 0.00002d0 * ru(2,4,i-4) + &
            0.00013d0 * ru(2,4,i-5) - 0.731798d0 * &
            r_di(2,4,i) - 0.10000d0 * if2529mac6u(i)) * 0.9981d0
            
         ! (319) PF2529MANC6_P 
         pmsnc6_p(2,4,3,i) = (0.82060d0 - 0.000015d0 - 0.00056d0 * ru(2,4,i) - &
            0.00070d0 * ru(2,4,i-1) - 0.00057d0 * ru(2,4,i-2) - &
            0.00029d0 * ru(2,4,i-3) - 0.00002d0 * ru(2,4,i-4) + &
            0.00013d0 * ru(2,4,i-5) - 0.802962d0 * &
            r_di(2,4,i)) * 0.9981d0
            
         ! (320) PF3034NMC6U_P 
         pmsc6u_p(2,5,1,i) = (0.73944d0 - 0.001998d0 - 0.00081d0 * ru(2,5,i) - &
            0.00065d0 * ru(2,5,i-1) + 0.00005d0 * ru(2,5,i-2) + &
            0.00089d0 * ru(2,5,i-3) + 0.00143d0 * ru(2,5,i-4) + &
            0.00128d0 * ru(2,5,i-5) - 0.690741d0 * &
            r_di(2,5,i)) * 0.9980d0

         ! (321) PF3034NMNC6_P 
         pmsnc6_p(2,5,1,i) = (0.84901d0 + 0.000598d0 - 0.00081d0 * ru(2,5,i) - &
            0.00065d0 * ru(2,5,i-1) + 0.00005d0 * ru(2,5,i-2) + &
            0.00089d0 * ru(2,5,i-3) + 0.00143d0 * ru(2,5,i-4) + &
            0.00128d0 * ru(2,5,i-5) - 0.838634d0 * &
            r_di(2,5,i)) * 0.9980d0
            
         ! (322) PF3034MSC6U_P 
         pmsc6u_p(2,5,2,i) = (0.78186d0 - 0.002891d0 - 0.000006d0 - 0.00081d0 * ru(2,5,i) - &
            0.00065d0 * ru(2,5,i-1) + 0.00005d0 * ru(2,5,i-2) + &
            0.00089d0 * ru(2,5,i-3) + 0.00143d0 * ru(2,5,i-4) + &
            0.00128d0 * ru(2,5,i-5) - 0.639819d0 * &
            r_di(2,5,i) - 0.1d0 * if3034msc6u(i)) * 0.9980d0

         ! (323) PF3034MSNC6_P 
         pmsnc6_p(2,5,2,i) = (0.80379d0 - 0.000056d0 - 0.00081d0 * ru(2,5,i) - &
            0.00065d0 * ru(2,5,i-1) + 0.00005d0 * ru(2,5,i-2) + &
            0.00089d0 * ru(2,5,i-3) + 0.00143d0 * ru(2,5,i-4) + &
            0.00128d0 * ru(2,5,i-5) - 0.801357d0 * &
            r_di(2,5,i)) * 0.9980d0
            
         ! (324) PF3034MAC6U_P 
         pmsc6u_p(2,5,3,i) = (0.88071d0 - 0.001042d0 - 0.00081d0 * ru(2,5,i) - &
            0.00065d0 * ru(2,5,i-1) + 0.00005d0 * ru(2,5,i-2) + &
            0.00089d0 * ru(2,5,i-3) + 0.00143d0 * ru(2,5,i-4) + &
            0.00128d0 * ru(2,5,i-5) - 0.745172d0 * &
            r_di(2,5,i) - 0.10000d0 * if3034mac6u(i)) * 0.9980d0
            
         ! (325) PF3034MANC6_P 
         pmsnc6_p(2,5,3,i) = (0.83806d0 + 0.000442d0 - 0.00081d0 * ru(2,5,i) - &
            0.00065d0 * ru(2,5,i-1) + 0.00005d0 * ru(2,5,i-2) + &
            0.00089d0 * ru(2,5,i-3) + 0.00143d0 * ru(2,5,i-4) + &
            0.00128d0 * ru(2,5,i-5) - 0.829717d0 * &
            r_di(2,5,i)) * 0.9980d0

         ! (326) PF3539NMC6U_P 
         pmsc6u_p(2,6,1,i) = (0.75363d0 - 0.003230d0 - 0.00195d0 * ru(2,6,i) - &
            0.00216d0 * ru(2,6,i-1) - 0.00128d0 * ru(2,6,i-2) + &
            0.00002d0 * ru(2,6,i-3) + 0.00111d0 * ru(2,6,i-4) + &
            0.00132d0 * ru(2,6,i-5) - 0.701148d0 * &
            r_di(2,6,i)) * 0.9989d0

         ! (327) PF3539NMNC6_P 
         pmsnc6_p(2,6,1,i) = (0.84953d0 + 0.000015d0 - 0.00195d0 * ru(2,6,i) - &
            0.00216d0 * ru(2,6,i-1) - 0.00128d0 * ru(2,6,i-2) + &
            0.00002d0 * ru(2,6,i-3) + 0.00111d0 * ru(2,6,i-4) + &
            0.00132d0 * ru(2,6,i-5) - 0.822121d0 * &
            r_di(2,6,i)) * 0.9989d0

         ! (328) PF3539MSC6U_P 
         pmsc6u_p(2,6,2,i) = (0.79072d0 - 0.004987d0 - 0.000005d0 - 0.00195d0 * ru(2,6,i) - &
            0.00216d0 * ru(2,6,i-1) - 0.00128d0 * ru(2,6,i-2) + &
            0.00002d0 * ru(2,6,i-3) + 0.00111d0 * ru(2,6,i-4) + &
            0.00132d0 * ru(2,6,i-5) - 0.635646d0 * &
            r_di(2,6,i) - 0.1d0 * if3539msc6u(i)) * 0.9989d0

         ! (329) PF3539MSNC6_P 
         pmsnc6_p(2,6,2,i) = (0.80906d0 - 0.000984d0 - 0.00195d0 * ru(2,6,i) - &
            0.00216d0 * ru(2,6,i-1) - 0.00128d0 * ru(2,6,i-2) + &
            0.00002d0 * ru(2,6,i-3) + 0.00111d0 * ru(2,6,i-4) + &
            0.00132d0 * ru(2,6,i-5) - 0.784868d0 * &
            r_di(2,6,i)) * 0.9989d0

         ! (330) PF3539MAC6U_P 
         pmsc6u_p(2,6,3,i) = (0.90258d0 - 0.002058d0 - 0.00195d0 * ru(2,6,i) - &
            0.00216d0 * ru(2,6,i-1) - 0.00128d0 * ru(2,6,i-2) + &
            0.00002d0 * ru(2,6,i-3) + 0.00111d0 * ru(2,6,i-4) + &
            0.00132d0 * ru(2,6,i-5) - 0.744846d0 * &
            r_di(2,6,i) - 0.10000d0 * if3539mac6u(i)) * 0.9989d0

         ! (331) PF3539MANC6_P 
         pmsnc6_p(2,6,3,i) = (0.86613d0 + 0.000458d0 - 0.00195d0 * ru(2,6,i) - &
            0.00216d0 * ru(2,6,i-1) - 0.00128d0 * ru(2,6,i-2) + &
            0.00002d0 * ru(2,6,i-3) + 0.00111d0 * ru(2,6,i-4) + &
            0.00132d0 * ru(2,6,i-5) - 0.838637d0 * &
            r_di(2,6,i)) * 0.9989d0

         ! (332) PF4044NMC6U_P 
         pmsc6u_p(2,7,1,i) = (0.73920d0 - 0.005589d0 - 0.00026d0 * ru(2,7,i) - &
            0.00050d0 * ru(2,7,i-1) - 0.00068d0 * ru(2,7,i-2) - &
            0.00076d0 * ru(2,7,i-3) - 0.00070d0 * ru(2,7,i-4) - &
            0.00046d0 * ru(2,7,i-5) - 0.677955d0 * &
            r_di(2,7,i)) * 0.9989d0

         ! (333) PF4044NMNC6_P 
         pmsnc6_p(2,7,1,i) = (0.83790d0 - 0.000878d0 - 0.00026d0 * ru(2,7,i) - &
            0.00050d0 * ru(2,7,i-1) - 0.00068d0 * ru(2,7,i-2) - &
            0.00076d0 * ru(2,7,i-3) - 0.00070d0 * ru(2,7,i-4) - &
            0.00046d0 * ru(2,7,i-5) - 0.799021d0 * &
            r_di(2,7,i)) * 0.9989d0

         ! (334) PF4044MSC6U_P 
         pmsc6u_p(2,7,2,i) = (0.79356d0 - 0.007041d0 - 0.000008d0 - 0.00026d0 * ru(2,7,i) - &
            0.00050d0 * ru(2,7,i-1) - 0.00068d0 * ru(2,7,i-2) - &
            0.00076d0 * ru(2,7,i-3) - 0.00070d0 * ru(2,7,i-4) - &
            0.00046d0 * ru(2,7,i-5) - 0.640644d0 * &
            r_di(2,7,i) - 0.1d0 * if4044msc6u(i)) * 0.9989d0

         ! (335) PF4044MSNC6_P 
         pmsnc6_p(2,7,2,i) = (0.82602d0 - 0.001249d0 - 0.00026d0 * ru(2,7,i) - &
            0.00050d0 * ru(2,7,i-1) - 0.00068d0 * ru(2,7,i-2) - &
            0.00076d0 * ru(2,7,i-3) - 0.00070d0 * ru(2,7,i-4) - &
            0.00046d0 * ru(2,7,i-5) - 0.789470d0 * &
            r_di(2,7,i)) * 0.9989d0

         ! (336) PF4044MAC6U_P 
         pmsc6u_p(2,7,3,i) = (0.89876d0 - 0.003975d0 - 0.000005d0- 0.00026d0 * ru(2,7,i) - &
            0.00050d0 * ru(2,7,i-1) - 0.00068d0 * ru(2,7,i-2) - &
            0.00076d0 * ru(2,7,i-3) - 0.00070d0 * ru(2,7,i-4) - &
            0.00046d0 * ru(2,7,i-5) - 0.719419d0 * &
            r_di(2,7,i) - 0.10000d0 * if4044mac6u(i)) * 0.9989d0

         ! (337) PF4044MANC6_P 
         pmsnc6_p(2,7,3,i) = (0.85937d0 + 0.000112d0 - 0.00026d0 * ru(2,7,i) - &
            0.00050d0 * ru(2,7,i-1) - 0.00068d0 * ru(2,7,i-2) - &
            0.00076d0 * ru(2,7,i-3) - 0.00070d0 * ru(2,7,i-4) - &
            0.00046d0 * ru(2,7,i-5) - 0.824438d0 * &
            r_di(2,7,i)) * 0.9989d0

         ! (338) PF2024NM_P 
         pms_p(2,3,1,i) = (pmsc6u_p(2,3,1,i) * nf2024nmc6u(i) + &
            pmsnc6_p(2,3,1,i) * nf2024nmnc6(i)) / nms(2,3,1,i)

         ! (339) PF2024MS_P 
         pms_p(2,3,2,i) = (pmsc6u_p(2,3,2,i) * nf2024msc6u(i) + &
            pmsnc6_p(2,3,2,i) * nf2024msnc6(i)) / nms(2,3,2,i)

         ! (340) PF2024MA_P 
         pms_p(2,3,3,i) = (pmsc6u_p(2,3,3,i) * nf2024mac6u(i) + &
            pmsnc6_p(2,3,3,i) * nf2024manc6(i)) / nms(2,3,3,i)

         ! (341) PF2024_P 
         p_p(2,3,i)= (pms_p(2,3,1,i) * nms(2,3,1,i) + pms_p(2,3,2,i) * &
            nms(2,3,2,i) + pms_p(2,3,3,i) * nms(2,3,3,i)) / n(2,3,i)

         ! (342) PF2529NM_P 
         pms_p(2,4,1,i) = (pmsc6u_p(2,4,1,i) * nf2529nmc6u(i) + &
            pmsnc6_p(2,4,1,i) * nf2529nmnc6(i)) / nms(2,4,1,i)

         ! (343) PF2529MS_P 
         pms_p(2,4,2,i) = (pmsc6u_p(2,4,2,i) * nf2529msc6u(i) + &
            pmsnc6_p(2,4,2,i) * nf2529msnc6(i)) / nms(2,4,2,i)

         ! (344) PF2529MA_P 
         pms_p(2,4,3,i) = (pmsc6u_p(2,4,3,i) * nf2529mac6u(i) + &
            pmsnc6_p(2,4,3,i) * nf2529manc6(i)) / nms(2,4,3,i)

         ! (345) PF2529_P 
         p_p(2,4,i) = (pms_p(2,4,1,i) * nms(2,4,1,i) + pms_p(2,4,2,i) * &
            nms(2,4,2,i) + pms_p(2,4,3,i) * nms(2,4,3,i)) / n(2,4,i)

         ! (346) PF3034NM_P 
         pms_p(2,5,1,i) = (pmsc6u_p(2,5,1,i) * nf3034nmc6u(i) + &
            pmsnc6_p(2,5,1,i) * nf3034nmnc6(i)) / nms(2,5,1,i)

         ! (347) PF3034MS_P 
         pms_p(2,5,2,i) = (pmsc6u_p(2,5,2,i) * nf3034msc6u(i) + &
            pmsnc6_p(2,5,2,i) * nf3034msnc6(i)) / nms(2,5,2,i)

         ! (348) PF3034MA_P 
         pms_p(2,5,3,i) = (pmsc6u_p(2,5,3,i) * nf3034mac6u(i) + &
            pmsnc6_p(2,5,3,i) * nf3034manc6(i)) / nms(2,5,3,i)

         ! (349) PF3034_P 
         p_p(2,5,i)= (pms_p(2,5,1,i) * nms(2,5,1,i) + pms_p(2,5,2,i) * &
            nms(2,5,2,i) + pms_p(2,5,3,i) * nms(2,5,3,i)) / n(2,5,i)

         ! (350) PF3539NM_P 
         pms_p(2,6,1,i) = (pmsc6u_p(2,6,1,i) * nf3539nmc6u(i) + &
            pmsnc6_p(2,6,1,i) * nf3539nmnc6(i)) / nms(2,6,1,i)

         ! (351) PF3539MS_P 
         pms_p(2,6,2,i) = (pmsc6u_p(2,6,2,i) * nf3539msc6u(i) + &
            pmsnc6_p(2,6,2,i) * nf3539msnc6(i)) / nms(2,6,2,i)

         ! (352) PF3539MA_P 
         pms_p(2,6,3,i) = (pmsc6u_p(2,6,3,i) * nf3539mac6u(i) + &
            pmsnc6_p(2,6,3,i) * nf3539manc6(i)) / nms(2,6,3,i)

         ! (353) PF3539_P 
         p_p(2,6,i)= (pms_p(2,6,1,i) * nms(2,6,1,i) + pms_p(2,6,2,i) * &
            nms(2,6,2,i) + pms_p(2,6,3,i) * nms(2,6,3,i)) / n(2,6,i)

         ! (354) PF4044NM_P 
         pms_p(2,7,1,i) = (pmsc6u_p(2,7,1,i) * nf4044nmc6u(i) + &
            pmsnc6_p(2,7,1,i) * nf4044nmnc6(i)) / nms(2,7,1,i)

         ! (355) PF4044MS_P 
         pms_p(2,7,2,i) = (pmsc6u_p(2,7,2,i) * nf4044msc6u(i) + &
            pmsnc6_p(2,7,2,i) * nf4044msnc6(i)) / nms(2,7,2,i)

         ! (356) PF4044MA_P 
         pms_p(2,7,3,i) = (pmsc6u_p(2,7,3,i) * nf4044mac6u(i) + &
            pmsnc6_p(2,7,3,i) * nf4044manc6(i)) / nms(2,7,3,i)

         ! (357) PF4044_P 
         p_p(2,7,i)= (pms_p(2,7,1,i) * nms(2,7,1,i) + pms_p(2,7,2,i) * &
            nms(2,7,2,i) + pms_p(2,7,3,i) * nms(2,7,3,i)) / n(2,7,i)

         ! (358) PF4549NM_P 
         pms_p(2,8,1,i) = 0.03650d0 - 0.003349d0 + pf4549e_de(i) - 0.00076d0 * ru(2,8,i) - &
            0.00070d0 * ru(2,8,i-1) - 0.00018d0 * ru(2,8,i-2) + 0.00049d0 * &
            ru(2,8,i-3) + 0.00096d0 * ru(2,8,i-4) + 0.00091d0 * ru(2,8,i-5) - &
            0.774819d0 * r_di(2,8,i)

         ! (359) PF4549MS_P 
         pms_p(2,8,2,i) = 0.03842d0 - 0.003545d0 + pf4549e_de(i) - 0.00076d0 * ru(2,8,i) - &
            0.00070d0 * ru(2,8,i-1) - 0.00018d0 * ru(2,8,i-2) + 0.00049d0 * &
            ru(2,8,i-3) + 0.00096d0 * ru(2,8,i-4) + 0.00091d0 * ru(2,8,i-5) - &
            0.771341d0 * r_di(2,8,i) - 0.15d0 * rf4549mscu6(i)

         ! (360) PF4549MA_P 
         pms_p(2,8,3,i) = 0.06830d0 - 0.001541d0 + pf4549e_de(i) - 0.00076d0 * ru(2,8,i) - &
            0.00070d0 * ru(2,8,i-1) - 0.00018d0 * ru(2,8,i-2) + 0.00049d0 * &
            ru(2,8,i-3) + 0.00096d0 * ru(2,8,i-4) + 0.00091d0 * ru(2,8,i-5) - &
            0.806961d0 * r_di(2,8,i) - 0.10d0 * rf4549macu6(i)

         ! (361) PF4549_P 
         p_p(2,8,i)= (pms_p(2,8,1,i) * nms(2,8,1,i) + pms_p(2,8,2,i) * &
            nms(2,8,2,i) + pms_p(2,8,3,i) * nms(2,8,3,i)) / n(2,8,i)
         
         ! (362) PF5054NM_P 
         pms_p(2,9,1,i) = 0.05788d0 - 0.008250d0 + pf5054e_de(i) + 0.00003d0 * ru(2,9,i) - &
            0.00011d0 * ru(2,9,i-1) - 0.00032d0 * ru(2,9,i-2) - 0.00051d0 * &
            ru(2,9,i-3) - 0.00059d0 * ru(2,9,i-4) - 0.00045d0 * ru(2,9,i-5) - &
            0.732696d0 * r_di(2,9,i)

         ! (363) PF5054MS_P 
         pms_p(2,9,2,i) = -0.4018d0 - 0.008735d0 + pf5054e_de(i) + 0.00003d0 * ru(2,9,i) - &
            0.00011d0 * ru(2,9,i-1) - 0.00032d0 * ru(2,9,i-2) - 0.00051d0 * &
            ru(2,9,i-3) - 0.00059d0 * ru(2,9,i-4) - 0.00045d0 * ru(2,9,i-5) + &
            0.00454d0 * tr_pf5054ms(i) - 0.726715d0 * &
            r_di(2,9,i) - 0.12d0 * rf5054mscu6(i)

         ! (364) PF5054MA_P 
         pms_p(2,9,3,i) = 0.08983d0 - 0.006021d0 + pf5054e_de(i) + 0.00003d0 * ru(2,9,i) - &
         0.00011d0 * ru(2,9,i-1) - 0.00032d0 * ru(2,9,i-2) - 0.00051d0 * &
         ru(2,9,i-3) - 0.00059d0 * ru(2,9,i-4) - 0.00045d0 * ru(2,9,i-5) - &
         0.760157d0 * r_di(2,9,i) - 0.2d0 * rf5054macu6(i)

         ! (365) PF5054_P 
         p_p(2,9,i)= (pms_p(2,9,1,i) * nms(2,9,1,i) + pms_p(2,9,2,i) * &
            nms(2,9,2,i) + pms_p(2,9,3,i) * nms(2,9,3,i)) / n(2,9,i)

         ! (366) PF2024 
         if (i > lastDataQtr) p(2,3,i) = p_p(2,3,i) + p_add2(2,3,i)
         
         ! (367) PF2024NMC6U 
         pmsc6u(2,3,1,i) = pmsc6u_p(2,3,1,i) * p(2,3,i) / p_p(2,3,i)

         ! (368) PF2024NMNC6 
         pmsnc6(2,3,1,i) = pmsnc6_p(2,3,1,i) * p(2,3,i) / p_p(2,3,i)

         ! (369) PF2024MSC6U 
         pmsc6u(2,3,2,i) = pmsc6u_p(2,3,2,i) * p(2,3,i) / p_p(2,3,i)

         ! (370) PF2024MSNC6 
         pmsnc6(2,3,2,i) = pmsnc6_p(2,3,2,i) * p(2,3,i) / p_p(2,3,i)

         ! (371) PF2024MAC6U 
         pmsc6u(2,3,3,i) = pmsc6u_p(2,3,3,i) * p(2,3,i) / p_p(2,3,i)

         ! (372) PF2024MANC6 
         pmsnc6(2,3,3,i) = pmsnc6_p(2,3,3,i) * p(2,3,i) / p_p(2,3,i)

         ! (373) PF2529 
         if (i > lastDataQtr) p(2,4,i) = p_p(2,4,i) + p_add2(2,4,i)

         ! (374) PF2529NMC6U 
         pmsc6u(2,4,1,i) = pmsc6u_p(2,4,1,i) * p(2,4,i) / p_p(2,4,i)

         ! (375) PF2529NMNC6 
         pmsnc6(2,4,1,i) = pmsnc6_p(2,4,1,i) * p(2,4,i) / p_p(2,4,i)

         ! (376) PF2529MSC6U 
         pmsc6u(2,4,2,i) = pmsc6u_p(2,4,2,i) * p(2,4,i) / p_p(2,4,i)

         ! (377) PF2529MSNC6 
         pmsnc6(2,4,2,i) = pmsnc6_p(2,4,2,i) * p(2,4,i) / p_p(2,4,i)

         ! (378) PF2529MAC6U 
         pmsc6u(2,4,3,i) = pmsc6u_p(2,4,3,i) * p(2,4,i) / p_p(2,4,i)

         ! (379) PF2529MANC6 
         pmsnc6(2,4,3,i) = pmsnc6_p(2,4,3,i) * p(2,4,i) / p_p(2,4,i)

         ! (380) PF3034 
         if (i > lastDataQtr) p(2,5,i) = p_p(2,5,i) + p_add2(2,5,i)

         ! (381) PF3034NMC6U 
         pmsc6u(2,5,1,i) = pmsc6u_p(2,5,1,i) * p(2,5,i) / p_p(2,5,i)

         ! (382) PF3034NMNC6 
         pmsnc6(2,5,1,i) = pmsnc6_p(2,5,1,i) * p(2,5,i) / p_p(2,5,i)

         ! (383) PF3034MSC6U 
         pmsc6u(2,5,2,i) = pmsc6u_p(2,5,2,i) * p(2,5,i) / p_p(2,5,i)

         ! (384) PF3034MSNC6 
         pmsnc6(2,5,2,i) = pmsnc6_p(2,5,2,i) * p(2,5,i) / p_p(2,5,i)

         ! (385) PF3034MAC6U 
         pmsc6u(2,5,3,i) = pmsc6u_p(2,5,3,i) * p(2,5,i) / p_p(2,5,i)

         ! (386) PF3034MANC6 
         pmsnc6(2,5,3,i) = pmsnc6_p(2,5,3,i) * p(2,5,i) / p_p(2,5,i)

         ! (387) PF3539 
         if (i > lastDataQtr) p(2,6,i) = p_p(2,6,i) + p_add2(2,6,i)

         ! (388) PF3539NMC6U 
         pmsc6u(2,6,1,i) = pmsc6u_p(2,6,1,i) * p(2,6,i) / p_p(2,6,i)

         ! (389) PF3539NMNC6 
         pmsnc6(2,6,1,i) = pmsnc6_p(2,6,1,i) * p(2,6,i) / p_p(2,6,i)

         ! (390) PF3539MSC6U 
         pmsc6u(2,6,2,i) = pmsc6u_p(2,6,2,i) * p(2,6,i) / p_p(2,6,i)

         ! (391) PF3539MSNC6 
         pmsnc6(2,6,2,i) = pmsnc6_p(2,6,2,i) * p(2,6,i) / p_p(2,6,i)

         ! (392) PF3539MAC6U 
         pmsc6u(2,6,3,i) = pmsc6u_p(2,6,3,i) * p(2,6,i) / p_p(2,6,i)

         ! (393) PF3539MANC6 
         pmsnc6(2,6,3,i) = pmsnc6_p(2,6,3,i) * p(2,6,i) / p_p(2,6,i)

         ! (394) PF4044 
         if (i > lastDataQtr) p(2,7,i) = p_p(2,7,i) + p_add2(2,7,i)

         ! (395) PF4044NMC6U 
         pmsc6u(2,7,1,i) = pmsc6u_p(2,7,1,i) * p(2,7,i) / p_p(2,7,i)

         ! (396) PF4044NMNC6 
         pmsnc6(2,7,1,i) = pmsnc6_p(2,7,1,i) * p(2,7,i) / p_p(2,7,i)

         ! (397) PF4044MSC6U 
         pmsc6u(2,7,2,i) = pmsc6u_p(2,7,2,i) * p(2,7,i) / p_p(2,7,i)

         ! (398) PF4044MSNC6 
         pmsnc6(2,7,2,i) = pmsnc6_p(2,7,2,i) * p(2,7,i) / p_p(2,7,i)

         ! (399) PF4044MAC6U 
         pmsc6u(2,7,3,i) = pmsc6u_p(2,7,3,i) * p(2,7,i) / p_p(2,7,i)

         ! (400) PF4044MANC6 
         pmsnc6(2,7,3,i) = pmsnc6_p(2,7,3,i) * p(2,7,i) / p_p(2,7,i)

         ! (401) PF2024NM 
         pms(2,3,1,i) = pms_p(2,3,1,i) * p(2,3,i) / p_p(2,3,i)

         ! (402) PF2024MS 
         pms(2,3,2,i) = pms_p(2,3,2,i) * p(2,3,i) / p_p(2,3,i)

         ! (403) PF2024MA 
         pms(2,3,3,i) = pms_p(2,3,3,i) * p(2,3,i) / p_p(2,3,i)

         ! (404) PF2529NM 
         pms(2,4,1,i) = pms_p(2,4,1,i) * p(2,4,i) / p_p(2,4,i)

         ! (405) PF2529MS
         pms(2,4,2,i) = pms_p(2,4,2,i) * p(2,4,i) / p_p(2,4,i)

         ! (406) PF2529MA 
         pms(2,4,3,i) = pms_p(2,4,3,i) * p(2,4,i) / p_p(2,4,i)

         ! (407) PF3034NM 
         pms(2,5,1,i) = pms_p(2,5,1,i) * p(2,5,i) / p_p(2,5,i)

         ! (408) PF3034MS 
         pms(2,5,2,i) = pms_p(2,5,2,i) * p(2,5,i) / p_p(2,5,i)

         ! (409) PF3034MA 
         pms(2,5,3,i) = pms_p(2,5,3,i) * p(2,5,i) / p_p(2,5,i)

         ! (410) PF3539NM 
         pms(2,6,1,i) = pms_p(2,6,1,i) * p(2,6,i) / p_p(2,6,i)

         ! (411) PF3539MS 
         pms(2,6,2,i) = pms_p(2,6,2,i) * p(2,6,i) / p_p(2,6,i)

         ! (412) PF3539MA 
         pms(2,6,3,i) = pms_p(2,6,3,i) * p(2,6,i) / p_p(2,6,i)

         ! (413) PF4044NM 
         pms(2,7,1,i) = pms_p(2,7,1,i) * p(2,7,i) / p_p(2,7,i)

         ! (414) PF4044MS 
         pms(2,7,2,i) = pms_p(2,7,2,i) * p(2,7,i) / p_p(2,7,i)

         ! (415) PF4044MA 
         pms(2,7,3,i) = pms_p(2,7,3,i) * p(2,7,i) / p_p(2,7,i)

         ! (416) PF4549 
         if (i > lastDataQtr) p(2,8,i) = p_p(2,8,i) + p_add2(2,8,i)

         ! (417) PF4549NM 
         pms(2,8,1,i) = pms_p(2,8,1,i) * p(2,8,i) / p_p(2,8,i)

         ! (418) PF4549MS 
         pms(2,8,2,i) = pms_p(2,8,2,i) * p(2,8,i) / p_p(2,8,i)

         ! (419) PF4549MA 
         pms(2,8,3,i) = pms_p(2,8,3,i) * p(2,8,i) / p_p(2,8,i)

         ! (420) PF5054 
         if (i > lastDataQtr) p(2,9,i) = p_p(2,9,i) + p_add2(2,9,i)

         ! (421) PF5054NM 
         pms(2,9,1,i) = pms_p(2,9,1,i) * p(2,9,i) / p_p(2,9,i)

         ! (422) PF5054MS 
         pms(2,9,2,i) = pms_p(2,9,2,i) * p(2,9,i) / p_p(2,9,i)

         ! (423) PF5054MA 
         pms(2,9,3,i) = pms_p(2,9,3,i) * p(2,9,i) / p_p(2,9,i)

      end do
   
   end subroutine SolveLaborForcePartRateFemles2054
   
!===============================================================================
   
   subroutine SolveLaborForcePartRateFemales5574()
   
      integer :: i

      ! Equations 424-433
      do i = startQtr, endQtr

         ! (424) PF55_P 
         psy_p(2,55,i) =  -0.678427d0 * rsy_di(2,55,i) - 0.011198d0 + &
            0.00064d0 * ru(2,10,i) + 0.00041d0 * ru(2,10,i-1) - &
            0.00029d0 * ru(2,10,i-2) - 0.00107d0 * ru(2,10,i-3) - &
            0.00155d0 * ru(2,10,i-4) - 0.00132d0 * ru(2,10,i-5) + &
            psye_de(2,55,i) + psy_dm(2,55,i) + 0.00368d0 * psy_coh48(55,i) - &
            0.90941d0
      
         ! (425) PF55 
         if (i > lastDataQtr) psy(2,55,i) = psy_p(2,55,i) + psy_add(2,55,i) + psy_add2(2,55,i)

         ! (426) PF56_P 
         psy_p(2,56,i) =  -0.651951d0 * rsy_di(2,56,i) - 0.014942d0 + &
            0.00064d0 * ru(2,10,i) + 0.00041d0 * ru(2,10,i-1) - &
            0.00029d0 * ru(2,10,i-2) - 0.00107d0 * ru(2,10,i-3) - &
            0.00155d0 * ru(2,10,i-4) - 0.00132d0 * ru(2,10,i-5) + &
            psye_de(2,56,i) + psy_dm(2,56,i) + 0.00486d0 * psy_coh48(56,i) - &
            0.96865d0

         ! (427) PF56 
         if (i > lastDataQtr) psy(2,56,i) =  psy_p(2,56,i) + psy_add(2,56,i) + psy_add2(2,56,i)

         ! (428) PF57_P 
         psy_p(2,57,i) =  -0.634496d0 * rsy_di(2,57,i) - 0.018113d0 + &
            0.00064d0 * ru(2,10,i) + 0.00041d0 * ru(2,10,i-1) - &
            0.00029d0 * ru(2,10,i-2) - 0.00107d0 * ru(2,10,i-3) - &
            0.00155d0 * ru(2,10,i-4) - 0.00132d0 * ru(2,10,i-5) + &
            psye_de(2,57,i) + psy_dm(2,57,i) + 0.00344d0 * psy_coh48(57,i) - &
            0.85033d0

         ! (429) PF57 
         if (i > lastDataQtr) psy(2,57,i) = psy_p(2,57,i) + psy_add(2,57,i) + psy_add2(2,57,i)

         ! (430) PF73_P 
         psy_p(2,73,i) =  - 0.00009d0 * ru(2,13,i) - 0.00028d0 * ru(2,13,i-1) - &
            0.00048d0 * ru(2,13,i-2) - 0.00063d0 * ru(2,13,i-3) - &
            0.00064d0 * ru(2,13,i-4) - 0.00046d0 * ru(2,13,i-5) - &
            0.094426d0 * rsy_di(2,61,i-48) + 0.012196d0 + &
            psye_de(2,73,i) + psy_dm(2,73,i) + 0.00370d0 * psy_coh48(73,i) - &
            0.41058d0

         ! (431) PF73 
         if (i > lastDataQtr) psy(2,73,i) =  psy_p(2,73,i) + psy_add(2,73,i) + psy_add2(2,73,i)

         ! (432) PF74_P 
         psy_p(2,74,i) =  - 0.00009d0 * ru(2,13,i) - 0.00028d0 * ru(2,13,i-1) - &
            0.00048d0 * ru(2,13,i-2) - 0.00063d0 * ru(2,13,i-3) - &
            0.00064d0 * ru(2,13,i-4) - 0.00046d0 * ru(2,13,i-5) - &
            0.082404d0 * rsy_di(2,61,i-52) + 0.010762d0 + &
            psye_de(2,74,i) + psy_dm(2,74,i) + 0.00304d0 * psy_coh48(74,i) - &
            0.35113d0

         ! (433) PF74 
         if (i > lastDataQtr) psy(2,74,i) = psy_p(2,74,i) + psy_add(2,74,i) + psy_add2(2,74,i)
      
      end do
   
   end subroutine SolveLaborForcePartRateFemales5574
   
!===============================================================================
   
   subroutine SolveLaborForcePartRate75100()
   
      integer :: i

      ! Equations 434-488

      do i = startQtr, endQtr

         ! (434) DPF75O_FE 
         dp_fe(2,17,i) = (-0.00007d0 * ru_fe(2,14,i) - &
            0.00012d0 * ru_fe(2,14,i-1) - 0.00015d0 * ru_fe(2,14,i-2) - &
            0.00014d0 * ru_fe(2,14,i-3) - 0.00012d0 * ru_fe(2,14,i-4) - &
            0.00007d0 * ru_fe(2,14,i-5)) - (-0.00007d0 * ru(2,14,i) - &
            0.00012d0 * ru(2,14,i-1) - 0.00015d0 * ru(2,14,i-2) - &
            0.00014d0 * ru(2,14,i-3) - 0.00012d0 * ru(2,14,i-4) - &
            0.00007d0 * ru(2,14,i-5))

         ! (435) PF75_P 
         psy_p(2,75,i) = psy(2,74,i-4) * 0.9d0 + dp_fe(2,17,i)
      
         ! (436) PF75 
         if (i > lastDataQtr) psy(2,75,i) = psy_p(2,75,i)

         ! (437) PF76_P 
         psy_p(2,76,i) = psy(2,75,i-4) * 0.9d0 + dp_fe(2,17,i)
         
         ! (438) PF76 
         if (i > lastDataQtr) psy(2,76,i) = psy_p(2,76,i)

         ! (439) PF77_P 
         psy_p(2,77,i) = psy(2,76,i-4) * 0.9d0 + dp_fe(2,17,i)
         
         ! (440) PF77 
         if (i > lastDataQtr) psy(2,77,i) = psy_p(2,77,i)

         ! (441) PF78_P 
         psy_p(2,78,i) = psy(2,77,i-4) * 0.9d0 + dp_fe(2,17,i)
         
         ! (442) PF78 
         if (i > lastDataQtr) psy(2,78,i) = psy_p(2,78,i)

         ! (443) PF79_P 
         psy_p(2,79,i) = psy(2,78,i-4) * 0.9d0 + dp_fe(2,17,i)
         
         ! (444) PF79 
         if (i > lastDataQtr) psy(2,79,i) = psy_p(2,79,i)

         ! (445) PF80_P 
         psy_p(2,80,i) = psy(2,79,i-4) * 0.965d0**(1d0) + dp_fe(2,17,i)
         
         ! (445) PF81_P 
         psy_p(2,81,i) = psy(2,79,i-8) * 0.965d0**(2d0) + dp_fe(2,17,i)
         
         ! (445) PF82_P 
         psy_p(2,82,i) = psy(2,79,i-12) * 0.965d0**(3d0) + dp_fe(2,17,i)
         
         ! (445) PF83_P 
         psy_p(2,83,i) = psy(2,79,i-16) * 0.965d0**(4d0) + dp_fe(2,17,i)
         
         ! (449) PF84_P 
         psy_p(2,84,i) = psy(2,79,i-20) * 0.965d0**(5d0) + dp_fe(2,17,i)
         
         ! (450) PF85_P 
         psy_p(2,85,i) = (sum(psy(2,79,i-31:i-24)) / 8.0d0) * 0.965d0**(6d0) + &
            dp_fe(2,17,i)
         
         ! (451) PF86_P 
         psy_p(2,86,i) = (sum(psy(2,79,i-35:i-28)) / 8.0d0) * 0.965d0**(7d0) + &
            dp_fe(2,17,i)
         
         ! (452) PF87_P 
         psy_p(2,87,i) = (sum(psy(2,79,i-39:i-32)) / 8.0d0) * 0.965d0**(8d0) + &
            dp_fe(2,17,i)
         
         ! (453) PF88_P 
         psy_p(2,88,i) = (sum(psy(2,79,i-43:i-36)) / 8.0d0) * 0.965d0**(9d0) + &
            dp_fe(2,17,i)
         
         ! (454) PF89_P 
         psy_p(2,89,i) = (sum(psy(2,79,i-47:i-40)) / 8.0d0) * 0.965d0**(10d0) + &
            dp_fe(2,17,i)
         
         ! (455) PF90_P 
         psy_p(2,90,i) = (sum(psy(2,79,i-51:i-44)) / 8.0d0) * 0.965d0**(11d0) + &
            dp_fe(2,17,i)
         
         ! (456) PF91_P 
         psy_p(2,91,i) = (sum(psy(2,79,i-55:i-48)) / 8.0d0) * 0.965d0**(12d0) + &
            dp_fe(2,17,i)
         
         ! (457) PF92_P 
         psy_p(2,92,i) = (sum(psy(2,79,i-59:i-52)) / 8.0d0) * 0.965d0**(13d0) + &
            dp_fe(2,17,i)
         
         ! (458) PF93_P 
         psy_p(2,93,i) = (sum(psy(2,79,i-63:i-56)) / 8.0d0) * 0.965d0**(14d0) + &
            dp_fe(2,17,i)
         
         ! (459) PF94_P 
         psy_p(2,94,i) = (sum(psy(2,79,i-67:i-60)) / 8.0d0) * 0.965d0**(15d0) + &
            dp_fe(2,17,i)
         
         ! (460) PF95_P 
         psy_p(2,95,i) = psy_p(2,94,i) * 0.965d0 + dp_fe(2,17,i)
         
         ! (461)PF96_P 
         psy_p(2,96,i) = psy_p(2,95,i) * 0.965d0 + dp_fe(2,17,i)
         
         ! (462) PF97_P 
         psy_p(2,97,i) = psy_p(2,96,i) * 0.965d0 + dp_fe(2,17,i)
            
         ! (463) PF98_P 
         psy_p(2,98,i) = psy_p(2,97,i) * 0.965d0 + dp_fe(2,17,i)
         
         ! (464) PF99_P 
         psy_p(2,99,i) = psy_p(2,98,i) * 0.965d0 + dp_fe(2,17,i)
         
         ! (465) PF100_P 
         psy_p(2,100,i) = psy_p(2,99,i) * 0.965d0 + dp_fe(2,17,i)
      
         ! (466) PF80O_P 
         p80o_p(2,i) = (psy_p(2,80,i) * nisy(2,80,i) + psy_p(2,81,i) * nisy(2,81,i) + &
            psy_p(2,82,i) * nisy(2,82,i) + psy_p(2,83,i) * nisy(2,83,i) + &
            psy_p(2,84,i) * nisy(2,84,i) + psy_p(2,85,i) * nisy(2,85,i) + &
            psy_p(2,86,i) * nisy(2,86,i) + psy_p(2,87,i) * nisy(2,87,i) + &
            psy_p(2,88,i) * nisy(2,88,i) + psy_p(2,89,i) * nisy(2,89,i) + &
            psy_p(2,90,i) * nisy(2,90,i) + psy_p(2,91,i) * nisy(2,91,i) + &
            psy_p(2,92,i) * nisy(2,92,i) + psy_p(2,93,i) * nisy(2,93,i) + &
            psy_p(2,94,i) * nisy(2,94,i) + psy_p(2,95,i) * nisy(2,95,i) + &
            psy_p(2,96,i) * nisy(2,96,i) + psy_p(2,97,i) * nisy(2,97,i) + &
            psy_p(2,98,i) * nisy(2,98,i) + psy_p(2,99,i) * nisy(2,99,i) + &
            psy_p(2,100,i) * nisy(2,100,i)) / sum(nisy(2,80:100,i))
            
         ! (467) PF80O 
         if (i > lastDataQtr) p80o(2,i) = p80o_p(2,i)
         
         ! (468) PF80 
         psy(2,80,i) = psy_p(2,80,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (469) PF81 
         psy(2,81,i) = psy_p(2,81,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (470) PF82 
         psy(2,82,i) = psy_p(2,82,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (471) PF83 
         psy(2,83,i) = psy_p(2,83,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (472) PF84 
         psy(2,84,i) = psy_p(2,84,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (473) PF85 
         psy(2,85,i) = psy_p(2,85,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (474) PF86 
         psy(2,86,i) = psy_p(2,86,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (475) PF87 
         psy(2,87,i) = psy_p(2,87,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (476) PF88 
         psy(2,88,i) = psy_p(2,88,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (477) PF89 
         psy(2,89,i) = psy_p(2,89,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (478) PF90 
         psy(2,90,i) = psy_p(2,90,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (479) PF91 
         psy(2,91,i) = psy_p(2,91,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (480) PF92 
         psy(2,92,i) = psy_p(2,92,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (481) PF93 
         psy(2,93,i) = psy_p(2,93,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (482)PF94 
         psy(2,94,i) = psy_p(2,94,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (483) PF95 
         psy(2,95,i) = psy_p(2,95,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (484) PF96 
         psy(2,96,i) = psy_p(2,96,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (485) PF97 
         psy(2,97,i) = psy_p(2,97,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (486) PF98 
         psy(2,98,i) = psy_p(2,98,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (487) PF99 
         psy(2,99,i) = psy_p(2,99,i) * p80o(2,i) / p80o_p(2,i)
         
         ! (488) PF100 
         psy(2,100,i) = psy_p(2,100,i) * p80o(2,i) / p80o_p(2,i)

      end do
   
   end subroutine SolveLaborForcePartRate75100
   
!===============================================================================
   
   subroutine SolveLaborForcePartRate1674(fullEmployment)
   
      logical, optional :: fullEmployment
      integer :: i
      
      if (present(fullEmployment)) then
      
         ! Equations 489-514
      
         do i = startQtr, endQtr
         
            ! (489) DPM1617_FE 
            dp_fe(1,1,i) = (-0.00158d0 * ru_fe(1,1,i) - &
               0.00180d0 * ru_fe(1,1,i-1) - 0.00115d0 * ru_fe(1,1,i-2) - &
               0.00014d0 * ru_fe(1,1,i-3) + 0.00072d0 * ru_fe(1,1,i-4) + &
               0.00094d0 * ru_fe(1,1,i-5)) - (-0.00158d0 * ru(1,1,i) - &
               0.00180d0 * ru(1,1,i-1) - 0.00115d0 * ru(1,1,i-2) - &
               0.00014d0 * ru(1,1,i-3) + 0.00072d0 * ru(1,1,i-4) + &
               0.00094d0 * ru(1,1,i-5))
            
            ! (490) DPM1819_FE 
            dp_fe(1,2,i) = (-0.00108d0 * ru_fe(1,2,i) - &
               0.00127d0 * ru_fe(1,2,i-1) - 0.00088d0 * ru_fe(1,2,i-2) - &
               0.00023d0 * ru_fe(1,2,i-3) + 0.00034d0 * ru_fe(1,2,i-4) + &
               0.00053d0 * ru_fe(1,2,i-5)) - (-0.00108d0 * ru(1,2,i) - &
               0.00127d0 * ru(1,2,i-1) - 0.00088d0 * ru(1,2,i-2) - &
               0.00023d0 * ru(1,2,i-3) + 0.00034d0 * ru(1,2,i-4) + &
               0.00053d0 * ru(1,2,i-5))
            
            ! (491) DPM2024_FE 
            dp_fe(1,3,i) = (-0.00063d0 * ru_fe(1,3,i) - &
               0.00077d0 * ru_fe(1,3,i-1) - 0.00059d0 * ru_fe(1,3,i-2) - &
               0.00027d0 * ru_fe(1,3,i-3) + 0.00005d0 * ru_fe(1,3,i-4) + &
               0.00020d0 * ru_fe(1,3,i-5)) - (-0.00063d0 * ru(1,3,i) - &
               0.00077d0 * ru(1,3,i-1) - 0.00059d0 * ru(1,3,i-2) - &
               0.00027d0 * ru(1,3,i-3) + 0.00005d0 * ru(1,3,i-4) + &
               0.00020d0 * ru(1,3,i-5))
            
            ! (492) DPM2529_FE 
            dp_fe(1,4,i) = (-0.00028d0 * ru_fe(1,4,i) - &
               0.00044d0 * ru_fe(1,4,i-1) - 0.00050d0 * ru_fe(1,4,i-2) - &
               0.00047d0 * ru_fe(1,4,i-3) - 0.00037d0 * ru_fe(1,4,i-4) - &
               0.00021d0 * ru_fe(1,4,i-5)) - (-0.00028d0 * ru(1,4,i) - &
               0.00044d0 * ru(1,4,i-1) - 0.00050d0 * ru(1,4,i-2) - &
               0.00047d0 * ru(1,4,i-3) - 0.00037d0 * ru(1,4,i-4) - &
               0.00021d0 * ru(1,4,i-5))
            
            ! (493) DPM3034_FE 
            dp_fe(1,5,i) = (-0.00046d0 * ru_fe(1,5,i) - &
               0.00061d0 * ru_fe(1,5,i-1) - 0.00054d0 * ru_fe(1,5,i-2) - &
               0.00036d0 * ru_fe(1,5,i-3) - 0.00014d0 * ru_fe(1,5,i-4) + &
               0.00001d0 * ru_fe(1,5,i-5)) - (-0.00046d0 * ru(1,5,i) - &
               0.00061d0 * ru(1,5,i-1) - 0.00054d0 * ru(1,5,i-2) - &
               0.00036d0 * ru(1,5,i-3) - 0.00014d0 * ru(1,5,i-4) + &
               0.00001d0 * ru(1,5,i-5))
            
            ! (494) DPM3539_FE 
            dp_fe(1,6,i) = (-0.00004d0 * ru_fe(1,6,i) - &
               0.00010d0 * ru_fe(1,6,i-1) - 0.00016d0 * ru_fe(1,6,i-2) - &
               0.00021d0 * ru_fe(1,6,i-3) - 0.00021d0 * ru_fe(1,6,i-4) - &
               0.00015d0 * ru_fe(1,6,i-5)) - (-0.00004d0 * ru(1,6,i) - &
               0.00010d0 * ru(1,6,i-1) - 0.00016d0 * ru(1,6,i-2) - &
               0.00021d0 * ru(1,6,i-3) - 0.00021d0 * ru(1,6,i-4) - &
               0.00015d0 * ru(1,6,i-5))
            
            ! (495) DPM4044_FE 
            dp_fe(1,7,i) = (-0.00057d0 * ru_fe(1,7,i) - &
               0.00066d0 * ru_fe(1,7,i-1) - 0.00044d0 * ru_fe(1,7,i-2) - &
               0.00009d0 * ru_fe(1,7,i-3) + 0.00022d0 * ru_fe(1,7,i-4) + &
               0.00031d0 * ru_fe(1,7,i-5)) - (-0.00057d0 * ru(1,7,i) - &
               0.00066d0 * ru(1,7,i-1) - 0.00044d0 * ru(1,7,i-2) - &
               0.00009d0 * ru(1,7,i-3) + 0.00022d0 * ru(1,7,i-4) + &
               0.00031d0 * ru(1,7,i-5))
            
            ! (496) DPM4549_FE 
            dp_fe(1,8,i) = (-0.00002d0 * ru_fe(1,8,i) - &
               0.00016d0 * ru_fe(1,8,i-1) - 0.00034d0 * ru_fe(1,8,i-2) - &
               0.00049d0 * ru_fe(1,8,i-3) - 0.00054d0 * ru_fe(1,8,i-4) - &
               0.00040d0 * ru_fe(1,8,i-5)) - (-0.00002d0 * ru(1,8,i) - &
               0.00016d0 * ru(1,8,i-1) - 0.00034d0 * ru(1,8,i-2) - &
               0.00049d0 * ru(1,8,i-3) - 0.00054d0 * ru(1,8,i-4) - &
               0.00040d0 * ru(1,8,i-5))
            
            ! (497) DPM5054_FE 
            dp_fe(1,9,i) = (0.00112d0 * ru_fe(1,9,i) + &
               0.00103d0 *  ru_fe(1,9,i-1) + 0.00023d0 * ru_fe(1,9,i-2) - &
               0.00078d0 * ru_fe(1,9,i-3) - 0.00149d0 * ru_fe(1,9,i-4) - &
               0.00139d0 * ru_fe(1,9,i-5)) - ( 0.00112d0 * ru(1,9,i) + &
               0.00103d0 * ru(1,9,i-1) + 0.00023d0 * ru(1,9,i-2) - &
               0.00078d0 * ru(1,9,i-3) - 0.00149d0 * ru(1,9,i-4) - &
               0.00139d0 * ru(1,9,i-5))
            
            ! (498) DPM5559_FE 
            dp_fe(1,10,i) = (0.00062d0 * ru_fe(1,10,i) + &
               0.00041d0 * ru_fe(1,10,i-1) - 0.00026d0 * ru_fe(1,10,i-2) - &
               0.00101d0 * ru_fe(1,10,i-3) - 0.00147d0 * ru_fe(1,10,i-4) - &
               0.00126d0 * ru_fe(1,10,i-5)) - (0.00062d0 * ru(1,10,i) + &
               0.00041d0 * ru(1,10,i-1) - 0.00026d0 * ru(1,10,i-2) - &
               0.00101d0 * ru(1,10,i-3) - 0.00147d0 * ru(1,10,i-4) - &
               0.00126d0 * ru(1,10,i-5))
            
            ! (499) DPM6064_FE 
            dp_fe(1,11,i) = (0.00203d0 * ru_fe(1,11,i) + &
               0.00160d0 * ru_fe(1,11,i-1) - 0.00021d0 * ru_fe(1,11,i-2) - &
               0.00235d0 * ru_fe(1,11,i-3) - 0.00374d0 * ru_fe(1,11,i-4) - &
               0.00331d0 * ru_fe(1,11,i-5)) - (0.00203d0 * ru(1,11,i) + &
               0.00160d0 * ru(1,11,i-1) - 0.00021d0 * ru(1,11,i-2) - &
               0.00235d0 * ru(1,11,i-3) - 0.00374d0 * ru(1,11,i-4) - &
               0.00331d0 * ru(1,11,i-5))
            
            ! (500) DPM6569_FE 
            dp_fe(1,12,i) = (0.00067d0 * ru_fe(1,12,i) + &
               0.00040d0 * ru_fe(1,12,i-1) - 0.00040d0 * ru_fe(1,12,i-2) - &
               0.00127d0 * ru_fe(1,12,i-3) - 0.00178d0 * ru_fe(1,12,i-4) - &
               0.00151d0 * ru_fe(1,12,i-5)) - (0.00067d0 * ru(1,12,i) + &
               0.00040d0 * ru(1,12,i-1) - 0.00040d0 * ru(1,12,i-2) - &
               0.00127d0 * ru(1,12,i-3) - 0.00178d0 * ru(1,12,i-4) - &
               0.00151d0 * ru(1,12,i-5))
            
            ! (501) DPM7074_FE 
            dp_fe(1,13,i) = (-0.00013d0 * ru_fe(1,13,i) - &
               0.00016d0 * ru_fe(1,13,i-1) - 0.00013d0 * ru_fe(1,13,i-2) - &
               0.00006d0 * ru_fe(1,13,i-3) + 0.00000d0 * ru_fe(1,13,i-4) + &
               0.00003d0 * ru_fe(1,13,i-5)) - (-0.00013d0 * ru(1,13,i) - &
               0.00016d0 * ru(1,13,i-1) - 0.00013d0 * ru(1,13,i-2) - &
               0.00006d0 * ru(1,13,i-3) + 0.00000d0 * ru(1,13,i-4) + &
               0.00003d0 * ru(1,13,i-5))

            ! (502) DPF1617_FE 
            dp_fe(2,1,i) = (-0.00224d0 * ru_fe(2,1,i) - &
               0.00239d0 * ru_fe(2,1,i-1) - 0.00126d0 * ru_fe(2,1,i-2) + &
               0.00035d0 * ru_fe(2,1,i-3) + 0.00163d0 * ru_fe(2,1,i-4) + &
               0.00178d0 * ru_fe(2,1,i-5)) - (-0.00224d0 *  ru(2,1,i) - &
               0.00239d0 * ru(2,1,i-1) - 0.00126d0 * ru(2,1,i-2) + &
               0.00035d0 * ru(2,1,i-3) + 0.00163d0 * ru(2,1,i-4) + &
               0.00178d0 * ru(2,1,i-5))
            
            ! (503) DPF1819_FE 
            dp_fe(2,2,i) = (-0.00124d0 * ru_fe(2,2,i) - &
               0.00147d0 * ru_fe(2,2,i-1) - 0.00106d0 * ru_fe(2,2,i-2) - &
               0.00035d0 * ru_fe(2,2,i-3) + 0.00030d0 * ru_fe(2,2,i-4) + &
               0.00053d0 * ru_fe(2,2,i-5)) - (-0.00124d0 *  ru(2,2,i) - &
               0.00147d0 * ru(2,2,i-1) - 0.00106d0 * ru(2,2,i-2) - &
               0.00035d0 * ru(2,2,i-3) + 0.00030d0 * ru(2,2,i-4) + &
               0.00053d0 * ru(2,2,i-5))
            
            ! (504) DPF2024_FE 
            dp_fe(2,3,i) = (-0.00087d0 * ru_fe(2,3,i) - &
               0.00099d0 * ru_fe(2,3,i-1) - 0.00063d0 * ru_fe(2,3,i-2) - &
               0.00007d0 * ru_fe(2,3,i-3) + 0.00041d0 * ru_fe(2,3,i-4) + &
               0.00052d0 * ru_fe(2,3,i-5)) - (-0.00087d0 *  ru(2,3,i) - &
               0.00099d0 * ru(2,3,i-1) - 0.00063d0 * ru(2,3,i-2) - &
               0.00007d0 * ru(2,3,i-3) + 0.00041d0 * ru(2,3,i-4) + &
               0.00052d0 * ru(2,3,i-5))
            
            ! (505) DPF2529_FE 
            dp_fe(2,4,i) = (-0.00056d0 * ru_fe(2,4,i) - &
               0.00070d0 * ru_fe(2,4,i-1) - 0.00057d0 * ru_fe(2,4,i-2) - &
               0.00029d0 * ru_fe(2,4,i-3) - 0.00002d0 * ru_fe(2,4,i-4) + &
               0.00013d0 * ru_fe(2,4,i-5)) - (-0.00056d0 *  ru(2,4,i) - &
               0.00070d0 * ru(2,4,i-1) - 0.00057d0 * ru(2,4,i-2) - &
               0.00029d0 * ru(2,4,i-3) - 0.00002d0 * ru(2,4,i-4) + &
               0.00013d0 * ru(2,4,i-5))
            
            ! (506) DPF3034_FE 
            dp_fe(2,5,i) = (-0.00081d0 * ru_fe(2,5,i) - &
               0.00065d0 * ru_fe(2,5,i-1) + 0.00005d0 * ru_fe(2,5,i-2) + &
               0.00089d0 * ru_fe(2,5,i-3) + 0.00143d0 * ru_fe(2,5,i-4) + &
               0.00128d0 * ru_fe(2,5,i-5)) - (-0.00081d0 *  ru(2,5,i) - &
               0.00065d0 * ru(2,5,i-1) + 0.00005d0 * ru(2,5,i-2) + &
               0.00089d0 * ru(2,5,i-3) + 0.00143d0 * ru(2,5,i-4) + &
               0.00128d0 * ru(2,5,i-5))
            
            ! (507) DPF3539_FE 
            dp_fe(2,6,i) = (-0.00195d0 * ru_fe(2,6,i) - &
               0.00216d0 * ru_fe(2,6,i-1) - 0.00128d0 * ru_fe(2,6,i-2) + &
               0.00002d0 * ru_fe(2,6,i-3) + 0.00111d0 * ru_fe(2,6,i-4) + &
               0.00132d0 * ru_fe(2,6,i-5)) - (-0.00195d0 *  ru(2,6,i) - &
               0.00216d0 * ru(2,6,i-1) - 0.00128d0 * ru(2,6,i-2) + &
               0.00002d0 * ru(2,6,i-3) + 0.00111d0 * ru(2,6,i-4) + &
               0.00132d0 * ru(2,6,i-5))
            
            ! (508) DPF4044_FE 
            dp_fe(2,7,i) = (-0.00026d0 * ru_fe(2,7,i) - &
               0.00050d0 * ru_fe(2,7,i-1) - 0.00068d0 * ru_fe(2,7,i-2) - &
               0.00076d0 * ru_fe(2,7,i-3) - 0.00070d0 * ru_fe(2,7,i-4) - &
               0.00046d0 * ru_fe(2,7,i-5)) - (-0.00026d0 *  ru(2,7,i) - &
               0.00050d0 * ru(2,7,i-1) - 0.00068d0 * ru(2,7,i-2) - &
               0.00076d0 * ru(2,7,i-3) - 0.00070d0 * ru(2,7,i-4) - &
               0.00046d0 * ru(2,7,i-5))
            
            ! (509) DPF4549_FE 
            dp_fe(2,8,i) = (-0.00076d0 * ru_fe(2,8,i) - &
               0.00070d0 * ru_fe(2,8,i-1) - 0.00018d0 * ru_fe(2,8,i-2) + &
               0.00049d0 * ru_fe(2,8,i-3) + 0.00096d0 * ru_fe(2,8,i-4) + &
               0.00091d0 * ru_fe(2,8,i-5)) - (-0.00076d0 *  ru(2,8,i) - &
               0.00070d0 * ru(2,8,i-1) - 0.00018d0 * ru(2,8,i-2) + &
               0.00049d0 * ru(2,8,i-3) + 0.00096d0 * ru(2,8,i-4) + &
               0.00091d0 * ru(2,8,i-5))
            
            ! (510) DPF5054_FE 
            dp_fe(2,9,i) = (0.00003d0 * ru_fe(2,9,i)  - &
               0.00011d0 * ru_fe(2,9,i-1) - 0.00032d0 * ru_fe(2,9,i-2) - &
               0.00051d0 * ru_fe(2,9,i-3) - 0.00059d0 * ru_fe(2,9,i-4) - &
               0.00045d0 * ru_fe(2,9,i-5)) - ( 0.00003d0 *  ru(2,9,i) - &
               0.00011d0 * ru(2,9,i-1) - 0.00032d0 * ru(2,9,i-2) - &
               0.00051d0 * ru(2,9,i-3) - 0.00059d0 * ru(2,9,i-4) - &
               0.00045d0 * ru(2,9,i-5))
            
            ! (511) DPF5559_FE 
            dp_fe(2,10,i) = (0.00064d0 * ru_fe(2,10,i) + &
               0.00041d0 * ru_fe(2,10,i-1) - 0.00029d0 * ru_fe(2,10,i-2) - &
               0.00107d0 * ru_fe(2,10,i-3) - 0.00155d0 * ru_fe(2,10,i-4) - &
               0.00132d0 * ru_fe(2,10,i-5)) - (0.00064d0 * ru(2,10,i) + &
               0.00041d0 * ru(2,10,i-1) - 0.00029d0 * ru(2,10,i-2) - &
               0.00107d0 * ru(2,10,i-3) - 0.00155d0 * ru(2,10,i-4) - &
               0.00132d0 * ru(2,10,i-5))
            
            ! (512) DPF6064_FE 
            dp_fe(2,11,i) = (0.00141d0 * ru_fe(2,11,i) + &
               0.00166d0 * ru_fe(2,11,i-1) + 0.00116d0 * ru_fe(2,11,i-2) + &
               0.00033d0 * ru_fe(2,11,i-3) - 0.00041d0 * ru_fe(2,11,i-4) - &
               0.00066d0 * ru_fe(2,11,i-5)) - (0.00141d0 * ru(2,11,i) + &
               0.00166d0 * ru(2,11,i-1) + 0.00116d0 * ru(2,11,i-2) + &
               0.00033d0 * ru(2,11,i-3) - 0.00041d0 * ru(2,11,i-4) - &
               0.00066d0 * ru(2,11,i-5))
            
            ! (513) DPF6569_FE 
            dp_fe(2,12,i) = (0.00029d0 * ru_fe(2,12,i) + &
               0.00014d0 * ru_fe(2,12,i-1) - 0.00023d0 * ru_fe(2,12,i-2) - &
               0.00063d0 * ru_fe(2,12,i-3) - 0.00086d0 * ru_fe(2,12,i-4) - &
               0.00072d0 * ru_fe(2,12,i-5)) - (0.00029d0 * ru(2,12,i) + &
               0.00014d0 * ru(2,12,i-1) - 0.00023d0 * ru(2,12,i-2) - &
               0.00063d0 * ru(2,12,i-3) - 0.00086d0 * ru(2,12,i-4) - &
               0.00072d0 * ru(2,12,i-5))
            
            ! (514) DPF7074_FE 
            dp_fe(2,13,i) = (-0.00009d0 * ru_fe(2,13,i) - &
               0.00028d0 * ru_fe(2,13,i-1) - 0.00048d0 * ru_fe(2,13,i-2) - &
               0.00063d0 * ru_fe(2,13,i-3) - 0.00064d0 * ru_fe(2,13,i-4) - &
               0.00046d0 * ru_fe(2,13,i-5)) - (-0.00009d0 * ru(2,13,i) - &
               0.00028d0 * ru(2,13,i-1) - 0.00048d0 * ru(2,13,i-2) - &
               0.00063d0 * ru(2,13,i-3) - 0.00064d0 * ru(2,13,i-4) - &
               0.00046d0 * ru(2,13,i-5))
                 
         end do

      else
      
         ! Not needed for now
      
      end if
   
   end subroutine SolveLaborForcePartRate1674
   
!===============================================================================

   subroutine SolveLaborForcePartRatePrelim()

      integer :: i, age, ageGrp

      ! Equations 515-520

      do i = startQtr, endQtr

         ! (515) PM16O_P
         do ageGrp = 1, 9
            p_p(1,24,i) = p_p(1,24,i) + p_p(1,ageGrp,i) * n(1,ageGrp,i)
         end do
         do age = 55, 79
            p_p(1,24,i) = p_p(1,24,i) + psy_p(1,age,i) * nisy(1,age,i)
         end do
         p_p(1,24,i) = p_p(1,24,i) + p80o_p(1,i) * sum(nisy(1,80:100,i))
         p_p(1,24,i) = p_p(1,24,i) / n(1,24,i)

         ! (516) LCM_P
         lc_p(1,0,i) = p_p(1,24,i) * n(1,24,i)

         ! (517) PF16O_P
         do ageGrp = 1, 9
            p_p(2,24,i) = p_p(2,24,i) + p_p(2,ageGrp,i) * n(2,ageGrp,i)
         end do
         do age = 55, 79
            p_p(2,24,i) = p_p(2,24,i) + psy_p(2,age,i) * nisy(2,age,i)
         end do
         p_p(2,24,i) = p_p(2,24,i) + p80o_p(2,i) * sum(nisy(2,80:100,i))
         p_p(2,24,i) = p_p(2,24,i) / n(2,24,i)

         ! (518) LCF_P
         lc_p(2,0,i) = p_p(2,24,i) * n(2,24,i)

         ! (519) P16O_P
         p_p(0,24,i) =  (p_p(1,24,i) * n(1,24,i) + &
                         p_p(2,24,i) * n(2,24,i)) / &
                         (n(1,24,i) + n(2,24,i))
         ! (520) LC_P
         lc_p(0,0,i) = lc_p(1,0,i) + lc_p(2,0,i)

      end do

   end subroutine SolveLaborForcePartRatePrelim

!===============================================================================

end module EconModSol1EquationsMod