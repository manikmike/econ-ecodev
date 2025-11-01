module EconModSolAEquationsMod

   use EconParMod
   use EconModSolAVarMod
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSolAEquationsMain, GetTrend

   ! Equation parameters for the AWW model
   ! Males (1) are rows 1-13, Females (2) are rows 14-26
   ! Age groups (1-13) are 1617, 1819, 2024, 2529, 3034, ..., 6569, 70o
   ! Columns (1-3) are trend, unemployment rate, and constant
   real (kind = 8), parameter, dimension(2,13,3) :: k = reshape( &
      (/ -0.10135d0,    0.10821d0,   38.87103d0, &
         -0.26079d0,   -0.16399d0,   59.94278d0, &
         -0.10444d0,   -0.20399d0,   52.93407d0, &
         -0.07062d0,   -0.25689d0,   54.60044d0, &
         -0.11854d0,   -0.26421d0,   60.49258d0, &
         -0.01961d0,   -0.12194d0,   50.95524d0, &
          0.00877d0,   -0.1792d0,    48.77217d0, &
          0.0592d0,    -0.1605d0,    43.61904d0, &
          0.06994d0,   -0.20015d0,   42.40736d0, &
          0.08423d0,   -0.14701d0,   39.4802d0,  &
          0.16591d0,   -0.04181d0,   25.83782d0, &
          0.09235d0,   -0.05071d0,   28.2055d0,  &
          0.20038d0,   -0.46791d0,   16.99531d0, &
         -0.12302d0,    0.12897d0,   41.41885d0, &
         -0.29702d0,   -0.1394d0,    61.8747d0,  &
         -0.10879d0,   -0.21126d0,   50.64397d0, &
         -0.1015d0,    -0.22016d0,   53.73949d0, &
         -0.17025d0,   -0.12264d0,   61.65237d0, &
         -0.14313d0,   -0.06333d0,   60.5055d0,  &
         -0.12561d0,   -0.09314d0,   60.30694d0, &
         -0.07918d0,   -0.11812d0,   56.25816d0, &
         -0.05666d0,   -0.18944d0,   54.1127d0,  &
         -0.08189d0,    0.03822d0,   55.14774d0, &
          0.02421d0,    0.03288d0,   39.72131d0, &
         -0.02386d0,   -0.24361d0,   40.86353d0, &
          0.22062d0,   -0.09578d0,   14.58432d0 /), &
	  shape(k), order = (/ 3, 2, 1/))

   integer, parameter :: phaseoutStartYr = 123

   ! trend_te series for AWW model
   ! If above equation parameters are reestimated, phaseoutStartYr should be reset.
   ! Above equation parameters were reestimated in summer 2024 (for upcoming TR25) and phaseoutStartYr was set to 2023.
   ! "In general, the trend phaseout should start after the end of the estimation period."
   ! From: Sinclair, Sven. Sent: Monday, 19 August, 2024 08:31. Subject: RE: model run: TR242BE06.   
   real (kind = 8), dimension(MAX_YR) :: trend_te
   
contains

!===============================================================================

   subroutine GetTrend()

      integer :: yr
      double precision :: inc
   
      do yr = 1, phaseoutStartYr-1           ! EViews @trend function
         trend_te(yr) = dble(yr) - 1d0       ! aka, Aremos year minus one
      end do

      inc = 0.9d0
      do yr = phaseoutStartYr, phaseoutStartYr+8
         trend_te(yr) = trend_te(yr-1) + inc ! Phaseout by one-tenth each year
         inc = inc - 0.1d0
      end do

      do yr = phaseoutStartYr+9, endYr
         trend_te(yr) = trend_te(yr-1)       ! Hold constant
      end do

   end subroutine GetTrend

!===============================================================================

   subroutine EconModSolAEquationsMain

      call SolveCoveredEmployment15u()                ! Equations   1- 52
      call SolveAverageWeeksTotalEmployment()         ! Equations  53-279
      call SolveEmploymentAtAnyTime()                 ! Equations 280-401
      call SolveEquations402_423()                    ! Equations 402-423

   end subroutine EconModSolAEquationsMain

!===============================================================================

   subroutine SolveCoveredEmployment15u()

      integer :: i
            
      ! Equations 1 - 52
!      do i = max(startYr, lastDataYr+1), endYr
      do i = startYr, endYr
      
         if (i>=histend+1) then
            ! (1) HE_M_M0 
            he_m_sy(1,0,i) = ( he_m_sy(1,0,histend)   / nsy_a(1,0,histend) &
                             + he_m_sy(1,0,histend-1) / nsy_a(1,0,histend-1) &
                             + he_m_sy(1,0,histend-2) / nsy_a(1,0,histend-2) ) / 3 &
                             *   nsy_a(1,0,i)
            
            ! (2) HE_M_M1 
            he_m_sy(1,1,i) = ( he_m_sy(1,1,histend)   / nsy_a(1,1,histend) &
                             + he_m_sy(1,1,histend-1) / nsy_a(1,1,histend-1) &
                             + he_m_sy(1,1,histend-2) / nsy_a(1,1,histend-2) ) / 3 &
                             *   nsy_a(1,1,i)
            
            ! (3) HE_M_M2 
            he_m_sy(1,2,i) = ( he_m_sy(1,2,histend)   / nsy_a(1,2,histend) &
                             + he_m_sy(1,2,histend-1) / nsy_a(1,2,histend-1) &
                             + he_m_sy(1,2,histend-2) / nsy_a(1,2,histend-2) ) / 3 &
                             *   nsy_a(1,2,i)
            
            ! (4) HE_M_M3 
            he_m_sy(1,3,i) = ( he_m_sy(1,3,histend)   / nsy_a(1,3,histend) &
                             + he_m_sy(1,3,histend-1) / nsy_a(1,3,histend-1) &
                             + he_m_sy(1,3,histend-2) / nsy_a(1,3,histend-2) ) / 3 &
                             *   nsy_a(1,3,i)
            
            ! (5) HE_M_M4 
            he_m_sy(1,4,i) = ( he_m_sy(1,4,histend)   / nsy_a(1,4,histend) &
                             + he_m_sy(1,4,histend-1) / nsy_a(1,4,histend-1) &
                             + he_m_sy(1,4,histend-2) / nsy_a(1,4,histend-2) ) / 3 &
                             *   nsy_a(1,4,i)
            
            ! (6) HE_M_M5 
            he_m_sy(1,5,i) = ( he_m_sy(1,5,histend)   / nsy_a(1,5,histend) &
                             + he_m_sy(1,5,histend-1) / nsy_a(1,5,histend-1) &
                             + he_m_sy(1,5,histend-2) / nsy_a(1,5,histend-2) ) / 3 &
                             *   nsy_a(1,5,i)
            
            ! (7) HE_M_M6 
            he_m_sy(1,6,i) = ( he_m_sy(1,6,histend)   / nsy_a(1,6,histend) &
                             + he_m_sy(1,6,histend-1) / nsy_a(1,6,histend-1) &
                             + he_m_sy(1,6,histend-2) / nsy_a(1,6,histend-2) ) / 3 &
                             *   nsy_a(1,6,i)
            
            ! (8) HE_M_M7 
            he_m_sy(1,7,i) = ( he_m_sy(1,7,histend)   / nsy_a(1,7,histend) &
                             + he_m_sy(1,7,histend-1) / nsy_a(1,7,histend-1) &
                             + he_m_sy(1,7,histend-2) / nsy_a(1,7,histend-2) ) / 3 &
                             *   nsy_a(1,7,i)
            
            ! (9) HE_M_M8 
            he_m_sy(1,8,i) = ( he_m_sy(1,8,histend)   / nsy_a(1,8,histend) &
                             + he_m_sy(1,8,histend-1) / nsy_a(1,8,histend-1) &
                             + he_m_sy(1,8,histend-2) / nsy_a(1,8,histend-2) ) / 3 &
                             *   nsy_a(1,8,i)
            
            ! (10) HE_M_M9 
            he_m_sy(1,9,i) = ( he_m_sy(1,9,histend)   / nsy_a(1,9,histend) &
                             + he_m_sy(1,9,histend-1) / nsy_a(1,9,histend-1) &
                             + he_m_sy(1,9,histend-2) / nsy_a(1,9,histend-2) ) / 3 &
                             *   nsy_a(1,9,i)
            
            ! (11) - (20) 
            ce_m_sy(1,0:9,i) = he_m_sy(1,0:9,i)
            
            ! (21) HE_M_M1013 
            
            he_m_sy(1,10,i) = ( he_m_sy(1,10,histend)   / nsy_a(1,10,histend) &
                              + he_m_sy(1,10,histend-1) / nsy_a(1,10,histend-1) &
                              + he_m_sy(1,10,histend-2) / nsy_a(1,10,histend-2) ) / 3 &
                              *   nsy_a(1,10,i)

            he_m_sy(1,11,i) = ( he_m_sy(1,11,histend)   / nsy_a(1,11,histend) &
                              + he_m_sy(1,11,histend-1) / nsy_a(1,11,histend-1) &
                              + he_m_sy(1,11,histend-2) / nsy_a(1,11,histend-2) ) / 3 &
                              *   nsy_a(1,11,i)
            
            he_m_sy(1,12,i) = ( he_m_sy(1,12,histend)   / nsy_a(1,12,histend) &
                              + he_m_sy(1,12,histend-1) / nsy_a(1,12,histend-1) &
                              + he_m_sy(1,12,histend-2) / nsy_a(1,12,histend-2) ) / 3 &
                              *   nsy_a(1,12,i)
            
            he_m_sy(1,13,i) = ( he_m_sy(1,13,histend)   / nsy_a(1,13,histend) &
                              + he_m_sy(1,13,histend-1) / nsy_a(1,13,histend-1) &
                              + he_m_sy(1,13,histend-2) / nsy_a(1,13,histend-2) ) / 3 &
                              *   nsy_a(1,13,i)
            
            ce_m_sy(1,10:13,i) = he_m_sy(1,10:13,i)
            
            he_m_1013(1,i) = sum(he_m_sy(1,10:13,i))
           
            
            ! (22) HE_M_M1415 
            
            he_m_sy(1,14,i) = ( he_m_sy(1,14,histend)   / nsy_a(1,14,histend) &
                              + he_m_sy(1,14,histend-1) / nsy_a(1,14,histend-1) &
                              + he_m_sy(1,14,histend-2) / nsy_a(1,14,histend-2) ) / 3 &
                              *   nsy_a(1,14,i)
            
            he_m_sy(1,15,i) = ( he_m_sy(1,15,histend)   / nsy_a(1,15,histend) &
                              + he_m_sy(1,15,histend-1) / nsy_a(1,15,histend-1) &
                              + he_m_sy(1,15,histend-2) / nsy_a(1,15,histend-2) ) / 3 &
                              *   nsy_a(1,15,i)

            ce_m_sy(1,14:15,i) = he_m_sy(1,14:15,i)
            
            he_m_1415(1,i) = sum(he_m_sy(1,14:15,i))
            
            ! (23) HE_M_M15U
            he_m_15u(1,i) = sum(he_m_sy(1,0:9,i)) + he_m_1013(1,i) + he_m_1415(1,i)
            
            ! (24) CE_M_M1013
            ce_m_1013(1,i) = he_m_1013(1,i)
            
            ! (25) CE_M_M1415
            ce_m_1415(1,i) = he_m_1415(1,i)
            
            ! (26) CE_M_M15U
            ce_m_15u(1,i) = sum(ce_m_sy(1,0:9,i)) + ce_m_1013(1,i) + ce_m_1415(1,i)

            ! (27) HE_M_F0 
            ! he_m_sy(2,0,i) = 0.00028d0 * nsy_a(2,0,i)
            
            he_m_sy(2,0,i) = ( he_m_sy(2,0,histend)   / nsy_a(2,0,histend) &
                             + he_m_sy(2,0,histend-1) / nsy_a(2,0,histend-1) &
                             + he_m_sy(2,0,histend-2) / nsy_a(2,0,histend-2) ) / 3 &
                             *   nsy_a(2,0,i)
            
            ! (28) HE_M_F1 
            ! he_m_sy(2,1,i) = 0.00028d0 * nsy_a(2,1,i)
            
            he_m_sy(2,1,i) = ( he_m_sy(2,1,histend)   / nsy_a(2,1,histend) &
                             + he_m_sy(2,1,histend-1) / nsy_a(2,1,histend-1) &
                             + he_m_sy(2,1,histend-2) / nsy_a(2,1,histend-2) ) / 3 &
                             *   nsy_a(2,1,i)
            
            ! (29) HE_M_F2 
            ! he_m_sy(2,2,i) = 0.00023d0 * nsy_a(2,2,i)
            
            he_m_sy(2,2,i) = ( he_m_sy(2,2,histend)   / nsy_a(2,2,histend) &
                             + he_m_sy(2,2,histend-1) / nsy_a(2,2,histend-1) &
                             + he_m_sy(2,2,histend-2) / nsy_a(2,2,histend-2) ) / 3 &
                             *   nsy_a(2,2,i)
            
            ! (30) HE_M_F3 
            ! he_m_sy(2,3,i) = 0.00020d0 * nsy_a(2,3,i)
            
            he_m_sy(2,3,i) = ( he_m_sy(2,3,histend)   / nsy_a(2,3,histend) &
                             + he_m_sy(2,3,histend-1) / nsy_a(2,3,histend-1) &
                             + he_m_sy(2,3,histend-2) / nsy_a(2,3,histend-2) ) / 3 &
                             *   nsy_a(2,3,i)
            
            ! (31) HE_M_F4 
            ! he_m_sy(2,4,i) = 0.00031d0 * nsy_a(2,4,i)
            
            he_m_sy(2,4,i) = ( he_m_sy(2,4,histend)   / nsy_a(2,4,histend) &
                             + he_m_sy(2,4,histend-1) / nsy_a(2,4,histend-1) &
                             + he_m_sy(2,4,histend-2) / nsy_a(2,4,histend-2) ) / 3 &
                             *   nsy_a(2,4,i)
            
            ! (32) HE_M_F5 
            ! he_m_sy(2,5,i) = 0.00041d0 * nsy_a(2,5,i)
            
            he_m_sy(2,5,i) = ( he_m_sy(2,5,histend)   / nsy_a(2,5,histend) &
                             + he_m_sy(2,5,histend-1) / nsy_a(2,5,histend-1) &
                             + he_m_sy(2,5,histend-2) / nsy_a(2,5,histend-2) ) / 3 &
                             *   nsy_a(2,5,i)
            
            ! (33) HE_M_F6 
            ! he_m_sy(2,6,i) = 0.00055d0 * nsy_a(2,6,i)
            
            he_m_sy(2,6,i) = ( he_m_sy(2,6,histend)   / nsy_a(2,6,histend) &
                             + he_m_sy(2,6,histend-1) / nsy_a(2,6,histend-1) &
                             + he_m_sy(2,6,histend-2) / nsy_a(2,6,histend-2) ) / 3 &
                             *   nsy_a(2,6,i)
            
            ! (34) HE_M_F7 
            ! he_m_sy(2,7,i) = 0.00129d0 * nsy_a(2,7,i)
            
            he_m_sy(2,7,i) = ( he_m_sy(2,7,histend)   / nsy_a(2,7,histend) &
                             + he_m_sy(2,7,histend-1) / nsy_a(2,7,histend-1) &
                             + he_m_sy(2,7,histend-2) / nsy_a(2,7,histend-2) ) / 3 &
                             *   nsy_a(2,7,i)
            
            ! (35) HE_M_F8 
            ! he_m_sy(2,8,i) = 0.00156d0 * nsy_a(2,8,i)
            
            he_m_sy(2,8,i) = ( he_m_sy(2,8,histend)   / nsy_a(2,8,histend) &
                             + he_m_sy(2,8,histend-1) / nsy_a(2,8,histend-1) &
                             + he_m_sy(2,8,histend-2) / nsy_a(2,8,histend-2) ) / 3 &
                             *   nsy_a(2,8,i)
            
            ! (36) HE_M_F9 
            ! he_m_sy(2,9,i) = 0.00212d0 * nsy_a(2,9,i)
            
            he_m_sy(2,9,i) = ( he_m_sy(2,9,histend)   / nsy_a(2,9,histend) &
                             + he_m_sy(2,9,histend-1) / nsy_a(2,9,histend-1) &
                             + he_m_sy(2,9,histend-2) / nsy_a(2,9,histend-2) ) / 3 &
                             *   nsy_a(2,9,i)

            ! (37) - (46) 
            ce_m_sy(2,0:9,i) = he_m_sy(2,0:9,i)
            
            ! (47) HE_M_F1013 
            
            he_m_sy(2,10,i) = ( he_m_sy(2,10,histend)   / nsy_a(2,10,histend) &
                              + he_m_sy(2,10,histend-1) / nsy_a(2,10,histend-1) &
                              + he_m_sy(2,10,histend-2) / nsy_a(2,10,histend-2) ) / 3 &
                              *   nsy_a(2,10,i)

            he_m_sy(2,11,i) = ( he_m_sy(2,11,histend)   / nsy_a(2,11,histend) &
                              + he_m_sy(2,11,histend-1) / nsy_a(2,11,histend-1) &
                              + he_m_sy(2,11,histend-2) / nsy_a(2,11,histend-2) ) / 3 &
                              *   nsy_a(2,11,i)
            
            he_m_sy(2,12,i) = ( he_m_sy(2,12,histend)   / nsy_a(2,12,histend) &
                              + he_m_sy(2,12,histend-1) / nsy_a(2,12,histend-1) &
                              + he_m_sy(2,12,histend-2) / nsy_a(2,12,histend-2) ) / 3 &
                              *   nsy_a(2,12,i)
            
            he_m_sy(2,13,i) = ( he_m_sy(2,13,histend)   / nsy_a(2,13,histend) &
                              + he_m_sy(2,13,histend-1) / nsy_a(2,13,histend-1) &
                              + he_m_sy(2,13,histend-2) / nsy_a(2,13,histend-2) ) / 3 &
                              *   nsy_a(2,13,i)
            
            ce_m_sy(2,10:13,i) = he_m_sy(2,10:13,i)
            
            he_m_1013(2,i) = sum(he_m_sy(2,10:13,i))
           
            ! (48) HE_M_F1415 
            
            he_m_sy(2,14,i) = ( he_m_sy(2,14,histend)   / nsy_a(2,14,histend) &
                              + he_m_sy(2,14,histend-1) / nsy_a(2,14,histend-1) &
                              + he_m_sy(2,14,histend-2) / nsy_a(2,14,histend-2) ) / 3 &
                              *   nsy_a(2,14,i)
            
            he_m_sy(2,15,i) = ( he_m_sy(2,15,histend)   / nsy_a(2,15,histend) &
                              + he_m_sy(2,15,histend-1) / nsy_a(2,15,histend-1) &
                              + he_m_sy(2,15,histend-2) / nsy_a(2,15,histend-2) ) / 3 &
                              *   nsy_a(2,15,i)

            ce_m_sy(2,14:15,i) = he_m_sy(2,14:15,i)

            he_m_1415(2,i) = sum(he_m_sy(2,14:15,i))
            
            ! (49) HE_M_F15U
            he_m_15u(2,i) = sum(he_m_sy(2,0:9,i)) + he_m_1013(2,i) + he_m_1415(2,i)
            
            ! (50) CE_M_F1013
            ce_m_1013(2,i) = he_m_1013(2,i)
            
            ! (51) CE_M_F1415
            ce_m_1415(2,i) = he_m_1415(2,i)
            
            ! (52) CE_M_F15U
            ce_m_15u(2,i) = sum(ce_m_sy(2,0:9,i)) + ce_m_1013(2,i) + ce_m_1415(2,i)
               
         end if  
            
      end do

   end subroutine SolveCoveredEmployment15u

!===============================================================================

   subroutine SolveAverageWeeksTotalEmployment()

      integer :: i, ageGrp
      
      ! Equations  53 - 279
      
      ! TODO: Figure out last data year for each series
      do i = startYr, endYr
      !do i = 111, endYr

         ! (53) AWWM1617_P 
         aww_p(1,1,i) = k(1,1,1) * trend_te(i) + k(1,1,2) * ru_a(1,1,i) + k(1,1,3)
         aww_p(1,1,i) = aww_p(1,1,i) * awwm_adj(i)
         
         ! (54) WEM1617_3_P 
         we_3_p(1,1,i) = e_a(1,1,i) * 52d0 / aww_p(1,1,i)

         ! (55) AWWM1617_PL 
         aww_pl(1,1,i) = k(1,1,1) * trend_te(i-1) + k(1,1,2) * ru_a(1,1,i-1) + k(1,1,3)
         aww_pl(1,1,i) = aww_pl(1,1,i) * awwm_adj(i-1)
         
         ! (56) WEM1617_3_PL 
          we_3_pl(1,1,i) = e_a(1,1,i-1) * 52d0 / aww_pl(1,1,i)

         ! (57) TEM1617_P 
         te_a_p(1,1,i) = ((we_3_p(1,1,i) / we_3_pl(1,1,i)) * &
                          (te_a(1,1,i-1)-m_a(1,1,i-1)) + m_a(1,1,i)) * &
                           mult1_te(1,1,i) * mult2_te(1,1,i)

         ! (58) AWWM1819_P
          aww_p(1,2,i) = k(1,2,1) * trend_te(i) + k(1,2,2) * ru_a(1,2,i) + k(1,2,3)
          aww_p(1,2,i) = aww_p(1,2,i) * awwm_adj(i)
          
         ! (59) WEM1819_3_P 
         we_3_p(1,2,i) = e_a(1,2,i) * 52d0 / aww_p(1,2,i)

         ! (60) AWWM1819_PL 
         aww_pl(1,2,i) = k(1,2,1) * trend_te(i-1) + k(1,2,2) * ru_a(1,2,i-1) + k(1,2,3)
         aww_pl(1,2,i) = aww_pl(1,2,i) * awwm_adj(i-1)
         
         ! (61) WEM1819_3_PL 
         we_3_pl(1,2,i) = e_a(1,2,i-1) * 52d0 / aww_pl(1,2,i)

         ! (62) TEM1819_P 
         te_a_p(1,2,i) = ((we_3_p(1,2,i) / we_3_pl(1,2,i)) * &
                          (te_a(1,2,i-1)-m_a(1,2,i-1)) + m_a(1,2,i)) * &
                           mult1_te(1,2,i) * mult2_te(1,2,i)

         ! (63) AWWM2024_P 
         aww_p(1,3,i) = k(1,3,1) * trend_te(i) + k(1,3,2) * ru_a(1,3,i) + k(1,3,3)
         aww_p(1,3,i) = aww_p(1,3,i) * awwm_adj(i)
         
         ! (64) WEM2024_3_P 
         we_3_p(1,3,i) = e_a(1,3,i) * 52d0 / aww_p(1,3,i)

         ! (65) AWWM2024_PL 
         aww_pl(1,3,i) = k(1,3,1) * trend_te(i-1) + k(1,3,2) * ru_a(1,3,i-1) + k(1,3,3)
         aww_pl(1,3,i) = aww_pl(1,3,i) * awwm_adj(i-1)
         
         ! (66) WEM2024_3_PL 
         we_3_pl(1,3,i) = e_a(1,3,i-1) * 52d0 / aww_pl(1,3,i)

         ! (67) TEM2024_P 
         te_a_p(1,3,i) = ((we_3_p(1,3,i) / we_3_pl(1,3,i)) * &
                          (te_a(1,3,i-1)-m_a(1,3,i-1)) + m_a(1,3,i)) * &
                           mult1_te(1,3,i) * mult2_te(1,3,i)

         ! (68) AWWM2529_P 
         aww_p(1,4,i) = k(1,4,1) * trend_te(i) + k(1,4,2) * ru_a(1,4,i) + k(1,4,3)
         aww_p(1,4,i) = aww_p(1,4,i) * awwm_adj(i)
         
         ! (69) WEM2529_3_P 
         we_3_p(1,4,i) = e_a(1,4,i) * 52d0 / aww_p(1,4,i)

         ! (70) AWWM2529_PL 
         aww_pl(1,4,i) = k(1,4,1) * trend_te(i-1) + k(1,4,2) * ru_a(1,4,i-1) + k(1,4,3)
         aww_pl(1,4,i) = aww_pl(1,4,i) * awwm_adj(i-1)
         
         ! (71) WEM2529_3_PL 
         we_3_pl(1,4,i) = e_a(1,4,i-1) * 52d0 / aww_pl(1,4,i)

         ! (72) TEM2529_P 
         te_a_p(1,4,i) = ((we_3_p(1,4,i) / we_3_pl(1,4,i)) * &
                          (te_a(1,4,i-1)-m_a(1,4,i-1)) + m_a(1,4,i)) * &
                           mult1_te(1,4,i) * mult2_te(1,4,i)

         ! (73) AWWM3034_P 
         aww_p(1,5,i) = k(1,5,1) * trend_te(i) + k(1,5,2) * ru_a(1,5,i) + k(1,5,3)
         aww_p(1,5,i) = aww_p(1,5,i) * awwm_adj(i)
         
         ! (74) WEM2529_3_P 
         we_3_p(1,5,i) = e_a(1,5,i) * 52d0 / aww_p(1,5,i)

         ! (75) AWWM3034_PL 
         aww_pl(1,5,i) = k(1,5,1) * trend_te(i-1) + k(1,5,2) * ru_a(1,5,i-1) + k(1,5,3)
         aww_pl(1,5,i) = aww_pl(1,5,i) * awwm_adj(i-1)
         
         ! (76) WEM3034_3_PL 
         we_3_pl(1,5,i) = e_a(1,5,i-1) * 52d0 / aww_pl(1,5,i)

         ! (77) TEM3034_P 
         te_a_p(1,5,i) = ((we_3_p(1,5,i) / we_3_pl(1,5,i)) * &
                          (te_a(1,5,i-1)-m_a(1,5,i-1)) + m_a(1,5,i)) * &
                           mult1_te(1,5,i) * mult2_te(1,5,i)

         ! (78) AWWM3539_P 
         aww_p(1,6,i) = k(1,6,1) * trend_te(i) + k(1,6,2) * ru_a(1,6,i) + k(1,6,3)
         aww_p(1,6,i) = aww_p(1,6,i) * awwm_adj(i)
         
         ! (79) WEM3539_3_P 
         we_3_p(1,6,i) = e_a(1,6,i) * 52d0 / aww_p(1,6,i)

         ! (80) AWWM3539_PL 
         aww_pl(1,6,i) = k(1,6,1) * trend_te(i-1) + k(1,6,2) * ru_a(1,6,i-1) + k(1,6,3)
         aww_pl(1,6,i) = aww_pl(1,6,i) * awwm_adj(i-1)
         
         ! (81) WEM3539_3_PL 
         we_3_pl(1,6,i) = e_a(1,6,i-1) * 52d0 / aww_pl(1,6,i)

         ! (82) TEM3539_P 
         te_a_p(1,6,i) = ((we_3_p(1,6,i) / we_3_pl(1,6,i)) * &
                          (te_a(1,6,i-1)-m_a(1,6,i-1)) + m_a(1,6,i)) * &
                           mult1_te(1,6,i) * mult2_te(1,6,i)

         ! (83) AWWM4044_P 
         aww_p(1,7,i) = k(1,7,1) * trend_te(i) + k(1,7,2) * ru_a(1,7,i) + k(1,7,3)
         aww_p(1,7,i) = aww_p(1,7,i) * awwm_adj(i)
         
         ! (84) WEM4044_3_P 
         we_3_p(1,7,i) = e_a(1,7,i) * 52d0 / aww_p(1,7,i)

         ! (85) AWWM4044_PL 
         aww_pl(1,7,i) = k(1,7,1) * trend_te(i-1) + k(1,7,2) * ru_a(1,7,i-1) + k(1,7,3)
         aww_pl(1,7,i) = aww_pl(1,7,i) * awwm_adj(i-1)
         
         ! (86) WEM4044_3_PL 
         we_3_pl(1,7,i) = e_a(1,7,i-1) * 52d0 / aww_pl(1,7,i)

         ! (87) TEM4044_P 
         te_a_p(1,7,i) = ((we_3_p(1,7,i) / we_3_pl(1,7,i)) * &
                          (te_a(1,7,i-1)-m_a(1,7,i-1)) + m_a(1,7,i)) * &
                           mult1_te(1,7,i) * mult2_te(1,7,i)

         ! (88) AWWM4549_P 
         aww_p(1,8,i) = k(1,8,1) * trend_te(i) + k(1,8,2) * ru_a(1,8,i) + k(1,8,3)
         aww_p(1,8,i) = aww_p(1,8,i) * awwm_adj(i)
         
         ! (89) WEM4549_3_P 
         we_3_p(1,8,i) = e_a(1,8,i) * 52d0 / aww_p(1,8,i)

         ! (90) AWWM4549_PL 
         aww_pl(1,8,i) = k(1,8,1) * trend_te(i-1) + k(1,8,2) * ru_a(1,8,i-1) + k(1,8,3)
         aww_pl(1,8,i) = aww_pl(1,8,i) * awwm_adj(i-1)
         
         ! (91) WEM4549_3_PL 
         we_3_pl(1,8,i) = e_a(1,8,i-1) * 52d0 / aww_pl(1,8,i)

         ! (92) TEM4549_P 
         te_a_p(1,8,i) = ((we_3_p(1,8,i) / we_3_pl(1,8,i)) * &
                          (te_a(1,8,i-1)-m_a(1,8,i-1)) + m_a(1,8,i)) * &
                           mult1_te(1,8,i) * mult2_te(1,8,i)

         ! (93) AWWM5054_P 
         aww_p(1,9,i) = k(1,9,1) * trend_te(i) + k(1,9,2) * ru_a(1,9,i) + k(1,9,3)
         aww_p(1,9,i) = aww_p(1,9,i) * awwm_adj(i)
         
         ! (94) WEM5054_3_P 
         we_3_p(1,9,i) = e_a(1,9,i) * 52d0 / aww_p(1,9,i)

         ! (95) AWWM5054_PL 
         aww_pl(1,9,i) = k(1,9,1) * trend_te(i-1) + k(1,9,2) * ru_a(1,9,i-1) + k(1,9,3)
         aww_pl(1,9,i) = aww_pl(1,9,i) * awwm_adj(i-1)
         
         ! (96) WEM5054_3_PL 
         we_3_pl(1,9,i) = e_a(1,9,i-1) * 52d0 / aww_pl(1,9,i)

         ! (97) TEM5054_P 
         te_a_p(1,9,i) = ((we_3_p(1,9,i) / we_3_pl(1,9,i)) * &
                          (te_a(1,9,i-1)-m_a(1,9,i-1)) + m_a(1,9,i)) * &
                           mult1_te(1,9,i) * mult2_te(1,9,i)

         ! (98) AWWM5559_P 
         aww_p(1,10,i) = k(1,10,1) * trend_te(i) + k(1,10,2) * ru_a(1,10,i) + k(1,10,3)
         aww_p(1,10,i) = aww_p(1,10,i) * awwm_adj(i)
         
         ! (99) WEM5559_3_P 
         we_3_p(1,10,i) = e_a(1,10,i) * 52d0 / aww_p(1,10,i)

         ! (100) AWWM5559_PL 
         aww_pl(1,10,i) = k(1,10,1) * trend_te(i-1) + k(1,10,2) * ru_a(1,10,i-1) + k(1,10,3)
         aww_pl(1,10,i) = aww_pl(1,10,i) * awwm_adj(i-1)
         
         ! (101) WEM5559_3_PL 
         we_3_pl(1,10,i) = e_a(1,10,i-1) * 52d0 / aww_pl(1,10,i)

         ! (102) TEM5559_P 
         te_a_p(1,10,i) = ((we_3_p(1,10,i) / we_3_pl(1,10,i)) * &
                          (te_a(1,10,i-1)-m_a(1,10,i-1)) + m_a(1,10,i)) * &
                           mult1_te(1,10,i) * mult2_te(1,10,i)

         ! (103) AWWM6064_P 
         aww_p(1,11,i) = k(1,11,1) * trend_te(i) + k(1,11,2) * ru_a(1,11,i) + k(1,11,3)
         aww_p(1,11,i) = aww_p(1,11,i) * awwm_adj(i)
         
         ! (104) WEM6064_3_P 
         we_3_p(1,11,i) = e_a(1,11,i) * 52d0 / aww_p(1,11,i)

         ! (105) AWWM6064_PL 
         aww_pl(1,11,i) = k(1,11,1) * trend_te(i-1) + k(1,11,2) * ru_a(1,11,i-1) + k(1,11,3)
         aww_pl(1,11,i) = aww_pl(1,11,i) * awwm_adj(i-1)
         
         ! (106) WEM6064_3_PL 
         we_3_pl(1,11,i) = e_a(1,11,i-1) * 52d0 / aww_pl(1,11,i)

         ! (107) TEM6064_P 
         te_a_p(1,11,i) = (we_3_p(1,11,i) / we_3_pl(1,11,i)) * &
                          (te_a(1,11,i-1)) * &
                           mult1_te(1,11,i) * mult2_te(1,11,i)

         ! (108) AWWM6569_P 
         aww_p(1,12,i) = k(1,12,1) * trend_te(i) + k(1,12,2) * ru_a(1,12,i) + k(1,12,3)
         aww_p(1,12,i) = aww_p(1,12,i) * awwm_adj(i)
         
         ! (109) WEM6569_3_P 
         we_3_p(1,12,i) = e_a(1,12,i) * 52d0 / aww_p(1,12,i)

         ! (110) AWWM6569_P 
         aww_pl(1,12,i) = k(1,12,1) * trend_te(i-1) + k(1,12,2) * ru_a(1,12,i-1) + k(1,12,3)
         aww_pl(1,12,i) = aww_pl(1,12,i) * awwm_adj(i-1)
         
         ! (111) WEM6569_3_PL 
         we_3_pl(1,12,i) = e_a(1,12,i-1) * 52d0 / aww_pl(1,12,i)

         ! (112) TEM6569_P 
         te_a_p(1,12,i) = (we_3_p(1,12,i) / we_3_pl(1,12,i)) * &
                          (te_a(1,12,i-1)) * &
                           mult1_te(1,12,i) * mult2_te(1,12,i)

         ! (113) AWWM70O_P 
         aww_p(1,23,i) = k(1,13,1) * trend_te(i) + k(1,13,2) * ru_a(1,15,i) + k(1,13,3) ! group indices 23, 13, and 15 are correct
         aww_p(1,23,i) = aww_p(1,23,i) * awwm_adj(i)
         
         ! (114) WEM70O_3_P 
         we_3_p(1,23,i) = e_a(1,23,i) * 52d0 / aww_p(1,23,i)

         ! (115) AWWM70O_PL 
         aww_pl(1,23,i) = k(1,13,1) * trend_te(i-1) + k(1,13,2) * ru_a(1,15,i-1) + k(1,13,3) ! group indices 23, 13, and 15 are correct
         aww_pl(1,23,i) = aww_pl(1,23,i) * awwm_adj(i-1)
         
         ! (116) WEM70O_3_PL 
         we_3_pl(1,23,i) = e_a(1,23,i-1) * 52d0 / aww_pl(1,23,i)

         ! (117) TEM70O_P 
         te_a_p(1,23,i) = (we_3_p(1,23,i) / we_3_pl(1,23,i)) * &
                          (te_a(1,23,i-1)) * &
                           mult1_te(1,23,i) * mult2_te(1,23,i)

         ! (118) TEM16O_P 
         te_a_p(1,24,i) = 0d0
         do ageGrp = 1, 12
            te_a_p(1,24,i) = te_a_p(1,24,i) + te_a_p(1,ageGrp,i)
         end do
         te_a_p(1,24,i) = te_a_p(1,24,i) + te_a_p(1,23,i)
         
         if (i >= histend+1) then

            ! (119) TEM16O
            te_a(1,24,i) = te_a_p(1,24,i)

            ! (120) TEM 
            te_a(1,0,i) = te_a(1,24,i) + he_m_15u(1,i)
            
         end if
         
         ! (121) AWWF1617_P 
         aww_p(2,1,i) = k(2,1,1) * trend_te(i) + k(2,1,2) * ru_a(2,1,i) + k(2,1,3)
         aww_p(2,1,i) = aww_p(2,1,i) * awwf_adj(i)
         
         ! (122) WEF1617_3_P 
         we_3_p(2,1,i) = e_a(2,1,i) * 52d0 / aww_p(2,1,i)

         ! (123) AWWF1617_PL 
         aww_pl(2,1,i) = k(2,1,1) * trend_te(i-1) + k(2,1,2) * ru_a(2,1,i-1) + k(2,1,3)
         aww_pl(2,1,i) = aww_pl(2,1,i) * awwf_adj(i-1)
         
         ! (124) WEF1617_3_PL 
          we_3_pl(2,1,i) = e_a(2,1,i-1) * 52d0 / aww_pl(2,1,i)

         ! (125) TEF1617_P 
         te_a_p(2,1,i) = ((we_3_p(2,1,i) / we_3_pl(2,1,i)) * &
                          (te_a(2,1,i-1)-m_a(2,1,i-1)) + m_a(2,1,i)) * &
                           mult1_te(2,1,i) * mult2_te(2,1,i)
         ! (126) AWWF1819_P
          aww_p(2,2,i) = k(2,2,1) * trend_te(i) + k(2,2,2) * ru_a(2,2,i) + k(2,2,3)
          aww_p(2,2,i) = aww_p(2,2,i) * awwf_adj(i)
          
         ! (127) WEF1819_3_P 
         we_3_p(2,2,i) = e_a(2,2,i) * 52d0 / aww_p(2,2,i)

         ! (128) AWWF1819_PL 
         aww_pl(2,2,i) = k(2,2,1) * trend_te(i-1) + k(2,2,2) * ru_a(2,2,i-1) + k(2,2,3)
         aww_pl(2,2,i) = aww_pl(2,2,i) * awwf_adj(i-1)
         
         ! (129) WEF1819_3_PL 
         we_3_pl(2,2,i) = e_a(2,2,i-1) * 52d0 / aww_pl(2,2,i)

         ! (130) TEF1819_P 
         te_a_p(2,2,i) = ((we_3_p(2,2,i) / we_3_pl(2,2,i)) * &
                          (te_a(2,2,i-1)-m_a(2,2,i-1)) + m_a(2,2,i)) * &
                           mult1_te(2,2,i) * mult2_te(2,2,i)

         ! (131) AWWF2024_P 
         aww_p(2,3,i) = k(2,3,1) * trend_te(i) + k(2,3,2) * ru_a(2,3,i) + k(2,3,3)
         aww_p(2,3,i) = aww_p(2,3,i) * awwf_adj(i)
         
         ! (132) WEF2024_3_P 
         we_3_p(2,3,i) = e_a(2,3,i) * 52d0 / aww_p(2,3,i)

         ! (133) AWWF2024_PL 
         aww_pl(2,3,i) = k(2,3,1) * trend_te(i-1) + k(2,3,2) * ru_a(2,3,i-1) + k(2,3,3)
         aww_pl(2,3,i) = aww_pl(2,3,i) * awwf_adj(i-1)
         
         ! (134) WEF2024_3_PL 
         we_3_pl(2,3,i) = e_a(2,3,i-1) * 52d0 / aww_pl(2,3,i)

         ! (135) TEF2024_P 
         te_a_p(2,3,i) = ((we_3_p(2,3,i) / we_3_pl(2,3,i)) * &
                          (te_a(2,3,i-1)-m_a(2,3,i-1)) + m_a(2,3,i)) * &
                           mult1_te(2,3,i) * mult2_te(2,3,i)

         ! (136) AWWF2529_P 
         aww_p(2,4,i) = k(2,4,1) * trend_te(i) + k(2,4,2) * ru_a(2,4,i) + k(2,4,3)
         aww_p(2,4,i) = aww_p(2,4,i) * awwf_adj(i)
         
         ! (137) WEF529_3_P 
         we_3_p(2,4,i) = e_a(2,4,i) * 52d0 / aww_p(2,4,i)

         ! (138) AWWF2529_PL 
         aww_pl(2,4,i) = k(2,4,1) * trend_te(i-1) + k(2,4,2) * ru_a(2,4,i-1) + k(2,4,3)
         aww_pl(2,4,i) = aww_pl(2,4,i) * awwf_adj(i-1)
         
         ! (139) WEF2529_3_PL 
         we_3_pl(2,4,i) = e_a(2,4,i-1) * 52d0 / aww_pl(2,4,i)

         ! (140) TEF2529_P 
         te_a_p(2,4,i) = ((we_3_p(2,4,i) / we_3_pl(2,4,i)) * &
                          (te_a(2,4,i-1)-m_a(2,4,i-1)) + m_a(2,4,i)) * &
                           mult1_te(2,4,i) * mult2_te(2,4,i)

         ! (141) AWWF3034_P 
         aww_p(2,5,i) = k(2,5,1) * trend_te(i) + k(2,5,2) * ru_a(2,5,i) + k(2,5,3)
         aww_p(2,5,i) = aww_p(2,5,i) * awwf_adj(i)
         
         ! (142) WEF2529_3_P 
         we_3_p(2,5,i) = e_a(2,5,i) * 52d0 / aww_p(2,5,i)

         ! (143) AWWF3034_PL 
         aww_pl(2,5,i) = k(2,5,1) * trend_te(i-1) + k(2,5,2) * ru_a(2,5,i-1) + k(2,5,3)
         aww_pl(2,5,i) = aww_pl(2,5,i) * awwf_adj(i-1)
         
         ! (144) WEF3034_3_PL 
         we_3_pl(2,5,i) = e_a(2,5,i-1) * 52d0 / aww_pl(2,5,i)

         ! (145) TEF3034_P 
         te_a_p(2,5,i) = ((we_3_p(2,5,i) / we_3_pl(2,5,i)) * &
                          (te_a(2,5,i-1)-m_a(2,5,i-1)) + m_a(2,5,i)) * &
                           mult1_te(2,5,i) * mult2_te(2,5,i)

         ! (146) AWWF3539_P 
         aww_p(2,6,i) = k(2,6,1) * trend_te(i) + k(2,6,2) * ru_a(2,6,i) + k(2,6,3)
         aww_p(2,6,i) = aww_p(2,6,i) * awwf_adj(i)
         
         ! (147) WEF3539_3_P 
         we_3_p(2,6,i) = e_a(2,6,i) * 52d0 / aww_p(2,6,i)

         ! (148) AWWF3539_PL 
         aww_pl(2,6,i) = k(2,6,1) * trend_te(i-1) + k(2,6,2) * ru_a(2,6,i-1) + k(2,6,3)
         aww_pl(2,6,i) = aww_pl(2,6,i) * awwf_adj(i-1)
         
         ! (149) WEF3539_3_PL 
         we_3_pl(2,6,i) = e_a(2,6,i-1) * 52d0 / aww_pl(2,6,i)

         ! (150) TEF3539_P 
         te_a_p(2,6,i) = ((we_3_p(2,6,i) / we_3_pl(2,6,i)) * &
                          (te_a(2,6,i-1)-m_a(2,6,i-1)) + m_a(2,6,i)) * &
                           mult1_te(2,6,i) * mult2_te(2,6,i)

         ! (151) AWWF4044_P 
         aww_p(2,7,i) = k(2,7,1) * trend_te(i) + k(2,7,2) * ru_a(2,7,i) + k(2,7,3)
         aww_p(2,7,i) = aww_p(2,7,i) * awwf_adj(i)
         
         ! (152) WEF4044_3_P 
         we_3_p(2,7,i) = e_a(2,7,i) * 52d0 / aww_p(2,7,i)

         ! (153) AWWF4044_PL 
         aww_pl(2,7,i) = k(2,7,1) * trend_te(i-1) + k(2,7,2) * ru_a(2,7,i-1) + k(2,7,3)
         aww_pl(2,7,i) = aww_pl(2,7,i) * awwf_adj(i-1)
         
         ! (154) WEF4044_3_PL 
         we_3_pl(2,7,i) = e_a(2,7,i-1) * 52d0 / aww_pl(2,7,i)

         ! (155) TEF4044_P 
         te_a_p(2,7,i) = ((we_3_p(2,7,i) / we_3_pl(2,7,i)) * &
                          (te_a(2,7,i-1)-m_a(2,7,i-1)) + m_a(2,7,i)) * &
                           mult1_te(2,7,i) * mult2_te(2,7,i)

         ! (156) AWWF4549_P 
         aww_p(2,8,i) = k(2,8,1) * trend_te(i) + k(2,8,2) * ru_a(2,8,i) + k(2,8,3)
         aww_p(2,8,i) = aww_p(2,8,i) * awwf_adj(i)
         
         ! (157) WEF4549_3_P 
         we_3_p(2,8,i) = e_a(2,8,i) * 52d0 / aww_p(2,8,i)

         ! (158) AWWF4549_PL 
         aww_pl(2,8,i) = k(2,8,1) * trend_te(i-1) + k(2,8,2) * ru_a(2,8,i-1) + k(2,8,3)
         aww_pl(2,8,i) = aww_pl(2,8,i) * awwf_adj(i-1)
         
         ! (159) WEF4549_3_PL 
         we_3_pl(2,8,i) = e_a(2,8,i-1) * 52d0 / aww_pl(2,8,i)

         ! (160) TEF4549_P 
         te_a_p(2,8,i) = ((we_3_p(2,8,i) / we_3_pl(2,8,i)) * &
                          (te_a(2,8,i-1)-m_a(2,8,i-1)) + m_a(2,8,i)) * &
                           mult1_te(2,8,i) * mult2_te(2,8,i)

         ! (161) AWWF5054_P 
         aww_p(2,9,i) = k(2,9,1) * trend_te(i) + k(2,9,2) * ru_a(2,9,i) + k(2,9,3)
         aww_p(2,9,i) = aww_p(2,9,i) * awwf_adj(i)
         
         ! (162) WEF5054_3_P 
         we_3_p(2,9,i) = e_a(2,9,i) * 52d0 / aww_p(2,9,i)

         ! (163) AWWF5054_PL 
         aww_pl(2,9,i) = k(2,9,1) * trend_te(i-1) + k(2,9,2) * ru_a(2,9,i-1) + k(2,9,3)
         aww_pl(2,9,i) = aww_pl(2,9,i) * awwf_adj(i-1)
         
         ! (164) WEF5054_3_PL 
         we_3_pl(2,9,i) = e_a(2,9,i-1) * 52d0 / aww_pl(2,9,i)

         ! (165) TEF5054_P 
         te_a_p(2,9,i) = ((we_3_p(2,9,i) / we_3_pl(2,9,i)) * &
                          (te_a(2,9,i-1)-m_a(2,9,i-1)) + m_a(2,9,i)) * &
                           mult1_te(2,9,i) * mult2_te(2,9,i)

         ! (166) AWWF5559_P 
         aww_p(2,10,i) = k(2,10,1) * trend_te(i) + k(2,10,2) * ru_a(2,10,i) + k(2,10,3)
         aww_p(2,10,i) = aww_p(2,10,i) * awwf_adj(i)
         
         ! (167) WEF5559_3_P 
         we_3_p(2,10,i) = e_a(2,10,i) * 52d0 / aww_p(2,10,i)

         ! (168) AWWF5559_PL 
         aww_pl(2,10,i) = k(2,10,1) * trend_te(i-1) + k(2,10,2) * ru_a(2,10,i-1) + k(2,10,3)
         aww_pl(2,10,i) = aww_pl(2,10,i) * awwf_adj(i-1)
         
         ! (169) WEF5559_3_PL 
         we_3_pl(2,10,i) = e_a(2,10,i-1) * 52d0 / aww_pl(2,10,i)

         ! (170) TEF5559_P 
         te_a_p(2,10,i) = ((we_3_p(2,10,i) / we_3_pl(2,10,i)) * &
                          (te_a(2,10,i-1)-m_a(2,10,i-1)) + m_a(2,10,i)) * &
                           mult1_te(2,10,i) * mult2_te(2,10,i)

         ! (171) AWWF6064_P 
         aww_p(2,11,i) = k(2,11,1) * trend_te(i) + k(2,11,2) * ru_a(2,11,i) + k(2,11,3)
         aww_p(2,11,i) = aww_p(2,11,i) * awwf_adj(i)
         
         ! (172) WEF6064_3_P 
         we_3_p(2,11,i) = e_a(2,11,i) * 52d0 / aww_p(2,11,i)

         ! (173) AWWF6064_PL 
         aww_pl(2,11,i) = k(2,11,1) * trend_te(i-1) + k(2,11,2) * ru_a(2,11,i-1) + k(2,11,3)
         aww_pl(2,11,i) = aww_pl(2,11,i) * awwf_adj(i-1)
         
         ! (174) WEF6064_3_PL 
         we_3_pl(2,11,i) = e_a(2,11,i-1) * 52d0 / aww_pl(2,11,i)

         ! (175) TEF6064_P 
         te_a_p(2,11,i) = (we_3_p(2,11,i) / we_3_pl(2,11,i)) * &
                          (te_a(2,11,i-1)) * &
                           mult1_te(2,11,i) * mult2_te(2,11,i)

         ! (176) AWWF6569_P 
         aww_p(2,12,i) = k(2,12,1) * trend_te(i) + k(2,12,2) * ru_a(2,12,i) + k(2,12,3)
         aww_p(2,12,i) = aww_p(2,12,i) * awwf_adj(i)
         
         ! (177) WEF6569_3_P 
         we_3_p(2,12,i) = e_a(2,12,i) * 52d0 / aww_p(2,12,i)

         ! (178) AWWF6569_P 
         aww_pl(2,12,i) = k(2,12,1) * trend_te(i-1) + k(2,12,2) * ru_a(2,12,i-1) + k(2,12,3)
         aww_pl(2,12,i) = aww_pl(2,12,i) * awwf_adj(i-1)
         
         ! (179) WEF6569_3_PL 
         we_3_pl(2,12,i) = e_a(2,12,i-1) * 52d0 / aww_pl(2,12,i)

         ! (180) TEF6569_P 
         te_a_p(2,12,i) = (we_3_p(2,12,i) / we_3_pl(2,12,i)) * &
                          (te_a(2,12,i-1)) * &
                           mult1_te(2,12,i) * mult2_te(2,12,i)

         ! (181) AWWF70O_P 
         aww_p(2,23,i) = k(2,13,1) * trend_te(i) + k(2,13,2) * ru_a(2,15,i) + k(2,13,3) ! group indices 23, 13, and 15 are correct
         aww_p(2,23,i) = aww_p(2,23,i) * awwf_adj(i)
         
         ! (182) WEF70O_3_P 
         we_3_p(2,23,i) = e_a(2,23,i) * 52d0 / aww_p(2,23,i)

         ! (183) AWWF70O_PL 
         aww_pl(2,23,i) = k(2,13,1) * trend_te(i-1) + k(2,13,2) * ru_a(2,15,i-1) + k(2,13,3) ! group indices 23, 13, and 15 are correct
         aww_pl(2,23,i) = aww_pl(2,23,i) * awwf_adj(i-1)
         
         ! (184) WEF70O_3_PL 
         we_3_pl(2,23,i) = e_a(2,23,i-1) * 52d0 / aww_pl(2,23,i)

         ! (185) TEF70O_P 
         te_a_p(2,23,i) = (we_3_p(2,23,i) / we_3_pl(2,23,i)) * &
                          (te_a(2,23,i-1)) * &
                           mult1_te(2,23,i) * mult2_te(2,23,i)

         ! (186) TEF16O_P 
         te_a_p(2,24,i) = 0d0
         do ageGrp = 1, 12
            te_a_p(2,24,i) = te_a_p(2,24,i) + te_a_p(2,ageGrp,i)
         end do
         te_a_p(2,24,i) = te_a_p(2,24,i) + te_a_p(2,23,i)

         if (i >= histend+1) then
            
            ! (187) TEF16O
            te_a(2,24,i) = te_a_p(2,24,i)

            ! (188) TEF 
            te_a(2,0,i) = te_a(2,24,i) + he_m_15u(2,i)
         
            ! (189) TE 
            te_a(0,0,i) = te_a(1,0,i) + te_a(2,0,i)
            
         end if

         ! (190) WEM16O_3_P 
         we_3_p(1,24,i) = we_3_p(1,1,i)
         do ageGrp = 2, 12
            we_3_p(1,24,i) = we_3_p(1,24,i) + we_3_p(1,ageGrp,i)
         end do
         we_3_p(1,24,i) = we_3_p(1,24,i) + we_3_p(1,23,i)

         ! (191) AWWM16O_P 
         aww_p(1,24,i) = e_a(1,24,i) * 52d0 / we_3_p(1,24,i)

         if (i > lastDataYr-1) then

            ! (192) - (230)
            do ageGrp = 1, 12
               aww(1,ageGrp,i) = aww_p(1,ageGrp,i)
               we_3(1,ageGrp,i) = we_3_p(1,ageGrp,i)
               if (i > lastDataYr-1) te_a(1,ageGrp,i) = te_a_p(1,ageGrp,i)
            end do
            if (i > lastDataYr-1) then
               aww(1,23,i) = aww_p(1,23,i)
               we_3(1,23,i) = we_3_p(1,23,i)
               te_a(1,23,i) = te_a_p(1,23,i)
            end if

            ! (231) WEM16O_3 
            we_3(1,24,i) = we_3_p(1,24,i)

            ! (232) AWWM16O
            aww(1,24,i) = aww_p(1,24,i)

         end if

         ! (233) WEF16O_3_P
         we_3_p(2,24,i) = we_3_p(2,1,i) 
         do ageGrp = 2, 12
            we_3_p(2,24,i) = we_3_p(2,24,i) + we_3_p(2,ageGrp,i)
         end do
         we_3_p(2,24,i) = we_3_p(2,24,i) + we_3_p(2,23,i)

         ! (234) AWWF16O_P 
         aww_p(2,24,i) = e_a(2,24,i) * 52d0 / we_3_p(2,24,i)

         if (i > lastDataYr-1) then
        
            ! (235) - (273)
            do ageGrp = 1, 12
               aww(2,ageGrp,i) = aww_p(2,ageGrp,i)
                we_3(2,ageGrp,i) = we_3_p(2,ageGrp,i)
               if (i > lastDataYr-1) te_a(2,ageGrp,i) = te_a_p(2,ageGrp,i)
            end do
            if (i > lastDataYr-1) then
               aww(2,23,i) = aww_p(2,23,i)
               we_3(2,23,i) = we_3_p(2,23,i)
               te_a(2,23,i) = te_a_p(2,23,i)
            end if

            ! (274) WEF16O_3 
            we_3(2,24,i) = we_3_p(2,24,i)

            ! (275) AWWF16O
            aww(2,24,i) = aww_p(2,24,i)

         end if

         ! (276) WE16O_3_P 
         we_3_p(0,24,i) = we_3_p(1,24,i) + we_3_p(2,24,i)

         ! (277) AWW16O_P 
         aww_p(0,24,i) = e_a(0,24,i) * 52d0 / we_3_p(0,24,i)

         ! (278) WE16O_3
         we_3(0,24,i) = we_3_p(0,24,i)

         if (i > lastDataYr-1) then

            ! (279) AWW16O 
            aww(0,24,i) = aww_p(0,24,i)

         end if

      end do

   end subroutine SolveAverageWeeksTotalEmployment

!===============================================================================

   subroutine SolveEmploymentAtAnyTime()

      integer :: i
      
      ! Equations 280-401

      do i = startYr, endYr

         if (i>=histend+1) then

            ! (280) TESL 
            tesl_a(i) = (tesl_a(i-1) / eggeslmax_a(i-1)) * eggeslmax_a(i)
            
            ! (281) TESL_O 
            tesl_o_a(i) = (tesl_o_a(i-1) / tesl_a(i-1)) * tesl_a(i)

            ! (282) TESL_N 
            tesl_n_a(i) = (tesl_a(i) - tesl_o_a(i))
            
            ! (283) TESL_N_N_NHI_S 
            tesl_n_n_nhi_s_a(i) = te_slos_m(0,0,i) / 0.99d0
            
            ! (284) TESL_N_O_NHI_S 
            tesl_n_o_nhi_s_a(i) = tesl_n_o_nhi_s_a(i-1) * (tesl_n_n_nhi_s_a(i) / &
               tesl_n_n_nhi_s_a(i-1))

            ! (285) TESL_N_E 
            tesl_n_e_a(i) = tesl_n_e_a(i-1) * (tesl_a(i) / tesl_a(i-1))
            
            ! (286) TESL_N_O_NHI_E 
            tesl_n_o_nhi_e_a(i) = tesl_n_e_a(i) * 0.6d0
            
            ! (287) TESL_N_O_NHI_NS 
            tesl_n_o_nhi_ns_a(i) = tesl_n_n_nhi_ns_a(i-1) * 0.373d0

            ! (288) TESL_N_O_NHI 
            tesl_n_o_nhi_a(i) = (tesl_n_o_nhi_s_a(i) + tesl_n_o_nhi_e_a(i) + &
               tesl_n_o_nhi_ns_a(i))
            
            ! (289) TESL_N_N_NHI_E  
            tesl_n_n_nhi_e_a(i) = (tesl_n_e_a(i) - tesl_n_o_nhi_e_a(i))            
            
            ! (290) TESL_N_N_NHI_NS 
            tesl_n_n_nhi_ns_a(i) = te_sloo_m(0,0,i)
            
            ! (291) TESL_N_N_NHI 
            tesl_n_n_nhi_a(i) = (tesl_n_n_nhi_s_a(i) + tesl_n_n_nhi_e_a(i) + &
               tesl_n_n_nhi_ns_a(i))
         
            ! (292) TESL_N_O_HI 
            tesl_n_o_hi_a(i) = (tesl_n_a(i) - tesl_n_o_nhi_a(i) - &
               tesl_n_n_nhi_a(i)) * cer_mqge_o_a(i)

            ! (293) TESL_N_O 
            tesl_n_o_a(i) = (tesl_n_o_hi_a(i) + tesl_n_o_nhi_a(i))

            ! (294) TESL_N_N 
            tesl_n_n_a(i) = (tesl_n_a(i) - tesl_n_o_a(i))

            ! (295) TESL_N_N_HI 
            tesl_n_n_hi_a(i) = (tesl_n_n_a(i) - tesl_n_n_nhi_a(i))
            
            ! (296) WSW_HIO_OTH 
            wsw_hio_oth_a(i) = 0d0 ! (0.2537d0 / 3.6495d0) * tesl_n_n_hi_a(i)
            
            ! (297) WESL 
            wesl_a(i) = (wesl_a(i-1) / wsggesl_a(i-1)) * wsggesl_a(i)
                   
            ! (298) WESL_O 
            wesl_o_a(i) = (wesl_o_a(i-1) / wsggesl_a(i-1)) * wsggesl_a(i)
           
            ! (299) WESL_N 
            wesl_n_a(i) = (wesl_a(i) - wesl_o_a(i))
         
            ! (300) WSCA_HIO_OTH 
            wsca_hio_oth_a(i) = wsw_hio_oth_a(i)*(wesl_n_a(i)/tesl_n_a(i))
         
            ! (301) WSW_HIO_OTH_SE 
            wsw_hio_oth_se_a(i) = 0d0 ! (wsw_hio_oth_se_a(i-1) / wsw_hio_oth_a(i-1)) * wsw_hio_oth_a(i)
            
            ! (302) TEFC_N_N_SE 
            tefc_n_n_se_a(i) = he_wosf_m(0,0,i)
            
            ! (303) TESL_N_N_HI_SE 
            tesl_n_n_hi_se_a(i) = (tesl_n_n_hi_se_a(i-1) / tesl_n_n_hi_a(i-1)) * tesl_n_n_hi_a(i)
            
            ! (304) HE_WOL_M
            he_wol_m(0,0,i) = tesl_n_n_hi_a(i)
            
            ! (305) HE_WOR_M 
            he_wor_m(0,0,i) = 0d0 ! (0.2537d0 / 3.6495d0) * he_wol_m(0,0,i)
            
            ! (306) HE_WOSL_M
            he_wosl_m(0,0,i) = (he_wosl_m(0,0,i-1) / he_wol_m(0,0,i-1)) * he_wol_m(0,0,i)
            
            ! (307) HE_WOSR_M 
            he_wosr_m(0,0,i) = 0d0 ! (he_wosr_m(0,0,i-1) / he_wor_m(0,0,i-1)) * he_wor_m(0,0,i)
            
         end if
         
         ! (308) SEOCMB 
         seocmb_a(i) = wsw_hio_oth_se_a(i) + tefc_n_n_se_a(i) + tesl_n_n_hi_se_a(i)

         ! (309) SEOCMBL1 
         seocmbl1_a(i) = wsw_hio_oth_se_a(i-1) + tefc_n_n_se_a(i-1) + tesl_n_n_hi_se_a(i-1)

         if (i>=histend+1) then
         
            ! (310) SEO 
            seo_a(0,0,i) = (seo_a(0,0,i-1) * (eas_a(0,0,i) + enas_a(0,0,i)) / &
               (eas_a(0,0,i-1) + enas_a(0,0,i-1)) + (seocmb_a(i) - &
               seocmbl1_a(i))) * multseo_a(i)

           ! (311) SEO_HI 
           seo_hi_a(i) = seo_a(0,0,i) - seocmb_a(i)
           
         
            ! (312) TE_U 
            te_u(0,0,i) = teo_und(0,0,i)
         
            ! (313) TEL_SO 
            tel_so(0,0,i) = tel_so(0,0,i-1) * (te_a(0,0,i) - teo_und(0,0,i) - teo_esf(0,0,i)) / &
               (te_a(0,0,i-1) - teo_und(0,0,i-1) - teo_esf(0,0,i-1))
         
            ! (314) TE_S 
            te_s(0,0,i) = tel_so(0,0,i) + teo_esf(0,0,i)
         
            ! (315) TE_M 
            te_m(0,0,i) = te_a(0,0,i) - te_u(0,0,i) - te_s(0,0,i)
         
            ! (316) TE_MN 
            te_mn(0,0,i) = te_rro_m(0,0,i) + te_sloo_m(0,0,i) + te_slos_m(0,0,i) + te_sloe_m(0,0,i) + &
               te_ps_m(0,0,i) + te_ph_m(0,0,i) + teo_asf1(0,0,i) + teo_asj1(0,0,i) + teo_awj(0,0,i) + teo_awh(0,0,i)
         
            ! (317) HE_M 
            he_m(0,0,i) = te_m(0,0,i) - te_mn(0,0,i)

            ! (318) TCEAHI 
            tceahi_a(i) = he_m(0,0,i) + te_s(0,0,i)
            
            ! (319) TEFC_N_N 
            tefc_n_n_a(i) = he_wof_m(0,0,i)
            
            ! (320) TCEA 
            tcea_a(i) = tceahi_a(i) - &
               ((tesl_n_n_hi_a(i) + tefc_n_n_a(i) + wsw_hio_oth_a(i)) - &
                (tesl_n_n_hi_se_a(i) + tefc_n_n_se_a(i) + wsw_hio_oth_se_a(i)))
         
            ! (321) WSWA 
            wswa_a(i) = (tcea_a(i) - seo_a(0,0,i))
      
            ! (322) CMB_TOT 
            cmb_tot_a(i) = ((-.01468 + 0.06227d0 * rtp_a(i-1) - 0.0008d0) * &
               wswa_a(i) - seocmb_a(i)) * multcmb_a(i)
        
         end if
         
        ! (323) WSPH_O 
         wsph_o_a(i) = cph_a(i) * wsph_a(i)
        
         if (i>=histend+1) then
         
            ! (324) WSPF_O 
            wspf_o_a(i) = wspf_o_a(i-1) * wspf_a(i) / wspf_a(i-1)
           
         end if

         ! (325) WSPRR_O 
         wsprr_o_a(i) = cprr_a(i) * wsprrb_a(i)
        
         if (i>=histend+2) then
         
            ! (326) TIPS_SR 
            tips_sr_a(i) = (0.000508328d0 * rtp_a(i) - 0.000481700d0) * &
               gdp_a(i) * 1.26393d0 + tips_sr_add_a(i)
            
         end if
         
         if (i >= histend+1) then
           
            ! (327) WSDPB 
            wsdpb_a(i) = wsdp_a(i) - wsph_a(i) - wspf_a(i) - wsprrb_a(i) - &
               tips_sr_a(i)
           
            ! (328) WSPB_O 
            wspb_o_a(i) = cpb_a(i) * wsdpb_a(i)

            ! (329) WSPC 
            wspc_a(i) = wsph_o_a(i) + wspf_o_a(i) + wsprr_o_a(i) + tips_sr_a(i) + &
            wspb_o_a(i)
                    
            ! (330) WSGSLCA 
            wsgslca_a(i) = wesl_o_a(i)
         
            ! (331) WEFC 
            wefc_a(i) = (wefc_a(i-1) / wsggefc_a(i-1)) * wsggefc_a(i)
           
            ! (332) WEFC_O 
            wefc_o_a(i) = (wefc_a(i) - wefc_n_a(i)) * adj_fsa_fc_a(i)
        
            ! (333) WSGFCA 
            wsgfca_a(i) = wefc_o_a(i)
           
            ! (334) WSGMLC 
            wsgmlc_a(i) = cml_a(i) * wsgfm_a(i)
           
            ! (335) WSCA 
            wsca_a(i) = (wspc_a(i) + wsgslca_a(i) + wsgfca_a(i) + wsgmlc_a(i))
           
            ! (336) ACWA 
            acwa_a(i) = wsca_a(i) / wswa_a(i)
           
            ! (337) AW_CMBTOT 
           aw_cmbtot_a(i) = 1.4953d0 * acwa_a(i)
           
            ! (338) W_CMBTOT 
            w_cmbtot_a(i) = aw_cmbtot_a(i) * cmb_tot_a(i)
         
         end if
        
         ! (339) RAIW 
         raiw_a(i) = aiw_a(i-2) / aiwbase_a(i)
        
         ! (340) TAXMAXB1 
         taxmaxb1_a(i) = raiw_a(i) * tmaxbase_a(i) * 1000.0d0 / 300.0d0
        
         ! (341) TAXMAXB2
         if (taxmaxb1_a(i) - nint(taxmaxb1_a(i)) >= 0.5d0) then
            taxmaxb2_a(i) =  nint(taxmaxb1_a(i)) + 1.0d0 
         else 
            taxmaxb2_a(i) = nint(taxmaxb1_a(i))
         end if
        
         ! (342) TAXMAXB3 
         if (taxmaxb2_a(i) < taxmax_a(i-1)) then 
            taxmaxb3_a(i) = taxmax_a(i-1) * 1000.0d0 / 300.0d0 
         else 
            taxmaxb3_a(i) = taxmaxb2_a(i)
         end if
                  
         ! (343) TAXMAX 
         if (beninc_a(i-1)<=0.001d0) then
            taxmax_a(i) =  taxmax_a(i-1) 
         else if (raiw_a(i) <= raiw_a(i-1)) then
            taxmax_a(i) =  taxmax_a(i-1)
         else
            taxmax_a(i) =  300.0d0 * taxmaxb3_a(i) / 1000.0d0
         end if
       
         ! (344) CMB_WRELMAX 
         cmb_wrelmax_a(i) = taxmax_a(i) / aw_cmbtot_a(i)
            
         ! (345) CMB_WAO1 
         if (cmb_wrelmax_a(i) < 0.0543009d0) then 
            cmb_wao1_a(i) = 1.0d0 - 0.722659d0 * cmb_wrelmax_a(i)**0.65d0 - &
               0.461913d0 * cmb_wrelmax_a(i)**0.8d0 
         else if (cmb_wrelmax_a(i) < 0.1086018d0) then  
            cmb_wao1_a(i) = -1.02884d0 * cmb_wrelmax_a(i)**0.6d0 + &
               0.324761d0 * cmb_wrelmax_a(i)**1.6d0 + 1.02015d0 
         else if (cmb_wrelmax_a(i) < 0.1629027d0) then  
            cmb_wao1_a(i) = -0.906607d0 * cmb_wrelmax_a(i)**0.7d0 + 0.947662d0 
         else if (cmb_wrelmax_a(i) < 0.2172037d0) then  
            cmb_wao1_a(i) = -0.813951d0 * cmb_wrelmax_a(i)**0.55d0 + 0.991722d0 
         else if (cmb_wrelmax_a(i) < 0.3258055d0) then  
            cmb_wao1_a(i) = -0.755135d0 * cmb_wrelmax_a(i)**0.55d0 + 0.964593d0 
         else 
            cmb_wao1_a(i) = 0.0d0
         end if
        
         ! (346) CMB_WAO2 
         if (cmb_wrelmax_a(i) < 0.5430091d0) then 
            cmb_wao2_a(i) = -0.649755d0 * cmb_wrelmax_a(i)**0.6d0 + 0.886467d0 
         else if (cmb_wrelmax_a(i) < 0.7059119d0) then  
            cmb_wao2_a(i) = -0.573205 * cmb_wrelmax_a(i)**0.7d0 + 0.810122d0 
         else if (cmb_wrelmax_a(i) < 0.9231155d0) then  
            cmb_wao2_a(i) = -5.22264d0 * cmb_wrelmax_a(i)**0.06d0 + 5.47514d0 
         else if (cmb_wrelmax_a(i) < 1.0860183d0) then  
            cmb_wao2_a(i) = -2.02619d0 * cmb_wrelmax_a(i)**0.15d0 + 2.27963d0 
         else if (cmb_wrelmax_a(i) < 1.5204256d0) then 
            cmb_wao2_a(i) = 0.605192d0 * exp(-0.2 * cmb_wrelmax_a(i)) - &
               0.827158d0 * exp(-0.8d0 * cmb_wrelmax_a(i)) + &
               1.52918d0 * exp(-1.5d0 * cmb_wrelmax_a(i)) - 0.212269d0 
         else 
            cmb_wao2_a(i) = 0.0d0
         end if
        
         ! (347) CMB_WAO3 
         if (cmb_wrelmax_a(i) < 1.8462311d0) then
            cmb_wao3_a(i) = 0.19139d0 * exp(-0.6d0 * cmb_wrelmax_a(i)) + &
               0.764408d0 * exp(-1.8d0 * cmb_wrelmax_a(i)) + 0.0194903d0 
         else if (cmb_wrelmax_a(i) < 2.3077888d0) then 
            cmb_wao3_a(i) = 0.12964d0 * exp(-0.5d0 * cmb_wrelmax_a(i)) + &
               0.644861d0 * exp(-1.5d0 * cmb_wrelmax_a(i)) + 0.0183343d0 
         else if (cmb_wrelmax_a(i) < 2.9865502d0) then 
            cmb_wao3_a(i) = 0.361318d0 * exp(-0.8d0 * cmb_wrelmax_a(i)) + &
               0.0219491d0 
         else if (cmb_wrelmax_a(i) < 4.3440731d0) then 
            cmb_wao3_a(i) = 0.193202d0 * exp(-0.45d0 * cmb_wrelmax_a(i)) + &
               0.00425171d0 
         else if (cmb_wrelmax_a(i) < 5.4300913d0) then 
            cmb_wao3_a(i) = 0.0560412d0 * exp(-0.25d0 * cmb_wrelmax_a(i)) + &
               0.311286d0 * exp(-0.8d0 * cmb_wrelmax_a(i)) + 0.00297316d0 
         else 
            cmb_wao3_a(i) = 0.0d0
         end if
        
         ! (348) CMB_WAO4
         if (cmb_wrelmax_a(i) < 13.5752283d0) then
            cmb_wao4_a(i) = 0.0995677d0 * exp(-0.32d0 * cmb_wrelmax_a(i)) + &
               0.00355234d0 
         else if (cmb_wrelmax_a(i) < 21.7203653d0) then 
            cmb_wao4_a(i) = 0.041159d0 * exp(-0.19d0 * cmb_wrelmax_a(i)) + &
               0.00156765d0 
         else if (cmb_wrelmax_a(i) < 678.7614168d0) then 
            cmb_wao4_a(i) = 0.265022d0 * cmb_wrelmax_a(i)**(-1.555d0) 
         else 
            cmb_wao4_a(i) = 0.0d0
         end if
        
         ! (349) CMB_WAO 
         if (cmb_wrelmax_a(i) < 0.3258055d0) then
            cmb_wao_a(i) = cmb_wao1_a(i) 
         else if (cmb_wrelmax_a(i) < 1.5204256d0) then 
            cmb_wao_a(i) = cmb_wao2_a(i) 
         else if (cmb_wrelmax_a(i) < 5.4300913d0) then 
            cmb_wao_a(i) = cmb_wao3_a(i) 
         else 
            cmb_wao_a(i) = cmb_wao4_a(i)
         end if
        
         if (i>=histend+1) then
         
            ! (350) CMB 
            cmb_a(i) = (1.0d0 - (cmb_wao_a(i) - 0.019d0)) * cmb_tot_a(i)
        
            ! (351) CMB_HI 
            cmb_hi_a(i) = cmb_tot_a(i) + seocmb_a(i)
         
         end if         
        
         ! (352) CSW_TOT 
         csw_tot_a(i) = seo_a(0,0,i) + cmb_tot_a(i)
        
         if (i>=histend+1) then
            
            ! (353) CSW 
            csw_a(i) = seo_a(0,0,i) + cmb_a(i)
           
         end if
        
         ! (354) CSW_HI 
         csw_hi_a(i) = seo_hi_a(i) + cmb_hi_a(i)
         
         if (i>=histend+1) then
            
            ! (355) TESL_N_S 
            tesl_n_s_a(i) = tesl_n_n_nhi_s_a(i) + tesl_n_o_nhi_s_a(i)
            
         end if
         
         if (i>=histend+1) then

            ! (356) WESL_N_NHI_S 
            wesl_n_nhi_s_a(i) = wesl_n_nhi_s_a(i-1) * (tesl_n_s_a(i) / &
               tesl_n_s_a(i-1)) * (awsggesl_a(i) / awsggesl_a(i-1))
           
            ! (357) WESL_N_NHI_E 
            wesl_n_nhi_e_a(i) = wesl_n_nhi_e_a(i-1) * (tesl_n_e_a(i) / &
               tesl_n_e_a(i-1)) * (awsggesl_a(i) / awsggesl_a(i-1))
                   
         end if
        
         ! (358) RAWR_NS
         if (awr_ns_a(i)==0) then 
            rawr_ns_a(i) = 0.0d0 
         else 
            rawr_ns_a(i) = awr_ns_a(i) / awr_ns_a(i-1)
         end if
        
         if (i>=histend+1) then

            ! (359) WESL_N_NHI_NS 
            if (esr_ns_a(i) == 0.0d0) then 
               wesl_n_nhi_ns_a(i) = 0.0d0 
            else 
               wesl_n_nhi_ns_a(i) = wesl_n_nhi_ns_a(i-1) * (tesl_n_o_nhi_ns_a(i) + &
                  tesl_n_n_nhi_ns_a(i)) / (tesl_n_o_nhi_ns_a(i-1) + &
                  tesl_n_n_nhi_ns_a(i-1)) * (awsggesl_a(i) / awsggesl_a(i-1)) * &
                  rawr_ns_a(i)
            end if
                   
            ! (360) WESL_N_NHI 
            wesl_n_nhi_a(i) = (wesl_n_nhi_s_a(i) + wesl_n_nhi_e_a(i) + &
               wesl_n_nhi_ns_a(i))
         
            ! (361) WESL_N_HI 
            wesl_n_hi_a(i) = (wesl_n_a(i) - wesl_n_nhi_a(i))

            ! (362) TEFC 
            tefc_a(i) = (tefc_a(i-1) / eggefc_a(i-1)) * eggefc_a(i)
            
            ! This is another "feature" TEFC_N.A exists in the dfile through MAX_YR,
            ! so the original model used it as actual data values and ignored equation
            ! (363) TEFC_N 
            ! tefc_n_a(i) = tefc_n_n_a(i) / 0.86d0
                           
            ! (364) TEFC_O 
            tefc_o_a(i) = (tefc_a(i) - tefc_n_a(i))
         
            ! (365) TEFC_N_O 
            tefc_n_o_a(i) = (tefc_n_a(i) - tefc_n_n_a(i))

            if (i >= histend+2) then ! match bmtony
            
               ! (366) CSE_TOT 
               cse_tot_a(i) = (yf_a(i) + ynf_a(i)) / (yf_a(i-1) + ynf_a(i-1)) * &
                  cse_tot_a(i-1) + cse_tot_add_a(i)
            end if
                           
            ! (367) CSE_CMB_N 
            cse_cmb_n_a(i) = (cse_tot_a(i) / (cmb_tot_a(i) + seo_a(0,0,i))) / &
               (cse_tot_a(i-1) / (cmb_tot_a(i-1) + seo_a(0,0,i-1))) * &
               (cse_cmb_n_a(i-1) / (cmb_tot_a(i-1) - cmb_a(i-1))) * &
               (cmb_tot_a(i) - cmb_a(i))
         
            ! (368) CSE 
            cse_a(i) = cse_tot_a(i) - cse_cmb_n_a(i)
            
         
            ! (369) ACSE_SEO 
            acse_seo_a(i) = (cse_tot_a(i) / (seo_a(0,0,i) + 0.416488d0 * cmb_tot_a(i)))
        
            ! (370) ACSE_CMB_TOT 
            acse_cmb_tot_a(i) = 0.416488d0 * acse_seo_a(i)
         
            ! (371) CSE_CMB_TOT 
            cse_cmb_tot_a(i) = acse_cmb_tot_a(i) * cmb_tot_a(i)
            
            ! (372) CSE_CMB 
            cse_cmb_a(i) = cse_cmb_tot_a(i) - cse_cmb_n_a(i)
         
         end if
         
         ! (373) ACSE_CMB 
         acse_cmb_a(i) = cse_cmb_a(i) / cmb_a(i)
         
         ! (374) CSE_SEO 
         cse_seo_a(i) = acse_seo_a(i) * seo_a(0,0,i)
         
         ! (375) CFCA 
         cfca_a(i) = wsgfca_a(i) / wsggefc_a(i)
         
         ! (376) CSLHI 
         cslhi_a(i) = (wesl_o_a(i) + wesl_n_hi_a(i)) / wsggesl_a(i)
         
         ! (377) CPF 
         cpf_a(i) = wspf_o_a(i) / wspf_a(i)
         
         if (i >= histend+1) then
         
            ! (378) CP 
            cp_a(i) = wspc_a(i) / wsdp_a(i)

            ! (379) COVERNA 
            coverna_a(i) = (wsca_a(i) + cse_a(i))
            
            ! (380) ASE 
            ase_a(i) = cse_a(i) / csw_a(i)
            
         end if
         
         ! (381) ASEHI 
         asehi_a(i) = cse_tot_a(i) / csw_hi_a(i)
         
         if (i >= histend+1) then
         
            ! (382) ACEA and ACEA_TOT
            acea_a(i)= coverna_a(i) / tcea_a(i)
            acea_tot_a(i) = (wsca_a(i) + cse_tot_a(i)) / tcea_a(i)
         
            ! (383) ACSLW 
            acslw_a(i) = wesl_o_a(i) / tesl_o_a(i)
         
            ! (384) ACMW 
            acmw_a(i) = acmw_a(i-1) * awsgfm_a(i) / awsgfm_a(i-1) * cml_a(i) / &
               cml_a(i-1)
         
            ! (385) ACFCW 
            acfcw_a(i) = wefc_o_a(i) / tefc_o_a(i)
         
            ! (386) ACFMW 
            acfmw_a(i) = acfmw_a(i-1) * (aiw_a(i-1) / aiw_a(i-3))**0.5d0
            
            if (i >= histend+2) then ! match bmtony
            
               ! (388) WSCAHI 
               wscahi_a(i) = wsca_a(i) + wefc_n_a(i) + wesl_n_hi_a(i) + &
                  wsca_hio_oth_a(i)
            
            end if
               
            ! (389) TE_SFO_LRP 
            te_sfo_lrp(0,0,i) = tel_so(0,0,i)

            ! (390) WE_SFO_LRP 
            we_sfo_lrp_a(i) = te_sfo_lrp(0,0,i) * acwa_a(i)
            
            ! (391) TE_SFM_LRP 
            te_sfm_lrp_a(i) = te_sfo_lrp(0,0,i)

            ! (392) WE_SFM_LRP 
            we_sfm_lrp_a(i) = te_sfm_lrp_a(i) * acwa_a(i) * 0.5d0
            
            ! (393) WE_SF_LRP 
            we_sf_lrp_a(i) = we_sfo_lrp_a(i) + we_sfm_lrp_a(i)
            
            ! (394) WE_SF 
            we_sf_a(i) = ws_eo_esf(i) + we_sf_lrp_a(i)

            ! (395) WS_MEF 
            ws_mef_a(i) = wscahi_a(i) - we_sf_a(i) + wsprrb_a(i) + wesl_n_nhi_a(i) + &
                          (wesl_n_nhi_s_a(i) /tesl_n_s_a(i)) *  te_ps_m(0,0,i) + &
                          (0.5d0 * 1.800d0 / 44.32167d0) * (aiw_a(i-1) / 1d3 * prod_a(i) / prod_a(i-1) * ahrs_a(i) / ahrs_a(i-1) * pgdp_a(i) / pgdp_a(i-1)) * te_ph_m(0,0,i) + &
                          ws_eo_asf1(i) + ws_eo_asj1(i) + ws_eo_awj(i) + ws_eo_awh(i)
            
            ! (396) HEW_M 
            hew_m(0,0,i) = he_m(0,0,i) - seo_hi_a(i)
            
            ! (397) WSW_MEF 
            wsw_mef_a(i) = hew_m(0,0,i) + te_mn(0,0,i)

            ! (398) AWS_MEF 
            aws_mef_a(i) = ws_mef_a(i) / wsw_mef_a(i)
         
         end if
         
         if (i>=histend+2) then

            ! (399) AIW 
            if (aiw_gr_yr_a(i) == 0.0d0) then 
               aiw_a(i) = aiw_a(i-1) * aws_mef_a(i) / aws_mef_a(i-1)
            else          
               aiw_a(i) = aiw_a(i-1) * (1.0d0 + aiw_gr_a(i) / 100.0d0)         
            end if
            
            ! (400) TEPH_N 
            teph_n_a(0,0,i) = enawph_a(0,0,i) * (1 - cph_a(i))
            
         end if
         
         if (i>=histend+1) then
            
            ! (401) TEP_N_N_S 
            tep_n_n_s_a(i) = te_ps_m(0,0,i)

         end if

      end do

   end subroutine SolveEmploymentAtAnyTime

!===============================================================================

   subroutine SolveEquations402_423()

      integer :: i

      ! Equations 402 - 423

      do i = startYr, endYr
           
         ! (402) CE_M_100F 
         ! ce_m_100f(i) = tcea_a(i) - te_sfo_lrp(0,0,i) - &
         !    teo_nol_s(0,24,i) - teo_noi_s(0,24,i)

         if (i>=histend+1) then
            
           ! (403) TE_SF_LRP 
           te_sf_lrp_a(i) = te_sfo_lrp(0,0,i) + te_sfm_lrp_a(i)
         
            ! (404) WE_SF_TEO 
            we_sf_teo_a(i) = ws_eo_esf(i)
            
            ! (405) TE_SF_TEO 
            te_sf_teo_a(i) = teo_nol_s(0,24,i) + teo_noi_s(0,24,i)

         end if
         
         ! (406) CE_M 
         ce_m(0,0,i) = he_m(0,0,i) - ((he_wof_m(0,0,i) + he_wol_m(0,0,i) + he_wor_m(0,0,i)) - (he_wosf_m(0,0,i) + he_wosl_m(0,0,i) + he_wosr_m(0,0,i)))
         
         ! (407) CEW_M 
         cew_m(0,0,i) = ce_m(0,0,i) - seo_a(0,0,i)
         
         ! (408) CESO_M 
         ceso_m(0,0,i) = seo_a(0,0,i)
         
         ! (409) HESO_M 
         heso_m(0,0,i) = seo_hi_a(i)
         
         ! (410) WSWAHI 
         wswahi_a(i) = (tceahi_a(i) - seo_hi_a(i))
         
         ! (411) ACWAHI  
         acwahi_a(i) = wscahi_a(i) / wswahi_a(i)
         
         ! (412) COVERNHI 
         covernhi_a(i) = wscahi_a(i) + cse_tot_a(i)
         
         ! (413) ACEAHI 
         aceahi_a(i) = covernhi_a(i) / tceahi_a(i)
         
         ! (414) EDMILAF 
         edmilaf_a(i) = edmil_a(i) * 1.1d0
         
         ! (415) EDMILT 
         edmilt_a(i) = (2.00303d0 - 50.7517d0 / year_a(i)) * edmilaf_a(i)
         
         ! (416) EDMILR 
         edmilr_a(i) = edmilt_a(i) - edmilaf_a(i)
         
         ! (417) MWC_ED_O 
         mwc_ed_o_a(i) = 1.2d0 * edmilaf_a(i) * 0.997d0
         
         ! (418) MWC_ED_HI 
         mwc_ed_hi_a(i) = 1.2d0 * edmilaf_a(i)
         
         ! (419) AMWC_GO2 
         amwc_go2_a(i) = min(1.2d0,awsgfm_a(i) * (2.0d0 / 52.0d0) * &
            (1.0d0 / 3.0d0))
         
         ! (420) MWC_EDR_O 
         mwc_edr_o_a(i) = amwc_go2_a(i) * edmilr_a(i) * (1.0d0 - 0.017d0)
         
         ! (421) MWC_EDR_HI 
         mwc_edr_hi_a(i) = mwc_edr_o_a(i) + ((1.2d0 + amwc_go2_a(i)) * 0.5d0) * &
            edmilr_a(i) * 0.017d0
         
         ! (422) MWC_O 
         mwc_o_a(i) = mwc_ed_o_a(i) + mwc_edr_o_a(i)
         
         ! (423) MWC_HI 
         mwc_hi_a(i) = mwc_ed_hi_a(i) + mwc_edr_hi_a(i) 
         
      end do

   end subroutine SolveEquations402_423

!===============================================================================

end module EconModSolAEquationsMod