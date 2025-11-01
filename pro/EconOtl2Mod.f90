module EconOtl2Mod

   use EconParMod
   use EconLibMod
   use EconModSolAVarMod ! uses EconModSol2VarMod and EconOtl1VarMod
   use EconOtl1OutMod
   use EconOtl2OutMod
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconOtl2Main

   ! These variables are not written to the OTL file and are private
   
   ! Weights for the proportion of male and female 70 and over considered workers (rather than family members)
   real (kind = 8), parameter :: WTM70OWKR = 1.0d0  ! male
   real (kind = 8), parameter :: WTF70OWKR = 1.0d0  ! female
   
   real (kind=8), dimension(0:2,0:31,MAX_YR) :: eo_no_s, eo_no_u
   real (kind=8), dimension(0:2,16:100,MAX_YR) :: eo_no_s_sy, eo_no_u_sy
   real (kind=8), dimension(0:2,16:100,MAX_YR) :: eo_no_s2_sy, eo_no_u2_sy
   real (kind=8), dimension(0:2,0:31,MAX_YR) :: eo_no_s2, eo_no_u2
   real (kind=8), dimension(0:2,1:31,MAX_YR) :: teo_esf_1
   real (kind=8), dimension(MAX_YR) :: teo_esf_m16o_x2, teo_esf_f16o_x2, teo_esf_16o_x2
   real (kind=8), dimension(MAX_YR) :: teo_esf_m16o_x3, teo_esf_f16o_x3, teo_esf_16o_x3
   real (kind=8), dimension(MAX_YR) :: wt2_ls_temp, wt2_is_temp, wt2_lu_temp, wt2_iu_temp
   ! Additional population concepts for matching the three broad categories from Demo (2016)
   real (kind = 8), dimension(MAX_YR) :: no_a_prelim, no_na_prelim
   real (kind = 8), dimension(MAX_YR) :: no_awk
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_awk_sy !, no_nawk_sy
   real (kind = 8), dimension(MAX_YR) :: no_ar_tot, no_anr_tot !, no_nar_tot, no_nanr_tot
   real (kind = 8), dimension(MAX_YR) :: no_anr_excess, no_anr_correct !, no_nanr_excess, no_nanr_correct
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_ar, no_anr !, no_nar, no_nanr
   real (kind = 8), dimension(MAX_YR) :: no_awtf16o, no_ar16o !, no_nar16o, no_nawtf16o
   real (kind = 8), dimension(MAX_YR) :: no_awk1669 !, no_nawk1669
   real (kind = 8), dimension(MAX_YR) :: no_ar1669 !, no_nar1669
   real (kind = 8), dimension(MAX_YR) :: no_awtf1669 !, no_nawtf1669
   real (kind = 8), dimension(MAX_YR) :: no_awtf_guess !, no_nawtf_guess
   ! Arrays to keep track of residual mismatch to Demo categories
   integer, dimension(1:2,0:100,MAX_YR) :: anr_excess, ever_excess = 0
   
contains

!===============================================================================

   subroutine EconOtl2Main()
   
      integer :: iterOtl2A

      call ProfileCheckpoint(8)
      call InitOtl2()
      call SolveOtl2()

      call SetOtherEmployment()
      call EstimatePopulationComponents()
      call ScaleAggregatesToPopData() !2016
      call DisaggregatePopulationComponents()
      call SumNonResidualGrpsA() !2016
      iterOtl2A = 1
      do while (sum(anr_excess) > 0 .and. iterotl2A <= 10)
        call BumpUpNonResidualGrpsA()
        iterotl2A = iterotl2A + 1
        call SumNonResidualGrpsA()
      end do
      call AllocateResidualChildren() !2016
      call Allocate70o() !2016
      
      call CompareAggregates()
      call ScaleCategories()
      
      call CreateAdditionalPopulationConcepts()
      call EstimateEmploymentComponents()
      call CreateAdditionalEmploymentConcepts()
      call AggregateEmploymentConcepts()
      
      call DistributeUnauthorized()
      call CalculateBaseValues()
      call EstimateTargetWeights()
      call AdjustTargets()
      call InitializeCohort()
      call ComputeCohorts()
      call ComputeWeights()
      call ControlHistoricalValues()
      call AdjustValues()
      call ComputeAuthorizedTotals()
      call ComputeUnauthorizedTotals()
      call SumByPosting()
      call SumAgeGroups()
      
      call ComputeAverageWages()
      
      call EconOtl1OutMain()
      call EconOtl2OutMain()
      call Finish()

   end subroutine EconOtl2Main

!===============================================================================

   subroutine InitOtl2()
   
      per1a = 64
      startQtr = per1a * 4 + (qtr1a - 1)
      startYr = per1a    

   end subroutine InitOtl2

!===============================================================================

   subroutine SolveOtl2
   
      write(*,'(//a//)') "Solving Otl2, Please Wait..."

   end subroutine SolveOtl2

!===============================================================================

   subroutine SetOtherEmployment()
   
      integer :: age, sex, grp, yr1
      integer, dimension(13) :: lowAge =  (/ 16, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70 /)
      integer, dimension(13) :: highAge = (/ 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74 /)
      real (kind = 8), dimension(MAX_YR) :: lctemp

      call FetchSeries(DFILE, "LC.A", lctemp)
      yr1 = sample(2) + 1

      do sex = 1, 2
         do grp = 1, 9
            do age = lowAge(grp), highAge(grp)
               re_sy(sex,age,yr1:) = p_a(sex,grp,yr1:) * (1d0 - ru_a(sex,grp,yr1:) / 100d0)
            end do
         end do
         do grp = 10, 13
            do age = lowAge(grp), highAge(grp)
               re_sy(sex,age,yr1:) = psy_a(sex,age,yr1:) * (1d0 - ru_a(sex,grp,yr1:) / 100d0)
            end do
         end do
         do age = 75, 100
            re_sy(sex,age,yr1:) = psy_a(sex,age,yr1:) * (1d0 - ru_a(sex,14,yr1:) / 100d0)
         end do
      end do

      do sex = 1, 2
         do age = 16, 100
            eo_sy(sex,age,yr1:) = re_sy(sex,age,yr1:) * (nilsy(sex,age,yr1:) - nildsy(sex,age,yr1:))
         end do
      end do

   end subroutine SetOtherEmployment

!===============================================================================
   
   subroutine EstimatePopulationComponents()
   
      integer :: yr
      
      real (kind=8), dimension(MAX_YR) :: prop_auth_sp
      
      ! These weights were developed by Sven to control to historical values from DHS

      asf1_hwt(64:86) = 0.35d0
      asf1_hwt(87:110) = (/ 0.36d0, 0.37d0, 0.38d0, 0.38d0, 0.4d0, 0.41d0, &
         0.42d0, 0.42d0, 0.43d0, 0.43d0, 0.52d0, 0.6d0, 0.62d0, 0.66d0, &
         0.67d0, 0.64d0, 0.61d0, 0.6d0, 0.63d0, 0.67d0, 0.74d0, 0.79d0, &
         0.87d0, 0.93d0 /)
      asf1_hwt(111:) = 1d0

      asj1_hwt(64:86) = 0.5d0
      asj1_hwt(87:110) = (/ 0.52d0, 0.54d0, 0.56d0, 0.58d0, 0.6d0, 0.64d0, &
         0.68d0, 0.72d0, 0.76d0, 0.80d0, 0.84d0, 0.88d0, 0.92d0, 0.96d0, &
         1.00d0, 1.00d0, 1.00d0, 1.00d0, 1.00d0, 1.00d0, 1.00d0, 1.00d0, &
         1.00d0, 1.00d0 /)
      asj1_hwt(111:) = 1d0

      awj_hwt(64:)  = asj1_hwt(64:)

      awh_hwt(64:88) = 0.30d0
      awh_hwt(89:110) = (/ 0.33d0, 0.30d0, 0.24d0, 0.22d0, 0.19d0, 0.17d0, &
         0.15d0, 0.13d0, 0.23d0, 0.36d0, 0.37d0, 0.36d0, 0.30d0, 0.22d0, &
         0.20d0, 0.21d0, 0.35d0, 0.60d0, 0.80d0, 0.99d0, 1.04d0, 0.97d0 /)
      awh_hwt(111:) = 1d0

      awt_hwt(64:88) = 0.22d0
      awt_hwt(89:110) = (/ 0.22d0, 0.23d0, 0.24d0, 0.24d0, 0.25d0, 0.27d0, &
         0.31d0, 0.33d0, 0.46d0, 0.63d0, 0.70d0, 0.81d0, 0.89d0, 0.90d0, &
         0.89d0, 0.88d0, 0.91d0, 0.98d0, 1.03d0, 1.01d0, 0.98d0, 0.96d0 /)
      awt_hwt(111:) = 1d0
         
      ! Students - F1 (and M1) Visa Holders and Families - Overstayers
      
      no_asf1(0,:) = 0.0055d0 * asf1_hwt * e_a(0,0,:)
      no_asf2(0,:) = 0.08d0 / 0.81d0 * no_asf1(0,:)
         
      no_nasf1(0,:) = 0.5d0          * no_asf1(0,:)
      no_nasf2(0,:) = 0.08d0 / 0.81d0 * no_nasf1(0,:)
         
      ! Visitors (J1 and H2A visas - authorized to work but not covered)
         
      !  J1 Visa Holders (not covered)
         
      no_asj1(0,:)  = (0.038d0 / 139.9d0) * asj1_hwt * e_a(0,0,:)
      no_asj2(0,:)  = (0.05d0 / 0.19d0) * no_asj1(0,:)

      no_awj(0,:)   = ((0.19d0 - 0.038d0) / 139.9d0) * awj_hwt * e_a(0,0,:)
      no_awjf(0,:)  = (0.05d0 / 0.19d0) * no_awj(0,:)
         
      !  J1 Visa Holders - Overstay
         
      no_nasj1(0,:) = (0.1d0) * no_asj1(0,:)
      no_nasj2(0,:) = (0.1d0) * no_asj2(0,:)

      no_nawj(0,:)  = (0.5d0) * no_awj(0,:)
      no_nawjf(0,:) = (0.5d0) * no_awjf(0,:)
         
      !  H2A Visa Holders (not covered and overstayers)
         
      no_awh(0,:)  = (0.06d0 / 139.9d0) * awh_hwt * e_a(0,0,:)
      no_nawh(0,:) = 0.5d0 * no_awh(0,:)
         
      ! Temporary workers (H1B, H2B, H3, O1, O2, P1, P2, P3, R1, TN, L1, E1, 
      ! E2 and E3 visas - authorized to work and covered)
         
      no_awt(0,:) = (0.7d0 - 0.06d0) / 139.9d0 * awt_hwt  * e_a(0,0,:)
         
      ! CHANGE FKA "CHANGE DUE TO 'INCREASE IN ENTREPRENEURS'"
      
      ! no_awt
      ! Sven provided factors below in 2015-02-24 email ("NonImmig stock to AWT conversion.xlsx")
      
      no_awt(0,115) = no_awt(0,115) + 0.011d0
      no_awt(0,116) = no_awt(0,116) + 0.032d0
      no_awt(0,117) = no_awt(0,117) + 0.048d0
      no_awt(0,118) = no_awt(0,118) + 0.060d0
      no_awt(0,119) = no_awt(0,119) + 0.069d0
      no_awt(0,120) = no_awt(0,120) + 0.077d0
      no_awt(0,121) = no_awt(0,121) + 0.083d0
      no_awt(0,122) = no_awt(0,122) + 0.087d0
      no_awt(0,123) = no_awt(0,123) + 0.089d0
      no_awt(0,124) = no_awt(0,124) + 0.089d0
      no_awt(0,125) = no_awt(0,125) + 0.087d0
      no_awt(0,126) = no_awt(0,126) + 0.085d0
      no_awt(0,127) = no_awt(0,127) + 0.083d0
      no_awt(0,128) = no_awt(0,128) + 0.081d0
      no_awt(0,129) = no_awt(0,129) + 0.079d0
      no_awt(0,130) = no_awt(0,130) + 0.078d0
      no_awt(0,131) = no_awt(0,131) + 0.076d0
      no_awt(0,132) = no_awt(0,132) + 0.075d0
      no_awt(0,133) = no_awt(0,133) + 0.074d0
      no_awt(0,134) = no_awt(0,134) + 0.073d0
      no_awt(0,135) = no_awt(0,135) + 0.072d0
      no_awt(0,136) = no_awt(0,136) + 0.070d0
      no_awt(0,137) = no_awt(0,137) + 0.069d0
      no_awt(0,138) = no_awt(0,138) + 0.069d0
      no_awt(0,139) = no_awt(0,139) + 0.068d0
      no_awt(0,140) = no_awt(0,140) + 0.068d0
      no_awt(0,141) = no_awt(0,141) + 0.067d0
      no_awt(0,142) = no_awt(0,142) + 0.066d0
      no_awt(0,143) = no_awt(0,143) + 0.066d0
      no_awt(0,144) = no_awt(0,144) + 0.066d0
      no_awt(0,145) = no_awt(0,145) + 0.066d0
      no_awt(0,146) = no_awt(0,146) + 0.066d0
      no_awt(0,147) = no_awt(0,147) + 0.065d0
      no_awt(0,148) = no_awt(0,148) + 0.065d0
      no_awt(0,149) = no_awt(0,149) + 0.066d0
      no_awt(0,150) = no_awt(0,150) + 0.066d0
      no_awt(0,151) = no_awt(0,151) + 0.066d0
      no_awt(0,152) = no_awt(0,152) + 0.066d0
      no_awt(0,153) = no_awt(0,153) + 0.066d0
      no_awt(0,154) = no_awt(0,154) + 0.066d0
      no_awt(0,155) = no_awt(0,155) + 0.067d0
      no_awt(0,156) = no_awt(0,156) + 0.067d0
      no_awt(0,157) = no_awt(0,157) + 0.066d0
      no_awt(0,158) = no_awt(0,158) + 0.067d0
      no_awt(0,159) = no_awt(0,159) + 0.067d0
      no_awt(0,160) = no_awt(0,160) + 0.068d0
      no_awt(0,161) = no_awt(0,161) + 0.068d0
      no_awt(0,162) = no_awt(0,162) + 0.068d0
      no_awt(0,163) = no_awt(0,163) + 0.068d0
      no_awt(0,164) = no_awt(0,164) + 0.068d0
      no_awt(0,165) = no_awt(0,165) + 0.068d0
      no_awt(0,166) = no_awt(0,166) + 0.069d0
      no_awt(0,167) = no_awt(0,167) + 0.069d0
      no_awt(0,168) = no_awt(0,168) + 0.069d0
      no_awt(0,169) = no_awt(0,169) + 0.069d0
      no_awt(0,170) = no_awt(0,170) + 0.069d0
      no_awt(0,171) = no_awt(0,171) + 0.069d0
      no_awt(0,172) = no_awt(0,172) + 0.069d0
      no_awt(0,173) = no_awt(0,173) + 0.069d0
      no_awt(0,174) = no_awt(0,174) + 0.069d0
      no_awt(0,175) = no_awt(0,175) + 0.069d0
      no_awt(0,176) = no_awt(0,176) + 0.069d0
      no_awt(0,177) = no_awt(0,177) + 0.069d0
      no_awt(0,178) = no_awt(0,178) + 0.069d0
      no_awt(0,179) = no_awt(0,179) + 0.069d0
      no_awt(0,180) = no_awt(0,180) + 0.069d0
      no_awt(0,181) = no_awt(0,181) + 0.069d0
      no_awt(0,182) = no_awt(0,182) + 0.069d0
      no_awt(0,183) = no_awt(0,183) + 0.069d0
      no_awt(0,184) = no_awt(0,184) + 0.069d0
      no_awt(0,185) = no_awt(0,185) + 0.069d0
      no_awt(0,186) = no_awt(0,186) + 0.069d0
      no_awt(0,187) = no_awt(0,187) + 0.069d0
      no_awt(0,188) = no_awt(0,188) + 0.069d0
      no_awt(0,189) = no_awt(0,189) + 0.069d0
      no_awt(0,190) = no_awt(0,190) + 0.069d0
      no_awt(0,191) = no_awt(0,191) + 0.069d0
      no_awt(0,192) = no_awt(0,192) + 0.069d0
      no_awt(0,193) = no_awt(0,193) + 0.069d0
      no_awt(0,194) = no_awt(0,194) + 0.069d0
      no_awt(0,195) = no_awt(0,195) + 0.069d0
      no_awt(0,196) = no_awt(0,196) + 0.069d0
      no_awt(0,197) = no_awt(0,197) + 0.069d0
      no_awt(0,198) = no_awt(0,198) + 0.069d0
      no_awt(0,199:) = no_awt(0,199:) + 0.069d0
      
      ! Temporary workers - Family members (H4, O3, P4, R2, TD, L2 visas - 
      ! not authorized to work except for L2)
         
      no_awtfa(0,:)  = 0.15d0 * 0.35d0 / 0.7d0 * (no_awh(0,:) + no_awt(0,:))
      no_awtfn(0,:)  = 0.15d0 / 0.7d0 * (no_awh(0,:) + no_awt(0,:)) * (1d0 - 0.35d0)
      no_awtf(0,:)  = no_awtfa(0,:) + no_awtfn(0,:)
         
      ! CHANGE FOR NEWLY AUTHORIZED SPOUSES OF WORKERS
      ! Assume 9/13 of the currently not authorized spouses will become authorized,
      ! so that at the end only 20% (instead of 65% as currently assumed) are not authorized.
      ! This is consistent with the majority of workers' spouses (but not all) being affected.
      ! We are phasing in the change over 5 years, as we expect labor force entry for newly 
      ! authorized spouses to be gradual.

      prop_auth_sp(:) = 0.35d0
      prop_auth_sp(116) = 0.44d0
      prop_auth_sp(117) = 0.53d0
      prop_auth_sp(118) = 0.62d0
      prop_auth_sp(119) = 0.71d0
      prop_auth_sp(120:) = 0.80d0
      
      ! New equations for no_awtf groups:

      no_awtfa(0,:) = 0.15d0/0.7d0 * (no_awh(0,:) + no_awt(0,:))  * prop_auth_sp(:)
      no_awtfn(0,:) = 0.15d0/0.7d0 * (no_awh(0,:) + no_awt(0,:)) * (1d0 - prop_auth_sp(:))
      no_awtf(0,:)  = no_awtfa(0,:) + no_awtfn(0,:)
      
      ! Temporary workers - Overstayers
      no_nawt(0,:)  = 0.5d0 * no_awt(0,:)
      no_nawtf(0,:) = 0.5d0 * no_awtf(0,:)
      
      no_nawk(:) = no_nawh(0,:)  + no_nawt(0,:)
         
      ! Temporary routine to check intermediate values
      !do yr = 64, MAX_YR
      !   write(3003,'(20f12.6)') no_asf1(yr), no_asf2(yr), no_nasf1(yr), &
      !   no_nasf2(yr), no_asj1(yr), no_asj2(yr), no_awj(yr), no_awjf(yr), &
      !   no_nasj1(yr), no_nasj2(yr), no_nawj(yr), no_nawjf(yr), no_awh(yr), &
      !   no_nawh(yr), no_awt(yr), no_awtfa(yr), no_awtfn(yr), no_awtf(yr), &
      !   no_nawt(yr), no_nawtf(yr)
      !end do
   
   end subroutine EstimatePopulationComponents
   
!===============================================================================
      
   subroutine ScaleAggregatesToPopData()
   
      integer :: age, sex
      
      ! This includes no_awtfn, which is not in our "authorized" category, but it is in the Non-Immigrant demo cat
      no_a_prelim = no_asf1(0, :) + no_asj1(0, :) &
                  + no_asf2(0, :) + no_asj2(0, :) &
                  + no_awj(0, :)  + no_awh(0, :)   + no_awt(0, :) &
                  + no_awjf(0, :) + no_awtfa(0, :) + no_awtfn(0, :) 
      
      ! The following calls (and the subroutine called) could be bypassed.
      ! The total population series exist in the OP file and could be pulled in the EconOtlVarMod.
      call TotalByAgeSex(nilasy, nila)
      call TotalByAgeSex(nilnasy, nilna)
      call TotalByAgeSex(nildnasy, nildna)
      
      ! Scale totals by category
      no_asf1(0, :)  = no_asf1(0, :)  * nila / no_a_prelim
      no_asj1(0, :)  = no_asj1(0, :)  * nila / no_a_prelim
      no_asf2(0, :)  = no_asf2(0, :)  * nila / no_a_prelim
      no_asj2(0, :)  = no_asj2(0, :)  * nila / no_a_prelim
      no_awj(0, :)   = no_awj(0, :)   * nila / no_a_prelim
      no_awh(0, :)   = no_awh(0, :)   * nila / no_a_prelim
      no_awt(0, :)   = no_awt(0, :)   * nila / no_a_prelim
      no_awjf(0, :)  = no_awjf(0, :)  * nila / no_a_prelim
      no_awtfa(0, :) = no_awtfa(0, :) * nila / no_a_prelim
      no_awtfn(0, :) = no_awtfn(0, :) * nila / no_a_prelim
      
      ! Additional concepts needed (for computing residual groups by age and sex)
      
      ! residual non-immigrant workers
      no_awk  = no_awh(0, :)   + no_awt(0, :)
      ! residual non-immigrant worker families
      no_awtf(0, :) = no_awtfa(0, :) + no_awtfn(0, :)
      ! total residual non-immigrants
      no_ar_tot = no_awk + no_awtf(0, :)
      ! total non-residual non-immigrants
      no_anr_tot = no_a_prelim - no_ar_tot
      !! residual non-immigrant worker families 16 and over
      no_awtf16o = no_awtf(0, :)
      ! total residual non-immigrants 16 and over
      no_ar16o = no_ar_tot
       
   end subroutine ScaleAggregatesToPopData
  
!===============================================================================
      
   subroutine TotalByAgeSex(disagg, agg)
   ! Aggregates a population over all ages and sexes
   
      integer :: age, sex, year
      real (kind=8), dimension(1:2,0:100,MAX_YR), intent(IN) :: disagg
      real (kind=8), dimension(MAX_YR), intent(OUT) :: agg
      
      agg = 0d0
      
      do year = startYr, MAX_YR
        do age = 0, 100
          do sex = 1, 2
            agg(year) = agg(year) + disagg(sex, age, year)
          end do
        end do
      end do
       
   end subroutine TotalByAgeSex
  
!===============================================================================
      
   subroutine SumNonResidualGrpsA()
   ! Sum non-residual groups for authorized non-immigrants and compute the grand total of all residual groups by age and sex
   ! first step of "splits change 4"
   
      integer :: age, sex, year
      real(kind = 8) :: adjf
      
      no_anr_excess = 0
      no_anr_correct = 0
      anr_excess = 0
      
      do year = startYr, MAX_YR
        do age = 0, 100
          do sex = 1, 2
            no_anr(sex, age, year) = no_asf1_sy(sex, age, year) + no_asf2_sy(sex, age, year) &
                                   + no_asj1_sy(sex, age, year) + no_asj2_sy(sex, age, year) &
                                   + no_awj_sy(sex, age, year) + no_awjf_sy(sex, age, year) 
            no_ar(sex, age, year) = nilasy(sex, age, year) - no_anr(sex, age, year)
            if (no_ar(sex, age, year) < -1d-10) then
              ! do not allow residual groups to be negative
              ! keep track of any such excess
              anr_excess(sex, age, year) = 1
              ever_excess(sex, age, year) = 1
              no_anr_excess(year) = no_anr_excess(year) - no_ar(sex, age, year)
              adjf = nilasy(sex, age, year) / (no_anr(sex, age, year) +  + epsilon(0d0))
              no_asf1_sy(sex, age, year) = no_asf1_sy(sex, age, year) * adjf
              no_asf2_sy(sex, age, year) = no_asf2_sy(sex, age, year) * adjf
              no_asj1_sy(sex, age, year) = no_asj1_sy(sex, age, year) * adjf
              no_asj2_sy(sex, age, year) = no_asj2_sy(sex, age, year) * adjf
              no_awj_sy(sex, age, year) = no_awj_sy(sex, age, year) * adjf
              no_awjf_sy(sex, age, year) = no_awjf_sy(sex, age, year) * adjf
              no_ar(sex, age, year) = 0
            else
              if (no_ar(sex, age, year) < 0) no_ar(sex, age, year) = 0d0
              ! round counts that are negative by a trivial amount to zero
              if (ever_excess(sex, age, year)==0) no_anr_correct(year) = no_anr_correct(year) + no_anr(sex, age, year)
            end if
          end do
        end do
      end do
      
   end subroutine SumNonResidualGrpsA
  
!===============================================================================
      
   subroutine BumpUpNonResidualGrpsA()
   ! If there was ecxess in non-residual groups at some ages,
   ! allocate it to other ages proportionally
   
      integer :: age, sex, year
      real(kind = 8) :: adjf
      
      do year = startYr, MAX_YR
        adjf = (no_anr_correct(year) + no_anr_excess(year)) / (no_anr_correct(year) + epsilon(0d0))
        do age = 0, 100
          do sex = 1, 2
            if (ever_excess(sex, age, year) == 0) then
              no_asf1_sy(sex, age, year) = no_asf1_sy(sex, age, year) * adjf
              no_asf2_sy(sex, age, year) = no_asf2_sy(sex, age, year) * adjf
              no_asj1_sy(sex, age, year) = no_asj1_sy(sex, age, year) * adjf
              no_asj2_sy(sex, age, year) = no_asj2_sy(sex, age, year) * adjf
              no_awj_sy(sex, age, year) = no_awj_sy(sex, age, year) * adjf
              no_awjf_sy(sex, age, year) = no_awjf_sy(sex, age, year) * adjf
            end if
          end do
        end do
      end do
       
   end subroutine BumpUpNonResidualGrpsA
!===============================================================================
      
   subroutine AllocateResidualChildren()
   ! assign everyone residual 15 and under to worker families (UNLESS TOTAL NUMBER OF FAM MEMBERS EXCEEDED)
   ! allocate the numbers proportionally between awtfa and awtfn
   ! keep track of the remaining (16 and over) totals
   ! first step of "splits change 4"
   
      integer :: age, sex, year
      real(kind=8) :: propa
      logical :: max_reached
      
      max_reached = .FALSE.
      
      do year = startYr, MAX_YR
        max_reached = .FALSE.
        do age = 0, 15
          do sex = 1, 2
            if (.NOT.max_reached) then
              no_awtf_sy(sex, age, year) = no_ar(sex, age, year)
              ! allocate the numbers proportionally between awtfa and awtfn
              propa = no_awtfa(0, year) / no_awtf(0, year)
              no_awtfa_sy(sex, age, year) = no_awtf_sy(sex, age, year) * propa
              no_awtfn_sy(sex, age, year) = no_awtf_sy(sex, age, year) - no_awtfa_sy(sex, age, year)
              ! make sure workers are assigned zeros
              no_awh_sy(sex, age, year) = 0d0
              no_awt_sy(sex, age, year) = 0d0
             ! keep track of the remaining (16 and over) totals
              no_awtf16o(year) = no_awtf16o(year) - no_awtf_sy(sex, age, year)
              no_ar16o(year) = no_ar16o(year) - no_awtf_sy(sex, age, year)
              if (no_awtf16o(year) <= 0) then
                max_reached = .TRUE.
                no_awtf_sy(sex, age, year) = -no_awtf16o(year)
                no_awtfa_sy(sex, age, year) = no_awtf_sy(sex, age, year) * propa
                no_awtfn_sy(sex, age, year) = no_awtf_sy(sex, age, year) - no_awtfa_sy(sex, age, year)
                no_awk_sy(sex, age, year) = no_ar(sex, age, year) - no_awtf_sy(sex, age, year)
                no_awh_sy(sex, age, year) = no_awk_sy(sex, age, year) * no_awh(0, year) / no_awk(year)
                no_awt_sy(sex, age, year) = no_awk_sy(sex, age, year) - no_awh_sy(sex, age, year)
                no_awtf16o(year) = 0d0
              end if
            else ! MAX FAM MEMBERS REACHED, MAKE THE REST WORKERS
              no_awtfa_sy(sex, age, year) = 0d0
              no_awtfn_sy(sex, age, year) = 0d0
              no_awtf_sy(sex, age, year) = 0d0
              no_awk_sy(sex,age,year) = no_ar(sex,age,year)
              no_awh_sy(sex, age, year) = no_awk_sy(sex, age, year) * no_awh(0, year) / no_awk(year)
              no_awt_sy(sex, age, year) = no_awk_sy(sex, age, year) - no_awh_sy(sex, age, year)
              no_ar16o(year) = no_ar16o(year) - no_awk_sy(sex, age, year)
            end if
          end do
        end do
      end do
              
   end subroutine AllocateResidualChildren
  
!===============================================================================
      
   subroutine Allocate70o()
   ! assign roles to older males and females
   ! and adjust totals by group accordingly
   ! remaining steps of "splits change 4"
   
      integer :: age, sex, year
      real(kind=8) :: excess_family, penda, pendn, pend, wtf_add
      
      ! Initializee totals for ages 16-69
      no_awk1669 = no_awk
      no_ar1669 = no_ar16o
      no_awtf1669 = no_awtf16o
      
      do year = startYr, MAX_YR
        do age = 70, 100
          ! assign a given proportion of residual males 70 and over to workers
          !   (currently this means every male)
          no_awk_sy(1,age,year) = WTM70OWKR * no_ar(1,age,year)
          ! allocate the numbers proportionally between awh and awt
          no_awh_sy(1, age, year) = no_awk_sy(1, age, year) * no_awh(0, year) / (no_awk(year) + epsilon(0d0))
          no_awt_sy(1, age, year) = no_awk_sy(1, age, year) - no_awh_sy(1, age, year)
          ! assign the rest to family members
          !   (currently, family members are assigned zeros)
          if (WTM70OWKR < 0.999) then
            no_awtf_sy(1, age, year) = (1 - WTM70OWKR) * no_ar(1,age,year)
            no_awtfa_sy(1, age, year) = no_awtf_sy(1, age, year) * no_awtfa(0, year) / (no_awtf(0, year) + epsilon(0d0))
            no_awtfn_sy(1, age, year) = no_awtf_sy(1, age, year) - no_awtfa_sy(1, age, year)
          else ! this is the current case
            no_awtfa_sy(1, age, year) = 0d0
            no_awtfn_sy(1, age, year) = 0d0
          end if
          ! keep track of the remaining (16-69) totals
          no_awk1669(year) = no_awk1669(year) - no_awk_sy(1, age, year)
          no_awtf1669(year) = no_awtf1669(year) - no_awtf_sy(1, age, year)
          no_ar1669(year) = no_ar1669(year) - no_awk_sy(1, age, year) - no_awtf_sy(1, age, year)
        end do
      end do
      
      ! assign a given proportion (currently all) of residual females 70 and over to workers and the rest to families
      ! allocate the numbers proportionally between awh and awt and between awtfa and awtfn
      !     (Note: It is unrealistic that there would be female temp ag workers 70+, but deviating from strict
      !      proportionality would complicate the computation, and the numbers should be small anyway.)
      ! keep track of the remaining (16-69) totals
      do year = startYr, MAX_YR
        do age = 70, 100
          no_awk_sy(2, age, year) = WTF70OWKR * no_ar(2, age, year)
          no_awtf_sy(2, age, year) = (1 - WTF70OWKR) * no_ar(2, age, year)
          ! allocate the numbers proportionally between awh and awt
          no_awh_sy(2, age, year) = no_awk_sy(2, age, year) * no_awh(0, year) / (no_awk(year) + epsilon(0d0))
          no_awt_sy(2, age, year) = no_awk_sy(2, age, year) - no_awh_sy(2, age, year)
          ! allocate the numbers proportionally between awtfa and awtfn
          no_awtfa_sy(2, age, year) = no_awtf_sy(2, age, year) * no_awtfa(0, year) / (no_awtf(0, year) + epsilon(0d0))
          no_awtfn_sy(2, age, year) = no_awtf_sy(2, age, year) - no_awtfa_sy(2, age, year)
          ! keep track of the remaining (16-69) totals
          no_awk1669(year) = no_awk1669(year) - no_awk_sy(2, age, year)
          no_awtf1669(year) = no_awtf1669(year) - no_awtf_sy(2, age, year)
          no_ar1669(year) = no_ar1669(year) - no_awk_sy(2, age, year) - no_awtf_sy(2, age, year)
        end do
      end do
      
      ! find the discrepancy between the age-sex numbers in the residual categories assigned so far
      ! and the totals for those categories, and adjust the 16-69 numbers.
      ! Start with workers' family members and keep their distribution as assigned previously, but scaled to match the totals.
      ! Then assign residual numbers by age and sex to workers.
      no_awtf_guess = 0d0
      do year = startYr, MAX_YR
        ! Initialize variables tracking imbalances
        excess_family = 0d0
        do age = 16, 69
          no_awtf_guess(year) = no_awtf_guess(year) + sum(no_awtfa_sy(1:2, age, year)) + sum(no_awtfn_sy(1:2, age, year))
        end do
        ! Allocate family members
        do sex = 1, 2
          do age = 16, 69
            ! note the currently guessed number of family members
            penda = no_awtfa_sy(sex, age, year)
            pendn = no_awtfn_sy(sex, age, year)
            pend = penda + pendn
                if ((penda <= -5e-8).or.(pendn <= -5e-8)) then
                  print*, "NEGATIVE:", sex, age, year, no_awtfa_sy(sex, age, year), no_awtfn_sy(sex, age, year)
                  pause
                end if
            no_awtfa_sy(sex, age, year) = penda * no_awtf1669(year) / no_awtf_guess(year)
            no_awtfn_sy(sex, age, year) = pendn * no_awtf1669(year) / no_awtf_guess(year)
            no_awtf_sy(sex, age, year) = no_awtfn_sy(sex, age, year) + no_awtfa_sy(sex, age, year) !***FIX?***
            if (no_awtf_sy(sex, age, year) > no_ar(sex, age, year)) then
              !! Prevent negative workers; keep track of excess family members
              excess_family = excess_family + no_awtf_sy(sex, age, year) - no_ar(sex, age, year)
              no_awtf_sy(sex, age, year) = no_ar(sex, age, year)
              no_awtfa_sy(sex, age, year) = no_awtf_sy(sex, age, year) * penda / pend
              no_awtfn_sy(sex, age, year) = no_awtf_sy(sex, age, year) * pendn / pend
            end if
            no_ar(sex, age, year) = no_ar(sex, age, year) - no_awtf_sy(sex, age, year)
            no_ar1669(year) = no_ar1669(year) - no_awtf_sy(sex, age, year)
          end do
        end do
        ! Fix any imbalances
        if (excess_family > 5d-7) then ! ignore imbalances that amount to less than half a person
          do sex = 1, 2
            do age = 16, 69
              wtf_add = excess_family * no_ar(sex, age, year) / no_ar1669(year)
              no_ar(sex, age, year) = no_ar(sex, age, year) - wtf_add
              if (no_awtf_sy(sex, age, year) > 1d-9) then
                penda = no_awtfa_sy(sex, age, year) / no_awtf_sy(sex, age, year)
                pendn = no_awtfn_sy(sex, age, year) / no_awtf_sy(sex, age, year)
              else
                penda = 0d0
                pendn = 0d0
              end if
              no_awtf_sy(sex, age, year) = no_awtf_sy(sex, age, year) + wtf_add
              no_awtfa_sy(sex, age, year) = no_awtf_sy(sex, age, year) * penda
              no_awtfn_sy(sex, age, year) = no_awtf_sy(sex, age, year) * pendn
            end do
          end do
        end if
        ! Allocate workers
        do sex = 1, 2
          do age = 16, 69
            no_awh_sy(sex, age, year) = no_ar(sex, age, year) * no_awh(0, year) / no_awk(year)
            no_awt_sy(sex, age, year) = no_ar(sex, age, year) - no_awh_sy(sex, age, year)
          end do
        end do
      end do
      
   end subroutine Allocate70o
  
!===============================================================================
      
   subroutine DisaggregatePopulationComponents()
   
      integer :: age, sex
   
      w_no_asf1_sy(1,0:100,startYr) = (/ 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.011245d0, 0.01541d0, 0.021253d0, 0.028034d0, 0.035401d0, 0.041691d0, 0.045151d0, 0.044726d0, 0.041355d0, 0.036996d0, 0.032949d0, 0.02887d0, 0.025185d0, 0.021837d0, 0.018244d0, 0.014481d0, 0.011314d0, 0.008987d0, 0.007341d0, 0.005928d0, 0.004578d0, 0.003539d0, 0.002807d0, 0.002326d0, 0.00206d0, 0.001906d0, 0.001752d0, 0.001527d0, 0.001258d0, 0.001045d0, 0.000921d0, 0.000813d0, 0.000712d0, 0.000619d0, 0.000546d0, 0.000491d0, 0.000439d0, 0.000384d0, 0.000328d0, 0.000277d0, 0.000235d0, 0.000202d0, 0.00018d0, 0.000165d0, 0.000153d0, 0.00014d0, 0.000122d0, 0.000096d0, 0.000056d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_asf1_sy(2,0:100,startYr) = (/ 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.008092d0, 0.01185d0, 0.01787d0, 0.025233d0, 0.033376d0, 0.040284d0, 0.043993d0, 0.043271d0, 0.039227d0, 0.034078d0, 0.029428d0, 0.025044d0, 0.021486d0, 0.018587d0, 0.015522d0, 0.012212d0, 0.009475d0, 0.007513d0, 0.006174d0, 0.005085d0, 0.004064d0, 0.003257d0, 0.002629d0, 0.002154d0, 0.001852d0, 0.001669d0, 0.001502d0, 0.001298d0, 0.001075d0, 0.000904d0, 0.000801d0, 0.000708d0, 0.000614d0, 0.000522d0, 0.000445d0, 0.000388d0, 0.000339d0, 0.000298d0, 0.000262d0, 0.000232d0, 0.000207d0, 0.000186d0, 0.000168d0, 0.00015d0, 0.000132d0, 0.000112d0, 0.000087d0, 0.000056d0, 0.000017d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_asf2_sy(2,0:100,startYr) = (/ 0d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.042099417d0, 0.045593312d0, 0.045164149d0, 0.041760125d0, 0.037358423d0, 0.033271778d0, 0.029152819d0, 0.025431719d0, 0.022050922d0, 0.018422723d0, 0.01462286d0, 0.011424835d0, 0.009075039d0, 0.007412915d0, 0.005986072d0, 0.004622847d0, 0.003573669d0, 0.002834498d0, 0.002348786d0, 0.00208018d0, 0.001924672d0, 0.001769163d0, 0.001541959d0, 0.001270324d0, 0.001055237d0, 0.000930022d0, 0.000820964d0, 0.000718975d0, 0.000625064d0, 0.000551349d0, 0.00049581d0, 0.000443301d0, 0.000387762d0, 0.000331213d0, 0.000279714d0, 0.000237302d0, 0.000203979d0, 0.000181763d0, 0.000166616d0, 0.000154499d0, 0.000141371d0, 0.000123195d0, 9.69404d-05, 5.65486d-05, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_asf2_sy(1,0:100,startYr) = (/ 0d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.040678634d0, 0.044423968d0, 0.043694895d0, 0.039611279d0, 0.034411838d0, 0.029716285d0, 0.025289338d0, 0.021696483d0, 0.018769084d0, 0.015674058d0, 0.012331632d0, 0.00956782d0, 0.007586599d0, 0.006234482d0, 0.005134814d0, 0.004103812d0, 0.003288907d0, 0.002654754d0, 0.002175101d0, 0.001870143d0, 0.00168535d0, 0.001516714d0, 0.001310716d0, 0.001085531d0, 0.000912856d0, 0.000808847d0, 0.000714936d0, 0.000620015d0, 0.000527114d0, 0.000449359d0, 0.000391801d0, 0.000342321d0, 0.000300919d0, 0.000264567d0, 0.000234273d0, 0.000209028d0, 0.000187822d0, 0.000169646d0, 0.000151469d0, 0.000133293d0, 0.000113097d0, 8.78523d-05, 5.65486d-05, 1.71665d-05, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_asj1_sy(1,0:100,startYr) = (/ 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.011245d0, 0.01541d0, 0.021253d0, 0.028034d0, 0.035401d0, 0.041691d0, 0.045151d0, 0.044726d0, 0.041355d0, 0.036996d0, 0.032949d0, 0.02887d0, 0.025185d0, 0.021837d0, 0.018244d0, 0.014481d0, 0.011314d0, 0.008987d0, 0.007341d0, 0.005928d0, 0.004578d0, 0.003539d0, 0.002807d0, 0.002326d0, 0.00206d0, 0.001906d0, 0.001752d0, 0.001527d0, 0.001258d0, 0.001045d0, 0.000921d0, 0.000813d0, 0.000712d0, 0.000619d0, 0.000546d0, 0.000491d0, 0.000439d0, 0.000384d0, 0.000328d0, 0.000277d0, 0.000235d0, 0.000202d0, 0.00018d0, 0.000165d0, 0.000153d0, 0.00014d0, 0.000122d0, 0.000096d0, 0.000056d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_asj1_sy(2,0:100,startYr) = (/ 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.008092d0, 0.01185d0, 0.01787d0, 0.025233d0, 0.033376d0, 0.040284d0, 0.043993d0, 0.043271d0, 0.039227d0, 0.034078d0, 0.029428d0, 0.025044d0, 0.021486d0, 0.018587d0, 0.015522d0, 0.012212d0, 0.009475d0, 0.007513d0, 0.006174d0, 0.005085d0, 0.004064d0, 0.003257d0, 0.002629d0, 0.002154d0, 0.001852d0, 0.001669d0, 0.001502d0, 0.001298d0, 0.001075d0, 0.000904d0, 0.000801d0, 0.000708d0, 0.000614d0, 0.000522d0, 0.000445d0, 0.000388d0, 0.000339d0, 0.000298d0, 0.000262d0, 0.000232d0, 0.000207d0, 0.000186d0, 0.000168d0, 0.00015d0, 0.000132d0, 0.000112d0, 0.000087d0, 0.000056d0, 0.000017d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_asj2_sy(2,0:100,startYr) = (/ 0d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.042099417d0, 0.045593312d0, 0.045164149d0, 0.041760125d0, 0.037358423d0, 0.033271778d0, 0.029152819d0, 0.025431719d0, 0.022050922d0, 0.018422723d0, 0.01462286d0, 0.011424835d0, 0.009075039d0, 0.007412915d0, 0.005986072d0, 0.004622847d0, 0.003573669d0, 0.002834498d0, 0.002348786d0, 0.00208018d0, 0.001924672d0, 0.001769163d0, 0.001541959d0, 0.001270324d0, 0.001055237d0, 0.000930022d0, 0.000820964d0, 0.000718975d0, 0.000625064d0, 0.000551349d0, 0.00049581d0, 0.000443301d0, 0.000387762d0, 0.000331213d0, 0.000279714d0, 0.000237302d0, 0.000203979d0, 0.000181763d0, 0.000166616d0, 0.000154499d0, 0.000141371d0, 0.000123195d0, 9.69404d-05, 5.65486d-05, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_asj2_sy(1,0:100,startYr) = (/ 0d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.040678634d0, 0.044423968d0, 0.043694895d0, 0.039611279d0, 0.034411838d0, 0.029716285d0, 0.025289338d0, 0.021696483d0, 0.018769084d0, 0.015674058d0, 0.012331632d0, 0.00956782d0, 0.007586599d0, 0.006234482d0, 0.005134814d0, 0.004103812d0, 0.003288907d0, 0.002654754d0, 0.002175101d0, 0.001870143d0, 0.00168535d0, 0.001516714d0, 0.001310716d0, 0.001085531d0, 0.000912856d0, 0.000808847d0, 0.000714936d0, 0.000620015d0, 0.000527114d0, 0.000449359d0, 0.000391801d0, 0.000342321d0, 0.000300919d0, 0.000264567d0, 0.000234273d0, 0.000209028d0, 0.000187822d0, 0.000169646d0, 0.000151469d0, 0.000133293d0, 0.000113097d0, 8.78523d-05, 5.65486d-05, 1.71665d-05, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awj_sy(1,0:100,startYr)  = (/ 0.000000d0,  0.000000d0,  0.000000d0,  0.000429d0,  0.000737d0,  0.000296d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000692d0,  0.001351d0,  0.002011d0,  0.004967d0,  0.010952d0,  0.018833d0,  0.027863d0,  0.035529d0,  0.039473d0,  0.038199d0,  0.033143d0,  0.026652d0,  0.021139d0,  0.017199d0,  0.015872d0,  0.016427d0,  0.017310d0,  0.017404d0,  0.016970d0,  0.015681d0,  0.013792d0,  0.011829d0,  0.010167d0,  0.008725d0,  0.007620d0,  0.006791d0,  0.006015d0,  0.005253d0,  0.004682d0,  0.004331d0,  0.004142d0,  0.004071d0,  0.004003d0,  0.003838d0,  0.003508d0,  0.003063d0,  0.002611d0,  0.002229d0,  0.001905d0,  0.001663d0,  0.001487d0,  0.001338d0,  0.001193d0,  0.001062d0,  0.000938d0,  0.000827d0,  0.000734d0,  0.000667d0,  0.000629d0,  0.000620d0,  0.000636d0,  0.000675d0,  0.000733d0,  0.000804d0,  0.000884d0,  0.000969d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awj_sy(2,0:100,startYr) = (/ 0.000000d0,  0.000000d0,  0.000000d0,  0.000352d0,  0.000687d0,  0.000263d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000887d0,  0.003835d0,  0.005442d0,  0.009697d0,  0.017413d0,  0.027193d0,  0.038419d0,  0.047811d0,  0.051936d0,  0.048769d0,  0.040346d0,  0.029911d0,  0.020907d0,  0.014314d0,  0.011662d0,  0.011909d0,  0.012693d0,  0.012509d0,  0.011982d0,  0.010734d0,  0.009000d0,  0.007330d0,  0.006064d0,  0.005030d0,  0.004302d0,  0.003817d0,  0.003435d0,  0.003074d0,  0.002772d0,  0.002506d0,  0.002271d0,  0.002100d0,  0.001969d0,  0.001812d0,  0.001602d0,  0.001358d0,  0.001118d0,  0.000916d0,  0.000753d0,  0.000642d0,  0.000569d0,  0.000518d0,  0.000470d0,  0.000419d0,  0.000359d0,  0.000294d0,  0.000235d0,  0.000192d0,  0.000169d0,  0.000166d0,  0.000180d0,  0.000206d0,  0.000239d0,  0.000271d0,  0.000297d0,  0.000313d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awjf_sy(1,0:100,startYr)  = (/ 0.0d0,  0.015220d0,  0.015917d0,  0.016377d0,  0.016604d0,  0.013741d0,  0.013504d0,  0.013043d0,  0.012370d0,  0.011515d0,  0.010532d0,  0.009489d0,  0.008465d0,  0.007513d0,  0.006627d0,  0.005771d0,  0.004921d0,  0.004103d0,  0.003321d0,  0.002606d0,  0.001962d0,  0.001451d0,  0.001152d0,  0.001110d0,  0.001299d0,  0.001508d0,  0.001812d0,  0.002500d0,  0.003658d0,  0.005113d0,  0.006744d0,  0.008192d0,  0.009126d0,  0.009326d0,  0.008970d0,  0.008427d0,  0.007951d0,  0.007477d0,  0.007083d0,  0.006740d0,  0.006360d0,  0.005898d0,  0.005401d0,  0.004869d0,  0.004324d0,  0.003755d0,  0.003216d0,  0.002787d0,  0.002509d0,  0.002342d0,  0.002227d0,  0.002099d0,  0.001939d0,  0.001720d0,  0.001462d0,  0.001211d0,  0.000999d0,  0.000818d0,  0.000680d0,  0.000577d0,  0.000499d0,  0.000437d0,  0.000386d0,  0.000346d0,  0.000322d0,  0.000320d0,  0.000348d0,  0.000414d0,  0.000525d0,  0.000688d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awjf_sy(2,0:100,startYr) = (/ 0.0d0,  0.014488d0,  0.015409d0,  0.015979d0,  0.016235d0,  0.013576d0,  0.013312d0,  0.012841d0,  0.012199d0,  0.011419d0,  0.010532d0,  0.009564d0,  0.008546d0,  0.007505d0,  0.006480d0,  0.005638d0,  0.004919d0,  0.004128d0,  0.003221d0,  0.002385d0,  0.001399d0,  0.000902d0,  0.001891d0,  0.004828d0,  0.009138d0,  0.013862d0,  0.018129d0,  0.021779d0,  0.024417d0,  0.026157d0,  0.027803d0,  0.029224d0,  0.029421d0,  0.028104d0,  0.025661d0,  0.022683d0,  0.019793d0,  0.017205d0,  0.015222d0,  0.013719d0,  0.012192d0,  0.010559d0,  0.009253d0,  0.008357d0,  0.007769d0,  0.007369d0,  0.006960d0,  0.006420d0,  0.005647d0,  0.004723d0,  0.003802d0,  0.003022d0,  0.002393d0,  0.001968d0,  0.001704d0,  0.001505d0,  0.001320d0,  0.001184d0,  0.001085d0,  0.001018d0,  0.000978d0,  0.000963d0,  0.000965d0,  0.000978d0,  0.000991d0,  0.000995d0,  0.000978d0,  0.000930d0,  0.000842d0,  0.000701d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awh_sy(1,0:100,startYr) = (/ 0.000000d0,  0.000000d0,  0.000209d0,  0.000322d0,  0.000333d0,  0.000284d0,  0.000182d0,  0.000102d0,  0.000072d0,  0.000101d0,  0.000170d0,  0.000241d0,  0.000266d0,  0.000222d0,  0.000165d0,  0.000210d0,  0.000409d0,  0.000725d0,  0.001179d0,  0.001814d0,  0.002439d0,  0.003298d0,  0.004868d0,  0.007346d0,  0.010460d0,  0.013812d0,  0.016960d0,  0.019733d0,  0.021910d0,  0.023587d0,  0.025100d0,  0.026543d0,  0.027667d0,  0.028440d0,  0.028894d0,  0.029137d0,  0.029181d0,  0.028939d0,  0.028400d0,  0.027614d0,  0.026640d0,  0.025561d0,  0.024429d0,  0.023295d0,  0.022151d0,  0.020950d0,  0.019679d0,  0.018372d0,  0.017040d0,  0.015697d0,  0.014355d0,  0.013034d0,  0.011757d0,  0.010543d0,  0.009393d0,  0.008285d0,  0.007235d0,  0.006298d0,  0.005495d0,  0.004809d0,  0.004211d0,  0.003675d0,  0.003188d0,  0.002746d0,  0.002361d0,  0.002043d0,  0.001809d0,  0.001676d0,  0.001658d0,  0.001770d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awh_sy(2,0:100,startYr) = (/ 0.000000d0,  0.000046d0,  0.000112d0,  0.000145d0,  0.000156d0,  0.000154d0,  0.000143d0,  0.000138d0,  0.000144d0,  0.000163d0,  0.000189d0,  0.000214d0,  0.000226d0,  0.000219d0,  0.000204d0,  0.000264d0,  0.000367d0,  0.000408d0,  0.000356d0,  0.000288d0,  0.000136d0,  0.000147d0,  0.000686d0,  0.001925d0,  0.003648d0,  0.005555d0,  0.007268d0,  0.008608d0,  0.009386d0,  0.009720d0,  0.009949d0,  0.010203d0,  0.010295d0,  0.010228d0,  0.010040d0,  0.009720d0,  0.009362d0,  0.009098d0,  0.008996d0,  0.008998d0,  0.009009d0,  0.008926d0,  0.008720d0,  0.008347d0,  0.007848d0,  0.007313d0,  0.006792d0,  0.006254d0,  0.005713d0,  0.005178d0,  0.004622d0,  0.004080d0,  0.003623d0,  0.003282d0,  0.003028d0,  0.002821d0,  0.002605d0,  0.002352d0,  0.002035d0,  0.001679d0,  0.001330d0,  0.001025d0,  0.000787d0,  0.000622d0,  0.000520d0,  0.000466d0,  0.000441d0,  0.000427d0,  0.000404d0,  0.000354d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awt_sy(1,0:100,startYr) = (/ 0.000000d0,  0.000000d0,  0.000209d0,  0.000322d0,  0.000333d0,  0.000284d0,  0.000182d0,  0.000102d0,  0.000072d0,  0.000101d0,  0.000170d0,  0.000241d0,  0.000266d0,  0.000222d0,  0.000165d0,  0.000210d0,  0.000409d0,  0.000725d0,  0.001179d0,  0.001814d0,  0.002439d0,  0.003298d0,  0.004868d0,  0.007346d0,  0.010460d0,  0.013812d0,  0.016960d0,  0.019733d0,  0.021910d0,  0.023587d0,  0.025100d0,  0.026543d0,  0.027667d0,  0.028440d0,  0.028894d0,  0.029137d0,  0.029181d0,  0.028939d0,  0.028400d0,  0.027614d0,  0.026640d0,  0.025561d0,  0.024429d0,  0.023295d0,  0.022151d0,  0.020950d0,  0.019679d0,  0.018372d0,  0.017040d0,  0.015697d0,  0.014355d0,  0.013034d0,  0.011757d0,  0.010543d0,  0.009393d0,  0.008285d0,  0.007235d0,  0.006298d0,  0.005495d0,  0.004809d0,  0.004211d0,  0.003675d0,  0.003188d0,  0.002746d0,  0.002361d0,  0.002043d0,  0.001809d0,  0.001676d0,  0.001658d0,  0.001770d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awt_sy(2,0:100,startYr) = (/ 0.000000d0,  0.000046d0,  0.000112d0,  0.000145d0,  0.000156d0,  0.000154d0,  0.000143d0,  0.000138d0,  0.000144d0,  0.000163d0,  0.000189d0,  0.000214d0,  0.000226d0,  0.000219d0,  0.000204d0,  0.000264d0,  0.000367d0,  0.000408d0,  0.000356d0,  0.000288d0,  0.000136d0,  0.000147d0,  0.000686d0,  0.001925d0,  0.003648d0,  0.005555d0,  0.007268d0,  0.008608d0,  0.009386d0,  0.009720d0,  0.009949d0,  0.010203d0,  0.010295d0,  0.010228d0,  0.010040d0,  0.009720d0,  0.009362d0,  0.009098d0,  0.008996d0,  0.008998d0,  0.009009d0,  0.008926d0,  0.008720d0,  0.008347d0,  0.007848d0,  0.007313d0,  0.006792d0,  0.006254d0,  0.005713d0,  0.005178d0,  0.004622d0,  0.004080d0,  0.003623d0,  0.003282d0,  0.003028d0,  0.002821d0,  0.002605d0,  0.002352d0,  0.002035d0,  0.001679d0,  0.001330d0,  0.001025d0,  0.000787d0,  0.000622d0,  0.000520d0,  0.000466d0,  0.000441d0,  0.000427d0,  0.000404d0,  0.000354d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awtfa_sy(1,0:100,startYr) = (/ 0.0d0,  0.010733d0,  0.011779d0,  0.012687d0,  0.013424d0,  0.012035d0,  0.012317d0,  0.012327d0,  0.012052d0,  0.011523d0,  0.010813d0,  0.010037d0,  0.009326d0,  0.008762d0,  0.008288d0,  0.007866d0,  0.007341d0,  0.006581d0,  0.005507d0,  0.004243d0,  0.002911d0,  0.001754d0,  0.000955d0,  0.000652d0,  0.000739d0,  0.000938d0,  0.001137d0,  0.001475d0,  0.001938d0,  0.002484d0,  0.003108d0,  0.003718d0,  0.004196d0,  0.004471d0,  0.004581d0,  0.004657d0,  0.004724d0,  0.004683d0,  0.004517d0,  0.004260d0,  0.003940d0,  0.003617d0,  0.003341d0,  0.003149d0,  0.003017d0,  0.002891d0,  0.002737d0,  0.002571d0,  0.002385d0,  0.002184d0,  0.001990d0,  0.001805d0,  0.001613d0,  0.001414d0,  0.001215d0,  0.001014d0,  0.000830d0,  0.000690d0,  0.000607d0,  0.000567d0,  0.000548d0,  0.000529d0,  0.000499d0,  0.000452d0,  0.000396d0,  0.000336d0,  0.000283d0,  0.000245d0,  0.000232d0,  0.000254d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awtfa_sy(2,0:100,startYr) = (/ 0.0d0,  0.010098d0,  0.011364d0,  0.012343d0,  0.013043d0,  0.011764d0,  0.011930d0,  0.011846d0,  0.011529d0,  0.011014d0,  0.010355d0,  0.009620d0,  0.008887d0,  0.008211d0,  0.007593d0,  0.007184d0,  0.006834d0,  0.006246d0,  0.005318d0,  0.004304d0,  0.003005d0,  0.002227d0,  0.003133d0,  0.006281d0,  0.010978d0,  0.016273d0,  0.020954d0,  0.024448d0,  0.026146d0,  0.026441d0,  0.026390d0,  0.026446d0,  0.026080d0,  0.025329d0,  0.024275d0,  0.022907d0,  0.021400d0,  0.020018d0,  0.018901d0,  0.017969d0,  0.017044d0,  0.015992d0,  0.014849d0,  0.013582d0,  0.012240d0,  0.010888d0,  0.009612d0,  0.008453d0,  0.007455d0,  0.006595d0,  0.005789d0,  0.005025d0,  0.004376d0,  0.003855d0,  0.003437d0,  0.003101d0,  0.002797d0,  0.002488d0,  0.002144d0,  0.001786d0,  0.001451d0,  0.001170d0,  0.000960d0,  0.000824d0,  0.000748d0,  0.000714d0,  0.000703d0,  0.000693d0,  0.000664d0,  0.000594d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awtfn_sy(1,0:100,startYr) = (/ 0.0d0,  0.010733d0,  0.011779d0,  0.012687d0,  0.013424d0,  0.012035d0,  0.012317d0,  0.012327d0,  0.012052d0,  0.011523d0,  0.010813d0,  0.010037d0,  0.009326d0,  0.008762d0,  0.008288d0,  0.007866d0,  0.007341d0,  0.006581d0,  0.005507d0,  0.004243d0,  0.002911d0,  0.001754d0,  0.000955d0,  0.000652d0,  0.000739d0,  0.000938d0,  0.001137d0,  0.001475d0,  0.001938d0,  0.002484d0,  0.003108d0,  0.003718d0,  0.004196d0,  0.004471d0,  0.004581d0,  0.004657d0,  0.004724d0,  0.004683d0,  0.004517d0,  0.004260d0,  0.003940d0,  0.003617d0,  0.003341d0,  0.003149d0,  0.003017d0,  0.002891d0,  0.002737d0,  0.002571d0,  0.002385d0,  0.002184d0,  0.001990d0,  0.001805d0,  0.001613d0,  0.001414d0,  0.001215d0,  0.001014d0,  0.000830d0,  0.000690d0,  0.000607d0,  0.000567d0,  0.000548d0,  0.000529d0,  0.000499d0,  0.000452d0,  0.000396d0,  0.000336d0,  0.000283d0,  0.000245d0,  0.000232d0,  0.000254d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_awtfn_sy(2,0:100,startYr) = (/ 0.0d0,  0.010098d0,  0.011364d0,  0.012343d0,  0.013043d0,  0.011764d0,  0.011930d0,  0.011846d0,  0.011529d0,  0.011014d0,  0.010355d0,  0.009620d0,  0.008887d0,  0.008211d0,  0.007593d0,  0.007184d0,  0.006834d0,  0.006246d0,  0.005318d0,  0.004304d0,  0.003005d0,  0.002227d0,  0.003133d0,  0.006281d0,  0.010978d0,  0.016273d0,  0.020954d0,  0.024448d0,  0.026146d0,  0.026441d0,  0.026390d0,  0.026446d0,  0.026080d0,  0.025329d0,  0.024275d0,  0.022907d0,  0.021400d0,  0.020018d0,  0.018901d0,  0.017969d0,  0.017044d0,  0.015992d0,  0.014849d0,  0.013582d0,  0.012240d0,  0.010888d0,  0.009612d0,  0.008453d0,  0.007455d0,  0.006595d0,  0.005789d0,  0.005025d0,  0.004376d0,  0.003855d0,  0.003437d0,  0.003101d0,  0.002797d0,  0.002488d0,  0.002144d0,  0.001786d0,  0.001451d0,  0.001170d0,  0.000960d0,  0.000824d0,  0.000748d0,  0.000714d0,  0.000703d0,  0.000693d0,  0.000664d0,  0.000594d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nasf1_sy(1,0:100,startYr) = (/ 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.002251d0, 0.005336d0, 0.009591d0, 0.015204d0, 0.022291d0, 0.028386d0, 0.03434d0, 0.03904d0, 0.041707d0, 0.042026d0, 0.040276d0, 0.037016d0, 0.033104d0, 0.029196d0, 0.025442d0, 0.021745d0, 0.01823d0, 0.014988d0, 0.012085d0, 0.00962d0, 0.007637d0, 0.006081d0, 0.004843d0, 0.003839d0, 0.003065d0, 0.00253d0, 0.002172d0, 0.001916d0, 0.001702d0, 0.001499d0, 0.001302d0, 0.001114d0, 0.000951d0, 0.000823d0, 0.000723d0, 0.000637d0, 0.000562d0, 0.000496d0, 0.000438d0, 0.000384d0, 0.000333d0, 0.000285d0, 0.000245d0, 0.000212d0, 0.000187d0, 0.000168d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nasf1_sy(2,0:100,startYr) = (/ 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.00162d0, 0.003992d0, 0.00757d0, 0.012622d0, 0.019303d0, 0.025748d0, 0.032184d0, 0.037269d0, 0.04007d0, 0.040211d0, 0.038038d0, 0.034244d0, 0.029883d0, 0.02575d0, 0.022035d0, 0.018589d0, 0.015472d0, 0.012675d0, 0.010189d0, 0.0081d0, 0.006469d0, 0.005224d0, 0.004246d0, 0.003441d0, 0.002794d0, 0.002314d0, 0.001963d0, 0.001697d0, 0.001481d0, 0.001291d0, 0.001117d0, 0.000958d0, 0.000821d0, 0.00071d0, 0.000618d0, 0.000536d0, 0.000462d0, 0.000399d0, 0.000347d0, 0.000304d0, 0.000268d0, 0.000237d0, 0.000211d0, 0.000189d0, 0.000169d0, 0.00015d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nasf2_sy(2,0:100,startYr) = (/ 0d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.007775444d0, 0.012325914d0, 0.01807136d0, 0.023012589d0, 0.027839509d0, 0.031649809d0, 0.033811952d0, 0.034070566d0, 0.032651837d0, 0.030008948d0, 0.026837482d0, 0.023669258d0, 0.020625882d0, 0.017628717d0, 0.014779099d0, 0.012150803d0, 0.009797335d0, 0.007798954d0, 0.006191332d0, 0.004929879d0, 0.00392623d0, 0.003112285d0, 0.002484802d0, 0.002051076d0, 0.001760845d0, 0.001553305d0, 0.001379815d0, 0.001215242d0, 0.001055534d0, 0.000903122d0, 0.000770978d0, 0.000667208d0, 0.000586138d0, 0.000516417d0, 0.000455615d0, 0.000402108d0, 0.000355088d0, 0.00031131d0, 0.000269964d0, 0.00023105d0, 0.000198622d0, 0.000171869d0, 0.000151601d0, 0.000136198d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nasf2_sy(1,0:100,startYr) = (/ 0d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.006137015d0, 0.010232682d0, 0.015648982d0, 0.020873957d0, 0.026091636d0, 0.030214056d0, 0.032484832d0, 0.032599141d0, 0.030837486d0, 0.027761682d0, 0.02422621d0, 0.020875579d0, 0.01786382d0, 0.015070141d0, 0.012543183d0, 0.010275649d0, 0.008260243d0, 0.006566687d0, 0.005244432d0, 0.004235108d0, 0.003442241d0, 0.002789626d0, 0.002265102d0, 0.001875965d0, 0.001591408d0, 0.001375761d0, 0.00120065d0, 0.001046616d0, 0.000905554d0, 0.000776653d0, 0.000665586d0, 0.000575598d0, 0.000501014d0, 0.000434536d0, 0.000374544d0, 0.00032347d0, 0.000281314d0, 0.000246453d0, 0.000217268d0, 0.000192136d0, 0.000171058d0, 0.000153223d0, 0.000137009d0, 0.000121605d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nasj1_sy(1,0:100,startYr) = (/ 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.002251d0, 0.005336d0, 0.009591d0, 0.015204d0, 0.022291d0, 0.028386d0, 0.03434d0, 0.03904d0, 0.041707d0, 0.042026d0, 0.040276d0, 0.037016d0, 0.033104d0, 0.029196d0, 0.025442d0, 0.021745d0, 0.01823d0, 0.014988d0, 0.012085d0, 0.00962d0, 0.007637d0, 0.006081d0, 0.004843d0, 0.003839d0, 0.003065d0, 0.00253d0, 0.002172d0, 0.001916d0, 0.001702d0, 0.001499d0, 0.001302d0, 0.001114d0, 0.000951d0, 0.000823d0, 0.000723d0, 0.000637d0, 0.000562d0, 0.000496d0, 0.000438d0, 0.000384d0, 0.000333d0, 0.000285d0, 0.000245d0, 0.000212d0, 0.000187d0, 0.000168d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nasj1_sy(2,0:100,startYr) = (/ 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.00162d0, 0.003992d0, 0.00757d0, 0.012622d0, 0.019303d0, 0.025748d0, 0.032184d0, 0.037269d0, 0.04007d0, 0.040211d0, 0.038038d0, 0.034244d0, 0.029883d0, 0.02575d0, 0.022035d0, 0.018589d0, 0.015472d0, 0.012675d0, 0.010189d0, 0.0081d0, 0.006469d0, 0.005224d0, 0.004246d0, 0.003441d0, 0.002794d0, 0.002314d0, 0.001963d0, 0.001697d0, 0.001481d0, 0.001291d0, 0.001117d0, 0.000958d0, 0.000821d0, 0.00071d0, 0.000618d0, 0.000536d0, 0.000462d0, 0.000399d0, 0.000347d0, 0.000304d0, 0.000268d0, 0.000237d0, 0.000211d0, 0.000189d0, 0.000169d0, 0.00015d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nasj2_sy(2,0:100,startYr) = (/ 0d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.007775444d0, 0.012325914d0, 0.01807136d0, 0.023012589d0, 0.027839509d0, 0.031649809d0, 0.033811952d0, 0.034070566d0, 0.032651837d0, 0.030008948d0, 0.026837482d0, 0.023669258d0, 0.020625882d0, 0.017628717d0, 0.014779099d0, 0.012150803d0, 0.009797335d0, 0.007798954d0, 0.006191332d0, 0.004929879d0, 0.00392623d0, 0.003112285d0, 0.002484802d0, 0.002051076d0, 0.001760845d0, 0.001553305d0, 0.001379815d0, 0.001215242d0, 0.001055534d0, 0.000903122d0, 0.000770978d0, 0.000667208d0, 0.000586138d0, 0.000516417d0, 0.000455615d0, 0.000402108d0, 0.000355088d0, 0.00031131d0, 0.000269964d0, 0.00023105d0, 0.000198622d0, 0.000171869d0, 0.000151601d0, 0.000136198d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nasj2_sy(1,0:100,startYr) = (/ 0d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0.01d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0.006137015d0, 0.010232682d0, 0.015648982d0, 0.020873957d0, 0.026091636d0, 0.030214056d0, 0.032484832d0, 0.032599141d0, 0.030837486d0, 0.027761682d0, 0.02422621d0, 0.020875579d0, 0.01786382d0, 0.015070141d0, 0.012543183d0, 0.010275649d0, 0.008260243d0, 0.006566687d0, 0.005244432d0, 0.004235108d0, 0.003442241d0, 0.002789626d0, 0.002265102d0, 0.001875965d0, 0.001591408d0, 0.001375761d0, 0.00120065d0, 0.001046616d0, 0.000905554d0, 0.000776653d0, 0.000665586d0, 0.000575598d0, 0.000501014d0, 0.000434536d0, 0.000374544d0, 0.00032347d0, 0.000281314d0, 0.000246453d0, 0.000217268d0, 0.000192136d0, 0.000171058d0, 0.000153223d0, 0.000137009d0, 0.000121605d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawj_sy(1,0:100,startYr) = (/ 0.000000d0,  0.000000d0,  0.000000d0,  0.000429d0,  0.000737d0,  0.000296d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000692d0,  0.001351d0,  0.002011d0,  0.004967d0,  0.010952d0,  0.018833d0,  0.027863d0,  0.035529d0,  0.039473d0,  0.038199d0,  0.033143d0,  0.026652d0,  0.021139d0,  0.017199d0,  0.015872d0,  0.016427d0,  0.017310d0,  0.017404d0,  0.016970d0,  0.015681d0,  0.013792d0,  0.011829d0,  0.010167d0,  0.008725d0,  0.007620d0,  0.006791d0,  0.006015d0,  0.005253d0,  0.004682d0,  0.004331d0,  0.004142d0,  0.004071d0,  0.004003d0,  0.003838d0,  0.003508d0,  0.003063d0,  0.002611d0,  0.002229d0,  0.001905d0,  0.001663d0,  0.001487d0,  0.001338d0,  0.001193d0,  0.001062d0,  0.000938d0,  0.000827d0,  0.000734d0,  0.000667d0,  0.000629d0,  0.000620d0,  0.000636d0,  0.000675d0,  0.000733d0,  0.000804d0,  0.000884d0,  0.000969d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawj_sy(2,0:100,startYr) = (/ 0.000000d0,  0.000000d0,  0.000000d0,  0.000352d0,  0.000687d0,  0.000263d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000000d0,  0.000887d0,  0.003835d0,  0.005442d0,  0.009697d0,  0.017413d0,  0.027193d0,  0.038419d0,  0.047811d0,  0.051936d0,  0.048769d0,  0.040346d0,  0.029911d0,  0.020907d0,  0.014314d0,  0.011662d0,  0.011909d0,  0.012693d0,  0.012509d0,  0.011982d0,  0.010734d0,  0.009000d0,  0.007330d0,  0.006064d0,  0.005030d0,  0.004302d0,  0.003817d0,  0.003435d0,  0.003074d0,  0.002772d0,  0.002506d0,  0.002271d0,  0.002100d0,  0.001969d0,  0.001812d0,  0.001602d0,  0.001358d0,  0.001118d0,  0.000916d0,  0.000753d0,  0.000642d0,  0.000569d0,  0.000518d0,  0.000470d0,  0.000419d0,  0.000359d0,  0.000294d0,  0.000235d0,  0.000192d0,  0.000169d0,  0.000166d0,  0.000180d0,  0.000206d0,  0.000239d0,  0.000271d0,  0.000297d0,  0.000313d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawjf_sy(1,0:100,startYr) = (/ 0.0d0,  0.015220d0,  0.015917d0,  0.016377d0,  0.016604d0,  0.013741d0,  0.013504d0,  0.013043d0,  0.012370d0,  0.011515d0,  0.010532d0,  0.009489d0,  0.008465d0,  0.007513d0,  0.006627d0,  0.005771d0,  0.004921d0,  0.004103d0,  0.003321d0,  0.002606d0,  0.001962d0,  0.001451d0,  0.001152d0,  0.001110d0,  0.001299d0,  0.001508d0,  0.001812d0,  0.002500d0,  0.003658d0,  0.005113d0,  0.006744d0,  0.008192d0,  0.009126d0,  0.009326d0,  0.008970d0,  0.008427d0,  0.007951d0,  0.007477d0,  0.007083d0,  0.006740d0,  0.006360d0,  0.005898d0,  0.005401d0,  0.004869d0,  0.004324d0,  0.003755d0,  0.003216d0,  0.002787d0,  0.002509d0,  0.002342d0,  0.002227d0,  0.002099d0,  0.001939d0,  0.001720d0,  0.001462d0,  0.001211d0,  0.000999d0,  0.000818d0,  0.000680d0,  0.000577d0,  0.000499d0,  0.000437d0,  0.000386d0,  0.000346d0,  0.000322d0,  0.000320d0,  0.000348d0,  0.000414d0,  0.000525d0,  0.000688d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawjf_sy(2,0:100,startYr) = (/ 0.0d0,  0.014488d0,  0.015409d0,  0.015979d0,  0.016235d0,  0.013576d0,  0.013312d0,  0.012841d0,  0.012199d0,  0.011419d0,  0.010532d0,  0.009564d0,  0.008546d0,  0.007505d0,  0.006480d0,  0.005638d0,  0.004919d0,  0.004128d0,  0.003221d0,  0.002385d0,  0.001399d0,  0.000902d0,  0.001891d0,  0.004828d0,  0.009138d0,  0.013862d0,  0.018129d0,  0.021779d0,  0.024417d0,  0.026157d0,  0.027803d0,  0.029224d0,  0.029421d0,  0.028104d0,  0.025661d0,  0.022683d0,  0.019793d0,  0.017205d0,  0.015222d0,  0.013719d0,  0.012192d0,  0.010559d0,  0.009253d0,  0.008357d0,  0.007769d0,  0.007369d0,  0.006960d0,  0.006420d0,  0.005647d0,  0.004723d0,  0.003802d0,  0.003022d0,  0.002393d0,  0.001968d0,  0.001704d0,  0.001505d0,  0.001320d0,  0.001184d0,  0.001085d0,  0.001018d0,  0.000978d0,  0.000963d0,  0.000965d0,  0.000978d0,  0.000991d0,  0.000995d0,  0.000978d0,  0.000930d0,  0.000842d0,  0.000701d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawh_sy(1,0:100,startYr) = (/ 0.000000d0,  0.000000d0,  0.000209d0,  0.000322d0,  0.000333d0,  0.000284d0,  0.000182d0,  0.000102d0,  0.000072d0,  0.000101d0,  0.000170d0,  0.000241d0,  0.000266d0,  0.000222d0,  0.000165d0,  0.000210d0,  0.000409d0,  0.000725d0,  0.001179d0,  0.001814d0,  0.002439d0,  0.003298d0,  0.004868d0,  0.007346d0,  0.010460d0,  0.013812d0,  0.016960d0,  0.019733d0,  0.021910d0,  0.023587d0,  0.025100d0,  0.026543d0,  0.027667d0,  0.028440d0,  0.028894d0,  0.029137d0,  0.029181d0,  0.028939d0,  0.028400d0,  0.027614d0,  0.026640d0,  0.025561d0,  0.024429d0,  0.023295d0,  0.022151d0,  0.020950d0,  0.019679d0,  0.018372d0,  0.017040d0,  0.015697d0,  0.014355d0,  0.013034d0,  0.011757d0,  0.010543d0,  0.009393d0,  0.008285d0,  0.007235d0,  0.006298d0,  0.005495d0,  0.004809d0,  0.004211d0,  0.003675d0,  0.003188d0,  0.002746d0,  0.002361d0,  0.002043d0,  0.001809d0,  0.001676d0,  0.001658d0,  0.001770d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawh_sy(2,0:100,startYr) = (/ 0.000000d0,  0.000046d0,  0.000112d0,  0.000145d0,  0.000156d0,  0.000154d0,  0.000143d0,  0.000138d0,  0.000144d0,  0.000163d0,  0.000189d0,  0.000214d0,  0.000226d0,  0.000219d0,  0.000204d0,  0.000264d0,  0.000367d0,  0.000408d0,  0.000356d0,  0.000288d0,  0.000136d0,  0.000147d0,  0.000686d0,  0.001925d0,  0.003648d0,  0.005555d0,  0.007268d0,  0.008608d0,  0.009386d0,  0.009720d0,  0.009949d0,  0.010203d0,  0.010295d0,  0.010228d0,  0.010040d0,  0.009720d0,  0.009362d0,  0.009098d0,  0.008996d0,  0.008998d0,  0.009009d0,  0.008926d0,  0.008720d0,  0.008347d0,  0.007848d0,  0.007313d0,  0.006792d0,  0.006254d0,  0.005713d0,  0.005178d0,  0.004622d0,  0.004080d0,  0.003623d0,  0.003282d0,  0.003028d0,  0.002821d0,  0.002605d0,  0.002352d0,  0.002035d0,  0.001679d0,  0.001330d0,  0.001025d0,  0.000787d0,  0.000622d0,  0.000520d0,  0.000466d0,  0.000441d0,  0.000427d0,  0.000404d0,  0.000354d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawt_sy(1,0:100,startYr) = (/ 0.000000d0,  0.000000d0,  0.000209d0,  0.000322d0,  0.000333d0,  0.000284d0,  0.000182d0,  0.000102d0,  0.000072d0,  0.000101d0,  0.000170d0,  0.000241d0,  0.000266d0,  0.000222d0,  0.000165d0,  0.000210d0,  0.000409d0,  0.000725d0,  0.001179d0,  0.001814d0,  0.002439d0,  0.003298d0,  0.004868d0,  0.007346d0,  0.010460d0,  0.013812d0,  0.016960d0,  0.019733d0,  0.021910d0,  0.023587d0,  0.025100d0,  0.026543d0,  0.027667d0,  0.028440d0,  0.028894d0,  0.029137d0,  0.029181d0,  0.028939d0,  0.028400d0,  0.027614d0,  0.026640d0,  0.025561d0,  0.024429d0,  0.023295d0,  0.022151d0,  0.020950d0,  0.019679d0,  0.018372d0,  0.017040d0,  0.015697d0,  0.014355d0,  0.013034d0,  0.011757d0,  0.010543d0,  0.009393d0,  0.008285d0,  0.007235d0,  0.006298d0,  0.005495d0,  0.004809d0,  0.004211d0,  0.003675d0,  0.003188d0,  0.002746d0,  0.002361d0,  0.002043d0,  0.001809d0,  0.001676d0,  0.001658d0,  0.001770d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawt_sy(2,0:100,startYr)  = (/ 0.000000d0,  0.000046d0,  0.000112d0,  0.000145d0,  0.000156d0,  0.000154d0,  0.000143d0,  0.000138d0,  0.000144d0,  0.000163d0,  0.000189d0,  0.000214d0,  0.000226d0,  0.000219d0,  0.000204d0,  0.000264d0,  0.000367d0,  0.000408d0,  0.000356d0,  0.000288d0,  0.000136d0,  0.000147d0,  0.000686d0,  0.001925d0,  0.003648d0,  0.005555d0,  0.007268d0,  0.008608d0,  0.009386d0,  0.009720d0,  0.009949d0,  0.010203d0,  0.010295d0,  0.010228d0,  0.010040d0,  0.009720d0,  0.009362d0,  0.009098d0,  0.008996d0,  0.008998d0,  0.009009d0,  0.008926d0,  0.008720d0,  0.008347d0,  0.007848d0,  0.007313d0,  0.006792d0,  0.006254d0,  0.005713d0,  0.005178d0,  0.004622d0,  0.004080d0,  0.003623d0,  0.003282d0,  0.003028d0,  0.002821d0,  0.002605d0,  0.002352d0,  0.002035d0,  0.001679d0,  0.001330d0,  0.001025d0,  0.000787d0,  0.000622d0,  0.000520d0,  0.000466d0,  0.000441d0,  0.000427d0,  0.000404d0,  0.000354d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawtf_sy(1,0:100,startYr)  = (/ 0.0d0,  0.010733d0,  0.011779d0,  0.012687d0,  0.013424d0,  0.012035d0,  0.012317d0,  0.012327d0,  0.012052d0,  0.011523d0,  0.010813d0,  0.010037d0,  0.009326d0,  0.008762d0,  0.008288d0,  0.007866d0,  0.007341d0,  0.006581d0,  0.005507d0,  0.004243d0,  0.002911d0,  0.001754d0,  0.000955d0,  0.000652d0,  0.000739d0,  0.000938d0,  0.001137d0,  0.001475d0,  0.001938d0,  0.002484d0,  0.003108d0,  0.003718d0,  0.004196d0,  0.004471d0,  0.004581d0,  0.004657d0,  0.004724d0,  0.004683d0,  0.004517d0,  0.004260d0,  0.003940d0,  0.003617d0,  0.003341d0,  0.003149d0,  0.003017d0,  0.002891d0,  0.002737d0,  0.002571d0,  0.002385d0,  0.002184d0,  0.001990d0,  0.001805d0,  0.001613d0,  0.001414d0,  0.001215d0,  0.001014d0,  0.000830d0,  0.000690d0,  0.000607d0,  0.000567d0,  0.000548d0,  0.000529d0,  0.000499d0,  0.000452d0,  0.000396d0,  0.000336d0,  0.000283d0,  0.000245d0,  0.000232d0,  0.000254d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      w_no_nawtf_sy(2,0:100,startYr)  = (/ 0.0d0,  0.010098d0,  0.011364d0,  0.012343d0,  0.013043d0,  0.011764d0,  0.011930d0,  0.011846d0,  0.011529d0,  0.011014d0,  0.010355d0,  0.009620d0,  0.008887d0,  0.008211d0,  0.007593d0,  0.007184d0,  0.006834d0,  0.006246d0,  0.005318d0,  0.004304d0,  0.003005d0,  0.002227d0,  0.003133d0,  0.006281d0,  0.010978d0,  0.016273d0,  0.020954d0,  0.024448d0,  0.026146d0,  0.026441d0,  0.026390d0,  0.026446d0,  0.026080d0,  0.025329d0,  0.024275d0,  0.022907d0,  0.021400d0,  0.020018d0,  0.018901d0,  0.017969d0,  0.017044d0,  0.015992d0,  0.014849d0,  0.013582d0,  0.012240d0,  0.010888d0,  0.009612d0,  0.008453d0,  0.007455d0,  0.006595d0,  0.005789d0,  0.005025d0,  0.004376d0,  0.003855d0,  0.003437d0,  0.003101d0,  0.002797d0,  0.002488d0,  0.002144d0,  0.001786d0,  0.001451d0,  0.001170d0,  0.000960d0,  0.000824d0,  0.000748d0,  0.000714d0,  0.000703d0,  0.000693d0,  0.000664d0,  0.000594d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0 /)

      do sex = 1, 2
         do age = 0, 100
         
            w_no_asf1_sy(sex,age,startYr+1:)  = w_no_asf1_sy(sex,age,startYr) 
            w_no_asf2_sy(sex,age,startYr+1:)  = w_no_asf2_sy(sex,age,startYr) 
            w_no_asj1_sy(sex,age,startYr+1:)  = w_no_asj1_sy(sex,age,startYr) 
            w_no_asj2_sy(sex,age,startYr+1:)  = w_no_asj2_sy(sex,age,startYr) 
            w_no_awj_sy(sex,age,startYr+1:)   = w_no_awj_sy(sex,age,startYr)  
            w_no_awjf_sy(sex,age,startYr+1:)  = w_no_awjf_sy(sex,age,startYr) 
            w_no_awh_sy(sex,age,startYr+1:)   = w_no_awh_sy(sex,age,startYr)
            w_no_awt_sy(sex,age,startYr+1:)   = w_no_awt_sy(sex,age,startYr)
            w_no_awtfa_sy(sex,age,startYr+1:) = w_no_awtfa_sy(sex,age,startYr)
            w_no_awtfn_sy(sex,age,startYr+1:) = w_no_awtfn_sy(sex,age,startYr)
            w_no_nasf1_sy(sex,age,startYr+1:) = w_no_nasf1_sy(sex,age,startYr)
            w_no_nasf2_sy(sex,age,startYr+1:) = w_no_nasf2_sy(sex,age,startYr)
            w_no_nasj1_sy(sex,age,startYr+1:) = w_no_nasj1_sy(sex,age,startYr)
            w_no_nasj2_sy(sex,age,startYr+1:) = w_no_nasj2_sy(sex,age,startYr)
            w_no_nawj_sy(sex,age,startYr+1:)  = w_no_nawj_sy(sex,age,startYr) 
            w_no_nawjf_sy(sex,age,startYr+1:) = w_no_nawjf_sy(sex,age,startYr)
            w_no_nawh_sy(sex,age,startYr+1:)  = w_no_nawh_sy(sex,age,startYr) 
            w_no_nawt_sy(sex,age,startYr+1:)  = w_no_nawt_sy(sex,age,startYr) 
            w_no_nawtf_sy(sex,age,startYr+1:) = w_no_nawtf_sy(sex,age,startYr)
            
            no_asf1_sy(sex,age,startYr:)  = w_no_asf1_sy(sex,age,startYr:)  * no_asf1(0,startYr:)  
            no_asf2_sy(sex,age,startYr:)  = w_no_asf2_sy(sex,age,startYr:)  * no_asf2(0,startYr:)  
            no_asj1_sy(sex,age,startYr:)  = w_no_asj1_sy(sex,age,startYr:)  * no_asj1(0,startYr:)  
            no_asj2_sy(sex,age,startYr:)  = w_no_asj2_sy(sex,age,startYr:)  * no_asj2(0,startYr:)  
            no_awj_sy(sex,age,startYr:)   = w_no_awj_sy(sex,age,startYr:)   * no_awj(0,startYr:)   
            no_awjf_sy(sex,age,startYr:)  = w_no_awjf_sy(sex,age,startYr:)  * no_awjf(0,startYr:)  
            no_awh_sy(sex,age,startYr:)   = w_no_awh_sy(sex,age,startYr:)   * no_awh(0,startYr:)   
            no_awt_sy(sex,age,startYr:)   = w_no_awt_sy(sex,age,startYr:)   * no_awt(0,startYr:)   
            no_awtfa_sy(sex,age,startYr:) = w_no_awtfa_sy(sex,age,startYr:) * no_awtfa(0,startYr:) 
            no_awtfn_sy(sex,age,startYr:) = w_no_awtfn_sy(sex,age,startYr:) * no_awtfn(0,startYr:) 
            no_nasf1_sy(sex,age,startYr:) = w_no_nasf1_sy(sex,age,startYr:) * no_nasf1(0,startYr:) 
            no_nasf2_sy(sex,age,startYr:) = w_no_nasf2_sy(sex,age,startYr:) * no_nasf2(0,startYr:) 
            no_nasj1_sy(sex,age,startYr:) = w_no_nasj1_sy(sex,age,startYr:) * no_nasj1(0,startYr:) 
            no_nasj2_sy(sex,age,startYr:) = w_no_nasj2_sy(sex,age,startYr:) * no_nasj2(0,startYr:) 
            no_nawj_sy(sex,age,startYr:)  = w_no_nawj_sy(sex,age,startYr:)  * no_nawj(0,startYr:)  
            no_nawjf_sy(sex,age,startYr:) = w_no_nawjf_sy(sex,age,startYr:) * no_nawjf(0,startYr:) 
            no_nawh_sy(sex,age,startYr:)  = w_no_nawh_sy(sex,age,startYr:)  * no_nawh(0,startYr:)  
            no_nawt_sy(sex,age,startYr:)  = w_no_nawt_sy(sex,age,startYr:)  * no_nawt(0,startYr:)  
            no_nawtf_sy(sex,age,startYr:) = w_no_nawtf_sy(sex,age,startYr:) * no_nawtf(0,startYr:)
            
            no_asf1_sy(sex,age,startYr:80)  = nilsy(sex,age,startYr:80) * no_asf1_sy(sex,age,80)  / nilsy(sex,age,80)
            no_asf2_sy(sex,age,startYr:80)  = nilsy(sex,age,startYr:80) * no_asf2_sy(sex,age,80)  / nilsy(sex,age,80)
            no_asj1_sy(sex,age,startYr:80)  = nilsy(sex,age,startYr:80) * no_asj1_sy(sex,age,80)  / nilsy(sex,age,80)
            no_asj2_sy(sex,age,startYr:80)  = nilsy(sex,age,startYr:80) * no_asj2_sy(sex,age,80)  / nilsy(sex,age,80)
            no_awj_sy(sex,age,startYr:80)   = nilsy(sex,age,startYr:80) * no_awj_sy(sex,age,80)   / nilsy(sex,age,80)
            no_awjf_sy(sex,age,startYr:80)  = nilsy(sex,age,startYr:80) * no_awjf_sy(sex,age,80)  / nilsy(sex,age,80)
            no_awh_sy(sex,age,startYr:80)   = nilsy(sex,age,startYr:80) * no_awh_sy(sex,age,80)   / nilsy(sex,age,80)
            no_awt_sy(sex,age,startYr:80)   = nilsy(sex,age,startYr:80) * no_awt_sy(sex,age,80)   / nilsy(sex,age,80)
            no_awtfa_sy(sex,age,startYr:80) = nilsy(sex,age,startYr:80) * no_awtfa_sy(sex,age,80) / nilsy(sex,age,80)
            no_awtfn_sy(sex,age,startYr:80) = nilsy(sex,age,startYr:80) * no_awtfn_sy(sex,age,80) / nilsy(sex,age,80)
            no_nasf1_sy(sex,age,startYr:80) = nilsy(sex,age,startYr:80) * no_nasf1_sy(sex,age,80) / nilsy(sex,age,80)
            no_nasf2_sy(sex,age,startYr:80) = nilsy(sex,age,startYr:80) * no_nasf2_sy(sex,age,80) / nilsy(sex,age,80)
            no_nasj1_sy(sex,age,startYr:80) = nilsy(sex,age,startYr:80) * no_nasj1_sy(sex,age,80) / nilsy(sex,age,80)
            no_nasj2_sy(sex,age,startYr:80) = nilsy(sex,age,startYr:80) * no_nasj2_sy(sex,age,80) / nilsy(sex,age,80)
            no_nawj_sy(sex,age,startYr:80)  = nilsy(sex,age,startYr:80) * no_nawj_sy(sex,age,80)  / nilsy(sex,age,80)
            no_nawjf_sy(sex,age,startYr:80) = nilsy(sex,age,startYr:80) * no_nawjf_sy(sex,age,80) / nilsy(sex,age,80)
            no_nawh_sy(sex,age,startYr:80)  = nilsy(sex,age,startYr:80) * no_nawh_sy(sex,age,80)  / nilsy(sex,age,80)
            no_nawt_sy(sex,age,startYr:80)  = nilsy(sex,age,startYr:80) * no_nawt_sy(sex,age,80)  / nilsy(sex,age,80)
            no_nawtf_sy(sex,age,startYr:80) = nilsy(sex,age,startYr:80) * no_nawtf_sy(sex,age,80) / nilsy(sex,age,80)
            
            no_awtf_sy(sex,age,startYr:) = no_awtfa_sy(sex,age,startYr:) + no_awtfn_sy(sex,age,startYr:)
         
         end do
      end do
  
   end subroutine DisaggregatePopulationComponents
   
!===============================================================================
   
   subroutine CompareAggregates()
   
      integer :: age, sex

      do sex = 1, 2
         do age = 0, 100
            
            !no_a_sy_p(sex,age,startYr:) =  &
            !   no_asf1_sy(sex,age,startYr:) + no_asj1_sy(sex,age,startYr:) + &
            !   no_asf2_sy(sex,age,startYr:) + no_asj2_sy(sex,age,startYr:) + &
            !   no_awj_sy(sex,age,startYr:)  + no_awh_sy(sex,age,startYr:) + no_awt_sy(sex,age,startYr:) + &
            !   no_awjf_sy(sex,age,startYr:) + no_awtfa_sy(sex,age,startYr:) + no_awtfn_sy(sex,age,startYr:)

            no_na_sy_p(sex,age,startYr:) =  &
               no_nasf1_sy(sex,age,startYr:) + no_nasj1_sy(sex,age,startYr:) + &
               no_nasf2_sy(sex,age,startYr:) + no_nasj2_sy(sex,age,startYr:) + &
               no_nawj_sy(sex,age,startYr:)  + no_nawh_sy(sex,age,startYr:) + no_nawt_sy(sex,age,startYr:) + &
               no_nawjf_sy(sex,age,startYr:) + no_nawtf_sy(sex,age,startYr:)

         end do
      end do
   
   end subroutine CompareAggregates
   
!===============================================================================   
   
   subroutine ScaleCategories()
   
      integer :: age, sex
      
      do sex = 1,2 
         do age = 1, 69
            
            !no_asf1_sy(sex,age,startYr:) = no_asf1_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            !no_asf2_sy(sex,age,startYr:) = no_asf2_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            !no_asj1_sy(sex,age,startYr:) = no_asj1_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            !no_asj2_sy(sex,age,startYr:) = no_asj2_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            !no_awj_sy(sex,age,startYr:) = no_awj_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            !no_awjf_sy(sex,age,startYr:) = no_awjf_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            !no_awh_sy(sex,age,startYr:) = no_awh_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            !no_awt_sy(sex,age,startYr:) = no_awt_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            !no_awtfa_sy(sex,age,startYr:) = no_awtfa_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            !no_awtfn_sy(sex,age,startYr:) = no_awtfn_sy(sex,age,startYr:) * &
            !   nilasy(sex,age,startYr:) / no_a_sy_p(sex,age,startYr:)
            
            no_nasf1_sy(sex,age,startYr:) = no_nasf1_sy(sex,age,startYr:) * &
               (nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)) / no_na_sy_p(sex,age,startYr:)
            no_nasf2_sy(sex,age,startYr:) = no_nasf2_sy(sex,age,startYr:) * &
               (nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)) / no_na_sy_p(sex,age,startYr:)
            no_nasj1_sy(sex,age,startYr:) = no_nasj1_sy(sex,age,startYr:) * &
               (nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)) / no_na_sy_p(sex,age,startYr:)
            no_nasj2_sy(sex,age,startYr:) = no_nasj2_sy(sex,age,startYr:) * &
               (nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)) / no_na_sy_p(sex,age,startYr:)
            no_nawj_sy(sex,age,startYr:) = no_nawj_sy(sex,age,startYr:) * &
               (nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)) / no_na_sy_p(sex,age,startYr:)
            no_nawjf_sy(sex,age,startYr:) = no_nawjf_sy(sex,age,startYr:) * &
               (nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)) / no_na_sy_p(sex,age,startYr:)
            no_nawh_sy(sex,age,startYr:) = no_nawh_sy(sex,age,startYr:) * &
               (nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)) / no_na_sy_p(sex,age,startYr:)
            no_nawt_sy(sex,age,startYr:) = no_nawt_sy(sex,age,startYr:) * &
               (nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)) / no_na_sy_p(sex,age,startYr:)
            no_nawtf_sy(sex,age,startYr:) = no_nawtf_sy(sex,age,startYr:) * &
               (nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)) / no_na_sy_p(sex,age,startYr:)

         end do
      end do
      
      do sex = 1, 2
         do age = 70, 100
            !no_awt_sy(sex,age,startYr:) = nilasy(sex,age,startYr:)
            no_nawt_sy(sex,age,startYr:) = nilnasy(sex,age,startYr:) - nildnasy(sex,age,startYr:)
         end do
      end do
      
      do sex = 1, 2
         !no_awtfa_sy(sex,0,startYr:) = nilasy(sex,0,startYr:)
         no_nawtf_sy(sex,0,startYr:) = nilnasy(sex,0,startYr:) - nildnasy(sex,0,startYr:)
      end do
      
      !do sex = 1, 2
      !   do age = 0, 100
      !      no_awtf_sy(sex,age,startYr:) = no_awtfa_sy(sex,age,startYr:) + no_awtfn_sy(sex,age,startYr:)
      !   end do
      !end do
   
   end subroutine ScaleCategories
   
!===============================================================================   
      
   subroutine CreateAdditionalPopulationConcepts()
   
      integer :: age, sex, yr
      
      do sex = 1, 2
         do age = 0, 100
            
            no_no_sy(sex,age,startYr:) = nilsy(sex,age,startYr:) &
                        - no_asf1_sy(sex,age,startYr:)  &
                        - no_asf2_sy(sex,age,startYr:)  &
                        - no_asj1_sy(sex,age,startYr:)  &
                        - no_asj2_sy(sex,age,startYr:)  &
                        -  no_awj_sy(sex,age,startYr:)  &
                        - no_awjf_sy(sex,age,startYr:)  &
                        -  no_awh_sy(sex,age,startYr:)  &
                        -  no_awt_sy(sex,age,startYr:)  &
                        - no_awtfa_sy(sex,age,startYr:)  &
                                      
                        - no_nasf1_sy(sex,age,startYr:) &
                        - no_nasf2_sy(sex,age,startYr:) &
                        - no_nasj1_sy(sex,age,startYr:) &
                        - no_nasj2_sy(sex,age,startYr:) &
                        -  no_nawj_sy(sex,age,startYr:) &
                        - no_nawjf_sy(sex,age,startYr:) &
                        -  no_nawh_sy(sex,age,startYr:) &
                        -  no_nawt_sy(sex,age,startYr:) &
                        -  no_awtfn_sy(sex,age,startYr:) &
                        - no_nawtf_sy(sex,age,startYr:) &
                                      
                        -nildsy(sex,age,startYr:)
            
            no_as1_sy(sex,age,startYr:)  = no_asf1_sy(sex,age,startYr:)  + no_asj1_sy(sex,age,startYr:)
            no_as2_sy(sex,age,startYr:)  = no_asf2_sy(sex,age,startYr:)  + no_asj2_sy(sex,age,startYr:)
            no_as_sy(sex,age,startYr:)   = no_as1_sy(sex,age,startYr:)   +  no_as2_sy(sex,age,startYr:)
            no_aw1_sy(sex,age,startYr:)  = no_awj_sy(sex,age,startYr:)   +  no_awh_sy(sex,age,startYr:) + no_awt_sy(sex,age,startYr:)
            no_aw2_sy(sex,age,startYr:)  = no_awjf_sy(sex,age,startYr:)  + no_awtfa_sy(sex,age,startYr:)
            no_aw_sy(sex,age,startYr:)   = no_aw1_sy(sex,age,startYr:)   +  no_aw2_sy(sex,age,startYr:)
            no_a_sy(sex,age,startYr:)    = no_as_sy(sex,age,startYr:)    +  no_aw_sy(sex,age,startYr:)
    
            no_nas1_sy(sex,age,startYr:) = no_nasf1_sy(sex,age,startYr:) + no_nasj1_sy(sex,age,startYr:)
            no_nas2_sy(sex,age,startYr:) = no_nasf2_sy(sex,age,startYr:) + no_nasj2_sy(sex,age,startYr:)
            no_nas_sy(sex,age,startYr:)  = no_nas1_sy(sex,age,startYr:)  +  no_nas2_sy(sex,age,startYr:)
            no_naw1_sy(sex,age,startYr:) = no_nawj_sy(sex,age,startYr:)  +  no_nawh_sy(sex,age,startYr:) + no_nawt_sy(sex,age,startYr:)
            no_naw2_sy(sex,age,startYr:) = no_nawjf_sy(sex,age,startYr:)
            no_naw_sy(sex,age,startYr:)  = no_naw1_sy(sex,age,startYr:)  +  no_naw2_sy(sex,age,startYr:)
            no_na_sy(sex,age,startYr:)   = no_nas_sy(sex,age,startYr:)   +  no_naw_sy(sex,age,startYr:)
      
         end do
      end do
      
      do yr = startYr, MAX_YR

         do sex = 1, 2
            no_no(sex,yr)   = sum(no_no_sy(sex,0:100,yr))
            
            no_as1(sex,yr)  = sum(no_as1_sy(sex,0:100,yr))
            no_as2(sex,yr)  = sum(no_as2_sy(sex,0:100,yr))
            no_as(sex,yr)   = sum(no_as_sy(sex,0:100,yr))
            no_aw1(sex,yr)  = sum(no_aw1_sy(sex,0:100,yr))
            no_aw2(sex,yr)  = sum(no_aw2_sy(sex,0:100,yr))
            no_aw(sex,yr)   = sum(no_aw_sy(sex,0:100,yr))
            no_a(sex,yr)    = sum(no_a_sy(sex,0:100,yr))

            no_nas1(sex,yr) = sum(no_nas1_sy(sex,0:100,yr))
            no_nas2(sex,yr) = sum(no_nas2_sy(sex,0:100,yr))
            no_nas(sex,yr)  = sum(no_nas_sy(sex,0:100,yr))
            no_naw1(sex,yr) = sum(no_naw1_sy(sex,0:100,yr))
            no_naw2(sex,yr) = sum(no_naw2_sy(sex,0:100,yr))
            no_naw(sex,yr)  = sum(no_naw_sy(sex,0:100,yr))
            no_na(sex,yr)   = sum(no_na_sy(sex,0:100,yr))
            
            no_asf1(sex,yr)  = sum(no_asf1_sy(sex,0:100,yr)) 
            no_nasf1(sex,yr) = sum(no_nasf1_sy(sex,0:100,yr))
            no_asf2(sex,yr)  = sum(no_asf2_sy(sex,0:100,yr)) 
            no_nasf2(sex,yr) = sum(no_nasf2_sy(sex,0:100,yr))
            no_asj1(sex,yr)  = sum(no_asj1_sy(sex,0:100,yr)) 
            no_nasj1(sex,yr) = sum(no_nasj1_sy(sex,0:100,yr))
            no_asj2(sex,yr)  = sum(no_asj2_sy(sex,0:100,yr)) 
            no_nasj2(sex,yr) = sum(no_nasj2_sy(sex,0:100,yr))
            no_awj(sex,yr)   = sum(no_awj_sy(sex,0:100,yr))  
            no_nawj(sex,yr)  = sum(no_nawj_sy(sex,0:100,yr)) 
            no_awjf(sex,yr)  = sum(no_awjf_sy(sex,0:100,yr)) 
            no_nawjf(sex,yr) = sum(no_nawjf_sy(sex,0:100,yr))
            no_awh(sex,yr)   = sum(no_awh_sy(sex,0:100,yr))  
            no_nawh(sex,yr)  = sum(no_nawh_sy(sex,0:100,yr)) 
            no_awt(sex,yr)   = sum(no_awt_sy(sex,0:100,yr))  
            no_nawt(sex,yr)  = sum(no_nawt_sy(sex,0:100,yr)) 
            no_awtfa(sex,yr) = sum(no_awtfa_sy(sex,0:100,yr))
            no_awtfn(sex,yr) = sum(no_awtfn_sy(sex,0:100,yr))
            no_awtf(sex,yr) = no_awtfa(sex,yr) + no_awtfn(sex,yr)
            no_nawtf(sex,yr) = sum( no_nawtf_sy(sex,0:100,yr))
         end do
         
         no_no(0,yr)  = sum(no_no(1:2,yr))
         
         no_as1(0,yr)  = sum(no_as1(1:2,yr)) 
         no_as2(0,yr)  = sum(no_as2(1:2,yr)) 
         no_as(0,yr)   = sum(no_as(1:2,yr))  
         no_aw1(0,yr)  = sum(no_aw1(1:2,yr)) 
         no_aw2(0,yr)  = sum(no_aw2(1:2,yr)) 
         no_aw(0,yr)   = sum(no_aw(1:2,yr))  
         no_a(0,yr)    = sum(no_a(1:2,yr))  
         
         no_nas1(0,yr) = sum(no_nas1(1:2,yr))
         no_nas2(0,yr) = sum(no_nas2(1:2,yr))
         no_nas(0,yr)  = sum(no_nas(1:2,yr)) 
         no_naw1(0,yr) = sum(no_naw1(1:2,yr))
         no_naw2(0,yr) = sum(no_naw2(1:2,yr))
         no_naw(0,yr)  = sum(no_naw(1:2,yr))
         no_na(0,yr)   = sum(no_na(1:2,yr))
         
         no_asf1(0,yr)  = sum(no_asf1(1:2,yr)) 
         no_nasf1(0,yr) = sum(no_nasf1(1:2,yr))
         no_asf2(0,yr)  = sum(no_asf2(1:2,yr)) 
         no_nasf2(0,yr) = sum(no_nasf2(1:2,yr))
         no_asj1(0,yr)  = sum(no_asj1(1:2,yr)) 
         no_nasj1(0,yr) = sum(no_nasj1(1:2,yr))
         no_asj2(0,yr)  = sum(no_asj2(1:2,yr)) 
         no_nasj2(0,yr) = sum(no_nasj2(1:2,yr))
         no_awj(0,yr)   = sum(no_awj(1:2,yr))  
         no_nawj(0,yr)  = sum(no_nawj(1:2,yr)) 
         no_awjf(0,yr)  = sum(no_awjf(1:2,yr)) 
         no_nawjf(0,yr) = sum(no_nawjf(1:2,yr))
         no_awh(0,yr)   = sum(no_awh(1:2,yr))  
         no_nawh(0,yr)  = sum(no_nawh(1:2,yr)) 
         no_awt(0,yr)   = sum(no_awt(1:2,yr))  
         no_nawt(0,yr)  = sum(no_nawt(1:2,yr)) 
         no_awtfa(0,yr) = sum(no_awtfa(1:2,yr))
         no_awtfn(0,yr) = sum(no_awtfn(1:2,yr))
         no_awtf(0,yr) = no_awtfa(0,yr) + no_awtfn(0,yr)
         no_nawtf(0,yr) = sum(no_nawtf(1:2,yr))
         
      end do

   end subroutine CreateAdditionalPopulationConcepts
   
!===============================================================================   
         
   subroutine EstimateEmploymentComponents()
   
      integer :: age,sex, yr
      
      do age = 16, 100
         
         eo_asf1_sy(1,age,startYr:) = 0.35d0 * no_asf1_sy(1,age,startYr:)
         eo_asf1_sy(2,age,startYr:) = 0.35d0 * no_asf1_sy(2,age,startYr:)

         eo_asf2_sy(1,age,startYr:) = 0.1d0 * no_asf2_sy(1,age,startYr:)
         eo_asf2_sy(2,age,startYr:) = 0.1d0 * no_asf2_sy(2,age,startYr:)

         eo_nasf1_sy(1,age,startYr:) = re_sy(1,age,startYr:) * no_nasf1_sy(1,age,startYr:)
         eo_nasf1_sy(2,age,startYr:) = re_sy(2,age,startYr:) * no_nasf1_sy(2,age,startYr:)  

         eo_nasf2_sy(1,age,startYr:) = re_sy(1,age,startYr:) * no_nasf2_sy(1,age,startYr:)
         eo_nasf2_sy(2,age,startYr:) = re_sy(2,age,startYr:) * no_nasf2_sy(2,age,startYr:)
         
         eo_asj1_sy(1,age,startYr:) = 0.35d0 * no_asj1_sy(1,age,startYr:)
         eo_asj1_sy(2,age,startYr:) = 0.35d0 * no_asj1_sy(2,age,startYr:)

         eo_asj2_sy(1,age,startYr:) = 0.5d0 * no_asj2_sy(1,age,startYr:)
         eo_asj2_sy(2,age,startYr:) = 0.5d0 * no_asj2_sy(2,age,startYr:)

         eo_awjf_sy(1,age,startYr:) = 0.5d0 * no_awjf_sy(1,age,startYr:)
         eo_awjf_sy(2,age,startYr:) = 0.5d0 * no_awjf_sy(2,age,startYr:)

         eo_awh_sy(1,age,startYr:) = 0.98d0 * no_awh_sy(1,age,startYr:)
         eo_awh_sy(2,age,startYr:) = 0.98d0 * no_awh_sy(2,age,startYr:)

      end do
      
      do age = 16, 19
         
          eo_awj_sy(1,age,startYr:) = 0d0 * no_awj_sy(1,age,startYr:)
          eo_awj_sy(2,age,startYr:) = 0d0 * no_awj_sy(2,age,startYr:)
         
      end do
      
      do age = 19, 100

          eo_awj_sy(1,age,startYr:) = 0.555d0 * no_awj_sy(1,age,startYr:)
          eo_awj_sy(2,age,startYr:) = 0.555d0 * no_awj_sy(2,age,startYr:)
         
      end do
      
      do age = 16, 100
         
         eo_awt_sy(1,age,startYr:) = 0.98d0 * no_awt_sy(1,age,startYr:)
         eo_awt_sy(2,age,startYr:) = 0.98d0 * no_awt_sy(2,age,startYr:)
         
         eo_awtfa_sy(1,age,startYr:) = (re_sy(1,age,startYr:) * 0.9d0) * no_awtfa_sy(1,age,startYr:)
         eo_awtfa_sy(2,age,startYr:) = (re_sy(2,age,startYr:) * 0.9d0) * no_awtfa_sy(2,age,startYr:)
         
         eo_awtfn_sy(1,age,startYr:) = 0.1d0 * no_awtfn_sy(1,age,startYr:)
         eo_awtfn_sy(2,age,startYr:) = 0.1d0 * no_awtfn_sy(2,age,startYr:)

         eo_awtf_sy(1,age,startYr:) = eo_awtfa_sy(1,age,startYr:) + eo_awtfn_sy(1,age,startYr:)
         eo_awtf_sy(2,age,startYr:) = eo_awtfa_sy(2,age,startYr:) + eo_awtfn_sy(2,age,startYr:)
         
         eo_nasj1_sy(1,age,startYr:)  = re_sy(1,age,startYr:) * no_nasj1_sy(1,age,startYr:)
         eo_nasj1_sy(2,age,startYr:)  = re_sy(2,age,startYr:) * no_nasj1_sy(2,age,startYr:)
  
         eo_nasj2_sy(1,age,startYr:)  = re_sy(1,age,startYr:) * no_nasj2_sy(1,age,startYr:)
         eo_nasj2_sy(2,age,startYr:)  = re_sy(2,age,startYr:) * no_nasj2_sy(2,age,startYr:)
    
         eo_nawj_sy(1,age,startYr:)  = re_sy(1,age,startYr:) * no_nawj_sy(1,age,startYr:)
         eo_nawj_sy(2,age,startYr:)  = re_sy(2,age,startYr:) * no_nawj_sy(2,age,startYr:)
    
         eo_nawjf_sy(1,age,startYr:)  = re_sy(1,age,startYr:) * no_nawjf_sy(1,age,startYr:)
         eo_nawjf_sy(2,age,startYr:)  = re_sy(2,age,startYr:) * no_nawjf_sy(2,age,startYr:)
    
         eo_nawh_sy(1,age,startYr:)  = re_sy(1,age,startYr:) * no_nawh_sy(1,age,startYr:)
         eo_nawh_sy(2,age,startYr:)  = re_sy(2,age,startYr:) * no_nawh_sy(2,age,startYr:)
    
         eo_nawt_sy(1,age,startYr:)  = re_sy(1,age,startYr:) * no_nawt_sy(1,age,startYr:)
         eo_nawt_sy(2,age,startYr:)  = re_sy(2,age,startYr:) * no_nawt_sy(2,age,startYr:)
    
         eo_nawtf_sy(1,age,startYr:)  = re_sy(1,age,startYr:) * no_nawtf_sy(1,age,startYr:)
         eo_nawtf_sy(2,age,startYr:)  = re_sy(2,age,startYr:) * no_nawtf_sy(2,age,startYr:)
         
      end do
   
   end subroutine EstimateEmploymentComponents
               
!===============================================================================   
               
   subroutine CreateAdditionalEmploymentConcepts()
   
      integer :: age, sex, yr
      
      do sex = 1, 2
         do age = 0, 100
            
            eo_no_sy(sex,age,startYr:) = re_sy(sex,age,startYr:) * no_no_sy(sex,age,startYr:)
            eo_sy(sex,age,startYr:) = eo_no_sy(sex,age,startYr:) &
                        + eo_asf1_sy(sex,age,startYr:)  &
                        + eo_asf2_sy(sex,age,startYr:)  &
                        + eo_asj1_sy(sex,age,startYr:)  &
                        + eo_asj2_sy(sex,age,startYr:)  &
                        +  eo_awj_sy(sex,age,startYr:)  &
                        + eo_awjf_sy(sex,age,startYr:)  &
                        +  eo_awh_sy(sex,age,startYr:)  &
                        +  eo_awt_sy(sex,age,startYr:)  &
                        + eo_awtfa_sy(sex,age,startYr:)  &
                                      
                        + eo_nasf1_sy(sex,age,startYr:) &
                        + eo_nasf2_sy(sex,age,startYr:) &
                        + eo_nasj1_sy(sex,age,startYr:) &
                        + eo_nasj2_sy(sex,age,startYr:) &
                        +  eo_nawj_sy(sex,age,startYr:) &
                        + eo_nawjf_sy(sex,age,startYr:) &
                        +  eo_nawh_sy(sex,age,startYr:) &
                        +  eo_nawt_sy(sex,age,startYr:) &
                        + eo_awtfn_sy(sex,age,startYr:)  &
                        + eo_nawtf_sy(sex,age,startYr:)
            
            eo_as1_sy(sex,age,startYr:)  = eo_asf1_sy(sex,age,startYr:)  + eo_asj1_sy(sex,age,startYr:)
            eo_as2_sy(sex,age,startYr:)  = eo_asf2_sy(sex,age,startYr:)  + eo_asj2_sy(sex,age,startYr:)
            eo_as_sy(sex,age,startYr:)   = eo_as1_sy(sex,age,startYr:)   +  eo_as2_sy(sex,age,startYr:)
            eo_aw1_sy(sex,age,startYr:)  = eo_awj_sy(sex,age,startYr:)   +  eo_awh_sy(sex,age,startYr:) + eo_awt_sy(sex,age,startYr:)
            eo_aw2_sy(sex,age,startYr:)  = eo_awjf_sy(sex,age,startYr:)  + eo_awtfa_sy(sex,age,startYr:)
            eo_aw_sy(sex,age,startYr:)   = eo_aw1_sy(sex,age,startYr:)   +  eo_aw2_sy(sex,age,startYr:)
            eo_a_sy(sex,age,startYr:)    = eo_as_sy(sex,age,startYr:)    +  eo_aw_sy(sex,age,startYr:)
    
            eo_nas1_sy(sex,age,startYr:) = eo_nasf1_sy(sex,age,startYr:) + eo_nasj1_sy(sex,age,startYr:)
            eo_nas2_sy(sex,age,startYr:) = eo_nasf2_sy(sex,age,startYr:) + eo_nasj2_sy(sex,age,startYr:)
            eo_nas_sy(sex,age,startYr:)  = eo_nas1_sy(sex,age,startYr:)  +  eo_nas2_sy(sex,age,startYr:)
            eo_naw1_sy(sex,age,startYr:) = eo_nawj_sy(sex,age,startYr:)  +  eo_nawh_sy(sex,age,startYr:) + eo_nawt_sy(sex,age,startYr:)
            eo_naw2_sy(sex,age,startYr:) = eo_nawjf_sy(sex,age,startYr:)
            eo_naw_sy(sex,age,startYr:)  = eo_naw1_sy(sex,age,startYr:)  +  eo_naw2_sy(sex,age,startYr:)
            eo_na_sy(sex,age,startYr:)   = eo_nas_sy(sex,age,startYr:)   +  eo_naw_sy(sex,age,startYr:)
      
         end do
      end do
   
   end subroutine CreateAdditionalEmploymentConcepts
      
!===============================================================================
   
   subroutine AggregateEmploymentConcepts()
   
      integer :: yr, sex
      
      do yr = startYr, MAX_YR
         do sex = 1, 2
            eo_asf1(sex,24,yr) = sum(eo_asf1_sy(sex,16:100,yr))
            eo_asf2(sex,24,yr) = sum(eo_asf2_sy(sex,16:100,yr))
            eo_asj1(sex,24,yr) = sum(eo_asj1_sy(sex,16:100,yr))
            eo_asj2(sex,24,yr) = sum(eo_asj2_sy(sex,16:100,yr))
            eo_awj(sex,24,yr) = sum(eo_awj_sy(sex,16:100,yr))
            eo_awjf(sex,24,yr) = sum(eo_awjf_sy(sex,16:100,yr))
            eo_awh(sex,24,yr) = sum(eo_awh_sy(sex,16:100,yr))
            eo_awt(sex,24,yr) = sum(eo_awt_sy(sex,16:100,yr))
            eo_awtf(sex,24,yr) = sum(eo_awtf_sy(sex,16:100,yr))
            eo_awtfa(sex,24,yr) = sum(eo_awtfa_sy(sex,16:100,yr))
            eo_awtfn(sex,24,yr) = sum(eo_awtfn_sy(sex,16:100,yr))
            
            eo_nasf1(sex,24,yr) = sum(eo_nasf1_sy(sex,16:100,yr))
            eo_nasf2(sex,24,yr) = sum(eo_nasf2_sy(sex,16:100,yr))
            eo_nasj1(sex,24,yr) = sum(eo_nasj1_sy(sex,16:100,yr))
            eo_nasj2(sex,24,yr) = sum(eo_nasj2_sy(sex,16:100,yr))
            eo_nawj(sex,24,yr) = sum(eo_nawj_sy(sex,16:100,yr))
            eo_nawjf(sex,24,yr) = sum(eo_nawjf_sy(sex,16:100,yr))
            eo_nawh(sex,24,yr) = sum(eo_nawh_sy(sex,16:100,yr))
            eo_nawt(sex,24,yr) = sum(eo_nawt_sy(sex,16:100,yr))
            eo_nawtf(sex,24,yr) = sum(eo_nawtf_sy(sex,16:100,yr))
            
            eo_as1(sex,24,yr) = sum(eo_as1_sy(sex,16:100,yr))
            eo_as2(sex,24,yr) = sum(eo_as2_sy(sex,16:100,yr))
            eo_as(sex,24,yr) = sum(eo_as_sy(sex,16:100,yr))
            eo_aw1(sex,24,yr) = sum(eo_aw1_sy(sex,16:100,yr))
            eo_aw2(sex,24,yr) = sum(eo_aw2_sy(sex,16:100,yr))
            eo_aw(sex,24,yr) = sum(eo_aw_sy(sex,16:100,yr))
            eo_a(sex,24,yr) = sum(eo_a_sy(sex,16:100,yr))
            
            eo_nas1(sex,24,yr) = sum(eo_nas1_sy(sex,16:100,yr))
            eo_nas2(sex,24,yr) = sum(eo_nas2_sy(sex,16:100,yr))
            eo_nas(sex,24,yr) = sum(eo_nas_sy(sex,16:100,yr))
            eo_naw1(sex,24,yr) = sum(eo_naw1_sy(sex,16:100,yr))
            eo_naw2(sex,24,yr) = sum(eo_naw2_sy(sex,16:100,yr))
            eo_naw(sex,24,yr) = sum(eo_naw_sy(sex,16:100,yr))
            eo_na(sex,24,yr) = sum(eo_na_sy(sex,16:100,yr))

            eo_no(sex,24,yr) = sum(eo_no_sy(sex,16:100,yr))
            eo(sex,24,yr) = sum(eo_sy(sex,16:100,yr))

         end do
         eo_asf1(0,24,yr)    = eo_asf1(1,24,yr) +  eo_asf1(2,24,yr)
         eo_asf1(0,24,yr)  = eo_asf1(1,24,yr)  + eo_asf1(2,24,yr)  
         eo_asf2(0,24,yr)  = eo_asf2(1,24,yr)  + eo_asf2(2,24,yr)  
         eo_asj1(0,24,yr)  = eo_asj1(1,24,yr)  + eo_asj1(2,24,yr)  
         eo_asj2(0,24,yr)  = eo_asj2(1,24,yr)  + eo_asj2(2,24,yr)  
         eo_awj(0,24,yr)   = eo_awj(1,24,yr)   + eo_awj(2,24,yr)   
         eo_awjf(0,24,yr)  = eo_awjf(1,24,yr)  + eo_awjf(2,24,yr)  
         eo_awh(0,24,yr)   = eo_awh(1,24,yr)   + eo_awh(2,24,yr)   
         eo_awt(0,24,yr)   = eo_awt(1,24,yr)   + eo_awt(2,24,yr)   
         eo_awtf(0,24,yr)  = eo_awtf(1,24,yr)  + eo_awtf(2,24,yr)  
         eo_awtfa(0,24,yr) = eo_awtfa(1,24,yr) + eo_awtfa(2,24,yr) 
         eo_awtfn(0,24,yr) = eo_awtfn(1,24,yr) + eo_awtfn(2,24,yr) 
         
         eo_nasf1(0,24,yr) = eo_nasf1(1,24,yr) + eo_nasf1(2,24,yr) 
         eo_nasf2(0,24,yr) = eo_nasf2(1,24,yr) + eo_nasf2(2,24,yr) 
         eo_nasj1(0,24,yr) = eo_nasj1(1,24,yr) + eo_nasj1(2,24,yr) 
         eo_nasj2(0,24,yr) = eo_nasj2(1,24,yr) + eo_nasj2(2,24,yr) 
         eo_nawj(0,24,yr)  = eo_nawj(1,24,yr)  + eo_nawj(2,24,yr)  
         eo_nawjf(0,24,yr) = eo_nawjf(1,24,yr) + eo_nawjf(2,24,yr) 
         eo_nawh(0,24,yr)  = eo_nawh(1,24,yr)  + eo_nawh(2,24,yr)  
         eo_nawt(0,24,yr)  = eo_nawt(1,24,yr)  + eo_nawt(2,24,yr)  
         eo_nawtf(0,24,yr) = eo_nawtf(1,24,yr) + eo_nawtf(2,24,yr) 
         
         eo_as1(0,24,yr)   = eo_as1(1,24,yr)   + eo_as1(2,24,yr)   
         eo_as2(0,24,yr)   = eo_as2(1,24,yr)   + eo_as2(2,24,yr)   
         eo_as(0,24,yr)    = eo_as(1,24,yr)    + eo_as(2,24,yr)    
         eo_aw1(0,24,yr)   = eo_aw1(1,24,yr)   + eo_aw1(2,24,yr)   
         eo_aw2(0,24,yr)   = eo_aw2(1,24,yr)   + eo_aw2(2,24,yr)   
         eo_aw(0,24,yr)    = eo_aw(1,24,yr)    + eo_aw(2,24,yr)    
         eo_a(0,24,yr)     = eo_a(1,24,yr)     + eo_a(2,24,yr)     
         
         eo_nas1(0,24,yr)  = eo_nas1(1,24,yr)  + eo_nas1(2,24,yr)  
         eo_nas2(0,24,yr)  = eo_nas2(1,24,yr)  + eo_nas2(2,24,yr)  
         eo_nas(0,24,yr)   = eo_nas(1,24,yr)   + eo_nas(2,24,yr)   
         eo_naw1(0,24,yr)  = eo_naw1(1,24,yr)  + eo_naw1(2,24,yr)  
         eo_naw2(0,24,yr)  = eo_naw2(1,24,yr)  + eo_naw2(2,24,yr)  
         eo_naw(0,24,yr)   = eo_naw(1,24,yr)   + eo_naw(2,24,yr)   
         eo_na(0,24,yr)    = eo_na(1,24,yr)    + eo_na(2,24,yr)    
         
         eo_no(0,24,yr)    = eo_no(1,24,yr)    + eo_no(2,24,yr)    
         eo(0,24,yr)       = eo(1,24,yr)      + eo(2,24,yr)    
         
         eo_asf1(0,0,yr) = eo_asf1(0,24,yr)
         eo_asf1(0,0,yr) = eo_asf1(0,24,yr) 
         eo_asf2(0,0,yr) = eo_asf2(0,24,yr) 
         eo_asj1(0,0,yr) = eo_asj1(0,24,yr) 
         eo_asj2(0,0,yr) = eo_asj2(0,24,yr) 
         eo_awj(0,0,yr)  = eo_awj(0,24,yr)  
         eo_awjf(0,0,yr) = eo_awjf(0,24,yr) 
         eo_awh(0,0,yr)  = eo_awh(0,24,yr)  
         eo_awt(0,0,yr)  = eo_awt(0,24,yr)  
         eo_awtf(0,0,yr) = eo_awtf(0,24,yr) 
         eo_awtfa(0,0,yr)= eo_awtfa(0,24,yr) 
         eo_awtfn(0,0,yr)= eo_awtfn(0,24,yr )
         
         eo_nasf1(0,0,yr)= eo_nasf1(0,24,yr) 
         eo_nasf2(0,0,yr)= eo_nasf2(0,24,yr) 
         eo_nasj1(0,0,yr)= eo_nasj1(0,24,yr) 
         eo_nasj2(0,0,yr)= eo_nasj2(0,24,yr) 
         eo_nawj(0,0,yr) = eo_nawj(0,24,yr) 
         eo_nawjf(0,0,yr)= eo_nawjf(0,24,yr) 
         eo_nawh(0,0,yr) = eo_nawh(0,24,yr) 
         eo_nawt(0,0,yr) = eo_nawt(0,24,yr) 
         eo_nawtf(0,0,yr)= eo_nawtf(0,24,yr) 
         
         eo_as1(0,0,yr)  = eo_as1(0,24,yr)  
         eo_as2(0,0,yr)  = eo_as2(0,24,yr)  
         eo_as(0,0,yr)   = eo_as(0,24,yr)   
         eo_aw1(0,0,yr)  = eo_aw1(0,24,yr)  
         eo_aw2(0,0,yr)  = eo_aw2(0,24,yr)  
         eo_aw(0,0,yr)   = eo_aw(0,24,yr)   
         eo_a(0,0,yr)    = eo_a(0,24,yr)    
         
         eo_nas1(0,0,yr) = eo_nas1(0,24,yr) 
         eo_nas2(0,0,yr) = eo_nas2(0,24,yr) 
         eo_nas(0,0,yr)  = eo_nas(0,24,yr)  
         eo_naw1(0,0,yr) = eo_naw1(0,24,yr) 
         eo_naw2(0,0,yr) = eo_naw2(0,24,yr) 
         eo_naw(0,0,yr)  = eo_naw(0,24,yr)  
         eo_na(0,0,yr)   = eo_na(0,24,yr)   
         
         eo_no(0,0,yr)   = eo_no(0,24,yr)   
         eo(0,0,yr)      = eo(0,24,yr)      
         
      end do

   end subroutine AggregateEmploymentConcepts
   
!===============================================================================
   
   subroutine DistributeUnauthorized()
  
      integer :: yr

      do yr = startYr, 101
         wt1_l(yr)  = 0.4d0 / (0.4d0 + 0.45d0)
         wt1_lm(yr) = 0.125d0 / 0.4d0
         wt1_ls(yr) = (0.125d0 * 0.7d0) / 0.4d0
         wt1_lu(yr) = 1d0 - wt1_lm(yr) - wt1_ls(yr)
      end do
      
      do yr = startYr, 101
         wt1_i(yr)  = 1d0 - wt1_l(yr)
         wt1_im(yr) = 0.075d0 / 0.45d0
         wt1_is(yr) = (0.125d0 * 0.7d0) / 0.45d0
         wt1_iu(yr) = 1d0 - wt1_im(yr) - wt1_is(yr)
      end do
      
      eo_nol(0,0,:) = wt1_l(:) * eo_no(0,0,:)
      eo_noi(0,0,:) = wt1_i(:) * eo_no(0,0,:)
      
      ! Temporary routine to check intermediate values
      !do yr = 80, MAX_YR
      !   write(3003,'(2f12.6)') eo_nol(0,0,yr), eo_noi(0,0,yr)
      !end do
   
   end subroutine DistributeUnauthorized
   
!===============================================================================

   subroutine CalculateBaseValues()
   
      integer :: yr, sex, age, grp
      integer, dimension(14) :: lowAge =  (/ 16, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75 /)
      integer, dimension(14) :: highAge = (/ 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79 /)
      
      do yr = startYr, 101
         eo_no_s(0,0,yr) = wt1_ls(yr) * eo_nol(0,0,yr) + wt1_is(yr) * eo_noi(0,0,yr)
         eo_no_u(0,0,yr) = wt1_lu(yr) * eo_nol(0,0,yr) + wt1_iu(yr) * eo_noi(0,0,yr)
         do sex = 1, 2
            do age = 16, 100
               eo_no_s_sy(sex,age,yr) = (eo_no_sy(sex,age,yr) / eo_no(0,0,yr)) * eo_no_s(0,0,yr)
               eo_no_u_sy(sex,age,yr) = (eo_no_sy(sex,age,yr) / eo_no(0,0,yr)) * eo_no_u(0,0,yr)
            end do
         end do
      end do
      
      do yr = startYr, 101
         do sex = 1, 2
            do grp = 1, 14
               eo_no_s(sex,grp,yr) = 0d0
               eo_no_u(sex,grp,yr) = 0d0
               do age = lowAge(grp), highAge(grp)
                  eo_no_s(sex,grp,yr) = eo_no_s(sex,grp,yr) + eo_no_s_sy(sex,age,yr)
                  eo_no_u(sex,grp,yr) = eo_no_u(sex,grp,yr) + eo_no_u_sy(sex,age,yr)
               end do
               teo_esf(sex,grp,yr) = eo_no_s(sex,grp,yr) * rte(sex,grp,yr)
            end do
            eo_no_s(sex,31,yr) = 0d0
            eo_no_u(sex,31,yr) = 0d0
            do age = 80, 100
               eo_no_s(sex,31,yr) = eo_no_s(sex,31,yr) + eo_no_s_sy(sex,age,yr)
               eo_no_u(sex,31,yr) = eo_no_u(sex,31,yr) + eo_no_u_sy(sex,age,yr)
            end do
            teo_esf(sex,31,yr) = eo_no_s(sex,31,yr) * rte(sex,31,yr)
            eo_no_s(sex,23,yr) = 0d0
            eo_no_u(sex,23,yr) = 0d0
            do age = 70, 100
               eo_no_s(sex,23,yr) = eo_no_s(sex,23,yr) + eo_no_s_sy(sex,age,yr)
               eo_no_u(sex,23,yr) = eo_no_u(sex,23,yr) + eo_no_u_sy(sex,age,yr)
            end do
            teo_esf(sex,23,yr) = eo_no_s(sex,23,yr) * rte(sex,23,yr)
         end do
      end do
      
      do yr = startYr, 115
         teo_esf_m16o_x2(yr) = sum(teo_esf(1,1:14,yr)) + teo_esf(1,31,yr)
         teo_esf_f16o_x2(yr) = sum(teo_esf(2,1:14,yr)) + teo_esf(2,31,yr)
         teo_esf_16o_x2(yr) = teo_esf_m16o_x2(yr) + teo_esf_f16o_x2(yr)
      end do
      
      ! Temporary routine to check intermediate values
      !do yr = 80, MAX_YR
         !write(3003,'(2f12.6)') eo_no_s(0,0,yr), eo_no_u(0,0,yr)
         !write(3003,'(170f12.6)') ((eo_no_s_sy(sex,age,yr),age=16,100),sex=1,2)
         !write(3003,'(48f12.6)') (((eo_no_s(sex,grp,yr), eo_no_u(sex,grp,yr)),grp=1,12),sex=1,2)
         !write(3003,'(4f12.6)') (((eo_no_s(sex,grp,yr), eo_no_u(sex,grp,yr)),grp=23,23),sex=1,2)
         !write(3003,'(24f12.6)') ((wtteo(sex,age,yr),age=1,12),sex=1,2)
         !write(3003,'(2f12.6)') wtteo(1:2,23,yr)
         !write(3003,'(24f12.6)') ((teo_esf(sex,age,yr),age=1,12),sex=1,2)
         !write(3003,'(2f12.6)') teo_esf(1:2,23,yr)
      !end do
   
   end subroutine CalculateBaseValues
   
!===============================================================================

   subroutine EstimateTargetWeights()
   
      integer :: yr
   
      do yr = 81, 101
   
         wt1_ls(yr) = teo_esf_16o_x1(yr) / teo_esf_16o_x2(yr) * wt1_ls(yr)
         wt1_is(yr) = teo_esf_16o_x1(yr) / teo_esf_16o_x2(yr) * wt1_is(yr)
         
         wt1_lu(yr) = 1d0 - wt1_lm(yr) - wt1_ls(yr)
         wt1_iu(yr) = 1d0 - wt1_im(yr) - wt1_is(yr)
         
      end do
      
      ! Temporary routine to check intermediate values
      !do yr = 80, MAX_YR
      !   write(3003,'(4f12.6)') wt1_ls(yr), wt1_is(yr), wt1_lu(yr), wt1_iu(yr)
      !end do
   
   end subroutine EstimateTargetWeights
   
!===============================================================================

   subroutine AdjustTargets()
   
      integer :: age, sex, yr
      
      eo_nol_m(0,0,:) = wt1_lm * eo_nol(0,0,:)
      eo_nol_s(0,0,:) = wt1_ls * eo_nol(0,0,:)
      eo_nol_u(0,0,:) = wt1_lu * eo_nol(0,0,:)
      eo_noi_m(0,0,:) = wt1_im * eo_noi(0,0,:)
      eo_noi_s(0,0,:) = wt1_is * eo_noi(0,0,:)
      eo_noi_u(0,0,:) = wt1_iu * eo_noi(0,0,:)
      
      do yr = startYr, 101
         do sex = 1, 2
            do age = 16, 100
               eo_nol_m_sy(sex,age,yr) = eo_no_sy(sex,age,yr) / eo_no(0,0,yr) * eo_nol_m(0,0,yr)
               eo_nol_s_sy(sex,age,yr) = eo_no_sy(sex,age,yr) / eo_no(0,0,yr) * eo_nol_s(0,0,yr)
               eo_nol_u_sy(sex,age,yr) = eo_no_sy(sex,age,yr) / eo_no(0,0,yr) * eo_nol_u(0,0,yr)
               eo_nol_sy(sex,age,yr) =  eo_nol_m_sy(sex,age,yr) + eo_nol_s_sy(sex,age,yr) + eo_nol_u_sy(sex,age,yr)
               eo_noi_m_sy(sex,age,yr) = eo_no_sy(sex,age,yr) / eo_no(0,0,yr) * eo_noi_m(0,0,yr)
               eo_noi_s_sy(sex,age,yr) = eo_no_sy(sex,age,yr) / eo_no(0,0,yr) * eo_noi_s(0,0,yr)
               eo_noi_u_sy(sex,age,yr) = eo_no_sy(sex,age,yr) / eo_no(0,0,yr) * eo_noi_u(0,0,yr)
               eo_noi_sy(sex,age,yr) =  eo_noi_m_sy(sex,age,yr) + eo_noi_s_sy(sex,age,yr) + eo_noi_u_sy(sex,age,yr)
            end do
         end do
      end do
      
      do yr = startYr, 101
         do sex = 1, 2
            eo_nol_m(sex,24,yr) = sum(eo_nol_m_sy(sex,16:100,yr))
            eo_nol_s(sex,24,yr) = sum(eo_nol_s_sy(sex,16:100,yr))
            eo_nol_u(sex,24,yr) = sum(eo_nol_u_sy(sex,16:100,yr))
            eo_nol(sex,24,yr) = sum(eo_nol_sy(sex,16:100,yr))
         end do
         eo_nol_m(0,24,yr) = sum(eo_nol_m(1:2,24,yr))
         eo_nol_s(0,24,yr) = sum(eo_nol_s(1:2,24,yr))
         eo_nol_u(0,24,yr) = sum(eo_nol_u(1:2,24,yr))
         eo_nol(0,24,yr) = sum(eo_nol(1:2,24,yr))
      end do
      
      do yr = startYr, 101
         do sex = 1, 2
            eo_noi_m(sex,24,yr) = sum(eo_noi_m_sy(sex,16:100,yr))
            eo_noi_s(sex,24,yr) = sum(eo_noi_s_sy(sex,16:100,yr))
            eo_noi_u(sex,24,yr) = sum(eo_noi_u_sy(sex,16:100,yr))
            eo_noi(sex,24,yr) = sum(eo_noi_sy(sex,16:100,yr))
         end do
         eo_noi_m(0,24,yr) = sum(eo_noi_m(1:2,24,yr))
         eo_noi_s(0,24,yr) = sum(eo_noi_s(1:2,24,yr))
         eo_noi_u(0,24,yr) = sum(eo_noi_u(1:2,24,yr))
         eo_noi(0,24,yr) = sum(eo_noi(1:2,24,yr))
      end do
      
      ! Temporary routine to check intermediate values
      !do yr = 80, MAX_YR
      !   write(3003,'(6f12.6)') eo_nol_m(0,24,yr), eo_nol_s(0,24,yr), eo_nol_u(0,24,yr), &
      !                          eo_noi_m(0,24,yr), eo_noi_s(0,24,yr), eo_noi_u(0,24,yr)
      !end do
   
   end subroutine AdjustTargets
   
!===============================================================================

   subroutine InitializeCohort()
   
      integer :: age, age1, age2, sex, yr
      real (kind = 8), dimension(MAX_YR) :: w_no1_rpi_m = 1d0
      real (kind = 8), dimension(MAX_YR) :: w_no1_rpi_s = 1d0
      real (kind = 8), dimension(MAX_YR) :: w_no1_rpi_u = 1d0
      real (kind = 8) :: scaleFactor
      
      w_no1_rpi_m(115:117) = (/ 0.717d0, 0.605d0, 0.346d0 /)
      w_no1_rpi_s(115:117) = (/ 0.723d0, 0.617d0, 0.448d0 /)
      w_no1_rpi_u(115:117) = (/ 0.767d0, 0.696d0, 0.562d0 /)
      
      eo_nol_m1_sy(1:2,16:100,101) = eo_nol_m_sy(1:2,16:100,101)
      eo_nol_s1_sy(1:2,16:100,101) = eo_nol_s_sy(1:2,16:100,101)
      eo_nol_u1_sy(1:2,16:100,101) = eo_nol_u_sy(1:2,16:100,101)
      
      eo_noi_m1_sy(1:2,16:100,101) = eo_noi_m_sy(1:2,16:100,101)
      eo_noi_s1_sy(1:2,16:100,101) = eo_noi_s_sy(1:2,16:100,101)
      eo_noi_u1_sy(1:2,16:100,101) = eo_noi_u_sy(1:2,16:100,101)
      
      age2 = 15
      do yr = 102, endYr
         age2 = age2 + 1
         do age = 16, 100
            age1 = age - 1
            do sex = 1, 2
            
               if (age <= age2) then
                  eo_nol_m1_sy(sex,age,yr) = 0
                  eo_nol_s1_sy(sex,age,yr) = 0
                  eo_nol_u1_sy(sex,age,yr) = 0
                  eo_noi_m1_sy(sex,age,yr) = 0
                  eo_noi_s1_sy(sex,age,yr) = 0
                  eo_noi_u1_sy(sex,age,yr) = 0
               else
                  rer_sy(sex,age,yr) = (eo_sy(sex,age,yr) / (nilsy(sex,age,yr) - nildsy(sex,age,yr))) / &
                     (eo_sy(sex,age1,yr-1) / (nilsy(sex,age1,yr-1) - nildsy(sex,age1,yr-1)))
                  eo_nol_m1_sy(sex,age,yr) = eo_nol_m1_sy(sex,age1,yr-1) * (1-qx(sex,age1,yr-1)) * &
                     (1-plv(sex,age,yr)) * rer_sy(sex,age,yr)
                  eo_nol_s1_sy(sex,age,yr) = eo_nol_s1_sy(sex,age1,yr-1) * (1-qx(sex,age1,yr-1)) * &
                     (1-plv(sex,age,yr)) * rer_sy(sex,age,yr)
                  eo_nol_u1_sy(sex,age,yr) = eo_nol_u1_sy(sex,age1,yr-1) * (1-qx(sex,age1,yr-1)) * &
                     (1-plv(sex,age,yr)) * rer_sy(sex,age,yr)
                  eo_noi_m1_sy(sex,age,yr) = eo_noi_m1_sy(sex,age1,yr-1) * (1-qx(sex,age1,yr-1)) * &
                     (1-plv(sex,age,yr)) * rer_sy(sex,age,yr)
                  eo_noi_s1_sy(sex,age,yr) = eo_noi_s1_sy(sex,age1,yr-1) * (1-qx(sex,age1,yr-1)) * &
                     (1-plv(sex,age,yr)) * rer_sy(sex,age,yr)
                  eo_noi_u1_sy(sex,age,yr) = eo_noi_u1_sy(sex,age1,yr-1) * (1-qx(sex,age1,yr-1)) * &
                     (1-plv(sex,age,yr)) * rer_sy(sex,age,yr)
               end if
               eo_nol_1_sy(sex,age,yr)= eo_nol_m1_sy(sex,age,yr) + eo_nol_s1_sy(sex,age,yr) + &
                  eo_nol_u1_sy(sex,age,yr)
               eo_noi_1_sy(sex,age,yr)= eo_noi_m1_sy(sex,age,yr) + eo_noi_s1_sy(sex,age,yr) + &
                  eo_noi_u1_sy(sex,age,yr)
               if (eo_nol_1_sy(sex,age,yr) + eo_noi_1_sy(sex,age,yr) > eo_no_sy(sex,age,yr)) then
                  scaleFactor = eo_no_sy(sex,age,yr) / (eo_nol_1_sy(sex,age,yr) + eo_noi_1_sy(sex,age,yr) + epsilon(0d0))
                  eo_nol_m1_sy(sex,age,yr) = eo_nol_m1_sy(sex,age,yr) * scaleFactor
                  eo_nol_s1_sy(sex,age,yr) = eo_nol_s1_sy(sex,age,yr) * scaleFactor
                  eo_nol_u1_sy(sex,age,yr) = eo_nol_u1_sy(sex,age,yr) * scaleFactor
                  eo_nol_1_sy(sex,age,yr) = eo_nol_1_sy(sex,age,yr) * scaleFactor
                  eo_noi_m1_sy(sex,age,yr) = eo_noi_m1_sy(sex,age,yr) * scaleFactor
                  eo_noi_s1_sy(sex,age,yr) = eo_noi_s1_sy(sex,age,yr) * scaleFactor
                  eo_noi_u1_sy(sex,age,yr) = eo_noi_u1_sy(sex,age,yr) * scaleFactor
                  eo_noi_1_sy(sex,age,yr) = eo_noi_1_sy(sex,age,yr) * scaleFactor
               end if               
            end do
         end do
      end do
      
      ! Temporary routine to check intermediate values
      !do yr = 102, MAX_YR
         !write(3003,'(170f12.6)') ((eo_nol_1_sy(sex,age,yr),age=16,100),sex=1,2)
         !write(3003,'(170f12.6)') ((eo_noi_1_sy(sex,age,yr),age=16,100),sex=1,2)         
      !end do
   
   end subroutine InitializeCohort
   
!===============================================================================

   subroutine ComputeCohorts()
   
      integer :: yr, sex, age
      
      do yr = 102, endYr
         eo_nol_m1(1,24,yr) = sum(eo_nol_m1_sy(1,16:100,yr))
         eo_nol_s1(1,24,yr) = sum(eo_nol_s1_sy(1,16:100,yr))
         eo_nol_u1(1,24,yr) = sum(eo_nol_u1_sy(1,16:100,yr))
         eo_nol_1(1,24,yr) = sum(eo_nol_1_sy(1,16:100,yr))
         eo_nol_m1(2,24,yr) = sum(eo_nol_m1_sy(2,16:100,yr))
         eo_nol_s1(2,24,yr) = sum(eo_nol_s1_sy(2,16:100,yr))
         eo_nol_u1(2,24,yr) = sum(eo_nol_u1_sy(2,16:100,yr))
         eo_nol_1(2,24,yr) = sum(eo_nol_1_sy(2,16:100,yr))
         eo_nol_m1(0,24,yr) = eo_nol_m1(1,24,yr) + eo_nol_m1(2,24,yr)
         eo_nol_s1(0,24,yr) = eo_nol_s1(1,24,yr) + eo_nol_s1(2,24,yr)
         eo_nol_u1(0,24,yr) = eo_nol_u1(1,24,yr) + eo_nol_u1(2,24,yr)
         eo_nol_1(0,24,yr) = eo_nol_1(1,24,yr) + eo_nol_1(2,24,yr)
      end do
      
      do yr = 102, endYr
         eo_noi_m1(1,24,yr) = sum(eo_noi_m1_sy(1,16:100,yr))
         eo_noi_s1(1,24,yr) = sum(eo_noi_s1_sy(1,16:100,yr))
         eo_noi_u1(1,24,yr) = sum(eo_noi_u1_sy(1,16:100,yr))
         eo_noi_1(1,24,yr) = sum(eo_noi_1_sy(1,16:100,yr))
         eo_noi_m1(2,24,yr) = sum(eo_noi_m1_sy(2,16:100,yr))
         eo_noi_s1(2,24,yr) = sum(eo_noi_s1_sy(2,16:100,yr))
         eo_noi_u1(2,24,yr) = sum(eo_noi_u1_sy(2,16:100,yr))
         eo_noi_1(2,24,yr) = sum(eo_noi_1_sy(2,16:100,yr))
         eo_noi_m1(0,24,yr) = eo_noi_m1(1,24,yr) + eo_noi_m1(2,24,yr)
         eo_noi_s1(0,24,yr) = eo_noi_s1(1,24,yr) + eo_noi_s1(2,24,yr)
         eo_noi_u1(0,24,yr) = eo_noi_u1(1,24,yr) + eo_noi_u1(2,24,yr)
         eo_noi_1(0,24,yr) = eo_noi_1(1,24,yr) + eo_noi_1(2,24,yr)
      end do
      
      do yr = 102, endYr
         do sex = 1, 2 
            do age = 16, 100
               eo_no_1_sy(sex,age,yr) = eo_nol_1_sy(sex,age,yr) + eo_noi_1_sy(sex,age,yr)
            end do
            eo_no_1(sex,24,yr) = sum(eo_no_1_sy(sex,16:100,yr))
         end do
         eo_no_1(0,24,yr) = eo_no_1(1,24,yr) + eo_no_1(2,24,yr)
      end do
      
      ! Temporary routine to check intermediate values
      !do yr = 102, MAX_YR
      !   write(3003,'(4f12.6)') eo_nol_m1(0,24,yr), eo_nol_s1(0,24,yr), &
      !      eo_nol_u1(0,24,yr), eo_nol_1(0,24,yr)
      !   write(3003,'(4f12.6)') eo_noi_m1(0,24,yr), eo_noi_s1(0,24,yr), &
      !      eo_noi_u1(0,24,yr), eo_noi_1(0,24,yr)            
      !   write(3003,'(3f12.6)') eo_no_1(1,24,yr), eo_no_1(2,24,yr), &
      !      eo_no_1(0,24,yr)
      !end do
   
   end subroutine ComputeCohorts
   
!===============================================================================

   subroutine ComputeWeights()
   
      integer :: sex, age, yr

      adj2013 = 0.80d0
      adj2013(111:119)  = (/ 0.588d0, 0.476d0, 0.508d0, 0.55d0, 0.60d0, 0.60d0, 0.685d0, 0.735d0, 0.78d0 /)
      
      wt2_l  = (0.0 + 0.125d0 + 0.150d0)/((0.0 + 0.125d0 + 0.150d0) + (0.0 + 0.125d0 + 0.250d0))
      wt2_lm = 0.0
      wt2_ls = (0.125d0 * 0.7d0 * adj2013)/ (0.0 + 0.125d0 + 0.150d0)
      wt2_lu = 1 - wt2_lm - wt2_ls
      
      wt2_i  = 1d0 - wt2_l
      wt2_im = 0.0
      wt2_is = (0.125d0 * 0.7d0 * adj2013)/ (0.0 + 0.125d0 + 0.250d0)
      wt2_iu = 1 - wt2_im - wt2_is
      
      do yr = 102, endYr
         do sex = 1, 2 
            do age = 16, 100
               eo_no_2_sy(sex,age,yr) = eo_no_sy(sex,age,yr) - eo_no_1_sy(sex,age,yr)
            end do
            eo_no_2(sex,24,yr) = sum(eo_no_2_sy(sex,16:100,yr))
         end do
         eo_no_2(0,24,yr) = eo_no_2(1,24,yr) + eo_no_2(2,24,yr)
         eo_no_2(0,0,yr) = eo_no_2(0,24,yr)
         eo_nol_2(0,0,yr) = wt2_l(yr) * eo_no_2(0,0,yr)
         eo_noi_2(0,0,yr) = wt2_i(yr) * eo_no_2(0,0,yr)
      end do
      
      ! Temporary routine to check intermediate values
      !do yr = 102, MAX_YR
      !   write(3003,'(3f12.6)') eo_no_2(1,24,yr), eo_no_2(2,24,yr), &
      !      eo_no_2(0,24,yr)
      !   write(3003,'(2f12.6)') eo_nol_2(0,0,yr), eo_noi_2(0,0,yr)
      !end do
   
   end subroutine ComputeWeights
   
!===============================================================================

   subroutine ControlHistoricalValues()
   
      integer :: yr, sex, age, grp
      integer, dimension(14) :: lowAge =  (/ 16, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75 /)
      integer, dimension(14) :: highAge = (/ 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79 /)
      
      wt2_ls_temp = wt2_ls
      wt2_is_temp = wt2_is
      wt2_lu_temp = wt2_lu
      wt2_iu_temp = wt2_iu
      
      do yr = 102, 117
      
         eo_no_s2(0,0,yr) = wt2_ls(yr) * eo_nol_2(0,0,yr) + wt2_is(yr) * eo_noi_2(0,0,yr)
         eo_no_u2(0,0,yr) = wt2_lu(yr) * eo_nol_2(0,0,yr) + wt2_iu(yr) * eo_noi_2(0,0,yr)
         
         do sex = 1, 2
         
            do age = 16, 100
               eo_no_s2_sy(sex,age,yr) = eo_no_2_sy(sex,age,yr) / eo_no_2(0,0,yr) * eo_no_s2(0,0,yr)
               eo_no_u2_sy(sex,age,yr) = eo_no_2_sy(sex,age,yr) / eo_no_2(0,0,yr) * eo_no_u2(0,0,yr)
            end do
         
            do grp = 1, 14
               eo_no_s2(sex,grp,yr) = 0
               eo_no_u2(sex,grp,yr) = 0
               do age = lowAge(grp), highAge(grp)
                  eo_no_s2(sex,grp,yr) = eo_no_s2(sex,grp,yr) + eo_no_s2_sy(sex,age,yr)
                  eo_no_u2(sex,grp,yr) = eo_no_u2(sex,grp,yr) + eo_no_u2_sy(sex,age,yr)
               end do
            end do

            eo_no_s2(sex,31,yr) = 0
            eo_no_u2(sex,31,yr) = 0
            do age = 80, 100
               eo_no_s2(sex,31,yr) = eo_no_s2(sex,31,yr) + eo_no_s2_sy(sex,age,yr)
               eo_no_u2(sex,31,yr) = eo_no_u2(sex,31,yr) + eo_no_u2_sy(sex,age,yr)
            end do
            eo_no_s2(sex,23,yr) = 0
            eo_no_u2(sex,23,yr) = 0
            do age = 70, 100
               eo_no_s2(sex,23,yr) = eo_no_s2(sex,23,yr) + eo_no_s2_sy(sex,age,yr)
               eo_no_u2(sex,23,yr) = eo_no_u2(sex,23,yr) + eo_no_u2_sy(sex,age,yr)
            end do
            
            do grp = 1, 14
               eo_nol_s1(sex,grp,yr) = 0
               eo_nol_u1(sex,grp,yr) = 0
               eo_noi_s1(sex,grp,yr) = 0
               eo_noi_u1(sex,grp,yr) = 0
               do age = lowAge(grp), highAge(grp)
                  eo_nol_s1(sex,grp,yr) = eo_nol_s1(sex,grp,yr) + eo_nol_s1_sy(sex,age,yr)
                  eo_nol_u1(sex,grp,yr) = eo_nol_u1(sex,grp,yr) + eo_nol_u1_sy(sex,age,yr)
                  eo_noi_s1(sex,grp,yr) = eo_noi_s1(sex,grp,yr) + eo_noi_s1_sy(sex,age,yr)
                  eo_noi_u1(sex,grp,yr) = eo_noi_u1(sex,grp,yr) + eo_noi_u1_sy(sex,age,yr)
               end do
            end do

            eo_nol_s1(sex,31,yr) = 0
            eo_nol_u1(sex,31,yr) = 0
            eo_noi_s1(sex,31,yr) = 0
            eo_noi_u1(sex,31,yr) = 0
            do age = 80, 100
               eo_nol_s1(sex,31,yr) = eo_nol_s1(sex,31,yr) + eo_nol_s1_sy(sex,age,yr)
               eo_nol_u1(sex,31,yr) = eo_nol_u1(sex,31,yr) + eo_nol_u1_sy(sex,age,yr)
               eo_noi_s1(sex,31,yr) = eo_noi_s1(sex,31,yr) + eo_noi_s1_sy(sex,age,yr)
               eo_noi_u1(sex,31,yr) = eo_noi_u1(sex,31,yr) + eo_noi_u1_sy(sex,age,yr)
            end do
            eo_nol_s1(sex,23,yr) = 0
            eo_nol_u1(sex,23,yr) = 0
            eo_noi_s1(sex,23,yr) = 0
            eo_noi_u1(sex,23,yr) = 0
            do age = 70, 100
               eo_nol_s1(sex,23,yr) = eo_nol_s1(sex,23,yr) + eo_nol_s1_sy(sex,age,yr)
               eo_nol_u1(sex,23,yr) = eo_nol_u1(sex,23,yr) + eo_nol_u1_sy(sex,age,yr)
               eo_noi_s1(sex,23,yr) = eo_noi_s1(sex,23,yr) + eo_noi_s1_sy(sex,age,yr)
               eo_noi_u1(sex,23,yr) = eo_noi_u1(sex,23,yr) + eo_noi_u1_sy(sex,age,yr)
            end do
            
            do grp = 1, 14
               teo_esf(sex,grp,yr) = (eo_nol_s1(sex,grp,yr) + eo_noi_s1(sex,grp,yr) + &
                  eo_no_s2(sex,grp,yr))* rte(sex,grp,yr)
               teo_esf_1(sex,grp,yr) = (eo_nol_s1(sex,grp,yr) + eo_noi_s1(sex,grp,yr))* rte(sex,grp,yr)
            end do
            teo_esf(sex,31,yr) = (eo_nol_s1(sex,31,yr) + eo_noi_s1(sex,31,yr) + &
               eo_no_s2(sex,31,yr))* rte(sex,31,yr)
           teo_esf_1(sex,31,yr) = (eo_nol_s1(sex,31,yr) + eo_noi_s1(sex,31,yr))* rte(sex,31,yr)
           teo_esf(sex,23,yr) = (eo_nol_s1(sex,23,yr) + eo_noi_s1(sex,23,yr) + &
               eo_no_s2(sex,23,yr))* rte(sex,23,yr)
           teo_esf_1(sex,23,yr) = (eo_nol_s1(sex,23,yr) + eo_noi_s1(sex,23,yr))* rte(sex,23,yr)
           
         end do ! sex
         
         teo_esf_m16o_x2(yr) = sum(teo_esf(1,1:14,yr)) + teo_esf(1,31,yr)
         teo_esf_f16o_x2(yr) = sum(teo_esf(2,1:14,yr)) + teo_esf(2,31,yr)
         teo_esf_16o_x2(yr) = teo_esf_m16o_x2(yr)+ teo_esf_f16o_x2(yr)
         
         teo_esf_m16o_x3(yr) = sum(teo_esf_1(1,1:14,yr)) + teo_esf_1(1,31,yr)
         teo_esf_f16o_x3(yr) = sum(teo_esf_1(2,1:14,yr)) + teo_esf_1(2,31,yr)
         teo_esf_16o_x3(yr) = teo_esf_m16o_x3(yr)+ teo_esf_f16o_x3(yr)
         
         wt2_ls(yr) = wt2_ls_temp(yr) * (teo_esf_16o_x1(yr) - teo_esf_16o_x3(yr)) / &
            (teo_esf_16o_x2(yr) - teo_esf_16o_x3(yr))
         wt2_is(yr) = wt2_is_temp(yr) * (teo_esf_16o_x1(yr) - teo_esf_16o_x3(yr)) / &
            (teo_esf_16o_x2(yr) - teo_esf_16o_x3(yr))
            
         wt2_lu(yr) = 1d0 - wt2_lm(yr) - wt2_ls(yr)
         wt2_iu(yr) = 1d0 - wt2_im(yr) - wt2_is(yr)
         
      end do ! yr
      
      ! Temporary routine to check intermediate values
      !do yr = 102, endYr
         !write(3003,'(2f12.6)') teo_esf_16o_x2(yr), teo_esf_16o_x3(yr)
         !write(3003,'(4f12.6)') wt2_ls(yr),wt2_is(yr),wt2_lu(yr),wt2_iu(yr)
      !end do
   
   end subroutine ControlHistoricalValues
   
!===============================================================================

   subroutine AdjustValues()
   
      integer :: yr, sex, age
      
      do yr = 102, endYr
      
         eo_nol_m2(0,0,yr) = wt2_lm(yr) * eo_nol_2(0,0,yr)
         eo_nol_s2(0,0,yr) = wt2_ls(yr) * eo_nol_2(0,0,yr)
         eo_nol_u2(0,0,yr) = wt2_lu(yr) * eo_nol_2(0,0,yr)
         
         eo_noi_m2(0,0,yr) = wt2_im(yr) * eo_noi_2(0,0,yr)
         eo_noi_s2(0,0,yr) = wt2_is(yr) * eo_noi_2(0,0,yr)
         eo_noi_u2(0,0,yr) = wt2_iu(yr) * eo_noi_2(0,0,yr)
         
         do sex = 1, 2
         
            do age = 16, 100
            
               eo_nol_m2_sy(sex,age,yr) = eo_no_2_sy(sex,age,yr) / eo_no_2(0,0,yr) * eo_nol_m2(0,0,yr)
               eo_nol_s2_sy(sex,age,yr) = eo_no_2_sy(sex,age,yr) / eo_no_2(0,0,yr) * eo_nol_s2(0,0,yr)
               eo_nol_u2_sy(sex,age,yr) = eo_no_2_sy(sex,age,yr) / eo_no_2(0,0,yr) * eo_nol_u2(0,0,yr)
               eo_nol_2_sy(sex,age,yr) = eo_nol_m2_sy(sex,age,yr) + eo_nol_s2_sy(sex,age,yr) + eo_nol_u2_sy(sex,age,yr)

               eo_noi_m2_sy(sex,age,yr) = eo_no_2_sy(sex,age,yr) / eo_no_2(0,0,yr) * eo_noi_m2(0,0,yr)
               eo_noi_s2_sy(sex,age,yr) = eo_no_2_sy(sex,age,yr) / eo_no_2(0,0,yr) * eo_noi_s2(0,0,yr)
               eo_noi_u2_sy(sex,age,yr) = eo_no_2_sy(sex,age,yr) / eo_no_2(0,0,yr) * eo_noi_u2(0,0,yr)
               eo_noi_2_sy(sex,age,yr) = eo_noi_m2_sy(sex,age,yr) + eo_noi_s2_sy(sex,age,yr) + eo_noi_u2_sy(sex,age,yr)
               
               eo_nol_2(sex,24,yr) = eo_nol_2(sex,24,yr) + eo_nol_2_sy(sex,age,yr)
               eo_noi_2(sex,24,yr) = eo_noi_2(sex,24,yr) + eo_noi_2_sy(sex,age,yr)
               eo_nol_m2(sex,24,yr) = eo_nol_m2(sex,24,yr) + eo_nol_m2_sy(sex,age,yr)
               eo_noi_m2(sex,24,yr) = eo_noi_m2(sex,24,yr) + eo_noi_m2_sy(sex,age,yr)
               eo_nol_s2(sex,24,yr) = eo_nol_s2(sex,24,yr) + eo_nol_s2_sy(sex,age,yr)
               eo_noi_s2(sex,24,yr) = eo_noi_s2(sex,24,yr) + eo_noi_s2_sy(sex,age,yr)
               eo_nol_u2(sex,24,yr) = eo_nol_u2(sex,24,yr) + eo_nol_u2_sy(sex,age,yr)
               eo_noi_u2(sex,24,yr) = eo_noi_u2(sex,24,yr) + eo_noi_u2_sy(sex,age,yr)


            end do ! age
            
            eo_nol_2(0,24,yr) = eo_nol_2(0,24,yr) + eo_nol_2(sex,24,yr)
            eo_noi_2(0,24,yr) = eo_noi_2(0,24,yr) + eo_noi_2(sex,24,yr)
            eo_nol_m2(0,24,yr) = eo_nol_m2(0,24,yr) + eo_nol_m2(sex,24,yr)
            eo_noi_m2(0,24,yr) = eo_noi_m2(0,24,yr) + eo_noi_m2(sex,24,yr)
            eo_nol_s2(0,24,yr) = eo_nol_s2(0,24,yr) + eo_nol_s2(sex,24,yr)
            eo_noi_s2(0,24,yr) = eo_noi_s2(0,24,yr) + eo_noi_s2(sex,24,yr)
            eo_nol_u2(0,24,yr) = eo_nol_u2(0,24,yr) + eo_nol_u2(sex,24,yr)
            eo_noi_u2(0,24,yr) = eo_noi_u2(0,24,yr) + eo_noi_u2(sex,24,yr)
            
         end do ! sex
      
      end do ! yr
      
      ! Temporary routine to check intermediate values
      !do yr = 102, endYr
      !   write(3003,'(8f12.6)') eo_nol_2(0,24,yr),eo_nol_m2(0,24,yr),eo_nol_s2(0,24,yr),eo_nol_u2(0,24,yr),&
      !                          eo_noi_2(0,24,yr),eo_noi_m2(0,24,yr),eo_noi_s2(0,24,yr),eo_noi_u2(0,24,yr)
      !end do
   
   end subroutine AdjustValues
   
!===============================================================================

   subroutine ComputeAuthorizedTotals()
   
      integer :: yr, sex, age
      
      do yr = 102, endYr
         do sex = 1, 2
            do age = 16, 100
         
               eo_nol_m_sy(sex,age,yr) = eo_nol_m1_sy(sex,age,yr) + eo_nol_m2_sy(sex,age,yr)
               eo_nol_s_sy(sex,age,yr) = eo_nol_s1_sy(sex,age,yr) + eo_nol_s2_sy(sex,age,yr)
               eo_nol_u_sy(sex,age,yr) = eo_nol_u1_sy(sex,age,yr) + eo_nol_u2_sy(sex,age,yr)
               eo_nol_sy(sex,age,yr) = eo_nol_1_sy(sex,age,yr) + eo_nol_2_sy(sex,age,yr)
               
               eo_nol_m(sex,24,yr) = eo_nol_m(sex,24,yr) + eo_nol_m_sy(sex,age,yr)
               eo_nol_s(sex,24,yr) = eo_nol_s(sex,24,yr) + eo_nol_s_sy(sex,age,yr)
               eo_nol_u(sex,24,yr) = eo_nol_u(sex,24,yr) + eo_nol_u_sy(sex,age,yr)
               eo_nol(sex,24,yr) = eo_nol(sex,24,yr) + eo_nol_sy(sex,age,yr)
               
            end do ! age
            
            eo_nol_m(0,24,yr) = eo_nol_m(0,24,yr) + eo_nol_m(sex,24,yr)
            eo_nol_s(0,24,yr) = eo_nol_s(0,24,yr) + eo_nol_s(sex,24,yr)
            eo_nol_u(0,24,yr) = eo_nol_u(0,24,yr) + eo_nol_u(sex,24,yr)
            eo_nol(0,24,yr) = eo_nol(0,24,yr) + eo_nol(sex,24,yr)
            
         end do ! sex
      end do ! yr
      
      !Temporary routine to check intermediate values
      !do yr = 102, endYr
      !   write(3003,'(4f12.6)') eo_nol_m(0,24,yr),eo_nol_s(0,24,yr),eo_nol_u(0,24,yr),eo_nol(0,24,yr)
      !end do
   
   end subroutine ComputeAuthorizedTotals
   
!===============================================================================

   subroutine ComputeUnauthorizedTotals()
   
      integer :: yr, sex, age
      
      do yr = 102, endYr
         do sex = 1, 2
            do age = 16, 100
         
               eo_noi_m_sy(sex,age,yr) = eo_noi_m1_sy(sex,age,yr) + eo_noi_m2_sy(sex,age,yr)
               eo_noi_s_sy(sex,age,yr) = eo_noi_s1_sy(sex,age,yr) + eo_noi_s2_sy(sex,age,yr)
               eo_noi_u_sy(sex,age,yr) = eo_noi_u1_sy(sex,age,yr) + eo_noi_u2_sy(sex,age,yr)
               eo_noi_sy(sex,age,yr) = eo_noi_1_sy(sex,age,yr) + eo_noi_2_sy(sex,age,yr)
               
               eo_noi_m(sex,24,yr) = eo_noi_m(sex,24,yr) + eo_noi_m_sy(sex,age,yr)
               eo_noi_s(sex,24,yr) = eo_noi_s(sex,24,yr) + eo_noi_s_sy(sex,age,yr)
               eo_noi_u(sex,24,yr) = eo_noi_u(sex,24,yr) + eo_noi_u_sy(sex,age,yr)
               eo_noi(sex,24,yr) = eo_noi(sex,24,yr) + eo_noi_sy(sex,age,yr)
               
            end do ! age
            
            eo_noi_m(0,24,yr) = eo_noi_m(0,24,yr) + eo_noi_m(sex,24,yr)
            eo_noi_s(0,24,yr) = eo_noi_s(0,24,yr) + eo_noi_s(sex,24,yr)
            eo_noi_u(0,24,yr) = eo_noi_u(0,24,yr) + eo_noi_u(sex,24,yr)
            eo_noi(0,24,yr) = eo_noi(0,24,yr) + eo_noi(sex,24,yr)
            
         end do ! sex
      end do ! yr
      
      !Temporary routine to check intermediate values
      !do yr = 102, endYr
         !write(3003,'(4f12.6)') eo_noi_m(0,24,yr),eo_noi_s(0,24,yr),eo_noi_u(0,24,yr),eo_noi(0,24,yr)
         !write(3003,'(3f12.6)') eo_no(0,0,yr),eo_nol(0,24,yr),eo_noi(0,24,yr)
      !end do
   
   end subroutine ComputeUnauthorizedTotals
   
!===============================================================================

   subroutine SumByPosting()
   
      integer :: age, sex, yr
      
      do yr = 50, endYr
         do sex = 1, 2
            do age = 16, 100
               eo_mef_sy(sex,age,yr)  = eo_asf1_sy(sex,age,yr)  &
                                      + eo_asj1_sy(sex,age,yr)  &
                                      + eo_asf2_sy(sex,age,yr)  &
                                      + eo_asj2_sy(sex,age,yr)  &
                                      + eo_awj_sy(sex,age,yr)   &
                                      + eo_awh_sy(sex,age,yr)   &
                                      + eo_awt_sy(sex,age,yr)   &
                                      + eo_awtfa_sy(sex,age,yr) &
                                      + eo_awjf_sy(sex,age,yr)  &

                                      + eo_nasf1_sy(sex,age,yr) &
                                      + eo_nasj1_sy(sex,age,yr) &
                                      + eo_nasf2_sy(sex,age,yr) &
                                      + eo_nasj2_sy(sex,age,yr) &
                                      + eo_nawj_sy(sex,age,yr)  &
                                      + eo_nawh_sy(sex,age,yr)  &
                                      + eo_nawt_sy(sex,age,yr)  &
                                      + eo_nawjf_sy(sex,age,yr) &
                                      + eo_nol_m_sy(sex,age,yr) &
                                      + eo_noi_m_sy(sex,age,yr)
               
               ! if (yr >= 117) eo_mef_sy(sex,age,yr) = eo_mef_sy(sex,age,yr) + eo_nawtf_sy(sex,age,yr)
               
               eo_mefc_sy(sex,age,yr) = eo_asf2_sy(sex,age,yr)  &
                                      + eo_asj2_sy(sex,age,yr)  &
                                      + eo_awt_sy(sex,age,yr)   &
                                      + eo_awtfa_sy(sex,age,yr) &
                                      + eo_awjf_sy(sex,age,yr)  &

                                      + eo_nasf1_sy(sex,age,yr) &
                                      + eo_nasj1_sy(sex,age,yr) &
                                      + eo_nasf2_sy(sex,age,yr) &
                                      + eo_nasj2_sy(sex,age,yr) &
                                      + eo_nawj_sy(sex,age,yr)  &
                                      + eo_nawh_sy(sex,age,yr)  &
                                      + eo_nawt_sy(sex,age,yr)  &
                                      + eo_nawjf_sy(sex,age,yr) &
                                      + eo_nol_m_sy(sex,age,yr) &
                                      + eo_noi_m_sy(sex,age,yr)
               
               ! if (yr >= 117) eo_mefc_sy(sex,age,yr) = eo_mefc_sy(sex,age,yr) + eo_nawtf_sy(sex,age,yr)
               
               eo_esf_sy(sex,age,yr)  = eo_nol_s_sy(sex,age,yr) + eo_noi_s_sy(sex,age,yr)
               eo_und_sy(sex,age,yr)  = eo_nol_u_sy(sex,age,yr) + eo_noi_u_sy(sex,age,yr) + &
                                        eo_awtfn_sy(sex,age,yr) + eo_nawtf_sy(sex,age,yr)
            end do
         end do
      end do
      
      do yr = 64, endYr
         do sex = 1, 2
            do age = 16, 100
               eo_mef(sex,24,yr)  = eo_mef(sex,24,yr)  + eo_mef_sy(sex,age,yr)
               eo_mefc(sex,24,yr) = eo_mefc(sex,24,yr) + eo_mefc_sy(sex,age,yr)
               eo_esf(sex,24,yr)  = eo_esf(sex,24,yr)  + eo_esf_sy(sex,age,yr)
               eo_und(sex,24,yr) = eo_und(sex,24,yr) + eo_und_sy(sex,age,yr)
            end do
            eo_mef(0,24,yr)  = eo_mef(0,24,yr)  + eo_mef(sex,24,yr)
            eo_mefc(0,24,yr) = eo_mefc(0,24,yr) + eo_mefc(sex,24,yr)
            eo_esf(0,24,yr)  = eo_esf(0,24,yr)  + eo_esf(sex,24,yr)
            eo_und(0,24,yr)  = eo_und(0,24,yr) + eo_und(sex,24,yr)
         end do
      end do
      
      !Temporary routine to check intermediate values
      !do yr = 80, endYr
      !  write(3003,'(4f12.6)') eo_mef(0,24,yr),eo_mefc(0,24,yr),eo_esf(0,24,yr),&
      !      eo_und(0,24,yr)
      !end do
   
   end subroutine SumByPosting
   
!===============================================================================

   subroutine SumAgeGroups()
   
      integer :: age, sex, yr, grp
      integer, dimension(27) :: lowAge =  (/ 16, 18, 20, 25, 30, 35, 40, 45, &
         50, 55, 60, 65, 70, 75, 80, 85, 90, 95,  75,  70,  65, 16, 25, 35, 45, 55, 16 /)
      integer, dimension(27) :: highAge = (/ 17, 19, 24, 29, 34, 39, 44, 49, &
         54, 59, 64, 69, 74, 79, 84, 89, 94, 99, 100, 100, 100, 19, 34, 44, 54, 64, 100 /)
      integer, dimension(27) :: ageGrp = (/ 1, 2, 3, 4, 5, 6, 7, 8, &
         9, 10, 11, 12, 13, 14, 15, 32, 33, 34, 17, 23, 22, 27, 18, &
        19, 20, 21, 24 /)
         
      do yr = 64, endYr
         do sex = 1, 2
            do grp = 1, 27
               
               eo_asf1(sex,ageGrp(grp),yr) = 0
               eo_asf2(sex,ageGrp(grp),yr) = 0
               eo_asj1(sex,ageGrp(grp),yr) = 0
               eo_asj2(sex,ageGrp(grp),yr) = 0
               eo_awj(sex,ageGrp(grp),yr) = 0
               eo_awjf(sex,ageGrp(grp),yr) = 0
               eo_awh(sex,ageGrp(grp),yr) = 0
               eo_awt(sex,ageGrp(grp),yr) = 0
               eo_awtf(sex,ageGrp(grp),yr) = 0
               eo_awtfa(sex,ageGrp(grp),yr) = 0
               eo_awtfn(sex,ageGrp(grp),yr) = 0
               eo_as1(sex,ageGrp(grp),yr) = 0
               eo_as2(sex,ageGrp(grp),yr) = 0
               eo_aw1(sex,ageGrp(grp),yr) = 0
               eo_aw2(sex,ageGrp(grp),yr) = 0

               eo_as(sex,ageGrp(grp),yr) = 0
               eo_aw(sex,ageGrp(grp),yr) = 0
               eo_a(sex,ageGrp(grp),yr) = 0
               
               eo_nasf1(sex,ageGrp(grp),yr) = 0
               eo_nasf2(sex,ageGrp(grp),yr) = 0
               eo_nasj1(sex,ageGrp(grp),yr) = 0
               eo_nasj2(sex,ageGrp(grp),yr) = 0
               eo_nawj(sex,ageGrp(grp),yr) = 0
               eo_nawjf(sex,ageGrp(grp),yr) = 0
               eo_nawh(sex,ageGrp(grp),yr) = 0
               eo_nawt(sex,ageGrp(grp),yr) = 0
               eo_nawtf(sex,ageGrp(grp),yr) = 0
               eo_nas1(sex,ageGrp(grp),yr) = 0
               eo_nas2(sex,ageGrp(grp),yr) = 0
               eo_naw1(sex,ageGrp(grp),yr) = 0
               eo_naw2(sex,ageGrp(grp),yr) = 0
               
               eo_nas(sex,ageGrp(grp),yr) = 0
               eo_naw(sex,ageGrp(grp),yr) = 0
               eo_na(sex,ageGrp(grp),yr) = 0
               
               eo_noi(sex,ageGrp(grp),yr) = 0
               eo_noi_1(sex,ageGrp(grp),yr) = 0
               eo_noi_2(sex,ageGrp(grp),yr) = 0
               eo_noi_m1(sex,ageGrp(grp),yr) = 0
               eo_noi_m2(sex,ageGrp(grp),yr) = 0
               eo_noi_m(sex,ageGrp(grp),yr) = 0
               eo_noi_s1(sex,ageGrp(grp),yr) = 0
               eo_noi_s2(sex,ageGrp(grp),yr) = 0
               eo_noi_s(sex,ageGrp(grp),yr) = 0
               eo_noi_u1(sex,ageGrp(grp),yr) = 0
               eo_noi_u2(sex,ageGrp(grp),yr) = 0
               eo_noi_u(sex,ageGrp(grp),yr) = 0
               
               eo_nol(sex,ageGrp(grp),yr) = 0
               eo_nol_1(sex,ageGrp(grp),yr) = 0
               eo_nol_2(sex,ageGrp(grp),yr) = 0
               eo_nol_m1(sex,ageGrp(grp),yr) = 0
               eo_nol_m2(sex,ageGrp(grp),yr) = 0
               eo_nol_m(sex,ageGrp(grp),yr) = 0
               eo_nol_s1(sex,ageGrp(grp),yr) = 0
               eo_nol_s2(sex,ageGrp(grp),yr) = 0
               eo_nol_s(sex,ageGrp(grp),yr) = 0
               eo_nol_u1(sex,ageGrp(grp),yr) = 0
               eo_nol_u2(sex,ageGrp(grp),yr) = 0
               eo_nol_u(sex,ageGrp(grp),yr) = 0
               
               eo_no_1(sex,ageGrp(grp),yr) = 0
               eo_no_2(sex,ageGrp(grp),yr) = 0
               eo_no(sex,ageGrp(grp),yr) = 0
               
               eo_mef(sex,ageGrp(grp),yr) = 0
               eo_mefc(sex,ageGrp(grp),yr) = 0
               eo_esf(sex,ageGrp(grp),yr) = 0
               eo_und(sex,ageGrp(grp),yr) = 0
               
               eo(sex,ageGrp(grp),yr) = 0
               
               do age = lowAge(grp), highAge(grp)
                  
                  eo_asf1(sex,ageGrp(grp),yr)  = eo_asf1(sex,ageGrp(grp),yr)  + eo_asf1_sy(sex,age,yr)
                  eo_asf2(sex,ageGrp(grp),yr)  = eo_asf2(sex,ageGrp(grp),yr)  + eo_asf2_sy(sex,age,yr)
                  eo_asj1(sex,ageGrp(grp),yr)  = eo_asj1(sex,ageGrp(grp),yr)  + eo_asj1_sy(sex,age,yr)
                  eo_asj2(sex,ageGrp(grp),yr)  = eo_asj2(sex,ageGrp(grp),yr)  + eo_asj2_sy(sex,age,yr)
                  eo_awj(sex,ageGrp(grp),yr)   = eo_awj(sex,ageGrp(grp),yr)   + eo_awj_sy(sex,age,yr)
                  eo_awjf(sex,ageGrp(grp),yr)  = eo_awjf(sex,ageGrp(grp),yr)  + eo_awjf_sy(sex,age,yr)
                  eo_awh(sex,ageGrp(grp),yr)   = eo_awh(sex,ageGrp(grp),yr)   + eo_awh_sy(sex,age,yr)
                  eo_awt(sex,ageGrp(grp),yr)   = eo_awt(sex,ageGrp(grp),yr)   + eo_awt_sy(sex,age,yr)
                  eo_awtf(sex,ageGrp(grp),yr)  = eo_awtf(sex,ageGrp(grp),yr)  + eo_awtf_sy(sex,age,yr)
                  eo_awtfa(sex,ageGrp(grp),yr) = eo_awtfa(sex,ageGrp(grp),yr) + eo_awtfa_sy(sex,age,yr)
                  eo_awtfn(sex,ageGrp(grp),yr) = eo_awtfn(sex,ageGrp(grp),yr) + eo_awtfn_sy(sex,age,yr)
                  eo_as1(sex,ageGrp(grp),yr)   = eo_as1(sex,ageGrp(grp),yr)   + eo_as1_sy(sex,age,yr)
                  eo_as2(sex,ageGrp(grp),yr)   = eo_as2(sex,ageGrp(grp),yr)   + eo_as2_sy(sex,age,yr)
                  eo_aw1(sex,ageGrp(grp),yr)   = eo_aw1(sex,ageGrp(grp),yr)   + eo_aw1_sy(sex,age,yr)
                  eo_aw2(sex,ageGrp(grp),yr)   = eo_aw2(sex,ageGrp(grp),yr)   + eo_aw2_sy(sex,age,yr)
               
                  eo_as(sex,ageGrp(grp),yr) = eo_as(sex,ageGrp(grp),yr) + eo_as_sy(sex,age,yr)
                  eo_aw(sex,ageGrp(grp),yr) = eo_aw(sex,ageGrp(grp),yr) + eo_aw_sy(sex,age,yr)
                  eo_a(sex,ageGrp(grp),yr) = eo_a(sex,ageGrp(grp),yr) + eo_a_sy(sex,age,yr)
                  
                  eo_nasf1(sex,ageGrp(grp),yr)  = eo_nasf1(sex,ageGrp(grp),yr)  + eo_nasf1_sy(sex,age,yr)
                  eo_nasf2(sex,ageGrp(grp),yr)  = eo_nasf2(sex,ageGrp(grp),yr)  + eo_nasf2_sy(sex,age,yr)
                  eo_nasj1(sex,ageGrp(grp),yr)  = eo_nasj1(sex,ageGrp(grp),yr)  + eo_nasj1_sy(sex,age,yr)
                  eo_nasj2(sex,ageGrp(grp),yr)  = eo_nasj2(sex,ageGrp(grp),yr)  + eo_nasj2_sy(sex,age,yr)
                  eo_nawj(sex,ageGrp(grp),yr)   = eo_nawj(sex,ageGrp(grp),yr)   + eo_nawj_sy(sex,age,yr)
                  eo_nawjf(sex,ageGrp(grp),yr)  = eo_nawjf(sex,ageGrp(grp),yr)  + eo_nawjf_sy(sex,age,yr)
                  eo_nawh(sex,ageGrp(grp),yr)   = eo_nawh(sex,ageGrp(grp),yr)   + eo_nawh_sy(sex,age,yr)
                  eo_nawt(sex,ageGrp(grp),yr)   = eo_nawt(sex,ageGrp(grp),yr)   + eo_nawt_sy(sex,age,yr)
                  eo_nawtf(sex,ageGrp(grp),yr)  = eo_nawtf(sex,ageGrp(grp),yr)  + eo_nawtf_sy(sex,age,yr)
                  eo_nas1(sex,ageGrp(grp),yr)   = eo_nas1(sex,ageGrp(grp),yr)   + eo_nas1_sy(sex,age,yr)
                  eo_nas2(sex,ageGrp(grp),yr)   = eo_nas2(sex,ageGrp(grp),yr)   + eo_nas2_sy(sex,age,yr)
                  eo_naw1(sex,ageGrp(grp),yr)   = eo_naw1(sex,ageGrp(grp),yr)   + eo_naw1_sy(sex,age,yr)
                  eo_naw2(sex,ageGrp(grp),yr)   = eo_naw2(sex,ageGrp(grp),yr)   + eo_naw2_sy(sex,age,yr)
                  
                  eo_nas(sex,ageGrp(grp),yr) = eo_nas(sex,ageGrp(grp),yr) + eo_nas_sy(sex,age,yr)
                  eo_naw(sex,ageGrp(grp),yr) = eo_naw(sex,ageGrp(grp),yr) + eo_naw_sy(sex,age,yr)
                  eo_na(sex,ageGrp(grp),yr) = eo_na(sex,ageGrp(grp),yr) + eo_na_sy(sex,age,yr)
                  
                  eo_noi(sex,ageGrp(grp),yr) = eo_noi(sex,ageGrp(grp),yr) + eo_noi_sy(sex,age,yr)
                  eo_noi_1(sex,ageGrp(grp),yr) = eo_noi_1(sex,ageGrp(grp),yr) + eo_noi_1_sy(sex,age,yr)
                  eo_noi_2(sex,ageGrp(grp),yr) = eo_noi_2(sex,ageGrp(grp),yr) + eo_noi_2_sy(sex,age,yr)
                  eo_noi_m1(sex,ageGrp(grp),yr) = eo_noi_m1(sex,ageGrp(grp),yr) + eo_noi_m1_sy(sex,age,yr)
                  eo_noi_m2(sex,ageGrp(grp),yr) = eo_noi_m2(sex,ageGrp(grp),yr) + eo_noi_m2_sy(sex,age,yr)
                  eo_noi_m(sex,ageGrp(grp),yr) = eo_noi_m(sex,ageGrp(grp),yr) + eo_noi_m_sy(sex,age,yr)
                  eo_noi_s1(sex,ageGrp(grp),yr) = eo_noi_s1(sex,ageGrp(grp),yr) + eo_noi_s1_sy(sex,age,yr)
                  eo_noi_s2(sex,ageGrp(grp),yr) = eo_noi_s2(sex,ageGrp(grp),yr) + eo_noi_s2_sy(sex,age,yr)
                  eo_noi_s(sex,ageGrp(grp),yr) = eo_noi_s(sex,ageGrp(grp),yr) + eo_noi_s_sy(sex,age,yr)
                  eo_noi_u1(sex,ageGrp(grp),yr) = eo_noi_u1(sex,ageGrp(grp),yr) + eo_noi_u1_sy(sex,age,yr)
                  eo_noi_u2(sex,ageGrp(grp),yr) = eo_noi_u2(sex,ageGrp(grp),yr) + eo_noi_u2_sy(sex,age,yr)
                  eo_noi_u(sex,ageGrp(grp),yr) = eo_noi_u(sex,ageGrp(grp),yr) + eo_noi_u_sy(sex,age,yr)
                  
                  eo_nol(sex,ageGrp(grp),yr) = eo_nol(sex,ageGrp(grp),yr) + eo_nol_sy(sex,age,yr)
                  eo_nol_1(sex,ageGrp(grp),yr) = eo_nol_1(sex,ageGrp(grp),yr) + eo_nol_1_sy(sex,age,yr)
                  eo_nol_2(sex,ageGrp(grp),yr) = eo_nol_2(sex,ageGrp(grp),yr) + eo_nol_2_sy(sex,age,yr)
                  eo_nol_m1(sex,ageGrp(grp),yr) = eo_nol_m1(sex,ageGrp(grp),yr) + eo_nol_m1_sy(sex,age,yr)
                  eo_nol_m2(sex,ageGrp(grp),yr) = eo_nol_m2(sex,ageGrp(grp),yr) + eo_nol_m2_sy(sex,age,yr)
                  eo_nol_m(sex,ageGrp(grp),yr) = eo_nol_m(sex,ageGrp(grp),yr) + eo_nol_m_sy(sex,age,yr)
                  eo_nol_s1(sex,ageGrp(grp),yr) = eo_nol_s1(sex,ageGrp(grp),yr) + eo_nol_s1_sy(sex,age,yr)
                  eo_nol_s2(sex,ageGrp(grp),yr) = eo_nol_s2(sex,ageGrp(grp),yr) + eo_nol_s2_sy(sex,age,yr)
                  eo_nol_s(sex,ageGrp(grp),yr) = eo_nol_s(sex,ageGrp(grp),yr) + eo_nol_s_sy(sex,age,yr)
                  eo_nol_u1(sex,ageGrp(grp),yr) = eo_nol_u1(sex,ageGrp(grp),yr) + eo_nol_u1_sy(sex,age,yr)
                  eo_nol_u2(sex,ageGrp(grp),yr) = eo_nol_u2(sex,ageGrp(grp),yr) + eo_nol_u2_sy(sex,age,yr)
                  eo_nol_u(sex,ageGrp(grp),yr) = eo_nol_u(sex,ageGrp(grp),yr) + eo_nol_u_sy(sex,age,yr)
                  
                  eo_no_1(sex,ageGrp(grp),yr) = eo_no_1(sex,ageGrp(grp),yr) + eo_no_1_sy(sex,age,yr)
                  eo_no_2(sex,ageGrp(grp),yr) = eo_no_2(sex,ageGrp(grp),yr) + eo_no_2_sy(sex,age,yr)
                  eo_no(sex,ageGrp(grp),yr) = eo_no(sex,ageGrp(grp),yr) + eo_no_sy(sex,age,yr)
                  
                  eo_mef(sex,ageGrp(grp),yr) = eo_mef(sex,ageGrp(grp),yr) + eo_mef_sy(sex,age,yr)
                  eo_mefc(sex,ageGrp(grp),yr) = eo_mefc(sex,ageGrp(grp),yr) + eo_mefc_sy(sex,age,yr)
                  eo_esf(sex,ageGrp(grp),yr) = eo_esf(sex,ageGrp(grp),yr) + eo_esf_sy(sex,age,yr)
                  eo_und(sex,ageGrp(grp),yr) = eo_und(sex,ageGrp(grp),yr) + eo_und_sy(sex,age,yr)
                  
                  eo(sex,ageGrp(grp),yr) = eo(sex,ageGrp(grp),yr) + eo_sy(sex,age,yr)
                  
               end do
            end do
         end do
         
         eo_asf1(0,24,yr)  = eo_asf1(1,24,yr)  + eo_asf1(2,24,yr) 
         eo_asf2(0,24,yr)  = eo_asf2(1,24,yr)  + eo_asf2(2,24,yr) 
         eo_asj1(0,24,yr)  = eo_asj1(1,24,yr)  + eo_asj1(2,24,yr) 
         eo_asj2(0,24,yr)  = eo_asj2(1,24,yr)  + eo_asj2(2,24,yr) 
         eo_awj(0,24,yr)   = eo_awj(1,24,yr)   + eo_awj(2,24,yr)  
         eo_awjf(0,24,yr)  = eo_awjf(1,24,yr)  + eo_awjf(2,24,yr) 
         eo_awh(0,24,yr)   = eo_awh(1,24,yr)   + eo_awh(2,24,yr)  
         eo_awt(0,24,yr)   = eo_awt(1,24,yr)   + eo_awt(2,24,yr)  
         eo_awtf(0,24,yr)  = eo_awtf(1,24,yr)  + eo_awtf(2,24,yr) 
         eo_awtfa(0,24,yr) = eo_awtfa(1,24,yr) + eo_awtfa(2,24,yr)
         eo_awtfn(0,24,yr) = eo_awtfn(1,24,yr) + eo_awtfn(2,24,yr)
         eo_as1(0,24,yr)   = eo_as1(1,24,yr)   + eo_as1(2,24,yr)  
         eo_as2(0,24,yr)   = eo_as2(1,24,yr)   + eo_as2(2,24,yr)  
         eo_aw1(0,24,yr)   = eo_aw1(1,24,yr)   + eo_aw1(2,24,yr)  
         eo_aw2(0,24,yr)   = eo_aw2(1,24,yr)   + eo_aw2(2,24,yr)  
         
         eo_as(0,24,yr) = eo_as(1,24,yr) + eo_as(2,24,yr)
         eo_aw(0,24,yr) = eo_aw(1,24,yr) + eo_aw(2,24,yr)
         eo_a(0,24,yr) = eo_a(1,24,yr) + eo_a(2,24,yr)
         
         eo_nasf1(0,24,yr)  = eo_nasf1(1,24,yr)  + eo_nasf1(2,24,yr) 
         eo_nasf2(0,24,yr)  = eo_nasf2(1,24,yr)  + eo_nasf2(2,24,yr) 
         eo_nasj1(0,24,yr)  = eo_nasj1(1,24,yr)  + eo_nasj1(2,24,yr) 
         eo_nasj2(0,24,yr)  = eo_nasj2(1,24,yr)  + eo_nasj2(2,24,yr) 
         eo_nawj(0,24,yr)   = eo_nawj(1,24,yr)   + eo_nawj(2,24,yr)  
         eo_nawjf(0,24,yr)  = eo_nawjf(1,24,yr)  + eo_nawjf(2,24,yr) 
         eo_nawh(0,24,yr)   = eo_nawh(1,24,yr)   + eo_nawh(2,24,yr)  
         eo_nawt(0,24,yr)   = eo_nawt(1,24,yr)   + eo_nawt(2,24,yr)  
         eo_nawtf(0,24,yr)  = eo_nawtf(1,24,yr)  + eo_nawtf(2,24,yr) 
         eo_nas1(0,24,yr)   = eo_nas1(1,24,yr)   + eo_nas1(2,24,yr)  
         eo_nas2(0,24,yr)   = eo_nas2(1,24,yr)   + eo_nas2(2,24,yr)  
         eo_naw1(0,24,yr)   = eo_naw1(1,24,yr)   + eo_naw1(2,24,yr)  
         eo_naw2(0,24,yr)   = eo_naw2(1,24,yr)   + eo_naw2(2,24,yr)  
         
         eo_nas(0,24,yr) = eo_nas(1,24,yr) + eo_nas(2,24,yr)
         eo_naw(0,24,yr) = eo_naw(1,24,yr) + eo_naw(2,24,yr)
         eo_na(0,24,yr) = eo_na(1,24,yr) + eo_na(2,24,yr)
         
         eo_noi(0,24,yr) = eo_noi(1,24,yr) + eo_noi(2,24,yr)
         eo_noi_1(0,24,yr) = eo_noi_1(1,24,yr) + eo_noi_1(2,24,yr)
         eo_noi_2(0,24,yr) = eo_noi_2(1,24,yr) + eo_noi_2(2,24,yr)
         eo_noi_m1(0,24,yr) = eo_noi_m1(1,24,yr) + eo_noi_m1(2,24,yr)
         eo_noi_m2(0,24,yr) = eo_noi_m2(1,24,yr) + eo_noi_m2(2,24,yr)
         eo_noi_m(0,24,yr) = eo_noi_m(1,24,yr) + eo_noi_m(2,24,yr)
         eo_noi_s1(0,24,yr) = eo_noi_s1(1,24,yr) + eo_noi_s1(2,24,yr)
         eo_noi_s2(0,24,yr) = eo_noi_s2(1,24,yr) + eo_noi_s2(2,24,yr)
         eo_noi_s(0,24,yr) = eo_noi_s(1,24,yr) + eo_noi_s(2,24,yr)
         eo_noi_u1(0,24,yr) = eo_noi_u1(1,24,yr) + eo_noi_u1(2,24,yr)
         eo_noi_u2(0,24,yr) = eo_noi_u2(1,24,yr) + eo_noi_u2(2,24,yr)
         eo_noi_u(0,24,yr) = eo_noi_u(1,24,yr) + eo_noi_u(2,24,yr)
         
         eo_nol(0,24,yr) = eo_nol(1,24,yr) + eo_nol(2,24,yr)
         eo_nol_1(0,24,yr) = eo_nol_1(1,24,yr) + eo_nol_1(2,24,yr)
         eo_nol_2(0,24,yr) = eo_nol_2(1,24,yr) + eo_nol_2(2,24,yr)
         eo_nol_m1(0,24,yr) = eo_nol_m1(1,24,yr) + eo_nol_m1(2,24,yr)
         eo_nol_m2(0,24,yr) = eo_nol_m2(1,24,yr) + eo_nol_m2(2,24,yr)
         eo_nol_m(0,24,yr) = eo_nol_m(1,24,yr) + eo_nol_m(2,24,yr)
         eo_nol_s1(0,24,yr) = eo_nol_s1(1,24,yr) + eo_nol_s1(2,24,yr)
         eo_nol_s2(0,24,yr) = eo_nol_s2(1,24,yr) + eo_nol_s2(2,24,yr)
         eo_nol_s(0,24,yr) = eo_nol_s(1,24,yr) + eo_nol_s(2,24,yr)
         eo_nol_u1(0,24,yr) = eo_nol_u1(1,24,yr) + eo_nol_u1(2,24,yr)
         eo_nol_u2(0,24,yr) = eo_nol_u2(1,24,yr) + eo_nol_u2(2,24,yr)
         eo_nol_u(0,24,yr) = eo_nol_u(1,24,yr) + eo_nol_u(2,24,yr)
         
         eo_no_1(0,24,yr) = eo_no_1(1,24,yr) + eo_no_1(2,24,yr)
         eo_no_2(0,24,yr) = eo_no_2(1,24,yr) + eo_no_2(2,24,yr)
         eo_no(0,24,yr) = eo_no(1,24,yr) + eo_no(2,24,yr)
         
         eo_mef(0,24,yr) = eo_mef(1,24,yr) + eo_mef(2,24,yr)
         eo_mefc(0,24,yr) = eo_mefc(1,24,yr) + eo_mefc(2,24,yr)
         eo_esf(0,24,yr) = eo_esf(1,24,yr) + eo_esf(2,24,yr)
         eo_und(0,24,yr) = eo_und(1,24,yr) + eo_und(2,24,yr)
         
         eo(0,24,yr) = eo(1,24,yr) + eo(2,24,yr)
         
         eo_asf1(:,0,yr) = eo_asf1(:,24,yr)
         eo_asf2(:,0,yr) = eo_asf2(:,24,yr)
         eo_asj1(:,0,yr) = eo_asj1(:,24,yr)
         eo_asj2(:,0,yr) = eo_asj2(:,24,yr)
         eo_awj(:,0,yr) = eo_awj(:,24,yr)
         eo_awjf(:,0,yr) = eo_awjf(:,24,yr)
         eo_awh(:,0,yr) = eo_awh(:,24,yr)
         eo_awt(:,0,yr) = eo_awt(:,24,yr)
         eo_awtf(:,0,yr) = eo_awtf(:,24,yr)
         eo_awtfa(:,0,yr) = eo_awtfa(:,24,yr)
         eo_awtfn(:,0,yr) = eo_awtfn(:,24,yr)
         eo_as1(:,0,yr) = eo_as1(:,24,yr)
         eo_as2(:,0,yr) = eo_as2(:,24,yr)
         eo_aw1(:,0,yr) = eo_aw1(:,24,yr)
         eo_aw2(:,0,yr) = eo_aw2(:,24,yr)
         
         eo_as(:,0,yr) = eo_as(:,24,yr)
         eo_aw(:,0,yr) = eo_aw(:,24,yr)
         eo_a(:,0,yr) = eo_a(:,24,yr)
         
         eo_nasf1(:,0,yr) = eo_nasf1(:,24,yr)
         eo_nasf2(:,0,yr) = eo_nasf2(:,24,yr)
         eo_nasj1(:,0,yr) = eo_nasj1(:,24,yr)
         eo_nasj2(:,0,yr) = eo_nasj2(:,24,yr)
         eo_nawj(:,0,yr) = eo_nawj(:,24,yr)
         eo_nawjf(:,0,yr) = eo_nawjf(:,24,yr)
         eo_nawh(:,0,yr) = eo_nawh(:,24,yr)
         eo_nawt(:,0,yr) = eo_nawt(:,24,yr)
         eo_nawtf(:,0,yr) = eo_nawtf(:,24,yr)
         eo_nas1(:,0,yr) = eo_nas1(:,24,yr)
         eo_nas2(:,0,yr) = eo_nas2(:,24,yr)
         eo_naw1(:,0,yr) = eo_naw1(:,24,yr)
         eo_naw2(:,0,yr) = eo_naw2(:,24,yr)
         
         eo_nas(:,0,yr) = eo_nas(:,24,yr)
         eo_naw(:,0,yr) = eo_naw(:,24,yr)
         eo_na(:,0,yr) = eo_na(:,24,yr)
         
         eo_noi(:,0,yr) = eo_noi(:,24,yr)
         eo_noi_1(:,0,yr) = eo_noi_1(:,24,yr)
         eo_noi_2(:,0,yr) = eo_noi_2(:,24,yr)
         eo_noi_m1(:,0,yr) = eo_noi_m1(:,24,yr)
         eo_noi_m2(:,0,yr) = eo_noi_m2(:,24,yr)
         eo_noi_m(:,0,yr) = eo_noi_m(:,24,yr)
         eo_noi_s1(:,0,yr) = eo_noi_s1(:,24,yr)
         eo_noi_s2(:,0,yr) = eo_noi_s2(:,24,yr)
         eo_noi_s(:,0,yr) = eo_noi_s(:,24,yr)
         eo_noi_u1(:,0,yr) = eo_noi_u1(:,24,yr)
         eo_noi_u2(:,0,yr) = eo_noi_u2(:,24,yr)
         eo_noi_u(:,0,yr) = eo_noi_u(:,24,yr)
         
         eo_nol(:,0,yr) = eo_nol(:,24,yr)
         eo_nol_1(:,0,yr) = eo_nol_1(:,24,yr)
         eo_nol_2(:,0,yr) = eo_nol_2(:,24,yr)
         eo_nol_m1(:,0,yr) = eo_nol_m1(:,24,yr)
         eo_nol_m2(:,0,yr) = eo_nol_m2(:,24,yr)
         eo_nol_m(:,0,yr) = eo_nol_m(:,24,yr)
         eo_nol_s1(:,0,yr) = eo_nol_s1(:,24,yr)
         eo_nol_s2(:,0,yr) = eo_nol_s2(:,24,yr)
         eo_nol_s(:,0,yr) = eo_nol_s(:,24,yr)
         eo_nol_u1(:,0,yr) = eo_nol_u1(:,24,yr)
         eo_nol_u2(:,0,yr) = eo_nol_u2(:,24,yr)
         eo_nol_u(:,0,yr) = eo_nol_u(:,24,yr)
         
         eo_no_1(:,0,yr) = eo_no_1(:,24,yr)
         eo_no_2(:,0,yr) = eo_no_2(:,24,yr)
         eo_no(:,0,yr) = eo_no(:,24,yr)
         
         eo_mef(:,0,yr) = eo_mef(:,24,yr)
         eo_mefc(:,0,yr) = eo_mefc(:,24,yr)
         eo_esf(:,0,yr) = eo_esf(:,24,yr)
         eo_und(:,0,yr) = eo_und(:,24,yr)
         
         eo(:,0,yr) = eo(:,24,yr)
         
      end do
      
      !Temporary routine to check intermediate values
      !do yr = 80, endYr
      !  write(3003,'(3f12.6)') (eo_noi(sex,24,yr),sex=0,2)
      !end do
         
   end subroutine SumAgeGroups
   
!===============================================================================
   
   subroutine ComputeAverageWages()
   
      real (kind=8), dimension(MAX_YR) :: wetemp

      aws_a = ws_a / (e_a(0,0,:) + edmil_a + epsilon(0d0))
      
      aws_eo_asf1   = aws_a * 0.25d0
      aws_eo_asf2   = aws_a * 0.50d0
      aws_eo_asj1   = aws_a * 0.25d0
      aws_eo_asj2   = aws_a * 0.8d0
      aws_eo_awj    = aws_a * 0.9d0
      aws_eo_awjf   = aws_a * 0.8d0
      aws_eo_awh    = aws_a * 0.6d0
      aws_eo_awt    = aws_a * 1.0d0
      aws_eo_awtfa  = aws_a * 0.8d0
      aws_eo_awtfn  = aws_a * 0.5d0
      aws_eo_nasf1  = aws_a * 1.0d0
      aws_eo_nasf2  = aws_a * 0.8d0
      aws_eo_nasj1  = aws_a * 1.0d0
      aws_eo_nasj2  = aws_a * 0.8d0
      aws_eo_nawj   = aws_a * 1.0d0
      aws_eo_nawjf  = aws_a * 0.8d0
      aws_eo_nawh   = aws_a * 0.6d0
      aws_eo_nawt   = aws_a * 0.8d0
      aws_eo_nawtf  = aws_a * 0.8d0
      
      ws_eo_asf1   = aws_eo_asf1  *  eo_asf1(0,0,:)
      ws_eo_asf2   = aws_eo_asf2  *  eo_asf2(0,0,:)
      ws_eo_asj1   = aws_eo_asj1  *  eo_asj1(0,0,:)
      ws_eo_asj2   = aws_eo_asj2  *  eo_asj2(0,0,:)
      ws_eo_awj    = aws_eo_awj   *  eo_awj(0,0,:)
      ws_eo_awjf   = aws_eo_awjf  *  eo_awjf(0,0,:)
      ws_eo_awh    = aws_eo_awh   *  eo_awh(0,0,:)
      ws_eo_awt    = aws_eo_awt   *  eo_awt(0,0,:)
      ws_eo_awtfa  = aws_eo_awtfa *  eo_awtfa(0,0,:)
      ws_eo_awtfn  = aws_eo_awtfn *  eo_awtfn(0,0,:)
      ws_eo_nasf1  = aws_eo_nasf1 *  eo_nasf1(0,0,:)
      ws_eo_nasf2  = aws_eo_nasf2 *  eo_nasf2(0,0,:)
      ws_eo_nasj1  = aws_eo_nasj1 *  eo_nasj1(0,0,:)
      ws_eo_nasj2  = aws_eo_nasj2 *  eo_nasj2(0,0,:)
      ws_eo_nawj   = aws_eo_nawj  *  eo_nawj(0,0,:)
      ws_eo_nawjf  = aws_eo_nawjf *  eo_nawjf(0,0,:)
      ws_eo_nawh   = aws_eo_nawh  *  eo_nawh(0,0,:)
      ws_eo_nawt   = aws_eo_nawt  *  eo_nawt(0,0,:)
      ws_eo_nawtf  = aws_eo_nawtf *  eo_nawtf(0,0,:)
      
      ws_eo_awtf = ws_eo_awtfa  +  ws_eo_awtfn

      ws_eo_as1 = ws_eo_asf1 + ws_eo_asj1
      ws_eo_as2 = ws_eo_asf2 + ws_eo_asj2
      ws_eo_as  = ws_eo_as1  + ws_eo_as2
      ws_eo_aw1 = ws_eo_awj + ws_eo_awh +  ws_eo_awt
      ws_eo_aw2 = ws_eo_awjf +              ws_eo_awtfa
      ws_eo_aw  = ws_eo_aw1  + ws_eo_aw2
      ws_eo_a   = ws_eo_as   + ws_eo_aw

      ws_eo_nas1 = ws_eo_nasf1 + ws_eo_nasj1
      ws_eo_nas2 = ws_eo_nasf2 + ws_eo_nasj2
      ws_eo_nas  = ws_eo_nas1  + ws_eo_nas2
      ws_eo_naw1 = ws_eo_nawj + ws_eo_nawh +  ws_eo_nawt
      ws_eo_naw2 = ws_eo_nawjf
      ws_eo_naw  = ws_eo_naw1  + ws_eo_naw2
      ws_eo_na   = ws_eo_nas   + ws_eo_naw
      
      aws_eo_nol_m = aws_a * 0.85d0
      aws_eo_nol_s = aws_a * 0.85d0
      call FetchSeries(DFILE, "WE_SF_TEO.A", wetemp(:))
      aws_eo_nol_s(sample(1):sample(2)) = aws_a(sample(1):sample(2)) * 0.85d0 * we_sf_teo_a(sample(1):sample(2)) / (aws_a(sample(1):sample(2)) * 0.85d0 * eo_nol_s(0,0,sample(1):sample(2)) + aws_a(sample(1):sample(2)) * 0.80d0 * eo_noi_s(0,0,sample(1):sample(2)))  
      aws_eo_nol_u = aws_a * 0.80d0

      aws_eo_noi_m = aws_a * 0.80d0
      aws_eo_noi_s = aws_a * 0.80d0
      aws_eo_noi_s(sample(1):sample(2)) = aws_a(sample(1):sample(2)) * 0.80d0 * we_sf_teo_a(sample(1):sample(2)) / (aws_a(sample(1):sample(2)) * 0.85d0 * eo_nol_s(0,0,sample(1):sample(2)) + aws_a(sample(1):sample(2)) * 0.80d0 * eo_noi_s(0,0,sample(1):sample(2)))  
      aws_eo_noi_u = aws_a * 0.75d0
      
      ws_eo_nol_m = aws_eo_nol_m * eo_nol_m(0,0,:)
      ws_eo_nol_s = aws_eo_nol_s * eo_nol_s(0,0,:)
      ws_eo_nol_u = aws_eo_nol_u * eo_nol_u(0,0,:)
      ws_eo_nol   = ws_eo_nol_m + ws_eo_nol_s + ws_eo_nol_u

      ws_eo_noi_m = aws_eo_noi_m * eo_noi_m(0,0,:)
      ws_eo_noi_s = aws_eo_noi_s * eo_noi_s(0,0,:)
      ws_eo_noi_u = aws_eo_noi_u * eo_noi_u(0,0,:)
      ws_eo_noi   = ws_eo_noi_m + ws_eo_noi_s + ws_eo_noi_u

      ws_eo_no = ws_eo_nol + ws_eo_noi
      ws_eo  =  ws_eo_a + ws_eo_na + ws_eo_awtfn + ws_eo_nawtf + ws_eo_no
      
      ws_eo_mef   = &
         ws_eo_asf1  + &
         ws_eo_asj1  + &
         ws_eo_asf2  + &
         ws_eo_asj2  + &
         ws_eo_awj   + &
         ws_eo_awh   + &
         ws_eo_awt   + &
         ws_eo_awtfa + &
         ws_eo_awjf  + &
               
         ws_eo_nasf1 + &
         ws_eo_nasj1 + &
         ws_eo_nasf2 + &
         ws_eo_nasj2 + &
         ws_eo_nawj  + &
         ws_eo_nawh  + &
         ws_eo_nawt  + &
         ws_eo_nawjf + &
         ws_eo_nol_m + &
         ws_eo_noi_m
      
      ! ws_eo_mef(117:) = ws_eo_mef(117:) + ws_eo_nawtf(117:)
    
      ws_eo_mefc  = &
         ws_eo_asf2  + &
         ws_eo_asj2  + &
         ws_eo_awt   + &
         ws_eo_awtfa + &
         ws_eo_awjf  + &
               
         ws_eo_nasf1 + &
         ws_eo_nasj1 + &
         ws_eo_nasf2 + &
         ws_eo_nasj2 + &
         ws_eo_nawj  + &
         ws_eo_nawh  + &
         ws_eo_nawt  + &
         ws_eo_nawjf + &
         ws_eo_nol_m + &
         ws_eo_noi_m
      
      ! ws_eo_mefc(117:) = ws_eo_mefc(117:) + ws_eo_nawtf(117:)

      ws_eo_esf  = ws_eo_nol_s + ws_eo_noi_s
      ws_eo_und  = ws_eo_nol_u + ws_eo_noi_u + ws_eo_awtfn + ws_eo_nawtf
   
   end subroutine ComputeAverageWages
   
!===============================================================================
   
   subroutine Finish()

      per1a = 107
      startQtr = per1a * 4 + (qtr1a - 1)
      startYr = per1a 
    
      write(*,'(/a/)') "Otl2 procedure finished"

   end subroutine Finish

!===============================================================================

end module EconOtl2Mod