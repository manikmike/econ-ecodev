module EconOtl3Mod

   use EconParMod
   use EconLibMod
   use EconModSolAVarMod ! uses EconModSol2VarMod and EconOtl1VarMod
   use EconOtl3OutMod
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconOtl3Main

contains

!===============================================================================

   subroutine EconOtl3Main()

      call ProfileCheckpoint(7)
      write(*,'(//a//)') "Solving Otl3, Please Wait..."

      call CalculateHistoricalTEO()
      call EstimateHistoricalDetailedTotalEmployment()
      
      call CreateAdditionalTotalEmploymentConcepts()

      call CalculateProjectedTEO()
      call ProjectDetailedTotalEmployment()
      call ProjectAdditionalTotalEmploymentConcepts()
      call AggregateTotalEmploymentConcepts()
      call SumByPosting()
      call SumTEOAgeGroups()

      call EconOtl3OutMain()

      write(*,'(/a/)') "Otl3 procedure finished"
                
   end subroutine EconOtl3Main

!===============================================================================

   subroutine CalculateHistoricalTEO()

      call CalculateHistoricalEmployment()
      call CalculateHistoricalRatios()
      call CalculateHistoricalTEOValues()

   end subroutine CalculateHistoricalTEO

!===============================================================================

   ! Step 1 - Calculate values for E by gender and age group (1617,1819,2024,...7579, 80o)
   !          using all avalilable historical data for L, RU, and E

   subroutine CalculateHistoricalEmployment()

      integer :: yr, sex, ageGrp
      real (kind = 8), dimension(MAX_YR) :: ltemp, rtemp, ntemp

      call FetchSeries(MEF, "CE_M.A", ce_m(0,0,:))
      histend = sample(2)

      do sex = 1, 2
         ltemp = 0
         call FetchSeries(BKDO1, "L"//trim(sexLabel(sex))//"70O.A", ltemp)
         lc_a(sex,23,sample(1):sample(2)) = ltemp(sample(1):sample(2))
         rtemp = 0
         call FetchSeries(BKDO1, "R"//trim(sexLabel(sex))//"65O.A", rtemp)
         ru_a(sex,22,sample(1):sample(2)) = rtemp(sample(1):sample(2))
         rtemp = 0
         call FetchSeries(BKDO1, "R"//trim(sexLabel(sex))//"70O.A", rtemp)
         ru_a(sex,15,sample(1):sample(2)) = rtemp(sample(1):sample(2))
         ntemp = 0
         call FetchSeries(BKDO1, "N"//trim(sexLabel(sex))//"70O.A", ntemp)
         n_a(sex,23,sample(1):sample(2)) = ntemp(sample(1):sample(2))
         ntemp = 0
         call FetchSeries(BKDO1, "N"//trim(sexLabel(sex))//"7074.A", ntemp)
         n_a(sex,13,sample(1):sample(2)) = ntemp(sample(1):sample(2))
         ntemp = 0
         call FetchSeries(CPSO_NILF, "N"//trim(sexLabel(sex))//"75O.A", ntemp)
         n_a(sex,17,94:sample(2)) = ntemp(94:sample(2))
         ntemp = 0
         call FetchSeries(BKDO1, "N"//trim(sexLabel(sex))//"75O.A", ntemp)
         n_a(sex,17,sample(1):93) = ntemp(sample(1):93)
         ntemp = 0
         call FetchSeries(CPSO_NILF, "N"//trim(sexLabel(sex))//"7579.A", ntemp)
         n_a(sex,14,94:sample(2)) = ntemp(94:sample(2))
         ntemp = 0
         call FetchSeries(BKDO1, "N"//trim(sexLabel(sex))//"7579.A", ntemp)
         n_a(sex,14,sample(1):93) = ntemp(sample(1):93)
      end do

      e_a(1:2,1:3,64:histend) = lc_a(1:2,1:3,64:histend) * &
         (1d0 - ru_a(1:2,1:3,64:histend)/100d0)

      do yr = 64, 76
         do sex = 1, 2
            e_a(sex,4,yr) = lc_a(sex,4,yr) * (1d0 - ru_a(sex,18,yr) / 100d0)
            e_a(sex,5,yr) = lc_a(sex,5,yr) * (1d0 - ru_a(sex,18,yr) / 100d0)
            e_a(sex,6,yr) = lc_a(sex,6,yr) * (1d0 - ru_a(sex,19,yr) / 100d0)
            e_a(sex,7,yr) = lc_a(sex,7,yr) * (1d0 - ru_a(sex,19,yr) / 100d0)
            e_a(sex,8,yr) = lc_a(sex,8,yr) * (1d0 - ru_a(sex,20,yr) / 100d0)
            e_a(sex,9,yr) = lc_a(sex,9,yr) * (1d0 - ru_a(sex,20,yr) / 100d0)
            e_a(sex,10,yr) = lc_a(sex,10,yr) * (1d0 - ru_a(sex,21,yr) / 100d0)
            e_a(sex,11,yr) = lc_a(sex,11,yr) * (1d0 - ru_a(sex,21,yr) / 100d0)
            e_a(sex,12,yr) = lc_a(sex,12,yr) * (1d0 - ru_a(sex,22,yr) / 100d0)
         end do
      end do

      e_a(1:2,4:12,77:histend) = lc_a(1:2,4:12,77:histend) * &
         (1d0 - ru_a(1:2,4:12,77:histend)/100d0)

      e_a(1:2,23,64:80) = lc_a(1:2,23,64:80) * (1d0 - ru_a(1:2,22,64:80)/100d0)
      e_a(1:2,13,64:80) = e_a(1:2,23,64:80) * n_a(1:2,13,64:80) / n_a(1:2,23,64:80)
      e_a(1:2,17,64:80) = e_a(1:2,23,64:80) - e_a(1:2,13,64:80)
      e_a(1:2,14,64:80) = e_a(1:2,17,64:80) * n_a(1:2,14,64:80) / n_a(1:2,17,64:80) 
      e_a(1:2,31,64:80) = e_a(1:2,17,64:80) - e_a(1:2,14,64:80)

      e_a(1:2,23,81:histend) = lc_a(1:2,23,81:histend) * (1d0 - ru_a(1:2,15,81:histend)/100d0)
      e_a(1:2,13,81:histend) = lc_a(1:2,13,81:histend) * (1d0 - ru_a(1:2,15,81:histend)/100d0)
      e_a(1:2,17,81:histend) = e_a(1:2,23,81:histend) - e_a(1:2,13,81:histend)
      e_a(1:2,14,81:histend) = e_a(1:2,17,81:histend) * n_a(1:2,14,81:histend) / n_a(1:2,17,81:histend) 
      e_a(1:2,14,81:93) = e_a(1:2,17,81:93) * n_a(1:2,14,81:93) / n_a(1:2,17,81:93) 
      e_a(1:2,31,81:histend) = e_a(1:2,17,81:histend) - e_a(1:2,14,81:histend)

      ! do yr = 64, histend
      !    write(9999,'(34f12.6)') ((e_a(sex,ageGrp,yr),ageGrp=1,13),&
      !       e_a(sex,23,yr),e_a(sex,14,yr),e_a(sex,17,yr),e_a(sex,31,yr),sex=2,1,-1)
      ! end do

   end subroutine CalculateHistoricalEmployment

!===============================================================================

   ! Step 2 - Calculate values for the ratio of TE to (E+Mil) 
   ! (i.e., RTE) by gender and age (1617,1819,2024,...7579, 80o)
   !          If TE not available, then HE (or CE)

   subroutine CalculateHistoricalRatios()

      integer :: yr, sex, ageGrp
      real (kind = 8), dimension(MAX_YR) :: ntemp
 
      do yr = 64, histend
         do sex = 1,2
             ! ce_m(sex,31,yr) =  ce_m(sex,17,yr) -  ce_m(sex,14,yr)
             ! te_m(sex,31,yr) =  te_m(sex,17,yr) -  te_m(sex,14,yr)
             ! he_m(sex,31,yr) =  he_m(sex,17,yr) -  he_m(sex,14,yr)
             ! tel_so(sex,31,yr) =  tel_so(sex,17,yr) -  tel_so(sex,14,yr)
             
             eo_esf(sex,31,yr) =  eo_esf(sex,17,yr) -  eo_esf(sex,14,yr)
             eo_und(sex,31,yr) =  eo_und(sex,17,yr) -  eo_und(sex,14,yr)
             
             eo_asf1(sex,31,yr) =  eo_asf1(sex,17,yr) -  eo_asf1(sex,14,yr)
             eo_asj1(sex,31,yr) =  eo_asj1(sex,17,yr) -  eo_asj1(sex,14,yr)
             eo_awj(sex,31,yr) =  eo_awj(sex,17,yr) -  eo_awj(sex,14,yr)
             eo_awh(sex,31,yr) =  eo_awh(sex,17,yr) -  eo_awh(sex,14,yr)
         end do
      end do

      ! do yr = 64, histend
      !    write(9999,'(12f12.6)') (ce_m(sex,31,yr), te_m(sex,31,yr), &
      !       he_m(sex,31,yr), te_sfo_lrp(sex,31,yr), eo_esf(sex,31,yr), &
      !       eo_und(sex,31,yr),sex=1,2)
      ! end do

      call FetchSeries(BKDO1, "NF1617M.A", ntemp) ! dfile and bkdo1 don't match 1972-81
      m_a(2,1,72:81) = ntemp(72:81)
      do ageGrp = 1, 10
         do sex = 1, 2
            do yr = 64, 86
               rte(sex,ageGrp,yr) = (ce_m(sex,ageGrp,yr) +tel_so(sex,ageGrp,yr) + &
                  eo_esf(sex,ageGrp,yr) + eo_und(sex,ageGrp,yr)) / (e_a(sex,ageGrp,yr) + m_a(sex,ageGrp,yr))
            end do
            do yr = 87, 87
               rte(sex,ageGrp,yr) = (he_m(sex,ageGrp,yr) + tel_so(sex,ageGrp,yr) + &
                  eo_esf(sex,ageGrp,yr) + eo_und(sex,ageGrp,yr)) / (e_a(sex,ageGrp,yr) + m_a(sex,ageGrp,yr))
            end do
            do yr = 88, histend
               rte(sex,ageGrp,yr) = (he_m(sex,ageGrp,yr) + te_rro_m(sex,ageGrp,yr) + te_sloo_m(sex,ageGrp,yr) + &
                  te_slos_m(sex,ageGrp,yr) + te_sloe_m(sex,ageGrp,yr) + te_ps_m(sex,ageGrp,yr) + &
                  te_ph_m(sex,ageGrp,yr) + eo_asf1(sex,ageGrp,yr) + eo_asj1(sex,ageGrp,yr) + &
                  eo_awj(sex,ageGrp,yr) + eo_awh(sex,ageGrp,yr) + tel_so(sex,ageGrp,yr) + &
                  eo_esf(sex,ageGrp,yr) + eo_und(sex,ageGrp,yr)) / (e_a(sex,ageGrp,yr) + m_a(sex,ageGrp,yr))
            end do
         end do
      end do

      do ageGrp = 11, 14
         do sex = 1, 2
            do yr = 64, 86
               rte(sex,ageGrp,yr) = (ce_m(sex,ageGrp,yr) + tel_so(sex,ageGrp,yr) + &
                  eo_esf(sex,ageGrp,yr) + eo_und(sex,ageGrp,yr)) / e_a(sex,ageGrp,yr)
            end do
            do yr = 87, 87
               rte(sex,ageGrp,yr) = (he_m(sex,ageGrp,yr) + tel_so(sex,ageGrp,yr) + &
                  eo_esf(sex,ageGrp,yr) + eo_und(sex,ageGrp,yr)) / e_a(sex,ageGrp,yr)
            end do
            do yr = 88, histend
               rte(sex,ageGrp,yr) = (he_m(sex,ageGrp,yr) + te_rro_m(sex,ageGrp,yr) + te_sloo_m(sex,ageGrp,yr) + &
                  te_slos_m(sex,ageGrp,yr) + te_sloe_m(sex,ageGrp,yr) + te_ps_m(sex,ageGrp,yr) + &
                  te_ph_m(sex,ageGrp,yr) + eo_asf1(sex,ageGrp,yr) + eo_asj1(sex,ageGrp,yr) + &
                  eo_awj(sex,ageGrp,yr) + eo_awh(sex,ageGrp,yr) + tel_so(sex,ageGrp,yr) + &
                  eo_esf(sex,ageGrp,yr) + eo_und(sex,ageGrp,yr)) / e_a(sex,ageGrp,yr)
            end do
         end do
      end do

      do ageGrp = 31, 31
         do sex = 1, 2
            do yr = 64, 86
               rte(sex,ageGrp,yr) = (ce_m(sex,ageGrp,yr) + tel_so(sex,ageGrp,yr) + &
                  eo_esf(sex,ageGrp,yr) + eo_und(sex,ageGrp,yr)) / e_a(sex,ageGrp,yr)
            end do
            do yr = 87, 87
               rte(sex,ageGrp,yr) = (he_m(sex,ageGrp,yr) + tel_so(sex,ageGrp,yr) + &
                  eo_esf(sex,ageGrp,yr) + eo_und(sex,ageGrp,yr)) / e_a(sex,ageGrp,yr)
            end do
            do yr = 88, histend
               rte(sex,ageGrp,yr) = (he_m(sex,ageGrp,yr) + te_rro_m(sex,ageGrp,yr) + te_sloo_m(sex,ageGrp,yr) + &
                  te_slos_m(sex,ageGrp,yr) + te_sloe_m(sex,ageGrp,yr) + te_ps_m(sex,ageGrp,yr) + &
                  te_ph_m(sex,ageGrp,yr) + eo_asf1(sex,ageGrp,yr) + eo_asj1(sex,ageGrp,yr) + &
                  eo_awj(sex,ageGrp,yr) + eo_awh(sex,ageGrp,yr) + tel_so(sex,ageGrp,yr) + &
                  eo_esf(sex,ageGrp,yr) + eo_und(sex,ageGrp,yr)) / e_a(sex,ageGrp,yr)
            end do
         end do
      end do

      ! do yr = 64, histend
      !    write(9999,'(30f12.6)') ((rte(sex,ageGrp,yr),ageGrp=1,14),rte(sex,31,yr),sex=2,1,-1)
      ! end do

   end subroutine CalculateHistoricalRatios

!===============================================================================

   ! Step 3 - Calculate values for TEO as the product of (TE / (E+Mil)) and 
   !          EO (EO is estimated by OTL1 and OTL2).

   subroutine CalculateHistoricalTEOValues()

      integer :: age, sex, ageGrp, yr

      integer, dimension(15) :: lowAge = &
         (/ 16, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75,  80 /)

      integer, dimension(15) :: hiAge  = &
         (/ 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 100 /)

      integer, dimension(15) :: grpNum = &
         (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 31 /)

      do ageGrp = 1, 15
         do age = lowAge(ageGrp), hiAge(ageGrp)
            do sex = 1, 2
               rte_sy(sex,age,64:histend) = rte(sex,grpNum(ageGrp),64:histend)
            end do
         end do
      end do

      ! do yr = 64, histend
      !   write(9998,'(170f12.6)') (rte_sy(sex,100,yr),(rte_sy(sex,age,yr),age=16,99),sex=2,1,-1)
      !   write(9999,'(170f12.6)') (teo_sy(sex,100,yr),(teo_sy(sex,age,yr),age=16,99),sex=2,1,-1)
      ! end do

   end subroutine CalculateHistoricalTEOValues

!===============================================================================
   
   subroutine EstimateHistoricalDetailedTotalEmployment()

      integer :: age, sex
   
      do sex = 1, 2
         do age = 16, 100
            
            teo_asf1_sy(sex,age,:) = 1.5625d0 * eo_asf1_sy(sex,age,:)
            teo_asf2_sy(sex,age,:) = 2.0844d0 * eo_asf2_sy(sex,age,:)
            teo_asj1_sy(sex,age,:) = 1.8750d0 * eo_asj1_sy(sex,age,:)
            teo_asj2_sy(sex,age,:) = 1.8750d0 * eo_asj2_sy(sex,age,:)
            teo_awj_sy(sex,age,:)  = 2.5800d0 * eo_awj_sy(sex,age,:)
            teo_awjf_sy(sex,age,:) = 1.6667d0 * eo_awjf_sy(sex,age,:)
            teo_awh_sy(sex,age,:)  = 1.3000d0 * eo_awh_sy(sex,age,:)
            teo_awt_sy(sex,age,:)  = 1.5000d0 * eo_awt_sy(sex,age,:)

            teo_awtfa_sy(sex,age,:) = 2.0833d0   * eo_awtfa_sy(sex,age,:)
            teo_awtfn_sy(sex,age,:) = 2.0833d0   * eo_awtfn_sy(sex,age,:)
            teo_awtf_sy(sex,age,:)  = teo_awtfa_sy(sex,age,:) + teo_awtfn_sy(sex,age,:)

            teo_nasf1_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nasf1_sy(sex,age,:)
            teo_nasf2_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nasf2_sy(sex,age,:)
            teo_nasj1_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nasj1_sy(sex,age,:)
            teo_nasj2_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nasj2_sy(sex,age,:)
            teo_nawj_sy(sex,age,:)  = rte_sy(sex,age,:) * eo_nawj_sy(sex,age,:)
            teo_nawjf_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nawjf_sy(sex,age,:)
            teo_nawh_sy(sex,age,:)  = rte_sy(sex,age,:) * eo_nawh_sy(sex,age,:)
            teo_nawt_sy(sex,age,:)  = rte_sy(sex,age,:) * eo_nawt_sy(sex,age,:)
            teo_nawtf_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nawtf_sy(sex,age,:)
            
         end do
      end do
   
   end subroutine EstimateHistoricalDetailedTotalEmployment
   
!===============================================================================
   
   subroutine CreateAdditionalTotalEmploymentConcepts()
   
      integer :: age, sex
   
      do sex = 1, 2
         do age = 16, 100
            
            teo_as1_sy(sex,age,:) = teo_asf1_sy(sex,age,:) + teo_asj1_sy(sex,age,:)
            teo_as2_sy(sex,age,:) = teo_asf2_sy(sex,age,:) + teo_asj2_sy(sex,age,:)
            teo_as_sy(sex,age,:)  = teo_as1_sy(sex,age,:)  +  teo_as2_sy(sex,age,:)
            teo_aw1_sy(sex,age,:) = teo_awj_sy(sex,age,:)  + teo_awh_sy(sex,age,:) + teo_awt_sy(sex,age,:)
            teo_aw2_sy(sex,age,:) = teo_awjf_sy(sex,age,:) + teo_awtfa_sy(sex,age,:)
            teo_aw_sy(sex,age,:)  = teo_aw1_sy(sex,age,:)  + teo_aw2_sy(sex,age,:)
            teo_a_sy(sex,age,:)   =  teo_as_sy(sex,age,:)  +  teo_aw_sy(sex,age,:)
    
            teo_nas1_sy(sex,age,:) = teo_nasf1_sy(sex,age,:) + teo_nasj1_sy(sex,age,:)
            teo_nas2_sy(sex,age,:) = teo_nasf2_sy(sex,age,:) + teo_nasj2_sy(sex,age,:)
            teo_nas_sy(sex,age,:)  = teo_nas1_sy(sex,age,:)  + teo_nas2_sy(sex,age,:)
            teo_naw1_sy(sex,age,:) = teo_nawj_sy(sex,age,:)  + teo_nawh_sy(sex,age,:) + teo_nawt_sy(sex,age,:)
            teo_naw2_sy(sex,age,:) = teo_nawjf_sy(sex,age,:)
            teo_naw_sy(sex,age,:)  = teo_naw1_sy(sex,age,:)  + teo_naw2_sy(sex,age,:)
            teo_na_sy(sex,age,:)   =  teo_nas_sy(sex,age,:)  + teo_naw_sy(sex,age,:)

            teo_nol_m_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nol_m_sy(sex,age,:)
            teo_nol_s_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nol_s_sy(sex,age,:)
            teo_nol_u_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nol_u_sy(sex,age,:)
            teo_nol_sy(sex,age,:)   = teo_nol_m_sy(sex,age,:) + teo_nol_s_sy(sex,age,:) + teo_nol_u_sy(sex,age,:)
            teo_noi_m_sy(sex,age,:) = rte_sy(sex,age,:) * eo_noi_m_sy(sex,age,:)
            teo_noi_s_sy(sex,age,:) = rte_sy(sex,age,:) * eo_noi_s_sy(sex,age,:)
            teo_noi_u_sy(sex,age,:) = rte_sy(sex,age,:) * eo_noi_u_sy(sex,age,:)
            teo_noi_sy(sex,age,:)   = teo_noi_m_sy(sex,age,:) + teo_noi_s_sy(sex,age,:) + teo_noi_u_sy(sex,age,:)
            teo_no_sy(sex,age,:)    = teo_nol_sy(sex,age,:)   + teo_noi_sy(sex,age,:)
            teo_sy(sex,age,:)       = teo_no_sy(sex,age,:)    + teo_a_sy(sex,age,:) + teo_na_sy(sex,age,:) + &
                                      teo_awtfa_sy(sex,age,:) + teo_nawtf_sy(sex,age,:)
    
          end do
      end do
      
   end subroutine CreateAdditionalTotalEmploymentConcepts
   
!===============================================================================
   
   subroutine CalculateProjectedTEO()

      integer :: age, sex, ageGrp, yr

      integer, dimension(13) :: lowAge = &
         (/ 16, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65,  70 /)

      integer, dimension(13) :: hiAge  = &
         (/ 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 100 /)

      integer, dimension(13) :: grpNum = &
         (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 23 /)

      do ageGrp = 1, 10
         do sex = 1, 2
            do yr = histend+1, endYr
               rte(sex,ageGrp,yr) = te_a(sex,ageGrp,yr) / &
                  (e_a(sex,ageGrp,yr) + m_a(sex,ageGrp,yr))
            end do
         end do
      end do

      do ageGrp = 11, 13
         do sex = 1, 2
            do yr = histend+1, endYr
               rte(sex,grpNum(ageGrp),yr) = te_a(sex,grpNum(ageGrp),yr) / &
                  e_a(sex,grpNum(ageGrp),yr)
            end do
         end do
      end do

      ! do yr = histend+1, endYr
      !    write(9999,'(26f12.6)') ((rte(sex,ageGrp,yr),ageGrp=1,12),rte(sex,23,yr),sex=2,1,-1)
      ! end do

      do ageGrp = 1, 13
         do age = lowAge(ageGrp), hiAge(ageGrp)
            do sex = 1, 2
               rte_sy(sex,age,histend+1:endYr) = rte(sex,grpNum(ageGrp),histend+1:endYr)
            end do
         end do
      end do
      
      rte(:,13,histend+1:endYr) = rte(:,23,histend+1:endYr) ! 70-74
      rte(:,14,histend+1:endYr) = rte(:,23,histend+1:endYr) ! 75-79
      rte(:,31,histend+1:endYr) = rte(:,23,histend+1:endYr) ! 80O

      ! do yr = histend+1, endYr
      !    write(9998,'(170f12.6)') (rte_sy(sex,100,yr),(rte_sy(sex,age,yr),age=16,99),sex=2,1,-1)
      !    write(9999,'(170f12.6)') (teo_sy(sex,100,yr),(teo_sy(sex,age,yr),age=16,99),sex=2,1,-1)
      ! end do

   end subroutine CalculateProjectedTEO

!===============================================================================
   
      subroutine ProjectDetailedTotalEmployment()

      integer :: age, sex
   
      do sex = 1, 2
         do age = 16, 100
            
            teo_asf1_sy(sex,age,:) = 1.5625d0 * eo_asf1_sy(sex,age,:)
            teo_asf2_sy(sex,age,:) = 2.0844d0 * eo_asf2_sy(sex,age,:)
            teo_asj1_sy(sex,age,:) = 1.8750d0 * eo_asj1_sy(sex,age,:)
            teo_asj2_sy(sex,age,:) = 1.8750d0 * eo_asj2_sy(sex,age,:)
            teo_awj_sy(sex,age,:)  = 2.5800d0 * eo_awj_sy(sex,age,:)
            teo_awjf_sy(sex,age,:) = 1.6667d0 * eo_awjf_sy(sex,age,:)
            teo_awh_sy(sex,age,:)  = 1.3000d0 * eo_awh_sy(sex,age,:)
            
            ! teo_awt_sy
            ! Sven provided factors below in 2015-02-20 email ("NonImmig stock to AWT conversion.xlsx")
            
            teo_awt_sy(sex,age,114)  = 1.5000d0 * eo_awt_sy(sex,age,114)
            teo_awt_sy(sex,age,115)  = 1.4959d0 * eo_awt_sy(sex,age,115)
            teo_awt_sy(sex,age,116)  = 1.4889d0 * eo_awt_sy(sex,age,116)
            teo_awt_sy(sex,age,117)  = 1.4831d0 * eo_awt_sy(sex,age,117)
            teo_awt_sy(sex,age,118)  = 1.4790d0 * eo_awt_sy(sex,age,118)
            teo_awt_sy(sex,age,119)  = 1.4768d0 * eo_awt_sy(sex,age,119)
            teo_awt_sy(sex,age,120)  = 1.4752d0 * eo_awt_sy(sex,age,120)
            teo_awt_sy(sex,age,121)  = 1.4743d0 * eo_awt_sy(sex,age,121)
            teo_awt_sy(sex,age,122)  = 1.4740d0 * eo_awt_sy(sex,age,122)
            teo_awt_sy(sex,age,123)  = 1.4740d0 * eo_awt_sy(sex,age,123)
            teo_awt_sy(sex,age,124)  = 1.4746d0 * eo_awt_sy(sex,age,124)
            teo_awt_sy(sex,age,125)  = 1.4756d0 * eo_awt_sy(sex,age,125)
            teo_awt_sy(sex,age,126)  = 1.4767d0 * eo_awt_sy(sex,age,126)
            teo_awt_sy(sex,age,127)  = 1.4778d0 * eo_awt_sy(sex,age,127)
            teo_awt_sy(sex,age,128)  = 1.4787d0 * eo_awt_sy(sex,age,128)
            teo_awt_sy(sex,age,129)  = 1.4796d0 * eo_awt_sy(sex,age,129)
            teo_awt_sy(sex,age,130)  = 1.4804d0 * eo_awt_sy(sex,age,130)
            teo_awt_sy(sex,age,131)  = 1.4812d0 * eo_awt_sy(sex,age,131)
            teo_awt_sy(sex,age,132)  = 1.4819d0 * eo_awt_sy(sex,age,132)
            teo_awt_sy(sex,age,133)  = 1.4826d0 * eo_awt_sy(sex,age,133)
            teo_awt_sy(sex,age,134)  = 1.4832d0 * eo_awt_sy(sex,age,134)
            teo_awt_sy(sex,age,135)  = 1.4837d0 * eo_awt_sy(sex,age,135)
            teo_awt_sy(sex,age,136)  = 1.4843d0 * eo_awt_sy(sex,age,136)
            teo_awt_sy(sex,age,137)  = 1.4848d0 * eo_awt_sy(sex,age,137)
            teo_awt_sy(sex,age,138)  = 1.4853d0 * eo_awt_sy(sex,age,138)
            teo_awt_sy(sex,age,139)  = 1.4858d0 * eo_awt_sy(sex,age,139)
            teo_awt_sy(sex,age,140)  = 1.4863d0 * eo_awt_sy(sex,age,140)
            teo_awt_sy(sex,age,141)  = 1.4868d0 * eo_awt_sy(sex,age,141)
            teo_awt_sy(sex,age,142)  = 1.4872d0 * eo_awt_sy(sex,age,142)
            teo_awt_sy(sex,age,143)  = 1.4876d0 * eo_awt_sy(sex,age,143)
            teo_awt_sy(sex,age,144)  = 1.4880d0 * eo_awt_sy(sex,age,144)
            teo_awt_sy(sex,age,145)  = 1.4883d0 * eo_awt_sy(sex,age,145)
            teo_awt_sy(sex,age,146)  = 1.4887d0 * eo_awt_sy(sex,age,146)
            teo_awt_sy(sex,age,147)  = 1.4890d0 * eo_awt_sy(sex,age,147)
            teo_awt_sy(sex,age,148)  = 1.4893d0 * eo_awt_sy(sex,age,148)
            teo_awt_sy(sex,age,149)  = 1.4897d0 * eo_awt_sy(sex,age,149)
            teo_awt_sy(sex,age,150:)  = 1.4899d0 * eo_awt_sy(sex,age,150:)

            teo_awtfa_sy(sex,age,:) = 2.0833d0   * eo_awtfa_sy(sex,age,:)
            teo_awtfn_sy(sex,age,:) = 2.0833d0   * eo_awtfn_sy(sex,age,:)
            teo_awtf_sy(sex,age,:)  = teo_awtfa_sy(sex,age,:) + teo_awtfn_sy(sex,age,:)

            teo_nasf1_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nasf1_sy(sex,age,:)
            teo_nasf2_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nasf2_sy(sex,age,:)
            teo_nasj1_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nasj1_sy(sex,age,:)
            teo_nasj2_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nasj2_sy(sex,age,:)
            teo_nawj_sy(sex,age,:)  = rte_sy(sex,age,:) * eo_nawj_sy(sex,age,:)
            teo_nawjf_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nawjf_sy(sex,age,:)
            teo_nawh_sy(sex,age,:)  = rte_sy(sex,age,:) * eo_nawh_sy(sex,age,:)
            teo_nawt_sy(sex,age,:)  = rte_sy(sex,age,:) * eo_nawt_sy(sex,age,:)
            teo_nawtf_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nawtf_sy(sex,age,:)
            
         end do
      end do
   
      end subroutine ProjectDetailedTotalEmployment
   
!===============================================================================
      
      subroutine ProjectAdditionalTotalEmploymentConcepts()
   
      integer :: age, sex
   
      do sex = 1, 2
         do age = 16, 100
            
            teo_as1_sy(sex,age,:) = teo_asf1_sy(sex,age,:) + teo_asj1_sy(sex,age,:)
            teo_as2_sy(sex,age,:) = teo_asf2_sy(sex,age,:) + teo_asj2_sy(sex,age,:)
            teo_as_sy(sex,age,:)  = teo_as1_sy(sex,age,:)  +  teo_as2_sy(sex,age,:)
            teo_aw1_sy(sex,age,:) = teo_awj_sy(sex,age,:)  + teo_awh_sy(sex,age,:) + teo_awt_sy(sex,age,:)
            teo_aw2_sy(sex,age,:) = teo_awjf_sy(sex,age,:) + teo_awtfa_sy(sex,age,:)
            teo_aw_sy(sex,age,:)  = teo_aw1_sy(sex,age,:)  + teo_aw2_sy(sex,age,:)
            teo_a_sy(sex,age,:)   =  teo_as_sy(sex,age,:)  +  teo_aw_sy(sex,age,:)
    
            teo_nas1_sy(sex,age,:) = teo_nasf1_sy(sex,age,:) + teo_nasj1_sy(sex,age,:)
            teo_nas2_sy(sex,age,:) = teo_nasf2_sy(sex,age,:) + teo_nasj2_sy(sex,age,:)
            teo_nas_sy(sex,age,:)  = teo_nas1_sy(sex,age,:)  + teo_nas2_sy(sex,age,:)
            teo_naw1_sy(sex,age,:) = teo_nawj_sy(sex,age,:)  + teo_nawh_sy(sex,age,:) + teo_nawt_sy(sex,age,:)
            teo_naw2_sy(sex,age,:) = teo_nawjf_sy(sex,age,:)
            teo_naw_sy(sex,age,:)  = teo_naw1_sy(sex,age,:)  + teo_naw2_sy(sex,age,:)
            teo_na_sy(sex,age,:)   =  teo_nas_sy(sex,age,:)  + teo_naw_sy(sex,age,:)

            teo_nol_m_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nol_m_sy(sex,age,:)
            teo_nol_s_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nol_s_sy(sex,age,:)
            teo_nol_u_sy(sex,age,:) = rte_sy(sex,age,:) * eo_nol_u_sy(sex,age,:)
            teo_nol_sy(sex,age,:)   = teo_nol_m_sy(sex,age,:) + teo_nol_s_sy(sex,age,:) + teo_nol_u_sy(sex,age,:)
            teo_noi_m_sy(sex,age,:) = rte_sy(sex,age,:) * eo_noi_m_sy(sex,age,:)
            teo_noi_s_sy(sex,age,:) = rte_sy(sex,age,:) * eo_noi_s_sy(sex,age,:)
            teo_noi_u_sy(sex,age,:) = rte_sy(sex,age,:) * eo_noi_u_sy(sex,age,:)
            teo_noi_sy(sex,age,:)   = teo_noi_m_sy(sex,age,:) + teo_noi_s_sy(sex,age,:) + teo_noi_u_sy(sex,age,:)
            teo_no_sy(sex,age,:)    = teo_nol_sy(sex,age,:)   + teo_noi_sy(sex,age,:)
            teo_sy(sex,age,:)       = teo_no_sy(sex,age,:)    + teo_a_sy(sex,age,:) + teo_na_sy(sex,age,:) + &
                                      teo_awtfn_sy(sex,age,:) + teo_nawtf_sy(sex,age,:)
    
          end do
      end do
      
      end subroutine ProjectAdditionalTotalEmploymentConcepts
      
!===============================================================================
      
   subroutine AggregateTotalEmploymentConcepts()
   
      integer :: yr, sex
      
      do yr = 64, endYr
         do sex = 1, 2
            
            teo_asf1(sex,24,yr)  = sum(teo_asf1_sy(sex,16:100,yr))
            teo_asf2(sex,24,yr)  = sum(teo_asf2_sy(sex,16:100,yr))
            teo_asj1(sex,24,yr)  = sum(teo_asj1_sy(sex,16:100,yr))
            teo_asj2(sex,24,yr)  = sum(teo_asj2_sy(sex,16:100,yr))
            teo_awj(sex,24,yr)   = sum(teo_awj_sy(sex,16:100,yr))
            teo_awjf(sex,24,yr)  = sum(teo_awjf_sy(sex,16:100,yr))
            teo_awh(sex,24,yr)   = sum(teo_awh_sy(sex,16:100,yr))
            teo_awt(sex,24,yr)   = sum(teo_awt_sy(sex,16:100,yr))
            teo_awtf(sex,24,yr)  = sum(teo_awtf_sy(sex,16:100,yr))
            teo_awtfa(sex,24,yr) = sum(teo_awtfa_sy(sex,16:100,yr))
            teo_awtfn(sex,24,yr) = sum(teo_awtfn_sy(sex,16:100,yr))

            teo_nasf1(sex,24,yr) = sum(teo_nasf1_sy(sex,16:100,yr))
            teo_nasf2(sex,24,yr) = sum(teo_nasf2_sy(sex,16:100,yr))
            teo_nasj1(sex,24,yr) = sum(teo_nasj1_sy(sex,16:100,yr))
            teo_nasj2(sex,24,yr) = sum(teo_nasj2_sy(sex,16:100,yr))
            teo_nawj(sex,24,yr)  = sum(teo_nawj_sy(sex,16:100,yr)) 
            teo_nawjf(sex,24,yr) = sum(teo_nawjf_sy(sex,16:100,yr))
            teo_nawh(sex,24,yr)  = sum(teo_nawh_sy(sex,16:100,yr)) 
            teo_nawt(sex,24,yr)  = sum(teo_nawt_sy(sex,16:100,yr)) 
            teo_nawtf(sex,24,yr) = sum(teo_nawtf_sy(sex,16:100,yr))

            teo_as1(sex,24,yr)   = sum(teo_as1_sy(sex,16:100,yr))  
            teo_as2(sex,24,yr)   = sum(teo_as2_sy(sex,16:100,yr))  
            teo_as(sex,24,yr)    = sum(teo_as_sy(sex,16:100,yr))   
            teo_aw1(sex,24,yr)   = sum(teo_aw1_sy(sex,16:100,yr))  
            teo_aw2(sex,24,yr)   = sum(teo_aw2_sy(sex,16:100,yr))  
            teo_aw(sex,24,yr)    = sum(teo_aw_sy(sex,16:100,yr))   
            teo_a(sex,24,yr)     = sum(teo_a_sy(sex,16:100,yr))    

            teo_nas1(sex,24,yr)  = sum(teo_nas1_sy(sex,16:100,yr)) 
            teo_nas2(sex,24,yr)  = sum(teo_nas2_sy(sex,16:100,yr)) 
            teo_nas(sex,24,yr)   = sum(teo_nas_sy(sex,16:100,yr))  
            teo_naw1(sex,24,yr)  = sum(teo_naw1_sy(sex,16:100,yr)) 
            teo_naw2(sex,24,yr)  = sum(teo_naw2_sy(sex,16:100,yr)) 
            teo_naw(sex,24,yr)   = sum(teo_naw_sy(sex,16:100,yr))  
            teo_na(sex,24,yr)    = sum(teo_na_sy(sex,16:100,yr))   
      
            teo_nol_m(sex,24,yr) = sum(teo_nol_m_sy(sex,16:100,yr))
            teo_nol_s(sex,24,yr) = sum(teo_nol_s_sy(sex,16:100,yr))
            teo_nol_u(sex,24,yr) = sum(teo_nol_u_sy(sex,16:100,yr))
            teo_nol(sex,24,yr)   = sum(teo_nol_sy(sex,16:100,yr))  
            teo_noi_m(sex,24,yr) = sum(teo_noi_m_sy(sex,16:100,yr))
            teo_noi_s(sex,24,yr) = sum(teo_noi_s_sy(sex,16:100,yr))
            teo_noi_u(sex,24,yr) = sum(teo_noi_u_sy(sex,16:100,yr))
            teo_noi(sex,24,yr)   = sum(teo_noi_sy(sex,16:100,yr))  
            teo_no(sex,24,yr)    = sum(teo_no_sy(sex,16:100,yr))   
            teo(sex,24,yr)       = sum(teo_sy(sex,16:100,yr))      

         end do
         
         teo_asf1(0,24,yr)  = sum(teo_asf1_sy(1:2,16:100,yr))
         teo_asf2(0,24,yr)  = sum(teo_asf2_sy(1:2,16:100,yr))
         teo_asj1(0,24,yr)  = sum(teo_asj1_sy(1:2,16:100,yr))
         teo_asj2(0,24,yr)  = sum(teo_asj2_sy(1:2,16:100,yr))
         teo_awj(0,24,yr)   = sum(teo_awj_sy(1:2,16:100,yr))
         teo_awjf(0,24,yr)  = sum(teo_awjf_sy(1:2,16:100,yr))
         teo_awh(0,24,yr)   = sum(teo_awh_sy(1:2,16:100,yr))
         teo_awt(0,24,yr)   = sum(teo_awt_sy(1:2,16:100,yr))
         teo_awtf(0,24,yr)  = sum(teo_awtf_sy(1:2,16:100,yr))
         teo_awtfa(0,24,yr) = sum(teo_awtfa_sy(1:2,16:100,yr))
         teo_awtfn(0,24,yr) = sum(teo_awtfn_sy(1:2,16:100,yr))

         teo_nasf1(0,24,yr) = sum(teo_nasf1_sy(1:2,16:100,yr))
         teo_nasf2(0,24,yr) = sum(teo_nasf2_sy(1:2,16:100,yr))
         teo_nasj1(0,24,yr) = sum(teo_nasj1_sy(1:2,16:100,yr))
         teo_nasj2(0,24,yr) = sum(teo_nasj2_sy(1:2,16:100,yr))
         teo_nawj(0,24,yr)  = sum(teo_nawj_sy(1:2,16:100,yr)) 
         teo_nawjf(0,24,yr) = sum(teo_nawjf_sy(1:2,16:100,yr))
         teo_nawh(0,24,yr)  = sum(teo_nawh_sy(1:2,16:100,yr)) 
         teo_nawt(0,24,yr)  = sum(teo_nawt_sy(1:2,16:100,yr)) 
         teo_nawtf(0,24,yr) = sum(teo_nawtf_sy(1:2,16:100,yr))

         teo_as1(0,24,yr)   = sum(teo_as1_sy(1:2,16:100,yr))  
         teo_as2(0,24,yr)   = sum(teo_as2_sy(1:2,16:100,yr))  
         teo_as(0,24,yr)    = sum(teo_as_sy(1:2,16:100,yr))   
         teo_aw1(0,24,yr)   = sum(teo_aw1_sy(1:2,16:100,yr))  
         teo_aw2(0,24,yr)   = sum(teo_aw2_sy(1:2,16:100,yr))  
         teo_aw(0,24,yr)    = sum(teo_aw_sy(1:2,16:100,yr))   
         teo_a(0,24,yr)     = sum(teo_a_sy(1:2,16:100,yr))    

         teo_nas1(0,24,yr)  = sum(teo_nas1_sy(1:2,16:100,yr)) 
         teo_nas2(0,24,yr)  = sum(teo_nas2_sy(1:2,16:100,yr)) 
         teo_nas(0,24,yr)   = sum(teo_nas_sy(1:2,16:100,yr))  
         teo_naw1(0,24,yr)  = sum(teo_naw1_sy(1:2,16:100,yr)) 
         teo_naw2(0,24,yr)  = sum(teo_naw2_sy(1:2,16:100,yr)) 
         teo_naw(0,24,yr)   = sum(teo_naw_sy(1:2,16:100,yr))  
         teo_na(0,24,yr)    = sum(teo_na_sy(1:2,16:100,yr))   
      
         teo_nol_m(0,24,yr) = sum(teo_nol_m_sy(1:2,16:100,yr))
         teo_nol_s(0,24,yr) = sum(teo_nol_s_sy(1:2,16:100,yr))
         teo_nol_u(0,24,yr) = sum(teo_nol_u_sy(1:2,16:100,yr))
         teo_nol(0,24,yr)   = sum(teo_nol_sy(1:2,16:100,yr))  
         teo_noi_m(0,24,yr) = sum(teo_noi_m_sy(1:2,16:100,yr))
         teo_noi_s(0,24,yr) = sum(teo_noi_s_sy(1:2,16:100,yr))
         teo_noi_u(0,24,yr) = sum(teo_noi_u_sy(1:2,16:100,yr))
         teo_noi(0,24,yr)   = sum(teo_noi_sy(1:2,16:100,yr))  
         teo_no(0,24,yr)    = sum(teo_no_sy(1:2,16:100,yr))   
         teo(0,24,yr)       = sum(teo_sy(1:2,16:100,yr))      
         
      end do
      
      teo_asf1(0,0,:)  =  teo_asf1(0,24,:)  
      teo_asf2(0 ,0,:)  =  teo_asf2(0,24,:)  
      teo_asj1(0,0,:)  =  teo_asj1(0,24,:)  
      teo_asj2(0,0,:)  =  teo_asj2(0,24,:)  
      teo_awj(0,0,:)   =  teo_awj(0,24,:)   
      teo_awjf(0,0,:)  =  teo_awjf(0,24,:)  
      teo_awh(0,0,:)   =  teo_awh(0,24,:)   
      teo_awt(0,0,:)   =  teo_awt(0,24,:)   
      teo_awtf(0,0,:)  =  teo_awtf(0,24,:)  
      teo_awtfa(0,0,:) =  teo_awtfa(0,24,:) 
      teo_awtfn(0,0,:) =  teo_awtfn(0,24,:) 
                            
      teo_nasf1(0,0,:) =  teo_nasf1(0,24,:) 
      teo_nasf2(0,0,:) =  teo_nasf2(0,24,:) 
      teo_nasj1(0,0,:) =  teo_nasj1(0,24,:) 
      teo_nasj2(0,0,:) =  teo_nasj2(0,24,:) 
      teo_nawj(0,0,:)  =  teo_nawj(0,24,:)  
      teo_nawjf(0,0,:) =  teo_nawjf(0,24,:) 
      teo_nawh(0,0,:)  =  teo_nawh(0,24,:)  
      teo_nawt(0,0,:)  =  teo_nawt(0,24,:)  
      teo_nawtf(0,0,:) =  teo_nawtf(0,24,:) 
                            
      teo_as1(0,0,:)   =  teo_as1(0,24,:)   
      teo_as2(0,0,:)   =  teo_as2(0,24,:)   
      teo_as(0,0,:)    =  teo_as(0,24,:)    
      teo_aw1(0,0,:)   =  teo_aw1(0,24,:)   
      teo_aw2(0,0,:)   =  teo_aw2(0,24,:)   
      teo_aw(0,0,:)    =  teo_aw(0,24,:)    
      teo_a(0,0,:)     =  teo_a(0,24,:)     
                            
      teo_nas1(0,0,:)  =  teo_nas1(0,24,:)  
      teo_nas2(0,0,:)  =  teo_nas2(0,24,:)  
      teo_nas(0,0,:)   =  teo_nas(0,24,:)   
      teo_naw1(0,0,:)  =  teo_naw1(0,24,:)  
      teo_naw2(0,0,:)  =  teo_naw2(0,24,:)  
      teo_naw(0,0,:)   =  teo_naw(0,24,:)   
      teo_na(0,0,:)    =  teo_na(0,24,:)    
                            
      teo_nol_m(0,0,:) =  teo_nol_m(0,24,:) 
      teo_nol_s(0,0,:) =  teo_nol_s(0,24,:) 
      teo_nol_u(0,0,:) =  teo_nol_u(0,24,:) 
      teo_nol(0,0,:)   =  teo_nol(0,24,:)   
      teo_noi_m(0,0,:) =  teo_noi_m(0,24,:) 
      teo_noi_s(0,0,:) =  teo_noi_s(0,24,:) 
      teo_noi_u(0,0,:) =  teo_noi_u(0,24,:) 
      teo_noi(0,0,:)   =  teo_noi(0,24,:)   
      teo_no(0,0,:)    =  teo_no(0,24,:)    
      teo(0,0,:)       =  teo(0,24,:)       
   
   end subroutine AggregateTotalEmploymentConcepts
   
!===============================================================================
   
   subroutine SumByPosting()
   
      integer :: age, sex, yr
      
      do yr = 50, endYr
         do sex = 1, 2
            do age = 16, 100
               teo_mef_sy(sex,age,yr)  = teo_asf1_sy(sex,age,yr)  &
                                       + teo_asj1_sy(sex,age,yr)  &
                                       + teo_asf2_sy(sex,age,yr)  &
                                       + teo_asj2_sy(sex,age,yr)  &
                                       + teo_awj_sy(sex,age,yr)   &
                                       + teo_awh_sy(sex,age,yr)   &
                                       + teo_awt_sy(sex,age,yr)   &
                                       + teo_awtfa_sy(sex,age,yr) &
                                       + teo_awjf_sy(sex,age,yr)  &

                                       + teo_nasf1_sy(sex,age,yr) &
                                       + teo_nasj1_sy(sex,age,yr) &
                                       + teo_nasf2_sy(sex,age,yr) &
                                       + teo_nasj2_sy(sex,age,yr) &
                                       + teo_nawj_sy(sex,age,yr)  &
                                       + teo_nawh_sy(sex,age,yr)  &
                                       + teo_nawt_sy(sex,age,yr)  &
                                       + teo_nawjf_sy(sex,age,yr) &
                                       + teo_nol_m_sy(sex,age,yr) &
                                       + teo_noi_m_sy(sex,age,yr)
               
               ! if (yr >= 117) teo_mef_sy(sex,age,yr) = teo_mef_sy(sex,age,yr) + teo_nawtf_sy(sex,age,yr)
               
               teo_mefc_sy(sex,age,yr) = teo_asf2_sy(sex,age,yr)  &
                                       + teo_asj2_sy(sex,age,yr)  &
                                       + teo_awt_sy(sex,age,yr)   &
                                       + teo_awtfa_sy(sex,age,yr) &
                                       + teo_awjf_sy(sex,age,yr)  &

                                       + teo_nasf1_sy(sex,age,yr) &
                                       + teo_nasj1_sy(sex,age,yr) &
                                       + teo_nasf2_sy(sex,age,yr) &
                                       + teo_nasj2_sy(sex,age,yr) &
                                       + teo_nawj_sy(sex,age,yr)  &
                                       + teo_nawh_sy(sex,age,yr)  &
                                       + teo_nawt_sy(sex,age,yr)  &
                                       + teo_nawjf_sy(sex,age,yr) &
                                       + teo_nol_m_sy(sex,age,yr) &
                                       + teo_noi_m_sy(sex,age,yr)
               
               ! if (yr >= 117) teo_mefc_sy(sex,age,yr) = teo_mefc_sy(sex,age,yr) + teo_nawtf_sy(sex,age,yr)
               
               teo_esf_sy(sex,age,yr)  = teo_nol_s_sy(sex,age,yr) + teo_noi_s_sy(sex,age,yr)
               teo_und_sy(sex,age,yr)  = teo_nol_u_sy(sex,age,yr) + teo_noi_u_sy(sex,age,yr) + &
                                         teo_awtfn_sy(sex,age,yr) + teo_nawtf_sy(sex,age,yr)
               teo_adj_sy(sex,age,yr)  = teo_mef_sy(sex,age,yr)   - teo_mefc_sy(sex,age,yr) - &
                                         (teo_asf1_sy(sex,age,yr) + teo_asj1_sy(sex,age,yr) + teo_awj_sy(sex,age,yr))
            end do
         end do
      end do
      
      do yr = 50, endYr
         
         do sex = 1, 2
            
            teo_mef(sex,24,yr)  = sum(teo_mef_sy(sex,16:100,yr))
            teo_mefc(sex,24,yr) = sum(teo_mefc_sy(sex,16:100,yr))
            teo_esf(sex,24,yr)  = sum(teo_esf_sy(sex,16:100,yr))
            teo_und(sex,24,yr)  = sum(teo_und_sy(sex,16:100 ,yr))
            teo_adj(sex,24,yr)  = sum(teo_adj_sy(sex,16:100 ,yr))
            
         end do
         
         teo_mef(0,24,yr)  = sum(teo_mef_sy(1:2,16:100,yr))
         teo_mefc(0,24,yr) = sum(teo_mefc_sy(1:2,16:100,yr))
         teo_esf(0,24,yr)  = sum(teo_esf_sy(1:2,16:100,yr))
         teo_und(0,24,yr)  = sum(teo_und_sy(1:2,16:100 ,yr))
         teo_adj(0,24,yr)  = sum(teo_adj_sy(1:2,16:100 ,yr))
         
      end do
      
      teo_mef(0,0,:)  = teo_mef(0,24,:)
      teo_mefc(0,0,:) = teo_mefc(0,24,:)
      teo_esf(0,0,:)  = teo_esf(0,24,:)
      teo_und(0,0,:)  = teo_und(0,24,:)
      teo_adj(0,0,:)  = teo_adj(0,24,:)
      
      ws_eo_adj  = ws_eo_mef - ws_eo_mefc - &
                  (ws_eo_asf1 + ws_eo_asj1 + ws_eo_awj)


   end subroutine SumByPosting

!===============================================================================

   subroutine SumTEOAgeGroups()
   
      integer :: yr, age, sex, i
      
      integer, dimension(27) :: grpNum = (/  1,  2,  3,   4,   5,   6,  7,  8,  9, 10, 11,  12, 13, 14, 15, &
                                            32, 33, 34,  17,  23,  22, 27, 18, 19, 20, 21,  24 /)
      
      integer, dimension(27) :: lowAge = (/ 16, 18, 20,  25,  30,  35, 40, 45, 50, 55, 60,  65, 70, 75, 80, &
                                            85, 90, 95,  75,  70,  65, 16, 25, 35, 45, 55,  16 /)
      
      integer, dimension(27) :: hiAge  = (/ 17, 19, 24,  29,  34,  39, 44, 49, 54, 59, 64,  69, 74, 79, 84, &
                                            89, 94, 99, 100, 100, 100, 19, 34, 44, 54, 64, 100 /)

      do yr = 50, endYr
         do sex = 1, 2
            do i = 1, 27
            
               teo_asf1(sex,grpNum(i),yr)  = sum(teo_asf1_sy(sex,lowAge(i):hiAge(i),yr))
               teo_asf2(sex,grpNum(i),yr)  = sum(teo_asf2_sy(sex,lowAge(i):hiAge(i),yr))
               teo_asj1(sex,grpNum(i),yr)  = sum(teo_asj1_sy(sex,lowAge(i):hiAge(i),yr))
               teo_asj2(sex,grpNum(i),yr)  = sum(teo_asj2_sy(sex,lowAge(i):hiAge(i),yr))
               teo_awj(sex,grpNum(i),yr)   = sum(teo_awj_sy(sex,lowAge(i):hiAge(i),yr))
               teo_awjf(sex,grpNum(i),yr)  = sum(teo_awjf_sy(sex,lowAge(i):hiAge(i),yr))
               teo_awh(sex,grpNum(i),yr)   = sum(teo_awh_sy(sex,lowAge(i):hiAge(i),yr))
               teo_awt(sex,grpNum(i),yr)   = sum(teo_awt_sy(sex,lowAge(i):hiAge(i),yr))
               teo_awtf(sex,grpNum(i),yr)  = sum(teo_awtf_sy(sex,lowAge(i):hiAge(i),yr))
               teo_awtfa(sex,grpNum(i),yr) = sum(teo_awtfa_sy(sex,lowAge(i):hiAge(i),yr))
               teo_awtfn(sex,grpNum(i),yr) = sum(teo_awtfn_sy(sex,lowAge(i):hiAge(i),yr))

               teo_nasf1(sex,grpNum(i),yr) = sum(teo_nasf1_sy(sex,lowAge(i):hiAge(i),yr))
               teo_nasf2(sex,grpNum(i),yr) = sum(teo_nasf2_sy(sex,lowAge(i):hiAge(i),yr))
               teo_nasj1(sex,grpNum(i),yr) = sum(teo_nasj1_sy(sex,lowAge(i):hiAge(i),yr))
               teo_nasj2(sex,grpNum(i),yr) = sum(teo_nasj2_sy(sex,lowAge(i):hiAge(i),yr))
               teo_nawj(sex,grpNum(i),yr)  = sum(teo_nawj_sy(sex,lowAge(i):hiAge(i),yr)) 
               teo_nawjf(sex,grpNum(i),yr) = sum(teo_nawjf_sy(sex,lowAge(i):hiAge(i),yr))
               teo_nawh(sex,grpNum(i),yr)  = sum(teo_nawh_sy(sex,lowAge(i):hiAge(i),yr)) 
               teo_nawt(sex,grpNum(i),yr)  = sum(teo_nawt_sy(sex,lowAge(i):hiAge(i),yr)) 
               teo_nawtf(sex,grpNum(i),yr) = sum(teo_nawtf_sy(sex,lowAge(i):hiAge(i),yr))

               teo_as1(sex,grpNum(i),yr)   = sum(teo_as1_sy(sex,lowAge(i):hiAge(i),yr))  
               teo_as2(sex,grpNum(i),yr)   = sum(teo_as2_sy(sex,lowAge(i):hiAge(i),yr))  
               teo_as(sex,grpNum(i),yr)    = sum(teo_as_sy(sex,lowAge(i):hiAge(i),yr))   
               teo_aw1(sex,grpNum(i),yr)   = sum(teo_aw1_sy(sex,lowAge(i):hiAge(i),yr))  
               teo_aw2(sex,grpNum(i),yr)   = sum(teo_aw2_sy(sex,lowAge(i):hiAge(i),yr))  
               teo_aw(sex,grpNum(i),yr)    = sum(teo_aw_sy(sex,lowAge(i):hiAge(i),yr))   
               teo_a(sex,grpNum(i),yr)     = sum(teo_a_sy(sex,lowAge(i):hiAge(i),yr))    

               teo_nas1(sex,grpNum(i),yr)  = sum(teo_nas1_sy(sex,lowAge(i):hiAge(i),yr)) 
               teo_nas2(sex,grpNum(i),yr)  = sum(teo_nas2_sy(sex,lowAge(i):hiAge(i),yr)) 
               teo_nas(sex,grpNum(i),yr)   = sum(teo_nas_sy(sex,lowAge(i):hiAge(i),yr))  
               teo_naw1(sex,grpNum(i),yr)  = sum(teo_naw1_sy(sex,lowAge(i):hiAge(i),yr)) 
               teo_naw2(sex,grpNum(i),yr)  = sum(teo_naw2_sy(sex,lowAge(i):hiAge(i),yr)) 
               teo_naw(sex,grpNum(i),yr)   = sum(teo_naw_sy(sex,lowAge(i):hiAge(i),yr))  
               teo_na(sex,grpNum(i),yr)    = sum(teo_na_sy(sex,lowAge(i):hiAge(i),yr))   
      
               teo_nol_m(sex,grpNum(i),yr) = sum(teo_nol_m_sy(sex,lowAge(i):hiAge(i),yr))
               teo_nol_s(sex,grpNum(i),yr) = sum(teo_nol_s_sy(sex,lowAge(i):hiAge(i),yr))
               teo_nol_u(sex,grpNum(i),yr) = sum(teo_nol_u_sy(sex,lowAge(i):hiAge(i),yr))
               teo_nol(sex,grpNum(i),yr)   = sum(teo_nol_sy(sex,lowAge(i):hiAge(i),yr))  
               teo_noi_m(sex,grpNum(i),yr) = sum(teo_noi_m_sy(sex,lowAge(i):hiAge(i),yr))
               teo_noi_s(sex,grpNum(i),yr) = sum(teo_noi_s_sy(sex,lowAge(i):hiAge(i),yr))
               teo_noi_u(sex,grpNum(i),yr) = sum(teo_noi_u_sy(sex,lowAge(i):hiAge(i),yr))
               teo_noi(sex,grpNum(i),yr)   = sum(teo_noi_sy(sex,lowAge(i):hiAge(i),yr))  
               teo_no(sex,grpNum(i),yr)    = sum(teo_no_sy(sex,lowAge(i):hiAge(i),yr))   
               teo(sex,grpNum(i),yr)       = sum(teo_sy(sex,lowAge(i):hiAge(i),yr))
               
               teo_mef(sex,grpNum(i),yr)   = sum(teo_mef_sy(sex,lowAge(i):hiAge(i),yr))
               teo_mefc(sex,grpNum(i),yr)  = sum(teo_mefc_sy(sex,lowAge(i):hiAge(i),yr))
               teo_esf(sex,grpNum(i),yr)   = sum(teo_esf_sy(sex,lowAge(i):hiAge(i),yr))
               teo_und(sex,grpNum(i),yr)   = sum(teo_und_sy(sex,lowAge(i):hiAge(i),yr))
               teo_adj(sex,grpNum(i),yr)   = sum(teo_adj_sy(sex,lowAge(i):hiAge(i),yr))
            
            end do
         end do
      end do

   end subroutine SumTEOAgeGroups
   
!===============================================================================

end module EconOtl3Mod