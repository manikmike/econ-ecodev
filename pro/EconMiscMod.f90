module EconMiscMod

   use EconParMod
   use EconLibMod
   use EconModSolAVarMod
   use SplineMod
   include "OcactFortLib.inc"
   implicit none

   private
   public :: histlcanndata, coveredworkers

   real (kind = 8), dimension(2,60:100,MAX_YR) :: nisy_a

contains

!===============================================================================

   subroutine coveredworkers()

      integer :: yr, sex, age, group, lowAge, ageGrp
      real (kind = 8), dimension(2,20,MAX_YR) :: rce
      real (kind = 8), dimension(2,0:99,MAX_YR) :: rce_sy
      real (kind = 8), dimension(2,MAX_YR) :: ce0t4, ce5t9, ce1014, ce1519
      real (kind = 8), dimension(2,0:100,MAX_YR) :: cexsy_a, exsy_a
      real (kind = 8), dimension(MAX_YR) :: ef75ox, em75ox
      real (kind = 8), dimension(MAX_YR) :: ef70ox, em70ox
      real (kind = 8), dimension(MAX_YR) :: ef7074x, em7074x
      
      do sex = 1, 2
         do age = 60, 100
            call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", nisy_a(sex,age,:))
         end do
      end do
            
      ! STEP 1 - Calculate historical values for CE by sex and single year of age

      do yr = 81, histend
         do sex = 1, 2
            do age = 0, 15
               cesy_a(sex,age,yr) = ce_m_sy(sex,age,yr) + tel_so_sy(sex,age,yr)
            end do
            do age = 16, 100
                 cesy_a(sex,age,yr) = ce_m_sy(sex,age,yr) + tel_so_sy(sex,age,yr) + &
                    teo_nol_s_sy(sex,age,yr) + teo_noi_s_sy(sex,age,yr)
            end do
         end do
      end do
      
      ! STEP 2 - Calculate projected values for CE by sex and single year of age from 0 to 19
      
      cesy_a(1:2,0:9,histend+1:endYr) = ce_m_sy(1:2,0:9,histend+1:endYr)
      do sex = 1, 2
         do yr = 81, endYr
            ce0t4(sex,yr) = sum(cesy_a(sex,0:4,yr))
            ce5t9(sex,yr) = sum(cesy_a(sex,5:9,yr))
         end do
      end do
      
      cesy_a(1:2,10:15,histend+1:endYr) = ce_m_sy(1:2,10:15,histend+1:endYr)
      
      do sex = 1, 2
         do yr = histend, endYr
            do age = 16, 19
               cexsy_a(sex,age,yr) = (cesy_a(sex,age,histend) / ndecsy_a(sex,age,histend)) * ndecsy_a(sex,age,yr)
            end do
         end do
      end do
      
      do sex = 1, 2
         do yr = histend+1, endYr
            do age = 16, 17
               cesy_a(sex,age,yr) = cexsy_a(sex,age,yr) / sum(cexsy_a(sex,16:17,yr)) * &
                  (ce_m(sex,1,yr) + te_s(sex,1,yr))
            end do
            do age = 18, 19
               cesy_a(sex,age,yr) = cexsy_a(sex,age,yr) / sum(cexsy_a(sex,18:19,yr)) * &
                  (ce_m(sex,2,yr) + te_s(sex,2,yr))
            end do
         end do
         do yr = 81, endYr
            ce1014(sex,yr) = sum(cesy_a(sex,10:14,yr))
            ce1519(sex,yr) = sum(cesy_a(sex,15:19,yr))
         end do
      end do
      
      ! STEP 3 - Calculate projected values for CE by sex and single year of age from 70 and over
      
      do sex = 1, 2
         do age = 75, 100
            do yr = histend+1, endYr
               exsy_a(sex,age,yr) = psy_a(sex,age,yr) * nisy_a(sex,age,yr) * (1d0 - ru_a(sex,14,yr) / 100d0)
            end do
         end do
      end do
      
      do yr = histend+1, endYr
         em75ox(yr) = sum(exsy_a(1,75:100,yr))
         ef75ox(yr) = sum(exsy_a(2,75:100,yr))
      end do
      
      do yr = histend+1, endYr
         do sex = 1, 2
            do age = 70, 74
               exsy_a(sex,age,yr) = psy_a(sex,age,yr) * nisy_a(sex,age,yr) * (1d0 - ru_a(sex,13,yr) / 100d0)
            end do
         end do
         em7074x(yr) = sum(exsy_a(1,70:74,yr))
         ef7074x(yr) = sum(exsy_a(2,70:74,yr))
         em70ox(yr) = em7074x(yr) + em75ox(yr)
         ef70ox(yr) = ef7074x(yr) + ef75ox(yr)
      end do
      
      do age = 70, 100
         do yr = histend+1, endYr
            cesy_a(1,age,yr) = ce_a(1,23,yr) * exsy_a(1,age,yr) / em70ox(yr)
            cesy_a(2,age,yr) = ce_a(2,23,yr) * exsy_a(2,age,yr) / ef70ox(yr)
         end do
      end do
      
      do sex = 1, 2
         do yr = 81, endYr
            ce_a(sex,13,yr) = sum(cesy_a(sex,70:74,yr))
            ce_a(sex,14,yr) = sum(cesy_a(sex,75:79,yr))
            ce_a(sex,15,yr) = sum(cesy_a(sex,80:84,yr))
            ce_a(sex,32,yr) = sum(cesy_a(sex,85:89,yr))
            ce_a(sex,33,yr) = sum(cesy_a(sex,90:94,yr))
            ce_a(sex,34,yr) = sum(cesy_a(sex,95:99,yr))
         end do
      end do
      
      ! STEP 4a - Calculate projected values for RCE by sex and 5-year age groups

      do yr = histend+1, endYr
         do sex = 1, 2
            rce(sex,1,yr) = (ce0t4(sex,yr)) / sum(ndecsy_a(sex,0:4,yr))
            rce(sex,2,yr) = (ce5t9(sex,yr)) / sum(ndecsy_a(sex,5:9,yr))
            rce(sex,3,yr) = (ce1014(sex,yr)) / sum(ndecsy_a(sex,10:14,yr))
            rce(sex,4,yr) = (ce1519(sex,yr)) / sum(ndecsy_a(sex,15:19,yr))
            do group = 5, 20
               lowAge = group * 5 - 5 ! notice the offset of 2 from what is normally used
               rce(sex,group,yr) = ce_a(sex,group-2,yr) / sum(ndecsy_a(sex,lowAge:lowAge+4,yr))
            end do
         end do
      end do
      
      !  STEP 4b - Calculate projected values for RCE by sex and single year of age from 20 to 69

      rce_sy(:,0:19,:) = cesy_a(:,0:19,:) / ndecsy_a(:,0:19,:)
      rce_sy(:,70:99,:) = cesy_a(:,70:99,:) / ndecsy_a(:,70:99,:)
      do yr = histend+1, endYr
         do sex = 1, 2
            ! call BeersCovEmp2(rce(sex,1:20,yr), rce_sy(sex,0:99,yr))
            call q00(ndecsy_a(sex,:,yr),ce_a(sex,3:12,yr),rce(sex,5:14,yr), rce_sy(sex,0:99,yr))
         end do
      end do
      
      !  STEP 5 - Calculate projected  values for CE by sex and single year of age from 20 to 69

      do yr = histend+1, endYr
         do sex = 1, 2
            do ageGrp = 3, 12
               lowAge = ageGrp * 5 + 5
               do age = lowAge, lowAge+4
                  cesy_a(sex,age,yr) = rce_sy(sex,age,yr) * ndecsy_a(sex,age,yr)
               end do
               cesy_a(sex,lowAge:lowAge+4,yr) = cesy_a(sex,lowAge:lowAge+4,yr) * ce_a(sex,ageGrp,yr) / sum(cesy_a(sex,lowAge:lowAge+4,yr))
            end do
         end do
      end do
      
      ! STEP 6 -  Calculate projected 'preliminary(p)' values for CE by sex and 5-year age groups for those age 20 to 69
      
      ! This step is not necessary (other than for checking) since Beers interpolation method guarantees
      ! sum of single-year age groups equals five-year age groups
      
      
      ! STEP 7 -  Calculate projected values for TEL_SO by sex and single year of age for those 10 to 100
      
      do yr = histend+1, endYr
         do sex = 1, 2
            do age = 0, 15
               tel_so_sy(sex,age,yr) = 0d0
            end do
            tel_so_sy(sex,16,yr) = tel_so(sex,1,yr) * cesy_a(sex,16,yr) / ce_a(sex,1,yr)
            tel_so_sy(sex,17,yr) = tel_so(sex,1,yr) * cesy_a(sex,17,yr) / ce_a(sex,1,yr)
            tel_so_sy(sex,18,yr) = tel_so(sex,2,yr) * cesy_a(sex,18,yr) / ce_a(sex,2,yr)
            tel_so_sy(sex,19,yr) = tel_so(sex,2,yr) * cesy_a(sex,19,yr) / ce_a(sex,2,yr)
            do ageGrp = 3, 12
               lowAge = ageGrp * 5 + 5
               do age = lowAge, lowAge+4
                  tel_so_sy(sex,age,yr) = tel_so(sex,ageGrp,yr) * cesy_a(sex,age,yr) / ce_a(sex,ageGrp,yr)
               end do
            end do
            do age = 70, 100
               tel_so_sy(sex,age,yr) = tel_so(sex,23,yr) * cesy_a(sex,age,yr) / ce_a(sex,23,yr)
            end do
         end do
      end do
      
      ! STEP 7 - Calculate projected values for CE_M by sex and single year of age for those 10 to 100
      
      do yr = histend+1, endYr
         do sex = 1, 2
            do age = 10, 15
               ce_m_sy(sex,age,yr) = cesy_a(sex,age,yr)
            end do
            do age = 16, 100
               ce_m_sy(sex,age,yr) = cesy_a(sex,age,yr) - tel_so_sy(sex,age,yr) - teo_esf_sy(sex,age,yr)
            end do
         end do
      end do

   end subroutine coveredworkers

!===============================================================================

   subroutine histlcanndata()

      integer :: yr, group, sex
      ! These are in a slightly different order than in the command file
      integer, dimension(15) :: ageGrp1 = (/  1, 27, 24,  2,  3, 4, 18, 5, 6, 19, 7, 8, 20, 9, 10 /)
      integer, dimension(4)  :: ageGrp2 = (/ 11, 22, 12, 13 /)
      integer, dimension(5)  :: ageGrp3 = (/ 25, 26, 14, 15, 16 /)
      real (kind = 8), dimension(MAX_YR) :: ptemp, ntemp

      ntemp = 0d0
      call FetchSeries(DFILE, "NF1617M.A", ntemp) ! dfile and bkdo1 don't match 1972-81
      m_a(2,1,81) = ntemp(81)
      do yr = 81, endYr
         do sex = 1, 2
            do group = 1, 15
               p_t_a(sex,ageGrp1(group),yr) = (lc_a(sex,ageGrp1(group),yr) + m_a(sex,ageGrp1(group),yr)) / &
                  (n_a(sex,ageGrp1(group),yr) + m_a(sex,ageGrp1(group),yr))
            end do
            ! 55-64
            p_t_a(sex,21,yr) = (lc_a(sex,21,yr) + m_a(sex,10,yr)) / &
               (n_a(sex,21,yr) + m_a(sex,10,yr))
            do group = 1, 4
               p_a(sex,ageGrp2(group),yr) = lc_a(sex,ageGrp2(group),yr) / &
                  n_a(sex,ageGrp2(group),yr)
            end do
         end do
      end do

      do sex = 1, 2
         do group = 1, 5
            ptemp = 0d0
            call FetchSeries(CPSFILE, "P"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp3(group)))//".A", ptemp)
            p_a(sex,ageGrp3(group),81:103) = ptemp(81:103)
         end do
         p_a(sex,25,104:endYr) = (lcsy_a(sex,60,104:endYr) + lcsy_a(sex,61,104:endYr)) / &
            (nisy_a(sex,60,104:endYr) + nisy_a(sex,61,104:endYr))
         p_a(sex,26,104:endYr) = (lcsy_a(sex,62,104:endYr) + lcsy_a(sex,63,104:endYr) + lcsy_a(sex,64,104:endYr)) / &
            (nisy_a(sex,62,104:endYr) + nisy_a(sex,63,104:endYr) + + nisy_a(sex,64,104:endYr))
      end do

      do yr = 104, endYr
         do sex = 1, 2
            lc_a(sex,15,yr) = dot_product(psy_a(sex,80:84,yr), nisy_a(sex,80:84,yr))
         end do
      end do

      do yr = 104, endYr
         do sex = 1, 2
            p_a(sex,15,yr) = lc_a(sex,15,yr) / n_a(sex,15,yr)
            p_a(sex,16,yr) = (lc_a(sex,31,yr) - lc_a(sex,15,yr)) / n_a(sex,16,yr)
         end do
      end do

      do sex = 1, 2
         ptemp = 0d0
         call FetchSeries(CPSFILE, "P"//trim(sexLabel(sex))//"8084.A",ptemp)
         p_a(sex,15,:startYr-1) = ptemp(:startYr-1)
         ptemp = 0d0
         call FetchSeries(CPSFILE, "P"//trim(sexLabel(sex))//"85O.A", ptemp)
         p_a(sex,16,:startYr-1) = ptemp(:startYr-1)
      end do


      do yr = 81, endYr
         do sex = 1, 2
            p_a(sex,23,yr) = lc_a(sex,23,yr) / (n_a(sex,22,yr) - n_a(sex,12,yr))
         end do
      end do

      do yr = 59, endYr
         lc_t_a(yr) = lc_a(0,0,yr) + m_a(0,24,yr)
      end do

      do sex = 1, 2
         call FetchSeries(CNIPOPDATA, "P"//trim(sexLabel(sex))//"62.A", psy_a(sex,62,:))
         call FetchSeries(DFILE, "P"//trim(sexLabel(sex))//"62.A", psy_a(sex,62,:))
      end do

   end subroutine histlcanndata

!===============================================================================

end module EconMiscMod