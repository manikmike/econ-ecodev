module EconModSolAOutMod

   use EconParMod
   use EconModSolAVarMod
   use ifport
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSolAOutMain
   
   integer, parameter :: NUM_SERIES = 1300
   character (len = 18) , dimension(NUM_SERIES) :: seriesName
   integer, dimension(2,NUM_SERIES) :: per
   integer :: fileNum
   integer :: passNum = 0
   character (len = 128) :: username
   real (kind = 8), dimension(2,13,MAX_YR) :: covwkr
   real (kind = 8), dimension(2,0:100,4,MAX_YR) :: ce, ce_mef
   
contains

!===============================================================================

   subroutine EconModSolAOutMain()
   
      fileNum = 1
      passNum = passNum + 1
      if (passNum == 8) then
         call awi()
         call awitm()
         call cmsecon()
         call cpiw()
         call getCoveredWorkers() ! must call this before cwpopn and cwrate
         call cwpopn()
         call cwrate()
         call cwsing()
         if (needModData) then
            call moddata()
            call moddata_cms() ! must call moddata first to get nomintr and p_t_a
         end if
         call mult2_te_out()
         call table62()
         call ModSolAToStoch()
         if (WORKFILE_EXPORT) then
            call cov()
            call histlcanndata()
            call FetchSeriesNames()
            call WriteOutputFileEV()
            call WriteProgramEV()
            call RunProgramEV()
         end if
      end if
             
   end subroutine EconModSolAOutMain

!===============================================================================

   subroutine FetchSeriesNames()
   
      integer :: n
      
      do n = 1, NUM_SERIES
         read(0501, '(a18,2i5)') seriesName(n), per(1,n), per(2,n)
      end do
   
   end subroutine FetchSeriesNames

!===============================================================================

   subroutine MapSeries(name, series)
   
      character (len = *) :: name
      real (kind = 8), dimension(:) :: series
      
      include "CompileSeriesMaps.fh"
      
      !DEC$ IF (COMPILE_SERIES_MAPS == 1)
      
      include "ModSolASeriesMap.fh"

      else
         series = 0d0
      end if
      
      !DEC$ END IF
   
   end subroutine MapSeries

!===============================================================================

   subroutine awi()
   
      integer :: yr

      do yr = OUTPUT_TRYR - 1, min(OUTPUT_LAST_YR, endYr)
         write(5101,'(i4,f12.2)') 1900+yr, aiw_a(yr)
      end do
      
   end subroutine awi

!===============================================================================

   subroutine awitm()
   
      integer :: yr
      real (kind = 8), dimension (MAX_YR) :: aiw, taxmax

      do yr = 71, min(OUTPUT_LAST_YR, endYr)
         write(5102,'(i4,f13.2,i13)') 1900+yr, aiw_a(yr), nint(1d3*taxmax_a(yr))
      end do
      
   end subroutine awitm

!===============================================================================
   
   subroutine cmsecon()

      integer :: yr
      real (kind = 8), dimension(MAX_YR) :: y, aeus, aheus, acus, ahcus

      y = yf_a + ynf_a
      aeus = (1000d0 * (wsd_a + y) ) / (e_a(0,0,:) + edmil_a) / 52d0;
      aheus = aeus/ahrs_a;
      acus = (1000d0 * (wss_a + y)) / (e_a(0,0,:) + edmil_a) / 52d0;
      ahcus = acus / ahrs_a;

      if (needHist) then
         write(5123,'(/a//)') "               gdp       "//trim(ToLower(REAL_GDP_SERIES))//"    pch(prod)    ahrs     pch(ahrs)" // &
            "  pch(aeus) pch(aheus) pch(ahcus)"

         do yr = 51, TRYR-2
            write(5123,'(i7,f15.3,7f11.3)') 1900+yr, gdp_a(yr), gdpreal_a(yr), 100*(prod_a(yr)/prod_a(yr-1)-1), &
               ahrs_a(yr), 100*(ahrs_a(yr)/ahrs_a(yr-1)-1), 100*(aeus(yr)/aeus(yr-1)-1), &
               100*(aheus(yr)/aheus(yr-1)-1), 100*(ahcus(yr)/ahcus(yr-1)-1)
         end do
         write(5123,*)
         write(5123,*)
      end if
      
      write(5124,'(/a//)') "              wswahi     csw_tot      tceahi       gdp        "//trim(ToLower(REAL_GDP_SERIES))//"     pch(prod)" // &
         "      ahrs     pch(ahrs)   pch(aeus)   pch(aheus)  pch(ahcus)"
      
      do yr = TRYR-1, min(OUTPUT_LAST_YR, endYr)
         write(5124,'(i7,f16.3,2f12.3,f15.3,7f11.3)') 1900+yr, wswahi_a(yr), csw_tot_a(yr), tceahi_a(yr), &
            gdp_a(yr), gdpreal_a(yr), 100*(prod_a(yr)/prod_a(yr-1)-1), &
            ahrs_a(yr), 100*(ahrs_a(yr)/ahrs_a(yr-1)-1), 100*(aeus(yr)/aeus(yr-1)-1), &
            100*(aheus(yr)/aheus(yr-1)-1), 100*(ahcus(yr)/ahcus(yr-1)-1)
      end do
      write(5124,*)
      write(5124,*)

   end subroutine cmsecon

!===============================================================================

   subroutine cpiw()
   
      integer :: yr

      do yr = OUTPUT_TRYR - 1, min(OUTPUT_LAST_YR, endYr)
         write(5150,'(i4,f10.3)') 1900+yr, RoundCl(cpiw_u_a(yr),5) * 1d2
      end do
      
   end subroutine cpiw

!===============================================================================

   subroutine getCoveredWorkers()

      integer :: sex, group, age, mstat, yr
      character (len = 4), dimension(13) :: label = &
         (/ "19u", "2024", "2529", "3034", "3539", "4044", "4549", &
            "5054", "5559", "6064", "6569", "70o", "totl" /)
      character (len=2), dimension(4) :: mstatLabel = (/ "NM", "MS", "W", "D"/)
      real (kind = 8), dimension(MAX_YR) :: temp, temp1, temp2

      covwkr = 0d0
      do sex = 1, 2

         call FetchSeries(CE3750,"CE" // trim(sexLabel(sex))//"19U.A", covwkr(sex,1,:))
         call FetchSeries(CE3750,"CE" // trim(sexLabel(sex)) // "70O.A", covwkr(sex,12,:))

         call FetchSeries(AFILE, "CE"//trim(sexLabel(sex))//".A", temp1)
         call FetchSeries(AFILE, "CE"//trim(sexLabel(sex))//"16O.A", temp2)

         do group = 1, 12
            call FetchSeries(AFILE, "CE"//trim(sexLabel(sex))//trim(ageGrpLabel(group))//".A",temp)
            ce_a(sex,group,51:80) = temp(51:80)
         end do
         group = 23
         call FetchSeries(AFILE, "CE"//trim(sexLabel(sex))//trim(ageGrpLabel(group))//".A",temp)
         ce_a(sex,group,51:80) = temp(51:80)
         
         covwkr(sex,1,51:80) = temp1(51:80) - temp2(51:80) + &
            ce_a(sex,1,51:80) + ce_a(sex,2,51:80)
         
         covwkr(sex,2:11,51:80) = ce_a(sex,3:12,51:80)
         covwkr(sex,12,51:80) = ce_a(sex,23,51:80)
         
         covwkr(sex,1,81:) = ce_m_15u(sex,81:) + &
            ce_a(sex,1,81:) + ce_a(sex,2,81:)

         do group = 2, 11
            call FetchSeries(CE3750,"CE" // trim(sexLabel(sex)) // &
               trim(label(group)) // ".A", covwkr(sex,group,:))
            covwkr(sex,group,81:) = ce_a(sex,group+1,81:)
         end do
         covwkr(sex,12,81:) = ce_a(sex,23,81:)
         
         do yr = 37, endYr
            covwkr(sex,13,yr) = sum(covwkr(sex,1:12,yr))
         end do

      end do

      ce = 0d0
      ce_mef = 0d0
      do sex = 1,2
         do age = 0, 100
            temp = 0d0
            call FetchSeries(AFILE,  "CE"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A",temp)
            cesy_a(sex,age,51:80) = temp(51:80)
            temp = 0d0
            if (age < 100) then
               call FetchSeries(MEF,  "CE_M_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A",temp)
            else
               call FetchSeries(MEF,  "CE_M_"//trim(sexLabel(sex))//trim(IntToAsc(age))//"O.A",temp)
            end if
            ce_m_sy(sex,age,51:80) = temp(51:80)
            temp = 0d0
            if (age > 15) then
               call FetchSeries(OTLFILE, "TEO_MEFC_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A",temp)
               teo_mefc_sy(sex,age,51:80) = temp(51:80)
            end if
            temp = 0d0
            do mstat = 1, 4
               call FetchSeries(CE3750, "CE" // trim(sexLabel(sex)) // &
                  trim(IntToAsc(age)) // trim(mstatLabel(mstat)) // ".A", ce(sex,age,mstat,:))
               ce_mef(sex,age,mstat,:) = ce(sex,age,mstat,:)
               if (mstat == 1) then
                  ce(sex,age,mstat,51:) = cesy_a(sex,age,51:)
                  ce_mef(sex,age,mstat,51:) = ce_m_sy(sex,age,51:)
               end if
            end do
         end do
      end do
      
      ce = 1d6 * ce
      ce_mef = 1d6 * ce_mef

   end subroutine getCoveredWorkers

!===============================================================================

   subroutine cwpopn()
   
      integer :: sex, group, yr
      
      do yr = 37, min(OUTPUT_LAST_YR, endYr)
         do sex = 1, 2
            if (yr >= OUTPUT_TRYR - 1) then
               write(5103,'(i4,13i11)') 1900+yr, (nint(1d6*covwkr(sex,group,yr)),group=1,13)
            else
               if (needHist) write(5104,'(i4,13i11)') 1900+yr, (nint(1d6*covwkr(sex,group,yr)),group=1,13)
            end if
         end do
      end do

   end subroutine cwpopn

!===============================================================================

   subroutine cwrate()
   
      integer :: yr, sex, group
      integer, dimension(13) :: lowAge = (/  0, 20, 25, 30, 35, 40, 45, &
         50, 55, 60, 65,  70,   0 /)
      integer, dimension(13) :: hiAge  = (/ 19, 24, 29, 34, 39, 44, 49, &
         54, 59, 64, 69, 100, 100 /)
      
      do yr = 47, min(OUTPUT_LAST_YR, endYr)
         do sex = 1, 2
            if (yr >= OUTPUT_TRYR - 1) then
               write(5105,'(i4,13f7.4)') 1900+yr, (covwkr(sex,group,yr) / &
                  sum(nsy_a(sex, lowAge(group):hiAge(group),yr)),group=1,13)
            else
               if (needHist) write(5106,'(i4,13f7.4)') 1900+yr, (covwkr(sex,group,yr) / &
                  sum(nsy_a(sex, lowAge(group):hiAge(group),yr)),group=1,13)
            end if
         end do
      end do

   end subroutine cwrate

!===============================================================================

   subroutine cwsing()
   
      integer :: age, sex, marstat, yr, year

      do yr = 37, min(OUTPUT_LAST_YR, endYr)
         year = 1900+yr
         do age = 0, 100
            if (yr >= OUTPUT_TRYR - 1) then
               write(5117, '(i4,1x,i3,8i9)') year, age, ((nint(ce(sex,age,marstat,yr)),marstat=1,4),sex=1,2)
               write(5119, '(i4,1x,i3,8i9)') year, age, ((int(nint(ce_mef(sex,age,marstat,yr))),marstat=1,4),sex=1,2)
               if (age > 15) then
                  write(5121, '(i4,1x,i3,8i9)') year, age, nint(sum(ce_mef(1,age,1:4,yr)) - 1d6 * teo_mefc_sy(1,age,yr)), 0, 0, 0, &
                                               nint(sum(ce_mef(2,age,1:4,yr)) - 1d6 * teo_mefc_sy(2,age,yr)), 0, 0, 0
               else
                  write(5121, '(i4,1x,i3,8i9)') year, age, nint(sum(ce_mef(1,age,1:4,yr))), 0, 0, 0, &
                                               nint(sum(ce_mef(2,age,1:4,yr))), 0, 0, 0
               end if
            else if (needHist) then
               write(5118, '(i4,1x,i3,8i9)') year, age, ((nint(ce(sex,age,marstat,yr)),marstat=1,4),sex=1,2)
               write(5120, '(i4,1x,i3,8i9)') year, age, ((nint(ce_mef(sex,age,marstat,yr)),marstat=1,4),sex=1,2)
               if (age > 15) then
                  write(5122, '(i4,1x,i3,8i9)') year, age, nint(sum(ce_mef(1,age,1:4,yr)) - 1d6 * teo_mefc_sy(1,age,yr)), 0, 0, 0, &
                                               nint(sum(ce_mef(2,age,1:4,yr)) - 1d6 * teo_mefc_sy(2,age,yr)), 0, 0, 0
               else
                  write(5122, '(i4,1x,i3,8i9)') year, age, nint(sum(ce_mef(1,age,1:4,yr))), 0, 0, 0, &
                                               nint(sum(ce_mef(2,age,1:4,yr))), 0, 0, 0
               end if
            end if
         end do
      end do
   
   end subroutine cwsing

!===============================================================================

   subroutine cov

      integer :: sex, age

      do sex = 2, 1, -1
         do age = 0, 100

            write(5004,'(a)') "pageselect a"
            write(5004,'(a)') "series ce"// trim(ToLower(sexLabel(sex))) // trim(IntToAsc(age))
            write(5004,'(a,i0,a)') "ce"// trim(ToLower(sexLabel(sex))) // trim(IntToAsc(age)) // ".fill(o=", 1951,  ") _" 
            call WriteSeriesEV(cesy_a(sex,age,:), 51, min(max(199, OUTPUT_LAST_YR), endYr), 5004)
            write(5004,*)

            write(5004,'(a)') "pageselect a"
            write(5004,'(a)') "series ce_m_" // trim(ToLower(sexLabel(sex))) // trim(IntToAsc(age))
            write(5004,'(a,i0,a)') "ce_m_"// trim(ToLower(sexLabel(sex))) // trim(IntToAsc(age)) // ".fill(o=", 1951,  ") _" 
            call WriteSeriesEV(ce_m_sy(sex,age,:), 51, min(max(199, OUTPUT_LAST_YR), endYr), 5004)
            write(5004,*)

         end do
      end do

      call CloseFile(5004)

   end subroutine cov

!===============================================================================

   subroutine histlcanndata()

      integer :: sex, group, age

      integer, dimension(16) :: ageGrp1 = (/  1, 27, 24,  2,  3,  4, 18,  5,  6, 19, 7, 8, 20, 9, 10, 21 /)
      integer, dimension(10) :: ageGrp2 = (/ 25, 11, 26, 12, 22, 13, 23, 14, 15, 16 /)
      integer, dimension(10) :: ageGrp2StartYr = (/ 81, 62, 81, 62, 48, 81, 81, 81, 81, 81 /)
 
      write(5006,'(a)') "pageselect a"
      write(5006,'(a)') "series l_t"
      write(5006,'(a,i0,a)') "l_t.fill(o=", 1959,  ") _"
      call WriteSeriesEV(lc_t_a, 59, min(max(199, OUTPUT_LAST_YR), endYr), 5006)
      write(5006,*)

      do sex = 2, 1, -1

         do group = 1, 16
            write(5006,'(a)') "pageselect a"
            write(5006,'(a)') "series p"//trim(ToLower(sexLabel(sex)))//trim(ageGrpLabel(ageGrp1(group)))//"_t"
            write(5006,'(a,i0,a)') "p"//trim(ToLower(sexLabel(sex)))//trim(ageGrpLabel(ageGrp1(group)))//"_t.fill(o=", 1981,  ") _"
            call WriteSeriesEV(p_t_a(sex,ageGrp1(group),:), 81, min(max(199, OUTPUT_LAST_YR), endYr), 5006)
            write(5006,*)
         end do

         do group = 1, 10
            write(5006,'(a)') "pageselect a"
            write(5006, '(a)') "series p"//trim(ToLower(sexLabel(sex)))//trim(ageGrpLabel(ageGrp2(group)))
            write(5006, '(a,i0,a)') "p"//trim(ToLower(sexLabel(sex)))//trim(ageGrpLabel(ageGrp2(group)))//".fill(o=", ageGrp2StartYr(group),  ") _"
            call WriteSeriesEV(p_a(sex,ageGrp2(group),:), ageGrp2StartYr(group), min(max(199, OUTPUT_LAST_YR), endYr), 5006)
            write(5006,*)
            if (group == 2) then
               write(5006,'(a)') "pageselect a"
               write(5006, '(a)') "series p"//trim(ToLower(sexLabel(sex)))//"62"
               write(5006, '(a,i0,a)') "p"//trim(ToLower(sexLabel(sex)))//"62.fill(o=", 1965,  ") _"
               call WriteSeriesEV(psy_a(sex,62,:), 65, min(max(199, OUTPUT_LAST_YR), endYr), 5006)
               write(5006,*)
            end if
         end do

      end do

      call CloseFile(5006)

   end subroutine histlcanndata

!===============================================================================

   subroutine moddata()

      integer :: yr, qtr, group, sex, mth, alt, n
      real (kind = 8), dimension(MAX_YR) :: aiwtm_a
      real (kind = 8), dimension(MAX_YR) :: temp
      real (kind = 8), dimension(MAX_MTH) :: nomintrx
      real (kind = 8) :: lo, hi, best
      integer, dimension(26) :: ageGroup = (/ 24, 27, 1, 2, 3, 18, 4, 5, 19, 6, 7, &
         20, 8, 9, 21, 10, 11, 25, 26, 22, 12, 23, 13, 14, 15, 16 /)
      integer :: lastYr, lastQtr, lastMth
      
      lastYr = LAST_MODDATA_YEAR - 1900
      lastQtr = lastYr * 4 + 3
      lastMth = lastYr * 12 + 11

      ! Quarterly CPI (indexed to 1) not seasonally adjusted 1981Q1 to LAST_MODDATA_YEAR:Q4
      write(5114,'(8f9.5)') (cpiw_u(qtr),qtr=81*4,lastQtr)

      if (isBudgetRun .and. endYr < 185) then
         do yr = endYr+1, 185
            aiw_a(yr) = aiw_a(yr-1) * (aiw_a(yr-1) / aiw_a(yr-2))
         end do
      end if
      ! Annual average wage for indexing in dollars 1981-2085
      write(5114,'(6f12.2)') (nint(1d2*aiw_a(yr))/1d2,yr=81,185)

      ! Obsolete annual average wage for indexing in dollars 1981-2025
      call FetchSeries(DFILE, "AIWTM.A", aiwtm_a)
      do yr = sample(2) + 1, lastYr
         aiwtm_a(yr) = aiwtm_a(yr-1) * (aiw_a(yr) / aiw_a(yr-1))
      end do
      write(5114,'(6f12.2)') (aiwtm_a(yr),yr=81,lastYr)
      
      if (isBudgetRun .and. endYr < 185) then
         do yr = endYr+1, 185
            acwa_a(yr) = acwa_a(yr-1) * (acwa_a(yr-1) / acwa_a(yr-2))
         end do
      end if
      ! Annual average OASDI covered wages for in dollars 1981-2085
      write(5114,'(6f12.3)') (nint(1d6*acwa_a(yr))/1d3,yr=81,185)

      ! Annual deemed military wage credits in millions of dollars 1982-LAST_MODDATA_YEAR
      write(5114,'(7i10)') (nint(1d3*dmwc_o_a(yr)),yr=81,lastYr)
      
      if (isBudgetRun .and. endYr < lastYr) then
         do yr = endYr+1, lastYr
           gdpreal_a(yr) = gdpreal_a(yr-1) * (gdpreal_a(yr-1) / gdpreal_a(yr-2))
         end do
      end if

      ! Annual real GDP in billions of dollars 1981-LAST_MODDATA_YEAR
      write(5114,'(7f10.1)') (nint(10d0 * gdpreal_a(yr))/10d0,yr=81,lastYr)
      
      if (isBudgetRun .and. endYr < lastYr) then
         do yr = endYr+1, lastYr
            ru_a(0:2,0,yr) = ru_a(0:2,0,yr-1)
         end do
      end if

      ! Annual average total unemployment rates as percent 1981-LAST_MODDATA_YEAR
      write(5114,'(7f10.1)') (nint(10d0 * ru_a(0,0,yr)) / 10d0,yr=81,lastYr)

      ! Annual average female unemployment rates as percent 1981-LAST_MODDATA_YEAR
      write(5114,'(7f10.1)') (nint(10d0 * ru_a(2,0,yr)) / 10d0,yr=81,lastYr)

      ! Annual average male unemployment rates as percent 1981-LAST_MODDATA_YEAR
      write(5114,'(7f10.1)') (nint(10d0 * ru_a(1,0,yr)) / 10d0,yr=81,lastYr)

      ! Annual average labor force participation rates (including military) 1981-LAST_MODDATA_YEAR
      ! by sex and age group (males 16+, 16-19, 16-17, 18-19, 20-24, 25-34, 25-29,
      ! 30-34, 35-44, 35-39, 40-44, 45-54, 45-49, 50-54, 55-64, 55-59, 60-64, &
      ! 60-61, 62-64, 65+, 65-69, 70+, 70-74, 75-79, 80-84, 85+)
      do sex = 1, 2
         do group = 1, 16
            if (isBudgetRun .and. endYr < lastYr) then
               do yr = endYr+1, lastYr
                  p_t_a(sex,ageGroup(group),yr) = p_t_a(sex,ageGroup(group),yr-1) * &
                     (p_t_a(sex,ageGroup(group),yr-1) / p_t_a(sex,ageGroup(group),yr-2))
               end do
            end if
            write(5114,'(10f7.4)') (p_t_a(sex,ageGroup(group),yr),yr=81,lastYr)
         end do
         do group = 17, 26
            if (isBudgetRun .and. endYr < lastYr) then
               do yr = endYr+1, lastYr
                  p_a(sex,ageGroup(group),yr) = p_a(sex,ageGroup(group),yr-1) * &
                     (p_a(sex,ageGroup(group),yr-1) / p_a(sex,ageGroup(group),yr-2))
               end do
            end if
            write(5114,'(10f7.4)') (p_a(sex,ageGroup(group),yr),yr=81,lastYr)
         end do
      end do

      ! Annual OASDI covered employment in thousands 1981-LAST_MODDATA_YEAR
      !write(5114,'(10i7)') (nint(1d3*ce_a(0,0,yr)),yr=81,lastYr)
      do yr = 81, lastYr
         temp(yr) = 1d3*(sum(ce_m_15u(1:2,yr))+sum(ce_a(1:2,1:12,yr))+sum(ce_a(1:2,23,yr)))
      end do
      if (isBudgetRun .and. endYr < lastYr) then
         do yr = endYr+1, lastYr
            temp(yr) = temp(yr-1)  * &
               (temp(yr-1) / temp(yr-2))
         end do
      end if
      write(5114,'(10i7)') (nint(temp(yr)),yr=81,lastYr)

      ! Annual average total labor force (including military) in thousands 1981-LAST_MODDATA_YEAR
      if (isBudgetRun .and. endYr < lastYr) then
         do yr = endYr+1, lastYr
            lc_a(0,0,yr) = lc_a(0,0,yr-1)  * &
               (lc_a(0,0,yr-1) / lc_a(0,0,yr-2))
         end do
      end if
      write(5114,'(8i9)') (nint(1d3*(lc_a(0,0,yr) + m_a(0,24,yr))),yr=81,lastYr)

      ! Monthly interest rates on new trust fund issues in percent 1981:M1 to LAST_MODDATA_YEAR:M12
      alt = 2
      if (trim(FILE_EXTENSION) == "alt1") then
         alt = 1
      else if (trim(FILE_EXTENSION) == "alt3") then
         alt = 3
      end if
      call FetchSeries(AFILE, "NOMINTR.M", nomintr)
      if (sample(2)+1 <= lastMth) nomintr(sample(2)+1:lastMth) = nomintr(sample(2))

      write(5114,'(12f8.3)') (nomintr(mth),mth=81*12,lastMth)

   end subroutine moddata

!===============================================================================

   subroutine moddata_cms()
   
      integer, parameter :: firstYr = 85
      integer, dimension(10) :: ageGroup = &
         (/ 24, 27, 3, 18, 19, 20, 21, 12, 23, 16 /)
      integer :: lastYrA, lastYrQ ! last yrs for annual and quarterly data
      integer :: yr, qtr, mth
      real (kind = 8), dimension(MAX_YR) :: pchcpiw_u_a, pchaiw_a, pchacwa_a, pchacwahi_a, pchgdpreal_a
      real (kind = 8), dimension(MAX_YR) :: nomintr_a
      real (kind = 8), dimension(MAX_YR) :: realintr_a
      real (kind = 8), dimension(MAX_YR) :: tctemp, wctemp, wstemp, cpitemp
      character (len=10) :: hitaxmax
      lastYrQ = AscToInt(assumpt(1)(3:4)) + 100 + 9 ! This is different for TR versus Budget
      lastYrA = min(endYr, lastYrQ + 65)
      
      write(5116,'(a)') "CPIW"
      do yr = firstYr, lastYrQ
         qtr = yr * 4
         if (yr < 107) then
            write(5116,'(i4,5f8.3)') 1900 + yr, 1d2 * cpiw_u(qtr:qtr+3), nint(1d3 * cpiw_u_a(yr)) / 10d0
         else
            write(5116,'(i4,5f8.3)') 1900 + yr, 1d2 * cpiw_u(qtr:qtr+3), nint(1d5 * cpiw_u_a(yr)) / 1d3
         end if
      end do
      
      write(5116,'(a)') "CPIW % chg"
      do yr = firstYr, lastYrQ
         qtr = yr * 4
         if (yr < 107) then
            pchcpiw_u_a(yr) = nint(1d6 * (RoundCl(1d2*cpiw_u_a(yr),1) / RoundCl(1d2*cpiw_u_a(yr-1),1) - 1d0)) / 1d4
            write(5116,'(i4,5f8.4)') 1900 + yr, nint(1d6 * (RoundCl(1d2*cpiw_u(qtr:qtr+3),1) / RoundCl(1d2*cpiw_u(qtr-4:qtr-1),1) - 1d0)) / 1d4, &
               pchcpiw_u_a(yr)
         else if (yr <= 115) then
            pchcpiw_u_a(yr) = nint(1d6 * (RoundCl(1d2*cpiw_u_a(yr),3) / RoundCl(1d2*cpiw_u_a(yr-1),3) - 1d0)) / 1d4
            write(5116,'(i4,5f8.4)') 1900 + yr, nint(1d6 * (RoundCl(1d2*cpiw_u(qtr:qtr+3),3) / RoundCl(1d2*cpiw_u(qtr-4:qtr-1),3) - 1d0)) / 1d4, &
               pchcpiw_u_a(yr)
         else if (yr == 116) then
            pchcpiw_u_a(yr) = nint(1d6 * (1d2*cpiw_u_a(yr) / RoundCl(1d2*cpiw_u_a(yr-1),3) - 1d0)) / 1d4
            write(5116,'(i4,5f8.4)') 1900 + yr, nint(1d6 * (RoundCl(1d2*cpiw_u(qtr:qtr+3),3) / RoundCl(1d2*cpiw_u(qtr-4:qtr-1),3) - 1d0)) / 1d4, &
               pchcpiw_u_a(yr)
         else
            pchcpiw_u_a(yr) = nint(1d6 * (cpiw_u_a(yr) / cpiw_u_a(yr-1) - 1d0)) / 1d4
            write(5116,'(i4,5f8.4)') 1900 + yr, nint(1d6 * (RoundCl(1d2*cpiw_u(qtr:qtr+3),3) / RoundCl(1d2*cpiw_u(qtr-4:qtr-1),3) - 1d0)) / 1d4, &
               pchcpiw_u_a(yr)
         end if
      end do
      do yr = lastYrQ, endYr
         pchcpiw_u_a(yr) = nint(1d6 * (cpiw_u_a(yr) / cpiw_u_a(yr-1) - 1d0)) / 1d4
      end do
      
      write(5116,'(a)') "Monthly interest rates"
      do yr = firstYr, lastYrQ
         mth = 12 * yr
         write(5116,'(i4,12f8.3)') 1900 + yr, nomintr(mth:mth+11)
      end do
      
      write(5116,'(a)') "Annual interest rates"
      mth = 12 * (firstYr - 1)
      nomintr_a(firstYr-1) = sum(nomintr(mth:mth+11)) / 12d0
      do yr = firstYr, lastYrA
         mth = 12 * yr
         nomintr_a(yr) = sum(nomintr(mth:mth+11)) / 12d0
         ! Use unrounded rates to match LR assumption value
         ! The rounded rates are only for short-range moddata file
         realintr_a(yr) = 1d2 * ((1d0 + nomintr_a(yr-1)/2d2)**2 / (1d0 + pchcpiw_u_a(yr)/1d2) - 1d0)
         write(5116,'(i4,5f8.4)') 1900 + yr, nomintr_a(yr), pchcpiw_u_a(yr), realintr_a(yr)
      end do
      
      write(5116,'(a)') "Male part. rates"
      do yr = firstYr, lastYrQ
         write(5116,'(i4,10f8.2)') 1900 + yr, 1d2 * p_t_a(1,ageGroup(1:7),yr), 1d2 * p_a(1,ageGroup(8:10),yr)
      end do
      
      write(5116,'(a)') "Female part. rates"
      do yr = firstYr, lastYrQ
         write(5116,'(i4,10f8.2)') 1900 + yr, 1d2 * p_t_a(2,ageGroup(1:7),yr), 1d2 * p_a(2,ageGroup(8:10),yr)
      end do
      
      write(5116,'(a)') "Ben inc, AWI, wkrs etc"
      call FetchSeries(AFILE, "TCEAHI.A", tctemp(:))
      tceahi_a(85:86) = tctemp(85:86) ! Needed for CMS
      do yr = firstYr, lastYrQ
         pchaiw_a(yr) = 1d2 * (aiw_a(yr) / aiw_a(yr-1) - 1d0)
         if (yr <= 90) then
             hitaxmax = trim(IntToAsc(nint(1d3*taxmax_a(yr)),useCommas=.true.))
         else if (yr == 91) then
            hitaxmax = "   125,000"
         else if (yr == 92) then
            hitaxmax = "   130,200"
         else if (yr == 93) then
            hitaxmax = "   135,000"
         else
            hitaxmax = "       N/A"
         end if

         write(5116,'(i4,f10.1,a10,f10.1,2a10,3a10,2a10)') 1900 + yr, beninc_a(yr),trim(DoublePrecToAsc(aiw_a(yr),2,.true.)), &
            pchaiw_a(yr),trim(IntToAsc(nint(1d3*taxmax_a(yr)),useCommas=.true.)), trim(hitaxmax), &
            trim(IntToAsc(nint(1d3*tcea_a(yr)),useCommas=.true.)), trim(IntToAsc(nint(1d3*tceahi_a(yr)),useCommas=.true.)), &
            trim(IntToAsc(nint(1d3*(lc_a(0,0,yr) + edmil_a(yr))),useCommas=.true.)), &
            trim(IntToAsc(nint(1d3*dmwc_o_a(yr)),useCommas=.true.)), trim(IntToAsc(nint(1d3*dmwc_h_a(yr)),useCommas=.true.))
      end do
      
      write(5116,'(a)') "Avg cov wages etc"
      call FetchSeries(DFILE, "WSWAHI.A", wstemp)
      wswahi_a(84:86) = wstemp(84:86)
      acwahi_a(84:107) = wscahi_a(84:107) / wswahi_a(84:107)
      do yr = firstYr, lastYrQ
         pchacwa_a(yr) = 1d2 * (acwa_a(yr) / acwa_a(yr-1) - 1d0)
         pchacwahi_a(yr) = 1d2 * (acwahi_a(yr) / acwahi_a(yr-1) - 1d0)
         pchgdpreal_a(yr) = 1d2 * (gdpreal_a(yr) / gdpreal_a(yr-1) - 1d0)
         if (yr < 107) then
            cpitemp(yr) = nint(1d3 * cpiw_u_a(yr)) / 10d0
         else
            cpitemp(yr) = nint(1d5 * cpiw_u_a(yr)) / 1d3
         end if
         write(5116,'(i4,x,a10,f10.1,a10,f10.1,f10.3,8f10.1)') 1900 + yr,  trim(DoublePrecToAsc(1d3*acwa_a(yr),2,.true.)), pchacwa_a(yr), &
            trim(DoublePrecToAsc(1d3*acwahi_a(yr),2,.true.)), pchacwahi_a(yr), cpitemp(yr), pchcpiw_u_a(yr), pchacwa_a(yr) - pchcpiw_u_a(yr), &
            pchgdpreal_a(yr), ru_a(0:2,0,yr), nomintr_a(yr), realintr_a(yr)
      end do

      !write(5116,'(8f9.5)') (cpiw_u(qtr),qtr=81*4,lastQtr)

    end subroutine moddata_cms

!===============================================================================
    
    subroutine mult2_te_out()
    
       integer :: yr
    
       write(1501,'(a/)') "       MULT2_TE FACTOR"
       write(1501,'(a8,a18)') "    Year", "          mult2_te"
       write(1501,'(a8,a18)') "    ----", "          --------"
       do yr = TRYR-4, TRYR+4
          write(1501,'(i8,f18.13)') 1900+yr, mult2_te(1,1,yr) ! all age-sex groups have the same factor
       end do
    
    end subroutine mult2_te_out
    
!===============================================================================
    
   subroutine table62()

      integer :: yr

      ! write (5115,'(a4,6a9)') "Year", "EGGEFC", "TEFC", "TEFC_N", &
      !    "EGGESL", "TESL", "TESL_N"

      do yr = 98, min(OUTPUT_LAST_YR, endYr)
         write (5115,'(i4,6f9.3)') 1900+yr, eggefc_a(yr), tefc_a(yr), tefc_n_a(yr), &
            eggesl_a(yr), tesl_a(yr), tesl_n_a(yr)
      end do

   end subroutine table62

!===============================================================================
   
   subroutine ModSolAToStoch()
   
      integer :: iSex, iAge, iYear
      character(len=16) :: serName, blnkStr = "                "
      character(len=12) :: serData
      real (kind = 8), dimension(0:2,0:35,MAX_YR) :: csrs_a
      
      call FetchSeries(MEF, "HE_WOF_M_F1617.A", csrs_a(2,1,:))
      call FetchSeries(MEF, "HE_WOF_M_F1819.A", csrs_a(2,2,:))
      call FetchSeries(MEF, "HE_WOF_M_F2024.A", csrs_a(2,3,:))
      call FetchSeries(MEF, "HE_WOF_M_F2529.A", csrs_a(2,4,:))
      call FetchSeries(MEF, "HE_WOF_M_F3034.A", csrs_a(2,5,:))
      call FetchSeries(MEF, "HE_WOF_M_F3539.A", csrs_a(2,6,:))
      call FetchSeries(MEF, "HE_WOF_M_F4044.A", csrs_a(2,7,:))
      call FetchSeries(MEF, "HE_WOF_M_F4549.A", csrs_a(2,8,:))
      call FetchSeries(MEF, "HE_WOF_M_F5054.A", csrs_a(2,9,:))
      call FetchSeries(MEF, "HE_WOF_M_F5559.A", csrs_a(2,10,:))
      call FetchSeries(MEF, "HE_WOF_M_F6064.A", csrs_a(2,11,:))
      call FetchSeries(MEF, "HE_WOF_M_F6569.A", csrs_a(2,12,:))
      call FetchSeries(MEF, "HE_WOF_M_F70O.A", csrs_a(2,23,:))
      call FetchSeries(MEF, "HE_WOF_M_M1617.A", csrs_a(1,1,:))
      call FetchSeries(MEF, "HE_WOF_M_M1819.A", csrs_a(1,2,:))
      call FetchSeries(MEF, "HE_WOF_M_M2024.A", csrs_a(1,3,:))
      call FetchSeries(MEF, "HE_WOF_M_M2529.A", csrs_a(1,4,:))
      call FetchSeries(MEF, "HE_WOF_M_M3034.A", csrs_a(1,5,:))
      call FetchSeries(MEF, "HE_WOF_M_M3539.A", csrs_a(1,6,:))
      call FetchSeries(MEF, "HE_WOF_M_M4044.A", csrs_a(1,7,:))
      call FetchSeries(MEF, "HE_WOF_M_M4549.A", csrs_a(1,8,:))
      call FetchSeries(MEF, "HE_WOF_M_M5054.A", csrs_a(1,9,:))
      call FetchSeries(MEF, "HE_WOF_M_M5559.A", csrs_a(1,10,:))
      call FetchSeries(MEF, "HE_WOF_M_M6064.A", csrs_a(1,11,:))
      call FetchSeries(MEF, "HE_WOF_M_M6569.A", csrs_a(1,12,:))
      call FetchSeries(MEF, "HE_WOF_M_M70O.A", csrs_a(1,23,:))

      write (serData,'(a2,2i5)') " a", TRYEAR - 2, 1900 + endYr
      
      do iSex = 2, 1, -1
        do iAge = 1, 12
          serName = "aww" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ageGrpLabel(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     aww(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
        iAge = 23 ! 70o
        serName = "aww" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   aww(iSex,iAge,TRYEAR-1900-2:endYr)
      end do
      
      serName = "ce_m"
      do iYear = TRYEAR-1900-2, endYr
        ce_m_sy(0,0,iYear) = sum(ce_m_sy(1:2,0:100,iYear))
      end do
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 ce_m_sy(0,0,TRYEAR-1900-2:endYr)

      serName = "eprrb"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 eprrb_a(0,0,TRYEAR-1900-2:endYr)

      serName = "seo"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                seo_a(0,0,TRYEAR-1900-2:endYr)

      serName = "te"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                te_a(0,0,TRYEAR-1900-2:endYr)

      serName = "te_sfo_lrp"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                te_sfo_lrp(0,0,TRYEAR-1900-2:endYr)

      serName = "teph_n"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                teph_n_a(0,0,TRYEAR-1900-2:endYr)

      serName = "tepo_n"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                tepo_n_a(0,0,TRYEAR-1900-2:endYr)

      do iSex = 2, 1, -1
        do iAge = 1, 12
          serName = "ce" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ageGrpLabel(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     ce_a(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
        iAge = 23 ! 70o
        serName = "ce" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   ce_a(iSex,iAge,TRYEAR-1900-2:endYr)
      end do
      
      do iSex = 2, 1, -1
        do iAge = 1, 12
          serName = "te" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ageGrpLabel(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     te_a(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
        iAge = 23 ! 70o
        serName = "te" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   te_a(iSex,iAge,TRYEAR-1900-2:endYr)
      end do
      
      serName = "tef1013"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 te1013_a(2,TRYEAR-1900-2:endYr)

      serName = "tef1415"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 te1415_a(2,TRYEAR-1900-2:endYr)

      serName = "tem1013"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 te1013_a(1,TRYEAR-1900-2:endYr)

      serName = "tem1415"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 te1415_a(1,TRYEAR-1900-2:endYr)

      do iSex = 2, 1, -1
        do iAge = 0, 100
          serName = "ce" // trim(ToLower(trim(sexLabel(iSex)))) // trim(NumToAsc(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     cesy_a(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
      end do
      
      do iSex = 2, 1, -1
        do iAge = 1, 12
          serName = "csrs" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ageGrpLabel(iAge))
          write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                     csrs_a(iSex,iAge,TRYEAR-1900-2:endYr)
        end do
        iAge = 23 ! 70o
        serName = "csrs" // trim(ToLower(trim(sexLabel(iSex)))) // trim(ToLower(ageGrpLabel(iAge)))
        write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                   csrs_a(iSex,iAge,TRYEAR-1900-2:endYr)
      end do
      
      serName = "acwa"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 acwa_a(TRYEAR-1900-2:endYr)

      serName = "aiw" ! NOTE DIFFERENT FORMAT
      write (9999,'(a28,94f15.6)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 aiw_a(TRYEAR-1900-2:endYr)

      serName = "beninc"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 beninc_a(TRYEAR-1900-2:endYr)

      serName = "cpb"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 cpb_a(TRYEAR-1900-2:endYr)

      serName = "cpb_xeo"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 cpb_xeo(TRYEAR-1900-2:endYr)

      serName = "cse_tot"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 cse_tot_a(TRYEAR-1900-2:endYr)

      serName = "enawp_b"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 enawp_b_a(TRYEAR-1900-2:endYr)

      serName = "rtp"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 rtp_a(TRYEAR-1900-2:endYr)

      serName = "seo_hi"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 seo_hi_a(TRYEAR-1900-2:endYr)

      serName = "tcea"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 tcea_a(TRYEAR-1900-2:endYr)

      serName = "tep_n_n_s"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 tep_n_n_s_a(TRYEAR-1900-2:endYr)

      serName = "tefc_n_n"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 tefc_n_n_a(TRYEAR-1900-2:endYr)

      serName = "te_sfm_lrp"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 te_sfm_lrp_a(TRYEAR-1900-2:endYr)

      serName = "tesl_n_n"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 tesl_n_n_a(TRYEAR-1900-2:endYr)

      serName = "tesl_n_n_nhi"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 tesl_n_n_nhi_a(TRYEAR-1900-2:endYr)

      serName = "we_sf"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 we_sf_a(TRYEAR-1900-2:endYr)

      serName = "ws_mef"
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 ws_mef_a(TRYEAR-1900-2:endYr)

      serName = "wsca"
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 wsca_a(TRYEAR-1900-2:endYr)

      serName = "wsdpb"
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 wsdpb_a(TRYEAR-1900-2:endYr)

      serName = "wspb_o"
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 wspb_o_a(TRYEAR-1900-2:endYr)

      serName = "wsw_mef"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 wsw_mef_a(TRYEAR-1900-2:endYr)

      serName = "wswa"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 wswa_a(TRYEAR-1900-2:endYr)

   end subroutine ModSolAToStoch   
   
!===============================================================================
   
   subroutine WriteOutputFileEV()
   
      integer :: n, len
      character :: freq
      real (kind = 8), dimension(MAX_YR) :: annualSeries
      real (kind = 8), dimension(MAX_QTR) :: quarterlySeries
            
      do n = 1, NUM_SERIES
         len = len_trim(seriesName(n))
         freq = ToUpper(seriesName(n)(len:len))
         if (freq == "A") then
            write(5003,'(a)') "pageselect a"
            write(5003,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(5003,'(a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", 1900+per(1,n),  ") _" 
            call MapSeries(trim(seriesName(n)), annualSeries)
            call WriteSeriesEV(annualSeries, per(1,n), per(2,n))
         else if (freq == "Q") then
            write(5003,'(a)') "pageselect q"
            write(5003,'(a)') "series " // trim(ToLower(seriesName(n)(1:len-2)))
            write(5003,'(a,i0,a,i0,a)') trim(ToLower(seriesName(n)(1:len-2))) // ".fill(o=", &
               1900+(per(1,n)/4),"q", mod(per(1,n),4)+1, ") _" 
            call MapSeries(trim(seriesName(n)), quarterlySeries)
            call WriteSeriesEV(quarterlySeries, per(1,n), per(2,n))
         end if
         write(5003,*)
      end do
      call CloseFile(5003)
   
   end subroutine WriteOutputFileEV

!===============================================================================
   
   subroutine WriteSeriesEV(series, i1, i2, fileNum)
   
      real (kind = 8), dimension(:) :: series
      integer :: i1, i2
      integer :: i, r, num
      integer, optional :: fileNum

      if (present(fileNum)) then
         num = fileNum
      else
         num = 5003
      end if
      
      do i = i1, i2, 4
         if (i + 4 <= i2) then
            write(num,'("  ",3(f29.13,","),f29.13,$)') series(i:i+3)
            write(num,'(a)') ", _"
            !write(num,'(a$)') "  "
         else
             r = i2 - i + 1
             if (r > 1) then 
                write(num,'("  ",' // IntToAsc(r-1) // '(f29.13,","),$)') series(i:i+r-2)         
                write(num,'(f29.13)') series(i+r-1)
             else
                write(num,'("  ",f29.13)') series(i+r-1)
             end if
         end if
         
      end do
   
   end subroutine WriteSeriesEV

!===============================================================================
   
   subroutine WriteProgramEV()
   
      ! Import Series
      write(5002,'(a)') "wfopen " // trim(OUT_PATH) // "\mul\" // trim(afilename)
      write(5002,'(a)') "exec " // trim(OUT_PATH) // "\internal\ModSolAOut.prg"
      write(5002,'(a)') "exec " // trim(OUT_PATH) // "\internal\CovOut.prg"
      write(5002,'(a)') "exec " // trim(OUT_PATH) // "\internal\HistLcAnnDataOut.prg"
      write(5002,'(a)') "wfsave(2) " // trim(OUT_PATH) // "\mul\" // trim(afilename)
      write(5002,'(a)') "wfclose"
      
      call CloseFile(5002)
   
   end subroutine WriteProgramEV

!===============================================================================
   
   subroutine RunProgramEV()
   
      integer :: rv3
   
      write(*,'(a$)') "Importing solution to file " // trim(afileName) // ".wf1"
      rv3 = ExecuteProgram(trim(OUT_PATH) // "\internal\ModSolAImport.prg")
      if (rv3 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
              
   end subroutine RunProgramEV

!===============================================================================   

end module EconModSolAOutMod