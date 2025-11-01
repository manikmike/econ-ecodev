module EconTrusteesReportTablesMod

   use EconParMod
   use EconLibMod
   use EconRevearnVarMod
   include "OcactFortLib.inc"
   implicit none

   private
   public :: EconTrusteesReportTablesMain
   
   ! Both tables
   integer, parameter :: FIRST_DATA_YEAR = 1959
   integer, parameter :: SECOND_DATA_YEAR = 1970
   integer, parameter :: THIRD_DATA_YEAR = 1980
   integer, parameter :: DEC5_YEAR = 2007
   integer, parameter :: PROJ_END_YEAR = TRYEAR + 74
   
   character (len = 32), parameter :: ALT_NAME(0:3) = (/ "Historical data:", &
      "Low Cost:", "Intermediate:", "High Cost:" /)      
   
   ! Table V.B1
   integer, parameter :: HIST_END_YR = TRYEAR - 1900 - 2
   integer, parameter :: PROJ_START_YR = TRYEAR - 1900 - 1
   integer, parameter :: PROJ_END_YR = PROJ_END_YEAR - 1900

   ! Table V.B2
   integer, parameter :: LAST_NOM_INT_RATE_DATA_YEAR = TRYEAR - 1
   integer, parameter :: FIRST_NOM_INT_RATE_PROJ_YEAR = &
                                         LAST_NOM_INT_RATE_DATA_YEAR + 1
   integer, parameter :: SHORT_RANGE_END_YEAR = TRYEAR + 9
   integer, parameter :: LONG_RANGE_START_YEAR = &
                                                SHORT_RANGE_END_YEAR + 1
   integer, parameter :: LONG_RANGE_END_YEAR = TRYEAR + 74
   integer, parameter :: LAST_YEAR = 2105
   integer, parameter :: LAST_YR = LAST_YEAR - 1900
   
   integer :: block
   integer, parameter :: NUMBLOCKS = 8 + 7 ! last 7 business cycles
   !integer, parameter :: NUMITEMS = 7
      
   integer, dimension(NUMBLOCKS), parameter :: STARTYEAR1 = &
      (/ 1960, &
         1969, 1973, 1979, 1990, 2001, 2007, 2019, &
         TRYEAR-11, &
         TRYEAR, TRYEAR+9, &
         TRYEAR, TRYEAR+9, &
         TRYEAR, TRYEAR+9 /)
                          
   integer, dimension(NUMBLOCKS), parameter :: ENDYEAR1 = &
      (/ 2015, &
         1973, 1979, 1990, 2001, 2007, 2019, TRYEAR-1, &
         TRYEAR-1, &
         TRYEAR+9, PROJ_END_YEAR, &
         TRYEAR+9, PROJ_END_YEAR, &
         TRYEAR+9, PROJ_END_YEAR /)
   
   integer, dimension(NUMBLOCKS), parameter :: STARTYEAR2 = &
      (/ 1960, &
         1969, 1973, 1979, 1990, 2001, 2007, 2019, &
         TRYEAR-11, &
         TRYEAR, 5*int((TRYEAR+9)/5)+5, &
         TRYEAR, 5*int((TRYEAR+9)/5)+5, &
         TRYEAR, 5*int((TRYEAR+9)/5)+5 /)
                          
   integer, dimension(NUMBLOCKS), parameter :: ENDYEAR2 = &
      (/ 2015, &
         1973, 1979, 1990, 2001, 2007, 2019, TRYEAR-1, &
         TRYEAR-1, &
         TRYEAR+9, LAST_YEAR, &
         TRYEAR+9, LAST_YEAR, &
         TRYEAR+9, LAST_YEAR /)
   
   ! Update element 8 each year for last incomplete cycle
   integer, dimension(NUMBLOCKS), parameter :: STEP = &
      (/ 5, 4, 6, 11, 11, 6, 12, 5, 1, 1, 65, 1, 65, 1, 65 /)
      
   ! Annual Series
   !(Year - 1900)
   ! Need to map these variables names to main model
   !real (kind = 8), dimension(1:199) :: acw, acwa, ahrs, ahw, cpiw_u, &
   !   cpiwudec3, gdpreal, pgdp, prod, propearntocomp, tothrs, wsca, wsd, &
   !   wss, wswa, yf, ynf
   real (kind = 8), dimension(MAX_YR) :: propearntocomp, cpiwudec3, tothrs, acw, nir, niru, nirult, e_avg
   real (kind = 8), dimension(MAX_MTH) :: srr
   real (kind = 8), dimension(MAX_QTR) :: niru_q
   integer :: alt 

contains

!===============================================================================

   subroutine EconTrusteesReportTablesMain()
   
      if (trim(FILE_EXTENSION) /= "alt1" .and. &
          trim(FILE_EXTENSION) /= "alt2" .and. &
          trim(FILE_EXTENSION) /= "alt3") return
      
      alt = AscToInt(FILE_EXTENSION(4:4))
      
      call InitTrusteesReportTables()
      
      if (trim(FILE_EXTENSION) == "alt2") call makeHistTable()
      
      call makeAltTable()
   
   end subroutine EconTrusteesReportTablesMain
   
!===============================================================================
   
   subroutine InitTrusteesReportTables()
   
      integer :: year
   
      ! Productivity (Real GDP / Total Hours)
      call FetchSeries(AFILE, "HRS.A", tothrs)
      
      ! Earnings as a percent of compensation
      do year = FIRST_DATA_YEAR - 1900, HIST_END_YR
         propearntocomp = (wsd_a + yf_a + ynf_a) / (wss_a + yf_a + ynf_a)
      end do      
      
      ! Average covered wage
      ! 1959 - 1970
      call FetchSeries(DFILE, "WSCA.A", wsca_a)
      call FetchSeries(DFILE, "WSWA.A", wswa_a)      
      do year = FIRST_DATA_YEAR - 1900, SECOND_DATA_YEAR - 1900
        acwa_a(year) = wsca_a(year) / wswa_a(year)
        acw(year) = 1d3 * acwa_a(year)
      end do
      do year = SECOND_DATA_YEAR - 1900 + 1, HIST_END_YR
        acw(year) = nint(1d6 * acwa_a(year),8) / 1d3 ! Specify INTEGER(8) result for nint
      end do
      
      ! Consumer price index      
      do year = FIRST_DATA_YEAR - 1900, HIST_END_YR
         if (year < DEC5_YEAR - 1900) then
            cpiwudec3(year) = nint(1000. * cpiw_u_a(year)) / 1000.
         else ! (year >= DEC5_YEAR)
            ! This section rounds to 5 decimal places
            cpiwudec3(year) = nint(100000. * cpiw_u_a(year)) / 100000.
         end if
      end do

      ! http://www.bls.gov Series Id:    CWUR0000SA0, annual average column
      cpiwudec3(13:HIST_END_YR) = (/ 10.0d0, 10.1d0, 10.2d0, 11.0d0, 12.9d0, 15.1d0, &
         17.4d0, 20.1d0, 18.0d0, 16.9d0, 17.2d0, 17.2d0, 17.6d0, 17.8d0, &
         17.5d0, 17.2d0, 17.2d0, 16.8d0, 15.3d0, 13.7d0, 13.0d0, 13.5d0, &
         13.8d0, 13.9d0, 14.4d0, 14.2d0, 14.0d0, 14.1d0, 14.8d0, 16.4d0, &
         17.4d0, 17.7d0, 18.1d0, 19.6d0, 22.5d0, 24.2d0, 24.0d0, 24.2d0, &
         26.1d0, 26.7d0, 26.9d0, 27.0d0, 26.9d0, 27.3d0, 28.3d0, 29.1d0, &
         29.3d0, 29.8d0, 30.1d0, 30.4d0, 30.8d0, 31.2d0, 31.7d0, 32.6d0, &
         33.6d0, 35.0d0, 36.9d0, 39.0d0, 40.7d0, 42.1d0, 44.7d0, 49.6d0, &
         54.1d0, 57.2d0, 60.9d0, 65.6d0, 73.1d0, 82.9d0, 91.4d0, 96.9d0, &
         99.8d0, 103.3d0, 106.9d0, 108.6d0, 112.5d0, 117.0d0, 122.6d0, &
         129.0d0, 134.3d0, 138.2d0, 142.1d0, 145.6d0, 149.8d0, 154.1d0, &
         157.6d0, 159.7d0, 163.2d0, 168.9d0, 173.5d0, 175.9d0, 179.8d0, &
         184.5d0, 191.0d0, 197.1d0, 202.767d0, 211.053d0, 209.630d0, &
         213.967d0, 221.575d0, 226.229d0, 229.324d0, 232.771d0, 231.810d0, &
         234.076d0, 239.051d0, 245.146d0, 249.222d0, 252.248d0, 265.510d0, &
         287.984d0,298.990d0 /)
      cpiwudec3(13:HIST_END_YR) = cpiwudec3(13:HIST_END_YR) / 100d0
      
      ! Year of incomplete data is not rounded
      cpiwudec3(HIST_END_YR+1) = cpiw_u_a(HIST_END_YR+1)
      
      ! collapse monthly series to annual by averaging
      call collapse(nir, nomintr)
      ! Household Employment - annual average of monthly values
      ! 1959  -1979
      call FetchSeries(BKDO1, "E.A", e_avg)
      e_avg(80:) = e_a(0,0,80:)
   
   end subroutine InitTrusteesReportTables
   
!===============================================================================
   
   subroutine makeHistTable()
   
      integer :: year
      integer :: n, i, j
      character (len = 64) :: prtFmt, yrLbl
      real (kind = 8) :: ruAvg, nirAvg, realIntAvg
      
      ! Table V.B1
      ! Historical table
      ! Raw data
      call writeLabel1(0)
      do year = FIRST_DATA_YEAR - 1900, HIST_END_YR
         write(9500,'(i8,f12.6,f10.6,f11.6,f11.6,f16.6,f11.6)') year+1900, &
            RoundCl(gdpreal_a(year) / tothrs(year),6), RoundCl(propearntocomp(year),6), &
            RoundCl(ahrs_a(year),6), RoundCl(pgdp_a(year),6), &
            RoundCl(acw(year),6), RoundCl(cpiwudec3(year),6)
      end do
      write(9500,'(2x,a)') 
      ! Printed copy for Trustees Report
      call writeHeader1(0)
      do block = 1, 9
         if (block == 2 .or. block == 9) write(9700,'(2x,a)') "  "
         do year = STARTYEAR1(block)-1900, ENDYEAR1(block)-1900, STEP(block)
            if (block >= 2 .and. block <= 8 .and. year == ENDYEAR1(block)-1900) cycle
            n = STEP(block)
            if (n /= 1) then
               i = year + STEP(block) ! use second year of range
               prtFmt = '(2x,a12,f15.2,f8.2,f11.2,f15.2,2f12.2,f11.2)'
               yrLbl = trim(IntToAsc(i-n+1900)) // " to " // &
                  trim(IntToAsc(i+1900))                 
            else
               i = year ! allow single year in table to match
               prtFmt = '(2x,a8,f19.2,f8.2,f11.2,f15.2,2f12.2,f11.2)'
               yrLbl = IntToAsc(i+1900)               
            end if
            !do j = year, year + n
            !   realWageDiff =                
            !end do
            write(9700,trim(prtFmt)) trim(yrLbl), RoundCl(carn(n, gdpreal_a / tothrs, i),2), &
               RoundCl(carn(n, pgdp_a, i),2), RoundCl(carn(n, ahrs_a, i),2), RoundCl(carn(n, propearntocomp, i),2), &
               RoundCl(carn(n, acwa_a, i),2), RoundCl(carn(n, acwa_a / cpiwudec3, i),2), RoundCl(carn(n, cpiwudec3, i),2)
         end do
      end do
      write(9700,'(2x,a)')
      
      ! Table V.B2
      ! Historical table
      ! Raw data
      call writeLabel2(0)
      do year = FIRST_DATA_YEAR - 1900, HIST_END_YR
         write(9510,'(i8,f11.6,f12.6,f13.6,f14.6,f11.6,f10.3)') 1900+year, &
            RoundCl(ru_a(0,0,year),6), RoundCl(lc_a(0,0,year),6), RoundCl(e_avg(year) + edmil_a(year),6), RoundCl(gdpreal_a(year),6), RoundCl(nir(year),6), &
            RoundCl(cpiwudec3(year),3)
      end do
      write(9510,'(a)') "-----------------------------------------------&
            &--------------------------------"
      ! Printed copy for Trustees Report
      call writeHeader2(0)
      do block = 1, 9
         if (block == 2 .or. block == 9) write(9710,'(2x,a)') "  "
         do year = STARTYEAR2(block)-1900, ENDYEAR2(block)-1900, STEP(block)
            if (block >= 2 .and. block <= 8 .and. year == ENDYEAR2(block)-1900) cycle
            n = STEP(block)
            if (n /= 1) then
               i = year + n   ! use second year of range
               ruAvg = sum(ru_a(0,0,i-n+1:i)) / dble(n)
               nirAvg = sum(nir(i-n+1:i)) / dble(n)
               realIntAvg = 1.0
               do j = 1, n
                  realIntAvg = &
                     realIntAvg * (1.0 + realIntRate(nir(i-n+j-1)/100., &
                     cpiwudec3(i-n+j)/cpiwudec3(i-n+j-1)-1.0))
               end do
               realIntAvg = 100. * (realIntAvg**(1.0/dble(n)) - 1.0)
               prtFmt = '(2x,a12,f11.1,f10.1,f12.1,3f10.1)'  
               yrLbl = trim(IntToAsc(i-n+1900)) // " to " // &
                  trim(IntToAsc(i+1900))             
            else
               i = year   ! allow single year in table to match
               ruAvg = ru_a(0,0,i)
               nirAvg =nir(i)
               realIntAvg = realIntRate(nir(i-1)/100., &
                  cpiwudec3(i)/cpiwudec3(i-1)-1.0)*100.
               prtFmt = '(2x,a8,f15.1,f10.1,f12.1,6f10.1)'
               yrLbl = IntToAsc(i+1900)
            end if
            write(9710,trim(prtFmt)) trim(yrLbl), RoundCl(ruAvg,1), &
               RoundCl(carn(n, lc_a(0,0,:), i),1), RoundCl(carn(n, (e_avg + edmil_a), min(i,LAST_YR)),1), RoundCl(carn(n, gdpreal_a, i),1), &
               RoundCL(nirAvg,1), RoundCl(realIntAvg,1)
         end do
      end do
      write(9710,'(2x,a)') "  "
     
      ! Table V.B1 (Single Year)
      ! Historical table
      ! Raw data
      call writeSingYearLabel1(0)
      do year = FIRST_DATA_YEAR - 1900, HIST_END_YR
         write(9520,'(i8,f12.6,f10.6,f11.6,f11.6,f16.6,f11.6)') year+1900, &
            RoundCl(gdpreal_a(year) / tothrs(year),6), RoundCl(propearntocomp(year),6), RoundCl(ahrs_a(year),6), RoundCl(pgdp_a(year),6), &
            RoundCl(acw(year),6), RoundCl(cpiwudec3(year),6)
      end do
      write(9520,'(a)') "--------------------------------------------------&
         &-----------------------------"
      ! Printed copy for Trustees Report
      call writeSingYearHeader1(0)
      do year = FIRST_DATA_YEAR - 1900 + 1, TRYEAR - 1900 - 1
         i = year ! allow single year in table to match
         prtFmt = '(a8,f19.2,f8.2,f11.2,f15.2,2f12.2,f11.2)'
         yrLbl = IntToAsc(i+1900)               
         write(9720,trim(prtFmt)) trim(yrLbl), RoundCl(carn(n, gdpreal_a / tothrs, i),2), &
            RoundCl(carn(n, pgdp_a, i),2), RoundCl(carn(n, ahrs_a, i),2), RoundCl(carn(n, propearntocomp, i),2), &
            RoundCl(carn(n, acwa_a, i),2), RoundCl(carn(n, acwa_a / cpiwudec3, i),2), RoundCl(carn(n, cpiwudec3, i),2)
      end do
         
      ! Table V.B2 (Single Year)
      ! Historical table
      ! Raw data
      call writeSingYearLabel2(0)
      do year = FIRST_DATA_YEAR - 1900, HIST_END_YR
         write(9530,'(i8,f11.6,f12.6,f13.6,f14.6,f11.6,f10.3)') 1900+year, &
            RoundCl(ru_a(0,0,year),6), RoundCl(lc_a(0,0,year),6), RoundCl(e_avg(year) + edmil_a(year),6), RoundCl(gdpreal_a(year),6), RoundCl(nir(year),6), &
            RoundCl(cpiwudec3(year),3)
      end do
      write(9530,'(a)') "-----------------------------------------------&
            &--------------------------------"
      ! Printed copy for Trustees Report
      call writeSingYearHeader2(0)
      do year = FIRST_DATA_YEAR - 1900 + 1, TRYEAR - 1900 - 1
         i = year   ! allow single year in table to match
         ruAvg = ru_a(0,0,i)
         nirAvg =nir(i)
         realIntAvg = realIntRate(nir(i-1)/100., &
            cpiwudec3(i)/cpiwudec3(i-1)-1.0)*100.
         prtFmt = '(a8,f15.1,f10.1,f12.1,6f10.1)'
         yrLbl = IntToAsc(i+1900)
         write(9730,trim(prtFmt)) trim(yrLbl), RoundCl(ruAvg,1), &
            RoundCl(carn(n, lc_a(0,0,:), i),1), RoundCl(carn(n, (e_avg + edmil_a), min(i,LAST_YR)),1), RoundCl(carn(n, gdpreal_a, i),1), &
            RoundCl(nint(10d0*nirAvg) / 10d0,1), RoundCl(realIntAvg,1)
      end do
      write(9730,'(a)') "--------------------------------------------------&
         &-------------------------"
      
   end subroutine makeHistTable
   
!===============================================================================
   
   subroutine makeAltTable()
   
      integer :: year
      integer :: n, i, j
      character (len = 64) :: prtFmt, yrLbl
      integer :: lastUnroundedYear
      real (kind = 8) :: nirtmp
      
      do year = HIST_END_YR + 1, LAST_YR
        acw(year) = nint(1d6 * acwa_a(year),8) / 1d3  ! Specify INTEGER(8) result for nint
      end do
      
      ! Table V.B1
      ! Raw data
      call writeLabel1(alt)
      do year = HIST_END_YR + 1, PROJ_END_YR
         write(9500+alt,'(i8,f12.6,f10.6,f11.6,f11.6,f16.6,f11.6)') year+1900,&
            RoundCl(gdpreal_a(year) / tothrs(year),6), RoundCl(propearntocomp(year),6), RoundCl(ahrs_a(year),6), RoundCl(pgdp_a(year),6), &
            acw(year), RoundCl(cpiw_u_a(year),6) ! Removed RoundCl on acw to prevent overflow
      end do
      write(9500+alt,'(2x,a)') 
      ! Printed copy for Trustees Report
      block = 10
      call writeHeader1(alt)
      do j = 1, 2
         if (j == 2) write(9700+alt,'(2x,a)') "  "
         do year = STARTYEAR1(block)-1900, ENDYEAR1(block)-1900, STEP(block)
            n = STEP(block)
            if (n /= 1) then ! blocks 11, 13, and 15 (65 year ranges)
               if (year == ENDYEAR1(block)-1900) exit
               i = year + STEP(block)   ! use second year of range
               prtFmt = '(2x,a12,f15.2,f8.2,f11.2,f15.2,2f12.2,f11.2)'
               yrLbl = trim(IntToAsc(i-n+1900)) // " to " // &
                  trim(IntToAsc(i+1900))
            else
               i = year   ! allow single year in table to match
               prtFmt = '(2x,a8,f19.2,f8.2,f11.2,f15.2,2f12.2,f11.2)'
               yrLbl = IntToAsc(i+1900)                
            end if
            write(9700+alt,trim(prtFmt)) trim(yrLbl), RoundCl(carn(n, gdpreal_a / tothrs, i),2), &
               RoundCl(carn(n, pgdp_a, i),2), RoundCl(carn(n, ahrs_a, i),2), RoundCl(carn(n, propearntocomp, i),2), &
               RoundCl(carn(n, acw, i),2), RoundCl(carn(n, acw / cpiw_u_a, i),2), RoundCl(carn(n, cpiw_u_a, i),2)
         end do
         block = block + 1
      end do
      write(9700+alt,'(2x,a)')
      
      ! Table V.B2
      ! Raw data
      call writeLabel2(alt)
      do year = HIST_END_YR + 1, LAST_YR
         write(9510+alt,'(2x,i8,f11.6,f11.6,f12.6,f12.6,f15.6,f10.6,f10.6)') 1900+year, RoundCl(ru_a(0,0,year),6),&
            RoundCl(ru_asa_a(0,0,year),6), RoundCl(lc_a(0,0,year),6), RoundCl(e_avg(year) + edmil_a(year),6), gdpreal_a(year), RoundCl(nir(year),6), &
            RoundCl(cpiw_u_a(year),6) ! Removed RoundCl on gdpreal_a to prevent overflow
      end do
      write(9510+alt,'(a)') "-----------------------------------------------&
            &------------------------------------------"
      ! Printed copy for Trustees Report
      block = 10
      call writeHeader2(alt)
      do j = 1, 2
         if (j == 2) write(9710+alt,'(2x,a)') "  "
         do year = STARTYEAR2(block)-1900, ENDYEAR2(block)-1900, min(STEP(block), 5)
            n = 1
            i = year
            ! Use ru for short-range period and ru_asa afterwards
            if (j == 1) then
               write(9710+alt,'(2x,i8,f15.1,f10.1,f12.1,6f10.1)') 1900+year, &
                  RoundCl(ru_a(0,0,i),1), RoundCl(carn(n, lc_a(0,0,:), i),1), RoundCl(carn(n, (e_avg + edmil_a), min(i,LAST_YR)),1), &
                  RoundCl(carn(n, gdpreal_a, i),1), RoundCl(nir(i),1), RoundCl(realIntRate(nir(i-1)/100., &
                  cpiw_u_a(i)/cpiw_u_a(i-1)-1d0)*100d0,1)
            else
               write(9710+alt,'(2x,i8,f15.1,f10.1,f12.1,6f10.1)') 1900+year, &
                  RoundCl(ru_asa_a(0,0,i),1), RoundCl(carn(n, lc_a(0,0,:), i),1), RoundCl(carn(n, (e_avg + edmil_a) , min(i,LAST_YR)),1), &
                  RoundCl(carn(n, gdpreal_a, i),1), RoundCl(nir(i),1), RoundCl(realIntRate(nir(i-1)/100., &
                  cpiw_u_a(i)/cpiw_u_a(i-1)-1d0)*100d0,1)
            end if
         end do
         block = block + 1
      end do      
      write(9710+alt,'(2x,a)') "  "
      
      ! Table V.B1 (Single Year)
      ! Raw data
      call writeSingYearLabel1(alt)
      do year = PROJ_START_YR, LAST_YR
         write(9520+alt,'(i8,f12.6,f10.6,f11.6,f11.6,f16.6,f11.6)') year+1900, &
            RoundCl(gdpreal_a(year) / tothrs(year),6), RoundCl(propearntocomp(year),6), RoundCl(ahrs_a(year),6), RoundCl(pgdp_a(year),6), &
            acw(year), RoundCl(cpiw_u_a(year),6) ! Removed RoundCl on acw to prevent overflow
      end do
      write(9520+alt,'(a)') "--------------------------------------------------&
         &-----------------------------"
      ! Printed copy for Trustees Report
      call writeSingYearHeader1(alt)
      do year = TRYEAR - 1900, LAST_YR
         i = year ! allow single year in table to match
         n = 1
         prtFmt = '(a8,f19.2,f8.2,f11.2,f15.2,2f12.2,f11.2)'
         yrLbl = IntToAsc(i+1900)               
         write(9720+alt,trim(prtFmt)) trim(yrLbl), RoundCl(carn(n, gdpreal_a / tothrs, i),2), &
            RoundCl(carn(n, pgdp_a, i),2), RoundCl(carn(n, ahrs_a, i),2), RoundCl(carn(n, propearntocomp, i),2), &
            RoundCl(carn(n, acwa_a, i),2), RoundCl(carn(n, acwa_a / cpiw_u_a, i),2), RoundCl(carn(n, cpiw_u_a, i),2)
      end do
         
      ! Table V.B2 (Single Year)
      ! Raw Data
      call writeSingYearLabel2(alt)
      do year =  HIST_END_YR + 1, LAST_YR
         write(9530+alt,'(i8,f11.6,f11.6,f12.6,f12.6,f15.6,f10.6,f10.6)') 1900+year, RoundCl(ru_a(0,0,year),6),&
            RoundCl(ru_asa_a(0,0,year),6), RoundCl(lc_a(0,0,year),6), RoundCl(e_avg(year) + edmil_a(year),6), gdpreal_a(year), RoundCl(nir(year),6), &
            RoundCl(cpiw_u_a(year),6) ! Removed RoundCl on gdpreal_a to prevent overflow
      end do
      write(9530+alt,'(a)') "-----------------------------------------------&
            &--------------------------------"
      ! Printed copy for Trustees Report
      call writeSingYearHeader2(alt)
      do year = TRYEAR - 1900, LAST_YR
         i = year   ! allow single year in table to match
            ! Use ru for short-range period and ru_asa afterwards
            if (year <= SHORT_RANGE_END_YEAR - 1900) then
               write(9730+alt,'(i8,f15.1,f10.1,f12.1,6f10.1)') 1900+year, &
                  RoundCl(ru_a(0,0,i),1), RoundCl(carn(n, lc_a(0,0,:), i),1), RoundCl(carn(n, (e_avg + edmil_a), min(i,LAST_YR)),1), &
                  RoundCl(carn(n, gdpreal_a, i),1), RoundCl(nir(i),1), RoundCl(realIntRate(nir(i-1)/100., &
                  cpiw_u_a(i)/cpiw_u_a(i-1)-1d0)*100d0,1)
            else
               write(9730+alt,'(i8,f15.1,f10.1,f12.1,6f10.1)') 1900+year, &
                  RoundCl(ru_asa_a(0,0,i),1), RoundCl(carn(n, lc_a(0,0,:), i),1), RoundCl(carn(n, (e_avg + edmil_a) , min(i,LAST_YR)),1), &
                  RoundCl(carn(n, gdpreal_a, i),1), RoundCl(nir(i),1), RoundCl(realIntRate(nir(i-1)/100., &
                  cpiw_u_a(i)/cpiw_u_a(i-1)-1d0)*100d0,1)
            end if
      end do
      write(9730+alt,'(a)') "--------------------------------------------------&
         &-------------------------"

   end subroutine makeAltTable
   
!===============================================================================
            
   real (kind = 8) function realIntRate(nomIntRate, infRate)
   
      real (kind = 8) :: nomIntRate, infRate
      real (kind = 8) :: nomYield
      
      nomYield = (1.0 + nomIntRate/2.0)**2 - 1.0
      realIntRate = ((1.0 + nomYield) / (1.0 + infRate)) - 1.0
   
   end function realIntRate

!===============================================================================
   
   subroutine writeLabel1(alt)
   
      integer :: alt
      write(9500+alt,'(2x,a)') "           Economic Data used for Trustees Report &
         &Table V.B1."
      write(9500+alt,'(2x,a)') !"--------------------------------------------------&
         !&-----------------------------"
      write(9500+alt,'(2x,a)') "                      Ratio of    Average         &
         &           Average   Consumer"
      write(9500+alt,'(2x,a)') "Calendar    Product-  Earnings      Hours        &
         &GDP         Covered      Price"
      write(9500+alt,'(2x,a)') "    Year       ivity   to Comp     Worked   &
         &Deflator            Wage      Index"
      write(9500+alt,'(2x,a)') !"--------------------------------------------------&
         !&-----------------------------"
      write(9500+alt,'(2x,a)') trim(ALT_NAME(alt))      
      
   end subroutine writeLabel1
   
!===============================================================================

   subroutine writeLabel2(alt)

      integer :: alt
      
      if (alt == 0) then
         write(9510+alt,'(a)') "            Economic Data used for Trustees &
            &Report Table V.B2."
         write(9510+alt,'(a)') "-----------------------------------------------&
            &--------------------------------"
         write(9510+alt,'(a)') "                       Civilian                &
            &               Nominal  Consumer"
         write(9510+alt,'(a)') "Calendar  Unemploy-       Labor        Total   &
            &       Real   Interest     Price"
         write(9510+alt,'(a)') "    Year  ment Rate       Force   Employment   &
            &        GDP       Rate     Index"
         write(9510+alt,'(a)') "-----------------------------------------------&
            &--------------------------------"
      else
         write(9510+alt,'(a)') "               Economic Data used for Trustees &
            &Report Table V.B2."
         write(9510+alt,'(a)') "-----------------------------------------------&
            &------------------------------------------"
         write(9510+alt,'(a)') "                       Age-sex"
         write(9510+alt,'(a)') "                      adjusted    Civilian     &
            &                         Nominal  Consumer"
         write(9510+alt,'(a)') "Calendar  Unemploy-  Unemploy-       Labor     &
            &  Total           Real  Interest     Price"
         write(9510+alt,'(a)') "    Year  ment Rate  ment Rate       Force  &
            &Employment            GDP      Rate     Index"
         write(9510+alt,'(a)') "-----------------------------------------------&
            &------------------------------------------"
      end if      
      write(9510+alt,'(a)') trim(ALT_NAME(alt))      

   end subroutine writeLabel2

!================================================================================

   subroutine writeHeader1(alt)
   
      integer :: alt

      write(9700+alt,'(2x,a)') "                        Table V.B1.--Principal &
         &Economic Assumptions"
      write(9700+alt,'(2x,a)') "  "
      write(9700+alt,'(2x,a)') "                                   Annual &
         &percentage change in--"
      write(9700+alt,'(2x,a)') "  "
      write(9700+alt,'(2x,a)') "                                       Average    Earnings as"
      write(9700+alt,'(2x,a)') "               Productivity     GDP      hours   a percent of     Average annual wage   Consumer"
      write(9700+alt,'(2x,a)') "                (Total U.S.   price     worked    total labor   in covered employment      Price"
      write(9700+alt,'(2x,a)') "Calendar Year      economy)   index   per week   compensation     Nominal        Real      Index"
      write(9700+alt,'(2x,a)') "  "
      write(9700+alt,'(2x,a)') trim(ALT_NAME(alt))
      
   end subroutine writeHeader1
   
!===============================================================================

   subroutine writeHeader2(alt)
   
      integer :: alt

      write(9710+alt,'(2x,a)') "              Table V.B2.--Additional Economic &
         &Factors"
      write(9710+alt,'(2x,a)') "                                                 &
         &            Average annual"
      write(9710+alt,'(2x,a)') "                      Annual percentage change &
         &in             interest rate"
      write(9710+alt,'(2x,a)') "  " !"              ------------------------------------&
         !&-----   -----------------"
      write(9710+alt,'(2x,a)') "                Average"
      write(9710+alt,'(2x,a)') "                 annual"
      write(9710+alt,'(2x,a)') "    Calendar  unemploy-     Labor       Total     &
         & Real"
      write(9710+alt,'(2x,a)') "        Year  ment rate     force  Employment     &
         &  GDP   Nominal      Real"
      write(9710+alt,'(2x,a)') "  " !"--------------------------------------------------&
         !&-------------------------"
      write(9710+alt,'(2x,a)') trim(ALT_NAME(alt))

   end subroutine writeHeader2
   
!===============================================================================
   
   subroutine writeSingYearLabel1(alt)
   
      integer :: alt
      write(9520+alt,'(a)') "           Economic Data used for Trustees Report &
         &Table V.B1."
      write(9520+alt,'(a)') "--------------------------------------------------&
         &-----------------------------"
      write(9520+alt,'(a)') "                      Ratio of    Average         &
         &           Average   Consumer"
      write(9520+alt,'(a)') "Calendar    Product-  Earnings      Hours        &
         &GDP         Covered      Price"
      write(9520+alt,'(a)') "    Year       ivity   to Comp     Worked   &
         &Deflator            Wage      Index"
      write(9520+alt,'(a)') "--------------------------------------------------&
         &-----------------------------"
      write(9520+alt,'(a)') trim(ALT_NAME(alt))      
      
   end subroutine writeSingYearLabel1
   
!===============================================================================

   subroutine writeSingYearLabel2(alt)

      integer :: alt
      
      if (alt == 0) then
         write(9530+alt,'(a)') "            Economic Data used for Trustees &
            &Report Table V.B2."
         write(9530+alt,'(a)') "-----------------------------------------------&
            &--------------------------------"
         write(9530+alt,'(a)') "                       Civilian                &
            &               Nominal  Consumer"
         write(9530+alt,'(a)') "Calendar  Unemploy-       Labor        Total   &
            &       Real   Interest     Price"
         write(9530+alt,'(a)') "    Year  ment Rate       Force   Employment   &
            &        GDP       Rate     Index"
         write(9530+alt,'(a)') "-----------------------------------------------&
            &--------------------------------"
      else
         write(9530+alt,'(a)') "               Economic Data used for Trustees &
            &Report Table V.B2."
         write(9530+alt,'(a)') "-----------------------------------------------&
            &------------------------------------------"
         write(9530+alt,'(a)') "                       Age-sex"
         write(9530+alt,'(a)') "                      adjusted    Civilian     &
            &                         Nominal  Consumer"
         write(9530+alt,'(a)') "Calendar  Unemploy-  Unemploy-       Labor     &
            &  Total           Real  Interest     Price"
         write(9530+alt,'(a)') "    Year  ment Rate  ment Rate       Force  &
            &Employment            GDP      Rate     Index"
         write(9530+alt,'(a)') "-----------------------------------------------&
            &------------------------------------------"
      end if      
      write(9530+alt,'(a)') trim(ALT_NAME(alt))      

   end subroutine writeSingYearLabel2
   
!===============================================================================   
   
   subroutine writeSingYearHeader1(alt)
   
      integer :: alt

      write(9720+alt,'(a)') "                        Table V.B1.--Principal &
         &Economic Assumptions"
      write(9720+alt,'(a)') "  "
      write(9720+alt,'(a)') "                                   Annual &
         &percentage change in--"
      write(9720+alt,'(a)') "  "
      write(9720+alt,'(a)') "                                       Average    Earnings as"   
      write(9720+alt,'(a)') "               Productivity     GDP      hours   a percent of     Average annual wage   Consumer"   
      write(9720+alt,'(a)') "                (Total U.S.   price     worked    total labor   in covered employment      Price"   
      write(9720+alt,'(a)') "Calendar Year      economy)   index   per week   compensation     Nominal        Real      Index"   
      write(9720+alt,'(a)') "  "
      write(9720+alt,'(a)') trim(ALT_NAME(alt))
      
   end subroutine writeSingYearHeader1   
   
!===============================================================================

   subroutine writeSingYearHeader2(alt)
   
      integer :: alt

      write(9730+alt,'(a)') "              Table V.B2.--Additional Economic &
         &Factors"
      write(9730+alt,'(a)') "              -----------------------------------&
         &-----       Average annual"
      write(9730+alt,'(a)') "                      Annual percentage change &
         &in--           interest rate"
      write(9730+alt,'(a)') "              ------------------------------------&
         &-----   -----------------"
      write(9730+alt,'(a)') "                Average"
      write(9730+alt,'(a)') "                 annual"
      write(9730+alt,'(a)') "    Calendar  unemploy-     Labor       Total     &
         & Real"
      write(9730+alt,'(a)') "        Year  ment rate     force  Employment     &
         &  GDP   Nominal      Real"
      write(9730+alt,'(a)') "--------------------------------------------------&
         &-------------------------"
      write(9730+alt,'(a)') trim(ALT_NAME(alt))

   end subroutine writeSingYearHeader2
   
!=============================================================================== 
   
end module EconTrusteesReportTablesMod