module EconRevEarnMod

   use ifport
   use EconParMod
   use EconLibMod
   use EconRevEarnVarMod
   use EconRevearnOutMod
   use WorkfileInterface
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconRevEarnMain

   integer :: firstYr = FIRST_AMALGAM_YEAR - 1900
   integer :: lastYr  = LAST_AMALGAM_YEAR - 1900
   integer :: middleYr = (LAST_AMALGAM_YEAR + FIRST_AMALGAM_YEAR) / 2 -1900
   integer :: numYears = LAST_AMALGAM_YEAR - FIRST_AMALGAM_YEAR + 1
   integer :: shortRangeYr = TRYR + 9
   integer :: lastSetYr = LAST_SELF_EMPLOYMENT_TAXABLE_YEAR - 1900
   real (kind = 8) :: trdiff = 0d0
   real (kind = 8) :: yearChange
   integer :: targetYr = TRYR + 9
   integer :: count
   integer :: i
   
contains

!===============================================================================

   subroutine EconRevEarnMain()

      integer :: n
      real (kind = 8) :: etp, cwse
      real (kind = 8) :: tw, cw
      integer :: targetIndex
      real (kind=8), dimension(MAX_YR) :: temp

      write(*,'(//a//)') "Solving RevEarn, Please Wait..."

      call InitializeRevearnVars()
      
! Call routine to get historical data for TR year minus 2.
      call GetTaxableData()

! Call routine to compute adjustments to the employee taxable wage ratio for
! changes in age-sex distribution of workers, business cycle, and trend effects.
      call computeWageRatioAdjustments()

      targetIndex = 2

      if (trim(FILE_EXTENSION) == "alt1") then
         targetIndex = 1
      else if (trim(FILE_EXTENSION) == "alt3") then
         targetIndex = 3
      else if (trim(FILE_EXTENSION) == "iro5") then
         targetIndex = 4
      end if
      
      if (targetIndex >= 1 .and. targetIndex <= 3) then ! target etp in a particular year

         if (TAXABLE_RATIO /= -1d0) TR_TARGET(targetIndex) = TAXABLE_RATIO

! Calculate OASDI employee taxable to covered wage ratios for Tr year minus 2 to
! last year of solution.
         call computeEmployeeRatio()

! Before adding adjustments to the OASDI taxable employee ratio, make sure that
! the base year error adjustment is zero in all array positions.
      addwstreeodbyerr = 0d0
! Call routine to compute OASDI taxable employee ratio as amalgam value plus
! adjustments for age-sex, business cycle, and trend for TR year minus one.
      call adjustEmployeeRatio(TRYR-1)

! Determine OASDI wages for TR year minus 1.
         if ( OASDI_TARGET == -1d0) then
! If no taxable target input, call routine to calculate taxable target based on
! amalgam.
            call CalculateOASDITaxableTarget()
            sample = (/ TRYR-1, TRYR-1 /)
            temp=0d0
            temp(TRYR-1) = wsterod(TRYR-1)
            call StoreSeries(WSQPROJTR, "WSULT941.A", temp)
            call CloseWorkfile(WSQPROJTR, .true.)
         else
! If taxable target input, store target in array and call routine to compute
! base year error adjustment amount. This also computes ratio of multi-employer
! refund wages to covered wages consistent with target.
            wsterod(TRYR-1) = OASDI_TARGET
            call baseYearErrorAdjustTaxableRatio()
! Compute levels of OASDI multi-employer refund wages, taxable employee wages,
! and employee taxable ratio for TR year minus 1.
            wsmerefod(TRYR-1) = wsrmerefod(TRYR-1) * wsca_a(TRYR-1)
            wsteeod(TRYR-1) = wsterod(TRYR-1) - wsmerefod(TRYR-1)
!            wstreeod(TRYR-1) = wsteeod(TRYR-1) / wsca_a(TRYR-1)
         end if

         do n = 1, 8
            etp = 0.5d0*(wsteeod(targetYr)+wsterod(targetYr))+seetod(targetYr)
            cwse =  wsca_a(targetYr) + cse_tot_a(targetYr)
            trdiff = trdiff + (TR_TARGET(targetIndex) /1d2 - etp / cwse) ! + 0.00004565d0 + 0.00000331d0 + 0.00000024d0 + 0.00000001d0

            do i = TRYR, TRYR + 9

               addtrtarget(i) = addtrtarget(i-1) + trdiff / 10d0
            end do
            addtrtarget(i:) = addtrtarget(i-1)

            call ComputeTaxablePayroll()

         end do
      
      else if (targetIndex == 4) then ! target an array of specified taxable wage ratios
         
         do n = 1, 8
            do i = TRYEAR - 1900, endYr
               tw = wsterod(i)
               cw =  wsca_a(i)
               trdiff = (TWR_TARGET(i) /1d2 - tw / cw)
               addtrtarget(i) = addtrtarget(i) + trdiff
            end do
            addtrtarget(i:) = addtrtarget(i-1)

            call ComputeTaxablePayroll()
         end do
         
      else
         call ComputeTaxablePayroll()
      end if
      
      ! Convert values from $ billions to $ millions for Revearn input file.
      do i = 37, endYr
          wsterod(i) = 1d3 * wsterod(i)
          wsmerefod(i) = 1d3 * wsmerefod(i)
          seetod(i) = 1d3 * seetod(i)
      end do
      
      do i = 37, 65
        wsteeod(i) = wsterod(i) - wsmerefod(i)
      end do
      
      do i = 66, 87
        wsteeod(i) = 1d3 * wsteeod(i)
      end do

      do i = 88, endYr
        wsteeod(i) = wsterod(i) - wsmerefod(i)
      end do

      call EconRevearnOutMain()
      call Finish()

   end subroutine EconRevEarnMain

!===============================================================================
   
   subroutine GetTaxableData()
! This routine gets historical taxable data from the A workfile and
! computes related values needed in subsequent calculations.
   
      real (kind = 8), dimension(MAX_YR) :: oasdise_ti_etp

      call FetchSeries(DFILE, "OASDI_TW_EE", wsteeod)     ! OASDI employer taxable wages
      call FetchSeries(DFILE, "OASDI_TW_ER", wsterod)     ! OASDI employer taxable wages
      call FetchSeries(DFILE, "OASDI_MERW", wsmerefod) ! OASDI multi-employer refund wages
      call FetchSeries(DFILE, "OASDISE_TI", seetod)    ! OASDI self-employment income
      call FetchSeries(DFILE, "OASDISE_TI_ETP", oasdise_ti_etp) ! OASDI self-employment income, 1951-83 about 75% of seetod
      seetod(51:83) = oasdise_ti_etp(51:83)
! Compute OASDI taxable employee wages as employer wages less refund wages.
!     wsteeod(TRYR-2) = wsterod(TRYR-2) - wsmerefod(TRYR-2)
! Compute ratios of employee taxable wages and refund wages to OASDI covered wages.
      wstreeod(TRYR-2) = wsteeod(TRYR-2) / wsca_a(TRYR-2)
      wsrmerefod(TRYR-2) = wsmerefod(TRYR-2) / wsca_a(TRYR-2)
   end subroutine GetTaxableData

!===============================================================================
   
   subroutine CalculateOASDITaxableTarget()
   
      ! Calculate OASDI taxable target for TR year minus 1.
      wsteeod(TRYR-1) = wstreeod(TRYR-1) * wsca_a(TRYR-1)
      ! Calculate ratio of multi-employer refund wages to covered wages for TR
      ! year less 1.
      wsrmerefod(TRYR-1) = GetRefundRatio(wsrmerefod(TRYR-2), wstreeod(TRYR-1), &
                                          wstreeod(TRYR-2), ru_a(0,0,TRYR-1), &
                                          ru_a(0,0,TRYR-2))
      ! Calculate multi-employer refund wages for TR year minus 1.
      wsmerefod(TRYR-1) = wsrmerefod(TRYR-1) * wsca_a(TRYR-1)
      ! Add refund wages to employer taxable wages to get employer taxable wages.
      wsterod(TRYR-1) = wsteeod(TRYR-1) + wsmerefod(TRYR-1)
      ! Round employer taxable wages to nearest $ million.
      wsterod(TRYR-1) = RoundCl(wsterod(TRYR-1), 3)
      ! Recompute employee taxable wages to be consistent with rounded employer
      ! value
      wsteeod(TRYR-1) = wsterod(TRYR-1) - wsmerefod(TRYR-1)

   end subroutine CalculateOASDITaxableTarget

!===============================================================================
   
   subroutine ComputeTaxablePayroll()


      do i = TRYR, endYr
         call adjustEmployeeRatio(i)
         wsteeod(i) = wstreeod(i) * wsca_a(i)
      end do
      ! Freeze employee ratio after short-range period
      do i = shortRangeYr+1, endYr
         if (trim(FILE_EXTENSION) /= "iro5") then
            wstreeod(i) = wstreeod(shortRangeYr)
         end if
         wsteeod(i) = wstreeod(i) * wsca_a(i)
      end do

      call computeRefundRatio()

      do i = TRYR, shortRangeYr
         wsmerefod(i) = wsrmerefod(i) * wsca_a(i)
      end do
      ! Freeze refund ratio after short-range period
      do i = shortRangeYr+1, endYr
        if (trim(FILE_EXTENSION) /= "iro5") then
           wsrmerefod(i) = wsrmerefod(shortRangeYr)
        end if
        wsmerefod(i) = wsrmerefod(i) * wsca_a(i)
      end do

      call computeEmployerRatio()

      ! Recompute base year employee as employer minus refund
      wsteeod(TRYR-1) = wsterod(TRYR-1) - wsmerefod(TRYR-1)

      do i = TRYR, endYr ! already frozen after short-range period
         wsterod(i) = wstrerod(i) * wsca_a(i)
      end do

      call computeSelfEmployedRatio()

      do i = TRYR-1, endYr
         seetod(i) = seetrod(i) * cse_tot_a(i)
      end do

   end subroutine ComputeTaxablePayroll

!===============================================================================

   subroutine computeEmployeeRatio()

      do i = TRYR-2, endYr
         wstreexrxadjodpg(i) = getTaxableRatio(taxmax_a(i), acwa_a(i))
      end do

   end subroutine computeEmployeeRatio

!===============================================================================

   subroutine computeWageRatioAdjustments()

! Set variable used for age-sex and business cycle adjustments.
      if (mod(numYears,2) == 1) then
         yearChange = 1.0d0     ! middleYear is an actual year
      else
         yearChange = 0.5d0     ! middleYear is between two years
      end if

      ! Age-sex
      call ageSexAdjustTaxableRatio()

      ! Business Cycle
      call busCycAdjustTaxableRatio()

      ! Trend
      call trendAdjustTaxableRatio()

   end subroutine computeWageRatioAdjustments

!===============================================================================

   subroutine adjustEmployeeRatio(year)

      integer :: year

      wstrxrpg(year) = wstreexrxadjodpg(year) + addwstreeodagesex(year) + &
                       addwstreeodbuscyc(year) + addwstreeodtrend(year)
      addwstreeodtrendtarget(year) = addwstreeodtrend(year) + addtrtarget(year)

      addwstreeod(year) = addwstreeodagesex(year) + &
                          addwstreeodbuscyc(year) + &
                          addwstreeodbyerr(year) + &
                          addwstreeodtrend(year) + &
                          addtrtarget(year)
      wstreeod(year) = wstreexrxadjodpg(year) + &
                       addwstreeod(year)

   end subroutine adjustEmployeeRatio

!===============================================================================

   subroutine ageSexAdjustTaxableRatio()

      integer :: ageGrp, sex

      ! Age groups are u16, 16-19, 20-24, ..., 65-69, 70o
      ! Base year is 1996
      ! Average covered wages by age and sex in base year
      real (kind = 8), dimension(2,13) :: awscod
      ! Taxable wage ratio be age and sex in base year
      real (kind = 8), dimension(2,13) :: wstreeod
      ! Covered employment from Modeem
      real (kind = 8), dimension(2,13,MAX_YR) :: ecwsod
      ! Covered and Taxable wage subtotals used for adjustment
      real (kind = 8), dimension(2,13) :: cw, tw
      ! Age-sex adjusted taxable wage ratio
      real (kind = 8), dimension(MAX_YR) :: twr
      real (kind = 8) :: twrbase

      awscod(2,:) = (/ 1088.55, 3158.39, 9031.53, 16540.3, 19811.15, 21426.49, &
         23405.05, 24392.57, 23610.87, 21202.06, 17810.55, 11669.81, 9474.53 /)

      awscod(1,:) = (/ 1232.11, 3680.38, 11354.72, 21244.43, 29153.31, 36119.39, &
         41898.11, 45874.92, 48901.63, 44803.65, 37066.18, 24985.17, 16938.17 /)

      wstreeod(2,:) = (/ 0.978396, 0.999931, 0.998891, 0.992929, 0.969213, &
         0.95002, 0.939153, 0.942122, 0.948539, 0.960661, 0.96232, 0.961614, 0.92998 /)

      wstreeod(1,:) = (/ 0.994066, 1.0, 0.996259, 0.96794, 0.916408, 0.845847, &
         0.786591, 0.757642, 0.727126, 0.735161, 0.727494, 0.680616, 0.722532 /)

      !call FetchSeries(AFILE,"WSWMU16_MEF_O.A",ecwsod(1,1,:))
      !call FetchSeries(AFILE,"WSWFU16_MEF_O.A",ecwsod(2,1,:))
      !call FetchSeries(AFILE,"WSWM1619_MEF_O.A",ecwsod(1,2,:))
      !call FetchSeries(AFILE,"WSWF1619_MEF_O.A",ecwsod(2,2,:))
      ecwsod(1,1,:) = cew_m(1,0,:) - cew_m(1,24,:) ! U16
      ecwsod(2,1,:) = cew_m(2,0,:) - cew_m(2,24,:) ! U16
      ecwsod(1,2,:) = cew_m(1,27,:) ! 16-19
      ecwsod(2,2,:) = cew_m(2,27,:) ! 16-19

      do sex = 1, 2
         do ageGrp = 3, 12
            ! call FetchSeries(AFILE,"WSW"//trim(sexLabel(sex))// &
            !   trim(ageGrpLabel(ageGrp))//"_MEF_O.A",ecwsod(sex,ageGrp,:))
            ecwsod(sex,ageGrp,:) = cew_m(sex,ageGrp,:)
         end do
      end do
      
      ! call FetchSeries(AFILE,"WSWM70O_MEF_O.A",ecwsod(1,13,:))
      ! call FetchSeries(AFILE,"WSWF70O_MEF_O.A",ecwsod(2,13,:))
      ecwsod(1,13,:) = cew_m(1,23,:) ! 70O
      ecwsod(2,13,:) = cew_m(2,23,:) ! 70O

      do i = middleYr, shortRangeYr
         do sex = 1, 2
            do ageGrp = 1, 13
               cw(sex,ageGrp) = awscod(sex,ageGrp) * ecwsod(sex,ageGrp,i)
               tw(sex,ageGrp) = wstreeod(sex,ageGrp) * cw(sex,ageGrp)
            end do
         end do
         twr(i) = sum(tw) / sum(cw)
      end do

      twrbase = yearChange * twr(middleYr) + (1d0 - yearChange) * twr(middleYr+1)

      do i = middleYr, shortRangeYr
         addwstreeodagesex(i) = twr(i) - twrbase
      end do
      addwstreeodagesex(i:) = addwstreeodagesex(i-1)

   end subroutine ageSexAdjustTaxableRatio

!===============================================================================

   subroutine busCycAdjustTaxableRatio()

      real (kind = 8) :: baseRTP

      baseRTP = sum(rtp_a(firstYr:lastYr)) / numYears
      do i =  middleYr + 1, shortRangeYr
         addwstreeodbuscyc(i) = -0.1564d0 * (rtp_a(i) - baseRTP)
      end do

      addwstreeodbuscyc(i:) = addwstreeodbuscyc(i-1)

   end subroutine busCycAdjustTaxableRatio


!===============================================================================

   subroutine trendAdjustTaxableRatio()

      i = middleYr + 1
      addwstreeodtrend(i) = addwstreeodtrend(i-1) - &
         0.00163d0 * multwstreeodtrend(i) * yearChange
      do i = middleYr + 2, shortRangeYr + 1
         addwstreeodtrend(i) = addwstreeodtrend(i-1) - &
            0.00163d0 * multwstreeodtrend(i) * 1d0
      end do
      addwstreeodtrend(i:) = addwstreeodtrend(i-1)

   end subroutine trendAdjustTaxableRatio

!===============================================================================

   subroutine baseYearErrorAdjustTaxableRatio()

      double precision :: wsrmeref
      double precision :: wstrxr
      double precision :: wstrxrpgdif
      double precision :: difwtrxr
      double precision :: factor

      wstrxrpgdif = wstreexrxadjodpg(TRYR-1) + addwstreeodagesex(TRYR-1) + &
                       addwstreeodbuscyc(TRYR-1) + addwstreeodtrend(TRYR-1)
      count = 0
      difwtrxr = 0
      do
         wsrmeref = GetRefundRatio(wsrmerefod(TRYR-2),wstrxrpgdif, &
                    wstreeod(TRYR-2), ru_a(0,0,TRYR-1), ru_a(0,0,TRYR-2))

! Compute OASDI taxable employee wages by subtracting computed refund wages from
! employer wages.
         wsteeod(TRYR-1) = wsterod(TRYR-1) - ( wsrmeref * wsca_a(TRYR-1) )
         ! Quasi-actual employee taxable wage ratio
         wstrxr = wsteeod(TRYR-1) / wsca_a(TRYR-1)
         
         count = count + 1

         if (abs(wstrxr-wstrxrpgdif) < 0.0000005 .or. count >= 50) exit
         !if (abs(wstrxr-wstrxrpgdif) < 0.0000005) exit

         difwtrxr = difwtrxr + wstrxr - wstrxrpgdif
         wstrxrpgdif = wstrxrpg(TRYR-1) + difwtrxr
      end do
      
      wsrmerefod(TRYR-1) = wsrmeref

      addwstreeodbyerr(TRYR-1) = difwtrxr
      ! Add base year error to previously computed taxable ratio and to total adjustment.
      wstreeod(TRYR-1) = wstreeod(TRYR-1) + addwstreeodbyerr(TRYR-1)
      addwstreeod(TRYR-1) = addwstreeod(TRYR-1) + addwstreeodbyerr(TRYR-1)

      ! Phase base year error out over 10 years
      factor = 1d0
      do i = TRYR, shortRangeYr-1
        factor = factor - 0.1d0
        addwstreeodbyerr(i) = factor * addwstreeodbyerr(TRYR-1)
      end do

   end subroutine baseYearErrorAdjustTaxableRatio

!===============================================================================

   subroutine computeRefundRatio()

      integer :: i
      do i = TRYR, endYr
         wsrmerefod(i) = getRefundRatio(wsrmerefod(i-1), wstreeod(i), &
                                        wstreeod(i-1),ru_a(0,0,i), &
                                        ru_a(0,0,i-1))
      end do

   end subroutine computeRefundRatio

!===============================================================================

   subroutine computeEmployerRatio()

      integer :: i

      do i = TRYR-1, endYr
         wstrerod(i) = (wstreeod(i) + wsrmerefod(i))
      end do

   end subroutine computeEmployerRatio

!===============================================================================

   function getTaxableRatio(taxmax, acwa) result (taxableRatio)
   ! This version is in the (reversed) form like Tony's spreadsheet and has
   ! all constants as double precision

      real (kind = 8) :: taxmax, acwa
      real (kind = 8) :: taxableRatio
      real (kind = 8) :: relmax

      relmax = taxmax / acwa

      if (relmax > 12.1735780592725d0) then
         taxableRatio = -(0.273245883726556d0 / 0.86d0) * relmax**(-0.86d0) + &
                          0.9987d0
      else if (relmax > 2.37096753676532d0) then
         taxableRatio = -(0.0117856970005267d0 / 0.15d0) * exp(-0.15d0 * relmax) - &
                         (0.0511846803376859d0 / 0.40d0) * exp(-0.40d0 * relmax) - &
                         (0.830367654271823d0 / 1.25d0) * exp(-1.25d0 * relmax) + &
                          0.000166660118101223d0 * relmax + 0.9689d0
      else if (relmax > 1.52779089050241d0) then
         taxableRatio = -(0.107457676694312d0 / 0.30d0) * exp(-0.30d0 * relmax) - &
                         (1.24726153506163d0 / 1.55d0) * exp(-1.55d0 * relmax) - &
                          0.0132984557884609d0 * relmax + 1.0582d0
      else if (relmax > 0.725920773954031d0) then
         taxableRatio =  -(0.145790565272216d0 / 0.25d0) * exp(-0.25d0 * relmax) - &
                         (1.06355416951773d0 / 1.3d0) * exp(-1.3d0 * relmax) - &
                          0.0744675074545115d0 * relmax + 1.3601d0
      else if (relmax > 0.242287838436218d0) then
         taxableRatio =  (0.341827369708108d0 / 1.5d00) * relmax**1.50d0 - &
                         (1.76932778063926d0 / 0.69d0) * exp(-0.69d0 * relmax) - &
                          0.901294429842495d0 * relmax + 2.5685d0
      else
         taxableRatio =  relmax - &
                         (0.284264263729929d0 / 1.5d0) * relmax**1.5d0 - &
                         (0.404939275941294d0 / 2.00d0) * relmax**2d0
      end if

   end function getTaxableRatio

!===============================================================================

   function getRefundRatio(refundRatioPY, wageRatioCY, wageRatioPY, ruCY, ruPY) &
            result (refundRatio)

      real (kind = 8) :: refundRatio
      real (kind = 8) :: refundRatioPY
      real (kind = 8) :: ruCY
      real (kind = 8) :: ruPY
      real (kind = 8) :: wageRatioCY
      real (kind = 8) :: wageRatioPY

   ! Computes ratio of refund wages to covered wages.
         refundRatio = refundRatioPY - 0.03217 * (wageRatioCY-wageRatioPY) - &
                       0.00024 * (ruCY - ruPY)

   end function getRefundRatio

!===============================================================================

   subroutine computeSelfEmployedRatio()

      real (kind = 8), dimension(MAX_YR) :: otr
      real (kind = 8), dimension(MAX_YR) :: cwtr
      real (kind = 8), dimension(MAX_YR) :: cttr
      integer :: i

      do i = TRYR-2, endYr
         otr(i) = getTaxableRatioSelfEmployedOnly(taxmax_a(i), acse_seo_a(i))
      end do

      do i = TRYR-2, endYr
         cwtr(i) = getTaxableRatioCombinationWorkerWages(taxmax_a(i), aw_cmbtot_a(i))
      end do

      do i = TRYR-2, endYr
         cttr(i) = getTaxableRatioCombinationWorkerTotal(taxmax_a(i), (aw_cmbtot_a(i) + acse_cmb_tot_a(i)))
      end do
      do i = TRYR-2, endYr
         seetrod(i) = (otr(i)  * cse_seo_a(i) + &
                      cttr(i) * (w_cmbtot_a(i) + cse_cmb_tot_a(i)) - &
                      cwtr(i) * w_cmbtot_a(i)) / &
                      cse_tot_a(i)
         seetodseo(i) = otr(i)  * cse_seo_a(i)
         tetodcmb(i) = cttr(i) * (w_cmbtot_a(i) + cse_cmb_tot_a(i))
         seetodcmb(i) = tetodcmb(i) - cwtr(i) * w_cmbtot_a(i)
      end do

      call adjustSelfEmployedRatio()

   end subroutine computeSelfEmployedRatio

!===============================================================================

   subroutine adjustSelfEmployedRatio()

      integer :: i
      real (kind = 8) :: dsetrxrm
      real (kind = 8) :: relmaxeff
      real (kind = 8), dimension(MAX_YR) :: relmax
      real (kind = 8), dimension(MAX_YR) :: wstrxrm
      real (kind = 8) :: rwstrxrm2by
      real (kind = 8) :: rsetrxrm2by
      real (kind = 8) :: setrby

      setrxrm(TRYR-2) = seetod(TRYR-2) / cse_tot_a(TRYR-2)
      addsetrod(TRYR-2) = setrxrm(TRYR-2) - seetrod(TRYR-2)
      relmax(TRYR-2) = taxmax_a(TRYR-2) / acwa_a(TRYR-2)
      wstrxrm(TRYR-2) = wstreeod(TRYR-2)
      setrby = setrxrm(TRYR-2)
      ! This should eventually be read from file adjsettrtrYY2.dat
      ! This file has not been used for quite some time (> 5 years)
      add2addsetrod(TRYR-1) = 0d0
      do i = TRYR-1, endYr
         relmax(i) = taxmax_a(i) / acwa_a(i)
         relmaxeff = (wstreexrxadjodpg(i) / wstreexrxadjodpg(lastSetYr) - 1d0) / &
            (relmax(i) / relmax(lastSetYr) - 1d0)
         wstrxrm(i) = wstreeod(i) - (relmaxeff * (relmax(i) / relmax(lastSetYr) - 1d0))
         rwstrxrm2by = wstrxrm(i) / wstrxrm(lastSetYr)
         rsetrxrm2by = 0.3 * (rwstrxrm2by - 1d0) + 1d0
         setrxrm(i) = rsetrxrm2by * setrby
         dsetrxrm = setrxrm(i) - setrxrm(i-1)
         if (i <= shortRangeYr) then
            addsetrod(i) = addsetrod(i-1) + dsetrxrm + add2addsetrod(i)
         else
            addsetrod(i) = addsetrod(shortRangeYr)
         end if
         seetrod(i) = seetrod(i) + addsetrod(i)
      end do
      
      if (trim(FILE_EXTENSION) == "iro5") then
         do i = TRYR-1, endYr
            addsetrod(i) = (TSE_TARGET(i) / 1d2 - seetrod(i))
            seetrod(i) = seetrod(i) + addsetrod(i)
         end do
      end if

   end subroutine adjustSelfEmployedRatio

!===============================================================================

   function getTaxableRatioSelfEmployedOnly(taxmax, aseseo) result (taxableRatio)

      real (kind = 8) :: taxmax, aseseo, taxableRatio
      real (kind = 8) :: baseo = 38188.22d0
      real (kind = 8) :: relmax, o, otr

      relmax = 1d3 * taxmax / (aseseo * baseo / acse_seo_a(117))

      o = relmax
      
      ! write(9991,'(f12.6)') o

      if ( o < 0.026186088d0 ) then
         otr = o - (9.574544d0 / 2.5d0) * o**2.5d0
      else if ( o < 0.104744354d0 ) then
         otr = -(1.04227d0 / 1.5d0) * o**1.5d0 + (0.67587d0 / 2.15d0) * o**2.15d0 - &
            (3.471502d0 / 3.3d0) * o**3.3d0 + 1.110432d0 * o - 0.00049085d0
      else if ( o < 0.209488707d0 ) then
         otr = (2.185075d0 / 1.3d0) * o**1.3d0 - (4.972755d0 / 1.75d0) * o**1.75d0 + &
            (3.184795d0 / 2.4d0) * o**2.4d0 + 0.475027d0 * o + 0.00377668d0
      else if ( o < 0.471349592d0 ) then
         otr = (7.17629d0 /1.25d0) * o**1.25d0 - (0.28816d0 / 1.6d0) * o**1.6d0 - &
            (4.650629d0 / 2.4d0) * dexp(-2.4d0 * o) - 6.893912d0 * o + 1.987803109d0
      else if ( o < 1.021257449d0 ) then
         otr = -(0.52704d0 / 0.85d0) * dexp(-0.85d0 * o) + (0.369144d0 / 1.55d0) * &
            dexp(-1.55d0 * o) - (0.845421d0 / 3.1d0) * dexp(-3.1d0 * o) - &
            0.001447d0 * o + 0.665888025d0
      else if ( o < 1.309304422d0 ) then
         otr = -(0.174971d0 / 0.55d0) * dexp(-0.55 * o) - (0.393885d0 / 1.75d0) * &
            dexp(-1.75d0 * o) - (0.003202d0 / 2.6d0) * dexp(-2.6d0 * o) + &
            0.013703d0 * o + 0.64673395d0
      else if ( o < 1.728281837d0 ) then
         otr = (0.338456d0 / 0.3d0) * dexp(-0.3d0 * o) - (0.904979d0 / 1.15d0) * &
            dexp(-1.15d0 * o) + (0.51182d0 / 2.5d0) * dexp(-2.5d0 * o) + &
            0.185928d0 * o - 0.351278731d0
      else if ( o < 2.17344534d0 ) then
         otr = (0.358360d0 / 0.55d0) * dexp(-0.55d0 * o) - (5.475731d0 / 1.85d0) * &
            dexp(-1.85d0 * o) + (9.368262d0 / 2.6d0) * dexp(-2.6d0 * o) + &
            0.121142d0 * o + 0.156160373d0
      else if ( o < 3.142330612d0 ) then
         otr = -(0.067751d0 / 0.2d0) * dexp(-0.2d0 * o) - (2.812835d0 / 1.9d0) * &
            dexp(-1.9d0 * o) + (8.107091d0 / 2.75d0) * dexp(-2.75d0 * o) + &
            0.00956d0 * o + 0.79107633d0
      else if ( o < 52.372176875d0 ) then
         otr = -(0.024629d0 / 0.1d0) * dexp(-0.1d0 * o) - (0.11274d0 / 0.4d0) * &
            dexp(-0.4d0 * o) + 0.001128d0 * o + 0.893694435d0
      else if ( o < 1000d0 ) then
         otr = -(1.22364d0 / 0.85d0) * o**(-0.85d0) + 1.001234831d0
      else
         otr = 1d0
      end if

      taxableRatio = otr
   end function getTaxableRatioSelfEmployedOnly

!===============================================================================

   function getTaxableRatioCombinationWorkerWages(taxmax, awscmb) result (taxableRatio)

      real (kind = 8) :: taxmax, awscmb, taxableRatio
      real (kind = 8) :: basecw = 59157.14d0
      real (kind = 8) :: relmax, cw, cwtr

      relmax = 1d3 * taxmax / (awscmb * basecw / aw_cmbtot_a(117))

      cw = relmax
      
      ! write(9992,'(f12.6)') cw

      if ( cw < 0.0190413d0 ) then
         cwtr = cw - (1.269097d0 /1.72d0) * cw**1.72d0
      else if ( cw < 0.059164455d0 ) then
         cwtr = (0.733496d0 / 1.45d0) * cw**1.45d0 - (1.932995d0 / 1.6d0) * cw**1.6d0 + &
            0.982734d0 * cw + 0.00003324d0
      else if ( cw < 0.160589235d0 ) then
         cwtr = -(1.777645d0 / 1.8d0) * cw**1.8d0 + (1.737345d0 / 2.65d0) * cw**2.65d0 + &
            1.002541d0 * cw - 0.00013743d0
      else if ( cw < 0.236657820d0 ) then
         cwtr = -(1.264913d0 / 1.3d0) * cw**1.3d0 + (0.225457d0 / 2.2d0) * cw**2.2d0 + &
            1.381608d0 * cw - 0.00414044d0
      else if ( cw < 0.388794990d0 ) then
         cwtr = -(1.257299d0 / 1.2d0) * cw**1.2d0 - (0.286311d0 / 6.5d0) * cw**6.5d0 + &
            1.542586d0 * cw - 0.00150659d0
      else if ( cw < 0.540932161d0 ) then
         cwtr = -(1.607077d0 / 1.2d0) * cw**1.2d0 - (1.475696d0 / 1.55d0) * &
            dexp(-1.55d0 * cw) - 1.637803d0 * cw + 0.98777752d0
      else if ( cw < 0.862110631d0 ) then
         cwtr = -(0.687757d0 / 0.35d0) * dexp(-0.35d0 * cw) - (0.424043d0 / 1.35d0) * &
            dexp(-1.35d0 * cw) - 0.351856d0 * cw + 2.29858919d0
      else if ( cw < 1.081864321d0 ) then
         cwtr  = -(0.51421d0 / 0.4d0) * dexp(-0.4d0 * cw) - (0.877948d0 / 2.9d0) * &
            dexp(-2.9d0 * cw) - 0.147171d0 * cw + 1.50626904d0
      else if ( cw < 1.572084092d0 ) then
         cwtr  = -(4.289368d0 / 0.25d0) * dexp(-0.25d0 * cw) + (4.153888d0 / 0.55d0) * &
            dexp(-0.55d0 * cw) - (2.186503d0 / 1.3d0) * dexp(-1.3d0 * cw) - &
            1.292888d0 * cw + 11.23676668d0
      else if ( cw < 3.211784704d0 ) then
         cwtr  = -(0.102092d0 / 0.3d0) * dexp(-0.3d0 * cw) - (0.647197d0 / 1.4d0) * &
            dexp(-1.4d0 * cw) + 0.00085d0 * cw + 0.84811899d0
      else if ( cw < 16.904130019d0 ) then
         cwtr  = -(0.028188d0 / 0.15d0) * dexp(-0.15d0 * cw) - (0.221879d0 / 0.63d0) * &
            dexp(-0.63d0 * cw) + 0.000527d0 * cw + 0.8767982d0
      else if ( cw < 1d3) then
         cwtr = -(0.088498d0 / 0.254d0) * cw**(-0.254d0) + 1.0407112d0
      else
         cwtr = 1d0
      end if

      taxableRatio = cwtr

   end function getTaxableRatioCombinationWorkerWages

!===============================================================================

   function getTaxableRatioCombinationWorkerTotal(taxmax, atecmb) result (taxableRatio)

      real (kind = 8) :: taxmax, atecmb, taxableRatio
      real (kind = 8) :: basect = 77074.88d0
      real (kind = 8) :: relmax, ct, cttr

      relmax = 1d3 * taxmax / (atecmb * basect / (aw_cmbtot_a(117) + acse_cmb_tot_a(117)))

      ct = relmax
      
      ! write(9993,'(f12.6)') ct

      if ( ct < 0.025948791d0 ) then
         cttr = ct - (40.88231d0 / 3.3d0) * ct**3.3d0
      else if ( ct < 0.051897583d0 ) then
         cttr = -(0.015962d0 / 1.4d0) * ct**1.4d0 - (2.849709d0 / 2.5d0) * ct**2.5d0 + &
               1.005757d0 * ct - 0.00002946d0
      else if ( ct < 0.097307968d0 ) then
         cttr = -(1.740332d0 / 2.25d0) * ct**2.25d0 - (4.39659d0 / 4.3d0) * ct**4.3d0 + &
               1.010732d0 * ct - 0.00017087d0
      else if ( ct < 0.233539123d0 ) then
         cttr = -(3.007061d0 / 2.35d0) * ct**2.35d0 + (29.74953d0 / 5.5d0) * ct**5.5d0 + &
               1.047821d0 * ct - 0.00257006d0
      else if ( ct < 0.376257476d0 ) then
         cttr = -(17.9483d0 / 1.85d0) * ct**1.85d0 + (56.48945d0 / 0.34d0) * dexp(-0.34d0 * ct) + &
               58.06407d0 * ct - 166.16320626d0
      else if ( ct < 0.518975828d0 ) then
         cttr = (0.35103d0 / 0.65d0) * dexp(-0.65d0 * ct) - (1.220332d0 / 1.25d0) * &
               dexp(-1.25d0 * ct) + 0.050683d0 * ct + 0.455703736d0
      else if ( ct < 0.700617368d0 ) then
         cttr = -(0.158062d0 / 0.45d0) * dexp(-0.45d0 * ct) - (0.865629d0 / 1.25d0) * &
               dexp(-1.25d0 * ct) - 0.139568d0 * ct + 1.069620633d0
      else if ( ct < 0.869284513d0 ) then
         cttr = -(10.956926d0 / 0.45d0) * dexp(-0.45d0 * ct) + (13.02327d0 / 1.35d0) * &
               dexp(-1.35d0 * ct) - (8.498083d0 / 2.45d0) * dexp(-2.45d0 * ct) - &
               4.127007d0 * ct + 17.95991367d0
      else if ( ct < 1.076874844d0 ) then
         cttr = -(2.015557d0 / 0.35d0) * dexp(-0.35d0 * ct) + (3.017185d0 / 1.2d0) * &
               dexp(-1.2d0 * ct) - (3.225627d0 / 2.0d0) * dexp(-2.0d0 * ct) - &
               0.725524d0 * ct + 4.75386607d0
      else if ( ct < 7.784637426d0 ) then
         cttr = -(0.100861d0 / 0.35d0) * dexp(-0.35d0 * ct) - (0.166637d0 / 1d0) * &
               dexp(-1d0 * ct) - (0.732639d0 /2.15d0) * dexp(-2.15d0 * ct) + &
               0.004175d0 * ct + 0.809186759d0
      else if ( ct < 25.948791419d0 ) then
         cttr = -(0.036719d0 / 0.18d0) * dexp(-0.18d0 * ct) - (0.780182d0 / 0.9d0) * &
               dexp(-0.9d0 * ct) + 0.001067d0 * ct + 0.865444162d0
      else if ( ct < 1d3 ) then
         cttr = -(0.142018d0 / 0.34d0) * ct**(-0.34d0) + 1.029280103d0
      else
         cttr = 1d0
      end if

      taxableRatio = cttr

   end function getTaxableRatioCombinationWorkerTotal

!===============================================================================

   subroutine Finish()

      write(*,'(/a/)') "RevEarn procedure finished"

   end subroutine Finish

!===============================================================================

end module EconRevEarnMod