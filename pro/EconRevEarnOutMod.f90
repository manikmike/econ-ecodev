module EconRevEarnOutMod

   use EconRevearnVarMod
   use WorkfileInterface
   use ifport
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconRevEarnOutMain
   
contains

!===============================================================================

   subroutine EconRevEarnOutMain()
   
      call WriteTaxablePayroll()
      call WriteTaxableRatio()
      call WriteTaxableWageRatio()
      call WriteTaxableSERatio()
      call WriteAdjustmentFactors()
      call awi_mce()
      call ate()
      call wiaceb()
      call RevearnData()
      if (WORKFILE_EXPORT) then
         call WriteOutputFileEV()
         call RevEarnToStoch()
         call SortedToStoch()
         call WriteProgramEV()
         call RunProgramEV()
      end if

   end subroutine EconRevEarnOutMain

!===============================================================================

   subroutine WriteTaxablePayroll()

      integer :: i

      do i = 37, min(OUTPUT_LAST_YR, endYr)
         
         if (i >= OUTPUT_TRYR - 1) then
            write(6003,'(i4,5f16.3)') 1900+i, wsteeod(i), &
               wsterod(i), seetod(i), 1d3*dmwc_o_a(i), &
               0.5d0*(wsteeod(i)+wsterod(i))+seetod(i)+1d3*dmwc_o_a(i)
         else
            if (needHist) write(6013,'(i4,5f16.3)') 1900+i, wsteeod(i), &
               wsterod(i), seetod(i), 1d3*dmwc_o_a(i), &
               0.5d0*(wsteeod(i)+wsterod(i))+seetod(i)+1d3*dmwc_o_a(i)
         end if
         
      end do
         
      do i = OUTPUT_TRYR-1, min(max(199, OUTPUT_LAST_YR), endYr)

         write(6008,'(i4,5f16.3,a16,f16.3)') 1900+i, 1d3*wscahi_a(i), 1d3*wscahi_a(i), 1d3*cse_tot_a(i), &
            1d3*(0.5d0*(wscahi_a(i)+wscahi_a(i))+cse_tot_a(i)), 1d3*wsprrb_a(i), &
            "         0.000", 1d3*(0.5d0*(wscahi_a(i)+wscahi_a(i))+cse_tot_a(i) + wsprrb_a(i))

      end do
      
   end subroutine WriteTaxablePayroll

!===============================================================================

   subroutine WriteTaxableRatio()

      integer :: i
      real (kind = 8) :: etp, cwse

      do i = TRYR-1, min(OUTPUT_LAST_YR, endYr)
         etp = 0.5d0*(wsteeod(i)+wsterod(i))+seetod(i)
         cwse =  1d3 * (wsca_a(i) + cse_tot_a(i))
         write(6004,'(i8,2f16.3,f16.6)') 1900+i, etp, cwse, 1d2 * etp/cwse
      end do

   end subroutine WriteTaxableRatio

!===============================================================================
   
   subroutine WriteTaxableWageRatio()

      integer :: i
      real (kind = 8) :: tw, cw

      do i = TRYR-1, min(OUTPUT_LAST_YR, endYr)
         tw = wsterod(i)
         cw =  1d3 * wsca_a(i)
         write(6006,'(i8,2f16.3,f16.6)') 1900+i, tw, cw, 1d2 * tw/cw
      end do

   end subroutine WriteTaxableWageRatio

!===============================================================================

   subroutine WriteTaxableSERatio()

      integer :: i
      real (kind = 8) :: tse, cse

      do i = TRYR-1, min(OUTPUT_LAST_YR, endYr)
         tse = seetod(i)
         cse =   1d3 * cse_tot_a(i)
         write(6007,'(i8,2f16.3,f16.6)') 1900+i, tse, cse, 1d2 * tse/cse
      end do

   end subroutine WriteTaxableSERatio

!===============================================================================

   subroutine WriteAdjustmentFactors()

      integer :: i

      write(6005,'(a)') "                  tr-pg        age-sex        bus cyc          trend      base year"
      do i = TRYR-1, min(OUTPUT_LAST_YR, endYr)
         write(6005,'(i8,5f15.9)') 1900+i, wstreexrxadjodpg(i), addwstreeodagesex(i), addwstreeodbuscyc(i), &
            addwstreeodtrend(i) + addtrtarget(i), addwstreeodbyerr(i)
      end do

   end subroutine WriteAdjustmentFactors

!===============================================================================

   subroutine awi_mce()
   
      integer :: yr
      real (kind = 8), dimension (MAX_YR) :: mce
      real (kind = 8), dimension (MAX_YR) :: relmaxearn, ter, oasdi_tw, oasdise_ti
      real (kind = 8), dimension (MAX_YR) :: tcearn, tcwage
      real (kind = 8), dimension (MAX_YR) :: acea, rmceace
      real (kind = 8), dimension (MAX_YR) :: rmxeffwsr, rmxeffser, rmxeffter
      real (kind = 8), dimension (MAX_YR) :: terrmadj
      real (kind = 8) :: relmaxearnbase
      
      oasdi_tw = wsterod / 1d3
      oasdise_ti = seetod / 1d3
      ! call FetchSeries(DFILE, "OASDI_TW.A", oasdi_tw)
      ! call FetchSeries(DFILE, "OASDISE_TI.A", oasdise_ti)
      
      ! earnings relmax
      relmaxearn = taxmax_a / ((wsca_a + cse_tot_a) / tcea_a)
      relmaxearnbase = relmaxearn(TRYEAR - 1900)
      
      ! taxable earnings ratio
      ter = (oasdi_tw + oasdise_ti) / (wsca_a + cse_tot_a)
      
      ! total covered earnings and total covered wages
      tcearn = (wsca_a + cse_tot_a) * 1000d0
      tcwage = wsca_a * 1000d0
      
      ! average covered earnings
      acea = (wsca_a + cse_tot_a) / tcea_a * 1000d0
      
      ! relmax adjust the taxable earnings ratio
      rmxeffwsr = ((relmaxearn / relmaxearnbase - 1d0) * 100d0) * 0.0016d0;
      rmxeffser = ((relmaxearn / relmaxearnbase - 1d0) * 100d0) * 0.0028d0;
      rmxeffter = tcwage / tcearn * rmxeffwsr + (1d0 - tcwage / tcearn) * rmxeffser;
      terrmadj = ter /(1d0 + rmxeffter);

      ! calculate ratio of median covered earnings to average
      rmceace = 1.07434d0 * terrmadj - 0.25000d0
      mce = rmceace * acea

      ! From Annual Statistical Supplement (2023, Table 4.B6)
      mce(112) = 26097d0
      mce(113) = 26603d0
      mce(114) = 27422d0
      mce(115) = 28468d0
      mce(116) = 29120d0
      mce(117) = 30055d0
      mce(118) = 31322d0
      mce(119) = 32660d0
      mce(120) = 32848d0
      mce(121) = 35895d0
      
      do yr = OUTPUT_TRYR - 4, min(OUTPUT_LAST_YR, endYr)
         write(6101,'(i4,2f11.2)') 1900+yr, aiw_a(yr), mce(yr)
      end do
   
   end subroutine awi_mce

!===============================================================================

   subroutine ate()
   
      integer :: yr
      real (kind = 8), dimension(MAX_YR) :: oasdi_tw, oasdi_merw, oasdise_ti
      real (kind = 8), dimension(MAX_YR) :: we_sf
      real (kind = 8), dimension(MAX_YR) :: oasdi_te_ee, oasdi_te_ee_mef
      !real (kind = 8), dimension(MAX_YR) :: atxe, atxe_mef
      real (kind = 8), dimension(MAX_YR) :: temp
      
      oasdi_tw = wsterod / 1d3
      call FetchSeries(DFILE, "OASDI_TW.A", oasdi_tw)

      oasdi_merw = wsmerefod / 1d3
      call FetchSeries(DFILE, "OASDI_MERW.A", oasdi_merw)

      oasdise_ti = seetod / 1d3
      call FetchSeries(DFILE, "OASDISE_TI.A", oasdise_ti)

      !call FetchSeries(AFILE, "WE_SF.A", we_sf)
      we_sf = we_sf_a
      call FetchSeries(DFILE, "WE_SF.A", temp)
      we_sf(37:TRYR-3) = temp(37:TRYR-3)
      temp = 0d0

      call FetchSeries(DFILE, "TCEA.A", temp)
      tcea_a(37:70) = temp(37:70)
      temp = 0d0
      call FetchSeries(AFILE,  "TCEA.A",temp)
      tcea_a(71:80) = temp(71:80)
      temp = 0d0

      call FetchSeries(DFILE, "CE_M.A", temp)
      ce_m(0,0,37:50) = temp(37:50)
      temp = 0d0
      call FetchSeries(MEF,  "CE_M.A",temp)
      ce_m(0,0,51:80) = temp(51:80)
      temp = 0d0

      oasdi_te_ee = (oasdi_tw - oasdi_merw + oasdise_ti) * 1d3
      do yr = 37, min(OUTPUT_LAST_YR, endYr)
         atxe(yr) = oasdi_te_ee(yr) / tcea_a(yr)
      end do

      oasdi_te_ee_mef = (oasdi_tw - oasdi_merw + oasdise_ti - we_sf) * 1d3
      do yr = 37, min(OUTPUT_LAST_YR, endYr)
         atxe_mef(yr) = oasdi_te_ee_mef(yr) / ce_m(0,0,yr)
      end do

      do yr = 37, min(OUTPUT_LAST_YR, endYr)
         if (yr >= OUTPUT_TRYR - 1) then
            write(6102,'(i4,f11.2)') 1900+yr, atxe(yr)
            write(6104,'(i4,f11.2)') 1900+yr, atxe_mef(yr)
         else
            if (needHist) write(6103,'(i4,f11.2)') 1900+yr, atxe(yr)
            if (needHist) write(6105,'(i4,f11.2)') 1900+yr, atxe_mef(yr)
         end if
      end do
   
   end subroutine ate

!===============================================================================

   subroutine wiaceb()
   
      integer :: yr
      real (kind = 8), dimension(MAX_YR) :: pchaiw
      real (kind = 8), dimension(MAX_YR) :: pchacea
      real (kind = 8), dimension(MAX_YR) :: pchcpiw_u_a
      real (kind = 8), dimension(MAX_YR) :: ate1, ate2
      real (kind = 8), dimension(MAX_YR) :: oasdi_tw, oasdi_merw, oasdise_ti
      real (kind = 8), dimension(MAX_YR) :: we_sf
      
      pchaiw = pch(aiw_a)
      
      pchacea = pch((wsca_a + cse_tot_a) / tcea_a)
      
      pchcpiw_u_a = pch(cpiw_u_a)

      oasdi_tw = wsterod / 1d3
      oasdi_merw = wsmerefod / 1d3
      oasdise_ti = seetod / 1d3
      ate1 = 1000d0 * (oasdi_tw - oasdi_merw + oasdise_ti) / (ce_m(0,0,:) + te_s(0,0,:))
      
      we_sf = we_sf_a
      ate2 = 1000d0 * (oasdi_tw - oasdi_merw + oasdise_ti - we_sf) / ce_m(0,0,:)
   
      do yr = OUTPUT_TRYR - 1, min(OUTPUT_LAST_YR, endYr)
         write(6106,'(i4,4f11.5,f11.2)') 1900+yr, pchaiw(yr), pchacea(yr), &
            beninc_a(yr), pchcpiw_u_a(yr), ate1(yr)
         write(6107,'(i4,4f11.5,f11.2)') 1900+yr, pchaiw(yr), pchacea(yr), &
            beninc_a(yr), pchcpiw_u_a(yr), ate2(yr)
      end do
   
   end subroutine wiaceb

!===============================================================================
   
   subroutine RevearnData()
   
      integer :: i
      double precision, dimension(MAX_YR) :: a1, a3, a4, a8, a9, a10, a11, &
         a12, a13, a17, a18, wscpnf, wscslod
      double precision, dimension(MAX_QTR) :: q1, q2, q3, q4

      write(6201,'(a)') "MODEEM"
      write(6201,'(a)') trim(assumpt(2))
      
      write(6201,'(a,t33,a)') "ADDSETROD", "A 2015 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (addsetrod(i),i=115,endYr)

      write(6201,'(a,t33,a)') "ADDWSTREEOD", "A 2015 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (addwstreeod(i),i=115,endYr)

      write(6201,'(a,t33,a)') "ADDWSTREEODTREND", "A 2015 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (addwstreeodtrendtarget(i),i=115,endYr)

      write(6201,'(a,t33,a)') "AIW", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (aiw_a(i),i=71,endYr)
      
      write(6201,'(a,t33,a)') "AWSCFM", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (acfmw_a(i)*1d3,i=71,endYr)

      write(6201,'(a,t33,a)') "AWSCML", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (acmw_a(i)*1d3,i=71,endYr)

      write(6201,'(a,t33,a)') "DMWCHI", "A 1983 "//trim(IntToAsc(1900+endYr))
      call FetchSeries(DFILE, "DMWC_H.A", a12)
      write(6201,'(4f20.10)') (a12(i)*1d3,i=83,endYr)

      write(6201,'(a,t33,a)') "DMWCOD", "A 1983 "//trim(IntToAsc(1900+endYr))
      call FetchSeries(DFILE, "DMWC_O.A", a13)
      write(6201,'(4f20.10)') (a13(i)*1d3,i=83,endYr)
      
      write(6201,'(a,t33,a)') "ECFCHO", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (tefc_n_a(i),i=83,endYr)
      
      write(6201,'(a,t33,a)') "ECFCOD", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (tefc_o_a(i),i=83,endYr)
      
      write(6201,'(a,t33,a)') "ECHITOT", "A 1987 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (he_m(0,0,i) + te_s(0,0,i),i=87,endYr)

      write(6201,'(a,t33,a)') "ECSEHI", "A 1988 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (ceso_m(0,0,i) + cmb_tot_a(i),i=88,endYr)

      write(6201,'(a,t33,a)') "ECSENOMAX", "A 1988 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (ceso_m(0,0,i) + cmb_tot_a(i),i=88,endYr)      

      write(6201,'(a,t33,a)') "ECSEO", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (ceso_m(0,0,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECSEOD", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (csw_a(i),i=81,endYr)
      
      write(6201,'(a,t33,a)') "ECSLNOIS", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (tesl_n_a(i),i=83,endYr)
      
      write(6201,'(a,t33,a)') "ECSLP91", "A 1983 "//trim(IntToAsc(1900+endYr))
      a1(83:90) = tesl_o_a(83:90)
      a1(91:) = tesl_a(91:) * 0.659d0
      write(6201,'(4f20.10)') (a1(i),i=83,endYr)      
      
      write(6201,'(a,t33,a)') "ECSLNRP", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (tesl_o_a(i) - a1(i),i=83,endYr)      

      write(6201,'(a,t33,a)') "ECSLOD", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (tesl_o_a(i),i=83,endYr)
      
      write(6201,'(a,t33,a)') "ECWSHI", "A 1981 "//trim(IntToAsc(1900+endYr))
      call FetchSeries(AFILE, "TCEAHI.A", a17)
      a18(81:82) = tcea_a(81:82)
      a18(83:86) = a17(83:86)
      a18(87:) = tceahi_a(87:)
      write(6201,'(4f20.10)') (a18(i) - (ceso_m(0,0,i) - .039d0 * (he_wof_m(0,0,i) + he_wol_m(0,0,i))),i=81,endYr)
      
      write(6201,'(a,t33,a)') "ECWSOD", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(0,0,i) + te_s(0,0,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSOD_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(0,0,i),i=81,endYr)
      
      write(6201,'(a,t33,a)') "ECWSODFU16_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,0,i) - cew_m(2,24,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF1619_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,27,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF2024_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,3,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF2529_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,4,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF3034_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,5,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF3539_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,6,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF4044_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,7,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF4549_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,8,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF5054_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,9,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF5559_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,10,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF6064_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,11,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF6569_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,12,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODF70O_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(2,23,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODMU16_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,0,i) - cew_m(1,24,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM1619_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,27,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM2024_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,3,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM2529_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))  
      write(6201,'(4f20.10)') (cew_m(1,4,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM3034_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,5,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM3539_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,6,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM4044_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,7,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM4549_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,8,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM5054_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,9,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM5559_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,10,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM6064_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,11,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM6569_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,12,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ECWSODM70O_MEF", "A 1981 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cew_m(1,23,i),i=81,endYr)

      write(6201,'(a,t33,a)') "ESLCG", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (tesl_n_o_nhi_ns_a(i) + tesl_n_n_nhi_ns_a(i),i=83,endYr)

      write(6201,'(a,t33,a)') "ESLSTUD", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (tesl_n_s_a(i),i=83,endYr)
      
      write(6201,'(a,t33,a)') "GAPLAG", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (rtp_a(i),i=71,endYr)
      
      write(6201,'(a,t33,a)') "RTP", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (rtp_a(i),i=71,endYr)
      
      write(6201,'(a,t33,a)') "RU", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (ru_a(0,0,i),i=71,endYr)
      
      write(6201,'(a,t33,a)') "SEECCMB", "A 1991 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cse_cmb_tot_a(i)*1d3,i=91,endYr)
      
      write(6201,'(a,t33,a)') "SEECHI", "A 1991 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cse_tot_a(i)*1d3,i=91,endYr)
      
      write(6201,'(a,t33,a)') "SEECNOMAX", "A 1991 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cse_tot_a(i)*1d3,i=91,endYr)
      
      write(6201,'(a,t33,a)') "SEECOD", "A 1991 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cse_tot_a(i)*1d3,i=91,endYr)
      
      write(6201,'(a,t33,a)') "SEECOD_OLD", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (cse_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "SEETODCMB", "A 1995 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (seetodcmb(i)*1d3,i=95,endYr)

      write(6201,'(a,t33,a)') "SEETODEXOG", "A 1995 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (seetod(i),i=95,endYr)

      write(6201,'(a,t33,a)') "SEETODSEO", "A 1995 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (seetodseo(i)*1d3,i=95,endYr)

      write(6201,'(a,t33,a)') "TAXMAXHI", "A 1971 "//trim(IntToAsc(1900+endYr))
      call FetchSeries(DFILE, "TAXMAXHI.A", a11)
      write(6201,'(4f20.10)') (a11(i)*1d3,i=71,endYr)

      write(6201,'(a,t33,a)') "TAXMAXOD", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (taxmax_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "TCFCD", "Q 1971 1 "//trim(IntToAsc(1900+endYr))//" 4"  
      call interpolate(q1, wsggefc_a, "repeat", "")
      write(6201,'(4f20.10)') (wsggefc(i) / (4d0 * q1(i)),i=284,endQtr)
      
      write(6201,'(a,t33,a)') "TCMD", "Q 1971 1 "//trim(IntToAsc(1900+endYr))//" 4" 
      call interpolate(q2, wsgfm_a, "repeat", "")
      write(6201,'(4f20.10)') ((wsgfm(i) / (4d0 * q2(i))),i=284,endQtr)

      write(6201,'(a,t33,a)') "TCPD", "Q 1971 1 "//trim(IntToAsc(1900+endYr))//" 4" 
      call interpolate(q3, wsdp_a, "repeat", "")
      write(6201,'(4f20.10)') ((wsdp(i) / (4d0 * q3(i))),i=284,endQtr)

      write(6201,'(a,t33,a)') "TCSLD", "Q 1971 1 "//trim(IntToAsc(1900+endYr))//" 4"  
      call interpolate(q4, wsggesl_a, "repeat", "")
      write(6201,'(4f20.10)') ((wsggesl(i) / (4d0 * q4(i))),i=284,endQtr)

      write(6201,'(a,t33,a)') "TETODCMB", "A 1995 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (tetodcmb(i)*1d3,i=95,endYr)

      write(6201,'(a,t33,a)') "WSCCMB", "A 1991 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (w_cmbtot_a(i)*1d3,i=91,endYr)
      
      write(6201,'(a,t33,a)') "WSCFCHO", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (wefc_n_a(i)*1d3,i=83,endYr)
      
      write(6201,'(a,t33,a)') "WSCFCOD", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (wsgfca_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSCFM", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (wspf_o_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSCHI", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(3f25.10)') (wscahi_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSCML", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (wsgmlc_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSCOD", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(3f25.10)') (wsca_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSCOD_SF", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (we_sf_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSCPHH", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (wsph_o_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSCPNF", "A 1971 "//trim(IntToAsc(1900+endYr))
      wscpnf(71:82) = wsca_a(71:82) - wspf_o_a(71:82) - wsgfca_a(71:82) - wsgmlc_a(71:82) - wsgslca_a(71:82)
      wscpnf(83:endYr) = wsca_a(83:endYr) - wspf_o_a(83:endYr) - wsgfca_a(83:endYr) - wsgmlc_a(83:endYr) - wesl_o_a(83:endYr)
      write(6201,'(3f25.10)') (wscpnf(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSCSLHI", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(3f25.10)') ((wesl_o_a(i) + wesl_n_hi_a(i))*1d3,i=71,endYr)

      write(6201,'(a,t33,a)') "WSCSLNRP", "A 1991 "//trim(IntToAsc(1900+endYr))
      a3 = wsgslca_a
      a3(91) = 266.5d0
      a3(92) = 280.8d0
      a3(93:) = wesl_a(93:) * (0.710d0 - 0.006d0)
      write(6201,'(4f20.10)') ((wsgslca_a(i) - a3(i))*1d3,i=91,endYr)
      
      write(6201,'(a,t33,a)') "WSCSLOD", "A 1971 "//trim(IntToAsc(1900+endYr))
      wscslod(71:82) = wsgslca_a(71:82)
      wscslod(83:endYr) = wesl_o_a(83:endYr)
      write(6201,'(4f20.10)') (wscslod(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSCSLP91", "A 1971 "//trim(IntToAsc(1900+endYr))
      a4 = wsgslca_a
      a4(91) = 266.5d0
      a4(92) = 280.8d0
      a4(93:) = wesl_a(93:) * (0.710d0 - 0.006d0)
      write(6201,'(4f20.10)') (a4(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSD", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(3f25.10)') (wsd_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSP", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(3f25.10)') (wsdp_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSS", "A 1971 "//trim(IntToAsc(1900+endYr))
      write(6201,'(3f25.10)') (wss_a(i)*1d3,i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSSLCG", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (wesl_n_nhi_ns_a(i)*1d3,i=83,endYr)
      
      write(6201,'(a,t33,a)') "WSSLNOIS", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (wesl_n_a(i)*1d3,i=83,endYr)
      
      write(6201,'(a,t33,a)') "WSSLSTUD", "A 1983 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (wesl_n_nhi_s_a(i)*1d3,i=83,endYr)
      
      write(6201,'(a,t33,a)') "WSMEREFODEXOG", "A 2014 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (wsmerefod(i),i=114,endYr)

      write(6201,'(a,t33,a)') "WSTEEODEXOG", "A 2015 "//trim(IntToAsc(1900+endYr))
      write(6201,'(3f25.10)') (wsterod(i),i=115,endYr)

      write(6201,'(a,t33,a)') "WSTRRTPHI", "A 1971 "//trim(IntToAsc(1900+endYr))

      call FetchSeries(DFILE, "WSPRRBT1.A", a8)
      a10 = a8
      call FetchSeries(AFILE,"WSPRRB.A",a9)
      a10(71:) = a9(71:) * 1d3
      write(6201,'(4f20.10)') (a10(i),i=71,endYr)
      
      write(6201,'(a,t33,a)') "WSTTIPSSR", "A 1978 "//trim(IntToAsc(1900+endYr))
      write(6201,'(4f20.10)') (tips_sr_a(i)*1d3,i=78,endYr)
      
      write(6201,'(a,t33,a)') "WTWPO", "Q 1971 1 "//trim(IntToAsc(1900+endYr))//" 4"  
      write(6201,'(4f20.10)') ((wsgefc(i) / wsggefc(i)),i=284,endQtr)
      
   end subroutine RevearnData
   
!===============================================================================
   
   subroutine WriteOutputFileEV()
   
      write(6009,'(a)') "pageselect a"
      write(6009, '(a)') "series oasdi_tw"
      write(6009,'(a,i0,a)') "oasdi_tw.fill(o=", OUTPUT_TRYEAR-1, ") _"
      call WriteSeriesEV(wsterod/1d3, OUTPUT_TRYR-1, min(max(199, OUTPUT_LAST_YR), endYr), 6009)
      write(6009,*)
      
      write(6009,'(a)') "pageselect a"
      write(6009, '(a)') "series oasdi_merw"
      write(6009,'(a,i0,a)') "oasdi_merw.fill(o=", OUTPUT_TRYEAR-1, ") _"
      call WriteSeriesEV(wsmerefod/1d3, OUTPUT_TRYR-1, min(max(199, OUTPUT_LAST_YR), endYr), 6009)
      write(6009,*)
      
      write(6009,'(a)') "pageselect a"
      write(6009, '(a)') "series oasdise_ti"
      write(6009,'(a,i0,a)') "oasdise_ti.fill(o=", OUTPUT_TRYEAR-1, ") _"
      call WriteSeriesEV(seetod/1d3, OUTPUT_TRYR-1, min(max(199, OUTPUT_LAST_YR), endYr), 6009)
      write(6009,*)

      write(6009,'(a)') "pageselect a"
      write(6009, '(a)') "series oasdi_etp"
      write(6009,'(a,i0,a)') "oasdi_etp.fill(o=", OUTPUT_TRYEAR-1, ") _"
      call WriteSeriesEV(wsterod/1d3 - 0.5*wsmerefod/1d3 + seetod/1d3, OUTPUT_TRYR-1, min(max(199, OUTPUT_LAST_YR), endYr), 6009)
      write(6009,*)
      
      write(6009,'(a)') "pageselect a"
      write(6009, '(a)') "series seetodcmb"
      write(6009,'(a,i0,a)') "seetodcmb.fill(o=", 1995, ") _"
      call WriteSeriesEV(seetodcmb, 95, min(max(199, OUTPUT_LAST_YR), endYr), 6009)
      write(6009,*)

      write(6009,'(a)') "pageselect a"
      write(6009, '(a)') "series seetodseo"
      write(6009,'(a,i0,a)') "seetodseo.fill(o=", 1995, ") _"
      call WriteSeriesEV(seetodseo, 95, min(max(199, OUTPUT_LAST_YR), endYr), 6009)
      write(6009,*)

      write(6009,'(a)') "pageselect a"
      write(6009, '(a)') "series tetodcmb"
      write(6009,'(a,i0,a)') "tetodcmb.fill(o=", 1995, ") _"
      call WriteSeriesEV(tetodcmb*1d3, 95, min(max(199, OUTPUT_LAST_YR), endYr), 6009)
      write(6009,*)

     call CloseFile(6009)      

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
         num = 6009
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
      write(6010,'(a)') "wfopen " // trim(OUT_PATH) // "\mul\" // trim(afilename)
      write(6010,'(a)') "exec " // trim(OUT_PATH) // "\internal\TaxPayOut.prg"
      write(6010,'(a)') "wfsave(2) " // trim(OUT_PATH) // "\mul\" // trim(afilename)
      write(6010,'(a)') "wfclose"
      
      call CloseFile(6010)
   
   end subroutine WriteProgramEV

!===============================================================================
   
   subroutine RunProgramEV()
   
      integer :: rv3
   
      write(*,'(a$)') "Importing solution to file " // trim(afileName) // ".wf1"
      rv3 = ExecuteProgram(trim(OUT_PATH) // "\internal\TaxPayImport.prg")
      if (rv3 == 0) then
         write(*,'(a)') " ...succeeded"
      else
         write(*,'(a)') " ... failed"
      end if
              
   end subroutine RunProgramEV

!===============================================================================  
   
   function pch(series) result(pchseries)
   
      real (kind = 8), dimension(:) :: series
      real (kind = 8), dimension(size(series)) :: pchseries
      integer :: i, sign
      
      do i = 1, size(series) - 1
      
         if (series(i) < 0) then
            sign = -1
         else if (series(i) > 0) then
            sign = 1
         else
            pchseries(i) = 0
            cycle
         end if
         
         pchseries(i + 1) = (series(i + 1) / series(i) - 1) * 100d0 * sign
         
      end do
   
   end function pch

!===============================================================================

   subroutine RevEarnToStoch()
   
      !integer :: iSex, iAge
      real (kind = 8), dimension (MAX_YR) :: oasdi_tw, oasdi_merw, oasdise_ti, oasdi_etp
      character(len=16) :: serName, blnkStr = "                "
      character(len=12) :: serData
      
      write (serData,'(a2,2i5)') " a", TRYEAR - 2, 1900 + endYr
      
      serName = "ate" ! NOTE DIFFERENT FORMAT
      write (9999,'(a28,94f15.6)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 atxe(TRYEAR-1900-2:endYr)
      
      serName = "ate_mef" ! NOTE DIFFERENT FORMAT
      write (9999,'(a28,94f15.6)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 atxe_mef(TRYEAR-1900-2:endYr)

      serName = "cpiw_u"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 cpiw_u_a(TRYEAR-1900-2:endYr)

      serName = "cpiwdec3_u"
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 RoundCl(cpiw_u_a(TRYEAR-1900-2:endYr), 3)

      serName = "oasdise_ti"
      oasdise_ti = seetod / 1d3
      call FetchSeries(DFILE, "OASDISE_TI.A", oasdise_ti)
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 oasdise_ti(TRYEAR-1900-2:endYr)

      serName = "oasdi_etp"
      oasdi_etp = wsterod/1d3 - 0.5*wsmerefod/1d3 + seetod/1d3
      call FetchSeries(DFILE, "OASDI_ETP.A", oasdi_etp)
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 oasdi_etp(TRYEAR-1900-2:endYr)

      serName = "oasdi_merw"
      oasdi_merw = wsmerefod / 1d3
      call FetchSeries(DFILE, "OASDI_MERW.A", oasdi_merw)
      write (9999,'(a28,94f15.8)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 oasdi_merw(TRYEAR-1900-2:endYr)

      serName = "oasdi_tw"
      oasdi_tw = wsterod / 1d3
      call FetchSeries(DFILE, "OASDI_TW.A", oasdi_tw)
      write (9999,'(a28,94f15.7)') trim(serName) // serData // blnkStr(len_trim(serName)+1:), &
                 oasdi_tw(TRYEAR-1900-2:endYr)

   end subroutine RevEarnToStoch

!===============================================================================

   subroutine ShellSortStrings(a)
   
      integer :: i, j, increment  
      character(len=1500) :: temp  
      character(len=1500), intent(inout), dimension(:) :: a   
      increment = size(a) / 2  
      !print*, size(a), increment
      do while (increment > 0)    
          !print*, "increment = ", increment  
          do i = increment+1, size(a)         
              j = i         
              temp = a(i)         
              do while (a(j-increment) > temp)   
                  a(j) = a(j-increment)            
                  j = j - increment  
                  if (j < increment+1) exit       
              end do        
              a(j) = temp      
          end do      
          if (increment == 2) then      
              increment = 1      
          else         
              increment = increment * 5 / 11      
          end if        
      end do 

   end subroutine ShellSortStrings

!===============================================================================

   subroutine SortedToStoch()
   
      integer :: i, j, n, begPos, endPos
      character(len=1500) :: seriesLine
      character(len=28) :: seriesHeader
      character(len=1500), dimension(4000) :: seriesStack
      character(len=15), dimension(MAX_YR-(TRYEAR-1900-3)) :: seriesValues
      logical :: eof
      integer :: MAX_YEAR = 1900 + MAX_YR
      
      rewind (9999)
      
      eof = .FALSE.   
      i = 0
      ! Abandon all hope, ye who enter here.
      do while (.not.eof)
        i = i + 1
        read (9999,'(a1333)',END=666) seriesStack(i)
      cycle
666     eof = .TRUE.
      end do
      n = i
      
      call ShellSortStrings(seriesStack(1:n))
      
      write (9998,'(a8)') "ECONDATA"
      
      do i = 1, n
        seriesLine = seriesStack(i)
        if (ichar(seriesLine(1:1)) /= 0) then
          seriesHeader = seriesLine(1:28)
          write (9998,'(a28)') seriesHeader
          do j = TRYEAR-1900-2, endYr
            begPos = 29 + 15 * (j - TRYEAR + 1902)
            endPos = begPos + 14
            seriesValues(j-TRYEAR+1903) = seriesLine(begPos:endPos)
          end do
          write (9998,'(8a15)') seriesValues(1:min(MAX_YEAR, endYr)-TRYEAR+1903)
        end if
      end do

   end subroutine SortedToStoch

!===============================================================================

end module EconRevEarnOutMod