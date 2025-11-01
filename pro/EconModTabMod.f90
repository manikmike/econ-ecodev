module EconModTabMod

   use EconParMod
   use EconLibMod
   use EconRevearnVarMod
   include "OcactFortLib.inc"
   implicit none

   private
   public :: EconModTabMain
   
   integer, parameter :: START_YEAR = 2008
   ! integer, parameter :: END_YEAR =  2090
   integer, parameter :: START_YR = START_YEAR - 1900
   ! integer, parameter :: endYr = END_YEAR - 1900
   integer :: yr
   character (len=36) :: label
   real (kind = 8), dimension(2, MAX_YR) :: n16onm
   real (kind = 8), dimension(MAX_YR) :: kgdpreal_a

contains

!===============================================================================

   subroutine EconModTabMain()
   
      call InitModTab()
   
      label = "Calendar Year"
      write(8001,'(a36,100i10)') label, (1900+yr,yr=START_YR, endYr)
      
      label = "OCACT Area Population"
      write(8001,'(/a36,100i9)') label
      
      label = "  Total"
      write(8001,'(a36,100f10.3)') label, (sum(nsy_a(1:2,0:100,yr)),yr=START_YR, endYr)
      
      label = "    Aged Under 16"
      write(8001,'(a36,100f10.3)') label, (sum(nsy_a(1:2,0:15,yr)),yr=START_YR, endYr)
      
      label = "      Aged Under 6"
      write(8001,'(a36,100f10.3)') label, (sum(nsy_a(1:2,0:5,yr)),yr=START_YR, endYr)
      
      label = "    Aged 16 and Over"
      write(8001,'(a36,100f10.3)') label, (sum(nsy_a(1:2,16:100,yr)),yr=START_YR, endYr)

      label = "      Aged 16 to 64"
      write(8001,'(a36,100f10.3)') label, (sum(nsy_a(1:2,16:64,yr)),yr=START_YR, endYr)
      
      label = "      Aged 65 and Over"
      write(8001,'(a36,100f10.3)') label, (sum(nsy_a(1:2,65:100,yr)),yr=START_YR, endYr)
      
      label = "      Proportion Female"
      write(8001,'(/a36,100f10.4)') label, (sum(nsy_a(2,16:100,yr)) / sum(nsy_a(1:2,16:100,yr)),yr=START_YR, endYr)
      
      label = "      Proportion Never Married"
      write(8001,'(a36,100f10.4)') label, (sum(n16onm(1:2,yr)) / sum(nsy_a(1:2,16:100,yr)),yr=START_YR, endYr)

      label = "      Ratio of Children (<6) to"      
      write(8001,'(a36,100f10.4)') label, (sum(nsy_a(1:2,0:5,yr)) / sum(nsy_a(2,20:44,yr)),yr=START_YR, endYr)

      label = "        Women (20-44)"
      write(8001,'(a36,100f10.3)') label

      label = "Civilian Noninst.Population"
      write(8001,'(/a36,100i9)') label
      
      label = "  Total"
      write(8001,'(a36,100f10.3)') label, (n_a(0,0,yr),yr=START_YR, endYr)
      
      label = "    Aged Under 16"
      write(8001,'(a36,100f10.3)') label, (n_a(0,0,yr) - sum(n_a(1:2,24,yr)),yr=START_YR, endYr)
      
      label = "    Aged 16 and Over"
      write(8001,'(a36,100f10.3)') label, (sum(n_a(1:2,24,yr)),yr=START_YR, endYr)

      label = "      Aged 16 to 64"
      write(8001,'(a36,100f10.3)') label, (sum(n_a(1:2,1:11,yr)),yr=START_YR, endYr)
      
      label = "      Aged 65 and Over"
      write(8001,'(a36,100f10.3)') label, (sum(n_a(1:2,22,yr)),yr=START_YR, endYr)
      
      label = "Civilian Labor Force Participation"
      write(8001,'(/a36,100f10.4)') label, (p_a(0,24,yr),yr=START_YR, endYr)
      
      label = "  Male LFPR"
      write(8001,'(a36,100f10.4)') label, (p_a(1,24,yr),yr=START_YR, endYr)
      
      label = "  Female LFPR"
      write(8001,'(a36,100f10.4)') label, (p_a(2,24,yr),yr=START_YR, endYr)
      
      label = "Civilian Labor Force"
      write(8001,'(a36,100f10.3)') label, (lc_a(0,0,yr),yr=START_YR, endYr)
      
      label = "Civilian Employment"
      write(8001,'(a36,100f10.3)') label, (e_a(0,0,yr),yr=START_YR, endYr)

      label = "Total At-any-time Employment (TE)"
      write(8001,'(/a36,100f10.3)') label, (te_a(0,0,yr),yr=START_YR, endYr)
      
      label = "  Not Reported"
      write(8001,'(a36,100f10.3)') label, (te_u(0,0,yr),yr=START_YR, endYr)
      
      label = "  Reported"
      write(8001,'(a36,100f10.3)') label, (te_a(0,0,yr) - te_u(0,0,yr),yr=START_YR, endYr)
      
      label = "    Suspense File"
      write(8001,'(a36,100f10.3)') label, (te_s(0,0,yr),yr=START_YR, endYr)
      
      label = "    Master Earnings File (MEF)"
      write(8001,'(a36,100f10.3)') label, (te_m(0,0,yr),yr=START_YR, endYr)
      
      label = "      OASDI Covered Workers"
      write(8001,'(a36,100f10.3)') label, (ce_m(0,0,yr),yr=START_YR, endYr)
      
      label = "        Wage and Salary Workers"
      write(8001,'(a36,100f10.3)') label, (cew_m(0,0,yr),yr=START_YR, endYr)

      label = "        Self-Employed Only"
      write(8001,'(a36,100f10.3)') label, (ceso_m(0,0,yr),yr=START_YR, endYr)
      
      label = "    Total OASDI Covered"
      write(8001,'(a36,100f10.3)') label, (tcea_a(yr),yr=START_YR, endYr)
      
      label = "      Wage and Salary Workers"
      write(8001,'(a36,100f10.3)') label, (wswa_a(yr),yr=START_YR, endYr)
      
      label = "Potential Real GDP"
      write(8001,'(/a36,100f10.1)') label, (kgdpreal_a(yr),yr=START_YR, endYr)
      
      label = "Actual Real GDP"
      write(8001,'(a36,100f10.1)') label, (gdpreal_a(yr),yr=START_YR, endYr)
      
      label = "Actual Nominal GDP"
      write(8001,'(a36,100f10.1)') label, (gdp_a(yr),yr=START_YR, endYr)

      label = "Total Labor Compensation"
      write(8001,'(/a36,100f10.1)') label, (wss_a(yr) + yf_a(yr) + ynf_a(yr),yr=START_YR, endYr)
      
      label = "  Employee Compensation"
      write(8001,'(a36,100f10.1)') label, (wss_a(yr),yr=START_YR, endYr)

      label = "    Total Wages and Salaries"
      write(8001,'(a36,100f10.1)') label, (wsd_a(yr),yr=START_YR, endYr)
      
      label = "    Total Covered Wages and Salaries"
      write(8001,'(a36,100f10.1)') label, (wsca_a(yr),yr=START_YR, endYr)
      
      label = "    OASDI Taxable Wages"
      write(8001,'(a36,100f10.1)') label, (wsterod(yr) / 1d3,yr=START_YR, endYr)

      label = "  Proprietor Income"
      write(8001,'(a36,100f10.1)') label, (yf_a(yr) + ynf_a(yr),yr=START_YR, endYr)
      
      label = "    Covered SE Net Earnings"
      write(8001,'(a36,100f10.1)') label, (cse_tot_a(yr),yr=START_YR, endYr)

      label = "    Taxable SE Net Earnings"
      write(8001,'(a36,100f10.1)') label, (seetod(yr) / 1d3,yr=START_YR, endYr)
      
      label = "  OASDI Effective Taxable Payroll"
      write(8001,'(a36,100f10.1)') label, ((0.5d0 * (wsteeod(yr) + wsterod(yr)) + seetod(yr)) / 1d3,yr=START_YR, endYr)

      label = "Average Covered Wage"
      write(8001,'(/a36,100f10.3)') label, (acwa_a(yr),yr=START_YR, endYr)
      
      label = "Average Indexing Wage"
      write(8001,'(a36,100f10.3)') label, (aiw_a(yr) / 1d3,yr=START_YR, endYr)

   end subroutine EconModTabMain

!===============================================================================

   subroutine InitModTab()
   
      real (kind = 8), dimension(MAX_YR) :: oasdi_tw, oasdise_ti, oasdi_tw_ee;
   
      call FetchSeries(OFILE1, "NM16ONM.A", n16onm(1,:))
      call FetchSeries(OFILE1, "NF16ONM.A", n16onm(2,:))
      call FetchSeries(DFILE, "N.A", N_a(0,0,:))
      call FetchSeries(AFILE, "K"//trim(REAL_GDP_SERIES)//".A", kgdpreal_a)

   end subroutine InitModTab
   
!===============================================================================

end module EconModTabMod