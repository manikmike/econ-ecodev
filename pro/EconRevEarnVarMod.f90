module EconRevEarnVarMod

   use EconModSolAVarMod
   implicit none
   public

   ! Total Taxable Payroll
   integer, parameter :: FIRST_AMALGAM_YEAR = 2000
   integer, parameter :: LAST_AMALGAM_YEAR  = 2022
   integer, parameter :: LAST_SELF_EMPLOYMENT_TAXABLE_YEAR  = 2023
   real (kind = 8), dimension(3) :: TR_TARGET = (/ 84.0d0, 82.5d0, 81.0d0 /)
   real (kind = 8), dimension(MAX_YR) :: TWR_TARGET
   real (kind = 8), dimension(MAX_YR) :: TSE_TARGET

   ! Average taxable payroll - DO NOT USE BEFORE CALLING SUBROUTINE ATE IN ECONREVEARNOUTMOD!
   real (kind = 8), dimension(MAX_YR) :: atxe, atxe_mef

   ! OASDI taxable...
   real (kind = 8), dimension(MAX_YR) :: wsteeod   ! employee wages
   real (kind = 8), dimension(MAX_YR) :: wsmerefod ! multi-employer refund wages
   real (kind = 8), dimension(MAX_YR) :: wsterod   ! employer wages
   real (kind = 8), dimension(MAX_YR) :: seetod    ! self-employment earnings
   real (kind = 8), dimension(MAX_YR) :: seetodcmb ! self-employment earnings of
                                                   ! combination workers
   real (kind = 8), dimension(MAX_YR) :: seetodseo ! self-employment earnings of
                                                   ! SE-only workers
   real (kind = 8), dimension(MAX_YR) :: tetodcmb  ! total earnings of
                                                   ! combination workers
   ! OASDI total covered employment: ce_a(0,0,i) declared in ModSolA
   ! Ratio of real to potential GDP :: rtp_a(i) declared in ModSolA
   real (kind = 8), dimension(MAX_YR) :: wstreeod   ! employee wages (taxable ratio)
   real (kind = 8), dimension(MAX_YR) :: wsrmerefod ! multi-employer refund wages (ratio)
   real (kind = 8), dimension(MAX_YR) :: wstrerod   ! employer wages (taxable ratio)
   real (kind = 8), dimension(MAX_YR) :: seetrod    ! self-employment earnings (taxable ratio)

   real (kind = 8), dimension(MAX_YR) :: wstreexrxadjodpg ! OASDI taxable wage ratio for employees 
                                                          ! without refunds and adjustments program generated

   ! OASDI taxable employee wage ratio adjustment factors
   real (kind = 8), dimension(MAX_YR) :: addwstreeod       ! total
   real (kind = 8), dimension(MAX_YR) :: addwstreeodagesex ! age and sex
   real (kind = 8), dimension(MAX_YR) :: addwstreeodbuscyc ! business cycle
   real (kind = 8), dimension(MAX_YR) :: addwstreeodbyerr  ! base year error
   real (kind = 8), dimension(MAX_YR) :: addwstreeodtrend  ! trend
   real (kind = 8), dimension(MAX_YR) :: addwstreeodtrendtarget  ! trend plus target
   real (kind = 8), dimension(MAX_YR) :: multwstreeodtrend ! trend
   real (kind = 8), dimension(MAX_YR) :: addtrtarget       ! taxable ratio target

   real (kind = 8), dimension(MAX_YR) :: wstrxrpg ! Adjusted OASDI taxable wage ratio for employees 
                                                  ! without refunds
   ! OASDI taxable self-employment adjustment factors
   real (kind = 8), dimension(MAX_YR) :: addsetrod     ! total of adjustment factors
   real (kind = 8), dimension(MAX_YR) :: add2addsetrod ! exogenous
   real (kind = 8), dimension(MAX_YR) :: setrxrm      ! taxable ratio without relmax effect

contains

!===============================================================================

   subroutine InitializeRevEarnVars()

      ! Eventually read from file MultwstrTR2013{Int,Low,High}.dat
      multwstreeodtrend(71:101) = 1.0d0
      multwstreeodtrend(102:)   = 0.4d0

      if (trim(FILE_EXTENSION) == "alt1") then
         multwstreeodtrend(TRYEAR-1900:) = 0d0
      else if (trim(FILE_EXTENSION) == "alt3") then
         multwstreeodtrend(TRYEAR-1900:) = 0.8d0
      end if
      
      TWR_TARGET(113:149) = (/ 85.503729d0, 85.070614d0, 84.530408d0, 84.342698d0, &
                               84.340253d0, 84.424265d0, 84.557905d0, 84.690593d0, & 
                               84.768818d0, 84.874781d0, 84.898997d0, 84.894581d0, &
                               84.903365d0, 84.909705d0, 84.911238d0, 84.889033d0, &
                               84.875296d0, 84.851505d0, 84.816413d0, 84.771726d0, &
                               84.712897d0, 84.661225d0, 84.611707d0, 84.564363d0, &
                               84.528368d0, 84.498358d0, 84.477176d0, 84.461561d0, &
                               84.450324d0, 84.442090d0, 84.436625d0, 84.432714d0, &
                               84.430141d0, 84.428373d0, 84.427097d0, 84.426295d0, &
                               84.424588d0 /)
      TWR_TARGET(150:) = TWR_TARGET(149)
      
      TSE_TARGET(113:190) = (/ 62.806669d0, 62.558978d0, 62.273876d0, 62.334511d0, &
                               62.535925d0, 62.754942d0, 63.006444d0, 63.155949d0, &
                               63.249871d0, 63.359213d0, 63.427251d0, 63.423156d0, &
                               63.403409d0, 63.411222d0, 63.382025d0, 63.339408d0, &
                               63.312634d0, 63.294086d0, 63.233004d0, 63.175828d0, &
                               63.081077d0, 63.000422d0, 62.935806d0, 62.851536d0, &
                               62.788817d0, 62.727839d0, 62.685462d0, 62.657454d0, &
                               62.625071d0, 62.586858d0, 62.570644d0, 62.544929d0, &
                               62.536112d0, 62.515016d0, 62.481473d0, 62.459367d0, &
                               62.444494d0, 62.417415d0, 62.397093d0, 62.371457d0, &
                               62.350097d0, 62.331913d0, 62.315713d0, 62.291259d0, &
                               62.266709d0, 62.241050d0, 62.221382d0, 62.198739d0, &
                               62.172491d0, 62.149124d0, 62.127413d0, 62.106310d0, &
                               62.084860d0, 62.061917d0, 62.036627d0, 62.008259d0, &
                               61.981472d0, 61.960298d0, 61.933250d0, 61.909728d0, &
                               61.888362d0, 61.867896d0, 61.843064d0, 61.821452d0, &
                               61.801766d0, 61.779012d0, 61.759874d0, 61.739468d0, &
                               61.720416d0, 61.701658d0, 61.685215d0, 61.663713d0, &
                               61.645487d0, 61.629331d0, 61.611282d0, 61.592836d0, &
                               61.575650d0, 61.558726d0 /)

   end subroutine InitializeRevEarnVars

!===============================================================================

end module EconRevEarnVarMod