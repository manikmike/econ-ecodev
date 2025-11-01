module EconModSol1VarMod

   use EconParMod
   use EconLibMod
   use ifwbase
   include "OcactFortLib.inc"
   implicit none
   public

   character (len = LIST_ITEM_LEN), dimension(0:MAX_LIST_SIZE) :: assumpt
   logical :: usingGUI = .false.
   integer :: selnum   ! Number model selected to solve                     
   character (len=128) :: per1, per2, version
   integer :: per1a, per2a
   integer :: qtr1a, qtr2a   
   character (len=128) :: sq, sa

   ! Starting quarter when solving model
   integer :: startQtr
   
   ! Ending quarter when solving model
   integer :: endQtr
   
   ! Last quarter of historical data
   integer :: lastDataQtr

   ! Last full year of quarterly historical data
   integer :: lastDataYr

   ! Starting year when solving model
   integer :: startYr
   
   ! Ending year when solving model
   integer :: endYr

   ! Workfile identifiers
   integer :: BKL, AFILE, ADFILE, DFILE, OFILE1, OFILE2, OFILE3, OFILE4, &
              OTLFILE, BKDO1, BKDR1, CPSO_NILF, CE3750, &
              CPSFILE, CNIPOPDATA, MEF, ESF, WSQPROJTR, &
              LFPR_PROJ
   
   character (len = 128) :: afileName, adfileName, efileName, dfileName, &
                            ofile1Name, ofile2Name, ofile3Name
 
   ! Employment
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: e
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: e_fe
   real (kind = 8), dimension(MAX_QTR) :: e_fex, e_fexx
   real (kind = 8), dimension(MAX_QTR) :: edmil, ee
   
   ! Labor force participation rate
   real (kind = 8), dimension(0:2,0:34,MAX_QTR) :: p
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: p_asa
   real (kind = 8), dimension(0:2,0:34,MAX_QTR) :: p_p  ! preliminary
   real (kind = 8), dimension(1:1,2:7,MAX_QTR) :: tp   
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: p_fe
   real (kind = 8), dimension(1:1,2:7,MAX_QTR) :: tp_fe
   real (kind = 8), dimension(1:1,13:15,MAX_QTR) :: pl   
   real (kind = 8), dimension(1:2,2:15,MAX_QTR) :: pl_fe
   real (kind = 8), dimension(1:2,55:100,MAX_QTR) :: psy
   real (kind = 8), dimension(1:2,55:100,MAX_QTR) :: psy_p ! preliminary
   real (kind = 8), dimension(1:2,55:74,MAX_QTR) :: psye_de
   real (kind = 8), dimension(1:2,55:74,MAX_QTR) :: psy_dm
   real (kind = 8), dimension(55:74,MAX_QTR) :: psy_coh48
   real (kind = 8), dimension(1:2,55:74,MAX_QTR) :: psy_add
   real (kind = 8), dimension(1:2,3:9,3,MAX_QTR) :: pms, pms_p ! preliminary
   real (kind = 8), dimension(2:2,8:9,3,MAX_QTR) :: pms_m
   real (kind = 8), dimension(2:2,4:8,1,MAX_QTR) :: pmsl_fe
   real (kind = 8), dimension(2:2,3:9,3,MAX_QTR) :: pmsc6u, pmsc6u_p ! preliminary
   real (kind = 8), dimension(2:2,3:9,3,MAX_QTR) :: pmsc6u_m
   real (kind = 8), dimension(2:2,3:3,1:1,MAX_QTR) :: pmsc6l_fe
   real (kind = 8), dimension(2:2,3:7,3,MAX_QTR) :: pmsnc6, pmsnc6_p ! preliminary
   real (kind = 8), dimension(2:2,3:7,3,MAX_QTR) :: pmsnc6_m
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: pt
   real (kind = 8), dimension(1:2,10:13,MAX_QTR) :: px
   real (kind = 8), dimension(1:2,MAX_QTR) :: p80o, p80o_p
   ! Additional add factors to calibrate LC and RU to E
   real (kind = 8), dimension(1:2,1:9,MAX_QTR) :: p_add2
   real (kind = 8), dimension(1:2,55:74,MAX_QTR) :: psy_add2
      
   ! Unemployment rate
   real (kind = 8), dimension(0:2,0:34,MAX_QTR) :: ru = 0d0
   real (kind = 8), dimension(0:2,0:34,MAX_QTR) :: ru_p ! preliminary
   real (kind = 8), dimension(0:2,0:34,MAX_QTR) :: ru_p_add ! preliminary
   real (kind = 8), dimension(0:2,0:34,MAX_QTR) :: ru_fe
   real (kind = 8), dimension(MAX_QTR) :: ru_fex
   real (kind = 8), dimension(0:2,0:0,MAX_QTR) :: ru_r
   real (kind = 8), dimension(0:2,0:0,MAX_QTR) :: ru_asa, ru_asa_p ! preliminary
      
   ! Civilian labor force
   real (kind = 8), dimension(0:2,0:34,MAX_QTR) :: lc
   real (kind = 8), dimension(0:2,0:0,MAX_QTR) :: lc_p ! preliminary
   real (kind = 8), dimension(0:2,55:100,MAX_QTR) :: lcsy
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: lc_by
   ! Civilian labor force at full employment
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: lc_fe = 0d0
   real (kind = 8), dimension(MAX_QTR) :: lc_fex, lc_fexx, lc_fex_adj
   real (kind = 8), dimension(2:2,28:28,2:2,MAX_QTR) :: lcmsc6u
   
   ! Current Population Survey (CPS) Population
   ! Civilian Noninstitutional
   real (kind = 8), dimension(0:2,0:34,MAX_QTR) :: n
   real (kind = 8), dimension(0:2,0:34,3,MAX_QTR) :: nms
   real (kind = 8), dimension(1:2,55:100,MAX_QTR) :: nisy
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: n_by
   ! Military
   !real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: m
   real (kind = 8), dimension(1,2:7,0:3,MAX_QTR) :: mms
   ! Ratios
   real (kind = 8), dimension(0:2,0:27,3,MAX_QTR) :: rnms
   
   ! Differences in civilian labor force at full employment
   real (kind = 8), dimension(0:2,0:13,MAX_QTR) :: dlc_fe = 0d0
   
   ! Differences in labor force participation at full employment
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: dp_fe
   
   ! Differences in unemployment at full employment
   real (kind = 8), dimension(0:2,0:27,MAX_QTR) :: dru_fe

   ! Annualized versions of key endogenous variables
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: e_a
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: e_fe_a
   real (kind = 8), dimension(MAX_YR) :: edmil_a, ee_a
   real (kind = 8), dimension(MAX_YR) :: e_fex_a, e_fexx_a
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: lc_a, lc_p_a
   real (kind = 8), dimension(MAX_YR) :: lc_t_a
   real (kind = 8), dimension(0:2,55:100,MAX_YR) :: lcsy_a
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: p_a, p_p_a, p_t_a
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: p_fe_a
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: p_asa_a
   real (kind = 8), dimension(1:2,3:9,3,MAX_YR) :: pms_a, pms_p_a
   real (kind = 8), dimension(2:2,3:7,3,MAX_YR) :: pmsc6u_a, pmsc6u_p_a
   real (kind = 8), dimension(2:2,3:7,3,MAX_YR) :: pmsnc6_a, pmsnc6_p_a
   real (kind = 8), dimension(1:2,55:100,MAX_YR) :: psy_a, psy_p_a
   real (kind = 8), dimension(0:2,0:29,MAX_YR) :: pt_a   
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: ru_a, ru_p_a
   real (kind = 8), dimension(0:2,0:0,MAX_YR) :: ru_asa_a, ru_asa_p_a
   real (kind = 8), dimension(0:2,0:0,MAX_YR) :: ru_r_a
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: ru_fe_a
   real (kind = 8), dimension(MAX_YR) :: ru_fex_a, ru_fexx_a
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: lc_fe_a
   real (kind = 8), dimension(MAX_YR) :: lc_fex_a, lc_fexx_a
   real (kind = 8), dimension(0:2,0:29,MAX_YR) :: n_a
   real (kind = 8), dimension(0:2,0:29,MAX_YR) :: m_a
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: dp_fe_a
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: dru_fe_a
   real (kind = 8), dimension(1:2,MAX_YR) :: p80o_a, p80o_p_a
   
   ! Business cycle variables
   real (kind=8), dimension(MAX_QTR) :: gdp, avg_gdp
   real (kind=8), dimension(MAX_YR) :: gdp_a, avg_gdp_a
   real (kind=8), dimension(MAX_YR) :: gdpreal_a, prod_a, ahrs_a, hrs_a
   real (kind=8), dimension(MAX_QTR) :: gdpreal, prod, ahrs, hrs
   
   ! RTP, lagged RTP, and moving average RTP
   real (kind = 8), dimension(MAX_QTR) :: rtp, rtp_1, rtp_3, rtp_5
   real (kind = 8), dimension(MAX_QTR) :: rtp_2, rtp_6, rtp_10, rtp_4, &
                                          rtp_14

   ! First difference of RTP, lagged RTP, moving average RTP
   real (kind = 8), dimension(MAX_QTR) :: drtp, drtp_1, drtp_2, drtp_3, drtp_5
   real (kind = 8), dimension(MAX_QTR) :: mvavg5_rtp_2, mvavg5_rtp_6, &
                                          mvavg5_rtp_10, mvavg5_rtp_4, &
                                          mvavg5_rtp_14, mvavg4_rtp_1, &
                                          mvavg9_rtp_10, mvavg4_rtp_2
   real (kind = 8), dimension(MAX_QTR) :: dmvavg5_rtp_6
   real (kind = 8), dimension(MAX_QTR) :: rradj_m62, drradj_m62
   real (kind = 8), dimension(MAX_QTR) :: rradj_m63, drradj_m63   
   real (kind = 8), dimension(MAX_QTR) :: rradj_m64, drradj_m64      
   real (kind = 8), dimension(MAX_QTR) :: rradj_m65, drradj_m65 
   real (kind = 8), dimension(MAX_QTR) :: rradj_m66, drradj_m66
   real (kind = 8), dimension(MAX_QTR) :: rradj_m67, drradj_m67
   real (kind = 8), dimension(MAX_QTR) :: rradj_m68, drradj_m68
   real (kind = 8), dimension(MAX_QTR) :: rradj_m69, drradj_m69
   real (kind = 8), dimension(MAX_QTR) :: rradj_m70, drradj_m70
   real (kind = 8), dimension(MAX_QTR) :: rradj_f62, rradj_f63, rradj_f64
   real (kind = 8), dimension(MAX_QTR) :: rradj_f65, rradj_f66, rradj_f67
   real (kind = 8), dimension(MAX_QTR) :: rradj_f68, rradj_f69, rradj_f70
   real (kind = 8), dimension(MAX_QTR) :: rradj_f62_4, rradj_f63_4, rradj_f65_4
   real (kind = 8), dimension(MAX_QTR) :: rradj_f66_4, rradj_f67_4, rradj_f68_4
   real (kind = 8), dimension(MAX_QTR) :: rradj_f69_4   
   real (kind = 8), dimension(MAX_QTR) :: retest64u, dretest64u, retest64u_4
   real (kind = 8), dimension(MAX_QTR) :: retest65, dretest65, retest65_4
   real (kind = 8), dimension(MAX_QTR) :: retest66, retest66_4, dretest66
   real (kind = 8), dimension(MAX_QTR) :: retest67, retest67_4, dretest67
   real (kind = 8), dimension(MAX_QTR) :: retest68, retest68_4, dretest68
   real (kind = 8), dimension(MAX_QTR) :: retest69, retest69_4, dretest69
   real (kind = 8), dimension(MAX_QTR) :: drradj_f63_minus_f62_4
   real (kind = 8), dimension(MAX_QTR) :: drradj_f64_minus_f63_4
   real (kind = 8), dimension(MAX_QTR) :: drradj_f66_minus_f65_4   
   real (kind = 8), dimension(MAX_QTR) :: drradj_f67_minus_f66_4
   real (kind = 8), dimension(MAX_QTR) :: drradj_f68_minus_f67_4
   real (kind = 8), dimension(MAX_QTR) :: drradj_f69_minus_f68_4
   real (kind = 8), dimension(MAX_QTR) :: drradj_f70_minus_f69_4   
   real (kind = 8), dimension(MAX_QTR) :: dmvavg5_rtp_6_minus_10
   real (kind = 8), dimension(MAX_QTR) :: dretest64u_minus_64u_4
   real (kind = 8), dimension(MAX_QTR) :: dretest_66_minus_65_4
   real (kind = 8), dimension(MAX_QTR) :: dretest_67_minus_66_4
   real (kind = 8), dimension(MAX_QTR) :: dretest_68_minus_67_4
   real (kind = 8), dimension(MAX_QTR) :: dretest_69_minus_68_4   
   
   ! Other series needed to solve model
   real (kind = 8), dimension(MAX_QTR) :: diffdonm1617, ddiffdonm1617
   real (kind = 8), dimension(MAX_QTR) :: ram1819, dram1819
   real (kind = 8), dimension(MAX_QTR) :: rnf1819s, drnf1819s
   real (kind = 8), dimension(MAX_QTR) :: dropout1012m, ddropout1012m
   real (kind = 8), dimension(MAX_QTR) :: rm1819m, drm1819m
   real (kind = 8), dimension(MAX_QTR) :: pf1819_fe, pf1819_fe_10
   real (kind = 8), dimension(MAX_QTR) :: pf1619_94m_te, pf1619_94m_te_10
   real (kind = 8), dimension(MAX_QTR) :: pf1619_90c_te, pf1619_90c_te_10
   real (kind = 8), dimension(MAX_QTR) :: rf2024d, pfw2024nm, if2024nmc6u
   real (kind = 8), dimension(MAX_QTR) :: rf2024nmc6u
   real (kind = 8), dimension(MAX_QTR) :: if2024msc6u
   real (kind = 8), dimension(MAX_QTR) :: rf2024msc6u
   real (kind = 8), dimension(MAX_QTR) :: pfw2024ma, if2024mac6u
   real (kind = 8), dimension(MAX_QTR) :: rf2024mac6u
   real (kind = 8), dimension(MAX_QTR) :: rf2024nm, rf2024ms, rf2024ma
   real (kind = 8), dimension(MAX_QTR) :: nf2024nm, nf2024ms, nf2024ma
   real (kind = 8), dimension(MAX_QTR) :: nf2024nmc6u, nf2024nmnc6
   real (kind = 8), dimension(MAX_QTR) :: nf2024msc6u, nf2024msnc6
   real (kind = 8), dimension(MAX_QTR) :: nf2024mac6u, nf2024manc6
   real (kind = 8), dimension(MAX_QTR) :: pf2024_fe_14
   real (kind = 8), dimension(MAX_QTR) :: pf2024_90c_te, pf2024_90c_te_14
   real (kind = 8), dimension(MAX_QTR) :: pf2024_94m_te, pf2024_94m_te_14
   real (kind = 8), dimension(MAX_QTR) :: rf2024d_14
   real (kind = 8), dimension(MAX_QTR) :: pf2024_adjwf, pf2024_adjwf_14
   real (kind = 8), dimension(MAX_QTR) :: pf2024nmc6u_14
   real (kind = 8), dimension(MAX_QTR) :: pf2024_94c_te, pf2024_94c_te_14
   real (kind = 8), dimension(MAX_QTR) :: pf2024c_94m_te, pf2024c_94m_te_14            
   real (kind = 8), dimension(MAX_QTR) :: nf2024nmc6u_14
   real (kind = 8), dimension(MAX_QTR) :: rf2529d, pfw2529nm, if2529nmc6u            
   real (kind = 8), dimension(MAX_QTR) :: rf2529nmc6u, rf2529msc6u, rf2529mac6u
   real (kind = 8), dimension(MAX_QTR) :: if2529msc6u
   real (kind = 8), dimension(MAX_QTR) :: pfw2529ma, if2529mac6u  
   real (kind = 8), dimension(MAX_QTR) :: rf2529nm, rf2529ms, rf2529ma
   real (kind = 8), dimension(MAX_QTR) :: nf2529nm, nf2529ms, nf2529ma
   real (kind = 8), dimension(MAX_QTR) :: nf2529nmc6u, nf2529nmnc6
   real (kind = 8), dimension(MAX_QTR) :: nf2529msc6u, nf2529msnc6
   real (kind = 8), dimension(MAX_QTR) :: nf2529mac6u, nf2529manc6
   real (kind = 8), dimension(MAX_QTR) :: pf2529_fe_14, pf2529ms_1
   real (kind = 8), dimension(MAX_QTR) :: pf2554_90c_te, pf2554_90c_te_14
   real (kind = 8), dimension(MAX_QTR) :: pf2554_94m_te, pf2554_94m_te_14
   real (kind = 8), dimension(MAX_QTR) :: rf2529d_14
   real (kind = 8), dimension(MAX_QTR) :: pf2529_adjwf, pf2529_adjwf_14
   real (kind = 8), dimension(MAX_QTR) :: pf2554_94c_te, pf2554_94c_te_14
   real (kind = 8), dimension(MAX_QTR) :: pf2529nm_14, nf2529nmc6u_14
   real (kind = 8), dimension(MAX_QTR) :: nf2529nmnc6_14, nf2529nm_14
   real (kind = 8), dimension(MAX_QTR) :: pf2554c_94m_te_14
   real (kind = 8), dimension(MAX_QTR) :: rf3034d, pfw3034nm, if3034nmc6u
   real (kind = 8), dimension(MAX_QTR) :: if3034msc6u
   real (kind = 8), dimension(MAX_QTR) :: rf3034nmc6u, rf3034msc6u, rf3034mac6u 
   real (kind = 8), dimension(MAX_QTR) :: pfw3034ma, if3034mac6u
   real (kind = 8), dimension(MAX_QTR) :: rf3034nm, rf3034ms, rf3034ma                   
   real (kind = 8), dimension(MAX_QTR) :: nf3034nm, nf3034ms, nf3034ma
   real (kind = 8), dimension(MAX_QTR) :: nf3034nmc6u, nf3034nmnc6
   real (kind = 8), dimension(MAX_QTR) :: nf3034msc6u, nf3034msnc6
   real (kind = 8), dimension(MAX_QTR) :: nf3034mac6u, nf3034manc6
   real (kind = 8), dimension(MAX_QTR) :: pf3034_fe_14, pf3034ms_1
   real (kind = 8), dimension(MAX_QTR) :: rf3034d_14
   real (kind = 8), dimension(MAX_QTR) :: pf3034_adjwf, pf3034_adjwf_14
   real (kind = 8), dimension(MAX_QTR) :: pf3034nm_14, nf3034nmc6u_14
   real (kind = 8), dimension(MAX_QTR) :: nf3034nmnc6_14, nf3034nm_14
   real (kind = 8), dimension(MAX_QTR) :: rf3539d, pfw3539nm
   real (kind = 8), dimension(MAX_QTR) :: rf3539nmc6u, rf3539msc6u, rf3539mac6u
   real (kind = 8), dimension(MAX_QTR) :: if3539msc6u
   real (kind = 8), dimension(MAX_QTR) :: rf3539nm, rf3539ms, rf3539ma                         
   real (kind = 8), dimension(MAX_QTR) :: nf3539nm, nf3539ms, nf3539ma
   real (kind = 8), dimension(MAX_QTR) :: nf3539nmc6u, nf3539nmnc6
   real (kind = 8), dimension(MAX_QTR) :: nf3539msc6u, nf3539msnc6
   real (kind = 8), dimension(MAX_QTR) :: nf3539mac6u, nf3539manc6
   real (kind = 8), dimension(MAX_QTR) :: pf3539_fe_14, pf3539ms_1
   real (kind = 8), dimension(MAX_QTR) :: rf3539d_14
   real (kind = 8), dimension(MAX_QTR) :: pf3539_adjwf, pf3539_adjwf_14
   real (kind = 8), dimension(MAX_QTR) :: pf3539nm_14, nf3539nmc6u_14
   real (kind = 8), dimension(MAX_QTR) :: nf3539nmnc6_14, nf3539nm_14
   real (kind = 8), dimension(MAX_QTR) :: rf4044d, ifw4044nmc6u
   real (kind = 8), dimension(MAX_QTR) :: rf4044nmc6u, rf4044msc6u, rf4044mac6u
   real (kind = 8), dimension(MAX_QTR) :: if4044nmc6u
   real (kind = 8), dimension(MAX_QTR) :: rf4044nm, rf4044ms, rf4044ma                         
   real (kind = 8), dimension(MAX_QTR) :: nf4044nm, nf4044ms, nf4044ma
   real (kind = 8), dimension(MAX_QTR) :: nf4044nmc6u, nf4044nmnc6
   real (kind = 8), dimension(MAX_QTR) :: nf4044msc6u, nf4044msnc6
   real (kind = 8), dimension(MAX_QTR) :: nf4044mac6u, nf4044manc6
   real (kind = 8), dimension(MAX_QTR) :: pf4044_fe_14, pf4044ms_1
   real (kind = 8), dimension(MAX_QTR) :: rf4044d_14
   real (kind = 8), dimension(MAX_QTR) :: pf4044_adjwf, pf4044_adjwf_14
   real (kind = 8), dimension(MAX_QTR) :: pf4044nm_14, nf4044nmc6u_14
   real (kind = 8), dimension(MAX_QTR) :: nf4044nmnc6_14, nf4044nm_14
   real (kind = 8), dimension(MAX_QTR) :: rf4549d
   real (kind = 8), dimension(MAX_QTR) :: rf4549nm, rf4549ms, rf4549ma
   real (kind = 8), dimension(MAX_QTR) :: nf4549nm, nf4549ms, nf4549ma            
   real (kind = 8), dimension(MAX_QTR) :: pf4549_fe_14      
   real (kind = 8), dimension(MAX_QTR) :: rf4549d_14
   real (kind = 8), dimension(MAX_QTR) :: pf4549nm_14, nf4549nm_14
   real (kind = 8), dimension(MAX_QTR) :: rf5054d
   real (kind = 8), dimension(MAX_QTR) :: rf5054nm, rf5054ms, rf5054ma
   real (kind = 8), dimension(MAX_QTR) :: nf5054nm, nf5054ms, nf5054ma            
   real (kind = 8), dimension(MAX_QTR) :: pf5054_fe_8      
   real (kind = 8), dimension(MAX_QTR) :: rf5054d_8
   real (kind = 8), dimension(MAX_QTR) :: pf5054nm_8, nf5054nm_8
   real (kind = 8), dimension(MAX_QTR) :: pf2554_90c_te_8, pf2554_94m_te_8
   real (kind = 8), dimension(MAX_QTR) :: pf7074_20, pf7579_20, pf8084_20
   real (kind = 8), dimension(MAX_QTR) :: pm2554_a90c, pm2554_a94m      
   real (kind = 8), dimension(MAX_QTR) :: pm2554_a90c_8, pm2554_a94m_8
   
   real (kind = 8), dimension(MAX_QTR) :: rf55d, rf55d_4, pf55_4
   real (kind = 8), dimension(MAX_QTR) :: rf56d, rf56d_4, pf56_4
   real (kind = 8), dimension(MAX_QTR) :: rf57d, rf57d_4, pf57_4
   real (kind = 8), dimension(MAX_QTR) :: rf58d, rf58d_4, pf58_4
   real (kind = 8), dimension(MAX_QTR) :: rf59d, rf59d_4, pf59_4
   real (kind = 8), dimension(MAX_QTR) :: rf60d, rf60d_4, pf60_4
   real (kind = 8), dimension(MAX_QTR) :: rf61d, rf61d_4, pf61_4
   real (kind = 8), dimension(MAX_QTR) :: rf62d, rf62d_4, pf62_4
   real (kind = 8), dimension(MAX_QTR) :: rf63d, rf63d_4, pf63_4
   real (kind = 8), dimension(MAX_QTR) :: rf64d, rf64d_4, pf64_4
   real (kind = 8), dimension(MAX_QTR) :: rf62d_5, pf62_5
         
   real (kind = 8), dimension(MAX_QTR) :: rm5054d, rm5054d_8
   real (kind = 8), dimension(MAX_QTR) :: rm5054nm, rm5054ms, rm5054ma
   real (kind = 8), dimension(MAX_QTR) :: nm5054nm, nm5054ms, nm5054ma
   real (kind = 8), dimension(MAX_QTR) :: pm5054_fe_8                  
     
   real (kind = 8), dimension(MAX_QTR) :: rm55d, rm55d_4, pm55_4
   real (kind = 8), dimension(MAX_QTR) :: rm56d, rm56d_4, pm56_4
   real (kind = 8), dimension(MAX_QTR) :: rm57d, rm57d_4, pm57_4
   real (kind = 8), dimension(MAX_QTR) :: rm58d, rm58d_4, pm58_4
   real (kind = 8), dimension(MAX_QTR) :: rm59d, rm59d_4, pm59_4

   real (kind = 8), dimension(MAX_QTR) :: rm61d, rm62d, rm63d, rm64d
   real (kind = 8), dimension(MAX_QTR) :: rm60d, rm60d_4, pm60_4
   real (kind = 8), dimension(MAX_QTR) :: rm61d_4, pm61_4
   real (kind = 8), dimension(MAX_QTR) :: rm62d_1
   
   real (kind = 8), dimension(MAX_QTR) :: rm2024d, rnm2024s
   real (kind = 8), dimension(MAX_QTR) :: rmm2024nm, rmm2024ms, rmm2024ma
   real (kind = 8), dimension(MAX_QTR) :: rm2024nm, rm2024ms, rm2024ma   
   real (kind = 8), dimension(MAX_QTR) :: pm2024_a94c, pm2024_a94m
   
   real (kind = 8), dimension(MAX_QTR) :: rm2529d
   real (kind = 8), dimension(MAX_QTR) :: rmm2529nm, rmm2529ms, rmm2529ma
   real (kind = 8), dimension(MAX_QTR) :: pm2554_a94c
   real (kind = 8), dimension(MAX_QTR) :: pf2554_a94c, pf2554_a94m
   real (kind = 8), dimension(MAX_QTR) :: pf2554_a94c_1, pf2554_a94m_1   
   real (kind = 8), dimension(MAX_QTR) :: mvavg4_pf2529ag
   
   real (kind = 8), dimension(MAX_QTR) :: rm3034d
   real (kind = 8), dimension(MAX_QTR) :: rmm3034nm, rmm3034ms, rmm3034ma
   real (kind = 8), dimension(MAX_QTR) :: mvavg4_pf3034ag   

   real (kind = 8), dimension(MAX_QTR) :: rm3539d
   real (kind = 8), dimension(MAX_QTR) :: rmm3539nm, rmm3539ms, rmm3539ma
   real (kind = 8), dimension(MAX_QTR) :: mvavg4_pf3539ag      

   real (kind = 8), dimension(MAX_QTR) :: rm4044d
   real (kind = 8), dimension(MAX_QTR) :: rmm4044nm, rmm4044ms, rmm4044ma
   real (kind = 8), dimension(MAX_QTR) :: mvavg4_pf4044ag     

   real (kind = 8), dimension(MAX_QTR) :: rm4549d
   real (kind = 8), dimension(MAX_QTR) :: rmm4549nm, rmm4549ms, rmm4549ma
   real (kind = 8), dimension(MAX_QTR) :: mvavg4_pf4549ag
   
   real (kind = 8), dimension(MAX_QTR) :: pm7074_20, pm7074_16, mvavg4_pm7074_20
   real (kind = 8), dimension(MAX_QTR) :: pm65o_a94mm, pm65o_a94mm_16
   real (kind = 8), dimension(MAX_QTR) :: pm7579_20, pm7579_16, mvavg4_pm7579_20
   real (kind = 8), dimension(MAX_QTR) :: pm8084_16, pm8084_20, mvavg4_pm8084_20

   real (kind = 8), dimension(62:69,MAX_QTR) :: pot_et_txrt
   real (kind = 8), dimension(1:2,65:66,MAX_QTR) :: r_di_eff
   !real (kind = 8), dimension(1:2,MAX_QTR) :: dp75o_fe
   real (kind = 8), dimension(MAX_QTR) :: tr_pf2024nmnc6
   real (kind = 8), dimension(MAX_QTR) :: tr_pf2529nmnc6 
   real (kind = 8), dimension(MAX_QTR) :: if3539mac6u
   real (kind = 8), dimension(MAX_QTR) :: if4044msc6u 
   real (kind = 8), dimension(MAX_QTR) :: if4044mac6u     
   real (kind = 8), dimension(MAX_QTR) :: pf4549e_de
   real (kind = 8), dimension(MAX_QTR) :: rf4549mscu6
   real (kind = 8), dimension(MAX_QTR) :: rf4549macu6
   real (kind = 8), dimension(MAX_QTR) :: pf5054e_de
   real (kind = 8), dimension(MAX_QTR) :: tr_pf5054ms
   real (kind = 8), dimension(MAX_QTR) :: rf5054mscu6
   real (kind = 8), dimension(MAX_QTR) :: rf5054macu6

   ! These were added for the new labor force model (modsol1.08a)
   ! This model was never implemented
   real (kind = 8), dimension(MAX_QTR) :: year
   real (kind = 8), dimension(MAX_QTR) :: rm1617d, rm1819d
   real (kind = 8), dimension(MAX_QTR) :: mvavg4_rtp_5
   real (kind = 8), dimension(MAX_QTR) :: rf1617d, rf1819d
   real (kind = 8), dimension(MAX_QTR) :: rf1617cu6, rf1819cu6
   real (kind = 8), dimension(MAX_QTR) :: rf2024cu6, rf2024c617
   real (kind = 8), dimension(MAX_QTR) :: rf2529cu6, rf2529c617
   real (kind = 8), dimension(MAX_QTR) :: rf3034cu6, rf3034c617
   real (kind = 8), dimension(MAX_QTR) :: rf5054cu6, rf5054c617
   real (kind = 8), dimension(MAX_QTR) :: rf5559cu6, rf5559c617
   
   ! These were added for the new labor force model (modsol1.09a)
   real (kind = 8), dimension(1:2,1:23,MAX_QTR) :: b1_d
   real (kind = 8), dimension(MAX_QTR) :: b2_2064di
   real (kind = 8), dimension(1:2,1:9,MAX_QTR) :: r_di
   real (kind = 8), dimension(1:2,55:64,MAX_QTR) :: rsy_di
   real (kind = 8), dimension(1:2,1:9,MAX_QTR) :: tr_p
   real (kind = 8), dimension(MAX_QTR) :: rf5054c6o
   
   ! Adjustment for unemployment rates
   double precision, dimension(MAX_QTR) :: ru_asa_adj

   ! Adjustment to distribute total labor force addfactor
   real (kind=8), dimension(0:2, 0:31, MAX_QTR) :: lcadj
   real (kind=8), dimension(0:2, 55:79, MAX_QTR) :: lcadjsy
   
contains

!===============================================================================

   subroutine InitializeModSol1Vars()
   
      integer :: sex, ageGrp, marStat, age
   
      call SetWorkfileVerboseMode(.true.)
      
      call FetchSeries(AFILE, "GDP.Q", gdp)
      call FetchSeries(AFILE, "GDP.A", gdp_a)
      call FetchSeries(AFILE, "EDMIL.Q", edmil)
      call FetchSeries(AFILE, "EDMIL.A", edmil_a)
      call FetchSeries(AFILE, trim(REAL_GDP_SERIES)//".Q", gdpreal(:)) 
      call FetchSeries(AFILE, trim(REAL_GDP_SERIES)//".A", gdpreal_a)
      call FetchSeries(AFILE, "PROD.Q", prod)
      call FetchSeries(AFILE, "PROD.A", prod_a)
      call FetchSeries(AFILE, "AHRS.Q", ahrs)
      call FetchSeries(AFILE, "AHRS.A", ahrs_a)
      call FetchSeries(AFILE, "HRS.Q", hrs)
      call FetchSeries(AFILE, "HRS.A", hrs_a)
      
      call FetchSeries(AFILE, "RTP.Q", rtp)
      call lag(rtp, rtp_1)
      call lagn(rtp, rtp_2, 2)
      call lagn(rtp, rtp_3, 3)  
      call lagn(rtp, rtp_5, 5)  
      call diff(rtp, drtp)
      call diff(rtp_1, drtp_1)
      call diff(rtp_2, drtp_2)
      call diff(rtp_3, drtp_3)
      
      ! Set the correct starting point
      call FetchSeries(DFILE, "RM1617.Q", ru(1,1,:))       
      lastDataQtr = sample(2)
      lastDataYr = lastDataQtr / 4 - 1
      
      ! Get Historical Unemployment Rates (RU)
      do sex = 1, 2
         do ageGrp = 1, 15
            call FetchSeries(DFILE, "R"//trim(sexLabel(sex))// &
               trim(ageGrpLabel2(ageGrp))//".Q",ru(sex, ageGrp, :))
            ru_p(sex,ageGrp,:) = ru(sex,ageGrp,:)
         end do
      end do
      call FetchSeries(ADFILE, "RU_ASA_ADJ.Q", ru_asa_adj)
      call FetchSeries(DFILE, "RF5054CU6.Q", rf5054cu6(:))
      call FetchSeries(DFILE, "RF5054C6O.Q", rf5054c6o(:))

      ! Get  Unemployment Rate Addfactors
      do sex = 1, 2
         do ageGrp = 1, 14
            call FetchSeries(ADFILE, "R"//trim(sexLabel(sex))// &
               trim(ageGrpLabel2(ageGrp))//"_P.ADD_Q",ru_p_add(sex, ageGrp, :))
         end do
      end do
      
      ! Get Historical Civilian Labor Force (LC)
      ! and Set Base Year Labor Force for age-adjustment (lc_by)
      do sex = 1, 2
         do ageGrp = 1, 13
            call FetchSeries(DFILE, &
               "L"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//".Q", &
               lc(sex, ageGrp, :))
            lc_by(sex, ageGrp, :) = &
               sum(lc(sex, ageGrp, LC_BASE_QTR:LC_BASE_QTR+3))/4d0
            if (ageGrp == 13) then
               call FetchSeries(DFILE, &
                  "L"//trim(sexLabel(sex))//trim(ageGrpLabel(17))//".Q", &
                  lc(sex, 17, :)) 
               lc_by(sex, 17, :) = &
                  sum(lc(sex, 17, LC_BASE_QTR:LC_BASE_QTR+3))/4d0
            end if
         end do
         call FetchSeries(DFILE, "LC"//trim(sexLabel(sex))// ".Q", lc(sex, 0, :))
         lc_by(sex, 0, :) = sum(lc(sex, 0, LC_BASE_QTR:LC_BASE_QTR+3))/4d0         
      end do
      call FetchSeries(DFILE, "LC.Q", lc(0, 0, :))
      lc_by(0, 0, :) = sum(lc(0, 0, LC_BASE_QTR:LC_BASE_QTR+3))/4d0
      
      ! Get Historical Civilian Labor Force (LC)
      ! and Set Base Year Labor Force for age-adjustment (lc_by)
      do sex = 1, 2
         do ageGrp = 1, 13
            call FetchSeries(DFILE, &
               "L"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//".Q", &
               lc(sex, ageGrp, :))
            lc_by(sex, ageGrp, :) = &
               sum(lc(sex, ageGrp, LC_BASE_QTR:LC_BASE_QTR+3))/4d0
            if (ageGrp == 13) then
               call FetchSeries(DFILE, &
                  "L"//trim(sexLabel(sex))//trim(ageGrpLabel(17))//".Q", &
                  lc(sex, 17, :)) 
               lc_by(sex, 17, :) = &
                  sum(lc(sex, 17, LC_BASE_QTR:LC_BASE_QTR+3))/4d0
            end if
         end do
         call FetchSeries(DFILE, "LC"//trim(sexLabel(sex))// ".Q", lc(sex, 0, :))
         lc_by(sex, 0, :) = sum(lc(sex, 0, LC_BASE_QTR:LC_BASE_QTR+3))/4d0         
      end do
      call FetchSeries(DFILE, "LC.Q", lc(0, 0, :))
      lc_by(0, 0, :) = sum(lc(0, 0, LC_BASE_QTR:LC_BASE_QTR+3))/4d0
      
      
      ! Get Historical Unemployment Rates at Full Employment(RU_FE)
      do sex = 1, 2
         do ageGrp = 1, 14
            call FetchSeries(DFILE, "R"//trim(sexLabel(sex))// &
               trim(ageGrpLabel2(ageGrp))//"_FE.Q",ru_fe(sex, ageGrp, :))      
         end do
      end do
      
      ! Get Historical Labor Force Participation Rates (P*)
      ! Get Disability Rates (R*DI)
      ! (TR_P*) 
      do sex = 1, 2
        do ageGrp = 1,9
           call FetchSeries(DFILE, "P"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".Q", p(sex,ageGrp,:))
           call FetchSeries(OFILE2, "R"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//"DI.Q", r_di(sex,ageGrp,:))
           if (sex == 1 .and. ageGrp <= 5 &
               .or. sex == 2 .and. ageGrp <= 2) call FetchSeries(DFILE, "TR_P"//trim(sexLabel(sex))// &
               trim(ageGrpLabel2(ageGrp))//".Q", tr_p(sex,ageGrp,:))
        end do
      end do
      
      ! Other oddly named mnemonics (B1_* )
      do sex = 1, 2
         do ageGrp = 1,13
           if (ageGrp < 4 .or. ageGrp > 9) then
              if (ageGrp /= 13) then
                 call FetchSeries(DFILE, "B1_"//trim(sexLabel(sex))//&
                    trim(ageGrpLabel(ageGrp))//"D.Q", b1_d(sex,ageGrp,:))
              else
                 call FetchSeries(DFILE, "B1_"//trim(sexLabel(sex))//&
                    trim(ageGrpLabel(23))//"D.Q", b1_d(sex,23,:))
              end if
           else  ! some are 10-year age groups
              if (mod(ageGrp,2) == 0) then
                 call FetchSeries(DFILE, "B1_"//trim(sexLabel(sex))//&
                    trim(ageGrpLabel(ageGrp/2+16))// &
                    "D.Q", b1_d(sex,ageGrp,:))
              else
                 b1_d(sex,ageGrp,:) = b1_d(sex,ageGrp-1,:)
              end if
           end if
         end do
      end do
      
      ! Get coefficient for PM1617 and PM1819
      call FetchSeries(DFILE, "B2_2064DI.Q", b2_2064di)
      
      ! Civilian Noninstitutional Population
      do ageGrp = 1, 34
         if (ageGrp == 28 .or. ageGrp == 29) cycle
         do sex = 1, 2
            call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               &".Q", n(sex,ageGrp,:))
            if (ageGrp <= 9) then
               do marStat = 1, 3
                  call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
                     &trim(marStatLabel(marStat)) &
                     &// ".Q", nms(sex,ageGrp,marStat,:))
            end do
            end if
          end do
      end do
      call FetchSeries(DFILE, "NM80O.Q", n(1,31,:))
      call FetchSeries(DFILE, "NF80O.Q", n(2,31,:))
      
      ! Military
      do sex = 1, 2
         do ageGrp = 1, 10
            call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               "M.A", m_a(sex,ageGrp,:))
         end do
         call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//"1619M.A", m_a(sex,27,:))
         call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//"16OM.A", m_a(sex,24,:))
         call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//"2534M.A", m_a(sex,18,:))
         call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//"3544M.A", m_a(sex,19,:))
         call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//"4554M.A", m_a(sex,20,:))
      end do
      call FetchSeries(DFILE, "N16OM.A", m_a(0,24,:))
      
      ! Set Base Year Labor Force for age-adjustment (n_by)
      do sex = 1, 2
         do ageGrp = 1, 13
            n_by(sex, ageGrp, :) = &
               sum(n(sex, ageGrp, N_BASE_QTR:N_BASE_QTR+3))/4d0
            if (ageGrp == 13) then
               n_by(sex, 17, :) = &
                  sum(n(sex, 17, N_BASE_QTR:N_BASE_QTR+3))/4d0
            end if
         end do
         n_by(sex,24,:) = sum(n(sex, 24, N_BASE_QTR:N_BASE_QTR+3))/4d0  
      end do
      n_by(0,24,:) = sum(n(1:2,24, N_BASE_QTR:N_BASE_QTR+3))/4d0
      
      ! Labor Force Participation Rates by single-year
      do age = 55, 74
         do sex = 1, 2
            call FetchSeries(DFILE, "P"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".Q", psy(sex,age,:))

            call FetchSeries(DFILE, "P"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//"E_DE.Q", psye_de(sex,age,:))

            call FetchSeries(DFILE, "P"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//"_DM.Q", psy_dm(sex,age,:))
               
         end do
         call FetchSeries(DFILE, "PF"//&
            trim(IntToAsc(age))//"COH48.Q", psy_coh48(age,:))
      end do

      ! Disability rates by single-year of age
      do age = 55, 64
         do sex = 1, 2
            call FetchSeries(OFILE2, "R"//trim(sexLabel(sex))//&
               trim(IntToAsc(age))//"DI.Q", rsy_di(sex,age,:))
            
         end do
      end do
      
      do sex = 1,2 
         do age = 65,66
            call FetchSeries(DFILE, &
               "R"//trim(sexLabel(sex))//trim(IntToAsc(age))//"DI_EFF.Q", &
               r_di_eff(sex,age,:))
         end do
      end do
      
      ! POT_ET_EXRT_*
      do age = 62, 69
         call FetchSeries(DFILE,"POT_ET_TXRT_"//trim(IntToAsc(age))//".Q",pot_et_txrt(age,:))
      end do
      
      ! Replacement Rates (RRADJ)
      call FetchSeries(DFILE, "RRADJ_M62.Q", rradj_m62)
      call FetchSeries(DFILE, "RRADJ_M63.Q", rradj_m63)
      call FetchSeries(DFILE, "RRADJ_M64.Q", rradj_m64)
      call FetchSeries(DFILE, "RRADJ_M65.Q", rradj_m65)
      call FetchSeries(DFILE, "RRADJ_M66.Q", rradj_m66)
      call FetchSeries(DFILE, "RRADJ_M67.Q", rradj_m67)
      call FetchSeries(DFILE, "RRADJ_M68.Q", rradj_m68)
      call FetchSeries(DFILE, "RRADJ_M69.Q", rradj_m69)
      call FetchSeries(DFILE, "RRADJ_F62.Q", rradj_f62)
      call FetchSeries(DFILE, "RRADJ_F63.Q", rradj_f63)
      call FetchSeries(DFILE, "RRADJ_F64.Q", rradj_f64)
      call FetchSeries(DFILE, "RRADJ_F65.Q", rradj_f65)
      call FetchSeries(DFILE, "RRADJ_F66.Q", rradj_f66)
      call FetchSeries(DFILE, "RRADJ_F67.Q", rradj_f67)
      call FetchSeries(DFILE, "RRADJ_F68.Q", rradj_f68)
      call FetchSeries(DFILE, "RRADJ_F69.Q", rradj_f69)
      
      ! Add Factors
      do sex = 1, 2
         do age = 55, 74
            call FetchSeries(ADFILE, "P"//trim(sexLabel(sex))//trim(IntToAsc(age))//"_ADD.Q", psy_add(sex,age,:))
         end do
      end do
      
      ! Additional add factors to calibrate LC and RU to E
      do sex = 1, 2
         do ageGrp = 1, 9
            call FetchSeries(ADFILE, "P"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//"_ADD2.Q", p_add2(sex,ageGrp,:))
         end do
         do age = 55, 74
            call FetchSeries(ADFILE, "P"//trim(sexLabel(sex))//trim(IntToAsc(age))//"_ADD2.Q", psy_add2(sex,age,:))
         end do
      end do
      
      ! Labor Force Participation Rates by single-year of age
      do sex = 1, 2
         do age = 75, 79
            call FetchSeries(DFILE, "P"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".Q", psy(sex,age,:))
         end do
      end do
      call FetchSeries(DFILE, "PM80O.Q", p80o(1,:))
      call FetchSeries(DFILE, "PF80O.Q", p80o(2,:))
      
      ! Population by single-year of age
      do age = 55, 100
         do sex =1, 2
            call FetchSeries(DFILE, "N"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".Q", nisy(sex,age,:))
         end do
      end do

      ! Ratio of females with children under 6
      call FetchSeries(DFILE, "RF1617CU6.Q", rf1617cu6(:))
      call FetchSeries(DFILE, "RF1819CU6.Q", rf1819cu6(:))
      
      ! Labor Force Participation Rate by marital status and presence of children
      call FetchSeries(DFILE, "TR_PF2024NMNC6.Q", tr_pf2024nmnc6(:))
      call FetchSeries(DFILE, "IF2024NMC6U.Q", if2024nmc6u(:))
      call FetchSeries(DFILE, "IF2024MSC6U.Q", if2024msc6u(:))
      call FetchSeries(DFILE, "IF2024MAC6U.Q", if2024mac6u(:))

      call FetchSeries(DFILE, "NF2024NMC6U.Q", nf2024nmc6u(:))
      call FetchSeries(DFILE, "NF2024MSC6U.Q", nf2024msc6u(:))
      call FetchSeries(DFILE, "NF2024MAC6U.Q", nf2024mac6u(:))
      call FetchSeries(DFILE, "NF2024NMNC6.Q", nf2024nmnc6(:))
      call FetchSeries(DFILE, "NF2024MSNC6.Q", nf2024msnc6(:))
      call FetchSeries(DFILE, "NF2024MANC6.Q", nf2024manc6(:))
      
      call FetchSeries(DFILE, "TR_PF2529NMNC6.Q", tr_pf2529nmnc6(:))
      call FetchSeries(DFILE, "IF2529NMC6U.Q", if2529nmc6u(:))
      call FetchSeries(DFILE, "IF2529MSC6U.Q", if2529msc6u(:))
      call FetchSeries(DFILE, "IF2529MAC6U.Q", if2529mac6u(:))
      
      call FetchSeries(DFILE, "NF2529NMC6U.Q", nf2529nmc6u(:))
      call FetchSeries(DFILE, "NF2529MSC6U.Q", nf2529msc6u(:))
      call FetchSeries(DFILE, "NF2529MAC6U.Q", nf2529mac6u(:))
      call FetchSeries(DFILE, "NF2529NMNC6.Q", nf2529nmnc6(:))
      call FetchSeries(DFILE, "NF2529MSNC6.Q", nf2529msnc6(:))
      call FetchSeries(DFILE, "NF2529MANC6.Q", nf2529manc6(:))
      
      call FetchSeries(DFILE, "IF3034NMC6U.Q", if3034nmc6u(:))
      call FetchSeries(DFILE, "IF3034MSC6U.Q", if3034msc6u(:))
      call FetchSeries(DFILE, "IF3034MAC6U.Q", if3034mac6u(:))
      
      call FetchSeries(DFILE, "NF3034NMC6U.Q", nf3034nmc6u(:))
      call FetchSeries(DFILE, "NF3034MSC6U.Q", nf3034msc6u(:))
      call FetchSeries(DFILE, "NF3034MAC6U.Q", nf3034mac6u(:))
      call FetchSeries(DFILE, "NF3034NMNC6.Q", nf3034nmnc6(:))
      call FetchSeries(DFILE, "NF3034MSNC6.Q", nf3034msnc6(:))
      call FetchSeries(DFILE, "NF3034MANC6.Q", nf3034manc6(:))
      
      call FetchSeries(DFILE, "IF3539MSC6U.Q", if3539msc6u(:))
      call FetchSeries(DFILE, "IF3539MAC6U.Q", if3539mac6u(:))

      call FetchSeries(DFILE, "NF3539NMC6U.Q", nf3539nmc6u(:))
      call FetchSeries(DFILE, "NF3539MSC6U.Q", nf3539msc6u(:))
      call FetchSeries(DFILE, "NF3539MAC6U.Q", nf3539mac6u(:))
      call FetchSeries(DFILE, "NF3539NMNC6.Q", nf3539nmnc6(:))
      call FetchSeries(DFILE, "NF3539MSNC6.Q", nf3539msnc6(:))
      call FetchSeries(DFILE, "NF3539MANC6.Q", nf3539manc6(:))

      call FetchSeries(DFILE, "IF4044MSC6U.Q", if4044msc6u(:))
      call FetchSeries(DFILE, "IF4044MAC6U.Q", if4044mac6u(:))

      call FetchSeries(DFILE, "NF4044NMC6U.Q", nf4044nmc6u(:))
      call FetchSeries(DFILE, "NF4044MSC6U.Q", nf4044msc6u(:))
      call FetchSeries(DFILE, "NF4044MAC6U.Q", nf4044mac6u(:))
      call FetchSeries(DFILE, "NF4044NMNC6.Q", nf4044nmnc6(:))
      call FetchSeries(DFILE, "NF4044MSNC6.Q", nf4044msnc6(:))
      call FetchSeries(DFILE, "NF4044MANC6.Q", nf4044manc6(:))
      
      call FetchSeries(DFILE, "PF4549E_DE.Q", pf4549e_de(:))
      call FetchSeries(DFILE, "RF4549MSCU6.Q", rf4549mscu6(:))
      call FetchSeries(DFILE, "RF4549MACU6.Q", rf4549macu6(:))      
      
      call FetchSeries(DFILE, "PF5054E_DE.Q", pf5054e_de(:))
      call FetchSeries(DFILE, "RF5054MSCU6.Q", rf5054mscu6(:))
      call FetchSeries(DFILE, "RF5054MACU6.Q", rf5054macu6(:))
      call FetchSeries(DFILE, "TR_PF5054MS.Q", tr_pf5054ms(:))
   
   end subroutine InitializeModSol1Vars
   
!===============================================================================   
      
end module EconModSol1VarMod
