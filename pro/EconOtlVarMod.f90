module EconOtlVarMod

   use EconModSol2VarMod
   include "OcactFortLib.inc"
   implicit none
   public

   ! Population components for visa holders, students, etc.
   real (kind = 8), dimension(0:2,MAX_YR) :: no_asf1, no_asf2   ! Students (F1 and M1) and Family - authorized
   real (kind = 8), dimension(0:2,MAX_YR) :: no_nasf1, no_nasf2 ! Students (F1 and M1) and Family - previously authorized
   real (kind = 8), dimension(0:2,MAX_YR) :: no_asj1, no_asj2   ! Students (J1) visa holders and Family - authorized
   real (kind = 8), dimension(0:2,MAX_YR) :: no_nasj1, no_nasj2 ! Students (J1) visa holders and Family - preivously authorized
   real (kind = 8), dimension(0:2,MAX_YR) :: no_awj, no_awjf    ! Workers (J1) visa and Family - authorized
   real (kind = 8), dimension(0:2,MAX_YR) :: no_nawj, no_nawjf  ! Workers (J1) visa and Family - previously authorized
   real (kind = 8), dimension(0:2,MAX_YR) :: no_awh, no_nawh    ! H2A visa holders - authorized and previously authorized
   real (kind = 8), dimension(0:2,MAX_YR) :: no_awt, no_awtf    ! Temporary workers and family members - authorized
   real (kind = 8), dimension(0:2,MAX_YR) :: no_awtfa, no_awtfn ! Temporary worker family members (L2 and all other)
   real (kind = 8), dimension(0:2,MAX_YR) :: no_nawt, no_nawtf  ! Temporary workers and family members - previously authorized
   ! Weights to control to historical values
   real (kind = 8), dimension(MAX_YR) :: asf1_hwt, asj1_hwt, awj_hwt, awh_hwt, awt_hwt, no_nawk
   ! Same components as above disaggregated by age and sex
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_asf1_sy, no_asf2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_nasf1_sy, no_nasf2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_asj1_sy, no_asj2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_nasj1_sy, no_nasj2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_awj_sy, no_awjf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_nawj_sy, no_nawjf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_awh_sy, no_nawh_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_awt_sy, no_awtf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_awtfa_sy, no_awtfn_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_nawt_sy, no_nawtf_sy
   ! Weights to apply to aggregate populations
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_asf1_sy, w_no_asf2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_nasf1_sy, w_no_nasf2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_asj1_sy, w_no_asj2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_nasj1_sy, w_no_nasj2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_awj_sy, w_no_awjf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_nawj_sy, w_no_nawjf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_awh_sy, w_no_nawh_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_awt_sy, w_no_awtf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_awtfa_sy, w_no_awtfn_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: w_no_nawt_sy, w_no_nawtf_sy
   ! Additional population concepts
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_no_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_as1_sy, no_as2_sy, no_as_sy, &
                                                   no_aw1_sy, no_aw2_sy, no_aw_sy, no_a_sy, no_a_sy_p
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: no_nas1_sy, no_nas2_sy, no_nas_sy, &
                                                   no_naw1_sy, no_naw2_sy, no_naw_sy, no_na_sy, no_na_sy_p
   ! Aggregates of above additional population concepts
   real (kind = 8), dimension(0:2,MAX_YR) :: no_no
   real (kind = 8), dimension(0:2,MAX_YR) :: no_as1, no_as2, no_as, no_aw1, no_aw2, no_aw, no_a
   real (kind = 8), dimension(0:2,MAX_YR) :: no_nas1, no_nas2, no_nas, no_naw1, no_naw2, no_naw, no_na

   ! Employment components of above by age and sex
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_asf1_sy, eo_asf2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nasf1_sy, eo_nasf2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_asj1_sy, eo_asj2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nasj1_sy, eo_nasj2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_awj_sy, eo_awjf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nawj_sy, eo_nawjf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_awh_sy, eo_nawh_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_awt_sy, eo_awtf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_awtfa_sy, eo_awtfn_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nawt_sy, eo_nawtf_sy
   ! Aggregates of above Employment components
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_asf1, eo_asf2
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_nasf1, eo_nasf2
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_asj1, eo_asj2
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_nasj1, eo_nasj2
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_awj, eo_awjf
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_nawj, eo_nawjf
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_awh, eo_nawh
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_awt, eo_awtf
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_awtfa, eo_awtfn
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_nawt, eo_nawtf
   ! Additional employment concepts
   ! real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_no_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_as1_sy, eo_as2_sy, & !eo_as_sy, &
                                                   eo_aw1_sy, eo_aw2_sy  !eo_aw_sy, eo_a_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nas1_sy, eo_nas2_sy, & !eo_nas_sy, &
                                                   eo_naw1_sy, eo_naw2_sy !eo_naw_sy, eo_na_sy
   ! Aggregates of above additional employment concepts
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_as1, eo_as2, eo_aw1, eo_aw2
   real (kind = 8), dimension(0:2,0:36,MAX_YR) :: eo_nas1, eo_nas2, eo_naw1, eo_naw2
   
   ! Average and Total Wages for above groups
   real (kind = 8), dimension(MAX_YR) :: aws_a
   real (kind = 8), dimension(MAX_YR) :: aws_eo_asf1, aws_eo_asf2
   real (kind = 8), dimension(MAX_YR) :: aws_eo_nasf1, aws_eo_nasf2
   real (kind = 8), dimension(MAX_YR) :: aws_eo_asj1, aws_eo_asj2
   real (kind = 8), dimension(MAX_YR) :: aws_eo_nasj1, aws_eo_nasj2
   real (kind = 8), dimension(MAX_YR) :: aws_eo_awj, aws_eo_awjf
   real (kind = 8), dimension(MAX_YR) :: aws_eo_nawj, aws_eo_nawjf
   real (kind = 8), dimension(MAX_YR) :: aws_eo_awh, aws_eo_nawh
   real (kind = 8), dimension(MAX_YR) :: aws_eo_awt, aws_eo_awtf
   real (kind = 8), dimension(MAX_YR) :: aws_eo_awtfa, aws_eo_awtfn
   real (kind = 8), dimension(MAX_YR) :: aws_eo_nawt, aws_eo_nawtf
   real (kind = 8), dimension(MAX_YR) :: ws_eo_asf1, ws_eo_asf2
   real (kind = 8), dimension(MAX_YR) :: ws_eo_nasf1, ws_eo_nasf2
   real (kind = 8), dimension(MAX_YR) :: ws_eo_asj1, ws_eo_asj2
   real (kind = 8), dimension(MAX_YR) :: ws_eo_nasj1, ws_eo_nasj2
   real (kind = 8), dimension(MAX_YR) :: ws_eo_awj, ws_eo_awjf
   real (kind = 8), dimension(MAX_YR) :: ws_eo_nawj, ws_eo_nawjf
   real (kind = 8), dimension(MAX_YR) :: ws_eo_awh, ws_eo_nawh
   real (kind = 8), dimension(MAX_YR) :: ws_eo_awt, ws_eo_awtf
   real (kind = 8), dimension(MAX_YR) :: ws_eo_awtfa, ws_eo_awtfn
   real (kind = 8), dimension(MAX_YR) :: ws_eo_nawt, ws_eo_nawtf
   
   ! Aggregates of above
   !real (kind = 8), dimension(0:2,MAX_YR) :: eo_no
   real (kind = 8), dimension(MAX_YR) :: ws_eo_as1, ws_eo_as2, ws_eo_as, ws_eo_aw1, ws_eo_aw2, ws_eo_aw, ws_eo_a
   real (kind = 8), dimension(MAX_YR) :: ws_eo_nas1, ws_eo_nas2, ws_eo_nas, ws_eo_naw1, ws_eo_naw2, ws_eo_naw, ws_eo_na
   real (kind = 8), dimension(MAX_YR) :: aws_eo_nol_m, aws_eo_nol_s, aws_eo_nol_u
   real (kind = 8), dimension(MAX_YR) :: aws_eo_noi_m, aws_eo_noi_s, aws_eo_noi_u
   real (kind = 8), dimension(MAX_YR) :: ws_eo_nol, ws_eo_nol_m, ws_eo_nol_s, ws_eo_nol_u
   real (kind = 8), dimension(MAX_YR) :: ws_eo_noi, ws_eo_noi_m, ws_eo_noi_s, ws_eo_noi_u
   real (kind = 8), dimension(MAX_YR) :: ws_eo_no, ws_eo
   real (kind = 8), dimension(MAX_YR) :: ws_eo_mef, ws_eo_mefc, ws_eo_esf, ws_eo_und
   
   ! Total employment components of above by age and sex
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_asf1_sy, teo_asf2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_nasf1_sy, teo_nasf2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_asj1_sy, teo_asj2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_nasj1_sy, teo_nasj2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_awj_sy, teo_awjf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_nawj_sy, teo_nawjf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_awh_sy, teo_nawh_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_awt_sy, teo_awtf_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_awtfa_sy, teo_awtfn_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_nawt_sy, teo_nawtf_sy
   ! Additional total employment concepts
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_as1_sy, teo_as2_sy, &
                                                   teo_aw1_sy, teo_aw2_sy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_nas1_sy, teo_nas2_sy, &
                                                   teo_naw1_sy, teo_naw2_sy
   
   ! Aggregates of above total employment components
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_asf1, teo_asf2
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_nasf1, teo_nasf2
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_asj1, teo_asj2
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_nasj1, teo_nasj2
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_awj, teo_awjf
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_nawj, teo_nawjf
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_awh, teo_nawh
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_awt, teo_awtf
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_awtfa, teo_awtfn
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_nawt, teo_nawtf
   
   ! Additional employment concepts
   ! real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_no_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_as1, teo_as2, &
                                                   teo_aw1, teo_aw2
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_nas1, teo_nas2, &
                                                   teo_naw1, teo_naw2
   ! Added for updated other-than-legal model
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teo_adj
   real (kind = 8), dimension(MAX_YR) :: ws_eo_adj
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_adj_sy
   real (kind = 8), dimension(MAX_YR) :: adj2013
   
   ! Employment ratios for new OTL model (2013 Trustees Report)
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: re_sy  ! employed
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: rte_sy ! total employed
   real (kind = 8), dimension(1:2,1:31,MAX_YR) :: rte ! 23==70O, 31==80O

   ! Social Security Area - Other Population
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: nilsy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: nildsy ! daca
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: nilasy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: nilnasy
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: nildnasy
   ! Totals (now just overall, we may want to change this to also be by sex)
   real (kind = 8), dimension(MAX_YR) :: nila, nilna, nildna
   
   ! Social Security Area Population - Annual by sex and age group
   real (kind = 8), dimension(1:2,1:17,MAX_YR) :: nssa
   
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_a
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_a_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_as
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_as_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_aw
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_aw_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_esf
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_esf_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_mefc
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_mefc_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_mef
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_mef_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_na
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_na_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nas
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nas_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_naw
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_naw_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_no
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_no_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_no_1
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_no_1_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_no_2
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_no_2_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_1
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_1_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_2
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_2_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_m1
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_m1_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_m2
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_m2_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_m
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_m_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_s1
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_s1_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_s2
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_s2_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_s
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_s_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_u1
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_u1_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_u2
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_u2_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_noi_u
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_noi_u_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_1
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_1_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_2
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_2_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_m1
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_m1_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_m2
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_m2_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_m
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_m_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_s1
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_s1_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_s2
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_s2_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_s
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_s_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_u1
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_u1_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_u2
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_u2_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_nol_u
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_nol_u_sy
   real (kind = 8), dimension(0:2,0:36,MAX_YR)   :: eo_und
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: eo_und_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: rer_sy
   real (kind = 8), dimension(MAX_YR) :: r_eoa_e
   real (kind = 8), dimension(MAX_YR) :: teo_esf_16o_x1
   real (kind = 8), dimension(MAX_YR) :: wt1_i
   real (kind = 8), dimension(MAX_YR) :: wt1_im
   real (kind = 8), dimension(MAX_YR) :: wt1_is
   real (kind = 8), dimension(MAX_YR) :: wt1_iu
   real (kind = 8), dimension(MAX_YR) :: wt1_l
   real (kind = 8), dimension(MAX_YR) :: wt1_lm
   real (kind = 8), dimension(MAX_YR) :: wt1_ls
   real (kind = 8), dimension(MAX_YR) :: wt1_lu
   real (kind = 8), dimension(MAX_YR) :: wt2_i
   real (kind = 8), dimension(MAX_YR) :: wt2_im
   real (kind = 8), dimension(MAX_YR) :: wt2_is
   real (kind = 8), dimension(MAX_YR) :: wt2_iu
   real (kind = 8), dimension(MAX_YR) :: wt2_l
   real (kind = 8), dimension(MAX_YR) :: wt2_lm
   real (kind = 8), dimension(MAX_YR) :: wt2_ls
   real (kind = 8), dimension(MAX_YR) :: wt2_lu
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: w_eo_as_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: w_eo_nas_sy

   real (kind = 8), dimension(0:2,0:31,MAX_YR) :: ce_m, te_m, he_m, tel_so, te_sfo_lrp
   ! Old names - duplicate names in Otl
   ! real (kind = 8), dimension(0:2,0:31,MAX_YR) :: ce_m_a
   ! real (kind = 8), dimension(0:2,0:31,MAX_YR) :: he_m_a
   ! real (kind = 8), dimension(0:2,0:31,MAX_YR) :: te_m_a
   ! real (kind = 8), dimension(0:2,0:31,MAX_YR) :: te_sfo_lrp_a
   ! real (kind = 8), dimension(0:2,0:31,MAX_YR) :: tel_so_a
   real (kind = 8), dimension(0:2,0:100,MAX_YR) :: tel_so_sy

   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: qx(1:2,16:100,MAX_YR)
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: plv(1:2,16:100,MAX_YR)

   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_a
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_a_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_as
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_as_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_aw
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_aw_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_esf
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_esf_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_mefc
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: teo_mefc_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_mef
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_mef_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_na
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_na_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_nas
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nas_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_naw
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_naw_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_no
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_no_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_no_1
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_no_1_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_no_2
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_no_2_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_noi_1
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_1_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_noi_2
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_2_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_noi
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_m1_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_m2_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_noi_m
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_m_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_s1_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_s2_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_noi_s
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_s_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_u1_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_u2_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_noi_u
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_noi_u_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_nol_1
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_1_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_nol_2
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_2_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_nol
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_m1_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_m2_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_nol_m
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_m_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_s1_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_s2_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_nol_s
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_s_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_u1_sy
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_u2_sy
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_nol_u
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_nol_u_sy
   
   ! The following two are old and new names
   real (kind = 8), dimension(0:2,0:35,MAX_YR)   :: teo_und
   real (kind = 8), dimension(0:2,0:24,MAX_YR) :: te_u
   
   real (kind = 8), dimension(1:2,16:100,MAX_YR) :: teo_und_sy
   real (kind = 8), dimension(1:2,0:23,MAX_YR)   :: wtteo

contains

!===============================================================================

   subroutine InitializeOtlVars()
   
      integer :: age, sex, ageGrp, yr
      
      call SetWorkfileVerboseMode(.true.)
      
      do sex = 1, 2
         do age = 0, 15
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"IL.A",&
               nilsy(sex,age,:))
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"ILD.A",&
               nildsy(sex,age,:))
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"ILA.A",&
               nilasy(sex,age,:))
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"ILNA.A",&
               nilnasy(sex,age,:))
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"ILDNA.A",&
               nildnasy(sex,age,:))
         end do
      end do
      
      do sex = 1, 2
         do age = 16, 100
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"IL.A",&
               nilsy(sex,age,:))
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"ILD.A",&
               nildsy(sex,age,:))
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"ILA.A",&
               nilasy(sex,age,:))
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"ILNA.A",&
               nilnasy(sex,age,:))
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(IntToAsc(age))//"ILDNA.A",&
               nildnasy(sex,age,:))
            do yr = 64, endYr
               if (nilsy(sex,age,yr) == 0d0) nilsy(sex,age,yr) = .000001d0
            end do
         end do
      end do
      
      do sex = 1, 2
         do ageGrp = 1, 13
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//".A",&
               nssa(sex,ageGrp,:))
         end do
         ageGrp = 17
         call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//".A",&
            nssa(sex,ageGrp,:))
      end do
      
      do sex = 1, 2
         do ageGrp = 1, 14
            call FetchSeries(MEF, "CE_M_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A", ce_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_M_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A", he_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TEL_SO_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A",  tel_so(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "EO_ESF_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A",  eo_esf(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "EO_UND_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A",  eo_und(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "EO_ASF1_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A",  eo_asf1(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "EO_ASJ1_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A",  eo_asj1(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "EO_AWJ_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A",  eo_awj(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "EO_AWH_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A",  eo_awh(sex,ageGrp,:))
         end do
         call FetchSeries(MEF, "CE_M_"//trim(sexLabel(sex))//&
            "75O.A", ce_m(sex,17,:))
         call FetchSeries(MEF, "HE_M_"//trim(sexLabel(sex))//&
            "75O.A", he_m(sex,17,:))
         call FetchSeries(MEF, "TEL_SO_"//trim(sexLabel(sex))//&
            "75O.A",  tel_so(sex,17,:))
         call FetchSeries(OTLFILE, "EO_ESF_"//trim(sexLabel(sex))//&
            "75O.A",  eo_esf(sex,17,:))
         call FetchSeries(OTLFILE, "EO_UND_"//trim(sexLabel(sex))//&
            "75O.A",  eo_und(sex,17,:))
         call FetchSeries(OTLFILE, "EO_ASF1_"//trim(sexLabel(sex))//&
            "75O.A",  eo_asf1(sex,17,:))
         call FetchSeries(OTLFILE, "EO_ASJ1_"//trim(sexLabel(sex))//&
            "75O.A",  eo_asj1(sex,17,:))
         call FetchSeries(OTLFILE, "EO_AWJ_"//trim(sexLabel(sex))//&
            "75O.A",  eo_awj(sex,17,:))
         call FetchSeries(OTLFILE, "EO_AWH_"//trim(sexLabel(sex))//&
            "75O.A",  eo_awh(sex,17,:))
         call FetchSeries(MEF, "TEL_SO_"//trim(sexLabel(sex))//&
           "70O.A",  tel_so(sex,23,:))
      end do
      
      do sex = 1, 2
         do age = 0, 99
            call FetchSeries(MEF, "TEL_SO_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", tel_so_sy(sex,age,:))
         end do
         call FetchSeries(MEF, "TEL_SO_"//trim(sexLabel(sex))//"100O.A", tel_so_sy(sex,100,:))
      end do
      
      do sex = 1, 2
         do ageGrp = 1, 12
            call FetchSeries(AFILE, "TE_M_"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//&
               ".A", te_m(sex,ageGrp,:))
         end do
         call FetchSeries(AFILE, "TE_M_"//trim(sexLabel(sex))//&
            "70O.A", te_m(sex,23,:))
      end do

      ! These don't seem to be updating properly in the historical period
      call FetchSeries(OTLFILE, "RTEM70O.A", rte(1,23,:))
      call FetchSeries(OTLFILE, "RTEF70O.A", rte(2,23,:))
      
      do sex = 1, 2
         do age = 16, 100
            call FetchSeries(OFILE3, "QX_"//trim(sexLabel(sex))//trim(IntToAsc(age))//&
               ".A", qx(sex,age,:))
            call FetchSeries(OFILE3, "PL_"//trim(sexLabel(sex))//trim(IntToAsc(age))//&
               ".A", plv(sex,age,:))
            ! The EOM and EOF series have been renamed as EO_M and EO_F
            call FetchSeries(OTLFILE, "EO_"//trim(sexLabel(sex))//trim(IntToAsc(age))//&
               ".A", eo_sy(sex,age,:))
         end do
      end do
      
      do sex = 1, 2
         do ageGrp = 1, 12
            call FetchSeries(OTLFILE, "TEO_ADJ_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_adj(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "TEO_AWTFN_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_awtfn(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "TEO_NAWTF_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_nawtf(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "TEO_ASF1_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_asf1(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "TEO_ASJ1_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_asj1(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "TEO_AWJ_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_awj(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "TEO_AWH_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_awh(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "TEO_ESF_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_esf(sex,ageGrp,:))
            call FetchSeries(OTLFILE, "TEO_UND_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_und(sex,ageGrp,:))
            te_u(sex,ageGrp,81:87) = teo_und(sex,ageGrp,81:87)
         end do
         ageGrp = 23
         call FetchSeries(OTLFILE, "TEO_ADJ_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_adj(sex,ageGrp,:))
         call FetchSeries(OTLFILE, "TEO_AWTFN_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_awtfn(sex,ageGrp,:))
         call FetchSeries(OTLFILE, "TEO_NAWTF_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", teo_nawtf(sex,ageGrp,:))
         call FetchSeries(OTLFILE, "TEO_ASF1_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", teo_asf1(sex,ageGrp,:))
         call FetchSeries(OTLFILE, "TEO_ASJ1_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", teo_asj1(sex,ageGrp,:))
         call FetchSeries(OTLFILE, "TEO_AWJ_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", teo_awj(sex,ageGrp,:))
         call FetchSeries(OTLFILE, "TEO_AWH_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", teo_awh(sex,ageGrp,:))
         call FetchSeries(OTLFILE, "TEO_ESF_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", teo_esf(sex,ageGrp,:))
         call FetchSeries(OTLFILE, "TEO_UND_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", teo_und(sex,ageGrp,:))
         te_u(sex,ageGrp,81:87) = teo_und(sex,ageGrp,81:87)
         ageGrp = 24
         call FetchSeries(OTLFILE, "TEO_ESF_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", teo_esf(sex,ageGrp,:))
         call FetchSeries(OTLFILE, "TEO_UND_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", teo_und(sex,ageGrp,:))
      end do
      
      call FetchSeries(OTLFILE, "TEO_ADJ.A", teo_adj(0,0,:))
      call FetchSeries(OTLFILE, "WS_EO_ADJ.A", ws_eo_adj)
      
      call FetchSeries(OTLFILE, "TEO_ASF1.A", teo_asf1(0,0,:))
      call FetchSeries(OTLFILE, "TEO_ASJ1.A", teo_asj1(0,0,:))
      call FetchSeries(OTLFILE, "TEO_AWJ.A", teo_awj(0,0,:))
      call FetchSeries(OTLFILE, "TEO_AWH.A", teo_awh(0,0,:))
      
      call FetchSeries(OTLFILE, "TEO_UND.A", teo_und(0,0,:))
      te_u(0,0,81:87) = teo_und(0,0,81:87)
      call FetchSeries(OTLFILE, "TEO_ESF.A", teo_esf(0,0,:))
      
      !DEC$ IF(.FALSE.)
      !Only needed if you want to match Otl3Out on first pass
      do sex = 1, 2
         do age = 16, 100
            
            call FetchSeries(OTLFILE, "EO_ASF1_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_asf1_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_ASF2_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_asf2_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_ASJ1_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_asj1_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_ASJ2_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_asj2_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_AWJ_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_awj_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_AWJF_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_awjf_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_AWH_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_awh_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_AWT_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_awt_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_AWTFA_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_awtfa_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_AWTFN_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_awtfn_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NASF1_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nasf1_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NASF2_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nasf2_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NASJ1_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nasj1_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NASJ2_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nasj2_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NAWJ_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nawj_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NAWJF_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nawjf_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NAWH_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nawh_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NAWT_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nawt_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NAWTF_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nawtf_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NOL_M_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nol_m_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NOL_S_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nol_s_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NOL_U_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_nol_u_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NOI_M_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_noi_m_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NOI_S_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_noi_s_sy(sex,age,:))
            call FetchSeries(OTLFILE, "EO_NOI_U_"//trim(sexLabel(sex))//trim(IntToAsc(age))//".A", eo_noi_u_sy(sex,age,:))

         end do
      end do
      !DEC$ END IF

      include "OtlSeriesInit.fh" ! Needed so ModSolA doesn't "bomb out" on first pass
   
   end subroutine InitializeOtlVars
   
!===============================================================================   

end module EconOtlVarMod
