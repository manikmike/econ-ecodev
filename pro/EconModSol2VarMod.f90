module EconModSol2VarMod

   use EconModSol1VarMod
   include "OcactFortLib.inc"
   implicit none
   public
   
   integer :: lastRaiseDataQtr

   ! Temporary Declarations for Robby to beautify
   real (kind = 8), dimension(MAX_YR) :: awefc_n_a
   real (kind = 8), dimension(MAX_YR) :: awse_a
   real (kind = 8), dimension(MAX_YR) :: awsgefc_a
   real (kind = 8), dimension(MAX_YR) :: awsgfc_a
   real (kind = 8), dimension(MAX_YR) :: awsgfm_a
   real (kind = 8), dimension(MAX_YR) :: awsggefc_a
   real (kind = 8), dimension(MAX_YR) :: awsggesl_a
   real (kind = 8), dimension(MAX_YR) :: awsp_a
   real (kind = 8), dimension(MAX_YR) :: awspf_a
   real (kind = 8), dimension(MAX_YR) :: awsph_a
   real (kind = 8), dimension(MAX_YR) :: awspl_a
   real (kind = 8), dimension(MAX_YR) :: awssp_a
   real (kind = 8), dimension(MAX_YR) :: awsspbnfxge_a
   real (kind = 8), dimension(MAX_YR) :: awsspes_a
   real (kind = 8), dimension(MAX_YR) :: awsspf_a
   real (kind = 8), dimension(MAX_YR) :: awssphs_a
   real (kind = 8), dimension(MAX_YR) :: awsspl_a
   real (kind = 8), dimension(MAX_YR) :: awsspss_a
   real (kind = 8), dimension(MAX_YR) :: awsui_a
   real (kind = 8), dimension(MAX_YR) :: ay_a
   real (kind = 8), dimension(MAX_YR) :: ayf_a
   real (kind = 8), dimension(MAX_YR) :: ayf_k_a
   real (kind = 8), dimension(MAX_YR) :: aynf_a
   real (kind = 8), dimension(MAX_YR) :: aynf_k_a
   real (kind = 8), dimension(MAX_YR) :: cfcgefc_a
   real (kind = 8), dimension(MAX_YR) :: cfcgesl_a
   real (kind = 8), dimension(MAX_YR) :: cfcgfc_a
   real (kind = 8), dimension(MAX_YR) :: cfcgfm_a
   real (kind = 8), dimension(MAX_YR) :: cfcgsl_a
   real (kind = 8), dimension(MAX_YR) :: cpiwms_a
   real (kind = 8), dimension(MAX_YR) :: craz1_a
   real (kind = 8), dimension(MAX_YR) :: cr_ui_a
   real (kind = 8), dimension(MAX_YR) :: dnedmil_a
   real (kind = 8), dimension(MAX_YR) :: ea_a
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: eas_a
   real (kind = 8), dimension(0:2,0:22,MAX_YR) :: eas_r_a
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: eau_a
   real (kind = 8), dimension(0:2,0:22,MAX_YR) :: eau_r_a
   real (kind = 8), dimension(0:2,0:22,MAX_YR) :: eaw_a
   real (kind = 8), dimension(0:2,0:22,MAX_YR) :: eaw_r_a
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: enas_a
   real (kind = 8), dimension(0:2,0:22,MAX_YR) :: enas_r_a
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: enau_a
   real (kind = 8), dimension(0:2,0:22,MAX_YR) :: enau_r_a
   real (kind = 8), dimension(0:2,0:23,MAX_YR) :: enawph_a
   real (kind = 8), dimension(0:2,0:22,MAX_YR) :: enawph_r_a
   real (kind = 8), dimension(MAX_YR) :: egefcps_a
   real (kind = 8), dimension(MAX_YR) :: egfc_a
   real (kind = 8), dimension(MAX_YR) :: eggefc_a
   real (kind = 8), dimension(MAX_YR) :: eggefcmax_a
   real (kind = 8), dimension(MAX_YR) :: eggesl_a
   real (kind = 8), dimension(MAX_YR) :: eggeslmax_a
   real (kind = 8), dimension(MAX_YR) :: ena_a
   real (kind = 8), dimension(MAX_YR) :: enaw_a
   real (kind = 8), dimension(MAX_YR) :: enawpbxge_a
   real (kind = 8), dimension(MAX_YR) :: enawspbxge_a
   real (kind = 8), dimension(MAX_YR) :: ep_a
   real (kind = 8), dimension(MAX_YR) :: epes_est_a
   real (kind = 8), dimension(MAX_YR) :: ephs_est_a
   real (kind = 8), dimension(MAX_YR) :: epss_est_a
   real (kind = 8), dimension(MAX_YR) :: gdpg_a
   real (kind = 8), dimension(MAX_YR) :: gdpge_a
   real (kind = 8), dimension(MAX_YR) :: gdpgefc_a
   real (kind = 8), dimension(MAX_YR) :: gdpgesl_a
   real (kind = 8), dimension(MAX_YR) :: gdpgf_a
   real (kind = 8), dimension(MAX_YR) :: gdpgfc_a
   real (kind = 8), dimension(MAX_YR) :: gdpgfm_a
   real (kind = 8), dimension(MAX_YR) :: gdpgge_a
   real (kind = 8), dimension(MAX_YR) :: gdpggefc_a
   real (kind = 8), dimension(MAX_YR) :: gdpggesl_a
   real (kind = 8), dimension(MAX_YR) :: gdpgsl_a
   real (kind = 8), dimension(MAX_YR) :: gdppbnfxge_a
   real (kind = 8), dimension(MAX_YR) :: gdppf_a
   real (kind = 8), dimension(MAX_YR) :: gdppfreal_a
   real (kind = 8), dimension(MAX_YR) :: gdpph_a
   real (kind = 8), dimension(MAX_YR) :: gdppni_a
   real (kind = 8), dimension(MAX_YR) :: hifc_l_a
   real (kind = 8), dimension(MAX_YR) :: hifm_l_a
   real (kind = 8), dimension(MAX_YR) :: hip_l_a
   real (kind = 8), dimension(MAX_YR) :: hisl_l_a
   real (kind = 8), dimension(MAX_YR) :: mraz_a
   real (kind = 8), dimension(MAX_YR) :: oasdifc_l_a
   real (kind = 8), dimension(MAX_YR) :: oasdifm_l_a
   real (kind = 8), dimension(MAX_YR) :: oasdip_l_a
   real (kind = 8), dimension(MAX_YR) :: oasdisl_l_a
   real (kind = 8), dimension(MAX_YR) :: oli_a
   real (kind = 8), dimension(MAX_YR) :: oli_csrs1_a
   real (kind = 8), dimension(MAX_YR) :: oli_fc_a
   real (kind = 8), dimension(MAX_YR) :: oli_fers1_a
   real (kind = 8), dimension(MAX_YR) :: oli_fersfc_a
   real (kind = 8), dimension(MAX_YR) :: oli_gge_a
   real (kind = 8), dimension(MAX_YR) :: oli_ghi_a
   real (kind = 8), dimension(MAX_YR) :: oli_ghi_fc_a
   real (kind = 8), dimension(MAX_YR) :: oli_ghi_p_a
   real (kind = 8), dimension(MAX_YR) :: oli_ghi_sl_a
   real (kind = 8), dimension(MAX_YR) :: oli_gli_a
   real (kind = 8), dimension(MAX_YR) :: oli_gli_fc_a
   real (kind = 8), dimension(MAX_YR) :: oli_gli_p_a
   real (kind = 8), dimension(MAX_YR) :: oli_gli_sl_a
   real (kind = 8), dimension(MAX_YR) :: oli_p_a
   real (kind = 8), dimension(MAX_YR) :: oli_ppps_a
   real (kind = 8), dimension(MAX_YR) :: oli_pps_a
   real (kind = 8), dimension(MAX_YR) :: oli_retfc_a
   real (kind = 8), dimension(MAX_YR) :: oli_retfm_a
   real (kind = 8), dimension(MAX_YR) :: oli_retsl_a
   real (kind = 8), dimension(MAX_YR) :: oli_sl_a
   real (kind = 8), dimension(MAX_YR) :: oli_su_a
   real (kind = 8), dimension(MAX_YR) :: oli_wc_a
   real (kind = 8), dimension(MAX_YR) :: oli_wcp_a
   real (kind = 8), dimension(MAX_YR) :: oli_wcsl_a
   real (kind = 8), dimension(MAX_YR) :: ooh_a
   real (kind = 8), dimension(MAX_YR) :: pgdp_a
   real (kind = 8), dimension(MAX_YR) :: pgdpaf_a
   real (kind = 8), dimension(MAX_YR) :: rcwsf_a
   real (kind = 8), dimension(MAX_YR) :: rcwsm_a
   real (kind = 8), dimension(MAX_YR) :: rcwsp_a
   real (kind = 8), dimension(MAX_YR) :: rcwssl_a
   real (kind = 8), dimension(MAX_YR) :: relmax_ui_a
   real (kind = 8), dimension(MAX_YR) :: rhip_l_a
   real (kind = 8), dimension(MAX_YR) :: roasdip_l_a
   real (kind = 8), dimension(MAX_YR) :: roli_ppps_a
   real (kind = 8), dimension(MAX_YR) :: roli_su_a
   real (kind = 8), dimension(MAX_YR) :: roli_wcp_a
   real (kind = 8), dimension(MAX_YR) :: rsocf_pbg_a
   real (kind = 8), dimension(MAX_YR) :: rsocsl_wc_a
   real (kind = 8), dimension(MAX_YR) :: rsoc_uip_a
   real (kind = 8), dimension(MAX_YR) :: rsoc_wcp_a
   real (kind = 8), dimension(MAX_YR) :: ruiws1_a
   real (kind = 8), dimension(MAX_YR) :: ruiws2_a
   real (kind = 8), dimension(MAX_YR) :: rwcws_a
   real (kind = 8), dimension(MAX_YR) :: rwsspbnfxge_a
   real (kind = 8), dimension(MAX_YR) :: soc_a
   real (kind = 8), dimension(MAX_YR) :: socf_hi_a
   real (kind = 8), dimension(MAX_YR) :: socf_mifm_a
   real (kind = 8), dimension(MAX_YR) :: socf_oasdi_a
   real (kind = 8), dimension(MAX_YR) :: socf_pbg_a
   real (kind = 8), dimension(MAX_YR) :: socf_retrr_a
   real (kind = 8), dimension(MAX_YR) :: socf_uif_a
   real (kind = 8), dimension(MAX_YR) :: socf_uifc_a
   real (kind = 8), dimension(MAX_YR) :: socf_uifed_a
   real (kind = 8), dimension(MAX_YR) :: socf_uifm_a
   real (kind = 8), dimension(MAX_YR) :: socf_uis_a
   real (kind = 8), dimension(MAX_YR) :: socf_wc_a
   real (kind = 8), dimension(MAX_YR) :: socsl_wc_a
   real (kind = 8), dimension(MAX_YR) :: soc_fc_a
   real (kind = 8), dimension(MAX_YR) :: soc_fm_a
   real (kind = 8), dimension(MAX_YR) :: soc_gge_a
   real (kind = 8), dimension(MAX_YR) :: soc_p_a
   real (kind = 8), dimension(MAX_YR) :: soc_sl_a
   real (kind = 8), dimension(MAX_YR) :: soc_uip_a
   real (kind = 8), dimension(MAX_YR) :: soc_uisl_a
   real (kind = 8), dimension(MAX_YR) :: soc_wcp_a
   real (kind = 8), dimension(MAX_YR) :: soc_wcsl_a
   real (kind = 8), dimension(MAX_YR) :: taxmax_a
   real (kind = 8), dimension(MAX_YR) :: tmaxui_sl_a
   real (kind = 8), dimension(MAX_YR) :: trate_ui_a
   real (kind = 8), dimension(MAX_YR) :: tratio_ui_a
   real (kind = 8), dimension(MAX_YR) :: wefc_n_a
   real (kind = 8), dimension(MAX_YR) :: ws_a
   real (kind = 8), dimension(MAX_YR) :: wsd_a
   real (kind = 8), dimension(MAX_YR) :: wsdp_a
   real (kind = 8), dimension(MAX_YR) :: wsgefc_a
   real (kind = 8), dimension(MAX_YR) :: wsgfc_a
   real (kind = 8), dimension(MAX_YR) :: wsgfm_a
   real (kind = 8), dimension(MAX_YR) :: wsgge_a
   real (kind = 8), dimension(MAX_YR) :: wsggefc_a
   real (kind = 8), dimension(MAX_YR) :: wsggesl_a
   real (kind = 8), dimension(MAX_YR) :: wsp_a
   real (kind = 8), dimension(MAX_YR) :: wspf_a
   real (kind = 8), dimension(MAX_YR) :: wsph_a
   real (kind = 8), dimension(MAX_YR) :: wspni_a
   real (kind = 8), dimension(MAX_YR) :: wss_a
   real (kind = 8), dimension(MAX_YR) :: wssg_a
   real (kind = 8), dimension(MAX_YR) :: wssge_a
   real (kind = 8), dimension(MAX_YR) :: wssgefc_a
   real (kind = 8), dimension(MAX_YR) :: wssgesl_a
   real (kind = 8), dimension(MAX_YR) :: wssgf_a
   real (kind = 8), dimension(MAX_YR) :: wssgfc_a
   real (kind = 8), dimension(MAX_YR) :: wssgfm_a
   real (kind = 8), dimension(MAX_YR) :: wssgge_a
   real (kind = 8), dimension(MAX_YR) :: wssggefc_a
   real (kind = 8), dimension(MAX_YR) :: wssggesl_a
   real (kind = 8), dimension(MAX_YR) :: wssgsl_a
   real (kind = 8), dimension(MAX_YR) :: wssp_a
   real (kind = 8), dimension(MAX_YR) :: wsspbnfxge_a
   real (kind = 8), dimension(MAX_YR) :: wsspes_a
   real (kind = 8), dimension(MAX_YR) :: wsspf_a
   real (kind = 8), dimension(MAX_YR) :: wssph_a
   real (kind = 8), dimension(MAX_YR) :: wssphs_a
   real (kind = 8), dimension(MAX_YR) :: wsspni_a
   real (kind = 8), dimension(MAX_YR) :: wsspss_a
   real (kind = 8), dimension(MAX_YR) :: y_a
   real (kind = 8), dimension(MAX_YR) :: yf_a
   real (kind = 8), dimension(MAX_YR) :: ynf_a

   ! Employment
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: enas_r
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: enau_r
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: eaw_r
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: eau_r
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: eas_r
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: enawph_r
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: enas
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: enau
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: eaw
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: eau
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: eas
   real (kind = 8), dimension(0:2,0:29,MAX_QTR) :: enawph
   real (kind = 8), dimension(MAX_QTR) :: egefcps
   real (kind = 8), dimension(MAX_QTR) :: egfc
   real (kind = 8), dimension(MAX_QTR) :: eggefc
   real (kind = 8), dimension(MAX_QTR) :: eggesl
   real (kind = 8), dimension(MAX_QTR) :: ea
   real (kind = 8), dimension(MAX_QTR) :: ena
   real (kind = 8), dimension(MAX_QTR) :: ep
   real (kind = 8), dimension(MAX_QTR) :: enaw
   real (kind = 8), dimension(MAX_QTR) :: enawpbxge
   real (kind = 8), dimension(MAX_QTR) :: enawspbxge

   ! Employment Adjustment Factor
   real (kind = 8), dimension(1:2,1:29,MAX_QTR) :: enas_adj
   real (kind = 8), dimension(1:2,1:29,MAX_QTR) :: enau_adj
   real (kind = 8), dimension(1:2,1:29,MAX_QTR) :: eaw_adj
   real (kind = 8), dimension(1:2,1:29,MAX_QTR) :: eau_adj
   real (kind = 8), dimension(1:2,1:29,MAX_QTR) :: eas_adj
   real (kind = 8), dimension(1:2,1:29,MAX_QTR) :: enawph_adj
   real (kind = 8), dimension(0:0,0:0,MAX_QTR) :: eaw_add

   ! Other
   real (kind = 8), dimension(MAX_QTR) :: gdppfreal
   real (kind = 8), dimension(MAX_QTR) :: gdppfreal_add
   real (kind = 8), dimension(MAX_QTR) :: pgdpaf
   real (kind = 8), dimension(MAX_QTR) :: dnedmil
   real (kind = 8), dimension(MAX_QTR) :: craz1
   real (kind = 8), dimension(MAX_QTR) :: awefc_n
   real (kind = 8), dimension(MAX_QTR) :: awspl
   real (kind = 8), dimension(MAX_QTR) :: awsggesl
   real (kind = 8), dimension(MAX_QTR) :: wsggesl
   real (kind = 8), dimension(MAX_QTR) :: wsgge
   real (kind = 8), dimension(MAX_QTR) :: awsggefc
   real (kind = 8), dimension(MAX_QTR) :: wsggefc
   real (kind = 8), dimension(MAX_QTR) :: awsgfm
   real (kind = 8), dimension(MAX_QTR) :: wsgfm
   real (kind = 8), dimension(MAX_QTR) :: socf_uifc
   real (kind = 8), dimension(MAX_QTR) :: socf_wc
   real (kind = 8), dimension(MAX_QTR) :: wefc_n
   real (kind = 8), dimension(MAX_QTR) :: oasdifc_l
   real (kind = 8), dimension(MAX_QTR) :: hifc_l
   real (kind = 8), dimension(MAX_QTR) :: soc_fc
   real (kind = 8), dimension(MAX_QTR) :: cpiwms
   real (kind = 8), dimension(MAX_QTR) :: oli_ghi_fc
   real (kind = 8), dimension(MAX_QTR) :: oli_gli_fc
   real (kind = 8), dimension(MAX_QTR) :: awsgefc
   real (kind = 8), dimension(MAX_QTR) :: wsgefc
   real (kind = 8), dimension(MAX_QTR) :: wsgfc
   real (kind = 8), dimension(MAX_QTR) :: oli_csrs1
   real (kind = 8), dimension(MAX_QTR) :: oli_fers1
   real (kind = 8), dimension(MAX_QTR) :: oli_fersfc
   real (kind = 8), dimension(MAX_QTR) :: oli_retfc
   real (kind = 8), dimension(MAX_QTR) :: oli_fc
   real (kind = 8), dimension(MAX_QTR) :: rcwsf
   real (kind = 8), dimension(MAX_QTR) :: wssgfc
   real (kind = 8), dimension(MAX_QTR) :: wssggefc
   real (kind = 8), dimension(MAX_QTR) :: cfcgfc
   real (kind = 8), dimension(MAX_QTR) :: gdpgfc
   real (kind = 8), dimension(MAX_QTR) :: wssgefc
   real (kind = 8), dimension(MAX_QTR) :: cfcgefc
   real (kind = 8), dimension(MAX_QTR) :: gdpgefc
   real (kind = 8), dimension(MAX_QTR) :: gdpggefc
   real (kind = 8), dimension(MAX_QTR) :: oasdisl_l
   real (kind = 8), dimension(MAX_QTR) :: hisl_l
   real (kind = 8), dimension(MAX_QTR) :: soc_uisl
   real (kind = 8), dimension(MAX_QTR) :: rsocsl_wc
   real (kind = 8), dimension(MAX_QTR) :: rwcws
   real (kind = 8), dimension(MAX_QTR) :: soc_wcsl
   real (kind = 8), dimension(MAX_QTR) :: soc_sl
   real (kind = 8), dimension(MAX_QTR) :: oli_gli_sl
   real (kind = 8), dimension(MAX_QTR) :: oli_ghi_sl
   real (kind = 8), dimension(MAX_QTR) :: oli_wcsl
   real (kind = 8), dimension(MAX_QTR) :: oli_retsl
   real (kind = 8), dimension(MAX_QTR) :: oli_sl
   real (kind = 8), dimension(MAX_QTR) :: rcwssl
   real (kind = 8), dimension(MAX_QTR) :: wssggesl
   real (kind = 8), dimension(MAX_QTR) :: wssgsl
   real (kind = 8), dimension(MAX_QTR) :: cfcgsl
   real (kind = 8), dimension(MAX_QTR) :: gdppgsl
   real (kind = 8), dimension(MAX_QTR) :: wssgesl
   real (kind = 8), dimension(MAX_QTR) :: cfcgesl
   real (kind = 8), dimension(MAX_QTR) :: gdpgesl
   real (kind = 8), dimension(MAX_QTR) :: gdpggesl
   real (kind = 8), dimension(MAX_QTR) :: oli_retfm
   real (kind = 8), dimension(MAX_QTR) :: socf_uifm
   real (kind = 8), dimension(MAX_QTR) :: socf_mifm
   real (kind = 8), dimension(MAX_QTR) :: oasdifm_l
   real (kind = 8), dimension(MAX_QTR) :: hifm_l
   real (kind = 8), dimension(MAX_QTR) :: soc_fm
   real (kind = 8), dimension(MAX_QTR) :: rcwsm  
   real (kind = 8), dimension(MAX_QTR) :: wssgfm
   real (kind = 8), dimension(MAX_QTR) :: cfcgfm
   real (kind = 8), dimension(MAX_QTR) :: gdpgfm
   real (kind = 8), dimension(MAX_QTR) :: gdpgge
   real (kind = 8), dimension(MAX_QTR) :: gdppf
   real (kind = 8), dimension(MAX_QTR) :: wssph
   real (kind = 8), dimension(MAX_QTR) :: ooh
   real (kind = 8), dimension(MAX_QTR) :: gdpph
   real (kind = 8), dimension(MAX_QTR) :: awsspl
   real (kind = 8), dimension(MAX_QTR) :: awssphs
   real (kind = 8), dimension(MAX_QTR) :: ephs_est
   real (kind = 8), dimension(MAX_QTR) :: wssphs
   real (kind = 8), dimension(MAX_QTR) :: awsspes
   real (kind = 8), dimension(MAX_QTR) :: epes_est
   real (kind = 8), dimension(MAX_QTR) :: wsspes
   real (kind = 8), dimension(MAX_QTR) :: awsspss
   real (kind = 8), dimension(MAX_QTR) :: epss_est
   real (kind = 8), dimension(MAX_QTR) :: wsspss
   real (kind = 8), dimension(MAX_QTR) :: gdppni
   real (kind = 8), dimension(MAX_QTR) :: gdppbnfxge
   real (kind = 8), dimension(MAX_QTR) :: ynf, ynf_mult
   real (kind = 8), dimension(MAX_QTR) :: wsspbnfxge
   real (kind = 8), dimension(MAX_QTR) :: wsspf
   real (kind = 8), dimension(MAX_QTR) :: wssp
   real (kind = 8), dimension(MAX_QTR) :: socf_retrr
   real (kind = 8), dimension(MAX_QTR) :: oli_gli_p
   real (kind = 8), dimension(MAX_QTR) :: oli_ghi_p
   real (kind = 8), dimension(MAX_QTR) :: roasdip_l
   real (kind = 8), dimension(MAX_QTR) :: rhip_l
   real (kind = 8), dimension(MAX_QTR) :: rsoc_uip
   real (kind = 8), dimension(MAX_QTR) :: rsoc_wcp
   real (kind = 8), dimension(MAX_QTR) :: rsocf_pbg
   real (kind = 8), dimension(MAX_QTR) :: roli_wcp
   real (kind = 8), dimension(MAX_QTR) :: roli_su
   real (kind = 8), dimension(MAX_QTR) :: roli_ppps
   real (kind = 8), dimension(MAX_QTR) :: rcwsp
   real (kind = 8), dimension(MAX_QTR) :: wssgge
   real (kind = 8), dimension(MAX_QTR) :: wss
   real (kind = 8), dimension(MAX_QTR) :: ws
   real (kind = 8), dimension(MAX_QTR) :: awse
   real (kind = 8), dimension(MAX_QTR) :: awsgfc
   real (kind = 8), dimension(MAX_QTR) :: wsp
   real (kind = 8), dimension(MAX_QTR) :: awsp
   real (kind = 8), dimension(MAX_QTR) :: wspf
   real (kind = 8), dimension(MAX_QTR) :: awspf
   real (kind = 8), dimension(MAX_QTR) :: wsph
   real (kind = 8), dimension(MAX_QTR) :: awsph
   real (kind = 8), dimension(MAX_QTR) :: awssp
   real (kind = 8), dimension(MAX_QTR) :: awsui
   real (kind = 8), dimension(MAX_QTR) :: gdpgf
   real (kind = 8), dimension(MAX_QTR) :: gdpg
   real (kind = 8), dimension(MAX_QTR) :: gdpge
   real (kind = 8), dimension(MAX_QTR) :: rwsspbnfxge
   real (kind = 8), dimension(MAX_QTR) :: rwsspbnfxge_adj
   !real (kind = 8), dimension(MAX_QTR) :: taxmaxq
   real (kind = 8), dimension(MAX_QTR) :: wspni
   real (kind = 8), dimension(MAX_QTR) :: wssgf
   real (kind = 8), dimension(MAX_QTR) :: wssg
   real (kind = 8), dimension(MAX_QTR) :: wssge
   real (kind = 8), dimension(MAX_QTR) :: ayf_k
   real (kind = 8), dimension(MAX_QTR) :: yf
   real (kind = 8), dimension(MAX_QTR) :: awsspbnfxge
   real (kind = 8), dimension(MAX_QTR) :: awsspf
   real (kind = 8), dimension(MAX_QTR) :: ayf
   real (kind = 8), dimension(MAX_QTR) :: aynf
   real (kind = 8), dimension(MAX_QTR) :: aynf_k
   real (kind = 8), dimension(MAX_QTR) :: cr_ui
   real (kind = 8), dimension(MAX_QTR) :: tmaxui_sl
   real (kind = 8), dimension(MAX_QTR) :: relmax_ui
   real (kind = 8), dimension(MAX_QTR) :: tratio_ui
   real (kind = 8), dimension(MAX_QTR) :: trate_ui
   real (kind = 8), dimension(MAX_QTR) :: ruiws1
   real (kind = 8), dimension(MAX_QTR) :: ruiws2
   real (kind = 8), dimension(MAX_QTR) :: wsd
   real (kind = 8), dimension(MAX_QTR) :: wsdp
   real (kind = 8), dimension(MAX_QTR) :: oli_gge
   real (kind = 8), dimension(MAX_QTR) :: oli_wcp
   real (kind = 8), dimension(MAX_QTR) :: oli_su
   real (kind = 8), dimension(MAX_QTR) :: oli_ppps
   real (kind = 8), dimension(MAX_QTR) :: oli_p
   real (kind = 8), dimension(MAX_QTR) :: oli
   real (kind = 8), dimension(MAX_QTR) :: soc_gge
   real (kind = 8), dimension(MAX_QTR) :: soc_uip
   real (kind = 8), dimension(MAX_QTR) :: soc_wcp
   real (kind = 8), dimension(MAX_QTR) :: oasdip_l
   real (kind = 8), dimension(MAX_QTR) :: hip_l
   real (kind = 8), dimension(MAX_QTR) :: soc_pbg
   real (kind = 8), dimension(MAX_QTR) :: soc_p
   real (kind = 8), dimension(MAX_QTR) :: soc
   real (kind = 8), dimension(MAX_QTR) :: oli_pps
   real (kind = 8), dimension(MAX_QTR) :: oli_wc
   real (kind = 8), dimension(MAX_QTR) :: oli_ghi
   real (kind = 8), dimension(MAX_QTR) :: oli_gli
   real (kind = 8), dimension(MAX_QTR) :: socsl_wc   
   real (kind = 8), dimension(MAX_QTR) :: socf_uifed
   real (kind = 8), dimension(MAX_QTR) :: socf_uis
   real (kind = 8), dimension(MAX_QTR) :: socf_uif
   real (kind = 8), dimension(MAX_QTR) :: socf_oasdi
   real (kind = 8), dimension(MAX_QTR) :: socf_hi
   real (kind = 8), dimension(MAX_QTR) :: long_range
   real (kind = 8), dimension(MAX_QTR) :: minw
   real (kind = 8), dimension(MAX_QTR) :: cpiw_u
   real (kind = 8), dimension(1:2,1:3,MAX_QTR) :: rns
   real (kind = 8), dimension(MAX_QTR) :: n_ssa
 ! real (kind = 8), dimension(MAX_QTR) :: gdpreal
   real (kind = 8), dimension(MAX_QTR) :: pgdp
   real (kind = 8), dimension(MAX_QTR) :: edmil_r
   real (kind = 8), dimension(MAX_QTR) :: nu10
   real (kind = 8), dimension(MAX_QTR) :: qtr
   real (kind = 8), dimension(MAX_QTR) :: mraz 
   real (kind = 8), dimension(MAX_QTR) :: tefc_n 
   real (kind = 8), dimension(MAX_QTR) :: emptroasi 
   real (kind = 8), dimension(MAX_QTR) :: emptrdi 
   real (kind = 8), dimension(MAX_QTR) :: adj_fsa_fc 
   real (kind = 8), dimension(MAX_QTR) :: emptrhi 
   real (kind = 8), dimension(MAX_QTR) :: rgr_ghi 
   real (kind = 8), dimension(MAX_QTR) :: olif_retfco 
   real (kind = 8), dimension(MAX_QTR) :: rcfcgfc 
   real (kind = 8), dimension(MAX_QTR) :: rcfcgefc 
   real (kind = 8), dimension(MAX_QTR) :: csla 
   real (kind = 8), dimension(MAX_QTR) :: cslhi 
   real (kind = 8), dimension(MAX_QTR) :: rcfcgsl 
   real (kind = 8), dimension(MAX_QTR) :: gdpsl	
   real (kind = 8), dimension(MAX_QTR) :: rcfcgesl	
   real (kind = 8), dimension(MAX_QTR) :: cpiwmswt 
   real (kind = 8), dimension(MAX_QTR) :: gdpgsl	
   real (kind = 8), dimension(MAX_QTR) :: cml 
   real (kind = 8), dimension(MAX_QTR) :: rcfcgfm 
   real (kind = 8), dimension(MAX_QTR) :: kgdpreal
   real (kind = 8), dimension(MAX_QTR) :: wsspni
   real (kind = 8), dimension(MAX_QTR) :: wsprrb
   real (kind = 8), dimension(MAX_QTR) :: txrp
   real (kind = 8), dimension(MAX_QTR) :: cp
   real (kind = 8), dimension(MAX_QTR) :: ws_to_wss_dyr
   real (kind = 8), dimension(MAX_QTR) :: ws_to_wss_d
   real (kind = 8), dimension(MAX_QTR) :: drtpp
   real (kind = 8), dimension(MAX_QTR) :: drtpn
   real (kind = 8), dimension(MAX_QTR) :: taxmax
   real (kind = 8), dimension(MAX_QTR) :: socf_pbg
   real (kind = 8), dimension(MAX_QTR) :: cph
   real (kind = 8), dimension(MAX_QTR) :: ay
   real (kind = 8), dimension(MAX_QTR) :: y
   real (kind = 8), dimension(0:0,0:0,MAX_QTR) :: enas_add
   real (kind = 8), dimension(MAX_QTR) :: ea_add
   real (kind = 8), dimension(MAX_QTR) :: pgdpaf_add
   real (kind = 8), dimension(0:0,0:0,MAX_QTR) :: enawph_add
   real (kind = 8), dimension(MAX_QTR) :: rcwsf_add
   real (kind = 8), dimension(MAX_QTR) :: rcwsm_add
   real (kind = 8), dimension(MAX_QTR) :: rcwsp_add
   real (kind = 8), dimension(MAX_QTR) :: rcwssl_add
   real (kind = 8), dimension(MAX_QTR) :: wsspf_add
   real (kind = 8), dimension(MAX_QTR) :: wsspbnfxge_add

contains

!===============================================================================

   subroutine InitializeModSol2Vars()
      
      call SetWorkfileVerboseMode(.true.)
      
      call FetchSeries(AFILE, "MINW.Q", minw(:))
      call FetchSeries(AFILE, "CPIW_U.Q", cpiw_u(:))
      call FetchSeries(DFILE, "RNM1617S.Q", rns(1,1,:))
      call FetchSeries(ADFILE, "EM1617NAS.ADJ", enas_adj(1,1,:))
      call FetchSeries(ADFILE, "EF1617NAS.ADJ", enas_adj(2,1,:))
      call FetchSeries(ADFILE, "EF1819NAS.ADJ", enas_adj(2,2,:))
      call FetchSeries(ADFILE, "EF2024NAS.ADJ", enas_adj(2,3,:))
      call FetchSeries(ADFILE, "EF2534NAS.ADJ", enas_adj(2,18,:))
      call FetchSeries(ADFILE, "EF3544NAS.ADJ", enas_adj(2,19,:))
      call FetchSeries(ADFILE, "EF4554NAS.ADJ", enas_adj(2,20,:))
      call FetchSeries(ADFILE, "EF5564NAS.ADJ", enas_adj(2,21,:))
      call FetchSeries(ADFILE, "EF65ONAS.ADJ", enas_adj(2,22,:))
      call FetchSeries(ADFILE, "EM1819NAS.ADJ", enas_adj(1,2,:))
      call FetchSeries(ADFILE, "EM2024NAS.ADJ", enas_adj(1,3,:))
      call FetchSeries(ADFILE, "EM2534NAS.ADJ", enas_adj(1,18,:))
      call FetchSeries(ADFILE, "EM3544NAS.ADJ", enas_adj(1,19,:))
      call FetchSeries(ADFILE, "EM4554NAS.ADJ", enas_adj(1,20,:))
      call FetchSeries(ADFILE, "EM5564NAS.ADJ", enas_adj(1,21,:))
      call FetchSeries(ADFILE, "EM65ONAS.ADJ", enas_adj(1,22,:)) 
      call FetchSeries(DFILE, "N_SSA.Q", n_ssa(:)) 
      call FetchSeries(DFILE, "YEAR.Q", year(:)) 
      call FetchSeries(DFILE, "LONGRANGE.Q", long_range(:)) 
      call FetchSeries(DFILE, "GDPPF"//REAL_GDP_SERIES(4:5)//".Q", gdppfreal(:)) 
      call FetchSeries(ADFILE, "GDPPF"//REAL_GDP_SERIES(4:5)//".ADD_Q", gdppfreal_add(:)) 
      call FetchSeries(AFILE, trim(REAL_GDP_SERIES)//".Q", gdpreal(:)) 
      call FetchSeries(DFILE, "EA.Q", ea(:)) 
      call FetchSeries(DFILE, "ENAS.Q", enas(0,0,:)) 
      call FetchSeries(DFILE, "ENA.Q", ena(:)) 
      call FetchSeries(ADFILE, "ENAS.ADD_Q", enas_add(0,0,:)) 
      call FetchSeries(ADFILE, "EA.ADD_Q", ea_add(:)) 
      call FetchSeries(ADFILE, "EM1617NAU.ADJ", enau_adj(1,1,:))
      call FetchSeries(ADFILE, "EF1617NAU.ADJ", enau_adj(2,1,:))
      call FetchSeries(ADFILE, "EF1819NAU.ADJ", enau_adj(2,2,:))
      call FetchSeries(ADFILE, "EF2024NAU.ADJ", enau_adj(2,3,:))
      call FetchSeries(ADFILE, "EF2534NAU.ADJ", enau_adj(2,18,:))
      call FetchSeries(ADFILE, "EF3544NAU.ADJ", enau_adj(2,19,:))
      call FetchSeries(ADFILE, "EF4554NAU.ADJ", enau_adj(2,20,:))
      call FetchSeries(ADFILE, "EF5564NAU.ADJ", enau_adj(2,21,:))
      call FetchSeries(ADFILE, "EF65ONAU.ADJ", enau_adj(2,22,:))
      call FetchSeries(ADFILE, "EM1819NAU.ADJ", enau_adj(1,2,:))
      call FetchSeries(ADFILE, "EM2024NAU.ADJ", enau_adj(1,3,:))
      call FetchSeries(ADFILE, "EM2534NAU.ADJ", enau_adj(1,18,:))
      call FetchSeries(ADFILE, "EM3544NAU.ADJ", enau_adj(1,19,:))
      call FetchSeries(ADFILE, "EM4554NAU.ADJ", enau_adj(1,20,:))
      call FetchSeries(ADFILE, "EM5564NAU.ADJ", enau_adj(1,21,:))
      call FetchSeries(ADFILE, "EM65ONAU.ADJ", enau_adj(1,22,:)) 
      call FetchSeries(DFILE, "ENAU.Q", enau(0,0,:)) 
      call FetchSeries(ADFILE, "EM1617AW.ADJ", eaw_adj(1,1,:))
      call FetchSeries(ADFILE, "EF1617AW.ADJ", eaw_adj(2,1,:))
      call FetchSeries(ADFILE, "EF1819AW.ADJ", eaw_adj(2,2,:))
      call FetchSeries(ADFILE, "EF2024AW.ADJ", eaw_adj(2,3,:))
      call FetchSeries(ADFILE, "EF2534AW.ADJ", eaw_adj(2,18,:))
      call FetchSeries(ADFILE, "EF3544AW.ADJ", eaw_adj(2,19,:))
      call FetchSeries(ADFILE, "EF4554AW.ADJ", eaw_adj(2,20,:))
      call FetchSeries(ADFILE, "EF5564AW.ADJ", eaw_adj(2,21,:))
      call FetchSeries(ADFILE, "EF65OAW.ADJ", eaw_adj(2,22,:))
      call FetchSeries(ADFILE, "EM1819AW.ADJ", eaw_adj(1,2,:))
      call FetchSeries(ADFILE, "EM2024AW.ADJ", eaw_adj(1,3,:))
      call FetchSeries(ADFILE, "EM2534AW.ADJ", eaw_adj(1,18,:))
      call FetchSeries(ADFILE, "EM3544AW.ADJ", eaw_adj(1,19,:))
      call FetchSeries(ADFILE, "EM4554AW.ADJ", eaw_adj(1,20,:))
      call FetchSeries(ADFILE, "EM5564AW.ADJ", eaw_adj(1,21,:))
      call FetchSeries(ADFILE, "EM65OAW.ADJ", eaw_adj(1,22,:)) 
      call FetchSeries(DFILE, "EAW.Q", eaw(0,0,:)) 
      call FetchSeries(ADFILE, "EAW.ADD_Q", eaw_add(0,0,:)) 
      call FetchSeries(ADFILE, "EM1617AS.ADJ", eas_adj(1,1,:))
      call FetchSeries(ADFILE, "EF1617AU.ADJ", eau_adj(2,1,:))
      call FetchSeries(ADFILE, "EF1819AU.ADJ", eau_adj(2,2,:))
      call FetchSeries(ADFILE, "EF2024AU.ADJ", eau_adj(2,3,:))
      call FetchSeries(ADFILE, "EF2534AU.ADJ", eau_adj(2,18,:))
      call FetchSeries(ADFILE, "EF3544AU.ADJ", eau_adj(2,19,:))
      call FetchSeries(ADFILE, "EF4554AU.ADJ", eau_adj(2,20,:))
      call FetchSeries(ADFILE, "EF5564AU.ADJ", eau_adj(2,21,:))
      call FetchSeries(ADFILE, "EF65OAU.ADJ", eau_adj(2,22,:))
      call FetchSeries(ADFILE, "EM1617AU.ADJ", eau_adj(1,1,:))
      call FetchSeries(ADFILE, "EM1819AU.ADJ", eau_adj(1,2,:))
      call FetchSeries(ADFILE, "EM2024AU.ADJ", eau_adj(1,3,:))
      call FetchSeries(ADFILE, "EM2534AU.ADJ", eau_adj(1,18,:))
      call FetchSeries(ADFILE, "EM3544AU.ADJ", eau_adj(1,19,:))
      call FetchSeries(ADFILE, "EM4554AU.ADJ", eau_adj(1,20,:))
      call FetchSeries(ADFILE, "EM5564AU.ADJ", eau_adj(1,21,:))
      call FetchSeries(ADFILE, "EM65OAU.ADJ", eau_adj(1,22,:)) 
      call FetchSeries(DFILE, "EAU.Q", eau(0,0,:)) 
      call FetchSeries(ADFILE, "EF1617AS.ADJ", eas_adj(2,1,:))
      call FetchSeries(ADFILE, "EF1819AS.ADJ", eas_adj(2,2,:))
      call FetchSeries(ADFILE, "EF2024AS.ADJ", eas_adj(2,3,:))
      call FetchSeries(ADFILE, "EF2534AS.ADJ", eas_adj(2,18,:))
      call FetchSeries(ADFILE, "EF3544AS.ADJ", eas_adj(2,19,:))
      call FetchSeries(ADFILE, "EF4554AS.ADJ", eas_adj(2,20,:))
      call FetchSeries(ADFILE, "EF5564AS.ADJ", eas_adj(2,21,:))
      call FetchSeries(ADFILE, "EF65OAS.ADJ", eas_adj(2,22,:))
      call FetchSeries(ADFILE, "EM1819AS.ADJ", eas_adj(1,2,:))
      call FetchSeries(ADFILE, "EM2024AS.ADJ", eas_adj(1,3,:))
      call FetchSeries(ADFILE, "EM2534AS.ADJ", eas_adj(1,18,:))
      call FetchSeries(ADFILE, "EM3544AS.ADJ", eas_adj(1,19,:))
      call FetchSeries(ADFILE, "EM4554AS.ADJ", eas_adj(1,20,:))
      call FetchSeries(ADFILE, "EM5564AS.ADJ", eas_adj(1,21,:))
      call FetchSeries(ADFILE, "EM65OAS.ADJ", eas_adj(1,22,:)) 
      call FetchSeries(DFILE, "EM1819AS.Q", eas(1,2,:)) 
      ! These need to come from DFILE
      call FetchSeries(DFILE, "EM2024AS.Q", eas(1,3,:)) 
      call FetchSeries(DFILE, "EM2534AS.Q", eas(1,18,:))
      call FetchSeries(DFILE, "EM3544AS.Q", eas(1,19,:))
      call FetchSeries(DFILE, "EM4554AS.Q", eas(1,20,:))
      call FetchSeries(DFILE, "EM5564AS.Q", eas(1,21,:))
      call FetchSeries(DFILE, "EM65OAS.Q", eas(1,22,:)) 
      !call FetchSeries(DFILE, "EM1617AU.Q", eau(1,1,:)) 
      !call FetchSeries(DFILE, "EM1819NAS.Q", enas(1,2,:)) 
      !call FetchSeries(DFILE, "EM1819NAU.Q", enau(1,2,:)) 
      !call FetchSeries(DFILE, "EM1819AW.Q", eaw(1,2,:)) 
      !call FetchSeries(DFILE, "EM1819AU.Q", eau(1,2,:)) 
      call FetchSeries(DFILE, "RNM1819S.Q", rns(1,2,:)) 
      call FetchSeries(DFILE, "RNM2024S.Q", rns(1,3,:)) 
      call FetchSeries(DFILE, "RNF1617S.Q", rns(2,1,:)) 
      call FetchSeries(DFILE, "RNF1819S.Q", rns(2,2,:)) 
      call FetchSeries(DFILE, "EF5564NAS.Q", enas(2,21,:)) 
      call FetchSeries(AFILE, "PGDP.A", pgdp_a(:)) 
      call FetchSeries(AFILE, "PGDP.Q", pgdp(:)) 
      call FetchSeries(DFILE, "PGDPAF.Q", pgdpaf(:)) 
      call FetchSeries(ADFILE, "PGDPAF.ADD_Q", pgdpaf_add(:)) 
      call FetchSeries(ADFILE, "EF1617NAWPH.ADJ", enawph_adj(2,1,:))
      call FetchSeries(ADFILE, "EF1819NAWPH.ADJ", enawph_adj(2,2,:))
      call FetchSeries(ADFILE, "EF2024NAWPH.ADJ", enawph_adj(2,3,:))
      call FetchSeries(ADFILE, "EF2534NAWPH.ADJ", enawph_adj(2,18,:))
      call FetchSeries(ADFILE, "EF3544NAWPH.ADJ", enawph_adj(2,19,:))
      call FetchSeries(ADFILE, "EF4554NAWPH.ADJ", enawph_adj(2,20,:))
      call FetchSeries(ADFILE, "EF5564NAWPH.ADJ", enawph_adj(2,21,:))
      call FetchSeries(ADFILE, "EF65ONAWPH.ADJ", enawph_adj(2,22,:))
      call FetchSeries(DFILE, "NU10.Q", nu10(:)) 
      call FetchSeries(ADFILE, "EM1617NAWPH.ADJ", enawph_adj(1,1,:))
      call FetchSeries(ADFILE, "EM1819NAWPH.ADJ", enawph_adj(1,2,:))
      call FetchSeries(ADFILE, "EM2024NAWPH.ADJ", enawph_adj(1,3,:))
      call FetchSeries(ADFILE, "EM2534NAWPH.ADJ", enawph_adj(1,18,:))
      call FetchSeries(ADFILE, "EM3544NAWPH.ADJ", enawph_adj(1,19,:))
      call FetchSeries(ADFILE, "EM4554NAWPH.ADJ", enawph_adj(1,20,:))
      call FetchSeries(ADFILE, "EM5564NAWPH.ADJ", enawph_adj(1,21,:))
      call FetchSeries(ADFILE, "EM65ONAWPH.ADJ", enawph_adj(1,22,:))
      call FetchSeries(DFILE, "ENAWPH.Q", enawph(0,0,:))
      call FetchSeries(ADFILE, "ENAWPH.ADD_Q", enawph_add(0,0,:)) 
      !call FetchSeries(DFILE, "EF1617NAWPH.Q", enawph(2,1,:))
      !call FetchSeries(DFILE, "EF1819NAWPH.Q", enawph(2,2,:))
      !call FetchSeries(DFILE, "EF2024NAWPH.Q", enawph(2,3,:))
      call FetchSeries(DFILE, "EF2534NAWPH.Q", enawph(2,18,:))
      call FetchSeries(DFILE, "EF3544NAWPH.Q", enawph(2,19,:))
      call FetchSeries(DFILE, "EF4554NAWPH.Q", enawph(2,20,:))
      call FetchSeries(DFILE, "EF5564NAWPH.Q", enawph(2,21,:))
      !call FetchSeries(DFILE, "EF65ONAWPH.Q", enawph(2,22,:))
      !call FetchSeries(DFILE, "EFNAWPH.Q", enawph(2,0,:))
      call FetchSeries(ADFILE, "EGEFCPS.EXG", egefcps(:))
      call FetchSeries(ADFILE, "EGFC.EXG", egfc(:))
      call FetchSeries(DFILE, "EGGEFC.Q", eggefc(:))
      call FetchSeries(DFILE, "EGGESL.Q", eggesl(:))
      !call FetchSeries(DFILE, "EM1617NAWPH.Q", enawph(1,1,:))
      !call FetchSeries(DFILE, "EM1819NAWPH.Q", enawph(1,2,:))
      !call FetchSeries(DFILE, "EM2024NAWPH.Q", enawph(1,3,:))
      !call FetchSeries(DFILE, "EM2534NAWPH.Q", enawph(1,18,:))
      !call FetchSeries(DFILE, "EM3544NAWPH.Q", enawph(1,19,:))
      !call FetchSeries(DFILE, "EM4554NAWPH.Q", enawph(1,20,:))
      call FetchSeries(DFILE, "EM5564NAWPH.Q", enawph(1,21,:))
      !call FetchSeries(DFILE, "EM65ONAWPH.Q", enawph(1,22,:))
      !call FetchSeries(DFILE, "EMNAWPH.Q", enawph(1,0,:))
      call FetchSeries(DFILE, "EP.Q", ep(:))
      call FetchSeries(DFILE, "QTR.Q", qtr(:)) 
      call FetchSeries(DFILE, "AWSP.Q", awsp(:)) 
      call FetchSeries(AFILE, "WS_TO_WSS_DYR.Q", ws_to_wss_dyr(:)) 
      call FetchSeries(AFILE, "WS_TO_WSS_D.Q", ws_to_wss_d(:)) 
      call FetchSeries(DFILE, "EDMIL_R.Q", edmil_r(:)) 
      call FetchSeries(DFILE, "EMPTROASI.Q", emptroasi(:)) 
      call FetchSeries(DFILE, "EMPTRDI.Q", emptrdi(:)) 
      call FetchSeries(DFILE, "ADJ_FSA_FC.Q", adj_fsa_fc(:)) 
      call FetchSeries(DFILE, "EMPTRHI.Q", emptrhi(:)) 
      call FetchSeries(DFILE, "CPIWMSWT.Q", cpiwmswt(:)) 
      call FetchSeries(DFILE, "RGR_GHI.Q", rgr_ghi(:)) 
      call FetchSeries(DFILE, "RCFCGFC.Q", rcfcgfc(:)) 
      call FetchSeries(DFILE, "CSLA.Q", csla(:)) 
      call FetchSeries(DFILE, "CSLHI.Q", cslhi(:)) 
      call FetchSeries(DFILE, "CML.Q", cml(:)) 
      call FetchSeries(AFILE, "K"//trim(REAL_GDP_SERIES)//".Q", kgdpreal(:)) 
      call FetchSeries(DFILE, "CP.Q", cp(:)) 
      call FetchSeries(DFILE, "TXRP.Q", txrp(:)) 
      call FetchSeries(DFILE, "CPH.Q", cph(:)) 
      call FetchSeries(DFILE, "TAXMAX.Q", taxmax(:))
      call FetchSeries(DFILE, "AWEFC_N.Q", awefc_n(:))
      call FetchSeries(DFILE, "AWSGGEFC.Q", awsggefc(:))
      call FetchSeries(DFILE, "AWSGGESL.Q", awsggesl(:))
      call FetchSeries(DFILE, "AWSGFM.Q", awsgfm(:))
      call FetchSeries(DFILE, "CPIWMS.Q", cpiwms(:))
      call FetchSeries(DFILE, "OLI_GHI_FC.Q", oli_ghi_fc(:))
      call FetchSeries(DFILE, "AWSGEFC.Q", awsgefc(:))
      call FetchSeries(DFILE, "RSOCSL_WC.Q", rsocsl_wc(:))
      call FetchSeries(DFILE, "RWCWS.Q", rwcws(:))
      call FetchSeries(DFILE, "OLI_GHI_SL.Q", oli_ghi_sl(:))
      call FetchSeries(DFILE, "OLI_RETSL.Q", oli_retsl(:))
      call FetchSeries(DFILE, "WSSGSL.Q", wssgsl(:))
      call FetchSeries(DFILE, "OLI_RETFM.Q", oli_retfm(:))
      call FetchSeries(DFILE, "WSSPH.Q", wssph(:))
      call FetchSeries(DFILE, "OOH.Q", ooh(:))
      call FetchSeries(DFILE, "AWSSPHS.Q", awssphs(:))
      call FetchSeries(DFILE, "EPHS_EST.Q", ephs_est(:))
      call FetchSeries(DFILE, "AWSSPES.Q", awsspes(:))
      call FetchSeries(DFILE, "EPES_EST.Q", epes_est(:))
      call FetchSeries(DFILE, "AWSSPSS.Q", awsspss(:))
      call FetchSeries(DFILE, "EPSS_EST.Q", epss_est(:))
      call FetchSeries(DFILE, "WSSPNI.Q", wsspni(:))
      call FetchSeries(DFILE, "GDPPNI.Q", gdppni(:))
      call FetchSeries(DFILE, "YNF.Q", ynf(:))
      call FetchSeries(ADFILE, "YNF_MULT.Q", ynf_mult(:))
      call FetchSeries(DFILE, "WSSPBNFXGE.Q", wsspbnfxge(:))
      call FetchSeries(DFILE, "OLI_GLI_P.Q",  oli_gli_p(:))
      call FetchSeries(DFILE, "ROLI_PPPS.Q", roli_ppps(:))
      call FetchSeries(DFILE, "WSPNI.Q", wspni(:))
      call FetchSeries(DFILE, "TMAXUI_SL.Q", tmaxui_sl(:))
      call FetchSeries(DFILE, "AWSPL.Q", awspl(:))
      call FetchSeries(DFILE, "TEFC_N.Q", tefc_n(:))
      call FetchSeries(DFILE, "RCFCGEFC.Q", rcfcgefc(:))
      call FetchSeries(DFILE, "WSGGESL.Q", wsggesl(:))
      call FetchSeries(DFILE, "WSSGGESL.Q", wssggesl(:))
      call FetchSeries(DFILE, "RCFCGSL.Q", rcfcgsl(:))
      call FetchSeries(DFILE, "RCFCGESL.Q", rcfcgesl(:))
      call FetchSeries(DFILE, "WSGFM.Q", wsgfm(:))
      call FetchSeries(DFILE, "RCFCGFM.Q", rcfcgfm(:))
      call FetchSeries(DFILE, "WSSP.Q", wssp(:))
      call FetchSeries(DFILE, "AWSSP.Q", awssp(:))
      call FetchSeries(DFILE, "AWSSPL.Q", awsspl(:))
      call FetchSeries(DFILE, "WSSPHS.Q", wssphs(:))
      call FetchSeries(DFILE, "WSSPES.Q", wsspes(:))
      call FetchSeries(DFILE, "WSSPSS.Q", wsspss(:))
      call FetchSeries(DFILE, "GDPPBNFXGE.Q", gdppbnfxge(:))
      call FetchSeries(DFILE, "ENAWPBXGE.Q", enawpbxge(:))
      call FetchSeries(AFILE, "WSPRRB.Q", wsprrb(:)) ! copied to AFILE by modass.09a
      call FetchSeries(DFILE, "OLI_GHI_P.Q",  oli_ghi_p(:))
      call FetchSeries(DFILE, "WSP.Q", wsp(:))
      call FetchSeries(DFILE, "AWSE.Q", awse(:))
      call FetchSeries(DFILE, "YF.Q", yf(:))
      call FetchSeries(DFILE, "WSSPF.Q", wsspf(:))
      call FetchSeries(DFILE, "EAS.Q", eas(0,0,:)) 
      call FetchSeries(DFILE, "AWSUI.Q", awsui(:))
      call FetchSeries(DFILE, "RUIWS1.Q", ruiws1(:))
      call FetchSeries(DFILE, "RUIWS2.Q", ruiws2(:))
      call FetchSeries(ADFILE, "RCWSF.ADD_Q", rcwsf_add(:))
      call FetchSeries(ADFILE, "RCWSM.ADD_Q", rcwsm_add(:))
      call FetchSeries(ADFILE, "RCWSP.ADD_Q", rcwsp_add(:))
      call FetchSeries(ADFILE, "RCWSSL.ADD_Q", rcwssl_add(:))
      call FetchSeries(ADFILE, "WSSPF.ADD_Q", wsspf_add(:))
      call FetchSeries(ADFILE, "WSSPBNFXGE.ADD_Q", wsspbnfxge_add(:))
      call FetchSeries(DFILE, "CRAZ1.Q", craz1(:))
      call FetchSeries(DFILE, "AWEFC_N.Q", awefc_n(:))
      call FetchSeries(DFILE, "WSGGEFC.Q", wsggefc(:))
      call FetchSeries(DFILE, "MRAZ.Q", mraz(:))
      call FetchSeries(DFILE, "SOCF_UIFC.Q", socf_uifc(:))
      call FetchSeries(DFILE, "SOCF_WC.Q",  socf_wc(:))
      call FetchSeries(DFILE, "WEFC_N.Q", wefc_n(:))
      !call FetchSeries(DFILE, "OASDIFC_L.Q", oasdifc_l(:))
      !call FetchSeries(DFILE, "HIFC_L.Q", hifc_l(:))
      !call FetchSeries(DFILE, "SOC_FC.Q", soc_fc(:))
      call FetchSeries(DFILE, "OLI_GLI_FC.Q", oli_gli_fc(:))
      call FetchSeries(DFILE, "WSGEFC.Q", wsgefc(:))
      call FetchSeries(DFILE, "WSGFC.Q", wsgfc(:))
      !call FetchSeries(DFILE, "OLI_CSRS1.Q", oli_csrs1(:))
      !call FetchSeries(DFILE, "OLI_FERS1.Q", oli_fers1(:))
      !call FetchSeries(DFILE, "OLI_FERSFC.Q", oli_fersfc(:))
      call FetchSeries(DFILE, "OLI_RETFC.Q", oli_retfc(:))
      call FetchSeries(DFILE, "OLIF_RETFCO.Q", olif_retfco(:))
      !call FetchSeries(DFILE, "OLI_FC.Q", oli_fc(:))
      call FetchSeries(DFILE, "RCWSF.Q", rcwsf(:))
      call FetchSeries(DFILE, "WSSGFC.Q", wssgfc(:))
      call FetchSeries(DFILE, "WSSGGEFC.Q", wssggefc(:))
      call FetchSeries(DFILE, "WSSGEFC.Q", wssgefc(:))
      call FetchSeries(DFILE, "CFCGEFC.Q", cfcgefc(:))
      call FetchSeries(DFILE, "GDPGEFC.Q", gdpgefc(:))
      call FetchSeries(DFILE, "GDPGGEFC.Q", gdpggefc(:))
      call FetchSeries(DFILE, "SOC_UISL.Q", soc_uisl(:))
      call FetchSeries(DFILE, "SOC_WCSL.Q", soc_wcsl(:))
      !call FetchSeries(DFILE, "OLI_SL.Q", oli_sl(:))
      call FetchSeries(DFILE, "RCWSSL.Q", rcwssl(:))
      call FetchSeries(DFILE, "CFCGSL.Q", cfcgsl(:))
      call FetchSeries(DFILE, "GDPGSL.Q", gdpgsl(:))
      call FetchSeries(DFILE, "WSSGESL.Q", wssgesl(:))
      call FetchSeries(DFILE, "CFCGESL.Q", cfcgesl(:))
      call FetchSeries(DFILE, "GDPGESL.Q", gdpgesl(:))
      call FetchSeries(DFILE, "GDPGGESL.Q", gdpggesl(:))
      call FetchSeries(DFILE, "SOCF_UIFM.Q", socf_uifm(:))
      call FetchSeries(DFILE, "SOCF_MIFM.Q", socf_mifm(:))
      call FetchSeries(DFILE, "OLI_GLI_SL.Q", oli_gli_sl(:))
      !call FetchSeries(DFILE, "SOC_SL.Q", soc_sl(:))
      call FetchSeries(DFILE, "OLI_WCSL.Q", oli_wcsl(:))
      call FetchSeries(DFILE, "RCWSM.Q", rcwsm(:))
      call FetchSeries(DFILE, "WSSGFM.Q", wssgfm(:))
      call FetchSeries(DFILE, "CFCGFM.Q", cfcgfm(:))
      call FetchSeries(DFILE, "GDPGFM.Q", gdpgfm(:))
      call FetchSeries(DFILE, "GDPGGE.Q", gdpgge(:))
      call FetchSeries(DFILE, "GDPPH.Q", gdpph(:))
      call FetchSeries(DFILE, "ENAW.Q", enaw(:))
      call FetchSeries(DFILE, "SOCF_RETRR.Q", socf_retrr(:))
      call FetchSeries(DFILE, "RCWSP.Q", rcwsp(:))
      call FetchSeries(DFILE, "WSPF.Q", wspf(:))
      call FetchSeries(DFILE, "AWSPF.Q", awspf(:))
      call FetchSeries(DFILE, "AWSPH.Q", awsph(:))
      call FetchSeries(DFILE, "RWSSPBNFXGE.Q ", rwsspbnfxge(:))
      call FetchSeries(ADFILE, "RWSSPBNFXGE_ADJ.Q ", rwsspbnfxge_adj(:))
      call FetchSeries(DFILE, "AYF_K.Q", ayf_k(:))
      !call FetchSeries(DFILE, "RELMAX_UI.Q", relmax_ui(:))
      call FetchSeries(DFILE, "WSD.Q", wsd(:))
      call FetchSeries(DFILE, "WS.Q", ws(:))
      !call FetchSeries(DFILE, "OLI_P.Q", oli_p(:))
      call FetchSeries(DFILE, "OLI_PPPS.Q", oli_ppps(:))
      call FetchSeries(DFILE, "OLI_SU.Q", oli_su(:))
      call FetchSeries(DFILE, "OLI_WCP.Q", oli_wcp(:))
      call FetchSeries(DFILE, "OLI.Q", oli(:))
      call FetchSeries(DFILE, "SOC_UIP.Q", soc_uip(:))
      call FetchSeries(DFILE, "SOC_WCP.Q", soc_wcp(:))
      !call FetchSeries(DFILE, "OASDIP_L.Q", oasdip_l(:))
      !call FetchSeries(DFILE, "HIP_L.Q", hip_l(:))
      call FetchSeries(DFILE, "SOCF_PBG.Q", socf_pbg(:))
      !call FetchSeries(DFILE, "SOC_P.Q", soc_p(:))
      call FetchSeries(DFILE, "SOC.Q", soc(:))
      call FetchSeries(DFILE, "GDPGFC.Q", gdpgfc(:))
      call FetchSeries(DFILE, "CFCGFC.Q", cfcgfc(:))
      call FetchSeries(DFILE, "AWSGFC.Q", awsgfc(:))
      call FetchSeries(DFILE, "AWSSPBNFXGE.Q", awsspbnfxge(:))
      call FetchSeries(DFILE, "AWSSPF.Q", awsspf(:))
      call FetchSeries(DFILE, "AYF.Q", ayf(:))
      call FetchSeries(DFILE, "AYNF.Q", aynf(:))
      call FetchSeries(DFILE, "DNEDMIL.Q", dnedmil(:))
      call FetchSeries(DFILE, "GDPG.Q", gdpg(:))      
      call FetchSeries(DFILE, "GDPGE.Q", gdpge(:))
      call FetchSeries(DFILE, "GDPGF.Q", gdpgf(:))
      call FetchSeries(DFILE, "GDPPF.Q", gdppf(:))
      call FetchSeries(DFILE, "WSDP.Q", wsdp(:))
      call FetchSeries(DFILE, "WSS.Q", wss(:))
      call FetchSeries(DFILE, "WSSG.Q", wssg(:))
      call FetchSeries(DFILE, "WSSGE.Q", wssge(:))
      call FetchSeries(DFILE, "WSSGF.Q", wssgf(:))
      call FetchSeries(DFILE, "WSSGGE.Q", wssgge(:))            
   
   end subroutine InitializeModSol2Vars
   
!===============================================================================   
   
end module EconModSol2VarMod