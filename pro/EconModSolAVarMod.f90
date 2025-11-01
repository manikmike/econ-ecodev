module EconModSolAVarMod

   use EconModSol2VarMod
   use EconOtlVarMod
   include "OcactFortLib.inc"
   implicit none
   public

   ! Added for wsw_mef_o_tr10 command file
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: wsw_mef_o
   real (kind = 8), dimension(1:2,MAX_YR) :: wswu16_mef_o
   
   ! Added for model change in 2013TR
   real (kind = 8), dimension(0:2,0:24,MAX_YR) :: aww, aww_p, aww_pl
   real (kind = 8), dimension(0:2,0:24,MAX_YR) :: we_3, we_3_p, we_3_pl
   real (kind = 8), dimension(0:2,0:24,MAX_YR) :: mult1_te, mult2_te
   real (kind = 8), dimension(MAX_YR) :: rutemp, etemp, lctemp
   
   ! Added for model change in 2012TR
   real (kind = 8), dimension(0:2,0:24,MAX_YR) :: ce_hi_a
   real (kind = 8), dimension(1:2,MAX_YR) :: ce1013_hi_a
   real (kind = 8), dimension(1:2,MAX_YR) :: ce1415_hi_a
   real (kind = 8), dimension(1:2,1:23,MAX_YR) :: wsw_hio_ox_a
   real (kind = 8), dimension(MAX_YR) :: tefc_n_n_se_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_n_hi_se_a
   real (kind = 8), dimension(MAX_YR) :: wsca_hio_oth_a
   real (kind = 8), dimension(MAX_YR) :: wsw_hio_oth_a
   real (kind = 8), dimension(MAX_YR) :: wsw_hio_oth_se_a
   ! Removed for model change in 2012TR
   !real (kind = 8), dimension(MAX_YR) :: seocmb_hi_a
   

   ! Covered Employment
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: cesy_a
   ! real (kind = 8), dimension(1:2,0:100,MAX_YR) :: cesy_m_100f_a ! ce on mef
   ! real (kind = 8), dimension(1:2,0:100,MAX_YR) :: cesy_m_a ! ce on mef
   real (kind = 8), dimension(0:2,0:100,MAX_YR) :: he_m_sy, ce_m_sy ! ce on mef
   real (kind = 8), dimension(MAX_YR) :: tcea_a
   real (kind = 8), dimension(1:2,MAX_YR) :: ce1013_a
   real (kind = 8), dimension(1:2,MAX_YR) :: ce1415_a
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: ce_a
   real (kind = 8), dimension(MAX_YR) :: ce_err2_a
   real (kind = 8), dimension(MAX_YR) :: ce10o_a
   
   ! Just needed to overwrite estimated if there is historical values
   real (kind = 8), dimension(1:2,MAX_YR) :: ce1013_hist_a
   real (kind = 8), dimension(1:2,MAX_YR) :: ce1415_hist_a
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: ce_hist_a

   ! Total at-any-time Employment
   real (kind = 8), dimension(1:2,MAX_YR) :: te1013_a, he_m_1013, ce_m_1013
   real (kind = 8), dimension(1:2,MAX_YR) :: te1415_a, he_m_1415, ce_m_1415
   
   ! New covered employment variables
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: cew_m, ceso_m, heso_m, hesw_m, hes_m
   real (kind = 8), dimension(0:2,0:15,MAX_YR) :: ceso_m_sy, cew_m_sy
   
   real (kind = 8), dimension(0:2,0:100,MAX_YR) :: te_ph_m_sy, te_slos_m_sy, te_ps_m_sy, te_sloe_m_sy
   real (kind = 8), dimension(0:2,MAX_YR) :: te_ph_m_0t4, te_ph_m_5t9, te_ph_m_1014, te_ph_m_15u
   real (kind = 8), dimension(0:2,MAX_YR) :: te_slos_m_0t4, te_slos_m_5t9, te_slos_m_1014, te_slos_m_15u
   real (kind = 8), dimension(0:2,MAX_YR) :: te_sloe_m_0t4, te_sloe_m_5t9, te_sloe_m_1014, te_sloe_m_15u
   real (kind = 8), dimension(0:2,MAX_YR) :: te_ps_m_0t4, te_ps_m_5t9, te_ps_m_1014, te_ps_m_15u
   real (kind = 8), dimension(0:2,MAX_YR) :: cew_m_1013, cew_m_1415, cew_m_15u
   real (kind = 8), dimension(0:2,MAX_YR) :: ceso_m_1013, ceso_m_1415, ceso_m_15u
   real (kind = 8), dimension(MAX_YR) :: n6079
   real (kind = 8), dimension(MAX_YR) :: lm75ox, lf75ox
   real (kind = 8), dimension(1:2,MAX_YR) :: he_m_15u, ce_m_15u
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: te_ph_m, te_ps_m
   real (kind = 8), dimension(0:2,0:34,MAX_YR) :: te_rro_m, te_sloe_m, te_sloo_m, te_slos_m
   real (kind = 8), dimension(0:2,0:24,MAX_YR) :: he_wol_m, he_wor_m, he_wosl_m, he_wosr_m, he_wosf_m, he_wof_m, hew_m
   real (kind = 8), dimension(0:2,0:24,MAX_YR) :: te_s, te_mn
   
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: te_a, te_a_p
   real (kind = 8), dimension(MAX_YR) :: tefc_n_a
   real (kind = 8), dimension(MAX_YR) :: tefc_n_n_a
   real (kind = 8), dimension(MAX_YR) :: tesl_a
   real (kind = 8), dimension(MAX_YR) :: tesl_o_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_s_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_o_nhi_s_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_e_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_o_nhi_e_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_o_nhi_ns_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_o_nhi_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_n_nhi_s_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_n_nhi_e_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_n_nhi_ns_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_n_nhi_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_o_hi_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_o_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_n_a
   real (kind = 8), dimension(MAX_YR) :: tesl_n_n_hi_a
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: teph_n_a
   real (kind = 8), dimension(MAX_YR) :: teph_n_n_s_a
   real (kind = 8), dimension(MAX_YR) :: tefc_a
   real (kind = 8), dimension(MAX_YR) :: tefc_o_a
   real (kind = 8), dimension(MAX_YR) :: tefc_n_o_a


   ! OASDI covered self-employed earnings
   real (kind = 8), dimension(MAX_YR) :: seocmb_a
   real (kind = 8), dimension(MAX_YR) :: seocmbl1_a
   real (kind = 8), dimension(0:2,0:27,MAX_YR) :: seo_a
   real (kind = 8), dimension(MAX_YR) :: seo_hi_a
   real (kind = 8), dimension(1:2,MAX_YR) :: seo1013_a
   real (kind = 8), dimension(1:2,MAX_YR) :: seo1415_a
   real (kind = 8), dimension(1:2,0:9,MAX_YR) :: seosy_a
   
   ! Other series needed to solve model
   real (kind = 8), dimension(1:2,0:100,MAX_YR) :: nsy_a, ndecsy_a
   real (kind = 8), dimension(1:2,MAX_YR) :: n1415_a
   real (kind = 8), dimension(1:2,MAX_YR) :: n1617_a
   real (kind = 8), dimension(MAX_YR) :: rtp_a
   real (kind = 8), dimension(MAX_YR) :: rtp_1_a
   real (kind = 8), dimension(0:2,1:27,MAX_YR) :: re_a_a
   real (kind = 8), dimension(MAX_YR) :: enawp_a
   real (kind = 8), dimension(1:2,1:11,MAX_YR) :: rd_a
   real (kind = 8), dimension(MAX_YR) :: wswa_a
   real (kind = 8), dimension(1:2,1:27,MAX_YR) :: wsw_a
   real (kind = 8), dimension(MAX_YR) :: cmb_tot_a
   real (kind = 8), dimension(MAX_YR) :: wsph_o_a
   real (kind = 8), dimension(MAX_YR) :: wspf_o_a
   real (kind = 8), dimension(MAX_YR) :: wsprr_o_a
   real (kind = 8), dimension(MAX_YR) :: tips_sr_a
   real (kind = 8), dimension(MAX_YR) :: wsdpb_a
   real (kind = 8), dimension(MAX_YR) :: wspb_o_a
   real (kind = 8), dimension(MAX_YR) :: wspc_a
   real (kind = 8), dimension(MAX_YR) :: wesl_o_a
   real (kind = 8), dimension(MAX_YR) :: wsgslca_a
   real (kind = 8), dimension(MAX_YR) :: wsca_a
   real (kind = 8), dimension(MAX_YR) :: acwa_a
   real (kind = 8), dimension(MAX_YR) :: aw_cmbtot_a
   real (kind = 8), dimension(MAX_YR) :: w_cmbtot_a
   real (kind = 8), dimension(MAX_YR) :: raiw_a
   real (kind = 8), dimension(MAX_YR) :: taxmax_b1_a
   real (kind = 8), dimension(MAX_YR) :: taxmax_b2_a
   real (kind = 8), dimension(MAX_YR) :: taxmax_b3_a
   real (kind = 8), dimension(MAX_YR) :: cmb_wrelmax_a
   real (kind = 8), dimension(MAX_YR) :: cmb_wao1_a
   real (kind = 8), dimension(MAX_YR) :: cmb_wao2_a
   real (kind = 8), dimension(MAX_YR) :: cmb_wao3_a
   real (kind = 8), dimension(MAX_YR) :: cmb_wao4_a
   real (kind = 8), dimension(MAX_YR) :: cmb_wao_a
   real (kind = 8), dimension(MAX_YR) :: cmb_a, cesw_m, cmb_over, ces_m
   real (kind = 8), dimension(MAX_YR) :: cmb_hi_a
   real (kind = 8), dimension(MAX_YR) :: csw_tot_a
   real (kind = 8), dimension(MAX_YR) :: csw_a
   real (kind = 8), dimension(MAX_YR) :: csw_hi_a
   real (kind = 8), dimension(MAX_YR) :: wesl_a
   real (kind = 8), dimension(MAX_YR) :: wesl_n_a
   real (kind = 8), dimension(MAX_YR) :: wesl_n_nhi_s_a
   real (kind = 8), dimension(MAX_YR) :: wesl_n_nhi_e_a
   real (kind = 8), dimension(MAX_YR) :: rawr_ns_a
   real (kind = 8), dimension(MAX_YR) :: wesl_n_nhi_ns_a
   real (kind = 8), dimension(MAX_YR) :: wesl_n_nhi_a
   real (kind = 8), dimension(MAX_YR) :: wesl_n_hi_a
   real (kind = 8), dimension(MAX_YR) :: cse_tot_a, cse_tot_add_a
   real (kind = 8), dimension(MAX_YR) :: cse_cmb_n_a
   real (kind = 8), dimension(MAX_YR) :: cse_a
   real (kind = 8), dimension(MAX_YR) :: acse_seo_a
   real (kind = 8), dimension(MAX_YR) :: acse_cmb_tot_a
   real (kind = 8), dimension(MAX_YR) :: cse_cmb_tot_a
   real (kind = 8), dimension(MAX_YR) :: cse_cmb_a
   real (kind = 8), dimension(MAX_YR) :: acse_cmb_a
   real (kind = 8), dimension(MAX_YR) :: cse_seo_a
   real (kind = 8), dimension(MAX_YR) :: cfca_a
   real (kind = 8), dimension(MAX_YR) :: cslhi_a
   real (kind = 8), dimension(MAX_YR) :: cpf_a
   real (kind = 8), dimension(MAX_YR) :: cp_a
   real (kind = 8), dimension(MAX_YR) :: coverna_a
   real (kind = 8), dimension(MAX_YR) :: ase_a
   real (kind = 8), dimension(MAX_YR) :: asehi_a
   real (kind = 8), dimension(MAX_YR) :: asea_a
   real (kind = 8), dimension(MAX_YR) :: acslw_a
   real (kind = 8), dimension(MAX_YR) :: acmw_a
   real (kind = 8), dimension(MAX_YR) :: acfcw_a
   real (kind = 8), dimension(MAX_YR) :: acfmw_a
   real (kind = 8), dimension(MAX_YR) :: ws_mef_a
   real (kind = 8), dimension(MAX_YR) :: wsw_mef_a, wsw_mef_o_a
   real (kind = 8), dimension(MAX_YR) :: aws_mef_a
   real (kind = 8), dimension(MAX_YR) :: aiw_a
   real (kind = 8), dimension(MAX_YR) :: we_sfo_lrp_a
   real (kind = 8), dimension(MAX_YR) :: te_sfm_lrp_a
   real (kind = 8), dimension(MAX_YR) :: we_sfm_lrp_a
   real (kind = 8), dimension(MAX_YR) :: we_sf_lrp_a
   real (kind = 8), dimension(MAX_YR) :: te_sf_teo_a
   real (kind = 8), dimension(MAX_YR) :: we_sf_teo_a
   real (kind = 8), dimension(MAX_YR) :: we_sf_a
   real (kind = 8), dimension(MAX_YR) :: te_sf_lrp_a
   real (kind = 8), dimension(MAX_YR) :: tceahi_a
   real (kind = 8), dimension(MAX_YR) :: wswahi_a
   real (kind = 8), dimension(MAX_YR) :: wscahi_a
   real (kind = 8), dimension(MAX_YR) :: acwahi_a
   real (kind = 8), dimension(MAX_YR) :: covernhi_a
   real (kind = 8), dimension(MAX_YR) :: aceahi_a
   real (kind = 8), dimension(MAX_YR) :: edmilaf_a
   real (kind = 8), dimension(MAX_YR) :: edmilt_a
   real (kind = 8), dimension(MAX_YR) :: edmilr_a
   real (kind = 8), dimension(MAX_YR) :: mwc_ed_o_a
   real (kind = 8), dimension(MAX_YR) :: mwc_ed_hi_a
   real (kind = 8), dimension(MAX_YR) :: amwc_go2_a
   real (kind = 8), dimension(MAX_YR) :: mwc_edr_o_a
   real (kind = 8), dimension(MAX_YR) :: mwc_edr_hi_a
   real (kind = 8), dimension(MAX_YR) :: mwc_o_a
   real (kind = 8), dimension(MAX_YR) :: mwc_hi_a
   real (kind = 8), dimension(MAX_YR) :: year_a

   real (kind = 8), dimension(MAX_YR) :: tefc_n_sw_a
   real (kind = 8), dimension(MAX_YR) :: esr_ns_a
   real (kind = 8), dimension(MAX_YR) :: cer_mqge_o_a
   real (kind = 8), dimension(0:2,0:35,MAX_YR) :: eprrb_a
   real (kind = 8), dimension(MAX_YR) :: multseo_a
   real (kind = 8), dimension(MAX_YR) :: multcmb_a
   real (kind = 8), dimension(MAX_YR) :: cph_a
   real (kind = 8), dimension(MAX_YR) :: cpb_a
   real (kind = 8), dimension(MAX_YR) :: wsdpb_xeo, wspb_o_xeo, cpb_xeo
   real (kind = 8), dimension(MAX_YR) :: tep_n_n_s_a
   real (kind = 8), dimension(0:2,0:24,MAX_YR) :: tepo_n_a
   real (kind = 8), dimension(MAX_YR) :: cprr_a
   real (kind = 8), dimension(MAX_YR) :: wsprrb_a
   real (kind = 8), dimension(MAX_YR) :: wefc_a
   real (kind = 8), dimension(MAX_YR) :: wefc_o_a
   real (kind = 8), dimension(MAX_YR) :: adj_fsa_fc_a
   real (kind = 8), dimension(MAX_YR) :: wsgfca_a
   real (kind = 8), dimension(MAX_YR) :: wsgmlc_a
   real (kind = 8), dimension(MAX_YR) :: wsgca_a
   real (kind = 8), dimension(MAX_YR) :: cml_a
   real (kind = 8), dimension(MAX_YR) :: aiwbase_a
   real (kind = 8), dimension(MAX_YR) :: taxmaxb1_a
   real (kind = 8), dimension(MAX_YR) :: taxmaxb2_a
   real (kind = 8), dimension(MAX_YR) :: taxmaxb3_a
   real (kind = 8), dimension(MAX_YR) :: tmaxbase_a
   real (kind = 8), dimension(MAX_YR) :: beninc_a
   real (kind = 8), dimension(MAX_YR) :: awr_ns_a
   real (kind = 8), dimension(MAX_YR) :: acea_a, acea_tot_a
   real (kind = 8), dimension(MAX_YR) :: aiw_gr_yr_a
   real (kind = 8), dimension(MAX_YR) :: aiw_gr_a
   real (kind = 8), dimension(MAX_YR) :: tips_sr_add_a
   real (kind = 8), dimension(MAX_YR) :: acwadiff_a
   real (kind = 8), dimension(MAX_YR) :: cpiwdec3_u_a
   real (kind = 8), dimension(MAX_YR) :: ws_n_nhi_a
   real (kind = 8), dimension(MAX_YR) :: wsp_n_nhi_a
   real (kind = 8), dimension(MAX_YR) :: wsprr_a
   real (kind = 8), dimension(MAX_YR) :: wspo_n_nhi_a
   real (kind = 8), dimension(0:2,0:29,MAX_YR) :: enawxph_a
   real (kind = 8), dimension(0:2,0:29,MAX_YR) :: tesl_hi_a
   real (kind = 8), dimension(0:2,0:29,MAX_YR) :: tesl_nhi_a
   real (kind = 8), dimension(0:2,MAX_YR) :: tesl_nhi_1415_a
   real (kind = 8), dimension(0:2,0:29,MAX_YR) :: tesl_e_a
   real (kind = 8), dimension(0:2,0:4,MAX_YR) :: tesl_nhi_s_a
   real (kind = 8), dimension(0:2,MAX_YR) :: tepo_n_1013_a
   real (kind = 8), dimension(0:2,MAX_YR) :: tepo_n_1415_a
   real (kind = 8), dimension(0:2,0:4,MAX_YR) :: tep_nhi_s_a
   real (kind = 8), dimension(MAX_YR) :: r_te16o
   real (kind = 8), dimension(MAX_YR) :: r_te16o_asa
! Variables needed to solve preliminary calculations
   real (kind = 8), dimension(1:2,MAX_YR) :: seou16_a
   real (kind = 8), dimension(1:2,MAX_YR) :: wswu16_a
   real (kind = 8), dimension(MAX_YR) :: eo_oasdi_a
   real (kind = 8), dimension(MAX_YR) :: eo_aw_a
   real (kind = 8), dimension(MAX_YR) :: eo_na_a
   real (kind = 8), dimension(MAX_YR) :: eo_noi_m_16o_a
   real (kind = 8), dimension(MAX_YR) :: eo_nol_m_16o_a
   real (kind = 8), dimension(MAX_YR) :: enawp_b_a
   real (kind = 8), dimension(MAX_YR) :: cpb_k_a
   real (kind = 8), dimension(MAX_YR) :: cpb_wt_a
   real (kind = 8), dimension(MAX_YR) :: cpb_adj_a
   real (kind = 8), dimension(0:2,1:27,MAX_YR) :: p_adj_a
   real (kind = 8), dimension(MAX_YR) :: hise_ti
   real (kind = 8), dimension(MAX_MTH) :: nomintr

   ! Moved here (from EconModSolAOutMod) so it is visible from
   ! EconRevEarnOutMod
   real (kind = 8), dimension(MAX_YR) :: dmwc_o_a
   real (kind = 8), dimension(MAX_YR) :: dmwc_h_a
   
   ! beninc_a(:)
   integer :: yr_ben, qtr_ben
   real (kind = 8) :: cpi_base, cola
   
   integer :: histend

   real (kind = 8), dimension(MAX_YR) :: cpiw_u_a ! needed for output files
   
   real (kind = 8), dimension(MAX_YR) :: awwm_adj, awwf_adj

contains
   
!===============================================================================

   subroutine InitializeModSolAVars()

      integer :: sex, ageGrp, age, group, i, yr
      double precision, dimension(MAX_YR) :: a1
      double precision :: inc
      
      call SetWorkfileVerboseMode(.true.)
      
      ! Added for model changes in 2014TR 
      do sex = 1, 2
         do ageGrp = 1, 14
            call FetchSeries(MEF, "TE_RRO_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_rro_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_SLOO_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_sloo_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_SLOS_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_slos_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_SLOE_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_sloe_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_PS_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_ps_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_PH_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_ph_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TEL_SO_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", tel_so(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_PH_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_ph_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOF_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wof_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOL_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wol_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOR_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wor_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOSF_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wosf_m(sex,ageGrp,:))   
            call FetchSeries(MEF, "HE_WOSL_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wosl_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOSR_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wosr_m(sex,ageGrp,:))
         end do
      end do
      
      call FetchSeries(MEF, "CE_M_M80O.A", ce_m(1,31,:))
      call FetchSeries(MEF, "CE_M_F80O.A", ce_m(2,31,:))
      call FetchSeries(MEF, "HE_M_M80O.A", he_m(1,31,:))
      call FetchSeries(MEF, "HE_M_F80O.A", he_m(2,31,:))
      call FetchSeries(MEF, "TEL_SO_M80O.A", tel_so(1,31,:))
      call FetchSeries(MEF, "TEL_SO_F80O.A", tel_so(2,31,:))

      do sex = 1, 2
         do ageGrp = 17, 23, 6
            call FetchSeries(MEF, "TE_RRO_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_rro_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_SLOO_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_sloo_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_SLOS_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_slos_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_SLOE_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_sloe_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_PS_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_ps_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_PH_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_ph_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_m(sex,ageGrp,:))
            call FetchSeries(MEF, "TEL_SO_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", tel_so(sex,ageGrp,:))
            call FetchSeries(MEF, "TE_PH_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", te_ph_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOF_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wof_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOL_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wol_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOR_M_"//trim(sexLabel(sex))// &
                trim(ageGrpLabel(ageGrp))//".A", he_wor_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOSF_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wosf_m(sex,ageGrp,:))   
            call FetchSeries(MEF, "HE_WOSL_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wosl_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOSR_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wosr_m(sex,ageGrp,:))
         end do
      end do

      do sex = 1, 2
         do age = 0, 15
            call FetchSeries(MEF, "HE_M_"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", he_m_sy(sex,age,:))
            call FetchSeries(MEF, "CESO_M_"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", ceso_m_sy(sex,age,:))
            call FetchSeries(MEF, "CEW_M_"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", cew_m_sy(sex,age,:))
         end do
         ceso_m_1013(sex,:) = ceso_m_sy(sex,10,:) + ceso_m_sy(sex,11,:) + ceso_m_sy(sex,12,:) + ceso_m_sy(sex,13,:)
         ceso_m_1415(sex,:) = ceso_m_sy(sex,14,:) + ceso_m_sy(sex,15,:)
         cew_m_1013(sex,:) = cew_m_sy(sex,10,:) + cew_m_sy(sex,11,:) + cew_m_sy(sex,12,:) + cew_m_sy(sex,13,:)
         cew_m_1415(sex,:) = cew_m_sy(sex,14,:) + cew_m_sy(sex,15,:)
      end do

      do sex = 1, 2
         do age = 0, 99
            call FetchSeries(MEF, "TE_PH_M_"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", te_ph_m_sy(sex,age,:))
            call FetchSeries(MEF, "TE_SLOS_M_"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", te_slos_m_sy(sex,age,:))
            call FetchSeries(MEF, "TE_PS_M_"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", te_ps_m_sy(sex,age,:))
            call FetchSeries(MEF, "TE_SLOE_M_"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", te_sloe_m_sy(sex,age,:))
         end do
         call FetchSeries(MEF, "TE_PH_M_"//trim(sexLabel(sex))// &
           "100O.A", te_ph_m_sy(sex,100,:))
         call FetchSeries(MEF, "TE_SLOS_M_"//trim(sexLabel(sex))// &
           "100O.A", te_slos_m_sy(sex,100,:))
         call FetchSeries(MEF, "TE_PS_M_"//trim(sexLabel(sex))// &
           "100O.A", te_ps_m_sy(sex,100,:))
         call FetchSeries(MEF, "TE_SLOE_M_"//trim(sexLabel(sex))// &
           "100O.A", te_sloe_m_sy(sex,100,:))
      end do

      do sex = 1, 2
         do age = 0, 100
            call FetchSeries(OFILE1, "N"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", nsy_a(sex,age,:))
            ! December population - only used for single-year of age covered workers
            call FetchSeries(OFILE4, "N"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", ndecsy_a(sex,age,:))
         end do
      end do
      
      do sex = 1, 2
         do ageGrp = 1, 12
            call FetchSeries(MEF, "CE_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", ce_m(sex,ageGrp,:))
            call FetchSeries(MEF, "CESO_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", ceso_m(sex,ageGrp,:))
            call FetchSeries(MEF, "CEW_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", cew_m(sex,ageGrp,:))
         end do
         do ageGrp = 23, 24
            call FetchSeries(MEF, "CE_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", ce_m(sex,ageGrp,:))
            call FetchSeries(MEF, "CESO_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", ceso_m(sex,ageGrp,:))
            call FetchSeries(MEF, "CEW_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", cew_m(sex,ageGrp,:))
         end do
         ! 15u
         call FetchSeries(MEF, "CE_M_"//trim(sexLabel(sex))// &
            "15U.A", ce_m_15u(sex,:))
         call FetchSeries(MEF, "CESO_M_"//trim(sexLabel(sex))// &
            "15U.A", ceso_m_15u(sex,:))
         call FetchSeries(MEF, "CEW_M_"//trim(sexLabel(sex))// &
            "15U.A", cew_m_15u(sex,:))
      end do
      call FetchSeries(MEF, "CE_M.A", ce_m(0,0,:))
      histend = sample(2)
      call FetchSeries(MEF, "CESO_M.A", ceso_m(0,0,:))
      call FetchSeries(MEF, "CESO_M_M.A", ceso_m(1,0,:))
      call FetchSeries(MEF, "CESO_M_F.A", ceso_m(2,0,:))
      
      call FetchSeries(MEF, "CEW_M.A", cew_m(0,0,:))
      call FetchSeries(MEF, "CEW_M_M.A", cew_m(1,0,:))
      call FetchSeries(MEF, "CEW_M_F.A", cew_m(2,0,:))
      
      call FetchSeries(MEF, "CES_M.A", ces_m)
      call FetchSeries(MEF, "TEL_SO.A", tel_so(0,0,:))
      call FetchSeries(DFILE, "TE_SFO_LRP.A", te_sfo_lrp(0,0,:))
      call FetchSeries(MEF, "HE_M.A", he_m(0,0,:))
      
      call FetchSeries(MEF, "TE_RRO_M.A", te_rro_m(0,0,:))
      call FetchSeries(MEF, "TE_SLOO_M.A", te_sloo_m(0,0,:))
      call FetchSeries(MEF, "TE_SLOS_M.A", te_slos_m(0,0,:))
      call FetchSeries(MEF, "TE_SLOE_M.A", te_sloe_m(0,0,:))
      call FetchSeries(MEF, "TE_PS_M.A", te_ps_m(0,0,:))
      call FetchSeries(MEF, "TE_PH_M.A", te_ph_m(0,0,:))
      
      do sex = 1, 2
         do age = 0, 99
            call FetchSeries(MEF,"CE_M_"//trim(sexLabel(sex))//&
               trim(IntToAsc(age))//".A",ce_m_sy(sex,age,:))
         end do
         call FetchSeries(MEF,"CE_M_"//trim(sexLabel(sex))//&
            "100O.A",ce_m_sy(sex,100,:))
      end do
      
      call FetchSeries(DFILE, "NM1415.A", n1415_a(1,:))
      call FetchSeries(DFILE, "NM1617.A", n1617_a(1,:))
      call FetchSeries(DFILE, "NF1415.A", n1415_a(2,:))
      call FetchSeries(DFILE, "NF1617.A", n1617_a(2,:))
      call FetchSeries(AFILE, "RTP.A", rtp_a(:))
      call FetchSeries(DFILE, "YEAR.A", year_a(:))
      ! These are old mnemonics that probably shouldn't be used
      call FetchSeries(AFILE, "SEOMU16.A", seou16_a(1,:))
      call FetchSeries(AFILE, "SEOFU16.A", seou16_a(2,:))

      call FetchSeries(DFILE, "TE.A", te_a(0,0,:))
      call FetchSeries(DFILE, "TEFC_N.A", tefc_n_a(:))
      call FetchSeries(DFILE, "TEFC_N_N.A", tefc_n_n_a(:))
      call FetchSeries(MEF, "HE_WOF_M.A", he_wof_m(0,0,:))
      call FetchSeries(DFILE, "TEFC_N_SW.A", tefc_n_sw_a(:))
      call FetchSeries(DFILE, "TESL.A", tesl_a(:))
      call FetchSeries(DFILE, "TESL_O.A", tesl_o_a(:))
      
      do sex = 1, 2
         do group = 1, 27
            if (group == 16) continue
            call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//trim(ageGrpLabel(group))//".A", n_a(sex,group,:))
         end do
      end do

      call FetchSeries(DFILE, "N16O.A", n_a(0,24,:))
      call FetchSeries(DFILE, "TESL_N_S.A", tesl_n_s_a(:))
      call FetchSeries(DFILE, "TESL_N_O_NHI_S.A", tesl_n_o_nhi_s_a(:))
      call FetchSeries(DFILE, "TESL_N_E.A", tesl_n_e_a(:))
      call FetchSeries(DFILE, "TESL_N_O_NHI_E.A", tesl_n_o_nhi_e_a(:))
      call FetchSeries(DFILE, "TESL_N_O_NHI_NS.A", tesl_n_o_nhi_ns_a(:))
      call FetchSeries(DFILE, "ESR_NS.A", esr_ns_a(:))
      call FetchSeries(DFILE, "TESL_N_N_NHI_NS.A", tesl_n_n_nhi_ns_a(:))
      call FetchSeries(DFILE, "CER_MQGE_O.A", cer_mqge_o_a(:))
      call FetchSeries(DFILE, "EPRRB.A", eprrb_a(0,0,:))
      call FetchSeries(DFILE, "TESL_N_O_HI.A", tesl_n_o_hi_a(:))
      call FetchSeries(DFILE, "TESL_N_N.A", tesl_n_n_a(:))
      call FetchSeries(ADFILE, "MULTSEO.A", multseo_a(:))
      !call FetchSeries(DFILE, "SEO.A", seo_a(0,0,:))
      call FetchSeries(DFILE, "CPH.A", cph_a(:))
      call FetchSeries(DFILE, "TEP_N_N_S.A", tep_n_n_s_a(:))
      call FetchSeries(DFILE, "TEPO_N.A", tepo_n_a(0,0,:))
      call FetchSeries(ADFILE, "MULTCMB.A", multcmb_a(:))
      call FetchSeries(DFILE, "WSPF_O.A", wspf_o_a(:))
      call FetchSeries(DFILE, "CPRR.A", cprr_a(:))
      call FetchSeries(AFILE, "WSPRRB.A", wsprrb_a(:))  ! from modass.09a ?
      call FetchSeries(DFILE, "WESL_O.A", wesl_o_a(:))
      !call FetchSeries(DFILE, "CMB_TOT.A", cmb_tot_a(:))
      call FetchSeries(ADFILE, "TIPS_SR.ADD_A", tips_sr_add_a(:))
      
      call FetchSeries(DFILE, "TIPS_SR.A", tips_sr_a)
      call FetchSeries(DFILE, "TIPS_SR.A", a1)
      tips_sr_a(100:) = a1(100:)
      
      call FetchSeries(DFILE, "WSDPB.A", wsdpb_a(:))
      call FetchSeries(DFILE, "WEFC.A", wefc_a(:))
      call FetchSeries(DFILE, "ADJ_FSA_FC.A", adj_fsa_fc_a(:))
      call FetchSeries(DFILE, "CML.A", cml_a(:))
      call FetchSeries(DFILE, "AIW.A", aiw_a(:))
      call FetchSeries(DFILE, "AIWBASE.A", aiwbase_a(:))
      call FetchSeries(DFILE, "WEFC_O.A", wefc_o_a(:))
      call FetchSeries(DFILE, "TMAXBASE.A", tmaxbase_a(:))
      call FetchSeries(DFILE, "CMB.A", cmb_a(:))
      !call FetchSeries(DFILE, "CSW.A", csw_a(:))
      call FetchSeries(DFILE, "WESL.A", wesl_a(:))
      call FetchSeries(DFILE, "WESL_N_NHI_S.A", wesl_n_nhi_s_a(:))
      call FetchSeries(DFILE, "WESL_N_NHI_E.A", wesl_n_nhi_e_a(:))
      call FetchSeries(DFILE, "AWR_NS.A", awr_ns_a(:))
      call FetchSeries(DFILE, "WESL_N_NHI_NS.A", wesl_n_nhi_ns_a(:))
      call FetchSeries(DFILE, "TEFC.A", tefc_a(:))
      call FetchSeries(DFILE, "CSE_CMB_N.A", cse_cmb_n_a(:))
      call FetchSeries(DFILE, "ACMW.A", acmw_a(:))
      call FetchSeries(DFILE, "ACFMW.A", acfmw_a(:))
      call FetchSeries(AFILE, "AIW_GR.A", aiw_gr_a(:))  ! ok
      call FetchSeries(AFILE, "AIW_GR_YR.A", aiw_gr_yr_a(:))  ! ok
      call FetchSeries(DFILE, "WS_MEF.A", ws_mef_a(:))
      call FetchSeries(DFILE, "WE_SFO_LRP.A", we_sfo_lrp_a(:))
      call FetchSeries(DFILE, "WE_SFM_LRP.A", we_sfm_lrp_a(:))
      call FetchSeries(DFILE, "WE_SF_LRP.A", we_sf_lrp_a(:))
      call FetchSeries(DFILE, "WE_SF_TEO.A", we_sf_teo_a(:))
      call FetchSeries(DFILE, "WE_SF.A", we_sf_a(:))
      call FetchSeries(DFILE, "TCEAHI.A", tceahi_a(:))
      call FetchSeries(DFILE, "WSGSLCA.A", wsgslca_a(:))
      call FetchSeries(DFILE, "WSGFCA.A", wsgfca_a(:))
      call FetchSeries(DFILE, "WSGMLC.A", wsgmlc_a(:))
      call FetchSeries(DFILE, "ACWA.A", acwa_a(:))
      call FetchSeries(AFILE, "CPIWDEC3_U.A", cpiwdec3_u_a(:))  ! ok
      call FetchSeries(DFILE, "WSCAHI.A", wscahi_a(:))
      call FetchSeries(DFILE, "WESL_N_NHI.A", wesl_n_nhi_a(:))
      call FetchSeries(DFILE, "WSPH_O.A", wsph_o_a(:))
      call FetchSeries(DFILE, "WSPRR.A", wsprr_a(:))
      call FetchSeries(DFILE, "TESL_N_N_HI.A", tesl_n_n_hi_a(:))
      call FetchSeries(DFILE, "TESL_N_N_NHI.A", tesl_n_n_nhi_a(:))
      call FetchSeries(DFILE, "TESL_NHI_F1415.A", tesl_nhi_1415_a(2,:))
      call FetchSeries(DFILE, "TESL_NHI_F1617.A", tesl_nhi_a(2,1,:))
      call FetchSeries(DFILE, "TESL_NHI_F1819.A", tesl_nhi_a(2,2,:))
      call FetchSeries(DFILE, "TESL_NHI_F2024.A", tesl_nhi_a(2,3,:))
      call FetchSeries(DFILE, "TESL_NHI_F2529.A", tesl_nhi_a(2,4,:))
      call FetchSeries(DFILE, "TESL_NHI_F3034.A", tesl_nhi_a(2,5,:))
      call FetchSeries(DFILE, "TESL_NHI_F3539.A", tesl_nhi_a(2,6,:))
      call FetchSeries(DFILE, "TESL_NHI_F4044.A", tesl_nhi_a(2,7,:))
      call FetchSeries(DFILE, "TESL_NHI_F4549.A", tesl_nhi_a(2,8,:))
      call FetchSeries(DFILE, "TESL_NHI_F5054.A", tesl_nhi_a(2,9,:))
      call FetchSeries(DFILE, "TESL_NHI_F5559.A", tesl_nhi_a(2,10,:))
      call FetchSeries(DFILE, "TESL_NHI_F6064.A", tesl_nhi_a(2,11,:))
      call FetchSeries(DFILE, "TESL_NHI_F6569.A", tesl_nhi_a(2,12,:))
      call FetchSeries(DFILE, "TESL_NHI_F7074.A", tesl_nhi_a(2,13,:))
      call FetchSeries(DFILE, "TESL_NHI_F75O.A", tesl_nhi_a(2,17,:))
      call FetchSeries(DFILE, "TESL_NHI_M1415.A", tesl_nhi_1415_a(1,:))
      call FetchSeries(DFILE, "TESL_NHI_M1617.A", tesl_nhi_a(1,1,:))
      call FetchSeries(DFILE, "TESL_NHI_M1819.A", tesl_nhi_a(1,2,:))
      call FetchSeries(DFILE, "TESL_NHI_M2024.A", tesl_nhi_a(1,3,:))
      call FetchSeries(DFILE, "TESL_NHI_M2529.A", tesl_nhi_a(1,4,:))
      call FetchSeries(DFILE, "TESL_NHI_M3034.A", tesl_nhi_a(1,5,:))
      call FetchSeries(DFILE, "TESL_NHI_M3539.A", tesl_nhi_a(1,6,:))
      call FetchSeries(DFILE, "TESL_NHI_M4044.A", tesl_nhi_a(1,7,:))
      call FetchSeries(DFILE, "TESL_NHI_M4549.A", tesl_nhi_a(1,8,:))
      call FetchSeries(DFILE, "TESL_NHI_M5054.A", tesl_nhi_a(1,9,:))
      call FetchSeries(DFILE, "TESL_NHI_M5559.A", tesl_nhi_a(1,10,:))
      call FetchSeries(DFILE, "TESL_NHI_M6064.A", tesl_nhi_a(1,11,:))
      call FetchSeries(DFILE, "TESL_NHI_M6569.A", tesl_nhi_a(1,12,:))
      call FetchSeries(DFILE, "TESL_NHI_M7074.A", tesl_nhi_a(1,13,:))
      call FetchSeries(DFILE, "TESL_NHI_M75O.A", tesl_nhi_a(1,17,:))
      call FetchSeries(DFILE, "TESL_N_N_NHI_E.A", tesl_n_n_nhi_e_a(:))

      call FetchSeries(DFILE, "TESL_N_N_NHI_S.A", tesl_n_n_nhi_s_a(:))
      call FetchSeries(DFILE, "TESL_N_N_NHI_S.A", tesl_n_n_nhi_s_a(:))
      call FetchSeries(DFILE, "TEPH_N.A", teph_n_a(0,0,:))
      
      !call FetchSeries(DFILE, "CE.A", ce_hist_a(0,0,:))
      call FetchSeries(DFILE, "TCEA.A", tcea_a(:))
      call FetchSeries(DFILE, "CSE_TOT.A", cse_tot_a(:))
      call FetchSeries(DFILE, "WSCA.A", wsca_a)
      acea_tot_a = (wsca_a + cse_tot_a) / tcea_a
      call FetchSeries(DFILE, "AW_CMBTOT.A", aw_cmbtot_a)
      call FetchSeries(DFILE, "AWS_MEF.A", aws_mef_a)
      call FetchSeries(DFILE, "CP.A", cp_a)
      call FetchSeries(DFILE, "WSPB_O.A", wspb_o_a)

      ! Added for model change in 2012TR
      ! Mnemonics changed for 2014 TR
      
      call FetchSeries(DFILE, "WSCA_HIO_OTH.A", wsca_hio_oth_a)

      !call FetchSeries(DFILE, "WSW_HIO_OTH.A", wsw_hio_oth_a)
      call FetchSeries(MEF, "HE_WOR_M.A", he_wor_m(0,0,:))
      
      call FetchSeries(MEF, "HE_WOL_M.A", he_wol_m(0,0,:))

      ! call FetchSeries(DFILE, "WSW_HIO_OTH_SE.A", wsw_hio_oth_se_a)
      call FetchSeries(MEF, "HE_WOSR_M.A", he_wosr_m(0,0,:))
      
      !call FetchSeries(DFILE, "TEFC_N_N_SE.A", tefc_n_n_se_a)
      call FetchSeries(MEF, "HE_WOSF_M.A", he_wosf_m(0,0,:))
      
      !call FetchSeries(DFILE, "TESL_N_N_HI_SE.A", tesl_n_n_hi_se_a)
      call FetchSeries(MEF, "HE_WOSL_M.A", he_wosl_m(0,0,:))
      
      call FetchSeries(DFILE, "CMB_HI.A", cmb_hi_a)
      
      
      !call FetchSeries(DFILE, "CSW_HI.A", csw_hi_a)
      
      !call FetchSeries(DFILE, "SEO_HI.A", seo_hi_a)
      call FetchSeries(MEF, "HESO_M.A", heso_m(0,0,:))
      
      !call FetchSeries(DFILE, "WSWAHI.A", wswahi_a)
      call FetchSeries(MEF, "HEW_M.A", hew_m(0,0,:))
      
      call FetchSeries(MEF, "HES_M.A", hes_m(0,0,:))
      call FetchSeries(MEF, "HESW_M.A", hesw_m(0,0,:))

! Moved series from EconModSolAMod
      call FetchSeries(DFILE, "WESL_N_HI.A", wesl_n_hi_a)
      call FetchSeries(DFILE, "CSE_CMB_TOT.A", cse_cmb_tot_a)
      call FetchSeries(DFILE, "CSE_CMB.A", cse_cmb_a)
      call FetchSeries(DFILE, "CSE.A", cse_a)
      
      !call FetchSeries(DFILE, "CMB.A", cmb_a)
      call FetchSeries(MEF, "CESW_M.A", cesw_m)
      
      !call FetchSeries(DFILE, "CMB_TOT.A", cmb_tot_a)
      call FetchSeries(MEF, "CMB_OVER.A", cmb_over)
      
      call FetchSeries(ADFILE, "CSE_TOT.ADD_A", cse_tot_add_a)
! Added for model change in NEWT
      do sex = 1, 2
         do ageGrp = 1, 12
            ! call FetchSeries(MEF,"TE"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//".A",te_a(sex,ageGrp,:))
            ! te_a_p(sex,ageGrp,:) = te_a(sex,ageGrp,:)
            call FetchSeries(ADFILE,"MULT1_TE"//trim(sexLabel(sex))//trim(ageGrpLabel(ageGrp))//".A",mult1_te(sex,ageGrp,:))
         end do
         ! call FetchSeries(MEF,"TE"//trim(sexLabel(sex))//"70O.A",te_a(sex,23,:))
         ! te_a_p(sex,23,:) = te_a(sex,23,:)
         call FetchSeries(ADFILE,"MULT1_TE"//trim(sexLabel(sex))//"70O.A",mult1_te(sex,23,:))
      end do

      rutemp = 0d0
      call FetchSeries(DFILE, "RM70O.A", rutemp)
      ru_a(1,15,sample(1):sample(2)) = rutemp(sample(1):sample(2))

      rutemp = 0d0
      call FetchSeries(DFILE, "RF70O.A", rutemp)
      ru_a(2,15,sample(1):sample(2)) = rutemp(sample(1):sample(2))

      do sex = 1, 2
         do ageGrp = 1, 12
            etemp = 0d0
            call FetchSeries(DFILE, "E"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A",etemp)
            e_a(sex,ageGrp,sample(1):sample(2)) = etemp(sample(1):sample(2))
         end do
         etemp = 0d0
         ageGrp = 23
         call FetchSeries(DFILE, "E"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A",etemp)
         e_a(sex,ageGrp,sample(1):sample(2)) = etemp(sample(1):sample(2))
      end do
      
      call FetchSeries(DFILE, "ACEA.A", acea_a)
      call FetchSeries(DFILE, "ACFCW.A",acfcw_a)
      call FetchSeries(DFILE, "ACSE_CMB.A", acse_cmb_a)
      call FetchSeries(DFILE, "ACSE_CMB_TOT.A",  acse_cmb_tot_a)
      call FetchSeries(DFILE, "ACSE_SEO.A", acse_seo_a)
      call FetchSeries(DFILE, "ACSLW.A", acslw_a)
      call FetchSeries(DFILE, "ASE.A", ase_a)
      call FetchSeries(DFILE, "CFCA.A", cfca_a)
      call FetchSeries(DFILE, "COVERNA.A", coverna_a)
      call FetchSeries(DFILE, "CPF.A", cpf_a)
      call FetchSeries(DFILE, "CSE_SEO.A", cse_seo_a)
      call FetchSeries(DFILE, "EDMILT.A", edmilt_a)
      call FetchSeries(DFILE, "MWC_HI.A", mwc_hi_a)
      call FetchSeries(DFILE, "MWC_O.A", mwc_o_a)
      call FetchSeries(DFILE, "TEFC_N_O.A", tefc_n_o_a)
      call FetchSeries(DFILE, "TEFC_O.A", tefc_o_a)
      call FetchSeries(DFILE, "TESL_N.A", tesl_n_a)
      call FetchSeries(DFILE, "TESL_N_O.A", tesl_n_o_a)
      call FetchSeries(DFILE, "TESL_N_O_NHI.A", tesl_n_o_nhi_a)
      call FetchSeries(DFILE, "TE_SFM_LRP.A", te_sfm_lrp_a)
      call FetchSeries(DFILE, "TE_SF_LRP.A", te_sf_lrp_a)
      call FetchSeries(DFILE, "TE_SF_TEO.A", te_sf_teo_a)
      call FetchSeries(DFILE, "WESL_N.A", wesl_n_a)
      call FetchSeries(DFILE, "WSPC.A", wspc_a)
      call FetchSeries(DFILE, "W_CMBTOT.A", w_cmbtot_a)

      ! Moved here (from EconModSolAOutMod) so it is visible from
      ! EconRevEarnOutMod
      call FetchSeries(DFILE, "DMWC_O.A", dmwc_o_a)
      call FetchSeries(DFILE, "DMWC_H.A", dmwc_h_a)
      
      ! beninc_a(:)
      ! Moved here (from EconRevEarnOutMod) so it can be used by:
      ! - EconModSolAEquationsMod, taxmax_a(i) equation
      ! - EconModSolAOutMod, ModSolAToStoch() subroutine
      
      call FetchSeries(DFILE, "BENINC.A", beninc_a(:))
      
      do yr_ben = sample(2), sample(1), -1
         if (beninc_a(yr_ben) > 0) then
            cpi_base = cpiw_u(4 * yr_ben + 2)
            exit
         end if
      end do
      
      do yr_ben = sample(2)+1, endYr
         qtr_ben = 4 * yr_ben + 2
         cola = 100d0 * ((nint(1d5 * cpiw_u(qtr_ben)) / 1d5) / (nint(1d5 * cpi_base) / 1d5) - 1d0)
         if (cola >= .05d0) then
            beninc_a(yr_ben) = nint(cola * 10d0) / 10d0
            cpi_base = cpiw_u(qtr_ben)
         else
            beninc_a(yr_ben) = 0d0
         end if
      end do
      
      call FetchSeries(AFILE, "CPIW_U.A", cpiw_u_a)
      
      call FetchSeries(ADFILE, "AWWM_ADJ.A", awwm_adj)
      call FetchSeries(ADFILE, "AWWF_ADJ.A", awwf_adj)
      
   end subroutine InitializeModSolAVars

!===============================================================================
   
   subroutine ReInitializeModSolAVars()
   
      integer :: sex, ageGrp, age, group, i
      double precision, dimension(MAX_YR) :: a1
   
      call FetchSeries(DFILE, "AWS_MEF.A", aws_mef_a)
      call FetchSeries(MEF, "CEW_M.A", cew_m(0,0,:))
      call FetchSeries(DFILE, "CMB.A", cmb_a(:))
      call FetchSeries(DFILE, "CMB_HI.A", cmb_hi_a)
     
      do sex = 1, 2
         etemp = 0d0
         ageGrp = 23
         call FetchSeries(DFILE, "E"//trim(sexLabel(sex))// &
           trim(ageGrpLabel(ageGrp))//".A",etemp)
         e_a(sex,ageGrp,sample(1):sample(2)) = etemp(sample(1):sample(2))
      end do
     
      do sex = 1, 2
         do ageGrp = 1, 12
            call FetchSeries(MEF, "HE_WOSL_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wosl_m(sex,ageGrp,:))
            call FetchSeries(MEF, "HE_WOL_M_"//trim(sexLabel(sex))// &
               trim(ageGrpLabel(ageGrp))//".A", he_wol_m(sex,ageGrp,:))
         end do
         ageGrp = 23
         call FetchSeries(MEF, "HE_WOL_M_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", he_wol_m(sex,ageGrp,:))
         call FetchSeries(MEF, "HE_WOSL_M_"//trim(sexLabel(sex))// &
            trim(ageGrpLabel(ageGrp))//".A", he_wosl_m(sex,ageGrp,:))
      end do
      
      call FetchSeries(MEF, "HE_WOSL_M.A", he_wosl_m(0,0,:))
      
      do sex = 1, 2
         call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//trim(ageGrpLabel(14))//".A", n_a(sex,14,:))
         call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//trim(ageGrpLabel(17))//".A", n_a(sex,17,:))
         call FetchSeries(DFILE, "N"//trim(sexLabel(sex))//trim(ageGrpLabel(23))//".A", n_a(sex,23,:))
      end do
      
      rutemp = 0d0
      call FetchSeries(DFILE, "RM70O.A", rutemp)
      ru_a(1,15,sample(1):sample(2)) = rutemp(sample(1):sample(2))

      rutemp = 0d0
      call FetchSeries(DFILE, "RF70O.A", rutemp)
      ru_a(2,15,sample(1):sample(2)) = rutemp(sample(1):sample(2))
      
      call FetchSeries(DFILE, "TCEA.A", tcea_a(:))
      call FetchSeries(DFILE, "TCEAHI.A", tceahi_a(:))
      call FetchSeries(DFILE, "TE.A", te_a(0,0,:))
      call FetchSeries(MEF, "TE_PH_M.A", te_ph_m(0,0,:))
      
      call FetchSeries(DFILE, "TIPS_SR.A", tips_sr_a)
      call FetchSeries(DFILE, "TIPS_SR.A", a1)
      tips_sr_a(100:) = a1(100:)
      
      call FetchSeries(DFILE, "WS_MEF.A", ws_mef_a(:))
      call FetchSeries(MEF, "TEL_SO.A", tel_so(0,0,:))
      
      do sex = 1, 2
         do age = 18, 34
            call FetchSeries(MEF, "TE_SLOS_M_"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", te_slos_m_sy(sex,age,:))
            call FetchSeries(MEF, "TE_PS_M_"//trim(sexLabel(sex))// &
               trim(IntToAsc(age))//".A", te_ps_m_sy(sex,age,:))
         end do
      end do
   
   end subroutine ReInitializeModSolAVars
   
!===============================================================================
   
end module EconModSolAVarMod
