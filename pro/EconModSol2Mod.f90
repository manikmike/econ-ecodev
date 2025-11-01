module EconModSol2Mod

   use EconParMod
   use EconLibMod
   use EconModSol2VarMod
   use EconModSol2OutMod   
   use EconModSol2EquationsMod
   include "OcactFortLib.inc"
   implicit none
   
   private
   public :: EconModSol2Main
   
contains

!===============================================================================

   subroutine EconModSol2Main()

      integer :: i
      
      call InitModSol2()
      ! call AdjustData() - not implemented
      ! call ExogenizeData() - not implemented
      call FetchSeries(DFILE, "CRAZ1.Q", craz1)
      lastRaiseDataQtr = sample(2)
      call FetchSeries(DFILE, "MRAZ.Q", mraz)

      ! This will occur only if the series names are in the config.txt file
      do i = 1, AscToInt(trim(EXOG_OUT_ASSUMPT(0)))
         if (trim(EXOG_OUT_ASSUMPT(i)) == "CRAZ1") then 
            call FetchSeries(AFILE, "CRAZ1."//trim(assumpt(6)(2:)), craz1)
            lastRaiseDataQtr = sample(2)
         end if
         if (trim(EXOG_OUT_ASSUMPT(i)) == "MRAZ")  call FetchSeries(AFILE, "MRAZ."//trim(assumpt(6)(2:)), mraz)
         if (trim(EXOG_OUT_ASSUMPT(i)) == "WS") call FetchSeries(AFILE, "WS."//trim(assumpt(6)(2:)), ws)
         if (trim(EXOG_OUT_ASSUMPT(i)) == "YF")  call FetchSeries(AFILE, "YF."//trim(assumpt(6)(2:)), yf)
         if (trim(EXOG_OUT_ASSUMPT(i)) == "YNF") call FetchSeries(AFILE, "YNF."//trim(assumpt(6)(2:)), ynf)
      end do
      
      call SolveModSol2()
      call ReEndogenize()
      call ConvertQuarterlyToAnnual()
      call CreateAdditionalSeries()
      call EconModSol2OutMain()
      call Finish()
                
   end subroutine EconModSol2Main

!===============================================================================

   subroutine InitModSol2()

      call InitializeModSol2Vars()
      
   end subroutine InitModSol2

!===============================================================================

   subroutine AdjustData()
   
      write(*,'(//a//)') "Checking for Adjustments to Endogenous Data, Please Wait..."
   
      !DEC$ IF(.FALSE.)   

        index series #adfile|:*.adj ladj;
        if(#ladj[0]<>0);
          list ladj=#modq2.stochastic*#ladj;
        !end;
        set screen mute no;
        set screen message yes;
        if(#ladj[0]==0);tell'     No Adjustments Found';!end;

        !     Checks to revert adjustments to zero, if desired

        target targ1a;
        if(#ladj[0]<>0);
          tell'\\\Program has determined that the following MODQ2\!
                  endogenous variables will be adjusted';
          tell'\\  Selection';
          tell'   Number     Variables\\';
          for i=1 to #ladj[0];
            tell'     #i        #ladj[#i] ';
          !end;
          tell'\                OR';
          !tell'\   ~'CR~'       Carriage Return - All of above are correct';
          tell'     q        (q)uit';
          tell'\\If all variables are to be adjusted, please enter a ~'CR~'.\!
                To remove adjustment from any one variable please enter\!
                selection number of variable.  Or, enter ~'q~' to (q)uit.';

          accept any param '\\\Enter Selection:';
          if('#param'=='q' or '#param'=='Q');goto fin;!end;
          if('#param'=='');
            goto pass2a;
          !end;
          assign param integer #param;
          list ladj=#ladj-#ladj[#param];
          goto targ1a;
        !end;
        target pass2a;


        !     Incorporate adjustments by combining them with addfactors

        if(#ladj[0]<>0);
          set screen message no;
          for x1=#ladj;
            set per 48 120;
            index #adfile|:#x1|.add ladd1;
            if(#ladd1[0]==0);
              series<#per1 #per2> #x1|.add=0 repeat *;
            !else
              copy #adfile|:#x1|.add;
            !end;
            copy #adfile|:#x1|.adj;
            assign pervb from series dfile:#x1|.q per2;
            assign perv1a from series #x1|.add per1;
            assign perv1b from series #x1|.add per2;
            assign perv2a from series #x1|.adj per1;
            assign perv2b from series #x1|.adj per2;
            assign tb integer time[#pervb];
            assign t1a integer time[#perv1a];
            assign t1b integer time[#perv1b];
            assign t2a integer time[#perv2a];
            assign t2b integer time[#perv2b];



            if(#t1a<#t2a);
              set per #perv1a #perv2a-1;
              series #x1|.adj=0 repeat *;
            !end;
            if(#t1b<#t2b);
              set per #perv1b+1 #perv2b;
              series #x1|.add=0 repeat *;
            !end;
            if(#tb<=#t1b);
              assign perst direct #pervb;
           ! else
              assign perst direct #perv1b;
            !end;
            set per #perst+1 #perv2b;
            series #x1|.add=#x1|.add+#x1|.adj;
          !end;
          set screen message yes;
        !end;

      !DEC$ ENDIF
   
   end subroutine AdjustData

!===============================================================================

   subroutine ExogenizeData()

      write(*,'(//a//)') "Checking for Exogenizing of Endogenous Data, Please Wait..."

      !DEC$ IF(.FALSE.)   
      
        index series #input1|:*.#assumpt[1] lass;
        if(#lass[0]<>0);
          list lass1=#modq2.endogenous*#lass;
        !else
          list lass1=null;
        !end;

        !     Defines list of any MODQ2 variables that will be exogenized by
        !     internal SSA adjustments

        index series #adfile|:*.exg exg;
        if(#exg[0]<>0);
          list exg1=#modq2.endogenous*#exg;
        !else
          list exg1=null;
        !end;
        set screen mute no;
        set screen message yes;
        list exc=null;

        !     Checks to revert exogenous back to endogenous variables.
        !     Copies exogenous variables from assumptions database to workspace.
        !     Renames version name to sq.

        for loop=lass1,exg1;
          target targ1;
          if(##loop[0]<>0);
            tell'\\\Program has determined that the following MODQ2\!
                    endogenous variables will be exogenized by';
            if('#loop'=='lass1');
              tell'outside Assumptions.';
            !else
              tell'internal SSA adjustments.';
            !end;
            tell'\\  Selection';
            tell'   Number     Variables\\';
            for i=1 to ##loop[0];
              tell'     #i        ##loop[#i] ';
            !end;
            tell'\                OR';
            !tell'\   ~'CR~'       Carriage Return - All of above are correct';
            tell'     q        (q)uit';
            tell'\\If all variables are exogenous, please enter a ~'CR~'.\!
                  To change any one variable to endogenous please enter,\!
                  selection number of variable.  Or, enter ~'q~' to (q)uit.';

            accept any param '\\\Enter Selection:';
            if('#param'=='q' or '#param'=='Q');goto fin;!end;
            if('#param'=='');
              set screen message no;
              if('#loop'=='lass1');
                for x2=##loop;
                  copy #input1|:#x2|.#assumpt[1] as #x2|.#sq;
                !end;
              !else
                for x2=##loop;
                  copy #adfile|:#x2|.exg as #x2|.#sq;
                !end;
              !end;
              set screen message yes;
              list exc=##loop;
              goto pass2;
            !end;
            assign param integer #param;
            list #loop=##loop-##loop[#param];
            goto targ1;
          !end;
          target pass2;
        !end;

        abort;

        !     Exogenizes variables

        copy model #efile|:modq2;
        set model name modq2;
        if(#exc[0]==0);tell'\     No Endogenous Variables will be Exogenized';!end;
        set screen message no;

        !NOTE: 5/20/98 

        !Endogenous variables that have been "exogenized" by the EXCLUDE command can't be "re-endogenized".
        !In the past, this did not create a problem since all excluded variables (e.g., EGGEFC) had 
        !exogenous values over the entire projection range. Thus, if "re-endogenizing", the variable should
        !not be exluded. Hence, the list named EXC is set to null.

        if(#exc[0]<>0);
        !  exclude #exc;
           list exc=null;
        !end;

      !DEC$ ENDIF
   
   end subroutine ExogenizeData

!===============================================================================

   subroutine SolveModSol2()
   
      write(*,'(//a//)') "Solving ModSol2, Please Wait..."
      call EconModSol2EquationsMain()
   
   end subroutine SolveModSol2

!===============================================================================

   subroutine ReEndogenize()
   
      real (kind=8), dimension(MAX_QTR) :: temp
      call FetchSeries(DFILE, "EGFC.Q", temp)
      egfc(sample(1):sample(2)) = temp(sample(1):sample(2))
      call FetchSeries(DFILE, "EGEFCPS.Q", temp)
      egefcps(sample(1):sample(2)) = temp(sample(1):sample(2))
   
   end subroutine ReEndogenize

!===============================================================================

   subroutine ConvertQuarterlyToAnnual()
   
      call CollapseAndAppend(awefc_n_a, awefc_n, "AWEFC_N.A")
      call CollapseAndAppend(awse_a, awse, "AWSE.A", .false.)
      call CollapseAndAppend(awsgefc_a, awsgefc, "AWSGEFC.A", .false.)
      call CollapseAndAppend(awsgfc_a, awsgfc, "AWSGFC.A")
      call CollapseAndAppend(awsgfm_a, awsgfm, "AWSGFM.A")
      call CollapseAndAppend(awsggefc_a, awsggefc, "AWSGGEFC.A")
      call CollapseAndAppend(awsggesl_a, awsggesl, "AWSGGESL.A")
      call CollapseAndAppend(awsp_a, awsp, "AWSP.A", .false.)
      call CollapseAndAppend(awspf_a, awspf, "AWSPF.A")
      call CollapseAndAppend(awsph_a, awsph, "AWSPH.A")
      call CollapseAndAppend(awspl_a, awspl, "AWSPL.A", .false.)
      call CollapseAndAppend(awssp_a, awssp, "AWSSP.A", .false.)
      call CollapseAndAppend(awsspbnfxge_a, awsspbnfxge, "AWSSPBNFXGE.A")
      call CollapseAndAppend(awsspes_a, awsspes, "AWSSPES.A")
      call CollapseAndAppend(awsspf_a, awsspf, "AWSSPF.A")
      call CollapseAndAppend(awssphs_a, awssphs, "AWSSPHS.A")
      call CollapseAndAppend(awsspl_a, awsspl, "AWSSPL.A", .false.)
      call CollapseAndAppend(awsspss_a, awsspss, "AWSSPSS.A")
      call CollapseAndAppend(awsui_a, awsui, "AWSUI.A", .false.)
      call CollapseAndAppend(ayf_a, ayf, "AYF.A")
      call CollapseAndAppend(ayf_k_a, ayf_k, "AYF_K.A")
      call CollapseAndAppend(aynf_a, aynf, "AYNF.A")
      call CollapseAndAppend(aynf_k_a, aynf_k, "AYNF_K.A", .false.)
      call CollapseAndAppend(cfcgefc_a, cfcgefc, "CFCGEFC.A")
      call CollapseAndAppend(cfcgesl_a, cfcgesl, "CFCGESL.A")
      call CollapseAndAppend(cfcgfc_a, cfcgfc, "CFCGFC.A")
      call CollapseAndAppend(cfcgfm_a, cfcgfm, "CFCGFM.A")
      call CollapseAndAppend(cfcgsl_a, cfcgsl, "CFCGSL.A")
      call CollapseAndAppend(cpiwms_a, cpiwms, "CPIWMS.A")
      call CollapseAndAppend(craz1_a, craz1, "CRAZ1.A", .false.)
      call CollapseAndAppend(cr_ui_a, cr_ui, "CR_UI.A", .false.)
      call CollapseAndAppend(dnedmil_a, dnedmil, "DNEDMIL.A", .false.)
      call CollapseAndAppend(ea_a, ea, "EA.A")
      call CollapseAndAppend(eas_a(0,0,:), eas(0,0,:), "EAS.A", .false.)
      call CollapseAndAppend(eas_r_a(0,0,:), eas_r(0,0,:), "EAS_R.A", .false.)
      call CollapseAndAppend(eau_a(0,0,:), eau(0,0,:), "EAU.A")
      call CollapseAndAppend(eau_r_a(0,0,:), eau_r(0,0,:), "EAU_R.A", .false.)
      call CollapseAndAppend(eaw_a(0,0,:), eaw(0,0,:), "EAW.A")
      call CollapseAndAppend(eaw_r_a(0,0,:), eaw_r(0,0,:), "EAW_R.A", .false.)
      call CollapseAndAppend(eas_a(2,1,:), eas(2,1,:), "EF1617AS.A")
      call CollapseAndAppend(eas_r_a(2,1,:), eas_r(2,1,:), "EF1617AS_R.A", .false.)
      call CollapseAndAppend(eau_a(2,1,:), eau(2,1,:), "EF1617AU.A")
      call CollapseAndAppend(eau_r_a(2,1,:), eau_r(2,1,:), "EF1617AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(2,1,:), eaw(2,1,:), "EF1617AW.A")
      call CollapseAndAppend(eaw_r_a(2,1,:), eaw_r(2,1,:), "EF1617AW_R.A", .false.)
      call CollapseAndAppend(enas_a(2,1,:), enas(2,1,:), "EF1617NAS.A")
      call CollapseAndAppend(enas_r_a(2,1,:), enas_r(2,1,:), "EF1617NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(2,1,:), enau(2,1,:), "EF1617NAU.A")
      call CollapseAndAppend(enau_r_a(2,1,:), enau_r(2,1,:), "EF1617NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(2,1,:), enawph(2,1,:), "EF1617NAWPH.A")
      call CollapseAndAppend(enawph_r_a(2,1,:), enawph_r(2,1,:), "EF1617NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(2,2,:), eas(2,2,:), "EF1819AS.A")
      call CollapseAndAppend(eas_r_a(2,2,:), eas_r(2,2,:), "EF1819AS_R.A", .false.)
      call CollapseAndAppend(eau_a(2,2,:), eau(2,2,:), "EF1819AU.A")
      call CollapseAndAppend(eau_r_a(2,2,:), eau_r(2,2,:), "EF1819AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(2,2,:), eaw(2,2,:), "EF1819AW.A")
      call CollapseAndAppend(eaw_r_a(2,2,:), eaw_r(2,2,:), "EF1819AW_R.A", .false.)
      call CollapseAndAppend(enas_a(2,2,:), enas(2,2,:), "EF1819NAS.A")
      call CollapseAndAppend(enas_r_a(2,2,:), enas_r(2,2,:), "EF1819NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(2,2,:), enau(2,2,:), "EF1819NAU.A")
      call CollapseAndAppend(enau_r_a(2,2,:), enau_r(2,2,:), "EF1819NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(2,2,:), enawph(2,2,:), "EF1819NAWPH.A")
      call CollapseAndAppend(enawph_r_a(2,2,:), enawph_r(2,2,:), "EF1819NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(2,3,:), eas(2,3,:), "EF2024AS.A")
      call CollapseAndAppend(eas_r_a(2,3,:), eas_r(2,3,:), "EF2024AS_R.A", .false.)
      call CollapseAndAppend(eau_a(2,3,:), eau(2,3,:), "EF2024AU.A")
      call CollapseAndAppend(eau_r_a(2,3,:), eau_r(2,3,:), "EF2024AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(2,3,:), eaw(2,3,:), "EF2024AW.A")
      call CollapseAndAppend(eaw_r_a(2,3,:), eaw_r(2,3,:), "EF2024AW_R.A", .false.)
      call CollapseAndAppend(enas_a(2,3,:), enas(2,3,:), "EF2024NAS.A")
      call CollapseAndAppend(enas_r_a(2,3,:), enas_r(2,3,:), "EF2024NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(2,3,:), enau(2,3,:), "EF2024NAU.A")
      call CollapseAndAppend(enau_r_a(2,3,:), enau_r(2,3,:), "EF2024NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(2,3,:), enawph(2,3,:), "EF2024NAWPH.A")
      call CollapseAndAppend(enawph_r_a(2,3,:), enawph_r(2,3,:), "EF2024NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(2,18,:), eas(2,18,:), "EF2534AS.A")
      call CollapseAndAppend(eas_r_a(2,18,:), eas_r(2,18,:), "EF2534AS_R.A", .false.)
      call CollapseAndAppend(eau_a(2,18,:), eau(2,18,:), "EF2534AU.A")
      call CollapseAndAppend(eau_r_a(2,18,:), eau_r(2,18,:), "EF2534AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(2,18,:), eaw(2,18,:), "EF2534AW.A")
      call CollapseAndAppend(eaw_r_a(2,18,:), eaw_r(2,18,:), "EF2534AW_R.A", .false.)
      call CollapseAndAppend(enas_a(2,18,:), enas(2,18,:), "EF2534NAS.A")
      call CollapseAndAppend(enas_r_a(2,18,:), enas_r(2,18,:), "EF2534NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(2,18,:), enau(2,18,:), "EF2534NAU.A")
      call CollapseAndAppend(enau_r_a(2,18,:), enau_r(2,18,:), "EF2534NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(2,18,:), enawph(2,18,:), "EF2534NAWPH.A")
      call CollapseAndAppend(enawph_r_a(2,18,:), enawph_r(2,18,:), "EF2534NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(2,19,:), eas(2,19,:), "EF3544AS.A")
      call CollapseAndAppend(eas_r_a(2,19,:), eas_r(2,19,:), "EF3544AS_R.A", .false.)
      call CollapseAndAppend(eau_a(2,19,:), eau(2,19,:), "EF3544AU.A")
      call CollapseAndAppend(eau_r_a(2,19,:), eau_r(2,19,:), "EF3544AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(2,19,:), eaw(2,19,:), "EF3544AW.A")
      call CollapseAndAppend(eaw_r_a(2,19,:), eaw_r(2,19,:), "EF3544AW_R.A", .false.)
      call CollapseAndAppend(enas_a(2,19,:), enas(2,19,:), "EF3544NAS.A")
      call CollapseAndAppend(enas_r_a(2,19,:), enas_r(2,19,:), "EF3544NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(2,19,:), enau(2,19,:), "EF3544NAU.A")
      call CollapseAndAppend(enau_r_a(2,19,:), enau_r(2,19,:), "EF3544NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(2,19,:), enawph(2,19,:), "EF3544NAWPH.A")
      call CollapseAndAppend(enawph_r_a(2,19,:), enawph_r(2,19,:), "EF3544NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(2,20,:), eas(2,20,:), "EF4554AS.A")
      call CollapseAndAppend(eas_r_a(2,20,:), eas_r(2,20,:), "EF4554AS_R.A", .false.)
      call CollapseAndAppend(eau_a(2,20,:), eau(2,20,:), "EF4554AU.A")
      call CollapseAndAppend(eau_r_a(2,20,:), eau_r(2,20,:), "EF4554AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(2,20,:), eaw(2,20,:), "EF4554AW.A")
      call CollapseAndAppend(eaw_r_a(2,20,:), eaw_r(2,20,:), "EF4554AW_R.A", .false.)
      call CollapseAndAppend(enas_a(2,20,:), enas(2,20,:), "EF4554NAS.A")
      call CollapseAndAppend(enas_r_a(2,20,:), enas_r(2,20,:), "EF4554NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(2,20,:), enau(2,20,:), "EF4554NAU.A")
      call CollapseAndAppend(enau_r_a(2,20,:), enau_r(2,20,:), "EF4554NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(2,20,:), enawph(2,20,:), "EF4554NAWPH.A")
      call CollapseAndAppend(enawph_r_a(2,20,:), enawph_r(2,20,:), "EF4554NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(2,21,:), eas(2,21,:), "EF5564AS.A")
      call CollapseAndAppend(eas_r_a(2,21,:), eas_r(2,21,:), "EF5564AS_R.A", .false.)
      call CollapseAndAppend(eau_a(2,21,:), eau(2,21,:), "EF5564AU.A")
      call CollapseAndAppend(eau_r_a(2,21,:), eau_r(2,21,:), "EF5564AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(2,21,:), eaw(2,21,:), "EF5564AW.A")
      call CollapseAndAppend(eaw_r_a(2,21,:), eaw_r(2,21,:), "EF5564AW_R.A", .false.)
      call CollapseAndAppend(enas_a(2,21,:), enas(2,21,:), "EF5564NAS.A")
      call CollapseAndAppend(enas_r_a(2,21,:), enas_r(2,21,:), "EF5564NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(2,21,:), enau(2,21,:), "EF5564NAU.A")
      call CollapseAndAppend(enau_r_a(2,21,:), enau_r(2,21,:), "EF5564NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(2,21,:), enawph(2,21,:), "EF5564NAWPH.A")
      call CollapseAndAppend(enawph_r_a(2,21,:), enawph_r(2,21,:), "EF5564NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(2,22,:), eas(2,22,:), "EF65OAS.A")
      call CollapseAndAppend(eas_r_a(2,22,:), eas_r(2,22,:), "EF65OAS_R.A", .false.)
      call CollapseAndAppend(eau_a(2,22,:), eau(2,22,:), "EF65OAU.A")
      call CollapseAndAppend(eau_r_a(2,22,:), eau_r(2,22,:), "EF65OAU_R.A", .false.)
      call CollapseAndAppend(eaw_a(2,22,:), eaw(2,22,:), "EF65OAW.A")
      call CollapseAndAppend(eaw_r_a(2,22,:), eaw_r(2,22,:), "EF65OAW_R.A", .false.)
      call CollapseAndAppend(enas_a(2,22,:), enas(2,22,:), "EF65ONAS.A")
      call CollapseAndAppend(enas_r_a(2,22,:), enas_r(2,22,:), "EF65ONAS_R.A", .false.)
      call CollapseAndAppend(enau_a(2,22,:), enau(2,22,:), "EF65ONAU.A")
      call CollapseAndAppend(enau_r_a(2,22,:), enau_r(2,22,:), "EF65ONAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(2,22,:), enawph(2,22,:), "EF65ONAWPH.A")
      call CollapseAndAppend(enawph_r_a(2,22,:), enawph_r(2,22,:), "EF65ONAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(2,0,:), eas(2,0,:), "EFAS.A")
      call CollapseAndAppend(eau_a(2,0,:), eau(2,0,:), "EFAU.A")
      call CollapseAndAppend(eaw_a(2,0,:), eaw(2,0,:), "EFAW.A")
      call CollapseAndAppend(enas_a(2,0,:), enas(2,0,:), "EFNAS.A")
      call CollapseAndAppend(enau_a(2,0,:), enau(2,0,:), "EFNAU.A")
      call CollapseAndAppend(enawph_a(2,0,:), enawph(2,0,:), "EFNAWPH.A")
      call CollapseAndAppend(egefcps_a, egefcps, "EGEFCPS.A")
      call CollapseAndAppend(egfc_a, egfc, "EGFC.A")
      call CollapseAndAppend(eggefc_a, eggefc, "EGGEFC.A")
      call CollapseAndAppend(eggesl_a, eggesl, "EGGESL.A")
      call CollapseAndAppend(eas_a(1,1,:), eas(1,1,:), "EM1617AS.A")
      call CollapseAndAppend(eas_r_a(1,1,:), eas_r(1,1,:), "EM1617AS_R.A", .false.)
      call CollapseAndAppend(eau_a(1,1,:), eau(1,1,:), "EM1617AU.A")
      call CollapseAndAppend(eau_r_a(1,1,:), eau_r(1,1,:), "EM1617AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(1,1,:), eaw(1,1,:), "EM1617AW.A")
      call CollapseAndAppend(eaw_r_a(1,1,:), eaw_r(1,1,:), "EM1617AW_R.A", .false.)
      call CollapseAndAppend(enas_a(1,1,:), enas(1,1,:), "EM1617NAS.A")
      call CollapseAndAppend(enas_r_a(1,1,:), enas_r(1,1,:), "EM1617NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(1,1,:), enau(1,1,:), "EM1617NAU.A")
      call CollapseAndAppend(enau_r_a(1,1,:), enau_r(1,1,:), "EM1617NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(1,1,:), enawph(1,1,:), "EM1617NAWPH.A")
      call CollapseAndAppend(enawph_r_a(1,1,:), enawph_r(1,1,:), "EM1617NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(1,2,:), eas(1,2,:), "EM1819AS.A")
      call CollapseAndAppend(eas_r_a(1,2,:), eas_r(1,2,:), "EM1819AS_R.A", .false.)
      call CollapseAndAppend(eau_a(1,2,:), eau(1,2,:), "EM1819AU.A")
      call CollapseAndAppend(eau_r_a(1,2,:), eau_r(1,2,:), "EM1819AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(1,2,:), eaw(1,2,:), "EM1819AW.A")
      call CollapseAndAppend(eaw_r_a(1,2,:), eaw_r(1,2,:), "EM1819AW_R.A", .false.)
      call CollapseAndAppend(enas_a(1,2,:), enas(1,2,:), "EM1819NAS.A")
      call CollapseAndAppend(enas_r_a(1,2,:), enas_r(1,2,:), "EM1819NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(1,2,:), enau(1,2,:), "EM1819NAU.A")
      call CollapseAndAppend(enau_r_a(1,2,:), enau_r(1,2,:), "EM1819NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(1,2,:), enawph(1,2,:), "EM1819NAWPH.A")
      call CollapseAndAppend(enawph_r_a(1,2,:), enawph_r(1,2,:), "EM1819NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(1,3,:), eas(1,3,:), "EM2024AS.A")
      call CollapseAndAppend(eas_r_a(1,3,:), eas_r(1,3,:), "EM2024AS_R.A", .false.)
      call CollapseAndAppend(eau_a(1,3,:), eau(1,3,:), "EM2024AU.A")
      call CollapseAndAppend(eau_r_a(1,3,:), eau_r(1,3,:), "EM2024AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(1,3,:), eaw(1,3,:), "EM2024AW.A")
      call CollapseAndAppend(eaw_r_a(1,3,:), eaw_r(1,3,:), "EM2024AW_R.A", .false.)
      call CollapseAndAppend(enas_a(1,3,:), enas(1,3,:), "EM2024NAS.A")
      call CollapseAndAppend(enas_r_a(1,3,:), enas_r(1,3,:), "EM2024NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(1,3,:), enau(1,3,:), "EM2024NAU.A")
      call CollapseAndAppend(enau_r_a(1,3,:), enau_r(1,3,:), "EM2024NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(1,3,:), enawph(1,3,:), "EM2024NAWPH.A")
      call CollapseAndAppend(enawph_r_a(1,3,:), enawph_r(1,3,:), "EM2024NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(1,18,:), eas(1,18,:), "EM2534AS.A")
      call CollapseAndAppend(eas_r_a(1,18,:), eas_r(1,18,:), "EM2534AS_R.A", .false.)
      call CollapseAndAppend(eau_a(1,18,:), eau(1,18,:), "EM2534AU.A")
      call CollapseAndAppend(eau_r_a(1,18,:), eau_r(1,18,:), "EM2534AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(1,18,:), eaw(1,18,:), "EM2534AW.A")
      call CollapseAndAppend(eaw_r_a(1,18,:), eaw_r(1,18,:), "EM2534AW_R.A", .false.)
      call CollapseAndAppend(enas_a(1,18,:), enas(1,18,:), "EM2534NAS.A")
      call CollapseAndAppend(enas_r_a(1,18,:), enas_r(1,18,:), "EM2534NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(1,18,:), enau(1,18,:), "EM2534NAU.A")
      call CollapseAndAppend(enau_r_a(1,18,:), enau_r(1,18,:), "EM2534NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(1,18,:), enawph(1,18,:), "EM2534NAWPH.A")
      call CollapseAndAppend(enawph_r_a(1,18,:), enawph_r(1,18,:), "EM2534NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(1,19,:), eas(1,19,:), "EM3544AS.A")
      call CollapseAndAppend(eas_r_a(1,19,:), eas_r(1,19,:), "EM3544AS_R.A", .false.)
      call CollapseAndAppend(eau_a(1,19,:), eau(1,19,:), "EM3544AU.A")
      call CollapseAndAppend(eau_r_a(1,19,:), eau_r(1,19,:), "EM3544AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(1,19,:), eaw(1,19,:), "EM3544AW.A")
      call CollapseAndAppend(eaw_r_a(1,19,:), eaw_r(1,19,:), "EM3544AW_R.A", .false.)
      call CollapseAndAppend(enas_a(1,19,:), enas(1,19,:), "EM3544NAS.A")
      call CollapseAndAppend(enas_r_a(1,19,:), enas_r(1,19,:), "EM3544NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(1,19,:), enau(1,19,:), "EM3544NAU.A")
      call CollapseAndAppend(enau_r_a(1,19,:), enau_r(1,19,:), "EM3544NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(1,19,:), enawph(1,19,:), "EM3544NAWPH.A")
      call CollapseAndAppend(enawph_r_a(1,19,:), enawph_r(1,19,:), "EM3544NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(1,20,:), eas(1,20,:), "EM4554AS.A")
      call CollapseAndAppend(eas_r_a(1,20,:), eas_r(1,20,:), "EM4554AS_R.A", .false.)
      call CollapseAndAppend(eau_a(1,20,:), eau(1,20,:), "EM4554AU.A")
      call CollapseAndAppend(eau_r_a(1,20,:), eau_r(1,20,:), "EM4554AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(1,20,:), eaw(1,20,:), "EM4554AW.A")
      call CollapseAndAppend(eaw_r_a(1,20,:), eaw_r(1,20,:), "EM4554AW_R.A", .false.)
      call CollapseAndAppend(enas_a(1,20,:), enas(1,20,:), "EM4554NAS.A")
      call CollapseAndAppend(enas_r_a(1,20,:), enas_r(1,20,:), "EM4554NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(1,20,:), enau(1,20,:), "EM4554NAU.A")
      call CollapseAndAppend(enau_r_a(1,20,:), enau_r(1,20,:), "EM4554NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(1,20,:), enawph(1,20,:), "EM4554NAWPH.A")
      call CollapseAndAppend(enawph_r_a(1,20,:), enawph_r(1,20,:), "EM4554NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(1,21,:), eas(1,21,:), "EM5564AS.A")
      call CollapseAndAppend(eas_r_a(1,21,:), eas_r(1,21,:), "EM5564AS_R.A", .false.)
      call CollapseAndAppend(eau_a(1,21,:), eau(1,21,:), "EM5564AU.A")
      call CollapseAndAppend(eau_r_a(1,21,:), eau_r(1,21,:), "EM5564AU_R.A", .false.)
      call CollapseAndAppend(eaw_a(1,21,:), eaw(1,21,:), "EM5564AW.A")
      call CollapseAndAppend(eaw_r_a(1,21,:), eaw_r(1,21,:), "EM5564AW_R.A", .false.)
      call CollapseAndAppend(enas_a(1,21,:), enas(1,21,:), "EM5564NAS.A")
      call CollapseAndAppend(enas_r_a(1,21,:), enas_r(1,21,:), "EM5564NAS_R.A", .false.)
      call CollapseAndAppend(enau_a(1,21,:), enau(1,21,:), "EM5564NAU.A")
      call CollapseAndAppend(enau_r_a(1,21,:), enau_r(1,21,:), "EM5564NAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(1,21,:), enawph(1,21,:), "EM5564NAWPH.A")
      call CollapseAndAppend(enawph_r_a(1,21,:), enawph_r(1,21,:), "EM5564NAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(1,22,:), eas(1,22,:), "EM65OAS.A")
      call CollapseAndAppend(eas_r_a(1,22,:), eas_r(1,22,:), "EM65OAS_R.A", .false.)
      call CollapseAndAppend(eau_a(1,22,:), eau(1,22,:), "EM65OAU.A")
      call CollapseAndAppend(eau_r_a(1,22,:), eau_r(1,22,:), "EM65OAU_R.A", .false.)
      call CollapseAndAppend(eaw_a(1,22,:), eaw(1,22,:), "EM65OAW.A")
      call CollapseAndAppend(eaw_r_a(1,22,:), eaw_r(1,22,:), "EM65OAW_R.A", .false.)
      call CollapseAndAppend(enas_a(1,22,:), enas(1,22,:), "EM65ONAS.A")
      call CollapseAndAppend(enas_r_a(1,22,:), enas_r(1,22,:), "EM65ONAS_R.A", .false.)
      call CollapseAndAppend(enau_a(1,22,:), enau(1,22,:), "EM65ONAU.A")
      call CollapseAndAppend(enau_r_a(1,22,:), enau_r(1,22,:), "EM65ONAU_R.A", .false.)
      call CollapseAndAppend(enawph_a(1,22,:), enawph(1,22,:), "EM65ONAWPH.A")
      call CollapseAndAppend(enawph_r_a(1,22,:), enawph_r(1,22,:), "EM65ONAWPH_R.A", .false.)
      call CollapseAndAppend(eas_a(1,0,:), eas(1,0,:), "EMAS.A")
      call CollapseAndAppend(eau_a(1,0,:), eau(1,0,:), "EMAU.A")
      call CollapseAndAppend(eaw_a(1,0,:), eaw(1,0,:), "EMAW.A")
      call CollapseAndAppend(enas_a(1,0,:), enas(1,0,:), "EMNAS.A")
      call CollapseAndAppend(enau_a(1,0,:), enau(1,0,:), "EMNAU.A")
      call CollapseAndAppend(enawph_a(1,0,:), enawph(1,0,:), "EMNAWPH.A")
      call CollapseAndAppend(ena_a, ena, "ENA.A")
      call CollapseAndAppend(enas_a(0,0,:), enas(0,0,:), "ENAS.A")
      call CollapseAndAppend(enas_r_a(0,0,:), enas_r(0,0,:), "ENAS_R.A", .false.)
      call CollapseAndAppend(enau_a(0,0,:), enau(0,0,:), "ENAU.A")
      call CollapseAndAppend(enau_r_a(0,0,:), enau_r(0,0,:), "ENAU_R.A", .false.)
      call CollapseAndAppend(enaw_a, enaw, "ENAW.A", .false.)
      call CollapseAndAppend(enawpbxge_a, enawpbxge, "ENAWPBXGE.A")
      call CollapseAndAppend(enawph_a(0,0,:), enawph(0,0,:), "ENAWPH.A")
      call CollapseAndAppend(enawph_r_a(0,0,:), enawph_r(0,0,:), "ENAWPH_R.A", .false.)
      call CollapseAndAppend(enawspbxge_a, enawspbxge, "ENAWSPBXGE.A", .false.)
      call CollapseAndAppend(ep_a, ep, "EP.A")
      call CollapseAndAppend(epes_est_a, epes_est, "EPES_EST.A")
      call CollapseAndAppend(ephs_est_a, ephs_est, "EPHS_EST.A")
      call CollapseAndAppend(epss_est_a, epss_est, "EPSS_EST.A")
      call CollapseAndAppend(gdpg_a, gdpg, "GDPG.A")
      call CollapseAndAppend(gdpge_a, gdpge, "GDPGE.A")
      call CollapseAndAppend(gdpgefc_a, gdpgefc, "GDPGEFC.A")
      call CollapseAndAppend(gdpgesl_a, gdpgesl, "GDPGESL.A")
      call CollapseAndAppend(gdpgf_a, gdpgf, "GDPGF.A")
      call CollapseAndAppend(gdpgfc_a, gdpgfc, "GDPGFC.A")
      call CollapseAndAppend(gdpgfm_a, gdpgfm, "GDPGFM.A")
      call CollapseAndAppend(gdpgge_a, gdpgge, "GDPGGE.A")
      call CollapseAndAppend(gdpggefc_a, gdpggefc, "GDPGGEFC.A")
      call CollapseAndAppend(gdpggesl_a, gdpggesl, "GDPGGESL.A")
      call CollapseAndAppend(gdpgsl_a, gdpgsl, "GDPGSL.A")
      call CollapseAndAppend(gdppbnfxge_a, gdppbnfxge, "GDPPBNFXGE.A")
      call CollapseAndAppend(gdppf_a, gdppf, "GDPPF.A")
      call CollapseAndAppend(gdppfreal_a, gdppfreal, "GDPPF"//REAL_GDP_SERIES(4:5)//".A")
      call CollapseAndAppend(gdpph_a, gdpph, "GDPPH.A")
      call CollapseAndAppend(gdppni_a, gdppni, "GDPPNI.A")
      call CollapseAndAppend(hifc_l_a, hifc_l, "HIFC_L.A", .false.)
      call CollapseAndAppend(hifm_l_a, hifm_l, "HIFM_L.A", .false.)
      call CollapseAndAppend(hip_l_a, hip_l, "HIP_L.A", .false.)
      call CollapseAndAppend(hisl_l_a, hisl_l, "HISL_L.A", .false.)
      call CollapseAndAppend(mraz_a, mraz, "MRAZ.A", .false.)
      call CollapseAndAppend(oasdifc_l_a, oasdifc_l, "OASDIFC_L.A", .false.)
      call CollapseAndAppend(oasdifm_l_a, oasdifm_l, "OASDIFM_L.A", .false.)
      call CollapseAndAppend(oasdip_l_a, oasdip_l, "OASDIP_L.A", .false.)
      call CollapseAndAppend(oasdisl_l_a, oasdisl_l, "OASDISL_L.A", .false.)
      call CollapseAndAppend(oli_a, oli, "OLI.A")
      call CollapseAndAppend(oli_csrs1_a, oli_csrs1, "OLI_CSRS1.A", .false.)
      call CollapseAndAppend(oli_fc_a, oli_fc, "OLI_FC.A")
      call CollapseAndAppend(oli_fers1_a, oli_fers1, "OLI_FERS1.A", .false.)
      call CollapseAndAppend(oli_fersfc_a, oli_fersfc, "OLI_FERSFC.A")
      call CollapseAndAppend(oli_gge_a, oli_gge, "OLI_GGE.A")
      call CollapseAndAppend(oli_ghi_a, oli_ghi, "OLI_GHI.A")
      call CollapseAndAppend(oli_ghi_fc_a, oli_ghi_fc, "OLI_GHI_FC.A")
      call CollapseAndAppend(oli_ghi_p_a, oli_ghi_p, "OLI_GHI_P.A")
      call CollapseAndAppend(oli_ghi_sl_a, oli_ghi_sl, "OLI_GHI_SL.A")
      call CollapseAndAppend(oli_gli_a, oli_gli, "OLI_GLI.A")
      call CollapseAndAppend(oli_gli_fc_a, oli_gli_fc, "OLI_GLI_FC.A")
      call CollapseAndAppend(oli_gli_p_a, oli_gli_p, "OLI_GLI_P.A")
      call CollapseAndAppend(oli_gli_sl_a, oli_gli_sl, "OLI_GLI_SL.A")
      call CollapseAndAppend(oli_p_a, oli_p, "OLI_P.A")
      call CollapseAndAppend(oli_ppps_a, oli_ppps, "OLI_PPPS.A")
      call CollapseAndAppend(oli_pps_a, oli_pps, "OLI_PPS.A")
      call CollapseAndAppend(oli_retfc_a, oli_retfc, "OLI_RETFC.A")
      call CollapseAndAppend(oli_retfm_a, oli_retfm, "OLI_RETFM.A")
      call CollapseAndAppend(oli_retsl_a, oli_retsl, "OLI_RETSL.A")
      call CollapseAndAppend(oli_sl_a, oli_sl, "OLI_SL.A")
      call CollapseAndAppend(oli_su_a, oli_su, "OLI_SU.A")
      call CollapseAndAppend(oli_wc_a, oli_wc, "OLI_WC.A")
      call CollapseAndAppend(oli_wcp_a, oli_wcp, "OLI_WCP.A")
      call CollapseAndAppend(oli_wcsl_a, oli_wcsl, "OLI_WCSL.A")
      call CollapseAndAppend(ooh_a, ooh, "OOH.A", .false.)
      call CollapseAndAppend(pgdpaf_a, pgdpaf, "PGDPAF.A")
      call CollapseAndAppend(rcwsf_a, rcwsf, "RCWSF.A", .false.)
      call CollapseAndAppend(rcwsm_a, rcwsm, "RCWSM.A", .false.)
      call CollapseAndAppend(rcwsp_a, rcwsp, "RCWSP.A", .false.)
      call CollapseAndAppend(rcwssl_a, rcwssl, "RCWSSL.A", .false.)
      call CollapseAndAppend(relmax_ui_a, relmax_ui, "RELMAX_UI.A", .false.)
      call CollapseAndAppend(rhip_l_a, rhip_l, "RHIP_L.A", .false.)
      call CollapseAndAppend(roasdip_l_a, roasdip_l, "ROASDIP_L.A", .false.)
      call CollapseAndAppend(roli_ppps_a, roli_ppps, "ROLI_PPPS.A")
      call CollapseAndAppend(roli_su_a, roli_su, "ROLI_SU.A", .false.)
      call CollapseAndAppend(roli_wcp_a, roli_wcp, "ROLI_WCP.A", .false.)
      call CollapseAndAppend(rsocf_pbg_a, rsocf_pbg, "RSOCF_PBG.A", .false.)
      call CollapseAndAppend(rsocsl_wc_a, rsocsl_wc, "RSOCSL_WC.A", .false.)
      call CollapseAndAppend(rsoc_uip_a, rsoc_uip, "RSOC_UIP.A", .false.)
      call CollapseAndAppend(rsoc_wcp_a, rsoc_wcp, "RSOC_WCP.A", .false.)
      call CollapseAndAppend(ruiws1_a, ruiws1, "RUIWS1.A")
      call CollapseAndAppend(ruiws2_a, ruiws2, "RUIWS2.A")
      call CollapseAndAppend(rwcws_a, rwcws, "RWCWS.A")
      call CollapseAndAppend(rwsspbnfxge_a, rwsspbnfxge, "RWSSPBNFXGE.A")
      call CollapseAndAppend(soc_a, soc, "SOC.A")
      call CollapseAndAppend(socf_hi_a, socf_hi, "SOCF_HI.A")
      call CollapseAndAppend(socf_mifm_a, socf_mifm, "SOCF_MIFM.A")
      call CollapseAndAppend(socf_oasdi_a, socf_oasdi, "SOCF_OASDI.A")
      call CollapseAndAppend(socf_pbg_a, socf_pbg, "SOCF_PBG.A")
      call CollapseAndAppend(socf_retrr_a, socf_retrr, "SOCF_RETRR.A")
      call CollapseAndAppend(socf_uif_a, socf_uif, "SOCF_UIF.A")
      call CollapseAndAppend(socf_uifc_a, socf_uifc, "SOCF_UIFC.A")
      call CollapseAndAppend(socf_uifed_a, socf_uifed, "SOCF_UIFED.A")
      call CollapseAndAppend(socf_uifm_a, socf_uifm, "SOCF_UIFM.A")
      call CollapseAndAppend(socf_uis_a, socf_uis, "SOCF_UIS.A")
      call CollapseAndAppend(socf_wc_a, socf_wc, "SOCF_WC.A")
      call CollapseAndAppend(socsl_wc_a, socsl_wc, "SOCSL_WC.A")
      call CollapseAndAppend(soc_fc_a, soc_fc, "SOC_FC.A")
      call CollapseAndAppend(soc_fm_a, soc_fm, "SOC_FM.A")
      call CollapseAndAppend(soc_gge_a, soc_gge, "SOC_GGE.A")
      call CollapseAndAppend(soc_p_a, soc_p, "SOC_P.A")
      call CollapseAndAppend(soc_sl_a, soc_sl, "SOC_SL.A")
      call CollapseAndAppend(soc_uip_a, soc_uip, "SOC_UIP.A")
      call CollapseAndAppend(soc_uisl_a, soc_uisl, "SOC_UISL.A")
      call CollapseAndAppend(soc_wcp_a, soc_wcp, "SOC_WCP.A")
      call CollapseAndAppend(soc_wcsl_a, soc_wcsl, "SOC_WCSL.A")
      call CollapseAndAppend(taxmax_a, taxmax, "TAXMAX.A")
      call CollapseAndAppend(tmaxui_sl_a, tmaxui_sl, "TMAXUI_SL.A")
      call CollapseAndAppend(trate_ui_a, trate_ui, "TRATE_UI.A", .false.)
      call CollapseAndAppend(tratio_ui_a, tratio_ui, "TRATIO_UI.A", .false.)
      call CollapseAndAppend(wefc_n_a, wefc_n, "WEFC_N.A")
      call CollapseAndAppend(ws_a, ws, "WS.A")
      call CollapseAndAppend(wsd_a, wsd, "WSD.A")
      call CollapseAndAppend(wsdp_a, wsdp, "WSDP.A")
      call CollapseAndAppend(wsgefc_a, wsgefc, "WSGEFC.A")
      call CollapseAndAppend(wsgfc_a, wsgfc, "WSGFC.A")
      call CollapseAndAppend(wsgfm_a, wsgfm, "WSGFM.A")
      call CollapseAndAppend(wsggefc_a, wsggefc, "WSGGEFC.A")
      call CollapseAndAppend(wsggesl_a, wsggesl, "WSGGESL.A")
      call CollapseAndAppend(wsp_a, wsp, "WSP.A")
      call CollapseAndAppend(wspf_a, wspf, "WSPF.A")
      call CollapseAndAppend(wsph_a, wsph, "WSPH.A")
      call CollapseAndAppend(wspni_a, wspni, "WSPNI.A")
      call CollapseAndAppend(wss_a, wss, "WSS.A")
      call CollapseAndAppend(wssg_a, wssg, "WSSG.A")
      call CollapseAndAppend(wssge_a, wssge, "WSSGE.A")
      call CollapseAndAppend(wssgefc_a, wssgefc, "WSSGEFC.A")
      call CollapseAndAppend(wssgesl_a, wssgesl, "WSSGESL.A")
      call CollapseAndAppend(wssgf_a, wssgf, "WSSGF.A")
      call CollapseAndAppend(wssgfc_a, wssgfc, "WSSGFC.A")
      call CollapseAndAppend(wssgfm_a, wssgfm, "WSSGFM.A")
      call CollapseAndAppend(wssgge_a, wssgge, "WSSGGE.A")
      call CollapseAndAppend(wssggefc_a, wssggefc, "WSSGGEFC.A")
      call CollapseAndAppend(wssggesl_a, wssggesl, "WSSGGESL.A")
      call CollapseAndAppend(wssgsl_a, wssgsl, "WSSGSL.A")
      call CollapseAndAppend(wssp_a, wssp, "WSSP.A")
      call CollapseAndAppend(wsspbnfxge_a, wsspbnfxge, "WSSPBNFXGE.A")
      call CollapseAndAppend(wsspes_a, wsspes, "WSSPES.A")
      call CollapseAndAppend(wsspf_a, wsspf, "WSSPF.A")
      call CollapseAndAppend(wssph_a, wssph, "WSSPH.A")
      call CollapseAndAppend(wssphs_a, wssphs, "WSSPHS.A")
      call CollapseAndAppend(wsspni_a, wsspni, "WSSPNI.A")
      call CollapseAndAppend(wsspss_a, wsspss, "WSSPSS.A")
      call CollapseAndAppend(yf_a, yf, "YF.A")
      call CollapseAndAppend(ynf_a, ynf, "YNF.A")

   end subroutine ConvertQuarterlyToAnnual

!===============================================================================

   subroutine CollapseAndAppend(annualSeries, quarterlySeries, seriesName, append)
   
      real (kind = 8), dimension(MAX_YR) :: annualSeries
      real (kind = 8), dimension(MAX_QTR) :: quarterlySeries
      character (len = *) :: seriesName
      logical, optional :: append
      real (kind = 8), dimension(MAX_YR) :: tempSeries
   
      tempSeries = 0d0
      call collapse(annualSeries, quarterlySeries)
      if (present(append)) then
         if (append == .false.) then
            return
         end if
      end if
      call FetchSeries(DFILE, trim(seriesName), tempSeries)
      if (sample(2) /= 0) annualSeries(47:sample(2)) = tempSeries(47:sample(2))
      if (trim(seriesName) == "TMAXUI_SL.A") annualSeries(sample(2)+1:startYr-1) = 0d0
   
   end subroutine CollapseAndAppend

!===============================================================================

   subroutine CreateAdditionalSeries()
   
      integer :: i
      
      call FetchSeries(DFILE, "WSGGE.Q", wsgge)
      do i = sample(2)+1, endQtr
         wsgge(i) = wsggesl(i) + wsggefc(i) + wsgfm(i)
      end do
      call collapse(wsgge_a, wsgge)
      
      do i = 71, endQtr
         ay(i) = (yf(i) + ynf(i)) / (eas(0,0,i) + enas(0,0,i))
         y(i) = yf(i) + ynf(i)
      end do
      call collapse(ay_a, ay)
      call collapse(y_a, y)
      call CollapseMax(eggeslmax_a, eggesl)
      call CollapseMax(eggefcmax_a, eggefc)
   
   end subroutine CreateAdditionalSeries
   
!===============================================================================

      subroutine CollapseMax(annualSeries, quarterlySeries)
   
      real (kind = 8), dimension(MAX_YR) :: annualSeries
      real (kind = 8), dimension(MAX_QTR) :: quarterlySeries
      integer :: i, j
      
      do i = 1, MAX_YR
         j = i * 4
         annualSeries(i) = max(quarterlySeries(j), quarterlySeries(j+1), &
                               quarterlySeries(j+2),quarterlySeries(j+3))
      end do

      end subroutine CollapseMax      
      
!===============================================================================

   subroutine Finish()
    
      write(*,'(/a/)') "ModSol2 procedure finished"
      
   end subroutine Finish      
   
!===============================================================================   

end module EconModSol2Mod