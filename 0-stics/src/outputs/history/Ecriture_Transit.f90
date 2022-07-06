! write to the file history.sti
!!
!! general paramv6 parameters
subroutine Ecriture_Transit(t,nbplantes, codeplante1,codeplante2)

USE Stics
USE Messages

implicit none

    type(Stics_Transit_), intent(IN) :: t
    integer, intent(IN) :: nbplantes
    character(len=3), intent(IN) :: codeplante1,codeplante2

!TODO: revoir les param�tres
    call EnvoyerMsgHistorique('   ')
    call EnvoyerMsgHistorique(264)
    call EnvoyerMsgHistorique('*********************************************')

!  Specificities of cut crops
   if(codeplante1=='fou')then
      if(t%P_codetempfauche==1)then
        call EnvoyerMsgHistorique('P_codetempfauche in upvt',t%P_codetempfauche)
      endif
       if(t%P_codetempfauche==2)then
        call EnvoyerMsgHistorique('P_codetempfauche in udevair',t%P_codetempfauche)
      endif
      call EnvoyerMsgHistorique('P_coefracoupe(1)',t%P_coefracoupe(1))
   endif
      if(codeplante2=='fou')then
      if(t%P_codetempfauche==1)then
        call EnvoyerMsgHistorique('P_codetempfauche in upvt',t%P_codetempfauche)
      endif
       if(t%P_codetempfauche==2)then
        call EnvoyerMsgHistorique('P_codetempfauche in udevair',t%P_codetempfauche)
      endif
      call EnvoyerMsgHistorique('P_coefracoupe(2)',t%P_coefracoupe(2))
   endif

!  Specificities of Quinoa
   if(codeplante1=='qui')then
      if(t%P_codepluiepoquet==1)then
       call EnvoyerMsgHistorique('Option to replace rainfall by irrigation at hole depth in the case of hole sowing activated' )
       call EnvoyerMsgHistorique('P_codepluiepoquet',t%P_codepluiepoquet)
       call EnvoyerMsgHistorique('P_nbjoursrrversirrig',t%P_nbjoursrrversirrig)
      endif
   endif
   if(codeplante2=='qui')then
      if(t%P_codepluiepoquet==1)then
       call EnvoyerMsgHistorique('Option to replace rainfall by irrigation at hole depth in the case of hole sowing activated' )
       call EnvoyerMsgHistorique('P_codepluiepoquet',t%P_codepluiepoquet)
       call EnvoyerMsgHistorique('P_nbjoursrrversirrig',t%P_nbjoursrrversirrig)
      endif
   endif

!  Activation of the module simulating tiller dynamics
    call EnvoyerMsgHistorique('P_swfacmin',t%P_swfacmin)

    if(t%P_codetranspitalle==1)then
      call EnvoyerMsgHistorique('Activation of the module simulating tiller dynamics ')
      call EnvoyerMsgHistorique('P_codetranspitalle',t%P_codetranspitalle)
      if(t%P_codedyntalle(1)==1)then
        call EnvoyerMsgHistorique('P_codedyntalle(1)',t%P_codedyntalle(1))
        call EnvoyerMsgHistorique('P_SurfApex(1)',t%P_SurfApex(1))
        call EnvoyerMsgHistorique('P_SeuilMorTalle(1)',t%P_SeuilMorTalle(1))
        call EnvoyerMsgHistorique('P_SigmaDisTalle(1)',t%P_SigmaDisTalle(1))
        call EnvoyerMsgHistorique('P_VitReconsPeupl(1)',t%P_VitReconsPeupl(1))
        call EnvoyerMsgHistorique('P_SeuilReconsPeupl(1)',t%P_SeuilReconsPeupl(1))
        call EnvoyerMsgHistorique('P_MaxTalle(1)',t%P_MaxTalle(1))
      endif
      if(nbplantes.gt.1.and.t%P_codedyntalle(2)==1)then
        call EnvoyerMsgHistorique('P_codedyntalle(2)',t%P_codedyntalle(2))
        call EnvoyerMsgHistorique('P_SurfApex(2)',t%P_SurfApex(2))
        call EnvoyerMsgHistorique('P_SeuilMorTalle(2)',t%P_SeuilMorTalle(2))
        call EnvoyerMsgHistorique('P_SigmaDisTalle(2)',t%P_SigmaDisTalle(2))
        call EnvoyerMsgHistorique('P_VitReconsPeupl(2)',t%P_VitReconsPeupl(2))
        call EnvoyerMsgHistorique('P_SeuilReconsPeupl(2)',t%P_SeuilReconsPeupl(2))
        call EnvoyerMsgHistorique('P_MaxTalle(2)',t%P_MaxTalle(2))
      endif
    endif

!   Calculation of the stem elongation stage for perenial grasslands, code to stop the reserve limitation from the stem elongation
    if(t%P_codemontaison(1)==1)then
       call EnvoyerMsgHistorique('code to stop the reserve limitation from the stem elongation for perenial grasslands activated')
       call EnvoyerMsgHistorique('P_codemontaison',t%P_codemontaison(1))
    endif
    if(nbplantes.gt.1.and.t%P_codemontaison(2)==1)then
       call EnvoyerMsgHistorique('plant 2,code to stop the reserve limitation from the stem elongation &
       & for perenial grasslands activated')
       call EnvoyerMsgHistorique('P_codemontaison',t%P_codemontaison(1))
    endif

!   Calculation of the maximal reserve compartment during reproductive stages
    call EnvoyerMsgHistorique('P_resplmax',t%P_resplmax(1))
    if(nbplantes.gt.1)then
           call EnvoyerMsgHistorique('P_resplmax plant 2',t%P_resplmax(2))
    endif


!  Moisture test for sowing decision
    call EnvoyerMsgHistorique('P_nbj_pr_apres_semis',t%P_nbj_pr_apres_semis)
    call EnvoyerMsgHistorique('P_eau_mini_decisemis',t%P_eau_mini_decisemis)
    call EnvoyerMsgHistorique('P_humirac_decisemis',t%P_humirac_decisemis)
! fertilisation driving
    if(t%P_codecalferti==1)then
       call EnvoyerMsgHistorique('Automatic calculation of fertilisation requirements activated')
       call EnvoyerMsgHistorique('P_ratiolN',t%P_ratiolN)
       call EnvoyerMsgHistorique('P_dosimxN',t%P_dosimxN)
       if(t%P_codetesthumN==1)then
           call EnvoyerMsgHistorique(' option of soil moisture test : minimum rainfall threshold')
       endif
       if(t%P_codetesthumN==2)then
           call EnvoyerMsgHistorique(' option of soil moisture test : soil moisture threshold')
       endif
     endif

!  Residues decomposition
     if(t%P_codeNmindec==1)then
       call EnvoyerMsgHistorique('Limitation of N availability for residues decomposition in soil activated')
       call EnvoyerMsgHistorique('P_rapNmindec',t%P_rapNmindec)
       call EnvoyerMsgHistorique('P_fNmindecmin',t%P_fNmindecmin)
     endif

! coupling with pathogen models
     if(t%P_codetrosee==1)then
       call EnvoyerMsgHistorique('calculation of hourly dew temperature : linear interpolation(actual calculation)')
     endif
     if(t%P_codetrosee==2)then
       call EnvoyerMsgHistorique('calculation of hourly dew temperature : sinusoidal interpolation (Debele Bekele et al.,2007)')
     endif
     if(t%P_codeSWDRH==1)then
       call EnvoyerMsgHistorique('calculation of surface wetness duration activated')
     endif

!  automatic irrigations (associated with the options of automatic irrigation in tec file)
    if(t%P_codedate_irrigauto==1)then
       call EnvoyerMsgHistorique('automatic irrigations (associated with the options of automatic irrigation in tec file)')
       call EnvoyerMsgHistorique('start and end in dates')
       call EnvoyerMsgHistorique('P_datedeb_irrigauto',t%P_datedeb_irrigauto)
       call EnvoyerMsgHistorique('P_datefin_irrigauto',t%P_datefin_irrigauto)
    endif
    if(t%P_codedate_irrigauto==2)then
       call EnvoyerMsgHistorique('automatic irrigations (associated with the options of automatic irrigation in tec file)')
       call EnvoyerMsgHistorique('start and end at stages')
       call EnvoyerMsgHistorique('P_stage_start_irrigauto',t%P_stage_start_irrigauto)
       call EnvoyerMsgHistorique('P_stage_end_irrigauto',t%P_stage_end_irrigauto)
    endif

!  calculation of the root death at cutting date for grasslands
   if(codeplante1=='fou')then
      if(t%P_codemortalracine==1)then
         call EnvoyerMsgHistorique('dry matter is calculated with masec')
      endif
      if(t%P_codemortalracine==2)then
         call EnvoyerMsgHistorique('dry matter is calculated with masectot')
      endif
   endif

!   special outputs AgMIP/Macsur
   if(t%P_type_project==2)then
       call EnvoyerMsgHistorique('special outputs for AgMIP')
       if(t%P_rules_sowing_AgMIP==1)then
         call EnvoyerMsgHistorique('sowing rules wheat3 activated')
       endif
       if(t%P_Flag_Agmip_rap==1)then
         call EnvoyerMsgHistorique('outputs for AgMIP Wheat')
       endif
       if(t%P_Flag_Agmip_rap==2)then
         call EnvoyerMsgHistorique('outputs for AgMIP Wheat Giacomo (HSC)')
       endif
       if(t%P_Flag_Agmip_rap==3)then
         call EnvoyerMsgHistorique('outputs for AgMIP wheat Canopy temp')
       endif
       if(t%P_Flag_Agmip_rap==4)then
         call EnvoyerMsgHistorique('outputs for AgMIP face_maize')
       endif
       if(t%P_Flag_Agmip_rap==5)then
         call EnvoyerMsgHistorique('outputs for AgMIP new wheat3')
       endif
   endif
   if(t%P_type_project==3)then
       call EnvoyerMsgHistorique('special outputs for Macsur')
   endif

!  option for several thinning
   if(t%P_option_thinning==1)then
       call EnvoyerMsgHistorique('activation of several thinning available in the tec file')
   endif

!  option for several fertilizer type
   if(t%P_option_engrais_multiple==1)then
       call EnvoyerMsgHistorique('activation of several fertilizer type available in the tec file')
   endif

!  option for pasture
! DR 10/11/2016 attention il faudra corriger le nom dans le code
   if(t%P_option_pature==1)then
       call EnvoyerMsgHistorique('activation of pasture available in the tec file')
       call EnvoyerMsgHistorique('P_coderes_pature',t%P_coderes_pature)
       call EnvoyerMsgHistorique('P_pertes_restit_ext',t%P_pertes_restit_ext)
       call EnvoyerMsgHistorique('P_Crespc_pature',t%P_Crespc_pature)
       call EnvoyerMsgHistorique('P_Nminres_pature',t%P_Nminres_pature)
       call EnvoyerMsgHistorique('P_eaures_pature',t%P_eaures_pature)
       call EnvoyerMsgHistorique('P_coef_calcul_qres',t%P_coef_calcul_qres)
       call EnvoyerMsgHistorique('P_engrais_pature',t%P_engrais_pature)
       call EnvoyerMsgHistorique('P_coef_calcul_doseN',t%P_coef_calcul_doseN)
   endif


! DR 10/11/2016 on gele le formalisme en attente de verification par joel et Bruno
!    call EnvoyerMsgHistorique('P_code_adapt_MO_CC',t%P_code_adapt_MO_CC)
!    call EnvoyerMsgHistorique('P_periode_adapt_CC',t%P_periode_adapt_CC)
!    call EnvoyerMsgHistorique('P_an_debut_serie_histo',t%P_an_debut_serie_histo)
!    call EnvoyerMsgHistorique('P_an_fin_serie_histo',t%P_an_fin_serie_histo)
!    call EnvoyerMsgHistorique('P_param_tmoy_histo',t%P_param_tmoy_histo)
!    call EnvoyerMsgHistorique('P_code_adaptCC_miner',t%P_code_adaptCC_miner)
!    call EnvoyerMsgHistorique('P_code_adaptCC_nit',t%P_code_adaptCC_nit)
!    call EnvoyerMsgHistorique('P_code_adaptCC_denit',t%P_code_adaptCC_denit)
!    call EnvoyerMsgHistorique('P_TREFdenit1',t%P_TREFdenit1)
!    call EnvoyerMsgHistorique('P_TREFdenit2',t%P_TREFdenit2)

if(t%P_code_shape(1)==2)then
    call EnvoyerMsgHistorique('*********************************************')
    call EnvoyerMsgHistorique('Plant heigth is computed using masec (code_shape==2) for the principal plant')
    call EnvoyerMsgHistorique('Stage with constant height (stage_const_height):',t%P_stage_const_height(1))
    call EnvoyerMsgHistorique('Parameter for the hauteur~masec relationship (hautK and hautA):')
    call EnvoyerMsgHistorique('hautK:',t%P_hautK(1))
    call EnvoyerMsgHistorique('hautA:',t%P_hautA(1))
    call EnvoyerMsgHistorique('Density at which hautK and hautA were fitted (hautdens):',t%P_hautdens(1))
    call EnvoyerMsgHistorique('Elongation parameter (P_elongation)',t%P_elongation(1))
endif

if(t%P_code_shape(2)==2)then
    call EnvoyerMsgHistorique('*********************************************')
    call EnvoyerMsgHistorique('Plant heigth is computed using masec (code_shape==2) for the associated plant')
    call EnvoyerMsgHistorique('Stage with constant height (stage_const_height):',t%P_stage_const_height(2))
    call EnvoyerMsgHistorique('Parameter for the hauteur~masec relationship (hautK and hautA):')
    call EnvoyerMsgHistorique('hautK:',t%P_hautK(2))
    call EnvoyerMsgHistorique('hautA:',t%P_hautA(2))
    call EnvoyerMsgHistorique('Density at which hautK and hautA were fitted (hautdens):',t%P_hautdens(2))
    call EnvoyerMsgHistorique('Elongation parameter (P_elongation)',t%P_elongation(2))
endif

return
end
