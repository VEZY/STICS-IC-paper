!subroutine Stics_Initialisation_Boucle_Annees(sc,p,pg,itk,c,sta,soil,t)   !DR 19/07/2012 c ,sta et soil n'est pas utilisé
subroutine Stics_Initialisation_Boucle_Annees(sc,p,pg,itk,t)

USE Stics
!USE P_USM
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers, only: tmoy_histo

implicit none

    !type(USM_),                  intent(INOUT) :: P_usm
    type(Stics_Communs_),        intent(INOUT) :: sc  

    type(Plante_),               intent(INOUT) :: p(sc%P_nbplantes)  

    type(Parametres_Generaux_),  intent(INOUT) :: pg    ! TODO pg devrait être en IN, tous les paramètres modifiés doivent être dupliqués dans sc  

    type(ITK_),                  intent(INOUT) :: itk(sc%P_nbplantes)  

!    type(Climat_),               intent(INOUT) :: c

!    type(Station_),              intent(INOUT) :: sta

!    type(Sol_),                  intent(INOUT) :: soil

    type(Stics_Transit_),        intent(INOUT) :: t  


!integer, intent(IN) :: P_culturean ! Code culture annuelle ou à cheval sur 2 années
!character(len=12), intent(IN) :: P_codesimul ! Code type simulation
!integer, intent(INOUT) :: nbans ! Nombre d'années de simulation

! Variables locales
    integer :: i  


      ! nbans est calculé par rapport au nombre d'années climatiques fournies en entrées.
      ! Pour une culture à cheval sur deux années, il faut enlever une année de simulation.
        if(sc%P_culturean /= 1) sc%nbans = sc%nbans-1

      ! Pour les versions -stress sans simulation de plante, on n'effectue
      ! la simulation que sur une seule année.
        if (lge(sc%P_codesimul,'feuille') .eqv. .TRUE.) sc%nbans = 1


      ! initialisation de variables qu'on n'effectue qu'une fois en début de simulation.
        do i = 1, sc%P_nbplantes
        ! DR et FR 17/02/2015 on conserve le codeperenne de la plante qu'on change quand on fait un semis la premiere annee
          p(i)%codeperenne0 = p(i)%P_codeperenne
          call initsimul(sc,pg,p(i),itk(i),t)
        end do

! ************************* adaptation des Mo au CC ****************************
! DR 26/11/07 on calcule la temperature moyenne annuelle sur la serie clim dispo
! on a toutes les moyennes annuelles (1ier jour serie premiere annee à j-1 annee d'apres)
! dans tmoy_an(1,i)=an, tmoy(2,i)=tmoy annuelle i etant egal à numcult
!    if(P_code_adapt_MO_CC.eq.1.and.nbans.gt.P_periode_adapt_CC)then
!      call calc_tmoy_annuel
!    endif

      ! DR 19/10/09  on enleve le test sur codeoutsti
        if (t%P_code_adaptCC_miner == 1 .or. t%P_code_adaptCC_nit == 1 .or. t%P_code_adaptCC_denit == 1) then

          if (t%P_code_adapt_MO_CC == 1) then
            if (sc%nbans > t%P_periode_adapt_CC) then
              call calc_tmoy_annuel(sc%tmoy_an)
            else
              call EnvoyerMsgHistorique(452)
            endif
          endif
! DR et NB et JS 22/11/07
! calcul tmoy histo et deltaT_CC
          if (t%P_code_adapt_MO_CC == 1 .and. sc%nbans > (t%P_an_fin_serie_histo - t%P_an_debut_serie_histo)) then
            sc%Tm_histo = tmoy_histo(t%P_an_debut_serie_histo,t%P_an_fin_serie_histo,sc%tmoy_an)
          else
            sc%Tm_histo = t%P_param_tmoy_histo
          endif

          call calcDeltaTCC(sc,t)

        endif


! ********************** fin adaptation des Mo au CC ****************************

return
end subroutine Stics_Initialisation_Boucle_Annees
 
 
