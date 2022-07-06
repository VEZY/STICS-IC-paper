
! Calculation of nitrification and N2O production by nitrification
! **********************************************************************************
! Stics book paragraph 8, pages 148-151
! See Bessou et al. (2010), European Journal of Soil Science, 61(3),
! 348-363.

! ----------------------------------------------------------------------------------
subroutine nitrif_N2O(nbCouches, numcult, P_profhum, P_codefente, P_codenitrif, P_rationit, P_hoptn,    &
    P_hminn, P_fnx, P_pH, P_pHmaxnit, P_pHminnit, var_tnitmin, var_tnitopt, var_tnitmax, var_tnitopt2,  &
    P_vnitmax, P_Kamm, P_nh4_min, tsol, hucc, hur, dacouche, humin, sat,                                &
!    P_code_vnit, P_fnx_soil, P_code_tnit, P_tnitmin_pw, P_tnitopt_pw, P_tnitopt2_pw,                   &
!    P_tnitmax_pw, P_tnitopt_gauss, P_scale_tnitopt, P_code_rationit, P_rationit_constant,              &
     P_code_vnit, P_code_tnit, P_tnitopt_gauss, P_scale_tnitopt, P_code_rationit,           &
     P_code_hourly_wfps_nit, precip, P_kdesat,                                                          & !IN
    amm, nit, nitrifj, Qnitrif, em_N2Onit, Qem_N2Onit)                                                    !INOUT

    implicit none

    integer, intent(IN) :: nbCouches
    integer, intent(IN) :: numcult
    real,    intent(IN) :: P_profhum       ! // PARAMETER // soil depth below which biological activity is nil  (max.60 cm) // cm // PARSOL // 1
    integer, intent(IN) :: P_codefente     ! // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0
    integer, intent(IN) :: P_codenitrif    ! // PARAMETER // option to activate nitrification calculation // code 1 (activated) /2 (non activated)// PARSOL // 0
    ! P_rationit � terme dans fichier sol ???
    real,    intent(IN) :: P_rationit      ! // PARAMETER // molar ratio of nitrification : N2O/N nitrified // SD // PARAM // 1
    real,    intent(IN) :: P_hoptn         ! // PARAMETER // moisture (proportion of field capacity) at which nitrification rate is maximum // g.g-1 // PARAM // 1
    real,    intent(IN) :: P_hminn         ! // PARAMETER // moisture (proportion of field capacity) below which nitrification rate is nil // g.g-1 // PARAM // 1
    ! P_fnx remplac� par P_fnx_soil (� terme dans fichier sol)
    real,    intent(IN) :: P_fnx           ! // PARAMETER // ! potential proportion of NH4 nitrified each day if linear model // day-1 // PARAM // 1
    real,    intent(IN) :: P_pH            ! // PARAMETER // Soil pH (basic value without addition of organic amendments)  // SD // PARSOL // 1
    real,    intent(IN) :: P_pHmaxnit      ! // PARAMETER // pH above which nitrification is maximal // pH // PARAM // 1
    real,    intent(IN) :: P_pHminnit      ! // PARAMETER // pH below which nitrification is nil // pH // PARAM // 1
    ! variables suivantes = pour utilisation avec option changement climatique
    ! d�sactiv� ici ; si on veut conserver plus facile avec mod�le gaussien
    ! remplac�es par P_tnitmin_pw, P_tnitopt_pw, P_tnitopt2_pw, P_tnitmax_pw
!    real,    intent(IN) :: var_tnitmin(200)   ! dimension ?
!    real,    intent(IN) :: var_tnitopt(200)   ! dimension ?
!    real,    intent(IN) :: var_tnitmax(200)   ! dimension ?
!    real,    intent(IN) :: var_tnitopt2(200)  ! Bruno : ajout plateau sur la courbe de nitrification  dimension ?
    real,    intent(IN) :: var_tnitmin   ! P_tnitmin pour le moment
    real,    intent(IN) :: var_tnitopt  ! P_tnitopt pour le moment
    real,    intent(IN) :: var_tnitmax   ! P_tnitmax  pour le moment
    real,    intent(IN) :: var_tnitopt2  ! P_tnitopt2  pour le moment

    real,    intent(IN) :: P_vnitmax          ! � terme dans fichier sol// PARAMETER // maximum nitrification rate avec option michaelis-menten(mg N/kg/d)
    real,    intent(IN) :: P_Kamm             ! // PARAMETER // affinity constant for NH4 in nitrification avec option michaelis-menten (mg N/l)
    real,    intent(IN) :: P_nh4_min          ! // PARAMETER // minimal (fixed ?) NH4 concentration found in soil (mg N/kg)
    real,    intent(IN) :: tsol(nbCouches)
    real,    intent(IN) :: hucc(nbCouches)
    real,    intent(IN) :: hur(nbCouches)
    real,    intent(IN) :: dacouche(nbCouches)
    real,    intent(IN) :: humin(nbCouches)
    real,    intent(IN) :: sat(nbCouches)
    real,    intent(INOUT) :: amm(nbCouches) ! en principe en kg/ha/cm
    real,    intent(INOUT) :: nit(nbCouches)
    real,    intent(INOUT) :: nitrifj       ! // OUTPUT // Daily N nitrified // kg.ha-1.d-1
    real,    intent(INOUT) :: Qnitrif       ! // OUTPUT // cumulative N nitrified // kg.ha-1
    real,    intent(INOUT) :: em_N2Onit     ! // OUTPUT // daily N2O flux due to  nitrification// kg.ha-1.d-1
    real,    intent(INOUT) :: Qem_N2Onit    ! // OUTPUT // cumulative N2O flux due to  nitrification // kg.ha-1

    ! 19/10/2016 ajout options et nouveaux params
    integer, intent(IN)    :: P_code_vnit ! choice of nitrification rate dependence on NH4 (linear or Michaelis-Menten)
!    real,    intent(IN)    :: P_fnx_soil ! potential proportion of NH4 nitrified each day if linear model
    integer, intent(IN)    ::  P_code_tnit ! choice of temperature function for nitrification (piecewise linear or gaussian)
!    real,    intent(IN)    ::  P_tnitmin_pw
!    real,    intent(IN)    ::  P_tnitopt_pw ! d�but optimum
!    real,    intent(IN)    ::  P_tnitopt2_pw ! fin optimum
!    real,    intent(IN)    ::  P_tnitmax_pw
    real,    intent(IN)    ::  P_tnitopt_gauss ! optimum temperature for nitrification
    real,    intent(IN)    ::  P_scale_tnitopt ! parameter related to the range of optimum temperature for nitrification
    integer, intent(IN)    ::  P_code_rationit ! choice of constant or variable N2O ratio for nitrification
!    real,    intent(IN)    ::  P_rationit_constant ! constant value of N2O ratio for nitrification
    integer, intent(IN) ::  P_code_hourly_wfps_nit ! choice of activating or not hourly WFPS calculation for nit

    real, intent(IN) :: P_kdesat         ! constante de vitesse de desaturation en eau (j-1)              3.0
    real, intent(IN) :: precip          ! pluie du jour (mm)
    
    ! Variables locales
    integer :: iz
    real :: fpHn
    real :: fhn
    real :: ftn
    real :: wsat
    real :: theta
    real :: w
    real :: wfps
    real :: wfpscc ! wfps � la capacit� au champ
    real :: wfpsminn ! wfps � la teneur en eau min de la nitrification
    real :: wfpsoptn ! wfps � la teneur en eau optimale de la nitrification
    real :: vnitpot
    real :: ammdisp
    real :: Famm
    real :: vnit                 ! nitrification rate in layer iz (kg N/ha/d)
    real, parameter :: aN = 0.40 ! pente gO2
    real, parameter :: bN = 1.04 ! cte gO2
    real :: gO2
    real :: emN2Ocouche

    integer :: i                  ! numero de heure du jour (1 a 24)
    integer :: is_sat(40)         ! indice de saturation de la couche iz (0/1) ; 40 en lien avec la prof max d'humification
    integer :: numcouche          ! compteur de couche
    real :: volpluierestant = 0.  ! voume d'eau restant a distribuer dans les couches inf�rieures
    real :: time(24)              ! fraction de jour decimal (0 a 1)
    real :: hur_horaire(24)       ! humidit� de heure i
    real :: wfps_horaire(24)      ! Taux de saturation en eau de l'heure i
    real :: fhn_horaire(24) = 0.  ! fonction eau nitrification
    real :: gO2_horaire(24) = 0.  ! fraction N2O from nitrification
    real :: FWaterN2O_horaire(24) = 0. ! combinaison effet eau nit et ratio
    real :: FWaterN2O = 0.
    
    ! Param�tres locaux
    real, parameter :: rho_s = 2.66

    ! initialisations
    nitrifj = 0.
    em_N2Onit = 0. ! initialisation flux N2O journalier
    is_sat = 0

    ! Vitesse de nitrification potentielle = vnitpot*Famm
    ! Vitesse de nitrification r�elle = vnitpot * f(pH, temp�rature, eau)
    ! tnitrif = taux de nitrification journalier du NH4 (Jorge Sierra)
    
    ! pour option calcul WFPS horaire
    if (P_code_hourly_wfps_nit == 1) then
        do i = 1, 24
            time(i) = (i - 0.5)/24.
        end do
        
        ! Recherche des couches qui sont satur�es les jours de pluie
        numcouche = 1
        if (precip > 0.) then ! la pluie peut saturer les couches de sol en partant de la surface
            volpluierestant = precip
            do while (volpluierestant > 0.)
                ! porosit� totale ou teneur en eau � saturation
                if (P_codefente == 1) then
                    wsat = (1.5 * hucc(numcouche) - 0.5 * humin(numcouche)) / 10. ! Jo�l : sols gonflants, pas clair pour moi !??
                else
                    wsat = 1. - dacouche(numcouche) / rho_s
                endif
                volpluierestant = volpluierestant - (wsat*10. - hur(numcouche))
                is_sat(numcouche) = 1
                numcouche = numcouche + 1
                if(numcouche == nint(P_profhum)) exit ! inutile au del� de la profondeur d'humification
            end do
        end if
    end if

    ! Boucle sur les couches contribuant a la nitrification
    ! ------------------------------------------------------
    do iz = 1,nint(P_profhum) ! sur profondeur de min�ralisation de l'humus

        if (P_codenitrif == 2) then ! 2 pour pas de calcul nitrification (nitrif imm�diate)
            vnit = amm(iz) ! kg/ha/cm/day
        else ! calcul nitrification
            ! 1) effet pH : eq 8.13
            if (P_pH <= P_pHminnit)  fpHn = 0.
            if (P_pH >= P_pHmaxnit)  fpHn = 1.
            if (P_pH < P_pHmaxnit .and. P_pH > P_pHminnit) then
                fpHn = (P_pH-P_pHminnit) / (P_pHmaxnit-P_pHminnit)
            endif

            ! 2) effet temp�rature
            if (P_code_tnit == 1) then
                ! fonction lin�aire croissante puis decroissante : eq 8.15 MODIFIEE pour avoir un plateau de Tnitopt a Tnitopt2 (Bruno)
                ! voir si on garde le param�trage permettant changement climatique
                ! pour l'instant chang� pour param�tres dans param_newform (� noter que peuvent d�pendre du site...)
                !if (tsol(iz) <= var_tnitopt(numcult)) then
                !   ftn = (tsol(iz)-var_tnitmin(numcult)) / (var_tnitopt(numcult)-var_tnitmin(numcult))
                !else
                !     if (tsol(iz) <= var_tnitopt2(numcult)) then
                !         ftn = 1.
                !     else
                !         ftn = (tsol(iz)-var_tnitmax(numcult)) / (var_tnitopt2(numcult)-var_tnitmax(numcult))
                !     endif
                !endif
                ! ftn = max(ftn,0.)
                ! ftn = min(ftn,1.)
                if (tsol(iz) <= var_tnitopt) then
                    ftn = (tsol(iz)-var_tnitmin) / (var_tnitopt-var_tnitmin) ! ok, ensuite mis � 0 si n�gatif
                else ! > P_tnitopt_pw
                     if (tsol(iz) <= var_tnitopt2) then
                         ftn = 1.
                     else
                         ftn = (tsol(iz)-var_tnitmax) / (var_tnitopt2-var_tnitmax)
                     endif
                endif
                 ftn = max(ftn,0.)
                 ftn = min(ftn,1.)
                 
            else
            ! Jo�l 1/9/15 fonction gauss (Article Marie Benoit) 
                ftn = exp((-(tsol(iz)-P_tnitopt_gauss)**2) / (P_scale_tnitopt**2))
            endif

            ! 3) effet humidit� (fonction lineaire entre Hminn et Hoptn) : eq 8.14
            ! serait plus simple de tout coder en wfps
            ! variables utiles par couche
            ! humidit� volumique (theta) et massique (w)
            theta = (hur(iz) + sat(iz)) / 10.        ! cm3 eau/cm3 sol      /!\ reste pas clair : l. 167 hur > hcc, et l� on ajoute sat ??
            w = theta / dacouche(iz)                 ! g eau/ g sol
            ! porosit� totale ou humidit� volumique � saturation
            if (P_codefente == 1) then
                wsat = (1.5 * hucc(iz) - 0.5 * humin(iz)) / 10. ! Jo�l : pas clair pour moi !??
            else
                wsat = 1. - dacouche(iz) / rho_s ! porosit�
            endif
            wfpscc = (hucc(iz) / 10.) / wsat ! wfps � la capacit� au champ
            wfpsminn = (P_hminn*hucc(iz) / 10.) / wsat ! wfps � teneur en eau minimale nitrification
            wfpsoptn = (P_hoptn*hucc(iz) / 10.) / wsat ! wfps � teneur en eau optimale nitrification

            if (P_code_hourly_wfps_nit == 1 .and. is_sat(iz) == 1) then
                ! Calcul horaire de la redistribution d'eau dans les couches qui sont initialement satur�es
                ! apr�s une pluie
                ! 95% drainage � t=1 pour kdesat = 3.00 -> valeur par d�faut
                ! avec kdesat = 1.00 on ne draine que (63)% ...
                hur_horaire  = hucc(iz) + exp(-P_kdesat * time) * (wsat*10. - hucc(iz))
                wfps_horaire = min(hur_horaire / (wsat*10.), 1.)
                fhn_horaire = (wfps_horaire-wfpsminn) / (wfpsoptn-wfpsminn)
                where (wfps_horaire > wfpscc)   
                    fhn_horaire = (wfps_horaire-1.)/(wfpscc-1.)
                end where
                fhn_horaire = min(1., max(fhn_horaire,0.))
                
                ! pour le ratio (ici pour �viter de faire les calculs apr�s
                gO2_horaire = (aN * wfps_horaire - bN) / (wfps_horaire - bN)    ! ratio variable (attention � wfps>1.03 devient n�gatif !! -> ajout contrainte sur wfps)
                FWaterN2O_horaire = fhn_horaire * gO2_horaire
                ! valeur moyenne
                fhn = sum(fhn_horaire)/24.
                FWaterN2O = sum(FWaterN2O_horaire)/24.
            else
                wfps = min(theta / wsat, 1.) ! p.156
                fhn = (wfps-wfpsminn) / (wfpsoptn-wfpsminn)
                if (wfps > wfpscc) then    ! diminution progressive de la nitrification en cas d'exc�s d'eau
                    fhn = (wfps-1.)/(wfpscc-1.) ! elsa correction fhn au lieu de fh
                endif
                fhn = min(1., max(fhn,0.))
                
                ! pour ratio
                gO2 = (aN * wfps - bN) / (wfps - bN)    ! ratio variable (attention � wfps>1.03 devient n�gatif !! -> ajout contrainte sur wfps)
                FWaterN2O = fhn * gO2
            end if
            ! supprim� apr�s expression de tout en fonction wfps Jo�l 25/10/2016
            !fhn = (hur(iz)-P_hminn*hucc(iz))/((P_hoptn-P_hminn)*hucc(iz)) ! OK (partie croissante, coup�e � 0 et 1 � la fin)
            !wfps = min(theta / wsat, 1.) ! p.156
            !if (hur(iz) > hucc(iz)) then    ! diminution progressive de la nitrification en cas d'exc�s d'eau
            !    fhn = (wfps-1.)/(wfpscc-1.) ! elsa correction fhn au lieu de fh
            !endif
            !fhn = min(1., max(fhn,0.))

            ! 4) vitesse de nitrification potentielle
            ! dans la 1ere option qui suit, on a une forte concentration dans la premi�re couche (1cm), tr�s au dessus de vnitmax
            ! du coup la nitrification est fortement sous estim�e
            ! dans NOE on pouvait ajuster en r�partissant l'ammonium sur plusieurs cm
            ! pr�f�rence provisoire pour 2e option, qui semble donner de bons r�sultats sur plusieurs sites diff�rents
            ! mais implique des flux tr�s forts et non r�alistes
            if (P_code_vnit == 2) then
                ammdisp = max(0., amm(iz)*10/dacouche(iz)-P_nh4_min) ! joel *10/dacouche(iz) --> conversion kg/ha/cm en mg/kg
                Famm = ammdisp /(P_Kamm * w + ammdisp)
                vnitpot = P_vnitmax * dacouche(iz) /10 * Famm ! joel il faut diviser par 10 pour avoir des kg/ha/cm
            else ! P_code_vnit == 1
                vnitpot = P_fnx * max(amm(iz)-(P_nh4_min*dacouche(iz)/10),0.) ! joel *dacouche(iz)/10 --> on veut des kg/ha/cm !
            endif
            
            ! Calcul de la vitesse de nitrification Eq 8.12
            vnit = vnitpot * fpHn * ftn * fhn
            vnit = max(0.,vnit)
            vnit = min(vnit, max(amm(iz)-(P_nh4_min*dacouche(iz)/10),0.)) ! joel ne pas oublier de prendre le max(0) !

        endif

        ! Production et r�duction du N2O
        if (P_code_rationit == 1) then ! ratio constant
            emN2Ocouche = vnit * P_rationit
        else ! P_code_rationit == 2
            ! emN2Ocouche = vnit * 0.0016 * gO2 ! 0.0016 cod� en dur pour la fonction variable, on pourrait laisser P_rationit du param_gen sinon mais risque confusion
            if (fhn>0.) then
                emN2Ocouche = vnit * 0.0016 * FWaterN2O/fhn
            else
                emN2Ocouche = 0.
            end if
        end if

        ! Actualisation des pools d'azote min�ral
        amm(iz) = amm(iz) - vnit

        nit(iz) = nit(iz) + vnit - emN2Ocouche

        ! Sommation des flux  de nitrification sur tout le profil
        nitrifj = nitrifj + vnit - emN2Ocouche ! nitrification au sens NO3 produit
        em_N2Onit = em_N2Onit + emN2Ocouche
    end do
    ! Fin de boucle sur les couches

    !  Nitrification cumul�e
    Qnitrif = Qnitrif + nitrifj
    Qem_N2Onit = Qem_N2Onit + em_N2Onit

    return
end subroutine nitrif_N2O
