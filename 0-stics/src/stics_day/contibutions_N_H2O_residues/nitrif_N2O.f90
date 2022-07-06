
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
    ! P_rationit à terme dans fichier sol ???
    real,    intent(IN) :: P_rationit      ! // PARAMETER // molar ratio of nitrification : N2O/N nitrified // SD // PARAM // 1
    real,    intent(IN) :: P_hoptn         ! // PARAMETER // moisture (proportion of field capacity) at which nitrification rate is maximum // g.g-1 // PARAM // 1
    real,    intent(IN) :: P_hminn         ! // PARAMETER // moisture (proportion of field capacity) below which nitrification rate is nil // g.g-1 // PARAM // 1
    ! P_fnx remplacé par P_fnx_soil (à terme dans fichier sol)
    real,    intent(IN) :: P_fnx           ! // PARAMETER // ! potential proportion of NH4 nitrified each day if linear model // day-1 // PARAM // 1
    real,    intent(IN) :: P_pH            ! // PARAMETER // Soil pH (basic value without addition of organic amendments)  // SD // PARSOL // 1
    real,    intent(IN) :: P_pHmaxnit      ! // PARAMETER // pH above which nitrification is maximal // pH // PARAM // 1
    real,    intent(IN) :: P_pHminnit      ! // PARAMETER // pH below which nitrification is nil // pH // PARAM // 1
    ! variables suivantes = pour utilisation avec option changement climatique
    ! désactivé ici ; si on veut conserver plus facile avec modèle gaussien
    ! remplacées par P_tnitmin_pw, P_tnitopt_pw, P_tnitopt2_pw, P_tnitmax_pw
!    real,    intent(IN) :: var_tnitmin(200)   ! dimension ?
!    real,    intent(IN) :: var_tnitopt(200)   ! dimension ?
!    real,    intent(IN) :: var_tnitmax(200)   ! dimension ?
!    real,    intent(IN) :: var_tnitopt2(200)  ! Bruno : ajout plateau sur la courbe de nitrification  dimension ?
    real,    intent(IN) :: var_tnitmin   ! P_tnitmin pour le moment
    real,    intent(IN) :: var_tnitopt  ! P_tnitopt pour le moment
    real,    intent(IN) :: var_tnitmax   ! P_tnitmax  pour le moment
    real,    intent(IN) :: var_tnitopt2  ! P_tnitopt2  pour le moment

    real,    intent(IN) :: P_vnitmax          ! à terme dans fichier sol// PARAMETER // maximum nitrification rate avec option michaelis-menten(mg N/kg/d)
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
!    real,    intent(IN)    ::  P_tnitopt_pw ! début optimum
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
    real :: wfpscc ! wfps à la capacité au champ
    real :: wfpsminn ! wfps à la teneur en eau min de la nitrification
    real :: wfpsoptn ! wfps à la teneur en eau optimale de la nitrification
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
    real :: volpluierestant = 0.  ! voume d'eau restant a distribuer dans les couches inférieures
    real :: time(24)              ! fraction de jour decimal (0 a 1)
    real :: hur_horaire(24)       ! humidité de heure i
    real :: wfps_horaire(24)      ! Taux de saturation en eau de l'heure i
    real :: fhn_horaire(24) = 0.  ! fonction eau nitrification
    real :: gO2_horaire(24) = 0.  ! fraction N2O from nitrification
    real :: FWaterN2O_horaire(24) = 0. ! combinaison effet eau nit et ratio
    real :: FWaterN2O = 0.
    
    ! Paramètres locaux
    real, parameter :: rho_s = 2.66

    ! initialisations
    nitrifj = 0.
    em_N2Onit = 0. ! initialisation flux N2O journalier
    is_sat = 0

    ! Vitesse de nitrification potentielle = vnitpot*Famm
    ! Vitesse de nitrification réelle = vnitpot * f(pH, température, eau)
    ! tnitrif = taux de nitrification journalier du NH4 (Jorge Sierra)
    
    ! pour option calcul WFPS horaire
    if (P_code_hourly_wfps_nit == 1) then
        do i = 1, 24
            time(i) = (i - 0.5)/24.
        end do
        
        ! Recherche des couches qui sont saturées les jours de pluie
        numcouche = 1
        if (precip > 0.) then ! la pluie peut saturer les couches de sol en partant de la surface
            volpluierestant = precip
            do while (volpluierestant > 0.)
                ! porosité totale ou teneur en eau à saturation
                if (P_codefente == 1) then
                    wsat = (1.5 * hucc(numcouche) - 0.5 * humin(numcouche)) / 10. ! Joël : sols gonflants, pas clair pour moi !??
                else
                    wsat = 1. - dacouche(numcouche) / rho_s
                endif
                volpluierestant = volpluierestant - (wsat*10. - hur(numcouche))
                is_sat(numcouche) = 1
                numcouche = numcouche + 1
                if(numcouche == nint(P_profhum)) exit ! inutile au delà de la profondeur d'humification
            end do
        end if
    end if

    ! Boucle sur les couches contribuant a la nitrification
    ! ------------------------------------------------------
    do iz = 1,nint(P_profhum) ! sur profondeur de minéralisation de l'humus

        if (P_codenitrif == 2) then ! 2 pour pas de calcul nitrification (nitrif immédiate)
            vnit = amm(iz) ! kg/ha/cm/day
        else ! calcul nitrification
            ! 1) effet pH : eq 8.13
            if (P_pH <= P_pHminnit)  fpHn = 0.
            if (P_pH >= P_pHmaxnit)  fpHn = 1.
            if (P_pH < P_pHmaxnit .and. P_pH > P_pHminnit) then
                fpHn = (P_pH-P_pHminnit) / (P_pHmaxnit-P_pHminnit)
            endif

            ! 2) effet température
            if (P_code_tnit == 1) then
                ! fonction linéaire croissante puis decroissante : eq 8.15 MODIFIEE pour avoir un plateau de Tnitopt a Tnitopt2 (Bruno)
                ! voir si on garde le paramétrage permettant changement climatique
                ! pour l'instant changé pour paramètres dans param_newform (à noter que peuvent dépendre du site...)
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
                    ftn = (tsol(iz)-var_tnitmin) / (var_tnitopt-var_tnitmin) ! ok, ensuite mis à 0 si négatif
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
            ! Joël 1/9/15 fonction gauss (Article Marie Benoit) 
                ftn = exp((-(tsol(iz)-P_tnitopt_gauss)**2) / (P_scale_tnitopt**2))
            endif

            ! 3) effet humidité (fonction lineaire entre Hminn et Hoptn) : eq 8.14
            ! serait plus simple de tout coder en wfps
            ! variables utiles par couche
            ! humidité volumique (theta) et massique (w)
            theta = (hur(iz) + sat(iz)) / 10.        ! cm3 eau/cm3 sol      /!\ reste pas clair : l. 167 hur > hcc, et là on ajoute sat ??
            w = theta / dacouche(iz)                 ! g eau/ g sol
            ! porosité totale ou humidité volumique à saturation
            if (P_codefente == 1) then
                wsat = (1.5 * hucc(iz) - 0.5 * humin(iz)) / 10. ! Joël : pas clair pour moi !??
            else
                wsat = 1. - dacouche(iz) / rho_s ! porosité
            endif
            wfpscc = (hucc(iz) / 10.) / wsat ! wfps à la capacité au champ
            wfpsminn = (P_hminn*hucc(iz) / 10.) / wsat ! wfps à teneur en eau minimale nitrification
            wfpsoptn = (P_hoptn*hucc(iz) / 10.) / wsat ! wfps à teneur en eau optimale nitrification

            if (P_code_hourly_wfps_nit == 1 .and. is_sat(iz) == 1) then
                ! Calcul horaire de la redistribution d'eau dans les couches qui sont initialement saturées
                ! après une pluie
                ! 95% drainage à t=1 pour kdesat = 3.00 -> valeur par défaut
                ! avec kdesat = 1.00 on ne draine que (63)% ...
                hur_horaire  = hucc(iz) + exp(-P_kdesat * time) * (wsat*10. - hucc(iz))
                wfps_horaire = min(hur_horaire / (wsat*10.), 1.)
                fhn_horaire = (wfps_horaire-wfpsminn) / (wfpsoptn-wfpsminn)
                where (wfps_horaire > wfpscc)   
                    fhn_horaire = (wfps_horaire-1.)/(wfpscc-1.)
                end where
                fhn_horaire = min(1., max(fhn_horaire,0.))
                
                ! pour le ratio (ici pour éviter de faire les calculs après
                gO2_horaire = (aN * wfps_horaire - bN) / (wfps_horaire - bN)    ! ratio variable (attention à wfps>1.03 devient négatif !! -> ajout contrainte sur wfps)
                FWaterN2O_horaire = fhn_horaire * gO2_horaire
                ! valeur moyenne
                fhn = sum(fhn_horaire)/24.
                FWaterN2O = sum(FWaterN2O_horaire)/24.
            else
                wfps = min(theta / wsat, 1.) ! p.156
                fhn = (wfps-wfpsminn) / (wfpsoptn-wfpsminn)
                if (wfps > wfpscc) then    ! diminution progressive de la nitrification en cas d'excès d'eau
                    fhn = (wfps-1.)/(wfpscc-1.) ! elsa correction fhn au lieu de fh
                endif
                fhn = min(1., max(fhn,0.))
                
                ! pour ratio
                gO2 = (aN * wfps - bN) / (wfps - bN)    ! ratio variable (attention à wfps>1.03 devient négatif !! -> ajout contrainte sur wfps)
                FWaterN2O = fhn * gO2
            end if
            ! supprimé après expression de tout en fonction wfps Joël 25/10/2016
            !fhn = (hur(iz)-P_hminn*hucc(iz))/((P_hoptn-P_hminn)*hucc(iz)) ! OK (partie croissante, coupée à 0 et 1 à la fin)
            !wfps = min(theta / wsat, 1.) ! p.156
            !if (hur(iz) > hucc(iz)) then    ! diminution progressive de la nitrification en cas d'excès d'eau
            !    fhn = (wfps-1.)/(wfpscc-1.) ! elsa correction fhn au lieu de fh
            !endif
            !fhn = min(1., max(fhn,0.))

            ! 4) vitesse de nitrification potentielle
            ! dans la 1ere option qui suit, on a une forte concentration dans la première couche (1cm), très au dessus de vnitmax
            ! du coup la nitrification est fortement sous estimée
            ! dans NOE on pouvait ajuster en répartissant l'ammonium sur plusieurs cm
            ! préférence provisoire pour 2e option, qui semble donner de bons résultats sur plusieurs sites différents
            ! mais implique des flux très forts et non réalistes
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

        ! Production et réduction du N2O
        if (P_code_rationit == 1) then ! ratio constant
            emN2Ocouche = vnit * P_rationit
        else ! P_code_rationit == 2
            ! emN2Ocouche = vnit * 0.0016 * gO2 ! 0.0016 codé en dur pour la fonction variable, on pourrait laisser P_rationit du param_gen sinon mais risque confusion
            if (fhn>0.) then
                emN2Ocouche = vnit * 0.0016 * FWaterN2O/fhn
            else
                emN2Ocouche = 0.
            end if
        end if

        ! Actualisation des pools d'azote minéral
        amm(iz) = amm(iz) - vnit

        nit(iz) = nit(iz) + vnit - emN2Ocouche

        ! Sommation des flux  de nitrification sur tout le profil
        nitrifj = nitrifj + vnit - emN2Ocouche ! nitrification au sens NO3 produit
        em_N2Onit = em_N2Onit + emN2Ocouche
    end do
    ! Fin de boucle sur les couches

    !  Nitrification cumulée
    Qnitrif = Qnitrif + nitrifj
    Qem_N2Onit = Qem_N2Onit + em_N2Onit

    return
end subroutine nitrif_N2O
