
!---------------------------------------------------------------------------
! Denitrification and N2O production and consumption
!
! See Bessou et al. (2010), European Journal of Soil Science, 61(3), 348-363.
!---------------------------------------------------------------------------

subroutine denit_N2O(nbCouches, P_codedenit, P_profdenit, P_vpotdenit, P_ratiodenit, P_codefente,    &
    pH, Norg, CsurNsol, P_pHminden, P_pHmaxden,                                                      &
!    var_TREFdenit1, var_TREFdenit2, P_Kd, P_kdesat, P_wfpsc, dacouche, hucc, humin, sat, hur,        & ! IN
     P_Kd, P_kdesat, P_wfpsc, dacouche, hucc, humin, sat, hur,        & ! IN
    tsol, precip, P_tdenitopt_gauss, P_scale_tdenitopt, P_code_pdenit, P_cmin_pdenit, P_cmax_pdenit, &
    P_min_pdenit, P_max_pdenit, P_code_ratiodenit, P_code_hourly_wfps_denit,  &
    nit, condenit, Ndenit, QNdenit, em_N2Oden, Qem_N2Oden)                                             ! INOUT

    implicit none

    integer, intent(IN) :: nbCouches   ! nombre de couches de 1 cm (1000 en principe)

    real, intent(IN) :: P_codedenit    ! 1 = d�nit activ�e, 2 inactiv�e (inutile maintenant � l'int�rieur routine car autres codes pour options)
    real, intent(IN) :: P_profdenit    ! profondeur jusqu'� laquelle la d�nitrification est active (max 40 cm)
    real, intent(IN) :: P_vpotdenit    ! vitesse potentielle de d�nitrification *sur P_profdenit* (kg N/ha/day) (1 par d�faut � v�rifier, ou fonction de Corg)
    real, intent(IN) :: P_ratiodenit   ! [obsol�te dans sa version paramgen] fraction molaire N2O/(N2O+N2) de r�f�rence
    real, intent(IN) :: P_codefente    ! // PARAMETER // option allowing an additional water compartment for the swelling soils: yes (1), no (0) // code 0/1 // PARSOL // 0
    real, intent(IN) :: pH             ! pH du sol
    real, intent(IN) :: Norg           ! Norg du sol
    real, intent(IN) :: CsurNsol       ! C/N du sol
    
    real, intent(IN) :: P_pHminden     ! pH en dessous duquel la fraction molaire de N2O �mis est maximale
    real, intent(IN) :: P_pHmaxden     ! pH au del� duquel la fraction molaire de N2O �mis est minimale
!    real, intent(IN) :: var_TREFdenit1  ! obsol�te (formalisme Q10 abandonn� car absence d'optimum)
!    real, intent(IN) :: var_TREFdenit2  ! obsol�te
    real, intent(IN) :: P_Kd           ! constante de Michaelis-Menten pour la denitrification (mg N/l) 215
    real, intent(IN) :: P_kdesat         ! constante de vitesse de desaturation en eau (j-1)              3.0
    real, intent(IN) :: P_wfpsc          ! seuil de wfps � partir duquel la d�nitrification est active    0.62
    
    real, intent(IN) :: P_tdenitopt_gauss ! optimum fonction gaussienne temp�rature
    real, intent(IN) :: P_scale_tdenitopt ! sd fonction gaussienne temp�rature (plage optimum �troite ou �tendue)
    integer, intent(IN) :: P_code_pdenit ! choix 1 = lecture fichier sol ; 2 = calcul � partir de Corg
    real, intent(IN) :: P_cmin_pdenit ! Corg en dessous duquel potentiel d�nit constant et min
    real, intent(IN) :: P_cmax_pdenit ! Corg en dessus duquel potentiel d�nit constant et max
    real, intent(IN) :: P_min_pdenit ! valeur min potentiel d�nit
    real, intent(IN) :: P_max_pdenit ! valeur max potentiel d�nit
    integer, intent(IN) :: P_code_ratiodenit ! choix 1 = ratio constant et lu ; 2 = ratio variable
!    real, intent(IN) :: P_ratiodenit_constant ! valeur du ratio d�nit si constant
    integer, intent(IN) :: P_code_hourly_wfps_denit ! code 1 = calcul WFPS horaire si pluie ; 2 = pas de calcul horaire
    
    real, intent(IN) :: dacouche(nbCouches) ! masse volumique (g/cm3)
    real, intent(IN) :: hucc(nbCouches)     ! 1 � P_profdenit
    real, intent(IN) :: humin(nbCouches)    ! 1 � P_profdenit
    real, intent(IN) :: sat(nbCouches)      ! 1 � P_profdenit
    real, intent(IN) :: hur(nbCouches)  ! mm d'eau par couche de 1 cm (mm) (hur = w * rho * 10 mm)
    real, intent(IN) :: tsol(nbCouches) ! temp�rature (�C)
    real, intent(IN) :: precip          ! pluie du jour (mm)

    real, intent(INOUT) :: nit(nbCouches) ! nitrate par couche (kg N / ha pour 1 cm) (modifi� apr�s initialisation)
    real, intent(INOUT) :: condenit       ! Index of environmental factors acting on denitrification // 0-1 or more (Jo�l : ?)
    real, intent(INOUT) :: Ndenit         ! d�nitrification journali�re (cumul sur les couches)
    real, intent(INOUT) :: QNdenit        ! d�nitrification totale (cumul sur le temps)
    real, intent(INOUT) :: em_N2Oden      ! �mission de N2O journali�re (cumul sur les couches)
    real, intent(INOUT) :: Qem_N2Oden     ! �mission totale de N2O (cumul sur le temps)

    ! Param�tres locaux
    real, parameter :: rho_s = 2.66
    ! d�nitrification
    real, parameter :: expwfps = 1.74 ! exposant de la fonction Fwfps
    
    real, parameter :: An = 0.44      ! [obsol�te] parametre de la fonction nitrate
    real, parameter :: Bn = 0.0015    ! [obsol�te] parametre de la fonction nitrate (kg/mg N)
    real, parameter :: NO3c = 3.      ! [obsol�te] parametre de la fonction nitrate (mg N/kg)
    real, parameter :: Bn0 = (An + Bn * NO3c) / NO3c ! [obsol�te] parametre de la fonction nitrate (kg/mg N)

    ! Variables locales
    integer :: iz     ! indice de couche (1 = surface, �paisseur = 1 cm)
    real :: Dp        ! potential denitrification in each layer (kg N/ha/day/cm)
    real :: theta     ! teneur en eau volumique (cm3/cm3)
    real :: w         ! teneur en eau massique (g/g)
    real :: wfps      ! taux de saturation en eau
    real :: theta_sat ! teneur en eau volumique � saturation (cm3/cm3)
    real :: no3       ! concentration en nitrate de la couche iz (mg N/kg)
    real :: Fno3      ! Fonction nitrate sur la denitrification
    real :: Fwfps     ! Fonction eau/anoxie sur la denitrification
    real :: FwfpsN2O  ! Fonction eau/anoxie sur l'emission de N2O
    real :: Ftemp     ! Fonction temperature sur la denitrification
    real :: Denitz    ! amount of denitrified N in layer iz (kg N/ha/day/cm)
    real :: N2Odenitz ! amount of N2O-N produced by denitrification in layer iz (kg N/ha/day/cm)
    real :: Bw        ! parametre de la fonction eau sur fraction molaire
    real :: r         ! fraction molaire (fonction du wfps, du no3 et du pH)
    real :: r0        ! valeur maximale de la fraction molaire (r)   0.63
    real :: Gn        ! Fonction nitrate sur la fraction molaire
    real :: Corg      ! Corg pour calcul potentiel denit
    

    integer :: i                  ! numero de heure du jour (1 a 24)
    integer :: is_sat(40)         ! indice de saturation de la couche iz (0/1) ; 40 en lien avec la prof max de d�nit
    integer :: numcouche          ! compteur de couche

    real :: volpluierestant = 0.  ! voume d'eau restant a distribuer dans les couches inf�rieures
    real :: time(24)              ! fraction de jour decimal (0 a 1)
    real :: hur_horaire(24)       ! humidit� de heure i
    real :: wfps_horaire(24)      ! Taux de saturation en eau de l'heure i
    real :: Fwfps_horaire(24) = 0.! fonction eau correspondante
    real :: r_horaire(24) = 0.    ! fraction molaire correspondante
    real :: FwfpsN2O_horaire(24) = 0.

    ! initialisations
    Corg = CsurNsol * Norg 
    ! print *, Corg
    
    Ndenit = 0.
    em_N2Oden = 0.
    condenit = 0.
    is_sat = 0

    ! calcul Dp par couche : lecture ou calcul � partir de Corg
    Dp = P_vpotdenit / P_profdenit ! Dp kg/ha/cm (valeur lue dans fichier sol)
    !Jo�l 3/9/15 test fonction pour Dp : calcul Dp � partir de Corg (exp�rimental !)
    if(P_code_pdenit == 2) then
        ! fonction lin�aire de Corg entre cmin et cmax et avec plancher et plafond
        Dp = max(P_min_pdenit, min(P_max_pdenit, P_min_pdenit + (P_max_pdenit-P_min_pdenit)*   &
             (Corg-P_cmin_pdenit)/(P_cmax_pdenit-P_cmin_pdenit))) ! mg/kg/jour
        ! print *, Dp
        Dp = Dp * dacouche(1) / 10 ! kg/ha/cm ; id�al serait de faire le produit dans la boucle par couche pour avoir le Da de chaque couche
    !   print *, Dp*20 ! pour 20 cm
    endif

    ! choix lecture ou calcul ratio
    if(P_code_ratiodenit == 1) then ! cas o� la fraction molaire est constante et �gale � ratiodenit_constant
        Bw = 0. ! Fwfps N2O = 1
        Gn = 1.
        r0 = P_ratiodenit ! valeur lue
    else  ! cas o� la fraction molaire varie au cours du temps
        !Bw = 2.05 ! effet eau sur le ratio N2O/N2
        ! Jo�l 18/11/2016 Bw calcul� � partir de wfpsc
        Bw = 1. / (1. - P_wfpsc)
        ! valeur maximale de la fraction molaire = f(pH du sol)
        ! Jo�l 1/9/15 -> calcul r0 fonction de pH *pour wfps=0.815* et *forte concentration no3* (valeurs moyennes relation C. H�nault)
        r0 = 1. - (pH - P_pHminden) / (P_pHmaxden - P_pHminden)
        !r0 = r0 * 1.67 ! car on est � 81.5% wfps en moyenne dans les donn�es utilis�es et pas 100%
                       ! et qu'on applique ensuite l'effet eau (pour wfps =81.5% on doit retomber sur le r0 pr�c�dent)
        ! Jo�l 18/11/2016 -> nouveau facteur de correction
        r0 = r0 / (1. - Bw * max(0.,(0.815 - P_wfpsc)))

        ! modif Jo�l 1/9/15 -> lignes suivantes en commentaire, ne pas tout de suite borner
        ! (mais ne pas oublier ensuite apr�s avoir appliqu� les effets eau et NO3)
        !r0 = min(1.,r0)
        !r0 = max(0.,r0) ! on pourrait mettre un plancher > 0

    endif

    
    ! pour option calcul WFPS horaire
    if (P_code_hourly_wfps_denit == 1) then
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
                    theta_sat = (1.5 * hucc(numcouche) - 0.5 * humin(numcouche)) / 10. ! Jo�l : sols gonflants, pas clair pour moi !??
                else
                    theta_sat = 1. - dacouche(numcouche) / rho_s
                endif
                volpluierestant = volpluierestant - (theta_sat*10. - hur(numcouche))
                is_sat(numcouche) = 1
                numcouche = numcouche + 1
                if(numcouche == nint(P_profdenit)) exit ! inutile au del� de la profondeur de d�nit
            end do
        end if
    end if
   
    ! Boucle sur les couches contribuant � la d�nitrification
    ! -------------------------------------------------------
    do iz = 1, nint(P_profdenit)

        ! calcul des variables d'�tat
        if (P_codefente == 1) then                               ! option sols gonflants
            theta_sat = (1.5 * hucc(iz) - 0.5 * humin(iz)) / 10. ! Jo�l : non v�rifi�
        else
            theta_sat = 1. - dacouche(iz)/rho_s   ! cm3 eau/cm3 sol
        endif
        theta = (hur(iz) + sat(iz)) / 10.         ! cm3 eau/cm3 sol // (hur + sat) pas clair pour moi !
        w = theta / dacouche(iz)                  ! humidit� massique g eau/ g sol
        wfps = min(theta / theta_sat, 1.) ! je borne pour �viter les soucis, mais je ne sais pas pourquoi �a d�passe parfois 1 ??

        ! Fonction nitrate sur d�nitrification totale (Fno3)
        no3 = 10. * nit(iz)/dacouche(iz)          ! mg N/kg sol
        Fno3 = no3 / (P_Kd*w + no3)               ! Kd en mg N/l
        
        ! Fonction nitrate sur la fraction molaire (Gn, peu important et pas tr�s s�r !)
        ! proposition de changement Jo�l 1/9/15
        !if(code_ratiodenit == 2) Gn = min(Bn0 * no3, An + Bn * no3, 1.) ! obsol�te (trop complexe pour gain et incertitude sur effet !)
        if (P_code_ratiodenit == 2) Gn = no3 / (no3+1) ! 1 mg/kg
   
        ! Fonction temp�rature (Ftemp)
        !Ftemp = exp(((tsol(iz) - var_TREFdenit2) * log(Q10den))/10.)
        ! Jo�l 1/9/15 suppression et remplacement par fonction gaussienne Marie Benoit
        ! born�e 0-1 plut�t que r�f�rence � 20� et fonction ouverte !
        Ftemp = exp((-(tsol(iz)-P_tdenitopt_gauss)**2) / (P_scale_tdenitopt**2))
        
        ! Fonctions WFPS (d�nit et ratio)
        
        ! fonction(s) eau sur d�nit et ratio
        if (P_code_hourly_wfps_denit == 1 .and. is_sat(iz) == 1) then
            ! Calcul horaire de la redistribution d'eau dans les couches qui sont initialement satur�es
            ! apr�s une pluie
            ! 95% drainage � t=1 pour kdesat = 3.00 -> valeur par d�faut
            ! avec kdesat = 1.00 on ne draine que (63)% ...
            hur_horaire  = hucc(iz) + exp(-P_kdesat * time) * (theta_sat*10. - hucc(iz))
            wfps_horaire = min(hur_horaire / (theta_sat*10.), 1.)
            
            ! calcul ratio et fonction WFPS
            where (wfps_horaire > P_wfpsc)
                ! Fonction eau sur d�nitrification totale
                Fwfps_horaire = ((wfps_horaire - P_wfpsc) / (1. - P_wfpsc)) ** expwfps
                ! Fonction eau et nitrate sur les �missions de N2O
                r_horaire = min(1., max(0., r0 * (1.- Bw * (wfps_horaire - P_wfpsc)) * Gn))
                FwfpsN2O_horaire = Fwfps_horaire * r_horaire ! effet eau denit et r�duction combin�
            end where

            ! valeur moyenne
            Fwfps = sum(Fwfps_horaire)/24.
            FwfpsN2O = sum(FwfpsN2O_horaire)/24.
        else
            Fwfps = (max((wfps - P_wfpsc),0.) / (1. - P_wfpsc)) ** expwfps
            r = min(1., max(0., r0 * Gn * (1. - Bw * max(0.,(wfps - P_wfpsc))))) ! Jo�l 1/9/15 ajout bornes 0-1
            FwfpsN2O = Fwfps * r
        end if

        ! D�nitrification par couche
        Denitz = Dp * Fno3 * Ftemp * Fwfps ! kg N/ha/day/cm
        Denitz = min(Denitz, nit(iz))      ! on ne peut pas d�nitrifier plus que la quantit� de NO3 disponible dans la couche
        condenit = condenit + Denitz/Dp

        if(Fwfps > 0.) then
            N2Odenitz = Denitz * FwfpsN2O/Fwfps ! FwfpsN2O/Fwfps est �gal au ratio
        else
            N2Odenitz = 0.
        endif

        ! Actualisation du pool de nitrate
        nit(iz) = nit(iz) - Denitz

        ! Cumul sur les couches
        Ndenit = Ndenit + Denitz          ! cumul d�nitrification
        em_N2Oden = em_N2Oden + N2Odenitz ! cumul N2O

    end do ! fin boucle sur couches

    ! Mise � jour des cumuls dans le temps
    condenit = condenit / P_profdenit
    QNdenit = QNdenit + Ndenit
    Qem_N2Oden = Qem_N2Oden + em_N2Oden

    return
end subroutine denit_N2O
