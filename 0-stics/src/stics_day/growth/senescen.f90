! *******************************************************************
!    version 6.0
!    reste pb cultures fauch�es � voir avec Domi
!--------------------------------------------------
!    calcul de la senescence de la matiere seche
!    avec eventuellement une partie residuelle pour les cultures fauchees
!
!    calcul de la s�nescence du LAI pour l'option LAI brut
! derniere modif 30/05/05
! *******************************************************************
! In STICS shoot senescence only concerns leaves: dry matter and LAI.  For cut crops, it also affects residual biomass after cutting.
! - Stics book paragraphe 3.1.2, page 44-46
! While in the first versions of the model senescence was implicit (Brisson et al., 1998a), it is now explicit, with a clear distinction between natural
!! senescence due to the natural ageing of leaves, and senescence accelerated by stresses (water, nitrogen, frost). The concept of leaf lifespan,
!! used for example by Maas (1993), is applied to the green leaf area and biomass produced. The leaf area and part of the leaf biomass produced on a given day
!! is therefore lost through senescence once the lifetime has elapsed (Duru et al., 1995). This part corresponds to the ratiosen parameter, taking into account
!! the part which was remobilised during its senescence.
!!
!! Calculation of lifespan:
!!
!! The natural lifespan of leaves (durage) is defined by two values: the  lifespan of early leaves, or durvieI (expressed as a proportion of durvieF ) and the
!! lifespan of the last leaves emitted, or durvieF (assumed genotype-dependent). Until the IAMF stage, the natural lifespan, calculated for the day when the
!! leaves are emitted (I0) is durvieI; from IAMF to ILAX, the natural lifespan increases between durvieI and durvieF as a function of the leaf development variable ULAI.
!! Because of water or nitrogen stress, the current lifespan may be shortened if the stress undergone is more intense than previous stresses.
!! Specific stress indices for senescence are introduced (senfac and innsenes).  Frost (fstressgel that can be either fgeljuv or fgelveg) may also reduce or
!! even cancel  lifespan.  In the event of over-fertilisation with nitrogen (inn >1), the foliage lifespan is increased from the IAMF stage up to a maximum
!! given by the durviesupmax parameter.
!! The lifespan of leaves is not expressed in degree.days (like phasic development), because this has the disadvantage of stopping any progression as soon as
!! the temperature is lower than the base temperature (tdmin).  To remedy this problem, the senescence course (somsen) is expressed by cumulated Q10 units
!! (with Q10=2), i.e. an exponential type function.
!! The senescence course between I0 and I is affected by the same cardinal temperatures as phasic development and can be slown down by stresses. The lifespan parameter
!! of the leaf (durvieF) expressed in Q10 units represents about 20% of the same  lifespan expressed in degree.days.
!!
!! Calculation of senescence:
!!
!! Material produced on day I0 disappears by senescence after a period corresponding to durvie(I0). Depending on the evolution of temperature and of lifespan
!! as a function of phenology and stresses, senescence can vary from one day to another and affect several days of production (J=I0, I0+1, �) or, on the contrary,
!! none (durvieE(I0)>somsen(I)).  This principle is applied to the biomass (dltamsen) and leaf area index (dltaisen). In general, the leaf biomass produced
!! does not completely disappear (remobilisation):  the ratiosen (<1) parameter enables the definition of the senescent proportion with reference to production.
!! It is the pfeuilverte ratio which defines the proportion of leaves in the biomass produced.
!! The cumulative senescent foliage is laisen. If the crop is a forage crop benefiting from residual dry matter from the previous cycle (msresiduel parameter),
!! the senescence of residual dry matter (deltamsresen) starts as from cutting.  It occurs at a rate estimated from the relative daily lifespan course and
!! weighted by the remobilisation (ratiosen).
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine senescen(nlev, n, nbjmax, lai, P_codeinnact, P_codeh2oact, senfac, innsenes, P_codlainet, &
                    P_codeperenne, nbfeuille, P_nbfgellev, P_codgellev, tcultmin, P_tletale, P_tdebgel, &
                    P_tgellev90, P_tgellev10, nbplant, densitelev, densiteassoc, P_codgeljuv, &
                    P_tgeljuv90, P_tgeljuv10, P_codgelveg, P_tgelveg90, P_tgelveg10, masecveg, &
                    nstopfeuille, somcour, resperenne, ndrp, nrec, P_QNpltminINN, numcult, &
                    P_codeinitprec, ulai, P_vlaimax, durvieI, P_durvieF, inn, P_durviesupmax, &
                    P_codestrphot, phoi, P_phobasesen, dltams, P_msresiduel, P_ratiosen, tdevelop, &
                    somtemp, pfeuilverte, deltai, P_lai0, &
                    dernier_n, nsencour, dltaisen, dltamsen, fstressgel, fgellev, gelee, &
                    densite, laisen, nlan, P_stsenlan, nsen, P_stlaxsen, namf, nlax, P_stlevamf, &
                    P_stamflax, nrecbutoir, mortplante, nst2, mortplanteN, durvie, strphot, &
                    msres, dltamsres, ndebsen, somsenreste, msresjaune, mafeuiljaune, &
                    msneojaune, dltamstombe, QNplante, P_dltamsminsen, P_dltamsmaxsen, &
                    P_alphaphot, strphotveille, deltai_sencour, dltams_sencour, pfeuilverte_sencour)

   USE Divers, only: GEL
   USE Messages

   implicit none

   !: Arguments

   integer, intent(IN)    :: nlev
   integer, intent(IN)    :: n
   integer, intent(IN)    :: nbjmax
   real, intent(IN)    :: lai   ! // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
   integer, intent(IN)    :: P_codeinnact  ! // PARAMETER // code activating  nitrogen stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
   integer, intent(IN)    :: P_codeh2oact  ! // PARAMETER // code to activate  water stress effect on the crop: yes (1), no (2) // code 1/2 // PARAM // 0
   real, intent(IN)    :: senfac   ! // OUTPUT // Water stress index on senescence // 0-1
   real, intent(IN)    :: innsenes   ! // OUTPUT // Index of nitrogen stress active on leaf death // P_innmin to 1
   integer, intent(IN)    :: P_codlainet  ! // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
   integer, intent(IN)    :: P_codeperenne  ! // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
   integer, intent(IN)    :: nbfeuille   ! // OUTPUT // Number of leaves on main stem // SD
   integer, intent(IN)    :: P_nbfgellev  ! // PARAMETER // leaf number at the end of the juvenile phase (frost sensitivity)  // nb pl-1 // PARPLT // 1
   integer, intent(IN)    :: P_codgellev  ! // PARAMETER // activation of plantlet frost // code 1/2 // PARPLT // 0
   real, intent(IN)    :: tcultmin
   real, intent(IN)    :: P_tletale  ! // PARAMETER // lethal temperature for the plant // degree C // PARPLT // 1
   real, intent(IN)    :: P_tdebgel  ! // PARAMETER // temperature of frost beginning // degree C // PARPLT // 1
   real, intent(IN)    :: P_tgellev90  ! // PARAMETER // temperature corresponding to 90% of frost damage on the plantlet  // degree C // PARPLT // 1
   real, intent(IN)    :: P_tgellev10  ! // PARAMETER // temperature corresponding to 10% of frost damage on the plantlet  // degree C // PARPLT // 1
   integer, intent(IN)    :: nbplant      ! Number of plants
   real, intent(IN)    :: densitelev
   real, intent(IN)    :: densiteassoc
   integer, intent(IN)    :: P_codgeljuv  ! // PARAMETER // activation of LAI frost at the juvenile stadge // code 1/2 // PARPLT // 0
   real, intent(IN)    :: P_tgeljuv90  ! // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
   real, intent(IN)    :: P_tgeljuv10  ! // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (juvenile stage) // degree C // PARPLT // 1
   integer, intent(IN)    :: P_codgelveg  ! // PARAMETER // activation of LAI frost at adult stage // code 1/2 // PARPLT // 0
   real, intent(IN)    :: P_tgelveg90  ! // PARAMETER // temperature corresponding to 90 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
   real, intent(IN)    :: P_tgelveg10  ! // PARAMETER // temperature corresponding to 10 % of frost damage on the LAI (adult stage) // degree C // PARPLT // 1
   real, intent(IN)    :: masecveg   ! // OUTPUT // Vegetative dry matter // t.ha-1
   integer, intent(IN)    :: nstopfeuille
   real, intent(IN)    :: somcour   ! // OUTPUT // Cumulated units of development between two stages // degree.days
   real, intent(IN)    :: resperenne   ! // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
   integer, intent(IN)    :: ndrp
   integer, intent(IN)    :: nrec
   real, intent(IN)    :: P_QNpltminINN  ! // PARAMETER // minimal amount of nitrogen in the plant allowing INN computing // kg ha-1 // PARAM // 1
   integer, intent(IN)    :: numcult
   integer, intent(IN)    :: P_codeinitprec  ! // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0
   ! dans l'id�al max(n,dernier_n) ! (dernier_n)
   real, intent(IN)    :: ulai(nbjmax)  !   // OUTPUT // Daily relative development unit for LAI // 0-3
   real, intent(IN)    :: P_vlaimax  ! // PARAMETER // ULAI at inflection point of the function DELTAI=f(ULAI) // SD // PARPLT // 1
   real, intent(IN)    :: durvieI
   real, intent(IN)    :: P_durvieF  ! // PARAMETER // maximal  lifespan of an adult leaf expressed in summation of P_Q10=2 (2**(T-Tbase)) // P_Q10 // PARPLT // 1
   real, intent(IN)    :: inn   ! // OUTPUT // Nitrogen nutrition index (satisfaction of nitrogen needs ) // 0-1
   real, intent(IN)    :: P_durviesupmax  ! // PARAMETER // proportion of additional lifespan due to an overfertilization // SD // PARPLT // 1
   integer, intent(IN)    :: P_codestrphot  ! // PARAMETER // activation of the photoperiodic stress on lifespan : yes (1), no (2) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: phoi   ! // OUTPUT // Photoperiod // hours
   real, intent(IN)    :: P_phobasesen  ! // PARAMETER // photoperiod under which the photoperiodic stress is activated on the leaf lifespan // heures // PARPLT // 1
   ! dans l'id�al max(n,dernier_n) ! (dernier_n) ! ndebsen � dernier_n, puis si n < ndebsen, 1 � n
   real, intent(IN)    :: dltams(nbjmax)  !   // OUTPUT // Growth rate of the plant  // t ha-1.j-1
   real, intent(IN)    :: P_msresiduel  ! // PARAMETER // Residual dry matter after a cut // t ha-1 // PARTEC // 1
   real, intent(IN)    :: P_ratiosen  ! // PARAMETER // fraction of senescent biomass (by ratio at the total biomass) // between 0 and 1 // PARPLT // 1
   real, intent(IN)    :: tdevelop(n) ! 1 to n
   real, intent(IN)    :: somtemp   ! // OUTPUT // Sum of temperatures // degree C.j
   ! dans l'id�al max(n,dernier_n) ! (dernier_n) ! ndebsen � dernier_n, puis si n < ndebsen, 1 � n
   real, intent(IN)    :: pfeuilverte(nbjmax) !  // OUTPUT // Proportion of green leaves in total non-senescent biomass // 0-1
   ! dans l'id�al max(n,dernier_n) ! (dernier_n) ! ndebsen � dernier_n, puis si n < ndebsen, 1 � n
   real, intent(IN)    :: deltai(nbjmax)  !   // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
   real, intent(INOUT) :: deltai_sencour(nbjmax)  !  RV: // OUTPUT // Helper var. Save how much deltai was senescent each day // m2 leafs.m-2 soil
   ! NB: This variable has the only purpose to not use with AO the same deltai that has already been used as senescent with the AS.
   real, intent(INOUT) :: dltams_sencour(nbjmax) ! Same here
   real, intent(INOUT) :: pfeuilverte_sencour(nbjmax) ! Same here
   real, intent(IN)    :: P_lai0  ! // PARAMETER // Initial leaf area index // m2 m-2 // INIT // 1

   ! INOUT
   integer, intent(INOUT) :: dernier_n
   integer, intent(INOUT) :: nsencour
   real, intent(INOUT) :: dltaisen   ! // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
   real, intent(INOUT) :: dltamsen   ! // OUTPUT // Senescence rate // t ha-1 j-1
   real, intent(INOUT) :: fstressgel   ! // OUTPUT // Frost index on the LAI // 0-1
   real, intent(INOUT) :: fgellev
   logical, intent(INOUT) :: gelee
   real, intent(INOUT) :: densite   ! // OUTPUT // Actual sowing density // plants.m-2
   real, intent(INOUT) :: laisen(0:1) ! veille (0), aujourd'hui (1)    // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
   integer, intent(INOUT) :: nlan
   real, intent(INOUT) :: P_stsenlan  ! // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
   integer, intent(INOUT) :: nsen
   real, intent(INOUT) :: P_stlaxsen  ! // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1
   integer, intent(INOUT) :: namf
   integer, intent(INOUT) :: nlax
   real, intent(INOUT) :: P_stlevamf  ! // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
   real, intent(INOUT) :: P_stamflax  ! // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
   integer, intent(INOUT) :: nrecbutoir
   integer, intent(INOUT) :: mortplante
   integer, intent(INOUT) :: nst2
   integer, intent(INOUT) :: mortplanteN
   ! dans l'id�al max(n,dernier_n) ! (dernier_n) ! ndebsen � dernier_n, puis si n < ndebsen, 1 � n
   real, intent(INOUT) :: durvie(nbjmax) !   // OUTPUT // Actual life span of the leaf surface //  degree C
   real, intent(OUT)   :: strphot
   real, intent(INOUT) :: msres
   real, intent(INOUT) :: dltamsres
   integer, intent(INOUT) :: ndebsen
   real, intent(INOUT) :: somsenreste
   real, intent(INOUT) :: msresjaune   ! // OUTPUT // Senescent residual dry matter  // t.ha-1
   real, intent(INOUT) :: mafeuiljaune   ! // OUTPUT // Dry matter of yellow leaves // t.ha-1
   real, intent(INOUT) :: msneojaune   ! // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
   real, intent(IN)    :: dltamstombe
   real, intent(IN)    :: QNplante   ! // OUTPUT // Amount of nitrogen taken up by the plant  // kgN.ha-1

   ! STRESS PHOTOPERIODIQUE
   real, intent(IN)    :: P_dltamsminsen  ! // PARAMETER // threshold value of deltams from which the photoperiodic effect on senescence is maximal // t ha-1j-1 // PARAM // 1
   real, intent(IN)    :: P_dltamsmaxsen  ! // PARAMETER // threshold value of deltams from which there is no more photoperiodic effect on senescence // t ha-1j-1 // PARPLT // 1
   real, intent(IN)    :: P_alphaphot  ! // PARAMETER // parameter of photoperiodic effect on leaf lifespan // P_Q10 // PARAM // 1
   real, intent(INOUT) :: strphotveille
   ! -- FIN STRESS PHOTOPERIODIQUE

   !  integer :: codegel  !
   integer :: i  !
   integer :: ncompteur  !
   integer :: debut  !
   integer :: fin
   real    :: vitsenres  !
   real    :: durage  !
   real    :: durviesup  !
   real    :: senstress  !
   real    :: somsen  !
   !  real    :: tgel10  !
   !  real    :: tgel90
   real    :: msresTMP
   real    :: QNpltmin ! This is just to recompute P_QNpltminINN in case of intercroping

   !:On shunte ce programme si la plante n'est pas lev�e ou si le lai est nul
   logical :: dbg_sen
   dbg_sen = .FALSE.

   if (nbplant == 1) then
      QNpltmin = P_QNpltminINN
   else
      QNpltmin = P_QNpltminINN/2 ! We divide by 2 if we are in intercrop
      ! NB: this is a crude approximation but this parameter has very low importance
   end if

   if (nlev == 0) then
      ! *- 07/09/06 DR et NB et IGC (la sainte famille)
      ! *- apres un 2 jours d'errements nadine a �t� notre lumiere !!
      ! *- Qu'elle en soit lou�e !
      ! *- initialisation de nsencour au debut du cycle
      nsencour = n
      return
   end if

   !: PB - 08/03/2005 - si encore de la mati�re jaune on continue la senescence
   if (lai <= 0 .and. mafeuiljaune <= 0.) then
      dltaisen = 0.
      dltamsen = 0.
      return
   end if

   ! ** calcul de la dur�e de vie
   ! *---------------------------
   ! *-  a) calcul du LAI net : on utilise version 4 avec une dur�e de vie fixe (stlevsenms)
   ! *-  b) calcul du LAI brut: la dur�e de vie est fonction de l'age de la plante
   ! *-                         (dur�e de vie potentielle x facteurs de stress)
   ! *-   dur�e de vie potentielle des feuilles (durage) calcul�e dans calai
   ! *-   facteurs de stress eau et azote calcul�s � partir de senfac (transpi.for)
   ! *-   et innsenes (absorn.for)
   ! *-------------------------------------------------------------------
   ! *- sp�cifiques de la s�nescence

   ! ** Introduction codeHxN - NB - le 12/04/05
   ! *- ML - le 29/05/07 : suppression de la possibilit� de multiplier les deux stress
   ! *- hydrique et azot� pour r�duire la dur�e de vie des feuilles;
   ! *- donc suppression du codeHxN
   !      if (P_codeinnact == 1 .and. P_codeh2oact == 1) then
   !         if (codeHxN == 1) then
   !           senstress = min(senfac,innsenes)
   !         else
   !           senstress = senfac*innsenes
   !         endif
   !      endif

   if (P_codeinnact == 1 .and. P_codeh2oact == 1) then
      senstress = min(senfac, innsenes)
   end if

   if (P_codeinnact == 2 .and. P_codeh2oact == 1) then
      senstress = min(senfac, 1.)
   end if

   if (P_codeinnact == 1 .and. P_codeh2oact == 2) then
      senstress = min(1., innsenes)
   end if

   if (P_codeinnact == 2 .and. P_codeh2oact == 2) senstress = 1.

   if (P_codlainet == 1) senstress = 1.

   ! ** calcul du STRESS li� au GEL  (utilisation de la fonction GEL.for)
   ! *-------------------------------------------------------------------
   ! *- GEL entre lev�e et plantule diminue la densit� de population uniquement pour les annuelles
   if (P_codeperenne == 1) then
      if (nbfeuille <= P_nbfgellev) then
         fgellev = GEL(P_codgellev, tcultmin, P_tletale, P_tdebgel, P_tgellev90, P_tgellev10)
         if (fgellev < 1.) gelee = .TRUE.
         densite = min(densite, densitelev*fgellev)
      end if
   end if

   ! ** GEL de l'appareil v�g�tatif � deux stades: juv�nil (avant AMF) et adulte (apr�s AMF)
   if (namf <= 0) then
      fstressgel = GEL(P_codgeljuv, tcultmin, P_tletale, P_tdebgel, P_tgeljuv90, P_tgeljuv10)
   else
      fstressgel = GEL(P_codgelveg, tcultmin, P_tletale, P_tdebgel, P_tgelveg90, P_tgelveg10)
   end if

   if (fstressgel < 1.) gelee = .TRUE.

   ! ** GEL LETAL pour la plante
   if (fstressgel <= 0.) then
      dltaisen = lai
      dltamsen = masecveg
      laisen(1) = laisen(0) + dltaisen

      ! ** affectation des stades
      if (nstopfeuille > 0 .and. nlan == 0) then
         nlan = n
         P_stsenlan = somcour
         if (nsen == 0) then
            nsen = n
            P_stlaxsen = somcour
            P_stsenlan = 0.
         end if
      else
         if (resperenne <= 0.) then
            if (namf == 0) then
               namf = n
               nlax = n
               nsen = n
               nlan = n
               P_stlevamf = somcour
               P_stamflax = 0.
               P_stlaxsen = 0.
               P_stsenlan = 0.
            end if
            if (nlax == 0 .and. namf > 0) then
               nlax = n
               nsen = n
               nlan = n
               P_stamflax = somcour
               P_stlaxsen = 0.
               P_stsenlan = 0.
            end if
            if (nsen == 0 .and. nlax > 0) then
               nsen = n
               nlan = n
               P_stlaxsen = somcour
               P_stsenlan = 0.
            end if
         end if
      end if
      if (resperenne <= 0.) then
         call EnvoyerMsgHistorique(1167)
         !: modif - NB - 20/09 - mort le jour d'apres pour �criture rapport
         nrecbutoir = n + 1
         mortplante = 1
         !: pour non plantade dans bilan.for - TODO : � mettre ici ou dans bilan ?
         if (ndrp == 0) nst2 = 1
      end if
      goto 30
   end if

   if (n == nlev .and. P_codeperenne == 1) then
      nsencour = nlev
   end if

   ! *- DR et NB - 25/08/08 - s'il n'y a plus d'azote dans la plante on la fait mourir
   ! DR 11/04/2012 pour le prairie on a des pbs car quand on coupe on a plus d'azote dans la  plante
   ! donc je mets < au lieu de <=
   if ((namf > 0 .and. nrec == 0) .and. QNplante < QNpltmin) then
      call EnvoyerMsgHistorique(2099)
      ! modif NB 20/09 mort le jour d'apres pour �criture rapport
      nrecbutoir = n + 1
      mortplanteN = 1
      ! pour non plantade dans bilan.for
      if (ndrp == 0) nst2 = 1
   end if

   ! *- NB - le 22/12/2003 - pb senescence perenne vu avec FR
   ! *- PB - 05/01/2005 - mise en commentaire jusqu'� nouvel ordre.
   ! *- Sinon cela perturbe le calcul de la s�nescence lors de l'enchainement des p�rennes.
   if (n == nlev .and. P_codeperenne == 2 .and. numcult == 1) then
      nsencour = 1
   end if
   ! 11/01/2017 pour la senescence des perennes on demarre la senescence le premier jour toutes les annees si on est en reinitialisation
   if (n == nlev .and. P_codeperenne == 2 .and. P_codeinitprec == 1) then
      nsencour = 1
   end if

   ! ** calcul de la dur�e de vie
   ! *----------------------------
   ! *-  nsenscour = jour o� la biomasse s�nescente a �t� produite
   ! *-  durage = dur�e de vie potentielle fonction de la P_ph�nologie
   ! *-  (varie entre durvieI et P_durvieF; durvieI avant lax)

   debut = nsencour
   if (dbg_sen) write (1111, *) 'debut:', debut, 'nsencour:', nsencour, 'nlev', nlev

   if (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1 .and. nsencour > n) then
      fin = dernier_n
   else
      fin = n
      dernier_n = n
   end if

5  do i = debut, fin
      !         write(*,*)'i',debut,fin
      if (ulai(i) <= P_vlaimax) then
         durage = durvieI
      else
         durage = durvieI + ((ulai(i) - P_vlaimax)/(3.-P_vlaimax))*(P_durvieF - durvieI)
         !          write(*,*)n,'durage', P_durvieF,durage
      end if
      ! ** application des stress qui raccourcissent la dur�e de vie
      ! if (i < fin .and. durvie(i) > 0.0) then

      if (i < fin .and. nsencour > nlev) then
         durvie(i) = min(durvie(i), durage*min(senstress, fstressgel))
      else
         durvie(i) = durage*min(senstress, fstressgel)
      end if
      ! ** augmentation de dur�e de vie maxi en cas de surfertilisation azot�e
      if (namf > 0 .and. i > namf .and. P_codlainet == 2) then
         if (inn > 1.) then
            durviesup = P_durvieF*min(P_durviesupmax, (inn - 1.))
            !                        write(*,*)n,'durviesup',P_durvieF,durviesup
         else
            durviesup = 0.
         end if
      else
         durviesup = 0.
      end if
      durvie(i) = durvie(i) + durviesup
      ! domi 04/01/06 pour inaki insertion du stress photoperiodique
      ! NB le 22/06/06 introduction dans la boucle des "i"
      if (P_codestrphot == 1 .and. nlax /= 0) then
         if (phoi < P_phobasesen) then
            call stressphot(dltams(n), P_dltamsminsen, P_dltamsmaxsen, P_alphaphot, strphot, strphotveille)
            durvie(i) = durvie(i)*strphot
         end if
      end if
   end do

   if (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1 .and. debut > n) then
      debut = 1
      fin = n
      goto 5
   end if

! ** la base de temps de la s�nescence est calcul�e dans develop.for
! *- tdevelop(n) est un P_Q10 somm� dans la variable somtemp

! ** pour les cultures fauch�es : senescence de la matiere seche r�siduelle
! *- qui demarre d�s la coupe
! *- PB - 25/01/2004 - ajout de msresTMP et dltamsres pour calculer le delta msres(n-1)<->msres(n)
! *- et ce, pour pallier � un probl�me dans le calcul de msresjaune pour les p�rennes enchain�es fauch�es
   if (msres > 0.) then
      vitsenres = P_msresiduel/durvieI
      msresTMP = msres - (P_ratiosen*vitsenres*tdevelop(n))
      msresTMP = max(0., msresTMP)
      dltamsres = max(0., msres - msresTMP)
      msres = msresTMP
   else
      dltamsres = 0.
   end if

! ** pour les autres cultures ou pour la mati�re s�che n�oform�e

! ** DOMI VOIR POUR PRAIRIE UTILISATION DE somcourfauche
! ** 2/ senescence de la matiere seche neoformee qui demarre au stade
! *- debut de senescence de la matiere seche identifie par le parcours
! *- de developpement depuis la levee : stlevsenms
! *- on calcule vitsen le jour du d�but de s�nescence : ndebsen
   if (dbg_sen) then
      write (1111, *) '*** n avant calcul senescence; n:', n, 'debut:', debut, 'fin:', fin
   end if
   if (somtemp >= durvie(nsencour)) then
      if (dbg_sen) write (1111, *) '(A) Senescence ; somtemp:', somtemp, 'durvie(n):', durvie(nsencour)
      if (ndebsen == 0) then ! premier jour de senescence
         ndebsen = n
         nsencour = nlev
         dltamsen = dltams(nsencour)*P_ratiosen*pfeuilverte(nsencour)
         dltams_sencour(nsencour) = dltams(nsencour)
         pfeuilverte_sencour(nsencour) = pfeuilverte(nsencour)
         if (P_codlainet == 2) then
            dltaisen = deltai(nsencour) + P_lai0
            deltai_sencour(nsencour) = deltai(nsencour)
         end if
         if (dbg_sen) write (1111, *) '(A.a) First senescence ; ndebsen:', ndebsen, 'nsencour:', nsencour, &
            'dltams(nsencour):', dltams(nsencour), 'dltamsen', dltamsen, &
            'dltaisen', dltaisen

         ! ** enlever le reste du LAI si deltai(nsencour) = 0.

      else  ! jours suivants
         somsen = somsenreste
         dltamsen = 0.
         ncompteur = 0
         if (P_codlainet == 2) dltaisen = 0.
         if (dbg_sen) write (1111, *) '(A.b) Following senescence ; n:', n, 'somsenreste', somsenreste, &
            'somsen:', somsen
         ! ** calcul de la somme de temp�rature depuis le dernier jour de s�nescence
         ! *- PB - 05/01/2005 - ajout d'un test pour l'enchainement des p�rennes. On calcul somsen
         ! *- � partir du nsencour du cycle pr�c�dent jusqu'� la fin du cycle pr�c�dent et
         ! *- on l'ajoute � la somme des temp�ratures depuis le d�but du cycle courant jusqu'au jour n
         if (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1 .and. nsencour > n) then
            do i = nsencour + 1, dernier_n
               somsen = somsen + tdevelop(i)
            end do
            do i = 1, n
               somsen = somsen + tdevelop(i)
            end do
         else
            do i = nsencour + 1, n
               somsen = somsen + tdevelop(i)
            end do
         end if
         if (dbg_sen) write (1111, *) '(A.b) New somsen:', somsen
         if (somsen > durvie(nsencour + 1)) then
            if (dbg_sen) write (1111, *) '(A.b.1) Further senescence: somsen>durvie(nsencour+1); durvie:', &
               durvie(nsencour + 1), 'nsencour', nsencour, 'somsenreste', somsenreste, &
               'somsen:', somsen
20          nsencour = nsencour + 1

            ! *- PB - 05/01/2004 - ajout d'un test lors de l'enchainement des p�rennes pour que nsencour
            ! *- ne d�passe pas dernier_n. Si nsencour>dernier_n alors on a atteint la fin du cycle pr�c�dent,
            ! *- et donc on doit repartir du d�but du cycle courant. Soit nsencour = 1
            if (P_codeperenne == 2 .and. P_codeinitprec == 2 .and. numcult > 1 .and. nsencour > dernier_n) then
               nsencour = 1
            end if

            somsen = somsen - durvie(nsencour)
            if (somsen < 0.) then
               somsen = 0.
               nsencour = nsencour - 1
               ! somsenreste = somsen
               if (dbg_sen) then
                  write (1111, *) '(A.b.1.a) End of senescence with somsen<0,somsenreste:', somsenreste
                  write (1111, *) ''
               end if
               goto 30
            end if
            dltamsen = dltamsen + (dltams(nsencour)*P_ratiosen*pfeuilverte(nsencour))
            dltams_sencour(nsencour) = dltams(nsencour)
            pfeuilverte_sencour(nsencour) = pfeuilverte(nsencour)

            ncompteur = ncompteur + 1
            if (P_codlainet == 2) then
               dltaisen = dltaisen + deltai(nsencour)
               deltai_sencour(nsencour) = deltai(nsencour)
            end if

            if (dbg_sen) write (1111, *) '(A.b.1.b) somsen>=0:', somsen, 'dltamsen:', dltamsen, &
               'dltams(nsencour):', dltams(nsencour), 'pfeuilverte(nsencour)', pfeuilverte(nsencour), &
               'deltai(nsencour)', deltai(nsencour), 'dltaisen', dltaisen

            ! ** si la dur�e de vie est raccourcie, on peut "faire mourir" plusieurs jours de production
            ! *- PB - 10/03/2005 - Petite modificatoin, on compare nsencour � dernier_n et non plus � n.

            if (somsen > durvie(nsencour) .and. nsencour < dernier_n) then
               if (dbg_sen) write (1111, *) '(A.b.2) Continue sen: somsen>durvie and nsencour<dernier_n ;', &
                  'durvie', durvie(nsencour), 'nsencour', nsencour, 'dernier_n', dernier_n
               goto 20
            end if

            somsenreste = somsen
            if (dbg_sen) then
               write (1111, *) '(A.b.3) End of senescence; somsenreste:', somsenreste
               write (1111, *) ''
            end if
         else
            dltamsen = 0.
            if (P_codlainet == 2) dltaisen = 0.
            ! somsenreste = somsen
            if (dbg_sen) then
               write (1111, *) '(A.b.2) End of senescence: somsen<durvie; durvie:', &
                  durvie(nsencour + 1), 'nsencour', nsencour, 'somsenreste', somsenreste, &
                  'somsen:', somsen
               write (1111, *) ''
            end if
         end if
      end if
   else
      dltamsen = 0.
      if (P_codlainet == 2) dltaisen = 0.
      if (dbg_sen) then
         write (1111, *) '(B) No senescence, somsenreste:', somsenreste
         write (1111, *) ''
      end if
   end if

30 continue

! **      msresjaune   = mati�re s�nescente r�siduelle
! *-      msneojaune   = mati�re s�nescente n�oform�e (limit�e au v�g�tatif)
! *-      mafeuiljaune = mati�re s�nescente cumul�e
! *- Hyp: msneojaune   = mafeuiljaune
   msresjaune = msresjaune + dltamsres
!write(1504,*)'avant calcul ',msneojaune,mafeuiljaune,dltamsen,dltamstombe
! PB&Inaki - 08/03/2005 : On enl�ve les feuilles tomb�es de la mati�re jaune
   mafeuiljaune = mafeuiljaune + dltamsen - dltamstombe

! write(1504,*)msneojaune,mafeuiljaune,dltamsen,dltamstombe
   msneojaune = mafeuiljaune
! write(1504,*)msneojaune

! Ben voil� encore une nouveaut� ....
! NB le 26/06/06
!      nsen = ndebsen

!dr 14/10/2011 ecriture de 2 varaibles locales pour pb inde AgMIP
!write(618,*)n,durage,senstress
!write(*,*)'fin senescen'
   return
end subroutine senescen
