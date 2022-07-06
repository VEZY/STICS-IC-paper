! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
! This subroutine calls the main modules involved in crop growth simulation:
!   - biomaer.f90 : shoot biomass growth
!   - senescen.f90 : leaf senescence
!   - ApportFeuillesMortes.f90 : decomposition of leaves fallen at soil surface
!   - fruit.f90 : yield formation for indeterminate crops
!   - grain.f90 : yield formation for determinate crops
!   - eauqual.f90 : water content evolution in harvested organs
!   - croissanceFrontRacinaire.f90 : root front growth
!   - densiteRacinaire.f90 : root density profile
!   - calpsibase.f90 : plant water potential
!   - repartir.f90 : dimensioning of organs
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine croissance(sc, pg, p, itk, soil, c, sta, t)

   USE Stics
   USE Plante
   USE Itineraire_Technique
   USE Sol
   USE Climat
   USE Station
   USE Parametres_Generaux
   USE Divers
   USE Module_Croissance ! Pour les interfaces des routines

   implicit none

   type(Stics_Communs_), intent(INOUT) :: sc
   type(Parametres_Generaux_), intent(IN)    :: pg
   type(Plante_), intent(INOUT) :: p(sc%P_nbplantes)
   type(ITK_), intent(INOUT) :: itk(sc%P_nbplantes)
   type(Sol_), intent(INOUT) :: soil
   type(Climat_), intent(INOUT) :: c
   type(Station_), intent(INOUT) :: sta
   type(Stics_Transit_), intent(INOUT) :: t

   !: Variables locales
   integer :: ens
   integer :: n
   integer :: i ! indice plante
   integer :: ic  !
   integer :: ii  !
   integer :: iz
   integer :: i2
   integer :: n2 ! Used to identify the other plant if any
   real :: repracmin  !
   real :: repracmax  !
   real :: kreprac
   real :: trg_abv
   real :: densiteassoc
   integer :: profhoriz
   real :: deltai_tmp(sc%nbjmax)
   real :: dltams_tmp(sc%nbjmax)
   real :: pfeuilverte_tmp(sc%nbjmax)
   logical :: true_dominance ! Are we in the case of a true dominance between both crops (for intercropping)
   ! ** on distingue la culture principale et la culture associ�e car les traitements
   ! *- sont diff�rents. Pour la culture principale on a notament besoin de calculer
   ! *- la variable originehaut et pour la culture associ�e d'affecter des trg(n) rapport�s
   ! *- au rayonnement intercept� par la culture principale

   ! pour all�ger l'�criture
   n = sc%n

   if (sc%P_nbplantes > 1) then
      densiteassoc = p(2)%densiteequiv
      true_dominance = abs(p(1)%hauteur(0) - p(2)%hauteur(0)) .gt. t%P_hauteur_threshold
   else
      densiteassoc = 0.
      true_dominance = .true.
   end if
   ! RV: true_dominance is true if the difference between the crops height is higher than P_hauteur_threshold

   do i = 1, sc%P_nbplantes
      do ens = sc%AS, sc%AO
         ! DR et ML le 07/09/2012: suppression de la ligne en dessous car cela emp�chait la croissance racinaire
         ! de la plante associee entre germination et levee de la plante principale
         !         if (p(i)%surf(ens) > 0.) then ! on ne travaille que si la surface de la plante est sup�rieur � z�ro.
         ! DR et ML 12/09/2012 on teste pour que ca focntionne avce plante puer et Cas (si on l'enlevait ca induisait des ecarts pour la vigne pure)
         if (p(i)%surf(ens) > 0.) then ! on ne travaille que si la surface de la plante est sup�rieur � z�ro.
            if (p(i)%P_codeplante /= 'snu') then

               if (i < sc%P_nbplantes) then
                  sc%originehaut = max(p(i + 1)%hauteur(sc%AO), p(i + 1)%hauteur(sc%AS))
               else
                  sc%originehaut = 0.0
               end if

               trg_abv = c%trg(n)

               ! RV: c%trg(n) was computed using rsoleil only, but rombre should be used while computing sc%AO
               ! There are two cases to compute the light incident to the plants:
               ! 1) The dominant plant is considered above the dominated plant (dominancy case):
               !   Explanation 1: p(i-1)%rsoleil represents the fraction of the radiation incoming right above the plant i,
               !   i.e. the light transmitted to the associated plant, for the sunlit part (rombre is used for the shaded one).
               !   So trg_abv * p(i-1)%rsoleil is the actual amount of radiation transmitted to the sunlit part of the associated plant,
               !   and trg_abv * p(i-1)%rombre is the actual amount of radiation transmitted to the shaded part of the associated plant.
               ! 2) There is no clear dominancy because both plants have approx. the same height (Delta height<hauteur_threshold):
               !   Explanation 2: the trg is not transmitted from one plant to the other, but shared between both plants. In that case,
               !   the competition for light arises only through the LAI of each plant.
               ! The implementation is made here, and further below to set rsoleil from the dominated plant to 1 if case 2:

               ! Case 1: true dominance:
               if ((i .gt. 1) .and. (p(i)%P_codetransrad == 2) .and. (true_dominance .eqv. .true.)) then
                  if (ens .eq. sc%AS) then
                     trg_abv = trg_abv*p(i - 1)%rsoleil
                  else
                     trg_abv = trg_abv*p(i - 1)%rombre
                  end if
               end if

              if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg2, *) 'nbio', n, i, ens, p(i)%magrain(0, n - 1), &
                  p(i)%chargefruit

               if (i == 1) then
                  i2 = 2
                  n2 = n - 1 ! We use the lai from the day before for the dominated plant because it is
                  ! not computed yet
               else
                  i2 = 1
                  n2 = n
               end if

               !  print *,'avant biomaer'

               call biomaer(n, i, ens, p(i)%nlev, p(i)%P_codeperenne, p(i)%nplt, p(i)%P_codehypo, p(i)%P_codegermin, &
                            p(i)%P_masecplantule, p(i)%P_adil, p(i)%namf, p(i)%P_adilmax, p(i)%nrec, itk(i)%P_codcueille, &
                            itk(i)%P_codefauche, p(i)%P_codelaitr, p(i)%P_codetransrad, p(i)%P_efcroijuv, p(i)%P_efcroiveg, &
                            p(i)%ndrp, p(i)%P_efcroirepro, p(i)%chargefruit, pg%P_coefb, sc%tcult, p(i)%P_teopt, p(i)%P_teoptbis, &
                        p(i)%P_temin, p(i)%P_temax, sta%P_codeclichange, p(i)%P_alphaco2, c%co2(n), t%P_resplmax(i), p(i)%densite, &
                        p(i)%densiteequiv, pg%P_codeh2oact, p(i)%swfac(ens), p(i)%exobiom, pg%P_codeinnact, p(i)%dltaremobil(ens), &
                            p(i)%P_codeindetermin, p(i)%fpv(ens, n), p(i)%fpft(ens), p(i)%P_remobres, p(i)%P_resperenne0, &
                            p(i)%demande(ens), p(i)%P_QNplante0, itk(i)%P_msresiduel(p(i)%numcoupe - 1), itk(i)%P_nbcueille, &
                            ! dr 07/12/2010 c'est nbrecolte-1 et non nbrecolte si on se fie � 7.1
                            p(i)%rdtint(ens, p(i)%nbrecolte - 1), p(i)%CNgrain(ens), t%P_codemontaison(i), t%nmontaison(i), &
                            ! dr et ML 29/08/2014
                            ! masec_veille doit etre masec global de la plante : masec(0,n-1) et non masec(ens, n-1) sachant que c'est ensuite pond�r�
                            ! par les surfaces � l'ombre et au soleil et que ces surfaces evoluent d'un jour � l'autre , notamment si il y a inversion de dominance
                            !                          p(i)%masec(ens,n-1),p(i)%masec(ens,n),p(i)%QNplante(ens,n-1),p(i)%QNplante(ens,n),                  &
                            ! idem pour qnplante
                            !                           p(i)%masec(0,n-1),p(i)%masec(ens,n),p(i)%QNplante(ens,n-1),p(i)%QNplante(ens,n),                    &
                            p(i)%masec(0, n - 1), p(i)%masec(ens, n), p(i)%QNplante(0, n - 1), p(i)%QNplante(ens, n), &
                            p(i)%inns(ens), p(i)%inn(ens), p(i)%innlai(ens), sc%cumdltaremobilN, p(i)%ebmax, p(i)%ftemp, &
                            p(i)%epsib(ens), p(i)%fco2, p(i)%dltams(ens, n), p(i)%dltamsen(ens), p(i)%dltamstombe(ens), &
                            p(i)%resperenne(ens), t%dltamsN(i, ens), p(i)%photnet(ens, n), p(i)%sourcepuits(ens), &
                            t%dltaremobilN(i, ens), p(i)%remobilj(ens), p(i)%cumdltares(ens), p(i)%magrain(0, n - 1), &
                            p(i)%magrain(ens, n), p(i)%masecneo(ens), p(i)%surf, p(i)%surfSous, sc%P_nbplantes, p(i)%P_extin, &
                            p(i2)%P_extin, p(i)%cumrg, p(i)%cumraint, p(i)%fapar(ens), &
                            p(i)%P_adfol, p(i)%lairognecum(ens), p(i)%laieffcum(ens), p(i)%P_dfolbas, p(i)%P_dfolhaut, p(i)%dfol, &
                        sc%rdif, p(i)%parapluie, p(i)%raint(ens), pg%P_parsurrg, p(i)%P_forme, p(i)%lai(ens, n), p(i2)%lai(0, n2), &
                            p(i)%laisen(ens, n), p(i)%eai(ens), p(i2)%eai(0), itk(i)%P_interrang, p(i)%nlax, p(i)%nsen, &
                            p(i)%P_codlainet, p(i)%P_hautbase, itk(i)%P_codepalissage, itk(i)%P_hautmaxtec, itk(i)%P_largtec, &
                            sc%originehaut, p(i)%hauteur(ens), p(i)%deltahauteur(ens), p(i)%P_hautmax, p(i)%varrapforme(ens), &
                            p(i)%largeur(ens), sc%jul, trg_abv, sta%P_latitude, p(i)%rombre, p(i)%rsoleil, itk(i)%P_orientrang, &
                          p(i)%P_ktrou, sc%tauxcouv(n), pg%P_khaut, sc%surfAO, sc%surfAS, p(i)%fpari, p(i)%P_masec0, t%P_hautK(i), &
                            t%P_hautA(i), t%P_code_shape(i), p(i)%nflo, p(i)%nmat, t%P_stage_const_height(i), t%P_elongation(i), &
                            t%P_hautdens(i), p(i)%ef_elongation, true_dominance, t%P_haut_dev_x0(i), &
                            t%P_haut_dev_k(i), p(i)%somupvtsem, p(i)%turfac(ens), t%P_nw_height(i), p(i)%ef_n_w_height(ens), &
                            p(i)%potential_height(ens), p(i)%fstressgel, p(i)%exolai, t%P_code_strip, t%P_nrow(i), &
                            sc%light_beer_active)

               ! on met � jour le cumul des parties AO/AS de masec, dltams calcul�s dans biomaer
               ! TODO: si on passe pour chaque partie AO/AS on effectue le calcul 2 fois... y'a pas mieux � faire ?
               p(i)%masec(0, n) = p(i)%masec(sc%AS, n)*p(i)%surf(sc%AS) + p(i)%masec(sc%AO, n)*p(i)%surf(sc%AO)
               p(i)%dltams(0, n) = p(i)%dltams(sc%AS, n)*p(i)%surf(sc%AS) + p(i)%dltams(sc%AO, n)*p(i)%surf(sc%AO)
               p(i)%pfeuilverte(0, n) = p(i)%pfeuilverte(sc%AS, n)*p(i)%surf(sc%AS) + p(i)%pfeuilverte(sc%AO, n)*p(i)%surf(sc%AO)
               p(i)%deltai(0, n) = p(i)%deltai(sc%AS, n)*p(i)%surf(sc%AS) + p(i)%deltai(sc%AO, n)*p(i)%surf(sc%AO)
               ! RV: recomputing FAPAR using the PAR from the atmosphere (and not only the PAR transmitted to the dominated plant)
               p(i)%fapar(ens) = (p(i)%raint(ens)/(trg_abv*pg%P_parsurrg))*p(i)%surf(ens)

               if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg2, *) 'nbio', n, i, ens, p(i)%magrain(0, n - 1) &
                  , p(i)%chargefruit

               ! On en profite pour attribuer les surfaces ombre/soleil aux plantes domin�es
               ! DR et ML 20/04/2016 on ne calcule les surfaces que si les 2 plantes ont lev� (et pas uniquement la principale)
               ! on rajoute une condition sur la lev�e des 2 plantes
               if (sc%P_nbplantes > 1 .and. p(1)%nlev .gt. 0 .and. p(2)%nlev .gt. 0) then
                  if (i < sc%P_nbplantes) then
                     ! RV: case 1, true dominance:
                     if ((abs(p(1)%hauteur(0) - p(2)%hauteur(0)) .gt. t%P_hauteur_threshold) .and. (p(i)%P_codetransrad .eq. 2) &
                         .and. (p(2)%hauteur(0) > 0.0)) then
                        ! If the associated plant had only sunlit area the previous day (i.e. the ao part is new):
                        if (p(i + 1)%surf(sc%AS) .eq. 1.0) then
                           ! 1/ initialise the lai of the shaded part to the value of the sunlit part:
                           p(i + 1)%lai(sc%AO, n) = p(i + 1)%lai(sc%AS, n)
                           ! 2/ set the shaded senescence vars equal to the sunlit component also:
                           p(i + 1)%nsencour(sc%AO) = p(i + 1)%nsencour(sc%AS)
                           p(i + 1)%ndebsen(sc%AO) = p(i + 1)%ndebsen(sc%AS)
                           p(i + 1)%durvie(sc%AO, 1:sc%nbjmax) = p(i + 1)%durvie(sc%AS, 1:sc%nbjmax)
                           p(i + 1)%hauteur(sc%AO) = p(i + 1)%hauteur(sc%AS)
                           p(i + 1)%largeur(sc%AO) = p(i + 1)%largeur(sc%AS)
                           p(i + 1)%varrapforme(sc%AO) = p(i + 1)%varrapforme(sc%AS)
                           p(i + 1)%Qfix(sc%AO) = p(i + 1)%Qfix(sc%AS)
                           p(i + 1)%potential_height(sc%AO) = p(i + 1)%potential_height(sc%AS)
                        end if

                        ! Now lets set the surface of the shaded and the sunlit parts according to the light transmitted
                        ! by the dominant plant:
                        p(i + 1)%surf(sc%AS) = p(i)%surfSous(sc%AS)
                        p(i + 1)%surf(sc%AO) = p(i)%surfSous(sc%AO)

                        ! RV: case 2, no true dominance, plants are side by side so there is only a sunlit part:
                        ! p(i+1)%surf(sc%AS) = 1.0
                        ! p(i+1)%surf(sc%AO) = 0.0
                        ! RV: moved to Stics_Jour because now the whole light interception is modified in this case (fall back to beer2),
                        ! so we have to use the same light interception module for both plants (then we have to know it at the beggining of the day,
                        ! not mid-computation)
                     end if
                  end if
               end if

               p(i)%gelee = .FALSE.

               ! DR et julie - 12/08/08 : le qnplante n'est pas encore calcul� on prend celui de la veille
               ! le trio 26/07/2012 si on a un qressuite c'est qu'on a fait l'apport des residus � la recolte donc on passe pas dans senescen et apportfeuillemortes

               !    avant 26/07/2012          if (p(i)%P_codelaitr == 1) then
               !    apres 26/07/2012          if (p(i)%P_codelaitr == 1.and.p(i)%qressuite_tot == 0) then
               ! DR et ML le 05/04/2013 cas specifique des cultures dont on laisse une partie en place qui peut senescer (ex prairie apres une coupe : on veut calculer msresjaune)
               ! il faudra voir si c'est interessant aussi pour le miscanthus et la canne
               ! write(1111,*)'n',sc%n,'i',i,'ens',ens
       if ((p(i)%P_codelaitr == 1 .and. p(i)%qressuite_tot == 0) .or. (p(i)%P_codelaitr == 1 .and. p(i)%P_codeplante == 'fou')) then
                  ! RV: add a temporary deltai_tmp, dltams_tmp and pfeuilverte_tmp to use the true remaining deltai, dltams and pfeuilverte for AO.
                  ! I first used only p(i)%deltai(0,1:sc%nbjmax) * p(i)%surf(ens), but the senescence of a deltai can be computed on different day
                  ! for AS and AO, so the reciprocal surface of AO is not the same used previously when computing the senescence for AS, leading to
                  ! an underestimation of senescence for increasing AO surface over time.

                  if (ens == sc%AS) then
                     ! if(p(i)%lai(sc%AO,n)>0.0001) then
                     !   deltai_tmp(1:sc%nbjmax)= p(i)%deltai(0,1:sc%nbjmax) * p(i)%surf(ens)
                     !   dltams_tmp(1:sc%nbjmax)= p(i)%dltams(0,1:sc%nbjmax) * p(i)%surf(ens)
                     !   pfeuilverte_tmp(1:sc%nbjmax)= p(i)%pfeuilverte(0,1:sc%nbjmax) * p(i)%surf(ens)
                     ! else
                     ! If the AO component has no more LAI, take all deltai, dltams and pfeuilverte
                     deltai_tmp(1:sc%nbjmax) = p(i)%deltai(0, 1:sc%nbjmax)
                     dltams_tmp(1:sc%nbjmax) = p(i)%dltams(0, 1:sc%nbjmax)
                     pfeuilverte_tmp(1:sc%nbjmax) = p(i)%pfeuilverte(0, 1:sc%nbjmax)
                     ! endif
                  else
                     ! For the AO, use the deltai less what was already set as senescent during the AS computation
                     deltai_tmp(1:sc%nbjmax) = p(i)%deltai(0, 1:sc%nbjmax) - p(i)%deltai_sencour(sc%AS, 1:sc%nbjmax)
                     dltams_tmp(1:sc%nbjmax) = p(i)%dltams(0, 1:sc%nbjmax) - p(i)%dltams_sencour(sc%AS, 1:sc%nbjmax)
                     pfeuilverte_tmp(1:sc%nbjmax) = p(i)%pfeuilverte(0, 1:sc%nbjmax) - &
                                                    p(i)%pfeuilverte_sencour(sc%AS, 1:sc%nbjmax)
                  end if

                  call senescen(p(i)%nlev, n, sc%nbjmax, p(i)%lai(ens, n), pg%P_codeinnact, pg%P_codeh2oact, p(i)%senfac(ens), &
                                p(i)%innsenes(ens), p(i)%P_codlainet, p(i)%P_codeperenne, p(i)%nbfeuille, p(i)%P_nbfgellev, &
                                p(i)%P_codgellev, sc%tcultmin, p(i)%P_tletale, p(i)%P_tdebgel, p(i)%P_tgellev90, &
                                p(i)%P_tgellev10, i, p(i)%densitelev, densiteassoc, p(i)%P_codgeljuv, p(i)%P_tgeljuv90, &
                                p(i)%P_tgeljuv10, p(i)%P_codgelveg, p(i)%P_tgelveg90, p(i)%P_tgelveg10, p(i)%masecveg(ens), &
                                p(i)%nstopfeuille, p(i)%somcour, p(i)%resperenne(ens), p(i)%ndrp, p(i)%nrec, pg%P_QNpltminINN, &
                                sc%numcult, pg%P_codeinitprec, p(i)%ulai(ens, 1:sc%nbjmax), p(i)%P_vlaimax, p(i)%durvieI, &
                                p(i)%P_durvieF(itk(i)%P_variete), p(i)%inn(ens), p(i)%P_durviesupmax, p(i)%P_codestrphot, c%phoi, &
                                p(i)%P_phobasesen, dltams_tmp(1:sc%nbjmax), itk(i)%P_msresiduel(p(i)%numcoupe - 1), &
                                p(i)%P_ratiosen, p(i)%tdevelop(1:n), p(i)%somtemp, pfeuilverte_tmp(1:sc%nbjmax), &
                                deltai_tmp(1:sc%nbjmax), p(i)%P_lai0, &
                                sc%dernier_n, p(i)%nsencour(ens), p(i)%dltaisen(ens), p(i)%dltamsen(ens), p(i)%fstressgel, &
                                p(i)%fgellev, p(i)%gelee, p(i)%densite, p(i)%laisen(ens, n - 1:n), p(i)%nlan, &
                                p(i)%P_stsenlan(itk(i)%P_variete), p(i)%nsen, p(i)%P_stlaxsen(itk(i)%P_variete), p(i)%namf, &
                                p(i)%nlax, p(i)%P_stlevamf(itk(i)%P_variete), p(i)%P_stamflax(itk(i)%P_variete), p(i)%nrecbutoir, &
                                p(i)%mortplante, p(i)%nst2, p(i)%mortplanteN, p(i)%durvie(ens, 1:sc%nbjmax), &
                                p(i)%strphot(ens), p(i)%msres(ens), p(i)%dltamsres(ens), p(i)%ndebsen(ens), &
                                p(i)%somsenreste(ens), p(i)%msresjaune(ens), p(i)%mafeuiljaune(ens), p(i)%msneojaune(ens), &
                                !02/09/2014 DR et ML
                                ! QNplante_veille doit etre QNplante global de la plante : QNplante(0,n-1) et non QNplante(ens, n-1) sachant que c'est ensuite pond�r�
                                ! par les surfaces � l'ombre et au soleil et que ces surfaces evoluent d'un jour � l'autre , notamment si il y a inversion de dominance
                                !                            p(i)%dltamstombe(ens),p(i)%QNplante(ens,n-1),p(i)%P_dltamsminsen,p(i)%P_dltamsmaxsen,          &
                                p(i)%dltamstombe(ens), p(i)%QNplante(0, n - 1), p(i)%P_dltamsminsen, p(i)%P_dltamsmaxsen, &
                                p(i)%P_alphaphot, p(i)%strphotveille(ens), p(i)%deltai_sencour(ens, 1:sc%nbjmax), &
                                p(i)%dltams_sencour(ens, 1:sc%nbjmax), p(i)%pfeuilverte_sencour(ens, 1:sc%nbjmax))

                  ! EC 06/08/2012 Ajout du code ires des feuilles mortes
                  sc%ires = 2
                  ! print *,'avant apportfeuillemorte'
             call ApportFeuillesMortes(p(i)%fstressgel, p(i)%CNplante(ens), p(i)%P_abscission, p(i)%inn(ens), p(i)%P_parazofmorte, &
                                            p(i)%dltamsen(ens), t%P_codedyntalle(i), p(i)%mortmasec, p(i)%mortreserve, &
                                            p(i)%surf(ens), pg%P_CNresmin(sc%ires), pg%P_CNresmax(sc%ires), &
                                            sc%nbCouchesSol, pg%nbResidus, pg%P_Qmulchdec(sc%ires), &
                                            !02/09/2014 DR et ML
                                            ! idem au dessus ligne 157
                                            !                                        p(i)%dltamstombe(ens),p(i)%mafeuiltombe(ens),p(i)%QNplante(ens,n-1),p(i)%QNplantetombe, &
                                  p(i)%dltamstombe(ens), p(i)%mafeuiltombe(ens), p(i)%QNplante(0, n - 1), p(i)%QNplantetombe(ens), &
                                            itk(i)%nap, sc%airg(sc%n + 1), soil%itrav1, soil%itrav2, sc%ires, &
                                           sc%Cres(1:sc%nbCouchesSol, 1:pg%nbResidus), sc%Nres(1:sc%nbCouchesSol, 1:pg%nbResidus), &
                                            sc%Cnondec(1:10), sc%Nnondec(1:10), p(i)%resperenne(ens), p(i)%QCplantetombe(ens), &
                                            sc%Cmulchnd, sc%Nmulchnd, sc%QCapp, sc%QNapp, sc%QCresorg, sc%QNresorg, &
                                            pg%P_awb(sc%ires), pg%P_bwb(sc%ires), pg%P_cwb(sc%ires), pg%P_CroCo(sc%ires), &
                                            pg%P_akres(sc%ires), pg%P_bkres(sc%ires), pg%P_ahres(sc%ires), pg%P_bhres(sc%ires), &
                                            sc%Wb(sc%ires), sc%kres(sc%ires), sc%hres(sc%ires))

                  !TODO: on passe airg(n+1) pour ApportsResidus. Mais attention si n == 731, d�bordement de tableau !
                  !
                  ! 02/09/2014 DR et ML pourquoi on fait ca !!!!
                  ! on supprime les 2 lignes suivantes on n'a pas de raison de recalculer ce qui a ete fait le jour d'avant
                  !  p(i)%QNplante(sc%AOAS,sc%n-1) = p(i)%QNplante(sc%AO,sc%n-1) * p(i)%surf(sc%AO)    &
                  !                                + p(i)%QNplante(sc%AS,sc%n-1) * p(i)%surf(sc%AS)

               end if

               !: Affectation des sensibilit�s au gel des fleurs et des fruits
               if (p(i)%nflo > 0 .and. p(i)%nrec == 0) then
               p(i)%fgelflo = GEL(p(i)%P_codgelflo, sc%tcultmin, p(i)%P_tletale, p(i)%P_tdebgel, p(i)%P_tgelflo90, p(i)%P_tgelflo10)
                  if (p(i)%fgelflo < 1.) p(i)%gelee = .TRUE.
               end if
               if (p(i)%gelee .eqv. .TRUE.) p(i)%nbjgel = p(i)%nbjgel + 1
               if (p(i)%P_codeindetermin == 2) then

                  ! on sauvegarde la valeur de nfruit
                  p(i)%nfruitv(sc%AO, p(i)%P_nboite) = p(i)%nfruit(sc%AO, p(i)%P_nboite)
                  p(i)%nfruitv(sc%AS, p(i)%P_nboite) = p(i)%nfruit(sc%AS, p(i)%P_nboite)
                  ! print *,'avant fruit'
                  call fruit(n, p(i)%namf, p(i)%P_codcalinflo, p(i)%P_pentinflores, p(i)%densite, p(i)%P_resperenne0, &
                             p(i)%cumdltares(ens), p(i)%P_inflomax, p(i)%ndrp, itk(i)%P_nbcueille, p(i)%nrec, &
                             p(i)%P_nboite, p(i)%P_dureefruit(itk(i)%P_variete), p(i)%fpv(ens, n), p(i)%dltams(ens, n), &
                             p(i)%dltaremobil(ens), p(i)%remobilj(ens), p(i)%P_spfrmin, p(i)%P_spfrmax, &
                             p(i)%P_afruitpot(itk(i)%P_variete), p(i)%upvt(n), p(i)%fgelflo, p(i)%P_codazofruit, &
                             pg%P_codeinnact, p(i)%inns(ens), p(i)%nnou, itk(i)%P_codeclaircie, itk(i)%P_nb_eclair, &
                             p(i)%neclair(1:itk(i)%P_nb_eclair), itk(i)%P_nbinfloecl(1:itk(i)%P_nb_eclair), &
                             p(i)%P_codetremp, sc%tcultmin, sc%tcultmax, p(i)%P_tminremp, p(i)%P_tmaxremp, &
                             p(i)%nbrecolte, p(i)%rdtint(ens, 1:p(i)%nbrecolte), p(i)%P_allocfrmax, p(i)%nmat, &
                             p(i)%P_afpf, p(i)%P_bfpf, p(i)%P_cfpf, p(i)%P_dfpf, p(i)%pfmax, &
                             p(i)%P_nbinflo, p(i)%nfruit(ens, 1:p(i)%P_nboite), p(i)%pdsfruit(ens, 1:p(i)%P_nboite), & !INOUT
                             p(i)%fpft(ens), p(i)%sourcepuits(ens), &
                             p(i)%spfruit(ens), p(i)%nbfruit, p(i)%nfruitnou(ens), p(i)%nbfrote, &
                             sc%devjourfr, p(i)%cumdevfr, p(i)%pousfruit(ens), p(i)%ftempremp, p(i)%nbj0remp, &
                             p(i)%dltags(ens), p(i)%frplusp(ens), p(i)%ircarb(ens, n), p(i)%magrain(ens, n - 1:n), &
                             p(i)%allocfruit(ens), p(i)%masec(ens, n), p(i)%compretarddrp, p(i)%pdsfruittot(ens))

                  ! on met � jour le cumul des parties AO/AS de masec qui a pu �tre modifi� dans la routine <fruit>
                  ! TODO: si on passe pour chaque partie AO/AS on effectue le calcul 2 fois... y'a pas mieux � faire ?
                  p(i)%masec(sc%AOAS, sc%n) = p(i)%masec(sc%AS, sc%n)*p(i)%surf(sc%AS) &
                                              + p(i)%masec(sc%AO, sc%n)*p(i)%surf(sc%AO)

                  ! TODO: Attention, il y a un pb avec ce genre de cumul, car si on le fait plusieurs fois pour une m�me journ�e n
                  !       alors on cumule plusieurs fois la m�me valeur ce qui fausse les r�sultats.
                  !       Pour l'instant, je d�sactive le calcul dans cumAOetAS.f90
                  p(i)%nfruit(sc%AOAS, 1:p(i)%P_nboite) = p(i)%nfruit(sc%AOAS, 1:p(i)%P_nboite) &
                                   + (p(i)%nfruit(sc%AO, 1:p(i)%P_nboite) - p(i)%nfruitv(sc%AO, 1:p(i)%P_nboite))*p(i)%surf(sc%AO) &
                                     + (p(i)%nfruit(sc%AS, 1:p(i)%P_nboite) - p(i)%nfruitv(sc%AS, 1:p(i)%P_nboite))*p(i)%surf(sc%AS)

                  p(i)%pdsfruit(ens, 1:p(i)%P_nboite) = p(i)%pdsfruit(sc%AO, 1:p(i)%P_nboite)*p(i)%surf(sc%AO) &
                                                        + p(i)%pdsfruit(sc%AS, 1:p(i)%P_nboite)*p(i)%surf(sc%AS)

                  p(i)%fpft(sc%AOAS) = p(i)%fpft(sc%AS)*p(i)%surf(sc%AS) &
                                       + p(i)%fpft(sc%AO)*p(i)%surf(sc%AO)

                  p(i)%sourcepuits(sc%AOAS) = p(i)%sourcepuits(sc%AO)*p(i)%surf(sc%AO) &
                                              + p(i)%sourcepuits(sc%AS)*p(i)%surf(sc%AS)

                  p(i)%spfruit(sc%AOAS) = p(i)%spfruit(sc%AO)*p(i)%surf(sc%AO) &
                                          + p(i)%spfruit(sc%AS)*p(i)%surf(sc%AS)

                  p(i)%nfruitnou(sc%AOAS) = p(i)%nfruitnou(sc%AO)*p(i)%surf(sc%AO) &
                                            + p(i)%nfruitnou(sc%AS)*p(i)%surf(sc%AS)

                  p(i)%pousfruit(sc%AOAS) = p(i)%pousfruit(sc%AO)*p(i)%surf(sc%AO) &
                                            + p(i)%pousfruit(sc%AS)*p(i)%surf(sc%AS)

                  p(i)%dltags(sc%AOAS) = p(i)%dltags(sc%AS)*p(i)%surf(sc%AS) &
                                         + p(i)%dltags(sc%AO)*p(i)%surf(sc%AO)

                  ! RV: compute ircarb(aoas) as any other variable:
                  p(i)%ircarb(sc%AOAS, n) = p(i)%ircarb(sc%AS, n)*p(i)%surf(sc%AS) &
                                            + p(i)%ircarb(sc%AO, n)*p(i)%surf(sc%AO)

                  p(i)%magrain(sc%AOAS, n) = p(i)%magrain(sc%AO, n)*p(i)%surf(sc%AO) &
                                             + p(i)%magrain(sc%AS, n)*p(i)%surf(sc%AS)

                  p(i)%allocfruit(sc%AOAS) = p(i)%allocfruit(sc%AO)*p(i)%surf(sc%AO) &
                                             + p(i)%allocfruit(sc%AS)*p(i)%surf(sc%AS)

               else
                  if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg2, *) 'ncroi', n, i, ens, p(i)%nbgrains(0) &
                     , p(i)%magrain(0, n), p(i)%chargefruit
                  !  print *,'avant grain'
                  ! print *, "plante",i,"ens",ens
                  call grain(n, p(i)%ndrp, p(i)%nrec, p(i)%nlev, p(i)%nrecbutoir, p(i)%P_nbjgrain, &
                             p(i)%dltams(ens, 1:p(i)%ndrp), p(i)%P_cgrain, p(i)%P_cgrainv0, p(i)%P_nbgrmin, &
                             p(i)%P_nbgrmax(itk(i)%P_variete), p(i)%P_codazofruit, pg%P_codeinnact, p(i)%inns(ens), p(i)%fgelflo, &
                             p(i)%P_codeir, p(i)%P_vitircarb, p(i)%P_irmax, p(i)%P_vitircarbT, p(i)%somcourdrp, p(i)%nmat, &
                             p(i)%masec(ens, n - 1:n), p(i)%P_codetremp, sc%tcultmin, sc%tcultmax, p(i)%P_tminremp, &
                             p(i)%P_tmaxremp, p(i)%P_pgrainmaxi(itk(i)%P_variete), &
                             p(i)%ircarb(ens, (n - 1):n), p(i)%nbgrains(ens), p(i)%pgrain(ens), p(i)%CNgrain(ens), &
                             p(i)%vitmoy(ens), p(i)%nbgraingel, p(i)%pgraingel(ens), p(i)%dltags(ens), p(i)%ftempremp, &
                             p(i)%magrain(ens, n - 1:n), p(i)%nbj0remp, p(i)%pdsfruittot(ens))

                  if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg2, *) 'ncroi', n, i, ens, p(i)%nbgrains(0), &
                     p(i)%magrain(0, n), p(i)%chargefruit
                  !  print *,'apres grain'

                  !DR 25/09/2012 tous ces cumuls sont � exterioris�s pourquoi sont t'ils la ???

                  p(i)%dltags(sc%AOAS) = p(i)%dltags(sc%AS)*p(i)%surf(sc%AS) &
                                         + p(i)%dltags(sc%AO)*p(i)%surf(sc%AO)

                  ! RV: compute ircarb(aoas) as any other variable:
                  p(i)%ircarb(sc%aoas, n) = p(i)%ircarb(sc%AS, n)*p(i)%surf(sc%AS) &
                                            + p(i)%ircarb(sc%AO, n)*p(i)%surf(sc%AO)
                  ! DR 25/09/2012 vraimenet bizarre ce truc on cumulait 2 fois le nbgrains pour les cas
                  !                p(i)%nbgrains(sc%AOAS) = p(i)%nbgrains(sc%AOAS)                                                   &
                  !                                         + (p(i)%nbgrains(sc%AO) - p(i)%nbgrainsv(sc%AO)) * p(i)%surf(sc%AO)      &
                  !                                         + (p(i)%nbgrains(sc%AS) - p(i)%nbgrainsv(sc%AS)) * p(i)%surf(sc%AS)
                  p(i)%nbgrains(sc%AOAS) = (p(i)%nbgrains(sc%AO)*p(i)%surf(sc%AO)) &
                                           + (p(i)%nbgrains(sc%AS)*p(i)%surf(sc%AS))

                  p(i)%CNgrain(sc%AOAS) = p(i)%CNgrain(sc%AO)*p(i)%surf(sc%AO) &
                                          + p(i)%CNgrain(sc%AS)*p(i)%surf(sc%AS)

                  p(i)%vitmoy(sc%AOAS) = p(i)%vitmoy(sc%AO)*p(i)%surf(sc%AO) + &
                                         p(i)%vitmoy(sc%AS)*p(i)%surf(sc%AS)

                  p(i)%magrain(0, n) = p(i)%magrain(0, n - 1) &
                                       + (p(i)%magrain(sc%AO, n) - p(i)%magrain(sc%AO, n - 1))*p(i)%surf(sc%AO) &
                                       + (p(i)%magrain(sc%AS, n) - p(i)%magrain(sc%AS, n - 1))*p(i)%surf(sc%AS)

                  p(i)%pgrain(sc%aoas) = p(i)%pgrain(sc%AS)*p(i)%surf(sc%AS) &
                                         + p(i)%pgrain(sc%AO)*p(i)%surf(sc%AO)
                  if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg2, *) 'ncroi', n, i, ens, p(i)%magrain(0, n) &
                     , p(i)%chargefruit

               end if
               !  print *,'avant eauqual'
               call eauqual(n, p(i)%P_deshydbase, p(i)%P_tempdeshyd, sc%tcult, sc%tairveille, p(i)%ndrp, &
                            p(i)%nrec, p(i)%P_codeindetermin, p(i)%P_nboite, p(i)%P_stdrpdes(itk(i)%P_variete), &
                            p(i)%P_dureefruit(itk(i)%P_variete), p(i)%nfruit(ens, 1:p(i)%P_nboite), p(i)%pousfruit(ens), &
                            p(i)%pdsfruit(ens, 1:p(i)%P_nboite), p(i)%P_h2ofrvert, p(i)%frplusp(ens), &
                            p(i)%P_vitpropsucre, p(i)%P_vitprophuile, p(i)%magrain(ens, n), p(i)%somcourdrp, p(i)%nmat, &
                            p(i)%maenfruit(ens), p(i)%nlev, p(i)%masec(ens, n), p(i)%mafeuilverte(ens), &
                            p(i)%P_h2ofeuilverte, p(i)%mafeuiljaune(ens), p(i)%P_h2ofeuiljaune, p(i)%matigestruc(ens), &
                            p(i)%P_h2otigestruc, p(i)%resperenne(ens), p(i)%P_h2oreserve, &
                            p(i)%deshyd(ens, 0:p(i)%P_nboite), p(i)%eaufruit(ens, 0:p(i)%P_nboite), p(i)%ndebdes, &
                            p(i)%teaugrain(ens), p(i)%h2orec(ens), p(i)%sucre(ens), p(i)%huile(ens), &
                            p(i)%sucreder(ens), p(i)%huileder(ens), p(i)%sucrems(ens), p(i)%huilems(ens), &
                            p(i)%pdsfruitfrais(ens), p(i)%mafraisrec(ens), p(i)%CNgrain(ens), &
                            p(i)%mafraisfeuille(ens), p(i)%mafraistige(ens), p(i)%mafraisres(ens), p(i)%mafrais(ens))
               !  print *,'apres eauqual'

               p(i)%CNgrain(sc%AOAS) = p(i)%CNgrain(sc%AO)*p(i)%surf(sc%AO) &
                                       + p(i)%CNgrain(sc%AS)*p(i)%surf(sc%AS)

               p(i)%h2orec(sc%AOAS) = p(i)%h2orec(sc%AO)*p(i)%surf(sc%AO) &
                                      + p(i)%h2orec(sc%AS)*p(i)%surf(sc%AS)

               p(i)%sucre(sc%AOAS) = p(i)%sucre(sc%AO)*p(i)%surf(sc%AO) &
                                     + p(i)%sucre(sc%AS)*p(i)%surf(sc%AS)

               p(i)%huile(sc%AOAS) = p(i)%huile(sc%AO)*p(i)%surf(sc%AO) &
                                     + p(i)%huile(sc%AS)*p(i)%surf(sc%AS)

               p(i)%pdsfruitfrais(sc%AOAS) = p(i)%pdsfruitfrais(sc%AO)*p(i)%surf(sc%AO) &
                                             + p(i)%pdsfruitfrais(sc%AS)*p(i)%surf(sc%AS)
               p(i)%mafraisrec(sc%AOAS) = p(i)%mafraisrec(sc%AS)*p(i)%surf(sc%AS) &
                                          + p(i)%mafraisrec(sc%AO)*p(i)%surf(sc%AO)

               p(i)%mafraisfeuille(sc%AOAS) = p(i)%mafraisfeuille(sc%AS)*p(i)%surf(sc%AS) &
                                              + p(i)%mafraisfeuille(sc%AO)*p(i)%surf(sc%AO)

               p(i)%mafraistige(sc%AOAS) = p(i)%mafraistige(sc%AS)*p(i)%surf(sc%AS) &
                                           + p(i)%mafraistige(sc%AO)*p(i)%surf(sc%AO)

               p(i)%mafraisres(sc%AOAS) = p(i)%mafraisres(sc%AS)*p(i)%surf(sc%AS) &
                                          + p(i)%mafraisres(sc%AO)*p(i)%surf(sc%AO)

               p(i)%mafrais(sc%AOAS) = p(i)%mafrais(sc%AO)*p(i)%surf(sc%AO) &
                                       + p(i)%mafrais(sc%AS)*p(i)%surf(sc%AS)

               ! � d�faut de mieux
               p(i)%sucreder(sc%AOAS) = p(i)%sucreder(sc%AS) ! p(i)%sucre(sc%AO) * p(i)%surf(sc%AO) + p(i)%sucre(sc%AS) * p(i)%surf(sc%AS)
               p(i)%huileder(sc%AOAS) = p(i)%huileder(sc%AS)
               p(i)%sucrems(sc%AOAS) = p(i)%sucrems(sc%AS)
               p(i)%huilems(sc%AOAS) = p(i)%huilems(sc%AS)

               ! TODO: il manque des cumuls ao/as

               !#if DEBUG == 1
               !              if (iand(sc%eauqual,8) >0) call eauqual_debug_write_output(1258,sc,p,itk,i,ens)
               !#endif

            end if ! fin P_codeplante /= snu

            if (ens == sc%AS) then

               if (p(i)%P_codtrophrac == 1) then
                  repracmax = p(i)%P_repracpermax
                  repracmin = p(i)%P_repracpermin
                  kreprac = p(i)%P_krepracperm
               end if
               if (p(i)%P_codtrophrac == 2) then
                  repracmax = p(i)%P_repracseumax
                  repracmin = p(i)%P_repracseumin
                  kreprac = p(i)%P_krepracseu
               end if
               if (p(i)%P_codtrophrac == 3) then
                  repracmax = 0.
                  repracmin = 0.
                  kreprac = 0.
               end if

               ! Calcul d'anoxmoy
               profhoriz = sc%nhe
               p(i)%anoxmoy = 0.
               do ic = sc%nh, 1, -1
                  do ii = 1, int(soil%P_epc(ic))
                     iz = profhoriz - ii + 1
                     if (iz <= int(p(i)%zrac)) p(i)%anoxmoy = p(i)%anoxmoy + sc%anox(iz)
                  end do
                  profhoriz = profhoriz - int(soil%P_epc(ic))
               end do
               p(i)%anoxmoy = p(i)%anoxmoy/max(p(i)%zrac, 1.)

               ! Calcul de la r�serve maximale utilis�e
               if (sc%n > p(i)%nplt .and. p(i)%nrec == 0) then
                  p(i)%rmaxi = 0.

                  ! 17/06/04 - les3
                  !do 188 iz = 1,int(zrac)+1
                  do iz = int(itk(i)%P_profsem), int(p(i)%zrac) + 1
                     if (sc%hur(iz) < sc%hurmini(iz)) then
                        sc%hurmini(iz) = sc%hur(iz)
                     end if
                     p(i)%rmaxi = p(i)%rmaxi + sc%hucc(iz) - sc%hurmini(iz)
                  end do
               end if

               !: FIN EXTRAIT LIXIV

               !#if DEBUG == 1
               !              if (iand(sc%croissanceFrontRacinaire,2) >0) &
               !                call croissanceFrontRacinaire_debug_write_input(2251,sc,pg,p,itk,soil,i)
               !#endif
               !  print *,'avant croissanceFrontRacinaire'
               call croissanceFrontRacinaire( &
                  n, sc%nh, sc%nbCouchesSol, sc%hur(1:sc%nbCouchesSol), sc%hucc(1:sc%nbCouchesSol), & ! IN
                  sc%tcult, sc%tsol(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), &
                  sc%dacouche(1:sc%nbCouchesSol), sc%P_codesimul, soil%P_epc(1:sc%NH), soil%P_obstarac, &
                  pg%P_daseuilbas, pg%P_daseuilhaut, pg%P_dacohes, p(i)%P_zrac0, p(i)%P_densinitial(1:sc%NH), &
                  p(i)%P_coderacine, p(i)%P_codeplante, p(i)%P_codeperenne, p(i)%P_codehypo, &
                  p(i)%P_codegermin, p(i)%P_zracplantule, p(i)%P_stoprac, p(i)%P_codetemprac, &
                  p(i)%P_tcmin, p(i)%P_tcmax, p(i)%P_tgmin, p(i)%P_croirac(itk(i)%P_variete), &
                  p(i)%P_contrdamax, p(i)%P_codeindetermin, p(i)%P_sensanox, p(i)%nplt, p(i)%nrec, &
                  p(i)%codeinstal, p(i)%nger, p(i)%nsen, p(i)%nlax, p(i)%nflo, p(i)%nmat, &
                  p(i)%nlev, p(i)%namf, p(i)%izrac, itk(i)%P_profsem, itk(i)%P_codcueille, &
                  p(i)%deltai(0, n), p(i)%lai(0, n), p(i)%sioncoupe, p(i)%P_sensrsec, t%P_codedyntalle(i), &
                  p(i)%P_tcxstop, p(i)%dltams(0, sc%n), &
                  sc%nhe, sc%nstoprac, p(i)%zrac, p(i)%znonli, & ! INOUT
                  p(i)%rl(p(i)%nger - 1, 1:sc%nbCouchesSol), p(i)%deltaz, p(i)%dtj(1:n), &
                  p(i)%cumlracz, p(i)%poussracmoy, p(i)%lracsenz(1:sc%nbCouchesSol), p(i)%tempeff, p(i)%efda, p(i)%humirac_mean)

               !     write(526,*)'apres croissance front racinaire',n,p(i)%efda,p(i)%humirac_mean
               !#if DEBUG == 1
               !              if (iand(sc%croissanceFrontRacinaire,8) >0) &
               !                call croissanceFrontRacinaire_debug_write_output(2253,sc,p,i)
               !#endif
               if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'avant densite racinaire'
               ! print *,'avant densite racinaire',sc%nbCouchesSol
               call densiteRacinaire( &
                  sc%n, sc%nbCouchesSol, sc%anox(1:sc%nbCouchesSol), sc%hur(1:sc%nbCouchesSol), sc%nstoprac, &
                  sc%humin(1:sc%nbCouchesSol), sc%dacouche(1:sc%nbCouchesSol), pg%P_daseuilbas, pg%P_daseuilhaut, &
                  pg%P_dacohes, pg%P_lvopt, p(i)%P_coderacine, p(i)%P_contrdamax, p(i)%P_sensanox, p(i)%zrac, &
                  p(i)%P_zlabour, p(i)%P_zpente, p(i)%P_zprlim, p(i)%znonli, p(i)%difrac, p(i)%lai(0, n), &
                  sc%tauxcouv(n), p(i)%nrec, p(i)%codeinstal, p(i)%nger, p(i)%rl(n, 1:sc%nbCouchesSol), &
                  p(i)%deltaz, p(i)%dtj(1:n), p(i)%nlev, p(i)%cumlracz, p(i)%poussracmoy, &
                  p(i)%lracsenz(1:sc%nbCouchesSol), p(i)%cumflrac, p(i)%racnoy(n), &
                  p(i)%flrac(1:sc%nbCouchesSol), p(i)%lracz(1:sc%nbCouchesSol), p(i)%cumlraczmaxi, &
                  p(i)%zracmax, itk(i)%P_profsem, p(i)%P_codazorac, p(i)%P_minazorac, p(i)%P_maxazorac, &
                  p(i)%P_minefnra, p(i)%P_codtrophrac, t%P_coefracoupe(i), soil%nit(1:sc%nbCouchesSol), &
                  soil%amm(1:sc%nbCouchesSol), soil%profsol, p(i)%msrac(n), p(i)%nsencourprerac, &
                  p(i)%somtemprac, p(i)%idzrac, p(i)%efdensite_rac, p(i)%ndebsenrac, &
                  p(i)%precrac(1:sc%nbCouchesSol), p(i)%drl(1:n, 1:sc%nbCouchesSol), p(i)%lracsentot, &
                  p(i)%masectot, p(i)%rltot, p(i)%sioncoupe, p(i)%P_sensrsec, &
                  p(i)%P_stlevamf(itk(i)%P_variete), p(i)%P_stamflax(itk(i)%P_variete), p(i)%P_lvfront, &
                  p(i)%P_laicomp, p(i)%P_adens(itk(i)%P_variete), p(i)%P_bdens, p(i)%P_draclong, p(i)%P_vlaimax, &
                  p(i)%P_longsperac, p(i)%P_debsenrac, t%P_codedyntalle(i), p(i)%drlsenmortalle, p(i)%densite, &
                  itk(i)%P_msresiduel(p(i)%numcoupe), sc%lai_tot_prev, p(i)%msrac(n - 1), &
                  p(i)%rl(n - 1, 1:sc%nbCouchesSol), p(i)%dltams(0, n), repracmin, repracmax, kreprac, &
                  p(i)%efda, p(i)%efnrac_mean, p(i)%humirac_mean, p(i)%humirac_z(1:sc%nbCouchesSol), &
                  p(i)%efnrac_z(1:sc%nbCouchesSol), p(i)%rlj, p(i)%masec(sc%AOAS, n), t%P_codemortalracine, &
                  p(i)%dltmsrac_plante, p(i)%densiteequiv)
               ! print *,'apres densite racinaire'
               !#if DEBUG == 1
               !              if (iand(sc%calpsibase,2) >0) call calpsibase_debug_write_input(1281,sc,pg,p,itk,soil,c,sta,t,i,ens)
               !#endif
               ! print *,'avant calpsibase'
               ! NB - le 07/06/2004 - introduction du calcul de psibase
               call calpsibase(p(i)%zrac, sc%nbCouchesSol, sc%hur(1:sc%nbCouchesSol), &
                               sc%sat(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), &
                               sc%hucc(1:sc%nbCouchesSol), sc%dacouche(1:sc%nbCouchesSol), &
                               pg%P_psihumin, p(i)%lracz(1:sc%nbCouchesSol), &
                               p(i)%lai(sc%AOAS, n), pg%P_psihucc, soil%P_codefente, p(i)%psibase)
               !#if DEBUG == 1
               !              if (iand(sc%calpsibase,8) >0) call calpsibase_debug_write_output(1283,sc,pg,p,itk,soil,c,sta,t,i,ens)
               !#endif

               ! print *,'apres calpsibase'
            end if

            if (p(i)%P_codelaitr == 1 .and. p(i)%nlev > 0) then

               !#if DEBUG == 1
               !              if (iand(sc%repartir,2) >0) call repartir_debug_write_input(1291,sc,pg,p,itk,soil,c,sta,t,i,ens)
               !#endif

               ! print*,'avant repartir'
               call repartir(n, p(i)%nrec, itk(i)%P_codcueille, p(i)%P_codeperenne, p(i)%nlev, p(i)%nlax, itk(i)%P_nbcueille, &
                             sc%numcult, sc%tustress, p(i)%P_slamin, p(i)%P_slamax, p(i)%P_codlainet, p(i)%P_codemonocot, &
                             sc%P_codesimul, p(i)%dltaisen(ens), p(i)%P_tigefeuil, p(i)%P_envfruit, p(i)%chargefruit, &
                             p(i)%ndrp, p(i)%ndebdes, p(i)%P_sea, p(i)%ntaille, itk(i)%P_codetaille, pg%P_codeinitprec, &
                             p(i)%dltams(ens, n), p(i)%lai(ens, n - 1), &
                             p(i)%resperenne(ens), p(i)%masecveg(ens), p(i)%pdsfruittot(ens), p(i)%tursla(ens), &
                             p(i)%sla(ens), p(i)%mafeuilverte(ens), p(i)%mafeuil(ens), p(i)%mafeuilp(ens), &
                             p(i)%lai(ens, n), p(i)%deltai(ens, n), p(i)%maenfruit(ens), p(i)%eai(ens), &
                             p(i)%mareserve(ens), p(i)%deltares(ens), p(i)%mabois(ens), p(i)%P_resperenne0, &
                             p(i)%masec(ens, n), p(i)%msresjaune(ens), p(i)%mafeuiljaune(ens), p(i)%msneojaune(ens), &
                             p(i)%matigestruc(ens), p(i)%pfeuil(ens, n), p(i)%pfeuilverte(ens, n), p(i)%pfeuiljaune(ens), &
                             p(i)%ptigestruc(ens), p(i)%penfruit(ens), p(i)%preserve(ens), itk(i)%P_codefauche)

               ! on recalcul les sommes pond�r�es par les surfaces ombre/soleil des variables AO/AS
               p(i)%lai(sc%AOAS, n) = p(i)%lai(sc%AS, n)*p(i)%surf(sc%AS) &
                                      + p(i)%lai(sc%AO, n)*p(i)%surf(sc%AO)
               p(i)%resperenne(sc%AOAS) = p(i)%resperenne(sc%AO)*p(i)%surf(sc%AO) &
                                          + p(i)%resperenne(sc%AS)*p(i)%surf(sc%AS)
               p(i)%masecveg(sc%AOAS) = p(i)%masecveg(sc%AO)*p(i)%surf(sc%AO) &
                                        + p(i)%masecveg(sc%AS)*p(i)%surf(sc%AS)
               p(i)%pdsfruittot(sc%AOAS) = p(i)%pdsfruittot(sc%AO)*p(i)%surf(sc%AO) &
                                           + p(i)%pdsfruittot(sc%AS)*p(i)%surf(sc%AS)
               p(i)%tursla(sc%AOAS) = p(i)%tursla(sc%AO)*p(i)%surf(sc%AO) &
                                      + p(i)%tursla(sc%AS)*p(i)%surf(sc%AS)
               p(i)%sla(sc%AOAS) = p(i)%sla(sc%AO)*p(i)%surf(sc%AO) &
                                   + p(i)%sla(sc%AS)*p(i)%surf(sc%AS)
               p(i)%mafeuilverte(sc%AOAS) = p(i)%mafeuilverte(sc%AO)*p(i)%surf(sc%AO) &
                                            + p(i)%mafeuilverte(sc%AS)*p(i)%surf(sc%AS)
               p(i)%mafeuil(sc%AOAS) = p(i)%mafeuil(sc%AO)*p(i)%surf(sc%AO) &
                                       + p(i)%mafeuil(sc%AS)*p(i)%surf(sc%AS)
               p(i)%mafeuilp(sc%AOAS) = p(i)%mafeuilp(sc%AO)*p(i)%surf(sc%AO) &
                                        + p(i)%mafeuilp(sc%AS)*p(i)%surf(sc%AS)
               p(i)%deltai(sc%AOAS, sc%n) = p(i)%deltai(sc%AO, sc%n)*p(i)%surf(sc%AO) &
                                            + p(i)%deltai(sc%AS, sc%n)*p(i)%surf(sc%AS)
               p(i)%maenfruit(sc%AOAS) = p(i)%maenfruit(sc%AO)*p(i)%surf(sc%AO) &
                                         + p(i)%maenfruit(sc%AS)*p(i)%surf(sc%AS)
               p(i)%eai(sc%AOAS) = p(i)%eai(sc%AO)*p(i)%surf(sc%AO) &
                                   + p(i)%eai(sc%AS)*p(i)%surf(sc%AS)
               p(i)%mareserve(sc%AOAS) = p(i)%mareserve(sc%AO)*p(i)%surf(sc%AO) &
                                         + p(i)%mareserve(sc%AS)*p(i)%surf(sc%AS)
               p(i)%deltares(sc%AOAS) = p(i)%deltares(sc%AO)*p(i)%surf(sc%AO) &
                                        + p(i)%deltares(sc%AS)*p(i)%surf(sc%AS)
               p(i)%mabois(sc%AOAS) = p(i)%mabois(sc%AO)*p(i)%surf(sc%AO) &
                                      + p(i)%mabois(sc%AS)*p(i)%surf(sc%AS)
               p(i)%masec(sc%AOAS, sc%n) = p(i)%masec(sc%AO, sc%n)*p(i)%surf(sc%AO) &
                                           + p(i)%masec(sc%AS, sc%n)*p(i)%surf(sc%AS)
               p(i)%msresjaune(sc%AOAS) = p(i)%msresjaune(sc%AO)*p(i)%surf(sc%AO) &
                                          + p(i)%msresjaune(sc%AS)*p(i)%surf(sc%AS)
               p(i)%mafeuiljaune(sc%AOAS) = p(i)%mafeuiljaune(sc%AO)*p(i)%surf(sc%AO) &
                                            + p(i)%mafeuiljaune(sc%AS)*p(i)%surf(sc%AS)
               ! DR et Fr 15/04/2016 on a enleve les cumuls dans cimAOAS avce les deltas (jour - veille) et on les fait la
               p(i)%msneojaune(sc%AOAS) = p(i)%msneojaune(sc%AO)*p(i)%surf(sc%AO) &
                                          + p(i)%msneojaune(sc%AS)*p(i)%surf(sc%AS)
               p(i)%masecneo(sc%AOAS) = p(i)%masecneo(sc%AO)*p(i)%surf(sc%AO) &
                                        + p(i)%masecneo(sc%AS)*p(i)%surf(sc%AS)
               p(i)%msres(sc%AOAS) = p(i)%msres(sc%AO)*p(i)%surf(sc%AO) &
                                     + p(i)%msres(sc%AS)*p(i)%surf(sc%AS)
               p(i)%mafeuiltombe(sc%AOAS) = p(i)%mafeuiltombe(sc%AO)*p(i)%surf(sc%AO) &
                                            + p(i)%mafeuiltombe(sc%AS)*p(i)%surf(sc%AS)
               !

               p(i)%matigestruc(sc%AOAS) = p(i)%matigestruc(sc%AO)*p(i)%surf(sc%AO) &
                                           + p(i)%matigestruc(sc%AS)*p(i)%surf(sc%AS)

               if (p(i)%masec(sc%AOAS, sc%n) > 0.) then
                  p(i)%ptigestruc(sc%AOAS) = p(i)%matigestruc(sc%AOAS)/p(i)%masec(sc%AOAS, sc%n)
                  p(i)%penfruit(sc%AOAS) = p(i)%maenfruit(sc%AOAS)/p(i)%masec(sc%AOAS, sc%n)
                  p(i)%preserve(sc%AOAS) = p(i)%resperenne(sc%AOAS)/p(i)%masec(sc%AOAS, sc%n)
                  p(i)%pfeuil(sc%AOAS, sc%n) = p(i)%mafeuil(sc%AOAS)/p(i)%masec(sc%AOAS, sc%n)
                  p(i)%pfeuiljaune(sc%AOAS) = p(i)%mafeuiljaune(sc%AOAS)/p(i)%masec(sc%AOAS, sc%n)
                  p(i)%pfeuilverte(sc%AOAS, sc%n) = p(i)%mafeuilverte(sc%AOAS)/p(i)%masec(sc%AOAS, sc%n)
               else
                  p(i)%ptigestruc(sc%AOAS) = 0.
                  p(i)%penfruit(sc%AOAS) = 0.
                  p(i)%preserve(sc%AOAS) = 0.
                  p(i)%pfeuil(sc%AOAS, sc%n) = 0.
                  p(i)%pfeuiljaune(sc%AOAS) = 0.
                  p(i)%pfeuilverte(sc%AOAS, sc%n) = 0.
               end if

               !#if DEBUG == 1
               !              if (iand(sc%repartir,8) >0) call repartir_debug_write_output(1293,sc,pg,p,itk,soil,c,sta,t,i,ens)
               !#endif

            end if
         end if
      end do
   end do

   return
end subroutine croissance
