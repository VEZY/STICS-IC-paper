! daily loop
! The STICS model is organised into modules (fig. 1.1, page 18),
! each module is composed of sub-modules dealing with specific mechanisms.
! A first set of 3 modules deals with ecophysiology of above-ground plant parts (phenology, shoot growth, yield formation).
! A second set of 4 modules deals with soil functions interacting with roots (root growth, water balance, nitrogen balance, soil transfers)
! The crop management module deals with interactions between the applied techniques and the soil-crop system.
! The microclimate module simulates the combined effects of climate and water balance on the temperature and air humidity within the canopy.
!!
! Each module includes options that can be used to extend the scope with which STICS can be applied to various crop systems.  These options relate to ecophysiology and to crop management, for example:
! -   competition for assimilate between vegetative organs and reserve organs (hereafter referred to as trophic competition);
! -   considering the geometry of the canopy when simulating radiation interception;
! -   the description of the root density profile;
! -   using a resistive approach to estimate the evaporative demand by plants;
! -   the mowing of forage crops;
! -   plant or plastic mulching under vegetation
!!
! some options depend on data availability.  For example, the use of a resistive model is based on availability of additional climatic driving variables: wind and air humidity.
subroutine Stics_Jour(sc, pg, p, itk, soil, c, sta, t)

   USE Stics
   USE Plante
   USE Itineraire_Technique
   USE Sol
   USE Climat
   USE Station
   USE Parametres_Generaux
   USE Divers
! ML je desactive pour compiler

   implicit none

   type(Stics_Communs_), intent(INOUT) :: sc

   type(Parametres_Generaux_), intent(IN)    :: pg

   type(Plante_), intent(INOUT) :: p(sc%P_nbplantes)

   type(ITK_), intent(INOUT) :: itk(sc%P_nbplantes)

   type(Sol_), intent(INOUT) :: soil

   type(Climat_), intent(INOUT) :: c

   type(Station_), intent(INOUT) :: sta

   type(Stics_Transit_), intent(INOUT) :: t
! ML je desactive pour compiler
! 17/10/2013 � garder si on doit faire un module � part pour les varaibles issus de stics
!  type(Patho_inputs_Stics_),   intent(IN) :: pis

!! variables locales
   integer :: n
   integer :: i  !
   integer ::  iz  !
   integer ::  ipl  !
   integer ::  ens
   real    :: epzTot(sc%nbCouchesSol)      ! est-ce que la taille de epzTot peut varier au cours du temps (tassement/detassement) ?
   real    :: zracMAXI
   real    :: tmaxveille
   real    :: troseh(24)
   real    :: tculth(24)
   real    :: humh(24)
   integer :: irecolte
   real    ::  cum_absz, cum_nitz
   real    :: toto
   real    :: magrain_abso  ! magrain that absorbs N.
   real    :: masecNmax_mean ! Total P_masecNmax: = P_masecNmax for sole crop, sum of P_masecNmax of both plants when intercropping.

   n = sc%n
!               write(200,*)'debut jour',sc%n,soil%nit(1:60)
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) write (*, *) 'n', n

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: climabri'
!#endif
   ! print*,'avant climabri',n
   !::: climabri
   if (itk(1)%P_codabri == 2) then
      if (n > 1) then
         call ClimatSousAbri(sta%P_aks, sta%P_bks, c%tvent(n), n, sc%nouvre2, sc%nouvre3, itk%P_surfouvre1, &
                             itk%P_surfouvre2, itk%P_surfouvre3, sta%P_cvent, sta%P_phiv0, &
                             sc%et, c%tetp(n - 1), c%tetp(n), sta%P_coefrnet, c%trgext(n), c%tmoy(n), &
                             c%tmoyext(n), c%tmin(n), c%tminext(n), c%tmax(n))
      else
         call ClimatSousAbri(sta%P_aks, sta%P_bks, c%tvent(n), n, sc%nouvre2, sc%nouvre3, itk%P_surfouvre1, &
                             itk%P_surfouvre2, itk%P_surfouvre3, sta%P_cvent, sta%P_phiv0, &
                             sc%et, 0, c%tetp(n), sta%P_coefrnet, c%trgext(n), c%tmoy(n), &
                             c%tmoyext(n), c%tmin(n), c%tminext(n), c%tmax(n))
      end if
   end if
! write(*,*)'apres climabri',n

!::: Fonctions Physiologiques (call physio => dominver, laidev, croissance)

   !: Si la hauteur de la plante domin�e d�passe celle de la plante dominante
   !- alors il doit y avoir INVERsion de DOMINance
   if (sc%P_nbplantes > 1) then
      if (p(2)%hauteur(0) > p(1)%hauteur(0)) then
         if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'inversion de dominance'

         p(1:2) = p(2:1:-1) ! on inverse les 2 plantes dans le tableau plante avec une manipulation d'indices du tableau.
         ! DR 12/02/2015 on teste l'inversion des parametres techniques
         itk(1:2) = itk(2:1:-1) ! on inverse les 2 plantes dans les tableau management avec une manipulation d'indices du tableau.

         ! 27/07/2016 DR Dans le cas ou on a deja recolte une plante , l'autre devient dominante et a toute sa surface au soleil donc a prendre en compte et mettre le lai(0) dans le lai(as)
         if (p(2)%nrec .gt. 0) then
            p(1)%lai(0, n - 1) = p(1)%lai(0, n - 1)
            p(1)%lai(sc%as, n - 1) = p(1)%lai(0, n - 1)
            p(1)%lai(sc%ao, n - 1) = 0.
         else
            ! DR et ML 29/08/2014
            !  lorsqu'il y a inversion de dominance, afin d'eviter que le lai de la plante domin�e qui passe � l'ombre ne soit nul ,
            !  on lui affecte la valeur du lai au soleil de la veille , sachant qu'il sera ensuite pond�r� par la surface � l'ombre
            ! (dans croissance.f90 apres l'appel � la fonction repartir)
            !write(*,*)n,p(2)%lai(sc%ao,n),p(2)%lai(sc%as,n),p(2)%lai(sc%ao,n-1),p(2)%lai(sc%as,n-1)
            p(2)%lai(sc%ao, n) = p(2)%lai(sc%as, n - 1)

         end if
         !write(*,*)n,p(2)%lai(sc%ao,n),p(2)%lai(sc%as,n),p(2)%lai(sc%ao,n-1),p(2)%lai(sc%as,n-1)
         !DR et ML 20/04/2016 on recalcule les surfaces de la plante qui devient dominante (pour la plante domin�e c'est fait dans transrad)
         p(1)%surf(sc%as) = 1.
         p(1)%surf(sc%ao) = 0.
         !DR et ML 21/04/2016 on reaffecte les bonnes valeurs au logique est dominante
         p(1)%estDominante = .TRUE.
         p(2)%estDominante = .FALSE.

         ! RV: for now some parameters are in the t list, so we have to manually reverse the order when plant dominance is changed.
         ! These parameters will be in the plant parameters file on the next version, so remove the following code as soon as it is done
         t%P_code_shape(1:2) = t%P_code_shape(2:1:-1)
         t%P_elongation(1:2) = t%P_elongation(2:1:-1)
         t%P_nw_height(1:2) = t%P_nw_height(2:1:-1)
         t%P_hautK(1:2) = t%P_hautK(2:1:-1)
         t%P_hautA(1:2) = t%P_hautA(2:1:-1)
         t%P_hautdens(1:2) = t%P_hautdens(2:1:-1)
         t%P_stage_const_height(1:2) = t%P_stage_const_height(2:1:-1)
         t%P_haut_dev_x0(1:2) = t%P_haut_dev_x0(2:1:-1)
         t%P_haut_dev_k(1:2) = t%P_haut_dev_k(2:1:-1)
         t%P_nrow(1:2) = t%P_nrow(2:1:-1)

         ! If first dominance inversion, set the new dominated shaded senescence vars equal to the sunlit component
         if (p(2)%nsencour(sc%ao) .LE. 0.0) p(2)%nsencour(sc%ao) = p(2)%nsencour(sc%as)
         if (p(2)%ndebsen(sc%ao) .LE. 0.0) p(2)%ndebsen(sc%ao) = p(2)%ndebsen(sc%as)
         if (p(2)%nbgrains(sc%ao) .LE. 0.0) p(2)%nbgrains(sc%ao) = p(2)%nbgrains(sc%as)
         ! Initialise the variables for grain filling:
         p(2)%ircarb(sc%ao, n - 1) = p(2)%ircarb(sc%as, n - 1)
         p(2)%magrain(sc%ao, n - 1) = p(2)%magrain(sc%as, n - 1)
         p(2)%dltams(sc%ao, :) = p(2)%dltams(sc%as, :)
         p(2)%magrain(sc%ao, :) = p(2)%magrain(sc%as, :)
         p(2)%masec(sc%ao, :) = p(2)%masec(sc%as, :)
         p(2)%durvie(sc%ao, 1:sc%nbjmax) = p(2)%durvie(sc%as, 1:sc%nbjmax)
      end if
      ! RV: (case 2, see croissance.f90), no true dominance, plants are side by side so there is only a sunlit part.
      if (abs(p(1)%hauteur(0) - p(2)%hauteur(0)) .le. t%P_hauteur_threshold) then
         p(2)%surf(sc%AS) = 1.0
         p(2)%surf(sc%AO) = 0.0
      end if

      ! When changing from beer to geometrical approach, we must initialise some variables for the shaded plant:
      do i = 2, sc%P_nbPlantes
         if (p(i)%vitmoy(sc%ao) .LE. 0.0) p(i)%vitmoy(sc%ao) = p(i)%vitmoy(sc%as)
         if (p(i)%nbgrains(sc%ao) .LE. 0.0) p(i)%nbgrains(sc%ao) = p(i)%nbgrains(sc%as)
         if (p(i)%ircarb(sc%ao, n - 1) .LE. 0.0) p(i)%ircarb(sc%ao, n - 1) = p(i)%ircarb(sc%as, n - 1)
         if (p(i)%ircarb(sc%ao, n) .LE. 0.0) p(i)%ircarb(sc%ao, n) = p(i)%ircarb(sc%as, n)
         if (p(i)%masec(sc%ao, n - 1) .LE. 0.0) p(i)%masec(sc%ao, n - 1) = p(i)%masec(sc%as, n - 1)
         if (p(i)%masec(sc%ao, n) .LE. 0.0) p(i)%masec(sc%ao, n) = p(i)%masec(sc%as, n)
         if (p(i)%magrain(sc%ao, n - 1) .LE. 0.0) p(i)%magrain(sc%ao, n - 1) = p(i)%magrain(sc%as, n - 1)
         if (p(i)%magrain(sc%ao, n) .LE. 0.0) p(i)%magrain(sc%ao, n) = p(i)%magrain(sc%as, n)
         if (p(i)%Qfix(sc%ao) .LE. 0.0) p(i)%Qfix(sc%ao) = p(i)%Qfix(sc%as)
      end do
   end if

!write(*,*)'avant laidev',sc%n,p(1)%lai(0,sc%n-1)

   ! RV: using lai_tot_prev as the total lai of the plot for efdensite and efdensiterac
   ! computations
   if (p(1)%nlev > 0) then
      if (sc%P_nbplantes > 1 .and. p(2)%nlev > 0) then
         sc%lai_tot_prev = p(1)%lai(0, sc%n - 1) + p(2)%lai(0, sc%n - 1)
      else
         sc%lai_tot_prev = p(1)%lai(0, sc%n - 1)
      end if
   else
      sc%lai_tot_prev = 0.0
   end if

   ! Calcul de la photop�riode
   sc%numdate = sc%jul
   call photpd(sta%P_latitude, sc%numdate, c%daylen, c%phoi)

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: laidev'
!#endif

   !: Lai - Developpement
   ! DR 23/07/2012 sta est inutilis�
   !  call laidev(sc,p,pg,itk,t,soil,c,sta)
   call laidev(sc, p, pg, itk, t, soil, c)

!write(*,*)'apres laidev',sc%n,p(1)%lai(0,sc%n)

!--print *,'p(1)%rdtint(0,1:p(1)%nbrecolte)',p(1)%rdtint(0,1:p(1)%nbrecolte)

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: croissance'
!#endif
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'avant croissance'
! print*,'avant croissance', p(1)%masec(1,sc%n)
   !: Croissance - Rendement
   call croissance(sc, pg, p, itk, soil, c, sta, t)
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg2, *) 'aprescroissance', n, 'nbgrains', p(1)%nbgrains(0), &
      p(2)%nbgrains(0)
!   print *,'p(1)%rdtint(0,1:p(1)%nbrecolte)',p(1)%rdtint(0,1:p(1)%nbrecolte)

!      write(*,*)'apres croissance', p(1)%masec(1,sc%n)
   sc%laiTot = 0.
   do i = 1, sc%P_nbPlantes
      sc%laiTot = sc%laiTot + p(i)%lai(0, n)
   end do

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: detassement'
!#endif

   !::: D�tassement

   ! DR 10/11/06 je deplace tassement apres physio
   ! car dans le cas ou on n'active pas la decision de recolte , on a pas acces �
   ! la date de recolte et donc on ne tasse jamais � la recolte
   !        if (P_codeDST == 1.or.P_codeDSTtass == 1) call tassement

   ! DR le 06/02/07 decoupage de tassement en 3 sp
   !  1) on fait le agir detassement du au travail du sol
   !  2) on prend decide de la date semis ou recolte
   !  3) on fait agir le tassement du au passage des engins

   if (itk(1)%P_codeDST == 1) then

!#if DEBUG == 1
!      if (iand(sc%DETASSEMENT,1) >0) &
!        call detassement_debug_read_input(1570,sc,pg,p,itk,soil)
!      if (iand(sc%DETASSEMENT,2) >0) &
!        call detassement_debug_write_input(1571,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif
!DR 01/02/2011 on remplace napS par P_nbjtrav car popur le detassement c'est du travail du sol
      call detassement(n, sc%NH, sc%nbCouchesSol, sc%sat(1:sc%nbCouchesSol), itk(1)%P_nbjtrav, &
                       p(1)%numjtrav(1:itk(1)%P_nbjtrav), itk(1)%P_proftrav(1:itk(1)%P_nbjtrav), &
                       pg%P_proflabour, &
                       pg%P_proftravmin, itk(1)%P_rugochisel, itk(1)%P_dachisel, soil%P_codefente, &
                       soil%P_hccf(1:sc%NH), soil%P_hminf(1:sc%NH), sc%beta_sol(1:2), itk(1)%P_rugolabour, &
                       itk(1)%P_dalabour, soil%AZnit(1:sc%NH), sc%AZamm(1:sc%NH), p(1)%P_coderacine, &
                       p(1)%lrach(1:sc%NH), pg%P_lvopt, soil%da_ini(1:2), soil%zesx_ini, soil%q0_ini, &
                       pg%nbResidus, &
                       sc%nhe, sc%HR(1:sc%NH), sc%TS(1:sc%NH), soil%P_epc(1:sc%NH), soil%profsol, &
                       soil%P_profhum, soil%P_z0solnu, soil%profhum_tass(1:2), soil%P_infil(1:2), soil%ncel, &
                       soil%da(1:sc%NH), soil%P_epd(1:sc%NH), soil%icel(0:sc%nbCouchesSol), &
                       sc%hur(1:sc%nbCouchesSol), sc%hucc(1:sc%nbCouchesSol), &
                       sc%humin(1:sc%nbCouchesSol), sc%tsol(1:sc%nbCouchesSol), soil%izcel(1:sc%NH), &
                       soil%izc(1:sc%NH), soil%nit(1:sc%nbCouchesSol), soil%amm(1:sc%nbCouchesSol), &
                       p(1)%rl(n, 1:sc%nbCouchesSol), p(1)%flrac(1:sc%nbCouchesSol), &
                       sc%Cbio(1:sc%nbCouchesSol, 1:pg%nbResidus), sc%Nbio(1:sc%nbCouchesSol, 1:pg%nbResidus), &
                       sc%Cres(1:sc%nbCouchesSol, 1:pg%nbResidus), sc%Nres(1:sc%nbCouchesSol, 1:pg%nbResidus), &
                       sc%Chum(1:sc%nbCouchesSol), sc%Nhum(1:sc%nbCouchesSol), soil%P_zesx, soil%P_q0)

!#if DEBUG == 1
!      if (iand(sc%DETASSEMENT,4) >0 .or. iand(sc%DETASSEMENT,16) >0)    &
!           call detassement_debug_read_output(1572,sc,pg)
!      if (iand(sc%DETASSEMENT,8) >0) &
!        call detassement_debug_write_output(1573,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%DETASSEMENT,16) >0) &
!        call detassement_debug_test_output(1574,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

   end if
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: decisionsemis'

   !::: D�cision semis et r�colte
   ! DR 31/01/07 on sort decision semis et recolte de tassement car
   !  finalement ca peut etre utilis� independemment du tassement
   if (itk(1)%P_codedecisemis == 1 .and. p(1)%P_codeplante /= 'snu') then
      call decisionsemis(n, sc%P_iwater, sc%NH, sc%nbCouchesSol, pg%P_prophumtasssem, soil%P_hccf(1:sc%NH), &
                         soil%P_epc(1:sc%NH), sc%hur(1:sc%nbCouchesSol), soil%da(1:sc%NH), &
                         sc%sat(1:sc%nbCouchesSol), itk(1)%P_profhumsemoir, itk(1)%P_profsem, &
                         sc%hucc(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), p(1)%P_sensrsec, &
                         t%P_humirac_decisemis, itk(1)%P_nbjseuiltempref, c%tmin(n:n + itk(1)%P_nbjseuiltempref), &
                         p(1)%P_tdebgel, c%tmoy(n:n + itk(1)%P_nbjseuiltempref), p(1)%P_tdmin, t%P_eau_mini_decisemis, &
                         t%P_nbj_pr_apres_semis, sc%esol, itk(1)%P_codecalirrig, pg%P_irrlev, &
                         sc%airg(n:n + t%P_nbj_pr_apres_semis), c%trr(n:n + t%P_nbj_pr_apres_semis), &
                         itk(1)%P_nbjmaxapressemis, &
                         p(1)%nbjpourdecisemis, sc%repoussesemis(1), p(1)%nplt, sc%iplt(1))
   end if
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: decisionrecolte'

   if (itk(1)%P_codedecirecolte == 1 .and. p(1)%P_codeplante /= 'snu') then
      call decisionrecolte(n, pg%P_prophumtassrec, soil%P_hccf(1:sc%NH), sc%NH, sc%nbCouchesSol, soil%P_epc(1:sc%NH), &
                           sc%hur(1:sc%nbCouchesSol), soil%da(1:sc%NH), sc%sat(1:sc%nbCouchesSol), &   ! IN
                           itk(1)%P_profhumrecolteuse, p(1)%nrecbutoir, itk(1)%P_nbjmaxapresrecolte, &
                           p(1)%nbjpourdecirecolte, sc%repousserecolte(1), p(1)%nrec)                          ! INOUT
   end if

   !::: Tassement semis & r�colte
   if (itk(1)%P_codeDSTtass == 1) then

!#if DEBUG == 1
!      if (iand(sc%TASSESEMISRECOLTE,1) >0) &
!       call tassesemisrecolte_debug_read_input(1580,sc,pg,p,itk,soil)
!      if (iand(sc%TASSESEMISRECOLTE,2) >0) &
!        call tassesemisrecolte_debug_write_input(1581,sc,pg,p,itk,soil)
!#endif
      if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: tassesemisrecolte'

      call tassesemisrecolte(n, sc%NH, sc%nbCouchesSol, p(1)%P_codeplante, p(1)%nplt, p(1)%nrec, itk(1)%P_profhumsemoir, &
                             pg%P_prophumtasssem, soil%P_hccf(1:sc%NH), itk(1)%P_dasemis, itk(1)%P_profhumrecolteuse, &
                             pg%P_prophumtassrec, itk(1)%P_darecolte, sc%sat(1:sc%nbCouchesSol), sc%beta_sol, &
                             itk(1)%P_codeDSTnbcouche, p(1)%P_coderacine, soil%da_ini(1:2), soil%zesx_ini, soil%q0_ini, &
                             pg%nbResidus, sc%Hinit(1:2), &
                             soil%profsol, soil%P_infil(1:sc%NH), soil%da(1:sc%NH), &
                             soil%profhum_tass(1:2), soil%P_obstarac, soil%icel(0:sc%nbCouchesSol), soil%ncel, &
                             soil%izcel(1:sc%NH), sc%nhe, soil%P_profhum, soil%izc(1:sc%NH), soil%P_epc(1:sc%NH), &
                             soil%P_epd(1:sc%NH), sc%hur(1:sc%nbCouchesSol), soil%nit(1:sc%nbCouchesSol), &
                             soil%amm(1:sc%nbCouchesSol), p(1)%flrac(1:sc%nbCouchesSol), p(1)%rl(n, 1:sc%nbCouchesSol), &
                             sc%Cbio(1:sc%nbCouchesSol, 1:pg%nbResidus), sc%Cres(1:sc%nbCouchesSol, 1:pg%nbResidus), &
                             sc%Nres(1:sc%nbCouchesSol, 1:pg%nbResidus), sc%Nbio(1:sc%nbCouchesSol, 1:pg%nbResidus), &
                             sc%Chum(1:sc%nbCouchesSol), sc%Nhum(1:sc%nbCouchesSol), sc%hucc(1:sc%nbCouchesSol), &
                             sc%humin(1:sc%nbCouchesSol), sc%tsol(1:sc%nbCouchesSol), soil%P_zesx, soil%P_q0, sc%HR(1:sc%NH))

!#if DEBUG == 1
!      if (iand(sc%TASSESEMISRECOLTE,4) >0 .or. iand(sc%tassesemisrecolte,16) >0)    &
!           call tassesemisrecolte_debug_read_output(1582,sc,pg)
!      if (iand(sc%TASSESEMISRECOLTE,8) >0) &
!        call tassesemisrecolte_debug_write_output(1583,sc,pg,p,soil)
!      if (iand(sc%TASSESEMISRECOLTE,16) >0) &
!        call tassesemisrecolte_debug_test_output(1584,sc,pg,p,soil)
!#endif

   end if

!#if DEBUG == 1
!print *,'Stics_Jour: apports'
!#endif
! DR et ML 16/10/2013 on change la valeur du code pour etre coherent avce la syntaxe classique (1 devinet 2 et 2 devient 1)
!if (t%P_codeSWDRH.eq.2) then
   if (t%P_codeSWDRH .eq. 1) then
!: ML - 29/10/12 - calcul dur�e d'humectation
!- appel prealable de shutwall et humheure
!- pour calculer tculthoraire et determiner s'il y a humectation ou non
      call shutwall(sc, pg, c, sta, soil, p, itk, t)
      if (n > 1) tmaxveille = sc%tcultveille*2 - c%tmin(n - 1)
      if (n == 1) tmaxveille = sc%tcultmax

!      c%compteurhumheure = 1

!      call humheure(n,sc%tcultmin,sc%tcultmax,tmaxveille,sc%humidite,c%daylen,c%tmin(n+1),sc%tcult,    &
!           sc%trosemax(0:n),c%humimoy,c%tmin(n),c%dureehumec,c%dureeRH,c%dureeRH1,c%dureeRH2,c%compteurhumheure,&
!           c%trr(n))

      call humheure(n, sc%tcultmin, sc%tcultmax, tmaxveille, sc%humidite, c%daylen, c%tmin(n + 1), sc%tcult, &
                    sc%trosemax(0:n), c%humimoy, c%tmin(n), t%P_codetrosee, tculth, troseh, humh)

      call surface_wetness_duration_relative_humidity(tculth, troseh, humh, &
                                                      c%dureehumec, c%dureeRH, c%dureeRH1, c%dureeRH2, c%trr(n))

!write(*,*)' ELIAS !!!! dureehumec',n,c%dureehumec
!: ML fin
   end if

!::: Apports
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg, *) 'Stics_Jour: before apports '
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: before apports '

   call apports(sc, pg, t, c, soil, p, itk)
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: after apports '

!write(71,*)'Stics_Jour: apres apport irmulch et qmulch',sc%irmulch,sc%qmulch

!::: Etatsurf

   sc%stemflowTot = 0.
   do i = 1, sc%P_nbPlantes
      sc%stemflowTot = sc%stemflowTot + p(i)%stemflow
   end do

   ipl = 1
   ens = sc%AS
! Ajout Bruno juin 2012 Cmulchnd en kg C/ha et qmulch en t MS/ha
!      sc%qmulch = sc%Cmulchnd / 420. correction bug Bruno 25/07/2014
! Modif Bruno avril 2013 Cmulchnd et Cmulchdec en kg C/ha et qmulch en t MS/ha
   sc%qmulch = (sc%Cmulchdec + sc%Cmulchnd)/420.

!#if DEBUG == 1
!      if (iand(sc%ETATSURF,1) >0) call etatsurf_debug_read_input(1360,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%ETATSURF,2) >0) call etatsurf_debug_write_input(1361,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

!      if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0) print *,'Stics_Jour: before etatsurf',itk(1)%P_codepaillage,sc%jjul

! DR 01/02/2011 on remplace naps pour P_nbjtrav
   if (itk(1)%P_codepaillage == 2) then   !! paillage mulch plastique
      !  call etatsurf(n,sc%jjul,itk(1)%P_nbjtrav,itk(1)%P_jultrav(1:itk(1)%P_nbjtrav),  &   ! IN
      !              itk(1)%P_proftrav(1:itk(1)%P_nbjtrav),sc%nappmulch,itk(1)%P_codepaillage,       &
      ! DR 07/06/2017 je concerve la valeur du couvermulch plastique sinon on la perd ...
      toto = itk(1)%P_couvermulchplastique

      call etatsurf(n, sc%jjul, &   ! IN
                    sc%stemflowTot, pg%P_pminruis, soil%P_ruisolnu, pg%P_qmulchruis0(sc%irmulch), &
                    pg%P_mouillabilmulch(sc%irmulch), pg%P_kcouvmlch(sc%irmulch), p(1)%P_codebeso, &
                    p(1)%P_codelaitr, c%tetp(n), sc%delta, sc%laiTot, sc%tauxcouv(n), &
                    sc%qmulch, sc%ruisselsurf, sc%precip, sc%mouillmulch, sc%intermulch, &   ! INOUT
                    itk(1)%P_couvermulchplastique, sc%Emulch, soil%P_penterui)
      ! DR 07/06/2017 je concerve la valeur du couvermulch plastique sinon on la perd ...
      itk(1)%P_couvermulchplastique = toto
   else
!write(71,*)'Stics_Jour: avant etatsurf irmulch et qmulch',sc%irmulch,sc%qmulch
      ! call etatsurf(n,sc%jjul,itk(1)%P_nbjtrav,itk(1)%P_jultrav(1:itk(1)%P_nbjtrav),  &   ! IN
      !             itk(1)%P_proftrav(1:itk(1)%P_nbjtrav),sc%nappmulch,itk(1)%P_codepaillage,       &

      call etatsurf(n, sc%jjul, &   ! IN
                    sc%stemflowTot, pg%P_pminruis, soil%P_ruisolnu, pg%P_qmulchruis0(sc%irmulch), &
                    pg%P_mouillabilmulch(sc%irmulch), pg%P_kcouvmlch(sc%irmulch), p(1)%P_codebeso, &
                    p(1)%P_codelaitr, c%tetp(n), sc%delta, sc%laiTot, sc%tauxcouv(n), &
                    sc%qmulch, sc%ruisselsurf, sc%precip, sc%mouillmulch, sc%intermulch, &   ! INOUT
                    sc%couvermulch, sc%Emulch, soil%P_penterui)
!write(71,*)'Stics_Jour: apres etatsurf',sc%irmulch,sc%qmulch

   end if
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: after etatsurf'

!#if DEBUG == 1
!      if (iand(sc%ETATSURF,4) >0 .or. iand(sc%ETATSURF,16) >0)    &
!           call etatsurf_debug_read_output(1362,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%ETATSURF,8) >0) call etatsurf_debug_write_output(1363,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%ETATSURF,16) >0) call etatsurf_debug_test_output(1364,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

!#if DEBUG == 1
!      if (iand(sc%MINERAL,1) >0) call mineral_debug_read_input(1370,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%MINERAL,2) >0) call mineral_debug_write_input(1371,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif
!  Calcul de la volatilisation li�e aux apports de r�sidus organiques
   if (soil%Nvolatorg <= 0.1) then
      sc%FsNH3 = 0.
   else
      if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'volatilisation'
      call volatorg(soil%da(1), sc%hur(1:2), soil%P_pH, sc%tcult, sta%P_NH3ref, sc%ras, sta%P_ra, soil%dpH, & ! IN
                    sc%FsNH3, soil%Nvolorg, soil%amm(1), soil%Nvolatorg, soil%QNvolorg, soil%pHvol)
   end if
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: Mineralisation'
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: Mineralisation'
!   print *,'avant mineral'
!   write(200,*)'avant mineral',sc%n,soil%nit(1:60)
   call mineral(sc%nbCouchesSol, pg%nbResidus, &
                soil%P_profhum, soil%P_argi, soil%P_calc, soil%P_codefente, t%P_codeNmindec, &  ! codes "activation"
                pg%P_fhminsat, pg%P_fmin1, pg%P_fmin2, pg%P_fmin3, pg%P_FTEMh, pg%P_FTEMha, &
                pg%P_FTEMr, pg%P_FTEMra, sc%FTEMhb, sc%FTEMrb, pg%P_hminm, pg%P_hoptm, sc%var_TREFh, &
                pg%P_Wh, &   !decomposition
                sc%kres(1:pg%nbResidus), pg%P_kbio(1:pg%nbResidus), sc%Wb(1:pg%nbResidus), &
                pg%P_yres(1:pg%nbResidus), sc%hres(1:pg%nbResidus), &
                pg%P_fNCbiomin, pg%P_fredlN, pg%P_fredkN, t%P_rapNmindec, t%P_fNmindecmin, &
                pg%P_fredNsup, pg%P_Primingmax, &   !mineralisation
                sc%tsol(1:sc%nbCouchesSol), sc%hucc(1:sc%nbCouchesSol), sc%hur(1:sc%nbCouchesSol), &
                sc%dacouche(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), sc%sat(1:sc%nbCouchesSol), &
                soil%itrav1, soil%itrav2, &                                     !variables d'entree IN
                sc%Cres(1:sc%nbCouchesSol, 1:pg%nbResidus), sc%Nres(1:sc%nbCouchesSol, 1:pg%nbResidus), &
                sc%Cbio(1:sc%nbCouchesSol, 1:pg%nbResidus), sc%Nbio(1:sc%nbCouchesSol, 1:pg%nbResidus), &
                sc%Chum(1:sc%nbCouchesSol), sc%Nhum(1:sc%nbCouchesSol), sc%Chuma, sc%Nhuma, sc%Chumi, sc%Nhumi, &
                sc%Chumt, sc%Nhumt, sc%Cr, sc%Nr, sc%Cb, sc%Nb, sc%NCbio, & !INOUT
                soil%amm(1:sc%nbCouchesSol), soil%nit(1:sc%nbCouchesSol), sc%CO2hum, sc%CO2res, sc%CO2sol, soil%vminr, &
                soil%cumvminh, soil%cumvminr, sc%QCO2sol, sc%QCO2res, sc%QCO2hum, sc%Qminh, sc%Qminr, &
                sc%QCprimed, sc%QNprimed, sc%tnhc, sc%tnrc, sc%Cnondec(1:10), sc%Nnondec(1:10), &
                sc%Cmulchnd, sc%Nmulchnd, sc%Cmulchdec, sc%Nmulchdec, sc%Cmulch, sc%Nmulch, sc%Cbmulch, sc%Nbmulch, sc%cum_immob, &
                sc%QCO2mul)
!   write(200,*)'apres mineral',sc%n,soil%nit(1:60)
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: nitrification'

! Jo�l 4/2/2015
! remplacement call nitrif (obsol�te) par call nitrif_N2O
! dr 08/11/2016 les parametres sont dans la structure pg maintenant
   call nitrif_N2O(sc%nbCouchesSol, sc%numcult, soil%P_profhum, soil%P_codefente, soil%P_codenitrif, &
                   pg%P_rationit, pg%P_hoptn, pg%P_hminn, pg%P_fnx, soil%P_pH, pg%P_pHmaxnit, pg%P_pHminnit, &
                   sc%var_tnitmin(sc%numcult), sc%var_tnitopt(sc%numcult), sc%var_tnitmax(sc%numcult), sc%var_tnitopt2, &
                   pg%P_vnitmax, pg%P_Kamm, pg%P_nh4_min, &
                   sc%tsol(1:sc%nbCouchesSol), sc%hucc(1:sc%nbCouchesSol), sc%hur(1:sc%nbCouchesSol), &
                   sc%dacouche(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), sc%sat(1:sc%nbCouchesSol), & ! IN
                   !           pg%P_code_vnit, pg%P_fnx_soil, pg%P_code_tnit, %P_tnitmin_pw, t%P_tnitopt_pw, t%P_tnitopt2_pw,          &
                   !           t%P_tnitmax_pw, pg%P_tnitopt_gauss, pg%P_scale_tnitopt, pg%P_code_rationit, pg%P_rationit,       &
                   pg%P_code_vnit, pg%P_code_tnit, &
                   pg%P_tnitopt_gauss, pg%P_scale_tnitopt, pg%P_code_rationit, &
                   pg%P_code_hourly_wfps_nit, sc%precip, pg%P_kdesat, &  ! IN
                   soil%amm(1:sc%nbCouchesSol), soil%nit(1:sc%nbCouchesSol), & ! INOUT
                   soil%nitrifj, sc%Qnitrif, sc%em_N2Onit, sc%Qem_N2Onit)

   !call nitrif(sc%nbCouchesSol,sc%numcult,soil%P_profhum,soil%P_codefente,soil%P_codenitrif,pg%P_rationit,     &
   !          pg%P_hoptn,pg%P_hminn,pg%P_fnx,soil%P_pH,pg%P_pHmaxnit,pg%P_pHminnit,                             &
   !          sc%var_tnitmin(sc%numcult),sc%var_tnitopt(sc%numcult),sc%var_tnitmax(sc%numcult),sc%var_tnitopt2, &
   !          sc%tsol(1:sc%nbCouchesSol),sc%hucc(1:sc%nbCouchesSol),sc%hur(1:sc%nbCouchesSol),                  &
   !          sc%dacouche(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),sc%sat(1:sc%nbCouchesSol),  & !variables d'entree
   !          soil%nitrifj,sc%Qnitrif,soil%amm(1:sc%nbCouchesSol),soil%nit(1:sc%nbCouchesSol),       &  !INOUT
   !          sc%em_N2Onit,sc%Qem_N2Onit)

!************
! Calcul de la d�nitrification par modele NOE (conditions de sol)
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: denitrification'

! Jo�l 4/2/2015
! remplacement call denit (obsol�te) par call denit_N2O

   if (soil%P_codedenit == 1) then
      call denit_N2O(sc%nbCouchesSol, soil%P_codedenit, soil%P_profdenit, soil%P_vpotdenit, pg%P_ratiodenit, &
                     soil%P_codefente, soil%P_pH, soil%P_Norg, soil%P_CsurNsol, pg%P_pHminden, pg%P_pHmaxden, &
                     !sc%var_TREFdenit1, sc%var_TREFdenit2,                                                             &
                     pg%P_Kd, pg%P_kdesat, pg%P_wfpsc, &
                     sc%dacouche(1:sc%nbCouchesSol), sc%hucc(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), &
                     sc%sat(1:sc%nbCouchesSol), sc%hur(1:sc%nbCouchesSol), sc%tsol(1:sc%nbCouchesSol), sc%precip, & ! IN
                     pg%P_tdenitopt_gauss, pg%P_scale_tdenitopt, pg%P_code_pdenit, pg%P_cmin_pdenit, pg%P_cmax_pdenit, &
                     pg%P_min_pdenit, pg%P_max_pdenit, pg%P_code_ratiodenit, &
                     pg%P_code_hourly_wfps_denit, &
                     soil%nit(1:sc%nbCouchesSol), soil%condenit, soil%Ndenit, soil%QNdenit, sc%em_N2Oden, sc%Qem_N2Oden)    ! INOUT
   end if

!         if (soil%P_codedenit == 1)then
!                    call denit(soil%P_profdenit,soil%P_codefente,sc%nbCouchesSol,sc%dacouche(1:sc%nbCouchesSol), &
!                    sc%hucc(1:sc%nbCouchesSol),sc%humin(1:sc%nbCouchesSol),sc%hur(1:sc%nbCouchesSol),            &
!                    sc%sat(1:sc%nbCouchesSol),sc%tsol(1:sc%nbCouchesSol),sc%var_TREFh,                           &   ! IN
!                    sc%var_TREFdenit1,sc%var_TREFdenit2, soil%P_vpotdenit,pg%P_ratiodenit,                       &
!                    soil%Ndenit,soil%condenit,soil%nit(1:sc%nbCouchesSol),soil%QNdenit,sc%em_N2Oden,sc%Qem_N2Oden)   ! INOUT
!         endif

! Emissions totales de N2O
   sc%em_N2O = sc%em_N2Onit + sc%em_N2Oden
   sc%Qem_N2O = sc%Qem_N2Onit + sc%Qem_N2Oden

!*************

!::: Calcul des Besoins - Besoin en eau, nodules, besoin en azote

!#if DEBUG == 1
!      if (sc%DEBUG .eqv. .TRUE.) print *,'Stics_Jour: beaupl'
!#endif
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: besoinsEnEauDeLaPlante'
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: besoinsEnEauDeLaPlante'
!   print *,'besoin eneau'
   call besoinsEnEauDeLaPlante(sc, pg, c, sta, soil, p, itk, t)

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: besoins azote'
!#endif

   ! Besoins en azote
   do i = 1, sc%P_nbplantes
      do ens = sc%AS, sc%AO
         if (p(i)%surf(ens) > 0.) then

            ! Nodosit�s
            if (p(i)%P_codelegume == 2 .and. pg%P_codesymbiose == 2) then

!#if DEBUG == 1
!              if (iand(sc%OFFRNODU,1) >0) call offrnodu_debug_read_input(1410,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!              if (iand(sc%OFFRNODU,2) >0) call offrnodu_debug_write_input(1411,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif
               if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'offrnodu'
!   write(200,*)'avant  offrnodu',sc%n,soil%nit(1:60)
               call offrnodu(n, p(i)%nlev, p(i)%zrac, p(i)%dtj(n), itk(i)%P_profsem, p(i)%P_profnod, sc%nbCouchesSol, & ! IN
                             soil%nit(1:sc%nbCouchesSol), sc%hur(1:sc%nbCouchesSol), p(i)%P_concNnodseuil, p(i)%P_vitno, &
                             p(i)%P_codefixpot, p(i)%P_fixmax, p(i)%P_fixmaxveg, p(i)%P_fixmaxgr, p(i)%dltams(ens, n), &
                             p(i)%dltags(ens), p(i)%P_concNrac100, p(i)%P_concNrac0, pg%P_codefxn, &
                             sc%humin(1:sc%nbCouchesSol), sc%tcultmin, sc%tcultmax, sc%tsol(1:sc%nbCouchesSol), &
                             p(i)%P_tempnod1, p(i)%P_tempnod2, p(i)%P_tempnod3, p(i)%P_tempnod4, sc%anox(1:sc%nbCouchesSol), &
                             p(i)%fixreel(ens), p(i)%somcourno(ens), p(i)%P_stlevdno, p(i)%ndno, p(i)%P_stdnofno, p(i)%nfno, & ! INOUT
                             p(i)%P_stfnofvino, p(i)%nfvino, sc%nodn, p(i)%propfixpot, p(i)%fixpotfno, &
                             p(i)%fixmaxvar(ens), p(i)%fixpot(ens), sc%fxn, sc%fxw, sc%fxt, sc%fxa)
!   write(200,*)'apres  offrnodu',sc%n,soil%nit(1:60)
!#if DEBUG == 1
!              if (iand(sc%OFFRNODU,4) >0) call offrnodu_debug_read_output(1412,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!              if (iand(sc%OFFRNODU,8) >0) call offrnodu_debug_write_output(1413,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!              if (iand(sc%OFFRNODU,16) >0) call offrnodu_debug_test_output(1414,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

            end if

!#if DEBUG == 1
!            if (iand(sc%BNPL,1) >0) call bnpl_debug_read_input(1420,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
            !           if (iand(sc%BNPL,2) >0) call bnpl_debug_write_input(1421,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

            ! RV: Trying to compute the dilution curve for intercrops with the most reasonable assumption that masecdil is the sum of both plants biomass:
            ! adil, bdil, adilmax and bdilmax are all measured at a reference density. The max and critical
            ! dilution curves are computed using a masec in sole crop, not in lower density intercrop. So to correct for the mixing effect that disturb our dilution
            ! curves, we use both plants biomass as a proxy for the sole crop equivalent biomass:
            if (sc%P_nbplantes > 1) then
               p(i)%masecdil(ens) = p(1)%masec(0, n - 1) + p(2)%masec(0, n - 1)
               ! NB: we use the masec from the previous day because we don't know yet the masec of the whole plot (during i==1 and i==2 + ens==AS)

               ! In case of picking (e.g. tomato, strawberry...), we add the biomass picked (I moved it out of bNpl).
               ! plant 1:
               do irecolte = 1, max(1, p(1)%nbrecolte - 1)
                  p(i)%masecdil(ens) = p(i)%masecdil(ens) + (p(1)%rdtint(0, irecolte)/100.)
               end do
               ! plant 2:
               do irecolte = 1, max(1, p(2)%nbrecolte - 1)
                  p(i)%masecdil(ens) = p(i)%masecdil(ens) + (p(2)%rdtint(0, irecolte)/100.)
               end do

magrain_abso = ((1 - sc%absodrp(i))*p(i)%magrain(ens, n - 1)/100.) + ((1 - sc%absodrp(2))*p(2)%magrain(0, n - 1)/100.) ! total fruit biomass that need N !
               ! RV: computing magrain_abso now because then absodrp is updated and we want to use the one from the previous day for both crops, but for sole crops we want
               ! to use the one from the current day, so we compute it afterwards.
               masecNmax_mean = (p(1)%P_masecNmax + p(2)%P_masecNmax)/2.0

               p(i)%masecpartiel(ens) = p(i)%masec(ens, n)
               ! masecpartiel is still only the biomass from the plant + rdtint of interest (and not the sum of both plants)
               do irecolte = 1, max(1, p(i)%nbrecolte - 1)
                  p(i)%masecpartiel(ens) = p(i)%masecpartiel(ens) + (p(i)%rdtint(0, irecolte)/100.)
               end do

            else
               ! Case of the sole crop:
               p(i)%masecdil(ens) = p(i)%masec(ens, n)

               do irecolte = 1, max(1, p(i)%nbrecolte - 1)
                  p(i)%masecdil(ens) = p(i)%masecdil(ens) + (p(i)%rdtint(ens, irecolte)/100.)
               end do

               p(i)%masecpartiel(ens) = p(i)%masecdil(ens)
               masecNmax_mean = p(i)%P_masecNmax
            end if

            if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'bNpl'
            call bNpl(p(i)%nbrecolte, & ! IN
                      p(i)%dltams(ens, n), p(i)%P_codeplisoleN, masecNmax_mean, p(i)%P_masecmeta, t%adilmaxI(i), &
                      t%bdilmaxI(i), t%adilI(i), t%bdilI(i), p(i)%P_adilmax, p(i)%P_bdilmax, p(i)%P_bdil, p(i)%P_adil, &
                      p(i)%dltags(ens), t%dltamsN(i, ens), p(i)%P_inngrain1, t%dltaremobilN(i, ens), p(i)%P_codelegume, &
                      pg%P_codesymbiose, p(i)%fixreel(ens), &
                      p(i)%masecdil(ens), p(i)%inns(ens), p(i)%innlai(ens), p(i)%demande(ens), & ! INOUT
                      p(i)%abso(ens, n), p(i)%dNdWcrit, t%deltabso(i, ens), sc%absodrp(i), p(i)%P_inngrain2, &
                      p(i)%offrenod(ens), sc%demandebrute)

            if (sc%P_nbplantes == 1) then
               !  p(i)%demande(ens) = 5.0
               magrain_abso = ((1 - sc%absodrp(i))*p(i)%magrain(ens, n)/100.) ! fruit biomass that need N. For intercrops, see computation just before bNpl.
               ! else
               !     p(i)%demande(ens) = 2.5
            end if

!#if DEBUG == 1
!            if (iand(sc%BNPL,4) >0) call bnpl_debug_read_output(1422,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!            if (iand(sc%BNPL,8) >0) call bnpl_debug_write_output(1423,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!            if (iand(sc%BNPL,16) >0) call bnpl_debug_test_output(1424,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

         end if
      end do
   end do

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: lixivation'
!#endif

!::: Lixivation

   ! on calcul la somme des epz de toutes les cultures...
   epzTot(:) = 0.0
   do iz = 1, sc%nhe
      do i = 1, sc%P_nbplantes
         ! DR et ML 01/09/2014 c'est le cumul des transpirations des 2 plantes  (prenant en compte les surfaces et
         ! calcul� dans transpi la veille ) qu'il faut enlever du sol
         ! le bug faisait qu'on n'enlevait pas � HUR les ep transpir�s la veille (on ne ponderait pas par les surfaces !)
         ! induisant un ecart de bilan pouvant etre fort
         !   epzTot(iz) = epzTot(iz) + p(i)%epz(sc%AS,iz) + p(i)%epz(sc%AO,iz)
         epzTot(iz) = epzTot(iz) + p(i)%epz(sc%AOAS, iz)
      end do
   end do

! dr 01/09/2014 on cumule les epz pour verif
!      epz_cum=sum(epzTot(:))
!      write(*,*)n,'avantlixiv',epz_cum
!

   zracMAXI = 0.0
   do i = 1, sc%P_nbplantes
      zracMAXI = max(zracMAXI, p(i)%zrac)
   end do

!#if DEBUG == 1
!      if (iand(sc%LIXIV,1) >0) call lixiv_debug_read_input(1430,sc,pg,p,itk,soil,c,sta,t,ipl,ens,epzTot(1:sc%nhe),zracMAXI)
!      if (iand(sc%LIXIV,2) >0) call lixiv_debug_write_input(1431,sc,pg,p,itk,soil,c,sta,t,ipl,ens,epzTot(1:sc%nhe),zracMAXI)
!#endif

   ! **************************************************************************************************
   ! DR 10/06/09 on met un test sur la lecture de leccapil pour l'utilisation du lixiv de Guillaume Jego
   ! DR 03/08/09 on nettoie le code des specificit�s nappe
   ! if (codeleccapil == 1) then
   !   call lixiv_nappe
   ! else
!   write(200,*)'avant lixiv',sc%n,soil%nit(1:60)
   call lixiv(sc%n, sc%precip, sc%hurlim, sc%ruisselsurf, sc%humin(1:int(soil%profsol)), sc%nhe, sc%nh, & ! les entr�es
              sc%hucc(1:int(soil%profsol)), sc%hr(1:sc%NH), soil%P_codefente, soil%ncel, soil%icel(0:soil%ncel), &
              soil%da(1:sc%NH), soil%izcel(1:5), soil%P_infil(0:5), soil%P_concseuil, soil%P_humcapil, soil%izc(1:5), &
              soil%hcc(1:sc%NH), soil%P_capiljour, soil%P_epc(1:sc%NH), soil%hmin(1:sc%NH), soil%P_codemacropor, &
              soil%P_profdrain, soil%profsol, pg%P_bformnappe, pg%P_distdrain, pg%P_codhnappe, soil%ldrains, soil%P_codrainage, &
              pg%P_rdrain, soil%P_profimper, soil%P_ksol, soil%profcalc, epzTot(1:sc%nhe), sc%esz(1:sc%nhe), zracMAXI, &
              p(1)%irrigprof(1:sc%nhe), &
              sc%bouchon, sc%hur(1:int(soil%profsol)), sc%sat(1:int(soil%profsol)), sc%azomes, sc%anox(1:sc%nhe), & ! les sorties
              soil%nit(1:int(soil%profsol)), sc%pluieruissel, sc%saturation, sc%resmes, sc%RU, sc%RsurRU, &
              soil%concno3les, soil%hnappe, soil%hmax, soil%hph, soil%hpb, soil%profnappe, soil%qdrain, soil%azlesd, &
              sc%azsup, sc%QLES, sc%DRAT, sc%drain, sc%ruissel, soil%remontee, soil%qlesd, soil%qdraincum, &
              sc%exces(0:5), soil%amm(1:int(soil%profsol)), sc%ammomes, &
              itk(1)%P_profsem, sc%P_profmesW, sc%P_profmesN, sc%SoilAvW, sc%SoilWatM, sc%SoilNM, sc%lessiv, &
              sc%som_HUR, sc%som_sat) ! ajout entrees et des sorties Macsur

   !  write(200,*)'apres lixiv',sc%n,soil%nit(1:60)
   ! endif

!#if DEBUG == 1
!      if (iand(sc%LIXIV,4) >0) call lixiv_debug_read_output(1432,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%LIXIV,8) >0) call lixiv_debug_write_output(1433,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%LIXIV,16) >0) call lixiv_debug_test_output(1434,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

   ! PB - 03/08/2010
   ! J'extrait cette modif de lixiv et je mets dans une boucle P_nbplantes

   ! DR et ML et BM et EJ 22/06/09
   ! *****************************
   ! introduction des modifications de BM et Eric Justes
   ! ** calcul de RsurRU
   do i = 1, sc%P_nbplantes
      p(i)%RsurRUrac = 0.
      p(i)%RUrac = 0.
      do iz = int(itk(i)%P_profsem), int(p(i)%zrac)
         p(i)%RsurRUrac = p(i)%RsurRUrac + max(sc%hur(iz) + sc%sat(iz) - sc%humin(iz), 0.)
         p(i)%RUrac = p(i)%RUrac + sc%hucc(iz) - sc%humin(iz)
      end do

      if (p(i)%RUrac == 0) then
         p(i)%RsurRUrac = 1.0
      else
         p(i)%RsurRUrac = p(i)%RsurRUrac/p(i)%RUrac
      end if
   end do
   ! fin DR et ML et BM 22/06/09

   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour:  Transpiration'
   do i = 1, sc%P_nbplantes
      do ens = sc%AS, sc%AO
         !if (p(i)%lai(0,n) > 0.) then
         if (p(i)%surf(ens) > 0.) then

!#if DEBUG == 1
!            print *,'Stics_Jour: transpiration'
!            if (iand(sc%transpi,1) >0) call transpi_debug_read_input(1440,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!            if (iand(sc%transpi,2) >0) call transpi_debug_write_input(1441,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif
!   print *,'avant transpi'
            call transpi(sc%n, sc%nbCouchesSol, soil%profsol, sc%hur(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), &
                         ! 01/09/2014 DR et ML  ERREUR : la surface surf n'est pas indexee sur AO/AS ce qui fait qu'on utilise tj la surface au soleil et ljamais celle a l'ombre
                         !                         p(i)%lracz(1:sc%nbCouchesSol),p(i)%surf,p(i)%P_codelaitr,p(i)%lai(ens,n),                &
                         p(i)%lracz(1:sc%nbCouchesSol), p(i)%surf(ens), p(i)%P_codelaitr, p(i)%lai(ens, n), &
                         sc%tauxcouv(n), p(i)%nrec, itk(i)%P_codcueille, p(i)%eop(ens), p(i)%exobiom, &
                         p(i)%epz(ens, 1:sc%nbCouchesSol), p(i)%ep(ens), p(i)%turfac(ens), p(i)%senfac(ens), &
                         p(i)%swfac(ens), p(i)%profexteau)

!#if DEBUG == 1
!            if (iand(sc%transpi,4) >0) call transpi_debug_read_output(1442,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!            if (iand(sc%transpi,8) >0) call transpi_debug_write_output(1443,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!            if (iand(sc%transpi,16) >0) call transpi_debug_test_output(1444,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

         end if
      end do
      ! Ombre/soleil => Tout
      p(i)%epz(sc%AOAS, :) = p(i)%epz(sc%AO, :)*p(i)%surf(sc%AO) + p(i)%epz(sc%AS, :)*p(i)%surf(sc%AS)
!        p%ep(sc%AOAS)       = p%ep(sc%AO) * p%surf(sc%AO) + p%ep(sc%AS) * p%surf(sc%AS)
!        p%turfac(sc%AOAS)   = min(p%turfac(sc%AO), p%turfac(sc%AS))
!        p%senfac(sc%AOAS)   = min(p%senfac(sc%AO), p%senfac(sc%AS))
!        p%swfac(sc%AOAS)    = min(p%swfac(sc%AO), p%swfac(sc%AS))
! DR 07/06/2012 erreur : pourquoi ce n'est pas sur la plante !!
      p(i)%ep(sc%AOAS) = p(i)%ep(sc%AO)*p(i)%surf(sc%AO) + p(i)%ep(sc%AS)*p(i)%surf(sc%AS)
      p(i)%turfac(sc%AOAS) = min(p(i)%turfac(sc%AO), p(i)%turfac(sc%AS))
      p(i)%senfac(sc%AOAS) = min(p(i)%senfac(sc%AO), p(i)%senfac(sc%AS))
      p(i)%swfac(sc%AOAS) = min(p(i)%swfac(sc%AO), p(i)%swfac(sc%AS))
   end do

! dr 01/09/2014 on cumule les epz pour verif
!      epz_cum2=sum(p(1)%epz(sc%AOAS,:))+sum(p(2)%epz(sc%AOAS,:))
!      write(*,*)n,'aprestranspi', epz_cum2
!      write(126,*)n,epz_cum, epz_cum2
!   print *,'avant offreN'
!::: Calcul des Offres - Offre en azote, absorption N, m�j de l'azote du sol
   do i = 1, sc%P_nbplantes

!#if DEBUG == 1
!        if (iand(sc%offren,1) >0) call offreN_debug_read_input(1450,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!        if (iand(sc%offren,2) >0) call offreN_debug_write_input(1451,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

      if (p(i)%masecdil(sc%AS) > 0.0) then

!#if DEBUG == 1
         if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: offreN'
!#endif
!  write(200,*)'avant offreN',sc%n,soil%nit(1:60)
         call offreN(p(i)%zrac, soil%nit(1:int(p(i)%zrac)), soil%amm(1:int(p(i)%zrac)), sc%hur(1:int(p(i)%zrac)), & ! les entr�es
                     sc%humin(1:int(p(i)%zrac)), sc%hucc(1:int(p(i)%zrac)), p(i)%flrac(1:int(p(i)%zrac)), &
                     pg%P_lvopt, pg%P_difN, p(i)%epz(sc%AOAS, 1:int(p(i)%zrac)), p(i)%P_Vmax1, p(i)%P_Kmabs1, p(i)%P_Vmax2, &
                     p(i)%P_Kmabs2, &
                     sc%cumoffrN, p(i)%flurac, p(i)%flusol, sc%offrN(1:int(p(i)%zrac)))                               ! les sorties
!   print *, 'offreN, i:',sc%n,", plant:",i,"N: ",soil%nit(1:int(p(i)%zrac))
! sc%offrN(1:int(p(i)%zrac)) = (/0.220789924,0.174929529,4.96065579E-02,6.14223070E-03,4.52531967E-03,5.20718470E-03,6.17036643E-03,&
! 7.23371655E-03,8.06049537E-03,8.20276514E-03,5.54003706E-03,5.84620936E-03,6.20882306E-03,6.64352067E-03,&
! 7.02813640E-03,7.33922515E-03,7.63079617E-03,7.92022888E-03,8.21161363E-03,8.66693072E-03,9.14046541E-03,&
! 9.41557717E-03,0.256428093,0.393907487,0.451421738,0.476077795/)
!#if DEBUG == 1
!          if (iand(sc%offren,4) >0) call offreN_debug_read_output(1452,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!          if (iand(sc%offren,8) >0) call offreN_debug_write_output(1453,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!          if (iand(sc%offren,16) >0) call offreN_debug_test_output(1454,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

         do ens = sc%AS, sc%AO

            !if (p(i)%lai(ens,n) > 0.) then
            if (p(i)%surf(ens) > 0.) then

!#if DEBUG == 1
               if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: absoN'
!              if (iand(sc%abson,1) >0) call absoN_debug_read_input(1460,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!              if (iand(sc%abson,2) >0) call absoN_debug_write_input(1461,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif
               cum_nitz = sum(soil%nit(1:sc%nbCouchesSol))
!   write(200,*)'avant absoN',sc%n,soil%nit(1:10),sc%absz(1:10)
               call absoN(p(i)%zrac, sc%nbCouchesSol, sc%cumoffrN, p(i)%surf(ens), sc%offrN(1:sc%nbCouchesSol), &
                          soil%nit(1:sc%nbCouchesSol), soil%amm(1:sc%nbCouchesSol), &
                          sc%absz(1:sc%nbCouchesSol), p(i)%demande(ens), p(i)%profextN, p(i)%abso(ens, n))
!   write(200,*)'apres absoN',sc%n,soil%nit(1:10),sc%absz(1:10)
               cum_absz = sum(sc%absz(1:sc%nbCouchesSol))
               cum_nitz = sum(soil%nit(1:sc%nbCouchesSol))

               ! if (sc%P_nbplantes == 1) then
               !     p(i)%abso(ens,n) = 2.5
               ! else
               !     p(i)%abso(ens,n) = 2.5 / 2.0
               ! endif
!#if DEBUG == 1
!              if (iand(sc%abson,4) >0) call absoN_debug_read_output(1462,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!              if (iand(sc%abson,8) >0) call absoN_debug_write_output(1463,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!              if (iand(sc%abson,16) >0) call absoN_debug_test_output(1464,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

!#if DEBUG == 1
               if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: majNsol'
!              if (iand(sc%majnsol,1) >0) call majNsol_debug_read_input(1470,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!              if (iand(sc%majnsol,2) >0) call majNsol_debug_write_input(1471,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

! 02/09/2014 DR et ML bug du bilan d'N non boucl� pour les CAS
! il faut ponderer l'azote absorb� par les surfaces � l'ombre et au soleil de chaque plante
! sinon on enleve plus d'azote du sol que la plante n'en absorbe
               sc%absz(1:sc%nbCouchesSol) = sc%absz(1:sc%nbCouchesSol)*p(i)%surf(ens)
!  write(200,*)'avant majNsol',sc%n,soil%nit(1:60)
               call majNsol(sc%nbCouchesSol, p(i)%zrac, sc%absz(1:sc%nbCouchesSol), &
                            soil%nit(1:sc%nbCouchesSol), soil%amm(1:sc%nbCouchesSol))
!   write(200,*)'apres majNsol',sc%n,soil%nit(1:60)
               cum_absz = sum(sc%absz(1:sc%nbCouchesSol))
               cum_nitz = sum(soil%nit(1:sc%nbCouchesSol))
!#if DEBUG == 1
!              if (iand(sc%majnsol,4) >0) call majNsol_debug_read_output(1472,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!              if (iand(sc%majnsol,8) >0) call majNsol_debug_write_output(1473,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!              if (iand(sc%majnsol,16) >0) call majNsol_debug_test_output(1474,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

            end if
         end do

      end if

   end do

!::: Calcul des Stress - hydrique, azot�, azote des grains, exc�s d'eau
   do i = 1, sc%P_nbplantes

      do ens = sc%AS, sc%AO

         !if (p(i)%lai(sc%AOAS,n) > 0.) then
         if (p(i)%surf(ens) > 0.) then

            ! Stress Eau

!#if DEBUG == 1
            if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: stressEau'
!            if (iand(sc%stressEau,1) >0) call stressEau_debug_read_input(1480,sc,pg,p,itk,soil,c,sta,t,i,ens)
!            if (iand(sc%stressEau,2) >0) call stressEau_debug_write_input(1481,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif
!   write(618,*)n,i,ens,p(i)%eop(ens)
!   print *,'avant stresseau'
            call stressEau(n, itk(i)%P_profsem, p(i)%zrac, sc%nbCouchesSol, sc%hur(1:sc%nbCouchesSol), & ! IN
                           sc%humin(1:sc%nbCouchesSol), p(i)%cumlracz, p(i)%surf(ens), p(i)%P_codelaitr, &
                           p(i)%lai(ens, n), sc%tauxcouv(n), p(i)%nrec, itk(i)%P_codcueille, p(i)%eop(ens), pg%P_rayon, &
                           p(i)%P_psisto, p(i)%P_psiturg, p(i)%P_rapsenturg, sc%supres, t%P_swfacmin, &
                           p(i)%resrac, p(i)%swfac(ens), p(i)%turfac(ens), p(i)%tetstomate, p(i)%teturg, p(i)%senfac(ens))        ! INOUT

!#if DEBUG == 1
!            if (iand(sc%stressEau,4) >0) call stressEau_debug_read_output(1482,sc,pg,p,itk,soil,c,sta,t,i,ens)
!            if (iand(sc%stressEau,8) >0) call stressEau_debug_write_output(1483,sc,pg,p,itk,soil,c,sta,t,i,ens)
!            if (iand(sc%stressEau,16) >0) call stressEau_debug_test_output(1484,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif

            ! Stress Azote

!#if DEBUG == 1
            if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: stressN'
!            if (iand(sc%stressN,1) >0) call stressN_debug_read_input(1490,sc,pg,p,itk,soil,c,sta,t,i,ens)
!            if (iand(sc%stressN,2) >0) call stressN_debug_write_input(1491,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif
!   print *,'avant stressN'
! DR et ML 02/09/2014
!            call stressN(p(i)%masecdil(ens),p(i)%abso(ens,n),p(i)%offrenod(ens),p(i)%QNplante(ens,n-1),            & ! IN
            call stressN(p(i)%masecdil(ens), p(i)%abso(ens, n), p(i)%offrenod(ens), p(i)%QNplante(0, n - 1), & ! IN
                         t%dltaremobilN(i, ens), p(i)%P_codelegume, p(i)%P_masec0, p(i)%P_adilmax, masecNmax_mean, &
                         p(i)%P_bdilmax, p(i)%masecpartiel(ens), magrain_abso, p(i)%P_codeplisoleN, &
                         p(i)%P_masecmeta, t%adilI(i), t%bdilI(i), p(i)%P_adil, p(i)%P_bdil, t%adilmaxI(i), t%bdilmaxI(i), &
                         p(i)%dNdWcrit, t%deltabso(i, ens), p(i)%P_codeINN, p(i)%P_INNmin, p(i)%P_INNimin, p(i)%P_innturgmin, &
                         p(i)%P_innsen, pg%P_QNpltminINN, &
                         p(i)%QNplante(ens, n), p(i)%Qfix(ens), p(i)%QNplanteres, p(i)%CNplante(ens), p(i)%inn(ens), &
                         t%inni(i, ens), p(i)%inns(ens), p(i)%innlai(ens), p(i)%innsenes(ens))                           ! INOUT

!#if DEBUG == 1
!            if (iand(sc%stressN,4) >0) call stressN_debug_read_output(1492,sc,pg,p,itk,soil,c,sta,t,i,ens)
!            if (iand(sc%stressN,8) >0) call stressN_debug_write_output(1493,sc,pg,p,itk,soil,c,sta,t,i,ens)
!            if (iand(sc%stressN,16) >0) call stressN_debug_test_output(1494,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif

         end if

      end do

      ! Cumuls AO/AS
      p(i)%QNplante(sc%aoas, sc%n) = p(i)%QNplante(sc%ao, sc%n)*p(i)%surf(sc%ao) &
                                     + p(i)%QNplante(sc%as, sc%n)*p(i)%surf(sc%as)
      p(i)%CNplante(sc%aoas) = p(i)%CNplante(sc%ao)*p(i)%surf(sc%ao) &
                               + p(i)%CNplante(sc%as)*p(i)%surf(sc%as)
! dr et ml 04/09/2014 on indexe sur ao et as pour les varaibles de l'abscission
      p(i)%QNplantetombe(sc%aoas) = p(i)%QNplantetombe(sc%ao)*p(i)%surf(sc%ao) &
                                    + p(i)%QNplantetombe(sc%as)*p(i)%surf(sc%as)
      p(i)%QCplantetombe(sc%aoas) = p(i)%QCplantetombe(sc%ao)*p(i)%surf(sc%ao) &
                                    + p(i)%QCplantetombe(sc%as)*p(i)%surf(sc%as)
!
      p(i)%Qfix(sc%aoas) = p(i)%Qfix(sc%ao)*p(i)%surf(sc%ao) + p(i)%Qfix(sc%as)*p(i)%surf(sc%as)

      ! RV: only take the minimum when a plant has two compartments (sunlit and shaded):
      if (p(i)%ipl .eq. 1) then
         p(i)%inn(sc%aoas) = p(i)%inn(sc%as)
      else
         p(i)%inn(sc%aoas) = min(p(i)%inn(sc%ao), p(i)%inn(sc%as))
      end if

      p(i)%inns(sc%aoas) = min(p(i)%inns(sc%ao), p(i)%inns(sc%as))
      p(i)%innsenes(sc%aoas) = min(p(i)%innsenes(sc%ao), p(i)%innsenes(sc%as))
      p(i)%innlai(sc%aoas) = min(p(i)%innlai(sc%ao), p(i)%innlai(sc%as))
      t%inni(p(i)%ipl, sc%aoas) = min(t%inni(p(i)%ipl, sc%ao), t%inni(p(i)%ipl, sc%as))

!#if DEBUG == 1
      if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: Ngrain'
      if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: Ngrain'
!        if (iand(sc%Ngrain,1) >0) call Ngrain_debug_read_input(1500,sc,pg,p,itk,soil,c,sta,t,i)
!        if (iand(sc%Ngrain,2) >0) call Ngrain_debug_write_input(1501,sc,pg,p,itk,soil,c,sta,t,i)
!#endif

      call Ngrain(n, p(i)%ndrp, p(i)%nrec, p(i)%P_vitirazo, p(i)%irazo(sc%AOAS, n - 1), p(i)%nmat, p(i)%dltags(sc%AOAS), &
                  p(i)%QNplante(sc%AOAS, n - 1), p(i)%QNplante(sc%AOAS, n), p(i)%pgrain(sc%AOAS), p(i)%nbgraingel, &
                  p(i)%P_irmax, p(i)%P_vitircarb, p(i)%magrain(sc%AOAS, n), &
                  p(i)%irazo(sc%AOAS, n), p(i)%QNgrain(sc%AOAS), p(i)%CNgrain(sc%AOAS))

!#if DEBUG == 1
!        if (iand(sc%Ngrain,4) >0) call Ngrain_debug_read_output(1502,sc,pg,p,itk,soil,c,sta,t,i)
!        if (iand(sc%Ngrain,8) >0) call Ngrain_debug_write_output(1503,sc,pg,p,itk,soil,c,sta,t,i)
!        if (iand(sc%Ngrain,16) >0) call Ngrain_debug_test_output(1504,sc,pg,p,itk,soil,c,sta,t,i)
!#endif

      ! finalement, on cumule pas puisqu'on a travaill� directement sur la partie cumul�e (AOAS)
      !p(i)%QNgrain(sc%AOAS,n) = p(i)%QNgrain(sc%AO,n) * p(i)%surf(sc%AO) + p(i)%QNgrain(sc%AS,n) * p(i)%surf(sc%AS)
      !p(i)%irazo(sc%aoas,sc%n)  = (p(i)%irazo(sc%ao,sc%n) + p(i)%irazo(sc%as,sc%n)) / min(2,sc%P_nbplantes)

! TODO : Ngrain est appel� 1 fois par plante, pour les deux parties AO et AS.
! Or, il calcul des variables qui sont s�par�es AO et AS, comme QNgrain, irazo et CNgrain.
! Pour QNgrain, il n'y pas d'incidence dans la version actuelle, car il n'est utilis�
! que pour sa partie AS. Mais CNgrain est utilis� pour AO et AS, il est m�me calcul�
! dans eauqual et dans grain. Il faudrait v�rifier qu'il n'y a pas d'effet de bord.
! Ici, j'affecte en fait la partie commune AOAS, plutot que chacune des 2 composantes.
! TODO : peut �tre qu'il vaudrait mieux finalement travailler sur la partie AS et laisser
!        le cumul AO/AS faire son travail ?
!        Parce qu'ici en r�affectant les composantes AO et AS, on risque de perdre des valeurs calcul�es pr�c�demment.

! Dans le doute, j'affecte les composantes AO et AS aussi des sorties de Ngrain
      p(i)%CNgrain(sc%AO) = p(i)%CNgrain(sc%AOAS)
      p(i)%CNgrain(sc%AS) = p(i)%CNgrain(sc%AOAS)

      p(i)%irazo(sc%AO, sc%n) = p(i)%irazo(sc%AOAS, sc%n)
      p(i)%irazo(sc%AS, sc%n) = p(i)%irazo(sc%AOAS, sc%n)

      p(i)%QNgrain(sc%AO) = p(i)%QNgrain(sc%AOAS)
      p(i)%QNgrain(sc%AS) = p(i)%QNgrain(sc%AOAS)

      ! Stress Exc�s d'eau

!#if DEBUG == 1
      if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: excesdeau'
!        if (iand(sc%excesdeau,1) >0) call excesdeau_debug_read_input(1510,sc,pg,p,itk,soil,c,sta,t,i,ens)
!        if (iand(sc%excesdeau,2) >0) call excesdeau_debug_write_input(1511,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif

      call excesdeau(n, p(i)%cumflrac, p(i)%rltot, p(i)%P_sensanox, p(i)%racnoy(n - 1), & ! entr�es
                     p(i)%racnoy(n), p(i)%exofac, p(i)%izrac, p(i)%idzrac, & ! sorties
                     p(i)%exolai, p(i)%exobiom)

!#if DEBUG == 1
!        if (iand(sc%excesdeau,4) >0) call excesdeau_debug_read_output(1512,sc,pg,p,itk,soil,c,sta,t,i,ens)
!        if (iand(sc%excesdeau,8) >0) call excesdeau_debug_write_output(1513,sc,pg,p,itk,soil,c,sta,t,i,ens)
!        if (iand(sc%excesdeau,16) >0) call excesdeau_debug_test_output(1514,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif

   end do

!::: MicroClimat
   ! le calcul de temperature s'effectue une seule fois pour l'ensemble des systemes plantes
   !--  ipl = 1

   ! *!* ATTENTION : ICI IL FAUT SOMMER LES EP DES PLANTES
   ! *!* TEMPSOL UNIQUE POUR TOUTES LES CULTURES
   sc%eptcult = 0.0
   do i = 1, sc%P_nbplantes
      sc%eptcult = sc%eptcult + p(i)%ep(sc%AS) + p(i)%ep(sc%AO)
   end do

   sc%hauteurMAX = 0.0
   do i = 1, sc%P_nbplantes
      sc%hauteurMAX = max(sc%hauteurMAX, p(i)%hauteur(sc%AS))
      sc%hauteurMAX = max(sc%hauteurMAX, p(i)%hauteur(sc%AO))
   end do

   sc%EmdTot = 0.0
   do i = 1, sc%P_nbplantes
      sc%EmdTot = sc%EmdTot + p(i)%Emd
   end do

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: caltcult'
!      if (iand(sc%caltcult_sj,1) >0) call caltcult_sj_debug_read_input(1520,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%caltcult_sj,2) >0) call caltcult_sj_debug_write_input(1521,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

   ! TODO: transmettre le jour julien modulo nbjsemis => jjul ou jul ? JUL !
   ! TODO: dans les initialisations, affecter albedomulch lu dans param.par � l'itk de la plante sous condition (P_codepaillage /= 2)
! DR 08/02/2010 je propose d'appeller caltcult avce le bon albedomulch

   if (itk(1)%P_codepaillage == 2) then   !! paillage mulch plastique
      call caltcult(itk(1)%P_codabri, sc%EmdTot, sc%hauteurMAX, sc%eptcult, sc%esol, sc%Emulch, soil%P_z0solnu, c%tmax(n), &
                    c%tvent(n), sc%raamin, sc%rnetS, p(1)%P_codebeso, c%phoi, sc%raamax, &
                    sc%tcult, sc%tcultmin, sc%tcultmax, sc%et, c%tutilrnet, sc%numdate, c%daylen, c%difftcult, &
                    c%nitetcult(n), sc%Ratm, &
                    sc%nbCouchesSol, sc%jul, sc%hur(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), &
                    sc%hucc(1:sc%nbCouchesSol), sc%laiTot, sc%tauxcouv(n), sc%posibsw, soil%P_albedo, &
                    itk%P_couvermulchplastique, itk(1)%P_albedomulchplastique, sta%P_albveg, sta%P_codernet, sta%P_aangst, &
                    sta%P_bangst, c%tmin(n), c%tmoy(n), sta%P_corecTrosee, c%trg(n), sta%P_latitude, itk(1)%P_codepaillage, &
                    p(1)%P_codelaitr, c%tpm(n), sta%P_codecaltemp, sc%albedolai, sc%rglo, sc%rnet)
   else
      call caltcult(itk(1)%P_codabri, sc%EmdTot, sc%hauteurMAX, sc%eptcult, sc%esol, sc%Emulch, soil%P_z0solnu, c%tmax(n), &
                    c%tvent(n), sc%raamin, sc%rnetS, p(1)%P_codebeso, c%phoi, sc%raamax, &
                    sc%tcult, sc%tcultmin, sc%tcultmax, sc%et, c%tutilrnet, sc%numdate, c%daylen, c%difftcult, &
                    c%nitetcult(n), sc%Ratm, &
                    sc%nbCouchesSol, sc%jul, sc%hur(1:sc%nbCouchesSol), sc%humin(1:sc%nbCouchesSol), &
                    sc%hucc(1:sc%nbCouchesSol), sc%laiTot, sc%tauxcouv(n), sc%posibsw, soil%P_albedo, &
                    sc%couvermulch, pg%P_albedomulchresidus(sc%irmulch), sta%P_albveg, sta%P_codernet, sta%P_aangst, sta%P_bangst, &
                    c%tmin(n), c%tmoy(n), sta%P_corecTrosee, c%trg(n), sta%P_latitude, itk(1)%P_codepaillage, p(1)%P_codelaitr, &
                    c%tpm(n), sta%P_codecaltemp, sc%albedolai, sc%rglo, sc%rnet)
   end if

!#if DEBUG == 1
!      if (iand(sc%caltcult_sj,4) >0) call caltcult_sj_debug_read_output(1522,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%caltcult_sj,8) >0) call caltcult_sj_debug_write_output(1523,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%caltcult_sj,16) >0) call caltcult_sj_debug_test_output(1524,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: tempsol'
!      if (iand(sc%tempsol,1) >0) call tempsol_debug_read_input(1530,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%tempsol,2) >0) call tempsol_debug_write_input(1531,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

   call tempsol(sc%tcultmin, sc%tcultmax, pg%P_diftherm, soil%profsol, & ! entr�es
                sc%tsolveille(1:int(soil%profsol)), sc%tcultveille, c%tmin(n), &
                sc%tsol(1:int(soil%profsol)))                                  ! sorties

   sc%tsolveille(1:int(soil%profsol)) = sc%tsol(1:int(soil%profsol))
   ! PB - 29/04/2004 - pour �viter des bugs qd zrac = 0
   sc%tsol(0) = sc%tsol(1)

!#if DEBUG == 1
!      if (iand(sc%tempsol,4) >0) call tempsol_debug_read_output(1532,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%tempsol,8) >0) call tempsol_debug_write_output(1533,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%tempsol,16) >0) call tempsol_debug_test_output(1534,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

   sc%tcultveille = sc%tcult

   ! rajout domi du 23/04/97 : pour homogeneiser avec la temp de surface on calcule
   ! les sommes de temp du developpement et le reste avec la temp air de la veille
   sc%tairveille = c%tmoy(n)

   ! inactivation de l'effet tcult sur  les plantes si P_codeh2oact = 0
   if (pg%P_codeh2oact == 0) sc%tcult = c%tmoy(n)

   sc%epTot = 0.0
   do i = 1, sc%P_nbplantes
      sc%epTot = sc%epTot + p(i)%ep(sc%AS) + p(i)%ep(sc%AO)
   end do

   if (itk(1)%P_codabri /= 2) then
      sc%et = sc%esol + sc%EmdTot + sc%epTot
   end if

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: humcouv'
!      if (iand(sc%humcouv_sj,1) >0) call humcouv_sj_debug_read_input(1540,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%humcouv_sj,2) >0) call humcouv_sj_debug_write_input(1541,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

   ! NB le 26/01/05 intro de P_patm
   call humcouv(c%tmoy(n), sc%tcult, c%tpm(n), sta%P_ra, sc%Edirect, 0, sc%epTot, sc%rnet, sta%P_patm, sc%humidite, c%humair)

!#if DEBUG == 1
!      if (iand(sc%humcouv_sj,4) >0) call humcouv_sj_debug_read_output(1542,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%humcouv_sj,8) >0) call humcouv_sj_debug_write_output(1543,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!      if (iand(sc%humcouv_sj,16) >0) call humcouv_sj_debug_test_output(1544,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

   if (pg%P_codemicheur == 1) then

      ! NB le 08/08/05 oubli de calcul de numdate
      ! TODO : on calcule la photop�riode syst�matiquement avant la phase de d�veloppement de la plante
      !        on doit pouvoir supprimer le calcul ici.
      sc%numdate = sc%jul

!#if DEBUG == 1
      if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: photpd'
!#endif

      call photpd(sta%P_latitude, sc%numdate, c%daylen, c%phoi)

!#if DEBUG == 1
      if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: humheure'
!        if (iand(sc%humheure,1) >0) call humheure_sj_debug_read_input(1550,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!        if (iand(sc%humheure,2) >0) call humheure_sj_debug_write_input(1551,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

      ! PB - 27/07/2009
      ! Quand n=1, on essaie de lire tmin(0) qui n'existe pas, pour �viter �a, on sort les deux lignes suivantes de humheure.
      ! TODO : attention, si n = nbjmax, on risque de lire tmin(nbjmax+1) => d�bordement !
      if (n > 1) tmaxveille = sc%tcultveille*2 - c%tmin(n - 1)
      if (n == 1) tmaxveille = sc%tcultmax

!: ML - 29/10/12 - calcul dur�e d'humectation: ajout de l'incrementation de compteurhumheure
!: et des arguments suppl�mentaires dans l'appel � la fonction humheure
!if (t%P_codeSWDRH.eq.2) c%compteurhumheure = 2

!        call humheure(n,sc%tcultmin,sc%tcultmax,tmaxveille,sc%humidite,c%daylen,c%tmin(n+1),  & ! entr�es
!                      sc%tcult,sc%trosemax(0:n),c%humimoy,c%tmin(n),c%dureehumec,c%dureeRH,c%dureeRH1, &  ! sorties
!                   c%dureeRH2,c%compteurhumheure,c%trr(n))

      call humheure(n, sc%tcultmin, sc%tcultmax, tmaxveille, sc%humidite, c%daylen, c%tmin(n + 1), sc%tcult, &
                    sc%trosemax(0:n), c%humimoy, c%tmin(n), t%P_codetrosee, tculth, troseh, humh)

!        c%compteurhumheure = 0

!: ML - fin

!#if DEBUG == 1
!        if (iand(sc%humheure,4) >0) call humheure_sj_debug_read_output(1552,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!        if (iand(sc%humheure,8) >0) call humheure_sj_debug_write_output(1553,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!        if (iand(sc%humheure,16) >0) call humheure_sj_debug_test_output(1554,sc,pg,p,itk,soil,c,sta,t,ipl,ens)
!#endif

   end if

!#if DEBUG == 1
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'Stics_Jour: cumAOetAS'
!#endif

!::: Cumul AO/AS
   if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg2, *) '  '
   do i = 1, sc%P_nbplantes
      if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg2, *) 'cum avant', n, i, p(i)%nbgrains(0) &
         , p(i)%magrain(0, n), p(i)%chargefruit
      call cumAOetAS(n, sc%P_codesimul, p(i), itk(i), c%trg(n)*pg%P_parsurrg)
      if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) write (sc%ficdbg2, *) 'cum apres', n, i, p(i)%nbgrains(0) &
         , p(i)%magrain(0, n), p(i)%chargefruit
   end do

!::: Fonction d'�criture des sorties journali�res
   ! on r�initialise le nombres des apports (irrig et fertil) pour les recalculer
   sc%naptot = 0.
   sc%napNtot = 0.
   ! domi le 20/07/04 on calcul naptot et napNtot comme le cumul des 2 plantes
   sc%napNtot = SUM(itk(:)%napN)
   sc%naptot = SUM(itk(:)%nap)

!#if DEBUG == 1
!if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: stics_jour_after'
!#endif

   !TODO: trouver un meilleur libell� pour cette routine.
!            if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'avant stics_jour_after',n,&
!            'nbgrains',p(1)%nbgrains(0),p(2)%nbgrains(0),p(1)%chargefruit,p(2)%chargefruit

! DR et FR 31/05/2016 on se demande bien pourquyoi j'ai fait ca , je begaye , faut que je parte � la retraite
! if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: gestion des coupes'
!      call GestionDesCoupes(sc,pg,c,sta,soil,p,itk,t)

   call Stics_Jour_after(sc, pg, c, sta, soil, p, itk, t)
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'apres stics_jour_after', n, &
      'nbgrains', p(1)%nbgrains(0), p(2)%nbgrains(0), p(1)%chargefruit, p(2)%chargefruit

!write(*,*)'avant gestion',sc%n,p(1)%lai(0,sc%n)
!::: Gestion des coupes
!if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)print *,'Stics_Jour: gestion des coupes'
   call GestionDesCoupes(sc, pg, c, sta, soil, p, itk, t)

! ****************************************************************
! DR 23/09/2016 on met l'appel aux sorties journalieres la plutot que dans stics_jour_after sinon incompatibilit� avce Record
   do i = 1, sc%P_nbplantes
      ! DR 13/01/06 pour simplifier les sorties je mets tauxrecouv ou lai(aoas) dans une var temporaire
      ! taux de couverture uniquement pour culture pure, donc p(1).
      if (p(1)%P_codelaitr /= 1) then
         p(1)%lai(sc%aoas, n) = sc%tauxcouv(n)
      end if
      if (iand(pg%P_flagEcriture, sc%ECRITURE_SORTIESJOUR) > 0) then
!if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)write(sc%ficdbg2,*)'n',n,p(i)%cep,p(i)%ep(1)
! DR 23/07/2012 on supprime itk qui ne sert pas 20/09/2012 t aussi
         call Ecriture_VariablesSortiesJournalieres(sc, pg, p(i), soil, c, sta)
!if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)write(sc%ficdbg2,*)'n',n,p(i)%cep,p(i)%ep(1)
      end if
! DR 07/09/2011 j'joute la possibilit� d'ecrire dans un fichier st3 pour avoir le vrai format agmip
! DR 29/08/2012 j'ajoute un code pour garder les sorties Agmip t%P_Config_Output : 1=rien , 2 sorties ecran , 3 sorties agmpi
      if (iand(pg%P_flagEcriture, sc%ECRITURE_AGMIP) > 0) then
         !if (iand(pg%P_flagEcriture,sc%ECRITURE_SORTIESJOUR) >0 ) then
         ! DR 19/07/2012 on supprime itk qui ne sert pas 20/09/2012 t aussi
         call Ecriture_VariablesSortiesJournalieres_st3(sc, pg, p(i), soil, c, sta, t)
         !endif
      end if
   end do
!**********************************************************************************

!write(*,*)'apres gestion',sc%n,p(1)%lai(0,sc%n)

!::: Dynamique des talles
   ! introduction de la dynamique des talles
   ! NB le 08/03/07
   ! TODO: v�rifier qu'il s'agit bien de passer deltai(n)
   do i = 1, sc%P_nbplantes

      ! DR et ML et SYL 15/06/09
      ! ************************
      ! introduction de la fin des modifications de Sylvain (nadine et FR)
      ! dans le cadre du projet PERMED
      ! on supprime la condition sur P_codeplante='fou' et on indexe P_codedyntalle sur la plante
      if (t%P_codedyntalle(i) == 1) then
         ! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain
!#if DEBUG == 1
         if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: dynamique des talles'
!#endif
! DR et ML et SYL 15/06/09
! ************************
! introduction de la fin des modifications de Sylvain (nadine et FR)
! dans le cadre du projet PERMED
!       if(P_codetranspitalle.eq.1) then
!            call dynamictalle(P_SurfApex,P_SeuilMorTalle,P_SigmaDisTalle,
!     s     P_VitReconsPeupl,P_SeuilReconsPeupl,P_MaxTalle,densite(ipl),
!     s        et,etm,LAIapex(ipl),P_SeuilLAIapex,tempeff(ipl)
!     s     ,mortalle(ipl),laiC(ipl,n)-laiC(ipl,n-1),resperenneC(ipl)
!     s        ,mortreserve(ipl),n,deltaiC(ipl),laiC(ipl,n))c
!       else
!           call dynamictalle(P_SurfApex,P_SeuilMorTalle,P_SigmaDisTalle,
!     s     P_VitReconsPeupl,P_SeuilReconsPeupl,P_MaxTalle,densite(ipl),
!     s        epc2(ipl),eopC(ipl),LAIapex(ipl),P_SeuilLAIapex,tempeff(ipl)
!     s     ,mortalle(ipl),laiC(ipl,n)-laiC(ipl,n-1),resperenneC(ipl)
!     s        ,mortreserve(ipl),n,deltaiC(ipl),laiC(ipl,n))
!              if(densite(ipl).lt.1.0) then
!               write(fichist,*)'Peuplement de talles mort le jour ',n
!               write(*,*) 'Peuplement de talles mort le jour ',n
!               stop
!           endif
!       endif
!

         if (t%P_codetranspitalle == 1) then
            call dynamictalle(t%P_SurfApex(i), t%P_SeuilMorTalle(i), t%P_SigmaDisTalle(i), &
                              t%P_VitReconsPeupl(i), t%P_SeuilReconsPeupl(i), t%P_MaxTalle(i), &
                              p(i)%densite, sc%et, sc%etm, t%LAIapex(i), t%P_SeuilLAIapex(i), &
                              p(i)%tempeff, p(i)%mortalle, &
                              p(i)%lai(sc%AOAS, n) - p(i)%lai(sc%AOAS, n - 1), &
                              p(i)%resperenne(sc%AOAS), p(i)%mortreserve, &
                              p(i)%deltai(sc%AOAS, n), p(i)%lai(sc%AOAS, n), &
                              p(i)%densitemax, p(i)%masec(0, n), p(i)%mortmasec, &
                              p(i)%drlsenmortalle)
         else
            call dynamictalle(t%P_SurfApex(i), t%P_SeuilMorTalle(i), t%P_SigmaDisTalle(i), &
                              t%P_VitReconsPeupl(i), t%P_SeuilReconsPeupl(i), t%P_MaxTalle(i), &
                              p(i)%densite, p(i)%ep(sc%AOAS), p(i)%eop(sc%AOAS), &
                              t%LAIapex(i), t%P_SeuilLAIapex(i), p(i)%tempeff, p(i)%mortalle, &
                              p(i)%lai(sc%AOAS, n) - p(i)%lai(sc%AOAS, n - 1), &
                              p(i)%resperenne(sc%AOAS), p(i)%mortreserve, &
                              p(i)%deltai(sc%AOAS, n), p(i)%lai(sc%AOAS, n), &
                              p(i)%densitemax, p(i)%masec(0, n), p(i)%mortmasec, &
                              p(i)%drlsenmortalle)
            if (p(i)%densite < 1.0) then
               call EnvoyerMsgHistorique(164, n)
               !--  stop
            end if
! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain
         end if
      end if
   end do
! print *,'end day ',n
   if (iand(pg%P_flagEcriture, sc%ECRITURE_ECRAN) > 0) print *, 'end day ', n

! 535  continue
!   if (n == 1) then
!   write(68,'(a130)') 'n    densite    et    etm    LAIC    LAIapex
!     s  tempeff    mortalle    swfacc    laisenc(ipl,n)    tdevelop   ep
!     s   eop    resperenne'
!   endif
!   write(68,535) n,densite,et,etm,LAIC(ipl,n),LAIapex,
!     s    tempeff,mortalle,swfacc,laisenc(ipl,n),
!     s        tdevelop(ipl,n),epc2,eopC,resperenneC
!535   format(i6,13f10.3)
!     if(n==1)then
!             do kkk=1,5
!             write(71,*)'fin jour 1',i,soil%P_epc(kkk),sc%HR(kkk),soil%AZnit(kkk),sc%AZamm(kkk)
!             enddo
!        write(71,*)'fin jour 1',sc%esol,sc%et
!     endif

   !::: Ecriture des rapports
   if (iand(pg%P_flagEcriture, sc%ECRITURE_RAPPORTS) > 0) then
      if (iand(pg%P_flagEcriture, sc%ECRITURE_DEBUG) > 0) print *, 'Stics_Jour: reports'

      do i = 1, sc%P_nbplantes

         ! ecriture de rapport.sti aux dates ou aux stades choisis
         ! domi 24/09/03 pb dans l'ecriture � des stades
         if (sc%codeaucun == 1) then
            ! domi - 07/10/03 - pour vianney stades+dates
            ! domi - 20/10/03 - je teste pour ne pas ecrire 2 fois le meme jour
            sc%ecritrap = .FALSE.

            if (sc%codetyperap == 1 .or. sc%codetyperap == 3) then
               do iz = 1, sc%nboccurrap
                  call NDATE(sc%date_calend_rap(iz, 3), sc%date_calend_rap(iz, 2), sc%date_calend_rap(iz, 1), sc%daterap(iz))
                  ! DR 10/03/2014 la gestion des dates calendaire dans le rapport avait ete fait un peu vite , y'a pb  dans le cas ou l'usms est sur 2 ans ,
                  ! du coup le jour julien de la date de la deuxieme annee est mauvais
                  ! comme a pas acces � l'annee ni � quoi que ce soit � ce stade du programme (dans lecture_variable_rapport) , je mets ici la conversion
                  if (sc%ansemis .ne. sc%date_calend_rap(iz, 1)) then
                     if (isBissextile(sc%ansemis)) then
                        sc%daterap(iz) = sc%daterap(iz) + 366
                     else
                        sc%daterap(iz) = sc%daterap(iz) + 365
                     end if
                  end if
                  if ((n == sc%daterap(iz) - sc%P_iwater + 1) .and. (sc%ecritrap .eqv. .FALSE.)) &
                     call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t) ! DR 19/07/2012 on supprime itk qui ne sert pas
               end do
               !-- if ((n == (daterap(ii)-P_iwater+1)) .and. (ecritrap.eqv..FALSE.)) call Ecriture_Rapport(sc,pg,soil,c,sta,p(i),itk(i),t)
            end if

            if ((sc%codetyperap == 2 .or. sc%codetyperap == 3) .and. (sc%ecritrap .eqv. .FALSE.)) then
               if ((p(i)%nplt == n) .and. sc%rapplt) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t) ! DR 19/07/2012 on supprime itk qui ne sert pas
               if ((p(i)%nger == n) .and. sc%rapger) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               if ((p(i)%nlev == n) .and. sc%raplev) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               if ((p(i)%namf == n) .and. sc%rapamf) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               if ((p(i)%nlax == n) .and. sc%raplax) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               if ((p(i)%ndrp == n) .and. sc%rapdrp) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               if ((p(i)%nflo == n) .and. sc%rapflo) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               if ((p(i)%nsen == n) .and. sc%rapsen) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               if ((p(i)%nrec == n) .and. sc%raprec) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
! dr au brazil le 05/08/2010 !!!manquait maturit� zut alors ...
!              if (p(i)%nmat == n) write(*,*)'=====',p(i)%nmat
               if ((p(i)%nmat == n) .and. sc%rapmat) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               if ((p(i)%ndebdorm == n) .and. sc%rapdebdorm) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               if ((p(i)%nfindorm == n) .and. sc%rapfindorm) call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
               ! dr 12/08/08 pour les perennes le stade debourrement est la levee
               if ((p(i)%P_codeperenne == 2) .and. (p(i)%nlev == n) .and. sc%rapdebdebour) &
                  call Ecriture_Rapport(sc, pg, soil, c, sta, p(i), t)
            end if

         end if

      end do

   end if

   !        write(200,*)'fin jour',sc%n,soil%nit(1:60)
!: Mise � z�ro de duree_humec et compteurhumheure
! dr 17/10/2013 compteurhumheure n'est plus utilis� et dureehumec ets remis � zero dans SWDRH.f90
!: ML - fin
!endif

   ! pour forcer l'affichage du buffer d'�criture sur l'�cran de sortie au jour le jour (STDOUT=6)
!      call flush(6)

end subroutine Stics_Jour
