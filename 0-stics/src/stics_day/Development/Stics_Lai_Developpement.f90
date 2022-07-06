!*-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
! - Stics book paragraphe 3.1.1, page 40-44
! - In STICS, leaf area growth is driven by phasic development, temperature and stresses. An empirical plant density-dependent function represents inter-plant
!! competition. For indeterminate plants, trophic competition is taken into account through a trophic stress index, while for determinate plants a maximal
!! expansion rate threshold is calculated to avoid unrealistic leaf expansion.
!! The calculation of leaf growth rate (deltai in m2m-2 d-1) is broken down: a first calculation of the LAI growth rate (in m2 plant-1 degree-day-1) describes
!! a logistic curve, related to the ILEV, IAMF and ILAX phenological stages. This value is then multiplied by the effective crop temperature, the plant density
!! combined with a density factor, supposed to stand for inter-plant competition, that is characteristic for the variety, and the water and nitrogen stress indices.
!!
!! The phasic development function is comparable to that of the model PUTU (Singels and Jagger, 1991), i.e. a logistic function with dlaimaxbrut as asymptote
!! and pentlaimax as the slope at the inflection point. It is driven by a normalized leaf development unit (ULAI) equal to 1 at ILEV and 3 at ILAX.
!! At the end of the juvenile stage (IAMF), it is equal to vlaimax when the inflection of the dynamics (point of maximal rate) also occurs.
!! Between the stages ILEV, IAMF and ILAX, the model performs linear interpolation based on development units (upvt) which include all the environmental effects
!! on phasic development. As the ILAX stage approaches, it is possible to introduce a gradual decline in growth rate using the udlaimax parameter
!! - the ULAI value beyond which there is a decline in the leaf growth rate. If udlaimax=3 it has no effect and the leaf stops growing at once at ILAX.
!!
!! The thermal function relies on crop temperature and cardinal temperatures (tcmin and tcmax) which differ from the ones used for the phasic development.
!! The extreme threshold tcxstop is the same as for development.
!!
!! The density function is active solely after a given LAI threshold occurs (laicomp parameter) if the plant density (densite in plant m-2 possibly decreased
!! by early frost) is greater than the bdens threshold, below which plant leaf area is assumed independent of density.  Beyond this density value, leaf area
!! per plant decreases exponentially.  The adens parameter represents the ability of a plant to withstand increasing densities.  It depends on the species
!! and may depend on the variety.  For branching or tillering plants, adens represents the plant�s branching or tillering ability (e. g. wheat or pea).
!! For single-stem plants, adens represents competition between plant leaves within a given stand (e.g. maize or sunflower).
!!
!! Water and nitrogen affect leaf growth as limiting factors, i.e. stress indices whose values vary between 0 and 1. Water (turfac) and nitrogen deficits (innlai)
!! are assumed to interact, justifying the use of the more severe of the two stresses. Meanwhile at the whole plant level the water-logging stress index is assumed
!! to act independently
!
!
!
! -	Features of determinate crops
!!   Failure to account for trophic aspects in the calculation of leaf growth may cause problems when the radiation intercepted by the crop is insufficient to
!!   ensure leaf expansion (e.g. for crops under a tree canopy or crops growing in winter).  Consequently, from the IAMF stage, we have introduced a trophic effect
!!  to calculate the definitive LAI growth rate in the form of a maximum threshold for leaf expansion (deltaimaxi in m2m-2d-1) using the notion of the maximum
!!   leaf expansion allowed per unit of biomass accumulated in the plant (sbvmax in cm2 g-1) and the daily biomass accumulation (dltams in t.ha-1day-1 possibly
!!   complemented by remobilized reserve remobilj). sbvmax is calculated using the slamax and tigefeuil parameters.
!!
! -	Features of indeterminate crops
!!   It has been possible to test the robustness of the above formalisation on a variety of crops, including crops where there is an overlap between the
!!   vegetative phase and the reproductive phase (soybean and flax for example).  However, when trophic competition between leaves and fruits is a driving force
!!   for the production and management of the crop (for example tomato, sugarbeet), this formalisation is unsuitable.  We therefore calculate the deltai variable
!!   so as to take trophic monitoring into consideration in the case of crops described as �indeterminate�, by introducing a trophic stress index (splai).
!!   As a consequence, the LAI can decrease markedly during the theoretical growth phase if the crop is experiencing severe stresses during the harvested
!!   organs filling phase.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
!TODO: renommer cette routine plus explicitement
! DR 23/07/2012 sta est inutilis�
! subroutine laidev(sc,p,pg,itk,t,soil,c,sta)
subroutine laidev(sc,p,pg,itk,t,soil,c)

  USE Stics
  USE Plante
  USE Itineraire_Technique
  USE Sol
  USE Climat
  USE Station
  USE Parametres_Generaux
  USE USM
  USE Developpement
  !USE debug

  implicit none

  type(Stics_Communs_),             intent(INOUT) :: sc
  type(Plante_),                    intent(INOUT) :: p(sc%P_nbplantes)
  type(ITK_),                       intent(INOUT) :: itk(sc%P_nbplantes)
  !type(USM_), intent(IN) :: usmc
  type(Parametres_Generaux_),       intent(INOUT) :: pg
  type(Stics_Transit_),             intent(INOUT) :: t
  type(Sol_),                       intent(INOUT) :: soil
  type(Climat_),                    intent(INOUT) :: c
!  type(Station_),                   intent(INOUT) :: sta


  ! variables locales
  integer :: i ! indice de plante
  integer :: ens ! ensoleillement AO/AS

! Pour les r�sidus du rognage
  real :: Cfeupc
  real :: CNrogne
  real :: Crogne ! Bruno-ajout variables locales C et N rogne
  real :: Nrogne
  real :: Wr !ajout calcul des param�tres de d�composition du r�sidu "feuilles rognees"

    do i = 1, sc%P_nbplantes


      ! on pourrait utiliser un pointeur pour raccourcir les �critures.
      ! Ex : p = plante(i). Et au lieu de p(i), on aurait juste p.

      if (lge(sc%P_codesimul,'feuille') .eqv. .TRUE.) then
          call recalcullevee(sc%n,p(i)%nlev,p(i)%nlevobs,p(i)%nlaxobs,p(i)%lai(1:2,1:sc%n+1), &
                             sc%tauxcouv(1:sc%n+1),p(i)%P_codelaitr,p(i)%estDominante)
      endif

      if (p(i)%P_codeplante /= 'snu') then

        sc%ens = sc%AS


! ::: Developpement de la plante

!#if DEBUG == 1
!        if (iand(sc%develop,1) >0) call develop_debug_read_input(1230,sc,pg,p,itk,soil,c,sta,t,i)
!        if (iand(sc%develop,2) >0) call develop_debug_write_input(1231,sc,pg,p,itk,soil,c,sta,t,i)
!#endif
        ! DR 23/07/2012 sta est inutilis�
!        call develop(sc,p(i),itk(i),soil,c,t,pg,sta)
        call develop(sc,p(i),itk(i),soil,c,t,pg)

! DR 12/09/2012 on pense que il y avait un pb sur le calcul de deltalai qui pour les CAS etait calcul� � partir de la densite equivalente
! cette densite equivalente ne doit etre utilis�e que pour le calcul de efdensite (en cas de cas et pour la plante 2 ...)
        ! si les 2 plantes sont arriv�es � levee on recalcule la densite equivalente
        if(sc%P_nbplantes.gt.1)then
          ! DR et ML 21/04/2016 le calcul de la densite equivalente est refait tous les jours � partir de le lev�e des 2 plantes
          ! de facon a prendre en compte les inversions de dominances et les modifications de densite ( gel )
          !     if((p(1)%nlev>0 .and. p(2)%nlev>0) .and. (p(1)%nlev==sc%n.or.p(2)%nlev==sc%n))then
          if((p(1)%nlev>0 .and. p(2)%nlev>0) )then
            if (p(1)%nplt > p(2)%nplt) then
              call EnvoyerMsgHistorique(25)
            endif

            ! RV: the plant equivalent density is used to compute the intra-species competition (:efdensite). It should always be equal to the density
            ! the crop would have in a sole crop, that is to say, twice its sowing density in the intercrop. The inter-species competition is taken
            ! into account using the light interception module + the elongation function.
            ! NB: It is twice because the interrow parameter is between plants of the same species, so twice the "true" interrow.
            p(1)%densiteequiv = p(1)%densite*2
            p(2)%densiteequiv = p(2)%densite*2
          endif
        endif
        ! si on a une seule plante on reaffecte la densite equivalente au cas ou il y aurait eu une modif sur la densite courante (gel ou autre )
        if(sc%P_nbplantes.eq.1) p(1)%densiteequiv= p(1)%densite

      ! on d�compose les valeurs AOAS calcul�es dans develop en AO et AS
        p(i)%rdtint(sc%AO,p(i)%nbrecolte-1) = p(i)%rdtint(sc%AOAS,p(i)%nbrecolte-1) * p(i)%surf(sc%AO)
        p(i)%rdtint(sc%AS,p(i)%nbrecolte-1) = p(i)%rdtint(sc%AOAS,p(i)%nbrecolte-1) * p(i)%surf(sc%AS)

        p(i)%nfruit(sc%AO,p(i)%P_nboite) = p(i)%nfruit(sc%AOAS,p(i)%P_nboite) * p(i)%surf(sc%AO)
        p(i)%nfruit(sc%AS,p(i)%P_nboite) = p(i)%nfruit(sc%AOAS,p(i)%P_nboite) * p(i)%surf(sc%AS)

        p(i)%pdsfruit(sc%AO,p(i)%P_nboite) = p(i)%pdsfruit(sc%AOAS,p(i)%P_nboite) * p(i)%surf(sc%AO)
        p(i)%pdsfruit(sc%AS,p(i)%P_nboite) = p(i)%pdsfruit(sc%AOAS,p(i)%P_nboite) * p(i)%surf(sc%AS)

!#if DEBUG == 1
!        if (iand(sc%develop,4) >0) call develop_debug_read_output(1232,sc,p,itk,i)
!        if (iand(sc%develop,8) >0) call develop_debug_write_output(1233,sc,p,itk,i)
!        if (iand(sc%develop,16) >0) call develop_debug_test_output(1234,sc,p,itk,i)
!#endif

      ! dr - 17/11/05: pb de recolte
      ! dr et ml - 06/12/05 : c'etait idiot pour les perennes de forcer la recolte des 2 cultures en meme temps!!
        if (itk(i)%P_coderecolteassoc == 1) then
          if (p(i)%nrec /= 0 .and. ALL(p(:)%P_codeperenne == 1))then
            p(:)%nrec = p(i)%nrec
          endif
        endif


        if (lge(sc%P_codesimul,'feuille') .eqv. .FALSE.) then
          if (p(i)%P_codelaitr == 1) then
            do ens = sc%AS,sc%AO
              if (p(i)%surf(ens) > 0.) then

                sc%ens = ens

                call calai(sc,pg,p(i),itk(i))

!write(199,*)'after calai, i,ens',i,ens,sc%n,p(i)%lai(0,sc%n-1),p(i)%lai(0,sc%n),p(i)%lai(ens,sc%n-1),&
!& p(i)%lai(ens,sc%n), p(i)%surf(sc%ens)
!#if DEBUG == 1
!                if (iand(sc%calai,4) >0) call calai_debug_read_output(1132,sc,pg,p,itk,soil,c,sta,t,i,ens)
!                if (iand(sc%calai,8) >0) call calai_debug_write_output(1133,sc,pg,p,itk,soil,c,sta,t,i,ens)
               !  TODO : if (iand(sc%calai,16) >0) call calai_debug_test_output(1134,sc,pg,p,itk,soil,c,sta,t,i,ens)
!#endif

              ! Rognage & Effeuillage, seulement si lai > 0
              ! !ATTENTION! on travaille sur masec(n-1) car � ce moment de la boucle journali�re masec(n) n'a pas encore �t� calcul�...
                if (p(i)%lai(ens,sc%n) > 0.0) then
                  if (itk(i)%P_codrognage == 2) then
                    call rognage(itk(i)%P_codcalrogne,p(i)%hauteur(ens),p(i)%largeur(ens),  & ! IN
                                 itk(i)%P_hautrogne,itk(i)%P_margerogne,itk(i)%P_largrogne, &
                                 p(i)%dfol,p(i)%P_hautbase,itk(i)%P_interrang,              &
                                 p(i)%sla(ens),p(i)%P_tigefeuil,itk(i)%P_biorognem,sc%n,    &
!                     p(i)%nrogne,p(i)%CNplante(ens),         & DR 20/07/2012 on a plus besoin du Cnplante il est calcul� apres
                                 p(i)%nrogne,                                             &
                                 p(i)%lairogne(ens),p(i)%biorogne(ens),                   & ! INOUT & OUT
                                 p(i)%lai(ens,sc%n),p(i)%masec(ens,sc%n-1),               &
                                 p(i)%varrapforme(ens),p(i)%P_forme,p(i)%biorognecum(ens),     &
                                 p(i)%lairognecum(ens))

                  ! Cumuls AO/AS
                    !p(i)%lairogne(ens)
                    !p(i)%biorogne(ens)
                    p(i)%lai(sc%aoas,sc%n) = p(i)%lai(sc%as,sc%n) * p(i)%surf(sc%as)              &
                                           + p(i)%lai(sc%ao,sc%n) * p(i)%surf(sc%ao)
                    p(i)%masec(sc%aoas,sc%n-1) = p(i)%masec(sc%as,sc%n-1) * p(i)%surf(sc%as)      &
                                               + p(i)%masec(sc%ao,sc%n-1) * p(i)%surf(sc%ao)
                    !p(i)%varrapforme
                    !p(i)%P_forme
                    !p(i)%biorognecum(ens)
                    !p(i)%lairognecum(ens)



                  ! NB le 23/05 recyclage de la biomasse rogn�e
                  ! PB - 08/08/2009 - Pas besoin d'apporter des r�sidus de rognage tous les jours.
                  !                   On conditionne l'apport en r�sidus, si il y a de la masse rogn�e.

                    if (sc%n == p(i)%nrogne .and. p(i)%biorogne(ens) > 0.) then
                      soil%itrav1 = 1
                      soil%itrav2 = 1
                      sc%ires = 2
                      Cfeupc  =  42.
                      CNrogne = Cfeupc / p(i)%CNplante(ens)

    ! Bruno 06/2012 - ajout quantites de C et N tombees au sol apres rognage
                      Crogne=p(i)%biorogne(ens)*Cfeupc*10.
                      Nrogne=Crogne/CNrogne
                      p(i)%QCrogne = p(i)%QCrogne + Crogne
                      p(i)%QNrogne = p(i)%QNrogne + Nrogne


!write(618,*)'rognage'
! EC 06/08/2012 Ajout du code ires des feuilles rogn�es
                     sc%ires = 2
                     call ResidusApportSurfaceSol(p(i)%biorogne(ens),Cfeupc,-1.e-10,sc%ires,pg%P_CNresmin(sc%ires),  & ! IN
                            pg%P_CNresmax(sc%ires),1.,1.,pg%P_Qmulchdec(sc%ires),sc%nbCouchesSol,pg%nbResidus,   &
                            itk(i)%nap,sc%airg(sc%n+1),CNrogne,sc%Cnondec(1:10),sc%Nnondec(1:10),                &
                            sc%Cres(1:sc%nbCouchesSol,1:pg%nbResidus),sc%Nres(1:sc%nbCouchesSol,1:pg%nbResidus), &
                            sc%QCapp,sc%QNapp,sc%QCresorg,sc%QNresorg)                                                 ! INOUT

                    Wr=0.
                    if (CNrogne > 0.) Wr=1./CNrogne
                    call ResiduParamDec(pg%P_awb(sc%ires),pg%P_bwb(sc%ires),pg%P_cwb(sc%ires),pg%P_CroCo(sc%ires),  &
                       pg%P_akres(sc%ires),pg%P_bkres(sc%ires),pg%P_ahres(sc%ires),pg%P_bhres(sc%ires),             &
                       sc%Wb(sc%ires),sc%kres(sc%ires),sc%hres(sc%ires),Wr)
                    endif

                  endif

                  if (itk(i)%P_codeffeuil /= 1) then
                    call effeuill(itk(i)%P_codcaleffeuil,itk(i)%P_laidebeff,p(i)%deltai(ens,sc%n),    & ! IN
                                  itk(i)%P_effeuil,p(i)%sla(ens),sc%n,p(i)%neffeuil,                &
                                  itk(i)%P_laieffeuil, p(i)%P_codetransrad,p(i)%P_hautmax,pg%P_khaut,     &
                                  p(i)%dfol,itk(i)%P_largrogne,itk(i)%P_codhauteff,p(i)%largeur(ens),      &
                                  p(i)%lai(ens,sc%n),p(i)%masec(ens,sc%n-1),p(i)%P_hautbase,        & ! INOUT
                                  p(i)%varrapforme(ens),p(i)%bioeffcum(ens),p(i)%laieffcum(ens))

                  ! Cumuls AO/AS
                    p(i)%lai(sc%aoas,sc%n) = p(i)%lai(sc%as,sc%n) * p(i)%surf(sc%as)              &
                                             + p(i)%lai(sc%ao,sc%n) * p(i)%surf(sc%ao)
                    p(i)%masec(sc%aoas,sc%n-1) = p(i)%masec(sc%as,sc%n-1) * p(i)%surf(sc%as)      &
                                                 + p(i)%masec(sc%ao,sc%n-1) * p(i)%surf(sc%ao)

                  endif
                endif
              endif
            end do
          else
            call TauxRecouvrement(sc,pg,p(i),itk(i))
          endif
        endif
      endif

!  write(199, *)'apres recalcul', p(i)%lai(sc%aoas,sc%n)

! TODO : On pourra remplacer les derni�res lignes par un appel � recalculevee apr�s avoir valid� et r�fl�chi
! � la pertinence d'�craser ou pas le stade LAXOBS par le stade LAX d�duit de la courbe de LAI forc�s.
!      if (lge(sc%P_codesimul,'feuille') .eqv. .TRUE.) call recalcullevee

      if (lge(sc%P_codesimul,'feuille') .eqv. .TRUE.) then
!: Recalcul de la lev�e en fonction de la courbe de LAI observ�e
        if (p(i)%nlev > 0) then
          if (sc%P_nbplantes > 1 .and. p(i)%lai(sc%AO,sc%n) <= 0.) then ! est-ce que �a ne devrait pas �tre if (i>1...) � la place de if (P_usm%P_nbplantes>1...) ?
            p(i)%lai(sc%AO,sc%n) = 0.001                                ! La plante principale n'a pas de partie � l'ombre, donc pas de lai.
          endif
          if (p(i)%lai(sc%AS,sc%n) <= 0.) then
            p(i)%lai(sc%AS,sc%n) = 0.001
          endif
        else
          if (p(i)%lai(sc%AS,sc%n) > 0. .or. p(i)%lai(sc%AO,sc%n) > 0.) then
            p(i)%nlevobs = sc%n
          endif
        endif
      endif

    end do


return
end
