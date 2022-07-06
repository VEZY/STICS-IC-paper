subroutine JourDeCoupe(ipl,sc,pg,c,sta,soil,p,itk,t)


USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Climat
USE Station
USE Sol
USE Bilans

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc

  type(Parametres_Generaux_), intent(IN)    :: pg

  type(Climat_),              intent(INOUT) :: c

  type(Plante_),              intent(INOUT) :: p

  type(ITK_),                 intent(INOUT) :: itk

  type(Stics_Transit_),       intent(INOUT) :: t

  type(Sol_),                 intent(INOUT) :: soil

  type(Station_),             intent(INOUT) :: sta
  integer ,intent(IN) :: ipl

  integer :: i  !
  integer :: kcoupe  !
  integer :: jj  !
  integer ::  ens
  real    :: cumdeltai  !
  real    :: cumdeltams  !
  real    :: NC  !
  real    :: varintlai(sc%n),varintms(sc%n)
  integer :: k  !
  integer ::  aoas  !
  integer ::  as  !
  integer ::  ao
  real    :: QNplanteavantfauche ! EC 07/08/2012 pour le calcul du N export� � la fauche et le calcul du bilan azot�
!  real :: save_tempfauche(20)
  real :: save_tempfauche(0:20)
  integer :: kk, jour
  real :: anit_engrais
  real :: equi_masec

  aoas = sc%AOAS
  as = sc%AS
  ao = sc%AO

! EC 07/08/2012 pour le calcul du N export� � la fauche et le calcul du bilan azot�
  QNplanteavantfauche = 0.

  ! ** NB - le 29/06 - s�chage du foin
  ! *- � voir avec DOMI
  ! *------------------
  ! --        foinpluie = 0.0
  ! --        nbjfoinsec = 3
  ! --        seuilrrfoin = 0.0
  ! --        do 21 jfoin = 0,nbjfoinsec
  ! --          foinpluie = foinpluie+trr(n+jfoin)
  ! --21      continue
  ! --        if (foinpluie > seuilrrfoin) then
  ! --          sioncoupe = .FALSE.
  ! --          fauchediff = .TRUE.
  ! --          goto 200
  ! --        else
  ! --          fauchediff = .FALSE.
  ! --        endif

        ! DR 02/05/07 pb si on repousse de plus d'une date de fauche on ne trouve plus la date de fauche d'apres
        ! DR 05/07/07 dans le cas des sommes ca ne pose pas de pb et du coup ca merdait
          if (itk%P_codemodfauche == 2) then
            if (sc%n > p%nfauche(p%numcoupe+1) .and. p%nfauche(p%numcoupe+1) > 0) then
            ! TODO: v�rifier s'il n'y a pas un risque de d�bordement tableau
              do  jj = p%numcoupe,itk%nbcoupe
                p%nfauche(jj) = p%nfauche(jj+1)
              enddo
              return
            endif
          endif

        !dr et FR 25/06/2015 on ne veux pas supprimer la coupe 1 initiale en cas de repousse de fauche de l'annee d'avant
        ! donc on decale les coupes d'apres
        if (itk%P_codemodfauche == 3) then
                !**************** DR 15/11/2016 , c'est la le pb *************** !!!!
            if(sc%numcult.gt.1.and.p%fauchediff.and.p%numcoupe==1)then
           ! if(p%fauchediff.and.p%numcoupe==1)then

             ! DR 20062016 en attente de reprise des modifs enchain'nouveau calendar jourdecoupe numcoupe=1 avant calcul', (p%tempfauche_ancours(i),i=p%numcoupe,itk%nbcoupe)
              save_tempfauche=p%tempfauche_ancours
            else
                if(p%fauchediff)then
                    p%tempfauche_ancours(p%numcoupe) = p%somcourfauche
                    p%tempfauche_ancours(p%numcoupe+1) = p%somcourfauche + itk%P_tempfauche(p%numcoupe+1)
                   ! DR 20062016 en attente de reprise des modifs enchain'nouveau calendar jourdecoupe numcoupe>1 et fauche repoussee',  &
                   ! &  (p%tempfauche_ancours(i),i=p%numcoupe,itk%nbcoupe+1)  !!DR 11/06/2016
                else
!!!!! regarder les indices pour gerer la coupe 6 alors que nbcoupe=5 !!!!!!!
                    p%tempfauche_ancours(p%numcoupe+1) = p%somcourfauche + itk%P_tempfauche(p%numcoupe+1)!!DR 11/06/2016

                   ! DR 20062016 en attente de reprise des modifs enchain'nouveau calendar jourdecoupe numcoupe>1 ou fauche non repoussee',  &
                   ! &  (p%tempfauche_ancours(i),i=p%numcoupe,itk%nbcoupe+1)
                endif
            endif
         endif

        ! DR 20062016 en attente de reprise des modifs enchain'on fauche coupe ',p%numcoupe,'le ',sc%n,' a la somme ',p%somcourfauche,' on a repousse de ',&
        ! & p%nbrepoussefauche
        ! DR 20062016 en attente de reprise des modifs enchain'nouveau calendar jourdecoupe 2', (p%tempfauche_ancours(i),i=p%numcoupe,itk%nbcoupe+1)!!DR 11/06/2016
        ! DR et FR 08/06/2016on garde memoire de la somme a laquelle on a fauche
          p%som_vraie_fauche(p%numcoupe) = p%somcourfauche
        ! on garde memoire de la date de fauche
          p%nfauche(p%numcoupe) = sc%n
       ! domi le 23/01/98 : les dates de fauches sont dans le calendrier hydrique
       ! faut faire attention le jour de la coupe de ne pas ecrire le jour d'avant
           p%day_cut = p%nfauche(p%numcoupe) + sc%P_iwater - 1 ! TODO: tCal1J...

        !: modif domi 25-08-97  pour les bilans intercoupe
        !- on soustrait le cumul de la coupe d'avant
          p%cescoupe      = p%ces      - p%cescoupe
          p%cepcoupe      = p%cep      - p%cepcoupe
          p%cetmcoupe     = p%cetm     - p%cetmcoupe
          p%cetcoupe      = p%cet      - p%cetcoupe
          p%cprecipcoupe  = p%cprecip  - p%cprecipcoupe
          p%masectot      = p%masectot + p%masecneo(aoas)

        ! DR 14/08/09
        ! integration modifs 7.4
        ! *******************************************************
        ! DR le 31/01/08 on garde le stade P_ilax pour chaque coupe
        ! DR le 21/09/09 on a decid� avec Marie de ne pas garder la variable
        !--          p%ilaxs_prairie(p%numcoupe) = p%ilaxs
        ! *******************************************************

        ! ecriture du bilan intercoupe
          if (iand(pg%P_flagEcriture,sc%ECRITURE_BILAN) > 0) then
             ! DR 12/05/2021 on n'a pas besoin ici de faire la boucle sur les plantes on est deja dans la boucle plante , sinon on ecrit 2 fois le bilan intercoupe !!
!            do i = 1, sc%P_nbplantes
              call ecrireBilanIntercoupe(sc,pg,p,itk)
!            end do
          endif

        ! domi - 19/06/01 - ligne dans rapport pour l'intercoupe
          if (iand(pg%P_flagEcriture,sc%ECRITURE_RAPPORTS) >0 ) then
           ! DR 19/07/2012 on supprime itk qui ne sert pas
            call Ecriture_Rapport(sc,pg,soil,c,sta,p,t)
          endif

        ! le repoussement de la date de fauche est fini
          p%fauchediff = .FALSE.

        ! on remet a zero les compteurs du stress
          p%nst1coupe = 0
          p%str1coupe = 0
          p%stu1coupe = 0
          p%inn1coupe = 0
          p%diftemp1coupe = 0
          p%nst2coupe = 0
          p%str2coupe = 0
          p%stu2coupe = 0
          p%inn2coupe = 0
          p%diftemp2coupe = 0
          ! DR 04/04/2016 on ecrase anitcoupe si c'est une pature
          ! 1=pature, 2=fauche
          ! DR 30/11/2016 on fait les apports dans tous les cas pature ou pas
          ! if(itk%flag_pature .eqv. .false. .or. itk%P_restit(p%numcoupe).eq.2)then
          !if(itk%flag_pature .eqv. .false. .or. itk%P_restit(p%numcoupe).eq.2)then
             !   sc%anit(sc%n+1) = itk%P_anitcoupe(p%numcoupe)
                sc%anit(sc%n+1) = sc%anit(sc%n+1)+itk%P_anitcoupe(p%numcoupe)
                sc%anit_engrais(sc%n+1)=sc%anit(sc%n+1)
                sc%type_ferti(sc%n+1)= itk%P_engrais(1)

                !write(2222,*)'jourdecoupe',sc%numcult,sc%n+1,sc%anit_engrais(sc%n+1)
          !endif

! DR 29/03/2016 voir si il faut faire pareil pour le caca
          p%numcoupe = p%numcoupe + 1

! 11/07/2016 DR et FR on affecte mscoupemini_courant pour' la coupe d'apres
         if( itk%flag_pature)then
            itk%mscoupemini_courant =itk%P_mscoupemini(p%numcoupe)
         else
            itk%mscoupemini_courant =itk%P_mscoupemini(1)
         endif

        ! d�placement du calcul de la r�initialisation du parcours de d�veloppement
        ! Nb - le 28/05
          p%msres(sc%aoas) = itk%P_msresiduel(p%numcoupe-1) ! on r�initialise toutes les composantes de msres (sc%aoas, sc%ao & sc%as)
          p%msres(sc%as)   = itk%P_msresiduel(p%numcoupe-1)
          p%msres(sc%ao)   = itk%P_msresiduel(p%numcoupe-1)

		! DR et ML et SYL 15/06/09
		! ************************
		! introduction de la fin des modifications de Sylvain (nadine et FR)
		! dans le cadre du projet PERMED
		!
		! DR et ML et SYL 16/06/09
		! P_tigefeuilcoupe sera � conserver dans la version 7.2 par contre ratioresfauche sera � supprimer
		! ML 20/10/09 dernier jour du 7ieme mois de grossesse (1m de tour de taille)
		! on supprime le calcul specifique de matigestruc en fonction de ratioresfauche (proportion de r�serves carbon�es des tiges apr�s la coupe)
          if (t%P_codedyntalle(p%ipl) == 1) then
		! ####
		! SYL 030308
		! introduction d'un rapport P_tigefeuilcoupe diff�rent du P_tigefeuil en phase de croissance
            p%matigestruc(sc%as) = p%msres(sc%as) / (1+(1/t%P_tigefeuilcoupe(p%ipl)))
            p%matigestruc(sc%ao) = p%msres(sc%ao) / (1+(1/t%P_tigefeuilcoupe(p%ipl)))
          else
        ! ** NB & PB - 01/02/2005 - on recalcule la masse de tige � la coupe
        ! *- en fonction de la biomasse r�siduelle
        ! 27/10/09  on a supprime le parametre ratioresfauche et on l'a mis � 0 dans l'equation
            p%matigestruc(sc%as) = p%msres(sc%as)*(1) / (1+(1/p%P_tigefeuil))
            p%matigestruc(sc%ao) = p%msres(sc%ao)*(1) / (1+(1/p%P_tigefeuil))
          endif
        ! DR et ML et SYL 15/06/09 FIN introduction de la fin des modifications de Sylvain

          p%somcour = p%udevlaires(p%numcoupe-1)

          p%somcourdrp = p%udevlaires(p%numcoupe-1)

        ! 17/11/05 - domi :
        ! si on coupe apres amf et avant drp on ne pourra plus
        ! faire d'epi donc on arrete le developpement des stades reproducteurs
        !--  somcourdrp = somcourdrp+upvt(n)
          if (p%namf /= 0 .and. (p%ndrp == 0 .or. p%nflo == 0)) then
            p%somcourdrp = p%somcourdrp - p%upvt(sc%n)
            p%onarretesomcourdrp = .TRUE.
          endif

        ! DR 08/11/05 du fait qu'on fonctionne en cumul de temp depuis le demarrage on ne reinitialise pas la somme
        !-- p%somcourfauche = 0.
          p%P_stlevamf(itk%P_variete) = p%stlevamf0
          p%P_stamflax(itk%P_variete) = p%stamflax0
          p%P_stlaxsen(itk%P_variete) = p%stlaxsen0
          p%P_stsenlan(itk%P_variete) = p%stsenlan0
          p%P_stlevdrp(itk%P_variete) = p%stlevdrp0

        ! On reinitialise les stades, on repart comme pour une levee
          p%nplt = sc%n
          p%nlev = sc%n
          !05/05/2015 DR et FR On ne veut pas repartir a un stade anterieur au stade initial
          !    p%namf = 0
          if(p%P_stade0.eq.'amf')then
              p%namf = sc%n
          else
              p%namf = 0
          endif
          p%nlax = 0
          p%ndrp = 0
          p%nsen = 0
          p%nlan = 0
          p%nmat = 0
        ! PB - 24/01/2005 : la fauche est une r�colte. On met nrec() = n
          p%nrec = 0
        ! NB - 11/07/2006 :
        ! on ne remet pas le calendrier de s�nescence � z�ro � la coupe
        !-- ndebsen = 0
          p%nnou = 0

          p%pdsfruit(:,:) = 0
          p%nbfruit = 0.0

        ! NB - 22/6/98 - pour densirac
        ! DR introduction modifs Sylvain
        ! SYL - 28/09/2007 : suppression de l'initialisation des variable de senescence racinaire
        !--  ndebsenrac = 0
        !--  somtemprac = 0.0


        ! reinitialisation des sommes de temperatures
          p%masec(:,:) = 0.
          p%cumrg = 0.

        ! 21/03/2016 on doit remettre � zero le cumul de dltamstombe (mafeuiltombe) pour ne pas soustraire � la coupe n+1 ce qui est tomb� aux coupes d'avant
          p%mafeuiltombe(:)= 0.


        ! *!* � verifier le 26/02/98
          p%masec(:,sc%n) = itk%P_msresiduel(p%numcoupe-1)
          p%lai(:,sc%n) = itk%P_lairesiduel(p%numcoupe-1)

        ! DR  17/06/08 c'est pas correct
        ! DR 04/06/08  en cas de coupe magrain n'etait pas reinitialis�
        ! sur les conseils de Fran�oise j'initialise � masec
        !-- p%magrain(:,sc%n)  = itk%P_msresiduel(p%numcoupe-1)


        ! ** version 5.x
        ! *- NB - le 28/05/01
        ! *- remise � jour des dltams et deltai
        ! *- on retire les plus r�cents
        ! NB et Sylvain initialisation aussi en ms le 06/07/07
        !  pour le LAI et la biomasse, on r�cup�re les surfaces les plus anciennes (et les biomasses
        !  correspondantes)jusqu'� atteindre la valeur du LAI r�siduel et on les place dans les
        !  cases correspondants aux jours juste avant la coupe.
        !  Cela permet d'augmenter la dur�e de vie des feuilles correspondantes suite � la coupe.
          do ens = as,ao
            cumdeltai = 0.0
            cumdeltams = 0.0
            do i = 1, sc%n
              varintlai(i) = 0.0
              varintms(i) = 0.0
              cumdeltai = cumdeltai + p%deltai(ens,i)
              cumdeltams = cumdeltams + p%dltams(ens,i)
              if (cumdeltai > p%lai(ens,sc%n)) p%deltai(ens,i) = 0.0
              if (cumdeltams > p%masec(ens,sc%n)) p%dltams(ens,i) = 0.0
            end do

            k = 0
            do i = 1, sc%n
              if (p%deltai(ens,i) > 0.0) then
                k = k+1
                varintlai(sc%n-k+1) = p%deltai(ens,i)
                varintms(sc%n-k+1) = p%dltams(ens,i)
              endif
            end do

            do i = 1, sc%n
              p%deltai(ens,i) = varintlai(i)
              p%dltams(ens,i) = varintms(i)
            end do
          end do


        ! NB - le 28/05 - remise � jour des gels
          p%gel1 = 1.0
          p%gel2 = 1.0
          p%gel3 = 1.0


        ! *!* pour compatibilit� comportement stics4
        ! � voir si on garde ou pas au cas par cas une fois le programme valid�
          p%inns(as) = p%inns(aoas)
          p%swfac(as) = p%swfac(aoas)
          p%turfac(as) = p%turfac(aoas)
          p%pgrain(as) = p%pgrain(aoas)
          p%teaugrain(as) = p%teaugrain(aoas)

        ! PB - pb de comportement avec la version modularis� qui recalcul un certain nombre de variable de cumuls AO/AS
        !      en cours de pas de temps. Cela peut fausser les calculs, en particulier en cas de culture pure.
        !      pour contourner le pb, on affecte la valeur aoas � ao, seulement si surf(ao) > 0, sinon, 0.
          if (p%surf(ao) > 0.) then
            p%inns(ao) = p%inns(aoas)
            p%swfac(ao) = p%swfac(aoas)
            p%turfac(ao) = p%turfac(aoas)
            p%pgrain(ao) = p%pgrain(aoas)
            p%teaugrain(ao) = p%teaugrain(aoas)
          else
            p%inns(ao) = 0.
            p%swfac(ao) = 0.
            p%turfac(ao) = 0.
            p%pgrain(ao) = 0.
            p%teaugrain(ao) = 0.
          endif
        ! *- FIN compatibilit�..

        ! Domi - 23/01/1998 :
        ! On reinitialise le QNplante entre chaque coupe
        ! QNplante doit correspondre � msdil
        ! Domi - 21/10/2004 : A VOIR !!! ca avait saut�
        !--  QNplanteres = CNplante*P_msresiduel(p%numcoupe-1)
        ! modif - 04/11/2004 - NB, FR - pour INN constant apr�s coupe calcul du nouvel NC
          if (itk%P_msresiduel(p%numcoupe-1) <= p%P_masecNmax) then
            NC = p%P_adil * p%P_masecNmax**(-p%P_bdil)
          else
            NC = p%P_adil * itk%P_msresiduel(p%numcoupe-1)**(-p%P_bdil)
          endif

! TODO: pour la plante dominante, ne vaudrait-il pas mieux ne rien affecter pour la partie AO ?
!       n'y a-t-il pas un risque d'effet de bord ind�sirable ?

        ! calcul de CNplante
        ! dr 170106 on fait des tests
          p%CNplante(ao)   = p%inn(ao)   * NC
          p%CNplante(as)   = p%inn(as)   * NC
        ! pb - 06/06/2009
          p%CNplante(aoas) = p%inn(aoas) * NC

! EC 07/08/2012 Pour le calcul de la quantit� de N export�e � la coupe
          QNplanteavantfauche = p%QNplante(aoas,sc%n)
        ! calcul de QNplante
          p%QNplante(as,sc%n)   = p%CNplante(as) * itk%P_msresiduel(p%numcoupe-1) * p%surf(as) * 10
          p%QNplante(ao,sc%n)   = p%CNplante(ao) * itk%P_msresiduel(p%numcoupe-1) * p%surf(ao) * 10
        ! pb - 06/06/2009
          p%QNplante(aoas,sc%n) = p%QNplante(ao,sc%n) + p%QNplante(as,sc%n)
! EC 07/08/2012 Cumul du N export� aux fauches (prend en les racines mortes restitu�es au sol ; ce stock sera d�duit dans apportresidusdeculture)
          p%QNplantefauche = p%QNplantefauche + (QNplanteavantfauche - p%QNplante(aoas,sc%n))

        ! on supprime les r�serves � la coupe
        ! PB&NB - 23/02/2005 - on ne supprime plus les r�serves � la coupe
          p%cumdltares(:)   = 0.0
          p%masecneo(:)     = 0.0
          p%msresjaune(:)   = 0.0
          p%msneojaune(:)   = 0.0
!          p%masecneov(:)    = p%masecneo(:)
!          p%msneojaunev(:)  = p%msneojaune(:)
!          p%msresjaunev(:)  = p%msresjaune(:)
!          p%msresv(:)       = p%msres(:)
          p%mafeuiljaune(:) = 0.0
          p%mafeuilp(:)     = 0.0

          p%magrain(:,sc%n) = 0.

        ! PB - 06/09/2009
        ! Si nsencour = 0, alors erreur dans le calcul de la s�nescence => index out of bound exception sur ulai car debut=0
        ! donc, je change =0 par =1. Je sais pas si �a change bcp de chose.
          p%nsencour(:) = 1
          p%nsencourpre = sc%n

          p%masecpartiel(as) = 0.0
          p%masecpartiel(ao) = 0.0

        ! on veut des cumuls inter-coupe (pour ces,cet,cep etc)
        ! donc on va conserver le cumul de la coupe d'avant
          p%cescoupe      = p%ces
          p%cepcoupe      = p%cep
          p%cetmcoupe     = p%cetm
          p%cetcoupe      = p%cet
          p%cprecipcoupe  = p%cprecip

        ! DR 20/07/06 on reinitialise
          p%nbrepoussefauche = 0.

          ! DR et FR 08/06/2015
          ! on calcule dltamstombe sur le dltamsen de la veille , donc le jour de coupe on calcule un mafeuiljaune negatif car
          !  mafeuiljaune= mafeuiljaune + dltamsen - dltamstombe alors que dltamsen est deja nul .
          ! donc on ne fait pas tomber ce qui a ete coupe

          p%dltamstombe(:) = 0.0

          ! RV: update the delta of lai and masec for the whole plant:
          p%deltai(0,:) = p%deltai(1,:) * p%surf(1) + p%deltai(2,:) * p%surf(2)
          p%dltams(0,:) = p%dltams(1,:) * p%surf(1) + p%dltams(2,:) * p%surf(2)

          ! RV: reset the senescence-related variables after cutting:
          p%dltaisen(:) = 0.0
          p%deltai_sencour(:,:) = 0.0
          p%dltams_sencour(:,:) = 0.0
          p%pfeuilverte_sencour(:,:) = 0.0
          ! p%pfeuilverte(:,:) = 0.0

          ! RV: Update the heigth according to each method (must add it because now heigth is
          ! computed incrementally each day):

          if (t%P_code_shape(p%ipl) == 1) then
            if (p%P_codelaitr == 1) then
                ! using LAI
                p%hauteur(aoas) = (p%P_hautmax - p%P_hautbase) * (1 - exp(-pg%P_khaut * &
                    (p%lai(aoas,sc%n) + p%laisen(aoas,sc%n)))) + p%P_hautbase
            else
                ! using tauxcouv
                p%hauteur(aoas) = p%P_hautbase + (p%P_hautmax - p%P_hautbase) * sc%tauxcouv(sc%n)
            endif
          else if (t%P_code_shape(p%ipl) == 2) then
                equi_masec = p%masec(aoas,sc%n) * (t%P_hautdens(p%ipl) / p%densite)
                ! Computing the height from an allometry with the dry mass (masec):
                p%hauteur(aoas) = p%P_hautbase + (t%P_hautK(p%ipl) * (equi_masec**t%P_hautA(p%ipl)))
          else
                p%hauteur(aoas) = p%P_hautbase + p%P_hautmax / (1.0 + EXP(-t%P_haut_dev_k(p%ipl) * &
                    (p%somupvtsem - t%P_haut_dev_x0(p%ipl))))

                ! RV: does not work properly. We should think about a better way here, because
                ! he development is not reset so...
          endif

          p%hauteur(as) = p%hauteur(aoas)
          p%hauteur(ao) = p%hauteur(aoas)

          ! DR 30/03/2016 on va prendre en compte les restitutions d'uree pour le jour d'apres
          if(itk%flag_pature)then
             if(itk%P_restit(p%numcoupe-1).eq.1)then
                  sc%flag_onacoupe=.TRUE.
                  ! DR je cherche le jour ou on va affecter l'uree car si on a un apport deja de type engrais on va decaler jusqu'a ce qu'on ai rien
                 ! do kk=sc%n+1,731
                 !    if(sc%anit_engrais(kk).eq.0)then
                 !       jour=kk
                 !       exit
                 !    endif
                 !enddo
                 ! DR 26/07/2016 on prend le CN plante de la veille (avant coupe) pour les pataurages
                 !call restitution_pature_uree(sc%n,p%CNplante_veille,p%msrec_fou,t%P_coef_calcul_doseN,t%P_pertes_restit_ext,&
                 !sc%anit_uree(jour))
                 call restitution_pature_uree(sc%n,p%CNplante_veille,p%msrec_fou,t%P_coef_calcul_doseN,t%P_pertes_restit_ext,&
                 sc%anit_uree(sc%n+1))
                 if(sc%anit_uree(sc%n+1).gt.0.)then
                      ! on garde anit_uree idependant puisque pas du meme type que les apports par engrais(anit et anicoupe)
                       !sc%anit(jour) = sc%anit(jour)+sc%anit_uree(jour)
                       itk%napN_uree=itk%napN_uree+1
                       itk%napN=itk%napN+1
                  ! DR 01/04/2016 on a pas besoin du jour on incremente juste anit
                 endif
                 itk%P_nbjres=itk%P_nbjres+1
                 call restitution_pature_organique(sc%n,p%CNplante_veille,p%msrec_fou,t%P_pertes_restit_ext,  &
                 t%P_coderes_pature,t%P_Crespc_pature, t%P_Nminres_pature, t%P_eaures_pature, t%P_coef_calcul_qres, &
                 itk%P_qres(itk%P_nbjres),itk%P_CsurNres(itk%P_nbjres),itk%P_coderes(itk%P_nbjres),   &
                 itk%P_Crespc(itk%P_nbjres),itk%P_Nminres(itk%P_nbjres),itk%P_eaures(itk%P_nbjres))

                 p%numjres(itk%P_nbjres)= sc%n+1

                 sc%CsurNres_pature = itk%P_CsurNres(itk%P_nbjres)
                 sc%qres_pature = itk%P_qres(itk%P_nbjres)
                 !write(*,*)sc%n,sc%qres_pature,itk%P_qres(itk%P_nbjres),itk%P_eaures(itk%P_nbjres)

             endif
          endif

return
end subroutine JourDeCoupe
