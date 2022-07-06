!*************************************************************
!      Gestion des sorties
!      calcul des valeurs moyennes de stress eau, azote et GEL
!      calcul des cumuls de flux et stocks d'eau et d'azote
!      calcul de la charge en fruits
!      enregistrement dans les fichiers:
!        - s*.sti et s*.st2
!        - racine.sti         (fichier 26)
!*************************************************************

! Output Management
! - calculation of mean values of stress water, nitrogen and GEL
! - calculation of cumulative stocks and flows of water and nitrogen
! - calculation of the fruit load
subroutine Stics_Calcul_Sorties_Plante(sc,pg,c,p,itk,soil,t)

USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Climat
USE Sol
!USE Station

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc

  type(Parametres_Generaux_), intent(IN)    :: pg

  type(Climat_),              intent(INOUT) :: c

  type(Plante_),              intent(INOUT) :: p

  type(ITK_),                 intent(INOUT) :: itk

  type(Stics_Transit_),       intent(INOUT) :: t

  type(Sol_),                 intent(INOUT) :: soil

!  type(Station_),             intent(INOUT) :: sta




!: Variables locales
      integer n,i,iz,izmax,nstress1, AOAS
      integer           :: codebbch_entier
!--  modif ML le 09/09/05: ajout d une variable poids du tubercule "matuber"
!--  pour modeliser la croissance du pivot de betterave = mafruit + resperenne
! dr 03/04/2012 matuber est passe dans la strucutre plante pour l'avoir en sortie
!      real matuber(2)


!: Fonction(s)
      !--real TEMPROSEE,TVAR,troseeair,troseecult



    ! pour faciliter un peu l'ecriture..
      n = sc%n

    ! indice Ombre + Soleil
      AOAS = 0

! TODO: enregistrer les valeur de CNgrain, CNplante, QNgrain pour le jour nrec, et supprimer leur tableau temporel

! DR 26/07/2016 on stocke le CNplante cde la veille pour les paturages
        p%CNplante_veille       = p%CNplante(AOAS)


    ! le jour de la recolte, on sauvegarde les valeurs
      if (n == p%nrec) then
        p%mafrais_nrec        = p%mafrais(AOAS)
        p%pdsfruitfrais_nrec  = p%pdsfruitfrais(AOAS)
        p%mabois_nrec         = p%mabois(AOAS)
        p%H2Orec_nrec         = p%H2Orec(AOAS)
        p%chargefruit_nrec    = p%chargefruit
        p%CNgrain_nrec        = p%CNgrain(AOAS)
        p%CNplante_nrec       = p%CNplante(AOAS)
        p%QNgrain_nrec        = p%QNgrain(AOAS)
        p%hauteur_nrec        = p%hauteur(AOAS)
        p%largeur_nrec        = p%largeur(AOAS)
      endif

    ! 29/04/03 - forcage de la masec et magrain  de recolte si P_codemsfinal = 1
      if (pg%P_codemsfinal == 1 .and. itk%P_codcueille == 1) then

      ! apres la recolte, on force les valeurs a� celle du jour de la recolte
        if (n > p%nrec .and. p%nrec > 0) then
          p%magrain(AOAS,n)     = p%magrain(AOAS,p%nrec)
          p%masec(AOAS,n)       = p%masec(AOAS,p%nrec)
        ! DR 13/01/06 dans ce cas on maintient toutes les variables plantes du sti
          p%QNplante(AOAS,n)    = p%QNplante(AOAS,p%nrec)
          p%CNgrain(AOAS)       = p%CNgrain_nrec
          p%hauteur(AOAS)       = p%hauteur_nrec
          p%largeur(AOAS)       = p%largeur_nrec
        !  DR 13/01/06 ceux la n'etant pas indexe sur le jour ca semble bete a� voir
          p%mafrais(AOAS)       = p%mafrais_nrec
          p%pdsfruitfrais(AOAS) = p%pdsfruitfrais_nrec
          p%mabois(AOAS)        = p%mabois_nrec
          p%H2Orec(AOAS)        = p%H2Orec_nrec
          p%chargefruit         = p%chargefruit_nrec
          ! 26/03/2014 maintien de variables suppl
          p%msrac(n)            = p%msrac(p%nrec)
          p%irazo(AOAS,n)       = p%irazo(AOAS,p%nrec)
          p%ircarb(AOAS,n)      = p%ircarb(AOAS,p%nrec)
        endif

      endif



    ! cas du sol nu: indicateurs de stress =  - 1  et profondeur racinaire = 0
      if (p%P_codeplante == 'snu') then
        p%swfac(AOAS)  = -1.
        p%turfac(AOAS) = -1.
        p%zrac = 0.
      endif




!: calcul des stress moyens sur les 2 grandes periodes du cycle
!--------------------------------------------------------------

    ! changement du stade fin de periode vegetative si pb drp avant lev
      if (p%nlev == 0 .and. p%ndrp > 0) then
        nstress1 = p%nlax
      else
        nstress1 = p%ndrp
      endif

      if (p%nlev > 0 .and. (nstress1 == 0 .or. n == nstress1)) then
        p%nst1 = p%nst1 + 1
        p%str1 = p%str1 + p%swfac(AOAS)
        p%stu1 = p%stu1 + p%turfac(AOAS)
        p%inn1 = p%inn1 + p%inns(AOAS)
        p%diftemp1 = p%diftemp1 + sc%tcult - c%tmoy(n)
        p%exofac1 = p%exofac1 + p%exofac
! DR 12/04/2011 j'ajoute les stress etr/etm et etm/etr dans le bilan
! calcul de etr/etm
        if(sc%etm > 0)then
           p%etr_etm1 = p%etr_etm1 + (sc%et / sc%etm)
        else
           p%etr_etm1 = p%etr_etm1 + 1
        endif
! calcul de etm/etr
        if(sc%et > 0)then
           p%etm_etr1 = p%etm_etr1 + (sc%etm / sc%et)
        else
           if (sc%etm >0)then
               p%etm_etr1 = p%etm_etr1 + 1
           else
               p%etm_etr1 = p%etm_etr1 + 0
           endif
        endif
!        write(*,*)n,'etr/etm = ',p%etr_etm1,sc%et,sc%etm

        p%inn1moy = p%inn1 / p%nst1
        p%turfac1moy = p%stu1 / p%nst1
        p%swfac1moy = p%str1 / p%nst1
        p%exofac1moy = p%exofac1 / p%nst1

        p%etr_etm1moy = p%etr_etm1 / p%nst1
        p%etm_etr1moy = p%etm_etr1 / p%nst1

      endif

      if (p%ndrp > 0 .and. (p%nmat == 0 .or. n == p%nmat)) then
        p%nst2 = p%nst2 + 1
        p%str2 = p%str2 + p%swfac(AOAS)
        p%stu2 = p%stu2 + p%turfac(AOAS)
        p%inn2 = p%inn2 + p%inns(AOAS)
        p%diftemp2 = p%diftemp2 + sc%tcult - c%tmoy(n)
        p%exofac2 = p%exofac2 + p%exofac

 ! DR 12/04/2011 j'ajoute les stress etr/etm et etm/etr dans le bilan
! calcul de etr/etm
        if(sc%etm > 0 )then
            p%etr_etm2 = p%etr_etm2 + (sc%et/sc%etm)
        else
            p%etr_etm2 = p%etr_etm2 + 1
        endif
! calcul de etm/etr
        if(sc%et > 0)then
            p%etm_etr2 = p%etm_etr2 + (sc%etm/sc%et)
        else
           if (sc%etm >0)then
                p%etm_etr2 = p%etm_etr2 + 1
           else
                p%etm_etr2 = p%etm_etr2 + 0
           endif
        endif

        p%inn2moy = p%inn2 / p%nst2
        p%turfac2moy = p%stu2 / p%nst2
        p%swfac2moy = p%str2 / p%nst2
        p%exofac2moy = p%exofac2 / p%nst2

        p%etr_etm2moy = p%etr_etm2 / p%nst2
        p%etm_etr2moy = p%etm_etr2 / p%nst2


      endif

    ! effet du GEL
      if (p%nlev > 0 .and. (p%namf == 0 .or. n == p%namf)) then
        p%gel1 = min(p%gel1, p%fstressgel)
      endif

      if (p%namf > 0) p%gel2 = min(p%gel2, p%fstressgel)

      p%gel3 = min(p%gel3, p%fgelflo)

    ! DR 12/08/08 si gel2 = 0.0 plante completement gelee, on met tous les stades suivants a amf
      if (p%P_codeperenne /= 2 .and. (p%gel2 <= 0.0 .or. p%gel1 <= 0.0)) then
        if (p%nrec == 0) then
          p%group = -1
          p%nrec = n
        endif
        if (p%nlev > 0 .and. p%namf == 0) then
          p%namf = n
          p%P_stlevamf(itk%P_variete) = p%somcour
        endif
        if (p%namf > 0 .and. p%nlax == 0) then
          p%nlax = n
          p%P_stamflax(itk%P_variete) = p%somcour
        endif
        if (p%nlax > 0 .and. p%nsen == 0) then
          p%nsen = n
          p%P_stlaxsen(itk%P_variete) = p%somcour
        endif
        if (p%nsen > 0 .and. p%nlan == 0) then
          p%nlan = n
          p%P_stsenlan(itk%P_variete) = p%somcour
        endif
        if (p%nflo == 0) then
          p%nflo = n
          p%stlevflo = p%somcourdrp
        endif
        if (p%ndrp == 0) then
          p%ndrp = n
          p%P_stlevdrp(itk%P_variete) = p%somcourdrp
        endif
        if (p%ndebdes == 0) then
          p%ndebdes = n
          p%P_stdrpdes(itk%P_variete) = p%somcourdrp
        endif
        if (p%ndrp > 0 .and. p%nmat == 0) then
          p%nmat = n
          p%P_stdrpmat(itk%P_variete) = p%somcourdrp
        endif
      endif


    !: Cumuls sur le cycle de la culture
    !- NB - le 08/05/02
      if (n >= p%nplt  .and.  (p%nrec == 0 .or. n <= p%nrec .or. itk%P_codcueille == 2)) then
        p%ces = p%ces + sc%esol
        p%cep = p%cep + p%ep(AOAS)
        p%cet = p%cep + p%ces
        p%cprecip = p%cprecip + sc%precip + sc%ruisselsurf
        ! DR 16/09/2016 On avait pas de varaible exprimant le cumul des evaporation totals(mulch+sol), on les ajoute
        sc%cEdirect=sc%cEdirect+sc%Edirect
      ! cas des irrigations en profondeur
        if (itk%P_codlocirrig == 3) then
          p%cprecip = p%cprecip + p%irrigprof(itk%P_locirrig)
        endif
        p%cetm = p%cetm + sc%etm
        p%cdemande = p%cdemande + p%demande(AOAS)
        p%cinterpluie = p%cinterpluie + p%interpluie(AOAS)
! dr 26/07/2011 anniversaire de maraige de Marie  on le deplace sinon pas de prise en compte dans bilan avant semis
!        sc%cintermulch = sc%cintermulch + sc%intermulch       ! TODO: cintermulch, pourquoi dans stics_communs et pas dans plante, comme les autres cumuls de cette partie ?
      ! DR 17/03/06 rajout de cumul sur la periode de culture
        p%crg = p%crg + c%trg(n)
        p%ctmoy = p%ctmoy + c%tmoy(n)
        p%ctcult = p%ctcult + sc%tcult
        p%cetp = p%cetp + c%tetp(n)
! DR 29/12/2014 ajout de la somme des tcultmax pour Giacomo
        p%ctcultmax = p%ctcultmax + sc%tcultmax


        p%cum_et0=p%cum_et0+p%eop(aoas)+sc%eos
      endif

    ! calcul d'une quantite d'azote au stade drp
      if (n == p%ndrp) sc%QNdrp = p%QNplante(AOAS,n)     ! TODO: que fait-on de  QNdrp ? variable plante non indicee dans le tronc commun et inutilisee.

    ! ML - le 23/03/04 : rajout d'un test pour eviter le calcul de rdtint(0)
    ! qui n'est pas défini, et est appele si nbrecolte = 1
      if (itk%P_codcueille == 2 .and. p%nbrecolte > 1) then
        p%mafruit = p%magrain(AOAS,n) / 100.  - p%rdtint(AOAS,p%nbrecolte-1) / 100.
      else
        p%mafruit = p%magrain(AOAS,n) / 100.
      endif

    ! calcul de la charge en fruits
      if (p%P_codeindetermin == 1) p%chargefruit = p%nbgrains(AOAS)
      if (p%P_codeindetermin == 2) then
        p%chargefruit = p%nbfruit
        if (pg%P_codefrmur == 1) then
          p%chargefruit = p%nbfruit + p%nfruit(AOAS,p%P_nboite)
        endif
      endif
      if (p%magrain(AOAS,n) <= 0.) p%chargefruit = 0.

      if (p%P_codeplante == 'fou') then
! 29/07/2015 DR et FR il faut retrancher de mafruit ce qui est tombe au sol en plus de ce qui est jaune
! 10/01/2017 finalement on a deja retranche dltamstombe de masec dans biomaer, donc on ne le fait pas la
      !  p%mafruit = p%masec(AOAS,n) - p%msresjaune(AOAS) - p%msneojaune(AOAS) - p%mafeuiltombe(AOAS)
        p%mafruit = p%masec(AOAS,n) - p%msresjaune(AOAS) - p%msneojaune(AOAS)
        ! DR 05/05/2015 DR et FR on ajoute une varaible
        p%msjaune(AOAS) = p%msresjaune(AOAS) + p%msneojaune(AOAS)
      endif

      p%mafruit = max(0.,p%mafruit)

      if (p%P_codeplante.eq.'bet')then
    ! modif ML le 09/09/05: ajout d une variable poids du tubercule "matuber"
    ! pour modeliser la croissance du pivot de betterave = mafruit + resperenne
    ! Dr 06/04/2012 on ne calcule le matuber que jusqu'a la recolte
         p%matuber = p%mafruit + p%resperenne(AOAS)
         if(n<=p%nrec.and.p%nrec>0)p%matuber_rec=p%matuber

    ! 29/04/03 - forcage de la masec et magrain  de recolte si P_codemsfinal = 1
         if (pg%P_codemsfinal == 1 .and. itk%P_codcueille == 1) then
      ! apres la recolte, on force les valeurs a� celle du jour de la recolte
           if (n > p%nrec .and. p%nrec > 0) then
               p%matuber= p%matuber_rec
           endif
         endif
      endif


!  domi - 03/04/2002 - on recalcule les stades dans le calendrier julien
!- pour pouvoir les ecrire dans le st2 et dans le rapport.sti
!- domi - 20/10/2003 - on implemente le stade que si on l'a deja passe
      if (p%nplt /= 0) p%iplts = p%nplt + sc%P_iwater - 1
      if (p%nlev /= 0) p%ilevs = p%nlev + sc%P_iwater - 1
      if (p%namf /= 0) p%iamfs = p%namf + sc%P_iwater - 1
      if (p%nlax /= 0) p%ilaxs = p%nlax + sc%P_iwater - 1
      if (p%ndrp /= 0) p%idrps = p%ndrp + sc%P_iwater - 1
      if (p%nflo /= 0) p%iflos = p%nflo + sc%P_iwater - 1
      if (p%nsen /= 0) p%isens = p%nsen + sc%P_iwater - 1
      if (p%nlan /= 0) p%ilans = p%nlan + sc%P_iwater - 1
      if (p%nmat /= 0) p%imats = p%nmat + sc%P_iwater - 1
      if (p%nrec /= 0) p%irecs = p%nrec + sc%P_iwater - 1
      if (p%ndebdorm /= 0) p%idebdorms = p%ndebdorm + sc%P_iwater - 1
      if (p%nfindorm /= 0) p%ifindorms = p%nfindorm + sc%P_iwater - 1
      if (p%nnou /= 0) p%inous = p%nnou + sc%P_iwater - 1
      if (p%ndebdes /= 0) p%idebdess = p%ndebdes + sc%P_iwater - 1
      if (p%nger /= 0) p%igers = p%nger + sc%P_iwater - 1
!- fin domi - 03/04/2002

         ! DR 31/05/2016 ajout du nombre de jours depuis le semis
         sc%day_after_sowing=sc%n-(p%iplts-sc%P_iwater)-1
!         write(*,*)sc%n,p%iplts,sc%P_iwater,sc%day_after_sowing






! DR et ML 29/06/09
! *****************
! fin introduction des modifications de Sylvain Satger pour la prairie: calcul de imontaisons
      if (t%nmontaison(p%ipl) /= 0) p%imontaisons = t%nmontaison(p%ipl) + sc%P_iwater - 1
! FIN DR et ML 29/06/09

! DR 03/03/08 pour climator on veut les variables suivantes
      if (n == p%nrec) then
! DR 20/09/2016 correction du calcul de ce qui est exporte , on enleve les racines qui ne sont pas compabilis�es dans le QNressuite
!        p%nexporte =  p%QNplante(AOAS,p%nrec) - p%QNressuite
        p%nexporte =  p%QNplante(AOAS,p%nrec) - p%QNressuite - p%QNrac
        ! dr et ml 04/09/2014 on indexe sur ao et as pour les varaibles de l'abscission
        p%nrecycle =  p%QNressuite + p%QNplantetombe(AOAS)
        p%MSexporte  =  p%masec(AOAS,p%nrec) - p%Qressuite
        p%MSrecycle =  p%Qressuite + p%mafeuiltombe(AOAS)
      endif

! 30/04/08 pour julie on ajoute le poids de 1000 grains
! DR 30/04/08 poids de 1000 grains pour Julie
      if (p%pgrain(AOAS) <= 0) then
        p%p1000grain = 0.0
      else
        p%p1000grain = p%pgrain(AOAS) * 1000
      endif


    ! 08/09/06 DR etIGC on calcule la tempmini moy et l'amplitude moyenne
    !  entre lax et rec (ca donne un indicateur de la qualite du pinardos )
      if (n >= p%nlax .and. p%nlax /= 0 .and. (p%nrec == 0 .or. p%nrec == n)) then
        c%tncultmat = c%tncultmat + sc%tcultmin
        c%amptcultmat = c%amptcultmat + (sc%tcultmax - sc%tcultmin)
        c%dureelaxrec = c%dureelaxrec + 1
      ! DR 09/10/06 je remplace le 37 degres par P_tmaxremp pour
      !             caluler un nb de jours d'echaudage
        if (sc%tcultmax > p%P_tmaxremp) c%nbjechaudage = c%nbjechaudage + 1    ! nbjechaudage dans comclim, pourquoi pas plutot une variable plante ?
        if (n == p%nrec) then
          c%tncultmat = c%tncultmat / c%dureelaxrec
          c%amptcultmat = c%amptcultmat / c%dureelaxrec
        endif
      endif



! DR  07/03/08 Pour JR estrade faut sortir des sommes de temp semis recolte
!  if (n >= nplt .and. nrec == 0) then
      if (n >= p%nplt) then
        p%somudevair = p%somudevair  + p%udevair
        p%somudevcult = p%somudevcult + p%udevcult
        p%somupvtsem  = p%somupvtsem  + p%upvt(n)
      endif


    ! moyenne des donnees eau, azote et temperature par horizon
      sc%nhe = 0
      do i = 1, sc%NH
        p%LRACH(i) = 0.
        izmax = int( soil%P_epc(i) )
        do iz = 1, izmax
          if (p%P_coderacine == 2) then
            p%LRACH(i) = p%LRACH(i) + p%rl(n, sc%nhe + iz)
          else
            p%LRACH(i) = p%LRACH(i) + p%flrac(sc%nhe + iz) * pg%P_lvopt
          endif
        end do
        sc%nhe = sc%nhe + izmax
        p%LRACH(i) = p%LRACH(i) / soil%P_epc(i)
      ! dr 20/06/08 si c'est inferieur a� E-40 on a un pb d'ecriture en scientifique
      ! d'ou les collage entre 2 colonnes
      ! dans ce cas je mets a zero
        if (p%LRACH(i) < 0.0000001) p%LRACH(i) = 0.0000001
      end do

!      write(*,*)' dans sortie n',n,'mafruit',p%mafruit

    ! 23/06/2015 DR et FR on ajoute la variable mafruit r�colt�
       p%msrec_fou= p%mafruit- itk%P_msresiduel(p%numcoupe)


! ajout des nouvelles varaibles plantes issues de correspondancevaraibledesorties
    ! dr 05/09/2011 je rajoute laimax
          if(p%lai(0,n)>p%laimax(0))then
            p%laimax(0) = p%lai(0,n)
          endif
! DR 19/02/2016 je demarre de creer les variables pour les changements d'unites
          p%masec_kg_ha = p%masec(0,n)*1000.
          p%mafruit_kg_ha = p%mafruit*1000
          p%mafeuil_kg_ha = p%mafeuil(0)*1000
          p%matigestruc_kg_ha = p%matigestruc(0)*1000
! DR 25/03/2016 je continue de sortir les varaibles specifiques de correspondance
          p%H2Orec_percent(aoas) = p%H2Orec(aoas)*100.  !
          p%huile_percent(aoas) = p%huile(aoas)*100.
          p%sucre_percent(aoas) = p%sucre(aoas)*100.  !
          p%gel1_percent = (1-p%gel1)*100. !
          p%gel2_percent = (1-p%gel2)*100. !
          p%gel3_percent = (1-p%gel3)*100. !

          p%nbinflo_recal=p%P_nbinflo

          p%ebmax_gr = p%ebmax*100.
          p%fpari_gr = p%fpari*100.

          call charversint(p%codebbch,codebbch_entier) !
          p%codebbch_output = codebbch_entier !







return
end subroutine Stics_Calcul_Sorties_Plante


! partie commune, non plante
subroutine Stics_Calcul_Sorties(sc,c,soil,p,itk,pg,sta)

USE Stics
USE Plante
USE Itineraire_Technique
USE Climat
USE Sol
USE Parametres_Generaux
USE Station

USE Divers, only : TVAR, F_temprosee

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc

  type(Climat_),              intent(INOUT) :: c

  type(Plante_),              intent(INOUT) :: p

  type(ITK_),                 intent(INOUT) :: itk

  type(Sol_),                 intent(INOUT) :: soil

  type(Parametres_Generaux_), intent(IN)    :: pg

  type(Station_),             intent(INOUT) :: sta

  integer :: i  !
  integer ::  iz  !
  integer ::  izmax  !
  integer ::  n  !
  integer ::  ir
  integer :: epaisseur_couche
!  real    :: troseeair  !
!  real    ::  troseecult


    ! pour alleger l'ecriture
      n = sc%n


    ! Cumuls sur l'ensemble de la simulation
      sc%cestout = sc%cestout + sc%esol
      sc%cpreciptout = sc%cpreciptout + sc%precip + sc%ruisselsurf
    ! DR 16/09/2016 On avait pas de varaible exprimant le cumul des evaporation totals(mulch+sol), on les ajoute
        sc%cEdirecttout=sc%cEdirecttout+sc%Edirect
! dr 26/07/2011 il est mieux la !!
      sc%cintermulch = sc%cintermulch + sc%intermulch       ! TODO: cintermulch, pourquoi dans stics_communs et pas dans plante, comme les autres cumuls de cette partie ?

! DR 16/12/2013 pour Macsur cumul a partir du semis des variables autres que plantes
      if (n >= p%nplt  .and.  (p%nrec == 0 .or. n <= p%nrec .or. itk%P_codcueille == 2)) then
        sc%drain_from_plt = sc%drain_from_plt +sc%drain
        sc%leaching_from_plt = sc%leaching_from_plt + sc%azsup
        sc%runoff_from_plt = sc%runoff_from_plt + sc%ruissel
        sc%Nmineral_from_plt = sc%Nmineral_from_plt + soil%cumvminr + soil%cumvminh
        sc%Nvolat_from_plt = sc%Nvolat_from_plt + soil%Nvoleng + soil%Nvolorg
        sc%QNdenit_from_plt = sc%QNdenit_from_plt + soil%Ndenit
      endif
! DR 02/03/2017 pour Macsur_vigne cumul a partir du debourrement des variables autres que plantes
      if ((n >= p%nlev .and. p%nlev.ne.0)  .and.  (p%nrec == 0 .or. n <= p%nrec )) then
        sc%drain_from_lev = sc%drain_from_lev +sc%drain
        sc%leaching_from_lev = sc%leaching_from_lev + sc%azsup
        sc%runoff_from_lev = sc%runoff_from_lev + sc%ruissel
        sc%Nmineral_from_lev = sc%Nmineral_from_lev + soil%cumvminr + soil%cumvminh
        sc%Nvolat_from_lev = sc%Nvolat_from_lev + soil%Nvoleng + soil%Nvolorg
        sc%QNdenit_from_lev = sc%QNdenit_from_lev + soil%Ndenit
        sc%cet_from_lev = sc%cet_from_lev + sc%esol + p%ep(sc%AOAS)
        sc%cum_et0_from_lev=sc%cum_et0_from_lev+p%eop(sc%AOAS)+sc%eos
      endif

    ! cas des irrigations en profondeur
    ! TODO: ajouter P_codlocirrig dans le Stics Communs. C'est une variable technique mais unique a� la simulation,
    !       donc a� dupliquer et harmoniser au sein d'une variable dans le tronc commun des variables stics.
    ! En fait, par defaut, on prend les valeurs de la plante principale.
      if (itk%P_codlocirrig == 3) then
        sc%cpreciptout = sc%cpreciptout + p%irrigprof(itk%P_locirrig)
      endif

    ! DR 20/04/06 on met le cumul de ruissel ici au lieu de lixiv
    ! cumul de ruissellement
      sc%ruisselt = sc%ruisselt + sc%ruissel

    ! moyenne des donnees eau, azote et temperature par horizon
      sc%nhe = 0
      do i = 1, sc%NH
        sc%HR(i)    = 0.
        soil%AZnit(i) = 0.
        sc%AZamm(i) = 0.
        sc%TS(i)    = 0.
        izmax = int( soil%P_epc(i) )
        do iz = 1, izmax

          if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG) >0)&
            write(sc%ficdbg,'(2i3,2f16.12)')sc%n,sc%nhe + iz,sc%hur(sc%nhe + iz),sc%sat(sc%nhe + iz)
          sc%HR(i) = sc%HR(i) + sc%HUR(sc%nhe + iz) + sc%sat(sc%nhe + iz)
          soil%AZnit(i) = soil%AZnit(i) + soil%nit(sc%nhe + iz)
          sc%AZamm(i) = sc%AZamm(i) + soil%amm(sc%nhe + iz)
          sc%TS(i) = sc%TS(i) + sc%tsol(sc%nhe + iz)
        end do
        sc%nhe = sc%nhe + izmax
!   write(70,*)n
          if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG) >0)then
               write(sc%ficdbg,*)n,'hur et sat',(sc%HUR(iz),sc%sat(iz),iz=1,20)
          endif

      ! DR et ML et BM et EJ 22/06/09
      ! *****************************
      ! introduction des modifications de BM et EJ
      ! calcul de la concentration en nitrate (mg NO3/litre) de chaque horizon
      ! (attention ce n'est pas une concentration en azote!)
        sc%concNO3sol(i) = 6200./14. * soil%AZnit(i) / sc%HR(i)
      ! 22/06/09 FIN introduction des modifications de BM

        sc%HR(i) = sc%HR(i) / soil%da(i) * 10 / soil%P_epc(i)
        sc%TS(i) = sc%TS(i) / soil%P_epc(i)
        if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG) >0)then
           write(sc%ficdbg,*)'ecriture_sortie n',sc%n,i,sc%HR(i),soil%AZnit(i),sc%AZamm(i)
        endif
      end do

! DR 13/10/2016 je divise les humidites volumiques par 100. d'apres Patrick B.
! DR 30/09/2016 pour AgMIP ET calcul des humdites sur 10 CM pour caler avce des obs
        epaisseur_couche=10
        ! couche 1-30
        sc%HR_vol_1_10  = 0.
        do iz = 1, 10
          sc%HR_vol_1_10 = sc%HR_vol_1_10 + (( sc%HUR( iz) + sc%sat( iz)) * 10 / epaisseur_couche)
         end do
         sc%HR_vol_1_10 = sc%HR_vol_1_10/100.
! DR 14/02/2016 pour AgMIP ET calcul des humdites sur 30 CM
! on reste en humidites volumique !!
        epaisseur_couche=30
        ! couche 1-30
        sc%HR_vol_1_30  = 0.
        do iz = 1, 30
          sc%HR_vol_1_30 = sc%HR_vol_1_30 + (( sc%HUR( iz) + sc%sat( iz)) * 10 / epaisseur_couche)
         end do
        sc%HR_vol_1_30 = sc%HR_vol_1_30/100.
        ! couche 31-60
        sc%HR_vol_31_60  = 0.
        do iz = 31, 60
          sc%HR_vol_31_60 = sc%HR_vol_31_60 + (( sc%HUR( iz) + sc%sat( iz)) * 10 / epaisseur_couche)
         end do
        sc%HR_vol_31_60 = sc%HR_vol_31_60/100.
        ! couche 61-90
        sc%HR_vol_61_90  = 0.
        do iz = 61, 90
          sc%HR_vol_61_90 = sc%HR_vol_61_90 + (( sc%HUR( iz) + sc%sat( iz)) * 10 / epaisseur_couche)
         end do
         sc%HR_vol_61_90 = sc%HR_vol_61_90/100.
        ! couche 91-120
        sc%HR_vol_91_120  = 0.
        do iz = 91, 120
          sc%HR_vol_91_120 = sc%HR_vol_91_120 + (( sc%HUR( iz) + sc%sat( iz)) * 10 / epaisseur_couche)
         end do
         sc%HR_vol_91_120 = sc%HR_vol_91_120/100.
        ! couche 121-150
        sc%HR_vol_121_150  = 0.
        do iz = 121, 150
          sc%HR_vol_121_150 = sc%HR_vol_121_150 + (( sc%HUR( iz) + sc%sat( iz)) * 10 / epaisseur_couche)
         end do
         sc%HR_vol_121_150 = sc%HR_vol_121_150/100.
        ! couche 151-180
        sc%HR_vol_151_180  = 0.
        do iz = 151, 180
          sc%HR_vol_151_180 = sc%HR_vol_151_180 + (( sc%HUR( iz) + sc%sat( iz)) * 10 / epaisseur_couche)
         end do
         sc%HR_vol_151_180 = sc%HR_vol_151_180/100.

! DR et PL pour Agmip ET ,   on a cree une variable , anciennement 'hur(10)_vol'
         sc%hur_10_vol = sc%hur(10)/10.

! DR 14/02/2016  Fin AgMIP ET

    !  cumul des pluies
!      if (ipl == 1) then
        sc%cpluie = sc%cpluie + c%trr(n)
        sc%toteaunoneffic = sc%toteaunoneffic + sc%eaunoneffic
!      endif


    ! domi 10/01/06 nouvelles variables pour expertise secheresse
    ! DR 16/03/06 on renomme Ctculttout et Ctairtout pour etre coherent dans les noms
      c%Ctculttout = c%Ctculttout + sc%tcult
      c%Ctairtout  = c%Ctairtout  + c%tmoy(n)
      c%Crgtout    = c%Crgtout    + c%trg(n)
      c%Cetmtout   = c%Cetmtout   + sc%etm
      c%Ctetptout  = c%Ctetptout  + c%tetp(n)


      c%somdifftculttair = c%somdifftculttair + sc%tcult - sc%tairveille
    ! DR et ML 03/09/2012 ces 2 varaibles ne servent plus
    !  troseecult = F_temprosee(sc%humidite * TVAR(sc%tcult))
    !  troseeair = F_temprosee(c%tpm(n))
    ! NB et FH le 01/03/06
    ! vaut mieux faire les moyennes en humidite
    !--  somtroseecult = somtroseecult + TEMPROSEE(humidite * tvar(tcult))
    !--  somtroseeair = somtroseeair + TEMPROSEE(tpm(n))
          !dr 14/09/2012 inutiles
    !  c%somtroseecult = c%somtroseecult + sc%humidite
    !  c%somtroseeair  = c%somtroseeair  + c%humair


    ! DR 03/02/2011 on ajoute des varaibles de sorties sur les residus

 !  DR et ML 03/02/2011
 ! Cres    calcul du carbone total des residus par couche de 1 cm de sol
 ! Nres    calcul de l'azote total des residus par couche de 1 cm de sol
 ! DR 06/07/2012 faire attention cette varaible n'est pas utilisee
! DR 04/12/2013 on avait une pb de dimensionnement sur cette varaible qui ne sert plus a� rien arghhhhhhhhhhh !!!
!       do iz =1,int(soil%P_profhum)
!          sc%Ctousresidusparcouche(iz)= SUM (sc%Cres(iz,11:pg%nbResidus))  ! voir avec BM si 11 a� nbResidus
!          sc%Ntousresidusparcouche(iz)= SUM (sc%Nres(iz,11:pg%nbResidus))  ! voir avec BM si 11 a� nbResidus
!       enddo
 !  DR et ML 03/02/2011
 ! Cres    calcul du carbone total des residus (tous residus sur P_profhum)
 ! Nres    calcul de l'azote total des residus (tous residus sur P_profhum)

          sc%Ctousresidusprofil = SUM (sc%Cres(1:int(soil%P_profhum),11:pg%nbResidus))  ! voir avec BM si 11 à nbResidus
          sc%Ntousresidusprofil = SUM (sc%Nres(1:int(soil%P_profhum),11:pg%nbResidus))  ! voir avec BM si 11 à nbResidus
 !  DR et ML 03/02/2011
 ! Cres    calcul du carbone total du residu (ires) sur P_profhum
 ! Nres    calcul de l'azote total du residu (ires) sur P_profhum
       do ir = 11,pg%nbResidus
          sc%Cresiduprofil(ir-10)= SUM (sc%Cres(1:int(soil%P_profhum),ir))  ! voir avec BM si 11 a� nbResidus
          sc%Nresiduprofil(ir-10)= SUM (sc%Nres(1:int(soil%P_profhum),ir))  ! voir avec BM si 11 a� nbResidus
       enddo

!          if(p%P_codelaitr /= 1) then
          c%humair_percent = c%humair*100.
          sc%humidite_percent = sc%humidite*100.

          sc%N_mineralisation = sc%Qminh+sc%Qminr
          soil%N_volatilisation = soil%QNvolorg+soil%QNvoleng
          sc%tcult_tairveille = sc%tcult-sc%tairveille
          sc%SoilN = sc%azomes + sc%ammomes

          soil%epc_recal(1)=soil%P_epc(1)
          soil%epc_recal(2)=soil%P_epc(2)
          soil%epc_recal(3)=soil%P_epc(3)
          soil%epc_recal(4)=soil%P_epc(4)
          soil%epc_recal(5)=soil%P_epc(5)
          soil%infil_recal(1)=soil%P_infil(1)
          soil%infil_recal(2)=soil%P_infil(2)
          soil%infil_recal(3)=soil%P_infil(3)
          soil%infil_recal(4)=soil%P_infil(4)
          soil%infil_recal(5)=soil%P_infil(5)

          sta%ra_recal = sta%P_ra

 ! DR 27/05/2016 j'ajoute le calcul de la variable eop+eos
          p%et0=p%eop(sc%AOAS)+sc%eos

! DR 22/04/2016 je rajoute les varaibles prairies calculees dans le bilan
! les quantites d'N export�s et les dates de fauche
     !*******************************
     ! cas 1 : les cultures fauch�es
     if (itk%P_codefauche == 1) then
               p%QNexport = p%QNplantefauche
!**********************************************
! Cas 2 : les cultures perennes avec cueillette
     else
        if (itk%P_codcueille == 2 .and. p%P_codeperenne == 2) then
               p%QNexport = p%QNgrain(sc%AOAS)
        else
    !*******************************
    ! Cas 3 : les cultures annuelles
               p%QNexport = p%QNplante(sc%AOAS,p%nrec) - p%QNressuite - p%QNrac
        endif
     endif
      ! domi le 23/01/98 : les dates de fauches sont dans le calendrier hydrique
      ! faut faire attention le jour de la coupe de ne pas ecrire le jour d'avant
      ! if(sc%n.eq.
!        p%day_cut = p%nfauche(p%numcoupe-1) + sc%P_iwater - 1 ! TODO: tCal1J...

 ! DR 07/06/2017 j'ajoute la variable leai qui est la somme de la surface foliaire pour la photosuynthese des feuilles et des epis
 ! DR 07/06/2017  on cumul ici les composantes de la varaibles leai pour chaque sous-systeme
              p%leai(sc%AO)=p%lai(sc%AO,n)+p%eai(sc%AO)
              p%leai(sc%AS)=p%lai(sc%AS,n)+p%eai(sc%AS)


return
end subroutine Stics_Calcul_Sorties
