! subroutine to read all files parameters
subroutine Stics_Lectures(sc,pg,p,itk,soil,c,sta,t,usma)

USE iso_varying_string

USE Stics
USE USM
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
USE Divers

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc

  type(Parametres_Generaux_), intent(INOUT) :: pg

  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)

  type(Sol_),                 intent(INOUT) :: soil

  type(Climat_),              intent(INOUT) :: c

  type(Station_),             intent(INOUT) :: sta

  type(Stics_Transit_),       intent(INOUT) :: t

  !DR 17/07/2012 je rajoute le module usm pour les noms de fichiers pour optimistics
  type(USM_),                 intent(INOUT) :: usma  ! // PARAMETER // name of the P_USM // SD // USMXML // 0

  integer,parameter :: nb_parameters_max = 300


! Variables locales
    integer :: i  !
    integer ::  ii
    ! variables locales pour l'optimisation
    integer :: nbpar, nblines
    real    :: valparopt(nb_parameters_max)
    type(varying_string) :: nompar (nb_parameters_max)



    character(len=255) :: ficplt
    ! dr 17/07/2012 j'allonge le nom de 20 � 50
    character(len=50) :: fictec
    ! DR 17/07/2012 j'ajoute ficini et ficsta
    character(len=50) :: ficini
    character(len=50) :: ficsta
    !

    character(len=50) :: moui
    integer           :: codeRetour


    character(len=4) :: treat
    integer :: num_treat
    real    :: profmesW, profmesN
    logical :: filexists
!! ATTENTION DR 04/09/2012 N_mineralisation est calcul� directement par qminh+qminr et Nvolatilisation est calcul� directement par qnvolorg+qnvoleng


if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*) 'lecinitialisations'
    !! Lecture du fichier d'initialisations : ficini.txt (format JavaStics)
          if (usma%file_Method_Optimistics_Init == 1)then
      ! DR 17/07/2012 on lit le nom du fichier dans newtravail.usm pour optimistics
        !fictec=p(i)%P_ftec
        ficini=usma%P_ficInit
      else
        write (ficini,'(A6,A4)') 'ficini','.txt'
      endif

      call lecinitialisations(sc,p,soil,ficini)


    !: Domi - 21/10/2004 - on met la lecture de lecparam en premier
!      call Lecture_Parametres_Generaux(pg)
      call Lecture_Parametres_Generaux(pg, sc%path, sc%pathtempopar)


      call Ecriture_Parametres_Generaux(pg)

    !: PB - Ouverture du fichier de debugging - 16/01/2004
    !- Domi - 22/10/2004 - index� sur le P_codesig
    !- PB - 29/07/2008 - le P_CODESIG est lu dans le fichier de param�tres g�n�raux
    ! DR 29/08/2012 on lit flagEcriture dan sle sparametres generaux et on supprime codesig
      if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG) >0) then
        sc%ficdbg = 88
        !open(sc%ficdbg,file='mod_debug.sti',status='unknown')
        open(sc%ficdbg,file='mod_debug.sti',status='unknown')
        if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *,'ouverture du fichier de debug ',&
        sc%ficdbg,' mod_debug.sti'
        sc%ficdbg2 = 100
        open(sc%ficdbg2,file='mod_debug2.sti',status='unknown')
        if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *,'ouverture du fichier de debug ',&
        sc%ficdbg2,' mod_debug2.sti'

      endif

    !: Lecture du fichier des parametres en attente
    !- d'�tre affect�s � leurs fichiers respectifs
!      call Stics_Lecture_Transit(t,sc%P_nbplantes)
       call Stics_Lecture_Transit(t,sc%P_nbplantes, sc%path, sc%pathtempoparv6)

    !TODO: regrouper les appels aux fonctions d'�critures des fichiers lus en 1 seul bloc ?
    ! DR 10/11/2016 effectivement je demarre le processus et je mets � la fin on a besoin des renseiegnements plantes
!      if (iand(pg%P_flagEcriture,sc%ECRITURE_HISTORIQUE) >0 ) &
!        call Ecriture_Transit(t)




    ! Pour chaque plante de la culture, on va lire un fichier technique et un fichier plante
      do i = 1, sc%P_nbplantes

      ! Ouverture des fichiers de sorties journali�res
        if (iand(pg%P_flagEcriture,sc%ECRITURE_SORTIESJOUR) > 0) then
        ! i = 1 pour la culture principale
        ! i = 2 pour la culture associ�e
          p(i)%ficsort = 15 + i
          if (sc%P_nbplantes == 1) then
            ii=min(40,len_trim(sc%P_usm))
            moui = 'mod_s'//sc%P_usm(1:ii)//'.sti'
            !moui = 's'//sc%P_usm(1:ii)//'.sti'
            open(p(i)%ficsort,file='./'//moui,status='unknown')
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'stics_lectures',sc%P_usm(1:ii),'output file',moui
          else
            ii=min(40,len_trim(sc%P_usm))
            if (i == 1) moui = 'mod_sp'//sc%P_usm(1:ii)//'.sti'
            !if (i == 1) moui = 'sp'//sc%P_usm(1:ii)//'.sti'
            if (i == 2) moui = 'mod_sa'//sc%P_usm(1:ii)//'.sti'
            !if (i == 2) moui = 'sa'//sc%P_usm(1:ii)//'.sti'
            open(p(i)%ficsort,file='./'//moui,status='unknown')
          endif

          ! 23/03/2016 pour Constance on a ajout� un param pour activer ou non la lecture des eclaircissages multiples
          if(t%P_option_thinning.eq.1)then
              itk(i)%flag_eclairmult=.TRUE.
          else
              itk(i)%flag_eclairmult=.FALSE.
          endif

          if(t%P_option_engrais_multiple.eq.1)then
              itk(i)%flag_plusieurs_engrais=.TRUE.
          else
              itk(i)%flag_plusieurs_engrais=.FALSE.
          endif

! DR 31/05/2017 avant la sortie de la version je desactive en dur la possibilite d'activation des patures
! on verra pour le mettre dans la prtochaine version
          if(t%P_option_pature.eq.1)then
            call EnvoyerMsgHistorique(2100)
            t%P_option_pature = 2
          endif
! fin 31/05/2017
          if(t%P_option_pature.eq.1)then
               itk(i)%flag_pature=.TRUE.
!               itk(i)%flag_plusieurs_engrais=.TRUE.
          else
               itk(i)%flag_pature=.FALSE.
          endif
        endif


      ! Ouverture des fichiers de sorties journali�res pour AGMIP
        if (iand(pg%P_flagEcriture,sc%ECRITURE_AGMIP) > 0) then
        ! i = 1 pour la culture principale
        ! i = 2 pour la culture associ�e
          p(i)%ficsort3 = 125 + i
          if (sc%P_nbplantes == 1) then
            ii=min(40,len_trim(sc%P_usm))
            moui = 'mod_s'//sc%P_usm(1:ii)//'.st3'
            open(p(i)%ficsort3,file='./'//moui,status='unknown')
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'stics_lectures',sc%P_usm(1:ii),'output file',moui
          else
            ii=min(40,len_trim(sc%P_usm))
            if (i == 1) moui = 'mod_sp'//sc%P_usm(1:ii)//'.st3'
            !if (i == 1) moui = 'sp'//sc%P_usm(1:ii)//'.sti'
            if (i == 2) moui = 'mod_sa'//sc%P_usm(1:ii)//'.st3'
            !if (i == 2) moui = 'sa'//sc%P_usm(1:ii)//'.sti'
            open(p(i)%ficsort3,file='./'//moui,status='unknown')
          endif
        endif

      ! Ouverture du fichier de sortie drainage le 10/09/2012 je vire
      ! Domi - 22/10/2004 - uniquement si P_codesig == 3
!        if (iand(pg%P_flagEcriture,sc%ECRITURE_DRAINAGE) > 0) then
!          p(i)%ficdrat = 50 + i
!          open(p(i)%ficsort,file='drailess.sti',position='append')
!        endif

      ! Ouverture des fichiers bilans
        if (iand(pg%P_flagEcriture,sc%ECRITURE_BILAN) > 0) then
          p(i)%ficbil = 25 + i
          if (sc%P_nbplantes == 1) then
            ii = min(40,len_trim(sc%P_usm))
            moui = 'mod_b'//sc%P_usm(1:ii)//'.sti'
            !moui = 'b'//sc%P_usm(1:ii)//'.sti'
          else
            ii = min(40,len_trim(sc%P_usm))
            if (i == 1) moui = 'mod_bp'//sc%P_usm(1:ii)//'.sti'
            !if (i == 1) moui = 'bp'//sc%P_usm(1:ii)//'.sti'
            if (i == 2) moui = 'mod_ba'//sc%P_usm(1:ii)//'.sti'
            !if (i == 2) moui = 'ba'//sc%P_usm(1:ii)//'.sti'
          endif
          open (p(i)%ficbil,file='./'//moui,status='unknown')
        endif

      ! Ouverture des fichiers rapports
        if (iand(pg%P_flagEcriture,sc%ECRITURE_RAPPORTS) > 0) then
          p(i)%ficrap = 30 + i
          p(i)%ficrap_AgMIP = 40 + i
          if (i == 1 .and. sc%P_nbplantes == 1) moui = 'mod_rapport.sti'
          !if (i == 1 .and. sc%P_nbplantes == 1) moui = 'rapport.sti'
          if (i == 1 .and. sc%P_nbplantes > 1)  moui = 'mod_rapportP.sti'
          !if (i == 1 .and. sc%P_nbplantes > 1)  moui = 'rapportP.sti'
          if (i == 2) moui = 'mod_rapportA.sti'
          !if (i == 2) moui = 'rapportA.sti'
          open (p(i)%ficrap,file=moui,position='append',recl=2000)
        endif

        if (iand(pg%P_flagEcriture,sc%ECRITURE_AGMIP) > 0) then
      !14/09/2011 ouverture rapport special pour AgMIP
          if (i == 1 .and. sc%P_nbplantes == 1) moui = 'mod_rapport_AgMIP.sti'
          !if (i == 1 .and. sc%P_nbplantes == 1) moui = 'rapport.sti'
          if (i == 1 .and. sc%P_nbplantes > 1)  moui = 'mod_rapportP_AgMIP.sti'
          !if (i == 1 .and. sc%P_nbplantes > 1)  moui = 'rapportP.sti'
          if (i == 2) moui = 'mod_rapportA_AgMIP.sti'
          !if (i == 2) moui = 'rapportA.sti'
          open (p(i)%ficrap_AgMIP,file=moui,position='append',recl=2000)
!          write(p(i)%ficrap_AgMIP,mes3000)

        endif

! fin modif STICS-SIG

! P_wdata1=fichier climatique du semis
! ancienne lecture des parametres  P_wdata1=wlieu//codpas//'.'//codan
        ii = index(sc%P_wdata1,'.')
        sc%wlieu = sc%P_wdata1(1:ii-2)



      ! Lecture du fichier technique (*.tec ou tempotec.sti)

      if (usma%file_Method_Optimistics_Tec(i) == 1)then
      ! DR 17/07/2012 on lit le nom du fichier dans newtravail.usm pour optimistics
        !fictec=p(i)%P_ftec
        fictec=itk(i)%P_ftec
      else
        write (fictec,'(A6,i1,A4)') 'fictec',i,'.txt'
      endif
        if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 ) write(*,*) fictec
!        call ITK_Lecture_V6(fictec,itk(i)) ! DR 19/07/2012 sc n'est pas utilis�
        call ITK_Lecture_V6(fictec,itk(i), sc%path, sc%pathtec) ! DR 19/07/2012 sc n'est pas utilis�


        call ITK_Ecriture_Tests(itk(i),pg,sc) !TODO: S�parer tests et �critures ?


      ! Lecture du fichier de param�tres de la plante (*.plt)
      ! -----------------------------------------------------

      if (usma%file_Method_Optimistics_Plt(i) == 1)then
      ! DR 17/07/2012 on lit le nom du fichier dans newtravail.usm pour optimistics
        ficplt=p(i)%P_fplt
      else
        write (ficplt,'(A6,i1,A4)') 'ficplt',i,'.txt'
      endif


        !call Plante_Lecture(p(i),ficplt,PLANTE_METHOD_STICS_V6,codeRetour)
        call Plante_Lecture(p(i),ficplt,PLANTE_METHOD_STICS_V6,codeRetour, sc%path, sc%pathplt)

        if (codeRetour == PLANTE_LECTURE_ERREUR_EOF) then
      ! erreur dans la lecture des param�tres
          call EnvoyerMsgHistorique(6)
          call EnvoyerMsgHistorique(5005)
          call EnvoyerMsgHistorique(133)
          !stop
          call exit(9)
        endif

        call Plante_Tests(p(i),itk(i)%P_variete)
        call Plante_Ecriture(p(i),sc,sta,itk(i),pg)

        sc%plante_ori(i)=.TRUE.
      ! PB - 23/07/2009 - On a introduit une variable logique qui d�termine si une plante est dominante ou non.
      !                   Par d�faut, la plante lue en premier est consid�r�e comme dominante et les suivantes comme domin�es.
      ! TODO : voir si on peut mettre les affectations de estDominante ailleurs
        p(i)%estDominante = .FALSE.

        ! si sol nu pas de test/�criture
        if (p(i)%P_codeplante == 'snu') then
          !--iplt = P_iwater
          itk(i)%P_iplt0 = sc%P_iwater
        else
            !call Plante_Ecriture
        endif

      end do

    ! PB - 23/07/2009 - On a introduit une variable logique qui d�termine si une plante est dominante ou non.
    !                   Par d�faut, la plante lue en premier est consid�r�e comme dominante et les suivantes comme domin�es.
    ! TODO : voir si on peut mettre les affectations de estDominante ailleurs
      p(1)%estDominante = .TRUE.


      if (usma%file_Method_Optimistics_Station == 1)then
      ! DR 17/07/2012 on lit le nom du fichier dans newtravail.usm pour optimistics
        ficsta=usma%P_ficStation
      else
        ficsta= 'station.txt'
      endif

    ! Lecture du fichier station
      call Station_Lecture(ficsta,sta,sc%path,sc%pathstation)

      !if (pg%P_codeSIG == 0) call Station_Ecriture(sta,itk(1)%P_codabri)

    ! Ecriture du fichier historique pour la structure Station
    !DR 13/12/2017 deplacement du test sur codeclichange ici et besoin de alphaCO2
      if (iand(pg%P_flagEcriture,sc%ECRITURE_HISTORIQUE) >0 ) &
        call Station_Ecriture(sta,itk(1)%P_codabri,p(1)%P_alphaCO2)

    ! dr 31/10/07 pour le moment on remet le nometp qu'il faudra changer apres
      if (sta%P_codeetp == 1) c%nometp = 'pe'
      if (sta%P_codeetp == 2) c%nometp = 'pc'
      if (sta%P_codeetp == 3) c%nometp = 'sw'
      if (sta%P_codeetp == 4) c%nometp = 'pt'

    ! pour les cultures associ�es, il faut SW, sinon stop
    ! - P_codeetp = 1 --> Penman forc�
    ! - P_codeetp = 2 --> Penman calcul�
    ! - P_codeetp = 3 --> Shutwall & Wallace
    ! - P_codeetp = 4 --> Prestley - Taylor
      if (sc%P_nbplantes > 1 .and. sta%P_codeetp /= 3) then
        call EnvoyerMsgHistorique(6)
        call EnvoyerMsgHistorique(433)
          !stop
          call exit(9)
      endif

    ! Pour les cultures associ�es (+ d'1 plante), il faut Shutwall & Wallace
      if (sc%P_nbplantes > 1 .and. sta%P_codeetp /= 3) then
        call EnvoyerMsgHistorique(433)
        call EnvoyerMsgHistorique(434)
        sta%P_codeetp = 3
    ! 16/10/06 - DR et ML - en culture associ� on ne sort plus mais on force le P_codeetp=3 (Shutwall & Wallace)
    ! On teste dans une routine d'initialisation du climat (iniclim) si les calculs sont possibles
        !--stop
      endif

    ! On effectue un test de coh�rence des param�tres pour le transfert radiatif
      if (sc%P_nbplantes > 1) then
        if (itk(1)%P_codetradtec == 2) then
          call EnvoyerMsgHistorique(6)
          call EnvoyerMsgHistorique(430)
          !stop
          call exit(9)
        endif
        p(1)%codetransradb = p(1)%P_codetransrad
        if (p(1)%P_codetransrad .ne. p(2)%P_codetransrad) then
          call EnvoyerMsgHistorique(6000)
          !stop
          call exit(9)
        endif

      endif

      do i = 1, sc%P_nbplantes
        if (p(i)%P_codetransrad == 2) then
          if (  itk(i)%P_interrang >= 999    .or.   &
                itk(i)%P_interrang <= 0.     .or.   &
                itk(i)%P_orientrang >= 999.  .or.   &
                p(i)%P_ktrou >= 999.         .or.   &
                p(i)%P_forme >= 999.         .or.   &
                p(i)%P_rapforme >= 999.      .or.   &
                p(i)%P_hautbase >= 999.      .or.   &
                p(i)%dfol >= 999.) then
            call EnvoyerMsgHistorique(6)
            call EnvoyerMsgHistorique(432)
            !stop
            call EnvoyerMsgHistorique(6)
            call exit(9)
          end if
        endif
      end do

! DR 11/12/2014 on verifie la coherence des parametres pour l'etp SW et le codebeso
     if(sta%P_codeetp == 3 .and. p(1)%P_codebeso==1 ) then
           call EnvoyerMsgHistorique(6)
           call EnvoyerMsgHistorique(203)
           call EnvoyerMsgHistorique(62)
          !stop
          call exit(9)
     endif

! DR  et Fr 30/05/2016 on verifie la coherence des parametres pour l'etp PC ou PE et le codebeso
     if(sta%P_codeetp .ne.3  .and. p(1)%P_codebeso==2 ) then
           call EnvoyerMsgHistorique(6)
           call EnvoyerMsgHistorique(1203)
           call EnvoyerMsgHistorique(1062)
!           call exit(9)
     endif

    ! Lecture des param�tres de sol
      ! DR 10/09/2012 j'ajoute le fichier sol
      if (usma%file_Method_Optimistics_Sol == 1)then
      ! DR 17/07/2012 on lit le nom du fichier dans newtravail.usm pour optimistics
        soil%ficsol=usma%P_nomsol
      else
        soil%ficsol='param.sol'
       ! write (ficplt,'(A6,i1,A4)') 'ficplt',i,'.txt'
      endif



!      call Sol_Lecture_V6(soil)
      call Sol_Lecture_V6(soil, sc%path, sc%pathsol)

      if(soil%P_CsurNsol==0.) soil%P_CsurNsol = 1./.105   !pg%P_Wh ! Bruno - initialisation du nouveau param�tre rapport C/N du sol
      if(soil%P_penterui==0.) soil%P_penterui = 0.33   !DR 27/07/2012 - externalisation du parametre penterui
      sc%nbCouchesSol =  soil%nbCouchesSol_max ! je le rajoute la car il n'est pas encore connu

    ! Pour tester les param�tres du sol
      call Sol_Tests(soil,sc%nbCouchesSol,sc%P_ichsl,pg%P_masvolcx,sc%beta_sol)
      do i = 1, sc%P_nbplantes
           call sol_test_itk(itk(i),soil%P_profhum)
      enddo

      if (iand(pg%P_flagEcriture,sc%ECRITURE_HISTORIQUE) >0 ) &
        call Sol_Ecriture(soil,pg,sc)


! ML et DR 08/06/09 introduction des modifs de Guillaume Jego
! ** DR et GJ 13/12/06 on lit les remontees capil dans P_capiljour.txt si codeleccapil=1
! DR 03/08/09 on nettoie le code des specificit�s nappe
!      if(codeleccapil.eq.1) call leccapil
! ML et DR 08/06/09 FIN introduction des modifs de Guillaume Jego


!: MODULE D'OPTIMISATION DES PARAMETRES D'UNE PLANTE
!-
!- Dans le cas o� le mod�le est lanc� pour une
!- optimisation, lecture des parametres de d�part
!- domi - 10/11/00 - changement des codes pour effectuer une optimisation
!- si codoptim=1, on optimise s'il y a lieu les param�tres de la Culture Principale
!- si codoptim=2  on optimise s'il y a lieu les param�tres de la Culture Associ�e
      if (sc%codoptim /= 0) then ! si le P_codeoptim est diff�rent de z�ro, on optimise
! dr 04/04/2011 voir si il faut le reactiver
!       idx = sc%codoptim
! DR 24/02/2011   j'active la lecture des parametres
        ! TODO : call lecoptim(sc%codoptim)
    ! DR 19/09/2012 je sort la lecture de param.sti ici pour Record voir si il faut le mettre encore en amont
        nblines = NumberOfLinesFile('param.sti')
        open(10,file='param.sti')
        read(10,*) nbpar ! le nb de param�tres � lire
        ! 18/01/2017 la lecture ne se fait plus sur le nb de parametres mais jusqu'a la fin du fichier pour gerer le forcage des CAS
        nbpar = (nblines-1)/2
        do i = 1,nbpar
            call get(10,nompar(i))
            read (10,*) valparopt(i)
        enddo
999     close(10)
      ! DR 19/09/2012 je passe les parametres � forcer en arguments
        call Lecture_Optimisation(sc,pg,p,itk,soil,sta,t,nbpar,nompar,valparopt)  ! DR 20/07/2012  c ne sert pas je vire
!       write(*,*)p(1)%P_durvieF(3)
!        write(*,*)'durvieF',j,p(1)%P_durvieF(j)


      endif

    ! Lecture des variables du fichier de sorties journali�res
    ! Ce sont les variables choisies par l'utilisateur que le syst�me
    ! transmettra en sortie � chaque pas de temps.
      call Lecture_VariablesSortiesJournalieres(sc)

    ! Lecture des variables du fichier rapport.sti
      call Lecture_VariablesRapport(sc)



! ecritures des messages dans l'history
    !TODO: regrouper les appels aux fonctions d'�critures des fichiers lus en 1 seul bloc ?
    ! DR 10/11/2016 effectivement je demarre le processus et je mets � la fin on a besoin des renseiegnements plantes
      if (iand(pg%P_flagEcriture,sc%ECRITURE_HISTORIQUE) >0 ) &
        call Ecriture_Transit(t,sc%P_nbplantes,p(1)%P_codeplante,p(2)%P_codeplante )








! DR 16/12/2013 j'ajoute des profmes specifiques pour Macsur
! une pour l'eau et une pour l'azote qu'on lit dans un fichier du workspace
   INQUIRE(file='depths_paramv6.txt',EXIST=filexists)
   if (filexists) then
      open(9936,file='depths_paramv6.txt')
      do num_treat=1,100
        read (9936,*, end=9999)treat,profmesW,profmesN
        if(treat==trim(sc%P_usm(3:6)))then
        sc%P_profmesW = profmesW
        sc%P_profmesN = profmesN
        endif
      enddo
 9999 continue
      close(9936)

   endif

return
end subroutine Stics_Lectures
