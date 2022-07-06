! ***** main program of Stics_modulo  *******
! - Programmation: N. Brisson et al
! - version v1.1
!! call readings / initializations and annual loop
!The aims of STICS (Simulateur mulTIdisciplinaire pour les Cultures Standard) are similar to those of a large number of existing models (Whisler et al., 1986).
! It is a crop model with a daily time-step and input variables relating to climate, soil and the crop system.
! Its output variables relate to yield in terms of quantity and quality and to the environment in terms of drainage and nitrate leaching.
! The simulated object is the crop situation for which a physical medium and a crop management schedule can be determined.
!! The main simulated processes are crop growth and development as well as the water and nitrogen balances.
!! A full description of crop models with their fundamental concepts is available in Brisson et al. (2006).
!!
! STICS has been developed since 1996 at INRA (French National Institute for Agronomic Research) in collaboration with other research
!! (CIRAD , CEMAGREF , Ecole des Mines de Paris, ESA , LSCE ) or professional (ARVALIS , CETIOM , CTIFL , ITV , ITB , Agrotransferts , etc.) and teaching institutes.
!!For more than 10 years STICS has been used and regularly improved thanks to a close link between development and application, involving scientists and technicians from various disciplines.
program Stics_Programme



!: LES MODULES
USE Stics
USE USM
USE Plante
USE Itineraire_Technique
USE Sol
USE Climat
USE Station
USE Parametres_Generaux
! DR 17/10/2013 on ajoute le module patho pour couplage avce Mila
!USE mod_patho


! #define DEBUG 1
! USE Debug


implicit none
!    integer, parameter :: nb_plant_max = 2

    type(USM_)                      :: usma  

    type(Stics_Communs_)            :: sc  

!    type(Plante_),allocatable       :: p(:)
    type(Plante_),dimension(nb_plant_max) :: p

    type(Parametres_Generaux_)      :: pg  

!    type(ITK_),allocatable          :: itk(:)
    type(ITK_),dimension(nb_plant_max)    :: itk

    type(Climat_)                   :: c  

    type(Station_)                  :: sta  

    type(Sol_)                      :: soil  

    type(Stics_Transit_)            :: t  

! DR 17/10/2013 on ajoute le module patho pour couplage avce Mila
! DR 07/02/2014 on enleve Mila du tronc , c'est gere dans la branche Stics_mila
!    type (Patho_)                   :: patho
!    type (Patho_inputs_Stics_)      :: pis

    integer :: i,n_args
    character (len=100) arg_content
    ! managing command line arguments for getting version number
    n_args=iargc()
    if  (n_args>0) then
       call getarg(1,arg_content)
       if (index(trim(arg_content),'version').gt.0) then
         call call_num_version(sc)
         
         print *, 'Modulostics version : ',sc%codeversion
       else
         print *, 'Unkown argument : ', arg_content
       endif
       call exit(0)
    endif

!TODO général:

! Priorité Haute
! --------------
! - gestion de l'échange entre plante et sol nu... + mort de la plante
! - découper biomaer
! - ...

! Priorité Moyenne
! ----------------
! - Harmoniser les variables "veille" entre celles suffixées '_veille' et celles préfixées 'p' (pour précédent)
! - Créer toutes les fonctions debug avec lectures/ecritures au jour le jour mais seulement pour les données différentes.
!   Ce qui permettra de directement voir les différences. Penser à mettre en sortie, la différence,
!   la fonction où intervient la différence et le jour.
! - Valider sur des jeux de tests appropriés les routines :
!       o Detassement
!       o Decisionsemis
!       o Decisionrecolte
!       o affectrrquinoapoquet
!       o calapNenUpvt
!       o calculAutomatiqueFertilisation => Testé avec la salade
! - Zéro-ifier toutes les structures.
! - Nettoyer le code de toutes les sorties sur l'écran pour du debug temporaire
! - Harmoniser la P_forme des commentaires dans toutes les routines du code
! - Debugguer la version Cultures Associées
! - Supprimer tous les GOTOs
! - Vérifier que les variables itrav1 et itrav2 n'ont pas à être mises
!   en entrées/sorties des fonctions où elles sont utilisées en tant que variables locales.
! - Apporter les modifications de la version 7.4 bis et des versions 6.4climator et 6.9 de référence.
! - Attention, P_fixmax, fixmaxvar, fixmaxC cumul de fixmaxvar. On enlève fixmaxC remplacé par fixmaxvar(0)
! - Stocker (ou écrire la partie du bilan concernée le jour de la récolte) la valeur de CNplante(nrec)
! - Pour éviter les débordements de tableaux, compiler avec l'option -fbounds-check (pour gfortran)
! - densiteVraieRacinaire(densirac.f90) : trier entrées/sorties
! - renommer les fichiers en fonction des noms des routines?
! - P_codecalferti,P_ratiolN : variables de Stics_Communs_ lues dans l'itk, faire un dédoublement et réaffectation.  DR 05/04/2011 passé dans paramv6
! - Comparer les sorties avec et sans le paramètre de compilation finit-local-zero pour traquer les pbs d'initialisations.
! - Créer les interfaces pour TOUTES les routines
! - Nettoyer/Corriger tous les Warnings
! - Calcul du jour julien à partir d'une date jour/mois/année :
!   J'ai trouvé la formule suivante :
!   >    Le quantième d'une date (numéro du jour de l'année)
!   >    peut se calculer de la manière suivante:
!   >    N = ENT (275 MM / 9) - 2 * ENT (MM + 9)/12 + DD - 30 pour les années ordinaires.
!   >    N = ENT (275 MM / 9) -  ENT (MM + 9)/12 + DD - 30 pour les années bisextiles.
!   >    avec MM = numéro du mois et DD le jour du mois.
!- Il y a un problème avec les tests d'équivalence de chaines de caractères,
!  notamment quand il s'agit d'un argument.
!  Souvent, Fortran considère les caractères nuls comme partie intégrante de la chaine.
!  Ex:
!   >   character(len=10) :: type = 'coupe'
!   >   character(len=5)  :: type2 = 'coupe'
!  Le test type == type2 renverra FAUX, même si l'on applique la fonction trim() !
! - Supprimer P_codeSIG ? FAIT le 29/08/2012
! - Lecture des fichiers XML (test avec paramv6 par ex.)
! - Mettre à jour et termine les routines de Zero-ification.
! - Supprimer toutes les occurences de la variable 'combithomas'
! - NH4init (et P_NH4initf) ne devrait-elle pas plutot se trouver dans la structure Sol ?
! - potsol et humpotsol sont deux fonctions assez similaires, possible d'extraire le code commun dans une sous-fonction ? On retrouve le même début de calcul dans 'denit' ainsi que dans 'mineral'.
! - D'une manière générale, on test des codes sur leur valeur oui/non représentés souvent dans Stics par 1 (oui) et 2 (non).
!   Pourtant, on trouve encore des tests d'égalité avec zéro.
!   En fait, il est vrai que les valeurs 1 (oui) et 0 (non) seraient somme toute plus logique dans le monde binaire de l'informatique.
!   Il vaudrait mieux aussi tester plutot l'égalité ou la différence d'avec 1 dont on est sûr de la valeur que l'égalité avec 0 ou 2.
! - qmulch0 dans ITK_ et Sol_ ?   DR 04/05/2011 n'existe plus
! - dans detassement et tassesemisrecolte, vérifier si on doit recalculer HR ou pas en fin de routine. Au risque d'écraser le HR = Hinit



! A TESTER :
!-----------
! - Lecture_Profil



! FAITS :
!--------
! - Débugguer Shutwall
! - Faire la routine de gestion des coupes
! - Lecture_Sorties
! - Faire la routine tauxrecouv
! - Faire les routines de sortie
! - Initialisation_PrairiePerenne
! - Lecture_LAIs
! - lecoptim + routine de génération du code principal de lecoptim
! - Réactiver l'écriture du microclimat (voir humheure.for en fin de routine, code commenté)
! - Créer une variable de choix des sorties par FLAG de puissance de 2
! - Répartir les variables de la structure Transit dans les structures adéquates
! - Déplacer P_codedateappN de la structure Stics_ vers ITK_ (lu dans l'itk)
! - Dans l'écriture des bilans "stades de développement", le stade DRP peut s'écrire à partir de
!   P_stlevdrp (bilcoupe) ou de P_stflodrp (bilan). Ca pose problème pour la généricité. Que faire ?
! => On test le P_codefauche au moment d'écrire pour choisir l'une ou l'autre variable.
! - Isoler le calcul de RU de RsurRU pour pouvoir metre RU en tant que variable de sortie (et non locale)

! =============== DEBUT DU PROGRAMME ================

! DEBUG

!    call Debug_Initialisation(sc)

! On fout des zéros partout dans la structure Sol
! (pas forcément nécessaire si le compilateur force l'initialisation à zéro, mais on sait jamais)

      call Stics_Zero(sc,usma)

      call Sol_Zero(soil)

      call Stics_Transit_Zero(t)

      call Parametres_Generaux_Zero(pg)



    ! TODO : ici, on affecte le flag Ecriture.
    !        Il faudra à l'avenir le lire dans un des fichiers d'entrée.
    ! DR 29/08/2012 maintenant c'est lu dans les parametres generaux à la place de codesig qui devient obsolete
!      pg%P_flagEcriture = 0                           &
!                      + sc%ECRITURE_BILAN           &
!                      + sc%ECRITURE_SORTIESJOUR     &
!                      + sc%ECRITURE_RAPPORTS        &
!                      + sc%ECRITURE_PROFIL          &
!                      + sc%ECRITURE_HISTORIQUE      &
!                      + sc%ECRITURE_ECRAN           &
!                      + sc%ECRITURE_AGMIP
!                      + 0

! DR 03/09/2012 on a pas encore lu le flagecriture donc je l'affecte à sorties historique
      pg%P_flagEcriture=1

    !: D'abord, on initialise le programme Stics
      call Stics_initialisation(sc) ! fonction d'initialisation du module Stics

    !: Ouverture du fichier de suivi de l'historique du déroulement de la simulation.
      if (iand(pg%P_flagEcriture,sc%ECRITURE_HISTORIQUE) >0 ) then
        sc%fichist = 89
        open (sc%fichist,file='modhistory.sti',status='unknown')
        call setFichierHistorique(sc%fichist)
        call setFlagEcriture(.true.)
      endif

      call EnvoyerMsgHistorique(437,sc%codeversion)
      call EnvoyerMsgHistorique(5)

       ! pour etre certain que le parametre path est vide
       write (sc%path, '(A255)') ' '            ! enabling_RECORD
       sc%path(1:255) = ' '                     ! enabling_RECORD
       write (sc%datapath, '(A255)') ' '        ! enabling_RECORD
       sc%datapath(1:255) = ' '                 ! enabling_RECORD
       write (sc%pathstation, '(A255)') ' '     ! enabling_RECORD
       sc%pathstation(1:255) = ' '              ! enabling_RECORD
       write (sc%pathclimat, '(A255)') ' '      ! enabling_RECORD
       sc%pathclimat(1:255) = ' '               ! enabling_RECORD
       write (sc%pathplt, '(A255)') ' '         ! enabling_RECORD
       sc%pathplt(1:255) = ' '                  ! enabling_RECORD
       write (sc%pathsol, '(A255)') ' '         ! enabling_RECORD
       sc%pathsol(1:255) = ' '                  ! enabling_RECORD
       write (sc%pathtec, '(A255)') ' '         ! enabling_RECORD
       sc%pathtec(1:255) = ' '                  ! enabling_RECORD
       write (sc%pathusm, '(A255)') ' '         ! enabling_RECORD
       sc%pathusm(1:255) = ' '                  ! enabling_RECORD
       write (sc%pathtempopar, '(A255)') ' '    ! enabling_RECORD
       sc%pathtempopar(1:255) = ' '             ! enabling_RECORD
       write (sc%pathtempoparv6, '(A255)') ' '  ! enabling_RECORD
       sc%pathtempoparv6(1:255) = ' '           ! enabling_RECORD
       write (sc%pathinit, '(A255)') ' '        ! enabling_RECORD
       sc%pathinit(1:255) = ' '                 ! enabling_RECORD

       write (sc%sticsid, '(A255)') ' '         ! enabling_RECORD
       sc%sticsid(1:255) = ' '                  ! enabling_RECORD



    !: Lecture du fichier P_usm : new-travail.P_usm (format JavaStics)
      if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 ) write(*,*) 'on lit le fichier usm'
      call USM_Lire(usma,sc%path, sc%pathusm)

      sc%P_codesimul = usma%P_codesimul
      sc%codoptim  = usma%codoptim
      sc%P_codesuite = usma%P_codesuite
      sc%P_iwater    = usma%P_iwater
      sc%P_ifwater   = usma%P_ifwater
      sc%P_nbplantes = usma%P_nbplantes
      sc%P_ichsl     = usma%P_ichsl
      sc%P_wdata1    = usma%P_wdata1
      sc%P_wdata2    = usma%P_wdata2
      sc%nbans     = usma%nbans
      sc%P_culturean = usma%P_culturean
      sc%P_usm       = usma%P_nomSimulation

! 18/09/2012 j'enleve l'allocatable pour Record
!      allocate(p(sc%P_nbplantes))
!      allocate(itk(sc%P_nbplantes))

! TODO : si pb d'allocation, message d'erreur et interruption

!      if (.not. allocated(p) .or. .not.allocated(itk)) then
      if (sc%P_nbplantes.gt.nb_plant_max) then
        call EnvoyerMsgHistorique(5001,sc%P_nbplantes)
       ! stop
       call exit(9)
      endif


!      call debug_init(sc%P_nbplantes)


    ! on zéro-ifie les structures plante et itk
      do i = 1, sc%P_nbplantes
      ! instance plante
      ! TODO : Plante_Zero()
        p(i)%ipl = i
      ! instance itk
        call ITK_Zero(itk(i))
        itk(i)%ipl = i
      ! fichiers relatifs à la plante (plante, ITK, LAIs)
        p(i)%P_fplt = usma%P_fplt(i)
        itk(i)%P_ftec = usma%P_ftec(i)
        p(i)%P_flai = usma%P_flai(i)
      end do


    ! lectures (lectures*, lecsol, lecoptim, lecturesSt2, lectureVariablesRapport, ..)

if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'before sticslectures'


      call Stics_Lectures(sc,pg,p,itk,soil,c,sta,t,usma)
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'after sticslectures'


!dr 14092011
        sc%ifwater_courant =sc%P_ifwater


      do i = 1, sc%P_nbplantes
      ! DR 03/03/08 pour la prairie perenne
      ! DR 17/06/08 pour la prairie climator je mets en option à voir si on gardera apres
      ! DR 08/10/09 on remplace codeoutsti par un test sur P_stade0
        if (p(i)%P_codeplante == 'fou'         &
              .and. itk(i)%P_codefauche == 1     &
              .and. itk(i)%lecfauche           &
              .and. (itk(i)%P_codemodfauche == 3 .or. itk(i)%P_codemodfauche == 2)) then
! DR et Fr 11/02/2015 on ne saute les coupes de la premiere annee que si on est en annee de semis
!          if (p(i)%P_stade0 /= 'snu' .and. p(i)%P_stade0 /= 'plt') then
! dr et FR 16/02/2015 on veut y passer dans tous les cas pour initialiser les sommes ou dates de coupes
!          if (p(i)%P_stade0 == 'snu') then
           ! DR et Fr 20/07/2016 on garde les valeurs initiales pour la prairie dans le cas ou elle meurt on va repartir sur les valeurs initiales
           ! call Initialisation_PrairiePerenne(sc,itk(i),i,p(i)%P_stade0)
            call Initialisation_PrairiePerenne(sc,itk(i),i,p(i)%P_stade0, p(i)%P_lai0,  &
             p(i)%P_masec0, p(i)%P_magrain0, p(i)%P_zrac0, p(i)%P_QNplante0,p(i)%P_resperenne0,&
             p(i)%P_densinitial)
          !endif
          endif
      end do

! c DR 02/02/2010 je regarde d'abord si ca lit bien les paraemtres
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'parameters are readed'
!      stop
    ! Préparation à la boucle des années
    ! Adaptation des Mo au CC
      call Stics_Initialisation_Boucle_Annees(sc,p,pg,itk,t) !DR 19/07/2012 c, sta et soil n'est pas utilisé

    ! Boucle des années
!write(618,*)'avant Stics_Boucle_Annees',sc%Nndec
! DR 18/07/2012 on ajoute usma pour avoir les noms de fichier dans le bilan
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'calling of the annual loop'
! DR 17/10/2013 on ajoute le module patho pour couplage avec Mila
!DR 07/02/2014 on supprime
!      call Stics_Boucle_Annees(sc,p,pg,itk,c,sta,soil,t,usma,patho,pis)
      call Stics_Boucle_Annees(sc,p,pg,itk,c,sta,soil,t,usma)


    ! TODO: Fermeture des fichiers de sorties
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )write(*,*)'fin'
!     call flush(6)
if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG) >0 )then
    close(sc%ficdbg)
    close(sc%ficdbg2)
endif

! Fin du programme.
! return
write(*,*)'The execution has been successfully accomplished'

end program Stics_Programme
 
 
