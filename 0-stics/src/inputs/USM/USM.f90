! Module of USM
!- Description  of the structure USM_
!- reading of the simulation description (files: plant, soil, itk, ...) in travail.usm
module USM

!: On utilise le module Sorties
  USE Messages


!: On force la déclaration de toutes les variables
  implicit none
  ! 18/09/2012 j'enleve les allocatable et je mets une taille maxi
  integer, parameter  :: nb_plant_max = 2


!: The derived type'USM_'
  type USM_
! DR 18/07/2012 je redimmensionne les noms des fichiers de 25 à 50 pour prendre en compte les noms de fichiers pour optimistics
    character(len=12)       :: P_codesimul !   // PARAMETER // simulation code (culture ou feuille=lai forcé) // SD // P_USM/USMXML // 0
    integer                 :: codoptim   ! code optimisation : 0 (pas d'opti), 1 (opti plante principale), 2 (opti plante associée)
    integer                 :: P_codesuite  !   // PARAMETER // code for successive P_USM ( 1=yes, 0=no) // SD // P_USM/USMXML // 0
    character(len=40)       :: P_nomSimulation !  // PARAMETER // name of the P_USM // SD // P_USM // 0
    integer                 :: P_iwater !   // PARAMETER // julian day of the beginning of the simulation // jour julien // P_USM // 1
    integer                 :: P_ifwater !   // PARAMETER // julian day of the end of simulation // julian day // P_USM // 1
    integer                 :: P_nbplantes !  // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0

    character(len=50)       :: P_ficInit !  // PARAMETER // name of the initialization file  // SD // P_USM // 0
    integer                 :: P_ichsl  !   // PARAMETER // soil numer in the  param.soil  file // SD // P_USM // 1
    character(len=50)       :: P_nomSol !  // PARAMETER // name of soil // SD // P_USM/USMXML // 0
    character(len=50)       :: P_ficStation !  // PARAMETER // name of station file // SD // P_USM // 0
    character(len=50)       :: P_wdata1 !   // PARAMETER // name of the beginning climate file // SD // P_USM // 0
    character(len=50)       :: P_wdata2 !   // PARAMETER // name of the ending climate file // SD // P_USM // 0
    integer                 :: nbans ! le nombre d'années de la simulation
    integer                 :: P_culturean !  // PARAMETER // crop status 1 = over 1 calendar year ,other than 1  = on two calendar years (winter crop in northern hemisphere) // code 0/1 // P_USM/USMXML // 0

!    character(len=50),dimension(:),allocatable :: P_fplt !   // PARAMETER // name of the plant file // SD // P_USM/USMXML // 0
!    character(len=50),dimension(:),allocatable :: P_ftec !   // PARAMETER // name of the technique file // SD // P_USM/USMXML // 0
!    character(len=50),dimension(:),allocatable :: P_flai !   // PARAMETER // name of the file LAI // SD // P_USM/USMXML // 0
  ! 18/09/2012 j'enleve les allocatable et je mets une taille maxi
    character(len=50),dimension(nb_plant_max) :: P_fplt !   // PARAMETER // name of the plant file // SD // P_USM/USMXML // 0
    character(len=50),dimension(nb_plant_max) :: P_ftec !   // PARAMETER // name of the technique file // SD // P_USM/USMXML // 0
    character(len=50),dimension(nb_plant_max) :: P_flai !   // PARAMETER // name of the file LAI // SD // P_USM/USMXML // 0

    integer :: file_Method_Optimistics_Station; !   // method to read the name of the station file for optimistics
    integer :: file_Method_Optimistics_Init; !   // method to read the name of the Init file for optimistics
    integer :: file_Method_Optimistics_Sol; !   // method to read the name of the sol file for optimistics
    integer,dimension(nb_plant_max) :: file_Method_Optimistics_Tec; !   // method to read the name of the tec file for optimistics
    integer,dimension(nb_plant_max) :: file_Method_Optimistics_Plt; !   // method to read the name of the plt file for optimistics
    integer,dimension(nb_plant_max) :: file_Method_Optimistics_lai; !   // method to read the name of the lai file for optimistics


  end type USM_

contains

!****f* USM_Lire
! NAME
!   USM_Lire - Fonction de lecture d'une P_USM
!
! DESCRIPTION
!
!***
  subroutine USM_Lire(P_usm, path, pathusm )

!  USE Stics , only : path, pathusm
!  USE Stics


 !     type(stics_communs_), intent(in) :: sc !enabling_record
      type(USM_), intent(INOUT) :: P_usm  ! // PARAMETER // name of the P_USM // SD // USMXML // 0 


   ! enabling_record :le chemin pour accéder à la config
   character(len=255), intent(IN) :: path ! enabling_record
   ! enabling_record :le chemin pour accéder directement à la config
   character(len=255), intent(IN) :: pathusm ! enabling_record


      character(len=25) :: vartmp ! variable de lecture temporaire.  
      integer :: i ! variable temporaire  
      integer :: i_xml
! DR le 21/06/2017 pour David et la calibration Atcha , je force le LAI pour qi le fichier est present sinon on ne peut pas faire d'optimisation
!  ou de lancement en usms independantes
! ******* forcage LAI à regarder quand on modifiera l'usm pour ajouter un code : codeforcage_lai
!      integer :: i_lai
    ! on utilise une variable logique pour tester l'existence du fichier
      logical::existe  

! DR 19/11/2013 pour record
      ! enabling_record : to get the full path
      integer ib0                                                   ! enabling_record
      integer ib1                                                   ! enabling_record
      character(len=300) :: filepluspath                            ! enabling_record
      ib0 = len_trim(pathusm)                                    ! enabling_record
      if (ib0 .ne. 0 ) then                                         ! enabling_record
         filepluspath =  pathusm                                 ! enabling_record
      else
         ib1 = len_trim(path)                                       ! enabling_record
         if (ib1 .eq. 0 ) then                                         ! enabling_record
            filepluspath = "new_travail.usm"                           ! enabling_record
         else                                                          ! enabling_record
            filepluspath = path(1:ib1) // '/' // "new_travail.usm"  ! enabling_record
         endif
      endif                                                         ! enabling_record
! fin record

    ! on teste l'existance de mon_fichier
!      inquire( file="new_travail.usm", exist=existe)
      inquire( file = filepluspath, exist = existe)                 ! enabling_record

    ! la réponse est dans la variable existe
      if ( existe ) Then ! existe = .true.
      ! Ouverture du fichier des données de l'unité de simulation
!        open(unit=11,action='read',file='new_travail.usm',status='old')
        open(unit=11,action='read',file=filepluspath,status='old') ! enabling_record
      else ! existe = .false.
        call EnvoyerMsgHistorique(6011)
        !stop
        call exit(9)
      endif


! ** Le type de simulation ('feuille' pour forcer le lai à partir d'un fichier)
! ** lecture d'un code determinant le mode de simulation
! *- P_codesimul = 'culture' --> mode normal
! *- P_codesimul = 'feuille' --> mode feuille (1 année maxi)
      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_codesimul


!: Lecture du code d'optimisation
      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%codoptim

!: Lecture du code de poursuite de simulation
      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_codesuite

!: Lecture des paramètres de la simulation
      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_nbplantes

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_nomSimulation

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_iwater

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_ifwater

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_ficInit

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_ichsl

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_nomsol

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_ficStation

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_wdata1

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_wdata2

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%nbans

      read(11,*,err=9999) vartmp
      read(11,*,err=9999) P_usm%P_culturean

!   18/09/2012 j'enleve les allocatable
!: Avant de lire les noms des fichiers, on alloue les tableaux de l'instance du type USM_
!      allocate(P_usm%P_ftec(P_usm%P_nbplantes))
!      allocate(P_usm%P_fplt(P_usm%P_nbplantes))
!      allocate(P_usm%P_flai(P_usm%P_nbplantes))

      do i = 1, P_usm%P_nbplantes

        read(11,*,err=9999) vartmp
        read(11,*,err=9999) P_usm%P_fplt(i)
      ! fic_plante
        i_xml=index(P_usm%P_fplt(i),'.xml')
        if(i_xml.eq.0)P_usm%file_Method_Optimistics_Plt(i)=1

        read(11,*,err=9999) vartmp
        read(11,*,err=9999) P_usm%P_ftec(i)
      ! fictec
        i_xml=index(P_usm%P_ftec(i),'.xml')
        if(i_xml.eq.0)P_usm%file_Method_Optimistics_Tec(i)=1

! DR 23/10/07 je crois que Pul l'avait mis en attente il faut le reactiver
        read(11,*,err=9999) vartmp
        read(11,*,err=9999) P_usm%P_flai(i)
      ! ficlai !!!regarder car ce n'est pas un xml
        i_xml=index(P_usm%P_flai(i),'.xml')
        if(i_xml.eq.0)P_usm%file_Method_Optimistics_lai(i)=1


! DR le 21/06/2017 pour David et la calibration Atcha , je force le LAI pour qi le fichier est present sinon on ne peut pas faire d'optimisation
!  ou de lancement en usms independantes
! ******* forcage LAI à regarder quand on modifiera l'usm pour ajouter un code : codeforcage_lai
!        i_lai=index(P_usm%P_flai(i),'.lai')
!        if(i_lai.ne.0.and.P_usm%P_flai(i).ne.'default.lai'.and.P_usm%P_flai(i).ne.'null')then
!            P_usm%P_codesimul='feuille'
!            call EnvoyerMsgHistorique(157)
!        endif


      end do

! DR 17/07/2012 pour gerer les lectures de fichiers parametres pour Optimistics on regarde si les noms de fichiers sont des xml ou pas
! fic_init
      i_xml=index(P_usm%P_ficInit,'.xml')
      if(i_xml.eq.0)P_usm%file_Method_Optimistics_Init=1
      ! fic_station
      i_xml=index(P_usm%P_ficStation,'.xml')
      if(i_xml.eq.0)P_usm%file_Method_Optimistics_Station=1
! DR 10/09/2012 j'ajoute le nom du fichier sol , ici c'etait le nom du sol mais il ne servait à rien donc on peut l'utiliser
      ! fic_sol
      i_xml=index(P_usm%P_nomsol,'.sol')
      if(i_xml>0)P_usm%file_Method_Optimistics_Sol=1


      close(11)
      return

!:!! Erreur dans la lecture du fichier P_USM, on stoppe !
9999  call EnvoyerMsgHistorique(156)
      close(11)
      !stop
      call exit(9)

  end subroutine USM_Lire

end module USM
 
 
