! **************************************************************
!    Getting parameters values for a table of parameters names
!
! **************************************************************
!
!  MODIFICATIONS (last commit)
!    $Date$
!    $Author$
!    $Revision$
!
!***************************************************************

subroutine CorrespondanceParametresEntree(sc,pg,p,itk,soil,sta,t,nbpar,tabNomPars,tabValPars,innumplt)

USE iso_varying_string

USE Stics
USE Plante
USE Itineraire_Technique
USE Sol
USE Station
USE Parametres_Generaux
USE Divers
USE Messages

  implicit none

  type(Stics_Communs_),       intent(IN) :: sc

  type(Parametres_Generaux_), intent(IN) :: pg

  type(Plante_),              intent(IN) :: p (sc%P_nbplantes)

  type(ITK_),                 intent(IN) :: itk (sc%P_nbplantes)

  type(Sol_),                 intent(IN) :: soil

  type(Station_),             intent(IN) :: sta

  type(Stics_Transit_),       intent(IN) :: t !(sc%P_nbplantes)

  integer,                     intent(IN) :: nbpar
!  character(len=19),          intent(IN)      :: tabNomPars(nbpar)
  type(varying_string),        intent(IN)      :: tabNomPars (nbpar)

  real,                       intent(OUT)     :: tabValPars(nbpar)

  integer,                     optional :: innumplt


  integer :: i
  integer :: numplt

  character(30) :: nom

  ! initialization
  tabValPars(:)=0.

  if (present(innumplt)) then
     numplt=innumplt
  else
     numplt=1
  endif

B1:    do i = 1,nbpar
         nom=tabNomPars(i)

    if (tabNomPars(i) == 'aangst') then
      tabValPars(i) = sta%P_aangst
      CYCLE
    endif
    if (tabNomPars(i) == 'abscission') then
      tabValPars(i) = p(numplt)%P_abscission
      CYCLE
    endif
    if (tabNomPars(i) == 'aclim') then
      tabValPars(i) = sta%P_aclim
      CYCLE
    endif
    if (tabNomPars(i) == 'adens') then
      tabValPars(i) = p(numplt)%P_adens(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'adfol') then
      tabValPars(i) = p(numplt)%P_adfol
      CYCLE
    endif
    if (tabNomPars(i) == 'adil') then
      tabValPars(i) = p(numplt)%P_adil
      CYCLE
    endif
    if (tabNomPars(i) == 'adilmax') then
      tabValPars(i) = p(numplt)%P_adilmax
      CYCLE
    endif
    if (tabNomPars(i) == 'afpf') then
      tabValPars(i) = p(numplt)%P_afpf
      CYCLE
    endif
    if (tabNomPars(i) == 'afruitpot') then
      tabValPars(i) = p(numplt)%P_afruitpot(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(1)') then
      tabValPars(i) = pg%P_ahres(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(2)') then
      tabValPars(i) = pg%P_ahres(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(3)') then
      tabValPars(i) = pg%P_ahres(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(4)') then
      tabValPars(i) = pg%P_ahres(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(5)') then
      tabValPars(i) = pg%P_ahres(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(6)') then
      tabValPars(i) = pg%P_ahres(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(7)') then
      tabValPars(i) = pg%P_ahres(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(8)') then
      tabValPars(i) = pg%P_ahres(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(9)') then
      tabValPars(i) = pg%P_ahres(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(10)') then
      tabValPars(i) = pg%P_ahres(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(11)') then
      tabValPars(i) = pg%P_ahres(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(12)') then
      tabValPars(i) = pg%P_ahres(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(13)') then
      tabValPars(i) = pg%P_ahres(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(14)') then
      tabValPars(i) = pg%P_ahres(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(15)') then
      tabValPars(i) = pg%P_ahres(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(16)') then
      tabValPars(i) = pg%P_ahres(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(17)') then
      tabValPars(i) = pg%P_ahres(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(18)') then
      tabValPars(i) = pg%P_ahres(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(19)') then
      tabValPars(i) = pg%P_ahres(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(20)') then
      tabValPars(i) = pg%P_ahres(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'ahres(21)') then
      tabValPars(i) = pg%P_ahres(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(1)') then
      tabValPars(i) = pg%P_akres(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(2)') then
      tabValPars(i) = pg%P_akres(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(3)') then
      tabValPars(i) = pg%P_akres(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(4)') then
      tabValPars(i) = pg%P_akres(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(5)') then
      tabValPars(i) = pg%P_akres(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(6)') then
      tabValPars(i) = pg%P_akres(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(7)') then
      tabValPars(i) = pg%P_akres(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(8)') then
      tabValPars(i) = pg%P_akres(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(9)') then
      tabValPars(i) = pg%P_akres(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(10)') then
      tabValPars(i) = pg%P_akres(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(11)') then
      tabValPars(i) = pg%P_akres(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(12)') then
      tabValPars(i) = pg%P_akres(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(13)') then
      tabValPars(i) = pg%P_akres(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(14)') then
      tabValPars(i) = pg%P_akres(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(15)') then
      tabValPars(i) = pg%P_akres(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(16)') then
      tabValPars(i) = pg%P_akres(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(17)') then
      tabValPars(i) = pg%P_akres(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(18)') then
      tabValPars(i) = pg%P_akres(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(19)') then
      tabValPars(i) = pg%P_akres(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(20)') then
      tabValPars(i) = pg%P_akres(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'akres(21)') then
      tabValPars(i) = pg%P_akres(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'aks') then
      tabValPars(i) = sta%P_aks
      CYCLE
    endif
    if (tabNomPars(i) == 'albedo') then
      tabValPars(i) = soil%P_albedo
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchplastique') then
      tabValPars(i) = itk(numplt)%P_albedomulchplastique
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(1)') then
      tabValPars(i) = pg%P_albedomulchresidus(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(2)') then
      tabValPars(i) = pg%P_albedomulchresidus(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(3)') then
      tabValPars(i) = pg%P_albedomulchresidus(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(4)') then
      tabValPars(i) = pg%P_albedomulchresidus(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(5)') then
      tabValPars(i) = pg%P_albedomulchresidus(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(6)') then
      tabValPars(i) = pg%P_albedomulchresidus(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(7)') then
      tabValPars(i) = pg%P_albedomulchresidus(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(8)') then
      tabValPars(i) = pg%P_albedomulchresidus(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(9)') then
      tabValPars(i) = pg%P_albedomulchresidus(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(10)') then
      tabValPars(i) = pg%P_albedomulchresidus(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(11)') then
      tabValPars(i) = pg%P_albedomulchresidus(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(12)') then
      tabValPars(i) = pg%P_albedomulchresidus(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(13)') then
      tabValPars(i) = pg%P_albedomulchresidus(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(14)') then
      tabValPars(i) = pg%P_albedomulchresidus(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(15)') then
      tabValPars(i) = pg%P_albedomulchresidus(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(16)') then
      tabValPars(i) = pg%P_albedomulchresidus(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(17)') then
      tabValPars(i) = pg%P_albedomulchresidus(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(18)') then
      tabValPars(i) = pg%P_albedomulchresidus(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(19)') then
      tabValPars(i) = pg%P_albedomulchresidus(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(20)') then
      tabValPars(i) = pg%P_albedomulchresidus(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'albedomulchresidus(21)') then
      tabValPars(i) = pg%P_albedomulchresidus(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'albveg') then
      tabValPars(i) = sta%P_albveg
      CYCLE
    endif
    if (tabNomPars(i) == 'allocfrmax') then
      tabValPars(i) = p(numplt)%P_allocfrmax
      CYCLE
    endif
    if (tabNomPars(i) == 'alphaCO2') then
      tabValPars(i) = p(numplt)%P_alphaCO2
      CYCLE
    endif
    if (tabNomPars(i) == 'alphapH') then
      tabValPars(i) = pg%P_alphapH
      CYCLE
    endif
    if (tabNomPars(i) == 'alphaphot') then
      tabValPars(i) = p(numplt)%P_alphaphot
      CYCLE
    endif
    if (tabNomPars(i) == 'alphapt') then
      tabValPars(i) = sta%P_alphapt
      CYCLE
    endif
    if (tabNomPars(i) == 'altinversion') then
      tabValPars(i) = sta%P_altinversion
      CYCLE
    endif
    if (tabNomPars(i) == 'altisimul') then
      tabValPars(i) = sta%P_altisimul
      CYCLE
    endif
    if (tabNomPars(i) == 'altistation') then
      tabValPars(i) = sta%P_altistation
      CYCLE
    endif
    if (tabNomPars(i) == 'ampfroid') then
      tabValPars(i) = p(numplt)%P_ampfroid
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(1)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(2)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(3)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(4)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(5)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(6)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(7)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(8)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(9)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(10)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(11)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(12)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(13)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(14)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(15)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(16)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(17)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(18)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(19)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'anitcoupe(20)') then
      tabValPars(i) = itk(numplt)%P_anitcoupe(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'argi') then
      tabValPars(i) = soil%P_argi
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(1)') then
      tabValPars(i) = pg%P_awb(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(2)') then
      tabValPars(i) = pg%P_awb(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(3)') then
      tabValPars(i) = pg%P_awb(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(4)') then
      tabValPars(i) = pg%P_awb(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(5)') then
      tabValPars(i) = pg%P_awb(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(6)') then
      tabValPars(i) = pg%P_awb(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(7)') then
      tabValPars(i) = pg%P_awb(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(8)') then
      tabValPars(i) = pg%P_awb(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(9)') then
      tabValPars(i) = pg%P_awb(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(10)') then
      tabValPars(i) = pg%P_awb(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(11)') then
      tabValPars(i) = pg%P_awb(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(12)') then
      tabValPars(i) = pg%P_awb(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(13)') then
      tabValPars(i) = pg%P_awb(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(14)') then
      tabValPars(i) = pg%P_awb(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(15)') then
      tabValPars(i) = pg%P_awb(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(16)') then
      tabValPars(i) = pg%P_awb(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(17)') then
      tabValPars(i) = pg%P_awb(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(18)') then
      tabValPars(i) = pg%P_awb(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(19)') then
      tabValPars(i) = pg%P_awb(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(20)') then
      tabValPars(i) = pg%P_awb(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'awb(21)') then
      tabValPars(i) = pg%P_awb(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'bangst') then
      tabValPars(i) = sta%P_bangst
      CYCLE
    endif
    if (tabNomPars(i) == 'bdens') then
      tabValPars(i) = p(numplt)%P_bdens
      CYCLE
    endif
    if (tabNomPars(i) == 'bdil') then
      tabValPars(i) = p(numplt)%P_bdil
      CYCLE
    endif
    if (tabNomPars(i) == 'bdilmax') then
      tabValPars(i) = p(numplt)%P_bdilmax
      CYCLE
    endif
    if (tabNomPars(i) == 'belong') then
      tabValPars(i) = p(numplt)%P_belong
      CYCLE
    endif
    if (tabNomPars(i) == 'beta') then
      tabValPars(i) = pg%P_beta
      CYCLE
    endif
    if (tabNomPars(i) == 'bformnappe') then
      tabValPars(i) = pg%P_bformnappe
      CYCLE
    endif
    if (tabNomPars(i) == 'bfpf') then
      tabValPars(i) = p(numplt)%P_bfpf
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(1)') then
      tabValPars(i) = pg%P_bhres(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(2)') then
      tabValPars(i) = pg%P_bhres(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(3)') then
      tabValPars(i) = pg%P_bhres(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(4)') then
      tabValPars(i) = pg%P_bhres(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(5)') then
      tabValPars(i) = pg%P_bhres(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(6)') then
      tabValPars(i) = pg%P_bhres(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(7)') then
      tabValPars(i) = pg%P_bhres(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(8)') then
      tabValPars(i) = pg%P_bhres(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(9)') then
      tabValPars(i) = pg%P_bhres(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(10)') then
      tabValPars(i) = pg%P_bhres(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(11)') then
      tabValPars(i) = pg%P_bhres(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(12)') then
      tabValPars(i) = pg%P_bhres(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(13)') then
      tabValPars(i) = pg%P_bhres(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(14)') then
      tabValPars(i) = pg%P_bhres(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(15)') then
      tabValPars(i) = pg%P_bhres(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(16)') then
      tabValPars(i) = pg%P_bhres(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(17)') then
      tabValPars(i) = pg%P_bhres(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(18)') then
      tabValPars(i) = pg%P_bhres(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(19)') then
      tabValPars(i) = pg%P_bhres(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(20)') then
      tabValPars(i) = pg%P_bhres(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'bhres(21)') then
      tabValPars(i) = pg%P_bhres(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'biorognem') then
      tabValPars(i) = itk(numplt)%P_biorognem
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(1)') then
      tabValPars(i) = pg%P_bkres(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(2)') then
      tabValPars(i) = pg%P_bkres(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(3)') then
      tabValPars(i) = pg%P_bkres(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(4)') then
      tabValPars(i) = pg%P_bkres(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(5)') then
      tabValPars(i) = pg%P_bkres(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(6)') then
      tabValPars(i) = pg%P_bkres(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(7)') then
      tabValPars(i) = pg%P_bkres(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(8)') then
      tabValPars(i) = pg%P_bkres(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(9)') then
      tabValPars(i) = pg%P_bkres(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(10)') then
      tabValPars(i) = pg%P_bkres(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(11)') then
      tabValPars(i) = pg%P_bkres(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(12)') then
      tabValPars(i) = pg%P_bkres(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(13)') then
      tabValPars(i) = pg%P_bkres(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(14)') then
      tabValPars(i) = pg%P_bkres(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(15)') then
      tabValPars(i) = pg%P_bkres(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(16)') then
      tabValPars(i) = pg%P_bkres(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(17)') then
      tabValPars(i) = pg%P_bkres(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(18)') then
      tabValPars(i) = pg%P_bkres(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(19)') then
      tabValPars(i) = pg%P_bkres(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(20)') then
      tabValPars(i) = pg%P_bkres(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'bkres(21)') then
      tabValPars(i) = pg%P_bkres(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'bks') then
      tabValPars(i) = sta%P_bks
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(1)') then
      tabValPars(i) = pg%P_bwb(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(2)') then
      tabValPars(i) = pg%P_bwb(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(3)') then
      tabValPars(i) = pg%P_bwb(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(4)') then
      tabValPars(i) = pg%P_bwb(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(5)') then
      tabValPars(i) = pg%P_bwb(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(6)') then
      tabValPars(i) = pg%P_bwb(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(7)') then
      tabValPars(i) = pg%P_bwb(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(8)') then
      tabValPars(i) = pg%P_bwb(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(9)') then
      tabValPars(i) = pg%P_bwb(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(10)') then
      tabValPars(i) = pg%P_bwb(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(11)') then
      tabValPars(i) = pg%P_bwb(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(12)') then
      tabValPars(i) = pg%P_bwb(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(13)') then
      tabValPars(i) = pg%P_bwb(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(14)') then
      tabValPars(i) = pg%P_bwb(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(15)') then
      tabValPars(i) = pg%P_bwb(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(16)') then
      tabValPars(i) = pg%P_bwb(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(17)') then
      tabValPars(i) = pg%P_bwb(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(18)') then
      tabValPars(i) = pg%P_bwb(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(19)') then
      tabValPars(i) = pg%P_bwb(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(20)') then
      tabValPars(i) = pg%P_bwb(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'bwb(21)') then
      tabValPars(i) = pg%P_bwb(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'cadencerec') then
      tabValPars(i) = float(itk(numplt)%P_cadencerec)
      CYCLE
    endif
    if (tabNomPars(i) == 'cailloux(1)') then
      tabValPars(i) = soil%P_cailloux(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'cailloux(2)') then
      tabValPars(i) = soil%P_cailloux(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'cailloux(3)') then
      tabValPars(i) = soil%P_cailloux(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'cailloux(4)') then
      tabValPars(i) = soil%P_cailloux(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'cailloux(5)') then
      tabValPars(i) = soil%P_cailloux(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'calc') then
      tabValPars(i) = soil%P_calc
      CYCLE
    endif
    if (tabNomPars(i) == 'capiljour') then
      tabValPars(i) = soil%P_capiljour
      CYCLE
    endif
    if (tabNomPars(i) == 'celong') then
      tabValPars(i) = p(numplt)%P_celong
      CYCLE
    endif
    if (tabNomPars(i) == 'cfes') then
      tabValPars(i) = soil%P_cfes
      CYCLE
    endif
    if (tabNomPars(i) == 'cfpf') then
      tabValPars(i) = p(numplt)%P_cfpf
      CYCLE
    endif
    if (tabNomPars(i) == 'cgrain') then
      tabValPars(i) = p(numplt)%P_cgrain
      CYCLE
    endif
    if (tabNomPars(i) == 'cgrainv0') then
      tabValPars(i) = p(numplt)%P_cgrainv0
      CYCLE
    endif
    if (tabNomPars(i) == 'cielclair') then
      tabValPars(i) = sta%P_cielclair
      CYCLE
    endif
    if (tabNomPars(i) == 'cmax_pdenit') then
      tabValPars(i) = pg%P_cmax_pdenit
      CYCLE
    endif
    if (tabNomPars(i) == 'cmin_pdenit') then
      tabValPars(i) = pg%P_cmin_pdenit
      CYCLE
    endif
    if (tabNomPars(i) == 'CNgrainrec') then
      tabValPars(i) = itk(numplt)%P_CNgrainrec
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(1)') then
      tabValPars(i) = pg%P_CNresmax(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(2)') then
      tabValPars(i) = pg%P_CNresmax(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(3)') then
      tabValPars(i) = pg%P_CNresmax(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(4)') then
      tabValPars(i) = pg%P_CNresmax(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(5)') then
      tabValPars(i) = pg%P_CNresmax(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(6)') then
      tabValPars(i) = pg%P_CNresmax(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(7)') then
      tabValPars(i) = pg%P_CNresmax(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(8)') then
      tabValPars(i) = pg%P_CNresmax(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(9)') then
      tabValPars(i) = pg%P_CNresmax(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(10)') then
      tabValPars(i) = pg%P_CNresmax(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(11)') then
      tabValPars(i) = pg%P_CNresmax(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(12)') then
      tabValPars(i) = pg%P_CNresmax(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(13)') then
      tabValPars(i) = pg%P_CNresmax(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(14)') then
      tabValPars(i) = pg%P_CNresmax(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(15)') then
      tabValPars(i) = pg%P_CNresmax(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(16)') then
      tabValPars(i) = pg%P_CNresmax(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(17)') then
      tabValPars(i) = pg%P_CNresmax(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(18)') then
      tabValPars(i) = pg%P_CNresmax(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(19)') then
      tabValPars(i) = pg%P_CNresmax(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(20)') then
      tabValPars(i) = pg%P_CNresmax(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmax(21)') then
      tabValPars(i) = pg%P_CNresmax(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(1)') then
      tabValPars(i) = pg%P_CNresmin(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(2)') then
      tabValPars(i) = pg%P_CNresmin(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(3)') then
      tabValPars(i) = pg%P_CNresmin(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(4)') then
      tabValPars(i) = pg%P_CNresmin(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(5)') then
      tabValPars(i) = pg%P_CNresmin(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(6)') then
      tabValPars(i) = pg%P_CNresmin(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(7)') then
      tabValPars(i) = pg%P_CNresmin(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(8)') then
      tabValPars(i) = pg%P_CNresmin(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(9)') then
      tabValPars(i) = pg%P_CNresmin(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(10)') then
      tabValPars(i) = pg%P_CNresmin(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(11)') then
      tabValPars(i) = pg%P_CNresmin(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(12)') then
      tabValPars(i) = pg%P_CNresmin(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(13)') then
      tabValPars(i) = pg%P_CNresmin(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(14)') then
      tabValPars(i) = pg%P_CNresmin(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(15)') then
      tabValPars(i) = pg%P_CNresmin(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(16)') then
      tabValPars(i) = pg%P_CNresmin(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(17)') then
      tabValPars(i) = pg%P_CNresmin(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(18)') then
      tabValPars(i) = pg%P_CNresmin(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(19)') then
      tabValPars(i) = pg%P_CNresmin(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(20)') then
      tabValPars(i) = pg%P_CNresmin(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'CNresmin(21)') then
      tabValPars(i) = pg%P_CNresmin(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'codabri') then
      tabValPars(i) = float(itk(numplt)%P_codabri)
      CYCLE
    endif
    if (tabNomPars(i) == 'codadret') then
      tabValPars(i) = float(sta%P_codadret)
      CYCLE
    endif
    if (tabNomPars(i) == 'codaltitude') then
      tabValPars(i) = float(sta%P_codaltitude)
      CYCLE
    endif
    if (tabNomPars(i) == 'codazofruit') then
      tabValPars(i) = float(p(numplt)%P_codazofruit)
      CYCLE
    endif
    if (tabNomPars(i) == 'codazorac') then
      tabValPars(i) = float(p(numplt)%P_codazorac)
      CYCLE
    endif
    if (tabNomPars(i) == 'codcaleffeuil') then
      tabValPars(i) = float(itk(numplt)%P_codcaleffeuil)
      CYCLE
    endif
    if (tabNomPars(i) == 'codcalinflo') then
      tabValPars(i) = float(p(numplt)%P_codcalinflo)
      CYCLE
    endif
    if (tabNomPars(i) == 'codcalrogne') then
      tabValPars(i) = float(itk(numplt)%P_codcalrogne)
      CYCLE
    endif
    if (tabNomPars(i) == 'codcueille') then
      tabValPars(i) = float(itk(numplt)%P_codcueille)
      CYCLE
    endif
    if (tabNomPars(i) == 'code_hourly_wfps_denit') then
      tabValPars(i) = float(pg%P_code_hourly_wfps_denit)
      CYCLE
    endif
    if (tabNomPars(i) == 'code_hourly_wfps_nit') then
      tabValPars(i) = float(pg%P_code_hourly_wfps_nit)
      CYCLE
    endif
    if (tabNomPars(i) == 'code_pdenit') then
      tabValPars(i) = float(pg%P_code_pdenit)
      CYCLE
    endif
    if (tabNomPars(i) == 'code_ratiodenit') then
      tabValPars(i) = float(pg%P_code_ratiodenit)
      CYCLE
    endif
    if (tabNomPars(i) == 'code_rationit') then
      tabValPars(i) = float(pg%P_code_rationit)
      CYCLE
    endif
    if (tabNomPars(i) == 'code_tnit') then
      tabValPars(i) = float(pg%P_code_tnit)
      CYCLE
    endif
    if (tabNomPars(i) == 'code_vnit') then
      tabValPars(i) = float(pg%P_code_vnit)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeactimulch') then
      tabValPars(i) = float(pg%P_codeactimulch)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeaumin') then
      tabValPars(i) = float(itk(numplt)%P_codeaumin)
      CYCLE
    endif
    if (tabNomPars(i) == 'codebeso') then
      tabValPars(i) = float(p(numplt)%P_codebeso)
      CYCLE
    endif
    if (tabNomPars(i) == 'codebfroid') then
      tabValPars(i) = float(p(numplt)%P_codebfroid)
      CYCLE
    endif
    if (tabNomPars(i) == 'codecailloux') then
      tabValPars(i) = float(soil%P_codecailloux)
      CYCLE
    endif
    if (tabNomPars(i) == 'codecalferti') then
      tabValPars(i) = float(t%P_codecalferti)
      CYCLE
    endif
    if (tabNomPars(i) == 'codecalirrig') then
      tabValPars(i) = float(itk(numplt)%P_codecalirrig)
      CYCLE
    endif
    if (tabNomPars(i) == 'codecaltemp') then
      tabValPars(i) = float(sta%P_codecaltemp)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeclaircie') then
      tabValPars(i) = float(itk(numplt)%P_codeclaircie)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeclichange') then
      tabValPars(i) = float(sta%P_codeclichange)
      CYCLE
    endif
    if (tabNomPars(i) == 'codedate_irrigauto') then
      tabValPars(i) = float(t%P_codedate_irrigauto)
      CYCLE
    endif
    if (tabNomPars(i) == 'codedateappH2O') then
      tabValPars(i) = float(itk(numplt)%P_codedateappH2O)
      CYCLE
    endif
    if (tabNomPars(i) == 'codedateappN') then
      tabValPars(i) = float(itk(numplt)%P_codedateappN)
      CYCLE
    endif
    if (tabNomPars(i) == 'codedecirecolte') then
      tabValPars(i) = float(itk(numplt)%P_codedecirecolte)
      CYCLE
    endif
    if (tabNomPars(i) == 'codedecisemis') then
      tabValPars(i) = float(itk(numplt)%P_codedecisemis)
      CYCLE
    endif
    if (tabNomPars(i) == 'codedenit') then
      tabValPars(i) = float(soil%P_codedenit)
      CYCLE
    endif
    if (tabNomPars(i) == 'codedormance') then
      tabValPars(i) = float(p(numplt)%P_codedormance)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeDST') then
      tabValPars(i) = float(itk(numplt)%P_codeDST)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeDSTnbcouche') then
      tabValPars(i) = float(itk(numplt)%P_codeDSTnbcouche)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeDSTtass') then
      tabValPars(i) = float(itk(numplt)%P_codeDSTtass)
      CYCLE
    endif
    if (tabNomPars(i) == 'codedyntalle(1)') then
      tabValPars(i) = float(t%P_codedyntalle(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'codedyntalle(2)') then
      tabValPars(i) = float(t%P_codedyntalle(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'codeetp') then
      tabValPars(i) = float(sta%P_codeetp)
      CYCLE
    endif
    if (tabNomPars(i) == 'codefauche') then
      tabValPars(i) = float(itk(numplt)%P_codefauche)
      CYCLE
    endif
    if (tabNomPars(i) == 'codefente') then
      tabValPars(i) = float(soil%P_codefente)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeffeuil') then
      tabValPars(i) = float(itk(numplt)%P_codeffeuil)
      CYCLE
    endif
    if (tabNomPars(i) == 'codefixpot') then
      tabValPars(i) = float(p(numplt)%P_codefixpot)
      CYCLE
    endif
    if (tabNomPars(i) == 'codefracappN') then
      tabValPars(i) = float(itk(numplt)%P_codefracappN)
      CYCLE
    endif
    if (tabNomPars(i) == 'codefrmur') then
      tabValPars(i) = float(pg%P_codefrmur)
      CYCLE
    endif
    if (tabNomPars(i) == 'codefxn') then
      tabValPars(i) = float(pg%P_codefxn)
      CYCLE
    endif
    if (tabNomPars(i) == 'codegdh') then
      tabValPars(i) = float(p(numplt)%P_codegdh)
      CYCLE
    endif
    if (tabNomPars(i) == 'codegdhdeb') then
      tabValPars(i) = float(p(numplt)%P_codegdhdeb)
      CYCLE
    endif
    if (tabNomPars(i) == 'codegermin') then
      tabValPars(i) = float(p(numplt)%P_codegermin)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeh2oact') then
      tabValPars(i) = float(pg%P_codeh2oact)
      CYCLE
    endif
    if (tabNomPars(i) == 'codehypo') then
      tabValPars(i) = float(p(numplt)%P_codehypo)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeindetermin') then
      tabValPars(i) = float(p(numplt)%P_codeindetermin)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeinitprec') then
      tabValPars(i) = float(pg%P_codeinitprec)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeINN') then
      tabValPars(i) = float(p(numplt)%P_codeINN)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeinnact') then
      tabValPars(i) = float(pg%P_codeinnact)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeintercept') then
      tabValPars(i) = float(p(numplt)%P_codeintercept)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeir') then
      tabValPars(i) = float(p(numplt)%P_codeir)
      CYCLE
    endif
    if (tabNomPars(i) == 'codelaitr') then
      tabValPars(i) = float(p(numplt)%P_codelaitr)
      CYCLE
    endif
    if (tabNomPars(i) == 'codelegume') then
      tabValPars(i) = float(p(numplt)%P_codelegume)
      CYCLE
    endif
    if (tabNomPars(i) == 'codemacropor') then
      tabValPars(i) = float(soil%P_codemacropor)
      CYCLE
    endif
    if (tabNomPars(i) == 'codemicheur') then
      tabValPars(i) = float(pg%P_codemicheur)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeminopt') then
      tabValPars(i) = float(pg%P_codeminopt)
      CYCLE
    endif
    if (tabNomPars(i) == 'codemodfauche') then
      tabValPars(i) = float(itk(numplt)%P_codemodfauche)
      CYCLE
    endif
    if (tabNomPars(i) == 'codemonocot') then
      tabValPars(i) = float(p(numplt)%P_codemonocot)
      CYCLE
    endif
    if (tabNomPars(i) == 'codemontaison(1)') then
      tabValPars(i) = float(t%P_codemontaison(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'codemontaison(2)') then
      tabValPars(i) = float(t%P_codemontaison(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'codemortalracine') then
      tabValPars(i) = float(t%P_codemortalracine)
      CYCLE
    endif
    if (tabNomPars(i) == 'codemsfinal') then
      tabValPars(i) = float(pg%P_codemsfinal)
      CYCLE
    endif
    if (tabNomPars(i) == 'codenitrif') then
      tabValPars(i) = float(soil%P_codenitrif)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeNmindec') then
      tabValPars(i) = float(t%P_codeNmindec)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeoutscient') then
      tabValPars(i) = float(pg%P_codeoutscient)
      CYCLE
    endif
    if (tabNomPars(i) == 'codepaillage') then
      tabValPars(i) = float(itk(numplt)%P_codepaillage)
      CYCLE
    endif
    if (tabNomPars(i) == 'codepalissage') then
      tabValPars(i) = float(itk(numplt)%P_codepalissage)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeperenne') then
      tabValPars(i) = float(p(numplt)%P_codeperenne)
      CYCLE
    endif
    if (tabNomPars(i) == 'codephot') then
      tabValPars(i) = float(p(numplt)%P_codephot)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeplisoleN') then
      tabValPars(i) = float(p(numplt)%P_codeplisoleN)
      CYCLE
    endif
    if (tabNomPars(i) == 'codepluiepoquet') then
      tabValPars(i) = float(t%P_codepluiepoquet)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeprofmes') then
      tabValPars(i) = float(pg%P_codeprofmes)
      CYCLE
    endif
    if (tabNomPars(i) == 'coderacine') then
      tabValPars(i) = float(p(numplt)%P_coderacine)
      CYCLE
    endif
    if (tabNomPars(i) == 'coderecolteassoc') then
      tabValPars(i) = float(itk(numplt)%P_coderecolteassoc)
      CYCLE
    endif
    if (tabNomPars(i) == 'coderemontcap') then
      tabValPars(i) = float(soil%P_coderemontcap)
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(1)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(2)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(3)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(4)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(5)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(6)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(7)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(8)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(9)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(10)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes(11)') then
      tabValPars(i) = float(itk(numplt)%P_coderes(11))
      CYCLE
    endif
    if (tabNomPars(i) == 'coderes_pature') then
      tabValPars(i) = float(t%P_coderes_pature)
      CYCLE
    endif
    if (tabNomPars(i) == 'coderetflo') then
      tabValPars(i) = float(p(numplt)%P_coderetflo)
      CYCLE
    endif
    if (tabNomPars(i) == 'codernet') then
      tabValPars(i) = float(sta%P_codernet)
      CYCLE
    endif
    if (tabNomPars(i) == 'codesensibilite') then
      tabValPars(i) = float(pg%P_codesensibilite)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeseprapport') then
      tabValPars(i) = float(pg%P_codeseprapport)
      CYCLE
    endif
    if (tabNomPars(i) == 'codestade') then
      tabValPars(i) = float(itk(numplt)%P_codestade)
      CYCLE
    endif
    if (tabNomPars(i) == 'codestrphot') then
      tabValPars(i) = float(p(numplt)%P_codestrphot)
      CYCLE
    endif
    if (tabNomPars(i) == 'codeSWDRH') then
      tabValPars(i) = float(t%P_codeSWDRH)
      CYCLE
    endif
    if (tabNomPars(i) == 'codesymbiose') then
      tabValPars(i) = float(pg%P_codesymbiose)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetaille') then
      tabValPars(i) = float(itk(numplt)%P_codetaille)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetemp') then
      tabValPars(i) = float(p(numplt)%P_codetemp)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetempfauche') then
      tabValPars(i) = float(t%P_codetempfauche)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetemprac') then
      tabValPars(i) = float(p(numplt)%P_codetemprac)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetesthumN') then
      tabValPars(i) = float(t%P_codetesthumN)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetradtec') then
      tabValPars(i) = float(itk(numplt)%P_codetradtec)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetranspitalle') then
      tabValPars(i) = float(t%P_codetranspitalle)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetransrad') then
      tabValPars(i) = float(p(numplt)%P_codetransrad)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetremp') then
      tabValPars(i) = float(p(numplt)%P_codetremp)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetrosee') then
      tabValPars(i) = float(t%P_codetrosee)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetycailloux') then
      tabValPars(i) = float(pg%P_codetycailloux)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetypeng') then
      tabValPars(i) = float(pg%P_codetypeng)
      CYCLE
    endif
    if (tabNomPars(i) == 'codetypres') then
      tabValPars(i) = float(pg%P_codetypres)
      CYCLE
    endif
    if (tabNomPars(i) == 'codgelflo') then
      tabValPars(i) = float(p(numplt)%P_codgelflo)
      CYCLE
    endif
    if (tabNomPars(i) == 'codgeljuv') then
      tabValPars(i) = float(p(numplt)%P_codgeljuv)
      CYCLE
    endif
    if (tabNomPars(i) == 'codgellev') then
      tabValPars(i) = float(p(numplt)%P_codgellev)
      CYCLE
    endif
    if (tabNomPars(i) == 'codgelveg') then
      tabValPars(i) = float(p(numplt)%P_codgelveg)
      CYCLE
    endif
    if (tabNomPars(i) == 'codhauteff') then
      tabValPars(i) = float(itk(numplt)%P_codhauteff)
      CYCLE
    endif
    if (tabNomPars(i) == 'codhnappe') then
      tabValPars(i) = float(pg%P_codhnappe)
      CYCLE
    endif
    if (tabNomPars(i) == 'codlainet') then
      tabValPars(i) = float(p(numplt)%P_codlainet)
      CYCLE
    endif
    if (tabNomPars(i) == 'codlocferti') then
      tabValPars(i) = float(itk(numplt)%P_codlocferti)
      CYCLE
    endif
    if (tabNomPars(i) == 'codlocirrig') then
      tabValPars(i) = float(itk(numplt)%P_codlocirrig)
      CYCLE
    endif
    if (tabNomPars(i) == 'codoptim') then
      tabValPars(i) = float(sc%codoptim)
      CYCLE
    endif
    if (tabNomPars(i) == 'codrainage') then
      tabValPars(i) = float(soil%P_codrainage)
      CYCLE
    endif
    if (tabNomPars(i) == 'codrecolte') then
      tabValPars(i) = float(itk(numplt)%P_codrecolte)
      CYCLE
    endif
    if (tabNomPars(i) == 'codrognage') then
      tabValPars(i) = float(itk(numplt)%P_codrognage)
      CYCLE
    endif
    if (tabNomPars(i) == 'codtrophrac') then
      tabValPars(i) = float(p(numplt)%P_codtrophrac)
      CYCLE
    endif
    if (tabNomPars(i) == 'coef_calcul_doseN') then
      tabValPars(i) = t%P_coef_calcul_doseN
      CYCLE
    endif
    if (tabNomPars(i) == 'coef_calcul_qres') then
      tabValPars(i) = t%P_coef_calcul_qres
      CYCLE
    endif
    if (tabNomPars(i) == 'coefamflax') then
      tabValPars(i) = p(numplt)%P_coefamflax
      CYCLE
    endif
    if (tabNomPars(i) == 'coefb') then
      tabValPars(i) = pg%P_coefb
      CYCLE
    endif
    if (tabNomPars(i) == 'coefdevil') then
      tabValPars(i) = sta%P_coefdevil
      CYCLE
    endif
    if (tabNomPars(i) == 'coefdrpmat') then
      tabValPars(i) = p(numplt)%P_coefdrpmat
      CYCLE
    endif
    if (tabNomPars(i) == 'coefflodrp') then
      tabValPars(i) = p(numplt)%P_coefflodrp
      CYCLE
    endif
    if (tabNomPars(i) == 'coeflaxsen') then
      tabValPars(i) = p(numplt)%P_coeflaxsen
      CYCLE
    endif
    if (tabNomPars(i) == 'coeflevamf') then
      tabValPars(i) = p(numplt)%P_coeflevamf
      CYCLE
    endif
    if (tabNomPars(i) == 'coeflevdrp') then
      tabValPars(i) = p(numplt)%P_coeflevdrp
      CYCLE
    endif
    if (tabNomPars(i) == 'coefmshaut') then
      tabValPars(i) = p(numplt)%P_coefmshaut
      CYCLE
    endif
    if (tabNomPars(i) == 'coefracoupe(1)') then
      tabValPars(i) = t%P_coefracoupe(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'coefracoupe(2)') then
      tabValPars(i) = t%P_coefracoupe(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'coefrnet') then
      tabValPars(i) = sta%P_coefrnet
      CYCLE
    endif
    if (tabNomPars(i) == 'coefsenlan') then
      tabValPars(i) = p(numplt)%P_coefsenlan
      CYCLE
    endif
    if (tabNomPars(i) == 'concirr') then
      tabValPars(i) = itk(numplt)%P_concirr
      CYCLE
    endif
    if (tabNomPars(i) == 'concNnodseuil') then
      tabValPars(i) = p(numplt)%P_concNnodseuil
      CYCLE
    endif
    if (tabNomPars(i) == 'concNrac0') then
      tabValPars(i) = p(numplt)%P_concNrac0
      CYCLE
    endif
    if (tabNomPars(i) == 'concNrac100') then
      tabValPars(i) = p(numplt)%P_concNrac100
      CYCLE
    endif
    if (tabNomPars(i) == 'concrr') then
      tabValPars(i) = pg%P_concrr
      CYCLE
    endif
    if (tabNomPars(i) == 'concseuil') then
      tabValPars(i) = soil%P_concseuil
      CYCLE
    endif
    if (tabNomPars(i) == 'contrdamax') then
      tabValPars(i) = p(numplt)%P_contrdamax
      CYCLE
    endif
    if (tabNomPars(i) == 'corecTrosee') then
      tabValPars(i) = sta%P_corecTrosee
      CYCLE
    endif
    if (tabNomPars(i) == 'couvermulchplastique') then
      tabValPars(i) = itk(numplt)%P_couvermulchplastique
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(1)') then
      tabValPars(i) = itk(numplt)%P_Crespc(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(2)') then
      tabValPars(i) = itk(numplt)%P_Crespc(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(3)') then
      tabValPars(i) = itk(numplt)%P_Crespc(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(4)') then
      tabValPars(i) = itk(numplt)%P_Crespc(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(5)') then
      tabValPars(i) = itk(numplt)%P_Crespc(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(6)') then
      tabValPars(i) = itk(numplt)%P_Crespc(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(7)') then
      tabValPars(i) = itk(numplt)%P_Crespc(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(8)') then
      tabValPars(i) = itk(numplt)%P_Crespc(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(9)') then
      tabValPars(i) = itk(numplt)%P_Crespc(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(10)') then
      tabValPars(i) = itk(numplt)%P_Crespc(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc(11)') then
      tabValPars(i) = itk(numplt)%P_Crespc(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'Crespc_pature') then
      tabValPars(i) = t%P_Crespc_pature
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(1)') then
      tabValPars(i) = pg%P_CroCo(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(2)') then
      tabValPars(i) = pg%P_CroCo(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(3)') then
      tabValPars(i) = pg%P_CroCo(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(4)') then
      tabValPars(i) = pg%P_CroCo(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(5)') then
      tabValPars(i) = pg%P_CroCo(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(6)') then
      tabValPars(i) = pg%P_CroCo(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(7)') then
      tabValPars(i) = pg%P_CroCo(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(8)') then
      tabValPars(i) = pg%P_CroCo(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(9)') then
      tabValPars(i) = pg%P_CroCo(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(10)') then
      tabValPars(i) = pg%P_CroCo(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(11)') then
      tabValPars(i) = pg%P_CroCo(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(12)') then
      tabValPars(i) = pg%P_CroCo(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(13)') then
      tabValPars(i) = pg%P_CroCo(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(14)') then
      tabValPars(i) = pg%P_CroCo(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(15)') then
      tabValPars(i) = pg%P_CroCo(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(16)') then
      tabValPars(i) = pg%P_CroCo(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(17)') then
      tabValPars(i) = pg%P_CroCo(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(18)') then
      tabValPars(i) = pg%P_CroCo(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(19)') then
      tabValPars(i) = pg%P_CroCo(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(20)') then
      tabValPars(i) = pg%P_CroCo(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'CroCo(21)') then
      tabValPars(i) = pg%P_CroCo(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'croirac') then
      tabValPars(i) = p(numplt)%P_croirac(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(1)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(2)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(3)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(4)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(5)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(6)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(7)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(8)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(9)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(10)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNres(11)') then
      tabValPars(i) = itk(numplt)%P_CsurNres(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'CsurNsol') then
      tabValPars(i) = soil%P_CsurNsol
      CYCLE
    endif
    if (tabNomPars(i) == 'cvent') then
      tabValPars(i) = sta%P_cvent
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(1)') then
      tabValPars(i) = pg%P_cwb(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(2)') then
      tabValPars(i) = pg%P_cwb(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(3)') then
      tabValPars(i) = pg%P_cwb(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(4)') then
      tabValPars(i) = pg%P_cwb(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(5)') then
      tabValPars(i) = pg%P_cwb(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(6)') then
      tabValPars(i) = pg%P_cwb(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(7)') then
      tabValPars(i) = pg%P_cwb(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(8)') then
      tabValPars(i) = pg%P_cwb(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(9)') then
      tabValPars(i) = pg%P_cwb(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(10)') then
      tabValPars(i) = pg%P_cwb(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(11)') then
      tabValPars(i) = pg%P_cwb(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(12)') then
      tabValPars(i) = pg%P_cwb(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(13)') then
      tabValPars(i) = pg%P_cwb(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(14)') then
      tabValPars(i) = pg%P_cwb(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(15)') then
      tabValPars(i) = pg%P_cwb(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(16)') then
      tabValPars(i) = pg%P_cwb(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(17)') then
      tabValPars(i) = pg%P_cwb(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(18)') then
      tabValPars(i) = pg%P_cwb(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(19)') then
      tabValPars(i) = pg%P_cwb(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(20)') then
      tabValPars(i) = pg%P_cwb(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'cwb(21)') then
      tabValPars(i) = pg%P_cwb(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'dachisel') then
      tabValPars(i) = itk(numplt)%P_dachisel
      CYCLE
    endif
    if (tabNomPars(i) == 'dacohes') then
      tabValPars(i) = pg%P_dacohes
      CYCLE
    endif
    if (tabNomPars(i) == 'DAF(1)') then
      tabValPars(i) = soil%P_DAF(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'DAF(2)') then
      tabValPars(i) = soil%P_DAF(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'DAF(3)') then
      tabValPars(i) = soil%P_DAF(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'DAF(4)') then
      tabValPars(i) = soil%P_DAF(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'DAF(5)') then
      tabValPars(i) = soil%P_DAF(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'dalabour') then
      tabValPars(i) = itk(numplt)%P_dalabour
      CYCLE
    endif
    if (tabNomPars(i) == 'darecolte') then
      tabValPars(i) = itk(numplt)%P_darecolte
      CYCLE
    endif
    if (tabNomPars(i) == 'dasemis') then
      tabValPars(i) = itk(numplt)%P_dasemis
      CYCLE
    endif
    if (tabNomPars(i) == 'daseuilbas') then
      tabValPars(i) = pg%P_daseuilbas
      CYCLE
    endif
    if (tabNomPars(i) == 'daseuilhaut') then
      tabValPars(i) = pg%P_daseuilhaut
      CYCLE
    endif
    if (tabNomPars(i) == 'datedeb_irrigauto') then
      tabValPars(i) = float(t%P_datedeb_irrigauto)
      CYCLE
    endif
    if (tabNomPars(i) == 'datefin_irrigauto') then
      tabValPars(i) = float(t%P_datefin_irrigauto)
      CYCLE
    endif
    if (tabNomPars(i) == 'debsenrac') then
      tabValPars(i) = p(numplt)%P_debsenrac
      CYCLE
    endif
    if (tabNomPars(i) == 'deneng(1)') then
      tabValPars(i) = pg%P_deneng(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'deneng(2)') then
      tabValPars(i) = pg%P_deneng(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'deneng(3)') then
      tabValPars(i) = pg%P_deneng(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'deneng(4)') then
      tabValPars(i) = pg%P_deneng(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'deneng(5)') then
      tabValPars(i) = pg%P_deneng(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'deneng(6)') then
      tabValPars(i) = pg%P_deneng(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'deneng(7)') then
      tabValPars(i) = pg%P_deneng(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'deneng(8)') then
      tabValPars(i) = pg%P_deneng(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'densinitial(1)') then
      tabValPars(i) = p(numplt)%P_densinitial(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'densinitial(2)') then
      tabValPars(i) = p(numplt)%P_densinitial(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'densinitial(3)') then
      tabValPars(i) = p(numplt)%P_densinitial(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'densinitial(4)') then
      tabValPars(i) = p(numplt)%P_densinitial(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'densinitial(5)') then
      tabValPars(i) = p(numplt)%P_densinitial(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'densitesem') then
      tabValPars(i) = itk(numplt)%P_densitesem
      CYCLE
    endif
    if (tabNomPars(i) == 'deshydbase') then
      tabValPars(i) = p(numplt)%P_deshydbase
      CYCLE
    endif
    if (tabNomPars(i) == 'dfolbas') then
      tabValPars(i) = p(numplt)%P_dfolbas
      CYCLE
    endif
    if (tabNomPars(i) == 'dfolhaut') then
      tabValPars(i) = p(numplt)%P_dfolhaut
      CYCLE
    endif
    if (tabNomPars(i) == 'dfpf') then
      tabValPars(i) = p(numplt)%P_dfpf
      CYCLE
    endif
    if (tabNomPars(i) == 'difN') then
      tabValPars(i) = pg%P_difN
      CYCLE
    endif
    if (tabNomPars(i) == 'diftherm') then
      tabValPars(i) = pg%P_diftherm
      CYCLE
    endif
    if (tabNomPars(i) == 'distdrain') then
      tabValPars(i) = pg%P_distdrain
      CYCLE
    endif
    if (tabNomPars(i) == 'dlaimax') then
      tabValPars(i) = p(numplt)%P_dlaimax
      CYCLE
    endif
    if (tabNomPars(i) == 'dlaimaxbrut') then
      tabValPars(i) = p(numplt)%P_dlaimaxbrut
      CYCLE
    endif
    if (tabNomPars(i) == 'dlaimin') then
      tabValPars(i) = p(numplt)%P_dlaimin
      CYCLE
    endif
    if (tabNomPars(i) == 'dltamsmaxsen') then
      tabValPars(i) = p(numplt)%P_dltamsmaxsen
      CYCLE
    endif
    if (tabNomPars(i) == 'dltamsminsen') then
      tabValPars(i) = p(numplt)%P_dltamsminsen
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(1)') then
      tabValPars(i) = itk(numplt)%P_doseI(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(2)') then
      tabValPars(i) = itk(numplt)%P_doseI(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(3)') then
      tabValPars(i) = itk(numplt)%P_doseI(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(4)') then
      tabValPars(i) = itk(numplt)%P_doseI(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(5)') then
      tabValPars(i) = itk(numplt)%P_doseI(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(6)') then
      tabValPars(i) = itk(numplt)%P_doseI(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(7)') then
      tabValPars(i) = itk(numplt)%P_doseI(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(8)') then
      tabValPars(i) = itk(numplt)%P_doseI(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(9)') then
      tabValPars(i) = itk(numplt)%P_doseI(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(10)') then
      tabValPars(i) = itk(numplt)%P_doseI(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(11)') then
      tabValPars(i) = itk(numplt)%P_doseI(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(12)') then
      tabValPars(i) = itk(numplt)%P_doseI(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(13)') then
      tabValPars(i) = itk(numplt)%P_doseI(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(14)') then
      tabValPars(i) = itk(numplt)%P_doseI(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(15)') then
      tabValPars(i) = itk(numplt)%P_doseI(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(16)') then
      tabValPars(i) = itk(numplt)%P_doseI(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(17)') then
      tabValPars(i) = itk(numplt)%P_doseI(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(18)') then
      tabValPars(i) = itk(numplt)%P_doseI(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(19)') then
      tabValPars(i) = itk(numplt)%P_doseI(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(20)') then
      tabValPars(i) = itk(numplt)%P_doseI(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(21)') then
      tabValPars(i) = itk(numplt)%P_doseI(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(22)') then
      tabValPars(i) = itk(numplt)%P_doseI(22)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(23)') then
      tabValPars(i) = itk(numplt)%P_doseI(23)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(24)') then
      tabValPars(i) = itk(numplt)%P_doseI(24)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(25)') then
      tabValPars(i) = itk(numplt)%P_doseI(25)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(26)') then
      tabValPars(i) = itk(numplt)%P_doseI(26)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(27)') then
      tabValPars(i) = itk(numplt)%P_doseI(27)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(28)') then
      tabValPars(i) = itk(numplt)%P_doseI(28)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(29)') then
      tabValPars(i) = itk(numplt)%P_doseI(29)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseI(30)') then
      tabValPars(i) = itk(numplt)%P_doseI(30)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseirrigmin') then
      tabValPars(i) = itk(numplt)%P_doseirrigmin
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(1)') then
      tabValPars(i) = itk(numplt)%P_doseN(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(2)') then
      tabValPars(i) = itk(numplt)%P_doseN(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(3)') then
      tabValPars(i) = itk(numplt)%P_doseN(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(4)') then
      tabValPars(i) = itk(numplt)%P_doseN(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(5)') then
      tabValPars(i) = itk(numplt)%P_doseN(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(6)') then
      tabValPars(i) = itk(numplt)%P_doseN(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(7)') then
      tabValPars(i) = itk(numplt)%P_doseN(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(8)') then
      tabValPars(i) = itk(numplt)%P_doseN(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(9)') then
      tabValPars(i) = itk(numplt)%P_doseN(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(10)') then
      tabValPars(i) = itk(numplt)%P_doseN(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(11)') then
      tabValPars(i) = itk(numplt)%P_doseN(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(12)') then
      tabValPars(i) = itk(numplt)%P_doseN(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(13)') then
      tabValPars(i) = itk(numplt)%P_doseN(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(14)') then
      tabValPars(i) = itk(numplt)%P_doseN(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(15)') then
      tabValPars(i) = itk(numplt)%P_doseN(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(16)') then
      tabValPars(i) = itk(numplt)%P_doseN(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(17)') then
      tabValPars(i) = itk(numplt)%P_doseN(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(18)') then
      tabValPars(i) = itk(numplt)%P_doseN(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(19)') then
      tabValPars(i) = itk(numplt)%P_doseN(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'doseN(20)') then
      tabValPars(i) = itk(numplt)%P_doseN(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'dosimx') then
      tabValPars(i) = itk(numplt)%P_dosimx
      CYCLE
    endif
    if (tabNomPars(i) == 'dosimxN') then
      tabValPars(i) = t%P_dosimxN
      CYCLE
    endif
    if (tabNomPars(i) == 'dpHvolmax') then
      tabValPars(i) = pg%P_dpHvolmax
      CYCLE
    endif
    if (tabNomPars(i) == 'draclong') then
      tabValPars(i) = p(numplt)%P_draclong
      CYCLE
    endif
    if (tabNomPars(i) == 'dureefruit') then
      tabValPars(i) = p(numplt)%P_dureefruit(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'durvieF') then
      tabValPars(i) = p(numplt)%P_durvieF(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'durviesupmax') then
      tabValPars(i) = p(numplt)%P_durviesupmax
      CYCLE
    endif
    if (tabNomPars(i) == 'eau_mini_decisemis') then
      tabValPars(i) = float(t%P_eau_mini_decisemis)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(1)') then
      tabValPars(i) = itk(numplt)%P_eaures(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(2)') then
      tabValPars(i) = itk(numplt)%P_eaures(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(3)') then
      tabValPars(i) = itk(numplt)%P_eaures(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(4)') then
      tabValPars(i) = itk(numplt)%P_eaures(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(5)') then
      tabValPars(i) = itk(numplt)%P_eaures(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(6)') then
      tabValPars(i) = itk(numplt)%P_eaures(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(7)') then
      tabValPars(i) = itk(numplt)%P_eaures(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(8)') then
      tabValPars(i) = itk(numplt)%P_eaures(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(9)') then
      tabValPars(i) = itk(numplt)%P_eaures(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(10)') then
      tabValPars(i) = itk(numplt)%P_eaures(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures(11)') then
      tabValPars(i) = itk(numplt)%P_eaures(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'eaures_pature') then
      tabValPars(i) = t%P_eaures_pature
      CYCLE
    endif
    if (tabNomPars(i) == 'ecartdrain') then
      tabValPars(i) = soil%P_ecartdrain
      CYCLE
    endif
    if (tabNomPars(i) == 'efcroijuv') then
      tabValPars(i) = p(numplt)%P_efcroijuv
      CYCLE
    endif
    if (tabNomPars(i) == 'efcroirepro') then
      tabValPars(i) = p(numplt)%P_efcroirepro
      CYCLE
    endif
    if (tabNomPars(i) == 'efcroiveg') then
      tabValPars(i) = p(numplt)%P_efcroiveg
      CYCLE
    endif
    if (tabNomPars(i) == 'effeuil') then
      tabValPars(i) = itk(numplt)%P_effeuil
      CYCLE
    endif
    if (tabNomPars(i) == 'effirr') then
      tabValPars(i) = itk(numplt)%P_effirr
      CYCLE
    endif
    if (tabNomPars(i) == 'elmax') then
      tabValPars(i) = p(numplt)%P_elmax
      CYCLE
    endif
    if (tabNomPars(i) == 'engamm(1)') then
      tabValPars(i) = pg%P_engamm(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'engamm(2)') then
      tabValPars(i) = pg%P_engamm(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'engamm(3)') then
      tabValPars(i) = pg%P_engamm(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'engamm(4)') then
      tabValPars(i) = pg%P_engamm(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'engamm(5)') then
      tabValPars(i) = pg%P_engamm(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'engamm(6)') then
      tabValPars(i) = pg%P_engamm(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'engamm(7)') then
      tabValPars(i) = pg%P_engamm(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'engamm(8)') then
      tabValPars(i) = pg%P_engamm(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(1)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(2)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(3)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(4)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(5)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(6)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(7)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(8)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(9)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais(10)') then
      tabValPars(i) = float(itk(numplt)%P_engrais(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'engrais_pature') then
      tabValPars(i) = float(t%P_engrais_pature)
      CYCLE
    endif
    if (tabNomPars(i) == 'envfruit') then
      tabValPars(i) = p(numplt)%P_envfruit
      CYCLE
    endif
    if (tabNomPars(i) == 'epc(1)') then
      tabValPars(i) = soil%P_epc(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'epc(2)') then
      tabValPars(i) = soil%P_epc(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'epc(3)') then
      tabValPars(i) = soil%P_epc(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'epc(4)') then
      tabValPars(i) = soil%P_epc(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'epc(5)') then
      tabValPars(i) = soil%P_epc(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'epd(1)') then
      tabValPars(i) = float(soil%P_epd(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'epd(2)') then
      tabValPars(i) = float(soil%P_epd(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'epd(3)') then
      tabValPars(i) = float(soil%P_epd(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'epd(4)') then
      tabValPars(i) = float(soil%P_epd(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'epd(5)') then
      tabValPars(i) = float(soil%P_epd(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'extin') then
      tabValPars(i) = p(numplt)%P_extin
      CYCLE
    endif
    if (tabNomPars(i) == 'fhminsat') then
      tabValPars(i) = pg%P_fhminsat
      CYCLE
    endif
    if (tabNomPars(i) == 'finert') then
      tabValPars(i) = pg%P_finert
      CYCLE
    endif
    if (tabNomPars(i) == 'fixmax') then
      tabValPars(i) = p(numplt)%P_fixmax
      CYCLE
    endif
    if (tabNomPars(i) == 'fixmaxgr') then
      tabValPars(i) = p(numplt)%P_fixmaxgr
      CYCLE
    endif
    if (tabNomPars(i) == 'fixmaxveg') then
      tabValPars(i) = p(numplt)%P_fixmaxveg
      CYCLE
    endif
    if (tabNomPars(i) == 'flagecriture') then
      tabValPars(i) = float(pg%P_flagecriture)
      CYCLE
    endif
    if (tabNomPars(i) == 'fmin1') then
      tabValPars(i) = pg%P_fmin1
      CYCLE
    endif
    if (tabNomPars(i) == 'fmin2') then
      tabValPars(i) = pg%P_fmin2
      CYCLE
    endif
    if (tabNomPars(i) == 'fmin3') then
      tabValPars(i) = pg%P_fmin3
      CYCLE
    endif
    if (tabNomPars(i) == 'fNCbiomin') then
      tabValPars(i) = pg%P_fNCbiomin
      CYCLE
    endif
    if (tabNomPars(i) == 'fNmindecmin') then
      tabValPars(i) = t%P_fNmindecmin
      CYCLE
    endif
    if (tabNomPars(i) == 'fnx') then
      tabValPars(i) = pg%P_fnx
      CYCLE
    endif
    if (tabNomPars(i) == 'forme') then
      tabValPars(i) = float(p(numplt)%P_forme)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(1)') then
      tabValPars(i) = itk(numplt)%P_fracN(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(2)') then
      tabValPars(i) = itk(numplt)%P_fracN(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(3)') then
      tabValPars(i) = itk(numplt)%P_fracN(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(4)') then
      tabValPars(i) = itk(numplt)%P_fracN(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(5)') then
      tabValPars(i) = itk(numplt)%P_fracN(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(6)') then
      tabValPars(i) = itk(numplt)%P_fracN(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(7)') then
      tabValPars(i) = itk(numplt)%P_fracN(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(8)') then
      tabValPars(i) = itk(numplt)%P_fracN(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(9)') then
      tabValPars(i) = itk(numplt)%P_fracN(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'fracN(10)') then
      tabValPars(i) = itk(numplt)%P_fracN(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'fredkN') then
      tabValPars(i) = pg%P_fredkN
      CYCLE
    endif
    if (tabNomPars(i) == 'fredlN') then
      tabValPars(i) = pg%P_fredlN
      CYCLE
    endif
    if (tabNomPars(i) == 'fredNsup') then
      tabValPars(i) = pg%P_fredNsup
      CYCLE
    endif
    if (tabNomPars(i) == 'ftemh') then
      tabValPars(i) = pg%P_ftemh
      CYCLE
    endif
    if (tabNomPars(i) == 'ftemha') then
      tabValPars(i) = pg%P_ftemha
      CYCLE
    endif
    if (tabNomPars(i) == 'ftemr') then
      tabValPars(i) = pg%P_ftemr
      CYCLE
    endif
    if (tabNomPars(i) == 'ftemra') then
      tabValPars(i) = pg%P_ftemra
      CYCLE
    endif
    if (tabNomPars(i) == 'gradtn') then
      tabValPars(i) = sta%P_gradtn
      CYCLE
    endif
    if (tabNomPars(i) == 'gradtninv') then
      tabValPars(i) = sta%P_gradtninv
      CYCLE
    endif
    if (tabNomPars(i) == 'gradtx') then
      tabValPars(i) = sta%P_gradtx
      CYCLE
    endif
    if (tabNomPars(i) == 'h2ofeuiljaune') then
      tabValPars(i) = p(numplt)%P_h2ofeuiljaune
      CYCLE
    endif
    if (tabNomPars(i) == 'h2ofeuilverte') then
      tabValPars(i) = p(numplt)%P_h2ofeuilverte
      CYCLE
    endif
    if (tabNomPars(i) == 'h2ofrvert') then
      tabValPars(i) = p(numplt)%P_h2ofrvert
      CYCLE
    endif
    if (tabNomPars(i) == 'h2ograinmax') then
      tabValPars(i) = itk(numplt)%P_h2ograinmax
      CYCLE
    endif
    if (tabNomPars(i) == 'h2ograinmin') then
      tabValPars(i) = itk(numplt)%P_h2ograinmin
      CYCLE
    endif
    if (tabNomPars(i) == 'h2oreserve') then
      tabValPars(i) = p(numplt)%P_h2oreserve
      CYCLE
    endif
    if (tabNomPars(i) == 'h2otigestruc') then
      tabValPars(i) = p(numplt)%P_h2otigestruc
      CYCLE
    endif
    if (tabNomPars(i) == 'hautbase') then
      tabValPars(i) = p(numplt)%P_hautbase
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(1)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(2)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(3)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(4)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(5)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(6)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(7)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(8)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(9)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(10)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(11)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(12)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(13)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(14)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(15)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(16)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(17)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(18)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(19)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupe(20)') then
      tabValPars(i) = itk(numplt)%P_hautcoupe(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautcoupedefaut') then
      tabValPars(i) = itk(numplt)%P_hautcoupedefaut
      CYCLE
    endif
    if (tabNomPars(i) == 'hautmax') then
      tabValPars(i) = p(numplt)%P_hautmax
      CYCLE
    endif
    if (tabNomPars(i) == 'hautmaxtec') then
      tabValPars(i) = itk(numplt)%P_hautmaxtec
      CYCLE
    endif
    if (tabNomPars(i) == 'hautrogne') then
      tabValPars(i) = itk(numplt)%P_hautrogne
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(1)') then
      tabValPars(i) = pg%P_hcccx(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(2)') then
      tabValPars(i) = pg%P_hcccx(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(3)') then
      tabValPars(i) = pg%P_hcccx(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(4)') then
      tabValPars(i) = pg%P_hcccx(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(5)') then
      tabValPars(i) = pg%P_hcccx(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(6)') then
      tabValPars(i) = pg%P_hcccx(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(7)') then
      tabValPars(i) = pg%P_hcccx(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(8)') then
      tabValPars(i) = pg%P_hcccx(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(9)') then
      tabValPars(i) = pg%P_hcccx(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'hcccx(10)') then
      tabValPars(i) = pg%P_hcccx(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'hccf(1)') then
      tabValPars(i) = soil%P_hccf(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'hccf(2)') then
      tabValPars(i) = soil%P_hccf(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'hccf(3)') then
      tabValPars(i) = soil%P_hccf(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'hccf(4)') then
      tabValPars(i) = soil%P_hccf(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'hccf(5)') then
      tabValPars(i) = soil%P_hccf(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'Hinitf(1)') then
      tabValPars(i) = sc%P_Hinitf(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'Hinitf(2)') then
      tabValPars(i) = sc%P_Hinitf(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'Hinitf(3)') then
      tabValPars(i) = sc%P_Hinitf(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'Hinitf(4)') then
      tabValPars(i) = sc%P_Hinitf(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'Hinitf(5)') then
      tabValPars(i) = sc%P_Hinitf(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'hminf(1)') then
      tabValPars(i) = soil%P_hminf(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'hminf(2)') then
      tabValPars(i) = soil%P_hminf(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'hminf(3)') then
      tabValPars(i) = soil%P_hminf(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'hminf(4)') then
      tabValPars(i) = soil%P_hminf(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'hminf(5)') then
      tabValPars(i) = soil%P_hminf(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'hminm') then
      tabValPars(i) = pg%P_hminm
      CYCLE
    endif
    if (tabNomPars(i) == 'hminn') then
      tabValPars(i) = pg%P_hminn
      CYCLE
    endif
    if (tabNomPars(i) == 'hoptm') then
      tabValPars(i) = pg%P_hoptm
      CYCLE
    endif
    if (tabNomPars(i) == 'hoptn') then
      tabValPars(i) = pg%P_hoptn
      CYCLE
    endif
    if (tabNomPars(i) == 'huilerec') then
      tabValPars(i) = itk(numplt)%P_huilerec
      CYCLE
    endif
    if (tabNomPars(i) == 'humcapil') then
      tabValPars(i) = soil%P_humcapil
      CYCLE
    endif
    if (tabNomPars(i) == 'humirac_decisemis') then
      tabValPars(i) = t%P_humirac_decisemis
      CYCLE
    endif
    if (tabNomPars(i) == 'iamf') then
      tabValPars(i) = float(itk(numplt)%P_iamf)
      CYCLE
    endif
    if (tabNomPars(i) == 'ichsl') then
      tabValPars(i) = float(sc%P_ichsl)
      CYCLE
    endif
    if (tabNomPars(i) == 'idebdorm') then
      tabValPars(i) = float(p(numplt)%P_idebdorm)
      CYCLE
    endif
    if (tabNomPars(i) == 'idrp') then
      tabValPars(i) = float(itk(numplt)%P_idrp)
      CYCLE
    endif
    if (tabNomPars(i) == 'ifindorm') then
      tabValPars(i) = float(p(numplt)%P_ifindorm)
      CYCLE
    endif
    if (tabNomPars(i) == 'iflo') then
      tabValPars(i) = float(itk(numplt)%P_iflo)
      CYCLE
    endif
    if (tabNomPars(i) == 'ifwater') then
      tabValPars(i) = float(sc%P_ifwater)
      CYCLE
    endif
    if (tabNomPars(i) == 'ilan') then
      tabValPars(i) = float(itk(numplt)%P_ilan)
      CYCLE
    endif
    if (tabNomPars(i) == 'ilax') then
      tabValPars(i) = float(itk(numplt)%P_ilax)
      CYCLE
    endif
    if (tabNomPars(i) == 'ilev') then
      tabValPars(i) = float(itk(numplt)%P_ilev)
      CYCLE
    endif
    if (tabNomPars(i) == 'imat') then
      tabValPars(i) = float(itk(numplt)%P_imat)
      CYCLE
    endif
    if (tabNomPars(i) == 'infil(0)') then
      tabValPars(i) = soil%P_infil(0)
      CYCLE
    endif
    if (tabNomPars(i) == 'infil(1)') then
      tabValPars(i) = soil%P_infil(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'infil(2)') then
      tabValPars(i) = soil%P_infil(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'infil(3)') then
      tabValPars(i) = soil%P_infil(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'infil(4)') then
      tabValPars(i) = soil%P_infil(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'infil(5)') then
      tabValPars(i) = soil%P_infil(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'inflomax') then
      tabValPars(i) = p(numplt)%P_inflomax
      CYCLE
    endif
    if (tabNomPars(i) == 'infrecouv') then
      tabValPars(i) = p(numplt)%P_infrecouv
      CYCLE
    endif
    if (tabNomPars(i) == 'iniprofil') then
      tabValPars(i) = float(pg%P_iniprofil)
      CYCLE
    endif
    if (tabNomPars(i) == 'inngrain1') then
      tabValPars(i) = p(numplt)%P_inngrain1
      CYCLE
    endif
    if (tabNomPars(i) == 'inngrain2') then
      tabValPars(i) = p(numplt)%P_inngrain2
      CYCLE
    endif
    if (tabNomPars(i) == 'INNimin') then
      tabValPars(i) = p(numplt)%P_INNimin
      CYCLE
    endif
    if (tabNomPars(i) == 'INNmin') then
      tabValPars(i) = p(numplt)%P_INNmin
      CYCLE
    endif
    if (tabNomPars(i) == 'innsen') then
      tabValPars(i) = p(numplt)%P_innsen
      CYCLE
    endif
    if (tabNomPars(i) == 'innturgmin') then
      tabValPars(i) = p(numplt)%P_innturgmin
      CYCLE
    endif
    if (tabNomPars(i) == 'interrang') then
      tabValPars(i) = itk(numplt)%P_interrang
      CYCLE
    endif
    if (tabNomPars(i) == 'iplt0') then
      tabValPars(i) = float(itk(numplt)%P_iplt0)
      CYCLE
    endif
    if (tabNomPars(i) == 'irec') then
      tabValPars(i) = float(itk(numplt)%P_irec)
      CYCLE
    endif
    if (tabNomPars(i) == 'irecbutoir') then
      tabValPars(i) = float(itk(numplt)%P_irecbutoir)
      CYCLE
    endif
    if (tabNomPars(i) == 'irmax') then
      tabValPars(i) = p(numplt)%P_irmax
      CYCLE
    endif
    if (tabNomPars(i) == 'irrlev') then
      tabValPars(i) = pg%P_irrlev
      CYCLE
    endif
    if (tabNomPars(i) == 'isen') then
      tabValPars(i) = float(itk(numplt)%P_isen)
      CYCLE
    endif
    if (tabNomPars(i) == 'iwater') then
      tabValPars(i) = float(sc%P_iwater)
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(1)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(2)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(3)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(4)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(5)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(6)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(7)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(8)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(9)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(10)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(11)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(11))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(12)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(12))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(13)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(13))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(14)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(14))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(15)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(15))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(16)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(16))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(17)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(17))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(18)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(18))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(19)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(19))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(20)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(20))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(21)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(21))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(22)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(22))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(23)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(23))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(24)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(24))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(25)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(25))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(26)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(26))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(27)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(27))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(28)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(28))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(29)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(29))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapI(30)') then
      tabValPars(i) = float(itk(numplt)%P_julapI(30))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(1)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(2)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(3)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(4)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(5)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(6)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(7)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(8)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(9)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(10)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(11)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(11))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(12)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(12))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(13)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(13))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(14)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(14))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(15)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(15))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(16)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(16))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(17)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(17))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(18)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(18))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(19)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(19))
      CYCLE
    endif
    if (tabNomPars(i) == 'julapN(20)') then
      tabValPars(i) = float(itk(numplt)%P_julapN(20))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(1)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(2)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(3)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(4)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(5)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(6)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(7)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(8)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(9)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleclair(10)') then
      tabValPars(i) = float(itk(numplt)%P_juleclair(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'juleffeuil') then
      tabValPars(i) = float(itk(numplt)%P_juleffeuil)
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(1)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(2)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(3)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(4)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(5)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(6)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(7)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(8)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(9)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(10)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(11)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(11))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(12)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(12))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(13)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(13))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(14)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(14))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(15)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(15))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(16)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(16))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(17)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(17))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(18)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(18))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(19)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(19))
      CYCLE
    endif
    if (tabNomPars(i) == 'julfauche(20)') then
      tabValPars(i) = float(itk(numplt)%P_julfauche(20))
      CYCLE
    endif
    if (tabNomPars(i) == 'julouvre2') then
      tabValPars(i) = float(itk(numplt)%P_julouvre2)
      CYCLE
    endif
    if (tabNomPars(i) == 'julouvre3') then
      tabValPars(i) = float(itk(numplt)%P_julouvre3)
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(1)') then
      tabValPars(i) = float(itk(numplt)%P_julres(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(2)') then
      tabValPars(i) = float(itk(numplt)%P_julres(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(3)') then
      tabValPars(i) = float(itk(numplt)%P_julres(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(4)') then
      tabValPars(i) = float(itk(numplt)%P_julres(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(5)') then
      tabValPars(i) = float(itk(numplt)%P_julres(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(6)') then
      tabValPars(i) = float(itk(numplt)%P_julres(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(7)') then
      tabValPars(i) = float(itk(numplt)%P_julres(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(8)') then
      tabValPars(i) = float(itk(numplt)%P_julres(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(9)') then
      tabValPars(i) = float(itk(numplt)%P_julres(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(10)') then
      tabValPars(i) = float(itk(numplt)%P_julres(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'julres(11)') then
      tabValPars(i) = float(itk(numplt)%P_julres(11))
      CYCLE
    endif
    if (tabNomPars(i) == 'julrogne') then
      tabValPars(i) = float(itk(numplt)%P_julrogne)
      CYCLE
    endif
    if (tabNomPars(i) == 'jultaille') then
      tabValPars(i) = float(itk(numplt)%P_jultaille)
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(1)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(2)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(3)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(4)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(5)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(6)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(7)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(8)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(9)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(10)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'jultrav(11)') then
      tabValPars(i) = float(itk(numplt)%P_jultrav(11))
      CYCLE
    endif
    if (tabNomPars(i) == 'julvernal') then
      tabValPars(i) = float(p(numplt)%P_julvernal)
      CYCLE
    endif
    if (tabNomPars(i) == 'jvc') then
      tabValPars(i) = p(numplt)%P_jvc(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'jvcmini') then
      tabValPars(i) = p(numplt)%P_jvcmini
      CYCLE
    endif
    if (tabNomPars(i) == 'Kamm') then
      tabValPars(i) = pg%P_Kamm
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(1)') then
      tabValPars(i) = pg%P_kbio(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(2)') then
      tabValPars(i) = pg%P_kbio(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(3)') then
      tabValPars(i) = pg%P_kbio(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(4)') then
      tabValPars(i) = pg%P_kbio(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(5)') then
      tabValPars(i) = pg%P_kbio(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(6)') then
      tabValPars(i) = pg%P_kbio(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(7)') then
      tabValPars(i) = pg%P_kbio(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(8)') then
      tabValPars(i) = pg%P_kbio(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(9)') then
      tabValPars(i) = pg%P_kbio(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(10)') then
      tabValPars(i) = pg%P_kbio(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(11)') then
      tabValPars(i) = pg%P_kbio(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(12)') then
      tabValPars(i) = pg%P_kbio(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(13)') then
      tabValPars(i) = pg%P_kbio(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(14)') then
      tabValPars(i) = pg%P_kbio(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(15)') then
      tabValPars(i) = pg%P_kbio(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(16)') then
      tabValPars(i) = pg%P_kbio(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(17)') then
      tabValPars(i) = pg%P_kbio(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(18)') then
      tabValPars(i) = pg%P_kbio(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(19)') then
      tabValPars(i) = pg%P_kbio(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(20)') then
      tabValPars(i) = pg%P_kbio(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'kbio(21)') then
      tabValPars(i) = pg%P_kbio(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(1)') then
      tabValPars(i) = pg%P_kcouvmlch(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(2)') then
      tabValPars(i) = pg%P_kcouvmlch(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(3)') then
      tabValPars(i) = pg%P_kcouvmlch(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(4)') then
      tabValPars(i) = pg%P_kcouvmlch(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(5)') then
      tabValPars(i) = pg%P_kcouvmlch(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(6)') then
      tabValPars(i) = pg%P_kcouvmlch(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(7)') then
      tabValPars(i) = pg%P_kcouvmlch(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(8)') then
      tabValPars(i) = pg%P_kcouvmlch(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(9)') then
      tabValPars(i) = pg%P_kcouvmlch(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(10)') then
      tabValPars(i) = pg%P_kcouvmlch(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(11)') then
      tabValPars(i) = pg%P_kcouvmlch(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(12)') then
      tabValPars(i) = pg%P_kcouvmlch(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(13)') then
      tabValPars(i) = pg%P_kcouvmlch(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(14)') then
      tabValPars(i) = pg%P_kcouvmlch(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(15)') then
      tabValPars(i) = pg%P_kcouvmlch(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(16)') then
      tabValPars(i) = pg%P_kcouvmlch(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(17)') then
      tabValPars(i) = pg%P_kcouvmlch(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(18)') then
      tabValPars(i) = pg%P_kcouvmlch(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(19)') then
      tabValPars(i) = pg%P_kcouvmlch(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(20)') then
      tabValPars(i) = pg%P_kcouvmlch(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'kcouvmlch(21)') then
      tabValPars(i) = pg%P_kcouvmlch(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'Kd') then
      tabValPars(i) = pg%P_Kd
      CYCLE
    endif
    if (tabNomPars(i) == 'kdesat') then
      tabValPars(i) = pg%P_kdesat
      CYCLE
    endif
    if (tabNomPars(i) == 'khaut') then
      tabValPars(i) = pg%P_khaut
      CYCLE
    endif
    if (tabNomPars(i) == 'Kmabs1') then
      tabValPars(i) = p(numplt)%P_Kmabs1
      CYCLE
    endif
    if (tabNomPars(i) == 'Kmabs2') then
      tabValPars(i) = p(numplt)%P_Kmabs2
      CYCLE
    endif
    if (tabNomPars(i) == 'kmax') then
      tabValPars(i) = p(numplt)%P_kmax
      CYCLE
    endif
    if (tabNomPars(i) == 'krepracperm') then
      tabValPars(i) = p(numplt)%P_krepracperm
      CYCLE
    endif
    if (tabNomPars(i) == 'krepracseu') then
      tabValPars(i) = p(numplt)%P_krepracseu
      CYCLE
    endif
    if (tabNomPars(i) == 'ksol') then
      tabValPars(i) = soil%P_ksol
      CYCLE
    endif
    if (tabNomPars(i) == 'kstemflow') then
      tabValPars(i) = p(numplt)%P_kstemflow
      CYCLE
    endif
    if (tabNomPars(i) == 'ktrou') then
      tabValPars(i) = p(numplt)%P_ktrou
      CYCLE
    endif
    if (tabNomPars(i) == 'lai0') then
      tabValPars(i) = p(numplt)%P_lai0
      CYCLE
    endif
    if (tabNomPars(i) == 'laicomp') then
      tabValPars(i) = p(numplt)%P_laicomp
      CYCLE
    endif
    if (tabNomPars(i) == 'laidebeff') then
      tabValPars(i) = itk(numplt)%P_laidebeff
      CYCLE
    endif
    if (tabNomPars(i) == 'laieffeuil') then
      tabValPars(i) = itk(numplt)%P_laieffeuil
      CYCLE
    endif
    if (tabNomPars(i) == 'laiplantule') then
      tabValPars(i) = p(numplt)%P_laiplantule
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(1)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(2)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(3)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(4)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(5)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(6)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(7)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(8)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(9)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(10)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(11)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(12)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(13)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(14)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(15)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(16)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(17)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(18)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(19)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'lairesiduel(20)') then
      tabValPars(i) = itk(numplt)%P_lairesiduel(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'largrogne') then
      tabValPars(i) = itk(numplt)%P_largrogne
      CYCLE
    endif
    if (tabNomPars(i) == 'largtec') then
      tabValPars(i) = itk(numplt)%P_largtec
      CYCLE
    endif
    if (tabNomPars(i) == 'latitude') then
      tabValPars(i) = sta%P_latitude
      CYCLE
    endif
    if (tabNomPars(i) == 'locferti') then
      tabValPars(i) = float(itk(numplt)%P_locferti)
      CYCLE
    endif
    if (tabNomPars(i) == 'locirrig') then
      tabValPars(i) = float(itk(numplt)%P_locirrig)
      CYCLE
    endif
    if (tabNomPars(i) == 'longsperac') then
      tabValPars(i) = p(numplt)%P_longsperac
      CYCLE
    endif
    if (tabNomPars(i) == 'lvfront') then
      tabValPars(i) = p(numplt)%P_lvfront
      CYCLE
    endif
    if (tabNomPars(i) == 'lvopt') then
      tabValPars(i) = pg%P_lvopt
      CYCLE
    endif
    if (tabNomPars(i) == 'magrain0') then
      tabValPars(i) = p(numplt)%P_magrain0
      CYCLE
    endif
    if (tabNomPars(i) == 'margerogne') then
      tabValPars(i) = itk(numplt)%P_margerogne
      CYCLE
    endif
    if (tabNomPars(i) == 'masec0') then
      tabValPars(i) = p(numplt)%P_masec0
      CYCLE
    endif
    if (tabNomPars(i) == 'masecmeta') then
      tabValPars(i) = p(numplt)%P_masecmeta
      CYCLE
    endif
    if (tabNomPars(i) == 'masecNmax') then
      tabValPars(i) = p(numplt)%P_masecNmax
      CYCLE
    endif
    if (tabNomPars(i) == 'masecplantule') then
      tabValPars(i) = p(numplt)%P_masecplantule
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(1)') then
      tabValPars(i) = pg%P_masvolcx(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(2)') then
      tabValPars(i) = pg%P_masvolcx(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(3)') then
      tabValPars(i) = pg%P_masvolcx(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(4)') then
      tabValPars(i) = pg%P_masvolcx(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(5)') then
      tabValPars(i) = pg%P_masvolcx(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(6)') then
      tabValPars(i) = pg%P_masvolcx(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(7)') then
      tabValPars(i) = pg%P_masvolcx(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(8)') then
      tabValPars(i) = pg%P_masvolcx(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(9)') then
      tabValPars(i) = pg%P_masvolcx(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'masvolcx(10)') then
      tabValPars(i) = pg%P_masvolcx(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'max_pdenit') then
      tabValPars(i) = pg%P_max_pdenit
      CYCLE
    endif
    if (tabNomPars(i) == 'maxazorac') then
      tabValPars(i) = p(numplt)%P_maxazorac
      CYCLE
    endif
    if (tabNomPars(i) == 'maxtalle(1)') then
      tabValPars(i) = t%P_maxtalle(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'maxtalle(2)') then
      tabValPars(i) = t%P_maxtalle(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'min_pdenit') then
      tabValPars(i) = pg%P_min_pdenit
      CYCLE
    endif
    if (tabNomPars(i) == 'minazorac') then
      tabValPars(i) = p(numplt)%P_minazorac
      CYCLE
    endif
    if (tabNomPars(i) == 'minefnra') then
      tabValPars(i) = p(numplt)%P_minefnra
      CYCLE
    endif
    if (tabNomPars(i) == 'mouillabil') then
      tabValPars(i) = p(numplt)%P_mouillabil
      CYCLE
    endif
    if (tabNomPars(i) == 'mouillabilmulch(1)') then
      tabValPars(i) = pg%P_mouillabilmulch(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'mouillabilmulch(2)') then
      tabValPars(i) = pg%P_mouillabilmulch(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'mouillabilmulch(3)') then
      tabValPars(i) = pg%P_mouillabilmulch(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'mouillabilmulch(4)') then
      tabValPars(i) = pg%P_mouillabilmulch(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'mouillabilmulch(5)') then
      tabValPars(i) = pg%P_mouillabilmulch(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(1)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(2)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(3)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(4)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(5)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(6)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(7)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(8)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(9)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(10)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(11)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(12)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(13)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(14)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(15)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(16)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(17)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(18)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(19)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'mscoupemini(20)') then
      tabValPars(i) = itk(numplt)%P_mscoupemini(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(1)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(2)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(3)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(4)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(5)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(6)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(7)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(8)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(9)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(10)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(11)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(12)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(13)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(14)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(15)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(16)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(17)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(18)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(19)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'msresiduel(20)') then
      tabValPars(i) = itk(numplt)%P_msresiduel(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'mulchbat') then
      tabValPars(i) = soil%P_mulchbat
      CYCLE
    endif
    if (tabNomPars(i) == 'nbcueille') then
      tabValPars(i) = float(itk(numplt)%P_nbcueille)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbfeuilplant') then
      tabValPars(i) = float(p(numplt)%P_nbfeuilplant)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbfgellev') then
      tabValPars(i) = float(p(numplt)%P_nbfgellev)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbgrmax') then
      tabValPars(i) = p(numplt)%P_nbgrmax(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbgrmin') then
      tabValPars(i) = p(numplt)%P_nbgrmin
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinflo') then
      tabValPars(i) = p(numplt)%P_nbinflo
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(1)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(2)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(3)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(4)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(5)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(6)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(7)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(8)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(9)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbinfloecl(10)') then
      tabValPars(i) = itk(numplt)%P_nbinfloecl(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbj_pr_apres_semis') then
      tabValPars(i) = float(t%P_nbj_pr_apres_semis)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbjgerlim') then
      tabValPars(i) = float(p(numplt)%P_nbjgerlim)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbjgrain') then
      tabValPars(i) = float(p(numplt)%P_nbjgrain)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbjmaxapresrecolte') then
      tabValPars(i) = float(itk(numplt)%P_nbjmaxapresrecolte)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbjmaxapressemis') then
      tabValPars(i) = float(itk(numplt)%P_nbjmaxapressemis)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbjoursrrversirrig') then
      tabValPars(i) = float(t%P_nbjoursrrversirrig)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbjres') then
      tabValPars(i) = float(itk(numplt)%P_nbjres)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbjseuiltempref') then
      tabValPars(i) = float(itk(numplt)%P_nbjseuiltempref)
      CYCLE
    endif
    if (tabNomPars(i) == 'nbjtrav') then
      tabValPars(i) = float(itk(numplt)%P_nbjtrav)
      CYCLE
    endif
    if (tabNomPars(i) == 'nboite') then
      tabValPars(i) = float(p(numplt)%P_nboite)
      CYCLE
    endif
    if (tabNomPars(i) == 'NH3ref') then
      tabValPars(i) = sta%P_NH3ref
      CYCLE
    endif
    if (tabNomPars(i) == 'nh4_min') then
      tabValPars(i) = pg%P_nh4_min
      CYCLE
    endif
    if (tabNomPars(i) == 'NH4initf(1)') then
      tabValPars(i) = sc%P_NH4initf(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'NH4initf(2)') then
      tabValPars(i) = sc%P_NH4initf(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'NH4initf(3)') then
      tabValPars(i) = sc%P_NH4initf(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'NH4initf(4)') then
      tabValPars(i) = sc%P_NH4initf(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'NH4initf(5)') then
      tabValPars(i) = sc%P_NH4initf(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'nlevlim1') then
      tabValPars(i) = float(p(numplt)%P_nlevlim1)
      CYCLE
    endif
    if (tabNomPars(i) == 'nlevlim2') then
      tabValPars(i) = float(p(numplt)%P_nlevlim2)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nmeta') then
      tabValPars(i) = p(numplt)%P_Nmeta
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(1)') then
      tabValPars(i) = itk(numplt)%P_Nminres(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(2)') then
      tabValPars(i) = itk(numplt)%P_Nminres(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(3)') then
      tabValPars(i) = itk(numplt)%P_Nminres(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(4)') then
      tabValPars(i) = itk(numplt)%P_Nminres(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(5)') then
      tabValPars(i) = itk(numplt)%P_Nminres(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(6)') then
      tabValPars(i) = itk(numplt)%P_Nminres(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(7)') then
      tabValPars(i) = itk(numplt)%P_Nminres(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(8)') then
      tabValPars(i) = itk(numplt)%P_Nminres(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(9)') then
      tabValPars(i) = itk(numplt)%P_Nminres(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(10)') then
      tabValPars(i) = itk(numplt)%P_Nminres(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres(11)') then
      tabValPars(i) = itk(numplt)%P_Nminres(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'Nminres_pature') then
      tabValPars(i) = t%P_Nminres_pature
      CYCLE
    endif
    if (tabNomPars(i) == 'NO3initf(1)') then
      tabValPars(i) = soil%P_NO3initf(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'NO3initf(2)') then
      tabValPars(i) = soil%P_NO3initf(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'NO3initf(3)') then
      tabValPars(i) = soil%P_NO3initf(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'NO3initf(4)') then
      tabValPars(i) = soil%P_NO3initf(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'NO3initf(5)') then
      tabValPars(i) = soil%P_NO3initf(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'Norg') then
      tabValPars(i) = soil%P_Norg
      CYCLE
    endif
    if (tabNomPars(i) == 'Nreserve') then
      tabValPars(i) = p(numplt)%P_Nreserve
      CYCLE
    endif
    if (tabNomPars(i) == 'numsol') then
      tabValPars(i) = float(soil%P_numsol)
      CYCLE
    endif
    if (tabNomPars(i) == 'obstarac') then
      tabValPars(i) = soil%P_obstarac
      CYCLE
    endif
    if (tabNomPars(i) == 'ombragetx') then
      tabValPars(i) = sta%P_ombragetx
      CYCLE
    endif
    if (tabNomPars(i) == 'option_engrais_multiple') then
      tabValPars(i) = float(t%P_option_engrais_multiple)
      CYCLE
    endif
    if (tabNomPars(i) == 'option_pature') then
      tabValPars(i) = float(t%P_option_pature)
      CYCLE
    endif
    if (tabNomPars(i) == 'option_thinning') then
      tabValPars(i) = float(t%P_option_thinning)
      CYCLE
    endif
    if (tabNomPars(i) == 'orgeng(1)') then
      tabValPars(i) = pg%P_orgeng(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'orgeng(2)') then
      tabValPars(i) = pg%P_orgeng(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'orgeng(3)') then
      tabValPars(i) = pg%P_orgeng(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'orgeng(4)') then
      tabValPars(i) = pg%P_orgeng(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'orgeng(5)') then
      tabValPars(i) = pg%P_orgeng(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'orgeng(6)') then
      tabValPars(i) = pg%P_orgeng(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'orgeng(7)') then
      tabValPars(i) = pg%P_orgeng(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'orgeng(8)') then
      tabValPars(i) = pg%P_orgeng(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'orientrang') then
      tabValPars(i) = itk(numplt)%P_orientrang
      CYCLE
    endif
    if (tabNomPars(i) == 'parazofmorte') then
      tabValPars(i) = p(numplt)%P_parazofmorte
      CYCLE
    endif
    if (tabNomPars(i) == 'parsurrg') then
      tabValPars(i) = pg%P_parsurrg
      CYCLE
    endif
    if (tabNomPars(i) == 'par_to_net') then
      tabValPars(i) = t%P_par_to_net
      CYCLE
    endif
    if (tabNomPars(i) == 'patm') then
      tabValPars(i) = sta%P_patm
      CYCLE
    endif
    if (tabNomPars(i) == 'penterui') then
      tabValPars(i) = soil%P_penterui
      CYCLE
    endif
    if (tabNomPars(i) == 'pentinflores') then
      tabValPars(i) = p(numplt)%P_pentinflores
      CYCLE
    endif
    if (tabNomPars(i) == 'pentlaimax') then
      tabValPars(i) = p(numplt)%P_pentlaimax
      CYCLE
    endif
    if (tabNomPars(i) == 'pentrecouv') then
      tabValPars(i) = p(numplt)%P_pentrecouv
      CYCLE
    endif
    if (tabNomPars(i) == 'pertes_restit_ext') then
      tabValPars(i) = t%P_pertes_restit_ext
      CYCLE
    endif
    if (tabNomPars(i) == 'pgrainmaxi') then
      tabValPars(i) = p(numplt)%P_pgrainmaxi(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'pH') then
      tabValPars(i) = soil%P_pH
      CYCLE
    endif
    if (tabNomPars(i) == 'phiv0') then
      tabValPars(i) = sta%P_phiv0
      CYCLE
    endif
    if (tabNomPars(i) == 'pHmaxden') then
      tabValPars(i) = pg%P_pHmaxden
      CYCLE
    endif
    if (tabNomPars(i) == 'pHmaxnit') then
      tabValPars(i) = pg%P_pHmaxnit
      CYCLE
    endif
    if (tabNomPars(i) == 'pHmaxvol') then
      tabValPars(i) = pg%P_pHmaxvol
      CYCLE
    endif
    if (tabNomPars(i) == 'pHminden') then
      tabValPars(i) = pg%P_pHminden
      CYCLE
    endif
    if (tabNomPars(i) == 'pHminnit') then
      tabValPars(i) = pg%P_pHminnit
      CYCLE
    endif
    if (tabNomPars(i) == 'pHminvol') then
      tabValPars(i) = pg%P_pHminvol
      CYCLE
    endif
    if (tabNomPars(i) == 'phobase') then
      tabValPars(i) = p(numplt)%P_phobase
      CYCLE
    endif
    if (tabNomPars(i) == 'phobasesen') then
      tabValPars(i) = p(numplt)%P_phobasesen
      CYCLE
    endif
    if (tabNomPars(i) == 'phosat') then
      tabValPars(i) = p(numplt)%P_phosat
      CYCLE
    endif
    if (tabNomPars(i) == 'pHvols') then
      tabValPars(i) = pg%P_pHvols
      CYCLE
    endif
    if (tabNomPars(i) == 'phyllotherme') then
      tabValPars(i) = p(numplt)%P_phyllotherme
      CYCLE
    endif
    if (tabNomPars(i) == 'plNmin') then
      tabValPars(i) = pg%P_plNmin
      CYCLE
    endif
    if (tabNomPars(i) == 'pluiebat') then
      tabValPars(i) = soil%P_pluiebat
      CYCLE
    endif
    if (tabNomPars(i) == 'pminruis') then
      tabValPars(i) = pg%P_pminruis
      CYCLE
    endif
    if (tabNomPars(i) == 'potgermi') then
      tabValPars(i) = p(numplt)%P_potgermi
      CYCLE
    endif
    if (tabNomPars(i) == 'primingmax') then
      tabValPars(i) = pg%P_primingmax
      CYCLE
    endif
    if (tabNomPars(i) == 'profdenit') then
      tabValPars(i) = soil%P_profdenit
      CYCLE
    endif
    if (tabNomPars(i) == 'profdrain') then
      tabValPars(i) = soil%P_profdrain
      CYCLE
    endif
    if (tabNomPars(i) == 'profhum') then
      tabValPars(i) = soil%P_profhum
      CYCLE
    endif
    if (tabNomPars(i) == 'profhumrecolteuse') then
      tabValPars(i) = itk(numplt)%P_profhumrecolteuse
      CYCLE
    endif
    if (tabNomPars(i) == 'profhumsemoir') then
      tabValPars(i) = itk(numplt)%P_profhumsemoir
      CYCLE
    endif
    if (tabNomPars(i) == 'profimper') then
      tabValPars(i) = soil%P_profimper
      CYCLE
    endif
    if (tabNomPars(i) == 'proflabour') then
      tabValPars(i) = pg%P_proflabour
      CYCLE
    endif
    if (tabNomPars(i) == 'profmes') then
      tabValPars(i) = itk(numplt)%P_profmes
      CYCLE
    endif
    if (tabNomPars(i) == 'profnod') then
      tabValPars(i) = p(numplt)%P_profnod
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(1)') then
      tabValPars(i) = itk(numplt)%P_profres(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(2)') then
      tabValPars(i) = itk(numplt)%P_profres(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(3)') then
      tabValPars(i) = itk(numplt)%P_profres(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(4)') then
      tabValPars(i) = itk(numplt)%P_profres(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(5)') then
      tabValPars(i) = itk(numplt)%P_profres(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(6)') then
      tabValPars(i) = itk(numplt)%P_profres(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(7)') then
      tabValPars(i) = itk(numplt)%P_profres(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(8)') then
      tabValPars(i) = itk(numplt)%P_profres(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(9)') then
      tabValPars(i) = itk(numplt)%P_profres(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(10)') then
      tabValPars(i) = itk(numplt)%P_profres(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'profres(11)') then
      tabValPars(i) = itk(numplt)%P_profres(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'profsem') then
      tabValPars(i) = itk(numplt)%P_profsem
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(1)') then
      tabValPars(i) = itk(numplt)%P_proftrav(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(2)') then
      tabValPars(i) = itk(numplt)%P_proftrav(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(3)') then
      tabValPars(i) = itk(numplt)%P_proftrav(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(4)') then
      tabValPars(i) = itk(numplt)%P_proftrav(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(5)') then
      tabValPars(i) = itk(numplt)%P_proftrav(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(6)') then
      tabValPars(i) = itk(numplt)%P_proftrav(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(7)') then
      tabValPars(i) = itk(numplt)%P_proftrav(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(8)') then
      tabValPars(i) = itk(numplt)%P_proftrav(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(9)') then
      tabValPars(i) = itk(numplt)%P_proftrav(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(10)') then
      tabValPars(i) = itk(numplt)%P_proftrav(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftrav(11)') then
      tabValPars(i) = itk(numplt)%P_proftrav(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'proftravmin') then
      tabValPars(i) = pg%P_proftravmin
      CYCLE
    endif
    if (tabNomPars(i) == 'prophumtassrec') then
      tabValPars(i) = pg%P_prophumtassrec
      CYCLE
    endif
    if (tabNomPars(i) == 'prophumtasssem') then
      tabValPars(i) = pg%P_prophumtasssem
      CYCLE
    endif
    if (tabNomPars(i) == 'propjgermin') then
      tabValPars(i) = p(numplt)%P_propjgermin
      CYCLE
    endif
    if (tabNomPars(i) == 'proprac') then
      tabValPars(i) = pg%P_proprac
      CYCLE
    endif
    if (tabNomPars(i) == 'psihucc') then
      tabValPars(i) = pg%P_psihucc
      CYCLE
    endif
    if (tabNomPars(i) == 'psihumin') then
      tabValPars(i) = pg%P_psihumin
      CYCLE
    endif
    if (tabNomPars(i) == 'psisto') then
      tabValPars(i) = p(numplt)%P_psisto
      CYCLE
    endif
    if (tabNomPars(i) == 'psiturg') then
      tabValPars(i) = p(numplt)%P_psiturg
      CYCLE
    endif
    if (tabNomPars(i) == 'q0') then
      tabValPars(i) = soil%P_q0
      CYCLE
    endif
    if (tabNomPars(i) == 'q10') then
      tabValPars(i) = p(numplt)%P_q10
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(1)') then
      tabValPars(i) = pg%P_qmulchdec(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(2)') then
      tabValPars(i) = pg%P_qmulchdec(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(3)') then
      tabValPars(i) = pg%P_qmulchdec(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(4)') then
      tabValPars(i) = pg%P_qmulchdec(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(5)') then
      tabValPars(i) = pg%P_qmulchdec(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(6)') then
      tabValPars(i) = pg%P_qmulchdec(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(7)') then
      tabValPars(i) = pg%P_qmulchdec(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(8)') then
      tabValPars(i) = pg%P_qmulchdec(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(9)') then
      tabValPars(i) = pg%P_qmulchdec(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(10)') then
      tabValPars(i) = pg%P_qmulchdec(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(11)') then
      tabValPars(i) = pg%P_qmulchdec(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(12)') then
      tabValPars(i) = pg%P_qmulchdec(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(13)') then
      tabValPars(i) = pg%P_qmulchdec(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(14)') then
      tabValPars(i) = pg%P_qmulchdec(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(15)') then
      tabValPars(i) = pg%P_qmulchdec(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(16)') then
      tabValPars(i) = pg%P_qmulchdec(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(17)') then
      tabValPars(i) = pg%P_qmulchdec(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(18)') then
      tabValPars(i) = pg%P_qmulchdec(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(19)') then
      tabValPars(i) = pg%P_qmulchdec(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(20)') then
      tabValPars(i) = pg%P_qmulchdec(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchdec(21)') then
      tabValPars(i) = pg%P_qmulchdec(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(1)') then
      tabValPars(i) = pg%P_qmulchruis0(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(2)') then
      tabValPars(i) = pg%P_qmulchruis0(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(3)') then
      tabValPars(i) = pg%P_qmulchruis0(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(4)') then
      tabValPars(i) = pg%P_qmulchruis0(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(5)') then
      tabValPars(i) = pg%P_qmulchruis0(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(6)') then
      tabValPars(i) = pg%P_qmulchruis0(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(7)') then
      tabValPars(i) = pg%P_qmulchruis0(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(8)') then
      tabValPars(i) = pg%P_qmulchruis0(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(9)') then
      tabValPars(i) = pg%P_qmulchruis0(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(10)') then
      tabValPars(i) = pg%P_qmulchruis0(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(11)') then
      tabValPars(i) = pg%P_qmulchruis0(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(12)') then
      tabValPars(i) = pg%P_qmulchruis0(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(13)') then
      tabValPars(i) = pg%P_qmulchruis0(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(14)') then
      tabValPars(i) = pg%P_qmulchruis0(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(15)') then
      tabValPars(i) = pg%P_qmulchruis0(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(16)') then
      tabValPars(i) = pg%P_qmulchruis0(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(17)') then
      tabValPars(i) = pg%P_qmulchruis0(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(18)') then
      tabValPars(i) = pg%P_qmulchruis0(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(19)') then
      tabValPars(i) = pg%P_qmulchruis0(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(20)') then
      tabValPars(i) = pg%P_qmulchruis0(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'qmulchruis0(21)') then
      tabValPars(i) = pg%P_qmulchruis0(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'QNplante0') then
      tabValPars(i) = p(numplt)%P_QNplante0
      CYCLE
    endif
    if (tabNomPars(i) == 'QNpltminINN') then
      tabValPars(i) = pg%P_QNpltminINN
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(1)') then
      tabValPars(i) = itk(numplt)%P_qres(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(2)') then
      tabValPars(i) = itk(numplt)%P_qres(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(3)') then
      tabValPars(i) = itk(numplt)%P_qres(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(4)') then
      tabValPars(i) = itk(numplt)%P_qres(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(5)') then
      tabValPars(i) = itk(numplt)%P_qres(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(6)') then
      tabValPars(i) = itk(numplt)%P_qres(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(7)') then
      tabValPars(i) = itk(numplt)%P_qres(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(8)') then
      tabValPars(i) = itk(numplt)%P_qres(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(9)') then
      tabValPars(i) = itk(numplt)%P_qres(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(10)') then
      tabValPars(i) = itk(numplt)%P_qres(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'qres(11)') then
      tabValPars(i) = itk(numplt)%P_qres(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'Qtot_N') then
      tabValPars(i) = float(itk(numplt)%P_Qtot_N)
      CYCLE
    endif
    if (tabNomPars(i) == 'ra') then
      tabValPars(i) = sta%P_ra
      CYCLE
    endif
    if (tabNomPars(i) == 'rapforme') then
      tabValPars(i) = p(numplt)%P_rapforme
      CYCLE
    endif
    if (tabNomPars(i) == 'rapNmindec') then
      tabValPars(i) = t%P_rapNmindec
      CYCLE
    endif
    if (tabNomPars(i) == 'rapsenturg') then
      tabValPars(i) = p(numplt)%P_rapsenturg
      CYCLE
    endif
    if (tabNomPars(i) == 'ratiodenit') then
      tabValPars(i) = pg%P_ratiodenit
      CYCLE
    endif
    if (tabNomPars(i) == 'ratiodurvieI') then
      tabValPars(i) = p(numplt)%P_ratiodurvieI
      CYCLE
    endif
    if (tabNomPars(i) == 'ratiol') then
      tabValPars(i) = itk(numplt)%P_ratiol
      CYCLE
    endif
    if (tabNomPars(i) == 'ratiolN') then
      tabValPars(i) = t%P_ratiolN
      CYCLE
    endif
    if (tabNomPars(i) == 'rationit') then
      tabValPars(i) = pg%P_rationit
      CYCLE
    endif
    if (tabNomPars(i) == 'ratiosen') then
      tabValPars(i) = p(numplt)%P_ratiosen
      CYCLE
    endif
    if (tabNomPars(i) == 'rayon') then
      tabValPars(i) = pg%P_rayon
      CYCLE
    endif
    if (tabNomPars(i) == 'rdrain') then
      tabValPars(i) = pg%P_rdrain
      CYCLE
    endif
    if (tabNomPars(i) == 'remobres') then
      tabValPars(i) = p(numplt)%P_remobres
      CYCLE
    endif
    if (tabNomPars(i) == 'repracpermax') then
      tabValPars(i) = p(numplt)%P_repracpermax
      CYCLE
    endif
    if (tabNomPars(i) == 'repracpermin') then
      tabValPars(i) = p(numplt)%P_repracpermin
      CYCLE
    endif
    if (tabNomPars(i) == 'repracseumax') then
      tabValPars(i) = p(numplt)%P_repracseumax
      CYCLE
    endif
    if (tabNomPars(i) == 'repracseumin') then
      tabValPars(i) = p(numplt)%P_repracseumin
      CYCLE
    endif
    if (tabNomPars(i) == 'resperenne0') then
      tabValPars(i) = p(numplt)%P_resperenne0
      CYCLE
    endif
    if (tabNomPars(i) == 'resplmax(1)') then
      tabValPars(i) = t%P_resplmax(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'resplmax(2)') then
      tabValPars(i) = t%P_resplmax(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'rsmin') then
      tabValPars(i) = p(numplt)%P_rsmin
      CYCLE
    endif
    if (tabNomPars(i) == 'rugochisel') then
      tabValPars(i) = itk(numplt)%P_rugochisel
      CYCLE
    endif
    if (tabNomPars(i) == 'rugolabour') then
      tabValPars(i) = itk(numplt)%P_rugolabour
      CYCLE
    endif
    if (tabNomPars(i) == 'ruisolnu') then
      tabValPars(i) = soil%P_ruisolnu
      CYCLE
    endif
    if (tabNomPars(i) == 'scale_tdenitopt') then
      tabValPars(i) = pg%P_scale_tdenitopt
      CYCLE
    endif
    if (tabNomPars(i) == 'scale_tnitopt') then
      tabValPars(i) = pg%P_scale_tnitopt
      CYCLE
    endif
    if (tabNomPars(i) == 'sea') then
      tabValPars(i) = p(numplt)%P_sea
      CYCLE
    endif
    if (tabNomPars(i) == 'sensanox') then
      tabValPars(i) = p(numplt)%P_sensanox
      CYCLE
    endif
    if (tabNomPars(i) == 'sensiphot') then
      tabValPars(i) = p(numplt)%P_sensiphot(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'sensrsec') then
      tabValPars(i) = p(numplt)%P_sensrsec
      CYCLE
    endif
    if (tabNomPars(i) == 'seuilLAIapex(1)') then
      tabValPars(i) = t%P_seuilLAIapex(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'seuilLAIapex(2)') then
      tabValPars(i) = t%P_seuilLAIapex(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'seuilmortalle(1)') then
      tabValPars(i) = t%P_seuilmortalle(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'seuilmortalle(2)') then
      tabValPars(i) = t%P_seuilmortalle(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'seuilreconspeupl(1)') then
      tabValPars(i) = t%P_seuilreconspeupl(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'seuilreconspeupl(2)') then
      tabValPars(i) = t%P_seuilreconspeupl(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'sigmadistalle(1)') then
      tabValPars(i) = t%P_sigmadistalle(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'sigmadistalle(2)') then
      tabValPars(i) = t%P_sigmadistalle(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'slamax') then
      tabValPars(i) = p(numplt)%P_slamax
      CYCLE
    endif
    if (tabNomPars(i) == 'slamin') then
      tabValPars(i) = p(numplt)%P_slamin
      CYCLE
    endif
    if (tabNomPars(i) == 'spfrmax') then
      tabValPars(i) = p(numplt)%P_spfrmax
      CYCLE
    endif
    if (tabNomPars(i) == 'spfrmin') then
      tabValPars(i) = p(numplt)%P_spfrmin
      CYCLE
    endif
    if (tabNomPars(i) == 'splaimax') then
      tabValPars(i) = p(numplt)%P_splaimax
      CYCLE
    endif
    if (tabNomPars(i) == 'splaimin') then
      tabValPars(i) = p(numplt)%P_splaimin
      CYCLE
    endif
    if (tabNomPars(i) == 'stamflax') then
      tabValPars(i) = p(numplt)%P_stamflax(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'stdnofno') then
      tabValPars(i) = p(numplt)%P_stdnofno
      CYCLE
    endif
    if (tabNomPars(i) == 'stdordebour') then
      tabValPars(i) = p(numplt)%P_stdordebour
      CYCLE
    endif
    if (tabNomPars(i) == 'stdrpdes') then
      tabValPars(i) = p(numplt)%P_stdrpdes(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'stdrpmat') then
      tabValPars(i) = p(numplt)%P_stdrpmat(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'stdrpnou') then
      tabValPars(i) = p(numplt)%P_stdrpnou
      CYCLE
    endif
    if (tabNomPars(i) == 'stemflowmax') then
      tabValPars(i) = p(numplt)%P_stemflowmax
      CYCLE
    endif
    if (tabNomPars(i) == 'stflodrp') then
      tabValPars(i) = p(numplt)%P_stflodrp(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'stfnofvino') then
      tabValPars(i) = p(numplt)%P_stfnofvino
      CYCLE
    endif
    if (tabNomPars(i) == 'stlaxsen') then
      tabValPars(i) = p(numplt)%P_stlaxsen(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'stlevamf') then
      tabValPars(i) = p(numplt)%P_stlevamf(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'stlevdno') then
      tabValPars(i) = p(numplt)%P_stlevdno
      CYCLE
    endif
    if (tabNomPars(i) == 'stlevdrp') then
      tabValPars(i) = p(numplt)%P_stlevdrp(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'stpltger') then
      tabValPars(i) = p(numplt)%P_stpltger
      CYCLE
    endif
    if (tabNomPars(i) == 'stressdev') then
      tabValPars(i) = p(numplt)%P_stressdev
      CYCLE
    endif
    if (tabNomPars(i) == 'stsenlan') then
      tabValPars(i) = p(numplt)%P_stsenlan(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'sucrerec') then
      tabValPars(i) = itk(numplt)%P_sucrerec
      CYCLE
    endif
    if (tabNomPars(i) == 'surfapex(1)') then
      tabValPars(i) = t%P_surfapex(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'surfapex(2)') then
      tabValPars(i) = t%P_surfapex(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'surfouvre1') then
      tabValPars(i) = itk(numplt)%P_surfouvre1
      CYCLE
    endif
    if (tabNomPars(i) == 'surfouvre2') then
      tabValPars(i) = itk(numplt)%P_surfouvre2
      CYCLE
    endif
    if (tabNomPars(i) == 'surfouvre3') then
      tabValPars(i) = itk(numplt)%P_surfouvre3
      CYCLE
    endif
    if (tabNomPars(i) == 'swfacmin') then
      tabValPars(i) = t%P_swfacmin
      CYCLE
    endif
    if (tabNomPars(i) == 'tauxrecouvkmax') then
      tabValPars(i) = p(numplt)%P_tauxrecouvkmax
      CYCLE
    endif
    if (tabNomPars(i) == 'tauxrecouvmax') then
      tabValPars(i) = p(numplt)%P_tauxrecouvmax
      CYCLE
    endif
    if (tabNomPars(i) == 'tcmax') then
      tabValPars(i) = p(numplt)%P_tcmax
      CYCLE
    endif
    if (tabNomPars(i) == 'tcmin') then
      tabValPars(i) = p(numplt)%P_tcmin
      CYCLE
    endif
    if (tabNomPars(i) == 'tcxstop') then
      tabValPars(i) = p(numplt)%P_tcxstop
      CYCLE
    endif
    if (tabNomPars(i) == 'tdebgel') then
      tabValPars(i) = p(numplt)%P_tdebgel
      CYCLE
    endif
    if (tabNomPars(i) == 'tdenitopt_gauss') then
      tabValPars(i) = pg%P_tdenitopt_gauss
      CYCLE
    endif
    if (tabNomPars(i) == 'tdmax') then
      tabValPars(i) = p(numplt)%P_tdmax
      CYCLE
    endif
    if (tabNomPars(i) == 'tdmaxdeb') then
      tabValPars(i) = p(numplt)%P_tdmaxdeb
      CYCLE
    endif
    if (tabNomPars(i) == 'tdmin') then
      tabValPars(i) = p(numplt)%P_tdmin
      CYCLE
    endif
    if (tabNomPars(i) == 'tdmindeb') then
      tabValPars(i) = p(numplt)%P_tdmindeb
      CYCLE
    endif
    if (tabNomPars(i) == 'temax') then
      tabValPars(i) = p(numplt)%P_temax
      CYCLE
    endif
    if (tabNomPars(i) == 'temin') then
      tabValPars(i) = p(numplt)%P_temin
      CYCLE
    endif
    if (tabNomPars(i) == 'tempdeshyd') then
      tabValPars(i) = p(numplt)%P_tempdeshyd
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(1)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(2)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(3)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(4)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(5)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(6)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(7)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(8)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(9)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(10)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(11)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(12)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(13)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(14)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(15)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(16)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(17)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(18)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(19)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempfauche(20)') then
      tabValPars(i) = itk(numplt)%P_tempfauche(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'tempnod1') then
      tabValPars(i) = p(numplt)%P_tempnod1
      CYCLE
    endif
    if (tabNomPars(i) == 'tempnod2') then
      tabValPars(i) = p(numplt)%P_tempnod2
      CYCLE
    endif
    if (tabNomPars(i) == 'tempnod3') then
      tabValPars(i) = p(numplt)%P_tempnod3
      CYCLE
    endif
    if (tabNomPars(i) == 'tempnod4') then
      tabValPars(i) = p(numplt)%P_tempnod4
      CYCLE
    endif
    if (tabNomPars(i) == 'teopt') then
      tabValPars(i) = p(numplt)%P_teopt
      CYCLE
    endif
    if (tabNomPars(i) == 'teoptbis') then
      tabValPars(i) = p(numplt)%P_teoptbis
      CYCLE
    endif
    if (tabNomPars(i) == 'tfroid') then
      tabValPars(i) = p(numplt)%P_tfroid
      CYCLE
    endif
    if (tabNomPars(i) == 'tgelflo10') then
      tabValPars(i) = p(numplt)%P_tgelflo10
      CYCLE
    endif
    if (tabNomPars(i) == 'tgelflo90') then
      tabValPars(i) = p(numplt)%P_tgelflo90
      CYCLE
    endif
    if (tabNomPars(i) == 'tgeljuv10') then
      tabValPars(i) = p(numplt)%P_tgeljuv10
      CYCLE
    endif
    if (tabNomPars(i) == 'tgeljuv90') then
      tabValPars(i) = p(numplt)%P_tgeljuv90
      CYCLE
    endif
    if (tabNomPars(i) == 'tgellev10') then
      tabValPars(i) = p(numplt)%P_tgellev10
      CYCLE
    endif
    if (tabNomPars(i) == 'tgellev90') then
      tabValPars(i) = p(numplt)%P_tgellev90
      CYCLE
    endif
    if (tabNomPars(i) == 'tgelveg10') then
      tabValPars(i) = p(numplt)%P_tgelveg10
      CYCLE
    endif
    if (tabNomPars(i) == 'tgelveg90') then
      tabValPars(i) = p(numplt)%P_tgelveg90
      CYCLE
    endif
    if (tabNomPars(i) == 'tgmin') then
      tabValPars(i) = p(numplt)%P_tgmin
      CYCLE
    endif
    if (tabNomPars(i) == 'tigefeuil') then
      tabValPars(i) = p(numplt)%P_tigefeuil
      CYCLE
    endif
    if (tabNomPars(i) == 'tigefeuilcoupe(1)') then
      tabValPars(i) = t%P_tigefeuilcoupe(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'tigefeuilcoupe(2)') then
      tabValPars(i) = t%P_tigefeuilcoupe(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'tletale') then
      tabValPars(i) = p(numplt)%P_tletale
      CYCLE
    endif
    if (tabNomPars(i) == 'tmaxremp') then
      tabValPars(i) = p(numplt)%P_tmaxremp
      CYCLE
    endif
    if (tabNomPars(i) == 'tminremp') then
      tabValPars(i) = p(numplt)%P_tminremp
      CYCLE
    endif
    if (tabNomPars(i) == 'tnitmax') then
      tabValPars(i) = pg%P_tnitmax
      CYCLE
    endif
    if (tabNomPars(i) == 'tnitmin') then
      tabValPars(i) = pg%P_tnitmin
      CYCLE
    endif
    if (tabNomPars(i) == 'tnitopt') then
      tabValPars(i) = pg%P_tnitopt
      CYCLE
    endif
    if (tabNomPars(i) == 'tnitopt_gauss') then
      tabValPars(i) = pg%P_tnitopt_gauss
      CYCLE
    endif
    if (tabNomPars(i) == 'tnitopt2') then
      tabValPars(i) = pg%P_tnitopt2
      CYCLE
    endif
    if (tabNomPars(i) == 'transplastic') then
      tabValPars(i) = itk(numplt)%P_transplastic
      CYCLE
    endif
    if (tabNomPars(i) == 'trefh') then
      tabValPars(i) = pg%P_trefh
      CYCLE
    endif
    if (tabNomPars(i) == 'trefr') then
      tabValPars(i) = pg%P_trefr
      CYCLE
    endif
    if (tabNomPars(i) == 'tustressmin') then
      tabValPars(i) = p(numplt)%P_tustressmin
      CYCLE
    endif
    if (tabNomPars(i) == 'typecailloux(1)') then
      tabValPars(i) = float(soil%P_typecailloux(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'typecailloux(2)') then
      tabValPars(i) = float(soil%P_typecailloux(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'typecailloux(3)') then
      tabValPars(i) = float(soil%P_typecailloux(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'typecailloux(4)') then
      tabValPars(i) = float(soil%P_typecailloux(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'typecailloux(5)') then
      tabValPars(i) = float(soil%P_typecailloux(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'udlaimax') then
      tabValPars(i) = p(numplt)%P_udlaimax
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(1)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(2)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(3)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(4)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(5)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(6)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(7)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(8)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(9)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(10)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(11)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(11))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(12)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(12))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(13)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(13))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(14)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(14))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(15)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(15))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(16)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(16))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(17)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(17))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(18)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(18))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(19)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(19))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(20)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(20))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(21)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(21))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(22)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(22))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(23)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(23))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(24)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(24))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(25)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(25))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(26)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(26))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(27)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(27))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(28)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(28))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(29)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(29))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapI(30)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapI(30))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(1)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(1))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(2)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(2))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(3)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(3))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(4)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(4))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(5)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(5))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(6)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(6))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(7)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(7))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(8)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(8))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(9)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(9))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(10)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(10))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(11)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(11))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(12)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(12))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(13)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(13))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(14)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(14))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(15)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(15))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(16)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(16))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(17)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(17))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(18)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(18))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(19)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(19))
      CYCLE
    endif
    if (tabNomPars(i) == 'upvttapN(20)') then
      tabValPars(i) = float(itk(numplt)%P_upvttapN(20))
      CYCLE
    endif
    if (tabNomPars(i) == 'Vabs2') then
      tabValPars(i) = pg%P_Vabs2
      CYCLE
    endif
    if (tabNomPars(i) == 'variete') then
      tabValPars(i) = float(itk(numplt)%P_variete)
      CYCLE
    endif
    if (tabNomPars(i) == 'vigueurbat') then
      tabValPars(i) = p(numplt)%P_vigueurbat
      CYCLE
    endif
    if (tabNomPars(i) == 'vitirazo') then
      tabValPars(i) = p(numplt)%P_vitirazo
      CYCLE
    endif
    if (tabNomPars(i) == 'vitircarb') then
      tabValPars(i) = p(numplt)%P_vitircarb
      CYCLE
    endif
    if (tabNomPars(i) == 'vitircarbT') then
      tabValPars(i) = p(numplt)%P_vitircarbT
      CYCLE
    endif
    if (tabNomPars(i) == 'vitno') then
      tabValPars(i) = p(numplt)%P_vitno
      CYCLE
    endif
    if (tabNomPars(i) == 'vitprophuile') then
      tabValPars(i) = p(numplt)%P_vitprophuile
      CYCLE
    endif
    if (tabNomPars(i) == 'vitpropsucre') then
      tabValPars(i) = p(numplt)%P_vitpropsucre
      CYCLE
    endif
    if (tabNomPars(i) == 'vitreconspeupl(1)') then
      tabValPars(i) = t%P_vitreconspeupl(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'vitreconspeupl(2)') then
      tabValPars(i) = t%P_vitreconspeupl(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'vlaimax') then
      tabValPars(i) = p(numplt)%P_vlaimax
      CYCLE
    endif
    if (tabNomPars(i) == 'Vmax1') then
      tabValPars(i) = p(numplt)%P_Vmax1
      CYCLE
    endif
    if (tabNomPars(i) == 'Vmax2') then
      tabValPars(i) = p(numplt)%P_Vmax2
      CYCLE
    endif
    if (tabNomPars(i) == 'vnitmax') then
      tabValPars(i) = pg%P_vnitmax
      CYCLE
    endif
    if (tabNomPars(i) == 'voleng(1)') then
      tabValPars(i) = pg%P_voleng(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'voleng(2)') then
      tabValPars(i) = pg%P_voleng(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'voleng(3)') then
      tabValPars(i) = pg%P_voleng(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'voleng(4)') then
      tabValPars(i) = pg%P_voleng(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'voleng(5)') then
      tabValPars(i) = pg%P_voleng(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'voleng(6)') then
      tabValPars(i) = pg%P_voleng(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'voleng(7)') then
      tabValPars(i) = pg%P_voleng(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'voleng(8)') then
      tabValPars(i) = pg%P_voleng(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'vpotdenit') then
      tabValPars(i) = soil%P_vpotdenit
      CYCLE
    endif
    if (tabNomPars(i) == 'wfpsc') then
      tabValPars(i) = pg%P_wfpsc
      CYCLE
    endif
    if (tabNomPars(i) == 'Wh') then
      tabValPars(i) = pg%P_Wh
      CYCLE
    endif
    if (tabNomPars(i) == 'Xorgmax') then
      tabValPars(i) = pg%P_Xorgmax
      CYCLE
    endif
    if (tabNomPars(i) == 'y0msrac') then
      tabValPars(i) = pg%P_y0msrac
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(1)') then
      tabValPars(i) = pg%P_yres(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(2)') then
      tabValPars(i) = pg%P_yres(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(3)') then
      tabValPars(i) = pg%P_yres(3)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(4)') then
      tabValPars(i) = pg%P_yres(4)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(5)') then
      tabValPars(i) = pg%P_yres(5)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(6)') then
      tabValPars(i) = pg%P_yres(6)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(7)') then
      tabValPars(i) = pg%P_yres(7)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(8)') then
      tabValPars(i) = pg%P_yres(8)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(9)') then
      tabValPars(i) = pg%P_yres(9)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(10)') then
      tabValPars(i) = pg%P_yres(10)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(11)') then
      tabValPars(i) = pg%P_yres(11)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(12)') then
      tabValPars(i) = pg%P_yres(12)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(13)') then
      tabValPars(i) = pg%P_yres(13)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(14)') then
      tabValPars(i) = pg%P_yres(14)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(15)') then
      tabValPars(i) = pg%P_yres(15)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(16)') then
      tabValPars(i) = pg%P_yres(16)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(17)') then
      tabValPars(i) = pg%P_yres(17)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(18)') then
      tabValPars(i) = pg%P_yres(18)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(19)') then
      tabValPars(i) = pg%P_yres(19)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(20)') then
      tabValPars(i) = pg%P_yres(20)
      CYCLE
    endif
    if (tabNomPars(i) == 'yres(21)') then
      tabValPars(i) = pg%P_yres(21)
      CYCLE
    endif
    if (tabNomPars(i) == 'z0solnu') then
      tabValPars(i) = soil%P_z0solnu
      CYCLE
    endif
    if (tabNomPars(i) == 'zesx') then
      tabValPars(i) = soil%P_zesx
      CYCLE
    endif
    if (tabNomPars(i) == 'zlabour') then
      tabValPars(i) = p(numplt)%P_zlabour
      CYCLE
    endif
    if (tabNomPars(i) == 'zpente') then
      tabValPars(i) = p(numplt)%P_zpente
      CYCLE
    endif
    if (tabNomPars(i) == 'zprlim') then
      tabValPars(i) = p(numplt)%P_zprlim
      CYCLE
    endif
    if (tabNomPars(i) == 'zr') then
      tabValPars(i) = sta%P_zr
      CYCLE
    endif
    if (tabNomPars(i) == 'zrac0') then
      tabValPars(i) = p(numplt)%P_zrac0
      CYCLE
    endif
    if (tabNomPars(i) == 'zracplantule') then
      tabValPars(i) = p(numplt)%P_zracplantule
      CYCLE
    endif
    if (tabNomPars(i) == 'hautK1') then
      tabValPars(i) = t%P_hautK(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautK2') then
      tabValPars(i) = t%P_hautK(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautA1') then
      tabValPars(i) = t%P_hautA(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'hautA2') then
      tabValPars(i) = t%P_hautA(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'hauteur_threshold') then
      tabValPars(i) = t%P_hauteur_threshold
      CYCLE
    endif
    if (tabNomPars(i) == 'elongation1') then
      tabValPars(i) = t%P_elongation(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'elongation2') then
      tabValPars(i) = t%P_elongation(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'nw_height1') then
      tabValPars(i) = t%P_nw_height(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'nw_height2') then
      tabValPars(i) = t%P_nw_height(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'haut_dev_B1') then
      tabValPars(i) = t%P_haut_dev_x0(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'haut_dev_B2') then
      tabValPars(i) = t%P_haut_dev_x0(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'haut_dev_C1') then
      tabValPars(i) = t%P_haut_dev_k(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'haut_dev_C2') then
      tabValPars(i) = t%P_haut_dev_k(2)
      CYCLE
    endif
    if (tabNomPars(i) == 'code_strip') then
      tabValPars(i) = t%P_code_strip
      CYCLE
    endif
    if (tabNomPars(i) == 'nrow_p') then
      tabValPars(i) = t%P_nrow(1)
      CYCLE
    endif
    if (tabNomPars(i) == 'nrow_a') then
      tabValPars(i) = t%P_nrow(2)
      CYCLE
    endif
      print *, nom, ': unknown parameter name (check case sensitivity)'
    end do B1
return
end subroutine CorrespondanceParametresEntree
