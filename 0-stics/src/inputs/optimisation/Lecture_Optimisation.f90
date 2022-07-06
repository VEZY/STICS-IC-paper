! **************************************************************
!    reading of parameters to optimize
!    in the file param.sti
! **************************************************************
!
!  MODIFICATIONS (last commit)
!    $Date: 2017-02-21 10:57:37 +0100 (mar., 21 févr. 2017) $
!    $Author: plecharpent $
!    $Revision: 1304 $
!
!***************************************************************



subroutine Lecture_Optimisation(sc,pg,p,itk,soil,sta,t,nbpar,nompar,valparopt)

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

  type(Stics_Communs_),       intent(INOUT) :: sc

  type(Parametres_Generaux_), intent(INOUT) :: pg

  type(Plante_),              intent(INOUT) :: p (sc%P_nbplantes)

  type(ITK_),                 intent(INOUT) :: itk (sc%P_nbplantes)

  type(Sol_),                 intent(INOUT) :: soil

  type(Station_),             intent(INOUT) :: sta

  type(Stics_Transit_),       intent(INOUT) :: t(sc%P_nbplantes)


    integer,                  intent(INOUT) :: nbpar
    real,                     intent(INOUT)    :: valparopt(nbpar)
    type(varying_string),     intent(INOUT) :: nompar (nbpar)

    !
    integer :: i
    character(len=250) :: tmp
    character(30) :: nom



    integer :: numplt
    integer :: pltread
    integer :: valopt

    numplt=1
    pltread=0

    call EnvoyerMsgHistorique('Lecture param.sti')

    do i = 1,nbpar
     nom=nompar(i)

     ! checking plt tags read
     if (pltread.gt.2) then
       call EnvoyerMsgHistorique('Erreur lecture param.sti: detection de plus de 2 plantes (tags plt) !')
       call exit(2)
     endif

    ! Detecting plt tag
    if (nompar(i) == 'plt') then

        valopt=int(valparopt(i))

        ! checking plt value
        if ((valopt > 2).AND.(valopt < 1)) then
           call EnvoyerMsgHistorique('Erreur lecture param.sti: numero de plante incorrect (0 ou >2) !')
           call exit(2)
        endif

        ! detecting same plt num
        if ((numplt == valopt).AND.(pltread > 0)) then
            call EnvoyerMsgHistorique('Erreur lecture param.sti: doublon pour le numero de plante !')
            call exit(2)
         endif

        !numplt=valparopt(i)
        numplt=valopt

        pltread=pltread+1

      CYCLE

    endif
    if (nompar(i) == 'aangst') then
      sta%P_aangst = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'abscission') then
      p(numplt)%P_abscission = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'aclim') then
      sta%P_aclim = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'adens') then
      p(numplt)%P_adens(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'adfol') then
      p(numplt)%P_adfol = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'adil') then
      p(numplt)%P_adil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'adilmax') then
      p(numplt)%P_adilmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'afpf') then
      p(numplt)%P_afpf = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'afruitpot') then
      p(numplt)%P_afruitpot(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(1)') then
      pg%P_ahres(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(2)') then
      pg%P_ahres(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(3)') then
      pg%P_ahres(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(4)') then
      pg%P_ahres(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(5)') then
      pg%P_ahres(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(6)') then
      pg%P_ahres(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(7)') then
      pg%P_ahres(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(8)') then
      pg%P_ahres(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(9)') then
      pg%P_ahres(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(10)') then
      pg%P_ahres(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(11)') then
      pg%P_ahres(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(12)') then
      pg%P_ahres(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(13)') then
      pg%P_ahres(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(14)') then
      pg%P_ahres(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(15)') then
      pg%P_ahres(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(16)') then
      pg%P_ahres(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(17)') then
      pg%P_ahres(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(18)') then
      pg%P_ahres(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(19)') then
      pg%P_ahres(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(20)') then
      pg%P_ahres(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ahres(21)') then
      pg%P_ahres(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(1)') then
      pg%P_akres(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(2)') then
      pg%P_akres(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(3)') then
      pg%P_akres(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(4)') then
      pg%P_akres(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(5)') then
      pg%P_akres(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(6)') then
      pg%P_akres(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(7)') then
      pg%P_akres(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(8)') then
      pg%P_akres(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(9)') then
      pg%P_akres(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(10)') then
      pg%P_akres(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(11)') then
      pg%P_akres(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(12)') then
      pg%P_akres(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(13)') then
      pg%P_akres(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(14)') then
      pg%P_akres(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(15)') then
      pg%P_akres(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(16)') then
      pg%P_akres(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(17)') then
      pg%P_akres(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(18)') then
      pg%P_akres(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(19)') then
      pg%P_akres(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(20)') then
      pg%P_akres(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'akres(21)') then
      pg%P_akres(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'aks') then
      sta%P_aks = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedo') then
      soil%P_albedo = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchplastique') then
      itk(numplt)%P_albedomulchplastique = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(1)') then
      pg%P_albedomulchresidus(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(2)') then
      pg%P_albedomulchresidus(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(3)') then
      pg%P_albedomulchresidus(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(4)') then
      pg%P_albedomulchresidus(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(5)') then
      pg%P_albedomulchresidus(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(6)') then
      pg%P_albedomulchresidus(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(7)') then
      pg%P_albedomulchresidus(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(8)') then
      pg%P_albedomulchresidus(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(9)') then
      pg%P_albedomulchresidus(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(10)') then
      pg%P_albedomulchresidus(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(11)') then
      pg%P_albedomulchresidus(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(12)') then
      pg%P_albedomulchresidus(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(13)') then
      pg%P_albedomulchresidus(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(14)') then
      pg%P_albedomulchresidus(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(15)') then
      pg%P_albedomulchresidus(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(16)') then
      pg%P_albedomulchresidus(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(17)') then
      pg%P_albedomulchresidus(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(18)') then
      pg%P_albedomulchresidus(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(19)') then
      pg%P_albedomulchresidus(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(20)') then
      pg%P_albedomulchresidus(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albedomulchresidus(21)') then
      pg%P_albedomulchresidus(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'albveg') then
      sta%P_albveg = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'allocfrmax') then
      p(numplt)%P_allocfrmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'alphaCO2') then
      p(numplt)%P_alphaCO2 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'alphapH') then
      pg%P_alphapH = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'alphaphot') then
      p(numplt)%P_alphaphot = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'alphapt') then
      sta%P_alphapt = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'altinversion') then
      sta%P_altinversion = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'altisimul') then
      sta%P_altisimul = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'altistation') then
      sta%P_altistation = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ampfroid') then
      p(numplt)%P_ampfroid = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(1)') then
      itk(numplt)%P_anitcoupe(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(2)') then
      itk(numplt)%P_anitcoupe(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(3)') then
      itk(numplt)%P_anitcoupe(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(4)') then
      itk(numplt)%P_anitcoupe(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(5)') then
      itk(numplt)%P_anitcoupe(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(6)') then
      itk(numplt)%P_anitcoupe(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(7)') then
      itk(numplt)%P_anitcoupe(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(8)') then
      itk(numplt)%P_anitcoupe(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(9)') then
      itk(numplt)%P_anitcoupe(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(10)') then
      itk(numplt)%P_anitcoupe(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(11)') then
      itk(numplt)%P_anitcoupe(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(12)') then
      itk(numplt)%P_anitcoupe(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(13)') then
      itk(numplt)%P_anitcoupe(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(14)') then
      itk(numplt)%P_anitcoupe(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(15)') then
      itk(numplt)%P_anitcoupe(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(16)') then
      itk(numplt)%P_anitcoupe(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(17)') then
      itk(numplt)%P_anitcoupe(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(18)') then
      itk(numplt)%P_anitcoupe(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(19)') then
      itk(numplt)%P_anitcoupe(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'anitcoupe(20)') then
      itk(numplt)%P_anitcoupe(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'argi') then
      soil%P_argi = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(1)') then
      pg%P_awb(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(2)') then
      pg%P_awb(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(3)') then
      pg%P_awb(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(4)') then
      pg%P_awb(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(5)') then
      pg%P_awb(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(6)') then
      pg%P_awb(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(7)') then
      pg%P_awb(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(8)') then
      pg%P_awb(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(9)') then
      pg%P_awb(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(10)') then
      pg%P_awb(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(11)') then
      pg%P_awb(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(12)') then
      pg%P_awb(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(13)') then
      pg%P_awb(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(14)') then
      pg%P_awb(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(15)') then
      pg%P_awb(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(16)') then
      pg%P_awb(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(17)') then
      pg%P_awb(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(18)') then
      pg%P_awb(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(19)') then
      pg%P_awb(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(20)') then
      pg%P_awb(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'awb(21)') then
      pg%P_awb(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bangst') then
      sta%P_bangst = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bdens') then
      p(numplt)%P_bdens = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bdil') then
      p(numplt)%P_bdil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bdilmax') then
      p(numplt)%P_bdilmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'belong') then
      p(numplt)%P_belong = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'beta') then
      pg%P_beta = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bformnappe') then
      pg%P_bformnappe = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bfpf') then
      p(numplt)%P_bfpf = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(1)') then
      pg%P_bhres(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(2)') then
      pg%P_bhres(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(3)') then
      pg%P_bhres(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(4)') then
      pg%P_bhres(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(5)') then
      pg%P_bhres(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(6)') then
      pg%P_bhres(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(7)') then
      pg%P_bhres(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(8)') then
      pg%P_bhres(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(9)') then
      pg%P_bhres(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(10)') then
      pg%P_bhres(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(11)') then
      pg%P_bhres(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(12)') then
      pg%P_bhres(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(13)') then
      pg%P_bhres(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(14)') then
      pg%P_bhres(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(15)') then
      pg%P_bhres(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(16)') then
      pg%P_bhres(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(17)') then
      pg%P_bhres(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(18)') then
      pg%P_bhres(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(19)') then
      pg%P_bhres(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(20)') then
      pg%P_bhres(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bhres(21)') then
      pg%P_bhres(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'biorognem') then
      itk(numplt)%P_biorognem = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(1)') then
      pg%P_bkres(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(2)') then
      pg%P_bkres(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(3)') then
      pg%P_bkres(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(4)') then
      pg%P_bkres(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(5)') then
      pg%P_bkres(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(6)') then
      pg%P_bkres(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(7)') then
      pg%P_bkres(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(8)') then
      pg%P_bkres(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(9)') then
      pg%P_bkres(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(10)') then
      pg%P_bkres(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(11)') then
      pg%P_bkres(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(12)') then
      pg%P_bkres(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(13)') then
      pg%P_bkres(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(14)') then
      pg%P_bkres(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(15)') then
      pg%P_bkres(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(16)') then
      pg%P_bkres(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(17)') then
      pg%P_bkres(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(18)') then
      pg%P_bkres(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(19)') then
      pg%P_bkres(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(20)') then
      pg%P_bkres(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bkres(21)') then
      pg%P_bkres(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bks') then
      sta%P_bks = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(1)') then
      pg%P_bwb(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(2)') then
      pg%P_bwb(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(3)') then
      pg%P_bwb(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(4)') then
      pg%P_bwb(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(5)') then
      pg%P_bwb(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(6)') then
      pg%P_bwb(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(7)') then
      pg%P_bwb(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(8)') then
      pg%P_bwb(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(9)') then
      pg%P_bwb(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(10)') then
      pg%P_bwb(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(11)') then
      pg%P_bwb(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(12)') then
      pg%P_bwb(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(13)') then
      pg%P_bwb(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(14)') then
      pg%P_bwb(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(15)') then
      pg%P_bwb(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(16)') then
      pg%P_bwb(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(17)') then
      pg%P_bwb(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(18)') then
      pg%P_bwb(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(19)') then
      pg%P_bwb(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(20)') then
      pg%P_bwb(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'bwb(21)') then
      pg%P_bwb(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cadencerec') then
      itk(numplt)%P_cadencerec = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'cailloux(1)') then
      soil%P_cailloux(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cailloux(2)') then
      soil%P_cailloux(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cailloux(3)') then
      soil%P_cailloux(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cailloux(4)') then
      soil%P_cailloux(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cailloux(5)') then
      soil%P_cailloux(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'calc') then
      soil%P_calc = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'capiljour') then
      soil%P_capiljour = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'celong') then
      p(numplt)%P_celong = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cfes') then
      soil%P_cfes = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cfpf') then
      p(numplt)%P_cfpf = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cgrain') then
      p(numplt)%P_cgrain = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cgrainv0') then
      p(numplt)%P_cgrainv0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cielclair') then
      sta%P_cielclair = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cmax_pdenit') then
      pg%P_cmax_pdenit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cmin_pdenit') then
      pg%P_cmin_pdenit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNgrainrec') then
      itk(numplt)%P_CNgrainrec = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(1)') then
      pg%P_CNresmax(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(2)') then
      pg%P_CNresmax(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(3)') then
      pg%P_CNresmax(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(4)') then
      pg%P_CNresmax(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(5)') then
      pg%P_CNresmax(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(6)') then
      pg%P_CNresmax(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(7)') then
      pg%P_CNresmax(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(8)') then
      pg%P_CNresmax(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(9)') then
      pg%P_CNresmax(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(10)') then
      pg%P_CNresmax(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(11)') then
      pg%P_CNresmax(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(12)') then
      pg%P_CNresmax(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(13)') then
      pg%P_CNresmax(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(14)') then
      pg%P_CNresmax(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(15)') then
      pg%P_CNresmax(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(16)') then
      pg%P_CNresmax(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(17)') then
      pg%P_CNresmax(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(18)') then
      pg%P_CNresmax(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(19)') then
      pg%P_CNresmax(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(20)') then
      pg%P_CNresmax(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmax(21)') then
      pg%P_CNresmax(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(1)') then
      pg%P_CNresmin(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(2)') then
      pg%P_CNresmin(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(3)') then
      pg%P_CNresmin(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(4)') then
      pg%P_CNresmin(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(5)') then
      pg%P_CNresmin(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(6)') then
      pg%P_CNresmin(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(7)') then
      pg%P_CNresmin(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(8)') then
      pg%P_CNresmin(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(9)') then
      pg%P_CNresmin(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(10)') then
      pg%P_CNresmin(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(11)') then
      pg%P_CNresmin(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(12)') then
      pg%P_CNresmin(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(13)') then
      pg%P_CNresmin(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(14)') then
      pg%P_CNresmin(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(15)') then
      pg%P_CNresmin(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(16)') then
      pg%P_CNresmin(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(17)') then
      pg%P_CNresmin(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(18)') then
      pg%P_CNresmin(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(19)') then
      pg%P_CNresmin(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(20)') then
      pg%P_CNresmin(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CNresmin(21)') then
      pg%P_CNresmin(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'codabri') then
      itk(numplt)%P_codabri = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codadret') then
      sta%P_codadret = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codaltitude') then
      sta%P_codaltitude = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codazofruit') then
      p(numplt)%P_codazofruit = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codazorac') then
      p(numplt)%P_codazorac = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codcaleffeuil') then
      itk(numplt)%P_codcaleffeuil = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codcalinflo') then
      p(numplt)%P_codcalinflo = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codcalrogne') then
      itk(numplt)%P_codcalrogne = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codcueille') then
      itk(numplt)%P_codcueille = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'code_hourly_wfps_denit') then
      pg%P_code_hourly_wfps_denit = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'code_hourly_wfps_nit') then
      pg%P_code_hourly_wfps_nit = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'code_pdenit') then
      pg%P_code_pdenit = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'code_ratiodenit') then
      pg%P_code_ratiodenit = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'code_rationit') then
      pg%P_code_rationit = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'code_tnit') then
      pg%P_code_tnit = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'code_vnit') then
      pg%P_code_vnit = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeactimulch') then
      pg%P_codeactimulch = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeaumin') then
      itk(numplt)%P_codeaumin = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codebeso') then
      p(numplt)%P_codebeso = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codebfroid') then
      p(numplt)%P_codebfroid = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codecailloux') then
      soil%P_codecailloux = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codecalferti') then
      t%P_codecalferti = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codecalirrig') then
      itk(numplt)%P_codecalirrig = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codecaltemp') then
      sta%P_codecaltemp = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeclaircie') then
      itk(numplt)%P_codeclaircie = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeclichange') then
      sta%P_codeclichange = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codedate_irrigauto') then
      t%P_codedate_irrigauto = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codedateappH2O') then
      itk(numplt)%P_codedateappH2O = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codedateappN') then
      itk(numplt)%P_codedateappN = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codedecirecolte') then
      itk(numplt)%P_codedecirecolte = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codedecisemis') then
      itk(numplt)%P_codedecisemis = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codedenit') then
      soil%P_codedenit = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codedormance') then
      p(numplt)%P_codedormance = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeDST') then
      itk(numplt)%P_codeDST = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeDSTnbcouche') then
      itk(numplt)%P_codeDSTnbcouche = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeDSTtass') then
      itk(numplt)%P_codeDSTtass = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codedyntalle(1)') then
      t%P_codedyntalle(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codedyntalle(2)') then
      t%P_codedyntalle(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeetp') then
      sta%P_codeetp = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codefauche') then
      itk(numplt)%P_codefauche = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codefente') then
      soil%P_codefente = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeffeuil') then
      itk(numplt)%P_codeffeuil = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codefixpot') then
      p(numplt)%P_codefixpot = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codefracappN') then
      itk(numplt)%P_codefracappN = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codefrmur') then
      pg%P_codefrmur = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codefxn') then
      pg%P_codefxn = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codegdh') then
      p(numplt)%P_codegdh = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codegdhdeb') then
      p(numplt)%P_codegdhdeb = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codegermin') then
      p(numplt)%P_codegermin = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeh2oact') then
      pg%P_codeh2oact = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codehypo') then
      p(numplt)%P_codehypo = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeindetermin') then
      p(numplt)%P_codeindetermin = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeinitprec') then
      pg%P_codeinitprec = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeINN') then
      p(numplt)%P_codeINN = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeinnact') then
      pg%P_codeinnact = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeintercept') then
      p(numplt)%P_codeintercept = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeir') then
      p(numplt)%P_codeir = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codelaitr') then
      p(numplt)%P_codelaitr = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codelegume') then
      p(numplt)%P_codelegume = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codemacropor') then
      soil%P_codemacropor = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codemicheur') then
      pg%P_codemicheur = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeminopt') then
      pg%P_codeminopt = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codemodfauche') then
      itk(numplt)%P_codemodfauche = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codemonocot') then
      p(numplt)%P_codemonocot = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codemontaison(1)') then
      t%P_codemontaison(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codemontaison(2)') then
      t%P_codemontaison(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codemortalracine') then
      t%P_codemortalracine = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codemsfinal') then
      pg%P_codemsfinal = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codenitrif') then
      soil%P_codenitrif = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeNmindec') then
      t%P_codeNmindec = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeoutscient') then
      pg%P_codeoutscient = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codepaillage') then
      itk(numplt)%P_codepaillage = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codepalissage') then
      itk(numplt)%P_codepalissage = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeperenne') then
      p(numplt)%P_codeperenne = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codephot') then
      p(numplt)%P_codephot = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeplisoleN') then
      p(numplt)%P_codeplisoleN = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codepluiepoquet') then
      t%P_codepluiepoquet = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeprofmes') then
      pg%P_codeprofmes = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderacine') then
      p(numplt)%P_coderacine = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderecolteassoc') then
      itk(numplt)%P_coderecolteassoc = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderemontcap') then
      soil%P_coderemontcap = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(1)') then
      itk(numplt)%P_coderes(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(2)') then
      itk(numplt)%P_coderes(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(3)') then
      itk(numplt)%P_coderes(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(4)') then
      itk(numplt)%P_coderes(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(5)') then
      itk(numplt)%P_coderes(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(6)') then
      itk(numplt)%P_coderes(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(7)') then
      itk(numplt)%P_coderes(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(8)') then
      itk(numplt)%P_coderes(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(9)') then
      itk(numplt)%P_coderes(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(10)') then
      itk(numplt)%P_coderes(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes(11)') then
      itk(numplt)%P_coderes(11) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderes_pature') then
      t%P_coderes_pature = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coderetflo') then
      p(numplt)%P_coderetflo = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codernet') then
      sta%P_codernet = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codesensibilite') then
      pg%P_codesensibilite = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeseprapport') then
      pg%P_codeseprapport = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codestade') then
      itk(numplt)%P_codestade = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codestrphot') then
      p(numplt)%P_codestrphot = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codeSWDRH') then
      t%P_codeSWDRH = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codesymbiose') then
      pg%P_codesymbiose = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetaille') then
      itk(numplt)%P_codetaille = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetemp') then
      p(numplt)%P_codetemp = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetempfauche') then
      t%P_codetempfauche = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetemprac') then
      p(numplt)%P_codetemprac = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetesthumN') then
      t%P_codetesthumN = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetradtec') then
      itk(numplt)%P_codetradtec = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetranspitalle') then
      t%P_codetranspitalle = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetransrad') then
      p(numplt)%P_codetransrad = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetremp') then
      p(numplt)%P_codetremp = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetrosee') then
      t%P_codetrosee = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetycailloux') then
      pg%P_codetycailloux = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetypeng') then
      pg%P_codetypeng = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codetypres') then
      pg%P_codetypres = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codgelflo') then
      p(numplt)%P_codgelflo = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codgeljuv') then
      p(numplt)%P_codgeljuv = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codgellev') then
      p(numplt)%P_codgellev = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codgelveg') then
      p(numplt)%P_codgelveg = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codhauteff') then
      itk(numplt)%P_codhauteff = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codhnappe') then
      pg%P_codhnappe = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codlainet') then
      p(numplt)%P_codlainet = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codlocferti') then
      itk(numplt)%P_codlocferti = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codlocirrig') then
      itk(numplt)%P_codlocirrig = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codoptim') then
      sc%codoptim = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codrainage') then
      soil%P_codrainage = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codrecolte') then
      itk(numplt)%P_codrecolte = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codrognage') then
      itk(numplt)%P_codrognage = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'codtrophrac') then
      p(numplt)%P_codtrophrac = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'coef_calcul_doseN') then
      t%P_coef_calcul_doseN = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coef_calcul_qres') then
      t%P_coef_calcul_qres = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefamflax') then
      p(numplt)%P_coefamflax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefb') then
      pg%P_coefb = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefdevil') then
      sta%P_coefdevil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefdrpmat') then
      p(numplt)%P_coefdrpmat = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefflodrp') then
      p(numplt)%P_coefflodrp = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coeflaxsen') then
      p(numplt)%P_coeflaxsen = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coeflevamf') then
      p(numplt)%P_coeflevamf = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coeflevdrp') then
      p(numplt)%P_coeflevdrp = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefmshaut') then
      p(numplt)%P_coefmshaut = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefracoupe(1)') then
      t%P_coefracoupe(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefracoupe(2)') then
      t%P_coefracoupe(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefrnet') then
      sta%P_coefrnet = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'coefsenlan') then
      p(numplt)%P_coefsenlan = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'concirr') then
      itk(numplt)%P_concirr = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'concNnodseuil') then
      p(numplt)%P_concNnodseuil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'concNrac0') then
      p(numplt)%P_concNrac0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'concNrac100') then
      p(numplt)%P_concNrac100 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'concrr') then
      pg%P_concrr = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'concseuil') then
      soil%P_concseuil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'contrdamax') then
      p(numplt)%P_contrdamax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'corecTrosee') then
      sta%P_corecTrosee = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'couvermulchplastique') then
      itk(numplt)%P_couvermulchplastique = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(1)') then
      itk(numplt)%P_Crespc(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(2)') then
      itk(numplt)%P_Crespc(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(3)') then
      itk(numplt)%P_Crespc(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(4)') then
      itk(numplt)%P_Crespc(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(5)') then
      itk(numplt)%P_Crespc(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(6)') then
      itk(numplt)%P_Crespc(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(7)') then
      itk(numplt)%P_Crespc(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(8)') then
      itk(numplt)%P_Crespc(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(9)') then
      itk(numplt)%P_Crespc(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(10)') then
      itk(numplt)%P_Crespc(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc(11)') then
      itk(numplt)%P_Crespc(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Crespc_pature') then
      t%P_Crespc_pature = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(1)') then
      pg%P_CroCo(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(2)') then
      pg%P_CroCo(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(3)') then
      pg%P_CroCo(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(4)') then
      pg%P_CroCo(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(5)') then
      pg%P_CroCo(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(6)') then
      pg%P_CroCo(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(7)') then
      pg%P_CroCo(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(8)') then
      pg%P_CroCo(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(9)') then
      pg%P_CroCo(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(10)') then
      pg%P_CroCo(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(11)') then
      pg%P_CroCo(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(12)') then
      pg%P_CroCo(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(13)') then
      pg%P_CroCo(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(14)') then
      pg%P_CroCo(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(15)') then
      pg%P_CroCo(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(16)') then
      pg%P_CroCo(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(17)') then
      pg%P_CroCo(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(18)') then
      pg%P_CroCo(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(19)') then
      pg%P_CroCo(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(20)') then
      pg%P_CroCo(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CroCo(21)') then
      pg%P_CroCo(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'croirac') then
      p(numplt)%P_croirac(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(1)') then
      itk(numplt)%P_CsurNres(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(2)') then
      itk(numplt)%P_CsurNres(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(3)') then
      itk(numplt)%P_CsurNres(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(4)') then
      itk(numplt)%P_CsurNres(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(5)') then
      itk(numplt)%P_CsurNres(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(6)') then
      itk(numplt)%P_CsurNres(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(7)') then
      itk(numplt)%P_CsurNres(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(8)') then
      itk(numplt)%P_CsurNres(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(9)') then
      itk(numplt)%P_CsurNres(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(10)') then
      itk(numplt)%P_CsurNres(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNres(11)') then
      itk(numplt)%P_CsurNres(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'CsurNsol') then
      soil%P_CsurNsol = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cvent') then
      sta%P_cvent = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(1)') then
      pg%P_cwb(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(2)') then
      pg%P_cwb(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(3)') then
      pg%P_cwb(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(4)') then
      pg%P_cwb(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(5)') then
      pg%P_cwb(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(6)') then
      pg%P_cwb(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(7)') then
      pg%P_cwb(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(8)') then
      pg%P_cwb(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(9)') then
      pg%P_cwb(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(10)') then
      pg%P_cwb(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(11)') then
      pg%P_cwb(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(12)') then
      pg%P_cwb(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(13)') then
      pg%P_cwb(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(14)') then
      pg%P_cwb(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(15)') then
      pg%P_cwb(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(16)') then
      pg%P_cwb(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(17)') then
      pg%P_cwb(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(18)') then
      pg%P_cwb(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(19)') then
      pg%P_cwb(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(20)') then
      pg%P_cwb(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'cwb(21)') then
      pg%P_cwb(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dachisel') then
      itk(numplt)%P_dachisel = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dacohes') then
      pg%P_dacohes = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'DAF(1)') then
      soil%P_DAF(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'DAF(2)') then
      soil%P_DAF(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'DAF(3)') then
      soil%P_DAF(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'DAF(4)') then
      soil%P_DAF(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'DAF(5)') then
      soil%P_DAF(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dalabour') then
      itk(numplt)%P_dalabour = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'darecolte') then
      itk(numplt)%P_darecolte = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dasemis') then
      itk(numplt)%P_dasemis = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'daseuilbas') then
      pg%P_daseuilbas = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'daseuilhaut') then
      pg%P_daseuilhaut = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'datedeb_irrigauto') then
      t%P_datedeb_irrigauto = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'datefin_irrigauto') then
      t%P_datefin_irrigauto = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'debsenrac') then
      p(numplt)%P_debsenrac = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'deneng(1)') then
      pg%P_deneng(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'deneng(2)') then
      pg%P_deneng(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'deneng(3)') then
      pg%P_deneng(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'deneng(4)') then
      pg%P_deneng(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'deneng(5)') then
      pg%P_deneng(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'deneng(6)') then
      pg%P_deneng(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'deneng(7)') then
      pg%P_deneng(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'deneng(8)') then
      pg%P_deneng(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'densinitial(1)') then
      p(numplt)%P_densinitial(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'densinitial(2)') then
      p(numplt)%P_densinitial(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'densinitial(3)') then
      p(numplt)%P_densinitial(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'densinitial(4)') then
      p(numplt)%P_densinitial(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'densinitial(5)') then
      p(numplt)%P_densinitial(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'densitesem') then
      itk(numplt)%P_densitesem = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'deshydbase') then
      p(numplt)%P_deshydbase = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dfolbas') then
      p(numplt)%P_dfolbas = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dfolhaut') then
      p(numplt)%P_dfolhaut = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dfpf') then
      p(numplt)%P_dfpf = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'difN') then
      pg%P_difN = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'diftherm') then
      pg%P_diftherm = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'distdrain') then
      pg%P_distdrain = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dlaimax') then
      p(numplt)%P_dlaimax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dlaimaxbrut') then
      p(numplt)%P_dlaimaxbrut = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dlaimin') then
      p(numplt)%P_dlaimin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dltamsmaxsen') then
      p(numplt)%P_dltamsmaxsen = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dltamsminsen') then
      p(numplt)%P_dltamsminsen = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(1)') then
      itk(numplt)%P_doseI(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(2)') then
      itk(numplt)%P_doseI(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(3)') then
      itk(numplt)%P_doseI(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(4)') then
      itk(numplt)%P_doseI(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(5)') then
      itk(numplt)%P_doseI(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(6)') then
      itk(numplt)%P_doseI(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(7)') then
      itk(numplt)%P_doseI(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(8)') then
      itk(numplt)%P_doseI(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(9)') then
      itk(numplt)%P_doseI(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(10)') then
      itk(numplt)%P_doseI(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(11)') then
      itk(numplt)%P_doseI(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(12)') then
      itk(numplt)%P_doseI(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(13)') then
      itk(numplt)%P_doseI(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(14)') then
      itk(numplt)%P_doseI(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(15)') then
      itk(numplt)%P_doseI(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(16)') then
      itk(numplt)%P_doseI(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(17)') then
      itk(numplt)%P_doseI(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(18)') then
      itk(numplt)%P_doseI(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(19)') then
      itk(numplt)%P_doseI(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(20)') then
      itk(numplt)%P_doseI(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(21)') then
      itk(numplt)%P_doseI(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(22)') then
      itk(numplt)%P_doseI(22) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(23)') then
      itk(numplt)%P_doseI(23) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(24)') then
      itk(numplt)%P_doseI(24) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(25)') then
      itk(numplt)%P_doseI(25) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(26)') then
      itk(numplt)%P_doseI(26) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(27)') then
      itk(numplt)%P_doseI(27) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(28)') then
      itk(numplt)%P_doseI(28) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(29)') then
      itk(numplt)%P_doseI(29) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseI(30)') then
      itk(numplt)%P_doseI(30) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseirrigmin') then
      itk(numplt)%P_doseirrigmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(1)') then
      itk(numplt)%P_doseN(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(2)') then
      itk(numplt)%P_doseN(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(3)') then
      itk(numplt)%P_doseN(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(4)') then
      itk(numplt)%P_doseN(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(5)') then
      itk(numplt)%P_doseN(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(6)') then
      itk(numplt)%P_doseN(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(7)') then
      itk(numplt)%P_doseN(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(8)') then
      itk(numplt)%P_doseN(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(9)') then
      itk(numplt)%P_doseN(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(10)') then
      itk(numplt)%P_doseN(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(11)') then
      itk(numplt)%P_doseN(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(12)') then
      itk(numplt)%P_doseN(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(13)') then
      itk(numplt)%P_doseN(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(14)') then
      itk(numplt)%P_doseN(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(15)') then
      itk(numplt)%P_doseN(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(16)') then
      itk(numplt)%P_doseN(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(17)') then
      itk(numplt)%P_doseN(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(18)') then
      itk(numplt)%P_doseN(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(19)') then
      itk(numplt)%P_doseN(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'doseN(20)') then
      itk(numplt)%P_doseN(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dosimx') then
      itk(numplt)%P_dosimx = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dosimxN') then
      t%P_dosimxN = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dpHvolmax') then
      pg%P_dpHvolmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'draclong') then
      p(numplt)%P_draclong = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'dureefruit') then
      p(numplt)%P_dureefruit(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'durvieF') then
      p(numplt)%P_durvieF(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'durviesupmax') then
      p(numplt)%P_durviesupmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eau_mini_decisemis') then
      t%P_eau_mini_decisemis = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'eaures(1)') then
      itk(numplt)%P_eaures(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(2)') then
      itk(numplt)%P_eaures(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(3)') then
      itk(numplt)%P_eaures(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(4)') then
      itk(numplt)%P_eaures(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(5)') then
      itk(numplt)%P_eaures(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(6)') then
      itk(numplt)%P_eaures(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(7)') then
      itk(numplt)%P_eaures(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(8)') then
      itk(numplt)%P_eaures(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(9)') then
      itk(numplt)%P_eaures(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(10)') then
      itk(numplt)%P_eaures(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures(11)') then
      itk(numplt)%P_eaures(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'eaures_pature') then
      t%P_eaures_pature = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ecartdrain') then
      soil%P_ecartdrain = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'efcroijuv') then
      p(numplt)%P_efcroijuv = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'efcroirepro') then
      p(numplt)%P_efcroirepro = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'efcroiveg') then
      p(numplt)%P_efcroiveg = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'effeuil') then
      itk(numplt)%P_effeuil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'effirr') then
      itk(numplt)%P_effirr = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'elmax') then
      p(numplt)%P_elmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'engamm(1)') then
      pg%P_engamm(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'engamm(2)') then
      pg%P_engamm(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'engamm(3)') then
      pg%P_engamm(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'engamm(4)') then
      pg%P_engamm(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'engamm(5)') then
      pg%P_engamm(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'engamm(6)') then
      pg%P_engamm(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'engamm(7)') then
      pg%P_engamm(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'engamm(8)') then
      pg%P_engamm(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'engrais(1)') then
      itk(numplt)%P_engrais(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais(2)') then
      itk(numplt)%P_engrais(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais(3)') then
      itk(numplt)%P_engrais(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais(4)') then
      itk(numplt)%P_engrais(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais(5)') then
      itk(numplt)%P_engrais(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais(6)') then
      itk(numplt)%P_engrais(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais(7)') then
      itk(numplt)%P_engrais(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais(8)') then
      itk(numplt)%P_engrais(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais(9)') then
      itk(numplt)%P_engrais(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais(10)') then
      itk(numplt)%P_engrais(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'engrais_pature') then
      t%P_engrais_pature = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'envfruit') then
      p(numplt)%P_envfruit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'epc(1)') then
      soil%P_epc(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'epc(2)') then
      soil%P_epc(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'epc(3)') then
      soil%P_epc(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'epc(4)') then
      soil%P_epc(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'epc(5)') then
      soil%P_epc(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'epd(1)') then
      soil%P_epd(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'epd(2)') then
      soil%P_epd(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'epd(3)') then
      soil%P_epd(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'epd(4)') then
      soil%P_epd(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'epd(5)') then
      soil%P_epd(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'extin') then
      p(numplt)%P_extin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fhminsat') then
      pg%P_fhminsat = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'finert') then
      pg%P_finert = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fixmax') then
      p(numplt)%P_fixmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fixmaxgr') then
      p(numplt)%P_fixmaxgr = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fixmaxveg') then
      p(numplt)%P_fixmaxveg = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'flagecriture') then
      pg%P_flagecriture = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'fmin1') then
      pg%P_fmin1 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fmin2') then
      pg%P_fmin2 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fmin3') then
      pg%P_fmin3 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fNCbiomin') then
      pg%P_fNCbiomin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fNmindecmin') then
      t%P_fNmindecmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fnx') then
      pg%P_fnx = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'forme') then
      p(numplt)%P_forme = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'fracN(1)') then
      itk(numplt)%P_fracN(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fracN(2)') then
      itk(numplt)%P_fracN(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fracN(3)') then
      itk(numplt)%P_fracN(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fracN(4)') then
      itk(numplt)%P_fracN(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fracN(5)') then
      itk(numplt)%P_fracN(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fracN(6)') then
      itk(numplt)%P_fracN(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fracN(7)') then
      itk(numplt)%P_fracN(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fracN(8)') then
      itk(numplt)%P_fracN(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fracN(9)') then
      itk(numplt)%P_fracN(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fracN(10)') then
      itk(numplt)%P_fracN(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fredkN') then
      pg%P_fredkN = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fredlN') then
      pg%P_fredlN = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'fredNsup') then
      pg%P_fredNsup = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ftemh') then
      pg%P_ftemh = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ftemha') then
      pg%P_ftemha = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ftemr') then
      pg%P_ftemr = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ftemra') then
      pg%P_ftemra = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'gradtn') then
      sta%P_gradtn = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'gradtninv') then
      sta%P_gradtninv = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'gradtx') then
      sta%P_gradtx = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'h2ofeuiljaune') then
      p(numplt)%P_h2ofeuiljaune = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'h2ofeuilverte') then
      p(numplt)%P_h2ofeuilverte = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'h2ofrvert') then
      p(numplt)%P_h2ofrvert = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'h2ograinmax') then
      itk(numplt)%P_h2ograinmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'h2ograinmin') then
      itk(numplt)%P_h2ograinmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'h2oreserve') then
      p(numplt)%P_h2oreserve = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'h2otigestruc') then
      p(numplt)%P_h2otigestruc = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautbase') then
      p(numplt)%P_hautbase = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(1)') then
      itk(numplt)%P_hautcoupe(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(2)') then
      itk(numplt)%P_hautcoupe(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(3)') then
      itk(numplt)%P_hautcoupe(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(4)') then
      itk(numplt)%P_hautcoupe(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(5)') then
      itk(numplt)%P_hautcoupe(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(6)') then
      itk(numplt)%P_hautcoupe(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(7)') then
      itk(numplt)%P_hautcoupe(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(8)') then
      itk(numplt)%P_hautcoupe(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(9)') then
      itk(numplt)%P_hautcoupe(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(10)') then
      itk(numplt)%P_hautcoupe(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(11)') then
      itk(numplt)%P_hautcoupe(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(12)') then
      itk(numplt)%P_hautcoupe(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(13)') then
      itk(numplt)%P_hautcoupe(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(14)') then
      itk(numplt)%P_hautcoupe(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(15)') then
      itk(numplt)%P_hautcoupe(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(16)') then
      itk(numplt)%P_hautcoupe(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(17)') then
      itk(numplt)%P_hautcoupe(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(18)') then
      itk(numplt)%P_hautcoupe(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(19)') then
      itk(numplt)%P_hautcoupe(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupe(20)') then
      itk(numplt)%P_hautcoupe(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautcoupedefaut') then
      itk(numplt)%P_hautcoupedefaut = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautmax') then
      p(numplt)%P_hautmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautmaxtec') then
      itk(numplt)%P_hautmaxtec = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautrogne') then
      itk(numplt)%P_hautrogne = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(1)') then
      pg%P_hcccx(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(2)') then
      pg%P_hcccx(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(3)') then
      pg%P_hcccx(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(4)') then
      pg%P_hcccx(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(5)') then
      pg%P_hcccx(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(6)') then
      pg%P_hcccx(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(7)') then
      pg%P_hcccx(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(8)') then
      pg%P_hcccx(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(9)') then
      pg%P_hcccx(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hcccx(10)') then
      pg%P_hcccx(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hccf(1)') then
      soil%P_hccf(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hccf(2)') then
      soil%P_hccf(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hccf(3)') then
      soil%P_hccf(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hccf(4)') then
      soil%P_hccf(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hccf(5)') then
      soil%P_hccf(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Hinitf(1)') then
      sc%P_Hinitf(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Hinitf(2)') then
      sc%P_Hinitf(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Hinitf(3)') then
      sc%P_Hinitf(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Hinitf(4)') then
      sc%P_Hinitf(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Hinitf(5)') then
      sc%P_Hinitf(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hminf(1)') then
      soil%P_hminf(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hminf(2)') then
      soil%P_hminf(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hminf(3)') then
      soil%P_hminf(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hminf(4)') then
      soil%P_hminf(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hminf(5)') then
      soil%P_hminf(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hminm') then
      pg%P_hminm = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hminn') then
      pg%P_hminn = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hoptm') then
      pg%P_hoptm = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hoptn') then
      pg%P_hoptn = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'huilerec') then
      itk(numplt)%P_huilerec = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'humcapil') then
      soil%P_humcapil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'humirac_decisemis') then
      t%P_humirac_decisemis = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'iamf') then
      itk(numplt)%P_iamf = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'ichsl') then
      sc%P_ichsl = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'idebdorm') then
      p(numplt)%P_idebdorm = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'idrp') then
      itk(numplt)%P_idrp = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'ifindorm') then
      p(numplt)%P_ifindorm = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'iflo') then
      itk(numplt)%P_iflo = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'ifwater') then
      sc%P_ifwater = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'ilan') then
      itk(numplt)%P_ilan = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'ilax') then
      itk(numplt)%P_ilax = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'ilev') then
      itk(numplt)%P_ilev = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'imat') then
      itk(numplt)%P_imat = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'infil(0)') then
      soil%P_infil(0) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'infil(1)') then
      soil%P_infil(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'infil(2)') then
      soil%P_infil(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'infil(3)') then
      soil%P_infil(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'infil(4)') then
      soil%P_infil(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'infil(5)') then
      soil%P_infil(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'inflomax') then
      p(numplt)%P_inflomax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'infrecouv') then
      p(numplt)%P_infrecouv = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'iniprofil') then
      pg%P_iniprofil = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'inngrain1') then
      p(numplt)%P_inngrain1 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'inngrain2') then
      p(numplt)%P_inngrain2 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'INNimin') then
      p(numplt)%P_INNimin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'INNmin') then
      p(numplt)%P_INNmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'innsen') then
      p(numplt)%P_innsen = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'innturgmin') then
      p(numplt)%P_innturgmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'interrang') then
      itk(numplt)%P_interrang = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'iplt0') then
      itk(numplt)%P_iplt0 = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'irec') then
      itk(numplt)%P_irec = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'irecbutoir') then
      itk(numplt)%P_irecbutoir = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'irmax') then
      p(numplt)%P_irmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'irrlev') then
      pg%P_irrlev = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'isen') then
      itk(numplt)%P_isen = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'iwater') then
      sc%P_iwater = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(1)') then
      itk(numplt)%P_julapI(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(2)') then
      itk(numplt)%P_julapI(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(3)') then
      itk(numplt)%P_julapI(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(4)') then
      itk(numplt)%P_julapI(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(5)') then
      itk(numplt)%P_julapI(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(6)') then
      itk(numplt)%P_julapI(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(7)') then
      itk(numplt)%P_julapI(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(8)') then
      itk(numplt)%P_julapI(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(9)') then
      itk(numplt)%P_julapI(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(10)') then
      itk(numplt)%P_julapI(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(11)') then
      itk(numplt)%P_julapI(11) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(12)') then
      itk(numplt)%P_julapI(12) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(13)') then
      itk(numplt)%P_julapI(13) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(14)') then
      itk(numplt)%P_julapI(14) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(15)') then
      itk(numplt)%P_julapI(15) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(16)') then
      itk(numplt)%P_julapI(16) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(17)') then
      itk(numplt)%P_julapI(17) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(18)') then
      itk(numplt)%P_julapI(18) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(19)') then
      itk(numplt)%P_julapI(19) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(20)') then
      itk(numplt)%P_julapI(20) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(21)') then
      itk(numplt)%P_julapI(21) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(22)') then
      itk(numplt)%P_julapI(22) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(23)') then
      itk(numplt)%P_julapI(23) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(24)') then
      itk(numplt)%P_julapI(24) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(25)') then
      itk(numplt)%P_julapI(25) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(26)') then
      itk(numplt)%P_julapI(26) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(27)') then
      itk(numplt)%P_julapI(27) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(28)') then
      itk(numplt)%P_julapI(28) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(29)') then
      itk(numplt)%P_julapI(29) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapI(30)') then
      itk(numplt)%P_julapI(30) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(1)') then
      itk(numplt)%P_julapN(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(2)') then
      itk(numplt)%P_julapN(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(3)') then
      itk(numplt)%P_julapN(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(4)') then
      itk(numplt)%P_julapN(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(5)') then
      itk(numplt)%P_julapN(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(6)') then
      itk(numplt)%P_julapN(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(7)') then
      itk(numplt)%P_julapN(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(8)') then
      itk(numplt)%P_julapN(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(9)') then
      itk(numplt)%P_julapN(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(10)') then
      itk(numplt)%P_julapN(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(11)') then
      itk(numplt)%P_julapN(11) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(12)') then
      itk(numplt)%P_julapN(12) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(13)') then
      itk(numplt)%P_julapN(13) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(14)') then
      itk(numplt)%P_julapN(14) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(15)') then
      itk(numplt)%P_julapN(15) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(16)') then
      itk(numplt)%P_julapN(16) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(17)') then
      itk(numplt)%P_julapN(17) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(18)') then
      itk(numplt)%P_julapN(18) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(19)') then
      itk(numplt)%P_julapN(19) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julapN(20)') then
      itk(numplt)%P_julapN(20) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(1)') then
      itk(numplt)%P_juleclair(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(2)') then
      itk(numplt)%P_juleclair(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(3)') then
      itk(numplt)%P_juleclair(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(4)') then
      itk(numplt)%P_juleclair(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(5)') then
      itk(numplt)%P_juleclair(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(6)') then
      itk(numplt)%P_juleclair(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(7)') then
      itk(numplt)%P_juleclair(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(8)') then
      itk(numplt)%P_juleclair(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(9)') then
      itk(numplt)%P_juleclair(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleclair(10)') then
      itk(numplt)%P_juleclair(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'juleffeuil') then
      itk(numplt)%P_juleffeuil = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(1)') then
      itk(numplt)%P_julfauche(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(2)') then
      itk(numplt)%P_julfauche(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(3)') then
      itk(numplt)%P_julfauche(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(4)') then
      itk(numplt)%P_julfauche(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(5)') then
      itk(numplt)%P_julfauche(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(6)') then
      itk(numplt)%P_julfauche(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(7)') then
      itk(numplt)%P_julfauche(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(8)') then
      itk(numplt)%P_julfauche(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(9)') then
      itk(numplt)%P_julfauche(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(10)') then
      itk(numplt)%P_julfauche(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(11)') then
      itk(numplt)%P_julfauche(11) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(12)') then
      itk(numplt)%P_julfauche(12) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(13)') then
      itk(numplt)%P_julfauche(13) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(14)') then
      itk(numplt)%P_julfauche(14) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(15)') then
      itk(numplt)%P_julfauche(15) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(16)') then
      itk(numplt)%P_julfauche(16) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(17)') then
      itk(numplt)%P_julfauche(17) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(18)') then
      itk(numplt)%P_julfauche(18) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(19)') then
      itk(numplt)%P_julfauche(19) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julfauche(20)') then
      itk(numplt)%P_julfauche(20) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julouvre2') then
      itk(numplt)%P_julouvre2 = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julouvre3') then
      itk(numplt)%P_julouvre3 = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(1)') then
      itk(numplt)%P_julres(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(2)') then
      itk(numplt)%P_julres(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(3)') then
      itk(numplt)%P_julres(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(4)') then
      itk(numplt)%P_julres(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(5)') then
      itk(numplt)%P_julres(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(6)') then
      itk(numplt)%P_julres(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(7)') then
      itk(numplt)%P_julres(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(8)') then
      itk(numplt)%P_julres(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(9)') then
      itk(numplt)%P_julres(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(10)') then
      itk(numplt)%P_julres(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julres(11)') then
      itk(numplt)%P_julres(11) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julrogne') then
      itk(numplt)%P_julrogne = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultaille') then
      itk(numplt)%P_jultaille = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(1)') then
      itk(numplt)%P_jultrav(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(2)') then
      itk(numplt)%P_jultrav(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(3)') then
      itk(numplt)%P_jultrav(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(4)') then
      itk(numplt)%P_jultrav(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(5)') then
      itk(numplt)%P_jultrav(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(6)') then
      itk(numplt)%P_jultrav(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(7)') then
      itk(numplt)%P_jultrav(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(8)') then
      itk(numplt)%P_jultrav(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(9)') then
      itk(numplt)%P_jultrav(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(10)') then
      itk(numplt)%P_jultrav(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jultrav(11)') then
      itk(numplt)%P_jultrav(11) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'julvernal') then
      p(numplt)%P_julvernal = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'jvc') then
      p(numplt)%P_jvc(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'jvcmini') then
      p(numplt)%P_jvcmini = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Kamm') then
      pg%P_Kamm = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(1)') then
      pg%P_kbio(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(2)') then
      pg%P_kbio(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(3)') then
      pg%P_kbio(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(4)') then
      pg%P_kbio(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(5)') then
      pg%P_kbio(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(6)') then
      pg%P_kbio(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(7)') then
      pg%P_kbio(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(8)') then
      pg%P_kbio(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(9)') then
      pg%P_kbio(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(10)') then
      pg%P_kbio(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(11)') then
      pg%P_kbio(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(12)') then
      pg%P_kbio(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(13)') then
      pg%P_kbio(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(14)') then
      pg%P_kbio(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(15)') then
      pg%P_kbio(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(16)') then
      pg%P_kbio(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(17)') then
      pg%P_kbio(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(18)') then
      pg%P_kbio(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(19)') then
      pg%P_kbio(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(20)') then
      pg%P_kbio(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kbio(21)') then
      pg%P_kbio(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(1)') then
      pg%P_kcouvmlch(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(2)') then
      pg%P_kcouvmlch(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(3)') then
      pg%P_kcouvmlch(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(4)') then
      pg%P_kcouvmlch(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(5)') then
      pg%P_kcouvmlch(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(6)') then
      pg%P_kcouvmlch(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(7)') then
      pg%P_kcouvmlch(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(8)') then
      pg%P_kcouvmlch(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(9)') then
      pg%P_kcouvmlch(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(10)') then
      pg%P_kcouvmlch(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(11)') then
      pg%P_kcouvmlch(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(12)') then
      pg%P_kcouvmlch(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(13)') then
      pg%P_kcouvmlch(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(14)') then
      pg%P_kcouvmlch(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(15)') then
      pg%P_kcouvmlch(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(16)') then
      pg%P_kcouvmlch(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(17)') then
      pg%P_kcouvmlch(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(18)') then
      pg%P_kcouvmlch(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(19)') then
      pg%P_kcouvmlch(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(20)') then
      pg%P_kcouvmlch(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kcouvmlch(21)') then
      pg%P_kcouvmlch(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Kd') then
      pg%P_Kd = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kdesat') then
      pg%P_kdesat = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'khaut') then
      pg%P_khaut = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Kmabs1') then
      p(numplt)%P_Kmabs1 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Kmabs2') then
      p(numplt)%P_Kmabs2 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kmax') then
      p(numplt)%P_kmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'krepracperm') then
      p(numplt)%P_krepracperm = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'krepracseu') then
      p(numplt)%P_krepracseu = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ksol') then
      soil%P_ksol = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'kstemflow') then
      p(numplt)%P_kstemflow = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ktrou') then
      p(numplt)%P_ktrou = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lai0') then
      p(numplt)%P_lai0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'laicomp') then
      p(numplt)%P_laicomp = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'laidebeff') then
      itk(numplt)%P_laidebeff = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'laieffeuil') then
      itk(numplt)%P_laieffeuil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'laiplantule') then
      p(numplt)%P_laiplantule = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(1)') then
      itk(numplt)%P_lairesiduel(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(2)') then
      itk(numplt)%P_lairesiduel(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(3)') then
      itk(numplt)%P_lairesiduel(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(4)') then
      itk(numplt)%P_lairesiduel(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(5)') then
      itk(numplt)%P_lairesiduel(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(6)') then
      itk(numplt)%P_lairesiduel(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(7)') then
      itk(numplt)%P_lairesiduel(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(8)') then
      itk(numplt)%P_lairesiduel(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(9)') then
      itk(numplt)%P_lairesiduel(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(10)') then
      itk(numplt)%P_lairesiduel(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(11)') then
      itk(numplt)%P_lairesiduel(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(12)') then
      itk(numplt)%P_lairesiduel(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(13)') then
      itk(numplt)%P_lairesiduel(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(14)') then
      itk(numplt)%P_lairesiduel(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(15)') then
      itk(numplt)%P_lairesiduel(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(16)') then
      itk(numplt)%P_lairesiduel(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(17)') then
      itk(numplt)%P_lairesiduel(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(18)') then
      itk(numplt)%P_lairesiduel(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(19)') then
      itk(numplt)%P_lairesiduel(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lairesiduel(20)') then
      itk(numplt)%P_lairesiduel(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'largrogne') then
      itk(numplt)%P_largrogne = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'largtec') then
      itk(numplt)%P_largtec = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'latitude') then
      sta%P_latitude = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'locferti') then
      itk(numplt)%P_locferti = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'locirrig') then
      itk(numplt)%P_locirrig = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'longsperac') then
      p(numplt)%P_longsperac = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lvfront') then
      p(numplt)%P_lvfront = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'lvopt') then
      pg%P_lvopt = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'magrain0') then
      p(numplt)%P_magrain0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'margerogne') then
      itk(numplt)%P_margerogne = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masec0') then
      p(numplt)%P_masec0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masecmeta') then
      p(numplt)%P_masecmeta = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masecNmax') then
      p(numplt)%P_masecNmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masecplantule') then
      p(numplt)%P_masecplantule = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(1)') then
      pg%P_masvolcx(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(2)') then
      pg%P_masvolcx(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(3)') then
      pg%P_masvolcx(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(4)') then
      pg%P_masvolcx(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(5)') then
      pg%P_masvolcx(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(6)') then
      pg%P_masvolcx(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(7)') then
      pg%P_masvolcx(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(8)') then
      pg%P_masvolcx(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(9)') then
      pg%P_masvolcx(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'masvolcx(10)') then
      pg%P_masvolcx(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'max_pdenit') then
      pg%P_max_pdenit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'maxazorac') then
      p(numplt)%P_maxazorac = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'maxtalle(1)') then
      t%P_maxtalle(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'maxtalle(2)') then
      t%P_maxtalle(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'min_pdenit') then
      pg%P_min_pdenit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'minazorac') then
      p(numplt)%P_minazorac = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'minefnra') then
      p(numplt)%P_minefnra = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mouillabil') then
      p(numplt)%P_mouillabil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mouillabilmulch(1)') then
      pg%P_mouillabilmulch(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mouillabilmulch(2)') then
      pg%P_mouillabilmulch(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mouillabilmulch(3)') then
      pg%P_mouillabilmulch(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mouillabilmulch(4)') then
      pg%P_mouillabilmulch(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mouillabilmulch(5)') then
      pg%P_mouillabilmulch(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(1)') then
      itk(numplt)%P_mscoupemini(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(2)') then
      itk(numplt)%P_mscoupemini(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(3)') then
      itk(numplt)%P_mscoupemini(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(4)') then
      itk(numplt)%P_mscoupemini(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(5)') then
      itk(numplt)%P_mscoupemini(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(6)') then
      itk(numplt)%P_mscoupemini(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(7)') then
      itk(numplt)%P_mscoupemini(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(8)') then
      itk(numplt)%P_mscoupemini(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(9)') then
      itk(numplt)%P_mscoupemini(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(10)') then
      itk(numplt)%P_mscoupemini(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(11)') then
      itk(numplt)%P_mscoupemini(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(12)') then
      itk(numplt)%P_mscoupemini(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(13)') then
      itk(numplt)%P_mscoupemini(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(14)') then
      itk(numplt)%P_mscoupemini(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(15)') then
      itk(numplt)%P_mscoupemini(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(16)') then
      itk(numplt)%P_mscoupemini(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(17)') then
      itk(numplt)%P_mscoupemini(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(18)') then
      itk(numplt)%P_mscoupemini(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(19)') then
      itk(numplt)%P_mscoupemini(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mscoupemini(20)') then
      itk(numplt)%P_mscoupemini(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(1)') then
      itk(numplt)%P_msresiduel(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(2)') then
      itk(numplt)%P_msresiduel(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(3)') then
      itk(numplt)%P_msresiduel(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(4)') then
      itk(numplt)%P_msresiduel(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(5)') then
      itk(numplt)%P_msresiduel(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(6)') then
      itk(numplt)%P_msresiduel(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(7)') then
      itk(numplt)%P_msresiduel(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(8)') then
      itk(numplt)%P_msresiduel(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(9)') then
      itk(numplt)%P_msresiduel(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(10)') then
      itk(numplt)%P_msresiduel(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(11)') then
      itk(numplt)%P_msresiduel(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(12)') then
      itk(numplt)%P_msresiduel(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(13)') then
      itk(numplt)%P_msresiduel(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(14)') then
      itk(numplt)%P_msresiduel(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(15)') then
      itk(numplt)%P_msresiduel(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(16)') then
      itk(numplt)%P_msresiduel(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(17)') then
      itk(numplt)%P_msresiduel(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(18)') then
      itk(numplt)%P_msresiduel(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(19)') then
      itk(numplt)%P_msresiduel(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'msresiduel(20)') then
      itk(numplt)%P_msresiduel(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'mulchbat') then
      soil%P_mulchbat = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbcueille') then
      itk(numplt)%P_nbcueille = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbfeuilplant') then
      p(numplt)%P_nbfeuilplant = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbfgellev') then
      p(numplt)%P_nbfgellev = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbgrmax') then
      p(numplt)%P_nbgrmax(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbgrmin') then
      p(numplt)%P_nbgrmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinflo') then
      p(numplt)%P_nbinflo = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(1)') then
      itk(numplt)%P_nbinfloecl(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(2)') then
      itk(numplt)%P_nbinfloecl(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(3)') then
      itk(numplt)%P_nbinfloecl(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(4)') then
      itk(numplt)%P_nbinfloecl(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(5)') then
      itk(numplt)%P_nbinfloecl(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(6)') then
      itk(numplt)%P_nbinfloecl(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(7)') then
      itk(numplt)%P_nbinfloecl(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(8)') then
      itk(numplt)%P_nbinfloecl(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(9)') then
      itk(numplt)%P_nbinfloecl(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbinfloecl(10)') then
      itk(numplt)%P_nbinfloecl(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nbj_pr_apres_semis') then
      t%P_nbj_pr_apres_semis = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbjgerlim') then
      p(numplt)%P_nbjgerlim = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbjgrain') then
      p(numplt)%P_nbjgrain = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbjmaxapresrecolte') then
      itk(numplt)%P_nbjmaxapresrecolte = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbjmaxapressemis') then
      itk(numplt)%P_nbjmaxapressemis = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbjoursrrversirrig') then
      t%P_nbjoursrrversirrig = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbjres') then
      itk(numplt)%P_nbjres = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbjseuiltempref') then
      itk(numplt)%P_nbjseuiltempref = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nbjtrav') then
      itk(numplt)%P_nbjtrav = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nboite') then
      p(numplt)%P_nboite = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'NH3ref') then
      sta%P_NH3ref = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nh4_min') then
      pg%P_nh4_min = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NH4initf(1)') then
      sc%P_NH4initf(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NH4initf(2)') then
      sc%P_NH4initf(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NH4initf(3)') then
      sc%P_NH4initf(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NH4initf(4)') then
      sc%P_NH4initf(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NH4initf(5)') then
      sc%P_NH4initf(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'nlevlim1') then
      p(numplt)%P_nlevlim1 = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'nlevlim2') then
      p(numplt)%P_nlevlim2 = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'Nmeta') then
      p(numplt)%P_Nmeta = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(1)') then
      itk(numplt)%P_Nminres(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(2)') then
      itk(numplt)%P_Nminres(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(3)') then
      itk(numplt)%P_Nminres(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(4)') then
      itk(numplt)%P_Nminres(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(5)') then
      itk(numplt)%P_Nminres(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(6)') then
      itk(numplt)%P_Nminres(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(7)') then
      itk(numplt)%P_Nminres(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(8)') then
      itk(numplt)%P_Nminres(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(9)') then
      itk(numplt)%P_Nminres(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(10)') then
      itk(numplt)%P_Nminres(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres(11)') then
      itk(numplt)%P_Nminres(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nminres_pature') then
      t%P_Nminres_pature = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NO3initf(1)') then
      soil%P_NO3initf(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NO3initf(2)') then
      soil%P_NO3initf(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NO3initf(3)') then
      soil%P_NO3initf(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NO3initf(4)') then
      soil%P_NO3initf(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'NO3initf(5)') then
      soil%P_NO3initf(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Norg') then
      soil%P_Norg = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Nreserve') then
      p(numplt)%P_Nreserve = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'numsol') then
      soil%P_numsol = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'obstarac') then
      soil%P_obstarac = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ombragetx') then
      sta%P_ombragetx = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'option_engrais_multiple') then
      t%P_option_engrais_multiple = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'option_pature') then
      t%P_option_pature = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'option_thinning') then
      t%P_option_thinning = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'orgeng(1)') then
      pg%P_orgeng(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'orgeng(2)') then
      pg%P_orgeng(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'orgeng(3)') then
      pg%P_orgeng(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'orgeng(4)') then
      pg%P_orgeng(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'orgeng(5)') then
      pg%P_orgeng(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'orgeng(6)') then
      pg%P_orgeng(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'orgeng(7)') then
      pg%P_orgeng(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'orgeng(8)') then
      pg%P_orgeng(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'orientrang') then
      itk(numplt)%P_orientrang = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'parazofmorte') then
      p(numplt)%P_parazofmorte = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'parsurrg') then
      pg%P_parsurrg = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'patm') then
      sta%P_patm = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'penterui') then
      soil%P_penterui = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pentinflores') then
      p(numplt)%P_pentinflores = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pentlaimax') then
      p(numplt)%P_pentlaimax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pentrecouv') then
      p(numplt)%P_pentrecouv = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pertes_restit_ext') then
      t%P_pertes_restit_ext = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pgrainmaxi') then
      p(numplt)%P_pgrainmaxi(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pH') then
      soil%P_pH = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'phiv0') then
      sta%P_phiv0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pHmaxden') then
      pg%P_pHmaxden = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pHmaxnit') then
      pg%P_pHmaxnit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pHmaxvol') then
      pg%P_pHmaxvol = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pHminden') then
      pg%P_pHminden = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pHminnit') then
      pg%P_pHminnit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pHminvol') then
      pg%P_pHminvol = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'phobase') then
      p(numplt)%P_phobase = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'phobasesen') then
      p(numplt)%P_phobasesen = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'phosat') then
      p(numplt)%P_phosat = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pHvols') then
      pg%P_pHvols = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'phyllotherme') then
      p(numplt)%P_phyllotherme = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'plNmin') then
      pg%P_plNmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pluiebat') then
      soil%P_pluiebat = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'pminruis') then
      pg%P_pminruis = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'potgermi') then
      p(numplt)%P_potgermi = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'primingmax') then
      pg%P_primingmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profdenit') then
      soil%P_profdenit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profdrain') then
      soil%P_profdrain = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profhum') then
      soil%P_profhum = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profhumrecolteuse') then
      itk(numplt)%P_profhumrecolteuse = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profhumsemoir') then
      itk(numplt)%P_profhumsemoir = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profimper') then
      soil%P_profimper = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proflabour') then
      pg%P_proflabour = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profmes') then
      itk(numplt)%P_profmes = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profnod') then
      p(numplt)%P_profnod = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(1)') then
      itk(numplt)%P_profres(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(2)') then
      itk(numplt)%P_profres(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(3)') then
      itk(numplt)%P_profres(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(4)') then
      itk(numplt)%P_profres(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(5)') then
      itk(numplt)%P_profres(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(6)') then
      itk(numplt)%P_profres(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(7)') then
      itk(numplt)%P_profres(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(8)') then
      itk(numplt)%P_profres(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(9)') then
      itk(numplt)%P_profres(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(10)') then
      itk(numplt)%P_profres(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profres(11)') then
      itk(numplt)%P_profres(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'profsem') then
      itk(numplt)%P_profsem = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(1)') then
      itk(numplt)%P_proftrav(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(2)') then
      itk(numplt)%P_proftrav(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(3)') then
      itk(numplt)%P_proftrav(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(4)') then
      itk(numplt)%P_proftrav(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(5)') then
      itk(numplt)%P_proftrav(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(6)') then
      itk(numplt)%P_proftrav(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(7)') then
      itk(numplt)%P_proftrav(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(8)') then
      itk(numplt)%P_proftrav(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(9)') then
      itk(numplt)%P_proftrav(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(10)') then
      itk(numplt)%P_proftrav(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftrav(11)') then
      itk(numplt)%P_proftrav(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proftravmin') then
      pg%P_proftravmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'prophumtassrec') then
      pg%P_prophumtassrec = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'prophumtasssem') then
      pg%P_prophumtasssem = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'propjgermin') then
      p(numplt)%P_propjgermin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'proprac') then
      pg%P_proprac = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'psihucc') then
      pg%P_psihucc = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'psihumin') then
      pg%P_psihumin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'psisto') then
      p(numplt)%P_psisto = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'psiturg') then
      p(numplt)%P_psiturg = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'q0') then
      soil%P_q0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'q10') then
      p(numplt)%P_q10 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(1)') then
      pg%P_qmulchdec(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(2)') then
      pg%P_qmulchdec(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(3)') then
      pg%P_qmulchdec(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(4)') then
      pg%P_qmulchdec(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(5)') then
      pg%P_qmulchdec(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(6)') then
      pg%P_qmulchdec(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(7)') then
      pg%P_qmulchdec(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(8)') then
      pg%P_qmulchdec(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(9)') then
      pg%P_qmulchdec(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(10)') then
      pg%P_qmulchdec(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(11)') then
      pg%P_qmulchdec(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(12)') then
      pg%P_qmulchdec(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(13)') then
      pg%P_qmulchdec(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(14)') then
      pg%P_qmulchdec(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(15)') then
      pg%P_qmulchdec(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(16)') then
      pg%P_qmulchdec(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(17)') then
      pg%P_qmulchdec(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(18)') then
      pg%P_qmulchdec(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(19)') then
      pg%P_qmulchdec(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(20)') then
      pg%P_qmulchdec(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchdec(21)') then
      pg%P_qmulchdec(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(1)') then
      pg%P_qmulchruis0(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(2)') then
      pg%P_qmulchruis0(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(3)') then
      pg%P_qmulchruis0(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(4)') then
      pg%P_qmulchruis0(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(5)') then
      pg%P_qmulchruis0(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(6)') then
      pg%P_qmulchruis0(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(7)') then
      pg%P_qmulchruis0(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(8)') then
      pg%P_qmulchruis0(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(9)') then
      pg%P_qmulchruis0(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(10)') then
      pg%P_qmulchruis0(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(11)') then
      pg%P_qmulchruis0(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(12)') then
      pg%P_qmulchruis0(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(13)') then
      pg%P_qmulchruis0(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(14)') then
      pg%P_qmulchruis0(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(15)') then
      pg%P_qmulchruis0(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(16)') then
      pg%P_qmulchruis0(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(17)') then
      pg%P_qmulchruis0(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(18)') then
      pg%P_qmulchruis0(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(19)') then
      pg%P_qmulchruis0(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(20)') then
      pg%P_qmulchruis0(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qmulchruis0(21)') then
      pg%P_qmulchruis0(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'QNplante0') then
      p(numplt)%P_QNplante0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'QNpltminINN') then
      pg%P_QNpltminINN = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(1)') then
      itk(numplt)%P_qres(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(2)') then
      itk(numplt)%P_qres(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(3)') then
      itk(numplt)%P_qres(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(4)') then
      itk(numplt)%P_qres(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(5)') then
      itk(numplt)%P_qres(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(6)') then
      itk(numplt)%P_qres(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(7)') then
      itk(numplt)%P_qres(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(8)') then
      itk(numplt)%P_qres(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(9)') then
      itk(numplt)%P_qres(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(10)') then
      itk(numplt)%P_qres(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'qres(11)') then
      itk(numplt)%P_qres(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Qtot_N') then
      itk(numplt)%P_Qtot_N = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'ra') then
      sta%P_ra = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'rapforme') then
      p(numplt)%P_rapforme = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'rapNmindec') then
      t%P_rapNmindec = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'rapsenturg') then
      p(numplt)%P_rapsenturg = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ratiodenit') then
      pg%P_ratiodenit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ratiodurvieI') then
      p(numplt)%P_ratiodurvieI = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ratiol') then
      itk(numplt)%P_ratiol = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ratiolN') then
      t%P_ratiolN = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'rationit') then
      pg%P_rationit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ratiosen') then
      p(numplt)%P_ratiosen = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'rayon') then
      pg%P_rayon = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'rdrain') then
      pg%P_rdrain = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'remobres') then
      p(numplt)%P_remobres = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'repracpermax') then
      p(numplt)%P_repracpermax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'repracpermin') then
      p(numplt)%P_repracpermin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'repracseumax') then
      p(numplt)%P_repracseumax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'repracseumin') then
      p(numplt)%P_repracseumin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'resperenne0') then
      p(numplt)%P_resperenne0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'resplmax(1)') then
      t%P_resplmax(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'resplmax(2)') then
      t%P_resplmax(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'rsmin') then
      p(numplt)%P_rsmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'rugochisel') then
      itk(numplt)%P_rugochisel = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'rugolabour') then
      itk(numplt)%P_rugolabour = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'ruisolnu') then
      soil%P_ruisolnu = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'scale_tdenitopt') then
      pg%P_scale_tdenitopt = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'scale_tnitopt') then
      pg%P_scale_tnitopt = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'sea') then
      p(numplt)%P_sea = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'sensanox') then
      p(numplt)%P_sensanox = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'sensiphot') then
      p(numplt)%P_sensiphot(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'sensrsec') then
      p(numplt)%P_sensrsec = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'seuilLAIapex(1)') then
      t%P_seuilLAIapex(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'seuilLAIapex(2)') then
      t%P_seuilLAIapex(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'seuilmortalle(1)') then
      t%P_seuilmortalle(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'seuilmortalle(2)') then
      t%P_seuilmortalle(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'seuilreconspeupl(1)') then
      t%P_seuilreconspeupl(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'seuilreconspeupl(2)') then
      t%P_seuilreconspeupl(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'sigmadistalle(1)') then
      t%P_sigmadistalle(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'sigmadistalle(2)') then
      t%P_sigmadistalle(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'slamax') then
      p(numplt)%P_slamax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'slamin') then
      p(numplt)%P_slamin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'spfrmax') then
      p(numplt)%P_spfrmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'spfrmin') then
      p(numplt)%P_spfrmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'splaimax') then
      p(numplt)%P_splaimax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'splaimin') then
      p(numplt)%P_splaimin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stamflax') then
      p(numplt)%P_stamflax(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stdnofno') then
      p(numplt)%P_stdnofno = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stdordebour') then
      p(numplt)%P_stdordebour = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stdrpdes') then
      p(numplt)%P_stdrpdes(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stdrpmat') then
      p(numplt)%P_stdrpmat(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stdrpnou') then
      p(numplt)%P_stdrpnou = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stemflowmax') then
      p(numplt)%P_stemflowmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stflodrp') then
      p(numplt)%P_stflodrp(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stfnofvino') then
      p(numplt)%P_stfnofvino = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stlaxsen') then
      p(numplt)%P_stlaxsen(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stlevamf') then
      p(numplt)%P_stlevamf(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stlevdno') then
      p(numplt)%P_stlevdno = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stlevdrp') then
      p(numplt)%P_stlevdrp(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stpltger') then
      p(numplt)%P_stpltger = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stressdev') then
      p(numplt)%P_stressdev = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'stsenlan') then
      p(numplt)%P_stsenlan(itk(numplt)%P_variete) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'sucrerec') then
      itk(numplt)%P_sucrerec = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'surfapex(1)') then
      t%P_surfapex(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'surfapex(2)') then
      t%P_surfapex(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'surfouvre1') then
      itk(numplt)%P_surfouvre1 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'surfouvre2') then
      itk(numplt)%P_surfouvre2 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'surfouvre3') then
      itk(numplt)%P_surfouvre3 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'swfacmin') then
      t%P_swfacmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tauxrecouvkmax') then
      p(numplt)%P_tauxrecouvkmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tauxrecouvmax') then
      p(numplt)%P_tauxrecouvmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tcmax') then
      p(numplt)%P_tcmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tcmin') then
      p(numplt)%P_tcmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tcxstop') then
      p(numplt)%P_tcxstop = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tdebgel') then
      p(numplt)%P_tdebgel = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tdenitopt_gauss') then
      pg%P_tdenitopt_gauss = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tdmax') then
      p(numplt)%P_tdmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tdmaxdeb') then
      p(numplt)%P_tdmaxdeb = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tdmin') then
      p(numplt)%P_tdmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tdmindeb') then
      p(numplt)%P_tdmindeb = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'temax') then
      p(numplt)%P_temax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'temin') then
      p(numplt)%P_temin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempdeshyd') then
      p(numplt)%P_tempdeshyd = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(1)') then
      itk(numplt)%P_tempfauche(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(2)') then
      itk(numplt)%P_tempfauche(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(3)') then
      itk(numplt)%P_tempfauche(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(4)') then
      itk(numplt)%P_tempfauche(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(5)') then
      itk(numplt)%P_tempfauche(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(6)') then
      itk(numplt)%P_tempfauche(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(7)') then
      itk(numplt)%P_tempfauche(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(8)') then
      itk(numplt)%P_tempfauche(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(9)') then
      itk(numplt)%P_tempfauche(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(10)') then
      itk(numplt)%P_tempfauche(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(11)') then
      itk(numplt)%P_tempfauche(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(12)') then
      itk(numplt)%P_tempfauche(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(13)') then
      itk(numplt)%P_tempfauche(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(14)') then
      itk(numplt)%P_tempfauche(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(15)') then
      itk(numplt)%P_tempfauche(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(16)') then
      itk(numplt)%P_tempfauche(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(17)') then
      itk(numplt)%P_tempfauche(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(18)') then
      itk(numplt)%P_tempfauche(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(19)') then
      itk(numplt)%P_tempfauche(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempfauche(20)') then
      itk(numplt)%P_tempfauche(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempnod1') then
      p(numplt)%P_tempnod1 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempnod2') then
      p(numplt)%P_tempnod2 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempnod3') then
      p(numplt)%P_tempnod3 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tempnod4') then
      p(numplt)%P_tempnod4 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'teopt') then
      p(numplt)%P_teopt = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'teoptbis') then
      p(numplt)%P_teoptbis = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tfroid') then
      p(numplt)%P_tfroid = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tgelflo10') then
      p(numplt)%P_tgelflo10 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tgelflo90') then
      p(numplt)%P_tgelflo90 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tgeljuv10') then
      p(numplt)%P_tgeljuv10 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tgeljuv90') then
      p(numplt)%P_tgeljuv90 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tgellev10') then
      p(numplt)%P_tgellev10 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tgellev90') then
      p(numplt)%P_tgellev90 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tgelveg10') then
      p(numplt)%P_tgelveg10 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tgelveg90') then
      p(numplt)%P_tgelveg90 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tgmin') then
      p(numplt)%P_tgmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tigefeuil') then
      p(numplt)%P_tigefeuil = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tigefeuilcoupe(1)') then
      t%P_tigefeuilcoupe(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tigefeuilcoupe(2)') then
      t%P_tigefeuilcoupe(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tletale') then
      p(numplt)%P_tletale = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tmaxremp') then
      p(numplt)%P_tmaxremp = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tminremp') then
      p(numplt)%P_tminremp = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tnitmax') then
      pg%P_tnitmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tnitmin') then
      pg%P_tnitmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tnitopt') then
      pg%P_tnitopt = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tnitopt_gauss') then
      pg%P_tnitopt_gauss = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tnitopt2') then
      pg%P_tnitopt2 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'transplastic') then
      itk(numplt)%P_transplastic = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'trefh') then
      pg%P_trefh = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'trefr') then
      pg%P_trefr = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'tustressmin') then
      p(numplt)%P_tustressmin = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'typecailloux(1)') then
      soil%P_typecailloux(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'typecailloux(2)') then
      soil%P_typecailloux(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'typecailloux(3)') then
      soil%P_typecailloux(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'typecailloux(4)') then
      soil%P_typecailloux(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'typecailloux(5)') then
      soil%P_typecailloux(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'udlaimax') then
      p(numplt)%P_udlaimax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(1)') then
      itk(numplt)%P_upvttapI(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(2)') then
      itk(numplt)%P_upvttapI(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(3)') then
      itk(numplt)%P_upvttapI(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(4)') then
      itk(numplt)%P_upvttapI(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(5)') then
      itk(numplt)%P_upvttapI(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(6)') then
      itk(numplt)%P_upvttapI(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(7)') then
      itk(numplt)%P_upvttapI(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(8)') then
      itk(numplt)%P_upvttapI(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(9)') then
      itk(numplt)%P_upvttapI(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(10)') then
      itk(numplt)%P_upvttapI(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(11)') then
      itk(numplt)%P_upvttapI(11) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(12)') then
      itk(numplt)%P_upvttapI(12) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(13)') then
      itk(numplt)%P_upvttapI(13) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(14)') then
      itk(numplt)%P_upvttapI(14) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(15)') then
      itk(numplt)%P_upvttapI(15) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(16)') then
      itk(numplt)%P_upvttapI(16) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(17)') then
      itk(numplt)%P_upvttapI(17) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(18)') then
      itk(numplt)%P_upvttapI(18) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(19)') then
      itk(numplt)%P_upvttapI(19) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(20)') then
      itk(numplt)%P_upvttapI(20) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(21)') then
      itk(numplt)%P_upvttapI(21) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(22)') then
      itk(numplt)%P_upvttapI(22) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(23)') then
      itk(numplt)%P_upvttapI(23) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(24)') then
      itk(numplt)%P_upvttapI(24) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(25)') then
      itk(numplt)%P_upvttapI(25) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(26)') then
      itk(numplt)%P_upvttapI(26) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(27)') then
      itk(numplt)%P_upvttapI(27) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(28)') then
      itk(numplt)%P_upvttapI(28) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(29)') then
      itk(numplt)%P_upvttapI(29) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapI(30)') then
      itk(numplt)%P_upvttapI(30) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(1)') then
      itk(numplt)%P_upvttapN(1) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(2)') then
      itk(numplt)%P_upvttapN(2) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(3)') then
      itk(numplt)%P_upvttapN(3) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(4)') then
      itk(numplt)%P_upvttapN(4) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(5)') then
      itk(numplt)%P_upvttapN(5) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(6)') then
      itk(numplt)%P_upvttapN(6) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(7)') then
      itk(numplt)%P_upvttapN(7) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(8)') then
      itk(numplt)%P_upvttapN(8) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(9)') then
      itk(numplt)%P_upvttapN(9) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(10)') then
      itk(numplt)%P_upvttapN(10) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(11)') then
      itk(numplt)%P_upvttapN(11) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(12)') then
      itk(numplt)%P_upvttapN(12) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(13)') then
      itk(numplt)%P_upvttapN(13) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(14)') then
      itk(numplt)%P_upvttapN(14) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(15)') then
      itk(numplt)%P_upvttapN(15) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(16)') then
      itk(numplt)%P_upvttapN(16) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(17)') then
      itk(numplt)%P_upvttapN(17) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(18)') then
      itk(numplt)%P_upvttapN(18) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(19)') then
      itk(numplt)%P_upvttapN(19) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'upvttapN(20)') then
      itk(numplt)%P_upvttapN(20) = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'Vabs2') then
      pg%P_Vabs2 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'variete') then
      itk(numplt)%P_variete = int(valparopt(i))
      CYCLE
    endif
    if (nompar(i) == 'vigueurbat') then
      p(numplt)%P_vigueurbat = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vitirazo') then
      p(numplt)%P_vitirazo = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vitircarb') then
      p(numplt)%P_vitircarb = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vitircarbT') then
      p(numplt)%P_vitircarbT = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vitno') then
      p(numplt)%P_vitno = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vitprophuile') then
      p(numplt)%P_vitprophuile = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vitpropsucre') then
      p(numplt)%P_vitpropsucre = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vitreconspeupl(1)') then
      t%P_vitreconspeupl(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vitreconspeupl(2)') then
      t%P_vitreconspeupl(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vlaimax') then
      p(numplt)%P_vlaimax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Vmax1') then
      p(numplt)%P_Vmax1 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Vmax2') then
      p(numplt)%P_Vmax2 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vnitmax') then
      pg%P_vnitmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'voleng(1)') then
      pg%P_voleng(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'voleng(2)') then
      pg%P_voleng(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'voleng(3)') then
      pg%P_voleng(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'voleng(4)') then
      pg%P_voleng(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'voleng(5)') then
      pg%P_voleng(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'voleng(6)') then
      pg%P_voleng(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'voleng(7)') then
      pg%P_voleng(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'voleng(8)') then
      pg%P_voleng(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'vpotdenit') then
      soil%P_vpotdenit = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'wfpsc') then
      pg%P_wfpsc = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Wh') then
      pg%P_Wh = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'Xorgmax') then
      pg%P_Xorgmax = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'y0msrac') then
      pg%P_y0msrac = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(1)') then
      pg%P_yres(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(2)') then
      pg%P_yres(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(3)') then
      pg%P_yres(3) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(4)') then
      pg%P_yres(4) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(5)') then
      pg%P_yres(5) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(6)') then
      pg%P_yres(6) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(7)') then
      pg%P_yres(7) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(8)') then
      pg%P_yres(8) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(9)') then
      pg%P_yres(9) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(10)') then
      pg%P_yres(10) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(11)') then
      pg%P_yres(11) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(12)') then
      pg%P_yres(12) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(13)') then
      pg%P_yres(13) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(14)') then
      pg%P_yres(14) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(15)') then
      pg%P_yres(15) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(16)') then
      pg%P_yres(16) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(17)') then
      pg%P_yres(17) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(18)') then
      pg%P_yres(18) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(19)') then
      pg%P_yres(19) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(20)') then
      pg%P_yres(20) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'yres(21)') then
      pg%P_yres(21) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'z0solnu') then
      soil%P_z0solnu = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'zesx') then
      soil%P_zesx = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'zlabour') then
      p(numplt)%P_zlabour = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'zpente') then
      p(numplt)%P_zpente = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'zprlim') then
      p(numplt)%P_zprlim = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'zr') then
      sta%P_zr = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'zrac0') then
      p(numplt)%P_zrac0 = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'zracplantule') then
      p(numplt)%P_zracplantule = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautK1') then
      t%P_hautK(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautK2') then
      t%P_hautK(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautA1') then
      t%P_hautA(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hautA2') then
      t%P_hautA(2) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'hauteur_threshold') then
      t%P_hauteur_threshold = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'elongation1') then
      t%P_elongation(1) = valparopt(i)
      CYCLE
    endif
    if (nompar(i) == 'elongation2') then
      t%P_elongation(2) = valparopt(i)
      CYCLE
    endif

      call EnvoyerMsgHistorique('')
      write(tmp,*) nom, ': unknown parameter name (check case sensitivity)'
      call EnvoyerMsgHistorique(tmp)
    enddo
return
end subroutine Lecture_Optimisation
