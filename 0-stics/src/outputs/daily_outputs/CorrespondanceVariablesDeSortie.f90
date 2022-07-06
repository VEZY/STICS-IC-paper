 ! **************************************************************
!    Getting variables values for a table of variable names
!
! **************************************************************
!
!  MODIFICATIONS (last commit)
!    $Date: 2017-09-26 10:01:35 +0200 (mar., 26 sept. 2017) $
!    $Author: plecharpent $
!    $Revision: 1523 $
!
!***************************************************************

 subroutine CorrespondanceVariablesDeSorties(sc, p, soil, c, sta, nbVars, tabNomVars, tabValVars)

    USE Stics
    USE Plante
    USE Sol
    USE Climat
    USE Station

    implicit none

    type(Stics_Communs_), intent(IN)      :: sc
    type(Plante_), intent(IN)      :: p
    type(Sol_), intent(IN)      :: soil
    type(Climat_), intent(IN)      :: c
    type(Station_), intent(IN)      :: sta

    integer, intent(IN)      :: nbVars
    character(len=30), intent(IN)      :: tabNomVars(nbVars) ! TODO: taille fixe ou dynamique ?
    real, intent(OUT)     :: tabValVars(nbVars) ! TODO: taille fixe ou dynamique ?

    character(30) :: nom
    character(100) :: tmp
    integer           :: i
    integer           :: n
    integer           :: aoas
    integer           :: ao
    integer           :: as
    !integer           :: codebbch_entier

    ! simplest use
    aoas = sc%aoas
    ao = sc%ao
    as = sc%as
    n = sc%n

    B1: do i = 1, nbVars

       nom = tabNomVars(i)

       if (tabNomVars(i) == 'abso(n)') then
          tabValVars(i) = p%abso(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'age_prairie') then
          tabValVars(i) = float(p%age_prairie)
          CYCLE
       end if
       if (tabNomVars(i) == 'airg(n)') then
          tabValVars(i) = sc%airg(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'albedolai') then
          tabValVars(i) = sc%albedolai
          CYCLE
       end if
       if (tabNomVars(i) == 'allocfruit') then
          tabValVars(i) = p%allocfruit(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'ammomes') then
          tabValVars(i) = sc%ammomes
          CYCLE
       end if
       if (tabNomVars(i) == 'amptcultmat') then
          tabValVars(i) = c%amptcultmat
          CYCLE
       end if
       if (tabNomVars(i) == 'anit(n)') then
          tabValVars(i) = sc%anit(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'anit_engrais(n)') then
          tabValVars(i) = sc%anit_engrais(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'anit_uree(n)') then
          tabValVars(i) = sc%anit_uree(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'anoxmoy') then
          tabValVars(i) = p%anoxmoy
          CYCLE
       end if
       if (tabNomVars(i) == 'AZamm(1)') then
          tabValVars(i) = sc%AZamm(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'AZamm(2)') then
          tabValVars(i) = sc%AZamm(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'AZamm(3)') then
          tabValVars(i) = sc%AZamm(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'AZamm(4)') then
          tabValVars(i) = sc%AZamm(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'AZamm(5)') then
          tabValVars(i) = sc%AZamm(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'azlesd') then
          tabValVars(i) = soil%azlesd
          CYCLE
       end if
       if (tabNomVars(i) == 'AZnit(1)') then
          tabValVars(i) = soil%AZnit(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'AZnit(2)') then
          tabValVars(i) = soil%AZnit(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'AZnit(3)') then
          tabValVars(i) = soil%AZnit(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'AZnit(4)') then
          tabValVars(i) = soil%AZnit(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'AZnit(5)') then
          tabValVars(i) = soil%AZnit(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'azomes') then
          tabValVars(i) = sc%azomes
          CYCLE
       end if
       if (tabNomVars(i) == 'bouchon') then
          tabValVars(i) = sc%bouchon
          CYCLE
       end if
       if (tabNomVars(i) == 'Cb') then
          tabValVars(i) = sc%Cb
          CYCLE
       end if
       if (tabNomVars(i) == 'Cbmulch') then
          tabValVars(i) = sc%Cbmulch
          CYCLE
       end if
       if (tabNomVars(i) == 'cdemande') then
          tabValVars(i) = p%cdemande
          CYCLE
       end if
       if (tabNomVars(i) == 'cEdirect') then
          tabValVars(i) = sc%cEdirect
          CYCLE
       end if
       if (tabNomVars(i) == 'cEdirecttout') then
          tabValVars(i) = sc%cEdirecttout
          CYCLE
       end if
       if (tabNomVars(i) == 'cep') then
          tabValVars(i) = p%cep
          CYCLE
       end if
       if (tabNomVars(i) == 'ces') then
          tabValVars(i) = p%ces
          CYCLE
       end if
       if (tabNomVars(i) == 'cestout') then
          tabValVars(i) = sc%cestout
          CYCLE
       end if
       if (tabNomVars(i) == 'cet') then
          tabValVars(i) = p%cet
          CYCLE
       end if
       if (tabNomVars(i) == 'cet_from_lev') then
          tabValVars(i) = sc%cet_from_lev
          CYCLE
       end if
       if (tabNomVars(i) == 'cetm') then
          tabValVars(i) = p%cetm
          CYCLE
       end if
       if (tabNomVars(i) == 'Cetmtout') then
          tabValVars(i) = c%Cetmtout
          CYCLE
       end if
       if (tabNomVars(i) == 'cetp') then
          tabValVars(i) = p%cetp
          CYCLE
       end if
       if (tabNomVars(i) == 'chargefruit') then
          tabValVars(i) = p%chargefruit
          CYCLE
       end if
       if (tabNomVars(i) == 'Chuma') then
          tabValVars(i) = sc%Chuma
          CYCLE
       end if
       if (tabNomVars(i) == 'Chumi') then
          tabValVars(i) = sc%Chumi
          CYCLE
       end if
       if (tabNomVars(i) == 'Chumt') then
          tabValVars(i) = sc%Chumt
          CYCLE
       end if
       if (tabNomVars(i) == 'cintermulch') then
          tabValVars(i) = sc%cintermulch
          CYCLE
       end if
       if (tabNomVars(i) == 'cinterpluie') then
          tabValVars(i) = p%cinterpluie
          CYCLE
       end if
       if (tabNomVars(i) == 'Cmulch') then
          tabValVars(i) = sc%Cmulch
          CYCLE
       end if
       if (tabNomVars(i) == 'Cmulchdec') then
          tabValVars(i) = sc%Cmulchdec
          CYCLE
       end if
       if (tabNomVars(i) == 'Cmulchnd') then
          tabValVars(i) = sc%Cmulchnd
          CYCLE
       end if
       if (tabNomVars(i) == 'CNgrain') then
          tabValVars(i) = p%CNgrain(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(1)') then
          tabValVars(i) = sc%Cnondec(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(10)') then
          tabValVars(i) = sc%Cnondec(10)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(2)') then
          tabValVars(i) = sc%Cnondec(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(3)') then
          tabValVars(i) = sc%Cnondec(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(4)') then
          tabValVars(i) = sc%Cnondec(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(5)') then
          tabValVars(i) = sc%Cnondec(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(6)') then
          tabValVars(i) = sc%Cnondec(6)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(7)') then
          tabValVars(i) = sc%Cnondec(7)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(8)') then
          tabValVars(i) = sc%Cnondec(8)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cnondec(9)') then
          tabValVars(i) = sc%Cnondec(9)
          CYCLE
       end if
       if (tabNomVars(i) == 'CNplante') then
          tabValVars(i) = p%CNplante(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'co2(n)') then
          tabValVars(i) = c%co2(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'CO2hum') then
          tabValVars(i) = sc%CO2hum
          CYCLE
       end if
       if (tabNomVars(i) == 'CO2res') then
          tabValVars(i) = sc%CO2res
          CYCLE
       end if
       if (tabNomVars(i) == 'CO2sol') then
          tabValVars(i) = sc%CO2sol
          CYCLE
       end if
       if (tabNomVars(i) == 'codebbch_output') then
          tabValVars(i) = float(p%codebbch_output)
          CYCLE
       end if
       if (tabNomVars(i) == 'concNO3les') then
          tabValVars(i) = soil%concNO3les
          CYCLE
       end if
       if (tabNomVars(i) == 'concNO3sol(1)') then
          tabValVars(i) = sc%concNO3sol(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'concNO3sol(2)') then
          tabValVars(i) = sc%concNO3sol(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'concNO3sol(3)') then
          tabValVars(i) = sc%concNO3sol(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'concNO3sol(4)') then
          tabValVars(i) = sc%concNO3sol(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'concNO3sol(5)') then
          tabValVars(i) = sc%concNO3sol(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'condenit') then
          tabValVars(i) = soil%condenit
          CYCLE
       end if
       if (tabNomVars(i) == 'couvermulch') then
          tabValVars(i) = sc%couvermulch
          CYCLE
       end if
       if (tabNomVars(i) == 'cpluie') then
          tabValVars(i) = sc%cpluie
          CYCLE
       end if
       if (tabNomVars(i) == 'cprecip') then
          tabValVars(i) = p%cprecip
          CYCLE
       end if
       if (tabNomVars(i) == 'cpreciptout') then
          tabValVars(i) = sc%cpreciptout
          CYCLE
       end if
       if (tabNomVars(i) == 'Cr') then
          tabValVars(i) = sc%Cr
          CYCLE
       end if
       if (tabNomVars(i) == 'Crac') then
          tabValVars(i) = p%Crac
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(1)') then
          tabValVars(i) = sc%Cresiduprofil(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(10)') then
          tabValVars(i) = sc%Cresiduprofil(10)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(2)') then
          tabValVars(i) = sc%Cresiduprofil(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(3)') then
          tabValVars(i) = sc%Cresiduprofil(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(4)') then
          tabValVars(i) = sc%Cresiduprofil(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(5)') then
          tabValVars(i) = sc%Cresiduprofil(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(6)') then
          tabValVars(i) = sc%Cresiduprofil(6)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(7)') then
          tabValVars(i) = sc%Cresiduprofil(7)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(8)') then
          tabValVars(i) = sc%Cresiduprofil(8)
          CYCLE
       end if
       if (tabNomVars(i) == 'Cresiduprofil(9)') then
          tabValVars(i) = sc%Cresiduprofil(9)
          CYCLE
       end if
       if (tabNomVars(i) == 'crg') then
          tabValVars(i) = p%crg
          CYCLE
       end if
       if (tabNomVars(i) == 'crgtout') then
          tabValVars(i) = c%crgtout
          CYCLE
       end if
       if (tabNomVars(i) == 'CsurNres_pature') then
          tabValVars(i) = sc%CsurNres_pature
          CYCLE
       end if
       if (tabNomVars(i) == 'ctairtout') then
          tabValVars(i) = c%ctairtout
          CYCLE
       end if
       if (tabNomVars(i) == 'ctcult') then
          tabValVars(i) = p%ctcult
          CYCLE
       end if
       if (tabNomVars(i) == 'ctculttout') then
          tabValVars(i) = c%ctculttout
          CYCLE
       end if
       if (tabNomVars(i) == 'ctetptout') then
          tabValVars(i) = c%ctetptout
          CYCLE
       end if
       if (tabNomVars(i) == 'ctmoy') then
          tabValVars(i) = p%ctmoy
          CYCLE
       end if
       if (tabNomVars(i) == 'Ctousresidusprofil') then
          tabValVars(i) = sc%Ctousresidusprofil
          CYCLE
       end if
       if (tabNomVars(i) == 'cum_et0') then
          tabValVars(i) = p%cum_et0
          CYCLE
       end if
       if (tabNomVars(i) == 'cum_et0_from_lev') then
          tabValVars(i) = sc%cum_et0_from_lev
          CYCLE
       end if
       if (tabNomVars(i) == 'cum_immob') then
          tabValVars(i) = sc%cum_immob
          CYCLE
       end if
       if (tabNomVars(i) == 'cumlracz') then
          tabValVars(i) = p%cumlracz
          CYCLE
       end if
       if (tabNomVars(i) == 'cumraint') then
          tabValVars(i) = p%cumraint
          CYCLE
       end if
       if (tabNomVars(i) == 'cumrg') then
          tabValVars(i) = p%cumrg
          CYCLE
       end if
       if (tabNomVars(i) == 'cumvminh') then
          tabValVars(i) = soil%cumvminh
          CYCLE
       end if
       if (tabNomVars(i) == 'cumvminr') then
          tabValVars(i) = soil%cumvminr
          CYCLE
       end if
       if (tabNomVars(i) == 'da(1)') then
          tabValVars(i) = soil%da(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'da(2)') then
          tabValVars(i) = soil%da(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'day_after_sowing') then
          tabValVars(i) = float(sc%day_after_sowing)
          CYCLE
       end if
       if (tabNomVars(i) == 'day_cut') then
          tabValVars(i) = float(p%day_cut)
          CYCLE
       end if
       if (tabNomVars(i) == 'deltai(n)') then
          tabValVars(i) = p%deltai(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'deltaz') then
          tabValVars(i) = p%deltaz
          CYCLE
       end if
       if (tabNomVars(i) == 'demande') then
          tabValVars(i) = p%demande(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'densite') then
          tabValVars(i) = p%densite
          CYCLE
       end if
       if (tabNomVars(i) == 'densiteequiv') then
          tabValVars(i) = p%densiteequiv
          CYCLE
       end if
       if (tabNomVars(i) == 'dfol') then
          tabValVars(i) = p%dfol
          CYCLE
       end if
       if (tabNomVars(i) == 'diftemp1intercoupe') then
          tabValVars(i) = p%diftemp1intercoupe
          CYCLE
       end if
       if (tabNomVars(i) == 'diftemp2intercoupe') then
          tabValVars(i) = p%diftemp2intercoupe
          CYCLE
       end if
       if (tabNomVars(i) == 'dltags') then
          tabValVars(i) = p%dltags(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'dltaisen') then
          tabValVars(i) = p%dltaisen(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'dltams(n)') then
          tabValVars(i) = p%dltams(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'dltamsen') then
          tabValVars(i) = p%dltamsen(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'dltaremobil') then
          tabValVars(i) = p%dltaremobil(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'dltmsrac_plante') then
          tabValVars(i) = p%dltmsrac_plante
          CYCLE
       end if
       if (tabNomVars(i) == 'drain') then
          tabValVars(i) = sc%drain
          CYCLE
       end if
       if (tabNomVars(i) == 'drain_from_plt') then
          tabValVars(i) = sc%drain_from_plt
          CYCLE
       end if
       if (tabNomVars(i) == 'drain_from_lev') then
          tabValVars(i) = sc%drain_from_lev
          CYCLE
       end if
       if (tabNomVars(i) == 'drat') then
          tabValVars(i) = sc%drat
          CYCLE
       end if
       if (tabNomVars(i) == 'drlsenmortalle') then
          tabValVars(i) = p%drlsenmortalle
          CYCLE
       end if
       if (tabNomVars(i) == 'dtj(n)') then
          tabValVars(i) = p%dtj(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'dureehumec') then
          tabValVars(i) = c%dureehumec
          CYCLE
       end if
       if (tabNomVars(i) == 'dureeRH') then
          tabValVars(i) = c%dureeRH
          CYCLE
       end if
       if (tabNomVars(i) == 'durvie(n)') then
          tabValVars(i) = p%durvie(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'ebmax') then
          tabValVars(i) = p%ebmax
          CYCLE
       end if
       if (tabNomVars(i) == 'ebmax_gr') then
          tabValVars(i) = p%ebmax_gr
          CYCLE
       end if
       if (tabNomVars(i) == 'eai') then
          tabValVars(i) = p%eai(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'Edirect') then
          tabValVars(i) = sc%Edirect
          CYCLE
       end if
       if (tabNomVars(i) == 'ef_n_w_height') then
          tabValVars(i) = p%ef_n_w_height(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'efda') then
          tabValVars(i) = p%efda
          CYCLE
       end if
       if (tabNomVars(i) == 'efdensite') then
          tabValVars(i) = p%efdensite
          CYCLE
       end if
       if (tabNomVars(i) == 'ef_elongation') then
          tabValVars(i) = p%ef_elongation
          CYCLE
       end if
       if (tabNomVars(i) == 'efdensite_rac') then
          tabValVars(i) = p%efdensite_rac
          CYCLE
       end if
       if (tabNomVars(i) == 'efNrac_mean') then
          tabValVars(i) = p%efNrac_mean
          CYCLE
       end if
       if (tabNomVars(i) == 'em_N2O') then
          tabValVars(i) = sc%em_N2O
          CYCLE
       end if
       if (tabNomVars(i) == 'em_N2Oden') then
          tabValVars(i) = sc%em_N2Oden
          CYCLE
       end if
       if (tabNomVars(i) == 'em_N2Onit') then
          tabValVars(i) = sc%em_N2Onit
          CYCLE
       end if
       if (tabNomVars(i) == 'emd') then
          tabValVars(i) = p%emd
          CYCLE
       end if
       if (tabNomVars(i) == 'emulch') then
          tabValVars(i) = sc%emulch
          CYCLE
       end if
       if (tabNomVars(i) == 'eo') then
          tabValVars(i) = sc%eo
          CYCLE
       end if
       if (tabNomVars(i) == 'eop') then
          tabValVars(i) = p%eop(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'eos') then
          tabValVars(i) = sc%eos
          CYCLE
       end if
       if (tabNomVars(i) == 'ep') then
          tabValVars(i) = p%ep(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'epc_recal(1)') then
          tabValVars(i) = soil%epc_recal(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'epc_recal(2)') then
          tabValVars(i) = soil%epc_recal(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'epc_recal(3)') then
          tabValVars(i) = soil%epc_recal(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'epc_recal(4)') then
          tabValVars(i) = soil%epc_recal(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'epc_recal(5)') then
          tabValVars(i) = soil%epc_recal(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'epsib') then
          tabValVars(i) = p%epsib(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'esol') then
          tabValVars(i) = sc%esol
          CYCLE
       end if
       if (tabNomVars(i) == 'et') then
          tabValVars(i) = sc%et
          CYCLE
       end if
       if (tabNomVars(i) == 'et0') then
          tabValVars(i) = p%et0
          CYCLE
       end if
       if (tabNomVars(i) == 'etm') then
          tabValVars(i) = sc%etm
          CYCLE
       end if
       if (tabNomVars(i) == 'etpp(n)') then
          tabValVars(i) = c%etpp(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'exces(1)') then
          tabValVars(i) = sc%exces(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'exces(2)') then
          tabValVars(i) = sc%exces(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'exces(3)') then
          tabValVars(i) = sc%exces(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'exces(4)') then
          tabValVars(i) = sc%exces(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'exces(5)') then
          tabValVars(i) = sc%exces(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'exobiom') then
          tabValVars(i) = p%exobiom
          CYCLE
       end if
       if (tabNomVars(i) == 'exofac') then
          tabValVars(i) = p%exofac
          CYCLE
       end if
       if (tabNomVars(i) == 'exofac1moy') then
          tabValVars(i) = p%exofac1moy
          CYCLE
       end if
       if (tabNomVars(i) == 'exofac2moy') then
          tabValVars(i) = p%exofac2moy
          CYCLE
       end if
       if (tabNomVars(i) == 'exolai') then
          tabValVars(i) = p%exolai
          CYCLE
       end if
       if (tabNomVars(i) == 'fapar') then
          tabValVars(i) = p%fapar(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'fco2') then
          tabValVars(i) = p%fco2
          CYCLE
       end if
       if (tabNomVars(i) == 'fco2s') then
          tabValVars(i) = p%fco2s
          CYCLE
       end if
       if (tabNomVars(i) == 'fgelflo') then
          tabValVars(i) = p%fgelflo
          CYCLE
       end if
       if (tabNomVars(i) == 'fixmaxvar') then
          tabValVars(i) = p%fixmaxvar(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'fixpot') then
          tabValVars(i) = p%fixpot(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'fixreel') then
          tabValVars(i) = p%fixreel(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'flurac') then
          tabValVars(i) = p%flurac
          CYCLE
       end if
       if (tabNomVars(i) == 'flusol') then
          tabValVars(i) = p%flusol
          CYCLE
       end if
       if (tabNomVars(i) == 'fpari') then
          tabValVars(i) = p%fpari
          CYCLE
       end if
       if (tabNomVars(i) == 'fpari_gr') then
          tabValVars(i) = p%fpari_gr
          CYCLE
       end if
       if (tabNomVars(i) == 'fpft') then
          tabValVars(i) = p%fpft(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'fpv(n)') then
          tabValVars(i) = p%fpv(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'FsNH3') then
          tabValVars(i) = sc%FsNH3
          CYCLE
       end if
       if (tabNomVars(i) == 'fstressgel') then
          tabValVars(i) = p%fstressgel
          CYCLE
       end if
       if (tabNomVars(i) == 'ftemp') then
          tabValVars(i) = p%ftemp
          CYCLE
       end if
       if (tabNomVars(i) == 'fxa') then
          tabValVars(i) = sc%fxa
          CYCLE
       end if
       if (tabNomVars(i) == 'fxn') then
          tabValVars(i) = sc%fxn
          CYCLE
       end if
       if (tabNomVars(i) == 'fxt') then
          tabValVars(i) = sc%fxt
          CYCLE
       end if
       if (tabNomVars(i) == 'fxw') then
          tabValVars(i) = sc%fxw
          CYCLE
       end if
       if (tabNomVars(i) == 'gel1') then
          tabValVars(i) = p%gel1
          CYCLE
       end if
       if (tabNomVars(i) == 'gel1_percent') then
          tabValVars(i) = p%gel1_percent
          CYCLE
       end if
       if (tabNomVars(i) == 'gel2') then
          tabValVars(i) = p%gel2
          CYCLE
       end if
       if (tabNomVars(i) == 'gel2_percent') then
          tabValVars(i) = p%gel2_percent
          CYCLE
       end if
       if (tabNomVars(i) == 'gel3') then
          tabValVars(i) = p%gel3
          CYCLE
       end if
       if (tabNomVars(i) == 'gel3_percent') then
          tabValVars(i) = p%gel3_percent
          CYCLE
       end if
       if (tabNomVars(i) == 'H2Orec') then
          tabValVars(i) = p%H2Orec(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'H2Orec_percent') then
          tabValVars(i) = p%H2Orec_percent(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'hauteur') then
          tabValVars(i) = p%hauteur(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'potential_height') then
          tabValVars(i) = p%potential_height(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'dominant') then
          if (p%estDominante) then
             tabValVars(i) = 1.
          else
             tabValVars(i) = 0.
          end if
          CYCLE
       end if
       if (tabNomVars(i) == 'varrapforme') then
          tabValVars(i) = p%varrapforme(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'Hmax') then
          tabValVars(i) = soil%Hmax
          CYCLE
       end if
       if (tabNomVars(i) == 'Hnappe') then
          tabValVars(i) = soil%Hnappe
          CYCLE
       end if
       if (tabNomVars(i) == 'Hpb') then
          tabValVars(i) = soil%Hpb
          CYCLE
       end if
       if (tabNomVars(i) == 'Hph') then
          tabValVars(i) = soil%Hph
          CYCLE
       end if
       if (tabNomVars(i) == 'HR(1)') then
          tabValVars(i) = sc%HR(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'HR(2)') then
          tabValVars(i) = sc%HR(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'HR(3)') then
          tabValVars(i) = sc%HR(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'HR(4)') then
          tabValVars(i) = sc%HR(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'HR(5)') then
          tabValVars(i) = sc%HR(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'HR_vol_1_10') then
          tabValVars(i) = sc%HR_vol_1_10
          CYCLE
       end if
       if (tabNomVars(i) == 'HR_vol_1_30') then
          tabValVars(i) = sc%HR_vol_1_30
          CYCLE
       end if
       if (tabNomVars(i) == 'HR_vol_121_150') then
          tabValVars(i) = sc%HR_vol_121_150
          CYCLE
       end if
       if (tabNomVars(i) == 'HR_vol_151_180') then
          tabValVars(i) = sc%HR_vol_151_180
          CYCLE
       end if
       if (tabNomVars(i) == 'HR_vol_31_60') then
          tabValVars(i) = sc%HR_vol_31_60
          CYCLE
       end if
       if (tabNomVars(i) == 'HR_vol_61_90') then
          tabValVars(i) = sc%HR_vol_61_90
          CYCLE
       end if
       if (tabNomVars(i) == 'HR_vol_91_120') then
          tabValVars(i) = sc%HR_vol_91_120
          CYCLE
       end if
       if (tabNomVars(i) == 'huile') then
          tabValVars(i) = p%huile(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'huile_percent') then
          tabValVars(i) = p%huile_percent(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'humair') then
          tabValVars(i) = c%humair
          CYCLE
       end if
       if (tabNomVars(i) == 'humair_percent') then
          tabValVars(i) = c%humair_percent
          CYCLE
       end if
       if (tabNomVars(i) == 'humidite') then
          tabValVars(i) = sc%humidite
          CYCLE
       end if
       if (tabNomVars(i) == 'humidite_percent') then
          tabValVars(i) = sc%humidite_percent
          CYCLE
       end if
       if (tabNomVars(i) == 'humirac_mean') then
          tabValVars(i) = p%humirac_mean
          CYCLE
       end if
       if (tabNomVars(i) == 'iamfs') then
          tabValVars(i) = float(p%iamfs)
          CYCLE
       end if
       if (tabNomVars(i) == 'idebdess') then
          tabValVars(i) = float(p%idebdess)
          CYCLE
       end if
       if (tabNomVars(i) == 'idebdorms') then
          tabValVars(i) = float(p%idebdorms)
          CYCLE
       end if
       if (tabNomVars(i) == 'idrps') then
          tabValVars(i) = float(p%idrps)
          CYCLE
       end if
       if (tabNomVars(i) == 'ifindorms') then
          tabValVars(i) = float(p%ifindorms)
          CYCLE
       end if
       if (tabNomVars(i) == 'iflos') then
          tabValVars(i) = float(p%iflos)
          CYCLE
       end if
       if (tabNomVars(i) == 'igers') then
          tabValVars(i) = float(p%igers)
          CYCLE
       end if
       if (tabNomVars(i) == 'ilans') then
          tabValVars(i) = float(p%ilans)
          CYCLE
       end if
       if (tabNomVars(i) == 'ilaxs') then
          tabValVars(i) = float(p%ilaxs)
          CYCLE
       end if
       if (tabNomVars(i) == 'ilevs') then
          tabValVars(i) = float(p%ilevs)
          CYCLE
       end if
       if (tabNomVars(i) == 'imats') then
          tabValVars(i) = float(p%imats)
          CYCLE
       end if
       if (tabNomVars(i) == 'imontaisons') then
          tabValVars(i) = float(p%imontaisons)
          CYCLE
       end if
       if (tabNomVars(i) == 'infil_recal(1)') then
          tabValVars(i) = soil%infil_recal(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'infil_recal(2)') then
          tabValVars(i) = soil%infil_recal(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'infil_recal(3)') then
          tabValVars(i) = soil%infil_recal(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'infil_recal(4)') then
          tabValVars(i) = soil%infil_recal(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'infil_recal(5)') then
          tabValVars(i) = soil%infil_recal(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'inn') then
          tabValVars(i) = p%inn(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'inn1intercoupe') then
          tabValVars(i) = p%inn1intercoupe
          CYCLE
       end if
       if (tabNomVars(i) == 'inn1moy') then
          tabValVars(i) = p%inn1moy
          CYCLE
       end if
       if (tabNomVars(i) == 'inn2intercoupe') then
          tabValVars(i) = p%inn2intercoupe
          CYCLE
       end if
       if (tabNomVars(i) == 'inn2moy') then
          tabValVars(i) = p%inn2moy
          CYCLE
       end if
       if (tabNomVars(i) == 'innlai') then
          tabValVars(i) = p%innlai(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'inns') then
          tabValVars(i) = p%inns(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'innsenes') then
          tabValVars(i) = p%innsenes(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'inous') then
          tabValVars(i) = p%inous
          CYCLE
       end if
       if (tabNomVars(i) == 'intermulch') then
          tabValVars(i) = sc%intermulch
          CYCLE
       end if
       if (tabNomVars(i) == 'interpluie') then
          tabValVars(i) = p%interpluie(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'iplts') then
          tabValVars(i) = float(p%iplts)
          CYCLE
       end if
       if (tabNomVars(i) == 'irazo(n)') then
          tabValVars(i) = p%irazo(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'ircarb(n)') then
          tabValVars(i) = p%ircarb(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'irecs') then
          tabValVars(i) = float(p%irecs)
          CYCLE
       end if
       if (tabNomVars(i) == 'irrigjN') then
          tabValVars(i) = sc%irrigjN
          CYCLE
       end if
       if (tabNomVars(i) == 'irrigN') then
          tabValVars(i) = sc%irrigN
          CYCLE
       end if
       if (tabNomVars(i) == 'isens') then
          tabValVars(i) = float(p%isens)
          CYCLE
       end if
       if (tabNomVars(i) == 'izrac') then
          tabValVars(i) = p%izrac
          CYCLE
       end if
       if (tabNomVars(i) == 'lai(n)') then
          if (p%P_codeplante == 'fou') then
             tabValVars(i) = p%lai_mx_av_cut
          else
             tabValVars(i) = p%lai(aoas, n)
          end if
          CYCLE
       end if
       if (tabNomVars(i) == 'lai(ao)') then
          tabValVars(i) = p%lai(ao, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'lai(as)') then
          tabValVars(i) = p%lai(as, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'lai_mx_av_cut') then
          tabValVars(i) = p%lai_mx_av_cut
          CYCLE
       end if
       if (tabNomVars(i) == 'laimax') then
          tabValVars(i) = p%laimax(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'laisen(n)') then
          tabValVars(i) = p%laisen(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'largeur') then
          tabValVars(i) = p%largeur(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'leaching_from_plt') then
          tabValVars(i) = sc%leaching_from_plt
          CYCLE
       end if
       if (tabNomVars(i) == 'leaching_from_lev') then
          tabValVars(i) = sc%leaching_from_lev
          CYCLE
       end if
       if (tabNomVars(i) == 'leai') then
          tabValVars(i) = p%leai(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'lessiv') then
          tabValVars(i) = sc%lessiv
          CYCLE
       end if
       if (tabNomVars(i) == 'LRACH(1)') then
          tabValVars(i) = p%LRACH(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'LRACH(2)') then
          tabValVars(i) = p%LRACH(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'LRACH(3)') then
          tabValVars(i) = p%LRACH(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'LRACH(4)') then
          tabValVars(i) = p%LRACH(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'LRACH(5)') then
          tabValVars(i) = p%LRACH(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'lracsentot') then
          tabValVars(i) = p%lracsentot
          CYCLE
       end if
       if (tabNomVars(i) == 'mabois') then
          tabValVars(i) = p%mabois(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'maenfruit') then
          tabValVars(i) = p%maenfruit(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'mafeuil') then
          tabValVars(i) = p%mafeuil(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'mafeuil_kg_ha') then
          tabValVars(i) = p%mafeuil_kg_ha
          CYCLE
       end if
       if (tabNomVars(i) == 'mafeuiljaune') then
          tabValVars(i) = p%mafeuiljaune(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'mafeuiltombe') then
          tabValVars(i) = p%mafeuiltombe(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'mafeuilverte') then
          tabValVars(i) = p%mafeuilverte(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'mafrais') then
          tabValVars(i) = p%mafrais(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'mafruit') then
          if (p%P_codeplante == 'bet') then
             tabValVars(i) = p%matuber
          else
             tabValVars(i) = p%mafruit
          end if
          CYCLE
       end if
       if (tabNomVars(i) == 'mafruit_kg_ha') then
          tabValVars(i) = p%mafruit_kg_ha
          CYCLE
       end if
       if (tabNomVars(i) == 'masec(n)') then
          if (p%P_codeplante == 'fou') then
             tabValVars(i) = p%masec_mx_av_cut
          else
             tabValVars(i) = p%masec(aoas, n)
          end if
          CYCLE
       end if
       if (tabNomVars(i) == 'masec_kg_ha') then
          tabValVars(i) = p%masec_kg_ha
          CYCLE
       end if
       if (tabNomVars(i) == 'masec_mx_av_cut') then
          tabValVars(i) = p%masec_mx_av_cut
          CYCLE
       end if
       if (tabNomVars(i) == 'masecneo') then
          tabValVars(i) = p%masecneo(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'masectot') then
          tabValVars(i) = p%masectot
          CYCLE
       end if
       if (tabNomVars(i) == 'masecveg') then
          tabValVars(i) = p%masecveg(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'matigestruc') then
          tabValVars(i) = p%matigestruc(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'matigestruc_kg_ha') then
          tabValVars(i) = p%matigestruc_kg_ha
          CYCLE
       end if
       if (tabNomVars(i) == 'matuber') then
          tabValVars(i) = p%matuber
          CYCLE
       end if
       if (tabNomVars(i) == 'mortalle') then
          tabValVars(i) = p%mortalle
          CYCLE
       end if
       if (tabNomVars(i) == 'mortmasec') then
          tabValVars(i) = p%mortmasec
          CYCLE
       end if
       if (tabNomVars(i) == 'mortreserve') then
          tabValVars(i) = p%mortreserve
          CYCLE
       end if
       if (tabNomVars(i) == 'MSexporte') then
          tabValVars(i) = p%MSexporte
          CYCLE
       end if
       if (tabNomVars(i) == 'msjaune') then
          tabValVars(i) = p%msjaune(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'msneojaune') then
          tabValVars(i) = p%msneojaune(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'msrac(n)') then
          tabValVars(i) = p%msrac(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'msrec_fou') then
          tabValVars(i) = p%msrec_fou
          CYCLE
       end if
       if (tabNomVars(i) == 'MSrecycle') then
          tabValVars(i) = p%MSrecycle
          CYCLE
       end if
       if (tabNomVars(i) == 'msresjaune') then
          tabValVars(i) = p%msresjaune(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'N_mineralisation') then
          tabValVars(i) = sc%N_mineralisation
          CYCLE
       end if
       if (tabNomVars(i) == 'N_volatilisation') then
          tabValVars(i) = soil%N_volatilisation
          CYCLE
       end if
       if (tabNomVars(i) == 'Nb') then
          tabValVars(i) = sc%Nb
          CYCLE
       end if
       if (tabNomVars(i) == 'nbfeuille') then
          tabValVars(i) = float(p%nbfeuille)
          CYCLE
       end if
       if (tabNomVars(i) == 'nbinflo_recal') then
          tabValVars(i) = p%nbinflo_recal
          CYCLE
       end if
       if (tabNomVars(i) == 'nbj0remp') then
          tabValVars(i) = float(p%nbj0remp)
          CYCLE
       end if
       if (tabNomVars(i) == 'nbjechaudage') then
          tabValVars(i) = float(c%nbjechaudage)
          CYCLE
       end if
       if (tabNomVars(i) == 'nbjgel') then
          tabValVars(i) = float(p%nbjgel)
          CYCLE
       end if
       if (tabNomVars(i) == 'nbjpourdecirecolte') then
          tabValVars(i) = float(p%nbjpourdecirecolte)
          CYCLE
       end if
       if (tabNomVars(i) == 'nbjpourdecisemis') then
          tabValVars(i) = float(p%nbjpourdecisemis)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nbmulch') then
          tabValVars(i) = sc%Nbmulch
          CYCLE
       end if
       if (tabNomVars(i) == 'NCbio') then
          tabValVars(i) = sc%NCbio
          CYCLE
       end if
       if (tabNomVars(i) == 'Ndenit') then
          tabValVars(i) = soil%Ndenit
          CYCLE
       end if
       if (tabNomVars(i) == 'Nexporte') then
          tabValVars(i) = p%Nexporte
          CYCLE
       end if
       if (tabNomVars(i) == 'nfruit(1)') then
          tabValVars(i) = p%nfruit(aoas, 1)
          CYCLE
       end if
       if (tabNomVars(i) == 'nfruit(2)') then
          tabValVars(i) = p%nfruit(aoas, 2)
          CYCLE
       end if
       if (tabNomVars(i) == 'nfruit(3)') then
          tabValVars(i) = p%nfruit(aoas, 3)
          CYCLE
       end if
       if (tabNomVars(i) == 'nfruit(4)') then
          tabValVars(i) = p%nfruit(aoas, 4)
          CYCLE
       end if
       if (tabNomVars(i) == 'nfruit(5)') then
          tabValVars(i) = p%nfruit(aoas, 5)
          CYCLE
       end if
       if (tabNomVars(i) == 'nfruit(nboite)') then
          tabValVars(i) = p%nfruit(aoas, p%P_nboite)
          CYCLE
       end if
       if (tabNomVars(i) == 'nfruit(nboite-1)') then
          tabValVars(i) = p%nfruit(aoas, p%P_nboite - 1)
          CYCLE
       end if
       if (tabNomVars(i) == 'nfruitnou') then
          tabValVars(i) = p%nfruitnou(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nhuma') then
          tabValVars(i) = sc%Nhuma
          CYCLE
       end if
       if (tabNomVars(i) == 'Nhumi') then
          tabValVars(i) = sc%Nhumi
          CYCLE
       end if
       if (tabNomVars(i) == 'Nhumt') then
          tabValVars(i) = sc%Nhumt
          CYCLE
       end if
       if (tabNomVars(i) == 'nitetcult(n)') then
          tabValVars(i) = float(c%nitetcult(n))
          CYCLE
       end if
       if (tabNomVars(i) == 'nitrifj') then
          tabValVars(i) = soil%nitrifj
          CYCLE
       end if
       if (tabNomVars(i) == 'Nmineral_from_plt') then
          tabValVars(i) = sc%Nmineral_from_plt
          CYCLE
       end if
       if (tabNomVars(i) == 'Nmineral_from_lev') then
          tabValVars(i) = sc%Nmineral_from_lev
          CYCLE
       end if
       if (tabNomVars(i) == 'Nmulchdec') then
          tabValVars(i) = sc%Nmulchdec
          CYCLE
       end if
       if (tabNomVars(i) == 'Nmulchnd') then
          tabValVars(i) = sc%Nmulchnd
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(1)') then
          tabValVars(i) = sc%Nnondec(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(10)') then
          tabValVars(i) = sc%Nnondec(10)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(2)') then
          tabValVars(i) = sc%Nnondec(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(3)') then
          tabValVars(i) = sc%Nnondec(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(4)') then
          tabValVars(i) = sc%Nnondec(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(5)') then
          tabValVars(i) = sc%Nnondec(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(6)') then
          tabValVars(i) = sc%Nnondec(6)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(7)') then
          tabValVars(i) = sc%Nnondec(7)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(8)') then
          tabValVars(i) = sc%Nnondec(8)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nnondec(9)') then
          tabValVars(i) = sc%Nnondec(9)
          CYCLE
       end if
       if (tabNomVars(i) == 'nodn') then
          tabValVars(i) = sc%nodn
          CYCLE
       end if
       if (tabNomVars(i) == 'Norgeng') then
          tabValVars(i) = soil%Norgeng
          CYCLE
       end if
       if (tabNomVars(i) == 'Nr') then
          tabValVars(i) = sc%Nr
          CYCLE
       end if
       if (tabNomVars(i) == 'Nrac') then
          tabValVars(i) = p%Nrac
          CYCLE
       end if
       if (tabNomVars(i) == 'Nrecycle') then
          tabValVars(i) = p%Nrecycle
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(1)') then
          tabValVars(i) = sc%Nresiduprofil(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(10)') then
          tabValVars(i) = sc%Nresiduprofil(10)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(2)') then
          tabValVars(i) = sc%Nresiduprofil(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(3)') then
          tabValVars(i) = sc%Nresiduprofil(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(4)') then
          tabValVars(i) = sc%Nresiduprofil(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(5)') then
          tabValVars(i) = sc%Nresiduprofil(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(6)') then
          tabValVars(i) = sc%Nresiduprofil(6)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(7)') then
          tabValVars(i) = sc%Nresiduprofil(7)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(8)') then
          tabValVars(i) = sc%Nresiduprofil(8)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nresiduprofil(9)') then
          tabValVars(i) = sc%Nresiduprofil(9)
          CYCLE
       end if
       if (tabNomVars(i) == 'Ntousresidusprofil') then
          tabValVars(i) = sc%Ntousresidusprofil
          CYCLE
       end if
       if (tabNomVars(i) == 'numcoupe') then
          tabValVars(i) = float(p%numcoupe)
          CYCLE
       end if
       if (tabNomVars(i) == 'numcult') then
          tabValVars(i) = float(sc%numcult)
          CYCLE
       end if
       if (tabNomVars(i) == 'Nvolat_from_plt') then
          tabValVars(i) = sc%Nvolat_from_plt
          CYCLE
       end if
       if (tabNomVars(i) == 'Nvolat_from_lev') then
          tabValVars(i) = sc%Nvolat_from_lev
          CYCLE
       end if
       if (tabNomVars(i) == 'Nvoleng') then
          tabValVars(i) = soil%Nvoleng
          CYCLE
       end if
       if (tabNomVars(i) == 'offrenod') then
          tabValVars(i) = p%offrenod(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'p1000grain') then
          tabValVars(i) = p%p1000grain
          CYCLE
       end if
       if (tabNomVars(i) == 'pdsfruit(1)') then
          tabValVars(i) = p%pdsfruit(aoas, 1)
          CYCLE
       end if
       if (tabNomVars(i) == 'pdsfruit(2)') then
          tabValVars(i) = p%pdsfruit(aoas, 2)
          CYCLE
       end if
       if (tabNomVars(i) == 'pdsfruit(3)') then
          tabValVars(i) = p%pdsfruit(aoas, 3)
          CYCLE
       end if
       if (tabNomVars(i) == 'pdsfruit(4)') then
          tabValVars(i) = p%pdsfruit(aoas, 4)
          CYCLE
       end if
       if (tabNomVars(i) == 'pdsfruit(5)') then
          tabValVars(i) = p%pdsfruit(aoas, 5)
          CYCLE
       end if
       if (tabNomVars(i) == 'pdsfruit(nboite)') then
          tabValVars(i) = p%pdsfruit(aoas, p%P_nboite)
          CYCLE
       end if
       if (tabNomVars(i) == 'pdsfruit(nboite-1)') then
          tabValVars(i) = p%pdsfruit(aoas, p%P_nboite - 1)
          CYCLE
       end if
       if (tabNomVars(i) == 'pdsfruitfrais') then
          tabValVars(i) = p%pdsfruitfrais(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'penfruit') then
          tabValVars(i) = p%penfruit(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'pfeuil(n)') then
          tabValVars(i) = p%pfeuil(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'pfeuiljaune') then
          tabValVars(i) = p%pfeuiljaune(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'pfeuilverte(n)') then
          tabValVars(i) = p%pfeuilverte(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'phoi') then
          tabValVars(i) = c%phoi
          CYCLE
       end if
       if (tabNomVars(i) == 'pHvol') then
          tabValVars(i) = soil%pHvol
          CYCLE
       end if
       if (tabNomVars(i) == 'pousfruit') then
          tabValVars(i) = p%pousfruit(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'poussracmoy') then
          tabValVars(i) = p%poussracmoy
          CYCLE
       end if
       if (tabNomVars(i) == 'precip') then
          tabValVars(i) = sc%precip
          CYCLE
       end if
       if (tabNomVars(i) == 'precipjN') then
          tabValVars(i) = sc%precipjN
          CYCLE
       end if
       if (tabNomVars(i) == 'precipN') then
          tabValVars(i) = sc%precipN
          CYCLE
       end if
       if (tabNomVars(i) == 'preserve') then
          tabValVars(i) = p%preserve(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'profexteau') then
          tabValVars(i) = p%profexteau
          CYCLE
       end if
       if (tabNomVars(i) == 'profextN') then
          tabValVars(i) = p%profextN
          CYCLE
       end if
       if (tabNomVars(i) == 'profnappe') then
          tabValVars(i) = soil%profnappe
          CYCLE
       end if
       if (tabNomVars(i) == 'psibase') then
          tabValVars(i) = p%psibase
          CYCLE
       end if
       if (tabNomVars(i) == 'ptigestruc') then
          tabValVars(i) = p%ptigestruc(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'QCapp') then
          tabValVars(i) = sc%QCapp
          CYCLE
       end if
       if (tabNomVars(i) == 'QCO2hum') then
          tabValVars(i) = sc%QCO2hum
          CYCLE
       end if
       if (tabNomVars(i) == 'QCO2mul') then
          tabValVars(i) = sc%QCO2mul
          CYCLE
       end if
       if (tabNomVars(i) == 'QCO2res') then
          tabValVars(i) = sc%QCO2res
          CYCLE
       end if
       if (tabNomVars(i) == 'QCO2sol') then
          tabValVars(i) = sc%QCO2sol
          CYCLE
       end if
       if (tabNomVars(i) == 'QCplantetombe') then
          tabValVars(i) = p%QCplantetombe(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'QCprimed') then
          tabValVars(i) = sc%QCprimed
          CYCLE
       end if
       if (tabNomVars(i) == 'QCrac') then
          tabValVars(i) = p%QCrac
          CYCLE
       end if
       if (tabNomVars(i) == 'QCresorg') then
          tabValVars(i) = sc%QCresorg
          CYCLE
       end if
       if (tabNomVars(i) == 'QCressuite') then
          tabValVars(i) = p%QCressuite
          CYCLE
       end if
       if (tabNomVars(i) == 'QCrogne') then
          tabValVars(i) = p%QCrogne
          CYCLE
       end if
       if (tabNomVars(i) == 'Qdrain') then
          tabValVars(i) = soil%Qdrain
          CYCLE
       end if
       if (tabNomVars(i) == 'Qdraincum') then
          tabValVars(i) = soil%Qdraincum
          CYCLE
       end if
       if (tabNomVars(i) == 'Qem_N2O') then
          tabValVars(i) = sc%Qem_N2O
          CYCLE
       end if
       if (tabNomVars(i) == 'Qem_N2Oden') then
          tabValVars(i) = sc%Qem_N2Oden
          CYCLE
       end if
       if (tabNomVars(i) == 'Qem_N2Onit') then
          tabValVars(i) = sc%Qem_N2Onit
          CYCLE
       end if
       if (tabNomVars(i) == 'Qfix') then
          tabValVars(i) = p%Qfix(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'Qles') then
          tabValVars(i) = sc%Qles
          CYCLE
       end if
       if (tabNomVars(i) == 'Qlesd') then
          tabValVars(i) = soil%Qlesd
          CYCLE
       end if
       if (tabNomVars(i) == 'Qminh') then
          tabValVars(i) = sc%Qminh
          CYCLE
       end if
       if (tabNomVars(i) == 'Qminr') then
          tabValVars(i) = sc%Qminr
          CYCLE
       end if
       if (tabNomVars(i) == 'qmulch') then
          tabValVars(i) = sc%qmulch
          CYCLE
       end if
       if (tabNomVars(i) == 'QNapp') then
          tabValVars(i) = sc%QNapp
          CYCLE
       end if
       if (tabNomVars(i) == 'QNdenit') then
          tabValVars(i) = soil%QNdenit
          CYCLE
       end if
       if (tabNomVars(i) == 'QNdenit_from_plt') then
          tabValVars(i) = sc%QNdenit_from_plt
          CYCLE
       end if
       if (tabNomVars(i) == 'QNdenit_from_lev') then
          tabValVars(i) = sc%QNdenit_from_lev
          CYCLE
       end if
       if (tabNomVars(i) == 'QNexport') then
          tabValVars(i) = p%QNexport
          CYCLE
       end if
       if (tabNomVars(i) == 'QNgrain') then
          tabValVars(i) = p%QNgrain(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'Qnitrif') then
          tabValVars(i) = sc%Qnitrif
          CYCLE
       end if
       if (tabNomVars(i) == 'QNorgeng') then
          tabValVars(i) = soil%QNorgeng
          CYCLE
       end if
       if (tabNomVars(i) == 'QNplante') then
          if (p%P_codeplante == 'fou') then
             tabValVars(i) = p%QNplante_mx_av_cut
          else
             tabValVars(i) = p%QNplante(aoas, n)
          end if
          CYCLE
       end if
       if (tabNomVars(i) == 'QNplante_mx_av_cut') then
          tabValVars(i) = p%QNplante_mx_av_cut
          CYCLE
       end if
       if (tabNomVars(i) == 'QNplantetombe') then
          tabValVars(i) = p%QNplantetombe(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'QNprimed') then
          tabValVars(i) = sc%QNprimed
          CYCLE
       end if
       if (tabNomVars(i) == 'QNrac') then
          tabValVars(i) = p%QNrac
          CYCLE
       end if
       if (tabNomVars(i) == 'QNresorg') then
          tabValVars(i) = sc%QNresorg
          CYCLE
       end if
       if (tabNomVars(i) == 'QNressuite') then
          tabValVars(i) = p%QNressuite
          CYCLE
       end if
       if (tabNomVars(i) == 'QNrogne') then
          tabValVars(i) = p%QNrogne
          CYCLE
       end if
       if (tabNomVars(i) == 'QNvoleng') then
          tabValVars(i) = soil%QNvoleng
          CYCLE
       end if
       if (tabNomVars(i) == 'QNvolorg') then
          tabValVars(i) = soil%QNvolorg
          CYCLE
       end if
       if (tabNomVars(i) == 'qres_pature') then
          tabValVars(i) = sc%qres_pature
          CYCLE
       end if
       if (tabNomVars(i) == 'Qressuite') then
          tabValVars(i) = p%Qressuite
          CYCLE
       end if
       if (tabNomVars(i) == 'Qressuite_tot') then
          tabValVars(i) = p%Qressuite_tot
          CYCLE
       end if
       if (tabNomVars(i) == 'ra_recal') then
          tabValVars(i) = sta%ra_recal
          CYCLE
       end if
       if (tabNomVars(i) == 'raint') then
          tabValVars(i) = p%raint(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'ras') then
          tabValVars(i) = sc%ras
          CYCLE
       end if
       if (tabNomVars(i) == 'Ratm') then
          tabValVars(i) = sc%Ratm
          CYCLE
       end if
       if (tabNomVars(i) == 'rc') then
          tabValVars(i) = p%rc
          CYCLE
       end if
       if (tabNomVars(i) == 'rdif') then
          tabValVars(i) = sc%rdif
          CYCLE
       end if
       if (tabNomVars(i) == 'remobilj') then
          tabValVars(i) = p%remobilj(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'remontee') then
          tabValVars(i) = soil%remontee
          CYCLE
       end if
       if (tabNomVars(i) == 'rendementsec') then
          tabValVars(i) = p%rendementsec
          CYCLE
       end if
       if (tabNomVars(i) == 'resmes') then
          tabValVars(i) = sc%resmes
          CYCLE
       end if
       if (tabNomVars(i) == 'resperenne') then
          tabValVars(i) = p%resperenne(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'resrac') then
          tabValVars(i) = p%resrac
          CYCLE
       end if
       if (tabNomVars(i) == 'rfpi') then
          tabValVars(i) = p%rfpi
          CYCLE
       end if
       if (tabNomVars(i) == 'rfvi') then
          tabValVars(i) = p%rfvi
          CYCLE
       end if
       if (tabNomVars(i) == 'rlj') then
          tabValVars(i) = p%rlj
          CYCLE
       end if
       if (tabNomVars(i) == 'rltot') then
          tabValVars(i) = p%rltot
          CYCLE
       end if
       if (tabNomVars(i) == 'rmaxi') then
          tabValVars(i) = p%rmaxi
          CYCLE
       end if
       if (tabNomVars(i) == 'rnet') then
          tabValVars(i) = sc%rnet
          CYCLE
       end if
       if (tabNomVars(i) == 'rnet_plant') then
          tabValVars(i) = p%rnet_plant
          CYCLE
       end if
       if (tabNomVars(i) == 'rnetS') then
          tabValVars(i) = sc%rnetS
          CYCLE
       end if
       if (tabNomVars(i) == 'rombre') then
          tabValVars(i) = p%rombre
          CYCLE
       end if
       if (tabNomVars(i) == 'rsoleil') then
          tabValVars(i) = p%rsoleil
          CYCLE
       end if
       if (tabNomVars(i) == 'RsurRU') then
          tabValVars(i) = sc%RsurRU
          CYCLE
       end if
       if (tabNomVars(i) == 'RsurRUrac') then
          tabValVars(i) = p%RsurRUrac
          CYCLE
       end if
       if (tabNomVars(i) == 'RU') then
          tabValVars(i) = sc%RU
          CYCLE
       end if
       if (tabNomVars(i) == 'ruissel') then
          tabValVars(i) = sc%ruissel
          CYCLE
       end if
       if (tabNomVars(i) == 'ruisselsurf') then
          tabValVars(i) = sc%ruisselsurf
          CYCLE
       end if
       if (tabNomVars(i) == 'ruisselt') then
          tabValVars(i) = sc%ruisselt
          CYCLE
       end if
       if (tabNomVars(i) == 'runoff_from_plt') then
          tabValVars(i) = sc%runoff_from_plt
          CYCLE
       end if
       if (tabNomVars(i) == 'runoff_from_lev') then
          tabValVars(i) = sc%runoff_from_lev
          CYCLE
       end if
       if (tabNomVars(i) == 'RUrac') then
          tabValVars(i) = p%RUrac
          CYCLE
       end if
       if (tabNomVars(i) == 'saturation') then
          tabValVars(i) = sc%saturation
          CYCLE
       end if
       if (tabNomVars(i) == 'senfac') then
          tabValVars(i) = p%senfac(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'sla') then
          tabValVars(i) = p%sla(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'SoilAvW') then
          tabValVars(i) = sc%SoilAvW
          CYCLE
       end if
       if (tabNomVars(i) == 'SoilN') then
          tabValVars(i) = sc%SoilN
          CYCLE
       end if
       if (tabNomVars(i) == 'SoilNM') then
          tabValVars(i) = sc%SoilNM
          CYCLE
       end if
       if (tabNomVars(i) == 'SoilWatM') then
          tabValVars(i) = sc%SoilWatM
          CYCLE
       end if
       if (tabNomVars(i) == 'som_HUR') then
          tabValVars(i) = sc%som_HUR
          CYCLE
       end if
       if (tabNomVars(i) == 'som_sat') then
          tabValVars(i) = sc%som_sat
          CYCLE
       end if
       if (tabNomVars(i) == 'somcour') then
          tabValVars(i) = p%somcour
          CYCLE
       end if
       if (tabNomVars(i) == 'somcourdrp') then
          tabValVars(i) = p%somcourdrp
          CYCLE
       end if
       if (tabNomVars(i) == 'somcourfauche') then
          tabValVars(i) = p%somcourfauche
          CYCLE
       end if
       if (tabNomVars(i) == 'somcourmont') then
          tabValVars(i) = p%somcourmont
          CYCLE
       end if
       if (tabNomVars(i) == 'somdifftculttair') then
          tabValVars(i) = c%somdifftculttair
          CYCLE
       end if
       if (tabNomVars(i) == 'somtemp') then
          tabValVars(i) = p%somtemp
          CYCLE
       end if
       if (tabNomVars(i) == 'somudevair') then
          tabValVars(i) = p%somudevair
          CYCLE
       end if
       if (tabNomVars(i) == 'somudevcult') then
          tabValVars(i) = p%somudevcult
          CYCLE
       end if
       if (tabNomVars(i) == 'somupvtsem') then
          tabValVars(i) = p%somupvtsem
          CYCLE
       end if
       if (tabNomVars(i) == 'sourcepuits') then
          tabValVars(i) = p%sourcepuits(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'spfruit') then
          tabValVars(i) = p%spfruit(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'splai') then
          tabValVars(i) = p%splai(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'stemflow') then
          tabValVars(i) = p%stemflow
          CYCLE
       end if
       if (tabNomVars(i) == 'str1intercoupe') then
          tabValVars(i) = p%str1intercoupe
          CYCLE
       end if
       if (tabNomVars(i) == 'str2intercoupe') then
          tabValVars(i) = p%str2intercoupe
          CYCLE
       end if
       if (tabNomVars(i) == 'stu1intercoupe') then
          tabValVars(i) = p%stu1intercoupe
          CYCLE
       end if
       if (tabNomVars(i) == 'stu2intercoupe') then
          tabValVars(i) = p%stu2intercoupe
          CYCLE
       end if
       if (tabNomVars(i) == 'sucre') then
          tabValVars(i) = p%sucre(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'sucre_percent') then
          tabValVars(i) = p%sucre_percent(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'surf(ao)') then
          tabValVars(i) = p%surf(ao)
          CYCLE
       end if
       if (tabNomVars(i) == 'surf(as)') then
          tabValVars(i) = p%surf(as)
          CYCLE
       end if
       if (tabNomVars(i) == 'surfSous(ao)') then
          tabValVars(i) = p%surfSous(ao)
          CYCLE
       end if
       if (tabNomVars(i) == 'surfSous(as)') then
          tabValVars(i) = p%surfSous(as)
          CYCLE
       end if
       if (tabNomVars(i) == 'swfac') then
          tabValVars(i) = p%swfac(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'swfac1moy') then
          tabValVars(i) = p%swfac1moy
          CYCLE
       end if
       if (tabNomVars(i) == 'swfac2moy') then
          tabValVars(i) = p%swfac2moy
          CYCLE
       end if
       if (tabNomVars(i) == 'tairveille') then
          tabValVars(i) = sc%tairveille
          CYCLE
       end if
       if (tabNomVars(i) == 'tauxcouv(n)') then
          if (p%P_codelaitr /= 1) then
             tabValVars(i) = sc%tauxcouv(n)
          else
             tabValVars(i) = p%lai(aoas, n)
          end if
          CYCLE
       end if
       if (tabNomVars(i) == 'tcult') then
          tabValVars(i) = sc%tcult
          CYCLE
       end if
       if (tabNomVars(i) == 'tcult_tairveille') then
          tabValVars(i) = sc%tcult_tairveille
          CYCLE
       end if
       if (tabNomVars(i) == 'tcultmax') then
          tabValVars(i) = sc%tcultmax
          CYCLE
       end if
       if (tabNomVars(i) == 'tcultmin') then
          tabValVars(i) = sc%tcultmin
          CYCLE
       end if
       if (tabNomVars(i) == 'tempeff') then
          tabValVars(i) = p%tempeff
          CYCLE
       end if
       if (tabNomVars(i) == 'tetp(n)') then
          tabValVars(i) = c%tetp(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'tetstomate') then
          tabValVars(i) = p%tetstomate
          CYCLE
       end if
       if (tabNomVars(i) == 'teturg') then
          tabValVars(i) = p%teturg
          CYCLE
       end if
       if (tabNomVars(i) == 'tmax(n)') then
          tabValVars(i) = c%tmax(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'tmaxext(n)') then
          tabValVars(i) = c%tmaxext(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'tmin(n)') then
          tabValVars(i) = c%tmin(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'tminext(n)') then
          tabValVars(i) = c%tminext(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'tmoy(n)') then
          tabValVars(i) = c%tmoy(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'tmoyext(n)') then
          tabValVars(i) = c%tmoyext(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'tmoyIpltJuin') then
          tabValVars(i) = p%tmoyIpltJuin
          CYCLE
       end if
       if (tabNomVars(i) == 'tmoyIpltSept') then
          tabValVars(i) = p%tmoyIpltSept
          CYCLE
       end if
       if (tabNomVars(i) == 'tncultmat') then
          tabValVars(i) = c%tncultmat
          CYCLE
       end if
       if (tabNomVars(i) == 'tnhc') then
          tabValVars(i) = sc%tnhc
          CYCLE
       end if
       if (tabNomVars(i) == 'tnrc') then
          tabValVars(i) = sc%tnrc
          CYCLE
       end if
       if (tabNomVars(i) == 'totapN') then
          tabValVars(i) = sc%totapN
          CYCLE
       end if
       if (tabNomVars(i) == 'totapNres') then
          tabValVars(i) = sc%totapNres
          CYCLE
       end if
       if (tabNomVars(i) == 'totir') then
          tabValVars(i) = sc%totir
          CYCLE
       end if
       if (tabNomVars(i) == 'tpm(n)') then
          tabValVars(i) = c%tpm(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'trg(n)') then
          tabValVars(i) = c%trg(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'trgext(n)') then
          tabValVars(i) = c%trgext(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'trr(n)') then
          tabValVars(i) = c%trr(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'TS(1)') then
          tabValVars(i) = sc%TS(1)
          CYCLE
       end if
       if (tabNomVars(i) == 'TS(2)') then
          tabValVars(i) = sc%TS(2)
          CYCLE
       end if
       if (tabNomVars(i) == 'TS(3)') then
          tabValVars(i) = sc%TS(3)
          CYCLE
       end if
       if (tabNomVars(i) == 'TS(4)') then
          tabValVars(i) = sc%TS(4)
          CYCLE
       end if
       if (tabNomVars(i) == 'TS(5)') then
          tabValVars(i) = sc%TS(5)
          CYCLE
       end if
       if (tabNomVars(i) == 'turfac') then
          tabValVars(i) = p%turfac(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'turfac1moy') then
          tabValVars(i) = p%turfac1moy
          CYCLE
       end if
       if (tabNomVars(i) == 'turfac2moy') then
          tabValVars(i) = p%turfac2moy
          CYCLE
       end if
       if (tabNomVars(i) == 'tustress') then
          tabValVars(i) = sc%tustress
          CYCLE
       end if
       if (tabNomVars(i) == 'tvent(n)') then
          tabValVars(i) = c%tvent(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'udevair') then
          tabValVars(i) = p%udevair
          CYCLE
       end if
       if (tabNomVars(i) == 'udevcult') then
          tabValVars(i) = p%udevcult
          CYCLE
       end if
       if (tabNomVars(i) == 'ulai(n)') then
          tabValVars(i) = p%ulai(aoas, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'ulai(ao)') then
          tabValVars(i) = p%ulai(ao, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'ulai(as)') then
          tabValVars(i) = p%ulai(as, n)
          CYCLE
       end if
       if (tabNomVars(i) == 'upvt(n)') then
          tabValVars(i) = p%upvt(n)
          CYCLE
       end if
       if (tabNomVars(i) == 'vitmoy') then
          tabValVars(i) = p%vitmoy(aoas)
          CYCLE
       end if
       if (tabNomVars(i) == 'xmlch1') then
          tabValVars(i) = sc%xmlch1
          CYCLE
       end if
       if (tabNomVars(i) == 'zrac') then
          tabValVars(i) = p%zrac
          CYCLE
       end if
       if (tabNomVars(i) == 'tsol(10)') then
          tabValVars(i) = sc%tsol(10)
          CYCLE
       end if
       if (tabNomVars(i) == 'hur_10_vol') then
          tabValVars(i) = sc%hur_10_vol
          CYCLE
       end if
       if (tabNomVars(i) == 'light_beer_active') then
          tabValVars(i) = sc%light_beer_active
          CYCLE
       end if
       call EnvoyerMsgHistorique('')
       write (tmp, *) nom, ': unknown variable name (check case sensitivity)'
       call EnvoyerMsgHistorique(tmp)
    end do B1
    return
 end subroutine CorrespondanceVariablesDeSorties
