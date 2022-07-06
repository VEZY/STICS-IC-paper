! ***************************************************** c
! * modif Bruno  20/04/99                              * c
! * écriture d'un rapport final en fin de simulation  * c
! * + éventuellement à 2 autres dates                 * c
! ***************************************************** c
! ***************************************************** c
! Writing a final report at the end of simulation for agmip project (file mod_rapport_AGMIP.sti)
!subroutine Ecriture_Rapport_agmip(sc,pg,soil,c,sta,p,itk,t)  !DR 19/07/2012 c, itk et t pas utile
subroutine Ecriture_Rapport_agmip(sc,pg,soil,sta,p,t)


USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Climat
USE Station
USE Sol

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Parametres_Generaux_), intent(IN)    :: pg  

  type(Sol_),                 intent(INOUT) :: soil  

!  type(Climat_),              intent(INOUT) :: c

  type(Station_),             intent(INOUT) :: sta  

  type(Plante_),              intent(INOUT) :: p  

!  type(ITK_),                 intent(INOUT) :: itk

  type(Stics_Transit_),       intent(INOUT) :: t


      integer           :: ancours  !  
      integer           :: ifin  !  
      integer           :: i
      real              :: nstt1  !  
      real              :: QNF  !  
      real              :: rdt  
      ! PB - je le rajoute en tant que variable locale
      real              :: nstt2  

      character         :: sep  
      character(len=10) :: commentcelia  
      character(len=10) :: commentCC


     character(len=500) :: mes3000
    ! character(len=9)   :: treatment
     integer       :: j, nbligne_entete
     character*329 :: ligne
     integer       :: long_usm
     character*3   :: nom_plante
     logical :: AgMIP,Macsur
     real :: tcultmoy_cycle, tcultmax_cycle
     integer :: an_iplt,jour_iplt,nummois_iplt,an_iflo,jour_iflo,nummois_iflo,an_imat,jour_imat,nummois_imat
     integer :: an_ilev,jour_ilev,nummois_ilev
     integer :: jour
!     integer :: Flag_Agmpip_rap
     character*3 :: mois
     character*2 :: charmois_iplt,charjour_iplt
     character*2 :: charmois_ilev,charjour_ilev
     character*2 :: charmois_iflo,charjour_iflo
     character*2 :: charmois_imat,charjour_imat
     character*2 :: chaine

     integer      ::  ii
     ! ,code_ecrit_nom_usm
     character*10 ::  scenario
     character*3  ::  co2
     character*30 ::  treatment,usm

     character*2 :: chiffre(31)
     chiffre = (/'01','02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19', &
                & '20','21','22','23','24','25','26','27','28','29','30','31'/)

  AgMIP= .FALSE.
  Macsur= .FALSE.
  if(t%P_type_project.eq.2) AgMIP= .TRUE.
  if(t%P_type_project.eq.3) Macsur= .TRUE.

! DR 26/01/2016 je mets un code Flag_Agmpip_rap pour generaliser les ecritures car maintenant les protocoles
!  sont repris entre les differents pilots
! 1 : agMIP Wheat
! 2 : AGmip Wheat Giacomo (HSC)
! 3 : DR 29/12/2014 pour wheat Canopy temp
! 4 : 21/09/2014pour face_maize
! 5 : nouveau wheat 3
!
!code_ecrit_nom_usm=1
! ecriture des entetes
if(.not.macsur)then
    if(t%P_Flag_Agmip_rap.eq.1)then
! DR pour agMIP Wheat
    mes3000 = "('P_usm;wlieu;co2;ansemis;P_iwater;ancours;ifin;P_ichsl;group;P_codeplante;' &
          &'stade;mafruit;masec(flo);masec(mat);laimax;iflos;imats;QLES;drat;Qnplante(flo);Qnplante(mat);' &
          &'QNgrain;chargefruit;cet;N_mineralisation;N_volatilisation;cum_immob;QNdenit;resmes;azomes;ruisselt;cep')"
    else if(t%P_Flag_Agmip_rap.eq.2)then
! 28/01/2013 POur AGmip Wheat Giacomo (HSC)
    mes3000 = "('P_usm;wlieu;co2;ansemis;P_iwater;ancours;ifin;P_ichsl;group;P_codeplante;stade;' &
          &'mafruit;iflos;imats;chargefruit;masec(flo);masec(mat);laimax;drat;cet;resmes;ruisselt;cep;' &
          &'Qnplante(flo);Qnplante(mat);QLES;QNgrain;N_mineralisation;N_volatilisation;cum_immob;azomes;QNdenit;' &
          &'nlev;nbfeuille;ceo')"
    else  if(t%P_Flag_Agmip_rap.eq.3)then
    ! DR 29/12/2014 pour wheat Canopy temp
    mes3000 = "('P_usm;' &
          &'mafruit;iflos;imats;chargefruit;masec(flo);masec(mat);laimax;drat;cet;SoilAvW;ruisselt;cep;' &
          &'Qnplante(flo);Qnplante(mat);Qles;QNgrain;N_mineralisation;N_volatilisation;cum_immob;azomes;QNdenit;' &
          &'nlev;nbfeuille;ceo;tcultmoy,tcultmax')"
    else if(t%P_Flag_Agmip_rap.eq.4)then
! 21/09/2014pour face_maize
    mes3000 = "('STICS;treatment;ancours;'    &
          &'mafruit;masecflo; masecmat;laimax;cumraint;iflos;imats;cetm;ces;cep;ruisselt;drat;'            &
          &'QNplanteflo;QNplantemat;QNgrain;chargefruit')"
    else
!!    28/01/2015 pour le wheat phase 3  !!  code=5
    mes3000 = "('ST;P_usm;ansemis;' &
          &'mafruit;iflos;imats;chargefruit;masec(flo);masec(mat);laimax;drat;cet;resmes;ruisselt;cep;' &
          &'Qnplante(flo);Qnplante(mat);QLES;QNgrain;N_mineralisation;N_volatilisation;cum_immob;azomes;QNdenit;' &
          &'nlev;nbfeuille;ceo;iplts;ircarb')"
   endif
else
    mes3000 = "('ST;ancours;irecs;treatment;'    &
          &'mafruit;iflos;imats;chargefruit;masecflo; masecmat;laimax;'            &
          &'drat;cet;SoilAvW;ruissel;cep;QNplanteflo;QNplantemat;QLES;QNgrain;N_mineralisation;'&
          &'N_volatilisation;Qminr;SoilN;QNdenit;ilevs;haun=a;cetm;Corg=na;Crop')"
endif

      if(sta%P_codeclichange==2)then
        commentCC='CO2'
      else
        commentCC='noCO2'
      endif
!!!======================== on demarre la lecture/ecriture des entetes =============================================!!!
if (macsur)then ! Macsur !   1
!*** DR 11/08/2011 on ecrit les entetes une seule fois
!   et on met des , entre noms des varaibles de la ligne d'entete , faire plus propre avec separateurrapport plus tard
    ! DR 29/12/09 il ne faut ecrire l'entete que le premier jour de simulation
    if (sc%codeenteterap_agmip == 1 ) then
        open(1,file='Summary_template_CropM_Rotationeffect.txt')
        nbligne_entete=0
        do j=1,10
            read(1,'(a205)',end=999)ligne
            if (j.eq.6.or.j.eq.7) then
               write(p%ficrap_AgMIP,'(a205)')ligne
               write(*,'(a205)')ligne
            endif
            nbligne_entete=nbligne_entete+1
        enddo
   999  continue
        close(999)
    endif
else  ! AgMIP !   1
     if (sc%codeenteterap_agmip == 1 ) then !2
       if (t%P_Flag_Agmip_rap.eq.5)then  !3
           open(1,file='Template-Summary.txt')
           do j=1,3
             read(1,'(a300)',end=998)ligne
             write(p%ficrap_AgMIP,'(a300)')ligne
           enddo
           ! ligne 4 ajouter le simulation
           ii=index(sc%P_usm,'-')
           scenario= sc%P_usm(ii+1:ii+2)
           co2 = sc%P_usm(ii+4:ii+6)
           treatment=sc%P_usm(1:ii-1)
           read(1,'(a30)',end=998)ligne
           write(p%ficrap_AgMIP,'(a10,a10,a3)')trim(ligne),trim(scenario),trim(co2)
           ! ligne 5 ajouter le treatment
           read(1,'(a30)',end=998)ligne
           write(p%ficrap_AgMIP,'(a10,a30)')trim(ligne),trim(treatment)
           do j=6,7
             read(1,'(a300)',end=998)ligne
             write(p%ficrap_AgMIP,'(a300)')ligne
           enddo
           read(1,*)
           read(1,*)sc%code_ecrit_nom_usm
           close(1)
       else    !3
         if (t%P_Flag_Agmip_rap.eq.6)then   !4
            open(1,file='entete_summary.txt')
            do j=1,8
              read(1,'(a300)',end=998)ligne
              write(p%ficrap_AgMIP,'(a300)')ligne
            enddo
         else    !4
         !open(1,file='Stics_Phase1_final.csv')
            open(1,file='entete_summary.txt')
            do j=1,9
              read(1,'(a300)',end=998)ligne
              write(p%ficrap_AgMIP,'(a300)')ligne
            enddo
         endif    !4
       endif   !3
 !      close(1)
     else    !2
        if(sc%numcult.eq.1)write(p%ficrap_AgMIP,mes3000)
     endif   !2
998 continue
! 20/12/2013 dr on enleve la ligne d'entete à nous pour ne laisser que celle des rapports
!        write(p%ficrap_AgMIP,mes3000)
!*** DR 11/08/2011 on ecrit les entetes une seule fois
    sc%codeenteterap_agmip=0
endif !1
! fin des histoires d'entete
      if (pg%P_codeseprapport == 2) then
        sep = pg%P_separateurrapport
      else
        sep = ' '
      endif

    ! actualisation des numéros de jour
      if (p%P_codeplante == 'snu') then
        !--p%nrec = n
        p%nlax = sc%n
        p%nst1 = 1
        p%nst2 = 1
      endif

      nstt1 = max(p%nst1,1)
      nstt2 = max(p%nst2,1)

    ! calcul du rendement, des quantités H20 et N sur l'ensemble du profil
    ! domi - 5.0 9/11/00 - rdt calcule meme avant recolte
      rdt      = p%magrain(sc%aoas,sc%n)/100.
      sc%QH2Of = 0.
      QNF      = 0.
      QNF = sum(soil%AZnit(1:5))
      sc%QH2Of = sum(sc%hur(1:int(soil%profsol)))

    ! date courante: jour (ifin), année (ancours)
      ifin = sc%n + sc%P_iwater - 1 ! TODO : remplacer par sc%jul ?
      ancours = sc%annee(ifin)
      if (ifin > sc%nbjsemis) ifin = ifin - sc%nbjsemis

    ! Ecriture des noms des stades
    ! DR 08/09/06 j'initialise commentcelia
      commentcelia = 'date'
    ! DR 11/05/06 pour celia ecriture des stades
      if (sc%n == p%nplt) commentcelia = 'semis'
!  if (n == maxwth.and.P_datefin)commentcelia = 'fin'
!     DR 08/09/06 je rajoute les noms des stades dans le cas ou on
!     ecrit dans le rapport à des stades
      if (sc%codetyperap == 2 .or. sc%codetyperap == 3) then
        if (sc%n == p%nger) commentcelia = 'ger'
        if (sc%n == p%nlev) commentcelia = 'lev'
        if (sc%n == p%ndrp) commentcelia = 'drp'
        if (sc%n == p%nlax) commentcelia = 'lax'
        if (sc%n == p%nflo) commentcelia = 'flo'
        if (sc%n == p%nsen) commentcelia = 'sen'
        if (sc%n == p%namf) commentcelia = 'amf'
! dr brazil oubli de nmat 05/08/2011
        if (sc%n == p%nmat) commentcelia = 'mat'
      endif

      if (sc%n == p%nrec) then
        if (sc%recolte1) then
          commentcelia = 'recphy'
          sc%recolte1 = .FALSE.
        else
          commentcelia = 'rectec'
        endif
      endif

      if (p%nrec == p%nrecbutoir) commentcelia = 'recbut'

    ! DR 30/04/08 on le met ici pour que ce soit la fin qui soit identifiée sinon
    ! il met 2 fois recbut
      if (sc%n == sc%maxwth .and. sc%P_datefin) commentcelia = 'fin'
! DR pour AGMIP wheat pilot et maize pilot activer plus haut
!    mes3000 = "('P_usm;wlieu;co2;ansemis;P_iwater;ancours;ifin;P_ichsl;group;P_codeplante;' &
!          &'stade;mafruit;masec(flo);masec(mat);laimax;iflos;imats;QLES;drat;Qnplante(flo);Qnplante(mat);' &
!          &'QNgrain;chargefruit;cet;N_mineralisation;N_volatilisation;cum_immob;QNdenit;resmes;azomes;ruisselt;cep')"


!888   format('ST ; ',1(i5,a1),f10.3,a1,a3,a1,22(f10.3,a1),'na; ',f10.3,a1,'na;',a3)

! dr 29/12/2014 j'ajoute les temp cult moy pour Giacomo
    tcultmoy_cycle= p%ctcult/(p%nrec - p%nplt+1)
    tcultmax_cycle= p%ctcultmax/(p%nrec - p%nplt+1)
!!!================ on attaque les ecriture en ligne ===========================================!!!
!! DR pour AGMIP wheat pilot et maize pilot
if(macsur)then

    sc%valsortie_mat(1)= p%irecs
    long_usm=len_trim(sc%P_usm)
    nom_plante=trim(sc%P_usm(long_usm-2:long_usm+1))
!DR 07/05/2014 je rajoute le recalcul des dates pour que les dates de stades ne soient pas superieures à 365 ou 366
! mat
    if (sc%valsortie_mat(1).gt.sc%nbjsemis )sc%valsortie_mat(1)= sc%valsortie_mat(1) - sc%nbjsemis
    if (sc%valsortie_mat(3).gt.sc%nbjsemis )sc%valsortie_mat(3)= sc%valsortie_mat(3) - sc%nbjsemis
    if (sc%valsortie_mat(4).gt.sc%nbjsemis )sc%valsortie_mat(4)= sc%valsortie_mat(4) - sc%nbjsemis
! si c'est une bettearve ou un raygras on met pas les dates de flo
    if (nom_plante.eq.'GRV'.or.nom_plante.eq.'SBT') sc%valsortie_mat(3) = 0


            write(p%ficrap_AgMIP,888)             &
!                                commentCC,sep,sc%ansemis,sep,ancours,sep,                  &
                                ancours,sep,sc%valsortie_mat(1),sep,sc%P_usm(5:6),sep,               &! irec
                                sc%valsortie_mat(2),sep,                                             &! mafruit
                                sc%valsortie_mat(3),sep,sc%valsortie_mat(4),sep,                     &! flo,mat
                                sc%valsortie_mat(5),sep,                                             &! chargefruit
                                sc%valsortie_flo(6),sep,sc%valsortie_mat(6),sep,                     &! masec à flo,mat
                                sc%valsortie_mat(7),sep,                                             &! laimax
                                sc%valsortie_mat(8)-sc%valsortie_iplt(8),sep,                                              &! drat
                                sc%valsortie_mat(9)-sc%valsortie_iplt(9),sep,                                              &! cet
                                sc%valsortie_mat(10)-sc%valsortie_iplt(10),sep,                                            &! resmes
                                sc%valsortie_mat(11)-sc%valsortie_iplt(11),sep,                                            &! ruissel
                                sc%valsortie_mat(12)-sc%valsortie_iplt(12),sep,                                            &! cep
                                sc%valsortie_flo(13),sep,sc%valsortie_mat(13),sep,                   &! qnplante
                                sc%valsortie_mat(14)-sc%valsortie_iplt(14),sep,                                            &! qles
                                sc%valsortie_mat(15),sep,                                            &! QNgrain
!! ATTENTION DR 04/09/2012 N_mineralisation est calculé directement par qminh+qminr, N_volatilisation est calcule directement par qnvolorg+qnvoleng
                                sc%valsortie_mat(16)-sc%valsortie_iplt(16),sep,                                            &! N_mineralisation
                                sc%valsortie_mat(17)-sc%valsortie_iplt(17),sep,                                            &! N_volatilisation
                                -sc%valsortie_mat(18)+sc%valsortie_iplt(18),sep,                                           &! Nimmo=qminr
                                sc%valsortie_mat(19)-sc%valsortie_iplt(19),sep,                                            &! SoilN=ammomes+azomes
                                sc%valsortie_mat(20)-sc%valsortie_iplt(20),sep,                                            &! Qndenit
                                sc%valsortie_mat(21),sep,                                            &! ilev
                                sc%valsortie_mat(23)-sc%valsortie_iplt(23) ,sep,                     &! cetm
!                                trim(sc%P_usm(12:14))
                                nom_plante

!888   format('ST ; ',1(i5,a1),f10.3,a1,a3,a1,22(f10.3,a1),'na; ',f10.3,a1,'na;',a3)

! fin ecriture macsur
else
! DR 26/01/2016 je mets un code Flag_Agmpip_rap pour generaliser les ecritures car maintenant les protocoles
!  sont repris entre les differents pilots
! 1 : agMIP Wheat
! 2 : AGmip Wheat Giacomo (HSC)
! 3 : DR 29/12/2014 pour wheat Canopy temp
! 4 : 21/09/2014pour face_maize

   if(t%P_Flag_Agmip_rap.eq.4)then
! DR 21/09/2014 pour maize_face
!             write(p%ficrap_AgMIP,888)             &
             write(p%ficrap_AgMIP,*)'STICS ;',            &
                                sc%P_usm(1:9),sep,ancours,sep,               &!
                                sc%valsortie_mat(1),sep,                                             &! mafruit
                                sc%valsortie_flo(2),sep,sc%valsortie_mat(2),sep,                     &! masec à flo,mat
                                sc%valsortie_mat(3),sep,                                             &! laimax
                                sc%valsortie_mat(4),sep,                                             &! cumraint
                                sc%valsortie_mat(5),sep,sc%valsortie_mat(6),sep,                     &! flo,mat
                                sc%valsortie_mat(7),sep,                                             &! cetm
                                sc%valsortie_mat(8)-sc%valsortie_iplt(8),sep,                        &! ces
                                sc%valsortie_mat(9),sep,                                             &! cep
                                sc%valsortie_mat(10)-sc%valsortie_iplt(10),sep,                      &! ruissel
                                sc%valsortie_mat(11)-sc%valsortie_iplt(11),sep,                      &! drat
                                sc%valsortie_flo(12),sep,sc%valsortie_mat(12),sep,                   &! qnplante à flo,mat
                                sc%valsortie_mat(13),sep,                                            &! QNgrain
                                sc%valsortie_mat(14),sep,                                            &! chargefruit
                                (sc%valsortie_mat(i),sep,i=15,sc%nbvarrap)                          ! les autres
    endif
    if(t%P_Flag_Agmip_rap.eq.2)then ! regarder à quoi ca correspond
! dr le 12/04/2013 je rajoute les 3 variables à Giacomo
! dr 29/12/2014 je recalcule les dates
        an_iplt=sc%annee(p%iplts)
        call julien(p%iplts,sc%annee(p%iplts),mois,jour_iplt,nummois_iplt)
        charmois_iplt=chiffre(nummois_iplt)
        charjour_iplt=chiffre(jour_iplt)
        an_ilev=sc%annee(p%ilevs)
        jour=p%ilevs
        if (p%ilevs > sc%nbjsemis)jour = p%ilevs - sc%nbjsemis
        call julien(jour,sc%annee(p%ilevs),mois,jour_ilev,nummois_ilev)
        charmois_ilev=chiffre(nummois_ilev)
        charjour_ilev=chiffre(jour_ilev)
        an_iflo=sc%annee(p%iflos)
        jour=p%iflos
        if (p%iflos > sc%nbjsemis) jour = p%iflos - sc%nbjsemis
        call julien(jour,sc%annee(p%iflos),mois,jour_iflo,nummois_iflo)
        charmois_iflo=chiffre(nummois_iflo)
        charjour_iflo=chiffre(jour_iflo)
        an_imat=sc%annee(p%imats)
        jour=p%imats
        if (p%imats > sc%nbjsemis) jour = p%imats - sc%nbjsemis
        call julien(jour,sc%annee(p%imats),mois,jour_imat,nummois_imat)
        charmois_imat=chiffre(nummois_imat)
        charjour_imat=chiffre(jour_imat)
        write(p%ficrap_AgMIP,888)'ST',sep,sc%P_usm,sep,                                           &
                                an_iplt,charmois_iplt,charjour_iplt,sep,                                     &! semis
                                sc%valsortie_mat(1),sep,                                             &! mafruit
                                an_iflo,charmois_iflo,charjour_iflo,sep,                                     &! flo
                                an_imat,charmois_imat,charjour_imat,sep,                                     &! mat
                                sc%valsortie_mat(2),sep,                                            &! chargefruit,
                                sc%valsortie_flo(3),sep,sc%valsortie_mat(3),sep,                     &! masec a flo et mat
                                sc%valsortie_mat(4),sep,                                             &! laimax
                                sc%valsortie_mat(5),sep,                                             &!  drat
                                sc%valsortie_mat(6),sep,                                            &! cet,
                                sc%valsortie_mat(7),sep,                                            &! resmes
                                sc%valsortie_mat(8),sep,sc%valsortie_mat(9),sep,                   &! ruisselt,cep
                                sc%valsortie_flo(10),sep,sc%valsortie_mat(10),sep,                     &! qnplante a flo et mat
                                sc%valsortie_mat(11),sep,                                             &! qles
                                sc%valsortie_mat(12),sep,                                             &! qngrain
                                sc%valsortie_mat(13),sep,sc%valsortie_mat(14),sep,                   &! N_mineralisation,N_volatilisation
                                sc%valsortie_mat(15),sep,                                            &! cum_immob
                                sc%valsortie_mat(16),sep,                                            &! azomes
                                sc%valsortie_mat(17),sep,                                            &!  QNdenit
                                an_ilev,charmois_ilev,charjour_ilev,sep,                                  &!  nlev
                                sc%valsortie_mat(18),sep,                                            &!  nbfeuille
                                sc%valsortie_mat(19),sep,                                            &!  eo
                                !'na',sep,                                                             &!  eo
                                tcultmoy_cycle,sep,                                                  &!  tcultmoy_cycle
                                tcultmax_cycle                                                       !  tcultmax_cycle
!888   format(a2,a1,a5,a1,(i4,a2,a2),a1,f10.3,a1,(i4,a2,a2),a1,(i4,a2,a2),a1,18(f10.3,a1),(i4,a2,a2),a1,18(f10.3,a1))
    endif
    if(t%P_Flag_Agmip_rap.eq.5)then
   ! DR 26/01/2016 je recalule les dates en numero de jour dans l'annee
        an_iplt=sc%annee(p%iplts)
        an_imat=sc%annee(p%imats)
        if (sc%valsortie_mat(20).gt.sc%nbjsemis )sc%valsortie_mat(20)= sc%valsortie_mat(20) - sc%nbjsemis     !lev
        if (sc%valsortie_mat(2).gt.sc%nbjsemis )sc%valsortie_mat(2)= sc%valsortie_mat(2) - sc%nbjsemis        ! flo
        if (sc%valsortie_mat(3).gt.sc%nbjsemis )sc%valsortie_mat(3)= sc%valsortie_mat(3) - sc%nbjsemis        ! mat

          if(sc%code_ecrit_nom_usm.eq.0)then
               write(p%ficrap_AgMIP,888)sc%P_usm,sep,'ST',sep,                                           &
                                an_imat,sep,                                          &! an recolte
                                sc%valsortie_mat(1),sep,                                             &! mafruit
                                int(sc%valsortie_mat(2)),sep,                                  &! flo
                                int(sc%valsortie_mat(3)),sep,                                     &! mat
                                sc%valsortie_mat(4),sep,                                            &! chargefruit,
                                sc%valsortie_flo(5),sep,sc%valsortie_mat(5),sep,                     &! masec a flo et mat
                                sc%valsortie_mat(6),sep,                                             &! laimax
                                sc%valsortie_mat(7)-sc%valsortie_iplt(7),sep,                            &!  drat
                                sc%valsortie_mat(8),sep,                                            &! cet,
                                sc%valsortie_mat(9),sep,                                            &! resmes
                                sc%valsortie_mat(10)-sc%valsortie_iplt(10),sep,                  &! ruisselt
                                sc%valsortie_mat(11),sep,                   &! cep
                                sc%valsortie_flo(12),sep,sc%valsortie_mat(12),sep,                     &! qnplante a flo et mat
                                sc%valsortie_mat(13),sep,                                             &! qles
                                sc%valsortie_mat(14),sep,                                             &! qngrain
                                sc%valsortie_mat(15),sep,sc%valsortie_mat(16),sep,                   &! N_mineralisation,N_volatilisation
                                sc%valsortie_mat(17),sep,                                            &! cum_immob
                                sc%valsortie_mat(18),sep,                                            &! azomes
                                sc%valsortie_mat(19),sep,                                            &!  QNdenit
                                int(sc%valsortie_mat(20)),sep,                                  &!  nlev
                                sc%valsortie_mat(21),sep,                                            &!  nbfeuille
                                sc%valsortie_mat(22),sep,                                        &!  eop+eos
                                int(sc%valsortie_mat(23)), &                                     ! date de semis
                                (sep,sc%valsortie_mat(i),i=24,sc%nbvarrap)
888   format(a25,a1,a2,a1,(i4),a1,f10.3,a1,(i4),a1,(i4),a1,18(f10.3,a1),(i4),a1,2(f10.3,a1),(i4),5(a1,f10.3))
          else
             write(p%ficrap_AgMIP,777)'ST',sep,                                           &
                                an_imat,sep,                                          &! an recolte
                                sc%valsortie_mat(1),sep,                                             &! mafruit
                                int(sc%valsortie_mat(2)),sep,                                  &! flo
                                int(sc%valsortie_mat(3)),sep,                                     &! mat
                                sc%valsortie_mat(4),sep,                                            &! chargefruit,
                                sc%valsortie_flo(5),sep,sc%valsortie_mat(5),sep,                     &! masec a flo et mat
                                sc%valsortie_mat(6),sep,                                             &! laimax
                                sc%valsortie_mat(7)-sc%valsortie_iplt(7),sep,                            &!  drat
                                sc%valsortie_mat(8),sep,                                            &! cet,
                                sc%valsortie_mat(9),sep,                                            &! resmes
                                sc%valsortie_mat(10)-sc%valsortie_iplt(10),sep,                  &! ruisselt
                                sc%valsortie_mat(11),sep,                   &! cep
                                sc%valsortie_flo(12),sep,sc%valsortie_mat(12),sep,                     &! qnplante a flo et mat
                                sc%valsortie_mat(13),sep,                                             &! qles
                                sc%valsortie_mat(14),sep,                                             &! qngrain
                                sc%valsortie_mat(15),sep,sc%valsortie_mat(16),sep,                   &! N_mineralisation,N_volatilisation
                                sc%valsortie_mat(17),sep,                                            &! cum_immob
                                sc%valsortie_mat(18),sep,                                            &! azomes
                                sc%valsortie_mat(19),sep,                                            &!  QNdenit
                                int(sc%valsortie_mat(20)),sep,                                  &!  nlev
                                sc%valsortie_mat(21),sep,                                            &!  nbfeuille
                                sc%valsortie_mat(22),sep,                                                      &!  eop+eos
                                int(sc%valsortie_mat(23)), &                                           ! date de semis
                                (sep,sc%valsortie_mat(i),i=24,sc%nbvarrap)
777   format(a2,a1,(i4),a1,f10.3,a1,(i4),a1,(i4),a1,18(f10.3,a1),(i4),a1,2(f10.3,a1),(i4),5(a1,f10.3))
        endif
     endif
! DR 19/02/2016 pour ET
     if(t%P_Flag_Agmip_rap.eq.6)then
        an_imat=sc%annee(p%imats)
             write(p%ficrap_AgMIP,979)trim(sc%model_name),sep,trim(sc%info_level),sep ,                         &
                                sc%P_usm,sep,'Maize',sep,                                      &
                                an_imat,sep,                                                   &! an recolte
                                sc%valsortie_mat(1),sep,                                       &! mafruit
                                'na',sep,                                                      &
                                sc%valsortie_flo(2),sep,sc%valsortie_mat(2),sep,               &! masec a flo et mat
                                sc%valsortie_mat(3),sep,                                       &! laimax
                                sc%valsortie_mat(4),sep,                                       &! cumraint,
                                int(sc%valsortie_mat(5)),sep,                                  &! flo
                                int(sc%valsortie_mat(6)),sep,                                  &! mat
                                sc%valsortie_mat(7),sep,                                       &! cetp
                                sc%valsortie_mat(8),sep,                                       &! cum_et0
                                sc%valsortie_mat(9),sep,                                       &! cep
                                sc%valsortie_mat(10),sep,                                      &! ces
                                sc%valsortie_mat(11),sep,                                      &! cet
                                sc%valsortie_mat(12),sep,                                      &! runoff_from_plt
                                sc%valsortie_mat(13),sep,                                      &! drain_from_plt
                                'na',sep,                                                      &
                                sc%valsortie_flo(14),sep,sc%valsortie_mat(14),sep,             &! qnplante a flo et mat
                                sc%valsortie_mat(15),sep,                                      &! qngrain
                                sc%valsortie_mat(16)                                            ! chargefruit,
!979   format(a4,a1,a13,a1,a15,a1,a5,a1,i4,5(a1,f10.3),2(a1,i4),18(a1,f10.3))
979   format(a4,a1,a13,a1,a15,a1,a5,a1,i4,1(a1,f10.3),a1,a3,4(a1,f10.3),2(a1,i4),&
             & 7(a1,f10.3),a1,a3,18(a1,f10.3))

     endif


! DR 26/01/2016  a nettoyer
!888   format(a2,a1,a2,a1,1(i4,2a2,a1),f10.3,a1,2(i4,2a2,a1),18(f10.3,a1),i4,2a2,a1,1(f10.3,a1),a4,a1,18(f10.3,a1))
!888   format(2(a40,a1),a10,a1,6(i5,a1),a3,a1,a10,a1,100(f10.3,a1))
!888  format(2(a40,a1),a10,a1,6(i5,a1),a3,a1,a10,a1,f10.3,a1,2(i5,a1),50(f10.3,a1))
!889   format(2(a40,a1),6(i5,a1),a3,a1,a10,a1,100(e10.3,a1))

endif

return
end subroutine Ecriture_Rapport_agmip
 
 
