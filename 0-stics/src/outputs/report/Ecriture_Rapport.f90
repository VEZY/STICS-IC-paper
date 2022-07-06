! ***************************************************** c
! * modif Bruno  20/04/99                              * c
! * écriture d'un rapport final en fin de simulation  * c
! * + éventuellement à 2 autres dates                 * c
! ***************************************************** c
! Writing a final report at the end of simulation (file mod_rapport.sti)
!subroutine Ecriture_Rapport(sc,pg,soil,c,sta,p,itk,t) ! DR 19/07/2012 on supprime itk qui ne sert pas
subroutine Ecriture_Rapport(sc,pg,soil,c,sta,p,t)

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

  type(Climat_),              intent(INOUT) :: c  

  type(Station_),             intent(INOUT) :: sta  

  type(Plante_),              intent(INOUT) :: p  

!  type(ITK_),                 intent(INOUT) :: itk

  type(Stics_Transit_),       intent(INOUT) :: t  

    ! variables locales pour l'optimisation
    integer,parameter :: nb_parameters_max = 300
    integer           :: nbpar  !
    real    :: valparopt(nb_parameters_max)
    type(varying_string) :: nompar (nb_parameters_max)


! Variables locales

      integer           :: ancours  !  
      integer           :: ifin  !  
      integer           :: k  !  
      integer           :: i  
      real              :: nstt1  !  
      real              :: QNF  !  
      real              :: rdt  
      ! PB - je le rajoute en tant que variable locale
      real              :: nstt2  

!      real              :: valparopt(5)

      integer           :: numpar  
      character         :: sep  
      character(len=10) :: commentcelia  
      integer           :: kk


!*** DR 11/08/2011 on ecrit les entetes une seule fois
!   et on met des , entre noms des varaibles de la ligne d'entete , faire plus propre avec separateurrapport plus tard
    ! DR 29/12/09 il ne faut ecrire l'entete que le premier jour de simulation
      if (sc%codeenteterap == 1 ) then
        write(p%ficrap,223)
223     format('P_usm;wlieu;ansemis;P_iwater;ancours;ifin;P_ichsl;group;P_codeplante;stade;nomversion',$)
        do i = 1, sc%nbvarrap
          !--read(10,'(a19)',end=998)nomsti(i)
          write(p%ficrap,221) trim(sc%valrap(i))
        end do
        write(p%ficrap,'(1x)')
221     format(';',A,$)
      endif
      sc%codeenteterap=0
!*** DR 11/08/2011 on ecrit les entetes une seule fois

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

! --    if (n.gt.p%nrec.and.p%nrec.gt.0) then
! --      nmasec = p%nrec
! --      nlaic = p%nlax
! --    else
! --      nmasec = n
! --      nlaic = n
! --    endif

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

      call CorrespondanceVariablesDeSorties(sc,p,soil,c,sta,          &  ! DR 19/07/2012 on supprime itk qui ne sert pas 20/09/2012 t aussi
                                            sc%nbvarrap,                    &
                                            sc%valrap(1:sc%nbvarrap),       &
                                            sc%valsortierap(1:sc%nbvarrap))


    ! Ecriture des noms des stades
    ! DR 08/09/06 j'initialise commentcelia
      commentcelia = 'date'
    ! DR 11/05/06 pour celia ecriture des stades

      if (sc%n == p%nplt.and.sc%rapplt) commentcelia = 'semis'
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
        ! DR 22/07/2013 j'ajoute le stade start pour Nicolas
        if (sc%n == 1.and.sc%rapdeb.and.sc%start_rap) commentcelia = 'start'

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
    ! DR 22/07/2013 je remplace 'fin' par 'end'
      if (sc%n == sc%maxwth .and. sc%P_datefin) commentcelia = 'end'


    ! DR
      if (pg%P_codesensibilite /= 1) then
! **************** cas courant pas analyse de sensibilite *************
! DR pour benj ecriture speciale pour l'instant je mets un P_codesensibilite = 3
        if (pg%P_codesensibilite == 3) then
          if (sc%codoptim == 0) then
            write(p%ficrap,889) sc%P_usm,sep,adjustl(sc%wlieu),sep,sc%ansemis,sep,sc%P_iwater,sep,ancours,sep,       &
                                ifin,sep,sc%P_ichsl,sep,p%group,sep,p%P_codeplante,sep,commentcelia,sep,    &
                                sc%codeversion,sep,&
                                (sc%valsortierap(k),sep,k = 1,sc%nbvarrap),                             &
! DR 14/09/07 pour benj on ajoute les dates d'apports N et eau à la fin du rapport
                                float(t%nbapN(p%ipl)),sep,(float(t%dateN(p%ipl,k)),sep,k = 1,5),        &
                                float(t%nbapirr(p%ipl)),sep,                                            &
                                (float(t%dateirr(p%ipl,k)),sep,k = 1,t%nbapirr(p%ipl))
          else

            open(28,file = 'param.sti')
            read(28,*) nbpar
            do k = 1,nbpar
              !read(28,*) numpar,valparopt(k)
            call get(28,nompar(k))
            read (28,*) valparopt(k)

            end do
            close (28)

        ! DR 13/01/06 on peut ecrire en expo dans sortie et dans rapport
            write(p%ficrap,889) sc%P_usm,sep,adjustl(sc%wlieu),sep,sc%ansemis,sep,sc%P_iwater,sep,ancours,sep,           &
                                ifin,sep,sc%P_ichsl,sep,p%group,sep,p%P_codeplante,sep,commentcelia,sep,        &
                                sc%codeversion,sep,&
                                (valparopt(k),sep,k = 1,nbpar),(sc%valsortierap(k),sep,k = 1,sc%nbvarrap),  &
        ! DR 14/09/07 pour benj on ajoute les dates d'apports N et eau à la fin du rapport
                                float(t%nbapN(p%ipl)),sep,                                                  &
                                !--((float(t%dateN(p%ipl,k)),sep),k = 1,t%nbapN(p%ipl)),                   &
                                (float(t%dateN(p%ipl,k)),sep,k = 1,5),float(t%nbapirr(p%ipl)),sep,          &
                                (float(t%dateirr(p%ipl,k)),sep,k = 1,t%nbapirr(p%ipl))
          endif

        else

          if (pg%P_codeoutscient == 1) then ! ecriture scientifique

            write(p%ficrap,889) sc%P_usm,sep,adjustl(sc%wlieu),sep,sc%ansemis,sep,sc%P_iwater,sep,ancours,sep,       &
                                ifin,sep,sc%P_ichsl,sep,p%group,sep,p%P_codeplante,sep,commentcelia,sep,    &
                                sc%codeversion,sep,&
                                (sc%valsortierap(k),sep,k = 1,sc%nbvarrap)
          else
 !         write(*,*)'je vais ecrire dans ',p%ficrap, commentcelia
            write(p%ficrap,888) sc%P_usm,sep,sc%P_wdata1(1:index(sc%P_wdata1,'.')-1),sep,sc%ansemis,sep,    &
                                sc%P_iwater,sep,ancours,sep,       &
                                ifin,sep,sc%P_ichsl,sep,p%group,sep,p%P_codeplante,sep,commentcelia,sep,    &
                                sc%codeversion,sep,&
                                (sc%valsortierap(k),sep,k = 1,sc%nbvarrap)

! DR 14/09/2011 je cree un rapport.mod sepcifique agMIP
      if(p%nflo==sc%n )then
         do kk=1,sc%nbvarrap
             sc%valsortie_flo(kk)= sc%valsortierap(kk)
         enddo
!             write(*,*)'mafruit',sc%valsortie_flo(1)
      endif
      if(p%nmat==sc%n .or. (p%nrec==sc%n))then
      if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN)>0)write(*,*)'n',sc%n,'mat',p%nmat,'rec',p%nrec
         do kk=1,sc%nbvarrap
             sc%valsortie_mat(kk)= sc%valsortierap(kk)
         enddo
      endif
! DR 08/12/2013 pour macsur , j'ajoute les valeurs au semis
      if(p%nplt==sc%n )then
         do kk=1,sc%nbvarrap
             sc%valsortie_iplt(kk)= sc%valsortierap(kk)
         enddo
!             write(*,*)'mafruit',sc%valsortie_flo(1)
      endif



          endif
        endif
      else
! ********* dans le cas d'une analyse de sensibilité ecriture des parametres *********
        open(28,file = 'param.sti')
        read(28,*) nbpar
        do k = 1,nbpar
            ! read(28,*) numpar,valparopt(k)
            call get(28,nompar(k))
            read (28,*) valparopt(k)


        end do
        close (28)

      ! DR 13/01/06 on peut ecrire en expo dans sortie et dans rapport
        if (pg%P_codeoutscient == 1) then
          write(p%ficrap,889) sc%P_usm,sep,adjustl(sc%wlieu),sep,sc%ansemis,sep,sc%P_iwater,sep,ancours,sep,         &
                              ifin,sep,sc%P_ichsl,sep,p%group,sep,p%P_codeplante,sep,commentcelia,sep,      &
                              sc%codeversion,sep,&
                              (valparopt(k),sep,k = 1,nbpar),(sc%valsortierap(k),sep,k = 1,sc%nbvarrap)
        else
          write(p%ficrap,888) sc%P_usm,sep,adjustl(sc%wlieu),sep,sc%ansemis,sep,sc%P_iwater,sep,ancours,sep,         &
                              ifin,sep,sc%P_ichsl,sep,p%group,sep,p%P_codeplante,sep,commentcelia,sep,      &
                              sc%codeversion,sep,&
                              (valparopt(k),sep,k = 1,nbpar),(sc%valsortierap(k),sep,k = 1,sc%nbvarrap)
        endif

      endif

! Bruno: la notation scientifique en sortie doit être préférée !
! dr domi 170206 je rajoute un separateur au choix dans paramv6.par
!  888   format(2(1x,a7),6i5,100f8.2)
!  889   format(2(1x,a7),6i5,100e10.3)

888   format(2(a40,a1),6(i5,a1),a3,a1,a10,a1,a30,a1,200(f10.3,a1))
889   format(2(a40,a1),6(i5,a1),a3,a1,a10,a1,a30,a1,200(e10.3,a1))


! **  penser à voir avec nadine si on met les deux dans la liste des variables
! *-  ou si on fait un test sur codelai
! --  +         lai(nlaic)
! --  +         tauxcouv(nlaic)

    ! domi on rajoute un boleen comme quoi on est passé dans rapport pour ne pas y passer 2 fois
      sc%ecritrap = .TRUE.
      commentcelia = '   '
! dr 11/08/2011 à supprimer jusqu'au bout , l'enete a ete remonté au debut
    ! DR 29/12/09 il ne faut ecrire l'entete que le premier jour de simulation
      if (sc%codeenteterap == 1 .and. sc%n ==1) then
        write(p%ficrap,223)
!*****************************
! domi 30/04/04 pour CAROLINE on veux garder la valeur des parametres RESIDUS
!*****************************
!        if(itk%P_codedateappN == 1)then
!            do ii = 1,p%napS
!              write(ficrap,1224)
!            end do
!1224        format(1x,'P_Qres C/N Cres P_Nminres P_Eaures',$)
!        endif



!*****************************
        do i = 1, sc%nbvarrap
          !--read(10,'(a19)',end=998)nomsti(i)
          write(p%ficrap,221) trim(sc%valrap(i))
        end do
        write(p%ficrap,'(1x)')
      endif


return
end subroutine Ecriture_Rapport
 
 
