! Lecture des variables du fichier Rapport (rapport.sti)
! On va lire dans rap.mod la liste des variables dont
! on souhaite avoir les valeurs écrites dans le fichier rapport.sti
! D'abord on récupérer les dates et/ou les stades pour lesquels
! on veut ces valeurs, puis on lit le nom des variables souhaitées.
! Read variables File Report (rapport.sti)
!! We will read rap.mod the list of variables which
!! one wishes to have the values ​​written to the file rapport.sti
!!
!! First we get the dates and / or stages for which
!! we want these values​​, then we read the name of the desired variables.
subroutine Lecture_VariablesRapport(sc)

    USE Stics

    implicit none

    type(Stics_Communs_), intent(INOUT) :: sc  


    integer :: ii  

! 17/02/2010 on ajoute la declaration de la date sous sa nouvelle forme
    integer :: jourdebutprof  !  
    integer :: moisdebutprof  !  
    integer :: andebutprof  

    ! on ouvre le fichier rap.mod
      open(33,file='rap.mod')

    ! on initialise les variables à FAUX
      sc%rapplt = .FALSE.
      sc%rapger = .FALSE.
      sc%raplev = .FALSE.
      sc%raplax = .FALSE.
      sc%rapflo = .FALSE.
      sc%rapdrp = .FALSE.
      sc%rapsen = .FALSE.
      sc%raprec = .FALSE.
      sc%rapamf = .FALSE.
      sc%rapfin = .FALSE.
    ! DR 30/07/08 3 stades ajoutés
      sc%rapdebdes = .FALSE.
      sc%rapdebdebour = .FALSE.
      sc%rapmat = .FALSE.
    ! DR 12/08/08 2 stades ajoutés
      sc%rapdebdorm = .FALSE.
      sc%rapfindorm = .FALSE.


    ! Lecture codeentete ou pas

      read(33,*,end=300) sc%codeaucun
      read(33,*,end=300) sc%codeenteterap
      read(33,*,end=300) sc%codetyperap

! domi 07/10/03 pour Vianney on peut lire des dates et des stades
! si codetyperap = 1 --> dates
! si codetyperap = 2 --> stades
! si codetyperap = 3 --> dates puis stades

!  --      if(codetyperap.eq.1)then
! domi - 17/10/03 - nb de stades

    ! on lit des dates
      if (sc%codetyperap == 1 .or. sc%codetyperap == 3) then

      ! on lit le nombre de date à récupérer
        read(33,*,end=300) sc%nboccurrap
      ! on borne ce nombre par la taille max du tableau
        sc%nboccurrap = min(sc%nboccurrap,SIZE(sc%daterap))
        do ii = 1,sc%nboccurrap
        !--read(33,*) sc%daterap(ii)
        ! DR 17/02/2010 maintenant javastics fournit des dates sous la forme an mois jour
          read(33,*,end=300)sc%date_calend_rap(ii,3),sc%date_calend_rap(ii,2),sc%date_calend_rap(ii,1)
        ! on recalcule le jour julien
!          call NDATE (jourdebutprof,moisdebutprof,andebutprof,sc%daterap(ii))
        ! DR 10/03/2014 bon ca avait ete fait un peu vite , y'a pb  dans le cas ou l'usms est sur 2 ans ,
        !  du coup le jour julien de la date de la deuxieme annee est mauvais
        ! comme a pas acces � l'annee ni � quoi que ce soit � ce stade du programme , je vais essayer de trimbaler plutot
        ! les dates calendaires et les transformer avant l'appel � l'ecriture du rapport
!        if(sc%ansemis.ne.andebutprof)then
!            if(isBissextile(sc%ansemis)) sc%daterap(ii)= sc%daterap(ii)+366
!            if(isBissextile(sc%ansemis)) sc%daterap(ii)= sc%daterap(ii)+365
!        endif

        end do

      endif

    ! on lit des stades
      if (sc%codetyperap == 2 .or. sc%codetyperap == 3) then

      ! on lit le nombre de stades à récupérer
        read(33,*,end=300) sc%nboccurstd
      ! on borne ce nombre par la taille max du tableau
        sc%nboccurstd = min(sc%nboccurstd,SIZE(sc%staderap))

      ! on lit nboccurstd stades
        do ii = 1,sc%nboccurstd

          read(33,'(a9)',end=913) sc%staderap(ii)

        ! en fonction des stades lus, on met à VRAI les booléens correspondants
! DR 29/12/09 javastics donne plt et non nplt
!         if (sc%staderap(ii) == 'nplt') sc%rapplt=.TRUE.
          if (sc%staderap(ii) == 'plt') sc%rapplt=.TRUE.
        ! DR 08/01/07 pour Sophie on rajoute l'ecriture à la germination
          if (sc%staderap(ii) == 'ger') sc%rapger=.TRUE.
          if (sc%staderap(ii) == 'lev') sc%raplev=.TRUE.
          if (sc%staderap(ii) == 'amf') sc%rapamf=.TRUE.
          if (sc%staderap(ii) == 'lax') sc%raplax=.TRUE.
          if (sc%staderap(ii) == 'flo') sc%rapflo=.TRUE.
          if (sc%staderap(ii) == 'drp') sc%rapdrp=.TRUE.
          if (sc%staderap(ii) == 'sen') sc%rapsen=.TRUE.
          if (sc%staderap(ii) == 'rec') sc%raprec=.TRUE.
          if (sc%staderap(ii) == 'fin') sc%rapfin=.TRUE.

        ! DR 30/07/08 3 stades ont ete rajoutés
          if (sc%staderap(ii) == 'mat') sc%rapmat=.TRUE.
          if (sc%staderap(ii) == 'debdes') sc%rapdebdes=.TRUE.
          if (sc%staderap(ii) == 'debdebour') sc%rapdebdebour=.TRUE.

        ! DR 12/08/08 2 stades ajoutés
          if (sc%staderap(ii) == 'findorm') sc%rapfindorm=.TRUE.
          if (sc%staderap(ii) == 'debdorm') sc%rapdebdorm=.TRUE.
          if (sc%staderap(ii) == 'start') sc%rapdeb=.TRUE.

        end do

      endif

913   continue

    ! on lit le nom des variables dont on veut écrire la valeur dans le fichier Rapport.
      do ii = 1,100
        read(33,'(a29)',end=299) sc%valrap(ii)
      end do
    ! on a lu le fichier jusqu'à sa fin, on calcul le nombre de variables lues pour le stocker.
299   sc%nbvarrap = ii-1

300   close(33)

! dr 11/03/2013 j'ajoute la variable   sc%codeenteterap_agmip
        sc%codeenteterap_agmip = sc%codeenteterap

return
end subroutine Lecture_VariablesRapport
 
 
