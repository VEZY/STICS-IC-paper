! reading the variables to write in the profile
!! profiles in prof.mod
subroutine Lecture_Profil(sc,p,soil,ipl)


USE Stics
USE Plante
USE Sol

  implicit none

  type(Stics_Communs_),       intent(INOUT) :: sc  

  type(Plante_),              intent(IN)    :: p  

  type(Sol_),                 intent(IN)    :: soil  

  integer,                    intent(IN)    :: ipl  

    integer :: nbjParAnnee

! les VARIABLES LOCALES
!      character(len=10) :: sc%nomvarprof(82) ! DR 17/02/2010 les variables de profil.csv sont codées sur 10 caractères
                                          ! j'ai corrigé aussi profil.csv pour qu'il soit coherent avce ce dimensionnement
      integer           :: freqprof  

    ! DR 23/12/09 on lit une date (jour/mois/an) et non plus un jour dans l'annee
      integer           :: jourdebutprof  !  
      integer           :: moisdebutprof  !  
      integer           :: andebutprof  
      integer           :: datedebutprof  
  ! DR 27/06/2013 on lit une date (jour/mois/an) et non plus un jour dans l'annee
      integer           :: jourprof  !
      integer           :: moisprof  !
      integer           :: anprof

      integer           :: j  !  
      integer           :: i  !  
      integer           :: kk  
!      character(len=15) :: fich
      integer           :: jour_courant
      integer           :: profsol_int
      integer           :: an1



       profsol_int=int(soil%profsol)
       !jour_courant=sc%n+sc%P_iwater+1 ! DR 01/04/2015
       jour_courant=sc%n+sc%P_iwater-1

    ! le profil
      if (sc%n == 1) then

        open(82,file='prof.mod')
        read(82,*,err=1999,end=1999) sc%codeprofil

      ! domi 30/09/05 y'avait un pb avec l'ecriture des profils à des frequences
        if (sc%codeprofil == 1 .or. sc%codeprofil == 2) then


        ! à des dates fixes
          if (sc%codeprofil == 1) then

            read(82,'(a19)',end=999) sc%valprof
            i = 1

          ! DR 27/06/2013 on lit une date (jour/mois/an) et non plus un jour dans l'annee
          ! c'etait fait pour les frequences mais pas pour les dates
!888         read(82,*,end=777) sc%dateprof(ipl,i)
! DR 02/04/2015 je verifie que les dates sont bonnes sinon pb et je rajoute un message
888         read(82,*,end=777) jourprof,moisprof,anprof
            if(anprof.eq.sc%annee(sc%P_iwater))then
                call NDATE (jourprof,moisprof,anprof,sc%dateprof(ipl,i))
                !write(158,*)i,'dateprof', sc%dateprof(ipl,i)
                i = i+1
            else
                if(anprof.eq.sc%annee(sc%P_ifwater))then
                    call NDATE (jourprof,moisprof,anprof,sc%dateprof(ipl,i))
                    an1=sc%annee(sc%P_iwater)
                    sc%dateprof(ipl,i)=sc%dateprof(ipl,i)+nbjParAnnee(an1)
                    !write(158,*)i,'dateprof', sc%dateprof(ipl,i), an1
                    i = i+1
                else
                    call EnvoyerMsgHistorique(1650,anprof)
                    !call NDATE (jourprof,moisprof,anprof,sc%dateprof(ipl,i))
                    !i=i+1
                    !sc%dateprof(ipl,i)=sc%dateprof(ipl,i-1)+365
                endif
            endif
            !call NDATE (jourprof,moisprof,anprof,sc%dateprof(ipl,i))
            !write(158,*)i,'dateprof', sc%dateprof(ipl,i)
            !i = i+1
            goto 888 ! TODO: remplacer ce goto par une boucle avec test sur eof
777         continue

          else

        ! à une date de debut et frequence
            read(82,'(a19)',end=999) sc%valprof
            read(82,*,end=999) freqprof

          ! DR 23/12/09 on lit une date (jour/mois/an) et non plus un jour dans l'annee
          !--  read(82,*,end=999) datedebutprof
            read(82,*,end=999) jourdebutprof,moisdebutprof,andebutprof
            call NDATE (jourdebutprof,moisdebutprof,andebutprof,datedebutprof)
! dr bra&zil 05/08/2011 faut prendre en compte les cultures sont 2 annees
            !if(sc%P_culturean==0)datedebutprof=datedebutprof+sc%nbjsemis

        ! tests de cohérence
            if (datedebutprof < jour_courant) then
              datedebutprof = jour_courant
              call EnvoyerMsgHistorique(165)
            endif

            freqprof = min(freqprof,sc%maxwth/19)

        ! calcul des dates de stockage
        ! DR 07/02/2014 j'augmenet le nb de jours de sorties pour Simtraces
            do j = 1,600
              sc%dateprof(ipl,j) = datedebutprof + ((j-1) * freqprof)
            end do

          endif

999       close(82)
          ! fin de freq ou date

    ! b) lecture du fichier proftot.mod
    ! DR 01/10/2012 on en a plus besoin
    !      fich='proftot.mod'
    !      open(82,file=fich,status='old',err=778)
    !      do i = 1,5
    !        read(82,*,end=778) sc%nomvarprof(i)
    !      end do
!778       close(82)

          sc%numdebprof(ipl) = 1

    ! domi - 5.0 - 8/11/00 - calage sur la premiere date valide
   ! DR 07/02/2014 j'augmenet le nb de jours de sorties pour Simtraces
   ! pose pb on va tester pluto sur le debut de simul
          do kk = 1,600
            if (jour_courant > sc%dateprof(ipl,kk)) then
              sc%numdateprof(ipl) = sc%numdateprof(ipl)+1
              sc%numdebprof(ipl)  = sc%numdateprof(ipl)
            else
              EXIT
            endif
          end do
        endif

      endif ! fin n==1

!   zz=index(sc%nomvarprof(1),"tsol")


    ! stockage des profils dans le tableau tabprof
    ! domi - 14/12/00 - passage des tableaux sol de 200 à 1000

! 779 continue
     if (jour_courant == sc%dateprof(ipl,sc%numdateprof(ipl))) then
     !write(158,*) 'bon jour', jour_courant, sc%dateprof(ipl,sc%numdateprof(ipl))
        !TODO: remplacer par un switch case
        !if (sc%valprof == trim(nomvrprof(1))) then
        if(index(sc%valprof,"tsol").gt.0)then
          sc%tabprof(ipl,sc%numdateprof(ipl),1:profsol_int) = sc%tsol(1:profsol_int)
          sc%numdateprof(ipl) = sc%numdateprof(ipl)+1
        else
          if(index(sc%valprof,"hur").gt.0) then
            sc%tabprof(ipl,sc%numdateprof(ipl),1:profsol_int) = sc%hur(1:profsol_int)
            sc%numdateprof(ipl) = sc%numdateprof(ipl)+1
          else
            if ((index(sc%valprof,"lracz").gt.0))then
              sc%tabprof(ipl,sc%numdateprof(ipl),1:profsol_int) = p%lracz(1:profsol_int)
              sc%numdateprof(ipl) = sc%numdateprof(ipl)+1
            else
              if ((index(sc%valprof,"rl").gt.0))then
                sc%tabprof(ipl,sc%numdateprof(ipl),1:profsol_int) = p%rl(sc%n,1:profsol_int)
                sc%numdateprof(ipl) = sc%numdateprof(ipl)+1
              else
                if ((index(sc%valprof,"nit").gt.0))then
                  sc%tabprof(ipl,sc%numdateprof(ipl),1:profsol_int) = soil%nit(1:profsol_int)
                  sc%numdateprof(ipl) = sc%numdateprof(ipl)+1
                else
                  if ((index(sc%valprof,"humirac").gt.0))then
                    sc%tabprof(ipl,sc%numdateprof(ipl),1:profsol_int) = p%humirac_z(1:profsol_int)
                    sc%numdateprof(ipl) = sc%numdateprof(ipl)+1
                  else
                    if ((index(sc%valprof,"efnrac").gt.0))then
                      sc%tabprof(ipl,sc%numdateprof(ipl),1:profsol_int) = p%efnrac_z(1:profsol_int)
                      sc%numdateprof(ipl) = sc%numdateprof(ipl)+1
                    endif
                  endif
                endif
              endif
            endif
          endif
        endif

    ! domi - 21/07/03 - on borne sc%numdateprof à 20 de toutes facons
! DR 27/062013 j'augmente le nombre de dates pour le fichier profil , je le passe de 20 à 60 dates
! dr 07/02/2014 on augmente le nb de jour du profil à 600
        sc%numdateprof(ipl) = min(sc%numdateprof(ipl),600)

      endif

! si impossible de lire le codeprofi, on ferme le fichier et on sort
1999  close(82)



return
end subroutine Lecture_Profil
 
 
