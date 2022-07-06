subroutine init_prairie(sc,p,itk,pg)

USE Stics
USE Plante
USE Itineraire_Technique
USE Parametres_Generaux
USE Messages
USE Divers, only: isBissextile

implicit none
  type(Stics_Communs_),       intent(INOUT) :: sc
  type(Parametres_Generaux_), intent(INOUT) :: pg
  type(Plante_),              intent(INOUT) :: p (sc%P_nbplantes)
  type(ITK_),                 intent(INOUT) :: itk (sc%P_nbplantes)

! Variable(s) locale(s)
      integer :: i  !
      integer :: ipl  !
      integer :: k  !
      character(len=500) :: tmp ! pour la consitution de messages complexes pour le fichier historique.
      real    :: difftemp(2)
!      real :: som_temp_manquante

!! DR 29/11/2016 nettoyage de la prise en compte de la premiere coupe climator qui pose pb !
  do ipl = 1, sc%P_nbplantes
! == = == = == = == = == = == =  PRAIRIES CLIMATOR 17/01/2017 y'a plus de prairies climator
!
    sc%onafaitunecoupedelanneedavant=.FALSE.

! dr et fr 09/01/2017 on passe dans init_prairie � maxwth tardivement voir pourquoi
    if(sc%n.eq.sc%maxwth)p(ipl)%fauchediff=.FALSE.
    !write(619,*)'numcult',sc%numcult
    !write(619,*)'**********'
    ! **** 22/06/2015 on est pas en prairies semees
    if ((p(ipl)%P_stade0 .ne. 'snu' .and. p(ipl)%P_stade0 .ne. 'plt')) then
        ! ** initialisations pour les cultures fauch�es
        ! DR 07/11/05 on initialise faucheannule   ! DR 13/01/06 on initialise onarretesomcourdrp
        sc%faucheannule = 0
        p(ipl)%onarretesomcourdrp = .FALSE.
             ! 12/01/2017 si on est en echainement climatique avce reinitilaisation on remet msres � masec0 chaque annee
             if(sc%numcult.eq.1)then
                  p(ipl)%msres = p(ipl)%P_masec0
                  itk(ipl)%P_msresiduel(0) =  p(ipl)%P_masec0
             else if(sc%P_codesuite.eq.0)then
                      p(ipl)%msres = p(ipl)%P_masec0
                      itk(ipl)%P_msresiduel(0) =  p(ipl)%P_masec0
                 else
                      p(ipl)%msres = itk(ipl)%P_msresiduel(1)
 !                     itk(ipl)%P_msresiduel(0) =  p(ipl)%P_masec0
             endif

!12/01/2017 ca n'a pas de sens de tester sur codefauche alors que le msresiduel n'a de sens que si on fauche
!      if (itk(ipl)%P_codefauche == 1 .and. itk(ipl)%lecfauche ) then
        if (itk(ipl)%lecfauche ) then
          ! DR 26/02/08 on teste dans l'enchainement de perenne paririe de garder le reliquat
          !   de somcourfauche depuis la derniere coupe de l'annee d'avant
            if (p(ipl)%P_codeperenne == 2) then
                do i = 1,itk(ipl)%nbcoupe
                    if (itk(ipl)%P_codemodfauche == 3) then
                 !    p(ipl)%tempfauche_ancours(i) = sc%tempfauche_ancours_an1(ipl,i)
                     p(ipl)%tempfauche_ancours(i) = sc%tempfauche_ancours_ini(ipl,i) !!! 02/01/2017 normalement deja initialis� dans init_priaries_perennes
                     itk(ipl)%P_tempfauche(1)=p(ipl)%tempfauche_ancours(1)
                    endif
                enddo
                if (sc%numcult==1) then
               ! DR et FR il faut qu'on ajoute un cas annee 1 car dans le cas ou on est en somme de temp , on saute l'annee 1 car tempfauche(1) devient = 0
               ! **** cas annee 1 **** on fait plus rien
               ! *********************
                else
               ! DR 28/12/2016 (anniv de Camille ) j'enleve les tableaux en ann�es2 puisque maintenant on a supprime le fait de faire une ann�e de demarrage
               !  en supprimant la premiere coupe
               ! **** cas annee 2 ****
               ! *********************
               !write(619,*)'cas annee2'
! 20/07/2016 on tente de garder les variables de stades de la vegetaion � la fin de l'annee
                   if(sc%onreinitialise_ulai(ipl).eqv. .TRUE.)then
                       p(ipl)%ulai(:,1) = p(ipl)%ulai0
                       p(ipl)%nlax = 0
                       p(ipl)%ndrp = 0
                       p(ipl)%codebbch0 = 35
                       sc%onreinitialise_ulai(ipl) = .FALSE.
                   endif
                endif
                if (.not.p(ipl)%fauchediff) then
                ! 1ER CAS ***********************************************************
                ! On est pas en fauche diff mais il peut rester des coupes � faire
                ! *******************************************************************
                ! on a pas cumul� les temperatures necessaires pour faire toutes les coupes de l'annee pr�c�dente
                !DR 16/06/2016 on se foue de gerer les coupes d'avant , on remet les compteurs � zero mais on garde la biomasse
                    difftemp(ipl) = 0.
                ! 19/02/2015 DR et FR on veut garder le calcul des somcourfauche meme si on est en dates
                ! donc il faut le reinitialiser en debut d'annee dans le cas ou on ets en dates
                ! 13/02/2015 on ne remet pas a zero puiqu'on garde le reliquat
                    p(ipl)%somcourfauche = 0.
                 !write(619,*)'2eme sous cas'
                 !write(619,*)'nouveau calendar',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)
                else
                ! 2EME CAS FAUCHE DIFFEREE ***************************************************
                ! On a reporte car on avait pas assez de lai ou de matiere seche pour couper
                ! on repousse la coupe jusqu'a avoir le lai min
                ! on recalcule les temfauche_ancours � partir de la coupe 2
                ! ***************************************************************************
                ! 11/07/2016 DR et FR on ne veut plus rien garder de l'annee d'avant
! 15/04/2016 on tente de garder les variables de stades de la vegetaion � la fin de l'annee
                    p(ipl)%ulai(:,1) = p(ipl)%ulai0
                    p(ipl)%durvie(:,1) = p(ipl)%durvie0(:)
                    if (p(ipl)%codebbch0.ge.9) p(ipl)%nlev=1
                    if (p(ipl)%codebbch0.ge.35)then
                        p(ipl)%nlev=1
                        p(ipl)%namf=1
                    endif
                    if (p(ipl)%codebbch0.ge.55) then
                        p(ipl)%nlev=1
                        p(ipl)%namf=1
                        p(ipl)%nlax=1
                    endif
! 15/04/2016
             ! DR 28/12/2016 (anniv de Camille ) j'enleve les tableaux en ann�es2 puisque maintenant on a supprime le fait de faire une ann�e de demarrage
             !  en supprimant la premiere coupe
                    if (itk(ipl)%P_codemodfauche == 3) then
                      do k = 1,19
                        if (k > 1) then
                        p(ipl)%tempfauche_ancours(k) = itk(ipl)%P_tempfauche(k) + p(ipl)%tempfauche_ancours(k-1)
                        else
                        p(ipl)%tempfauche_ancours(k) = itk(ipl)%P_tempfauche(k)
                        endif
                      enddo
                    endif
                    p(ipl)%somcourfauche = 0.
                ! DR 28/12/2016 itk(ipl)%nbcoupe = sc%nbcoupe_an2(ipl) - 1
                endif ! *** fin fauche diff ou pas diff
              !write(619,*)'nouveau calendar',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)
            else
            ! les 4 coupes normales
            ! **** cas annee 1 semis ****
            !****************************
            ! DR cas annee 1
              !write(619,*)'les 4 coupes normales'
              do  i = 1,20
                 p(ipl)%tempfauche_ancours(i) = sc%tempfauche_ancours_ini(ipl,i) !!! 02/01/2017 normalement deja initialis� dans init_priaries_perennes
              enddo
              !write(619,*)'nouveau calendar annee 1 (init_prairie, 368)',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)
              ! DR 28/12/2016 itk(ipl)%nbcoupe = sc%nbcoupe_an1(ipl)
              p(ipl)%somcourfauche = 0.
            endif ! fin codeperenne ou pas
        endif  ! fin lecfauche
!*******************************  On fait ca dans tous les cas  *************************************************
          p(ipl)%numcoupe = 1
          ! TRAITEMENT DES HAUTEURS DE COUPE QUELQUE SOIT LE MODE DE COUPE (DATE,SOMME,STADE)
          !*********************************************************************************
          !12/01/2017 pas de sens puisque on est en culture fauchees
          !if (itk(ipl)%P_codefauche == 1) then
          if (itk(ipl)%lecfauche) then
             ! CAS FAUCHE A DATE OU SOMME  ***************
              do i = 1,itk(ipl)%nbcoupe
                p(ipl)%nfauche(i) = itk(ipl)%P_julfauche(i)- sc%P_iwater + 1
              ! initialisation pour la prairie calcul de lai et ms residuels f(hauteur de coupe)
                if (itk(ipl)%P_hautcoupe(i) < 999) then
                ! utilisation des relations suivantes : P_msresiduel = P_coefmshaut * P_hautcoupe
                ! et la relation de raytrans.for        lai = P_hautmax *(1-exp(-P_khaut * hauteur)) + P_hautbase
                  itk(ipl)%P_msresiduel(i) = p(ipl)%P_coefmshaut * (itk(ipl)%P_hautcoupe(i) - p(ipl)%P_hautbase)
                  itk(ipl)%P_lairesiduel(i) = -1. / pg%P_khaut *                                             &
                                                   log(1. - ((itk(ipl)%P_hautcoupe(i) - p(ipl)%P_hautbase) / p(ipl)%P_hautmax))
                  itk(ipl)%P_lairesiduel(i) = max(itk(ipl)%P_lairesiduel(i),0.)
                  itk(ipl)%P_msresiduel(i) = max(itk(ipl)%P_msresiduel(i),0.)
!                  P_mscoupemini = max(P_mscoupemini,itk(ipl)%P_msresiduel(1))
                endif
              end do
          else
            ! CAS FAUCHE A UN STADE ***************
              do i = 1,10
                p(ipl)%udevlaires(i)    = 0.
                itk(ipl)%P_hautcoupe(i)   = itk(ipl)%P_hautcoupedefaut
                itk(ipl)%P_msresiduel(i)  = p(ipl)%P_coefmshaut * (itk(ipl)%P_hautcoupe(i) - p(ipl)%P_hautbase)
                itk(ipl)%P_lairesiduel(i) = -1. / pg%P_khaut * log(1. -                                      &
                                           ((itk(ipl)%P_hautcoupe(i) - p(ipl)%P_hautbase) / p(ipl)%P_hautmax))
                itk(ipl)%P_lairesiduel(i) = max(itk(ipl)%P_lairesiduel(i),0.)
                itk(ipl)%P_msresiduel(i)  = max(itk(ipl)%P_msresiduel(i),0.)
              end do
              itk(ipl)%nbcoupe = 10
          endif
        ! DR 31/01/08 climator : on garde ilaxs pour les pariries
          do i = 1,10
            p(ipl)%ilaxs_prairie(i) = -999
          enddo
    endif  ! fin prairies semees ou en place
! == = == = == = == = == = fin  prairies climator / plutot prairies semees
! DR et FR 16/02/2015 on traite le cas des prairies en semis l'annee 1  codestade0=snu
!  on calcule le nfauche pour l'annee de semis
    if(sc%numcult.eq.1)then
           !12/01/2017 pas de sens puisque on est en culture fauchees
           !if (itk(ipl)%P_codefauche == 1 .and. itk(ipl)%lecfauche) then
        if (itk(ipl)%lecfauche) then
             p(ipl)%numcoupe = 1
             do i = 1,itk(ipl)%nbcoupe
                 if (itk(ipl)%P_codemodfauche == 2 )then
                    p(ipl)%nfauche(i) = itk(ipl)%P_julfauche(i)- sc%P_iwater + 1
                 endif
                 if (itk(ipl)%P_codemodfauche == 3 )then
                    p(ipl)%tempfauche_ancours(i) = sc%tempfauche_ancours_ini(ipl,i)
                 endif
             enddo
              !write(619,*)'nouveau calendar annee 1 (init_prairie, 351)',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)
        endif
    endif
! DR et FR 24/06/2015 on deplace ici car valide pour toutes les situations
! DR et Fr 16/02/2016 le mscoupe mini est vraiment ce qu'on veut recolter au minimum , sinon on se fatigue pas � se deplacer !
            itk(ipl)%mscoupemini_courant =itk(ipl)%P_mscoupemini(1)
 !  *** ecritures dans history
    do i = 1,itk(ipl)%nbcoupe
        if (itk(ipl)%P_codemodfauche == 2) then
            write(tmp,*) 'P_julfauche,P_hautcoupe,P_lairesiduel,P_msresiduel,P_anitcoupe ',        &
                         itk%P_julfauche(i),itk(ipl)%P_hautcoupe(i),itk(ipl)%P_lairesiduel(i), &
                         itk(ipl)%P_msresiduel(i),itk(ipl)%P_anitcoupe(i)
            call EnvoyerMsgHistorique(tmp)
            ! NB - le 20/01/2004 - test de coh�rence (avec FR)
            if (itk(ipl)%P_lairesiduel(i) > 0.0 .and. itk(ipl)%P_msresiduel(i) <= 0.0)   &
                call EnvoyerMsgHistorique(91)
        endif

        if (itk(ipl)%P_codemodfauche == 3) then
            write(tmp,*) 'P_tempfauche,P_hautcoupe,P_lairesiduel,P_msresiduel,P_anitcoupe',              &
                         itk(ipl)%P_tempfauche(i),itk(ipl)%P_hautcoupe(i),itk(ipl)%P_lairesiduel(i), &
                         itk(ipl)%P_msresiduel(i),itk(ipl)%P_anitcoupe(i)
            call EnvoyerMsgHistorique(tmp)
            ! NB - le 20/01/2004 - test de coh�rence (avec FR)
            if (itk(ipl)%P_lairesiduel(i) > 0.0 .and. itk(ipl)%P_msresiduel(i) <= 0.0)   &
                call EnvoyerMsgHistorique(91)
        endif
    enddo
  enddo
         !write(619,*)'fin de initprairie',(p(ipl)%tempfauche_ancours(i),i=1,itk(ipl)%nbcoupe)
end subroutine
