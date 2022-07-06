
subroutine Stics_Jour_after(sc,pg,c,sta,soil,p,itk,t)


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

  type(Climat_),              intent(INOUT) :: c  

  type(Station_),             intent(INOUT) :: sta  

  type(Plante_),              intent(INOUT) :: p(sc%P_nbplantes)  

  type(ITK_),                 intent(INOUT) :: itk(sc%P_nbplantes)  

  type(Stics_Transit_),       intent(INOUT) :: t  

  type(Sol_),                 intent(INOUT) :: soil  


   integer :: i  !  
   integer :: jour  !  
   integer :: nummois  !  
   integer :: n  

if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *,'beginning stics_jour_after '
    ! pour alléger l'écriture
      n = sc%n

    ! préparation des variables pour les écritures. Par défaut, on utilise les valeurs de la plante principale
if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)write(sc%ficdbg2,*)'ns',n,p(i)%masec(0,n),p(i)%nbgrains(0),&
p(i)%magrain(0,n),p(i)%chargefruit
      call Stics_Calcul_Sorties(sc,c,soil,p(1),itk(1),pg,sta)
if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)write(sc%ficdbg2,*)'ns',n,p(i)%masec(0,n),p(i)%nbgrains(0),&
p(i)%magrain(0,n),p(i)%chargefruit
    ! préparations des variables plantes pour les écritures.
      do i= 1, sc%P_nbplantes

      ! DR 24/02/2011 calcul des codebbch journaliers
          call stadeversbbch(sc%n,p(i))

if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)write(sc%ficdbg2,*)'np',n,i,p(i)%masec(0,n),p(i)%nbgrains(0)&
,p(i)%magrain(0,n),p(i)%chargefruit
          call Stics_Calcul_Sorties_Plante(sc,pg,c,p(i),itk(i),soil,t)
if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)write(sc%ficdbg2,*)'np',n,i,p(i)%masec(0,n),p(i)%nbgrains(0)&
,p(i)%magrain(0,n),p(i)%chargefruit


!    !  ajout du drainage et lessivage au fichier drailess.sti ! DR 10/09/2012 je vire
!         if (iand(pg%P_flagEcriture,sc%ECRITURE_DRAINAGE) > 0) then
!            write(p(i)%ficdrat,95) sc%P_usm,sc%annee(sc%jjul),nummois,jour,sc%drat,sc%QLES
!95          format(1x,a40,3i6,f9.0,f9.1)
!         endif

      end do



      do i= 1, sc%P_nbplantes

      ! DR 13/01/06 pour simplifier les sorties je mets tauxrecouv ou lai(aoas) dans une var temporaire
      ! taux de couverture uniquement pour culture pure, donc p(1).
        if (p(1)%P_codelaitr /= 1) then
          p(1)%lai(sc%aoas,n) = sc%tauxcouv(n)
        endif


! DR 23/09/2016  on a sortie les ecritures pour les mettre dans stics_jour apres la gestion des coupes
! et on a cree 3 varaibles qui conservenbt les valeurs avant coupe
!===============================
! DR 23/09/2016 pour Efese prairie on garde 3 variables correspondants aux maxi atteint le jour de la coupe
           p(i)%lai_mx_av_cut= p(i)%lai(0,sc%n)
           p(i)%masec_mx_av_cut= p(i)%masec(0,sc%n)
           p(i)%QNplante_mx_av_cut= p(i)%QNplante(0,sc%n)

!      write(618,*)'avant sortie stics_jour_after', n, p(1)%masec(1,n),p(1)%masec(0,n)


!        if (iand(pg%P_flagEcriture,sc%ECRITURE_SORTIESJOUR) >0 ) then
!if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)write(sc%ficdbg2,*)'n',n,p(i)%cep,p(i)%ep(1)
! DR 23/07/2012 on supprime itk qui ne sert pas 20/09/2012 t aussi

!DR 23/09/2016 on vire de la
!          call Ecriture_VariablesSortiesJournalieres(sc,pg,p(i),soil,c,sta)
!if (iand(pg%P_flagEcriture,sc%ECRITURE_DEBUG)>0)write(sc%ficdbg2,*)'n',n,p(i)%cep,p(i)%ep(1)

 !       endif
! DR 07/09/2011 j'joute la possibilité d'ecrire dans un fichier st3 pour avoir le vrai format agmip
! DR 29/08/2012 j'ajoute un code pour garder les sorties Agmip t%P_Config_Output : 1=rien , 2 sorties ecran , 3 sorties agmpi
 !       if (iand(pg%P_flagEcriture,sc%ECRITURE_AGMIP) >0 )then
          !if (iand(pg%P_flagEcriture,sc%ECRITURE_SORTIESJOUR) >0 ) then
          ! DR 19/07/2012 on supprime itk qui ne sert pas 20/09/2012 t aussi
!          call Ecriture_VariablesSortiesJournalieres_st3(sc,pg,p(i),soil,c,sta,t)
          !endif
!        endif
!      write(618,*)'apres sortie',n, p(1)%masec(1,n),p(1)%masec(0,n)
!==================================

    ! stockage des variables par profil
        if (iand(pg%P_flagEcriture,sc%ECRITURE_PROFIL) >0 ) then
          call Lecture_Profil(sc,p(i),soil,i)
        endif


!  enregistrement dans le fichier racine.sti - on garde en commentaire - 25/06/02
!--       if (n == 1) then
!--        if (ipl == 1) then
!--           open (26,file = 'racineP.sti',status = 'unknown')
!--        else
!--           open (26,file = 'racineA.sti',status = 'unknown')
!--        endif
!--        write(26,4)' jul   rltot rlsentot rlbrute poussmoy',(j,j = 5,int(profsol),30)
!--       endif
!--   4   format(a38,i6,35i8)
!--       write(26,90) n + P_iwater - 1,rltot,lracsentot,rltot + lracsentot,poussracmoy,(rl(n,j),j = 5,int(profsol),30)
!--  90   format(i4,3f8.1,35f8.3)



    ! on enregistre certaines valeurs de la plante pour des jours particuliers
    ! ça permet de supprimer certains tableaux temporels qui stockent toute la simmulation et de ne conserver que les
    ! valeurs utiles

        if (sc%n == p(i)%ntaille - 1) then
            p(i)%QNgrain_ntailleveille = p(i)%QNgrain(sc%AOAS)
        endif

        if (sc%n == p(i)%nrec) then

        endif

      end do
if (iand(pg%P_flagEcriture,sc%ECRITURE_ECRAN) >0 )print *,'end stics_jour_after '

return
end subroutine Stics_Jour_after
 
 
