!
! On initialise les variables fauche pour l'annee de semis
! (on saute la premiere coupe de regain d'octobre)
! et pour les autres annees

subroutine Initialisation_PrairiePerenne(sc,itk,ipl,stade0, lai0, masec0, magrain0, zrac0, QNplante0,resperenne0,densinitial)


USE Stics
USE Plante
USE Itineraire_Technique

    implicit none

    type(Stics_Communs_),       intent(INOUT) :: sc  

    type(ITK_),                 intent(INOUT) :: itk  

    integer,                    intent(INOUT) :: ipl  


    integer :: k  
    character*3 :: stade0
    real  :: lai0, masec0, magrain0, zrac0, QNplante0,resperenne0
    real :: densinitial(5)

! DR 26/02/08 pour climator la premiere annee on zappe la premiere coupe
!             qui est la coupe de regain qui ne se fait pas en annee de semis
!DR 29/11/2016 on supprime cete satané premiere coupe qui nous foutait le bazard !!!

    sc%ipl = ipl

! DR 28/12/2016 (anniv de Camille ) j'enleve les tableaux en années2 puisque maintenant on a supprime le fait de faire une année de demarrage
!  en supprimant la premiere coupe. Donc ca simplifie beaucoup le code
!
      do k = 1,itk%nbcoupe
        if(itk%P_codemodfauche == 3) then
          if(k > 1) then
           sc%tempfauche_ancours_ini(sc%ipl,k) = itk%P_tempfauche(k)  &
                                              + sc%tempfauche_ancours_ini(sc%ipl,k-1)
          else
            sc%tempfauche_ancours_ini(sc%ipl,k) =  itk%P_tempfauche(k)
          endif
        endif
      enddo


! DR et Fr 20/07/2016 on garde les valeurs initiales pour la prairie dans le cas ou elle meurt on va repartir sur les valeurs initiales
      sc%stade0_ini(sc%ipl) = stade0
      sc%lai0_ini(sc%ipl) = lai0
      sc%masec0_ini(sc%ipl) = masec0
      sc%QNplante0_ini(sc%ipl) = QNplante0
      sc%magrain0_ini(sc%ipl) = magrain0
      sc%zrac0_ini(sc%ipl) = zrac0
      sc%resperenne0_ini(sc%ipl) = resperenne0
      sc%densinitial_ini(sc%ipl,:) = densinitial(:)
return
end subroutine Initialisation_PrairiePerenne
 
 
