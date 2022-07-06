! ------------------------------------------------
!  écriture des variables de sortie
!  les variables de .st2 sont dans var2.mod
!  les variables de .sti sont dans var.mod
!  domi maj du 30/09/2004
! ------------------------------------------------
! writing of the output variables
!! variables. sti are in var.mod
!subroutine Ecriture_VariablesSortiesJournalieres(sc,pg,p,itk,soil,c,sta,t)  !DR 23/07/2012 on enleve itk non utile 20/09/2012 t aussi
subroutine Ecriture_VariablesSortiesJournalieres(sc,pg,p,soil,c,sta)

    USE Stics
    USE Parametres_Generaux
    USE Plante
!    USE Itineraire_Technique
    USE Sol
    USE Climat
    USE Station

    implicit none

!: Arguments
    type(Stics_Communs_),       intent(INOUT)   :: sc  
    type(Parametres_Generaux_), intent(IN)      :: pg  
    type(Plante_),              intent(IN)      :: p  
!    type(ITK_),                 intent(IN)      :: itk
    type(Sol_),                 intent(IN)      :: soil  
    type(Climat_),              intent(IN)      :: c  
    type(Station_),             intent(IN)      :: sta  
!    type(Stics_Transit_),       intent(IN)      :: t

!: Variables locales
    integer           :: i  !  
    integer           ::  k  
    character         :: sep  



!      call CorrespondanceVariablesDeSorties(sc,p,itk,soil,c,sta,t,          &   ! DR 19/07/2012 on supprime itk qui ne sert pas 20/09/2012 t aussi
      call CorrespondanceVariablesDeSorties(sc,p,soil,c,sta,          &
                                            sc%nbvarsortie,                 &
                                            sc%valpar(1:sc%nbvarsortie),     &
                                            sc%valsortie(1:sc%nbvarsortie))


! Ecriture de l'entete dans .sti
! ==============================
      if (sc%n == 1 .and. sc%numcult ==1) then

        write(p%ficsort,222)
222     format('ian;mo;jo;jul',$)

        do i = 1,sc%nbvarsortie
          write(p%ficsort,221) trim(sc%valpar(i))
        end do

221     format(';',A,$)

      ! on retourne à la ligne
        write(p%ficsort,*)

      endif
! ===============================


      sep = ';'

    !  Bruno - 29/04/03 - en attente de verif dans winstics
    ! DR 171006 si on a choisit le code separateur pour rapport on
    ! DR 21/07/08 pour la version standard on a plus que le sti
      if (pg%P_codeoutscient == 2) then
        write(p%ficsort,852) sc%annee(sc%jjul),sep,sc%nummois,sep,sc%jour,sep,sc%jul,(sep,sc%valsortie(k),k = 1,sc%nbvarsortie)
      else
        write(p%ficsort,851) sc%annee(sc%jjul),sep,sc%nummois,sep,sc%jour,sep,sc%jul,(sep,sc%valsortie(k),k = 1,sc%nbvarsortie)
      endif

    ! DR 05/09/06 dans le cas de chiffre negatif les colonnes sotn collées
    ! je rajoute 2colonnes et 1 espace
    ! DR 17/10/06 si on met un separateur faut modifier le format


!    write(*,*)p%masec(0,sc%n)
!-- 851      format(i4,i3,i3,i4,100(1x,e12.3))
 851  format(i4,a1,i2,a1,i2,a1,i3,200(a1,e12.3),/)
!-- 852      format(i4,i3,i3,i4, 100f8.2)
 852  format(i4,a1,i2,a1,i2,a1,i3,200(a1,f12.5),/)


return
end subroutine Ecriture_VariablesSortiesJournalieres
 
 
