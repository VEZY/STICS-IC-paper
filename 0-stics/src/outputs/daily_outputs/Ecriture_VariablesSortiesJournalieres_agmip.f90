! ------------------------------------------------
!  écriture des variables de sortie dans le fichier st3
!  les variables de .st3 sont dans var2.mod
!  les variables de .sti sont dans var.mod
!  domi maj du 30/09/2004
! ------------------------------------------------

subroutine Ecriture_VariablesSortiesJournalieres_st3(sc,pg,p,soil,c,sta,t) ! DR 19/07/2012 on supprime itk qui ne sert pas 20/09/2012 t aussi

    USE Stics
    USE Parametres_Generaux
    USE Plante
!    USE Itineraire_Technique
    USE Sol
    USE Climat
    USE Station
!    USE Divers, only: isBissextile
    implicit none

!: Arguments
    type(Stics_Communs_),       intent(INOUT)   :: sc  
    type(Parametres_Generaux_), intent(IN)      :: pg  
    type(Plante_),              intent(IN)      :: p  
!    type(ITK_),                 intent(IN)      :: itk
    type(Sol_),                 intent(IN)      :: soil  
    type(Climat_),              intent(IN)      :: c  
    type(Station_),             intent(IN)      :: sta  
    type(Stics_Transit_),       intent(IN)      :: t


!: Variables locales
    integer           :: i  !  
    integer           ::  k
    character         :: sep  
    character*20      :: pays
    integer   :: rmon,nday,nummois,nan
    character*850 :: ligne
    integer    ::j,nbligne_entete
    character*2 :: nom_mois(12)
    logical :: isBissextile
    integer :: a,jjul2
    character*2 :: nom_jour(31)
    data nom_mois/'01','02','03','04','05','06','07','08','09','10','11','12'/
    data nom_jour/'01','02','03','04','05','06','07','08','09','10',   &
                       '11','12','13','14','15','16','17','18','19','20',   &
                       '21','22','23','24','25','26','27','28','29','30','31'/
    !    character*30      :: chaine_debut_ligne
    integer       :: long_usm,ii
    character*5  :: nom_plante
    character*20  :: nom_lieu
    logical       :: macsur,AgMIP

  AgMIP= .FALSE.
  Macsur= .FALSE.
  if(t%P_type_project.eq.2) AgMIP= .TRUE.
  if(t%P_type_project.eq.3) Macsur= .TRUE.

! write(*,*)sc%n, sc%nbvarsortie

      call CorrespondanceVariablesDeSorties(sc,p,soil,c,sta,         &  ! DR 19/07/2012 on supprime itk qui ne sert pas20/09/2012 t aussi
                                            sc%nbvarsortie,                 &
                                            sc%valpar(1:sc%nbvarsortie),     &
                                            sc%valsortie(1:sc%nbvarsortie))

! Ecriture de l'entete dans .sti
! ==============================
      if (sc%n == 1 .and. sc%numcult ==1) then
! dr 20/12/2013 ecriture der la ligne d'entete on l'enleve
!        write(p%ficsort3,223)
223     format('Stics ',$)
!        write(p%ficsort3,221)trim(sc%P_usm)
!        write(p%ficsort3,222)
222     format(';ian;jul',$)
        do i = 1,sc%nbvarsortie
!          write(p%ficsort3,221) trim(sc%valpar(i))
        end do
221     format(';',A,$)
      ! on retourne à la ligne
!        write(p%ficsort3,*)
!       sc%chaine_debut_ligne='Stics; low '//';'//trim(pays)//';'
       sc%chaine_debut_ligne='ST;date;'//trim(sc%P_usm(5:6))//';'

       if (sc%codeenteterap == 1 ) then
            if (macsur)then
         open(1,file='Daily_template_CropM_Rotationeffect.txt')
         nbligne_entete=0
         do j=1,10
                  read(1,'(a300)',end=999)ligne
         if (j.eq.6.or.j.eq.7)then
                      write(p%ficsort3,'(a300)')ligne
             write(*,'(a225)')ligne
         endif
         nbligne_entete=nbligne_entete+1
         enddo
   999 continue
      close(999)
            endif
            if(AgMIP)then
                open(1,file='entete_daily.txt')
                read(1,*,end=9999)nbligne_entete
                do j=1,nbligne_entete
                  read(1,'(a850)',end=9999)ligne
                  write(p%ficsort3,'(a850)')ligne
                  write(*,'(a850)')ligne
                enddo
               read(1,'(a20)',end=9999)sc%model_name
               read(1,'(a20)',end=9999)sc%info_level
   9999 continue
               close(999)
               close(1)
           endif
       else
             sc%model_name='ST'
             sc%info_level=''
       endif
      endif
! =========================================================================================
      if (pg%P_codeseprapport == 2) then
        sep = pg%P_separateurrapport
      else
        sep = ' '
      endif

      long_usm=len_trim(sc%P_usm)
      nom_plante=trim(sc%P_usm(long_usm-2:long_usm+1))

           ii=index(sc%P_usm,'_')
           nom_lieu=sc%P_usm(1:ii-1)
           nom_plante= sc%P_usm(ii+1:ii+5)

    !  Bruno - 29/04/03 - en attente de verif dans winstics
    ! DR 171006 si on a choisit le code separateur pour rapport on
    ! DR 21/07/08 pour la version standard on a plus que le sti
if (pg%P_codeoutscient == 2) then
      ! dr 19/09/2014 je teste en elevant ca qui pose pb
!        write(2013,852)trim(sc%chaine_debut_ligne), sc%annee(sc%jjul),sep,sc%jul,   &
!        (sep,sc%valsortie(k),k = 1,sc%nbvarsortie),sep,trim(sc%P_usm(12:14))

         ! dr 09/12/2013 pour macsur il faut ecrire les cumuls à partir du semis
!        call julien(sc%jjul,sc%annee(sc%jjul),rmon,nday,nummois)
        a=sc%annee(sc%jjul)-1
        isBissextile = ((mod(a,4) == 0) .and. (mod(a,100) /= 0)) .or. (mod(a,400) == 0)
        jjul2=sc%jjul
        if (isBissextile.and.sc%jjul.gt.366)then
        jjul2=sc%jjul-366
        endif
        if (.not.isBissextile.and.sc%jjul.gt.365)then
        jjul2=sc%jjul-365
        endif
        call julien(jjul2,sc%annee(sc%jjul),rmon,nday,nummois)

        !pour agmip qminr en positif
!        sc%valsortie(15) = -sc%valsortie(15)
       if(macsur)then
            write(p%ficsort3,853)'ST',sep,sc%annee(sc%jjul),nom_mois(nummois),nom_jour(nday),sep,trim(sc%P_usm(5:6)),&
            (sep,sc%valsortie(k),k = 1,sc%nbvarsortie),sep,nom_plante
 853        format(A2,a1,i4,2a2,a1,a3,22(a1,f12.5),a1,a3)
       endif
       if(AgMIP)then
! pour agmpip_face
! on converti les rendts en g/m2 on multiplie par 100
!        write(p%ficsort3,855)'STICS;LOW;Braunschweig',sep,sc%annee(sc%jjul),sep,sc%jul,sep,trim(sc%P_usm(1:3)),&
!         sep,trim(sc%P_usm(5:8)),(sep,sc%valsortie(k),k = 1,sc%nbvarsortie)
            write(p%ficsort3,855)trim(sc%model_name),sep,trim(sc%info_level),sep, trim(nom_lieu),   &
           sep, trim(nom_plante), sep,sc%annee(sc%jjul),sep,sc%jul, (sep,sc%valsortie(k),k = 1,sc%nbvarsortie)
 !855  format(A22,a1,i4,a1,i4,a1,a3,a1,a4,22(a1,f12.5),a1,a3)
 855        format(A5,a1,a14,a1,a10,a1,a5,a1,i4,a1,i3,40(a1,f12.5))
        endif
else
        write(p%ficsort3,851)trim(sc%chaine_debut_ligne), sc%annee(sc%jjul),sep,sc%jul,   &
        (sep,sc%valsortie(k),k = 1,sc%nbvarsortie),sep,trim(sc%P_usm(12:14))
endif


!-- 851      format(i4,i3,i3,i4,100(1x,e12.3))
 851  format(A,i4,a1,i3,100(a1,e12.3))
!-- 852      format(i4,i3,i3,i4, 100f8.2)
! 852  format(A,i4,a1,i3,100(a1,f12.5))
 852  format(A,i4,a1,i3,22(a1,f12.5),a1,a3)



return
end subroutine Ecriture_VariablesSortiesJournalieres_st3
 
 
