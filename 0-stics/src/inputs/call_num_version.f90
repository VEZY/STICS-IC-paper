subroutine call_num_version (sc)

USE Stics

  implicit none
  type(Stics_Communs_), intent(INOUT) :: sc
! sous programme de recuperation du nom et du numero de version
! dr 20/06/2013 j'ajoute le numero de version et la date genere automatiquement à la pose d'un tag ??
  character(len=10) :: dateversion
  character(len=30) :: nomversion

! DR 20/06/2013 je lis la version dans un fichier qui me sera fourni par Ju.
!     nomversion='v8.3.2'
!     dateversion='11/15/2013'
        nomversion='v8.5_IC' 
        dateversion='Debug' 

     sc%codeversion= trim(nomversion)//'_'//trim(dateversion)

end subroutine call_num_version