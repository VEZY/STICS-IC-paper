!   domi 01/12/2005 extration depuis offreN de la confrontation offre /demande
!   et de la maj du profil d'azote 
!   on le fait pour la plante � l'ombre et au soleil
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
!! This module updates the elementary layer nitrogen and ammonium contents, nit(z) and amm(z), over the depth of soil.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine majNsol(nbCouches,zrac,absz,nit,amm)

  implicit none
  
!: Arguments
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: zrac      ! // OUTPUT // Depth reached by root system // cm
  real,    intent(IN)    :: absz(nbCouches)  
  real,    intent(INOUT) :: nit(nbCouches)  
  real,    intent(INOUT) :: amm(nbCouches)  

!: Variables locales
  integer :: iz  !  
  integer :: NRAC  
  real    :: absamm  !  
  real    :: absnit  !  
  real    :: prop  
 
 
      NRAC = int(zrac)
       
! *------------------------------------------* c
! * 3. Mise � jour des profils de NH4 et NO3 * c
! *------------------------------------------* c
      do iz = 1,NRAC
        if (nit(iz) <= 0 .and. amm(iz) <= 0.) CYCLE
        ! ** l'absorption de NH4 et NO3 est r�partie au prorata des quantit�s 
        prop   = absz(iz) / (nit(iz) + amm(iz))
        absnit = prop * nit(iz)
        absamm = prop * amm(iz)
        nit(iz) =  nit(iz) - absnit
        amm(iz) =  amm(iz) - absamm
      end do

return 
end subroutine majNsol 
 
