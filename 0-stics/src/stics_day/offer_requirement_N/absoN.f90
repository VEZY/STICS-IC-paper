! DR le 06/01/06 on � decompos� offren en absoN et majNsol
! on le fait sur la partie � l'ombre et au soleil
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
! This module calculates the actual N uptake.
! - Stics book paragraphe 8.6.4, page 100
!
!! The mineral N available for root uptake in each layer (offrN) is equal to the smallest of the three terms: soil supply, uptake capacity and available mineral N.
!! The integration of offrN over the whole profile yields cumoffrN. In each layer, the N supply can be compared to the crop demand through the ratio prop.
!!
!! If prop = 1, the soil N supply is the factor limiting N uptake. In this case the N uptake in each layer is equal to the N supply offrN.
!! Conversely, the demand is the factor limiting N uptake if prop < 1; in this case, the actual N uptake in each layer is smaller than the N supply and
!! proportional to it.
!!
!! In both cases, the actual N uptake in each soil layer (absz) and the total uptake over the root profile (abso) can be written as functions of the prop variable.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
subroutine absoN(zrac,nbCouches,cumoffrN,surf,offrN,nit,amm,  &
                 absz,demande,profextN,abso)

  implicit none

!: Arguments
  real,    intent(IN)    :: zrac      ! // OUTPUT // Depth reached by root system // cm
  integer, intent(IN)    :: nbCouches  
  real,    intent(IN)    :: cumoffrN  
  real,    intent(IN)    :: surf      ! // OUTPUT // Fraction of surface in the shade // 0-1
  real,    intent(IN)    :: offrN(nbCouches)  
  real,    intent(IN)    :: nit(nbCouches)  
  real,    intent(IN)    :: amm(nbCouches)  


  real,    intent(INOUT) :: absz(nbCouches)  
  real,    intent(INOUT) :: demande      ! // OUTPUT // Daily nitrogen need of the plant   // kgN.ha-1.j-1
  real,    intent(INOUT) :: profextN      ! // OUTPUT // Average depth of Nitrogen absorption // cm
  real,    intent(INOUT) :: abso                        ! (n)       // OUTPUT // Nitrogen absorption rate by plant  // kg N ha-1




!: Variables locales
  integer :: iz  !  
  integer :: NRAC  
  real    :: prop  

!write(245,*)'absoN',abso,zrac,demande, cumoffrN ,surf


      NRAC = int(zrac)

      absz(:) = 0.0

      ! *-----------------------------------------------------* c
      ! * 2. Confrontation offre/demande et absorption r�elle * c
      ! *-----------------------------------------------------* c
      if (demande < cumoffrN * surf) then
        prop = demande / (cumoffrN * surf)
        do iz = 1,NRAC
          absz(iz) = offrN(iz) * prop * surf
        end do
      else
        do iz = 1,NRAC
          absz(iz) = offrN(iz) * surf
        end do
      endif

      ! DR 25/02/08 calcul de la profondeur moyenne d'extration d'azote
      profextN = 0.
      do iz = 1,NRAC
        if (nit(iz) <= 0 .and. amm(iz) <= 0.) CYCLE
        abso = abso + absz(iz)
        profextN = profextN + (absz(iz) * iz)
      end do

      if (abso > 0.) then
        profextN = profextN / abso
      else
        profextN = 0.
      endif

!write(245,*)'absoN',abso

return
end subroutine absoN
 
 
