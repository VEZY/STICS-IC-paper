
! *************************************************************
!! * subroutine de calcul des dates d'apport d'eau en upvt     *
!! *************************************************************

!subroutine calculApportsIrrigationEnUpvt(jjul,nlev,upvt,P_upvttapI,P_doseI,        &
!                                         somupvtI,numeroappI,P_julapI,airg,nap)

subroutine calculApportsIrrigationEnUpvt(sc,p,itk)

USE Stics
USE Plante
USE Itineraire_technique

    implicit none

    type(Stics_Communs_),  intent(INOUT) :: sc  
    type(Plante_),         intent(INOUT) :: p  
    type(ITK_),            intent(INOUT) :: itk  

!    integer, intent(IN) :: jjul
!    integer, intent(IN) :: nlev
!    real,    intent(IN) :: upvt
!    real,    intent(IN) :: P_upvttapI
!    real,    intent(IN) :: P_doseI
!
!    real,    intent(INOUT) :: somupvtI
!    integer, intent(INOUT) :: numeroappI
!    integer, intent(INOUT) :: julappI
!    real,    intent(INOUT) :: airg
!    integer, intent(INOUT) :: nap

    if (p%nlev > 0) then

      p%somupvtI = p%somupvtI + p%upvt(sc%n)

      if (itk%P_upvttapI(itk%numeroappI) /= 0 .and. p%somupvtI >= itk%P_upvttapI(itk%numeroappI)) then
        itk%P_julapI(itk%numeroappI) = sc%jjul
        sc%airg(sc%n) = itk%P_doseI(itk%numeroappI)
        itk%numeroappI = itk%numeroappI+1
        itk%nap = itk%numeroappI-1
      endif

    endif

return
end subroutine calculApportsIrrigationEnUpvt
 
 
