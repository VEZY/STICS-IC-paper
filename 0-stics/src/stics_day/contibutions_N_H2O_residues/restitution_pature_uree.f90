subroutine restitution_pature_uree(n,CNplante,msrec_fou,coef_calcul_doseN,perte,anit)
! DR 30/03/2016 on implemente la prise en compte de l'urée des vaches comme un apport d'engrais mineral
    implicit none

real , intent(IN) :: CNplante   ! Cnplante doit etre *10 pour passer en g/kg
real , intent(IN) :: msrec_fou, coef_calcul_doseN, perte
integer , intent(IN) :: n
real , intent(OUT) :: anit
real :: doseN

  ! attention 14/04/2016 changement d'unite

    if(CNplante*10. .le. coef_calcul_doseN)then
         doseN = 0.
    else
  ! DR et FR 25/07/2016 on consreve l'equation du debut apres confirmation de AIG
  ! il faut utiliser la teneur en azote de l’herbe exprimée en gN/kgMS.
        doseN=((CNplante*10.-coef_calcul_doseN)*msrec_fou)*(1-perte)
        ! doseN=((CNplante-coef_calcul_doseN)*msrec_fou)*(1-perte)
    endif

    anit=doseN

end subroutine restitution_pature_uree
