! /***************************************************************************
!  * Calcule le rayonnement intercept� par une loi de Beer utilisant le
!  * param�tre P_extin  (recommand� pour la plupart des cultures herbac�es)
!  */
! ml_com !
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
! - Stics book paragraphe 3.2.1, page 49
!
!! This module calculates the radiation interception, assuming that the canopy is a homogenous environment with leaves being randomly distributed over the area.
!! A consequence of this random, homogeneous representation is that it allows the use of an optical analogy (Beer's law) to estimate the interception of
!! photosynthetically active radiation.
!!
!! Thus, the radiation intercepted by the crop (raint) is expressed according to a Beer's law function of LAI.  extin is a daily extinction coefficient and
!! parsurrg is a climatic parameter corresponding to the ratio of photosynthetically active radiation to the global radiation, trg
!! For homogenous crops, crop height is deduced from the leaf area index or the ground cover. It serves particularly in the calculation module for
!! water requirements via the resistive option. khaut is assumed to be plant-independent (a general value of 0.7 is proposed) while the potential height
!! of foliage growth is mostly plant-dependent and defined by the two limits hautbase and hautmax.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine beer(P_codelaitr, raint, P_parsurrg, P_extin, lai, eai, trg, tauxcouv, rombre, rsoleil, parapluie, &
                hauteur, P_hautmax, surface, ipl, surfaceSous)

   implicit none

   !: Arguments
   integer, intent(IN) :: P_codelaitr  ! // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
   real, intent(OUT)   :: raint   ! // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
   real, intent(IN)    :: P_parsurrg  ! // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1
   real, intent(IN)    :: P_extin  ! // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
   real, intent(IN)    :: lai   ! // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
   real, intent(IN)    :: eai
   real, intent(IN)    :: trg   ! // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
   real, intent(IN)    :: tauxcouv   ! // OUTPUT // Cover rate // SD
   real, intent(OUT)   :: rombre   ! // OUTPUT // Radiation fraction in the shade // 0-1
   real, intent(OUT)   :: rsoleil   ! // OUTPUT // Radiation fraction in the full sun // 0-1
   integer, intent(OUT)   :: parapluie
   real, intent(INOUT) :: hauteur   ! // OUTPUT // Height of canopy // m
   real, intent(IN)    :: surface(2)
   real, intent(IN)    :: P_hautmax  ! // PARAMETER // Maximum height of crop // m // PARPLT // 1

   integer, intent(IN)    :: ipl
   real, intent(OUT)   :: surfaceSous(2)

   if (P_codelaitr == 1) then
      !: NB le 28/03/02 ajout de eai
      raint = 0.95*P_parsurrg*(1.0 - (exp(-P_extin*(lai + eai))))*trg
   else
      !: Nb le 15/06
      raint = 0.95*tauxcouv*trg*P_parsurrg
   end if

   rombre = 0.0
   rsoleil = 0.0

   !: NB le 26/3/98 pour irrig
   if (ipl == 1) then
      if (P_codelaitr == 1) then
         surfaceSous(1) = exp(-P_extin*lai)
      else
         surfaceSous(1) = 1 - tauxcouv
      end if
      surfaceSous(2) = 1 - surface(1)
      ! Seuillage des surfaces
      if (surfaceSous(2) <= 0.0) then
         surfaceSous(2) = 0.001
         surfaceSous(1) = 1 - surfaceSous(2)
      end if
   end if

   !: Pas d'effet parapluie quand on utilise une loi de Beer
   parapluie = 0

   if (hauteur > P_hautmax) hauteur = P_hautmax

   return
end subroutine beer

! /***************************************************************************
!  * Computes the linght interception using the Beer-Lambert law of extinction
!  * for two mixed crops.
!  */
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c!
! - Stics book paragraphe 3.2.1, page 49
!
!! This module calculates the radiation interception, assuming that the canopy is a homogenous environment with leaves being randomly distributed over the area.
!! A consequence of this random, homogeneous representation is that it allows the use of an optical analogy (Beer's law) to estimate the interception of
!! photosynthetically active radiation.
!!
!! Thus, the radiation intercepted by the crop (raint) is expressed according to a Beer's law function of the LAI. extin is a daily extinction coefficient and
!! parsurrg is a climatic parameter corresponding to the ratio of photosynthetically active radiation to the global radiation, trg
!! Crop height is computed either using the plant leaf area index, the ground cover or the biomass. It serves particularly in the calculation module for
!! water requirements via the resistive option. khaut is assumed to be plant-independent (a general value of 0.7 is proposed) while the potential height
!! of foliage growth is mostly plant-dependent and defined by the two limits hautbase and hautmax.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine beer2(P_codelaitr,raint,P_parsurrg,P_extin,P_extin2,lai,lai2,eai,eai2,trg,tauxcouv,rombre,rsoleil,parapluie,nsen,  &
                 nlax, P_codlainet, hauteur, deltahauteur, P_hautmax, P_hautbase, P_khaut, P_hautK, P_hautA, laisen, surface, &
                 ipl, surfaceSous, masec, densite, ef_elongation, P_hautdens, P_code_shape, &
                 P_haut_dev_x0, P_haut_dev_k, somupvtsem)

   implicit none

   !: Arguments
   integer, intent(IN)    :: P_codelaitr  ! // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
   real, intent(OUT)   :: raint   ! // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
   real, intent(IN)    :: P_parsurrg  ! // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1
   real, intent(IN)    :: P_extin  ! // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
   real, intent(IN)    :: P_extin2 ! // PARAMETER // extinction coefficient of photosynthetic active radiation canopy of the other plant // SD // PARPLT // 1
   real, intent(IN)    :: lai   ! // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
   real, intent(IN)    :: lai2  ! // OUTPUT // Leaf area index of the other plant // m2 leafs  m-2 soil
   real, intent(IN)    :: eai
   real, intent(IN)    :: eai2
   real, intent(IN)    :: trg   ! // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
   real, intent(IN)    :: tauxcouv   ! // OUTPUT // Cover rate // SD
   real, intent(OUT)   :: rombre   ! // OUTPUT // Radiation fraction in the shade // 0-1
   real, intent(OUT)   :: rsoleil   ! // OUTPUT // Radiation fraction in the full sun // 0-1
   integer, intent(OUT)   :: parapluie
   integer, intent(IN)    :: nsen
   integer, intent(IN)    :: nlax
   integer, intent(IN)    :: P_codlainet  ! // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
   real, intent(INOUT) :: hauteur   ! // OUTPUT // Height of canopy // m
   real, intent(INOUT) :: deltahauteur
   real, intent(IN)    :: P_hautmax  ! // PARAMETER // Maximum height of crop // m // PARPLT // 1
   real, intent(IN)    :: P_hautbase  ! // PARAMETER // Base height of crop // m // PARPLT // 1
   real, intent(IN)    :: P_khaut  ! // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
   real, intent(INOUT) :: P_hautK ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(INOUT) :: P_hautA ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(IN)    :: P_haut_dev_x0 ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: P_haut_dev_k ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: somupvtsem   ! // INPUT // sum of development units from sowing // degree C
   real, intent(IN)    :: laisen   ! // INPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
   real, intent(IN)    :: surface(2)
   real, intent(IN)    :: masec
   real, intent(IN)    :: densite   ! // OUTPUT // Actual sowing density // plants.m-2
   real, intent(OUT)   :: ef_elongation ! // OUTPUT // Shoot elongation effect due to plant shading // 1-P_elongation
   real, intent(IN)    :: P_hautdens   !// PARAMETER // density at which the hauteur~masec relationship was measured // plants m-2 // PARAMv6 // 1
   integer, intent(IN)    :: P_code_shape  ! // PARAMETER // code for plant shape computation (default) using LAI, (2) using masec, (3) using dev // PARPLT // 1
   integer, intent(IN)    :: ipl
   real, intent(OUT)   :: surfaceSous(2)
   real :: equi_masec
   real :: fapar       ! fraction of light absorbed by the plant

   if (P_codelaitr == 1) then
      fapar = ((lai + eai)*P_extin)/((lai + eai)*P_extin + (lai2 + eai2)*P_extin2)* &
              (1 - exp(-(lai + eai)*P_extin - (lai2 + eai2)*P_extin2))
      raint = 0.95*P_parsurrg*trg*fapar
      ! print *, "lai",lai,"lai2",lai2,"extin",P_extin,"extin2",P_extin2,"fapar:",fapar,"raint:",raint
   else
      !: Nb le 15/06
      raint = 0.95*tauxcouv*trg*P_parsurrg
   end if

   rombre = 0.0
   rsoleil = 1.0

   !: Pas d'effet parapluie quand on utilise une loi de Beer
   parapluie = 0

   return
end subroutine beer2

! /***************************************************************************
!  * Computes the plant height
!  */
! *---------------------------------------------------------------------------
!
!! This subroutine computes the plant height using either the increment in biomass (masec), lai,
!! ground cover or phasic development (somupvtsem).
!! The plant height can take into account the elongation effect when one plant is shaded in the
!! case of intercroping. It also takes into account the nitrogen, water, frost and root anoxy stresses.
!! The plant height growth can be stopped at any developpement stage. See subroutine raytrans for the
!! computation of `compute_height` that controls it.
!! It is particularly used for the calculation of water requirements via the resistive option.
!! khaut is assumed to be plant-independent (a general value of 0.7 is proposed) while the potential height
!! of foliage growth is mostly plant-dependent and defined by the two limits hautbase and hautmax.
! *-----------------------------------------------------------------------------------------------------------------------------------------------------------* c
subroutine height(P_codelaitr, lai, tauxcouv, nlax, nsen, P_codlainet, hauteur, deltahauteur, P_hautmax, P_hautbase, &
                  P_khaut, P_hautK, P_hautA, laisen, masec, densite, ef_elongation, P_hautdens, P_code_shape, &
                  P_haut_dev_x0, P_haut_dev_k, somupvtsem, inns, turfac, P_nw_height, ef_n_w_height, &
                  potential_height, fstressgel, exolai)

   implicit none

   !: Arguments
   integer, intent(IN)    :: P_codelaitr  ! // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
   real, intent(IN)    :: lai          ! // OUTPUT // Leaf area index // m2 leafs  m-2 soil
   real, intent(IN)    :: tauxcouv   ! // OUTPUT // Cover rate // SD
   integer, intent(IN)    :: P_codlainet  ! // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
   real, intent(INOUT) :: hauteur   ! // OUTPUT // Height of canopy // m
   real, intent(INOUT) :: deltahauteur
   real, intent(INOUT) :: potential_height ! Plant height without stress.
   real, intent(IN)    :: P_hautmax  ! // PARAMETER // Maximum height of crop // m // PARPLT // 1
   real, intent(IN)    :: P_hautbase  ! // PARAMETER // Base height of crop // m // PARPLT // 1
   real, intent(IN)    :: P_khaut  ! // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
   real, intent(INOUT) :: P_hautK ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(INOUT) :: P_hautA ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(IN)    :: P_haut_dev_x0 ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: P_haut_dev_k ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: somupvtsem   ! // INPUT // sum of development units from sowing // degree C
   real, intent(IN)    :: laisen   ! // INPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
   real, intent(IN)    :: masec         ! // INPUT // aboveground biomass // t ha-1
   real, intent(IN)    :: densite   ! // OUTPUT // Actual sowing density // plants.m-2
   real, intent(OUT)   :: ef_elongation ! // OUTPUT // Shoot elongation effect due to plant shading // 1-P_elongation
   real, intent(IN)    :: P_hautdens   !// PARAMETER // density at which the hauteur~masec relationship was measured // plants m-2 // PARAMv6 // 1
   integer, intent(IN)    :: P_code_shape  ! // PARAMETER // code for plant shape computation (default) using LAI, (2) using masec, (3) using dev // PARPLT // 1
   integer, intent(IN)    :: nsen
   integer, intent(IN)    :: nlax
   real, intent(IN)    :: inns   ! // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
   real, intent(IN)    :: turfac   ! // OUTPUT // Index of turgescence water stress  // 0-1
   real, intent(IN)    :: fstressgel   ! // OUTPUT // Frost index on the LAI // 0-1
   real, intent(IN)    :: exolai   ! // OUTPUT // Index for excess water active on growth in biomass // 0-1
   real, intent(IN)    :: P_nw_height ! // PARAMETER // used to compute the stress effect of water and nitrogen on height
   real, intent(OUT)   :: ef_n_w_height ! // OUTPUT // Effect of water and nitrogen deficit on height (0-1)
   real :: equi_masec
   real :: hauteurjour ! Used for lai, or in masec to compute a new height while keeping the previous value to check that it did not decrease due to a change in elongation.
   real :: potential_delta_height
   real :: potential_height_veille
   real :: overall_stress

   ! Initializing the potential height from the day before at the potential height before it is re-computed:
   potential_height_veille = potential_height

   ! Computing the effect of water and nitrogen deficit on height:
   if (P_nw_height == 0.0) then
      ef_n_w_height = 1.0
   else
      overall_stress = min(inns, turfac, fstressgel, exolai)
      ef_n_w_height = min(1 - (1 - overall_stress)*P_nw_height, 1.0)
   end if
   ! RV: P_nw_height is used to set the importance of the stress effect on height. It is between 0 and 1.
   ! close to 0 means low effect, close to 1 means strong effect (1 means = to overall_stress).
   ! NB: The computation is repeated in the formplante subroutine (the height is computed differently there)

   ! Computing plant heigth:
   if (P_code_shape == 2) then
      ! RV: The heigth ~ masec relationship is fitted at a given density. To make this relationship kind of independent
      ! from the density, we correct the current masec by the current density using P_hautdens (the density
      ! at which the relationship was measured). This is done because the heigth should be the same for a
      ! low and high sowing density.
      ! NB: This trick also works for intercropping whatever the sowing density.
      equi_masec = masec*(P_hautdens/densite)
      ! Computing the height from an allometry with the dry mass (masec):
      potential_height = P_hautbase + (P_hautK*(equi_masec**P_hautA))

   else if (P_code_shape == 3) then
      ! RV: Third choice, the height is computed using development units. This makes the height independent
      ! from the biomass, the planting density and the lai (which is true for many species).
      ! Computing the height from an allometry with the phasic development (somupvtsem):
      potential_height = P_hautbase + P_hautmax/(1 + EXP(-P_haut_dev_k*(somupvtsem - P_haut_dev_x0)))
   else
      ! RV: Actual default choice, as before using LAI or tauxcouv
      if (P_codelaitr == 1) then
         if (nsen == 0) then
            if (nlax > 0 .and. nsen == 0) then
               if (P_codlainet == 1) potential_height = hauteur + deltahauteur
            else
               potential_height = (P_hautmax - P_hautbase)*(1 - exp(-P_khaut*(lai + laisen))) + P_hautbase
            end if
         end if
      else
         if (nsen == 0) then
            if (nlax > 0 .and. nsen == 0) then
               potential_height = hauteur + deltahauteur
            else
               potential_height = P_hautbase + (P_hautmax - P_hautbase)*tauxcouv
            end if
         end if
      end if
   end if

   ! The potential increment in height is the difference between the potential height from today and the day before
   potential_delta_height = potential_height - potential_height_veille

   ! The actual increment in height is:
   deltahauteur = potential_delta_height*ef_n_w_height*ef_elongation

   ! And so is the height:
   hauteurjour = hauteur + deltahauteur

   ! hauteur is only updated if it is not lower than the previous day:
   if (hauteurjour > hauteur) then
      hauteur = hauteurjour
   end if

   if (hauteur > P_hautmax) hauteur = P_hautmax

   return
end subroutine height
