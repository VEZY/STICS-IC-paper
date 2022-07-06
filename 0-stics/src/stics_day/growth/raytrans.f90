! **************************************************************** c
! * sous-programme de transfert radiatif                         * c
! * selon la valeur de P_codetransrad, ce sous-programme           * c
! * calcule le rayonnement intercept� par                        * c
! * 1 - une loi de Beer utilisant le param�tre P_EXTIN             * c
! * (recommand� pour la plupart des cultures herbac�es)          * c
! * 2-  un mod�le de transfert radiatif prenant en compte        * c
! * la g�om�trie du couvert (recommand� pour arbres et           * c
! * cultures principales dans les associations)                  * c
! **************************************************************** c
! This subroutine calculates the radiation interception.
! - Stics book paragraphe 3,2, page 49
!
!! Since most crop models are devoted to industrial crops the canopy is assumed to be a homogenous environment with leaves being randomly distributed over the area.
!! A consequence of this random, homogeneous representation is that it allows the use of an optical analogy (Beer's law) to estimate the interception of
!! photosynthetically active radiation. This law, having only one parameter (the extinction coefficient), has been thoroughly studied for many crops
!! (Varlet-Grancher et al., 1989): the more erect the plant, the smaller is the extinction coefficient. This approach is very successful for homogenous crops,
!! but poorly suited to canopies in rows or during the first stages of an annual crop because the homogeneity hypothesis cannot apply.
!! Consequently, like CROPGRO (Boote and Pickering, 1994) the STICS model can simulate canopies in rows, with prediction of light interception dependent
!! not only on LAI, but also on plant height and width, row spacing, plant spacing and direct and diffuse light absorption. Such capabilities are also required
!! to simulate intercropping.
!!
!! Thus in STICS two options are available to calculate radiation interception: a simple Beer's law, recommended for homogenous crops (see beer.f90),
!! and a more complex calculation for radiation transfers within the canopy, recommended for crops in rows (see transrad.f90). If the leaf status variable
!! is the ground cover and not the leaf area index, then only the Beer's law option is permitted.
!-----------------------------------------------------------
subroutine raytrans(P_codetransrad, P_extin, P_extin2, cumrg, cumraint, P_adfol, lairognecum, laieffcum, P_dfolbas, &
                   P_dfolhaut, dfol, rdif, parapluie, raint, P_parsurrg, P_forme, lai, lai2, laisen, eai, eai2, P_interrang, nlax, &
                    nsen, P_codlainet, P_hautbase, P_codepalissage, P_hautmaxtec, P_largtec, originehaut, hauteur, deltahauteur, &
                    P_hautmax, varrapforme, largeur, jul, trg, P_latitude, rombre, rsoleil, P_orientrang, P_ktrou, P_codelaitr, &
                   P_khaut, tauxcouv, surface, surfaceSous, ipl, P_nbplantes, ens, surfAO, surfAS, P_hautK, P_hautA, P_code_shape, &
                    masec, densite, nflo, ndrp, nmat, nrec, P_stage_const_height, P_elongation, P_hautdens, ef_elongation, &
                    true_dominance, P_haut_dev_x0, P_haut_dev_k, somupvtsem, inns, turfac, P_nw_height, ef_n_w_height, &
                    potential_height, fstressgel, exolai, P_code_strip, P_nrow, light_beer_active)

   implicit none

   !: Arguments

   ! RAYTRANS
   integer, intent(IN)    :: P_codetransrad  ! // PARAMETER // simulation option of radiation 'interception: law Beer (1), radiation transfers (2) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: surface(2)
   real, intent(OUT)   :: surfaceSous(2)
   integer, intent(IN)    :: ipl
   integer, intent(IN)    :: P_nbplantes  ! // PARAMETER // number of simulated plants // SD // P_USM/USMXML // 0
   integer, intent(IN)    :: ens
   real, intent(INOUT) :: P_extin  ! // PARAMETER // extinction coefficient of photosynthetic active radiation canopy // SD // PARPLT // 1
   real, intent(INOUT) :: P_extin2 ! // PARAMETER // extinction coefficient of photosynthetic active radiation canopy for the other plant if any// SD // PARPLT // 1
   real, intent(INOUT) :: cumrg   ! // OUTPUT // Sum of global radiation during the stage sowing-harvest   // Mj.m-2
   real, intent(INOUT) :: cumraint   ! // OUTPUT // Sum of intercepted radiation  // Mj.m-2
   ! TRANSRAD
   real, intent(IN)    :: P_adfol  ! // PARAMETER // parameter determining the leaf density evolution within the chosen shape // m-1 // PARPLT // 1
   real, intent(IN)    :: lairognecum
   real, intent(IN)    :: laieffcum
   real, intent(IN)    :: P_dfolbas  ! // PARAMETER // minimal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
   real, intent(IN)    :: P_dfolhaut  ! // PARAMETER // maximal foliar density within the considered shape // m2 leaf m-3 // PARPLT // 1
   real, intent(OUT)   :: dfol   ! // OUTPUT //  "Within the shape  leaf density" // m2 m-3
   real, intent(OUT)   :: rdif   ! // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
   integer, intent(OUT)   :: parapluie
   real, intent(OUT)   :: raint   ! // OUTPUT // Photosynthetic active radiation intercepted by the canopy  // Mj.m-2
   real, intent(IN)    :: P_parsurrg  ! // PARAMETER // coefficient PAR/RG for the calculation of PAR  // * // STATION // 1

   integer, intent(IN)    :: P_forme  ! // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: lai      ! // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
   real, intent(IN)    :: lai2     ! // OUTPUT // Leaf area index of the other plant // m2 leafs  m-2 soil
   real, intent(IN)    :: laisen   ! // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
   real, intent(IN)    :: eai
   real, intent(IN)    :: eai2
   real, intent(IN)    :: P_interrang  ! // PARAMETER // Width of the P_interrang // m // PARTEC // 1
   real, intent(IN)    :: nlax
   real, intent(IN)    :: nsen
   real, intent(IN)    :: nflo
   real, intent(IN)    :: ndrp
   real, intent(IN)    :: nmat
   real, intent(IN)    :: nrec
   real, intent(IN)    :: P_codlainet  ! // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: P_hautbase  ! // PARAMETER // Base height of crop // m // PARPLT // 1
   integer, intent(IN)    :: P_codepalissage  ! // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0
   real, intent(IN)    :: P_hautmaxtec  ! // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1
   real, intent(IN)    :: P_largtec  ! // PARAMETER // technical width // m // PARTEC // 1
   real, intent(IN)    :: originehaut
   integer, intent(IN)    :: P_code_shape  ! // PARAMETER // code for plant shape computation (default) using LAI, (2) using masec, (3) using dev // PARPLT // 1
   real, intent(IN)    :: masec
   real, intent(IN)    :: densite   ! // OUTPUT // Actual sowing density // plants.m-2

   real, intent(INOUT) :: hauteur   ! // OUTPUT // Height of canopy // m
   real, intent(INOUT) :: deltahauteur
   real, intent(INOUT) :: P_hautmax  ! // PARAMETER // Maximum height of crop // m // PARPLT // 1
   real, intent(INOUT) :: P_hautK ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(INOUT) :: P_hautA ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(INOUT) :: varrapforme
   real, intent(IN)    :: P_haut_dev_x0 ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: P_haut_dev_k ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: somupvtsem   ! // INPUT // sum of development units from sowing // degree C

   real, intent(OUT)   :: largeur   ! // OUTPUT // Width of the plant shape  // m

   integer, intent(IN)    :: jul
   real, intent(IN)    :: trg   ! // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
   real, intent(IN)    :: P_latitude  ! // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0

   real, intent(OUT)   :: rombre   ! // OUTPUT // Radiation fraction in the shade // 0-1
   real, intent(OUT)   :: rsoleil   ! // OUTPUT // Radiation fraction in the full sun // 0-1
   real, intent(IN)    :: P_orientrang  ! // PARAMETER // Direction of ranks // rd (0=NS) // PARTEC // 1
   real, intent(IN)    :: P_ktrou  ! // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1

   !  real,    intent(INOUT) :: surf(2,2)
   !  integer, intent(IN)    :: ipl
   !  integer, intent(IN)    :: P_nbplantes
   ! -- FIN TRANSRAD

   ! BEER
   integer, intent(IN)    :: P_codelaitr  ! // PARAMETER // choice between soil cover or LAI calculation // code 1/2 // PARPLT // 0
   !  real,    intent(OUT)   :: raint
   !  real,    intent(IN)    :: P_parsurrg
   !  real,    intent(IN)    :: P_extin
   !  real,    intent(IN)    :: lai
   !  real,    intent(IN)    :: eai
   !  real,    intent(IN)    :: trg
   real, intent(IN)    :: tauxcouv   ! // OUTPUT // Cover rate // SD
   !  real,    intent(OUT)   :: rombre
   !  real,    intent(OUT)   :: rsoleil
   !  integer, intent(OUT)   :: parapluie
   !  integer, intent(IN)    :: nsen
   !  integer, intent(IN)    :: nlax
   !  integer, intent(IN)    :: P_codlainet
   !  real,    intent(INOUT) :: hauteur
   !  real,    intent(INOUT) :: deltahauteur
   !  real,    intent(IN)    :: P_hautmax
   !  real,    intent(IN)    :: P_hautbase
   real, intent(IN)    :: P_khaut  ! // PARAMETER // Extinction Coefficient connecting leaf area index to height crop // * // PARAM // 1
   !  real,    intent(IN)    :: laisen

   !  integer, intent(IN)    :: ipl
   !  integer, intent(OUT)   :: surf(2,2)
   ! -- FIN BEER
   real, intent(INOUT)   :: surfAO
   real, intent(INOUT)   :: surfAS
   character(len=3), intent(IN) :: P_stage_const_height  ! // PARAMETER // stage when height growth stops (lax, sen, flo, mat, rec)
   real, intent(IN)    :: P_elongation ! // PARAMETER // plant elongation parameter: more elongation when >1.0 compared to reference in sole crop // factor // PARAMv6 // 1
   real, intent(IN)    :: P_hautdens   !// PARAMETER // density at which the hauteur~masec relationship was measured // plants m-2 // PARAMv6 // 1
   real, intent(INOUT)   :: ef_elongation ! // OUTPUT // Shoot elongation effect due to plant shading // 1-P_elongation
   logical, intent(IN)    :: true_dominance ! // INPUT // Is there a true dominance between both crops (for intercropping) ? (delta hauteur > P_hauteur_threshold)
   real, intent(IN)    :: inns   ! // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
   real, intent(IN)    :: turfac   ! // OUTPUT // Index of turgescence water stress  // 0-1
   real, intent(IN)    :: P_nw_height ! // PARAMETER // used to compute the stress effect of water and nitrogen on height
   real, intent(OUT)   :: ef_n_w_height ! // OUTPUT // Effect of water and nitrogen deficit on height (0-1)
   real, intent(INOUT) :: potential_height
   real, intent(IN)    :: fstressgel   ! // OUTPUT // Frost index on the LAI // 0-1
   real, intent(IN)    :: exolai   ! // OUTPUT // Index for excess water active on growth in biomass // 0-1
   integer, intent(IN)    :: P_code_strip ! // PARAMETER //  is it a strip intercrop? // PARAMv6 // 1: yes, 2: no
   integer, intent(IN)    :: P_nrow ! // PARAMETER // number of plant rows in the strip
   integer, intent(INOUT) :: light_beer_active ! Is Beer-Lambert law used for light interception ?
   logical :: compute_height ! Do we have to compute plant height or not ? (in case we compute it with masec, P_code_shape== 2)
   ! variables locales pass� en commun
   !    real :: surfAO  !
   !    real ::  surfAS

   ! RV: Plant height is linked to lai/dry mass/phasic development until a user specified development stage (or
   ! until the end of the simulation), then it remain constant.
   compute_height = .TRUE.

   SELECT CASE (P_stage_const_height)
   case default
      compute_height = .TRUE.
   case ('lax')
      ! max. leaf area index, end of leaf growth
      if (nlax > 0.) compute_height = .FALSE.
   case ('sen')
      ! start of leaf senescence
      if (nsen > 0.) compute_height = .FALSE.
   case ('flo')
      ! flowering
      if (nflo > 0.) compute_height = .FALSE.
   case ('drp')
      ! Start of grain filling
      if (ndrp > 0.) compute_height = .FALSE.
   case ('mat')
      ! Physiological maturity
      if (nmat > 0.) compute_height = .FALSE.
   case ('rec')
      ! Harvest
      if (nrec > 0.) compute_height = .FALSE.
   end select

   ! If harvest, height is reset to 0.0:
   if (nrec > 0.) then
      compute_height = .FALSE.
      hauteur = 0.0
   end if

   ! print *, "Test 1 P_extin", P_extin, "P_extin2", P_extin2, "P_codetransrad",P_codetransrad,"P_nbplantes",P_nbplantes
   ! pause
   !: loi de Beer
   if ((P_codetransrad /= 2) .or. (true_dominance .eqv. .false.)) then
      ! NB: if not true_dominance, then the crops canopies are mixed -> go back to beer
      light_beer_active = 1
      if (P_nbplantes .eq. 1) then
         call beer(P_codelaitr, raint, P_parsurrg, P_extin, lai, eai, trg, tauxcouv, rombre, rsoleil, &
                   parapluie, hauteur, P_hautmax, surface, ipl, surfaceSous)
      else
         ! If intercropping, make a different computation.
         if ((lai + eai) .gt. 0.0) then
        call beer2(P_codelaitr, raint, P_parsurrg, P_extin, P_extin2, lai, lai2, eai, eai2, trg, &
                   tauxcouv, rombre, rsoleil, parapluie, nsen, nlax, P_codlainet, hauteur, &
                   deltahauteur, P_hautmax, P_hautbase, P_khaut, P_hautK, P_hautA, laisen, surface, &
                   ipl, surfaceSous, masec, densite, ef_elongation, P_hautdens, P_code_shape, &
                   P_haut_dev_x0, P_haut_dev_k, somupvtsem)
         else
            raint = 0.0
         end if
      end if

      ef_elongation = 1.0 ! no elongation effect because crops are considered homogeneously mixed here.

      if (compute_height) then
         call height(P_codelaitr, lai, tauxcouv, nlax, nsen, P_codlainet, hauteur, deltahauteur, P_hautmax, P_hautbase, &
                     P_khaut, P_hautK, P_hautA, laisen, masec, densite, ef_elongation, P_hautdens, P_code_shape, &
                     P_haut_dev_x0, P_haut_dev_k, somupvtsem, inns, turfac, P_nw_height, ef_n_w_height, &
                     potential_height, fstressgel, exolai)
      end if

   else
      light_beer_active = 0

      ! 18/03/08 DR et ML
      !*****************************************
      ! pour la vigne il arrive qu'apres lev on est encore un lai nul si par exemple
      ! il fait trop froid (tempeff = 0) donc quand on rentre dans raytrans on a des pbs
      ! avec largeur = 0 division par zero et tout le binz
      ! on a mis un test qui nous semble logique mais qu'il faut discuter avce le chef

      if (lai + eai .gt. 0.0) then
         ! RV: Compute an elongation effect in case the plant is shaded.
         ! ef_elongation is between 1.0 (no elongation, the plant is in full sun)
         ! and P_elongation (the plant is fully shaded). It is linearly related to the
         ! shading (i.e. the surface of the plant that is shaded):
         ef_elongation = surface(2)*(P_elongation - 1.0) + 1.0

         call transrad(P_adfol, lairognecum, laieffcum, P_dfolbas, P_dfolhaut, dfol, rdif, parapluie, &
                       raint, P_parsurrg, P_forme, lai, laisen, eai, P_interrang, nlax, nsen, P_codlainet, &
                       P_hautbase, P_codepalissage, P_hautmaxtec, P_largtec, originehaut, hauteur, &
                       deltahauteur, P_hautmax, varrapforme, largeur, jul, trg, P_latitude, rombre, &
                       rsoleil, P_orientrang, P_ktrou, surfAO, surfAS, ipl, P_hautK, P_hautA, P_code_shape, &
                       masec, densite, compute_height, P_hautdens, &
                       ef_elongation, P_haut_dev_x0, P_haut_dev_k, somupvtsem, inns, turfac, &
                       P_nw_height, ef_n_w_height, potential_height, fstressgel, exolai, &
                       P_code_strip, P_nrow)

         ! Register the relative surfaces of the plane below the plant:
         surfaceSous(1) = surfAS
         surfaceSous(2) = surfAO
         ! /!\ RV: here surfAS/AO are the relative surfaces of the sunlit/shaded parts of the
         ! plane below the current plant, **not** the surfaces of the compartments of the plant
      else
         raint = 0.
      end if
   end if

   !: cumul de rayonnement
   cumrg = cumrg + (trg*surface(ens))

   !: cumul du rayonnement intercept�
   cumraint = cumraint + (raint*surface(ens))

   return
end subroutine raytrans
