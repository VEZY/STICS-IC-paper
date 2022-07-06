! version 5.0
!  derniere modif 14/05/01                                     * c
!  sous-programme de transfert radiatif                         * c
!  un mod�le de transfert radiatif prenant en compte            * c
!  la g�om�trie du couvert (recommand� pour arbres et           * c
!  cultures principales dans les associations)                  * c
!
! This subroutine calculates the radiation interception, with prediction of light interception dependent not only on LAI, but also on plant height and width,
! row spacing, plant spacing and direct and diffuse light absorption.
! - Stics book paragraphe 3.2.2, page 50-51
!
!! A calculation of radiation transfer enables an estimate of the radiation intercepted by a crop in rows, taking account of its geometry in a simple fashion.
!! The objective is to estimate, on a daily time step, the fraction of radiation intercepted by the crop and fraction part transmitted to the layer below,
!! which can be either the soil or another crop (case of intercropping). To calculate those two components, the soil surface is split into a shaded part and
!! a sunlit part and by convention the shaded part corresponds to the vertical projection of the crop foliage onto the soil surface. The available daily variables
!! are the Leaf Area Index (LAI), calculated independently and the global radiation (trg)
!!
!! The simplest method of calculating the radiation received at a given point X (located on the soil in the inter-row) is to calculate angles H1 and H2
!! corresponding to the critical angles below which point X receives the total radiation directly.  At angles below H1 and above H2, point X receives an amount
!! of radiation below the total radiation value, due to absorption by the crop. Within those angle windows, Beer�s law is used to estimate the fraction of
!! transmitted radiation. It is assumed that a canopy can be represented by a simple geometric shape (rectangle or triangle) and that it is isotropically infinite.
!! We can therefore describe the daily radiation received at point X as the sum of the radiation not intercepted by the crop (rdroit)(sun at an angle between H1 and H2)
!! and the radiation transmitted (rtransmis).  The "infinite canopy" hypothesis allows us to assume that when the sun is at an angle below H1 and H2,
!! all the radiation passes through the crop. Each part of the radiation received at X includes a direct component and a diffuse component.
!! Let us assume that, for the transmitted part, the same extinction coefficient (ktrou) applies to both components (which is generally accepted to be the case
!! when the general Beer law is used with a daily time scale).
!!
!! In contrast, for rdroit, direct and diffuse components should be separated because of the directional character of the direct component, which requires
!! the calculation of separate proportions of radiation reaching the soil (kgdiffus and kgdirect are the proportions of diffuse radiation, rdiffus, and direct
!! radiation, rdirect, respectively, reaching the soil).
!---------------------------------------------------------------
subroutine transrad(P_adfol,lairognecum,laieffcum,P_dfolbas,P_dfolhaut,dfol,rdif,parapluie,raint,P_parsurrg,P_forme,lai,  &
                    laisen, eai, P_interrang, nlax, nsen, P_codlainet, P_hautbase, P_codepalissage, P_hautmaxtec, P_largtec, &
                    originehaut, hauteur, deltahauteur, P_hautmax, varrapforme, largeur, jul, trg, P_latitude, rombre, rsoleil, &
                    P_orientrang, P_ktrou, surfAO, surfAS, ipl, P_hautK, P_hautA, P_code_shape, masec, densite, &
                    compute_height, P_hautdens, ef_elongation, P_haut_dev_x0, P_haut_dev_k, &
                    somupvtsem, inns, turfac, P_nw_height, ef_n_w_height, potential_height, fstressgel, exolai, P_code_strip, &
                    P_nrow)

   implicit none
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
   real, intent(IN)    :: lai   ! // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
   real, intent(IN)    :: laisen   ! // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
   real, intent(IN)    :: eai
   real, intent(IN)    :: P_interrang  ! // PARAMETER // Width of the P_interrang // m // PARTEC // 1
   real, intent(IN)    :: nlax
   real, intent(IN)    :: nsen
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
   real, intent(INOUT) :: P_hautmax   ! // PARAMETER // Maximum height of crop // m // PARPLT // 1
   real, intent(INOUT) :: P_hautK ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(INOUT) :: P_hautA ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(IN)    :: P_haut_dev_x0 ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: P_haut_dev_k ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: somupvtsem   ! // INPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
   real, intent(INOUT) :: varrapforme
   real, intent(OUT)   :: largeur   ! // OUTPUT // Width of the plant shape  // m
   integer, intent(IN)    :: jul
   real, intent(IN)    :: trg   ! // OUTPUT // Active radiation (entered or calculated) // MJ.m-2
   real, intent(IN)    :: P_latitude  ! // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0
   real, intent(OUT)   :: rombre   ! // OUTPUT // Radiation fraction in the shade // 0-1
   real, intent(OUT)   :: rsoleil   ! // OUTPUT // Radiation fraction in the full sun // 0-1
   real, intent(IN)    :: P_orientrang  ! // PARAMETER // Direction of ranks // rd (0=NS) // PARTEC // 1
   real, intent(IN)    :: P_ktrou  ! // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1
   real, intent(INOUT) :: surfAO ! // OUTPUT // relative surface of the shaded compartment of the plane below the plant
   real, intent(INOUT) :: surfAS ! // OUTPUT // relative surface of the sunlit compartment of the plane below the plant
   integer, intent(IN)    :: ipl
   real, intent(IN)    :: P_hautdens    !// PARAMETER // density at which the hauteur~masec relationship was measured // plants m-2 // PARAMv6 // 1
   real, intent(IN)   :: ef_elongation ! // OUTPUT // Shoot elongation effect due to plant shading // 1-P_elongation
   logical, intent(IN)   :: compute_height ! Is plant heigth to be computed ?
   real, intent(IN)    :: inns   ! // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
   real, intent(IN)    :: turfac   ! // OUTPUT // Index of turgescence water stress  // 0-1
   real, intent(IN)    :: P_nw_height ! // PARAMETER // used to compute the stress effect of water and nitrogen on height
   real, intent(OUT)   :: ef_n_w_height ! // OUTPUT // Effect of water and nitrogen deficit on height (0-1)
   real, intent(INOUT) :: potential_height ! Plant height without stress.
   real, intent(IN)    :: fstressgel   ! // OUTPUT // Frost index on the LAI // 0-1
   real, intent(IN)    :: exolai   ! // OUTPUT // Index for excess water active on growth in biomass // 0-1
   integer, intent(IN)    :: P_code_strip ! // PARAMETER //  is it a strip intercrop? // PARAMv6 // 1: yes, 2: no
   integer, intent(IN)    :: P_nrow ! // PARAMETER // number of plant rows in the strip

   real :: calcul_RDif

   ! les VARIABLES LOCALES
   integer :: formetrans
   real    :: hauteurzero
   real    :: largtrans
   real    :: raptrans

   !: Transferts radiatifs
   if (P_code_shape == 2 .or. P_code_shape == 3) then
      if (compute_height) then
         ! New computation using masec or development units
         call formplante_mas(P_forme, masec, P_hautbase, P_codepalissage, &
                             P_hautmaxtec, P_largtec, originehaut, hauteur, P_hautmax, &
                             P_hautK, P_hautA, varrapforme, largeur, formetrans, &
                             raptrans, largtrans, hauteurzero, P_interrang, densite, &
                             ef_elongation, P_hautdens, P_code_shape, &
                             P_haut_dev_x0, P_haut_dev_k, somupvtsem, inns, turfac, &
                             P_nw_height, ef_n_w_height, potential_height, &
                             deltahauteur, fstressgel, exolai, P_code_strip, P_nrow)

         ! Computing the foliar density AFTER computing the plant shape. In this case, dfol can increase indefinitely
         ! to compensate for maximum height and width allowed
      else
         raptrans = varrapforme
         largtrans = largeur
         formetrans = P_forme
      end if

      if (hauteur > 0.00001) then
         select case (P_forme)
         case (1)
            dfol = ((lai + eai + lairognecum + laieffcum + laisen)/densite)/(hauteur*(largeur**2.))
         case (2)
            dfol = ((lai + eai + lairognecum + laieffcum + laisen)/densite)/((1./3.)*(hauteur*(largeur**2.)))
         case (3)
            dfol = ((lai + eai + lairognecum + laieffcum + laisen)/densite)/((1./3.)*(hauteur*(largeur**2.)))
         end select
      else
         dfol = 0.
      end if

   else
      ! Original computation using LAI, computing dfol BEFORE the plant shape.
      ! NB le 28/03/02 ajout de EAI
      ! - NB le 14/05 la densit� foliaire varie entre deux bornes
      ! - NB le 10/10/05 la densit� foliaire peut �tre d�croissante
      ! si P_adfol <0
      if (compute_height) then

         if (P_adfol > 0.) then
            dfol = P_adfol*(lai + eai + lairognecum + laieffcum + laisen)
         else
            dfol = P_adfol*(lai + eai + lairognecum + laieffcum + laisen) + P_dfolbas + P_dfolhaut
         end if
         if (dfol < P_dfolbas) dfol = P_dfolbas
         if (dfol > P_dfolhaut) dfol = P_dfolhaut
         call formplante(P_forme, lai, laisen, eai, P_interrang, nlax, nsen, P_codlainet, P_hautbase, P_codepalissage, &
                         P_hautmaxtec, P_largtec, originehaut, hauteur, deltahauteur, P_hautmax, varrapforme, dfol, &
                         largeur, formetrans, raptrans, largtrans, hauteurzero, inns, turfac, P_nw_height, fstressgel, exolai, &
                         potential_height, ef_elongation)
      else
         raptrans = varrapforme
         largtrans = largeur
         formetrans = P_forme
      end if
   end if
   !: Calcul de rdif : diffus/global
   rdif = calcul_RDif(trg, P_latitude, jul)

   !: Calcul du rayonnement transmis
   call rtrans(rombre, rsoleil, largtrans, P_latitude, jul, raptrans, P_interrang, formetrans, &
               hauteurzero, P_orientrang, rdif, P_ktrou, lai, eai, hauteur)

   !: Computation of the relative surfaces of the shaded/sunlit parts of the plane below the plant:
   surfAO = largeur/P_interrang
   if (rombre == 0.0) surfAO = 0.0 ! RV: when largeur is very low the first point is not even shaded
   surfAO = min(surfAO, 1.0)
   surfAS = 1.0 - surfAO

   if (surfAS <= 0.0) then
      surfAS = 0.001
      surfAO = 1.0 - surfAS
   end if

   if (ipl == 1) then
      !: Il y a un effet parapluie pour interception de la pluie
      parapluie = 1
   else
      parapluie = 0
   end if

   ! Intercepted radiation
   raint = P_parsurrg*trg*(1 - (rombre*surfAO) - (rsoleil*surfAS))

   if (raint < 0.0) raint = 0.0

   return
end subroutine transrad

!----------------------------------------------------------------
!  function permettant de calculer le rayonnement transmis
!  sous la culture dominante en un point quelconque entre
!  deux rangs
!  ir = P_interrang (m)
!  l = largeur du houppier
!  e = epaisseur du houppier
!  rap = rapport e/l
!  x = position du point par rapport au rang le plus proche (m)
!  haut = hauteur de la base du houppier (m)
!  P_forme = 1 pour rectangle et 2 pour triangle
!  si rap<0, triangle � l'envers
!  alpha = angle des rangs avec le nord (radian)
!  rdif = rapport diffus/total
!  dfol = densit� folaire (m3/m2)
!
subroutine rtrans(rombre, rsoleil, width, P_latitude, j, rap, ir, P_forme, haut0, alpha, rdif, P_ktrou, lai, eai, hauteur)

   implicit none

   !: Arguments
   real, intent(OUT)   :: rombre   ! // OUTPUT // Radiation fraction in the shade // 0-1
   real, intent(OUT)   :: rsoleil   ! // OUTPUT // Radiation fraction in the full sun // 0-1
   real, intent(IN)    :: width
   real, intent(IN)    :: P_latitude  ! // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0
   integer, intent(IN) :: j
   real, intent(IN)    :: rap ! e/width ratio
   real, intent(IN)    :: ir
   integer, intent(IN) :: P_forme  ! // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: haut0
   real, intent(IN)    :: hauteur
   real, intent(IN)    :: alpha
   real, intent(IN)    :: rdif   ! // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1
   real, intent(IN)    :: P_ktrou  ! // PARAMETER // Extinction Coefficient of PAR through the crop  (radiation transfer) // * // PARPLT // 1
   real, intent(IN)    :: lai   ! // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
   real, intent(IN)    :: eai

   !: Variables locales
   real :: rtransmis(200)
   real :: lat_rad
   real :: x
   real :: rg
   real :: rdirect
   real :: kgdiffus
   real :: kgdirect
   real :: rdroit
   real :: pi
   real :: htab(23)
   real :: aztab(23)
   real :: SOCtab(23)
   integer :: interval
   real :: xprec
   integer :: ilim
   integer :: i
   real :: e

   ! Values given by Hervé Sinoquet, gives sun zenith (htab, in degrees), sun azimuth (aztab,
   ! degrees) and fraction of diffuse light according to the SOC standard for 23 directions
   data htab/5*9.23, 2*10.81, 3*26.57, 5*31.08, 3*47.41, 2*52.62, 3*69.16/
   data aztab/12.23,59.77,84.23,131.77,156.23,36,108,0,72,144,23.27,48.73,95.27,120.73,167.27,0,72,144,36,108,0,72,144/
   data SOCtab/5*0.0043, 2*0.0055, 3*0.0140, 5*0.0197, 3*0.0336, 2*0.0399, 3*0.0495/

   ! Discretisation de P_interrang en 200 pas:
   interval = 200

   ! RV: we pass e to other functions, not hauteur!
   e = hauteur - haut0 ! e is the crown length
   rg = 1.0
   rdirect = rg - rdif
   pi = 4*atan(1.0)
   lat_rad = P_latitude/180*pi ! Latitude in radians

   !: Boucle sur la position dans l'P_interrang
   xprec = 0.0
   ilim = interval ! we initialise at the interval so if the plant is larger than the interrow
   ! it is by default equal to the interrow index.

   do i = 1, interval
      x = real(i)/real(interval)*(ir/2) ! x is the coordinates of the point in meters

      if (xprec <= width/2 .and. x > width/2) ilim = i
      xprec = x

      ! Diffuse radiation:
      call kdif(kgdiffus, htab, aztab, SOCtab, x, haut0, width, ir, e, P_forme, rap)

      ! Direct radiation:
      call kdir(kgdirect, lat_rad, j, width, rap, x, ir, P_forme, haut0, alpha, e)

      rdroit = (kgdiffus*rdif) + (kgdirect*rdirect)
      rtransmis(i) = (1.0 - rdroit)*(exp(-P_ktrou*(lai + eai))) + rdroit
      ! RV: note that rtransmis is the transmitted + direct light = total light intercepted by the point
   end do

   rombre = 0.0 ! Average radiation intercepted by the shaded points
   rsoleil = 0.0 ! Average radiation intercepted by the sunlit points

   do i = 1, interval
      if (i < ilim) then
         rombre = rombre + rtransmis(i)
      else
         rsoleil = rsoleil + rtransmis(i)
      end if
   end do

   ! Compute the average transmitted light for shaded component:
   if (ilim == 1) then
      rombre = 0.0
   else
      rombre = rombre/(ilim - 1)
   end if

   ! Compute the average transmitted light for sunlit component:
   if (ilim < interval) then
      rsoleil = rsoleil/(interval - ilim)
      if (rsoleil > 1.0) rsoleil = 1.0
   end if

   return
end subroutine rtrans

! Function kdir
! Fraction of direct radiation received by a point.
subroutine kdir(kdirect, lat, j, width, rap, x, ir, P_forme, haut0, alpha, e)

   implicit none

   !: Arguments
   real, intent(IN)    :: lat ! Latitude in radian
   integer, intent(IN) :: j ! Julian day of year
   real, intent(IN)    :: width ! Plant width (between the interrows)
   real, intent(IN)    :: rap ! e/width ratio
   real, intent(IN)    :: x ! Sample point x coordinates
   real, intent(IN)    :: ir ! Interrow distance
   integer, intent(IN) :: P_forme  ! // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: haut0 ! Base height of the crown
   real, intent(IN)    :: e ! crown length (hauteur - haut0)
   real, intent(IN)    :: alpha ! row direction from North
   real, intent(OUT)   :: kdirect ! Fraction of direct radiation received by a point.

   ! Local variables
   real :: pi
   real :: theta1 ! RHS angle that gives the portion of sky that is seen by a point.
   real :: theta2 ! LHS angle that gives the portion of sky that is seen by a point.

   pi = 4*atan(1.0)

   call get_theta(lat, j, width, x, ir, P_forme, rap, haut0, alpha, e, theta1, theta2)

   kdirect = (-theta1 + theta2)/pi ! proportion of sky the point receives direct light

   return
end subroutine kdir

subroutine get_theta(lat, j, width, x, ir, P_forme, rap, haut0, alpha, e, theta1, theta2)
   implicit none

   !: Arguments
   real, intent(IN)    :: lat ! Latitude in radian
   integer, intent(IN) :: j ! Julian day of year
   real, intent(IN)    :: width ! Plant width (between the interrows)
   real, intent(IN)    :: x ! Sample point x coordinates
   real, intent(IN)    :: ir ! Interrow distance
   integer, intent(IN) :: P_forme ! // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: rap ! e/width ratio
   real, intent(IN)    :: haut0 ! Base height of the crown
   real, intent(IN)    :: e ! crown length (hauteur - haut0)
   real, intent(IN)    :: alpha ! row direction from North
   real, intent(OUT)   :: theta1 ! RHS angle that gives the portion of sky that is seen by a point.
   real, intent(OUT)   :: theta2 ! LHS angle that gives the portion of sky that is seen by a point.
   real    :: G1 ! ratio between crop height and distance between point x and RHS plant.
   real    :: G2 ! Same for LHS plant
   real    :: limite
   real    :: thetacrit

   ! Plant half width, which is used as the limit between the shaded and sunlit points, i.e.
   ! x < limite are below the plant canopy (shaded), and x > limite not below (sunlit).
   limite = width/2

   ! NB: using trigonometry here, remember tan(β) = AC/AB ? Well here AC is the crop height,
   ! and AB is the distance between the point and the plant on the horizontal plane.
   ! So atan(G) woule be the β from above, and it represents the angle between the horizontal
   ! line (AB) and BC, the line between the point and the top of the canopy.
   ! For reference, RHS means the right-hand side, and LHS means left-hand side.
   ! theta1 = angle between the vertical plane and the line from the point to the top canopy of the RHS plant
   ! theta2 = angle between the vertical plane and the line from the point to the canopy of the LHS plant
   ! theta2 depends on the plant shape and on the position of the point on the plane because for the
   ! rectangle and top triangle a different part of the plant blocks the light: either the top
   ! of the canopy if the point is not directly below the plant, or the bottom of the canopy
   ! if it is right below (x < limite)

   call get_G(x, P_forme, rap, limite, haut0, e, width, ir, G1, G2)

   theta1 = thetacrit(lat, j, G1, alpha) ! theta1 is negative because it runs clockwise
   theta2 = -thetacrit(lat, j, G2, alpha) ! This one runs anti-clockwise so it is negative

   if (x < limite) then
      ! The point is below the canopy, its angle is negative (going towards the right-hand-side)
      theta2 = -theta2

      ! In this case it may happen that theta1 > theta2, which means the point sees nothing
      ! because the top of the righ-hand side plant is above the view angle of the point (i.e.
      ! it is completely shaded.
      if (theta1 > theta2) then
         theta1 = 0.0
         theta2 = 0.0
      end if
   end if

end subroutine get_theta

! Compute the cosinus of the theta angle for the apparent sun height `h` (tangent of the
! angle in radian)
real function thetacrit(lat, j, tgh, alpha)

   USE Divers, only: decangle
   implicit none

   !: Arguments
   real, intent(IN) :: lat ! latitude in radians
   integer, intent(IN) :: j ! julian day of year
   real, intent(IN) :: tgh ! ratio between plant height and distance to the plant
   real, intent(IN) :: alpha ! plants row orientation

   !: Variables locales
   real :: theta(180)
   real :: sinh
   real :: h
   real :: hcrit
   real :: a
   real :: b
   real :: acrit
   real :: bcrit
   real :: dec
   real :: pi
   integer :: n
   integer :: i
   real :: hprec
   real :: cosazim
   real :: azim
   real :: hcritprec

   ! Initialisations
   acrit = 0.0
   bcrit = 0.0
   a = 0.0
   b = 0.0
   thetacrit = 0.0
   n = 3
   dec = decangle(j) ! Sun declination angle according to the julian day j
   hprec = 0.0
   pi = 4*atan(1.0)

   do i = 1, 18*n
      theta(i) = 10./n*(i - 1)
      ! This gives theta between 0.0 and 176.67 by steps of 3.33 degrees

      theta(i) = (theta(i) - 90)/180*pi ! same in radian

      ! Sun position in the sky (h,azim)
      sinh = sin(lat)*sin(dec) + cos(lat)*cos(dec)*cos(theta(i))
      h = asin(sinh)
      cosazim = (-cos(lat)*sin(dec) + sin(lat)*cos(dec)*cos(theta(i)))/cos(h)

      cosazim = min(1.0, cosazim)
      if (theta(i) /= 0.0) then
         azim = acos(cosazim)*theta(i)/abs(theta(i))
      else
         azim = 0.0
      end if
      if (sinh < 0.0) h = 0.0

      ! Critical height
      hcrit = atan(tgh*abs(sin(azim + alpha + 0.00001)))
      ! Test wether h >= hcrit, in that case the direction is pointing to the sky
      if (hcritprec >= hprec .and. hcrit <= h .and. i > 1) then
         ! Linear interpolation
         acrit = (hcrit - hcritprec)/(theta(i) - theta(i - 1))
         bcrit = hcrit - acrit*theta(i)
         a = (h - hprec)/(theta(i) - theta(i - 1))
         b = h - a*theta(i)
         if (a /= acrit) thetacrit = (b - bcrit)/(acrit - a)
         return
      end if

      hcritprec = hcrit
      hprec = h
   end do

   return
end function thetacrit

!---------------------------------------------------------------------------
! kdif(kgdiffus, htab, aztab, SOCtab, x, haut0, width, ir, e)
!
! Fraction of diffuse radiation received by a point.
! The computation uses a turtle with 46 (2x23) directions to discretize the hemisphere.
! Then a ray is emmitted from each direction of the turtle to check if the point sees the
! sky at this particular angle. If so, it cumulates the proportion of diffuse radiation in
! this sky sector.
! This is done in two steps:
! - first for the plant on the right-hand side of the interrow (RHS), for 23 directions.
! - then for the plant on the left-hand side (LHS), for 23 directions again. This side is only
! computed if the sample point is not under the crown of LHS plant, because else
! it means it only receives transmitted light from these point of views.
!
subroutine kdif(kgdiffus, htab, aztab, SOCtab, x, haut0, width, ir, e, shape, rap)

   implicit none

   real, intent(INOUT) :: x ! sample point x coordinates
   real, intent(IN)    :: haut0 ! base height of the canopy (*e.g.* the cep height for a vine)
   real, intent(IN)    :: e ! plant crown height (hauteur - haut0)
   real, intent(IN)    :: ir ! interrow distance, *i.e.* the distance between the two plants
   real, intent(IN)    :: width ! plant width between the two rows
   real, intent(OUT)   :: kgdiffus ! fraction of diffuse radiation intercepted by a sample point (output)
   real, intent(IN)    :: htab(23) ! turtle zenithal angles along the day
   real, intent(IN)    :: aztab(23) ! turtle azimuthal angles
   real, intent(IN)    :: SOCtab(23) ! fraction of diffuse ligth in a turtle sector (for a particular azimuth al and zenithal angles couple
   integer, intent(IN) :: shape ! plant shape (1: rectangle, 2 triangle)
   real, intent(IN)    :: rap ! e/width ratio

   ! Local variables
   real    :: limite ! middle of the interrow
   integer :: i
   real    :: pi
   real    :: hcrit_right, hcrit_left ! zenithal angle pointing to the top of the plant
   real    :: G1 ! ratio between crop height and distance between point x and RHS plant.
   real    :: G2 ! Same for LHS plant

   pi = 4*atan(1.0)
   x = min(x, ir/2) ! x can only be in-between the LHS and the middle of the interrow
   limite = width/2.
   kgdiffus = 0.

   call get_G(x, shape, rap, limite, haut0, e, width, ir, G1, G2)
   ! RV: we use the new get_G function now that computes G1, G2 according to their shape

   ! For the RHS (Right-hand side)
   do i = 1, 23 ! 23 zenithal and azimuthal angles on the right of the point
      ! hcrit_right is the ray that point to the top of the RHS plant according to the
      !azimuthal angle (= points to the plant at aztab = 0, and below when turning)
      hcrit_right = atan(G1*sin(aztab(i)/180*pi))/pi*180
      ! hcrit_left is the same but pointing towards the LHS plant
      hcrit_left = atan(G2*sin(aztab(i)/180*pi))/pi*180

      ! When the point is under the LHS plant, we also have to check which canopy is shading first, the LHS or RHS, depending on the configuration:
      if (x >= limite) then
         hcrit_left = 180.0
      else
         ! In some cases when the point is under the LHS canopy, it may happen that hcrit_right < hcrit_left, which means the point sees nothing
         ! because the top of the righ-hand side plant is above the view angle of the point (i.e.
         ! it is completely shaded.
         if (hcrit_right > hcrit_left) then
            hcrit_left = 0.0
            ! NB: In this case we put hcrit_left = 0.0 to say to the code below to not keep this angle (htab[i] is always > 0.0 so htab[i] < hcrit_left will be false)
         end if
      end if

      if (hcrit_right < htab(i) .AND. htab(i) < hcrit_left) then
         ! This ray receives light from the sky
         kgdiffus = kgdiffus + SOCtab(i)
      end if
   end do

   ! For the left-hand side:
   ! If the point is not under the plant canopy (else it is only transmitted light):
   if (x > limite) then
      do i = 1, 23
         hcrit_right = atan(G2*sin(aztab(i)/180*pi))/pi*180
         if (hcrit_right < htab(i)) then
            kgdiffus = kgdiffus + SOCtab(i)
         end if
      end do
   end if

   return
end subroutine kdif

!
! get_G(x, shape, limite, haut0, e, width)
!
! Get the ratio between the crop height and the distance between the point x and the plant.
! The crop height and distance to the plant are always computed using the top of the crop
! seen by the point, which can be different than the top of the canopy if the point is
! under the canopy, or close to the canopy and the canopy is an up-pointing triangle.
!
! Note that the function returns G1, the ratio for the plant on the right, and G2, the
! ratio for the plant on the left.
subroutine get_G(x, P_forme, rap, limite, haut0, e, width, ir, G1, G2)

   implicit none

   real, intent(IN)    :: x ! sample point position
   integer, intent(IN) :: P_forme ! plant shape
   real, intent(IN)    :: rap ! e/width ratio
   real, intent(IN)    :: ir ! interrow distance, *i.e.* the distance between the two plants
   real, intent(IN)    :: limite ! middle of the interrow
   real, intent(IN)    :: haut0 ! base height of the canopy
   real, intent(IN)    :: e ! plant crown height (hauteur - haut0)
   real, intent(IN)    :: width ! plant width between the two rows
   real, intent(OUT)   :: G1 ! ratio between crop height and distance between point x and RHS plant.
   real, intent(OUT)   :: G2 ! Same for LHS plant

   real :: limite2 ! see comment below, only used for the RHS part

   ! G1 is the angle with the right-hand side plant
   ! G2 with the left-hand side plant.
   if (P_forme == 1 .or. e <= 0.0) then
      ! Case of the rectangle, or if plant height is 0 (or lower)
      G1 = (haut0 + e)/(ir - x - limite)

      if (x >= limite) then
         ! the point is not under plant canopy
         G2 = (haut0 + e)/(x - limite)
      elseif (x < limite) then
         ! the point is under plant canopy
         G2 = haut0/(-x + limite)
      elseif (x == limite) then
         G2 = 0.0
      end if
   elseif (P_forme == 2 .and. rap <= 0.) then
      ! Triangle pointing down
      G1 = (haut0 + e)/(ir - x - limite)
      if (x > limite) then
         G2 = (haut0 + e)/(x - limite)
      elseif (x < limite) then
         G2 = (haut0 + e)/(-x + limite)
      elseif (x == limite) then
         G2 = 0.0
      end if
   elseif (P_forme == 2 .and. rap > 0.) then
      ! RV: Triangle pointing up, the most complex one because the point can see either the top
      ! of the canopy if it is far enough (x >= limite2), or just the bottom if it is close
      ! to it or under it.

      if (e > 0.0) then
         limite2 = width/2*(haut0/e + 1)
         ! limite2 is the limit in the point x position above which the point starts to see
         ! the top of the canopy. Below that it only sees the bottom of the canopy, which blocks
         ! its view.
      else
         limite2 = 0.0
      end if

      if (x < limite2) then
         if ((ir - x) < limite2) then
            ! G1 is inside the limit of the right-hand-side plant,
            ! It does not see the top of the plant, but only the bottom corner
            G1 = haut0/(ir - x - limite)
         else
            ! G1 sees the top of the right-hand side plant
            G1 = (haut0 + e)/(ir - x)
         end if
         if (x > limite) then
            G2 = haut0/(x - limite)
         elseif (x < limite) then
            G2 = haut0/(limite - x)
         elseif (x == limite) then
            G2 = 0.0
         end if
      else
         G1 = (haut0 + e)/(ir - x)
         G2 = (haut0 + e)/x
      end if
   end if
end subroutine get_G

! Computes the diffuse fraction of the global radiation.
real function calcul_RDif(rg, P_latitude, jul)

   USE Divers, only: RGEX

   implicit none

   !: Arguments
   integer, intent(IN) :: jul ! julian day
   real, intent(IN) :: rg ! global radiation in MJ m-2 day-1
   real, intent(IN) :: P_latitude  ! // PARAMETER // Latitudinal position of the crop  // degree // STATION // 0

   !: Variable locale
   real :: RsRso  !
   real ::  rdif   ! // OUTPUT // Ratio between diffuse radiation and global radiation  // 0-1

   RsRso = rg/RGEX(P_latitude/180*3.14156, jul)

   !: A priori cette ligne est inutile
   !-- if (RsRso > 0.76) RsRso = 0.76

   ! rapport diffus/global  (Spitters et al 1986 AFM 38 : 217-229)
   if (RsRso < 0.07) rdif = 1.0
   if (RsRso >= 0.07) rdif = 1.0 - (2.3*(RsRso - 0.07)**2)
   if (RsRso > 0.35) rdif = 1.33 - (1.46*RsRso)
   if (RsRso > 0.75) rdif = 0.23

   calcul_RDif = rdif

   return
end function calcul_RDif

! ---------------------------------------------------------

! ---------------------------------------------------------
subroutine formplante(P_forme,lai,laisen,eai,P_interrang,nlax,nsen,P_codlainet,P_hautbase,P_codepalissage,P_hautmaxtec,P_largtec, &
                      originehaut, hauteur, deltahauteur, P_hautmax, varrapforme, dfol, largeur, formetrans, raptrans, largtrans, &
                      hauteurzero, inns, turfac, P_nw_height, fstressgel, exolai, potential_height, ef_elongation)
   implicit none

   !: Arguments
   integer, intent(IN)    :: P_forme  ! // PARAMETER // Form of leaf density profile  of crop: rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: lai   ! // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
   real, intent(IN)    :: laisen   ! // OUTPUT // Leaf area index of senescent leaves // m2 leafs  m-2 soil
   real, intent(IN)    :: eai
   real, intent(IN)    :: P_interrang  ! // PARAMETER // Width of the P_interrang // m // PARTEC // 1
   real, intent(IN)    :: nlax
   real, intent(IN)    :: nsen
   real, intent(IN)    :: P_codlainet  ! // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: P_hautbase  ! // PARAMETER // Base height of crop // m // PARPLT // 1
   integer, intent(IN)    :: P_codepalissage  ! // PARAMETER // option: no (1),  yes2) // code 1/2 // PARTEC // 0
   real, intent(IN)    :: P_hautmaxtec  ! // PARAMETER // maximal height of the plant allowed by the management // m // PARTEC // 1
   real, intent(IN)    :: P_largtec  ! // PARAMETER // technical width // m // PARTEC // 1
   real, intent(IN)    :: originehaut
   real, intent(IN)    :: ef_elongation ! // OUTPUT // Shoot elongation effect due to plant shading // 1-P_elongation

   real, intent(INOUT) :: hauteur   ! // OUTPUT // Height of canopy // m
   real, intent(INOUT) :: deltahauteur
   real, intent(INOUT) :: potential_height ! Plant height without stress.
   real, intent(INOUT) :: P_hautmax  ! // PARAMETER // Maximum height of crop // m // PARPLT // 1
   real, intent(INOUT) :: varrapforme
   real, intent(INOUT) :: dfol   ! // OUTPUT //  "Within the shape  leaf density" // m2 m-3

   real, intent(OUT)   :: largeur   ! // OUTPUT // Width of the plant shape  // m
   integer, intent(OUT)   :: formetrans
   real, intent(OUT)   :: raptrans
   real, intent(OUT)   :: largtrans
   real, intent(OUT)   :: hauteurzero
   real, intent(IN)    :: inns   ! // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
   real, intent(IN)    :: turfac   ! // OUTPUT // Index of turgescence water stress  // 0-1
   real, intent(IN)    :: P_nw_height ! // PARAMETER // used to compute the stress effect of water and nitrogen on height
   real, intent(IN)    :: fstressgel   ! // OUTPUT // Frost index on the LAI // 0-1
   real, intent(IN)    :: exolai   ! // OUTPUT // Index for excess water active on growth in biomass // 0-1

   !: Variables locales
   real :: enouv
   real :: tmp1
   real :: tmp2
   real :: hauteurjour
   real :: ef_n_w_height ! Effect of water and nitrogen deficit on height (0-1)
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

   !: Calcul de la largeur en fonction de lai
   !- de la P_forme et de la densit� foliaire
   if (P_forme == 1) then
      tmp1 = lai + laisen + eai
      tmp2 = dfol*varrapforme
      largeur = sqrt(tmp1*P_interrang/tmp2)
   else
      tmp1 = lai + laisen + eai
      tmp2 = dfol*abs(varrapforme)
      largeur = sqrt(2*tmp1*P_interrang/tmp2)
   end if

   ! *- NB - le 23/11
   if (nlax > 0 .and. nsen == 0) then
      if (P_codlainet == 1) potential_height = hauteur + deltahauteur
   else
      if (nsen == 0) then
         potential_height = P_hautbase + largeur*abs(varrapforme)
      end if
   end if

   ! The potential increment in height is the difference between the potential height from today and the day before
   potential_delta_height = potential_height - potential_height_veille

   ! The actual increment in height is:
   deltahauteur = potential_delta_height*ef_n_w_height*ef_elongation

   ! And so is the height:
   hauteurjour = hauteur + deltahauteur

   ! hauteur is only updated if it is not lower than the previous day.
   if (hauteurjour > hauteur) then
      hauteur = hauteurjour
   end if

   ! NB: The computation is repeated in the height subroutine (the height is computed differently there)

   ! NB le 01/07/05 introduction d'une hauteur et largeur techniques maximales
   ! ** ML le 29/05/07 les largeur et hauteur techniques maximales ne sont
   ! *- prises en compte que dans le cas o� il y a palissage (P_codepalissage = 2)
   if (P_codepalissage == 2) then
      P_hautmax = min(P_hautmax, P_hautmaxtec)
   end if

   !: ML le 29/05/07
   !: Limitation de la largeur par P_largtec sans limitation de hauteur
   if (hauteur < P_hautmax) then
      if (P_codepalissage /= 2 .or. (P_codepalissage == 2 .and. largeur < P_largtec)) then
         varrapforme = sign(1.0, varrapforme)*(hauteur - P_hautbase)/largeur
      end if
      if (P_codepalissage == 2 .and. largeur >= P_largtec) then
         largeur = P_largtec
         varrapforme = sign(1.0, varrapforme)*(hauteur - P_hautbase)/largeur
      end if
   end if

   !: Limitation de la hauteur par P_hautmax sans limitation de largeur
   if (hauteur >= P_hautmax) then
      hauteur = P_hautmax
      if (P_codepalissage /= 2 .or. (P_codepalissage == 2 .and. largeur < P_largtec)) then
         varrapforme = sign(1.0, varrapforme)*(hauteur - P_hautbase)/largeur
      end if

      !: Limitation de la hauteur par P_hautmax et de la largeur par P_largtec
      !
      !- dans ce cas il faut non seulement calculer varrapforme, mais �galement
      !- recalculer dfol afin qu'il soit coh�rent avec P_hautmaxtec et P_largtec en sortie
      !- sachant que dans les 2 cas pr�c�dents (soit la hauteur, soit la largeur est limit�e)
      !- la densit� foliaire dfol avait toujours la possibilit� de compenser
      !- un manque de place en hauteur par un remplissage en largeur ou inversement
      if (P_codepalissage == 2 .and. largeur >= P_largtec) then
         largeur = P_largtec
         varrapforme = sign(1.0, varrapforme)*(hauteur - P_hautbase)/largeur
         if (P_forme == 1) then
            dfol = tmp1*P_interrang/(largeur**2*abs(varrapforme))
         else
            dfol = 2*tmp1*P_interrang/(largeur**2*abs(varrapforme))
         end if
      end if
   end if

   hauteurzero = P_hautbase - originehaut

   formetrans = P_forme
   if (hauteurzero < 0.0) then

      !: Cas des cultures en dont les feuillages se m�langent
      enouv = largeur*abs(varrapforme) + hauteurzero

      !: Cas du rectangle
      if (P_forme == 1) then
         raptrans = enouv/largeur
         largtrans = largeur
      end if

      !: Cas du triangle � l'endroit
      if (P_forme == 2 .and. varrapforme > 0.0) then
         raptrans = varrapforme
         largtrans = enouv/raptrans
      end if

      !: Cas du triangle � l'envers qui "devient" rectangle au
      !- dessus de la culture associ�e
      if (P_forme == 2 .and. varrapforme < 0.0) then
         largtrans = largeur
         raptrans = enouv/largtrans
         formetrans = 1
      end if

      hauteurzero = 0.001

   else
      raptrans = varrapforme
      largtrans = largeur
   end if

   return
end subroutine formplante

subroutine formplante_mas(P_forme, masec, P_hautbase, P_codepalissage, &
                          P_hautmaxtec, P_largtec, originehaut, hauteur, P_hautmax, &
                          P_hautK, P_hautA, varrapforme, largeur, formetrans, &
                          raptrans, largtrans, hauteurzero, P_interrang, densite, &
                          ef_elongation, P_hautdens, P_code_shape, &
                          P_haut_dev_x0, P_haut_dev_k, somupvtsem, inns, turfac, &
                          P_nw_height, ef_n_w_height, potential_height, deltahauteur, &
                          fstressgel, exolai, P_code_strip, P_nrow)
   implicit none

   integer, intent(IN)    :: P_forme         ! // PARAMETER // Form of leaf density profile  of crop:
   ! rectangle (1), triangle (2) // code 1/2 // PARPLT // 0
   real, intent(IN)    :: P_hautbase      ! // PARAMETER // Base height of crop // m // PARPLT // 1
   integer, intent(IN)    :: P_codepalissage ! // PARAMETER // option: no (1),  yes (2) // code 1/2 // PARTEC // 0
   real, intent(IN)    :: P_hautmaxtec    ! // PARAMETER // Max height of the plant allowed by management // m // PARTEC // 1
   real, intent(IN)    :: P_largtec       ! // PARAMETER // technical width // m // PARTEC // 1
   real, intent(IN)    :: originehaut
   real, intent(IN)    :: masec
   real, intent(INOUT) :: hauteur         ! // OUTPUT // Height of plant // m
   real, intent(INOUT) :: P_hautmax       ! // PARAMETER // Maximum height of crop // m // PARPLT // 1
   real, intent(INOUT) :: P_hautK ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(INOUT) :: P_hautA ! // PARAMETER // Parameter for the hauteur~masec relationship // m.(t.ha-1)-1 // PARPLT // 1
   real, intent(IN)    :: P_haut_dev_x0 ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: P_haut_dev_k ! // PARAMETER // Parameter for the hauteur~development relationship // - // PARPLT // 1
   real, intent(IN)    :: somupvtsem   ! // INPUT // sum of development units from sowing // degree C
   integer, intent(IN)    :: P_code_shape  ! // PARAMETER // code for plant shape computation (default) using LAI, (2) using masec, (3) using dev // PARPLT // 1
   real, intent(INOUT) :: varrapforme
   real, intent(OUT)   :: largeur         ! // OUTPUT // Width of the plant shape  // m
   integer, intent(OUT)   :: formetrans
   real, intent(OUT)   :: raptrans
   real, intent(OUT)   :: largtrans
   real, intent(OUT)   :: hauteurzero
   real, intent(IN)    :: P_interrang  ! // PARAMETER // Width of the P_interrang // m // PARTEC // 1
   real, intent(IN)    :: densite   ! // OUTPUT // Actual sowing density // plants.m-2
   real, intent(IN)    :: ef_elongation ! // OUTPUT // Shoot elongation effect due to plant shading // 1-P_elongation
   real, intent(IN)    :: P_hautdens   !// PARAMETER // density at which the hauteur~masec relationship was measured // plants m-2 // PARAMv6 // 1
   real, intent(IN)    :: inns   ! // OUTPUT // Index of nitrogen stress active on growth in biomass // P_innmin to 1
   real, intent(IN)    :: turfac   ! // OUTPUT // Index of turgescence water stress  // 0-1
   real, intent(IN)    :: P_nw_height ! // PARAMETER // used to compute the stress effect of water and nitrogen on height
   real, intent(OUT)   :: ef_n_w_height ! // OUTPUT // Effect of water and nitrogen deficit on height (0-1)
   real, intent(INOUT) :: potential_height ! Plant height without stress.
   real, intent(INOUT) :: deltahauteur
   real, intent(IN)    :: fstressgel   ! // OUTPUT // Frost index on the LAI // 0-1
   real, intent(IN)    :: exolai   ! // OUTPUT // Index for excess water active on growth in biomass // 0-1
   integer, intent(IN)    :: P_code_strip ! // PARAMETER //  is it a strip intercrop? // PARAMv6 // 1: yes, 2: no
   integer, intent(IN)    :: P_nrow ! // PARAMETER // number of plant rows in the strip

   real :: enouv

   ! RV: Computing plant height using the height subroutine also used in the beers computation. All 0s used as inputs
   ! here are variables normaly used when P_code_shape == 1, which is never the case here because we use subroutine
   ! formplante in that case.
   call height(0, 0.0, 0.0, 0, 0, 0, hauteur, deltahauteur, P_hautmax, P_hautbase, 0.0, P_hautK, P_hautA, 0.0, masec, &
               densite, ef_elongation, P_hautdens, P_code_shape, P_haut_dev_x0, &
               P_haut_dev_k, somupvtsem, inns, turfac, P_nw_height, ef_n_w_height, potential_height, &
               fstressgel, exolai)

   ! largeur is not for one plant only but the sum of all plants in the strip:
   if (P_code_strip == 1) then
      largeur = ((hauteur - P_hautbase)/abs(varrapforme))*P_nrow
   else
      largeur = (hauteur - P_hautbase)/abs(varrapforme)
   end if
   ! RV: note that this is a very bad approximation but this it the best one we can do considering
   ! the simplicity of the model. Here the simulated plant is considered to be the whole strip, and
   ! the light interception is computed as if all plants formed one (sum of their width). This is
   ! better that nothing, but very far from reality, so carefull when simulating those systems.
   ! /!\ in this case the user must set the interrow distance as the distance between the center of
   ! the strips of the same plant. E.g. for one sunflower + 2 soybean, the distance between one strip
   ! of sunflower and the other one is 1.5, and the distance between the center of two soybean strips
   ! is 1.5.

   ! Correction for maximum technical height and width (management).
   ! NB: not computing if trellising (P_codepalissage != 2)
   if (P_codepalissage == 2) then
      P_hautmax = min(P_hautmax, P_hautmaxtec)
      if (largeur >= P_largtec) then
         largeur = P_largtec
         varrapforme = sign(1.0, varrapforme)*(hauteur - P_hautbase)/largeur
      end if
   end if

   ! Height limitation by P_hautmax. If heigth is limited, the plant will try to grow
   ! in width first, and then in foliar density
   if (hauteur >= P_hautmax) then
      hauteur = P_hautmax
      if (P_codepalissage /= 2 .or. (P_codepalissage == 2 .and. largeur < P_largtec)) then
         varrapforme = sign(1.0, varrapforme)*(hauteur - P_hautbase)/largeur
      end if
      ! Width limitation by P_largtec
      if (P_codepalissage == 2 .and. largeur >= P_largtec) then
         largeur = P_largtec
         varrapforme = sign(1.0, varrapforme)*(hauteur - P_hautbase)/largeur
      end if
   end if

   ! Width cannot exceed the interrow spacing in the model because it is used then to
   ! compute relative surfaces. So if width is higher than interrow, the leaf density
   ! is increased only. In reality, if the width is higher than the interrow spacing,
   ! it is mixing with another plant. In STICS we work with an average surface of a
   ! plot, so if all plants mix together, it can be considered that their dfol is
   ! increased also.
   if (largeur > P_interrang) then
      largeur = P_interrang
      varrapforme = sign(1.0, varrapforme)*(hauteur - P_hautbase)/largeur
   end if

   ! Under the case of intercrop, if the dominated plant crown is higher than the base
   ! height of the dominant plant, then the crown of the dominant plant is reduced to
   ! fit only the part that is not overlapping. Indeed, the plant crown shape is only
   ! used to compute the shade effect above the dominated plant on light quality, i.e.
   ! either transmitted by the upper canopy or coming from the atmosphere, not the light
   ! interception in itself (the lai is used fot this).
   hauteurzero = P_hautbase - originehaut

   formetrans = P_forme
   if (hauteurzero < 0.0) then
      enouv = hauteur + hauteurzero

      if (P_forme == 1) then
         raptrans = enouv/largeur
         largtrans = largeur
      end if

      if (P_forme == 2 .and. varrapforme > 0.0) then
         raptrans = varrapforme
         largtrans = enouv/raptrans
      end if
      ! A reversed triangle (poiting towards the soil) becomes a rectangle if it has
      ! to be reduced.
      if (P_forme == 2 .and. varrapforme < 0.0) then
         largtrans = largeur
         raptrans = enouv/largtrans
         formetrans = 1
      end if
      hauteurzero = 0.001
   else
      raptrans = varrapforme
      largtrans = largeur
   end if

   return
end subroutine formplante_mas
