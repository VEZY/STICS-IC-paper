! *----------------------------------------------* c
! * r�partition de la biomasse entre les organes * c
! * programmation N. Brisson le 14/11/2000       * c
! * modif le 18/04 NB                            * c
! *----------------------------------------------* c
! There are models for which allocation of assimilates is critical to the operation of the model (e.g. SUCROS described by Van Ittersum et al., 2003).
!- Stics book paragraphe 3,5, page 68-71
!
! In STICS this module was added at a late stage, mainly to help dimensioning the reserve pool. For annual plants with determinate growth, the partitioning
!! calculations simply allow the dimensioning of envelopes of harvested organs which may play a trophic role and ensure an input of information for the
!! senescence module. For perennial plants or those with indeterminate growth, those calculations enable the dimensioning of a compartment for reserves which
!! are recycled in the carbon balance. The calculation of root biomass is not directly connected to that of the above-ground biomass.
! - Organs and compartments identified: the reasons for identifying an organ or a compartment are either its internal trophic role within the plant or an
!!   external role by participation in the nitrogen balance of the system (such as falling leaves and the recycling of roots). The reserve compartment is not
!!   located in a specific organ: it is just a certain quantity of carbon available for the plant growth.
! - Dimensioning of organs:
!!   Green leaves: The biomass of green leaves is calculated without accounting for potential reserves that may be stored in the leaves and remobilized later on,
!!   which are accounted for in the resperenne non-located reserve pool. The mafeuilverte variable is deducted from the LAI, based on the
!!   maximum specific leaf area variable (slamax). We assume that the difference between the actual sla and slamax corresponds to remobilized leaf carbon
!!   Yellow leaves: The biomass of yellow leaves (mafeuiljaune) is calculated in the senescence module.  The proportion of leaves in the senescent biomass
!!   on a given day (dltamsen) is determined using the pfeuilverte ratio (proportion of green leaves in the non-senescent biomass) on the day of production
!!   of this senescent biomass. Some of these yellow leaves may fall to the ground depending on the abscission  parameter (between 0 and 1). The daily falling
!!   quantity (dltamstombe) is recycled in the nitrogen balance; its cumulative value is mafeuiltombe.
!!   Stems: this concerns only the structural component of stems (matigestruc). The non-structural component, if significant, can be included in the reserve
!!   compartment (e.g. for cereals) or in the harvested part (sugar cane).  The matigestruc variable is calculated as a constant proportion (tigefeuille of the
!!   total mass of foliage. For monocotyledonous plants, the stem is secondary and the matigestruc variable is only incremented from the time when accumulated
!!   biomass so allows.  It is thus assumed that the first organs to emerge are the leaves. For dicotyledonous plants, it is assumed that the tigefeuille
!!   proportionality is always respected.  Consequently, if the accumulated biomass and the foliage biomass (calculated from the LAI and SLA) are incompatible
!!   with this proportionality, then the SLA (or LAI if the SLA arises from fixed limits) is recalculated. The matigestruc variable cannot diminish,
!!   except in the case of cutting fodder crops.
!!   Harvested organs:
!   - Fruits and grains: the calculation of the number and mass of fruits (indeterminate plants) or seeds (determinate plants) is achieved in modules
!!       fruit.f90 and grain.f90.
!   - Envelops of harvested organs (pods, raches, etc.): the mass corresponding to the envelope is assumed to depend solely upon the number of organs.
!!       In any case, it cannot exceed the residual biomass (masecveg - mafeuil - matigestruc). The envfruit parameter corresponds to the proportion of
!!       membrane related to the maximum weight of the fruit. If the sea parameter is not zero, then this biomass is transformed into an equivalent
!!       leaf surface area, photosynthetically active from the IDRP stage to the IDEBDES stage.
!
!!   Reserves (resperenne) are calculated as the difference between the total biomass and the accumulated biomass of leaves, stems and harvested organs.
!!   For perennial plants, at the beginning of the cropping season, the reserves (carbon) can be initialised at a non-zero value (resperenne0), so as to
!!   represent the role played by root reserves at the resumption of growth. Yet it is assumed that a limit exists to the size of the reserve compartment,
!!   parametrized at the plant level by resplmax. If this limlit is reached a �sink on source� effect is simulated. The use of reserves concerns perennial plants
!!   or indeterminate plants.  As for determinate annuals, the use of reserves for grain filling is not simulated as such, but taken globally into account
!!   when calculating the ercarb variable (index of progressive harvest).
!-----------------------------------------------------------------------------
subroutine repartir(n, nrec, P_codcueille, P_codeperenne, nlev, nlax, P_nbcueille, numcult, tustress, P_slamin, P_slamax, &
                    P_codlainet, P_codemonocot, P_codesimul, dltaisen, P_tigefeuil, P_envfruit, chargefruit, ndrp, &
                    ndebdes, P_sea, ntaille, P_codetaille, P_codeinitprec, dltams, lai_veille, &
                    resperenne, masecveg, pdsfruittot, tursla, sla, mafeuilverte, mafeuil, mafeuilp, lai, deltai, &
                    maenfruit, eai, mareserve, deltares, mabois, P_resperenne0, masec, msresjaune, mafeuiljaune, &
                    msneojaune, matigestruc, pfeuil, pfeuilverte, pfeuiljaune, ptigestruc, penfruit, preserve, P_codefauche)

   USE Messages

   implicit none

!: Arguments

   integer, intent(IN)    :: n
   integer, intent(IN)    :: nrec
   integer, intent(IN)    :: P_codcueille  ! // PARAMETER // way how to harvest // code 1/2 // PARTEC // 0
   integer, intent(IN)    :: P_codeperenne  ! // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
   integer, intent(IN)    :: P_codefauche  ! // PARAMETER // option of cut modes for forage crops: yes (1), no (2) // code 1/2 // PARTEC // 0
   integer, intent(IN)    :: nlev
   integer, intent(IN)    :: nlax
   integer, intent(IN)    :: P_nbcueille  ! // PARAMETER // number of fruit harvestings // code 1/2 // PARTEC // 0
   integer, intent(IN)    :: numcult
   real, intent(IN)    :: tustress   ! // OUTPUT // Stress index active on leaf growth (= minimum(turfac,innlai))  // 0-1
   real, intent(IN)    :: P_slamin  ! // PARAMETER // minimal SLA of green leaves // cm2 g-1 // PARPLT // 1
   real, intent(IN)    :: P_slamax  ! // PARAMETER // maximal SLA of green leaves // cm2 g-1 // PARPLT // 1
   integer, intent(IN)    :: P_codlainet  ! // PARAMETER //option of calculation of the LAI (1 : direct LAInet; 2 : LAInet = gross LAI - senescent LAI) // code 1/2 // PARPLT // 0
   integer, intent(IN)    :: P_codemonocot  ! // PARAMETER // option plant monocot(1) or dicot(2) // code 1/2 // PARPLT // 0
   character(len=12), intent(IN) :: P_codesimul  ! // PARAMETER // simulation code (culture ou feuille=lai forc�) // SD // P_USM/USMXML // 0
   real, intent(IN)    :: dltaisen   ! // OUTPUT // Daily increase of the senescent leaf index // m2.m-2 sol.j-1
   real, intent(IN)    :: P_tigefeuil  ! // PARAMETER // stem (structural part)/leaf proportion // SD // PARPLT // 1
   real, intent(IN)    :: P_envfruit  ! // PARAMETER // proportion envelop/P_pgrainmaxi in weight  // SD // PARPLT // 1
   real, intent(IN)    :: chargefruit   ! // OUTPUT // Amount of filling fruits per m-2 // nb fruits.m-2
   integer, intent(IN)    :: ndrp
   integer, intent(IN)    :: ndebdes
   real, intent(IN)    :: P_sea  ! // PARAMETER // specifique surface of fruit envelops // cm2 g-1 // PARPLT // 1
   integer, intent(IN)    :: ntaille
   integer, intent(IN)    :: P_codetaille  ! // PARAMETER // option of pruning // code 1/2 // PARTEC // 0
   integer, intent(IN)    :: P_codeinitprec  ! // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0
   real, intent(IN)    :: dltams   ! // OUTPUT // Growth rate of the plant  // t ha-1.j-1
   real, intent(IN)    :: lai_veille              ! n-1

   real, intent(INOUT) :: resperenne   ! // OUTPUT // C crop reserve, during the cropping season, or during the intercrop period (for perenial crops) // t ha-1
   real, intent(INOUT) :: masecveg   ! // OUTPUT // Vegetative dry matter // t.ha-1
   real, intent(INOUT) :: pdsfruittot
   real, intent(INOUT) :: tursla
   real, intent(INOUT) :: sla   ! // OUTPUT // Specific surface area // cm2 g-1
   real, intent(IN) :: mafeuilverte   ! // OUTPUT // Dry matter of green leaves // t.ha-1
   real, intent(IN) :: mafeuil   ! // OUTPUT // Dry matter of leaves // t.ha-1
   real, intent(INOUT) :: mafeuilp
   real, intent(INOUT) :: matigestruc   ! // OUTPUT // Dry matter of stems (only structural parts) // t.ha-1
   real, intent(IN) :: lai                     ! n    // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
   real, intent(IN) :: deltai   ! // OUTPUT // Daily increase of the green leaf index // m2 leafs.m-2 soil
   real, intent(INOUT) :: maenfruit   ! // OUTPUT // Dry matter of harvested organ envelopes // t.ha-1
   real, intent(INOUT) :: eai
   real, intent(INOUT) :: mareserve
   real, intent(INOUT) :: deltares
   real, intent(INOUT) :: mabois   ! // OUTPUT // Prunning dry weight // t.ha-1
   real, intent(INOUT) :: P_resperenne0  ! // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1
   real, intent(INOUT) :: masec   ! // OUTPUT // Aboveground dry matter  // t.ha-1
   real, intent(INOUT) :: msresjaune   ! // OUTPUT // Senescent residual dry matter  // t.ha-1
   real, intent(INOUT) :: mafeuiljaune   ! // OUTPUT // Dry matter of yellow leaves // t.ha-1
   real, intent(INOUT) :: msneojaune   ! // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
   real, intent(INOUT) :: pfeuil   ! // OUTPUT // Proportion of leaves in total biomass // 0-1
   real, intent(INOUT) :: pfeuilverte   ! // OUTPUT // Proportion of green leaves in total non-senescent biomass // 0-1
   real, intent(INOUT) :: pfeuiljaune   ! // OUTPUT // Proportion of yellow leaves in total biomass // 0-1
   real, intent(INOUT) :: ptigestruc   ! // OUTPUT // Proportion of structural stems in total biomass // 0-1
   real, intent(INOUT) :: penfruit   ! // OUTPUT // Proportion of fruit envelopes in total biomass // 0-1
   real, intent(INOUT) :: preserve   ! // OUTPUT // Proportion of reserve in the total biomass // 0-1

!: Variables locales
   real :: mareservep  !
   real :: ptigestrucveg  !
   real :: matigestruc1
   real :: laiajour

   ! RV: Not changing the values of these variables anymore! This is happening too late in the
   ! model, their values were already computed elsewhere and used by the model for the
   ! development and growth of the plant. If we correct their values now we only change them
   ! for the output and for the variables that use previous values then, which is very wrong
   ! because then the true values used by the model were overwritten here and we lose
   ! consistency. This is especially true for computing senescence of the leaves.
   ! If we need to integrate a feedback between max sla / min sla and lai / leaf biomass, then
   ! we have to do it in the LAI computation so all computations are consistant with the new
   ! value. NB: I keep the output of repartir though.

   real :: mafeuilverte_repartir
   real :: mafeuil_repartir
   real :: lai_repartir
   real :: deltai_repartir
   real :: dltaisen_repartir

   mafeuilverte_repartir = mafeuilverte
   mafeuil_repartir = mafeuil
   lai_repartir = lai
   deltai_repartir = deltai
   dltaisen_repartir = dltaisen

! ** param�tres
! *-    slavert est la surface sp�cifique du feuillage "sans stress"
! *-    slavertmin est le minimum de slavert apr�s application des stress
! *-    tigefeuille est le rapport tigestruc/feuilles totales
! *-    P_envfruit correspond � la proportion enveloppe/P_pgrainmaxi
! *-    photores est la photop�riode seuil en jour d�croissant
! *-      qui d�clenche la mise en r�serve

   ! ** apr�s la r�colte (nrec+1)
   ! ** en cas de moisson,on shunte ce module apr�s r�colte

   if (n >= nrec + 1 .and. nrec > 0 .and. P_codcueille == 1 .and. P_codeperenne == 1) then
      resperenne = 0.
   else

      ! ** la biomasse non r�coltable est masecveg
      ! --     if (nrec == 0.or.n == nrec) masecveg = masec-pdsfruittot/100.
      ! -- ML le 10/10/09 calcul de masecveg avant annulation de pdsfruittot le jour de recolte
      ! 11/07/2013 DR et ML on corrige un bug suite � un pb pour la betterave (matuber incluait 2 fois mafruit) , (c'etait le cas pour toutes les plantes )
      !  dans biomaer la biomasse des fruits est retir�e de masec uniquement si codcueille=2 : dans ce cas il ne faut pas la retirer une 2ieme fois dans repartir
!          if (P_nbcueille == 1 .and. n >= nrec .and. nrec > 0) then
      if (P_codcueille == 2 .and. n >= nrec .and. nrec > 0) then
         ! !!!!!!!!!DR et Ml le 31/01/06 on se pose des questions sur la ligne mise en commantaire
         ! et on la reactive car masec est la matiere seche avce les fruits d'ou pb
         !  a voir
         !    masecveg = masec-pdsfruittot/100.
         ! NB le 09/07/06
         ! les fruits r�colt�s ont d�j� �t� �t�s dans le sspg biomaer.for
         ! donc il ne faut pas les enlever 2 fois....
         masecveg = masec
         pdsfruittot = 0.
      else
         masecveg = masec - pdsfruittot/100.
      end if

      ! ** conservation de la r�serve du jour pr�c�dent
      mareservep = mareserve

      ! ** initialisation de la r�serve p�renne
      if (n == nlev .and. numcult == 1) then
         if (P_codeperenne == 2) then
            resperenne = P_resperenne0
         else
            resperenne = 0.
         end if
      end if

      ! ** les feuilles vertes
      ! *- calcul d'un stress moyen pour module slavert
      !-- if (n == nlev) tursla = 1.

      tursla = (tursla + tustress)/2.
      sla = max(tursla*P_slamax, P_slamin)

      ! le 09/07/06 mise � jour du LAI pour ad�quation avec s�nescence
      if (P_codlainet == 2) then
         laiajour = lai_repartir - dltaisen_repartir
      else
         laiajour = lai_repartir
      end if

      ! NB le 09/07/06 pour permettre le stockage de r�serve dans les feuilles
      ! mafeuilleverte correspond � de la mati�re structurale et donc n'est pas
      ! affect�e par les stress, ce qui se traduit par l'utilisation de P_slamax et non sla
      !      mafeuilverte_repartir = min(lai_repartir/sla * 1e2,masecveg)
      ! loic 20/05/2016 il faut retirer de masecveg ce qui est senecent mafeuiljaune , sinon mafeuilverte_repartir n'estr plus verte
      ! mafeuilverte_repartir = min(laiajour/P_slamax * 1e2, masecveg)
      mafeuilverte_repartir = min(laiajour/P_slamax*1e2, (masecveg - mafeuiljaune))

      ! ** les feuilles jaunes sont calcul�es dans senescen.for
      ! *- variable mafeuiljaune

      ! ** cumul ensemble des feuilles
      mafeuil_repartir = mafeuilverte_repartir + mafeuiljaune

      ! ** test sur mafeuil_repartir >masecveg NB le 22/04
      if (masecveg < mafeuil_repartir) then
         mafeuilverte_repartir = masecveg - mafeuiljaune
         mafeuil_repartir = masecveg
      end if
!          if(n.eq.1)write(*,*)' , n, lai_repartir,dltaisen_repartir, masec, mafeuil_repartir, mafeuilverte_repartir, mafeuiljaune, masecveg,(pdsfruittot / 100.)'
!          write(*,*)' ',n, lai_repartir,dltaisen_repartir, masec, mafeuil_repartir, mafeuilverte_repartir, mafeuiljaune, masecveg,(pdsfruittot / 100.)
      if (mafeuilverte_repartir <= 0 .and. nlax == 0) then
         call EnvoyerMsgHistorique(403)
!            write(*,*)'* ',n, lai_repartir,dltaisen_repartir, masec, mafeuil_repartir, mafeuilverte_repartir, mafeuiljaune, masecveg,(pdsfruittot / 100.)
      end if

      ! ** la masse structurale des tiges est une proportion de la masse de feuille
      ! *- matigestruc = min(P_tigefeuil*mafeuil_repartir,masecveg-mafeuil_repartir)

      ! ** pour les monocotyl�dones : priorit� aux feuilles
      if (P_codemonocot == 1) then
         matigestruc = matigestruc + max(P_tigefeuil*(mafeuil_repartir - mafeuilp), 0.)
         matigestruc = min(matigestruc, masecveg - mafeuil_repartir)

         ! ** pour les dicotyl�dones : priorit� aux tiges
      else
         matigestruc1 = P_tigefeuil*mafeuil_repartir
         if (matigestruc1 > (masecveg - mafeuil_repartir)) then
            ! ** on recalcule mafeuil_repartir tel que masecveg = (tigfeuil+1)mafeuil_repartir
            mafeuil_repartir = masecveg/(P_tigefeuil + 1)
            mafeuilverte_repartir = mafeuil_repartir - mafeuiljaune

            if (mafeuilverte_repartir > 0.) then
               ! NB le 09/07/06 mise � jour lai_repartir
               !-- sla = lai_repartir/mafeuilverte_repartir*100.
               sla = laiajour/mafeuilverte_repartir*100.
            end if

            if (sla < P_slamin) then
               ! NB le 09/07/06 mise � jour lai_repartir
               !-- mafeuilverte_repartir = lai_repartir/P_slamin*100.
               mafeuilverte_repartir = laiajour/P_slamin*100.
               mafeuil_repartir = mafeuilverte_repartir + mafeuiljaune
               sla = P_slamin
            end if
            ! ** domi - 29/04/03 - pour stics-feuille incompatibilite avce le forcage du lai_repartir
            ! DR 15/10/2010 y'avait un pb dans les tests --> incompatibilit� avec le version 7.1 , j'ajoute des parentheses
!              if (sla > P_slamax  .and. lge(P_codesimul,'feuille') .eqv. .FALSE.) then
            if ((lge(P_codesimul, 'feuille') .eqv. .FALSE.) .and. (sla > P_slamax)) then
               sla = P_slamax
               lai_repartir = sla*(mafeuil_repartir - mafeuiljaune)/100.
               ! DR et IGC le 07/09/06
               deltai_repartir = lai_repartir - lai_veille
               ! DR et SB 27/02/2017 il y a un pb en cas de phase de decroissance du lai_repartir , le deltai_repartir calcule ici devient negatif
               ! et du coup dans le calcul du lai_repartir brut on fait lai_repartir(n)=lai_repartir(n-1) + deltai_repartir - deltaisen, on ajoute la scenesence !! , on ajoute les 4 lignes qui suivent
               deltai_repartir = lai_repartir - lai_veille + dltaisen_repartir
               if (deltai_repartir .lt. 0.0) then
                  deltai_repartir = 0.0
                  dltaisen_repartir = lai_veille - lai_repartir
               end if
    write (618, *) n, 'repartir', sla, lai_repartir, lai_veille, mafeuil_repartir, mafeuiljaune, 'deltai', deltai, dltaisen_repartir
            end if
         end if

         matigestruc = matigestruc + max(P_tigefeuil*(mafeuil_repartir - mafeuilp), 0.)
         ! loic 20/05/2016 il faut ajouter le test pour eviter  que la somme des composants
         ! de la biomasse aerienne soient plus grands que masecveg.
         matigestruc = min(matigestruc, masecveg - mafeuil_repartir)
      end if

      mafeuilp = mafeuil_repartir

      ! ** NB - le 12/04 - pour �viter plantade li�e au GEL
      if (masecveg > 0.0) then
         ptigestrucveg = matigestruc/masecveg
      else
         ptigestrucveg = 0.0
      end if

      ! ** les enveloppes des organes r�colt� sont un % du
      ! *- nombre de fruits � condition qu'il y ait assez de biomasse
      maenfruit = P_envfruit*chargefruit

      ! ** seuillage de maenfruit - NB - le 21/04
      maenfruit = min(maenfruit, 0.99*(masecveg - mafeuil_repartir - matigestruc))

      maenfruit = max(maenfruit, 0.)

      if ((masecveg - mafeuil_repartir - matigestruc - maenfruit) < 0.) then
         call EnvoyerMsgHistorique(404)
         !--     stop
      end if

      ! ** NB - le 28/03/02 - estimation d'une surface photosynth�tique pour le fruit
      ! *- entre les stades DRP et DEBDES
      if (ndrp > 0 .and. ndebdes == 0) then
         eai = P_sea*maenfruit/100.
      else
         eai = 0.
      end if
      if (maenfruit <= 0.) eai = 0.

      ! ** les r�serves sont le compl�mentaire
      mareserve = masecveg - mafeuil_repartir - matigestruc - maenfruit
      if (mareserve < 0.) mareserve = 0.

      ! ** pour les p�rennes les r�serves migrent vers des
      ! *- organes de stockage (racines ou bois)

      ! *- un seul compartiment de r�serve
      ! *- NB - calcul des r�serves en deux �tapes
      ! *- pour �viter de consommer des r�serves
      ! *- qui n'existe pas

      deltares = mareserve - mareservep
      resperenne = max(resperenne + deltares, 0.0)

      ! TODO : fonction taille
      ! ** taille
      if (n == ntaille .and. P_codetaille == 2) then
         call repartir_taille(mafeuil_repartir, P_codeperenne, P_codeinitprec, & ! IN
                              mabois, lai_repartir, P_resperenne0, masec, msresjaune, & ! INOUT
                              mafeuiljaune, msneojaune, matigestruc)
      end if

   end if

   ! ** arr�t � la r�colte
   ! *- PB - 10/12/2004 - pour l'enchainement des ligneux (vigne),
   ! *- on ne remet pas � z�ro ce qui n'a pas lieu d'etre
   ! *- TODO : voir avec nadine s'il ne faut pas qd meme remettre certaines choses � z�ro.
   if (masec <= 0.0) then
      mafeuil_repartir = 0.0
      mafeuilverte_repartir = 0.0
      mafeuiljaune = 0.0
      matigestruc = 0.0
      masecveg = 0.0
      maenfruit = 0.0
      mareserve = 0.0
      pfeuil = 0.0
      pfeuilverte = 0.0
      pfeuiljaune = 0.0
      ptigestruc = 0.0
      penfruit = 0.0
      preserve = 0.0

! DR 28/02/2017 quand on a recolte (sauf les cultures fauche) on a plus de fruit et plus de eai
      if (nrec > 0 .and. n > nrec) then
         if (P_codcueille == 1 .and. P_codefauche /= 1) then
            eai = 0.0
         end if
      end if
! fin DR 28/02/2017

   else
      ! ** m�moire de pfeuil pour la r�partition
      ! *- de la biomasse s�n�scente dans senescen.for
      pfeuil = mafeuil_repartir/masec

      ! *- Nb & PB - le 23/02/05
      ! *- pfeuilverte calcul� en fonction des deltas et non des masses cumul�es
      !-- pfeuilverte = mafeuilverte_repartir / (masec-mafeuiljaune)
      if (dltams > 0.) then
         ! Nb le 09/07/06 mafeuilleverte et jaune sont en structural (P_slamax au lieu de sla)
         !-- pfeuilverte = (deltai_repartir/sla*1e2) / dltams
         pfeuilverte = (deltai_repartir/P_slamax*1e2)/dltams
         pfeuilverte = min(1.0, pfeuilverte)
      else
         pfeuilverte = 0.0
      end if

      pfeuiljaune = mafeuiljaune/masec

      ptigestruc = matigestruc/masec
      penfruit = maenfruit/masec
      preserve = resperenne/masec
   end if

   return
end subroutine repartir

subroutine repartir_taille(mafeuil, P_codeperenne, P_codeinitprec, & ! IN
                           mabois, lai, P_resperenne0, masec, msresjaune, & ! INOUT
                           mafeuiljaune, msneojaune, matigestruc)

   real, intent(IN)    :: mafeuil   ! // OUTPUT // Dry matter of leaves // t.ha-1
   integer, intent(IN)    :: P_codeperenne  ! // PARAMETER // option defining the annual (1) or perenial (2) character of the plant // code 1/2 // PARPLT // 0
   integer, intent(IN)    :: P_codeinitprec  ! // PARAMETER // reinitializing initial status in case of chaining simulations : yes (1), no (2) // code 1/2 // PARAM // 0

   real, intent(INOUT) :: mabois   ! // OUTPUT // Prunning dry weight // t.ha-1
   real, intent(INOUT) :: lai   ! // OUTPUT // Leaf area index (table) // m2 leafs  m-2 soil
   real, intent(INOUT) :: P_resperenne0  ! // PARAMETER // initial reserve biomass // t ha-1 // INIT // 1
   real, intent(INOUT) :: masec   ! // OUTPUT // Aboveground dry matter  // t.ha-1
   real, intent(OUT)   :: msresjaune   ! // OUTPUT // Senescent residual dry matter  // t.ha-1
   real, intent(OUT)   :: mafeuiljaune   ! // OUTPUT // Dry matter of yellow leaves // t.ha-1
   real, intent(OUT)   :: msneojaune   ! // OUTPUT // Newly-formed senescent dry matter  // t.ha-1
   real, intent(OUT)   :: matigestruc   ! // OUTPUT // Dry matter of stems (only structural parts) // t.ha-1

   mabois = matigestruc + mafeuil

   ! *- PB&Inaki - 08/03/2005 - on enl�ve plus les feuilles tomb�es de mabois... absurde !!
   !- mabois = matigestruc + mafeuil - mafeuiltombe

   ! *- PB & inaki - 08/12/2004
   ! *- � la taille, il faut r�initialiser les variables plantes
   lai = 0.0

   if (P_codeperenne == 2 .and. P_codeinitprec == 2) then
      ! 07/09/06 DR et IGC on rajoute un then et on garde reserveN
      P_resperenne0 = masec - mabois
      !-- P_QNplante0 = QNplante(1,n)+QNplante(2,n)
   end if

   ! *- INAKI - 08/03/2005 - on change le mode de calcul du masec � la taille.
   !-- masec = masec - mabois
   masec = 0.
   msresjaune = 0.0
   mafeuiljaune = 0.0
   msneojaune = 0.0
   matigestruc = 0.0
   !-- sometemp = 0.

end subroutine
