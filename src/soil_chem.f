      subroutine soil_chem

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil chemical properties

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)    |none          |pesticide use flag:
!!                                 | 0: no pesticides used in HRU
!!                                 | 1: pesticides used in HRU
!!    i             |none          |HRU number
!!    nactfr        |none          |nitrogen active pool fraction. The fraction
!!                                 |of organic nitrogen in the active pool.
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    npno(:)       |none          |array of unique pesticides used in watershed
!!    psp           |none          |Phosphorus availability index. The fraction
!!                                 |of fertilizer P remaining in labile pool
!!                                 |after initial rapid phase of P sorption.
!!    skoc(:)       |(mg/kg)/(mg/L)|soil adsorption coefficient normalized
!!                                 |for soil organic carbon content
!!    sol_bd(:,:)   |Mg/m**3       |bulk density of the soil
!!    sol_cbn(:,:)  |%             |percent organic carbon in soil layer
!!    sol_nly(:)    |none          |number of soil layers 
!!    sol_no3(:,:)  |mg N/kg soil  |nitrate concentration in soil layer
!!    sol_orgn(:,:) |mg/kg         |organic N concentration in soil layer
!!    sol_orgp(:,:) |mg/kg         |organic P concentration in soil layer
!!    sol_pst(:,:,1)|kg/ha         |initial amount of pesticide in first layer
!!                                 |read in from .chm file
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil layer
!!                                 |classified as residue
!!    sol_solp(:,:) |mg/kg         |solution P concentration in soil layer
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    basminpi      |kg P/ha       |average amount of phosphorus initially in
!!                                 |the mineral P pool in watershed soil
!!    basno3i       |kg N/ha       |average amount of nitrogen initially in the
!!                                 |nitrate pool in watershed soil
!!    basorgni      |kg N/ha       |average amount of nitrogen initially in
!!                                 |the organic N pool in watershed soil
!!    basorgpi      |kg P/ha       |average amount of phosphorus initially in
!!                                 |the organic P pool in watershed soil
!!    conv_wt(:,:)  |none          |factor which converts kg/kg soil to kg/ha
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_cov(:)    |kg/ha         |amount of residue on soil surface
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_hum(:,:)  |kg humus/ha   |amount of organic matter in the soil layer
!!                                 |classified as humic substances
!!    sol_kp(:,:,:) |(mg/kg)/(mg/L)|pesticide sorption coefficient, Kp; the
!!                                 |ratio of the concentration in the solid
!!                                 |phase to the concentration in solution
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool. This variable is read in as
!!                                 |a concentration and converted to kg/ha.
!!                                 |(this value is read from the .sol file in
!!                                 |units of mg/kg)
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool NOTE UNIT CHANGE!
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool NOTE UNIT CHANGE!
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer NOTE UNIT
!!                                 |CHANGE!
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!                                 |NOTE UNIT CHANGE!
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus 
!!                                 |pool
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dg          |mm            |depth of layer
!!    j           |none          |counter
!!    jj          |none          |dummy variable to hold value
!!    n           |none          |counter
!!    nly         |none          |number of soil layers
!!    soldepth    |mm            |depth from bottom of 1st soil layer to
!!                               |the bottom of the layer of interest
!!    solpst      |mg/kg         |concentration of pesticide in soil
!!    summinp     |kg P/ha       |amount of phosphorus stored in the mineral P
!!                               |pool in the profile
!!    sumno3      |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in the soil profile
!!    sumorgn     |kg N/ha       |amount of nitrogen stored in the organic N
!!                               |pools in the profile
!!    sumorgp     |kg P/ha       |amount of phosphorus stored in the organic P
!!                               |pools in the profile
!!    wt1         |none          |converts mg/kg (ppm) to kg/ha
!!    xx          |none          |variable to hold value
!!    zdst        |none          |variable to hold value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: nly, j, jj, n
      real :: xx, dg, wt1, zdst, soldepth, sumno3, sumorgn, summinp
      real :: sumorgp, solpst

      nly = 0
      solpst = 0.
      sumno3 = 0.
      sumorgn = 0.
      summinp = 0.
      sumorgp = 0.
      nly = sol_nly(i)

!!    calculate sol_cbn for lower layers if only have upper layer
      if (nly >= 3 .and. sol_cbn(3,i) <= 0) then
        do j = 3, nly
          if (sol_cbn(j,i) == 0.) then
            soldepth = 0
            soldepth = sol_z(j,i) - sol_z(2,i)
            sol_cbn(j,i) = sol_cbn(j-1,i) * Exp(-.001 * soldepth)
          end if
        end do
      end if

      cmup_kgh = 0.
      cmtot_kgh = 0.
      do j = 1, nly
        if (j == 1) then
          sol_thick = sol_z(j,i)
        else
          sol_thick = sol_z(j,i) - sol_z(j-1,i)
        end if
 
!! soil carbon and nitrogen
        sol_mass = (sol_thick / 1000.) * 10000. * sol_bd(j,i)
     &       * 1000. * (1 - sol_rock(j,i) / 100.)
        sol_cmass = sol_mass * (sol_cbn(j,i) / 100.)

        if (j == 1) cmup_kgh(i) = sol_cmass
        cmtot_kgh(i) = cmtot_kgh(i) + sol_cmass
      end do


!!    calculate sol_kp as function of koc and sol_cbn
!!    and set initial pesticide in all layers equal to value given for
!!    upper layer
      if (hrupest(i) == 1) then
      do j = 1, npmx
        jj = 0
        jj = npno(j)
        if (jj > 0) then
          solpst = 0.
          solpst = sol_pst(j,i,1)  !!concentration of pesticide in soil
          xx = 0.
          do n = 1, nly
            dg = 0.
            wt1 = 0.
            dg = (sol_z(n,i) - xx)
            xx = sol_z(n,i) 
            wt1 = sol_bd(n,i) * dg / 100.              !! mg/kg => kg/ha
            sol_kp(j,i,n) = skoc(jj) * sol_cbn(n,i) / 100.
            sol_pst(j,i,n) = solpst * wt1
          end do
        end if
      end do
      end if


!!    calculate initial nutrient contents of layers, profile and
!!    average in soil for the entire watershed
!!    convert mg/kg (ppm) to kg/ha
      xx = 0.
      sol_fop(1,i) = sol_rsd(1,i) * .0010 !! was 0.0003 Armen January 2009
      sol_fon(1,i) = sol_rsd(1,i) * .0055 !! was 0.0015 Armen January 2009
      sol_cov(i) = sol_rsd(1,i)
      do j = 1, nly
        dg = 0.
        wt1 = 0.
        dg = (sol_z(j,i) - xx)
        wt1 = sol_bd(j,i) * dg / 100.              !! mg/kg => kg/ha
        conv_wt(j,i) = 1.e6 * wt1                  !! kg/kg => kg/ha

        if (sol_no3(j,i) <= 0.) then
          zdst = 0.
          zdst = Exp(-sol_z(j,i) / 1000.)
          sol_no3(j,i) = 10. * zdst * .7
        end if
        sol_no3(j,i) = sol_no3(j,i) * wt1          !! mg/kg => kg/ha
        sumno3 = sumno3 + sol_no3(j,i)

        if (sol_orgn(j,i) > 0.0001) then
          sol_orgn(j,i) = sol_orgn(j,i) * wt1      !! mg/kg => kg/ha
        else
          !! assume C:N ratio of 10:1
          sol_orgn(j,i) = 10000. * (sol_cbn(j,i) / 11.) * wt1  !! CN ratio was 14 before 01-22-09 Armen
        end if
        sol_aorgn(j,i) = sol_orgn(j,i) * nactfr
        sol_orgn(j,i) = sol_orgn(j,i) * (1. - nactfr)
        sumorgn = sumorgn + sol_aorgn(j,i) + sol_orgn(j,i) +            &
     &            sol_fon(j,i)

        if (sol_orgp(j,i) > 0.0001) then
          sol_orgp(j,i) = sol_orgp(j,i) * wt1      !! mg/kg => kg/ha
        else
	!! assume N:P ratio of 8:1 
          sol_orgp(j,i) = .125 * sol_orgn(j,i)   
        end if

        if (sol_solp(j,i) > 0.0001) then
          sol_solp(j,i) = sol_solp(j,i) * wt1      !! mg/kg => kg/ha
        else
          !! assume initial concentration of 5 mg/kg
          sol_solp(j,i) = 5. * wt1
        end if
        sol_actp(j,i) = sol_solp(j,i) * (1. - psp) / psp
        sol_stap(j,i) = 4. * sol_actp(j,i)
        sol_hum(j,i) = sol_cbn(j,i) * wt1 * 17200.
        xx = sol_z(j,i)
        summinp = summinp + sol_solp(j,i) + sol_actp(j,i) +             &
     &        sol_stap(j,i)
        sumorgp = sumorgp + sol_orgp(j,i) + sol_fop(j,i)
      end do

      basno3i = basno3i + sumno3 * hru_km(i) / da_km
      basorgni = basorgni + sumorgn * hru_km(i) / da_km
      basminpi = basminpi + summinp * hru_km(i) / da_km
      basorgpi = basorgpi + sumorgp * hru_km(i) / da_km

      return
      end
