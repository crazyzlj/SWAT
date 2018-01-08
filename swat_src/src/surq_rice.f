      subroutine surq_rice

    !!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
    !!    name        |units         |definition
    !!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    !!    precipday   |mm H2O        |precipitation for the day in HRU
    !!    pot_k       |(mm/hr)       |hydraulic conductivity of soil surface of pothole
    !!                   [defaults to conductivity of upper soil (0.01--10.) layer]
    !!    hru_ha(:)     |ha            |area of HRU in hectares

    !!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
    !!    name           |units         |definition
    !!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    !!    pot_vol(:)     |mm            |current volume of water stored in the
    !!                                  |depression/impounded area
    !!    pot_spillo(:)  |mm            |amount of water released to main channel from
    !!                                  |impounded water body due to spill-over
    !!    pot_seep(:)    |mm            |amount of water seepage to soil

      use parm

      real :: yy, potvol_sep

      j = 0
      j = ihru

      ! add precipation to the water layer of paddy rice
      pot_vol(j) = pot_vol(j) + precipday

       ! if overflow, then send the overflow to the HRU surface flow
      if (pot_vol(j) > prpnd_max(j)) then
        qdr(j) = qdr(j) + (pot_vol(j)- prpnd_max(j))
        !          qday = qday + (pot_vol(j)- pot_volxmm(j))
        pot_spillo(j) = pot_vol(j) - prpnd_max(j)
        pot_vol(j) = prpnd_max(j)
      end if       !! if overflow

    !      compute seepage, pot_seep will be used in percmain.f
      if (pot_vol(j) > 1.e-6) then
!        limit seepage into soil if profile is near field capacity
         if (pot_k(j) > 0.) then
           yy = pot_k(j)
         else
           yy = sol_k(1,j)
         endif

!        calculate seepage into soil
         potsep = yy * 24.
         potsep = Min(potsep, pot_vol(j))
         potvol_sep = pot_vol(j)
         pot_vol(j) = pot_vol(j) - potsep
         pot_seep(j) = potsep
      endif

      ! evaporation will be calculated in etact.f

      end subroutine surq_rice