      subroutine npup
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates plant phosphorus uptake

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none           |current year of simulation
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_p1(:)   |none           |1st shape parameter for plant P uptake
!!                                |equation
!!    bio_p2(:)   |none           |2st shape parameter for plant P uptake
!!                                |equation
!!    bioday      |kg             |biomass generated on current day in HRU
!!    hru_dafr(:) |km**2/km**2    |fraction of watershed area in HRU
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    idplt(:)    |none           |land cover code from crop.dat
!!    ihru        |none           |HRU number
!!    nro(:)      |none           |sequence number of year in rotation
!!    nyskip      |none           |number of years to skip output summarization/
!!                                |printing
!!    p_updis     |none           |phosphorus uptake distribution parameter
!!                                |This parameter controls the amount of
!!                                |phosphorus removed from the different soil
!!                                |layers by the plant. In particular, this
!!                                |parameter allows the amount of phosphorus
!!                                |removed from the surface layer via plant
!!                                |uptake to be controlled. While the relation-
!!                                |ship between UBP and P uptake from the
!!                                |surface layer is affected by the depth of the
!!                                |soil profile, in general, as UBP increases
!!                                |the amount of P removed from the surface
!!                                |layer relative to the amount removed from the
!!                                |entire profile increases
!!    phuacc(:)   |none           |fraction of plant heat units accumulated
!!    plantp(:)   |kg P/ha        |amount of phosphorus stored in plant
!!    pltpfr(1,:) |kg P/kg biomass|phosphorus uptake parameter #1: normal
!!                                |fraction of P in crop biomass at emergence
!!    pltpfr(3,:) |kg P/kg biomass|phosphorus uptake parameter #3: normal
!!                                |fraction of P in crop biomass at maturity
!!    sol_nly(:)  |none           |number of soil layers in profile
!!    sol_solp(:,:)|kg P/ha       |amount of phosohorus stored in solution
!!    sol_z(:,:)  |mm             |depth to bottom of soil layer
!!    uobp        |none           |phosphorus uptake normalization parameter
!!                                |This variable normalizes the phosphorus
!!                                |uptake so that the model can easily verify
!!                                |that uptake from the different soil layers
!!                                |sums to 1.0
!!    wshd_pup    |kg P/ha        |average annual amount of plant uptake of 
!!                                |phosphorus 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    plantp(:)   |kg P/ha       |amount of phosphorus stored in plant
!!    pltfr_p(:)  |none          |fraction of plant biomass that is phosphorus
!!    pplnt(:)    |kg P/ha       |plant uptake of phosphorus in HRU for the day
!!    sol_solp(:,:)|kg P/ha      |amount of phosohorus stored in solution
!!    strsp(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |phosphorus stress
!!    wshd_pup    |kg P/ha       |average annual amount of plant uptake of 
!!                               |phosphorus
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
     
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |mm            |lowest depth in layer from which phosphorus
!!                               |may be removed
!!    icrop       |none          |land cover code
!!    ir          |none          |flag for bottom of root zone
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layers)
!!    uapd        |kg P/ha       |plant demand of phosphorus
!!    uapl        |kg P/ha       |amount of phosphorus removed from layer
!!    up2         |kg P/ha       |optimal plant phosphorus content
!!    upmx        |kg P/ha       |maximum amount of phosphorus that can be
!!                               |removed from the soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, icrop, l, ir
      real :: up2, uapd, upmx, uapl, gx

      j = 0
      j = ihru

      icrop = 0
      icrop = idplt(j)
      pltfr_p(j) = (pltpfr(1,icrop) - pltpfr(3,icrop)) * (1. - phuacc(j)&
     &   / (phuacc(j) + Exp(bio_p1(icrop) - bio_p2(icrop) * phuacc(j))))&
     &   + pltpfr(3,icrop)

      up2 = 0.
      uapd = 0.
      up2 = pltfr_p(j) * bio_ms(j)
      if (up2 < plantp(j)) up2 = plantp(j)
      uapd = up2 - plantp(j)
      uapd = Min(4. * pltpfr(3,icrop) * bioday, uapd)
      uapd = 1.5 * uapd                         !! luxury p uptake

      strsp(j) = 1.
      ir = 0
      if (uapd < 1.e-6) return

      do l = 1, sol_nly(j)
        if (ir > 0) exit

        gx = 0.
        if (sol_rd <= sol_z(l,j)) then
          gx = sol_rd
          ir = 1
        else
          gx = sol_z(l,j)
        end if

        upmx = 0.
        uapl = 0.
        upmx = uapd * (1. - Exp(-p_updis * gx / sol_rd)) / uobp
        uapl = Min(upmx - pplnt(j), sol_solp(l,j))
        pplnt(j) = pplnt(j) + uapl
        sol_solp(l,j) = sol_solp(l,j) - uapl
      end do
      if (pplnt(j) < 0.) pplnt(j) = 0.

      plantp(j) = plantp(j) + pplnt(j)

!! compute phosphorus stress
      call nuts(plantp(j), up2, strsp(j))

!! summary calculations
      if (curyr > nyskip) then
        wshd_pup = wshd_pup + pplnt(j) * hru_dafr(j)
      end if

      return
      end
