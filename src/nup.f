      subroutine nup
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine calculates plant nitrogen uptake

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_n1(:)   |none           |1st shape parameter for plant N uptake
!!                                |equation
!!    bio_n2(:)   |none           |2nd shape parameter for plant N uptake
!!                                |equation
!!    bioday      |kg             |biomass generated on current day in HRU
!!    fixn        |kg N/ha        |amount of nitrogen added to soil via fixation
!!                                |on the day in HRU
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    idc(:)      |none           |crop/landcover category:
!!                                |1 warm season annual legume
!!                                |2 cold season annual legume
!!                                |3 perennial legume
!!                                |4 warm season annual
!!                                |5 cold season annual
!!                                |6 perennial
!!                                |7 trees
!!    idplt(:)    |none           |land cover code from crop.dat
!!    ihru        |none           |HRU number
!!    nro(:)      |none           |sequence number of year in rotation
!!    phuacc(:)   |none           |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha        |amount of nitrogen in plant biomass
!!    pltnfr(1,:) |kg N/kg biomass|nitrogen uptake parameter #1: normal fraction
!!                                |of N in crop biomass at emergence
!!    pltnfr(3,:) |kg N/kg biomass|nitrogen uptake parameter #3: normal fraction
!!                                |of N in crop biomass at maturity
!!    sol_nly(:)  |none           |number of soil layers in profile
!!    sol_no3(:,:)|kg N/ha        |amount of nitrogen stored in the
!!                                |nitrate pool.
!!    sol_z(:,:)  |mm             |depth to bottom of soil layer
!!    n_updis     |none           |nitrogen uptake distribution parameter
!!                                |This parameter controls the amount of
!!                                |nitrogen removed from the different soil 
!!                                |layers by the plant. In particular, this
!!                                |parameter allows the amount of nitrogen
!!                                |removed from the surface layer via plant
!!                                |uptake to be controlled. While the relation-
!!                                |ship between UBN and N removed from the
!!                                |surface layer is affected by the depth of the
!!                                |soil profile, in general, as UBN increases the
!!                                |amount of N removed from the surface layer
!!                                |relative to the amount removed from the entire
!!                                |profile increases
!!    uobn        |none           |nitrogen uptake normalization parameter
!!                                |This variable normalizes the nitrogen uptake
!!                                |so that the model can easily verify that
!!                                |upake from the different soil layers sums to
!!                                |1.0
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nplnt(:)    |kg N/ha       |plant uptake of nitrogen in HRU for the day
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
!!    pltfr_n(:)  |none          |fraction of plant biomass that is nitrogen
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in the layer
!!    strsn(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |nitrogen stress
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    gx          |mm            |lowest depth in layer from which nitrogen
!!                               |may be removed
!!    icrop       |none          |land cover code
!!    ir          |none          |flag to denote bottom of root zone reached
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layer)
!!    un2         |kg N/ha       |ideal plant nitrogen content
!!    unmx        |kg N/ha       |maximum amount of nitrogen that can be 
!!                               |removed from soil layer
!!    uno3l       |kg N/ha       |amount of nitrogen removed from soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min
!!    SWAT: nfix, nuts

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, icrop, l, ir
      real :: un2, unmx, uno3l, gx

      j = 0
      j = ihru

      tno3 = 0.
      do l = 1, sol_nly(j)
        tno3 = tno3 + sol_no3(l,j)
      end do
      tno3 = tno3 / n_reduc(j)
      up_reduc = tno3 / (tno3 + Exp(1.56 - 4.5 * tno3))
      
      icrop = idplt(j)
      pltfr_n(j) = (pltnfr(1,icrop) - pltnfr(3,icrop)) * (1. - phuacc(j)
     &         / (phuacc(j) + Exp(bio_n1(icrop) - bio_n2(icrop) *       
     &         phuacc(j)))) + pltnfr(3,icrop)

      un2 = 0.
      un2 = pltfr_n(j) * bio_ms(j)
      if (un2 < plantn(j)) un2 = plantn(j)
      uno3d = un2 - plantn(j)
      uno3d = Min(4. * pltnfr(3,icrop) * bioday, uno3d)

      strsn(j) = 1.
      ir = 0
      if (uno3d < 1.e-6) return

      do l = 1, sol_nly(j)
        if (ir > 0) exit

        gx = 0.
        if (sol_rd <= sol_z(l,j)) then
          gx = sol_rd
          ir = 1
        else
          gx = sol_z(l,j)
        end if

        unmx = 0.
        uno3l = 0.
        unmx = uno3d * (1. - Exp(-n_updis * gx / sol_rd)) / uobn
        uno3l = Min(unmx - nplnt(j), sol_no3(l,j))
        !uno3l = up_reduc * uno3l
        nplnt(j) = nplnt(j) + uno3l 
        sol_no3(l,j) = sol_no3(l,j) - uno3l
      end do
      if (nplnt(j) < 0.) nplnt(j) = 0.

!! if crop is a legume, call nitrogen fixation routine
      select case (idc(idplt(j)))
        case (1,2,3)
          call nfix
      end select

      nplnt(j) = nplnt(j) + fixn
      plantn(j) = plantn(j) + nplnt(j)
 
!! compute nitrogen stress
      select case (idc(idplt(j)))
        case (1,2,3)
          strsn(j) = 1.
        case default
          call nuts(plantn(j),un2,strsn(j))
          if (uno3d > 1.e-5) then
            xx = nplnt(j) / uno3d
          else
            xx = 1.
          end if
          strsn(j) = amax1(strsn(j), xx)
          strsn(j) = amin1(strsn(j), 1.)
      end select

      return
      end