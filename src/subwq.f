      subroutine subwq
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes HRU loadings of chlorophyll-a, CBOD, 
!!    and dissolved oxygen to the main channel

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    enratio     |none          |enrichment ratio calculated for day in HRU
!!    hru_km(:)   |km^2          |area of HRU in square kilometers
!!    ihru        |none          |HRU number
!!    qtile       |mm H2O        |drainage tile flow in soil layer for the
!!                               |day
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    sedorgp(:)  |kg P/ha       |amount of organic phosphorus in surface runoff
!!                               |in HRU for the day
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in
!!                               |HRU
!!    sol_cbn(:,:)|%             |percent organic carbon in soil layer
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    surqsolp(:) |kg P/ha       |amount of soluble phosphorus in surface runoff
!!                               |in HRU for the day
!!    t_ov(:)     |hr            |time for flow from farthest point in subbasin
!!                               |to enter a channel
!!    tmpav(:)    |deg C         |average air temperature on current day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cbodu(:)    |mg/L          |carbonaceous biological oxygen demand of 
!!                               |surface runoff on current day in HRU
!!    chl_a(:)    |microgram/L   |chlorophyll-a concentration in water yield
!!                               |on current day in HRU
!!    doxq(:)     |mg/L          |dissolved oxygen concentration in the surface
!!                               |runoff on current day in HRU
!!    soxy        |mg/L          |dissolved oxygen saturation concentration
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flow_cms    |m^3/s H2O     |rate of flow to main channel generated on
!!                               |day in HRU
!!    j           |none          |HRU number
!!    org_c       |kg            |organic carbon content of surface runoff on
!!                               |day in HRU
!!    qtot        |mm H2O        |total loadings to main channel generated on
!!                               |day in HRU
!!    tn          |kmoles N      |kilomoles of nitrogen in nutrient loading to
!!                               |main channel
!!    tn_tp       |mol N/mol P   |atomic ratio of N to P in surface runoff
!!    tp          |kmoles P      |kilomoles of phosphorus in nutrient loading to
!!                               |main channel
!!    wtmp        |deg K         |temperature of surface runoff
!!    ww          |none          |variable to hold intermediate calculation
!!                               |result
!!    xx          |none          |variable to hold intermediate calculation
!!                               |result
!!    yy          |none          |variable to hold intermediate calculation
!!                               |result
!!    zz          |none          |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real :: tn, tp, qtot, org_c, tn_tp, wtmp, ww, xx, yy, zz, flow_cms

      j = 0
      j = ihru

        !! calculcate water temperature
        !! Stefan and Preudhomme. 1993.  Stream temperature estimation
        !!from air temperature.  Water Res. Bull. p. 27-45
        !! SWAT manual 2.3.13
        wtmp = 0.
        wtmp = 5.0 + 0.75 * tmpav(j)
        if (wtmp <= 0.1) wtmp = 0.1
        wtmp = wtmp + 273.15    !! deg C to deg K

      select case(isubwq)

      case (1)
      if (surfq(j) > 0.0001) then
        !! calculate moles of N and P in surface runoff
        tn = 0.
        tp = 0.
        tn = ((sedorgn(j) + surqno3(j)) * hru_km(j) * 100.) / 14.
        tp = ((sedorgp(j) + surqsolp(j)) * hru_km(j) * 100.) / 31.

        !! calculate total water loading to main channel generated
        !! on day in HRU
        qtot = 0.
        flow_cms = 0.
        qtot = surfq(j)
        flow_cms = (qtot * hru_km(j)) / 86.4

        !! calculate chlorophyll-a loading from HRU
        !! use atomic ratio instead of actual ratio to do this
        tn_tp = 0.
        if (flow_cms > 1.e-5) then
          if (tp > 1.e-6) then
            tn_tp = tn / tp
            if (tn_tp <= 0.) then
              chl_a(j) = 0.0
            else 
              chl_a(j) = (10.**2.7) / (2. * flow_cms)
            end if
          elseif (tn > 1.e-6) then
            chl_a(j) = (10.**0.5) / (2. * flow_cms)
          else
            chl_a(j) = 0.0
          endif
        else
          chl_a(j) = 0.0
        endif

        !! testing for Steve B
        !  chl_a = chla_subco * chl_a
 

        !! calculate organic carbon loading to main channel
        org_c = 0.
        org_c = (sol_cbn(1,j) / 100.) * enratio * sedyld(j) * 1000.

        !! calculate carbonaceous biological oxygen demand (CBOD)
        cbodu(j) = 2.7 * org_c / (surfq(j) * hru_km(j))

        !! calculate dissolved oxygen saturation concentration
        !! QUAL2E equation III-29
        ww = 0.
        xx = 0.
        yy = 0.
        zz = 0.
        ww = -139.34410 + (1.575701E05 / wtmp)
        xx = 6.642308E07 / (wtmp**2)
        yy = 1.243800E10 / (wtmp**3)
        zz = 8.621949E11 / (wtmp**4)
        soxy = Exp(ww - xx + yy - zz)
        if (soxy < 0.) soxy = 0.

        !! calculate actual dissolved oxygen concentration
        !! based on QUAL2E equation III-28
        doxq(j) = soxy - (1.047 * cbodu(j) * t_ov(j) / 24.)
        if (doxq(j) < 0.0) doxq(j) = 0.0

      end if
      case default

        chl_a(j) = 0.

        cbodu(j) = 0.

        !! calculate dissolved oxygen saturation concentration
        !! QUAL2E equation III-29
        ww = 0.
        xx = 0.
        yy = 0.
        zz = 0.
        ww = -139.34410 + (1.575701E05 / wtmp)
        xx = 6.642308E07 / (wtmp**2)
        yy = 1.243800E10 / (wtmp**3)
        zz = 8.621949E11 / (wtmp**4)
        soxy = Exp(ww - xx + yy - zz)
        if (soxy < 0.) soxy = 0.
        doxq(j) = soxy

      end select

      return
      end
