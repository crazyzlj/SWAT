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
      real*8 :: tn, tp, qtot, org_c, tn_tp, ww, xx, yy, zz, flow_cms

      j = 0
      j = ihru

        !! calculcate water temperature
        !! Stefan and Preudhomme. 1993.  Stream temperature estimation
        !!from air temperature.  Water Res. Bull. p. 27-45
        !! SWAT manual 2.3.13
        call temparms
        !wtmp = 5.0 + 0.75 * tmpav(j)
        if (wtmp <= 0.1) wtmp = 0.1
        wtmp = wtmp + 273.15    !! deg C to deg K
      
        if (qdr(j) > 1.e-4) then
          tp = 100. * (sedorgn(j) + surqno3(j)) / qdr(j)   !100*kg/ha/mm = ppm 
          chl_a(j) = chla_subco * tp
 
          !! calculate organic carbon loading to main channel
          org_c = 0.
          org_c = (sol_cbn(1,j) / 100.) * enratio * sedyld(j) * 1000.
          
          !!add by zhang
          !!========================
          if (cswat == 2) then
            org_c = sedc_d(j)*hru_ha(j)
          end if
          !!add by zhang
          !!========================
          
                  
          !! calculate carbonaceous biological oxygen demand (CBOD)
          cbodu(j) = cbodu(j) + 2.7 * org_c / (qdr(j) * hru_km(j)) !jaehak 2016

          !! calculate dissolved oxygen saturation concentration
          !! QUAL2E equation III-29
          ww = -139.34410 + (1.575701E05 / wtmp)
          xx = 6.642308E07 / (wtmp**2)
          yy = 1.243800E10 / (wtmp**3)
          zz = 8.621949E11 / (wtmp**4)
          soxy = Exp(ww - xx + yy - zz)
          if (soxy < 0.) soxy = 0.

          !! calculate actual dissolved oxygen concentration
          doxq(j) = soxy * exp(-0.1 * cbodu(j))
          if (doxq(j) < 0.0) doxq(j) = 0.0
          if (doxq(j) > soxy) doxq(j) = soxy
        else
          chl_a(j) = 0.
          cbodu(j) = 0.
          doxq(j) = 0.
        end if

      return
      end