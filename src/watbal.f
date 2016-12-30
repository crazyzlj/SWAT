      subroutine watbal
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the daily water balance for each HRU
!!    changes in storage should equal water losses from the system
!!    write statements can be uncommented for model debugging

!!    this subroutine will give errors for HRUs receiving irrigation water
!!    from reaches or reservoirs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)     |mm H2O        |amount of water applied to HRU on current
!!                               |day
!!    bsprev      |mm H2O        |surface runoff lagged from prior day
!!    bss(1,:)    |mm H2O        |amount of lateral flow lagged
!!    bssprev     |mm H2O        |lateral flow lagged from prior day of
!!                               |simulation
!!    curyr       |none          |current year of simulation
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    deepstp     |mm H2O        |depth of water in deep aquifer in HRU
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs on
!!                               |day in HRU
!!    es_day      |mm H2O        |actual amount of evaporation (from soil) that
!!                               |occurs on day in HRU
!!    etday       |mm H2O        |actual amount of evapotranspiration that
!!                               |occurs on day in HRU
!!    gw_q(:)     |mm H2O        |groundwater contribution to streamflow from
!!                               |HRU on current day
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on
!!                               |current day
!!    iida        |julian date   |current day of simulation
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates into
!!                               |soil (enters soil)
!!    ihru        |none          |HRU number
!!    latq(:)     |mm H2O        |amount of water in lateral flow in HRU for the
!!                               |day
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    qday        |mm H2O        |surface runoff loading to main channel for
!!                               |day in HRU (includes effects of transmission
!!                               |losses)
!!    qtile       |mm H2O        |drainage tile flow for day in HRU
!!    rchrg(:)    |mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU
!!    revapday    |mm H2O        |amount of water moving from the shallow
!!                               |aquifer into the soil profile or being taken
!!                               |up by plant roots in the shallow aquifer
!!    sepbtm(:)   |mm H2O        |seepage leaving the bottom of the soil profile
!!                               |on day in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    shallstp    |mm H2O        |depth of water in shallow aquifer in HRU on
!!                               |previous day
!!    sno_hru(:)  |mm H2O        |amount of water stored as snow
!!    snoev       |mm H2O        |amount of water in snow lost through
!!                               |sublimation on current day in HRU
!!    snofall     |mm H2O        |amount of precipitation falling as freezing
!!                               |rain/snow on day in HRU
!!    snomlt      |mm H2O        |amount of water in snow melt for the day in
!!                               |HRU
!!    snoprev     |mm H2O        |amount of water stored as snow on previous day
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on any
!!                               |given day
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    surf_bs(1,:)|mm H2O        |amount of surface runoff lagged over one
!!                               |day
!!    swprev      |mm H2O        |amount of water stored in soil profile in the
!!                               |HRU on the previous day
!!    tloss       |mm H2O        |amount of water removed from surface runoff
!!    twlpnd      |mm H2O        |water lost through seepage from ponds on day
!!                               |in HRU
!!    twlwet      |mm H2O        |water lost through seepage from wetlands on
!!                               |day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dstor       |mm H2O        |change in storage (snow, soil, shallow
!!                               |and deep aquifers)
!!    h2oloss     |mm H2O        |net movement of water out of system
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real :: dstor, h2oloss

      j = 0
      j = ihru

      dstor = 0.
      h2oloss = 0.

      if (ievent == 0) then
          dstor = sno_hru(j) - snoprev + sol_sw(j) - swprev +     
     &        shallst(j) - shallstp + deepst(j) - deepstp +             
     &        surf_bs(1,j) - bsprev + bss(1,j) - bssprev
      else
         dstor = sno_hru(j) - snoprev + sol_sw(j) - swprev +          
     &        shallst(j) - shallstp + deepst(j) - deepstp +           
     &        hhsurf_bs(1,j,nstep) - bsprev + bss(1,j) - bssprev
      endif

!!   subtraction of snoev term in h2oloss variable removed
!!   this term is already included in the variable:
!!        etday = ep_day + es_day + canev
!!   es_day includes the value of the variable snoev (see etact.f routine)
!$$$$$$       h2oloss = subp(j) - qday - latq(j) - etday - gw_q(j) -            &
!$$$$$$      &          revapday + twlpnd + twlwet + aird(j) + rchrg(j) - qtile &
!$$$$$$      &          - sepbtm(j)
      h2oloss = subp(j) - qday - latq(j) - qtile - etday - gw_q(j)    
     &          + aird(j) - revapday + rchrg(j) - sepbtm(j) - tloss  
          

!$$$$$$       write (17,100) iida, sno_hru(j), sol_sw(j), shallst(j), deepst(j),&
!$$$$$$      &                precipday, snofall, snomlt, snoev, inflpcp, qday, &
!$$$$$$      &                sepbtm(j), latq(j), es_day, ep_day, rchrg(j),     &
!$$$$$$      &                gw_q(j), revapday, gwseep, tloss
!$$$$$$ 
!$$$$$$       write (17,100) iida, dstor, h2oloss
!     if (Abs(dstor - h2oloss) > 0.001) then
!       write (17,101) j, iida, curyr, dstor - h2oloss
!     endif

!$$$$$$  100  format (i4, 20f8.3)
 101  format (' Water Balance Problem - Subbassin', i6,' Day/Year',2i4,
     *  f10.5,' mm')
      return
      end