      subroutine gwmod
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates groundwater contribution to
!!    streamflow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bf(:) |1/days        |alpha factor for groundwater recession curve
!!    alpha_bfe(:)|none          |Exp(-alpha_bf(:))
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    ihru        |none          |HRU number
!!    gw_delaye(:)|none          |Exp(-1./(delay(:)) where delay(:) is the 
!!                               |groundwater delay (time required for water
!!                               |leaving the bottom of the root zone to reach
!!                               |the shallow aquifer; units-days)
!!    gw_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from the shallow aquifer to
!!                               |the root zone as a result of soil moisture
!!                               |depletion
!!    gw_spyld(:) |m**3/m**3     |specific yield for shallow aquifer
!!    gwht(:)     |m             |groundwater height
!!    gwqmn(:)    |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required before groundwater flow will occur
!!    pet_day     |mm H2O        |potential evapotranspiration on current day
!!                               |in HRU
!!    rchrg(:)    |mm H2O        |amount of water entering shallow aquifer on
!!                               |previous day in HRU
!!    rchrg_dp(:) |none          |recharge to deep aquifer: the fraction of
!!                               |root zone percolation that reaches the deep
!!                               |aquifer
!!    revapmn(:)  |mm H2O        |threshold depth of water in shallow aquifer
!!                               |required to allow revap to occur
!!    sepbtm(:)   |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    gw_q(:)     |mm H2O        |groundwater contribution to streamflow from
!!                               |HRU on current day
!!    gwht(:)     |m             |groundwater height
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on
!!                               |current day in HRU
!!    rchrg(:)    |mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU
!!    revapday    |mm H2O        |amount of water moving from the shallow 
!!                               |aquifer into the soil profile or being taken
!!                               |up by plant roots in the shallow aquifer
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    rchrg1      |mm H2O        |amount of water entering shallow aquifer on
!!                               |previous day
!!    rchrg_karst |mm H2O        |amount of water from secondary channels,
!!                               |ponds, and wetlands
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
!!    revap is subtracted and rchrg is delayed (johnson, 1977)

      use parm

      integer :: j
      real*8 :: rchrg1, rchrg_karst

      j = 0
      j = ihru

      rchrg1 = 0.
      rchrg_karst = 0.
      rchrg1 = rchrg(j) + rchrg_src(j)

!! add seepage from secondary channels, ponds, and wetlands;
      rchrg_karst = tloss + twlpnd(j) + twlwet(j)
      twlpnd(j) = 0.
      twlwet(j) = 0.
      
!! compute shallow aquifer level for current day, assumes karst losses 
!! infiltrate at the same speed as what goes through the soil profile.
      rchrg(j) = 0.
      rchrg(j) = (1.-gw_delaye(j)) * (sepbtm(j) + gwq_ru(j) +           
     &                             rchrg_karst) + gw_delaye(j) * rchrg1
      if (rchrg(j) < 1.e-6) rchrg(j) = 0.
      gwq_ru(j) = 0.

!! compute deep aquifer level for day
      gwseep = rchrg(j) * rchrg_dp(j)
      deepst(j) = deepst(j) + gwseep

      shallst(j) = shallst(j) + (rchrg(j) - gwseep)
      gwht(j) = gwht(j) * alpha_bfe(j) + rchrg(j) * (1. - alpha_bfe(j)) 
     &    / (800. * gw_spyld(j) * alpha_bf(j) + 1.e-6)
      gwht(j) = Max(1.e-6, gwht(j))

!! compute groundwater contribution to streamflow for day
      if (shallst(j) > gwqmn(j)) then
        gw_q(j) = gw_q(j) * alpha_bfe(j) + (rchrg(j) - gwseep ) *       
     &                                               (1. - alpha_bfe(j))
      else
        gw_q(j) = 0.
      end if
      
      !! bmp adjustment
      gw_q(j) = gw_q(j) * bmp_flos(j)

!! compute revap to soil profile/plant roots
      revapday = gw_revap(j) * pet_day
      if (shallst(j) < revapmn(j)) then
        revapday = 0.
      else
        shallst(j) = shallst(j) - revapday
        if (shallst(j) < revapmn(j)) then
          revapday = shallst(j) + revapday - revapmn(j)
          shallst(j) = revapmn(j)
        end if
      end if

!! remove ground water flow from shallow aquifer storage
      if (shallst(j) >= gwqmn(j)) then
        shallst(j) = shallst(j) - gw_q(j)
        if (shallst(j) < gwqmn(j)) then
          gw_q(j) = shallst(j) + gw_q(j) - gwqmn(j)
          shallst(j) = gwqmn(j)
        end if
       else
        gw_q(j) = 0.
      end if

      return
      end