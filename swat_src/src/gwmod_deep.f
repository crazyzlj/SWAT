      subroutine gwmod_deep
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates groundwater contribution to
!!    streamflow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bfe_d(:)|none         |Exp(-alpha_bf_d(:))
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    gw_qdeep(:) |mm H2O        |groundwater contribution to streamflow from deep aquifer from
!!                               |HRU on current day
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on
!!                               |current day in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    rchrg1      |mm H2O        |amount of water entering deep aquifer on
!!                               |previous day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
!!    revap is subtracted and rchrg is delayed (johnson, 1977)

      use parm

      integer :: j
      real :: rchrg1

      j = 0
      j = ihru

      rchrg1 = 0.
      rchrg1 = gwseep


!! compute groundwater contribution to streamflow for day (deep aquifer)
!      if (shallst(j) > gwqmn(j)) then
        gw_qdeep(j) = gw_qdeep(j) * alpha_bfe_d(j) + gwseep *           
     &                                         (1. - alpha_bfe_d(j))
 !     else
 !       gw_qdeep(j) = 0.
 !     end if


!! remove ground water flow from deep aquifer storage
      deepst(j) = deepst(j) - gw_qdeep(j)

      return
      end