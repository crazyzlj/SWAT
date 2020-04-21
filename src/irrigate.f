      subroutine irrigate(jj,volmm)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies irrigation water to HRU

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aairr(:)    |mm H2O        |average annual amount of irrigation water
!!                               |applied to HRU
!!    curyr       |none          |current year of simulation
!!    irn(:)      |none          |average annual number of irrigation 
!!                               |applications in HRU
!!    nyskip      |none          |number of years to skip output summarization
!!                               |and printing
!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc - wp)
!!    sol_nly(:)  |none          |number of soil layers
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer
!!                               |on any given day (less wp water)
!!    hrumono(22,:)|mm H2O        |amount of irrigation water applied to HRU
!!                               |during month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aairr(:)    |mm H2O        |average annual amount of irrigation water
!!                               |applied to HRU
!!    aird(:)     |mm H2O        |amount of water applied to HRU on current
!!                               |day
!!    irn(:)      |none          |average annual number of irrigation 
!!                               |applications in HRU
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer
!!                               |on any given day (less wp water)
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on any given day
!!    hrumono(22,:)|mm H2O        |amount of irrigation water applied to HRU
!!                               |during month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fcx         |mm H2O        |amount of water stored in soil layer when
!!                               |moisture content is at field capacity
!!    jj          |none          |HRU number
!!    k           |none          |counter (soil layers)
!!    stx         |mm H2O        |amount of water stored in soil layer on 
!!                               |current day
!!    volmm       |mm H2O        |depth irrigation water applied to HRU
!!    yy          |mm H2O        |amount of water added to soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: jj
      real, intent (in out) :: volmm
      integer :: k
      real :: fcx, stx, yy

!! initialize variable for HRU
!! (because irrigation can be applied in different command loops
!! the variable is initialized here)

      aird(jj) = volmm * (1. - sq_rto)
      qird(jj) = volmm * sq_rto

!! summary calculations
      if (curyr > nyskip) then 
        irn(jj) = irn(jj) + 1
        aairr(jj) = aairr(jj) + aird(jj)
        hrumono(22,jj) = hrumono(22,jj) + aird(jj)
      end if


      return
      end
