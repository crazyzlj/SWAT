      subroutine nfix
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates nitrogen fixation by legumes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year of simulation
!!    hru_dafr(:) |km**2/km**2   |fraction of watershed area in HRU
!!    ihru        |none          |HRU number
!!    nplnt(:)    |kg N/ha       |plant uptake of nitrogen in HRU for the day
!!    nyskip      |none          |number of years to skip output printing/
!!                               |summarization
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    sol_nly(:)  |none          |number of layers in soil profile
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the
!!                               |nitrate pool of the layer
!!    sol_sumfc(:)|mm H2O        |amount of water held in the soil profile
!!                               |at field capacity
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on any given day
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fixn        |kg N/ha       |amount of nitrogen added to the plant via
!!                               |fixation on the day in HRU
!!    wshd_fixn   |kg N/ha       |average annual amount of nitrogen added to
!!                               |plant biomass via fixation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fixco       |none          |nitrogen fixation coefficient
!!    fxg         |
!!    fxn         |
!!    fxr         |
!!    fxw         |
!!    j           |none          |HRU number
!!    l           |none          |counter (soil layer)
!!    sumn        |kg N/ha       |total amount of nitrate stored in soil profile
!!    uno3l       |kg N/ha       |plant nitrogen demand
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, l
      real*8 :: uno3l, fxw, sumn, fxn, fxg, fxr

      j = 0
      j = ihru
 
!! compute the difference between supply and demand
      if (uno3d > nplnt(j)) then
        uno3l = 0.
        uno3l = uno3d - nplnt(j)
      else
        !! if supply is being met, fixation=0 and return
        fixn = 0.
        return
      endif


!! compute fixation as a function of no3, soil water, and growth stage

      !! compute soil water factor
      fxw = 0.
      fxw = sol_sw(j) / (.85 * sol_sumfc(j))

      !! compute no3 factor
      sumn = 0.
      fxn = 0.
      do l = 1, sol_nly(j)
        sumn = sumn + sol_no3(l,j)
      end do
      if (sumn > 300.) fxn = 0.
      if (sumn > 100. .and. sumn <= 300.) fxn = 1.5 - .0005 * sumn
      if (sumn <= 100.) fxn = 1.

      !! compute growth stage factor
      fxg = 0.
      if (phuacc(j) > .15 .and. phuacc(j) <= .30) then
         fxg = 6.67 * phuacc(j) - 1.
      endif
      if (phuacc(j) > .30 .and. phuacc(j) <= .55) fxg = 1.
      if (phuacc(j) > .55 .and. phuacc(j) <= .75) then
         fxg = 3.75 - 5. * phuacc(j)
      endif

      fxr = Min(1., fxw, fxn) * fxg
      fxr = Max(0., fxr)

      fixn = Min(6., fxr * uno3l)
      fixn = fixco * fixn + (1. - fixco) * uno3l
             !! if fixco=0 then fix the entire demand
      fixn = Min(fixn, uno3l)
      fixn = Min(nfixmx, fixn)

!! summary calculations
      if (curyr > nyskip) then
        wshd_fixn = wshd_fixn + fixn * hru_dafr(j)
      end if

      return
      end