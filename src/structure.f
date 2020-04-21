      subroutine structure

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine adjusts dissolved oxygen content for aeration at
!!    structures.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hhvaroute(:,:)|varies       |hourly routing storage array
!!    ievent       |none          |rainfall/runoff code
!!                                |0 daily rainfall/curve number technique
!!                                |1 daily rainfall/Green&Ampt technique/daily
!!                                |  routing
!!                                |2 sub-daily rainfall/Green&Ampt technique/
!!                                |  daily routing
!!                                |3 sub-daily rainfall/Green&Ampt/hourly
!!                                |  routing
!!    ihout        |none          |hydrograph storage location number for
!!                                |output
!!    mvaro        |none          |max number of variables routed through the
!!                                |reach
!!    rnum1        |none          |aeration coefficient
!!    varoute(:,:) |varies        |daily routing storage array
!!    varoute(1,:) |deg C         |water temperature
!!    varoute(2,:) |m^3 H2O       |water
!!    varoute(17,:)|kg O2         |dissolved oxygen
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hhvaroute(:,:)|varies      |hourly routing storage array
!!    soxy        |mg O2/L       |saturation concetration of dissolved oxygen
!!    varoute(:,:)|varies        |daily routing storage array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    disoxin     |mg O2/L       |dissolved oxygen concentration
!!    ii          |none          |counter
!!    jj          |none          |counter
!!    reak        |none          |aeration coefficient
!!    wtmp        |deg C         |water temperature
!!    ww          |varies        |variable to hold intermediate calculation
!!                               |result
!!    xx          |varies        |variable to hold intermediate calculation
!!                               |result
!!    yy          |varies        |variable to hold intermediate calculation
!!                               |result
!!    zz          |varies        |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
 
!!    subroutine developed by A. Van Griensven,
!!    Hydrology-Vrije Universiteit Brussel, Belgium

      use parm

      real :: reak, wtmp, ww, xx, yy, zz, disoxin
      integer :: ii, jj

!! initialize variables
      reak = 0.
      reak = rnum1
      if (reak <= 1.) reak = 1.
      soxy = 0.

!! daily array
        do ii = 1, mvaro
          varoute(ii,ihout) = 0.
          varoute(ii,ihout) = varoute(ii,inum1)
        end do
        if (varoute(2,inum1) > 0.001) then 
          wtmp = 0.
          wtmp = varoute(1,inum1)
          !! calculate saturation concentration for dissolved oxygen
          !! QUAL2E section 3.6.1 equation III-29
          disoxin = 0.
          ww = 0.
          xx = 0.
          yy = 0.
          zz = 0.
          ww = -139.34410 + (1.575701e05 / (wtmp + 273.15))
          xx = 6.642308e07 / ((wtmp + 273.15)**2)
          yy = 1.243800e10 / ((wtmp + 273.15)**3)
          zz = 8.621949e11 / ((wtmp + 273.15)**4)
          soxy = Exp(ww - xx + yy - zz)
          if (soxy < 0.) soxy = 0.
          disoxin = varoute(17,inum1) * 1000. / varoute(2,inum1)
          disoxin = soxy - ((soxy - disoxin) / reak)
          if (disoxin < 0.) disoxin = 0.
          varoute(17,ihout) = disoxin * varoute(2,inum1) / 1000.
        else
          varoute(17,ihout)=0.
        end if

!! subdaily array
      if (ievent > 2) then
        do ii = 1, 24
          do jj = 1, mvaro
            hhvaroute(jj,ihout,ii) = hhvaroute(jj,inum1,ii) 
          end do
          soxy = 0.
          if (hhvaroute(2,inum1,ii) > 0.0001) then 
            wtmp = 0.
            wtmp = hhvaroute(1,inum1,ii)
            !! calculate saturation concentration for dissolved oxygen
            !! QUAL2E section 3.6.1 equation III-29
            disoxin = 0.
            ww = 0.
            xx = 0.
            yy = 0.
            zz = 0.
            ww = -139.34410 + (1.575701e05 / (wtmp + 273.15))
            xx = 6.642308e07 / ((wtmp + 273.15)**2)
            yy = 1.243800e10 / ((wtmp + 273.15)**3)
            zz = 8.621949e11 / ((wtmp + 273.15)**4)
            soxy = Exp(ww - xx + yy - zz)
            if (soxy < 0.) soxy = 0.
            disoxin = hhvaroute(17,inum1,ii) * 1000. /                  &
     &                                             hhvaroute(2,inum1,ii)
            disoxin = soxy - ((soxy - disoxin) / reak)
            if (disoxin < 0.) disoxin = 0.
            hhvaroute(17,ihout,ii) = disoxin * hhvaroute(2,inum1,ii) /  &
     &                                                             1000.
          end if
        end do
      end if

      return
      end
