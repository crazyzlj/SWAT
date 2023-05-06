      subroutine rtbact
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes bacteria through the stream network

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrchwtr(:)       |m^3 H2O     |water stored in reach at beginning of hour
!!    hhvaroute(2,:,:) |m^3 H2O     |water flowing into reach on day
!!    hhvaroute(18,:,:)|# cfu/100ml |persistent bacteria
!!    hhvaroute(19,:,:)|# cfu/100ml |less persistent bacteria
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    inum1            |none        |reach number
!!    inum2            |none        |inflow hydrograph storage location number
!!    rch_bactlp(:)    |# cfu/100ml |less persistent bacteria stored in reach
!!    rch_bactp(:)     |# cfu/100ml |persistent bacteria stored in reach
!!    rchwtr           |m^3 H2O     |water stored in reach at beginning of day
!!    rnum1            |none        |fraction of overland flow
!!    thbact           |none        |temperature adjustment factor for bacteria
!!                                  |die-off/growth
!!    tmpav(:)         |deg C       |average air temperature on current day
!!    varoute(2,:)     |m^3 H2O     |water flowing into reach on day
!!    varoute(18,:)    |# cfu/100ml |persistent bacteria
!!    varoute(19,:)    |# cfu/100ml |less persistent bacteria
!!    wdlprch          |1/day       |Die-off factor for less persistent bacteria
!!                                  |in streams
!!    wdprch           |1/day       |Die-off factor for persistent bacteria in 
!!                                  |streams
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hbactlp(:)   |# cfu/100mL  |less persistent bacteria in reach/outflow
!!                               |during hour
!!    hbactp(:)    |# cfu/100mL  |persistent bacteria in reach/outflow during
!!                               |hour
!!    rch_bactlp(:)|# cfu/100ml  |less persistent bacteria in reach/outflow
!!                               |at end of day
!!    rch_bactp(:) |# cfu/100ml  |persistent bacteria in reach/outflow at end
!!                               |of day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    initlp      |# cfu/100mL   |bacteria concentration in reach at beginning
!!                               |of hour (less persistent)
!!    initp       |# cfu/100mL   |bacteria concentration in reach at beginning
!!                               |of hour (persistent)
!!    jrch        |none          |reach number
!!    netwtr      |m^3 H2O       |net amount of water in reach during time step
!!    tday        |day           |routing time for the reach
!!    totbactlp   |10^4 cfu      |mass less persistent bacteria
!!    totbactp    |10^4 cfu      |mass persistent bacteria
!!    wtmp        |deg C         |temperature of water in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none

      integer :: ii, jrch
      real*8 :: totbactp, totbactlp, netwtr, initlp, initp
      real*8 :: tday

      jrch = 0
      jrch = inum1

      !! calculate temperature in stream
      !! Stefan and Preudhomme. 1993.  Stream temperature estimation
      !! from air temperature.  Water Res. Bull. p. 27-45
      !! SWAT manual equation 2.3.13
      call temparms
      !wtmp = 5.0 + 0.75 * tmpav(jrch)
      if (wtmp <= 0.) wtmp = 0.1

!     skipping hourly bacteria route for now  04/16/07 nubs
      if (ievent > 0) then                !! hourly mass balance
        initlp = 0.
        initp = 0.
        initlp = rch_bactlp(jrch)
        initp = rch_bactp(jrch)
        do ii = 1, nstep
          !! total bacteria mass in reach
          totbactp = 0.
          totbactlp = 0.
          totbactp = hhvaroute(18,inum2,ii) * hhvaroute(2,inum2,ii) *   
     &                                (1. - rnum1) + initp * hrchwtr(ii)
          totbactlp = hhvaroute(19,inum2,ii) * hhvaroute(2,inum2,ii) *  
     &                               (1. - rnum1) + initlp * hrchwtr(ii)

          !! compute bacteria die-off
          totbactp = totbactp * Exp(-Theta(wdprch / 24.,thbact,wtmp))
          totbactp = Max(0., totbactp)
          totbactlp = totbactlp * Exp(-Theta(wdlprch / 24.,thbact,wtmp))
          totbactlp = Max(0., totbactlp)

          !! new concentration
          netwtr = 0.
          netwtr = hhvaroute(2,inum2,ii) * (1. - rnum1) + hrchwtr(ii)
          if (netwtr >= 1.) then
            hbactp(ii) = totbactp / netwtr
            hbactlp(ii) = totbactlp / netwtr
          end if
          initlp = 0.
          initp = 0.
          initp = hbactp(ii)
          initlp = hbactlp(ii)
        end do
        if (totbactp < 1.e-6) totbactp = 0.0 
	  if (totbactlp < 1.e-6) totbactlp = 0.0
        if (netwtr >= 1.) then
          rch_bactp(jrch) = hbactp(nstep)
          rch_bactlp(jrch) = hbactlp(nstep)
        else
          rch_bactp(jrch) = 0.
          rch_bactlp(jrch) = 0.
        end if

      else

!! daily mass balance
      !! total bacteria mass in reach

      totbactp = 0.
      totbactlp = 0.
      totbactp = varoute(18,inum2) * varoute(2,inum2) * (1. - rnum1)    
     &                                        + rch_bactp(jrch) * rchwtr
      totbactlp = varoute(19,inum2) * varoute(2,inum2) *                
     &                          (1. - rnum1) + rch_bactlp(jrch) * rchwtr

      !! compute bacteria die-off
      !! calculate flow duration
      tday = 0.
      tday = rttime / 24.0
      if (tday > 1.0) tday = 1.0
      totbactp = totbactp * Exp(-Theta(wdprch,thbact,wtmp)*tday)
      totbactp = Max(0., totbactp)
      totbactlp = totbactlp * Exp(-Theta(wdlprch,thbact,wtmp)*tday) 
      totbactlp = Max(0., totbactlp)

      !! new concentration
      netwtr = 0.
      netwtr = varoute(2,inum2) * (1. - rnum1) + rchwtr
	
!!	!! change made by CS while running region 4; date 2 jan 2006	
	 if (totbactp < 1.e-6) totbactp = 0.0 
	 if (totbactlp < 1.e-6) totbactlp = 0.0
      if (netwtr >= 1.) then
        rch_bactp(jrch) = totbactp / netwtr
        rch_bactlp(jrch) = totbactlp / netwtr
      else
        rch_bactp(jrch) = 0.
        rch_bactlp(jrch) = 0.
      end if
      end if

      return
      end