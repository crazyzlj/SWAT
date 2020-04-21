      subroutine rthourly
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine routes hourly flow through the reach using a
!!    constant storage coefficient.     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)         |m             |average depth of main channel
!!    ch_k(2,:)       |mm/hr         |effective hydraulic conductivity of
!!                                   |main channel alluvium
!!    ch_l2(:)        |km            |length of main channel
!!    ch_n(2,:)       |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)       |m/m           |average slope of main channel
!!    ch_w(2,:)       |m             |average width of main channel
!!    chside(:)       |none          |change in horizontal distance per unit
!!                                   |change in vertical distance on channel
!!                                   |side slopes; always set to 2 (slope=1/2)
!!    evrch           |none          |Reach evaporation adjustment factor.
!!                                   |Evaporation from the reach is multiplied
!!                                   |by EVRCH. This variable was created to 
!!                                   |limit the evaporation predicted in arid 
!!                                   |regions.
!!    hhvaroute(2,:,:)|m^3 H2O       |water
!!    inum1           |none          |reach number
!!    inum2           |none          |inflow hydrograph storage location number
!!    pet_day         |mm H2O        |potential evapotranspiration
!!    phi(1,:)        |m^2           |cross-sectional area of flow in channel at
!!                                   |bankfull depth
!!    phi(6,:)        |m             |bottom width of main channel
!!    rchstor(:)      |m^3 H2O       |water stored in reach
!!    rnum1           |none          |fraction of overland flow
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hdepth(:)   |m             |depth of flow during hour
!!    hharea(:)   |m^2           |cross-sectional area of flow
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of hour
!!    hhtime(:)   |hr            |flow travel time for hour
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of hour
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during hour
!!    hsdti(:)    |m^3/s         |average flow rate during hour
!!    rchdep      |m             |depth of flow on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    det         |hr            |time step (1 hours)
!!    c           |none          |inverse of channel side slope
!!    hrtevp      |m^3 H2O       |evaporation losses for hour
!!    hrttlc      |m^3 H2O       |transmission losses for hour
!!    ii          |none          |counter (hour)
!!    inhyd       |none          |inflow hydrograph storage location number
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    rh          |m             |hydraulic radius
!!    scoef       |none          |storage coefficient
!!    topw        |m             |width of channel at water level
!!    vol         |m^3 H2O       |volume of water in reach
!!    wtrin       |m^3 H2O       |water entering reach during hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Min, Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven,
!!    Hydrology-Vrije Universiteit Brussel, Belgium
!!    Modified by Balaji Narasimhan
!!    Spatial Sciences Laboratory, Texas A&M University

      use parm

      integer :: jrch, ii, inhyd
      real :: wtrin, hrttlc, hrtevp, c, p, rh, scoef
      real :: vol, topw
	  real :: volrt, maxrt, adddep, addp, addarea
	  real :: hrttlc1, hrttlc2, hrtevp1, hrtevp2

      jrch = 0
      jrch = inum1
	  rttlc = 0.
	  rtevp = 0.

      inhyd = 0
      inhyd = inum2

!! hour loop
      do ii = 1, 24
        !! water entering reach during hour
        wtrin = 0.
        wtrin = hhvaroute(2,inhyd,ii) * (1. - rnum1)

        !! calculate volume of water in reach
        vol = 0.
        if (ii == 1) then
          hrchwtr(ii) = rchstor(jrch)
          vol = wtrin + rchstor(jrch)
        else
          hrchwtr(ii) = hhstor(ii-1)
          vol = wtrin + hhstor(ii-1)
        end if
        vol = Max(vol,1.e-4)

!! Find average flowrate (m^3/sec) in an hour
      volrt = vol / 3600

!! Find maximum flow capacity of the channel at bank full
      c = 0.
      c = chside(jrch)
	p = phi(6,jrch) + 2. * ch_d(jrch) * Sqrt(1. + c * c)
	rh = phi(1,jrch) / p
	maxrt = Qman(phi(1,jrch), rh, ch_n(2,jrch), ch_s(2,jrch))

      hsdti(ii) = 0.
	hdepth(ii) = 0.
	p = 0.
	rh = 0.
	vc = 0.

!! If average flowrate is greater than than the channel capacity at bank full
!! then simulate flood plain flow else simulate the regular channel flow
      if (volrt > maxrt) then
	  hharea(ii) = phi(1,jrch)
	  hdepth(ii) = ch_d(jrch)
	  p = phi(6,jrch) + 2. * ch_d(jrch) * Sqrt(1. + c * c)
	  rh = phi(1,jrch) / p
	  hsdti(ii) = maxrt
	  adddep = 0
	!! find the crossectional area and depth for volrt
	!! by iteration method at 1cm interval depth
	!! find the depth until the discharge rate is equal to volrt
	  Do While (hsdti(ii) < volrt)
          adddep = adddep + 0.01
          addarea = hharea(ii) + ((ch_w(2,jrch) * 5) + 4 *adddep)*adddep
          addp = p + (ch_w(2,jrch) * 4) + 2. * adddep * Sqrt(1. + 4 * 4)
	    rh = addarea / addp
          hsdti(ii) = Qman(addarea, rh, ch_n(2,jrch), ch_s(2,jrch))
	  end do
	  hharea(ii) = addarea
	  hdepth(ii) = ch_d(jrch) + adddep
	  p = addp
	  hsdti(ii) = volrt
	else
	!! find the crossectional area and depth for volrt
	!! by iteration method at 1cm interval depth
	!! find the depth until the discharge rate is equal to volrt
	  Do While (hsdti(ii) < volrt)
	    hdepth(ii) = hdepth(ii) + 0.01
	    hharea(ii) = (phi(6,jrch) + c * hdepth(ii)) * hdepth(ii)
	    p = phi(6,jrch) + 2. * hdepth(ii) * Sqrt(1. + c * c)
	    rh = hharea(ii) / p
          hsdti(ii) = Qman(hharea(ii), rh, ch_n(2,jrch), ch_s(2,jrch))
	  end do
	  hsdti(ii) = volrt
	end if

!! calculate top width of channel at water level
      topw = 0.
      if (hdepth(ii) <= ch_d(jrch)) then
        topw = phi(6,jrch) + 2. * hdepth(ii) * c
      else
        topw = 5 * ch_w(2,jrch) + 2. * (hdepth(ii) - ch_d(jrch)) * 4.
      end if
       
!!	Time step of simulation (in hour)
        det = 1.

        if (hsdti(ii) > 0.) then

          !! calculate travel time
          hhtime(ii) = ch_l2(jrch) * hharea(ii) / (3.6 * hsdti(ii))
          rttime = rttime + hhtime(ii)

          !! calculate volume of water leaving reach on day
          scoef = 0.
          scoef = 2. * det / (2. * rttime + det)
          if (scoef > 1.) scoef = 1.
          hrtwtr(ii) = scoef * vol

          !! set volume of water in channel at end of hour
          if (ii == 1) then
            hhstor(ii) = rchstor(jrch) + wtrin - hrtwtr(ii)
          else
            hhstor(ii) = hhstor(ii-1) + wtrin - hrtwtr(ii)
          end if

          if (hhstor(ii) < 0.) then
            hrtwtr(ii) = hrtwtr(ii) + hhstor(ii)
            hhstor(ii) = 0.
            if (hrtwtr(ii) < 0.) hrtwtr(ii) = 0.
          end if

!! transmission and evaporation losses are proportionally taken from the 
!! channel storage and from volume flowing out

       !! calculate transmission losses
	  hrttlc = 0.

	  if (hrtwtr(ii) > 0.) then

          hrttlc = ch_k(2,jrch) * ch_l2(jrch) * p * det

          hrttlc = Min(hrtwtr(ii),hrttlc)

	  hrttlc2 = hrttlc * hhstor(ii) / (hrtwtr(ii) + hhstor(ii))

	    if (hhstor(ii) <= hrttlc2) then
	      hrttlc2 = min(hrttlc2, hhstor(ii))
	      hhstor(ii) = hhstor(ii) - hrttlc2
	      hrttlc1 = hrttlc - hrttlc2
	      if (hrtwtr(ii) <= hrttlc1) then
	        hrttlc1 = min(hrttlc1, hrtwtr(ii))
	        hrtwtr(ii) = hrtwtr(ii) - hrttlc1
	      else
	        hrtwtr(ii) = hrtwtr(ii) - hrttlc1
	      end if
	    else
	      hhstor(ii) = hhstor(ii) - hrttlc2
	      hrttlc1 = hrttlc - hrttlc2
	      if (hrtwtr(ii) <= hrttlc1) then
	        hrttlc1 = min(hrttlc1, hrtwtr(ii))
	        hrtwtr(ii) = hrtwtr(ii) - hrttlc1
	      else
	        hrtwtr(ii) = hrtwtr(ii) - hrttlc1
	      end if
	    end if
	  hrttlc = hrttlc1 + hrttlc2
        end if
        rttlc = rttlc + hrttlc

        !! calculate evaporation
	  hrtevp = 0.
       if (hrtwtr(ii) > 0.) then

            aaa = evrch * pet_day * det / (24. * 1000.)

	    if (hdepth(ii) <= ch_d(jrch)) then
              hrtevp = aaa * ch_l2(jrch) * 1000. * topw
	    else
              if (aaa <=  (hdepth(ii) - ch_d(jrch))) then
              	hrtevp = aaa * ch_l2(jrch) * 1000. * topw
	      else
	        hrtevp = (hdepth(ii) - ch_d(jrch)) 
	        hrtevp = hrtevp + (aaa - (hdepth(ii) - ch_d(jrch))) 
              topw = phi(6,jrch) + 2. * ch_d(jrch) * c           
	        hrtevp = hrtevp * ch_l2(jrch) * 1000. * topw
	      end if
	    end if

	    hrtevp2 = hrtevp * hhstor(ii) / (hrtwtr(ii) + hhstor(ii))

	    if (hhstor(ii) <= hrtevp2) then
	      hrtevp2 = min(hrtevp2, hhstor(ii))
	      hhstor(ii) = hhstor(ii) - hrtevp2
	      hrtevp1 = hrtevp - hrtevp2
	      if (hrtwtr(ii) <= hrtevp1) then
	        hrtevp1 = min(hrtevp1, hrtwtr(ii))
	        hrtwtr(ii) = hrtwtr(ii) - hrtevp1
	      else
	        hrtwtr(ii) = hrtwtr(ii) - hrtevp1
	      end if
	    else
	      hhstor(ii) = hhstor(ii) - hrtevp2
	      hrtevp1 = hrtevp - hrtevp2
	      if (hrtwtr(ii) <= hrtevp1) then
	        hrtevp1 = min(hrtevp1, hrtwtr(ii))
	        hrtwtr(ii) = hrtwtr(ii) - hrtevp1
	      else
	        hrtwtr(ii) = hrtwtr(ii) - hrtevp1
	      end if
	    end if
	  hrtevp = hrtevp1 + hrtevp2
        end if
        rtevp = rtevp + hrtevp

        end if

      end do                     !! end hour loop

!! calculate amount of water in channel at end of day
      if (hhstor(24) < 10.) then
        hrtwtr(24) = hrtwtr(24) + hhstor(24)
        hhstor(24) = 0.
      end if
      if (hrtwtr(24) < 0.) hrtwtr(24) = 0.
      
!! daily average values
      !! set volume of water in reach at end of day
      rchstor(jrch) = hhstor(24)
      !! calculate total amount of water leaving reach
      rtwtr = Sum(hrtwtr)
      !! calculate average flow area
      rcharea = Sum (hharea) / 24.
      !! calculate average flow depth
      rchdep = Sum(hdepth) / 24.
      !! calculate average flow rate
      sdti = Sum(hsdti) / 24.
	  !! Calculate Velocity
 	  vel_chan(jrch) = sdti / rcharea


      return
      end
