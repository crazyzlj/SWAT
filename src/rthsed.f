      subroutine rthsed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes sediment from subbasin to basin outlets
!!    on an hourly timestep

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_cov1(:)  |none          |channel erodibility factor (0.0-1.0)
!!                               |0 non-erosive channel
!!                               |1 no resistance to erosion
!!    ch_cov2(:)   |none         |channel bed cover factor (0.0-1.0)
!!                               |0 channel is completely protected from
!!                               |  erosion by cover
!!                               |1 no vegetative cover on channel
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_di(:)    |m             |initial depth of main channel
!!    ch_li(:)    |km            |initial length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_si(:)    |m/m           |initial slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    ch_wdr(:)   |m/m           |channel width to depth ratio
!!    hdepth(:)   |m             |depth of flow on day
!!    ideg        |none          |channel degredation code
!!                               |0: do not compute channel degradation
!!                               |1: compute channel degredation (downcutting
!!                               |   and widening)
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    prf         |none          |Peak rate adjustment factor for sediment
!!                               |routing in the channel. Allows impact of
!!                               |peak flow rate on sediment routing and
!!                               |channel reshaping to be taken into account
!!    rnum1       |none          |fraction of overland flow
!!    sdti        |m^3/s         |average flow on day in reach
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    spcon       |none          |linear parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    spexp       |none          |exponent parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    varoute(3,:)|metric tons   |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)     |m             |average depth of main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    peakr       |m^3/s         |peak runoff rate in channel
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    sedrch      |metric tons   |sediment transported out of channel
!!                               |during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dat2        |m             |change in channel depth during time step
!!    deg         |metric tons   |sediment reentrained in water by channel
!!                               |degradation
!!    dep         |metric tons   |sediment deposited on river bottom
!!    depdeg      |m             |depth of degradation/deposition from original
!!    depnet      |metric tons   |
!!    dot         |
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    qdin        |m^3 H2O       |water in reach during time step
!!    vc          |m/s           |flow velocity in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: ttcoef

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch, ii
      real :: qdin, sedin, vc, cyin, cych, depnet, deg, dep
      real :: depdeg, dot, thbase, deg24, dep24, deg1, deg2

      jrch = 0
      jrch = inum1
	deg24 = 0.
	dep24 = 0.

      do ii = 1, 24
        if (hrtwtr(ii) > 0. .and. hdepth(ii) > 0.) then

         !! initialize water in reach during time step
         qdin = 0.
         qdin = hrtwtr(ii) + hhstor(ii)

         !! do not perform sediment routing if no water in reach
         if (qdin > 0.01) then

           !! initialize sediment in reach during time step
           sedin = 0.
           if (ii == 1) then
             sedin = hhvaroute(3,inum2,ii) * (1. - rnum1) + sedst(jrch)
           else
             sedin = hhvaroute(3,inum2,ii) * (1. - rnum1) + hsedst(ii-1)
           end if

          if (sedin < 1.e-6) sedin = 0.
		          
          !! initialize reach peak runoff rate
           peakr = prf * hsdti(ii)

           !! calculate peak flow velocity
           vc = 0.
           if (hharea(ii) < .010) then
             vc = 0.01
           else
             vc = peakr / hharea(ii)
           end if
           if (vc > 5.) vc = 5.

           thbase = 0.
           thbase = ch_l2(jrch) * 1000. / (3600. * 24. * vc)
           if (thbase > 1.) thbase = 1.

           !! JIMMY'S NEW IMPROVED METHOD for sediment transport
           cyin = 0.
           cych = 0.
           depnet = 0.
           deg = 0.
	       deg1 = 0.
	       deg2 = 0.
           dep = 0.
           cyin = sedin / qdin
           cych = spcon * vc ** spexp
           depnet = qdin * (cych - cyin)
	     if(abs(depnet) < 1.e-6) depnet = 0.

!!  tbase is multiplied so that erosion is proportional to the traveltime, 
!!  which is directly related to the length of the channel
!!  Otherwise for the same discharge rate and sediment deficit
!!  the model will erode more sediment per unit length of channel 
!!  from a small channel than a larger channel. Modification made by Balaji Narasimhan
           if (depnet > 1.e-6) then
             deg = depnet * thbase
	       !! First the deposited material will be degraded before channel bed
	       if (deg >= depch(jrch)) then
	         deg1 = depch(jrch)
               deg2 = (deg - deg1) * ch_cov1(jrch) * ch_cov2(jrch)
	         depch(jrch) = 0.
	       else
	         deg1 = deg
	         deg2 = 0.
	         depch(jrch) = depch(jrch) - deg1
	       endif
             dep = 0.
           else
             dep = -depnet * thbase
             deg = 0.
	         deg1 = 0.
	         deg2 = 0.
           endif

  	     hsedyld(ii) = sedin + deg1 + deg2 - dep
           if (hsedyld(ii) < 1.e-6) hsedyld(ii) = 0.

	     d_fract = hrtwtr(ii) / qdin
	     if (d_fract > 1.) d_fract = 1.

           hsedyld(ii) = hsedyld(ii) * d_fract

!!  In this default sediment routing sediment is not tracked by particle size
           rch_san = 0.
	     rch_sil = rch_sil + hsedyld(ii)  !!All are assumed to silt type particles
           rch_cla = 0.
           rch_sag = 0.
           rch_lag = 0.
	     rch_gra = 0.
     
           hsedst(ii) = sedin + deg1 + deg2 - dep - hsedyld(ii) 
           if (hsedst(ii) < 1.e-6) hsedst(ii) = 0.
	     depch(jrch) = depch(jrch) + dep
           sedst(jrch) = hsedst(ii)

	     deg24 = deg24 + deg2
	     dep24 = dep24 + dep
	   else
	     hsedst(ii) = sedin
           sedst(jrch) = hsedst(ii)
         end if
        end if
      end do

!!    Bank erosion
      rchdy(55,jrch) = 0.
!!    Channel Degredation
      rchdy(56,jrch) = deg24
!!    Channel Deposition
      rchdy(57,jrch) = dep24
!!    Floodplain Deposition
      rchdy(58,jrch) = 0.
!!    Total suspended sediments
	rchdy(59,jrch) = 0.

!!    Organic nitrogen and Organic Phosphorus contribution from channel erosion
!!    ch_orgn(jrch) = deg24 * ch_onco(jrch) * 1000.
!!    ch_orgp(jrch) = deg24 * ch_opco(jrch) * 1000.
      
      ch_orgn(jrch) = deg24 * ch_onco(jrch) / 1000.
      ch_orgp(jrch) = deg24 * ch_opco(jrch) / 1000.


      qdin = 0.
      qdin = rtwtr + rchstor(jrch)

      if ((rtwtr == 0. .and. rchdep == 0.) .or. qdin <= 0.01) then
	  sedrch = 0.
	  rch_san = 0.
	  rch_sil = 0.
	  rch_cla = 0.
	  rch_sag = 0.
	  rch_lag = 0.
	  rch_gra = 0.
!!    Bank erosion
      rchdy(55,jrch) = 0.
!!    Channel Degredation
      rchdy(56,jrch) = 0.
!!    Channel Deposition
      rchdy(57,jrch) = 0.
!!    Floodplain Deposition
      rchdy(58,jrch) = 0.
!!    Total suspended sediments
	rchdy(59,jrch) = 0.
	end if

      !! compute changes in channel dimensions
      if ((rtwtr > 0. .and. rchdep > 0.) .or. qdin > 0.01) then
        if (ideg == 1) then
          qdin = 0.
          qdin = rtwtr + rchstor(jrch)
          depdeg = 0.
          depdeg = ch_d(jrch) - ch_di(jrch)
        if (depdeg < ch_si(jrch) * ch_li(jrch) * 1000.) then
          if (qdin > 1400000.) then
            dot = 0.
            dot = 358.6 * rchdep * ch_s(2,jrch) * ch_cov1(jrch)
            dat2 = 1.
            dat2 =  dat2 * dot
            ch_d(jrch) = ch_d(jrch) + dat2
            ch_w(2,jrch) = ch_wdr(jrch) * ch_d(jrch)
            ch_s(2,jrch) = ch_s(2,jrch) - dat2 / (ch_l2(jrch) * 1000.)
            ch_s(2,jrch) = Max(.0001, ch_s(2,jrch))
            call ttcoef(jrch)
          endif
        endif
        endif
	endif

      return
      end

