      subroutine rthsed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes sediment from subbasin to basin outlets
!!    on a sub-daily timestep

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
!!    ch_d50      |mm            |median particle diameter of main channel
!!    ch_li(:)    |km            |initial length of main channel
!!    ch_n(2,:)   |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)   |m/m           |average slope of main channel
!!    ch_si(:)    |m/m           |initial slope of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    ch_wdr(:)   |m/m           |channel width to depth ratio
!!    hdepth(:)   |m             |depth of flow on day
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of time step
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during time step
!!    hsdti(:)    |m^3/s         |average flow rate during time step
!!    ideg        |none          |channel degredation code
!!                               |0: do not compute channel degradation
!!                               |1: compute channel degredation (downcutting and widening)
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    phi(5,:)    |m^3/s         |flow rate when reach is at bankfull depth
!!    prf(:)      |none          |Reach peak rate adjustment factor for sediment routing in the channel. Allows impact of
!!                               |peak flow rate on sediment routing and channel reshaping to be taken into account
!!    rhy(:)      |m H2O         |main channel hydraulic radius
!!    rnum1       |none          |fraction of overland flow
!!    sdti        |m^3/s         |average flow on day in reach
!!    sedst(:)    |metric tons   |amount of sediment stored in reach
!!    sig_g       |none          |geometric standard deviation of particle sizes for the main channel
!!    spcon       |none          |linear parameter for calculating sediment reentrained in channel sediment routing
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
!!    sedrch      |metric tons   |sediment transported out of channel on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dat2        |m             |change in channel depth during time step
!!    deg         |metric tons   |sediment reentrained in water by channel degradation
!!    dep         |metric tons   |sediment deposited on river bottom
!!    depdeg      |m             |depth of degradation/deposition from original
!!    depnet      |metric tons   |
!!    dot         |
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    qin         |m^3 H2O       |water in reach during time step
!!    vc          |m/s           |flow velocity in reach
!!    sedcon      |mg/L          |sediment concentration
!!    shrstrss    |none          |critical shear stress for bed erosion
!!    Reynolds_g  |none          |grain Reynolds number 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: ttcoef

!!    code modified by J.Jeong and N.Kannan for urban sub-hourly sediment modeling
!!    and by Balagi for bank erosion.
!!    Brownlie (1981) bed load model and Yang (1973, 1984) model added. 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

	use parm

	integer :: jrch, ii
	real :: qin, qdin, sedin, vc, cyin, cych, depnet, deg, dep
	real :: depdeg, dot, ycoeff, Reynolds_g, visco_h2o, tmpw
	real :: channel_d50, particle_specific_gravity, Fr_g, Fr_gc
	real :: log10sedcon, sedcon, deg24, dep24
	real :: vfall, coefa, coefb, coefc, coefd, coefe
      
      real :: thbase,  shear_stress, vshear, deg1, deg2, d_fract, dat2

	jrch = 0; deg24=0.; dep24=0
	jrch = inum1
	channel_d50 = ch_d50 / 1000. !! unit change mm->m
	particle_specific_gravity = 2.65
	sedin = 0.

	do ii = 1, nstep

	if (hrtwtr(ii)>0. .and. hdepth(ii)>0.) then

	 !! initialize water in reach during time step
	 qin = 0.
       sedin = 0.
	 qin = hrtwtr(ii) + hhstor(ii)

	 !! do not perform sediment routing if no water in reach
	 if (qin > 0.01) then

	   !! initialize sediment in reach during time step
	   if (ii == 1) then
           sedin = hhvaroute(3,inum2,ii) * (1. - rnum1) + sedst(jrch)
	   else
		 sedin = hhvaroute(3,inum2,ii) * (1. - rnum1) + hsedst(ii-1)
	   end if

       if (sedin < 1.e-6) sedin = 0.
       !! initialize reach peak runoff rate
       !!  peakr = prf(ii) * hsdti(ii)      commented by nbs 082714
       peakr = prf(jrch) * hsdti(ii)

       !! calculate flow velocity
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
       dep = 0.
       if (sedin < 1e-6) sedin = 0.
	   cyin = sedin / qin !tons/m3

	   !!water temperature (Celsius)
	   tmpw = sub_hhwtmp(jrch,ii) 

	   !! water viscosity (m2/s) using 3rd order polynomial interpolation
	   visco_h2o = -3.e-6 * tmpw ** 3 + 0.0006 * tmpw ** 2 - 
     &	   0.0469 * tmpw + 1.7517		
	   visco_h2o = visco_h2o * 1.e-6
   
	   !! Use either Brownlie or Yang Model for bead load calculation
	   select case (sed_ch)
	     case (0)
		   !! Bagnold's (1977) stream power
           cych = spcon(jrch) * vc ** spexp(jrch)
		 case (1)
		   !!Brownlie Model 
		   !! grain Reynolds number
		   Reynolds_g = sqrt(9.81 * channel_d50 ** 3) / visco_h2o
 
		   !!critical shear stress for grain Froude number
		   ycoeff = (sqrt(particle_specific_gravity - 1.) * 
     & 	              Reynolds_g) ** (-0.6)
		   shear_stress = 0.22 * ycoeff + 0.06 * 10 ** (-7.7 * ycoeff)

		   !! critical grain Froude number
		   fr_gc = 4.596 * shear_stress ** 0.5293 * ch_s(2,jrch) ** (-0.1405) 
     &               * sig_g ** (-0.1606)

		   !! grain Froude number
		   fr_g = vc / sqrt((particle_specific_gravity - 1.) * 
     & 		   9.81 * (ch_d50 / 1000.))

		   !! sediment concentration at the channel outlet [ppm, or g/m3]
		   if(fr_g>fr_gc) then
		     sedcon = 7115 * 1.268 * (fr_g - fr_gc) ** 1.978 * 
     &   	 ch_s(2,jrch) ** 0.6601 * (rhy(ii) / channel_d50) ** (-0.3301)
		   else
		     sedcon = 0.
		   endif
		   cych = sedcon * 1.e-6 !tons/m3		 
		
		 case (2)
		   !!Yang Model
		   !! particle fall velocity
		   vfall = 9.81 * channel_d50 ** 2 * (particle_specific_gravity - 1.)
     &		    / (18.* visco_h2o)
		   
		   !! shear velocity
		   vshear = sqrt(9.81 * rhy(ii) * ch_s(2,jrch))

		   coefa = vfall * channel_d50 / visco_h2o
		   coefe = vshear * channel_d50 / visco_h2o
		 
		   if(coefe<70) then
		     if (coefe<1.2) coefe = 1.2
			 coefb = 2.5 / (log10(coefe) - 0.06) + 0.66
		   elseif(coefe>=70) then
		     coefb = 2.05
		   else
		     write(*,*) 'Error in implementing Yang erosion model'
!!		     stop
		   endif

		   coefc = vshear / vfall
		   coefd = vc * ch_s(2,jrch) / vfall - coefb * ch_s(2,jrch)
 		   if(coefd<=0) coefd = 1.e-6
		   
		   if(ch_d50<=2.0) then ! in millimeter 
		     !! use sand equation (1973)
		     log10sedcon = 5.435 - 0.286 * log10(coefa) - 0.457 * 
     &           log10(coefc) +(1.799 - 0.409 *log10(coefa) - 0.314 *
     &           log10(coefc)) * log10(coefd)

		   elseif(ch_d50>2.0) then 
		     !! use gravel equation (1984)
		     log10sedcon = 6.681 - 0.633 * log10(coefa) - 4.816 * 
     &           log10(coefc) +(2.784 - 0.305 *log10(coefa) - 0.282 * 
     &           log10(coefc)) * log10(coefd)
		   endif
		   sedcon = 10 ** log10sedcon !ppm
   		   cych = sedcon * 1.e-6 !tons/m3		 
	   end select

	   depnet = qin * (cych - cyin)
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
           if (hsedyld(ii) < 1.e-12) hsedyld(ii) = 0.



	     d_fract = hrtwtr(ii) / qin
	     if (d_fract > 1.) d_fract = 1.

           hsedyld(ii) = hsedyld(ii) * d_fract

!!  In this default sediment routing sediment is not tracked by particle size
           rch_san = 0.
           rch_sil = rch_sil + hsedyld(ii) !All are assumed to silt part
           rch_cla = 0.
           rch_sag = 0.
           rch_lag = 0.
	     rch_gra = 0.
     
           hsedst(ii) = sedin + deg1 + deg2 - dep - hsedyld(ii) 
           if (hsedst(ii) < 1.e-12) hsedst(ii) = 0.
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
      ch_orgn(jrch) = deg24 * ch_onco(jrch) * 1000.
      ch_orgp(jrch) = deg24 * ch_opco(jrch) * 1000.



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