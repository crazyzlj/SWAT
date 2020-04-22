      subroutine grass_wway
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the grass waterways                      
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru            |none          |HRU number
!!    surfq(:)	   	|mm H2O        |amount of water in surface runoff generated
!!	grwat_n(:)      |none          |Mannings's n for grassed waterway
!!	grwat_i(:)      |none          |On/off Flag for waterway simulation
!!	grwat_l(:)      |km	           |Length of Grass Waterway
!!	grwat_w(:)      |none          |Width of grass waterway
!!	grwat_d(:)      |m             |Depth of Grassed waterway
!!	grwat_s(:)      |m/m           |Slope of grass waterway
!!	grwat_spcon(:)  |none          |sediment transport coefficant defined by user
!!	tc_gwat(:)      |none          |Time of concentration for Grassed waterway and its drainage area
!!	mhru
!!    sedyld(:)       |metric tons   |daily soil loss caused by water erosion

!!    wat_phi(1,:)        |m^2           |cross-sectional area of flow at bankfull
!!                                   |depth
!!    wat_phi(2,:)        |none          |
!!    wat_phi(3,:)        |none          |
!!    wat_phi(4,:)        |none          |
!!    wat_phi(5,:)        |m^3/s         |flow rate when reach is at bankfull depth
!!    wat_phi(6,:)        |m             |bottom width of main channel
!!    wat_phi(7,:)        |m             |depth of water when reach is at bankfull
!!                                   |depth
!!    wat_phi(8,:)        |m/s           |average velocity when reach is at 
!!                                   |bankfull depth
!!    wat_phi(9,:)        |m/s           |wave celerity when reach is at
!!                                   |bankfull depth
!!    wat_phi(10,:)       |hr            |storage time constant for reach at
!!                                   |bankfull depth (ratio of storage to
!!                                   |discharge)
!!    wat_phi(11,:)       |m/s           |average velocity when reach is at
!!                                   |0.1 bankfull depth (low flow)
!!    wat_phi(12,:)       |m/s           |wave celerity when reach is at
!!                                   |0.1 bankfull depth (low flow)
!!    wat_phi(13,:)       |hr            |storage time constant for reach at
!!                                   |0.1 bankfull depth (low flow) (ratio
!!                                   |of storage to discharge)
!!    sedyld(:)       |metric tons   |daily soil loss caused by water erosion
!!    surfq(:)        |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedyld(:)       |metric tons   |daily soil loss caused by water erosion
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    peakr       |m^3/s         |peak runoff rate for the day 
!!	chflow_m3   |m^3/s         |Runoff in CMS
!!	chflow_day	|m^3/day	   |Runoff
!!	K           |m^3/s         |Total number of HRUs plus this HRU number
!!	rcharea     |m^2           |cross-sectional area of flow
!!    rchdep      |m             |depth of flow on day
!!    sf_area     |m^2           |area of waterway sides in sheetflow
!!    sf_sed      |kg/m^2        |sediment loads on sides of waterway
!!    surq_remove |%             |percent of surface runoff capture in VFS
!!    sed_remove  |%             |percent of sediment capture in VFS
!!    vc          |m/s           |flow velocity in reach
!!	Sedin		|mg			   | Sediment in waterway 
!!	Sedint   	|mg			   | Sediment into waterway channel
!!	Sedout		|mg			   | Sediment out of waterway channel
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm
      real*8 :: chflow_m3, sf_area, surq_remove, sf_sed ,sed_remove,vc,
     & chflow_day, rh
!!	set variables
      j = ihru

                                
!!	do this only if there is surface runoff this day
      if (surfq(j) > 0.001) then

!!        compute channel peak rate using SCS triangular unit hydrograph
!!		Calculate average flow based on 3 hours of runoff
		chflow_day = 1000. * surfq(j) * hru_km(ihru)
          chflow_m3 = chflow_day/10800
          peakr = 2. * chflow_m3 / (1.5 * tc_gwat(j))

!! if peak rate is greater than bankfull discharge
          if (peakr > wat_phi(5,j)) then
            rcharea = wat_phi(1,j)
            rchdep = grwat_d(j)
          else
!!          find the crossectional area and depth for todays flow
!!          by iteration method at 1cm interval depth
!!          find the depth until the discharge rate is equal to volrt
            sdti = 0.
            rchdep = 0.

            Do While (sdti < peakr)
              rchdep = rchdep + 0.01
              rcharea = (wat_phi(6,j) + 8 * rchdep) * rchdep
              p = wat_phi(6,j) + 2. * rchdep * Sqrt(1. + 8 * 
     *           8)
              rh = rcharea / p
              sdti = Qman(rcharea, rh, grwat_n(j), grwat_s(j))
            end do
          end if

!!        Sediment yield (kg) from fraction of area drained by waterway

		  sedin = sedyld(ihru)
!! Calculate sediment losses in sheetflow at waterway sides

!! calculate area of sheeflow in m^2 assumne *:1 side slope 8.06 = (8^2+1^2)^.5
	sf_area = (grwat_d(j) - rchdep) * 8.06 * grwat_l(j) * 1000
!! Adjust Area to account for flow nonuniformities White and Arnold 2009 found half of flow in VFS
!!handled by 10% of VFS area. Waterways likely even more concentrated Assume only 20% of sideslope acts as filters
      if (sf_area > 1.e-6) then
	  sf_area = sf_area * 0.20 
!! calculate runoff depth over sheetflow area in mm
	  sf_depth=surfq(j)  * hru_km(ihru) * 1000000/sf_area
!! Calculate sediment load on sheetflow area kg/ha
	  sf_sed = sedin * 1000 / sf_area
!! Calculate runoff and sediment losses taken from mostly from filter.f
      end if

	if (sf_area > 0.) then 
!!		surq_remove = 75.8 - 10.8 * Log(sf_depth) + 25.9 
!!     &    * Log(sol_k(1,j))
	!! Simpler form derived from vfsmod simulations. r2 = 0.57 Publication pending white and arnold 2008

	    surq_remove = 95.6 - 10.79 * Log(sf_depth) 
		if (surq_remove > 100.) surq_remove = 100.
		if (surq_remove < 0.) surq_remove = 0.

		sed_remove = 79.0 - 1.04 * sf_sed + 0.213 * surq_remove 
		if (sed_remove > 100.) sed_remove = 100.
		if (sed_remove < 0.) sed_remove = 0.

	Else
		sed_remove = 0 
		surq_remove = 0
	endif
	sedint = sedin * (1. - sed_remove / 100.)

!!        calculate flow velocity
          vc = 0.001
          if (rcharea > 1.e-4) then
            vc = peakr / rcharea
            if (vc > wat_phi(9,j)) vc = wat_phi(9,j)
          end if

!!        compute deposition in the waterway
          cyin = 0.
          cych = 0.
          depnet = 0.
          deg = 0.
          dep = 0.
!! if there is significant flow calculate 
          if (chflow_m3 > 1.e-4) then
!! Calculate sediment concentration in inflow mg/m^3
            cyin = sedint / chflow_day
!! Calculate sediment transport capacity mg/m^3
            cych = grwat_spcon(j) * vc ** 1.5
!! Calculate deposition in mg
            depnet = chflow_day * (cyin - cych)
            if (depnet < 0.) depnet = 0
		  if (depnet > sedint) depnet = sedint
          endif
!! Calculate sediment out of waterway channel
	sedout = sedint - depnet

!! Calculate total fraction of sediment and surface runoff transported
      if (sedyld(j) < .0001) sedyld(j) = .0001
	sed_frac =  sedout/sedyld(j)


	surq_frac = 1 - surq_remove/100

!! Subtract reductions from sediment, nutrients, bacteria, and pesticides NOT SURFACE RUNOFF to protect water balance
      sedtrap = sedyld(j) * (1. - sed_frac)
	sedyld(j) = sedyld(j) * sed_frac 
	sedminpa(j) = sedminpa(j) * sed_frac
	sedminps(j) = sedminps(j) * sed_frac
	sedorgp(j) = sedorgp(j) * sed_frac
	surqsolp(j) = surqsolp(j) * surq_frac
	sedorgn(j) = sedorgn(j) * sed_frac
	surqno3(j) = surqno3(j) * surq_frac

        xrem = 0.
	  if (sedtrap <= lagyld(j)) then
	    lagyld(j) = lagyld(j) - sedtrap
	  else
	    xrem = sedtrap - lagyld(j)
	    lagyld(j) = 0.
	    if (xrem <= sanyld(j)) then
	      sanyld(j) = sanyld(j) - xrem
	    else
	      xrem = xrem - sanyld(j)
	      sanyld(j) = 0.
	      if (xrem <= sagyld(j)) then
	        sagyld(j) = sagyld(j) - xrem
	      else
	        xrem = xrem - sagyld(j)
	        sagyld(j) = 0.
	        if (xrem <= silyld(j)) then
	          silyld(j) = silyld(j) - xrem
	        else
	          xrem = xrem - silyld(j)
	          silyld(j) = 0.
	          if (xrem <= clayld(j)) then
	            clayld(j) = clayld(j) - xrem
	          else
	            xrem = xrem - clayld(j)
	            clayld(j) = 0.
	          end if
	        end if
	      end if
	    end if
	  end if
        sanyld(j) = Max(0., sanyld(j))
        silyld(j) = Max(0., silyld(j))
        clayld(j) = Max(0., clayld(j))
        sagyld(j) = Max(0., sagyld(j))
        lagyld(j) = Max(0., lagyld(j))

!! Calculate pesticide removal 
!! based on the sediment and runoff removal only
      if (hrupest(j) == 1) then
        do k = 1, npmx
          pst_surq(k,j) = pst_surq(k,j) * surq_frac
          pst_sed(k,j) = pst_sed(k,j) * (1. - sed_remove / 100.)
        end do
      end if
!! compute bacteria reductions
      bactrop = bactrop * surq_frac
      bactrolp = bactrolp * surq_frac
      bactsedp = bactsedp * sed_frac
      bactsedlp = bactsedlp * sed_frac

      Endif
      return
      end