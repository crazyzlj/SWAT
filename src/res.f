      subroutine res
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes water and sediment through reservoirs
!!    computes evaporation and seepage from the reservoir.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    br1(:)       |none          |1st shape parameter for reservoir surface
!!                                |area equation
!!    br2(:)       |none          |2nd shape parameter for reservoir surface
!!                                |area equation
!!    curyr        |none          |current year of simulation
!!    evrsv(:)     |none          |lake evaporation coefficient
!!    iflod1r(:)   |none          |beginning month of non-flood season
!!                                |(needed if IRESCO=2)
!!    iflod2r(:)   |none          |ending month of non-flood season
!!                                |(needed if IRESCO=2)
!!    inum1        |none          |reservoir number
!!    iresco(:)    |none          |outflow simulation code:
!!                                |0 compute outflow for uncontrolled reservoir
!!                                |  with average annual release rate
!!                                |1 measured monthly outflow
!!                                |2 simulated controlled outflow-target release
!!                                |3 measured daily outflow
!!                                |4 stage/volume/outflow relationship 
!!    i_mo         |none          |current month of simulation
!!    ndtargr(:)   |days          |number of days to reach target storage from
!!                                |current reservoir storage
!!                                |(needed if IRESCO=2)
!!    oflowmn(:,:) |m^3/day       |minimum daily ouflow for the month
!!    oflowmx(:,:) |m^3/day       |maximum daily ouflow for the month
!!    pet_day      |mm H2O        |potential evapotranspiration on day
!!    res_evol(:)  |m**3          |volume of water needed to fill the reservoir
!!                                |to the emergency spillway
!!    res_k(:)     |mm/hr         |hydraulic conductivity of the reservoir 
!!                                |bottom
!!    res_nsed(:)  |kg/L          |normal amount of sediment in reservoir
!!    res_pvol(:)  |m**3          |volume of water needed to fill the reservoir
!!                                |to the principal spillway 
!!    res_rr(:)    |m**3/day      |average daily principal spillway release
!!                                |volume
!!    res_sed(:)   |kg/L (ton/m^3)|amount of sediment in reservoir
!!    res_sub(:)   |none          |number of subbasin reservoir is in
!!    res_vol(:)   |m^3 H2O       |reservoir volume
!!    resflwi      |m^3 H2O       |water entering reservoir on day
!!    res_out(:,:,:)|m**3/day      |measured average daily outflow from the
!!                                |reservoir for the month
!!    ressedi      |metric tons   |sediment entering reservoir during time step
!!    sub_subp(:)  |mm H2O        |precipitation for day in subbasin
!!    sub_sumfc(:) |mm H2O        |amount of water in subbasin soil at field 
!!                                |capacity
!!    sub_sw(:)    |mm H2O        |amount of water in soil profile in subbasin
!!    starg(:,:)   |m**3          |monthly target reservoir storage
!!    wuresn(:,:)  |m**3          |average amount of water withdrawn from
!!                                |reservoir each month for consumptive water 
!!                                |use
!!    wurtnf(:)    |none          |fraction of water removed from the reservoir
!!                                |via WURESN which is returned and becomes flow
!!                                |from the reservoir outlet
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    res_sed(:)  |kg/L (ton/m^3)|amount of sediment in reservoir
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    resev       |m^3 H2O       |evaporation from reservoir on day
!!    resflwo     |m^3 H2O       |water leaving reservoir on day
!!    respcp      |m^3 H2O       |precipitation on reservoir for day
!!    ressa       |ha            |surface area of reservoir on day
!!    ressep      |m^3 H2O       |seepage from reservoir on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    flw         |m^3/s         |reservoir outflow for day
!!    jres        |none          |reservoir number
!!    sed         |kg/L          |concentration of sediment in reservoir at
!!                               |beginning of day
!!    targ        |m^3 H2O       |target reservoir volume for day
!!    vol         |m^3 H2O       |volume of water stored in reservoir at 
!!                               |beginning of day
!!    vvr         |m^3 H2O       |maximum controlled water release for day
!!    xx          |none          |variable to hold intermediate calculation 
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jres
      real :: vol, sed, vvr, targ, xx, flw
	real :: san,sil,cla,sag,lag,gra,ndespill
	real :: inised, finsed, setsed, remsetsed
 
      jres = 0
      jres = inum1

!! store initial values
      vol = 0.
      sed = 0.
	inised = 0.
	finsed = 0.
	setsed = 0.
	remsetsed = 0.

      vol = res_vol(jres)
      sed = res_sed(jres)

!!    Storage by particle sizes
      san = res_san(jres)
      sil = res_sil(jres)
      cla = res_cla(jres)
      sag = res_sag(jres)
      lag = res_lag(jres)
	gra = res_gra(jres)

!! calculate surface area for day
      ressa = br1(jres) * res_vol(jres) ** br2(jres)

!! calculate water balance for day
      resev = 10. * evrsv(jres) * pet_day * ressa
      ressep = res_k(jres) * ressa * 240.
      respcp = sub_subp(res_sub(jres)) * ressa * 10.

!! new water volume for day
      if (iresco(jres) /= 5) then 
       res_vol(jres) = res_vol(jres) + respcp + resflwi - resev - ressep
      endif
      
!! subtract consumptive water use from reservoir storage
        xx = 0.
        xx = wuresn(i_mo,jres)
        res_vol(jres) = res_vol(jres) - xx
        if (res_vol(jres) < 0.) then
          xx = xx + res_vol(jres)
          res_vol(jres) = 0.
        end if

!! if reservoir volume is greater than zero

        !! determine reservoir outflow
        select case (iresco(jres))
          case (0)                    !! uncontrolled reservoir
            vvr = 0.
            if (res_vol(jres) > res_pvol(jres)) then
              vvr = res_vol(jres) - res_pvol(jres)
              if (res_rr(jres) > vvr) then
                resflwo = resflwo + vvr
              else
                resflwo = resflwo + res_rr(jres)
              endif
            endif

          case (1)                   !! use measured monthly outflow
            resflwo = res_out(jres,i_mo,curyr)
          !! This will override the measured outflow! This is just a check 
          !! should really calibrate inflow or check res volumes

          case (2)                   !! controlled outflow-target release
            targ = 0.
            if (starg(i_mo,jres) > 0.) then
              targ = starg(i_mo,jres)
            else
              !! target storage based on flood season and soil water
              if (iflod2r(jres) > iflod1r(jres)) then
                if (i_mo > iflod1r(jres) .and. i_mo < iflod2r(jres))    
     &                                                              then
                  targ = res_evol(jres)
                else
                xx = Min(sub_sw(res_sub(jres))/sub_sumfc(res_sub(jres)),
     &                                                               1.)
                targ = res_pvol(jres) + .5 * (1. - xx) *                
     &                                 (res_evol(jres) - res_pvol(jres))
                end if
              else
                if (i_mo > iflod1r(jres) .or. i_mo < iflod2r(jres)) then
                  targ = res_evol(jres)
                else
                xx = Min(sub_sw(res_sub(jres))/sub_sumfc(res_sub(jres)),
     &                                                               1.)
                targ = res_pvol(jres) + .5 * (1. - xx) *                
     &                                 (res_evol(jres) - res_pvol(jres))
                end if
              end if
            endif
            if (res_vol(jres) > targ) then
              resflwo = (res_vol(jres) - targ) / ndtargr(jres)
            else
              resflwo = 0.
            end if

          case (3)                   !! use measured daily outflow
            flw = 0.
            read (350+jres,5000) flw
            resflwo = 86400. * flw
            
          case (4)
            targ = res_pvol(jres) * starg_fps(jres)
            if (res_vol(jres) > targ) then
              resflwo = (res_vol(jres) - targ) / ndtargr(jres)
            else
              resflwo = 0.
            end if
            if (resflwo < oflowmn_fps(jres)) resflwo = oflowmn_fps(jres)
            
          case (5)
            resflwo = 0.
            do jj = 1, nostep
              !! solve quadratic to find new depth
              !testing relationship res_vol(jres) = float(jj) * .1 * res_pvol(jres)
              x1 = bcoef(jres) ** 2 + 4. * ccoef(jres) * (1. - 
     &                                  res_vol(jres) / res_pvol(jres))
              if (x1 < 1.e-6) then
                res_h = 0.
              else
                res_h1 = (-bcoef(jres) - sqrt(x1)) / (2. * ccoef(jres))
                res_h = res_h1 + bcoef(jres)
              end if

              !! calculate water balance for timestep with new surface area
              ressa = res_psa(jres) * (1. + acoef(jres) * res_h)
              resev = 10. * evrsv(jres) * pet_day * ressa
              ressep = res_k(jres) * ressa * 240.
              respcp = sub_subp(res_sub(jres)) * ressa * 10.

              if(res_h <= 1.e-6) then
                res_qi = 0.
                res_h = 0.
              else
                res_qi = weirc(jres) * weirk(jres) * weirw(jres) * 
     &                                                    (res_h ** 1.5)
              end  if
              resflwo = resflwo + res_qi
              res_vol(jres) = res_vol(jres) + (respcp + resflwi - resev 
     &                                                - ressep) / nostep
              res_vol(jres) = res_vol(jres) - res_qi
            enddo

        end select
          
            ndespill = ndtargr(jres)
            if (ndespill <= 0.) ndespill = 10.
            if (res_vol(jres) > res_evol(jres)) then
              resflwo = resflwo+(res_vol(jres)-res_evol(jres))/ndespill
            endif

!! if reservoir volume is zero
      if (res_vol(jres) < 0.001) then

        !! if volume deficit in reservoir exists, reduce seepage so
        !! that reservoir volume is zero
        ressep = ressep + res_vol(jres)
        res_vol(jres) = 0.

        !! if seepage is less than volume deficit, take remainder
        !! from evaporation
        if (ressep < 0.) then
          resev = resev + ressep
          ressep = 0.
        end if
        res_sed(jres) = 0.

      else

        !! check calculated outflow against specified max and min values
        if (resflwo < oflowmn(i_mo,jres)) resflwo = oflowmn(i_mo,jres)
        if (resflwo > oflowmx(i_mo,jres) .and. oflowmx(i_mo,jres) > 0.) 
     &                                                              then
          resflwo = oflowmx(i_mo,jres)
        endif
           
        !! subtract outflow from reservoir storage
        if(iresco(jres) /= 5) then
          res_vol(jres) = res_vol(jres) - resflwo
          if (res_vol(jres) < 0.) then
             resflwo = resflwo + res_vol(jres)
             res_vol(jres) = 0.
          end if
        end if  

        !! add spillage from consumptive water use to reservoir outflow
        resflwo = resflwo + xx * wurtnf(jres)

        !! compute new sediment concentration in reservoir
	  if (ressedi < 1.e-6) ressedi = 0.0      !!nbs 02/05/07
	  if (ressa == 0.) ressa = 1.e-6     !! MJW added 040711
	  velofl = (resflwo / ressa) / 10000.  !!m3/d / ha * 10000. = m/d
!!	  velsetl = 1.35      !! for clay particle m/d
	  if (velofl > 1.e-6) then
	    trapres = velsetlr(jres) / velofl
	    if (trapres > 1.) trapres = 1.  !! set to nres
	    susp = 1. - trapres
	  else
	    susp = 0.
	  end if

	if (res_vol(jres) > 0.) then                         !!MJW added 040811
        res_sed(jres) = (ressedi * susp + sed * vol) / res_vol(jres)
        res_san(jres) = (ressani + san * vol) / res_vol(jres)
        res_sil(jres) = (ressili + sil * vol) / res_vol(jres)
        res_cla(jres) = (resclai + cla * vol) / res_vol(jres)
        res_sag(jres) = (ressagi + sag * vol) / res_vol(jres)
        res_lag(jres) = (reslagi + lag * vol) / res_vol(jres)
        res_gra(jres) = (resgrai + gra * vol) / res_vol(jres)

        res_sed(jres) = Max(1.e-6,res_sed(jres))
        res_san(jres) = Max(1.e-6,res_san(jres))
        res_sil(jres) = Max(1.e-6,res_sil(jres))
        res_cla(jres) = Max(1.e-6,res_cla(jres))
        res_sag(jres) = Max(1.e-6,res_sag(jres))
        res_lag(jres) = Max(1.e-6,res_lag(jres))
        res_gra(jres) = Max(1.e-6,res_gra(jres))
	else
        res_sed(jres) = 1.e-6             !!MJW added 040711
        res_san(jres) = 1.e-6
        res_sil(jres) = 1.e-6
        res_cla(jres) = 1.e-6
        res_sag(jres) = 1.e-6
        res_lag(jres) = 1.e-6
        res_gra(jres) = 1.e-6
	endif
        
        !! compute change in sediment concentration due to settling
        if (res_sed(jres) < 1.e-6) res_sed(jres) = 0.0    !!nbs 02/05/07
        if (res_sed(jres) > res_nsed(jres)) then
	    inised = res_sed(jres)
          res_sed(jres) = (res_sed(jres) - res_nsed(jres)) *            
     &                                   sed_stlr(jres) + res_nsed(jres)
	    finsed = res_sed(jres)
	    setsed = inised - finsed

        if (res_gra(jres) >= setsed) then
	    res_gra(jres) = res_gra(jres) - setsed
	    remsetsed = 0.
	  else
	    remsetsed = setsed - res_gra(jres)
          res_gra(jres) = 0.
		if (res_lag(jres) >= remsetsed) then
	      res_lag(jres) = res_lag(jres) - remsetsed
	      remsetsed = 0.
	    else
	      remsetsed = remsetsed - res_lag(jres)
	      res_lag(jres) = 0.
	      if (res_san(jres) >= remsetsed) then
	        res_san(jres) = res_san(jres) - remsetsed
	        remsetsed = 0.
	      else
	        remsetsed = remsetsed - res_san(jres)
	        res_san(jres) = 0.
              if (res_sag(jres) >= remsetsed) then
	          res_sag(jres) = res_sag(jres) - remsetsed
	          remsetsed = 0.
	        else
	          remsetsed = remsetsed - res_sag(jres)
	          res_sag(jres) = 0.
                if (res_sil(jres) >= remsetsed) then
  	            res_sil(jres) = res_sil(jres) - remsetsed
	            remsetsed = 0.
	          else
	            remsetsed = remsetsed - res_sil(jres)
	            res_sil(jres) = 0.
                  if (res_cla(jres) >= remsetsed) then
	              res_cla(jres) = res_cla(jres) - remsetsed
	              remsetsed = 0.
	            else
	              remsetsed = remsetsed - res_cla(jres)
	              res_cla(jres) = 0.
	            end if
                end if
	        end if
	      end if
	    end if
	  endif

        end if

        !! compute sediment leaving reservoir
        ressedo = res_sed(jres) * resflwo
        ressano = res_san(jres) * resflwo
        ressilo = res_sil(jres) * resflwo
        resclao = res_cla(jres) * resflwo
        ressago = res_sag(jres) * resflwo
        reslago = res_lag(jres) * resflwo
	    resgrao = res_gra(jres) * resflwo

        !! net change in amount of sediment in reservoir for day
        ressedc = vol * sed + ressedi - ressedo - res_sed(jres) *       
     &                                                     res_vol(jres)
!      write (130,5999) i, jres, res_sed(jres), sed_stlr(jres),          
!     & res_nsed(jres), ressedi, ressedo, resflwi, resflwo
!5999  format (2i4,7e12.4)
      
      end if

!!    update surface area for day
      ressa = br1(jres) * res_vol(jres) ** br2(jres)

      return
 5000 format (f8.2)
      end