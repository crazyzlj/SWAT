      subroutine reshr
      
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
!!    iflod1r(:)   |none          |beginning month of non-flood season
!!                                |(needed if IRESCO=2)
!!    iflod2r(:)   |none          |ending month of non-flood season
!!                                |(needed if IRESCO=2)
!!    inum1        |none          |reservoir number
!!    inum2        |none          |inflow hydrograph location number
!!    iresco(:)    |none          |outflow simulation code:
!!                                |0 compute outflow for uncontrolled reservoir
!!                                |  with average annual release rate
!!                                |1 measured monthly outflow
!!                                |2 simulated controlled outflow-target release
!!                                |3 measured daily outflow
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
!!    sub_subp_dt(:,:)  |mm H2O   |precipitation for time step in subbasin
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
!!    inhyd       |none          |inflow hydrograph location number
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

      integer :: jres, inhyd
      real :: vol, sed, vvr, targ, xx, flw
 
      jres = 0
      jres = inum1
	inhyd = inum2

!! store initial values
      flw = 0.
      vol = 0.
      sed = 0.
      vol = res_vol(jres)
      sed = res_sed(jres)
      hhresflwi = 0.
      hhresflwo = 0.
      hhressedi = 0.
      hhressedo = 0.

!! start sub-daily simulation
      do k=1,nstep

!! update inflow to reservoir
	  hhresflwi(k) = hhvaroute(2,inhyd,k)
	  hhressedi(k) = hhvaroute(3,inhyd,k)

!! calculate surface area for day (ha)
        ressa = br1(jres) * res_vol(jres) ** br2(jres)

!! calculate water balance for day
        resev = 6. * pet_day * ressa / nstep		!! urban modeling by J.Jeong
        ressep = res_k(jres) * ressa * 240./ nstep	!! urban modeling by J.Jeong
        respcp = sub_subp_dt(res_sub(jres),k) * ressa * 10.	!! urban modeling by J.Jeong

!! new water volume for day
        res_vol(jres) = res_vol(jres) + respcp + hhresflwi(k) - 
     &                   resev - ressep

!! if reservoir volume is zero
        if (res_vol(jres) < 0.001) then

        !! if volume deficit in reservoir exists, reduce seepage so that reservoir volume is zero
          ressep = ressep + res_vol(jres)
          res_vol(jres) = 0.

        !! if seepage is less than volume deficit, take remainder from evaporation
          if (ressep < 0.) then
            resev = resev + ressep
            ressep = 0.
          end if
          res_sed(jres) = 0.

        else
!! if reservoir volume is greater than zero

        !! compute new sediment concentration in reservoir
          res_sed(jres) = (hhressedi(k) + sed * vol) / res_vol(jres)
          res_sed(jres) = Max(0.,res_sed(jres))

        !! determine reservoir outflow
          select case (iresco(jres))
            case (0)                    !! uncontrolled reservoir
              vvr = 0.
              if (res_vol(jres) > res_pvol(jres)) then
                vvr = res_vol(jres) - res_pvol(jres)
                if (res_vol(jres) > res_evol(jres)) then
                  hhresflwo(k) = (res_vol(jres) - res_evol(jres)) 
                  vvr = res_evol(jres) - res_pvol(jres)
                endif
                if (res_rr(jres) > vvr) then
                  hhresflwo(k) = (hhresflwo(k) + vvr) / nstep !m3
                else
                  hhresflwo(k) = (hhresflwo(k) + res_rr(jres)) / nstep !m3
                endif
              endif

            case (1)                   !! use measured monthly outflow
              hhresflwo(k) = res_out(jres,i_mo,curyr) * 86400. / nstep !m3/s -> m3

            case (2)                   !! controlled outflow-target release
              targ = 0.
              if (starg(i_mo,jres) > 0.) then
                targ = starg(i_mo,jres)
              else
              !! target storage based on flood season and soil water
                if (iflod2r(jres) > iflod1r(jres)) then
                  if (i_mo > iflod1r(jres) .and. i_mo < iflod2r(jres)) 
     &            then
                    targ = res_evol(jres)
                  else
                    xx = Min(sub_sw(res_sub(jres)) / 
     &               sub_sumfc(res_sub(jres)),1.)
                    targ = res_pvol(jres) + .5 * (1. - xx) * 
     &               (res_evol(jres) - res_pvol(jres))
                  end if
                else
                  if (i_mo > iflod1r(jres) .or. i_mo < iflod2r(jres)) 
     &             then
                    targ = res_evol(jres)
                  else
                    xx = Min(sub_sw(res_sub(jres)) / 
     &               sub_sumfc(res_sub(jres)),1.)
                    targ = res_pvol(jres) + .5 * (1. - xx) * 
     &               (res_evol(jres) - res_pvol(jres))
                  end if
                end if
              endif
              if (res_vol(jres) > targ) then
                hhresflwo(k) = (res_vol(jres) - targ) / ndtargr(jres) /
     &                         nstep
              else
                hhresflwo(k) = 0.
              end if

            case (3)                   !! use measured daily outflow
              if (k==1) read (350+jres,5000) flw
  		      hhresflwo(k) = 86400. * flw / nstep		!! m3, urban modeling by J.Jeong
          end select

        !! check calculated outflow against specified max and min values
          if (hhresflwo(k)<oflowmn(i_mo,jres)*86400./nstep) then
            resflwo = oflowmn(i_mo,jres) * 86400. / nstep
          end if
          if (hhresflwo(k)>oflowmx(i_mo,jres)*86400./nstep.and. 
     &         oflowmx(i_mo,jres)>0.) then
             hhresflwo(k) = oflowmx(i_mo,jres) * 86400. / nstep
          endif
           
        !! subtract outflow from reservoir storage
          res_vol(jres) = res_vol(jres) - hhresflwo(k)
          if (res_vol(jres) < 0.) then
            hhresflwo(k) = hhresflwo(k) + res_vol(jres)
            res_vol(jres) = 0.
          end if

        !! subtract consumptive water use from reservoir storage
          xx = 0.
          xx = wuresn(i_mo,jres) / nstep		!! urban modeling by J.Jeong
          res_vol(jres) = res_vol(jres) - xx
          if (res_vol(jres) < 0.) then
            xx = xx + res_vol(jres)
            res_vol(jres) = 0.
          end if
        !! add spillage from consumptive water use to reservoir outflow
          hhresflwo(k) = hhresflwo(k) + xx * wurtnf(jres)

        !! compute change in sediment concentration due to settling
          if (res_sed(jres) > res_nsed(jres)) then
            res_sed(jres) = (res_sed(jres) - res_nsed(jres)) * 
     &        sed_stlr(jres) + res_nsed(jres)
          end if

        !! compute sediment leaving reservoir
          hhressedo(k) = res_sed(jres) * hhresflwo(k)

        !! net change in amount of sediment in reservoir for day
          ressedc = vol * sed + hhressedi(k) - hhressedo(k) 
     &     - res_sed(jres) * res_vol(jres)

        end if
      end do
!!    update surface area for day
      ressa = br1(jres) * res_vol(jres) ** br2(jres)
!!    daily total amount
      resflwi = sum(hhresflwi)
      resflwo = sum(hhresflwo)
      ressedi = sum(hhressedi)
      ressedo = sum(hhressedo)
      return
 5000 format (f8.2)
      end