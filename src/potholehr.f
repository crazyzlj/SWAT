      subroutine potholehr()
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates depressional areas that do not drain to the 
!!    stream network (potholes) and impounded areas such as rice paddies at sub-daily time step

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr          |none          |current year of simulation
!!    evlai          |none          |leaf area index at which no evaporation
!!                                  |occurs from the water surface. This
!!                                  |variable is used in ponded HRUs (eg rice)
!!                                  |where evaporation from the water surface
!!                                  |is restricted by the plant canopy cover.
!!                                  |Evaporation from the water surface equals
!!                                  |potential ET when LAI = 0 an decreases
!!                                  |linearly to O when LAI = EVLAI
!!    hru_dafr(:)    |none          |fraction of watershed area in HRU
!!    hru_ha(:)      |ha            |area of HRU in hectares
!!    hru_slp(:)     |m/m           |average slope steepness
!!    iida           |julian date   |day being simulated (current julian date)
!!    ihru           |none          |HRU number
!!    ipot(:)        |none          |number of HRU (in subbasin) that is ponding
!!                                  |water--the HRU that the surface runoff from
!!                                  |current HRU drains into. This variable is
!!                                  |used only for rice paddys or closed
!!                                  |depressional areas
!!    imp_trig(:,:,:)|none          |release/impound action code:
!!                                  |0 begin impounding water
!!                                  |1 release impounded water
!!    irelease(:,:,:)|julian date   |date of impound/release operation
!!    laiday(:)      |none          |leaf area index
!!    nrelease(:)    |none          |sequence number of impound/release
!!                                  |operation within the year
!!    nro(:)         |none          |sequence number of year in rotation
!!    nyskip         |none          |number of years to skip output 
!!                                  |summarization/printing
!!    pet_day        |mm H2O        |potential evapotranspiration on current day
!!                                  |in HRU
!!    pot_fr(:)      |km2/km2       |fraction of HRU area that drains into 
!!                                  |pothole
!!    pot_no3(:)     |kg N          |amount of nitrate in pothole water body
!!    pot_no3l(:)    |1/day         |nitrate decay rate in impounded area
!!    pot_nsed(:)    |mg/L          |normal sediment concentration in impounded
!!                                  |water (needed only if current HRU is IPOT)
!!    pot_sed(:)     |metric tons   |amount of sediment in pothole water body
!!    pot_tile(:)    |m3/d          |average daily outflow to main channel from
!!                                  |tile flow if drainage tiles are installed
!!                                  |in pothole (needed only if current HRU is 
!!                                  |IPOT)
!!    pot_vol(:)     |m**3 H2O      |current volume of water stored in the
!!                                  |depression/impounded area
!!    pot_volx(:)    |m**3 H2O      |maximum volume of water stored in the
!!                                  |depression/impounded area
!!    qday           |mm H2O        |surface runoff loading to main channel from
!!                                  |HRU for day
!!    rainsub(:,:)	 |mm H2O        |precipitation for the time step during the
!!									|day in HRU
!!    sed_stl(:)     |kg/kg         |fraction of sediment remaining suspended in
!!                                  |impoundment after settling for one day
!!    sedyld(:)      |metric tons   |daily soil loss caused by water erosion 
!!                                  |in HRU
!!    sol_k(:,:)     |mm/hr         |saturated hydraulic conductivity of soil
!!                                  |layer
!!    sol_nly(:)     |none          |number of layers in soil profile
!!    sol_por(:,:)   |none          |total porosity of soil layer expressed as
!!                                  |a fraction of the total volume
!!    sol_st(:,:)    |mm H2O        |amount of water stored in the soil layer
!!                                  |on any given day
!!    sol_sumfc(:)   |mm H2O        |amount of water held in the soil profile
!!                                  |at field capacity
!!    sol_sw(:)      |mm H2O        |amount of water stored in soil profile on
!!                                  |current day
!!    sol_z(:,:)     |mm            |depth to bottom of soil layer
!!    spadyev        |mm H2O        |average annual amount of water removed
!!                                  |from potholes by evaporation in watershed
!!    spadyo         |mm H2O        |average annual amount of water released to 
!!                                  |main channel from potholes in watershed
!!    spadyrfv       |mm H2O        |average annual amount of precipitation on
!!                                  |potholes in watershed
!!    spadysp        |mm H2O        |average annual amount of water removed
!!                                  |from potholes by seepage in watershed
!!    subp(:)        |mm H2O        |precipitation for the day in HRU
!!    surqno3(:)     |kg N/ha       |amount of NO3-N in surface runoff in HRU
!!                                  |for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pot_no3(:)     |kg N          |amount of nitrate in pothole water body
!!    pot_sed(:)     |metric tons   |amount of sediment in pothole water body
!!    pot_vol(:)     |m**3 H2O      |current volume of water stored in the
!!                                  |depression/impounded area
!!    potevmm        |mm H2O        |volume of water evaporated from pothole
!!                                  |expressed as depth over HRU
!!    potflwi(:)     |m^3 H2O       |water entering pothole on day
!!    potflwo        |mm H2O        |discharge from pothole expressed as depth
!!                                  |over HRU
!!    potpcpmm       |mm H2O        |precipitation falling on pothole water body
!!                                  |expressed as depth over HRU
!!    potsa(:)       |ha            |surface area of impounded water body
!!    potsedi(:)     |metric tons   |sediment entering pothole on day
!!    potsedo        |metric tons   |sediment leaving pothole on day
!!    potsepmm       |mm H2O        |seepage from pothole expressed as depth over
!!                                  |HRU
!!    qday           |mm H2O        |surface runoff loading to main channel from
!!                                  |HRU for day
!!    sedyld(:)      |metric tons   |daily soil loss caused by water erosion 
!!                                  |in HRU
!!    sol_st(:,:)    |mm H2O        |amount of water stored in the soil layer
!!                                  |on any given day
!!    sol_sw(:)      |mm H2O        |amount of water stored in soil profile on
!!                                  |current day
!!    spadyev        |mm H2O        |average annual amount of water removed 
!!                                  |from potholes by evaporation in watershed
!!    spadyo         |mm H2O        |average annual amount of water released to 
!!                                  |main channel from potholes in watershed
!!    spadyrfv       |mm H2O        |average annual amount of precipitation on
!!                                  |potholes in watershed
!!    spadysp        |mm H2O        |average annual amount of water removed
!!                                  |from potholes by seepage in watershed
!!    surqno3(:)     |kg N/ha       |amount of NO3-N in surface runoff in HRU
!!                                  |for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    dg          |mm            |depth of soil layer
!!    excess      |mm H2O        |amount of water moving into soil that exceeds
!!                               |storage of layer
!!    j           |none          |HRU number
!!    ly          |none          |counter (soil layers)
!!    no3loss     |kg N          |amount of nitrate lost from water body
!!    pi          |none          |pi
!!    potev       |m^3 H2O       |evaporation from impouned water body
!!    potmm       |mm H2O        |volume of water in pothole expressed as depth
!!                               |over HRU
!!    potpcp      |m^3 H2O       |precipitation falling on water body
!!    potsep      |m^3 H2O       |seepage from impounded water body
!!    sedloss     |metric tons   |amount of sediment lost from water body
!!    sedsetl     |metric tons   |amount of sediment settling out of water
!!                               |during day
!!    spillo      |m^3 H2O       |amount of water released to main channel from
!!                               |impounded water body due to spill-over
!!    stmax       |mm H2O        |maximum water storage in soil layer
!!    sumo        |m^3 H2O       |sum of all releases from water body on
!!                               |current day
!!    tileo       |m^3 H2O       |amount of water released to the main channel
!!                               |from the water body by drainage tiles
!!    yy          |none          |fraction of maximum seepage allowed to occur
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs, Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

	  use parm

      real, parameter :: pi = 3.1416
      integer :: j, ly,kk,ll,k
      real :: potsep, sumo, potev, cnv, potpcp, spillo
      real :: sedloss, no3loss, yy, dg, excess, stmax, sedsetl
      real :: potmm

      j = 0
      j = ihru

!! initialize variables
      potev = 0.
      potpcp = 0.
      potsep = 0.
      sumo = 0.

!! conversion factors
      cnv = 0.
      cnv = 10. * hru_ha(j)

!! iterate for time step calculation using 1hr rainfall values, hhsubp(mhru,24)
	  do k=1,nstep

!! when water is impounding
        if (imp_trig(j) == 0) then

        !! update volume of water in pothole
          pot_vol(ipot(j)) = pot_vol(ipot(j)) + hhqday(k) * 
     &      Abs(pot_fr(j)) * cnv
          potflwi(ipot(j)) = potflwi(ipot(j)) + hhqday(k) * 
     &      Abs(pot_fr(j)) * cnv
          hhqday(k) = hhqday(k) * (1. - Abs(pot_fr(j)))

        !! update sediment in pothole  
          pot_sed(ipot(j)) = pot_sed(ipot(j)) + hhsedy(j,k) * 
     &      Abs(pot_fr(j)) 
          potsedi(ipot(j)) = potsedi(ipot(j)) + hhsedy(j,k) * 
     &      Abs(pot_fr(j))
          hhsedy(j,k) = hhsedy(j,k) * (1. - Abs(pot_fr(j)))				!! urban modeling by J.Jeong

        !! update nitrate in pothole
          pot_no3(ipot(j)) = pot_no3(ipot(j)) + surqno3(j) * 
     &      Abs(pot_fr(j)) * hru_ha(j)
          surqno3(j) = surqno3(j) * (1. - Abs(pot_fr(j)))


        !! adjust water balance if rice or pothole in HRU
        !! i.e. if ipot(j) == j

          if (pot_vol(j) > 0.) then

          !! compute surface area assuming a cone shape (m^2)
            potsa(j) = pi * (3. * pot_vol(j) / 
     &       (pi * hru_slp(j)))**.66666
            potsa(j) = potsa(j) / 10000.                    !convert to ha
            potsa(j) = Min(potsa(j), hru_ha(j))

          !! compute rainfall on impounded water and add to current volume
            potpcp = rainsub(j,k) * potsa(j) * 10.							!! urban modeling by J.Jeong
            pot_vol(j) = pot_vol(j) + potpcp

          !! check if max volume is exceeded
            if (pot_vol(j) > pot_volx(j)) then
              spillo = 0.
              sedloss = 0.
              no3loss = 0.
              spillo = pot_vol(j) - pot_volx(j)
              sumo = sumo + spillo
              pot_vol(j) = pot_volx(j)
              hhqday(k) = hhqday(k) + spillo / cnv		!! urban modeling by J.Jeong

              sedloss = pot_sed(j) *  spillo / pot_vol(j)
              sedloss = Min(sedloss, pot_sed(j))
              pot_sed(j) = pot_sed(j) - sedloss
              potsedo = potsedo + sedloss
              hhsedy(j,k) = hhsedy(j,k) + sedloss		!! urban modeling by J.Jeong

              no3loss = pot_no3(j) *  spillo / pot_vol(j)
              no3loss = Min(no3loss, pot_no3(j))
              pot_no3(j) = pot_no3(j) - no3loss
              surqno3(j) = surqno3(j) + no3loss / hru_ha(j)
            endif

          !! limit seepage into soil if profile is near field capacity
            yy = 0.
            if (sol_sw(j) / sol_sumfc(j) < .5) then
              yy = 1.
            elseif (sol_sw(j) / sol_sumfc(j) < 1.) then
              yy = 1. - sol_sw(j) / sol_sumfc(j)
            endif

          !! calculate seepage into soil
            potsep = yy * sol_k(1,j) * potsa(j) * 240./ nstep	!! urban modeling by J.Jeong
            potsep = Min(potsep, pot_vol(j))
            pot_vol(j) = pot_vol(j) - potsep
            sol_st(1,j) = sol_st(1,j) + potsep / cnv

          !! redistribute water so that no layer exceeds maximum storage
            do ly = 1, sol_nly(j)
              dg = 0.
              stmax = 0.
              excess = 0.
              if (ly == 1) then
                dg = sol_z(ly,j)
              else
                dg = sol_z(ly,j) - sol_z(ly-1,j)
              end if
              stmax = sol_por(ly,j) * dg
              if (sol_st(ly,j) <= stmax) exit
              excess = stmax - sol_st(ly,j)
              sol_st(ly,j) = stmax
              if (ly + 1 <= sol_nly(j)) then
                sol_st(ly+1,j) = sol_st(ly+1,j) + excess
              end if
            end do

          !! recompute total soil water
            sol_sw(j) = 0.
            do ly = 1, sol_nly(j)
              sol_sw(j) = sol_sw(j) + sol_st(ly,j)
            end do

          !! compute evaporation from water surface
            if (laiday(j) < evlai) then
              potev = (1. - laiday(j) / evlai) * pet_day / nstep	!! urban modeling by J.Jeong
              potev = 5. * potev * potsa(j)             !!units mm => m^3
              potev = Min(potev, pot_vol(j))
              pot_vol(j) = pot_vol(j) - potev
            endif

          !! Check date for release/impounding water on rice fields
            if (imp_trig(j) == 1) then
              hhqday(k) = hhqday(k) + pot_vol(j) / cnv
              sumo = sumo + pot_vol(j)
              pot_vol(j) = 0.
              potsedo = potsedo + pot_sed(j)
              hhsedy(j,k) = hhsedy(j,k) + pot_sed(j)
              pot_sed(j) = 0.
              surqno3(j) = surqno3(j) + pot_no3(j) / hru_ha(j)
              pot_no3(j) = 0.
            else
              tileo = Min(pot_tile(j),pot_vol(j))
              sumo = sumo + tileo
              pot_vol(j) = pot_vol(j) - tileo
              hhqday(k) = hhqday(k) + tileo / cnv
              if (pot_vol(j) > 1.) then
                sedloss = 0.
                no3loss = 0.
                sedloss = pot_sed(j) *  tileo / pot_vol(j)
                pot_sed(j) = pot_sed(j) - sedloss
                potsedo = potsedo + sedloss
                hhsedy(j,k) = hhsedy(j,k) + sedloss
                no3loss = pot_no3(j) *  tileo / pot_vol(j)
                pot_no3(j) = pot_no3(j) - no3loss
                surqno3(j) = surqno3(j) + no3loss / hru_ha(j)
              else
                hhsedy(j,k) = 0.
                surqno3(j) = 0.
              endif
            endif

          !! calculate settling in the pothole
            sedsetl = 0.
            sedsetl = (pot_sed(j) - (pot_nsed(j) * pot_vol(j) / 1.e6)) 
     &         * sed_stl(j) / nstep  !! urban modeling by J.Jeong
            sedsetl = Min(sedsetl, pot_sed(j))
            pot_sed(j) = pot_sed(j) - sedsetl
            pot_sed(j) = Max(0., pot_sed(j))
            pot_no3(j) = pot_no3(j) * (1. - pot_no3l(j))
          endif
        endif
	    if(k==nstep) exit
	  end do

        potpcpmm = potpcp / cnv
        potevmm = potev / cnv
        potsepmm = potsep / cnv
        potflwo = sumo / cnv

!! summary calculations
        if (curyr > nyskip) then
          potmm = 0.
          potmm = pot_vol(j) / cnv

          spadyo = spadyo + potflwo * hru_dafr(j)
          spadyev = spadyev + potevmm * hru_dafr(j)
          spadysp = spadysp + potsepmm * hru_dafr(j)
          spadyrfv = spadyrfv + potpcpmm * hru_dafr(j)
        end if

        return
1000    format (1x,i4,2x,9(f8.2,2x))
        end
