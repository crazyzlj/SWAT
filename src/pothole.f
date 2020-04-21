      subroutine pothole
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates depressional areas that do not drain to the 
!!    stream network (potholes) and impounded areas such as rice paddies

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
      integer :: j, ly
      real :: potsep, sumo, potev, cnv, potpcp, spillo
      real :: sedloss, no3loss, yy, dg, excess, stmax, sedsetl
	real :: sanloss, silloss, claloss, sagloss, lagloss
      real :: potmm

      j = 0
      j = ihru

!! initialize variables
      potev = 0.
	spillo = 0.  !!Testing for Aziz
      potpcp = 0.
      potsep = 0.
      sumo = 0.

!! conversion factors
      cnv = 0.
      cnv = 10. * hru_ha(j)

!! when water is impounding
      if (imp_trig(nro(j),nrelease(j),ipot(j)) == 0) then

        !! update volume of water in pothole
       if (ipot(j) == ihru) then
         pot_vol(j) = pot_vol(j) + precipday * cnv
         potflwi(j) = potflwi(j) + precipday * cnv
       else
          pot_vol(ipot(j)) = pot_vol(ipot(j)) + qday * Abs(pot_fr(j)) * &
     &       cnv
          potflwi(ipot(j)) = potflwi(ipot(j)) + qday * Abs(pot_fr(j)) * &
     &       cnv
          qday = qday * (1. - Abs(pot_fr(j)))

        !! update sediment in pothole  
          pot_sed(ipot(j)) = pot_sed(ipot(j)) + sedyld(j)*Abs(pot_fr(j))
          potsedi(ipot(j)) = potsedi(ipot(j)) + sedyld(j)*Abs(pot_fr(j))

        pot_san(ipot(j)) = pot_san(ipot(j)) + sanyld(j) * Abs(pot_fr(j)) 
        potsani(ipot(j)) = potsani(ipot(j)) + sanyld(j) * Abs(pot_fr(j))
        pot_sil(ipot(j)) = pot_sil(ipot(j)) + silyld(j) * Abs(pot_fr(j)) 
        potsili(ipot(j)) = potsili(ipot(j)) + silyld(j) * Abs(pot_fr(j))
        pot_cla(ipot(j)) = pot_cla(ipot(j)) + clayld(j) * Abs(pot_fr(j)) 
        potclai(ipot(j)) = potclai(ipot(j)) + clayld(j) * Abs(pot_fr(j))
        pot_sag(ipot(j)) = pot_sag(ipot(j)) + sagyld(j) * Abs(pot_fr(j)) 
        potsagi(ipot(j)) = potsagi(ipot(j)) + sagyld(j) * Abs(pot_fr(j))
        pot_lag(ipot(j)) = pot_lag(ipot(j)) + lagyld(j) * Abs(pot_fr(j)) 
        potlagi(ipot(j)) = potlagi(ipot(j)) + lagyld(j) * Abs(pot_fr(j))

          sedyld(j) = sedyld(j) * (1. - Abs(pot_fr(j)))

          sanyld(j) = sanyld(j) * (1. - Abs(pot_fr(j)))
          silyld(j) = silyld(j) * (1. - Abs(pot_fr(j)))
          clayld(j) = clayld(j) * (1. - Abs(pot_fr(j)))
          sagyld(j) = sagyld(j) * (1. - Abs(pot_fr(j)))
          lagyld(j) = lagyld(j) * (1. - Abs(pot_fr(j)))

        !! update nitrate in pothole
          pot_no3(ipot(j)) = pot_no3(ipot(j)) + surqno3(j) *              &
     &       Abs(pot_fr(j)) * hru_ha(j)
          surqno3(j) = surqno3(j) * (1. - Abs(pot_fr(j)))
       end if


        !! adjust water balance if rice or pothole in HRU
        !! i.e. if ipot(j) == j

        if (pot_vol(j) > 0.) then

          !! compute surface area assuming a cone shape (m^2)
          potsa(j) = pi * (3. * pot_vol(j) / (pi * hru_slp(j)))**.66666
          potsa(j) = potsa(j) / 10000.                    !convert to ha
          potsa(j) = Min(potsa(j), hru_ha(j))

          !! compute rainfall on impounded water and add to current volume
        !! this appears to be already accounted for in code above (Jens)
        !! potpcp = subp(j) * potsa(j) * 10.
        !! pot_vol(j) = pot_vol(j) + potpcp

          !! check if max volume is exceeded
          if (pot_vol(j) > pot_volx(j)) then
            spillo = 0.
            sedloss = 0.
            no3loss = 0.

            sanloss = 0.
            silloss = 0.
            claloss = 0.
            sagloss = 0.
            alaglos = 0.

            spillo = pot_vol(j) - pot_volx(j)
            sumo = sumo + spillo
            pot_vol(j) = pot_volx(j)
            qday = qday + spillo / cnv

            sedloss = pot_sed(j) *  spillo / pot_vol(j)
            sedloss = Min(sedloss, pot_sed(j))
            pot_sed(j) = pot_sed(j) - sedloss
            potsedo = potsedo + sedloss
            sedyld(j) = sedyld(j) + sedloss

            no3loss = pot_no3(j) *  spillo / pot_vol(j)
            no3loss = Min(no3loss, pot_no3(j))
            pot_no3(j) = pot_no3(j) - no3loss
            surqno3(j) = surqno3(j) + no3loss / hru_ha(j)

            sanloss = pot_san(j) *  spillo / pot_vol(j)
            sanloss = Min(sanloss, pot_san(j))
            pot_san(j) = pot_san(j) - sanloss
            potsano = potsano + sanloss
            sanyld(j) = sanyld(j) + sanloss

            silloss = pot_sil(j) *  spillo / pot_vol(j)
            silloss = Min(silloss, pot_sil(j))
            pot_sil(j) = pot_sil(j) - silloss
            potsilo = potsilo + silloss
            silyld(j) = silyld(j) + silloss

            claloss = pot_cla(j) *  spillo / pot_vol(j)
            claloss = Min(claloss, pot_cla(j))
            pot_cla(j) = pot_cla(j) - claloss
            potclao = potclao + claloss
            clayld(j) = clayld(j) + claloss

            sagloss = pot_sag(j) *  spillo / pot_vol(j)
            sagloss = Min(sagloss, pot_sag(j))
            pot_sag(j) = pot_sag(j) - sagloss
            potsago = potsago + sagloss
            sagyld(j) = sagyld(j) + sagloss

            alaglos = pot_lag(j) *  spillo / pot_vol(j)
		  alaglos = Min(alaglos, pot_lag(j))
            pot_lag(j) = pot_lag(j) - alaglos
            potlago = potlago + alaglos
            lagyld(j) = lagyld(j) + alaglos

          endif

          !! limit seepage into soil if profile is near field capacity
          yy = 0.
          if (sol_sw(j) / sol_sumfc(j) < .5) then
            yy = 1.
          elseif (sol_sw(j) / sol_sumfc(j) < 1.) then
            yy = 1. - sol_sw(j) / sol_sumfc(j)
          endif

          !! calculate seepage into soil
          potsep = yy * sol_k(1,j) * potsa(j) * 240.
       !!   potsep = sol_k(1,j) * hru_ha(j) * 240. !!Testing for Aziz
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
            excess = sol_st(ly,j) - stmax
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
            potev = (1. - laiday(j) / evlai) * pet_day
            potev = 10. * potev * potsa(j)          !!units mm => m^3
         !!   potev = 10. * pet_day * hru_ha(j)  !!Testing for Aziz
            potev = Min(potev, pot_vol(j))
            pot_vol(j) = pot_vol(j) - potev
          endif

!        write (125,2000)i,j,pot_vol(j),potsa(j),spillo,potsep,potev,   &
!    &       sol_sw(j)
!2000      format (2i4,6f10.2)

          !! Check date for release/impounding water on rice fields
          if (iida == irelease(nro(j),nrelease(j),j) .and.              &
     &                        imp_trig(nro(j),nrelease(j),j) == 1) then
            qday = qday + pot_vol(j) / cnv
            sumo = sumo + pot_vol(j)
            pot_vol(j) = 0.
            potsedo = potsedo + pot_sed(j)
            sedyld(j) = sedyld(j) + pot_sed(j)
            pot_sed(j) = 0.
            surqno3(j) = surqno3(j) + pot_no3(j) / hru_ha(j)
            pot_no3(j) = 0.

            potsano = potsano + pot_san(j)
            sanyld(j) = sanyld(j) + pot_san(j)
            pot_san(j) = 0.

            potsilo = potsilo + pot_sil(j)
            silyld(j) = silyld(j) + pot_sil(j)
            pot_sil(j) = 0.

            potclao = potclao + pot_cla(j)
            clayld(j) = clayld(j) + pot_cla(j)
            pot_cla(j) = 0.

            potsago = potsago + pot_sag(j)
            sagyld(j) = sagyld(j) + pot_sag(j)
            pot_sag(j) = 0.

            potlago = potlago + pot_lag(j)
            lagyld(j) = lagyld(j) + pot_lag(j)
            pot_lag(j) = 0.


          else
            tileo = Min(pot_tile(j), pot_vol(j))
            sumo = sumo + tileo
            pot_vol(j) = pot_vol(j) - tileo
            qday = qday + tileo / cnv
            if (pot_vol(j) > 1.) then
              sedloss = 0.
              no3loss = 0.

              sanloss = 0.
              silloss = 0.
              claloss = 0.
              sagloss = 0.
              lagloss = 0.

              sedloss = pot_sed(j) *  tileo / pot_vol(j)
              pot_sed(j) = pot_sed(j) - sedloss
              potsedo = potsedo + sedloss
              sedyld(j) = sedyld(j) + sedloss
              no3loss = pot_no3(j) *  tileo / pot_vol(j)
              pot_no3(j) = pot_no3(j) - no3loss
              surqno3(j) = surqno3(j) + no3loss / hru_ha(j)

              sanloss = pot_san(j) *  tileo / pot_vol(j)
              pot_san(j) = pot_san(j) - sanloss
              potsano = potsano + sanloss
              sanyld(j) = sanyld(j) + sanloss

              silloss = pot_sil(j) *  tileo / pot_vol(j)
              pot_sil(j) = pot_sil(j) - silloss
              potsilo = potsilo + silloss
              silyld(j) = silyld(j) + silloss

              claloss = pot_cla(j) *  tileo / pot_vol(j)
              pot_cla(j) = pot_cla(j) - claloss
              potclao = potclao + claloss
              clayld(j) = clayld(j) + claloss

              sagloss = pot_sag(j) *  tileo / pot_vol(j)
              pot_sag(j) = pot_sag(j) - sagloss
              potsago = potsago + sagloss
              sagyld(j) = sagyld(j) + sagloss

              lagloss = pot_lag(j) *  tileo / pot_vol(j)
              pot_lag(j) = pot_lag(j) - lagloss
              potlago = potlago + lagloss
              lagyld(j) = lagyld(j) + lagloss

            else
              sedyld(j) = 0.
              surqno3(j) = 0.

              sanyld(j) = 0.
              silyld(j) = 0.
              clayld(j) = 0.
              sagyld(j) = 0.
              lagyld(j) = 0.

            endif
          endif

          !! calculate settling in the pothole
          sedsetl = 0.
          remsetl = 0.
          sedsetl = (pot_sed(j) - (pot_nsed(j) * pot_vol(j) / 1.e6)) *  &
     &       sed_stl(j)
          sedsetl = Min(sedsetl, pot_sed(j))
          pot_sed(j) = pot_sed(j) - sedsetl
          pot_sed(j) = Max(1.e-6, pot_sed(j))
          pot_no3(j) = pot_no3(j) * (1. - pot_no3l(j))


	      if (sedsetl <= pot_lag(j)) then
	        pot_lag(j) = pot_lag(j) - sedsetl
	      else
	        remsetl = sedsetl - pot_lag(j)
	        pot_lag(j) = 0.
	        if (remsetl <= pot_san(j)) then
	          pot_san(j) = pot_san(j) - remsetl
	        else
	          remsetl = remsetl - pot_san(j)
	          pot_san(j) = 0.
	          if (remsetl <= pot_sag(j)) then
	            pot_sag(j) = pot_sag(j) - remsetl
	          else
	            remsetl = remsetl - pot_sag(j)
	            pot_san(j) = 0.
	            if (remsetl <= pot_sil(j)) then
	              pot_sil(j) = pot_sil(j) - remsetl
	            else
	              remsetl = remsetl - pot_sil(j)
	              pot_sil(j) = 0.
	              if (remsetl <= pot_cla(j)) then
	                pot_cla(j) = pot_cla(j) - remsetl
	              else
	                remsetl = remsetl - pot_cla(j)
	                pot_cla(j) = 0.
	              end if
	            end if
	          end if
	        end if
	      end if
          pot_san(j) = Max(0., pot_san(j))
          pot_sil(j) = Max(0., pot_sil(j))
          pot_cla(j) = Max(0., pot_cla(j))
          pot_sag(j) = Max(0., pot_sag(j))
          pot_lag(j) = Max(0., pot_lag(j))

        endif
      endif


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
1000  format (1x,i4,2x,9(f8.2,2x))
      end
