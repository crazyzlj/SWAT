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
!!    pot_vol(:)     |mm            |current volume of water stored in the
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
!!    pot_solp(:)    |1/d           | soluble P loss rate in the pothole (.01 - 0.5)
!!    pot_orgn(:)     |kg N         |amount of organic N in pothole water body
!!    pot_orgp(:)     |             |amount of organic P in pothole water body
!!    pot_mpa(:)     |kg N          |amount of active mineral pool P in pothole water body
!!    pot_mps(:)     |kg N          |amount of stable mineral pool P in pothole water body
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
!!    potev       |m^3 H2O       |evaporation from impounded water body
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
!!    Intrinsic: abs, Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real*8, parameter :: pi = 3.1416
      integer :: j, ly
      real*8 :: potsep, sumo, potev, cnv, potpcp, no3in, qdayi
      real*8 :: sedloss, no3loss, yy, dg, excess, stmax, sedsetl
	real*8 :: sanloss, silloss, claloss, sagloss, lagloss
      real*8 :: potmm,minpsloss,minpaloss, solploss, orgnloss, orgploss, rto

      j = 0
      j = ihru

!! initialize variables
      tileo = 0.
      potev = 0.
      spillo = 0.  
      potpcp = 0.
      potsep = 0.
      sumo = 0.
      potpcpmm = 0.
      potevmm = 0.
      potsepmm = 0.
      potflwo = 0.
      potflwosp = 0.
      potsedo = 0.
      potsano = 0.
      potsilo = 0.
      potclao = 0.
      potsago = 0.
      potlago = 0.
      potno3o = 0.
      potsolpo = 0.
      potorgno = 0.
      potorgpo = 0.
      potmpso = 0.
      potmpao = 0.
      potvol_ini = 0.
      potsa_ini = 0.
      sedloss = 0.
      no3loss = 0.
      solploss = 0.
      orgnloss = 0.
      orgploss = 0.
      minpsloss = 0.
      minpaloss = 0.

      qin = qday * pot_fr(j)   !inflow = surface flow
      qdayi = qday
      qday = qday * (1. - pot_fr(j))
      potloss = qdayi - qday
      qdr(j) = qdr(j) - potloss
      no3in = surqno3(j)   !+ latno3(j) + gwno3(j) - don't include groundwater no3
      
!!    conversion factors
      cnv = 10. * hru_ha(j)
      rto = 1.

!     when water is impounding
      if (imp_trig(j) == 1) return
        
!       update volume of water in pothole
!       pot_fr is now the fraction of the hru draining into the pothole
!       the remainder (1-pot_fr) goes directly to runoff
        pot_vol(j) = pot_vol(j) + qin
        potflwi(j) = potflwi(j) + qin

!       compute surface area assuming a cone shape (m^2)
        potvol_m3 = pot_vol(j) * cnv
        potsa(j) = pi * (3. * potvol_m3 / (pi * hru_slp(j)))**.6666
        potsa(j) = potsa(j) / 10000.                  !convert to ha  
        if (potsa(j) <= 0.000001) then
          potsa(j) = 0.001
        endif
        if (potsa(j) > hru_ha(j)) then
          potsa(j) = hru_ha(j)
        endif
        potvol_ini = pot_vol(j)
        potsa_ini = potsa(j)
        
!       update sediment in pothole  
        pot_sed(j) = pot_sed(j) + sedyld(j) * pot_fr(j)
        potsedi(j) = pot_sed(j) 
        pot_san(j) = pot_san(j) + sanyld(j) * pot_fr(j) 
        potsani(j) = pot_san(j) 
        pot_sil(j) = pot_sil(j) + silyld(j) * pot_fr(j) 
        potsili(j) = pot_sil(j) 
        pot_cla(j) = pot_cla(j) + clayld(j) * pot_fr(j) 
        potclai(j) = pot_cla(j) 
        pot_sag(j) = pot_sag(j) + sagyld(j) * pot_fr(j) 
        potsagi(j) = pot_sag(j) 
        pot_lag(j) = pot_lag(j) + lagyld(j) * pot_fr(j)
        potlagi(j) = pot_lag(j) 

        yy = 1. - pot_fr(j)
        sedyld(j) = sedyld(j) * yy
        sanyld(j) = sanyld(j) * yy
        silyld(j) = silyld(j) * yy
        clayld(j) = clayld(j) * yy
        sagyld(j) = sagyld(j) * yy
        lagyld(j) = lagyld(j) * yy

!       update forms of N and P in pothole
        xx = pot_fr(j) * hru_ha(j)
        pot_no3(j) = pot_no3(j) + no3in * xx
        pot_solp(j) = pot_solp(j) + surqsolp(j) * xx
        pot_orgn(j) = pot_orgn(j) + sedorgn(j) * xx
        pot_orgp(j) = pot_orgp(j) + sedorgp(j) * xx
        pot_mps(j) = pot_mps(j) + sedminps(j) * xx
        pot_mpa(j) = pot_mpa(j) + sedminpa(j) * xx
!       track incoming loads
        pot_sedin(j)= pot_sedin(j) + sedyld(j) * pot_fr(j)
        pot_no3i(j) = pot_no3i(j) + no3in * xx
        pot_solpi(j) = pot_solpi(j) + surqsolp(j) * xx
        pot_orgni(j) = pot_orgni(j) + sedorgn(j) * xx
        pot_orgpi(j) = pot_orgpi(j) + sedorgp(j) * xx
        pot_mpsi(j) = pot_mpsi(j) + sedminps(j) * xx
        pot_mpai(j) = pot_mpai(j) + sedminpa(j) * xx

!       update forms of N and P in surface runoff
        yy = 1. - pot_fr(j)
        surqno3(j) = surqno3(j) * yy
        latno3(j) = latno3(j) * yy
!        gwno3(j) = gwno3(j) * yy
        surqsolp(j) = surqsolp(j) * yy
        sedorgn(j) = sedorgn(j) * yy
        sedorgp(j) = sedorgp(j) * yy
        sedminps(j) = sedminps(j) * yy
        sedminpa(j) = sedminpa (j) * yy

!       if overflow, then send the overflow to the HRU surface flow  
        if (pot_vol(j) > pot_volxmm(j)) then
          qdr(j) = qdr(j) + (pot_vol(j)- pot_volxmm(j))
!          qday = qday + (pot_vol(j)- pot_volxmm(j))
          spillo = pot_vol(j) - pot_volxmm(j)
          pot_vol(j) = pot_volxmm(j)
          xx = spillo / (spillo + pot_volxmm(j))
          potsedo = potsedo + pot_sed(j) * xx
          potsano = potsano + pot_san(j) * xx
          potsilo = potsilo + pot_sil(j) * xx
          potclao = potclao + pot_cla(j) * xx
          potsago = potsago + pot_sag(j) * xx
          potlago = potlago + pot_lag(j) * xx
          potno3o = potno3o + pot_no3(j) * xx
          potsolpo = potsolpo + pot_solp(j) * xx
          potorgno = potorgno + pot_orgn(j) * xx
          potorgpo = potorgpo + pot_orgp(j) * xx
          potmpso = potmpso + pot_mps(j) * xx
          potmpao = potmpao + pot_mpa(j) * xx
          
          pot_sed(j) = pot_sed(j) - potsedo
          pot_san(j) = pot_san(j) - potsano
          pot_sil(j) = pot_sil(j) - potsilo 
          pot_cla(j) = pot_cla(j) - potclao
          pot_sag(j) = pot_sag(j) - potsago
          pot_lag(j) = pot_lag(j) - potlago

          pot_no3(j) = pot_no3(j) - potno3o 
          pot_solp(j) = pot_solp(j) - potsolpo 
          pot_orgn(j) = pot_orgn(j) - potorgno 
          pot_orgp(j) = pot_orgp(j) - potorgpo
          pot_mps(j) = pot_mps(j) - potmpso
          pot_mpa(j) = pot_mpa(j) - potmpao
          
          sedyld(j) = sedyld(j) + potsedo
          sanyld(j) = sanyld(j) + potsano
          silyld(j) = silyld(j) + potsilo 
          clayld(j) = clayld(j) + potclao
          pot_sag(j) = sagyld(j) + potsago
          lagyld(j) = lagyld(j) + potlago

          surqno3(j) = surqno3(j) + potno3o
          surqsolp(j) = surqsolp(j) + potsolpo
          sedorgn(j) = sedorgn(j) + potorgno
          sedorgp(j) = sedorgp(j) + potorgpo
          sedminps(j) = sedminps(j) + potmpso
          sedminpa(j) = sedminpa(j) + potmpao
        end if       !! if overflow 
          
!      If no overflow, compute settling and losses, surface inlet tile
!      flow, evap, seepage, and redistribute soil water
       if (pot_vol(j) > 1.e-6) then
!        compute settling -clay and silt based on fall velocity (v=411*d2) d=mm, v=m/hr
         pot_depth = pot_vol(j)
         if (pot_depth > 10.) then        !assume clay v(fall)= 10 mm/d
           drcla = 1. - .5 * 10. / pot_depth
         else
           drcla = .5 * pot_depth / 10.
         end if
         pot_cla(j) = drcla * pot_cla(j)
         
         if (pot_depth > 1000.) then    !assume silt v(fall)= 1000 mm/d
           drsil = 1. - .5 * 1000. / pot_depth
         else
           drsil = .5 * pot_depth / 1000.
         end if
         pot_sil(j) = drsil * pot_sil(j)
!        assume complete settlling of all other sizes (dr = 0)
         pot_san(j) = 0.
         pot_sag(j) = 0.
         pot_lag(j) = 0.
         
!        compute total delivery ratio for pot_sed
         drtot = (pot_cla(j) + pot_sil(j) + pot_san(j) + pot_sag(j) +   
     &      pot_lag(j)) / (potclai(j) + potsili(j) + potsani(j) +       
     &      potsagi(j) + potlagi(j))
         pot_sed(j) = drtot * pot_sed(j)
         
!        compute organic settling assuming an enrichment ratio of 3 on clay (0.75)  
!        delivery of organics is 0.75*dr(clay)- assuming dr on all non-clay = 1
         pot_orgn(j) = .75 * drcla * pot_orgn(j)
         pot_orgp(j) = .75 * drcla * pot_orgp(j) 
         pot_mps(j) = .75 * drcla * pot_mps(j) 
         pot_mpa(j) = .75 * drcla * pot_mpa(j) 
          
         pot_no3(j) = pot_no3(j) * (1. - pot_no3l(j))
         pot_solp(j) = pot_solp(j) * (1. - pot_solpl(j))
!         hlife_pot = 20.    !!assume half life of 20 days
!         pot_no3(j) = Exp(-.693 / hlife_pot) * pot_no3(j)          
!         pot_solp(j) = Exp(-.693 / hlife_pot) * pot_solp(j)
          
!       compute flow from surface inlet tile
        tileo = Min(pot_tilemm(j), pot_vol(j))
        potvol_tile = pot_vol(j)
        pot_vol(j) = pot_vol(j) - tileo
        qdr(j) = qdr(j) + tileo
        tileq(j) = tileq(j) + tileo
        sumo = sumo + tileo
        tile_out(j) = tile_out(j) + tileo
          
!       limit seepage into soil if profile is near field capacity
         if (pot_k(j) > 0.) then
           yy = pot_k(j)
         else
           yy = sol_k(1,j)
         endif  
         
!        calculate seepage into soil
         potsep = yy * potsa(j) * 240. / cnv                       !!mm/h*ha/240=m3/cnv=mm
         potsep = Min(potsep, pot_vol(j))
         potvol_sep = pot_vol(j)
         pot_vol(j) = pot_vol(j) - potsep
         pot_seep(j) = potsep
         
!         call percmain
!         sol_st(1,j) = sol_st(1,j) + potsep
!!        redistribute water so that no layer exceeds maximum storage
!          excess = sol_st(ly,j) - sol_fc(ly,j)
!          do ly = 1, sol_nly(j)
!            if (excess < 0.) exit
!            if (ly < sol_nly(j)) then
!              sol_st(ly+1,j) = sol_st(ly+1,j) + excess
!              excess = sol_st(ly+1,j) - sol_fc(ly+1,j)
!              sol_st(ly,j) = sol_fc(ly,j)
!            else
!              sol_st(ly,j) = sol_fc(ly,j)
!            end if
!          end do
!          excess = Max(0.,excess)
!          
!          if (excess > 1.e-9) then
!            do ly = 1, sol_nly(j)
!              excess = sol_st(ly,j) - sol_ul(ly,j)
!              if (excess < 0.) exit
!              if (ly < sol_nly(j)) then
!                sol_st(ly+1,j) = sol_st(ly+1,j) + excess
!                sol_st(ly,j) = sol_ul(ly,j)
!              else
!                sol_st(ly,j) = sol_ul(ly,j)
!                pot_vol(j) = pot_vol(j) + excess
!                potsep = potsep - excess
!              end if
!            end do
!            pot_seep(j) = pot_seep(j) + potsep
!          end if
          
!         recompute total soil water
          sol_sw(j) = 0.
          do ly = 1, sol_nly(j)
            sol_sw(j) = sol_sw(j) + sol_st(ly,j)
          end do
      
!       compute evaporation from water surface
          if (laiday(j) < evlai) then
            potev = (1. - laiday(j) / evlai) * pet_day
            potev = Min(potev, pot_vol(j))
            pot_vol(j) = pot_vol(j) - potev
            pot_evap(j)= pot_evap(j) + potev
          endif

        if (potvol_tile > 1.e-6) then

          sedloss = pot_sed(j) * tileo / potvol_tile
          sedloss = Min(sedloss, pot_sed(j))            
			  
          pot_sed(j) = pot_sed(j) - sedloss
          potsedo = potsedo + sedloss
          sedyld(j) = sedyld(j) + sedloss
          no3loss = pot_no3(j) *  tileo / potvol_tile
          no3loss = Min(no3loss, pot_no3(j))
          pot_no3(j) = pot_no3(j) - no3loss
          surqno3(j) = surqno3(j) + no3loss / hru_ha(j)
			  
          solploss = pot_solp(j) *  tileo / potvol_tile
          solploss = Min(solploss, pot_solp(j))
          solp_tileo = solploss
          pot_solp(j) = pot_solp(j) - solploss
          surqsolp(j) = surqsolp(j) + solploss / hru_ha(j)

          orgnloss = pot_orgn(j) *  tileo / potvol_tile
          orgnloss = Min(orgnloss, pot_orgn(j))
          pot_orgn(j) = pot_orgn(j) - orgnloss
          sedorgn(j) = sedorgn(j) + orgnloss / hru_ha(j)

          orgploss = pot_orgp(j) *  tileo / potvol_tile
          orgploss = Min(orgploss, pot_orgp(j))
          pot_orgp(j) = pot_orgp(j) - orgploss
          sedorgp(j) = sedorgp(j) + orgploss / hru_ha(j)

          minpsloss = pot_mps(j) *  tileo / potvol_tile
          minpsloss = Min(minpsloss, pot_mps(j))
          pot_mps(j) = pot_mps(j) - minpsloss
          sedminps(j) = sedminps(j) + minpsloss / hru_ha(j)

          minpaloss = pot_mpa(j) *  tileo / potvol_tile
          minpaloss = Min(minpaloss, pot_mpa(j))
          pot_mpa(j) = pot_mpa(j) - minpaloss
          sedminpa(j) = sedminpa(j) + minpaloss / hru_ha(j)

          sanloss = pot_san(j) *  tileo / potvol_tile
          pot_san(j) = pot_san(j) - sanloss
          potsano = potsano + sanloss
          sanyld(j) = sanyld(j) + sanloss

          silloss = pot_sil(j) *  tileo / potvol_tile
          pot_sil(j) = pot_sil(j) - silloss
          potsilo = potsilo + silloss
          silyld(j) = silyld(j) + silloss

          claloss = pot_cla(j) *  tileo / potvol_tile
          pot_cla(j) = pot_cla(j) - claloss
          potclao = potclao + claloss
          clayld(j) = clayld(j) + claloss

          sagloss = pot_sag(j) *  tileo / potvol_tile
          pot_sag(j) = pot_sag(j) - sagloss
          potsago = potsago + sagloss
          sagyld(j) = sagyld(j) + sagloss

          lagloss = pot_lag(j) *  tileo / potvol_tile
          pot_lag(j) = pot_lag(j) - lagloss
          potlago = potlago + lagloss
          lagyld(j) = lagyld(j) + lagloss

    
!         track loadings removed via tile flow
          tile_sedo(j)= tile_sedo(j)+ sedloss
          tile_no3o(j)= tile_no3o(j)+ no3loss
          tile_solpo(j)= tile_solpo(j)+ solploss
          tile_orgno(j)= tile_orgno(j)+ orgnloss
          tile_orgpo(j)= tile_orgpo(j)+ orgploss
          tile_minpso(j)= tile_minpso(j)+ minpsloss
          tile_minpao(j)= tile_minpao(j)+ minpaloss
        end if

        if (potvol_sep > 1.e-6) then

          sedloss = pot_sed(j) * potsep / potvol_sep
          sedloss = Min(sedloss, pot_sed(j))            
          pot_sed(j) = pot_sed(j) - sedloss

          no3loss = pot_no3(j) *  potsep / potvol_sep
          no3loss = Min(no3loss, pot_no3(j))
          pot_no3(j) = pot_no3(j) - no3loss
			  
          solploss = pot_solp(j) *  potsep / potvol_sep
          solploss = Min(solploss, pot_solp(j))
          pot_solp(j) = pot_solp(j) - solploss

          orgnloss = pot_orgn(j) *  potsep / potvol_sep
          orgnloss = Min(orgnloss, pot_orgn(j))
          pot_orgn(j) = pot_orgn(j) - orgnloss

          orgploss = pot_orgp(j) *  potsep / potvol_sep
          orgploss = Min(orgploss, pot_orgp(j))
          pot_orgp(j) = pot_orgp(j) - orgploss

          minpsloss = pot_mps(j) *  potsep / potvol_sep
          minpsloss = Min(minpsloss, pot_mps(j))
          pot_mps(j) = pot_mps(j) - minpsloss

          minpaloss = pot_mpa(j) *  potsep / potvol_sep
          minpaloss = Min(minpaloss, pot_mpa(j))
          pot_mpa(j) = pot_mpa(j) - minpaloss

          sanloss = pot_san(j) *  potsep / potvol_sep
          pot_san(j) = pot_san(j) - sanloss

          silloss = pot_sil(j) *  potsep / potvol_sep
          pot_sil(j) = pot_sil(j) - silloss

          claloss = pot_cla(j) *  potsep / potvol_sep
          pot_cla(j) = pot_cla(j) - claloss

          sagloss = pot_sag(j) *  potsep / potvol_sep
          pot_sag(j) = pot_sag(j) - sagloss

          lagloss = pot_lag(j) *  potsep / potvol_sep
          pot_lag(j) = pot_lag(j) - lagloss
    
!         track loadings removed via seepage

        end if

        endif

!       if urban bmp - set maximum concentrations           
!        xx = sed_con(j) + soln_con(j) + solp_con(j) + orgn_con(j)       &
!     &             + orgp_con(j)
!        if (xx > 1.e-6) then
!          call urb_bmp
!        end if

!     summary calculations
      if (curyr > nyskip) then
        potmm = 0.
        if (pot_vol(j) > 0. .and. potsa(j) > 0.0) then
          potmm = pot_vol(j) / potsa(j) / 10.
        endif
        wshd_pinlet = wshd_pinlet + solp_tileo / hru_ha(j) * hru_dafr(j)
        spadyo = spadyo + sumo * hru_dafr(j)
        spadyosp = spadyosp + spillo * hru_dafr(j)
        spadyev = spadyev + potev * hru_dafr(j)
        spadysp = spadysp + potsep * hru_dafr(j)
        spadyrfv = spadyrfv + precipday * hru_dafr(j)
      end if
      
      potvol_m3 = pot_vol(j) * cnv
      potsa(j) = pi * (3. * potvol_m3 / (pi * hru_slp(j)))**.6666
      potsa(j) = potsa(j) / 10000.                  !convert to ha  
      if (potsa(j) <= 0.000001) then
        potsa(j) = 0.001
      endif
      if (potsa(j) > hru_ha(j)) then
        potsa(j) = hru_ha(j)
      endif
!     !!! output.pot and output.wtr turned on by same code named IWTR in file.cio
      if (iwtr == 1) then
        write (125,2000) subnum(j), hruno(j), i, iyr, potvol_ini,       
     &       potsa_ini, spillo, potsep, potev, sol_sw(j), tileo,        
     &       pot_vol(j), potsa(j)
      endif
  
      return
1000  format (1x,i4,2x,9(f8.2,2x))
2000  format (a5,a4,1x,2i5,9f10.2)
      end    