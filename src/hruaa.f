      subroutine hruaa(years)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_aams(:)   |metric tons/ha|average annual biomass (dry weight) in HRU
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    deepst(:)     |mm H2O        |depth of water in deep aquifer
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_sub(:)    |none          |subbasin in which HRU is located
!!    hruaao(1,:)   |mm H2O        |precipitation in HRU during simulation
!!    hruaao(2,:)   |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow in HRU during simulation
!!    hruaao(3,:)   |mm H2O        |amount of snow melt in HRU during simulation
!!    hruaao(4,:)   |mm H2O        |amount of surface runoff to main channel
!!                                 |from HRU during simulation (ignores impact of
!!                                 |transmission losses)
!!    hruaao(5,:)   |mm H2O        |amount of lateral flow contribution to main
!!                                 |channel from HRU during simulation
!!    hruaao(6,:)   |mm H2O        |amount of groundwater flow contribution to
!!                                 |main channel from HRU during simulation
!!    hruaao(7,:)   |mm H2O        |amount of water moving from shallow aquifer
!!                                 |to plants or soil profile in HRU during 
!!                                 |simulation
!!    hruaao(8,:)   |mm H2O        |amount of water recharging deep aquifer in
!!                                 |HRU during simulation
!!    hruaao(9,:)   |mm H2O        |total amount of water entering both aquifers
!!                                 |from HRU during simulation
!!    hruaao(10,:)  |mm H2O        |water yield (total amount of water entering
!!                                 |main channel) from HRU during simulation
!!    hruaao(11,:)  |mm H2O        |amount of water percolating out of the soil
!!                                 |profile and into the vadose zone in HRU
!!                                 |during simulation
!!    hruaao(12,:)  |mm H2O        |actual evapotranspiration in HRU during 
!!                                 |simulation
!!    hruaao(13,:)  |mm H2O        |amount of transmission losses from tributary
!!                                 |channels in HRU for simulation
!!    hruaao(14,:)  |metric tons/ha|sediment yield from HRU for simulation
!!    hruaao(17,:)  |kg N/ha       |amount of nitrogen applied in continuous 
!!                                 |fertilizer operation in HRU for simulation
!!    hruaao(18,:)  |kg P/ha       |amount of phosphorus applied in continuous
!!                                 |fertilizer operation in HRU for simulation
!!    hruaao(23,:)  |mm H2O        |amount of water removed from shallow aquifer
!!                                 |in HRU for irrigation during simulation
!!    hruaao(24,:)  |mm H2O        |amount of water removed from deep aquifer
!!                                 |in HRU for irrigation during simulation
!!    hruaao(25,:)  |mm H2O        |potential evapotranspiration in HRU during
!!                                 |simulation
!!    hruaao(26,:)  |kg N/ha       |annual amount of N (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hruaao(27,:)  |kg P/ha       |annual amount of P (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hruaao(28,:)  |kg N/ha       |average annual amount of N (organic &
!!                                 |mineral) auto-applied in HRU
!!    hruaao(29,:)  |kg P/ha       |average annual amount of P (organic &
!!                                 |mineral) auto-applied in HRU
!!    hruaao(31,:)  |stress days   |water stress days in HRU during simulation
!!    hruaao(32,:)  |stress days   |temperature stress days in HRU during 
!!                                 |simulation
!!    hruaao(33,:)  |stress days   |nitrogen stress days in HRU during simulation
!!    hruaao(34,:)  |stress days   |phosphorus stress days in HRU during 
!!                                 |simulation
!!    hruaao(35,:)  |kg N/ha       |organic nitrogen in surface runoff in HRU
!!                                 |during simulation
!!    hruaao(36,:)  |kg P/ha       |organic phosphorus in surface runoff in HRU
!!                                 |during simulation
!!    hruaao(37,:)  |kg N/ha       |nitrate in surface runoff in HRU during
!!                                 |simulation
!!    hruaao(38,:)  |kg N/ha       |nitrate in lateral flow in HRU during
!!                                 |simulation
!!    hruaao(39,:)  |kg P/ha       |soluble phosphorus in surface runoff in HRU
!!                                 |during simulation
!!    hruaao(40,:)  |kg N/ha       |amount of nitrogen removed from soil by plant
!!                                 |uptake in HRU during simulation
!!    hruaao(41,:)  |kg N/ha       |nitrate percolating past bottom of soil
!!                                 |profile in HRU during simulation
!!    hruaao(42,:)  |kg P/ha       |amount of phosphorus removed from soil by
!!                                 |plant uptake in HRU during simulation
!!    hruaao(43,:)  |kg P/ha       |amount of phosphorus moving from labile
!!                                 |mineral to active mineral pool in HRU during
!!                                 |simulation
!!    hruaao(44,:)  |kg P/ha       |amount of phosphorus moving from active
!!                                 |mineral to stable mineral pool in HRU during
!!                                 |simulation
!!    hruaao(45,:)  |kg N/ha       |amount of nitrogen applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |simulation
!!    hruaao(46,:)  |kg P/ha       |amount of phosphorus applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |simulation
!!    hruaao(47,:)  |kg N/ha       |amount of nitrogen added to soil by fixation
!!                                 |in HRU during simulation
!!    hruaao(48,:)  |kg N/ha       |amount of nitrogen lost by denitrification
!!                                 |in HRU during simulation
!!    hruaao(49,:)  |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to nitrate pool in HRU during simulation
!!    hruaao(50,:)  |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to stable organic pool in HRU during 
!!                                 |simulation
!!    hruaao(51,:)  |kg P/ha       |amount of phosphorus moving from organic to
!!                                 |labile mineral pool in HRU during simulation
!!    hruaao(52,:)  |kg N/ha       |amount of nitrogen moving from fresh organic
!!                                 |to nitrate and active organic pools in HRU
!!                                 |during simulation
!!    hruaao(53,:)  |kg P/ha       |amount of phosphorus moving from fresh
!!                                 |organic to the labile mineral and organic
!!                                 |pools in HRU during simulation
!!    hruaao(54,:)  |kg N/ha       |amount of nitrogen added to soil in rain
!!                                 |during simulation
!!    hruaao(61,:)  |metric tons/ha|daily soil loss predicted with USLE equation
!!    hruaao(63,:)  |# bacteria/ha |less persistent bacteria transported to main
!!                                 |channel from HRU during simulation
!!    hruaao(64,:)  |# bacteria/ha |persistent bacteria transported to main
!!                                 |channel from HRU during simulation
!!    hruaao(65,:)  |kg N/ha       |nitrate loading from groundwater in HRU to
!!                                 |main channel during simulation
!!    hruaao(66,:)  |kg P/ha       |soluble P loading from groundwater in HRU to
!!                                 |main channel during simulation
!!    hruaao(67,:)  |kg P/ha       |loading of mineral P attached to sediment
!!                                 |in HRU to main channel during simulation
!!    hrugis(:)     |none          |GIS code printed to output files(output.hru,.rch)
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    idplt(:)      |none          |land cover code from crop.dat
!!    ipdvas(:)     |none          |output variable codes for output.hru file
!!    isproj        |none          |special project code:
!!                                 |1 test rewind (run simulation twice)
!!    itots         |none          |number of output variables printed (output.hru)
!!    lai_aamx(:)   |none          |average annual maximum leaf area index in
!!                                 |HRU
!!    mhruo         |none          |maximum number of variables written to
!!                                 |HRU output file (output.hru)
!!    nhru          |none          |number of HRUs in watershed
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    nro(:)        |none          |sequence number of year in rotation
!!    shallst(:)    |mm H2O        |depth of water in shallow aquifer
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |on any given day
!!    yldaa(:)      |metric tons/ha|average annual yield (dry weight) in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    pdvas(:)    |varies        |array to hold HRU output values
!!    pdvs(:)     |varies        |array to hold selected HRU output values
!!                               |when user doesn't want to print all
!!    sb          |none          |subbasin number
!!    years       |years         |length of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real*8, intent (in) :: years
      integer :: j, sb, ii, iflag
      real*8, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname

      do j = 1, nhru
        sb = hru_sub(j)

        iflag = 0
        do ii = 1, itoth
          if (ipdhru(ii) == j) iflag = 1
        end do


        if (iflag == 1) then
        pdvas = 0.
        pdvs = 0.

        pdvas(1) = hruaao(1,j)
        pdvas(2) = hruaao(2,j)
        pdvas(3) = hruaao(3,j)
        pdvas(4) = hruaao(22,j)
        pdvas(5) = hruaao(25,j)
        pdvas(6) = hruaao(12,j)
        pdvas(7) = hruaao(21,j) / 365.4
        pdvas(8) = sol_sw(j)
        pdvas(9) = hruaao(11,j)
        pdvas(10) = hruaao(9,j)
        pdvas(11) = hruaao(8,j)
        pdvas(12) = hruaao(7,j)
        pdvas(13) = hruaao(23,j)
        pdvas(14) = hruaao(24,j)
        pdvas(15) = shallst(j)
        pdvas(16) = deepst(j)
        pdvas(17) = hruaao(19,j)
        pdvas(18) = hruaao(4,j)
        pdvas(19) = hruaao(13,j)
        pdvas(20) = hruaao(5,j)
        pdvas(21) = hruaao(6,j)
        pdvas(22) = hruaao(10,j)
        pdvas(23) = hruaao(20,j) / 365.4
        pdvas(24) = hruaao(57,j) / 365.4
        pdvas(25) = hruaao(55,j) / 365.4
        pdvas(26) = hruaao(56,j) / 365.4
        pdvas(27) = hruaao(30,j) / 365.4
        pdvas(28) = hruaao(58,j) / 365.4
        pdvas(29) = hruaao(14,j)
        pdvas(30) = hruaao(61,j)
        pdvas(31) = hruaao(45,j)
        pdvas(32) = hruaao(46,j)
        pdvas(33) = hruaao(28,j)
        pdvas(34) = hruaao(29,j)
        pdvas(35) = hruaao(26,j)
        pdvas(36) = hruaao(27,j)
        pdvas(37) = hruaao(17,j)
        pdvas(38) = hruaao(18,j)
        pdvas(39) = hruaao(54,j)
        pdvas(40) = hruaao(47,j)
        pdvas(41) = hruaao(52,j)
        pdvas(42) = hruaao(49,j)
        pdvas(43) = hruaao(50,j)
        pdvas(44) = hruaao(53,j)
        pdvas(45) = hruaao(51,j)
        pdvas(46) = hruaao(43,j)
        pdvas(47) = hruaao(44,j)
        pdvas(48) = hruaao(48,j)
        pdvas(49) = hruaao(40,j)
        pdvas(50) = hruaao(42,j)
        pdvas(51) = hruaao(35,j)
        pdvas(52) = hruaao(36,j)
        pdvas(53) = hruaao(67,j)
        pdvas(54) = hruaao(37,j)
        pdvas(55) = hruaao(38,j)
        pdvas(56) = hruaao(41,j)
        pdvas(57) = hruaao(65,j)
        pdvas(58) = hruaao(39,j)
        pdvas(59) = hruaao(66,j)
        pdvas(60) = hruaao(31,j)
        pdvas(61) = hruaao(32,j)
        pdvas(62) = hruaao(33,j)
        pdvas(63) = hruaao(34,j)
        pdvas(64) = bio_aams(j)
        pdvas(65) = lai_aamx(j)
        pdvas(66) = yldaa(j)
        pdvas(67) = hruaao(63,j)
        pdvas(68) = hruaao(64,j)
!!      the following two variables are values at the of the year
!!      they are not summed each day
        pdvas(69) = wtab(j)  !! based on 30 day antecedent climate(mm) (prec,et)
        pdvas(70) = wtabelo  !! based on depth from soil surface(mm)
!!      added current snow content in the hru (not summed)
        pdvas(71) = sno_hru(j)
        
!!      added current soil carbon for first layer
        pdvas(72) = cmup_kgh(j)   !! first soil layer only
!!      added current soil carbon integrated - aggregating all soil layers
        pdvas(73) = cmtot_kgh(j)
  
!!      adding qtile to output.hru write 3/2/2010 gsm
        pdvas(74) = hruaao(62,j)
!!      tileno3 - output.hru
        pdvas(75) = hruaao(68,j)
!!      latno3 - output.hru
        pdvas(76) = hruaao(69,j)
!!      gw deep
        pdvas(77) = hruaao(70,j)
!!      latq contribution
        pdvas(78) = hruaao(71,j)
!!      phos due to crack flow (tvap)
        pdvas(79) = hruaao(72,j)


        if (ipdvas(1) > 0) then
          do ii = 1, itots
            pdvs(ii) = pdvas(ipdvas(ii))
          end do

        idplant = idplt(j)
        if (idplant > 0) then
          cropname = cpnm(idplant)
        else
          cropname = "NOCR"
        endif

          if (iscen == 1 .and. isproj == 0) then
          write (28,1000) cropname, j, subnum(j), hruno(j), sb,         
     &        nmgt(j), years, hru_km(j), (pdvs(ii), ii = 1, itots)
          else if (isproj == 1) then
          write (21,1000) cropname, j, subnum(j), hruno(j),             
     &    sb, nmgt(j), years, hru_km(j), (pdvs(ii), ii = 1, itots)
          else if (iscen == 1 .and. isproj == 2) then
          write (28,2000) cropname, j, subnum(j), hruno(j), sb,         
     &    nmgt(j), years, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
          endif
        else
          if (iscen == 1 .and. isproj == 0) then
          write (28,1001) cropname, j, subnum(j), hruno(j), sb,         
     &            nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, mhruo)
          else if (isproj == 1) then
          write (21,1001) cropname, j, subnum(j), hruno(j),             
     &         sb, nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, mhruo)
          else if (iscen == 1 .and. isproj == 2) then
          write (28,1001) cropname, j, subnum(j), hruno(j), sb,         
     &    nmgt(j), years, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
          endif
        end if
        end if
      end do

      return

1000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,f4.1,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,8e10.3,3f10.3)
2000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,f4.1,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,5e10.3,6f10.3,1x,i4)
1001  format (a4,i7,1x,a5,a4,i5,1x,i4,1x,f4.1,e10.5,66f10.3,1x 
     *e10.5,1x,e10.5,8e10.3,3f10.3,1x,i4)
      
      end