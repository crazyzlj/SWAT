      subroutine impndmon

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly HRU impoundment output to the output.wtr
!!    file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_sub(:)    |none          |subbasin in which HRU is located
!!    hrugis(:)     |none          |GIS code printed to output files(output.hru,.rch)
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    idplt(:,:,:)  |none          |land cover code from crop.dat
!!    ihru          |none          |HRU number
!!    ipot(:)       |none          |number of HRU (in subbasin) that is ponding
!!                                 |water--the HRU that the surface runoff from
!!                                 |current HRU drains into. This variable is
!!                                 |used only for rice paddys or closed
!!                                 |depressional areas
!!    mo_chk        |none          |current month of simulation
!!    nhru          |none          |number of HRUs in watershed
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    nro(:)        |none          |sequence number of year in rotation
!!    pnd_chla(:)   |kg chl_a      |amount of chlorophyll-a in pond at end of 
!!                                 |day
!!    pnd_fr(:)     |none          |fraction of HRU/subbasin area that drains
!!                                 |into ponds
!!    pnd_no3(:)    |kg N          |amount of nitrate originating from surface
!!                                 |runoff in pond at end of day
!!    pnd_no3g(:)   |kg N          |amount of nitrate originating from
!!                                 |groundwater in pond at end of day
!!    pnd_no3s(:)   |kg N          |amount of nitrate originating from lateral
!!                                 |flow in pond at end of day
!!    pnd_orgn(:)   |kg N          |amount of organic N originating from
!!                                 |surface runoff in pond at end of day
!!    pnd_orgp(:)   |kg P          |amount of organic P originating from
!!                                 |surface runoff in pond at end of day
!!    pnd_psed(:)   |kg P          |amount of mineral P attached to sediment
!!                                 |originating from surface runoff in pond at
!!                                 |end of day
!!    pnd_seci(:)   |m             |secchi-disk depth of pond
!!    pnd_solp(:)   |kg P          |amount of soluble P originating from surface
!!                                 |runoff in pond at end of day
!!    pnd_solpg(:)  |kg P          |amount of soluble P originating from
!!                                 |groundwater in pond at end of day
!!    pot_vol(:)    |m**3 H2O      |current volume of water stored in the
!!                                 |depression/impounded area
!!    potsa(:)      |ha            |surface area of impounded water body
!!    hrumono(4,:)  |mm H2O        |amount of surface runoff to main channel
!!                                 |from HRU during month (ignores impact of
!!                                 |transmission losses)
!!    hrumono(15,:) |mm H2O        |actual amount of transpiration that occurs
!!                                 |during month in HRU
!!    hrumono(16,:) |mm H2O        |actual amount of evaporation (from soil) that
!!                                 |occurs during month in HRU
!!    wet_chla(:)   |kg chla       |amount of chlorophyll-a in wetland at end
!!                                 |of day
!!    wet_fr(:)     |none          |fraction of HRU/subbasin area that drains
!!                                 |into wetlands
!!    wet_no3(:)    |kg N          |amount of nitrate originating from surface
!!                                 |runoff in wetland at end of day
!!    wet_no3g(:)   |kg N          |amount of nitrate originating from
!!                                 |groundwater in wetland at end of day
!!    wet_no3s(:)   |kg N          |amount of nitrate originating from lateral
!!                                 |flow in wetland at end of day
!!    wet_orgn(:)   |kg N          |amount of organic N originating from
!!                                 |surface runoff in wetland at end of day
!!    wet_orgp(:)   |kg P          |amount of organic P originating from
!!                                 |surface runoff in wetland at end of day
!!    wet_psed(:)   |kg P          |amount of mineral P attached to sediment
!!                                 |originating from surface runoff in wetland at
!!                                 |end of day
!!    wet_seci(:)   |m             |secchi-disk depth in wetland at end of day
!!    wet_solp(:)   |kg P          |amount of soluble P originating from surface
!!                                 |runoff in wetland at end of day
!!    wet_solpg(:)  |kg P          |amount of soluble P originating from
!!                                 |groundwater in wetland at end of day
!!    wet_vol(:)    |m^3 H2O       |volume of water in wetlands
!!    wtrmon(1,:)   |mm H2O        |evaporation from ponds in HRU for month
!!    wtrmon(2,:)   |mm H2O        |seepage from ponds in HRU for month
!!    wtrmon(3,:)   |mm H2O        |precipitation on ponds in HRU for month
!!    wtrmon(4,:)   |mm H2O        |amount of water entering ponds in HRU for
!!                                 |month
!!    wtrmon(5,:)   |metric tons/ha|sediment entering ponds in HRU for month
!!    wtrmon(6,:)   |mm H2O        |amount of water leaving ponds in HRU for
!!                                 |month
!!    wtrmon(7,:)   |metric tons/ha|sediment leaving ponds in HRU for month
!!    wtrmon(8,:)   |mm H2O        |precipitation on wetlands in HRU for month
!!    wtrmon(9,:)   |mm H2O        |volume of water entering wetlands from HRU
!!                                 |for month
!!    wtrmon(10,:)  |metric tons/ha|sediment loading to wetlands for month from
!!                                 |HRU
!!    wtrmon(11,:)  |mm H2O        |evaporation from wetlands in HRU for month
!!    wtrmon(12,:)  |mm H2O        |seeepage from wetlands in HRU for month
!!    wtrmon(13,:)  |mm H2O        |volume of water leaving wetlands in HRU
!!                                 |for month
!!    wtrmon(14,:)  |metric tons/ha|sediment loading from wetlands in HRU to
!!                                 |main channel during month
!!    wtrmon(15,:)  |mm H2O        |precipitation on potholes in HRU for month
!!    wtrmon(16,:)  |mm H2O        |evaporation from potholes in HRU for month
!!    wtrmon(17,:)  |mm H2O        |seepage from potholes in HRU for month
!!    wtrmon(18,:)  |mm H2O        |water leaving potholes in HRU for month
!!    wtrmon(19,:)  |mm H2O        |water entering potholes in HRU for month
!!    wtrmon(20,:)  |metric tons/ha|sediment entering potholes in HRU for month
!!    wtrmon(21,:)  |metric tons/ha|sediment leaving potholes in HRU for month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ano3_ppm    |mg N/L        |nitrate concentration in pond
!!    ano3_ppw    |mg N/L        |nitrate concentration in wetland
!!    chla_ppm    |mg chla/L     |chlorophyll-a concentration in pond
!!    chla_ppw    |mg chla/L     |chlorophyll-a concentration in wetland
!!    iflag       |none          |flag to denote presence of impoundment in 
!!                               |HRU
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    minp_ppm    |mg P/L        |mineral P concentration in pond
!!    minp_ppw    |mg P/L        |mineral P concentration in wetland
!!    orgn_ppm    |mg N/L        |organic N concentration in pond
!!    orgn_ppw    |mg N/L        |organic N concentration in wetland
!!    orgp_ppm    |mg P/L        |organic P concentration in pond
!!    orgp_ppw    |mg P/L        |organic P concentration in wetland
!!    pdvas(:)    |varies        |array to hold HRU output values
!!    sb          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, sb, ii, iflag
      real :: orgn_ppm, orgp_ppm, ano3_ppm, minp_ppm, chla_ppm
      real :: orgn_ppw, orgp_ppw, ano3_ppw, solp_ppw, chla_ppw
      real, dimension (40) :: pdvas
      character*4 cropname

      do j = 1, nhru
        sb = 0
        sb = hru_sub(j)
        
        iflag = 0
        if (pnd_fr(j) >= 0.01) iflag = 1
        if (wet_fr(j) >= 0.01) iflag = 1
  !!      if (ipot(j) == j) iflag = 1
        if (pot_fr(j) > 0.) iflag = 1

        if (iflag == 1) then

!! calculate nutrient concentrations
      orgn_ppm = 0.
      orgp_ppm = 0.
      ano3_ppm = 0.
      minp_ppm = 0.
      chla_ppm = 0.
      orgn_ppw = 0.
      orgp_ppw = 0.
      ano3_ppw = 0.
      solp_ppw = 0.
      chla_ppw = 0.
      if (pnd_vol(j) > 1.) then
        orgn_ppm = 1000. * pnd_orgn(j) / pnd_vol(j)
        orgp_ppm = 1000. * pnd_orgp(j) / pnd_vol(j)
        ano3_ppm = 1000. * (pnd_no3(j) + pnd_no3s(j) + pnd_no3g(j)) /   
     &                                                        pnd_vol(j)
        minp_ppm = 1000. * (pnd_solp(j) + pnd_psed(j) + pnd_solpg(j)) / 
     &                                                        pnd_vol(j)
        chla_ppm = 1000. * pnd_chla(j) / pnd_vol(j)
      endif
      if (wet_vol(j) > 1.) then
        orgn_ppw = 1000. * wet_orgn(j) / wet_vol(j)
        orgp_ppw = 1000. * wet_orgp(j) / wet_vol(j)
        ano3_ppw = 1000. * (wet_no3(j) + wet_no3s(j) + wet_no3g(j)) /   
     &                                                        wet_vol(j)
        solp_ppw = 1000. * (wet_solp(j) + wet_solpg(j) + wet_psed(j)) / 
     &                                                        wet_vol(j)
        chla_ppw = 1000. * wet_chla(j) / wet_vol(j)
      end if

          pdvas = 0.

          pdvas(1) = wtrmon(3,j)
          pdvas(2) = wtrmon(4,j)
          pdvas(3) = wtrmon(5,j)
          pdvas(4) = wtrmon(1,j)
          pdvas(5) = wtrmon(2,j)
          pdvas(6) = wtrmon(6,j)
          pdvas(7) = wtrmon(7,j)
          pdvas(8) = pnd_vol(j)
          pdvas(9) = orgn_ppm
          pdvas(10) = ano3_ppm
          pdvas(11) = orgp_ppm
          pdvas(12) = minp_ppm
          pdvas(13) = chla_ppm
          pdvas(14) = pnd_seci(j)
          pdvas(15) = wtrmon(8,j)
          pdvas(16) = wtrmon(9,j)
          pdvas(17) = wtrmon(10,j)
          pdvas(18) = wtrmon(11,j)
          pdvas(19) = wtrmon(12,j)
          pdvas(20) = wtrmon(13,j)
          pdvas(21) = wtrmon(14,j)
          pdvas(22) = wet_vol(j)
          pdvas(23) = orgn_ppw
          pdvas(24) = ano3_ppw
          pdvas(25) = orgp_ppw
          pdvas(26) = solp_ppw
          pdvas(27) = chla_ppw
          pdvas(28) = wet_seci(j)
          pdvas(29) = wtrmon(15,j)
          pdvas(30) = wtrmon(19,j)
          pdvas(31) = wtrmon(20,j)
          pdvas(32) = wtrmon(16,j)
          pdvas(33) = wtrmon(17,j)
          pdvas(34) = wtrmon(18,j)
          pdvas(35) = wtrmon(21,j)
          pdvas(36) = pot_vol(j)
          pdvas(37) = potsa(j)
          pdvas(38) = hrumono(4,j)
          pdvas(39) = hrumono(15,j)
          pdvas(40) = hrumono(16,j)

      if (idplt(j) > 0) then
        cropname = cpnm(idplt(j))
      else
        cropname = 'BARR'

      end if
      if (iwtr == 1) then
          write (29,1000) cropname, j, subnum(j), hruno(j), sb,         
     &            nmgt(j), mo_chk, hru_km(j), (pdvas(ii), ii = 1, 40)
      end if
        end if
      end do

      return
 1000 format (a4,i5,1x,a5,a4,1x,i4,1x,i4,1x,i4,8f10.3,1e10.4,13f10.3,   
     & 1e10.4,13f10.3,1e10.4,5f10.3)
      end