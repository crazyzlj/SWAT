      subroutine impndday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily HRU output to the output.wtr file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    ep_day        |mm H2O        |actual amount of transpiration that occurs on
!!                                 |day in HRU
!!    es_day        |mm H2O        |actual amount of evaporation (from soil) that
!!                                 |occurs on day in HRU
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_ha(:)     |ha            |area of HRU in hectares
!!    hrugis(:)     |none          |GIS code printed to output files(output.hru,.rch)
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    iida          |julian date   |current day of simulation
!!    idplt(:,:,:)  |none          |land cover code from crop.dat
!!    ihru          |none          |HRU number
!!    inum1         |none          |subbasin number
!!    ipot(:)       |none          |number of HRU (in subbasin) that is ponding
!!                                 |water--the HRU that the surface runoff from
!!                                 |current HRU drains into. This variable is
!!                                 |used only for rice paddys or closed
!!                                 |depressional areas
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    nro(:)        |none          |sequence number of year in rotation
!!    pnd_chla(:)   |kg chl_a      |amount of chlorophyll-a in pond at end of day
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
!!    pnd_vol(:)    |m^3 H2O       |volume of water in pond
!!    pndev         |m^3 H2O       |evaporation from pond on day
!!    pndflwi       |m^3 H2O       |volume of water flowing into pond on day
!!    pndflwo       |m^3 H2O       |volume of water flowing out of pond on day
!!    pndpcp        |m^3 H2O       |precipitation on pond during day
!!    pndsedin      |metric tons   |sediment entering pond during day
!!    pndsedo       |metric tons   |sediment leaving pond during day
!!    pndsep        |m^3 H2O       |seepage from pond on day
!!    pot_vol(:)    |m**3 H2O      |current volume of water stored in the
!!                                 |depression/impounded area
!!    potevmm       |mm H2O        |volume of water evaporated from pothole
!!                                 |expressed as depth over HRU
!!    potflwi(:)    |m^3 H2O       |water entering pothole on day
!!    potflwo       |mm H2O        |discharge from pothole expressed as depth
!!                                 |over HRU
!!    potpcpmm      |mm H2O        |precipitation falling on pothole water body
!!                                 |expressed as depth over HRU
!!    potsa(:)      |ha            |surface area of impounded water body
!!    potsedi(:)    |metric tons   |sediment entering pothole on day
!!    potsedo       |metric tons   |sediment leaving pothole on day
!!    potsepmm      |mm H2O        |seepage from pothole expressed as depth over
!!                                 |HRU
!!    qday          |mm H2O        |surface runoff loading to main channel for
!!                                 |day in HRU
!!    tloss         |mm H2O        |amount of water removed from surface runoff
!!                                 |via transmission losses on day in HRU
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
!!    wetev         |m^3 H2O       |evaporation from wetland for day
!!    wetflwi       |m^3 H2O       |volume of water flowing in wetland on day
!!    wetflwo       |m^3 H2O       |volume of water flowing out wetland on day
!!    wetpcp        |m^3 H2O       |precipitation on wetland for day
!!    wetsedi       |metric tons   |sediment loading to wetland for day
!!    wetsedo       |metric tons   |sediment loading from wetland for day
!!    wetsep        |m^3 H2O       |seepage from wetland bottom for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ano3_ppm    |mg N/L        |nitrate concentration in pond
!!    ano3_ppw    |mg N/L        |nitrate concentration in wetland
!!    chla_ppm    |mg chla/L     |chlorophyll-a concentration in pond
!!    chla_ppw    |mg chla/L     |chlorophyll-a concentration in wetland
!!    cnv         |none          |convert mm H2O to m^3 H2O
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
      real*8 :: orgn_ppm, orgp_ppm, ano3_ppm, minp_ppm, chla_ppm
      real*8 :: orgn_ppw, orgp_ppw, ano3_ppw, solp_ppw, chla_ppw, cnv 
      real*8, dimension (40) :: pdvas
      character*4 cropname

      j = 0
      sb = 0
      j = ihru
      sb = inum1

      iflag = 0
      if (pnd_fr(j) >= 0.01) iflag = 1
      if (wet_fr(j) >= 0.01) iflag = 1
  !!    if (ipot(j) == j) iflag = 1
      if (pot_fr(j) > 0.) iflag = 1

      if (iflag == 1) then
      cnv = 0.
      cnv = 10. * hru_ha(j)

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

      pdvas(1) = pndpcp / cnv
      pdvas(2) = pndflwi / cnv
      pdvas(3) = pndsedin / hru_ha(j)
      pdvas(4) = pndev / cnv
      pdvas(5) = pndsep / cnv
      pdvas(6) = pndflwo / cnv
      pdvas(7) = pndsedo / hru_ha(j)
      pdvas(8) = pnd_vol(j)
      pdvas(9) = orgn_ppm
      pdvas(10) = ano3_ppm
      pdvas(11) = orgp_ppm
      pdvas(12) = minp_ppm
      pdvas(13) = chla_ppm
      pdvas(14) = pnd_seci(j)
      pdvas(15) = wetpcp / cnv
      pdvas(16) = wetflwi / cnv
      pdvas(17) = wetsedi / hru_ha(j)
      pdvas(18) = wetev / cnv
      pdvas(19) = wetsep / cnv
      pdvas(20) = wetflwo / cnv
      pdvas(21) = wetsedo / hru_ha(j)
      pdvas(22) = wet_vol(j)
      pdvas(23) = orgn_ppw
      pdvas(24) = ano3_ppw
      pdvas(25) = orgp_ppw
      pdvas(26) = solp_ppw
      pdvas(27) = chla_ppw
      pdvas(28) = wet_seci(j)
      pdvas(29) = potpcpmm
      pdvas(30) = potflwi(j) / cnv
      pdvas(31) = potsedi(j) / hru_ha(j)
      pdvas(32) = potevmm
      pdvas(33) = potsepmm
      pdvas(34) = potflwo
      pdvas(35) = potsedo / hru_ha(j)
      pdvas(36) = pot_vol(j)
      pdvas(37) = potsa(j)
      pdvas(38) = qday + tloss
      pdvas(39) = ep_day
      pdvas(40) = es_day

      if (idplt(j) > 0) then
        cropname = cpnm(idplt(j))
      else
        cropname = 'BARR'
      end if

        if (iwtr == 1) then
          write (29,1000) cropname, j, subnum(j), hruno(j), sb,         
     &     nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, 40)
        end if
      end if

      return
 1000 format (a4,i5,1x,a5,a4,1x,i4,1x,i4,1x,i4,8f10.3,1e10.4,13f10.3,   
     & 1e10.4,13f10.3,1e10.4,5f10.3)
      end