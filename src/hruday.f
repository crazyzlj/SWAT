      subroutine hruday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes daily HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)       |mm H2O        |amount of water applied to HRU on current
!!                                 |day
!!    auton         |kg N/ha       |amount of nitrogen applied in auto-fert
!!                                 |application
!!    autop         |kg P/ha       |amount of phosphorus applied in auto-fert
!!                                 |application
!!    bactrolp      |# colonies/ha |less persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactrop       |# colonies/ha |persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactsedlp     |# colonies/ha |less persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bactsedp      |# colonies/ha |persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bio_ms(:)     |kg/ha         |land cover/crop biomass (dry weight)
!!    cfertn        |kg N/ha       |amount of nitrogen added to soil in
!!                                 |continuous fertilizer operation on day
!!    cfertp        |kg P/ha       |amount of phosphorus added to soil in
!!                                 |continuous fertilizer operation on day
!!    cnday(:)      |none          |curve number for current day, HRU and at
!!                                 |current soil moisture
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    deepirr(:)    |mm H2O        |amount of water removed from deep aquifer
!!                                 |for irrigation
!!    deepst(:)     |mm H2O        |depth of water in deep aquifer
!!    etday         |mm H2O        |actual amount of evapotranspiration that
!!                                 |occurs on day in HRU
!!    fertn         |kg N/ha       |total amount of nitrogen added to soil in
!!                                 |HRU on day in fertilizer application
!!    fertp         |kg P/ha       |total amount of phosphorus added to soil in
!!                                 |HRU on day in fertilizer application
!!    fixn          |kg N/ha       |amount of nitrogen added to plant biomass
!!                                 |via fixation on the day in HRU
!!    grazn         |kg N/ha       |amount of nitrogen added to soil in grazing
!!                                 |on the day in HRU
!!    grazp         |kg P/ha       |amount of phosphorus added to soil in
!!                                 |grazing on the day in HRU
!!    gw_q(:)       |mm H2O        |groundwater contribution to streamflow from
!!                                 |HRU on current day
!!    gwseep        |mm H2O        |amount of water recharging deep aquifer on
!!                                 |current day
!!    hmntl         |kg N/ha       |amount of nitrogen moving from active
!!                                 |organic to nitrate pool in soil profile
!!                                 |on current day in HRU
!!    hmptl         |kg P/ha       |amount of phosphorus moving from the
!!                                 |organic to labile pool in soil profile
!!                                 |on current day in HRU
!!    hru_ha(:)     |ha            |area of HRU in hectares
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_ra(:)     |MJ/m^2        |solar radiation for the day in HRU
!!    hrugis(:)     |none          |GIS code printed to output files
!!                                 |(output.hru, output.rch)
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    iida          |julian date   |current day of simulation
!!    idplt(:)      |none          |land cover code from crop.dat
!!    ihru          |none          |HRU number
!!    ipdvas(:)     |none          |output variable codes for output.hru file
!!    isproj        |none          |special project code:
!!                                 |1 test rewind (run simulation twice)
!!    itots         |none          |number of output variables printed 
!!                                 |(output.hru)
!!    laiday(:)     |none          |leaf area index for HRU
!!    latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for
!!                                 |the day
!!    latq(:)       |mm H2O        |amount of water in lateral flow in HRU for
!!                                 |the day
!!    mhruo         |none          |maximum number of variables written to
!!                                 |HRU output file (output.hru)
!!    minpgw(:)     |kg P/ha       |soluble P loading to reach in groundwater
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    no3gw(:)      |kg N/ha       |nitrate loading to reach in groundwater
!!    no3pcp        |kg N/ha       |nitrate added to the soil in rainfall
!!    nro(:)        |none          |sequence number of year in rotation
!!    nplnt(:)      |kg N/ha       |plant uptake of nitrogen in HRU for the day
!!    percn(:)      |kg N/ha       |NO3-N leached from soil profile during the
!!                                 |day
!!    pet_day       |mm H2O        |potential evapotranspiration for day in HRU
!!    pplnt(:)      |kg P/ha       |plant uptake of phosphorus in HRU for the 
!!                                 |day
!!    qday          |mm H2O        |surface runoff loading to main channel for
!!                                 |day in HRU
!!    qdr(:)        |mm H2O        |total amount of water entering main channel
!!                                 |for day from HRU
!!    rchrg(:)      |mm H2O        |amount of water recharging both aquifers on
!!                                 |current day in HRU
!!    revapday      |mm H2O        |amount of water moving from the shallow
!!                                 |aquifer into the soil profile or being taken
!!                                 |up by plant roots in the shallow aquifer
!!    rmn2tl        |kg N/ha       |amount of nitrogen moving from the fresh
!!                                 |organic (residue) to the nitrate(80%) and
!!                                 |active organic(20%) pools in soil profile
!!                                 |on current day in HRU
!!    rmp1tl        |kg P/ha       |amount of phosphorus moving from the labile
!!                                 |mineral pool to the active mineral pool in
!!                                 |the soil profile on the current day in the
!!                                 |HRU
!!    rmptl         |kg P/ha       |amount of phosphorus moving from the
!!                                 |fresh organic (residue) to the labile(80%)
!!                                 |and organic(20%) pools in soil profile
!!                                 |on current day in HRU
!!    roctl         |kg P/ha       |amount of phosphorus moving from the active
!!                                 |mineral pool to the stable mineral pool
!!                                 |in the soil profile on the current day in
!!                                 |the HRU
!!    rwntl         |kg N/ha       |amount of nitrogen moving from active
!!                                 |organic to stable organic pool in soil
!!                                 |profile on current day in HRU
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedorgn(:)    |kg N/ha       |amount of organic nitrogen in surface runoff
!!                                 |in HRU for the day
!!    sedorgp(:)    |kg P/ha       |amount of organic phosphorus in surface
!!                                 |runoff in HRU for the day
!!    sedyld(:)     |metric tons   |daily soil loss caused by water erosion
!!    sepbtm(:)     |mm H2O        |seepage leaving the bottom of the soil
!!                                 |profile on day in HRU
!!    shallirr(:)   |mm H2O        |amount of water removed from shallow aquifer
!!                                 |for irrigation
!!    shallst(:)    |mm H2O        |depth of water in shallow aquifer
!!    snofall       |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow on day in HRU
!!    snomlt        |mm H2O        |amount of water in snow melt for the day in
!!                                 |HRU
!!    sol_cnsw(:)   |mm H2O        |soil water content used to calculate daily
!!                                 |CN value (initial soil wter content for day)
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |at end of any given day
!!    sol_tmp(2,:)  |deg C         |daily average temperature of second soil 
!!                                 |layer
!!    strsn(:)      |none          |fraction of potential plant growth achieved
!!                                 |on the day where the reduction is caused by
!!                                 |nitrogen stress
!!    strsp(:)      |none          |fraction of potential plant growth achieved
!!                                 |on the day where the reduction is caused by
!!                                 |phosphorus stress
!!    strstmp(:)    |none          |fraction of potential plant growth achieved
!!                                 |on the day in HRU where the reduction is
!!                                 |caused by temperature stress
!!    strsw(:)      |none          |fraction of potential plant growth achieved
!!                                 |on the day where the reduction is caused by
!!                                 |water stress
!!    subp(:)       |mm H2O        |precipitation for the day in HRU
!!    surfq(:)      |mm H2O        |surface runoff generated on day in HRU
!!    surqno3(:)    |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                                 |the day
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    tloss         |mm H2O        |amount of water removed from surface runoff
!!                                 |via transmission losses on day in HRU
!!    tmn(:)        |deg C         |minimum temperature for the day in HRU
!!    tmpav(:)      |deg C         |average temperature for the day in HRU
!!    tmx(:)        |deg C         |maximum temperature for the day in HRU
!!    usle          |metric tons   |daily soil loss predicted with USLE equation
!!    wdntl         |kg N/ha       |amount of nitrogen lost from nitrate pool
!!                                 |by denitrification in soil profile on
!!                                 |current day in HRU
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, sb, ii, iflag
      real, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname

      j = 0
      j = ihru
      
      iflag = 0
      do ii = 1, itoth
        if (ipdhru(ii) == j) iflag = 1
      end do
      if (iflag == 0) return

      pdvas = 0.
      pdvs = 0.

      pdvas(1) = subp(j)
      pdvas(2) = snofall
      pdvas(3) = snomlt
      pdvas(4) = aird(j)
      pdvas(5) = pet_day
      pdvas(6) = etday
      pdvas(7) = sol_cnsw(j)
      pdvas(8) = sol_sw(j)
      pdvas(9) = sepbtm(j)
      pdvas(10) = rchrg(j)
      pdvas(11) = gwseep
      pdvas(12) = revapday
      pdvas(13) = shallirr(j)
      pdvas(14) = deepirr(j)
      pdvas(15) = shallst(j)
      pdvas(16) = deepst(j)
      pdvas(17) = surfq(j)
      pdvas(18) = qday + tloss
      pdvas(19) = tloss
      pdvas(20) = latq(j)
      pdvas(21) = gw_q(j)
      pdvas(22) = qdr(j)
      pdvas(23) = cnday(j)
      pdvas(24) = tmpav(j)
      pdvas(25) = tmx(j)
      pdvas(26) = tmn(j)
      pdvas(27) = sol_tmp(2,j)
      pdvas(28) = hru_ra(j)
      pdvas(29) = sedyld(j) / hru_ha(j)
      pdvas(30) = usle
      pdvas(31) = fertn
      pdvas(32) = fertp
      pdvas(33) = auton
      pdvas(34) = autop
      pdvas(35) = grazn
      pdvas(36) = grazp
      pdvas(37) = cfertn
      pdvas(38) = cfertp
      pdvas(39) = no3pcp
      pdvas(40) = fixn
      pdvas(41) = rmn2tl
      pdvas(42) = hmntl
      pdvas(43) = rwntl
      pdvas(44) = rmptl
      pdvas(45) = hmptl
      pdvas(46) = rmp1tl
      pdvas(47) = roctl
      pdvas(48) = wdntl
      pdvas(49) = nplnt(j)
      pdvas(50) = pplnt(j)
      pdvas(51) = sedorgn(j)
      pdvas(52) = sedorgp(j)
      pdvas(53) = sedminpa(j) + sedminps(j)
      pdvas(54) = surqno3(j)
      pdvas(55) = latno3(j)
      pdvas(56) = percn(j)
      pdvas(57) = no3gw(j)
      pdvas(58) = surqsolp(j)
      pdvas(59) = minpgw(j)
      pdvas(60) = (1.-strsw(j))
      pdvas(61) = (1.-strstmp(j))
      pdvas(62) = (1.-strsn(j))
      pdvas(63) = (1.-strsp(j))
      pdvas(64) = bio_ms(j)
      pdvas(65) = laiday(j)
      pdvas(66) = yield
      yield = 0.
      pdvas(67) = bactrop + bactsedp
      pdvas(68) = bactrolp + bactsedlp
      pdvas(69) = wtab(j)   !! based on 30 day antecedent climate (mm) (prec,et)
      pdvas(70) = wtabelo   !! based on depth from soil surface (mm)
!!    added current snow content in the hru (not summed)
      pdvas(71) = sno_hru(j)

!!    added current soil carbon for first layer
      pdvas(72) = cmup_kgh(j)  !! first soil layer only
!!    added current soil carbon integrated - aggregating all soil layers
      pdvas(73) = cmtot_kgh(j)
      
!!    adding qtile to output.hru write 3/2/2010 gsm
      pdvas(74) = qtile
!    tileno3 - output.hru
      pdvas(75) = tileno3(j)
!    latno3 - output.hru
      pdvas(76) = latno3(j)   

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
           write (28,1001) cropname, j, subnum(j), hruno(j), sb,        &
     &               nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots)

!!    added for binary files 3/25/09 gsm line below and write (33333
	      if (ia_b == 1) then
	        write (33333) j, hrugis(j), sb,                           
     *               nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots)
	      endif
        else if (isproj == 1) then
        write (21,1000) cropname, j, subnum(j), hruno(j), sb,           &
     &               nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots)
        else if (iscen == 1 .and. isproj == 2) then
        write (28,1000) cropname, j, subnum(j), hruno(j), sb,           &
     &  nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
        end if
      else
        if (iscen == 1 .and. isproj == 0) then
        write (28,1000) cropname, j, subnum(j), hruno(j), sb,           &
     &              nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
!!    added for binary files 3/25/09 gsm line below and write (33333
	    if (ia_b == 1) then
             write (33333)  j, hrugis(j), sb,                           &
     &              nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
	    endif

        else if (isproj == 1) then
        write (21,1000) cropname, j, subnum(j), hruno(j), sb,           &
     &              nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
        else if (iscen == 1 .and. isproj == 2) then
        write (28,1000) cropname, j, subnum(j), hruno(j), sb,           &
     &  nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
        end if
      end if

      return

1000  format (a4,i5,1x,a5,a7,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,8e10.3,1x,i4)
1001  format (a4,i5,1x,a5,a7,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,8e10.3)
      end
