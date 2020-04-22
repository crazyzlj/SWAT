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
      real*8, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname

      !!by zhang print out soil water
      !!===============================    
      integer :: ly
      real*8 :: sumwater, sumwfsc, sumdepth, sat, wc, dp
      real*8 :: ssoilwater(100), swfsc(100)
      real*8 :: soilwater(11), wfsc(11), sum_depth(11) !10, 100, 200, 300, 400, ..., 1000 mm
      !!by zhang print out soil water
      !!===============================


      !!by zhang print out soil water
      !!===============================
      if (cswat == 2) then
          !fc = sol_fc(kk,j) + sol_wpmm(kk,j)  ! units mm
          !wc = sol_st(kk,j) + sol_wpmm(kk,j)  ! units mm
          !sat = sol_ul(kk,j) + sol_wpmm(kk,j) ! units mm
          !void = sol_por(kk,j) * (1. - wc / sat)   ! fraction

          soilwater(1) = 0.
          wfsc(1) = 0.
          sum_depth(1) = 10.
          do k = 2, 11
            soilwater(k) = 0.
            wfsc(k) = 0.
            sum_depth(k) = 100. * (k -1)
          end do
          
          wc = sol_st(1,ihru) + sol_wpmm(1,ihru)
          sat = sol_ul(1,ihru) + sol_wpmm(1,ihru)
          soilwater(1) = wc      
          wfsc(1) = sol_por(1,ihru) * (wc / sat)   ! fraction
          
          if (sol_nly(ihru) .ge. 2) then
              do k = 2, 11
                sumwater = 0.
                sumwfsc = 0.
                sumdepth = 0.
                do ly = 2, sol_nly(ihru)
                    if (sol_z(ly-1,ihru) .ge. sum_depth(k-1) .and. sol_z(ly,ihru) .le. sum_depth(k)) then

                              dp = sol_z(ly,ihru) - sol_z(ly-1,ihru)
                              if (dp .gt. 0.) then
                                  wc = sol_st(ly,ihru) + sol_wpmm(ly,ihru)*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  sat = sol_ul(ly,ihru) + sol_wpmm(ly,ihru)*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + sol_por(ly,ihru) * (wc / sat) * dp
                                  sumdepth = sumdepth + dp
                              end if
                    
                    elseif ((sol_z(ly-1,ihru) .gt. sum_depth(k-1) .and. sol_z(ly,ihru) .gt. sum_depth(k)) &
                            .or. (sol_z(ly-1,ihru) .ge. sum_depth(k-1) .and. sol_z(ly,ihru) .gt. sum_depth(k)) &
                            .or. (sol_z(ly-1,ihru) .gt. sum_depth(k-1) .and. sol_z(ly,ihru) .ge. sum_depth(k))) &
                             then
                            if (sol_z(ly-1,ihru) .le. sum_depth(k)) then 
                              dp = sum_depth(k) - sol_z(ly-1,ihru)
                              if (dp .gt. 0.) then
                                  wc = (sol_st(ly,ihru) + sol_wpmm(ly,ihru)) *(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  sat = (sol_ul(ly,ihru) + sol_wpmm(ly,ihru)) *(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + sol_por(ly,ihru) * (wc / sat) * dp
                                  sumdepth = sumdepth + dp
                              end if
                            end if
                    elseif ((sol_z(ly-1,ihru) .lt. sum_depth(k-1) .and. sol_z(ly,ihru) .lt. sum_depth(k)) & 
                            .or. (sol_z(ly-1,ihru) .le. sum_depth(k-1) .and. sol_z(ly,ihru) .lt. sum_depth(k)) & 
                            .or. (sol_z(ly-1,ihru) .lt. sum_depth(k-1) .and. sol_z(ly,ihru) .le. sum_depth(k))) &
                             then
                            if (sol_z(ly,ihru) .ge. sum_depth(k-1)) then
                              dp = sol_z(ly,ihru) - sum_depth(k-1)
                              if (dp .gt. 0.) then
                                  wc = (sol_st(ly,ihru) + sol_wpmm(ly,ihru))*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  sat = (sol_ul(ly,ihru) + sol_wpmm(ly,ihru)) *(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + sol_por(ly,ihru) * (wc / sat) * dp
                                  sumdepth = sumdepth + dp
                              end if
                            end if
                    
                    elseif ((sol_z(ly-1,ihru) .lt. sum_depth(k-1) .and. sol_z(ly,ihru) .gt. sum_depth(k)) & 
                             .or. (sol_z(ly-1,ihru) .le. sum_depth(k-1) .and. sol_z(ly,ihru) .gt. sum_depth(k)) & 
                             .or. (sol_z(ly-1,ihru) .lt. sum_depth(k-1) .and. sol_z(ly,ihru) .ge. sum_depth(k))) &
                              then 
                              dp = sum_depth(k) - sum_depth(k-1)
                              if (dp .gt. 0.) then
                                  wc = (sol_st(ly,ihru) + sol_wpmm(ly,ihru))*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  sat = (sol_ul(ly,ihru) + sol_wpmm(ly,ihru))*(dp/(sol_z(ly,ihru)-sol_z(ly-1,ihru)))
                                  
                                  
                                  sumwater = sumwater + wc * dp
                                  sumwfsc = sumwfsc + sol_por(ly,ihru) * (wc / sat) * dp
                                  sumdepth = sumdepth + dp    
                              end if
                    end if
                end do !!End lyr
                
                if (sumdepth .gt. 0.) then
                      soilwater(k) = sumwater / sumdepth     
                      wfsc(k) = sumwfsc / sumdepth   ! fraction                
                end if
                
              end do !!end k
              
              
          end if
      end if
      !!by zhang print out soil water
      !!===============================


      j = 0
      j = ihru
      sb = hru_sub(j)
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
      pdvas(18) = qday
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
      pdvas(64) = bio_ms(j) / 1000.
      pdvas(65) = laiday(j)
      pdvas(66) = yield
      yield = 0.
      pdvas(67) = bactrop + bactsedp
      pdvas(68) = bactrolp + bactsedlp
      pdvas(69) = wtab(j)   !! based on 30 day antecedent climate (mm) (prec,et)
!      pdvas(70) = wtabelo   !! based on depth from soil surface (mm)
      pdvas(70) = wat_tbl(j)   !! based on depth from soil surface (mm): Dmoriasi 4/08/2014
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
!    groundwater deep
      pdvas(77) = gw_qdeep(j)
      pdvas(78) = latq(j) - lpndloss - lwetloss
!!    phos due to crack flow (tvap)
      pdvas(79) = vap_tile

      call xmon 
          
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
        if (icalen == 0) write (28,1001) cropname, j, subnum(j),        &
     &      hruno(j), sb, nmgt(j), iida, hru_km(j),                     &
     &       (pdvs(ii), ii = 1, itots)
        if (icalen == 1) write (28,1002) cropname, j, subnum(j),        &
     &      hruno(j), sb, nmgt(j), i_mo, icl(iida), iyr, hru_km(j),     &
     &       (pdvs(ii), ii = 1, itots)
1002  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i2,1x,i2,1x,i4,1x,e10.5,       &
     & 66f10.3,1x,e10.5,1x,e10.5,8e10.3,3f10.3)
      
!!    added for binary files 3/25/09 gsm line below and write (33333
	      if (ia_b == 1) then
	        write (33333) j, hrugis(j), sb,                                  &
     &               nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots)
	      endif
        else if (isproj == 1) then
        write (21,1000) cropname, j, subnum(j), hruno(j), sb,           &
     &          nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots)
        else if (iscen == 1 .and. isproj == 2) then
        if(icalen == 0)write (28,1000) cropname, j, subnum(j), hruno(j),&
     &      sb, nmgt(j), iida, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
        if(icalen == 1)write (28,1003) cropname, j, subnum(j), hruno(j),&
     &      sb, nmgt(j), i_mo, icl(iida), iyr, hru_km(j),               &
     &      (pdvs(ii), ii = 1, itots), iyr
1003  format(a4,i5,1x,a5,a4,i5,1x,i4,1x,i2,1x,i2,1x,i4,1x,e10.5,66f10.3,&
     &1x,e10.5,1x,e10.5,8e10.3,f10.3,1x,i4)
        end if
      else
        if (iscen == 1 .and. isproj == 0) then
        if(icalen == 0)write (28,1000) cropname, j, subnum(j), hruno(j),&
     &        sb,nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
        if(icalen == 1)write (28,1003) cropname, j, subnum(j), hruno(j),&
     &        sb,nmgt(j), i_mo, icl(iida), iyr, hru_km(j),              &
     &        (pdvas(ii), ii = 1, mhruo)
!!    added for binary files 3/25/09 gsm line below and write (33333
	    if (ia_b == 1) then
             write (33333)  j, hrugis(j), sb,                           &
     &              nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
	    endif

        else if (isproj == 1) then
        write (21,1000) cropname, j, subnum(j), hruno(j), sb,           &
     &              nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo)
        else if (iscen == 1 .and. isproj == 2) then
        if(icalen == 0)write (28,1000) cropname, j, subnum(j), hruno(j),& 
     &      sb,nmgt(j), iida, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
         if(icalen == 1)write(28,1000) cropname, j, subnum(j), hruno(j),& 
     &      sb,nmgt(j), i_mo, icl(iida), iyr, hru_km(j),                &
     &      (pdvas(ii), ii = 1, mhruo), iyr
        end if
      end if


      !!add by zhang
      !!output carbon related variables
      !!=================================
      if (cswat == 2) then
          if (j == 1) then
          tot_mass = 0.
          tot_cmass = 0.
          tot_nmass = 0.
          tot_LSC = 0.
          tot_LMC = 0.
          tot_HSC = 0.
          tot_HPC = 0.
          tot_BMC = 0.
          tot_pmass = 0. 
          tot_solp = 0.
          tot_no3_nh3 =0.
          do k=1,sol_nly(j) 
              sol_mass = 0.
              if (k == 1) then
 		        sol_mass = (10) / 1000.* 10000. * sol_bd(k,j)* 1000. * &
     	         (1- sol_rock(k,j) / 100.)            
              else
		        sol_mass = (sol_z(k,j) - sol_z(k-1,j)) / 1000.* 10000. &
     	         * sol_bd(k,j)* 1000. *	(1- sol_rock(k,j) / 100.)
	         end if       
          sol_cmass = 0.
          sol_cmass = sol_LSC(k,j)+sol_LMC(k,j)+sol_HPC(k,j)+sol_HSC(k,j) &
                   +sol_BMC(k,j)
          sol_nmass = 0. 
          sol_nmass = sol_LSN(k,j)+sol_LMN(k,j)+sol_HPN(k,j)+sol_HSN(k,j) &
                   +sol_BMN(k,j)     
          write (98,9000) iyr, i, k, j, sol_mass,sol_cmass,             &
             sol_nmass,sol_LS(k,j),sol_LM(k,j),                         &
             sol_LSC(k,j),sol_LMC(k,j),sol_HSC(k,j),sol_HPC(k,j),       &
             sol_BMC(k,j),sol_LSN(k,j),sol_LMN(k,j),sol_HPN(k,j),       &
             sol_HSN(k,j),sol_BMN(k,j),sol_no3(k,j),sol_fop(k,j),       &
             sol_orgp(k,j),sol_solp(k,j)   
         
           tot_mass = tot_mass + sol_mass
           tot_cmass = tot_cmass + sol_cmass 
           tot_nmass = tot_nmass + sol_nmass
           tot_LSC = tot_LSC + sol_LSC(k,j)
           tot_LMC = tot_LMC + sol_LMC(k,j)
           tot_HSC = tot_HSC + sol_HSC(k,j)
           tot_HPC = tot_HPC + sol_HPC(k,j)
           tot_BMC = tot_BMC + sol_BMC(k,j)
           tot_pmass =tot_pmass+ sol_orgp(k,j) + sol_fop(k,j)                       &
            +  sol_solp(k,j)
           tot_solp = tot_solp + sol_solp(k,j)
           
           tot_no3_nh3 = tot_no3_nh3  + sol_no3(k,j) + sol_nh3(k,j)
          end do      

          write (1001,9001) iyr, i, j, rsdc_d(j), sedc_d(j), percc_d(j),             &
              latc_d(j),emitc_d(j), grainc_d(j), surfqc_d(j), stoverc_d(j),         &
              NPPC_d(j), foc_d(j),rspc_d(j),tot_mass,tot_cmass,tot_nmass,           &
              tot_LSC,tot_LMC,tot_HSC,tot_HPC,tot_BMC,                              &
              bio_ms(j)*0.42, rwt(j), tot_no3_nh3,wdntl,etday,tillage_factor(j),    &
              (soilwater(ii), ii = 1, 11), (wfsc(ii), ii = 1, 11)     
          end if  
      end if
      !!add by zhang
      !!output carbon related variables
      !!=================================

      return

1000  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,e10.5,1x,e10.5,8e10.3,3f10.3,1x,i4)
1001  format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,e10.5,1x,e10.5,8e10.3,3f10.3)
9000  format(i4,i4,i2,i8,21(f16.3))
9001  format(i4,i4,i8,48(f16.3))
      end