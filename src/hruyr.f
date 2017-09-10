      subroutine hruyr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes annual HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_yrms(:)   |metric tons/ha|annual biomass (dry weight) in the HRU
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    deepst(:)     |mm H2O        |depth of water in deep aquifer
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_sub(:)    |none          |subbasin in which HRU is located
!!    hrugis(:)     |none          |GIS code printed to output files(output.hru,.rch)
!!    hruyro(1,:)   |mm H2O        |precipitation in HRU during year
!!    hruyro(2,:)   |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow in HRU during year
!!    hruyro(3,:)   |mm H2O        |amount of snow melt in HRU during year
!!    hruyro(4,:)   |mm H2O        |amount of surface runoff to main channel
!!                                 |from HRU during year (ignores impact of
!!                                 |transmission losses)
!!    hruyro(5,:)   |mm H2O        |amount of lateral flow contribution to main
!!                                 |channel from HRU during year
!!    hruyro(6,:)   |mm H2O        |amount of groundwater flow contribution to
!!                                 |main channel from HRU during year
!!    hruyro(7,:)   |mm H2O        |amount of water moving from shallow aquifer
!!                                 |to plants or soil profile in HRU during year
!!    hruyro(8,:)   |mm H2O        |amount of water recharging deep aquifer in
!!                                 |HRU during year
!!    hruyro(9,:)   |mm H2O        |total amount of water entering both aquifers
!!                                 |from HRU during year
!!    hruyro(10,:)  |mm H2O        |water yield (total amount of water entering
!!                                 |main channel) from HRU during year
!!    hruyro(11,:)  |mm H2O        |amount of water percolating out of the soil
!!                                 |profile and into the vadose zone in HRU
!!                                 |during year
!!    hruyro(12,:)  |mm H2O        |actual evapotranspiration in HRU during year
!!    hruyro(13,:)  |mm H2O        |amount of transmission losses from tributary
!!                                 |channels in HRU for year
!!    hruyro(14,:)  |metric tons/ha|sediment yield from HRU for year
!!    hruyro(17,:)  |kg N/ha       |amount of nitrogen applied in continuous 
!!                                 |fertilizer operation during year in HRU 
!!    hruyro(18,:)  |kg P/ha       |amount of phosphorus applied in continuous
!!                                 |fertilizer operation during year in HRU
!!    hruyro(23,:)  |mm H2O        |amount of water removed from shallow aquifer
!!                                 |in HRU for irrigation during year
!!    hruyro(24,:)  |mm H2O        |amount of water removed from deep aquifer
!!                                 |in HRU for irrigation during year
!!    hruyro(25,:)  |mm H2O        |potential evapotranspiration in HRU during
!!                                 |year
!!    hruyro(26,:)  |kg N/ha       |annual amount of N (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hruyro(27,:)  |kg P/ha       |annual amount of P (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hruyro(28,:)  |kg N/ha       |annual amount of N (organic & mineral)
!!                                 |auto-applied in HRU
!!    hruyro(29,:)  |kg P/ha       |annual amount of P (organic & mineral)
!!                                 |auto-applied in HRU
!!    hruyro(31,:)  |stress days   |water stress days in HRU during year
!!    hruyro(32,:)  |stress days   |temperature stress days in HRU during year
!!    hruyro(33,:)  |stress days   |nitrogen stress days in HRU during year
!!    hruyro(34,:)  |stress days   |phosphorus stress days in HRU during year
!!    hruyro(35,:)  |kg N/ha       |organic nitrogen in surface runoff in HRU
!!                                 |during year
!!    hruyro(36,:)  |kg P/ha       |organic phosphorus in surface runoff in HRU
!!                                 |during year
!!    hruyro(37,:)  |kg N/ha       |nitrate in surface runoff in HRU during year
!!    hruyro(38,:)  |kg N/ha       |nitrate in lateral flow in HRU during year
!!    hruyro(39,:)  |kg P/ha       |soluble phosphorus in surface runoff in HRU
!!                                 |during year
!!    hruyro(40,:)  |kg N/ha       |amount of nitrogen removed from soil by plant
!!                                 |uptake in HRU during year
!!    hruyro(41,:)  |kg N/ha       |nitrate percolating past bottom of soil
!!                                 |profile in HRU during year
!!    hruyro(42,:)  |kg P/ha       |amount of phosphorus removed from soil by
!!                                 |plant uptake in HRU during year
!!    hruyro(43,:)  |kg P/ha       |amount of phosphorus moving from labile
!!                                 |mineral to active mineral pool in HRU during
!!                                 |year
!!    hruyro(44,:)  |kg P/ha       |amount of phosphorus moving from active
!!                                 |mineral to stable mineral pool in HRU during
!!                                 |year
!!    hruyro(45,:)  |kg N/ha       |amount of nitrogen applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |year
!!    hruyro(46,:)  |kg P/ha       |amount of phosphorus applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |year
!!    hruyro(47,:)  |kg N/ha       |amount of nitrogen added to soil by fixation
!!                                 |in HRU during year
!!    hruyro(48,:)  |kg N/ha       |amount of nitrogen lost by denitrification
!!                                 |in HRU during year
!!    hruyro(49,:)  |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to nitrate pool in HRU during year
!!    hruyro(50,:)  |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to stable organic pool in HRU during year
!!    hruyro(51,:)  |kg P/ha       |amount of phosphorus moving from organic to
!!                                 |labile mineral pool in HRU during year
!!    hruyro(52,:)  |kg N/ha       |amount of nitrogen moving from fresh organic
!!                                 |to nitrate and active organic pools in HRU
!!                                 |during year
!!    hruyro(53,:)  |kg P/ha       |amount of phosphorus moving from fresh
!!                                 |organic to the labile mineral and organic
!!                                 |pools in HRU during year
!!    hruyro(54,:)  |kg N/ha       |amount of nitrogen added to soil in rain
!!                                 |during year
!!    hruyro(61,:)  |metric tons/ha|daily soil loss predicted with USLE equation
!!    hruyro(63,:)  |# bacteria/ha |less persistent bacteria transported to main
!!                                 |channel from HRU during year
!!    hruyro(64,:)  |# bacteria/ha |persistent bacteria transported to main
!!                                 |channel from HRU during year
!!    hruyro(65,:)  |kg N/ha       |nitrate loading from groundwater in HRU to
!!                                 |main channel during year
!!    hruyro(66,:)  |kg P/ha       |soluble P loading from groundwater in HRU to
!!                                 |main channel during year
!!    hruyro(67,:)  |kg P/ha       |loading of mineral P attached to sediment
!!                                 |in HRU to main channel during year
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    idplt(:)      |none          |land cover code from crop.dat
!!    ipdvas(:)     |none          |output variable codes for output.hru file
!!    isproj        |none          |special project code:
!!                                 |1 test rewind (run simulation twice)
!!    itots         |none          |number of output variables printed (output.hru)
!!    iyr           |year          |current year of simulation (eg 1980)
!!    lai_yrmx(:)   |none          |maximum leaf area index for the year in the
!!                                 |HRU
!!    mhruo         |none          |maximum number of variables written to
!!                                 |HRU output file (output.hru)
!!    nhru          |none          |number of HRUs in watershed
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    nro(:)        |none          |sequence number of year in rotation
!!    shallst(:)    |mm H2O        |depth of water in shallow aquifer
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |on any given day
!!    yldanu(:)     |metric tons/ha|annual yield (dry weight) in the HRU
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

      do j = 1, nhru
        iflag = 0
	    sb = hru_sub(j)
	  
        do ii = 1, itoth
          if (ipdhru(ii) == j) iflag = 1
        end do

        if (iflag == 1) then

        pdvas = 0.
        pdvs = 0.

        pdvas(1) = hruyro(1,j)
        pdvas(2) = hruyro(2,j)
        pdvas(3) = hruyro(3,j)
        pdvas(4) = hruyro(22,j)
        pdvas(5) = hruyro(25,j)
        pdvas(6) = hruyro(12,j)
        pdvas(7) = hruyro(21,j) / Real(366 - leapyr)
        pdvas(8) = sol_sw(j)
        pdvas(9) = hruyro(11,j)
        pdvas(10) = hruyro(9,j)
        pdvas(11) = hruyro(8,j)
        pdvas(12) = hruyro(7,j)
        pdvas(13) = hruyro(23,j)
        pdvas(14) = hruyro(24,j)
        pdvas(15) = shallst(j)
        pdvas(16) = deepst(j)
        pdvas(17) = hruyro(19,j)
        pdvas(18) = hruyro(4,j)
        pdvas(19) = hruyro(13,j)
        pdvas(20) = hruyro(5,j)
        pdvas(21) = hruyro(6,j)
        pdvas(22) = hruyro(10,j)
        pdvas(23) = hruyro(20,j) / Real(366 - leapyr)
        pdvas(24) = hruyro(57,j) / Real(366 - leapyr)
        pdvas(25) = hruyro(55,j) / Real(366 - leapyr)
        pdvas(26) = hruyro(56,j) / Real(366 - leapyr)
        pdvas(27) = hruyro(30,j) / Real(366 - leapyr)
        pdvas(28) = hruyro(58,j) / Real(366 - leapyr)
        pdvas(29) = hruyro(14,j)
        pdvas(30) = hruyro(61,j)
        pdvas(31) = hruyro(45,j)
        pdvas(32) = hruyro(46,j)
        pdvas(33) = hruyro(28,j)
        pdvas(34) = hruyro(29,j)
        pdvas(35) = hruyro(26,j)
        pdvas(36) = hruyro(27,j)
        pdvas(37) = hruyro(17,j)
        pdvas(38) = hruyro(18,j)
        pdvas(39) = hruyro(54,j)
        pdvas(40) = hruyro(47,j)
        pdvas(41) = hruyro(52,j)
        pdvas(42) = hruyro(49,j)
        pdvas(43) = hruyro(50,j)
        pdvas(44) = hruyro(53,j)
        pdvas(45) = hruyro(51,j)
        pdvas(46) = hruyro(43,j)
        pdvas(47) = hruyro(44,j)
        pdvas(48) = hruyro(48,j)
        pdvas(49) = hruyro(40,j)
        pdvas(50) = hruyro(42,j)
        pdvas(51) = hruyro(35,j)
        pdvas(52) = hruyro(36,j)
        pdvas(53) = hruyro(67,j)
        pdvas(54) = hruyro(37,j)
        pdvas(55) = hruyro(38,j)
        pdvas(56) = hruyro(41,j)
        pdvas(57) = hruyro(65,j)
        pdvas(58) = hruyro(39,j)
        pdvas(59) = hruyro(66,j)
        pdvas(60) = hruyro(31,j)
        pdvas(61) = hruyro(32,j)
        pdvas(62) = hruyro(33,j)
        pdvas(63) = hruyro(34,j)
        pdvas(64) = bio_yrms(j)
        pdvas(65) = lai_yrmx(j)
        pdvas(66) = yldanu(j)
        pdvas(67) = hruyro(63,j)
        pdvas(68) = hruyro(64,j)
        pdvas(69) = wtab(j)  !! based on 30 day antecedent climate(mm) (prec,et)
!       pdvas(70) = wtabelo   !! based on depth from soil surface (mm)
        pdvas(70) = wat_tbl(j)   !! based on depth from soil surface (mm): Dmoriasi 4/08/2014
!!      added current snow content in the hru (not summed)
        pdvas(71) = sno_hru(j)

!!      added current soil carbon for first layer
        pdvas(72) = cmup_kgh(j)    !! first soil layer only
!!      added current soil carbon integrated - ggreagating all soil layers
        pdvas(73) = cmtot_kgh(j)
        
!!    adding qtile to output.hru write 3/2/2010 gsm
        pdvas(74) = hruyro(62,j)
!!    tileno3 - output.hru
        pdvas(75) = hruyro(68,j)
!!    latno3 - output.hru
        pdvas(76) = hruyro(69,j)
!!    gwq deep
        pdvas(77) = hruyro(70,j)
!!    latq contribution
        pdvas(78) = hruyro(71,j)
!!      phos due to crack flow (tvap)
        pdvas(79) = hruyro(72,j)

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
     &             nmgt(j), iyr, hru_km(j), (pdvs(ii), ii = 1, itots)
          else if (isproj == 1) then
          write (21,1000) cropname, j, subnum(j), hruno(j),             
     &            sb, nmgt(j), iyr, hru_km(j), (pdvs(ii), ii = 1, itots)
          else if (iscen == 1 .and. isproj == 2) then
          write (28,2000) cropname, j, subnum(j), hruno(j), sb,         
     &    nmgt(j), iyr, hru_km(j), (pdvs(ii), ii = 1, itots), iyr
          endif
        else
          if (iscen == 1 .and. isproj == 0) then
          write (28,1001) cropname, j, subnum(j), hruno(j), sb,         
     &            nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, mhruo)
          else if (isproj == 1) then
          write (21,1001) cropname, j, subnum(j), hruno(j),             
     &           sb, nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, mhruo)
          else if (iscen == 1 .and. isproj == 2) then
          write (28,1001) cropname, j, subnum(j), hruno(j), sb,         
     &    nmgt(j), iyr, hru_km(j), (pdvas(ii), ii = 1, mhruo), iyr
          endif
        end if
        end if
      end do

      return

 1000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,8e10.3,6f10.3)
 2000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,5e10.3,6f10.3,1x,i4)
 1001 format (a4,i7,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,3e10.3,6f10.3,1x,i4)      
      end