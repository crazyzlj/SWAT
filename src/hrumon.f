      subroutine hrumon

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly HRU output to the output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)     |kg/ha         |land cover/crop biomass (dry weight)
!!    cpnm(:)       |NA            |four character code to represent crop name
!!    deepst(:)     |mm H2O        |depth of water in deep aquifer
!!    hru_km(:)     |km^2          |area of HRU in square kilometers
!!    hru_sub(:)    |none          |subbasin in which HRU is located
!!    hrugis(:)     |none          |GIS code printed to output files(output.hru,.rch)
!!    hvstiadj(:)   |(kg/ha)/(kg/ha)|optimal harvest index for current time
!!                                 |during growing season
!!    icr(:)        |none          |sequence number of crop grown within the
!!                                 |current year
!!    idplt(:)      |none          |land cover code from crop.dat
!!    ipdvas(:)     |none          |output variable codes for output.hru file
!!    isproj        |none          |special project code:
!!                                 |1 test rewind (run simulation twice)
!!    itots         |none          |number of output variables printed (output.hru)
!!    mo_chk        |none          |current month of simulation
!!    mhruo         |none          |maximum number of variables written to
!!                                 |HRU output file (output.hru)
!!    nhru          |none          |number of HRUs in watershed
!!    nmgt(:)       |none          |management code (for GIS output only)
!!    nro(:)        |none          |sequence number of year in rotation
!!    rwt(:)        |none          |fraction of total plant biomass that is
!!                                 |in roots
!!    shallst(:)    |mm H2O        |depth of water in shallow aquifer
!!    sol_sw(:)     |mm H2O        |amount of water stored in the soil profile
!!                                 |on any given day
!!    hrumono(1,:)  |mm H2O        |precipitation in HRU during month
!!    hrumono(2,:)  |mm H2O        |amount of precipitation falling as freezing
!!                                 |rain/snow in HRU during month
!!    hrumono(3,:)  |mm H2O        |amount of snow melt in HRU during month
!!    hrumono(4,:)  |mm H2O        |amount of surface runoff to main channel
!!                                 |from HRU during month (ignores impact of
!!                                 |transmission losses)
!!    hrumono(5,:)  |mm H2O        |amount of lateral flow contribution to main
!!                                 |channel from HRU during month
!!    hrumono(6,:)  |mm H2O        |amount of groundwater flow contribution to
!!                                 |main channel from HRU during month
!!    hrumono(7,:)  |mm H2O        |amount of water moving from shallow aquifer
!!                                 |to plants or soil profile in HRU during month
!!    hrumono(8,:)  |mm H2O        |amount of water recharging deep aquifer in
!!                                 |HRU during month
!!    hrumono(9,:)  |mm H2O        |total amount of water entering both aquifers
!!                                 |from HRU during month
!!    hrumono(10,:) |mm H2O        |water yield (total amount of water entering
!!                                 |main channel) from HRU during month
!!    hrumono(11,:) |mm H2O        |amount of water percolating out of the soil
!!                                 |profile and into the vadose zone in HRU
!!                                 |during month
!!    hrumono(12,:) |mm H2O        |actual evapotranspiration in HRU during month
!!    hrumono(13,:) |mm H2O        |amount of transmission losses from tributary
!!                                 |channels in HRU for month
!!    hrumono(14,:) |metric tons/ha|sediment yield from HRU for month
!!    hrumono(17,:) |kg N/ha       |amount of nitrogen applied in continuous 
!!                                 |fertilizer operation during month in HRU 
!!    hrumono(18,:) |kg P/ha       |amount of phosphorus applied in continuous
!!                                 |fertilizer operation during month in HRU
!!    hrumono(23,:) |mm H2O        |amount of water removed from shallow aquifer
!!                                 |in HRU for irrigation during month
!!    hrumono(24,:) |mm H2O        |amount of water removed from deep aquifer
!!                                 |in HRU for irrigation during month
!!    hrumono(25,:) |mm H2O        |potential evapotranspiration in HRU during
!!                                 |month
!!    hrumono(26,:) |kg N/ha       |monthly amount of N (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hrumono(27,:) |kg P/ha       |monthly amount of P (organic & mineral)
!!                                 |applied in HRU during grazing
!!    hrumono(28,:) |kg N/ha       |monthly amount of N (organic & mineral)
!!                                 |auto-applied in HRU
!!    hrumono(29,:) |kg P/ha       |monthly amount of P (organic & mineral)
!!                                 |auto-applied in HRU
!!    hrumono(31,:) |stress days   |water stress days in HRU during month
!!    hrumono(32,:) |stress days   |temperature stress days in HRU during month
!!    hrumono(33,:) |stress days   |nitrogen stress days in HRU during month
!!    hrumono(34,:) |stress days   |phosphorus stress days in HRU during month
!!    hrumono(35,:) |kg N/ha       |organic nitrogen in surface runoff in HRU
!!                                 |during month
!!    hrumono(36,:) |kg P/ha       |organic phosphorus in surface runoff in HRU
!!                                 |during month
!!    hrumono(37,:) |kg N/ha       |nitrate in surface runoff in HRU during month
!!    hrumono(38,:) |kg N/ha       |nitrate in lateral flow in HRU during month
!!    hrumono(39,:) |kg P/ha       |soluble phosphorus in surface runoff in HRU
!!                                 |during month
!!    hrumono(40,:) |kg N/ha       |amount of nitrogen removed from soil by plant
!!                                 |uptake in HRU during month
!!    hrumono(41,:) |kg N/ha       |nitrate percolating past bottom of soil
!!                                 |profile in HRU during month
!!    hrumono(42,:) |kg P/ha       |amount of phosphorus removed from soil by
!!                                 |plant uptake in HRU during month
!!    hrumono(43,:) |kg P/ha       |amount of phosphorus moving from labile
!!                                 |mineral to active mineral pool in HRU during
!!                                 |month
!!    hrumono(44,:) |kg P/ha       |amount of phosphorus moving from active
!!                                 |mineral to stable mineral pool in HRU during
!!                                 |month
!!    hrumono(45,:) |kg N/ha       |amount of nitrogen applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |month
!!    hrumono(46,:) |kg P/ha       |amount of phosphorus applied to HRU in
!!                                 |fertilizer and grazing operations during 
!!                                 |month
!!    hrumono(47,:) |kg N/ha       |amount of nitrogen added to soil by fixation
!!                                 |in HRU during month
!!    hrumono(48,:) |kg N/ha       |amount of nitrogen lost by denitrification
!!                                 |in HRU during month
!!    hrumono(49,:) |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to nitrate pool in HRU during month
!!    hrumono(50,:) |kg N/ha       |amount of nitrogen moving from active organic
!!                                 |to stable organic pool in HRU during month
!!    hrumono(51,:) |kg P/ha       |amount of phosphorus moving from organic to
!!                                 |labile mineral pool in HRU during month
!!    hrumono(52,:) |kg N/ha       |amount of nitrogen moving from fresh organic
!!                                 |to nitrate and active organic pools in HRU
!!                                 |during month
!!    hrumono(53,:) |kg P/ha       |amount of phosphorus moving from fresh
!!                                 |organic to the labile mineral and organic
!!                                 |pools in HRU during month
!!    hrumono(54,:) |kg N/ha       |amount of nitrogen added to soil in rain
!!    hrumono(61,:) |metric tons/ha|daily soil loss predicted with USLE equation
!!    hrumono(63,:) |# bacteria/ha |less persistent bacteria transported to main
!!                                 |channel from HRU during month
!!    hrumono(64,:) |# bacteria/ha |persistent bacteria transported to main
!!                                 |channel from HRU during month
!!    hrumono(65,:) |kg N/ha       |nitrate loading from groundwater in HRU to
!!                                 |main channel during month
!!    hrumono(66,:) |kg P/ha       |soluble P loading from groundwater in HRU to
!!                                 |main channel during month
!!    hrumono(67,:) |kg P/ha       |loading of mineral P attached to sediment
!!                                 |in HRU to main channel during month
!!    laiday(:)     |none          |leaf area index for HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    days        |none          |number of days in month
!!    dmt         |metric tons/ha|land cover/crop biomass (dry weight)
!!    ii          |none          |counter
!!    j           |none          |HRU number
!!    pdvas(:)    |varies        |array to hold HRU output values
!!    pdvs(:)     |varies        |array to hold selected HRU output values
!!                               |when user doesn't want to print all
!!    sb          |none          |subbasin number
!!    yldt        |metric tons/ha|land cover/crop yield (dry weight)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, sb, ii, days, iflag
      real*8 :: dmt, yldt
      real*8, dimension (mhruo) :: pdvas, pdvs
      character (len=4) :: cropname

      days = 0

      select case(mo_chk)
        case (9, 4, 6, 11)
          days = 30
        case (2)
          days = 29 - leapyr
        case default
          days = 31
      end select

      do j = 1, nhru
        sb = 0
        sb = hru_sub(j)

        iflag = 0
        do ii = 1, itoth
          if (ipdhru(ii) == j) iflag = 1
        end do

        if (iflag == 1) then

        pdvas = 0.
        pdvs = 0.

        dmt = 0.
        yldt = 0.
        dmt = bio_ms(j) / 1000.
        yldt = (1. - rwt(j)) * dmt * hvstiadj(j)

        pdvas(1) = hrumono(1,j)
        pdvas(2) = hrumono(2,j)
        pdvas(3) = hrumono(3,j)
        pdvas(4) = hrumono(22,j)
        pdvas(5) = hrumono(25,j)
        pdvas(6) = hrumono(12,j)
        pdvas(7) = hrumono(21,j) / dfloat(days)
        pdvas(8) = sol_sw(j)
        pdvas(9) = hrumono(11,j)
        pdvas(10) = hrumono(9,j)
        pdvas(11) = hrumono(8,j)
        pdvas(12) = hrumono(7,j)
        pdvas(13) = hrumono(23,j)
        pdvas(14) = hrumono(24,j)
        pdvas(15) = shallst(j)
        pdvas(16) = deepst(j)
        pdvas(17) = hrumono(19,j)
        pdvas(18) = hrumono(4,j)
        pdvas(19) = hrumono(13,j)
        pdvas(20) = hrumono(5,j)
        pdvas(21) = hrumono(6,j)
        pdvas(22) = hrumono(10,j)
        pdvas(23) = hrumono(20,j) / dfloat(days)
        pdvas(24) = hrumono(57,j) / dfloat(days)
        pdvas(25) = hrumono(55,j) / dfloat(days)
        pdvas(26) = hrumono(56,j) / dfloat(days)
        pdvas(27) = hrumono(30,j) / dfloat(days)
        pdvas(28) = hrumono(58,j) / dfloat(days)
        pdvas(29) = hrumono(14,j)
        pdvas(30) = hrumono(61,j)
        pdvas(31) = hrumono(45,j)
        pdvas(32) = hrumono(46,j)
        pdvas(33) = hrumono(28,j)
        pdvas(34) = hrumono(29,j)
        pdvas(35) = hrumono(26,j)
        pdvas(36) = hrumono(27,j)
        pdvas(37) = hrumono(17,j)
        pdvas(38) = hrumono(18,j)
        pdvas(39) = hrumono(54,j)
        pdvas(40) = hrumono(47,j)
        pdvas(41) = hrumono(52,j)
        pdvas(42) = hrumono(49,j)
        pdvas(43) = hrumono(50,j)
        pdvas(44) = hrumono(53,j)
        pdvas(45) = hrumono(51,j)
        pdvas(46) = hrumono(43,j)
        pdvas(47) = hrumono(44,j)
        pdvas(48) = hrumono(48,j)
        pdvas(49) = hrumono(40,j)
        pdvas(50) = hrumono(42,j)
        pdvas(51) = hrumono(35,j)
        pdvas(52) = hrumono(36,j)
        pdvas(53) = hrumono(67,j)
        pdvas(54) = hrumono(37,j)
        pdvas(55) = hrumono(38,j)
        pdvas(56) = hrumono(41,j)
        pdvas(57) = hrumono(65,j)
        pdvas(58) = hrumono(39,j)
        pdvas(59) = hrumono(66,j)
        pdvas(60) = hrumono(31,j)
        pdvas(61) = hrumono(32,j)
        pdvas(62) = hrumono(33,j)
        pdvas(63) = hrumono(34,j)
        pdvas(64) = dmt
        pdvas(65) = laiday(j)
        pdvas(66) = yldt
        pdvas(67) = hrumono(63,j)
        pdvas(68) = hrumono(64,j)
        pdvas(69) = wtab(j)  !! based on 30 day antecedent climate(mm) (prec,et)
!       pdvas(70) = wtabelo   !! based on depth from soil surface (mm)
        pdvas(70) = wat_tbl(j)   !! based on depth from soil surface (mm): Dmoriasi 4/08/2014
!!      added current snow content in the hru (not summed)
        pdvas(71) = sno_hru(j)

!!      added current soil carbon for first layer
        pdvas(72) = cmup_kgh(j)    !! first soil layer only
!!      added current soil carbon integrated - aggregating all soil layers
        pdvas(73) = cmtot_kgh(j)
        
!!    adding qtile to output.hru write 3/2/2010 gsm
        pdvas(74) = hrumono(62,j)
!!    tileno3 - output.hru
        pdvas(75) = hrumono(68,j)
!!    latno3 - output.hru
        pdvas(76) = hrumono(69,j)
!!    gw_qdeep 
        pdvas(77) = hrumono(70,j)
!!    latq continuous
        pdvas(78) = hrumono(71,j)
!!      phos due to crack flow (tvap)
        pdvas(79) = hrumono(72,j)

      if (itots > 0) then 
	   ix = itots
	else
         ix = mhruo
	endif


        if (ipdvas(1) > 0) then
          do ii = 1, ix
            pdvs(ii) = pdvas(ipdvas(ii))
          end do
 
          idplant = idplt(j)
          if (idplant > 0) then
            cropname = cpnm(idplant)
          else
            cropname = "NOCR"
          endif

          if (iscen == 1) then                                          
            select case (isproj)
            case (0)
            write (28,1000) cropname, j, subnum(j), hruno(j), sb,       
     &         nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
            case (1)
            write (21,1000) cropname, j, subnum(j), hruno(j),           
     &         sb, nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
            case (2)
            write (28,2000) cropname, j, subnum(j), hruno(j), sb,       
     &         nmgt(j), mo_chk, hru_km(j),(pdvs(ii), ii = 1, ix), iyr
            end select
          end if
!     else
!! write with different format for hrus greater than 9999
!        select case (isproj)
!            case (0)
!            write (28,1001) cropname, j, subnum(j), hruno(j), sb,       &
!     &         nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
!            case (1) 
!            write (21,1001) cropname, j, subnum(j), hruno(j),           &
!     &         sb, nmgt(j), mo_chk, hru_km(j), (pdvs(ii), ii = 1, ix)
!            case(2) 
!            write (28,1001) cropname, j, subnum(j), hruno(j), sb,       &
!     &         nmgt(j), mo_chk, hru_km(j),(pdvs(ii), ii = 1, ix), iyr
!         end select
          end if

        end if
      end do

      return
 1000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,8e10.3,3f10.3)
 2000 format (a4,i5,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,5e10.3,6f10.3,1x,i4)
 1001 format (a4,i7,1x,a5,a4,i5,1x,i4,1x,i4,e10.5,66f10.3,1x,
     *e10.5,1x,e10.5,3e10.3,3f10.3,1x,i4)
      end