      subroutine subbasin
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_wstr(:)   |none          |water stress factor which triggers auto
!!                                  |irrigation
!!    bio_e(:)       |(kg/ha)/      |biomass-energy ratio
!!                   |     (MJ/m**2)|The potential (unstressed) growth rate per
!!                                  |unit of intercepted photosynthetically
!!                                  |active radiation.
!!    canev          |mm H2O        |amount of water evaporated from canopy
!!                                  |storage
!!    ep_day         |mm H2O        |actual amount of transpiration that occurs
!!                                  |on day in HRU
!!    es_day         |mm H2O        |actual amount of evaporation (soil et) that
!!                                  |occurs on day in HRU
!!    gw_q(:)        |mm H2O        |groundwater contribution to streamflow from
!!                                  |HRU on current day
!!    hru_ra(:)      |MJ/m^2        |solar radiation for the day in HRU
!!    iida           |julian date   |day being simulated (current julian date)
!!    idplt(:)       |none          |land cover code from crop.dat
!!    igro(:)        |none          |land cover status code
!!                                  |0 no land cover currently growing
!!                                  |1 land cover growing
!!    inum1          |none          |subbasin number
!!    imp_trig(:)    |none          |release/impound action code:
!!                                  |0 begin impounding water
!!                                  |1 release impounded water
!!    irrsc(:)       |none          |irrigation source code:
!!                                  |1 divert water from reach
!!                                  |2 divert water from reservoir
!!                                  |3 divert water from shallow aquifer
!!                                  |4 divert water from deep aquifer
!!                                  |5 divert water from source outside
!!                                  |  watershed
!!    iurban(:)      |none          |urban simulation code:
!!                                  |0  no urban sections in HRU
!!                                  |1  urban sections in HRU, simulate using
!!                                  |   USGS regression equations
!!                                  |2  urban sections in HRU, simulate using
!!                                  |   build up/wash off algorithm
!!    latq(:)        |mm H2O        |total lateral flow in soil profile for the
!!                                  |day in HRU
!!    nafert(:)      |none          |sequence number of auto-fert application
!!                                  |within the year
!!    nair(:)        |none          |sequence number of auto-irrigation 
!!                                  |application within the year
!!    nfert(:)       |none          |sequence number of fertilizer application
!!                                  |within the year
!!    nirr(:)        |none          |sequence number of irrigation application
!!                                  |within the year
!!    npest(:)       |none          |sequence number of pesticide application
!!                                  |within the year
!!    nrelease(:)    |none          |sequence number of impound/release
!!                                  |operation within the year
!!    nro(:)         |none          |sequence number of year in rotation
!!    peakr          |m^3/s         |peak runoff rate
!!    pet_day        |mm H2O        |potential evapotranspiration on current
!!                                  |day in HRU
!!    phuacc(:)      |none          |fraction of plant heat units accumulated
!!    phubase(:)     |heat units    |base zero total heat units (used when no
!!                                  |land cover is growing)
!!                                  |pesticide application occurs
!!    pot_fr(:)      |km2/km2       |fraction of HRU area that drains into
!!                                  |pothole
!!    pot_vol(:)     |m**3 H2O      |current volume of water stored in the 
!!                                  |depression/impounded area
!!    precipday      |mm H2O        |precipitation for the day in HRU
!!    qday           |mm H2O        |surface runoff loading to main channel from
!!                                  |HRU for day
!!    qtile          |mm H2O        |drainage tile flow in soil layer for the 
!!                                  |day
!!    sci(:)         |none          |retention coefficient for CN method based
!!                                  |on plant ET
!!    sedyld(:)      |metric tons   |soil loss for day in HRU
!!    smx(:)         |none          |retention coefficient for CN method based
!!                                  |on soil moisture
!!    surfq(:)       |mm H2O        |surface runoff generated on day in HRU
!!    tmn(:)         |deg C         |minimum temperature for the day in HRU
!!    tmpav(:)       |deg C         |average temperature for the day in HRU
!!    tmx(:)         |deg C         |maximum temperature for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    albday      |none          |albedo, the fraction of the solar radiation
!!                               |reflected at the soil surface back into
!!                               |space
!!    etday       |mm H2O        |actual evapotranspiration occuring on day
!!                               |in HRU
!!    ihru        |none          |HRU number
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    nafert(:)   |none          |sequence number of auto-fert application
!!                               |within the year
!!    nair(:)     |none          |sequence number of auto-irrigation 
!!                               |application within the year
!!    qdfr        |none          |fraction of water yield that is surface
!!                               |runoff
!!    qdr(:)      |mm H2O        |total amount of water entering main channel
!!                               |for day from HRU
!!    sci(:)      |none          |retention coefficient for CN method based
!!                               |on plant ET
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    d           |
!!    gma         |kPa/deg C     |psychrometric constant
!!    ho          |              |net radiation
!!    j           |none          |HRU number
!!    pet_alpha   |none          |alpha factor in Priestley-Taylor ET 
!!                               |equation
!!    tmpk        |deg K         |average temperature for the day in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max
!!    SWAT: varinit, albedo, solt, surface, percmain, etpot, etact, fert
!!    SWAT: confert, graze, plantmod, nminrl, nitvol, pminrl, gwmod, apply
!!    SWAT: washp, decay, pestlch, enrsb, pesty, orgn, psed, nrain, nlch
!!    SWAT: solp, subwq, bacteria, urban, pothole, latsed, surfstor
!!    SWAT: substor, wetland, hrupond, irrsub, autoirr, watuse, watbal
!!    SWAT: sumv, virtual

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j,sb,kk
      real :: tmpk, d, gma, ho, pet_alpha, aphu, phuop

      ihru = 0
      ihru = hru1(inum1) 
      
      call sub_subbasin

      do iihru = 1, hrutot(inum1)

      j = 0
      j = ihru

      call varinit
      if (icr(j) <= 0) icr(j) = 1
      
      i_wtrhru = 0
      if (idplt(j) /= 0) then
          if (cpnm(idplt(j)) == "WATR") then
              i_wtrhru = 1
          end if
      endif
      
	if (i_wtrhru == 1) then
         call water_hru
      else 

        !! Simulate land covers other than water

        !! update base zero total heat units
        if (tmpav(j) > 0. .and. phutot(hru_sub(j)) > 0.01) then
           phubase(j) = phubase(j) + tmpav(j) / phutot(hru_sub(j))
        end if
        
        call schedule_ops

        !! calculate albedo for day
        call albedo

        !! calculate soil temperature for soil layers
        call solt

!       if (ipot(j) /= j .and. imp_trig(nro(j),nrelease(j),j)==1)       &  Srini pothole
!
!     &        then             
          !! calculate surface runoff if HRU is not impounded or an 
          !! undrained depression--
          call surface

          !! compute effective rainfall (amount that percs into soil)
          inflpcp = Max(0.,precipday - surfq(j))
!        end if
         
        !! perform management operations
        if (yr_skip(j) == 0) call operatn
          
        if (auto_wstr(j) > 1.e-6 .and. irrsc(j) > 2) call autoirr       !!NUBZ
        
        !! perform soil water routing
        call percmain

        !! compute evapotranspiration
        call etpot
        if (pot_vol(j) < 1.e-6) call etact

        !! compute water table depth using climate drivers
        call wattable

        !! new CN method
        if (icn == 1) then 
        sci(j) = sci(j) + pet_day*exp(-cncoef_sub(hru_sub(j))*sci(j)/   &
     &    smx(j)) - precipday + qday
        else if (icn == 2) then 
        sci(j) = sci(j) + pet_day*exp(-cncoef_sub(hru_sub(j))*sci(j)/   &
     &    smx(j)) - precipday + qday + latq(j) + sepbtm(j) + qtile
        sci(j) = amin1(sci(j),smxco * smx(j))
        end if 
        
        !! apply fertilizer/manure in continuous fert operation
        if (icfrt(j) == 1) then
          ndcfrt(j) = ndcfrt(j) + 1
          call confert
        end if
        
        !! apply pesticide in continuous pest operation
        if (icpst(j) == 1) then 
          ndcpst(j) = ndcpst(j) + 1
          call conapply
        end if 
        
        !! remove biomass from grazing and apply manure
        if (igrz(j) == 1) then
          ndeat(j) = ndeat(j) + 1
          call graze
        end if
       
        !! compute crop growth
        call plantmod
        
        !! check for dormancy
        if (igro(j) == 1) call dormant
        !! compute actual ET for day in HRU
        etday = ep_day + es_day + canev

        !! write daily air and soil temperature file
        !! can be uncommmented if needed by user and also in readfile.f

!      write (120,12112) i,j,tmx(j),tmn(j),(sol_tmp(k,j),k=1,sol_nly(j))
!12112  format (2i4,12f8.2)

        !! compute nitrogen and phosphorus mineralization 

      if (cswat == 0) then
        call nminrl
	  else
		call carbon
	  end if

        call nitvol
        if (sol_P_model == 1) then
            call pminrl
        else
            call pminrl2
        end if

!!    compute biozone processes in septic HRUs
!!    if 1)current is septic hru and 2)  soil temperature is above zero
	  if (isep_opt(j)/=0.and.iyr>=isep_iyr(j)) then
	   if (sol_tmp(i_sep(j),j) > 0.) call biozone     
	  endif

        !! compute ground water contribution
        call gwmod

        !! compute pesticide washoff   
        if (precipday >= 2.54) call washp

        !! compute pesticide degradation
        call decay

        !! compute pesticide movement in soil
        call pestlch

        if (surfq(j) > 0. .and. peakr > 1.e-6) then
          if (precipday > 0.) then
            call enrsb(0)
            if (sedyld(j) > 0.) call pesty(0)

		  if (cswat == 0) then
			call orgn(0)
	      else
		    call orgncswat(0)
		  end if

            call psed(0)
          end if
        end if

        !! add nitrate in rainfall to soil profile
        call nrain

        !! compute nitrate movement leaching
        call nlch

        !! compute phosphorus movement
        call solp

        !! compute chl-a, CBOD and dissolved oxygen loadings
        call subwq

        !! compute bacteria transport
        call bacteria

        !! compute loadings from urban areas
        if (urblu(j) > 0) then
	     if(ievent<3) then
	        call urban ! daily simulation
	     else
		     call urbanhr ! subdaily simulation J.Jeong 4/20/2009
	     endif
	  endif
	  
!! Srini Pothole
        !! compute undrained depression/impounded area (eg rice) processes
!        if (pot_fr(j) > 0.) then
!           if (ievent<3) then   
!          call pothole
!           else
!              call potholehr
!           endif
!        endif
        
        !! compute sediment loading in lateral flow and add to sedyld
        call latsed

        !! compute nutrient loading in groundwater flow
        call gwnutr
        call gw_no3

        !! lag nutrients and sediment in surface runoff
        call surfstor

        !! lag subsurface flow and nitrate in subsurface flow

        call substor

        !! compute reduction in pollutants due to edge-of-field filter strip
        if (vfsi(j) >0.)then
          call filter
          if (filterw(j) > 0.) call buffer
        end if
              if (vfsi(j) == 0. .and. filterw(j) > 0.) then 
                call filtw
                call buffer
              end if

	 !! compute reduction in pollutants due to in field grass waterway
         if (grwat_i(j) == 1) then
          call grass_wway
        end if

	 !! compute reduction in pollutants due to in fixed BMP eff
	   if (bmp_flag(j) == 1) then
          call bmpfixed
        end if


        !! compute water yield for HRU
        qdr(j) = qday + latq(j) + gw_q(j) + qtile
        if (qdr(j) < 0.) qdr(j) = 0.
        if (qdr(j) > 0.) then
          qdfr = qday / qdr(j)
        else
          qdfr = 0.
        end if

        !! compute wetland processes
        call wetlan

        !! compute pond processes
        if (ievent<3) then
           call hrupond
        else
           call hrupondhr
        endif
        
!       Srini pothole        
        if (pot_fr(j) > 0.) call pothole
                
        xx = sed_con(j)+soln_con(j)+solp_con(j)+orgn_con(j)+orgp_con(j)
        if (xx > 1.e-6) then
          call urb_bmp
        end if

        !! consumptive water use (ponds, shallow aquifer, deep aquifer)
        call watuse

        !! perform water balance
        call watbal

      endif

      !! perform output summarization
      call sumv

      !! summarize output for multiple HRUs per subbasin
      !! store reach loadings for new fig method
      call virtual
      aird(j) = 0.
      
      ihru = ihru + 1
      end do

 1000 format(4i10,a10)
      return
      end
