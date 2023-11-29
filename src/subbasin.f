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
!!    SWAT: confert, graze, plantmod, nminrl, nitvol, pminrl, gwmod, apply, gwmod_deep
!!    SWAT: washp, decay, pestlch, enrsb, pesty, orgn, psed, nrain, nlch
!!    SWAT: solp, subwq, bacteria, urban, pothole, latsed, surfstor
!!    SWAT: substor, wetland, hrupond, irrsub, autoirr, watuse, watbal
!!    SWAT: sumv, virtual

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j,sb,kk
      real*8 :: tmpk, d, gma, ho, pet_alpha, aphu, phuop,lid_sto
      integer :: ppet_mce

      ihru = 0
      ihru = hru1(inum1) 
      
      call sub_subbasin

      do iihru = 1, hrutot(inum1)

      j = 0
      j = ihru
	sb = hru_sub(j)
	
      !!by zhang DSSAT tillage
      !!======================
      !!    deptil(:)   |mm  |depth of mixing caused by tillage operation
      !jj is hru number
      if (cswat == 2) then
          if (tillage_switch(ihru) .eq. 1) then
              if (tillage_days(ihru) .ge. 30) then
                    tillage_switch(ihru) = 0
                    tillage_days(ihru) = 0
              else
                    tillage_days(ihru) = tillage_days(ihru) + 1
              end if                
              !tillage_depth(ihru) = dtil
              !tillage_switch(ihru) = .TRUE. 
          end if
      end if
      !!by zhang DSSAT tillage  
      !!====================== 



      call varinit
      if (icr(j) <= 0) icr(j) = 1
      
      i_wtrhru = 0
      idplrot(icr(j),ihru) = idplt(j)
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

        !! compute evapotranspiration
        call etpot
!        if (pot_vol(j) < 1.e-6) call etact
        call etact

        !! Update CMI and Precip minus PET 30 day moving sum
        ppet(j)%mce = ppet(j)%mce + 1
        if (ppet(j)%mce > ppet(j)%ndays) ppet(j)%mce = 1
        ppet_mce = ppet(j)%mce
        !! calculate climatic moisture index - cumulative p/pet
        !! subtract the 30 day previous and add the current day precip/pet
        ppet(j)%precip_sum = ppet(j)%precip_sum + precipday - ppet(j)%precip(ppet_mce)
        ppet(j)%pet_sum = ppet(j)%pet_sum + pet_day - ppet(j)%pet(ppet_mce)
        ppet(j)%rto = ppet(j)%precip_sum / (ppet(j)%pet_sum + 0.5)
        ppet(j)%precip(ppet_mce) = precipday
        ppet(j)%pet(ppet_mce) = pet_day
        if (ppet(j)%trop > 0) then
        if (ppet(j)%mon_seas > 0) then
          ppet(j)%curday_mon = ppet(j)%curday_mon + 1
          !! plant if last day of monsoon and not planted
          if (ppet(j)%curday_mon == ppet(j)%ndays_mon) then
            ppet(j)%mon_seas = 0
            if (ppet(j)%peren == 0) then
              !! annual planting
              call plantop 
                write (144, 1001) subnum(j), hruno(j), iyr, i_mo, iida,   
     &          hru_km(j), cpnm(idplt(j))," PLANT", precipday, pet_day, 
     &          ppet(j)%rto, ppet(j)%precip_sum, ppet(j)%pet_sum, sol_sw(j)
            else
              !! perennial phenology reset
              igro(j) = 1
              idorm(j) = 0
              phuacc(j) = 0.
                write (144, 1001) subnum(j), hruno(j), iyr, i_mo, iida,   
     &          hru_km(j), cpnm(idplt(j))," PHENO-RESET", precipday, pet_day, 
     &          ppet(j)%rto, ppet(j)%precip_sum, ppet(j)%pet_sum, sol_sw(j)
 1001  format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,6f10.2)  
            end if
          end if
        !else
            !! check if ppet ratio is exceeded
            if (ppet(j)%rto > ppet(j)%trig) then
            ppet(j)%mon_seas = 0
            if (ppet(j)%peren == 0) then
              !! annual planting
              call plantop
              write (144, 1001) subnum(j), hruno(j), iyr, i_mo, iida,   
     &          hru_km(j), cpnm(idplt(j))," PLANT", precipday, pet_day, 
     &          ppet(j)%rto, ppet(j)%precip_sum, ppet(j)%pet_sum, sol_sw(j)
            else
              !! perennial phenology reset
              igro(j) = 1
              idorm(j) = 0
              phuacc(j) = 0.          
              write (144, 1001) subnum(j), hruno(j), iyr, i_mo, iida,   
     &          hru_km(j), cpnm(idplt(j))," PHENO-RESET", precipday, pet_day, 
     &          ppet(j)%rto, ppet(j)%precip_sum, ppet(j)%pet_sum, sol_sw(j)
            end if
          end if
        end if
        end if
      
        call surface
		
       !! compute effective rainfall (amount that percs into soil)
	  lid_str_curday(j,:) = lid_str_curday(j,:) / (hru_ha(j) * 10.) !m3 to mm
	  lid_sto = sum(lid_str_curday(j,:))
	  inflpcp = Max(0.,precipday - surfq(j) - lid_sto)
         
        !! perform management operations
        if (yr_skip(j) == 0) call operatn
          
        if (irrsc(j) > 2) call autoirr       
        
        !! perform soil water routing
        call percmain

        !! compute water table depth using climate drivers
        call wattable

        !! new CN method
        if (icn == 1) then 
        sci(j) = sci(j) + pet_day*exp(-cncoef_sub(hru_sub(j))*sci(j)/   
     &    smx(j)) - precipday + qday + qtile + latq(j) + sepbtm(j)
        else if (icn == 2) then 
        sci(j) = sci(j) + pet_day*exp(-cncoef_sub(hru_sub(j))*sci(j)/   
     &    smx(j)) - precipday + qday + latq(j) + sepbtm(j) + qtile
        sci(j) = dmin1(sci(j),smxco * smx(j))
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
        
        !! check for dormancy - not for tropical plants
        if (igro(j) == 1 .and. ppet(j)%trop == 0) call dormant
        
        !! compute actual ET for day in HRU
        etday = ep_day + es_day + canev

        !! write daily air and soil temperature file
        !! can be uncommmented if needed by user and also in readfile.f

!      write (120,12112) i,j,tmx(j),tmn(j),(sol_tmp(k,j),k=1,sol_nly(j))
!12112  format (2i4,12f8.2)

        !! compute nitrogen and phosphorus mineralization 

      if (cswat == 0) then
        call nminrl
	end if
	if (cswat == 1) then
		call carbon
	end if
	
	!! Add by zhang
	!!=================
	if (cswat == 2) then
	  call carbon_zhang2
	end if
	!! Add by zhang
	!!=================	

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
        call gwmod_deep

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
	    end if
	    if (cswat == 1) then
	    
		    call orgncswat(0)
		  end if
		  
		  !! Add by zhang
		  !! ====================
		  if (cswat == 2) then
		    call orgncswat2(0)
		  end if
		  !! Add by zhang
		  !! ====================

            call psed(0)
          end if
        end if

        !! add nitrate in rainfall to soil profile
        call nrain

        !! compute nitrate movement leaching
        call nlch

        !! compute phosphorus movement
        call solp

        !! compute bacteria transport
        call bacteria

        !! compute loadings from urban areas
        if (urblu(j) > 0) then
	     if(ievent == 0) then
	        call urban ! daily simulation
	     else
		     call urbanhr ! subdaily simulation J.Jeong 4/20/2009
	     endif
	  endif
	  
!! Srini Pothole
        !! compute undrained depression/impounded area (eg rice) processes
!        if (pot_fr(j) > 0.) then
!           if (ievent == 0) then   
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

        !! add lateral flow that was routed across the landscape on the previous day
      !!  latq(j) = latq(j) + latq_ru(j)
      !!  latq_ru(j) = 0.
        
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
	 !  if (bmp_flag(j) == 1) then
       !   call bmpfixed
       ! end if


        !! compute water yield for HRU
        qdr(j) = qday + latq(j) + gw_q(j) + qtile + gw_qdeep(j)
        if (qdr(j) < 0.) qdr(j) = 0.
        if (qdr(j) > 0.) then
          qdfr = qday / qdr(j)
        else
          qdfr = 0.
        end if

        !! compute wetland processes
        call wetlan

        !! compute pond processes
        if (ievent == 0) then
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
        
        !! compute chl-a, CBOD and dissolved oxygen loadings
        call subwq

        !! qdayout is surface runoff leaving the hru - after wetlands, ponds, and potholes
        qdayout(j) = qday

      endif

      !! perform output summarization
      call sumv

      !! summarize output for multiple HRUs per subbasin
      !! store reach loadings for new fig method
      call virtual
      aird(j) = 0.
      
      ihru = ihru + 1
      end do

      !! route 2 landscape units
      if (ils2flag(inum1) > 0) then
      isub = inum1                        ! save the subbasin number
      
      !! calculate outputs from hillslope
      ihout1 = mhyd_bsn + (inum1 - 1) * 4 ! first outflow hyd number
      ihout = ihout1                      ! outflow hyd number
      inum1 = 1                           ! landscape unit number
      inum2 = isub                        ! subbasin number
      call routeunit                      ! hillslope unit
      call sumhyd
      inum1s(ihout) = inum1
      inum2s(ihout) = inum2
      ihouts(ihout) = ihout
      
      !! calculate outputs from valley bottom
      inum1 = 2                           ! landscape unit number
      ihout = ihout + 1                   ! outflow hyd number
      sumdaru = 0.
      do j = 1, hrutot(isub)
        sumdaru = sumdaru + hru_km(j)
      end do 
      daru_km(inum2,inum1) = sumdaru
      call routeunit                      ! valley bottom unit
      call sumhyd
      inum1s(ihout) = inum1
      inum2s(ihout) = inum2
      ihouts(ihout) = ihout
      
      !! route output from hillslope across valley bottom
      ihout = ihout + 1                   ! outflow hyd number
      inum1 = 2                           ! valley bottom landscape unit
      inum2 = ihout1                      ! inflow hyd=outlfow from hillslope
      inum3 = isub                        ! subbasin number
      rnum1 = 1.                          ! fraction overland flow
      iru_sub = 1                         ! route across landscape unit
      !! compute weighted K factor for sediment transport capacity
      sumk = 0.
      ovsl = 0.
      ovs = 0.
      do j = 1, hrutot(isub)
        sumk = sumk + usle_k(j) * hru_rufr(inum1,j)
        ovsl = ovsl + slsubbsn(j)
        ovs = ovs + hru_slp(j)
      end do 
      ovsl = ovsl / hrutot(isub)
      ovs = ovs / hrutot(isub)
      ru_k(isub,inum1) = sumk
      ru_ovsl(isub,inum1) = ovsl
      ru_ovs(isub,inum1) = ovs
      ru_ktc(isub,inum1) = 50.
      ru_a(isub,inum1) = daru_km(isub,1) / ru_ovsl(isub,inum1)
      call routels(iru_sub)               ! route across valley bottom
      call sumhyd
      inum1s(ihout) = inum1
      inum2s(ihout) = inum2
      inum3s(ihout) = inum3
      ihouts(ihout) = ihout
      
      !! add routed with valley bottom loading
      inum1 = ihout                       ! hyd from routed 
      inum2 = ihout - 1                   ! hyd from loading
      ihout = ihout + 1                   ! outflow hyd number
      call addh                           ! add hyd's
      call sumhyd
      inum1s(ihout) = inum1
      inum2s(ihout) = inum2
      ihouts(ihout) = ihout
      
      !! save landscape routed output in place of subbasin output for routing
      varoute(isub,:) = varoute(ihout,:)
      end if
      
      return
      end