      subroutine writeaa

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual output

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aairr(:)     |mm H2O        |average annual amount of irrigation water
!!                                |applied to HRU
!!    bio_aams(:)  |metric tons/ha|average annual biomass (dry weight) in HRU
!!    bio_hv(:,:,:)|kg/ha         |harvested biomass (dry weight)
!!    da_ha        |ha            |area of watershed in hectares
!!    hru_dafr(:)  |none          |fraction of watershed area in HRU
!!    hruaao       |varies        |HRU average annual output array
!!    hrupest(:)   |none          |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    hrupsta(:,1,:)|mg pst       |amount of pesticide type in surface runoff
!!                                |contribution to stream from HRU during 
!!                                |simulation (in solution)
!!    hrupsta(:,2,:)|mg pst       |amount of pesticide type in surface runoff
!!                                |contribution to stream from HRU during
!!                                |simulation (sorbed to sediment)
!!    icodes(:)     |none         |routing command code:
!!                                |0 = finish       9 = save
!!                                |1 = subbasin    10 = recday
!!                                |2 = route       11 = reccnst
!!                                |3 = routres     12 = structure
!!                                |4 = transfer    13 = 
!!                                |5 = add         14 = saveconc
!!                                |6 = rechour     15 = 
!!                                |7 = recmon 
!!                                |8 = recyear
!!    idaf         |julian date   |beginning day of simulation
!!    idal         |julian date   |ending day of simulation
!!    ihouts(:)    |none          |For ICODES equal to
!!                                |0: not used
!!                                |1,2,3,5,7,8,10,11: hydrograph storage
!!                                |                     location number
!!                                |4: water source type
!!                                |   (1=reach)
!!                                |   (2=reservoir)
!!                                |9: hydrograph storage location of data to
!!                                |   be printed to event file
!!                                |14:hydrograph storage location of data to
!!                                |   be printed to saveconc file
!!    inum1s(:)    |none          |For ICODES equal to
!!                                |0: not used
!!                                |1: HRU number
!!                                |2: reach number
!!                                |3: reservoir number
!!                                |4: reach or res # flow is diverted from
!!                                |5: hydrograph storage location of 1st
!!                                |   dataset to be added
!!                                |7,8,9,10,11,14: file number
!!    inum2s(:)    |none          |For ICODES equal to
!!                                |0,1,7,8,10,11: not used
!!                                |2,3: inflow hydrograph storage location
!!                                |4: destination type
!!                                |   (1=reach)
!!                                |   (2=reservoir)
!!                                |5: hydrograph storage location of 2nd
!!                                |   dataset to be added
!!                                |9,14:print frequency
!!                                |   (0=daily)
!!                                |   (1=hourly)
!!    inum3s(:)    |none          |For ICODES equal to
!!                                |0,1,2,3,5,7,8,10,11: not used
!!                                |4: destination number. Reach or
!!                                |   reservoir receiving water
!!                                |9: print format
!!                                |   (0=normal, fixed format)
!!                                |   (1=txt format for AV interface,recday)
!!    iprp         |none          |print code for output.pes file
!!                                |0 do not print pesticide output
!!                                |1 print pesticide output
!!    irn(:)       |none          |average annual number of irrigation
!!                                |applications in HRU
!!    lai_aamx(:)  |none          |average annual maximum leaf area index in
!!                                |HRU
!!    leapyr       |none          |leap year flag:
!!                                |0  leap year
!!                                |1  regular year
!!    mcr          |none          |max number of crops grown per year
!!    nbyr         |none          |number of calendar years simulated
!!    ncrops(:,:,:)|
!!    ndays(:)     |julian date   |julian date for last day of preceding
!!                                |month (where the array location is the
!!                                |number of the month). The dates are for
!!                                |leap years
!!    ndmo(:)      |days          |cumulative number of days accrued in the
!!                                |month since the simulation began where the
!!                                |array location number is the number of the
!!                                |month
!!    nhru         |none          |number of HRUs in watershed
!!    npmx         |none          |number of different pesticides used in
!!                                |the simulation
!!    nres         |none          |number of reservoirs in watershed
!!    nrot(:)      |none          |number of years of rotation
!!    nyskip       |none          |number of years to skip output
!!                                |summarization and printing
!!    rchaao       |varies        |reach average annual output array
!!    resouta(3,:) |metric tons   |sediment entering reservoir during simulation
!!    resouta(4,:) |metric tons   |sediment leaving reservoir during simulation
!!    resouta(17,:)|m^3 H2O       |evaporation from reservoir during simulation
!!    resouta(18,:)|m^3 H2O       |seepage from reservoir during simulation
!!    resouta(19,:)|m^3 H2O       |precipitation on reservoir during simulation
!!    resouta(20,:)|m^3 H2O       |water flowing into reservoir during 
!!                                |simulation
!!    resouta(21,:)|m^3 H2O       |water flowing out of reservoir during
!!                                |simulation
!!    sbactlchlp   |# colonies/ha |average annual number of less persistent
!!                                |bacteria lost from soil surface layer by
!!                                |percolation
!!    sbactlchp    |# colonies/ha |average annual number of persistent bacteria
!!                                |lost from soil surface layer by percolation
!!    sbactrolp    |# colonies/ha |average annual number of less persistent
!!                                |bacteria transported to main channel
!!                                |with surface runoff in solution
!!    sbactrop     |# colonies/ha |average annual number of persistent bacteria
!!                                |transported to main channel with surface
!!                                |runoff in solution
!!    sbactsedlp   |# colonies/ha |average annual number of less persistent
!!                                |bacteria transported with sediment in
!!                                |surface runoff
!!    sbactsedp    |# colonies/ha |average annual number of persistent bacteria
!!                                |transported with sediment in surface runoff
!!    sdiegrolpq   |# colonies/ha |average annual change in the number of
!!                                |less persistent bacteria colonies in soil
!!                                |solution in watershed
!!    sdiegrolps   |# colonies/ha |average annual change in the number of
!!                                |less persistent bacteria colonies on soil
!!                                |particles in watershed
!!    sdiegropq    |# colonies/ha |average annual change in the number of
!!                                |persistent bacteria colonies in soil solution
!!                                |in watershed
!!    sdiegrops    |# colonies/ha |average annual change in the number of
!!                                |persistent bacteria colonies on soil particles
!!                                |in watershed
!!    shyd(1,:)    |m^3 H2O       |water
!!    shyd(2,:)    |metric tons   |sediment or suspended solid load
!!    shyd(3,:)    |kg N          |organic nitrogen
!!    shyd(4,:)    |kg P          |organic phosphorus
!!    shyd(5,:)    |kg N          |nitrate
!!    shyd(6,:)    |kg P          |soluble phosphorus
!!    shyd(7,:)    |mg pst        |soluble pesticide
!!    shyd(8,:)    |mg pst        |sorbed pesticide  
!!    sno3up       |kg N/ha       |amount of nitrate moving upward in the soil
!!                                |profile in watershed
!!    sol_actp(:,:)|kg P/ha       |amount of phosphorus stored in the
!!                                |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha      |amount of nitrogen stored in the active
!!                                |organic (humic) nitrogen pool
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_nly(:)   |none          |number of layers in soil profile
!!    sol_no3(:,:) |kg N/ha       |amount of nitrogen stored in the
!!                                |nitrate pool.
!!    sol_orgn(:,:)|kg N/ha       |amount of nitrogen stored in the stable
!!                                |organic N pool
!!    sol_orgp(:,:)|kg P/ha       |amount of phosphorus stored in the organic
!!                                |P pool
!!    sol_solp(:,:)|kg P/ha       |amount of phosohorus stored in solution
!!    sol_stap(:,:)|kg P/ha       |amount of phosphorus in the soil layer
!!                                |stored in the stable mineral phosphorus pool
!!    spadyev      |mm H2O        |average annual amount of water removed
!!                                |from potholes by evaporation in watershed
!!    spadyo       |mm H2O        |average annual amount of water released to
!!                                |main channel from potholes in watershed
!!    spadyrfv     |mm H2O        |average annual amount of precipitation on
!!                                |potholes in watershed
!!    spadysp      |mm H2O        |average annual amount of water removed
!!                                |from potholes by seepage in watershed
!!    subaao       |varies        |subbasin average annual output array
!!    sumix(:)     |none          |sum of mixing efficiencies in HRU
!!    wshd_aamon(:,:)|varies        |array of watershed monthly average values
!!    wshd_dnit    |kg N/ha       |average annual amount of nitrogen lost from
!!                                |nitrate pool due to denitrification in
!!                                |watershed
!!    wshd_fixn    |kg N/ha       |average annual amount of nitrogen added to
!!                                |plant biomass via fixation
!!    wshd_fminp   |kg P/ha       |average annual amount of mineral P applied
!!                                |in watershed
!!    wshd_fnh3    |kg N/ha       |average annual amount of NH3-N applied in
!!                                |watershed
!!    wshd_fno3    |kg N/ha       |average annual amount of NO3-N applied in
!!                                |watershed
!!    wshd_forgn   |kg N/ha       |average annual amount of organic N applied
!!                                |in watershed
!!    wshd_forgp   |kg P/ha       |average annual amount of organic P applied
!!                                |in watershed
!!    wshd_ftotn   |kg N/ha       |average annual amount of N (mineral &
!!                                |organic) applied in watershed
!!    wshd_ftotp   |kg P/ha       |average annual amount of P (mineral &
!!                                |organic) applied in watershed
!!    wshd_hmn     |kg N/ha       |average annual amount of nitrogen moving
!!                                |from active organic to nitrate pool in
!!                                |watershed
!!    wshd_hmp     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from organic to labile pool in watershed
!!    wshd_nitn    |kg N/ha       |average annual amount of nitrogen moving
!!                                |from the NH3 to the NO3 pool by
!!                                |nitrification in the watershed
!!    wshd_nstrs   |stress units  |average annual number of nitrogen stress
!!                                |units in watershed
!!    wshd_pal     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from labile mineral to active mineral pool
!!                                |in watershed
!!    wshd_pas     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from active mineral to stable mineral pool
!!                                |in watershed
!!    wshd_plch    |kg P/ha       |average annual amount of phosphorus leached
!!                                |into second soil layer
!!    wshd_pstap(:)|kg pst/ha     |total amount of pesticide type applied in
!!                                |watershed during simulation
!!    wshd_pstdg(:)|kg pst/ha     |amount of pesticide lost through degradation
!!                                |in watershed
!!    wshd_pstrs   |stress units  |average annual number of phosphorus stress
!!                                |units in watershed
!!    wshd_pup     |kg P/ha       |average annual amount of plant uptake of
!!                                |phosphorus
!!    wshd_raino3  |kg N/ha       |average annual amount of NO3 added to soil
!!                                |by rainfall in watershed
!!    wshd_rmn     |kg N/ha       |average annual amount of nitrogen moving
!!                                |from fresh organic (residue) to nitrate
!!                                |and active organic pools in watershed
!!    wshd_rmp     |kg P/ha       |average annual amount of phosphorus moving
!!                                |from fresh organic (residue) to labile
!!                                |and organic pools in watershed
!!    wshd_rwn     |kg N/ha       |average annual amount of nitrogen moving
!!                                |from active organic to stable organic pool
!!                                |in watershed
!!    wshd_tstrs   |stress units  |average annual number of temperature stress
!!                                |units in watershed
!!    wshd_voln    |kg N/ha       |average annual amount if nitrogen lost by
!!                                |ammonia volatilization in watershed
!!    wshd_wstrs   |stress units  |average annual number of water stress units
!!                                |in watershed
!!    wshd_yldn    |kg N/ha       |amount of nitrogen removed from soil in
!!                                |watershed in the yield
!!    wshd_yldp    |kg P/ha       |amount of phosphorus removed from soil in
!!                                |watershed in the yield
!!    wshdaao      |varies        |watershed average annual output array
!!    wtraa(:,:)   |varies        |HRU impoundment average annual output array
!!    yldaa(:)     |metric tons/ha|average annual yield (dry weight) in HRU
!!    yldkg(:,:,:) |kg/ha         |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    basminpf    |kg P/ha       |final average amount of phosphorus in
!!                               |the mineral P pool in watershed soil
!!    basno3f     |kg N/ha       |final average amount of nitrogen in the
!!                               |nitrate pool in watershed soil
!!    basorgnf    |kg N/ha       |final average amount of nitrogen in the
!!                               |organic N pool in watershed soil
!!    basorgpf    |kg P/ha       |final average amount of phosphorus in
!!                               |the organic P pool in watershed soil
!!    bio_aahv(:,:,:)|kg/ha         |harvested biomass of plant
!!    resdata(1)  |mm H2O        |average annual evaporation from reservoirs
!!                               |in watershed
!!    resdata(2)  |mm H2O        |average annual seepage from reservoirs in
!!                               |watershed
!!    resdata(3)  |mm H2O        |average annual precipitation on reservoirs
!!                               |in watershed
!!    resdata(4)  |mm H2O        |average annual amount of water transported
!!                               |into reservoirs in watershed
!!    resdata(5)  |metric tons/ha|average annual amount of sediment transported
!!                               |into reservoirs in watershed
!!    resdata(6)  |mm H2O        |average annual amount of water transported
!!                               |out of reservoirs in watershed
!!    resdata(7)  |metric tons/ha|average annual amount of sediment transported
!!                               |out of reservoirs in watershed
!!    wshd_pstap(:)|kg pst/ha     |average annual amount of pesticide type
!!                               |applied in watershed during simulation
!!    wshd_pstdg(:)|kg pst/ha     |average annual amount of pesticide lost
!!                               |through degradation in watershed
!!    yldn(:,:,:) |kg/ha         |average value for yield of crop
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ic          |none          |hydrograph storage location number
!!    idum        |none          |line # in .fig file
!!    ii          |none          |counter
!!    j           |none          |counter
!!    k           |none          |counter
!!    ly          |none          |counter
!!    nicr        |none          |crop number in sequence within year
!!    nnro        |none          |year number in rotation sequence
!!    summinp     |kg P/ha       |total mineral P in HRU soil profile
!!    sumno3      |kg N/ha       |total nitrate in HRU soil profile
!!    sumorgn     |kg N/ha       |total organic N in HRU soil profile
!!    sumorgp     |kg P/ha       |total organic P in HRU soil profile
!!    yrs         |years         |length of simulation
!!    xmm         |months        |number of months simulated
!!    xx          |none          |days in year
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: real*8
!!    SWAT: hruaa, impndaa, rchaa, subaa, stdaa

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real*8 :: yrs, xx, xmm, sumno3, sumorgn, summinp, sumorgp
      integer :: j, nnro, nicr, k, ly, ic, ii

!! calculate number of years simulated
      yrs = 0.
      do j = 1, nbyr
        xx = 0.
        xx = 366. - dfloat(leapyr)
        if (j > nyskip) then
          if (j == 1 .and. idaf > 0) then
            yrs = yrs + (xx - (dfloat(idaf) - 1. - dfloat(fcstcnt))) / xx
          elseif (j == nbyr .and. idal > 0) then
            yrs = yrs + ((dfloat(idal) - dfloat(fcstcnt)) / xx)
          else
            yrs = yrs + 1.
          end if
        end if
      end do
      if (yrs <= 0) return

	if (da_ha < 1.e-9) then
	   rchaao = rchaao / yrs
	     if (iprint /=1) then
	       call rchaa(yrs)
	       call rsedaa(yrs)
	     end if
	     return
      end if

!! calculate average annual values for HRU data
      hruaao = hruaao / yrs
      wtraa = wtraa / yrs
      bio_aams = bio_aams / yrs
      lai_aamx = lai_aamx / yrs
      yldaa = yldaa / yrs
      irn = irn / yrs
      aairr = aairr / yrs
      do j = 1, nhru
        do nicr = 1, mcrhru(j)
          if (ncrops(nicr,j) > 0) then            
            yldn(nicr,j) = yldkg(nicr,j) /  ncrops(nicr,j)
            bio_aahv(nicr,j) = bio_hv(nicr,j) / ncrops(nicr,j)  
          else
            yldn(nicr,j) = 0.
            bio_aahv(nicr,j) = 0.
          end if
        end do
      end do
      hrupsta = hrupsta / yrs
      sumix = sumix / yrs

!! calculate average annual values for reach data
      rchaao = rchaao / yrs

!! calculate average annual values for subbasin data
      subaao = subaao / yrs

!! calculate average annual values for reservoir data
      resouta = resouta / yrs
      do j = 1, nres
        resdata(1) = resdata(1) + resouta(17,j)
        resdata(2) = resdata(2) + resouta(18,j)
        resdata(3) = resdata(3) + resouta(19,j)
        resdata(4) = resdata(4) + resouta(20,j)
        resdata(5) = resdata(5) + resouta(3,j)
        resdata(6) = resdata(6) + resouta(21,j)
        resdata(7) = resdata(7) + resouta(4,j)
      end do
      resdata(1) = resdata(1) / (da_ha * 10.)
      resdata(2) = resdata(2) / (da_ha * 10.)
      resdata(3) = resdata(3) / (da_ha * 10.)
      resdata(4) = resdata(4) / (da_ha * 10.)
      resdata(5) = resdata(5) / da_ha
      resdata(6) = resdata(6) / (da_ha * 10.)
      resdata(7) = resdata(7) / da_ha

!! calculate average annual values for watershed data
      wshdaao = wshdaao / yrs
      wpstaao = wpstaao / yrs
      !!convert metric tons to metric tons/ha
      wshdaao(11) = wshdaao(11) / da_ha
      !! wshdaao(12) converted in writem.f
      do j = 13, 18
        wshdaao(j) = wshdaao(j) / da_ha
      end do
      !! convert m^3 H2O to mm H2O
      do j = 19, 34
        wshdaao(j) = wshdaao(j) / (da_ha * 10.)
      end do
      wshd_pstap = wshd_pstap / yrs
      wshd_pstdg = wshd_pstdg / yrs
      !! calculate monthly averages
      do j = 1, 12
        xmm = dfloat(ndmo(j)) / dfloat(ndays(j+1) - ndays(j))
        if (xmm > 0.) then
          do k = 1, 8
            wshd_aamon(j,k) = wshd_aamon(j,k) / xmm
          end do
        end if
      end do
      !! calculate average stresses for watershed
      wshd_wstrs = wshd_wstrs / yrs
      wshd_tstrs = wshd_tstrs / yrs
      wshd_nstrs = wshd_nstrs / yrs
      wshd_pstrs = wshd_pstrs / yrs
      wshd_astrs = wshd_astrs / yrs
      !! calculate watershed pothole averages
      spadyo = spadyo / yrs
      spadyosp = spadyosp / yrs
      spadyev = spadyev / yrs
      spadysp = spadysp / yrs
      spadyrfv = spadyrfv / yrs
      !! calculate watershed nutrient averages
      wshd_pup = wshd_pup / yrs
      wshd_plch = wshd_plch / yrs
      wshd_pinlet = wshd_pinlet / yrs
      wshd_ptile = wshd_ptile / yrs
      wshd_pal = wshd_pal / yrs
      wshd_pas = wshd_pas / yrs
      wshd_ftotn = wshd_ftotn / yrs
      wshd_ftotp = wshd_ftotp / yrs
      wshd_dnit = wshd_dnit / yrs
      wshd_fixn = wshd_fixn / yrs  !! fix

      if (cswat == 0) then
		wshd_hmn = wshd_hmn / yrs  !! humus n for active
		wshd_rwn = wshd_rwn / yrs  !! active to stable
		wshd_hmp = wshd_hmp / yrs  !! humus min on active org   	
	end if

	  wshd_rmn = wshd_rmn / yrs  !! min from fresh orgn
      wshd_rmp = wshd_rmp / yrs  !! min from fresh orgp 
      wshd_raino3 = wshd_raino3 / yrs
      wshd_fno3 = wshd_fno3 / yrs
      wshd_fnh3 = wshd_fnh3 / yrs
      wshd_forgn = wshd_forgn / yrs
      wshd_fminp = wshd_fminp / yrs
      wshd_forgp = wshd_forgp / yrs
      wshd_yldn = wshd_yldn / yrs
      wshd_yldp = wshd_yldp / yrs
      wshd_voln = wshd_voln / yrs
      wshd_nitn = wshd_nitn / yrs
      sno3up = sno3up / yrs
      !! calculate final nutrient levels in watershed soils
      do j = 1, nhru
        sumno3 = 0.
        sumorgn = 0.
        summinp = 0.
        sumorgp = 0.
        do ly = 1, sol_nly(j)
          sumno3 = sumno3 + sol_no3(ly,j)
	    if (cswat == 0) then
            sumorgn = sumorgn + sol_aorgn(ly,j) + sol_orgn(ly,j) +
     &        sol_fon(ly,j)
	      sumorgp = sumorgp + sol_fop(ly,j) + sol_orgp(ly,j)
	    end if
	    if (cswat == 1) then
      	    sumorgn = sumorgn + sol_orgn(ly,j) + sol_fon(ly,j) +
     &        sol_mn(ly,j)
		    sumorgp = sumorgp + sol_fop(ly,j) + sol_orgp(ly,j) +
     &        sol_mp(ly,j)
	    end if
	    !!add by zhang
	    !!=======================
	    if (cswat == 2) then
            sumorgn = sumorgn + sol_LMN(ly,j) + sol_LSN(ly,j) +
     &        sol_HPN(ly,j) + sol_BMN(ly,j) + sol_HSN(ly,j)
	      sumorgp = sumorgp + sol_fop(ly,j) + sol_orgp(ly,j)	      
	    end if
	    !!add by zhang
	    !!=======================	    
	    
          summinp = summinp + sol_solp(ly,j) + sol_actp(ly,j) +
     &              sol_stap(ly,j)
        end do
        basno3f = basno3f + sumno3 * hru_dafr(j)
        basorgnf = basorgnf + sumorgn * hru_dafr(j)
        basminpf = basminpf + summinp * hru_dafr(j)
        basorgpf = basorgpf + sumorgp * hru_dafr(j)
      end do
      !! calculate watershed bacteria averages
      sdiegropq = sdiegropq / yrs
      sdiegrolpq = sdiegrolpq / yrs
      sdiegrops = sdiegrops / yrs
      sdiegrolps = sdiegrolps / yrs
      sbactrop = sbactrop / yrs
      sbactrolp = sbactrolp / yrs
      sbactsedp = sbactsedp / yrs
      sbactsedlp = sbactsedlp / yrs
      sbactlchp = sbactlchp / yrs
      sbactlchlp = sbactlchlp / yrs
      

!! write average annual data
      if (iprint /= 1) then
        !! write average annual output--HRU (output.hru)
        call hruaa(yrs)
        call impndaa(yrs)

        !! write average annual output--reach (.rch)
        if (iprint /= 3) call rchaa(yrs)

!       !! write average annual output--sediment routing (.sed)
        call rsedaa(yrs)

        !! write average annual output--subbasin (output.sub)
        call subaa(yrs)
      end if

!! write average annual pesticide data (output.pes)
      if (iprp == 1) then
        write (30,5500)
        do j = 1, nhru
          if (hrupest(j) == 1) then
                write (30,5600) subnum(j), hruno(j), yrs,               
     &                     (hrupsta(k,1,j), hrupsta(k,2,j), k = 1, npmx)
          end if
        end do
      end if

!! write to hydrograph output file
      do idmm = 1, mhyd
        ic = ihouts(idmm)
        if (ic > 0) then
        write(11123,9400) icodes(idmm), ic, inum1s(idmm), inum2s(idmm), 
     &               inum3s(idmm),subed(ic),recmonps(ic),reccnstps(ic), 
     &               (shyd(ii,ic), ii = 1, 8)
        end if
      end do

!! average septic outputs for output.std
      wshd_sepno3 = wshd_sepno3 / yrs
      wshd_sepnh3 = wshd_sepnh3 / yrs
      wshd_seporgn = wshd_seporgn / yrs
      wshd_sepfon = wshd_sepfon / yrs
      wshd_seporgp = wshd_seporgp / yrs
      wshd_sepfop = wshd_sepfop / yrs
      wshd_sepsolp = wshd_sepsolp / yrs
      wshd_sepbod = wshd_sepbod / yrs
      wshd_sepmm = wshd_sepmm / yrs
!! average septic outputs for output.std


!! write average annual summary tables in standard output file (.std)
      call stdaa

!! write average annual forecast table
      if (ffcst == 1 .and. fcstcnt > 0) then
        write (18,*) iscen, (fcstaao(j), j = 1, 16)
      end if

!! write life span of septic HRUs (time to first failure, years)
	write(173,'(1/,a50)') ' Year the first failure occurred'
      write(173,'(1/,20x,a5,a7)') 'HRU',' Failyr'
      do j=1,nhru
	  if (isep_opt(j)/=0) then
	    if(failyr(j)==0) write(173,'(20x,i5,a8)') j,' No Fail'
	    if(failyr(j)/=0) write(173,'(20x,i5,f7.3)') j,failyr(j)
        end if
      end do

      return
 5500 format ("Average Annual Loadings")
 5600 format (1x,a5,a4,1x,f4.0,4x,1x,250(e16.4,1x))
!!! changed the format for hyd.out for Bill Komar
!9400 format (6i8,2(5x,a),8e12.4)
 9400 format (6(i8,1x),2(a10,1x),8e12.4)
      end