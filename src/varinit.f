      subroutine varinit
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables for the daily simulation of the 
!!    land phase of the hydrologic cycle (the subbasin command loop)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    ihru        |none          |HRU number
!!    nstep       |none          |number of lines of rainfall data for each
!!                               |day
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    sno_hru(:)  |mm H2O        |amount of water stored as snow
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on any
!!                               |given day
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    al5         |none          |fraction of total rainfall that occurs
!!                               |during 0.5h of highest intensity rain
!!    albday      |none          |albedo for the day in HRU
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert 
!!                               |application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert 
!!                               |application
!!    bactlchlp   |# colonies/ha |less persistent bacteria removed from soil
!!                               |surface layer by percolation
!!    bactlchp    |# colonies/ha |persistent bacteria removed from soil surface
!!                               |layer by percolation
!!    bactrolp    |# colonies/ha |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# colonies/ha |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# colonies/ha |less persistent bacteria transported with
!!                               |sediment in surface runoff
!!    bactsedp    |# colonies/ha |persistent bacteria transported with
!!                               |sediment in surface runoff
!!    bioday      |kg            |biomass generated on current day in HRU
!!    bsprev      |mm H2O        |surface runoff lagged from prior day of
!!                               |simulation
!!    bssprev     |mm H2O        |lateral flow lagged from prior day of
!!                               |simulation
!!    canev       |mm H2O        |amount of water evaporated from canopy storage
!!    cfertn      |kg N/ha       |amount of nitrogen added to soil in continuous
!!                               |fertilizer operation on day
!!    cfertp      |kg P/ha       |amount of phosphorus added to soil in continuous
!!                               |fertilizer operation on day
!!    crk         |mm H2O        |percolation due to crack flow
!!    deepstp     |mm H2O        |depth of water in deep aquifer in HRU
!!    enratio     |none          |enrichment ratio calculated for day in HRU
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs on
!!                               |day in HRU
!!    ep_max      |mm H2O        |maximum amount of transpiration (plant et)
!!                               |that can occur on day in HRU
!!    es_day      |mm H2O        |actual amount of evaporation (from soil) that
!!                               |occurs on day in HRU
!!    etday       |mm H2O        |actual amount of evapotranspiration that 
!!                               |occurs on day in HRU
!!    fertn       |kg N/ha       |total amount of nitrogen added to soil in HRU
!!                               |on day
!!    fertp       |kg P/ha       |total amount of phosphorus added to soil in
!!                               |HRU on day
!!    fixn        |kg N/ha       |amount of nitrogen added to plant biomass via
!!                               |fixation on the day in HRU
!!    grazn       |kg N/ha       |amount of nitrogen added to soil in grazing
!!                               |on the day in HRU
!!    grazp       |kg P/ha       |amount of phosphorus added to soil in grazing
!!                               |on the day in HRU
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on 
!!                               |current day
!!    hhqday(:)   |mm H2O        |surface runoff from HRU for every hour in day
!!    hmntl       |kg N/ha       |amount of nitrogen moving from active
!!                               |organic to nitrate pool in soil profile
!!                               |on current day in HRU
!!    hmptl       |kg P/ha       |amount of phosphorus moving from the
!!                               |organic to labile pool in soil profile
!!                               |on current day in HRU
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates into
!!                               |soil (enters soil)
!!    lat_pst(:)  |kg pst/ha     |amount of pesticide in lateral flow in HRU for
!!                               |the day
!!    latlyr      |mm H2O        |amount of water in lateral flow in layer in 
!!                               |HRU for the day
!!    no3pcp      |kg N/ha       |nitrate added to the soil in rainfall
!!    peakr       |m^3/s         |peak runoff rate for the day in HRU
!!    pet_day     |mm H2O        |potential evapotranspiration for day in HRU
!!    pndev       |m^3 H2O       |evaporation from pond on day
!!    pndflwi     |m^3 H2O       |volume of water flowing into pond on day
!!    pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
!!    pndpcp      |m^3 H2O       |precipitation on pond during day
!!    pndsedc     |metric tons   |net change in sediment in pond during day
!!    pndsedin    |metric tons   |sediment inflow to the pond from HRU
!!    pndsedo     |metric tons   |sediment leaving pond during day
!!    pndsep      |m^3 H2O       |seepage from pond on day
!!    potevmm     |mm H2O        |volume of water evaporated from pothole
!!                               |expressed as depth over HRU
!!    potflwo     |mm H2O        |volume of water released to main channel from
!!                               |pothole exporessed as depth over HRU
!!    potpcpmm    |mm H2O        |precipitation falling on pothole water body
!!                               |expressed as depth over HRU
!!    potsedo     |t/ha          |sediment released to main channel from 
!!                               |HRU
!!    potsepmm    |mm H2O        |seepage from pothole expressed as depth over
!!                               |HRU
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    precipdt(:) |mm H2O        |precipitation for the time step during day
!!    pstsol(:)   |kg pst/ha     |soluble pesticide leached from bottom of 
!!                               |soil profile
!!    qday        |mm H2O        |surface runoff loading to main channel for 
!!                               |day in HRU
!!    qdfr        |none          |fraction of water yield that is surface
!!                               |runoff
!!    qtile       |mm H2O        |drainage tile flow for day in HRU
!!    revapday    |mm H2O        |amount of water moving from the shallow 
!!                               |aquifer into the soil profile or being taken
!!                               |up by plant roots in the shallow aquifer
!!    rmn2tl      |kg N/ha       |amount of nitrogen moving from the fresh
!!                               |organic (residue) to the nitrate(80%) and
!!                               |active organic(20%) pools in soil profile
!!                               |on current day in HRU
!!    rmp1tl      |kg P/ha       |amount of phosphorus moving from the labile
!!                               |mineral pool to the active mineral pool in
!!                               |the soil profile on the current day in the
!!                               |HRU
!!    rmptl       |kg P/ha       |amount of phosphorus moving from the
!!                               |fresh organic (residue) to the labile(80%)
!!                               |and organic(20%) pools in soil profile
!!                               |on current day in HRU
!!    roctl       |kg P/ha       |amount of phosphorus moving from the active
!!                               |mineral pool to the stable mineral pool
!!                               |in the soil profile on the current day in
!!                               |the HRU
!!    rwntl       |kg N/ha       |amount of nitrogen moving from active
!!                               |organic to stable organic pool in soil
!!                               |profile on current day in HRU
!!    sepday      |mm H2O        |percolation from bottom of the soil layer on
!!                               |day in HRU
!!    shallstp    |mm H2O        |depth of water in shallow aquifer in HRU on 
!!                               |previous day
!!    snoev       |mm H2O        |amount of water in snow lost through 
!!                               |sublimation on current day in HRU
!!    snofall     |mm H2O        |amount of precipitation falling as freezing 
!!                               |rain/snow on day in HRU
!!    snomlt      |mm H2O        |amount of water in snow melt for the day in 
!!                               |HRU
!!    snoprev     |mm H2O        |amount of water stored as snow on previous day
!!    sol_rd      |mm            |current rooting depth
!!    soxy        |mg/L          |saturation dissolved oxygen concentration
!!    sw_excess   |mm H2O        |amount of water in soil that exceeds field 
!!                               |capacity (gravity drained water)
!!    swprev      |mm H2O        |amount of water stored in soil profile in the 
!!                               |HRU on the previous day
!!    tloss       |mm H2O        |amount of water removed from surface runoff
!!                               |via transmission losses on day in HRU
!!    twlpnd      |mm H2O        |water lost through seepage from ponds on day
!!                               |in HRU
!!    twlwet      |mm H2O        |water lost through seepage from wetlands on
!!                               |day in HRU
!!    uno3d       |kg N/ha       |plant nitrogen deficiency for day in HRU
!!    usle        |metric tons/ha|daily soil loss predicted with USLE equation
!!    usle_ei     |none          |USLE erodibility index on day for HRU
!!    vpd         |kPa           |vapor pressure deficit
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    wdntl       |kg N/ha       |amount of nitrogen lost from nitrate pool
!!                               |by denitrification in soil profile on
!!                               |current day in HRU
!!    wetev       |m^3 H2O       |evaporation from wetland for day
!!    wetflwi     |m^3 H2O       |volume of water flowing in wetland on day
!!    wetflwo     |m^3 H2O       |volume of water flowing out wetland on day
!!    wetpcp      |m^3 H2O       |precipitation on wetland for day
!!    wetsedc     |metric tons   |net change in sediment in wetland during day
!!    wetsedi     |metric tons   |sediment loading to wetland for day
!!    wetsedo     |metric tons   |sediment loading from wetland for day
!!    wetsep      |m^3 H2O       |seepage from wetland bottom for day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |HRU number
!!    ly          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, ly

      j = 0
      j = ihru

      !!initialize variables
        al5 = 0.
        albday = 0.
        auton = 0.
        autop = 0.
        bactlchlp = 0.
        bactlchp = 0.
        bactrolp = 0.
        bactrop = 0.
        bactsedlp = 0.
        bactsedp = 0.
        bioday = 0.
        bsprev = 0.
        bssprev = 0.
        canev = 0.
        cfertn = 0.
        cfertp = 0.
        crk = 0.
        deepstp = 0.
        deepstp = deepst(j)
        enratio = 0.
        ep_day = 0.
        ep_max = 0.
        es_day = 0.
        etday = 0.
        fertn = 0.
        fertp = 0.
        fixn = 0.
        grazn = 0.
        grazp = 0.
        gwseep = 0.
        hhqday = 0.
        hmntl = 0.
        hmptl = 0.
        inflpcp = 0.
        lat_pst = 0.
        latlyr = 0.
        lyrtile = 0.
        no3pcp = 0.
        peakr = 0.
        pet_day = 0.
        pndev = 0.
        pndflwi = 0.
        pndflwo = 0.
        pndpcp = 0.
        pndsedc = 0.
        pndsedin = 0.
        pndsedo = 0.

        pndsanin = 0.
        pndsilin = 0.
        pndclain = 0.
        pndsagin = 0.
        pndlagin = 0.
        pndsano = 0.
        pndsilo = 0.
        pndclao = 0.
        pndsago = 0.
        pndlago = 0.

        pndsep = 0.
        potevmm = 0.
        potflwo = 0.
        potpcpmm = 0.
        potsedo = 0.

        potsano = 0.
        potsilo = 0.
        potclao = 0.
        potsago = 0.
        potlago = 0.

        potsepmm = 0.
         precipday = 0.
         precipday = subp(j)
         precipdt = 0.
         if (nstep > 0) then
           do ii = 1, nstep
             precipdt(ii+1) = rainsub(j,ii)
           end do
         end if
        pstsol = 0.
        qday = 0.
        qdfr = 0.
        qtile = 0.
        revapday = 0.
        rmn2tl = 0.
        rmp1tl = 0.
        rmptl = 0.
        roctl = 0.
        rwntl = 0.
        sepday = 0.
        shallstp = 0.
        shallstp = shallst(j)
        snoev = 0.
        snofall = 0.
        snomlt = 0.
        snoprev = 0.
        snoprev = sno_hru(j)
        sol_rd = 0.
        soxy = 0.
        sw_excess = 0.
        swprev = 0.
        swprev = sol_sw(j)
        tloss = 0.
        twlpnd = 0.
        twlwet = 0.
        uno3d = 0.
        usle = 0.
        usle_ei = 0.
        vpd = 0.
        voltot = 0.
        wdntl = 0.
        wetev = 0.
        wetflwi = 0.
        wetflwo = 0.
        wetpcp = 0.
        wetsedc = 0.
        wetsedi = 0.
        wetsedo = 0.
        wetsep = 0.
        
	!! urban modeling by J.Jeong
	  sedprev = 0.
	  ubnrunoff = 0.
	  irmmdt = 0.
        hhsedy = 0.
        ubntss = 0.

       return
       end