      subroutine sumv

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs summary calculations for HRU

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert
!!                               |application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert
!!                               |application
!!    bactrolp    |# colonies/ha |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# colonies/ha |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# colonies/ha |less persistent bacteria transported with
!!                               |sediment in surface runoff
!!    bactsedp    |# colonies/ha |persistent bacteria transported with
!!                               |sediment in surface runoff
!!    cfertn      |kg N/ha       |amount of nitrogen added to soil in continuous
!!                               |fertilizer operation on day
!!    cfertp      |kg P/ha       |amount of phosphorus added to soil in continuous
!!                               |fertilizer operation on day
!!    cnday(:)    |none          |curve number for current day, HRU and at
!!                               |current soil moisture
!!    curyr       |none          |current year of simulation
!!    deepirr(:)  |mm H2O        |amount of water removed from deep aquifer
!!                               |for irrigation
!!    ep_day      |mm H2O        |actual amount of transpiration that occurs on
!!                               |day in HRU
!!    es_day      |mm H2O        |actual amount of evaporation (from soil) that
!!                               |occurs on day in HRU
!!    etday       |mm H2O        |actual amount of evapotranspiration that
!!                               |occurs on day in HRU
!!    fertn       |kg N/ha       |total amount of nitrogen added to soil in HRU
!!                               |on day in fertilizer application
!!    fertp       |kg P/ha       |total amount of phosphorus added to soil in
!!                               |HRU on day in fertilizer application
!!    fixn        |kg N/ha       |amount of nitrogen added to plant biomass via
!!                               |fixation on the day in HRU
!!    grazn       |kg N/ha       |amount of nitrogen added to soil in grazing
!!                               |on the day in HRU
!!    grazp       |kg P/ha       |amount of phosphorus added to soil in grazing
!!                               |on the day in HRU
!!    gw_q(:)     |mm H2O        |groundwater contribution to streamflow from
!!                               |HRU on current day
!!    gwseep      |mm H2O        |amount of water recharging deep aquifer on
!!                               |current day
!!    hmntl       |kg N/ha       |amount of nitrogen moving from active
!!                               |organic to nitrate pool in soil profile
!!                               |on current day in HRU
!!    hmptl       |kg P/ha       |amount of phosphorus moving from the
!!                               |organic to labile pool in soil profile
!!                               |on current day in HRU
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    ihru        |none          |HRU number
!!    lat_pst(:)  |kg pst/ha     |amount of pesticide in lateral flow in HRU
!!                               |for the day
!!    latno3(:)   |kg N/ha       |amount of NO3-N in lateral flow in HRU for the
!!                               |day
!!    latq(:)     |mm H2O        |amount of water in lateral flow in HRU for the
!!                               |day
!!    minpgw(:)   |kg P/ha       |soluble P loading to reach in groundwater
!!    no3gw(:)    |kg N/ha       |nitrate loading to reach in groundwater
!!    no3pcp      |kg N/ha       |nitrate added to the soil in rainfall
!!    nplnt(:)    |kg N/ha       |plant uptake of nitrogen in HRU for the day
!!    npmx        |none          |number of different pesticides used in
!!                               |the simulation
!!    nyskip      |none          |number of years to skip output summarization
!!                               |and printing
!!    percn(:)    |kg N/ha       |NO3-N leached from soil profile during the day
!!    pet_day     |mm H2O        |potential evapotranspiration for day in HRU
!!    pndev       |m^3 H2O       |evaporation from pond on day
!!    pndflwi     |m^3 H2O       |volume of water flowing into pond on day
!!    pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
!!    pndpcp      |m^3 H2O       |precipitation on pond during day
!!    pndsedc     |metric tons   |net change in sediment in pond during day
!!    pndsedin    |metric tons   |sediment entering pond during day
!!    pndsedo     |metric tons   |sediment leaving pond during day
!!    pndsep      |m^3 H2O       |seepage from pond on day
!!    potevmm     |mm H2O        |volume of water evaporated from pothole
!!                               |expressed as depth over HRU
!!    potflwi(:)  |m^3 H2O       |water entering pothole on day
!!    potflwo     |mm H2O        |discharge from pothole expressed as depth
!!                               |over HRU
!!    potpcpmm    |mm H2O        |precipitation falling on pothole water body
!!                               |expressed as depth over HRU
!!    potsedi(:)  |metric tons   |sediment entering pothole on day
!!    potsedo     |metric tons   |sediment leaving pothole on day
!!    potsepmm    |mm H2O        |seepage from pothole expressed as depth over
!!                               |HRU
!!    pplnt(:)    |kg P/ha       |plant uptake of phosphorus in HRU for the day
!!    pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto
!!                               |sediment
!!    pst_surq(:,:)|kg/ha        |amount of pesticide type lost in surface
!!                               |runoff on current day in HRU
!!    pstsol(:)   |kg/ha         |amount of pesticide type leached from soil
!!                               |profile on current day
!!    qday        |mm H2O        |surface runoff loading to main channel for
!!                               |day in HRU
!!    qdr(:)      |mm H2O        |total amount of water entering main channel
!!                               |for day from HRU
!!    qtile       |mm H2O        |drainage tile flow for day in HRU
!!    rchrg(:)    |mm H2O        |amount of water recharging both aquifers on
!!                               |current day in HRU
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
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    sedorgp(:)  |kg P/ha       |amount of organic phosphorus in surface runoff
!!                               |in HRU for the day
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion
!!    sepbtm(:)   |mm H2O        |seepage leaving the bottom of the soil profile
!!                               |on day in HRU
!!    shallirr(:) |mm H2O        |amount of water removed from shallow aquifer
!!                               |for irrigation
!!    snoev       |mm H2O        |amount of water in snow lost through
!!                               |sublimation on current day in HRU
!!    snofall     |mm H2O        |amount of precipitation falling as freezing
!!                               |rain/snow on day in HRU
!!    snomlt      |mm H2O        |amount of water in snow melt for the day in
!!                               |HRU
!!    sol_cnsw(:) |mm H2O        |soil water content used to calculate daily
!!                               |CN value (initial soil wter content for day)
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on any given day
!!    sol_tmp(2,:)|deg C         |daily average temperature of second soil layer
!!    strsn(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |nitrogen stress
!!    strsp(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |phosphorus stress
!!    strstmp(:)  |none          |fraction of potential plant growth achieved on
!!                               |the day in HRU where the reduction is caused
!!                               |by temperature stress
!!    strsw(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |water stress
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    surqsolp(:) |kg P/ha       |amount of soluble phosphorus in surface runoff
!!                               |in HRU for the day
!!    tloss       |mm H2O        |amount of water removed from surface runoff
!!                               |via transmission losses on day in HRU
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    usle        |metric tons   |daily soil loss predicted with USLE equation
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupstd(:,1,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU on day
!!                               |(in solution)
!!    hrupstd(:,2,:)|mg pst      |amount of pesticide type in surface runoff
!!                               |contribution to stream from HRU on day
!!                               |(sorbed to sediment)
!!    hrupstd(:,3,:)|mg pst/ha   |total pesticide loading to stream in surface
!!                               |runoff from HRU
!!    hrupstd(:,4,:)|mg pst      |amount of pesticide type in lateral flow
!!                               |contribution to stream from HRU on day
!!                               |(in solution)
!!    hrumono(1,:)|mm H2O        |precipitation in HRU during month
!!    hrumono(2,:)|mm H2O        |amount of precipitation falling as freezing
!!                               |rain/snow in HRU during month
!!    hrumono(3,:)|mm H2O        |amount of snow melt in HRU during month
!!    hrumono(4,:)|mm H2O        |amount of surface runoff to main channel 
!!                               |from HRU during month (ignores impact of
!!                               |transmission losses)
!!    hrumono(5,:)|mm H2O        |amount of lateral flow contribution to main
!!                               |channel from HRU during month
!!    hrumono(6,:)|mm H2O        |amount of groundwater flow contribution to
!!                               |main channel from HRU during month
!!    hrumono(7,:)|mm H2O        |amount of water moving from shallow aquifer
!!                               |to plants or soil profile in HRU during month
!!    hrumono(8,:)|mm H2O        |amount of water recharging deep aquifer in
!!                               |HRU during month
!!    hrumono(9,:)|mm H2O        |total amount of water entering both aquifers
!!                               |from HRU during month
!!    hrumono(10,:)|mm H2O        |water yield (total amount of water entering
!!                               |main channel) from HRU during month
!!    hrumono(11,:)|mm H2O        |amount of water percolating out of the soil
!!                               |profile and into the vadose zone in HRU
!!                               |during month
!!    hrumono(12,:)|mm H2O        |actual evapotranspiration in HRU during month
!!    hrumono(13,:)|mm H2O        |amount of transmission losses from tributary
!!                               |channels in HRU for month
!!    hrumono(14,:)|metric tons/ha|sediment yield from HRU for month
!!    hrumono(15,:)|mm H2O        |actual amount of transpiration that occurs
!!                               |during month in HRU
!!    hrumono(16,:)|mm H2O        |actual amount of evaporation (from soil) that
!!                               |occurs during month in HRU
!!    hrumono(17,:)|kg N/ha       |amount of nitrogen applied in continuous 
!!                               |fertilizer operation during month in HRU 
!!    hrumono(18,:)|kg P/ha       |amount of phosphorus applied in continuous 
!!                               |fertilizer operation during month in HRU 
!!    hrumono(19,:)|mm H2O        |amount of surface runoff generated during month
!!                               |in HRU
!!    hrumono(20,:)|none          |CN values during month in HRU
!!    hrumono(21,:)|mm H2O        |sum of daily soil water values used to calculate
!!                               |the curve number
!!    hrumono(23,:)|mm H2O        |amount of water removed from shallow aquifer
!!                               |in HRU for irrigation during month
!!    hrumono(24,:)|mm H2O        |amount of water removed from deep aquifer
!!                               |in HRU for irrigation during month
!!    hrumono(25,:)|mm H2O        |potential evapotranspiration in HRU during
!!                               |month
!!    hrumono(26,:)|kg N/ha       |monthly amount of N (organic & mineral)
!!                               |applied in HRU during grazing
!!    hrumono(27,:)|kg P/ha       |monthly amount of P (organic & mineral)
!!                               |applied in HRU during grazing
!!    hrumono(28,:)|kg N/ha       |monthly amount of N (organic & mineral)
!!                               |auto-applied in HRU
!!    hrumono(29,:)|kg P/ha       |monthly amount of P (organic & mineral)
!!                               |auto-applied in HRU
!!    hrumono(30,:)|deg C         |sum of daily soil temperature values
!!    hrumono(31,:)|stress days   |water stress days in HRU during month
!!    hrumono(32,:)|stress days   |temperature stress days in HRU during month
!!    hrumono(33,:)|stress days   |nitrogen stress days in HRU during month
!!    hrumono(34,:)|stress days   |phosphorus stress days in HRU during month
!!    hrumono(35,:)|kg N/ha       |organic nitrogen in surface runoff in HRU
!!                               |during month
!!    hrumono(36,:)|kg P/ha       |organic phosphorus in surface runoff in HRU
!!                               |during month
!!    hrumono(37,:)|kg N/ha       |nitrate in surface runoff in HRU during month
!!    hrumono(38,:)|kg N/ha       |nitrate in lateral flow in HRU during month
!!    hrumono(39,:)|kg P/ha       |soluble phosphorus in surface runoff in HRU
!!                               |during month
!!    hrumono(40,:)|kg N/ha       |amount of nitrogen removed from soil by plant
!!                               |uptake in HRU during month
!!    hrumono(41,:)|kg N/ha       |nitrate percolating past bottom of soil 
!!                               |profile in HRU during month
!!    hrumono(42,:)|kg P/ha       |amount of phosphorus removed from soil by 
!!                               |plant uptake in HRU during month
!!    hrumono(43,:)|kg P/ha       |amount of phosphorus moving from labile
!!                               |mineral to active mineral pool in HRU during
!!                               |month
!!    hrumono(44,:)|kg P/ha       |amount of phosphorus moving from active
!!                               |mineral to stable mineral pool in HRU during
!!                               |month
!!    hrumono(45,:)|kg N/ha       |amount of nitrogen applied to HRU in 
!!                               |fertilizer and grazing operations during month
!!    hrumono(46,:)|kg P/ha       |amount of phosphorus applied to HRU in 
!!                               |fertilizer and grazing operations during month
!!    hrumono(47,:)|kg N/ha       |amount of nitrogen added to soil by fixation
!!                               |in HRU during month
!!    hrumono(48,:)|kg N/ha       |amount of nitrogen lost by denitrification
!!                               |in HRU during month
!!    hrumono(49,:)|kg N/ha       |amount of nitrogen moving from active organic
!!                               |to nitrate pool in HRU during month
!!    hrumono(50,:)|kg N/ha       |amount of nitrogen moving from active organic
!!                               |to stable organic pool in HRU during month
!!    hrumono(51,:)|kg P/ha       |amount of phosphorus moving from organic to
!!                               |labile mineral pool in HRU during month
!!    hrumono(52,:)|kg N/ha       |amount of nitrogen moving from fresh organic
!!                               |to nitrate and active organic pools in HRU
!!                               |during month
!!    hrumono(53,:)|kg P/ha       |amount of phosphorus moving from fresh
!!                               |organic to the labile mineral and organic
!!                               |pools in HRU during month
!!    hrumono(54,:)|kg N/ha       |amount of nitrogen added to soil in rain
!!    hrumono(61,:)|metric tons/ha|daily soil loss predicted with USLE equation
!!    hrumono(62,:)|mm H2O        |drainage tile flow contribution to main 
!!                               |channel from HRU in month
!!    hrumono(63,:)|# bacteria/ha |persistent bacteria transported to main 
!!                               |channel from HRU during month
!!    hrumono(64,:)|# bacteria/ha |less persistent bacteria transported to main
!!                               |channel from HRU during month
!!    hrumono(65,:)|kg N/ha       |nitrate loading from groundwater in HRU to
!!                               |main channel during month
!!    hrumono(66,:)|kg P/ha       |soluble P loading from groundwater in HRU to
!!                               |main channel during month
!!    hrumono(67,:)|kg P/ha       |loading of mineral P attached to sediment
!!                               |in HRU to main channel during month
!!    wpstdayo(:,1) |mg pst/ha   |amount of pesticide type in surface runoff
!!                               |contribution to stream in watershed on day
!!                               |(in solution)
!!    wpstdayo(:,2) |mg pst/ha   |amount of pesticide type in surface runoff
!!                               |contribution to stream in watershed on day
!!                               |(sorbed to sediment)
!!    wpstdayo(:,3) |kg pst/ha   |amount of pesticide type leached from soil
!!                               |profile in watershed on day
!!    wpstdayo(:,4) |kg pst/ha   |amount of pesticide type in lateral flow
!!                               |contribution to stream in watershed on day
!!    wshddayo(1) |mm H2O        |average amount of precipitation in watershed
!!                               |for the day
!!    wshddayo(3) |mm H2O        |surface runoff in watershed for day
!!    wshddayo(4) |mm H2O        |lateral flow contribution to streamflow in 
!!                               |watershed for day
!!    wshddayo(5) |mm H2O        |water percolation past bottom of soil profile
!!                               |in watershed for day
!!    wshddayo(6) |mm H2O        |water yield to streamflow from HRUs in
!!                               |watershed for day
!!    wshddayo(7) |mm H2O        |actual evapotranspiration in watershed
!!                               |for day
!!    wshddayo(8) |deg C         |average maximum temperature in watershed for
!!                               |the day
!!    wshddayo(9) |deg C         |average minimum temperature in watershed for
!!                               |the day
!!    wshddayo(12)|metric tons   |sediment yield from HRUs in watershed for day
!!    wshddayo(13)|metric tons   |sediment loading to ponds in watershed for day
!!    wshddayo(14)|metric tons   |sediment loading from ponds in watershed for
!!                               |day
!!    wshddayo(15)|metric tons   |net change in sediment level in ponds in
!!                               |watershed for day
!!    wshddayo(16)|metric tons   |sediment loading to wetlands for day
!!                               |in watershed
!!    wshddayo(17)|metric tons   |sediment loading to main channels from
!!                               |wetlands for day in watershed
!!    wshddayo(18)|metric tons   |net change in sediment in wetlands for day
!!                               |in watershed
!!    wshddayo(19)|m^3 H2O       |evaporation from ponds in watershed for day
!!    wshddayo(20)|m^3 H2O       |seepage from ponds in watershed for day
!!    wshddayo(21)|m^3 H2O       |precipitation on ponds in watershed for day
!!    wshddayo(22)|m^3 H2O       |volume of water entering ponds in watershed
!!                               |for day
!!    wshddayo(23)|m^3 H2O       |volume of water leaving ponds in watershed
!!                               |for day
!!    wshddayo(24)|m^3 H2O       |evaporation from wetlands for day in watershed
!!    wshddayo(25)|m^3 H2O       |seepage from wetlands for day in watershed
!!    wshddayo(26)|m^3 H2O       |precipitation on wetlands for day in watershed
!!    wshddayo(27)|m^3 H2O       |volume of water entering wetlands on day in
!!                               |watershed
!!    wshddayo(28)|m^3 H2O       |volume of water leaving wetlands on day in
!!                               |watershed
!!    wshddayo(33)|m^3 H2O       |net change in water volume of ponds in
!!                               |watershed for day
!!    wshddayo(35)|mm H2O        |amount of water stored in soil profile in
!!                               |watershed for day
!!    wshddayo(36)|mm H2O        |snow melt in watershed for day
!!    wshddayo(37)|mm H2O        |sublimation in watershed for day
!!    wshddayo(38)|mm H2O        |average amount of tributary channel
!!                               |transmission losses in watershed on day
!!    wshddayo(39)|mm H2O        |freezing rain/snow fall in watershed for day
!!    wshddayo(40)|kg N/ha       |organic N loading to stream in watershed for
!!                               |day
!!    wshddayo(41)|kg P/ha       |organic P loading to stream in watershed for
!!                               |day
!!    wshddayo(42)|kg N/ha       |nitrate loading to stream in surface runoff
!!                               |in watershed for day
!!    wshddayo(43)|kg P/ha       |soluble P loading to stream in watershed for
!!                               |day
!!    wshddayo(44)|kg N/ha       |plant uptake of N in watershed for day
!!    wshddayo(45)|kg N/ha       |nitrate loading to stream in lateral flow
!!                               |in watershed for day
!!    wshddayo(46)|kg N/ha       |nitrate percolation past bottom of soil 
!!                               |profile in watershed for day
!!    wshddayo(104)|mm H2O        |groundwater contribution to stream in
!!                               |watershed on day
!!    wshddayo(105)|mm H2O        |amount of water moving from shallow aquifer
!!                               |to plants/soil profile in watershed on day
!!    wshddayo(106)|mm H2O        |deep aquifer recharge in watershed on day
!!    wshddayo(107)|mm H2O        |total amount of water entering both aquifers
!!                               |in watershed on day
!!    wshddayo(108)|mm H2O        |potential evapotranspiration in watershed
!!                               |on day
!!    wshddayo(109)|mm H2O        |drainage tile flow contribution to stream
!!                               |in watershed on day
!!    wshddayo(110)|kg/ha        |NO3 yield (gwq)                          
!!    wshddayo(111)|mm H2O       |NO3 yield (tile)                          
!!    wtrmon(1,:) |mm H2O        |evaporation from ponds in HRU for month
!!    wtrmon(2,:) |mm H2O        |seepage from ponds in HRU for month
!!    wtrmon(3,:) |mm H2O        |precipitation on ponds in HRU for month
!!    wtrmon(4,:) |mm H2O        |amount of water entering ponds in HRU for
!!                               |month
!!    wtrmon(5,:) |metric tons/ha|sediment entering ponds in HRU for month
!!    wtrmon(6,:) |mm H2O        |amount of water leaving ponds in HRU for
!!                               |month
!!    wtrmon(7,:) |metric tons/ha|sediment leaving ponds in HRU for month
!!    wtrmon(8,:) |mm H2O        |precipitation on wetlands in HRU for month
!!    wtrmon(9,:) |mm H2O        |volume of water entering wetlands from HRU
!!                               |for month
!!    wtrmon(10,:)|metric tons/ha|sediment loading to wetlands for month from
!!                               |HRU
!!    wtrmon(11,:)|mm H2O        |evaporation from wetlands in HRU for month
!!    wtrmon(12,:)|mm H2O        |seeepage from wetlands in HRU for month
!!    wtrmon(13,:)|mm H2O        |volume of water leaving wetlands in HRU
!!                               |for month
!!    wtrmon(14,:)|metric tons/ha|sediment loading from wetlands in HRU to main
!!                               |channel during month
!!    wtrmon(15,:)|mm H2O        |precipitation on potholes in HRU for month
!!    wtrmon(16,:)|mm H2O        |evaporation from potholes in HRU for month
!!    wtrmon(17,:)|mm H2O        |seepage from potholes in HRU for month
!!    wtrmon(18,:)|mm H2O        |water leaving potholes in HRU for month
!!    wtrmon(19,:)|mm H2O        |water entering potholes in HRU for month
!!    wtrmon(20,:)|metric tons/ha|sediment entering potholes in HRU for month
!!    wtrmon(21,:)|metric tons/ha|sediment leaving potholes in HRU for month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    j           |none          |HRU number
!!    k           |none          |counter (pesticides)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, k
      real :: cnv

      j = 0
      j = ihru
  
      cnv = 0.
      cnv = 10. * hru_ha(j)

      if (curyr > nyskip) then
      !! HRU summations
        hrumono(1,j) = hrumono(1,j) + subp(j)
        hrumono(2,j) = hrumono(2,j) + snofall
        hrumono(3,j) = hrumono(3,j) + snomlt
        hrumono(4,j) = hrumono(4,j) + qday
        hrumono(5,j) = hrumono(5,j) + latq(j)
        hrumono(6,j) = hrumono(6,j) + gw_q(j)
        hrumono(7,j) = hrumono(7,j) + revapday
        hrumono(8,j) = hrumono(8,j) + gwseep
        hrumono(9,j) = hrumono(9,j) + rchrg(j)
        hrumono(10,j) = hrumono(10,j) + qdr(j)
        hrumono(11,j) = hrumono(11,j) + sepbtm(j)
        hrumono(12,j) = hrumono(12,j) + etday
        hrumono(13,j) = hrumono(13,j) + tloss
        hrumono(14,j) = hrumono(14,j) + sedyld(j) / hru_ha(j)
        hrumono(15,j) = hrumono(15,j) + ep_day
        hrumono(16,j) = hrumono(16,j) + es_day
        hrumono(17,j) = hrumono(17,j) + cfertn
        hrumono(18,j) = hrumono(18,j) + cfertp
        hrumono(19,j) = hrumono(19,j) + surfq(j)
        hrumono(20,j) = hrumono(20,j) + cnday(j)
        hrumono(21,j) = hrumono(21,j) + sol_cnsw(j)
         !hrumono(22,j) calculated in irrigate.f
        hrumono(23,j) = hrumono(23,j) + shallirr(j)
        hrumono(24,j) = hrumono(24,j) + deepirr(j)
        hrumono(25,j) = hrumono(25,j) + pet_day
        hrumono(26,j) = hrumono(26,j) + grazn
        hrumono(27,j) = hrumono(27,j) + grazp
        hrumono(28,j) = hrumono(28,j) + auton
        hrumono(29,j) = hrumono(29,j) + autop
        hrumono(30,j) = hrumono(30,j) + sol_tmp(2,j)
        hrumono(31,j) = hrumono(31,j) + (1.-strsw(j))
        hrumono(32,j) = hrumono(32,j) + (1.-strstmp(j))
        hrumono(33,j) = hrumono(33,j) + (1.-strsn(j))
        hrumono(34,j) = hrumono(34,j) + (1.-strsp(j))
        hrumono(35,j) = hrumono(35,j) + sedorgn(j)
        hrumono(36,j) = hrumono(36,j) + sedorgp(j)
        hrumono(37,j) = hrumono(37,j) + surqno3(j)
        hrumono(38,j) = hrumono(38,j) + latno3(j)
        hrumono(39,j) = hrumono(39,j) + surqsolp(j)
        hrumono(40,j) = hrumono(40,j) + nplnt(j)
        hrumono(41,j) = hrumono(41,j) + percn(j)
        hrumono(42,j) = hrumono(42,j) + pplnt(j)
        hrumono(43,j) = hrumono(43,j) + rmp1tl
        hrumono(44,j) = hrumono(44,j) + roctl
        hrumono(45,j) = hrumono(45,j) + fertn
        hrumono(46,j) = hrumono(46,j) + fertp
        hrumono(47,j) = hrumono(47,j) + fixn
        hrumono(48,j) = hrumono(48,j) + wdntl
        hrumono(49,j) = hrumono(49,j) + hmntl
        hrumono(50,j) = hrumono(50,j) + rwntl
        hrumono(51,j) = hrumono(51,j) + hmptl
        hrumono(52,j) = hrumono(52,j) + rmn2tl
        hrumono(53,j) = hrumono(53,j) + rmptl
        hrumono(54,j) = hrumono(54,j) + no3pcp
        hrumono(55,j) = hrumono(55,j) + tmx(j)
        hrumono(56,j) = hrumono(56,j) + tmn(j)
        hrumono(57,j) = hrumono(57,j) + tmpav(j)
        hrumono(58,j) = hrumono(58,j) + hru_ra(j)

        hrumono(61,j) = hrumono(61,j) + usle
        hrumono(62,j) = hrumono(62,j) + qtile
        hrumono(63,j) = hrumono(63,j) + bactrop + bactsedp
        hrumono(64,j) = hrumono(64,j) + bactrolp + bactsedlp
        hrumono(65,j) = hrumono(65,j) + no3gw(j)
        hrumono(66,j) = hrumono(66,j) + minpgw(j)
        hrumono(67,j) = hrumono(67,j) + sedminpa(j) + sedminps(j)
        hrumono(68,j) = hrumono(68,j) + tileno3(j)
        hrumono(69,j) = hrumono(69,j) + latno3(j)
        hrumono(70,j) = hrumono(70,j) + gw_qdeep(j)
        hrumono(71,j) = hrumono(71,j) + latq(j) - lpndloss - lwetloss

        wtrmon(1,j) = wtrmon(1,j) + pndev / cnv
        wtrmon(2,j) = wtrmon(2,j) + pndsep / cnv
        wtrmon(3,j) = wtrmon(3,j) + pndpcp / cnv
        wtrmon(4,j) = wtrmon(4,j) + pndflwi / cnv
        wtrmon(5,j) = wtrmon(5,j) + pndsedin / hru_ha(j)
        wtrmon(6,j) = wtrmon(6,j) + pndflwo / cnv
        wtrmon(7,j) = wtrmon(7,j) + pndsedo / hru_ha(j)
        wtrmon(8,j) = wtrmon(8,j) + wetpcp / cnv
        wtrmon(9,j) = wtrmon(9,j) + wetflwi / cnv
        wtrmon(10,j) = wtrmon(10,j) + wetsedi / hru_ha(j)
        wtrmon(11,j) = wtrmon(11,j) + wetev / cnv
        wtrmon(12,j) = wtrmon(12,j) + wetsep / cnv
        wtrmon(13,j) = wtrmon(13,j) + wetflwo / cnv
        wtrmon(14,j) = wtrmon(14,j) + wetsedo / hru_ha(j)
        wtrmon(15,j) = wtrmon(15,j) + potpcpmm
        wtrmon(16,j) = wtrmon(16,j) + potevmm
        wtrmon(17,j) = wtrmon(17,j) + potsepmm
        wtrmon(18,j) = wtrmon(18,j) + potflwo
        wtrmon(19,j) = wtrmon(19,j) + potflwi(j) / cnv
        wtrmon(20,j) = wtrmon(20,j) + potsedi(j) / hru_ha(j)
        wtrmon(21,j) = wtrmon(21,j) + potsedo / hru_ha(j)

      !! watershed summations
        if (ffcst == 0 .and. iscen == 1) then
        wshddayo(1) = wshddayo(1) + subp(j) * hru_dafr(j)
        wshddayo(3) = wshddayo(3) + surfq(j) * hru_dafr(j)
        wshddayo(4) = wshddayo(4) + latq(j) * hru_dafr(j)
        wshddayo(5) = wshddayo(5) + sepbtm(j) * hru_dafr(j)
        wshddayo(6) = wshddayo(6) + qdr(j) * hru_dafr(j)
        wshddayo(7) = wshddayo(7) + etday * hru_dafr(j)
        wshddayo(8) = wshddayo(8) + tmx(j) * hru_dafr(j)
        wshddayo(9) = wshddayo(9) + tmn(j) * hru_dafr(j)
        wshddayo(12) = wshddayo(12) + sedyld(j)
        wshddayo(13) = wshddayo(13) + pndsedin
        wshddayo(14) = wshddayo(14) + pndsedo
        wshddayo(15) = wshddayo(15) + pndsedc
        wshddayo(16) = wshddayo(16) + wetsedi
        wshddayo(17) = wshddayo(17) + wetsedo
        wshddayo(18) = wshddayo(18) + wetsedc
        wshddayo(19) = wshddayo(19) + pndev
        wshddayo(20) = wshddayo(20) + pndsep
        wshddayo(21) = wshddayo(21) + pndpcp
        wshddayo(22) = wshddayo(22) + pndflwi
        wshddayo(23) = wshddayo(23) + pndflwo
        wshddayo(24) = wshddayo(24) + wetev
        wshddayo(25) = wshddayo(25) + wetsep
        wshddayo(26) = wshddayo(26) + wetpcp
        wshddayo(27) = wshddayo(27) + wetflwi
        wshddayo(28) = wshddayo(28) + wetflwo
        wshddayo(33) = wshddayo(33) + pndflwi - pndflwo
        wshddayo(35) = wshddayo(35) + sol_sw(j) * hru_dafr(j)
        wshddayo(36) = wshddayo(36) + snomlt * hru_dafr(j)
        wshddayo(37) = wshddayo(37) + snoev * hru_dafr(j)
        wshddayo(38) = wshddayo(38) + tloss * hru_dafr(j)
        wshddayo(39) = wshddayo(39) + snofall * hru_dafr(j)
        wshddayo(40) = wshddayo(40) + sedorgn(j) * hru_dafr(j)
        wshddayo(41) = wshddayo(41) + sedorgp(j) * hru_dafr(j)
        wshddayo(42) = wshddayo(42) + surqno3(j) * hru_dafr(j)
        wshddayo(43) = wshddayo(43) + surqsolp(j) * hru_dafr(j)
        wshddayo(44) = wshddayo(44) + nplnt(j) * hru_dafr(j)
        wshddayo(45) = wshddayo(45) + latno3(j) * hru_dafr(j)
        wshddayo(46) = wshddayo(46) + percn(j) * hru_dafr(j)

        !! wshddayo(47) - wshddayo (103) not used

        wshddayo(104) = wshddayo(104) + gw_q(j) * hru_dafr(j)
        wshddayo(105) = wshddayo(105) + revapday * hru_dafr(j)
        wshddayo(106) = wshddayo(106) + gwseep * hru_dafr(j)
        wshddayo(107) = wshddayo(107) + rchrg(j) * hru_dafr(j)
        wshddayo(108) = wshddayo(108) + pet_day * hru_dafr(j)
        wshddayo(109) = wshddayo(109) + qtile * hru_dafr(j)
        wshddayo(110) = wshddayo(110) + no3gw(j) * hru_dafr(j)
        wshddayo(111) = wshddayo(111) + tileno3(j) * hru_dafr(j)
        wshddayo(113) = wshddayo(113) + gw_qdeep(j) * hru_dafr(j)     
          do ii=1,mstdo
            if(wshddayo(ii).ne.wshddayo(ii)) wshddayo(ii) = 0  !! float error debug, Jaehak Jeong, 2011 Feb
          end do     
        
        else if (ffcst == 1) then
          if (j == 1) fcstcnt = fcstcnt + 1
          fcstaao(1) = fcstaao(1) + subp(j) * hru_dafr(j)
          fcstaao(2) = fcstaao(2) + snofall * hru_dafr(j)
          fcstaao(3) = fcstaao(3) + snomlt * hru_dafr(j)
          fcstaao(4) = fcstaao(4) + snoev * hru_dafr(j)
          fcstaao(5) = fcstaao(5) + (qday + tloss) * hru_dafr(j)
          fcstaao(6) = fcstaao(6) + latq(j) * hru_dafr(j)
          fcstaao(7) = fcstaao(7) + qtile * hru_dafr(j)
          fcstaao(8) = fcstaao(8) + gw_q(j) * hru_dafr(j)
          fcstaao(9) = fcstaao(9) + revapday * hru_dafr(j)
          fcstaao(10) = fcstaao(10) + gwseep * hru_dafr(j)
          fcstaao(11) = fcstaao(11) + rchrg(j) * hru_dafr(j)
          fcstaao(12) = fcstaao(12) + qdr(j) * hru_dafr(j)
          fcstaao(13) = fcstaao(13) + sepbtm(j) * hru_dafr(j)
          fcstaao(14) = fcstaao(14) + etday * hru_dafr(j)
          fcstaao(15) = fcstaao(15) + pet_day * hru_dafr(j)
          fcstaao(16) = fcstaao(16) + tloss * hru_dafr(j)
        end if

        !! pesticide summary
        if (hrupest(j) == 1) then
          do k = 1, npmx
            !! HRU summary
            hrupstd(k,1,j) = pst_surq(k,j) * 1.e6 * hru_ha(j)
            hrupstd(k,2,j) = pst_sed(k,j) * 1.e6 * hru_ha(j)
            hrupstd(k,3,j) = (pst_surq(k,j) + pst_sed(k,j)) * 1.e6
            hrupstd(k,4,j) = lat_pst(k) * 1.e6 * hru_ha(j)
            !! watershed summary
            wpstdayo(k,1) = wpstdayo(k,1) + pst_surq(k,j) * hru_dafr(j) &
     &                                                            * 1.e6
            wpstdayo(k,2) = wpstdayo(k,2) + pst_sed(k,j) * hru_dafr(j) *&
     &                                                              1.e6
            wpstdayo(k,3) = wpstdayo(k,3) + pstsol(k) * hru_dafr(j)
            wpstdayo(k,4) = wpstdayo(k,4) + lat_pst(k) * hru_dafr(j)
          end do
        end if
      end if

      return
      end
