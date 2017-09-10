      subroutine stdaa

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual output to .std file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aairr(:)    |mm H2O        |average annual amount of irrigation water
!!                               |applied to HRU
!!    basminpf    |kg P/ha       |final average amount of phosphorus in
!!                               |the mineral P pool in watershed soil
!!    basminpi    |kg P/ha       |initial average amount of phosphorus in
!!                               |the mineral P pool in watershed soil
!!    basno3f     |kg N/ha       |final average amount of nitrogen in the
!!                               |nitrate pool in watershed soil
!!    basno3i     |kg N/ha       |initial average amount of nitrogen in the
!!                               |nitrate pool in watershed soil
!!    basorgnf    |kg N/ha       |final average amount of nitrogen in the
!!                               |organic N pool in watershed soil
!!    basorgni    |kg N/ha       |initial average amount of nitrogen in the
!!                               |organic N pool in watershed soil
!!    basorgpf    |kg P/ha       |final average amount of phosphorus in
!!                               |the organic P pool in watershed soil
!!    basorgpi    |kg P/ha       |initial average amount of phosphorus in
!!                               |the organic P pool in watershed soil
!!    bio_aahv(:,:,:)|kg/ha         |harvested biomass of plant
!!    bio_aams(:) |metric tons/ha|average annual biomass (dry weight) in HRU
!!    cn2(:)      |none          |SCS runoff curve number for moisture
!!                               |condition II
!!    cpnm(:)     |NA            |four character code to represent crop name
!!    hru_km(:)   |km^2          |area of HRU in square kilometers
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    hruaao(1,:) |mm H2O        |precipitation in HRU during simulation
!!    hruaao(4,:) |mm H2O        |amount of surface runoff to main channel
!!                               |from HRU during simulation (ignores impact of
!!                               |transmission losses)
!!    hruaao(5,:) |mm H2O        |amount of lateral flow contribution to main
!!                               |channel from HRU during simulation
!!    hruaao(6,:) |mm H2O        |amount of groundwater flow contribution to
!!                               |main channel from HRU during simulation
!!    hruaao(12,:)|mm H2O        |actual evapotranspiration in HRU during
!!                               |simulation
!!    hruaao(14,:)|metric tons/ha|sediment yield from HRU for simulation
!!    hruaao(22,:)|mm H2O        |amount of irrigation water applied to HRU
!!                               |during simulation
!!    hruaao(28,:)|kg N/ha       |average annual amount of N (organic &
!!                               |mineral) auto-applied in HRU
!!    hruaao(29,:)|kg P/ha       |average annual amount of P (organic &
!!                               |mineral) auto-applied in HRU
!!    hruaao(35,:)|kg N/ha       |organic nitrogen in surface runoff in HRU
!!                               |during simulation
!!    hruaao(37,:)|kg N/ha       |nitrate in surface runoff in HRU during
!!                               |simulation
!!    hruaao(38,:)|kg N/ha       |nitrate in lateral flow in HRU during
!!                               |simulation
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    idplt(:,:,:)|none          |land cover code from crop.dat
!!    ipot(:)     |none          |number of HRU (in subbasin) that is ponding
!!                               |water--the HRU that the surface runoff from
!!                               |current HRU drains into. This variable is
!!                               |used only for rice paddys or closed
!!                               |depressional areas
!!    irn(:)      |none          |average annual number of irrigation
!!                               |applications in HRU
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    mcr         |none          |max number of crops grown per year
!!    nhru        |none          |number of HRUs in watershed
!!    nro(:)      |none          |sequence number of year in rotation
!!    nrot(:)     |none          |number of years of rotation
!!    prog        |NA            |program name and version
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
!!    sbactlchlp  |# colonies/ha |average annual number of less persistent
!!                               |bacteria lost from soil surface layer by
!!                               |percolation
!!    sbactlchp   |# colonies/ha |average annual number of persistent bacteria
!!                               |lost from soil surface layer by percolation
!!    sbactrolp   |# colonies/ha |average annual number of less persistent
!!                               |bacteria transported to main channel
!!                               |with surface runoff in solution
!!    sbactrop    |# colonies/ha |average annual number of persistent bacteria
!!                               |transported to main channel with surface
!!                               |runoff in solution
!!    sbactsedlp  |# colonies/ha |average annual number of less persistent
!!                               |bacteria transported with sediment in
!!                               |surface runoff
!!    sdiegrolpq  |# colonies/ha |average annual change in the number of
!!                               |less persistent bacteria colonies in soil
!!                               |solution in watershed
!!    sdiegrolps  |# colonies/ha |average annual change in the number of
!!                               |less persistent bacteria colonies on soil
!!                               |particles in watershed
!!    sdiegropq   |# colonies/ha |average annual change in the number of
!!                               |persistent bacteria colonies in soil solution
!!                               |in watershed
!!    sdiegrops   |# colonies/ha |average annual change in the number of
!!                               |persistent bacteria colonies on soil particles
!!                               |in watershed
!!    snam(:)     |NA            |soil series name
!!    sno3up      |kg N/ha       |amount of nitrate moving upward in the soil
!!                               |profile in watershed
!!    sol_sumfc(:)|mm H2O        |amount of water held in the soil profile
!!                               |at field capacity
!!    spadyev     |mm H2O        |average annual amount of water removed
!!                               |from potholes by evaporation in watershed
!!    spadyo      |mm H2O        |average annual amount of water released to
!!                               |main channel from potholes in watershed
!!    spadyrfv    |mm H2O        |average annual amount of precipitation on
!!                               |potholes in watershed
!!    spadysp     |mm H2O        |average annual amount of water removed
!!                               |from potholes by seepage in watershed
!!    sumix(:)    |none          |sum of mixing efficiencies in HRU
!!    title       |NA            |title from file.cio
!!    usle_ls(:)  |none          |USLE equation length slope (LS) factor
!!    wshd_aamon(:,1)|mm H2O        |average annual precipitation in watershed
!!                               |falling during month
!!    wshd_aamon(:,2)|mm H2O        |average annual freezing rain in watershed
!!                               |falling during month
!!    wshd_aamon(:,3)|mm H2O        |average annual surface runoff in watershed
!!                               |during month
!!    wshd_aamon(:,4)|mm H2O        |average annual lateral flow in watershed
!!                               |during month
!!    wshd_aamon(:,5)|mm H2O        |average annual water yield in watershed
!!                               |during month
!!    wshd_aamon(:,6)|mm H2O        |average annual actual evapotranspiration
!!                               |in watershed during month
!!    wshd_aamon(:,7)|metric tons   |average annual sediment yield in watershed
!!                               |during month
!!    wshd_aamon(:,8)|mm H2O        |average annual potential evapotranspiration
!!                               |in watershed during month
!!    wshd_dnit   |kg N/ha       |average annual amount of nitrogen lost from
!!                               |nitrate pool due to denitrification in
!!                               |watershed
!!    wshd_fixn   |kg N/ha       |average annual amount of nitrogen added to
!!                               |plant biomass via fixation
!!    wshd_fminp  |kg P/ha       |average annual amount of mineral P applied
!!                               |in watershed
!!    wshd_fnh3   |kg N/ha       |average annual amount of NH3-N applied in
!!                               |watershed
!!    wshd_fno3   |kg N/ha       |average annual amount of NO3-N applied in
!!                               |watershed
!!    wshd_forgn  |kg N/ha       |average annual amount of organic N applied
!!                               |in watershed
!!    wshd_forgp  |kg P/ha       |average annual amount of organic P applied
!!                               |in watershed
!!    wshd_ftotn  |kg N/ha       |average annual amount of N (mineral &
!!                               |organic) applied in watershed
!!    wshd_ftotp  |kg P/ha       |average annual amount of P (mineral &
!!                               |organic) applied in watershed
!!    wshd_hmn    |kg N/ha       |average annual amount of nitrogen moving
!!                               |from active organic to nitrate pool in
!!                               |watershed
!!    wshd_hmp    |kg P/ha       |average annual amount of phosphorus moving
!!                               |from organic to labile pool in watershed
!!    wshd_nitn   |kg N/ha       |average annual amount of nitrogen moving
!!                               |from the NH3 to the NO3 pool by
!!                               |nitrification in the watershed
!!    wshd_nstrs  |stress units  |average annual number of nitrogen stress
!!                               |units in watershed
!!    wshd_pal    |kg P/ha       |average annual amount of phosphorus moving
!!                               |from labile mineral to active mineral pool
!!                               |in watershed
!!    wshd_pas    |kg P/ha       |average annual amount of phosphorus moving
!!                               |from active mineral to stable mineral pool
!!                               |in watershed
!!    wshd_plch   |kg P/ha       |average annual amount of phosphorus leached
!!                               |into second soil layer
!!    wshd_pstrs  |stress units  |average annual number of phosphorus stress
!!                               |units in watershed
!!    wshd_pup    |kg P/ha       |average annual amount of plant uptake of
!!                               |phosphorus
!!    wshd_raino3 |kg N/ha       |average annual amount of NO3 added to soil
!!                               |by rainfall in watershed
!!    wshd_rmn    |kg N/ha       |average annual amount of nitrogen moving
!!                               |from fresh organic (residue) to nitrate
!!                               |and active organic pools in watershed
!!    wshd_rmp    |kg P/ha       |average annual amount of phosphorus moving
!!                               |from fresh organic (residue) to labile
!!                               |and organic pools in watershed
!!    wshd_rwn    |kg N/ha       |average annual amount of nitrogen moving
!!                               |from active organic to stable organic pool
!!                               |in watershed
!!    wshd_tstrs  |stress units  |average annual number of temperature stress
!!                               |units in watershed
!!    wshd_voln   |kg N/ha       |average annual amount if nitrogen lost by
!!                               |ammonia volatilization in watershed
!!    wshd_wstrs  |stress units  |average annual number of water stress units
!!                               |in watershed
!!    wshd_yldn   |kg N/ha       |amount of nitrogen removed from soil in
!!                               |watershed in the yield
!!    wshd_yldp   |kg P/ha       |amount of phosphorus removed from soil in
!!                               |watershed in the yield
!!    wshdaao(1)  |mm H2O        |average amount of precipitation in watershed
!!                               |for the simulation
!!    wshdaao(3)  |mm H2O        |surface runoff in watershed for simulation
!!    wshdaao(4)  |mm H2O        |lateral flow contribution to streamflow in
!!                               |watershed for simulation
!!    wshdaao(5)  |mm H2O        |water percolation past bottom of soil profile
!!                               |in watershed for simulation
!!    wshdaao(6)  |mm H2O        |water yield to streamflow from HRUs in
!!                               |watershed for simulation
!!    wshdaao(7)  |mm H2O        |actual evapotranspiration in watershed
!!                               |for simulation
!!    wshdaao(11) |metric tons/ha|net change in sediment of reservoirs in
!!                               |watershed during simulation
!!    wshdaao(12) |metric tons/ha|sediment yield from HRUs in watershed for
!!                               |the simulation
!!    wshdaao(13) |metric tons/ha|sediment loading to ponds in watershed 
!!                               |during simulation
!!    wshdaao(14) |metric tons/ha|sediment loading from ponds in watershed
!!                               |during simulation
!!    wshdaao(15) |metric tons/ha|net change in sediment level in ponds in
!!                               |watershed during simulation
!!    wshdaao(19) |mm H2O        |evaporation from ponds in watershed during
!!                               |simulation
!!    wshdaao(20) |mm H2O        |seepage from ponds in watershed during
!!                               |simulation
!!    wshdaao(21) |mm H2O        |precipitation on ponds in watershed during
!!                               |simulation
!!    wshdaao(22) |mm H2O        |volume of water entering ponds in watershed
!!                               |during simulation
!!    wshdaao(23) |mm H2O        |volume of water leaving ponds in watershed
!!                               |during simulation
!!    wshdaao(33) |mm H2O        |net change in water volume of ponds in
!!                               |watershed during simulation
!!    wshdaao(34) |mm H2O        |net change in water volume of reservoirs in
!!                               |watershed during simulation
!!    wshdaao(36) |mm H2O        |snow melt in watershed for simulation
!!    wshdaao(38) |mm H2O        |average amount of tributary channel
!!                               |transmission losses in watershed during
!!                               |simulation
!!    wshdaao(39) |mm H2O        |freezing rain/snow fall in watershed for 
!!                               |the simulation
!!    wshdaao(40) |kg N/ha       |organic N loading to stream in watershed for
!!                               |the simulation
!!    wshdaao(41) |kg P/ha       |organic P loading to stream in watershed for
!!                               |the simulation
!!    wshdaao(42) |kg N/ha       |nitrate loading to stream in surface runoff
!!                               |in watershed for the simulation
!!    wshdaao(43) |kg P/ha       |soluble P loading to stream in watershed for
!!                               |the simulation
!!    wshdaao(44) |kg N/ha       |plant uptake of N in watershed for the 
!!                               |simulation
!!    wshdaao(45) |kg N/ha       |nitrate loading to stream in lateral flow
!!                               |in watershed for the simulation
!!    wshdaao(46) |kg N/ha       |nitrate percolation past bottom of soil
!!                               |profile in watershed for the simulation
!!    wshdaao(104)|mm H2O        |groundwater contribution to stream in
!!                               |watershed for the simulation (shallow aquifer)
!!    wshdaao(105)|mm H2O        |amount of water moving from shallow aquifer
!!                               |to plants/soil profile in watershed during
!!                               |simulation
!!    wshdaao(106)|mm H2O        |deep aquifer recharge in watershed during
!!                               |simulation
!!    wshdaao(107)|mm H2O        |total amount of water entering both aquifers
!!                               |in watershed during simulation
!!    wshdaao(108)|mm H2O        |potential evapotranspiration in watershed
!!                               |for the simulation
!!    wshdaao(109)|mm H2O        |drainage tile flow contribution to stream
!!                               |in watershed for the simulation
!!    wshdaao(113)|mm H2O        |groundwater contribution to stream in
!!                               |watershed for the simulation (deep aquifer)
!!    yldaa(:)    |metric tons/ha|average annual yield (dry weight) in HRU
!!    yldn(:,:,:) |kg/ha         |average value for yield of crop
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |counter
!!    k           |none          |counter
!!    nicr        |none          |sequence number for crop in year
!!    nnro        |none          |sequence number for year in rotation
!!    sumpady     |none          |number of HRUs with potholes
!!    xirr        |mm H2O        |average annual amount of irrigation water
!!                               |applied to watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
 
      real :: xirr
      integer :: j, nnro, nicr, k, sumpady, ncrp
      character*4 cropname

      if (iscen == 1) then
      write (26,1000) prog
      write (26,1100) title
      write (26,1200)
      else if (isproj == 1) then
      write (19,1000) prog
      write (19,1100) title
      write (19,1200)
      endif
        xirr = 0.
        xirr = Sum(aairr)
        !! print irrigation data
        if (xirr > 0.) then
          if (iscen == 1) then
          write (26,1300)
          else if (isproj == 1) then
          write (19,1300)
          endif
          do j = 1, nhru
            if (iscen == 1) then
            if (aairr(j) > 1.e-4) write (26,1400) j, irn(j), aairr(j)
            else if (isproj == 1) then
            if (aairr(j) > 1.e-4) write (19,1400) j, irn(j), aairr(j)
            endif
          end do
        end if
     
!! write average crop information to std output file
!      ncrp = 0
!      if (mcr < 3) then
      ncrp = mcr
!      else
!        ncrp = 3
!      end if
      if (iscen == 1) then
      write (26,1000) prog
      write (26,1100) title
      write (26,1500)
      else if (isproj == 1) then
      write (19,1000) prog
      write (19,1100) title
      write (19,1500)
      endif
      do j = 1, nhru
        if (iscen == 1) then
          if (mcrhru(j) > 0) then
          write (26,1600) j, hru_sub(j), (cpnm(idplrot(nicr,j)),        
     &        yldn(nicr,j), bio_aahv(nicr,j),nicr = 1, mcrhru(j))
          else
          write (26,1601) j, ' BARE'
          end if
          else if (isproj == 1) then
          if (mcrhru(j) > 0) then
          write (19,1602) j, (cpnm(idplrot(nicr,j)),                    
     &        yldn(nicr,j), bio_aahv(nicr,j),nicr = 1, mcrhru(j))
          else
          write (26,1601) j, ' BARE'
          end if
        endif
      end do

!! write average annual HRU data

      if (iscen == 1) then
      write (26,1700)
      write (26,1800)
      do j = 1, nhru
      if (idplt(j) > 0) then
        cropname = cpnm(idplt(j))
      else
        cropname = 'BARE'
      end if
        write (26,1900) j, hru_sub(j),                                  
     &     snam(j), hru_km(j), cn2(j), sol_sumfc(j), usle_ls(j),        
     &     hruaao(22,j), hruaao(28,j), hruaao(29,j), sumix(j),          
     &     hruaao(1,j), hruaao(19,j), hruaao(5,j) + hruaao(6,j),        
     &     hruaao(12,j), hruaao(14,j), hruaao(37,j) + hruaao(38,j),     
     &     hruaao(35,j), bio_aams(j), yldaa(j), hruaao(4,j)
      end do
      else if (isproj == 1) then
      write (19,1700)
      write (19,1800)
      do j = 1, nhru
      if (idplt(j) > 0) then
        cropname = cpnm(idplt(j))
      else
        cropname = 'BARE'
      end if
        write (19,1900) j, hru_sub(j), cropname,
     &     snam(j), hru_km(j), cn2(j), sol_sumfc(j), usle_ls(j),        
     &     hruaao(22,j), hruaao(28,j), hruaao(29,j), sumix(j),          
     &     hruaao(1,j), hruaao(19,j), hruaao(5,j) + hruaao(6,j),        
     &     hruaao(12,j), hruaao(14,j), hruaao(37,j) + hruaao(38,j),     
     &     hruaao(35,j), bio_aams(j), yldaa(j), hruaao(4,j)
      end do
      endif

!! write average annual watershed monthly values
      if (iscen == 1) then
      write (26,2000)
      do j = 1, 12
        write (26,2100) j, (wshd_aamon(j,k), k = 1, 8)
      end do
      else if (isproj == 1) then
      write (19,2000)
      do j = 1, 12
        write (19,2100) j, (wshd_aamon(j,k), k = 1, 8)
      end do
      endif
	
!! write average annual stress values
      if (iscen == 1) then
      write (26,2200) wshd_wstrs, wshd_tstrs, wshd_nstrs, wshd_pstrs,   
     &      wshd_astrs 
      else if (isproj == 1) then
      write (19,2200) wshd_wstrs, wshd_tstrs, wshd_nstrs, wshd_pstrs,   
     &      wshd_astrs 
      endif

!! watershed summary water balance table
      if (iscen == 1) then
      write (26,1000) prog
      write (26,1100) title
      write (26,2300) wshdaao(1), wshdaao(39), wshdaao(36), wshdaao(37),
     &    wshdaao(3), wshdaao(4),wshdaao(109),wshdaao(104),wshdaao(113),
     &    wshdaao(105), wshdaao(106), wshdaao(107), wshdaao(6),         
     &    wshdaao(5), wshdaao(7), wshdaao(108), wshdaao(38),            
     &    wshd_sepmm,  wshdaao(12)
 !    &    wshd_sepmm,  wshdaao(12), (wshdaao(k),k = 19,22), wshdaao(13),&
 !    &    wshdaao(23), wshdaao(14), (resdata(k),k = 1,7)

 !     write (26,2400) wshdaao(33), wshdaao(15), wshdaao(34), wshdaao(11)
      else if (isproj == 1) then
      write (19,1000) prog
      write (19,1100) title
      write (19,2300) wshdaao(1), wshdaao(39), wshdaao(36), wshdaao(37),
     &    wshdaao(3), wshdaao(4),wshdaao(109),wshdaao(104),wshdaao(113),
     &    wshdaao(105), wshdaao(106), wshdaao(107), wshdaao(6),         
     &    wshdaao(5), wshdaao(7), wshdaao(108), wshdaao(38),            
     &    wshdaao(12)
 !    &    wshdaao(12), (wshdaao(k),k = 19,22), wshdaao(13),             &
 !    &    wshdaao(23), wshdaao(14), (resdata(k),k = 1,7)
 !     write (19,2400) wshdaao(33), wshdaao(15), wshdaao(34), wshdaao(11)
      endif

!! watershed pothole summary values
!      sumpady = 0
!     sumpady = Sum(ipot)
!      if (sumpady > 0) then
        if (iscen == 1) then
        write (26,2500) spadyo, spadyev, spadysp, spadyosp
        else if (isproj == 1) then
        write (19,2500) spadyo, spadyev, spadysp, spadyosp
        endif
!     end if
      
!! watershed summary nutrient table
      if (iscen == 1) then
      write (26,1000) prog
      write (26,1100) title
      write (26,2600)
      write (26,2700)wshdaao(40), wshdaao(41), wshdaao(42), wshdaao(45),
     &    wshdaao(111), wshd_ptile, wshd_pinlet,
     &    wshdaao(43), wshdaao(46), wshd_plch, wshdaao(44), wshd_pup,   
     &    wshdaao(110)
      write (26,2800) wshd_pal, wshd_pas, wshd_ftotn, wshd_ftotp,       
     &    wshd_fixn, wshd_dnit
      write (26,2900) wshd_hmn, wshd_rwn, wshd_hmp, wshd_rmn, wshd_rmp
      write (26,3000) wshd_raino3, basno3i, basno3f, basorgni, basorgnf,
     &    basminpi, basminpf, basorgpi, basorgpf, wshd_fno3,            
     &    wshd_fnh3, wshd_forgn, wshd_fminp, wshd_forgp, wshd_yldn,     
     &    wshd_yldp, wshd_voln, wshd_nitn, sno3up
      else if (isproj == 1) then
      write (19,1000) prog
      write (19,1100) title
      write (19,2600)
      write (19,2700) wshdaao(40), wshdaao(41), wshdaao(42),            
     &    wshdaao(45), wshdaao(43), solpyldjga, tile_solpo(j),
     &    wshdaao(46), wshd_plch, wshdaao(44),
     &    wshd_pup, wshdaao(110)
      write (19,2800) wshd_pal, wshd_pas, wshd_ftotn, wshd_ftotp,       
     &    wshd_fixn, wshd_dnit
      write (19,2900) wshd_hmn, wshd_rwn, wshd_hmp, wshd_rmn, wshd_rmp
      write (19,3000) wshd_raino3, basno3i, basno3f, basorgni, basorgnf,
     &    basminpi, basminpf, basorgpi, basorgpf, wshd_fno3,            
     &    wshd_fnh3, wshd_forgn, wshd_fminp, wshd_forgp, wshd_yldn,     
     &    wshd_yldp, wshd_voln, wshd_nitn, sno3up
      endif

!! watershed bacteria summary table
      if (iscen == 1) then
      write (26,3100) sdiegropq,sdiegrolpq,sdiegrops,sdiegrolps,        
     &     sbactrop,sbactrolp,sbactsedp,sbactsedlp,sbactlchp,sbactlchlp
      else if (isproj == 1) then
      write (19,3100) sdiegropq,sdiegrolpq,sdiegrops,sdiegrolps,        
     &     sbactrop,sbactrolp,sbactsedp,sbactsedlp,sbactlchp,sbactlchlp
      endif

!! septic variables to output.std
      write (26,3101) wshd_sepno3, wshd_sepnh3, wshd_seporgn,
     * wshd_sepfon, wshd_seporgp, wshd_sepfop, wshd_sepsolp,
     * wshd_sepbod

3101  format (t20,'NITRATE SEPTIC = ',f12.2,' (kg/ha)',/,t20,
     *'AMMONIA SEPTIC = ',f12.2,' (kg/ha)',/,t20,
     *'ORG N SEPTIC = ',f12.2,' (kg/ha)',/,t20,
     *'FRESH ORGN SEPTIC = ',f12.2,' (kg/ha)',/,t20,
     *'ORG P SEPTIC = ',f12.2,' (kg/ha)',/,t20,
     *'FRESH ORGP SEPTIC = ',f12.2,' (kg/ha)',/,t20,
     *'SOL P SEPTIC = 'f12.2,' (kg/ha)',/,t20,
     *'BOD SEPTIC = ',f12.2,' (kg/ha)')              

      return

 1000 format ('1',/t5,a80,t105,2(a2,'/'),a2,5x,2(i2,':'),i2)
 1100 format (/(t5,20a4))
 1200 format (t5,'FINAL VALUES'/)
 1300 format (//t21,'IRRIGATION - AVE. ANNUAL',/,t14,'HRU',t27,         
     &    'NO.OF ',t44,'VOLUME',/,t14,'NO.',t24,'APPLICATIONS',t42,     
     &    'APPLIED(MM)',/)
 1400 format (11x,i6,9x,i4,13x,f8.3)
 1500 format (44x,'Average Plant Values (kg/ha)',/)
!! 1600 format (1x,'HRU ',i6,1x,6(a4,'  Yld =',f8.1,1x,'BIOM = ',f8.1,2x))
 1600 format (1x,' HRU ',i7,' SUB',i4,1x,6(a4,'  Yld =',f8.1,1x,
     * 'BIOM = ',f8.1,2x))
 1601 format (1x,' HRU ',i7,a)
 1602 format (1x,'HRU ',i6,1x,6(a4,2f8.1,2x))
 1700 format (/t5,'HRU STATISTICS'//t17,'AVE ANNUAL VALUES'/)
 1800 format (3x,'HRU',t8,' SUB',t14,'SOIL',t25,'AREAkm2',              
     & t36,'CN',                                                        
     & t43,'AWCmm',t51,'USLE_LS',t60,'IRRmm',t67,'AUTONkh ',t75,        
     & 'AUTOPkh ',t84,'MIXEF',t90,'PRECmm',t97,'SURQGENmm',t109,        
     & 'GWQmm',t118,'ETmm',t125,'SEDth ',t132,'NO3kgh ',t140,           
     & 'ORGNkgh ',t148,'BIOMth',t157,'YLDth',t164,'SURQmm')
 1900 format (i7,i4,3x,a8,3x,e8.3,17f8.2) 
 2000 format (///,t17,'AVE MONTHLY BASIN VALUES',/t20,'SNOW',t46,       
     &   'WATER',t66,'SED',/t3,'MON',t11,'RAIN',t20,'FALL',t27,'SURF Q',
     &    t37,'LAT Q',t46,'YIELD',t58,'ET',t64,'YIELD',t75,'PET',/t11,  
     &    '(MM)',t20,'(MM)',t29,'(MM)',t38,'(MM)',t47,'(MM)',t56,'(MM)',
     &    t63,'(T/HA)',t74,'(MM)')
 2100 format (i5,14f9.2)
 2200 format (/,t5,' AVE ANNUAL BASIN STRESS DAYS',/,t15,               
     &    ' WATER STRESS DAYS = ',f8.2,/,t15,                           
     &    ' TEMPERATURE STRESS DAYS = ',f8.2,/,t15,                     
     &    ' NITROGEN STRESS DAYS = ',f8.2,/,t15,                        
     &    ' PHOSPHORUS STRESS DAYS = ',f8.2,/,t15,                      
     &    ' AERATION STRESS DAYS = ',f8.2)
 2300 format (t10,'AVE ANNUAL BASIN VALUES'//                           
     &        t15,'PRECIP = ',f8.1,' MM'/                               
     &        t15,'SNOW FALL =',f8.2,' MM'/                             
     &        t15,'SNOW MELT = ',f8.2,' MM'/                            
     &        t15,'SUBLIMATION = ',f8.2,' MM'/                          
     &        t15,'SURFACE RUNOFF Q = ',f8.2,' MM'/                     
     &        t15,'LATERAL SOIL Q =',f8.2,' MM'/                        
     &        t15,'TILE Q = ',f8.2,' MM'/                               
     &        t15,'GROUNDWATER (SHAL AQ) Q = ',f8.2,' MM'/              
     &        t15,'GROUNDWATER (DEEP AQ) Q = ',f8.2,' MM'/              
     &        t15,'REVAP (SHAL AQ => SOIL/PLANTS) =',f8.2,' MM'/        
     &        t15,'DEEP AQ RECHARGE = ',f8.2,' MM'/                     
     &        t15,'TOTAL AQ RECHARGE =',f8.2,' MM'/                     
     &        t15,'TOTAL WATER YLD = ',f8.2,' MM'/                      
     &        t15,'PERCOLATION OUT OF SOIL =',f8.2,' MM'/               
     &        t15,'ET = ',f8.1,' MM'/                                   
     &        t15,'PET = ',f8.1,'MM'/                                   
     &        t15,'TRANSMISSION LOSSES = ',f8.2,' MM'/                
     &        t15,'SEPTIC INFLOW = ',f10.2,' MM'/                       
     &        t15,'TOTAL SEDIMENT LOADING =  ',f8.2,' T/HA')
 !    &        t15,'POND BUDGET'/                                        
 !    &        t20,'EVAPORATION = ',f8.3,' MM'/                          
 !    &        t20,'SEEPAGE = ',f8.3,' MM'/                              &
 !    &        t20,'RAINFALL ON POOL =',f8.3,' MM'/                      &
 !    &        t20,'INFLOW'/t25,'WATER = ',f8.3,' MM'/                   &
 !    &        t25,'SEDIMENT = ',f8.3,' T/HA'/                           &
 !    &        t20,'OUTFLOW'/t25,'WATER = ',f8.3,' MM'/                  &
 !    &        t25,'SEDIMENT = ',f8.3,' T/HA'/                           &
 !    &        t15,'RESERVOIR BUDGET'/                                   &
 !    &        t20,'EVAPORATION = ',f8.3,' MM'/                          &
 !    &        t20,'SEEPAGE = ',f8.3,' MM'/                              &
 !    &        t20,'RAINFALL ON RESERVOIR = ',f8.3,' MM'/                &
 !    &        t20,'INFLOW'/t25,'WATER = ',f8.3,' MM'/                   &
 !    &        t25,'SEDIMENT = ',f8.3,' T/HA'/                           &
 !    &        t20,'OUTFLOW'/t25,'WATER = ',f8.3,' MM'/                  &
 !    &        t25,'SEDIMENT = ',f8.3,' T/HA')

! 2400 format (t15,'YIELD LOSS FROM PONDS'/t20,'WATER = ',f7.3,' MM'/t20,&
!     &    'SEDIMENT = ',f7.3,' T/HA'/t15,'YIELD LOSS FROM RESERVOIRS'/  &
!     &    t20,'WATER = ',f8.3,' MM'/t20,'SEDIMENT = ',f7.3,' T/HA')
 2500 format (t15,'TILE FROM IMPOUNDED WATER =  ',f8.3,' (MM)',/,t15,   
     &    'EVAPORATION FROM IMPOUNDED WATER =  ',f8.3,' (MM)',/,t15,    
     &    'SEEPAGE INTO SOIL FROM IMPOUNDED WATER = ',f8.3,' (MM)',/,t15
     &    ,'OVERFLOW FROM IMPOUNDED WATER = ',f8.3,' (MM)')
 2600 format (t15,'AVE ANNUAL BASIN VALUES')
 2700 format (//,t15,'NUTRIENTS',/,t20,'ORGANIC N =  ',f8.3,' (KG/HA)', 
     &    /,t20,'ORGANIC P =  ',f8.3,' (KG/HA)',/,t20,                  
     &    'NO3 YIELD (SQ) =  ',f8.3,' (KG/HA)',/,t20,                   
     &    'NO3 YIELD (LAT) = ',f8.3,' (KG/HA)',                         
     &    /,t20,'NO3 YIELD (TILE) = ', f8.3,' (KG/HA)', 
     &    /,t20,'SOLP YIELD (TILE) = ', f8.3, '(KG/HA)',
     &    /,t20,'SOLP YIELD (SURF INLET RISER) = ', f8.3,
     &    ' (KG/HA)',     
     &    /,t20,'SOL P YIELD = ',f8.3,' (KG/HA)',                       
     &    /,t20,'NO3 LEACHED =  ',f8.3,' (KG/HA)',/,t20,                
     &    'P LEACHED =  ',f8.3,' (KG/HA)',/,t20,                        
     &    'N UPTAKE =  ',f8.3,' (KG/HA)',/,t20,'P UPTAKE = ',f8.3,      
     &    ' (KG/HA)',/,t20,'NO3 YIELD (GWQ) =  ',f8.3,' (KG/HA)')
 2800 format (t20,'ACTIVE TO SOLUTION P FLOW =  ',f12.3,' (KG/HA)',/,t20
     &    ,'ACTIVE TO STABLE P FLOW =  ',f12.3,' (KG/HA)',/,t20,      
     &    'N FERTILIZER APPLIED = ',f9.3,' (KG/HA)',/,t20,              
     &    'P FERTILIZER APPLIED = ',f9.3,' (KG/HA)',/,t20,              
     &    'N FIXATION = ',f9.3,' (KG/HA)',/,t20,'DENITRIFICATION = ',f9 
     &    .3,' (KG/HA)')
 2900 format (t20,'HUMUS MIN ON ACTIVE ORG N =  ',f8.3,' (KG/HA)',/,t20,
     &    'ACTIVE TO STABLE ORG N =  ',f8.3,' (KG/HA)',/,t20,           
     &    'HUMUS MIN ON ACTIVE ORG P = ',f8.3,' (KG/HA)',/,t20,         
     &    'MIN FROM FRESH ORG N = ',f8.3,' (KG/HA)',/,t20,              
     &    'MIN FROM FRESH ORG P = ',f8.3,' (KG/HA)')
 3000 format (t20,'NO3 IN RAINFALL =  ',f8.3,' (KG/HA)',/,t20,          
     &    'INITIAL NO3 IN SOIL =  ',f10.3,' (KG/HA)',/,t20,             
     &    'FINAL NO3 IN SOIL =  ',f12.3,' (KG/HA)',/,t20,               
     &    'INITIAL ORG N IN SOIL =  ',f10.3,' (KG/HA)',/,t20,           
     &    'FINAL ORG N IN SOIL =  ',f12.3,' (KG/HA)',/,t20,             
     &    'INITIAL MIN P IN SOIL =  ',f10.3,' (KG/HA)',/,t20,           
     &    'FINAL MIN P IN SOIL =  ',f12.3,' (KG/HA)',/,t20,             
     &    'INITIAL ORG P IN SOIL =  ',f10.3,' (KG/HA)',/,t20,           
     &    'FINAL ORG P IN SOIL =  ',f10.3,' (KG/HA)',/,t20,             
     &    'NO3 IN FERT =  ',f8.3,' (KG/HA)',/,t20,                      
     &    'AMMONIA IN FERT =  ',f8.3,' (KG/HA)',/,t20,                  
     &    'ORG N IN FERT =  ',f8.3,' (KG/HA)',/,t20,                    
     &    'MINERAL P IN FERT =  ',f8.3,' (KG/HA)',/,t20,                
     &    'ORG P IN FERT =  ',f8.3,' (KG/HA)',/,t20,                    
     &    'N REMOVED IN YIELD =  ',f8.3,' (KG/HA)',/,t20,               
     &    'P REMOVED IN YIELD =  ',f8.3,' (KG/HA)',/,t20,               
     &    'AMMONIA VOLATILIZATION =  ',f8.3,' (KG/HA)',/,t20,           
     &    'AMMONIA NITRIFICATION = ',f10.3,' (KG/HA)',/,t20,            
     &    'NO3 EVAP-LAYER 2 TO 1 = ',f8.3)
 3100 format (/,t20,'DIE-GRO P Q =  ',f12.1,' (No/M2)',/,t20,            
     &    'DIE-GRO LP Q =  ',f12.1,' (No/M2)',/,t20,                     
     &    'DIE-GRO P SED = ',f12.1,' (No/M2)',/,t20,                     
     &    'DIE-GRO LP SED = ',f12.1,' (No/M2)',/,t20,                    
     &    'BACT P RUNOFF = ',f12.1,' (No/M2)',/,t20,                     
     &    'BACT LP RUNOFF = ',f12.1,' (No/M2)',/,t20,                    
     &    'BACT P SEDIMENT = ',f12.1,' (No/M2)',/,t20,                   
     &    'BACT LP SEDIMENT = ',f12.1,' (No/M2)',/,t20,                  
     &    'BACT P INCORP = ',f12.1,' (No/M2)',/,t20,                     
     &    'BACT LP INCORP = ',f12.1,' (No/M2)',/)
      end