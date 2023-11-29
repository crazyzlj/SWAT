      subroutine routres
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs reservoir routing     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury        |mg pst        |loss of pesticide from active sediment layer
!!                               |by burial
!!    curyr       |none          |current year of simulation
!!    da_ha       |ha            |area of watershed in hectares
!!    difus       |mg pst        |diffusion of pesticide from sediment to lake
!!                               |water
!!    iida        |julian date   |day being simulated (current julian date)
!!    ihout       |none          |outflow hydrograph storage location number
!!    inum1       |none          |reservoir number
!!    inum2       |none          |inflow hydrograph storage location number
!!    iprint      |none          |print code:
!!                               |0 monthly
!!                               |1 daily
!!                               |2 annually
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    iyr         |year          |current year of simulation (eg 1980)
!!    iyres(:)    |none          |year of the simulation that the reservoir
!!                               |becomes operational
!!    lkpst_conc(:)|mg/m^3       |pesticide concentration in lake water
!!    lkspst_conc(:)|mg/m^3      |pesticide concentration in lake bed sediment
!!    i_mo        |none          |current month of simulation
!!    mores(:)    |none          |month the reservoir becomes operational
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    nhru        |none          |number of HRUs in watershed
!!    nyskip      |none          |number of years to skip output summarization
!!                               |and printing
!!    reactb      |mg pst        |amount of pesticide in sediment that is lost
!!                               |through reactions
!!    reactw      |mg pst        |amount of pesticide in lake water lost
!!                               |through reactions
!!    res_nh3(:)  |kg N          |amount of ammonia in reservoir
!!    res_no2(:)  |kg N          |amount of nitrite in reservoir
!!    res_no3(:)  |kg N          |amount of nitrate in reservoir
!!    res_orgn(:) |kg N          |amount of organic N in reservoir
!!    res_orgp(:) |kg P          |amount of organic P in reservoir
!!    res_sed(:)  |kg/L (ton/m^3)|amount of sediment in reservoir
!!    res_solp(:) |kg P          |amount of soluble P in reservoir
!!    res_seci(:) |m             |secchi-disk depth
!!    res_sub(:)  |none          |number of subbasin reservoir is in
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    reschlao    |kg chl-a      |amount of chlorophyll-a leaving reservoir
!!                               |on day
!!    resev       |m^3 H2O       |evaporation from reservoir on day
!!    resflwi     |m^3 H2O       |water entering reservoir on day
!!    resflwo     |m^3 H2O       |water leaving reservoir on day
!!    resnh3o     |kg N          |amount of ammonia leaving reservoir on day
!!    resno2o     |kg N          |amount of nitrite leaving reservoir on day
!!    resno3o     |kg N          |amount of nitrate leaving reservoir on day
!!    resorgno    |kg N          |amount of organic N leaving reservoir on day
!!    resorgpo    |kg P          |amount of organic P leaving reservoir on day
!!    respcp      |m^3 H2O       |precipitation on reservoir for day
!!    respesti    |mg pst        |pesticide entering reservoir on day
!!    ressedi     |metric tons   |sediment entering reservoir during time step
!!    ressedo     |metric tons   |sediment leaving reservoir during time step
!!    ressep      |m^3 H2O       |seepage from reservoir on day
!!    ressolpo    |kg P          |amount of soluble P leaving reservoir on day
!!    resuspst    |mg pst        |amount of pesticide moving from sediment to
!!                               |lake water due to resuspension
!!    setlpst     |mg pst        |amount of pesticide moving from water to
!!                               |sediment due to settling
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    solpesto    |mg pst        |soluble pesticide in outflow on day
!!    sorpesto    |mg pst        |sorbed pesticide in outflow on day
!!    sub_fr(:)   |none          |fraction of watershed area in subbasin
!!    volatpst    |mg pst        |amount of pesticide lost from lake water
!!                               |by volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    resoutm(1,:) |m^3/s        |flow into reservoir during month
!!    resoutm(2,:) |m^3/s        |flow out of reservoir during month
!!    resoutm(3,:) |metric tons  |sediment entering reservoir during month
!!    resoutm(4,:) |metric tons  |sediment leaving reservoir during month
!!    resoutm(5,:) |mg/L         |sediment concentration in reservoir during
!!                               |month
!!    resoutm(6,:) |mg pst       |pesticide entering reservoir during month
!!    resoutm(7,:) |mg pst       |pesticide lost from reservoir through
!!                               |reactions during month
!!    resoutm(8,:) |mg pst       |pesticide lost from reservoir through
!!                               |volatilization during month
!!    resoutm(9,:) |mg pst       |pesticide moving from water to sediment  <
!!                               |through settling during month
!!    resoutm(10,:)|mg pst       |pesticide moving from sediment to water <
!!                               |through resuspension during month
!!    resoutm(11,:)|mg pst       |pesticide moving from water to sediment <
!!                               |through diffusion during month
!!    resoutm(12,:)|mg pst       |pesticide lost from reservoir sediment layer <
!!                               |through reactions during month
!!    resoutm(13,:)|mg pst       |pesticide lost from reservoir sediment layer <
!!                               |through burial during month
!!    resoutm(14,:)|mg pst       |pesticide transported out of reservoir during <
!!                               |month
!!    resoutm(15,:)|mg pst/m^3   |pesticide concentration in reservoir water
!!                               |during month
!!    resoutm(16,:)|mg pst/m^3   |pesticide concentration in reservoir sediment <
!!                               |layer during month
!!    resoutm(17,:)|m^3 H2O      |evaporation from reservoir during month
!!    resoutm(18,:)|m^3 H2O      |seepage from reservoir during month
!!    resoutm(19,:)|m^3 H2O      |precipitation on reservoir during month
!!    resoutm(20,:)|m^3 H2O      |water flowing into reservoir during month
!!    resoutm(21,:)|m^3 H2O      |water flowing out of reservoir during month
!!    resoutm(22,:)|kg N         |organic N entering reservoir during month
!!    resoutm(23,:)|kg N         |organic N leaving reservoir during month
!!    resoutm(24,:)|kg P         |organic P entering reservoir during month
!!    resoutm(25,:)|kg P         |organic P leaving reservoir during month
!!    resoutm(26,:)|kg N         |nitrate entering reservoir during month
!!    resoutm(27,:)|kg N         |nitrate leaving reservoir during month
!!    resoutm(28,:)|kg N         |nitrite entering reservoir during month
!!    resoutm(29,:)|kg N         |nitrite leaving reservoir during month
!!    resoutm(30,:)|kg N         |ammonia entering reservoir during month
!!    resoutm(31,:)|kg N         |ammonia leaving reservoir during month
!!    resoutm(32,:)|kg P         |mineral P entering reservoir during month
!!    resoutm(33,:)|kg P         |mineral P leaving reservoir during month
!!    resoutm(34,:)|kg chla      |chlorophyll-a entering reservoir during month
!!    resoutm(35,:)|kg chla      |chlorophyll-a leaving reservoir during month
!!    resoutm(36,:)|mg P/L       |average organic P concentration in reservoir 
!!                               |during month
!!    resoutm(37,:)|mg P/L       |average soluble P concentration in reservoir
!!                               |during month
!!    resoutm(38,:)|mg N/L       |average organic N concentration in reservoir
!!                               |during month
!!    resoutm(39,:)|mg N/L       |average nitrate concentration in reservoir
!!                               |during month
!!    resoutm(40,:)|mg N/L       |average nitrite concentration in reservoir
!!                               |during month
!!    resoutm(41,:)|mg N/L       |average ammonia concentration in reservoir
!!                               |during month
!!    shallst(:)   |mm H2O       |depth of water in shallow aquifer
!!    wshddayo(11) |metric tons  |net change in sediment of reservoirs in
!!                               |watershed for day
!!    wshddayo(34) |m^3 H2O      |net change in water volume of reservoirs in
!!                               |watershed for day
!!    varoute(2,:) |m^3 H2O      |water
!!    varoute(3,:) |metric tons  |sediment or suspended solid load
!!    varoute(4,:) |kg N         |organic nitrogen
!!    varoute(5,:) |kg P         |organic posphorus
!!    varoute(6,:) |kg N         |nitrate
!!    varoute(7,:) |kg P         |soluble phosphorus
!!    varoute(11,:)|mg pst       |pesticide in solution
!!    varoute(12,:)|mg pst       |pesticide sorbed to sediment
!!    varoute(13,:)|kg           |chlorophyll-a
!!    varoute(14,:)|kg N         |ammonia
!!    varoute(15,:)|kg N         |nitrite
!!    varoute(18,:)|# cfu/100ml  |persistent bacteria
!!    varoute(19,:)|# cfu/100ml  |less persistent bacteria
!!    varoute(20,:)|kg           |conservative metal #1
!!    varoute(21,:)|kg           |conservative metal #2
!!    varoute(22,:)|kg           |conservative metal #3
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    jres        |none          |reservoir number
!!    k           |none          |counter
!!    sedcon      |mg/L          |sediment concentration in reservoir water
!!                               |during day
!!    sepmm       |mm H2O        |depth of reservoir seepage over subbasin 
!!                               |area
!!    resnh3c     |mg N/L        |concentration of ammonia in reservoir
!!                               |on day
!!    resno2c     |mg N/L        |concentration of nitrite in reservoir
!!                               |on day
!!    resno3c     |mg N/L        |concentration of nitrate in reservoir
!!                               |on day
!!    resorgnc    |mg N/L        |concentration of organic N in reservoir
!!                               |on day
!!    resorgpc    |mg P/L        |concentration of organic P in reservoir
!!                               |on day
!!    ressolpc    |mg P/L        |concentration of soluble P in reservoir
!!                               |on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: resinit, irr_res, res, resnut, lakeq

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jres, k, ii
      real*8 :: sepmm, resorgpc, ressolpc, sedcon, resorgnc, resno3c
      real*8 :: resno2c, resnh3c,zz

      jres = 0
      jres = inum1
!!    ires_code = 0 do not turn off reservoirs
!!    ires_code = 1 turn off reservoirs for santhi

      ires_code = 0

      !! initialize variables for reservoir daily simulation
      call resinit
      if (ires_code == 1) then 

        varoute(1,ihout) = 0.           !!undefined
        varoute(2,ihout) = varoute(2,inum2)
        varoute(3,ihout) = varoute(3,inum2)
        varoute(4,ihout) = varoute(4,inum2)
        varoute(5,ihout) = varoute(5,inum2)
        varoute(6,ihout) = varoute(6,inum2)
        varoute(7,ihout) = varoute(7,inum2)
        varoute(8,ihout) = 0.           !!undefined
        varoute(9,ihout) = 0.           !!undefined
        varoute(10,ihout) = 0.          !!undefined
        varoute(11,ihout) = varoute(11,inum2)
        varoute(12,ihout) = varoute(12,inum2)
        varoute(13,ihout) = varoute(13,inum2)
        varoute(14,ihout) = varoute(14,inum2)
        varoute(15,ihout) = varoute(15,inum2)
        varoute(16,ihout) = 0.          !!CBOD
        varoute(17,ihout) = 0.          !!dis O2
        varoute(18,ihout) = varoute(18,inum2)  !!persistent bact
        varoute(19,ihout) = varoute(19,inum2)  !!less persistent bact
        varoute(20,ihout) = varoute(20,inum2)  !!conservative metal #1
        varoute(21,ihout) = varoute(21,inum2)  !!conservative metal #2
        varoute(22,ihout) = varoute(22,inum2)  !!conservative metal #3
      else


      if (iyr > iyres(jres) .or.                                        
     &              (i_mo >= mores(jres) .and. iyr == iyres(jres))) then

        !! Adjust Reservoir Storage for Irrigation Diversions
        call irr_res
      zz=varoute(19,inum2)
        !! perform reservoir water/sediment balance
        if(ievent == 0) then		!! urban modeling by J.Jeong
	    call res
	  else
	    call reshr
	  endif
  !!      call res

        !! perform reservoir nutrient balance
        call resnut

        !! perform reservoir pesticide transformations
        call lakeq

        !! add reservoir seepage to shallow aquifer convert from m^3 to mm 
        if (ressep > 0.) then
          sepmm = 0.
          sepmm = ressep / (da_ha * sub_fr(res_sub(jres)) * 10.)
          do k = 1, nhru
            if (hru_sub(k) == res_sub(jres)) then
              shallst(k) = shallst(k) + sepmm
            end if
          end do
        end if

        !! set values for routing variables
        varoute(1,ihout) = 0.           !!undefined
        varoute(2,ihout) = resflwo
        varoute(3,ihout) = ressedo
        varoute(4,ihout) = resorgno
        varoute(5,ihout) = resorgpo
        varoute(6,ihout) = resno3o
        varoute(7,ihout) = ressolpo
        varoute(8,ihout) = 0.           !!undefined
        varoute(9,ihout) = 0.           !!undefined
        varoute(10,ihout) = 0.          !!undefined
        varoute(11,ihout) = solpesto
        varoute(12,ihout) = sorpesto
        varoute(13,ihout) = reschlao
        varoute(14,ihout) = resnh3o
        varoute(15,ihout) = resno2o
        varoute(16,ihout) = 0.          !!CBOD
        varoute(17,ihout) = 0.          !!dis O2
        varoute(18,ihout) = varoute(18,inum2)  !!persistent bact
        varoute(19,ihout) = varoute(19,inum2)  !!less persistent bact
        varoute(20,ihout) = varoute(20,inum2)  !!conservative metal #1
        varoute(21,ihout) = varoute(21,inum2)  !!conservative metal #2
        varoute(22,ihout) = varoute(22,inum2)  !!conservative metal #3

        if (ievent > 0) then
          do ii = 1, nstep
            hhvaroute(1,ihout,ii) = 0.           !!undefined
            hhvaroute(2,ihout,ii) = hhresflwo(ii)
            hhvaroute(3,ihout,ii) = hhressedo(ii)
            hhvaroute(4,ihout,ii) = resorgno / dfloat(nstep)
            hhvaroute(5,ihout,ii) = resorgpo / dfloat(nstep)
            hhvaroute(6,ihout,ii) = resno3o / dfloat(nstep)
            hhvaroute(7,ihout,ii) = ressolpo / dfloat(nstep)
            hhvaroute(8,ihout,ii) = 0.           !!undefined
            hhvaroute(9,ihout,ii) = 0.           !!undefined
            hhvaroute(10,ihout,ii) = 0.          !!undefined
            hhvaroute(11,ihout,ii) = solpesto / dfloat(nstep)
            hhvaroute(12,ihout,ii) = sorpesto / dfloat(nstep)
            hhvaroute(13,ihout,ii) = reschlao / dfloat(nstep)
            hhvaroute(14,ihout,ii) = resnh3o / dfloat(nstep)
            hhvaroute(15,ihout,ii) = resno2o / dfloat(nstep)
            hhvaroute(16,ihout,ii) = 0.          !!CBOD
            hhvaroute(17,ihout,ii) = 0.          !!dis O2
            hhvaroute(18,ihout,ii) = hhvaroute(18,inum2,ii) !!persistent bact
            hhvaroute(19,ihout,ii) = hhvaroute(19,inum2,ii)  !!less persist bact
            hhvaroute(20,ihout,ii) = varoute(20,inum2) / dfloat(nstep) !!cons metal #1
            hhvaroute(21,ihout,ii) = varoute(21,inum2) / dfloat(nstep) !!cons metal #2
            hhvaroute(22,ihout,ii) = varoute(22,inum2) / dfloat(nstep) !!cons metal #3

            hhvaroute(23,ihout,ii) = varoute(23,inum2) / dfloat(nstep) !!Sand out
            hhvaroute(24,ihout,ii) = varoute(24,inum2) / dfloat(nstep) !!Silt out
            hhvaroute(25,ihout,ii) = varoute(25,inum2) / dfloat(nstep) !!clay out
            hhvaroute(26,ihout,ii) = varoute(26,inum2) / dfloat(nstep) !!Small agg out
            hhvaroute(27,ihout,ii) = varoute(27,inum2) / dfloat(nstep) !!Large agg out
            hhvaroute(28,ihout,ii) = varoute(28,inum2) / dfloat(nstep) !!Gravel out

          end do
        end if

        !! summarization calculations
        if (curyr > nyskip) then
          !!calculate concentrations
          resorgnc = 0.
          resnh3c = 0.
          resno3c = 0.
          resno2c = 0.
          resorgpc = 0.
          ressolpc = 0.
          sedcon = 0.
          resorgnc = res_orgn(jres) / (res_vol(jres)+.1) * 1000.
          resno3c = res_no3(jres) / (res_vol(jres)+.1) * 1000.
          resno2c = res_no2(jres) / (res_vol(jres)+.1) * 1000.
          resnh3c = res_nh3(jres) / (res_vol(jres)+.1) * 1000.
          resorgpc = res_orgp(jres) / (res_vol(jres)+.1) * 1000.
          ressolpc = res_solp(jres) / (res_vol(jres)+.1) * 1000.
          sedcon = res_sed(jres) * 1.e6

          resoutm(1,jres) = resoutm(1,jres) + resflwi / 86400.
          resoutm(2,jres) = resoutm(2,jres) + resflwo / 86400.
          resoutm(3,jres) = resoutm(3,jres) + ressedi
          resoutm(4,jres) = resoutm(4,jres) + ressedo
          resoutm(5,jres) = resoutm(5,jres) + sedcon
          resoutm(6,jres) = resoutm(6,jres) + respesti
          resoutm(7,jres) = resoutm(7,jres) + reactw
          resoutm(8,jres) = resoutm(8,jres) + volatpst
          resoutm(9,jres) = resoutm(8,jres) + setlpst
          resoutm(10,jres) = resoutm(10,jres) + resuspst
          resoutm(11,jres) = resoutm(11,jres) - difus
          resoutm(12,jres) = resoutm(12,jres) + reactb
          resoutm(13,jres) = resoutm(13,jres) + bury
          resoutm(14,jres) = resoutm(14,jres) + solpesto + sorpesto
          resoutm(15,jres) = resoutm(15,jres) + lkpst_conc(jres)
          resoutm(16,jres) = resoutm(16,jres) + lkspst_conc(jres)
          resoutm(17,jres) = resoutm(17,jres) + resev
          resoutm(18,jres) = resoutm(18,jres) + ressep
          resoutm(19,jres) = resoutm(19,jres) + respcp
          resoutm(20,jres) = resoutm(20,jres) + resflwi
          resoutm(21,jres) = resoutm(21,jres) + resflwo
          resoutm(22,jres) = resoutm(22,jres) + varoute(4,inum2)
          resoutm(23,jres) = resoutm(23,jres) + resorgno
          resoutm(24,jres) = resoutm(24,jres) + varoute(5,inum2)
          resoutm(25,jres) = resoutm(25,jres) + resorgpo
          resoutm(26,jres) = resoutm(26,jres) + varoute(6,inum2)
          resoutm(27,jres) = resoutm(27,jres) + resno3o
          resoutm(28,jres) = resoutm(28,jres) + varoute(15,inum2)
          resoutm(29,jres) = resoutm(29,jres) + resno2o
          resoutm(30,jres) = resoutm(30,jres) + varoute(14,inum2)
          resoutm(31,jres) = resoutm(31,jres) + resnh3o
          resoutm(32,jres) = resoutm(32,jres) + varoute(7,inum2)
          resoutm(33,jres) = resoutm(33,jres) + ressolpo
          resoutm(34,jres) = resoutm(34,jres) + varoute(13,inum2)
          resoutm(35,jres) = resoutm(35,jres) + reschlao
          resoutm(36,jres) = resoutm(36,jres) + resorgpc
          resoutm(37,jres) = resoutm(37,jres) + ressolpc
          resoutm(38,jres) = resoutm(38,jres) + resorgnc
          resoutm(39,jres) = resoutm(39,jres) + resno3c
          resoutm(40,jres) = resoutm(40,jres) + resno2c
          resoutm(41,jres) = resoutm(41,jres) + resnh3c
          wshddayo(11) = wshddayo(11) + ressedc
          wshddayo(34) = wshddayo(34) + resflwi - resflwo
        end if
      

      if (iprint == 1 .and. curyr > nyskip) then
        if (iscen == 1.and. isproj == 0) then
        write (8,5000) jres, iida, res_vol(jres), resflwi / 86400.,     
     &    (resflwo / 86400.), respcp, resev, ressep, ressedi, ressedo,  
     &    sedcon, varoute(4,inum2), resorgno, resorgnc,                 
     &    varoute(5,inum2), resorgpo, resorgpc, varoute(6,inum2),       
     &    resno3o, resno3c, varoute(15,inum2), resno2o, resno2c,        
     &    varoute(14,inum2), resnh3o, resnh3c, varoute(7,inum2),        
     &    ressolpo, ressolpc, varoute(13,inum2), reschlao,              
     &    res_seci(jres), respesti, reactw, volatpst, setlpst, resuspst,
     &    difus, reactb, bury, solpesto + sorpesto, lkpst_conc(jres),   
     &    lkspst_conc(jres),iyr
        else if (isproj == 1) then
        write (22,5000) jres, iida, res_vol(jres), resflwi / 86400.,    
     &    (resflwo / 86400.), respcp, resev, ressep, ressedi, ressedo,  
     &    sedcon, varoute(4,inum2), resorgno, resorgnc,                 
     &    varoute(5,inum2), resorgpo, resorgpc, varoute(6,inum2),       
     &    resno3o, resno3c, varoute(15,inum2), resno2o, resno2c,        
     &    varoute(14,inum2), resnh3o, resnh3c, varoute(7,inum2),        
     &    ressolpo, ressolpc, varoute(13,inum2), reschlao,              
     &    res_seci(jres), respesti, reactw, volatpst, setlpst, resuspst,
     &    difus, reactb, bury, solpesto + sorpesto, lkpst_conc(jres),   
     &    lkspst_conc(jres),iyr
        else if (iscen == 1 .and. isproj == 2) then
        write (8,6000) jres, iida, res_vol(jres), resflwi / 86400.,     
     &    (resflwo / 86400.), respcp, resev, ressep, ressedi, ressedo,  
     &    sedcon, varoute(4,inum2), resorgno, resorgnc,                 
     &    varoute(5,inum2), resorgpo, resorgpc, varoute(6,inum2),       
     &    resno3o, resno3c, varoute(15,inum2), resno2o, resno2c,        
     &    varoute(14,inum2), resnh3o, resnh3c, varoute(7,inum2),        
     &    ressolpo, ressolpc, varoute(13,inum2), reschlao,              
     &    res_seci(jres), respesti, reactw, volatpst, setlpst, resuspst,
     &    difus, reactb, bury, solpesto + sorpesto, lkpst_conc(jres),   
     &    lkspst_conc(jres), iyr
        endif
      endif
      else
        !! reservoir has not been constructed yet
        do ii = 1, mvaro
          varoute(ii,ihout) = varoute(ii,inum2)
        end do
      end if
      endif

      return
 5000 format ('RES   ',i8,1x,i4,41e12.4,1x,i4)
 6000 format ('RES   ',i8,1x,i4,41e12.4,1x,i4)
      end