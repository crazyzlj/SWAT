       subroutine reachout
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine summarizes data for reaches

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ammonian(:) |mg N/L        |ammonia concentration in reach
!!    bury        |mg pst        |loss of pesticide from active sediment layer
!!                               |by burial
!!    ch_l2(:)    |km            |length of main channel
!!    ch_w(2,:)   |m             |average width of main channel
!!    chlora(:)   |mg chl-a/L    |chlorophyll-a concentration in reach
!!    difus       |mg pst        |diffusion of pesticide from sediment to reach
!!    disolvp(:)  |mg P/L        |dissolved phosphorus concentration in reach
!!    rch_dox(:)  |mg O2/L       |dissolved oxygen concentration in reach
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during hour
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    inum1       |none          |reach number
!!    nitraten(:) |mg N/L        |nitrate concentration in reach
!!    nitriten(:) |mg N/L        |nitrite concentration in reach
!!    organicn(:) |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:) |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:) |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                               |reach
!!    reactb      |mg pst        |amount of pesticide in sediment that is lost
!!                               |through reactions
!!    reactw      |mg pst        |amount of pesticide in reach that is lost
!!                               |through reactions
!!    resuspst    |mg pst        |amount of pesticide moving from sediment to
!!                               |reach due to resuspension
!!    rnum1       |none          |fraction of inflow that is overland flow
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedpst_act(:)|m            |depth of active sediment layer in reach for
!!                               |pesticide
!!    sedpst_conc(:)|mg/(m**3)   |inital pesticide concentration in river bed
!!                               |sediment
!!    sedrch      |metric tons   |sediment transported out of channel
!!                               |during time step
!!    setlpst     |mg pst        |amount of pesticide moving from water to
!!                               |sediment due to settling
!!    solpesto    |mg pst/m^3    |soluble pesticide concentration in outflow
!!                               |on day
!!    sorpesto    |mg pst/m^3    |sorbed pesticide concentration in outflow
!!                               |on day
!!    volatpst    |mg pst        |amount of pesticide in reach lost by
!!                               |volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rchdy(1,:)   |m^3/s        |flow into reach on day
!!    rchdy(2,:)   |m^3/s        |flow out of reach on day
!!    rchdy(3,:)   |m^3/s        |evaporation from reach on day
!!    rchdy(4,:)   |m^3/s        |transmission losses from reach on day
!!    rchdy(5,:)   |metric tons  |sediment transported into reach on day
!!    rchdy(6,:)   |metric tons  |sediment transported out of reach on day
!!    rchdy(7,:)   |mg/L         |sediment concentration in outflow
!!    rchdy(8,:)   |kg N         |organic N transported into reach on day
!!    rchdy(9,:)   |kg N         |organic N transported out of reach on day
!!    rchdy(10,:)  |kg P         |organic P transported into reach on day
!!    rchdy(11,:)  |kg P         |organic P transported out of reach on day
!!    rchdy(12,:)  |kg N         |nitrate transported into reach on day
!!    rchdy(13,:)  |kg N         |nitrate transported out of reach on day
!!    rchdy(14,:)  |kg N         |ammonia transported into reach on day
!!    rchdy(15,:)  |kg N         |ammonia transported out of reach on day
!!    rchdy(16,:)  |kg N         |nitrite transported into reach on day
!!    rchdy(17,:)  |kg N         |nitrite transported out of reach on day
!!    rchdy(18,:)  |kg P         |soluble P transported into reach on day
!!    rchdy(19,:)  |kg P         |soluble P transported out of reach on day
!!    rchdy(20,:)  |kg chla      |chlorophyll-a transported into reach on day
!!    rchdy(21,:)  |kg chla      |chlorophyll-a transported out of reach on day
!!    rchdy(22,:)  |kg O2        |CBOD transported into reach on day
!!    rchdy(23,:)  |kg O2        |CBOD transported out of reach on day
!!    rchdy(24,:)  |kg O2        |dissolved oxygen transported into reach on day
!!    rchdy(25,:)  |kg O2        |dissolved oxygen transported out of reach on
!!                               |day
!!    rchdy(26,:)  |mg pst       |soluble pesticide transported into reach on
!!                               |day
!!    rchdy(27,:)  |mg pst       |soluble pesticide transported out of reach on
!!                               |day
!!    rchdy(28,:)  |mg pst       |sorbed pesticide transported into reach on day
!!    rchdy(29,:)  |mg pst       |sorbed pesticide transported out of reach on
!!                               |day
!!    rchdy(30,:)  |mg pst       |amount of pesticide lost through reactions in
!!                               |reach on day
!!    rchdy(31,:)  |mg pst       |amount of pesticide lost through 
!!                               |volatilization from reach on day
!!    rchdy(32,:)  |mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment on day
!!    rchdy(33,:)  |mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach on day
!!    rchdy(34,:)  |mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment on day
!!    rchdy(35,:)  |mg pst       |amount of pesticide in sediment layer lost 
!!                               |through reactions on day
!!    rchdy(36,:)  |mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial on day
!!    rchdy(37,:)  |mg pst       |amount of pesticide stored in river bed
!!                               |sediments
!!    rchdy(38,:)  |kg bact      |persistent bacteria transported out of reach
!!                               |on day
!!    rchdy(39,:)  |kg bact      |less persistent bacteria transported out of
!!                               |reach on day
!!    rchdy(40,:)  |kg           |amount of conservative metal #1 transported
!!                               |out of reach on day
!!    rchdy(41,:)  |kg           |amount of conservative metal #2 transported
!!                               |out of reach on day
!!    rchdy(42,:)  |kg           |amount of conservative metal #3 transported
!!                               |out of reach on day
!!    rchmono(1,:) |m^3/s        |flow into reach during month
!!    rchmono(2,:) |m^3/s        |flow out of reach during month
!!    rchmono(3,:) |metric tons  |sediment transported into reach during month
!!    rchmono(4,:) |metric tons  |sediment transported out of reach during month
!!    rchmono(5,:) |mg/L         |sediment concentration in outflow during month
!!    rchmono(6,:) |kg N         |organic N transported into reach during month
!!    rchmono(7,:) |kg N         |organic N transported out of reach during 
!!                               |month
!!    rchmono(8,:) |kg P         |organic P transported into reach during month
!!    rchmono(9,:) |kg P         |organic P transported out of reach during
!!                               |month
!!    rchmono(10,:)|m^3/s        |evaporation from reach during month
!!    rchmono(11,:)|m^3/s        |transmission losses from reach during month
!!    rchmono(12,:)|kg           |conservative metal #1 transported out of reach
!!                               |during month
!!    rchmono(13,:)|kg           |conservative metal #2 transported out of reach
!!                               |during month
!!    rchmono(14,:)|kg           |conservative metal #3 transported out of reach
!!                               |during month
!!    rchmono(15,:)|kg N         |nitrate transported into reach during month
!!    rchmono(16,:)|kg N         |nitrate transported out of reach during month
!!    rchmono(17,:)|kg P         |soluble P transported into reach during month
!!    rchmono(18,:)|kg P         |soluble P transported out of reach during 
!!                               |month
!!    rchmono(19,:)|mg pst       |soluble pesticide transported into reach
!!                               |during month
!!    rchmono(20,:)|mg pst       |soluble pesticide transported out of reach
!!                               |during month
!!    rchmono(21,:)|mg pst       |sorbed pesticide transported into reach during
!!                               |month
!!    rchmono(22,:)|mg pst       |sorbed pesticide transported out of reach
!!                               |during month
!!    rchmono(23,:)|mg pst       |amount of pesticide lost through reactions in
!!                               |reach during month
!!    rchmono(24,:)|mg pst       |amount of pesticide lost through
!!                               |volatilization from reach during month
!!    rchmono(25,:)|mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment during month
!!    rchmono(26,:)|mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach during month
!!    rchmono(27,:)|mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment during month
!!    rchmono(28,:)|mg pst       |amount of pesticide in sediment layer lost
!!                               |through reactions during month
!!    rchmono(29,:)|mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial during month
!!    rchmono(30,:)|kg chla      |chlorophyll-a transported into reach during
!!                               |month
!!    rchmono(31,:)|kg chla      |chlorophyll-a transported out of reach during
!!                               |month
!!    rchmono(32,:)|kg N         |ammonia transported into reach during month
!!    rchmono(33,:)|kg N         |ammonia transported out of reach during month
!!    rchmono(34,:)|kg N         |nitrite transported into reach during month
!!    rchmono(35,:)|kg N         |nitrite transported out of reach during month
!!    rchmono(36,:)|kg O2        |CBOD transported into reach during month
!!    rchmono(37,:)|kg O2        |CBOD transported out of reach during month
!!    rchmono(38,:)|kg O2        |dissolved oxygen transported into reach during
!!                               |month
!!    rchmono(39,:)|kg O2        |dissolved oxygen transported out of reach
!!                               |during month
!!    rchmono(40,:)|kg bact      |persistent bacteria transported out of reach
!!                               |during month
!!    rchmono(41,:)|kg bact      |less persistent bacteria transported out of
!!                               |reach during month
!!    varoute(2,:) |m^3 H2O      |water
!!    varoute(3,:) |metric tons  |sediment or suspended solid load
!!    varoute(4,:) |kg N         |organic nitrogen
!!    varoute(5,:) |kg P         |organic phosphorus
!!    varoute(6,:) |kg N         |nitrate
!!    varoute(7,:) |kg P         |soluble phosphorus
!!    varoute(11,:)|mg pst       |pesticide in solution
!!    varoute(12,:)|mg pst       |pesticide sorbed to sediment
!!    varoute(13,:)|kg           |chlorophyll-a
!!    varoute(14,:)|kg N         |ammonia
!!    varoute(15,:)|kg N         |nitrite
!!    varoute(16,:)|kg           |carbonaceous biological oxygen demand
!!    varoute(17,:)|kg           |dissolved oxygen
!!    varoute(18,:)|# bact/ha    |persistent bacteria
!!    varoute(19,:)|# bact/ha    |less persistent bacteria
!!    varoute(20,:)|kg           |conservative metal #1
!!    varoute(21,:)|kg           |conservative metal #2
!!    varoute(22,:)|kg           |conservative metal #3
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bedvol      |m^3           |volume of river bed sediment
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    sedcon      |mg/L          |sediment concentration in outflow
!!    sedpest     |mg pst        |pesticide in river bed sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch, ii
      real :: sedcon, bedvol, sedpest

      jrch = 0
      jrch = inum1

!! set values for routing variables
      varoute(1,ihout) = 0.
      varoute(2,ihout) = rtwtr
      varoute(3,ihout) = sedrch
      varoute(8,ihout) = 0.
      varoute(9,ihout) = 0.
      varoute(10,ihout) = 0.
      !! varoute(18,ihout) set in rtbact
      !! varoute(19,ihout) set in rtbact
      varoute(20,ihout) = varoute(20,inum2) * (1. - rnum1)
      varoute(21,ihout) = varoute(21,inum2) * (1. - rnum1)
      varoute(22,ihout) = varoute(22,inum2) * (1. - rnum1)
      if (ievent < 3) then
        varoute(4,ihout) = organicn(jrch) * rtwtr / 1000.
        varoute(5,ihout) = organicp(jrch) *  rtwtr / 1000.
        varoute(6,ihout) = nitraten(jrch) * rtwtr / 1000.
        varoute(7,ihout) = disolvp(jrch) * rtwtr / 1000.
        varoute(11,ihout) = solpesto * rtwtr
        varoute(12,ihout) = sorpesto * rtwtr
        varoute(13,ihout) = chlora(jrch) * rtwtr / 1000.
        varoute(14,ihout) = ammonian(jrch) * rtwtr / 1000.
        varoute(15,ihout) = nitriten(jrch) * rtwtr / 1000.
        varoute(16,ihout) = rch_cbod(jrch) *  rtwtr/ 1000.
        varoute(17,ihout) = rch_dox(jrch) *  rtwtr/ 1000.
      else
       do ii = 1, 24
         hhvaroute(1,ihout,ii) = 0.
         hhvaroute(2,ihout,ii) = hrtwtr(ii)
         hhvaroute(3,ihout,ii) = hsedyld(ii)
         hhvaroute(4,ihout,ii) = horgn(ii) * hrtwtr(ii) / 1000.
         hhvaroute(5,ihout,ii) = horgp(ii) *  hrtwtr(ii) / 1000.
         hhvaroute(6,ihout,ii) = hno3(ii) * hrtwtr(ii) / 1000.
         hhvaroute(7,ihout,ii) = hsolp(ii) * hrtwtr(ii) / 1000.
         hhvaroute(8,ihout,ii) = 0.
         hhvaroute(9,ihout,ii) = 0.
         hhvaroute(10,ihout,ii) = 0.
         hhvaroute(11,ihout,ii) = hsolpst(ii) * hrtwtr(ii)
         hhvaroute(12,ihout,ii) = hsorpst(ii) * hrtwtr(ii)
         hhvaroute(13,ihout,ii) = hchla(ii) * hrtwtr(ii) / 1000.
         hhvaroute(14,ihout,ii) = hnh4(ii) * hrtwtr(ii) / 1000.
         hhvaroute(15,ihout,ii) = hno2(ii) * hrtwtr(ii) / 1000.
         hhvaroute(16,ihout,ii) = hbod(ii) *  hrtwtr(ii)/ 1000.
         hhvaroute(17,ihout,ii) = hdisox(ii) *  hrtwtr(ii)/ 1000.
         !! hhvaroute(18,ihout,ii) set in rtbact
         !! hhvaroute(19,ihout,ii) set in rtbact
         hhvaroute(20,ihout,ii) = hhvaroute(20,inum2,ii) * (1. - rnum1)
         hhvaroute(21,ihout,ii) = hhvaroute(21,inum2,ii) * (1. - rnum1)
         hhvaroute(22,ihout,ii) = hhvaroute(22,inum2,ii) * (1. - rnum1)

         varoute(4,ihout) = varoute(4,ihout) + hhvaroute(4,ihout,ii)
         varoute(5,ihout) = varoute(5,ihout) + hhvaroute(5,ihout,ii)
         varoute(6,ihout) = varoute(6,ihout) + hhvaroute(6,ihout,ii)
         varoute(7,ihout) = varoute(7,ihout) + hhvaroute(7,ihout,ii)
         varoute(11,ihout) = varoute(11,ihout) + hhvaroute(11,ihout,ii)
         varoute(12,ihout) = varoute(12,ihout) + hhvaroute(12,ihout,ii)
         varoute(13,ihout) = varoute(13,ihout) + hhvaroute(13,ihout,ii)
         varoute(14,ihout) = varoute(14,ihout) + hhvaroute(14,ihout,ii)
         varoute(15,ihout) = varoute(15,ihout) + hhvaroute(15,ihout,ii)
         varoute(16,ihout) = varoute(16,ihout) + hhvaroute(16,ihout,ii)
         varoute(17,ihout) = varoute(17,ihout) + hhvaroute(17,ihout,ii)
       end do
      end if

!! determine sediment concentration in outflow
      sedcon = 0.
      if (rtwtr > 0.01) then
        sedcon = sedrch / rtwtr * 1.e6
      else
        sedcon = 0.
      end if
      if (sedcon > 200000.) sedcon = 200000.

!! determine amount of pesticide in river bed sediments
      bedvol = 0.
      sedpest = 0.
      bedvol = ch_w(2,jrch) * ch_l2(jrch) * 1000. * sedpst_act(jrch)
      sedpest = sedpst_conc(jrch) * bedvol

!! set daily reach output
      rchdy(1,jrch) = varoute(2,inum2) * (1. - rnum1) / 86400. !!flow in (m^3/s)
      rchdy(2,jrch) = rtwtr / 86400.                      !!flow out (m^3/s)
      rchdy(3,jrch) = rtevp / 86400.                      !!evap (m^3/s)
      rchdy(4,jrch) = rttlc / 86400.                      !!tloss (m^3/s)
      rchdy(5,jrch) = varoute(3,inum2) * (1. - rnum1)     !!sed in (tons)
      rchdy(6,jrch) = sedrch                              !!sed out (tons)
      rchdy(7,jrch) = sedcon                              !!sed conc (mg/L)
      rchdy(8,jrch) = varoute(4,inum2) * (1. - rnum1)     !!orgN in (kg N)
      rchdy(9,jrch) = varoute(4,ihout)                    !!orgN out (kg N)
      rchdy(10,jrch) = varoute(5,inum2) * (1. - rnum1)    !!orgP in (kg P)
      rchdy(11,jrch) = varoute(5,ihout)                   !!orgP out (kg P)
      rchdy(12,jrch) = varoute(6,inum2) * (1. - rnum1)    !!NO3 in (kg N)
      rchdy(13,jrch) = varoute(6,ihout)                   !!NO3 out (kg N)
      rchdy(14,jrch) = varoute(14,inum2) * (1. - rnum1)   !!NH4 in (kg)
      rchdy(15,jrch) = varoute(14,ihout)                  !!NH4 out (kg)
      rchdy(16,jrch) = varoute(15,inum2) * (1. - rnum1)   !!NO2 in (kg)
      rchdy(17,jrch) = varoute(15,ihout)                  !!NO2 out (kg)
      rchdy(18,jrch) = varoute(7,inum2) * (1. - rnum1)    !!solP in (kg P)
      rchdy(19,jrch) = varoute(7,ihout)                   !!solP out (kg P)
      rchdy(20,jrch) = varoute(13,inum2) * (1. - rnum1)   !!chl-a in (kg)
      rchdy(21,jrch) = varoute(13,ihout)                  !!chl-a out (kg)
      rchdy(22,jrch) = varoute(16,inum2) * (1. - rnum1)   !!CBOD in (kg)
      rchdy(23,jrch) = varoute(16,ihout)                  !!CBOD out (kg)
      rchdy(24,jrch) = varoute(17,inum2) * (1. - rnum1)   !!dis O2 in (kg)
      rchdy(25,jrch) = varoute(17,ihout)                  !!dis O2 out (kg)
      rchdy(26,jrch) = varoute(11,inum2) * (1. - rnum1)   !!solpst in (mg pst)
      rchdy(27,jrch) = varoute(11,ihout)                  !!solpst out (mg pst)
      rchdy(28,jrch) = varoute(12,inum2) * (1. - rnum1)   !!srbpst in (mg pst)
      rchdy(29,jrch) = varoute(12,ihout)                  !!srbpst out (mg pst)
      rchdy(30,jrch) = reactw                             !!reacted pst (mg pst)
      rchdy(31,jrch) = volatpst                           !!volatilized pst (mg)
      rchdy(32,jrch) = setlpst                            !!pst settling (mg pst)
      rchdy(33,jrch) = resuspst                           !!pst resuspension (mg)
      rchdy(34,jrch) = -difus                             !!pst diffuse to sed mg
      rchdy(35,jrch) = reactb                             !!react pst/sed (mg)
      rchdy(36,jrch) = bury                               !!pst bury (mg)
      rchdy(37,jrch) = sedpest                            !!pst in rivbed sed mg
      rchdy(38,jrch) = varoute(18,ihout)                  !!persistent bact out
      rchdy(39,jrch) = varoute(19,ihout)                  !!lpersistent bact out
      rchdy(40,jrch) = varoute(20,ihout)                  !!cmetal #1
      rchdy(41,jrch) = varoute(21,ihout)                  !!cmetal #2
      rchdy(42,jrch) = varoute(22,ihout)                  !!cmetal #3

!! summarize monthly reach output
      rchmono(1,jrch) = rchmono(1,jrch) + rchdy(1,jrch)
      rchmono(2,jrch) = rchmono(2,jrch) + rchdy(2,jrch)
      rchmono(3,jrch) = rchmono(3,jrch) + rchdy(5,jrch)
      rchmono(4,jrch) = rchmono(4,jrch) + rchdy(6,jrch)
      rchmono(5,jrch) = rchmono(5,jrch) + rchdy(7,jrch)
      rchmono(6,jrch) = rchmono(6,jrch) + rchdy(8,jrch)
      rchmono(7,jrch) = rchmono(7,jrch) + rchdy(9,jrch)
      rchmono(8,jrch) = rchmono(8,jrch) + rchdy(10,jrch)
      rchmono(9,jrch) = rchmono(9,jrch) + rchdy(11,jrch)
      rchmono(10,jrch) = rchmono(10,jrch) + rchdy(3,jrch)
      rchmono(11,jrch) = rchmono(11,jrch) + rchdy(4,jrch)
      rchmono(12,jrch) = rchmono(12,jrch) + rchdy(40,jrch)
      rchmono(13,jrch) = rchmono(13,jrch) + rchdy(41,jrch)
      rchmono(14,jrch) = rchmono(14,jrch) + rchdy(42,jrch)
      rchmono(15,jrch) = rchmono(15,jrch) + rchdy(12,jrch)
      rchmono(16,jrch) = rchmono(16,jrch) + rchdy(13,jrch)
      rchmono(17,jrch) = rchmono(17,jrch) + rchdy(18,jrch)
      rchmono(18,jrch) = rchmono(18,jrch) + rchdy(19,jrch)
      rchmono(19,jrch) = rchmono(19,jrch) + rchdy(26,jrch)
      rchmono(20,jrch) = rchmono(20,jrch) + rchdy(27,jrch)
      rchmono(21,jrch) = rchmono(21,jrch) + rchdy(28,jrch)
      rchmono(22,jrch) = rchmono(22,jrch) + rchdy(29,jrch)
      rchmono(23,jrch) = rchmono(23,jrch) + rchdy(30,jrch)
      rchmono(24,jrch) = rchmono(24,jrch) + rchdy(31,jrch)
      rchmono(25,jrch) = rchmono(25,jrch) + rchdy(32,jrch)
      rchmono(26,jrch) = rchmono(26,jrch) + rchdy(33,jrch)
      rchmono(27,jrch) = rchmono(27,jrch) + rchdy(34,jrch)
      rchmono(28,jrch) = rchmono(28,jrch) + rchdy(35,jrch)
      rchmono(29,jrch) = rchmono(29,jrch) + rchdy(36,jrch)
      rchmono(30,jrch) = rchmono(30,jrch) + rchdy(20,jrch)
      rchmono(31,jrch) = rchmono(31,jrch) + rchdy(21,jrch)
      rchmono(32,jrch) = rchmono(32,jrch) + rchdy(14,jrch)
      rchmono(33,jrch) = rchmono(33,jrch) + rchdy(15,jrch)
      rchmono(34,jrch) = rchmono(34,jrch) + rchdy(16,jrch)
      rchmono(35,jrch) = rchmono(35,jrch) + rchdy(17,jrch)
      rchmono(36,jrch) = rchmono(36,jrch) + rchdy(22,jrch)
      rchmono(37,jrch) = rchmono(37,jrch) + rchdy(23,jrch)
      rchmono(38,jrch) = rchmono(38,jrch) + rchdy(24,jrch)
      rchmono(39,jrch) = rchmono(39,jrch) + rchdy(25,jrch)
      rchmono(40,jrch) = rchmono(40,jrch) + rchdy(38,jrch)
      rchmono(41,jrch) = rchmono(41,jrch) + rchdy(39,jrch)

      return
      end
