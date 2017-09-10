      subroutine recday
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine inputs measured loadings to the stream network for 
!!    routing through the watershed where the records are summarized on a
!!    daily basis

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    id1         |julian date   |first day of simulation in year
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    inum1       |none          |reach number
!!    ifirstr(:)  |none          |measured data search code
!!                               |0 first day of measured data located in file
!!                               |1 first day of measured data not located in
!!                               |file
!!    ihout       |none          |hydrograph storage location number
!!    inum1       |none          |file number
!!    iyr         |year          |current year of simulation (actual year)
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhvaroute(2,:,:) |m^3          |volume of water
!!    hhvaroute(3,:,:) |metric tons  |sediment
!!    hhvaroute(4,:,:) |kg N         |organic N
!!    hhvaroute(5,:,:) |kg P         |organic P
!!    hhvaroute(6,:,:) |kg N         |NO3-N
!!    hhvaroute(7,:,:) |kg P         |mineral (soluble) P
!!    hhvaroute(13,:,:)|kg           |chlorophyll-a
!!    hhvaroute(14,:,:)|kg N         |NH3
!!    hhvaroute(15,:,:)|kg N         |NO2
!!    hhvaroute(16,:,:)|kg           |carbonaceous biological oxygen demand
!!    hhvaroute(17,:,:)|kg           |dissolved oxygen
!!    hhvaroute(18,:,:)|# cfu/100ml  |persistent bacteria
!!    hhvaroute(19,:,:)|# cfu/100ml  |less persistent bacteria
!!    hhvaroute(20,:,:)|kg           |conservative metal #1
!!    hhvaroute(21,:,:)|kg           |conservative metal #2
!!    hhvaroute(22,:,:)|kg           |conservative metal #3
!!    ifirstr(:)       |none         |measured data search code
!!                                   |0 first day of measured data located in 
!!                                   |file
!!                                   |1 first day of measured data not located in
!!                                   |file
!!    varoute(2,:)     |m^3          |volume of water
!!    varoute(3,:)     |metric tons  |sediment
!!    varoute(4,:)     |kg N         |organic N
!!    varoute(5,:)     |kg P         |organic P
!!    varoute(6,:)     |kg N         |NO3-N
!!    varoute(7,:)     |kg P         |mineral (soluble) P
!!    varoute(13,:)    |kg           |chlorophyll-a
!!    varoute(14,:)    |kg N         |NH3
!!    varoute(15,:)    |kg N         |NO2
!!    varoute(16,:)    |kg           |carbonaceous biological oxygen demand
!!    varoute(17,:)    |kg           |dissolved oxygen
!!    varoute(18,:)    |# bact       |persistent bacteria
!!    varoute(19,:)    |# bact       |less persistent bacteria
!!    varoute(20,:)    |kg           |conservative metal #1
!!    varoute(21,:)    |kg           |conservative metal #2
!!    varoute(22,:)    |kg           |conservative metal #3
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpday   |# cfu/100ml   |loading of less persistent bacteria to 
!!                               |reach on day
!!    bactpday    |# cfu/100ml   |loading of persistent bacteria to reach
!!                               |on day
!!    cbodday     |kg            |CBOD loading to reach on day
!!    chladay     |kg            |chlorophyll a loading to reach on day
!!    cmtl1day    |kg            |loading of conservative metal #1 to reach 
!!                               |on day
!!    cmtl2day    |kg            |loading of conservative metal #2 to reach 
!!                               |on day
!!    cmtl3day    |kg            |loading of conservative metal #3 to reach 
!!                               |on day
!!    disoxday    |kg            |dissolved oxygen loading to reach on day
!!    floday      |m^3 H2O       |water loading to reach on day
!!    idap        |julian date   |julian date of record
!!    ii          |none          |counter
!!    iyp         |year          |year of record
!!    j           |none          |counter
!!    minpday     |kg P          |soluble P loading to reach on day
!!    nh3day      |kg N          |ammonia loading to reach on day
!!    no2day      |kg N          |nitrite loading to reach on day
!!    no3day      |kg N          |nitrate loading to reach on day
!!    orgnday     |kg N          |organic N loading to reach on day
!!    orgpday     |kg P          |organic P loading to reach on day
!!    sedday      |metric tons   |sediment loading to reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real :: floday, sedday, orgnday, orgpday, no3day, minpday
      real :: nh3day, no2day, cmtl1day, cmtl2day, cmtl3day, solpstday
      real :: bactpday, bactlpday, chladay, disoxday, cbodday, srbpstday
      integer :: idap, iyp, ii, j

!! initialize variables
      floday = 0.
      sedday = 0.
      orgnday = 0.
      orgpday = 0.
      no3day = 0.
      minpday = 0.
      nh3day = 0.
      no2day = 0.
      cmtl1day = 0.
      cmtl2day = 0.
      cmtl3day = 0.
      bactpday = 0.
      bactlpday = 0.
      chladay = 0.
      disoxday = 0.
      cbodday = 0.
      solpstday = 0.
      srbpstday = 0.
      idap = 0
      iyp = 0
      do j = 1, mvaro
        varoute(j,ihout) = 0.
        do ii = 1, nstep
          hhvaroute(j,ihout,ii) = 0.
        end do
      end do

      if (ifirstr(inum1) == 0) then
          read (555+inum1,*) idap, iyp, floday, sedday, orgnday,        
     &        orgpday, no3day, nh3day, no2day, minpday, cbodday,        
     &        disoxday, chladay, solpstday, srbpstday, bactpday,        
     &        bactlpday, cmtl1day, cmtl2day, cmtl3day
      else
        ifirstr(inum1) = 0
        do
          read (555+inum1,*) idap, iyp, floday, sedday, orgnday,        
     &        orgpday, no3day, nh3day, no2day, minpday, cbodday,        
     &        disoxday, chladay, solpstday, srbpstday, bactpday,        
     &        bactlpday, cmtl1day, cmtl2day, cmtl3day
          if (iyp + idap <= 0) exit
          if (iyp == iyr .and. idap == id1) exit
        end do
      endif

      varoute(2,ihout) = floday
      varoute(3,ihout) = sedday
      varoute(4,ihout) = orgnday
      varoute(5,ihout) = orgpday
      varoute(6,ihout) = no3day
      varoute(7,ihout) = minpday
      varoute(11,ihout) = solpstday
      varoute(12,ihout) = srbpstday
      varoute(13,ihout) = chladay
      varoute(14,ihout) = nh3day
      varoute(15,ihout) = no2day
      varoute(16,ihout) = cbodday
      varoute(17,ihout) = disoxday
      varoute(18,ihout) = bactpday
      varoute(19,ihout) = bactlpday
      varoute(20,ihout) = cmtl1day
      varoute(21,ihout) = cmtl2day
      varoute(22,ihout) = cmtl3day

      !! Assumed equal distribution of sediment
      varoute(23,ihout) = sedday * 0.   ! sand
      varoute(24,ihout) = sedday * 1.   ! silt
      varoute(25,ihout) = sedday * 0.   ! cla
      varoute(26,ihout) = sedday * 0.   ! sag
      varoute(27,ihout) = sedday * 0.   ! lag
      varoute(28,ihout) = 0.            ! gravel

      if (ievent > 0) then
        do ii = 1, nstep
          hhvaroute(2,ihout,ii) = floday / real(nstep)
          hhvaroute(3,ihout,ii) = sedday / real(nstep)
          hhvaroute(4,ihout,ii) = orgnday / real(nstep)
          hhvaroute(5,ihout,ii) = orgpday / real(nstep)
          hhvaroute(6,ihout,ii) = no3day / real(nstep)
          hhvaroute(7,ihout,ii) = minpday / real(nstep)
          hhvaroute(11,ihout,ii) = solpstday / real(nstep)
          hhvaroute(12,ihout,ii) = srbpstday / real(nstep)
          hhvaroute(13,ihout,ii) = chladay / real(nstep)
          hhvaroute(14,ihout,ii) = nh3day / real(nstep)
          hhvaroute(15,ihout,ii) = no2day / real(nstep)
          hhvaroute(16,ihout,ii) = cbodday / real(nstep)
          hhvaroute(17,ihout,ii) = disoxday / real(nstep)
          hhvaroute(18,ihout,ii) = bactpday / real(nstep)
          hhvaroute(19,ihout,ii) = bactlpday / real(nstep)
          hhvaroute(20,ihout,ii) = cmtl1day / real(nstep)
          hhvaroute(21,ihout,ii) = cmtl2day / real(nstep)
          hhvaroute(22,ihout,ii) = cmtl3day / real(nstep)
        end do
      end if

      return
      end