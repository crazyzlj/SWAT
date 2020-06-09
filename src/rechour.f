      subroutine rechour
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine inputs measured loadings to the stream network for 
!!    routing through the watershed where the records are summarized on a
!!    hourly basis

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    id1         |julian date   |first day of simulation in year
!!    inum1       |none          |reach number
!!    ifirsthr(:) |none          |measured data search code
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
!!    ifirsthr(:)      |none         |measured data search code
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
!!    varoute(18,:)    |# cfu/100ml  |persistent bacteria
!!    varoute(19,:)    |# cfu/100ml  |less persistent bacteria
!!    varoute(20,:)    |kg           |conservative metal #1
!!    varoute(21,:)    |kg           |conservative metal #2
!!    varoute(22,:)    |kg           |conservative metal #3
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlphr    |# cfu/100ml   |loading of less persistent bacteria to 
!!                               |reach in hour
!!    bactphr     |# cfu/100ml   |loading of persistent bacteria to reach
!!                               |in hour
!!    cbodhr      |kg            |CBOD loading to reach in hour
!!    chlahr      |kg            |chlorophyll a loading to reach in hour
!!    cmtl1hr     |kg            |loading of conservative metal #1 to reach 
!!                               |in hour
!!    cmtl2hr     |kg            |loading of conservative metal #2 to reach 
!!                               |in hour
!!    cmtl3hr     |kg            |loading of conservative metal #3 to reach 
!!                               |in hour
!!    disoxhr     |kg            |dissolved oxygen loading to reach in hour
!!    flohr       |m^3 H2O       |water loading to reach in hour
!!    idap        |julian date   |julian date of record
!!    ihr         |hour (0-23)   |hour of record
!!    ii          |none          |counter
!!    iyp         |year          |year of record
!!    j           |none          |counter
!!    minphr      |kg P          |soluble P loading to reach in hour
!!    nh3hr       |kg N          |ammonia loading to reach in hour
!!    no2hr       |kg N          |nitrite loading to reach in hour
!!    no3hr       |kg N          |nitrate loading to reach in hour
!!    orgnhr      |kg N          |organic N loading to reach in hour
!!    orgphr      |kg P          |organic P loading to reach in hour
!!    sedhr       |metric tons   |sediment loading to reach in hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real*8 :: flohr, sedhr, orgnhr, orgphr, no3hr, minphr, solpsthr
      real*8 :: nh3hr, no2hr, cmtl1hr, cmtl2hr, cmtl3hr, srbpsthr
      real*8 :: bactphr, bactlphr, chlahr, disoxhr, cbodhr
      integer :: idap, iyp, ii, j, ihr

!! initialize variables
      idap = 0
      iyp = 0
      do j = 1, mvaro
        varoute(j,ihout) = 0.
        do ii = 1, nstep   ! subhourly time step, jaehak jeong
          hhvaroute(j,ihout,ii) = 0.
        end do
      end do

      do ii = 1, nstep   ! subhourly time step, jaehak jeong
        flohr = 0.
        sedhr = 0.
        orgnhr = 0.
        orgphr = 0.
        no3hr  = 0.
        minphr = 0.
        nh3hr = 0.
        no2hr = 0.
        cmtl1hr = 0.
        cmtl2hr = 0.
        cmtl3hr = 0.
        bactphr = 0.
        bactlphr = 0.
        chlahr = 0.
        disoxhr = 0.
        cbodhr = 0.
        solpsthr = 0.
        srbpsthr = 0.
        if (ifirsthr(inum1) == 0) then
          read (200+inum1,*) idap, iyp, ihr, flohr, sedhr, orgnhr,      
     &        orgphr, no3hr, nh3hr, no2hr, minphr, cbodhr, disoxhr,     
     &        chlahr, solpsthr, srbpsthr, bactphr, bactlphr, cmtl1hr,   
     &        cmtl2hr, cmtl3hr
        else
          ifirsthr(inum1) = 0
          do
            read (200+inum1,*) idap, iyp, ihr, flohr, sedhr, orgnhr,    
     &        orgphr, no3hr, nh3hr, no2hr, minphr, cbodhr, disoxhr,     
     &        chlahr, solpsthr, srbpsthr, bactphr, bactlphr, cmtl1hr,
     &        cmtl2hr, cmtl3hr
            if (iyp + idap <= 0) exit
            if (iyp == iyr .and. idap == id1) exit
          end do
        endif

        hhvaroute(2,ihout,ii) = flohr
        hhvaroute(3,ihout,ii) = sedhr
        hhvaroute(4,ihout,ii) = orgnhr
        hhvaroute(5,ihout,ii) = orgphr
        hhvaroute(6,ihout,ii) = no3hr
        hhvaroute(7,ihout,ii) = minphr
        hhvaroute(11,ihout,ii) = solpsthr
        hhvaroute(12,ihout,ii) = srbpsthr
        hhvaroute(13,ihout,ii) = chlahr
        hhvaroute(14,ihout,ii) = nh3hr
        hhvaroute(15,ihout,ii) = no2hr
        hhvaroute(16,ihout,ii) = cbodhr
        hhvaroute(17,ihout,ii) = disoxhr
        hhvaroute(18,ihout,ii) = bactphr
        hhvaroute(19,ihout,ii) = bactlphr
        hhvaroute(20,ihout,ii) = cmtl1hr
        hhvaroute(21,ihout,ii) = cmtl2hr
        hhvaroute(22,ihout,ii) = cmtl3hr

        varoute(2,ihout) = varoute(2,ihout) + flohr
        varoute(3,ihout) = varoute(3,ihout) + sedhr
        varoute(4,ihout) = varoute(4,ihout) + orgnhr
        varoute(5,ihout) = varoute(5,ihout) + orgphr
        varoute(6,ihout) = varoute(6,ihout) + no3hr
        varoute(7,ihout) = varoute(7,ihout) + minphr
        varoute(11,ihout) = varoute(11,ihout) + solpsthr
        varoute(12,ihout) = varoute(12,ihout) + srbpsthr
        varoute(13,ihout) = varoute(13,ihout) + chlahr
        varoute(14,ihout) = varoute(14,ihout) + nh3hr
        varoute(15,ihout) = varoute(15,ihout) + no2hr
        varoute(16,ihout) = varoute(16,ihout) + cbodhr
        varoute(17,ihout) = varoute(17,ihout) + disoxhr
        varoute(18,ihout) = varoute(18,ihout) + bactphr
        varoute(19,ihout) = varoute(19,ihout) + bactlphr
        varoute(20,ihout) = varoute(20,ihout) + cmtl1hr
        varoute(21,ihout) = varoute(21,ihout) + cmtl2hr
        varoute(22,ihout) = varoute(22,ihout) + cmtl3hr

      !! Assumed equal distribution of sediment
      varoute(23,ihout) = varoute(23,ihout) + sedhr * 0.   ! sand
      varoute(24,ihout) = varoute(24,ihout) + sedhr * 1.   ! silt
      varoute(25,ihout) = varoute(25,ihout) + sedhr * 0.   ! cla
      varoute(26,ihout) = varoute(26,ihout) + sedhr * 0.   ! sag
      varoute(27,ihout) = varoute(27,ihout) + sedhr * 0.   ! lag
      varoute(28,ihout) = 0.                    ! gravel

      
      QHY(ii,ihout,IHX(1)) = hhvaroute(2,ihout,ii) / (dthy * 3600.) !added by Jaehak for subdaily routing, 2019

      end do

      return
      end