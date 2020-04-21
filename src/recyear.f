      subroutine recyear
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine inputs measured loadings to the stream network
!!    for routing through the watershed where the records are summarized
!!    on an annual basis

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpyr(:,:)|# cfu/100ml  |average daily loading of less persistent
!!                               |bacteria for year
!!    bactpyr(:,:)|# cfu/100ml   |average daily loading of persistent bacteria
!!                               |for year
!!    cbodyr(:,:) |kg/day        |average daily loading of CBOD for year
!!    chlayr(:,:) |kg/day        |average daily loading of chlorophyll-a for year
!!    cmtl1yr(:,:)|kg/day        |average daily loading of conservative metal #1
!!                               |for year
!!    cmtl2yr(:,:)|kg/day        |average daily loading of conservative metal #2
!!                               |for year
!!    cmtl3yr(:,:)|kg/day        |average daily loading of conservative metal #3
!!                               |for year
!!    curyr       |none          |year of simulation
!!    disoxyr(:,:)|kg/day        |average daily loading of dissolved oxygen for 
!!                               |year
!!    floyr(:,:)  |m**3/d        |average daily water loading for year
!!    ihout       |none          |hydrograph storage location number
!!    inum1       |none          |file number
!!    minpyr(:,:) |kg P/day      |average daily mineral P loading for year
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    nh3yr(:,:)  |kg N/day      |average daily NH3-N loading for year
!!    no2yr(:,:)  |kg N/day      |average daily NO2-N loading for year
!!    no3yr(:,:)  |kg N/day      |average daily NO3-N loading for year
!!    orgnyr(:,:) |kg N/day      |average daily organic N loading for year
!!    orgpyr(:,:) |kg P/day      |average daily organic P loading for year
!!    sedyr(:,:)  |metric tons/d |average daily sediment loading for year
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
!!    varoute(2,:)     |m^3          |volume of water
!!    varoute(3,:)     |metric tons  |sediment
!!    varoute(4,:)     |kg N         |organic N
!!    varoute(5,:)     |kg P         |organic P
!!    varoute(6,:)     |kg N         |NO3-N
!!    varoute(7,:)     |kg P         |mineral (soluble) P
!!    varoute(13,:)    |kg           |chlorophyll-a
!!    varoute(14,:)    |kg N         |NH3
!!    varoute(15,:)    |kg N         |NO2
!!    varoute(16,:)    |kg           |CBOD
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
!!    ii          |none          |counter
!!    j           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: ii, j

!! zero flow out variables
      do j = 1, mvaro
        varoute(j,ihout) = 0.
        do ii = 1, nstep
          hhvaroute(j,ihout,ii) = 0.
        end do
      end do


      varoute(2,ihout) = floyr(inum1,curyr)
      varoute(3,ihout) = sedyr(inum1,curyr)
      varoute(4,ihout) = orgnyr(inum1,curyr)
      varoute(5,ihout) = orgpyr(inum1,curyr)
      varoute(6,ihout) = no3yr(inum1,curyr)
      varoute(7,ihout) = minpyr(inum1,curyr)
      varoute(11,ihout) = solpstyr(inum1,curyr)
      varoute(12,ihout) = srbpstyr(inum1,curyr)
      varoute(13,ihout) = chlayr(inum1,curyr)
      varoute(14,ihout) = nh3yr(inum1,curyr)
      varoute(15,ihout) = no2yr(inum1,curyr)
      varoute(16,ihout) = cbodyr(inum1,curyr)
      varoute(17,ihout) = disoxyr(inum1,curyr)
      varoute(18,ihout) = bactpyr(inum1,curyr)
      varoute(19,ihout) = bactlpyr(inum1,curyr)
      varoute(20,ihout) = cmtl1yr(inum1,curyr)
      varoute(21,ihout) = cmtl2yr(inum1,curyr)
      varoute(22,ihout) = cmtl3yr(inum1,curyr)

      !! Assumed equal distribution of sediment
      varoute(23,ihout) = sedyr(inum1,curyr) * 0.   ! sand
      varoute(24,ihout) = sedyr(inum1,curyr) * 1.   ! silt
      varoute(25,ihout) = sedyr(inum1,curyr) * 0.   ! cla
      varoute(26,ihout) = sedyr(inum1,curyr) * 0.   ! sag
      varoute(27,ihout) = sedyr(inum1,curyr) * 0.   ! lag
      varoute(28,ihout) = 0.                        ! gravel

      if (ievent > 2) then
        do ii = 1, nstep
          hhvaroute(2,ihout,ii) = floyr(inum1,curyr) / real(nstep)
          hhvaroute(3,ihout,ii) = sedyr(inum1,curyr) / real(nstep)
          hhvaroute(4,ihout,ii) = orgnyr(inum1,curyr) / real(nstep)
          hhvaroute(5,ihout,ii) = orgpyr(inum1,curyr) / real(nstep)
          hhvaroute(6,ihout,ii) = no3yr(inum1,curyr) / real(nstep)
          hhvaroute(7,ihout,ii) = minpyr(inum1,curyr) / real(nstep)
          hhvaroute(11,ihout,ii) = solpstyr(inum1,curyr) / real(nstep)
          hhvaroute(12,ihout,ii) = srbpstyr(inum1,curyr) / real(nstep)
          hhvaroute(13,ihout,ii) = chlayr(inum1,curyr) / real(nstep)
          hhvaroute(14,ihout,ii) = nh3yr(inum1,curyr) / real(nstep)
          hhvaroute(15,ihout,ii) = no2yr(inum1,curyr) / real(nstep)
          hhvaroute(16,ihout,ii) = cbodyr(inum1,curyr) / real(nstep)
          hhvaroute(17,ihout,ii) = disoxyr(inum1,curyr) / real(nstep)
          hhvaroute(18,ihout,ii) = bactpyr(inum1,curyr) / real(nstep)
          hhvaroute(19,ihout,ii) = bactlpyr(inum1,curyr) / real(nstep)
          hhvaroute(20,ihout,ii) = cmtl1yr(inum1,curyr) / real(nstep)
          hhvaroute(21,ihout,ii) = cmtl2yr(inum1,curyr) / real(nstep)
          hhvaroute(22,ihout,ii) = cmtl3yr(inum1,curyr) / real(nstep)
        end do
      end if

      return
      end
 
