      subroutine reccnst

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine inputs measured loadings to the stream network
!!    for routing through the watershed where the records are averaged
!!    over the entire period of record

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpcnst(:)|# cfu/100ml  |average daily less persistent bacteria
!!                               |loading to reach
!!    bactpcnst(:)|# cfu/100ml   |average daily persistent bacteria loading
!!                               |to reach
!!    cbodcnst(:) |kg/day        |average daily CBOD loading to reach
!!    chlacnst(:) |kg/day        |average daily chlorophyll-a loading to reach
!!    cmtl1cnst(:)|kg/day        |average daily conservative metal #1 loading
!!    cmtl2cnst(:)|kg/day        |average daily conservative metal #2 loading
!!    cmtl3cnst(:)|kg/day        |average daily conservative metal #3 loading
!!    disoxcnst(:)|kg/day        |average daily dissolved oxygen loading to reach
!!    flocnst(:)  |m^3 H2O/day   |average daily water loading to reach
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 daily rainfall/Green&Ampt technique/daily
!!                               |  routing
!!                               |2 sub-daily rainfall/Green&Ampt technique/
!!                               |  daily routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ihout       |none          |hydrograph storage location number
!!    inum1       |none          |file number
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    minpcnst(:) |kg P/day      |average daily soluble P loading to reach
!!    nh3cnst(:)  |kg N/day      |average daily ammonia loading to reach
!!    no2cnst(:)  |kg N/day      |average daily nitrite loading to reach
!!    no3cnst(:)  |kg N/day      |average daily nitrate loading to reach
!!    orgncnst(:) |kg N/day      |average daily organic N loading to reach
!!    orgpcnst(:) |kg P/day      |average daily organic P loading to reach
!!    sedcnst(:)  |metric tons/d |average daily sediment loading to reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: ii, j

!! zero flow out variables
      do j = 1, mvaro
        varoute(j,ihout) = 0.
        if (ievent > 1) then
          do ii = 1, nstep
            hhvaroute(j,ihout,ii) = 0.
          end do
        endif
      end do

      varoute(2,ihout) = flocnst(inum1)
      varoute(3,ihout) = sedcnst(inum1)
      varoute(4,ihout) = orgncnst(inum1)
      varoute(5,ihout) = orgpcnst(inum1)
      varoute(6,ihout) = no3cnst(inum1)
      varoute(7,ihout) = minpcnst(inum1)
      varoute(11,ihout) = solpstcnst(inum1)
      varoute(12,ihout) = srbpstcnst(inum1)
      varoute(13,ihout) = chlacnst(inum1)
      varoute(14,ihout) = nh3cnst(inum1)
      varoute(15,ihout) = no2cnst(inum1)
      varoute(16,ihout) = cbodcnst(inum1)
      varoute(17,ihout) = disoxcnst(inum1)
      varoute(18,ihout) = bactpcnst(inum1)
      varoute(19,ihout) = bactlpcnst(inum1)
      varoute(20,ihout) = cmtl1cnst(inum1)
      varoute(21,ihout) = cmtl2cnst(inum1)
      varoute(22,ihout) = cmtl3cnst(inum1)

      !! Assumed equal distribution of sediment
      varoute(23,ihout) = sedcnst(inum1) * 0.   ! sand
      varoute(24,ihout) = sedcnst(inum1) * 1.   ! silt
      varoute(25,ihout) = sedcnst(inum1) * 0.   ! cla
      varoute(26,ihout) = sedcnst(inum1) * 0.   ! sag
      varoute(27,ihout) = sedcnst(inum1) * 0.   ! lag
      varoute(28,ihout) = 0.                    ! gravel

      if (ievent > 2) then
        do ii = 1,nstep
          hhvaroute(2,ihout,ii) = flocnst(inum1) / real(nstep)
          hhvaroute(3,ihout,ii) = sedcnst(inum1) / real(nstep)
          hhvaroute(4,ihout,ii) = orgncnst(inum1) / real(nstep)
          hhvaroute(5,ihout,ii) = orgpcnst(inum1) / real(nstep)
          hhvaroute(6,ihout,ii) = no3cnst(inum1) / real(nstep)
          hhvaroute(7,ihout,ii) = minpcnst(inum1) / real(nstep)
          hhvaroute(11,ihout,ii) = solpstcnst(inum1) / real(nstep)
          hhvaroute(12,ihout,ii) = srbpstcnst(inum1) / real(nstep)
          hhvaroute(13,ihout,ii) = chlacnst(inum1) / real(nstep)
          hhvaroute(14,ihout,ii) = nh3cnst(inum1) / real(nstep)
          hhvaroute(15,ihout,ii) = no2cnst(inum1) / real(nstep)
          hhvaroute(16,ihout,ii) = cbodcnst(inum1) / real(nstep)
          hhvaroute(17,ihout,ii) = disoxcnst(inum1) / real(nstep)
          hhvaroute(18,ihout,ii) = bactpcnst(inum1) / real(nstep)
          hhvaroute(19,ihout,ii) = bactlpcnst(inum1) / real(nstep)
          hhvaroute(20,ihout,ii) = cmtl1cnst(inum1) / real(nstep)
          hhvaroute(21,ihout,ii) = cmtl2cnst(inum1) / real(nstep)
          hhvaroute(22,ihout,ii) = cmtl3cnst(inum1) / real(nstep)
        end do
      end if


      return
      end

