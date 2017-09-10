      subroutine apex_day
      
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

      integer :: ii, j

      do j = 1, mvaro
        varoute(j,ihout) = 0.
        if (ievent > 0) then
          do ii = 1, nstep
            hhvaroute(j,ihout,ii) = 0.
          end do
        endif
      end do

!!   read from apex measured file
!!   check if idap/iyp are same
     
!!    changes for Mike Winchell if apex.swt files has a different start date than the SWAT run
      if (ifirsta(inum1) == 1) then
        do 
          read (112+inum1,*) idapa(inum1), iypa(inum1), flodaya(inum1),
     & seddaya(inum1), orgndaya(inum1), orgpdaya(inum1), no3daya(inum1),
     & minpdaya(inum1)
	    if(idapa(inum1) == id1 .and. iypa(inum1) == iyr) exit
        end do
        ifirsta(inum1) = 0
	endif 

      if (iypa(inum1) == iyr .and. idapa(inum1) == i) then
         varoute(2,ihout) = flodaya(inum1)
         varoute(3,ihout) = seddaya(inum1)
         varoute(4,ihout) = orgndaya(inum1)
         varoute(5,ihout) = orgpdaya(inum1)
         varoute(6,ihout) = no3daya(inum1)
         varoute(7,ihout) = minpdaya(inum1)
         varoute(14,ihout) = 0.0     
         varoute(15,ihout) = 0.0    
         varoute(11,ihout) = 0.0        
         varoute(12,ihout) = 0.0       
         varoute(13,ihout) = 0.0     
         varoute(14,ihout) = 0.0    
         varoute(15,ihout) = 0.0    
         varoute(16,ihout) = 0.0     
         varoute(17,ihout) = 0.0      
         varoute(18,ihout) = 0.0
         varoute(19,ihout) = 0.0       
         varoute(20,ihout) = 0.0      
         varoute(21,ihout) = 0.0      
         varoute(22,ihout) = 0.0 
         if (curyr /= nbyr .and. iida /= idal) then
           read (112+inum1,*) idapa(inum1), iypa(inum1), flodaya(inum1),
     & seddaya(inum1), orgndaya(inum1), orgpdaya(inum1), no3daya(inum1),
     & minpdaya(inum1)
         endif
      else
         varoute(2,ihout) = 0.0
         varoute(3,ihout) = 0.0              
         varoute(4,ihout) = 0.0               
         varoute(5,ihout) = 0.0              
         varoute(6,ihout) = 0.0              
         varoute(7,ihout) = 0.0                
         varoute(14,ihout) = 0.0     
         varoute(15,ihout) = 0.0    
         varoute(11,ihout) = 0.0        
         varoute(12,ihout) = 0.0       
         varoute(13,ihout) = 0.0     
         varoute(14,ihout) = 0.0    
         varoute(15,ihout) = 0.0    
         varoute(16,ihout) = 0.0     
         varoute(17,ihout) = 0.0      
         varoute(18,ihout) = 0.0      
         varoute(19,ihout) = 0.0       
         varoute(20,ihout) = 0.0      
         varoute(21,ihout) = 0.0      
         varoute(22,ihout) = 0.0      
	endif
     
      if (ievent > 0) then
        do ii = 1, nstep
          hhvaroute(2,ihout,ii) = flodaya(inum1) / real(nstep)
          hhvaroute(3,ihout,ii) = seddaya(inum1) / real(nstep)
          hhvaroute(4,ihout,ii) = orgndaya(inum1) / real(nstep)
          hhvaroute(5,ihout,ii) = orgpdaya(inum1) / real(nstep)
          hhvaroute(6,ihout,ii) = no3daya(inum1) / real(nstep)
          hhvaroute(7,ihout,ii) = minpdaya(inum1) / real(nstep)

        end do
      end if

      return
      end