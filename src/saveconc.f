      subroutine saveconc
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine saves hourly or average daily concentrations from 
!!    a particular hydrograph node to a file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhvaroute(1,:) |deg C         |temperature
!!    hhvaroute(2,:) |m^3 H2O       |water
!!    hhvaroute(3,:) |metric tons   |sediment
!!    hhvaroute(4,:) |kg N          |organic nitrogen
!!    hhvaroute(5,:) |kg P          |organic phosphorus
!!    hhvaroute(6,:) |kg N          |nitrate
!!    hhvaroute(7,:) |kg P          |mineral phosphorus
!!    hhvaroute(11,:)|mg pst        |pesticide in solution
!!    hhvaroute(12,:)|mg pst        |pesticide sorbed to sediment
!!    hhvaroute(13,:)|kg            |chlorophyll-a
!!    hhvaroute(14,:)|kg N          |ammonia
!!    hhvaroute(15,:)|kg N          |nitrite
!!    hhvaroute(16,:)|kg            |carbonaceous biological oxygen demand 
!!    hhvaroute(17,:)|kg            |dissolved oxygen
!!    hhvaroute(18,:)|# cfu/100ml   |persistent bacteria
!!    hhvaroute(19,:)|# cfu/100ml   |less persistent bacteria
!!    hhvaroute(20,:)|kg            |conservative metal #1
!!    hhvaroute(21,:)|kg            |conservative metal #2
!!    hhvaroute(22,:)|kg            |conservative metal #3
!!    ihout          |none          |hydrograph storage location of data to be
!!                                  |printed to saveconc file
!!    ievent         |none          |rainfall/runoff code
!!                                  |0 daily rainfall/curve number technique
!!                                  |1 daily rainfall/curve number technique/daily
!!                                  |  routing
!!                                  |2 sub-daily rainfall/Green&Ampt technique/
!!                                  |  daily routing
!!                                  |3 sub-daily rainfall/Green&Ampt/hourly 
!!                                  |  routing
!!    inum2          |none          |printout frequency for saveconc command
!!                                  |0 daily average concentrations
!!                                  |1 hourly average concentrations
!!    mvaro          |none          |max number of variables routed through the
!!                                  |reach
!!    varoute(1,:)   |deg C         |temperature
!!    varoute(2,:)   |m^3 H2O       |water
!!    varoute(3,:)   |metric tons   |sediment or suspended solid load
!!    varoute(4,:)   |kg N          |organic nitrogen
!!    varoute(5,:)   |kg P          |organic phosphorus
!!    varoute(6,:)   |kg N          |nitrate
!!    varoute(7,:)   |kg P          |mineral phosphorus
!!    varoute(11,:)  |mg pst        |pesticide in solution
!!    varoute(12,:)  |mg pst        |pesticide sorbed to sediment
!!    varoute(13,:)  |kg            |chlorophyll-a
!!    varoute(14,:)  |kg N          |ammonia
!!    varoute(15,:)  |kg N          |nitrite
!!    varoute(16,:)  |kg            |carbonaceous biological oxygen demand
!!    varoute(17,:)  |kg            |dissolved oxygen
!!    varoute(18,:)  |# cfu/100ml   |persistent bacteria
!!    varoute(19,:)  |# cfu/100ml   |less persistent bacteria
!!    varoute(20,:)  |kg            |conservative metal #1
!!    varoute(21,:)  |kg            |conservative metal #2
!!    varoute(22,:)  |kg            |conservative metal #3
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter
!!    varii(1)    |m^3/s         |rate of flow
!!    varii(2)    |mg/L          |sediment concentration
!!    varii(3)    |mg N/L        |organic N concentration
!!    varii(4)    |mg P/L        |organic P concentration
!!    varii(5)    |mg N/L        |nitrate concentration
!!    varii(6)    |mg N/L        |ammonia concentration
!!    varii(7)    |mg N/L        |nitrite concentration
!!    varii(8)    |mg P/L        |mineral P concentration
!!    varii(9)    |mg/L          |CBOD concentration
!!    varii(10)   |mg/L          |dissolved oxygen concentration
!!    varii(11)   |microg/L      |chl-a concentration
!!    varii(12)   |mg/L          |soluble pesticide concentration
!!    varii(13)   |mg/L          |sorbed pesiticide concentration
!!    varii(14)   |ct/L          |persistent bacteria concentration
!!    varii(15)   |ct/L          |less persistent bacteria concentration
!!    varii(16)   |mg/L          |conservative metal #1 concentration
!!    varii(17)   |mg/L          |conservative metal #2 concentration
!!    varii(18)   |mg/L          |conservative metal #3 concentration
!!    varii(19)   |deg C         |temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven, 
!!    Hydrology-Vrije Universiteit Brussel, Belgium

!!    Modified by N.Kannan, Blackland Research at Temple

      use parm

      real*8, dimension (19) :: varii
      integer :: ii, j
      real*8 :: inflow
      
      if (inum1 <= 50 .and. inum1 > 0) then
      if (ievent == 1 .and. inum2 == 1) then


        !! Write sub-daily values (any time step) : URBAN MODELING
        !! Convert the unit of "hhvaroute" from "m3/dt" to "m3/sec" where dt in minute

        do ii = 1, nstep
          varii = 0.
          if (hhvaroute(2,ihout,ii) > 0.001) then
            varii(1) = hhvaroute(2,ihout,ii) / (idt * 60.)  !! urban modeling by J.Jeong 4/17/2008
            varii(2) = hhvaroute(3,ihout,ii) * 1.e6                     
     &                                           / hhvaroute(2,ihout,ii)
     !!       varii(3) = hhvaroute(4,ihout,ii) * 1000.                    
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(4) = hhvaroute(5,ihout,ii) * 1000.                    
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(5) = hhvaroute(6,ihout,ii) * 1000.                    
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(6) = hhvaroute(14,ihout,ii) * 1000.                   
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(7) = hhvaroute(15,ihout,ii) * 1000.                   
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(8) = hhvaroute(7,ihout,ii) * 1000.                    
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(9) = hhvaroute(16,ihout,ii) * 1000.                   
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(10) = hhvaroute(17,ihout,ii) * 1000.                  
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(11) = hhvaroute(13,ihout,ii) * 1.e6                   
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(12) = hhvaroute(11,ihout,ii)                          
     !!&                                 / (1000. * hhvaroute(2,ihout,ii))
     !!       varii(13) = hhvaroute(12,ihout,ii)                          
     !!&                                 / (1000. * hhvaroute(2,ihout,ii))
     !!       varii(14) = hhvaroute(18,ihout,ii)                          
     !!&                                 / (1000. * hhvaroute(2,ihout,ii))
     !!       varii(15) = hhvaroute(19,ihout,ii)                          
     !!&                                 / (1000. * hhvaroute(2,ihout,ii))
     !!       varii(16) = hhvaroute(20,ihout,ii) * 1000.                  
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(17) = hhvaroute(21,ihout,ii) * 1000.                  
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(18) = hhvaroute(22,ihout,ii) * 1000.                  
     !!&                                           / hhvaroute(2,ihout,ii)
     !!       varii(19) = hhvaroute(1,ihout,ii)
     !     !end if
          if (curyr > nyskip) then
		  !write (50+inum1,2000) iyr, iida, ii-1, (varii(j), j = 1, 19)
		  write (50+inum1,2000) iyr, iida, ii-1, (varii(j), j = 1, 2)
           endif
           endif
        end do
      else
        !! Write daily values
        varii = 0.
        if (ievent == 1) then         !! sum sub-daily to daily if needed
          !! zero daily flow out variables
          do ii = 1, mvaro
            varoute(ii,ihout) = 0.
          end do
          do ii = 1, nstep
            varoute(2,ihout) = varoute(2,ihout) + hhvaroute(2,ihout,ii)
          end do
          if (varoute(2,ihout) > 0.1) then
            do ii = 1, nstep
              do j = 3, mvaro
                varoute(j,ihout) = varoute(j,ihout) +                   
     &                                             hhvaroute(j,ihout,ii)
              end do
              varoute(1,ihout) = varoute(1,ihout) +                     
     &                     hhvaroute(2,ihout,ii) * hhvaroute(1,ihout,ii)
            end do
            varoute(1,ihout) = varoute(1,ihout) / varoute(2,ihout)
          end if
        end if
	  if (varoute(2,ihout) > 0.1) then
          varii(1) = varoute(2,ihout) / 86400.   ! changed by J.Jeong 4/17/2008
          varii(2) = varoute(3,ihout) * 1.e6 / varoute(2,ihout)
          varii(3) = varoute(4,ihout) * 1000. / varoute(2,ihout)
          varii(4) = varoute(5,ihout) * 1000. / varoute(2,ihout)
          varii(5) = varoute(6,ihout) * 1000. / varoute(2,ihout)
          varii(6) = varoute(14,ihout) * 1000. / varoute(2,ihout)
          varii(7) = varoute(15,ihout) * 1000. / varoute(2,ihout)
          varii(8) = varoute(7,ihout) * 1000. / varoute(2,ihout)
          varii(9) = varoute(16,ihout) * 1000. / varoute(2,ihout)
          varii(10) = varoute(17,ihout) * 1000. / varoute(2,ihout)
          varii(11) = varoute(13,ihout) * 1.e6 / varoute(2,ihout)
          varii(12) = varoute(11,ihout) / (1000. * varoute(2,ihout))
          varii(13) = varoute(12,ihout) / (1000. * varoute(2,ihout))
          varii(14) = varoute(18,ihout)
          varii(15) = varoute(19,ihout)
          varii(16) = varoute(20,ihout) * 1000. / varoute(2,ihout)
          varii(17) = varoute(21,ihout) * 1000. / varoute(2,ihout)
          varii(18) = varoute(22,ihout) * 1000. / varoute(2,ihout)
          varii(19) = varoute(1,ihout)
        endif
        write (50+inum1,1000)iyr, iida, '   0',(varii(ii), ii = 1, 19)
      endif
      endif

      return
 2000 format (1x,i4,2x,i3,1x,i4,20(1x,e10.3))
 1000 format (1x,i4,2x,i3,3x,a4,19(1x,e10.3))
      end