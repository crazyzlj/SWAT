      subroutine autocal

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine saves hourly or average daily concentrations from 
!!    a particular hydrograph node to a file, to be used for auto-calibration

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
!!    hhvaroute(18,:)|# bact        |persistent bacteria
!!    hhvaroute(19,:)|# bact        |less persistent bacteria
!!    hhvaroute(20,:)|kg            |conservative metal #1
!!    hhvaroute(21,:)|kg            |conservative metal #2
!!    hhvaroute(22,:)|kg            |conservative metal #3
!!    ihout          |none          |hydrograph storage location of data to be
!!                                  |printed to saveconc file
!!    ievent         |none          |rainfall/runoff code
!!                                  |0 daily rainfall/curve number technique
!!                                  |1 daily rainfall/Green&Ampt technique/daily
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
!!    varoute(18,:)  |# bact        |persistent bacteria
!!    varoute(19,:)  |# bact        |less persistent bacteria
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
!!    varii(20)   |mg/L          |Kjeldahl N
!!    varii(21)   |mg/L          |Total N
!!    varii(22)   |mg/L          |Total P
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    subroutine developed by A. Van Griensven, 
!!    Hydrology-Vrije Universiteit Brussel, Belgium

      use parm

      real, dimension (22) :: varii
      integer :: ii, j
	character*1 mflag
	if (curyr > nyskip) then
      if (inum1 <= 100 .and. inum1 > 0) then
	select case (inum2)

	case(1)  
      if (ievent == 3) then
        !! Write hourly values
        do ii = 1, 24
          varii = 0.
          if (hhvaroute(2,ihout,ii) > 0.01) then
            varii(1) = hhvaroute(2,ihout,ii) / 3600.
            varii(2) = hhvaroute(3,ihout,ii) * 1.e6                     &
     &                                           / hhvaroute(2,ihout,ii)
            varii(3) = hhvaroute(4,ihout,ii) * 1000.                    &
     &                                           / hhvaroute(2,ihout,ii)
            varii(4) = hhvaroute(5,ihout,ii) * 1000.                    &
     &                                           / hhvaroute(2,ihout,ii)
            varii(5) = hhvaroute(6,ihout,ii) * 1000.                    &
     &                                           / hhvaroute(2,ihout,ii)
            varii(6) = hhvaroute(14,ihout,ii) * 1000.                   &
     &                                           / hhvaroute(2,ihout,ii)
            varii(7) = hhvaroute(15,ihout,ii) * 1000.                   &
     &                                           / hhvaroute(2,ihout,ii)
            varii(8) = hhvaroute(7,ihout,ii) * 1000.                    &
     &                                           / hhvaroute(2,ihout,ii)
            varii(9) = hhvaroute(16,ihout,ii) * 1000.                   &
     &                                           / hhvaroute(2,ihout,ii)
            varii(10) = hhvaroute(17,ihout,ii) * 1000.                  &
     &                                           / hhvaroute(2,ihout,ii)
            varii(11) = hhvaroute(13,ihout,ii) * 1.e6                   &
     &                                           / hhvaroute(2,ihout,ii)
            varii(12) = hhvaroute(11,ihout,ii)                          &
     &                                 / (1000. * hhvaroute(2,ihout,ii))
            varii(13) = hhvaroute(12,ihout,ii)                          &
     &                                 / (1000. * hhvaroute(2,ihout,ii))
            varii(14) = hhvaroute(18,ihout,ii)                          &
     &                                 / (1000. * hhvaroute(2,ihout,ii))
            varii(15) = hhvaroute(19,ihout,ii)                          &
     &                                 / (1000. * hhvaroute(2,ihout,ii))
            varii(16) = hhvaroute(20,ihout,ii) * 1000.                  &
     &                                           / hhvaroute(2,ihout,ii)
            varii(17) = hhvaroute(21,ihout,ii) * 1000.                  &
     &                                           / hhvaroute(2,ihout,ii)
            varii(18) = hhvaroute(22,ihout,ii) * 1000.                  &
     &                                           / hhvaroute(2,ihout,ii)
            varii(19) = hhvaroute(1,ihout,ii)

            varii(20) = varii(3)+varii(6)
            varii(21) = varii(20)+varii(5)+varii(7)
		  varii(22) = varii(4)+varii(8) 

          end if
          write (8000+inum1,2000) iyr, iida, ii-1, (varii(j), j = 1, 22)
        end do
	end if
      case(0)
        !! Write daily values
        varii = 0.


	if (varoute(2,ihout) > 0.1) then
          varii(1) = varoute(2,ihout) / 86400.
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
          varii(14) = varoute(18,ihout) / (1000. * varoute(2,ihout))
          varii(15) = varoute(19,ihout) / (1000. * varoute(2,ihout))
          varii(16) = varoute(20,ihout) * 1000. / varoute(2,ihout)
          varii(17) = varoute(21,ihout) * 1000. / varoute(2,ihout)
          varii(18) = varoute(22,ihout) * 1000. / varoute(2,ihout)
          varii(19) = varoute(1,ihout)
          varii(20) = varii(3)+varii(6)
          varii(21) = varii(20)+varii(5)+varii(7)
		varii(22) = varii(4)+varii(8) 
        endif
        write (8000+inum1,1000) iyr, iida, (varii(ii), ii = 1, 22)
	
	case(2)
	! write yearly values
	

	mflag='y'
	itelyr(inum1)=itelyr(inum1)+1
          variiyr(inum1,1) = variiyr(inum1,1)+ varoute(2,ihout)
          variiyr(inum1,2) = variiyr(inum1,2)+ varoute(3,ihout) 
          variiyr(inum1,3) = variiyr(inum1,3)+varoute(4,ihout)
          variiyr(inum1,4) = variiyr(inum1,4)+ varoute(5,ihout)
          variiyr(inum1,5) = variiyr(inum1,5)+ varoute(6,ihout)
          variiyr(inum1,6) = variiyr(inum1,6)+ varoute(14,ihout)
          variiyr(inum1,7) = variiyr(inum1,7) + varoute(15,ihout) 
          variiyr(inum1,8) = variiyr(inum1,8)+ varoute(7,ihout) 
          variiyr(inum1,9) = variiyr(inum1,9) + varoute(16,ihout) 
          variiyr(inum1,10) = variiyr(inum1,10) + varoute(17,ihout)
          variiyr(inum1,11) = variiyr(inum1,11)+ varoute(13,ihout) 
          variiyr(inum1,12) = variiyr(inum1,12)+ varoute(11,ihout) 
          variiyr(inum1,13) = variiyr(inum1,13) + varoute(12,ihout) 
	



        !! calculate number of days in year
        idlast = 0
          idlast = 366
          if (leapyr == 1) idlast = idlast - 1
	iii	=idlast
	if (idlast== iida) then
	if (itelyr(inum1).gt.0) then

		do ii=1,16
	variiyr(inum1,ii)=variiyr(inum1,ii)/itelyr(inum1)
	end do
		variiyr(inum1,1) = variiyr(inum1,1)/3600./24.
	    variiyr(inum1,14) = variiyr(inum1,3)+variiyr(inum1,6)
          variiyr(inum1,15) = variiyr(inum1,14)+variiyr(inum1,5)+
     *		variiyr(inum1,7)
		variiyr(inum1,16) =variiyr(inum1,4)+variiyr(inum1,8)
	write(8000+inum1,4000)iyr,mflag, 
     *	(variiyr(inum1,ii), ii = 1, 16)


	do ii=1,16
	variiyr(inum1,ii)=0.
	end do
	itelyr(inum1)=0
	end if
	end if
	
	case(3)
      ! write monthly values
	mflag='m'
	itelmon(inum1)=itelmon(inum1)+1
          variimon(inum1,1) = variimon(inum1,1)+ varoute(2,ihout)
          variimon(inum1,2) = variimon(inum1,2)+ varoute(3,ihout) 
          variimon(inum1,3) = variimon(inum1,3)+varoute(4,ihout)
          variimon(inum1,4) = variimon(inum1,4)+ varoute(5,ihout)
          variimon(inum1,5) = variimon(inum1,5)+ varoute(6,ihout)
          variimon(inum1,6) = variimon(inum1,6)+ varoute(14,ihout)
          variimon(inum1,7) = variimon(inum1,7) + varoute(15,ihout) 
          variimon(inum1,8) = variimon(inum1,8)+ varoute(7,ihout) 
          variimon(inum1,9) = variimon(inum1,9) + varoute(16,ihout) 
          variimon(inum1,10) = variimon(inum1,10) + varoute(17,ihout)
          variimon(inum1,11) = variimon(inum1,11)+ varoute(13,ihout) 
          variimon(inum1,12) = variimon(inum1,12)+ varoute(11,ihout) 
          variimon(inum1,13) = variimon(inum1,13) + varoute(12,ihout) 
	



        !! calculate number of days in month
        idlast = 0
          idlast = ndays(mo_chk+1) 
          if (leapyr == 1 .and. mo_chk > 1) idlast = idlast - 1

	iii	=idlast
	if (idlast== iida) then
	if (itelmon(inum1).gt.0) then

		do ii=1,16
	variimon(inum1,ii)=variimon(inum1,ii)/itelmon(inum1)
	end do
		variimon(inum1,1) = variimon(inum1,1)/3600./24.
	    variimon(inum1,14) = variimon(inum1,3)+variimon(inum1,6)
          variimon(inum1,15) = variimon(inum1,14)+variimon(inum1,5)+
     *		variimon(inum1,7)
		variimon(inum1,16) =variimon(inum1,4)+variimon(inum1,8)
	write(8000+inum1,3000)iyr,mflag,i_mo, 
     *	(variimon(inum1,ii), ii = 1, 16)


	do ii=1,16
	variimon(inum1,ii)=0.
	end do
	itelmon(inum1)=0
	end if
	end if
	

	end select	
	endif
	end if
      return
 4000 format (1x,i4,1x,a1,6x,22e11.3)
 3000 format (1x,i4,1x,a1,1x,i2,3x,22e11.3)
 2000 format (1x,i4,2x,i3,1x,i2,22e11.3)
 1000 format (1x,i4,2x,i3,3x,22e11.3)
      end
