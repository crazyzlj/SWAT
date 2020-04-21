      subroutine surfst_h2o

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine determines the net surface runoff reaching the 
!!    main channel on a given day. The net amount of water reaching
!!    the main channel can include water in surface runoff from the 
!!    previous day and will exclude surface runoff generated on the
!!    current day which takes longer than one day to reach the main
!!    channel

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    brt(:)      |none          |fraction of surface runoff that takes
!!                               |one day or less to reach the subbasin
!!                               |outlet
!!    ihru        |none          |HRU number
!!    surf_bs(1,:)|mm H2O        |amount of surface runoff lagged over one
!!                               |day
!!    surfq(:)    |mm H2O        |surface runoff generated in HRU on the
!!                               |current day
!!    hhqday(:)   |mm H2O        |surface runoff generated in HRU on the
!!                               |current hour at current day  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bsprev      |mm H2O        |surface runoff lagged from prior day
!!    qday        |mm H2O        |surface runoff loading to main channel
!!                               |from HRU on current day
!!    surf_bs(1,:)|mm H2O        |amount of surface runoff lagged over one
!!                               |day
!!    hhqday(:)   |mm H2O        |surface runoff generated in HRU on the
!!                               |current hour at current day  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer :: j
      
      j = 0
      j = ihru

      bsprev = surf_bs(1,j)

      if (ievent < 3) then	
      
	surf_bs(1,j) = Max(1.e-6, surf_bs(1,j) + surfq(j))
      qday = surf_bs(1,j) * brt(j)
      surf_bs(1,j) = surf_bs(1,j) - qday
	else
		qday = 0.
		do ii =1,24
		  surf_bs(1,j) = Max(1.e-6, surf_bs(1,j) + hhqday(ii))
		  hhqday(ii) = surf_bs(1,j) * brt(j) / 24.
		  surf_bs(1,j) = surf_bs(1,j) - hhqday(ii)
		  qday = qday + hhqday(ii)
		end do
	end if
     
      return
      end
