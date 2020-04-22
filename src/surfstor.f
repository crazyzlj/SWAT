      subroutine surfstor

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine stores and lags sediment and nutrients in surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactrolp      |# colonies/ha|less persistent bacteria transported to main
!!                                |channel with surface runoff
!!    bactrop       |# colonies/ha|persistent bacteria transported to main
!!                                |channel with surface runoff
!!    bactsedlp     |# colonies/ha|less persistent bacteria transported with
!!                                |sediment in surface runoff
!!    bactsedp      |# colonies/ha|persistent bacteria transported with
!!                                |sediment in surface runoff
!!    brt(:)        |none         |fraction of surface runoff that takes
!!                                |one day or less to reach the subbasin
!!                                |outlet
!!    hrupest(:)    |none         |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    ihru          |none         |HRU number
!!    npmx          |none         |number of different pesticides used in
!!                                |the simulation
!!    pst_lag(:,1,:)|kg pst/ha    |amount of soluble pesticide in surface runoff
!!                                |lagged
!!    pst_lag(:,2,:)|kg pst/ha    |amount of sorbed pesticide in surface runoff
!!                                |lagged
!!    pst_sed(:,:)  |kg/ha        |pesticide loading from HRU sorbed onto
!!                                |sediment
!!    pst_surq(:,:) |kg/ha        |amount of pesticide type lost in surface
!!                                |runoff on current day in HRU
!!    sedminpa(:)   |kg P/ha      |amount of active mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha      |amount of stable mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedorgn(:)    |kg N/ha      |amount of organic nitrogen in surface runoff
!!                                |in HRU for the day
!!    sedorgp(:)    |kg P/ha      |amount of organic phosphorus in surface
!!                                |runoff in HRU for the day
!!    sedyld(:)     |metric tons  |daily soil loss caused by water erosion
!!    surf_bs(2,:)  |metric tons  |amount of sediment yield lagged over one
!!                                |day
!!    surf_bs(3,:)  |kg N/ha      |amount of organic nitrogen loading lagged
!!                                |over one day
!!    surf_bs(4,:)  |kg P/ha      |amount of organic phosphorus loading lagged
!!                                |over one day
!!    surf_bs(5,:)  |kg N/ha      |amount of nitrate loading in surface runoff
!!                                |lagged over one day
!!    surf_bs(6,:)  |kg P/ha      |amount of soluble phosphorus loading lagged
!!                                |over one day
!!    surf_bs(7,:)  |kg P/ha      |amount of active mineral phosphorus loading 
!!                                |lagged over one day
!!    surf_bs(8,:)  |kg P/ha      |amount of stable mineral phosphorus loading 
!!                                |lagged over one day
!!    surf_bs(9,:)  |# colonies/ha|amount of less persistent bacteria in 
!!                                |solution lagged over one day
!!    surf_bs(10,:) |# colonies/ha|amount of persistent bacteria in 
!!                                |solution lagged over one day
!!    surf_bs(11,:) |# colonies/ha|amount of less persistent bacteria
!!                                |sorbed lagged over one day
!!    surf_bs(12,:) |# colonies/ha|amount of persistent bacteria
!!                                |sorbed lagged over one day
!!    surqno3(:)    |kg N/ha      |amount of NO3-N in surface runoff in HRU for
!!                                |the day
!!    surqsolp(:)   |kg P/ha      |amount of soluble phosphorus in surface
!!                                |runoff in HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactrolp      |# colonies/ha|less persistent bacteria transported to main
!!                                |channel with surface runoff
!!    bactrop       |# colonies/ha|persistent bacteria transported to main
!!                                |channel with surface runoff
!!    bactsedlp     |# colonies/ha|less persistent bacteria transported with
!!                                |sediment in surface runoff
!!    bactsedp      |# colonies/ha|persistent bacteria transported with
!!                                |sediment in surface runoff
!!    pst_lag(:,1,:)|kg pst/ha    |amount of soluble pesticide in surface runoff
!!                                |lagged
!!    pst_lag(:,2,:)|kg pst/ha    |amount of sorbed pesticide in surface runoff
!!                                |lagged
!!    pst_sed(:,:)  |kg/ha        |pesticide loading from HRU sorbed onto
!!                                |sediment
!!    pst_surq(:,:) |kg/ha        |amount of pesticide type lost in surface
!!                                |runoff on current day in HRU
!!    sedminpa(:)   |kg P/ha      |amount of active mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha      |amount of stable mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedorgn(:)    |kg N/ha      |amount of organic nitrogen in surface runoff
!!                                |in HRU for the day
!!    sedorgp(:)    |kg P/ha      |amount of organic phosphorus in surface
!!                                |runoff in HRU for the day
!!    sedyld(:)     |metric tons  |daily soil loss caused by water erosion
!!    surf_bs(2,:)  |metric tons  |amount of sediment yield lagged over one
!!                                |day
!!    surf_bs(3,:)  |kg N/ha      |amount of organic nitrogen loading lagged
!!                                |over one day
!!    surf_bs(4,:)  |kg P/ha      |amount of organic phosphorus loading lagged
!!                                |over one day
!!    surf_bs(5,:)  |kg N/ha      |amount of nitrate loading in surface runoff
!!                                |lagged over one day
!!    surf_bs(6,:)  |kg P/ha      |amount of soluble phosphorus loading lagged
!!                                |over one day
!!    surf_bs(7,:)  |kg P/ha      |amount of active mineral phosphorus loading 
!!                                |lagged over one day
!!    surf_bs(8,:)  |kg P/ha      |amount of stable mineral phosphorus loading 
!!                                |lagged over one day
!!    surf_bs(9,:)  |# colonies/ha|amount of less persistent bacteria in 
!!                                |solution lagged over one day
!!    surf_bs(10,:) |# colonies/ha|amount of persistent bacteria in 
!!                                |solution lagged over one day
!!    surf_bs(11,:) |# colonies/ha|amount of less persistent bacteria
!!                                |sorbed lagged over one day
!!    surf_bs(12,:) |# colonies/ha|amount of persistent bacteria
!!                                |sorbed lagged over one day
!!    surqno3(:)    |kg N/ha      |amount of NO3-N in surface runoff in HRU for
!!                                |the day
!!    surqsolp(:)   |kg P/ha      |amount of soluble phosphorus in surface
!!                                |runoff in HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, k
     
      j = 0
      j = ihru

      if (ievent == 0) then
         surf_bs(2,j) = Max(1.e-9, surf_bs(2,j) + sedyld(j))
         sedyld(j) = surf_bs(2,j) * brt(j)
         surf_bs(2,j) = surf_bs(2,j) - sedyld(j)

      else !subdaily time steps, Jaehak Jeong 2011
      	sedprev = hhsurf_bs(2,j,nstep)

	   do k=1,nstep

	!! Left-over (previous timestep) + inflow (current  timestep)
          hhsurf_bs(2,j,k) = Max(0., sedprev + hhsedy(j,k))
	
	!! new estimation of sediment reaching the main channel
          hhsedy(j,k) = hhsurf_bs(2,j,k) * brt(j)! tons
 	    hhsurf_bs(2,j,k) = hhsurf_bs(2,j,k) - hhsedy(j,k)
	  
	!! lagged at the end of time step  
	    sedprev = hhsurf_bs(2,j,k)
      surf_bs(2,j) = Max(1.e-9, surf_bs(2,j) + sedyld(j))

	   end do

	!! daily total sediment yield from the HRU
	   sedyld(j) = sum(hhsedy(j,:))
      endif
      
      surf_bs(13,j) = Max(1.e-6, surf_bs(13,j) + sanyld(j))
      surf_bs(14,j) = Max(1.e-6, surf_bs(14,j) + silyld(j))
      surf_bs(15,j) = Max(1.e-6, surf_bs(15,j) + clayld(j))
      surf_bs(16,j) = Max(1.e-6, surf_bs(16,j) + sagyld(j))
      surf_bs(17,j) = Max(1.e-6, surf_bs(17,j) + lagyld(j))

      surf_bs(3,j) = Max(1.e-9, surf_bs(3,j) + sedorgn(j))
      surf_bs(4,j) = Max(1.e-9, surf_bs(4,j) + sedorgp(j))
      surf_bs(5,j) = Max(1.e-9, surf_bs(5,j) + surqno3(j))
      surf_bs(6,j) = Max(1.e-9, surf_bs(6,j) + surqsolp(j))
      surf_bs(7,j) = Max(1.e-9, surf_bs(7,j) + sedminpa(j))
      surf_bs(8,j) = Max(1.e-9, surf_bs(8,j) + sedminps(j))
      surf_bs(9,j) = Max(0., surf_bs(9,j) + bactrolp)
      surf_bs(10,j) = Max(0., surf_bs(10,j) + bactrop)
      surf_bs(11,j) = Max(0., surf_bs(11,j) + bactsedlp)
      surf_bs(12,j) = Max(0., surf_bs(12,j) + bactsedp)
      if (hrupest(j) == 1) then
        do k = 1, npmx
          !MFW, 3/15/12: Modified to account for decay during lag
          !pst_lag(k,1,j) = pst_lag(k,1,j) + pst_surq(k,j)
          pst_lag(k,1,j) = (pst_lag(k,1,j) * EXP(-1. *                  
     &                      chpst_rea(inum1))) + pst_surq(k,j)
          if (pst_lag(k,1,j) < 1.e-10) pst_lag(k,1,j) = 0.0
          !pst_lag(k,2,j) = pst_lag(k,2,j) + pst_sed(k,j)
          pst_lag(k,2,j) = (pst_lag(k,2,j) * EXP(-1. *                  
     &                      sedpst_rea(inum1))) + pst_sed(k,j)
          if (pst_lag(k,2,j) < 1.e-10) pst_lag(k,2,j) = 0.0
        end do
      end if

 !!     sedyld(j) = surf_bs(2,j) * brt(j)    <----line of code in x 2.  fixes sedyld low prob

      sanyld(j) = surf_bs(13,j) * brt(j)
      silyld(j) = surf_bs(14,j) * brt(j)
      clayld(j) = surf_bs(15,j) * brt(j)
      sagyld(j) = surf_bs(16,j) * brt(j)
      lagyld(j) = surf_bs(17,j) * brt(j)

      sedorgn(j) = surf_bs(3,j) * brt(j)
      sedorgp(j) = surf_bs(4,j) * brt(j)
      surqno3(j) = surf_bs(5,j) * brt(j)
      surqsolp(j) = surf_bs(6,j) * brt(j)
      sedminpa(j) = surf_bs(7,j) * brt(j)
      sedminps(j) = surf_bs(8,j) * brt(j)
      bactrolp = Max(0.,bactrolp)
      bactrop = Max(0.,bactrop)
      bactsedlp = Max(0.,bactsedlp)
      bactsedp = Max(0.,bactsedp)
      if (hrupest(j) == 1) then
        do k = 1, npmx
          pst_surq(k,j) = pst_lag(k,1,j) * brt(j)
          pst_sed(k,j) = pst_lag(k,2,j) * brt(j)
        end do
      end if
     
      surf_bs(2,j) = surf_bs(2,j) - sedyld(j)

      surf_bs(13,j) = surf_bs(13,j) - sanyld(j)
      surf_bs(14,j) = surf_bs(14,j) - silyld(j)
      surf_bs(15,j) = surf_bs(15,j) - clayld(j)
      surf_bs(16,j) = surf_bs(16,j) - sagyld(j)
      surf_bs(17,j) = surf_bs(17,j) - lagyld(j)

      surf_bs(3,j) = surf_bs(3,j) - sedorgn(j)
      surf_bs(4,j) = surf_bs(4,j) - sedorgp(j)
      surf_bs(5,j) = surf_bs(5,j) - surqno3(j)
      surf_bs(6,j) = surf_bs(6,j) - surqsolp(j)
      surf_bs(7,j) = surf_bs(7,j) - sedminpa(j)
      surf_bs(8,j) = surf_bs(8,j) - sedminps(j)
      surf_bs(9,j) = surf_bs(9,j) - bactrolp
      surf_bs(10,j) = surf_bs(10,j) - bactrop
      surf_bs(11,j) = surf_bs(11,j) - bactsedlp
      surf_bs(12,j) = surf_bs(12,j) - bactsedp
      if (hrupest(j) == 1) then
        do k = 1, npmx
          pst_lag(k,1,j) = pst_lag(k,1,j) - pst_surq(k,j)
          pst_lag(k,2,j) = pst_lag(k,2,j) - pst_sed(k,j)
        end do
      end if

      return
      end