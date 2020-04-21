      subroutine percmain
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine is the master soil percolation component.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icrk        |none          |crack flow code
!!                               |1 simulate crack flow in watershed
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    ihru        |none          |HRU number
!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc - wp)
!!    sol_nly(:)  |none          |number of layers in soil profile
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |the current day (less wp water)
!!    sol_ul(:,:) |mm H2O        |amount of water held in the soil layer at
!!                               |saturation
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    wtabelo     |mm            |water table based on depth from soil surface
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flat(:,:)   |mm H2O        |lateral flow storage array
!!    latlyr      |mm H2O        |lateral flow in soil layer for the day
!!    latq(:)     |mm H2O        |total lateral flow in soil profile for the 
!!                               |day in HRU
!!    lyrtile     |mm H2O        |drainage tile flow in soil layer for day
!!    qtile       |mm H2O        |drainage tile flow in soil profile for the day
!!    sepday      |mm H2O        |micropore percolation from soil layer
!!    sepbtm(:)   |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |the current day (less wp water)
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on current day
!!    sw_excess   |mm H2O        |amount of water in excess of field capacity
!!                               |stored in soil layer on the current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    j1          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: percmacro, percmicro

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, j1

      j = 0
      j = ihru

      !! initialize water entering first soil layer

      if (icrk == 1) then
        sepday = Max(0., inflpcp - voltot)
      else
        sepday = inflpcp
      end if

!!  add irrigation water
	if (aird(j)>0) then
	  j=j
	end if
      sepday = inflpcp + aird(j)
!! if unlimted, or groundwater source reset aird here (otherwise in virtual)
	if (irrsc(j) > 2)  aird(j) = 0.


      !! calculate crack flow 
      if (icrk == 1) then 
	    call percmacro
	    sepday = sepday - sepcrktot
	  endif

      do j1 = 1, sol_nly(j)
        !! add water moving into soil layer from overlying layer
        sol_st(j1,j) = sol_st(j1,j) + sepday
        
 	  !! septic tank inflow to biozone layer  J.Jeong
	  ! STE added to the biozone layer if soil temp is above zero. 
	  if(j1==i_sep(j).and.sol_tmp(j1,j) > 0. .and. isep_opt(j) /= 0) then
		sol_st(j1,j) = sol_st(j1,j) + qstemm(j)  ! in mm
        end if

       !! determine gravity drained water in layer
        sw_excess = 0.
        sw_excess = sol_st(j1,j) - sol_fc(j1,j)

        !! initialize variables for current layer
        sepday = 0.
        latlyr = 0.
        lyrtile = 0.
        lyrtilex = 0.

        if (sw_excess > 1.e-5) then
          !! calculate tile flow (lyrtile), lateral flow (latlyr) and
          !! percolation (sepday)
          call percmicro(j1)

          sol_st(j1,j) = sol_st(j1,j) - sepday - latlyr - lyrtile
          sol_st(j1,j) = Max(1.e-6,sol_st(j1,j))

          !! redistribute soil water if above field capacity (high water table)
          call sat_excess(j1)
!         sol_st(j1,j) = sol_st(j1,j) - lyrtilex
!         sol_st(j1,j) = Max(1.e-6,sol_st(j1,j))
        end if

        !! summary calculations
        if (j1 == sol_nly(j)) then
          sepbtm(j) = sepbtm(j) + sepday
        endif
        latq(j) = latq(j) + latlyr
        qtile = qtile + lyrtile
        flat(j1,j) = latlyr + lyrtile
        sol_prk(j1,j) = sol_prk(j1,j) + sepday
	  if (latq(j) < 1.e-6) latq(j) = 0.
        if (qtile < 1.e-6) qtile = 0.
        if (flat(j1,j) < 1.e-6) flat(j1,j) = 0.

      end do

      !! compute shallow water table depth and tile flow
      qtile = 0.
      wt_shall = 0.    !CB 8/24/09
      wt_shall = dep_imp(j)
      if (sol_tmp(2,j) > 0.) then
        por_air = 0.5
        d = dep_imp(j) - ddrain(j)
        if (sol_sw(j) > sol_sumfc(j)) then
          yy = sol_sumul(j) * por_air
          if (yy < 1.1 * sol_sumfc(j)) then
            yy = 1.1 * sol_sumfc(j)
          end if
          xx = (sol_sw(j) - sol_sumfc(j)) / (yy - sol_sumfc(j))
          if (xx > 1.) xx = 1.
          wt_shall = xx * dep_imp(j)
          if (ddrain(j) > 0.) then
            if (wt_shall < d) then
              qtile = 0.
            else
              dmod_m = wt_shall - d
              sw_excess = (dmod_m / wt_shall) * (sol_sw(j) -
     &                                             sol_sumfc(j))
              qtile = sw_excess * (1. - Exp(-24. / tdrain(j)))
            end if
          end if
        end if

         wtabelo = (dep_imp(j) - wt_shall) / 1000.
!        write (333,333) curyr,iida,j,wt_shall, wtabelo
!333     format(3i4,2f10.4)

        if (qtile > 0.) then
          !! update soil profile water after tile drainage
          sumqtile = qtile
          do j1 = 1, sol_nly(j)
            xx = sol_st(j1,j) - sol_fc(j1,j)
            if (xx > 0.) then
              if (xx > sumqtile) then
                sol_st(j1,j) = sol_st(j1,j) - sumqtile
                sumqtile = 0.
              else
                sumqtile = sumqtile - xx
                sol_st(j1,j) = sol_fc(j1,j)
              end if
            end if
          end do
          if (sumqtile > 0.) then
            qtile = qtile - sumqtile
            qtile = amax1(0., qtile)
          end if
        end if
      end if

      !! update soil profile water
      sol_sw(j) = 0.
      do j1 = 1, sol_nly(j)
        sol_sw(j) = sol_sw(j) + sol_st(j1,j)
      end do

      return
      end
