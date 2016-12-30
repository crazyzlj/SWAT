      subroutine wattable
      
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
!!    watab       |mm            |water table based on 30 day antecedent
!!                               | climate (precip,et)
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
      wtab_mn(j) = 0.
      wtab_mx(j) = 2.5

      !! compute 30 day sums
      rfqeo_30d(nd_30,j) = precipday - qday - pet_day
      eo_30d(nd_30,j) = pet_day
      rfqeo_sum = 0.
      eo_sum = 0.

      do i30 = 1, 30
        rfqeo_sum = rfqeo_sum + rfqeo_30d(i30,j)
        eo_sum = eo_sum + eo_30d(i30,j)
      end do

      if (eo_sum > 1.e-4) then
        w2 = rfqeo_sum / eo_sum
      else
        w2 = 0.
      end if
      w1 = amin1 (0.1,abs(w2))
      if (w2 > 0.) then
        wtl = wtab_mn(j)
      else
        wtl = wtab_mx(j)
      end if
      if (wtab(j) < 1.e-6) wtab(j) = 0.0
      wtab(j) = wtab(j) - w1 * (wtab(j) - wtl)
      
!      write (444,1000) curyr,iida,j,wtab(j)
!1000   format (3i4,f8.2)

      return
      end