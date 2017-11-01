      subroutine wattable
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes water table depth using climate drivers.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    nd_30       |none          |day index of 30 days loop
!!    wtab        |mm            |water table based on 30 day antecedent
!!                               | climate (precip,et)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    added by ljzhu, 11/01/2017
!!    wtab_mn(:)  |mm H2O        |min
!!    wtab_mx(:)  |mm H2O        |max
!!    wtab        |mm            |water table based on 30 day antecedent
!!                               | climate (precip,et)
!! rfqeo_30d(30,:)|mm H2O        |remainer water in the last 30 days based on climate
!!                               |(precipday - qday - pet_day)
!!    eo_30d(30,:)|mm H2O        |pet in the last 30 days
!!    added by ljzhu, 11/01/2017
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    j1          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic:
!!    SWAT:

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      real :: wtl, w2, w1, rfqeo_sum, eo_sum
      integer :: j, j1, i30

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