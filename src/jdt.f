      function jdt(numdays,i,m)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the julian date given the month and
!!    the day of the month

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    numdays(:)  |julian date   |julian date for last day of preceding
!!                               |month (where the array location is the
!!                               |number of the month). The dates are for
!!                               |leap years (numdays=ndays)
!!    m           |none          |month
!!    i           |none          |day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jdt         |julian date   |julian date
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      integer, intent (in), dimension (13) :: numdays
      integer, intent (in) :: m, i
      integer :: jdt
     
      jdt = 0

      if (m /= 0) then
        if (m <= 2) then
          jdt = numdays(m) + i
        else
          jdt = numdays(m) - 1 + i
        end if
      end if

      return
      end