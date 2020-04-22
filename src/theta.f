      real*8 function theta(r20,thk,tmp) result (r_theta)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function corrects rate constants for temperature
!!    Equation is III-52 from QUAL2E

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    r20         |1/day         |value of the reaction rate coefficient at
!!                               |the standard temperature (20 degrees C)
!!    thk         |none          |temperature adjustment factor (empirical
!!                               |constant for each reaction coefficient)
!!    tmp         |deg C         |temperature on current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    theta       |1/day         |value of the reaction rate coefficient at
!!                               |the local temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      real*8, intent (in) :: r20, thk, tmp

      r_theta = 0.
      r_theta = r20 * thk ** (tmp - 20.)

      return
      end