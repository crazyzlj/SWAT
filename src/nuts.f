      subroutine nuts(u1,u2,uu)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function calculates the plant stress factor caused by limited
!!    supply of nitrogen or phosphorus

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    u1          |kg/ha         |actual amount of element in plant
!!    u2          |kg/ha         |optimal amount of element in plant
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    uu          |none          |fraction of optimal plant growth achieved
!!                               |where reduction is caused by plant element
!!                               |deficiency
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      real, intent (in) :: u1, u2
      real, intent (out) :: uu

      uu = 0.

      uu = 200. * (u1 / (u2 + .0001) - .5)

      if (uu <= 0.) then
        uu = 0.
      else
        if (uu < 99.) then
          uu = uu / (uu + Exp(3.535 - .02597 * uu))
        else
          uu = 1.
        endif
      end if

      if (u2 <= 1.e-6) uu = 1.

      return
      end