      function atri(at1,at2,at3,at4i) result (r_atri)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function generates a random number from a triangular distribution
!!    given X axis points at start, end, and peak Y value

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    at1         |none          |lower limit for distribution
!!    at2         |none          |monthly mean for distribution
!!    at3         |none          |upper limit for distribution
!!    at4i        |none          |random number seed 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    atri        |none          |daily value generated for distribution
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    amn         |              |
!!    b1          |              |
!!    b2          |              |
!!    rn          |none          |random number between 0.0 and 1.0
!!    u3          |              |
!!    x1          |              |
!!    xx          |              |
!!    y           |              |
!!    yy          |              |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt
!!    SWAT: Aunif

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm, except_this_one => atri

      real*8, intent (in) :: at1, at2, at3
      integer, intent (in out) :: at4i
      real*8 :: u3, rn, y, b1, b2, x1, xx, yy, amn
      real*8 :: r_atri

      u3 = 0.
      rn = 0.
      y = 0.
      b1 = 0.
      b2 = 0.
      x1 = 0.
      xx = 0.
      yy = 0.
      amn = 0.

      u3 = at2 - at1
      rn = Aunif(at4i)
      y = 2.0 / (at3 - at1)
      b2 = at3 - at2
      b1 = rn / y
      x1 = y * u3 / 2.0

      if (rn <= x1) then
        xx = 2.0 * b1 * u3
        if (xx <= 0.) then
          yy = 0.
        else
          yy = Sqrt(xx)
        end if
        r_atri = yy + at1
      else
        xx = b2 * b2 - 2.0 * b2 * (b1 - 0.5 * u3)
        if (xx <= 0.) then
          yy = 0.
        else
          yy = Sqrt(xx)
        end if
        r_atri = at3 - yy
      end if

      amn = (at3 + at2 + at1) / 3.0
      r_atri = r_atri * at2 / amn

      if (r_atri >= 1.0) r_atri = 0.99
      if (r_atri <= 0.0) r_atri = 0.001

      return
      end