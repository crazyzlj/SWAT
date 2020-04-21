      subroutine weatgn(j)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates weather parameters used to simulate the impact
!!    of precipitation on the other climatic processes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    rnd2(:)     |none          |random number between 0.0 and 1.0
!!    rnd8(:)     |none          |random number between 0.0 and 1.0
!!    rnd9(:)     |none          |random number between 0.0 and 1.0
!!    rndseed(:,:)|none          |random number generator seeds
!!    wgnold(:,:) |none          |previous value of wgncur(:)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rnd2(:)     |none          |random number between 0.0 and 1.0
!!    rnd8(:)     |none          |random number between 0.0 and 1.0
!!    rnd9(:)     |none          |random number between 0.0 and 1.0
!!    wgncur(:,:) |none          |parameter to predict the impact of precip on
!!                               |other weather attributes
!!    wgnold(:,:) |none          |previous value of wgncur(:,:)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    a(:,:)      |none          |3 x 3 matrix whose elements are defined such
!!                               |that the new sequences of max temp, min temp, 
!!                               |and radiation have the desired serial-
!!                               |correlation and cross-correlation coefficients
!!    b(:,:)      |none          |3 x 3 matrix whose elements are defined such
!!                               |that the new sequences of max temp, min temp, 
!!                               |and radiation have the desired serial-
!!                               |correlation and cross-correlation coefficients
!!    e(:)        |none          |3 x 1 matrix of independent random components
!!    j           |none          |HRU number
!!    l           |none          |counter
!!    n           |none          |counter
!!    v2          |none          |random number between 0.0 and 1.0
!!    xx(:)       |none          |variable to hold calculation value
!!    zshape(:)   |none          |array shape parameters
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Aunif, Dstn1

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: j
      integer, dimension (2) :: zshape
      integer :: n, l
      real, dimension (3,3) :: a, b
      real, dimension (3) :: xx, e
      real :: v2
 
      zshape = (/3, 3/)
      a = Reshape((/.567, .253, -.006, .086, .504, -.039, -.002, -.050, &
     &             .244/), zshape)
      b = Reshape((/.781, .328, .238, 0., .637, -.341, 0., 0., .873/),  &
     &             zshape)
      xx = 0.
      e = 0.

!!    set random number array values
        v2 = 0.
        v2 = Aunif(rndseed(idg(8),j))
        e(1) = Dstn1(rnd8(j),v2)    !! for max temp
        rnd8(j) = v2
        v2 = 0.
        v2 = Aunif(rndseed(idg(9),j))
        e(2) = Dstn1(rnd9(j),v2)    !! for min temp
        rnd9(j) = v2
        v2 = 0.
        v2 = Aunif(rndseed(idg(2),j))
        e(3) = Dstn1(rnd2(j),v2)    !! for radiation
        rnd2(j) = v2

      do n = 1, 3
        wgncur(n,j) = 0.
        do l = 1, 3
          wgncur(n,j) = wgncur(n,j) + b(n,l) * e(l)
          xx(n) = xx(n) + a(n,l) * wgnold(l,j)
        end do
      end do

      do n = 1, 3
        wgncur(n,j) = wgncur(n,j) + xx(n)
        wgnold(n,j) = wgncur(n,j)
      end do

      return
      end
