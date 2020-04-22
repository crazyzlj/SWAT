      subroutine gcycl

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine initializes the random number seeds. If the user
!!    desires a different set of random numbers for each simulation run,
!!    the random number generator is used to reset the values of the 
!!    seeds.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    igen        |none          |random number generator code:
!!                               | 0: use default numbers
!!                               | 1: generate new numbers in every simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idg(:)      |none          |array location of random number seed used
!!                               |for a given process
!!    rndseed(:,:)|none          |random number seeds. The seeds in the array
!!                               |are used to generate random numbers for the
!!                               |following purposes
!!                               |(1) wet/dry day probability
!!                               |(2) solar radiation
!!                               |(3) precipitation
!!                               |(4) USLE rainfall erosion index
!!                               |(5) wind speed
!!                               |(6) 0.5 hr rainfall fraction
!!                               |(7) relative humidity
!!                               |(8) maximum temperature
!!                               |(9) minimum temperature
!!                               |(10) generate new random numbers
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |variable to hold calculated value
!!    j           |none          |counter
!!    k           |none          |counter, and variable
!!    rn          |none          |random number between 0.0 and 1.0
!!    rndseed10   |none          |seed for random number generator that is 
!!                               |used to reset other random number seeds
!!    xx          |none          |dummy variable to accept function value
!!                               |which is then discarded
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Aunif

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real*8 :: xx, rn
      integer :: ii, j, k, rndseed10
      
!!    initialize random number array locator
      idg = (/1,2,3,4,5,6,7,8,9/)

!!    initialize random number seeds
       

      do j = 1, mhru
        rndseed(1,j) = 748932582
        rndseed(2,j) = 1985072130
        rndseed(3,j) = 1631331038
        rndseed(4,j) = 67377721
        rndseed(5,j) = 366304404
        rndseed(6,j) = 1094585182
        rndseed(7,j) = 1767585417
        rndseed(8,j) = 1980520317
        rndseed(9,j) = 392682216
      end do
      rndseed10 = 64298628

      if (igen /= 0) then
        !! assign new random number seeds
        do j = 1, 9
           rn = 0.
           ii = 0
           rn = Aunif(rndseed10)
           ii = 100 * igen * rn
           do k = 1, ii
             xx = Aunif(rndseed10)
           end do  
           rndseed(j,1) = rndseed10
        end do
      
        !! shuffle seeds randomly (Bratley, Fox, Schrage, p34)
        do j = 9, 1, -1
          ii = 0
          rn = 0.
          ii = idg(j)
          rn = Aunif(rndseed10)
          k = j * rn + 1
          idg(j) = idg(k)
          idg(k) = ii
        end do
      end if

      !! assign half-hour maximum rainfall seed to second array location for use
      !! in sub-daily pcp generator
      do j = 1, mhru
        rndseed(10,j) = rndseed(idg(6),j)
      end do

      do j = 1, mhru
        rnd2(j) = Aunif(rndseed(idg(2),j))
        rnd3(j) = Aunif(rndseed(idg(3),j))
        rnd8(j) = Aunif(rndseed(idg(8),j))
        rnd9(j) = Aunif(rndseed(idg(9),j))
      end do

      return
      end