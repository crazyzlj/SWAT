      real*8 function gasdev(idum)

c
c
c  THIS SUBROUTINE IS FROM "NUMERicalm RECIPES" BY PRESS ET AL.
      implicit real*8 (a-h,o-z)
      common /gasblk/ iset
      data iset / 0 /
      if (iset .eq. 0) then
    1 v1 = (2. * ran1(idum)) - 1.
      v2 = (2. * ran1(idum)) - 1.
      rqq = (v1 ** 2) + (v2 ** 2)
      if (rqq .ge. 1.) goto 1
      fac = sqrt(- ((2. * log(rqq)) / rqq))
      gset = v1 * fac
      gasdev = v2 * fac
      iset = 1
      else
      gasdev = gset
      iset = 0
      end if
c
c  END OF SUBROUTINE GASDEV
      return
      end
