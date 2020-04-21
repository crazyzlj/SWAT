      subroutine sort1(n,raq)

c
c
c  SORTING SUBROUTINE ADAPTED FROM "NUMERicalm RECIPES"
c  BY W.H. PRESS ET AL., pp. 231
c
c  LIST OF VARIABLES
c     raq(.) = integer array to be sorted
c
      implicit real*8 (a-h,o-z)
      dimension raq(n)
c
      integer raq, rra
c
      l = (n / 2) + 1
      ir = n
   10 continue
      if (l .gt. 1) then
      l = l - 1
      rra = raq(l)
      else
      rra = raq(ir)
      raq(ir) = raq(1)
      ir = ir - 1
      if (ir .eq. 1) then
      raq(1) = rra
      return
      end if
      end if
      i = l
      j = l + l
   20 if (j .le. ir) then
      if (j .lt. ir) then
      if (raq(j) .lt. raq(j + 1)) j = j + 1
      end if
      if (rra .lt. raq(j)) then
      raq(i) = raq(j)
      i = j
      j = j + j
      else
      j = ir + 1
      end if
      goto 20
      end if
      raq(i) = rra
      goto 10
c
c  END OF SUBROUTINE SORT1
      end
