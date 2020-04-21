      subroutine chkcst(nopt,xi,bl,bu,ibound)
c
c     This subroutine check if the trial point satisfies all
c     constraints.
c
c     ibound - violation indicator
c            = -1 initial value
c            = 0  no violation
c            = 1  violation
c     nopt = number of optimizing variables
c     ii = the ii'th variable of the arrays x, bl, and bu
c
      implicit real*8 (a-h,o-z)
      dimension xi(nopt),bl(nopt),bu(nopt)
c
      ibound = -1
c
c     Check if explicit constraints are violated
c
      do ii=1, nopt
        if (xi(ii) .lt. bl(ii) .or. xi(ii) .gt. bu(ii)) go to 10
      end do
      if (nopt .eq. 1) go to 9
c
c     Check if implicit constraints are violated
c     (no implicit constraints for this function)
c
c     No constraints are violated
c
    9 ibound = 0
      return
c
c     At least one of the constraints are violated
c
   10 ibound = 1
      return
      end
