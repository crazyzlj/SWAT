	subroutine getpnt2(nopt,x,bl,bu,bl2,bu2)
c
c     This subroutine generates a new point within feasible region
c
c     x(.) = new point
c     xi(.) = focal point
c     bl(.) = lower bound
c     bu(.) = upper bound
c     std(.) = standard deviation of probability distribution
c     idistq = probability flag
c           = 1 - uniform distribution
c           = 2 - Gaussian distribution
c
      implicit real*8 (a-h,o-z)
!$$$$$$       integer nopt, iseed
      integer nopt
      real*8 x(nopt),bl(nopt),bu(nopt)
!$$$$$$       real*8 x(nopt),bl(nopt),bu(nopt),std(nopt),xi(nopt)
      real*8 bl2(nopt), bu2(nopt)
      do i = 1, nopt
	x(i)=max(x(i),bl(i))
	x(i)=min(x(i),bu(i))
	end do
      return
      end
