      subroutine getpnt(nopt,idistq,iseedval,x,bl,bu,std,xi)
!$$$$$$       subroutine getpnt(nopt,idistq,iseed,x,bl,bu,std,xi)
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
!$$$$$$       integer nopt, iseed, idistq
      integer nopt, iseedval, idistq
      real*8 x(nopt),bl(nopt),bu(nopt),std(nopt),xi(nopt)
      do 1921 i = 1, nopt



 1921 continue

c
    1 do j=1, nopt
    2   if (idistq .eq. 1) rand = ran1(iseedval)
        if (idistq .eq. 2) rand = gasdev(iseedval)
        x(j) = xi(j) + std(j) * rand * (bu(j) - bl(j))

c
c     Check explicit constraints
c        
        call chkcst(1,x(j),bl(j),bu(j),ibound)
        if (ibound .ge. 1) go to 2


      end do
c
c     Check implicit constraints
c      
      call chkcst(nopt,x,bl,bu,ibound)
      if (ibound .ge. 1) go to 1
c
      return
      end
