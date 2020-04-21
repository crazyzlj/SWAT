      subroutine parstt2(npt,nopt,x,xnstd,bound,gnrng,ipcnvg, bbound
     *	,bbound2)

c
c  SUBROUTINE CHECKING FOR PARAMETER CONVERGENCE
      implicit real*8 (a-h,o-z)
      real*8 x(npt,nopt),xmaxq(nopt),xminq(nopt)
      real*8 xmean(nopt),xnstd(nopt),bound(nopt)
      real*8 xnstd2(nopt), xmean2(nopt), bbound(2,nopt),bbound2(2,nopt)
      parameter (delta = 1.0d-20,peps=1.0d-3)
c
c  COMPUTE MAXIMUM, MINIMUM AND STANDARD DEVIATION OF PARAMETER VALUES
      gsum = 0.d0
      do k = 1, nopt
        xmaxq(k) = -1.0d+20
        xminq(k) = 1.0d+20
        xsum1 = 0.d0
        xsum2 = 0.d0
        do i = 1, npt
          xmaxq(k) = dmax1(x(i,k), xmaxq(k))
          xminq(k) = dmin1(x(i,k), xminq(k))
          xsum1 = xsum1 + x(i,k)
          xsum2 = xsum2 + x(i,k)*x(i,k)
        end do

        if (xmaxq(k).lt.xminq(k)) xmaxq(k)=xminq(k)
        xmean2(k) = xsum1 / dble(npt)
        xnstd2(k) = (xsum2 / dble(npt) - xmean2(k)*xmean2(k))
        if (xnstd2(k) .le. delta) xnstd2(k) = delta
        xnstd2(k) = dsqrt(xnstd2(k))
        xnstd2(k) = xnstd2(k) / bound(k)
	  xmean(k)=(bbound(2,k)-bbound(1,k))/2.
        xnstd(k)=0.5
        gsum = gsum + dlog( delta + (xmaxq(k)-xminq(k))/bound(k) )
c	bbound2(2,k)=min(xmaxq(k)+0.05/bound(k), bbound(2,k))
c	bbound2(1,k)=max(xminq(k)-0.05/bound(k),bbound(1,k)) 
      end do
      gnrng = dexp(gsum/dble(nopt))

c
c  CHECK IF NORMALIZED STANDARD DEVIATION OF PARAMETER IS <= eps
      ipcnvg = 0
      if (gnrng .le. peps) then
        ipcnvg = 1
      end if
c
c  END OF SUBROUTINE PARSTT
      return
      end
