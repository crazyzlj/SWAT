!$$$$$$       subroutine oat(x,m,ip,bbound,dt,iseed)
	subroutine oat(x,m,ip,bbound,dt)
c	This subroutine perfoms One-factor-At-a-Time desing

	
	real*8 X(m,ip), yy(ip+1, ip),dt, bbound(2,200), yp(ip)
!$$$$$$       integer isample(ip), iseed, zero
	integer isample(ip), zero
      real ranval
	itel=0
	zero=0
	do nloop=1, m
	write (18019,*) 'loop', nloop

	do ii=1,ip
	yy(1,ii) = x(nloop,ii)
	end do
	jj=1
	write(18019,1) jj,zero, (yy(1,kk), kk=1,ip)
	do ii=1,ip
	yp(ii)= bbound(1,ii)+(bbound(2,ii)-bbound(1,ii))*yy(1,ii)
	end do
	itel=itel+1
	write(18013,8013) itel,(yp(kk), kk=1,ip)
!$$$$$$       call sample1(isample,ip,iseed)
	call sample1(isample,ip)
	
	do jj=2,ip+1
	do ii=1,ip
	yy(jj,ii)=yy(jj-1,ii)
	end do
      call random_number(ranval)
!$$$$$$       if (ran(iseed).gt.0.5) then
      if(ranval.gt.0.5) then
	t=1.
	else
	t=-1.
	end if
	zz=yy(jj,isample(jj-1))+dt*t
	if (zz.lt.0.or.zz.gt.1) zz=yy(jj,isample(jj-1)) - dt*t
	yy(jj,isample(jj-1))=zz
	write(18019,1) jj,isample(jj-1), (yy(jj,kk), kk=1,ip)
	do kk=1,ip
	  yp(kk)= bbound(1,kk)+(bbound(2,kk)-bbound(1,kk))*yy(jj,kk)
	end do
	itel=itel+1
	write(18013,8013) itel, (yp(kk), kk=1,ip)
	end do
	end do

1	format(2i4,200e12.5)
8013   format(i5, 100e12.5)
	return
	end 
