!$$$$$$       subroutine  sample(ip,m,iseed, bbound,dt)
	subroutine  sample(ip,m,bbound,dt)



c	Latin Hypercube sampling program
c	Written by Ann van Griensven at UC Riverside
c
c
c	ip = number of parameters
c     m  = number of intervals
c	X= matrix with ip parameter values for m samples
c	IX stores positions within hypercube intervals	
c
c
c
	real*8 x(m,ip), bbound(2,200),dt
      real ranval
!$$$$$$       integer ix(m,ip), ip, m, iseed
!$$$$$$       integer ix(m,ip), ip, m
	integer ip, m
	integer isample(m)
	write (*,*) 'starting sampling	 ....'

	do ii=1,ip
!$$$$$$       call sample1(isample,m,iseed) 
	call sample1(isample,m)	
	do jj=1,m
      call random_number(ranval)
!$$$$$$       ix(jj,ii)=isample(jj)
!$$$$$$       x(jj,ii)=(isample(jj)-1.+ran(iseed))/m
	x(jj,ii)=(isample(jj)-1.+ranval)/m
	end do
	end do
	do jj=1,m
	write(18018,1) isample(jj),(x(jj,kk), kk=1,ip)
1	format(i4,100f12.5)
	end do

!$$$$$$       call oat(x,m,ip, bbound,dt,iseed)
	call oat(x,m,ip, bbound,dt)
	return
	end
