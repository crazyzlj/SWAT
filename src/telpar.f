      subroutine telpar(nopt)

c
c	WRITTEN BY ANN VAN GRIENSVEN - UNIVERSITY OF CALIFORNIA, 2003
c
c

!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)

      integer it(2000)

      common /iopar/ in,iprii

c

c
c  INITIALIZE I/O VARIABLES

	read(18010,*)

c
      bbound=0.



c  READ THE INITIAL PARAMETER VALUES AND THE PARAMETER BOUNDS
      iopt = 0
  820 iopt = iopt + 1

      read(18010,830,end=840) bl1, bu2,in1, in2, in3
       if (in1.eq.0) go to 840 
       if (in3.eq.0) go to 5566
       if (in3.gt.2000) go to 5566
       read(18010,831,end=840) (it(ii), ii=1,in3)
5566   continue	
111    format(2i5,a13)
  830 format(2f10.5,3i5)
  832 format(i4,2f10.5,3i5)
  831 format (50i5)
      go to 820
840   continue
      nopt=iopt-1
1918   format(2f10.5)

      return
      end
