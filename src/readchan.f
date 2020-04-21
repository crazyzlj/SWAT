      subroutine readchangepar(bbound,nopt,iiname,iinr, inrhru,imet,
     *      parname)
c
c   THIS SUBROUTINE READS AND PRINTS LIST OF PARAMTERS TO BE CHANGED

!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
      real*8 bbound(2,nopt)
!$$$$$$       integer iinr(nopt), iiname(nopt), isens, imet(nopt)
      integer iinr(nopt), iiname(nopt), imet(nopt)
!$$$$$$       integer inrhru(nopt,2000), iobj, nintval, iseed
      integer inrhru(nopt,2000)
      character*10 parname(nopt), pname
      common /iopar/ in,iprii
c
c
c  INITIALIZE I/O VARIABLES
c
      bbound=0.

!$$$$$$ 997   format(4i3, f8.3)      
      read(18010,*)
      


c
      write (iprii,*)'================================================='

      write(iprii,700)
  700 format(10x,'Parameter list of changepar.dat')
      write (iprii,*)'================================================='
      write(iprii,916)
  916 format(//,8x,'INITIAL PARAMETER VALUES AND PARAMETER BOUNDS',/,
     &       8x,45(1h=),//,2x,'PARAMETER',5x,'INITIAL VALUE',5x,
     &       'LOWER BOUND',5x,'UPPER BOUND',/,2x,9(1h-),5x,13(1h-),5x,
     &       11(1h-),5x,11(1h-))
c
c
c  READ THE INITIAL PARAMETER VALUES AND THE PARAMETER BOUNDS
      do iopt=1,nopt

      read(18010,830) bl1, bu2,in1, in2, in3, pname
             write(iprii,832) iopt,bl1, bu2,in1, in2, in3, pname
       bbound(1,iopt)=bl1
       bbound(2,iopt)=bu2
       iiname(iopt)=in1
       imet(iopt)=in2
       iinr(iopt)=in3
       parname(iopt)=pname
       if (in3.eq.0) go to 5566
       if (in3.gt.2000) go to 5566
       read(18010,831) (inrhru(iopt,ii), ii=1,in3)
5566   continue      
!$$$$$$ 111    format(2i5,a13)
  830 format(2f10.5,3i5, a10)
  832 format(i4,2f10.5,3i5, a10)
  831 format (50i5)
      end do
!$$$$$$ 1918   format(2f10.5)
c
C  END OF SUBROUTINE SENSIN
      return
      end
