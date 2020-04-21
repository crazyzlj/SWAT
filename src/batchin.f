      subroutine batchin(bbound,nopt,
     & iiname,iinr, inrhru,iobj,
     & calw, icalpar,isenspar,sensw,isens, imet)
c
c   THIS SUBROUTINE READS AND PRINTS THE INPUT VARIABLES FOR
c   SHUFFLED COMPLEX EVOLUTION METHOD FOR GLOBAL OPTIMIZATION
c     -- Version 2.2
c
c   WRITTEN BY QINGYUN DUAN - UNIVERSITY OF ARIZONA, APRIL 1992
c
c

!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
      real*8 bbound(2,nopt), calw(100), sensw(100)
      integer iinr(nopt), iiname(nopt), isens, imet(nopt)
!$$$$$$       integer inrhru(nopt,2000), iobj, nintval, iseed
      integer inrhru(nopt,2000), iobj
      integer icalpar(4,100), nopt, isenspar(4,100)
      common /iopar/ in,iprii

c
      write (*,*) ' ENTER THE BATCH RUN SUBROUTINE --- '
c
c
c  INITIALIZE I/O VARIABLES



c
      bbound=0.

      write(18011,700)
  700 format(10x,'BATCH RUNS ',
     &       /,10x,46(1h=))


      write (*,*) ' READING CHANGEPAR.DAT '

        do mm=1,100
!     !  !         !         !         !         !         !         ! !
      read(18017,997, end=998) isenspar(1,mm),isenspar(2,mm),
     *   isenspar(3,mm),isenspar(4,mm),sensw(mm) 
        if (isenspar(1,mm).le.0) go to 998
        end do
998     continue
        isens=mm-1
        write(18011,*)" output codes are : "
        do mm = 1,isens
       write (18011,997)isenspar(1,mm), isenspar(2,mm),
     * isenspar(3,mm),isenspar(4,mm),sensw(mm)
       end do

c
c  READ THE CALIBRATION CONTROL PARAMETERS

        do mm=1,40
!     !  !         !         !         !         !         !         ! !
      read(18016,997, end=996) icalpar(1,mm),icalpar(2,mm),icalpar(3,mm)
     *   ,icalpar(4,mm),calw(mm) 
        if (calw(mm).le.0.) calw(mm)=1.
        if (icalpar(1,mm).le.0) go to 996
        end do
996     continue
        iobj=mm-1
	
        write(18011,*) " objective codes are : "
        do mm = 1,iobj
       write (18011,997)icalpar(1,mm), icalpar(2,mm),icalpar(3,mm),
     * icalpar(4,mm),calw(mm)
       end do

997   format(4i3, f8.3)	
	read(18010,*)
	
c
c
c  READ THE INITIAL PARAMETER VALUES AND THE PARAMETER BOUNDS
      iopt = 0
  820 iopt = iopt + 1

      read(18010,830,end=840) bl1, bu2,in1, in2, in3
       bbound(1,iopt)=bl1
       bbound(2,iopt)=bu2
       iiname(iopt)=in1
	 imet(iopt)=in2
       iinr(iopt)=in3
       if (in3.eq.0) go to 5566
       if (in3.gt.2000) go to 5566
       read(18010,831,end=840) (inrhru(iopt,ii), ii=1,in3)
5566   continue	
!$$$$$$ 111    format(2i5,a13)
  830 format(2f10.5,3i5)
!$$$$$$   832 format(i4,2f10.5,3i5)
  831 format (50i5)
      go to 820
840   continue
      nopt=iopt-1
      nopt=min(nopt,200)
!$$$$$$ 1918   format(2f10.5)

      return
      end

