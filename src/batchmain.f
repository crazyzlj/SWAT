      subroutine batchmain(nopt)
!     !  !         !         !         !         !         !         ! !
c	Main program to rerun the good parameter set

!!    iobj         |none          |number of objective functions
!!    obj(:)       |none          |values of the objective functions
!!    iclb         |none          |method to form GOC out of objective functions
!!    icalpar(:,:) |none          |objective functions codes of CALMET.DAT
!!    iitel        |none          |number of simulation run
!!    iinr(:)      !              |number of HRUs to change for parameter (:)
!!    inrhru(:,:)  |none          |list of HRU numbers to change  

c     AVG

c
c  THIS IS THE MAIN PROGRAM CALLING SUBROUTINES SENSIN AND RUNSENS



c  THIS IS THE MAIN PROGRAM CALLING SUBROUTINES SENSIN AND RUNSENS
!     !  !         !         !         !         !         !         ! !
      real*8 bbound(2,nopt), calw(100), sensw(nopt), xxo(nopt),obj(40)
      real*8 valmin(40), valmax(40)
      integer iinr(nopt), iiname(nopt), isens, imet(nopt)
!$$$$$$       integer inrhru(nopt,2000), iobj, nintval, iseed
      integer inrhru(nopt,2000), iobj
      integer icalpar(4,100), nopt, isenspar(4,100)
      common /iopar/ in,iprii
c       
	 character*4 fmnname(40), fmxname(40), tt
        data fmnname /'mn01','mn02','mn03','mn04','mn05','mn06','mn07',
     &'mn08','mn09','mn10','mn11','mn12','mn13','mn14','mn15','mn16',
     &'mn17','mn18','mn19','mn20','mn21','mn22','mn23',
     &'mn24','mn25','mn26','mn27','mn28','mn29','mn30','mn31','mn32',
     &'mn33','mn34','mn35','mn36','mn37','mn38','mn39','mn40'/
       data fmxname /'mx01','mx02','mx03','mx04','mx05','mx06','mx07',
     &'mx08','mx09','mx10','mx11','mx12','mx13','mx14','mx15','mx16',
     &'mx17','mx18','mx19','mx20','mx21','mx22','mx23',
     &'mx24','mx25','mx26','mx27','mx28','mx29','mx30','mx31','mx32',
     &'mx33','mx34','mx35','mx36','mx37','mx38','mx39','mx40'/

	

      open(18012, file='batchobjf.out')
      open(18013, file='batchpar.out')
      open(18015, file='goodpar.out')
	open(18014, file='batchrespons.out')
      open(18016, file='objmet.dat')
	open(18017, file='responsmet.dat')
	open (18118, file='minval.out')
	open (18119, file='maxval.out')
			
	call batchin(bbound,nopt,
     & iiname,iinr, inrhru,iobj,
     & calw, icalpar,isenspar,sensw,isens, imet)
	iitel=0
	ipr=0
5	read(18015,8013, end =4) i, (xxo(ii), ii=1,nopt)

8013    format(i5, 100e12.5)
	iitel=iitel+1
	icd=2
      call rerun(xxo, nopt,iiname, inrhru, iinr, iitel, imet,ipr)
      call objfunctn(obj, iobj, icalpar, iitel,icd)
	call response(sensw, isens, isenspar, iitel)
	go to 5
4	continue	

	do ii=1,iobj
	tt=fmnname(ii)
	open(7700+ii,file=tt) 
	tt=fmxname(ii)
	open(7800+ii,file=tt) 
	end do
8	do ii=1,iobj
	read(7700+ii,5000,end=7) iyr, iday, ihr, valmin(ii)
	read(7800+ii,5000,end=7) iyr, iday, ihr, valmax(ii)
5000  format(1x,i4,2x,i3,1x,i2,22e11.3)	
	end do
	write(18118,5001) iyr, iday, ihr,(valmin(ii), ii=1,iobj)
	write(18119,5001)iyr, iday, ihr, (valmax(ii), ii=1,iobj)
5001	format(1x,i4,2x,i3,1x,i2,40e11.3)
	go to 8
7	continue
	do ii=1,iobj
	close(7700+ii) 
	close(7800+ii) 
	end do
	close(18010)
      close(18011)
      close(18012)
      close(18013)
	close(18014)
	close(18015)
      close(18016)
	close(18017)
      close(18118)
	close(18119)

c  END OF THE BATCH ROUTINE
      return
	end
