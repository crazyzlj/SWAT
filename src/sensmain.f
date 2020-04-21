      subroutine sensmain(nopt,iobj,isens)
!     !  !         !         !         !         !         !         ! !
c	Main program for the sensitivity analysis

!!    iobj         |none          |number of objective functions
!!    obj(:)       |none          |values of the objective functions
!!    iclb         |none          |method to form GOC out of objective functions
!!    icalpar(:,:) |none          |objective functions codes of CALMET.DAT
!!    iitel        |none          |number of simulation run
!!    iinr(:)      !              |number of HRUs to change for parameter (:)
!!    inrhru(:,:)  |none          |list of HRU numbers to change  

c     AVG

!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
	real*8 bbound(2,nopt), calw(iobj), sensw(isens),dt, xxo(nopt)
	real*8 obj(iobj)
!$$$$$$       integer iinr(nopt), iiname(nopt), isens, iobj, iitel,m
      integer iinr(nopt), iiname(nopt), isens, iobj, iitel
!$$$$$$       integer inrhru(nopt,2000), nintval, iseed, imet(nopt)
      integer inrhru(nopt,2000), nintval, imet(nopt)
      integer icalpar(4,iobj), nopt, isenspar(4,isens)
	character*10 parname(nopt)
      common /iopar/ in,iprii
c
	
      in = 18009
      iprii = 18011
	ipr=0
      open(unit=in,file='sensin.dat',status='old')
      open(unit=iprii,file='sensout.out',status='unknown')
      open(18012, file='sensobjf.out')
      open(18013, file='senspar.out')
	open(18014, file='sensrespons.out')
      open(18015, file='sensresult.out')
	open(18018, file='lathyppar.out')
	open(18019, file='oatpar.out')
	
!$$$$$$       call sensin(dt,nintval,iseed)
	call sensin(dt,nintval)
	call readmetfiles(iobj,calw, icalpar,isenspar,sensw,
     *	isens)
	call readchangepar(bbound,nopt,iiname,iinr, inrhru,imet,parname)
	write(18015,1815) (parname(kk), kk=1,nopt)
1815  format(13x,100a10)
c	go to 4
!$$$$$$       call sample(nopt,nintval,iseed, bbound,dt)
	call sample(nopt,nintval,bbound,dt)
	write (*,*)
	write (*,*) 'WARNING: the program will run ',nintval*(nopt+1),
     * ' times!!!'
      write (*,*)	
	icd=0

	iitel=0
	rewind(18013)

5      read (18013,8013, end =4) i, (xxo(ii), ii=1,nopt)
8013    format(i5, 100e12.5)
       iitel=iitel+1
	call rerun(xxo, nopt,iiname, inrhru, iinr, iitel,imet,ipr)
      call objfunctn(obj, iobj, icalpar, iitel,icd)
	if (isens.gt.0) call response(sensw, isens, isenspar, iitel)
	go to 5
4	continue	
	call analyse(nintval,nopt,iobj,isens, parname)
	close(in)
      close(iprii)
      close(18012)
      close(18013)
	close(18014)
      close(18015)
      close(18016)
	close(18017)
	close(18018)
	close(18019)

c  END OF THE MAIN PROGRAM
      return
	end
