      subroutine rerunfile(nopt,iobj,isens)
!     !  !         !         !         !         !         !         ! !
c	Program to rerun the best parameter set

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
!$$$$$$       real*8 bbound(2,nopt), calw(iobj), sensw(isens),dt, xxo(nopt)
      real*8 bbound(2,nopt), calw(iobj), sensw(isens),xxo(nopt)
	real*8 obj(iobj)
      integer iinr(nopt), iiname(nopt), isens, imet(nopt)
!$$$$$$       integer inrhru(nopt,2000), iobj, nintval, iseed
      integer inrhru(nopt,2000), iobj
      integer icalpar(4,iobj), nopt, isenspar(4,isens)
      common /iopar/ in,iprii

c
	
      in = 18010
      iprii = 18011
	ipr=1
	icd=0

      open(unit=in,file='changepar.dat',status='old')
      open(18012, file='objf.out')
      open(18015, file='bestpar.out')
	open(18014, file='respons.out')

			
	call batchin(bbound,nopt,
     & iiname,iinr, inrhru,iobj,
     & calw, icalpar,isenspar,sensw,isens,imet)
5	read(18015,8013, end =4) iitel, (xxo(ii), ii=1,nopt)

8013    format(i5, 100e12.5)
      call rerun(xxo, nopt,iiname, inrhru, iinr, iitel,imet,ipr)
      call objfunctn(obj, iobj, icalpar, iitel,icd)
	call response(sensw, isens, isenspar, iitel)
	go to 5
4	continue	

	close(in)
      close(iprii)
	close(18014)
	close(18015)
      close(18016)
	close(18017)

c  END OF THE BATCH ROUTINE
      return
	end
