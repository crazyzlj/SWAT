      subroutine writeswatfilemain
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

c
c  THIS IS THE MAIN PROGRAM CALLING SUBROUTINES SENSIN AND RUNSENS



c  THIS IS THE MAIN PROGRAM CALLING SUBROUTINES SENSIN AND RUNSENS
!     !  !         !         !         !         !         !         ! !
      use parm
      
!$$$$$$       real*8 bbound(2,200), calw(100), sensw(200),dt, xxo(200),obj(40)
      real*8 bbound(2,200), calw(100), sensw(200),xxo(200),obj(40)
      real ranval
      integer iinr(200), iiname(200), isens, imet(200)
!$$$$$$       integer inrhru(200,2000), iobj, nintval, iseed
      integer inrhru(200,2000), iobj
      integer icalpar(4,100), nopt, isenspar(4,100)
	character*7 swatfilen
c

      open(18010,file='batchin.dat',status='old')
      open(18011,file='batchout.dat',status='unknown')
      open(18012, file='batchobjf.out')
      open(18013, file='senspar.out')
      open(18015, file='batchpar.dat')
	open(18014, file='batchrespons.out')
      open(18016, file='objmet.dat')
	open(18017, file='responsmet.dat')
	open(18018, file='swatfilenames.prn')	
      open(18019, file='mcin.dat')			
	call batchin(bbound,nopt,
     & iiname,iinr, inrhru,iobj,
     & calw, icalpar,isenspar,sensw,isens, imet)
	read(18019,19) iseed(1), initel, iendtel
19	format(3i10)
      call random_seed
      call random_seed(size=n)
      call random_seed(put = iseed)
      ipr=0
	icd=0
	do iitel=initel,iendtel
	do ii=1,nopt
      call random_number(ranval)
!$$$$$$       xxo(ii)= bbound(1,ii)+(bbound(2,ii)-bbound(1,ii))*ran(iseed)
	xxo(ii)= bbound(1,ii)+(bbound(2,ii)-bbound(1,ii))*ranval
	end do
	write(18015,8013) iitel, (xxo(ii), ii=1,nopt)
8013    format(i5, 100e12.5)
	read(18018, 7) swatfilen
	open (18019,file=swatfilen)
      call rerun(xxo, nopt,iiname, inrhru, iinr, iitel, imet,ipr)
      call objfunctn(obj, iobj, icalpar, iitel,icd)
	call response(sensw, isens, isenspar, iitel)
	call writeswatfile
	close(18019)
	end do
!$$$$$$ 4     continue    
7	format(a7)
	close(18010)
      close(18011)
      close(18012)
      close(18013)
	close(18014)
	close(18015)
      close(18016)
	close(18017)

c  END OF THE BATCH ROUTINE
      return
	end
