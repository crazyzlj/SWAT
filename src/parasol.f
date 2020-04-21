      subroutine parasol(nopt,iobj,isens)
!     subroutine parasol(iclb,nopt)
      use parm
!     !  !         !         !         !         !         !         ! !
c	Main program for the optimisation and uncertainty analysis

!!    iobj         |none          |number of objective functions
!!    obj(:)       |none          |values of the objective functions
!!    iclb         |none          |method to form GOC out of objective functions
!!    icalpar(:,:) |none          |objective functions codes of OBJMET.DAT
!!    iitel        |none          |number of simulation run
!!    iinr(:)      !              |number of HRUs to change for parameter (:)
!!    inrhru(:,:)  |none          |list of HRU numbers to change  

c     AVG



!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
      integer icalpar(4,iobj), iobj, isenspar(4,isens), imet(nopt),isens
      integer  iiname(nopt), iinr(nopt), inrhru(nopt,2000),nobs(iobj)
	integer icd,kstop,icalml
	integer nopt,npt,nintval,igoc,ngs,npg,nps,nspl,istat, iprob
      real*8 bbound(2,nopt), sensw(isens), omin(iobj), obj(40),
     *	bestf, aveval(iobj), varval(iobj), calw(100)
      real*8 valmin(iobj), valmax(iobj), xxo(nopt),pcento
      character*10 deflt,usrsp, parname(nopt)
      character*8 ysflg,noflg
      data deflt/' DEFAULT  '/
      data usrsp/'USER SPEC.'/
      data ysflg/'YES '/
      data noflg/'NO  '/
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
      common /iopar/ in,iprii
	

      open(18009,file='parasolin.dat',status='old')
      open(18011,file='parasolout.out',status='unknown')
      open(18012, file='sceobjf.out')
	open(18014, file='sceresponse.out')
      open(18013, file='scepar.out')
      open(18018, file='sceparobj.dat')
	open(18019, file='scegoc.out')
	open(19014, file='uncresponse.out')
      open(19013, file='goodpar.out')
      open(19012, file='uncobjf.out')
	open(18020,file='bestpar.out')
	open(19020,file='test.out')
		iprii=18011
	call readchangepar(bbound,nopt,iiname,iinr, inrhru,imet,parname)
	call readmetfiles(iobj,calw, icalpar,isenspar,sensw,
     *	isens)
!$$$$$$       call parasolin(maxn,kstop,pcento,iseed,nopt,iobj,npt,
	call parasolin(maxn,kstop,pcento,nopt,iobj,npt,
     &  nintval,igoc,ngs,npg,nps,nspl,istat, iprob)

	call countobs(nobs,iobj, icalpar,aveval,varval)

	write(18011, *) 'OF   # obs| average |variance    '
	do ii=1,iobj
	write(18011,121) ii, nobs(ii), aveval(ii),varval(ii)
121   format(i5,i5,2e11.3)
	end do
	ipr=0
	icd=0
c	go to 66
c
c	calling subroutine to optimise for all the objective functions
!$$$$$$       call parasolopt(nopt,iobj,isens, npt,icd,ngs,npg,nps,nspl,
!$$$$$$      *      maxn,kstop,pcento,iseed,igoc,icalml,iiname, bbound,
!$$$$$$      * iinr, inrhru, icalpar, sensw, isenspar,
!$$$$$$      *bestf, imet,omin,nobs,varval,iprob,istat)
      call parasolopt(nopt,iobj,isens, npt,icd,ngs,npg,nps,nspl,
     *	maxn,kstop,pcento,igoc,icalml,iiname, bbound,
     * iinr, inrhru, icalpar, sensw, isenspar,
     *bestf, imet,omin,nobs,varval,iprob,istat)
	write(18011, *)
	write(18011, *) '*** AUTOCALIBRATION SUCCESSFULLY ENDED ***'
	write(18011, *)
	call parasolunc(iobj,igoc,nopt,icalml,istat, iprob,bbound,
     *	parname, ntot,omin,nobs,nintval, iiname, inrhru, iinr,
     *isens,icalpar, isenspar, sensw, imet)
      close(18011)
      close(18012)
      close(18013)
	close(18014)
      close(18016)
	close(18017)
	close(18018)
	close(18019)
	close(18020)
      close(19012)
      close(19013)
	close(19014)
!$$$$$$ 66    continue
      continue


	



	if (iclb.eq.3) then
c	calling subroutine to analyse selection to compute uncertainty

c  START RUNNING GOODPAR.OUT
	write(*,*) 'start rerunning goodpar.out parameter sets....'
      open(18011,file='batchout.dat',status='unknown')
      open(18012, file='batchobjf.out')
      open(18013, file='batchpar.out')
      open(18015, file='goodpar.out')
	open(18014, file='batchrespons.out')
	open (18118, file='minval.out')
	open (18119, file='maxval.out')
	icd=2
	icalml=0
	ipr=0
5	read(18015,8013, end =4) i, (xxo(ii), ii=1,nopt)
8013  format(i5, 100e12.5)
	icalml=icalml+1
      call rerun(xxo, nopt,iiname, inrhru, iinr, icalml, imet,ipr)
      call objfunctn(obj, iobj, icalpar, icalml,icd)
	call response(sensw, isens, isenspar, icalml)
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
5000  format(1x,i4,2x,i3,1x,i2,22e10.3)	
	end do
	write(18118,5001) iyr, iday, ihr,(valmin(ii), ii=1,iobj)
	write(18119,5001)iyr, iday, ihr, (valmax(ii), ii=1,iobj)
5001	format(1x,i4,2x,i3,1x,i2,100e10.3)
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
      close(18118)
	close(18119)

	end if
	write(*,*) "ParaSol is rerunning the best parameter set"
	open(18020,file='bestpar.out')
	read(18020,8013) iitel, (xxo(ii), ii=1,nopt)
	ipr=1
      call rerun(xxo, nopt,iiname, inrhru, iinr, iitel, imet,ipr)
	close(18020)
      return
	end
