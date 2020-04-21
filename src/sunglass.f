      subroutine sunglasses(nopt,iobj,isens)
!     subroutine sunglasses(iclb,nopt)
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



c  THIS IS THE MAIN PROGRAM CALLING SUBROUTINES sunglasin, parasolopt, parasolunc, sunglasunc
!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
      integer icalpar(4,iobj), iobj, isenspar(4,isens),iprob, imet(nopt)
      integer  iiname(nopt), iinr(nopt), inrhru(nopt,2000),nobs(iobj)
	integer icalml, igoc
!$$$$$$       real*8 a(100), bbound(2,nopt), sensw(isens), omin(iobj),
!$$$$$$      *      bestf, varobj(iobj), varval(iobj), avval(iobj), calw(100)
      real*8 bbound(2,nopt), sensw(isens), omin(iobj),
     *	bestf, varval(iobj), avval(iobj), calw(100)
!$$$$$$       real*8  xxo(nopt)
!$$$$$$       character*10 pcntrl,deflt,usrsp, parname(nopt)
      character*10 deflt,usrsp, parname(nopt)
!$$$$$$       character*8 reduc,initl,ysflg,noflg
      character*8 ysflg,noflg
      data deflt/' DEFAULT  '/
      data usrsp/'USER SPEC.'/
      data ysflg/'YES '/
      data noflg/'NO  '/
      common /iopar/ in,iprii


!$$$$$$        character*4 fmnname(40), fmxname(40), tt
	 character*4 fmnname(40), fmxname(40)
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

	
      open(18010,file='changepar.dat',status='old')
      open(18009,file='sunglasses.in',status='old')
      open(18011,file='sunglasses.out',status='unknown')
      open(18012, file='sceobjf.out')
	open(18014, file='respons.out')
      open(18013, file='scepar.out')
      open(18018, file='sceparobj.dat')
	open(18019, file='scegoc.out')
	open(18020, file='bestpar.out')
	open(19014, file='paruncbias.out')
      open(19013, file='pargoodpar.out')
      open(19012, file='paruncobjf.out')
	open(18021, file='sunglasbias.out')
      open(18024, file='sunglasuncbias.out')
      open(19022, file='sunglasuncobjf.out')
	open(19023, file='sunglasmet.in')
		iprii=18011

	call readchangepar(bbound,nopt,iiname,iinr, inrhru,imet,parname)
	call readmetfiles(iobj,calw, icalpar,isenspar,sensw,
     *	isens)
!$$$$$$       call parasolin(maxn,kstop,pcento,iseed,nopt,iobj,npt,
!$$$$$$      &  nintval,igoc,ngs,npg,nps,nspl,istat, iprob)
	call parasolin(maxn,kstop,pcento,nopt,iobj,npt,
     &  nintval,igoc,ngs,npg,nps,nspl,istat, iprob)

	call countobs(nobs,iobj, icalpar,avval,varval)
	write(18011, *) '# observations |average   |variance    '
	do ii=1,iobj
	write(18011,*) ii, nobs(ii), avval(ii),varval(ii)
	end do
!$$$$$$       ipr=0
	icd=3
c	go to 66
c	go to 67
c	calling subroutine to optimise for all the objective functions


!      call parasolopt(bbound,nopt,maxn,kstop,pcento,iseed,
!     *iiname, ngs,npg,nps,nspl,
!     *iniflg, iinr, inrhru,iobj, igoc,icalpar, 
!     *isens, sensw, isenspar,bestf,icalml, imet,omin,nobs,npt,i
!    * ,istat, iprob,varval)
	write(18011, *)
	write(18011, *) '*** AUTOCALIBRATION SUCCESSFULLY ENDED ***'
	write(18011, *)
!$$$$$$ 67    continue

!$$$$$$       call parasolopt(nopt,iobj,isens, npt,icd,ngs,npg,nps,nspl,
!$$$$$$      *      maxn,kstop,pcento,iseed,igoc,icalml,iiname, bbound,
!$$$$$$      * iinr, inrhru, icalpar, sensw, isenspar,
!$$$$$$      *bestf, imet,omin,nobs,varval,iprob,istat)
      call parasolopt(nopt,iobj,isens, npt,icd,ngs,npg,nps,nspl,
     *	maxn,kstop,pcento,igoc,icalml,iiname, bbound,
     * iinr, inrhru, icalpar, sensw, isenspar,
     *bestf, imet,omin,nobs,varval,iprob,istat)

	close(19013)
      open(19013, file='SGgoodpar.out')

	call sunglasunc(iobj,igoc,nopt,icalml, bbound,
     *	parname, ntot,omin,nobs,nintval,
     *    iiname, inrhru, iinr,
     * isens,icalpar, isenspar, sensw, imet)
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
	close(19023)
	close(19022)
!$$$$$$ 66    continue
	return
	end

c  END OF THE MAIN PROGRAM
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
