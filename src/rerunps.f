      subroutine rerunPS(nopt,iobj,isens)
!     !  !         !         !         !         !         !         ! !
c	Main program to rerun the uncertainty analysis
c	written by Ann van Griensven - University of California Riverside, april 2003

!!    iobj         |none          |number of objective functions
!!    obj(:)       |none          |values of the objective functions
!!    iclb         |none          |method to form GOC out of objective functions
!!    icalpar(:,:) |none          |objective functions codes of OBJMET.DAT
!!    iitel        |none          |number of simulation run
!!    iinr(:)      !              |number of HRUs to change for parameter (:)
!!    inrhru(:,:)  |none          |list of HRU numbers to change  

c     AVG



c  THIS SUBROUTINES RERUNS SWAT FOR UNCERTAINTY ON OUTPUT
!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
      integer icalpar(4,iobj), iobj, isenspar(4,isens),iprob, imet(nopt)
      integer  iiname(nopt), iinr(nopt), inrhru(nopt,2000),nobs(100)
!$$$$$$       integer icalml, igoc,icd,istat
	integer icalml, igoc,istat
!$$$$$$       real*8 a(100), bbound(2,nopt), sensw(isens), omin(iobj),
!$$$$$$      *      bestf, varobj(iobj),aveval(iobj), varval(iobj), calw(100)
      real*8 bbound(2,nopt), sensw(isens), omin(iobj),
     *	aveval(iobj), varval(iobj), calw(100)
!$$$$$$       real*8 valmin(iobj), valmax(iobj), xxo(nopt),vobj(iobj)
      real*8 vobj(iobj)
!$$$$$$       character*10 pcntrl,deflt,usrsp, parname(nopt)
      character*10 deflt,usrsp, parname(nopt)
!$$$$$$       character*8 reduc,initl,ysflg,noflg
      character*8 ysflg,noflg
      data deflt/' DEFAULT  '/
      data usrsp/'USER SPEC.'/
      data ysflg/'YES '/
      data noflg/'NO  '/

      common /iopar/ in,iprii
   
      open(18009,file='parasolin.dat',status='old')
      open(18011,file='parasolout2.out',status='unknown')
      open(18012, file='sceobjf.out')
	open(18014, file='sceresponse.out')
      open(18013, file='scepar.out')
      open(18018, file='sceparobj.dat')
	open(18019, file='scegoc.out')
	open(19014, file='uncresponse.out')
      open(19013, file='goodpar.out')
      open(19012, file='uncobjf.out')
	open(18020,file='bestpar.out')
		iprii=18011			
	call readchangepar(bbound,nopt,iiname,iinr, inrhru,imet,parname)
	call readmetfiles(iobj,calw, icalpar,isenspar,sensw,
     *	isens)
!$$$$$$       call parasolin(maxn,kstop,pcento,iseed,nopt,iobj,npt,
!$$$$$$      &  nintval,igoc,ngs,npg,nps,nspl, istat, iprob)
	call parasolin(maxn,kstop,pcento,nopt,iobj,npt,
     &  nintval,igoc,ngs,npg,nps,nspl, istat, iprob)

	call countobs(nobs,iobj, icalpar,aveval,varval)
	write(18011, *) 'OF   # obs| average |variance    '
	do ii=1,iobj
	write(18011,121) ii, nobs(ii), aveval(ii),varval(ii)
121   format(i5,i5,2e11.3)	
	end do

	omin=1.e10
	rewind(18012)
	icalml=0
	read(18012,*)
1	read(18012,5,end=3) ik,(vobj(kk), kk=1,iobj)
	do kk=1,iobj
	omin(kk)=min(omin(kk),vobj(kk))
	end do
	icalml=icalml+1
	go to 1 
3     continue

5     format(1i5,100e12.5)
c	omin(1)=121000.
c	omin(2)=2370.
c	omin(3)=36700.
c	omin(4)=171.
c	icalml=1243
c	selecting of acceptible simulations based on objective functions

	call parasolunc(iobj,igoc,nopt,icalml,istat, iprob,bbound,
     *	parname, ntot,omin,nobs,m,    iiname, inrhru, iinr,
     *isens,icalpar, isenspar, sensw, imet)

c	calling subroutine to analyse selection to compute uncertainty
	
	close(18010)
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
c  END OF THE MAIN PROGRAM
      return
	end
