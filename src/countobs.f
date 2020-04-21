       subroutine countobs(itelt,iobj, icalpar,aveval,varval)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    calculating the number of observations, the mean and variance of the data

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iobj         |none          |number of objective functions
!!    nobs(:)       |none         |number of observations
!!    icalpar(:,:) |none          |objective functions codes of OBJk_annaMET.DAT
!!    iitel        |none          |number of simulation run
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    obj(:)       |none          |values of the objective functions
!!    ffz(:)       |none          |transformed objectif function
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

		implicit real*8 (a-h,o-z)
        real*8    valdat,  ave, aveval(iobj),varval(iobj) ,avv ,vrv

        INTEGER yout, dagout, ydat, dagdat, daysout, daysdat, hdat, hout
        integer itelt(iobj),iobj,icalpar(4,iobj)
        real*8  calouttot(22), caldattot(22)
	  character*1 mflag


c
c  mean sqare function
c  
        itelt=0
        aveval=0.
        varval=0.
	open(7778, file='help')
	  do mm=1,iobj
         ii=icalpar(1,mm)
        jj=icalpar(2,mm)
        kk=icalpar(3,mm)
        ll=icalpar(4,mm)
	rewind(7778)
      rewind(8000+ll)
      rewind(9000+ll)

       read (8000+ll,5001, end=60) yout, mflag,  dagout, hout,
     *  (calouttot(ki), ki=1,22)
      read(9000+ll,5000) ydat, dagdat, hdat,(caldattot(ki), ki=1,22)
47    continue

       daysout = yout*100000 + dagout*100+hout
       daysdat = ydat*100000 +dagdat*100+hdat

	  if (kk.eq.1.and.ii.ne.1.and.mflag.ne.'m') then
	  if (caldattot(1).lt.0.or.calouttot(1).lt.0.) go to 49
        if (caldattot(ii).lt.0.or.calouttot(ii).lt.0.) go to 49
	  valdat=caldattot(1)*caldattot(ii)
	  else
       if (daysout-daysdat) 44,45,46
44     read (8000+ll,5001, end=60) yout, mflag,  dagout, hout,
     *  (calouttot(ki), ki=1,22)
      go to 47

45     continue		
        if (caldattot(ii).lt.0.or.caldattot(ii).lt.0.) go to 49
	  valdat=caldattot(ii)
	  end if

	aveval(mm)=aveval(mm)+valdat
      itelt(mm)=itelt(mm)+1

	write(7778, 78) valdat
 
49      continue
         read (8000+ll,5001, end=60) yout, mflag,  dagout, hout,
     *  (calouttot(ki), ki=1,22)
        read (9000+ll,5000, end=60) ydat, dagdat, hdat,
     *(caldattot(ki), ki=1,22)
         go to 47

46       read (9000+ll,5000, end=60) ydat, dagdat,hdat,
     * (caldattot(ki), ki=1,22)
        go to 47
60      continue
5000  format(1x,i4,2x,i3,1x,i2,22e11.3)
5001  format(1x,i4,1x,a1,i3,1x,i2,22e11.3)
78    format(e12.5)
	if( itelt(mm).gt.0) aveval(mm)=aveval(mm)/itelt(mm)
	itel=itelt(mm)
	avv=aveval(mm)

	call vrval(itel,avv,vrv)
	varval(mm)=vrv
	end do

       return
      end
