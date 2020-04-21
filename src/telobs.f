       subroutine telobs(itelt,iobj, icalpar)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    calculating the number of observations

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
        real*8   valout , valdat, hh(3), ave
        INTEGER yout, dagout, ydat, dagdat, daysout, daysdat, hdat, hout
        integer itelt(iobj), icalpar(4,iobj),iobj
        real*8 calouttot(22), caldattot(22)

        character*4 fmnname(40), fmxname(40)
        character*1 mflag
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
c
c::::::::::::::
c  objfunct.for
c::::::::::::::

c
c  mean sqare function
c  

        itelt=0

        do mm=1,iobj
         ii=icalpar(1,mm)
        jj=icalpar(2,mm)
        kk=icalpar(3,mm)
        ll=icalpar(4,mm)

        rewind(8000+ll)
        rewind(9000+ll)

      read(8000+ll,5001)yout, mflag,  dagout, hout,
     *	(calouttot(ki), ki=1,22)
      read(9000+ll,5000) ydat, dagdat, hdat,(caldattot(ki), ki=1,22)
47    continue

       daysout = yout*100000 + dagout*100+hout
       daysdat = ydat*100000 +dagdat*100+hdat

       if (daysout-daysdat) 44,45,46
44    read (8000+ll,5001, end=60) yout, mflag,  dagout, hout,
     *	(calouttot(ki), ki=1,22)
      go to 47

45     continue
	
	  if (kk.eq.1.and.ii.ne.1.and.mflag.ne.'m') then
	  if (caldattot(1).lt.0.or.calouttot(1).lt.0.) go to 49
        if (caldattot(ii).lt.0.or.calouttot(ii).lt.0.) go to 49
	  else
        if (caldattot(ii).lt.0.or.calouttot(ii).lt.0.) go to 49
	  end if
        itelt(mm)=itelt(mm)+1
 
49      continue
        read (8001+ll,5000, end=60) yout, mflag,  dagout, hout,
     *	(calouttot(ki), ki=1,22)
        read (9000+ll,5000, end=60) ydat, dagdat, hdat,
     *(caldattot(ki), ki=1,22)
         go to 47

46       read (9000+ll,5000, end=60) ydat, dagdat,hdat,
     * (caldattot(ki), ki=1,22)
        go to 47
60      continue
5000  format(1x,i4,2x,i3,1x,i2,22e11.3)
5001  format(1x,i4,1x,a1,i3,1x,i2,22e11.3)

	end do
         return
      end
