       subroutine objfunctn(obj,iobj, icalpar, iitel,icd)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    calculating the objective functions for SCEUA
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iobj         |none          |number of objective functions
!!    obj(:)       |none          |values of the objective functions
!!    icalpar(:,:) |none          |objective functions codes of OBJMET.DAT
!!    icalml       |none          |number of simulation run
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    obj(:)       |none          |values of the objective functions
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

        real*8   valout , valdat, hh(3), ave
        INTEGER yout, dagout, ydat, dagdat, daysout, daysdat, hdat, hout
        integer itelt(iobj), iobj
        integer itel,status, iitel, icalpar(4,iobj)
        real*8 help,ff,cal1,cal2
        real*8  obj(iobj),calouttot(22),caldattot(22), tempval, helpval
        real*8 objmn(iobj),objmx(iobj),sgobj(iobj)
        character*4 fmnname(40), fmxname(40),tt
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

c::::::::::::::
c  objfunct.for
c::::::::::::::

c
c  mean sqare function
c  


7777    format(40e10.3)
	  open(7778,file='helprank')
77      continue

        do mm=1,iobj

        obj(mm)=0.
        help=0.

        ii=icalpar(1,mm)
        jj=icalpar(2,mm)
        kk=icalpar(3,mm)
        ll=icalpar(4,mm)
        rewind(7778)
        rewind(8000+ll)
        rewind(9000+ll)
        itel=0
      read(8000+ll,5001) yout, mflag,  dagout, hout,
     *	(calouttot(ki), ki=1,22)
      read(9000+ll,5000) ydat, dagdat, hdat,(caldattot(ki), ki=1,22)
47    continue

       daysout = yout*100000 + dagout*100+hout
       daysdat = ydat*100000 +dagdat*100+hdat

       if (daysout-daysdat) 44,45,46
44    read (8000+ll,5001, end=60)yout, mflag,  dagout, hout,
     *  (calouttot(ki), ki=1,22)
      go to 47

45     continue		


        if (caldattot(ii).lt.0.or.calouttot(ii).lt.0.) go to 49
        if(kk.eq.1.and.ii.ne.1.and.mflag.eq.' ') then
        if (caldattot(1).lt.0.or.calouttot(1).lt.0.) go to 49
		cal1=caldattot(ii)*caldattot(1)
		cal2=calouttot(ii)*calouttot(1)
	   else
		cal1=caldattot(ii)
		cal2=calouttot(ii)
	   end if
	  itel=itel+1
	if (jj.le.4) then
		select case (jj)
		case (1)
		  help=help+(cal1-cal2)**2
		case(2)
		  if (cal1.le.0.001) cal1=0.001 
		  if (cal2.le.0.001) cal2=0.001 
		  help=help+(log(cal1)-log(cal2))**2
		case(3)
		  help=help+(sqrt(cal1)- sqrt(cal2))**2
		CASE(4)
		  ff=(cal1+cal2)/2
		  help=help+((cal1-cal2)**2)/ff
		end select

	else
	write(7778, 778) cal1,cal2
778    format(2e12.5)
	end if
49      continue
        read (8000+ll,5001, end=60)yout, mflag,  dagout, hout,
     *(calouttot(ki), ki=1,22)
        read (9000+ll,5000, end=60) ydat, dagdat, hdat,
     *(caldattot(ki), ki=1,22)
         go to 47

46       read (9000+ll,5000, end=60) ydat, dagdat,hdat,
     * (caldattot(ki), ki=1,22)
        go to 47
60      continue
5000  format(1x,i4,2x,i3,1x,i2,22e11.3)
5001  format(1x,i4,1x,a1,i3,1x,i2,22e11.3)

        if (jj.le.4) then
	obj(mm)=help
	   else
			if (jj.lt.8) then
			  call ranked(itel,hh)
				else
			  call aveval(itel, ave)
			end if
        select case (jj)
         case (5) 
         obj(mm) = hh(1)
        case (6)
         obj(mm) = hh(2)
        case(7)
         obj(mm) = hh(3)
        case(8)
         obj(mm) = ave
	  case(9) 
	   obj(mm)=abs(ave)
        end select
	end if
       end do

	write(18012,8012) iitel, (obj(mm), mm=1,iobj)
8012    format(i5, 100e12.5)
c	section only for rerunning goodpar file
	if (icd.eq.2) then

        do mm=1,iobj
        objmn(mm)=obj(mm)
        ii=icalpar(1,mm)
        jj=icalpar(2,mm)
        kk=icalpar(3,mm)
        ll=icalpar(4,mm)
	if (iitel.le.1) then 
		open(7700+mm,file=fmnname(mm))
		open(7800+mm,file=fmxname(mm))
		rewind(8000+ll)
188   read (8000+ll,5000, end=79) ydat, dagdat,hdat,
     *	(calouttot(ki), ki=1,22)

C     From concentration to flux (when kk == 1)
	if(kk.eq.1.and.ii.ne.1.and.mflag.eq.' ') then
		if (hdat.eq.0) then 
		tempval=calouttot(ii)*calouttot(1)*24.*3600./1000000.
		else
	    tempval=calouttot(ii)*calouttot(1)*3600./1000000.
	    end if
	else
	tempval=calouttot(ii)

	end if
		write(7700+mm,5000) ydat, dagdat, hdat,tempval
		write(7800+mm,5000) ydat, dagdat, hdat,tempval
		go to 188

79     continue

		else
	tt=fmnname(mm)
	call copyfile(tt, 'helpmin')
	tt=fmxname(mm)
	call copyfile(tt, 'helpmax')

	open(7799, file='helpmin')
      open(7700+mm,file=fmnname(mm))

500   format(i5,f10.3)
        rewind(8000+ll)
177     continue	
        read (8000+ll,5001, end=80)yout, mflag,  dagout, hout,
     *	(calouttot(ki), ki=1,22)
		if(kk.eq.1.and.ii.ne.1.and.mflag.eq.' ') then
		if (hdat.eq.0) then 
	    tempval=calouttot(ii)*calouttot(1)*24.*3600./1000000.
		else
		tempval=calouttot(ii)*calouttot(1)*3600./1000000.
	    end if
	else
	tempval=calouttot(ii)
	end if
	read(7799,5000) ydat, dagdat,hdat,helpval
		helpval=min(helpval,tempval)

       write(7700+mm,5000) ydat, dagdat, hdat,helpval
       go to 177
80	continue
	close(7799) 
	open(7799, file='helpmax')
	open(7800+mm,file=fmxname(mm))

		rewind(8000+ll)
178		continue	
		 read (8000+ll,5001, end=78)yout, mflag,  dagout, hout,
     *	(calouttot(ki), ki=1,22)
		if(kk.eq.1.and.ii.ne.1.and.mflag.eq.' ') then
		if (hdat.eq.0) then 
	    tempval=calouttot(ii)*calouttot(1)*24.*3600./1000000.
		else
	    tempval=calouttot(ii)*calouttot(1)*3600./1000000.
	    end if
		else
		tempval=caldattot(ii)
	end if
		read(7799,5000) ydat, dagdat,hdat,helpval
		do it=1,22
          tempval=calouttot(ii)
		end do
		 write(7800+mm,5000) ydat, dagdat, hdat,helpval
        go to 178

78      continue
	close(7799)

	  end if
	rewind(8000+ll)
      close(7700+mm)
	close(7800+mm)	
	  end do
	end if
c	section only for sunglasses method
	if (icd.eq.3) then
        do mm=1,iobj

        sgobj(mm)=0.
        help=0.

        ii=icalpar(1,mm)
        jj=icalpar(2,mm)
        kk=icalpar(3,mm)
        ll=icalpar(4,mm)
        rewind(7778)
        rewind(8000+ll)
        rewind(9000+ll)
        itel=0
      read(8000+ll,5001)yout, mflag,  dagout, hout,
     *	(calouttot(ki), ki=1,22)
      read(9000+ll,5000) ydat, dagdat, hdat,(caldattot(ki), ki=1,22)
147    continue

       daysout = yout*100000 + dagout*100+hout
       daysdat = ydat*100000 +dagdat*100+hdat

       if (daysout-daysdat) 144,145,146
144    read (8000+ll,5001, end=160) yout, mflag,  dagout, hout,
     *  (calouttot(ki), ki=1,22)
      go to 147

145     continue		


        if (caldattot(ii).lt.0.or.calouttot(ii).lt.0.) go to 149
        if(kk.eq.1.and.ii.ne.1.and.mflag.ne.'m') then
        if (caldattot(1).lt.0.or.calouttot(1).lt.0.) go to 149
		cal1=caldattot(ii)*caldattot(1)
		cal2=calouttot(ii)*calouttot(1)
	   else
		cal1=caldattot(ii)
		cal2=calouttot(ii)
	   end if
	  itel=itel+1

	write(7778, 778) cal1,cal2

149      continue
        read (8000+ll,5001, end=160) yout, mflag,  dagout, hout,
     *(calouttot(ki), ki=1,22)
        read (9000+ll,5000, end=160) ydat, dagdat, hdat,
     *(caldattot(ki), ki=1,22)
         go to 147

146       read (9000+ll,5000, end=160) ydat, dagdat,hdat,
     * (caldattot(ki), ki=1,22)
        go to 147
160      continue

      call aveval(itel, ave)

	   sgobj(mm)=ave
   

       end do

	write(18021,8012) iitel, (sgobj(mm), mm=1,iobj)

	end if

      close(7778)

  100 format (1a,i4,i5,i3,f11.5)
  110 format (1a,i4,i5,i3,2f11.5)
  
      return
      end
