	subroutine parasolunc(iobj,igoc,nopt,icalml,istat, iprob,bbound,
     *	parname, ntot,omin,nobs,m,    iiname, inrhru, iinr,
     *isens,icalpar, isenspar, sensw, imet)
	character*10 parname(iobj)
c	calculates uncertainty
c	written by Ann van Griensven - University of California Riverside, april 2003


      real*8  sensw(isens), objfmin,objflim

	real*8 varobj(iobj), oflim(iobj),vobj(iobj),vresp(isens)
	real*8 parmn(nopt),parmx(nopt), bbound(2,nopt),vpar(nopt)
	real*8 objmx(iobj),respmn(isens),respmx(isens)
	real*8 respmng(isens),respmxg(isens),ff,omin(iobj)
	real*8 parr(nopt),objr(iobj),respr(isens), xp(nopt), yp(nopt)
	real*8 objgmx(iobj), objgmn(iobj)		
	integer isenspar(4,isens), imet(nopt)
	integer  m,ntot,igoc, istat, iprob
	integer  iiname(nopt), iinr(nopt), inrhru(nopt,2000),nobs(iobj)
	integer iobj,nopt,isens,icalml,icalpar(4,iobj)
	integer ipg(nopt,21),ipb(nopt,21),ipp(nopt)
	write(18011, *)
	write(18011, *)
	write(18011, *) '*** UNCERTAINTY ANALYSIS RESULT ***'
	write(18011, *)
	ntot=0
	do mm=1,iobj
	    varobj(mm)=omin(mm)/nobs(mm)
		ntot=ntot+nobs(mm)
	end do

	select case (istat)
	case (1)
	write(18011, *) 'Results using Xi squared statistics'
	call xiunc(omin,nobs,nopt,igoc,iprob,icalml,objflim,objfmin)
	case (2)
	write(18011, *) 'Results using Bayesian statistics'
	call bayes(m,omin,nobs,nopt,igoc,iprob,icalml,objflim,
     *	objfmin,bbound)
	end select

	objgmx=-1.e20
	objmx=-1.e20
	
	respmxg=0.
	respmx=0.
	omin=1.e20
	objgmn=1.e20
	respmn=1.e20
	respmng=1.e20
	parmn=1.e20
	parmx=-1.e20
	islc=0

	write(*,*) 'objective function limit',objflim

	do j=1,nopt
	  parmn(j)=bbound(2,j)
	  parmx(j)=bbound(1,j)
	end do

	rewind(18014)
	rewind(18012)
      rewind(18018)
	rewind(19013)
	rewind(19014)
      rewind(19012)
	read(18012,*)
	do i=1,icalml


	  read(18012,5) ik,(vobj(kk), kk=1,iobj)
	  read(18018,18) ik,ii,(vpar(kk),kk=1,nopt),t
	  read(18014,5, end=66) ik,(vresp(kk), kk=1,isens)

	  do j=1,isens
		respmx(j)=max(respmx(j),vresp(j))
		respmn(j)=min(respmn(j),vresp(j))
	  end do
66	  continue
	  do j=1,iobj
		omin(j)=min(omin(j),vobj(j))
		objmx(j)=max(objmx(j),vobj(j))
	  end do
	  if (t/objflim.le.1.01) then
	    do j=1,nopt
	      parmn(j)=min(parmn(j),vpar(j))
	      parmx(j)=max(parmx(j),vpar(j))
	    end do
	    do j=1,iobj
		  objgmn(j)=min(objgmn(j),vobj(j))
		  objgmx(j)=max(objgmx(j),vobj(j))
	    end do
	    write(19012,5) i,(vobj(kk), kk=1,iobj)

	    write(19013,5) i,(vpar(kk),kk=1,nopt)
	    if (isens.gt.0) then
	      write(19014,5) i,(vresp(kk), kk=1,isens)
	      do j=1,isens
	        respmxg(j)=max(respmxg(j),vresp(j))
	        respmng(j)=min(respmng(j),vresp(j))
	      end do
	    end if
	    do j=1,nopt
		parmx(j)=max(parmx(j),vpar(j))
		parmn(j)=min(parmn(j),vpar(j))
            ip=int((vpar(j)-bbound(1,j))*20./(bbound(2,j)-bbound(1,j)))
		ipg(j,ip+1)=ipg(j,ip+1)+1
		ipg(j,21)=ipg(j,21)+1
	    end do	
	    islc=islc+1
	  end if
	end do
	do j=1,nopt
	  parr(j)=(parmx(j)-parmn(j))*100./(bbound(2,j)-bbound(1,j))
	end do
	do j=1,isens
	  xx = respmx(j) - respmn(j)
	  if (xx > 1.e-6) then
          respr(j)=(respmxg(j)-respmng(j))*100./(respmx(j)-respmn(j))
	  else
	     respr(j) = 0.0
	  end if
	end do

	write(18011,*)'*****statistics of the entire space*****'
	write(18011,*) 'minimum global objective function', objfmin
	write(18011,*) 'total number of runs = ', icalml
	write(18011,*) 'minimum objective functions'
	write(18011,6) (omin(kk), kk=1,iobj)
	write(18011,*) 'maximum objective function values'
	write(18011,6) (objmx(kk),kk=1,iobj)	
	write(18011,*)
	write(18011,*) 'minimum output values'
	write(18011,6) (respmn(kk),kk=1,isens)
	write(18011,*) 'maximum output values'
	write(18011,6) (respmx(kk),kk=1,isens)
	write(18011,*)

	write(18011,*)'*****statistics of selected space  *****'
	select case (iprob)
	case (1)
	write(18011,*) '90% probability uncertainty analysis' 
	case (2)
	write(18011,*) '95% probability uncertainty analysis' 
	case (3)
	write(18011,*) '97.5% probability uncertainty analysis' 
	end select
	write(18011,*) ' Total number of observations = ',ntot 
	write(18011,*) ' Number of free parameters = ',nopt
	write(18011,*) ' Limit on global objective function = ', objflim

	write(18011,*) ' number of selections', islc
	write(*,*) ' number of selections', islc
	write(18011,*)

	write(18011,*)'minimum parameter values'
	write(18011,6) (parmn(kk), kk=1,nopt)
	write(18011,*)'maximum parameter values'
	write(18011,6) (parmx(kk), kk=1,nopt)
	write(18011,*)'percentage of range '
	write(18011,7) (parr(kk), kk=1,nopt)
	write(18011,*)
	write(18011,*) 'mimimum objective function values'
	write(18011,6) (objgmn(kk), kk=1,iobj)
	write(18011,*) 'maximum objective function values'
	write(18011,6) (objgmx(kk),kk=1,iobj)	
	write(18011,*)
	write(18011,*)'minimum output values'
	write(18011,6) (respmng(kk),kk=1,isens)
	write(18011,*)'maximum output values'
	write(18011,6) (respmxg(kk),kk=1,isens)
	write(18011,*)'percentage of range'
	write(18011,7) (respr(kk),kk=1,isens)

7	format(5x,100f12.5)
6	format(5x,100e12.5)
5	format(i5,100e12.5)
18	format(2i5,100e12.5)
111	continue
	return
	end
