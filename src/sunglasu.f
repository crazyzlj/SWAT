	subroutine sunglasunc(iobj,igoc,nopt,icalml,bbound,
     *	parname, ntot,omin,nobs,m,iiname, inrhru, iinr,
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
	real*8 objgmx(iobj), objgmn(iobj),goc(icalml)
	real*8 biasmax(iobj),biasmin(iobj),vbias(iobj), vofsg(iobj,2)		
	integer isenspar(4,isens), imet(nopt)
	integer  m,ntot,igoc, itest(iobj), iofsg(iobj)
	integer  iiname(nopt), iinr(nopt), inrhru(nopt,2000),nobs(iobj)
	integer iobj,nopt,isens,icalml,icalpar(4,iobj)
	integer ipg(nopt,21),ipb(nopt,21),ipp(nopt), ind(icalml)

	vofsg=0.
602   continue
	read(19023,19023,end=601) j,vofsg(j,1),vofsg(j,2)
19023	format (i5,2f10.3)
	go to 602
601	continue

	close(18023)

	write(18011, *)
	write(18011, *)
	write(18011, *) '*** UNCERTAINTY ANALYSIS RESULT ***'
	write(18011, *)
	
	write(18011, *) 'Results using SUNGLASSES'
	rewind(18018)
	rewind(18014)
	do j=1,icalml
	read(18018,18) ik,ii,(vpar(kk),kk=1,nopt),goc(j)

	end do
	call sorteer6(goc,ind,icalml)
	itest=0
	ji=0
	biasmax=-1.e10
	biasmin=1.e10
441	continue
	ji=ji+1
	ialltest=1
	rewind(18021)
	do ii=1,ind(ji)
	read(18021,5)
	end do
	read(18021,5) ik,(vbias(kk), kk=1,iobj)	
		do m=1,iobj
		biasmax(m)=max(biasmax(m),vbias(m))
	    biasmin(m)=min(biasmin(m),vbias(m))
	   end do
		do i=1,iobj
             if (biasmin(i).lt.vofsg(i,1).and.biasmax(i).gt.vofsg(i,2)) &
     &		itest(i)=1
	    ialltest=ialltest*itest(i)
	    end do
	if(ji.eq.icalml) go to 442
	if(ialltest.ne.1) go to 441

	objflim=goc(ji)


	objgmx=-1.e10
	objmx=-1.e10
	
	respmxg=-1.e10
	respmx=-1.e10
	omin=1.e10
	objgmn=1.e10
	respmn=1.e10
	respmng=1.e10
	parmn=1.e10
	parmx=-1.e10
	islc=0

	write(*,*) 'objective function limit',objflim
	write(18011,*) 'objective function limit',objflim	
	write(18011,*) 'maximum biases'
	write(18011,*) (biasmax(m),m=1,iobj)	
	write(18011,*) 'minimum biases'
	write(18011,*) (biasmin(m),m=1,iobj)
	do j=1,nopt
	parmn(j)=bbound(2,j)
	parmx(j)=bbound(1,j)
	end do



	rewind(18014)
	rewind(18012)
      rewind(18018)

	read(18012,*)
	do i=1,icalml


		read(18012,5) ik,(vobj(kk), kk=1,iobj)
		read(18018,18) ik,ii,(vpar(kk),kk=1,nopt),t
		read(18014,5, end=66) ik,(vresp(kk), kk=1,isens)

		do j=1,isens
		  respmx(j)=max(respmx(j),vresp(j))
		  respmn(j)=min(respmn(j),vresp(j))
		end do
66	continue
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
	  respr(j)=(respmxg(j)-respmng(j))*100./(respmx(j)-respmn(j))
	end do


	write(18011,*)'*****statistics of selected space  *****'
	write(18011,*)'       *****  SUNGLASSES *****' 
	write(18011,*) ' Number of observations = ',nobs 
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
	go to 111
442	continue
      write(18011,*) 'sunglasses couldnt make any selection'
	write(18011,*) 'maximum biases'
	write(18011,6) (biasmax(m),m=1,iobj)	
	write(18011,*) 'minimum biases'
	write(18011,6) (biasmin(m),m=1,iobj)
	write(18011,*) (itest(m),m=1,iobj)
7	format(5x,100f12.5)
6	format(5x,100e12.5)
5	format(i5,100e12.5)
18	format(2i5,100e12.5)
111	continue
	return
	end
