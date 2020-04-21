	subroutine bayes(m,omin,nobs,nopt,igoc,iprob,icalml,objflim,
     *	objfmin,bbound)
c	this subroutine calculates the threshold on the objective function based on 
c	Bayesian statisitics	 
c	written by Ann van Griensven - University of California Riverside, april 2003

	real*8 vobj(igoc), vpar(nopt), omin(igoc), varobj(igoc)
	real*8 bbound(2,nopt), bestpar(nopt)
	integer nobs(igoc),itot(icalml), ipar(icalml,nopt)
	integer ipb(nopt,m)
	real*8 objfmin, objflim,vtot(icalml,3)
	rewind(18014)
	rewind(18012)
      rewind(18013)
	rewind(18018)
	read(18012,*)
	ii=0
	objfmin=1.e10
	ipb=0

	select case (iprob)
	case(1)
	probab=.9
	case(2)
	probab=.95
	case(3)
	probab=.975
	end select
	do mm=1,igoc
	  varobj(mm)=omin(mm)/nobs(mm)
		write(18011,*) varobj(mm), omin(mm), nobs(mm)
	end do

	do i=1,icalml
		itot(i)=i
			read(18012,5) ik,(vobj(kk), kk=1,igoc)
			read(18013,5) ik,(vpar(kk),kk=1,nopt)
		objf=0.
		do mm=1,igoc
		objf=objf+vobj(mm)/(varobj(mm)*2)
		end do
			do j=1,nopt
		    ip=int(m*(vpar(j)-bbound(1,j))/
     *			(bbound(2,j)-bbound(1,j)))+1
	        ip=min(ip,m)
			ipb(j,ip)=ipb(j,ip)+1
	        ipar(i,j)=ip
			end do
	        vtot(i,1)=objf
	    if (objf.lt.objfmin) then
          objfmin=objf
	    bestpar=vpar
	    isim=i
	    end if
          write(18018,18) i, ii,vpar, objf
18     format(2i5,100e12.5)
5     format(1i5,100e12.5)
	end do
	write(18020,5) isim,(bestpar(kk),kk=1,nopt)

	tot=0.
	do i=1,icalml
	    tt=1.
		do  j=1,nopt
		ip=ipar(i,j)
	    t=1.*icalml/m/ipb(j,ip)
		tt=tt*t
		end do
	vtot(i,3)=tt**(1./nopt)
	vtot(i,2)=exp(-vtot(i,1)+objfmin)
		tot=tot+vtot(i,2)*vtot(i,3)
	end do
	call sorteer5(vtot,itot,icalml)
	sortot=0.
	do i=1,icalml
	write(11111,18) i, itot(i), vtot(i,1), vtot(i,2), vtot(i,3)
	sortot=sortot+vtot(i,2)*vtot(i,3)/tot
	
	if (sortot.gt.probab) go to 74
	end do
74	continue
      ivt=itot(i)
	objflim=vtot(i,1)
	end
