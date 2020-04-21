       subroutine functn(xx,nopt, iiname, inrhru, iinr, icalml,iobj,
     *	 icalpar, isens, isenspar, sensw,obj, imet,icd)

      real*8 bbound(2,nopt), calw(iobj), sensw(isens), funcv, xx(nopt)
	real*8 obj(iobj)
	integer iinr(nopt), iiname(nopt), isens,iof, icalml
      integer inrhru(nopt,2000), iobj, imet(nopt)
      integer icalpar(4,iobj), nopt, isenspar(4,isens)
	character*7 swatfilen
	ipr=0
c
		write(18013, 8013) icalml, (xx(ii), ii=1,nopt)
8013    format(i5, 100e12.5)
      call rerun(xx, nopt,iiname, inrhru, iinr, icalml, imet,ipr)
      call objfunctn(obj, iobj, icalpar, icalml,icd)
	call response(sensw, isens, isenspar, icalml)

c	read(19030, 7,end=9) swatfilen
c	open (18019,file=swatfilen)
c	call writeswatfile
c	close (18019)
9	continue
	
7	format(a7)
	return
      end
