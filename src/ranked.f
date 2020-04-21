      subroutine ranked(itel,hh)

	implicit real*8 (a-h,o-z)
      integer itel
       real*8 hh(3)
       real*8 caldat(itel), calout(itel)
	rewind(7778)
        do ii=1,itel
			read(7778, 78) cal1,cal2
78    format(2e12.5)
        caldat(ii)=cal1
        calout(ii)=cal2

        end do
        call sorteer (caldat,itel)
        call sorteer (calout,itel)
        hh(1)=0.
        hh(2)=0.
        hh(3)=0.

        do ii=1,itel
        hh(1)=hh(1)+(calout(ii)-caldat(ii))**2
        if (calout(ii).le.0.001) calout(ii)=0.001
        if (caldat(ii).le.001) caldat(ii)=0.001
        hh(2)=hh(2)+(log(calout(ii))-log(caldat(ii)))**2
        hh(3)=hh(3)+(sqrt(calout(ii))-sqrt(caldat(ii)))**2
        end do
        return
        end
