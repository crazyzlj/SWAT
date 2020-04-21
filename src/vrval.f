        subroutine vrval(itel,ave,varv)
		implicit real*8 (a-h,o-z)

        integer itel
        real*8  ave, cal1, varv
        varv=0.
     	rewind(7778)
        do ii=1,itel
			read(7778, 78) cal1
78    format(2e12.5)
	      varv=varv+(cal1-ave)**2
        end do
        varv=varv/itel
        return
        end
