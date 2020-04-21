      subroutine sorteer6(arr,ind,len)

      integer len, lenn, ind(len), ihelp1, ihelp2
       real*8 arr(len), help1, help2
	do ii=1,len
	ind(ii)=ii
	end do
	  lenn=len
        do ii=1,len
         lenn=lenn-1
         help1=arr(1)
	 ihelp1=ind(1)
          do jj=1,lenn
		  ihelp1=ind(jj)
	      ihelp2=ind(jj+1)
            help1=arr(jj)
            help2=arr(jj+1)
            if (help2.lt.help1) then
            arr(jj)=help2
            arr(jj+1)=help1
	      ind(jj)=ihelp2
	      ind(jj+1)=ihelp1
          end if
          end do
          end do

c  END OF SUBROUTINE SORT
      return
      end
