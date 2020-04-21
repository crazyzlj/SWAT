        subroutine sorteer(arr,len)
        		implicit real*8 (a-h,o-z)
	  integer len, lenn
        real*8 arr(len), help1, help2
        lenn=len
        do ii=1,len
         lenn=lenn-1
         help1=arr(1)
          do jj=1,lenn
            help1=arr(jj)
            help2=arr(jj+1)
            if (help2.gt.help1) then
            arr(jj)=help2
            arr(jj+1)=help1
          end if
          end do
          end do
         return
	 end
