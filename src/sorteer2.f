      subroutine sorteer2(arr,len)
         integer len
         real*8 arr(len),help1,help2

         lenn=len
         do ii=1,len
           lenn=lenn-1
           help1=arr(1)
           do jj=1,lenn
            help1=arr(jj)
            help2=arr(jj+1)
          if (help2.lt.help1) then
            arr(jj)=help2
            arr(jj+1)=help1
         end if
         end do
        end do
        return
        end
