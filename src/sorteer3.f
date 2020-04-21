        subroutine sorteer3(ari, arr,len,irank,ira)
        integer len, ira(len), irank(len)
	  real*8 arr(len),ari(len), help1, help2
	
	arr=ari
	  lenn=len
	  do ii=1,len
	  ira(ii)=ii
	  end do
        do ii=1,len
         lenn=lenn-1
         help1=arr(1)
          do jj=1,lenn
            help1=arr(jj)
            help2=arr(jj+1)
            if (help2.gt.help1) then
            arr(jj)=help2
            arr(jj+1)=help1
		  it=ira(jj)			
	      ira(jj)=ira(jj+1)
	      ira(jj+1)=it
          end if
          end do
          end do

		do ii=1,len 
		irank(ira(ii))=ii
	    END DO
         return
         end
