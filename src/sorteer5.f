        subroutine sorteer5(vtot,itot,len)
c	sorts an integer and real matrix according to the real matrix
c	written by Ann van Griensven - University of California Riverside, april 2003

        integer len, itot(len),lenn,ihelp1,ihelp2
        real*8 vtot(len,3), help1(3), help2(3)
        lenn=len
        do ii=1,len
         lenn=lenn-1
           do jj=1,lenn
	      ihelp1=itot(jj)
            ihelp2=itot(jj+1)
		  do n=1,3
		  help1(n)=vtot(jj,n)
            help2(n)=vtot(jj+1,n)
	      end do
            if (help2(1).lt.help1(1)) then
		   do n=1,3
             vtot(jj,n)=help2(n)
    	       vtot(jj+1,n)=help1(n)	
		  end do
		  itot(jj)=ihelp2
	      itot(jj+1)=ihelp1
          end if
          end do
          end do
         return
         end
