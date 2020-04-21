      subroutine telobjresp(iobj,isens)
!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)

      integer isens
      integer iobj
      integer irde(4)
		



c
c  READ THE OBJECTIVE FUNCTIONS CONTROL PARAMETERS

        do mm=1,100
!     !  !         !         !         !         !         !         ! !
      read(18016,997, end=996) (irde(ii),ii=1,4),clw 
        if (irde(1).le.0) go to 996
        end do
996     continue
        iobj=mm-1
	


        do mm=1,100
!     !  !         !         !         !         !         !         ! !
      read(18017,997, end=998) (irde(ii),ii=1,4),clw 
        if (irde(1).le.0) go to 998
        end do
998     continue
        isens=mm-1

997   format(4i3, f8.3)	



      return
      end
