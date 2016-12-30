      subroutine soil_write

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes output to the output.sol file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, l
      real :: solp_t, solno3_t, solorgn_t, solorgp_t

      do j = 1,nhru
        solp_t = 0.
	  solno3_t = 0.
	  solorgn_t = 0.
	  solorgp_t = 0.
         do l = 1,sol_nly(j)
           solp_t = solp_t + sol_solp(l,j)
           solno3_t = solno3_t + sol_no3(l,j)
	     !if (cswat == 0) then
		   !	solorgn_t = solorgn_t + sol_orgn(l,j)
	     !else
		   !	solorgn_t = solorgn_t + sol_n(l,j)
		   !end if
		   
		   !!By Zhang
		   !!============
	     if (cswat == 0) then
			solorgn_t = solorgn_t + sol_orgn(l,j)
	     end if
	     if (cswat == 1) then
			solorgn_t = solorgn_t + sol_n(l,j)
		   end if		   
		   if (cswat ==2) then
		    solorgn_t = solorgn_t + sol_HSN(l,j) + sol_HPN(l,j)
		   end if
		   !!By Zhang
		   !!============		   
		   
           solorgp_t = solorgp_t + sol_orgp(l,j)
         end do
         write (121,1000) i, subnum(j), hruno(j), sol_rsd(1,j), solp_t, 
     &    solno3_t, solorgn_t, solorgp_t, cnday(j)
      end do
      
      return
 1000 format ('SNU   ',i4,1x,a5,a4,1x,6f10.2)
      end