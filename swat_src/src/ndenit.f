      subroutine ndenit(k,j,cdg,wdn,void)
!!    this subroutine computes denitrification 

	use parm
	integer :: k,j
	real :: cdg, wdn, void

      wdn = 0.
	vof = 1. / (1. + (void/0.04)**5)
	wdn = sol_no3(k,j) * (1. - Exp(-cdn(j) * cdg * vof * sol_cbn(k,j)))
	sol_no3(k,j) = sol_no3(k,j) - wdn

	return
	end