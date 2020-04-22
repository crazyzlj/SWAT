      subroutine layersplit(dep_new)

      use parm, except_this_one => layersplit
      integer nly,n,j
	integer :: flag
	real*8, intent(in):: dep_new
	nly = sol_nly(ihru)

!!    create a septic layer
!! changed all sol_zmx(ihru) in subroutine to dep_new 1/27/09 gsm 
      flag = 0
      
      do j = 2, nly 
        xx = 0.
        xx = abs(dep_new - sol_z(j,ihru))
        !! if values are within 10 mm of one another, reset boundary
        if (xx < 10.) then
          sol_z(j,ihru) = dep_new
          exit
        end if

        !! set a soil layer at dep_new and adjust all lower layers
        if (sol_z(j,ihru) > dep_new) then
          flag = 1
          sol_nly(ihru) = sol_nly(ihru) + 1
          nly = nly + 1
          jj = 0
          jj = j + 1
          do n = nly, jj, -1
            sol_z(n,ihru) = sol_z(n-1,ihru)
            sol_bd(n,ihru) = sol_bd(n-1,ihru)
            sol_awc(n,ihru) = sol_awc(n-1,ihru)
            sol_k(n,ihru) = sol_k(n-1,ihru)
            sol_cbn(n,ihru) = sol_cbn(n-1,ihru)
            sol_rock(n,ihru) = sol_rock(n-1,ihru) !!! Armen 13 Jan 2008 MJW rev 490
            sol_clay(n,ihru) = sol_clay(n-1,ihru)
            sol_sand(n,ihru) = sol_sand(n-1,ihru) !!! Claire 2 Dec 2009 MJW rev 490
            sol_silt(n,ihru) = sol_silt(n-1,ihru) !!! Claire 2 Dec 2009 MJW rev 490
            sol_ec(n,ihru) = sol_ec(n-1,ihru)
            sol_no3(n,ihru) = sol_no3(n-1,ihru)
            sol_orgn(n,ihru) = sol_orgn(n-1,ihru)
            sol_orgp(n,ihru) = sol_orgp(n-1,ihru)
            sol_solp(n,ihru) = sol_solp(n-1,ihru)
            sol_mc(n,ihru) = sol_mc(n-1,ihru)
            sol_mn(n,ihru) = sol_mn(n-1,ihru)
            sol_mp(n,ihru) = sol_mp(n-1,ihru)
		    sol_n(n,ihru) = sol_n(n-1,ihru)
		    sol_ph(n,ihru) = sol_ph(n-1,ihru) !! MJW rev 490
		    sol_cal(n,ihru) = sol_cal(n-1,ihru) !! MJW rev 490

          end do
          sol_z(j,ihru) = dep_new
        end if
        if (flag == 1) exit
      end do
	
	iseptic = j 
      end             