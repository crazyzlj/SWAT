      subroutine sw_init

      use parm
      
      integer :: ly
      real :: dep_prev
      character (len=80) :: titldum
      character (len=80) :: header
     
      open (1234,file='sw_data.in')
      open (1235,file='sw_data.out')
      
       write (1235,100)                                                                                     
 100   format (2x,'HRU',8x,'SOIL LAYER - SOL_ST(mm)',/, 12x,'    
     &           1           2           3           4           5
     &           6           7           8           9          10')
      
      read (1234,*) titldum
      read (1234,*) header

      do j = 1, nhru
        read (1234,*) k, sw_up10, sw_lo10
        dep_prev = 0.
          do ly = 1, sol_nly(j)
            if (dep_prev < 100. .and. sol_z(ly,j) < 100.) then
              sol_st(ly,j) = sw_up10 * sol_ul(ly,j)
            end if
            if (dep_prev < 100. .and. sol_z(ly,j) > 100.) then
              thick = sol_z(ly,j) - dep_prev
              sw_up = (100. - dep_prev) / thick * sol_ul(ly,j)
              sw_lo = (sol_z(ly,j) - 100.) / thick * sol_ul(ly,j)
              sol_st(ly,j) = sw_up10 * sw_up + sw_lo10 * sw_lo
            end if
            if (dep_prev > 100. .and. sol_z(ly,j) > 100.) then
              sol_st(ly,j) = sw_lo10 * sol_ul(ly,j)
            end if
            dep_prev = sol_z(ly,j)
          end do
          write (1235,101) j, (sol_st(ly,j), ly = 1, sol_nly(j))
      end do
      
101   format (1x,i4,10f12.3)
      close (1234)
      close (1235)
      
      return
      end