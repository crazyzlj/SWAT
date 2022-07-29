      subroutine lids(sb,j,k,kk,lid_prec)
!!    kk               |none          |LID index in *.lid files

      use parm
      implicit none
      
      integer :: sb, j, k,kk
      real*8 :: lid_prec
      
      lid_farea = 0.
      lid_farea_sum = 0.
      lid_qsurf_total = 0.
      lid_sw_add = 0.
      
      if (gr_onoff(sb,kk)==1) then
        lid_farea(j,1) = gr_farea(sb,kk) 
        call lid_greenroof(sb,j,k,kk,lid_prec)
      end if
      
      if (rg_onoff(sb,kk)==1) then
        lid_farea(j,2) = rg_farea(sb,kk)
        call lid_raingarden(sb,j,k,kk,lid_prec)
      end if
      
      if (cs_onoff(sb,kk)==1) then
	  lid_farea(j,3) = cs_farea(sb,kk)
        if (cs_grcon(sb,kk)==0) then
          call lid_cistern(sb,j,k,kk,lid_prec)
        else
          call lid_cistern(sb,j,k,kk,lid_qsurf(j,1))
          lid_qsurf(j,1) = 0.
        end if
      end if
      
      if (pv_onoff(sb,kk)==1) then
        lid_farea(j,4) = pv_farea(sb,kk) 
        call lid_porpavement(sb,j,k,kk,lid_prec)
      end if
      
      return
      end subroutine