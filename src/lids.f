      subroutine lids(sb,j,k,lid_prec)

      use parm
      implicit none
      
      integer :: jj, sb, j, k
!      real :: lid_farea,lid_prec,lid_cumr
      real :: lid_prec, fr
      
      jj = urblu(j)
      
      lid_farea = 0.
      lid_farea_sum = 0.
      lid_qsurf_total = 0.
      lid_sw_add = 0.
      fr = 0.
      
      if (gr_onoff(sb,jj)==1) then
        if (jj==14 .or. jj==18) then
          fr = 0.80
        else if (jj==19 .or. jj==20 .or. jj==21 .or. jj==24) then
          fr = 0.40
        end if
        lid_farea(j,1) = gr_farea(sb,jj) * fr ! assuming fractions of roof area to impervious area is 0.80 and 0.40 for regidential and commercial/industrial, respectively
        call lid_greenroof(sb,j,k,lid_prec)
      end if
      
      if (rg_onoff(sb,jj)==1) then
        lid_farea(j,2) = rg_farea(sb,jj)
        call lid_raingarden(sb,j,k,lid_prec)
      end if
      
      if (cs_onoff(sb,jj)==1) then
        if (jj==14 .or. jj==18) then
          fr = 0.80
        else if (jj==19 .or. jj==20 .or. jj==21 .or. jj==24) then
          fr = 0.40
        end if
        if (cs_grcon(sb,jj)==0) then
          lid_farea(j,3) = cs_farea(sb,jj)
          call lid_cistern(sb,j,k,lid_prec)
        else
          lid_farea(j,3) = gr_farea(sb,jj) * fr ! assuming fractions of roof area to impervious area is 0.80 and 0.40 for regidential and commercial/industrial, respectively
          call lid_cistern(sb,j,k,lid_qsurf(j,1))
          lid_qsurf(j,1) = 0.
        end if
      end if
      
      if (pv_onoff(sb,jj)==1) then
        if (jj==14 .or. jj==18) then
          fr = 0.05
        else if (jj==19 .or. jj==20 .or. jj==21 .or. jj==24 .or.
     &  jj==26 .or. jj==30 .or. jj==31) then
          fr = 0.40
        end if
        lid_farea(j,4) = pv_farea(sb,jj) * fr
        call lid_porpavement(sb,j,k,lid_prec)
      end if
      
      return
      end subroutine