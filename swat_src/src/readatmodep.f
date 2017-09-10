      subroutine readatmodep
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the atmospheric deposition values
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    drydep_no3  |kg/ha/yr      |atmospheric dry deposition of nitrates
!!    drydep_nh4  |kg/ha/yr      |atmospheric dry deposition of ammonia
!!    rammo_sub   |mg/l          |atmospheric deposition of ammonium values for
!!                                 entire watershed
!!    rcn_sub     |mg/l          |atmospheric deposition of nitrate for
!!                                 entire watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    eof         |none          |end of file flag (=-1 if eof, else = 0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      character (len=80) :: titldum
      integer :: eof 
  
      eof = 0
  
      if (idaf > 0) then
          id1 = idaf
      else
          id1 = 1
      end if
      
      rcn_sub = rcn_sub_bsn

!!    Atmosperic deposition filename present in file.cio
      if (atmofile /= '             ') then
        open (127,file=atmofile)
          do iii = 1, 5
            read (127,5101) titldum
          end do
      else
      !!    no filename present in file.cio - set defaults
        rammo_sub = 0.
	  rcn_sub = rcn_sub_bsn
      end if
        
      select case (iatmodep)
      case (0)
        do isub = 1, subtot
          read (127,1000,iostat=eof) rammo_sub(isub), rcn_sub(isub),  
     &      drydep_nh4(isub), drydep_no3(isub)
          if (eof < 0) exit
        end do 
        close (127)
      case (1)
        read (127,1001,iostat=eof) mo_atmo1, iyr_atmo1
          iii = 0
          momax = 12 * nbyr
          do iii = 1, msub
            read (127,1002) (rammo_mo(imo,iii),imo = 1,momax)  
            read (127,1002) (rcn_mo(imo,iii), imo = 1,momax)
            read (127,1002) (drydep_nh4_mo(imo,iii), imo = 1, momax)
            read (127,1002) (drydep_no3_mo(imo,iii), imo = 1,momax)
          end do
          close (127)
      case (2)
        read (127,*) matmo  !!maximum number of subbasins
        do
          iyp = 0
          idap = 0
          read (127,*,iostat=eof) iyp, idap
          if (eof < 0) exit
          if (iyp + idap <= 0) exit
          if (iyp == iyr .and. idap == id1) then
            backspace (127)
            exit
          end if
        end do
        
      end select

1001  format (2i6)
1002  format (1200f10.3)   !!!nbs
1000  format (8x,4f10.3)
5101  format (a80)
      return
      end