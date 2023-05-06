      subroutine temparms
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine summarizes data for subbasins with multiple HRUs and
!!    prints the daily output.hru file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      
      jrch = inum1

!! check for month to input water temperature parms
      if (i_mo == 12 .or. i_mo == 1 .or. i_mo == 2) then
        wtmp = tmp_win1(jrch) + tmp_win2(jrch) * tmpav(jrch)
      end if 
      
      if (i_mo == 3 .or. i_mo == 4 .or. i_mo == 5) then
        wtmp = tmp_spr1(jrch) + tmp_spr2(jrch) * tmpav(jrch)
      end if  
      
      if (i_mo == 6 .or. i_mo == 7 .or. i_mo == 8) then
        wtmp = tmp_sum1(jrch) + tmp_sum2(jrch) * tmpav(jrch)
      end if  
      
      if (i_mo == 9 .or. i_mo == 10 .or. i_mo == 11) then
        wtmp = tmp_fal1(jrch) + tmp_fal2(jrch) * tmpav(jrch)
      end if 
      
      !if (wtmp > 30.) then
      !  xx = 0.
      !end if
      
      return   
      end