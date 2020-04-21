      subroutine readru
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the sub input file (.sub).
!!    This file contains data related to routing .

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definitionov
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    da_ru       |ha            |area of routing unit
!!    ovsl_ru     |(m)           |average slope length
!!    ovs_ru      |(m)           |average slope steepness
!!    ovn_ru      |              |Manning's N value overland flow
!!    chl_ru      |(km)          |channel length
!!    chs_ru      |(m/m)         |ave slope
!!    chw_ru      |(mm/km)       |ave width
!!    chk_ru      |(mm/hr)       |eff hydr cond
!!    chn_ru      |              |Manning's N tributary channels
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
      
      character (len=80) :: titldum
      integer :: eof
      
      eof = 0
	do
        read (113,5000,iostat=eof) titldum
        if (eof < 0) exit
        read (113,*,iostat=eof) da_ru
        if (eof < 0) exit
        read (113,*,iostat=eof) ovsl_ru
        if (eof < 0) exit
        read (113,*,iostat=eof) ovs_ru
        if (eof < 0) exit
        read (113,*,iostat=eof) ovn_ru
        if (eof < 0) exit
        read (113,*,iostat=eof) chl_ru
        if (eof < 0) exit
        read (113,*,iostat=eof) chs_ru
        if (eof < 0) exit
        read (113,*,iostat=eof) chw_ru
        if (eof < 0) exit
        read (113,*,iostat=eof) chk_ru
        if (eof < 0) exit
        read (113,*,iostat=eof) chn_ru
        if (eof < 0) exit
        exit
      end do
      
      do j = 1, hrutot(i)
	  read (113,*) ix, hru_rufr(iru,j)
      end do
      
5000  format (a)     
	return
      end
