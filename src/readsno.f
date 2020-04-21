      subroutine readsno

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine reads snow data from the HRU/subbasin soil chemical input

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!                               |watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    sub_sftmp    |deg C         |Snowfall temperature
!!                                |Mean air temperature at which precipitation
!!                                |is equally likely to be rain as snow/freezing
!!                                |rain.
!!    sub_smfmn    |mm/deg C/day  |Minimum melt rate for snow during year (Dec.
!!                                |21) where deg C refers to the air temperature
!!    sub_smfmx    |mm/deg C/day  |Maximum melt rate for snow during year (June
!!                                |21) where deg C refers to the air temperature
!!                                |SMFMX and SMFMN allow the rate of snow melt
!!                                |to vary through the year. These parameters
!!                                |are accounting for the impact of soil
!!                                |temperature on snow melt.
!!    sub_smtmp    |deg C         |Snow melt base temperature
!!                                |Mean air temperature at which snow melt will 
!!                                |occur.
!!    sub_timp     |none          |snow pack temperature lag factor (0-1)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)    |none          |pesticide use flag:
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
      integer :: eof
      eof = 0
      
      do
        read (113,1001) titldum
        if (eof < 0) exit
        read (113,1000,iostat=eof) (sub_sftmp(ib,i), ib = 1, 10)
        if (eof < 0) exit
        read (113,1000,iostat=eof) (sub_smtmp(ib,i), ib = 1, 10)
        if (eof < 0) exit
        read (113,1000,iostat=eof) (sub_smfmx(ib,i), ib = 1, 10)
        if (eof < 0) exit
        read (113,1000,iostat=eof) (sub_smfmn(ib,i), ib = 1, 10)
        if (eof < 0) exit
        read (113,1000,iostat=eof) (sub_timp(ib,i), ib = 1, 10)
        if (eof < 0) exit
        exit 
      end do
      
      do ib = 1, 10
          if (sub_sftmp(ib,i) <= 0.) sub_sftmp(ib,i) = sftmp
          if (sub_smtmp(ib,i) <= 0.) sub_smtmp(ib,i) = smtmp
          if (sub_smfmx(ib,i) <= 0.) sub_smfmx(ib,i) = smfmx
          if (sub_smfmn(ib,i) <= 0.) sub_smfmn(ib,i) = smfmn
          if (sub_timp(ib,i) <= 0.) sub_timp(ib,i) = timp
      end do

      close (113)
        
      return
 1000 format (10f8.3)
 1001 format (a)
      end
