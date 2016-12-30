      subroutine rsedaa(years)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the annual reach output to the .sed file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rch_dakm(:)  |km**2        |total drainage area contributing to flow at
!!                               |the outlet (pour point) of the reach in
!!                               |square kilometers
!!    subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
!!    subtot       |none         |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter (reach number)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j

      do j = 1, subtot
          write (84,5000) j, subgis(j), years, rch_dakm(j),             
     &       rchaao(3,j), rchaao(4,j),(rchaao(ii,j),ii=42,58)
      end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,f5.1,20e12.4)
      end