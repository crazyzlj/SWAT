      subroutine rsedmon(mdays)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the monthly reach output to the .sed file

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
          rchmono(58,j) = rchmono(58,j)/Real(mdays)                     
          write (84,5000) j, subgis(j), mo_chk, rch_dakm(j),            
     &       rchmono(3,j), rchmono(4,j),(rchmono(ii,j),ii=42,58)
      end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,i5,20e12.4)
      end