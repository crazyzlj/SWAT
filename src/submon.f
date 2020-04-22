      subroutine submon

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes monthly subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub)
!!    msubo         |none          |max number of variables in output.sub file
!!    mo_chk        |none          |current month of simulation
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
!!    submono(1,:)  |mm H2O        |precipitation in subbasin for month
!!    submono(2,:)  |mm H2O        |snow melt in subbasin for month
!!    submono(3,:)  |mm H2O        |surface runoff loading in subbasin for month
!!    submono(4,:)  |mm H2O        |water yield from subbasin for month
!!    submono(5,:)  |mm H2O        |potential evapotranspiration in subbasin for
!!                                 |month
!!    submono(6,:)  |mm H2O        |actual evapotranspiration in subbasin for
!!                                 |month
!!    submono(7,:)  |metric tons/ha|sediment yield from subbasin for month
!!    submono(8,:)  |kg N/ha       |organic N loading from subbasin for month
!!    submono(9,:)  |kg P/ha       |organic P loading from subbasin for month
!!    submono(10,:) |kg N/ha       |NO3 loading from surface runoff in subbasin
!!                                 |for month
!!    submono(11,:) |kg P/ha       |soluble P loading from subbasin for month
!!    submono(12,:) |mm H2O        |groundwater loading from subbasin for month
!!    submono(13,:) |mm H2O        |percolation out of soil profile in subbasin
!!                                 |for month
!!    submono(14,:) |kg P/ha       |loading to reach of mineral P attached to
!!                                 |sediment from subbasin for month
!!    subtot        |none          |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    pdvab(:)    |varies        |array to hold subbasin output values
!!    pdvb(:)     |varies        |array of user selected subbasin output
!!                               |values
!!    sb          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: sb, ii
      real*8, dimension (msubo) :: pdvab, pdvb

      do sb = 1, subtot

        pdvab = 0.
        pdvb = 0.
  
        pdvab(1) = submono(1,sb)
        pdvab(2) = submono(2,sb)
        pdvab(3) = submono(5,sb)
        pdvab(4) = submono(6,sb)
        pdvab(5) = sub_sw(sb)
        pdvab(6) = submono(13,sb)
        pdvab(7) = submono(3,sb)
        pdvab(8) = submono(12,sb)
        pdvab(9) = submono(4,sb)
        pdvab(10) = submono(7,sb)
        pdvab(11) = submono(8,sb)
        pdvab(12) = submono(9,sb)
        pdvab(13) = submono(10,sb)
        pdvab(14) = submono(11,sb)
        pdvab(15) = submono(14,sb)
        pdvab(16) = submono(15,sb)
        pdvab(17) = submono(16,sb)
        pdvab(18) = submono(17,sb)
!!      chl_a, cbodu and doxq were all written out in daily
!!      output code.  not set up for monthly/yearly
!!      all values will be zero for all codes except daily
!!      added for jennifer b.
        pdvab(19) = 0.0
        pdvab(20) = 0.0
        pdvab(21) = 0.0
        pdvab(22) = submono(19,sb)  !! tile_no3
        pdvab(23) = submono(18,sb)  !! qtile   jane f.
        pdvab(24) = submono(20,sb)  !! phos due to crack flow

        if (ipdvab(1) > 0) then
          do ii = 1, itotb
            pdvb(ii) = pdvab(ipdvab(ii))
          end do
          write (31,1000) sb, subgis(sb), mo_chk, sub_km(sb),           
     &                                    (pdvb(ii), ii = 1, itotb)
        else
          write (31,1000) sb, subgis(sb), mo_chk, sub_km(sb),           
     &                                    (pdvab(ii), ii = 1, msubo)
        end if
      end do

      return
!1000 format ('BIGSUB',i4,1x,i8,1x,i4,e10.5,21f10.3)
 1000 format ('BIGSUB',i5,1x,i8,1x,i4,e10.5,18f10.3,1x,e10.5,5e10.3) 
      end 