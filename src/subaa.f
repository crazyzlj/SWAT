      subroutine subaa(years)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes average annual subbasin output to the output.sub file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ipdvab(:)     |none          |output variable codes for output.sub file
!!    itotb         |none          |number of output variables printed (output.sub)
!!    msubo         |none          |max number of variables in output.sub file
!!    sub_km(:)     |km^2          |area of subbasin in square kilometers
!!    sub_sw(:)     |mm H2O        |water in soil profile in subbasin
!!    subaao(1,:)   |mm H2O        |precipitation in subbasin for simulation
!!    subaao(2,:)   |mm H2O        |snow melt in subbasin for simulation
!!    subaao(3,:)   |mm H2O        |surface runoff loading in subbasin for 
!!                                 |simulation
!!    subaao(4,:)   |mm H2O        |water yield from subbasin for simulation
!!    subaao(5,:)   |mm H2O        |potential evapotranspiration in subbasin for
!!                                 |simulation
!!    subaao(6,:)   |mm H2O        |actual evapotranspiration in subbasin for
!!                                 |simulation
!!    subaao(7,:)   |metric tons/ha|sediment yield from subbasin for simulation
!!    subaao(8,:)   |kg N/ha       |organic N loading from subbasin for 
!!                                 |simulation
!!    subaao(9,:)   |kg P/ha       |organic P loading from subbasin for
!!                                 |simulation
!!    subaao(10,:)  |kg N/ha       |NO3 loading from surface runoff in subbasin
!!                                 |for simulation
!!    subaao(11,:)  |kg P/ha       |soluble P loading from subbasin for 
!!                                 |simulation
!!    subaao(12,:)  |mm H2O        |groundwater loading from subbasin for 
!!                                 |simulation
!!    subaao(13,:)  |mm H2O        |percolation out of soil profile in subbasin
!!                                 |for simulation
!!    subaao(14,:)  |kg P/ha       |loading to reach of mineral P attached to
!!                                 |sediment from subbasin for simulation
!!    subaao(18,i)  |              |groundwater***?
!!    subgis(:)     |none          |GIS code printed to output files(output.sub)
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
!!    years       |years         |length of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real, intent (in) :: years
      integer :: sb, ii
      real, dimension (msubo) :: pdvab, pdvb

      do sb = 1, subtot

        pdvab = 0.
        pdvb = 0.
  
        pdvab(1) = subaao(1,sb)
        pdvab(2) = subaao(2,sb)
        pdvab(3) = subaao(5,sb)
        pdvab(4) = subaao(6,sb)
        pdvab(5) = sub_sw(sb)
        pdvab(6) = subaao(13,sb)
        pdvab(7) = subaao(3,sb)
        pdvab(8) = subaao(12,sb)
        pdvab(9) = subaao(4,sb)
        pdvab(10) = subaao(7,sb)
        pdvab(11) = subaao(8,sb)
        pdvab(12) = subaao(9,sb)
        pdvab(13) = subaao(10,sb)
        pdvab(14) = subaao(11,sb)
        pdvab(15) = subaao(14,sb)
        pdvab(16) = subaao(15,sb)
        pdvab(17) = subaao(16,sb)
        pdvab(18) = subaao(17,sb)
!!      chl_a, cbodu and doxq were all written out in daily
!!      output code.  not set up for monthly/yearly
!!      all values will be zero for all codes except daily
!!      added for jennifer b.
        pdvab(19) = 0.0
        pdvab(20) = 0.0
        pdvab(21) = 0.0
        pdvab(22) = subaao(18,sb)   !!tile_no3


        if (ipdvab(1) > 0) then
          do ii = 1, itotb
            pdvb(ii) = pdvab(ipdvab(ii))
          end do
          write (31,1000) sb, subgis(sb), years, sub_km(sb),            &
     &                                         (pdvb(ii), ii = 1, itotb)
        else
          write (31,1000) sb, subgis(sb), years, sub_km(sb),            &
     &                                        (pdvab(ii), ii = 1, msubo)
        end if
      end do

      return
!1000 format ('BIGSUB',i4,1x,i8,1x,f4.1,e10.5,21f10.3)
 1000 format ('BIGSUB',i4,1x,i8,1x,f4.1,e10.5,18f10.3,1x,e10.5,3e10.3)
      end 
