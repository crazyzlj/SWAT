      subroutine h2omgt_init 

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine initializes variables related to water management
!!    (irrigation, consumptive water use, etc.)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    divmax(:)   |mm H2O or     |maximum daily irrigation diversion from the
!!                |   10**4 m H2O|reach (when IRR=3): when value is positive,
!!                               |the units are mm H2O; when the value is
!!                               |negative, the units are (10**4 m H2O)
!!    hru_fr(:)   |km2/km2       |fraction of subbasin area contained in HRU
!!    nhru        |none          |number of HRUs in watershed
!!    sol_sumfc(:)|mm H2O        |amount of water held in soil profile at
!!                               |field capacity
!!    sub_fr(:)   |km2/km2       |fraction of total watershed area contained
!!                               |in subbasin
!!    wudeep(:,:) |10^4 m^3/day  |average daily water removal from the deep
!!                               |aquifer for the month
!!    wupnd(:,:)  |10^4 m^3/day  |average daily water removal from the pond
!!                               |for the month
!!    wushal(:,:) |10^4 m^3/day  |average daily water removal from the shallow
!!                               |aquifer for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    divmax(:)   |mm H2O or     |maximum daily irrigation diversion from the
!!                |   10**4 m H2O|reach (when IRR=3): when value is positive,
!!                               |the units are mm H2O; when the value is
!!                               |negative, the units are (10**4 m H2O)
!!    wudeep(:,:) |10^4 m^3/day  |average daily water removal from the deep
!!                               |aquifer for the month for the HRU within the
!!                               |subbasin
!!    wupnd(:,:)  |10^4 m^3/day  |average daily water removal from the pond
!!                               |for the month for the HRU within the subbasin
!!    wushal(:,:) |10^4 m^3/day  |average daily water removal from the shallow
!!                               |aquifer for the month for the HRU within the
!!                               |subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    j           |none          |counter
!!    mon         |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer :: j, mon


!!    partition subbasin consumptive water use between HRUs within subbasin
      do j = 1, nhru
        do mon = 1,12
          wupnd(mon,j) = wupnd(mon,j) * hru_fr(j) 
          wushal(mon,j) = wushal(mon,j) * hru_fr(j) 
          wudeep(mon,j) = wudeep(mon,j) * hru_fr(j) 
        end do
      end do

!!    initialize maximum irrigation diversion if user did not enter
      do j = 1, nhru
        if (divmax(j) < 1.e-6 .and. divmax(j) > -1.e-6) then
          divmax(j) = sol_sumfc(j)
        endif
      end do

      return
      end
