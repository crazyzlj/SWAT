      subroutine smeas
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads in daily solar radiation data and assigns the
!!    values to the proper HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    id1         |julian date   |first day of simulation in current year
!!    ifirsts     |none          |solar radiation data search code
!!                               |0 first day of solar radiation data located in 
!!                               |  file
!!                               |1 first day of solar radiation data not
!!                               |  located in file
!!    isgage(:)   |none          |HRU solar radiation data code (record # for
!!                               |solar radiation used in HRU)
!!    iyr         |none          |beginning year of simulation
!!    mrg         |none          |maximum number of rainfall/temp gages
!!    nhru        |none          |number of HRUs in watershed
!!    nstot       |none          |total number of solar radiation records
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    ifirsts     |none          |solar radiation data search code
!!                               |0 first day of solar radiation data located in 
!!                               |  file
!!                               |1 first day of solar radiation data not
!!                               |  located in file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idap        |julian date   |julian date of measured weather data
!!    inum3sprev  |none          |subbasin number of previous HRU
!!    iyp         |none          |last 2 digits of year measured weather data
!!    k           |none          |counter
!!    l           |none          |counter
!!    rabsb       |MJ/m^2        |generated solar radiation for subbasin
!!    slrmeas(:)  |MJ/m^2        |solar radiation read in from file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT subroutines: clgen, slrgen

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: k, iyp, idap, l, inum3sprev
      real :: rabsb
      real, dimension (mrg) :: slrmeas

      !! initialize variables for the day
      slrmeas = 0.
      

        !! read solar radiation data from file
          if (ifirsts == 0) then
            read (137,5200) (slrmeas(l), l = 1, nstot)
          else
            ifirsts = 0
            do
              iyp = 0
              idap = 0
              read (137,5300) iyp, idap, (slrmeas(l), l = 1, nstot)
              if (iyp + idap <= 0) exit
              if (iyp == iyr .and. idap == id1) exit
            end do
          end if

        !! assign solar radiation data to HRUs
        inum3sprev = 0
        do k = 1, nhru
          call clgen(k)
          hru_ra(k) = slrmeas(isgage(hru_sub(k)))
          !! generate values to replace missing data
          if (hru_ra(k) <  -97.) then
            !! use same generated data for all HRUs in a subbasin
            if (hru_sub(k) == inum3sprev .and. hru_sub(k) /= 0) then
              hru_ra(k) = rabsb
            else
              call slrgen(k)
              !! set subbasin generated values
              inum3sprev = 0
              rabsb = 0.
              inum3sprev = hru_sub(k)
              rabsb = hru_ra(k)
            end if
          end if
        end do

      return
 5200 format (7x,300f8.3)
 5300 format (i4,i3,300f8.3)
      end
