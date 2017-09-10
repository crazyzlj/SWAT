      subroutine pkq(iwave)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the peak runoff rate for each HRU
!!    and the entire subbasin using a modification of the rational formula

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    al5          |none          |fraction of daily rainfall that occurs
!!                                |during 0.5h highest intensity
!!    hru_km(:)    |km^2          |area of HRU in square kilometers
!!    ihru         |none          |HRU number
!!    iwave        |none          |flag to differentiate calculation of HRU and
!!                                |subbasin sediment calculation
!!                                |iwave = 0 for HRU
!!                                |iwave = subbasin # for subbasin
!!    qday         |mm H2O        |surface runoff that reaches main channel
!!                                |during day in HRU
!!    sub_km(:)    |km^2          |area of subbasin in square kilometers
!!    sub_tc(:)    |hr            |time of concentration for subbasin
!!    tconc(:)     |hr            |time of concentration for HRU
!!    sub_qd(:)    |mm H2O        |surface runoff that reaches main channel
!!                                |during day in subbasin
!!    sub_tran(:)  |mm H2O        |transmission losses on day in subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    peakr        |m^3/s         |peak runoff rate
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    altc         |
!!    j            |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Expo

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: iwave
      integer :: j
      real :: altc

      j = 0
      j = ihru

      altc = 0.

      if (iwave > 0) then
        !! subbasin sediment calculations
        altc = 1. - Expo(2. * sub_tc(iwave) * Log(1. - al5))
        peakr = altc * (sub_qd(iwave) + sub_tran(iwave)) / sub_tc(iwave) !! mm/h
        peakr = peakr * sub_km(iwave) / 3.6                              !! m^3/s
      else
        !! HRU sediment calculations
        altc = 1. - Expo(2. * tconc(j) * Log(1. - al5))
        peakr = altc * qday / tconc(j)           !! mm/h
        peakr = peakr * hru_km(j) / 3.6          !! m^3/s
      end if

      return
      end