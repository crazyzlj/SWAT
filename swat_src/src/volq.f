      subroutine volq

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Call subroutines to calculate the current day's CN for the HRU and
!!    to calculate surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: surq_daycn, surq_breakcn, surq_greenampt, dir_rnff
!!    SWAT: surq_hourly

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm


!! Compute surface runoff for day
      select case (ievent)
        case (0)
          call surq_daycn
        case (1)
          call surq_greenampt
        !  call dir_rnff
        !case (3)
        !  call surq_hourly
      end select

      return
      end