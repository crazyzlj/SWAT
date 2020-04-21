      subroutine tstr
      
!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     computes temperature stress for crop growth - strstmp

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    idplt(:)    |none          |land cover code from crop.dat
!!    ihru        |none          |HRU number
!!    nro(:)      |none          |sequence number of year in rotation
!!    t_base(:)   |deg C         |minimum temperature for plant growth
!!    t_opt(:)    |deg C         |optimal temperature for plant growth
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmp_an(:)   |deg C         |average annual air temperature
!!    tmpav(:)    |deg C         |average air temperature on current day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    strstmp(:)  |none          |fraction of potential plant growth achieved on
!!                               |the day in HRU where the reduction is caused
!!                               |by temperature stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    rto         |
!!    tgx         |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real :: tgx, rto

      j = 0
      j = ihru

      tgx = 0.
      tgx = tmpav(j) - t_base(idplt(j))

      if (tgx <= 0.) then
        strstmp(j) = 0.
      else
        if (tmpav(j) > t_opt(idplt(j))) then
         tgx = 2. * t_opt(idplt(j)) -                                   &
     &                         t_base(idplt(j)) - tmpav(j)
        end if

        rto = 0.
        rto = ((t_opt(idplt(j)) - tmpav(j)) /                           &
     &                                               (tgx + 1.e-6)) ** 2

        if (rto <= 200. .and. tgx > 0.) then
          strstmp(j) = Exp(-0.1054 * rto)
        else
          strstmp(j) = 0.
        end if

        if (tmn(j) <= tmp_an(hru_sub(j)) - 15.) strstmp(j) = 0.

      end if

      return
      end
