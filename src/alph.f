      subroutine alph(iwave)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes alpha, a dimensionless parameter that
!!    expresses the fraction of total rainfall that occurs during 0.5h

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    amp_r(:,:)  |none        |alpha factor for rain(mo max 0.5h rain)
!!    idg(:)      |none        |array location of random number seed
!!                             |used for a given process
!!    idt         |minutes     |length of time step used to report
!!                             |precipitation data for sub-daily modeling
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ihru        |none        |HRU number
!!    iwave       |none        |flag to differentiate calculation of HRU and
!!                             |subbasin sediment calculation
!!                             |iwave = 0 for HRU
!!                                      MUSLE(sedyld) each hru is calculated
!!                             |        independently using hru area and 
!!                             |        adjusted channel length
!!                             |iwave = 1 subbasin # for subbasin
!!                                      MUSLE is computed for entire subbasin
!!                             |        using hru weighted KLSCP
!!    i_mo        |none        |month being simulated
!!    nstep       |none        |number of lines of rainfall data for each
!!                             |day
!!    ovrlnd(:)   |mm H2O      |overland flow onto HRU from upstream
!!                             |routing unit
!!    precipday   |mm H2O      |amount of water reaching soil surface in HRU
!!    precipdt(:) |mm H2O      |precipitation in time step for HRU
!!    rndseed(:,:)|none        |random number generator seed
!!    snomlt      |mm H2O      |amount of snow melt in HRU on current day
!!    sub_precip(:)|mm H2O      |amount of water reaching soil surface in
!!                             |subbasin
!!    sub_snom(:) |mm H2O      |amount of snow melt in subbasin on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    al5         |none        |fraction of total rainfall on day that occurs
!!                             |during 0.5h highest intensity rainfall
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ab          |mm H2O      |lowest value al5 can have
!!    ajp         |mm H2O      |highest value al5 can have
!!    j           |none        |HRU number
!!    jj          |none        |counter
!!    k           |none        |number of time steps equivalent to 30 minutes
!!    kk          |none        |counter
!!    preceff     |mm H2O      |amount of rainfall on day in HRU
!!    rainsum     |mm H2O      |sum of rainfall during 30 min period
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Expo, Atri

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: iwave
      integer :: j, k, kk, jj
      real :: ab, ajp, preceff, rainsum

      j = 0
      j = ihru
      ab = 0.02083
      
      select case (ievent)
        case(0)                !! daily rainfall, estimate al5
          preceff = 0.
          if (iwave > 0) then
            !! subbasin sediment calculations
            if (sub_precip(iwave) > sub_snom(iwave)) then
              preceff = sub_precip(iwave) - sub_snom(iwave)
            else
              preceff = 0.
            endif
          else
            !! HRU sediment calculations
            if (precipday > snomlt) then
              preceff = precipday - snomlt
            else
              preceff = 0.
            endif
            if (preceff > ovrlnd(j)) then
              preceff = preceff - ovrlnd(j)
            else
              preceff = 0.
            endif
          endif

          ajp = 0.
          ajp = 1. - Expo(-125. / (preceff + 5.))
          if (ised_det == 0) then
          al5 = Atri(ab, amp_r(i_mo,hru_sub(j)), ajp, rndseed(idg(6),j))
          else
            al5 = amp_r(i_mo,hru_sub(j))
          end if

        case (1)            !! subdaily rainfall, get from pcp data
          if (idt <= 30) then
            k = 0
            k = 30 / idt
            k = k - 1
            do kk = 1, nstep+1-k
              rainsum = 0.
              do jj = 0, k
                if (precipdt(kk+jj) > (snomlt+ovrlnd(j))/nstep) then
                  rainsum = rainsum + precipdt(kk+jj) -                 
     &                                      (snomlt + ovrlnd(j)) / nstep
                end if
              end do
              al5 = Max(al5,rainsum)
            end do
            if (subp(j) > 0.01) then
              al5 = al5 / subp(j)
              al5 = Min(al5,.99)
            else
              al5 = ab
            end if
          else
            preceff = 0.
            if (iwave > 0) then
              !! subbasin sediment calculations
              if (sub_precip(iwave) > sub_snom(iwave)) then
                preceff = sub_precip(iwave) - sub_snom(iwave)
              else
                preceff = 0.
              endif
            else
              !! HRU sediment calculations
              if (precipday > snomlt) then
                preceff = precipday - snomlt
              else
                preceff = 0.
              endif
              if (preceff > ovrlnd(j)) then
                preceff = preceff - ovrlnd(j)
              else
                preceff = 0.
              endif
            endif

            ajp = 0.
            ajp = 1. - Expo(-125. / (preceff + 5.))
            if (ised_det == 0) then
              al5 = Atri(ab, amp_r(i_mo,hru_sub(j)), ajp,               
     &                                                rndseed(idg(6),j))
            else
              al5 = amp_r(i_mo,hru_sub(j))
            end if
          end if

      end select

      return
      end