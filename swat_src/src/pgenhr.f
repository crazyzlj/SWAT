      subroutine pgenhr(jj)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine distributes daily rainfall exponentially within the day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    amp_r(:,:)   |none          |alpha factor for rain(mo max 0.5h rain)
!!    hru_km(:)    |km^2          |area of HRU in square kilometers
!!    hru_sub(:)   |none          |subbasin in which HRU is located
!!    idg(:)       |none          |array location of random number seed
!!                                |used for a given process
!!    idt          |minutes       |length of time step used to report
!!                                |precipitation data for sub-daily modeling
!!    jj           |none          |HRU number
!!    i_mo         |none          |month being simulated
!!    rndseed(:,:) |none          |random number generator seed
!!    subp(:)      |mm H2O        |precipitation for the day in HRU
!!    tconc(:)     |hr            |time of concentration for HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rainsub(:)   |mm H2O        |rainfall during time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ab           |mm H2O        |lowest value al5 can have
!!    ajp          |mm H2O        |highest value al5 can have
!!    al5          |none          |fraction of total rainfall on day that occurs
!!                                |during 0.5h highest intensity rainfall
!!    altc         |none          |equation coefficient
!!    blm          |none          |lowest random number value allowed
!!    dur          |hours         |duration of storm during day
!!    ihour        |none          |counter
!!    itime        |none          |time step during day
!!    j            |none          |HRU number
!!    k            |none          |random number seed, counter
!!    nhour        |none          |number of time steps per hour
!!    pkrain       |mm H2O        |volume of rain at time of peak rainfall
!!    pkrr         |mm/hr         |peak rainfall rate
!!    pt           |min           |time during day
!!    qmn          |none          |mean random number value
!!    rtp          |min           |time of peak rainfall rate
!!    rx           |mm H2O        |total rainfall at end of time step
!!    sumrain      |mm H2O        |total amount of daily rainfall prior to
!!                                |time step
!!    uplm         |none          |highest random number value
!!    vv           |none          |random number between 0.0 and 1.0 that 
!!                                |represents time to peak rainfall rate
!!                                |expressed as a fraction of storm duration
!!    xk1          |none          |1st constant in dimensionless exponential
!!                                |rainfall distribution
!!    xk2          |none          |2nd constant in dimensionless exponential
!!                                |rainfall distribution
!!    xkp1         |hr            |1st constant in exponential rainfall
!!                                |distribution
!!    xkp2         |hr            |2nd constant in exponential rainfall
!!                                |distribution
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log
!!    SWAT: Atri, Expo

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: jj
      integer :: itime, pt, ihour, nhour, k
      real :: vv, blm, qmn, uplm, dur, ab, ajp, altc, pkrain, rtp
      real :: xk1, xk2, xkp1, xkp2, rx, pkrr, sumrain


      !! calculate maximum half-hour rainfall
      ab = 0.02083
      ajp = 0.
      al5 = 0.
      ajp = 1. - Expo(-125. / (subp(jj) + 5.))
      al5 = Atri(ab, amp_r(i_mo,hru_sub(jj)), ajp, rndseed(10,jj))

      !! need peak rainfall rate 
      !! calculate peak rate using same method as that for peak runoff
      altc = 0.
      pkrr = 0.
      altc = 1. - Expo(2. * tconc(jj) * Log(1. - al5))
      pkrr = altc * subp(jj) / tconc(jj)           !! mm/h


      !! generate random number between 0.0 and 1.0
      !! because all input set to constant value, vv always the same
      !! vv => time to peak expressed as fraction of total storm duration
      vv = 0.
      blm = 0.05
      qmn = 0.25
      uplm = 0.95
      k = 8
      vv = Atri(blm, qmn, uplm, k)
      !vv = 0.03
      
      !! calculate storm duration
      xk1 = 0.
      xk2 = 0.
      dur = 0.
      xk1 = vv / 4.605
      xk2 = (1.- vv) / 4.605
      dur = subp(jj) / (pkrr * (xk1 + xk2))
      if (dur > 24.0) then
        dur = 24.0
        pkrr = subp(jj) / (dur * (xk1 + xk2))
      end if

      !! calculate amount of total rainfall fallen at time of peak
      !! rainfall and time of peak rainfall in units of minutes
      pkrain = 0.
      rtp = 0.
      pkrain = vv * subp(jj)
      rtp = vv * dur * 60

      !! calculate constants for exponential rainfall distribution
      !! equation
      xkp1 = 0.
      xkp2 = 0.
      xkp1 = dur * xk1 
      xkp2 = dur * xk2

      pt = 0
      pt = idt
      itime = 1
      sumrain = 0.

      !! do before time of peak rainfall
      !! do while pt less than rtp
      do
        if (pt >= Int(rtp)) exit
        rx = 0.
        rx = pkrain - pkrr * xkp1 *                                     
     &                       (1. - Exp((Real(pt) - rtp) / (60. * xkp1)))
        rainsub(jj,itime) = rx - sumrain
        pt = pt + idt
        itime = itime + 1
        if (itime > nstep) exit
        sumrain = 0.
        sumrain = rx
      end do

      !! after peak rainfall and before end of storm
      do
        if (pt >= Int(dur * 60.)) exit
        rx = 0.
        rx = pkrain + pkrr * xkp2 *                                     
     &                       (1. - Exp((rtp - Real(pt)) / (60. * xkp2)))
        rainsub(jj,itime) = rx - sumrain
        pt = pt + idt
        itime = itime + 1
        if (itime > nstep) exit
        sumrain = 0.
        sumrain = rx
      end do

      !! at end of storm
      if (subp(jj) > sumrain .and. itime <= nstep) then
        rainsub(jj,itime) = subp(jj) - sumrain
      end if

      return
      end