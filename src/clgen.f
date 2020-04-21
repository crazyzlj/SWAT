      subroutine clgen(j)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the daylength, distribution of 
!!    radiation throughout the day and maximum radiation for day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    iida        |julian date   |day being simulated (current julian date)
!!    ipet        |none          |code for potential ET method
!!                               |0 Priestley-Taylor method
!!                               |1 Penman/Monteith method
!!                               |2 Hargreaves method
!!    j           |none          |HRU number
!!    latcos(:)   |none          |Cos(Latitude) for HRU
!!    latsin(:)   |none          |Sin(Latitude) for HRU
!!    npcp(:)     |none          |prior day category
!!                               |1 dry day
!!                               |2 wet day
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dayl(:)     |hours         |day length
!!    frad(:,:)   |none          |fraction of solar radiation occuring during 
!!                               |hour in day in HRU
!!    hru_rmx(:)  |MJ/m^2        |maximum possible radiation for the day in HRU
!!    npcp(:)     |none          |prior day category
!!                               |1 dry day
!!                               |2 wet day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch          |none          |(-Tan(sd)*Tan(lat))
!!    cosrho(:)   |none          |Cos(zenith angle for hour)
!!    dd          |none          |relative distance of the earth from the sun
!!    h           |none          |Acos(-Tan(sd)*Tan(lat))
!!    ii          |none          |counter
!!    sd          |radians       |solar declination: latitude at which the sun
!!                               |is directly overhead at noon
!!    totrho      |none          |sum of cosrho values for all hours of day
!!    yc          |none          |Cos(sd)*Cos(lat)
!!    ys          |none          |Sin(sd)*Sin(lat)
!!    w           |none          |hour angle
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sin, Tan, Acos, Cos, Real

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: j
      integer :: ii
      real :: sd, ch, h, ys, yc, dd, w, cosrho(24), totrho

      !! Reset prior day category for precipitation
      if (subp(j) >= 0.1) then
        npcp(j) = 2
      else
        npcp(j) = 1
      end if


      !! Calculate Daylength !!
      !! calculate solar declination: equation 2.1.2 in SWAT manual
      sd = 0.
      sd = Asin(.4 * Sin((Real(iida) - 82.) / 58.09))  !!365/2pi = 58.09

      !! calculate the relative distance of the earth from the sun
      !! the eccentricity of the orbit
      !! equation 2.1.1 in SWAT manual
      dd = 0.
      dd = 1.0 + 0.033 * Cos(Real(iida) / 58.09)

      !!daylength = 2 * Acos(-Tan(sd) * Tan(lat)) / omega
      !!where the angular velocity of the earth's rotation, omega, is equal
      !! to 15 deg/hr or 0.2618 rad/hr and 2/0.2618 = 7.6374
      !! equation 2.1.6 in SWAT manual

      ch = 0.
      h = 0.
      ch = -latsin(hru_sub(j)) * Tan(sd) / latcos(hru_sub(j))
      if (ch > 1.) then    !! ch will be >= 1. if latitude exceeds +/- 66.5 deg in winter
        h = 0.
      elseif (ch >= -1.) then
        h = Acos(ch)
      else
        h = 3.1416         !! latitude exceeds +/- 66.5 deg in summer
      endif
      dayl(j) = 7.6394 * h
          
      !! Calculate Potential (maximum) Radiation !!
      !! equation 2.2.7 in SWAT manual
      ys = 0.
      yc = 0.
      ys = latsin(hru_sub(j)) * Sin(sd)
      yc = latcos(hru_sub(j)) * Cos(sd)
      hru_rmx(j) = 30. * dd * (h * ys + yc * Sin(h))

      !! Calculate fraction of radiation recieved during each hour in day
      !! this calculation assumes solar noon (when the angle between the
      !! observer on the earth to the sun and a line normal to the earth's
      !! at that position is at a minimum) falls at 12 noon in day.
      !! equation 2.2.10 in SWAT manual
      
      cosrho = 0.
      totrho = 0.
      do ii = 1, 24
        !!angular velocity times hour away from solar noon. To calculate
        !!radiation for an hour, the hour angle for the midpoint of the 
        !!time period is used. time = 0. at solar noon with positive values
        !! in the morning and negative in the evening
        w = 0.
        w = (12.5 - Real(ii)) * 0.2618   !!0.2618 rad/hr
        cosrho(ii) = ys + yc * Cos(w)
        if (cosrho(ii) <= 0.) cosrho(ii) = 0.
        totrho = totrho + cosrho(ii)
      end do
      if (totrho > 0.001) then
        do ii = 1, 24
          frad(j,ii) = cosrho(ii) / totrho
        end do
      end if

      return
      end
