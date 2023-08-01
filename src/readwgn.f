      subroutine readwgn

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the HRU weather generator parameters from the 
!!    .wgn file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    adj_pkr     |none          |peak rate adjustment factor in the subbasin
!!                               |Used in the MUSLE equation to account for
!!                               |impact of peak flow on erosion
!!    ffcb        |none          |initial soil water content expressed as a 
!!                               |fraction of field capacity
!!    i           |none          |HRU number
!!    idaf        |julian date   |beginning day of simulation
!!    idist       |none          |rainfall distribution code
!!                               |  0 for skewed normal dist
!!                               |  1 for mixed exponential distribution
!!    ndays(:)    |julian date   |julian date for last day of preceding
!!                               |month (where the array location is the
!!                               |number of the month) The dates are for
!!                               |leap years
!!    rndseed(:,:)|none          |random number generator seeds
!!    rnmd1       |none          |random number between 0.0 and 1.0
!!    sub_lat(:)  |degrees       |latitude of HRU/subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    amp_r(:,:)  |none          |average fraction of total daily rainfall
!!                               |occuring in maximum half-hour period
!!                               |for month
!!    daylmn(:)   |hr            |shortest daylength occurring during the year
!!    dewpt(:,:)  |deg C         |average dew point temperature for the month
!!    dormhr(:)   |hour          |time threshold used to define dormant
!!                               |period for plant (when daylength is within
!!                               |the time specified by dl from the minimum
!!                               |daylength for the area, the plant will go
!!                               |dormant)
!!    ffc(:)      |none          |initial HRU soil water content
!!                               |expressed as fraction of field capacity
!!    ireg(:)     |none          |precipitation category:
!!                               |  1 precipitation <= 508 mm/yr
!!                               |  2 precipitation > 508 and <= 1016 mm/yr
!!                               |  3 precipitation > 1016 mm/yr
!!    latcos(:)   |none          |Cos(Latitude)
!!    latsin(:)   |none          |Sin(Latitude)
!!    pcf(:,:)    |none          |normalization coefficient for precipitation
!!                               |generator
!!    pcp_stat(:,1,:)|mm/day     |average amount of precipitation falling in
!!                               |one day for the month
!!    pcp_stat(:,2,:)|mm/day     |standard deviation for the average daily
!!                               |precipitation
!!    pcp_stat(:,3,:)|none       |skew coefficient for the average daily 
!!                               |precipitation
!!    phutot(:)   |heat unit     |total potential heat units for year (used
!!                               |when no crop is growing)
!!    pr_w(1,:,:) |none          |probability of wet day after dry day in month
!!    pr_w(2,:,:) |none          |probability of wet day after wet day in month
!!    pr_w(3,:,:) |none          |proportion of wet days in the month
!!    solarav(:,:)|MJ/m^2/day    |average daily solar radiation for the month
!!    tmp_an(:)   |deg C         |average annual air temperature
!!    tmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    tmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    tmpstdmn(:,:)|deg C        |standard deviation for avg monthly minimum air
!!                               |temperature
!!    tmpstdmx(:,:)|deg C        |standard deviation for avg monthly maximum air
!!                               |temperature
!!    welev(:)    |m             |elevation of weather station used to compile
!!                               |data
!!    wlat(:)     |degrees       |latitude of weather station used to compile
!!                               |data
!!    wndav(:,:) |m/s            |average wind speed for the month
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dl          |hour          |time threshold used to define dormant
!!                               |period for plant (when daylength is within
!!                               |the time specified by dl from the minimum
!!                               |daylength for the area, the plant will go
!!                               |dormant)
!!    j           |none          |counter
!!    irelh       |none          |irelh = 0 (dewpoint)
!!                               |      = 1 (relative humidity)
!!                               |note:  inputs > 1.0 (dewpoint)
!!                               |       inputs < 1.0 (relative hum)
!!    lattan      |none          |Tan(Latitude)
!!    m1          |none          |array location (see definition of ndays)
!!    mdays       |none          |number of days in the month
!!    mon         |none          |monthly counter
!!    nda         |julian date   |julian date of last day in the month
!!    pcp         |mm H2O        |generated precipitation
!!    pcpmm(:)    |mm            |amount of precipitation in month
!!    pcpd(:)     |days          |average number of days of precipitation
!!                               |in the month
!!    r6          |none          |variable to hold calculation result
!!    rainhhmx(:) |mm            |maximum 0.5 hour rainfall in month
!!                               |for entire period of record
!!    rain_hhsm(:)|mm            |smoothed values for maximum 0.5 hour rainfall
!!    rain_yrs    |none          |number of years of recorded maximum 0.5h 
!!                               |rainfall used to calculate values for 
!!                               |rainhhmx(:)
!!    rndm1       |none          |random number between 0.0 and 1.0
!!    rnm2        |none          |random number between 0.0 and 1.0
!!    sum         |none          |variable to hold summation results
!!    summm_p     |mm            |sum of precipitation over year
!!    summn_t     |deg C         |sum of mimimum temp values over year
!!    summx_t     |deg C         |sum of maximum temp values over year
!!    tav         |deg C         |average monthly temperature
!!    titldum     |NA            |title line of .wgn file (not used elsewhere)
!!    tmax        |deg C         |maximum average monthly temperature
!!    tmin        |deg C         |minimum average monthly temperature
!!    tmpsoil     |deg C         |initial temperature of soil layers
!!    x1          |none          |variable to hold calculation results
!!    x2          |none          |variable to hold calculation results
!!    x3          |none          |variable to hold calculation results
!!    xlv         |none          |variable to hold calculation results
!!    xx          |varies        |variable to hold calculation results
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sin, Cos, Tan, abs, Acos, Log, Exp, MaxVal
!!    SWAT: Aunif, Dstn1

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
      real*8 :: xx, lattan, x1, x2, x3, tav, tmin, tmax, rain_yrs
      real*8 :: summx_t, summn_t, summm_p, sum, rnm2, r6, xlv, pcp
      real*8, dimension (12) :: rainhhmx, rain_hhsm, pcpd
      real*8 :: tmpsoil, sffc, rndm1, dl
      integer :: mon, mdays, j, m1, nda, xrnd


      pcpd = 0.
      rainhhmx = 0.
      pcpmm = 0.

      read (114,5000) titldum
      read (114,5100) wlat(i)
      read (114,5100) welev(i)
      read (114,5100) rain_yrs
      read (114,5200) (tmpmx(mon,i),mon = 1,12)
      read (114,5200) (tmpmn(mon,i),mon = 1,12)
      read (114,5200) (tmpstdmx(mon,i),mon = 1,12)
      read (114,5200) (tmpstdmn(mon,i),mon = 1,12)
      read (114,5201) (pcpmm(mon),mon = 1,12)
      read (114,5200) (pcp_stat(mon,2,i),mon = 1,12)  !pcpstd
      read (114,5200) (pcp_stat(mon,3,i),mon = 1,12)  !pcpskw
      read (114,5200) (pr_w(1,mon,i),mon = 1,12)
      read (114,5200) (pr_w(2,mon,i),mon = 1,12)
      read (114,5200) (pcpd(mon),mon = 1,12)
      read (114,5200) (rainhhmx(mon),mon = 1,12)
      read (114,5200) (solarav(mon,i),mon = 1,12)
      read (114,5200) (dewpt(mon,i),mon = 1,12)
      read (114,5200) (wndav(mon,i),mon = 1,12)

!! determine if input for dewpt is relative humidity
      do mon = 1,12
       if (dewpt(mon,i) > 1.0  .or. dewpt(mon,i) < 0.0) then
         irelh(i) = 0
       end if
      end do 

!! variables needed for radiation calcs.
      xx = 0.0
      lattan = 0.0
      x1 = 0.0
      x2 = 0.0
      if (sub_lat(i) < 1.e-4) sub_lat(i) = wlat(i)
      xx = sub_lat(i) / 57.296 
                          !!convert degrees to radians (2pi/360=1/57.296)
      latsin(i) = Sin(xx)
      latcos(i) = Cos(xx)
      lattan = Tan(xx)
!! calculate minimum daylength 
!! daylength=2*acos(-tan(sd)*tan(lat))/omega
!! where solar declination, sd, = -23.5 degrees for minimum daylength in
!!                      northern hemisphere and -tan(sd) = .4348
!!       dabsolute value is taken of tan(lat) to convert southern hemisphere
!!                      values to northern hemisphere
!!       the angular velocity of the earth's rotation, omega, = 15 deg/hr or
!!                      0.2618 rad/hr and 2/0.2618 = 7.6394
      x1 = .4348 * abs(lattan)      
      if (x1 < 1.) x2 = Acos(x1) 
                         !!x1 will be >= 1. if sub_lat > 66.5 or < -66.5
      daylmn(i) = 7.6394 * x2

!! calculate day length threshold for dormancy
      if (dorm_hr < -1.e-6) then
        dl = 0.
         if (abs(sub_lat(i)) > 40.) then
          dl = 1.
         else if (abs(sub_lat(i)) < 20.) then
          dl = -1.
         else
         dl = (abs(sub_lat(i)) - 20.) / 20.
         end if
      else
         dl = dorm_hr
      end if



!! calculate smoothed maximum 0.5hr rainfall amounts
      rain_hhsm = 0.
      rain_hhsm(1) = (rainhhmx(12) + rainhhmx(1) + rainhhmx(2)) / 3.
      do mon = 2, 11
        rain_hhsm(mon) = (rainhhmx(mon-1) + rainhhmx(mon) +             
     &                     rainhhmx(mon+1)) / 3.
      end do
      rain_hhsm(12) = (rainhhmx(11) + rainhhmx(12) + rainhhmx(1)) / 3.


!! calculate missing values and additional parameters
      summx_t = 0.
      summn_t = 0.
      summm_p = 0.
      tmin = 100.
      tmax = 0.
      pcpdays(i) = 0.
      do mon = 1, 12
        mdays = 0
        tav = 0.
        mdays = ndays(mon+1) - ndays(mon)
        tav = (tmpmx(mon,i) + tmpmn(mon,i)) / 2.
        if (tav > tmax) tmax = tav
        if (tav < tmin) tmin = tav
        summx_t = summx_t + tmpmx(mon,i)
        summn_t = summn_t + tmpmn(mon,i)

        !! calculate total potential heat units
        if (tav > 0.) phutot(i) = phutot(i) + tav * mdays

        !! calculate values for pr_w if missing or bad
        if (pr_w(2,mon,i) <= pr_w(1,mon,i).or.pr_w(1,mon,i) <= 0.) then
          if (pcpd(mon) < .1) pcpd(mon) = 0.1
          pr_w(1,mon,i) = .75 * pcpd(mon) / mdays
          pr_w(2,mon,i) = .25 + pr_w(1,mon,i)
        else
        !! if pr_w values good, use calculated pcpd based on these values
        !! using first order Markov chain
        pcpd(mon) = mdays * pr_w(1,mon,i) /                             
     &                              (1. - pr_w(2,mon,i) + pr_w(1,mon,i))
    
        end if

        !! calculate precipitation-related values
        if (pcpd(mon) <= 0.) pcpd(mon) = .001
        pr_w(3,mon,i) = pcpd(mon) / mdays
        pcp_stat(mon,1,i) = pcpmm(mon) / pcpd(mon)
        if (pcp_stat(mon,3,i) < 0.2) pcp_stat(mon,3,i) = 0.2
        summm_p = summm_p + pcpmm(mon)
        pcpdays(i) = pcpdays(i) + pcpd(mon)
      end do

      tmp_an(i) = (summx_t + summn_t) / 24.

      !! calculate initial temperature of soil layers
      if (idaf > ndays(2)) then
        do mon = 2, 12
          m1 = 0
          nda = 0
          m1 = mon + 1
          nda = ndays(m1) - 1
          if (idaf <= nda) exit
        end do
      else
        mon = 1
      end if
      tmpsoil = 0.
      tmpsoil = (tmpmx(mon,i) + tmpmn(mon,i)) / 2.

      xrnd = 0
      xrnd = rndseed(idg(3),i)
      rndm1 = Aunif(xrnd)
      do mon = 1, 12
        !! calculate precipitation correction factor for pcp generator
        if (idist == 0) then
          r6 = 0.
          rnm2 = 0.
          xlv = 0.
          pcp = 0.
          sum = 0.
          r6 = pcp_stat(mon,3,i) / 6.
          do j = 1, 1000
            rnm2 = Aunif(xrnd)
            xlv = (Dstn1(rndm1,rnm2) -r6) * r6 + 1
            rndm1 = rnm2
            xlv = (xlv**3 - 1.) * 2 / pcp_stat(mon,3,i)
            pcp = xlv * pcp_stat(mon,2,i) + pcp_stat(mon,1,i)
            if (pcp < 0.01) pcp = 0.01
            sum = sum + pcp
          end do
          if (sum > 0.) then
            pcf(mon,i) = 1000. * pcp_stat(mon,1,i) / sum
          else
            pcf(mon,i) = 1.
          end if
        end if

        !! calculate or estimate amp_r values
        x1 = 0.
        x2 = 0.
        x3 = 0.
        if (rain_yrs < 1.0) rain_yrs = 10.
        x1 = .5 / rain_yrs 
        x2 = x1 / pcpd(mon)
        x3 = rain_hhsm(mon) / Log(x2)
        if (pcp_stat(mon,1,i) > 1.e-4) then
          amp_r(mon,i) = adj_pkr * (1. - Exp(x3 / pcp_stat(mon,1,i)))
        else
          amp_r(mon,i) = 0.95
        end if
        if (amp_r(mon,i) < .1) amp_r(mon,i) = .1
        if (amp_r(mon,i) > .95) amp_r(mon,i) = .95
      end do

!!    determine precipitation category (ireg initialized to category 1)
      xx = 0
      xx = summm_p
      if (summm_p > 508.) ireg(i) = 2
      if (summm_p > 1016.) ireg(i) = 3

!!    set fraction of field capacity in soil
      sffc = 0.
      if (ffcb <= 0.) then
        sffc = summm_p / (summm_p + Exp(9.043 - 0.002135*summm_p)) 
                         !!S-curve equation Jeff made up.
      else
        sffc = ffcb
      end if

!! assign HRU values
      do j = 1, hrutot(i)
        ihru = 0
        ihru = nhru + j
        do k = 1, sol_nly(ihru)
          sol_tmp(k,ihru) = tmpsoil
        end do
        ffc(ihru) = sffc
        dormhr(ihru) = dl
        !! set initial precip/pet values for tropical plant growth
        !! compute potential et with Preistley-Taylor Method
        mdays = ndays(13) - ndays(12)
        tav = (tmpmx(12,i) + tmpmn(12,i)) / 2.
        tk = tav  + 273.
        alb = .15   !tropical rainforests (0.05-0.15)
        d = EXP(21.255 - 5304. / tk) * 5304. / tk ** 2
        gma = d / (d +.68)
        ho = 23.9 * solarav(12,i) * (1. - alb) / 58.3
        aph = 1.28
        pet_dec = aph * ho * gma * 30.
        ppet(j)%precip = pcpmm(12) / mdays
        ppet(j)%pet = pet_dec / mdays
        ppet(j)%precip_sum = pcpmm(12)
        ppet(j)%pet_sum = pet_dec
        ppet(j)%rto = ppet(j)%precip_sum / ppet(j)%pet_sum

      end do

      close (114)
      return
 5000 format (a)
 5100 format (12x,f7.2)
 5200 format (12f6.2)
 5201 format (12f6.1)  
      end