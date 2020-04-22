      subroutine solt
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily average temperature at the bottom
!!    of each soil layer     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    albday      |none          |albedo of ground for day
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    hru_sub(:)  |none          |subbasin in which HRU is located
!!    ihru        |none          |HRU number
!!    sno_hru(:)  |mm H2O        |amount of water in snow in HRU on current day
!!    sol_avbd(:) |Mg/m^3        |average bulk density of soil profile
!!    sol_cov(:)  |kg/ha         |amount of residue on soil surface
!!    sol_nly(:)  |none          |number of soil layers in profile
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on
!!                               |any given day
!!    sol_tmp(:,:)|deg C         |average temperature of soil layer on previous
!!                               |day
!!    sol_z(:,:)  |mm            |depth to bottom of soil layer
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmp_an(:)   |deg C         |average annual air temperature
!!    tmpav(:)    |deg C         |average temperature for the day in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_tmp(:,:)|deg C         |daily average temperature of soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    b           |none          |variable to hold intermediate calculation
!!    bcv         |none          |lagging factor for cover
!!    dd          |mm            |damping depth for day
!!    df          |none          |depth factor
!!    dp          |mm            |maximum damping depth
!!    f           |none          |variable to hold intermediate calculation 
!!                               |result
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    st0         |MJ/m^2        |radiation hitting soil surface on day
!!    tbare       |deg C         |temperature of bare soil surface
!!    tcov        |deg C         |temperature of soil surface corrected for
!!                               |cover
!!    tlag        |none          |lag coefficient for soil temperature
!!    tmp_srf     |deg C         |temperature of soil surface
!!    wc          |none          |scaling factor for soil water impact on daily
!!                               |damping depth
!!    ww          |none          |variable to hold intermediate calculation
!!    xx          |none          |variable to hold intermediate calculation
!!    zd          |none          |ratio of depth at center of layer to
!!                               |damping depth
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Log, Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer :: j, k
      real*8 :: f, dp, ww, b, wc, dd, xx, st0
      real*8 :: tlag, df, zd, bcv, tbare, tcov, tmp_srf

      j = 0
      j = ihru

      tlag = 0.8

!! calculate damping depth

      !! calculate maximum damping depth
      !! SWAT manual equation 2.3.6
      f = 0.
      dp = 0.
      f = sol_avbd(j) / (sol_avbd(j) + 686. * Exp(-5.63 * sol_avbd(j)))
      dp = 1000. + 2500. * f

      !! calculate scaling factor for soil water
      !! SWAT manual equation 2.3.7
      ww = 0.
      wc = 0.
      ww = .356 - .144 * sol_avbd(j)
      wc = sol_sw(j) / (ww * sol_z(sol_nly(j),j))

      !! calculate daily value for damping depth
      !! SWAT manual equation 2.3.8
      b = 0.
      f = 0.
      dd = 0.
      b = Log(500. / dp)
      f = Exp(b * ((1. - wc) / (1. + wc))**2)
      dd = f * dp


!! calculate lagging factor for soil cover impact on soil surface temp
!! SWAT manual equation 2.3.11
      bcv = 0.
      bcv = sol_cov(j) /                                                
     &                 (sol_cov(j) + Exp(7.563 - 1.297e-4 * sol_cov(j)))
      if (sno_hru(j) /= 0.) then
        if (sno_hru(j) <= 120.) then
          xx = 0.
          xx = sno_hru(j) /                                             
     &                    (sno_hru(j) + Exp(6.055 - .3002 * sno_hru(j)))
        else
          xx = 1.
        end if
        bcv = Max(xx,bcv)
      end if

!! calculate temperature at soil surface
      st0 = 0.
      tbare = 0.
      tcov = 0.
      tmp_srf = 0.
      !! SWAT manual equation 2.3.10
      st0 = (hru_ra(j) * (1. - albday) - 14.) / 20.
      !! SWAT manual equation 2.3.9
      tbare = tmpav(j) + 0.5 * (tmx(j) - tmn(j)) * st0
      !! SWAT manual equation 2.3.12
      tcov = bcv * sol_tmp(2,j) + (1. - bcv) * tbare

!!    taking average of bare soil and covered soil as in APEX
!!    previously using minumum causing soil temp to decrease
!!    in summer due to high biomass

      tmp_srf = 0.5 * (tbare + tcov)  ! following Jimmy's code
      


!! calculate temperature for each layer on current day
      xx = 0.
      do k = 1, sol_nly(j)
        zd = 0.
        df = 0.
        zd = (xx + sol_z(k,j)) / 2.  ! calculate depth at center of layer
        zd = zd / dd                 ! SWAT manual equation 2.3.5
        !! SWAT manual equation 2.3.4
        df = zd / (zd + Exp(-.8669 - 2.0775 * zd))
        !! SWAT manual equation 2.3.3
        sol_tmp(k,j) = tlag * sol_tmp(k,j) + (1. - tlag) *              
     &                   (df * (tmp_an(hru_sub(j)) - tmp_srf) + tmp_srf)
        xx = sol_z(k,j)

        ! Temperature correction for Onsite Septic systems
        if (isep_opt(j)/=0.and.iyr>=isep_iyr(j).and.k>=i_sep(j)) then
	   if (sol_tmp(k,j) < 10.) then
	      sol_tmp(k,j) = 10. - (10. - sol_tmp(k,j)) * 0.1
	   end if     
	  endif

      end do

      return
      end