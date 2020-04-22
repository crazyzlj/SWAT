      subroutine nitvol

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine estimates daily mineralization (NH3 to NO3)
!!    and volatilization of NH3

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr         |none          |current year of simulation
!!    hru_dafr(:)   |km**2/km**2   |fraction of watershed area in HRU
!!    ihru          |none          |HRU number
!!    nyskip        |none          |number of years to skip output
!!                                 |summarization and printing
!!    sol_fc(:,:)   |mm H2O        |amount of water available to plants in soil
!!                                 |layer at field capacity (fc - wp)
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_nly(:)    |none          |number of layers in soil profile
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool in soil layer
!!    sol_st(:,:)   |mm H2O        |amount of water stored in the soil layer
!!                                 |on any given day (less wp water)
!!    sol_tmp(:,:)  |deg C         |daily average temperature of soil layer
!!    sol_wpmm(:,:) |mm H20        |water content of soil at -1.5 MPa (wilting
!!                                 |point)
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    wshd_nitn     |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from the NH3 to the NO3 pool by
!!                                 |nitrification in the watershed
!!    wshd_voln     |kg N/ha       |average annual amount if nitrogen lost by
!!                                 |ammonia volatilization in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool in soil layer
!!    wshd_nitn     |kg N/ha       |average annual amount of nitrogen moving
!!                                 |from the NH3 to the NO3 pool by
!!                                 |nitrification in the watershed
!!    wshd_voln     |kg N/ha       |average annual amount if nitrogen lost by
!!                                 |ammonia volatilization in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    akn         |
!!    akv         |
!!    cecf        |none          |volatilization CEC factor
!!    dmidl       |
!!    dpf         |
!!    j           |none          |HRU number
!!    k           |none          |counter (soil layer)
!!    rnit        |kg N/ha       |amount of nitrogen moving from the NH3 to the
!!                               |NO3 pool (nitrification) in the layer
!!    rnv         |
!!    rvol        |kg N/ha       |amount of nitrogen lost from the NH3 pool due
!!                               |to volatilization
!!    sw25        |
!!    swf         |
!!    swwp        |
!!    tf          |
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm

      integer :: j, k
      real*8 :: sw25, swwp, swf, xx, dmidl, dpf, akn, akv, rnv, rnit, rvol
      real*8 :: tf 
      real*8 :: cecf = 0.15

      j = 0
      j = ihru 

      do k = 1, sol_nly(j)
        tf = 0.
        tf = .41 * (sol_tmp(k,j) - 5.) / 10.

        if (sol_nh3(k,j) > 0. .and. tf >= 0.001) then
          sw25 = 0.
          swwp = 0.
          sw25 = sol_wpmm(k,j) + 0.25 * sol_fc(k,j)
          swwp = sol_wpmm(k,j) + sol_st(k,j)
          if (swwp < sw25) then
            swf = 0.
            swf = (swwp - sol_wpmm(k,j)) /(sw25 - sol_wpmm(k,j))
          else
            swf = 1.
          endif

          if (k == 1) then
            xx = 0.
          else
            xx = 0.
            xx = sol_z(k-1,j)
          endif

          dmidl = 0.
          dpf = 0.
          akn = 0.
          akv = 0.
          rnv = 0.
          rnit = 0.
          rvol = 0.
          dmidl = (sol_z(k,j) + xx) / 2.
          dpf = 1. - dmidl / (dmidl + Exp(4.706 - .0305 * dmidl))
          akn = tf * swf
          akv = tf * dpf * cecf
          rnv = sol_nh3(k,j) * (1. - Exp(-akn - akv))
          rnit = 1. - Exp(-akn)
          rvol = 1. - Exp(-akv)

          !! calculate nitrification (NH3 => NO3)
	    !! apply septic algorithm only to active septic systems
          if(k/=i_sep(j).or.isep_opt(j)/= 1) then  ! J.Jeong for septic, biozone layer
             if (rvol + rnit > 1.e-6) then
               rvol = rnv * rvol / (rvol + rnit)
               rnit = rnv - rvol
               if (rnit < 0.) rnit = 0.
               sol_nh3(k,j) = Max(1.e-6, sol_nh3(k,j) - rnit)
             endif
             if (sol_nh3(k,j) < 0.) then
               rnit = rnit + sol_nh3(k,j)
               sol_nh3(k,j) = 0.
             endif
             sol_no3(k,j) = sol_no3(k,j) + rnit

             !! calculate ammonia volatilization
             sol_nh3(k,j) = Max(1.e-6, sol_nh3(k,j) - rvol)
             if (sol_nh3(k,j) < 0.) then
               rvol = rvol + sol_nh3(k,j)
               sol_nh3(k,j) = 0.
             endif

             !! summary calculations
             if (curyr > nyskip) then
               wshd_voln = wshd_voln + rvol * hru_dafr(j)
               wshd_nitn = wshd_nitn + rnit * hru_dafr(j)
             end if
          end if
        end if

      end do

      return
      end