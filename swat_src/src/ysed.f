      subroutine ysed(iwave)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine predicts daily soil loss caused by water erosion
!!    using the modified universal soil loss equation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cvm(:)      |none          |natural log of USLE_C (the minimum value
!!                               |of the USLE C factor for the land cover)
!!    hru_km(:)   |km**2         |area of HRU in square kilometers
!!    icr(:)      |none          |sequence number of crop grown within a year
!!    idplt(:,:,:)|none          |land cover code from crop.dat
!!    ihru        |none          |HRU number
!!    iwave       |none          |flag to differentiate calculation of HRU and
!!                               |subbasin sediment calculation
!!                               |iwave = 0 for HRU
!!                               |iwave = subbasin # for subbasin
!!    nro(:)      |none          |sequence number of year in rotation
!!    peakr       |m^3/s         |peak runoff rate
!!    rsd_covco   |              |residue cover factor for computing fraction of
!!                                  cover
!!    sno_hru(:)  |mm H2O        |amount of water in snow in HRU on current day
!!    sol_cov(:)  |kg/ha         |amount of residue on soil surface
!!    sub_km(:)   |km^2          |area of subbasin in square kilometers
!!    sub_qd(:)   |mm H2O        |surface runoff loading from subbasin for day
!!    surfq(:)    |mm H2O        |surface runoff for the day in HRU
!!    usle_ei     |100(ft-tn in)/(acre-hr)|USLE rainfall erosion index
!!    usle_mult(:)|none          |product of USLE K,P,LS,exp(rock)
!!    wcklsp(:)   |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cklsp(:)    |
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion
!!    usle        |metric tons/ha|daily soil loss predicted with USLE equation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |
!!    j           |none          |HRU number
!!    bio_frcov   |              |fraction of cover by biomass - adjusted for
!!                                  canopy height
!!    grcov_fr    |              |fraction of cover by biomass as function of lai
!!    rsd_frcov   |              |fraction of cover by residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: iwave
      integer :: j
      real :: c

      j = 0
      j = ihru
      
      !! initialize variables
      c = 0.
      cklsp(j) = 0.

      if (iwave > 0) then
        !! subbasin sediment calculations
        cklsp(j) = wcklsp(iwave)
      else
      !! HRU sediment calculations
        cklsp(j) = usle_cfac(j) * usle_mult(j)
      end if

      !! compute sediment yield with musle
      if (iwave > 0) then
        !! subbasin sediment calculations
        sedyld(j) = (sub_qd(iwave) * peakr * 1000. * sub_km(iwave))     
     &                                                 ** .56 * cklsp(j)
      else
        !! HRU sediment calculations
        sedyld(j) = (surfq(j) * peakr * 1000. * hru_km(j)) ** .56       
     &                                                        * cklsp(j)
      end if

      if (isproj == 2) then
        sedyld(j) = sedyld(j) * dr_sub(j)
      end if

      if (sedyld(j) < 0.) sedyld(j) = 0.

      !!adjust sediment yield for protection of snow cover
      if (sno_hru(j) > 0.) then
        if (sedyld(j) < 1.e-6) sedyld(j) = 0.0
      else if (sno_hru(j) > 100.) then
        sedyld(j) = 0.
      else
        sedyld(j) = sedyld(j) / Exp(sno_hru(j) * 3. / 25.4)
      end if

      !! bmp adjustment
      sedyld(j) = sedyld(j) * bmp_sed(j)
      
	!!Particle size distribution of sediment yield
	  sanyld(j) = sedyld(j) * det_san(j)    !! Sand yield
	  silyld(j) = sedyld(j) * det_sil(j)    !! Silt yield
	  clayld(j) = sedyld(j) * det_cla(j)    !! Clay yield
	  sagyld(j) = sedyld(j) * det_sag(j)    !! Small Aggregate yield
	  lagyld(j) = sedyld(j) * det_lag(j)    !! Large Aggregate yield

      !! compute erosion with usle (written to output for comparison)
      usle = 1.292 * usle_ei * cklsp(j) / 11.8

      return
      end