      subroutine readwwq

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the watershed stream water quality input 
!!    data (.wwq file) and initializes the QUAL2E variables which apply to 
!!    the entire watershed

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ai0         |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    ai1         |mg N/mg alg   |fraction of algal biomass that is nitrogen
!!    ai2         |mg P/mg alg   |fraction of algal biomass that is phosphorus
!!    ai3         |mg O2/mg alg  |the rate of oxygen production per unit of
!!                               |algal photosynthesis
!!    ai4         |mg O2/mg alg  |the rate of oxygen uptake per unit of algae
!!                               |respiration
!!    ai5         |mg O2/mg N    |the rate of oxygen uptake per unit of NH3 
!!                               |nitrogen oxidation
!!    ai6         |mg O2/mg N    |the rate of oxygen uptake per unit of NO2 
!!                               |nitrogen oxidation
!!    chla_subco  |fraction      |regional adjustment on sub chla_a loading
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    igropt      |none          |Qual2E option for calculating the local
!!                               |specific growth rate of algae
!!                               |1: multiplicative: 
!!                                   u = mumax * fll * fnn * fpp
!!                               |2: limiting nutrient
!!                               |   u = mumax * fll * Min(fnn, fpp)
!!                               |3: harmonic mean
!!                               |   u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
!!    k_l         |MJ/(m2*hr)    |half-saturation coefficient for light
!!    k_n         |mg N/L        |michaelis-menton half-saturation constant 
!!                               |for nitrogen
!!    k_p         |mg P/L        |michaelis-menton half saturation constant 
!!                               |for phosphorus
!!    lambda0     |1/m           |non-algal portion of the light extinction 
!!                               |coefficient
!!    lambda1     |1/(m*ug chla/L)|linear algal self-shading coefficient
!!    lambda2     |(1/m)(ug chla/L)**(-2/3)
!!                               |nonlinear algal self-shading coefficient
!!    lao         |NA            |Qual2E light averaging option. Qual2E defines
!!                               |four light averaging options. The only option
!!                               |currently available in SWAT is #2.
!!    mumax       |1/day or 1/hr |maximum specific algal growth rate
!!    p_n         |none          |algal preference factor for ammonia
!!    rhoq        |1/day or 1/hr |algal respiration rate
!!    tfact       |none          |fraction of solar radiation computed in the
!!                               |temperature heat balance that is 
!!                               |photosynthetically active
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    titldum     |NA            |title line for .wwq file, not used
!!    eof         |none          |end of file flag
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
      integer :: eof

!!    initialize variables
      eof = 0

      do
      read (101,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (101,*,iostat=eof) lao 
      if (eof < 0) exit
      read (101,*,iostat=eof) igropt 
      if (eof < 0) exit
      read (101,*,iostat=eof) ai0 
      if (eof < 0) exit
      read (101,*,iostat=eof) ai1 
      if (eof < 0) exit
      read (101,*,iostat=eof) ai2 
      if (eof < 0) exit
      read (101,*,iostat=eof) ai3 
      if (eof < 0) exit
      read (101,*,iostat=eof) ai4
      if (eof < 0) exit
      read (101,*,iostat=eof) ai5 
      if (eof < 0) exit
      read (101,*,iostat=eof) ai6 
      if (eof < 0) exit
      read (101,*,iostat=eof) mumax 
      if (eof < 0) exit
      read (101,*,iostat=eof) rhoq 
      if (eof < 0) exit
      read (101,*,iostat=eof) tfact 
      if (eof < 0) exit
      read (101,*,iostat=eof) k_l 
      if (eof < 0) exit
      read (101,*,iostat=eof) k_n 
      if (eof < 0) exit
      read (101,*,iostat=eof) k_p 
      if (eof < 0) exit
      read (101,*,iostat=eof) lambda0 
      if (eof < 0) exit
      read (101,*,iostat=eof) lambda1
      if (eof < 0) exit
      read (101,*,iostat=eof) lambda2 
      if (eof < 0) exit
      read (101,*,iostat=eof) p_n
      if (eof < 0) exit
      read (101,*,iostat=eof) chla_subco
      if (eof < 0) exit
      exit
      end do


!!    set default values for undefined parameters

      if (lao <= 0) lao = 2
      if (igropt <= 0) igropt = 2
      if (ai0 <= 0.) ai0 = 50.
      if (ai1 <= 0.) ai1 = 0.08
      if (ai2 <= 0.) ai2 = 0.015
      if (ai3 <= 0.) ai3 = 1.60
      if (ai4 <= 0.) ai4 = 2.0
      if (ai5 <= 0.) ai5 = 3.5
      if (ai6 <= 0.) ai6 = 1.07
      if (mumax <= 0.) mumax = 2.0
      if (rhoq <= 0.) rhoq = 2.5      !! previous 0.3
      if (tfact <= 0.) tfact = 0.3
      if (k_l <= 0.) k_l = 0.75
      if (k_n <= 0.) k_n = 0.02
      if (k_p <= 0.) k_p = 0.025
      if (lambda0 <= 0.) lambda0 = 1.0
      if (lambda1 <= 0.) lambda1 = 0.03
      if (lambda2 <= 0.) lambda2 = 0.054
      if (p_n <= 0.) p_n = 0.5
      if (chla_subco <= 0.) chla_subco = 40.0 


!!    currently, only one of the four light averaging options is coded
!!    lao must be set to this value
      lao = 2

!! convert units on k_l:read in as kJ/(m2*min), use as MJ/(m2*hr)
      k_l = k_l * 1.e-3 * 60.

!! change units from day to hour if hourly (subdaily) routing is performed
      if (ievent > 0) then
        mumax = mumax / 24.
        rhoq = rhoq / 24.
      end if

      close (101)
      return
 5100 format (a)
      end