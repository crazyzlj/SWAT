      subroutine watqual

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs in-stream nutrient transformations and water
!!    quality calculations

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ai0          |ug chla/mg alg|ratio of chlorophyll-a to algal biomass
!!    ai1          |mg N/mg alg   |fraction of algal biomass that is nitrogen
!!    ai2          |mg P/mg alg   |fraction of algal biomass that is phosphorus
!!    ai3          |mg O2/mg alg  |the rate of oxygen production per unit of
!!                                |algal photosynthesis
!!    ai4          |mg O2/mg alg  |the rate of oxygen uptake per unit of algae
!!                                |respiration
!!    ai5          |mg O2/mg N    |the rate of oxygen uptake per unit of NH3
!!                                |nitrogen oxidation
!!    ai6          |mg O2/mg N    |the rate of oxygen uptake per unit of NO2
!!                                |nitrogen oxidation
!!    algae(:)     |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:)  |mg N/L        |ammonia concentration in reach
!!    bc1(:)       |1/day         |rate constant for biological oxidation of NH3
!!                                |to NO2 in reach at 20 deg C
!!    bc2(:)       |1/day         |rate constant for biological oxidation of NO2
!!                                |to NO3 in reach at 20 deg C
!!    bc3(:)       |1/day         |rate constant for hydrolysis of organic N to
!!                                |ammonia in reach at 20 deg C
!!    bc4(:)       |1/day         |rate constant for the decay of organic P to
!!                                |dissolved P in reach at 20 deg C
!!    chlora(:)    |mg chl-a/L    |chlorophyll-a concentration in reach
!!    dayl(:)      |hours         |day length for current day
!!    disolvp(:)   |mg P/L        |dissolved phosphorus concentration in reach
!!    hru_ra(:)    |MJ/m^2        |solar radiation for the day in HRU
!!    igropt       |none          |Qual2E option for calculating the local
!!                                |specific growth rate of algae
!!                                |1: multiplicative:
!!                                |   u = mumax * fll * fnn * fpp
!!                                |2: limiting nutrient
!!                                |   u = mumax * fll * Min(fnn, fpp)
!!                                |3: harmonic mean
!!                                |   u = mumax * fll * 2. / ((1/fnn)+(1/fpp))
!!    inum1        |none          |reach number
!!    inum2        |none          |inflow hydrograph storage location number
!!    k_l          |MJ/(m2*hr)    |half saturation coefficient for light
!!    k_n          |mg N/L        |michaelis-menton half-saturation constant
!!                                |for nitrogen
!!    k_p          |mg P/L        |michaelis-menton half saturation constant
!!                                |for phosphorus
!!    lambda0      |1/m           |non-algal portion of the light extinction
!!                                |coefficient
!!    lambda1      |1/(m*ug chla/L)|linear algal self-shading coefficient
!!    lambda2      |(1/m)(ug chla/L)**(-2/3)
!!                                |nonlinear algal self-shading coefficient
!!    mumax        |1/day         |maximum specific algal growth rate at 20 deg 
!!                                |C
!!    nitraten(:)  |mg N/L        |nitrate concentration in reach
!!    nitriten(:)  |mg N/L        |nitrite concentration in reach
!!    organicn(:)  |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:)  |mg P/L        |organic phosphorus concentration in reach
!!    p_n          |none          |algal preference factor for ammonia
!!    rch_cbod(:)  |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                                |reach 
!!    rch_dox(:)   |mg O2/L       |dissolved oxygen concentration in reach
!!    rchdep       |m             |depth of flow on day
!!    rchwtr       |m^3 H2O       |water stored in reach at beginning of day
!!    rhoq         |1/day         |algal respiration rate at 20 deg C
!!    rk1(:)       |1/day         |CBOD deoxygenation rate coefficient in reach 
!!                                |at 20 deg C
!!    rk2(:)       |1/day         |reaeration rate in accordance with Fickian
!!                                |diffusion in reach at 20 deg C
!!    rk3(:)       |1/day         |rate of loss of CBOD due to settling in reach
!!                                |at 20 deg C
!!    rk4(:)       |mg O2/        |sediment oxygen demand rate in reach
!!                 |  ((m**2)*day)|at 20 deg C
!!    rnum1        |none          |fraction of overland flow
!!    rs1(:)       |m/day         |local algal settling rate in reach at 20 deg
!!                                |C
!!    rs2(:)       |(mg disP-P)/  |benthos source rate for dissolved phosphorus
!!                 |  ((m**2)*day)|in reach at 20 deg C
!!    rs3(:)       |(mg NH4-N)/   |benthos source rate for ammonia nitrogen in
!!                 |  ((m**2)*day)|reach at 20 deg C
!!    rs4(:)       |1/day         |rate coefficient for organic nitrogen
!!                                |settling in reach at 20 deg C
!!    rs5(:)       |1/day         |organic phosphorus settling rate in reach at
!!                                |20 deg C
!!    rttime       |hr            |reach travel time
!!    rtwtr        |m^3 H2O       |flow out of reach
!!    tfact        |none          |fraction of solar radiation computed in the
!!                                |temperature heat balance that is
!!                                |photosynthetically active
!!    tmpav(:)     |deg C         |average air temperature on current day in HRU
!!    varoute(2,:) |m^3 H2O       |water
!!    varoute(4,:) |kg N          |organic nitrogen
!!    varoute(5,:) |kg P          |organic posphorus
!!    varoute(6,:) |kg N          |nitrate
!!    varoute(7,:) |kg P          |soluble phosphorus
!!    varoute(13,:)|kg            |chlorophyll-a
!!    varoute(14,:)|kg N          |ammonium
!!    varoute(15,:)|kg N          |nitrite
!!    varoute(16,:)|kg            |carbonaceous biological oxygen demand
!!    varoute(17,:)|kg O2         |dissolved oxygen
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algae(:)    |mg alg/L      |algal biomass concentration in reach
!!    ammonian(:) |mg N/L        |ammonia concentration in reach
!!    chlora(:)   |mg chl-a/L    |chlorophyll-a concentration in reach
!!    disolvp(:)  |mg P/L        |dissolved phosphorus concentration in reach
!!    nitraten(:) |mg N/L        |nitrate concentration in reach
!!    nitriten(:) |mg N/L        |nitrite concentration in reach
!!    organicn(:) |mg N/L        |organic nitrogen concentration in reach
!!    organicp(:) |mg P/L        |organic phosphorus concentration in reach
!!    rch_cbod(:) |mg O2/L       |carbonaceous biochemical oxygen demand in
!!                               |reach
!!    rch_dox(:)  |mg O2/L       |dissolved oxygen concentration in reach
!!    soxy        |mg O2/L       |saturation concetration of dissolved oxygen
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    algcon      |mg alg/L      |initial algal biomass concentration in reach
!!    algi        |MJ/(m2*hr)    |daylight average, photosynthetically active,
!!                               |light intensity
!!    algin       |mg alg/L      |algal biomass concentration in inflow
!!    ammoin      |mg N/L        |ammonium N concentration in inflow
!!    bc1mod      |1/day         |rate constant for biological oxidation of NH3
!!                               |to NO2 modified to reflect impact of low 
!!                               |oxygen concentration
!!    bc2mod      |1/day         |rate constant for biological oxidation of NO2
!!                               |to NO3 modified to reflect impact of low
!!                               |oxygen concentration
!!    cbodcon     |mg/L          |initial carbonaceous biological oxygen demand
!!                               |concentration in reach
!!    cbodin      |mg/L          |carbonaceous biological oxygen demand 
!!                               |concentration in inflow
!!    chlin       |mg chl-a/L    |chlorophyll-a concentration in inflow
!!    cinn        |mg N/L        |effective available nitrogen concentration
!!    cordo       |none          |nitrification rate correction factor
!!    disoxin     |mg O2/L       |dissolved oxygen concentration in inflow
!!    dispin      |mg P/L        |soluble P concentration in inflow
!!    f1          |none          |fraction of algal nitrogen uptake from
!!                               |ammonia pool
!!    fl_1        |none          |growth attenuation factor for light, based on
!!                               |daylight-average light intensity
!!    fll         |none          |growth attenuation factor for light averaged
!!                               |over the diurnal cycle
!!    fnn         |none          |algal growth limitation factor for nitrogen
!!    fpp         |none          |algal growth limitation factor for phosphorus
!!    gra         |1/day         |local algal growth rate at 20 deg C
!!    jrch        |none          |reach number
!!    lambda      |1/m           |light extinction coefficient
!!    nh3con      |mg N/L        |initial ammonia concentration in reach
!!    nitratin    |mg N/L        |nitrate concentration in inflow
!!    nitritin    |mg N/L        |nitrite concentration in inflow
!!    no2con      |mg N/L        |initial nitrite concentration in reach
!!    no3con      |mg N/L        |initial nitrate concentration in reach
!!    o2con       |mg O2/L       |initial dissolved oxygen concentration in 
!!                               |reach
!!    orgncon     |mg N/L        |initial organic N concentration in reach
!!    orgnin      |mg N/L        |organic N concentration in inflow
!!    orgpcon     |mg P/L        |initial organic P concentration in reach
!!    orgpin      |mg P/L        |organic P concentration in inflow
!!    solpcon     |mg P/L        |initial soluble P concentration in reach
!!    tday        |none          |flow duration (fraction of 24 hr)
!!    thbc1       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NH3 to NO2
!!    thbc2       |none          |temperature adjustment factor for local
!!                               |biological oxidation of NO2 to NO3
!!    thbc3       |none          |temperature adjustment factor for local
!!                               |hydrolysis of organic N to ammonia N
!!    thbc4       |none          |temperature adjustment factor for local
!!                               |decay of organic P to dissolved P
!!    thgra       |none          |temperature adjustment factor for local algal
!!                               |growth rate
!!    thrho       |none          |temperature adjustment factor for local algal
!!                               |respiration rate
!!    thrk1       |none          |temperature adjustment factor for local CBOD
!!                               |deoxygenation
!!    thrk2       |none          |temperature adjustment factor for local oxygen
!!                               |reaeration rate
!!    thrk3       |none          |temperature adjustment factor for loss of
!!                               |CBOD due to settling
!!    thrk4       |none          |temperature adjustment factor for local
!!                               |sediment oxygen demand
!!    thrs1       |none          |temperature adjustment factor for local algal
!!                               |settling rate
!!    thrs2       |none          |temperature adjustment factor for local
!!                               |benthos source rate for dissolved phosphorus
!!    thrs3       |none          |temperature adjustment factor for local
!!                               |benthos source rate for ammonia nitrogen
!!    thrs4       |none          |temperature adjustment factor for local
!!                               |organic N settling rate
!!    thrs5       |none          |temperature adjustment factor for local
!!                               |organic P settling rate
!!    wtmp        |deg C         |temperature of water in reach
!!    wtrin       |m^3 H2O       |water flowing into reach on day
!!    uu          |varies        |variable to hold intermediate calculation
!!                               |result
!!    vv          |varies        |variable to hold intermediate calculation
!!                               |result
!!    wtrtot      |m^3 H2O       |inflow + storage water
!!    ww          |varies        |variable to hold intermediate calculation
!!                               |result
!!    xx          |varies        |variable to hold intermediate calculation
!!                               |result
!!    yy          |varies        |variable to hold intermediate calculation
!!                               |result
!!    zz          |varies        |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log, Exp, Min
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch
      real*8 :: wtrin, chlin, algin, orgnin, ammoin, nitratin, nitritin
      real*8 :: orgpin, dispin, cbodin, disoxin, tday, fll, gra
      real*8 :: lambda, fnn, fpp, algi, fl_1, xx, yy, zz, ww, cinn
      real*8 :: uu, vv, cordo, f1, algcon, orgncon, nh3con, no2con, no3con
      real*8 :: orgpcon, solpcon, cbodcon, o2con, wtrtot, bc1mod, bc2mod
      real*8 :: thgra = 1.047, thrho = 1.047, thrs1 = 1.024
      real*8 :: thrs2 = 1.074, thrs3 = 1.074, thrs4 = 1.024, thrs5 = 1.024
      real*8 :: thbc1 = 1.083, thbc2 = 1.047, thbc3 = 1.047, thbc4 = 1.047
      real*8 :: thrk1 = 1.047, thrk2 = 1.024, thrk3 = 1.024, thrk4 = 1.060
!      real*8 :: thrk5 = 1.047, thrk6 = 1.0, thrs6 = 1.024, thrs7 = 1.0

      jrch = 0
      jrch = inum1
      dcoef= 3.

       !! initialize water flowing into reach
       wtrin = 0.
       wtrin = varoute(2,inum2) * (1. - rnum1)

       if (wtrin > 1.e-4) then
!! concentrations
         !! initialize inflow concentrations
         chlin = 0.
         algin = 0.
         orgnin = 0.
         ammoin = 0.
         nitritin = 0.
         nitratin = 0.
         orgpin = 0.
         dispin = 0.
         cbodin = 0.
         disoxin = 0.
         cinn = 0.
         if (wtrin > 0.001) then
         chlin = 1000. * varoute(13,inum2) * (1. - rnum1) / wtrin
         algin = 1000. * chlin / ai0        !! QUAL2E equation III-1
         orgnin = 1000. * varoute(4,inum2) * (1. - rnum1) / wtrin
         ammoin = 1000. * varoute(14,inum2) * (1. - rnum1) / wtrin
         nitritin = 1000. * varoute(15,inum2) * (1. - rnum1) / wtrin
         nitratin = 1000. * varoute(6,inum2) * (1. - rnum1) / wtrin
         orgpin = 1000. * varoute(5,inum2) * (1. - rnum1) / wtrin
         dispin = 1000. * varoute(7,inum2) * (1. - rnum1) / wtrin
         cbodin = 1000. * varoute(16,inum2) * (1. - rnum1) / wtrin
         disoxin = 1000. * varoute(17,inum2) * (1. - rnum1) / wtrin
         end if

         !! initialize concentration of nutrient in reach
         wtrtot = 0.
         algcon = 0.
         orgncon = 0.
         nh3con = 0.
         no2con = 0.
         no3con = 0.
         orgpcon = 0.
         solpcon = 0.
         cbodcon = 0.
         o2con = 0.
         rch_cbod(jrch) = dmax1(1.e-6_8,rch_cbod(jrch))
         wtrtot = wtrin + rchwtr
         algcon = (algin * wtrin + algae(jrch) * rchwtr) / wtrtot
         orgncon = (orgnin * wtrin + organicn(jrch) * rchwtr) / wtrtot
         nh3con = (ammoin * wtrin + ammonian(jrch) * rchwtr) / wtrtot
         no2con = (nitritin * wtrin + nitriten(jrch) * rchwtr) / wtrtot
         no3con = (nitratin * wtrin + nitraten(jrch) * rchwtr) / wtrtot
         orgpcon = (orgpin * wtrin + organicp(jrch) * rchwtr) / wtrtot
         solpcon = (dispin * wtrin + disolvp(jrch) * rchwtr) / wtrtot
         cbodcon = (cbodin * wtrin + rch_cbod(jrch) * rchwtr) / wtrtot
         o2con = (disoxin * wtrin + rch_dox(jrch) * rchwtr) / wtrtot

         if (orgncon < 1.e-6) orgncon = 0.0
	   if (nh3con < 1.e-6) nh3con = 0.0
	   if (no2con < 1.e-6) no2con = 0.0
	   if (no3con < 1.e-6) no3con = 0.0
	   if (orgpcon < 1.e-6) orgpcon = 0.0
	   if (solpcon < 1.e-6) solpcon = 0.0
	   if (cbodcon < 1.e-6) cbodcon = 0.0
	   if (o2con < 1.e-6) o2con = 0.0

         !! calculate temperature in stream
         !! Stefan and Preudhomme. 1993.  Stream temperature estimation 
         !! from air temperature.  Water Res. Bull. p. 27-45
         !! SWAT manual equation 2.3.13
         call temparms
         !wtmp = 5.0 + 0.75 * tmpav(jrch)
         if (wtmp <= 0.) wtmp = 0.1

         !! calculate effective concentration of available nitrogen
         !! QUAL2E equation III-15
         cinn = nh3con + no3con

         !! calculate saturation concentration for dissolved oxygen
         !! QUAL2E section 3.6.1 equation III-29
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         ww = -139.34410 + (1.575701e05 / (wtmp + 273.15))
         xx = 6.642308e07 / ((wtmp + 273.15)**2)
         yy = 1.243800e10 / ((wtmp + 273.15)**3)
         zz = 8.621949e11 / ((wtmp + 273.15)**4)
         soxy = Exp(ww - xx + yy - zz)
         if (soxy < 1.e-6) soxy = 0. 
!! end initialize concentrations

!! O2 impact calculations
        !! calculate nitrification rate correction factor for low
        !! oxygen QUAL2E equation III-21
        cordo = 0.
	if (o2con.le.0.001) o2con=0.001
	if (o2con.gt.30.) o2con=30.
        cordo = 1.0 - Exp(-0.6 * o2con)
        !! modify ammonia and nitrite oxidation rates to account for
        !! low oxygen
        bc1mod = 0.
        bc2mod = 0.
        bc1mod = bc1(jrch) * cordo
        bc2mod = bc2(jrch) * cordo
!! end O2 impact calculations

         !! calculate flow duration
         tday = 0.
         tday = rttime / 24.0
         if (tday > 1.0) tday = 1.0
    !!     tday = 1.0

!! algal growth
         !! calculate light extinction coefficient 
         !! (algal self shading) QUAL2E equation III-12
         if (ai0 * algcon > 1.e-6) then
           lambda = lambda0 + (lambda1 * ai0 * algcon) + lambda2 *    
     &                                        (ai0 * algcon) ** (.66667)
         else
           lambda = lambda0
         endif

	   If (lambda > lambda0) lambda = lambda0

         !! calculate algal growth limitation factors for nitrogen
         !! and phosphorus QUAL2E equations III-13 & III-14
         fnn = 0.
         fpp = 0.
         fnn = cinn / (cinn + k_n)
         fpp = solpcon / (solpcon + k_p)

         !! calculate daylight average, photosynthetically active,
         !! light intensity QUAL2E equation III-8
         !! Light Averaging Option # 2
         algi = 0.
         if (dayl(hru1(jrch)) > 0.) then
           algi = hru_ra(hru1(jrch)) * tfact / dayl(hru1(jrch))
         else
           algi = 0.
         end if

         !! calculate growth attenuation factor for light, based on
         !! daylight average light intensity QUAL2E equation III-7b
         fl_1 = 0.
         fll = 0.
         fl_1 = (1. / (lambda * rchdep)) *                            
     &        Log((k_l + algi) / (k_l + algi * (Exp(-lambda * rchdep))))
         fll = 0.92 * (dayl(hru1(jrch)) / 24.) * fl_1

         !! calculcate local algal growth rate
         gra = 0.
         select case (igropt)
           case (1)
             !! multiplicative QUAL2E equation III-3a
             gra = mumax * fll * fnn * fpp
           case (2)
             !! limiting nutrient QUAL2E equation III-3b
             gra = mumax * fll * Min(fnn, fpp)
           case (3)
             !! harmonic mean QUAL2E equation III-3c
             if (fnn > 1.e-6 .and. fpp > 1.e-6) then
               gra = mumax * fll * 2. / ((1. / fnn) + (1. / fpp))
             else
               gra = 0.
             endif
         end select

         !! calculate algal biomass concentration at end of day
         !! (phytoplanktonic algae)
         !! QUAL2E equation III-2
         algae(jrch) = 0.
         algae(jrch) = algcon + (Theta(gra,thgra,wtmp) * algcon -       
     &    Theta(rhoq,thrho,wtmp) * algcon - Theta(rs1(jrch),thrs1,wtmp) 
     &                                         / rchdep * algcon) * tday
         if (algae(jrch) < 1.e-6) algae(jrch) = 0.
	!! JGA added to set algae limit *****
	   if (algae(jrch) > 5000.) algae(jrch) = 5000.
         if (algae(jrch) > dcoef * algcon) algae(jrch) = dcoef * algcon

         !! calculate chlorophyll-a concentration at end of day
         !! QUAL2E equation III-1
         chlora(jrch) = 0.
         chlora(jrch) = algae(jrch) * ai0 / 1000.
         !! end algal growth 

!! oxygen calculations
         !! calculate carbonaceous biological oxygen demand at end
         !! of day QUAL2E section 3.5 equation III-26
         yy = 0.
         zz = 0.
         yy = Theta(rk1(jrch),thrk1,wtmp) * cbodcon
         zz = Theta(rk3(jrch),thrk3,wtmp) * cbodcon
         rch_cbod(jrch) = 0.
         rch_cbod(jrch) = cbodcon - (yy + zz) * tday
         
         !!deoxygenation rate
         coef = exp(-Theta(rk1(jrch),thrk1,wtmp) * tday)
         cbodrch = coef * cbodcon
         !!cbod rate loss due to settling
         coef = exp(-Theta(rk3(jrch),thrk3,wtmp) * tday)
         cbodrch = coef * cbodrch
         
         rch_cbod(jrch) = cbodrch
         if (rch_cbod(jrch) < 1.e-6) rch_cbod(jrch) = 0.
	   if (rch_cbod(jrch) > dcoef * cbodcon) rch_cbod(jrch) = dcoef * 
     &	   cbodcon

         !! calculate dissolved oxygen concentration if reach at 
         !! end of day QUAL2E section 3.6 equation III-28
         uu = 0.
         vv = 0.
         ww = 0.
         xx = 0.
         yy = 0.
         zz = 0.
         rhoq = 1.0
         rk2(jrch) = 1.0
         uu = Theta(rk2(jrch),thrk2,wtmp) * (soxy - o2con)
         vv = (ai3 * Theta(gra,thgra,wtmp) - ai4 *                      
     &                                  Theta(rhoq,thrho,wtmp)) * algcon
         ww = Theta(rk1(jrch),thrk1,wtmp) * cbodcon
         xx = Theta(rk4(jrch),thrk4,wtmp) / (rchdep * 1000.)
         yy = ai5 * Theta(bc1mod,thbc1,wtmp) * nh3con
         zz = ai6 * Theta(bc2mod,thbc2,wtmp) * no2con
         rch_dox(jrch) = o2con + (uu + vv - ww - xx - yy - zz) * tday
         rch_dox(jrch) = dmin1(0.1_8, rch_dox(jrch))
         
         !algea O2 production minus respiration
         !if (vv > 0.) then
           doxrch = soxy
         !else
         !  coef = exp(-0.03 * vv)
         !  doxrch = coef * soxy
         !end if
         
         !cbod deoxygenation
         coef = exp(-0.1 * ww)
         doxrch = coef * doxrch
         
         !benthic sediment oxidation
         coef = 1. - (Theta(rk4(jrch),thrk4,wtmp) / 100.)
         doxrch = coef * doxrch
         
         !ammonia oxydation
         coef = exp(-0.05 * yy)
         doxrch = coef * doxrch
         
         !nitrite oxydation
         coef = exp(-0.05 * zz)
         doxrch = coef * doxrch
         
         !reaeration
         uu = Theta(rk2(jrch),thrk2,wtmp) / 100. * (soxy - doxrch)
         rch_dox(jrch) = doxrch + uu
         
         if (rch_dox(jrch) < 1.e-6) rch_dox(jrch) = 0.
         if (rch_dox(jrch) > soxy) rch_dox(jrch) = soxy
         !if (rch_dox(jrch) > dcoef * o2con) rch_dox(jrch)= dcoef * o2con
!! end oxygen calculations

!! nitrogen calculations
         !! calculate organic N concentration at end of day
         !! QUAL2E section 3.3.1 equation III-16
         xx = 0.
         yy = 0.
         zz = 0.
         xx = ai1 * Theta(rhoq,thrho,wtmp) * algcon
         yy = Theta(bc3(jrch),thbc3,wtmp) * orgncon
         zz = Theta(rs4(jrch),thrs4,wtmp) * orgncon
!        red_fac = orgncon / 4.
!        if (red_fac > 0.75) red_fac = 0.75
!        zz = zz + red_fac
         organicn(jrch) = 0.
         organicn(jrch) = orgncon + (xx - yy - zz) * tday
         if (organicn(jrch) < 1.e-6) organicn(jrch) = 0.
   	   if(organicn(jrch) > dcoef * orgncon) organicn(jrch) = dcoef * 
     & 	orgncon

        !! calculate fraction of algal nitrogen uptake from ammonia
        !! pool QUAL2E equation III-18
        f1 = 0.
        f1 = p_n * nh3con / (p_n * nh3con + (1. - p_n) * no3con +       
     &                                                            1.e-6)

        !! calculate ammonia nitrogen concentration at end of day
        !! QUAL2E section 3.3.2 equation III-17
        ww = 0.
        xx = 0.
        yy = 0.
        zz = 0.
        ww = Theta(bc3(jrch),thbc3,wtmp) * orgncon
        xx = Theta(bc1mod,thbc1,wtmp) * nh3con
        yy = Theta(rs3(jrch),thrs3,wtmp) / (rchdep * 1000.)
        zz = f1 * ai1 * algcon * Theta(gra,thgra,wtmp)
        ammonian(jrch) = 0.
        ammonian(jrch) = nh3con + (ww - xx + yy - zz) * tday
        if (ammonian(jrch) < 1.e-6) ammonian(jrch) = 0.
        if (ammonian(jrch) > dcoef * nh3con .and. nh3con > 0.) 
     &   ammonian(jrch) = dcoef * nh3con  

        !! calculate concentration of nitrite at end of day
        !! QUAL2E section 3.3.3 equation III-19
        yy = 0.
        zz = 0.
        yy = Theta(bc1mod,thbc1,wtmp) * nh3con
        zz = Theta(bc2mod,thbc2,wtmp) * no2con
        nitriten(jrch) = 0.
        nitriten(jrch) = no2con + (yy - zz) * tday
        if (nitriten(jrch) < 1.e-6) nitriten(jrch) = 0.
	  if (nitriten(jrch) > dcoef * no2con .and. no2con > 0.) 
     &  nitriten(jrch) = dcoef * no2con

        !! calculate nitrate concentration at end of day
        !! QUAL2E section 3.3.4 equation III-20
        yy = 0.
        zz = 0.
        yy = Theta(bc2mod,thbc2,wtmp) * no2con
        zz = (1. - f1) * ai1 * algcon * Theta(gra,thgra,wtmp)
        nitraten(jrch) = 0.
        nitraten(jrch) = no3con + (yy - zz) * tday
        if (nitraten(jrch) > dcoef * no3con) nitraten(jrch) = dcoef * 
     &	   no3con
	
        if (nitraten(jrch) < 1.e-6) nitraten(jrch) = 0.
!! end nitrogen calculations

!! phosphorus calculations
        !! calculate organic phosphorus concentration at end of
        !! day QUAL2E section 3.3.6 equation III-24
        xx = 0.
        yy = 0.
        zz = 0.
        xx = ai2 * Theta(rhoq,thrho,wtmp) * algcon
        yy = Theta(bc4(jrch),thbc4,wtmp) * orgpcon
        zz = Theta(rs5(jrch),thrs5,wtmp) * orgpcon
        organicp(jrch) = 0.
        organicp(jrch) = orgpcon + (xx - yy - zz) * tday
        if (organicp(jrch) < 1.e-6) organicp(jrch) = 0.
        if (organicp(jrch) > dcoef * orgpcon) organicp(jrch) = dcoef * 
     &	orgpcon

        !! calculate dissolved phosphorus concentration at end
        !! of day QUAL2E section 3.4.2 equation III-25
        xx = 0.
        yy = 0.
        zz = 0.
        xx = Theta(bc4(jrch),thbc4,wtmp) * orgpcon
        yy = Theta(rs2(jrch),thrs2,wtmp) / (rchdep * 1000.)
        zz = ai2 * Theta(gra,thgra,wtmp) * algcon
        disolvp(jrch) = 0.
        disolvp(jrch) = solpcon + (xx + yy - zz) * tday
        if (disolvp(jrch) < 1.e-6) disolvp(jrch) = 0.
	  if (disolvp(jrch) > dcoef * solpcon) disolvp(jrch) = dcoef * 
     &    solpcon   
!! end phosphorus calculations

      else
        !! all water quality variables set to zero when no flow
        algin = 0.0
        chlin = 0.0
        orgnin = 0.0
        ammoin = 0.0
        nitritin = 0.0
        nitratin = 0.0
        orgpin = 0.0
        dispin = 0.0
        cbodin = 0.0
        disoxin = 0.0
        algae(jrch) = 0.0
        chlora(jrch) = 0.0
        organicn(jrch) = 0.0
        ammonian(jrch) = 0.0
        nitriten(jrch) = 0.0
        nitraten(jrch) = 0.0
        organicp(jrch) = 0.0
        disolvp(jrch) = 0.0
        rch_cbod(jrch) = 0.0
        rch_dox(jrch) = 0.0
        soxy = 0.0
        orgncon = 0.0
      endif

!!!! commented following statements per conversation with 
!!!! srini 10/22/08
!    write for srinisan 12/07/2004
!!    write added back 03/02/2010 - per Srin email
      if (ihumus == 1) then
         write (82,5000) jrch, i, tmpav(jrch),
     *   chlin, chlora(jrch), orgncon, organicn(jrch),
     *   ammoin, ammonian(jrch), nitritin, nitriten(jrch),
     *   nitratin, nitraten(jrch), orgpin, organicp(jrch),
     *   dispin, disolvp(jrch), cbodin, rch_cbod(jrch), soxy, 
     *   disoxin, rch_dox(jrch), varoute (2,inum2), rttime
 5000    format ('REACH', i4, i5, 22e12.4)
      end if

      return
      end
