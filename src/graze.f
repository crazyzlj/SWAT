      subroutine graze
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates biomass lost to grazing

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkddb(:)  |none          |bacteria partition coefficient:
!!                                |1: all bacteria in solution
!!                                |0: all bacteria sorbed to soil particles
!!    bactlp_plt(:)|# cfu/m^2     |less persistent bacteria on foliage
!!    bactlpdb(:)  |# cfu/g       |concentration of less persistent
!!                                |bacteria in manure(fertilizer)
!!    bactlpq(:)   |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)   |# cfu/m^2     |less persistent bacteria attached to soil
!!                                |particles
!!    bactp_plt(:) |# cfu/m^2     |persistent bacteria on foliage
!!    bactpdb(:)   |# cfu/g       |concentration of persistent bacteria
!!                                |in manure(fertilizer)
!!    bactpq(:)    |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)    |# cfu/m^2     |persistent bacteria attached to soil particles
!!    bio_min(:)   |kg/ha         |minimum plant biomass for grazing
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_eat(:)   |(kg/ha)/day   |dry weight of biomass removed by grazing
!!                                |daily
!!    bio_trmp(:)  |(kg/ha)/day   |dry weight of biomass removed by
!!                                |trampling daily
!!    curyr        |none          |current year of simulation
!!    fminn(:)     |kg minN/kg frt|fraction of mineral N (NO3 + NH3) in 
!!                                |fertilizer/manure
!!    fminp(:)     |kg minP/kg frt|fraction of mineral P in fertilizer/manure
!!    fnh3n(:)     |kg NH3-N/kg minN|fraction of NH3-N in mineral N in 
!!                                |fertilizer/manure
!!    forgn(:)     |kg orgN/kg frt|fraction of organic N in fertilizer/manure
!!    forgp(:)     |kg orgP/kg frt|fraction of organic P in fertilizer/manure
!!    grazn        |kg N/ha       |total amount of nitrogen applied to soil
!!                                |during grazing in HRU on day
!!    grazp        |kg P/ha       |total amount of phosphorus applied to soil
!!                                |during grazing in HRU on day
!!    hru_dafr(:)  |km**2/km**2   |fraction of watershed area in HRU
!!    icr(:)       |none          |sequence number of crop grown within the
!!                                |current year
!!    iida         |julian date   |day being simulated (current julian day
!!    manure_id(:) |none          |manure (fertilizer) identification
!!                                |number from fert.dat
!!    igrz(:)      |none          |grazing flag for HRU:
!!                                |0 HRU currently not grazed
!!                                |1 HRU currently grazed
!!    ihru         |none          |HRU number
!!    laiday(:)    |m**2/m**2     |leaf area index
!!    grz_days(:)  |none          |number of days grazing will be simulated
!!    ngr(:)       |none          |sequence number of grazing operation
!!                                |within the year
!!    nro(:)       |none          |sequence number of year in rotation
!!    nyskip       |none          |number of years to skip output summarization
!!                                |and printing
!!    phuacc(:)    |none          |fraction of plant heat units accumulated
!!    phug(:,:,:)  |none          |fraction of plant heat units at which
!!                                |grazing begins
!!    plantn(:)    |kg N/ha       |amount of nitrogen in plant
!!    plantp(:)    |kg P/ha       |amount of phosphorus in plant
!!    pltfr_n(:)   |none          |fraction of plant biomass that is nitrogen
!!    pltfr_p(:)   |none          |fraction of plant biomass that is phosphorus
!!    sol_bd(:,:)  |Mg/m**3       |bulk density of the soil
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_nh3(:,:) |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                |pool in soil layer
!!    sol_no3(:,:) |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                                |in soil layer
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    sol_solp(:,:)|kg P/ha       |amount of phosohorus stored in solution
!!    sol_z(:,:)   |mm            |depth to bottom of soil layer
!!    manure_kg(:) |(kg/ha)/day   |dry weight of manure deposited on HRU
!!                                |daily
!!    wshd_fminp   |kg P/ha       |average annual amount of mineral P applied
!!                                |in watershed
!!    wshd_fnh3    |kg N/ha       |average annual amount of NH3-N applied in
!!                                |watershed
!!    wshd_fno3    |kg N/ha       |average annual amount of NO3-N applied in
!!                                |watershed
!!    wshd_orgn    |kg N/ha       |average annual amount of organic N applied
!!                                |in watershed
!!    wshd_orgp    |kg P/ha       |average annual amount of organic P applied
!!                                |in watershed
!!    wshd_ftotn   |kg N/ha       |average annual amount of N (mineral &
!!                                |organic) applied in watershed
!!    wshd_ftotp   |kg P/ha       |average annual amount of P (mineral &
!!                                |organic) applied in watershed
!!    yldkg(:,:,:) |kg/ha         |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlp_plt(:)|# cfu/m^2    |less persistent bacteria on foliage
!!    bactlpq(:)  |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)  |# cfu/m^2     |less persistent bacteria attached to soil
!!                               |particles
!!    bactp_plt(:)|# cfu/m^2     |persistent bacteria on foliage
!!    bactpq(:)   |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)   |# cfu/m^2     |persistent bacteria attached to soil particles
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    grazn       |kg N/ha       |total amount of nitrogen applied to soil
!!                               |during grazing in HRU on day
!!    grazp       |kg P/ha       |total amount of phosphorus applied to soil
!!                               |during grazing in HRU on day
!!    igrz(:)     |none          |grazing flag for HRU:
!!                               |0 HRU currently not grazed
!!                               |1 HRU currently grazed
!!    laiday(:)   |m**2/m**2     |leaf area index
!!    ndeat(:)    |days          |number of days HRU has been grazed
!!    ngr(:)      |none          |sequence number of grazing operation
!!                               |within the year
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant
!!    plantp(:)   |kg P/ha       |amount of phosphorus in plant
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool
!!    sol_nh3(:,:)|kg N/ha       |amount of nitrogen stored in the ammonium
!!                               |pool in soil layer
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in soil layer
!!    sol_rsd(:,:)|kg/ha         |amount of organic matter in the soil
!!                               |classified as residue
!!    sol_solp(:,:)|kg P/ha      |amount of phosohorus stored in solution
!!    wshd_fminp  |kg P/ha       |average annual amount of mineral P applied
!!                               |in watershed
!!    wshd_fnh3   |kg N/ha       |average annual amount of NH3-N applied in
!!                               |watershed
!!    wshd_fno3   |kg N/ha       |average annual amount of NO3-N applied in
!!                               |watershed
!!    wshd_orgn   |kg N/ha       |average annual amount of organic N applied
!!                               |in watershed
!!    wshd_orgp   |kg P/ha       |average annual amount of organic P applied
!!                               |in watershed
!!    wshd_ftotn  |kg N/ha       |average annual amount of N (mineral &
!!                               |organic) applied in watershed
!!    wshd_ftotp  |kg P/ha       |average annual amount of P (mineral &
!!                               |organic) applied in watershed
!!    yldkg(:,:,:)|kg/ha         |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dmi         |kg/ha         |biomass in HRU prior to grazing
!!    dmii        |kg/ha         |biomass prior to trampling
!!    frt_t       |
!!    gc          |
!!    gc1         |
!!    it          |none          |manure/fertilizer id number from fert.dat
!!    j           |none          |HRU number
!!    l           |none          |number of soil layer that manure is applied
!!    swf         |
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, l, it
      real :: dmi, dmii, gc, gc1, swf, frt_t, xx

      j = 0
      j = ihru

!! graze only if adequate biomass in HRU
      if (bio_ms(j) > bio_min(j)) then

        !! determine new biomass in HRU
        dmi = 0.
        dmi = bio_ms(j)
        bio_ms(j) = bio_ms(j) - bio_eat(j)
        if (bio_ms(j) < bio_min(j)) bio_ms(j) = bio_min(j)

        !! adjust nutrient content of biomass
        plantn(j) = plantn(j) - (dmi - bio_ms(j)) * pltfr_n(j)
        plantp(j) = plantp(j) - (dmi - bio_ms(j)) * pltfr_p(j)
        if (plantn(j) < 0.) plantn(j) = 0.
        if (plantp(j) < 0.) plantp(j) = 0.

        !! remove trampled biomass and add to residue
        dmii = 0.
        dmii = bio_ms(j)
        bio_ms(j) = bio_ms(j) - bio_trmp(j)                   
        if (bio_ms(j) < bio_min(j))  then
          sol_rsd(1,j) = sol_rsd(1,j) + dmii - bio_min(j)
          bio_ms(j) = bio_min(j)
        else
          sol_rsd(1,j) = sol_rsd(1,j) + bio_trmp(j)                    
        endif
        sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
        bio_ms(j) = Max(bio_ms(j),0.)

        !! adjust nutrient content of residue and biomass for
        !! trampling
        plantn(j) = plantn(j) - (dmii - bio_ms(j)) * pltfr_n(j)
        plantp(j) = plantp(j) - (dmii - bio_ms(j)) * pltfr_p(j)
        if (plantn(j) < 0.) plantn(j) = 0.
        if (plantp(j) < 0.) plantp(j) = 0.
        if (dmii - bio_ms(j) > 0.) then
          sol_fon(1,j) = (dmii - bio_ms(j)) * pltfr_n(j) + sol_fon(1,j)
          sol_fop(1,j) = (dmii - bio_ms(j)) * pltfr_p(j) + sol_fop(1,j) 
        end if


        !! apply manure
        it = 0
        it = manure_id(j)
        if (manure_kg(j) > 0.) then 
          l = 1
          
          if (cswat == 0) then

          sol_no3(l,j) = sol_no3(l,j) + manure_kg(j)               *    &
     &                 (1. - fnh3n(it)) * fminn(it)
          sol_fon(l,j) = sol_fon(l,j) + manure_kg(j)               *    &
     &                 forgn(it)
          sol_nh3(l,j) = sol_nh3(l,j) + manure_kg(j)               *    &
     &                 fnh3n(it) * fminn(it)
          sol_solp(l,j) = sol_solp(l,j) + manure_kg(j)             *    &
     &                 fminp(it)
          sol_fop(l,j) = sol_fop(l,j) + manure_kg(j)               *    &
     &                 forgp(it)
          else
          sol_no3(l,j) = sol_no3(l,j) + manure_kg(j)               *    &
     &                 (1. - fnh3n(it)) * fminn(it)
          sol_mn(l,j) = sol_mn(l,j) + manure_kg(j)                 *
     &                 forgn(it)
          sol_nh3(l,j) = sol_nh3(l,j) + manure_kg(j)               *    &
     &                 fnh3n(it) * fminn(it)
          sol_solp(l,j) = sol_solp(l,j) + manure_kg(j)             *    &
     &                 fminp(it)
          sol_mp(l,j) = sol_mp(l,j) + manure_kg(j)               *
     &                 forgp(it)          
          sol_mc(l,j) = sol_mc(l,j) + manure_kg(j)               *
     &                 forgn(it) * 10.
          end if

!! add bacteria - #cfu/g * t(manure)/ha * 1.e6 g/t * ha/10,000 m^2 = 100.
!! calculate ground cover
          gc = 0.
          gc = (1.99532 - Erfc(1.333 * laiday(j) - 2.)) / 2.1
          if (gc < 0.) gc = 0.

          gc1 = 0.
          gc1 = 1. - gc

          swf = .15

          frt_t = 0.
          frt_t = bact_swf * manure_kg(j) / 1000.

          bactp_plt(j) = gc * bactpdb(it) * frt_t * 100. + bactp_plt(j)
          bactlp_plt(j) = gc * bactlpdb(it) * frt_t * 100.+bactlp_plt(j)

          bactpq(j) = gc1 * bactpdb(it)  * frt_t * 100. + bactpq(j)
          bactpq(j) = bactkddb(it) * bactpq(j)

          bactps(j) = gc1 * bactpdb(it) * frt_t * 100. + bactps(j)
          bactps(j) = (1. - bactkddb(it)) * bactps(j)

          bactlpq(j) = gc1 * bactlpdb(it) * frt_t * 100. + bactlpq(j)     
          bactlpq(j) = bactkddb(it) * bactlpq(j)

          bactlps(j) = gc1 * bactlpdb(it) * frt_t * 100. + bactlps(j)
          bactlps(j) = (1. - bactkddb(it)) * bactlps(j)

        endif

        !! reset leaf area index and fraction of growing season
        if (dmi > 1.) then
          laiday(j) = laiday(j) * bio_ms(j) / dmi
          phuacc(j) = phuacc(j) * bio_ms(j) / dmi
        else
          laiday(j) = 0.05
          phuacc(j) = 0.
        endif


        !! summary calculations
        !! I do not understand these summary calculations Armen March 2009
        grazn = grazn + manure_kg(j)               *                    &
     &               (fminn(it) + forgn(it))
        grazp = grazp + manure_kg(j)               *                    &
     &               (fminp(it) + forgp(it))
        tgrazn(j) = tgrazn(j) + grazn
        tgrazp(j) = tgrazp(j) + grazp

        if (curyr > nyskip) then
          wshd_ftotn = wshd_ftotn + manure_kg(j)               *        &
     &               hru_dafr(j) * (fminn(it) + forgn(it))
          wshd_forgn = wshd_forgn + manure_kg(j)               *        &
     &               hru_dafr(j) * forgn(it)
          wshd_fno3 = wshd_fno3 + manure_kg(j)               *          &
     &               hru_dafr(j) * fminn(it) * (1. - fnh3n(it))
          wshd_fnh3 = wshd_fnh3 + manure_kg(j)               *          &
     &               hru_dafr(j) * fminn(it) * fnh3n(it)
          wshd_ftotp = wshd_ftotp + manure_kg(j)               *        &
     &               hru_dafr(j) * (fminp(it) + forgp(it))
          wshd_fminp = wshd_fminp + manure_kg(j)               *        &
     &               hru_dafr(j) * fminp(it)
          wshd_forgp = wshd_forgp + manure_kg(j)               *        &
     &               hru_dafr(j) * forgp(it)
  !       yldkg(nro(j),1,j) = yldkg(nro(j),1,j) + (dmi - bio_ms(j))
      yldkg(icr(j),j)=yldkg(icr(j),j) + (dmi - bio_ms(j))
        end if
      end if

!! check to set if grazing period is over
      if (ndeat(j) == grz_days(j)) then
        igrz(j) = 0
        ndeat(j) = 0
        ngr(j) = ngr(j) + 1
      end if


      return
      end
