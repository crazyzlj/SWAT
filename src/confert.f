      subroutine confert
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates a continuous fertilizer operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkddb(:)  |none          |bacteria partition coefficient:
!!                                |1: all bacteria in solution
!!                                |0: all bacteria sorbed to soil particles
!!    bactlp_plt(:)|# colonies/ha |less persistent bacteria on foliage
!!    bactlpdb(:)  |# bact/kg man |concentration of less persistent
!!                                |bacteria in manure(fertilizer)
!!    bactlpq(:)   |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)   |# colonies/ha |less persistent bacteria attached to soil
!!                                |particles
!!    bactp_plt(:) |#colonies/ha  |persistent bacteria on foliage
!!    bactpdb(:)   |# bact/kg man |concentration of persistent bacteria
!!                                |in manure(fertilizer)
!!    bactpq(:)    |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)    |# colonies/ha |persistent bacteria attached to soil particles
!!    cfrt_id(:,:,:)|none         |manure (fertilizer) identification
!!                                |number from fert.dat
!!    cfrt_kg(:,:,:)|(kg/ha)/day  |dry weight of fertilizer/manure deposited
!!                                |on HRU daily
!!    curyr        |none          |current year of simulation
!!    fminn(:)     |kg minN/kg frt|fraction of mineral N (NO3 + NH3) in 
!!                                |fertilizer/manure
!!    fminp(:)     |kg minP/kg frt|fraction of mineral P in fertilizer/manure
!!    fnh3n(:)     |kg NH3-N/kg minN|fraction of NH3-N in mineral N in 
!!                                |fertilizer/manure
!!    forgn(:)     |kg orgN/kg frt|fraction of organic N in fertilizer/manure
!!    forgp(:)     |kg orgP/kg frt|fraction of organic P in fertilizer/manure
!!    cfertn       |kg N/ha       |total amount of nitrogen applied to soil
!!                                |during continuous fertilizer operation in 
!!                                |HRU on day
!!    cfertp       |kg P/ha       |total amount of phosphorus applied to soil
!!                                |during continuous fertilizer operation in 
!!                                |HRU on day
!!    hru_dafr(:)  |km**2/km**2   |fraction of watershed area in HRU
!!    icfert(:,:,:)|julian date   |date continuous fertilizer operation begins
!!    icfrt(:)     |none          |continuous fert flag for HRU:
!!                                |0 HRU currently not continuously fertilized
!!                                |1 HRU currently continuously fertilized
!!    iida         |julian date   |day being simulated (current julian day
!!    ihru         |none          |HRU number
!!    laiday(:)    |m**2/m**2     |leaf area index
!!    ncf(:)       |none          |sequence number of continuous fertilizer
!!                                |operation within the year
!!    ndcfrt(:)    |days          |number of days HRU has been continuously
!!                                |fertilized
!!    fert_days(:,:,:)|none          |number of days continuous fertilization
!!                                |will be simulated
!!    nro(:)       |none          |sequence number of year in rotation
!!    nyskip       |none          |number of years to skip output summarization
!!                                |and printing
!!    phuacc(:)    |none          |fraction of plant heat units accumulated
!!    phucf(:,:,:) |none          |fraction of plant heat units at which
!!                                |continuous fertilization begins
!!    sol_bd(:,:)  |Mg/m**3       |bulk density of the soil
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_nh3(:,:) |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                |pool in soil layer
!!    sol_no3(:,:) |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                                |in soil layer
!!    sol_solp(:,:)|kg P/ha       |amount of phosohorus stored in solution
!!    sol_z(:,:)   |mm            |depth to bottom of soil layer
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlp_plt(:)|# colonies/ha|less persistent bacteria on foliage
!!    bactlpq(:)  |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)  |# colonies/ha |less persistent bacteria attached to soil
!!                               |particles
!!    bactp_plt(:)|# colonies/ha |persistent bacteria on foliage
!!    bactpq(:)   |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)   |# colonies/ha |persistent bacteria attached to soil particles
!!    cfertn      |kg N/ha       |total amount of nitrogen applied to soil
!!                               |during continuous fertilizer operation in 
!!                               |HRU on day
!!    cfertp      |kg P/ha       |total amount of phosphorus applied to soil
!!                               |during continuous fertilizer operation in 
!!                               |HRU on day
!!    icfrt(:)    |none          |continuous fertilizer flag for HRU:
!!                               |0 HRU currently not continuously fertilized
!!                               |1 HRU currently continuously fertilized
!!    ncf(:)      |none          |sequence number of continuous fertilizer
!!                               |operation within the year
!!    ndcfrt(:)   |days          |number of days HRU has been continuously
!!                               |fertilized
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool
!!    sol_nh3(:,:)|kg N/ha       |amount of nitrogen stored in the ammonium
!!                               |pool in soil layer
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in soil layer
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
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
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
      real :: gc, gc1, swf, frt_t, xx

      j = 0
      j = ihru

!! if continuous fertilization not currently on, check to see if it is time
!! to initialize continuous fertilization
      if (icfrt(j) == 0) then
        if (icfert(nro(j),ncf(j),j) > 0 .and.                           &
     &                             iida >= icfert(nro(j),ncf(j),j)) then
          icfrt(j) = 1
          ndcfrt(j) = 1
          iday_fert(j) = ifrt_freq(nro(j),ncf(j),j)
        else if (phuacc(j) > phucf(nro(j),ncf(j),j)) then
          icfrt(j) = 1
          ndcfrt(j) = 1
          iday_fert(j) = ifrt_freq(nro(j),ncf(j),j)
        else
          return
        end if
      else
        !! if not first day of continuous fert increment total days of 
        !! continuous fert by one
        ndcfrt(j) = ndcfrt(j) + 1
      end if

      if (iday_fert(j) == ifrt_freq(nro(j),ncf(j),j)) then
        !! apply manure
        it = 0
        it = cfrt_id(nro(j),ncf(j),j)
        if (cfrt_kg(nro(j),ncf(j),j) > 0.) then
          l = 1

          sol_no3(l,j) = sol_no3(l,j) + cfrt_kg(nro(j),ncf(j),j) *      &
     &                 (1. - fnh3n(it)) * fminn(it)
          sol_fon(l,j) = sol_fon(l,j) + cfrt_kg(nro(j),ncf(j),j) *      &
     &                 forgn(it)
          sol_nh3(l,j) = sol_nh3(l,j) + cfrt_kg(nro(j),ncf(j),j) *      &
     &                 fnh3n(it) * fminn(it)
          sol_solp(l,j) = sol_solp(l,j) + cfrt_kg(nro(j),ncf(j),j) *    &
     &                 fminp(it)
          sol_fop(l,j) = sol_fop(l,j) + cfrt_kg(nro(j),ncf(j),j) *      &
     &                 forgp(it)

!! add bacteria - (cells/t*t/ha + 10t/m^3*mm*cells/t)/(t/ha + 10t/m^3*mm)
!! calculate ground cover
          gc = 0.
          gc = (1.99532 - Erfc(1.333 * laiday(j) - 2.)) / 2.1
          if (gc < 0.) gc = 0.

          gc1 = 0.
          gc1 = 1. - gc

          frt_t = 0.
          frt_t = bact_swf * cfrt_kg(nro(j),ncf(j),j) / 1000.

          bactp_plt(j) = gc * bactpdb(it) * frt_t * 100. +              &
     &           bactp_plt(j)
          bactlp_plt(j) = gc * bactlpdb(it) * frt_t * 100. +            &  
     &           bactlp_plt(j)

          bactpq(j) = gc1 * bactpdb(it)  * frt_t * 100. + bactpq(j)
          bactpq(j) = bactkddb(it) * bactpq(j)

          bactps(j) = gc1 * bactpdb(it) * frt_t * 100. + bactps(j)
          bactps(j) = (1. - bactkddb(it)) * bactps(j)

          bactlpq(j) = gc1 * bactlpdb(it) * frt_t * 100. + bactlpq(j)
          bactlpq(j) = bactkddb(it) * bactlpq(j)

          bactlps(j) = gc1 * bactlpdb(it) * frt_t * 100. + bactlps(j)
          bactlps(j) = (1. - bactkddb(it)) * bactlps(j)

        endif
 
        !! reset frequency counter
        iday_fert(j) = 1

        !! summary calculations
        cfertn = cfertn + cfrt_kg(nro(j),ncf(j),j) *                    &
     &               (fminn(it) + forgn(it))
        cfertp = cfertp + cfrt_kg(nro(j),ncf(j),j) *                    &
     &               (fminp(it) + forgp(it))
        tcfrtn(j) = tcfrtn(j) + cfertn
        tcfrtp(j) = tcfrtp(j) + cfertp

        if (curyr > nyskip) then
          wshd_ftotn = wshd_ftotn + cfrt_kg(nro(j),ncf(j),j) *          &
     &                 hru_dafr(j) * (fminn(it) + forgn(it))
          wshd_forgn = wshd_forgn + cfrt_kg(nro(j),ncf(j),j) *          &
     &                 hru_dafr(j) * forgn(it)
          wshd_fno3 = wshd_fno3 + cfrt_kg(nro(j),ncf(j),j) *            &
     &                hru_dafr(j) * fminn(it) * (1. - fnh3n(it))
          wshd_fnh3 = wshd_fnh3 + cfrt_kg(nro(j),ncf(j),j) * hru_dafr(j)&
     &               * fminn(it) * fnh3n(it)
          wshd_ftotp = wshd_ftotp + cfrt_kg(nro(j),ncf(j),j) *          &
     &                 hru_dafr(j) * (fminp(it) + forgp(it))
          wshd_fminp = wshd_fminp + cfrt_kg(nro(j),ncf(j),j) *          &
     &                 hru_dafr(j) * fminp(it)
          wshd_forgp = wshd_forgp + cfrt_kg(nro(j),ncf(j),j) *          &
     &                 hru_dafr(j) * forgp(it)
        end if
      else
        iday_fert(j) = iday_fert(j) + 1
      end if

!! check to set if continuous fertilizer period is over
      if (ndcfrt(j) == fert_days(nro(j),ncf(j),j)) then
        icfrt(j) = 0
        ndcfrt(j) = 0
        iday_fert(j) = 0
        ncf(j) = ncf(j) + 1
      end if


      return
      end
