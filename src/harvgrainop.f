      subroutine harvgrainop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest grain only operation 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none           |fertilizer application efficiency calculated
!!                                |as the amount of N applied divided by the
!!                                |amount of N removed at harvest
!!    bio_hv(:,:,:)|kg/ha          |harvested biomass (dry weight)
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha |annual biomass (dry weight) in the HRU
!!    cnop        |none           |SCS runoff curve number for moisture
!!                                |condition II
!!    cnyld(:)    |kg N/kg yield  |fraction of nitrogen in yield
!!    cpyld(:)    |kg P/kg yield  |fraction of phosphorus in yield
!!    curyr       |none           |current year in simulation
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                                |planting
!!    hru_dafr(:) |km2/km2        |fraction of watershed in HRU
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    hvstiadj(:) |(kg/ha)/(kg/ha)|optimal harvest index for current time during
!!                                |growing season
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    idplt(:)    |none           |land cover code from crop.dat
!!    ihru        |none           |HRU number
!!    ncrops(:,:,:)|
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    nro(:)      |none           |sequence number for year in rotation
!!    nyskip      |none           |number of years to not summarize/print output
!!    plantp(:)   |kg P/ha        |amount of phosphorus in plant biomass
!!    plt_et(:)   |mm H2O         |actual ET simulated during life of plant
!!    plt_pet(:)  |mm H2O         |potential ET simulated during life of plant
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    pltfr_n(:)  |none           |fraction of plant biomass that is nitrogen
!!    rwt(:)      |none           |fraction of total plant biomass that is
!!                                |in roots
!!    plantn(:)   |kg N/ha        |amount of nitrogen in plant biomass
!!    sol_fon(:,:)|kg N/ha        |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha        |amount of phosphorus stored in the fresh
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!                                |organic (residue) pool0
!!    sol_rsd(:,:)|kg/ha          |amount of organic matter in the soil
!!                                |classified as residue
!!    wshd_yldn   |kg N/ha        |amount of nitrogen removed from soil in
!!                                |watershed in the yield
!!    wshd_yldp   |kg P/ha        |amount of phosphorus removed from soil in
!!                                |watershed in the yield
!!    wsyf(:)     |(kg/ha)/(kg/ha)|Value of harvest index between 0 and HVSTI
!!                                |which represents the lowest value expected
!!                                |due to water stress
!!    yldanu(:)   |metric tons/ha |annual yield (dry weight) in the HRU
!!    yldkg(:,:,:)|kg/ha          |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_hv(:,:,:)|kg/ha         |harvested biomass (dry weight)
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha|annual biomass (dry weight) in the HRU
!!    hvstiadj(:) |(kg/ha)/(kg/ha)|optimal harvest index for current time during
!!                                |growing season
!!    idorm(:)    |none          |dormancy status code:
!!                               |0 land cover growing (not dormant)
!!                               |1 land cover dormant
!!    igro(:)     |none          |land cover status code:
!!                               |0 no land cover currently growing
!!                               |1 land cover growing
!!    laiday(:)   |m**2/m**2     |leaf area index
!!    ncrops(:,:,:)|
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha       |amount of phosphorus in plant biomass
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha       |pesticide in first layer of soil
!!    sol_rsd(:,:)|kg/ha         |amount of organic matter in the soil
!!                               |classified as residue
!!    strsw(:)    |none          |fraction of potential plant growth achieved
!!                               |on the day where the reduction is caused by
!!                               |water stress
!!    tnyld(:)    |kg N/kg yield |modifier for autofertilization target
!!                               |nitrogen content for plant
!!    wshd_yldn   |kg N/ha       |amount of nitrogen removed from soil in
!!                               |watershed in the yield
!!    wshd_yldp   |kg P/ha       |amount of phosphorus removed from soil in
!!                               |watershed in the yield
!!    yldanu(:)   |metric tons/ha|annual yield (dry weight) in the HRU
!!    yldkg(:,:,:)|kg/ha         |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hiad1       |
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    resnew      |
!!    wur         |
!!    yield       |kg            |yield (dry weight)
!!    yieldn      |
!!    yieldp      |
!!    yldpst      |kg pst/ha     |pesticide removed in yield
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min
!!    SWAT: curno

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
  
      integer :: j, k
!!   change per JGA 8/31/2011 gsm PUT YIELD IN modparm.f
!!      real*8 :: wur, hiad1, yield, yieldn, yieldp, yldpst
  
      real*8 :: wur, hiad1, resnew, yldpst
!      real*8 :: wur, hiad1, resnew, yield, yieldn, yieldp, yldpst

      j = 0
      j = ihru


      hiad1 = 0.
      if (hi_targ(j) > 0.) then
        hiad1 = hi_targ(j)
      else
        if (plt_pet(j) < 10.) then
          wur = 100.
        else
          wur = 0.
          wur = 100. * plt_et(j) / plt_pet(j)
        endif

        hiad1 = (hvstiadj(j) - wsyf(idplt(j))) *                        
     &      (wur / (wur + Exp(6.13 - .0883 * wur))) +                   
     &      wsyf(idplt(j))

        if (hiad1 > hvsti(idplt(j))) then 
          hiad1 = hvsti(idplt(j))
        end if
      end if


!! check if yield is from above or below ground
      yield = 0.
      resnew = 0.
      if (hvsti(idplt(j)) > 1.001) then
        yield = bio_ms(j) * (1. - 1. / (1. + hiad1))
      else
        yield = (1. - rwt(j)) * bio_ms(j) * hiad1
      endif
      if (yield < 0.) yield = 0.
      yield = yield * harveff

      !!add by zhang
      !!====================
      !!use idplt(:,:,:) to calculate the crop type, then
      !! decide which type of crop yield should be used.
      if (cswat == 2) then
        grainc_d(j) = grainc_d(j)+ yield * 0.42
      end if
      !!add by zhang
      !!====================      
      
!! calculate nutrients removed with yield
      yieldn = 0.
      yieldp = 0.
      yieldn = yield * cnyld(idplt(j))
      yieldp = yield * cpyld(idplt(j))
      yieldn = Min(yieldn, 0.85 * plantn(j))
      yieldp = Min(yieldp, 0.85 * plantp(j))
      plantn(j) = plantn(j) - yieldn
      plantn(j) = dmax1(0.0D+00,plantn(j))
      plantp(j) = plantp(j) - yieldp
      plantp(j) = dmax1(0.0D+00,plantp(j))

!! calculate modifier for autofertilization target nitrogen content
      tnyld(j) = 0.
      tnyld(j) = (1. - rwt(j)) * bio_ms(j) * pltfr_n(j) * auto_eff(j)
!     if (icr(j) > 1) then
!       tnyld(nro(j),icr(j)-1,j) = tnyld(nro(j),icr(j),j)
!     else
!       tnyld(nro(j),icr(j)+1,j) = tnyld(nro(j),icr(j),j)
!     end if

!! summary calculations
       xx = bio_ms(j)
       bio_ms(j) = bio_ms(j) - yield
       rwt(j) = rwt(j) * xx / bio_ms(j)
      if (curyr > nyskip) then
       wshd_yldn = wshd_yldn + yieldn * hru_dafr(j)
       wshd_yldp = wshd_yldp + yieldp * hru_dafr(j)
       yldkg(icr(j),j) = yldkg(icr(j),j) + yield
       bio_hv(icr(j),j) = yield + bio_hv(icr(j),j)
       yldanu(j) = yldanu(j) + yield / 1000.
       bio_yrms(j) = bio_yrms(j) + bio_ms(j) / 1000.
!       ncrops(nro(j),icr(j),j) = ncrops(nro(j),icr(j),j) + 1
      endif

!! update curve number
      if (cnop > 0.) 
     *      call curno(cnop,j)

!! increment harvest sequence number
      ncut(j) = ncut(j) + 1

      if (curyr > nyskip) then
        ncrops(icr(j),j) = ncrops(icr(j),j) + 1
      end if 

      return
      end