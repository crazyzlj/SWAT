      subroutine harvkillop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest and kill operation

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
!!      real :: wur, hiad1, yield, yieldn, yieldp, yldpst
      real :: wur, hiad1, yieldn, yieldp, yldpst
	real :: resnew, rtresnew 
	

      j = 0
      j = ihru

	!! calculate modifier for autofertilization target nitrogen content
      tnyld(j) = 0.
      tnyld(j) = (1. - rwt(j)) * bio_ms(j) * pltfr_n(j) * auto_eff(j)
!     if (icr(j) > 1) then
!       tnyld(nro(j),icr(j)-1,j) = tnyld(nro(j),icr(j),j)
!     else
!       tnyld(nro(j),icr(j)+1,j) = tnyld(nro(j),icr(j),j)
!     end if


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

        hiad1 = (hvstiadj(j) - wsyf(idplt(j))) *                        &
     &      (wur / (wur + Exp(6.13 - .0883 * wur))) +                   &
     &      wsyf(idplt(j))

        if (hiad1 > hvsti(idplt(j))) then 
          hiad1 = hvsti(idplt(j))
        end if
      end if


!! check if yield is from above or below ground
      yield = 0.
      resnew = 0.
      rtresnew = 0.

!! stover fraction during harvkillop
      xx = frac_harvk
      if (xx .lt. 1.e-6) then
        xx = hi_ovr                      
      endif
!! stover fraction during harvkillop

      if (hvsti(idplt(j)) > 1.001) then
        yield = bio_ms(j) * (1. - 1. / (1. + hiad1))
        resnew = bio_ms(j) / (1. + hiad1)
        resnew = resnew * (1. - xx)
      else
        yield = (1. - rwt(j)) * bio_ms(j) * hiad1
        resnew = (1. - rwt(j)) * (1. - hiad1) * bio_ms(j)
        !! remove stover during harvkillop
        resnew = resnew * (1. - xx)
        rtresnew = rwt(j) * bio_ms(j)	
      endif
      if (yield < 0.) yield = 0.
      if (resnew < 0.) resnew = 0.
	if (rtresnew < 0.) rtresnew = 0.	! Armen 19 May 2008
										! I would avoid this check, it is
										! safer to know if variable is negative

	!! calculate nutrients removed with yield
      yieldn = 0.
      yieldp = 0.
      yieldn = yield * cnyld(idplt(j))
      yieldp = yield * cpyld(idplt(j))
      yieldn = Min(yieldn, 0.80 * plantn(j))
      yieldp = Min(yieldp, 0.80 * plantp(j))

	!! Armen 19 May 2008 / 21 January 2008	
	!! fraction of roots in each layer
	call rootfr

	!! fraction of N, P in residue (ff1) or roots (ff2)
	ff1 = (1 - hiad1) / (1 - hiad1 + rwt(j))
	ff2 = 1 - ff1

	!! update residue, N, P on soil surface
      sol_rsd(1,j) = resnew + sol_rsd(1,j)
	sol_fon(1,j) = sol_fon(1,j) + FF1 * (plantn(j) - yieldn)
      sol_fop(1,j) = sol_fop(1,j) + FF1 * (plantp(j) - yieldp) 
      sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
	sol_fon(1,j) = Max(sol_fon(1,j),0.)
	sol_fop(1,j) = Max(sol_fop(1,j),0.)

	!! allocate dead roots, N, P to soil layers
	do l=1, sol_nly(j)
	 sol_rsd(l,j) = sol_rsd(l,j) + rtfr(l) *rtresnew
       sol_fon(l,j) = sol_fon(l,j) + rtfr(l) *ff2 * (plantn(j) - yieldn)
       sol_fop(l,j) = sol_fop(l,j) + rtfr(l) *ff2 * (plantp(j) - yieldp)
	end do
   
	!! adjust foliar pesticide for plant removal
      if (hrupest(j) == 1) then
        do k = 1, npmx
          !! calculate amount of pesticide removed with yield
          yldpst = 0.
          if (hvsti(idplt(j)) > 1.001) then
            yldpst = plt_pst(k,j)
            plt_pst(k,j) = 0.
          else
            yldpst = hiad1 * plt_pst(k,j)
            plt_pst(k,j) = plt_pst(k,j) - yldpst
            if (plt_pst(k,j) < 0.) plt_pst(k,j) = 0.
          endif
          !! add pesticide in residue to soil surface
          sol_pst(k,j,1) = sol_pst(k,j,1) + plt_pst(k,j)
          plt_pst(k,j) = 0.
        end do
      end if

	!! summary calculations
      if (curyr > nyskip) then
       wshd_yldn = wshd_yldn + yieldn * hru_dafr(j)
       wshd_yldp = wshd_yldp + yieldp * hru_dafr(j)
       yldkg(icr(j),j) = yldkg(icr(j),j) + yield
       bio_hv(icr(j),j) = bio_ms(j) + bio_hv(icr(j),j)
       yldanu(j) = yldanu(j) + yield / 1000.
       bio_yrms(j) = bio_yrms(j) + bio_ms(j) / 1000.
       ncrops(icr(j),j) = ncrops(icr(j),j) + 1
      endif

	!! update curve number
      if (cnop > 0.) 
     *      call curno(cnop,j)

	!! reset variables
      igro(j) = 0
      idorm(j) = 0
      bio_ms(j) = 0.
	rwt(j) = 0.
      plantn(j) = 0.
      plantp(j) = 0.
      strsw(j) = 1.
      laiday(j) = 0.
      hvstiadj(j) = 0.
	rtfr = 0. ! resetting root fraction per layer array to 0

      return
      end
