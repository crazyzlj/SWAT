      subroutine harvestop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the harvest operation (no kill)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none           |fertilizer application efficiency calculated
!!                                |as the amount of N applied divided by the
!!                                |amount of N removed at harvest
!!    bio_hv(:,:,:)|kg/ha          |harvested biomass (dry weight)
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha |annual biomass (dry weight) in the HRU
!!    cnyld(:)    |kg N/kg yield  |fraction of nitrogen in yield
!!    cpyld(:)    |kg P/kg yield  |fraction of phosphorus in yield
!!    curyr       |none           |current year in simulation
!!    harveff       |none         |harvest efficiency: fraction of harvested 
!!                                |yield that is removed from HRU; the 
!!                                |remainder becomes residue on the soil
!!                                |surface
!!    hi_ovr      |(kg/ha)/(kg/ha)|harvest index target specified at
!!                                |harvest
!!    hru_dafr(:) |km2/km2        |fraction of watershed area in HRU
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    hvsti(:)    |(kg/ha)/(kg/ha)|harvest index: crop yield/aboveground
!!                                |biomass
!!    hvstiadj(:) |(kg/ha)/(kg/ha)|optimal harvest index for specific time 
!!                                |during growing season
!!    icr(:)      |none           |sequence number of crop grown within the
!!                                |current year
!!    idc(:)      |none           |crop/landcover category:
!!                                |1 warm season annual legume
!!                                |2 cold season annual legume
!!                                |3 perennial legume
!!                                |4 warm season annual
!!                                |5 cold season annual
!!                                |6 perennial
!!                                |7 trees
!!    idplt(:)    |none           |land cover code from crop.dat
!!    ihru        |none           |HRU number
!!    laiday(:)   |none           |leaf area index
!!    ncut(:)     |none           |sequence number of harvest operation within
!!                                |a year
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    nro(:)      |none           |sequence number of year in rotation
!!    nyskip      |none           |number of years output is not printed/
!!                                |summarized
!!    phuacc(:)   |none           |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha        |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha        |amount of phosphorus in plant biomass
!!    plt_et(:)   |mm H2O         |actual ET simulated during life of plant
!!    plt_pet(:)  |mm H2O         |potential ET simulated during life of plant
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    pltfr_n(:)  |none           |fraction of plant biomass that is nitrogen
!!    pltfr_p(:)  |none           |fraction of plant biomass that is phosphorus
!!    rwt(:)      |none           |fraction of total plant biomass that is
!!                                |in roots
!!    sol_fon(:,:)|kg N/ha        |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha        |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
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
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_hv(:,:,:)|kg/ha          |harvested biomass (dry weight)
!!    bio_ms(:)   |kg/ha          |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha |annual biomass (dry weight) in the HRU
!!    laiday(:)   |none           |leaf area index
!!    phuacc(:)   |none           |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha        |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha        |amount of phosphorus in plant biomass
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    rsr1c(:)    |               |initial root to shoot ratio at beg of growing season
!!    rsr2c(:)    |               |root to shoot ratio at end of growing season
!!    sol_fon(:,:)|kg N/ha        |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha        |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    sol_rsd(:,:)|kg/ha          |amount of organic matter in the soil
!!                                |classified as residue
!!    tnyld(:)    |kg N/kg yield  |modifier for autofertilization target
!!                                |nitrogen content for plant
!!    wshd_yldn   |kg N/ha        |amount of nitrogen removed from soil in
!!                                |watershed in the yield
!!    wshd_yldp   |kg P/ha        |amount of phosphorus removed from soil in
!!                                |watershed in the yield
!!    yldanu(:)   |metric tons/ha |annual yield (dry weight) in the HRU
!!    yldkg(:,:,:)|kg/ha          |yield (dry weight) by crop type in the HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    clip        |kg/ha          |yield lost during harvesting
!!    clipn       |kg N/ha        |nitrogen in clippings
!!    clipp       |kg P/ha        |phosphorus in clippings
!!    clippst     |kg pst/ha      |pesticide in clippings
!!    hiad1       |none           |actual harvest index (adj for water/growth)
!!    j           |none           |HRU number
!!    k           |none           |counter
!!    wur         |none           |water deficiency factor
!!    yield       |kg             |yield (dry weight)
!!    yieldn      |kg N/ha        |nitrogen removed in yield
!!    yieldp      |kg P/ha        |phosphorus removed in yield
!!    yldpst      |kg pst/ha      |pesticide removed in yield
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm
  
      integer :: j, k
      
!!   change per JGA 8/31/2011 gsm PUT YIELD IN modparm.f
!!    real :: hiad1, wur, yield, clip, yieldn, yieldp, clipn, clipp
      real :: hiad1, wur, clip, yieldn, yieldp, clipn, clipp
      real :: yldpst, clippst, rtresnew

      j = 0
      j = ihru

      ssb = 0.
      ssabg = 0.
      ssr = 0.
      ssn = 0.
      ssp = 0.
      ssb = bio_ms(j)                            ! Armen 16 Jan 2009 storing info
      ssabg = bio_ms(j) * (1.- rwt(j))	    ! Armen 16 Jan 2009 storing info
      ssr = ssb * rwt(j)                         ! Armen 16 Jan 2009 storing info
      ssn = plantn(j)                            ! Armen 20 May 2006 storing info
      ssp = plantp(j)                            ! Armen 20 May 2006 storing info
	
	
!! calculate modifier for autofertilization target nitrogen content
      tnyld(j) = 0.
      tnyld(j) = (1. - rwt(j)) * bio_ms(j) * pltfr_n(j) * auto_eff(j)
!     if (icr(j) > 1) then
!       tnyld(nro(j),icr(j)-1,j) = tnyld(nro(j),icr(j),j)
!     else
!       tnyld(nro(j),icr(j)+1,j) = tnyld(nro(j),icr(j),j)
!     end if

      hiad1 = 0.
      if (hi_ovr > 0.) then
        hiad1 = hi_ovr
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
      if (hvsti(idplt(j)) > 1.001) then
        yield = bio_ms(j) * (1. - 1. / (1. + hiad1))
      else
        yield = (1.-rwt(j)) * bio_ms(j) * hiad1
      endif
      if (yield < 0.) yield = 0.

!! determine clippings (biomass left behind) and update yield
      clip = 0.
      clip = yield * (1. - harveff)
      yield = yield * harveff
      if (yield < 0.) yield = 0.
      if (clip < 0.) clip = 0.

      if (hi_ovr                   > 0.) then
        !! calculate nutrients removed with yield
        yieldn = 0.
        yieldp = 0.
        yieldn = yield * pltfr_n(j)
        yieldp = yield * pltfr_p(j)
        yieldn = Min(yieldn, 0.80 * plantn(j)) ! note Armen changed .80 for 0.9
        yieldp = Min(yieldp, 0.80 * plantp(j)) ! note Armen changed .80 for 0.9
        !! calculate nutrients removed with clippings
        clipn = 0.
        clipp = 0.
        clipn = clip * pltfr_n(j)
        clipp = clip * pltfr_p(j)
        clipn = Min(clipn,plantn(j)-yieldn)
        clipp = Min(clipp,plantp(j)-yieldp)
      else
        !! calculate nutrients removed with yield
        yieldn = 0.
        yieldp = 0.
        yieldn = yield * cnyld(idplt(j))
        yieldp = yield * cpyld(idplt(j))
        yieldn = Min(yieldn, 0.80 * plantn(j)) ! note Armen changed .80 for 0.9
        yieldp = Min(yieldp, 0.80 * plantp(j)) ! note Armen changed .80 for 0.9
        !! calculate nutrients removed with clippings
        clipn = 0.
        clipp = 0.
        clipn = clip * cnyld(idplt(j))
        clipp = clip * cpyld(idplt(j))
        clipn = Min(clipn,plantn(j)-yieldn)
        clipp = Min(clipp,plantp(j)-yieldp)
      endif

      yieldn = Max(yieldn,0.)
      yieldp = Max(yieldp,0.)
      clipn = Max(clipn,0.)
      clipp = Max(clipp,0.)

      !! add clippings to residue and organic n and p
      sol_rsd(1,j) = sol_rsd(1,j) + clip
      sol_fon(1,j) = clipn + sol_fon(1,j)
      sol_fop(1,j) = clipp + sol_fop(1,j)

	!! Calculation for dead roots allocations, resetting phenology, updating other pools

      ff3 = 0.
      if (ssabg > 1.e-6) then
        ff3 = (yield + clip) / ssabg	! Armen 20 May 2008 and 16 Jan 2009
      else
        ff3 = 1.
      endif 
      if (ff3 > 1.0) ff3 = 1.0


	! nssr is the new mass of roots
      nssr = rwt(j) * ssabg * (1. - ff3) / (1. - rwt(j))  
      rtresnew = ssr - nssr
      if (ssr > 1.e-6) then
       ff4 = rtresnew / ssr
      else
	   ff4 = 0.
      end if
      rtresn = ff4 * ssn
      rtresp = ff4 * ssp


      !! reset leaf area index and fraction of growing season
      if (ssb > 0.001) then
        laiday(j) = laiday(j) * (1. - ff3)
        if (laiday(j) < alai_min(idplt(j))) then   !Sue
          laiday(j) = alai_min(idplt(j))
        end if
        if (phuacc(j) < .999) phuacc(j) = phuacc(j) * (1. - ff3)  
        rwt(j) = .4 - .2 * phuacc(j)        
      else
        bio_ms(j) = 0.
        laiday(j) = 0.
        phuacc(j) = 0.
      endif

	!! remove n and p in harvested yield, clipped biomass, and dead roots 
      bio_ms(j) = bio_ms(j) - yield - clip - rtresnew ! Armen 20 May 2008
      plantn(j) = plantn(j) - yieldn - clipn - rtresn
      plantp(j) = plantp(j) - yieldp - clipp - rtresp 
      if (bio_ms(j) < 0.) bio_ms(j) = 0.
      if (plantn(j) < 0.) plantn(j) = 0.
      if (plantp(j) < 0.) plantp(j) = 0.

      !! compute fraction of roots in each layer	! Armen 20 May 2008
      call rootfr

      !! allocate roots, N, and P to soil pools	! Armen 20 May 2008
      do l=1, sol_nly(j)
            sol_rsd(l,j) = sol_rsd(l,j) + rtfr(l) * rtresnew
            sol_fon(l,j) = sol_fon(l,j) + rtfr(l) * rtresn
            sol_fop(l,j) = sol_fop(l,j) + rtfr(l) * rtresp
      end do

      rtfr = 0.

	!! adjust foliar pesticide for plant removal
      if (hrupest(j) == 1) then
        do k = 1, npmx
          !! calculate amount of pesticide removed with yield and clippings
          yldpst = 0.
          clippst = 0.
          if (hvsti(idplt(j)) > 1.001) then
            yldpst = plt_pst(k,j)
            plt_pst(k,j) = 0.
          else
            yldpst = hiad1 * plt_pst(k,j)
            plt_pst(k,j) = plt_pst(k,j) - yldpst
            if (plt_pst(k,j) < 0.) plt_pst(k,j) = 0.
          endif
          clippst = yldpst * (1. - harveff)
          if (clippst < 0.) clippst = 0.
          !! add pesticide in clippings to soil surface
          sol_pst(k,j,1) = sol_pst(k,j,1) + clippst
        end do   
      end if

	!! summary calculations
      if (curyr > nyskip) then
        wshd_yldn = wshd_yldn + yieldn * hru_dafr(j)
        wshd_yldp = wshd_yldp + yieldp * hru_dafr(j)
        yldkg(icr(j),j) = yldkg(icr(j),j) + yield
        yldanu(j) = yldanu(j) + yield  / 1000.    

       ! select case (idc(idplt(j)))
       !   case (3, 6, 7)
       !     bio_hv(nro(j),icr(j),j) = (yield + clip) + bio_hv(nro(j),icr(j),j)
       !     bio_yrms(j) = bio_yrms(j) + (yield + clip) / 1000.
       !   case default
        bio_hv(icr(j),j) = (yield + clip + rtresnew) + 
     &                                           bio_hv(icr(j),j)                       !! Jeff, is this the intention
        bio_yrms(j) = bio_yrms(j) + (yield + clip + rtresnew) / 1000.		            !! Jeff, is this the intention
       ! end select
      endif


       ncut(j) = ncut(j) + 1

      return
      end subroutine
