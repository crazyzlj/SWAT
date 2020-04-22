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
!!    real*8 :: hiad1, wur, yield, clip, yieldn, yieldp, clipn, clipp
      real*8 :: hiad1, wur, clip, clipn, clipp
      real*8 :: yldpst, clippst, rtresnew
      real*8 :: clipgrn,cliptbr,clipngrn,clippgrn,yieldngrn
      real*8 :: yieldntbr,yieldnrsd,yieldpgrn,yieldptbr,yieldprsd
      real*8 :: clipntbr,clipptbr,rtresp

      !!add by zhang
      !!===================
      real*8 :: BLG1, BLG2, BLG3,  CLG, sf
      real*8 :: sol_min_n, resnew, resnew_n, resnew_ne
      real*8 :: LMF, LSF, LSLF, LSNF,LMNF 
      real*8 ::  RLN, RLR
      orgc_f = 0.
      BLG1 = 0.
      BLG2 = 0.
      BLG3 = 0.
      CLG = 0.
      sf = 0.
      sol_min_n = 0.
      resnew = 0.
      resnew_n = 0.
      resnew_ne = 0.
      LMF = 0.
      LSF = 0.
      LSLF = 0.
      LSNF = 0.
      LMNF = 0.
      clipbms = 0.
      clipnbms = 0.
      clippbms = 0.
      yieldnbms = 0.
      yieldpbms = 0.
      
      RLN = 0.
      RLR = 0.
      clipgrn = 0.; cliptbr = 0.; clipngrn = 0.; clippgrn = 0.
      yieldngrn = 0.; yieldntbr = 0.; yieldnrsd = 0.; yieldpgrn = 0.
      yieldptbr = 0.; yieldprsd = 0.; clipntbr = 0.; clipptbr = 0.
      rtresnew = 0.; rtresn = 0.; rtresp = 0.
      
      !!add by zhang
      !!===================

      j = ihru

      yieldgrn = 0.
      yieldbms = 0.
      yieldtbr = 0.
      yieldrsd = 0.
      
      ssb = bio_ms(j)                            ! Armen 16 Jan 2009 storing info
      ssabg = bio_ms(j) * (1.- rwt(j))	    ! Armen 16 Jan 2009 storing info
      ssr = ssb * rwt(j)                         ! Armen 16 Jan 2009 storing info
      ssn = plantn(j)                            ! Armen 20 May 2006 storing info
      ssp = plantp(j)                            ! Armen 20 May 2006 storing info
	
      !! calculate modifier for autofertilization target nitrogen content
      tnyld(j) = (1. - rwt(j)) * bio_ms(j) * pltfr_n(j) * auto_eff(j)

      !! compute grain yield
      hiad1 = 0.
      if (plt_pet(j) < 10.) then
        wur = 100.
      else
        wur = 100. * plt_et(j) / plt_pet(j)
      endif
      hiad1 = (hvstiadj(j) - wsyf(idplt(j))) *
     &    (wur / (wur + Exp(6.13 - .0883 * wur))) + wsyf(idplt(j))
      if (hiad1 > hvsti(idplt(j))) then
        hiad1 = hvsti(idplt(j))
      end if
	

      !! check if yield is from above or below ground
      if (hvsti(idplt(j)) > 1.001) then
        !! compute tuber yields
 !!       yieldtbr = bio_ms(j) * (1. - 1. / (1. + hiad1))
        !! determine clippings (biomass left behind) and update yield
        yieldtbr = bio_ms(j) * (1. - 1. / (1. + hiad1)) * harveff  !! corrected by cibin Nov/2013
        cliptbr = bio_ms(j) * (1. - 1. / (1. + hiad1)) * (1. - harveff) !! corrected by cibin Nov/2013
        bio_ms(j) = bio_ms(j) - yieldtbr - cliptbr
        !! calculate nutrients removed with yield
        yieldntbr = yieldtbr * cnyld(idplt(j))
        yieldptbr = yieldtbr * cpyld(idplt(j))
        yieldntbr = Min(yieldntbr, 0.80 * plantn(j))
        yieldptbr = Min(yieldptbr, 0.80 * plantp(j))
        clipntbr = cliptbr * pltfr_n(j)
        clipptbr = cliptbr * pltfr_p(j)
        clipntbr = Min(clipntbr, plantn(j) - yieldntbr)
        clipptbr = Min(clipptbr, plantp(j) - yieldptbr)
        plantn(j) = plantn(j) - yieldntbr - clipntbr
        plantp(j) = plantp(j) - yieldptbr - clipptbr
      endif

	if (hi_bms > 0.) then       !! compute biomass yield !! corrected by cibin Nov/2013
        yieldbms = hi_bms * (1.-rwt(j)) * bio_ms(j)*harveff
        clipbms = hi_bms * (1.-rwt(j)) * bio_ms(j) * (1. - harveff)  
        bio_ms(j) = bio_ms(j) - yieldbms - clipbms !corrected by Jaehak Jeong sep. 2013
        !! calculate nutrients removed with yield
        yieldnbms = yieldbms * cnyld(idplt(j))   !! corrected by cibin Nov/2013
        yieldpbms = yieldbms * cpyld(idplt(j))
        yieldnbms = Min(yieldnbms, 0.80 * plantn(j))
        yieldpbms = Min(yieldpbms, 0.80 * plantp(j))
        !! calculate nutrients removed with clippings
        clipnbms = clipbms * cnyld(idplt(j))   !! corrected by cibin Nov/2013
        clippbms = clipbms * cpyld(idplt(j))
        clipnbms = Min(clipnbms, plantn(j) - yieldnbms)
        clippbms = Min(clippbms, plantp(j) - yieldpbms)
        plantn(j) = plantn(j) - yieldnbms - clipnbms
        plantp(j) = plantp(j) - yieldpbms - clippbms
	else
        !! compute grain yields
        yieldgrn = (1.-rwt(j)) * bio_ms(j) * hiad1* harveff
        !! determine clippings (biomass left behind) and update yield

        clipgrn = (1.-rwt(j)) * bio_ms(j) * hiad1 * (1. - harveff)
        bio_ms(j) = bio_ms(j) - yieldgrn - clipgrn
        !! calculate nutrients removed with yield
        yieldngrn = yieldgrn * cnyld(idplt(j))
        yieldpgrn = yieldgrn * cpyld(idplt(j))
        yieldngrn = Min(yieldngrn, 0.80 * plantn(j))
        yieldpgrn = Min(yieldpgrn, 0.80 * plantp(j))
        !! calculate nutrients removed with clippings
        clipngrn = clipgrn * cnyld(idplt(j))
        clippgrn = clipgrn * cpyld(idplt(j))
        clipngrn = Min(clipngrn, plantn(j) - yieldngrn)
        clippgrn = Min(clippgrn, plantp(j) - yieldpgrn)
        plantn(j) = plantn(j) - yieldngrn - clipngrn
        plantp(j) = plantp(j) - yieldpgrn - clippgrn
      endif

      
     

      !! add clippings to residue and organic n and p
      sol_rsd(1,j) = sol_rsd(1,j) + clipgrn + clipbms + cliptbr
      sol_fon(1,j) = sol_fon(1,j) + clipngrn + clipnbms + cliptbr
      sol_fop(1,j) = sol_fop(1,j) + clippgrn + clippbms + cliptbr
      
      !! compute residue yield
      if (hi_rsd > 0.) then
        yieldrsd = hi_rsd * sol_rsd(1,j)
        yieldnrsd = hi_rsd * sol_fon(1,j)
        yieldprsd = hi_rsd * sol_fop(1,j)
        sol_rsd(1,j) = sol_rsd(1,j) - yieldrsd
        sol_fon(1,j) = sol_fon(1,j) - yieldnrsd
        sol_fop(1,j) = sol_fop(1,j) - yieldprsd
      end if

      yield = yieldgrn + yieldbms + yieldtbr + yieldrsd
      yieldn = yieldngrn + yieldnbms + yieldntbr + yieldnrsd
      yieldp = yieldpgrn + yieldpbms + yieldptbr + yieldprsd
	clip= clipgrn + clipbms + cliptbr   !! cibin nov 2013
      clipn = clipngrn + clipnbms + clipntbr !! cibin nov 2013
      clipp = clippgrn + clippbms + clipptbr !! cibin nov 2013
      
      !!add by zhang
      !!=====================
      !!use idplt(:,:,:) to calculate the crop type, then
      !! decide which type of crop yield should be used.
      if (cswat == 2) then
        grainc_d(j) = grainc_d(j)+ yield * 0.42
        rsdc_d(j) = rsdc_d(j)+(clip+yield) * 0.42
      end if
      !!add by zhang
      !!=====================
      
            
      !!insert new biomss by zhang
      !!===============================
      if (cswat == 2) then
        BLG1 = 0.01/0.10 !BLG1/BLG2
        BLG2 = 0.99
        BLG3 = 0.10 !BLG2
        !CALL ASCRV(BLG(1,I),BLG(2,I),.5,1.)
        XX = log(0.5/BLG1-0.5)
        BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
        BLG1 = XX + 0.5*BLG2
        CLG=BLG3*phuacc(j)/(phuacc(j)+ EXP(BLG1-BLG2*phuacc(j)))
        sf = 0.05
        !kg/ha   
        sol_min_n = 0.	
        sol_min_n = (sol_no3(1,j)+sol_nh3(1,j))
        resnew = clip 
        resnew_n = clipn   	    
        resnew_ne = resnew_n + sf * sol_min_n
        !Not sure 1000 should be here or not!
        !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
        RLN = (resnew * CLG/(resnew_n+1.E-5))
        !RLR is the fraction of lignin in the added residue
        RLR = MIN(.8, resnew * CLG/1000/(resnew/1000+1.E-5))
        !In most cases, lignin content in residue should be less than 30%
        !Therefore, RLR is expected to be less than 0.3
        !In the future, we may want to add a check make sure LMF is less than 1.0 - RLR.
        !this would help to avoid sol_LS becoming less than sol_LSL 
        	    
        LMF = 0.85 - 0.018 * RLN
        if (LMF <0.01) then
          LMF = 0.01
        else
          if (LMF >0.7) then
            LMF = 0.7
          end if
        end if
        
        LSF =  1 - LMF  
        sol_LM(1,j) = sol_LM(1,j) + LMF * resnew
        sol_LS(1,j) = sol_LS(1,j) + LSF * resnew
        	  
        !In Jimmy's code, lignin added to sol_LSL is calculated as RLR*LSF*resnew
        !However, I think we should use RLR*resnew; Confirmed with Jimmy
        !sol_LSL(1,j) = sol_LSL(1,j) + RLR* LSF * resnew	
        sol_LSL(1,j) = sol_LSL(1,j) + RLR*resnew
        sol_LSC(1,j) = sol_LSC(1,j) + 0.42*LSF * resnew  
        !In allignment with the sol_LSL calculation, sol_LSLC is also changed
        !sol_LSLC(1,j) = sol_LSLC(1,j) + RLR*0.42*LSF * resnew
        sol_LSLC(1,j) = sol_LSLC(1,j) + RLR*0.42+resnew
        sol_LSLNC(1,j) = sol_LSC(1,j) - sol_LSLC(1,j)              
                
        !X3 = MIN(X6,0.42*LSF * resnew/150)                 
        if (resnew_ne >= (0.42 * LSF * resnew /150)) then
          sol_LSN(1,j) = sol_LSN(1,j) + 0.42 * LSF * resnew / 150
          sol_LMN(1,j) = sol_LMN(1,j) + resnew_ne - 
     &                         (0.42 * LSF * resnew / 150) + 1.E-25
        else
          sol_LSN(1,j) = sol_LSN(1,j) + resnew_ne
          sol_LMN(1,j) = sol_LMN(1,j) + 1.E-25
        end if
        sol_LMC(1,j) = sol_LMC(1,j) + 0.42 * LMF * resnew	
        !update no3 and nh3 in soil
        sol_no3(1,j) = sol_no3(1,j) * (1-sf)
        sol_nh3(1,j) = sol_nh3(1,j) * (1-sf)
      end if
      !!insert new biomss by zhang
      !!=============================

	!! Calculation for dead roots allocations, resetting phenology, updating other pools
      if (ssabg > 1.e-6) then
        ff3 = (yield + clip) / ssabg	! Armen 20 May 2008 and 16 Jan 2009
      else
        ff3 = 1.
      endif 
      if (ff3 > 1.0) ff3 = 1.0

	! nssr is the new mass of roots
!      nssr = rwt(j) * ssabg * (1. - ff3) / (1. - rwt(j))  
!      rtresnew = ssr - nssr
!      if (ssr > 1.e-6) then
!       ff4 = rtresnew / ssr
!      else!
!	   ff4 = 0.
!      end if
!      rtresn = ff4 * ssn
!      rtresp = ff4 * ssp


      !! reset leaf area index and fraction of growing season
      if (ssb > 0.001) then
        laiday(j) = laiday(j) * (1. - ff3)
        if (laiday(j) < alai_min(idplt(j))) then   !Sue
          laiday(j) = alai_min(idplt(j))
        end if
        phuacc(j) = phuacc(j) * (1. - ff3)  
        rwt(j) = .4 - .2 * phuacc(j)        
      else
        bio_ms(j) = 0.
        laiday(j) = 0.
        phuacc(j) = 0.
      endif


!      !! compute fraction of roots in each layer	! Armen 20 May 2008
!      call rootfr
!
!      !! allocate roots, N, and P to soil pools	! Armen 20 May 2008
!      do l=1, sol_nly(j)
!            sol_rsd(l,j) = sol_rsd(l,j) + rtfr(l) * rtresnew
!            sol_fon(l,j) = sol_fon(l,j) + rtfr(l) * rtresn
!            sol_fop(l,j) = sol_fop(l,j) + rtfr(l) * rtresp
!      end do
!
!     rtfr = 0.
      !! allocate roots, N, and P to soil pools	! Armen 20 May 2008
      do l=1, sol_nly(j)
        sol_rsd(l,j) = sol_rsd(l,j) + rtfr(l) * rtresnew
        sol_fon(l,j) = sol_fon(l,j) + rtfr(l) * rtresn
        sol_fop(l,j) = sol_fop(l,j) + rtfr(l) * rtresp

        !!insert new biomss by zhang
        !!=============================
        if (cswat == 2) then
          rsdc_d(j) = rsdc_d(j)+rtfr(l) * rtresnew * 0.42
          BLG3 = 0.10
          BLG1 = 0.01/0.10
          BLG2 = 0.99
                
          XX = log(0.5/BLG1-0.5)
          BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
          BLG1 = XX + 0.5*BLG2
          CLG=BLG3*phuacc(j)/(phuacc(j)+ EXP(BLG1-BLG2*phuacc(j)))
          !kg/ha  
          sol_min_n = 0.	
          sol_min_n = (sol_no3(l,j)+sol_nh3(l,j))
	          	          
          resnew = rtfr(l) * rtresnew
          !resnew_n = resnew * pltfr_n(j)     	    
          !resnew_ne = resnew_n + sf * sol_min_n
          resnew_n = rtfr(l) * rtresn
          resnew_ne = resnew_n + sf * sol_min_n
          !Not sure 1000 should be here or not!
          !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
          RLN = (resnew * CLG/(resnew_n+1.E-5))
          RLR = MIN(.8, resnew * CLG/1000/(resnew/1000+1.E-5))
        	    
          LMF = 0.85 - 0.018 * RLN
          if (LMF <0.01) then
          LMF = 0.01
          else
            if (LMF >0.7) then
              LMF = 0.7
            end if
          end if      	  
          
          LSF =  1 - LMF  
          sol_LM(l,j) = sol_LM(l,j) + LMF * resnew
          sol_LS(l,j) = sol_LS(l,j) + LSF * resnew        	  

          !here a simplified assumption of 0.5 LSL
          LSLF = CLG          
          sol_LSL(l,j) = sol_LSL(l,j) + RLR* LSF * resnew	          
          sol_LSC(l,j) = sol_LSC(l,j) + 0.42*LSF * resnew  
          sol_LSLC(l,j) = sol_LSLC(l,j) + RLR*0.42*LSF * resnew
          sol_LSLNC(l,j) = sol_LSC(l,j) - sol_LSLC(1,j)              
               
          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
            sol_LSN(l,j) = sol_LSN(l,j) + 0.42 * LSF * resnew / 150
            sol_LMN(l,j) = sol_LMN(l,j) + resnew_ne - 
     &                         (0.42 * LSF * resnew / 150) + 1.E-25
          else
            sol_LSN(l,j) = sol_LSN(l,j) + resnew_ne
            sol_LMN(l,j) = sol_LMN(l,j) + 1.E-25
          end if	
            sol_LMC(l,j) = sol_LMC(l,j) + 0.42 * LMF * resnew	
            !update no3 and nh3 in soil
            sol_no3(1,j) = sol_no3(1,j) * (1-sf)
            sol_nh3(1,j) = sol_nh3(1,j) * (1-sf)  
          end if        
          !!insert new biomss by zhang
          !!=============================
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
        ncrops(icr(j),j) = ncrops(icr(j),j) + 1           !!!!!!!!!!!!!!!!nubz 

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