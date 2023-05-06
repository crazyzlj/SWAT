       subroutine route
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates channel routing     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alpha_bnke(:)|none         |Exp(-alpha_bnk(:))
!!    bankst(:)   |m^3 H2O       |bank storage
!!    ch_eqn      |              |sediment routing methods
!!                               | 0 = original SWAT method
!!                               | 1 = Bagnold's
!!                               | 2 = Kodatie
!!                               | 3 = Molinas Wu
!!                               | 4 = Yang
!!    ch_l2(:)    |km            |length of main channel
!!    ch_revap(:) |none          |revap coeff: this variable controls the amount
!!                               |of water moving from bank storage to the root
!!                               |zone as a result of soil moisture depletion
!!    ch_w(2,:)   |m             |average width of main channel
!!    da_ha       |ha            |area of watershed in hectares
!!    hru_sub(:)  |none          |subbasin number for HRU
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    inum1       |none          |reach number
!!    inum2       |none          |inflow hydrograph storage location number
!!    irte        |none          |water routing method:
!!                               |0 variable storage method
!!                               |1 Muskingum method
!!    iwq         |none          |stream water quality code
!!                               |0 do not model stream water quality
!!                               |1 model stream water quality (QUAL2E)
!!    nhru        |none          |number of HRUs in watershed
!!    pet_day     |mm H2O        |potential evapotranspiration on day
!!    rchdep      |m             |depth of flow on day
!!    rnum1       |none          |fraction of overland flow 
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    sub_fr(:)   |none          |fraction of watershed area in subbasin
!!    varoute(3,:)|metric tons   |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    revapday    |m^3 H2O       |amount of water moving from bank storage
!!                               |into the soil profile or being taken
!!                               |up by plant roots in the bank storage zone
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    jrch        |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: rchinit, rtover, rtday, rtmusk, rthourly, rtsed, rthsed, watqual
!!    SWAT: noqual, hhwatqual, hhnoqual, rtpest, rthpest, rtbact, irr_rch
!!    SWAT: rchuse, reachout

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch, ii
      real*8 :: subwtr

      jrch = 0
      jrch = inum1
      !inum3 is the subbasin for stream-aquifer interaction
      !inum5 is the landscape within the subbasin
      isub = inum3
      iru = inum5
      !ru_ovs(isub,iru)

!! initialize variables for route command loop
      call rchinit

!! route overland flow
!!      iru_sub = inum4   !!routing unit number
!!      call routels(iru_sub)

      vel_chan(jrch) = 0.
      dep_chan(jrch) = 0.

!! route water through reach
      if (ievent == 0) then
        if (irte == 0) call rtday
        if (irte == 1) call rtmusk
      else
        if (irte == 0) call rthvsc
        if (irte == 1) call rthmusk
      endif

!! average daily water depth for sandi doty 09/26/07
      dep_chan(jrch) = rchdep

!! if reach is an irrigation canal, restrict outflow
      if (icanal(jrch) == 1) then
        rchstor(jrch) = rchstor(jrch) + rtwtr
        rtwtr = 0.
      end if

!! add transmission losses to bank storage/deep aquifer in subbasin

      if (rttlc > 0.) then
        bankst(jrch) = bankst(jrch) + rttlc * (1. - trnsrch)
        if (da_ha > 1.e-9) then 
          subwtr = rttlc * trnsrch / (da_ha * sub_fr(jrch) * 10.)
          do j = hru1(jrch), hru1(jrch) + hrutot(jrch) - 1
            deepst(j) = deepst(j) + subwtr
          end do
	  end if
      end if
 
!! compute revap from bank storage
      revapday = ch_revap(jrch) * pet_day * ch_l2(jrch) * ch_w(2,jrch)
      revapday = Min(revapday,bankst(jrch))
      bankst(jrch) = bankst(jrch) - revapday

!! compute contribution of water in bank storage to streamflow
      qdbank = bankst(jrch) * (1. - alpha_bnke(jrch))
      bankst(jrch) = bankst(jrch) - qdbank
      rtwtr = rtwtr + qdbank
      if (ievent > 0) then
        do ii = 1, nstep
          hrtwtr(ii) = hrtwtr(ii) + qdbank / dfloat(nstep)
          hsdti(ii) = max(0.,hrtwtr(ii) / (dthy * 3600.)) !m3 -> m3/s
        end do
      end if


!! perform in-stream sediment calculations
	  sedrch = 0.
	  rch_san = 0.
	  rch_sil = 0.
	  rch_cla = 0.
	  rch_sag = 0.
	  rch_lag = 0.
	  rch_gra = 0.
	  ch_orgn(jrch) = 0.
	  ch_orgp(jrch) = 0.
!!    Bank erosion
        rchdy(55,jrch) = 0.
!!    Channel Degredation
        rchdy(56,jrch) = 0.
!!    Channel Deposition
        rchdy(57,jrch) = 0.
!!    Floodplain Deposition
        rchdy(58,jrch) = 0.
!!    Total suspended sediments
	  rchdy(59,jrch) = 0.

!! do not perform sediment routing for headwater subbasins
	    !! when i_subhw = 0
	  if (i_subhw == 0 .and. inum1 == inum2) then
          if (ievent == 0) then
            if (rtwtr > 0. .and. rchdep > 0.) then
              sedrch  = varoute(3,inum2)  * (1. - rnum1)
	        rch_san = varoute(23,inum2) * (1. - rnum1)
	        rch_sil = varoute(24,inum2) * (1. - rnum1)
	        rch_cla = varoute(25,inum2) * (1. - rnum1)
	        rch_sag = varoute(26,inum2) * (1. - rnum1)
	        rch_lag = varoute(27,inum2) * (1. - rnum1)
	        rch_gra = varoute(28,inum2) * (1. - rnum1)
            end if
          else
            do ii = 1, nstep
              if (hrtwtr(ii) > 0. .and. hdepth(ii) > 0.) then
                hsedyld(ii) = hhvaroute(3,inum2,ii) * (1. - rnum1)
                sedrch = sedrch + hsedyld(ii)
                rch_san = 0.
	          rch_sil = rch_sil + hsedyld(ii)  !!All are assumed to be silt type particles
                rch_cla = 0.
                rch_sag = 0.
                rch_lag = 0.
	          rch_gra = 0.
              end if
            end do
          end if
        else
            if (ievent == 0) then
            if (ch_eqn(jrch) == 0) call rtsed
            if (ch_eqn(jrch) == 1) call rtsed_bagnold
            if (ch_eqn(jrch) == 2) call rtsed_kodatie
            if (ch_eqn(jrch) == 3) call rtsed_Molinas_Wu
            if (ch_eqn(jrch) == 4) call rtsed_yangsand
          else
            call rthsed
            do ii = 1, nstep
               if (hrtwtr(ii) > 0. .and. hdepth(ii) > 0.) then
                 sedrch = sedrch + hsedyld(ii)
                 rch_sil = rch_sil + hsedyld(ii)  !!All are assumed to be silt type particles
               end if
             end do

            end if      
        end if

!! perform in-stream nutrient calculations
      if (ievent == 0) then
        if (iwq == 2) call watqual2
        if (iwq == 1) call watqual
        if (iwq == 0) call noqual
      else
        if (iwq == 1) call hhwatqual
        if (iwq == 0) call hhnoqual
      end if

!! perform in-stream pesticide calculations
!!      call biofilm
      
!! perform in-stream pesticide calculations
      if (ievent == 0) then
        call rtpest
      else
        call rthpest
      end if

!! perform in-stream bacteria calculations
      call rtbact

!! remove water from reach for irrigation
      call irr_rch

!! remove water from reach for consumptive water use
      call rchuse

!! call subroutine for temperature constant/coefficient Srini 11_1_22
      call temparms
      
!! summarize output/determine loadings to next routing unit
      call rtout

      return
      end