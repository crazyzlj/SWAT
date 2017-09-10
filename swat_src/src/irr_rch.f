      subroutine irr_rch

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the irrigation operation when the water
!!    source is a reach

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)         |mm H2O        |amount of water applied to HRU on current
!!                                   |day
!!    auto_wstr(:)    |none or mm    |water stress factor which triggers auto
!!                                   |irrigation
!!    divmax(:)       |mm H2O or     |maximum daily irrigation diversion from
!!                    |  10^4 m^3 H2O|the reach (when IRR=1): when value is
!!                                   |positive the units are mm H2O; when the
!!                                   |value is negative, the units are (10**4
!!                                   |m^3 H2O
!!    flag                           |1 = manual 2 = auto
!!    flowfr(:)       |none          |fraction of available flow in reach that
!!                                   |is allowed to be applied to the HRU
!!    flowmin(:)      |m**3/s        |minimum instream flow for irrigation
!!                                   |diversions when IRR=1, irrigation water
!!                                   |will be diverted only when streamflow is
!!                                   |at or above FLOWMIN.
!!    iida            |julian date   |day being simulated (current julian date)
!!    wstrs_id(:)     |none          |water stress identifier:
!!                                   |1 plant water demand
!!                                   |2 soil water deficit
!!    inum1           |none          |reach number
!!    ipot(:)         |none          |number of HRU (in subbasin) that is ponding
!!                                   |water--the HRU that the surface runoff from
!!                                   |current HRU drains into. This variable is
!!                                   |used only for rice paddys or closed
!!                                   |depressional areas
!!    irramt(:)       |mm H2O        |depth of irrigation water applied to
!!                                   |HRU
!!    irrno(:)        |none          |irrigation source location
!!                                   |if IRR=1, IRRNO is the number of the
!!                                   |          reach
!!                                   |if IRR=2, IRRNO is the number of the
!!                                   |          reservoir
!!                                   |if IRR=3, IRRNO is the number of the
!!                                   |          subbasin
!!                                   |if IRR=4, IRRNO is the number of the
!!                                   |          subbasin
!!                                   |if IRR=5, not used
!!    irrsc(:)        |none          |irrigation source code:
!!                                   |1 divert water from reach
!!                                   |2 divert water from reservoir
!!                                   |3 divert water from shallow aquifer
!!                                   |4 divert water from deep aquifer
!!                                   |5 divert water from source outside
!!                                   |  watershed
!!    nair(:)         |none          |sequence number of auto-irrigation
!!                                   |application within the year
!!    nhru            |none          |number of HRUs in watershed
!!    nirr(:)         |none          |sequence number of irrigation application
!!                                   |within the year
!!    nro(:)          |none          |sequence number of year in rotation
!!    phuacc(:)       |none          |fraction of plant heat units accumulated
!!    pot_vol(:)      |m**3 H2O      |current volume of water stored in the
!!                                   |depression/impounded area
!!    rtwtr           |m^3 H2O       |water leaving reach on day
!!    sedrch          |metric tons   |sediment transported out of reach on day
!!    sol_sumfc(:)    |mm H2O        |amount of water held in the soil profile
!!                                   |at field capacity
!!    sol_sw(:)       |mm H2O        |amount of water stored in soil profile on any
!!                                   |given day
!!    strsw(:)        |none          |fraction of potential plant growth achieved
!!                                   |on the day where the reduction is caused by
!!                                   |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nirr(:)     |none          |sequence number of irrigation application
!!                               |within the year
!!    pot_vol(:)  |mm            |current volume of water stored in the
!!                               |depression/impounded area
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sedrch      |metric tons   |sediment transported out of reach on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm => m^3)
!!    flag        |none          |irrigation flag:
!!                               |0 no irrigation operation on current day
!!                               |1 scheduled irrigation
!!                               |2 auto irrigation
!!    jrch        |none          |reach number
!!    k           |none          |HRU number
!!    vminmm      |mm H2O        |maximum amount of water available for
!!                               |irrigation from reach
!!    vmm         |mm H2O        |depth of irrigation water over HRU
!!    vmxi        |mm H2O        |amount of water specified in irrigation
!!                               |operation
!!    vol         |m^3 H2O       |volume of water applied in irrigation 
!!                               |operation
!!    wtrin       |m^3 H2O       |water outflow from reach prior to subtracting
!!                               |irrigation diversions
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Abs, Min
!!    SWAT: irrigate

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch, k, flag, ii
      real :: cnv, vmm, vminmm, vol, wtrin

      jrch = 0
      jrch = inum1

      wtrin = 0.
      wtrin = rtwtr + rchstor(jrch)

      do k = 1, nhru
          !! check for timing of irrigation operation
          flag = 0
          flag = irr_flag(k)
          if (auto_wstr(k) > 0.) then
            if (wstrs_id(k) == 1 .and. strsw(k) < auto_wstr(k)) flag = 2
            if (wstrs_id(k) == 2 .and. sol_sumfc(k) - sol_sw(k) >       
     &             auto_wstr(k)) flag = 2
          end if

            !! Set parameters based on manual or auto irrigation
			if (flag == 1) then
			  sq_rto = irrsq(k)
			  irrsc(k) = irr_sc(k)                              
			  irrno(k) = irr_no(k)
			else
			  sq_rto = irr_asq(k)
			  irrsc(k) = irr_sca(k)
			  irrno(k) = irr_noa(k)                   
			endif

        if (irrsc(k) == 1 .and. irrno(k) == jrch) then
          aird(k) = 0.                                          

          if (flag > 0) then
            !!irrigate only if flow is greater than minimum flow
            if (rtwtr > flowmin(k) * 86400.) then
              cnv = 0.
              cnv = hru_ha(k) * 10.

              vmm = 0.
              vminmm = 0.
              !! compute maximum amount of water allowed in HRU
              if (divmax(k) < 0.) then
                !!divmax units are 10^4 m^3
                vmm = Abs(divmax(k)) * 10000. / cnv
              else
                !! divmax units are mm H2O
                vmm = divmax(k)
              endif
              !! compute maximum amount of water available for irrigation
              !! from reach
              wtr_avail = rtwtr + rchstor(jrch)
              vminmm = (wtr_avail - flowmin(k) * 86400.) * flowfr(k)/cnv
              vmm = Min(vminmm, vmm)

              !! check available against set amount in scheduled operation
              if (flag == 1) then
                vmxi = 0.
                vmxi = irramt(k)                       
                if (vmxi < 1.e-6) vmxi = sol_sumfc(k)
                if (vmm > vmxi) vmm = vmxi
              end if
              if (flag == 2) then
                vmxi = 0.
                vmxi = irr_mx(k)
                if (vmm > vmxi) vmm = vmxi
              end if

              if (vmm > 0.) then
                vol = 0.
                vol = vmm * cnv

       !!         if (ipot(k) == k) then
                if (pot_fr(k) > 1.e-6) then
                  pot_vol(k) = pot_vol(k) + vol / (10. * potsa(k))
                else
                  call irrigate(k,vmm)
                end if

                !! subtract irrigation from reach outflow
           !!     if (ipot(k) /= k) then
                if (pot_fr(k) > 1.e-6) then
                  vol = 0.
                  vol = aird(k) * cnv
                end if
                if (ievent > 0) then
                  do ii = 1, nstep
                    hrtwtr(ii) = hrtwtr(ii) - vol * hrtwtr(ii) / rtwtr
                    if (hrtwtr(ii) < 0.) hrtwtr(ii) = 0.
                  end do
                end if
!!                xx = vol     							                           !! BN: replaced "wtrin" with "vol"
                vol = vol / irr_eff(k)   !! BN: inserted to account for irr. efficiency                                             
                xx = (wtr_avail - flowmin(k) * 86400.) * flowfr(k)                 !! BN: inserted: xx = available/allowed amount in m3/s
                xx = Min(xx, vol)                                                  !! BN: inserted abstracted water cannot be more than allowed/available amount
                if (xx > rchstor(jrch)) then
                  xx = vol - rchstor(jrch)                                         !! BN: replaced "wtrin" with "vol"
                  rchstor(jrch) = 0.
                else
                  rchstor(jrch) = rchstor(jrch) - xx
                  xx = 0.
                end if
                if (xx > 0.) then
                  rtwtr = rtwtr - xx
                  rtwtr = amax1(0., rtwtr)
                end if

                !! advance irrigation operation number
                if (flag == 1) then
                  nirr(k) = nirr(k) + 1
                end if
            
            end if
            
            if (imgt == 1) then
             write (143, 1000) subnum(k), hruno(k), iyr, i_mo, iida, 
     *       hru_km(j), "         ",  " AUTOIRR", phubase(k), phuacc(k),
     *      sol_sw(k), bio_ms(k), sol_rsd(1,k),sol_sumno3(k),
     *      sol_sumsolp(k), aird(k), irrsc(k), irrno(k)
1000        format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,7f10.2,10x,f10.2,70x,
     *      i10,10x,i10) 
            end if
            end if
          end if
          end if
          
      end do

      if (wtrin /= rtwtr .and. wtrin > 0.01) then
        sedrch = sedrch * rtwtr / wtrin

        rch_san = rch_san * rtwtr / wtrin
        rch_sil = rch_sil * rtwtr / wtrin
        rch_cla = rch_cla * rtwtr / wtrin
        rch_sag = rch_sag * rtwtr / wtrin
        rch_lag = rch_lag * rtwtr / wtrin
        rch_gra = rch_gra * rtwtr / wtrin

        if (sedrch  < 1.e-6) then
	    sedrch = 0.
	    rch_san = 0.
          rch_sil = 0.
          rch_cla = 0.
          rch_sag = 0.
          rch_lag = 0.
          rch_gra = 0.
	  end if

        if (ievent > 0) then
          do ii = 1, nstep
            hsedyld(ii) = hsedyld(ii) * rtwtr / wtrin
          end do
        end if
      end if

      return
      end