      subroutine rewind_init

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reinitializes values for running different scenarios
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

      use parm

      character (len=80) :: titldum
      integer :: ii

!! reset output arrays/variables
      aairr = 0.
      basminpf = 0.
      basno3f = 0.
      basorgnf = 0.
      basorgpf = 0.
      hruaao = 0.
      hrupsta = 0.
      hrupstm = 0.
      hrupsty = 0.
      hruyro = 0.
      iskip = 0
      wshd_pstdg = 0.
      rchaao = 0.
      rchyro = 0.
      resdata = 0.
      resouta = 0.
      resouty = 0.
      sbactlchlp = 0.
      sbactlchp = 0.
      sbactrolp = 0.
      sbactrop = 0.
      sbactsedlp = 0.
      sbactsedp = 0.
      sdiegrolpq = 0.
      sdiegrolps = 0.
      sdiegropq = 0.
      sdiegrops = 0.
      sno3up = 0.
      spadyev = 0.
      spadyo = 0.
      spadyosp = 0.
      spadyrfv = 0.
      spadysp = 0.
      subaao = 0.
      subyro = 0.
      wpstaao = 0.
      wpstmono = 0.
      wpstyro = 0.
      wshd_aamon = 0.
      wshd_dnit = 0.
      wshd_fixn = 0.
      wshd_fminp = 0.
      wshd_fnh3 = 0.
      wshd_fno3 = 0.
      wshd_forgn = 0.
      wshd_forgp = 0.
      wshd_ftotn = 0.
      wshd_ftotp = 0.
      wshd_hmn = 0.
      wshd_hmp = 0.
      wshd_nitn = 0.
      wshd_nstrs = 0.
      wshd_pal = 0.
      wshd_pas = 0.
      wshd_plch = 0.
      wshd_pinlet = 0.
      wshd_ptile = 0.
      wshd_pstrs = 0.
      wshd_astrs = 0.
      wshd_pup = 0.
      wshd_raino3 = 0.
      wshd_rmn = 0.
      wshd_rmp = 0.
      wshd_rwn = 0.
      wshd_tstrs = 0.
      wshd_voln = 0.
      wshd_wstrs = 0.
      wshd_yldn = 0.
      wshd_yldp = 0.
      wshdaao = 0.
      wshdmono = 0.
      wshdyro = 0.
      wtraa = 0.
      wtryr = 0.
      fcstcnt = 0
      fcstaao = 0.

      wshd_sw = 0.
      wshd_sw = rvar_orig(1)
      wshd_pndv = 0.
      wshd_pndv = rvar_orig(2)
      wshd_resv = 0.
      wshd_resv = rvar_orig(3)
      wshd_ressed = 0.
      wshd_ressed = rvar_orig(4)

!! reset watershed variables
      iyr = 0
      iyr = ivar_orig(1)

!! summation/counter variables
      dtot = 0.0
      immo = 0
      ndmo = 0

!! weather variables
      ffcst = 0
      ifirstpcp = 1
      ifirstt = 1
      ifirsth = 1
      ifirstpet = 1
      ifirsts = 1
      ifirstw = 1
      npcp = 1
      !!weather variables changed during real time/forecast simulation
      pcpsim = 0
      pcpsim = ivar_orig(2)
      tmpsim = 0
      tmpsim = ivar_orig(3)
      rhsim = 0
      rhsim = ivar_orig(4)
      slrsim = 0
      slrsim = ivar_orig(5)
      wndsim = 0
      wndsim = ivar_orig(6)
      wgnold = 0.
      tmpmx = 0.
      tmpmx = otmpmx
      tmpmn = 0.
      tmpmn = otmpmn
      tmpstdmn = 0.
      tmpstdmn = otmpstdmn
      tmpstdmx = 0.
      tmpstdmx = otmpstdmx
      pcp_stat = 0.
      pcp_stat = opcp_stat
      pr_w = 0.
      pr_w = opr_w
      igen = 0
      igen = ogen

!! HRU variables
      snoeb = 0.
      snoeb = orig_snoeb
      sno_hru = 0.
      sno_hru = orig_snohru
      snotmp = 0.
      snotmpeb = 0.
      tmp_hi = 0.
      tmp_lo = 0.
      tmpavp = 0.

      igro = 0
      igro = orig_igro
      laiday = 0.
      laiday = orig_alai
      cht = 0.
      canstor = 0.
      pltfr_n = 0.
      pltfr_p = 0.
      plantn = 0.
      plantp = 0.
      bio_hv = 0.
      bio_aahv = 0.
      bio_ms = 0.
      bio_ms = orig_bioms
      phuacc = 0.
      phuacc = orig_phuacc
      phu_plt = 0.
      phu_plt = orig_phu
      tnylda = 0.
      tnylda = orig_tnylda
      tnyld = 0.
      yldkg = 0.
      yldn = 0.
      hvstiadj = 0.
      laimxfr = 0.
      olai = 0.
      rwt = 0.
      plt_et = 0.
      plt_pet = 0.

      icfrt = 0
      icr = 1
      iday_fert = 0
      idorm = 0
      igrz = 0
      irn = 0
      ncrops = 0
      ncut = 1
      ndcfrt = 0
      ntil = 1
      nro = 1
      ndeat = 0

      sumix = 0.
      sumix = orig_sumix

      plt_pst = 0.
      plt_pst = orig_pltpst
      sol_pst = 0.
      sol_pst = orig_solpst
      sol_no3 = 0.
      sol_no3 = orig_solno3
      sol_nh3 = 0.
      sol_orgn = 0.
      sol_orgn = orig_solorgn
      ! Armen March 5, 2009
      sol_n = 0.
      sol_mn = 0.
      sol_mp = 0.
      sol_mc = 0.
      ! Armen March 5, 2009
      sol_orgp = 0.
      sol_orgp = orig_solorgp
      sol_solp = 0.
      sol_solp = orig_solsolp
      sol_tmp = 0.
      sol_tmp = orig_soltmp
      sol_rsd = 0.
      sol_rsd = orig_solrsd
      sol_cov = 0.
      sol_cov = orig_solcov
      sol_fon = 0.
      sol_fon = orig_solfon
      sol_fop = 0.
      sol_fop = orig_solfop
      sol_aorgn = 0.
      sol_aorgn = orig_solaorgn
      sol_actp = 0.
      sol_actp = orig_solactp
      sol_stap = 0.
      sol_stap = orig_solstap
      sol_st = 0.
      sol_st = orig_solst
      sol_sw = 0.
      sol_sw = orig_solsw
      volcr = 0.
      volcr = orig_volcr
      bss = 0.
      newrti = 0.
      pst_lag = 0.
      surf_bs = 0.
      do j = 1, nhru
        call curno(cn2(j),j)
      end do
      bactlp_plt = 0.
      bactp_plt = 0.
      bactlpq = 0.
      bactlps = 0.
      bactpq = 0.
      bactps = 0.
      wshd_pstap = 0.
      rchrg = 0.
      swtrg = 0
      cklsp = 0.
      ovrlnd = 0.
      sci = 0.
      twash = 0.
      zdb = 0.
      aird = 0.

      gw_q = 0.
      shallst = 0.
      shallst = orig_shallst
      deepst = 0.
      deepst = orig_deepst
      shallirr = 0.
      deepirr = 0.

!! HRU impoundments
      pot_vol = 0.
      pot_vol = orig_potvol
      pot_sed = 0.
      pot_sed = orig_potsed
      pot_no3 = 0.
      pot_no3 = orig_potno3
      pot_san = orig_potsed * 0. 
      pot_sil = orig_potsed * 1. 
      pot_cla = orig_potsed * 0. 
      pot_sag = orig_potsed * 0. 
      pot_lag = orig_potsed * 0. 
      potflwi = 0.
      potsedi = 0.
      potsani = 0.
      potsili = 0.
      potclai = 0.
      potsagi = 0.
      potlagi = 0.

      pnd_vol = 0.
      pnd_vol = orig_pndvol
      pnd_sed = 0.
      pnd_sed = orig_pndsed
      pnd_san = orig_pndsed * 0. 
      pnd_sil = orig_pndsed * 1. 
      pnd_cla = orig_pndsed * 0. 
      pnd_sag = orig_pndsed * 0. 
      pnd_lag = orig_pndsed * 0. 
      pnd_no3 = 0.
      pnd_no3 = orig_pndno3
      pnd_solp = 0.
      pnd_solp = orig_pndsolp
      pnd_orgn = 0.
      pnd_orgn = orig_pndorgn
      pnd_orgp = 0.
      pnd_orgp = orig_pndorgp
      pnd_chla = 0.
      pnd_no3g = 0.
      pnd_no3s = 0.
      pnd_seci = 0.
      pnd_psed = 0.
      pnd_solpg = 0.

      wet_vol = 0.
      wet_vol = orig_wetvol
      wet_sed = 0.
      wet_sed = orig_wetsed
      wet_san = orig_wetsed * 0. 
      wet_sil = orig_wetsed * 1. 
      wet_cla = orig_wetsed * 0. 
      wet_sag = orig_wetsed * 0. 
      wet_lag = orig_wetsed * 0. 
      wet_no3 = 0.
      wet_no3 = orig_wetno3
      wet_orgn = 0.
      wet_orgn = orig_wetorgn
      wet_orgp = 0.
      wet_orgp = orig_wetorgp
      wet_solp = 0.
      wet_solp = orig_wetsolp
      wet_chla = 0.
      wet_no3g = 0.
      wet_no3s = 0.
      wet_seci = 0.
      wet_psed = 0.
      wet_solpg = 0.

!! reach variables
      ch_d = 0.
      ch_d = ch_di
      do j = 1, nrch
        ch_s(2,j) = 0.
        ch_s(2,j) = ch_si(j)
        ch_w(2,j) = 0.
        ch_w(2,j) = ch_wi(j)
        call ttcoef(j)
      end do
      rchstor = 0.
      bankst = 0.
      shyd = 0.
      flwin = 0.
      flwout = 0.
      sedpst_conc = 0.
      sedpst_conc = orig_sedpstconc
      chpst_conc = 0.
      ammonian = 0.
      chlora = 0.
      disolvp = 0.
      nitraten = 0.
      nitriten = 0.
      organicn = 0.
      organicp = 0.
      sedst = 0.
      algae = 0.
      rch_cbod = 0.
      rch_dox = 0.
      varoute = 0.
      hhvaroute = 0.

!! reservoir variables
      res_vol = 0.
      res_vol = orig_resvol
      res_sed = 0.
      res_sed = orig_ressed
	res_san = orig_ressed * 0. 
	res_sil = orig_ressed * 1. 
	res_cla = orig_ressed * 0. 
	res_sag = orig_ressed * 0. 
	res_lag = orig_ressed * 0. 
	res_gra = 0.
      lkpst_conc = 0.
      lkpst_conc = orig_lkpstconc
      lkspst_conc = 0.
      lkspst_conc = orig_lkspstconc
      res_solp = 0.
      res_solp = orig_ressolp
      res_orgp = 0.
      res_orgp = orig_resorgp
      res_no3 = 0.
      res_no3 = orig_resno3
      res_no2 = 0.
      res_no2 = orig_resno2
      res_nh3 = 0.
      res_nh3 = orig_resnh3
      res_orgn = 0.
      res_orgn = orig_resorgn
      res_chla = 0.
      res_seci = 0.

!! reset random number generator seeds
      rndseed = 0
      rnd2 = 0.
      rnd3 = 0.
      rnd8 = 0.
      rnd9 = 0.
      call gcycl

!! rewind daily/hourly files
       ifirsthr = 1
       ifirsta = 1
       ifirstr = 1

!! files listed in .fig
       do idum = 1, mhyd
        icode = 0
        ihout = 0
        inum1 = 0
        inum2 = 0
        inum3 = 0
        rnum1 = 0.
        inum4 = 0
        icode = icodes(idum)
        ihout = ihouts(idum)
        inum1 = inum1s(idum)
        inum2 = inum2s(idum)
        inum3 = inum3s(idum)
        rnum1 = rnum1s(idum)
        inum4 = inum4s(idum)

        select case (icode)
          case (6)   !!rechour
            rewind (unit=200+inum1)
              do ii = 1, 6
                read (200+inum1,5000) titldum
              end do
          case (10)  !!recday
            rewind (unit=555+inum1)
              do ii = 1, 6
                read (555+inum1,5000) titldum
              end do
          !!case (14) !!saveconc
          case default
        end select
      end do

!! weather files
       !!precipitation
       do idum = 1, nrgage
        if (rfile(idum) /= '             ') then
          rewind (unit=100+idum)
          do ii = 1, 4
            read (100+idum,5000) titldum
          end do
        end if
       end do
       !! temperature
       do idum = 1, ntgage
        if (tfile(idum) /= '             ') then
          rewind (unit=118+idum)
          do ii = 1, 4
            read (118+idum,5000) titldum
          end do
        end if
       end do
       !! solar radiation
       if (slrfile /= '             ') then
         rewind (unit=137)
         read (137,5000) titldum
       end if
       !! relative humidity
       if (rhfile /= '             ') then
         rewind (unit=138)
         read (138,5000) titldum
       end if
       !! wind speed
       if (wndfile /= '             ') then
         rewind (unit=139)
         read (139,5000) titldum
       end if
       !! PET
       if (petfile /= '             ') then
         rewind (unit=140)
         read (140,5000) titldum
       end if

       !!daily reservoir outflow file
       do idum = 1, nres
         if (iresco(idum) == 3) then
           rewind (unit=350+idum)
           read (350+idum,5000) titldum
         end if
       end do
    
      return
 5000 format (a80)
      end