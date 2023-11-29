      subroutine allocate_parms
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine allocates array sizes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mapp        |none          |max number of applications
!!    mch         |none          |max number of channels
!!    mcr         |none          |max number of crops grown per year
!!    mcrdb       |none          |max nunber of crops in crop.dat
!!    mcut        |none          |max number of cuttings per year
!!    mfdb        |none          |max number of fertilizers in fert.dat
!!    mgr         |none          |max number of grazings per year
!!    mhru        |none          |max number of HRUs
!!    mhyd        |none          |max number of hydrographs
!!    mlyr        |none          |max number of soil layers
!!    mnr         |none          |max number of years of rotation
!!    mpst        |none          |max number of pesticides used in wshed
!!    mpdb        |none          |max number of pesticides in pest.dat
!!    mrecc       |none          |max number of reccnst files
!!    mrecd       |none          |max number of recday files
!!    mrech       |none          |max number of rechour files
!!    mrecm       |none          |max number of recmon files
!!    mrecy       |none          |max number of recyear files
!!    mres        |none          |max number of reservoirs
!!    mrg         |none          |max number of rainfall/temp gages
!!    nstep       |none          |max number of time steps per day
!!    msub        |none          |max number of subbasins
!!    mtil        |none          |max number of tillage types in till.dat
!!    mudb        |none          |max number of urban land types in urban.dat
!!    myr         |none          |max number of years of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mhruo       |none          |max number of variables in output.hru
!!    mrcho       |none          |max number of variables in reach file
!!    mstdo       |none          |max number of variables summarized in 
!!                               |output.std
!!    msubo       |none          |max number of variables in output.sub
!!    mvaro       |none          |max number of variables routed through the
!!                               |reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
            
      allocate (alph_e(mhru))
      allocate (co_p(mhru))      
      
!! initialize variables    
      mvaro = 45    !!!! salty dog +1
      mhruo = 79
      mrcho = 74 !!modified from 62 for salt - srini   +1 salty dog
      msubo = 24
      mstdo = 113
      motot = 1200    !!! 100 year limit
      
!! ppet arrays for tropical growth
      allocate (ppet(mhru))
      do ihru = 1, mhru
        allocate (ppet(ihru)%precip(ppet(ihru)%ndays))
        allocate (ppet(ihru)%pet(ppet(ihru)%ndays))
        ppet(ihru)%precip = 0.
        ppet(ihru)%pet = 0.
      end do
      
!! Srini 11_1_22
      allocate (tmp_win1(mhru))
      allocate (tmp_win2(mhru))
      allocate (tmp_sum1(mhru))
      allocate (tmp_sum2(mhru))
      allocate (tmp_spr1(mhru))
      allocate (tmp_spr2(mhru))
      allocate (tmp_fal1(mhru))
      allocate (tmp_fal2(mhru))
        
      allocate (tilep(mhru))
      allocate (surlag(mhru))
      allocate (cdn(mhru))
      allocate (cmn(mhru))
      allocate (nperco(mhru))
      allocate (phoskd(mhru))
      allocate (psp(mhru))
      allocate (sdnco(mhru))
!!!!!!!!!! drains
      allocate (wnan(mlyr))

!!      allocate (rcn(12,msub))

!!    arrays for Landscape Transport Capacity
      allocate (l_k1(msub))
      allocate (l_k2(msub))
      allocate (l_lambda(msub))
      allocate (l_beta(msub))
      allocate (l_gama(msub))
      allocate (l_harea(msub))
      allocate (l_vleng(msub))
      allocate (l_vslope(msub))
      allocate (l_ktc(msub))
		
!!    arrays for Biofilm in reach
      allocate (biofilm_mumax(mch))
      allocate (biofilm_kinv(mch))
      allocate (biofilm_klw(mch))
      allocate (biofilm_kla(mch))
      allocate (biofilm_cdet(mch))
      allocate (biofilm_bm(mch))
      
      mxsubch = Max(msub+1,mch+1)
      itempa = Max(mhru,mch)     
	
!!    new arrays for routing units
      allocate (hru_rufr(mru,mhru))   
      allocate (daru_km(msub,mru))   
      allocate (ru_k(msub,mru))      
      allocate (ru_c(msub,mru))      
      allocate (ru_eiq(msub,mru))    
      allocate (ru_ovs(msub,mru))    
      allocate (ru_ovsl(msub,mru))   
      allocate (ru_a(msub,mru))      
      allocate (ru_ktc(msub,mru))    
      allocate (gwq_ru(mhru))
      allocate (mhyd1(msub))
      allocate (ils2(mhru))
      allocate (ils2flag(msub))
      allocate (irtun(msub))
      allocate (ifirsthr(mrech))

!!    arrays which contain data related to the number of recday commands
      allocate (ifirstr(mrecd))

!!    arrays which contain data related to the date
      allocate (values(8))

!!    arrays which contain data related to rainfall/temperature gages
!!     test for JRW
      allocate (elevp(mrg))
      allocate (elevt(mrg))
      allocate (ifirstpcp(mrg))
      allocate (ifirstt(mrg))

!!    apex/command variables
      allocate (ifirsta(mapex))
      allocate (iypa(mapex))
      allocate (idapa(mapex))
      allocate (flodaya(mapex))
      allocate (seddaya(mapex))
      allocate (orgndaya(mapex))
      allocate (orgpdaya(mapex))
      allocate (no3daya(mapex))
      allocate (minpdaya(mapex))

!! septic inputs
      allocate (isep_hru(mhru))
      allocate (sptqs(msdb+1))
      allocate (sptbodconcs(msdb+1))
      allocate (spttssconcs(msdb+1))
      allocate (spttnconcs(msdb+1))
      allocate (sptnh4concs(msdb+1))
      allocate (sptno3concs(msdb+1))
      allocate (sptno2concs(msdb+1))
      allocate (sptorgnconcs(msdb+1))
      allocate (spttpconcs(msdb+1))  
      allocate (sptminps(msdb+1))
      allocate (sptorgps(msdb+1))
      allocate (sptfcolis(msdb+1))
!! pothole changes for srini
      allocate (spill_hru(mhru))
      allocate (spill_precip(mhru))
      allocate (tile_out(mhru))
      allocate (pot_seep(mhru))
      allocate (pot_sedin(mhru))
      allocate (pot_evap(mhru))
      allocate (hru_in(mhru))
      allocate (pot_solp(mhru))
      allocate (pot_solpi(mhru))
      allocate (pot_orgp(mhru))
      allocate (pot_orgpi(mhru))
      allocate (pot_orgn(mhru))
      allocate (pot_orgni(mhru))
      allocate (pot_mps(mhru))
      allocate (pot_mpsi(mhru))
      allocate (pot_mpa(mhru))
      allocate (pot_mpai(mhru))
      allocate (pot_no3i(mhru))
      allocate (precip_in(mhru))
      allocate (tile_sedo(mhru))
      allocate (tile_no3o(mhru))
      allocate (tile_solpo(mhru))
      allocate (tile_orgno(mhru))
      allocate (tile_orgpo(mhru))
      allocate (tile_minpso(mhru))
      allocate (tile_minpao(mhru))
!! septic changes added 1/28/09 gsm
      allocate (percp(mhru))
      allocate (i_sep(mhru))
      allocate (sep_tsincefail(mhru))
      allocate (isep_tfail(mhru))
      allocate (failyr(mhru))
      allocate (qstemm(mhru))
      allocate (sep_cap(mhru))
      allocate (bz_area(mhru))
      allocate (bio_amn(mhru))
      allocate (bio_bod(mhru))
      allocate (biom(mhru))
      allocate (rbiom(mhru))
      allocate (fcoli(mhru))
      allocate (bio_ntr(mhru))
      allocate (bz_perc(mhru))
      allocate (bz_z(mhru))
      allocate (bz_thk(mhru))
      allocate (bio_bd(mhru))
!!    carbon outputs for .hru file
      allocate (cmup_kgh(mhru))
      allocate (cmtot_kgh(mhru))
!!    carbon outputs for .hru file
      allocate (coeff_bod_dc(mhru))
      allocate (coeff_bod_conv(mhru))
      allocate (coeff_fc1(mhru))
      allocate (coeff_fc2(mhru))
      allocate (coeff_fecal(mhru))
      allocate (coeff_plq(mhru))
      allocate (coeff_mrt(mhru))
      allocate (coeff_rsp(mhru))
      allocate (coeff_slg1(mhru))
      allocate (coeff_slg2(mhru))
      allocate (coeff_nitr(mhru))
      allocate (coeff_denitr(mhru))
	  allocate (isep_typ(mhru))
	  allocate (isep_opt(mhru))
      allocate (plqm(mhru))
      allocate (coeff_pdistrb(mhru))
      allocate (coeff_psorpmax(mhru))
      allocate (coeff_solpslp(mhru))
      allocate (coeff_solpintc(mhru))
      allocate (isep_iyr(mhru))
      allocate (sep_strm_dist(mhru))
      allocate (sep_den(mhru))

!! septic changes added 1/28/09 gsm
      allocate (qird(mhru))
     
!!    arrays which contain data related to channels
      allocate (algae(mch))
      allocate (alpha_bnk(mxsubch)) 
      allocate (alpha_bnke(mxsubch)) 
      allocate (ammonian(mch))
      allocate (bankst(mch))
      allocate (bc1(mch))
      allocate (bc2(mch))
      allocate (bc3(mch))
      allocate (bc4(mch))
      allocate (ch_bnk_bd(mch))
      allocate (ch_cov(mch))
      allocate (ch_cov1(mch))
      allocate (ch_cov2(mch))
      allocate (ch_bnk_d50(mch))
      allocate (ch_eqn(mch))    
!      allocate (ch_erod(mch))
      allocate (ch_li(mch))
      allocate (ch_onco(mch))
      allocate (ch_opco(mch))
      allocate (ch_orgn(mch))
      allocate (ch_orgp(mch))
      allocate (ch_si(mch))
      allocate (ch_wdr(mch))
      allocate (ch_wi(mch))
      allocate (ch_erodmo(mch,12))
      allocate (chlora(mch))
      allocate (chpst_conc(mch))
      allocate (chpst_koc(mch))
      allocate (chpst_mix(mch))
      allocate (chpst_rea(mch))
      allocate (chpst_rsp(mch))
      allocate (chpst_stl(mch))
      allocate (chpst_vol(mch))
      allocate (dep_chan(mch))
      allocate (disolvp(mch))
      allocate (drift(mch))
      allocate (flwin(mch))
      allocate (flwout(mch))
      allocate (icanal(mch))
      allocate (nitraten(mch))
      allocate (nitriten(mch))
      allocate (organicn(mch))
      allocate (organicp(mch))
      allocate (orig_sedpstconc(mch))
      allocate (rch_bactlp(mch))
      allocate (rch_bactp(mch))
      allocate (rch_cbod(mch))
      allocate (rch_dox(mch))
      allocate (rchstor(mch))
      allocate (rk1(mch))
      allocate (rk2(mch))
      allocate (rk3(mch))
      allocate (rk4(mch))
      allocate (rk5(mch))
      allocate (rk6(mch))
      allocate (rs1(mch))
      allocate (rs2(mch))
      allocate (rs3(mch))
      allocate (rs4(mch))
      allocate (rs5(mch))
      allocate (rs6(mch))
      allocate (rs7(mch))
      allocate (sedpst_act(mch))
      allocate (sedpst_bry(mch))
      allocate (sedpst_conc(mch))
      allocate (sedpst_rea(mch))
      allocate (sedst(mch))
      allocate (vel_chan(mch))

      allocate (wurch(12,mxsubch)) 

!!    arrays for channel added by Balaji for the new routines
      allocate (ch_bnk_san(mch))
      allocate (ch_bnk_sil(mch))
      allocate (ch_bnk_cla(mch))
      allocate (ch_bnk_gra(mch))
      allocate (ch_bed_san(mch))
      allocate (ch_bed_sil(mch))
      allocate (ch_bed_cla(mch))
      allocate (ch_bed_gra(mch))
      allocate (ch_bed_bd(mch))
      allocate (ch_bed_d50(mch))
      allocate (ch_bnk_kd(mch))
      allocate (ch_bed_kd(mch))
      allocate (tc_bnk(mch))
      allocate (tc_bed(mch))
      allocate (depfp(mch))
      allocate (depprfp(mch))
      allocate (depsanfp(mch))
      allocate (depsilfp(mch))
      allocate (depclafp(mch))
      allocate (depsagfp(mch))
      allocate (deplagfp(mch))
      allocate (depgrafp(mch))
      allocate (depch(mch))
      allocate (depprch(mch))
      allocate (depsanch(mch))
      allocate (depsilch(mch))
      allocate (depclach(mch))
      allocate (depsagch(mch))
      allocate (deplagch(mch))
      allocate (depgrach(mch))
      allocate (sanst(mch))
      allocate (silst(mch))
      allocate (clast(mch))
      allocate (sagst(mch))
      allocate (lagst(mch))
      allocate (grast(mch))

!!    arrays which contain data related to reach output 
      allocate (icolr(mrcho))
!     allocate (ipdvar(mrcho))
!!  increased ipdvar(42) to 45 to add Total N/Total P/NO3conc(mg/l)
!!  increased ipdvar(46) to 56 to add salt - srini
!!  increased ipdvar(56) to 58 to add sar and ec - Katrin
      allocate (ipdvar(58))    
      allocate (rchaao(mrcho,mxsubch))  
      allocate (rchdy(mrcho,mxsubch))  
      allocate (rchmono(mrcho,mxsubch))  
      allocate (rchyro(mrcho,mxsubch))  

!!    arrays which contain data related to subbasins
      allocate (sub_tilep(msub))
      allocate (ch_revap(mxsubch)) 
      allocate (cncoef_sub(msub))
      allocate (co2(msub))
      allocate (daylmn(msub))
      allocate (drydep_no3(msub))
      allocate (drydep_nh4(msub))
      
!!!   atmospheric deposition by month
      allocate (rcn_mo(motot,msub))
      allocate (rammo_mo(motot,msub))
      allocate (drydep_no3_mo(motot,msub))
      allocate (drydep_nh4_mo(motot,msub))
!!!   atmospheric deposition by day      
      allocate (rcn_d(msub))
      allocate (rammo_d(msub))
      allocate (drydep_no3_d(msub))
      allocate (drydep_nh4_d(msub))
      
      allocate (fcst_reg(msub))
      allocate (harg_petco(msub))
!      allocate (hqd(nstep*3+1))  !! was 73, changed for urban
      allocate (hqdsave(msub,nstep*4))  !! was 49, changed for urban -> changed to 2d array J.Jeong 4/17/2009
      allocate (hsdsave(msub,nstep*4))  !! J.Jeong 4/22/2009
  !!    allocate (hqd(73))
  !!    allocate (hqdsave(msub,49))
      allocate (hru1(msub))
      allocate (hrutot(msub))
      allocate (ihgage(msub))
      allocate (ireg(msub))
      allocate (irelh(msub))
      allocate (irgage(msub))
      allocate (isgage(msub))
      allocate (itb(msub))
      allocate (itgage(msub))
      allocate (iwgage(msub))
      allocate (latcos(msub))
      allocate (latsin(msub))
      allocate (pcpdays(msub))
      allocate (phutot(msub))
      allocate (plaps(msub))
      allocate (rammo_sub(msub))
      allocate (atmo_day(msub))
      allocate (rcn_sub(msub))
      allocate (sub_bactlp(msub))
      allocate (sub_bactp(msub))
      allocate (sub_bd(msub))
      allocate (sub_cbod(msub))
      allocate (sub_chl(msub))
      allocate (sub_dsan(msub))
      allocate (sub_dsil(msub))
      allocate (sub_dcla(msub))
      allocate (sub_dsag(msub))
      allocate (sub_dlag(msub))
      allocate (sub_dox(msub))
      allocate (sub_elev(msub))
      allocate (sub_etday(msub))
      allocate (sub_fr(msub))
      allocate (sub_gwno3(msub))
      allocate (sub_gwsolp(msub))
      allocate (sub_gwq(msub))
      allocate (sub_gwq_d(msub))
      allocate (sub_km(msub))
      allocate (sub_lat(msub))
      allocate (sub_latq(msub))
      allocate (sub_tileq(msub))
      allocate (sub_vaptile(msub))
      allocate (sub_latno3(msub))
      allocate (sub_minp(msub))
      allocate (sub_minpa(msub))
      allocate (sub_minps(msub))
      allocate (sub_no3(msub))
      allocate (sub_orgn(msub))
      allocate (sub_orgp(msub))

      allocate (sub_precip(msub))
      allocate (sub_qd(msub))
      allocate (sub_sedpa(msub))
      allocate (sub_sedps(msub))
      allocate (sub_sedy(msub))
      allocate (sub_sep(msub))
      allocate (sub_tileno3(msub))
      allocate (sub_snom(msub))
      allocate (sub_solp(msub))
      allocate (sub_solpst(msub))
      allocate (sub_sorpst(msub))
      allocate (sub_subp(msub))
      allocate (sub_sumfc(msub))
      allocate (sub_surfq(msub))
      allocate (sub_sw(msub))
      allocate (sub_tc(msub))
      allocate (sub_tran(msub))
      allocate (sub_wtmp(msub))
      allocate (sub_wyld(msub))
      allocate (sub_yorgn(msub))
      allocate (sub_yorgp(msub))
      allocate (subfr_nowtr(msub))
      allocate (subgis(msub))
      allocate (tlaps(msub))
      allocate (tmp_an(msub))
      allocate (wcklsp(msub))
      allocate (welev(msub))
      allocate (wlat(msub))

      allocate (sub_pst(mpst,msub))

      allocate (huminc(msub,12))
      allocate (radinc(msub,12))
      allocate (rfinc(msub,12))
      allocate (tmpinc(msub,12))
  !!    allocate (sub_hhqd(msub,24))
  !!    allocate (sub_hhwtmp(msub,24))
  !!    allocate (uh(msub,49))

      allocate (sub_sftmp(10,msub))
      allocate (sub_smtmp(10,msub))
      allocate (sub_smfmx(10,msub))
      allocate (sub_smfmn(10,msub))
      allocate (sub_timp(10,msub))
      allocate (sub_hhqd(msub,nstep))  ! 24 changed to nstep 4 urban modeling  Oct. 19,2007
      allocate (sub_hhwtmp(msub,nstep))   ! 24 changed to nstep 4 urban modeling  Oct. 19,2007
      allocate (uh(msub,nstep*3+1))      !! was 49 changed to nstep  OCt22, 2007

      allocate (ch_k(2,mxsubch))
      allocate (ch_n(2,mxsubch))
      allocate (ch_s(2,mxsubch))
      allocate (ch_w(2,mxsubch))   
      allocate (ch_l2(mxsubch))
      allocate (ch_d(mxsubch))
      allocate (chside(mxsubch))
      allocate (ch_di(mxsubch))
      allocate (sub_pet(mxsubch))
      allocate (elevb(10,msub))
      allocate (elevb_fr(10,msub))
      allocate (amp_r(12,msub))
      allocate (dewpt(12,msub))
      allocate (pcf(12,msub))
      allocate (solarav(12,msub))
      allocate (tmpmn(12,msub))
      allocate (tmpmx(12,msub))
      allocate (tmpstdmn(12,msub))
      allocate (tmpstdmx(12,msub))
      allocate (wndav(12,msub))

      allocate (pcp_stat(12,3,msub))
      allocate (pr_w(3,12,msub))

!!    arrays which contain data related to forecast parameters
      allocate (ftmpmn(12,msub))
      allocate (ftmpmx(12,msub))
      allocate (ftmpstdmn(12,msub))
      allocate (ftmpstdmx(12,msub))
      allocate (fpcp_stat(12,3,msub))
      allocate (fpr_w(3,12,msub))
      allocate (otmpmn(12,msub))
      allocate (otmpmx(12,msub))
      allocate (otmpstdmn(12,msub))
      allocate (otmpstdmx(12,msub))
      allocate (opcp_stat(12,3,msub))
      allocate (opr_w(3,12,msub))

!!    arrays which contain data related to subbasin output
      allocate (submono(msubo,msub))
      allocate (subaao(msubo,msub))
      allocate (subyro(msubo,msub))
      allocate (ipdvab(msubo))
      allocate (icolb(msubo))

!!    arrays which contain data related to soil layers, HRUs
!    Drainmod tile equations  01/2006
      allocate (vwt(mlyr,mhru))
      allocate (wat_tbl(mhru))     
      allocate (sol_swpwt(mhru))
      allocate (conk(mlyr,mhru))
	allocate (r2adj(mhru))	        
      allocate (sol_stpwt(mlyr,mhru)) !Moriasi 4/8/2014
!    Drainmod tile equations  01/2006
      allocate (conv_wt(mlyr,mhru))
      allocate (crdep(mlyr,mhru))
      allocate (flat(mlyr,mhru))
      allocate (orig_solactp(mlyr,mhru))
      allocate (orig_solaorgn(mlyr,mhru))
      allocate (orig_solfon(mlyr,mhru))
      allocate (orig_solfop(mlyr,mhru))
      allocate (orig_solno3(mlyr,mhru))
      allocate (orig_solorgn(mlyr,mhru))
      allocate (orig_solorgp(mlyr,mhru))
      allocate (orig_solrsd(mlyr,mhru))
      allocate (orig_solsolp(mlyr,mhru))
      allocate (orig_solst(mlyr,mhru))
      allocate (orig_solstap(mlyr,mhru))
      allocate (orig_soltmp(mlyr,mhru))
      allocate (orig_volcr(mlyr,mhru))
      allocate (pperco_sub(mlyr,mhru))
      allocate (sol_actp(mlyr,mhru))
      allocate (sol_aorgn(mlyr,mhru))
      allocate (sol_awc(mlyr,mhru))
      allocate (sol_bd(mlyr,mhru))
      allocate (sol_cbn(mlyr,mhru))
      allocate (sol_clay(mlyr,mhru))
!  added 1/27/09 when making septic changes
      allocate (sol_ec(mlyr,mhru))
!  added 1/27/09 when making septic changes
      allocate (sol_fc(mlyr,mhru))
      allocate (sol_fon(mlyr,mhru))
      allocate (sol_fop(mlyr,mhru))
      allocate (sol_hk(mlyr,mhru))
      allocate (sol_hum(mlyr,mhru))
      allocate (sol_k(mlyr,mhru))
      allocate (sol_nh3(mlyr,mhru))
      allocate (sol_no3(mlyr,mhru))
      allocate (sol_orgn(mlyr,mhru))
      allocate (sol_orgp(mlyr,mhru))
      allocate (sol_por(mlyr,mhru))
      allocate (sol_prk(mlyr,mhru))
      allocate (sol_rock(mlyr,mhru))
      allocate (sol_rsd(mlyr,mhru))
      allocate (sol_sand(mlyr,mhru))
      allocate (sol_silt(mlyr,mhru))
      allocate (sol_solp(mlyr,mhru))
      allocate (sol_st(mlyr,mhru))
      allocate (sol_stap(mlyr,mhru))
      allocate (sol_tmp(mlyr,mhru))
      allocate (sol_ul(mlyr,mhru))
      allocate (sol_up(mlyr,mhru))
      allocate (sol_wp(mlyr,mhru))
      allocate (sol_wpmm(mlyr,mhru))
      allocate (sol_z(mlyr,mhru))
      allocate (volcr(mlyr,mhru))

!!    arrays which contain data related to soil layers, HRUs, pesticides
      allocate (orig_solpst(mpst,mhru,mlyr))
      allocate (sol_kp(mpst,mhru,mlyr))
      allocate (sol_pst(mpst,mhru,mlyr))

!!    arrays which contain data related to transfer command
      allocate (mo_transb(100))
      allocate (mo_transe(100))
      allocate (ih_tran(100))

!!    arrays which contain data related to reservoirs
      allocate (br1(mres))
      allocate (br2(mres))
      allocate (chlar(mres))
      allocate (evrsv(mres))
      allocate (iflod1r(mres))
      allocate (iflod2r(mres))
      allocate (ires1(mres))
      allocate (ires2(mres))
      allocate (iresco(mres))
      allocate (iyres(mres))
      allocate (lkpst_conc(mres))
      allocate (lkpst_koc(mres))
      allocate (lkpst_mix(mres))
      allocate (lkpst_rea(mres))
      allocate (lkpst_rsp(mres))
      allocate (lkpst_stl(mres))
      allocate (lkpst_vol(mres))
      allocate (lkspst_act(mres))
      allocate (lkspst_bry(mres))
      allocate (lkspst_conc(mres))
      allocate (lkspst_rea(mres))
      allocate (theta_n(mres))
      allocate (theta_p(mres))
      allocate (con_nirr(mres))
      allocate (con_pirr(mres))
      allocate (mores(mres))
      allocate (ndtargr(mres))
      allocate (oflowmn_fps(mres))
      allocate (orig_lkpstconc(mres))
      allocate (orig_lkspstconc(mres))
      allocate (orig_resnh3(mres))
      allocate (orig_resno2(mres))
      allocate (orig_resno3(mres))
      allocate (orig_resorgn(mres))
      allocate (orig_resorgp(mres))
      allocate (orig_ressed(mres))
      allocate (orig_ressolp(mres))
      allocate (orig_resvol(mres))
      allocate (res_bactlp(mres))
      allocate (res_bactp(mres))
      allocate (res_chla(mres))
      allocate (res_esa(mres))
      allocate (res_evol(mres))
      allocate (res_k(mres))
      allocate (res_nh3(mres))
      allocate (res_no2(mres))
      allocate (res_no3(mres))
      allocate (res_nsed(mres))
      allocate (res_orgn(mres))
      allocate (res_orgp(mres))
      allocate (res_psa(mres))
      allocate (res_pvol(mres))
      allocate (res_rr(mres))
      allocate (res_seci(mres))
      allocate (res_sed(mres))

      allocate (res_san(mres))
      allocate (res_sil(mres))
      allocate (res_cla(mres))
      allocate (res_sag(mres))
      allocate (res_lag(mres))
      allocate (res_gra(mres))

      allocate (res_solp(mres))
      allocate (res_sub(mres))
      allocate (res_vol(mres))
      allocate (seccir(mres))
      allocate (sed_stlr(mres))
      allocate (starg_fps(mres))
      allocate (weirc(mres))
      allocate (weirk(mres))
      allocate (weirw(mres))
      allocate (acoef(mres))
      allocate (bcoef(mres))
      allocate (ccoef(mres))
      allocate (wurtnf(mres))
      allocate (lkpst_mass(mres))
      allocate (lkspst_mass(mres))

      allocate (nsetlr(2,mres))
      allocate (psetlr(2,mres))
      allocate (oflowmx(12,mres))
      allocate (oflowmn(12,mres))
      allocate (starg(12,mres))
      allocate (wuresn(12,mres))
 
 !!  added per JGA for Srini by gsm 9/8/2011
 !! arrays for mangement output (output.mgt)  
      allocate (sol_sumno3(mhru))
      allocate (sol_sumsolp(mhru))
      allocate (strsw_sum(mhru))
      allocate (strstmp_sum(mhru))
      allocate (strsn_sum(mhru))
      allocate (strsp_sum(mhru))
      allocate (strsa_sum(mhru))
      allocate (velsetlr(mres))

!! arrays for reservoir output
      allocate (icolrsv(41))
      allocate (resoutm(41,mres))
      allocate (resouta(41,mres))
      allocate (resouty(41,mres))

      allocate (resdata(7))

!!    arrays which contain data related to reservoirs, year
      allocate (res_out(mres,12,myr))

!!    arrays which contain data related to pesticides in database
      allocate (ap_ef(mpdb))
      allocate (decay_f(mpdb))
      allocate (decay_s(mpdb))
      allocate (hlife_f(mpdb))
      allocate (hlife_s(mpdb))
      allocate (nope(mpdb))
      allocate (pst_wof(mpdb))
      allocate (pst_wsol(mpdb))
      allocate (skoc(mpdb))

!!    arrays which contain data related to landcover/landuse in database
      allocate (alai_min(mcrdb))
!     allocate (air_str(mcrdb))
      allocate (bio_e(mcrdb))
      allocate (bio_leaf(mcrdb))
      allocate (bio_n1(mcrdb))
      allocate (bio_n2(mcrdb))
      allocate (bio_p1(mcrdb))
      allocate (bio_p2(mcrdb))
      allocate (blai(mcrdb))
      allocate (bm_dieoff(mcrdb))
      allocate (bmx_trees(mcrdb))
      allocate (chtmx(mcrdb))
      allocate (cnyld(mcrdb))
      allocate (cpyld(mcrdb))
      allocate (cvm(mcrdb))
      allocate (dlai(mcrdb))
      allocate (ext_coef(mcrdb))
      allocate (gsi(mcrdb))
      allocate (hvsti(mcrdb))
      allocate (idc(mcrdb))
      allocate (leaf1(mcrdb))
      allocate (leaf2(mcrdb))
      allocate (mat_yrs(mcrdb))
      allocate (rdmx(mcrdb))
      allocate (rsdco_pl(mcrdb))
      allocate (rsr1(mcrdb))
      allocate (rsr2(mcrdb))
      allocate (t_base(mcrdb))
      allocate (t_opt(mcrdb))
      allocate (vpd2(mcrdb))
      allocate (wac21(mcrdb))
      allocate (wac22(mcrdb))
      allocate (wavp(mcrdb))
      allocate (wsyf(mcrdb))

      allocate (pltnfr(3,mcrdb))
      allocate (pltpfr(3,mcrdb))

!!    arrays which contain data related to fertilizers in database
      allocate (bactkddb(mfdb))
      allocate (bactlpdb(mfdb))
      allocate (bactpdb(mfdb))
      allocate (fminn(mfdb))
      allocate (fminp(mfdb))
      allocate (fnh3n(mfdb))
      allocate (forgn(mfdb))
      allocate (forgp(mfdb))

!!    arrays which contain data related to urban land types in database
      allocate (curbden(mudb))
      allocate (dirtmx(mudb))
      allocate (fcimp(mudb))
      allocate (fimp(mudb))
      allocate (thalf(mudb))
      allocate (tnconc(mudb))
      allocate (tno3conc(mudb))
      allocate (tpconc(mudb))
      allocate (urbcoef(mudb))
      allocate (urbcn2(mudb))

!!    arrays which contain data related to years of rotation,
!!    applications, and HRUs
      allocate (auto_wstr(mhru))
      allocate (irr_daymin(mhru))
      allocate (irr_daycur(mhru))
     
      allocate (cfrt_id(mhru))
      allocate (cfrt_kg(mhru))
      allocate (cpst_id(mhru))
      allocate (cpst_kg(mhru))
     
      allocate (wstrs_id(mhru))
      allocate (ifrt_freq(mhru))
      allocate (ipst_freq(mhru))

      allocate (imp_trig(mhru))
      allocate (irr_asq(mhru))
      allocate (irr_mx(mhru))
      allocate (irrsq(mhru))
      allocate (irr_eff(mhru))
      allocate (irrefm(mhru))
      allocate (irrsalt(mhru))

      allocate (irr_sc(mhru))
      allocate (irr_no(mhru))
      allocate (irr_sca(mhru))
      allocate (irr_noa(mhru))
      allocate (fert_days(mhru))
      allocate (pest_days(mhru))

 !!   burn 3/5/09









!!    changes pesticide incorporation in soil 3/31/08 gsm



!!    arrays which contain data related to years of rotation,
!!    crops grown per year, and HRUs
     
      allocate (bio_aahv(mcr,mhru))
      allocate (bio_hv(mcr,mhru))
   
  
      allocate (hi_targ(mhru))
      allocate (idplt(mhru))
      allocate (mcrhru(mhru))
      allocate (idplrot(mcr,mhru))      
      allocate (ncrops(mcr,mhru))
      allocate (orig_phu(mhru))

      allocate (phu_plt(mhru))
      allocate (nstress(mhru))
      allocate (igrotree(mhru))
      allocate (tnyld(mhru))
      allocate (tnylda(mhru))
      allocate (yldkg(mcr,mhru))
      allocate (yldn(mcr,mhru))

!!    arrays which contain data related to years of rotation,
!!    grazings per year, and HRUs
      allocate (bio_eat(mhru))
      allocate (bio_trmp(mhru))
      allocate (grz_days(mhru))
      allocate (manure_id(mhru))
      allocate (phug(mnr,mgr,mhru))
      allocate (manure_kg(mhru))

!!    arrays which contain data related to years of rotation,
!!    cuttings per year, and HRUs


!!    arrays which contain data related to tillages in the database
      allocate (deptil(mtil))
      allocate (effmix(mtil))
!! drainmod tile equations   06/2006
	  allocate (ranrns(mtil))
	  allocate (ranrns_hru(mhru))
!! drainmod tile equations   06/2006

!!    arrays which contain data related to hydrograph nodes
      allocate (hyd_dakm(mhyd))
      allocate (icodes(mhyd))
      allocate (ihouts(mhyd))
      allocate (inum1s(mhyd))
      allocate (inum2s(mhyd))
      allocate (inum3s(mhyd))
      allocate (inum4s(mhyd))
      allocate (inum5s(mhyd))
      allocate (inum6s(mhyd))
      allocate (inum7s(mhyd))
      allocate (inum8s(mhyd))
      allocate (reccnstps(mhyd))
      allocate (recmonps(mhyd))
      allocate (rnum1s(mhyd))
      allocate (subed(mhyd))
      allocate (subnum(mhru))
      allocate (hruno(mhru))

      allocate (shyd(8,mhyd))
      allocate (varoute(mvaro,mhyd))
      allocate (vartran(mvaro,mhyd))
      allocate (hhvaroute(mvaro,mhyd,nstep))  !! from 24 to nstep for urban
  !!    allocate (hhvaroute(mvaro,mhyd,24))  !! from 24 to nstep for urban

!!    arrays which contain data related to HRUs
      allocate (aairr(mhru))
      allocate (afrt_surface(mhru))
      allocate (aird(mhru))
      allocate (alpha_bf(mhru))
      allocate (alpha_bf_d(mhru))
      allocate (alpha_bfe(mhru))
      allocate (alpha_bfe_d(mhru))
      allocate (anano3(mhru))
      allocate (anion_excl(mhru))
      allocate (auto_eff(mhru))
      allocate (auto_nyr(mhru))
      allocate (auto_napp(mhru))
      allocate (auto_nstrs(mhru))
      allocate (bactlp_plt(mhru))
      allocate (bactlpq(mhru))
      allocate (bactlps(mhru))
      allocate (bactp_plt(mhru))
      allocate (bactpq(mhru))
      allocate (bactps(mhru))
      allocate (bio_aams(mhru))
      allocate (bio_min(mhru))
      allocate (bio_ms(mhru))
      allocate (bio_yrms(mhru))
      allocate (biomix(mhru))
      allocate (bp1(mhru))
      allocate (bp2(mhru))
      allocate (brt(mhru))
      allocate (bw1(mhru))
      allocate (bw2(mhru))
      allocate (canmx(mhru))
      allocate (canstor(mhru))
      allocate (cbodu(mhru))
      allocate (ch_l1(mhru))
      allocate (chl_a(mhru))
      allocate (chlap(mhru))
      allocate (chlaw(mhru))
      allocate (cht(mhru))
      allocate (cklsp(mhru))
      allocate (cn1(mhru))
      allocate (cn2(mhru))
      allocate (cn3(mhru))
      allocate (cnday(mhru))
!    Drainmod tile equations  01/2006 
      allocate (cont_cn(21,mhru))
	  allocate (cumei(mhru))
	  allocate (cumeira(mhru))
	  allocate (cumrt(mhru))
	  allocate (cumrai(mhru))
!    Drainmod tile equations  01/2006
      allocate (cont_p(21,mhru))
      allocate (cropno_upd(21,mhru))
      allocate (curyr_mat(mhru))
      allocate (dayl(mhru))
      allocate (det_san(mhru))
      allocate (det_sil(mhru))
      allocate (det_cla(mhru))
      allocate (det_sag(mhru))
      allocate (det_lag(mhru))
!    Drainmod tile equations  01/2006 
	  allocate (drain_co(mhru))
	  allocate (ddrain_hru(mhru))
!    Drainmod tile equations  01/2006
      allocate (ddrain(mhru))
      allocate (deepirr(mhru))
      allocate (deepst(mhru))
      allocate (delay(mhru))
      allocate (dep_imp(mhru))
      allocate (dis_stream(mhru))
      allocate (divmax(mhru))
      allocate (dormhr(mhru))
      allocate (doxq(mhru))
      allocate (drain_d(21,mhru))
      allocate (drain_idep(21,mhru))
      allocate (drain_t(21,mhru))
      allocate (drain_g(21,mhru))
      allocate (driftco(mhru))
      allocate (dr_sub(mhru))
      allocate (epco(mhru))
      allocate (esco(mhru))
      allocate (erorgn(mhru))
      allocate (erorgp(mhru))
      allocate (evpot(mhru))
      allocate (evpnd(mhru))
      allocate (evwet(mhru))
      allocate (ffc(mhru))
      allocate (filterw(mhru))
      allocate (filt_w(21,mhru))
      allocate (fire_cn(21,mhru))
      allocate (fld_fr(mhru))
      allocate (flowfr(mhru))
      allocate (flowmin(mhru))
      allocate (fsred(mhru))
      allocate (gdrain(mhru))
      allocate (grwat_n(mhru))
      allocate (grwat_i(mhru))
      allocate (grwat_l(mhru))
      allocate (grwat_w(mhru))
      allocate (grwat_d(mhru))
      allocate (grwat_s(mhru))
      allocate (grwat_spcon(mhru))
      allocate (gwati(21,mhru))
      allocate (gwatn(21,mhru))
      allocate (gwatl(21,mhru))
      allocate (gwatw(21,mhru))
      allocate (gwatd(21,mhru))
      allocate (gwatveg(21,mhru))
      allocate (gwats(21,mhru))
      allocate (gwatspcon(21,mhru))
      allocate (gwata(21,mhru))
      allocate (gw_delaye(mhru))
      allocate (gw_nloss(mhru))
      allocate (gw_q(mhru))
      allocate (gw_qdeep(mhru))
      allocate (gw_revap(mhru))
      allocate (gw_spyld(mhru))
      allocate (gwht(mhru))
      allocate (gwminp(mhru))
      allocate (gwno3(mhru))
      allocate (gwqmn(mhru))
      allocate (hi_upd(21,mhru))
      allocate (hru_dafr(mhru))
      allocate (hru_fr(mhru))
      allocate (hru_ha(mhru))
      allocate (hru_km(mhru))
      allocate (hru_ra(mhru))
      allocate (hru_rmx(mhru))
      allocate (hru_slp(mhru))
      allocate (hru_sub(mhru))
      allocate (hru_seq(mhru))
      allocate (hrugis(mhru))
      allocate (hrupest(mhru))
      allocate (hvstiadj(mhru))
      allocate (iafrttyp(mhru))
      allocate (icont(20,mhru))
      allocate (icfrt(mhru))
      allocate (icpst(mhru))
      allocate (icr(mhru))
      allocate (icrmx(mhru))
      allocate (iday_fert(mhru))
      allocate (iday_pest(mhru))
      allocate (idorm(mhru))
      allocate (ifilt(20,mhru))
      allocate (ifld(mhru))
      allocate (iflod1(mhru))
      allocate (iflod2(mhru))
      allocate (igro(mhru))
      allocate (igrz(mhru))
      allocate (iopday(iopera,mhru))
      allocate (iopyr(iopera,mhru))
      allocate (mgt_ops(iopera,mhru))
      allocate (ioper(mhru))
!      allocate (mcri(mhru))
      allocate (mgt_sdr(iopera,mhru))
      allocate (mgtop(iopera,mhru))
      allocate (idop(iopera,mhru))
      allocate (phu_op(iopera,mhru))
      allocate (mgt1iop(iopera,mhru))
      allocate (mgt2iop(iopera,mhru))
      allocate (mgt3iop(iopera,mhru))
      allocate (mgt4op(iopera,mhru))
      allocate (mgt5op(iopera,mhru))
      allocate (mgt6op(iopera,mhru)) 
      allocate (mgt7op(iopera,mhru))
      allocate (mgt8op(iopera,mhru))
      allocate (mgt9op(iopera,mhru))
      allocate (mgt10iop(iopera,mhru))
      allocate (nopmx(mhru))
      allocate (irramt(mhru))
      allocate (yr_skip(mhru))
      allocate (isweep(mhru))
      allocate (phusw(mhru))
      allocate (phusw_nocrop(mhru))
      allocate (bio_targ(mhru))
      allocate (irr_flag(mhru))
      allocate (irra_flag(mhru))
      imho = max(mhru,20)
      allocate (ipdhru(imho))
      allocate (ipnd1(mhru))
      allocate (ipnd2(mhru))
!!      allocate (ipot(mhru))
      allocate (irip(mhru))
      allocate (irn(mhru))
      allocate (irrno(mhru))
      allocate (irrsc(mhru))
      allocate (istrip(20,mhru))
      allocate (itdrain(20,mhru))
      allocate (iterr(20,mhru))
      allocate (iurban(mhru))
      allocate (iwatable(mhru))
      allocate (iycont(20,mhru))
      allocate (iyfilt(20,mhru))
      allocate (iydrain(20,mhru))
      allocate (iystrip(20,mhru))
      allocate (iyterr(20,mhru))
      allocate (lai_aamx(mhru))
      allocate (lai_yrmx(mhru))
      allocate (laiday(mhru))
      allocate (laimxfr(mhru))
      allocate (latksatf(mhru))
      allocate (laimx_upd(21,mhru))
      allocate (lat_sed(mhru))
      allocate (lat_ttime(mhru))
      allocate (latno3(mhru))
      allocate (latq(mhru))
      allocate (ldrain(mhru))
      allocate (minpgw(mhru))
      allocate (nafert(mhru))
      allocate (nair(mhru))
      allocate (ncf(mhru))
      allocate (ncpest(mhru))
      allocate (ncut(mhru))
      allocate (ndeat(mhru))
      allocate (ndcfrt(mhru))
      allocate (ndcpst(mhru))
      allocate (ndtarg(mhru))
      allocate (newrti(mhru))
      allocate (nfert(mhru))
      allocate (ngr(mhru))
      allocate (ngrwat(mhru))
      allocate (nirr(mhru))
      allocate (nmgt(mhru))
      allocate (nop(mhru))
      allocate (no3gw(mhru))
      allocate (npcp(mhru))
      allocate (nplnt(mhru))
      allocate (nrelease(mhru))
      allocate (nro(mhru))
      allocate (nrot(mhru))
      allocate (nsweep(mhru))
      allocate (ntil(mhru))
      allocate (olai(mhru))
      allocate (orgn_con(mhru))
      allocate (orgp_con(mhru))
      allocate (orig_alai(mhru))
      allocate (orig_bioms(mhru))
      allocate (orig_deepst(mhru))
      allocate (orig_igro(mhru))
      allocate (orig_phuacc(mhru))
      allocate (orig_pndno3(mhru))
      allocate (orig_pndorgn(mhru))
      allocate (orig_pndorgp(mhru))
      allocate (orig_pndsed(mhru))
      allocate (orig_pndsolp(mhru))
      allocate (orig_pndvol(mhru))
      allocate (orig_potno3(mhru))
      allocate (orig_potsed(mhru))
      allocate (orig_potvol(mhru))
      allocate (orig_shallst(mhru))
      allocate (orig_snohru(mhru))
      allocate (orig_solcov(mhru))
      allocate (orig_solsw(mhru))
      allocate (orig_sumix(mhru))
      allocate (orig_wetno3(mhru))
      allocate (orig_wetorgn(mhru))
      allocate (orig_wetorgp(mhru))
      allocate (orig_wetsed(mhru))
      allocate (orig_wetsolp(mhru))
      allocate (orig_wetvol(mhru))
      allocate (ov_n(mhru))
      allocate (ovrlnd(mhru))
!    Drainmod tile equations  01/2006 
	  allocate (pc(mhru))
!    Drainmod tile equations  01/2006 
      allocate (percn(mhru))
      allocate (phuacc(mhru))
      allocate (phubase(mhru))
      allocate (plantn(mhru))
      allocate (plantp(mhru))
      allocate (plt_et(mhru))
      allocate (plt_pet(mhru))
      allocate (pltfr_n(mhru))
      allocate (pltfr_p(mhru))
      allocate (pnd_chla(mhru))
      allocate (pnd_esa(mhru))
      allocate (pnd_evol(mhru))
      allocate (pnd_fr(mhru))
      allocate (pnd_k(mhru))
      allocate (pnd_no3(mhru))
      allocate (pnd_no3g(mhru))
      allocate (pnd_no3s(mhru))
      allocate (pnd_nsed(mhru))
      allocate (pnd_orgn(mhru))
      allocate (pnd_orgp(mhru))
      allocate (pnd_psa(mhru))
      allocate (pnd_psed(mhru))
      allocate (pnd_pvol(mhru))
      allocate (pnd_seci(mhru))
      allocate (pnd_sed(mhru))

      allocate (pnd_san(mhru))
      allocate (pnd_sil(mhru))
      allocate (pnd_cla(mhru))
      allocate (pnd_sag(mhru))
      allocate (pnd_lag(mhru))
      
      allocate (twlpnd(mhru))     !!srini pond/wet infiltration to shallow gw storage
      allocate (twlwet(mhru))     !!srini pond/wet infiltration to shallow gw storage

      allocate (pnd_solp(mhru))
      allocate (pnd_solpg(mhru))
      allocate (pnd_vol(mhru))
      allocate (pot_fr(mhru))
      allocate (pot_no3(mhru))
      allocate (pot_no3l(mhru))
      allocate (pot_k(mhru))
      allocate (pot_solpl(mhru))
      allocate (pot_nsed(mhru))
      allocate (pot_sed(mhru))
      allocate (pot_san(mhru))
      allocate (pot_sil(mhru))
      allocate (pot_cla(mhru))
      allocate (pot_sag(mhru))
      allocate (pot_lag(mhru))
      allocate (n_reduc(mhru))
      allocate (n_lag(mhru))
      allocate (n_ln(mhru))
      allocate (n_lnco(mhru))

      allocate (pot_tile(mhru))
      allocate (pot_vol(mhru))
      allocate (pot_volx(mhru))
      allocate (pot_tilemm(mhru))     !!NUBZ
      allocate (pot_volmm(mhru))
      allocate (pot_volxmm(mhru))
      allocate (potflwi(mhru))
      allocate (potsa(mhru))
      allocate (potsedi(mhru))
      
      allocate (potsani(mhru))
      allocate (potsili(mhru))
      allocate (potclai(mhru))
      allocate (potsagi(mhru))
      allocate (potlagi(mhru))

      allocate (pplnt(mhru))
      allocate (prf(mch))  !Moriasi 4/8/14  
      allocate (spcon(mch))
      allocate (spexp(mch))
      allocate (qdr(mhru))
      allocate (qdayout(mhru))
      allocate (rch_dakm(mxsubch))  
      allocate (rchrg(mhru))
      allocate (rchrg_n(mhru))    !! amount of nitrate getting to the shallow aquifer
      allocate (rchrg_dp(mhru))
      allocate (re(mhru))
      allocate (revapmn(mhru))
      allocate (rhd(mhru))
      allocate (rip_fr(mhru))
      allocate (rnd2(mhru))
      allocate (rnd3(mhru))
      allocate (rnd8(mhru))
      allocate (rnd9(mhru))
      allocate (rsdin(mhru))
      allocate (rwt(mhru))
      allocate (sci(mhru))
!    Drainmod tile equations  01/2006 
	  allocate (sdrain(mhru))
	  allocate (sstmaxd(mhru))	  
!    Drainmod tile equations  01/2006 
      allocate (seccip(mhru))
      allocate (secciw(mhru))
      allocate (sed_stl(mhru))
      allocate (sedminpa(mhru))
      allocate (sedminps(mhru))
      allocate (sedorgn(mhru))
      allocate (sedorgp(mhru))
      allocate (sedyld(mhru))

      allocate (sanyld(mhru))
      allocate (silyld(mhru))
      allocate (clayld(mhru))
      allocate (sagyld(mhru))
      allocate (lagyld(mhru))
      allocate (grayld(mhru))
      allocate (sed_con(mhru))
      allocate (sepbtm(mhru))
      allocate (shallirr(mhru))
      allocate (rchrg_src(mhru))
      allocate (shallst(mhru))
      allocate (shallst_n(mhru))
      allocate (slsoil(mhru))
      allocate (slsubbsn(mhru))
      allocate (smx(mhru))
      allocate (sno_hru(mhru))
      allocate (snotmp(mhru))
      allocate (soln_con(mhru))
      allocate (solp_con(mhru))
      allocate (sol_alb(mhru))
      allocate (sol_avbd(mhru))
      allocate (sol_avpor(mhru))
      allocate (sol_cnsw(mhru))
      allocate (sol_cov(mhru))
      allocate (sol_crk(mhru))
      allocate (sol_nly(mhru))
      allocate (sol_sumfc(mhru))
      allocate (sol_sumul(mhru))
      allocate (sol_sumwp(mhru))
      allocate (sol_sw(mhru))
      allocate (sol_zmx(mhru))
      allocate (strip_n(21,mhru))
!!    Drainmod tile equations  01/2006 
	allocate (strip_cn(21,mhru))
	allocate (strip_c(21,mhru))
	allocate (strip_p(21,mhru))
	allocate (stmaxd(mhru))
      allocate (itill(mhru))
      allocate (strsa(mhru))
      allocate (strsn(mhru))
      allocate (strsp(mhru))
      allocate (strstmp(mhru))
      allocate (strsw(mhru))
      allocate (stsol_rd(mhru))
      allocate (subp(mhru))
      allocate (sumix(mhru))
      allocate (surfq(mhru))
      allocate (surqno3(mhru))
      allocate (surqsolp(mhru))
      allocate (swtrg(mhru))
      allocate (t_ov(mhru))
      allocate (tauton(mhru))
      allocate (tautop(mhru))
      allocate (tcfrtn(mhru))
      allocate (tcfrtp(mhru))
      allocate (tconc(mhru))
      allocate (tdrain(mhru))
      allocate (tc_gwat(mhru))
      allocate (terr_cn(21,mhru))
      allocate (terr_p(21,mhru))
      allocate (terr_sl(21,mhru))
      allocate (tfertn(mhru))
      allocate (tfertp(mhru))
      allocate (tgrazn(mhru))
      allocate (tgrazp(mhru))
      allocate (tile_ttime(mhru))
      allocate (tileq(mhru))
      allocate (tileno3(mhru))
      allocate (tmn(mhru))
      allocate (tmpav(itempa))
      allocate (tmp_hi(mhru))
      allocate (tmp_lo(mhru))
      allocate (tmpavp(mhru))
      allocate (tmx(mhru))
      allocate (trapeff(mhru))
      allocate (twash(mhru))
      allocate (u10(mhru))
      allocate (urblu(mhru))
      allocate (usle_cfac(mhru))
      allocate (usle_eifac(mhru))
      allocate (usle_k(mhru))
      allocate (usle_mult(mhru))
      allocate (usle_ls(mhru))
      allocate (usle_p(mhru))
      allocate (velsetlp(mhru))
      allocate (wtab(mhru))
      allocate (wtab_mn(mhru))
      allocate (wtab_mx(mhru))
      allocate (wet_chla(mhru))
      allocate (wet_fr(mhru))
      allocate (iwetgw(mhru))
      allocate (iwetile(mhru))
      allocate (wet_k(mhru))
      allocate (wet_mxsa(mhru))
      allocate (wet_mxvol(mhru))
      allocate (wet_no3(mhru))
      allocate (wet_no3g(mhru))
      allocate (wet_no3s(mhru))
      allocate (wet_nsa(mhru))
      allocate (wet_nsed(mhru))
      allocate (wet_nvol(mhru))
      allocate (wet_orgn(mhru))
      allocate (wet_orgp(mhru))
      allocate (wet_psed(mhru))
      allocate (wet_seci(mhru))
      allocate (wet_sed(mhru))
      allocate (wet_solp(mhru))
      allocate (wet_solpg(mhru))
      allocate (wet_vol(mhru))
      allocate (wfsh(mhru))
      allocate (yldaa(mhru))
      allocate (yldanu(mhru))

      allocate (wet_san(mhru))
      allocate (wet_sil(mhru))
      allocate (wet_cla(mhru))
      allocate (wet_sag(mhru))
      allocate (wet_lag(mhru))

      allocate (frad(mhru,nstep))
!      allocate (hhsubp(mhru,24))

 !     allocate (rhrbsb(24))
      allocate (rstpbsb(nstep))
      allocate (rainsub(mhru,nstep))
      allocate (precipdt(nstep+1))

      allocate (bss(4,mhru))
      allocate (nsetlp(2,mhru))
      allocate (nsetlw(2,mhru))
      allocate (psetlp(2,mhru))
      allocate (psetlw(2,mhru))
      allocate (wrt(2,mhru))
      allocate (wgncur(3,mhru))
      allocate (wgnold(3,mhru))
      allocate (surf_bs(17,mhru))
      allocate (rndseed(10,mhru))
      allocate (pcpband(10,mhru))
      allocate (snoeb(10,mhru))
      allocate (orig_snoeb(10,mhru))
      allocate (snotmpeb(10,mhru))
      allocate (tavband(10,mhru))
      allocate (tmnband(10,mhru))
      allocate (tmxband(10,mhru))
      allocate (wudeep(12,mhru))
      allocate (wupnd(12,mhru))
      allocate (wushal(12,mhru))
!     allocate (phi(13,msub+1))
      allocate (phi(13,mch))  
      allocate (wat_phi(13,mhru))
      allocate (rfqeo_30d(30,mhru))
      allocate (eo_30d(30,mhru))


!!    arrays which contain data related to pesticides, HRUs
      allocate (orig_pltpst(mpst,mhru))
      allocate (plt_pst(mpst,mhru))
      allocate (pst_enr(mpst,mhru))
      allocate (pst_sed(mpst,mhru))
      allocate (pst_surq(mpst,mhru))
      allocate (zdb(mpst,mhru))

      allocate (pst_lag(mpst,3,mhru))


!!    arrays which contain data related to HRU output 
      allocate (hrupsta(mpst,4,mhru))
      allocate (hrupstd(mpst,4,mhru))
      allocate (hrupstm(mpst,4,mhru))
      allocate (hrupsty(mpst,4,mhru))
      allocate (icols(mhruo))
      allocate (ipdvas(mhruo))
      allocate (hrumono(74,mhru))
      allocate (hruyro(74,mhru))
      allocate (hruaao(74,mhru))
      allocate (wtrmon(40,mhru))
      allocate (wtryr(40,mhru))
      allocate (wtraa(40,mhru))

!!    arrays which contain data related to pesticides
      allocate (lat_pst(mpst))
      allocate (npno(mpst))
      allocate (pstsol(mpst))
      allocate (wshd_pstap(mpst))
      allocate (wshd_pstdg(mpst))

!!    arrays which contain data related to years
      allocate (flocnst(mrecc))
      allocate (sedcnst(mrecc))
      allocate (orgncnst(mrecc))
      allocate (orgpcnst(mrecc))
      allocate (no3cnst(mrecc))
      allocate (minpcnst(mrecc))
      allocate (nh3cnst(mrecc))
      allocate (no2cnst(mrecc))
      allocate (bactpcnst(mrecc))
      allocate (bactlpcnst(mrecc))
      allocate (cmtl1cnst(mrecc))
      allocate (cmtl2cnst(mrecc))
      allocate (cmtl3cnst(mrecc))
      allocate (chlacnst(mrecc))
      allocate (disoxcnst(mrecc))
      allocate (cbodcnst(mrecc))
      allocate (solpstcnst(mrecc))
      allocate (srbpstcnst(mrecc))

      allocate (floyr(mrecy,myr))
      allocate (sedyr(mrecy,myr))
      allocate (orgnyr(mrecy,myr))
      allocate (orgpyr(mrecy,myr))
      allocate (no3yr(mrecy,myr))
      allocate (minpyr(mrecy,myr))
      allocate (nh3yr(mrecy,myr))
      allocate (no2yr(mrecy,myr))
      allocate (bactpyr(mrecy,myr))
      allocate (bactlpyr(mrecy,myr))
      allocate (cmtl1yr(mrecy,myr))
      allocate (cmtl2yr(mrecy,myr))
      allocate (cmtl3yr(mrecy,myr))
      allocate (chlayr(mrecy,myr))
      allocate (disoxyr(mrecy,myr))
      allocate (cbodyr(mrecy,myr))
      allocate (solpstyr(mrecy,myr))
      allocate (srbpstyr(mrecy,myr))

      allocate (flomon(mrecm,myr,12))
      allocate (sedmon(mrecm,myr,12))
      allocate (orgnmon(mrecm,myr,12))
      allocate (orgpmon(mrecm,myr,12))
      allocate (no3mon(mrecm,myr,12))
      allocate (minpmon(mrecm,myr,12))
      allocate (nh3mon(mrecm,myr,12))
      allocate (no2mon(mrecm,myr,12))
      allocate (bactpmon(mrecm,myr,12))
      allocate (bactlpmon(mrecm,myr,12))
      allocate (cmtl1mon(mrecm,myr,12))
      allocate (cmtl2mon(mrecm,myr,12))
      allocate (cmtl3mon(mrecm,myr,12))
      allocate (chlamon(mrecm,myr,12))
      allocate (disoxmon(mrecm,myr,12))
      allocate (cbodmon(mrecm,myr,12))
      allocate (solpstmon(mrecm,myr,12))
      allocate (srbpstmon(mrecm,myr,12))

!!    arrays
      allocate (ndays(13))
      allocate (ndays_leap(13))
      allocate (ndays_noleap(13))
      allocate (idg(9))
      allocate (ndmo(12))
   !   allocate (halgae(24))
   !   allocate (hbactlp(24))
   !   allocate (hbactp(24))
   !   allocate (hbod(24))
   !   allocate (hchla(24))
   !   allocate (hdepth(24))
   !   allocate (hdisox(24))
   !   allocate (hharea(24))
   !  allocate (hhqday(24))
   !   allocate (hhstor(24))
   !   allocate (hhtime(24))
   !   allocate (hnh4(24))
   !   allocate (hno2(24))
   !   allocate (hno3(24))
   !   allocate (horgn(24))
   !   allocate (horgp(24))
   !   allocate (hrchwtr(24))
   !   allocate (hrtwtr(24))
   !   allocate (hsdti(24))
   !   allocate (hsedst(24))
   !   allocate (hsedyld(24))
   !   allocate (hsolp(24))
   !   allocate (hsolpst(24))
   !   allocate (hsorpst(24))
   !   allocate (hhprecip(24))
      allocate (halgae(nstep))
      allocate (hbactlp(nstep))
      allocate (hbactp(nstep))
      allocate (hbod(nstep))
      allocate (hchla(nstep))
      allocate (hdepth(nstep))      ! changed as per nstep  !nstep Mar 19,2008
      allocate (hdisox(nstep))
      allocate (hharea(nstep))	  ! changed as per nstep  !nstep Mar 19,2008
      allocate (hhqday(nstep))  ! changed as  nstep  Oct. 18, 2007
      allocate (hhstor(nstep))  ! changed as per nstep   !nstep Mar 19,2008
      allocate (hhtime(nstep))  ! changed as per nstep   !nstep Mar 19,2008
      allocate (hnh4(nstep))
      allocate (hno2(nstep))
      allocate (hno3(nstep))
      allocate (horgn(nstep))
      allocate (horgp(nstep))
      allocate (hrchwtr(nstep))  ! changed as per nstep  !nstep Mar 19,2008
      allocate (hrtwtr(nstep))   ! changed as per nstep  !nstep Mar 19,2008
      allocate (hsdti(nstep))	   ! changed as per nstep  !nstep Mar 19,2008
      allocate (hsedst(nstep))
      allocate (hsedyld(nstep))
      allocate (hsolp(nstep))
      allocate (hsolpst(nstep))
      allocate (hsorpst(nstep))
      allocate (wshdaao(mstdo))
      allocate (wshddayo(mstdo))
      allocate (wshdmono(mstdo))
      allocate (wshdyro(mstdo))
      allocate (fcstaao(16))

      allocate (wpstaao(mpst,5))
      allocate (wpstmono(mpst,5))
      allocate (wpstyro(mpst,5))
      allocate (wpstdayo(mpst,5))

      allocate (wshd_aamon(12,8))

!!arrays that store initial values
      allocate (ivar_orig(10))
      allocate (rvar_orig(10))
      allocate (wattemp(mch))    
!! sj, june 07 modifications to carbon balance routines
      allocate (sol_n(mlyr,mhru))
!! sj, june 07 end

!! sj dec 07 modification tillage
      allocate (sol_bdp(mlyr,mhru))
!! sj dec 07 end

!!Armen Jan 2008
      allocate (tillagef(mlyr,mhru))
!  test rtfr
      allocate (rtfr(mlyr))

!!    added for manure Armen Jan 2009
      allocate (sol_mc(mlyr,mhru))
      allocate (sol_mn(mlyr,mhru))
      allocate (sol_mp(mlyr,mhru))



!!Armen Jan 2008 end
!! sj aug 09 SWAT-C MC stuff
	allocate (cf(mhru))
	allocate (cfh(mhru))
	allocate (cfdec(mhru))
!! sj aug 09 end
	allocate (hhsurf_bs(2,mhru,nstep))  !! nstep changed to nstep  OCt. 18,2007
      allocate (ubnrunoff(nstep),ubntss(nstep))
      allocate (sub_ubnrunoff(msub,nstep),sub_ubntss(msub,nstep))

!! Arrays for subdaily erosion modeling by Jaehak Jeong
	allocate (hhsedy(mhru,nstep),rhy(nstep),ovrlnd_dt(mhru,nstep))
      allocate (snam(mhru),hydgrp(mhru),kirr(mhru))
	allocate (dratio(msub),init_abstrc(mhru))
	allocate (sub_subp_dt(msub,nstep),sub_hhsedy(msub,nstep))
	allocate (sub_atmp(msub,nstep),bmp_recharge(msub))
	allocate (rchhr(mrcho,mch,nstep),hrtevp(nstep),hrttlc(nstep))
	allocate (hhresflwi(nstep), hhresflwo(nstep),hhressedi(nstep),
     &	 hhressedo(nstep))
!! Arrays for bmp simulation by jaehak jeong
	allocate (lu_nodrain(30),bmpdrain(mhru))
	allocate (subdr_km(mhyd),subdr_ickm(mhyd),sub_cn2(msub))
      ! sedimentation-filtration
      allocate (num_sf(msub),sf_fr(msub,10),sf_dim(msub,10),         
     &  sf_typ(msub,10),sf_im(msub,10),sf_iy(msub,10),sp_sa(msub,10),  
     &  sp_pvol(msub,10),sp_pd(msub,10),sp_sedi(msub,10),
     &  sp_sede(msub,10),ft_sa(msub,10),ft_fsa(msub,10),
     &  ft_dep(msub,10),ft_h(msub,10),ft_pd(msub,10),
     &  ft_k(msub,10),ft_dp(msub,10),ft_dc(msub,10),
     &  ft_por(msub,10),tss_den(msub,10),ft_alp(msub,10),
     &  sp_qi(msub,10),sp_k(msub,10),sp_bpw(msub,10),
     &  ft_bpw(msub,10),sp_dp(msub,10),ft_sed_cumul(msub,10),
     &  sp_sed_cumul(msub,10),ft_qfg(msub,10),sp_qfg(msub,10))
      allocate (sub_ha_imp(msub),ft_qpnd(msub,10),ft_qsw(msub,10), 
     &  ft_qin(msub,10),ft_qout(msub,10),ft_sedpnd(msub,10),
     &  sf_ptp(msub,10),ft_fc(msub,10),sub_ha_urb(msub)) 
!! additional var by Ann
!! Filter Strip variable allocation MJW
      allocate (vfscon(mhru))
      allocate (vfsratio(mhru))
      allocate (vfsch(mhru))
      allocate (vfsi(mhru))
      allocate (filter_i(21,mhru))
      allocate (filter_ratio(21,mhru))
      allocate (filter_con(21,mhru))
      allocate (filter_ch(21,mhru))  
      
      ! detention pond
 	allocate(dtp_subnum(mhyd),dtp_imo(mhyd),dtp_iyr(mhyd),
     &  dtp_numweir(mhyd),dtp_numstage(mhyd),
     &  dtp_stagdis(mhyd),dtp_reltype(mhyd),dtp_onoff(mhyd))
	
	allocate(dtp_evrsv(msub),
     &  dtp_inflvol(msub),dtp_totwrwid(msub),dtp_lwratio(msub),
     &  dtp_wdep(msub),dtp_totdep(msub),dtp_watdepact(msub),
     &  dtp_outflow(msub),dtp_totrel(msub),dtp_backoff(msub),
     &  dtp_seep_sa(msub),dtp_evap_sa(msub),dtp_pet_day(msub),
     &  dtp_pcpvol(msub),dtp_seepvol(msub),dtp_evapvol(msub),
     &  dtp_flowin(msub),dtp_backup_length(msub),dtp_intcept(msub),
     &  dtp_expont(msub),dtp_coef1(msub),dtp_coef2(msub),
     &  dtp_coef3(msub),dtp_dummy1(msub),dtp_dummy2(msub),
     &  dtp_dummy3(msub),dtp_ivol(msub),dtp_ised(msub))
      
  	allocate(dtp_wdratio(msub,10),dtp_depweir(msub,10),
     &  dtp_diaweir(msub,10),dtp_retperd(msub,10),dtp_pcpret(msub,10),
     &  dtp_cdis(msub,10),dtp_flowrate(msub,10),
     &  dtp_wrwid(msub,10),dtp_weirtype(msub,10),dtp_weirdim(msub,10),
     &  dtp_addon(msub,10)) 
      !! additional var by jeong for nutrient speciation
      allocate (lat_orgn(mhru))
      allocate (lat_orgp(mhru))

!! Variables for soil P and additional operations mjw
	allocate (sol_watp(mlyr,mhru))
	allocate (a_days(mlyr,mhru))
	allocate (b_days(mlyr,mhru))
	allocate (psp_store(mlyr,mhru))
	allocate (ssp_store(mlyr,mhru))
	allocate (sol_cal(mlyr,mhru))
        allocate (sol_ph(mlyr,mhru))
	allocate (harv_min(mhru))
	allocate (fstap(mfdb)) 
	allocate (min_res(mhru))
	allocate (so_res(20,mhru))
	allocate (so_res_flag(20,mhru))
	allocate (ro_bmp_flag(21,mhru))
        allocate (ro_bmp_flo(21,mhru))
        allocate (ro_bmp_sed(21,mhru))
        allocate (ro_bmp_pp(21,mhru))
	allocate (ro_bmp_sp(21,mhru))
	allocate (ro_bmp_pn(21,mhru))
	allocate (ro_bmp_sn(21,mhru))
	allocate (ro_bmp_bac(21,mhru))

        allocate (ro_bmp_flos(21,mhru))
        allocate (ro_bmp_seds(21,mhru))
        allocate (ro_bmp_pps(21,mhru))
	allocate (ro_bmp_sps(21,mhru))
	allocate (ro_bmp_pns(21,mhru))
	allocate (ro_bmp_sns(21,mhru))
	allocate (ro_bmp_bacs(21,mhru))

        allocate (ro_bmp_flot(21,mhru))
        allocate (ro_bmp_sedt(21,mhru))
        allocate (ro_bmp_ppt(21,mhru))
	allocate (ro_bmp_spt(21,mhru))
	allocate (ro_bmp_pnt(21,mhru))
	allocate (ro_bmp_snt(21,mhru))
	allocate (ro_bmp_bact(21,mhru))

	allocate (bmp_flag(mhru))
      
      allocate (bmp_flo(mhru))
	allocate (bmp_sed(mhru))
	allocate (bmp_pp(mhru)) 	
	allocate (bmp_sp(mhru))	
	allocate (bmp_pn(mhru)) 	
	allocate (bmp_sn(mhru))	
	allocate (bmp_bac(mhru))
	
	!! **salt**
      allocate (bmp_salt(mhru))
      allocate (sub_salt(10,msub))
      allocate (salt_flag(20,mhru))
      allocate (sro_salt(20,mhru,10))
      allocate (slt_salt(20,mhru,10))
      allocate (gw_salt(20,mhru,10))
      allocate (tile_salt(20,mhru,10))
      allocate (sub_saltmo(mch,12))
      allocate (saltdr(mch))
      !! **salt**
      
      allocate (bmp_flos(mhru))
	allocate (bmp_seds(mhru))
	allocate (bmp_pps(mhru)) 	
	allocate (bmp_sps(mhru))	
	allocate (bmp_pns(mhru)) 	
	allocate (bmp_sns(mhru))	
	allocate (bmp_bacs(mhru))
      
      allocate (bmp_flot(mhru))
	allocate (bmp_sedt(mhru))
	allocate (bmp_ppt(mhru)) 	
	allocate (bmp_spt(mhru))	
	allocate (bmp_pnt(mhru)) 	
	allocate (bmp_snt(mhru))	
	allocate (bmp_bact(mhru))
      
      !retention irrigation
      allocate(ri_sed(msub,10),ri_fr(msub,10),ri_dim(msub,10),
     &   ri_im(msub,10),ri_iy(msub,10),ri_sa(msub,10),ri_vol(msub,10), 
     &   ri_qi(msub,10),ri_k(msub,10),ri_dd(msub,10),ri_evrsv(msub,10),
     &   ri_dep(msub,10),ri_ndt(msub,10),ri_nirr(msub,30),
     &   num_noirr(msub),ri_totpvol(nstep),ri_luflg(mhru),
     &   ri_subkm(msub),ri_sed_cumul(msub,10),irmmdt(nstep),
     &   ri_pumpv(msub,10),ri_sedi(msub,10))   
      allocate(num_ri(msub), ri_pmpvol(10,nstep),hrnopcp(msub,0:nstep),
     &   ri_qloss(10,nstep))
      
      !wet pond
      allocate(wtp_subnum(mhyd),wtp_onoff(mhyd),wtp_imo(mhyd),
     &  wtp_iyr(mhyd),wtp_dim(mhyd),wtp_stagdis(mhyd),wtp_sdtype(mhyd),
     &  wtp_pvol(mhyd),wtp_pdepth(mhyd),wtp_sdslope(mhyd),
     &  wtp_lenwdth(mhyd),wtp_extdepth(mhyd),wtp_hydeff(mhyd),
     &  wtp_evrsv(mhyd),wtp_sdintc(mhyd),wtp_sdexp(mhyd),wtp_sdc1(mhyd),
     &  wtp_sdc2(mhyd),wtp_sdc3(mhyd),wtp_pdia(mhyd),wtp_plen(mhyd),
     &  wtp_pmann(mhyd),wtp_ploss(mhyd),wtp_k(mhyd),
     &  wtp_dp(mhyd),wtp_sedi(mhyd),wtp_sede(mhyd),wtp_qi(mhyd))

!!    LID simulations
!!    Common variable
!!    van Genuchten equation's coefficients
!      allocate(lid_vgcl,lid_vgcm,lid_qsurf_total,
!     & lid_farea_sum)
      allocate(lid_cuminf_last(mhru,4),lid_sw_last(mhru,4),
     & interval_last(mhru,4),lid_f_last(mhru,4),lid_cumr_last(mhru,4),
     & lid_str_last(mhru,4),lid_farea(mhru,4),lid_qsurf(mhru,4),
     & lid_sw_add(mhru,4),lid_cumqperc_last(mhru,4),
     & lid_cumirr_last(mhru,4),lid_excum_last(mhru,4),
     & lid_str_curday(mhru,4),lid_qsurf_curday(mhru,4))	!!  nbs

!!    Green Roof
      allocate(gr_onoff(msub,mudb),gr_imo(msub,mudb),gr_iyr(msub,mudb),
     & gr_farea(msub,mudb),gr_solop(msub,mudb),gr_etcoef(msub,mudb),
     & gr_fc(msub,mudb),gr_wp(msub,mudb),gr_ksat(msub,mudb),
     & gr_por(msub,mudb),gr_hydeff(msub,mudb),gr_soldpt(msub,mudb),
     & gr_dummy1(msub,mudb),gr_dummy2(msub,mudb),gr_dummy3(msub,mudb),
     & gr_dummy4(msub,mudb),gr_dummy5(msub,mudb),nlid(msub))

!!    Rain Garden
      allocate(rg_onoff(msub,mudb),rg_imo(msub,mudb),rg_iyr(msub,mudb),
     & rg_farea(msub,mudb),rg_solop(msub,mudb),rg_etcoef(msub,mudb),
     & rg_fc(msub,mudb),rg_wp(msub,mudb),rg_ksat(msub,mudb),
     & rg_por(msub,mudb),rg_hydeff(msub,mudb),rg_soldpt(msub,mudb),
     & rg_dimop(msub,mudb),rg_sarea(msub,mudb),rg_vol(msub,mudb),
     & rg_sth(msub,mudb),rg_sdia(msub,mudb),rg_bdia(msub,mudb),
     & rg_sts(msub,mudb),rg_orifice(msub,mudb),rg_oheight(msub,mudb),
     & rg_odia(msub,mudb),rg_dummy1(msub,mudb),rg_dummy2(msub,mudb),
     & rg_dummy3(msub,mudb),rg_dummy4(msub,mudb),rg_dummy5(msub,mudb))
      
!!    CiStern
      allocate(cs_onoff(msub,mudb),cs_imo(msub,mudb),cs_iyr(msub,mudb),
     & cs_grcon(msub,mudb),cs_farea(msub,mudb),cs_vol(msub,mudb),
     & cs_rdepth(msub,mudb),cs_dummy1(msub,mudb),cs_dummy2(msub,mudb),
     & cs_dummy3(msub,mudb),cs_dummy4(msub,mudb),cs_dummy5(msub,mudb))

!!    Poropus paVement
      allocate(pv_onoff(msub,mudb),pv_imo(msub,mudb),pv_iyr(msub,mudb),
     & pv_grvdep(msub,mudb),pv_grvpor(msub,mudb),pv_farea(msub,mudb),
     & pv_solop(msub,mudb),pv_drcoef(msub,mudb),pv_fc(msub,mudb),
     & pv_wp(msub,mudb),pv_ksat(msub,mudb),pv_por(msub,mudb),
     & pv_hydeff(msub,mudb),pv_soldpt(msub,mudb),pv_dummy1(msub,mudb),
     & pv_dummy2(msub,mudb),pv_dummy3(msub,mudb),pv_dummy4(msub,mudb),
     & pv_dummy5(msub,mudb))
      
!!    LID general
      allocate(lid_onoff(msub,mudb),lid_lunam(msub,mudb))
      
      !! By Zhang for C/N cycling
      !! ============================
	!allocate(sol_PH(mlyr,mhru))
	allocate(sol_CAC(mlyr,mhru)) 
	allocate(sol_CEC(mlyr,mhru)) 
	allocate(sol_BMC(mlyr,mhru)) 
	allocate(sol_BMN(mlyr,mhru)) 
	allocate(sol_HSC(mlyr,mhru)) 
	allocate(sol_HSN(mlyr,mhru)) 
	allocate(sol_HPC(mlyr,mhru)) 
	allocate(sol_HPN(mlyr,mhru)) 
	allocate(sol_LM(mlyr,mhru)) 
	allocate(sol_LMC(mlyr,mhru)) 
	allocate(sol_LMN(mlyr,mhru)) 
	allocate(sol_LS(mlyr,mhru)) 
	allocate(sol_LSC(mlyr,mhru)) 
	allocate(sol_LSN(mlyr,mhru)) 
      allocate(sol_LSL(mlyr,mhru)) 
      allocate(sol_RNMN(mlyr,mhru))
      allocate(sol_LSLC(mlyr,mhru))
      allocate(sol_LSLNC(mlyr,mhru))
      allocate(sol_RSPC(mlyr,mhru))      
      allocate(sol_WOC(mlyr,mhru))
      allocate(sol_WON(mlyr,mhru))
      allocate(sol_HP(mlyr,mhru))
      allocate(sol_HS(mlyr,mhru))
      allocate(sol_BM(mlyr,mhru))

	!daily update
	allocate(sol_percc(mlyr,mhru))
	allocate(sol_latc(mlyr,mhru))
	      
	!!for print out at daily, monthly, and annual scale
	allocate(sedc_d(mhru))
	allocate(surfqc_d(mhru))
	allocate(latc_d(mhru))
	allocate(percc_d(mhru))
	allocate(foc_d(mhru))
	allocate(NPPC_d(mhru))
	allocate(rsdc_d(mhru)) 
	allocate(grainc_d(mhru))
	allocate(stoverc_d(mhru))
	allocate(emitc_d(mhru))
	allocate(soc_d(mhru))
	allocate(rspc_d(mhru))

	allocate(sub_sedc_d(msub))
	allocate(sub_surfqc_d(msub))
	allocate(sub_latc_d(msub))
	allocate(sub_percc_d(msub))
	allocate(sub_foc_d(msub))
	allocate(sub_NPPC_d(msub))
	allocate(sub_rsdc_d(msub))
	allocate(sub_grainc_d(msub))
	allocate(sub_stoverc_d(msub))
	allocate(sub_emitc_d(msub))
	allocate(sub_soc_d(msub))
	allocate(sub_rspc_d(mhru))
	   
	allocate(sedc_m(mhru))
	allocate(surfqc_m(mhru))
	allocate(latc_m(mhru))
	allocate(percc_m(mhru))
	allocate(foc_m(mhru))
	allocate(NPPC_m(mhru))
	allocate(rsdc_m(mhru)) 
	allocate(grainc_m(mhru))
	allocate(stoverc_m(mhru))
	allocate(emitc_m(mhru))
	allocate(soc_m(mhru))
	allocate(rspc_m(mhru))	
	
	allocate(sedc_a(mhru))
	allocate(surfqc_a(mhru))
	allocate(latc_a(mhru))
	allocate(percc_a(mhru))
	allocate(foc_a(mhru))
	allocate(NPPC_a(mhru))
	allocate(rsdc_a(mhru)) 
	allocate(grainc_a(mhru))
	allocate(stoverc_a(mhru))
	allocate(emitc_a(mhru))
	allocate(soc_a(mhru))
	allocate(rspc_a(mhru))	

       !Tillage factor on SOM decomposition
       allocate(tillage_switch(mhru))
       allocate(tillage_depth(mhru))
       allocate(tillage_days(mhru))
       allocate(tillage_factor(mhru))
       tillage_switch = 0
       tillage_depth = 0.
       tillage_days = 0
       tillage_factor = 0.
      !! By Zhang for C/N cycling
      !! ============================
       
       !FLOOD ROUTING
       allocate(QHY(nstep+1,mhyd,4), NHY(4*msub))
       allocate(RCHX(msub),RCSS(msub),QCAP(msub),CHXA(msub),CHXP(msub))
      	  
      call zero0
      call zero1
      call zero2
      call zeroini
      call zero_urbn
   
      return
      end