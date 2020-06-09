      subroutine zero_urbn

!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    this subroutine zeros all array values used in urban modeling

      use parm

	urban_flag = 0
	sci = 0.

	hhsurf_bs = 0. 
      bf_flg = 0.
      ubnrunoff = 0.
      ubntss = 0.
      abstinit = 0.
      iabstr = 0.
      
!!	subdaily sediment modeling by J.Jeong
	hhsedy=0.
	eros_spl = 0.
	rill_mult = 0.
	eros_expo = 0.
	sig_g = 0.
	ch_d50 = 0.
	rhy = 0.
	dratio = 0.
	c_factor = 0.
	iuh = 1
	uhalpha = 1.
	rchhr = 0.
	init_abstrc = 0
	
!!    subdaily bmp modeling
      lu_nodrain = "    "
      bmpdrain = 0
      !sed-fil
      num_sf = 0
      sf_fr = 0.
      sf_dim = 0
      sf_typ = 0.
      sf_im = 0.
      sf_iy = 0.
      sp_sa = 0.
      sp_pvol = 0.
      sp_qi = 0.
      sp_pd = 0.
      sp_bpw = 0.
      sp_k = 0.
      sp_sedi = 0.
      sp_sede = 0.
      sp_dp = 0.
      ft_sa = 0.
      ft_fsa = 0.
      ft_dep = 0.
      ft_h = 0.
      ft_pd = 0.
      ft_bpw = 0.
      ft_k = 0.
      ft_dp = 0.
      ft_dc = 0.
      ft_por = 0.4	
      tss_den = 0.
      ft_alp = 0.
      sub_ha_imp = 0.
      sub_ha_urb = 0.
	ft_qpnd = 0.
	ft_qsw = 0.
	ft_qin = 0.
	ft_qout = 0.
	ft_sedpnd = 0.
	ft_sed_cumul = 0.
	sp_sed_cumul = 0.
	ft_qfg = 0
	sp_qfg = 0
	sf_ptp = 0
	ft_fc = 0
	!detention pond
	dtp_subnum = 0
	dtp_imo = 0
	dtp_iyr = 0
	dtp_numweir = 0
	dtp_numstage = 0
	dtp_stagdis = 0
	dtp_reltype = 0
	dtp_onoff = 0
	dtp_evrsv = 0.
	dtp_inflvol = 0.
	dtp_totwrwid = 0.
	dtp_lwratio = 0.
	dtp_wdep = 0.
	dtp_totdep = 0.
	dtp_watdepact = 0.
	dtp_outflow = 0.
	dtp_totrel = 0.
	dtp_backoff = 0.
	dtp_seep_sa = 0.
	dtp_evap_sa = 0.
	dtp_pet_day = 0.
	dtp_pcpvol = 0.
	dtp_seepvol = 0.
	dtp_evapvol = 0.
	dtp_flowin = 0.
	dtp_backup_length = 0.
	dtp_intcept = 0.
	dtp_expont = 0.
	dtp_coef1 = 0.
	dtp_coef2 = 0.
	dtp_coef3 = 0.
	dtp_dummy1 = 0.
	dtp_dummy2 = 0.
	dtp_dummy3 = 0.
	dtp_wdratio = 0.
	dtp_depweir = 0.
	dtp_diaweir = 0.
	dtp_retperd = 0.
	dtp_pcpret = 0.
	dtp_cdis = 1.
	dtp_wrwid = 0.
	dtp_weirtype = 0.
	dtp_weirdim = 0.
	dtp_addon = 0.
	dtp_ivol = 0.
	dtp_ised = 0.
	dtp_flowrate = 0.
      !retention-irrigation
      ri_luflg(:) = 0
      ri_sed = 0.
      ri_sed = 0.
      ri_fr = 0.
      ri_dim = 0.
      ri_im = 0.
      ri_iy = 0.
      ri_sa = 0.
      ri_vol = 0.
      ri_qi = 0.
      ri_sedi = 0.
      ri_k = 0.
      ri_dd = 0.
      ri_evrsv = 0.
      ri_nirr = "    "
      ri_dep = 0.
      ri_ndt = 0.
      ri_subkm = 0.
      num_ri = 0
      hrnopcp = 100.
      ri_sed_cumul = 0.
      irmmdt = 0.
      subdr_km = 0.
      subdr_ickm = 0.
      num_noirr = 0
      ri_qloss = 0
      ri_pumpv = 0
      
      !! wet pond
      wtp_subnum = 0
      wtp_onoff = 0
      wtp_imo = 0
      wtp_iyr = 0
      wtp_dim = 0
      wtp_stagdis = 0
      wtp_sdtype = 0
      wtp_pvol = 0.
      wtp_pdepth = 0.
      wtp_sdslope = 0.
      wtp_lenwdth = 0.
      wtp_extdepth = 0.
      wtp_hydeff = 0.
      wtp_evrsv = 0.
      wtp_sdintc = 0.
      wtp_sdexp = 0.
      wtp_sdc1 = 0.
      wtp_sdc2 = 0.
      wtp_sdc3 = 0.
      wtp_pdia = 0.
      wtp_plen = 0.
      wtp_pmann = 0.
      wtp_ploss = 0.
      sub_cn2 = 0.   
 	  wtp_dp = 0.
	  wtp_sedi = 0.
	  wtp_sede = 0.	 
	  wtp_qi = 0.
      ovrlnd_dt = 0.
      bmp_recharge = 0.
      sfsedmean = 0.
      sfsedstdev = 0.
	  
      return
      end