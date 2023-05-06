      subroutine zero0

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes the values for some of the arrays 

      use parm
      
      isalt = 0
      salt_num = 0
      sub_saltmo = 0
!!    Srin co2 (EPA)
      co2_x2 = 0.
      co2_x = 0.
      
      lid_sw_add = 0.
      lid_farea = 0.
      rg_sarea = 0.
      co_p = .1
      
      ifirstatmo = 1
      mo_atmo = 0
      
      ires_nut = 0
!!    apex command initialize
      idapa = 0
      iypa = 0
      flodaya = 0.
      seddaya = 0.
      orgndaya = 0.
      orgpdaya = 0.
      no3daya = 0.
      minpdaya = 0.
!!    apex command initialize

      lat_orgn = 0.
      lat_orgp = 0.
      cf = 0.0
      cfh = 0.0
      cfdec = 0.0
      pnd_d50 = 0.0
      isnow = 0
      imgt = 0
      iwtr = 0


!!    initialize mike van liew variables from .bsn file
      anion_excl_bsn = 0.
      ch_onco_bsn = 0.
      ch_opco_bsn = 0.
      hlife_ngw_bsn = 0.
      rcn_sub_bsn = 0.
      bc1_bsn = 0.
      bc2_bsn = 0.
      bc3_bsn = 0.
      bc4_bsn = 0.
      decr_min = 0.
!!    initialize mike van liew variables from .bsn file

      
      wtab = 0.8
!    Drainmod tile equations  01/2006 
      cumeira = 0.
      cumei = 0.
      cumrai = 0.
      cumrt = 0.
      ranrns_hru = 20.
!    Drainmod tile equations  01/2006
      afrt_surface = 0.
      alai_min = 0.
      lai_init = 0.
      aird = 0.
      alpha_bf = 0.
      alpha_bfe = 0.
      ammonian = 0.
      amp_r = 0.
      ap_ef = 0.
      atmo_day = 0.
      auto_eff = 0.
      auto_nyr = 0.
      auto_napp = 0.
      auto_nstrs = -1.
      auto_wstr = 0.
      bactkddb = 0.
      bactlp_plt = 0.
      bactlpdb = 0.
      bactp_plt = 0.
      bactpdb = 0.
      bc1 = 0.
      bc2 = 0.
      bc3 = 0.
      bc4 = 0.
      bio_e = 0.
      bio_hv = 0.
      bio_init = 0.
      bio_leaf = 0.
      bio_min = 0.
      bio_ms = 0.
      bio_n1 = 0.
      bio_n2 = 0.
      bio_p1 = 0.
      bio_p2 = 0.
      bio_targ = 0.
      blai = 0.
      bio_eat = 0.
      bio_trmp = 0.
      brt = 0.
      bss = 0.
      canmx = 0.
      canstor = 0.
      cbodcnst = 0.
      cbodmon = 0.
      cbodyr = 0.
      cfrt_id = 0
      cfrt_kg = 0.
      ch_d = 0.
      ch_di = 0.
      ch_k = 0.
      ch_li = 0.
      ch_onco = 0.
      ch_opco = 0.
      ch_orgn = 0.
      ch_orgp = 0.
      ch_n = 0.
      ch_s = 0.
      ch_si = 0.
      ch_w = 0.
      ch_wi = 0.
      ch_wdr = 0.

!!    Initialization by balaji
      ch_bnk_bd = 0.
      ch_bed_bd = 0.
      ch_bnk_kd = 0.
      ch_bed_kd = 0.
      tc_bnk = 0.
      tc_bed = 0.
      ch_bnk_d50 = 0.
      ch_bed_d50 = 0.
      ch_bed_san = 0.
      ch_bed_sil = 0.
      ch_bed_cla = 0.
      ch_bed_gra = 0.
      ch_bnk_san = 0.
      ch_bnk_sil = 0.
      ch_bnk_cla = 0.
      ch_bnk_gra = 0.
      ch_eqn = 0 
      ch_erodmo = 0.      !CB 12/2/09
      ch_cov1 = 0.
      ch_cov2 = 0.
      chlacnst = 0.
      chlamon = 0.
      chlayr = 0.
      chlora = 0.
      chpst_conc = 0.
      chpst_koc = 0.
      chpst_mix = 0.
      chpst_rea = 0.
      chpst_rsp = 0.
      chpst_stl = 0.
      chpst_vol = 0.
      chside = 0.
      chtmx = 0.
      cncoef_sub = 0.
      cn1 = 0.
      cn2 = 0.
      cn3 = 0.
      cnop = 0.
      cnyld = 0.
      co2 = 0.
!    Drainmod tile equations  01/2006 
      conk = 0.
!    Drainmod tile equations  01/2006
      conv_wt = 0.
      cpst_id = 0
      cpnm = ""
      cpyld = 0.
      curbden = 0.
      curyr = 0
      curyr_mat = 0
      igrotree = 0
      cvm = 0.
      daylmn = 0.
      daru_km = 0.
!    Drainmod tile equations  01/2006 
      dc = 0.
      drain_co_bsn = 0.
!    Drainmod tile equations  01/2006
      ddrain = 0.
      ddrain_bsn = 0.
      decay_f = 0.
      decay_s = 0.
      deepirr = 0.
      deepst = 0.
      delay = 0.
      depimp_bsn = 0.
      dep_imp = 0.
      deptil = 0.
      det_san = 0.
      det_sil = 0.
      det_cla = 0.
      det_sag = 0.
      det_lag = 0.
      dewpt = 0.
      dirtmx = 0.
      dis_stream = 0.
      disolvp = 0.
      disoxcnst = 0.
      disoxmon = 0.
      disoxyr = 0.
      divmax = 0.
      dlai = 0.
      dormhr = 0.
      dorm_hr = -1.
      dorm_flag = 0
      driftco = 0.
!    Drainmod tile equations  01/2006 
      dtwt = 600.
      dz = 0.
!    Drainmod tile equations  01/2006
      drydep_no3 = 0.
      drydep_nh4 = 0.
      eo_30d = 0.
      effmix = 0.
      elevb = 0.
      elevb_fr = 0.
      epco = 0.
      esco = 0.
      evpnd = 0.
      evpot = 0.
      evrsv = 0.
      evwet = 0.
      fcst_reg = 0
      fcstaao = 0.
      fcstcnt = 0
      fertnm = ""
      ffc = 0.
      ffcst = 0
      filterw = 0.
      fimp = 0.
      fld_fr = 0.
      flomon = 0.
      flowmin = 0.
      fminn = 0.
      fminp = 0.
      fnh3n = 0.
      forgn = 0.
      forgp = 0.
      fpcp_stat = 0.
      fpr_w = 0.
      frac_harvk = 0.
      frt_kg = 0.
      frt_surface = 0.
      ftmpmn = 0.
      ftmpmx = 0.
      ftmpstdmn = 0.
      ftmpstdmx = 0.
      gdrain = 0.
      gdrain_bsn = 0.
!    Drainmod tile equations  01/2006 
      gee = 0.
!    Drainmod tile equations  01/2006
      gsi = 0.
      gw_delaye = 0.
      gw_q = 0.
      gw_revap = 0.
      gw_spyld = 0.
      gwht = 0.
      gwq_ru = 0.
!     latq_ru = 0.
!      surfq_ru = 0.
!      infl_ru = 0.
      gwqmn = 0.
!    Drainmod tile equations  01/2006 
      hdrain = 0.
      hdrain_bsn = 0.
!    Drainmod tile equations  01/2006
      hi_ovr = 0.
      hi_targ = 0.
      hlife_f = 0.
      hlife_s = 0.
      hqd = 0.
      hqdsave = 0.
      hru_dafr = 0.
      hru_fr = 0.
      hru_ha = 0.
      hru_km = 0.
      hru_rufr = 0.
      hrugis = 0
      hrupest = 0
      hrupsta = 0.
      hrupstm = 0.
      hrupsty = 0.
      hru_rufr = 0.
      huminc = 0.
      hvsti = 0.
      hyd_dakm = 0.
      iafrttyp = 0
      iatmodep = 0
      icalen = 0
      icfrt = 0
      icodes = 0
      icpst = 0
      icr = 0
      icrmx = 0
      iday_fert = 0
      idc = 0
      idop = 0
      idorm = 0
      idplt = 0
      idplrot = 1
      idtill = 0
      ihv_gbm = 0
      wstrs_id = 0
      ifirstpcp = 1
      ifirsthr = 1
      ifirsta = 1 
      ifirstr = 1 
      ifirstt = 1 
      ifld = 0
      iflod1 = 0
      iflod2 = 0
      ifrt_freq = 0
      ifrttyp = 0
      irelh = 1
      manure_id = 0
      no_lup = 1
      igro = 0
      igrz = 0
      ihouts = 0
      ils2 = 0
      ils2flag = 0
      ils_nofig = 0
      inum1s = 0
      inum2s = 0
      inum3s = 0
      inum4s = 0
      inum5s = 0
      inum6s = 0
      inum7s = 0
      inum8s = 1
      iop = 0
      ioper = 1
      iopera = 1
      iopday = 0
      iopyr = 0
      ipdhru = 0
      ipdvab = 0
      ipdvar = 0
      ipdvas = 0
      ipest = 0
 !!     ipot = 0
      ireg = 1
      irgage = 0
      irip = 0
      iroutunit = 0
      irn = 0
      irramt = 0.
      irreff = 1.
      irrefm = 1.
      irrsalt = 0.
      irrsc = 0
      irrno = 0
      irr_sca = 0
      irr_noa = 0
      irrsq = 0
      irrno = 0
      irrsc = 0
      irr_sc = 0
      irr_no = 0
      irr_sq = 0
      irr_asq = 0
      irr_sca = 0
      irr_noa = 0
      itb = 0 
      itemp = 0
      itgage = 0
      iurban = 0
      ivar_orig = 0
      kirr = " "
      laiday = 0.
!    Drainmod tile equations  02/2009 
      latksatf = 0.
      latksatf_bsn = 0.
!    Drainmod tile equations  02/2009
      lat_sed = 0.
      lat_ttime = 0.
      latcos = 0.
      latsin = 0.
      leaf1 = 0.
      leaf2 = 0.
      mcr = 1
      mcrhru = 0
      mgtop = 0
      mgt1iop = 0
      mgt2iop = 0
      mgt3iop = 0
      mgt4op = 0.0
      mgt5op = 0.0
      mgt6op = 0.0
      mgt7op = 0.0
      mgt8op = 0.0
      mgt9op = 0.0
      mgt10iop = 0
      mo_transb = 0
      mo_transe = 0
      ncrops = 0 
      ncut = 1
      ndcfrt = 0
      fert_days = 0
      grz_days = 0
      nair = 1
      irr_mx = 0.
      latno3 = 0.
      nicr = 0
      ndmo = 0
      ndtarg = 0
      newrti = 0.
      nitraten = 0.
      nitriten = 0.
      nmgt = 0
      nope = 0
      nopmx = 0
      npcp = 1
      npno = 0
      nro = 1
      nrot = 0
      ntil = 1
      orig_alai = 0.
      orig_bioms = 0.
      orig_deepst = 0.
      orig_igro = 0
      organicn = 0.
      organicp = 0.
      orgn_con = 0.
      orgp_con = 0.
      ov_n = 0.
!    Drainmod tile equations  01/2006 
      pc = 0.
      pc_bsn = 0.
!    Drainmod tile equations  01/2006
      phubase = 0.
      pltnfr = 0.
      pltpfr = 0.
      pot_seep = 0.
!----------------------- !Moriasi 4/8/2014  
      prf = 0.
      prf_bsn = 0. 
      spcon_bsn = 0.
      spexp_bsn = 0.
      r2adj = 0.
      r2adj_bsn = 0.
!----------------------- !Moriasi 4/8/2014       
!! drainmod tile equations   06/2006
      ranrns = 0.
!! drainmod tile equations   06/2006
      qird = 0.
      rammo_sub = 0.
      rch_cbod = 0.
      rch_dox = 0.
      rchrg_n = 0.
      rcn_sub = 0.
      rcn_sub_bsn = 0.
      reccnstps = '          ' ! defined in modparm.f for 10 chars By lj
      recmonps = '          ' ! defined in modparm.f for 10 chars. By lj
      rammo_mo = 0.
      rcn_mo = 0.
      drydep_nh4_mo = 0.
      drydep_no3_mo = 0.
      rammo_d = 0.
      rcn_d = 0.
      drydep_nh4_d = 0.
      drydep_no3_d = 0.
!! routing 5/3/2010 gsm per jga
      idum = 0
      mhyd1 = 0
      irtun = 0
      
!    Drainmod tile equations  01/2006 
      sdrain = 0.
      sdrain_bsn = 0.
      sstmaxd = 0.
      sstmaxd_bsn = 0.
      re = 0.
      re_bsn = 0.
      drain_co = 0.
      
 !New water table depth parameters D. Moriasi 4/8/2014
      sol_swpwt = 0.
      sol_stpwt = 0.
      vwt = 0.
      wat_tbl = 0.         
!    Drainmod tile equations  01/2006
      rsr1 = 0.
      rsr2 = 0.
      rsr1 = 0.
      rsr2 = 0.
      sed_con = 0.
      sepcrk = 0.
      sq_rto = 0.
      sol_clay = 0. 
!    Drainmod tile equations  01/2006 
      stmaxd = 0.
      stmaxd_bsn = 0.
!    Drainmod tile equations  01/2006      
      sol_ec = 0.
      sol_sand = 0.
      sol_silt = 0.
      sol_clay = 0.
!!   added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
      sol_sumno3 = 0.
      sol_sumsolp = 0.
      strsw = 1.
      strsw_sum = 0.
      strstmp_sum = 0.
      strsn_sum = 0.
      strsp_sum = 0.
      strsa_sum = 0.
      stsol_rd = 0.
      soln_con = 0.
      solp_con = 0.
      subed = 0
      sub_elev = 0.
      subfr_nowtr = 0.
      sub_lat = 0.
      sub_latq = 0.
      sub_tileq = 0.
      sub_vaptile = 0.
      sub_latno3 = 0.
      sub_smtmp = 0.
      sub_tileno3 = 0.
      sub_gwq_d = 0.
      alpha_bf_d = 0.
      gw_qdeep = 0.
      subgis = 0
      tb_adj = 0.
      tdrain = 0.
      tdrain_bsn = 0.
      tile_no3 = 0.
      tileq = 0.
      tile_ttime = 0.
      uh = 0.
      vfsratio = 0.
      vfscon = 0.
      vfsi = 0.
      vfsch = 0.
      wt_shall = 0.
      wshd_aamon = 0.
!      wshddayo = 0.
      yr_skip = 0
      
      !initialize flood routing variables
      do i=1,4
          IHX(i) = i
      end do
      QHY = 0.
      NHY = 1
      RCHX = 0.
      RCSS = 0.
      QCAP = 0.
      CHXA = 0.
      CHXP = 0.
      return
      end