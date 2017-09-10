      subroutine sim_initday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initialized arrays at the beginning of the day

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i_mo        |none          |current month being simulated
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dayl(:)     |hr            |day length for the day in HRU
!!    flat(:,:)   |mm H2O        |lateral flow storage array
!!    frad(:,:)   |none          |fraction of solar radiation occuring during 
!!                               |hour in day in HRU
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    hru_rmx(:)  |MJ/m^2        |maximum possible radiation for the day in HRU
!!    mo_chk      |none          |check for month being simulated; when mo_chk
!!                               |differs from mo, monthly output is printed
!!    pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto 
!!                               |sediment
!!    pst_surq(:,:)|kg/ha        |pesticide loading from HRU in the water phase
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the
!!                               |day in HRU
!!    rchdy(:,:)  |varies        |daily reach output array
!!    rhd(:)      |none          |relative humidity for the day in HRU
!!    hrupstd(:,:,:)|varies      |HRU daily pesticide output array
!!    sol_cnsw(:) |mm H2O        |amount of water stored in soil profile on day
!!                               |(value used to calculated CN number for day)
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    strsw(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |water stress
!!    sub_bactlp(:)|# bact/ha     |less persistent bacteria loading on day
!!                               |from subbasin
!!    sub_bactp(:)|# bact/ha     |persistent bacteria loading on day from 
!!                               |subbasin
!!    sub_bd(:)   |Mg/m^3        |average bulk density for subbasin
!!    sub_cbod(:) |kg cbod       |carbonaceous biological oxygen demand loading
!!                               |on day from subbasin
!!    sub_chl(:)  |kg chl-a      |chlorophyll-a loading on day from subbasin
!!    sub_dox(:)  |kg O2         |dissolved oxygen loading on day from subbasin
!!    sub_etday(:)|mm H2O        |actual evapotranspiration on day in subbasin
!!    sub_gwq(:)  |mm H2O        |groundwater loading on day in subbasin
!!    sub_latno3(:)|kg N/ha       |NO3 in lateral flow on day in subbasin
!!    sub_no3(:)  |kg N/ha       |NO3 in surface runoff on day in subbasin
!!    sub_orgn(:) |kg N/ha       |organic nitrogen in soil of subbasin
!!    sub_orgp(:) |kg P/ha       |organic phosphorus in soil of subbasin
!!    sub_precip(:)|mm H2O        |water reaching ground surface on day in 
!!                               |subbasin
!!    sub_pst(:,:)|kg/ha         |pesticide in soil of subbasin
!!    sub_qd(:)   |mm H2O        |surface runoff loading on day in subbasin
!!    sub_sedy(:) |metric tons   |sediment loading on day from subbasin
!!    sub_sep(:)  |mm H2O        |percolation out of soil profile on day in 
!!                               |subbasin
!!    sub_snom(:) |mm H2O        |snow melt for day in subbasin
!!    sub_solp(:) |kg P/ha       |soluble P in surface runoff on day in subbasin
!!    sub_solpst(:)|mg pst        |soluble pesticide loading on day in subbasin
!!    sub_sorpst(:)|mg pst        |sorbed pesticide loading on day in subbasin
!!    sub_subp(:) |mm H2O        |precipitation for day in subbasin
!!    sub_sumfc(:)|mm H2O        |amount of water in soil at field capacity in
!!                               |subbasin
!!    sub_surfq(:)|mm H2O        |surface runoff generated on day in subbasin
!!    sub_sw(:)   |mm H2O        |amount of water in soil on day in subbasin
!!    sub_tileno3 |kg N/ha       |NO3 in tile flow on day in subbasin       
!!    sub_tran(:) |mm H2O        |transmission losses on day in subbasin
!!    sub_wyld(:) |mm H2O        |water yield on day in subbasin
!!    sub_yorgn(:)|kg N/ha       |organic N loading on day in subbasin
!!    sub_yorgp(:)|kg P/ha       |organic P loading on day in subbasin
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    tmn(:)      |deg C         |minimum temperature for the day in HRU
!!    tmpav(:)    |deg C         |average temperature for the day in HRU
!!    tmx(:)      |deg C         |maximum temperature for the day in HRU
!!    u10(:)      |m/s           |wind speed for the day in HRU
!!    wcklsp(:)   |
!!    wpstdayo(:,:)|varies        |watershed daily pesticide output array
!!    wshddayo(:) |varies        |watershed daily output array
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      !!initialize variables at beginning of day
      cbodu = 0.
      chl_a = 0.
      cnday = 0.
      dayl = 0.
      doxq = 0.
      drift = 0.
      flat = 0.
      frad = 0.
!      gwq_ru = 0.
      hru_ra = 0.
      hru_rmx = 0.
      hrupstd = 0.
	  irr_flag = 0
      latno3 = 0.
      latq = 0.
      minpgw = 0.
      no3gw = 0.
      nplnt = 0.
      pcpband = 0.
      percn = 0.
      petmeas = 0.
      potsa = 0.
      pplnt = 0.
      pst_sed = 0.
      pst_surq = 0.
      qdr = 0.
      rainsub = 0.
      rchdy = 0.
      rchrg_src = 0.    !CB 8/24/09
      rhd = 0.
      sedminpa = 0.
      sedminps = 0.
      sedorgn = 0.
      sedorgp = 0.

      sedyld = 0.
      sanyld = 0.
      silyld = 0.
      clayld = 0.
      sagyld = 0.
      lagyld = 0.
      grayld = 0.

      sepbtm = 0.
      sol_cnsw = 0.
      sol_prk = 0.
      strsa = 1.
      strsn = 1.
      strsp = 1.
      strstmp = 1.
!!  NUBZ    strsw = 1.
      sub_bactlp = 0.
      sub_bactp = 0.
      sub_bd = 0.
      sub_cbod = 0.
      sub_chl = 0.
      sub_dox = 0.
      sub_etday = 0.
      sub_gwno3 = 0.
      sub_gwsolp = 0.
      sub_gwq = 0.
      sub_hhqd = 0.
      sub_hhwtmp = 0.
      sub_latno3 = 0.
      sub_latq = 0.
      sub_tileq = 0.
      sub_minp = 0.
      sub_minpa = 0.
      sub_minps = 0.
      sub_no3 = 0.
      sub_orgn = 0.
      sub_orgp = 0.
      sub_pet = 0.
      sub_precip = 0.
      sub_pst = 0.
      sub_qd = 0.
      sub_sedpa = 0.
      sub_sedps = 0.

      sub_sedy = 0.
	sub_dsan = 0.
	sub_dsil = 0.
	sub_dcla = 0.
	sub_dsag = 0.
	sub_dlag = 0.
	sub_dgra = 0.

      sub_sep = 0.
      sub_snom = 0.
      sub_solp = 0.
      sub_solpst = 0.
      sub_sorpst = 0.
      sub_subp = 0.
      sub_sumfc = 0.
      sub_surfq = 0.
      sub_sw = 0.
      sub_tileno3 = 0.
      sub_tran = 0.
      sub_wtmp = 0.
      sub_wyld = 0.
      sub_gwq_d = 0.
      sub_yorgn = 0.
      sub_yorgp = 0.
      subp = 0.
      surfq = 0.
      surqno3 = 0.
      surqsolp = 0.
      tavband = 0.
      tileno3 = 0.    !CB 8/24/09
      tmn = 0.
      tmnband = 0.
      tmpav = 0.
      tmx = 0.
      tmxband = 0.
      u10 = 0.
      wcklsp = 0.
      wpstdayo = 0.
      wshddayo = 0.

      mo_chk = i_mo
!----------------------------------------------------        
! added by J.Jeong for urban modeling 4/29/2008
      ubnrunoff = 0.
      ubntss = 0.
      sub_ubnrunoff = 0.
      sub_ubntss = 0.
      latq = 0.
	  sub_subp_dt = 0.
	  sub_hhsedy = 0.
      sub_atmp = 0.
	  rchhr = 0.
!-----------------------------------------------------        

      !!add by zhang
      !!==========================
        sedc_d = 0.
        surfqc_d =0.
        latc_d = 0.
        percc_d = 0.
        foc_d = 0.
        NPPC_d = 0.
        rsdc_d = 0. 
        grainc_d = 0.
        stoverc_d = 0.
        emitc_d = 0.
        soc_d = 0.  
        rspc_d = 0.   

	sub_sedc_d =0.
	sub_surfqc_d=0.
	sub_latc_d=0.
	sub_percc_d=0.
	sub_foc_d=0.
	sub_NEPC_d=0.
	sub_rsdc_d=0.
	sub_grainc_d=0.
	sub_stover_c_d=0.
	sub_emit_c_d=0.
	sub_soc_d	=0.
	sub_rspc_d =0.
      !!add by zhang
      !!==========================
	
      return
      end