      module parm
!!   change per JGA 8/31/2011 gsm for output.mgt 
      real :: yield
!!    arrays for Landscape Transport Capacity 5/28/2009 nadia
      real, dimension (:), allocatable :: l_k1, l_k2, l_lambda, l_beta
      real, dimension (:), allocatable :: l_gama, l_harea, l_vleng
      real, dimension (:), allocatable :: l_vslope, l_ktc
     
!!    new arrays for routing units
      real, dimension (:,:), allocatable :: hru_rufr
      real, dimension (:), allocatable :: daru_km, gwq_ru
      integer :: iru, mru, irch, isub, idum, mhyd_bsn, ipest
      integer, dimension (:), allocatable :: mhyd1 , irtun

!! septic variables for output.std
      real :: wshd_sepno3, wshd_sepnh3, wshd_seporgn, wshd_sepfon
      real :: wshd_seporgp, wshd_sepfop, wshd_sepsolp, wshd_sepbod
      real :: wshd_sepmm
      integer, dimension (:), allocatable :: isep_hru                
!! septic variables for output.std
      real :: fixco, nfixmx, rsd_covco, buff_cf, vcrit, res_stlr_co
      real :: wshd_sw, wshd_snob, wshd_pndfr, wshd_pndv, wshd_pndsed
      real :: wshd_wetfr, wshd_resfr, wshd_resha, wshd_pndha, percop
      real :: wshd_fminp, wshd_ftotn, wshd_fnh3, wshd_fno3, wshd_forgn
      real :: wshd_forgp, wshd_ftotp, wshd_yldn, wshd_yldp, wshd_fixn
      real :: wshd_pup, wshd_wstrs, wshd_nstrs, wshd_pstrs, wshd_tstrs
      real :: wshd_hmn, wshd_rwn, wshd_hmp, wshd_rmn, wshd_dnit, ffcb
      real :: wshd_rmp, wshd_voln, wshd_nitn, wshd_pas, wshd_pal, wdpq
      real :: wshd_plch, wshd_raino3, ressedc, basno3f, basorgnf, wof_p
      real :: basminpf, basorgpf, sftmp, smtmp, smfmx, smfmn, wgpq
      real :: wshd_resv, wshd_ressed, basno3i, basorgni, basminpi, wdlpq
      real :: basorgpi, peakr, pndsedin, sw_excess, albday, wglpq, wdps
      real :: wtabelo, timp, tilep, wt_shall
      real :: sq_rto
      real :: tloss, inflpcp, snomlt, snofall, fixn, qtile, crk, latlyr
      real :: sedrch, fertn, sol_rd, cfertn, cfertp, sepday, bioday
      real :: sepcrk, sepcrktot, fertno3, fertnh3, fertorgn, fertsolp
      real :: fertorgp
      real :: fertp, grazn, grazp, soxy, qdfr, sdti, rtwtr, ressa, wgps
      real :: rttime, rchdep, rtevp, rttlc, da_km, resflwi, wdlps, wglps
      real :: resflwo, respcp, resev, ressep,ressedi,ressedo,dtot,wdprch
      real :: nperco, pperco, rsdco, phoskd, voltot, volcrmin, msk_x
      real :: uno3d, canev, usle, rcn, surlag, bactkdq, precipday, wdpf
      real :: thbact, wpq20, wlpq20, wps20, wlps20, bactrop, bactsedp
      real :: bactlchp, bactlchlp, enratio, wetpcp, pndpcp, wetsep, wgpf
      real :: pndsep, wetev, pndev, pndsedo, wetsedo, pndflwi, wetflwi
      real :: pndflwo, wetflwo, wetsedi, da_ha, twlwet, twlpnd, vpd
      real :: bactrolp, bactsedlp, evrch, evlai, pet_day, ep_day, wdlpf
      real :: snoev, sno3up, adj_pkr, n_updis, p_updis, nactfr, reactw
      real :: sdiegropq, sdiegrolpq, sdiegrops, sdiegrolps, es_day
      real :: sbactrop, sbactrolp, sbactsedp, sbactsedlp, ep_max, wof_lp
      real :: sbactlchp, sbactlchlp, psp, rchwtr, resuspst, setlpst
      real :: bsprev, bssprev, spadyo, spadyev, spadysp, spadyrfv
      real :: qday, usle_ei, al5, pndsedc, no3pcp, rcharea, volatpst
      real :: wetsedc, uobw, ubw, uobn, uobp, prf, respesti, wglpf
      real :: snocovmx, snocov1, snocov2, rexp, rcor, lyrtile, lyrtilex
      real :: ai0, ai1, ai2, ai3, ai4, ai5, ai6, rhoq, tfact, sno50cov
      real :: mumax, lambda0, lambda1, lambda2, k_l, k_n, k_p, p_n
      real :: rnum1, autop, auton, etday, hmntl, rwntl, hmptl, rmn2tl
      real :: rmptl, wdntl, cmn, rmp1tl, roctl, gwseep, revapday, reswtr
      real :: bury, difus, reactb, solpesto, petmeas, wdlprch, wdpres
      real :: sorpesto, spcon, spexp, solpesti, sorpesti, wdlpres
      real :: snoprev, swprev, shallstp, deepstp, msk_co1, msk_co2
      real :: ressolpo, resorgno, resorgpo, resno3o, reschlao, resno2o
      real :: resnh3o, qdbank, potpcpmm, potevmm, potsepmm, potflwo
      real :: potsedo, pest_sol, trnsrch, wp20p_plt, bactminp, bactminlp
      real :: wp20lp_plt, cncoef, cdn, sdnco, bact_swf, bactmx, bactmin
      real :: chla_subco, tb_adj, cn_froz, dorm_hr, smxco
      real :: depimp_bsn, ddrain_bsn, tdrain_bsn, gdrain_bsn
      real :: rch_san, rch_sil, rch_cla, rch_sag, rch_lag, rch_gra


!!    delcare mike van liew variables
      real :: hlife_ngw_bsn, ch_opco_bsn, ch_onco_bsn
      real :: bc1_bsn, bc2_bsn, bc3_bsn, bc4_bsn, rcn_sub_bsn, decr_min         
      real :: anion_excl_bsn
!!    delcare mike van liew variables

!    Drainmod tile equations  01/2006
      real, dimension (:), allocatable :: wat_tbl,sol_swpwt
      real, dimension (:,:), allocatable :: vwt
	real :: re_bsn, sdrain_bsn
	real :: drain_co_bsn, pc_bsn, latksatf_bsn 
!    Drainmod tile equations  01/2006
      integer :: i_subhw, imgt, idlast, iwtr, ifrttyp
      integer :: mrg, mch, mcr, mpdb, mcrdb, mfdb, mhru, mhyd, mfcst
      integer :: mnr, myr, mcut, mgr, msubo, mrcho, isubwq, ffcst
      integer :: nhru, isproj, mo, nbyr, immo, nrch, nres, irte, i_mo
      integer :: icode, ihout, inum1, inum2, inum3, inum4, wndsim, ihru
      integer :: inum5, icfac
      integer :: nrgage, ntgage, nrgfil, ntgfil, nrtot, nttot, mrech
      integer :: lao, igropt, npmx, irtpest, curyr, tmpsim, icrk, iihru
!    Drainmod tile equations  01/2006
	integer :: itdrn, iwtdn
!    Drainmod tile equations  01/2006
      integer :: mtil, mvaro, mrecd, idist, mudb, mrecm, mrecc, iclb
      integer :: mrecy, ipet, nyskip, ideg, ievent, slrsim, iopera
      integer :: id1, idaf, idal, leapyr, mo_chk, rhsim, mstdo
      integer :: ifirsts, ifirsth, ifirstw, nstot, nhtot, nwtot, icst
      integer :: ilog, i, iyr, itotr, iwq, iskip, scenario, ifirstpet
      integer :: itotb,itots,iprp,pcpsim,itoth,nd_30,iops,iphr,isto,isol
      integer :: iscen, fcstyr, fcstday, fcstcycles, subtot, ogen
      integer :: msub, mhruo, mres, mapp, mpst, mlyr, igen, iprint, iida
      integer :: fcstcnt, icn, ised_det, mtran, idtill
      integer, dimension(100) :: ida_lup, iyr_lup
      integer :: no_lup, no_up
!  routing 5/3/2010 gsm per jga
      integer :: rutot     
! date
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone
      character(len=80) :: prog
      character(len=13) :: slrfile, wndfile, rhfile, petfile, calfile
      character(len=13) :: atmofile, lucfile
      character(len=13) :: septdb
      character(len=13) :: dpd_file, wpd_file, rib_file, sfb_file
      integer, dimension (:), allocatable :: ifirstr, idg, ifirsthr
      integer, dimension (:), allocatable :: values, ndays
!     apex/command output files
      integer :: mapex
      real, dimension (:), allocatable :: flodaya, seddaya, orgndaya
      real, dimension (:), allocatable :: orgpdaya, no3daya, minpdaya
      real, dimension (:), allocatable :: hi_targ, bio_targ, tnyld
      integer, dimension (:), allocatable :: idapa, iypa, ifirsta
      integer, dimension (:), allocatable :: mo_transb, mo_transe
      integer, dimension (:), allocatable :: ih_tran
!     apex/command output files
!  septic inputs
!! septic change added iseptic 1/28/09 gsm
      integer :: msdb, iseptic
      real, dimension (:), allocatable :: sptqs,percp
      real, dimension (:), allocatable :: sptbodconcs, spttssconcs
      real, dimension (:), allocatable :: spttnconcs, sptnh4concs
      real, dimension (:), allocatable :: sptno3concs, sptno2concs
      real, dimension (:), allocatable :: sptorgnconcs, spttpconcs
      real, dimension (:), allocatable :: sptminps, sptorgps      
      real, dimension (:), allocatable :: sptfcolis ,failyr,qstemm               
!! septic changes added 1/28/09 gsm
      real, dimension (:), allocatable :: bio_amn, bio_bod, biom,rbiom
      real, dimension (:), allocatable :: fcoli, bio_ntr, bz_perc
      real, dimension (:), allocatable :: plqm,sep_cap,bz_area
      real, dimension (:), allocatable :: bz_z, bz_thk,  bio_bd
!! carbon outputs for .hru file
      real, dimension (:), allocatable :: cmup_kgh, cmtot_kgh
!! carbon outputs for .hru file
      real, dimension (:), allocatable :: coeff_bod_dc, coeff_bod_conv
      real, dimension (:), allocatable :: coeff_fc1, coeff_fc2
      real, dimension (:), allocatable :: coeff_fecal, coeff_plq
      real, dimension (:), allocatable :: coeff_mrt, coeff_rsp
      real, dimension (:), allocatable :: coeff_slg1, coeff_slg2
      real, dimension (:), allocatable :: coeff_nitr, coeff_denitr
      real, dimension (:), allocatable :: coeff_pdistrb,coeff_solpslp 
      real, dimension (:), allocatable :: coeff_solpintc,coeff_psorpmax
!! Septic system by Jaehak Jeong
      integer, dimension (:), allocatable :: i_sep,isep_typ
      integer, dimension (:), allocatable :: isep_opt,sep_tsincefail
      integer, dimension (:), allocatable :: isep_tfail,isep_iyr
      integer, dimension (:), allocatable :: sep_strm_dist,sep_den
      
 !!   change per JGA 9/8/2011 gsm for output.mgt 
      real, dimension (:), allocatable :: sol_sumno3, sol_sumsolp
      real, dimension (:), allocatable :: strsw_sum, strstmp_sum
      real, dimension (:), allocatable :: strsn_sum, strsp_sum
      real, dimension (:), allocatable :: strsa_sum
      

!! New pothole variables
      real, dimension (:), allocatable :: spill_hru,tile_out,hru_in
      real, dimension (:), allocatable :: spill_precip,pot_seep
      real, dimension (:), allocatable :: pot_evap,pot_sedin
      real, dimension (:), allocatable :: pot_solp,pot_solpi
      real, dimension (:), allocatable :: pot_orgp,pot_orgpi
      real, dimension (:), allocatable :: pot_orgn,pot_orgni
      real, dimension (:), allocatable :: pot_mps,pot_mpsi
      real, dimension (:), allocatable :: pot_mpa,pot_mpai   
      real, dimension (:), allocatable :: pot_no3i,precip_in
      real, dimension (:), allocatable :: tile_sedo,tile_no3o
      real, dimension (:), allocatable :: tile_solpo,tile_orgno
      real, dimension (:), allocatable :: tile_orgpo,tile_minpso   
      real, dimension (:), allocatable :: tile_minpao                  
! output files 
!!  added for binary files 3/25/09 gsm
      integer :: ia_b, ihumus, itemp, isnow
      integer, dimension (:), allocatable :: icolb,icolr,icolrsv,icols
      integer, dimension (:), allocatable :: ipdvar,ipdvab,ipdvas,ipdhru
      real, dimension (:), allocatable :: wshddayo,wshdmono,wshdyro
      real, dimension (:), allocatable :: wshdaao,fcstaao
      real, dimension (:,:), allocatable :: wpstdayo,wpstmono,wpstyro
      real, dimension (:,:), allocatable :: yldkg, bio_hv
      real, dimension (:,:), allocatable :: wpstaao,rchmono,rchyro
      real, dimension (:,:), allocatable :: rchaao,rchdy,hrumono,hruyro
      real, dimension (:,:), allocatable :: hruaao,submono,subyro,subaao
      real, dimension (:,:), allocatable :: resoutm,resouty,resouta
      real, dimension (:,:), allocatable :: wshd_aamon
      real, dimension (:,:), allocatable :: wtrmon,wtryr,wtraa 
      real, dimension (:,:), allocatable :: sub_smfmx, sub_smfmn
      real, dimension (:,:,:), allocatable :: hrupstd,hrupsta,hrupstm
      real, dimension (:,:,:), allocatable :: hrupsty
! mrg = max number of rainfall/temperature gages
      integer, dimension (:), allocatable :: ifirstt,ifirstpcp
      integer, dimension (:), allocatable :: elevp,elevt
! mfcst = max number of forecast regions
      real, dimension (:,:), allocatable :: ftmpstdmn,ftmpmn,ftmpmx
      real, dimension (:,:), allocatable :: ftmpstdmx
      real, dimension (:,:,:), allocatable :: fpr_w,fpcp_stat
! mch = max number of channels
      real, dimension (:), allocatable :: flwin,flwout,bankst,ch_wi,ch_d
      real, dimension (:), allocatable :: ch_onco, ch_opco
      real, dimension (:), allocatable :: ch_orgn, ch_orgp
      real, dimension (:), allocatable :: drift,rch_dox,rch_bactp
      real, dimension (:), allocatable :: alpha_bnk,alpha_bnke
      real, dimension (:), allocatable :: disolvp,algae,sedst,rchstor
      real, dimension (:), allocatable :: organicn,organicp,chlora
      real, dimension (:), allocatable :: nitraten,nitriten,ch_li,ch_si

!      real, dimension (:), allocatable :: ch_cov,ch_di,ch_erod,ch_l2
!      real, dimension (:), allocatable :: ch_san, ch_sil, ch_cla, ch_veg 
!      real, dimension (:), allocatable :: ch_rcur, ch_ss, ch_fpr, ch_eqn
!      real, dimension (:), allocatable :: ch_crht

!     Sediment parameters added by Balaji for the new routines

      real, dimension (:), allocatable :: ch_bnk_san, ch_bnk_sil
      real, dimension (:), allocatable :: ch_bnk_cla, ch_bnk_gra
      real, dimension (:), allocatable :: ch_bed_san, ch_bed_sil
      real, dimension (:), allocatable :: ch_bed_cla, ch_bed_gra
      real, dimension (:), allocatable :: depfp,depsanfp,depsilfp
      real, dimension (:), allocatable :: depclafp,depsagfp,deplagfp
      real, dimension (:), allocatable :: depch,depsanch,depsilch
      real, dimension (:), allocatable :: depclach,depsagch,deplagch
      real, dimension (:), allocatable :: depgrach,depgrafp,grast
      real, dimension (:), allocatable :: depprch,depprfp
      real, dimension (:), allocatable :: sanst,silst,clast,sagst,lagst
      real, dimension (:), allocatable :: pot_san,pot_sil,pot_cla
      real, dimension (:), allocatable :: pot_sag,pot_lag
      real, dimension (:), allocatable :: potsani,potsili,potclai
      real, dimension (:), allocatable :: potsagi,potlagi
      real, dimension (:), allocatable :: sanyld,silyld,clayld,sagyld
      real, dimension (:), allocatable :: lagyld,grayld
      real, dimension (:), allocatable :: res_san,res_sil,res_cla
      real, dimension (:), allocatable :: res_sag,res_lag,res_gra
      real, dimension (:), allocatable :: pnd_san,pnd_sil,pnd_cla
      real, dimension (:), allocatable :: pnd_sag,pnd_lag
      real, dimension (:), allocatable :: wet_san,wet_sil,wet_cla
      real, dimension (:), allocatable :: wet_lag, wet_sag
      real :: ressano,ressilo,resclao,ressago,reslago, resgrao
      real :: ressani, ressili, resclai, ressagi, reslagi,resgrai
      real :: potsano,potsilo,potclao,potsago,potlago
	real :: pndsanin,pndsilin,pndclain,pndsagin,pndlagin
	real :: pndsano,pndsilo,pndclao,pndsago,pndlago

      real, dimension (:), allocatable :: ch_di,ch_erod,ch_l2, ch_cov
      real, dimension (:), allocatable :: ch_cov1, ch_cov2, ch_bnk_bd
      real, dimension (:), allocatable :: ch_bed_bd,ch_bnk_kd,ch_bed_kd
      real, dimension (:), allocatable :: ch_bnk_d50, ch_bed_d50     
      real, dimension (:), allocatable :: tc_bed,tc_bnk
      integer, dimension (:), allocatable :: ch_eqn                        
      real, dimension (:), allocatable :: chpst_conc,chpst_rea,chpst_vol
      real, dimension (:), allocatable :: chpst_koc,chpst_stl,chpst_rsp
      real, dimension (:), allocatable :: chpst_mix,sedpst_conc,ch_wdr
      real, dimension (:), allocatable :: sedpst_rea,sedpst_bry
      real, dimension (:), allocatable :: sedpst_act,rch_cbod,rch_bactlp
      real, dimension (:), allocatable :: chside,rs1,rs2,rs3,rs4,rs5
      real, dimension (:), allocatable :: rs6,rs7,rk1,rk2,rk3,rk4,rk5
      real, dimension (:), allocatable :: rk6,bc1,bc2,bc3,bc4,ammonian
      real, dimension (:), allocatable :: orig_sedpstconc
      real, dimension (:,:), allocatable :: wurch
      integer, dimension (:), allocatable :: icanal
      integer, dimension (:), allocatable :: itb
! msub = max number of subbasins
      real, dimension (:), allocatable :: ch_revap, dep_chan
      real, dimension (:), allocatable :: harg_petco, subfr_nowtr
      real, dimension (:), allocatable :: cncoef_sub, dr_sub
      real, dimension (:), allocatable :: wcklsp,sub_fr,sub_minp,sub_sw
      real, dimension (:), allocatable :: sub_sumfc,sub_gwno3,sub_gwsolp
      real, dimension (:), allocatable :: sub_km,sub_tc,wlat,sub_pet,co2
      real, dimension (:), allocatable :: welev,sub_orgn,sub_orgp,sub_bd
      real, dimension (:), allocatable :: sub_wtmp,sub_sedpa,sub_sedps
      real, dimension (:), allocatable :: sub_minpa,sub_minps,daylmn
      real, dimension (:), allocatable :: latcos,latsin,phutot
      real, dimension (:), allocatable :: tlaps,plaps,tmp_an,sub_precip
      real, dimension (:), allocatable :: pcpdays, rcn_sub, rammo_sub
      real, dimension (:), allocatable :: sub_snom,sub_qd,sub_sedy
      real, dimension (:), allocatable :: sub_tran,sub_no3,sub_latno3
      real, dimension (:,:), allocatable :: sub_smtmp,sub_timp,sub_sftmp
      real, dimension (:), allocatable :: sub_tileno3     
      real, dimension (:), allocatable :: sub_solp,sub_subp,sub_etday
      real, dimension (:), allocatable :: sub_wyld,sub_surfq,sub_elev
      real, dimension (:), allocatable :: qird
      real, dimension (:), allocatable :: sub_gwq,sub_sep,sub_chl
      real, dimension (:), allocatable :: sub_cbod,sub_dox,sub_solpst
      real, dimension (:), allocatable :: sub_sorpst,sub_yorgn,sub_yorgp
      real, dimension (:), allocatable :: sub_bactp,sub_bactlp,sub_lat
      real, dimension (:), allocatable :: sub_latq
      real, dimension (:), allocatable :: sub_dsan, sub_dsil, sub_dcla
      real, dimension (:), allocatable :: sub_dsag, sub_dlag
!!!!!! drains
!!      real, dimension (:), allocatable :: w
      real, dimension (:,:), allocatable :: sub_pst,sub_hhqd,sub_hhwtmp
      real, dimension (:,:), allocatable :: rfinc,tmpinc,radinc,huminc
      real, dimension (:,:), allocatable :: wndav,ch_k,elevb,elevb_fr
      real, dimension (:,:), allocatable :: dewpt,ch_w,ch_s,ch_n
      real, dimension (:,:), allocatable :: amp_r,solarav,tmpstdmx
      real, dimension (:,:), allocatable :: tmpstdmn,pcf,tmpmn,tmpmx
      real, dimension (:,:), allocatable :: otmpstdmn,otmpmn,otmpmx
      real, dimension (:,:), allocatable :: otmpstdmx, ch_erodmo
      real, dimension (:,:), allocatable :: uh, hqdsave, hsdsave
      real, dimension (:,:,:), allocatable :: pr_w,pcp_stat
      real, dimension (:,:,:), allocatable :: opr_w,opcp_stat
      integer, dimension (:), allocatable :: hrutot,hru1,ireg
      integer, dimension (:), allocatable :: isgage,ihgage,iwgage
      integer, dimension (:), allocatable :: irgage,itgage,subgis
      integer, dimension (:), allocatable :: fcst_reg, irelh
! mlyr = max number of soil layers
      real, dimension (:,:), allocatable :: sol_aorgn,sol_tmp,sol_fon
      real, dimension (:,:), allocatable :: sol_awc,sol_prk,volcr
      real, dimension (:,:), allocatable :: pperco_sub
      real, dimension (:,:), allocatable :: sol_actp,sol_stap,conv_wt
      real, dimension (:,:), allocatable :: sol_solp,sol_ul,sol_fc,crdep
      real, dimension (:,:), allocatable :: sol_z,sol_up,sol_bd,sol_st
      real, dimension (:,:), allocatable :: flat,sol_nh3,sol_hk,sol_clay
!  added 1/27/09 when making septic changes
      real, dimension (:,:), allocatable :: sol_ec 
!  added 1/27/09 when making septic changes
      real, dimension (:,:), allocatable :: sol_orgn,sol_por,sol_wp
      real, dimension (:,:), allocatable :: sol_orgp,sol_hum,sol_wpmm
      real, dimension (:,:), allocatable :: sol_k,sol_cbn,sol_no3
      real, dimension (:,:), allocatable :: sol_rsd,sol_fop
      real, dimension (:,:), allocatable :: sol_silt, sol_sand, sol_rock
      real, dimension (:,:), allocatable :: orig_solno3,orig_solorgn
      real, dimension (:,:), allocatable :: orig_solsolp,orig_solorgp
      real, dimension (:,:), allocatable :: orig_soltmp,orig_solrsd
      real, dimension (:,:), allocatable :: orig_solfop,orig_solfon
      real, dimension (:,:), allocatable :: orig_solaorgn,orig_solst
      real, dimension (:,:), allocatable :: orig_solactp,orig_solstap
      real, dimension (:,:), allocatable :: orig_volcr
!    Drainmod tile equations  01/2006 
	  real, dimension (:,:), allocatable :: conk
!    Drainmod tile equations  01/2006
      real, dimension (:,:,:), allocatable :: sol_pst,sol_kp
      real, dimension (:,:,:), allocatable :: orig_solpst
! mres = max number of reservoirs
      real, dimension (:), allocatable :: velsetlr, velsetlp
      real, dimension (:), allocatable :: br1,res_k,lkpst_conc, evrsv
      real, dimension (:), allocatable :: res_evol,res_pvol,res_vol
      real, dimension (:), allocatable :: res_psa,lkpst_rea,lkpst_vol
      real, dimension (:), allocatable :: br2,res_rr,res_sed,lkpst_koc
      real, dimension (:), allocatable :: lkpst_stl,lkpst_rsp,lkpst_mix
      real, dimension (:), allocatable :: lkspst_conc,lkspst_rea
      real, dimension (:), allocatable :: lkspst_bry,lkspst_act,sed_stlr
      real, dimension (:), allocatable :: wurtnf,res_nsed,resdata,chlar
      real, dimension (:), allocatable :: res_orgn,res_orgp,res_no3
      real, dimension (:), allocatable :: res_solp,res_chla,res_seci
      real, dimension (:), allocatable :: res_esa,seccir,res_no2,res_nh3
      real, dimension (:), allocatable :: res_bactp, res_bactlp
      real, dimension (:), allocatable :: oflowmn_fps, starg_fps
      real, dimension (:), allocatable :: orig_resvol,orig_ressed
      real, dimension (:), allocatable :: orig_lkpstconc,orig_lkspstconc
      real, dimension (:), allocatable :: orig_ressolp,orig_resorgp
      real, dimension (:), allocatable :: orig_resno3,orig_resno2
      real, dimension (:), allocatable :: orig_resnh3,orig_resorgn
      real, dimension (:,:), allocatable :: starg,oflowmx,oflowmn
      real, dimension (:,:), allocatable :: psetlr,nsetlr,wuresn
      real, dimension (:,:,:), allocatable :: res_out
      integer, dimension (:), allocatable :: ires1,ires2,res_sub
      integer, dimension (:), allocatable :: iresco,mores,iyres
      integer, dimension (:), allocatable :: iflod1r,iflod2r,ndtargr
! mpdb = max number of pesticides in the database
      real, dimension (:), allocatable :: skoc,ap_ef,decay_f
      real, dimension (:), allocatable :: hlife_f,hlife_s,decay_s
      real, dimension (:), allocatable :: pst_wsol,pst_wof, irramt
      real, dimension (:), allocatable :: phusw, phusw_nocrop
      real, dimension (:,:), allocatable :: pst_dep
      integer, dimension (:), allocatable :: nope, pstflg, nop
      integer, dimension (:), allocatable :: yr_skip, isweep
      integer, dimension (:), allocatable :: icrmx, nopmx
! new management scehduling variables
      integer, dimension (:,:), allocatable :: mgtop, idop
      integer, dimension (:,:), allocatable :: mgt1iop,mgt2iop,mgt3iop
      real, dimension (:,:), allocatable ::  mgt4op, mgt5op, mgt6op
      real, dimension (:,:), allocatable :: mgt7op, mgt8op, mgt9op
      real, dimension (:,:), allocatable :: mgt10iop, phu_op
! mcrdb = maximum number of crops in database
      real, dimension (:), allocatable :: wac21,wac22,cnyld,rsdco_pl
      real, dimension (:), allocatable :: wsyf,leaf1,leaf2,alai_min
      real, dimension (:), allocatable :: t_base,t_opt,hvsti,bio_e
      real, dimension (:), allocatable :: vpd2,gsi,chtmx,wavp,cvm
      real, dimension (:), allocatable :: blai,dlai,rdmx,cpyld,bio_leaf
      real, dimension (:), allocatable :: bio_n1,bio_n2,bio_p1,bio_p2
      real, dimension (:), allocatable :: bmx_trees,ext_coef,bm_dieoff
      real, dimension (:), allocatable :: rsr1, rsr2                    
!     real, dimension (:), allocatable :: air_str
      real, dimension (:,:), allocatable :: pltnfr,pltpfr
      integer, dimension (:), allocatable :: idc, mat_yrs
! mfdb = maximum number of fertilizer in database
      real, dimension (:), allocatable :: forgn,forgp,fminn,bactpdb
      real, dimension (:), allocatable :: fminp,fnh3n,bactlpdb,bactkddb
      character(len=8), dimension (200) :: fertnm
! mudb = maximum number of land types in urban database
      real, dimension (:), allocatable :: fimp,curbden,urbcoef,dirtmx
      real, dimension (:), allocatable :: thalf,tnconc,tpconc,tno3conc
      real, dimension (:), allocatable :: fcimp,urbcn2
! mapp = max number of applications
      real :: sweepeff,frt_kg
!! added pst_dep to statement below 3/31/08 gsm
!!   burn 3/5/09       
! mnr = max number years of rotation
!!   burn 3/5/09  
! mtil = max number tillages in database
 !! drainmod tile equations   06/2006

      real, dimension (:), allocatable :: ranrns_hru
	  integer, dimension (:), allocatable :: itill
!! drainmod tile equations   06/2006
      real, dimension (:), allocatable :: effmix,deptil, ranrns
      character(len=8), dimension (550) :: tillnm
! mhyd = max number of hydrograph nodes
      real, dimension (:), allocatable :: rnum1s,hyd_dakm
      real, dimension (:,:), allocatable :: varoute,shyd
      real, dimension (:,:,:), allocatable :: hhvaroute
      integer, dimension (:), allocatable :: icodes,ihouts,inum1s
      integer, dimension (:), allocatable :: inum2s,inum3s,inum4s
      integer, dimension (:), allocatable :: inum5s
      integer, dimension (:), allocatable :: subed
      character(len=10), dimension (:), allocatable :: recmonps
      character(len=10), dimension (:), allocatable :: reccnstps
      character(len=5), dimension (:), allocatable :: subnum
      character(len=7), dimension (:), allocatable :: hruno

! mhru = maximum number of hydrologic response units
      real, dimension (:), allocatable :: grwat_n, grwat_i, grwat_l
      real, dimension (:), allocatable :: grwat_w, grwat_d
      real, dimension (:), allocatable :: grwat_s, grwat_spcon
      real, dimension (:), allocatable :: tc_gwat
      real, dimension (:), allocatable ::pot_volmm,pot_tilemm,pot_volxmm  !!NUBZ
      real, dimension (:), allocatable :: pot_fr,pot_tile,pot_vol,potsa
      real, dimension (:), allocatable :: pot_volx,potflwi,potsedi,wfsh
      real, dimension (:), allocatable :: pot_nsed,pot_no3l,newrti,gwno3
      real, dimension (:), allocatable :: pot_sed,pot_no3,fsred,tmpavp
      real, dimension (:), allocatable :: evpot, dis_stream
      real, dimension (:), allocatable :: sed_con, orgn_con, orgp_con
      real, dimension (:), allocatable :: soln_con, solp_con
      integer, dimension (:), allocatable :: ioper
      integer, dimension (:), allocatable :: ngrwat
      real, dimension (:), allocatable :: filterw,sumix,usle_ls,phuacc
      real, dimension (:), allocatable :: esco,epco,slsubbsn,hru_slp
      real, dimension (:), allocatable :: erorgn,erorgp,biomix,pnd_seci
      real, dimension (:), allocatable :: flowmin,divmax,canmx,usle_p
      real, dimension (:), allocatable :: lat_sed,rch_dakm,pnd_no3s,cn1
      real, dimension (:), allocatable :: cn2,lat_ttime,flowfr,sol_zmx
      real, dimension (:), allocatable :: tile_ttime
      real, dimension (:), allocatable :: slsoil,sed_stl,gwminp,sol_cov
      real, dimension (:), allocatable :: yldanu,pnd_solp,pnd_no3,ov_n
      real, dimension (:), allocatable :: driftco,pnd_orgp,pnd_orgn,cn3
      real, dimension (:), allocatable :: sol_sumul,pnd_chla,hru_fr
      real, dimension (:), allocatable :: bio_ms,sol_alb,strsw,hru_km
      real, dimension (:), allocatable :: pnd_fr,pnd_psa,pnd_pvol,pnd_k
      real, dimension (:), allocatable :: pnd_esa,pnd_evol,pnd_vol,yldaa
      real, dimension (:), allocatable :: pnd_sed,pnd_nsed,strsa,dep_imp
      real, dimension (:), allocatable :: evpnd, evwet
      real, dimension (:), allocatable :: wet_fr,wet_nsa,wet_nvol,wet_k
      real, dimension (:), allocatable :: wet_mxsa,wet_mxvol,wet_vol
      real, dimension (:), allocatable :: wet_sed,wet_nsed
      real, dimension (:), allocatable :: smx,sci,bp1,bp2
      real, dimension (:), allocatable :: bw1,bw2,bactpq
      real, dimension (:), allocatable :: bactp_plt,bactlp_plt,cnday
      real, dimension (:), allocatable :: bactlpq,auto_eff,sol_sw,secciw
      real, dimension (:), allocatable :: bactps,bactlps,tmpav,chlaw
      real, dimension (:), allocatable :: subp,sno_hru,hru_ra,wet_orgn
      real, dimension (:), allocatable :: tmx,tmn,rsdin,tmp_hi,tmp_lo
      real, dimension (:), allocatable :: rwt,olai,usle_k,tconc,hru_rmx
      real, dimension (:), allocatable :: anano3,aird,t_ov,sol_sumfc
      real, dimension (:), allocatable :: sol_avpor,usle_mult,wet_orgp
      real, dimension (:), allocatable :: aairr,cht,u10,rhd
      real, dimension (:), allocatable :: shallirr,deepirr,lai_aamx
      real, dimension (:), allocatable :: canstor,ovrlnd,ch_l1,wet_no3
      real, dimension (:), allocatable :: irr_mx, auto_wstr
      real, dimension (:), allocatable :: cfrt_id, cfrt_kg, cpst_id
      real, dimension (:), allocatable :: cpst_kg
      real, dimension (:), allocatable :: irr_asq, irr_eff
      real, dimension (:), allocatable :: irrsq, irrefm, irrsalt
      real, dimension (:), allocatable :: bio_eat, bio_trmp             !!NUBZ
      integer, dimension (:), allocatable :: ifrt_freq,ipst_freq,irr_noa
      integer, dimension (:), allocatable :: irr_sc,irr_no
      integer, dimension (:), allocatable :: imp_trig, fert_days,irr_sca
      integer, dimension (:), allocatable :: pest_days, idplt, wstrs_id
      real, dimension (:,:), allocatable :: bio_aahv
!    Drainmod tile equations  08/2006 
	  real, dimension (:), allocatable :: cumei,cumeira
	  real, dimension (:), allocatable :: cumrt, cumrai
!    Drainmod tile equations  08/2006
      real, dimension (:), allocatable :: wet_solp,wet_no3s,wet_chla
      real, dimension (:), allocatable :: wet_seci,pnd_no3g,pstsol
      real, dimension (:), allocatable :: gwht,delay,gw_q,pnd_solpg
      real, dimension (:), allocatable :: alpha_bf,alpha_bfe,gw_spyld
      real, dimension (:), allocatable :: gw_delaye,gw_revap,rchrg_dp
      real, dimension (:), allocatable :: revapmn,anion_excl,rchrg
      real, dimension (:), allocatable :: ffc,bio_min,surqsolp
      real, dimension (:), allocatable :: cklsp,deepst,shallst,wet_solpg
      real, dimension (:), allocatable :: rchrg_src
      real, dimension (:), allocatable :: wet_no3g,sol_avbd,trapeff
      real, dimension (:), allocatable :: gwqmn,tdrain,pplnt,snotmp
      real, dimension (:), allocatable :: ddrain,gdrain,sol_crk,dayl,brt
!    Drainmod tile equations  01/2006 
	real, dimension (:), allocatable ::ddrain_hru,re,sdrain
	real, dimension (:), allocatable :: stmaxd,drain_co,pc,latksatf
!    Drainmod tile equations  01/2006
      real, dimension (:), allocatable :: twash,rnd2,rnd3,sol_cnsw,doxq
      real, dimension (:), allocatable :: rnd8,rnd9,percn,sol_sumwp
      real, dimension (:), allocatable :: tauton,tautop,cbodu,chl_a,qdr
      real, dimension (:), allocatable :: tfertn,tfertp,tgrazn,tgrazp
      real, dimension (:), allocatable :: latno3,latq,minpgw,no3gw,nplnt
      real, dimension (:), allocatable :: tileq, tileno3
      real, dimension (:), allocatable :: sedminpa,sedminps,sedorgn
      real, dimension (:), allocatable :: sedorgp,sedyld,sepbtm,strsn
      real, dimension (:), allocatable :: strsp,strstmp,surfq,surqno3
      real, dimension (:), allocatable :: tcfrtn,tcfrtp,hru_ha,hru_dafr
      real, dimension (:), allocatable :: drydep_no3, drydep_nh4
      real, dimension (:), allocatable :: phubase,bio_yrms,hvstiadj
      real, dimension (:), allocatable :: laimxfr,laiday,chlap,pnd_psed
      real, dimension (:), allocatable :: wet_psed,seccip,plantn,plt_et
      real, dimension (:), allocatable :: plt_pet,plantp,bio_aams
      real, dimension (:), allocatable :: bio_aamx,lai_yrmx,dormhr
      real, dimension (:), allocatable :: lat_pst
      real, dimension (:), allocatable :: orig_snohru,orig_potvol,fld_fr
      real, dimension (:), allocatable :: orig_alai,orig_bioms,pltfr_n
      real, dimension (:), allocatable :: orig_phuacc,orig_sumix,pltfr_p
      real, dimension (:), allocatable :: orig_phu
      real, dimension (:), allocatable :: orig_shallst,orig_deepst
      real, dimension (:), allocatable :: orig_pndvol,orig_pndsed,rip_fr
      real, dimension (:), allocatable :: orig_pndno3,orig_pndsolp
      real, dimension (:), allocatable :: orig_pndorgn,orig_pndorgp
      real, dimension (:), allocatable :: orig_wetvol,orig_wetsed
      real, dimension (:), allocatable :: orig_wetno3,orig_wetsolp
      real, dimension (:), allocatable :: orig_wetorgn,orig_wetorgp
      real, dimension (:), allocatable :: orig_solcov,orig_solsw
      real, dimension (:), allocatable :: orig_potno3,orig_potsed
      real, dimension (:), allocatable :: wtab,wtab_mn,wtab_mx
      real, dimension (:), allocatable :: shallst_n,gw_nloss,rchrg_n
      real, dimension (:), allocatable :: det_san, det_sil, det_cla
      real, dimension (:), allocatable :: det_sag, det_lag
      real, dimension (:), allocatable :: tnylda, afrt_surface
      real, dimension (:), allocatable :: auto_nyr, auto_napp
      real, dimension (:), allocatable :: manure_kg, auto_nstrs
      real, dimension (:,:), allocatable :: yldn
      real, dimension (:,:), allocatable :: gwati, gwatn, gwatl
      real, dimension (:,:), allocatable :: gwatw, gwatd, gwatveg
      real, dimension (:,:), allocatable :: gwata, gwats, gwatspcon
      real, dimension (:,:), allocatable :: rfqeo_30d,eo_30d
      real, dimension (:,:), allocatable :: wgncur,wgnold,wrt,psetlp
      real, dimension (:,:), allocatable :: zdb,pst_surq,pst_enr
      real, dimension (:,:), allocatable :: plt_pst,pst_sed,psetlw
      real, dimension (:,:), allocatable :: pcpband,wupnd,tavband,phi
      real, dimension (:,:), allocatable :: wat_phi
      real, dimension (:,:), allocatable :: wushal,wudeep,tmnband,snoeb
      real, dimension (:,:), allocatable :: nsetlw,snotmpeb,bss,surf_bs  
      real, dimension (:,:), allocatable :: tmxband,nsetlp
      real, dimension (:,:), allocatable :: rainsub,hhsubp,frad
      real, dimension (:), allocatable :: rhrbsb, rstpbsb
      real, dimension (:,:), allocatable :: orig_snoeb,orig_pltpst
      real, dimension (:,:), allocatable :: terr_p, terr_cn, terr_sl
      real, dimension (:,:), allocatable :: drain_d, drain_t, drain_g
      real, dimension (:,:), allocatable :: drain_idep
      real, dimension (:,:), allocatable :: cont_cn, cont_p, filt_w   
      real, dimension (:,:), allocatable :: strip_n, strip_cn, strip_c
      real, dimension (:,:), allocatable :: strip_p, fire_cn
      real, dimension (:,:), allocatable :: cropno_upd,hi_upd,laimx_upd
      real, dimension (:,:,:), allocatable :: pst_lag, phug
      integer, dimension (:), allocatable :: ipot,nrelease,swtrg,hrupest
      integer, dimension (:), allocatable :: nro,nrot,nfert,npest
      integer, dimension (:), allocatable :: igro,nair,ipnd1,ipnd2
      integer, dimension (:), allocatable :: nirr,iflod1,iflod2,ndtarg
      integer, dimension (:), allocatable :: phu_plt, iafrttyp
      !! burn
      integer, dimension (:), allocatable :: i_burn, grz_days
      integer, dimension (:), allocatable :: nmgt,icr,ncut,nsweep,nafert
      integer, dimension (:), allocatable :: irn,irrno,sol_nly,npcp
      integer, dimension (:), allocatable :: igrz,ndeat,ngr,ncf
      integer, dimension (:), allocatable :: idorm,urblu,hru_sub,ldrain
      integer, dimension (:), allocatable :: hru_seq
      integer, dimension (:), allocatable :: iurban,iday_fert,icfrt
      integer, dimension (:), allocatable :: ndcfrt,irip,ifld,hrugis
      integer, dimension (:), allocatable :: orig_igro,ntil,irrsc
      integer, dimension (:), allocatable :: iwatable,curyr_mat
      integer, dimension (:), allocatable :: ncpest,icpst,ndcpst
      integer, dimension (:), allocatable :: iday_pest, irr_flag
      integer, dimension (:), allocatable :: irra_flag
      integer, dimension (:,:), allocatable :: rndseed, iterr, iyterr
      integer, dimension (:,:), allocatable :: itdrain, iydrain, ncrops
      integer, dimension (:), allocatable :: manure_id

!!     gsm added for sdr (drainage) 7/24/08
      integer, dimension (:,:), allocatable :: mgt_sdr,idplrot
      integer, dimension (:,:), allocatable :: icont, iycont
      integer, dimension (:,:), allocatable :: ifilt, iyfilt
      integer, dimension (:,:), allocatable :: istrip, iystrip
      integer, dimension (:,:), allocatable :: iopday, iopyr, mgt_ops
      real, dimension (:), allocatable :: wshd_pstap, wshd_pstdg
      integer, dimension (:), allocatable :: ndmo,npno,mcrhru
      character(len=13), dimension (18) :: rfile,tfile
!      character(len=1), dimension (50000) :: hydgrp, kirr  !!for srin's big runs

!     character(len=4), dimension (50) :: urbname
      character(len=4), dimension (1000) :: urbname
!      character(len=16), dimension (50000) :: snam   !! for srin's big runs

      character(len=1), dimension (:), allocatable :: hydgrp, kirr
      character(len=16), dimension (:), allocatable :: snam
      character(len=17), dimension (300) :: pname
!!    adding qtile to output.hru write 3/2/2010 gsm  increased heds(70) to heds(71)
!!    increased hedr(42) to hedr(45) for output.rch gsm 10/17/2011
      character(len=13) :: heds(76),hedb(22),hedr(45),hedrsv(41)
!!      character(len=13) :: heds(73),hedb(21),hedr(42),hedrsv(41)
      character(len=13) :: hedwtr(40)
!     character(len=4) :: title(60), cpnm(250)
      character(len=4) :: title(60), cpnm(5000)
      character(len=17), dimension(50) :: fname
! measured input files
      real, dimension (:,:,:), allocatable :: flomon,solpstmon,srbpstmon
      real, dimension (:,:,:), allocatable :: sedmon,orgnmon,orgpmon
      real, dimension (:,:,:), allocatable :: no3mon,minpmon,nh3mon
      real, dimension (:,:,:), allocatable :: no2mon,bactpmon,bactlpmon
      real, dimension (:,:,:), allocatable :: cmtl1mon,cmtl2mon,cmtl3mon
      real, dimension (:,:,:), allocatable :: chlamon,disoxmon,cbodmon
      real, dimension (:,:), allocatable :: floyr,sedyr,orgnyr,orgpyr
      real, dimension (:,:), allocatable :: no3yr,minpyr,nh3yr,no2yr
      real, dimension (:,:), allocatable :: bactpyr,bactlpyr,cmtl1yr
      real, dimension (:,:), allocatable :: cmtl2yr,cmtl3yr,chlayr
      real, dimension (:,:), allocatable :: disoxyr,cbodyr,solpstyr
      real, dimension (:,:), allocatable :: srbpstyr
	real, dimension (:,:), allocatable :: sol_mc,sol_mn,sol_mp
      real, dimension (:), allocatable :: flocnst,sedcnst,orgncnst
      real, dimension (:), allocatable :: orgpcnst,no3cnst,minpcnst
      real, dimension (:), allocatable :: nh3cnst,no2cnst,bactpcnst
      real, dimension (:), allocatable :: cmtl1cnst,cmtl2cnst,bactlpcnst
      real, dimension (:), allocatable :: cmtl3cnst,chlacnst,disoxcnst
      real, dimension (:), allocatable :: cbodcnst,solpstcnst,srbpstcnst

! hourly time step (by AVG)
      integer :: idt, nstep
      real, dimension (:), allocatable :: hrtwtr,hhstor,hdepth,hsdti
      real, dimension (:), allocatable :: hrchwtr,halgae,horgn,hnh4
      real, dimension (:), allocatable :: hno2,hno3,horgp,hsolp,hbod
      real, dimension (:), allocatable :: hdisox,hchla,hsedyld,hsedst
      real, dimension (:), allocatable :: hharea,hsolpst,hsorpst
      real, dimension (:), allocatable :: hhqday,hhprecip,precipdt
      real, dimension (:), allocatable :: hhtime,hbactp,hbactlp
! store initial values
      integer, dimension (:), allocatable :: ivar_orig
      real, dimension (:), allocatable :: rvar_orig
! Input Uncertainty, added by Ann van Griensven
      integer ::  nauto, nsave
!	integer, dimension (:), allocatable :: iseed
	integer, dimension (:), allocatable :: itelmon, itelyr
       real, dimension (:,:), allocatable :: variimon, variiyr
	integer, dimension (:), allocatable :: itelmons, itelyrs
       real, dimension (:,:), allocatable :: variimons, variiyrs
! additional reach variables , added by Ann van Griensven
        real, dimension (:), allocatable :: wattemp
! Modifications to Pesticide and Water routing routines by Balaji Narasimhan
        real, dimension (:), allocatable :: lkpst_mass, lkspst_mass
        real, dimension (:), allocatable :: vel_chan
!Additional buffer and filter strip variables Mike White
      real, dimension (:), allocatable :: vfscon,vfsratio,vfsch,vfsi
      real, dimension (:,:), allocatable :: filter_i,filter_ratio
      real, dimension (:,:), allocatable :: filter_con,filter_ch
!! sj, june 07 modifications to carbon balance routines
      real, dimension (:,:), allocatable :: sol_n
      integer :: cswat
!! sj, june 07 end

!! sj, dec 07 dynamic bulk density
      real, dimension (:,:), allocatable :: sol_bdp
!! sj dec 07 end

!! Armen Jan 08
      real, dimension (:,:), allocatable :: tillagef
      real, dimension (:), allocatable :: rtfr
      real, dimension (:), allocatable :: stsol_rd
!! Armen Jan 08 end
	integer:: urban_flag, dorm_flag
	real :: bf_flg, iabstr
	real, dimension (:), allocatable :: ubnrunoff,ubntss
	real, dimension (:,:), allocatable :: sub_ubnrunoff,sub_ubntss,
     & ovrlnd_dt	
	real, dimension (:,:,:), allocatable :: hhsurf_bs

!! subdaily erosion modeling by Jaehak Jeong
	integer:: sed_ch,iuh
	real :: eros_spl, rill_mult, eros_expo, sedprev, c_factor
	real :: sig_g, ch_d50, uhalpha, abstinit,abstmax
	real, dimension(:,:), allocatable:: hhsedy, sub_subp_dt
	real, dimension(:,:), allocatable:: sub_hhsedy,sub_atmp
	real, dimension(:), allocatable:: rhy,init_abstrc
	real, dimension(:), allocatable:: dratio, hrtevp, hrttlc
	real, dimension(:,:,:), allocatable:: rchhr
	
!! bmp modeling by jaehak jeong
      character(len=4), dimension(:), allocatable:: lu_nodrain
      integer, dimension(:), allocatable:: bmpdrain
      real, dimension(:), allocatable :: sub_cn2
      !sed-fil
      real, dimension(:), allocatable:: sub_ha_urb,subdr_km,subdr_ickm
      real, dimension(:,:), allocatable:: sf_im,sf_iy,sp_sa,
     &  sp_pvol,sp_pd,sp_sedi,sp_sede,ft_sa,ft_fsa,
     &  ft_dep,ft_h,ft_pd,ft_k,ft_dp,ft_dc,ft_por,
     &  tss_den,ft_alp,sf_fr,sp_qi,sp_k,ft_qpnd,sp_dp,
     &  ft_qsw,ft_qin,ft_qout,ft_sedpnd,sp_bpw,ft_bpw,
     &  ft_sed_cumul,sp_sed_cumul
      integer, dimension(:), allocatable:: num_sf
      integer, dimension(:,:), allocatable:: sf_typ,sf_dim,ft_qfg,
     &  sp_qfg,sf_ptp,ft_fc 
      
      !detention pond
	integer, dimension(:), allocatable :: dtp_subnum,dtp_imo,
     &  dtp_iyr,dtp_numweir,dtp_numstage,dtp_stagdis,
     &  dtp_reltype,dtp_onoff
!! sj & armen changes for SWAT-C
	real, dimension (:), allocatable :: cf, cfh, cfdec
!! sj & armen changes for SWAT-C end
! additional nutrient variables by jeong for montana bitterroot
      real, dimension(:), allocatable :: lat_orgn, lat_orgp 

	integer, dimension(:,:), allocatable :: dtp_weirtype,dtp_weirdim
	
	real, dimension(:), allocatable ::dtp_evrsv,
     &  dtp_inflvol,dtp_totwrwid,dtp_parm,dtp_wdep,dtp_totdep,
     &  dtp_watdepact,dtp_outflow,dtp_totrel,dtp_backoff,dtp_seep_sa,
     &  dtp_evap_sa,dtp_pet_day,dtp_pcpvol,dtp_seepvol,dtp_evapvol,
     &  dtp_flowin,dtp_backup_length,dtp_intcept,dtp_expont,dtp_coef1,
     &  dtp_coef2,dtp_coef3,dtp_ivol,dtp_ised

      integer, dimension (:,:),allocatable :: so_res_flag, ro_bmp_flag
      real, dimension (:,:),allocatable :: sol_watp, sol_solp_pre   
	real, dimension (:,:),allocatable :: psp_store, ssp_store, so_res
	real, dimension (:,:),allocatable :: sol_cal, sol_ph
      integer:: sol_p_model
      integer, dimension (:,:),allocatable :: a_days, b_days
      real, dimension (:), allocatable :: harv_min, fstap, min_res       
      real, dimension (:,:),allocatable :: ro_bmp_sed, ro_bmp_bac
      real, dimension (:,:),allocatable :: ro_bmp_pp, ro_bmp_sp
      real, dimension (:,:),allocatable :: ro_bmp_pn, ro_bmp_sn
      real, dimension (:),allocatable :: bmp_sed, bmp_bac
      real, dimension (:),allocatable :: bmp_pp, bmp_sp
      real, dimension (:),allocatable :: bmp_pn, bmp_sn, bmp_flag       
      real, dimension(:,:), allocatable:: dtp_wdratio,dtp_depweir,
     &  dtp_diaweir,dtp_retperd,dtp_pcpret,dtp_cdis,dtp_flowrate,
     &  dtp_wrwid,dtp_addon
!!    added for manure Armen Jan 2009
 !!     real, dimension (:,:), allocatable :: sol_mc, sol_mn, sol_mp

      !retention irrigation
      real, dimension(:), allocatable:: ri_subkm,ri_totpvol,
     &  irmmdt
      real, dimension(:,:), allocatable:: ri_sed,ri_fr,ri_dim,
     &  ri_im,ri_iy,ri_sa,ri_vol,ri_qi,ri_k,ri_dd,ri_evrsv, 
     &  ri_dep,ri_ndt,ri_pmpvol,ri_sed_cumul,hrnopcp,ri_qloss,
     &  ri_pumpv
      character(len=4), dimension(:,:), allocatable:: ri_nirr
      integer, dimension(:), allocatable:: num_ri,ri_luflg,num_noirr
      
      !wet pond
      integer, dimension(:), allocatable:: wtp_subnum,wtp_onoff,wtp_imo,
     &  wtp_iyr,wtp_dim,wtp_stagdis,wtp_sdtype      
      real, dimension(:), allocatable:: wtp_pvol,wtp_pdepth,wtp_sdslope,
     &  wtp_lenwdth,wtp_extdepth,wtp_hydeff,wtp_evrsv,wtp_sdintc,
     &  wtp_sdexp,wtp_sdc1,wtp_sdc2,wtp_sdc3,wtp_pdia,wtp_plen,
     &  wtp_pmann,wtp_ploss,wtp_k,wtp_dp,wtp_sedi,wtp_sede,wtp_qi 
      end module parm
