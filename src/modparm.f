      module parm
      integer icalen
      real*8 :: prf_bsn
            
      type precip_pet_moving_average                  !!for tropical plant growth  
        integer :: trop = 0                           !!      |1=tropical growth ?moisture driven
        integer :: peren = 0                          !!      |0=annual crop; 1=perennial
        integer :: mce = 0                            !!      |my current element -day in the p/pet arrays 
        integer :: mon_seas = 0                       !!      |0=not monsoon season; 1=in monsoon
        integer :: ndays_mon = 0                      !!      |number of days in the monsoon period 
        integer :: curday_mon = 0                     !!      |current day into the monsoon period
        integer :: ndays = 30                         !!      |number of days for precip/pet moving average
        real :: trig = 0.5                            !!mm/mm |precip/pet ratio to trigger plant/restart
        real :: precip_sum = 0.                       !!mm    |sum of precip during moving average period
        real :: pet_sum = 0.                          !!mm    |sum of pet during moving average period
        real, dimension (:), allocatable :: precip    !!mm    |precip dimensioned by ndays 
        real, dimension (:), allocatable :: pet       !!mm    |pet dimensioned by ndays 
        real :: rto = 0                               !!      |sum of precip/sum of pet
      end type precip_pet_moving_average
      type (precip_pet_moving_average), dimension(:),allocatable :: ppet  !dimensioned by subbasin
          
!!    srin - co2 (EPA)
      real*8 :: co2_x2, co2_x
      
!!    srin wtmp
      real*8, dimension (:), allocatable :: tmp_win1, tmp_win2, 
     & tmp_sum1, tmp_sum2, tmp_spr1, 
     & tmp_spr2, tmp_fal1, tmp_fal2
      integer :: isub_sav
     
      real*8 :: wtmp
      real*8, dimension (12) :: pcpmm        
      real*8, dimension (:), allocatable :: alph_e
      real*8, dimension (:), allocatable :: co_p, surlag, cdn, nperco
      real*8, dimension (:), allocatable :: cmn, phoskd, psp, sdnco
      
!!   change per JGA 8/31/2011 gsm for output.mgt 
      real*8 :: yield, burn_frlb, pst_kg, r2adj_bsn
      real*8 :: yieldgrn, yieldbms, yieldtbr, yieldn, yieldp
      real*8 :: hi_bms, hi_rsd, yieldrsd
!!    arrays for Landscape Transport Capacity 5/28/2009 nadia
      real*8, dimension (:), allocatable :: l_k1, l_k2, l_lambda, l_beta
      real*8, dimension (:), allocatable :: l_gama, l_harea, l_vleng
      real*8, dimension (:), allocatable :: l_vslope, l_ktc
      
!!    arrays for Biofilm variables
      real*8, dimension (:), allocatable :: biofilm_mumax, biofilm_kinv
      real*8, dimension (:), allocatable :: biofilm_klw, biofilm_kla
      real*8, dimension (:), allocatable :: biofilm_cdet, biofilm_bm
      
     
!!    new arrays for routing units
      real*8, dimension (:,:), allocatable :: hru_rufr, daru_km, ru_k
      real*8, dimension (:,:), allocatable :: ru_c, ru_eiq, ru_ovsl, ru_a
      real*8, dimension (:,:), allocatable :: ru_ovs, ru_ktc
      real*8, dimension (:), allocatable :: gwq_ru, qdayout
      integer, dimension (:), allocatable :: ils2, ils2flag
      integer :: iru, mru, irch, isub, idum, mhyd_bsn, ipest, ils_nofig
      integer :: mhru1, isalt
      integer, dimension (:), allocatable :: mhyd1 , irtun

!! septic variables for output.std
      real*8 :: wshd_sepno3, wshd_sepnh3, wshd_seporgn, wshd_sepfon
      real*8 :: wshd_seporgp, wshd_sepfop, wshd_sepsolp, wshd_sepbod
      real*8 :: wshd_sepmm
      integer, dimension (:), allocatable :: isep_hru                
!! septic variables for output.std
      real*8 :: fixco, nfixmx, rsd_covco, vcrit, res_stlr_co
      real*8 :: wshd_sw, wshd_snob, wshd_pndfr, wshd_pndv, wshd_pndsed
      real*8 :: wshd_wetfr, wshd_resfr, wshd_resha, wshd_pndha, percop
      real*8 :: wshd_fminp, wshd_ftotn, wshd_fnh3, wshd_fno3, wshd_forgn
      real*8 :: wshd_forgp, wshd_ftotp, wshd_yldn, wshd_yldp, wshd_fixn
      real*8 :: wshd_pup, wshd_wstrs, wshd_nstrs, wshd_pstrs, wshd_tstrs
      real*8 :: wshd_astrs
      real*8 :: wshd_hmn, wshd_rwn, wshd_hmp, wshd_rmn, wshd_dnit, ffcb
      real*8 :: wshd_rmp, wshd_voln, wshd_nitn, wshd_pas, wshd_pal, wdpq
      real*8 :: wshd_plch, wshd_raino3, ressedc, basno3f, basorgnf, wof_p
      real*8 :: wshd_pinlet, wshd_ptile
      real*8 :: basminpf, basorgpf, sftmp, smtmp, smfmx, smfmn, wgpq
      real*8 :: wshd_resv, wshd_ressed, basno3i, basorgni, basminpi, wdlpq
      real*8 :: basorgpi, peakr, pndsedin, sw_excess, albday, wglpq, wdps
      real*8 :: wtabelo, timp, wt_shall
      real*8 :: sq_rto
      real*8 :: tloss, inflpcp, snomlt, snofall, fixn, qtile, crk, latlyr
      real*8 :: pndloss, wetloss,potloss, lpndloss, lwetloss
      real*8 :: sedrch, fertn, sol_rd, cfertn, cfertp, sepday, bioday
      real*8 :: sepcrk, sepcrktot, fertno3, fertnh3, fertorgn, fertsolp
      real*8 :: fertorgp
      real*8 :: fertp, grazn, grazp, soxy, qdfr, sdti, rtwtr, ressa, wgps
      real*8 :: rttime, rchdep, rtevp, rttlc, da_km, resflwi, wdlps, wglps
      real*8 :: resflwo, respcp, resev, ressep,ressedi,ressedo,dtot,wdprch
      real*8 :: nperco_bsn,pperco_bsn,rsdco,phoskd_bsn,voltot
      real*8 :: volcrmin, msk_x
      real*8 :: uno3d, canev, usle, rcn, surlag_bsn,bactkdq,precipday,wdpf
      real*8 :: thbact, wpq20, wlpq20, wps20, wlps20, bactrop, bactsedp
      real*8 :: bactlchp, bactlchlp, enratio, wetpcp, pndpcp, wetsep, wgpf
      real*8 :: pndsep, wetev, pndev, pndsedo, wetsedo, pndflwi, wetflwi
      real*8 :: pndflwo, wetflwo, wetsedi, da_ha, vpd
      real*8 :: bactrolp, bactsedlp, evrch, evlai, pet_day, ep_day, wdlpf
      real*8 :: snoev, sno3up, adj_pkr, n_updis,p_updis,nactfr,reactw
      real*8 :: sdiegropq, sdiegrolpq, sdiegrops, sdiegrolps, es_day
      real*8 :: sbactrop, sbactrolp, sbactsedp, sbactsedlp, ep_max, wof_lp
      real*8 :: sbactlchp, sbactlchlp, psp_bsn, rchwtr, resuspst, setlpst
      real*8 :: bsprev, bssprev, spadyo, spadyev, spadysp, spadyrfv
      real*8 :: spadyosp
      real*8 :: qday, usle_ei, al5, pndsedc, no3pcp, rcharea, volatpst
      real*8 :: wetsedc, uobw, ubw, uobn, uobp, respesti, wglpf
      real*8 :: snocovmx, snocov1, snocov2, rexp, rcor, lyrtile, lyrtilex
      real*8 :: ai0, ai1, ai2, ai3, ai4, ai5, ai6, rhoq, tfact, sno50cov
      real*8 :: mumax, lambda0, lambda1, lambda2, k_l, k_n, k_p, p_n
      real*8 :: rnum1, autop, auton, etday, hmntl, rwntl, hmptl, rmn2tl
      real*8 :: rmptl,wdntl,cmn_bsn,rmp1tl,roctl,gwseep,revapday,reswtr
      real*8 :: bury, difus, reactb, solpesto, petmeas, wdlprch, wdpres
      real*8 :: sorpesto, spcon_bsn, spexp_bsn, solpesti, sorpesti,wdlpres
      real*8 :: snoprev, swprev, shallstp, deepstp, msk_co1, msk_co2
      real*8 :: ressolpo, resorgno, resorgpo, resno3o, reschlao, resno2o
      real*8 :: resnh3o, qdbank, potpcpmm, potevmm, potsepmm, potflwo
      real*8 :: potsedo, pest_sol, trnsrch, wp20p_plt, bactminp, bactminlp
      real*8 :: wp20lp_plt,cncoef,cdn_bsn,sdnco_bsn,bact_swf,bactmx,bactmin
      real*8 :: chla_subco, tb_adj, cn_froz, dorm_hr, smxco
      real*8 :: depimp_bsn, ddrain_bsn, tdrain_bsn, gdrain_bsn
      real*8 :: rch_san, rch_sil, rch_cla, rch_sag, rch_lag, rch_gra


!!    declare mike van liew variables
      real*8 :: hlife_ngw_bsn, ch_opco_bsn, ch_onco_bsn
      real*8 :: bc1_bsn, bc2_bsn, bc3_bsn, bc4_bsn, rcn_sub_bsn, decr_min
      real*8 :: anion_excl_bsn
!!    delcare mike van liew variables

!    Drainmod tile equations  01/2006
      real*8, dimension (:), allocatable :: wat_tbl,sol_swpwt
      real*8, dimension (:,:), allocatable :: vwt
	  real*8 :: re_bsn, sdrain_bsn, sstmaxd_bsn
	  real*8 :: drain_co_bsn, pc_bsn, latksatf_bsn 
!    Drainmod tile equations  01/2006
      integer :: i_subhw, imgt, idlast, iwtr, ifrttyp, mo_atmo, mo_atmo1
      integer :: ifirstatmo, iyr_atmo, iyr_atmo1, matmo
      integer :: mrg, mch, mcr, mpdb, mcrdb, mfdb, mhru, mhyd, mfcst
      integer :: mnr, myr, mcut, mgr, msubo, mrcho, isubwq, ffcst
      integer :: nhru, isproj, mo, nbyr, immo, nrch, nres, irte, i_mo
      integer :: icode, ihout, inum1, inum2, inum3, inum4, wndsim, ihru
      integer :: inum5, inum6, inum7, inum8, icfac
      integer :: nrgage, ntgage, nrgfil, ntgfil, nrtot, nttot, mrech
      integer :: lao, igropt, npmx, irtpest, curyr, tmpsim, icrk, iihru
!    Drainmod tile equations  01/2006
	integer :: ismax, itdrn, iwtdn, iroutunit, ires_nut
!    Drainmod tile equations  01/2006
      integer :: mtil, mvaro, mrecd, idist, mudb, mrecm, mrecc, iclb
      integer :: mrecy, ipet, nyskip, ideg, ievent, slrsim, iopera
      integer :: id1, idaf, idal, leapyr, mo_chk, rhsim, mstdo
      integer :: ifirsts, ifirsth, ifirstw, nstot, nhtot, nwtot, icst
      integer :: ilog, i, iyr, itotr, iwq, iskip, scenario, ifirstpet
      integer :: itotb,itots,iprp,pcpsim,itoth,nd_30,iops,iphr,isto,isol
      integer :: iscen, fcstyr, fcstday, fcstcycles, subtot, ogen
      integer :: msub, mhruo, mres, mapp, mpst, mlyr, igen, iprint, iida
      integer :: fcstcnt, icn, ised_det, mtran, idtill, motot
      integer, dimension(100) :: ida_lup, iyr_lup
      integer :: no_lup, no_up, nostep
!  routing 5/3/2010 gsm per jga    
! date
      character(len=8) :: date
      character(len=10) :: time
      character(len=5) :: zone
      character(len=80) :: prog
      character(len=13) :: slrfile, wndfile, rhfile, petfile, calfile
      character(len=13) :: atmofile, lucfile
      character(len=13) :: septdb
      character(len=13) :: dpd_file, wpd_file, rib_file, sfb_file,
     &                     lid_file
      integer, dimension (:), allocatable :: ifirstr, idg, ifirsthr
      integer, dimension (:), allocatable :: values, ndays
      integer, dimension (:), allocatable :: ndays_noleap, ndays_leap
!     apex/command output files
      integer :: mapex
      real*8, dimension (:), allocatable :: flodaya, seddaya, orgndaya
      real*8, dimension (:), allocatable :: orgpdaya, no3daya, minpdaya
      real*8, dimension (:), allocatable :: hi_targ, bio_targ, tnyld
      integer, dimension (:), allocatable :: idapa, iypa, ifirsta
      integer, dimension (:), allocatable :: mo_transb, mo_transe
      integer, dimension (:), allocatable :: ih_tran
!     apex/command output files
!  septic inputs
!! septic change added iseptic 1/28/09 gsm
      integer :: msdb, iseptic
      real*8, dimension (:), allocatable :: sptqs,percp
      real*8, dimension (:), allocatable :: sptbodconcs, spttssconcs
      real*8, dimension (:), allocatable :: spttnconcs, sptnh4concs
      real*8, dimension (:), allocatable :: sptno3concs, sptno2concs
      real*8, dimension (:), allocatable :: sptorgnconcs, spttpconcs
      real*8, dimension (:), allocatable :: sptminps, sptorgps      
      real*8, dimension (:), allocatable :: sptfcolis ,failyr,qstemm  
!! septic changes added 1/28/09 gsm
      real*8, dimension (:), allocatable :: bio_amn, bio_bod, biom,rbiom
      real*8, dimension (:), allocatable :: fcoli, bio_ntr, bz_perc
      real*8, dimension (:), allocatable :: plqm,sep_cap,bz_area
      real*8, dimension (:), allocatable :: bz_z, bz_thk,  bio_bd
!! carbon outputs for .hru file
      real*8, dimension (:), allocatable :: cmup_kgh, cmtot_kgh
!! carbon outputs for .hru file
      real*8, dimension (:), allocatable :: coeff_bod_dc, coeff_bod_conv
      real*8, dimension (:), allocatable :: coeff_fc1, coeff_fc2
      real*8, dimension (:), allocatable :: coeff_fecal, coeff_plq
      real*8, dimension (:), allocatable :: coeff_mrt, coeff_rsp
      real*8, dimension (:), allocatable :: coeff_slg1, coeff_slg2
      real*8, dimension (:), allocatable :: coeff_nitr, coeff_denitr
      real*8, dimension (:), allocatable :: coeff_pdistrb,coeff_solpslp 
      real*8, dimension (:), allocatable :: coeff_solpintc,coeff_psorpmax
!! Septic system by Jaehak Jeong
      integer, dimension (:), allocatable :: i_sep,isep_typ
      integer, dimension (:), allocatable :: isep_opt,sep_tsincefail
      integer, dimension (:), allocatable :: isep_tfail,isep_iyr
      integer, dimension (:), allocatable :: sep_strm_dist,sep_den
      
 !!   change per JGA 9/8/2011 gsm for output.mgt 
      real*8, dimension (:), allocatable :: sol_sumno3, sol_sumsolp
      real*8, dimension (:), allocatable :: strsw_sum, strstmp_sum
      real*8, dimension (:), allocatable :: strsn_sum, strsp_sum
      real*8, dimension (:), allocatable :: strsa_sum
      

!! New pothole variables
      real*8, dimension (:), allocatable :: spill_hru,tile_out,hru_in
      real*8, dimension (:), allocatable :: spill_precip,pot_seep
      real*8, dimension (:), allocatable :: pot_evap,pot_sedin
      real*8, dimension (:), allocatable :: pot_solp,pot_solpi
      real*8, dimension (:), allocatable :: pot_orgp,pot_orgpi
      real*8, dimension (:), allocatable :: pot_orgn,pot_orgni
      real*8, dimension (:), allocatable :: pot_mps,pot_mpsi
      real*8, dimension (:), allocatable :: pot_mpa,pot_mpai   
      real*8, dimension (:), allocatable :: pot_no3i,precip_in
      real*8, dimension (:), allocatable :: tile_sedo,tile_no3o
      real*8, dimension (:), allocatable :: tile_solpo,tile_orgno
      real*8, dimension (:), allocatable :: tile_orgpo,tile_minpso   
      real*8, dimension (:), allocatable :: tile_minpao
! output files 
!!  added for binary files 3/25/09 gsm
      integer :: ia_b, ihumus, itemp, isnow
      integer, dimension (:), allocatable :: icolb,icolr,icolrsv,icols
      integer, dimension (:), allocatable :: ipdvar,ipdvab,ipdvas,ipdhru
      real*8, dimension (:), allocatable :: wshddayo,wshdmono,wshdyro
      real*8, dimension (:), allocatable :: wshdaao,fcstaao
      real*8, dimension (:,:), allocatable :: wpstdayo,wpstmono,wpstyro
      real*8, dimension (:,:), allocatable :: yldkg, bio_hv
      real*8, dimension (:,:), allocatable :: wpstaao,rchmono,rchyro
      real*8, dimension (:,:), allocatable :: rchaao,rchdy,hrumono,hruyro
      real*8, dimension (:,:), allocatable :: hruaao,submono,subyro,subaao
      real*8, dimension (:,:), allocatable :: resoutm,resouty,resouta
      real*8, dimension (:,:), allocatable :: wshd_aamon
      real*8, dimension (:,:), allocatable :: wtrmon,wtryr,wtraa 
      real*8, dimension (:,:), allocatable :: sub_smfmx, sub_smfmn
      real*8, dimension (:,:,:), allocatable :: hrupstd,hrupsta,hrupstm
      real*8, dimension (:,:,:), allocatable :: hrupsty
! mrg = max number of rainfall/temperature gages
      integer, dimension (:), allocatable :: ifirstt,ifirstpcp
      integer, dimension (:), allocatable :: elevp,elevt
! mfcst = max number of forecast regions
      real*8, dimension (:,:), allocatable :: ftmpstdmn,ftmpmn,ftmpmx
      real*8, dimension (:,:), allocatable :: ftmpstdmx
      real*8, dimension (:,:,:), allocatable :: fpr_w,fpcp_stat
! mch = max number of channels
      real*8, dimension (:), allocatable :: flwin,flwout,bankst,ch_wi,ch_d
      real*8, dimension (:), allocatable :: ch_onco, ch_opco
      real*8, dimension (:), allocatable :: ch_orgn, ch_orgp
      real*8, dimension (:), allocatable :: drift,rch_dox,rch_bactp
      real*8, dimension (:), allocatable :: alpha_bnk,alpha_bnke
      real*8, dimension (:), allocatable :: disolvp,algae,sedst,rchstor
      real*8, dimension (:), allocatable :: organicn,organicp,chlora
      real*8, dimension (:), allocatable :: nitraten,nitriten,ch_li,ch_si

!      real*8, dimension (:), allocatable :: ch_cov,ch_di,ch_erod,ch_l2
!      real*8, dimension (:), allocatable :: ch_san, ch_sil, ch_cla, ch_veg 
!      real*8, dimension (:), allocatable :: ch_rcur, ch_ss, ch_fpr, ch_eqn
!      real*8, dimension (:), allocatable :: ch_crht

!     Sediment parameters added by Balaji for the new routines

      real*8, dimension (:), allocatable :: ch_bnk_san, ch_bnk_sil
      real*8, dimension (:), allocatable :: ch_bnk_cla, ch_bnk_gra
      real*8, dimension (:), allocatable :: ch_bed_san, ch_bed_sil
      real*8, dimension (:), allocatable :: ch_bed_cla, ch_bed_gra
      real*8, dimension (:), allocatable :: depfp,depsanfp,depsilfp
      real*8, dimension (:), allocatable :: depclafp,depsagfp,deplagfp
      real*8, dimension (:), allocatable :: depch,depsanch,depsilch
      real*8, dimension (:), allocatable :: depclach,depsagch,deplagch
      real*8, dimension (:), allocatable :: depgrach,depgrafp,grast
      real*8, dimension (:), allocatable :: depprch,depprfp,prf,r2adj
      real*8, dimension (:), allocatable :: spcon, spexp
      real*8, dimension (:), allocatable :: sanst,silst,clast,sagst,lagst
      real*8, dimension (:), allocatable :: pot_san,pot_sil,pot_cla
      real*8, dimension (:), allocatable :: pot_sag,pot_lag
      real*8, dimension (:), allocatable :: potsani,potsili,potclai
      real*8, dimension (:), allocatable :: potsagi,potlagi
      real*8, dimension (:), allocatable :: sanyld,silyld,clayld,sagyld
      real*8, dimension (:), allocatable :: lagyld,grayld
      real*8, dimension (:), allocatable :: res_san,res_sil,res_cla
      real*8, dimension (:), allocatable :: res_sag,res_lag,res_gra
      real*8, dimension (:), allocatable :: pnd_san,pnd_sil,pnd_cla
      real*8, dimension (:), allocatable :: pnd_sag,pnd_lag
      real*8, dimension (:), allocatable :: wet_san,wet_sil,wet_cla
      real*8, dimension (:), allocatable :: wet_lag, wet_sag
      real*8 :: ressano,ressilo,resclao,ressago,reslago, resgrao
      real*8 :: ressani, ressili, resclai, ressagi, reslagi,resgrai
      real*8 :: potsano,potsilo,potclao,potsago,potlago
	real*8 :: pndsanin,pndsilin,pndclain,pndsagin,pndlagin
	real*8 :: pndsano,pndsilo,pndclao,pndsago,pndlago

      real*8, dimension (:), allocatable :: ch_di,ch_erod,ch_l2, ch_cov
      real*8, dimension (:), allocatable :: ch_cov1, ch_cov2, ch_bnk_bd
      real*8, dimension (:), allocatable :: ch_bed_bd,ch_bnk_kd,ch_bed_kd
      real*8, dimension (:), allocatable :: ch_bnk_d50, ch_bed_d50     
      real*8, dimension (:), allocatable :: tc_bed,tc_bnk
      integer, dimension (:), allocatable :: ch_eqn
      real*8, dimension (:), allocatable :: chpst_conc,chpst_rea,chpst_vol
      real*8, dimension (:), allocatable :: chpst_koc,chpst_stl,chpst_rsp
      real*8, dimension (:), allocatable :: chpst_mix,sedpst_conc,ch_wdr
      real*8, dimension (:), allocatable :: sedpst_rea,sedpst_bry
      real*8, dimension (:), allocatable :: sedpst_act,rch_cbod,rch_bactlp
      real*8, dimension (:), allocatable :: chside,rs1,rs2,rs3,rs4,rs5
      real*8, dimension (:), allocatable :: rs6,rs7,rk1,rk2,rk3,rk4,rk5
      real*8, dimension (:), allocatable :: rk6,bc1,bc2,bc3,bc4,ammonian
      real*8, dimension (:), allocatable :: orig_sedpstconc
      real*8, dimension (:,:), allocatable :: wurch
      integer, dimension (:), allocatable :: icanal
      integer, dimension (:), allocatable :: itb
! msub = max number of subbasins
      real*8, dimension (:), allocatable :: ch_revap, dep_chan
      real*8, dimension (:), allocatable :: harg_petco, subfr_nowtr
      real*8, dimension (:), allocatable :: cncoef_sub, dr_sub
      real*8, dimension (:), allocatable :: wcklsp,sub_fr,sub_minp,sub_sw
      real*8, dimension (:), allocatable :: sub_sumfc,sub_gwno3,sub_gwsolp
      real*8, dimension (:), allocatable :: sub_km,sub_tc,wlat,sub_pet,co2
      real*8, dimension (:), allocatable :: welev,sub_orgn,sub_orgp,sub_bd
      real*8, dimension (:), allocatable :: sub_wtmp,sub_sedpa,sub_sedps
      real*8, dimension (:), allocatable :: sub_minpa,sub_minps,daylmn
      real*8, dimension (:), allocatable :: latcos,latsin,phutot
      real*8, dimension (:), allocatable :: tlaps,plaps,tmp_an,sub_precip
      real*8, dimension (:), allocatable :: pcpdays, rcn_sub, rammo_sub
      real*8, dimension (:), allocatable :: atmo_day
      real*8, dimension (:), allocatable :: sub_snom,sub_qd,sub_sedy
      real*8, dimension (:), allocatable :: sub_tran,sub_no3,sub_latno3
      real*8, dimension (:,:), allocatable :: sub_smtmp,sub_timp,sub_sftmp
      real*8, dimension (:), allocatable :: sub_tileno3, sub_tilep
      real*8, dimension (:), allocatable :: sub_solp,sub_subp,sub_etday
      real*8, dimension (:), allocatable :: sub_wyld,sub_surfq,sub_elev
      real*8, dimension (:), allocatable :: qird
      real*8, dimension (:), allocatable :: sub_gwq,sub_sep,sub_chl
      real*8, dimension (:), allocatable :: sub_cbod,sub_dox,sub_solpst
      real*8, dimension (:), allocatable :: sub_sorpst,sub_yorgn,sub_yorgp
      real*8, dimension (:), allocatable :: sub_bactp,sub_bactlp,sub_lat
      real*8, dimension (:), allocatable :: sub_latq, sub_gwq_d,sub_tileq
      real*8, dimension (:), allocatable :: sub_vaptile
      real*8, dimension (:), allocatable :: sub_dsan, sub_dsil, sub_dcla
      real*8, dimension (:), allocatable :: sub_dsag, sub_dlag
      
!!!!!! drains
      real*8 :: vap_tile
      real*8, dimension (:), allocatable :: wnan
      real*8, dimension (:,:), allocatable :: sol_stpwt
      real*8, dimension (:,:), allocatable :: sub_pst,sub_hhqd,sub_hhwtmp
      real*8, dimension (:,:), allocatable :: rfinc,tmpinc,radinc,huminc
      real*8, dimension (:,:), allocatable :: wndav,ch_k,elevb,elevb_fr
      real*8, dimension (:,:), allocatable :: dewpt,ch_w,ch_s,ch_n
      real*8, dimension (:,:), allocatable :: amp_r,solarav,tmpstdmx
      real*8, dimension (:,:), allocatable :: tmpstdmn,pcf,tmpmn,tmpmx
      real*8, dimension (:,:), allocatable :: otmpstdmn,otmpmn,otmpmx
      real*8, dimension (:,:), allocatable :: otmpstdmx, ch_erodmo
      real*8, dimension (:,:), allocatable :: uh, hqdsave, hsdsave
      real*8, dimension (:,:,:), allocatable :: pr_w,pcp_stat
      real*8, dimension (:,:,:), allocatable :: opr_w,opcp_stat
      integer, dimension (:), allocatable :: hrutot,hru1,ireg
      integer, dimension (:), allocatable :: isgage,ihgage,iwgage
      integer, dimension (:), allocatable :: irgage,itgage,subgis
      integer, dimension (:), allocatable :: fcst_reg, irelh
! mlyr = max number of soil layers
      real*8, dimension (:,:), allocatable :: sol_aorgn,sol_tmp,sol_fon
      real*8, dimension (:,:), allocatable :: sol_awc,sol_prk,volcr
      real*8, dimension (:,:), allocatable :: pperco_sub
      real*8, dimension (:,:), allocatable :: sol_actp,sol_stap,conv_wt
      real*8, dimension (:,:), allocatable :: sol_solp,sol_ul,sol_fc,crdep
      real*8, dimension (:,:), allocatable :: sol_z,sol_up,sol_bd,sol_st
      real*8, dimension (:,:), allocatable :: flat,sol_nh3,sol_hk,sol_clay
!  added 1/27/09 when making septic changes
      real*8, dimension (:,:), allocatable :: sol_ec 
!  added 1/27/09 when making septic changes
      real*8, dimension (:,:), allocatable :: sol_orgn,sol_por,sol_wp
      real*8, dimension (:,:), allocatable :: sol_orgp,sol_hum,sol_wpmm
      real*8, dimension (:,:), allocatable :: sol_k,sol_cbn,sol_no3
      real*8, dimension (:,:), allocatable :: sol_rsd,sol_fop
      real*8, dimension (:,:), allocatable :: sol_silt, sol_sand, sol_rock
      real*8, dimension (:,:), allocatable :: orig_solno3,orig_solorgn
      real*8, dimension (:,:), allocatable :: orig_solsolp,orig_solorgp
      real*8, dimension (:,:), allocatable :: orig_soltmp,orig_solrsd
      real*8, dimension (:,:), allocatable :: orig_solfop,orig_solfon
      real*8, dimension (:,:), allocatable :: orig_solaorgn,orig_solst
      real*8, dimension (:,:), allocatable :: orig_solactp,orig_solstap
      real*8, dimension (:,:), allocatable :: orig_volcr
!    Drainmod tile equations  01/2006 
	  real*8, dimension (:,:), allocatable :: conk
!    Drainmod tile equations  01/2006
      real*8, dimension (:,:,:), allocatable :: sol_pst,sol_kp
      real*8, dimension (:,:,:), allocatable :: orig_solpst
! mres = max number of reservoirs
      real*8, dimension (:), allocatable :: velsetlr, velsetlp
      real*8, dimension (:), allocatable :: br1,res_k,lkpst_conc, evrsv
      real*8, dimension (:), allocatable :: res_evol,res_pvol,res_vol
      real*8, dimension (:), allocatable :: res_psa,lkpst_rea,lkpst_vol
      real*8, dimension (:), allocatable :: br2,res_rr,res_sed,lkpst_koc
      real*8, dimension (:), allocatable :: lkpst_stl,lkpst_rsp,lkpst_mix
      real*8, dimension (:), allocatable :: lkspst_conc,lkspst_rea
      real*8, dimension (:), allocatable :: theta_n, theta_p, con_nirr
      real*8, dimension (:), allocatable :: con_pirr
      real*8, dimension (:), allocatable :: lkspst_bry,lkspst_act,sed_stlr
      real*8, dimension (:), allocatable :: wurtnf,res_nsed,resdata,chlar
      real*8, dimension (:), allocatable :: res_orgn,res_orgp,res_no3
      real*8, dimension (:), allocatable :: res_solp,res_chla,res_seci
      real*8, dimension (:), allocatable :: res_esa,seccir,res_no2,res_nh3
      real*8, dimension (:), allocatable :: res_bactp, res_bactlp
      real*8, dimension (:), allocatable :: oflowmn_fps, starg_fps
      real*8, dimension (:), allocatable :: weirc, weirk, weirw
      real*8, dimension (:), allocatable :: acoef, bcoef, ccoef
      real*8, dimension (:), allocatable :: orig_resvol,orig_ressed
      real*8, dimension (:), allocatable :: orig_lkpstconc,orig_lkspstconc
      real*8, dimension (:), allocatable :: orig_ressolp,orig_resorgp
      real*8, dimension (:), allocatable :: orig_resno3,orig_resno2
      real*8, dimension (:), allocatable :: orig_resnh3,orig_resorgn
      real*8, dimension (:,:), allocatable :: starg,oflowmx,oflowmn
      real*8, dimension (:,:), allocatable :: psetlr,nsetlr,wuresn
      real*8, dimension (:,:,:), allocatable :: res_out
      integer, dimension (:), allocatable :: ires1,ires2,res_sub
      integer, dimension (:), allocatable :: iresco,mores,iyres
      integer, dimension (:), allocatable :: iflod1r,iflod2r,ndtargr
! mpdb = max number of pesticides in the database
      real*8, dimension (:), allocatable :: skoc,ap_ef,decay_f
      real*8, dimension (:), allocatable :: hlife_f,hlife_s,decay_s
      real*8, dimension (:), allocatable :: pst_wsol,pst_wof, irramt
      real*8, dimension (:), allocatable :: phusw, phusw_nocrop
      integer, dimension (:), allocatable :: nope, pstflg, nop
      integer, dimension (:), allocatable :: yr_skip, isweep
      integer, dimension (:), allocatable :: icrmx, nopmx
! new management scehduling variables
      integer, dimension (:,:), allocatable :: mgtop, idop
      integer, dimension (:,:), allocatable :: mgt1iop,mgt2iop,mgt3iop
      real*8, dimension (:,:), allocatable ::  mgt4op, mgt5op, mgt6op
      real*8, dimension (:,:), allocatable :: mgt7op, mgt8op, mgt9op
      real*8, dimension (:,:), allocatable :: mgt10iop, phu_op
! mcrdb = maximum number of crops in database
      real*8, dimension (:), allocatable :: wac21,wac22,cnyld,rsdco_pl
      real*8, dimension (:), allocatable :: wsyf,leaf1,leaf2,alai_min
      real*8, dimension (:), allocatable :: t_base,t_opt,hvsti,bio_e
      real*8, dimension (:), allocatable :: vpd2,gsi,chtmx,wavp,cvm
      real*8, dimension (:), allocatable :: blai,dlai,rdmx,cpyld,bio_leaf
      real*8, dimension (:), allocatable :: bio_n1,bio_n2,bio_p1,bio_p2
      real*8, dimension (:), allocatable :: bmx_trees,ext_coef,bm_dieoff
      real*8, dimension (:), allocatable :: rsr1, rsr2
!     real*8, dimension (:), allocatable :: air_str
      real*8, dimension (:,:), allocatable :: pltnfr,pltpfr
      integer, dimension (:), allocatable :: idc, mat_yrs
! mfdb = maximum number of fertilizer in database
      real*8, dimension (:), allocatable :: forgn,forgp,fminn,bactpdb
      real*8, dimension (:), allocatable :: fminp,fnh3n,bactlpdb,bactkddb
      character(len=8), dimension (200) :: fertnm
! mudb = maximum number of land types in urban database
      real*8, dimension (:), allocatable :: fimp,curbden,urbcoef,dirtmx
      real*8, dimension (:), allocatable :: thalf,tnconc,tpconc,tno3conc
      real*8, dimension (:), allocatable :: fcimp,urbcn2
! mapp = max number of applications
      real*8 :: sweepeff,frt_kg, pst_dep
!! added pst_dep to statement below 3/31/08 gsm
!!   burn 3/5/09       
! mnr = max number years of rotation
!!   burn 3/5/09  
! mtil = max number tillages in database
 !! drainmod tile equations   06/2006

      real*8, dimension (:), allocatable :: ranrns_hru
	  integer, dimension (:), allocatable :: itill
!! drainmod tile equations   06/2006
      real*8, dimension (:), allocatable :: effmix,deptil, ranrns
      character(len=8), dimension (550) :: tillnm
! mhyd = max number of hydrograph nodes
      real*8, dimension (:), allocatable :: rnum1s,hyd_dakm
      real*8, dimension (:,:), allocatable :: varoute,shyd, vartran
      real*8, dimension (:,:,:), allocatable :: hhvaroute
      integer, dimension (:), allocatable :: icodes,ihouts,inum1s
      integer, dimension (:), allocatable :: inum2s,inum3s,inum4s
      integer, dimension (:), allocatable :: inum5s,inum6s,inum7s,inum8s
      integer, dimension (:), allocatable :: subed
      character(len=10), dimension (:), allocatable :: recmonps
      character(len=10), dimension (:), allocatable :: reccnstps
      character(len=5), dimension (:), allocatable :: subnum
      character(len=4), dimension (:), allocatable :: hruno

! mhru = maximum number of hydrologic response units
      real*8, dimension (:), allocatable :: grwat_n, grwat_i, grwat_l
      real*8, dimension (:), allocatable :: grwat_w, grwat_d
      real*8, dimension (:), allocatable :: grwat_s, grwat_spcon
      real*8, dimension (:), allocatable :: tc_gwat
      real*8, dimension (:), allocatable ::pot_volmm,pot_tilemm,pot_volxmm  !!NUBZ
      real*8, dimension (:), allocatable :: pot_fr,pot_tile,pot_vol,potsa
      real*8, dimension (:), allocatable :: pot_volx,potflwi,potsedi,wfsh
      real*8, dimension (:), allocatable :: pot_nsed,pot_no3l,newrti,gwno3
      real*8, dimension (:), allocatable :: pot_sed,pot_no3,fsred,tmpavp
      real*8, dimension (:), allocatable :: evpot, dis_stream, pot_solpl
      real*8, dimension (:), allocatable :: sed_con, orgn_con, orgp_con
      real*8, dimension (:), allocatable :: soln_con, solp_con, pot_k
      real*8, dimension (:), allocatable :: n_reduc, n_lag, n_ln, n_lnco 
      integer, dimension (:), allocatable :: ioper
      integer, dimension (:), allocatable :: ngrwat
      real*8, dimension (:), allocatable :: filterw,sumix,usle_ls,phuacc
      real*8, dimension (:), allocatable :: esco,epco,slsubbsn,hru_slp
      real*8, dimension (:), allocatable :: erorgn,erorgp,biomix,pnd_seci
      real*8, dimension (:), allocatable :: flowmin,divmax,canmx,usle_p
      real*8, dimension (:), allocatable :: lat_sed,rch_dakm,pnd_no3s,cn1
      real*8, dimension (:), allocatable :: cn2,lat_ttime,flowfr,sol_zmx
      real*8, dimension (:), allocatable :: tile_ttime
      real*8, dimension (:), allocatable :: slsoil,sed_stl,gwminp,sol_cov
      real*8, dimension (:), allocatable :: yldanu,pnd_solp,pnd_no3,ov_n
      real*8, dimension (:), allocatable :: driftco,pnd_orgp,pnd_orgn,cn3
      real*8, dimension (:), allocatable :: twlpnd, twlwet               !!srini pond/wet infiltration to shallow gw storage
      real*8, dimension (:), allocatable :: sol_sumul,pnd_chla,hru_fr
      real*8, dimension (:), allocatable :: bio_ms,sol_alb,strsw,hru_km
      real*8, dimension (:), allocatable :: pnd_fr,pnd_psa,pnd_pvol,pnd_k
      real*8, dimension (:), allocatable :: pnd_esa,pnd_evol,pnd_vol,yldaa
      real*8, dimension (:), allocatable :: pnd_sed,pnd_nsed,strsa,dep_imp
      real*8, dimension (:), allocatable :: evpnd, evwet
      real*8, dimension (:), allocatable :: wet_fr,wet_nsa,wet_nvol,wet_k
      integer, dimension (:), allocatable :: iwetgw, iwetile
      real*8, dimension (:), allocatable :: wet_mxsa,wet_mxvol,wet_vol
      real*8, dimension (:), allocatable :: wet_sed,wet_nsed
      real*8, dimension (:), allocatable :: smx,sci,bp1,bp2
      real*8, dimension (:), allocatable :: bw1,bw2,bactpq
      real*8, dimension (:), allocatable :: bactp_plt,bactlp_plt,cnday
      real*8, dimension (:), allocatable :: bactlpq,auto_eff,sol_sw,secciw
      real*8, dimension (:), allocatable :: bactps,bactlps,tmpav,chlaw
      real*8, dimension (:), allocatable :: subp,sno_hru,hru_ra,wet_orgn
      real*8, dimension (:), allocatable :: tmx,tmn,rsdin,tmp_hi,tmp_lo
      real*8, dimension (:), allocatable :: rwt,olai,usle_k,tconc,hru_rmx
      real*8, dimension (:), allocatable :: usle_cfac,usle_eifac
      real*8, dimension (:), allocatable :: anano3,aird,t_ov,sol_sumfc
      real*8, dimension (:), allocatable :: sol_avpor,usle_mult,wet_orgp
      real*8, dimension (:), allocatable :: aairr,cht,u10,rhd
      real*8, dimension (:), allocatable :: shallirr,deepirr,lai_aamx
      real*8, dimension (:), allocatable :: canstor,ovrlnd,ch_l1,wet_no3
      real*8, dimension (:), allocatable :: irr_mx, auto_wstr
      real*8, dimension (:), allocatable :: cfrt_id, cfrt_kg, cpst_id
      real*8, dimension (:), allocatable :: cpst_kg
      real*8, dimension (:), allocatable :: irr_asq, irr_eff
      real*8, dimension (:), allocatable :: irrsq, irrefm, irrsalt
      real*8, dimension (:), allocatable :: bio_eat, bio_trmp             !!NUBZ
      integer, dimension (:), allocatable :: ifrt_freq,ipst_freq,irr_noa
      integer, dimension (:), allocatable :: irr_sc,irr_no,irr_daymin,irr_daycur
      integer, dimension (:), allocatable :: imp_trig, fert_days,irr_sca
      integer, dimension (:), allocatable :: pest_days, idplt, wstrs_id
      real*8, dimension (:,:), allocatable :: bio_aahv
!    Drainmod tile equations  08/2006 
	  real*8, dimension (:), allocatable :: cumei,cumeira
	  real*8, dimension (:), allocatable :: cumrt, cumrai
!    Drainmod tile equations  08/2006
      real*8, dimension (:), allocatable :: wet_solp,wet_no3s,wet_chla
      real*8, dimension (:), allocatable :: wet_seci,pnd_no3g,pstsol
      real*8, dimension (:), allocatable :: gwht,delay,gw_q,pnd_solpg
      real*8, dimension (:), allocatable :: alpha_bf,alpha_bfe,gw_spyld
      real*8, dimension (:), allocatable :: alpha_bf_d,alpha_bfe_d
      real*8, dimension (:), allocatable :: gw_qdeep
      real*8, dimension (:), allocatable :: gw_delaye,gw_revap,rchrg_dp
      real*8, dimension (:), allocatable :: revapmn,anion_excl,rchrg
      real*8, dimension (:), allocatable :: ffc,bio_min,surqsolp
      real*8, dimension (:), allocatable :: cklsp,deepst,shallst,wet_solpg
      real*8, dimension (:), allocatable :: rchrg_src
      real*8, dimension (:), allocatable :: wet_no3g,sol_avbd,trapeff
      real*8, dimension (:), allocatable :: gwqmn,tdrain,pplnt,snotmp
      real*8, dimension (:), allocatable :: ddrain,gdrain,sol_crk,dayl,brt
!    Drainmod tile equations  01/2006 
	real*8, dimension (:), allocatable ::ddrain_hru,re,sdrain,sstmaxd
	real*8, dimension (:), allocatable :: stmaxd,drain_co,pc,latksatf
!    Drainmod tile equations  01/2006
      real*8, dimension (:), allocatable :: twash,rnd2,rnd3,sol_cnsw,doxq
      real*8, dimension (:), allocatable :: rnd8,rnd9,percn,sol_sumwp
      real*8, dimension (:), allocatable :: tauton,tautop,cbodu,chl_a,qdr
      real*8, dimension (:), allocatable :: tfertn,tfertp,tgrazn,tgrazp
      real*8, dimension (:), allocatable :: latno3,latq,minpgw,no3gw,nplnt
      real*8, dimension (:), allocatable :: tileq, tileno3, tilep
      real*8, dimension (:), allocatable :: sedminpa,sedminps,sedorgn
      real*8, dimension (:), allocatable :: sedorgp,sedyld,sepbtm,strsn
      real*8, dimension (:), allocatable :: strsp,strstmp,surfq,surqno3
      real*8, dimension (:), allocatable :: tcfrtn,tcfrtp,hru_ha,hru_dafr
      real*8, dimension (:), allocatable :: drydep_no3, drydep_nh4
      real*8, dimension (:), allocatable :: phubase,bio_yrms,hvstiadj
      real*8, dimension (:), allocatable :: laimxfr,laiday,chlap,pnd_psed
      real*8, dimension (:), allocatable :: wet_psed,seccip,plantn,plt_et
      real*8, dimension (:), allocatable :: plt_pet,plantp,bio_aams
      real*8, dimension (:), allocatable :: bio_aamx,lai_yrmx,dormhr
      real*8, dimension (:), allocatable :: lat_pst
      real*8, dimension (:), allocatable :: orig_snohru,orig_potvol,fld_fr
      real*8, dimension (:), allocatable :: orig_alai,orig_bioms,pltfr_n
      real*8, dimension (:), allocatable :: orig_phuacc,orig_sumix,pltfr_p
      real*8, dimension (:), allocatable :: orig_phu, phu_plt
      real*8, dimension (:), allocatable :: orig_shallst,orig_deepst
      real*8, dimension (:), allocatable :: orig_pndvol,orig_pndsed,rip_fr
      real*8, dimension (:), allocatable :: orig_pndno3,orig_pndsolp
      real*8, dimension (:), allocatable :: orig_pndorgn,orig_pndorgp
      real*8, dimension (:), allocatable :: orig_wetvol,orig_wetsed
      real*8, dimension (:), allocatable :: orig_wetno3,orig_wetsolp
      real*8, dimension (:), allocatable :: orig_wetorgn,orig_wetorgp
      real*8, dimension (:), allocatable :: orig_solcov,orig_solsw
      real*8, dimension (:), allocatable :: orig_potno3,orig_potsed
      real*8, dimension (:), allocatable :: wtab,wtab_mn,wtab_mx
      real*8, dimension (:), allocatable :: shallst_n,gw_nloss,rchrg_n
      real*8, dimension (:), allocatable :: det_san, det_sil, det_cla
      real*8, dimension (:), allocatable :: det_sag, det_lag
      real*8, dimension (:), allocatable :: tnylda, afrt_surface
      real*8 :: frt_surface
      real*8, dimension (:), allocatable :: auto_nyr, auto_napp
      real*8, dimension (:), allocatable :: manure_kg, auto_nstrs
      real*8, dimension (:,:), allocatable :: rcn_mo, rammo_mo
      real*8, dimension (:,:), allocatable :: drydep_no3_mo, drydep_nh4_mo
      real*8, dimension (:), allocatable :: rcn_d, rammo_d
      real*8, dimension (:), allocatable :: drydep_no3_d, drydep_nh4_d
      real*8, dimension (:,:), allocatable :: yldn
      real*8, dimension (:,:), allocatable :: gwati, gwatn, gwatl
      real*8, dimension (:,:), allocatable :: gwatw, gwatd, gwatveg
      real*8, dimension (:,:), allocatable :: gwata, gwats, gwatspcon
      real*8, dimension (:,:), allocatable :: rfqeo_30d,eo_30d
      real*8, dimension (:,:), allocatable :: wgncur,wgnold,wrt,psetlp
      real*8, dimension (:,:), allocatable :: zdb,pst_surq,pst_enr
      real*8, dimension (:,:), allocatable :: plt_pst,pst_sed,psetlw
      real*8, dimension (:,:), allocatable :: pcpband,wupnd,tavband,phi
      real*8, dimension (:,:), allocatable :: wat_phi
      real*8, dimension (:,:), allocatable :: wushal,wudeep,tmnband,snoeb
      real*8, dimension (:,:), allocatable :: nsetlw,snotmpeb,bss,surf_bs  
      real*8, dimension (:,:), allocatable :: tmxband,nsetlp
      real*8, dimension (:,:), allocatable :: rainsub,frad
      real*8, dimension (:),   allocatable ::  rstpbsb
      real*8, dimension (:,:), allocatable :: orig_snoeb,orig_pltpst
      real*8, dimension (:,:), allocatable :: terr_p, terr_cn, terr_sl
      real*8, dimension (:,:), allocatable :: drain_d, drain_t, drain_g
      real*8, dimension (:,:), allocatable :: drain_idep
      real*8, dimension (:,:), allocatable :: cont_cn, cont_p, filt_w   
      real*8, dimension (:,:), allocatable :: strip_n, strip_cn, strip_c
      real*8, dimension (:,:), allocatable :: strip_p, fire_cn
      real*8, dimension (:,:), allocatable :: cropno_upd,hi_upd,laimx_upd
      real*8, dimension (:,:,:), allocatable :: pst_lag, phug
 !!     integer, dimension (:), allocatable :: ipot,nrelease,swtrg,hrupest
      integer, dimension (:), allocatable :: nrelease,swtrg,hrupest
      integer, dimension (:), allocatable :: nro,nrot,nfert
      integer, dimension (:), allocatable :: igro,nair,ipnd1,ipnd2
      integer, dimension (:), allocatable :: nirr,iflod1,iflod2,ndtarg
      integer, dimension (:), allocatable :: iafrttyp, nstress
      integer, dimension (:), allocatable :: igrotree
      !! burn
      integer, dimension (:), allocatable :: grz_days
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
      real*8, dimension (:), allocatable :: wshd_pstap, wshd_pstdg
      integer, dimension (:), allocatable :: ndmo,npno,mcrhru
      character(len=13), dimension (18) :: rfile,tfile
!!      character(len=1), dimension (50000) :: hydgrp, kirr  !!for srin's big run

!     character(len=4), dimension (50) :: urbname
      character(len=4), dimension (1000) :: urbname
!!      character(len=16), dimension (50000) :: snam   !! for srin's big runs

      character(len=1), dimension (:), allocatable :: hydgrp, kirr
      character(len=16), dimension (:), allocatable :: snam
      character(len=17), dimension (300) :: pname
!!    adding qtile to output.hru write 3/2/2010 gsm  increased heds(70) to heds(71)
      character(len=13) :: heds(79),hedb(24),hedr(58),hedrsv(41)
      character(len=13) :: hedwtr(40)
      character(len=4) :: title(60), cpnm(10000)
      character(len=17), dimension(100) :: fname
! measured input files
      real*8, dimension (:,:,:), allocatable :: flomon,solpstmon,srbpstmon
      real*8, dimension (:,:,:), allocatable :: sedmon,orgnmon,orgpmon
      real*8, dimension (:,:,:), allocatable :: no3mon,minpmon,nh3mon
      real*8, dimension (:,:,:), allocatable :: no2mon,bactpmon,bactlpmon
      real*8, dimension (:,:,:), allocatable :: cmtl1mon,cmtl2mon,cmtl3mon
      real*8, dimension (:,:,:), allocatable :: chlamon,disoxmon,cbodmon
      real*8, dimension (:,:), allocatable :: floyr,sedyr,orgnyr,orgpyr
      real*8, dimension (:,:), allocatable :: no3yr,minpyr,nh3yr,no2yr
      real*8, dimension (:,:), allocatable :: bactpyr,bactlpyr,cmtl1yr
      real*8, dimension (:,:), allocatable :: cmtl2yr,cmtl3yr,chlayr
      real*8, dimension (:,:), allocatable :: disoxyr,cbodyr,solpstyr
      real*8, dimension (:,:), allocatable :: srbpstyr
	real*8, dimension (:,:), allocatable :: sol_mc,sol_mn,sol_mp
      real*8, dimension (:), allocatable :: flocnst,sedcnst,orgncnst
      real*8, dimension (:), allocatable :: orgpcnst,no3cnst,minpcnst
      real*8, dimension (:), allocatable :: nh3cnst,no2cnst,bactpcnst
      real*8, dimension (:), allocatable :: cmtl1cnst,cmtl2cnst,bactlpcnst
      real*8, dimension (:), allocatable :: cmtl3cnst,chlacnst,disoxcnst
      real*8, dimension (:), allocatable :: cbodcnst,solpstcnst,srbpstcnst

! hourly time step (by AVG)
      integer :: idt, nstep
      real*8, dimension (:), allocatable :: hrtwtr,hhstor,hdepth,hsdti
      real*8, dimension (:), allocatable :: hrchwtr,halgae,horgn,hnh4
      real*8, dimension (:), allocatable :: hno2,hno3,horgp,hsolp,hbod
      real*8, dimension (:), allocatable :: hdisox,hchla,hsedyld,hsedst
      real*8, dimension (:), allocatable :: hharea,hsolpst,hsorpst
      real*8, dimension (:), allocatable :: hhqday,precipdt
      real*8, dimension (:), allocatable :: hhtime,hbactp,hbactlp
! store initial values
      integer, dimension (:), allocatable :: ivar_orig
      real*8, dimension (:), allocatable :: rvar_orig
! Input Uncertainty, added by Ann van Griensven
      integer ::  nauto, nsave, iatmodep
! additional reach variables , added by Ann van Griensven
        real*8, dimension (:), allocatable :: wattemp
! Modifications to Pesticide and Water routing routines by Balaji Narasimhan
        real*8, dimension (:), allocatable :: lkpst_mass, lkspst_mass
        real*8, dimension (:), allocatable :: vel_chan
!Additional buffer and filter strip variables Mike White
      real*8, dimension (:), allocatable :: vfscon,vfsratio,vfsch,vfsi
      real*8, dimension (:,:), allocatable :: filter_i,filter_ratio
      real*8, dimension (:,:), allocatable :: filter_con,filter_ch
!! sj, june 07 modifications to carbon balance routines
      real*8, dimension (:,:), allocatable :: sol_n
      integer :: cswat
!! sj, june 07 end

!! sj, dec 07 dynamic bulk density
      real*8, dimension (:,:), allocatable :: sol_bdp
!! sj dec 07 end

!! Armen Jan 08
      real*8, dimension (:,:), allocatable :: tillagef
      real*8, dimension (:), allocatable :: rtfr
      real*8, dimension (:), allocatable :: stsol_rd
!! Armen Jan 08 end
	integer:: urban_flag, dorm_flag
	real*8 :: bf_flg, iabstr
	real*8, dimension (:), allocatable :: ubnrunoff,ubntss
	real*8, dimension (:,:), allocatable :: sub_ubnrunoff,sub_ubntss,
     & ovrlnd_dt	
	real*8, dimension (:,:,:), allocatable :: hhsurf_bs

!! subdaily erosion modeling by Jaehak Jeong
	integer:: sed_ch,iuh
	real*8 :: eros_spl, rill_mult, eros_expo, sedprev, c_factor
	real*8 :: sig_g, ch_d50, uhalpha, abstinit,abstmax
	real*8, dimension(:,:), allocatable:: hhsedy, sub_subp_dt
	real*8, dimension(:,:), allocatable:: sub_hhsedy,sub_atmp
	real*8, dimension(:), allocatable:: rhy,init_abstrc
	real*8, dimension(:), allocatable:: dratio, hrtevp, hrttlc
	real*8, dimension(:,:,:), allocatable:: rchhr
!! subdaily reservoir modeling by Jaehak Jeong
	real*8, dimension(:), allocatable:: hhresflwi, hhresflwo,
     & hhressedi, hhressedo 
	
!! bmp modeling by jaehak jeong
      character(len=4), dimension(:), allocatable:: lu_nodrain
      integer, dimension(:), allocatable:: bmpdrain
      real*8, dimension(:), allocatable :: sub_cn2, sub_ha_urb,
     & bmp_recharge 
      !sed-fil
      real*8, dimension(:), allocatable:: sub_ha_imp,subdr_km,subdr_ickm
      real*8, dimension(:,:), allocatable:: sf_im,sf_iy,sp_sa,
     &  sp_pvol,sp_pd,sp_sedi,sp_sede,ft_sa,ft_fsa,
     &  ft_dep,ft_h,ft_pd,ft_k,ft_dp,ft_dc,ft_por,
     &  tss_den,ft_alp,sf_fr,sp_qi,sp_k,ft_qpnd,sp_dp,
     &  ft_qsw,ft_qin,ft_qout,ft_sedpnd,sp_bpw,ft_bpw,
     &  ft_sed_cumul,sp_sed_cumul
      integer, dimension(:), allocatable:: num_sf
      integer, dimension(:,:), allocatable:: sf_typ,sf_dim,ft_qfg,
     &  sp_qfg,sf_ptp,ft_fc 
      real*8 :: sfsedmean,sfsedstdev  !Jaehak effluent probability method for urban bmp 2017
      
      !detention pond
	integer, dimension(:), allocatable :: dtp_subnum,dtp_imo,
     &  dtp_iyr,dtp_numweir,dtp_numstage,dtp_stagdis,
     &  dtp_reltype,dtp_onoff
!! sj & armen changes for SWAT-C
	real*8, dimension (:), allocatable :: cf, cfh, cfdec
!! sj & armen changes for SWAT-C end
! additional nutrient variables by jeong for montana bitterroot
      real*8, dimension(:), allocatable :: lat_orgn, lat_orgp 

	integer, dimension(:,:), allocatable :: dtp_weirtype,dtp_weirdim
	
	real*8, dimension(:), allocatable ::dtp_evrsv,
     &  dtp_inflvol,dtp_totwrwid,dtp_lwratio,dtp_wdep,dtp_totdep,
     &  dtp_watdepact,dtp_outflow,dtp_totrel,dtp_backoff,dtp_seep_sa,
     &  dtp_evap_sa,dtp_pet_day,dtp_pcpvol,dtp_seepvol,dtp_evapvol,
     &  dtp_flowin,dtp_backup_length,dtp_intcept,dtp_expont,dtp_coef1,
     &  dtp_coef2,dtp_coef3,dtp_dummy1,dtp_dummy2,
     &  dtp_dummy3,dtp_ivol,dtp_ised

      integer, dimension (:,:),allocatable :: so_res_flag, ro_bmp_flag
      real*8, dimension (:,:),allocatable :: sol_watp, sol_solp_pre   
	real*8, dimension (:,:),allocatable :: psp_store, ssp_store, so_res
	real*8, dimension (:,:),allocatable :: sol_cal, sol_ph
      integer:: sol_p_model
      integer, dimension (:,:),allocatable :: a_days, b_days
      real*8, dimension (:), allocatable :: harv_min, fstap, min_res
      real*8, dimension (:,:),allocatable :: ro_bmp_flo, ro_bmp_sed
      real*8, dimension (:,:),allocatable :: ro_bmp_bac
      real*8, dimension (:,:),allocatable :: ro_bmp_pp, ro_bmp_sp
      real*8, dimension (:,:),allocatable :: ro_bmp_pn, ro_bmp_sn

      real*8, dimension (:,:),allocatable :: ro_bmp_flos, ro_bmp_seds
      real*8, dimension (:,:),allocatable :: ro_bmp_bacs
      real*8, dimension (:,:),allocatable :: ro_bmp_pps, ro_bmp_sps
      real*8, dimension (:,:),allocatable :: ro_bmp_pns, ro_bmp_sns

      real*8, dimension (:,:),allocatable :: ro_bmp_flot, ro_bmp_sedt
      real*8, dimension (:,:),allocatable :: ro_bmp_bact
      real*8, dimension (:,:),allocatable :: ro_bmp_ppt, ro_bmp_spt
      real*8, dimension (:,:),allocatable :: ro_bmp_pnt, ro_bmp_snt

      real, dimension (:),allocatable :: bmp_flo, bmp_sed, bmp_bac
      real, dimension (:),allocatable :: bmp_pp, bmp_sp
      real, dimension (:),allocatable :: bmp_pn, bmp_sn, bmp_flag
      
      !! **salt**
      integer, dimension(:), allocatable :: bmp_salt
      real, dimension(:,:),allocatable ::  sub_salt,salt_flag,sub_saltmo
      real, dimension(:,:,:), allocatable :: sro_salt,slt_salt
      real, dimension(:,:,:), allocatable:: gw_salt,tile_salt
      real, dimension(:), allocatable :: saltdr 
      real :: ec_int,ec_slp
      integer :: salt_num
      !! **salt**

      real*8, dimension (:),allocatable :: bmp_flos, bmp_seds, bmp_bacs
      real*8, dimension (:),allocatable :: bmp_pps, bmp_sps
      real*8, dimension (:),allocatable :: bmp_pns, bmp_sns
       
      real*8, dimension (:),allocatable :: bmp_flot, bmp_sedt, bmp_bact
      real*8, dimension (:),allocatable :: bmp_ppt, bmp_spt
      real*8, dimension (:),allocatable :: bmp_pnt, bmp_snt

      real*8, dimension(:,:), allocatable:: dtp_wdratio,dtp_depweir,
     &  dtp_diaweir,dtp_retperd,dtp_pcpret,dtp_cdis,dtp_flowrate,
     &  dtp_wrwid,dtp_addon
!!    added for manure Armen Jan 2009
 !!     real*8, dimension (:,:), allocatable :: sol_mc, sol_mn, sol_mp

      !retention irrigation
      real*8, dimension(:), allocatable:: ri_subkm,ri_totpvol,
     &  irmmdt
      real*8, dimension(:,:), allocatable:: ri_sed,ri_fr,ri_dim,
     &  ri_im,ri_iy,ri_sa,ri_vol,ri_qi,ri_k,ri_dd,ri_evrsv, 
     &  ri_dep,ri_ndt,ri_pmpvol,ri_sed_cumul,hrnopcp,ri_qloss,
     &  ri_pumpv,ri_sedi
      character(len=4), dimension(:,:), allocatable:: ri_nirr
      integer, dimension(:), allocatable:: num_ri,ri_luflg,num_noirr
      
      !wet pond
      integer, dimension(:), allocatable:: wtp_subnum,wtp_onoff,wtp_imo,
     &  wtp_iyr,wtp_dim,wtp_stagdis,wtp_sdtype,nlid      
      real*8, dimension(:), allocatable:: wtp_pvol,wtp_pdepth,
     &  wtp_lenwdth,wtp_extdepth,wtp_hydeff,wtp_evrsv,wtp_sdintc,
     &  wtp_sdexp,wtp_sdc1,wtp_sdc2,wtp_sdc3,wtp_pdia,wtp_plen,
     &  wtp_pmann,wtp_ploss,wtp_k,wtp_dp,wtp_sedi,wtp_sede,wtp_qi,
     &  wtp_sdslope	
     
      real*8 :: bio_init, lai_init, cnop,hi_ovr,harveff,frac_harvk

      ! van Genuchten equation's coefficients
      real*8 :: lid_vgcl,lid_vgcm,lid_qsurf_total,
     & lid_farea_sum
      
      ! soil water content and amount of accumulated infiltration
      real*8, dimension(:,:), allocatable :: lid_cuminf_last,
     & lid_sw_last, interval_last,lid_f_last,lid_cumr_last,lid_str_last,
     & lid_farea,lid_qsurf,lid_sw_add,lid_cumqperc_last,lid_cumirr_last,
     & lid_excum_last,lid_str_curday,lid_qsurf_curday                                                      !! nbs
      
      ! Green Roof
      integer, dimension(:,:), allocatable:: gr_onoff,gr_imo,gr_iyr
      real*8, dimension(:,:), allocatable:: gr_farea,gr_solop,gr_etcoef,
     & gr_fc,gr_wp,gr_ksat,gr_por,gr_hydeff,gr_soldpt,gr_dummy1,
     & gr_dummy2,gr_dummy3,gr_dummy4,gr_dummy5
            
      ! Rain Gerden
      integer, dimension(:,:), allocatable:: rg_onoff,rg_imo,rg_iyr
      real*8, dimension(:,:), allocatable:: rg_farea,rg_solop,rg_etcoef,
     & rg_fc,rg_wp,rg_ksat,rg_por,rg_hydeff,rg_soldpt,rg_dimop,rg_sarea,
     & rg_vol,rg_sth,rg_sdia,rg_bdia,rg_sts,rg_orifice,rg_oheight,
     & rg_odia,rg_dummy1,rg_dummy2,rg_dummy3,rg_dummy4,rg_dummy5
      
      ! CiStern
      integer, dimension(:,:), allocatable:: cs_onoff,cs_imo,cs_iyr,
     & cs_grcon
      real*8, dimension(:,:), allocatable:: cs_farea,cs_vol,cs_rdepth,
     & cs_dummy1,cs_dummy2,cs_dummy3,cs_dummy4,cs_dummy5
      
      ! Porous paVement
      integer, dimension(:,:), allocatable:: pv_onoff,pv_imo,pv_iyr,
     & pv_solop
      real*8, dimension(:,:), allocatable:: pv_grvdep,pv_grvpor,pv_farea,
     & pv_drcoef,pv_fc,pv_wp,pv_ksat,pv_por,pv_hydeff,pv_soldpt,
     & pv_dummy1,pv_dummy2,pv_dummy3,pv_dummy4,pv_dummy5
      
      ! LID general
      integer, dimension(:,:), allocatable:: lid_onoff
      character(len=5), dimension(:,:), allocatable:: lid_lunam
      

!! By Zhang for C/N cycling
      !!SOM-residue C/N state variables -- currently included
	real*8, dimension(:,:), allocatable :: sol_BMC, sol_BMN, sol_HSC, 
     &	sol_HSN, sol_HPC, sol_HPN, sol_LM, 
     &  sol_LMC, sol_LMN, sol_LS, sol_LSL, sol_LSC, sol_LSN	, sol_RNMN,
     &  sol_LSLC, sol_LSLNC, sol_RSPC, sol_WOC, sol_WON, sol_HP, sol_HS,
     &  sol_BM	
      !	HSC mass of C present in slow humus (kg ha-1)
      !	HSN mass of N present in slow humus (kg ha-1)
      !	HPC mass of C present in passive humus (kg ha-1)
      !	HPN mass of N present in passive humus (kg ha-1)
      !	LM mass of metabolic litter (kg ha-1)
      !	LMC mass of C in metabolic litter (kg ha-1)
      !	LMN mass of N in metabolic litter (kg ha-1)
      !	LS mass of structural litter (kg ha-1)
      !	LSC mass of C in structural litter (kg ha-1)
      !	LSL mass of lignin in structural litter (kg ha-1)
      !	LSN mass of N in structural litter (kg ha-1)
       
      !!SOM-residue C/N state variables -- may need to be included
	real*8, dimension(:,:), allocatable :: sol_CAC, sol_CEC  
          
	!!daily updated soil layer associated percolaton and lateral flow Carbon loss
	real*8, dimension(:,:), allocatable :: sol_percc
	real*8, dimension(:,:), allocatable :: sol_latc
	
      !!Daily carbon change by different means (entire soil profile for each HRU)
	real*8, dimension(:), allocatable :: sedc_d, surfqc_d, latc_d,
     &	percc_d, foc_d, NPPC_d, rsdc_d, grainc_d, stoverc_d, soc_d, 
     &  rspc_d, emitc_d 	
	!!emitc_d include biomass_c eaten by grazing, burnt

      
      !!Daily carbon change by different means (entire soil profile for each Subbasin)
      !!Only defined the variables, but not used them in the code
	real*8, dimension(:), allocatable :: sub_sedc_d, sub_surfqc_d, 
     &  sub_latc_d,	sub_percc_d, sub_foc_d, sub_NPPC_d, sub_rsdc_d,
     &  sub_grainc_d, sub_stoverc_d, sub_emitc_d, sub_soc_d, sub_rspc_d
	
	
      !!Monthly carbon change by different means (entire soil profile for each HRU)	
	real*8, dimension(:), allocatable :: sedc_m, surfqc_m, latc_m, percc_m, 
     &  foc_m, NPPC_m, rsdc_m, grainc_m, stoverc_m, emitc_m, soc_m, 
     &  rspc_m	

      !!Yearly carbon change by different means (entire soil profile for each HRU)	
	real*8, dimension(:), allocatable :: sedc_a, surfqc_a, latc_a, 
     &  percc_a, foc_a, NPPC_a, rsdc_a, grainc_a, stoverc_a, emitc_a, 	 
     &  soc_a, rspc_a	

	
      !! The following variables are defined and calculated locally
      !! ==================================================================
      !	HSCTP potential transformation of C in slow humus (kg ha-1 day-1)
      !	HSNTP potential transformation of N in slow humus (kg ha.1 day-1)
      !	HPCTP potential transformation of C in passive humus (kg ha-1 day-1)
      !	HPNTP potential transformation of N in passive humus (kg ha-1 day-1)
      !	HPR rate of transformation of passive humus under optimal conditions (subsurface
      !	layers = 0.000012 day-1) (Parton et al.,1993, 1994)
      !	HSR rate of transformation of slow humus under optimal conditions (all layers
      !	= 0.0005 day.1) (Parton et al., 1993, 1994; Vitousek et al., 1993)
      !	KOC liquid C solid partition coefficient for microbial biomass (10^3 m3 Mg-1)
      !	LMF fraction of the litter that is metabolic
      !	LMNF fraction of metabolic litter that is N (kg kg-1)
      !	LMR rate of transformation of metabolic litter under optimal conditions (surface =
      !	 0.0405 day-1; all other layers = 0.0507 day-1) (Parton et al., 1994)
      !	LMCTP potential transformation of C in metabolic litter (kg ha-1 day-1)
      !	LMNTP potential transformation of N in metabolic litter (kg ha-1 day-1)
      !	LSCTP potential transformation of C in structural litter (kg ha-1 day-1) 
      !	LSF fraction of the litter that is structural
      !	LSLF fraction of structural litter that is lignin (kg kg-1)
      !	LSNF fraction of structural litter that is N (kg kg-1)
      !	LSLCTP potential transformation of C in lignin of structural litter (kg ha-1 day-1)
      !	LSLNCTP potential transformation of C in nonlignin structural litter (kg ha-1 day-1)
      !	LSNTP potential transformation of N in structural litter (kg ha-1 day-1)
      !	LSR rate of potential transformation of structural litter under optimal conditions
      !	(surface = 0.0107 day.1; all other layers = 0.0132 day.1) (Parton et al., 1994)
      !	NCBM N/C ratio of biomass
      !	NCHP N/C ratio passive humus
      !	NCHS N/C ratio of the slow humus
      !	OX oxygen control on biological processes with soil depth
      !	Sf fraction of mineral N sorbed to litter: 0.05 for surface litter, 0.1 for belowground litter

      !!Tillage factor on SOM decomposition
      integer, dimension(:), allocatable :: tillage_switch
      real*8, dimension(:), allocatable :: tillage_depth
      integer, dimension(:), allocatable :: tillage_days
      real*8, dimension(:), allocatable :: tillage_factor
      ! tillage_factor: = 1.6 in 30 days after tillage practices occur; otherwise 1.0;
!! By Zhang for C/N cycling
      
      !Flood routing variables by Jaehak Jeong 2017
      real*8 :: dthy
      integer, dimension(4) :: IHX
      integer, dimension(:), allocatable :: NHY
      real*8, dimension(:), allocatable :: RCHX,RCSS,QCAP,CHXA,CHXP
      real*8, dimension(:,:,:), allocatable :: QHY


      ! tdc 2018-03-29 prototypes for some functions that returned REAL implicitly
      ! but which must now return REAL*8 explicitly
      INTERFACE
      
      function atri(at1,at2,at3,at4i) result (r_atri)
      real*8, intent (in) :: at1, at2, at3
      integer, intent (in out) :: at4i
      real*8 ::r_atri
      end function
      
      real*8 function aunif (x1) result (unif)
      integer, intent (in out) :: x1
      end function

      real*8 function dstn1(rn1,rn2) result (r_dstn1)
      real*8, intent (in) :: rn1, rn2
      end function
            
      real*8 function ee(tk) result (r_ee)
      real*8, intent (in) :: tk
      end
      
	real*8 Function fcgd(xx)
        real*8, intent (in) :: xx
      End function
      
      real*8 function qman(x1,x2,x3,x4) result (r_qman)
      real*8, intent (in) :: x1, x2, x3, x4
      end function
      
      real*8 function regres(k) result (r_regres)
      integer, intent (in) :: k
      end
      
      function tair(hr,jj) result (r_tair)
      integer, intent (in) ::  jj
      real*8, intent(in) :: hr
      real*8 :: r_tair
      end function
      
      real*8 function theta(r20,thk,tmp) result (r_theta)
      real*8, intent (in) :: r20, thk, tmp
      end
      
      subroutine ascrv(x1,x2,x3,x4,x5,x6)
      real*8, intent (in) :: x1, x2, x3, x4
      real*8, intent (out) :: x5, x6
      end subroutine
      
      SUBROUTINE HQDAV(A,CBW,QQ,SSS,ZCH,ZX,CHW,FPW,jrch)
      real*8, intent (in out) :: A, ZX, CHW, FPW
      real*8, intent (in) :: CBW, QQ, SSS, ZCH
      integer, intent (in) :: jrch
      end subroutine
      
      subroutine layersplit(dep_new)
	real*8, intent(in):: dep_new
      end subroutine
      
      subroutine ndenit(k,j,cdg,wdn,void)
	integer :: k,j
	real*8 :: cdg, wdn, void
      end subroutine
      
      subroutine rsedaa(years)
      real*8, intent (in) :: years
      end subroutine
      
      subroutine vbl(evx,spx,pp,qin,ox,vx1,vy,yi,yo,ysx,vf,vyf,aha)
      real*8, intent (in) :: evx, spx, pp, qin, ox, yi, yo, ysx
      real*8, intent (in) :: vf, vyf, aha
      real*8, intent (in out) :: vx1, vy
      end subroutine      
      
      END INTERFACE
      
      end module parm