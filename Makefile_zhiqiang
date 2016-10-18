#Generated using GenerateMakefile by Zhiqiang Yu, hawklorry@gmail.com
#Saturday, February 08, 2014 10:18:22 AM

#Define compiler
CC=gcc
FC=gfortran

#C Flag, remove -Wall if don't want to all the warning information
CFLAG=-c -fmessage-length=0
#Fortran Flag, remove -Wall if don't want to all the warning information
FFLAG=-c -fmessage-length=0 -funderscoring -fbacktrace -ffpe-trap=invalid,zero,overflow
#Dedug Flag
DFLAG=-O0 -g -fbounds-check -Wextra
#Release Flag
RFLAG=-O3
#Flag for long fix fortran codes, used for some special fortran files
LONGFIX=-ffixed-line-length-132
#Flag for long free fortran codes, used for some special fortran files
LONGFREE=-ffree-line-length-200
#Flag for target machine architecture.
#Note: MinGW doesn't support 64-bit architecture. Replace -m64 with empty string instead.
ARCH32=-m32
ARCH64=-m64


OBJECTS_DEBUG32=  debug32/addh.o debug32/albedo.o debug32/allocate_parms.o debug32/alph.o debug32/anfert.o debug32/apex_day.o debug32/apply.o debug32/ascrv.o debug32/atri.o debug32/aunif.o debug32/autoirr.o debug32/aveval.o debug32/bacteria.o debug32/biofilm.o debug32/biozone.o debug32/bmpfixed.o debug32/bmpinit.o debug32/bmp_det_pond.o debug32/bmp_ri_pond.o debug32/bmp_sand_filter.o debug32/bmp_sed_pond.o debug32/bmp_wet_pond.o debug32/buffer.o debug32/burnop.o debug32/canopyint.o debug32/caps.o debug32/carbon_new.o debug32/carbon_zhang2.o debug32/cfactor.o debug32/chkcst.o debug32/clgen.o debug32/clicon.o debug32/command.o debug32/conapply.o debug32/confert.o debug32/crackflow.o debug32/crackvol.o debug32/curno.o debug32/dailycn.o debug32/decay.o debug32/depstor.o debug32/distrib_bmps.o debug32/dormant.o debug32/drains.o debug32/dstn1.o debug32/ee.o debug32/eiusle.o debug32/enrsb.o debug32/erfc.o debug32/estimate_ksat.o debug32/etact.o debug32/etpot.o debug32/expo.o debug32/fert.o debug32/filter.o debug32/filtw.o debug32/finalbal.o debug32/gcycl.o debug32/getallo.o debug32/grass_wway.o debug32/graze.o debug32/grow.o debug32/gwmod.o debug32/gwmod_deep.o debug32/gwnutr.o debug32/gw_no3.o debug32/h2omgt_init.o debug32/harvestop.o debug32/harvgrainop.o debug32/harvkillop.o debug32/header.o debug32/headout.o debug32/hhnoqual.o debug32/hhwatqual.o debug32/hmeas.o debug32/hruaa.o debug32/hruallo.o debug32/hruday.o debug32/hrumon.o debug32/hrupond.o debug32/hrupondhr.o debug32/hruyr.o debug32/hydroinit.o debug32/icl.o debug32/impndaa.o debug32/impndday.o debug32/impndmon.o debug32/impndyr.o debug32/impnd_init.o debug32/irrigate.o debug32/irrsub.o debug32/irr_rch.o debug32/irr_res.o debug32/jdt.o debug32/killop.o debug32/lakeq.o debug32/latsed.o debug32/layersplit.o debug32/lwqdef.o debug32/main.o debug32/ncsed_leach.o debug32/ndenit.o debug32/newtillmix.o debug32/nfix.o debug32/nitvol.o debug32/nlch.o debug32/nminrl.o debug32/noqual.o debug32/npup.o debug32/nrain.o debug32/nup.o debug32/nuts.o debug32/openwth.o debug32/operatn.o debug32/orgn.o debug32/orgncswat.o debug32/origtile.o debug32/ovr_sed.o debug32/percmacro.o debug32/percmain.o debug32/percmicro.o debug32/pestlch.o debug32/pestw.o debug32/pesty.o debug32/pgen.o debug32/pgenhr.o debug32/pkq.o debug32/plantmod.o debug32/plantop.o debug32/pmeas.o debug32/pminrl.o debug32/pminrl2.o debug32/pond.o debug32/pondhr.o debug32/pothole.o debug32/potholehr.o debug32/print_hyd.o debug32/psed.o debug32/qman.o debug32/ran1.o debug32/rchaa.o debug32/rchday.o debug32/rchinit.o debug32/rchmon.o debug32/rchuse.o debug32/rchyr.o debug32/reachout.o debug32/readatmodep.o debug32/readbsn.o debug32/readchm.o debug32/readcnst.o debug32/readfcst.o debug32/readfert.o debug32/readfig.o debug32/readfile.o debug32/readgw.o debug32/readhru.o debug32/readinpt.o debug32/readlup.o debug32/readlwq.o debug32/readmgt.o debug32/readmon.o debug32/readops.o debug32/readpest.o debug32/readplant.o debug32/readpnd.o debug32/readres.o debug32/readrte.o debug32/readru.o debug32/readsdr.o debug32/readsepticbz.o debug32/readseptwq.o debug32/readsno.o debug32/readsol.o debug32/readsub.o debug32/readswq.o debug32/readtill.o debug32/readurban.o debug32/readwgn.o debug32/readwus.o debug32/readwwq.o debug32/readyr.o debug32/reccnst.o debug32/recday.o debug32/rechour.o debug32/recmon.o debug32/recyear.o debug32/regres.o debug32/res.o debug32/resbact.o debug32/resetlu.o debug32/reshr.o debug32/resinit.o debug32/resnut.o debug32/rewind_init.o debug32/rhgen.o debug32/rootfr.o debug32/route.o debug32/routels.o debug32/routeunit.o debug32/routres.o debug32/rsedaa.o debug32/rseday.o debug32/rsedmon.o debug32/rsedyr.o debug32/rtbact.o debug32/rtday.o debug32/rteinit.o debug32/rthmusk.o debug32/rthpest.o debug32/rthr.o debug32/rthsed.o debug32/rtmusk.o debug32/rtout.o debug32/rtpest.o debug32/rtsed.o debug32/rtsed_bagnold.o debug32/rtsed_kodatie.o debug32/rtsed_molinas_wu.o debug32/rtsed_yangsand.o debug32/sat_excess.o debug32/save.o debug32/saveconc.o debug32/schedule_ops.o debug32/sched_mgt.o debug32/simulate.o debug32/sim_initday.o debug32/sim_inityr.o debug32/slrgen.o debug32/smeas.o debug32/snom.o debug32/soil_chem.o debug32/soil_par.o debug32/soil_phys.o debug32/soil_write.o debug32/solp.o debug32/solt.o debug32/std1.o debug32/std2.o debug32/std3.o debug32/stdaa.o debug32/storeinitial.o debug32/structure.o debug32/subaa.o debug32/subbasin.o debug32/subday.o debug32/submon.o debug32/substor.o debug32/subwq.o debug32/subyr.o debug32/sub_subbasin.o debug32/sumhyd.o debug32/sumv.o debug32/surface.o debug32/surfstor.o debug32/surfst_h2o.o debug32/surq_daycn.o debug32/surq_greenampt.o debug32/swbl.o debug32/sweep.o debug32/swu.o debug32/tair.o debug32/tgen.o debug32/theta.o debug32/tillfactor.o debug32/tillmix.o debug32/tmeas.o debug32/tran.o debug32/transfer.o debug32/tstr.o debug32/ttcoef.o debug32/ttcoef_wway.o debug32/urban.o debug32/urbanhr.o debug32/urb_bmp.o debug32/varinit.o debug32/vbl.o debug32/virtual.o debug32/volq.o debug32/washp.o debug32/watbal.o debug32/water_hru.o debug32/watqual.o debug32/watqual2.o debug32/wattable.o debug32/watuse.o debug32/weatgn.o debug32/wetlan.o debug32/wmeas.o debug32/wndgen.o debug32/writea.o debug32/writeaa.o debug32/writed.o debug32/writem.o debug32/xmon.o debug32/ysed.o debug32/zero0.o debug32/zero1.o debug32/zero2.o debug32/zeroini.o debug32/zero_urbn.o

NAMEDEBUG32=swat_debug32
debug32:debug32_mkdir ${NAMEDEBUG32}

debug32_mkdir:
	mkdir -p debug32

${NAMEDEBUG32}: ${OBJECTS_DEBUG32}
	${FC} ${OBJECTS_DEBUG32} ${ARCH32} -static -o ${NAMEDEBUG32}


debug32/addh.o: addh.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  addh.f -o debug32/addh.o -I debug32

debug32/albedo.o: albedo.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  albedo.f -o debug32/albedo.o -I debug32

debug32/allocate_parms.o: allocate_parms.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  allocate_parms.f -o debug32/allocate_parms.o -I debug32

debug32/alph.o: alph.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  alph.f -o debug32/alph.o -I debug32

debug32/anfert.o: anfert.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  anfert.f -o debug32/anfert.o -I debug32

debug32/apex_day.o: apex_day.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  apex_day.f -o debug32/apex_day.o -I debug32

debug32/apply.o: apply.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  apply.f -o debug32/apply.o -I debug32

debug32/ascrv.o: ascrv.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  ascrv.f -o debug32/ascrv.o -I debug32

debug32/atri.o: atri.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  atri.f -o debug32/atri.o -I debug32

debug32/aunif.o: aunif.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  aunif.f -o debug32/aunif.o -I debug32

debug32/autoirr.o: autoirr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  autoirr.f -o debug32/autoirr.o -I debug32

debug32/aveval.o: aveval.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  aveval.f -o debug32/aveval.o -I debug32

debug32/bacteria.o: bacteria.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  bacteria.f -o debug32/bacteria.o -I debug32

debug32/biofilm.o: biofilm.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  biofilm.f -o debug32/biofilm.o -I debug32

debug32/biozone.o: biozone.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG} ${LONGFIX} biozone.f -o debug32/biozone.o -I debug32

debug32/bmpfixed.o: bmpfixed.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  bmpfixed.f -o debug32/bmpfixed.o -I debug32

debug32/bmpinit.o: bmpinit.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG} ${LONGFIX} bmpinit.f -o debug32/bmpinit.o -I debug32

debug32/bmp_det_pond.o: bmp_det_pond.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  bmp_det_pond.f -o debug32/bmp_det_pond.o -I debug32

debug32/bmp_ri_pond.o: bmp_ri_pond.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  bmp_ri_pond.f -o debug32/bmp_ri_pond.o -I debug32

debug32/bmp_sand_filter.o: bmp_sand_filter.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  bmp_sand_filter.f -o debug32/bmp_sand_filter.o -I debug32

debug32/bmp_sed_pond.o: bmp_sed_pond.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  bmp_sed_pond.f -o debug32/bmp_sed_pond.o -I debug32

debug32/bmp_wet_pond.o: bmp_wet_pond.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  bmp_wet_pond.f -o debug32/bmp_wet_pond.o -I debug32

debug32/buffer.o: buffer.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  buffer.f -o debug32/buffer.o -I debug32

debug32/burnop.o: burnop.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  burnop.f -o debug32/burnop.o -I debug32

debug32/canopyint.o: canopyint.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  canopyint.f -o debug32/canopyint.o -I debug32

debug32/caps.o: caps.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  caps.f -o debug32/caps.o -I debug32

debug32/carbon_new.o: carbon_new.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  carbon_new.f -o debug32/carbon_new.o -I debug32

debug32/carbon_zhang2.o: carbon_zhang2.f90 debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG} ${LONGFREE} carbon_zhang2.f90 -o debug32/carbon_zhang2.o -I debug32

debug32/cfactor.o: cfactor.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  cfactor.f -o debug32/cfactor.o -I debug32

debug32/chkcst.o: chkcst.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  chkcst.f -o debug32/chkcst.o -I debug32

debug32/clgen.o: clgen.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  clgen.f -o debug32/clgen.o -I debug32

debug32/clicon.o: clicon.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  clicon.f -o debug32/clicon.o -I debug32

debug32/command.o: command.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  command.f -o debug32/command.o -I debug32

debug32/conapply.o: conapply.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  conapply.f -o debug32/conapply.o -I debug32

debug32/confert.o: confert.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  confert.f -o debug32/confert.o -I debug32

debug32/crackflow.o: crackflow.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  crackflow.f -o debug32/crackflow.o -I debug32

debug32/crackvol.o: crackvol.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  crackvol.f -o debug32/crackvol.o -I debug32

debug32/curno.o: curno.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  curno.f -o debug32/curno.o -I debug32

debug32/dailycn.o: dailycn.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  dailycn.f -o debug32/dailycn.o -I debug32

debug32/decay.o: decay.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  decay.f -o debug32/decay.o -I debug32

debug32/depstor.o: depstor.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  depstor.f -o debug32/depstor.o -I debug32

debug32/distrib_bmps.o: distrib_bmps.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  distrib_bmps.f -o debug32/distrib_bmps.o -I debug32

debug32/dormant.o: dormant.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  dormant.f -o debug32/dormant.o -I debug32

debug32/drains.o: drains.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  drains.f -o debug32/drains.o -I debug32

debug32/dstn1.o: dstn1.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  dstn1.f -o debug32/dstn1.o -I debug32

debug32/ee.o: ee.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  ee.f -o debug32/ee.o -I debug32

debug32/eiusle.o: eiusle.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  eiusle.f -o debug32/eiusle.o -I debug32

debug32/enrsb.o: enrsb.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  enrsb.f -o debug32/enrsb.o -I debug32

debug32/erfc.o: erfc.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  erfc.f -o debug32/erfc.o -I debug32

debug32/estimate_ksat.o: estimate_ksat.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  estimate_ksat.f -o debug32/estimate_ksat.o -I debug32

debug32/etact.o: etact.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  etact.f -o debug32/etact.o -I debug32

debug32/etpot.o: etpot.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  etpot.f -o debug32/etpot.o -I debug32

debug32/expo.o: expo.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  expo.f -o debug32/expo.o -I debug32

debug32/fert.o: fert.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  fert.f -o debug32/fert.o -I debug32

debug32/filter.o: filter.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  filter.f -o debug32/filter.o -I debug32

debug32/filtw.o: filtw.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  filtw.f -o debug32/filtw.o -I debug32

debug32/finalbal.o: finalbal.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  finalbal.f -o debug32/finalbal.o -I debug32

debug32/gcycl.o: gcycl.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  gcycl.f -o debug32/gcycl.o -I debug32

debug32/getallo.o: getallo.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  getallo.f -o debug32/getallo.o -I debug32

debug32/grass_wway.o: grass_wway.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  grass_wway.f -o debug32/grass_wway.o -I debug32

debug32/graze.o: graze.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  graze.f -o debug32/graze.o -I debug32

debug32/grow.o: grow.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  grow.f -o debug32/grow.o -I debug32

debug32/gwmod.o: gwmod.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  gwmod.f -o debug32/gwmod.o -I debug32

debug32/gwmod_deep.o: gwmod_deep.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  gwmod_deep.f -o debug32/gwmod_deep.o -I debug32

debug32/gwnutr.o: gwnutr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  gwnutr.f -o debug32/gwnutr.o -I debug32

debug32/gw_no3.o: gw_no3.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  gw_no3.f -o debug32/gw_no3.o -I debug32

debug32/h2omgt_init.o: h2omgt_init.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  h2omgt_init.f -o debug32/h2omgt_init.o -I debug32

debug32/harvestop.o: harvestop.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  harvestop.f -o debug32/harvestop.o -I debug32

debug32/harvgrainop.o: harvgrainop.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  harvgrainop.f -o debug32/harvgrainop.o -I debug32

debug32/harvkillop.o: harvkillop.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  harvkillop.f -o debug32/harvkillop.o -I debug32

debug32/header.o: header.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  header.f -o debug32/header.o -I debug32

debug32/headout.o: headout.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  headout.f -o debug32/headout.o -I debug32

debug32/hhnoqual.o: hhnoqual.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hhnoqual.f -o debug32/hhnoqual.o -I debug32

debug32/hhwatqual.o: hhwatqual.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hhwatqual.f -o debug32/hhwatqual.o -I debug32

debug32/hmeas.o: hmeas.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hmeas.f -o debug32/hmeas.o -I debug32

debug32/hruaa.o: hruaa.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hruaa.f -o debug32/hruaa.o -I debug32

debug32/hruallo.o: hruallo.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hruallo.f -o debug32/hruallo.o -I debug32

debug32/hruday.o: hruday.f90 debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hruday.f90 -o debug32/hruday.o -I debug32

debug32/hrumon.o: hrumon.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hrumon.f -o debug32/hrumon.o -I debug32

debug32/hrupond.o: hrupond.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hrupond.f -o debug32/hrupond.o -I debug32

debug32/hrupondhr.o: hrupondhr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hrupondhr.f -o debug32/hrupondhr.o -I debug32

debug32/hruyr.o: hruyr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hruyr.f -o debug32/hruyr.o -I debug32

debug32/hydroinit.o: hydroinit.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  hydroinit.f -o debug32/hydroinit.o -I debug32

debug32/icl.o: icl.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  icl.f -o debug32/icl.o -I debug32

debug32/impndaa.o: impndaa.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  impndaa.f -o debug32/impndaa.o -I debug32

debug32/impndday.o: impndday.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  impndday.f -o debug32/impndday.o -I debug32

debug32/impndmon.o: impndmon.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  impndmon.f -o debug32/impndmon.o -I debug32

debug32/impndyr.o: impndyr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  impndyr.f -o debug32/impndyr.o -I debug32

debug32/impnd_init.o: impnd_init.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  impnd_init.f -o debug32/impnd_init.o -I debug32

debug32/irrigate.o: irrigate.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  irrigate.f -o debug32/irrigate.o -I debug32

debug32/irrsub.o: irrsub.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  irrsub.f -o debug32/irrsub.o -I debug32

debug32/irr_rch.o: irr_rch.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  irr_rch.f -o debug32/irr_rch.o -I debug32

debug32/irr_res.o: irr_res.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  irr_res.f -o debug32/irr_res.o -I debug32

debug32/jdt.o: jdt.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  jdt.f -o debug32/jdt.o -I debug32

debug32/killop.o: killop.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  killop.f -o debug32/killop.o -I debug32

debug32/lakeq.o: lakeq.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  lakeq.f -o debug32/lakeq.o -I debug32

debug32/latsed.o: latsed.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  latsed.f -o debug32/latsed.o -I debug32

debug32/layersplit.o: layersplit.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  layersplit.f -o debug32/layersplit.o -I debug32

debug32/lwqdef.o: lwqdef.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  lwqdef.f -o debug32/lwqdef.o -I debug32

debug32/main.o: main.f modparm.f
	${FC} ${ARCH32} ${FFLAG} ${DFLAG} ${LONGFIX} main.f -o debug32/main.o -J debug32

debug32/ncsed_leach.o: NCsed_leach.f90 debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  NCsed_leach.f90 -o debug32/ncsed_leach.o -I debug32

debug32/ndenit.o: ndenit.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  ndenit.f -o debug32/ndenit.o -I debug32

debug32/newtillmix.o: newtillmix.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  newtillmix.f -o debug32/newtillmix.o -I debug32

debug32/nfix.o: nfix.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  nfix.f -o debug32/nfix.o -I debug32

debug32/nitvol.o: nitvol.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  nitvol.f -o debug32/nitvol.o -I debug32

debug32/nlch.o: nlch.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  nlch.f -o debug32/nlch.o -I debug32

debug32/nminrl.o: nminrl.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  nminrl.f -o debug32/nminrl.o -I debug32

debug32/noqual.o: noqual.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  noqual.f -o debug32/noqual.o -I debug32

debug32/npup.o: npup.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  npup.f -o debug32/npup.o -I debug32

debug32/nrain.o: nrain.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  nrain.f -o debug32/nrain.o -I debug32

debug32/nup.o: nup.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  nup.f -o debug32/nup.o -I debug32

debug32/nuts.o: nuts.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  nuts.f -o debug32/nuts.o -I debug32

debug32/openwth.o: openwth.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  openwth.f -o debug32/openwth.o -I debug32

debug32/operatn.o: operatn.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  operatn.f -o debug32/operatn.o -I debug32

debug32/orgn.o: orgn.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  orgn.f -o debug32/orgn.o -I debug32

debug32/orgncswat.o: orgncswat.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  orgncswat.f -o debug32/orgncswat.o -I debug32

debug32/origtile.o: origtile.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  origtile.f -o debug32/origtile.o -I debug32

debug32/ovr_sed.o: ovr_sed.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG} ${LONGFIX} ovr_sed.f -o debug32/ovr_sed.o -I debug32

debug32/percmacro.o: percmacro.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  percmacro.f -o debug32/percmacro.o -I debug32

debug32/percmain.o: percmain.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG} ${LONGFIX} percmain.f -o debug32/percmain.o -I debug32

debug32/percmicro.o: percmicro.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  percmicro.f -o debug32/percmicro.o -I debug32

debug32/pestlch.o: pestlch.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pestlch.f -o debug32/pestlch.o -I debug32

debug32/pestw.o: pestw.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pestw.f -o debug32/pestw.o -I debug32

debug32/pesty.o: pesty.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pesty.f -o debug32/pesty.o -I debug32

debug32/pgen.o: pgen.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pgen.f -o debug32/pgen.o -I debug32

debug32/pgenhr.o: pgenhr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pgenhr.f -o debug32/pgenhr.o -I debug32

debug32/pkq.o: pkq.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pkq.f -o debug32/pkq.o -I debug32

debug32/plantmod.o: plantmod.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  plantmod.f -o debug32/plantmod.o -I debug32

debug32/plantop.o: plantop.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  plantop.f -o debug32/plantop.o -I debug32

debug32/pmeas.o: pmeas.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pmeas.f -o debug32/pmeas.o -I debug32

debug32/pminrl.o: pminrl.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pminrl.f -o debug32/pminrl.o -I debug32

debug32/pminrl2.o: pminrl2.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pminrl2.f -o debug32/pminrl2.o -I debug32

debug32/pond.o: pond.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pond.f -o debug32/pond.o -I debug32

debug32/pondhr.o: pondhr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pondhr.f -o debug32/pondhr.o -I debug32

debug32/pothole.o: pothole.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  pothole.f -o debug32/pothole.o -I debug32

debug32/potholehr.o: potholehr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  potholehr.f -o debug32/potholehr.o -I debug32

debug32/print_hyd.o: print_hyd.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  print_hyd.f -o debug32/print_hyd.o -I debug32

debug32/psed.o: psed.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  psed.f -o debug32/psed.o -I debug32

debug32/qman.o: qman.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  qman.f -o debug32/qman.o -I debug32

debug32/ran1.o: ran1.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  ran1.f -o debug32/ran1.o -I debug32

debug32/rchaa.o: rchaa.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rchaa.f -o debug32/rchaa.o -I debug32

debug32/rchday.o: rchday.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rchday.f -o debug32/rchday.o -I debug32

debug32/rchinit.o: rchinit.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rchinit.f -o debug32/rchinit.o -I debug32

debug32/rchmon.o: rchmon.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rchmon.f -o debug32/rchmon.o -I debug32

debug32/rchuse.o: rchuse.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rchuse.f -o debug32/rchuse.o -I debug32

debug32/rchyr.o: rchyr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rchyr.f -o debug32/rchyr.o -I debug32

debug32/reachout.o: reachout.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  reachout.f -o debug32/reachout.o -I debug32

debug32/readatmodep.o: readatmodep.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readatmodep.f -o debug32/readatmodep.o -I debug32

debug32/readbsn.o: readbsn.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readbsn.f -o debug32/readbsn.o -I debug32

debug32/readchm.o: readchm.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readchm.f -o debug32/readchm.o -I debug32

debug32/readcnst.o: readcnst.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readcnst.f -o debug32/readcnst.o -I debug32

debug32/readfcst.o: readfcst.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readfcst.f -o debug32/readfcst.o -I debug32

debug32/readfert.o: readfert.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readfert.f -o debug32/readfert.o -I debug32

debug32/readfig.o: readfig.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readfig.f -o debug32/readfig.o -I debug32

debug32/readfile.o: readfile.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readfile.f -o debug32/readfile.o -I debug32

debug32/readgw.o: readgw.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readgw.f -o debug32/readgw.o -I debug32

debug32/readhru.o: readhru.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readhru.f -o debug32/readhru.o -I debug32

debug32/readinpt.o: readinpt.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readinpt.f -o debug32/readinpt.o -I debug32

debug32/readlup.o: readlup.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readlup.f -o debug32/readlup.o -I debug32

debug32/readlwq.o: readlwq.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readlwq.f -o debug32/readlwq.o -I debug32

debug32/readmgt.o: readmgt.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readmgt.f -o debug32/readmgt.o -I debug32

debug32/readmon.o: readmon.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readmon.f -o debug32/readmon.o -I debug32

debug32/readops.o: readops.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readops.f -o debug32/readops.o -I debug32

debug32/readpest.o: readpest.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readpest.f -o debug32/readpest.o -I debug32

debug32/readplant.o: readplant.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readplant.f -o debug32/readplant.o -I debug32

debug32/readpnd.o: readpnd.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readpnd.f -o debug32/readpnd.o -I debug32

debug32/readres.o: readres.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readres.f -o debug32/readres.o -I debug32

debug32/readrte.o: readrte.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readrte.f -o debug32/readrte.o -I debug32

debug32/readru.o: readru.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readru.f -o debug32/readru.o -I debug32

debug32/readsdr.o: readsdr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readsdr.f -o debug32/readsdr.o -I debug32

debug32/readsepticbz.o: readsepticbz.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readsepticbz.f -o debug32/readsepticbz.o -I debug32

debug32/readseptwq.o: readseptwq.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readseptwq.f -o debug32/readseptwq.o -I debug32

debug32/readsno.o: readsno.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readsno.f -o debug32/readsno.o -I debug32

debug32/readsol.o: readsol.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readsol.f -o debug32/readsol.o -I debug32

debug32/readsub.o: readsub.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readsub.f -o debug32/readsub.o -I debug32

debug32/readswq.o: readswq.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readswq.f -o debug32/readswq.o -I debug32

debug32/readtill.o: readtill.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readtill.f -o debug32/readtill.o -I debug32

debug32/readurban.o: readurban.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readurban.f -o debug32/readurban.o -I debug32

debug32/readwgn.o: readwgn.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readwgn.f -o debug32/readwgn.o -I debug32

debug32/readwus.o: readwus.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readwus.f -o debug32/readwus.o -I debug32

debug32/readwwq.o: readwwq.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readwwq.f -o debug32/readwwq.o -I debug32

debug32/readyr.o: readyr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  readyr.f -o debug32/readyr.o -I debug32

debug32/reccnst.o: reccnst.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  reccnst.f -o debug32/reccnst.o -I debug32

debug32/recday.o: recday.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  recday.f -o debug32/recday.o -I debug32

debug32/rechour.o: rechour.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rechour.f -o debug32/rechour.o -I debug32

debug32/recmon.o: recmon.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  recmon.f -o debug32/recmon.o -I debug32

debug32/recyear.o: recyear.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  recyear.f -o debug32/recyear.o -I debug32

debug32/regres.o: regres.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  regres.f -o debug32/regres.o -I debug32

debug32/res.o: res.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  res.f -o debug32/res.o -I debug32

debug32/resbact.o: resbact.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  resbact.f -o debug32/resbact.o -I debug32

debug32/resetlu.o: resetlu.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  resetlu.f -o debug32/resetlu.o -I debug32

debug32/reshr.o: reshr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  reshr.f -o debug32/reshr.o -I debug32

debug32/resinit.o: resinit.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  resinit.f -o debug32/resinit.o -I debug32

debug32/resnut.o: resnut.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  resnut.f -o debug32/resnut.o -I debug32

debug32/rewind_init.o: rewind_init.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rewind_init.f -o debug32/rewind_init.o -I debug32

debug32/rhgen.o: rhgen.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rhgen.f -o debug32/rhgen.o -I debug32

debug32/rootfr.o: rootfr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rootfr.f -o debug32/rootfr.o -I debug32

debug32/route.o: route.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  route.f -o debug32/route.o -I debug32

debug32/routels.o: routels.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  routels.f -o debug32/routels.o -I debug32

debug32/routeunit.o: routeunit.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  routeunit.f -o debug32/routeunit.o -I debug32

debug32/routres.o: routres.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  routres.f -o debug32/routres.o -I debug32

debug32/rsedaa.o: rsedaa.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rsedaa.f -o debug32/rsedaa.o -I debug32

debug32/rseday.o: rseday.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rseday.f -o debug32/rseday.o -I debug32

debug32/rsedmon.o: rsedmon.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rsedmon.f -o debug32/rsedmon.o -I debug32

debug32/rsedyr.o: rsedyr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rsedyr.f -o debug32/rsedyr.o -I debug32

debug32/rtbact.o: rtbact.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtbact.f -o debug32/rtbact.o -I debug32

debug32/rtday.o: rtday.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtday.f -o debug32/rtday.o -I debug32

debug32/rteinit.o: rteinit.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rteinit.f -o debug32/rteinit.o -I debug32

debug32/rthmusk.o: rthmusk.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rthmusk.f -o debug32/rthmusk.o -I debug32

debug32/rthpest.o: rthpest.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rthpest.f -o debug32/rthpest.o -I debug32

debug32/rthr.o: rthr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rthr.f -o debug32/rthr.o -I debug32

debug32/rthsed.o: rthsed.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG} ${LONGFIX} rthsed.f -o debug32/rthsed.o -I debug32

debug32/rtmusk.o: rtmusk.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtmusk.f -o debug32/rtmusk.o -I debug32

debug32/rtout.o: rtout.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtout.f -o debug32/rtout.o -I debug32

debug32/rtpest.o: rtpest.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtpest.f -o debug32/rtpest.o -I debug32

debug32/rtsed.o: rtsed.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtsed.f -o debug32/rtsed.o -I debug32

debug32/rtsed_bagnold.o: rtsed_bagnold.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtsed_bagnold.f -o debug32/rtsed_bagnold.o -I debug32

debug32/rtsed_kodatie.o: rtsed_kodatie.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtsed_kodatie.f -o debug32/rtsed_kodatie.o -I debug32

debug32/rtsed_molinas_wu.o: rtsed_Molinas_Wu.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtsed_Molinas_Wu.f -o debug32/rtsed_molinas_wu.o -I debug32

debug32/rtsed_yangsand.o: rtsed_yangsand.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  rtsed_yangsand.f -o debug32/rtsed_yangsand.o -I debug32

debug32/sat_excess.o: sat_excess.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  sat_excess.f -o debug32/sat_excess.o -I debug32

debug32/save.o: save.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  save.f -o debug32/save.o -I debug32

debug32/saveconc.o: saveconc.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  saveconc.f -o debug32/saveconc.o -I debug32

debug32/schedule_ops.o: schedule_ops.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  schedule_ops.f -o debug32/schedule_ops.o -I debug32

debug32/sched_mgt.o: sched_mgt.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  sched_mgt.f -o debug32/sched_mgt.o -I debug32

debug32/simulate.o: simulate.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  simulate.f -o debug32/simulate.o -I debug32

debug32/sim_initday.o: sim_initday.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  sim_initday.f -o debug32/sim_initday.o -I debug32

debug32/sim_inityr.o: sim_inityr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  sim_inityr.f -o debug32/sim_inityr.o -I debug32

debug32/slrgen.o: slrgen.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  slrgen.f -o debug32/slrgen.o -I debug32

debug32/smeas.o: smeas.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  smeas.f -o debug32/smeas.o -I debug32

debug32/snom.o: snom.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  snom.f -o debug32/snom.o -I debug32

debug32/soil_chem.o: soil_chem.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  soil_chem.f -o debug32/soil_chem.o -I debug32

debug32/soil_par.o: soil_par.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  soil_par.f -o debug32/soil_par.o -I debug32

debug32/soil_phys.o: soil_phys.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  soil_phys.f -o debug32/soil_phys.o -I debug32

debug32/soil_write.o: soil_write.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  soil_write.f -o debug32/soil_write.o -I debug32

debug32/solp.o: solp.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  solp.f -o debug32/solp.o -I debug32

debug32/solt.o: solt.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  solt.f -o debug32/solt.o -I debug32

debug32/std1.o: std1.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  std1.f -o debug32/std1.o -I debug32

debug32/std2.o: std2.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  std2.f -o debug32/std2.o -I debug32

debug32/std3.o: std3.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  std3.f -o debug32/std3.o -I debug32

debug32/stdaa.o: stdaa.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  stdaa.f -o debug32/stdaa.o -I debug32

debug32/storeinitial.o: storeinitial.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  storeinitial.f -o debug32/storeinitial.o -I debug32

debug32/structure.o: structure.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  structure.f -o debug32/structure.o -I debug32

debug32/subaa.o: subaa.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  subaa.f -o debug32/subaa.o -I debug32

debug32/subbasin.o: subbasin.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  subbasin.f -o debug32/subbasin.o -I debug32

debug32/subday.o: subday.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  subday.f -o debug32/subday.o -I debug32

debug32/submon.o: submon.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  submon.f -o debug32/submon.o -I debug32

debug32/substor.o: substor.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  substor.f -o debug32/substor.o -I debug32

debug32/subwq.o: subwq.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  subwq.f -o debug32/subwq.o -I debug32

debug32/subyr.o: subyr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  subyr.f -o debug32/subyr.o -I debug32

debug32/sub_subbasin.o: sub_subbasin.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  sub_subbasin.f -o debug32/sub_subbasin.o -I debug32

debug32/sumhyd.o: sumhyd.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  sumhyd.f -o debug32/sumhyd.o -I debug32

debug32/sumv.o: sumv.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  sumv.f -o debug32/sumv.o -I debug32

debug32/surface.o: surface.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  surface.f -o debug32/surface.o -I debug32

debug32/surfstor.o: surfstor.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  surfstor.f -o debug32/surfstor.o -I debug32

debug32/surfst_h2o.o: surfst_h2o.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  surfst_h2o.f -o debug32/surfst_h2o.o -I debug32

debug32/surq_daycn.o: surq_daycn.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  surq_daycn.f -o debug32/surq_daycn.o -I debug32

debug32/surq_greenampt.o: surq_greenampt.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  surq_greenampt.f -o debug32/surq_greenampt.o -I debug32

debug32/swbl.o: swbl.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  swbl.f -o debug32/swbl.o -I debug32

debug32/sweep.o: sweep.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  sweep.f -o debug32/sweep.o -I debug32

debug32/swu.o: swu.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  swu.f -o debug32/swu.o -I debug32

debug32/tair.o: tair.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  tair.f -o debug32/tair.o -I debug32

debug32/tgen.o: tgen.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  tgen.f -o debug32/tgen.o -I debug32

debug32/theta.o: theta.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  theta.f -o debug32/theta.o -I debug32

debug32/tillfactor.o: tillfactor.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  tillfactor.f -o debug32/tillfactor.o -I debug32

debug32/tillmix.o: tillmix.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  tillmix.f -o debug32/tillmix.o -I debug32

debug32/tmeas.o: tmeas.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  tmeas.f -o debug32/tmeas.o -I debug32

debug32/tran.o: tran.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  tran.f -o debug32/tran.o -I debug32

debug32/transfer.o: transfer.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  transfer.f -o debug32/transfer.o -I debug32

debug32/tstr.o: tstr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  tstr.f -o debug32/tstr.o -I debug32

debug32/ttcoef.o: ttcoef.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  ttcoef.f -o debug32/ttcoef.o -I debug32

debug32/ttcoef_wway.o: ttcoef_wway.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  ttcoef_wway.f -o debug32/ttcoef_wway.o -I debug32

debug32/urban.o: urban.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  urban.f -o debug32/urban.o -I debug32

debug32/urbanhr.o: urbanhr.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  urbanhr.f -o debug32/urbanhr.o -I debug32

debug32/urb_bmp.o: urb_bmp.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  urb_bmp.f -o debug32/urb_bmp.o -I debug32

debug32/varinit.o: varinit.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  varinit.f -o debug32/varinit.o -I debug32

debug32/vbl.o: vbl.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  vbl.f -o debug32/vbl.o -I debug32

debug32/virtual.o: virtual.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  virtual.f -o debug32/virtual.o -I debug32

debug32/volq.o: volq.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  volq.f -o debug32/volq.o -I debug32

debug32/washp.o: washp.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  washp.f -o debug32/washp.o -I debug32

debug32/watbal.o: watbal.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  watbal.f -o debug32/watbal.o -I debug32

debug32/water_hru.o: water_hru.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  water_hru.f -o debug32/water_hru.o -I debug32

debug32/watqual.o: watqual.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  watqual.f -o debug32/watqual.o -I debug32

debug32/watqual2.o: watqual2.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  watqual2.f -o debug32/watqual2.o -I debug32

debug32/wattable.o: wattable.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  wattable.f -o debug32/wattable.o -I debug32

debug32/watuse.o: watuse.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  watuse.f -o debug32/watuse.o -I debug32

debug32/weatgn.o: weatgn.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  weatgn.f -o debug32/weatgn.o -I debug32

debug32/wetlan.o: wetlan.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  wetlan.f -o debug32/wetlan.o -I debug32

debug32/wmeas.o: wmeas.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  wmeas.f -o debug32/wmeas.o -I debug32

debug32/wndgen.o: wndgen.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  wndgen.f -o debug32/wndgen.o -I debug32

debug32/writea.o: writea.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  writea.f -o debug32/writea.o -I debug32

debug32/writeaa.o: writeaa.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  writeaa.f -o debug32/writeaa.o -I debug32

debug32/writed.o: writed.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  writed.f -o debug32/writed.o -I debug32

debug32/writem.o: writem.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  writem.f -o debug32/writem.o -I debug32

debug32/xmon.o: xmon.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  xmon.f -o debug32/xmon.o -I debug32

debug32/ysed.o: ysed.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  ysed.f -o debug32/ysed.o -I debug32

debug32/zero0.o: zero0.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  zero0.f -o debug32/zero0.o -I debug32

debug32/zero1.o: zero1.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  zero1.f -o debug32/zero1.o -I debug32

debug32/zero2.o: zero2.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  zero2.f -o debug32/zero2.o -I debug32

debug32/zeroini.o: zeroini.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  zeroini.f -o debug32/zeroini.o -I debug32

debug32/zero_urbn.o: zero_urbn.f debug32/main.o
	${FC} ${ARCH32} ${FFLAG} ${DFLAG}  zero_urbn.f -o debug32/zero_urbn.o -I debug32

debug32_clean:
	rm -f ${NAMEDEBUG32}.exe
	rm -f debug32/*.o
	rm -f debug32/*.mod
	rm -f debug32/*~

OBJECTS_DEBUG64=  debug64/addh.o debug64/albedo.o debug64/allocate_parms.o debug64/alph.o debug64/anfert.o debug64/apex_day.o debug64/apply.o debug64/ascrv.o debug64/atri.o debug64/aunif.o debug64/autoirr.o debug64/aveval.o debug64/bacteria.o debug64/biofilm.o debug64/biozone.o debug64/bmpfixed.o debug64/bmpinit.o debug64/bmp_det_pond.o debug64/bmp_ri_pond.o debug64/bmp_sand_filter.o debug64/bmp_sed_pond.o debug64/bmp_wet_pond.o debug64/buffer.o debug64/burnop.o debug64/canopyint.o debug64/caps.o debug64/carbon_new.o debug64/carbon_zhang2.o debug64/cfactor.o debug64/chkcst.o debug64/clgen.o debug64/clicon.o debug64/command.o debug64/conapply.o debug64/confert.o debug64/crackflow.o debug64/crackvol.o debug64/curno.o debug64/dailycn.o debug64/decay.o debug64/depstor.o debug64/distrib_bmps.o debug64/dormant.o debug64/drains.o debug64/dstn1.o debug64/ee.o debug64/eiusle.o debug64/enrsb.o debug64/erfc.o debug64/estimate_ksat.o debug64/etact.o debug64/etpot.o debug64/expo.o debug64/fert.o debug64/filter.o debug64/filtw.o debug64/finalbal.o debug64/gcycl.o debug64/getallo.o debug64/grass_wway.o debug64/graze.o debug64/grow.o debug64/gwmod.o debug64/gwmod_deep.o debug64/gwnutr.o debug64/gw_no3.o debug64/h2omgt_init.o debug64/harvestop.o debug64/harvgrainop.o debug64/harvkillop.o debug64/header.o debug64/headout.o debug64/hhnoqual.o debug64/hhwatqual.o debug64/hmeas.o debug64/hruaa.o debug64/hruallo.o debug64/hruday.o debug64/hrumon.o debug64/hrupond.o debug64/hrupondhr.o debug64/hruyr.o debug64/hydroinit.o debug64/icl.o debug64/impndaa.o debug64/impndday.o debug64/impndmon.o debug64/impndyr.o debug64/impnd_init.o debug64/irrigate.o debug64/irrsub.o debug64/irr_rch.o debug64/irr_res.o debug64/jdt.o debug64/killop.o debug64/lakeq.o debug64/latsed.o debug64/layersplit.o debug64/lwqdef.o debug64/main.o debug64/ncsed_leach.o debug64/ndenit.o debug64/newtillmix.o debug64/nfix.o debug64/nitvol.o debug64/nlch.o debug64/nminrl.o debug64/noqual.o debug64/npup.o debug64/nrain.o debug64/nup.o debug64/nuts.o debug64/openwth.o debug64/operatn.o debug64/orgn.o debug64/orgncswat.o debug64/origtile.o debug64/ovr_sed.o debug64/percmacro.o debug64/percmain.o debug64/percmicro.o debug64/pestlch.o debug64/pestw.o debug64/pesty.o debug64/pgen.o debug64/pgenhr.o debug64/pkq.o debug64/plantmod.o debug64/plantop.o debug64/pmeas.o debug64/pminrl.o debug64/pminrl2.o debug64/pond.o debug64/pondhr.o debug64/pothole.o debug64/potholehr.o debug64/print_hyd.o debug64/psed.o debug64/qman.o debug64/ran1.o debug64/rchaa.o debug64/rchday.o debug64/rchinit.o debug64/rchmon.o debug64/rchuse.o debug64/rchyr.o debug64/reachout.o debug64/readatmodep.o debug64/readbsn.o debug64/readchm.o debug64/readcnst.o debug64/readfcst.o debug64/readfert.o debug64/readfig.o debug64/readfile.o debug64/readgw.o debug64/readhru.o debug64/readinpt.o debug64/readlup.o debug64/readlwq.o debug64/readmgt.o debug64/readmon.o debug64/readops.o debug64/readpest.o debug64/readplant.o debug64/readpnd.o debug64/readres.o debug64/readrte.o debug64/readru.o debug64/readsdr.o debug64/readsepticbz.o debug64/readseptwq.o debug64/readsno.o debug64/readsol.o debug64/readsub.o debug64/readswq.o debug64/readtill.o debug64/readurban.o debug64/readwgn.o debug64/readwus.o debug64/readwwq.o debug64/readyr.o debug64/reccnst.o debug64/recday.o debug64/rechour.o debug64/recmon.o debug64/recyear.o debug64/regres.o debug64/res.o debug64/resbact.o debug64/resetlu.o debug64/reshr.o debug64/resinit.o debug64/resnut.o debug64/rewind_init.o debug64/rhgen.o debug64/rootfr.o debug64/route.o debug64/routels.o debug64/routeunit.o debug64/routres.o debug64/rsedaa.o debug64/rseday.o debug64/rsedmon.o debug64/rsedyr.o debug64/rtbact.o debug64/rtday.o debug64/rteinit.o debug64/rthmusk.o debug64/rthpest.o debug64/rthr.o debug64/rthsed.o debug64/rtmusk.o debug64/rtout.o debug64/rtpest.o debug64/rtsed.o debug64/rtsed_bagnold.o debug64/rtsed_kodatie.o debug64/rtsed_molinas_wu.o debug64/rtsed_yangsand.o debug64/sat_excess.o debug64/save.o debug64/saveconc.o debug64/schedule_ops.o debug64/sched_mgt.o debug64/simulate.o debug64/sim_initday.o debug64/sim_inityr.o debug64/slrgen.o debug64/smeas.o debug64/snom.o debug64/soil_chem.o debug64/soil_par.o debug64/soil_phys.o debug64/soil_write.o debug64/solp.o debug64/solt.o debug64/std1.o debug64/std2.o debug64/std3.o debug64/stdaa.o debug64/storeinitial.o debug64/structure.o debug64/subaa.o debug64/subbasin.o debug64/subday.o debug64/submon.o debug64/substor.o debug64/subwq.o debug64/subyr.o debug64/sub_subbasin.o debug64/sumhyd.o debug64/sumv.o debug64/surface.o debug64/surfstor.o debug64/surfst_h2o.o debug64/surq_daycn.o debug64/surq_greenampt.o debug64/swbl.o debug64/sweep.o debug64/swu.o debug64/tair.o debug64/tgen.o debug64/theta.o debug64/tillfactor.o debug64/tillmix.o debug64/tmeas.o debug64/tran.o debug64/transfer.o debug64/tstr.o debug64/ttcoef.o debug64/ttcoef_wway.o debug64/urban.o debug64/urbanhr.o debug64/urb_bmp.o debug64/varinit.o debug64/vbl.o debug64/virtual.o debug64/volq.o debug64/washp.o debug64/watbal.o debug64/water_hru.o debug64/watqual.o debug64/watqual2.o debug64/wattable.o debug64/watuse.o debug64/weatgn.o debug64/wetlan.o debug64/wmeas.o debug64/wndgen.o debug64/writea.o debug64/writeaa.o debug64/writed.o debug64/writem.o debug64/xmon.o debug64/ysed.o debug64/zero0.o debug64/zero1.o debug64/zero2.o debug64/zeroini.o debug64/zero_urbn.o

NAMEDEBUG64=swat_debug64
debug64:debug64_mkdir ${NAMEDEBUG64}

debug64_mkdir:
	mkdir -p debug64

${NAMEDEBUG64}: ${OBJECTS_DEBUG64}
	${FC} ${OBJECTS_DEBUG64} ${ARCH64} -static -o ${NAMEDEBUG64}


debug64/addh.o: addh.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  addh.f -o debug64/addh.o -I debug64

debug64/albedo.o: albedo.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  albedo.f -o debug64/albedo.o -I debug64

debug64/allocate_parms.o: allocate_parms.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  allocate_parms.f -o debug64/allocate_parms.o -I debug64

debug64/alph.o: alph.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  alph.f -o debug64/alph.o -I debug64

debug64/anfert.o: anfert.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  anfert.f -o debug64/anfert.o -I debug64

debug64/apex_day.o: apex_day.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  apex_day.f -o debug64/apex_day.o -I debug64

debug64/apply.o: apply.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  apply.f -o debug64/apply.o -I debug64

debug64/ascrv.o: ascrv.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  ascrv.f -o debug64/ascrv.o -I debug64

debug64/atri.o: atri.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  atri.f -o debug64/atri.o -I debug64

debug64/aunif.o: aunif.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  aunif.f -o debug64/aunif.o -I debug64

debug64/autoirr.o: autoirr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  autoirr.f -o debug64/autoirr.o -I debug64

debug64/aveval.o: aveval.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  aveval.f -o debug64/aveval.o -I debug64

debug64/bacteria.o: bacteria.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  bacteria.f -o debug64/bacteria.o -I debug64

debug64/biofilm.o: biofilm.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  biofilm.f -o debug64/biofilm.o -I debug64

debug64/biozone.o: biozone.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG} ${LONGFIX} biozone.f -o debug64/biozone.o -I debug64

debug64/bmpfixed.o: bmpfixed.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  bmpfixed.f -o debug64/bmpfixed.o -I debug64

debug64/bmpinit.o: bmpinit.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG} ${LONGFIX} bmpinit.f -o debug64/bmpinit.o -I debug64

debug64/bmp_det_pond.o: bmp_det_pond.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  bmp_det_pond.f -o debug64/bmp_det_pond.o -I debug64

debug64/bmp_ri_pond.o: bmp_ri_pond.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  bmp_ri_pond.f -o debug64/bmp_ri_pond.o -I debug64

debug64/bmp_sand_filter.o: bmp_sand_filter.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  bmp_sand_filter.f -o debug64/bmp_sand_filter.o -I debug64

debug64/bmp_sed_pond.o: bmp_sed_pond.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  bmp_sed_pond.f -o debug64/bmp_sed_pond.o -I debug64

debug64/bmp_wet_pond.o: bmp_wet_pond.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  bmp_wet_pond.f -o debug64/bmp_wet_pond.o -I debug64

debug64/buffer.o: buffer.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  buffer.f -o debug64/buffer.o -I debug64

debug64/burnop.o: burnop.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  burnop.f -o debug64/burnop.o -I debug64

debug64/canopyint.o: canopyint.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  canopyint.f -o debug64/canopyint.o -I debug64

debug64/caps.o: caps.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  caps.f -o debug64/caps.o -I debug64

debug64/carbon_new.o: carbon_new.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  carbon_new.f -o debug64/carbon_new.o -I debug64

debug64/carbon_zhang2.o: carbon_zhang2.f90 debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG} ${LONGFREE} carbon_zhang2.f90 -o debug64/carbon_zhang2.o -I debug64

debug64/cfactor.o: cfactor.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  cfactor.f -o debug64/cfactor.o -I debug64

debug64/chkcst.o: chkcst.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  chkcst.f -o debug64/chkcst.o -I debug64

debug64/clgen.o: clgen.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  clgen.f -o debug64/clgen.o -I debug64

debug64/clicon.o: clicon.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  clicon.f -o debug64/clicon.o -I debug64

debug64/command.o: command.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  command.f -o debug64/command.o -I debug64

debug64/conapply.o: conapply.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  conapply.f -o debug64/conapply.o -I debug64

debug64/confert.o: confert.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  confert.f -o debug64/confert.o -I debug64

debug64/crackflow.o: crackflow.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  crackflow.f -o debug64/crackflow.o -I debug64

debug64/crackvol.o: crackvol.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  crackvol.f -o debug64/crackvol.o -I debug64

debug64/curno.o: curno.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  curno.f -o debug64/curno.o -I debug64

debug64/dailycn.o: dailycn.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  dailycn.f -o debug64/dailycn.o -I debug64

debug64/decay.o: decay.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  decay.f -o debug64/decay.o -I debug64

debug64/depstor.o: depstor.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  depstor.f -o debug64/depstor.o -I debug64

debug64/distrib_bmps.o: distrib_bmps.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  distrib_bmps.f -o debug64/distrib_bmps.o -I debug64

debug64/dormant.o: dormant.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  dormant.f -o debug64/dormant.o -I debug64

debug64/drains.o: drains.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  drains.f -o debug64/drains.o -I debug64

debug64/dstn1.o: dstn1.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  dstn1.f -o debug64/dstn1.o -I debug64

debug64/ee.o: ee.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  ee.f -o debug64/ee.o -I debug64

debug64/eiusle.o: eiusle.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  eiusle.f -o debug64/eiusle.o -I debug64

debug64/enrsb.o: enrsb.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  enrsb.f -o debug64/enrsb.o -I debug64

debug64/erfc.o: erfc.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  erfc.f -o debug64/erfc.o -I debug64

debug64/estimate_ksat.o: estimate_ksat.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  estimate_ksat.f -o debug64/estimate_ksat.o -I debug64

debug64/etact.o: etact.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  etact.f -o debug64/etact.o -I debug64

debug64/etpot.o: etpot.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  etpot.f -o debug64/etpot.o -I debug64

debug64/expo.o: expo.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  expo.f -o debug64/expo.o -I debug64

debug64/fert.o: fert.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  fert.f -o debug64/fert.o -I debug64

debug64/filter.o: filter.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  filter.f -o debug64/filter.o -I debug64

debug64/filtw.o: filtw.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  filtw.f -o debug64/filtw.o -I debug64

debug64/finalbal.o: finalbal.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  finalbal.f -o debug64/finalbal.o -I debug64

debug64/gcycl.o: gcycl.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  gcycl.f -o debug64/gcycl.o -I debug64

debug64/getallo.o: getallo.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  getallo.f -o debug64/getallo.o -I debug64

debug64/grass_wway.o: grass_wway.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  grass_wway.f -o debug64/grass_wway.o -I debug64

debug64/graze.o: graze.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  graze.f -o debug64/graze.o -I debug64

debug64/grow.o: grow.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  grow.f -o debug64/grow.o -I debug64

debug64/gwmod.o: gwmod.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  gwmod.f -o debug64/gwmod.o -I debug64

debug64/gwmod_deep.o: gwmod_deep.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  gwmod_deep.f -o debug64/gwmod_deep.o -I debug64

debug64/gwnutr.o: gwnutr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  gwnutr.f -o debug64/gwnutr.o -I debug64

debug64/gw_no3.o: gw_no3.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  gw_no3.f -o debug64/gw_no3.o -I debug64

debug64/h2omgt_init.o: h2omgt_init.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  h2omgt_init.f -o debug64/h2omgt_init.o -I debug64

debug64/harvestop.o: harvestop.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  harvestop.f -o debug64/harvestop.o -I debug64

debug64/harvgrainop.o: harvgrainop.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  harvgrainop.f -o debug64/harvgrainop.o -I debug64

debug64/harvkillop.o: harvkillop.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  harvkillop.f -o debug64/harvkillop.o -I debug64

debug64/header.o: header.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  header.f -o debug64/header.o -I debug64

debug64/headout.o: headout.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  headout.f -o debug64/headout.o -I debug64

debug64/hhnoqual.o: hhnoqual.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hhnoqual.f -o debug64/hhnoqual.o -I debug64

debug64/hhwatqual.o: hhwatqual.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hhwatqual.f -o debug64/hhwatqual.o -I debug64

debug64/hmeas.o: hmeas.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hmeas.f -o debug64/hmeas.o -I debug64

debug64/hruaa.o: hruaa.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hruaa.f -o debug64/hruaa.o -I debug64

debug64/hruallo.o: hruallo.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hruallo.f -o debug64/hruallo.o -I debug64

debug64/hruday.o: hruday.f90 debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hruday.f90 -o debug64/hruday.o -I debug64

debug64/hrumon.o: hrumon.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hrumon.f -o debug64/hrumon.o -I debug64

debug64/hrupond.o: hrupond.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hrupond.f -o debug64/hrupond.o -I debug64

debug64/hrupondhr.o: hrupondhr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hrupondhr.f -o debug64/hrupondhr.o -I debug64

debug64/hruyr.o: hruyr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hruyr.f -o debug64/hruyr.o -I debug64

debug64/hydroinit.o: hydroinit.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  hydroinit.f -o debug64/hydroinit.o -I debug64

debug64/icl.o: icl.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  icl.f -o debug64/icl.o -I debug64

debug64/impndaa.o: impndaa.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  impndaa.f -o debug64/impndaa.o -I debug64

debug64/impndday.o: impndday.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  impndday.f -o debug64/impndday.o -I debug64

debug64/impndmon.o: impndmon.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  impndmon.f -o debug64/impndmon.o -I debug64

debug64/impndyr.o: impndyr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  impndyr.f -o debug64/impndyr.o -I debug64

debug64/impnd_init.o: impnd_init.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  impnd_init.f -o debug64/impnd_init.o -I debug64

debug64/irrigate.o: irrigate.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  irrigate.f -o debug64/irrigate.o -I debug64

debug64/irrsub.o: irrsub.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  irrsub.f -o debug64/irrsub.o -I debug64

debug64/irr_rch.o: irr_rch.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  irr_rch.f -o debug64/irr_rch.o -I debug64

debug64/irr_res.o: irr_res.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  irr_res.f -o debug64/irr_res.o -I debug64

debug64/jdt.o: jdt.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  jdt.f -o debug64/jdt.o -I debug64

debug64/killop.o: killop.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  killop.f -o debug64/killop.o -I debug64

debug64/lakeq.o: lakeq.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  lakeq.f -o debug64/lakeq.o -I debug64

debug64/latsed.o: latsed.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  latsed.f -o debug64/latsed.o -I debug64

debug64/layersplit.o: layersplit.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  layersplit.f -o debug64/layersplit.o -I debug64

debug64/lwqdef.o: lwqdef.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  lwqdef.f -o debug64/lwqdef.o -I debug64

debug64/main.o: main.f modparm.f
	${FC} ${ARCH64} ${FFLAG} ${DFLAG} ${LONGFIX} main.f -o debug64/main.o -J debug64

debug64/ncsed_leach.o: NCsed_leach.f90 debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  NCsed_leach.f90 -o debug64/ncsed_leach.o -I debug64

debug64/ndenit.o: ndenit.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  ndenit.f -o debug64/ndenit.o -I debug64

debug64/newtillmix.o: newtillmix.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  newtillmix.f -o debug64/newtillmix.o -I debug64

debug64/nfix.o: nfix.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  nfix.f -o debug64/nfix.o -I debug64

debug64/nitvol.o: nitvol.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  nitvol.f -o debug64/nitvol.o -I debug64

debug64/nlch.o: nlch.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  nlch.f -o debug64/nlch.o -I debug64

debug64/nminrl.o: nminrl.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  nminrl.f -o debug64/nminrl.o -I debug64

debug64/noqual.o: noqual.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  noqual.f -o debug64/noqual.o -I debug64

debug64/npup.o: npup.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  npup.f -o debug64/npup.o -I debug64

debug64/nrain.o: nrain.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  nrain.f -o debug64/nrain.o -I debug64

debug64/nup.o: nup.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  nup.f -o debug64/nup.o -I debug64

debug64/nuts.o: nuts.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  nuts.f -o debug64/nuts.o -I debug64

debug64/openwth.o: openwth.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  openwth.f -o debug64/openwth.o -I debug64

debug64/operatn.o: operatn.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  operatn.f -o debug64/operatn.o -I debug64

debug64/orgn.o: orgn.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  orgn.f -o debug64/orgn.o -I debug64

debug64/orgncswat.o: orgncswat.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  orgncswat.f -o debug64/orgncswat.o -I debug64

debug64/origtile.o: origtile.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  origtile.f -o debug64/origtile.o -I debug64

debug64/ovr_sed.o: ovr_sed.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG} ${LONGFIX} ovr_sed.f -o debug64/ovr_sed.o -I debug64

debug64/percmacro.o: percmacro.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  percmacro.f -o debug64/percmacro.o -I debug64

debug64/percmain.o: percmain.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG} ${LONGFIX} percmain.f -o debug64/percmain.o -I debug64

debug64/percmicro.o: percmicro.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  percmicro.f -o debug64/percmicro.o -I debug64

debug64/pestlch.o: pestlch.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pestlch.f -o debug64/pestlch.o -I debug64

debug64/pestw.o: pestw.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pestw.f -o debug64/pestw.o -I debug64

debug64/pesty.o: pesty.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pesty.f -o debug64/pesty.o -I debug64

debug64/pgen.o: pgen.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pgen.f -o debug64/pgen.o -I debug64

debug64/pgenhr.o: pgenhr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pgenhr.f -o debug64/pgenhr.o -I debug64

debug64/pkq.o: pkq.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pkq.f -o debug64/pkq.o -I debug64

debug64/plantmod.o: plantmod.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  plantmod.f -o debug64/plantmod.o -I debug64

debug64/plantop.o: plantop.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  plantop.f -o debug64/plantop.o -I debug64

debug64/pmeas.o: pmeas.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pmeas.f -o debug64/pmeas.o -I debug64

debug64/pminrl.o: pminrl.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pminrl.f -o debug64/pminrl.o -I debug64

debug64/pminrl2.o: pminrl2.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pminrl2.f -o debug64/pminrl2.o -I debug64

debug64/pond.o: pond.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pond.f -o debug64/pond.o -I debug64

debug64/pondhr.o: pondhr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pondhr.f -o debug64/pondhr.o -I debug64

debug64/pothole.o: pothole.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  pothole.f -o debug64/pothole.o -I debug64

debug64/potholehr.o: potholehr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  potholehr.f -o debug64/potholehr.o -I debug64

debug64/print_hyd.o: print_hyd.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  print_hyd.f -o debug64/print_hyd.o -I debug64

debug64/psed.o: psed.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  psed.f -o debug64/psed.o -I debug64

debug64/qman.o: qman.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  qman.f -o debug64/qman.o -I debug64

debug64/ran1.o: ran1.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  ran1.f -o debug64/ran1.o -I debug64

debug64/rchaa.o: rchaa.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rchaa.f -o debug64/rchaa.o -I debug64

debug64/rchday.o: rchday.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rchday.f -o debug64/rchday.o -I debug64

debug64/rchinit.o: rchinit.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rchinit.f -o debug64/rchinit.o -I debug64

debug64/rchmon.o: rchmon.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rchmon.f -o debug64/rchmon.o -I debug64

debug64/rchuse.o: rchuse.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rchuse.f -o debug64/rchuse.o -I debug64

debug64/rchyr.o: rchyr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rchyr.f -o debug64/rchyr.o -I debug64

debug64/reachout.o: reachout.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  reachout.f -o debug64/reachout.o -I debug64

debug64/readatmodep.o: readatmodep.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readatmodep.f -o debug64/readatmodep.o -I debug64

debug64/readbsn.o: readbsn.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readbsn.f -o debug64/readbsn.o -I debug64

debug64/readchm.o: readchm.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readchm.f -o debug64/readchm.o -I debug64

debug64/readcnst.o: readcnst.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readcnst.f -o debug64/readcnst.o -I debug64

debug64/readfcst.o: readfcst.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readfcst.f -o debug64/readfcst.o -I debug64

debug64/readfert.o: readfert.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readfert.f -o debug64/readfert.o -I debug64

debug64/readfig.o: readfig.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readfig.f -o debug64/readfig.o -I debug64

debug64/readfile.o: readfile.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readfile.f -o debug64/readfile.o -I debug64

debug64/readgw.o: readgw.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readgw.f -o debug64/readgw.o -I debug64

debug64/readhru.o: readhru.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readhru.f -o debug64/readhru.o -I debug64

debug64/readinpt.o: readinpt.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readinpt.f -o debug64/readinpt.o -I debug64

debug64/readlup.o: readlup.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readlup.f -o debug64/readlup.o -I debug64

debug64/readlwq.o: readlwq.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readlwq.f -o debug64/readlwq.o -I debug64

debug64/readmgt.o: readmgt.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readmgt.f -o debug64/readmgt.o -I debug64

debug64/readmon.o: readmon.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readmon.f -o debug64/readmon.o -I debug64

debug64/readops.o: readops.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readops.f -o debug64/readops.o -I debug64

debug64/readpest.o: readpest.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readpest.f -o debug64/readpest.o -I debug64

debug64/readplant.o: readplant.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readplant.f -o debug64/readplant.o -I debug64

debug64/readpnd.o: readpnd.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readpnd.f -o debug64/readpnd.o -I debug64

debug64/readres.o: readres.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readres.f -o debug64/readres.o -I debug64

debug64/readrte.o: readrte.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readrte.f -o debug64/readrte.o -I debug64

debug64/readru.o: readru.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readru.f -o debug64/readru.o -I debug64

debug64/readsdr.o: readsdr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readsdr.f -o debug64/readsdr.o -I debug64

debug64/readsepticbz.o: readsepticbz.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readsepticbz.f -o debug64/readsepticbz.o -I debug64

debug64/readseptwq.o: readseptwq.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readseptwq.f -o debug64/readseptwq.o -I debug64

debug64/readsno.o: readsno.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readsno.f -o debug64/readsno.o -I debug64

debug64/readsol.o: readsol.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readsol.f -o debug64/readsol.o -I debug64

debug64/readsub.o: readsub.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readsub.f -o debug64/readsub.o -I debug64

debug64/readswq.o: readswq.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readswq.f -o debug64/readswq.o -I debug64

debug64/readtill.o: readtill.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readtill.f -o debug64/readtill.o -I debug64

debug64/readurban.o: readurban.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readurban.f -o debug64/readurban.o -I debug64

debug64/readwgn.o: readwgn.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readwgn.f -o debug64/readwgn.o -I debug64

debug64/readwus.o: readwus.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readwus.f -o debug64/readwus.o -I debug64

debug64/readwwq.o: readwwq.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readwwq.f -o debug64/readwwq.o -I debug64

debug64/readyr.o: readyr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  readyr.f -o debug64/readyr.o -I debug64

debug64/reccnst.o: reccnst.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  reccnst.f -o debug64/reccnst.o -I debug64

debug64/recday.o: recday.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  recday.f -o debug64/recday.o -I debug64

debug64/rechour.o: rechour.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rechour.f -o debug64/rechour.o -I debug64

debug64/recmon.o: recmon.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  recmon.f -o debug64/recmon.o -I debug64

debug64/recyear.o: recyear.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  recyear.f -o debug64/recyear.o -I debug64

debug64/regres.o: regres.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  regres.f -o debug64/regres.o -I debug64

debug64/res.o: res.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  res.f -o debug64/res.o -I debug64

debug64/resbact.o: resbact.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  resbact.f -o debug64/resbact.o -I debug64

debug64/resetlu.o: resetlu.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  resetlu.f -o debug64/resetlu.o -I debug64

debug64/reshr.o: reshr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  reshr.f -o debug64/reshr.o -I debug64

debug64/resinit.o: resinit.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  resinit.f -o debug64/resinit.o -I debug64

debug64/resnut.o: resnut.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  resnut.f -o debug64/resnut.o -I debug64

debug64/rewind_init.o: rewind_init.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rewind_init.f -o debug64/rewind_init.o -I debug64

debug64/rhgen.o: rhgen.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rhgen.f -o debug64/rhgen.o -I debug64

debug64/rootfr.o: rootfr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rootfr.f -o debug64/rootfr.o -I debug64

debug64/route.o: route.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  route.f -o debug64/route.o -I debug64

debug64/routels.o: routels.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  routels.f -o debug64/routels.o -I debug64

debug64/routeunit.o: routeunit.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  routeunit.f -o debug64/routeunit.o -I debug64

debug64/routres.o: routres.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  routres.f -o debug64/routres.o -I debug64

debug64/rsedaa.o: rsedaa.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rsedaa.f -o debug64/rsedaa.o -I debug64

debug64/rseday.o: rseday.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rseday.f -o debug64/rseday.o -I debug64

debug64/rsedmon.o: rsedmon.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rsedmon.f -o debug64/rsedmon.o -I debug64

debug64/rsedyr.o: rsedyr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rsedyr.f -o debug64/rsedyr.o -I debug64

debug64/rtbact.o: rtbact.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtbact.f -o debug64/rtbact.o -I debug64

debug64/rtday.o: rtday.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtday.f -o debug64/rtday.o -I debug64

debug64/rteinit.o: rteinit.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rteinit.f -o debug64/rteinit.o -I debug64

debug64/rthmusk.o: rthmusk.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rthmusk.f -o debug64/rthmusk.o -I debug64

debug64/rthpest.o: rthpest.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rthpest.f -o debug64/rthpest.o -I debug64

debug64/rthr.o: rthr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rthr.f -o debug64/rthr.o -I debug64

debug64/rthsed.o: rthsed.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG} ${LONGFIX} rthsed.f -o debug64/rthsed.o -I debug64

debug64/rtmusk.o: rtmusk.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtmusk.f -o debug64/rtmusk.o -I debug64

debug64/rtout.o: rtout.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtout.f -o debug64/rtout.o -I debug64

debug64/rtpest.o: rtpest.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtpest.f -o debug64/rtpest.o -I debug64

debug64/rtsed.o: rtsed.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtsed.f -o debug64/rtsed.o -I debug64

debug64/rtsed_bagnold.o: rtsed_bagnold.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtsed_bagnold.f -o debug64/rtsed_bagnold.o -I debug64

debug64/rtsed_kodatie.o: rtsed_kodatie.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtsed_kodatie.f -o debug64/rtsed_kodatie.o -I debug64

debug64/rtsed_molinas_wu.o: rtsed_Molinas_Wu.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtsed_Molinas_Wu.f -o debug64/rtsed_molinas_wu.o -I debug64

debug64/rtsed_yangsand.o: rtsed_yangsand.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  rtsed_yangsand.f -o debug64/rtsed_yangsand.o -I debug64

debug64/sat_excess.o: sat_excess.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  sat_excess.f -o debug64/sat_excess.o -I debug64

debug64/save.o: save.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  save.f -o debug64/save.o -I debug64

debug64/saveconc.o: saveconc.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  saveconc.f -o debug64/saveconc.o -I debug64

debug64/schedule_ops.o: schedule_ops.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  schedule_ops.f -o debug64/schedule_ops.o -I debug64

debug64/sched_mgt.o: sched_mgt.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  sched_mgt.f -o debug64/sched_mgt.o -I debug64

debug64/simulate.o: simulate.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  simulate.f -o debug64/simulate.o -I debug64

debug64/sim_initday.o: sim_initday.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  sim_initday.f -o debug64/sim_initday.o -I debug64

debug64/sim_inityr.o: sim_inityr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  sim_inityr.f -o debug64/sim_inityr.o -I debug64

debug64/slrgen.o: slrgen.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  slrgen.f -o debug64/slrgen.o -I debug64

debug64/smeas.o: smeas.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  smeas.f -o debug64/smeas.o -I debug64

debug64/snom.o: snom.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  snom.f -o debug64/snom.o -I debug64

debug64/soil_chem.o: soil_chem.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  soil_chem.f -o debug64/soil_chem.o -I debug64

debug64/soil_par.o: soil_par.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  soil_par.f -o debug64/soil_par.o -I debug64

debug64/soil_phys.o: soil_phys.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  soil_phys.f -o debug64/soil_phys.o -I debug64

debug64/soil_write.o: soil_write.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  soil_write.f -o debug64/soil_write.o -I debug64

debug64/solp.o: solp.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  solp.f -o debug64/solp.o -I debug64

debug64/solt.o: solt.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  solt.f -o debug64/solt.o -I debug64

debug64/std1.o: std1.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  std1.f -o debug64/std1.o -I debug64

debug64/std2.o: std2.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  std2.f -o debug64/std2.o -I debug64

debug64/std3.o: std3.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  std3.f -o debug64/std3.o -I debug64

debug64/stdaa.o: stdaa.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  stdaa.f -o debug64/stdaa.o -I debug64

debug64/storeinitial.o: storeinitial.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  storeinitial.f -o debug64/storeinitial.o -I debug64

debug64/structure.o: structure.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  structure.f -o debug64/structure.o -I debug64

debug64/subaa.o: subaa.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  subaa.f -o debug64/subaa.o -I debug64

debug64/subbasin.o: subbasin.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  subbasin.f -o debug64/subbasin.o -I debug64

debug64/subday.o: subday.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  subday.f -o debug64/subday.o -I debug64

debug64/submon.o: submon.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  submon.f -o debug64/submon.o -I debug64

debug64/substor.o: substor.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  substor.f -o debug64/substor.o -I debug64

debug64/subwq.o: subwq.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  subwq.f -o debug64/subwq.o -I debug64

debug64/subyr.o: subyr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  subyr.f -o debug64/subyr.o -I debug64

debug64/sub_subbasin.o: sub_subbasin.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  sub_subbasin.f -o debug64/sub_subbasin.o -I debug64

debug64/sumhyd.o: sumhyd.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  sumhyd.f -o debug64/sumhyd.o -I debug64

debug64/sumv.o: sumv.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  sumv.f -o debug64/sumv.o -I debug64

debug64/surface.o: surface.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  surface.f -o debug64/surface.o -I debug64

debug64/surfstor.o: surfstor.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  surfstor.f -o debug64/surfstor.o -I debug64

debug64/surfst_h2o.o: surfst_h2o.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  surfst_h2o.f -o debug64/surfst_h2o.o -I debug64

debug64/surq_daycn.o: surq_daycn.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  surq_daycn.f -o debug64/surq_daycn.o -I debug64

debug64/surq_greenampt.o: surq_greenampt.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  surq_greenampt.f -o debug64/surq_greenampt.o -I debug64

debug64/swbl.o: swbl.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  swbl.f -o debug64/swbl.o -I debug64

debug64/sweep.o: sweep.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  sweep.f -o debug64/sweep.o -I debug64

debug64/swu.o: swu.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  swu.f -o debug64/swu.o -I debug64

debug64/tair.o: tair.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  tair.f -o debug64/tair.o -I debug64

debug64/tgen.o: tgen.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  tgen.f -o debug64/tgen.o -I debug64

debug64/theta.o: theta.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  theta.f -o debug64/theta.o -I debug64

debug64/tillfactor.o: tillfactor.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  tillfactor.f -o debug64/tillfactor.o -I debug64

debug64/tillmix.o: tillmix.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  tillmix.f -o debug64/tillmix.o -I debug64

debug64/tmeas.o: tmeas.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  tmeas.f -o debug64/tmeas.o -I debug64

debug64/tran.o: tran.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  tran.f -o debug64/tran.o -I debug64

debug64/transfer.o: transfer.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  transfer.f -o debug64/transfer.o -I debug64

debug64/tstr.o: tstr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  tstr.f -o debug64/tstr.o -I debug64

debug64/ttcoef.o: ttcoef.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  ttcoef.f -o debug64/ttcoef.o -I debug64

debug64/ttcoef_wway.o: ttcoef_wway.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  ttcoef_wway.f -o debug64/ttcoef_wway.o -I debug64

debug64/urban.o: urban.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  urban.f -o debug64/urban.o -I debug64

debug64/urbanhr.o: urbanhr.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  urbanhr.f -o debug64/urbanhr.o -I debug64

debug64/urb_bmp.o: urb_bmp.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  urb_bmp.f -o debug64/urb_bmp.o -I debug64

debug64/varinit.o: varinit.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  varinit.f -o debug64/varinit.o -I debug64

debug64/vbl.o: vbl.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  vbl.f -o debug64/vbl.o -I debug64

debug64/virtual.o: virtual.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  virtual.f -o debug64/virtual.o -I debug64

debug64/volq.o: volq.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  volq.f -o debug64/volq.o -I debug64

debug64/washp.o: washp.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  washp.f -o debug64/washp.o -I debug64

debug64/watbal.o: watbal.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  watbal.f -o debug64/watbal.o -I debug64

debug64/water_hru.o: water_hru.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  water_hru.f -o debug64/water_hru.o -I debug64

debug64/watqual.o: watqual.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  watqual.f -o debug64/watqual.o -I debug64

debug64/watqual2.o: watqual2.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  watqual2.f -o debug64/watqual2.o -I debug64

debug64/wattable.o: wattable.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  wattable.f -o debug64/wattable.o -I debug64

debug64/watuse.o: watuse.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  watuse.f -o debug64/watuse.o -I debug64

debug64/weatgn.o: weatgn.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  weatgn.f -o debug64/weatgn.o -I debug64

debug64/wetlan.o: wetlan.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  wetlan.f -o debug64/wetlan.o -I debug64

debug64/wmeas.o: wmeas.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  wmeas.f -o debug64/wmeas.o -I debug64

debug64/wndgen.o: wndgen.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  wndgen.f -o debug64/wndgen.o -I debug64

debug64/writea.o: writea.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  writea.f -o debug64/writea.o -I debug64

debug64/writeaa.o: writeaa.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  writeaa.f -o debug64/writeaa.o -I debug64

debug64/writed.o: writed.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  writed.f -o debug64/writed.o -I debug64

debug64/writem.o: writem.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  writem.f -o debug64/writem.o -I debug64

debug64/xmon.o: xmon.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  xmon.f -o debug64/xmon.o -I debug64

debug64/ysed.o: ysed.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  ysed.f -o debug64/ysed.o -I debug64

debug64/zero0.o: zero0.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  zero0.f -o debug64/zero0.o -I debug64

debug64/zero1.o: zero1.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  zero1.f -o debug64/zero1.o -I debug64

debug64/zero2.o: zero2.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  zero2.f -o debug64/zero2.o -I debug64

debug64/zeroini.o: zeroini.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  zeroini.f -o debug64/zeroini.o -I debug64

debug64/zero_urbn.o: zero_urbn.f debug64/main.o
	${FC} ${ARCH64} ${FFLAG} ${DFLAG}  zero_urbn.f -o debug64/zero_urbn.o -I debug64

debug64_clean:
	rm -f ${NAMEDEBUG64}.exe
	rm -f debug64/*.o
	rm -f debug64/*.mod
	rm -f debug64/*~

OBJECTS_REL32=  rel32/addh.o rel32/albedo.o rel32/allocate_parms.o rel32/alph.o rel32/anfert.o rel32/apex_day.o rel32/apply.o rel32/ascrv.o rel32/atri.o rel32/aunif.o rel32/autoirr.o rel32/aveval.o rel32/bacteria.o rel32/biofilm.o rel32/biozone.o rel32/bmpfixed.o rel32/bmpinit.o rel32/bmp_det_pond.o rel32/bmp_ri_pond.o rel32/bmp_sand_filter.o rel32/bmp_sed_pond.o rel32/bmp_wet_pond.o rel32/buffer.o rel32/burnop.o rel32/canopyint.o rel32/caps.o rel32/carbon_new.o rel32/carbon_zhang2.o rel32/cfactor.o rel32/chkcst.o rel32/clgen.o rel32/clicon.o rel32/command.o rel32/conapply.o rel32/confert.o rel32/crackflow.o rel32/crackvol.o rel32/curno.o rel32/dailycn.o rel32/decay.o rel32/depstor.o rel32/distrib_bmps.o rel32/dormant.o rel32/drains.o rel32/dstn1.o rel32/ee.o rel32/eiusle.o rel32/enrsb.o rel32/erfc.o rel32/estimate_ksat.o rel32/etact.o rel32/etpot.o rel32/expo.o rel32/fert.o rel32/filter.o rel32/filtw.o rel32/finalbal.o rel32/gcycl.o rel32/getallo.o rel32/grass_wway.o rel32/graze.o rel32/grow.o rel32/gwmod.o rel32/gwmod_deep.o rel32/gwnutr.o rel32/gw_no3.o rel32/h2omgt_init.o rel32/harvestop.o rel32/harvgrainop.o rel32/harvkillop.o rel32/header.o rel32/headout.o rel32/hhnoqual.o rel32/hhwatqual.o rel32/hmeas.o rel32/hruaa.o rel32/hruallo.o rel32/hruday.o rel32/hrumon.o rel32/hrupond.o rel32/hrupondhr.o rel32/hruyr.o rel32/hydroinit.o rel32/icl.o rel32/impndaa.o rel32/impndday.o rel32/impndmon.o rel32/impndyr.o rel32/impnd_init.o rel32/irrigate.o rel32/irrsub.o rel32/irr_rch.o rel32/irr_res.o rel32/jdt.o rel32/killop.o rel32/lakeq.o rel32/latsed.o rel32/layersplit.o rel32/lwqdef.o rel32/main.o rel32/ncsed_leach.o rel32/ndenit.o rel32/newtillmix.o rel32/nfix.o rel32/nitvol.o rel32/nlch.o rel32/nminrl.o rel32/noqual.o rel32/npup.o rel32/nrain.o rel32/nup.o rel32/nuts.o rel32/openwth.o rel32/operatn.o rel32/orgn.o rel32/orgncswat.o rel32/origtile.o rel32/ovr_sed.o rel32/percmacro.o rel32/percmain.o rel32/percmicro.o rel32/pestlch.o rel32/pestw.o rel32/pesty.o rel32/pgen.o rel32/pgenhr.o rel32/pkq.o rel32/plantmod.o rel32/plantop.o rel32/pmeas.o rel32/pminrl.o rel32/pminrl2.o rel32/pond.o rel32/pondhr.o rel32/pothole.o rel32/potholehr.o rel32/print_hyd.o rel32/psed.o rel32/qman.o rel32/ran1.o rel32/rchaa.o rel32/rchday.o rel32/rchinit.o rel32/rchmon.o rel32/rchuse.o rel32/rchyr.o rel32/reachout.o rel32/readatmodep.o rel32/readbsn.o rel32/readchm.o rel32/readcnst.o rel32/readfcst.o rel32/readfert.o rel32/readfig.o rel32/readfile.o rel32/readgw.o rel32/readhru.o rel32/readinpt.o rel32/readlup.o rel32/readlwq.o rel32/readmgt.o rel32/readmon.o rel32/readops.o rel32/readpest.o rel32/readplant.o rel32/readpnd.o rel32/readres.o rel32/readrte.o rel32/readru.o rel32/readsdr.o rel32/readsepticbz.o rel32/readseptwq.o rel32/readsno.o rel32/readsol.o rel32/readsub.o rel32/readswq.o rel32/readtill.o rel32/readurban.o rel32/readwgn.o rel32/readwus.o rel32/readwwq.o rel32/readyr.o rel32/reccnst.o rel32/recday.o rel32/rechour.o rel32/recmon.o rel32/recyear.o rel32/regres.o rel32/res.o rel32/resbact.o rel32/resetlu.o rel32/reshr.o rel32/resinit.o rel32/resnut.o rel32/rewind_init.o rel32/rhgen.o rel32/rootfr.o rel32/route.o rel32/routels.o rel32/routeunit.o rel32/routres.o rel32/rsedaa.o rel32/rseday.o rel32/rsedmon.o rel32/rsedyr.o rel32/rtbact.o rel32/rtday.o rel32/rteinit.o rel32/rthmusk.o rel32/rthpest.o rel32/rthr.o rel32/rthsed.o rel32/rtmusk.o rel32/rtout.o rel32/rtpest.o rel32/rtsed.o rel32/rtsed_bagnold.o rel32/rtsed_kodatie.o rel32/rtsed_molinas_wu.o rel32/rtsed_yangsand.o rel32/sat_excess.o rel32/save.o rel32/saveconc.o rel32/schedule_ops.o rel32/sched_mgt.o rel32/simulate.o rel32/sim_initday.o rel32/sim_inityr.o rel32/slrgen.o rel32/smeas.o rel32/snom.o rel32/soil_chem.o rel32/soil_par.o rel32/soil_phys.o rel32/soil_write.o rel32/solp.o rel32/solt.o rel32/std1.o rel32/std2.o rel32/std3.o rel32/stdaa.o rel32/storeinitial.o rel32/structure.o rel32/subaa.o rel32/subbasin.o rel32/subday.o rel32/submon.o rel32/substor.o rel32/subwq.o rel32/subyr.o rel32/sub_subbasin.o rel32/sumhyd.o rel32/sumv.o rel32/surface.o rel32/surfstor.o rel32/surfst_h2o.o rel32/surq_daycn.o rel32/surq_greenampt.o rel32/swbl.o rel32/sweep.o rel32/swu.o rel32/tair.o rel32/tgen.o rel32/theta.o rel32/tillfactor.o rel32/tillmix.o rel32/tmeas.o rel32/tran.o rel32/transfer.o rel32/tstr.o rel32/ttcoef.o rel32/ttcoef_wway.o rel32/urban.o rel32/urbanhr.o rel32/urb_bmp.o rel32/varinit.o rel32/vbl.o rel32/virtual.o rel32/volq.o rel32/washp.o rel32/watbal.o rel32/water_hru.o rel32/watqual.o rel32/watqual2.o rel32/wattable.o rel32/watuse.o rel32/weatgn.o rel32/wetlan.o rel32/wmeas.o rel32/wndgen.o rel32/writea.o rel32/writeaa.o rel32/writed.o rel32/writem.o rel32/xmon.o rel32/ysed.o rel32/zero0.o rel32/zero1.o rel32/zero2.o rel32/zeroini.o rel32/zero_urbn.o

NAMEREL32=swat_rel32
rel32:rel32_mkdir ${NAMEREL32}

rel32_mkdir:
	mkdir -p rel32

${NAMEREL32}: ${OBJECTS_REL32}
	${FC} ${OBJECTS_REL32} ${ARCH32} -static -o ${NAMEREL32}


rel32/addh.o: addh.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  addh.f -o rel32/addh.o -I rel32

rel32/albedo.o: albedo.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  albedo.f -o rel32/albedo.o -I rel32

rel32/allocate_parms.o: allocate_parms.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  allocate_parms.f -o rel32/allocate_parms.o -I rel32

rel32/alph.o: alph.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  alph.f -o rel32/alph.o -I rel32

rel32/anfert.o: anfert.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  anfert.f -o rel32/anfert.o -I rel32

rel32/apex_day.o: apex_day.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  apex_day.f -o rel32/apex_day.o -I rel32

rel32/apply.o: apply.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  apply.f -o rel32/apply.o -I rel32

rel32/ascrv.o: ascrv.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  ascrv.f -o rel32/ascrv.o -I rel32

rel32/atri.o: atri.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  atri.f -o rel32/atri.o -I rel32

rel32/aunif.o: aunif.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  aunif.f -o rel32/aunif.o -I rel32

rel32/autoirr.o: autoirr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  autoirr.f -o rel32/autoirr.o -I rel32

rel32/aveval.o: aveval.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  aveval.f -o rel32/aveval.o -I rel32

rel32/bacteria.o: bacteria.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  bacteria.f -o rel32/bacteria.o -I rel32

rel32/biofilm.o: biofilm.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  biofilm.f -o rel32/biofilm.o -I rel32

rel32/biozone.o: biozone.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG} ${LONGFIX} biozone.f -o rel32/biozone.o -I rel32

rel32/bmpfixed.o: bmpfixed.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  bmpfixed.f -o rel32/bmpfixed.o -I rel32

rel32/bmpinit.o: bmpinit.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG} ${LONGFIX} bmpinit.f -o rel32/bmpinit.o -I rel32

rel32/bmp_det_pond.o: bmp_det_pond.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  bmp_det_pond.f -o rel32/bmp_det_pond.o -I rel32

rel32/bmp_ri_pond.o: bmp_ri_pond.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  bmp_ri_pond.f -o rel32/bmp_ri_pond.o -I rel32

rel32/bmp_sand_filter.o: bmp_sand_filter.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  bmp_sand_filter.f -o rel32/bmp_sand_filter.o -I rel32

rel32/bmp_sed_pond.o: bmp_sed_pond.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  bmp_sed_pond.f -o rel32/bmp_sed_pond.o -I rel32

rel32/bmp_wet_pond.o: bmp_wet_pond.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  bmp_wet_pond.f -o rel32/bmp_wet_pond.o -I rel32

rel32/buffer.o: buffer.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  buffer.f -o rel32/buffer.o -I rel32

rel32/burnop.o: burnop.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  burnop.f -o rel32/burnop.o -I rel32

rel32/canopyint.o: canopyint.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  canopyint.f -o rel32/canopyint.o -I rel32

rel32/caps.o: caps.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  caps.f -o rel32/caps.o -I rel32

rel32/carbon_new.o: carbon_new.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  carbon_new.f -o rel32/carbon_new.o -I rel32

rel32/carbon_zhang2.o: carbon_zhang2.f90 rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG} ${LONGFREE} carbon_zhang2.f90 -o rel32/carbon_zhang2.o -I rel32

rel32/cfactor.o: cfactor.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  cfactor.f -o rel32/cfactor.o -I rel32

rel32/chkcst.o: chkcst.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  chkcst.f -o rel32/chkcst.o -I rel32

rel32/clgen.o: clgen.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  clgen.f -o rel32/clgen.o -I rel32

rel32/clicon.o: clicon.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  clicon.f -o rel32/clicon.o -I rel32

rel32/command.o: command.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  command.f -o rel32/command.o -I rel32

rel32/conapply.o: conapply.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  conapply.f -o rel32/conapply.o -I rel32

rel32/confert.o: confert.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  confert.f -o rel32/confert.o -I rel32

rel32/crackflow.o: crackflow.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  crackflow.f -o rel32/crackflow.o -I rel32

rel32/crackvol.o: crackvol.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  crackvol.f -o rel32/crackvol.o -I rel32

rel32/curno.o: curno.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  curno.f -o rel32/curno.o -I rel32

rel32/dailycn.o: dailycn.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  dailycn.f -o rel32/dailycn.o -I rel32

rel32/decay.o: decay.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  decay.f -o rel32/decay.o -I rel32

rel32/depstor.o: depstor.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  depstor.f -o rel32/depstor.o -I rel32

rel32/distrib_bmps.o: distrib_bmps.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  distrib_bmps.f -o rel32/distrib_bmps.o -I rel32

rel32/dormant.o: dormant.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  dormant.f -o rel32/dormant.o -I rel32

rel32/drains.o: drains.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  drains.f -o rel32/drains.o -I rel32

rel32/dstn1.o: dstn1.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  dstn1.f -o rel32/dstn1.o -I rel32

rel32/ee.o: ee.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  ee.f -o rel32/ee.o -I rel32

rel32/eiusle.o: eiusle.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  eiusle.f -o rel32/eiusle.o -I rel32

rel32/enrsb.o: enrsb.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  enrsb.f -o rel32/enrsb.o -I rel32

rel32/erfc.o: erfc.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  erfc.f -o rel32/erfc.o -I rel32

rel32/estimate_ksat.o: estimate_ksat.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  estimate_ksat.f -o rel32/estimate_ksat.o -I rel32

rel32/etact.o: etact.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  etact.f -o rel32/etact.o -I rel32

rel32/etpot.o: etpot.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  etpot.f -o rel32/etpot.o -I rel32

rel32/expo.o: expo.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  expo.f -o rel32/expo.o -I rel32

rel32/fert.o: fert.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  fert.f -o rel32/fert.o -I rel32

rel32/filter.o: filter.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  filter.f -o rel32/filter.o -I rel32

rel32/filtw.o: filtw.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  filtw.f -o rel32/filtw.o -I rel32

rel32/finalbal.o: finalbal.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  finalbal.f -o rel32/finalbal.o -I rel32

rel32/gcycl.o: gcycl.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  gcycl.f -o rel32/gcycl.o -I rel32

rel32/getallo.o: getallo.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  getallo.f -o rel32/getallo.o -I rel32

rel32/grass_wway.o: grass_wway.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  grass_wway.f -o rel32/grass_wway.o -I rel32

rel32/graze.o: graze.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  graze.f -o rel32/graze.o -I rel32

rel32/grow.o: grow.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  grow.f -o rel32/grow.o -I rel32

rel32/gwmod.o: gwmod.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  gwmod.f -o rel32/gwmod.o -I rel32

rel32/gwmod_deep.o: gwmod_deep.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  gwmod_deep.f -o rel32/gwmod_deep.o -I rel32

rel32/gwnutr.o: gwnutr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  gwnutr.f -o rel32/gwnutr.o -I rel32

rel32/gw_no3.o: gw_no3.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  gw_no3.f -o rel32/gw_no3.o -I rel32

rel32/h2omgt_init.o: h2omgt_init.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  h2omgt_init.f -o rel32/h2omgt_init.o -I rel32

rel32/harvestop.o: harvestop.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  harvestop.f -o rel32/harvestop.o -I rel32

rel32/harvgrainop.o: harvgrainop.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  harvgrainop.f -o rel32/harvgrainop.o -I rel32

rel32/harvkillop.o: harvkillop.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  harvkillop.f -o rel32/harvkillop.o -I rel32

rel32/header.o: header.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  header.f -o rel32/header.o -I rel32

rel32/headout.o: headout.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  headout.f -o rel32/headout.o -I rel32

rel32/hhnoqual.o: hhnoqual.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hhnoqual.f -o rel32/hhnoqual.o -I rel32

rel32/hhwatqual.o: hhwatqual.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hhwatqual.f -o rel32/hhwatqual.o -I rel32

rel32/hmeas.o: hmeas.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hmeas.f -o rel32/hmeas.o -I rel32

rel32/hruaa.o: hruaa.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hruaa.f -o rel32/hruaa.o -I rel32

rel32/hruallo.o: hruallo.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hruallo.f -o rel32/hruallo.o -I rel32

rel32/hruday.o: hruday.f90 rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hruday.f90 -o rel32/hruday.o -I rel32

rel32/hrumon.o: hrumon.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hrumon.f -o rel32/hrumon.o -I rel32

rel32/hrupond.o: hrupond.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hrupond.f -o rel32/hrupond.o -I rel32

rel32/hrupondhr.o: hrupondhr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hrupondhr.f -o rel32/hrupondhr.o -I rel32

rel32/hruyr.o: hruyr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hruyr.f -o rel32/hruyr.o -I rel32

rel32/hydroinit.o: hydroinit.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  hydroinit.f -o rel32/hydroinit.o -I rel32

rel32/icl.o: icl.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  icl.f -o rel32/icl.o -I rel32

rel32/impndaa.o: impndaa.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  impndaa.f -o rel32/impndaa.o -I rel32

rel32/impndday.o: impndday.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  impndday.f -o rel32/impndday.o -I rel32

rel32/impndmon.o: impndmon.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  impndmon.f -o rel32/impndmon.o -I rel32

rel32/impndyr.o: impndyr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  impndyr.f -o rel32/impndyr.o -I rel32

rel32/impnd_init.o: impnd_init.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  impnd_init.f -o rel32/impnd_init.o -I rel32

rel32/irrigate.o: irrigate.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  irrigate.f -o rel32/irrigate.o -I rel32

rel32/irrsub.o: irrsub.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  irrsub.f -o rel32/irrsub.o -I rel32

rel32/irr_rch.o: irr_rch.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  irr_rch.f -o rel32/irr_rch.o -I rel32

rel32/irr_res.o: irr_res.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  irr_res.f -o rel32/irr_res.o -I rel32

rel32/jdt.o: jdt.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  jdt.f -o rel32/jdt.o -I rel32

rel32/killop.o: killop.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  killop.f -o rel32/killop.o -I rel32

rel32/lakeq.o: lakeq.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  lakeq.f -o rel32/lakeq.o -I rel32

rel32/latsed.o: latsed.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  latsed.f -o rel32/latsed.o -I rel32

rel32/layersplit.o: layersplit.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  layersplit.f -o rel32/layersplit.o -I rel32

rel32/lwqdef.o: lwqdef.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  lwqdef.f -o rel32/lwqdef.o -I rel32

rel32/main.o: main.f modparm.f
	${FC} ${ARCH32} ${FFLAG} ${RFLAG} ${LONGFIX} main.f -o rel32/main.o -J rel32

rel32/ncsed_leach.o: NCsed_leach.f90 rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  NCsed_leach.f90 -o rel32/ncsed_leach.o -I rel32

rel32/ndenit.o: ndenit.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  ndenit.f -o rel32/ndenit.o -I rel32

rel32/newtillmix.o: newtillmix.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  newtillmix.f -o rel32/newtillmix.o -I rel32

rel32/nfix.o: nfix.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  nfix.f -o rel32/nfix.o -I rel32

rel32/nitvol.o: nitvol.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  nitvol.f -o rel32/nitvol.o -I rel32

rel32/nlch.o: nlch.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  nlch.f -o rel32/nlch.o -I rel32

rel32/nminrl.o: nminrl.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  nminrl.f -o rel32/nminrl.o -I rel32

rel32/noqual.o: noqual.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  noqual.f -o rel32/noqual.o -I rel32

rel32/npup.o: npup.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  npup.f -o rel32/npup.o -I rel32

rel32/nrain.o: nrain.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  nrain.f -o rel32/nrain.o -I rel32

rel32/nup.o: nup.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  nup.f -o rel32/nup.o -I rel32

rel32/nuts.o: nuts.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  nuts.f -o rel32/nuts.o -I rel32

rel32/openwth.o: openwth.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  openwth.f -o rel32/openwth.o -I rel32

rel32/operatn.o: operatn.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  operatn.f -o rel32/operatn.o -I rel32

rel32/orgn.o: orgn.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  orgn.f -o rel32/orgn.o -I rel32

rel32/orgncswat.o: orgncswat.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  orgncswat.f -o rel32/orgncswat.o -I rel32

rel32/origtile.o: origtile.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  origtile.f -o rel32/origtile.o -I rel32

rel32/ovr_sed.o: ovr_sed.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG} ${LONGFIX} ovr_sed.f -o rel32/ovr_sed.o -I rel32

rel32/percmacro.o: percmacro.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  percmacro.f -o rel32/percmacro.o -I rel32

rel32/percmain.o: percmain.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG} ${LONGFIX} percmain.f -o rel32/percmain.o -I rel32

rel32/percmicro.o: percmicro.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  percmicro.f -o rel32/percmicro.o -I rel32

rel32/pestlch.o: pestlch.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pestlch.f -o rel32/pestlch.o -I rel32

rel32/pestw.o: pestw.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pestw.f -o rel32/pestw.o -I rel32

rel32/pesty.o: pesty.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pesty.f -o rel32/pesty.o -I rel32

rel32/pgen.o: pgen.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pgen.f -o rel32/pgen.o -I rel32

rel32/pgenhr.o: pgenhr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pgenhr.f -o rel32/pgenhr.o -I rel32

rel32/pkq.o: pkq.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pkq.f -o rel32/pkq.o -I rel32

rel32/plantmod.o: plantmod.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  plantmod.f -o rel32/plantmod.o -I rel32

rel32/plantop.o: plantop.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  plantop.f -o rel32/plantop.o -I rel32

rel32/pmeas.o: pmeas.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pmeas.f -o rel32/pmeas.o -I rel32

rel32/pminrl.o: pminrl.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pminrl.f -o rel32/pminrl.o -I rel32

rel32/pminrl2.o: pminrl2.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pminrl2.f -o rel32/pminrl2.o -I rel32

rel32/pond.o: pond.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pond.f -o rel32/pond.o -I rel32

rel32/pondhr.o: pondhr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pondhr.f -o rel32/pondhr.o -I rel32

rel32/pothole.o: pothole.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  pothole.f -o rel32/pothole.o -I rel32

rel32/potholehr.o: potholehr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  potholehr.f -o rel32/potholehr.o -I rel32

rel32/print_hyd.o: print_hyd.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  print_hyd.f -o rel32/print_hyd.o -I rel32

rel32/psed.o: psed.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  psed.f -o rel32/psed.o -I rel32

rel32/qman.o: qman.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  qman.f -o rel32/qman.o -I rel32

rel32/ran1.o: ran1.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  ran1.f -o rel32/ran1.o -I rel32

rel32/rchaa.o: rchaa.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rchaa.f -o rel32/rchaa.o -I rel32

rel32/rchday.o: rchday.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rchday.f -o rel32/rchday.o -I rel32

rel32/rchinit.o: rchinit.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rchinit.f -o rel32/rchinit.o -I rel32

rel32/rchmon.o: rchmon.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rchmon.f -o rel32/rchmon.o -I rel32

rel32/rchuse.o: rchuse.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rchuse.f -o rel32/rchuse.o -I rel32

rel32/rchyr.o: rchyr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rchyr.f -o rel32/rchyr.o -I rel32

rel32/reachout.o: reachout.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  reachout.f -o rel32/reachout.o -I rel32

rel32/readatmodep.o: readatmodep.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readatmodep.f -o rel32/readatmodep.o -I rel32

rel32/readbsn.o: readbsn.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readbsn.f -o rel32/readbsn.o -I rel32

rel32/readchm.o: readchm.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readchm.f -o rel32/readchm.o -I rel32

rel32/readcnst.o: readcnst.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readcnst.f -o rel32/readcnst.o -I rel32

rel32/readfcst.o: readfcst.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readfcst.f -o rel32/readfcst.o -I rel32

rel32/readfert.o: readfert.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readfert.f -o rel32/readfert.o -I rel32

rel32/readfig.o: readfig.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readfig.f -o rel32/readfig.o -I rel32

rel32/readfile.o: readfile.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readfile.f -o rel32/readfile.o -I rel32

rel32/readgw.o: readgw.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readgw.f -o rel32/readgw.o -I rel32

rel32/readhru.o: readhru.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readhru.f -o rel32/readhru.o -I rel32

rel32/readinpt.o: readinpt.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readinpt.f -o rel32/readinpt.o -I rel32

rel32/readlup.o: readlup.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readlup.f -o rel32/readlup.o -I rel32

rel32/readlwq.o: readlwq.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readlwq.f -o rel32/readlwq.o -I rel32

rel32/readmgt.o: readmgt.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readmgt.f -o rel32/readmgt.o -I rel32

rel32/readmon.o: readmon.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readmon.f -o rel32/readmon.o -I rel32

rel32/readops.o: readops.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readops.f -o rel32/readops.o -I rel32

rel32/readpest.o: readpest.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readpest.f -o rel32/readpest.o -I rel32

rel32/readplant.o: readplant.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readplant.f -o rel32/readplant.o -I rel32

rel32/readpnd.o: readpnd.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readpnd.f -o rel32/readpnd.o -I rel32

rel32/readres.o: readres.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readres.f -o rel32/readres.o -I rel32

rel32/readrte.o: readrte.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readrte.f -o rel32/readrte.o -I rel32

rel32/readru.o: readru.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readru.f -o rel32/readru.o -I rel32

rel32/readsdr.o: readsdr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readsdr.f -o rel32/readsdr.o -I rel32

rel32/readsepticbz.o: readsepticbz.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readsepticbz.f -o rel32/readsepticbz.o -I rel32

rel32/readseptwq.o: readseptwq.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readseptwq.f -o rel32/readseptwq.o -I rel32

rel32/readsno.o: readsno.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readsno.f -o rel32/readsno.o -I rel32

rel32/readsol.o: readsol.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readsol.f -o rel32/readsol.o -I rel32

rel32/readsub.o: readsub.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readsub.f -o rel32/readsub.o -I rel32

rel32/readswq.o: readswq.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readswq.f -o rel32/readswq.o -I rel32

rel32/readtill.o: readtill.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readtill.f -o rel32/readtill.o -I rel32

rel32/readurban.o: readurban.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readurban.f -o rel32/readurban.o -I rel32

rel32/readwgn.o: readwgn.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readwgn.f -o rel32/readwgn.o -I rel32

rel32/readwus.o: readwus.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readwus.f -o rel32/readwus.o -I rel32

rel32/readwwq.o: readwwq.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readwwq.f -o rel32/readwwq.o -I rel32

rel32/readyr.o: readyr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  readyr.f -o rel32/readyr.o -I rel32

rel32/reccnst.o: reccnst.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  reccnst.f -o rel32/reccnst.o -I rel32

rel32/recday.o: recday.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  recday.f -o rel32/recday.o -I rel32

rel32/rechour.o: rechour.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rechour.f -o rel32/rechour.o -I rel32

rel32/recmon.o: recmon.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  recmon.f -o rel32/recmon.o -I rel32

rel32/recyear.o: recyear.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  recyear.f -o rel32/recyear.o -I rel32

rel32/regres.o: regres.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  regres.f -o rel32/regres.o -I rel32

rel32/res.o: res.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  res.f -o rel32/res.o -I rel32

rel32/resbact.o: resbact.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  resbact.f -o rel32/resbact.o -I rel32

rel32/resetlu.o: resetlu.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  resetlu.f -o rel32/resetlu.o -I rel32

rel32/reshr.o: reshr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  reshr.f -o rel32/reshr.o -I rel32

rel32/resinit.o: resinit.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  resinit.f -o rel32/resinit.o -I rel32

rel32/resnut.o: resnut.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  resnut.f -o rel32/resnut.o -I rel32

rel32/rewind_init.o: rewind_init.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rewind_init.f -o rel32/rewind_init.o -I rel32

rel32/rhgen.o: rhgen.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rhgen.f -o rel32/rhgen.o -I rel32

rel32/rootfr.o: rootfr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rootfr.f -o rel32/rootfr.o -I rel32

rel32/route.o: route.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  route.f -o rel32/route.o -I rel32

rel32/routels.o: routels.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  routels.f -o rel32/routels.o -I rel32

rel32/routeunit.o: routeunit.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  routeunit.f -o rel32/routeunit.o -I rel32

rel32/routres.o: routres.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  routres.f -o rel32/routres.o -I rel32

rel32/rsedaa.o: rsedaa.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rsedaa.f -o rel32/rsedaa.o -I rel32

rel32/rseday.o: rseday.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rseday.f -o rel32/rseday.o -I rel32

rel32/rsedmon.o: rsedmon.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rsedmon.f -o rel32/rsedmon.o -I rel32

rel32/rsedyr.o: rsedyr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rsedyr.f -o rel32/rsedyr.o -I rel32

rel32/rtbact.o: rtbact.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtbact.f -o rel32/rtbact.o -I rel32

rel32/rtday.o: rtday.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtday.f -o rel32/rtday.o -I rel32

rel32/rteinit.o: rteinit.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rteinit.f -o rel32/rteinit.o -I rel32

rel32/rthmusk.o: rthmusk.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rthmusk.f -o rel32/rthmusk.o -I rel32

rel32/rthpest.o: rthpest.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rthpest.f -o rel32/rthpest.o -I rel32

rel32/rthr.o: rthr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rthr.f -o rel32/rthr.o -I rel32

rel32/rthsed.o: rthsed.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG} ${LONGFIX} rthsed.f -o rel32/rthsed.o -I rel32

rel32/rtmusk.o: rtmusk.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtmusk.f -o rel32/rtmusk.o -I rel32

rel32/rtout.o: rtout.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtout.f -o rel32/rtout.o -I rel32

rel32/rtpest.o: rtpest.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtpest.f -o rel32/rtpest.o -I rel32

rel32/rtsed.o: rtsed.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtsed.f -o rel32/rtsed.o -I rel32

rel32/rtsed_bagnold.o: rtsed_bagnold.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtsed_bagnold.f -o rel32/rtsed_bagnold.o -I rel32

rel32/rtsed_kodatie.o: rtsed_kodatie.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtsed_kodatie.f -o rel32/rtsed_kodatie.o -I rel32

rel32/rtsed_molinas_wu.o: rtsed_Molinas_Wu.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtsed_Molinas_Wu.f -o rel32/rtsed_molinas_wu.o -I rel32

rel32/rtsed_yangsand.o: rtsed_yangsand.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  rtsed_yangsand.f -o rel32/rtsed_yangsand.o -I rel32

rel32/sat_excess.o: sat_excess.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  sat_excess.f -o rel32/sat_excess.o -I rel32

rel32/save.o: save.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  save.f -o rel32/save.o -I rel32

rel32/saveconc.o: saveconc.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  saveconc.f -o rel32/saveconc.o -I rel32

rel32/schedule_ops.o: schedule_ops.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  schedule_ops.f -o rel32/schedule_ops.o -I rel32

rel32/sched_mgt.o: sched_mgt.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  sched_mgt.f -o rel32/sched_mgt.o -I rel32

rel32/simulate.o: simulate.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  simulate.f -o rel32/simulate.o -I rel32

rel32/sim_initday.o: sim_initday.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  sim_initday.f -o rel32/sim_initday.o -I rel32

rel32/sim_inityr.o: sim_inityr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  sim_inityr.f -o rel32/sim_inityr.o -I rel32

rel32/slrgen.o: slrgen.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  slrgen.f -o rel32/slrgen.o -I rel32

rel32/smeas.o: smeas.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  smeas.f -o rel32/smeas.o -I rel32

rel32/snom.o: snom.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  snom.f -o rel32/snom.o -I rel32

rel32/soil_chem.o: soil_chem.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  soil_chem.f -o rel32/soil_chem.o -I rel32

rel32/soil_par.o: soil_par.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  soil_par.f -o rel32/soil_par.o -I rel32

rel32/soil_phys.o: soil_phys.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  soil_phys.f -o rel32/soil_phys.o -I rel32

rel32/soil_write.o: soil_write.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  soil_write.f -o rel32/soil_write.o -I rel32

rel32/solp.o: solp.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  solp.f -o rel32/solp.o -I rel32

rel32/solt.o: solt.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  solt.f -o rel32/solt.o -I rel32

rel32/std1.o: std1.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  std1.f -o rel32/std1.o -I rel32

rel32/std2.o: std2.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  std2.f -o rel32/std2.o -I rel32

rel32/std3.o: std3.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  std3.f -o rel32/std3.o -I rel32

rel32/stdaa.o: stdaa.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  stdaa.f -o rel32/stdaa.o -I rel32

rel32/storeinitial.o: storeinitial.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  storeinitial.f -o rel32/storeinitial.o -I rel32

rel32/structure.o: structure.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  structure.f -o rel32/structure.o -I rel32

rel32/subaa.o: subaa.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  subaa.f -o rel32/subaa.o -I rel32

rel32/subbasin.o: subbasin.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  subbasin.f -o rel32/subbasin.o -I rel32

rel32/subday.o: subday.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  subday.f -o rel32/subday.o -I rel32

rel32/submon.o: submon.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  submon.f -o rel32/submon.o -I rel32

rel32/substor.o: substor.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  substor.f -o rel32/substor.o -I rel32

rel32/subwq.o: subwq.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  subwq.f -o rel32/subwq.o -I rel32

rel32/subyr.o: subyr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  subyr.f -o rel32/subyr.o -I rel32

rel32/sub_subbasin.o: sub_subbasin.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  sub_subbasin.f -o rel32/sub_subbasin.o -I rel32

rel32/sumhyd.o: sumhyd.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  sumhyd.f -o rel32/sumhyd.o -I rel32

rel32/sumv.o: sumv.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  sumv.f -o rel32/sumv.o -I rel32

rel32/surface.o: surface.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  surface.f -o rel32/surface.o -I rel32

rel32/surfstor.o: surfstor.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  surfstor.f -o rel32/surfstor.o -I rel32

rel32/surfst_h2o.o: surfst_h2o.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  surfst_h2o.f -o rel32/surfst_h2o.o -I rel32

rel32/surq_daycn.o: surq_daycn.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  surq_daycn.f -o rel32/surq_daycn.o -I rel32

rel32/surq_greenampt.o: surq_greenampt.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  surq_greenampt.f -o rel32/surq_greenampt.o -I rel32

rel32/swbl.o: swbl.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  swbl.f -o rel32/swbl.o -I rel32

rel32/sweep.o: sweep.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  sweep.f -o rel32/sweep.o -I rel32

rel32/swu.o: swu.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  swu.f -o rel32/swu.o -I rel32

rel32/tair.o: tair.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  tair.f -o rel32/tair.o -I rel32

rel32/tgen.o: tgen.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  tgen.f -o rel32/tgen.o -I rel32

rel32/theta.o: theta.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  theta.f -o rel32/theta.o -I rel32

rel32/tillfactor.o: tillfactor.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  tillfactor.f -o rel32/tillfactor.o -I rel32

rel32/tillmix.o: tillmix.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  tillmix.f -o rel32/tillmix.o -I rel32

rel32/tmeas.o: tmeas.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  tmeas.f -o rel32/tmeas.o -I rel32

rel32/tran.o: tran.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  tran.f -o rel32/tran.o -I rel32

rel32/transfer.o: transfer.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  transfer.f -o rel32/transfer.o -I rel32

rel32/tstr.o: tstr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  tstr.f -o rel32/tstr.o -I rel32

rel32/ttcoef.o: ttcoef.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  ttcoef.f -o rel32/ttcoef.o -I rel32

rel32/ttcoef_wway.o: ttcoef_wway.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  ttcoef_wway.f -o rel32/ttcoef_wway.o -I rel32

rel32/urban.o: urban.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  urban.f -o rel32/urban.o -I rel32

rel32/urbanhr.o: urbanhr.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  urbanhr.f -o rel32/urbanhr.o -I rel32

rel32/urb_bmp.o: urb_bmp.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  urb_bmp.f -o rel32/urb_bmp.o -I rel32

rel32/varinit.o: varinit.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  varinit.f -o rel32/varinit.o -I rel32

rel32/vbl.o: vbl.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  vbl.f -o rel32/vbl.o -I rel32

rel32/virtual.o: virtual.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  virtual.f -o rel32/virtual.o -I rel32

rel32/volq.o: volq.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  volq.f -o rel32/volq.o -I rel32

rel32/washp.o: washp.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  washp.f -o rel32/washp.o -I rel32

rel32/watbal.o: watbal.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  watbal.f -o rel32/watbal.o -I rel32

rel32/water_hru.o: water_hru.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  water_hru.f -o rel32/water_hru.o -I rel32

rel32/watqual.o: watqual.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  watqual.f -o rel32/watqual.o -I rel32

rel32/watqual2.o: watqual2.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  watqual2.f -o rel32/watqual2.o -I rel32

rel32/wattable.o: wattable.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  wattable.f -o rel32/wattable.o -I rel32

rel32/watuse.o: watuse.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  watuse.f -o rel32/watuse.o -I rel32

rel32/weatgn.o: weatgn.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  weatgn.f -o rel32/weatgn.o -I rel32

rel32/wetlan.o: wetlan.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  wetlan.f -o rel32/wetlan.o -I rel32

rel32/wmeas.o: wmeas.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  wmeas.f -o rel32/wmeas.o -I rel32

rel32/wndgen.o: wndgen.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  wndgen.f -o rel32/wndgen.o -I rel32

rel32/writea.o: writea.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  writea.f -o rel32/writea.o -I rel32

rel32/writeaa.o: writeaa.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  writeaa.f -o rel32/writeaa.o -I rel32

rel32/writed.o: writed.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  writed.f -o rel32/writed.o -I rel32

rel32/writem.o: writem.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  writem.f -o rel32/writem.o -I rel32

rel32/xmon.o: xmon.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  xmon.f -o rel32/xmon.o -I rel32

rel32/ysed.o: ysed.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  ysed.f -o rel32/ysed.o -I rel32

rel32/zero0.o: zero0.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  zero0.f -o rel32/zero0.o -I rel32

rel32/zero1.o: zero1.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  zero1.f -o rel32/zero1.o -I rel32

rel32/zero2.o: zero2.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  zero2.f -o rel32/zero2.o -I rel32

rel32/zeroini.o: zeroini.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  zeroini.f -o rel32/zeroini.o -I rel32

rel32/zero_urbn.o: zero_urbn.f rel32/main.o
	${FC} ${ARCH32} ${FFLAG} ${RFLAG}  zero_urbn.f -o rel32/zero_urbn.o -I rel32

rel32_clean:
	rm -f ${NAMEREL32}.exe
	rm -f rel32/*.o
	rm -f rel32/*.mod
	rm -f rel32/*~

OBJECTS_REL64=  rel64/addh.o rel64/albedo.o rel64/allocate_parms.o rel64/alph.o rel64/anfert.o rel64/apex_day.o rel64/apply.o rel64/ascrv.o rel64/atri.o rel64/aunif.o rel64/autoirr.o rel64/aveval.o rel64/bacteria.o rel64/biofilm.o rel64/biozone.o rel64/bmpfixed.o rel64/bmpinit.o rel64/bmp_det_pond.o rel64/bmp_ri_pond.o rel64/bmp_sand_filter.o rel64/bmp_sed_pond.o rel64/bmp_wet_pond.o rel64/buffer.o rel64/burnop.o rel64/canopyint.o rel64/caps.o rel64/carbon_new.o rel64/carbon_zhang2.o rel64/cfactor.o rel64/chkcst.o rel64/clgen.o rel64/clicon.o rel64/command.o rel64/conapply.o rel64/confert.o rel64/crackflow.o rel64/crackvol.o rel64/curno.o rel64/dailycn.o rel64/decay.o rel64/depstor.o rel64/distrib_bmps.o rel64/dormant.o rel64/drains.o rel64/dstn1.o rel64/ee.o rel64/eiusle.o rel64/enrsb.o rel64/erfc.o rel64/estimate_ksat.o rel64/etact.o rel64/etpot.o rel64/expo.o rel64/fert.o rel64/filter.o rel64/filtw.o rel64/finalbal.o rel64/gcycl.o rel64/getallo.o rel64/grass_wway.o rel64/graze.o rel64/grow.o rel64/gwmod.o rel64/gwmod_deep.o rel64/gwnutr.o rel64/gw_no3.o rel64/h2omgt_init.o rel64/harvestop.o rel64/harvgrainop.o rel64/harvkillop.o rel64/header.o rel64/headout.o rel64/hhnoqual.o rel64/hhwatqual.o rel64/hmeas.o rel64/hruaa.o rel64/hruallo.o rel64/hruday.o rel64/hrumon.o rel64/hrupond.o rel64/hrupondhr.o rel64/hruyr.o rel64/hydroinit.o rel64/icl.o rel64/impndaa.o rel64/impndday.o rel64/impndmon.o rel64/impndyr.o rel64/impnd_init.o rel64/irrigate.o rel64/irrsub.o rel64/irr_rch.o rel64/irr_res.o rel64/jdt.o rel64/killop.o rel64/lakeq.o rel64/latsed.o rel64/layersplit.o rel64/lwqdef.o rel64/main.o rel64/ncsed_leach.o rel64/ndenit.o rel64/newtillmix.o rel64/nfix.o rel64/nitvol.o rel64/nlch.o rel64/nminrl.o rel64/noqual.o rel64/npup.o rel64/nrain.o rel64/nup.o rel64/nuts.o rel64/openwth.o rel64/operatn.o rel64/orgn.o rel64/orgncswat.o rel64/origtile.o rel64/ovr_sed.o rel64/percmacro.o rel64/percmain.o rel64/percmicro.o rel64/pestlch.o rel64/pestw.o rel64/pesty.o rel64/pgen.o rel64/pgenhr.o rel64/pkq.o rel64/plantmod.o rel64/plantop.o rel64/pmeas.o rel64/pminrl.o rel64/pminrl2.o rel64/pond.o rel64/pondhr.o rel64/pothole.o rel64/potholehr.o rel64/print_hyd.o rel64/psed.o rel64/qman.o rel64/ran1.o rel64/rchaa.o rel64/rchday.o rel64/rchinit.o rel64/rchmon.o rel64/rchuse.o rel64/rchyr.o rel64/reachout.o rel64/readatmodep.o rel64/readbsn.o rel64/readchm.o rel64/readcnst.o rel64/readfcst.o rel64/readfert.o rel64/readfig.o rel64/readfile.o rel64/readgw.o rel64/readhru.o rel64/readinpt.o rel64/readlup.o rel64/readlwq.o rel64/readmgt.o rel64/readmon.o rel64/readops.o rel64/readpest.o rel64/readplant.o rel64/readpnd.o rel64/readres.o rel64/readrte.o rel64/readru.o rel64/readsdr.o rel64/readsepticbz.o rel64/readseptwq.o rel64/readsno.o rel64/readsol.o rel64/readsub.o rel64/readswq.o rel64/readtill.o rel64/readurban.o rel64/readwgn.o rel64/readwus.o rel64/readwwq.o rel64/readyr.o rel64/reccnst.o rel64/recday.o rel64/rechour.o rel64/recmon.o rel64/recyear.o rel64/regres.o rel64/res.o rel64/resbact.o rel64/resetlu.o rel64/reshr.o rel64/resinit.o rel64/resnut.o rel64/rewind_init.o rel64/rhgen.o rel64/rootfr.o rel64/route.o rel64/routels.o rel64/routeunit.o rel64/routres.o rel64/rsedaa.o rel64/rseday.o rel64/rsedmon.o rel64/rsedyr.o rel64/rtbact.o rel64/rtday.o rel64/rteinit.o rel64/rthmusk.o rel64/rthpest.o rel64/rthr.o rel64/rthsed.o rel64/rtmusk.o rel64/rtout.o rel64/rtpest.o rel64/rtsed.o rel64/rtsed_bagnold.o rel64/rtsed_kodatie.o rel64/rtsed_molinas_wu.o rel64/rtsed_yangsand.o rel64/sat_excess.o rel64/save.o rel64/saveconc.o rel64/schedule_ops.o rel64/sched_mgt.o rel64/simulate.o rel64/sim_initday.o rel64/sim_inityr.o rel64/slrgen.o rel64/smeas.o rel64/snom.o rel64/soil_chem.o rel64/soil_par.o rel64/soil_phys.o rel64/soil_write.o rel64/solp.o rel64/solt.o rel64/std1.o rel64/std2.o rel64/std3.o rel64/stdaa.o rel64/storeinitial.o rel64/structure.o rel64/subaa.o rel64/subbasin.o rel64/subday.o rel64/submon.o rel64/substor.o rel64/subwq.o rel64/subyr.o rel64/sub_subbasin.o rel64/sumhyd.o rel64/sumv.o rel64/surface.o rel64/surfstor.o rel64/surfst_h2o.o rel64/surq_daycn.o rel64/surq_greenampt.o rel64/swbl.o rel64/sweep.o rel64/swu.o rel64/tair.o rel64/tgen.o rel64/theta.o rel64/tillfactor.o rel64/tillmix.o rel64/tmeas.o rel64/tran.o rel64/transfer.o rel64/tstr.o rel64/ttcoef.o rel64/ttcoef_wway.o rel64/urban.o rel64/urbanhr.o rel64/urb_bmp.o rel64/varinit.o rel64/vbl.o rel64/virtual.o rel64/volq.o rel64/washp.o rel64/watbal.o rel64/water_hru.o rel64/watqual.o rel64/watqual2.o rel64/wattable.o rel64/watuse.o rel64/weatgn.o rel64/wetlan.o rel64/wmeas.o rel64/wndgen.o rel64/writea.o rel64/writeaa.o rel64/writed.o rel64/writem.o rel64/xmon.o rel64/ysed.o rel64/zero0.o rel64/zero1.o rel64/zero2.o rel64/zeroini.o rel64/zero_urbn.o

NAMEREL64=swat_rel64
rel64:rel64_mkdir ${NAMEREL64}

rel64_mkdir:
	mkdir -p rel64

${NAMEREL64}: ${OBJECTS_REL64}
	${FC} ${OBJECTS_REL64} ${ARCH64} -static -o ${NAMEREL64}


rel64/addh.o: addh.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  addh.f -o rel64/addh.o -I rel64

rel64/albedo.o: albedo.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  albedo.f -o rel64/albedo.o -I rel64

rel64/allocate_parms.o: allocate_parms.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  allocate_parms.f -o rel64/allocate_parms.o -I rel64

rel64/alph.o: alph.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  alph.f -o rel64/alph.o -I rel64

rel64/anfert.o: anfert.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  anfert.f -o rel64/anfert.o -I rel64

rel64/apex_day.o: apex_day.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  apex_day.f -o rel64/apex_day.o -I rel64

rel64/apply.o: apply.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  apply.f -o rel64/apply.o -I rel64

rel64/ascrv.o: ascrv.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  ascrv.f -o rel64/ascrv.o -I rel64

rel64/atri.o: atri.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  atri.f -o rel64/atri.o -I rel64

rel64/aunif.o: aunif.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  aunif.f -o rel64/aunif.o -I rel64

rel64/autoirr.o: autoirr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  autoirr.f -o rel64/autoirr.o -I rel64

rel64/aveval.o: aveval.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  aveval.f -o rel64/aveval.o -I rel64

rel64/bacteria.o: bacteria.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  bacteria.f -o rel64/bacteria.o -I rel64

rel64/biofilm.o: biofilm.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  biofilm.f -o rel64/biofilm.o -I rel64

rel64/biozone.o: biozone.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFIX} biozone.f -o rel64/biozone.o -I rel64

rel64/bmpfixed.o: bmpfixed.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  bmpfixed.f -o rel64/bmpfixed.o -I rel64

rel64/bmpinit.o: bmpinit.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFIX} bmpinit.f -o rel64/bmpinit.o -I rel64

rel64/bmp_det_pond.o: bmp_det_pond.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  bmp_det_pond.f -o rel64/bmp_det_pond.o -I rel64

rel64/bmp_ri_pond.o: bmp_ri_pond.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  bmp_ri_pond.f -o rel64/bmp_ri_pond.o -I rel64

rel64/bmp_sand_filter.o: bmp_sand_filter.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  bmp_sand_filter.f -o rel64/bmp_sand_filter.o -I rel64

rel64/bmp_sed_pond.o: bmp_sed_pond.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  bmp_sed_pond.f -o rel64/bmp_sed_pond.o -I rel64

rel64/bmp_wet_pond.o: bmp_wet_pond.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  bmp_wet_pond.f -o rel64/bmp_wet_pond.o -I rel64

rel64/buffer.o: buffer.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  buffer.f -o rel64/buffer.o -I rel64

rel64/burnop.o: burnop.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  burnop.f -o rel64/burnop.o -I rel64

rel64/canopyint.o: canopyint.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  canopyint.f -o rel64/canopyint.o -I rel64

rel64/caps.o: caps.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  caps.f -o rel64/caps.o -I rel64

rel64/carbon_new.o: carbon_new.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  carbon_new.f -o rel64/carbon_new.o -I rel64

rel64/carbon_zhang2.o: carbon_zhang2.f90 rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFREE} carbon_zhang2.f90 -o rel64/carbon_zhang2.o -I rel64

rel64/cfactor.o: cfactor.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  cfactor.f -o rel64/cfactor.o -I rel64

rel64/chkcst.o: chkcst.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  chkcst.f -o rel64/chkcst.o -I rel64

rel64/clgen.o: clgen.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  clgen.f -o rel64/clgen.o -I rel64

rel64/clicon.o: clicon.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  clicon.f -o rel64/clicon.o -I rel64

rel64/command.o: command.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  command.f -o rel64/command.o -I rel64

rel64/conapply.o: conapply.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  conapply.f -o rel64/conapply.o -I rel64

rel64/confert.o: confert.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  confert.f -o rel64/confert.o -I rel64

rel64/crackflow.o: crackflow.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  crackflow.f -o rel64/crackflow.o -I rel64

rel64/crackvol.o: crackvol.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  crackvol.f -o rel64/crackvol.o -I rel64

rel64/curno.o: curno.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  curno.f -o rel64/curno.o -I rel64

rel64/dailycn.o: dailycn.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  dailycn.f -o rel64/dailycn.o -I rel64

rel64/decay.o: decay.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  decay.f -o rel64/decay.o -I rel64

rel64/depstor.o: depstor.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  depstor.f -o rel64/depstor.o -I rel64

rel64/distrib_bmps.o: distrib_bmps.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  distrib_bmps.f -o rel64/distrib_bmps.o -I rel64

rel64/dormant.o: dormant.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  dormant.f -o rel64/dormant.o -I rel64

rel64/drains.o: drains.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  drains.f -o rel64/drains.o -I rel64

rel64/dstn1.o: dstn1.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  dstn1.f -o rel64/dstn1.o -I rel64

rel64/ee.o: ee.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  ee.f -o rel64/ee.o -I rel64

rel64/eiusle.o: eiusle.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  eiusle.f -o rel64/eiusle.o -I rel64

rel64/enrsb.o: enrsb.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  enrsb.f -o rel64/enrsb.o -I rel64

rel64/erfc.o: erfc.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  erfc.f -o rel64/erfc.o -I rel64

rel64/estimate_ksat.o: estimate_ksat.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  estimate_ksat.f -o rel64/estimate_ksat.o -I rel64

rel64/etact.o: etact.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  etact.f -o rel64/etact.o -I rel64

rel64/etpot.o: etpot.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  etpot.f -o rel64/etpot.o -I rel64

rel64/expo.o: expo.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  expo.f -o rel64/expo.o -I rel64

rel64/fert.o: fert.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  fert.f -o rel64/fert.o -I rel64

rel64/filter.o: filter.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  filter.f -o rel64/filter.o -I rel64

rel64/filtw.o: filtw.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  filtw.f -o rel64/filtw.o -I rel64

rel64/finalbal.o: finalbal.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  finalbal.f -o rel64/finalbal.o -I rel64

rel64/gcycl.o: gcycl.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  gcycl.f -o rel64/gcycl.o -I rel64

rel64/getallo.o: getallo.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  getallo.f -o rel64/getallo.o -I rel64

rel64/grass_wway.o: grass_wway.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  grass_wway.f -o rel64/grass_wway.o -I rel64

rel64/graze.o: graze.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  graze.f -o rel64/graze.o -I rel64

rel64/grow.o: grow.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  grow.f -o rel64/grow.o -I rel64

rel64/gwmod.o: gwmod.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  gwmod.f -o rel64/gwmod.o -I rel64

rel64/gwmod_deep.o: gwmod_deep.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  gwmod_deep.f -o rel64/gwmod_deep.o -I rel64

rel64/gwnutr.o: gwnutr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  gwnutr.f -o rel64/gwnutr.o -I rel64

rel64/gw_no3.o: gw_no3.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  gw_no3.f -o rel64/gw_no3.o -I rel64

rel64/h2omgt_init.o: h2omgt_init.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  h2omgt_init.f -o rel64/h2omgt_init.o -I rel64

rel64/harvestop.o: harvestop.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  harvestop.f -o rel64/harvestop.o -I rel64

rel64/harvgrainop.o: harvgrainop.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  harvgrainop.f -o rel64/harvgrainop.o -I rel64

rel64/harvkillop.o: harvkillop.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  harvkillop.f -o rel64/harvkillop.o -I rel64

rel64/header.o: header.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  header.f -o rel64/header.o -I rel64

rel64/headout.o: headout.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  headout.f -o rel64/headout.o -I rel64

rel64/hhnoqual.o: hhnoqual.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hhnoqual.f -o rel64/hhnoqual.o -I rel64

rel64/hhwatqual.o: hhwatqual.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hhwatqual.f -o rel64/hhwatqual.o -I rel64

rel64/hmeas.o: hmeas.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hmeas.f -o rel64/hmeas.o -I rel64

rel64/hruaa.o: hruaa.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hruaa.f -o rel64/hruaa.o -I rel64

rel64/hruallo.o: hruallo.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hruallo.f -o rel64/hruallo.o -I rel64

rel64/hruday.o: hruday.f90 rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hruday.f90 -o rel64/hruday.o -I rel64

rel64/hrumon.o: hrumon.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hrumon.f -o rel64/hrumon.o -I rel64

rel64/hrupond.o: hrupond.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hrupond.f -o rel64/hrupond.o -I rel64

rel64/hrupondhr.o: hrupondhr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hrupondhr.f -o rel64/hrupondhr.o -I rel64

rel64/hruyr.o: hruyr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hruyr.f -o rel64/hruyr.o -I rel64

rel64/hydroinit.o: hydroinit.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  hydroinit.f -o rel64/hydroinit.o -I rel64

rel64/icl.o: icl.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  icl.f -o rel64/icl.o -I rel64

rel64/impndaa.o: impndaa.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  impndaa.f -o rel64/impndaa.o -I rel64

rel64/impndday.o: impndday.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  impndday.f -o rel64/impndday.o -I rel64

rel64/impndmon.o: impndmon.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  impndmon.f -o rel64/impndmon.o -I rel64

rel64/impndyr.o: impndyr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  impndyr.f -o rel64/impndyr.o -I rel64

rel64/impnd_init.o: impnd_init.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  impnd_init.f -o rel64/impnd_init.o -I rel64

rel64/irrigate.o: irrigate.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  irrigate.f -o rel64/irrigate.o -I rel64

rel64/irrsub.o: irrsub.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  irrsub.f -o rel64/irrsub.o -I rel64

rel64/irr_rch.o: irr_rch.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  irr_rch.f -o rel64/irr_rch.o -I rel64

rel64/irr_res.o: irr_res.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  irr_res.f -o rel64/irr_res.o -I rel64

rel64/jdt.o: jdt.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  jdt.f -o rel64/jdt.o -I rel64

rel64/killop.o: killop.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  killop.f -o rel64/killop.o -I rel64

rel64/lakeq.o: lakeq.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  lakeq.f -o rel64/lakeq.o -I rel64

rel64/latsed.o: latsed.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  latsed.f -o rel64/latsed.o -I rel64

rel64/layersplit.o: layersplit.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  layersplit.f -o rel64/layersplit.o -I rel64

rel64/lwqdef.o: lwqdef.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  lwqdef.f -o rel64/lwqdef.o -I rel64

rel64/main.o: main.f modparm.f
	${FC} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFIX} main.f -o rel64/main.o -J rel64

rel64/ncsed_leach.o: NCsed_leach.f90 rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  NCsed_leach.f90 -o rel64/ncsed_leach.o -I rel64

rel64/ndenit.o: ndenit.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  ndenit.f -o rel64/ndenit.o -I rel64

rel64/newtillmix.o: newtillmix.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  newtillmix.f -o rel64/newtillmix.o -I rel64

rel64/nfix.o: nfix.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  nfix.f -o rel64/nfix.o -I rel64

rel64/nitvol.o: nitvol.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  nitvol.f -o rel64/nitvol.o -I rel64

rel64/nlch.o: nlch.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  nlch.f -o rel64/nlch.o -I rel64

rel64/nminrl.o: nminrl.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  nminrl.f -o rel64/nminrl.o -I rel64

rel64/noqual.o: noqual.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  noqual.f -o rel64/noqual.o -I rel64

rel64/npup.o: npup.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  npup.f -o rel64/npup.o -I rel64

rel64/nrain.o: nrain.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  nrain.f -o rel64/nrain.o -I rel64

rel64/nup.o: nup.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  nup.f -o rel64/nup.o -I rel64

rel64/nuts.o: nuts.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  nuts.f -o rel64/nuts.o -I rel64

rel64/openwth.o: openwth.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  openwth.f -o rel64/openwth.o -I rel64

rel64/operatn.o: operatn.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  operatn.f -o rel64/operatn.o -I rel64

rel64/orgn.o: orgn.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  orgn.f -o rel64/orgn.o -I rel64

rel64/orgncswat.o: orgncswat.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  orgncswat.f -o rel64/orgncswat.o -I rel64

rel64/origtile.o: origtile.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  origtile.f -o rel64/origtile.o -I rel64

rel64/ovr_sed.o: ovr_sed.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFIX} ovr_sed.f -o rel64/ovr_sed.o -I rel64

rel64/percmacro.o: percmacro.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  percmacro.f -o rel64/percmacro.o -I rel64

rel64/percmain.o: percmain.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFIX} percmain.f -o rel64/percmain.o -I rel64

rel64/percmicro.o: percmicro.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  percmicro.f -o rel64/percmicro.o -I rel64

rel64/pestlch.o: pestlch.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pestlch.f -o rel64/pestlch.o -I rel64

rel64/pestw.o: pestw.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pestw.f -o rel64/pestw.o -I rel64

rel64/pesty.o: pesty.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pesty.f -o rel64/pesty.o -I rel64

rel64/pgen.o: pgen.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pgen.f -o rel64/pgen.o -I rel64

rel64/pgenhr.o: pgenhr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pgenhr.f -o rel64/pgenhr.o -I rel64

rel64/pkq.o: pkq.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pkq.f -o rel64/pkq.o -I rel64

rel64/plantmod.o: plantmod.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  plantmod.f -o rel64/plantmod.o -I rel64

rel64/plantop.o: plantop.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  plantop.f -o rel64/plantop.o -I rel64

rel64/pmeas.o: pmeas.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pmeas.f -o rel64/pmeas.o -I rel64

rel64/pminrl.o: pminrl.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pminrl.f -o rel64/pminrl.o -I rel64

rel64/pminrl2.o: pminrl2.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pminrl2.f -o rel64/pminrl2.o -I rel64

rel64/pond.o: pond.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pond.f -o rel64/pond.o -I rel64

rel64/pondhr.o: pondhr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pondhr.f -o rel64/pondhr.o -I rel64

rel64/pothole.o: pothole.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  pothole.f -o rel64/pothole.o -I rel64

rel64/potholehr.o: potholehr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  potholehr.f -o rel64/potholehr.o -I rel64

rel64/print_hyd.o: print_hyd.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  print_hyd.f -o rel64/print_hyd.o -I rel64

rel64/psed.o: psed.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  psed.f -o rel64/psed.o -I rel64

rel64/qman.o: qman.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  qman.f -o rel64/qman.o -I rel64

rel64/ran1.o: ran1.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  ran1.f -o rel64/ran1.o -I rel64

rel64/rchaa.o: rchaa.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rchaa.f -o rel64/rchaa.o -I rel64

rel64/rchday.o: rchday.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rchday.f -o rel64/rchday.o -I rel64

rel64/rchinit.o: rchinit.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rchinit.f -o rel64/rchinit.o -I rel64

rel64/rchmon.o: rchmon.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rchmon.f -o rel64/rchmon.o -I rel64

rel64/rchuse.o: rchuse.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rchuse.f -o rel64/rchuse.o -I rel64

rel64/rchyr.o: rchyr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rchyr.f -o rel64/rchyr.o -I rel64

rel64/reachout.o: reachout.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  reachout.f -o rel64/reachout.o -I rel64

rel64/readatmodep.o: readatmodep.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readatmodep.f -o rel64/readatmodep.o -I rel64

rel64/readbsn.o: readbsn.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readbsn.f -o rel64/readbsn.o -I rel64

rel64/readchm.o: readchm.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readchm.f -o rel64/readchm.o -I rel64

rel64/readcnst.o: readcnst.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readcnst.f -o rel64/readcnst.o -I rel64

rel64/readfcst.o: readfcst.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readfcst.f -o rel64/readfcst.o -I rel64

rel64/readfert.o: readfert.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readfert.f -o rel64/readfert.o -I rel64

rel64/readfig.o: readfig.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readfig.f -o rel64/readfig.o -I rel64

rel64/readfile.o: readfile.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readfile.f -o rel64/readfile.o -I rel64

rel64/readgw.o: readgw.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readgw.f -o rel64/readgw.o -I rel64

rel64/readhru.o: readhru.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readhru.f -o rel64/readhru.o -I rel64

rel64/readinpt.o: readinpt.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readinpt.f -o rel64/readinpt.o -I rel64

rel64/readlup.o: readlup.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readlup.f -o rel64/readlup.o -I rel64

rel64/readlwq.o: readlwq.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readlwq.f -o rel64/readlwq.o -I rel64

rel64/readmgt.o: readmgt.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readmgt.f -o rel64/readmgt.o -I rel64

rel64/readmon.o: readmon.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readmon.f -o rel64/readmon.o -I rel64

rel64/readops.o: readops.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readops.f -o rel64/readops.o -I rel64

rel64/readpest.o: readpest.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readpest.f -o rel64/readpest.o -I rel64

rel64/readplant.o: readplant.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readplant.f -o rel64/readplant.o -I rel64

rel64/readpnd.o: readpnd.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readpnd.f -o rel64/readpnd.o -I rel64

rel64/readres.o: readres.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readres.f -o rel64/readres.o -I rel64

rel64/readrte.o: readrte.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readrte.f -o rel64/readrte.o -I rel64

rel64/readru.o: readru.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readru.f -o rel64/readru.o -I rel64

rel64/readsdr.o: readsdr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readsdr.f -o rel64/readsdr.o -I rel64

rel64/readsepticbz.o: readsepticbz.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readsepticbz.f -o rel64/readsepticbz.o -I rel64

rel64/readseptwq.o: readseptwq.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readseptwq.f -o rel64/readseptwq.o -I rel64

rel64/readsno.o: readsno.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readsno.f -o rel64/readsno.o -I rel64

rel64/readsol.o: readsol.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readsol.f -o rel64/readsol.o -I rel64

rel64/readsub.o: readsub.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readsub.f -o rel64/readsub.o -I rel64

rel64/readswq.o: readswq.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readswq.f -o rel64/readswq.o -I rel64

rel64/readtill.o: readtill.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readtill.f -o rel64/readtill.o -I rel64

rel64/readurban.o: readurban.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readurban.f -o rel64/readurban.o -I rel64

rel64/readwgn.o: readwgn.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readwgn.f -o rel64/readwgn.o -I rel64

rel64/readwus.o: readwus.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readwus.f -o rel64/readwus.o -I rel64

rel64/readwwq.o: readwwq.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readwwq.f -o rel64/readwwq.o -I rel64

rel64/readyr.o: readyr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  readyr.f -o rel64/readyr.o -I rel64

rel64/reccnst.o: reccnst.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  reccnst.f -o rel64/reccnst.o -I rel64

rel64/recday.o: recday.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  recday.f -o rel64/recday.o -I rel64

rel64/rechour.o: rechour.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rechour.f -o rel64/rechour.o -I rel64

rel64/recmon.o: recmon.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  recmon.f -o rel64/recmon.o -I rel64

rel64/recyear.o: recyear.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  recyear.f -o rel64/recyear.o -I rel64

rel64/regres.o: regres.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  regres.f -o rel64/regres.o -I rel64

rel64/res.o: res.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  res.f -o rel64/res.o -I rel64

rel64/resbact.o: resbact.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  resbact.f -o rel64/resbact.o -I rel64

rel64/resetlu.o: resetlu.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  resetlu.f -o rel64/resetlu.o -I rel64

rel64/reshr.o: reshr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  reshr.f -o rel64/reshr.o -I rel64

rel64/resinit.o: resinit.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  resinit.f -o rel64/resinit.o -I rel64

rel64/resnut.o: resnut.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  resnut.f -o rel64/resnut.o -I rel64

rel64/rewind_init.o: rewind_init.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rewind_init.f -o rel64/rewind_init.o -I rel64

rel64/rhgen.o: rhgen.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rhgen.f -o rel64/rhgen.o -I rel64

rel64/rootfr.o: rootfr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rootfr.f -o rel64/rootfr.o -I rel64

rel64/route.o: route.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  route.f -o rel64/route.o -I rel64

rel64/routels.o: routels.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  routels.f -o rel64/routels.o -I rel64

rel64/routeunit.o: routeunit.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  routeunit.f -o rel64/routeunit.o -I rel64

rel64/routres.o: routres.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  routres.f -o rel64/routres.o -I rel64

rel64/rsedaa.o: rsedaa.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rsedaa.f -o rel64/rsedaa.o -I rel64

rel64/rseday.o: rseday.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rseday.f -o rel64/rseday.o -I rel64

rel64/rsedmon.o: rsedmon.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rsedmon.f -o rel64/rsedmon.o -I rel64

rel64/rsedyr.o: rsedyr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rsedyr.f -o rel64/rsedyr.o -I rel64

rel64/rtbact.o: rtbact.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtbact.f -o rel64/rtbact.o -I rel64

rel64/rtday.o: rtday.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtday.f -o rel64/rtday.o -I rel64

rel64/rteinit.o: rteinit.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rteinit.f -o rel64/rteinit.o -I rel64

rel64/rthmusk.o: rthmusk.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rthmusk.f -o rel64/rthmusk.o -I rel64

rel64/rthpest.o: rthpest.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rthpest.f -o rel64/rthpest.o -I rel64

rel64/rthr.o: rthr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rthr.f -o rel64/rthr.o -I rel64

rel64/rthsed.o: rthsed.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG} ${LONGFIX} rthsed.f -o rel64/rthsed.o -I rel64

rel64/rtmusk.o: rtmusk.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtmusk.f -o rel64/rtmusk.o -I rel64

rel64/rtout.o: rtout.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtout.f -o rel64/rtout.o -I rel64

rel64/rtpest.o: rtpest.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtpest.f -o rel64/rtpest.o -I rel64

rel64/rtsed.o: rtsed.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtsed.f -o rel64/rtsed.o -I rel64

rel64/rtsed_bagnold.o: rtsed_bagnold.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtsed_bagnold.f -o rel64/rtsed_bagnold.o -I rel64

rel64/rtsed_kodatie.o: rtsed_kodatie.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtsed_kodatie.f -o rel64/rtsed_kodatie.o -I rel64

rel64/rtsed_molinas_wu.o: rtsed_Molinas_Wu.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtsed_Molinas_Wu.f -o rel64/rtsed_molinas_wu.o -I rel64

rel64/rtsed_yangsand.o: rtsed_yangsand.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  rtsed_yangsand.f -o rel64/rtsed_yangsand.o -I rel64

rel64/sat_excess.o: sat_excess.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  sat_excess.f -o rel64/sat_excess.o -I rel64

rel64/save.o: save.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  save.f -o rel64/save.o -I rel64

rel64/saveconc.o: saveconc.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  saveconc.f -o rel64/saveconc.o -I rel64

rel64/schedule_ops.o: schedule_ops.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  schedule_ops.f -o rel64/schedule_ops.o -I rel64

rel64/sched_mgt.o: sched_mgt.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  sched_mgt.f -o rel64/sched_mgt.o -I rel64

rel64/simulate.o: simulate.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  simulate.f -o rel64/simulate.o -I rel64

rel64/sim_initday.o: sim_initday.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  sim_initday.f -o rel64/sim_initday.o -I rel64

rel64/sim_inityr.o: sim_inityr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  sim_inityr.f -o rel64/sim_inityr.o -I rel64

rel64/slrgen.o: slrgen.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  slrgen.f -o rel64/slrgen.o -I rel64

rel64/smeas.o: smeas.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  smeas.f -o rel64/smeas.o -I rel64

rel64/snom.o: snom.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  snom.f -o rel64/snom.o -I rel64

rel64/soil_chem.o: soil_chem.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  soil_chem.f -o rel64/soil_chem.o -I rel64

rel64/soil_par.o: soil_par.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  soil_par.f -o rel64/soil_par.o -I rel64

rel64/soil_phys.o: soil_phys.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  soil_phys.f -o rel64/soil_phys.o -I rel64

rel64/soil_write.o: soil_write.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  soil_write.f -o rel64/soil_write.o -I rel64

rel64/solp.o: solp.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  solp.f -o rel64/solp.o -I rel64

rel64/solt.o: solt.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  solt.f -o rel64/solt.o -I rel64

rel64/std1.o: std1.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  std1.f -o rel64/std1.o -I rel64

rel64/std2.o: std2.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  std2.f -o rel64/std2.o -I rel64

rel64/std3.o: std3.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  std3.f -o rel64/std3.o -I rel64

rel64/stdaa.o: stdaa.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  stdaa.f -o rel64/stdaa.o -I rel64

rel64/storeinitial.o: storeinitial.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  storeinitial.f -o rel64/storeinitial.o -I rel64

rel64/structure.o: structure.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  structure.f -o rel64/structure.o -I rel64

rel64/subaa.o: subaa.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  subaa.f -o rel64/subaa.o -I rel64

rel64/subbasin.o: subbasin.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  subbasin.f -o rel64/subbasin.o -I rel64

rel64/subday.o: subday.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  subday.f -o rel64/subday.o -I rel64

rel64/submon.o: submon.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  submon.f -o rel64/submon.o -I rel64

rel64/substor.o: substor.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  substor.f -o rel64/substor.o -I rel64

rel64/subwq.o: subwq.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  subwq.f -o rel64/subwq.o -I rel64

rel64/subyr.o: subyr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  subyr.f -o rel64/subyr.o -I rel64

rel64/sub_subbasin.o: sub_subbasin.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  sub_subbasin.f -o rel64/sub_subbasin.o -I rel64

rel64/sumhyd.o: sumhyd.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  sumhyd.f -o rel64/sumhyd.o -I rel64

rel64/sumv.o: sumv.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  sumv.f -o rel64/sumv.o -I rel64

rel64/surface.o: surface.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  surface.f -o rel64/surface.o -I rel64

rel64/surfstor.o: surfstor.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  surfstor.f -o rel64/surfstor.o -I rel64

rel64/surfst_h2o.o: surfst_h2o.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  surfst_h2o.f -o rel64/surfst_h2o.o -I rel64

rel64/surq_daycn.o: surq_daycn.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  surq_daycn.f -o rel64/surq_daycn.o -I rel64

rel64/surq_greenampt.o: surq_greenampt.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  surq_greenampt.f -o rel64/surq_greenampt.o -I rel64

rel64/swbl.o: swbl.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  swbl.f -o rel64/swbl.o -I rel64

rel64/sweep.o: sweep.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  sweep.f -o rel64/sweep.o -I rel64

rel64/swu.o: swu.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  swu.f -o rel64/swu.o -I rel64

rel64/tair.o: tair.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  tair.f -o rel64/tair.o -I rel64

rel64/tgen.o: tgen.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  tgen.f -o rel64/tgen.o -I rel64

rel64/theta.o: theta.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  theta.f -o rel64/theta.o -I rel64

rel64/tillfactor.o: tillfactor.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  tillfactor.f -o rel64/tillfactor.o -I rel64

rel64/tillmix.o: tillmix.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  tillmix.f -o rel64/tillmix.o -I rel64

rel64/tmeas.o: tmeas.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  tmeas.f -o rel64/tmeas.o -I rel64

rel64/tran.o: tran.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  tran.f -o rel64/tran.o -I rel64

rel64/transfer.o: transfer.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  transfer.f -o rel64/transfer.o -I rel64

rel64/tstr.o: tstr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  tstr.f -o rel64/tstr.o -I rel64

rel64/ttcoef.o: ttcoef.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  ttcoef.f -o rel64/ttcoef.o -I rel64

rel64/ttcoef_wway.o: ttcoef_wway.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  ttcoef_wway.f -o rel64/ttcoef_wway.o -I rel64

rel64/urban.o: urban.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  urban.f -o rel64/urban.o -I rel64

rel64/urbanhr.o: urbanhr.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  urbanhr.f -o rel64/urbanhr.o -I rel64

rel64/urb_bmp.o: urb_bmp.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  urb_bmp.f -o rel64/urb_bmp.o -I rel64

rel64/varinit.o: varinit.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  varinit.f -o rel64/varinit.o -I rel64

rel64/vbl.o: vbl.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  vbl.f -o rel64/vbl.o -I rel64

rel64/virtual.o: virtual.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  virtual.f -o rel64/virtual.o -I rel64

rel64/volq.o: volq.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  volq.f -o rel64/volq.o -I rel64

rel64/washp.o: washp.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  washp.f -o rel64/washp.o -I rel64

rel64/watbal.o: watbal.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  watbal.f -o rel64/watbal.o -I rel64

rel64/water_hru.o: water_hru.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  water_hru.f -o rel64/water_hru.o -I rel64

rel64/watqual.o: watqual.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  watqual.f -o rel64/watqual.o -I rel64

rel64/watqual2.o: watqual2.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  watqual2.f -o rel64/watqual2.o -I rel64

rel64/wattable.o: wattable.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  wattable.f -o rel64/wattable.o -I rel64

rel64/watuse.o: watuse.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  watuse.f -o rel64/watuse.o -I rel64

rel64/weatgn.o: weatgn.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  weatgn.f -o rel64/weatgn.o -I rel64

rel64/wetlan.o: wetlan.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  wetlan.f -o rel64/wetlan.o -I rel64

rel64/wmeas.o: wmeas.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  wmeas.f -o rel64/wmeas.o -I rel64

rel64/wndgen.o: wndgen.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  wndgen.f -o rel64/wndgen.o -I rel64

rel64/writea.o: writea.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  writea.f -o rel64/writea.o -I rel64

rel64/writeaa.o: writeaa.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  writeaa.f -o rel64/writeaa.o -I rel64

rel64/writed.o: writed.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  writed.f -o rel64/writed.o -I rel64

rel64/writem.o: writem.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  writem.f -o rel64/writem.o -I rel64

rel64/xmon.o: xmon.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  xmon.f -o rel64/xmon.o -I rel64

rel64/ysed.o: ysed.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  ysed.f -o rel64/ysed.o -I rel64

rel64/zero0.o: zero0.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  zero0.f -o rel64/zero0.o -I rel64

rel64/zero1.o: zero1.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  zero1.f -o rel64/zero1.o -I rel64

rel64/zero2.o: zero2.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  zero2.f -o rel64/zero2.o -I rel64

rel64/zeroini.o: zeroini.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  zeroini.f -o rel64/zeroini.o -I rel64

rel64/zero_urbn.o: zero_urbn.f rel64/main.o
	${FC} ${ARCH64} ${FFLAG} ${RFLAG}  zero_urbn.f -o rel64/zero_urbn.o -I rel64

rel64_clean:
	rm -f ${NAMEREL64}.exe
	rm -f rel64/*.o
	rm -f rel64/*.mod
	rm -f rel64/*~

