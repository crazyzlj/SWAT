      subroutine biozone()
	    
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine conducts biophysical processes occuring 
!!    in the biozone layer of a septic HRU.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_bd(:)        |kg/m^3        |density of biomass 
!!    bio_bod(:)       |kg/ha         |BOD concentration in biozone
!!    biom(:)          |kg/ha         |biomass of live bacteria in biozone       
!!    bz_thk(:)        |mm            |thickness of biozone                    
!!    coeff_bod_dc(:)  |m^3/day       |BOD decay rate coefficient
!!    coeff_bod_conv(:)|none          |BOD to live bacteria biomass conversion factor
!!    coeff_denitr(:)  |none          |Denitrification rate coefficient
!!    coeff_fc1(:)     |none          |field capacity calibration parameter 1
!!    coeff_fc2(:)     |none          |field capacity calibration parameter 2  
!!    coeff_fecal(:)   |m^3/day       |Fecal coliform bacteria decay rate coefficient  
!!    coeff_mrt(:)     |none          |mortality rate coefficient          
!!    coeff_nitr(:)    |none          |Nitrification rate coefficient
!!    coeff_plq(:)     |none          |Conversion factor for plaque from TDS           
!!    coeff_rsp(:)     |none          |respiration rate coefficient          
!!    coeff_slg1(:)    |none          |slough-off calibration parameter
!!    coeff_slg2(:)    |none          |slough-off calibration parameter
!!    fcoli(:)         |cfu/100ml     |concentration of the fecal coliform in the biozone 
!!                     |              |septic tank effluent
!!    ihru             |none          |HRU number
!!    iida             |day           |day being simulated
!!    i_sep(:)         |none          |soil layer where biozone exists
!!    isep_typ(:)      |none          |Septic system type                 
!!    isep_opt(:)      |none          |Septic system operation flag (1=active,2=failing,0=not operated)                 
!!    plqm             |kg/ha         |plaque in biozone
!!    rbiom(:)         |kg/ha         |daily change in biomass of live bacteria
!!    rplqm            |kg/ha         |daily change in plaque
!!    sepday           |mm H2O        |percolation from soil layer
!!    sol_bd(:,:)      |Mg/m**3       |bulk density of the soil
!!    sol_fc(:,:)      |mm H2O        |amount of water available to plants in soil 
!!                                    |layer at field capacity (fc - wp),Index:(layer,HRU)
!!    sol_fon(:,:)     |kg N/ha       |amount of nitrogen stored in the fresh
!!                                    |organic (residue) pool
!!    sol_fop(:,:)     |kg P/ha       |amount of phosphorus stored in the fresh
!!                                    |organic (residue) pool
!!    sol_k(:,:)       |mm/hr         |saturated hydraulic conductivity of soil 
!!                                    |layer. Index:(layer,HRU)
!!    sol_nh3(:,:)     |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                    |pool in soil layer
!!    sol_nly(:)       |none          |number of soil layers 
!!    sol_no3(:,:)     |kg N/ha       |amount of nitrogen stored in the
!!                                    |nitrate pool in soil layer
!!    sol_orgn(:,:)    |kg N/ha       |amount of nitrogen stored in the stable
!!                                    |organic N pool
!!    sol_orgp(:,:)    |kg P/ha       |amount of phosphorus stored in the organic
!!                                    |P pool
!!    sol_por(:,:)     |none          |total porosity of soil layer expressed as a 
!!                                    |fraction of the total volume, Index:(layer,HRU)
!!    sol_solp(:,:)    |kg P/ha       |amount of phosohorus stored in solution
!!    sol_st(:,:)      |mm H2O        |amount of water stored in the soil layer
!!                                    |on any given day (less wp water)
!!    sol_ul(:,:)      |mm H2O        |amount of water held in the soil layer at
!!                                    |saturation (sat - wp water)
!!    sol_up(:,:)      |mm H2O/mm soil|water content of soil at -0.033 MPa (field
!!                                    |capacity)
!!    sol_z(:,:)       |mm            |depth to bottom of soil layer,Index:(layer,HRU)
!!    sptqs(:)         |m3/d/cap      |Flow rate of the septic tank effluent 
!!                     |              |per capita
!!    sptbodconcs(:)   |mg/l          |Biological Oxygen Demand of the septic 
!!                     |              |tank effluent  
!!    spttssconcs(:)   |mg/l          |Concentration of total suspended solid in the 
!!                     |              |septic tank effluent
!!    spttnconcs(:)    |mg/l          |Concentration of total nitrogen 
!!                     |              |in the septic tank effluent
!!    sptnh4concs(:)   |mg/l          |Concentration of total phosphorus 
!!                     |              |of the septic tank effluent
!!    sptno3concs(:)   |mg/l          |Concentration of nitrate 
!!                     |              |in the septic tank effluent
!!    sptno2concs(:)   |mg/l          |Concentration of nitrite 
!!                     |              |in the septic tank effluent
!!    sptorgnconcs(:)  |mg/l          |Concentration of organic nitrogen in 
!!                     |              |the septic tank effluent
!!    spttpconcs(:)    |mg/l          |Concentration of total phosphorus in 
!!                     |              |the septic tank effluent  
!!    sptminps(:)      |mg/l          |Concentration of mineral phosphorus in
!!                     |              |the septic tank effluent
!!    sptorgps(:)      |mg/l          |concentration of organic phosphorus in the 
!!                     |              |septic tank effluent
!!    sptfcolis(:)     |cfu/100ml     |concentration of the facel caliform in the 
!!                     |              |septic tank effluent
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                     |none          |                       

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bod_rt           |1/day         |BOD reaction rate
!!    bz_lyr           |none          |soil layer where biozone exists
!!    bz_vol           |m^3           |volume of biozone
!!    dentr_rt         |1/day         |denitrification reaction rate
!!    fcoli_rt         |1/day         |fecal coliform reaction rate
!!    isp              |none          |type of septic system for current hru
!!    ntr_rt           |1/day         |nitrification reaction rate
!!    rbod             |mg/l          |daily change in bod concentration
!!    rdenit           |kg/ha         |denitrification during the day
!!    rfcoli           |cfu/100ml     |daily change in fecal coliform
!!    rmort            |kg/ha         |daily mortality of bacteria
!!    rnit             |kg/ha         |nitrification during the day
!!    rrsp             |kg/ha         |daily resparation of bacteria
!!    rslg             |kg/ha         |daily slough-off bacteria
!!    rtof             |none          |weighting factor used to partition the 
!!                                    |organic N & P concentration of septic effluent
!!                                    |between the fresh organic and the stable 
!!                                    |organic pools

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!    Coded by J.Jeong and C.Santhi. BRC, Temple TX
!!    Septic algorithm adapted from Siegrist et al., 2005

      use parm
	implicit none
	      
!	real*8 ntr_rt
      integer bz_lyr, isp, ii,j,nly
	real*8 bz_vol, rtrate,bodconc, qin, qout,qmm,qvol,pormm,rplqm
	real*8 ntr_rt,dentr_rt, bod_rt, fcoli_rt,rtof,xx,bodi,bode
	real*8 rnit, rdenit, rbio, rmort, rrsp, rslg, rbod, rfcoli
	real*8 nh3_begin, nh3_end, nh3_inflw_ste, no3_begin, no3_end 
	real*8 no3_inflow_ste, bio_steintobz,bio_outbz,bza,qi,nperc
	real*8 hvol, solpconc, solpsorb, qlyr,qsrf
	real*8 n2,n3,n5,n6,n7,n8,p2,p3,p4
	real*8 solp_begin,solp_end,svolp,totalp,ctmp,plch

	j = ihru
	nly = sol_nly(j)
	isp = isep_typ(j) 	   !! J.Jeong 3/09/09
      bz_lyr = i_sep(j)    
	bza = hru_ha(j)
	bz_vol = bz_thk(j) * bza * 10. !m^3
	qlyr = qstemm(j)
	qsrf = 0
	
	!temperature correctioin factor for bacteria growth/dieoff (Eppley, 1972)
	ctmp = thbact ** (sol_tmp(bz_lyr,j) - 20.) 

	! initial water volume
	qi = (sol_st(bz_lyr,j)+sol_prk(bz_lyr-1,j)+qstemm(j)) * bza * 10. !m3
    ! STE volume
	qin = qstemm(j) * bza * 10. ! m^3
	! leaching to sub layer
	qout = bz_perc(j) * bza * 10. !m3/d
	! final volume
	hvol = sol_st(bz_lyr,j) * bza * 10.
	rtof = 0.5
      
	xx = qin / hru_ha(j) / 1000.

	!! Failing system: STE saturates upper soil layers
	if (isep_opt(j)==2) then
	  
	  ! increment the number of failing days
	  if(sep_tsincefail(j)>0) sep_tsincefail(j) = sep_tsincefail(j) + 1

      ! convert the failing system into an active system if duration of failing ends
	  if (sep_tsincefail(j) >= isep_tfail(j)) then
	     isep_opt(j) = 1
         sol_ul(bz_lyr,j)=bz_thk(j)*(sol_por(bz_lyr,j)-sol_wp(bz_lyr,j)) 
         sol_fc(bz_lyr,j)=bz_thk(j)*(sol_up(bz_lyr,j)-sol_wp(bz_lyr,j))
		 sol_nh3(bz_lyr,j) = 0
		 sol_no3(bz_lyr,j) = 0
		 sol_orgn(bz_lyr,j) = 0
		 sol_orgp(bz_lyr,j) = 0
		 sol_fop(bz_lyr,j) = 0
		 sol_solp(bz_lyr,j) = 0
         sol_actp(bz_lyr,j) = 0
		 biom(j) = 0		
         plqm(j) = 0
		 bio_bod(j) = 0
		 fcoli(j) = 0
		 sep_tsincefail(j) = 0
	  end if

	  return
	endif

	!! Active system

      bodi = bio_bod(j) * bza / qi * 1000.  !mg/l

	!! Field capacity in the biozone Eq. 4-6  ! 
      sol_fc(bz_lyr,j) = sol_fc(bz_lyr,j) + coeff_fc1(j) *              
     &   (sol_ul(bz_lyr,j) - sol_fc(bz_lyr,j)) ** coeff_fc2(j)          
     &      * rbiom(j) / (bio_bd(j) * 10)

	!! Saturated water content in the biozone - Eq. 4-7    
	! mm = mm - kg/ha / (kg/m^3 * 10)
      sol_ul(bz_lyr,j)=sol_por(bz_lyr,j)*bz_thk(j)-plqm(j)              
     &/(bio_bd(j)*10.)

	if(sol_ul(bz_lyr,j).le.sol_fc(bz_lyr,j)) then
	  sol_ul(bz_lyr,j) = sol_fc(bz_lyr,j)
	  isep_opt(j) = 2
	  failyr(j) = iida/365. + curyr - 1
	endif
     
	!! Respiration rate(kg/ha)  Eq. 4-2   
	rrsp = ctmp * coeff_rsp(j) * biom(j) 

	!! Mortality rate(kg/ha) Eq. 4-3      
	rmort = ctmp * coeff_mrt(j) * biom(j) 

	!! Slough-off rate(kg/ha)      
	rslg = coeff_slg1(j) * bz_perc(j) ** coeff_slg2(j) * biom(j) 
			
	
	!! Build up of plqm(kg/ha) Eq.4-5
	! kg/ha (perday) = kg/ha + dimensionless * m^3/d * mg/l / (1000*ha)
      rplqm = (rmort - rslg) + coeff_plq(j) * qin *                     
     &                 spttssconcs(isp) / (1000. * bza)  
	rplqm = max(0.,rplqm)

	!! Add build up to plqm  ! kg/ha = kg/ha + kg/ha 
      plqm(j) = plqm(j) + rplqm
	
!	bio_steintobz = coeff_bod_conv(j) * qvol * sptbodconcs(isp) / (1000. * bza)     
!	bio_outbz = coeff_bod_conv(j) * qout * bodconc / (1000. * bza) + (rrsp + rmort + rslg)
	nh3_inflw_ste = xx * sptnh4concs(isp)
	no3_inflow_ste = xx * (sptno3concs(isp) + sptno2concs(isp)) 
	nh3_begin = sol_nh3(bz_lyr,j)
	no3_begin = sol_no3(bz_lyr,j)
	solp_begin = sol_solp(bz_lyr,j)

	!! Add STE f.coli concentration by volumetric averaging
      xx = 10.* sol_st(bz_lyr,j) * bza / (qin                           
     &     + 10.* sol_st(bz_lyr,j) * bza)
	fcoli(j) = fcoli(j) * xx + sptfcolis(isp) * (1.- xx)  ! J.Jeong 3/09/09
	
	!! nutrients reaction rate (Equation 4-13)
	rtrate =  biom(j) * bza / (bz_vol * sol_por(bz_lyr,j))
		      
	!! BOD (kg/ha) 4-14 ! 
 	bod_rt = max(0.,coeff_bod_dc(j) * rtrate)		!bod
      if (bod_rt>4) bod_rt=4
	rbod = bodi * (1.- Exp(-bod_rt))
      bode = bodi - rbod					!mg/l
	bio_bod(j) = bode * (sol_st(bz_lyr,j)*10)/1000. !kg/ha

	!! Fecal coliform(cfu/100ml) Eq 4-14, J.Jeong 3/09/09
	fcoli_rt = max(0.,coeff_fecal(j) * rtrate)		!fecal coliform
	rfcoli = fcoli(j) * (1.- exp(-fcoli_rt))
	fcoli(j) = fcoli(j) - rfcoli

	!! change in nh3 & no3 in soil pools due to nitrification(kg/ha) Eq.4-13, 4-14  
	ntr_rt = max(0.,coeff_nitr(j) * rtrate)			!nitrification
	rnit = sol_nh3(bz_lyr,j) * (1. - Exp(-ntr_rt)) !! J.Jeong 4/03/09
	sol_nh3(bz_lyr,j) = sol_nh3(bz_lyr,j) - rnit	!J.Jeong 3/09/09
	sol_no3(bz_lyr,j) = sol_no3(bz_lyr,j) + rnit	!J.Jeong 3/09/09
	
	!ammonium percolation
	nperc = 0.2 * qout / qi * sol_nh3(bz_lyr,j)
	nperc = min(nperc,0.5*sol_nh3(bz_lyr,j))
	sol_nh3(bz_lyr,j) = sol_nh3(bz_lyr,j) - nperc
	sol_nh3(bz_lyr+1,j) = sol_nh3(bz_lyr+1,j) + nperc

	!! denitrification,(kg/ha) Eq 4-14  
	dentr_rt = max(0.,coeff_denitr(j) * rtrate)		!denitrification
      rdenit = sol_no3(bz_lyr,j) * (1. - Exp(-dentr_rt))	!J.Jeong 3/09/09
	sol_no3(bz_lyr,j) = sol_no3(bz_lyr,j) - rdenit		!J.Jeong 3/09/09

 	!soil volume for sorption: soil thickness below biozone 
      svolp = (sol_z(nly,j) - bz_z(j)) * bza * 10.*(1-sol_por(bz_lyr,j))!m3, 
   
   !max adsorption amnt: linear isotherm, McCray 2005
      solpconc = sol_solp(bz_lyr,j) * bza / qi * 1000. !mg/l
	solpsorb = min(coeff_pdistrb(j) * solpconc,coeff_psorpmax(j)) !mgP/kgSoil
	solpsorb = 1.6 * 1.e-3 * solpsorb * svolp  !kgP sorption potential	

  !check if max. P sorption is reached 
      if(sol_solp(bz_lyr,j)*bza<solpsorb) then
        totalp = sol_solp(bz_lyr,j)+sol_actp(bz_lyr,j) ! kg/ha
        totalp = totalp * bza *1e3 / (1.6 * svolp) !mgP/kg Soil
        solp_end = coeff_solpslp(j) * totalp  + coeff_solpintc(j) !mgP/kgSoil; Bond et al. (2006)
        solp_end = 1.6 * 1.e-3 * solp_end * svolp / bza ! kg/ha
        if (solp_end>sol_solp(bz_lyr,j)) then
          solp_end = sol_solp(bz_lyr,j)
        endif 
       sol_actp(bz_lyr,j)=sol_actp(bz_lyr,j)+sol_solp(bz_lyr,j)-solp_end
        sol_solp(bz_lyr,j) = solp_end
      endif
      
      solpconc = sol_solp(bz_lyr,j) * bza / qi * 1000. !mg/l
	plch = solpconc * qout / bza * 1.e-3 !kg/ha
	sol_solp(bz_lyr,j) = sol_solp(bz_lyr,j) - plch !kg/ha
      sol_solp(bz_lyr+1,j) = sol_solp(bz_lyr+1,j) + plch !kg/ha	     
      nh3_end = sol_nh3(bz_lyr,j)
	no3_end = sol_no3(bz_lyr,j)
      solp_end = sol_solp(bz_lyr,j)  

	!! daily change in live bacteria biomass(kg/ha) Eq. 4-1 
	! kg/ha = m^3 * mg/L/(1000.*ha)6
      rbiom(j) = ctmp * coeff_bod_conv(j)*(qin*sptbodconcs(isp)-qout     
     &         * bode) / (1000. * bza) - (rrsp + rmort + rslg)         
      rbiom(j) = max(1.e-06,rbiom(j))

	!! total live biomass in biozone(kg/ha)    
	biom(j) = biom(j) + rbiom(j)

	!! summary calculations  
      if (curyr > nyskip) then
        wshd_nitn = wshd_nitn + rnit * hru_dafr(j)
        wshd_dnit = wshd_dnit + rdenit * hru_dafr(j)
        wdntl = wdntl + rdenit
      end if
	       
	!! print out time seriese results. header is in "readfile.f"
      if(curyr>nyskip) then
         n2=nh3_begin
         n3=nh3_end
         n5=no3_begin
         n6=no3_end !*bza/hvol*1000
         n7=rnit
         n8=rdenit
         p2=solp_begin
         p3=solp_end
         p4 = solpconc

	write(173,1000) ihru,iyr,iida,precipday,bz_perc(j),sol_ul(bz_lyr,j),
     &  sol_st(bz_lyr,j),sol_fc(bz_lyr,j),n2,n3,n5,n6,   
     &  n7,n8,p2,p3,p4
	endif 	

!! output.std variables added 03/01/2011 jga
        wshd_sepno3 = wshd_sepno3 + xx * (sptno3concs(isp) +            
     &        sptno2concs(isp)) * hru_dafr(j)
        wshd_sepnh3 = wshd_sepnh3 + xx * sptnh4concs(isp) * hru_dafr(j)
        wshd_seporgn = wshd_seporgn + xx * sptorgnconcs(isp) *          
     &     rtof * hru_dafr(j)
        wshd_sepfon = wshd_sepfon + xx * sptorgnconcs(isp)*             
     &    (1.-rtof) * hru_dafr(j)
        wshd_seporgp = wshd_seporgp + xx * sptorgps(isp) *              
     &    rtof * hru_dafr(j)
        wshd_sepfop = wshd_sepfop + xx * sptorgps(isp) *                
     &    (1.-rtof) * hru_dafr(j)
        wshd_sepsolp = wshd_sepsolp + xx * sptminps(isp) * hru_dafr(j)
        wshd_sepbod = wshd_sepbod + xx * sptbodconcs(isp) * hru_dafr(j)
        wshd_sepmm = wshd_sepmm + qstemm(j) * hru_dafr(j)
!! output.std variables added 03/01/2011 jga

        
1000  format(3i5,50es15.4)
      return
      end 