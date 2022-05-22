      subroutine soil_chem

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes soil chemical properties

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hrupest(:)    |none          |pesticide use flag:
!!                                 | 0: no pesticides used in HRU
!!                                 | 1: pesticides used in HRU
!!    i             |none          |HRU number
!!    nactfr        |none          |nitrogen active pool fraction. The fraction
!!                                 |of organic nitrogen in the active pool.
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    npno(:)       |none          |array of unique pesticides used in watershed
!!    psp           |none          |Phosphorus availability index. The fraction
!!                                 |of fertilizer P remaining in labile pool
!!                                 |after initial rapid phase of P sorption.
!!    skoc(:)       |(mg/kg)/(mg/L)|soil adsorption coefficient normalized
!!                                 |for soil organic carbon content
!!    sol_bd(:,:)   |Mg/m**3       |bulk density of the soil
!!    sol_cbn(:,:)  |%             |percent organic carbon in soil layer
!!    sol_nly(:)    |none          |number of soil layers 
!!    sol_no3(:,:)  |mg N/kg soil  |nitrate concentration in soil layer
!!    sol_orgn(:,:) |mg/kg         |organic N concentration in soil layer
!!    sol_orgp(:,:) |mg/kg         |organic P concentration in soil layer
!!    sol_pst(:,:,1)|kg/ha         |initial amount of pesticide in first layer
!!                                 |read in from .chm file
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil layer
!!                                 |classified as residue
!!    sol_solp(:,:) |mg/kg         |solution P concentration in soil layer
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    basminpi      |kg P/ha       |average amount of phosphorus initially in
!!                                 |the mineral P pool in watershed soil
!!    basno3i       |kg N/ha       |average amount of nitrogen initially in the
!!                                 |nitrate pool in watershed soil
!!    basorgni      |kg N/ha       |average amount of nitrogen initially in
!!                                 |the organic N pool in watershed soil
!!    basorgpi      |kg P/ha       |average amount of phosphorus initially in
!!                                 |the organic P pool in watershed soil
!!    conv_wt(:,:)  |none          |factor which converts kg/kg soil to kg/ha
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_cov(:)    |kg/ha         |amount of residue on soil surface
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_hum(:,:)  |kg humus/ha   |amount of organic matter in the soil layer
!!                                 |classified as humic substances
!!    sol_kp(:,:,:) |(mg/kg)/(mg/L)|pesticide sorption coefficient, Kp; the
!!                                 |ratio of the concentration in the solid
!!                                 |phase to the concentration in solution
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool. This variable is read in as
!!                                 |a concentration and converted to kg/ha.
!!                                 |(this value is read from the .sol file in
!!                                 |units of mg/kg)
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool NOTE UNIT CHANGE!
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool NOTE UNIT CHANGE!
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer NOTE UNIT
!!                                 |CHANGE!
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!                                 |NOTE UNIT CHANGE!
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus 
!!                                 |pool
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dg          |mm            |depth of layer
!!    j           |none          |counter
!!    jj          |none          |dummy variable to hold value
!!    n           |none          |counter
!!    nly         |none          |number of soil layers
!!    soldepth    |mm            |depth from bottom of 1st soil layer to
!!                               |the bottom of the layer of interest
!!    solpst      |mg/kg         |concentration of pesticide in soil
!!    summinp     |kg P/ha       |amount of phosphorus stored in the mineral P
!!                               |pool in the profile
!!    sumno3      |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in the soil profile
!!    sumorgn     |kg N/ha       |amount of nitrogen stored in the organic N
!!                               |pools in the profile
!!    sumorgp     |kg P/ha       |amount of phosphorus stored in the organic P
!!                               |pools in the profile
!!    wt1         |none          |converts mg/kg (ppm) to kg/ha
!!    xx          |none          |variable to hold value
!!    zdst        |none          |variable to hold value
!!    labfrac     |none          |fraction of total soil mineral P which is labile
!!    soil_TP	  |kg/ha         |Total Soil Mineral P
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: nly, j, jj, n
      real*8 :: xx, dg, wt1, zdst, soldepth, sumno3, sumorgn, summinp
      real*8 :: sumorgp, solpst, soil_TP, labfrac,solp
      
      !!by zhang
      !!=============
          real*8 :: sol_mass
          real*8 :: FBM, FHP, RTNO, FHS, X1, RTO, sol_min_n
          sol_mass = 0.
          DG = 0.
          FBM = 0.
          FHP = 0.
          RTNO = 0.
          FHS = 0.
          X1 = 0.
          RTO = 0.
      !!by zhang
      !!=============
      
      
      nly = 0
      solpst = 0.
      sumno3 = 0.
      sumorgn = 0.
      summinp = 0.
      sumorgp = 0.
      nly = sol_nly(i)

!!    calculate sol_cbn for lower layers if only have upper layer
      if (nly >= 3 .and. sol_cbn(3,i) <= 0) then
        do j = 3, nly
          if (sol_cbn(j,i) == 0.) then
            soldepth = 0
            soldepth = sol_z(j,i) - sol_z(2,i)
            sol_cbn(j,i) = sol_cbn(j-1,i) * Exp(-.001 * soldepth)
          end if
        end do
      end if

      cmup_kgh = 0.
      cmtot_kgh = 0.
      do j = 1, nly
        if (j == 1) then
          sol_thick = sol_z(j,i)
        else
          sol_thick = sol_z(j,i) - sol_z(j-1,i)
        end if
 
!! soil carbon and nitrogen
        sol_mass = (sol_thick / 1000.) * 10000. * sol_bd(j,i)
     &       * 1000. * (1 - sol_rock(j,i) / 100.)
        sol_cmass = sol_mass * (sol_cbn(j,i) / 100.)

        if (j == 1) cmup_kgh(i) = sol_cmass
        cmtot_kgh(i) = cmtot_kgh(i) + sol_cmass
      end do


!!    calculate sol_kp as function of koc and sol_cbn
!!    and set initial pesticide in all layers equal to value given for
!!    upper layer
      if (hrupest(i) == 1) then
      do j = 1, npmx
        jj = 0
        jj = npno(j)
        if (jj > 0) then
          solpst = 0.
          solpst = sol_pst(j,i,1)  !!concentration of pesticide in soil
          xx = 0.
          do n = 1, nly
            dg = 0.
            wt1 = 0.
            dg = (sol_z(n,i) - xx)
            xx = sol_z(n,i) 
            wt1 = sol_bd(n,i) * dg / 100.              !! mg/kg => kg/ha
            sol_kp(j,i,n) = skoc(jj) * sol_cbn(n,i) / 100.
            sol_pst(j,i,n) = solpst * wt1
          end do
        end if
      end do
      end if


!!    calculate initial nutrient contents of layers, profile and
!!    average in soil for the entire watershed
!!    convert mg/kg (ppm) to kg/ha
      xx = 0.
      sol_fop(1,i) = sol_rsd(1,i) * .0010 !! was 0.0003 Armen January 2009
      sol_fon(1,i) = sol_rsd(1,i) * .0055 !! was 0.0015 Armen January 2009
      sol_cov(i) = sol_rsd(1,i)
      do j = 1, nly
        dg = 0.
        wt1 = 0.
        dg = (sol_z(j,i) - xx)
        wt1 = sol_bd(j,i) * dg / 100.              !! mg/kg => kg/ha
        conv_wt(j,i) = 1.e6 * wt1                  !! kg/kg => kg/ha

        if (sol_no3(j,i) <= 0.) then
          zdst = 0.
          zdst = Exp(-sol_z(j,i) / 1000.)
          sol_no3(j,i) = 10. * zdst * .7
        end if
        sol_no3(j,i) = sol_no3(j,i) * wt1          !! mg/kg => kg/ha
        sumno3 = sumno3 + sol_no3(j,i)

        if (sol_orgn(j,i) > 0.0001) then
          sol_orgn(j,i) = sol_orgn(j,i) * wt1      !! mg/kg => kg/ha
        else
          !! assume C:N ratio of 10:1
          sol_orgn(j,i) = 10000. * (sol_cbn(j,i) / 14.) * wt1  !! CN ratio changed back to 14 cibin 03022012
        end if
        sol_aorgn(j,i) = sol_orgn(j,i) * nactfr
        sol_orgn(j,i) = sol_orgn(j,i) * (1. - nactfr)
        sumorgn = sumorgn + sol_aorgn(j,i) + sol_orgn(j,i) +            
     &            sol_fon(j,i)

        if (sol_orgp(j,i) > 0.0001) then
          sol_orgp(j,i) = sol_orgp(j,i) * wt1      !! mg/kg => kg/ha
        else
	!! assume N:P ratio of 8:1 
          sol_orgp(j,i) = .125 * sol_orgn(j,i)   
        end if

        if (sol_solp(j,i) > 0.0001) then
          sol_solp(j,i) = sol_solp(j,i) * wt1      !! mg/kg => kg/ha
        else
          !! assume initial concentration of 5 mg/kg
          sol_solp(j,i) = 5. * wt1
        end if

        !! Set active pool based on dynamic PSP MJW
		
	    if (sol_P_model == 0) then 
	      !! Allow Dynamic PSP Ratio
            !! convert to concentration
            solp = sol_solp(j,i) / conv_wt(j,i) * 1000000.
	      !! PSP = -0.045*log (% clay) + 0.001*(Solution P, mg kg-1) - 0.035*(% Organic C) + 0.43
	      if (sol_clay(j,i) > 0.) then
              psp(i) = -0.045 * log(sol_clay(j,i))+ (0.001 * solp) 
              psp(i) = psp(i) - (0.035  * sol_cbn(j,i)) + 0.43 
            else
              psp(i) = 0.4
            endif   		
            !! Limit PSP range
            if (psp(i) <.05) then
              psp(i) = 0.05
	      else if (psp(i) > 0.9) then
              psp(i) = 0.9
            end if
            end if
	    
        sol_actp(j,i) = sol_solp(j,i) * (1. - psp(i)) / psp(i)

          !! Set Stable pool based on dynamic coefficant
	    if (sol_P_model == 0) then  !! From White et al 2009 
            !! convert to concentration for ssp calculation
	      actp = sol_actp(j,i) / conv_wt(j,i) * 1000000.
		    solp = sol_solp(j,i) / conv_wt(j,i) * 1000000.
            !! estimate Total Mineral P in this soil based on data from sharpley 2004
		    ssp = 25.044 * (actp + solp)** (-0.3833)
		    !!limit SSP Range
		    if (SSP > 7.) SSP = 7.
		    if (SSP < 1.) SSP = 1.	      	  
		    sol_stap(j,i) = SSP * (sol_actp(j,i) + sol_solp(j,i))!define stableP
         else
	!! The original code
		  sol_stap(j,i) = 4. * sol_actp(j,i)
	   end if

        sol_hum(j,i) = sol_cbn(j,i) * wt1 * 17200.
        xx = sol_z(j,i)
        summinp = summinp + sol_solp(j,i) + sol_actp(j,i) +             
     &        sol_stap(j,i)
        sumorgp = sumorgp + sol_orgp(j,i) + sol_fop(j,i)
      end do

      basno3i = basno3i + sumno3 * hru_km(i) / da_km
      basorgni = basorgni + sumorgn * hru_km(i) / da_km
      basminpi = basminpi + summinp * hru_km(i) / da_km
      basorgpi = basorgpi + sumorgp * hru_km(i) / da_km

      !! By Zhang for C/N cycling
      !!=============================== 
      if (cswat == 2) then
      if (rsdin(i) > 0.) sol_rsd(1,i) = rsdin(i)		
	do j = 1, nly
		!!kg/ha sol mass in each layer
		if (j == 1) then
		    sol_mass = (sol_z(j,i)) / 1000.
          !&						10000. * sol_bd(j,ihru)* 1000. *			
          !&							(1- sol_rock(j,ihru) / 100.)
            sol_mass = sol_mass * 10000. * sol_bd(j,i)* 1000.
            sol_mass = sol_mass * (1- sol_rock(j,i) / 100.) 	
            
		else
		    sol_mass = (sol_z(j,i) - sol_z(j-1,i)) / 1000.
          !&						10000. * sol_bd(j,ihru)* 1000. *			
          !&							(1- sol_rock(j,ihru) / 100.)
            sol_mass = sol_mass * 10000. * sol_bd(j,i)* 1000.
            sol_mass = sol_mass * (1- sol_rock(j,i) / 100.) 			
		end if
		!!kg/ha mineral nitrogen
		sol_min_n = sol_no3(j,i)+sol_nh3(j,i)	     
 
        !XCB = 0.2
        !mm
        if (j == 1) then
            !DG = 10
            DG = sol_z(j,i)
        else
            DG = (sol_z(j,i) - sol_z(j-1,i))
        end if        
                
        !if(sol_WOC(j,ihru)<1.E-5) sol_WOC(j,ihru)=XCB*exp(-.001*DG)  
                                             
        !XCB=sol_WOC(j,ihru)                                                                     
        !XZ=sol_WOC(j,ihru) *.0172                                                                
        !ZZ=1.-XZ                                                                       
        !sol_BDM(j,ihru)=ZZ/(1./sol_BD(j,ihru)-XZ/.224)                                                   
	  !if(sol_BDM(j,ihru)<1.)then                                                                  
	  !    sol_BDM(j,ihru)=1.                                                                         
	  !    sol_BD(j,ihru)=1./(ZZ+XZ/.224)                                                             
	  !end if                                                                             
           
        
        !ton/ha
        !WT = sol_mass/1000.
        
        !WT1 = WT/1000.
        !X1 = 10. * sol_cbn(j,ihru) * WT
        !WT(J)=BD(J)*DG*10.                                                             
        !DG1=DG                                                                         
        !WT1=WT(J)/1000.                                                                
        !X1=10.*WOC(J)*WT(J) 
        !WOC(J)=X1  
        !kg/ha                                                           
        !sol_WOC(j,ihru)=X1
        sol_WOC(j,i) = sol_mass * sol_cbn(j,i)/100                  
        !if(sol_WON(j,ihru)>0.)then                                                             
        !      sol_WON(j,ihru)=WT1*sol_WON(j,ihru)  
        !      KK=0 
        !else
        sol_WON(j,i) = sol_aorgn(j,i)+  sol_orgn(j,i)!0.1 * sol_WOC(j,i)
        !      KK=1 
        !end if     

        !Frction of Mirobial Biomass, Humus Passive C pools
        FBM = 0.0
        FHP = 0.0
        IF(FBM<1.E-10)FBM=.04   
        RTN0 = 100.                   
        IF(FHP<1.E-10)FHP=.7-.4*EXP(-.0277*100) 
        FHS = 1 - FBM - FHP
        !From DSSAT
        !FBM = 0.02
        !FHS = 0.54
        !FHP = 0.44
				
		!NCC = 0
        !IF(NCC==0)THEN
            !sol_WBM(j,ihru)=FBM*X1
            sol_BM(j,i)=FBM*sol_WOC(j,i)                             
            sol_BMC(j,i)=sol_BM(j,i)                                  
            !IF(KK==0)THEN                                                                  
	            RTO=sol_WON(j,i)/sol_WOC(j,i)                         
	      !ELSE                                                                                
	      !      RTO=.1                                                                            
	      !END IF                                                                              
            sol_BMN(j,i)=RTO*sol_BMC(j,i)                              
            !sol_HP(j,ihru)=FHP*(X1-sol_BM(j,ihru))  
            sol_HP(j,i)=FHP*(sol_WOC(j,i)-sol_BM(j,i))               
            sol_HS(j,i)=sol_WOC(j,i)-sol_BM(j,i)-sol_HP(j,i)  
            !sol_HP(j,i)=sol_WOC(j,i)-sol_BM(j,i)-sol_HP(j,i)                                                                
            sol_HSC(j,i)=sol_HS(j,i)                                  
            sol_HSN(j,i)= RTO*sol_HSC(j,i)  !sol_aorgn(j,i)           
            sol_HPC(j,i)=sol_HP(j,i)                                 
            sol_HPN(j,i)= RTO*sol_HPC(j,i)  !sol_orgn(j,i)   
            
                                                                    
            X1=sol_rsd(j,i) /1000.  
            !!skip std in SWAT                                                                   
            !IF(j==1)X1=X1+STD(j)/1000.
                                                                  
            sol_LM(j,i)=500.*X1                                        
            sol_LS(j,i)=sol_LM(j,i)                                    
            sol_LSL(j,i)=.8*sol_LS(j,i)                              
            sol_LMC(j,i)=.42*sol_LM(j,i) 
                                                                        
            sol_LMN(j,i)=.1*sol_LMC(j,i)                            
            sol_LSC(j,i)=.42*sol_LS(j,i)                              
            sol_LSLC(j,i)=.8*sol_LSC(j,i)                            
            sol_LSLNC(j,i)=.2*sol_LSC(j,i)
            sol_LSN(j,i)=sol_LSC(j,i)/150.                          
            !sol_WOC(j,ihru)=sol_WOC(j,ihru)+sol_LSC(j,ihru)+sol_WLMC(j,ihru)
            sol_WOC(j,i)=sol_WOC(j,i)+sol_LSC(j,i)+sol_LMC(j,i)        
            !sol_WON(j,ihru)=sol_WON(j,ihru)+sol_LSN(j,ihru)+sol_WLMN(j,ihru)
            sol_WON(j,i)=sol_WON(j,i)+sol_LSN(j,i)+sol_LMN(j,i)
            !END IF 		
            
            !if (sol_orgn(j,i) > 0.0001) then
            !  sol_orgn(j,i) = sol_orgn(j,i) * wt1      !! mg/kg => kg/ha
            !else
              !! assume C:N ratio of 10:1
            !  sol_orgn(j,i) = 10000. * (sol_cbn(j,i) / 11.) * wt1  !! CN ratio was 14 before 01-22-09 Armen
            !end if
            sol_orgn(j,i) = sol_HPN(j,i)
            sol_aorgn(j,i) = sol_HSN(j,i)
            sol_fon(1,i) = sol_LMN(j,i) + sol_LSN(j,i)
            !sol_aorgn(j,i) = sol_orgn(j,i) * nactfr
            !sol_orgn(j,i) = sol_orgn(j,i) * (1. - nactfr)
            sumorgn = sumorgn + sol_aorgn(j,i) + sol_orgn(j,i) +
     &           sol_fon(j,i) + sol_BMN(j,i)
        
		
	end do	
	
	end if
      !! By Zhang for C/N cycling      
      	
	!!May need to think about moving the following lines which appear before in this module to the end of this module,
	!!because orgn has been re-calculated.
	!!============================
      !basno3i = basno3i + sumno3 * hru_km(i) / da_km
      !basorgni = basorgni + sumorgn * hru_km(i) / da_km
      !basminpi = basminpi + summinp * hru_km(i) / da_km
      !basorgpi = basorgpi + sumorgp * hru_km(i) / da_km

      return
      end
