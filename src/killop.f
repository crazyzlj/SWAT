      subroutine killop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the kill operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    curyr        |none          |current year of simulation
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    icr(:)       |none          |sequence number of crop grown within the
!!                                |current year
!!    ihru         |none          |HRU number
!!    ncrops(:,:,:)|
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    nro(:)       |none          |sequence number for year in rotation
!!    nyskip       |none          |number of years to skip output printing/
!!                                |summarization
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    idorm(:)     |none          |dormancy status code:
!!                                |0 land cover growing (not dormant)
!!                                |1 land cover dormant
!!    igro(:)      |none          |land cover status code:
!!                                |0 no land cover currently growing
!!                                |1 land cover growing
!!    laiday(:)    |m**2/m**2     |leaf area index
!!    ncrops(:,:,:)|
!!    phuacc(:)    |none          |fraction of plant heat units accumulated
!!    plantn(:)    |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)    |kg P/ha       |amount of phosphorus in plant biomass
!!    plt_pst(:,:) |kg/ha         |pesticide on plant foliage
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    strsw(:)     |none          |fraction of potential plant growth achieved
!!                                |on the day where the reduction is caused by
!!                                |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    resnew      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
  
      integer :: j, k
      real :: resnew
      
      !!by zhang
      !!====================
      real :: BLG1, BLG2, BLG3,  CLG, sf
      real :: sol_min_n, resnew_n, resnew_ne
      real :: LMF, LSF, LSLF, LSNF,LMNF 
      orgc_f = 0.
      BLG1 = 0.
      BLG2 = 0.
      BLG3 = 0.
      CLG = 0.
      sf = 0.
      sol_min_n = 0.
      resnew = 0.
      resnew_n = 0.
      resnew_ne = 0.
      LMF = 0.
      LSF = 0.
      LSLF = 0.
      LSNF = 0.
      LMNF = 0.
      !!by zhang
      !!====================      
      
      
      j = 0
      j = ihru

!      if (curyr > nyskip) then
!       ncrops(icr(j),j) = ncrops(icr(j),j) + 1
!      endif

	!! 22 January 2008	
	resnew = 0.
	rtresnew = 0.
      resnew = bio_ms(j) * (1. - rwt(j))
	rtresnew = bio_ms(j) * rwt(j)
	call rootfr

	!! update residue, N, P on soil surface
      sol_rsd(1,j) = resnew + sol_rsd(1,j)
      sol_fon(1,j) = plantn(j) * (1. - rwt(j)) + sol_fon(1,j)
      sol_fop(1,j) = plantp(j) * (1. - rwt(j)) + sol_fop(1,j)
      sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
	sol_fon(1,j) = Max(sol_fon(1,j),0.)
	sol_fop(1,j) = Max(sol_fop(1,j),0.)

            !!insert new biomss by zhang
            !!=================================
            if (cswat == 2) then
	          !!all the lignin from STD is assigned to LSL, 
	            !!add STDL calculation
	          !!
	          !sol_LSL(k,ihru) = sol_STDL(k,ihru)
	          !CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&HUI(JJK))
	          ! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
                ! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
                !CROPCOM.dat BLG1 = 0.01 BLG2 = 0.10
                !SUBROUTINE ASCRV(X1,X2,X3,X4)
                !EPIC0810
                !THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
                !USE PARM
                !XX=LOG(X3/X1-X3)
                !X2=(XX-LOG(X4/X2-X4))/(X4-X3)
                !X1=XX+X3*X2
                !RETURN
                !END 
                !HUI(JJK)=HU(JJK)/XPHU               
                
                BLG1 = 0.01/0.10
                BLG2 = 0.99
                BLG3 = 0.10
                XX = log(0.5/BLG1-0.5)
                BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
                BLG1 = XX + 0.5*BLG2
                CLG=BLG3*phuacc(j)/(phuacc(j)+EXP(BLG1-BLG2*phuacc(j)))
    

	          !if (k == 1) then
		        sf = 0.05
	          !else
		        !sf = 0.1
	          !end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (sol_no3(1,j)+sol_nh3(1,j))
	          
	          resnew = resnew
	          resnew_n = ff1 * (plantn(j) - yieldn)    	    
        	    resnew_ne = resnew_n + sf * sol_min_n
        	    
       	        !Not sure 1000 should be here or not!
        	    !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
        	    RLN = (resnew * CLG/(resnew_n+1.E-5))
        	    RLR = MIN(.8, resnew * CLG/(resnew+1.E-5))
        	    
        	    LMF = 0.85 - 0.018 * RLN
        	    if (LMF <0.01) then
        	        LMF = 0.01
        	    else
        	        if (LMF >0.7) then
        	            LMF = 0.7
        	        end if
        	    end if      	  
	          !if ((resnew * CLG/(resnew_n+1.E-5)) < 47.22) then
		        !    LMF = 0.85 - 0.018 * (resnew * CLG/(resnew_n+1.E-5))
	          !else
		        !    LMF = 0.
	          !end if 	

	          LSF =  1 - LMF  
        	  
	          sol_LM(1,j) = sol_LM(1,j) + LMF * resnew
	          sol_LS(1,j) = sol_LS(1,j) + LSF * resnew
        	  

                
	          !here a simplified assumption of 0.5 LSL
	          LSLF = 0.0
	          LSLF = CLG          
	          
	          sol_LSL(1,j) = sol_LSL(1,j) + RLR* LSF * resnew	          
	          sol_LSC(1,j) = sol_LSC(1,j) + 0.42*LSF * resnew  
	          
	          sol_LSLC(1,j) = sol_LSLC(1,j) + RLR*0.42*LSF * resnew
	          sol_LSLNC(1,j) = sol_LSC(1,j) - sol_LSLC(1,j)              
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_n >= (0.42 * LSF * resnew /150)) then
		         sol_LSN(1,j) = sol_LSN(1,j) + 0.42 * LSF * resnew / 150
		         sol_LMN(1,j) = sol_LMN(1,j) + resnew_n - 
     &                         (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         sol_LSN(1,j) = sol_LSN(1,j) + resnew_n
		         sol_LMN(1,j) = sol_LMN(1,j) + 1.E-25
	          end if	
        	
	          !LSNF = sol_LSN(1,j)/(sol_LS(1,j)+1.E-5)	
        	  
	          sol_LMC(1,j) = sol_LMC(1,j) + 0.42 * LMF * resnew	
	          !LMNF = sol_LMN(1,j)/(sol_LM(1,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
                sol_no3(1,j) = sol_no3(1,j) * (1-sf)
                sol_nh3(1,j) = sol_nh3(1,j) * (1-sf)
            end if
            !!insert new biomss by zhang
            !!===============================

	!! allocate dead roots, N, P to soil layers
	do l=1, sol_nly(j)
	 sol_rsd(l,j) = sol_rsd(l,j) + rtfr(l) * rtresnew
	 sol_fon(l,j) = sol_fon(l,j) + rtfr(l) * plantn(j) * rwt(j)
	 sol_fop(l,j) = sol_fop(l,j) + rtfr(l) * plantp(j) * rwt(j)

              !!insert new biomss by zhang
              !!==============================
              if (cswat == 2) then
	          !!all the lignin from STD is assigned to LSL, 
	            !!add STDL calculation
	          !!
	          !sol_LSL(k,ihru) = sol_STDL(k,ihru)
	          !CLG=BLG(3,JJK)*HUI(JJK)/(HUI(JJK)+EXP(BLG(1,JJK)-BLG(2,JJK)*&HUI(JJK))
	          ! 52  BLG1 = LIGNIN FRACTION IN PLANT AT .5 MATURITY
                ! 53  BLG2 = LIGNIN FRACTION IN PLANT AT MATURITY
                !CROPCOM.dat BLG1 = 0.01 BLG2 = 0.10
                !SUBROUTINE ASCRV(X1,X2,X3,X4)
                !EPIC0810
                !THIS SUBPROGRAM COMPUTES S CURVE PARMS GIVEN 2 (X,Y) POINTS.
                !USE PARM
                !XX=LOG(X3/X1-X3)
                !X2=(XX-LOG(X4/X2-X4))/(X4-X3)
                !X1=XX+X3*X2
                !RETURN
                !END 
                !HUI(JJK)=HU(JJK)/XPHU               
                
                BLG1 = 0.01/0.10
                BLG2 = 0.99
                BLG3 = 0.10
                XX = log(0.5/BLG1-0.5)
                BLG2 = (XX -log(1./BLG2-1.))/(1.-0.5)
                BLG1 = XX + 0.5*BLG2
                CLG=BLG3*phuacc(j)/(phuacc(j)+EXP(BLG1-BLG2*phuacc(j)))
         

	          if (l == 1) then
		        sf = 0.05
	          else
		        sf = 0.1
	          end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (sol_no3(l,j)+sol_nh3(l,j))
	          	          
	          resnew = rtfr(l) * rtresnew 
	          resnew_n = rtfr(l) *ff2 * (plantn(j) - yieldn)  	    
        	    resnew_ne = resnew_n + sf * sol_min_n
        	        !Not sure 1000 should be here or not!
        	    !RLN = 1000*(resnew * CLG/(resnew_n+1.E-5))
        	    RLN = (resnew * CLG/(resnew_n+1.E-5))
        	    RLR = MIN(.8, resnew * CLG/1000/(resnew/1000+1.E-5))
        	    
        	    LMF = 0.85 - 0.018 * RLN
        	    if (LMF <0.01) then
        	        LMF = 0.01
        	    else
        	        if (LMF >0.7) then
        	            LMF = 0.7
        	        end if
        	    end if      	  
	          !if ((resnew * CLG/(resnew_n+1.E-5)) < 47.22) then
		        !    LMF = 0.85 - 0.018 * (resnew * CLG/(resnew_n+1.E-5))
	          !else
		        !    LMF = 0.
	          !end if 	

	          LSF =  1 - LMF  
        	  
	          sol_LM(l,j) = sol_LM(l,j) + LMF * resnew
	          sol_LS(l,j) = sol_LS(l,j) + LSF * resnew
        	  

                
	          !here a simplified assumption of 0.5 LSL
	          !LSLF = 0.0
	          !LSLF = CLG          
	          
	          sol_LSL(l,j) = sol_LSL(l,j) + RLR*resnew	          
	          sol_LSC(l,j) = sol_LSC(l,j) + 0.42*LSF * resnew  
	          
	          sol_LSLC(l,j) = sol_LSLC(l,j) + RLR*0.42*resnew
	          sol_LSLNC(l,j) = sol_LSC(l,j) - sol_LSLC(l,j)              
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         sol_LSN(l,j) = sol_LSN(l,j) + 0.42 * LSF * resnew / 150
		         sol_LMN(l,j) = sol_LMN(l,j) + resnew_ne - 
     &                         (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         sol_LSN(l,j) = sol_LSN(l,j) + resnew_ne
		         sol_LMN(l,j) = sol_LMN(l,j) + 1.E-25
	          end if	
        	
	          !LSNF = sol_LSN(l,j)/(sol_LS(l,j)+1.E-5)	
        	  
	          sol_LMC(l,j) = sol_LMC(l,j) + 0.42 * LMF * resnew	
	          !LMNF = sol_LMN(l,j)/(sol_LM(l,j) + 1.E-5)           
                
                !update no3 and nh3 in soil
                sol_no3(l,j) = sol_no3(l,j) * (1-sf)
                sol_nh3(l,j) = sol_nh3(l,j) * (1-sf)
            end if
            !!insert new biomss by zhang    
            !!=============================== 


	end do

      if (hrupest(j) == 1) then
        do k = 1, npmx
          sol_pst(k,j,1) = sol_pst(k,j,1) + plt_pst(k,j)
          plt_pst(k,j) = 0.
        end do
      end if

      bio_hv(icr(j),j) = bio_ms(j) + bio_hv(icr(j),j)
      !bio_yrms(j) = bio_yrms(j) + bio_ms(j) / 1000.

	!! reset variables
      igro(j) = 0
      idorm(j) = 0
      bio_ms(j) = 0.
      rwt(j) = 0.
	plantn(j) = 0.
      plantp(j) = 0.
      strsw(j) = 1.
      laiday(j) = 0.
      hvstiadj(j) = 0.
      phuacc(j) = 0.
!      phubase(j) = 0.
	rtfr = 0. ! Resetting roots fraction per layer array
	 
      return
      end