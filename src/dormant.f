      subroutine dormant

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the dormant status of the different plant types

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alai_min(:)    |m**2/m**2     |minimum LAI during winter dormant period
!!    bio_leaf(:)    |none          |fraction of biomass that drops during
!!                                  |dormancy (for trees only)
!!    bio_ms(:)      |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_yrms(:)    |metric tons/ha|annual biomass (dry weight) in the HRU
!!    dayl(:)        |hours         |day length for current day
!!    daylmn(:)      |hours         |shortest daylength occurring during the
!!                                  |year
!!    dormhr(:)      |hour          |time threshold used to define dormant
!!                                  |period for plant (when daylength is within
!!                                  |the time specified by dormhr from the minimum
!!                                  |daylength for the area, the plant will go
!!                                  |dormant)
!!    icr(:)         |none          |sequence number of crop grown within the
!!                                  |current year
!!    idc(:)         |none          |crop/landcover category:
!!                                  |1 warm season annual legume
!!                                  |2 cold season annual legume
!!                                  |3 perennial legume
!!                                  |4 warm season annual
!!                                  |5 cold season annual
!!                                  |6 perennial
!!                                  |7 trees
!!    idorm(:)       |none          |dormancy status code:
!!                                  |0 land cover growing
!!                                  |1 land cover dormant
!!    idplt(:)       |none          |land cover code from crop.dat
!!    ihru           |none          |HRU number
!!    nro(:)         |none          |sequence number for year in rotation
!!    phuacc(:)      |none          |fraction of plant heat units accumulated
!!    plantn(:)      |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)      |kg P/ha       |amount of phosphorus in plant biomass
!!    pltfr_n(:)     |none          |fraction of plant biomass that is nitrogen
!!    pltfr_p(:)     |none          |fraction of plant biomass that is phosphorus
!!    sol_fon(:,:)   |kg N/ha       |amount of nitrogen stored in the fresh
!!                                  |organic (residue) pool
!!    sol_fop(:,:)   |kg P/ha       |amount of phosphorus stored in the fresh
!!                                  |organic (residue) pool
!!    sol_rsd(:,:)   |kg/ha         |amount of organic matter in the soil
!!                                  |classified as residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    laiday(:)   |m**2/m**2     |leaf area index
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha|annual biomass (dry weight) in the HRU
!!    idorm(:)    |none          |dormancy status code:
!!                               |0 land cover growing
!!                               |1 land cover dormant
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha       |amount of phosphorus in plant biomass
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool
!!    sol_rsd(:,:)|kg/ha         |amount of organic matter in the soil
!!                               |classified as residue
!!    strsw(:)    |none          |fraction of potential plant growth achieved
!!                               |on the day where the reduction is caused by
!!                               |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    resnew      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real*8 :: resnew
      integer :: j

      !!by zhang
      !!====================

      real*8 :: BLG1, BLG2, BLG3,  CLG, sf
      real*8 :: sol_min_n,  resnew_n, resnew_ne
      real*8 :: LMF, LSF, LSLF, LSNF,LMNF 
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


!! check for beginning of dormant season
      if (idc(idplt(j)) == 1 .or. idc(idplt(j)) == 4) return
      if (idorm(j) == 0 .and. dayl(j)-dormhr(j) < daylmn(hru_sub(j)))   
     &                                                              then

        select case (idc(idplt(j)))
        
          !! make sure all operations are scheduled during growing season of warm season annual
          case (1,4)
            dorm_flag = 1
            call operatn
            dorm_flag = 0

          !! beginning of forest dormant period
          case (7)
            idorm(j) = 1
            resnew = 0.
            resnew = bio_ms(j) * bio_leaf(idplt(j))            
            
            !!add by zhang
            !!===================
            if (cswat == 2) then
                rsdc_d(j) = rsdc_d(j) + resnew*0.42
            end if
            !!add by zhang
            !!===================

            !!insert new biomss by zhang
            !!=============================
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
                CLG=BLG3*phuacc(j)/(phuacc(j)+
     &              EXP(BLG1-BLG2*phuacc(j)))

	          !if (k == 1) then
		        sf = 0.05
	          !else
		        !sf = 0.1
	          !end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (sol_no3(1,j)+sol_nh3(1,j))
	          	          
	          resnew = bio_ms(j) * bio_leaf(idplt(j)) 
	          resnew_n = resnew * pltfr_n(j)   	    
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
                
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         sol_LSN(1,j) = sol_LSN(1,j) + 0.42 * LSF * resnew / 150
		         sol_LMN(1,j) = sol_LMN(1,j) + resnew_ne - 
     &                         (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         sol_LSN(1,j) = sol_LSN(1,j) + resnew_ne
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
            !!===========================

            
            sol_rsd(1,j) = sol_rsd(1,j) + resnew
            sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
            sol_fon(1,j) = resnew * pltfr_n(j) + sol_fon(1,j)
            sol_fop(1,j) = resnew * pltfr_p(j) + sol_fop(1,j)
            bio_hv(icr(j),j) = bio_ms(j) + bio_hv(icr(j),j)
            bio_yrms(j) = bio_yrms(j) + bio_ms(j) / 1000.
            bio_ms(j) = bio_ms(j) * (1. - bio_leaf(idplt(j)))
            plantn(j) = plantn(j) - resnew * pltfr_n(j)
            plantp(j) = plantp(j) - resnew * pltfr_p(j)
            strsw(j) = 1.
            laiday(j) = alai_min(idplt(j))
            phuacc(j) = 0.
            laimxfr(j) = 0.        !Sue White - dormancy
            ncrops(icr(j),j) = ncrops(icr(j),j) + 1

          !! beginning of perennial (pasture/alfalfa) dormant period
          case (3, 6)
            idorm(j) = 1
            resnew = 0.
            resnew = bm_dieoff(idplt(j)) * bio_ms(j)

            !!add by zhang
            !!===================
            if (cswat == 2) then
                rsdc_d(j) = rsdc_d(j) + resnew*0.42
            end if
            !!add by zhang
            !!===================

            !!insert new biomss by zhang
            !!=============================
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
                CLG=BLG3*phuacc(j)/(phuacc(j)+
     &              EXP(BLG1-BLG2*phuacc(j)))

	          !if (k == 1) then
		        sf = 0.05
	          !else
		        !sf = 0.1
	          !end if	

               !kg/ha  
	          sol_min_n = 0.	
	          sol_min_n = (sol_no3(1,j)+sol_nh3(1,j))
	          	          
	          resnew = bm_dieoff(idplt(j)) * bio_ms(j) 
	          resnew_n = bm_dieoff(idplt(j)) * plantn(j)   	    
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
        	  
	          sol_LM(1,j) = sol_LM(1,j) + LMF * resnew
	          sol_LS(1,j) = sol_LS(1,j) + LSF * resnew
        	  

                
	          !here a simplified assumption of 0.5 LSL
	          !LSLF = 0.0
	          !LSLF = CLG          
	          
	          sol_LSL(1,j) = sol_LSL(1,j) + RLR*resnew	          
	          sol_LSC(1,j) = sol_LSC(1,j) + 0.42*LSF * resnew  
	          
	          sol_LSLC(1,j) = sol_LSLC(1,j) + RLR*0.42*resnew
	          sol_LSLNC(1,j) = sol_LSC(1,j) - sol_LSLC(1,j)              
                
                !X3 = MIN(X6,0.42*LSF * resnew/150) 
                
	          if (resnew_ne >= (0.42 * LSF * resnew /150)) then
		         sol_LSN(1,j) = sol_LSN(1,j) + 0.42 * LSF * resnew / 150
		         sol_LMN(1,j) = sol_LMN(1,j) + resnew_ne - 
     &                         (0.42 * LSF * resnew / 150) + 1.E-25
	          else
		         sol_LSN(1,j) = sol_LSN(1,j) + resnew_ne
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
            !!===========================


            sol_rsd(1,j) = sol_rsd(1,j) + resnew
            sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
            sol_fon(1,j) = sol_fon(1,j) +                               
     &         bm_dieoff(idplt(j)) * plantn(j)
            sol_fop(1,j) = sol_fop(1,j) +                               
     &         bm_dieoff(idplt(j)) * plantp(j)
            bio_hv(icr(j),j) = bio_ms(j) *                              
     &        bm_dieoff(idplt(j)) +                                     
     &	    bio_hv(icr(j),j)
            bio_yrms(j) = bio_yrms(j) + bio_ms(j) *                     
     &         bm_dieoff(idplt(j)) / 1000.
            bio_ms(j) = (1. - bm_dieoff(idplt(j))) *                    
     &         bio_ms(j)
            plantn(j) = (1. - bm_dieoff(idplt(j))) *                    
     &         plantn(j)
            plantp(j) = (1. - bm_dieoff(idplt(j))) *                    
     &         plantp(j)
            strsw(j) = 1.
             laiday(j) = alai_min(idplt(j))
             phuacc(j) = 0.
!            ncrops(icr(j),j) = ncrops(icr(j),j) + 1

          !! beginning of cool season annual dormant period
          case (2, 5)
            if (phuacc(j) < 0.75) then
              idorm(j) = 1
              strsw(j) = 1.
            end if 
          end select
           if (imgt == 1) then
            write (143,1000) subnum(j), hruno(j), iyr, i_mo, iida,
     *       hru_km(j),
     *       cpnm(idplt(j)),"START-DORM", phubase(j), phuacc(j), 
     *       sol_sw(j),bio_ms(j), sol_rsd(1,j), sol_sumno3(j),
     *       sol_sumsolp(j)
           end if
           
          end if

!! check if end of dormant period
        if (idorm(j) == 1 .and. dayl(j)-dormhr(j) >= daylmn(hru_sub(j)))
     &                                                              then

          select case (idc(idplt(j)))
          
            !! end of perennial dormant period
            case (3, 6, 7)
              idorm(j) = 0

            !! end of cool season annual dormant period
            case (2, 5)
              idorm(j) = 0
              phuacc(j) = 0.

            end select
            
            if (imgt == 1) then
                 write (143,1000) subnum(j), hruno(j), iyr, i_mo, iida,
     *       hru_km(j),
     *       cpnm(idplt(j)), "END-DORM", phubase(j), phuacc(j), 
     *       sol_sw(j), bio_ms(j), sol_rsd(1,j), sol_sumno3(j),
     *       sol_sumsolp(j)
            end if

        end if

1000  format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,7f10.2)
      return
      end