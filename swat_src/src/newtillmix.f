      subroutine newtillmix(jj,bmix)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine mixes residue and nutrients during tillage and 
!!    biological mixing
!!    New version developed by Armen R. Kemanian in collaboration with Stefan Julich and Cole Rossi
!!    Mixing was extended to all layers
!!    A subroutine to simulate stimulation of organic matter decomposition was added
!!    March 2009: testing has been minimal and further adjustments are expected
!!    use with caution 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil 
!!                                 |particles
!!    cnop          |none          |SCS runoff curve number for moisture
!!                                 |condition II
!!    curyr         |none          |current year of simulation
!!    deptil(:)     |mm            |depth of mixing caused by tillage
!!                                 |operation
!!    effmix(:)     |none          |mixing efficiency of tillage operation
!!    sol_nly(jj)          |none          |maximum number of soil layers
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    nro(:)        |none          |sequence number of year in rotation
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    nyskip        |none          |number of years to skip output printing/
!!                                 |summarization
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_nly(:)    |none          |number of soil layers
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool.
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil
!!                                 |classified as residue
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus pool
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    sumix(:)      |none          |sum of mixing efficiencies in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil 
!!                                 |particles
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool.
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil
!!                                 |classified as residue
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus pool
!!    sumix(:)      |none          |sum of mixing efficiencies in HRU
!!    min_res(:)	|kg/ha		   |Min residue allowed due to implementation of 
!!                                 |residue managment in the OPS file.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bmix        |none          |biological mixing efficiency: this 
!!                               |number is zero for tillage operations
!!    dg          |mm            |depth of soil layer
!!    dtil        |mm            |depth of mixing
!!    emix        |none          |mixing efficiency
!!    jj          |none          |HRU number
!!    k           |none          |counter
!!    l           |none          |counter
!!    nl          |none          |number of layers being mixed
!!    smix(:)     |varies        |amount of substance in soil profile
!!                               |that is being redistributed between 
!!                               |mixed layers
!!    thtill(:)   |none          |fraction of soil layer that is mixed
!!    sol_msm					 | sol_mass mixed
!!    sol_msn					 | sol_mass not mixed
!!    maxmix      |none          | maximum mixing eff to preserve specified minimum residue cover
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max
!!    SWAT: curno 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: jj
      real, intent (in) :: bmix
!$$$$$$       integer :: l, k, nl, a
      integer :: l, k              !CB 12/2/09 nl and a are not used.
      real :: emix, dtil, XX, WW1, WW2, WW3, WW4, maxmix
!$$$$$$       real :: thtill(sol_nly(jj)), smix(20+npmx)  
      !!by zhang
      !!=============   
      real :: smix(22+npmx+12)        !CB 12/2/09 thtill is not used. mjw rev 490
      !!changed the dimension from 22 + npmx to 22 + npmx + 12
      !!by zhang
      !!=============
      real :: sol_mass(sol_nly(jj))
      real :: sol_thick(sol_nly(jj)), sol_msm(sol_nly(jj))
      real :: sol_msn(sol_nly(jj))


      XX = 0.
      WW1 = 0.
      WW2 = 0.
      WW3 = 0.
      WW4 = 0.
      emix = 0.
      dtil = 0.
      if (bmix > 1.e-6) then
        !! biological mixing
        emix = bmix !bmix MJW (rev 412)
        dtil = Min(sol_z(sol_nly(jj),jj), 50.) ! it was 300.  MJW (rev 412)
      else 
        !! tillage operation
        emix = effmix(idtill)
        dtil = deptil(idtill)
      end if
      ! -------------------------------- Original D. Moriasi code replaced by code below        
!	Drainmod  07/2006
!      if(itill(jj) == 1) then
!	  cumei(jj) = 0.
!	  cumeira(jj) = 0.
!	  cumrt(jj) = 0.
!        cumrai(jj) = 0.
!	  ranrns_hru(jj) = ranrns(idtill)   
!      end if
!!    Drainmod 7/2006
! --------------------------------------------------------------------
        if (idtill.GE. 1) then ! Updated dynamic depressional storage D.Moriasi 4/8/2014
          cumei(jj)   = 0.
	    cumeira(jj) = 0.
	    cumrt(jj)   = 0.
          cumrai(jj)  = 0.
	    ranrns_hru(jj) = ranrns(idtill)
        end if
! --------------------------------------------------------------------

      !!by zhang DSSAT tillage
      !!=======================
      !!    deptil(:)   |mm  |depth of mixing caused by tillage operation
      !jj is hru number
      if (cswat == 2) then
          tillage_days(jj) = 0
          tillage_depth(jj) = dtil
          tillage_switch(jj) = 1
      end if
      !!by zhang DSSAT tillage
      !!=======================


      smix = 0.
      sol_mass = 0.
      sol_thick = 0.
      sol_msm = 0.
      sol_msn = 0.

	!! incorporate bacteria - no mixing - lost from transport
      if (dtil > 10.) then     
        bactpq(jj) = bactpq(jj) * (1. - emix)
        bactps(jj) = bactps(jj) * (1. - emix)
        bactlpq(jj) = bactlpq(jj) * (1. - emix)
        bactlps(jj) = bactlps(jj) * (1. - emix)
	end if
      	
	!! calculate max mixing to preserve target surface residue MJW rev 490
	!! Assume residue in all other layers is negligible to simplify calculation and remove depth dependency
      if (min_res(jj) > 1. .and. bmix < 0.001) then
	  maxmix = 1 - min_res(jj)/sol_rsd(1,jj)
	  if (maxmix <0.05)  maxmix = 0.05	
	  if (emix > maxmix)  emix = maxmix
      end if


      do l=1, sol_nly(jj)
        if ( l == 1) then
          sol_thick(l) = sol_z(l,jj)
        else	
          sol_thick(l) = sol_z(l,jj) - sol_z(l-1,jj)
        end if
	
      sol_mass(l) = (sol_thick(l) / 1000.) * 10000. * 
     &       sol_bd(l,jj) * 1000. * (1.- sol_rock(l,jj) / 100.)

      end do

!       do l=1,20+npmx
!         smix(l)=0.
!       end do
      smix = 0.

      if (dtil > 0.) then
!!!  added by Armen 09/10/2010 next line only
        if (dtil < 10.0) dtil = 11.0
	 do l=1, sol_nly(jj)

          if (sol_z(l,jj) <= dtil) then
            !! msm = mass of soil mixed for the layer
            !! msn = mass of soil not mixed for the layer		
            sol_msm(l) = emix * sol_mass(l)	
            sol_msn(l) = sol_mass(l) - sol_msm(l)	
          else if (sol_z(l,jj) > dtil.AND.sol_z(l-1,jj) < dtil) then 
            sol_msm(l) = emix * sol_mass(l) *                           
     &      (dtil - sol_z(l-1,jj)) / sol_thick(l)
            sol_msn(l) =  sol_mass(l) -  sol_msm(l)
          else
            sol_msm(l) = 0.
            sol_msn(l) = sol_mass(l)
          end if
			
          !! calculate the mass or concentration of each mixed element 
          !! mass based mixing
          WW1 = sol_msm(l)/(sol_msm(l) + sol_msn(l))
          smix(1) = smix(1) + sol_no3(l,jj) * WW1
          smix(2) = smix(2) + sol_orgn(l,jj) * WW1
          smix(3) = smix(3) + sol_nh3(l,jj) * WW1
          smix(4) = smix(4) + sol_solp(l,jj) * WW1
          smix(5) = smix(5) + sol_orgp(l,jj) * WW1
          smix(6) = smix(6) + sol_aorgn(l,jj) * WW1
          smix(7) = smix(7) + sol_actp(l,jj) * WW1
          smix(8) = smix(8) + sol_fon(l,jj) * WW1
          smix(9) = smix(9) + sol_fop(l,jj) * WW1
          smix(10) = smix(10) + sol_stap(l,jj) * WW1
          smix(11) = smix(11) + sol_rsd(l,jj) * WW1
          smix(12) = smix(12) + sol_mc(l,jj) * WW1
          smix(13) = smix(13) + sol_mn(l,jj) * WW1
          smix(14) = smix(14) + sol_mp(l,jj) * WW1

		!! concentration based mixing
          WW2 = XX + sol_msm(l)
          smix(15) = (XX * smix(15) + sol_cbn(l,jj) * sol_msm(l)) /WW2
          smix(16) = (XX * smix(16) + sol_n(l,jj) * sol_msm(l)) /WW2
          smix(17) = (XX * smix(17) + sol_clay(l,jj) * sol_msm(l)) /WW2
          smix(18) = (XX * smix(18) + sol_silt(l,jj) * sol_msm(l)) /WW2
          smix(19) = (XX * smix(19) + sol_sand(l,jj) * sol_msm(l)) /WW2
!          smix(20) = (XX * smix(20) + sol_rock(l,jj) * sol_msm(l)) / WW2
!          smix(21) = (XX * smix(21) + sol_ph(l,jj) * sol_msm(l)) /WW2 !! mjw rev490
!          smix(22) = (XX * smix(22) + sol_cal(l,jj) * sol_msm(l)) /WW2 !! mjw rev490
		!! mass based distribution
          do k = 1, npmx
          	smix(20+k) = smix(20+k) + sol_pst(k,jj,l) * WW1
          end do

            !!by zhang
            !!============== 
            if (cswat == 2) then         
	        smix(20+npmx+1) = smix(20+npmx+1) +sol_LSC(l,jj)* WW1
	        smix(20+npmx+2) = smix(20+npmx+2) +sol_LSLC(l,jj)* WW1
	        smix(20+npmx+3) = smix(20+npmx+3) +sol_LSLNC(l,jj)* WW1
	        smix(20+npmx+4) = smix(20+npmx+4) +sol_LMC(l,jj)* WW1
	        smix(20+npmx+5) = smix(20+npmx+5) +sol_LM(l,jj)* WW1
	        smix(20+npmx+6) = smix(20+npmx+6) +sol_LSL(l,jj)* WW1
	        smix(20+npmx+7) = smix(20+npmx+7) +sol_LS(l,jj)* WW1	        
	        
	        smix(20+npmx+8) = smix(20+npmx+8) +sol_LSN(l,jj)* WW1
	        smix(20+npmx+9) = smix(20+npmx+9) +sol_LMN(l,jj)* WW1
	        smix(20+npmx+10) = smix(20+npmx+10) +sol_BMN(l,jj)* WW1
	        smix(20+npmx+11) = smix(20+npmx+11) +sol_HSN(l,jj)* WW1
	        smix(20+npmx+12) = smix(20+npmx+12) +sol_HPN(l,jj)* WW1  
	      end if
            !!by zhang 	
            !!=============
			
          XX = XX + sol_msm(l)
        end do

          do l=1, sol_nly(jj)
			
            ! reconstitute each soil layer 
            WW3 = sol_msn(l) / sol_mass(l)
            WW4 = sol_msm(l) / XX

            sol_no3(l,jj) = sol_no3(l,jj) * WW3 + smix(1) * WW4
            sol_orgn(l,jj) = sol_orgn(l,jj) * WW3 + smix(2) * WW4
            sol_nh3(l,jj) = sol_nh3(l,jj) * WW3 + smix(3) * WW4
            sol_solp(l,jj) = sol_solp(l,jj) * WW3 + smix(4) * WW4
            sol_orgp(l,jj) = sol_orgp(l,jj) * WW3 + smix(5) * WW4
            sol_aorgn(l,jj) = sol_aorgn(l,jj) * WW3 + smix(6) * WW4
            sol_actp(l,jj) = sol_actp(l,jj) * WW3 + smix(7) * WW4
            sol_fon(l,jj) = sol_fon(l,jj) * WW3 + smix(8) * WW4
            sol_fop(l,jj) = sol_fop(l,jj) * WW3 + smix(9) * WW4
            sol_stap(l,jj) = sol_stap(l,jj) * WW3 + smix(10) * WW4
            sol_rsd(l,jj) = sol_rsd(l,jj) * WW3 + smix(11) * WW4
            if (sol_rsd(l,jj) < 1.e-10) sol_rsd(l,jj) = 1.e-10
            sol_mc(l,jj) = sol_mc(l,jj) * WW3 + smix(12) * WW4
            sol_mn(l,jj) = sol_mn(l,jj) * WW3 + smix(13) * WW4
            sol_mp(l,jj) = sol_mp(l,jj) * WW3 + smix(14) * WW4

            sol_cbn(l,jj) = (sol_cbn(l,jj) * sol_msn(l) + smix(15)      
     &           * sol_msm(l)) / sol_mass(l)
            sol_n(l,jj) = (sol_n(l,jj) * sol_msn(l) + smix(16)          
     &           * sol_msm(l)) / sol_mass(l)
            sol_clay(l,jj) = (sol_clay(l,jj) * sol_msn(l) + smix(17)    
     &           * sol_msm(l)) / sol_mass(l)
            sol_silt(l,jj) = (sol_silt(l,jj) * sol_msn(l) + smix(18)    
     &           * sol_msm(l)) / sol_mass(l)
            sol_sand(l,jj) = (sol_sand(l,jj) * sol_msn(l) + smix(19)    
     &           * sol_msm(l)) / sol_mass(l)
!		sol_rock(l,jj) = (sol_rock(l,jj) * sol_msn(l) + smix(20) * sol_msm(l)) / sol_mass(l)
!            sol_ph(l,jj) = (sol_ph(l,jj) * sol_msn(l) + smix(21)        &
!     &           * sol_msm(l)) / sol_mass(l) !! mjw rev 490 simplified, PH not linear
!            sol_cal(l,jj) = (sol_cal(l,jj) * sol_msn(l) + smix(22)      &
!     &           * sol_msm(l)) / sol_mass(l) !! mjw rev 490 


			
            do k = 1, npmx
              sol_pst(k,jj,l) = sol_pst(k,jj,l) * WW3 + smix(20+k) * WW4
            end do

             !!by zhang
             !!=============
             if (cswat == 2) then
             sol_LSC(l,jj) = sol_LSC(l,jj)*WW3+smix(20+npmx+1)* WW4
             sol_LSLC(l,jj) = sol_LSLC(l,jj)*WW3+smix(20+npmx+2)* WW4
             sol_LSLNC(l,jj) = sol_LSLNC(l,jj)*WW3+smix(20+npmx+3)* WW4
             sol_LMC(l,jj) = sol_LMC(l,jj)*WW3 + smix(20+npmx+4)* WW4
             sol_LM(l,jj) = sol_LM(l,jj)*WW3 + smix(20+npmx+5)* WW4
             sol_LSL(l,jj) = sol_LSL(l,jj)*WW3 + smix(20+npmx+6)* WW4
             sol_LS(l,jj) = sol_LS(l,jj)*WW3 + smix(20+npmx+7)* WW4
             sol_LSN(l,jj) = sol_LSN(l,jj)*WW3 + smix(20+npmx+8)* WW4
             sol_LMN(l,jj) = sol_LMN(l,jj)*WW3 + smix(20+npmx+9)* WW4
             sol_BMN(l,jj) = sol_BMN(l,jj)*WW3 + smix(20+npmx+10)* WW4
             sol_HSN(l,jj) = sol_HSN(l,jj)*WW3 + smix(20+npmx+11)* WW4
             sol_HPN(l,jj) = sol_HPN(l,jj)*WW3 + smix(20+npmx+12)* WW4
             end if
            !!by zhang 
            !!==============

	  end do
	
        if (cswat == 1) then
            call tillfactor(jj,bmix,emix,dtil,sol_thick)
        end if

		!! summary calculations
        if (curyr > nyskip) then
            sumix(jj) = sumix(jj) + emix
        end if

      end if
	
      !! perform final calculations for tillage operation
 
      !! count the tillage only if it is a scheduled operation biomix does not count MJW Rev 490
      if (bmix <= 1.e-6) then
        ntil(jj) = ntil(jj) + 1
      end if
      if (cnop > 1.e-4) call curno(cnop,jj)
      
      !ntil(jj) = ntil(jj) + 1 ' orig code

      return
      end