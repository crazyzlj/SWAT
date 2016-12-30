	subroutine depstor

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes maximum surface depressional storage depth based on   
!!    random and oriented roughness and slope steepness

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!	hru_slp(:)	|m/m	       |average slope steepness in HRU
!!	iida		|julian date   |day being simulated (current julian day)
!!    iop(:,:,:)  |julian date   |date of tillage operation
!!    mgt_op      |none          |operation code number
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    ranrns_hru(:)|mm           |random roughness for a given HRU
!!    sol_bd(1,:) |Mg/m^3        |bulk density of top soil layer in HRU
!!    sol_cbn(1,:)|%             |percent organic carbon in top soil layer in HRU
!!	sol_clay(1,:)|% 		   |percent clay content in top soil layer in HRU
!!    sol_rsd(1,:)|kg/ha         |amount of organic matter in the top soil layer
!!                               |classified as residue in HRU
!!	sol_ori(:)	|mm			   |oriented roughness (ridges) at time of a given tillage operation
!!    sol_z(1,:)  |mm            |depth to bottom of top soil layer
!!    usle_ei     |100(ft-tn in)/(acre-hr)|USLE rainfall erosion index
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!	stmaxd(:)	|mm			   |maximum surface depressional storage for day in a given HRU 							   
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!	cumei(:)	|Mj*mm/ha*hr   |cumulative USLE rainfall erosion index since last 
!!							   |tillage operation
!!	cumrt(:)	|mm H2O		   |cumulative rainfall since last tillage operation
!!	df  		|none		   |oriented and random roughness decay factor - based
!!                               |on cumulative EI and cumulative precipday
!!    ei          |Mj*mm/ha*hr   |USLE rainfall erosion index
!!	hru_slpp    |%	           |average percent slope steepness
!!	sol_orgm    |%      	   |percent organic matter content in soil material
!!	sol_rrr     |cm			   |random roughness after a rain event
!!	sol_orr     |cm			   |oriented roughness (ridges) after a rain event
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic:exp 
!!    SWAT:eiusle

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer ::j
	real:: df, hru_slpp, sol_orgm, sol_orr, sol_rrr, ei 
      j = 0
      j = ihru


!! Calculate current cummulative erosivity and rainfall
	ei = usle_ei*18.7633
	if(itill(j) ==1)then
	  cumeira(j) = cumeira(j) + ei
	  cumei(j) = cumeira(j) - ei
	  cumrai(j) = cumrai(j) + precipday
	  cumrt(j) = cumrai(j) - precipday
      end if
!! Calculate the decay factor df based on %clay and %organic matter or %organic carbon
!	sol_orgm = (sol_rsd(1,j)*0.01)/(sol_z(1,j)*sol_bd(1,j))
	sol_orgm = sol_cbn(1,j)/0.58
	xx = (0.943 - 0.07 * sol_clay(1,j) + 0.0011 * sol_clay(1,j)**2      
     &    - 0.67 * sol_orgm + 0.12 * sol_orgm**2)
      if (xx > 1.) then
        df = 1.
      else
        df = exp (xx)
      end if
      

!! Determine the current random and oriented roughness using cumei and cumrt and initial
!! random and oriented roughness values
      
	sol_rrr = 0.1*ranrns_hru(j)                                         
     &	* exp(df*(-0.0009*cumei(j)-0.0007 * cumrt(j)))
	
!	sol_orr = 0.1*sol_ori(j)*                                                &
!     &	exp(df*(-0.025*(cumei(j)**0.31)-0.0085*(cumrt(j)**0.567)))

!! Compute the current maximum depressional storage using percent slope steepness 
!! and current random and oriented roughness values determined above
	hru_slpp = hru_slp(j)*100
!	if(irk=0) then !irk=0 for random rough, and irk=1, for oriented roughness
	stmaxd(j)= 0.112*sol_rrr+0.031*sol_rrr**2-0.012*sol_rrr*hru_slpp
!	else 
!	stmaxd(j)= 0.112*sol_orr+0.031*sol_orr**2-0.012*sol_orr*hru_slpp 
!	endif
   
   	return
	end