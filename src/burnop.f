      subroutine burnop    
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs all management operations             

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ibrn        |none          |counter in readmgt 
!!    iburn(:     |julian date   |date of burning  
!!    i_burn      |none          |counter that tracks curent burn operation  
!!    burn_frlb   |none          |fraction of biomass and residue that burn(input in
!!                               |management file) range (0 - 1.0)                         
!!    nro         |none          |sequence number of year in rotation
!!    phub        |              |heat units to schedule burning
!!    pburn       |              |amount of phosphorus that burns - removed from plant
!!                               |phosphorus and added to soil organic phosphorus 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
   
      integer :: j
      real :: aphu

      j = 0
      j = ihru

      xx = burn_frlb
      bio_ms(j) = bio_ms(j) * xx
      plantn(j) = plantn(j) * xx
      pburn = plantp(j) * xx
      sol_orgp(1,j) = sol_orgp(1,j) + pburn
      plantp(j) = plantp(j) - pburn
      sol_rsd(1,j) = sol_rsd(1,j) * xx
      sol_fon(1,j) = sol_fon(1,j) * xx
      sol_aorgn(1,j) = sol_aorgn(1,j) * xx
      sol_orgn(1,j) = sol_orgn(1,j) * xx
  !     sol_fop - remains after burn
      i_burn(j) = i_burn(j) + 1      

      return
      end
