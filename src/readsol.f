      subroutine readsol

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU/subbasin soil properties file 
!!    (.sol). This file contains data related to soil physical properties and
!!    general chemical properties.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none          |HRU number
!!    mlyr          |none          |maximum number of soil layers
!!    idplt(:)      |none          |land cover/crop identification code for
!!                                 |first crop grown in HRU (the only crop if
!!                                 |there is no rotation)
!!    rdmx(:)       |m             |maximum root depth of plant
!!    rsdin(:)      |kg/ha         |initial residue cover
!!    sol_no3(:,:)  |mg N/kg       |concentration of nitrate in soil layer
!!    sol_orgn(1,:) |mg N/kg soil  |organic N concentration in top soil layer
!!    sol_orgp(1,:) |mg P/kg soil  |organic P concentration in top soil layer
!!    sol_solp(1,:) |mg P/kg soil  |soluble P concentration in top soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    anion_excl(:) |none          |fraction of porosity from which anions
!!                                 |are excluded
!!    sol_clay(:,:) |%             |percent clay content in soil material
!!    sol_rock(:,:) |%            |percent of rock fragments in soil layer
!!    sol_silt(:,:) |%             |percent silt content in soil material
!!    snam(:)       |NA            |soil series name
!!    sol_alb(:)    |none          |albedo when soil is moist
!!    sol_awc(:,:)  |mm H20/mm soil|available water capacity of soil layer
!!    sol_bd(:,:)   |Mg/m**3       |bulk density of the soil
!!    sol_cbn(:,:)  |%             |percent organic carbon in soil layer
!!    sol_crk(:)    |none          |crack volume potential of soil
!!    sol_k(:,:)    |mm/hr         |saturated hydraulic conductivity of soil 
!!                                 |layer
!!    sol_nly(:)    |none          |number of soil layers 
!!    sol_no3(:,:)  |mg N/kg       |concentration of nitrate in soil layer
!!    sol_orgn(1,:) |mg N/kg soil  |organic N concentration in top soil layer
!!    sol_orgp(1,:) |mg P/kg soil  |organic P concentration in top soil layer
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil layer
!!                                 |classified as residue
!!    sol_solp(1,:) |mg P/kg soil  |soluble P concentration in top soil layer
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus 
!!                                 |pool
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    sol_zmx(:)    |mm            |maximum rooting depth
!!    usle_k(:)     |none          |USLE equation soil erodibility (K) factor
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flag        |none          |flag to exit do loop
!!    j           |none          |counter
!!    jj          |none          |dummy variable to hold value
!!    n           |none          |counter
!!    nly         |none          |number of soil layers
!!    plt_zmx     |mm            |rooting depth of plant
!!    sol_sand(:,:) |%             |percent sand content of soil material
!!    sol_ec(:)   |dS/m          |electrical conductivity of soil layer
!!    titldum     |NA            |title line/skipped line in .sol file
!!    xx          |none          |variable to hold value
!!    yy          |none          |variable to hold value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Exp, Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
!      integer :: j, nly, n, jj, flag, eof
      integer :: j, nly, eof            !claire: jj, n, and flag are not used 12/02/09
!      real :: xx, plt_zmx, yy
      real :: plt_zmx                   !Claire, xx and yy are not used 12/2/09

!!    initialize local variables
      nly = 0
      plt_zmx = 0.

      read (107,5500) titldum
      read (107,5100) snam(ihru)
      read (107,5200) hydgrp(ihru)
      read (107,5300) sol_zmx(ihru)
      read (107,5400) anion_excl(ihru)
      read (107,5600) sol_crk(ihru)
      read (107,5500) titldum
      read (107,5000) (sol_z(j,ihru), j = 1, mlyr)

      
      !! calculate number of soil layers in HRU soil series
      do j = 1, mlyr
!!    khan soils
!      sol_z(j,ihru) = sol_z(j,ihru) / 5.0
        if (sol_z(j,ihru) <= 0.001) sol_nly(ihru) = j - 1
        if (sol_z(j,ihru) <= 0.001) exit
      enddo
      if (sol_nly(ihru) == 0) sol_nly(ihru) = 10
      nly = sol_nly(ihru)

      eof = 0
      do
      read (107,5000) (sol_bd(j,ihru), j = 1, nly)
      read (107,5000) (sol_awc(j,ihru), j = 1, nly)
      read (107,5000) (sol_k(j,ihru), j = 1, nly)
      read (107,5000) (sol_cbn(j,ihru), j = 1, nly)
      read (107,5000) (sol_clay(j,ihru), j = 1, nly)
      read (107,5000) (sol_silt(j,ihru), j = 1, nly)
      read (107,5000) (sol_sand(j,ihru), j = 1, nly)

      read (107,5000) (sol_rock(j,ihru), j = 1, nly)
      read (107,5000) sol_alb(ihru)
      read (107,5000) usle_k(ihru)
!    change below double subscripted sol_ec statement 1/27/09 when making septic changes
      read (107,5000,iostat=eof) (sol_ec(j,ihru), j = 1, nly)
!    change below double subscripted sol_ec statement 1/27/09 when making septic changes

      !! MJW added rev 490
	!!CaCo3 content (%) 
	if (eof < 0) exit	
	  read (107,5000,iostat=eof) (sol_cal(j,ihru), j = 1, nly) 	
	!! PH-H20  
	if (eof < 0) exit
	  read (107,5000,iostat=eof) (sol_ph(j,ihru), j = 1, nly) 
      
      if (eof < 0) exit
      exit
      end do

	!!Armen January 2009 
	do j=1, nly
         if (sol_rock(j,ihru) > 98.0) sol_rock(j,ihru) = 98.0
         if (sol_awc(j,ihru) <= .01) sol_awc(j,ihru) = .01
         if (sol_awc(j,ihru) >= .80) sol_awc(j,ihru) = .80
         if (sol_cbn(j,ihru) < 1.e-6) sol_cbn(j,ihru) = .10
         sol_n(j,ihru) = sol_cbn(j,ihru) / 11.0
	end do	
	!!Armen January 2009 end

!!    add 10mm layer at surface of soil
      if (sol_z(1,ihru) > 10.1) then
        sol_nly(ihru) = sol_nly(ihru) + 1
        nly = nly + 1
        do j = nly, 2, -1
          sol_z(j,ihru) = sol_z(j-1,ihru)
          sol_bd(j,ihru) = sol_bd(j-1,ihru)
          sol_awc(j,ihru) = sol_awc(j-1,ihru)
          sol_k(j,ihru) = sol_k(j-1,ihru)
          sol_cbn(j,ihru) = sol_cbn(j-1,ihru)
	!!Armen January 2009
                  sol_n(j,ihru) = sol_n(j-1,ihru)
!                 sol_mc(j,ihru) = sol_mc(j-1,ihru)
!                 sol_mn(j,ihru) = sol_mn(j-1,ihru)
!                 sol_mp(j,ihru) = sol_mp(j-1,ihru)
                  sol_rock(j,ihru) = sol_rock(j-1,ihru) !!! Armen 13 Jan 2008
                  sol_clay(j,ihru) = sol_clay(j-1,ihru)
                  sol_sand(j,ihru) = sol_sand(j-1,ihru) !!! Claire 2 Dec 2009
                  sol_silt(j,ihru) = sol_silt(j-1,ihru) !!! Claire 2 Dec 2009
                  sol_ph(j,ihru) = sol_ph(j-1,ihru) !! mjw rev 490
		          sol_cal(j,ihru) = sol_cal(j-1,ihru) !! mjw rev 490
	!!Armen January 2009 end
!    change below double subscripted sol_ec statement 1/27/09 when making septic changes
          sol_ec(j,ihru) = sol_ec(j-1,ihru)
!    change below double subscripted sol_ec statement 1/27/09 when making septic changes
          sol_no3(j,ihru) = sol_no3(j-1,ihru)
          sol_orgn(j,ihru) = sol_orgn(j-1,ihru)
          sol_orgp(j,ihru) = sol_orgp(j-1,ihru)
          sol_solp(j,ihru) = sol_solp(j-1,ihru)
        end do
        sol_z(1,ihru) = 10.
      endif

      if (isproj == 2) then
        call estimate_ksat(sol_clay(j,ihru),sol_k(j,ihru))  !!  NK June 28, 2006
      endif


!!    compare maximum rooting depth in soil to maximum rooting depth of
!!    plant
      if (sol_zmx(ihru) <= 0.001) sol_zmx(ihru) = sol_z(nly,ihru)
      plt_zmx = 0.
	if (idplt(ihru) > 0) then
	   if (idc(idplt(ihru)) > 0) then
           plt_zmx = 1000. * rdmx(idplt(ihru))
	   end if
      end if
      if (sol_zmx(ihru) > 1. .and. plt_zmx > 1.) then
         sol_zmx(ihru) = Min(sol_zmx(ihru),plt_zmx)
      else
         !! if one value is missing it will set to the one available
         sol_zmx(ihru) = Max(sol_zmx(ihru),plt_zmx)
      end if

!! create a layer boundary at maximum rooting depth (sol_zmx)
      !if (sol_zmx(i) > 0.001.and.sol_zmx(ihru)/=sol_z(nly,ihru)) then
      !   call layersplit (sol_zmx(ihru))
      !end if

!! create a bizone layer in septic HRUs
      if (isep_opt(ihru) /= 0) then 
	 if (bz_z(ihru)+bz_thk(ihru) > sol_z(nly,ihru)) then
	   if (sol_z(nly,ihru)>bz_thk(ihru)+10.) then !min. soil thickness for biozone layer (10mm top+biozone layer thickness)
	      bz_z(ihru) = sol_z(nly,ihru) - bz_thk(ihru)
	   else
	      bz_z(ihru) = sol_z(nly,ihru)
	      sol_z(nly,ihru) = sol_z(nly,ihru) + bz_thk(ihru)
	   endif
       endif 
       if (bz_z(ihru) > 0.) then 
         call layersplit (bz_z(ihru))
         dep_new = bz_z(ihru) + bz_thk(ihru)
         call layersplit (dep_new)  
         i_sep(ihru) = iseptic
       endif    
      endif

      nly = sol_nly(ihru)
      
!!    set default values/initialize variables
      if (sol_alb(ihru) < 0.1) sol_alb(ihru) = 0.1
      if (anion_excl(ihru) <= 1.e-6) anion_excl(ihru) = anion_excl_bsn
      if (anion_excl(ihru) >= 1.) anion_excl(ihru) = 0.99
      if (rsdin(ihru) > 0.) sol_rsd(1,ihru) = rsdin(ihru)
      do j = 1, nly
        a = 50.0
        b = 20.0
        c = 5.0
        d = 2.0           
        nota = 10
        if (sol_k(j,ihru) <= 0.0) then 
          if (hydgrp(ihru) == "A") then
            sol_k(j,ihru) = a
	    else
          if (hydgrp(ihru) == "B") then
            sol_k(j,ihru) = b
	    else
          if (hydgrp(ihru) == "C") then
            sol_k(j,ihru) = c
	    else
          if (hydgrp(ihru) == "D") then
!            sol_k(j,ihru) = c
            sol_k(j,ihru) = d          !Claire 12/2/09
         else 
           sol_k(j,ihru) = nota
          endif
          endif
          endif
          endif
         endif
        if (sol_bd(j,ihru) <= 1.e-6) sol_bd(j,ihru) = 1.3
        if (sol_bd(j,ihru) > 2.) sol_bd(j,ihru) = 2.0
        if (sol_awc(j,ihru) <= 0.) sol_awc(j,ihru) = .005
        !! Defaults for ph and calcium mjw average of 20,000 SSURGO soils mjw rev 490
        if (sol_cal(j,ihru)<= 1.e-6) sol_cal(j,ihru) = 2.8
        if (sol_ph(j,ihru)<= 1.e-6) sol_ph(j,ihru) = 6.5
      end do


      close (107)
      return
 5000 format (27x,15f12.2)
 5100 format (12x,a16)
 5200 format (24x,a1)
 5300 format (28x,f12.2)
 5400 format (51x,f5.3)
 5500 format (a80)
 5600 format (33x,f5.3)
      end