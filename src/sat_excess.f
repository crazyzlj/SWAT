      subroutine sat_excess(j1)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine is the master soil percolation component.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    icrk        |none          |crack flow code
!!                               |1 simulate crack flow in watershed
!!    inflpcp     |mm H2O        |amount of precipitation that infiltrates
!!                               |into soil (enters soil)
!!    ihru        |none          |HRU number
!!    nn          |none          |number of soil layers
!!    sol_fc(:,:) |mm H2O        |amount of water available to plants in soil
!!                               |layer at field capacity (fc - wp)
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |the current day (less wp water)
!!    sol_ul(:,:) |mm H2O        |amount of water held in the soil layer at
!!                               |saturation
!!    voltot      |mm            |total volume of cracks expressed as depth
!!                               |per unit area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flat(:,:)   |mm H2O        |lateral flow storage array
!!    latlyr      |mm H2O        |lateral flow in soil layer for the day
!!    latq        |mm H2O        |total lateral flow in soil profile for the 
!!                               |day in HRU
!!    lyrtile     |mm H2O        |drainage tile flow in soil layer for day
!!    qtile       |mm H2O        |drainage tile flow in soil profile for the day
!!    sep         |mm H2O        |micropore percolation from soil layer
!!    sepbtm      |mm H2O        |percolation from bottom of soil profile for
!!                               |the day in HRU
!!    sol_prk(:,:)|mm H2O        |percolation storage array
!!    sol_st(:,:) |mm H2O        |amount of water stored in the soil layer on
!!                               |the current day (less wp water)
!!    sol_sw(:)   |mm H2O        |amount of water stored in the soil profile
!!                               |on current day
!!    sw_excess   |mm H2O        |amount of water in excess of field capacity
!!                               |stored in soil layer on the current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    j1          |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: percmacro, percmicro

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, j1, ii, isp
      real:: ul_excess,qlyr,pormm,rtof

      j = 0
      j = ihru
      isp = isep_typ(j) 	   !! J.Jeong 3/09/09
      rtof = 0.5


 	if (isep_opt(j)==2.and.j1==i_sep(j)) then
	  
	  ii = j1 
	  qlyr = sol_st(ii,j)
	  
	  ! distribute excess STE to upper soil layers 
	  do while (qlyr>0.and.ii>1)
	    
	   ! distribute STE to soil layers above biozone layer
	   if (sol_st(ii,j) > sol_ul(ii,j)) then
	      
	     qlyr = max(sol_st(ii,j) - sol_ul(ii,j),0.) 	! excess water moving to upper layer
	     sol_st(ii,j) = sol_st(ii,j) - qlyr  
         
         qvol = qlyr * hru_ha(j) * 10.         ! volume water m^3
	     xx = qvol / hru_ha(j) / 1000.
	     sol_no3(ii,j) = sol_no3(ii,j) - xx * (sptno3concs(isp) 
     &                   + sptno2concs(isp))  
	     sol_nh3(ii,j) = sol_nh3(ii,j) - xx * sptnh4concs(isp) 
	     sol_orgn(ii,j) = sol_orgn(ii,j) - xx * sptorgnconcs(isp)*0.5
         sol_fon(ii,j) = sol_fon(ii,j) - xx * sptorgnconcs(isp) * 0.5
         sol_orgp(ii,j) = sol_orgp(ii,j) - xx * sptorgps(isp) * 0.5
	     sol_fop(ii,j) = sol_fop(ii,j) - xx * sptorgps(isp) * 0.5
         sol_solp(ii,j) = sol_solp(ii,j) - xx * sptminps(isp)  

	     ! add soil moisture and nutrient to upper layer
	     sol_st(ii-1,j) = sol_st(ii-1,j) + qlyr ! add excess water to upper layer
	     sol_no3(ii-1,j) = sol_no3(ii-1,j) + xx * (sptno3concs(isp) 
     &                   + sptno2concs(isp))  
	     sol_nh3(ii-1,j) = sol_nh3(ii-1,j) + xx * sptnh4concs(isp) 
	     sol_orgn(ii-1,j) = sol_orgn(ii-1,j) + xx*sptorgnconcs(isp)*0.5
         sol_fon(ii-1,j) = sol_fon(ii-1,j) + xx * sptorgnconcs(isp)*0.5
         sol_orgp(ii-1,j) = sol_orgp(ii-1,j) + xx * sptorgps(isp) * 0.5
	     sol_fop(ii-1,j) = sol_fop(ii-1,j) + xx * sptorgps(isp) * 0.5
         sol_solp(ii-1,j) = sol_solp(ii-1,j) + xx * sptminps(isp)  
	   else 
	     qlyr = 0.
	   endif
	  
	   ! Add surface ponding to the 10mm top layer when the top soil layer is saturated
	   !  and surface ponding occurs.
	   if (ii==2) then
	     qlyr = sol_st(1,j) - sol_ul(1,j)
	     ! excess water makes surface runoff
	     if (qlyr>0) then
	         sol_st(1,j) = sol_ul(1,j)
	         surfq(j) = surfq(j) + qlyr 
		     qvol = qlyr * hru_ha(j) * 10.
		       ! nutrients in surface runoff
		     xx = qvol / hru_ha(j) / 1000.
	         surqno3(j) = surqno3(j) + xx 
     &                      * (sptno3concs(isp) + sptno2concs(isp)) 
             surqsolp(j) =  surqsolp(j) +  xx * sptminps(isp) 
               
	         !compute runoff lag for the ponded STE and add to already estimated qday
	         surf_bs(1,j) = Max(1.e-6, surf_bs(1,j) + qlyr)
               cbodu(j) = (cbodu(j) * qday + sptbodconcs(isep_typ(j)) 
     &             * qlyr * brt(j)) / (qday + qlyr * brt(j)) !add septic effluent cbod (mg/l) concentration 
               qday = qday + qlyr * brt(j) !runoff that drains into the main channel for the day
               surf_bs(1,j) = surf_bs(1,j) - qday

	         ! Initiate counting the number of days the system fails and makes surface ponding of STE
	         if(sep_tsincefail(j)==0) sep_tsincefail(j) = 1
             qlyr = 0.
	     endif     
	   endif

       ii = ii - 1
	  end do
	endif
	       
      if (isep_opt(j)==0) then
      if (j1 < sol_nly(j)) then
        if (sol_st(j1,j) - sol_ul(j1,j) > 1.e-4) then
          sepday = sepday + (sol_st(j1,j) - sol_ul(j1,j))
          sol_st(j1,j) = sol_ul(j1,j)
        end if
      else

        if (sol_st(j1,j) - sol_ul(j1,j) > 1.e-4) then
          ul_excess = sol_st(j1,j) - sol_ul(j1,j)
          sol_st(j1,j) = sol_ul(j1,j)
          nn = sol_nly(j)
          do ly = nn - 1, 1, -1
            sol_st(ly,j) = sol_st(ly,j) + ul_excess
            if (sol_st(ly,j) > sol_ul(ly,j)) then
              ul_excess = sol_st(ly,j) - sol_ul(ly,j)
              sol_st(ly,j) = sol_ul(ly,j)
            else
              ul_excess = 0.
              exit
            end if
            if (ly == 1 .and. ul_excess > 0.) then
              !! add ul_excess to depressional storage and then to surfq
              pot_vol(j) = pot_vol(j) + ul_excess
            end if
          end do
          !compute tile flow again after saturation redistribution
!         if (ldrain(j) > 0.) then
!           ul_excess = sol_st(ldrain(j),j) - sol_fc(ldrain(j),j) 
!           if (ul_excess > 0.) then
!             lyrtilex = ul_excess * (1. - Exp(-24. / tdrain(j)))
!           end if
!         end if
        end if
      end if
      end if

      return
      end