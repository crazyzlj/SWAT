      subroutine det_pond
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    the purpose of this program is to read in data from the detention pond
!!    input file (.dtp) and perform computations

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dtp_evrsv      |none          |detention pond evaporation coefficient
!!    dtp_weirtype(:)|none          |Type of weir: 1 rectangular and 2 circular
!!    dtp_numweir(:) |none          |Total number of weirs in the BMP
!!    dtp_numstage(:)|none          |Total number of stages in the weir
!!    dtp_wdratio(:,:)|none         |Width depth ratio (rectangular wier) at different stages
!!    dtp_depweir(:,:)|m            |Depth of rectangular wier at different stages
!!    dtp_diaweir(:,:)|m            |Diameter of circular wier at different stages
!!    dtp_retperd(:,:)|years        |Return period at different stages
!!    dtp_pcpret(:,:)|mm            |precipitation for different return periods (not used)
!!    dtp_cdis(:,:)  |none          |coeffieicne of discharge at different stages
!!    hhvaroute(2,:,:) |m^3 H2O     |water
!!    hhvaroute(3,:,:) |metric tons |sediment or suspended solid load
!!    i_mo           |none          |current month of simulation
!!    sub_subp_dt(:,:)  |mm H2O      |precipitation for time step in subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    hhvaroute(2,:,:) |m^3 H2O    |water
!!    hhvaroute(3,:,:) |metric tons|sediment or suspended solid load
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |time step counter
!!    k           |none          |weir stage counter
!!    titldum     |NA            |dummy string
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT:  surf_seep,surf_evap,water_depth
!!    Intrinsic: Log,exp

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
      implicit none

      character (len=80) :: titldum
      integer :: ii, k, sb
      real :: qin,qout,qpnd,sedin,sedout,sedpnd,spndconc,qdepth,
     &        watdepact,qstage,backup_length,seep_sa,evap_sa,pcp_vol,
     &        evap_vol,seep_vol,warea,pi,qovmax,qaddon,depaddon
      
      pi = 3.14159
      sb = inum1
      qout = 0.; sedout = 0.; depaddon = 0.

      if (iyr<dtp_iyr(sb) .or. 
     &(iyr==dtp_iyr(sb) .and. i_mo<dtp_imo(sb))) then
         return
      endif
      
      !! Get initial values from previous day
      qpnd = dtp_ivol(sb) !m^3
      sedpnd = dtp_ised(sb) !tons

      !!	iterate for subdaily flow/sediment routing
      do ii=1,nstep
      
         qout = 0.; qovmax = 0
         qin = hhvaroute(2,ihout,ii) + qpnd !m^3
         sedin = hhvaroute(3,ihout,ii) + sedpnd  !tons
         if (qin>1e-6) then
            spndconc = sedin / qin !initial sed conc, tons/m3
         else
            cycle
         end if

	   !! Estimate water depth
	   	 qdepth = dtp_parm(sb) * (3.*qin*ch_s(2,sb)/pi)**0.33333

	   !! skip to next time step if no ponding occurs 
	     if (qdepth<=0.0001) cycle   
         
         !! Calculate weir outflow 
         do k=1,dtp_numstage(sb)  
            qstage = 0.
            ! height to the top of addon from bottom
            depaddon = depaddon + dtp_addon(sb,k)
            if (k>1) depaddon = depaddon + dtp_depweir(sb,k-1)  
           !  volume below the current stage addon
            qaddon = (depaddon / dtp_parm(sb)) ** 3.  
     &  	            / (3.*ch_s(2,sb)) * pi !m3

            !! Circular weir ? 
	         if (dtp_weirtype(sb,k)==2) then
	            dtp_depweir(sb,k) = dtp_diaweir(sb,k) + dtp_addon(sb,k)
	         end if

            !! Fully submerged 
	         if (qdepth>dtp_depweir(sb,k)) then  
                qdepth = qdepth - dtp_depweir(sb,k) 
                if (dtp_weirtype(sb,k)==1) then
                  watdepact = qdepth + dtp_depweir(sb,k) / 2
 	              warea = dtp_depweir(sb,k) * dtp_wrwid(sb,k)
                else
                  watdepact = qdepth + dtp_diaweir(sb,k) / 2
  	              warea = 3.14159 * dtp_diaweir(sb,k) ** 2 / 4.
                end if               

		         !! Estimate discharge using orifice equation
   	            qstage = dtp_cdis(sb,k) * 0.6 * warea * 
     & 	            sqrt(19.6 * watdepact) !m3/s/unit
                qstage = qstage * dtp_numweir(sb) * 60. * idt !m^3
	            !Limit total outflow amount less than available water above addon
	            if(qin-qstage<qaddon) qstage = qin - qaddon
               
               !! Flow over the emergency weir
                if (k==dtp_numstage(sb)) then
                   qstage = qstage + dtp_cdis(sb,k) * 1.84 * 
     &             dtp_totwrwid(sb) * (qdepth ** 1.5) * 60. * idt !m3/s
                end if
                qout = qout + qstage 
          
            !! Partially submerged
	         else    
	            watdepact = qdepth - dtp_addon(sb,k)  !! look for add on
	            if (watdepact<0) watdepact = 0.  !! created for add on
                if (dtp_weirtype(sb,k)==2) then
                   dtp_wrwid(sb,k) = dtp_diaweir(sb,k) * 0.667
                end if
		         !! Estimate weir/orifice discharge
		        qstage = dtp_cdis(sb,k) * 1.84 * dtp_wrwid(sb,k) * 
     &		       watdepact ** 1.5 !m3/s
		        qstage = qstage * dtp_numweir(sb) * 60. * idt !m^3

	            !Limit total outflow amount less than available water above addon
	            if(qin-qstage<qaddon) qstage = qin - qaddon

	   	        ! Limit overflow amount to the volume above add-on level
		        if(qin>qaddon) qovmax = qin - qaddon		        
		        if(qstage>qovmax) qstage = qovmax  

		         !! Use stage-discharge relationship if available
		        if (dtp_stagdis(sb)==1) then  
			       select case(dtp_reltype(sb))
			       case(1) !! 1 is exponential function
			          qstage = dtp_coef1(sb) * exp(dtp_expont(sb) * qdepth) +
     &			             dtp_intcept(sb) 
			       case(2) !! 2 is Linear function
			          qstage = dtp_coef1(sb) * qdepth + dtp_intcept(sb)       
			       case(3) !! 3 is logarthmic function
			          qstage = dtp_coef1(sb) * log(qdepth) + dtp_intcept(sb)  
			       case(4) !! 4 is power function
			          qstage = dtp_coef1(sb) * (qdepth**3) + dtp_coef2(sb) * 
     & 			        (qdepth**2) + dtp_coef3(sb) * qdepth + dtp_intcept(sb)
                   case(5)
                      qstage = dtp_coef1(sb)*(qdepth**dtp_expont(sb))+
     &                   dtp_intcept(sb)
			       end select 
		           qstage = qstage * 60. * idt
	            end if  !! end of stage-discharge calculation
                qout = qout + qstage 
                exit
	         end if  
            
	      
	     end do  !! End of weir discharge calculations
         
         !! Check mss balance for flow
         if (qout>qin) then !no detention occurs
            qout = qin
            qpnd = 0.
         else !detention occurs
            !!	Estimating surface area of water
            backup_length = qdepth / ch_s(2,sb)
            call surf_seep(qdepth,backup_length,seep_sa)
            call surf_evap(qdepth,backup_length,evap_sa)

            !! converting surface area to hectares (ha)
            seep_sa = seep_sa / 10000.0  
            evap_sa = evap_sa / 10000.0  

            !!	Estimate rainfall, evapotranspiration, and seepage
            pcp_vol  = 10.0 * sub_subp_dt(sb,ii) * evap_sa !m^3
            evap_vol = 10.0 * dtp_evrsv(sb) * pet_day * evap_sa !m^3
            seep_vol = 10.0 * ch_k(2,sb) * seep_sa * idt / 60. !m^3

            !!	Check mass balance for water in the pond
            qpnd = qin + pcp_vol - qout - evap_vol - seep_vol
            if (qpnd<0) qpnd = 0.
	   end if
         
         !! Mass balance for sediment
         sedout = spndconc * qout !tons
         sedpnd = max(0.,sedin - sedout) !tons
	      
   	   !! Store flow/sediment out of the pond at the subbasin outlet
   	   hhvaroute(2,ihout,ii) = max(0.,qout)
   	   hhvaroute(3,ihout,ii) = max(0.,sedout)

	   
      end do  !! Outermost do loop ends here

      ! Store end-of-day values for next day
      dtp_ivol(sb) = qpnd !m^3
      dtp_ised(sb) = sedpnd !tons

      end subroutine 
   !-------------------------------------------------------------------
   	
	subroutine surf_seep(a,b,sa)

   !!	This subroutine computes seepage surface area of 
   !!	water backed up behind the detention pond weir
   !!	references: mathworld.wolfram.com  and 
   !!	en.wikipedia.org/wiki/Prolate_spheroid

	   implicit none
	   real::pi,a,b,sa,asq,bsq,ecc,intercal


	   pi=3.141593

	   asq=a*a
	   bsq=b*b
	   ecc=sqrt(1-(asq/bsq))
	   intercal=a*b*asin(ecc)/ecc

	   sa=pi*(asq+intercal)/2.0
   	
   	
	end subroutine surf_seep
   !-------------------------------------------------------------------

	subroutine surf_evap(a,b,saevap)

   !!	This subroutine computes surface area of 
   !!	water backed up available for evaporation

	   implicit none
	   real::pi,a,saevap,b

	   pi=3.141593
	   saevap=pi*a*b/2.0
   	
	   return
	end subroutine surf_evap
   !-------------------------------------------------------------------

