      subroutine carbon
      !! This code simulates organic C, N, and P cycling in the soil
      !! The code was developed by Armen R. Kemanian and Stefan Julich
      !! It has been adapted from Kemanian and Stockle (2010) (European Journal of Agronomy 32:22-29)
      !! and crafted to accomodate to SWAT conventions
      !! Plant residues and manure residues are decomposed separately
      !! For convenience, the denitrification subroutine is called from here
      !! March 2009: testing has been minimal and further adjustments are expected
      !! manuscript describing this subroutine to be submitted to Ecological Modelling (September, 2010)
      !! use with caution and report anomalous results to akemanian@psu.edu, and jeff.arnold@ars.usda.edu, stefan.julich@tudor.lu
      
    
!!!!!!!
!      cmup_kgh       kg/ha    current soil carbon for first soil layer
!      cmtot_kgh      kg/ha    current soil carbon integrated - aggregating 
!                                 all soil layers 
 
      !! local variables
      !! cx = saturated soil carbon concentration (%) (Hassink and Whitmore, 1997)
      !! decf = decomposition factor  
      !! net_N = nitrogen net mineralization
      !! net_P = phosphorus net mineralization
      !! rnet_N, rnet_P, mnet_N, mnet_P for residue and manure respectively
      !! sol_cdec = carbon decomposition (CO2)
      !! tilf = tillage factor
      !! resc_hum = humified carbon residue
      !! manc_hum = humified carbon manure

      use parm
      
      INTERFACE
      
	real*8 Function fwf(fc,wc,pwp)
      real*8, intent (in) :: fc, wc, pwp
      end function
      
      real*8 Function fof(void,por)
        real*8, intent (in) :: void, por
      end function

      real*8 Function ftilf(tillage, wc, sat)
        real*8, intent (in out) :: tillage
        real*8, intent (in) :: wc, sat
      End function

      real*8 Function fcx(pclay)
        real*8, intent (in) :: pclay
      End function

	real*8 Function fsol_cdec(pcarbon, cx, cfdec, tilf, csf, sol_cmass)
        real*8, intent (in) :: pcarbon, cx, cfdec, tilf, csf, sol_cmass
	End function

      real*8 Function fCNnew(yy1,yy2,CNpool,yy5)
        real*8, intent(in) :: yy1, yy2, CNpool
        real*8, intent(in) :: yy5
      End function

	real*8 Function fhc(pclay, pcarbon, cx) 		
        real*8, intent(in) :: pclay, pcarbon, cx
	End function

	real*8 Function fnetmin(poold, R1, R2, hc, dummy, poolm, xinorg, cc1)
        real*8, intent(in) :: R1, R2, hc, poolm, xinorg, cc1
        real*8, intent(in out) :: poold, dummy
      End function      
      
      END INTERFACE
    
    
    !! private variables
      real*8 :: cx, decf, rhc, mhc, sol_cdec, tilf
      real*8 :: resc_hum, manc_hum
      real*8 :: xx, xx1, xx2, xx3, xx4, csf
      real*8 :: rdc, mdc, wdn, cdg, sut
      real*8 :: CNsoil, CPsoil, NPsoil
      real*8 :: CNres, CPres, CNman, CPman, rCNnew, mCNnew
      real*8 :: sol_thick, sol_mass, sol_cmass, sol_nmass
      real*8 :: net_N, net_P, rnet_N, rnet_P, mnet_N, mnet_P
      real*8 :: wc, fc, wf, of, void
      real*8 :: sat, ffres1, ffres2, ffman1, ffman2

    !! mass balance variables
      real*8 :: sum_c_i, sum_n_i, sum_p_i
      real*8 :: sum_c_f, sum_n_f, sum_p_f
      real*8 :: bal_c, bal_n, bal_p

    !! output variables for soil profile
      real*8 :: cmass_pro, nmass_pro, sol_orgp_pro
      real*8 :: sol_rsd_pro, sol_fon_pro, sol_fop_pro
      real*8 :: sol_mc_pro, sol_mn_pro, sol_mp_pro
      real*8 :: sol_no3_pro, sol_solp_pro, sol_nh3_pro
      real*8 :: sol_cdec_pro, wdn_pro, net_N_pro, solN_net_min
      real*8 :: solN_net_min_pro 

      integer :: j, k, kk



      j = 0; wdn = 0
      j = ihru
  
    !! initialize
      cmass_pro = 0.
      nmass_pro = 0.
      sol_orgp_pro = 0.
      sol_rsd_pro = 0.
      sol_fon_pro = 0.
      sol_fop_pro = 0.
      sol_mc_pro = 0.
      sol_mn_pro = 0.
      sol_mp_pro = 0.
      sol_no3_pro = 0.
      sol_nh3_pro = 0.
      sol_solp_pro = 0. 
      sol_cdec_pro = 0.
      wdn_pro = 0.
      net_N_pro = 0.
      solN_net_min = 0.
      solN_net_min_pro=0.
      
!!    zero new carbon variables for output.hru
      cmup_kgh(j) = 0.
      cmtot_kgh(j) = 0.
!!    zero new carbon variables for output.hru


      if (sol_cbn(1,j) == 0.) return	
 
      do k = 1, sol_nly(j)

      sum_c_i = 0.
      sum_n_i = 0.
      sum_p_i = 0.    

      rdc = 0.
      mdc = 0.
      rhc  = 0.
      mhc = 0.
      sol_cdec = 0.    
      resc_hum = 0.
      manc_hum = 0.
      rnet_N = 0.
      rnet_P = 0.
      mnet_N = 0.
      mnet_P = 0.
      net_N = 0.
      net_P = 0.

      rCNnew = 0.
      mCNnew = 0.

      wc = 0.
      fc = 0.
      wf = 0.
      of = 0.
      void = 0.
      cdg = 0.
      sut = 0.
      csf = 0.

      ffres = 0.
      ffres1 = 0.
      ffres2 = 0.

      ffman = 0.
      ffman1 = 0.
      ffman2 = 0.

	if (k == 1) then
	    sol_thick = sol_z(k,j)
	else	
	    sol_thick = sol_z(k,j) - sol_z(k-1,j)
	end if

      !! soil carbon and nitrogen mass
      sol_mass = (sol_thick / 1000.) * 10000. * sol_bd(k,j) 
     &     * 1000. * (1- sol_rock(k,j) / 100.)
      sol_cmass = sol_mass * (sol_cbn(k,j) / 100.) 
      sol_nmass = sol_mass * (sol_n(k,j) / 100.) 

	!! sum initial C,N,P for mass balance
	sum_c_i = sol_cmass + 0.43 * sol_rsd (k,j) + sol_mc(k,j)
	sum_n_i = sol_nmass + sol_no3(k,j) + sol_nh3(k,j) + sol_fon(k,j)
     &	+ sol_mn(k,j)
	sum_p_i = sol_orgp(k,j) + sol_solp(k,j) + sol_fop(k,j) 
     &	+ sol_mp(k,j)

      kk = k 
      if (k == 1) kk = 2

	if (sol_tmp(kk,j) > 0. .and. sol_st(kk,j) > 0.) then
		!! microbial processes if temp > 0 C
		!! microbial processes if water > pwp
	
		!!compute soil water factor - sut
		fc = sol_fc(kk,j) + sol_wpmm(kk,j)  ! units mm
		wc = sol_st(kk,j) + sol_wpmm(kk,j)  ! units mm
		sat = sol_ul(kk,j) + sol_wpmm(kk,j) ! units mm
		void = sol_por(kk,j) * (1. - wc / sat)	

        wf = fwf(fc,wc,sol_wpmm(kk,j))
        of = fof(void,sol_por(kk,j))
        sut = wf * of

        !! compute soil temperature factor - cdg
        cdg = fcgd(sol_tmp(kk,j))

		!! compute combined factor
		xx = 0.
!!		xx = sqrt(cdg * sut)
		xx = (cdg * sut) ** cf(j)
		if (xx < 0.) xx = 0.
		if (xx > 1.) xx = 1.
		csf = xx
	
		!! call denitrification (to use void and cdg factor)
		wdn = 0.
		if (cdg > 0. .and. void <= 0.1) then
			call ndenit(k,j,cdg,wdn,void)
		end if
		wshd_dnit = wshd_dnit + wdn * hru_dafr(j)
		wdntl = wdntl + wdn

		!! calculate soil carbon 'decomposition'
		!! tillage factor
		tilf = ftilf(tillagef(k,j), wc, sat)	
		!! saturated carbon concentration (%)
		cx = fcx(sol_clay(k,j))	
		!! soil carbon decomposition
		sol_cdec=fsol_cdec(sol_cbn(k,j),cx,cfdec(j),
     & 		tilf,csf,sol_cmass)

        !! residue and manure decomposition and N and P mineralization    
        CNsoil = sol_cbn(k,j) / sol_n(k,j)
        if (sol_orgp(k,j) < .01) then
          NPsoil = 25.
        else
          NPsoil = (sol_mass * sol_n(k,j) / 100.)/ sol_orgp(k,j)
        end if
        !sol_orgp(k,j) = dmin1(sol_orgp(k,j), 25.)  !commented 110118 Armen K
        CPsoil = CNsoil * NPsoil
    
        if (sol_rsd(k,j) > 0.00001) then

            ! This IF STATEMENT preserved in case residues with different properties are considered            
            !if (idplt(nro(j),icr(j),j) > 0) then
            !    decr = rsdco_pl(idplt(nro(j),icr(j),j)) * csf
            !else
            !    decr = 0.05 *  csf
            !end if

            !! residue decomposition
            rdc = 0.05 * csf * sol_rsd(k,j)

			!! humification factor
			rhc = fhc(sol_clay(k,j),sol_cbn(k,j),cx) * cfh(j)
            
            if (sol_fon(k,j) < .01) then
              CNres = 15.
            else
              CNres = 0.43 * sol_rsd(k,j) / sol_fon(k,j)
            end if
            CNres = dmax1(CNres, 15._8)
            
            if (sol_fop(k,j) < .01) then
              CPres = 400.
            else
              CPres = 0.43 * sol_rsd(k,j) / sol_fop(k,j)
            end if
            CPres = dmax1(CPres, 400._8)
            
            !! CN of new organic matter (humified residue)
            rCNnew = fCNnew(sol_no3(k,j),sol_mass,CNres, 1.1D+02)   ! 110.)

			!! N mineralization / immobilization			
			xx1 = sol_rsd(k,j)
			xx2 = sol_no3(k,j) + sol_nh3(k,j)
			rnet_N = fnetmin(rdc,CNres,rCNnew,rhc,ffres1,xx1,xx2, 4.3D-01)

            !! P mineralization / immobilization
            xx2 = sol_solp(k,j)
            rnet_P = fnetmin(rdc,CPres,CPsoil,rhc,ffres2,xx1,xx2, 4.3D-01)
  
            !! check if P limits N mineralization and re-adjust
            if (ffres2 < ffres1) then
                  rnet_N = rdc * 0.43 * (1. / CNres - rhc / rCNnew)
                ffres = ffres2
            else
                ffres = ffres1
            end if
        end if

        ! manure decomposition and N and P mineralization
        if (sol_mc(k,j) > 0.00001 .and. sol_mp(k,j) > 0.00001) then     

            !! residue decomposition
            !! decomposition rate about 1/2 of residue
            mdc = 0.025 * csf * sol_mc(k,j)

            !! humification factor
			mhc = 1.6 * fhc(sol_clay(k,j),sol_cbn(k,j),cx)
			
			CNman = sol_mc(k,j) / sol_mn(k,j)
			CPman = sol_mc(k,j) / sol_mp(k,j)
			
			!! CN of new organic matter (humified manure)
			mCNnew = fCNnew(sol_no3(k,j),sol_mass,CNman, 5.5D+01)

			!! N mineralization / immobilization			
			xx1 = sol_mc(k,j)
			xx2 = sol_no3(k,j) + sol_nh3(k,j)
			mnet_N = fnetmin(mdc,CNman,mCNnew,mhc,ffman1,xx1,xx2,1.0D+00)

            !! P mineralization / immobilization
            xx2 = sol_solp(k,j)
            mnet_P = fnetmin(mdc,CPman,CPsoil,mhc,ffman2,xx1,xx2,1.0D+00)
  
            !! check if P or N limit mineralization and re-adjust
            if (ffman2 < ffman1) then
                  mnet_N = mdc * (1. / CNman - mhc / mCNnew)
                ffman = ffman1
            else
                ffman = ffman2
            end if
        end if

        !! check if sufficient mineral N for both residue and manure decomposition
		if ((sol_no3(k,j) + sol_nh3(k,j)) > 0.) then
			xx = (rnet_N + mnet_N) / (sol_no3(k,j) + sol_nh3(k,j))
            if (xx < -1.) then
                rdc = -rdc / xx
                rnet_N = -rnet_N / xx
                rnet_P = -rnet_P / xx
                ffres = -ffres / xx

                mdc = -mdc / xx
                mnet_N = -mnet_N / xx
                mnet_P = -mnet_P / xx
                ffman = -ffman / xx
            end if
        end if
        
        !! check if sufficient mineral P for both residue and manure decomposition
        if (sol_solp(k,j) > 0.) then
            xx = (rnet_P + mnet_P) / sol_solp(k,j)
            if (xx < -1.) then
                rdc = -rdc / xx
                rnet_N = -rnet_N / xx
                rnet_P = -rnet_P / xx
                ffres = -ffres / xx

                mdc = -mdc / xx
                mnet_N = -mnet_N / xx
                mnet_P = -mnet_P / xx
                ffman = -ffman / xx
            end if
        end if

        resc_hum = rhc * rdc * 0.43
        manc_hum = mhc * mdc
        net_N = rnet_N + mnet_N
        net_P = rnet_P + mnet_P
        if (resc_hum == 0.) rCNnew = 1000.
        if (manc_hum == 0.) mCNnew = 1000.

		!! C N P pools update
		sol_cmass = sol_cmass + resc_hum + manc_hum - sol_cdec
 		sol_cbn(k,j) = 100. * sol_cmass / sol_mass
		
		sol_nmass = sol_nmass - sol_cdec / CNsoil
     &       	  + resc_hum / rCNnew + manc_hum / mCNnew

!! april 2010 output net_min
		solN_net_min=-(- sol_cdec / CNsoil
     &             + resc_hum / rCNnew + manc_hum / mCNnew)
 		sol_n(k,j) = 100. * sol_nmass / sol_mass
		sol_orgn(k,j) = sol_nmass ! for output
        sol_orgp(k,j) = sol_orgp(k,j) - sol_cdec / CPsoil
     &         + (resc_hum + manc_hum) / CPsoil

                if (ffres > 1.) ffres = 1.

        sol_rsd(k,j) = sol_rsd(k,j) * (1. - ffres)
        sol_fon(k,j) = sol_fon(k,j) * (1. - ffres)
        sol_fop(k,j) = sol_fop(k,j) * (1. - ffres)
     
                if (ffman > 1.) ffman = 1.

        sol_mc(k,j) = sol_mc(k,j) * (1. - ffman)
        sol_mn(k,j) = sol_mn(k,j) * (1. - ffman)
        sol_mp(k,j) = sol_mp(k,j) * (1. - ffman)

		!sol_no3(k,j) = sol_no3(k,j) + net_N + 
     &	!	sol_cdec * (1. / CNsoil)
		
		! add positive n-mineralization to ammonia pool in the layer
		if (rnet_N>0.) sol_nh3(k,j) = sol_nh3(k,j) + rnet_N 
		if (mnet_N>0.) sol_nh3(k,j) = sol_nh3(k,j) + mnet_N

		if (rnet_N<0.) then
			if (abs(rnet_N) < sol_nh3(k,j)) then
				sol_nh3(k,j) = sol_nh3(k,j) + rnet_N
			else
				xx4 = sol_nh3(k,j) + rnet_N
				sol_nh3(k,j) = 0.
				sol_no3(k,j) = sol_no3(k,j) + xx4
			end if
		end if
	
		if (mnet_N<0.) then
			if (abs(mnet_N) < sol_nh3(k,j)) then
				sol_nh3(k,j) = sol_nh3(k,j) + mnet_N
			else
				xx4 = sol_nh3(k,j) + mnet_N
				sol_nh3(k,j) = 0.
				sol_no3(k,j) = sol_no3(k,j) + xx4
			end if
		end if
	
		sol_nh3(k,j) = sol_nh3(k,j) + sol_cdec * (1. / CNsoil)
		
		sol_solp(k,j) = sol_solp(k,j) + net_P + 
     &		sol_cdec * (1. / CPsoil)

          wshd_rmn = wshd_rmn + net_N * hru_dafr(j)
		wshd_rmp = wshd_rmp + net_P * hru_dafr(j)
	 
		If (sol_rsd(k,j) < 1e-10) sol_rsd(k,j) = 1e-10
		If (sol_fon(k,j) < 1e-11) sol_fon(k,j) = 1e-11
		If (sol_fop(k,j) < 1e-12) sol_fop(k,j) = 1e-12
		If (sol_mc(k,j) < 1e-10) sol_mc(k,j) = 1e-10
		If (sol_mn(k,j) < 1e-11) sol_mn(k,j) = 1e-11
		If (sol_mp(k,j) < 1e-12) sol_mp(k,j) = 1e-12
		If (sol_no3(k,j) < 1e-12) sol_no3(k,j) = 1e-12

	end if

	!! balance file cswat_balance
	sum_c_f = sol_cmass + 0.43 * sol_rsd(k,j) + sol_cdec
     &	+ (1. - rhc) * rdc * 0.43 + sol_mc(k,j) + (1. - mhc) * mdc
	sum_n_f = sol_nmass + sol_no3(k,j) + sol_nh3(k,j) + sol_fon(k,j)  
     &	+ sol_mn(k,j) + wdn
	sum_p_f = sol_orgp(k,j) + sol_solp(k,j) + sol_fop(k,j)
     &	+ sol_mp(k,j)
		  
	bal_c = sum_c_i - sum_c_f
	bal_n = sum_n_i - sum_n_f
	bal_p = sum_p_i - sum_p_f

	!! writing daily output by layer for testing purposes of the routine SJ and AK 2010
	!!if (i==365) then
!!	   write (98,9000) iyr, i, k, j, sol_cmass, sol_cbn(k,j),
!!     &	sol_nmass, sol_n(k,j), sol_orgp(k,j), sol_rsd(k,j),
!!     &	sol_fon(k,j), sol_fop(k,j), sol_solp(k,j), sol_mc(k,j),
!!     &	sol_mn(k,j), sol_mp(k,j), sol_no3(k,j),
!!     &	sol_cmass/sol_nmass, sol_nmass/sol_orgp(k,j), sol_nh3(k,j),
!!     &    tilf, sol_cdec, wdn, net_N
!!      	 write (99,9001) iyr, i, k, j, bal_c, sum_c_i, sum_c_f, 
!!     &     bal_n,sum_n_i, sum_n_f, bal_p, sum_p_i, sum_p_f
	!!end if


!!    carbon outputs for .hru file
      if (k == 1) cmup_kgh(j) = sol_cmass
      cmtot_kgh(j) = cmtot_kgh(j) + sol_cmass
!!    carbon outputs for .hru file


	cmass_pro = cmass_pro + sol_cmass
	nmass_pro = nmass_pro + sol_nmass
	sol_rsd_pro = sol_rsd_pro + sol_rsd(k,j)
	sol_cdec_pro = sol_cdec_pro + sol_cdec
	sol_fon_pro = sol_fon_pro + sol_fon(k,j)
	sol_no3_pro = sol_no3_pro + sol_no3(k,j)
	sol_nh3_pro = sol_nh3_pro + sol_nh3(k,j)
	sol_orgp_pro = sol_orgp_pro + sol_orgp(k,j)
	sol_fop_pro = sol_fop_pro + sol_fop(k,j)
	sol_solp_pro = sol_solp_pro + sol_solp(k,j)
	sol_mc_pro = sol_mc_pro + sol_mc(k,j)
	sol_mn_pro = sol_mn_pro + sol_mn(k,j)
	sol_mp_pro = sol_mp_pro + sol_mp(k,j)
	wdn_pro = wdn_pro + wdn
	net_N_pro = net_N_pro + net_N
	solN_net_min_pro = solN_net_min_pro + solN_net_min

      end do
	
	!! writing daily profile output

        write (100,9002) iyr, i, j, cmass_pro, sol_rsd_pro, sol_mc_pro  


!9000  format(i4,';',i3,';',i1,';',i4,20(';',f10.3))
!9001  format(i4,';',i3,';',i1,';',i4,10(';',f10.3))
9002  format(i4,';',i3,';',i4,';',f11.3,';',f11.3,';',f11.3)

      return
      end subroutine

	
	!! LOCAL FUNCTIONS
	real*8 Function fwf(fc,wc,pwp)
          real*8, intent (in) :: fc, wc, pwp
		xx2 = 0.
		if (wc <= pwp) then
			xx2 = 0.4 * wc / pwp
		else if (wc <= fc) then
			xx2 = 0.4 + 0.6 * (wc - pwp)/(fc - pwp)
		else
			xx2 = 1.
		end if

        !!fwf = (1. + (1. - xx2) / (1. - 0.75)) * (xx2 / 1.) ** (1./ (1. - 0.75))    
        fwf = (1. + (1. - xx2) / 0.25) * (xx2) ** 4.
      End function

      real*8 Function fof(void,por)
        real*8, intent (in) :: void, por
        xx3 = 0.
        if (void >= 0.1) then
            xx3 = 0.2 + 0.8 * (void - 0.1) / (por - 0.1)
        else
            xx3 = 0.2 * void / 0.1
        end if
        fof = 0.5 + 0.5 * xx3 / (xx3 + Exp(-20. * xx3))
      End function

	
	real*8 Function fcgd(xx)
        real*8, intent (in) :: xx
		tn = -5.
	  top = 35.
		tx = 50.
		qq = (tn - top)/(top - tx)
	  fcgd = ((xx-tn)**qq)*(tx-xx)/(((top-tn)**qq)*(tx-top))
		if (fcgd < 0.) fcgd = 0.
	End function

      real*8 Function ftilf(tillage, wc, sat)
        real*8, intent (in out) :: tillage
        real*8, intent (in) :: wc, sat
        !! tillage factor effect on decomposition
        !! tillage factor returns to baseline (=1) based on WC 
        tillage = tillage * (1. - 0.02 * wc/sat) 
        if (tillage < 0.) tillage = 0.
        ftilf = 1. + tillage 
      End function
    

      real*8 Function fcx(pclay)
        real*8, intent (in) :: pclay
        !! saturated soil carbon concentration (%) from Hassink and Whitmore 1997
        fcx = 2.11 + 0.0375 * pclay
      End function


	real*8 Function fsol_cdec(pcarbon, cx, cfdec, tilf, csf, sol_cmass)
        real*8, intent (in) :: pcarbon, cx, cfdec, tilf, csf, sol_cmass
		!! decomposition adjustment by current SOC 
		decf = (pcarbon / cx) ** 0.5	
		! if (decf > 1.) decf = 1. 
		!! maximum soil carbon decomposition = 0.045 kg C / kg C per year
		fsol_cdec = cfdec / 365. * decf * tilf * csf * sol_cmass
	End function


      real*8 Function fCNnew(yy1,yy2,CNpool,yy5)
        real*8, intent(in) :: yy1,yy2,CNpool
        real*8, intent(in) :: yy5
      !! CN ratio of newly formed organic matter
      !! based on CN or decomposing residue and nitrate in soil
      !! the same approach used for crop residues and manure
      !! CNpool = the CN of decomposing pool
      !! yy1 = the layer nitrate mass
      !! yy2 = the layer soil mass
      !! yy3 = nitrate concentration g/g
      !! yy4 = correction factor based on CN of decomposing pool
      !! yy5 = input-dependent constant to correct CN ratio

        yy3 = yy1 / yy2
        yy4 = 5.5 * (1. - 1. / (1. + (CNpool / yy5)**3.))
        fCNnew = 8.5 + yy4 * (0.5 + 0.5 / (1. + (yy3 / 8e-6)**3.)) 
      End function


	real*8 Function fhc(pclay, pcarbon, cx) 		
        real*8, intent(in) :: pclay, pcarbon, cx
	!! maximum and actual humification factor 
	!! hx = maximum humification factor
	!! hf = humification adjustment factor 
	!! pclay = %clay
	!! pcarbon = %carbon
	!! cx = saturated soil carbon, %
	
	real*8 :: hx, hf
			 
		hx = 0.09 + 0.09 * (1. - Exp(-5.5 * pclay / 100.))
		!! humification adjustment by current SOC
		if (pcarbon > cx) then
			hf = 0.
		else
			hf = 1. - (pcarbon / cx) ** 6.
		end if
		fhc = hx * hf		  
	End function

	real*8 Function fnetmin(poold, R1, R2, hc, dummy, poolm, xinorg, cc1)
	!! This function computes net mineralization
	!! R1 = CN or CP ratio of decomposing pool
	!! R2 = CN or CP ratio of receiving pool
	!! hc = humification rate
	!! dummy = fraction of pool being decomposed
	!! poolm = current mass of pool being decomposed
	!! poold = mass of pool decomposed
	!! xinorg = mass of NO3 or P in solution
	!! xx = net mineralization of N or P
	!! cc1 = pool's carbon fraction
        real*8, intent(in) :: R1, R2, hc, poolm, xinorg, cc1
        real*8, intent(in out) :: poold, dummy

		xx = 0.
		xx = poold * cc1 * (1. / R1 - hc / R2) 
		
		if (xx > 0.) then
			dummy = poold / poolm
		else if (abs(xx)< xinorg) then 
			!! net mineralization is positive or
			!! immobilization not consuming all mineral N or P
			dummy = poold / poolm
		else
			!! immobilization, consuming all mineral N or P
			xx = -xinorg
			poold = xx / cc1 * 1. / (1. / R1 - hc / R2)
			dummy = poold / poolm
    		end if

        fnetmin = xx
      End function
