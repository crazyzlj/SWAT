      subroutine lid_raingarden(sb,j,k,kk,lid_prec)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Simulate rain garden processes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sb               |none          |Subbasin number
!!    j                |none          |HRU number
!!    k                |none          |Subdaily time index
!!    kk               |none          |LID index in *.lid files
!!    lid_prec         |mm            |Precipitation depth a LID receives in a simulation time interval
!!    idt              |minutes       |Simulation time interval for sub-daily modeling
!!    ihru             |none          |HRU number
!!    nstep            |none          |Number of time intervals for a day
!!    urblu(:)         |none          |Urban land type identification number from urban.dat
!!    lid_ksat         |mm/hr         |Saturated hydraulic conductivity of the amended soil layer
!!    lid_por          |none          |Porosity of the amended soil layer expressed as a fraction of the total volume
!!    lid_fc           |mm/mm H2O     |Amount of water held in the amended soil layer at the field capacity, expressed as a fraction of the total volume
!!    lid_wp           |mm/mm H2O     |Amount of water held in the amended soil layer at the wilting point, expressed as a fraction of the total volume
!!    lid_soldpt       |m             |Depth of the amended soil layer
!!    lid_etcoef       |none          |Evapotranspiration ceofficient
!!    lid_vgcl         |none          |van Genuchten equation's coefficient, l
!!    lid_vgcm         |none          |van Genuchten equation's coefficient, m
!!    lid_cumr_last    |mm H2O        |Cumulative amount of rainfall at the last time step in a day
!!    lid_cuminf_last  |mm H2O        |Cumulative amount of water infiltrated into the amended soil layer at the last time step in a day
!!    lid_f_last       |mm/mm H2O     |Potential infiltration rate of the amended soil layer at the last time step in a day
!!    Cumulative amount of rainfall a LID receives in a time interval_last   |mm H2O        |Cumulative amount of excess rainfall at the last time step in a day
!!    lid_sw_last      |mm/mm H2O     |Soil water content of the amended soil layer at the last time step in a day
!!    lid_pet          |mm            |Amount of potential evapotranspiration in a time interval
!!    lid_oheight      |m             |Height of the bottom of an orifice pipe from the rain garden soil surface
!!    lid_odia         |m             |Diameter of the orifice pipe
!!    lid_vol          |m3            |Volume of the rain garden storage
!!    lid_barea        |m2            |Bottom area of the rain garden storage
!!    lid_sarea        |m2            |Surface area of the rain garden storage
!!    plen             |m             |Length of the orifice pipe
!!    mann             |m1/3          |Manning's coefficient
!!    mloss            |none          |Minor loss coefficient
!!    coeff_C          |none          |Discharge coefficient
!!    pipe_slp         |none          |Slope of the orifice pipe
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    lid_qsurf        |mm H2O        |Depth of runoff generated on a LID in a given time interval
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dt               |hour          |Time interval in hours
!!    lid_perc         |mm H2O        |Amount of soil water percolated through the amended soil layer
!!    lid_cumr         |mm H2O        |Cumulative amount of rainfall a LID receives in a time interval
!!    Cumulative amount of rainfall a LID receives in a time interval        |mm H2O        |Cumulative amount of excess rainfall at a time step in a day
!!    lid_re_sw        |none          |Relative soil water content of the amended soil layer considering its porosity and wilting point
!!    lid_adj_ksat     |mm/hr         |Adjusted saturated hydraulic conductivity of the amended soil layer
!!    lid_et           |mm H2O        |Amount of soil water evapotranspirated into the air in a time interval
!!    lid_vet          |m3 H2O        |Volume of soil water evapotranspirated into the air in a time interval
!!    lid_pet          |m3 H2O        |Volume of soil water evapotranspirated into the air at the potential evapotranspiration rate in a time interval
!!    lid_sw           |mm/mm H2O     |Soil water content of the amended soil layer at a time step
!!    lid_f            |mm/hr         |Potential infiltration rate at a time step
!!    lid_cumf         |mm H2O        |Cumulative amount of water infiltrated into the amended soil layer at a time step
!!    lid_qinf         |mm H2O        |Amount of rainfall infiltrated into the amended soil layer in a time interval
!!    lid_vinf
!!    lid_qseep        |mm H2O        |Amount of rainfall seeped out of the amended soil layer in a time interval
!!    whd              |mm/mm H2O     |Wetting front suction head
!!    tst              |mm H2O        |A temporary variable (cumulative amount of infiltrated water) for the interative calculation for solving the Green-Ampt equation
!!    cvwc             |mm/mm H2O     |Change in volumetric water content across the wetting front
!!    lid_usat_ratio   |none          |Anisotropic ratio
!!    lid_hydeff       |none          |Hydraulic efficiency factor (considering clogging up and anisotropy ratio)
!!    lid_f1           |mm H2O        |Potential amount of water that can be infiltrated into the amended soil layer at a time step
!!    lid_str          |m3            |Volume of stored water in the rain garden storage
!!    lid_str_depth    |m             |Depth of stored water in the rain garden storage
!!    lid_odepth       |mm            |Depth of water discharging through an orifice
!!    lid_vorifice     |m3            |Volume of water discharging through the orifice
!!    lid_qbypass      |m3            |Volume of water bypassinig the raingarden
!!    lid_bypass       |mm H2O        |Depth of water bypassinig the raingarden
!!    lid_qorifice     |cms           |Discharge rate of the orifice flow
!!    lid_dorifice     |mm            |Depth of water discharging through the orifice
!!    lid_ostr         |m3            |Volume of water stored below the orifice bottom
!!    parea            |ft2           |Crossectional area of flow discharging through the orifice pipe (partially filled pipe)
!!    rh               |              |Intermediate variable for orifice discharge calculation
!!    kf               |              |Intermediate variable for orifice discharge calculation
!!    hdep_ratio       |none          |Fraction of the depth of water discharging through the orifice to the crossectional area of the orifice pipe
!!    n_partial        |              |Intermediate variable for orifice discharge calculation
!!    mann_n           |m1/3          |Manning's coefficient for partially filled flow in the orifice pipe
!!    prad_ft          |              |Intermediate variable for orifice discharge calculation
!!    hdep_ft          |              |Intermediate variable for orifice discharge calculation
!!    cangle           |              |Intermediate variable for orifice discharge calculation
!!    carea_ft2        |              |Intermediate variable for orifice discharge calculation
!!    wperi_ft         |              |Intermediate variable for orifice discharge calculation
!!    hyd_rad_ft       |              |Intermediate variable for orifice discharge calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    None

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use parm
      implicit none
      
      integer :: sb,j,k,kk
      real*8 :: lid_ksat,lid_por,lid_fc,lid_wp,lid_wetfsh,whd,
     & tst,cvwc,lid_usat_ratio,lid_qinf,lid_perc,lid_qperc,dt,
     & lid_re_sw,lid_adj_ksat,lid_sw,lid_f,lid_cuminf,lid_soldpt,
     & lid_f1,lid_str,lid_vinf,lid_str_depth,lid_vet,
     & lid_vorifice,lid_qbypass,lid_bypass,lid_pet,lid_cumqperc,
     & lid_vpet,lid_odepth,lid_vol,lid_oheight,lid_odia,lid_qorifice,
     & lid_dorifice,lid_ostr
 !     real*8 :: lid_prec,lid_cumr,Cumulative amount of rainfall a LID receives in a time interval,lid_et,lid_etcoef,lid_barea,
       real*8 :: lid_prec,lid_cumr,lid_et,lid_etcoef,lid_barea,
     & lid_sarea,plen,mann,mloss,coeff_C,pipe_slp,parea,rh,kf,
     & hdep_ratio,n_partial,mann_n,prad_ft,hdep_ft,cangle,carea_ft2,
     & wperi_ft,hyd_rad_ft,lid_hydeff
      
      lid_ksat = rg_ksat(sb,kk)
      lid_por = rg_por(sb,kk)
      lid_fc = rg_fc(sb,kk)
      lid_wp = rg_wp(sb,kk)
      lid_soldpt = rg_soldpt(sb,kk)
      lid_etcoef = rg_etcoef(sb,kk)
      lid_sarea = lid_farea(j,2) * fcimp(urblu(j)) *
     & hru_ha(j) * 10000. * rg_sarea(sb,kk) !m2
      lid_barea = lid_farea(j,2) * fcimp(urblu(j)) * 
     & hru_ha(j) * 10000. * rg_sarea(sb,kk)
      lid_vol = rg_sth(sb,kk) * lid_sarea
      lid_oheight = rg_oheight(sb,kk) * 1000.
      lid_odia = rg_odia(sb,kk)
      lid_ostr = rg_oheight(sb,kk) * lid_barea
      lid_hydeff = rg_hydeff(sb,kk)
      
      dt = dfloat(idt) / 60.
      
!!    Initialize parameters and coefficients for rain garden modeling
      lid_sw = lid_sw_last(j,2)
      lid_f = lid_f_last(j,2)
      lid_cuminf = lid_cuminf_last(j,2)
      lid_cumr = lid_cumr_last(j,2)
      lid_str = lid_str_last(j,2)
      lid_cumqperc = lid_cumqperc_last(j,2)

      lid_str = lid_str + (lid_prec / 1000.) *
     & (lid_farea(j,2) * fcimp(urblu(j)) * hru_ha(j) * 10000.)

      lid_qbypass = lid_str - lid_vol
      if (lid_qbypass > 0.) lid_str = lid_vol
      if (lid_qbypass < 0.) lid_qbypass = 0.
      lid_bypass = lid_qbypass / (hru_ha(j) * 10.) 
!     & (lid_farea(j,2) * fcimp(urblu(j)) * hru_ha(j) * 10000.) * 1000.

      lid_str_depth = lid_str / ((lid_sarea + lid_barea)/2) * 1000.
      
      if (lid_str_depth > 0.) Then
!      if (lid_prec > 0.) Then

        lid_cumr = lid_cumr + lid_prec
        whd = wfsh(j) + lid_str_depth
        lid_adj_ksat = (56.82 * lid_ksat ** 0.286)          
     &  / (1. + 0.051 * Exp(0.062 * cnday(j))) - 2.
        if (lid_adj_ksat <= 0.) lid_adj_ksat = 0.001
        lid_re_sw = (lid_sw - lid_wp)/(lid_por - lid_wp)
        lid_usat_ratio = (lid_re_sw**lid_vgcl) * ((1-(1-lid_re_sw
     &  **(lid_vgcl/lid_vgcm))**lid_vgcm)**2)
        cvwc = (1 - (lid_sw-lid_wp)/lid_fc)*(0.95*lid_por)
        if (cvwc < 0) cvwc = 0.001*0.95*lid_por
        tst = lid_adj_ksat * dt
        do
          lid_f1 = 0.
          lid_f1 = lid_cuminf_last(j,2) + lid_adj_ksat * dt
     &    + whd * cvwc * Log((tst + whd * cvwc) /
     &    (lid_cuminf_last(j,2) + whd * cvwc))
          if (abs(lid_f1 - tst) < 0.001) then
            lid_f = lid_adj_ksat * (1 + (whd * cvwc)/lid_f1)
            lid_qinf = lid_f * dt
            if (lid_qinf > lid_str_depth) then
              lid_cuminf = lid_cuminf_last(j,2) + lid_str_depth
              lid_qinf = lid_str_depth
            else
              lid_cuminf = lid_cuminf_last(j,2) + lid_qinf
            end if
            exit
          else
            tst = lid_f1
          end if
        end do
!     else
        if (k == nstep+1) then
!          lid_sw_add(j,2) = lid_cuminf
          lid_cumr = 0
          lid_cuminf = 0
        else
          lid_cumr = lid_cumr_last(j,2)
          lid_cuminf = lid_cuminf_last(j,2)
        end if
!        lid_qinf = 0.
!     end if
      else
        if (k == nstep+1) then
!          lid_sw_add(j,2) = lid_cuminf
          lid_cumr = 0
          lid_cuminf = 0
        else
          lid_cumr = lid_cumr_last(j,2)
          lid_cuminf = lid_cuminf_last(j,2)
        end if
        lid_re_sw = max(0.,(lid_sw - lid_wp)/(lid_por - lid_wp))
        lid_usat_ratio = (lid_re_sw**lid_vgcl) * ((1-(1-lid_re_sw
     &  **(lid_vgcl/lid_vgcm))**lid_vgcm)**2)
        lid_f = 0.
        lid_qinf = 0.
      end if
        
      lid_vinf = lid_qinf / 1000.0 * lid_barea
      
!!    Amount of water going out through an orifice (mm)
      plen = 5.0 ! m
      mann = 0.012 ! concrete surface
      mloss = 0.1 ! minor loss
      coeff_C = 0.611
      pipe_slp = 0.0005
      lid_qorifice = 0.
      if (rg_orifice(sb,kk)==1) then !there exists an orifice pipe for drainage
	if (lid_str_depth > lid_oheight) then
        lid_odepth = lid_str_depth - lid_oheight
        if (lid_odepth >= 1.0 * lid_odia) then
          parea = 3.14159 * (3.2808 * lid_odia) ** 2 / 4. !ft^2
          rh = 3.2808 * lid_odia / 4. !ft
          kf = 29. * plen * mann ** 2 / rh ** 1.33
          lid_qorifice = parea * ((64.4 * 3.2808 * lid_odepth) /
     &    (1.+ kf + mloss)) ** 0.5 !cfs
          lid_qorifice = lid_qorifice / 35.31 ! m3/s
        else
          hdep_ratio = lid_odepth / lid_odia
          if (lid_odepth < 0.5 * lid_odia) then
            if (hdep_ratio <= 0.03) then
              n_partial = 1.0 + hdep_ratio / 0.3
            else if (0.03 < hdep_ratio .and. hdep_ratio <= 0.10) then
              n_partial = 1.1 + (hdep_ratio - 0.03) * 12 / 7
            else if (0.10 < hdep_ratio .and. hdep_ratio <= 0.20) then
              n_partial = 1.22 + (hdep_ratio - 0.1) * 0.6
            else if (0.20 < hdep_ratio .and. hdep_ratio <= 0.30) then
              n_partial = 1.29
            else
              n_partial = 1.29 - (hdep_ratio - 0.3) * 0.2
            end if
            mann_n = mann * n_partial
            prad_ft = lid_odia / 2 * 3.2808 ! ft
            hdep_ft = lid_odepth * 3.2808 ! ft
            cangle = 2 * ACOS((prad_ft - hdep_ft)/prad_ft)
            carea_ft2 = prad_ft ** 2 * (cangle - SIN(cangle)) / 2 ! ft2
            wperi_ft = prad_ft * cangle ! ft
            hyd_rad_ft = carea_ft2 / wperi_ft !ft
            lid_qorifice = 1.49 / mann_n * carea_ft2 * pipe_slp ** 0.5
     & * (hyd_rad_ft ** 2) ** 0.33333 ! cfs
          else
            n_partial = 1.25 - (hdep_ratio - 0.5) * 0.5
            mann_n = mann * n_partial
            prad_ft = lid_odia / 2 * 3.2808 ! ft
            hdep_ft = 2 * prad_ft - lid_odepth * 3.2808 ! ft
            cangle = 2 * ACos((prad_ft - hdep_ft)/prad_ft)
            carea_ft2 = 3.14159 * prad_ft ** 2 - prad_ft ** 2 * 
     & (cangle - Sin(cangle)) / 2 ! ft2
            wperi_ft = 2 * 3.14159 * prad_ft - prad_ft * cangle ! ft
            hyd_rad_ft = carea_ft2 / wperi_ft !ft
            lid_qorifice = 1.49 / mann_n * carea_ft2 * pipe_slp ** 0.5
     & * (hyd_rad_ft ** 2) ** 0.33333 ! cfs
            lid_qorifice = lid_qorifice / 35.31 ! m3/s
          end if
        end if
        lid_vorifice = lid_qorifice * dfloat(idt) * 60 ! m3
      else
        lid_vorifice = 0.
	end if
	
	else
		lid_vorifice=0.
      end if
      if (lid_vorifice > (lid_str - lid_ostr)) lid_vorifice = 
     & lid_str - lid_ostr
      if (lid_vorifice < 0.) lid_vorifice = 0.
      lid_dorifice = lid_vorifice / (hru_ha(j) * 10.)  ! mm
      
!!    Amount of water seeped out of the amended soil layer (mm)
      lid_perc = lid_ksat * lid_usat_ratio * lid_hydeff
      lid_qperc = lid_perc * dt ! convert mm/hr to mm
      lid_cumqperc = lid_cumqperc + lid_qperc
      if (k == nstep+1) then
          lid_sw_add(j,2) = lid_cumqperc * lid_farea(j,2) 
     &		* fcimp(urblu(j)) !mm normalized by HRU's area
          lid_cumqperc = 0.
      end if
      
!!    Amount of soil water evapotranspirated into the air (mm)
      call etpot
      lid_et = lid_etcoef * pet_day / 1440. * dfloat(idt)
      lid_vet = lid_et / 1000.0 * lid_sarea ! m3
      lid_pet = pet_day / 1440. * dfloat(idt)
      lid_vpet = lid_pet / 1000.0 * lid_sarea ! m3

!!    Update soil water content of the amended soil layer (mm)
      if (lid_str_depth < 0.1) then
        lid_sw = lid_sw_last(j,2) + (lid_qinf - lid_qperc - lid_et) /
     &  (lid_soldpt * 1000)
      else
        lid_sw = lid_sw_last(j,2) + (lid_qinf - lid_qperc) /
     &  (lid_soldpt * 1000)
      end if
      if (lid_sw < lid_wp) lid_sw = lid_wp
      if (lid_sw > lid_fc) lid_sw = lid_fc
                
!!    Amount of water that becomes surface runoff (mm)
      lid_qsurf(j,2) = lid_bypass + lid_dorifice
 	
     
!!    Update the water storage
      if (lid_str_depth < 0.1) then
          lid_str = lid_str - lid_vinf - lid_vorifice
      else
          lid_str = lid_str - lid_vinf - lid_vorifice - lid_vpet
      end if
      if (lid_str < 0) lid_str = 0.

	lid_str_curday(j,2) = lid_str_curday(j,2) !cumulative water volume stored today, m3
     &   + max(0.,lid_str - lid_str_last(j,2))
    
!!    Assign the calculated soil water content of the amended soil layer, accumulated infiltration depth
      lid_sw_last(j,2) = lid_sw
      lid_cumr_last(j,2) = lid_cumr
      lid_cuminf_last(j,2) = lid_cuminf
      lid_f_last(j,2) = lid_f
      lid_str_last(j,2) = lid_str
      lid_cumqperc_last(j,2) = lid_cumqperc
	lid_qsurf_curday(j,2) = lid_qsurf_curday(j,2)+lid_qsurf(j,2)
        
      return
      end subroutine