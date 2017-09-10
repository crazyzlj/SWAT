      subroutine lid_porpavement(sb,j,k,lid_prec)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Simulate porous pavement processes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sb               |none          |Subbasin number
!!    j                |none          |HRU number
!!    k                |none          |Subdaily time index
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
!!    lid_sw_last      |mm/mm H2O     |Soil water content of the amended soil layer at the last time step in a day
!!    lid_drcoef       |none          |Drainage coefficient (ratio of drainage rate to percolation rate)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    lid_qsurf        |mm H2O        |Depth of runoff generated on a LID in a given time interval
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jj               |none          |Urban land type identification number from
!!                                    |urban.dat
!!    dt               |hour          |Time interval in hours
!!    lid_qperc        |mm H2O        |Amount of soil water percolated through the amended soil layer
!!    lid_cumr         |mm H2O        |Cumulative amount of rainfall a LID receives in a time interval
!!    lid_exinc        |mm H2O        |Amount of excess rainfall
!!    lid_re_sw        |none          |Relative soil water content of the amended soil layer considering its porosity and wilting point
!!    lid_adj_ksat     |mm/hr         |Adjusted saturated hydraulic conductivity of the amended soil layer
!!    lid_et           |mm H2O        |Amount of soil water evapotranspirated into the air in a time interval
!!    lid_sw           |mm/mm H2O     |Soil water content of the amended soil layer at a time step
!!    lid_f            |mm/hr         |Potential infiltration rate at a time step
!!    lid_cumf         |mm H2O        |Cumulative amount of water infiltrated into the amended soil layer at a time step
!!    lid_qinf         |mm H2O        |Amount of rainfall infiltrated into the amended soil layer in a time interval
!!    lid_qdrain       |mm H2O        |Amount of rainfall going out of the drainage pipe in a time interval
!!    whd              |mm/mm H2O     |Wetting front suction head
!!    tst              |mm H2O        |A temporary variable (cumulative amount of infiltrated water) for the interative calculation for solving the Green-Ampt equation
!!    cvwc             |mm/mm H2O     |Change in volumetric water content across the wetting front
!!    lid_usat_ratio   |none          |Anisotropic ratio
!!    lid_hydeff       |none          |Hydraulic efficiency factor (considering clogging up and anisotropy ratio)
!!    lid_f1           |mm H2O        |Potential amount of water that can be infiltrated into the amended soil layer at a time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    None
!!    Intrinsic: Log

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use parm
      implicit none
      
      integer :: jj,sb,j,k
      real*8 :: lid_ksat,lid_por,lid_fc,lid_wp,lid_wetfsh,whd,
     & tst,cvwc,lid_usat_ratio,lid_qinf,lid_perc,lid_qperc,lid_qdrain,
     & dt,lid_re_sw,lid_adj_ksat,lid_sw,lid_f,lid_cuminf,lid_soldpt,
     & lid_f1,lid_str,lid_vinf,lid_str_depth,
     & lid_qbypass,lid_bypass,lid_cumqperc,lid_vol,lid_vdrain
      real :: lid_prec,lid_cumr,lid_drcoef,lid_hydeff
      
      jj = urblu(j)
      
      lid_ksat = pv_ksat(sb,jj)
      lid_por = pv_por(sb,jj)
      lid_fc = pv_fc(sb,jj)
      lid_wp = pv_wp(sb,jj)
      lid_soldpt = pv_soldpt(sb,jj)
      lid_drcoef = pv_drcoef(sb,jj)
      lid_vol = pv_grvdep(sb,jj) / 1000. * pv_grvpor(sb,jj) * 
     & (lid_farea(j,4) * fcimp(urblu(j)) * hru_ha(j) * 10000.)
      lid_hydeff = pv_hydeff(sb,jj)
     
      dt = real(idt) / 60.
      
!!    Initialize parameters and coefficients for green roof modeling
      lid_sw = lid_sw_last(j,4)
      lid_f = lid_f_last(j,4)
      lid_cuminf = lid_cuminf_last(j,4)
      lid_cumr = lid_cumr_last(j,4)
      lid_str = lid_str_last(j,4)
      lid_cumqperc = lid_cumqperc_last(j,4)
      
      lid_str = lid_str + (lid_prec / 1000.) *
     & (lid_farea(j,4) * fcimp(urblu(j)) * hru_ha(j) * 10000.)

      lid_qbypass = lid_str - lid_vol
      if (lid_qbypass > 0.) lid_str = lid_vol
      if (lid_qbypass < 0.) lid_qbypass = 0.
      lid_bypass = lid_qbypass /
     & (hru_ha(j) * 10000.) * 1000.
!     & (lid_farea(j,4) * fcimp(urblu(j)) * hru_ha(j) * 10000.) * 1000.

      lid_str_depth = lid_str / (lid_farea(j,4) * fcimp(urblu(j)) * 
     & hru_ha(j) * 10000.) * 1000.
      
      if (lid_str_depth > 0.) Then
!      if (lid_prec > 0) Then
      
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
          lid_f1 = lid_cuminf_last(j,4) + lid_adj_ksat * dt
     &    + whd * cvwc * Log((tst + whd * cvwc) /
     &    (lid_cuminf_last(j,4) + whd * cvwc))
          if (Abs(lid_f1 - tst) < 0.001) then
            lid_f = lid_adj_ksat * (1 + (whd * cvwc)/lid_f1)
            lid_qinf = lid_f * dt ! (mm)
            if (lid_qinf > lid_str_depth) then
              lid_cuminf = lid_cuminf_last(j,4) + lid_str_depth
              lid_qinf = lid_str_depth
            else
              lid_cuminf = lid_cuminf_last(j,4) + lid_qinf
            end if
            exit
          else
            tst = lid_f1
          end if
        end do
!      else
        if (k == nstep+1) then
!          lid_sw_add(j,4) = lid_cuminf
          lid_cumr = 0
          lid_cuminf = 0
        else
          lid_cumr = lid_cumr_last(j,4)
          lid_cuminf = lid_cuminf_last(j,4)
        end if
      else
        if (k == nstep+1) then
!          lid_sw_add(j,4) = lid_cuminf
          lid_cumr = 0
          lid_cuminf = 0
        else
          lid_cumr = lid_cumr_last(j,4)
          lid_cuminf = lid_cuminf_last(j,4)
        end if
        lid_re_sw = (lid_sw - lid_wp)/(lid_por - lid_wp)
        lid_usat_ratio = (lid_re_sw**lid_vgcl) * ((1-(1-lid_re_sw
     &  **(lid_vgcl/lid_vgcm))**lid_vgcm)**2)
        lid_f = 0.
        lid_qinf = 0.
      end if

      lid_vinf = lid_qinf / 1000.0 * 
     & (lid_farea(j,4) * fcimp(urblu(j)) * hru_ha(j) * 10000.)
        
!!    Amount of water percolated out of the amended soil layer into the native HRU soil (mm)
      lid_perc = lid_ksat * lid_usat_ratio * lid_hydeff
      lid_qperc = lid_perc * dt
      lid_cumqperc = lid_cumqperc + (1-lid_drcoef)*lid_qperc
      if (k == nstep+1) then
          lid_sw_add(j,4) = lid_cumqperc
          lid_cumqperc = 0.
      end if
      
!!    Amount of water drained out of the drainage pipe (mm)
      lid_qdrain = lid_drcoef * lid_qperc
      lid_vdrain = lid_qdrain / 1000.0 *
     & (lid_farea(j,4) * fcimp(urblu(j)) * hru_ha(j) * 10000.)
     
!!    Update soil water content of the amended soil layer (mm)
      lid_sw = lid_sw_last(j,4) + (lid_qinf - lid_qperc - lid_qdrain) / 
     & (lid_soldpt * 1000)
      if (lid_sw < lid_wp) lid_sw = lid_wp
      if (lid_sw > lid_por) lid_sw = lid_por
                
!!    Amount of water that becomes surface runoff (mm)
      lid_qsurf(j,4) = lid_bypass
      
!! begin temporary
!      if (k == nstep+1) then
!      if (sb == 1) then
!        if (jj == 14) then
!        if (j == 1) then
!          write (4444444,'(10f12.4)') lid_prec,lid_qinf,lid_str_depth,
!     & lid_qperc,lid_qdrain,lid_sw,lid_bypass,lid_qsurf(j,4)
!        end if
!      end if
!      end if
!! end temporary
             
!!    Update the water storage
      lid_str = lid_str - lid_vinf - lid_vdrain
      if (lid_str < 0) lid_str = 0.
      
      lid_sw_last(j,4) = lid_sw
      lid_cumr_last(j,4) = lid_cumr
      lid_cuminf_last(j,4) = lid_cuminf
      lid_f_last(j,4) = lid_f
      lid_str_last(j,4) = lid_str
      lid_cumqperc_last(j,4) = lid_cumqperc
      
      return      
      end
      