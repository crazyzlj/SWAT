      subroutine sand_filter(kk,flw,sed)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes water and sediment through sand filters in the subbasin

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |subbasin in which HRU/reach is located
!!    i_mo        |none          |current month of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pnd_sed(:)  |kg/L          |ratio of sediment to water in pond
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sb          |none          |subbasin or reach number
!!    kk          |none          |filter id number in the subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: pipeflow coded in bmp_sed_pond.f90

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none
     
      integer :: sb, ii
      integer, intent(in) :: kk
      real*8 :: tsa,ffsa,vfiltr,mxvol,pdia,ksat,por,dp,dc,pden,alp,
     & wetfsh,whd,sub_ha,dt,qcms,effct,effl,effg,effbr,vpipe,phead,hpnd,
     & tmpw,qloss,fsat,qpipe,mu,pipeflow,splw,hweir,tst,kb,qintns,qq, 
     & qfiltr,sloss,spndconc,sedpnd,qpndi,qpnde,sedrmeff,sed_removed,
     & sedconc
      real*8, dimension(:) :: qpnd(0:nstep),qsw(0:nstep),qin(0:nstep),
     & qout(0:nstep),fc(0:nstep),f(0:nstep)
      real, dimension(2,nstep), intent(inout) :: flw, sed
      
      sb = inum1
      sub_ha = da_ha * sub_fr(sb)
      dt = real(idt) / 60. !time interval in hours 
      qin = 0.; qout = 0.
      flw(2,:) = 0.; sed(2,:) = 0.
      qpnd = 0.; qsw = 0.
      kb = 1.38e-16 !Boltzmann constant, g-cm^2/s^2-K

      !! Initialize parameters, coefficients, etc
      tsa = ft_sa(sb,kk)     !total surface area of filter (m^2)
      ffsa = ft_fsa(sb,kk)     !fraction of infiltration bed in the filtration basin (m2/m2)
      mxvol = ft_h(sb,kk) / 1000. * tsa    !max. capacity of the basin (m^3)
      pdia = ft_pd(sb,kk)       !outflow orifice pipe diameter (mm)
      splw = ft_bpw(sb,kk)       !spillway overflow weir width (m)
      ksat = ft_k(sb,kk)      !saturated hydraulic conductivity (mm/hr)
      por = ft_por(sb,kk)     !filter porosity
      dp = ft_dp(sb,kk) / 10.      !median diameter of TSS particle (cm)
      dc = ft_dc(sb,kk) / 10.      !median diameter of filter media (cm)
      pden = tss_den(sb,kk)     !density of tss particle (g/cm3)
      alp = ft_alp(sb,kk)     !filter attachment efficiency (0-1)      
      vfiltr = ft_dep(sb,kk) / 1000. * tsa * ffsa * por   !actual volume of filter column (m^3)
                                     
      !! wetting front suction head (mm)
      wetfsh = 10. * Exp(6.5309 - 7.32561 * por + 3.809479 * por ** 2 - 
     & 0.049837 * por * 100. + 0.001608 * por ** 2 * 100. ** 2 - 
     & 0.000799 * 100. ** 2 * por)

      !! Get initial values from previous day
      qpnd(0) = ft_qpnd(sb,kk)
      qsw(0) = ft_qsw(sb,kk)
      qin(0) = ft_qin(sb,kk)
      qout(0) = ft_qout(sb,kk)
      sedpnd = ft_sedpnd(sb,kk)
      fc(0) = ft_fc(sb,kk)
      
      do ii=1,nstep
         qloss = 0.

         qin(ii) = flw(1,ii) * 10. * (sub_ha - tsa / 10000.) + 
     &             precipdt(ii) * tsa / 1000.  !m^3
         qout(ii) = qout(ii-1)
        
         If (qin(ii)<0.001.and.qpnd(ii-1)<0.001)then
           
           if (qsw(ii-1)<0.001) then
             !No flow
             flw(2,ii) = 0.
           else
             qout(ii) = ksat * dt * qsw(ii-1) / vfiltr / 1000.* tsa 
     &          * ffsa !m^3
             qsw(ii)= max(0.,qsw(ii-1) - qout(ii)) ! m^3
             flw(2,ii) = qout(ii)  / (sub_ha *10000. - tsa) * 1000.  !mm
           endif
        
         Else
           qpnd(ii) = qpnd(ii-1) + qin(ii) !m^3
           hpnd = qpnd(ii) / tsa * 1000. !ponding depth on the filter surface, mm
 
           !spillway overflow
           If (hpnd > ft_h(sb,kk)) Then
              qloss = max(0.,qpnd(ii) - mxvol) !weir outflow
              hpnd = ft_h(sb,kk)
              qpnd(ii) = max(0.,qpnd(ii) - qloss) 
           End If
           qpndi = qpnd(ii) + qsw(ii-1)
            
           ! estimate unsaturated filter flow 
           if(qsw(ii-1)<0.99*vfiltr) then
            
             whd = (wetfsh + hpnd) 
             tst = ksat
             Do  !green and ampt infiltration
               fc(ii) = fc(ii - 1) + ksat * dt + whd * Log((tst + whd) /
     &                 (fc(ii - 1) + whd))
               If (Abs(fc(ii) - tst) < 0.0001) Then
                 Exit 
               Else
                 tst = fc(ii)
               End If
             End do
          
             !infiltration rate
             f(ii) = ksat * (1 + whd / fc(ii)) !mm/hr
            
             !water infiltrated, m^3
             qfiltr = f(ii) * dt / 1000. * tsa * ffsa 
            
             !infiltration limited by the total available water
             If (qfiltr > qpnd(ii)) then
               qfiltr = qpnd(ii)
               qpnd(ii) = 0.
             else
               qpnd(ii) = qpnd(ii) - qfiltr
             endif
             hpnd = qpnd(ii) / tsa * 1000. !mm
             
             !update soil water
             qsw(ii) = qsw(ii-1) + qfiltr
            
             !soil water no more than saturation
             if (qsw(ii) > vfiltr) then
               qout(ii) = ksat * dt / 1000. * tsa * ffsa  !m3
               qsw(ii) = vfiltr
               qpnd(ii) = qpndi - qsw(ii) - qout(ii) 
             else
               if (qpnd(ii)>=qpnd(ii-1).and.qout(ii-1)<0.001) then
                 !rising hydrograph, no outflow
                 qout(ii) = 0.
               else
                 !receding or continuing hydrograph
                 qout(ii) = ksat * qsw(ii) / vfiltr * dt / 1000. 
     &                      * tsa * ffsa  !m3
                 if (qout(ii)>qout(ii-1)) qout(ii) = qout(ii-1)
                 qsw(ii) = qsw(ii) - qout(ii)
               endif
             endif
                         
           else
            
             !darcy flow when saturated
             qout(ii) = ksat * (hpnd + ft_dep(sb,kk)) / 
     &                  ft_dep(sb,kk) * dt / 1000. * tsa * ffsa 
             qpnd(ii) = qpnd(ii) - qout(ii)
             
             if(qpnd(ii)<0) then
               qsw(ii) = vfiltr + qpnd(ii)
               qpnd(ii) = 0.
             else
               qsw(ii) = vfiltr
             endif
           end if
               
                       
           !check if orifice pipe limits outflow in case outflow control exists
            if (sf_ptp(sb,kk)==1) then
               phead = (qsw(ii)/(tsa*ffsa)/por + qpnd(ii)/tsa) * 
     &           1000. - pdia / 2. !mm
               If (phead>pdia/2.) then
                  qpipe = pipeflow(pdia,phead) * dt *3600. !m^3 
               else
                  qpipe = qout(ii) * 2. !pipe flow does not affect outflow
               endif

               !recalculate water balance if orifice pipe limits outflow
               if(qout(ii) > qpipe) then
                  qout(ii) = qpipe
                  qsw(ii)= max(0.,qsw(ii - 1) + qfiltr - qout(ii)) ! m^3
                  If (qsw(ii) > vfiltr) qsw(ii) = vfiltr   
                  qfiltr = max(0.,qsw(ii) - qsw(ii-1) + qout(ii))
                  qpnd(ii) = max(0.,qpnd(ii - 1) + qin(ii) - qfiltr) !m^3
                  qloss = max(0.,qpnd(ii) - mxvol)
                  qpnd(ii) = max(0.,qpnd(ii) - qloss)
               endif
            end if
            qpnde = qpnd(ii) + qsw(ii)
            
            !effluent from the filter unit (through-flow+overflow), normalized to subbasin area
            flw(2,ii) = (qout(ii) + qloss) / (sub_ha *10000. - tsa) *
     &        1000.  !mm
         Endif
        
         !--------------------------------------------------------------------------------------
         ! TSS removal 
         sloss = 0.; sedrmeff = 0.
         
         ! sediment bypass in spillway overflow
         if (qloss>0) then
            if(qin(ii)>0) then
                sedconc = sed(1,ii) / qin(ii) !tons/m3
            else
                sedconc = sedpnd / qpnd(ii) !tons/m3
            endif 
            sloss = sedconc * qloss !tons
            sedpnd = sedpnd + sed(1,ii) - sloss !tons
         end if
         
         if (qpndi>0.001) then
            spndconc = sedpnd / qpndi ! tons/m^3
         else
            spndconc = 0.
         end if
         
         If (qout(ii)<0.001)then
            ! no outflow through filter media
            if (qloss>0.001) then
               sed(2,ii) = sloss
            else
               sed(2,ii) = 0.
            end if
         Else
            ! water temperature, C
            tmpw = sub_hhwtmp(sb,ii)  
            ! water viscosity (g/cm-s) using 3rd order polynomial interpolation
	         mu = -3.e-6 * tmpw ** 3 + 0.0006 * tmpw ** 2 - 
     &	         0.0469 * tmpw + 1.7517		
	         mu = mu * 1.e-2

            !filter flow, cm/s
            qcms = qout(ii) / (tsa * ffsa) * 100. / dt / 3600. 

            If (qcms > 0.001) Then
               !sedimentation efficiency
               effg = (pden - 1) * 981 * dp ** 2 / (18 * mu * qcms)
               if (effg<0.001) effg = 0.001
               !brownian motion efficiency
               effbr = 0.905 * (kb * (tmpw+273.) / (mu * dp * dc * 
     &          qcms)) ** 0.6667
            Else
               effg = 0.999
               effbr = 0.999
            End If

            !interception efficiency
            effl = 1.5 * (dp / dc) ** 2
               
            If (effl > 0.999) effl = 0.99
            If (effg > 0.999) effg = 0.99
            If (effbr > 0.999) effbr = 0.99
           
            !contact efficiency
            effct = effl + effg + effbr
            If (effct > 0.999) effct = 0.99
           
            ! sediment removal efficiency
            sedrmeff = 1. - Exp(-1.5 * (1. - por) * alp * effct * 
     &       ft_dep(sb,kk) / ft_dc(sb,kk))
            ! sediment removed during the time step
            sed_removed =  spndconc * qout(ii) * sedrmeff
            ! sediment through filter, tons
            sed(2,ii) = spndconc * qout(ii) * (1. - sedrmeff)
            
            ! add bypass sediment
            sed(2,ii) = sed(2,ii) + sloss
            sedpnd = sedpnd - spndconc * qout(ii) !tons
            if (sedpnd<0) sedpnd = 0.
            
            ! write cumulative amount of sediment removed
            ft_sed_cumul(sb,kk) = ft_sed_cumul(sb,kk) + sed_removed !tons
         End if
      write(*,'(3i5,10f7.3)') iyr,iida,ii,precipdt(ii),qin(ii),qout(ii),
     & qloss,qpndi,qpnde,qpnd(ii),qsw(ii),f(ii)
!      write(*,'(3i5,10f7.3)') iyr,iida,ii,precipdt(ii),qin(ii),qout(ii),
!     & qloss,sed(1,ii)*1000.,sed_removed*1000.,sloss*1000.
      end do
      
      ! store end-of-day values for next day
      ft_qpnd(sb,kk) = qpnd(nstep)
      ft_qsw(sb,kk) = qsw(nstep)
      ft_qin(sb,kk) = qin(nstep)
      ft_qout(sb,kk) = qout(nstep)
      ft_sedpnd(sb,kk) = sedpnd 
      ft_fc(sb,kk) = fc(nstep)

      return
      end subroutine
   
