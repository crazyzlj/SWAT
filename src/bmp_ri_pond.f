      subroutine ri_pond(kk,riflw,rised)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes water through a retention irrigation pond in the subbasin

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flw(:,:)    |mm            |stormwater runoff coming in/out of pond at a time step
!!    kk          |none          |pond id number in the subbasin
!!    ri_sed(:)   |tons          |total sediment deposited in the pond
!!    sed(:,:)    |mm            |overland flow sediment coming in/out of pond at a time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flw(:,:)    |mm            |stormwater runoff coming in/out of pond at a time step
!!    ri_sed(:)   |tons          |total sediment deposited in the pond
!!    sed(:,:)    |mm            |overland flow sediment coming in/out of pond at a time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sb          |none          |subbasin or reach number
!!    ii          |none          |time step
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none
     
      integer :: sb, kk, ii
      real :: tsa,mxvol,pdia,ksat,dp,sub_ha,mxh,hweir,phead,pipeflow
      real :: qin,qout,qpnd,sweir,hpnd,qet
      real :: qweir, qseep,qpipe,qpndi,decayexp,splw,qpump
      real :: sedconc,sedpndi, sedpnde,ksed,td,sedpump
      real, dimension(3,0:nstep), intent(in out) :: riflw,rised
      real, dimension(0:nstep) :: inflw,insed,outflw,outsed
      
      sb = inum1
      sub_ha = da_ha * sub_fr(sb)
      qin = 0.; qout = 0.
      outflw = 0.; outsed = 0.

      !! Initialize parameters, coefficients, etc
      tsa = ri_sa(sb,kk)     !total surface area of pond (m^2)
      mxvol = ri_vol(sb,kk)    !max. capacity of the basin (m^3)
      mxh = ri_dep(sb,kk)  !max. depth of water, m
      ksat = ri_k(sb,kk)      !saturated hydraulic conductivity (mm/hr)
                                     
      !! Get initial values from previous day
      qpnd = ri_qi(sb,kk) !m^3
      sedpnde =  ri_sedi(sb,kk)
      inflw(:) = riflw(1,:)
      insed(:) = rised(1,:)

    
      do ii=1,nstep

         qout = 0.

         !inflow = runoff + precipitation
         qin = inflw(ii) * 10. * (sub_ha - tsa / 10000.) + 
     &      precipdt(ii) * tsa / 1000.  !m^3
         
         !update ponded water volume
         qpnd = qpnd + qin
        
         !bypass flow when pond is full
         if (qpnd>mxvol) then
            qout = qpnd - mxvol !m3
            outflw(ii) = qout 
            outsed(ii) = insed(ii) * qout / qin !tons
            qpnd = mxvol
         end if
         
         !initial sediment
         sedpndi = sedpnde + insed(ii)
         if (qpnd>0) then
            sedconc = sedpndi / qpnd * 1.e6 !mg/l
         else
            sedconc = 0.
         endif
         
         !Transmission loss through infiltration 
         qseep = ksat * tsa / 1000./ 60. * idt !m^3
                        
         !Evapotranspiration loss
         qet = ri_evrsv(sb,kk) * tsa * pet_day / 1000. / 1440. * idt !m^3
         
         !water pumped for irrigation
         qpump =  max(0.,ri_pmpvol(kk,ii))
         
         !mass balance  
         qpnd = qpnd - qseep - qet - qpump
         if(qpnd<0) qpnd = 0
         hpnd = qpnd / (mxvol / mxh)
          
         !Estimate TSS removal due to sedimentation
         if (sedconc>12.) then ! assume 12mg/l as equilibrium concentration, , Huber et al. 2006
           ksed = min(134.8,41.1 * hpnd ** -0.999)  !decay coefficient, Huber et al. 2006
           td = 1. / nstep !detention time, day
           sedconc = (sedconc - 12.) * exp(-ksed * td) + 12.
         endif

         !sediment pumped
         sedpump = qpump * sedconc / 1.e6 !tons
         
         !sediment deposition
         sedpnde = qpnd *sedconc / 1.e6 !tons
         
         ri_sed_cumul(sb,kk) = ri_sed(sb,kk) + sedpndi - sedpnde
          
         riflw(1,ii) = qin / (sub_ha *10000. - tsa) * 1000.  !mm
         riflw(2,ii) = outflw(ii) / (sub_ha *10000. - tsa) * 1000. 
         riflw(3,ii) = qpump / (sub_ha *10000. - tsa) * 1000. 
         rised(3,:) = sedpump 
         rised(2,:) = outsed(:)
      End do
      
     
      ! Store end-of-day values for next day
      ri_qi(sb,kk) = qpnd
      ri_qloss(kk,1) = qet
      ri_qloss(kk,2) = qseep
      ri_sedi(sb,kk) = sedpnde

      return
      end subroutine
   

