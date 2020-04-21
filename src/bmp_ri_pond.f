      subroutine ri_pond(kk,inflw,outflw,insed,outsed)
      
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
      real*8 :: qin,qout,qpnd,sweir,sedpnd,hpnd,qet
      real*8 :: qweir, qseep,qpipe,qpndi,decayexp,splw,qpump
      real, dimension(0:nstep), intent(in) :: inflw,insed
      real, dimension(0:nstep), intent(out) :: outflw,outsed
      
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
      

    
      do ii=1,nstep

         qout = 0.

         !inflow = runoff + precipitation
         qin = inflw(ii) * 10. * (sub_ha - tsa / 10000.) + 
     &      precipdt(ii) * tsa / 1000.  !m^3
        
         !bypass flow when pond is full
         if (qin+qpnd>mxvol) then
            qout = qin + qpnd - mxvol !m3
            outflw(ii) = qout / ((sub_ha- tsa / 10000.)*10.) !mm
            outsed(ii) = insed(ii) * qout / qin !tons
         end if
         
         !Transmission loss through infiltration 
         qseep = ksat * tsa / 1000./ 60. * idt !m^3
                        
         !Evapotranspiration loss
         qet = ri_evrsv(sb,kk) * tsa * pet_day / 1000. / 1440. * idt !m^3
         
         !water pumped for irrigation
         qpump =  max(0.,ri_pmpvol(kk,ii))
         
         !mass balance  
         qpnd = qpnd + qin - qseep - qet - qout - qpump
         if(qpnd<0) qpnd = 0
          
         !sediment deposition
         ri_sed_cumul(sb,kk) = ri_sed(sb,kk) + insed(ii) - outsed(ii)
          
      End do
     
      ! Store end-of-day values for next day
      ri_qi(sb,kk) = qpnd
      ri_qloss(kk,1) = qet
      ri_qloss(kk,2) = qseep

      return
      end subroutine
   

