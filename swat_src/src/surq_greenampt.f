      subroutine surq_greenampt

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Predicts daily runoff given breakpoint precipitation and snow melt
!!    using the Green & Ampt technique

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idt         |minutes       |length of time step used to report
!!                               |precipitation data for sub-daily modeling
!!    ihru        |none          |HRU number
!!    iyr         |year          |year being simulated (eg 1980)
!!    nstep       |none          |max number of time steps per day
!!    newrti(:)   |mm/hr         |infiltration rate for last time step from the
!!                               |previous day
!!    nstep       |none          |number of rainfall time steps for day
!!    precipdt(:) |mm H2O        |precipitation for the time step during day
!!    sol_k(1,:)  |mm/hr         |saturated hydraulic conductivity of 1st soil
!!                               |layer
!!    sol_por(:,:)|none          |total porosity of soil layer expressed as a
!!                               |fraction of the total volume
!!    sol_sumfc(:)|mm H2O        |amount of water held in the soil profile
!!                               |at field capacity
!!    sol_sw(:)   |mm H2O        |amount of water stored in soil profile on
!!                               |any given day
!!    swtrg(:)    |none          |rainfall event flag:
!!                               |  0: no rainfall event over midnight
!!                               |  1: rainfall event over midnight
!!    wfsh(:)     |mm            |average capillary suction at wetting front
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhqday(:)   |mm H2O        |surface runoff generated each hour of day
!!                               |in HRU
!!    newrti(:)   |mm/hr         |infiltration rate for last time step from the
!!                               |previous day
!!    surfq(:)    |mm H2O        |surface runoff for the day in HRU
!!    swtrg(:)    |none          |rainfall event flag:
!!                               |  0: no rainfall event over midnight
!!                               |  1: rainfall event over midnight
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    adj_hc      |mm/hr         |adjusted hydraulic conductivity
!!    cuminf(:)   |mm H2O        |cumulative infiltration for day
!!    cumr(:)     |mm H2O        |cumulative rainfall for day
!!    dthet       |mm/mm         |initial moisture deficit
!!    excum(:)    |mm H2O        |cumulative runoff for day
!!    exinc(:)    |mm H2O        |runoff for time step
!!    f1          |mm H2O        |test value for cumulative infiltration
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    kk          |hour          |hour of day in which runoff is generated
!!    psidt       |mm            |suction at wetting front*initial moisture 
!!                               |deficit
!!    rateinf(:)  |mm/hr         |infiltration rate for time step
!!    rintns(:)   |mm/hr         |rainfall intensity
!!    soilw       |mm H2O        |amount of water in soil profile
!!    tst         |mm H2O        |test value for cumulative infiltration
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Exp, Real, Mod

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer :: j, k, kk, sb, ii
      real :: adj_hc, dthet, soilw, psidt, tst, f1
      real :: lid_prec, lid_cumr, urban_prec
      real, dimension (nstep+1) :: cumr, cuminf, excum, exinc, rateinf
      real, dimension (nstep+1) :: rintns
        !! array location #1 is for last time step of prev day

       j = 0
       j = ihru
       sb = hru_sub(j)     
      
       !! reset values for day
       cumr = 0.
       cuminf = 0.
       excum = 0.
       exinc = 0.
       rateinf = 0.
       rintns = 0.

       !! calculate effective hydraulic conductivity
       adj_hc = 0.
       adj_hc = (56.82 * sol_k(1,j) ** 0.286)          
     &               / (1. + 0.051 * Exp(0.062 * cnday(j))) - 2.
       if (adj_hc <= 0.) adj_hc = 0.001

       dthet = 0.
       if (swtrg(j) == 1) then
         swtrg(j) = 0
         dthet = 0.001 * sol_por(1,j) * 0.95
         rateinf(1) = newrti(j)
         newrti(j) = 0.
       else
         soilw = 0.
         if (sol_sw(j) >= sol_sumfc(j)) then
           soilw = 0.999 * sol_sumfc(j)
         else
           soilw = sol_sw(j)
         end if
         dthet = (1. - soilw / sol_sumfc(j)) * sol_por(1,j) * 0.95
         rateinf(1) = 2000.
       end if

       psidt = 0.
       psidt = dthet * wfsh(j)

       k = 1
       rintns(1) = 60. * precipdt(2) / Real(idt)  !! urban 60./idt  NK Feb 4,08

       do k = 2, nstep+1
         !! calculate total amount of rainfall during day for time step
         cumr(k) = cumr(k-1) + precipdt(k)
         !! and rainfall intensity for time step
         rintns(k) = 60. * precipdt(k+1) / Real(idt) !!urban 60./idt NK Feb 4,08 

         !! if rainfall intensity is less than infiltration rate
         !! everything will infiltrate
         if (rateinf(k-1) >= rintns(k-1)) then
           cuminf(k) = cuminf(k-1) + rintns(k-1) * Real(idt) / 60. !!urban 60./idt NK Feb 4,08
           if (excum(k-1) > 0.) then
             excum(k) = excum(k-1)
             exinc(k) = 0.
           else
             excum(k) = 0.
             exinc(k) = 0.
           end if
          else
          !! if rainfall intensity is greater than infiltration rate
          !! find cumulative infiltration for time step by successive
          !! substitution
           tst = 0.
           tst = adj_hc * Real(idt) / 60.  !!urban 60./idt NK Feb 4,08
           do
             f1 = 0.
             f1 = cuminf(k-1) + adj_hc * Real(idt) / 60. +             
     &             psidt * Log((tst + psidt)/(cuminf(k-1) + psidt))
             if (Abs(f1 - tst) <= 0.001) then
               cuminf(k) = f1
               excum(k) = cumr(k) - cuminf(k)
               exinc(k) = excum(k) - excum(k-1)
               if (exinc(k) < 0.) exinc(k) = 0.
               hhqday(k-1) = exinc(k)
               exit
             else
               tst = 0.
               tst = f1
             end if
           end do
         end if  

	   !! Urban Impervious cover 
	   if (iurban(j)>0) then
	     !runoff from pervious area
	     hhqday(k-1) = hhqday(k-1) * (1.- fcimp(urblu(j))) 


           ! runoff from a LID and its upstream drainage areas (green roof, rain garden, cistern, and porous pavement)
           if (lid_onoff(sb,urblu(j))==1) then
             lid_prec = real(precipdt(k) - abstinit)
             if (lid_prec < 0.) lid_prec = 0.
             call lids(sb,j,k,lid_prec)
             lid_qsurf_total = 0.
             lid_farea_sum = 0.
             do ii = 1, 4
               if (lid_farea(j,ii) > 0) then
                 lid_qsurf_total = lid_qsurf_total + fcimp(urblu(j)) * 
     &           lid_farea(j,ii) * lid_qsurf(j,ii)
                 if (ii==1) then
                   if (cs_grcon(sb,urblu(j))==0) then
                     lid_farea_sum = lid_farea_sum + lid_farea(j,ii)
                   end if
                 else
                   lid_farea_sum = lid_farea_sum + lid_farea(j,ii)
                 end if
               end if
             end do
!             if (lid_farea_sum > 1.0) ! error massage
!             ubnrunoff(k-1) = (precipdt(k) - abstinit) * fcimp(urblu(j))


             ubnrunoff(k-1) = lid_prec * fcimp(urblu(j))
     &       * (1 - lid_farea_sum) + lid_qsurf_total
           else
             urban_prec = precipdt(k) - abstinit
             if (urban_prec < 0.) urban_prec = 0.
!             ubnrunoff(k-1) = (precipdt(k) - abstinit) * fcimp(urblu(j))
             ubnrunoff(k-1) = urban_prec * fcimp(urblu(j))
           end if
         else
           ubnrunoff(k-1) = 0.
         end if

         if (ubnrunoff(k-1)<0)  ubnrunoff(k-1) = 0.
         
	   !! daily total runoff
	   surfq(j) = surfq(j) + hhqday(k-1) + ubnrunoff(k-1)

         !! calculate new rate of infiltration
         rateinf(k) = adj_hc * (psidt / (cuminf(k) + 1.e-6) + 1.)
        
      end do
       
      if (Sum(precipdt) > 12.) then
        swtrg(j) = 1
        newrti(j) = rateinf(nstep)
      end if

      return
 5000 format(//,'Excess rainfall calculation for day ',i3,' of year ',  
     &        i4,' for sub-basin',i4,'.',/)
 5001 format(t2,'Time',t9,'Incremental',t22,'Cumulative',t35,'Rainfall',
     &       t45,'Infiltration',t59,'Cumulative',t71,'Cumulative',t82,  
     &       'Incremental',/,t2,'Step',t10,'Rainfall',t23,'Rainfall',   
     &       t35,'Intensity',t49,'Rate',t58,'Infiltration',t73,'Runoff',
     &       t84,'Runoff',/,t12,'(mm)',t25,'(mm)',t36,'(mm/h)',t48,     
     &       '(mm/h)',t62,'(mm)',t74,'(mm)',t85,'(mm)',/)
 5002 format(i5,t12,f5.2,t24,f6.2,t36,f6.2,t47,f7.2,t61,f6.2,t73,f6.2,  
     &       t84,f6.2)
      end