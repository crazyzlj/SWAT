      subroutine rthvsc()
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    This subroutine routes flow at any required time step through the reach 
!!    using a variable storage coefficient  
!!	Routing method: Enhanced Variable Storage routing (Jeong et al., 2014) adopted from APEX   

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)         |m             |average depth of main channel
!!    ch_k(2,:)       |mm/hr         |effective hydraulic conductivity of
!!                                   |main channel alluvium
!!    ch_l2(:)        |km            |length of main channel
!!    ch_n(2,:)       |none          |Manning's "n" value for the main channel
!!    ch_s(2,:)       |m/m           |average slope of main channel
!!    ch_w(2,:)       |m             |average width of main channel
!!    chside(:)       |none          |change in horizontal distance per unit
!!                                   |change in vertical distance on channel
!!                                   |side slopes; always set to 2 (slope=1/2)
!!    dthy            |hour          |time interval for flood routing
!!    evrch           |none          |Reach evaporation adjustment factor.
!!                                   |Evaporation from the reach is multiplied
!!                                   |by EVRCH. This variable was created to 
!!                                   |limit the evaporation predicted in arid 
!!                                   |regions.
!!    hhvaroute(2,:,:)|m^3 H2O       |water
!!    idt             |min           |model operational time step
!!    inum1           |none          |reach number
!!    inum2           |none          |inflow hydrograph storage location number
!!    pet_day         |mm H2O        |potential evapotranspiration
!!    phi(1,:)        |m^2           |cross-sectional area of flow in channel at
!!                                   |bankfull depth
!!    phi(6,:)        |m             |bottom width of main channel
!!    rchstor(:)      |m^3 H2O       |water stored in reach
!!    rnum1           |none          |fraction of overland flow
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hdepth(:)   |m             |depth of flow during time step
!!    hharea(:)   |m^2           |cross-sectional area of flow
!!    hhstor(:)   |m^3 H2O       |water stored in reach at end of time step
!!    hhtime(:)   |hr            |flow travel time for time step
!!    hrchwtr(:)  |m^3 H2O       |water stored in reach at beginning of time step
!!    hrtevp(:)   |m^3 H2O       |evaporation losses for hour
!!    hrttlc(:)    |m^3 H2O       |transmission losses for hour
!!    hrtwtr(:)   |m^3 H2O       |water leaving reach during time step
!!    hsdti(:)    |m^3/s         |average flow rate during time step
!!    rchdep      |m             |depth of flow on day
!!    rchstor(:)  |m^3 H2O       |water stored in reach
!!    rhy(:)          |m H2O         |main channel hydraulic radius
!!    rtevp       |m^3 H2O       |evaporation from reach on day
!!    rttime      |hr            |reach travel time
!!    rttlc       |m^3 H2O       |transmission losses from reach on day
!!    rtwtr       |m^3 H2O       |water leaving reach on day
!!    sdti        |m^3/s         |average flow on day in reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    c           |none          |inverse of channel side slope
!!    ii          |none          |counter (hour)
!!    inhyd       |none          |inflow hydrograph storage location number
!!    jrch        |none          |reach number
!!    p           |m             |wetted perimeter
!!    nstep       |none          |No. of steps in a day (depends on model operational time step)
!!    topw        |m             |width of channel at water level
!!    vol         |m^3 H2O       |volume of water in reach
!!    wtrin       |m^3 H2O       |water entering reach during hour
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sum, Min, Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch, ii, inhyd,j,l
      real :: wtrin, c, p
      real :: vol, topw
      real*8, dimension(nstep*100) :: QMS, QMSI
      real*8 :: ai, aii, ao, cbw, chw, fpw, g1, qi2, sss, xflo, zch, zi, zii, zo

      QMS = 0.
      QMSI = 0.
      hrtevp = 0.
      hrttlc = 0.
      
      jrch = inum1
      inhyd = inum2
    
      
      if (sum(QHY(:,inhyd,IHX(1)))==0.) return

      ii = 1
 	   DO J = 1, 4
	      ii = ii - 1
          DO K = 1, nstep+1 !number of time steps a day
              ii = ii + 1
              QMSI(ii) = QHY(K,inhyd,IHX(J)) !inflow from upstream during dt for the day, m^3/s
          END DO
       END DO
       
      if(sum(QMSI(1:nstep))<=0.01) then
          if (sum(QHY(1:nstep,ihout,IHX(1)))<=0.01) then
              !no new inflow or outflow from previous days
              return
          else
              !no new inflow for routing during today but there exists continuing outflow from yesterday. 
              ii = 1
  	         DO J = 1, 4
                 ii = ii - 1
                 DO K = 1, nstep+1
                      ii = ii + 1
                      QMS(ii) = QHY(K,ihout,IHX(J))  
                 END DO
             END DO
          end if
                        
      else
        !new inflow coming from upstream today which needs to be routed.
          
        QI1 = QHY(1,inhyd,IHX(1)) ! m3/s  !inflow at timestep 1 of the current day 
        QO1 = QHY(1,ihout,IHX(1)) !outflow at timestep 1 of the current day that is predicted previous day
        !QMSI(1) = QMSI(1) + rchstor(jrch) / (dthy * 3600.)
        
        ZCH = phi(7,jrch) !channel depth,m
        CBW = phi(6,jrch) ! channel bottom width, m
        XL3 = ch_l2(jrch) / 3.6 
        XLT = XL3 / dthy
        XLS = 1000.* ch_l2(jrch) * ch_s(2,jrch) 
        RCHX(jrch) = SQRT(ch_s(2,jrch)) / ch_n(2,jrch)
        SSS = SQRT(RCSS(jrch) * RCSS(jrch) + 1.)

        ADI = 0.
        STHY=0.
        ! ROUTE UNTIL OUTFLOW IS GREATER THAN 0. 
        IF (rchstor(jrch)>0.001.and.QO1>0.0001) THEN
            ii = 1
            QI2 = QI1
            QI1 = QHY(nstep,inhyd,IHX(4)) !Inflow during the previous time step (last time step of previous day), m3/s
            STHY = rchstor(jrch) / (dthy * 3600.)
        ELSE
            
            DO ii = 1, NHY(jrch)  !routing starts when inflow > 0.
               I1 = ii - 1                                                                    
               QI2 = QMSI(ii) !inflow at the current timestep; m3/s
               ADI = ADI + QI2
               CALL HQDAV(AI,CBW,QI2,SSS,ZCH,ZI,CHW,FPW,jrch) !INPUT:CBW,QI2,SSS,ZCH output: AI,ZI,CHW,FPW
               DD = SQRT((XLS+ZI)/XLS)                                                          
               XFLO = QI2 / DD 
               CALL HQDAV(AII,CBW,XFLO,SSS,ZCH,ZII,CHW,FPW,jrch)
               SMO = 2. * ADI - QI2                                                         
               XFLO = SMO - XLT * AII
               !O2 = SUM OF INFLOW - STORAGE                                                                         
               IF(XFLO > 0.)EXIT
               QMS(ii) = 0.
               STHY = STHY + QI2                                                                                                                                        
               QI1 = QI2
            END DO
        
        END IF
        IIY=0
      
        !Calculate discharge rate using variable storage coefficient
        DO 
            I1 = ii - 1
            QI2 = QMSI(ii)
            SIA = .5 * (QI1 + QI2) + STHY !SIA=Ia+S1/dt
            G1 = (QI1 + QI2 + QO1) / 3.
            GB = MAX(QI1,QI2,QO1)                      
            GL = 0.
            CALL HQDAV(AI,CBW,QI2,SSS,ZCH,ZI,CHW,FPW,jrch)
            IF(IIY == 0)THEN
                V = MAX(.1,QI2/AI)
                T1 = XL3 / V
                IIY = 1
            END IF
            DO IT = 1, 20
               CALL HQDAV(AO,CBW,G1,SSS,ZCH,ZO,CHW,FPW,jrch)
               XX = MAX(.75,(XLS+ZI-ZO)/XLS)
               DD = SQRT(XX)      
               V = MAX(.05,(QI2+G1)*DD/(AI+AO))
               T2 = XL3 / V
               TT = .5 * (T1 + T2)
               C = MIN(.99,2.*DTHY/(2.*TT+DTHY))
               Q1 = C * SIA !Q1 is outflow m3/s Eq(15) in Jeong et al. (2014)
               GQ = Q1 - G1                                                                       
                    IF (abs(GQ/(G1+0.0001)) < .001) EXIT
               IF (GQ > 0.)THEN
                   GL = G1
               ELSE
                   GB = G1
               END IF
                    G1 = .5 * (GB + GL)+0.0001
               IF (GB-GL < .001)EXIT                                                         
            END DO
            QO2 = MAX(.0001,G1)                                                  
            QMS(ii) = QO2 
            STHY = SIA - QO2
            T1 = T2
            IF(ii > NHY(jrch)) THEN
               QMSI(ii) = .9 * QMSI(I1)                                              
               IF(QMS(I1) <= 1.) EXIT
            END IF   
            ii = ii + 1
            QI1 = QI2
            QO1 = QO2
        END DO
        NHY(jrch) = ii - 1
      end if
      
      ii = 0
      DO K = 1, nstep+1
          ii = ii + 1
          QHY(K,ihout,IHX(1)) = QMS(ii)
      END DO
      DO J = 2, 4
          ii = ii - 1
          DO K = 1, nstep+1
             ii = ii + 1
             QHY(K,ihout,IHX(J)) = QMS(ii)
          END DO
	  END DO


      do ii = 1, nstep
          !discharge, m3 
          hrtwtr(ii) = QMS(ii) * dthy * 3600.

          CALL HQDAV(AI,CBW,QMS(ii),SSS,ZCH,ZI,CHW,FPW,jrch) !INPUT:CBW,Q,SSS,ZCH output: AI,ZI,CHW,FPW

          !! flow depth, m
          hdepth(ii) = ZI 
          
          !! cross-sectional area of flow, m2
          hharea(ii) = AI

          !! calculate wetted perimeter
          c = chside(jrch)
          if (hdepth(ii) <= ch_d(jrch)) then
             p = phi(6,jrch) + 2. * hdepth(ii) * Sqrt(1. + c * c)
          else
              p = phi(6,jrch) + 2. * ch_d(jrch) * Sqrt(1. + c * c) + 4. *    &
                  ch_w(2,jrch) + 2. * (hdepth(ii) - ch_d(jrch)) * Sqrt(17.)
          end if

          !! calculate hydraulic radius
          if (p > 0.01) then
             rhy(ii) = hharea(ii) / p
          else
             rhy(ii) = 0.
          end if

          !! rate of flow in reach, m3/s
          hsdti(ii) = QMS(ii)
       
          if (hsdti(ii) > 0.) then
          
            !! calculate travel time
            hhtime(ii) = ch_l2(jrch) * hharea(ii) / (3.6 * hsdti(ii))
            if (hhtime(ii) < 1.) then
               rttime = rttime + hhtime(ii)
            else
               rttime = rttime + 1.
            end if
          
            !! calculate transmission losses
            if (hhtime(ii) < 1.) then
              hrttlc(ii) = ch_k(2,jrch) * ch_l2(jrch) * p * hhtime(ii)
            else
              hrttlc(ii) = ch_k(2,jrch) * ch_l2(jrch) * p
            end if
            hrttlc(ii) = Min(hrtwtr(ii),hrttlc(ii))
            hrtwtr(ii) = hrtwtr(ii) - hrttlc(ii)
          
            !! calculate evaporation
            if (hrtwtr(ii) > 0.) then
                 
                !! calculate width of channel at water level
                if (hdepth(ii) <= ch_d(jrch)) then
                   topw = phi(6,jrch) + 2. * hdepth(ii) * chside(jrch)
                else
                   topw = 5. * ch_w(2,jrch) + 8. * (hdepth(ii) - ch_d(jrch))
                end if
          
                if (hhtime(ii) < 1.) then
                   hrtevp(ii) = evrch * pet_day/nstep * ch_l2(jrch) * topw * hhtime(ii)
                else
                   hrtevp(ii) = evrch * pet_day/nstep * ch_l2(jrch) * topw
                end if
               
                if (hrtevp(ii) < 0.) hrtevp(ii) = 0.
                hrtevp(ii) = Min(hrtwtr(ii),hrtevp(ii))
                hrtwtr(ii) = hrtwtr(ii) - hrtevp(ii)
            end if   
            
            !! set volume of water in channel at end of time step
            wtrin = QMSI(ii) * dthy * 3600.
            if (ii == 1) then
                hhstor(ii) = rchstor(jrch) + wtrin - hrtwtr(ii) -  &
                  hrtevp(ii) - hrttlc(ii)
            else
                hhstor(ii) = hhstor(ii-1) + wtrin - hrtwtr(ii) -   &
                  hrtevp(ii) - hrttlc(ii)
            end if
            if (hhstor(ii) < 0.) then
                hrtwtr(ii) = hrtwtr(ii) + hhstor(ii)
                hhstor(ii) = 0.
                if (hrtwtr(ii) < 1.e-12) hrtwtr(ii) = 0.
            end if
          
          end if
          
      end do                     !! end of sub-daily loop

      ii = 0
      DO K = 1, nstep
          ii = ii + 1
          hsdti(ii) = max(0.,hrtwtr(ii) / (dthy * 3600.)) !m3 -> m3/s
          QHY(K,ihout,IHX(1)) = hsdti(ii)
          rtevp = rtevp + hrtevp(ii)
          rttlc = rttlc + hrttlc(ii)
      END DO


      !! calculation of daily average values

      !! set volume of water in reach at end of day
      rchstor(jrch) = hhstor(nstep)
      !! calculate total amount of water leaving reach
      rtwtr = Sum(hrtwtr)
      !! calculate average flow area
      rcharea = Sum (hharea) / nstep
      !! calculate average flow depth
      rchdep = Sum(hdepth) / nstep
      !! calculate average flow rate
      sdti = Sum(hsdti) / nstep

      return
      end