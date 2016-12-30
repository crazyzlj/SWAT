      subroutine rchaa(years)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the average annual reach output to the .rch file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ilog         |none         |streamflow print code
!!                               |0 print streamflow in reach
!!                               |1 print Log10 streamflow in reach
!!    ipdvar(:)    |none         |output variable codes for .rch file
!!    isproj       |none         |special project code:
!!                               |1 test rewind (run simulation twice)
!!    itotr        |none         |number of output variables printed (.rch)
!!    mrcho        |none         |maximum number of variables written to
!!                               |reach output file (.rch)
!!    rch_dakm(:)  |km**2        |total drainage area contributing to flow at
!!                               |the outlet (pour point) of the reach in
!!                               |square kilometers
!!    rchaao(1,:)  |m^3/s        |flow into reach during simulation
!!    rchaao(2,:)  |m^3/s        |flow out of reach during simulation
!!    rchaao(3,:)  |metric tons  |sediment transported into reach during 
!!                               |simulation
!!    rchaao(4,:)  |metric tons  |sediment transported out of reach during 
!!                               |simulation
!!    rchaao(5,:)  |mg/L         |sediment concentration in outflow during 
!!                               |simulation
!!    rchaao(6,:)  |kg N         |organic N transported into reach during 
!!                               |simulation
!!    rchaao(7,:)  |kg N         |organic N transported out of reach during
!!                               |simulation
!!    rchaao(8,:)  |kg P         |organic P transported into reach during 
!!                               |simulation
!!    rchaao(9,:)  |kg P         |organic P transported out of reach during
!!                               |simulation
!!    rchaao(10,:) |m^3/s        |evaporation from reach during simulation
!!    rchaao(11,:) |m^3/s        |transmission losses from reach during 
!!                               |simulation
!!    rchaao(12,:) |kg           |conservative metal #1 transported out of reach
!!                               |during simulation
!!    rchaao(13,:) |kg           |conservative metal #2 transported out of reach
!!                               |during simulation
!!    rchaao(14,:) |kg           |conservative metal #3 transported out of reach
!!                               |during simulation
!!    rchaao(15,:) |kg N         |nitrate transported into reach during 
!!                               |simulation
!!    rchaao(16,:) |kg N         |nitrate transported out of reach during 
!!                               |simulation
!!    rchaao(17,:) |kg P         |soluble P transported into reach during 
!!                               |simulation
!!    rchaao(18,:) |kg P         |soluble P transported out of reach during
!!                               |simulation
!!    rchaao(19,:) |mg pst       |soluble pesticide transported into reach
!!                               |during simulation
!!    rchaao(20,:) |mg pst       |soluble pesticide transported out of reach
!!                               |during simulation
!!    rchaao(21,:) |mg pst       |sorbed pesticide transported into reach during
!!                               |simulation
!!    rchaao(22,:) |mg pst       |sorbed pesticide transported out of reach
!!                               |during simulation
!!    rchaao(23,:) |mg pst       |amount of pesticide lost through reactions in
!!                               |reach during simulation
!!    rchaao(24,:) |mg pst       |amount of pesticide lost through
!!                               |volatilization from reach during simulation
!!    rchaao(25,:) |mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment during simulation
!!    rchaao(26,:) |mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach during simulation
!!    rchaao(27,:) |mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment during simulation
!!    rchaao(28,:) |mg pst       |amount of pesticide in sediment layer lost
!!                               |through reactions during simulation
!!    rchaao(29,:) |mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial during simulation
!!    rchaao(30,:) |kg chla      |chlorophyll-a transported into reach during
!!                               |simulation
!!    rchaao(31,:) |kg chla      |chlorophyll-a transported out of reach during
!!                               |simulation
!!    rchaao(32,:) |kg N         |ammonia transported into reach during 
!!                               |simuation
!!    rchaao(33,:) |kg N         |ammonia transported out of reach during 
!!                               |simuation
!!    rchaao(34,:) |kg N         |nitrite transported into reach during
!!                               |simuation
!!    rchaao(35,:) |kg N         |nitrite transported out of reach during 
!!                               |simuation
!!    rchaao(36,:) |kg O2        |CBOD transported into reach during simulation
!!    rchaao(37,:) |kg O2        |CBOD transported out of reach during 
!!                               |simuation
!!    rchaao(38,:) |kg O2        |dissolved oxygen transported into reach during
!!                               |simuation
!!    rchaao(39,:) |kg O2        |dissolved oxygen transported out of reach
!!                               |during simulation
!!    rchaao(40,:) |kg bact      |persistent bacteria transported out of reach
!!                               |during simulation
!!    rchaao(41,:) |kg bact      |less persistent bacteria transported out of
!!                               |reach during simulation
!!    rchdy(37,:)  |mg pst       |amount of pesticide stored in river bed
!!                               |sediments
!!    rchaao(43,:) |kg           |Total N (org N + no3 + no2 + nh4 outs)
!!    rchaao(44,:) |kg           |Total P (org P + sol p outs)

!!    subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
!!    subtot       |none         |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter (reach number)
!!    pdvar(:)    |varies        |array of default reach output values
!!    pdvr(:)     |varies        |array of custom reach output values
!!    srch_av(:)  |varies        |annual reach inflow/outflow 
!!    years       |years         |length of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log10

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real, intent (in) :: years
      integer :: j
      real, dimension (mrcho) :: pdvar, pdvr
      real, dimension (2) :: srch_av

      do j = 1, subtot

        !! take log10 of annual inflow/outflow for graphing
        srch_av = 0.
        srch_av(1) = rchaao(1,j)
        srch_av(2) = rchaao(2,j)
        if (ilog > 0) then
          if (srch_av(1) > 1.) then
            srch_av(1) = Log10(srch_av(1))
          else
            srch_av(1) = 0.
          end if
          if (srch_av(2) > 1.) then
            srch_av(2) = Log10(srch_av(2))
          else
            srch_av(2) = 0.
          end if
        end if

        pdvar = 0. 
        pdvr = 0.

        !! assign average annual values
        pdvar(1) = srch_av(1)        !!flow in (m^3/s)
        pdvar(2) = srch_av(2)        !!flow out (m^3/s)
        pdvar(3) = rchaao(10,j)      !!evaporation (m^3/s)
        pdvar(4) = rchaao(11,j)      !!tloss (m^3/s)
        pdvar(5) = rchaao(3,j)       !!sed in (metric tons)
        pdvar(6) = rchaao(4,j)       !!sed out (met tons)
        pdvar(7) = rchaao(5,j)       !!sed conc (mg/L)
        pdvar(8) = rchaao(6,j)       !!orgN in (kg N)
        pdvar(9) = rchaao(7,j)       !!orgN out (kg N)
        pdvar(10) = rchaao(8,j)      !!orgP in (kg P)
        pdvar(11) = rchaao(9,j)      !!orgP out (kg P)
        pdvar(12) = rchaao(15,j)     !!NO3 in (kg N)
        pdvar(13) = rchaao(16,j)     !!NO3 out (kg N)
        pdvar(14) = rchaao(32,j)     !!NH4 in (kg)
        pdvar(15) = rchaao(33,j)     !!NH4 out (kg)
        pdvar(16) = rchaao(34,j)     !!NO2 in (kg)
        pdvar(17) = rchaao(35,j)     !!NO2 out (kg)
        pdvar(18) = rchaao(17,j)     !!solP in (kg P)
        pdvar(19) = rchaao(18,j)     !!solP out (kg P)
        pdvar(20) = rchaao(30,j)     !!algae in (kg)
        pdvar(21) = rchaao(31,j)     !!algae out (kg)
        pdvar(22) = rchaao(36,j)     !!CBOD in (kg)
        pdvar(23) = rchaao(37,j)     !!CBOD out (kg)
        pdvar(24) = rchaao(38,j)     !!dis O2 in (kg)
        pdvar(25) = rchaao(39,j)     !!dis O2 out (kg)
        pdvar(26) = rchaao(19,j)     !!solpst in (mg pst)
        pdvar(27) = rchaao(20,j)     !!solpst out (mg pst)
        pdvar(28) = rchaao(21,j)     !!srbpst in (mg pst)
        pdvar(29) = rchaao(22,j)     !!srbpst out (mg pst)
        pdvar(30) = rchaao(23,j)     !!reacted pst (mg pst)
        pdvar(31) = rchaao(24,j)     !!volatilized pst (mg)
        pdvar(32) = rchaao(25,j)     !!pst settling (mg pst)
        pdvar(33) = rchaao(26,j)     !!pst resuspension (mg)
        pdvar(34) = rchaao(27,j)     !!pst diffuse to sed mg
        pdvar(35) = rchaao(28,j)     !!react pst/sed (mg)
        pdvar(36) = rchaao(29,j)     !!pst bury (mg)
        pdvar(37) = rchdy(37,j)      !!pst in rivbed sed mg
        pdvar(38) = rchaao(40,j)     !!p bact out
        pdvar(39) = rchaao(41,j)     !!lessp bact out 
        pdvar(40) = rchaao(12,j)     !!metal #1
        pdvar(41) = rchaao(13,j)     !!metal #2
        pdvar(42) = rchaao(14,j)     !!metal #3
 !! added for Total N (org N + no3 + no2 + nh4 outs) to output.rch gsm 10/17/2011
        pdvar(43) = rchaao(7,j) + rchaao(16,j) + rchaao(35,j) + 
     *   rchaao(33,j)                                               !! Total N
 !! added for Total P (org P + sol p outs)to output.rch gsm 10/17/2011
        pdvar(44) = rchaao(9,j) + rchaao(18,j)                      !! Total P
 
 !! added NO3 Concentration to output.rch (for daily only) gsm 10/26/2011
        
        
        if (ipdvar(1) > 0) then
          do ii = 1, itotr
            pdvr(ii) = pdvar(ipdvar(ii))
          end do
          if (iscen == 1 .and. isproj == 0) then
          write (7,5000) j, subgis(j), years, rch_dakm(j),              
     &                                    (pdvr(ii), ii = 1, itotr)
          else if (isproj == 1) then
          write (20,5000) j, subgis(j), years, rch_dakm(j),             
     &                                    (pdvr(ii), ii = 1, itotr)
          else if (iscen == 1 .and. isproj == 2) then
          write (7,6000) j, subgis(j), years, rch_dakm(j),              
     &                             (pdvr(ii), ii = 1, itotr), iyr  
          endif
        else
!!  increase to 44 in loops below from 42 gsm 10/17/2011
          if (iscen == 1 .and. isproj == 0) then
          write (7,5000) j, subgis(j), years, rch_dakm(j),              
     &                                (pdvar(ii), ii = 1, 44)    
          else if (isproj == 1) then
          write (20,5000) j, subgis(j), years, rch_dakm(j),             
     &                                (pdvar(ii), ii = 1, 44)    
          else if (iscen == 1 .and. isproj == 2) then
          write (7,6000) j, subgis(j), years, rch_dakm(j),              
     &                             (pdvar(ii), ii = 1, 44), iyr      
          endif
        end if
      end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,f5.1,47e12.4)
 6000 format ('REACH ',i4,1x,i8,1x,f5.1,47e12.4,1x,i4)
      end