      subroutine rchyr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the annual reach output to the .rch file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i            |julian date  |current day of simulation
!!    id1          |julian date  |first day of simulation in current year
!!    ilog         |none         |streamflow print code
!!                               |0 print streamflow in reach
!!                               |1 print Log10 streamflow in reach
!!    ipdvar(:)    |none         |output variable codes for .rch file
!!    isproj       |none         |special project code:
!!                               |1 test rewind (run simulation twice)
!!    itotr        |none         |number of output variables printed (.rch)
!!    iyr          |year         |current year of simulation (eg 1980)
!!    mrcho        |none         |maximum number of variables written to
!!                               |reach output file (.rch)
!!    rch_dakm(:)  |km**2        |total drainage area contributing to flow at
!!                               |the outlet (pour point) of the reach in
!!                               |square kilometers
!!    rchdy(37,:)  |mg pst       |amount of pesticide stored in river bed
!!                               |sediments
!!    rchyro(1,:)  |m^3/s        |flow into reach during year
!!    rchyro(2,:)  |m^3/s        |flow out of reach during year
!!    rchyro(3,:)  |metric tons  |sediment transported into reach during year
!!    rchyro(4,:)  |metric tons  |sediment transported out of reach during year
!!    rchyro(5,:)  |mg/L         |sediment concentration in outflow during year
!!    rchyro(6,:)  |kg N         |organic N transported into reach during year
!!    rchyro(7,:)  |kg N         |organic N transported out of reach during
!!                               |year
!!    rchyro(8,:)  |kg P         |organic P transported into reach during year
!!    rchyro(9,:)  |kg P         |organic P transported out of reach during
!!                               |year
!!    rchyro(10,:) |m^3/s        |evaporation from reach during year
!!    rchyro(11,:) |m^3/s        |transmission losses from reach during year
!!    rchyro(12,:) |kg           |conservative metal #1 transported out of reach
!!                               |during year
!!    rchyro(13,:) |kg           |conservative metal #2 transported out of reach
!!                               |during year
!!    rchyro(14,:) |kg           |conservative metal #3 transported out of reach
!!                               |during year
!!    rchyro(15,:) |kg N         |nitrate transported into reach during year
!!    rchyro(16,:) |kg N         |nitrate transported out of reach during year
!!    rchyro(17,:) |kg P         |soluble P transported into reach during year
!!    rchyro(18,:) |kg P         |soluble P transported out of reach during
!!                               |year
!!    rchyro(19,:) |mg pst       |soluble pesticide transported into reach
!!                               |during year
!!    rchyro(20,:) |mg pst       |soluble pesticide transported out of reach
!!                               |during year
!!    rchyro(21,:) |mg pst       |sorbed pesticide transported into reach during
!!                               |year
!!    rchyro(22,:) |mg pst       |sorbed pesticide transported out of reach
!!                               |during year
!!    rchyro(23,:) |mg pst       |amount of pesticide lost through reactions in
!!                               |reach during year
!!    rchyro(24,:) |mg pst       |amount of pesticide lost through
!!                               |volatilization from reach during year
!!    rchyro(25,:) |mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment during year
!!    rchyro(26,:) |mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach during year
!!    rchyro(27,:) |mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment during year
!!    rchyro(28,:) |mg pst       |amount of pesticide in sediment layer lost
!!                               |through reactions during year
!!    rchyro(29,:) |mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial during year
!!    rchyro(30,:) |kg chla      |chlorophyll-a transported into reach during
!!                               |year
!!    rchyro(31,:) |kg chla      |chlorophyll-a transported out of reach during
!!                               |year
!!    rchyro(32,:) |kg N         |ammonia transported into reach during year
!!    rchyro(33,:) |kg N         |ammonia transported out of reach during year
!!    rchyro(34,:) |kg N         |nitrite transported into reach during year
!!    rchyro(35,:) |kg N         |nitrite transported out of reach during year
!!    rchyro(36,:) |kg O2        |CBOD transported into reach during year
!!    rchyro(37,:) |kg O2        |CBOD transported out of reach during year
!!    rchyro(38,:) |kg O2        |dissolved oxygen transported into reach during
!!                               |year
!!    rchyro(39,:) |kg O2        |dissolved oxygen transported out of reach
!!                               |during year
!!    rchyro(40,:) |kg bact      |persistent bacteria transported out of reach
!!                               |during year
!!    rchyro(41,:) |kg bact      |less persistent bacteria transported out of
!!                               |reach during year
!!    subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
!!    subtot       |none         |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    idlast      |none          |number of days simulated in year
!!    ii          |none          |counter
!!    j           |none          |(counter) reach number
!!    pdvar(:)    |varies        |array of default reach output values
!!    pdvr(:)     |varies        |array of custom reach output values
!!    srch_av(:)  |varies        |annual reach inflow/outflow 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: real*8, Log10

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real*8, dimension (mrcho) :: pdvar, pdvr
      real, dimension (5) :: srch_av

      idlast = 0
      idlast = i - (id1 - 1)
      

      do j = 1, subtot

        !! calculate annual averages where applicable
        rchyro(1,j) = rchyro(1,j) / dfloat(idlast)
        rchyro(2,j) = rchyro(2,j) / dfloat(idlast)
        rchyro(5,j) = rchyro(5,j) / dfloat(idlast)
        rchyro(10,j) = rchyro(10,j) / dfloat(idlast)
        rchyro(11,j) = rchyro(11,j) / dfloat(idlast)

        !! take log10 of annual inflow/outflow for graphing
        srch_av = 0.
        srch_av(1) = rchyro(1,j)
        srch_av(2) = rchyro(2,j)
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

        !! yearly averages for salt1, salt2, and salt3 - katrin 3/14/2017
        srch_av(3) = rchyro(59,j) / Real(idlast)
        srch_av(4) = rchyro(60,j) / Real(idlast)
        srch_av(5) = rchyro(61,j) / Real(idlast)
        
        pdvar = 0. 
        pdvr = 0.

        !! assign yearly values
        pdvar(1) = srch_av(1)        !!flow in (m^3/s)
        pdvar(2) = srch_av(2)        !!flow out (m^3/s)
        pdvar(3) = rchyro(10,j)      !!evaporation (m^3/s)
        pdvar(4) = rchyro(11,j)      !!tloss (m^3/s)
        pdvar(5) = rchyro(3,j)       !!sed in (metric tons)
        pdvar(6) = rchyro(4,j)       !!sed out (met tons)
        pdvar(7) = rchyro(5,j)       !!sed conc (mg/L)
        pdvar(8) = rchyro(6,j)       !!orgN in (kg N)
        pdvar(9) = rchyro(7,j)       !!orgN out (kg N)
        pdvar(10) = rchyro(8,j)      !!orgP in (kg P)
        pdvar(11) = rchyro(9,j)      !!orgP out (kg P)
        pdvar(12) = rchyro(15,j)     !!NO3 in (kg N)
        pdvar(13) = rchyro(16,j)     !!NO3 out (kg N)
        pdvar(14) = rchyro(32,j)     !!NH4 in (kg)
        pdvar(15) = rchyro(33,j)     !!NH4 out (kg)
        pdvar(16) = rchyro(34,j)     !!NO2 in (kg)
        pdvar(17) = rchyro(35,j)     !!NO2 out (kg)
        pdvar(18) = rchyro(17,j)     !!solP in (kg P)
        pdvar(19) = rchyro(18,j)     !!solP out (kg P)
        pdvar(20) = rchyro(30,j)     !!algae in (kg)
        pdvar(21) = rchyro(31,j)     !!algae out (kg)
        pdvar(22) = rchyro(36,j)     !!CBOD in (kg)
        pdvar(23) = rchyro(37,j)     !!CBOD out (kg)
        pdvar(24) = rchyro(38,j)     !!dis O2 in (kg)
        pdvar(25) = rchyro(39,j)     !!dis O2 out (kg)
        pdvar(26) = rchyro(19,j)     !!solpst in (mg pst)
        pdvar(27) = rchyro(20,j)     !!solpst out (mg pst)
        pdvar(28) = rchyro(21,j)     !!srbpst in (mg pst)
        pdvar(29) = rchyro(22,j)     !!srbpst out (mg pst)
        pdvar(30) = rchyro(23,j)     !!reacted pst (mg pst)
        pdvar(31) = rchyro(24,j)     !!volatilized pst (mg)
        pdvar(32) = rchyro(25,j)     !!pst settling (mg pst)
        pdvar(33) = rchyro(26,j)     !!pst resuspension (mg)
        pdvar(34) = rchyro(27,j)     !!pst diffuse to sed mg
        pdvar(35) = rchyro(28,j)     !!react pst/sed (mg)
        pdvar(36) = rchyro(29,j)     !!pst bury (mg)
        pdvar(37) = rchdy(37,j)      !!pst in rivbed sed mg
        pdvar(38) = rchyro(40,j)     !!p bact out
        pdvar(39) = rchyro(41,j)     !!lessp bact out 
        pdvar(40) = rchyro(12,j)     !!metal #1
        pdvar(41) = rchyro(13,j)     !!metal #2
        pdvar(42) = rchyro(14,j)     !!metal #3
 !! added for Total N (org N + no3 + no2 + nh4 outs) to output.rch gsm 10/17/2011
        pdvar(43) = rchyro(7,j)+ rchyro(16,j) + rchyro(35,j) + 
     *   rchyro(33,j)                                                  !! Total N
 !! added for Total P (org P + sol p outs)to output.rch gsm 10/17/2011
        pdvar(44) = rchyro(9,j) + rchyro(18,j)                        !! Total P
 !! added NO3 Concentration to output.rch (for daily only) gsm 10/26/2011
        pdvar(45) = 0.
        pdvar(46) = 0.
 
 !! salt - srini
        do ii=1,10
          pdvar(46+ii) = rchyro(58+ii,j)
        end do

!! salt sar and ec - katrin 
        
       if (srch_av(2) > .00001 .and. srch_av(3) > 0 .and. 
     &    srch_av(4) > 0 .and. srch_av(5) > 0) then 
          pdvar(57) = (srch_av(5) / (srch_av(2) * 86.4) / 23) / 
     &        SQRT(.5 * ((srch_av(3) / (srch_av(2) * 86.4) / 20.039) 
     &        + (srch_av(4) / (srch_av(2) * 86.4) / 12.1525)))
       else 
           pdvar(57) = 0.
       end if      
       
       if (srch_av(2) > .00001 .and. rchyro(58 + salt_num,j) > 0) then 
          pdvar(58) = ec_slp * ((rchyro(58 + salt_num,j) /
     &        Real(idlast)) / (srch_av(2) * 86.4)) + ec_int
       else 
          pdvar(58) = 0.
       end if 
        
       
         if (ipdvar(1) > 0) then
          do ii = 1, itotr
            pdvr(ii) = pdvar(ipdvar(ii))
          end do
          if (iscen == 1 .and. isproj == 0 .or. isproj == 3) then
          write (7,5000) j, subgis(j), iyr, rch_dakm(j),                
     &                                    (pdvr(ii), ii = 1, itotr)
          else if (isproj == 1) then
          write (20,5000) j, subgis(j), iyr, rch_dakm(j),               
     &                                    (pdvr(ii), ii = 1, itotr)
          else if (iscen == 1 .and. isproj == 2) then
          write (7,6000) j, subgis(j), iyr, rch_dakm(j),                
     &                             (pdvr(ii), ii = 1, itotr), iyr 
          endif
        else
     !!  increase to 44 in loops below from 42 gsm 10/17/2011
     !!  increase to 54 in loops below from 44 srini - salt 2/4/2017
          if (iscen == 1 .and. isproj == 0) then
          write (7,5000) j, subgis(j), iyr, rch_dakm(j),                
     &                                (pdvar(ii), ii = 1, 55)    
          else if (isproj == 1) then
          write (20,5000) j, subgis(j), iyr, rch_dakm(j),               
     &                                (pdvar(ii), ii = 1, 55)    
          else if (iscen == 1 .and. isproj == 2) then
          write (7,6000) j, subgis(j), iyr, rch_dakm(j),                
     &                             (pdvar(ii), ii = 1, 55), iyr     
     
          endif
        end if
      end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,i5,59e12.4)
 6000 format ('REACH ',i4,1x,i8,1x,i5,59e12.4,1x,i4)
      end