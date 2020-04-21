      subroutine rchmon(mdays)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the monthly reach output to the .rch file

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
!!    mo_chk       |none         |month for current day of simulation
!!    mrcho        |none         |maximum number of variables written to
!!                               |reach output file (.rch)
!!    rch_dakm(:)  |km**2        |total drainage area contributing to flow at
!!                               |the outlet (pour point) of the reach in
!!                               |square kilometers
!!    rchdy(37,:)  |mg pst       |amount of pesticide stored in river bed
!!                               |sediments
!!    rchmono(1,:) |m^3/s        |flow into reach during month
!!    rchmono(2,:) |m^3/s        |flow out of reach during month
!!    rchmono(3,:) |metric tons  |sediment transported into reach during month
!!    rchmono(4,:) |metric tons  |sediment transported out of reach during month
!!    rchmono(5,:) |mg/L         |sediment concentration in outflow during month
!!    rchmono(6,:) |kg N         |organic N transported into reach during month
!!    rchmono(7,:) |kg N         |organic N transported out of reach during
!!                               |month
!!    rchmono(8,:) |kg P         |organic P transported into reach during month
!!    rchmono(9,:) |kg P         |organic P transported out of reach during
!!                               |month
!!    rchmono(10,:)|m^3/s        |evaporation from reach during month
!!    rchmono(11,:)|m^3/s        |transmission losses from reach during month
!!    rchmono(12,:)|kg           |conservative metal #1 transported out of reach
!!                               |during month
!!    rchmono(13,:)|kg           |conservative metal #2 transported out of reach
!!                               |during month
!!    rchmono(14,:)|kg           |conservative metal #3 transported out of reach
!!                               |during month
!!    rchmono(15,:)|kg N         |nitrate transported into reach during month
!!    rchmono(16,:)|kg N         |nitrate transported out of reach during month
!!    rchmono(17,:)|kg P         |soluble P transported into reach during month
!!    rchmono(18,:)|kg P         |soluble P transported out of reach during
!!                               |month
!!    rchmono(19,:)|mg pst       |soluble pesticide transported into reach
!!                               |during month
!!    rchmono(20,:)|mg pst       |soluble pesticide transported out of reach
!!                               |during month
!!    rchmono(21,:)|mg pst       |sorbed pesticide transported into reach during
!!                               |month
!!    rchmono(22,:)|mg pst       |sorbed pesticide transported out of reach
!!                               |during month
!!    rchmono(23,:)|mg pst       |amount of pesticide lost through reactions in
!!                               |reach during month
!!    rchmono(24,:)|mg pst       |amount of pesticide lost through
!!                               |volatilization from reach during month
!!    rchmono(25,:)|mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment during month
!!    rchmono(26,:)|mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach during month
!!    rchmono(27,:)|mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment during month
!!    rchmono(28,:)|mg pst       |amount of pesticide in sediment layer lost
!!                               |through reactions during month
!!    rchmono(29,:)|mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial during month
!!    rchmono(30,:)|kg chla      |chlorophyll-a transported into reach during
!!                               |month
!!    rchmono(31,:)|kg chla      |chlorophyll-a transported out of reach during
!!                               |month
!!    rchmono(32,:)|kg N         |ammonia transported into reach during month
!!    rchmono(33,:)|kg N         |ammonia transported out of reach during month
!!    rchmono(34,:)|kg N         |nitrite transported into reach during month
!!    rchmono(35,:)|kg N         |nitrite transported out of reach during month
!!    rchmono(36,:)|kg O2        |CBOD transported into reach during month
!!    rchmono(37,:)|kg O2        |CBOD transported out of reach during month
!!    rchmono(38,:)|kg O2        |dissolved oxygen transported into reach during
!!                               |month
!!    rchmono(39,:)|kg O2        |dissolved oxygen transported out of reach
!!                               |during month
!!    rchmono(40,:)|kg bact      |persistent bacteria transported out of reach
!!                               |during month
!!    rchmono(41,:)|kg bact      |less persistent bacteria transported out of
!!                               |reach during month

!!    rchmono(43,:)|kg           |Total N (org N + no3 + no2 + nh4 outs)
!!    rchmono(44,:)|kg           |Total P (org P + sol p outs)

!!    subgis(:)    |none         |GIS code printed to output files(output.sub,.rch)
!!    subtot       |none         |number of subbasins in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    j           |none          |counter (reach number)
!!    mdays       |none          |number of days simulated in month
!!    pdvar(:)    |varies        |array of default reach output values
!!    pdvr(:)     |varies        |array of custom reach output values
!!    srch_av(:)  |varies        |array storing average monthly values for
!!                               |reach output data
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Real, Log10

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: mdays
      integer :: j
      real, dimension (mrcho) :: pdvar, pdvr
      real, dimension (11) :: srch_av

      do j = 1, subtot

        !! calculate monthly averages where applicable
        srch_av = 0.
        srch_av(1) = rchmono(1,j) / Real(mdays)
        srch_av(2) = rchmono(2,j) / Real(mdays)
        !! take log10 of monthly flow for graphing
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
        srch_av(5) = rchmono(5,j) / Real(mdays)
        srch_av(10) = rchmono(10,j) / Real(mdays)
        srch_av(11) = rchmono(11,j) / Real(mdays)

        pdvar = 0. 
        pdvr = 0.

        !! assign monthly values
        pdvar(1) = srch_av(1)      !!flow in (m^3/s)
        pdvar(2) = srch_av(2)      !!flow out (m^3/s)
        pdvar(3) = srch_av(10)     !!evaporation (m^3/s)
        pdvar(4) = srch_av(11)     !!tloss (m^3/s)
        pdvar(5) = rchmono(3,j)    !!sed in (metric tons)
        pdvar(6) = rchmono(4,j)    !!sed out (met tons)
        pdvar(7) = srch_av(5)      !!sed conc (mg/L)
        pdvar(8) = rchmono(6,j)    !!orgN in (kg N)
        pdvar(9) = rchmono(7,j)    !!orgN out (kg N)
        pdvar(10) = rchmono(8,j)   !!orgP in (kg P)
        pdvar(11) = rchmono(9,j)   !!orgP out (kg P)
        pdvar(12) = rchmono(15,j)  !!NO3 in (kg N)
        pdvar(13) = rchmono(16,j)  !!NO3 out (kg N)
        pdvar(14) = rchmono(32,j)  !!NH4 in (kg)
        pdvar(15) = rchmono(33,j)  !!NH4 out (kg)
        pdvar(16) = rchmono(34,j)  !!NO2 in (kg)
        pdvar(17) = rchmono(35,j)  !!NO2 out (kg)
        pdvar(18) = rchmono(17,j)  !!solP in (kg P)
        pdvar(19) = rchmono(18,j)  !!solP out (kg P)
        pdvar(20) = rchmono(30,j)  !!algae in (kg)
        pdvar(21) = rchmono(31,j)  !!algae out (kg)
        pdvar(22) = rchmono(36,j)  !!CBOD in (kg)
        pdvar(23) = rchmono(37,j)  !!CBOD out (kg)
        pdvar(24) = rchmono(38,j)  !!dis O2 in (kg)
        pdvar(25) = rchmono(39,j)  !!dis O2 out (kg)
        pdvar(26) = rchmono(19,j)  !!solpst in (mg pst)
        pdvar(27) = rchmono(20,j)  !!solpst out (mg pst)
        pdvar(28) = rchmono(21,j)  !!srbpst in (mg pst)
        pdvar(29) = rchmono(22,j)  !!srbpst out (mg pst)
        pdvar(30) = rchmono(23,j)  !!reacted pst (mg pst)
        pdvar(31) = rchmono(24,j)  !!volatilized pst (mg)
        pdvar(32) = rchmono(25,j)  !!pst settling (mg pst)
        pdvar(33) = rchmono(26,j)  !!pst resuspension (mg)
        pdvar(34) = rchmono(27,j)  !!pst diffuse to sed mg
        pdvar(35) = rchmono(28,j)  !!react pst/sed (mg)
        pdvar(36) = rchmono(29,j)  !!pst bury (mg)
        pdvar(37) = rchdy(37,j)    !!pst in rivbed sed mg
        pdvar(38) = rchmono(40,j)  !!p bact out
        pdvar(39) = rchmono(41,j)  !!lessp bact out 
        pdvar(40) = rchmono(12,j)  !!metal #1
        pdvar(41) = rchmono(13,j)  !!metal #2
        pdvar(42) = rchmono(14,j)  !!metal #3
 !! added for Total N (org N + no3 + no2 + nh4 outs) to output.rch gsm 10/17/2011
        pdvar(43) = rchmono(7,j)+ rchmono(16,j) + rchmono(35,j) + 
     *   rchmono(33,j)                                                  !! Total N
 !! added for Total P (org P + sol p outs)to output.rch gsm 10/17/2011
        pdvar(44) = rchmono(9,j) + rchmono(18,j)                        !! Total P
 !! added NO3 Concentration to output.rch (for daily only) gsm 10/26/2011
 
 
        if (ipdvar(1) > 0) then
          do ii = 1, itotr
            pdvr(ii) = pdvar(ipdvar(ii))
          end do

          if (iscen == 1 .and. isproj == 0) then
          write (7,5000) j, subgis(j), mo_chk, rch_dakm(j),             &
     &                                         (pdvr(ii), ii = 1, itotr)
          else if (isproj == 1) then
          write (20,5000) j, subgis(j), mo_chk, rch_dakm(j),            &
     &                                         (pdvr(ii), ii = 1, itotr)
          else if (iscen == 1 .and. isproj == 2) then 
          write (7,6000) j, subgis(j), mo_chk, rch_dakm(j),             &
     &                              (pdvr(ii), ii = 1, itotr),iyr  
          endif
        else
 !  increase to 44 in loops below from 42 gsm 10/17/2011      
          if (iscen == 1 .and. isproj == 0) then
          write (7,5000) j, subgis(j), mo_chk, rch_dakm(j),             &
     &                                        (pdvar(ii), ii = 1, 44)    
          else if (isproj == 1) then
          write (20,5000) j, subgis(j), mo_chk, rch_dakm(j),            &
     &                                        (pdvar(ii), ii = 1, 44)    
          else if (iscen == 1 .and. isproj == 2) then
          write (7,6000) j, subgis(j), mo_chk, rch_dakm(j),             &
     &                              (pdvar(ii), ii = 1, 44), iyr     

          endif
        end if
      end do

      return
 5000 format ('REACH ',i4,1x,i8,1x,i5,46e12.4)
 6000 format ('REACH ',i4,1x,i8,1x,i5,46e12.4,1x,i4)
      end

