      subroutine rchday

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the daily reach output to the .rch file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iida         |julian date  |current day of simulation
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
!!    rchdy(1,:)   |m^3/s        |flow into reach on day
!!    rchdy(2,:)   |m^3/s        |flow out of reach on day
!!    rchdy(3,:)   |m^3/s        |evaporation from reach on day
!!    rchdy(4,:)   |m^3/s        |transmission losses from reach on day
!!    rchdy(5,:)   |metric tons  |sediment transported into reach on day
!!    rchdy(6,:)   |metric tons  |sediment transported out of reach on day
!!    rchdy(7,:)   |mg/L         |sediment concentration in outflow
!!    rchdy(8,:)   |kg N         |organic N transported into reach on day
!!    rchdy(9,:)   |kg N         |organic N transported out of reach on day
!!    rchdy(10,:)  |kg P         |organic P transported into reach on day
!!    rchdy(11,:)  |kg P         |organic P transported out of reach on day
!!    rchdy(12,:)  |kg N         |nitrate transported into reach on day
!!    rchdy(13,:)  |kg N         |nitrate transported out of reach on day
!!    rchdy(14,:)  |kg N         |ammonia transported into reach on day
!!    rchdy(15,:)  |kg N         |ammonia transported out of reach on day
!!    rchdy(16,:)  |kg N         |nitrite transported into reach on day
!!    rchdy(17,:)  |kg N         |nitrite transported out of reach on day
!!    rchdy(18,:)  |kg P         |soluble P transported into reach on day
!!    rchdy(19,:)  |kg P         |soluble P transported out of reach on day
!!    rchdy(20,:)  |kg chla      |chlorophyll-a transported into reach on day
!!    rchdy(21,:)  |kg chla      |chlorophyll-a transported out of reach on day
!!    rchdy(22,:)  |kg O2        |CBOD transported into reach on day
!!    rchdy(23,:)  |kg O2        |CBOD transported out of reach on day
!!    rchdy(24,:)  |kg O2        |dissolved oxygen transported into reach on day
!!    rchdy(25,:)  |kg O2        |dissolved oxygen transported out of reach on
!!                               |day
!!    rchdy(26,:)  |mg pst       |soluble pesticide transported into reach on
!!                               |day
!!    rchdy(27,:)  |mg pst       |soluble pesticide transported out of reach on
!!                               |day
!!    rchdy(28,:)  |mg pst       |sorbed pesticide transported into reach on day
!!    rchdy(29,:)  |mg pst       |sorbed pesticide transported out of reach on
!!                               |day
!!    rchdy(30,:)  |mg pst       |amount of pesticide lost through reactions in
!!                               |reach on day
!!    rchdy(31,:)  |mg pst       |amount of pesticide lost through
!!                               |volatilization from reach on day
!!    rchdy(32,:)  |mg pst       |amount of pesticide settling out of reach to
!!                               |bed sediment on day
!!    rchdy(33,:)  |mg pst       |amount of pesticide resuspended from bed
!!                               |sediment to reach on day
!!    rchdy(34,:)  |mg pst       |amount of pesticide diffusing from reach to
!!                               |bed sediment on day
!!    rchdy(35,:)  |mg pst       |amount of pesticide in sediment layer lost
!!                               |through reactions on day
!!    rchdy(36,:)  |mg pst       |amount of pesticide in sediment layer lost
!!                               |through burial on day
!!    rchdy(37,:)  |mg pst       |amount of pesticide stored in river bed
!!                               |sediments
!!    rchdy(38,:)  |kg bact      |persistent bacteria transported out of reach
!!                               |on day
!!    rchdy(39,:)  |kg bact      |less persistent bacteria transported out of
!!                               |reach on day
!!    rchdy(40,:)  |kg           |amount of conservative metal #1 transported
!!                               |out of reach on day
!!    rchdy(41,:)  |kg           |amount of conservative metal #2 transported
!!                               |out of reach on day
!!    rchdy(42,:)  |kg           |amount of conservative metal #3 transported
!!                               |out of reach on day
!!    rchday(43,:) |kg           |Total N (org N + no3 + no2 + nh4 outs)
!!    rchday(44,:) |kg           |Total P (org P + sol p outs)

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
!!    srch_av(:)  |varies        |daily flow values for reach
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log10

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real, dimension (mrcho) :: pdvar, pdvr
      real, dimension (2) :: srch_av

      do j = 1, subtot

        !! take log10 of daily flow for graphing
        srch_av = 0.
        srch_av(1) = rchdy(1,j)
        srch_av(2) = rchdy(2,j)

        !! take log10 of daily flow for graphing
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

        !! assign daily values
        pdvar(1) = srch_av(1)       !!flow in (m^3/s)
        pdvar(2) = srch_av(2)       !!flow out (m^3/s)
        pdvar(3) = rchdy(3,j)       !!evaporation (m^3/s)
        pdvar(4) = rchdy(4,j)       !!tloss (m^3/s)
        pdvar(5) = rchdy(5,j)       !!sed in (metric tons)
        pdvar(6) = rchdy(6,j)       !!sed out (met tons)
        pdvar(7) = rchdy(7,j)       !!sed conc (mg/L)
        pdvar(8) = rchdy(8,j)       !!orgN in (kg N)
        pdvar(9) = rchdy(9,j)       !!orgN out (kg N)
        pdvar(10) = rchdy(10,j)     !!orgP in (kg P)
        pdvar(11) = rchdy(11,j)     !!orgP out (kg P)
        pdvar(12) = rchdy(12,j)     !!NO3 in (kg N)
        pdvar(13) = rchdy(13,j)     !!NO3 out (kg N)
        pdvar(14) = rchdy(14,j)     !!NH4 in (kg)
        pdvar(15) = rchdy(15,j)     !!NH4 out (kg)
        pdvar(16) = rchdy(16,j)     !!NO2 in (kg)
        pdvar(17) = rchdy(17,j)     !!NO2 out (kg)
        pdvar(18) = rchdy(18,j)     !!solP in (kg P)
        pdvar(19) = rchdy(19,j)     !!solP out (kg P)
        pdvar(20) = rchdy(20,j)     !!algae in (kg)
        pdvar(21) = rchdy(21,j)     !!algae out (kg)
        pdvar(22) = rchdy(22,j)     !!CBOD in (kg)
        pdvar(23) = rchdy(23,j)     !!CBOD out (kg)
        pdvar(24) = rchdy(24,j)     !!dis O2 in (kg)
        pdvar(25) = rchdy(25,j)     !!dis O2 out (kg)
        pdvar(26) = rchdy(26,j)     !!solpst in (mg pst)
        pdvar(27) = rchdy(27,j)     !!solpst out (mg pst)
        pdvar(28) = rchdy(28,j)     !!srbpst in (mg pst)
        pdvar(29) = rchdy(29,j)     !!srbpst out (mg pst)
        pdvar(30) = rchdy(30,j)     !!reacted pst (mg pst)
        pdvar(31) = rchdy(31,j)     !!volatilized pst (mg)
        pdvar(32) = rchdy(32,j)     !!pst settling (mg pst)
        pdvar(33) = rchdy(33,j)     !!pst resuspension (mg)
        pdvar(34) = rchdy(34,j)     !!pst diffuse to sed mg
        pdvar(35) = rchdy(35,j)     !!react pst/sed (mg)
        pdvar(36) = rchdy(36,j)     !!pst bury (mg)
        pdvar(37) = rchdy(37,j)     !!pst in rivbed sed mg
        pdvar(38) = rchdy(38,j)     !!p bact out
        pdvar(39) = rchdy(39,j)     !!lessp bact out 
        pdvar(40) = rchdy(40,j)     !!metal #1
        pdvar(41) = rchdy(41,j)     !!metal #2
        pdvar(42) = rchdy(42,j)     !!metal #3
 !! added for Total N (org N + no3 + no2 + nh4 outs) to output.rch gsm 10/17/2011
        pdvar(43) = rchdy(9,j)+ rchdy(13,j) + rchdy(17,j) + rchdy(15,j) !!Total N
 !! added for Total P (org P + sol p outs)to output.rch gsm 10/17/2011
        pdvar(44) = rchdy(11,j) + rchdy(19,j)                            !! Total P
 !! added for NO3 Concentration to output.rch (daily only) gsm 10/26/2011
        if (srch_av(2) > .001) then            
          pdvar(45) = rchdy(13,j) / (srch_av(2)* 86.4)    !! NO3 Concentration
        else
          pdvar(45) = 0. 
        endif
        pdvar(46) = rchdy(60,j)   ! water temperature deg c
        

!!  compute month and day given julian day
        call xmon 

      if (ievent==1.and.iprint==3) then
	  ! print out subdaily reach output in output.rch
        if (ipdvar(1) > 0) then
          do kk=1,nstep
           do ii = 1, itotr
             pdvr(ii) = rchhr(ipdvar(ii),j,kk)
           end do
           if (iscen == 1 .and. isproj == 0) then
             write (7,5001) j, subgis(j), iida, kk, rch_dakm(j),        
     &                                    (pdvr(ii), ii = 1, itotr)
           end if
          end do
	  else
	    if (iscen == 1 .and. isproj == 0) then
           do kk=1,nstep
             write (7,5001) j, subgis(j), iida, kk, rch_dakm(j),        
     &                                    (rchhr(ii,j,kk), ii = 1, 7)
	     end do
	     endif
	  endif
      else
        if (ipdvar(1) > 0) then
          do ii = 1, itotr
            pdvr(ii) = pdvar(ipdvar(ii))
          end do
          if (iscen == 1 .and. isproj == 0) then
            if (icalen == 0) write (7,5000) j, subgis(j), iida,       
     &             rch_dakm(j), (pdvr(ii), ii = 1, itotr)
            if(icalen == 1)write (7,5002) j, subgis(j), i_mo, icl(iida),
     &             iyr, rch_dakm(j), (pdvr(ii), ii = 1, itotr)
!!    added for binary files 3/25/09 gsm line below and write (77777
	      if (ia_b == 1) then
             write (77777) j, subgis(j), iida, rch_dakm(j),             
     &                                         (pdvr(ii), ii = 1, itotr)
            endif	        
          else if (isproj == 1) then
          write (20,5000) j, subgis(j), iida, rch_dakm(j),              
     &                                    (pdvr(ii), ii = 1, itotr)
          else if (iscen == 1 .and. isproj == 2) then
          if(icalen == 0)write (7,6000) j, subgis(j), iida, rch_dakm(j),
     &                               (pdvr(ii), ii = 1, itotr),iyr 
          if (icalen == 1) write (7,6002) j, subgis(j), i_mo, icl(iida),
     &          iyr, rch_dakm(j),(pdvr(ii), ii = 1, itotr), iyr
          endif
        else
        
  !  increase to 45 in loops below from 42 gsm 10/26/2011      
          if (iscen == 1 .and. isproj == 0) then
          if (icalen == 0)write(7,5000) j, subgis(j), iida, rch_dakm(j),
     &                                        (pdvar(ii), ii = 1, 45)
          if (icalen == 1) write (7,5002) j, subgis(j), i_mo, icl(iida),
     &            iyr, rch_dakm(j),(pdvar(ii), ii = 1, 45)

!!    added for binary files 3/25/09 gsm line below and write (77777
             if (ia_b == 1) then
                write (77777) j, subgis(j), iida, rch_dakm(j),          
     &                                         (pdvr(ii), ii = 1, itotr)
             endif	    
             
          else if (isproj == 1) then
          write (20,5000) j, subgis(j), iida, rch_dakm(j),              
     &                                        (pdvar(ii), ii = 1, 45) 
          else if (iscen == 1 .and. isproj == 2) then
          if (icalen == 0)write(7,6000) j, subgis(j), iida, rch_dakm(j),
     &                               (pdvar(ii), ii = 1, 45), iyr 
          if (icalen == 1) write (7,6002) j, subgis(j), i_mo, icl(iida),
     &              iyr, rch_dakm(j), (pdvar(ii), ii = 1, 45)
          endif
        end if
      endif
      end do
      return

 5000 format ('REACH ',i4,1x,i8,1x,i5,47e12.4)
 5001 format ('REACH ',i4,1x,i8,1x,i5,1x,i5,47e12.4)
 5002 format ('REACH ',i4,1x,i8,2x,i2,1x,i2,1x,i4,1x,47e12.4)
 6000 format ('REACH ',i4,1x,i8,1x,i5,47e12.4,1x,i4)
 6002 format ('REACH ',i4,1x,i8,1x,i2,1x,i2,1x,i4,1x,47e12.4,1x,i4)
      end