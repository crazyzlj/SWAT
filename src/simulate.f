      subroutine simulate

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine contains the loops governing the modeling of processes
!!    in the watershed 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    biomix(:)   |none          |biological mixing efficiency.
!!                               |Mixing of soil due to activity of earthworms
!!                               |and other soil biota. Mixing is performed at
!!                               |the end of every calendar year.
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                               |planting
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    idaf        |julian date   |beginning day of simulation
!!    idal        |julian date   |ending day of simulation
!!    idplt(:)    |none          |land cover code from crop.dat
!!    igro(:)     |none          |land cover status code:
!!                               |0 no land cover currently growing
!!                               |1 land cover growing
!!    iyr         |none          |beginning year of simulation
!!    mcr         |none          |max number of crops grown per year
!!    nbyr        |none          |number of years in simulation
!!    ncrops(:,:,:)|
!!    nhru        |none          |number of HRUs in watershed
!!    nro(:)      |none          |sequence number of year in rotation
!!    nrot(:)     |none          |number of years of rotation
!!    nyskip      |none          |number of years to not print output
!!    phu_plt(:)  |heat units    |total number of heat units to bring plant
!!                               |to maturity
!!    sub_lat(:)  |degrees       |latitude of HRU/subbasin
!!    tnyld(:)    |kg N/kg yield |modifier for autofertilization target
!!                               |nitrogen content for plant
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr       |none          |current year in simulation (sequence)
!!    hi_targ(:,:,:)|(kg/ha)/(kg/ha)|harvest index target of cover defined at
!!                               |planting
!!    hvstiadj(:) |(kg/ha)/(kg/ha)|optimal harvest index for current time during
!!                               |growing season
!!    i           |julian date   |current day in simulation--loop counter
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    id1         |julian date   |first day of simulation in current year
!!    iida        |julian date   |day being simulated (current julian day)
!!    idplt(:)    |none          |land cover code from crop.dat
!!    iyr         |year          |current year of simulation (eg 1980)
!!    laimxfr(:)  |
!!    leapyr      |none          |leap year flag:
!!                               |0  leap year
!!                               |1  regular year
!!    ncrops(:,:,:)|
!!    ncut(:)     |none          |sequence number of harvest operation within
!!                               |a year
!!    ndmo(:)     |days          |cumulative number of days accrued in the
!!                               |month since the simulation began where the
!!                               |array location number is the number of the
!!                               |month
!!    nro(:)      |none          |sequence number of year in rotation
!!    ntil(:)     |none          |sequence number of tillage operation within
!!                               |current year
!!    phu_plt(:)  |heat units    |total number of heat units to bring plant
!!                               |to maturity
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ic          |none          |counter
!!    idlst       |julian date   |last day of simulation in current year
!!    iix         |none          |sequence number of current year in rotation
!!    iiz         |none          |sequence number of current crop grown
!!                               |within the current year
!!    j           |none          |counter
!!    xx          |none          |current year in simulation sequence
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Mod, Real
!!    SWAT: sim_inityr, std3, xmon, sim_initday, clicon, command
!!    SWAT: writed, writem, tillmix

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: idlst, j, iix, iiz, ic, mon, ii
      real :: xx


      do curyr = 1, nbyr
        write (*,1234) curyr

        !! initialize annual variables
        call sim_inityr
       
        !! write header for watershed annual table in .std file
        call std3


        !!determine beginning and ending dates of simulation in current year
        if (Mod(iyr,4) == 0) then 
          leapyr = 0   !!leap year
        else 
          leapyr = 1   !!regular year
        end if

        !! set beginning day of simulation for year
        id1 = 0
        if (curyr == 1 .and. idaf > 0) then
          id1 = idaf
        else
          id1 = 1
        end if

        !! set ending day of simulation for year
        idlst = 0
        if (curyr == nbyr .and. idal > 0) then
          idlst = idal
        else
          idlst = 366 - leapyr
        end if
        
        !! set current julian date to begin annual simulation
        iida = 0
        iida = id1

        call xmon


        do i = id1, idlst                            !! begin daily loop

          !!if last day of month 
          if (i_mo /= mo_chk) then
            immo = immo + 1
          endif

          !! initialize variables at beginning of day
          call sim_initday
          !! added for Srini in output.mgt nitrogen and phosphorus nutrients per JGA by gsm 9/8/2011
                  
          sol_sumno3 = 0.
          sol_sumsolp = 0.
          do j = 1, mhru
            do ly = 1, sol_nly(j)
              sol_sumno3(j) = sol_sumno3(j) + sol_no3(ly,j) +
     *          sol_nh3(ly,j)
              sol_sumsolp(j) = sol_sumsolp(j) + sol_solp(ly,j)
            enddo
          enddo
 
          if ( fcstyr == iyr .and. fcstday == i) then
            ffcst = 1
            pcpsim = 2
            tmpsim = 2
            rhsim = 2
            slrsim = 2
            wndsim = 2
            igen = igen + iscen
            call gcycl
            do j = 1, subtot
              ii = 0
              ii = fcst_reg(j)
              if (ii <= 0) ii = 1
              do mon = 1, 12
                 tmpmx(mon,j) = 0.
                 tmpmn(mon,j) = 0.
                 tmpstdmx(mon,j) = 0.
                 tmpstdmn(mon,j) = 0.
                 pcp_stat(mon,1,j) = 0.
                 pcp_stat(mon,2,j) = 0.
                 pcp_stat(mon,3,j) = 0.
                 pr_w(1,mon,j) = 0.
                 pr_w(2,mon,j) = 0.
                 tmpmx(mon,j) = ftmpmx(mon,ii)
                 tmpmn(mon,j) = ftmpmn(mon,ii)
                 tmpstdmx(mon,j) = ftmpstdmx(mon,ii)
                 tmpstdmn(mon,j) = ftmpstdmn(mon,ii)
                 pcp_stat(mon,1,j) = fpcp_stat(mon,1,ii)
                 pcp_stat(mon,2,j) = fpcp_stat(mon,2,ii)
                 pcp_stat(mon,3,j) = fpcp_stat(mon,3,ii)
                 pr_w(1,mon,j) = fpr_w(1,mon,ii)
                 pr_w(2,mon,j) = fpr_w(2,mon,ii)
              end do
            end do
          end if

          dtot = dtot + 1.
          nd_30 = nd_30 + 1
          if (nd_30 > 30) nd_30 = 1

          if (curyr > nyskip) ndmo(i_mo) = ndmo(i_mo) + 1

          if (pcpsim < 3) call clicon      !! read in/generate weather

           !! call resetlu
           if (ida_lup(no_lup) == i .and. iyr_lup(no_lup) == iyr) then
              call resetlu
              no_lup = no_lup + 1
           end if

          call command              !! command loop


        do ihru = 1, nhru                                    
          if (idaf > 180 .and. sub_lat(hru_sub(ihru)) < 0) then
            if (i == 180) then
               if (mgtop(nop(ihru),ihru) /=17) then         
                  dorm_flag = 1
                  call operatn
                  dorm_flag = 0
               endif
                nop(ihru) = nop(ihru) + 1
          
                if (nop(ihru) > nopmx(ihru)) then
                  nop(ihru) = 1
                end if
      
              phubase(ihru) = 0.
	        yr_skip = 0
	      endif
	    
	    endif
        end do
        
	    !! write daily and/or monthly output
          if (curyr > nyskip) then
            call writed

          !! output.sol file
          if (isol == 1) call soil_write

            iida = i + 1
            call xmon
            call writem
          else
            iida = i + 1
            call xmon
          endif

        end do                                        !! end daily loop

        !! perform end-of-year processes
        do j = 1, nhru

          !! compute biological mixing at the end of every year

!          if (biomix(j) > .001) call tillmix (j,biomix(j))
          if (biomix(j) > .001) call newtillmix (j,biomix(j))

          !! update sequence number for year in rotation to that of
          !! the next year and reset sequence numbers for operations
          if (idplt(j) > 0) then
            if (idc(idplt(j)) == 7) then
              curyr_mat(j) = curyr_mat(j) + 1
              curyr_mat(j) = Min(curyr_mat(j),
     &                     mat_yrs(idplt(j)))
            end if
          end if

          !! update target nitrogen content of yield with data from
          !! year just simulated
          do ic = 1, mcr
            xx = 0.
            xx = Real(curyr)
            tnylda(j) = (tnylda(j) * xx + tnyld(j)) / (xx + 1.)
          end do

          if (idaf < 181) then
            if (mgtop(nop(j),j) /= 17) then
              dorm_flag = 1
              ihru = j
              call operatn
              dorm_flag = 0
            end if
            nop(j) = nop(j) + 1
          
            if (nop(j) > nopmx(j)) then
              nop(j) = 1
            end if
            
            phubase(j) = 0.
            yr_skip = 0
          endif

        end do

      !! update simulation year
      iyr = iyr + 1
      end do            !!     end annual loop

      return
 1234 format (1x,' Executing year ', i4)
      end
