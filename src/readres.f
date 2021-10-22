      subroutine readres
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    the purpose of this subroutine is to read in data from the reservoir
!!    input file (.res)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i           |none          |reservoir number
!!    nbyr        |none          |number of calendar years simulated
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    br1(:)       |none          |1st shape parameter for reservoir surface area
!!                                |equation
!!    br2(:)       |none          |2nd shape parameter for reservoir surface area
!!                                |equation
!!    evrsv(:)     |none          |lake evaporation coefficient
!!    iflod1r(:)   |none          |beginning month of non-flood season
!!                                |(needed if IRESCO=2)
!!    iflod2r(:)   |none          |ending month of non-flood season
!!                                |(needed if IRESCO=2)
!!    iresco(:)    |none          |outflow simulation code:
!!                                |0 compute outflow for uncontrolled reservoir
!!                                |  with average annual release rate
!!                                |1 measured monthly outflow
!!                                |2 simulated controlled outflow-target release
!!                                |3 measured daily outflow
!!                                |4 stage/volume/outflow relationship                                  
!!    iyres(:)     |none          |year of the simulation that the reservoir 
!!                                |becomes operational
!!    mores(:)     |none          |month the reservoir becomes operational
!!    ndtargr(:)   |days          |number of days to reach target storage from
!!                                |current reservoir storage
!!                                |(needed if IRESCO=2)
!!    oflowmn(:,:) |m^3/day       |minimum daily ouflow for the month (read in as
!!                                |m^3/s and converted to m^3/day)
!!    oflowmx(:,:) |m^3/day       |maximum daily ouflow for the month (read in as
!!    oflowmn_fps  |fraction      |minimum reservoir outflow as a fraction of
!!                                | the principal spillway volume (0-1)
!!                                |m^3/s and converted to m^3/day)
!!    res_esa(:)   |ha            |reservoir surface area when reservoir is
!!                                |filled to emergency spillway 
!!    res_evol(:)  |m**3          |volume of water needed to fill the reservoir
!!                                |to the emergency spillway (read in as 10^4 m^3
!!                                |and converted to m^3)
!!    res_k(:)     |mm/hr         |hydraulic conductivity of the reservoir bottom
!!    res_nsed(:)  |kg/L          |normal amount of sediment in reservoir (read
!!                                |in as mg/L and convert to kg/L)
!!    res_psa(:)   |ha            |reservoir surface area when reservoir is
!!                                |filled to principal spillway
!!    res_pvol(:)  |m**3          |volume of water needed to fill the reservoir
!!                                |to the principal spillway (read in as 10^4 m^3
!!                                |and converted to m^3)
!!    res_rr(:)    |m**3/day      |average daily principal spillway release
!!                                |volume (read in as a release rate in m^3/s and
!!                                |converted to m^3/day)
!!    res_sed(:)   |kg/L          |amount of sediment in reservoir (read in as
!!                                |mg/L and converted to kg/L)
!!    res_sub(:)   |none          |number of subbasin reservoir is in (weather
!!                                |for the subbasin is used for the reservoir)
!!    res_vol(:)   |m**3          |reservoir volume (read in as 10^4 m^3 and
!!                                |converted to m^3)
!!    res_out(:,:,:)|m**3/day      |measured average daily outflow from the 
!!                                |reservoir for the month (needed if IRESCO=1)
!!                                |(read in as m**3/s and converted to m**3/day)
!!    sedstlr(:)   |none          |sediment settling rate 
!!    starg(:,:)   |m**3          |monthly target reservoir storage (needed if
!!                                |IRESCO=2) (read in as 10^4 m^3 and converted
!!                                |to m^3)
!!    starg_fps    |fraction      |target volume as a fraction of the principal
!!                                |spillway volume (.1-5)
!!    wshd_resfr
!!    wshd_ressed  |metric tons   |total amount of suspended sediment in 
!!                                |reservoirs in the watershed
!!    wshd_resv    |m**3          |total volume of water in all reservoirs in 
!!                                |the watershed
!!    wuresn(:,:)  |m**3          |average amount of water withdrawn from 
!!                                |reservoir each month for consumptive water use
!!                                |(read in as 10^4 m^3 and converted to m^3)
!!    wurtnf(:)    |none          |fraction of water removed from the reservoir
!!                                |via WURESN which is returned and becomes flow
!!                                |from the reservoir outlet
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag
!!    j           |none          |counter
!!    lnvol       |none          |variable to hold denominator value
!!    mon         |none          |counter
!!    titldum     |NA            |title line in .res file (not used in program)
!!    res_d50     |micrometers   |median particle diameter of sediment
!!    resdayo     |NA            |name of daily reservoir outflow file
!!                               |(needed if IRESCO = 3)
!!    resdif      |m^3 H2O       |difference in volume held in reservoir at
!!                               |emergency spillway and at principal
!!                               |spillway
!!    resmono     |NA            |name of monthly reservoir outflow file
!!                               |(needed if IRESCO = 1)
!!    targ        |10^4 m^3 H2O  |target reservoir volume
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: readlwq, caps
!!    Intrinsic: Log10

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
      character (len=13) :: resdayo, resmono
      integer :: eof, mon, j
      real*8 :: resdif, targ, lnvol, res_d50

!!    initialize local variables
      resdayo = ""
      resmono = ""
      eof = 0
      res_d50 = 0.

!!    read in data from .res file
      do
        read (105,1000,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) res_sub(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) mores(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) iyres(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) res_esa(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) res_evol(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) res_psa(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) res_pvol(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) res_vol(i) 
        if (eof < 0) exit
        read (105,*,iostat=eof) res_sed(i) 
        if (eof < 0) exit
        read (105,*,iostat=eof) res_nsed(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) res_d50
        if (eof < 0) exit
        read (105,*,iostat=eof) res_k(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) iresco(i)
        if (eof < 0) exit
        read (105,1000,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) (oflowmx(mon,i), mon = 1, 6)
        if (eof < 0) exit
        read (105,1000,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) (oflowmx(mon,i), mon = 7, 12)
        if (eof < 0) exit
        read (105,1000,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) (oflowmn(mon,i), mon = 1, 6)
        if (eof < 0) exit
        read (105,1000,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) (oflowmn(mon,i), mon = 7, 12)
        if (eof < 0) exit
        read (105,*,iostat=eof) res_rr(i)
        if (eof < 0) exit
        read (105,1100,iostat=eof) resmono
        if (eof < 0) exit
        read (105,*,iostat=eof) iflod1r(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) iflod2r(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) ndtargr(i)
        if (eof < 0) exit
        read (105,1000,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) (starg(mon,i), mon = 1, 6)
        if (eof < 0) exit
        read (105,1000,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) (starg(mon,i), mon = 7, 12)
        if (eof < 0) exit
        read (105,1100,iostat=eof) resdayo
        if (eof < 0) exit
        read (105,1000,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) (wuresn(mon,i), mon = 1, 6)
        if (eof < 0) exit
        read (105,1000,iostat=eof) titldum
        if (eof < 0) exit
        read (105,*,iostat=eof) (wuresn(mon,i), mon = 7, 12)
        if (eof < 0) exit
        read (105,*,iostat=eof) wurtnf(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) evrsv(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) oflowmn_fps(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) starg_fps(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) nostep
        if (eof < 0) exit
        read (105,*,iostat=eof) weirc(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) weirk(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) weirw(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) acoef(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) bcoef(i)
        if (eof < 0) exit
        read (105,*,iostat=eof) ccoef(i)
        if (eof < 0) exit
!! code added for C. Ikenberry to swicth from new (1) to old (0) equations in resnut.f
        read (105,*,iostat=eof) ires_nut
        if (eof < 0) exit
        exit
      end do

!!    set default values
      if (res_sub(i) <= 0) res_sub(i) = 1
      if (ndtargr(i) <= 0) ndtargr(i) = 15
      if (res_d50 <= 0) res_d50 = 10.
      if (nostep <= 0) nostep = 24
      if (weirc(i) <= 0) weirc(i) = 1.
      if (weirk(i) <= 0) weirk(i) = 150000.0
      if (weirw(i) <= 0) weirw(i) = 2.0 
      if (acoef(i) <= 0) acoef(i) = 1.0
      if (bcoef(i) <= 0) bcoef(i) = 1.75
      if (ccoef(i) <= 0) ccoef(i) = 1.0
      if (res_pvol(i) + res_evol(i) > 0.) then
        if (res_pvol(i) <= 0) res_pvol(i) = 0.9 * res_evol(i)
      else
        if (res_pvol(i) <= 0) res_pvol(i) = 60000.0
      end if
      if (res_evol(i) <= 0.0) res_evol(i) = 1.11 * res_pvol(i)     	
      if (res_psa(i) <= 0.0) res_psa(i) = 0.08 * res_pvol(i)
      if (res_esa(i) <= 0.0) res_esa(i) = 1.5 * res_psa(i) 
      targ = 0.
      targ = res_pvol(i) + 0.1 * (res_evol(i) - res_pvol(i))
      if (res_vol(i) < 0.0) res_vol(i) = 0.0
      !if (res_vol(i) > targ ) res_vol(i) = targ
      if (evrsv(i) <= 0.) evrsv(i) = 0.6
     
     
!!    convert units
      res_evol(i) = res_evol(i) * 10000.          !! 10**4 m**3 => m**3
      res_pvol(i) = res_pvol(i) * 10000.          !! 10**4 m**3 => m**3
      res_vol(i) = res_vol(i) * 10000.            !! 10**4 m**3 => m**3
      res_rr(i) = res_rr(i) * 86400.              !! m**3/s => m**3/day
      res_sed(i) = res_sed(i) * 1.e-6             !! mg/L => ton/m^3
      res_d50mm = res_d50 / 1000.                 !! micrometers to millimeters

	res_san(i) = res_sed(i) * 0. 
	res_sil(i) = res_sed(i) * 1. 
	res_cla(i) = res_sed(i) * 0. 
	res_sag(i) = res_sed(i) * 0. 
	res_lag(i) = res_sed(i) * 0. 
	res_gra(i) = 0.

      res_nsed(i) = res_nsed(i) * 1.e-6           !! mg/L => ton/m^3

      velsetlr(i) = 24. * 411. * res_d50mm ** 2.

      do mon = 1, 12
        wuresn(mon,i) = wuresn(mon,i) * 10000.    !! 10**4 m**3 => m**3
        starg(mon,i) = starg(mon,i) * 10000.      !! 10**4 m**3 => m**3
        oflowmx(mon,i) = oflowmx(mon,i) * 86400.  !! m**3/s => m**3/day
        oflowmn(mon,i) = oflowmn(mon,i) * 86400.  !! m**3/s => m**3/day
      end do

      if (starg_fps(i) < 1.e-6) starg_fps(i) = 1.
      starg_fps(i) = starg_fps(i) * res_pvol(i)
      oflowmn_fps(i) = oflowmn_fps(i) * res_pvol(i)

!     if (isproj == 2) iresco(i) = 4        !! NK hided Oct. 4, 2006
      
!!    open daily reservoir outflow file 
      if (iresco(i) == 3) then
        call caps(resdayo)
        open (350+i,file=resdayo)
        read (350+i,1000) titldum
      end if

!!    initialize watershed reservoir parameters
      wshd_resfr = wshd_resfr + 1.   !!need to check this and modify (??)
      wshd_resv = wshd_resv + res_vol(i)
      wshd_ressed = wshd_ressed + res_vol(i) * res_sed(i)

!!    calculate shape parameters for surface area equation
      resdif = 0.
      resdif = res_evol(i) - res_pvol(i)
      if ((res_esa(i) - res_psa(i)) > 0. .and. resdif > 0.) then	
      lnvol = 0.
      lnvol = Log10(res_evol(i)) - Log10(res_pvol(i))
      if (lnvol > 1.e-4) then
        br2(i) = (Log10(res_esa(i)) - Log10(res_psa(i))) / lnvol
      else 
        br2(i) = (Log10(res_esa(i)) - Log10(res_psa(i))) / 0.001
      end if
        if (br2(i) > 0.9) then
          br2(i) = 0.9
          br1(i) = res_psa(i)/(res_pvol(i) ** 0.9)
        else
          br1(i) = res_esa(i)/(res_evol(i) ** br2(i))
        end if  
      else
        br2(i) = 0.9
        br1(i) = res_psa(i)/(res_pvol(i) ** 0.9)
      end if

!! calculate sediment settling rate
      if(ievent== 0) then
	  sed_stlr(i) = Exp(-.184 * res_d50)
	else
	  sed_stlr(i) = Exp(-.184 * res_d50 / nstep)	!! urban modeling by J.Jeong
	endif
!!     xx = res_stlr_co * res_d50
!!	if (xx > 20.) xx = 20.
!!    sed_stlr(i) = Exp(-xx)

!! read in monthly release data
      if (iresco(i) == 1) then
        call caps(resmono)
        open (101,file=resmono)
        read (101,1000) titldum
          do j = 1, nbyr+2
            read (101,*,iostat=eof) (res_out(i,mon,j), mon = 1, 12)
            if (eof < 0) exit
            do mon = 1, 12
              !! convert m**3/s => m**3/day
              res_out(i,mon,j) = res_out(i,mon,j) * 86400.
            end do
          end do
        close (101)
      end if

      close (105)

      return
 1000 format (a80)
 1100 format (a13)
      end