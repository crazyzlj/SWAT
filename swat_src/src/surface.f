      subroutine surface

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine models surface hydrology at any desired time step

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    ovrlnd(:)   |mm H2O        |overland flow onto HRU from upstream
!!                               |routing unit
!!    nstep       |none          |Number of time intervals for a day
!!    peakr       |mm/hr         |peak runoff rate
!!    precipday   |mm H2O        |effective precipitation for the day in HRU
!!    qday        |mm H2O        |surface runoff loading to main channel
!!                               |for day
!!    surfq(:)    |mm H2O        |surface runoff generated in HRU during
!!                               |the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    precipday   |mm H2O        |effective precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: canopyint, snom, crackvol, dailycn, volq, crackflow, surfst_h2o,
!!    SWAT: alph, pkq, tran, eiusle, ysed

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer :: j,sb,kk, ii, ib
      real :: precip_fr
      real :: irfr  !! the irrigation area fraction of subbasin irrigation area
      real :: hruvirr,hruirrday

      j = 0
      j = ihru
      sb = hru_sub(j) ! subbasin in which HRU is located
      hruirrday = 0.
      irmmdt = 0.

      !! compute canopy interception
      if (idplt(j) > 0 .AND. precipday > 0.) then
        call canopyint
      end if

      !! compute snow melt
      call snom

      !! output by elevation band to output.snw
      if (isnow == 1) then    
        write(115,1010) i, iyr, subnum(j), hruno(j),                   
     &                (snoeb(ib,j), ib = 1,10)
      end if
      
      if (isnow ==1) then
        write (116,1010) i, iyr, subnum(j), hruno(j),
     &    (tavband(ib,j), ib = 1, 10)
      end if
      
      !! compute crack volume
      if (icrk == 1) call crackvol

      !! add overland flow from upstream routing unit
      !! currently, ovrlnd(j) is always 0. commented by ljzhu
      precipday = precipday + ovrlnd(j)
      if (nstep > 0) then
        do ii = 1, nstep
          precipdt(ii+1) = precipdt(ii+1) + ovrlnd_dt(j,ii)
        end do
      end if
      
      !! add irrigation from retention-irrigation ponds to soil water
      !! irmmdt(ii) first satisfy soil water to sol_ul, then store the remainer,
      !! and, the irmmdt(ii) will not be used else where. commented by ljzhu
      if (ri_luflg(j)==1) then
        irfr = hru_km(j)* (1.-fimp(urblu(j))) / ri_subkm(sb) 
        do ii=1,nstep
          !amount irrigated in hru
          hruvirr = ri_totpvol(ii) * irfr !m3
          irmmdt(ii) = hruvirr / (hru_km(j) 
     &       * (1.- fimp(urblu(j))) * 1000.) !mm/dt
          
          !add irrigated water to soil water content
          do kk=1,sol_nly(j)
            if(irmmdt(ii)<sol_ul(kk,j)-sol_st(kk,j)) then
               sol_st(kk,j) = sol_st(kk,j) + irmmdt(ii)
               exit
            else
               sol_st(kk,j) = sol_ul(kk,j)
               irmmdt(ii) = irmmdt(ii) - (sol_ul(kk,j)-sol_st(kk,j))
            end if
          end do
         
        end do
      end if

      !!calculate subdaily curve number value
      call dailycn

      !! compute runoff - surfq in mm H2O
      if (precipday > 0.1) then
        call volq 
        !! bmp adjustment
        surfq(j) = surfq(j) * bmp_flo(j)
        !! adjust runoff for loss into crack volume
        if (surfq(j) > 0. .and. icrk == 1) call crackflow
      end if

      surfq(j) = surfq(j) + qird(j)
      qird(j) = 0.

      !! calculate amount of surface runoff reaching main channel during day
      !! (qday) and store the remainder
      call surfst_h2o

      !! calculate half-hour rainfall
      if (precipday > 0.01) call alph(0)

      if (qday > 0.0001) then
        !! compute peak rate - peakr in m3/s  
        call pkq(0)  
      end if  

      if (qday > 0.0001 .and. peakr > 0.) then
        !! compute transmission losses for non-HUMUS datasets
        call tran
        call eiusle

        !! calculate sediment erosion by rainfall and overland flow
        call ovr_sed
      end if

      call cfactor
      if (surfq(j) > 1.e-6 .and. peakr > 1.e-6) call ysed(0)

      if (qday < 0.) qday = 0.

1010  format (2(i4,1x),a5,a4,1x,10f8.1)
      return
      end