      subroutine surface

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine models surface hydrology

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    ovrlnd(:)   |mm H2O        |overland flow onto HRU from upstream
!!                               |routing unit
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

      integer :: j
      j = 0
      j = ihru

      !! compute canopy interception
      if (idplt(nro(j),icr(j),j) > 0) then
        call canopyint
      end if

      !! compute snow melt
      call snom

      !! output by elevation band to snowband.out
      if (isnow == 1) then 
	 
         write(114,1010) i, iyr, j, (snoeb(ib,j), ib = 1,10)
      end if

      !! compute crack volume
      if (icrk == 1) call crackvol

      !! add overland flow from upstream routing unit
      precipday = precipday + ovrlnd(j)
      if (nstep > 0) then
        do ii = 1, 24
          hhprecip(ii) = hhprecip(ii) + ovrlnd(j) / 24.
        end do
        do ii = 1, nstep
          precipdt(ii+1) = precipdt(ii+1) + ovrlnd(j) / nstep
        end do
      end if

      !!calculate daily curve number value
      call dailycn

      if (precipday > 0.) then
        !! compute runoff - surfq in mm H2O
        call volq

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
      end if

      if (surfq(j) > 1.e-6 .and. peakr > 1.e-6) call ysed(0)

      if (qday < 0.) qday = 0.

1010  format(3(i4,1x),10f8.3)
      return
      end
