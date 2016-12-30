      subroutine readfcst

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads the HRU forecast weather generator parameters
!!    from the .cst file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ndays(:)    |julian date   |julian date for last day of preceding
!!                               |month (where the array location is the
!!                               |number of the month) The dates are for
!!                               |leap years
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fpcp_stat(:,1,:)|mm/day     |average amount of precipitation falling in
!!                               |one day for the month
!!    fpcp_stat(:,2,:)|mm/day     |standard deviation for the average daily
!!                               |precipitation
!!    fpcp_stat(:,3,:)|none       |skew coefficient for the average daily 
!!                               |precipitation
!!    fpr_w(1,:,:) |none          |probability of wet day after dry day in month
!!    fpr_w(2,:,:) |none          |probability of wet day after wet day in month
!!    fpr_w(3,:,:) |none          |proportion of wet days in the month
!!    ftmpmn(:,:)  |deg C         |avg monthly minimum air temperature
!!    ftmpmx(:,:)  |deg C         |avg monthly maximum air temperature
!!    ftmpstdmn(:,:)|deg C        |standard deviation for avg monthly minimum air
!!                               |temperature
!!    ftmpstdmx(:,:)|deg C        |standard deviation for avg monthly maximum air
!!                               |temperature
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fcstregtot  |none          |total number of forecast regions defined
!!                               |in watershed
!!    i           |none          |forecast region number
!!    j           |none          |counter
!!    mdays       |none          |number of days in the month
!!    mon         |none          |monthly counter
!!    pcpmm(:)    |mm            |amount of precipitation in month
!!    pcpd(:)     |days          |average number of days of precipitation
!!                               |in the month
!!    titldum     |NA            |title line of .wgn file (not used elsewhere)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sin, Cos, Tan, Abs, Acos, Log, Exp, MaxVal
!!    SWAT: Aunif, Dstn1

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
      real, dimension (12) :: pcpmm, pcpd
      integer :: mon, mdays, j, fcstregtot


      fcstregtot = 0
      i = 0
      pcpd = 0.
      pcpmm = 0.

      read (109,5000) titldum
      read (109,5100) fcstregtot

      do j = 1, fcstregtot
        read (109,5000) titldum
        read (109,5100) i    !forecast region number
          if (i <= 0 ) i = 1
        read (109,5200) (ftmpmx(mon,i),mon = 1,12)
        read (109,5200) (ftmpmn(mon,i),mon = 1,12)
        read (109,5200) (ftmpstdmx(mon,i),mon = 1,12)
        read (109,5200) (ftmpstdmn(mon,i),mon = 1,12)
        read (109,5200) (pcpmm(mon),mon = 1,12)
        read (109,5200) (fpcp_stat(mon,2,i),mon = 1,12)  !pcpstd
        read (109,5200) (fpcp_stat(mon,3,i),mon = 1,12)  !pcpskw
        read (109,5200) (fpr_w(1,mon,i),mon = 1,12)
        read (109,5200) (fpr_w(2,mon,i),mon = 1,12)
        read (109,5200) (pcpd(mon),mon = 1,12)



!! calculate missing values and additional parameters
      do mon = 1, 12
        mdays = 0
        mdays = ndays(mon+1) - ndays(mon)

        !! calculate values for fpr_w if missing or bad
        if (fpr_w(2,mon,i) <= fpr_w(1,mon,i).or.fpr_w(1,mon,i) <= 0.)   
     &                                                              then
          if (pcpd(mon) < .1) pcpd(mon) = 0.1
          fpr_w(1,mon,i) = .75 * pcpd(mon) / mdays
          fpr_w(2,mon,i) = .25 + fpr_w(1,mon,i)
        else
        !! if fpr_w values good, use calculated pcpd based on these values
        !! using first order Markov chain
        pcpd(mon) = mdays * fpr_w(1,mon,i) /                            
     &                            (1. - fpr_w(2,mon,i) + fpr_w(1,mon,i))
    
        end if

        !! calculate precipitation-related values
        if (pcpd(mon) <= 0.) pcpd(mon) = .001
        fpr_w(3,mon,i) = pcpd(mon) / mdays
        fpcp_stat(mon,1,i) = pcpmm(mon) / pcpd(mon)
        if (fpcp_stat(mon,3,i) < 0.2) fpcp_stat(mon,3,i) = 0.2
      end do

      end do

      close (109)
      return
 5000 format (a)
 5100 format (i6)
 5200 format (12f6.2)
      end