      subroutine lid_cistern(sb,j,k,lid_prec)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Simulate cistern processes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sb               |none          |Subbasin number
!!    j                |none          |HRU number
!!    k                |none          |Subdaily time index
!!    lid_prec         |mm            |Precipitation depth a LID receives in a simulation time interval
!!    idt              |minutes       |Simulation time interval for sub-daily modeling
!!    ihru             |none          |HRU number
!!    nstep            |none          |Number of time intervals for a day
!!    urblu(:)         |none          |Urban land type identification number from urban.dat
!!    lid_vol          |m3            |Volume of a cistern storage
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    lid_qsurf        |mm H2O        |Depth of runoff generated on a LID in a given time interval
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jj               |none          |Urban land type identification number from
!!                                    |urban.dat
!!    dt               |hour          |Time interval in hours
!!    lid_cumr         |mm H2O        |Cumulative amount of rainfall a LID receives in a time interval
!!    Cumulative amount of rainfall a LID receives in a time interval        |mm H2O        |Cumulative amount of excess rainfall at a time step in a day
!!    lid_exinc        |mm H2O        |Amount of excess rainfall
!!    lid_str          |m3            |Volume of stored water in the cistern
!!    lid_vbypass      |m3            |Volume of flow bypassing the cistern
!!    lid_bypass       |mm            |Depth of flow bypassing the cistern
!!    lid_irr          |m3            |Volume of water taken from the cistern for irrigation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    None

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use parm
      implicit none
      
      integer :: jj,sb,j,k,temp_count
      real*8 :: lid_str,lid_vbypass,lid_bypass,lid_irr,lid_vol,
     & lid_cumirr
      real :: lid_prec
      
      jj = urblu(j)
      
      lid_vol = cs_vol(sb,jj)         ! commented out for testing
      if (lid_vol <= 0) then
        lid_vol = (cs_rdepth(sb,jj) / 1000.) *
     &  (lid_farea(j,3) * fcimp(urblu(j)) * hru_ha(j) * 10000.)         ! for testing, assuming the storage can handle excess rainfall of 2.5 mm
      end if
      
      lid_str = lid_str_last(j,3)
      lid_cumirr = lid_cumirr_last(j,3)
      lid_bypass = 0.
      
      if (hrnopcp(sb,k-1) > 96) then ! four days
          lid_irr = 0.3 * lid_vol / nstep     ! assumming 30% of water storage of a cistern in a day
      else
          lid_irr = 0.
      end if
      
      if (cs_grcon(sb,jj)==0) then
        lid_str = lid_str_last(j,3) + (lid_prec / 1000.) *
     &  (lid_farea(j,3) * fcimp(urblu(j)) * hru_ha(j) * 10000.)           ! m3
      else
        lid_str = lid_str_last(j,3) + (lid_prec / 1000.) *
     &  (hru_ha(j) * 10000.)                                              ! m3
      end if
            
      if (lid_str > lid_vol) then
          lid_vbypass = lid_str - lid_vol ! assuming water stored in a cistern is used 
          lid_str = lid_vol
          lid_bypass = lid_vbypass / 
     & (hru_ha(j) * 10000.) * 1000.  ! mm
!     & (lid_farea(j,3) * fcimp(urblu(j)) * hru_ha(j) * 10000.) * 1000.  ! mm
      else
          lid_vbypass = 0.
      end if
      
      lid_qsurf(j,3) = lid_bypass
      
!! begin temporary
!      if (k == nstep+1) then
!     if (sb == 1) then
!        if (jj == 14) then
!        if (j == 1) then
!          write (3333333,'(5f12.4)') lid_prec,lid_str,lid_irr,
!     & lid_bypass,lid_qsurf(j,3)
!        end if
!      end if
!      end if
!! end temporary
        
      lid_str = lid_str - lid_irr
      if (lid_str < 0) then
        lid_str = 0.
        lid_irr = lid_str
      end if

      lid_cumirr = lid_cumirr + lid_irr
      
      if (k == nstep+1) then
          lid_sw_add(j,3) = lid_cumirr / (hru_ha(j) * 10000.) * 1000.
          lid_cumirr = 0.
      end if
      
      lid_cumirr_last(j,3) = lid_cumirr
      lid_str_last(j,3) = lid_str
        
      return
      end subroutine