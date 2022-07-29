      subroutine lid_cistern(sb,j,k,jj,lid_prec)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    Simulate cistern processes

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~ ~ ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sb               |none          |Subbasin number
!!    j                |none          |HRU number
!!    k                |none          |Subdaily time index
!!    jj               |none          |LID index in *.lid files
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
!!    dt               |hour          |Time interval in hours
!!    lid_cumr         |mm H2O        |Cumulative amount of rainfall a LID receives in a time interval
!!    Cumulative amount of rainfall a LID receives in a time interval   
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
      
      integer, intent(in) :: sb,j,k,jj
      real*8 :: lid_str,lid_vbypass,lid_bypass,lid_irr,lid_vol,
     & lid_cumirr
      real*8 :: lid_prec
      
	
      lid_vol = cs_vol(sb,jj)         ! m3 volume for this HRU
      lid_bypass = 0.
      
      if (cs_grcon(sb,jj)==0) then
        lid_str = lid_prec * lid_farea(j,3) * fcimp(urblu(j)) 
     &	* hru_ha(j) * 10.           ! m3
      else
        lid_str = lid_prec * hru_ha(j) * 10.    ! m3
      end if
	
      if (lid_str_last(j,3)+lid_str > lid_vol) then
          lid_vbypass = lid_str_last(j,3) + lid_str - lid_vol  
          lid_str_last(j,3) = lid_vol
		lid_str = max(0.,lid_str - lid_vbypass)
          lid_bypass = lid_vbypass / (10. * hru_ha(j))   ! m3 to mm
      else
          lid_vbypass = 0.
		lid_str_last(j,3) = lid_str_last(j,3) + lid_str !Current storage volume, m3
      end if
      
      lid_str_curday(j,3) = lid_str_curday(j,3) + lid_str !cumulative storage volume for the day, m3
	lid_qsurf(j,3) = lid_bypass !mm
	lid_qsurf_curday(j,3) = lid_qsurf_curday(j,3) + lid_qsurf(j,3) !mm

      return
      end subroutine