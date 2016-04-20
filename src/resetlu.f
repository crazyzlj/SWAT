      subroutine resetlu
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU/subbasin management input file
!!    (.mgt). This file contains data related to management practices used in
!!    the HRU/subbasin.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                 |urban.dat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                    |daily
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    eof         |none          |end of file flag (=-1 if eof, else = 0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Jdt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
	character(len=80) :: titldum

      open (9123,file=fname(no_lup))
      read (9123, 5101) titldum
      do j = 1, mhru
	   read (9123,*,end=99) hru, hru_fr(j)
	end do

!!    reset all hru_fr variables
	do j = 1, mhru
	 if (hru_fr(j) <= 0.) hru_fr(j) = .0000001
	     hru_km(j) = sub_km(hru_sub(j)) * hru_fr(j)
         hru_ha(j) = hru_km(j) * 100.  !MJW
         hru_dafr(j) = hru_km(j) / da_km  !MJW
	  do mon = 1, 12
          wupnd(mon,j) = wupnd(mon,j) * hru_fr(j)
	    wushal(mon,j) = wushal(mon,j) * hru_fr(j)
	    wudeep(mon,j) = wudeep(mon,j) * hru_fr(j)
	  end do
	  pnd_psa(j) = pnd_psa(j) * hru_fr(j)
	  pnd_esa(j) = pnd_esa(j) * hru_fr(j)
	  pnd_pvol(j) = pnd_pvol(j) * hru_fr(j)
	  pnd_evol(j) = pnd_evol(j) * hru_fr(j)
	  pnd_vol(j) = pnd_vol(j) * hru_fr(j)
	  wet_nsa(j) = wet_nsa(j) * hru_fr(j)
	  wet_mxsa(j) = wet_mxsa(j) * hru_fr(j)
	  wet_nvol(j) = wet_nvol(j) * hru_fr(j)
	  wet_mxvol(j) = wet_mxvol(j) * hru_fr(j)
	  wet_vol(j) = wet_vol(j) * hru_fr(j)
	  hru_ha(j) = hru_km(j) * 100.
!	  pot_vol(j) = 10. * pot_volmm(j) * hru_ha(j)   !! mm => m^3     NUBZ
	  pot_volx(j) = pot_volxmm(j)
	  pot_tile(j) = pot_tilemm(j) 
	end do

5101  format (a80)
99    return
      end