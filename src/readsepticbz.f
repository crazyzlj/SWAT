      subroutine readsepticbz

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the septic input file (.sep).  This file
!!    contains information related to septic tanks modeled or defined at the 
!!    watershed level

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru             |none          |HRU number

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bz_z(:)          |mm            |Depth of biozone layer
!!    bz_thk(:)        |mm            |thickness of biozone                    
!!    bio_bd(:)        |kg/m^3        |density of biomass 
!!    coeff_bod_dc(:)  |m^3/day       |BOD decay rate coefficient
!!    coeff_bod_conv(:)|none          |BOD to live bacteria biomass conversion factor
!!    coeff_denitr(:)  |none          |Denitrification rate coefficient
!!    coeff_fc1(:)     |none          |field capacity calibration parameter 1
!!    coeff_fc2(:)     |none          |field capacity calibration parameter 2  
!!    coeff_fecal(:)   |m^3/day       |Fecal coliform bacteria decay rate coefficient  
!!    coeff_mrt(:)     |none          |mortality rate coefficient          
!!    coeff_nitr(:)    |none          |Nitrification rate coefficient
!!    coeff_plq(:)     |none          |Conversion factor for plaque from TDS           
!!    coeff_rsp(:)     |none          |respiration rate coefficient          
!!    coeff_slg1(:)    |none          |slough-off calibration parameter
!!    coeff_slg2(:)    |none          |slough-off calibration parameter
!!    sep_cap(:)      |none          |Number of permanent residents in the hourse                 
!!    isep_typ(:)      |none          |Septic system type                 
!!    isep_opt(:)      |none          |Septic system operation flag (1=active,2=failing,3=not operated)                 
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name             |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof              |none          |end of file flag (=-1 if eof, else =0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
	integer :: eof

!!    initialize variables
      eof = 0


!! read septic parameters
      do
        read (172,1000) titldum
	  read (172,*,iostat=eof) isep_typ(ihru)
	  if (eof < 0) exit
        if (isep_typ(ihru) <= 0) return
	  read (172,*,iostat=eof) isep_iyr(ihru)
	  if (eof < 0) exit
	  read (172,*,iostat=eof) isep_opt(ihru)       
	  if (eof < 0) exit
	  read (172,*,iostat=eof) sep_cap(ihru)
	  if (eof < 0) exit
	  read (172,*,iostat=eof) bz_area(ihru)
	  if (eof < 0) exit
	  read (172,*,iostat=eof) isep_tfail(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) bz_z(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) bz_thk(ihru)
        if (eof < 0) exit
        read (172,*,iostat=eof) sep_strm_dist(ihru)
        if (eof < 0) exit
        read (172,*,iostat=eof) sep_den(ihru)
        if (eof < 0) exit
        read (172,*,iostat=eof) bio_bd(ihru)
        if (eof < 0) exit
        read (172,*,iostat=eof) coeff_bod_dc(ihru)
	  if (eof < 0) exit   
        read (172,*,iostat=eof) coeff_bod_conv(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_fc1(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_fc2(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_fecal(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_plq(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_mrt(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_rsp(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_slg1(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_slg2(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_nitr(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_denitr(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_pdistrb(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_psorpmax(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_solpslp(ihru)
	  if (eof < 0) exit
        read (172,*,iostat=eof) coeff_solpintc(ihru)
	  exit
	end do

	coeff_mrt(ihru) = 0.01 * coeff_mrt(ihru)
	coeff_rsp(ihru) = 0.01 * coeff_rsp(ihru)
	coeff_slg1(ihru) = 0.001 * coeff_slg1(ihru)
	coeff_nitr(ihru) = 0.01 * coeff_nitr(ihru)
	coeff_denitr(ihru) = 0.01 * coeff_denitr(ihru)

	!!Convert QSTE from volume to depth unit, mm
	qstemm(ihru) = sptqs(isep_typ(ihru)) * sep_cap(ihru) / 
     &	bz_area(ihru) * 1000.

!!    set default values for undefined parameters
      if (isep_iyr(ihru)==0) isep_iyr(ihru) = iyr
      if (bz_z(ihru) <= 1.e-6) bz_z(ihru) = 500.
      if (bz_thk(ihru) <= 1.e-6) bz_thk(ihru) = 20.
      if (bio_bd(ihru) <= 1.e-6) bio_bd(ihru) = 1000.
      if (coeff_bod_dc(ihru) <= 1.e-6) coeff_bod_dc(ihru) = 9.33
      if (coeff_bod_conv(ihru) <= 1.e-6) coeff_bod_conv(ihru) = 0.42
      if (coeff_fc1(ihru) <= 1.e-6) coeff_fc1(ihru) = 30.0
      if (coeff_fc2(ihru) <= 1.e-6) coeff_fc2(ihru) = 0.7
      if (coeff_fecal(ihru) <= 1.e-6) coeff_fecal(ihru) = 0.11
      if (coeff_plq(ihru) <= 1.e-6) coeff_plq(ihru) = 0.10
      if (coeff_mrt(ihru) <= 1.e-6) coeff_mrt(ihru) = 0.025
      if (coeff_rsp(ihru) <= 1.e-6) coeff_rsp(ihru) = 0.0156
      if (coeff_slg1(ihru) <= 1.e-6) coeff_slg1(ihru) = 4. e-8
      if (coeff_slg2(ihru) <= 1.e-6) coeff_slg2(ihru) = 1.5
      if (coeff_nitr(ihru) <= 1.e-6) coeff_nitr(ihru) = 0.086
      if (coeff_denitr(ihru) <= 1.e-6) coeff_denitr(ihru) = 0.00432

      
      close (172)
1000  format (a)
      return
      end