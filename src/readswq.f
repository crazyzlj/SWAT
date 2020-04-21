      subroutine readswq

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads parameters from the subbasin instream water 
!!    quality file (.swq) and initializes the QUAL2E variables which apply to
!!    the individual subbasins

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |reach number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    bc1(:)      |1/day or 1/hr |rate constant for biological oxidation of NH3
!!                               |to NO2 in reach at 20 deg C
!!    bc2(:)      |1/day or 1/hr |rate constant for biological oxidation of NO2
!!                               |to NO3 in reach at 20 deg C
!!    bc3(:)      |1/day or 1/hr |rate constant for hydrolysis of organic N to
!!                               |ammonia in reach at 20 deg C
!!    bc4(:)      |1/day or 1/hr |rate constant for the decay of organic P to
!!                               |dissolved P in reach at 20 deg C
!!    chpst_koc(:)  |m**3/g      |pesticide partition coefficient between
!!                               |water and sediment in reach
!!    chpst_mix(:)  |m/day       |mixing velocity (diffusion/dispersion) for
!!                               |pesticide in reach
!!    chpst_rea(:)  |1/day       |pesticide reaction coefficient in reach
!!    chpst_rsp(:)  |m/day       |resuspension velocity in reach for pesticide
!!                               |sorbed to sediment
!!    chpst_stl(:)  |m/day       |settling velocity in reach for pesticide
!!                               |sorbed to sediment
!!    chpst_vol(:)  |m/day       |pesticide volatilization coefficient in reach
!!    rk1(:)      |1/day or 1/hr |CBOD deoxygenation rate coefficient in reach 
!!                               |at 20 deg C
!!    rk2(:)      |1/day or 1/hr |reaeration rate in accordance with Fickian
!!                               |diffusion in reach at 20 deg C
!!    rk3(:)      |1/day or 1/hr |rate of loss of CBOD due to settling in reach
!!                               |at 20 deg C
!!    rk4(:)      |mg O2/        |sediment oxygen demand rate in reach
!!                |  ((m**2)*day)|at 20 deg C
!!                |or mg O2/((m**2)*hr)
!!    rk5(:)      |1/day         |coliform die-off rate in reach
!!    rk6(:)      |1/day         |decay rate for arbitrary non-conservative
!!                               |constituent in reach
!!    rs1(:)      |m/day or m/hr |local algal settling rate in reach at 20 deg C
!!    rs2(:)      |(mg disP-P)/  |benthos source rate for dissolved phosphorus
!!                |  ((m**2)*day)|in reach at 20 deg C
!!                |or (mg disP-P)/((m**2)*hr)|
!!    rs3(:)      |(mg NH4-N)/   |benthos source rate for ammonia nitrogen in
!!                |  ((m**2)*day)|reach at 20 deg C
!!                |or (mg NH4-N)/((m**2)*hr)|
!!    rs4(:)      |1/day or 1/hr |rate coefficient for organic nitrogen 
!!                               |settling in reach at 20 deg C
!!    rs5(:)      |1/day or 1/hr |organic phosphorus settling rate in reach at
!!                               |20 deg C
!!    rs6(:)      |1/day         |rate coefficient for settling of arbitrary 
!!                               |non-conservative constituent in reach
!!    rs7(:)      |(mg ANC)/     |benthal source rate for arbitrary 
!!                   ((m**2)*day)|non-conservative constituent in reach
!!    sedpst_act(:) |m           |depth of active sediment layer in reach for
!!                               |pesticide
!!    sedpst_bry(:) |m/day       |pesticide burial velocity in river bed
!!                               |sediment
!!    sedpst_conc(:)|mg/(m**3)   |inital pesticide concentration in river bed
!!                               |sediment
!!    sedpst_rea(:) |1/day       |pesticide reaction coefficient in river bed
!!                               |sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    eof         |none          |end of file flag
!!    titldum     |NA            |title line in .wq file (not used)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
      integer :: eof

      eof = 0

      do
      read (104,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (104,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (104,*,iostat=eof) rs1(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs2(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs3(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs4(i)
      if (eof < 0) exit
      read (104,*,iostat=eof) rs5(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs6(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs7(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk1(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk2(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk3(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk4(i)
      if (eof < 0) exit
      read (104,*,iostat=eof) rk5(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk6(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) bc1(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) bc2(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) bc3(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) bc4(i)
      if (eof < 0) exit
      read (104,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_rea(i)
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_vol(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_koc(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_stl(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_rsp(i)
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_mix(i)
      if (eof < 0) exit
      read (104,*,iostat=eof) sedpst_conc(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) sedpst_rea(i)
      if (eof < 0) exit
      read (104,*,iostat=eof) sedpst_bry(i) 
      if (eof < 0) exit
      read (104,*,iostat=eof) sedpst_act(i)
      exit
      end do

!!    set default values for undefined parameters
      if (rs1(i) <= 0.) rs1(i) = 1.0
      if (rs2(i) <= 0.) rs2(i) = 0.05
      if (rs3(i) <= 0.) rs3(i) = 0.5
      if (rs4(i) <= 0.) rs4(i) = 0.05
      if (rs5(i) <= 0.) rs5(i) = 0.05
      if (rs6(i) <= 0.) rs6(i) = 2.5
      if (rs7(i) <= 0.) rs7(i) = 2.5
      if (rk1(i) <= 0.) rk1(i) = 1.71
      if (rk2(i) <= 0.) rk2(i) = 50.0
      if (rk4(i) <= 0.) rk4(i) = 2.0
      if (rk5(i) <= 0.) rk5(i) = 2.0
      if (rk6(i) <= 0.) rk6(i) = 1.71
      if (bc1(i) <= 0.) bc1(i) = 0.55 
      if (bc2(i) <= 0.) bc2(i) = 1.1
      if (bc3(i) <= 0.) bc3(i) = 0.21
      if (bc4(i) <= 0.) bc4(i) = 0.35
      if (chpst_rea(i) <= 1.e-6) chpst_rea(i) = 0.007
      if (chpst_vol(i) <= 1.e-6) chpst_vol(i) = 0.01
      if (chpst_koc(i) <= 1.e-12) chpst_koc(i) = 0.
      if (chpst_stl(i) <= 1.e-6) chpst_stl(i) = 1.
      if (chpst_rsp(i) <= 1.e-6) chpst_rsp(i) = 0.002
      if (chpst_mix(i) <= 1.e-6) chpst_mix(i) = 0.001
      if (sedpst_conc(i) <= 1.e-6) sedpst_conc(i) = 0.
      if (sedpst_rea(i) <= 1.e-6) sedpst_rea(i) = 0.05
      if (sedpst_bry(i) <= 1.e-6) sedpst_bry(i) = 0.002
      if (sedpst_act(i) <= 1.e-6) sedpst_act(i) = 0.030

!!  set default values for mike van liew
      if (bc1(i) <= 0.) bc1(i) = bc1_bsn
      if (bc2(i) <= 0.) bc2(i) = bc2_bsn
      if (bc3(i) <= 0.) bc3(i) = bc3_bsn
      if (bc4(i) <= 0.) bc4(i) = bc4_bsn
!!  set default values for mike van liew

!! change units from day to hour if hourly routing is performed
      if (ievent > 2) then
        rs1(i) = rs1(i) / 24.
        rs2(i) = rs2(i) / 24.
        rs3(i) = rs3(i) / 24.
        rs4(i) = rs4(i) / 24.
        rs5(i) = rs5(i) / 24.
        rk1(i) = rk1(i) / 24.
        rk2(i) = rk2(i) / 24.
        rk3(i) = rk3(i) / 24.
        rk4(i) = rk4(i) / 24.
        bc1(i) = bc1(i) / 24.
        bc2(i) = bc2(i) / 24.
        bc3(i) = bc3(i) / 24.
        bc4(i) = bc4(i) / 24.
      end if

      close (104)
      return
 5100 format (a)
      end
