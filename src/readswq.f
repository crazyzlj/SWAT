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
      read (104,*,iostat=eof) rs1(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs2(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs3(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs4(irch)
      if (eof < 0) exit
      read (104,*,iostat=eof) rs5(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs6(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rs7(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk1(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk2(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk3(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk4(irch)
      if (eof < 0) exit
      read (104,*,iostat=eof) rk5(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) rk6(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) bc1(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) bc2(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) bc3(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) bc4(irch)
      if (eof < 0) exit
      read (104,5100,iostat=eof) titldum
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_rea(irch)
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_vol(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_koc(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_stl(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_rsp(irch)
      if (eof < 0) exit
      read (104,*,iostat=eof) chpst_mix(irch)
      if (eof < 0) exit
      read (104,*,iostat=eof) sedpst_conc(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) sedpst_rea(irch)
      if (eof < 0) exit
      read (104,*,iostat=eof) sedpst_bry(irch) 
      if (eof < 0) exit
      read (104,*,iostat=eof) sedpst_act(irch)
      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_mumax(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_kinv(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_klw(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_kla(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_cdet(irch)
!      if (eof < 0) exit
!      read (104,*,iostat=eof) biofilm_bm(irch)
      exit
      end do

!!    set default values for undefined parameters
      if (rs1(irch) <= 0.) rs1(irch) = 1.0
      if (rs2(irch) <= 0.) rs2(irch) = 0.05
      if (rs3(irch) <= 0.) rs3(irch) = 0.5
      if (rs4(irch) <= 0.) rs4(irch) = 0.05
      if (rs5(irch) <= 0.) rs5(irch) = 0.05
      if (rs6(irch) <= 0.) rs6(irch) = 2.5
      if (rs7(irch) <= 0.) rs7(irch) = 2.5
      if (rk1(irch) <= 0.) rk1(irch) = 1.71
      if (rk2(irch) <= 0.) rk2(irch) = 1.0    ! previous 50.0
      if (rk4(irch) <= 0.) rk4(irch) = 2.0
      if (rk5(irch) <= 0.) rk5(irch) = 2.0
      if (rk6(irch) <= 0.) rk6(irch) = 1.71
      if (bc1(irch) <= 0.) bc1(irch) = 0.55 
      if (bc2(irch) <= 0.) bc2(irch) = 1.1
      if (bc3(irch) <= 0.) bc3(irch) = 0.21
      if (bc4(irch) <= 0.) bc4(irch) = 0.35
      if (chpst_rea(irch) <= 1.e-6) chpst_rea(irch) = 0.007
      if (chpst_vol(irch) <= 1.e-6) chpst_vol(irch) = 0.01
      if (chpst_koc(irch) <= 1.e-6) chpst_koc(irch) = 0.
      if (chpst_stl(irch) <= 1.e-6) chpst_stl(irch) = 1.
      if (chpst_rsp(irch) <= 1.e-6) chpst_rsp(irch) = 0.002
      if (chpst_mix(irch) <= 1.e-6) chpst_mix(irch) = 0.001
      if (sedpst_conc(irch) <= 1.e-6) sedpst_conc(irch) = 0.
      if (sedpst_rea(irch) <= 1.e-6) sedpst_rea(irch) = 0.05
      if (sedpst_bry(irch) <= 1.e-6) sedpst_bry(irch) = 0.002
      if (sedpst_act(irch) <= 1.e-6) sedpst_act(irch) = 0.030

!!  set default values for mike van liew
      if (bc1(irch) <= 0.) bc1(irch) = bc1_bsn
      if (bc2(irch) <= 0.) bc2(irch) = bc2_bsn
      if (bc3(irch) <= 0.) bc3(irch) = bc3_bsn
      if (bc4(irch) <= 0.) bc4(irch) = bc4_bsn
!!  set default values for mike van liew

!! change units from day to hour if hourly routing is performed
      if (ievent > 0) then
        rs1(irch) = rs1(irch) / 24.
        rs2(irch) = rs2(irch) / 24.
        rs3(irch) = rs3(irch) / 24.
        rs4(irch) = rs4(irch) / 24.
        rs5(irch) = rs5(irch) / 24.
        rk1(irch) = rk1(irch) / 24.
        rk2(irch) = rk2(irch) / 24.
        rk3(irch) = rk3(irch) / 24.
        rk4(irch) = rk4(irch) / 24.
        bc1(irch) = bc1(irch) / 24.
        bc2(irch) = bc2(irch) / 24.
        bc3(irch) = bc3(irch) / 24.
        bc4(irch) = bc4(irch) / 24.
      end if

      close (104)
      return
 5100 format (a)
      end