      subroutine readpest

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads parameters from the toxin/pesticide database 
!!    (pest.dat)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    mpdb        |none          |maximum number of toxins/pesticides in 
!!                               |database
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ap_ef(:)    |none          |application efficiency (0-1)
!!    decay_f(:)  |none          |exponential of the rate constant for 
!!                               |degradation of the pesticide on foliage
!!    decay_s(:)  |none          |exponential of the rate constant for 
!!                               |degradation of the pesticide in soil
!!    hlife_f(:)  |days          |half-life of pesticide on foliage
!!    hlife_s(:)  |days          |half-life of pesticide in soil
!!    pname(:)    |NA            |name of pesticide/toxin
!!    pst_wof(:)  |none          |fraction of pesticide on foliage which
!!                               |is washed-off by a rainfall event
!!    pst_wsol(:) |mg/L (ppm)    |solubility of chemical in water
!!    skoc(:)     |(mg/kg)/(mg/L)|soil adsorption coefficient normalized
!!                               |for soil organic carbon content
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    eof         |none          |end of file flag
!!    ip          |none          |counter which represents the array
!!                               |storage number of the pesticide data
!!                               |the array storage number is used by the
!!                               |model to access data for a specific
!!                               |pesticide
!!    ipnum       |none          |number of pesticide/toxin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: ip, ipnum, eof
      character (len=17) :: pstnm
      real :: skocp, wofp, hlff, hlfs, apefp, pwsol
      eof = 0


      do
        !!initialize local variables
        apefp = 0.0
        hlff = 0.0
        hlfs = 0.0
        pstnm = ""
        pwsol = 0.0
        skocp = 0.0
        wofp = 0.0

        read (106,5000,iostat=eof) ip, pstnm, skocp, wofp, hlff, hlfs,  
     &       apefp, pwsol
        if (eof < 0) exit
        
        if (ip == 0) exit

        pname(ip) = pstnm
        skoc(ip) = skocp
        pst_wof(ip) = wofp
        hlife_f(ip) = hlff
        hlife_s(ip) = hlfs
        ap_ef(ip) = apefp
        pst_wsol(ip) = pwsol


      !! calculations: the first-order rate law for the decay of pesticides
      !! is dP/dt = -kP where P is the amount of pesticide, 
      !! t is the time and k is the rate constant for degradation. To calculate
      !! the amount of pesticide at any time after application the equation
      !! P(t) = P_o*Exp(-kt) is used where P_o is the original amount of 
      !! pesticide. k can be calculate with the equation k = 0.693/hlife.
      !! decay_f or decay_s = Exp(-k)
        if (hlife_f(ip) > 0.) then
          decay_f(ip) = Exp(-.693/hlife_f(ip))
        else
          decay_f(ip) = 0.
        endif
        if (hlife_s(ip) > 0.) then
          decay_s(ip) = Exp(-.693/hlife_s(ip))
        else
          decay_s(ip) = 0.
        endif
   
      !! set values for pesticide routed through main channel network
        if (ip == irtpest) then
          pest_sol = pst_wsol(ip) * 1000.
        end if

      end do

      close (106)

      return
 5000 format (i3,a17,f10.1,f5.2,2f8.1,f5.2,f11.3)
      end