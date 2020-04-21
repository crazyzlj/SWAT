      subroutine fert
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies N and P specified by date and
!!    amount in the management file (.mgt)

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition                  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkddb(:)   |none          |fraction of bacteria in solution (the
!!                                 |remaining fraction is sorbed to soil
!!                                 |particles)
!!    bactlp_plt(:) |# cfu/m^2     |less persistent bacteria on foliage
!!    bactlpdb(:)   |# cfu/g   frt |concentration of less persistent bacteria
!!                                 |in fertilizer
!!    bactpdb(:)    |# cfu/g   frt |concentration of persistent bacteria in
!!                                 |fertilizer
!!    bactlpq(:)    |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)    |# cfu/m^2     |less persistent bacteria attached to soil
!!                                 |particles
!!    bactp_plt(:)  |# cfu/m^2     |persistent bacteria on foliage
!!    bactpq(:)     |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)     |# cfu/m^2     |persistent bacteria attached to soil 
!!                                 |particles
!!    curyr         |none          |current year of simulation
!!    fertn         |kg N/ha       |total amount of nitrogen applied to soil
!!                                 |in HRU on day
!!    fertp         |kg P/ha       |total amount of phosphorus applied to soil
!!                                 |in HRU on day
!!    fminn(:)      |kg minN/kg frt|fraction of fertilizer that is mineral N
!!                                 |(NO3 + NH4)
!!    fminp(:)      |kg minP/kg frt|fraction of fertilizer that is mineral P
!!    fnh3n(:)      |kgNH3-N/kgminN|fraction of mineral N in fertilizer that
!!                                 |is NH3-N
!!    forgn(:)      |kg orgN/kg frt|fraction of fertilizer that is organic N
!!    forgp(:)      |kg orgP/kg frt|fraction of fertilizer that is organic P
!!    frt_kg        |kg/ha         |amount of fertilizer applied to HRU
!!    frt_surface   |none          |fraction of fertilizer which is applied to
!!                                 |the top 10 mm of soil (the remaining
!!                                 |fraction is applied to first soil layer)
!!    hru_dafr(:)   |km2/km2       |fraction of watershed area in HRU
!!    ihru          |none          |HRU number
!!    laiday(:)     |m**2/m**2     |leaf area index
!!    nfert(:)      |none          |sequence number of fertilizer application
!!                                 |within the year
!!    nro(:)        |none          |sequence number of year in rotation
!!    nyskip        |none          |number of years to not print/summarize output
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_bd(1,:)   |Mg/m^3        |bulk density of top soil layer in HRU
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                                 |in soil layer
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool
!!    sol_solp(:,:) |kg P/ha       |amount of inorganic phosohorus stored in
!!                                 |solution
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    wshd_fminp    |kg P/ha       |average annual amount of mineral P applied
!!                                 |in watershed
!!    wshd_fnh3     |kg N/ha       |average annual amount of NH3-N applied in
!!                                 |watershed
!!    wshd_fno3     |kg N/ha       |average annual amount of NO3-N applied in
!!                                 |watershed
!!    wshd_orgn     |kg N/ha       |average annual amount of organic N applied
!!                                 |in watershed
!!    wshd_orgp     |kg P/ha       |average annual amount of organic P applied
!!                                 |in watershed
!!    wshd_ftotn    |kg N/ha       |average annual amount of N (mineral & 
!!                                 |organic) applied in watershed
!!    wshd_ftotp    |kg P/ha       |average annual amount of P (mineral &
!!                                 |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlp_plt(:) |# cfu/m^2    |less persistent bacteria on foliage
!!    bactlpq(:)    |# cfu/m^2    |less persistent bacteria in soil solution
!!    bactlps(:)    |# cfu/m^2    |less persistent bacteria attached to soil
!!                                |particles
!!    bactp_plt(:)  |# cfu/m^2    |persistent bacteria on foliage
!!    bactpq(:)     |# cfu/m^2    |persistent bacteria in soil solution
!!    bactps(:)     |# cfu/m^2    |persistent bacteria attached to soil 
!!                                |particles
!!    fertn         |kg N/ha      |total amount of nitrogen applied to soil
!!                                |in HRU on day
!!    fertp         |kg P/ha      |total amount of phosphorus applied to soil
!!                                |in HRU on day
!!    nfert(:)      |none         |sequence number of fertilizer application
!!                                |within the year
!!    sol_aorgn(:,:)|kg N/ha      |amount of nitrogen stored in the active
!!                                |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha      |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha      |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_nh3(:,:)  |kg N/ha      |amount of nitrogen stored in the ammonium
!!                                |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha      |amount of nitrogen stored in the nitrate pool
!!                                |in soil layer
!!    sol_orgp(:,:) |kg P/ha      |amount of phosphorus stored in the organic
!!                                |P pool
!!    sol_solp(:,:) |kg P/ha      |amount of inorganic phosohorus stored in
!!                                |solution
!!    wshd_fminp    |kg P/ha      |average annual amount of mineral P applied
!!                                |in watershed
!!    wshd_fnh3     |kg N/ha      |average annual amount of NH3-N applied in
!!                                |watershed
!!    wshd_fno3     |kg N/ha      |average annual amount of NO3-N applied in
!!                                |watershed
!!    wshd_orgn     |kg N/ha      |average annual amount of organic N applied
!!                                |in watershed
!!    wshd_orgp     |kg P/ha      |average annual amount of organic P applied
!!                                |in watershed
!!    wshd_ftotn    |kg N/ha      |average annual amount of N (mineral & 
!!                                |organic) applied in watershed
!!    wshd_ftotp    |kg P/ha      |average annual amount of P (mineral &
!!                                |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units        |definition                  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    frt_t        |
!!    gc           |
!!    gc1          |
!!    j            |none         |HRU number
!!    l            |none         |counter (soil layer #)
!!    rtof         |none         |weighting factor used to partition the 
!!                               |organic N & P content of the fertilizer
!!                               |between the fresh organic and the active 
!!                               |organic pools
!!    xx           |none         |fraction of fertilizer applied to layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Erfc

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real, parameter :: rtof=0.5
      integer :: j, l, ifrt
      real :: xx, gc, gc1, swf, frt_t

      j = 0
      j = ihru

      ifrt = 0
      ifrt = ifrttyp

      do l = 1, 2
        xx = 0.
        if (l == 1) then
          xx = frt_surface                       
        else
          xx = 1. - frt_surface                     
        endif

        sol_no3(l,j) = sol_no3(l,j) + xx * frt_kg                    *  &
     &      (1. - fnh3n(ifrt)) * fminn(ifrt)

        if (cswat == 0) then
        sol_fon(l,j) = sol_fon(l,j) + rtof * xx *                       &
     &     frt_kg                    * forgn(ifrt)
	  sol_aorgn(l,j) = sol_aorgn(l,j) + (1. - rtof) * xx *            
     &     frt_kg                    * forgn(ifrt)
        sol_fop(l,j) = sol_fop(l,j) + rtof * xx *                       &
     &      frt_kg                    * forgp(ifrt)
        sol_orgp(l,j) = sol_orgp(l,j) + (1. - rtof) * xx *              &
     &      frt_kg                    * forgp(ifrt)
	  else
	  sol_mc(l,j) = sol_mc(l,j) + xx * frt_kg                    *
     &		forgn(ifrt) * 10.
	  sol_mn(l,j) = sol_mn(l,j) + xx * frt_kg                    *
     &		forgn(ifrt)
	  sol_mp(l,j) = sol_mp(l,j) + xx * frt_kg                    *
     &		forgp(ifrt)
	  end if

        sol_nh3(l,j) = sol_nh3(l,j) + xx * frt_kg                    *  &
     &      fnh3n(ifrt) * fminn(ifrt)

        sol_solp(l,j) = sol_solp(l,j) + xx * frt_kg                    *&
     &      fminp(ifrt)

      end do 

!!!    write statement for virgina/mari-vaughn study        !!!
!      write (1112,1112) (sol_no3(l,j), sol_fon(l,j), sol_aorgn(l,j),    &
!    &sol_nh3(l,j),  sol_solp(l,j), sol_fop(l,j), sol_orgp(l,j),        &
!     &l = 1,4)
!1112  format (200f8.2)


!! add bacteria - #cfu/g * t(manure)/ha * 1.e6g/t * ha/10,000m^2 = 100.
!! calculate ground cover
      gc = 0.
      gc = (1.99532 - Erfc(1.333 * laiday(j) - 2.)) / 2.1
      if (gc < 0.) gc = 0.

      gc1 = 0.
      gc1 = 1. - gc


      frt_t = 0.
      frt_t = bact_swf * frt_kg                    / 1000.

      bactp_plt(j) = gc * bactpdb(ifrt) * frt_t * 100. + bactp_plt(j)
      bactlp_plt(j) = gc * bactlpdb(ifrt) * frt_t * 100. + bactlp_plt(j)

      bactpq(j) = gc1 * bactpdb(ifrt) * frt_t * 100. + bactpq(j)
      bactpq(j) = bactkddb(ifrt) * bactpq(j)

      bactps(j) = gc1 * bactpdb(ifrt) * frt_t * 100. + bactps(j)
      bactps(j) = (1. - bactkddb(ifrt)) * bactps(j)

      bactlpq(j) = gc1 * bactlpdb(ifrt) * frt_t * 100. + bactlpq(j)
      bactlpq(j) = bactkddb(ifrt) * bactlpq(j)

      bactlps(j) = gc1 * bactlpdb(ifrt) * frt_t * 100. + bactlps(j)
      bactlps(j) = (1. - bactkddb(ifrt)) * bactlps(j)


!! summary calculations
      fertno3 = frt_kg * fminn(ifrt) * (1. - fnh3n(ifrt))
      fertnh3 = frt_kg * (fminn(ifrt) * fnh3n(ifrt))
      fertorgn = frt_kg * forgn(ifrt)
      fertsolp = frt_kg * fminp(ifrt)
      fertorgp = frt_kg * forgp(ifrt)  
      fertn = fertn + (frt_kg                    + cfertn) *             &           
     &   (fminn(ifrt) + forgn(ifrt))

      fertp = fertp + (frt_kg                    + cfertp) *             &
     &   (fminp(ifrt) + forgp(ifrt))

      tfertn(j) = tfertn(j) + fertn
      tfertp(j) = tfertp(j) + fertp

      if (curyr > nyskip) then
      wshd_ftotn = wshd_ftotn + frt_kg                    * hru_dafr(j) &
     &   * (fminn(ifrt) + forgn(ifrt))

      wshd_forgn = wshd_forgn + frt_kg                    * hru_dafr(j) &
     &   * forgn(ifrt)

      wshd_fno3 = wshd_fno3 + frt_kg                    * hru_dafr(j) * &
     &   fminn(ifrt) * (1. - fnh3n(ifrt))

      wshd_fnh3 = wshd_fnh3 + frt_kg                    * hru_dafr(j) * &
     &   fminn(ifrt) * fnh3n(ifrt)

      wshd_ftotp = wshd_ftotp + frt_kg                    * hru_dafr(j) &
     &   * (fminp(ifrt) + forgp(ifrt))

      wshd_fminp = wshd_fminp + frt_kg                    * hru_dafr(j) &
     &   * fminp(ifrt)

      wshd_forgp = wshd_forgp + frt_kg                    * hru_dafr(j) &
     &   * forgp(ifrt)

      end if


!! increase fertilizer sequence number by one
      nfert(j) = nfert(j) + 1

      return
      end
