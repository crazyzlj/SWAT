      subroutine bacteria
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates bacteria growth, transport with runoff and
!!    loss due to percolation into soil 

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkdq     |none          |Bacteria partition coefficient.
!!                               |Partition coefficient for bacteria between
!!                               |soluble and sorbed phase in surface runoff.
!!    bactlp_plt(:)|# cfu/m^2    |less persistent bacteria on foliage
!!    bactlpq(:)  |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)  |# cfu/m^2     |less persistent bacteria attached to soil
!!                               |particles
!!    bactminlp   |# cfu/m^2     |Threshold detection level for less persistent
!!                               |bacteria
!!                               |when bacteria levels drop to this amount the 
!!                               |model considers bacteria in the soil to be
!!                               |insignificant and sets the levesl to zero
!!    bactminp    |# cfu/m^2     |Threshold detection level for persistent 
!!                               |bacteria
!!                               |when bacteria levels drop to this amount the
!!                               |model considers bacterial in the soil to be 
!!                               |insignificant and sets the levels to zero
!!    bactmx      |none          |bacteria percolation coefficient
!!                               |Ratio of solution bacteria in surface layer
!!                               |to solution bacteria in percolate
!!    bactp_plt(:)|# cfu/m^2     |persistent bacteria on foliage
!!    bactpq(:)   |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)   |# cfu/m^2     |persistent bacteria attached to soil particles
!!    curyr       |none          |current year of simulation
!!    enratio     |none          |enrichment ratio calculated for current day 
!!                               |in HRU
!!    filterw(:)  |m             |filter strip width for bacteria transport
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    ihru        |none          |HRU number
!!    nyskip      |none          |number of years to skip output summarization
!!                               |and printing
!!    precipday   |mm H2O        |precipitation for the day in HRU
!!    sbactlchlp  |# cfu/m^2     |average annual number of less persistent 
!!                               |bacteria lost from soil surface layer by
!!                               |percolation
!!    sbactlchp   |# cfu/m^2     |average annual number of persistent bacteria
!!                               |lost from soil surface layer by percolation
!!    sdiegrolpq  |# cfu/m^2     |average annual change in the number of 
!!                               |less persistent bacteria colonies in soil
!!                               |solution in watershed
!!    sdiegrolps  |# cfu/m^2     |average annual change in the number of
!!                               |less persistent bacteria colonies on soil
!!                               |particles in watershed
!!    sdiegropq   |# cfu/m^2     |average annual change in the number of
!!                               |persistent bacteria colonies in soil solution
!!                               |in watershed
!!    sdiegrops   |# cfu/m^2     |average annual change in the number of
!!                               |persistent bacteria colonies on soil particles
!!                               |in watershed
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion
!!    sol_bd(:,:) |Mg/m**3       |bulk density of the soil
!!    sol_z(:,:)  |mm            |depth to bottom of soil layer
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    thbact      |none          |temperature adjustment factor for bacteria
!!                               |die-off/growth
!!    tmpav(:)    |deg C         |average air temperature on current day in HRU
!!    wlpq20      |1/day         |Overall rate change for less persistent
!!                               |bacteria in soil solution.
!!    wlps20      |1/day         |Overall rate change for less persistent
!!                               |bacteria adsorbed to soil particles.
!!    wof_lp      |none          |fraction of less persistent bacteria on foliage
!!                               |that is washed off by a rainfall event
!!    wof_p       |none          |fraction of persistent bacteria on foliage that
!!                               |is washed off by a rainfall event
!!    wp20lp_plt  |1/day         |Overall rate change for less persistent bacteria
!!                               |on foliage
!!    wp20p_plt   |1/day         |Overall rate change for persistent bacteria on
!!                               |foliage
!!    wpq20       |1/day         |Overall rate change for persistent bacteria in
!!                               |soil solution.
!!    wps20       |1/day         |Overall rate change for persistent bacteria
!!                               |adsorbed to soil particles.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlchlp   |# cfu/m^2     |less persistent bacteria removed from soil
!!                               |surface layer by percolation
!!    bactlchp    |# cfu/m^2     |persistent bacteria removed from soil surface
!!                               |layer by percolation
!!    bactlp_plt(:)|# cfu/m^2    |less persistent bacteria on foliage
!!    bactlpq(:)  |# cfu/m^2     |less persistent bacteria in soil solution
!!    bactlps(:)  |# cfu/m^2     |less persistent bacteria attached to soil
!!                               |particles
!!    bactp_plt(:)|# cfu/m^2     |persistent bacteria on foliage
!!    bactpq(:)   |# cfu/m^2     |persistent bacteria in soil solution
!!    bactps(:)   |# cfu/m^2     |persistent bacteria attached to soil particles
!!    bactrolp    |# cfu/m^2     |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# cfu/m^2     |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# cfu/m^2     |less persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    bactsedp    |# cfu/m^2     |persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    sbactlchlp  |# cfu/m^2     |average annual number of less persistent 
!!                               |bacteria lost from soil surface layer by
!!                               |percolation
!!    sbactlchp   |# cfu/m^2     |average annual number of persistent bacteria
!!                               |lost from soil surface layer by percolation
!!    sdiegrolpq  |# cfu/m^2     |average annual change in the number of 
!!                               |less persistent bacteria colonies in soil
!!                               |solution in watershed
!!    sdiegrolps  |# cfu/m^2     |average annual change in the number of
!!                               |less persistent bacteria colonies on soil
!!                               |particles in watershed
!!    sdiegropq   |# cfu/m^2     |average annual change in the number of
!!                               |persistent bacteria colonies in soil solution
!!                               |in watershed
!!    sdiegrops   |# cfu/m^2     |average annual change in the number of
!!                               |persistent bacteria colonies on soil particles
!!                               |in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactmn      |
!!    bactmx      |
!!    blpq        |# cfu/m^2     |less persistent bacteria in soil solution at
!!                               |beginning of day
!!    blps        |# cfu/m^2     |less persistent bacteria attached to soil
!!                               |particles at beginning of day
!!    bpq         |# cfu/m^2     |persistent bacteria in soil solution at
!!                               |beginning of day
!!    bps         |# cfu/m^2     |persistent bacteria attached to soil particles
!!                               |at beginning of day
!!    cbact       |
!!    j           |none          |HRU number
!!    wt1         |none          |conversion factor to convert kg/ha to g/t(ppm)
!!    xx          |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Min, Max
!!    SWAT: Theta

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real*8 :: bpq, blpq, bps, blps, wt1, cbact, xx

      j = 0
      j = ihru

      if (bactlps(j) < 1.e-6) bactlps(j) = 0.0
      if (bactlpq(j) < 1.e-6) bactlpq(j) = 0.0
      if (bactpq(j) < 1.e-6) bactpq(j) = 0.0
      if (bactps(j) < 1.e-6) bactps(j) = 0.0
      if (bactp_plt(j) < 1.e-6) bactp_plt(j) = 0.0
      
!! compute bacteria wash off
      if (precipday >= 2.54) then
        xx = 0.
        xx = wof_p * bactp_plt(j)
        if (xx > bactp_plt(j)) xx = bactp_plt(j)
        bactpq(j) = bactpq(j) + xx
        bactp_plt(j) = bactp_plt(j) - xx
        xx = 0.
        xx = wof_lp * bactlp_plt(j)
        if (xx > bactlp_plt(j)) xx = bactlp_plt(j)
        bactlpq(j) = bactlpq(j) + xx
        bactlp_plt(j) = bactlp_plt(j) - xx
      end if
   
!! compute bacteria die-off and re-growth on foilage
      if (tmpav(j) > 1.e-6) then
        bactp_plt(j) = bactp_plt(j) * Exp(-Theta(wp20p_plt,thbact,
     &    tmpav(j))) - bactminp
        bactp_plt(j) = Max(0., bactp_plt(j))
        if (bactp_plt(j) < bactminp) bactp_plt(j) = 0.
        bactlp_plt(j) = bactlp_plt(j) * Exp(-Theta(wp20lp_plt,thbact,
     &    tmpav(j))) - bactminlp
        bactlp_plt(j) = Max(0., bactlp_plt(j))
        if (bactlp_plt(j) < bactminlp) bactlp_plt(j) = 0.
      endif

!! compute bacteria die-off and re-growth in surface soil layer
      bpq = 0.
      blpq = 0.
      bps = 0.
      blps = 0.
      bpq = bactpq(j)
      bactpq(j) = bactpq(j) * Exp(-Theta(wpq20,thbact,tmpav(j))) -
     &    bactminp
      bactpq(j) = Max(0., bactpq(j))
      if (bactpq(j) < bactminp) bactpq(j) = 0.
      blpq = bactlpq(j) 
      bactlpq(j) = bactlpq(j) * Exp(-Theta(wlpq20,thbact,tmpav(j))) -
     &    bactminlp
      bactlpq(j) = Max(0., bactlpq(j))
      if (bactlpq(j) < bactminlp) bactlpq(j) = 0.
      bps = bactps(j)
      bactps(j) = bactps(j) * Exp(-Theta(wps20,thbact,tmpav(j))) -
     &    bactminp
      bactps(j) = Max(0., bactps(j))
      if (bactps(j) < bactminp) bactps(j) = 0.
      blps = bactlps(j)
      bactlps(j) = bactlps(j) * Exp(-Theta(wlps20,thbact,tmpav(j))) -
     &    bactminlp
      bactlps(j) = Max(0., bactlps(j))
      if (bactlps(j) < bactminlp) bactlps(j) = 0.

!! compute bacteria in the runoff
      bactrop = bactpq(j) * surfq(j) /                                  
     &                        (sol_bd(1,j) * sol_z(1,j) * bactkdq)
      bactrop = Min(bactrop, bactpq(j))
      bactrop = Max(bactrop, 0.)
      bactpq(j) = bactpq(j) - bactrop

      bactrolp = bactlpq(j) * surfq(j) /                                
     &                          (sol_bd(1,j) * sol_z(1,j) * bactkdq)
      bactrolp = Min(bactrolp, bactlpq(j))
      bactrolp = Max(bactrolp, 0.)
      bactlpq(j) = bactlpq(j) - bactrolp

!! compute bacteria transported with sediment
      if (enratio > 0.) then 
        wt1 = 0.
        wt1 = sol_bd(1,j) * sol_z(1,j) / 1000.

        cbact = 0.
        cbact = bactps(j) * enratio / wt1
        bactsedp = .0001 * cbact * sedyld(j) / hru_ha(j)
        bactsedp = Min(bactsedp, bactps(j))
        bactps(j) = bactps(j) - bactsedp

        cbact = 0.
        cbact = bactlps(j) * enratio / wt1
        bactsedlp = .0001 * cbact * sedyld(j) / hru_ha(j)
        bactsedlp = Min(bactsedlp, bactlps(j))
        bactlps(j) = bactlps(j) - bactsedlp
      end if

!! compute bacteria incorporated into the soil
      bactlchp = bactpq(j) * sol_prk(1,j) / ((conv_wt(1,j) / 1000.)     
     &                                                       * bactmx)
      bactlchp = Min(bactlchp, bactpq(j))
      bactlchp = Max(bactlchp, 0.)
      bactpq(j) = bactpq(j) - bactlchp

      bactlchlp = bactlpq(j) * sol_prk(1,j) / ((conv_wt(1,j) / 1000.)   
     &                                                       * bactmx)
      bactlchlp = Min(bactlchlp, bactlpq(j))
      bactlchlp = Max(bactlchlp, 0.)
      bactlpq(j) = bactlpq(j) - bactlchlp

!! summary calculations
      if (curyr > nyskip) then
        sdiegropq = sdiegropq + (bpq - bactpq(j)) * hru_dafr(j)
        sdiegrolpq = sdiegrolpq + (blpq - bactlpq(j)) * hru_dafr(j)
        sdiegrops = sdiegrops + (bps - bactps(j)) * hru_dafr(j)
        sdiegrolps = sdiegrolps + (blps - bactlps(j)) * hru_dafr(j)
!! added 4 here
        sbactrop = sbactrop + bactrop * hru_dafr(j)
        sbactrolp = sbactrolp + bactrolp * hru_dafr(j) 
        sbactsedp = sbactsedp + bactsedp * hru_dafr(j)
        sbactsedlp = sbactsedlp +  bactsedlp * hru_dafr(j)
!! added 4 here
        sbactlchp = sbactlchp + bactlchp * hru_dafr(j)
        sbactlchlp = sbactlchlp + bactlchlp * hru_dafr(j)
      end if

! 1 is HRU number!
!      xx = bactpq(1) + bactps(1) + bactp_plt(1)
!      yy = bactlpq(1) + bactlps(1) + bactlp_plt(1)
!      write (17,100) iida, xx, yy,
!     &     bactpq(1), bactps(1), bactlpq(1), bactlps(1),
!     &     bactrop, bactrolp, bactsedp, bactsedlp, bactlchp, bactlchlp,
!     &     bactp_plt(1), bactlp_plt(1)
! 100  format (i4,14f10.7)

      return
      end