      subroutine readbsn

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the basin input file (.bsn). This file
!!    contains information related to processes modeled or defined at the
!!    watershed level

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    adj_pkr     |none          |peak rate adjustment factor in the subbasin.
!!                               |Used in the MUSLE equation to account for
!!                               |impact of peak flow on erosion.
!!    anion_excl(:) |none          |fraction of porosity from which anions
!!                                 |are excluded
!!    bact_swf    |none          |fraction of manure containing active colony
!!                               |forming units (cfu)
!!    bactkdq     |none          |Bacteria soil partitioning coefficient.
!!                               |Ratio of solution bacteria in surface layer
!!                               |to solution bacteria in runoff
!!                               |soluble and sorbed phase in surface runoff.
!!    bactminlp   |# cfu/m^2     |Threshold detection level for less persistent
!!                               |bacteria
!!                               |when bacteria levels drop to this amount the
!!                               |model considers bacteria in the soil to be
!!                               |insignificant and sets the levels to zero
!!    bactminp    |# cfu/m^2     |Threshold detection level for persistent
!!                               |bacteria
!!                               |when bacteria levels drop to this amount the
!!                               |model considers bacteria in the soil to be
!!                               |insignificant and sets the levels to zero
!!    bactmx      |none          |bacteria percolation coefficient 
!!                               |Ratio of solution bacteria in surface layer
!!                               |to solution bacteria in percolate
!!    bc1(:)           |1/hr          |rate constant for biological oxidation of
!!                                    |NH3 to NO2 in reach at 20 deg C
!!    bc2(:)           |1/hr          |rate constant for biological oxidation of
!!                                    |NO2 to NO3 in reach at 20 deg C
!!    bc3(:)           |1/hr          |rate constant for hydrolysis of organic N
!!                                    |to ammonia in reach at 20 deg C
!!    bc4(:)           |1/hr          |rate constant for the decay of organic P
!!                                    |to dissolved P in reach at 20 deg C
!!    bf_flag          |
!!    cdn              |none          |denitrification exponential rate coefficient
!!    cfactor          |              |scaling parameter for cover and mgt factor in ANSWERS erosion model
!!    ch_d50           |mm            |median particle diameter of channel bed
!!    ch_onco(:)       |ppm           |channel organic n concentration
!!    ch_opco(:)       |ppm           |channel organic p concentration
!!    cmn              |none          |rate factor for humus mineralization on
!!                                    |active organic N
!!    cncoef           |none          |plant ET curve number coefficient 
!!    cnfroz           |              |Drainge coefficient (mm day -1) 
!!    cswat                           | = 0 Static soil carbon (old mineralization routines)
!!                                 = 1 C-FARM one carbon pool model 
!!                                 = 2 Century model
!!    decr_min    |              |Minimum daily residue decay
!!    depimp_bsn  |mm            |depth to impervious layer. Used to model
!!                               |perched water tables in all HRUs in watershed
!!    drain_co_bsn |mm-day-1     |Drainage coeffcient (range 10.0 - 51.0)
!!    ddrain_bsn  |mm            |depth to the sub-surface drain
!!    dorm_hr     |hours         |time threshold used to define dormant
!!    epco(:)     |none          |plant water uptake compensation factor (0-1)
!!    eros_expo   |              |an expoenet in the overland flow erosion equ ranges 1.5-3.0
!!    eros_spl    |none          |coefficient of splash erosion varing 0.9-3.1
!!    esco(:)     |none          |soil evaporation compensation factor (0-1)
!!    evlai       |none          |leaf area index at which no evaporation
!!                               |occurs.  This variable is used in ponded HRUs
!!                               |where evaporation from the water surface is 
!!                               |restricted by the plant canopy cover. Evapor-
!!                               |ation from the water surface equals potential
!!                               |ET when LAI = 0 and decreased linearly to O
!!                               |when LAI = EVLAI
!!    evrch       |none          |Reach evaporation adjustment factor. 
!!                               |Evaporation from the reach is multiplied by
!!                               |EVRCH. This variable was created to limit the 
!!                               |evaporation predicted in arid regions.
!!    ffcb        |none          |initial soil water content expressed as a
!!                               |fraction of field capacity
!!    fixco       |none          |nitrogen fixation coefficient
!!    gdrain      |hours         |drain tile lag time
!!    hlife_ngw   |days          |?Half-life of nitrogen in groundwater
!!    icfac       |              | icfac = 0 for C-factor calculation using
!!                                  Cmin (as described in manual)
!!                                       = 1 for new C-factor calculation
!!                                  from RUSLE (no minimum needed)
!!    icn         |none          |CN method flag: 
!!                               |(for testing alternative method)
!!                               |0 use traditional SWAT method which bases
!!                               |  CN on soil moisture
!!                               |1 use alternative method which bases CN on
!!                               |  plant ET
!!    icrk        |none          |crack flow code
!!                               |1: compute flow in cracks
!!    ideg        |none          |channel degredation code
!!                               |1: compute channel degredation (downcutting
!!                               |   and widening)
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ipet        |none          |code for potential ET method
!!                               |0 Priestley-Taylor method
!!                               |1 Penman/Monteith method
!!                               |2 Hargreaves method
!!                               |3 read in daily potential ET data
!!    irte        |none          |water routing method:
!!                               |0 variable storage method
!!                               |1 Muskingum method
!!    irtpest     |none          |number of pesticide to be routed through the
!!                               |watershed
!!    ised_det    |none          |max half-hour rainfall fraction calc option:
!!                               |0 generate max half-hour rainfall fraction from 
!!                               |  triangular distribution
!!                               |1 use monthly mean max half-hour rainfall
!!                               |  fraction
!!    isubwq      |none          |subbasin water quality code
!!                               |0 do not calculate algae/CBOD
!!                               |1 calculate algae/CBOD
!!                                    drainmod tile equations   01/2006
!!    itdrn       |none          |tile drainage equations flag/code
!!                               |1 simulate tile flow using subroutine drains(wt_shall)
!!                               |0 simulate tile flow using subroutine origtile(wt_shall,d) 
!!    iwtdn       |none          |water table depth algorithms flag/code
!!                               |1 simulate wt_shall using subroutine new water table depth routine
!!                               |0 simulate wt_shall using subroutine original water table depth routine
!!    ismax       |none          |maximum depressional storage selection flag/code
!!                               |  0 = static depressional storage
!!                               |  1 = dynamic storage based on tillage and cumulative rainfall 
!!    iroutunit   |none          | not being implemented in this version
!!                                    drainmod tile equations   01/2006
!!    iuh         |              |unit hydrograph method: 1=triangular UH; 2=gamma funtion UH;
!!    iwq         |none          |stream water quality code
!!                               |0 do not model stream water quality
!!                               |1 model stream water quality
!!                               |   (QUAL2E & pesticide transformations)
!!    latksatf_bsn |             |Multiplication factor to determine lateral ksat from SWAT ksat input value for HRU
!!                                  (range 0.01 - 4.0)
!!    msk_co1     |none          |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at bankfull depth (phi(10,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_co2     |none          |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at 0.1 bankfull depth (phi(13,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_x       |none          |weighting factor controling relative
!!                               |importance of inflow rate and outflow rate
!!                               |in determining storage on reach
!!    nfixmx      |kg/ha         |maximum daily n-fixation
!!    n_updis     |none          |nitrogen uptake distribution parameter
!!                               |This parameter controls the amount of
!!                               |nitrogen removed from the different soil layer
!!                               |layers by the plant. In particular, this
!!                               |parameter allows the amount of nitrogen
!!                               |removed from the surface layer via plant
!!                               |uptake to be controlled. While the relation-
!!                               |ship between UBN and N removed from the
!!                               |surface layer is affected by the depth of the
!!                               |soil profile, in general, as UBN increases
!!                               |the amount of N removed from the surface
!!                               |layer relative to the amount removed from the
!!                               |entire profile increases
!!    nactfr      |none          |nitrogen active pool fraction. The fraction of
!!                               |organic nitrogen in the active pool.
!!    nperco      |none          |nitrate percolation coefficient (0-1)
!!                               |0:concentration of nitrate in surface runoff
!!                               |  is zero
!!                               |1:percolate has same concentration of nitrate
!!                               |  as surface runoff
!!    pc_bsn      |mm h-1        |Pump capacity (def val = 1.042 mm h-1 or 25 mm day-1)
!!    p_updis     |none          |phosphorus uptake distribution parameter
!!                               |This parameter controls the amount of
!!                               |phosphorus removed from the different soil
!!                               |layers by the plant. In particular, this 
!!                               |parameter allows the amount of phosphorus
!!                               |removed from the surface layer via plant
!!                               |uptake to be controlled. While the relation-
!!                               |ship between UBP and P uptake from the
!!                               |surface layer is affected by the depth of the
!!                               |soil profile, in general, as UBP increases
!!                               |the amount of P removed from the surface
!!                               |layer relative to the amount removed from the
!!                               |entire profile increases
!!    percop      |none          |pesticide percolation coefficient (0-1)
!!                               |0: concentration of pesticide in surface
!!                               |   runoff is zero
!!                               |1: percolate has same concentration of 
!!                               |   pesticide as surface runoff
!!    petfile     |NA            |potential ET file name (.pet)
!!    phoskd      |none          |Phosphorus soil partitioning coefficient
!!                               |Ratio of soluble phosphorus in surface layer
!!                               |to soluble phosphorus in runoff
!!    pperco      |none          |phosphorus percolation coefficient
!!                               |ratio of soluble phosphorus in surface
!!                               |to soluble phosphorus in percolate
!!    prf_bsn     |none          |Basinwide peak rate adjustment factor for sediment 
!!                               |routing in the channel. Allows impact of 
!!                               |peak flow rate on sediment routing and 
!!                               |channel reshaping to be taken into account.
!!    psp         |none          |Phosphorus availibility index. The fraction
!!                               |of fertilizer P remaining in labile pool
!!                               |after initial rapid phase of P sorption.
!!    rcn_sub_bsn |mg/kg         |Concentration of nitrogen in the rainfall
!!    re_bsn      |mm            |Effective radius of drains (range 3.0 - 40.0) 
!!    rill_mult   |              |Multiplier to USLE_K for soil susceptible to rill erosion, range 0.5-2.0
!!    res_stlr_co |none          |reservoir sediment settling coefficient
!!    rsd_covco   |              |residue cover factor for computing frac of cover
!!    rsdco       |none          |residue decomposition coefficient
!!                               |The fraction of residue which will decompose
!!                               |in a day assuming optimal moisture,
!!                               |temperature, C:N ratio, and C:P ratio
!!    sdnco       |none          |denitrification threshold:  fraction of field
!!                               | capacity triggering denitrification
!!    sed_ch      |              |channel routing for HOURLY; 0=Bagnold;2=Brownlie;3=Yang;
!!    sftmp       |deg C         |Snowfall temperature
!!    sig_g       |none          |geometric standard deviation of particle sizes for the main channel
!!                               |Mean air temperature at which precipitation
!!                               |is equally likely to be rain as snow/freezing
!!                               |rain.
!!    sdrain_bsn  |mm            |Distance bewtween two drain or tile tubes (range 7600.0 - 30000.0)
!!    sstmaxd(:)  |mm            |static maximum depressional storage; read from .sdr 
!----------------------------retention parameter adjustment factor D. Moriasi 4/8/2014
!!    r2adj_bsn   |none          |basinwide retention parameter adjustment factor (greater than 1)! D. Moriasi 4/8/2014
!!    smfmn       |mm/deg C/day  |Minimum melt rate for snow during year (Dec.
!!    smfmn       |mm/deg C/day  |Minimum melt rate for snow during year (Dec.
!!                               |21) where deg C refers to the air temperature.
!!    smfmx       |mm/deg C/day  |Maximum melt rate for snow during year (June
!!                               |21) where deg C refers to the air temperature.
!!                               |SMFMX and SMFMN allow the rate of snow melt
!!                               |to vary through the year. These parameters 
!!                               |are accounting for the impact of soil
!!                               |temperature on snow melt.
!!    smtmp       |deg C         |Snow melt base temperature 
!!                               |Mean air temperature at which snow melt will
!!                               |occur.
!!    smxco       |              |adjustment factor for max curve number s factor (0-1)
!!    sno50cov    |none          |Fraction of SNOCOVMX that corresponds to 50%
!!                               |snow cover. SWAT assumes a nonlinear relation-
!!                               |ship between snow water and snow cover.
!!    snocov1     |none          |1st shape parameter for snow cover equation
!!                               |This parameter is determined by solving the 
!!                               |equation for 50% snow cover
!!    snocov2     |none          |2nd shape parameter for snow cover equation
!!                               |This parameter is determined by solving the 
!!                               |equation for 95% snow cover
!!    snocovmx    |mm H2O        |Minimum snow water content that corresponds to
!!                               |100% snow cover. If the snow water content is 
!!                               |less than SNOCOVMX, then a certain percentage 
!!                               |of the ground will be bare.
!!    spcon       |none          |linear parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    spexp       |none          |exponent parameter for calculating sediment
!!                               |reentrained in channel sediment routing
!!    surlag      |days          |Surface runoff lag time.
!!                               |This parameter is needed in subbasins where
!!                               |the time of concentration is greater than 1
!!                               |day. SURLAG is used to create a "storage" for
!!                               |surface runoff to allow the runoff to take
!!                               |longer than 1 day to reach the subbasin outlet
!!    tb_adj      |none          |adjustment factor for subdaily unit hydrograph
!!                               |basetime
!!    tdrain_bsn  |hours         |time to drain soil to field capacity
!!    thbact      |none          |temperature adjustment factor for bacteria
!!                               |die-off/growth
!!    timp        |none          |Snow pack temperature lag factor (0-1)
!!                               |1 = no lag (snow pack temp=current day air
!!                               |temp) as the lag factor goes to zero, the snow
!!                               |pack's temperature will be less influenced by
!!                               |the current day's air temperature
!!    trnsrch     |none          |fraction of transmission losses from main 
!!                               |channel that enter deep aquifer
!!    ubw         |none          |water uptake distribution parameter
!!                               |This parameter controls the amount of
!!                               |water removed from the different soil layers
!!                               |by the plant. In particular, this parameter
!!                               |allows the amount of water removed from
!!                               |the surface layer via plant uptake to be
!!                               |controlled. While the relationship between
!!                               |UBW and H2O removed from the surface layer is
!!                               |affected by the depth of the soil profile, in
!!                               |general, as UBW increases the amount of water
!!                               |removed from the surface layer relative to the
!!                               |amount removed from the entire profile 
!!                               |increases
!!    uhalpha     |              |alpha coefficient for estimating unit hydrograph
!!                               |using a gamma function (*.bsn)
!!    uobn        |none          |nitrogen uptake normalization parameter
!!                               |This variable normalizes the nitrogen uptake
!!                               |so that the model can easily verify that
!!                               |upake from the different soil layers sums to 
!!                               |1.0
!!    uobp        |none          |phosphorus uptake normalization parameter
!!                               |This variable normalizes the phosphorus uptake
!!                               |so that the model can easily verify that
!!                               |uptake from the different soil layers sums to 
!!                               |1.0
!!    uobw        |none          |water uptake normalization parameter
!!                               |This variable normalizes the water uptake so 
!!                               |that the model can easily verify that uptake 
!!                               |from the different soil layers sums to 1.0
!!    vcrit       |              |Critical velocity
!!    wdlpf       |1/day         |Die-off factor for less persistent bacteria on
!!                               |foliage.
!!    wdlpq       |1/day         |Die-off factor for less persistent bacteria in
!!                               |soil solution.
!!    wdlprch     |1/day         |Die-off factor for less persistent bacteria
!!                               |in streams
!!    wdlpres     |1/day         |Die-off factor for less persistent bacteria
!!                               |in reservoirs
!!    wdlps       |1/day         |Die-off factor for less persistent bacteria 
!!                               |absorbed to soil particles.
!!    wdpf        |1/day         |Die-off factor for persistent bacteria on 
!!                               |foliage.
!!    wdpq        |1/day         |Die-off factor for persistent bacteria in 
!!                               |soil solution.
!!    wdprch      |1/day         |Die-off factor for persistent bacteria in
!!                               |streams
!!    wdpres      |1/day         |Die-off factor for persistent bacteria in
!!                               |reservoirs
!!    wdps        |1/day         |Die-off factor for persistent bacteria
!!                               |adsorbed to soil particles.
!!    wglpf       |1/day         |Growth factor for less persistent bacteria on
!!                               |foliage
!!    wglpq       |1/day         |Growth factor for less persistent bacteria in
!!                               |soil solution.
!!    wglps       |1/day         |Growth factor for less persistent bacteria
!!                               |adsorbed to soil particles.
!!    wgpf        |1/day         |Growth factor for persistent bacteria on
!!                               |foliage.
!!    wgpq        |1/day         |Growth factor for persistent bacteria in soil
!!                               |solution.
!!    wgps        |1/day         |Growth factor for persistent bacteria
!!                               |adsorbed to soil particles.
!!    wof_lp      |none          |Wash off fraction for less persistent
!!                               |bacteria on foliage during a rainfall event
!!    wof_p       |none          |Wash off fraction for persistent bacteria on
!!                               |foliage during a rainfall event
!!    wlpq20      |1/day         |Overall rate change for less persistent
!!                               |bacteria in soil solution.
!!    wlps20      |1/day         |Overall rate change for less persistent
!!                               |bacteria adsorbed to soil particles.
!!    wp20lp_plt  |1/day         |Overall rate change for less persistent bacteria
!!                               |on foliage
!!    wp20p_plt   |1/day         |Overall rate change for persistent bacteria on
!!                               |foliage
!!    wpq20       |1/day         |Overall rate change for persistent bacteria in
!!                               |soil solution.
!!    wps20       |1/day         |Overall rate change for persistent bacteria
!!                               |adsorbed to soil particles.
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    eof         |none          |end of file flag (=-1 if eof, else =0)
!!    epcobsn     |none          |plant water uptake compensation factor (0-1)
!!    escobsn     |none          |soil evaporation compensation factor (0-1)
!!    titldum     |NA            |title line for .bsn file, not used
!!!    wwqfile     |NA            |name of watershed water quality file (.wwq)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: ascrv

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
      character (len=130) :: tlu
      character (len=13) :: wwqfile
      integer :: eof, numlu
      real :: escobsn, epcobsn
!!      real :: r2adj_bsn  !D. Moriasi 4/8/2014    

!!    initialize variables
      eof = 0
      escobsn = 0.
      epcobsn = 0.
      r2adj_bsn = 0.  !D. Moriasi 4/8/2014
      wwqfile = ""
      numlu=1

!! read basin parameters
      do
      read (103,1000) titldum
      read (103,1000) titldum
      read (103,1000) titldum
      read (103,*) sftmp
      read (103,*) smtmp
      read (103,*) smfmx
      read (103,*) smfmn
      read (103,*) timp
      read (103,*) snocovmx
      read (103,*) sno50cov
      read (103,*) ipet
      read (103,1000) petfile
      read (103,*) escobsn
      read (103,*) epcobsn
      read (103,*) evlai
      read (103,*) ffcb
      read (103,1000) titldum
      read (103,*) ievent
      read (103,*) icrk
      read (103,*) surlag_bsn
      read (103,*) adj_pkr
      read (103,*) prf_bsn
      read (103,*) spcon_bsn
      read (103,*) spexp_bsn
      read (103,1000) titldum
      read (103,*) rcn_sub_bsn
      read (103,*) cmn_bsn
      read (103,*) n_updis
      read (103,*) p_updis
      read (103,*) nperco_bsn
      read (103,*) pperco_bsn
      read (103,*) phoskd_bsn
      read (103,*) psp_bsn
      read (103,*) rsdco
      read (103,1000) titldum
      read (103,*) percop
      read (103,1000) titldum
      read (103,*) isubwq
      read (103,1000) titldum
      read (103,*) wdpq
      read (103,*) wgpq
      read (103,*) wdlpq
      read (103,*) wglpq
      read (103,*) wdps
      read (103,*) wgps
      read (103,*) wdlps
      read (103,*) wglps
      read (103,*) bactkdq
      read (103,*) thbact
      read (103,*) wof_p
      read (103,*) wof_lp
      read (103,*) wdpf
      read (103,*) wgpf
      read (103,*) wdlpf
      read (103,*) wglpf
      read (103,1001) ised_det
      read (103,1000) titldum
      read (103,*) irte
      read (103,*) msk_co1
      read (103,*) msk_co2
      read (103,*) msk_x
      read (103,*) ideg
      read (103,*) iwq
      read (103,1000) wwqfile
      read (103,*) trnsrch
      read (103,*) evrch
      read (103,*) irtpest
      read (103,*) icn
      read (103,*) cncoef
      read (103,*) cdn_bsn
      read (103,*) sdnco_bsn
      read (103,*) bact_swf
      read (103,*,iostat=eof) bactmx
      if (eof < 0) exit
      read (103,*,iostat=eof) bactminlp
      if (eof < 0) exit
      read (103,*,iostat=eof) bactminp
      if (eof < 0) exit
      read (103,*,iostat=eof) wdlprch
      if (eof < 0) exit
      read (103,*,iostat=eof) wdprch
      if (eof < 0) exit
      read (103,*,iostat=eof) wdlpres
      if (eof < 0) exit
      read (103,*,iostat=eof) wdpres
      if (eof < 0) exit
      read (103,*,iostat=eof) tb_adj
      if (eof < 0) exit
      read (103,*,iostat=eof) depimp_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) ddrain_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) tdrain_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) gdrain_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) cn_froz
      if (eof < 0) exit
      read (103,*,iostat=eof) dorm_hr
      if (eof < 0) exit
      read (103,*,iostat=eof) smxco  
      if (eof < 0) exit
      read (103,*,iostat=eof) fixco
      if (eof < 0) exit
      read (103,*,iostat=eof) nfixmx
      if (eof < 0) exit
      read (103,*,iostat=eof) anion_excl_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_onco_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_opco_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) hlife_ngw_bsn
      if (eof < 0) exit
      read (103,1000,iostat=eof) titldum    
      if (eof < 0) exit
      read (103,*,iostat=eof) bc1_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) bc2_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) bc3_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) bc4_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) decr_min
      if (eof < 0) exit
      read (103,*,iostat=eof) icfac
      if (eof < 0) exit
      read (103,*,iostat=eof) rsd_covco
      if (eof < 0) exit
      read (103,*,iostat=eof) vcrit
      if (eof < 0) exit
      read (103,*,iostat=eof) cswat
      if (eof < 0) exit
      read (103,*,iostat=eof) res_stlr_co
      if (eof < 0) exit
      read (103,*,iostat=eof) bf_flg
      if (eof < 0) exit
      read (103,*,iostat=eof) iuh 
      if (eof < 0) exit
      read (103,*,iostat=eof) uhalpha 
      if (eof < 0) exit
      read (103,*,iostat=eof) titldum
      read (103,'(a130)') tlu
       do ii=3,len_trim(tlu)
          if ((tlu(ii:ii).eq.','.and.tlu(ii-1:ii-1).ne.',').or.   
     &       (tlu(ii:ii).eq.' '.and.tlu(ii-1:ii-1).ne.' ')) then
             numlu = numlu + 1
          end if	   
       end do 
       if (len_trim(tlu).le.3) numlu = 0
       backspace(103)
       read (103,*) (lu_nodrain(kk), kk=1,numlu)
       

 !!   subdaily erosion modeling by Jaehak Jeong
      read (103,*,iostat=eof) titldum
      if (eof < 0) exit
      read (103,*,iostat=eof) eros_spl
      if (eof < 0) exit
      read (103,*,iostat=eof) rill_mult
      if (eof < 0) exit
      read (103,*,iostat=eof) eros_expo
      if (eof < 0) exit
      read (103,*,iostat=eof) sed_ch
      if (eof < 0) exit
      read (103,*,iostat=eof) c_factor
      if (eof < 0) exit
      read (103,*,iostat=eof) ch_d50
      if (eof < 0) exit
      read (103,*,iostat=eof) sig_g 
      if (eof < 0) exit
!!    Drainmod input variables - 01/2006
      read (103,*,iostat=eof) re_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) sdrain_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) drain_co_bsn
      if (eof < 0) exit
!!    Drainmod input variables - 01/2006
      read (103,*,iostat=eof) pc_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) latksatf_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) itdrn
      if (eof < 0) exit
      read (103,*,iostat=eof) iwtdn
      if (eof < 0) exit
      read (103,*,iostat=eof) sol_p_model  !! if = 1  use new soil P model
      if (eof < 0) exit
      read (103,*,iostat=eof) iabstr
      if (eof < 0) exit
!     iatmodep = 0 - average annual = 1 - monthly  2 = daily
      read (103,*,iostat=eof) iatmodep
      if (eof < 0) exit
      read (103,*,iostat=eof) r2adj_bsn ! Modified by D. Moriasi 4/8/2014
      if (eof < 0) exit
      read (103,*,iostat=eof) sstmaxd_bsn
      if (eof < 0) exit
      read (103,*,iostat=eof) ismax
      if (eof < 0) exit   
      read (103,*,iostat=eof) iroutunit
      exit
!!    Drainmod input variables - 01/2006
      end do

!!    copy global values to local HRUs
      esco = escobsn
      epco = epcobsn
      
!!    set default values for undefined parameters
!     if (ievent == 1) nstep = 24
      if (r2adj_bsn < 1.e-6) r2adj_bsn = 0.
      if (drain_co_bsn < 1.e-6) drain_co_bsn = 10. 
 !!Parameter variables added D. Moriasi 4/8/2014  
      if (sstmaxd_bsn < 1.e-6) sstmaxd_bsn = 20. 
  !!----------------------------------------------------------------        
      if (res_stlr_co < 1.e-6) res_stlr_co = .184
      if (depimp_bsn < 1.e-6) depimp_bsn = 6000.
      if (bact_swf < 1.e-6) bact_swf = 0.15
      if (adj_pkr <= 0.) adj_pkr = 1.
      if (spcon_bsn <= 0.) spcon_bsn = .0001
      if (spexp_bsn <= 0.) spexp_bsn = 1.0
      if (prf_bsn <= 0.) prf_bsn = 1.0
      if (percop <= 0.) percop = .5
      if (n_updis <= 0.) n_updis = 20.
      if (p_updis <= 0.) p_updis = 20.
      if (nperco_bsn <= 0.) nperco_bsn = .20
      if (pperco_bsn <= 0.) pperco_bsn = 10.
      if (rsdco <= 0.) rsdco = .05
      if (phoskd_bsn <= 0.) phoskd_bsn = 175.
      if (psp_bsn <= 0.) psp_bsn = 0.4
      if (cmn_bsn <= 0.) cmn_bsn = .0003
      if (smfmx <= 0.) smfmx = 4.5
      if (smfmn <= 0.) smfmn = 4.5
      if (timp <= 0.) timp = 1.0
      if (snocovmx <= 0.) snocovmx = 1.0
      if (sno50cov <= 0.) sno50cov = .5
      if (surlag_bsn <= 0.) surlag_bsn = 4.
      if (evrch <= 0.) evrch = 0.6
      if (bactkdq <= 0.) bactkdq = 75.
      if (thbact <= 0.) thbact = 1.07
      if (msk_x <= 0.) msk_x = 0.2
      if (msk_co1 <= 0. .and. msk_co2 <= 0.) then
        msk_co1 = 0.75
        msk_co2 = 0.25
      end if

      if (evlai <= 0.) evlai = 3.0
      if (cncoef <= 0.) cncoef = 1.0
      if (cdn_bsn <= 0.) cdn_bsn = 1.4
      if (sdnco_bsn <= 0.) sdnco_bsn = 1.30
      if (bactmx <= 0.) bactmx = 10.
      if (bactminlp <= 0.) bactminlp = .0
      if (bactminp <= 0.) bactminp = 0.
      if (cn_froz <= 0.) cn_froz = .000862
      if (smxco <= 0.) smxco = 1.0
      if (fixco <= 0.) fixco = 0.5
      if (nfixmx <= 0.) nfixmx = 20.0

!!    mike van liew additions for basins.bsn
      if (anion_excl_bsn <= 1.e-6) anion_excl_bsn = 0.2
      if (ch_onco_bsn <= 1.e-6) ch_onco_bsn = 0.0
      if (ch_opco_bsn <= 1.e-6) ch_opco_bsn = 0.0
      if (hlife_ngw_bsn <= 1.e-6) hlife_ngw_bsn = 5.0
      if (rcn_sub_bsn <= 1.e-6) rcn_sub_bsn = 1.0
      if (bc1_bsn <= 1.e-6) bc1_bsn = 0.1
      if (bc2_bsn <= 1.e-6) bc2_bsn = 0.1
      if (bc3_bsn <= 1.e-6) bc3_bsn = 0.02
      if (bc4_bsn <= 1.e-6) bc4_bsn = 0.35
      if (decr_min <= 1.e-6) decr_min = 0.01
!!    mike van liew additions for basins.bsn
      
    ! check parameter values for urban project jaehak 9/15/09 
      if(iuh/=1.and.iuh/=2) then
          iuh = 1
      endif
      if(bf_flg>1.or.bf_flg<0) then
       write(*,*) 'The range of BFLO_DIST in bsn file should be 0-1'
    !  stop
      endif
      if(sed_ch>2) then
       write(*,*) 'Error in choosing channel erosion model:
     &	0-Bagnold, 1-Brownlie, 2-Yang'
       write(*,*) 'Check *.bsn file to correct the error'
    !  stop
      endif
      if (icfac <= 0) icfac = 0
      if (rsd_covco <= 1.e-6) rsd_covco = 0.3


      call caps(petfile)
      call caps(wwqfile)
      open (101,file=wwqfile)

!!    calculate normalization parameters for water, nitrogen, and
!!    phosphorus uptake
      uobn = 0.0
      uobp = 0.0
      uobw = 0.0
      ubw = 10.0       !! the uptake distribution for water is hardwired
                       !! users are not allowed to modify the water
                       !! water uptake distribution
      uobw = 1. - exp(-ubw)
      uobn = 1. - exp(-n_updis)
      uobp = 1. - exp(-p_updis)

!!    determine the shape parameters for the equation which describes area of
!!    snow cover as a function of amount of snow
      call ascrv(.5,.95,sno50cov,.95,snocov1,snocov2)

!!    calculate additional bacteria parameters
      wp20p_plt = wdpf - wgpf
      wp20lp_plt = wdlpf - wglpf
      wpq20 = wdpq - wgpq
      wlpq20 = wdlpq - wglpq
      wps20 = wdps - wgps
      wlps20 = wdlps - wglps

!!    initialize variables (may make these .bsn inputs for user adjustment
!!    at some future time)
      nactfr = 0.02
      abstinit = iabstr

      
      close (103)
      
      if (cswat == 1) then
	 open (100,file="cswat_profile.txt",recl=280)
	 write (100,*) 'year',';','day',';','hru',';','cmass',';','sol_rsd',
     & ';','mancmass'
      end if

       !!add by zhang
      !!=====================
      if (cswat == 2) then
      open (98,file="cswat_profile.txt",recl=356)
       write (98,5102) 'year','day','lay','hru',
     &'sol_mass','sol_cmass','sol_nmass','sol_LS',
     &'sol_LM','sol_LSC','sol_LMC','sol_HSC',
     &'sol_HPC','sol_BMC','sol_LSN','sol_LMN',
     &'sol_HPN','sol_HSN','sol_BMN','sol_no3',
     &'sol_fop','sol_orgp','sol_actp','sol_stap',
     &'sol_solp' 

      open (1001,file="cswat_daily.txt",recl=786)
      write (1001,5104) 'year','day','hru','rsdc','sedc',
     &'percc','latc','emitc','grainc','surfq_c',
     &'stoverc','NPPC','foc','rspc','tot_mass','tot_cmass','tot_nmass',
     &'tot_LSC','tot_LMC','tot_HSC','tot_HPC','tot_BMC','Biom_C','rwtf',
     &'tot_no3_nh3','wdntl', 
     &'ET','Tillfactor','SW1','SW2','SW3','SW4','SW5','SW6','SW7','SW8',
     &'SW9','SW10','SW11',
     &'WFSC1','WFSC2','WFSC3','WFSC4','WFSC5','WFSC6','WFSC7','WFSC8',
     &'WFSC9','WFSC10','WFSC11'
      endif       
      !!add by zhang
      !!=====================

!	open (111, file="final_n_balance.txt")
!	open (112, file="final_yields.txt")
    !! carbon output ends 
     
      
      return
 1000 format (a)
 1001 format (i4)
 5102 format (3a5,30a15)
 5104 format (a4,a4,a8,48a16)
      end