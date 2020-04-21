      subroutine readmgt
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU/subbasin management input file
!!    (.mgt). This file contains data related to management practices used in
!!    the HRU/subbasin.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactkddb(:)|none             |bacteria partition coefficient:
!!                                 |1: all bacteria in solution
!!                                 |0: all bacteria sorbed to soil particles
!!    bactlpdb(:)|# bact/kg manure |concentration of less persistent
!!                                 |bacteria in manure(fertilizer)
!!    bactpdb(:) |# bact/kg manure |concentration of persistent bacteria
!!                                 |in manure(fertilizer)
!!    bio_e(:)   |(kg/ha)/(MJ/m**2)|biomass-energy ratio
!!                                 |The potential (unstressed) growth rate per
!!                                 |unit of intercepted photosynthetically
!!                                 |active radiation.
!!    biomix(:)  |none             |biological mixing efficiency.
!!                                 |Mixing of soil due to activity of earthworms
!!                                 |and other soil biota. Mixing is performed at
!!                                 |the end of every calendar year.
!!    cnyld(:)   |kg N/kg yield    |fraction of nitrogen in yield
!!    deptil(:)  |mm               |depth of mixing caused by operation
!!    effmix(:)  |none             |mixing efficiency of operation
!!    fcimp(:)   |fraction         |fraction of HRU area that is classified
!!                                 |as directly connected impervious
!!    fimp(:)    |fraction         |fraction of HRU area that is
!!                                 |impervious (both directly and
!!                                 |indirectly connected)
!!    fminn(:)   |kg minN/kg fert  |fraction of mineral N (NO3 + NH3)
!!    fminp(:)   |kg minP/kg fert  |fraction of mineral P
!!    fnh3n(:)   |kg NH3-N/kg minN |fraction of NH3-N in mineral N
!!    forgn(:)   |kg orgN/kg fert  |fraction of organic N
!!    forgp(:)   |kg orgP/kg fert  |fraction of organic P
!!    hvsti(:)   |(kg/ha)/(kg/ha)  |harvest index: crop yield/aboveground 
!!                                 |biomass
!!    ihru       |none             |HRU number
!!    irr_asq    |                 |surface runoff ratio
!!    irr_mx     |mm               |maximum irrigation amount per auto application
!!    irr_sq     |frac             |surface runoff ratio (0-1) .1 is 10% surface runoff
!!    iurban(:)  |none             |urban simulation code:
!!                                 |0  no urban sections in HRU
!!                                 |1  urban sections in HRU, simulate using USGS
!!                                 |   regression equations
!!                                 |2  urban sections in HRU, simulate using
!!                                 |   build up/wash off algorithm
!!    mp         |none             |maximum number of pesticides used in 
!!                                 |watershed
!!    ndays(:)   |julian date      |julian date for last day of preceding
!!                                 |month (where the array location is the
!!                                 |number of the month) The dates are for
!!                                 |leap years
!!    urbcn2(:)  |none             |Moisture condition II curve number for 
!!                                 |impervious areas 
!!    urblu(:)   |none             |urban land type identification number from
!!                                 |urban.dat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    afrt_surface(:)     |none           |fraction of fertilizer which is applied
!!                                    |to top 10 mm of soil (the remaining 
!!                                    |fraction is applied to first soil 
!!                                    |layer)
!!    lai_init(:,:,:)  |none           |initial leaf area index of transplants
!!    auto_eff(:)     |none           |fertilizer application efficiency
!!                                    |calculated as the amount of N applied
!!                                    |divided by the amount of N removed at 
!!                                    |harvest
!!    auto_nyr(:)    |kg NO3-N/ha    |maximum NO3-N content allowed to be
!!                                    |applied in one year
!!    auto_napp(:)    |kg NO3-N/ha    |maximum NO3-N content allowed in one
!!                                    |fertilizer application
!!    auto_nstrs(:)    |none           |nitrogen stress factor which triggers
!!                                    |auto fertilization
!!    auto_wstr(:,:,:)|none or mm     |water stress factor which triggers auto
!!                                    |irrigation
!!    fr_curb(:,:,:)    |none           |availability factor, the fraction of the
!!                                    |curb length that is sweepable
!!    bio_init(:,:,:) |kg/ha          |initial biomass of transplants
!!    bio_min(:)      |kg/ha          |minimum plant biomass for grazing
!!    bio_ms(:)       |kg/ha          |cover/crop biomass
!!    bio_targ(:,:,:)  |kg/ha          |biomass target
!!    bio_eat(:,:,:)    |(kg/ha)/day    |dry weight of biomass removed by grazing 
!!                                    |daily
!!    bio_trmp(:,:,:)   |(kg/ha)/day    |dry weight of biomass removed by 
!!                                    |trampling daily
!!    cfrt_id(:,:,:)  |none           |fertilizer/manure id number from database
!!    cfrt_kg(:,:,:)  |kg/ha          |amount of fertilzier applied to HRU on a
!!                                    |given day
!!    cn2(:)          |none           |SCS runoff curve number for moisture
!!                                    |condition II
!!    cnop(:,:,:)     |none           |SCS runoff curve number for moisture
!!                                    |condition II
!!    ddrain(:)   |mm            |depth to the sub-surface drain
!!    divmax(:)   |mm H2O or     |maximum daily irrigation diversion from
!!                |  10^4 m^3 H2O|the reach (when IRRSC=1): when value is
!!                               |positive the units are mm H2O; when the
!!                               |value is negative, the units are (10**4
!!                               |m^3 H2O
!!    filterw(:)  |m             |filter strip width for bacteria transport
!!    flowfr(:)   |none          |fraction of available flow in reach that
!!                               |is allowed to be applied to the HRU
!!    flowmin(:)  |m**3/s        |minimum instream flow for irrigation
!!                               |diversions when IRRSC=1, irrigation water
!!                               |will be diverted only when streamflow is
!!                               |at or above FLOWMIN.
!!    frt_kg(:,:,:)   |kg/ha          |amount of fertilizer applied to HRU
!!    frt_surface(:,:,:)  |none           |fraction of fertilizer which is applied
!!                                    |to the top 10 mm of soil (the remaining
!!                                    |fraction is applied to the first soil 
!!                                    |layer)
!!    fsred(:)    |none          |reduction in bacteria loading from filter
!!                               |strip
!!    gdrain(:)   |hrs           |drain tile lag time: the amount of time
!!                               |between the transfer of water from the
!!                               |soil to the drain tile and the release
!!                               |of the water from the drain tile to the
!!                               |reach.
!!    harveff(:,:,:)  |none           |harvest efficiency: fraction of harvested
!!                                    |yield that is removed from HRU; the 
!!                                    |remainder becomes residue on the soil
!!                                    |surface
!!    hi_ovr(:,:,:)    |(kg/ha)/(kg/ha)|harvest index target specified at 
!!                                    |harvest
!!    hi_targ(:,:,:)    |(kg/ha)/(kg/ha)|harvest index target of cover defined
!!                                    |at planting
!!    hrupest(:)      |none           |pesticide use flag:
!!                                    | 0: no pesticides used in HRU
!!                                    | 1: pesticides used in HRU
!!    iafer(:,:,:)    |julian date    |date of auto fertilization 
!!                                    |initialization
!!    iairr(:,:,:)    |julian date    |date of auto irrigation initialization
!!    icfert(:,:,:)   |julian date    |date of continuous fertilization 
!!                                    |initialization
!!    idplt(:,:,:)    |none           |land cover code from crop.dat
!!    iurban(:)   |none          |urban simulation code:
!!                               |0  no urban sections in HRU
!!                               |1  urban sections in HRU, simulate using USGS
!!                               |   regression equations
!!                               |2  urban sections in HRU, simulate using build
!!                               |   up/wash off algorithm
!!    wstrs_id(:,:,:)  |none           |water stress identifier
!!                                    |1: plant water demand
!!                                    |2: soil water deficit
!!    ifert(:,:,:)    |julian date    |date of fertilizer application
!!    ifrt_freq(:,:,:)|days           |number of days between applications in
!!                                    |continuous fertilizer operation
!!    manure_id(:,:,:)   |none           |manure (fertilizer) identification
!!                                    |number from fert.dat
!!    igraz(:,:,:)    |julian date    |date grazing operation begins
!!    igro(:)         |none           |land cover status code. This code
!!                                    |informs the model whether or not a land
!!                                    |cover is growing at the beginning of 
!!                                    |the simulation
!!                                    |0 no land cover growing
!!                                    |1 land cover growing
!!    ihv(:,:,:)      |julian date    |date of harvest and kill operation
!!    ihvo(:,:,:)     |julian date    |date of harvest operation
!!    iir(:,:,:)      |julian date    |date of irrigation operation
!!    ikill(:,:,:)    |julian date    |date of kill operation
!!    iop(:,:,:)      |julian date    |date of tillage operation
!!    ipest(:,:,:)    |none           |pesticide identification number from 
!!                                    |pest.dat
!!    iplant(:,:,:)   |julian date    |date of planting/beginning of growing 
!!                                    |season
!!    ipst(:,:,:)     |julian date    |date of pesticide application
!!    imp_trig(:,:,:) |none           |release/impound action code:
!!                                    |0 begin impounding water
!!                                    |1 release impounded water
!!    irelease(:,:,:) |julian date    |date of impound/release operation
!!    irr_amt(:,:,:)  |mm H2O         |depth of irrigation water applied to 
!!                                    |HRU
!!    irr_salt(:,:,:) |mg/kg          |concentration of salt in irrigation 
!!                                    |water
!!    irrno(:)    |none          |irrigation source location
!!                               |if IRRSC=1, IRRNO is the number of the
!!                               |          reach
!!                               |if IRRSC=2, IRRNO is the number of the
!!                               |          reservoir
!!                               |if IRRSC=3, IRRNO is the number of the
!!                               |          subbasin
!!                               |if IRRSC=4, IRRNO is the number of the
!!                               |          subbasin
!!                               |if IRRSC=5, not used
!!    irrsc(:)    |none          |irrigation source code:
!!                               |1 divert water from reach
!!                               |2 divert water from reservoir
!!                               |3 divert water from shallow aquifer
!!                               |4 divert water from deep aquifer
!!                               |5 divert water from source outside
!!                               |  watershed
!!    isweep(:,:,:)   |julian date    |date of street sweeping operation
!!    kirr(:)         |NA             |irrigation in HRU
!!    laiday(:)       |m**2/m**2      |leaf area index
!!    fert_days(:,:,:)   |none           |number of days continuous fertilization
!!                                    |will be simulated
!!    grz_days(:,:,:)   |none           |number of days grazing will be simulated
!!    nmgt(:)         |none           |management code (for GIS output only)
!!    nope(:)         |none           |sequence number of pesticide in NPNO(:)
!!    npmx            |none           |number of different pesticides used in 
!!                                    |the simulation
!!    npno(:)         |none           |array of unique pesticides used in 
!!                                    |watershed
!!    nrot(:)         |none           |number of years of rotation
!!    phu_plt(1,1,:)  |heat units     |total number of heat units to bring
!!                                    |plant to maturity
!!    phuacc(:)       |none           |fraction of plant heat units 
!!                                    |accumulated
!!    phuaf(:,:,:)    |none           |fraction of plant heat units at which
!!                                    |auto fertilization is initialized
!!    phuai(:,:,:)    |none           |fraction of plant heat units at which
!!                                    |auto irrigation is initialized
!!    phucf(:,:,:)    |none           |fraction of plant heat units at which
!!                                    |continuous fertilization is initialized
!!    phug(:,:,:)     |none           |fraction of plant heat units at which
!!                                    |grazing begins
!!    phuh(:,:,:)     |none           |fraction of plant heat units at which
!!                                    |harvest and kill operation occurs
!!    phuho(:,:,:)    |none           |fraction of plant heat units at which
!!                                    |harvest operation occurs
!!    phuimp(:,:,:)   |none           |fraction of heat units at which impound
!!                                    |release operation occurs
!!    phuirr(:,:,:)   |none           |fraction of plant heat units at which
!!                                    |irrigation occurs
!!    phuk(:,:,:)     |none           |fraction of plant heat units at which
!!                                    |kill operation occurs
!!    phun(:,:,:)     |none           |fraction of plant heat units at which
!!                                    |fertilization occurs
!!    phup(:,:,:)     |none           |fraction of solar heat units at which 
!!                                    |planting occurs
!!    phupst(:,:,:)   |none           |fraction of plant heat units at which
!!                                    |pesticide application occurs
!!    phusw(:,:,:)    |none           |fraction of heat units at which sweeping
!!                                    |operation occurs
!!    phut(:,:,:)     |none           |fraction of heat units  (base zero or
!!                                    |plant) at which tillage occurs
!!    pst_kg(:,:,:)   |kg/ha          |amount of pesticide applied to HRU
!! added for pesticide in incorporation in soil 3/31/08 gsm
!!    pst_dep(:,:)    |mm             |depth of pesticide in the soil
!!    sumix(:)        |none           |sum of all tillage mixing efficiencies 
!!                                    |for HRU
!!    sweepeff(:,:,:) |none           |removal efficiency of sweeping 
!!                                    |operation
!!    tdrain(:)   |hrs           |time to drain soil to field capacity
!!    tnylda(:,:,:)   |kg N/kg yield  |estimated/target nitrogen content of
!!                                    |yield used in autofertilization
!!    trapeff(:)  |none          |filter strip trapping efficiency (used for
!!                               |everything but bacteria)
!!    urblu(:)    |none          |urban land type identification number from
!!                               |urban.dat
!!    usle_p(:)       |none           |USLE equation support practice (P) factor
!!    manure_kg(:,:,:)  |(kg/ha)/day    |dry weight of manure deposited on HRU 
!!                                    |daily
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    day         |none          |day operation occurs
!!    husc        |none          |heat unit scheduling for operation expressed
!!                               |as fraction of total heat units of crop
!!                               |at maturity
!!    icf         |none          |number of continuous fertilizer operation
!!                               |in year
!!    ifn         |none          |number of fertilizer application in year
!!    igr         |none          |number of grazing operation in year
!!    inop        |none          |number of tillage operation in year
!!    iro         |none          |counter for years of rotation
!!    j           |none          |counter
!!    lcr         |none          |crop id number
!!    mgt_op      |none          |operation code number
!!                               |0 end of rotation year
!!                               |1 plant/beginning of growing season
!!                               |2 irrigation operation
!!                               |3 fertilizer application
!!                               |4 pesticide application
!!                               |5 harvest and kill operation
!!                               |6 tillage operation
!!                               |7 harvest only operation
!!                               |8 kill/end of growing season
!!                               |9 grazing operation
!!                               |10 auto irrigation initialization
!!                               |11 auto fertilizer initialization
!!                               |12 street sweeping operation
!!                               |13 release/impound operation
!!                               |14 continuous fertilization operation
!!    mgt1i       |none          |first management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt2i       |none          |second management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt3i       |none          |third management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt4        |none          |fourth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt5        |none          |fifth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt6        |none          |sixth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt7        |none          |seventh management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt8        |none          |eighth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt9        |none          |ninth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mon         |none          |month operation occurs
!!    nafer       |none          |number of auto fertilization operation in
!!                               |year
!!    nairr       |none          |number of auto irrigation operation in year
!!    ncrp        |none          |land cover identification number 
!!                               |(from crop.dat). Need only if IGRO=1.
!!    newpest     |none          |pesticide flag
!!    nhv         |none          |number of harvest and kill operation in
!!                               |year
!!    nhvo        |none          |number of harvest operation in year
!!    nir         |none          |number of irrigation operation in year
!!    nkill       |none          |number of kill operation in year
!!    npl         |none          |number of planting operation in year
!!    npst        |none          |number of pesticide application in year
!!    nrel        |none          |number of release/impound operations in year
!!    nsw         |none          |number of street sweeping operation in year
!!    titldum     |NA            |title line from input dataset
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Jdt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
      integer :: ncrp, iro, npl, mon, day, mgt_op, mgt2i, mgt1i, lcr
      integer :: nir, ifn, npst, j, nhv, inop, nhvo, nkill, newpest
      integer :: igr, nairr, nafer, nsw, nrel, icf, mgt3i
      real :: husc, mgt6, mgt9, mgt4, mgt5, mgt7, mgt8
      real :: disc

      lcr = 0
      ncrp = 0
      npl = 1 
      nir = 0
      ifn = 0
      npst = 0
      nhv = 1
      inop = 0
      nhvo = 0
      nkill = 1
      icf = 0
      icp = 0
      ibrn = 0
      igr = 0
      nairr = 1
      nafer = 0
      icnp = 1
      nsw = 0
      nrel = 0
      igrow = 0

!!    read general management parameters
      read (109,5000) titldum
      read (109,*) nmgt(ihru)
      read (109,5000) titldum
      read (109,*) igro(ihru)
      read (109,*) ncrp
      read (109,*) laiday(ihru)
      read (109,*) bio_ms(ihru)
      read (109,*) phu_plt(1,1,ihru)
      read (109,5000) titldum
!     read (109,*) curyr_mat(ihru)
      read (109,*) biomix(ihru)
      read (109,*) cn2(ihru)
      read (109,*) usle_p(ihru)
      read (109,*) bio_min(ihru)
      read (109,*) filterw(ihru)
      read (109,5000) titldum
      read (109,*) iurban(ihru)
      read (109,*) urblu(ihru)
      read (109,5000) titldum
      read (109,*) irrsc(ihru)
      read (109,*) irrno(ihru)
      read (109,*) flowmin(ihru)
      read (109,*) divmax(ihru)
      read (109,*) flowfr(ihru)
      read (109,5000) titldum
      read (109,*) ddrain(ihru)
      read (109,*) tdrain(ihru)
      read (109,*) gdrain(ihru)
      read (109,5000) titldum
      read (109,*) nrot(ihru)
      read (109,5000) titldum

!!    set pothole trigger
      if (ipot(ihru) == ihru) then
        do irot = 1, nrot(ihru)
          imp_trig(irot,1,ihru) = 0
        end do
      end if

!!    set default values
      if (cn2(ihru) <= 35.0) cn2(ihru) = 35.0
      if (cn2(ihru) >= 98.0) cn2(ihru) = 98.0
      if (usle_p(ihru) <= 0.0) usle_p(ihru) = 0.0
      if (usle_p(ihru) >= 1.0) usle_p(ihru) = 1.0
      if (biomix(ihru) <= 0.) biomix(ihru) = .2
      if (iurban(ihru) == 2 .and. urblu(ihru) <= 0) urblu(ihru) = 1
      if (irrsc(ihru) <= 0) irrsc(ihru) = 5
      if (irrno(ihru) <= 0) irrno(ihru) = ihru
      if (flowfr(ihru) <= 0.) flowfr(ihru) = 1.0
      if (ddrain(ihru) > .001) then 
        if (tdrain(ihru) <= .001) tdrain(ihru) = 24.
        if (gdrain(ihru) <= .001) gdrain(ihru) = 96.
      end if 

!!    set values for cover/crop already growing
      if (igro(ihru) == 1) then
        igrow = 1
        idplt(1,1,ihru) = ncrp
        idplt(1,2,ihru) = ncrp
        lcr = ncrp
        phuacc(ihru) = .1
        npl = 1
        nhv = 0
        !! calculate tnylda for autofertilization 
        if (hvsti(ncrp) < 1.) then
          tnylda(1,1,ihru) = 350. * cnyld(ncrp) * bio_e(ncrp)
        else
          tnylda(1,1,ihru) = 1000. * cnyld(ncrp) * bio_e(ncrp)
        endif
      end if

!!    Set curve number for urban disconnected impervious areas and pervious
!!    areas. This assumes CN2 given in mgt file is for pervious area only
      if (iurban(ihru) > 0) then
        disc = 0.
        disc = fimp(urblu(ihru)) - fcimp(urblu(ihru))
        if (fimp(urblu(ihru)) < 0.30) then
          cn2(ihru) = cn2(ihru) + fimp(urblu(ihru)) *                   &
     &                      (urbcn2(urblu(ihru)) - cn2(ihru)) *         &
     &                               (1. - disc/(2.* fimp(urblu(ihru))))
        else
          cn2(ihru) = cn2(ihru) + fimp(urblu(ihru)) *                   &
     &                                 (urbcn2(urblu(ihru)) - cn2(ihru))
        endif
      endif

!!    Filter strip calculations
      if (filterw(ihru) > 0.) then
        fsred(ihru) = 1. - ((12. + 4.5 * filterw(ihru)) / 100.)
        trapeff(ihru) = 0.367 * filterw(ihru)**0.2967
      else
        fsred(ihru) = 1.
        trapeff(ihru) = 0.
      endif
      fsred(ihru) = Min(fsred(ihru), 1.)
      fsred(ihru) = Max(fsred(ihru), 0.)
      trapeff(ihru) = Min(trapeff(ihru), 1.)
      trapeff(ihru) = Max(trapeff(ihru), 0.)

!!    If years of rotation are set to zero, assume continuous fallow. For
!!    continuous fallow, no management practices allowed.
      if (nrot(ihru) > 0) then

!!      read scheduled management practices
        do iro = 1, nrot(ihru)                  !! rotation loop

          do                                    !! operation loop
          mon = 0
          day = 0
          husc = 0.
          mgt_op = 0
          mgt1i = 0
          mgt2i = 0
          mgt3i = 0
          mgt4 = 0.

          mgt5 = 0.
          mgt6 = 0
          mgt7 = 0.
          mgt8 = 0.
          mgt9 = 0.

          read (109,5200) mon, day, husc, mgt_op, mgt1i, mgt2i, mgt3i,  &
     &                   mgt4, mgt5, mgt6, mgt7, mgt8, mgt9
 

          select case (mgt_op)

          case (0)  !! end of rotation year

            !! this defines a crop for the model to recognize between harvest
            !! and the end of the year. The model does not simulate growth
            !! of the crop--it is needed to set parameters in erosion processes
            idplt(iro+1,1,ihru) = lcr

            !! the following equations set values for fallow years
            if (idplt(iro,1,ihru) == 0) idplt(iro,1,ihru) = lcr
            if (phu_plt(iro,1,ihru) == 0.)                              &
     &                         phu_plt(iro,1,ihru) = phu_plt(iro,2,ihru)

            !! re-initialize annual counters
            npl = 1
            if (igrow == 1) then
              nhv = 0
            else
              nhv = 1
            end if 
            nkill = 1
            nhvo = 0
            npst = 0
            nir = 0
            ifn = 0
            inop = 0
            igr = 0
            nairr = 1
            nafer = 0
	      nrel = 0
            icnp = 1
            nsw = 0
            icf = 0
            icp = 0
            exit

          case (1)  !! plant operation
            igrow = 1
            if (igro(ihru) == 1) idplt(1,1,ihru) = mgt1i
            npl = npl + 1
            idplt(iro,npl,ihru) = mgt1i
            idplt(iro,npl+1,ihru) = mgt1i
            lcr = mgt1i
            iplant(iro,npl,ihru) = Jdt(ndays,day,mon)
            if (mgt4 < 700.) mgt4 = 1700.
            if (mgt4 > 5000.) mgt4 = 5000.
            phu_plt(iro,npl,ihru) = mgt4
            phu_plt(iro,npl+1,ihru) = mgt4
            if (isproj == 2) then
              if (husc > .5) husc = .15      !!CEAP fix for winter crops
            end if
            if (husc > 0.) then
              phup(iro,npl,ihru) = husc
              if (husc > .5) phup(iro,npl+1,ihru) = husc
            endif
            cnop(iro,icnp,ihru) = mgt9
            icnp = icnp + 1
            curyr_mat(ihru) = mgt3i
            hi_targ(iro,npl,ihru) = mgt7
            bio_targ(iro,npl,ihru) = mgt8 * 1000.
            lai_init(iro,npl,ihru) = mgt5
            bio_init(iro,npl,ihru) = mgt6
          
            !! calculate tnylda for autofertilization 
            if (hvsti(mgt1i) < 1.) then
              tnylda(iro,npl,ihru) = 350. * cnyld(mgt1i) * bio_e(mgt1i)
            else
              tnylda(iro,npl,ihru) = 1000. * cnyld(mgt1i) * bio_e(mgt1i)
            endif

          case (2)  !! irrigation operation
            kirr(ihru) = "x"
            nir = nir + 1
            irr_amt(iro,nir,ihru) = mgt4
            iir(iro,nir,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) then
              if (igrow == 1) then 
                phuirr(iro,nir,ihru) = husc
              else
                phuirr_nocrop(iro,nir,ihru) = husc
              end if
            end if 
            irr_salt(iro,nir,ihru) = mgt5
            irr_efm(iro,nir,ihru) = mgt6
            irr_sq(iro,nir,ihru) = mgt7
            if (irr_efm(iro,nir,ihru) < 1.e-6)irr_efm(iro,nir,ihru) =1.0
!!!! Srin's irrigation source by each application changes
            if (mgt2i <= 0) mgt2i = irrsc(ihru)
            if (mgt3i <= 0) mgt3i = irrno(ihru)
            irr_sc(iro,nir,ihru) = mgt2i
            irr_no(iro,nir,ihru) = mgt3i
!!!! Srin's irrigation source by each application changes

        
          case (3)  !! fertilizer operation
            if (mgt1i > 0) then           !! no fertilizer id #, ignore operation
              ifn = ifn + 1
              ifert(iro,ifn,ihru) = Jdt(ndays,day,mon)
              if (husc > 0.) then
                if (igrow == 1) then 
                  phun(iro,ifn,ihru) = husc
                else
                  phun_nocrop(iro,ifn,ihru) = husc
                end if
              end if
              frt_surface(iro,ifn,ihru) = mgt5
              if (frt_surface(iro,ifn,ihru) <= 1.e-6)                   &
     &                                    frt_surface(iro,ifn,ihru) = .2
              ifrttyp(iro,ifn,ihru) = mgt1i
              frt_kg(iro,ifn,ihru) = mgt4
            end if

          case (4)  !! pesticide application
            hrupest(ihru) = 1
            npst = npst + 1
            ipst(iro,npst,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) then
              if(igrow == 1) then
                phupst(iro,npst,ihru) = husc
              else
                phupst_nocrop(iro,npst,ihru) = husc
              end if
            end if
            pst_kg(iro,npst,ihru) = mgt4
            ipest(iro,npst,ihru) = mgt1i
!!  added line below for pesticide 3/31/08 gsm
            pst_dep(iro,npst,ihru) = mgt5

            newpest = 0
            do j = 1, npmx
              if (mgt1i == npno(j)) then
                newpest = 1
                exit
              endif
            end do
            if (newpest == 0) then
              npno(npmx) = mgt1i
              nope(mgt1i) = npmx
              npmx = npmx + 1
            end if

          case (5)  !! harvest and kill operation
            nhv = nhv + 1
            igrow = 0
            ihv(iro,npl,ihru) = Jdt(ndays,day,mon)
            if (nhv > 1) then
              if (ihv(iro,nhv-1,ihru) <= 0) then
                ihv(iro,nhv-1,ihru) = ihv(iro,nhv,ihru)
              end if
            end if 
            if (husc > 0.) then
              phuh(iro,npl,ihru) = husc
              phuh(iro,npl+1,ihru) = husc
            endif
            cnop(iro,icnp,ihru) = mgt4
            icnp = icnp + 1
!!! add fraction stover removed for harvest and kill
            frac_harvk(iro,icnp,ihru) = mgt5

          case (6)  !! tillage operation
            inop = inop + 1
            iop(iro,inop,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) then
              if (igrow == 1) then
                phut(iro,inop,ihru) = husc
              else
                phut_nocrop(iro,inop,ihru) = husc
              end if
            end if
            idtill(iro,inop,ihru) = mgt1i
            cnop(iro,icnp,ihru) = mgt4
            icnp = icnp + 1

          case (7)  !! harvest only operation
            nhvo = nhvo + 1
            ihvo(iro,nhvo,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) phuho(iro,nhvo,ihru) = husc
            ihv_gbm(iro,nhvo,ihru) = mgt2i
            hi_ovr(iro,nhvo,ihru) = mgt5
            harveff(iro,nhvo,ihru) = mgt4
            if (harveff(iro,nhvo,ihru) <= 0.) then
              harveff(iro,nhvo,ihru) = 1.
            endif

          case (8)  !! kill operation
            nkill = nkill + 1
            igrow = 0
            ikill(iro,npl,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) phuk(iro,npl,ihru) = husc
!           idplt(iro,npl+1,ihru) = lcr

          case (9)  !! grazing operation
            igr = igr + 1
            igraz(iro,igr,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) phug(iro,igr,ihru) = husc
            if (bio_min(ihru) <= 1.e-4) bio_min(ihru) = 3000.
            bio_eat(iro,igr,ihru) = mgt4
            grz_days(iro,igr,ihru) = mgt1i
            bio_trmp(iro,igr,ihru) = mgt5
            manure_kg(iro,igr,ihru) = mgt6
            if (manure_kg(iro,igr,ihru) <= 0.) then
              manure_kg(iro,igr,ihru) = .95 * mgt4
            endif
      !!     manure_id(iro,igr,ihru) = 1
             manure_id(iro,igr,ihru) = mgt2i

          case (10)  !! auto irrigation operation
            nairr = nairr + 1
            iairr(iro,nairr-1,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) phuai(iro,nairr-1,ihru) = husc
            if (mgt1i <= 0) mgt1i = 1
            wstrs_id(iro,nairr,ihru) = mgt1i
            auto_wstr(iro,nairr,ihru) = mgt4
            irr_eff(iro,nairr,ihru) = mgt5					     !! BN: inserted to account for irr.efficiency
            irr_asq(iro,nairr,ihru) = mgt7
            irr_mx(iro,nairr,ihru) = mgt6
            if (irr_eff(iro,nairr,ihru) > 1.)                           &    !! BN: inserted (see below)
     &              irr_eff(iro,nairr,ihru) = 1.				     !! BN: inserted: irr.efficiency cannot be > 1
            if (irr_eff(iro,nairr,ihru) == 0.)                          &    !! BN: inserted (see below)
     &              irr_eff(iro,nairr,ihru) = 1.                             !! BN: inserted: irr.efficiency cannot be 0
 !!    change per JGA for irrigation 4/2/2009
            if (irr_mx(iro,nairr,ihru) < 1.e-6)                         & 
     &		   irr_mx (iro,nairr,ihru) = 25.4
             
            kirr(ihru) = "x"
!!!! Srin's irrigation source by each application changes
          if (mgt2i <= 0) mgt2i = irrsc(ihru)
          if (mgt3i <= 0) mgt3i = irrno(ihru)
          irr_sca(iro,nairr,ihru) = mgt2i
          irr_noa(iro,nairr,ihru) = mgt3i
!!!! Srin's irrigation source by each application changes


          case (11)  !! auto fertilizer operation
            if (mgt1i > 0) then  
              nafer = nafer + 1
              iafer(iro,nafer,ihru) = Jdt(ndays,day,mon)
              if (husc > 0.) phuaf(iro,nafer,ihru) = husc
              afrt_surface(ihru) = mgt8
              if (afrt_surface(ihru) <= 1.e-6) afrt_surface(ihru) = .2
              auto_nstrs(ihru) = mgt4
              iafrttyp(ihru) = mgt1i
              auto_napp(ihru) = mgt5
              if (auto_napp(ihru) <= 0.) auto_napp(ihru) = 200.
              auto_nyr(ihru) = mgt6
              if (auto_nyr(ihru) <= 0.) auto_nyr(ihru) = 300.
              auto_eff(ihru) = mgt7
              if (auto_eff(ihru) <= 0.) auto_eff(ihru) = 1.3
            end if
          
          case (12)  !! street sweeping  (only if IURBAN=2)
            nsw = nsw + 1
            isweep(iro,nsw,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) then
              if (igrow == 1) then
                phusw(iro,nsw,ihru) = husc
              else
                phusw_nocrop(iro,nsw,ihru) = husc
              end if
            end if
            sweepeff(iro,nsw,ihru) = mgt4
            fr_curb(iro,nsw,ihru) = mgt5
            if (fr_curb(iro,nsw,ihru) <= 0.) fr_curb(iro,nsw,ihru) = 1.0
        
          case (13)  !! release/impound water in rice fields
            nrel = nrel + 1
            imp_trig(iro,nrel+1,ihru) = mgt1i
            irelease(iro,nrel,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) then
              if (igrow == 1) then
                phuimp(iro,nrel,ihru) = husc
              else
                phuimp_nocrop(iro,nrel,ihru) = husc
              end if
            end if

          case (14)  !! continuous fertilization operation
            icf = icf + 1
            icfert(iro,icf,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) phucf(iro,icf,ihru) = husc
            fert_days(iro,icf,ihru) = mgt1i
            cfrt_kg(iro,icf,ihru) = mgt4
            cfrt_id(iro,icf,ihru) = mgt2i
            ifrt_freq(iro,icf,ihru) = mgt3i
            if (ifrt_freq(iro,icf,ihru) <= 0) then
              ifrt_freq(iro,icf,ihru) = 1
            end if
 
          case (15)  !! continuous pesticide operation
            icp = icp + 1
            icpest(iro,icp,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) phucp(iro,icp,ihru) = husc
            pest_days(iro,icp,ihru) = mgt2i
            cpst_kg(iro,icp,ihru) = mgt4
            cpst_id(iro,icp,ihru) = mgt1i
            ipst_freq(iro,icp,ihru) = mgt3i
            if (ipst_freq(iro,icp,ihru) <= 0) then
              ipst_freq(iro,icp,ihru) = 1
            end if

            newpest = 0
            do j = 1, npmx
              if (mgt1i == npno(j)) then
                newpest = 1
                exit
              endif
            end do
            if (newpest == 0) then
              npno(npmx) = mgt1i
              nope(mgt1i) = npmx
              npmx = npmx + 1
            end if


	    case (16)  !! burning
            ibrn = ibrn + 1
            iburn(iro,ibrn,ihru) = Jdt(ndays,day,mon)
            if (husc > 0.) phub(iro,ibrn,ihru) = husc
            burn_frlb(iro,ibrn,ihru) = mgt4

            end select
          end do                                !! operation loop
 

        if (iro == nrot(ihru)) exit
        end do                                  !! rotation loop

        idplt(1,1,ihru) = lcr
        do irotate = 2, nrot(ihru)
         if (idplt(irotate,1,ihru) == 0) idplt(irotate,1,ihru) =        &
     &     idplt(irotate-1,1,ihru)
         if (tnylda(irotate,2,ihru) == 0) tnylda(irotate,2,ihru) =      &
     &     tnylda(irotate-1,2,ihru)
         if (tnylda(irotate,1,ihru) == 0) tnylda(irotate,1,ihru) =      &
     &     tnylda(irotate,2,ihru)
        enddo
      end if

      close (109)
     
      return
 5000 format (a)
 5200 format (1x,i2,1x,i2,1x,f8.3,1x,i2,1x,i4,1x,i3,1x,i2,1x,f12.5,1x,  &
     &        f6.2,1x,f11.5,1x,f4.2,1x,f6.2,1x,f5.2)
      end
