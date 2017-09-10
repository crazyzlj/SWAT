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
!! drainmod tile equations   06/2006
!!	  ranrns(:)  |mm			   |random roughness of operation
!! drainmod tile equations   06/2006
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
!!    lai_init        |none           |initial leaf area index of transplants
!!    auto_eff(:)     |none           |fertilizer application efficiency
!!                                    |calculated as the amount of N applied
!!                                    |divided by the amount of N removed at 
!!                                    |harvest
!!    auto_nyr(:)     |kg NO3-N/ha    |maximum NO3-N content allowed to be
!!                                    |applied in one year
!!    auto_napp(:)    |kg NO3-N/ha    |maximum NO3-N content allowed in one
!!                                    |fertilizer application
!!    auto_nstrs(:)    |none           |nitrogen stress factor which triggers
!!                                    |auto fertilization
!!    auto_wstr(:,:,:)|none or mm     |water stress factor which triggers auto
!!                                    |irrigation
!!    fr_curb(:,:,:)    |none           |availability factor, the fraction of the
!!                                    |curb length that is sweepable
!!    bio_init        |kg/ha          |initial biomass of transplants
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
!!    cnop            |none           |SCS runoff curve number for moisture
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
!!    frt_surface     |none           |fraction of fertilizer which is applied
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
!!    harveff         |none           |harvest efficiency: fraction of harvested
!!                                    |yield that is removed from HRU; the 
!!                                    |remainder becomes residue on the soil
!!                                    |surface
!!    hi_ovr           |(kg/ha)/(kg/ha)|harvest index target specified at 
!!                                    |harvest
!!    hi_targ(:,:,:)    |(kg/ha)/(kg/ha)|harvest index target of cover defined
!!                                    |at planting
!!    hrupest(:)      |none           |pesticide use flag:
!!                                    | 0: no pesticides used in HRU
!!                                    | 1: pesticides used in HRU
!!                                    |initialization
!!    idplt(:)        |none           |land cover code from crop.dat
!!    iurban(:)   |none          |urban simulation code:
!!                               |0  no urban sections in HRU
!!                               |1  urban sections in HRU, simulate using USGS
!!                               |   regression equations
!!                               |2  urban sections in HRU, simulate using build
!!                               |   up/wash off algorithm
!!    manure_id(:)|none               |manure (fertilizer) identification
!!                                    |number from fert.dat
!!    igro(:)         |none           |land cover status code. This code
!!                                    |informs the model whether or not a land
!!                                    |cover is growing at the beginning of 
!!                                    |the simulation
!!                                    |0 no land cover growing
!!                                    |1 land cover growing
!!    iop(:,:,:)      |julian date    |date of tillage operation
!!    ipest(:,:,:)    |none           |pesticide identification number from 
!!                                    |pest.dat
!!    imp_trig        |none           |release/impound action code:
!!                                    |0 begin impounding water
!!                                    |1 release impounded water
!!    irr_salt(:)     |mg/kg          |concentration of salt in irrigation 
!!                                    |water
!!    irrno(:)        |none      |irrigation source location
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
!!    kirr(:)         |NA             |irrigation in HRU
!!    laiday(:)       |m**2/m**2      |leaf area index
!!    nmgt(:)         |none           |management code (for GIS output only)
!!    nope(:)         |none           |sequence number of pesticide in NPNO(:)
!!    npmx            |none           |number of different pesticides used in 
!!                                    |the simulation
!!    npno(:)         |none           |array of unique pesticides used in 
!!                                    |watershed
!!    nrot(:)         |none           |number of years of rotation
!!    phu_plt(:)  |heat units     |total number of heat units to bring
!!                                    |plant to maturity
!!    phuacc(:)       |none           |fraction of plant heat units 
!!                                    |accumulated
!!                                    |continuous fertilization is initialized
!!    phug(:,:,:)     |none           |fraction of plant heat units at which
!!                                    |grazing begins
!!    phut(:,:,:)     |none           |fraction of heat units  (base zero or
!!                                    |plant) at which tillage occurs
!!    pst_kg(:,:,:)   |kg/ha          |amount of pesticide applied to HRU
!! added for pesticide in incorporation in soil 3/31/08 gsm
!!    pst_dep         |mm             |depth of pesticide in the soil
!!    sumix(:)        |none           |sum of all tillage mixing efficiencies 
!!                                    |for HRU
!!                                    |operation
!!    tdrain(:)   |hrs           |time to drain soil to field capacity
!!                                    |yield used in autofertilization
!!    trapeff(:)  |none          |filter strip trapping efficiency (used for
!!                               |everything but bacteria)
!!    urblu(:)    |none          |urban land type identification number from
!!                               |urban.dat
!!    usle_p(:)       |none           |USLE equation support practice (P) factor
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
      integer :: eof
      integer :: ncrp, iro, npl, mon, day, mgt_op, mgt2i, mgt1i, lcr
      integer :: nir, ifn, npst, j, nhv, inop, nhvo, nkill, newpest
      integer :: igr, nairr, nafer, nsw, nrel, icf, mgt3i
      real :: husc, mgt6, mgt9, mgt4, mgt5, mgt7, mgt8
      real :: disc
      
      eof = 0
      iop = 0
      ncrp = 0

!!    read general management parameters
      read (109,5000) titldum
      read (109,*) nmgt(ihru)
      read (109,5000) titldum
      read (109,*) igro(ihru)
      read (109,*) ncrp
      idplt(ihru) = ncrp
      read (109,*) laiday(ihru)
      read (109,*) bio_ms(ihru)
      read (109,*) phu_plt(ihru)
      read (109,5000) titldum
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
 !!     read (109,*) nrot(ihru)
      read (109,5000) titldum 
      read (109,5000) titldum


!!    set pothole trigger
!    if (ipot(ihru) == ihru) then
!        do irot = 1, nrot(ihru)
          imp_trig(ihru) = 0
!        end do
!     end if

!!    set default values
      if (cn2(ihru) <= 35.0) cn2(ihru) = 35.0
      if (cn2(ihru) >= 98.0) cn2(ihru) = 98.0
      if (usle_p(ihru) <= 0.0) usle_p(ihru) = 0.0
      if (usle_p(ihru) >= 1.0) usle_p(ihru) = 1.0
      if (biomix(ihru) <= 0.) biomix(ihru) = .2
      if (urblu(ihru) == 0 .and. urblu(ihru) >= 0) iurban(ihru) = 0  !urban jaehak
      if (irrsc(ihru) <= 0) irrsc(ihru) = 5
      if (irrno(ihru) <= 0) irrno(ihru) = ihru
      if (flowfr(ihru) <= 0.) flowfr(ihru) = 1.0
      if (ddrain(ihru) < 1.e-6) ddrain(ihru) = ddrain_bsn
      if (ddrain(ihru) > .001) then 
      if (tdrain(ihru) <= .001) tdrain(ihru) = 24.
      if (gdrain(ihru) <= .001) gdrain(ihru) = 96.
      end if 

!!    set values for cover/crop already growing
      idplt(ihru) = ncrp
      if (igro(ihru) == 1) then
        igrow = 1
        idplt(ihru) = ncrp
        phuacc(ihru) = .1
	  icr(ihru) = 1
	  icrmx(ihru) = icrmx(ihru) + 1
	  idplrot(icrmx(ihru),ihru) = ncrp              
	  mcrhru(ihru) = mcrhru(ihru) + 1
        curyr_mat(ihru) = mat_yrs(ncrp)
        !! calculate tnylda for autofertilization 
        if (hvsti(ncrp) < 1.) then
          tnylda(ihru) = 350. * cnyld(ncrp) * bio_e(ncrp)
        else
          tnylda(ihru) = 1000. * cnyld(ncrp) * bio_e(ncrp)
        endif
      end if

!!    Set curve number for urban disconnected impervious areas and pervious
!!    areas. This assumes CN2 given in mgt file is for pervious area only
      if (iurban(ihru) > 0) then
        disc = 0.
        disc = fimp(urblu(ihru)) - fcimp(urblu(ihru))
        if (fimp(urblu(ihru)) < 0.30) then
          cn2(ihru) = cn2(ihru) + fimp(urblu(ihru)) *                   
     &                      (urbcn2(urblu(ihru)) - cn2(ihru)) *         
     &                               (1. - disc/(2.* fimp(urblu(ihru))))
        else
          cn2(ihru) = cn2(ihru) + fimp(urblu(ihru)) *                   
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
!!      if (nrot(ihru) > 0) then
        mgt_opprev = 0
!!      read scheduled management practices
        do                                      !! operation loop
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
          read (109,5200,iostat=eof) mon, day, husc, mgt_op, mgt1i,     
     &          mgt2i, mgt3i, mgt4, mgt5, mgt6, mgt7, mgt8, mgt9, mgt10i
          if (eof < 0) then
            if (mgt_opprev /= 17 .and. mgt_opprev /= 0) then
              iop = iop + 1
              mgtop(iop,ihru) = 17
              idop(iop,ihru) = idop(iop-1,ihru)
              phu_op(iop,ihru) = phu_op(iop-1,ihru)
              nopmx(ihru) = nopmx(ihru) + 1
              exit
            end if
          end if
          if (mgt_opprev + mgt_op == 0) then
              close (109)
              return
          endif
          if (mgt_opprev == 17 .and. mgt_op == 0) then
              close (109)
              return
          endif
          mgt_opprev = mgt_op
   
 !! 1/12/2012 in operations if mgt2i and mgt10i are 0 default to irrsc and irrno per jga  
 !!   for manual & auto irrigaton
         if (mgt_op == 2) then
            if (mgt2i <= 0) then
              mgt2i = irrsc(ihru)
            endif
            if (mgt10i <= 0) then
              mgt10i = irrno(ihru)
            endif
         endif 
         
         if (mgt_op == 10) then
            if (mgt2i <= 0) then
              mgt2i = irrsc(ihru)
            endif
            if (mgt10i <= 0) then
              mgt10i = irrno(ihru)
            endif
         endif
 !! above added 
 
        if (mgt_op == 0) then                             !! mgt_op if
           iop = iop + 1
           mgtop(iop,ihru) = 17
           idop(iop,ihru) = idop(iop-1,ihru)
           phu_op(iop,ihru) = phu_op(iop-1,ihru)
        else
          iop = iop + 1
          idop(iop,ihru) = Jdt(ndays,day,mon)
          phu_op(iop,ihru) = husc
          mgtop(iop,ihru) = mgt_op
          mgt1iop(iop,ihru) = mgt1i
          mgt2iop(iop,ihru) = mgt2i
          mgt3iop(iop,ihru) = mgt3i
          mgt4op(iop,ihru) = mgt4
          mgt5op(iop,ihru) = mgt5
          mgt6op(iop,ihru) = mgt6
          mgt7op(iop,ihru) = mgt7
          mgt8op(iop,ihru) = mgt8
          mgt9op(iop,ihru) = mgt9
          mgt10iop(iop,ihru) = mgt10i
          if (mgt_op == 1) then
            idplt(ihru) = mgt1i
            icrmx(ihru) = icrmx(ihru) + 1
            idplrot(icrmx(ihru),ihru) = mgt1i
            mcrhru(ihru) = mcrhru(ihru) + 1
          end if
	    if (mgt_op == 4 .or. mgt_op == 15) then
	      newpest = 0
	      hrupest(ihru) = 1
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
            endif
	    endif
	  end if                                            !! mgt_op if        
		nopmx(ihru) = nopmx(ihru) + 1
        end do                                  !! operation loop
!!    add a skip command to the end of every rotation
!!        iop = iop + 1
!!        mgtop(iop,ihru) = 17
!!        idop(iop,ihru) = idop(iop - 1, ihru)
!!        phu_op(iop,ihru) = phu_op(iop-1,ihru)    
!!     endif  
      close (109)
     
      return
 5000 format (a)
 5200 format (1x,i2,1x,i2,1x,f8.3,1x,i2,1x,i4,1x,i3,1x,i2,1x,f12.5,1x,  
     &        f6.2,1x,f11.5,1x,f4.2,1x,f6.2,1x,f5.2,i12)
      end