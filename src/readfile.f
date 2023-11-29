      subroutine readfile

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   this subroutine opens the main input and output files and reads watershed
!!   information from the file.cio

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mhruo       |none          |maximum number of variables in output.hru file
!!    mrcho       |none          |maximum number of variables in output.rch file
!!    msubo       |none          |maximum number of variables in output.sub file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    calfile     |NA          |name of file containing calibration parameters
!!    fcstcycles  |none        |number of times forecast period is simulated
!!                             |(using different weather generator seeds each
!!                             |time)
!!    fcstday     |julian date |beginning date of forecast period
!!    fcstyr      |year        |beginning year of forecast period
!!    iclb        |none        |auto-calibration flag
!!    idaf        |julian date |beginning day of simulation
!!    idal        |julian date |ending day of simulation
!!    idg(:)      |none        |array location of random number seed
!!                             |used for a given process
!!    idist       |none        |rainfall distribution code
!!                             |  0 for skewed normal dist
!!                             |  1 for mixed exponential distribution
!!    idt         |minutes     |length of time step used to report
!!                             |precipitation data for sub-daily modeling
!!    igen        |none        |random number generator seed code
!!    ilog        |none        |streamflow print code
!!    iprint      |none        |print code:0=monthly,1=daily,2=annual
!!    ipdhru(:)   |none        |HRUs whose output information will be
!!                             |printed to the output.hru and output.wtr
!!                             |files
!!    ipdvab(:)   |none        |output variable codes for output.sub file
!!    ipdvar(:)   |none        |output variable codes for output.rch file
!!    ipdvas(:)   |none        |output varaible codes for output.hru file
!!    iprp        |none        |print code for output.pes file
!!                             |0 do not print pesticide output
!!                             |1 print pesticide output
!!    isproj      |none        |special project code:
!!                             |1 test rewind (run simulation twice)
!!    itotb       |none        |number of output variables printed 
!!                             |(output.sub)
!!    itoth       |none        |number of HRUs printed (output.hru/output.wtr)
!!    itotr       |none        |number of output variables printed (output.rch)
!!    itots       |none        |number of output variables printed (output.hru)
!!    iyr         |year        |beginning year of simulation
!!    nbyr        |none        |number of calendar years simulated
!!    nhtot       |none        |number of relative humidity records in file
!!    nrgage      |none        |number of raingage files
!!    nrgfil      |none        |number of rain gages per file
!!    nrtot       |none        |total number of rain gages
!!    nstep       |none        |number of lines of rainfall data for each
!!                             |day
!!    nstot       |none        |number of solar radiation records in file
!!    ntgage      |none        |number of temperature gage files
!!    ntgfil      |none        |number of temperature gages per file
!!    nttot       |none        |total number of temperature gages
!!    nwtot       |none        |number of wind speed records in file
!!    nyskip      |none        |number of years to not print output
!!    pcpsim      |none        |rainfall input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    rcor        |none        |correction coefficient for generated rainfall
!!                             |to ensure that the annual means for generated
!!                             |and observed values are comparable. (needed
!!                             |only if IDIST=1)
!!    rexp        |none        |value of exponent for mixed exponential
!!                             |rainfall distribution (needed only if
!!                             |IDIST=1)
!!    rfile(:)    |NA          |rainfall file names (.pcp)
!!    rhfile      |NA          |relative humidity file name (.hmd)
!!    rhsim       |none        |relative humidity input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    rndseed(:,:)|none        |random number generator seed
!!    slrfile     |NA          |solar radiation file name (.slr)
!!    slrsim      |none        |solar radiation input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    tfile(:)    |NA          |temperature file names (.tmp)
!!    title       |NA          |description lines in file.cio(1st 3 lines)
!!    tmpsim      |none        |temperature input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    wndfile     |NA          |wind speed file name (.wnd)
!!    wndsim      |none        |wind speed input code
!!                             |1 measured data read for each subbasin
!!                             |2 data simulated for each subbasin
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bsnfile     |NA          |name of basin input file (.bsn)
!!    fcstfile    |NA          |name of weather forecast data file (.cst
!!    fertdb      |NA          |name of fertilizer database file (fert.dat)
!!    figfile     |NA          |name of watershed configuration file (.fig)
!!    ii          |none        |counter
!!    j           |none        |counter
!!    pestdb      |NA          |name of pesticide database input file(pest.dat)
!!    plantdb     |NA          |name of LU/LC database input file (crop.dat)
!!    rn          |none        |random number generator seed
!!    sumv        |none        |variable to hold intermediate calculation
!!    tilldb      |NA          |name of tillage database input file(till.dat)
!!    urbandb     |NA          |name of urban database file (urban.dat)
!!    xx          |none        |random number between 0.0 and 1.0
!!    septdb      |none        |name of pesticide database input file(septwq.dat) !! CS
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: gcycl, caps, Aunif
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=13) :: figfile, bsnfile, plantdb, tilldb, urbandb, 
     &    pestdb, fertdb, fcstfile
	
      character (len=80) :: titldum
      real*8 :: sumv, xx
      integer :: rn, j, ii, eof
      eof = 0

      bsnfile = ""
      fcstfile = ""
      plantdb = ""
      fertdb = ""
      pestdb = ""
      figfile = ""
      tilldb = ""
      urbandb = ""
      septdb = ""   !!SEPTIC CHANGES GSM 1/30/09

      open (101,file="file.cio")

!! Read project description
      read (101,5101) titldum
      read (101,5101) titldum
      read (101,5100) title

!! Read general information/watershed configuration
      read (101,5101) titldum
      read (101,5000) figfile
      read (101,*) nbyr
      read (101,*) iyr
      read (101,*) idaf
      read (101,*) idal

      call caps(figfile)
      open (102,file=figfile)

!! Read climate information
      read (101,5101) titldum
      read (101,*) igen
      read (101,*) pcpsim
      read (101,*) idt
      read (101,*) idist
      read (101,*) rexp
      read (101,*) nrgage
      read (101,*) nrtot
      read (101,*) nrgfil
      read (101,*) tmpsim
      read (101,*) ntgage
      read (101,*) nttot
      read (101,*) ntgfil
      read (101,*) slrsim
      read (101,*) nstot
      read (101,*) rhsim
      read (101,*) nhtot
      read (101,*) wndsim
      read (101,*) nwtot
      read (101,*) fcstyr
      read (101,*) fcstday
      read (101,*) fcstcycles
      read (101,5101) titldum
      read (101,5000) (rfile(j),j = 1,18)
      read (101,5101) titldum
      read (101,5000) (tfile(j),j = 1,18)
      read (101,5000) slrfile
      read (101,5000) rhfile
      read (101,5000) wndfile
      read (101,5000) fcstfile
 
      !! calculate precipitation data lines per day
      if (idt > 0) nstep = 1440 / idt
!!    added air and soil temperature file for carbon
!!    can be commented if needed by user
!     open (120, file='air_soil.out')
!     write (120,12111)
!12111  format ('  Day','  Hru','  Tmax','   Tmin','   Soil Temp for
!     & Soil Layers')

      call gcycl

      !! calculate values related to exponential rainfall distribution
      if (idist == 1) then
        if (rexp <= 0.) rexp = 1.3
        sumv = 0.
        rn = 0
        rn = rndseed(idg(3),1)
        do j = 1, 10000
          xx = 0.
          xx = Aunif(rn)
          sumv = sumv + (-Log(xx))**rexp
        end do
        if (sumv > 0.) then
          rcor = 10100. / sumv
        else
          rcor = 1.
        endif
      endif

      if (nrgfil <= 0) nrgfil = 10
      if (nrtot <= 0) nrtot = nrgage * nrgfil
      if (ntgfil <= 0) ntgfil = 10
      if (nttot <= 0) nttot = nrgage * ntgfil

      !! check for values on forecast variables
      if (fcstyr > 0 .and. fcstday > 0) then
        if (fcstcycles <= 0) fcstcycles = 1
      else
        fcstcycles = 1
      end if

      do j = 1, nrgage
        call caps(rfile(j))
      end do
      do j = 1, ntgage
        call caps(tfile(j))
      end do
      call caps(slrfile)
      call caps(rhfile)
      call caps(wndfile)

      call caps(fcstfile)
      if (fcstfile /= '             ') then 
        open (109,file=fcstfile)
      else
        fcstyr = 0
        fcstday = 0
      end if

!!Open watershed modeling option file
      read (101,5101) titldum
      read (101,5000) bsnfile

      call caps(bsnfile)
      open (103,file=bsnfile)

!!Open database files
      read (101,5101) titldum
      read (101,5000) plantdb
      read (101,5000) tilldb
      read (101,5000) pestdb
      read (101,5000) fertdb
      read (101,5000) urbandb

      call caps(plantdb)
      call caps(tilldb)
      call caps(pestdb)
      call caps(fertdb)
      call caps(urbandb)
      open (104,file=plantdb)
      open (105,file=tilldb)
      open (106,file=pestdb)
      open (107,file=fertdb)
      open (108,file=urbandb)
	

!!Special Projects input
      read (101,5101) titldum
      read (101,*) isproj
      read (101,*) iclb
      read (101,5000) calfile

!!Output Information input
      read (101,5101) titldum
      read (101,*) iprint
      read (101,*) nyskip
!!! check that nyskip input is not greater or equal to nbyr input
      if (nyskip >= nbyr) nyskip = nbyr - 1
      
      read (101,*) ilog
      read (101,*) iprp
      read (101,5101) titldum

      !!The user has the option of limiting the number of output
      !!variables printed to the output.rch, output.sub and 
      !!output.hru files. Lines 60-67 of file.cio are used to 
      !!identify the variables the user wants to print


      !!Output variables printed in REACH (output.rch) file

      read (101,5101) titldum
      read (101,*) (ipdvar(ii),ii=1,20)

      !!IPDVAR  - Output variables to output.rch file
             !![   1] Streamflow into reach (cms)
             !![   2] Streamflow out of reach (cms)
             !![   3] Loss of water from reach via evaporation (cms)
             !![   4] Loss of water from reach via transmission (cms)
             !![   5] Sediment entering reach (tons)
             !![   6] Sediment transported out of reach (tons)
             !![   7] Sediment concentration in reach (mg/kg)
             !![   8] Organic N entering reach (kg N)
             !![   9] Organic N transported out of reach (kg N)
             !![  10] Organic P entering reach (kg P)
             !![  11] Organic P transported out of reach (kg P)
             !![  12] NO3 entering reach (kg N)
             !![  13] NO3 transported out of reach (kg N)
             !![  14] NH4 entering reach (kg N)
             !![  15] NH4 transported out of reach (kg N)
             !![  16] NO2 entering reach (kg N)
             !![  17] NO2 transported out of reach (kg N)
             !![  18] Soluble P entering reach (kg P)
             !![  19] Soluble P transported out of reach (kg P)
             !![  20] Chlorophyll-a  entering reach (kg)
             !![  21] Chlorophyll-a transported out of reach (kg)
             !![  22] CBOD entering reach (kg O2)
             !![  23] CBOD transported out of reach (kg O2)
             !![  24] Dissolved O2 entering reach (kg O2)
             !![  25] Dissolved O2 transported out of reach (kg O2)
             !![  26] Soluble pesticide entering reach (mg ai)
             !![  27] Soluble pesticide transported out of reach (mg ai)
             !![  28] Sorbed pesticide entering reach (mg ai)
             !![  29] Sorbed pesticide transported out of reach (mg ai)
             !![  30] Loss of pesticide from water by reaction (mg ai)
             !![  31] Loss of pesticide from water by volatilization (mg ai)
             !![  32] Transfer of pesticide from water to bed sediment by 
             !!       settling (mg ai)
             !![  33] Transfer of pesticide from bed sediment to water by
             !!       resuspension (mg ai)
             !![  34] Transfer of pesticide between water and bed sediment by
             !!       diffusion (mg ai)
             !![  35] Loss of pesticide from bed sediment by reaction (mg ai)
             !![  36] Loss of pesticide from bed sediment by burial (mg ai)
             !![  37] Amount of pesticide in bed sediment (mg ai)
             !![  38] Persistent bacteria transported out of reach (#)
             !![  39] Less persistent bacteria transported out of reach (#)
             !![  40] Conservative metal #1 transported out of reach (kg)
             !![  41] Conservative metal #2 transported out of reach (kg)
             !![  42] Conservative metal #3 transported out of reach (kg)
             !![  43] Total N (org N + no3 + no2 + nh4 outs) to output.rch gsm 10/17/2011
             !![  44] Total P (org P + sol p outs)to output.rch gsm 10/17/2011
             !![  45] NO3 concentration output.rch (daily only) gsm 10/30/2011
             

      !!Output variables printed in SUBASIN (output.sub) file

      read (101,5101) titldum
      read (101,*) (ipdvab(ii),ii=1,15)

      !!IPDVAB  - Output variables to output.sub file
             !![   1] Total precipitation falling on subbasin (mm)
             !![   2] Snow melt in subbasin (mm)
             !![   3] Potential evapotranspiration (mm)
             !![   4] Evapotranspiration (mm)
             !![   5] Soil water content (mm)
             !![   6] Water percolating out of soil profile (mm)
             !![   7] Amount of water entering reach from surface runoff (mm)
             !![   8] Groundwater discharge into reach from subbasin (mm)
             !![   9] Net water contribution to reach from subbasin (mm)
             !![  10] Amount of sediment entering reach from subbasin (t/ha)
             !![  11] Organic N released to reach from subbasin (kg N/ha)
             !![  12] Organic P released to reach from subbasin (kg P/ha)
             !![  13] NO3 released to reach from subbasin (kg N/ha)
             !![  14] Soluble P released to reach from subbasin (kg P/ha)


      !!Output variables printed in HRU (output.hru) file

      read (101,5101) titldum
      read (101,*) (ipdvas(ii),ii=1,20)

      !!IPDVAS  - Output variables to output.hru file
             !![   1] Total precipitation falling on HRU (mm)
             !![   2] Precipitation falling as snow, ice, or freezing rain(mm)
             !![   3] Amount of snow or ice melt(mm)
             !![   4] Amt of irrigation water applied to HRU (mm)
             !![   5] Potential evapotranspiration (mm)
             !![   6] Loss of water by evapotranspiration (mm)
             !![   7] Soil water content at beginning of day/ave soil water (mm)
             !![   8] Soil water content at end of time step(mm)
             !![   9] Amt of water percolating past soil zone (mm)
             !![  10] Amt of water entering aquifers by percolation (mm)
             !![  11] Amt of water entering deep aquifer (mm)
             !![  12] Amt of water moving from shallow aquifer to soil zone(mm)
             !![  13] Amt of water removed from shallow aquifer to irrigate(mm)
             !![  14] Amt of water removed from deep aquifer to irrigate (mm)
             !![  15] Amt of water in shallow groundwater storage (mm)
             !![  16] Amt of water in deep groundwater storage (mm)
             !![  17] Surface runoff generated in time step (mm)
             !![  18] Surface runoff contribution to reach (mm)
             !![  19] Loss of water by transmission from stream channels 
             !!       within HRU-enters shallow aquifer (mm)
             !![  20] Lateral flow contribution to reach (mm)
             !![  21] Groundwater contribution to reach (mm)
             !![  22] Net amt of water contributed by HRU to reach (mm)
             !![  23] Curve number
             !![  24] Average air temperature (deg C)
             !![  25] Average of daily max air temperatures (deg C)
             !![  26] Average of daily min air temperatures (deg C)
             !![  27] Average soil temperature for time period (deg C)
             !![  28] Average daily solar radiation (MJ/m^2)
             !![  29] Amount of sediment entering reach from HRU (t/ha)
             !![  30] Sediment yield calculated with USLE (t/ha)
             !![  31] Amt of N fertilizer applied (kg N/ha)
             !![  32] Amt of P fertilizer applied (kg P/ha)
             !![  33] Amt of N fertilizer auto-applied (kg N/ha)
             !![  34] Amt of P fertilizer auto-applied (kg P/ha)
             !![  35] Amt of N applied in grazing operation (kg N/ha)
             !![  36] Amt of P applied in grazing operation (kg N/ha)
             !![  37] Amt of N applied in continuous fert operation (kg N/ha)
             !![  38] Amt of P applied in continuous fert operation (kg P/ha)
             !![  39] Amt of N added to soil in rainwater (kg N/ha)
             !![  40] Amt of N added to soil via fixation by legumes (kg N/ha)
             !![  41] Transformation of N from fresh organic to mineral pool
             !!       (kg N/ha)
             !![  42] Transformation of N from active organic to mineral pool 
             !!       (kg N/ha)
             !![  43] Transformation of N from active organic to stable organic 
             !!       pool (kg N/ha)
             !![  44] Transformation of P from fresh organic to mineral pool 
             !!       (kg P/ha)
             !![  45] Transformation of P from organic to labile pool (kg P/ha)
             !![  46] Transformation of P from labile to active mineral pool 
             !!       (kg P/ha)
             !![  47] Transformation of P from active mineral to stable mineral 
             !!       pool (kg P/ha)
             !![  48] Amt of N removed from soil via denitrification (kg N/ha)
             !![  49] Nitrogen uptake by plants (kg N/ha)
             !![  50] Phosphorus uptake by plants (kg P/ha)
             !![  51] Organic N contributed by HRU to reach (kg N/ha)
             !![  52] Organic P contributed by HRU to reach (kg P/ha)
             !![  53] Mineral P attached to sediment in surface runoff to 
             !!       reach (kg P/ha)
             !![  54] NO3 contributed by HRU in surface runoff to reach(kgN/ha)
             !![  55] NO3 contributed by HRU in lateral flow to reach (kgN/ha)
             !![  56] NO3 leached below soil profile (kg N/ha)
             !![  57] NO3 contributed by HRU in baseflow to reach(kgN/ha)
             !![  58] Soluble P contributed by HRU in surface runoff to 
             !!       reach (kg P/ha)
             !![  59] Soluble P contributed by HRU in baseflow to reach(kgP/ha)
             !![  60] Number of water stress days
             !![  61] Number of temperature stress days
             !![  62] Number of nitrogen stress days
             !![  63] Number of phosphorus stress days
             !![  64] Total plant biomass (t/ha)
             !![  65] Leaf area index
             !![  66] Harvested yield (t/ha)
             !![  67] Persistent bacteria in surface runoff (count)
             !![  68] Less persistent bacteria in surface runoff (count)

      !!HRUs printed in HRU (output.hru,output.wtr) files

      read (101,5101) titldum
      read (101,*) (ipdhru(ii),ii=1,20)

      !! Atmospheric deposition file (Kannan/Santhi input file)
      do
       read (101,5101,iostat=eof) titldum
       if (eof < 0) exit
       read (101,5000,iostat=eof) atmofile
       if (eof < 0) exit
       exit
      end do

!!   mauro code for printing hourly output file hard wired (hourq.out)
!!   IPHR = 0 no print
!!   IPHR = 1 print file
      iphr = 0
      read (101,*,iostat=eof) iphr
!!   code for printing soil storage values by soil layer (output.swr)
!!   ISTO = 0 no print
!!   ISTO = 1 print file
      isto = 0
      read (101,*,iostat=eof) isto

!!   code for printing output.sol file (formerly 'output.sol' - now output.snu)
!!   isol = 0 no print
!!   isol = 1 print file
      isol = 0
      read (101,*,iostat=eof) isol  
      if (isol == 1) then
         open (121,file='output.snu')
         write (121,12222) 
12222   format (t25,'SURFACE',t39,'-------  SOIL PROFILE  -------',/, 
     &  t8,'DAY',t15,'GISnum',t25,'SOL_RSD',t37,'SOL_P',t48,            
     &  'NO3',t57,'ORG_N',t67,'ORG_P',t80,'CN'/,t26,                    
     &  '(t/ha)',t35,'(kg/ha)',t45,                                     
     &  '(kg/ha)',t55,'(kg/ha)',t66,'(kg/ha)')
      end if  
!! headwater code (0=do not route; 1=route)
      i_subhw = 0
      read (101,*,iostat=eof) i_subhw 

!! SEPTIC CHANGES GSM 01/29/09
!!   gsm had to take do off when added ia_b ??? 3/25/09 for binary files
!!      do
      read (101,5000,iostat=eof) septdb
!!      if (eof < 0) exit
      call caps(septdb)
!!	end do

!!    read from readlup (landuse update file)
       open (122,file='lup.dat')
                      
!!    added for binary files 3/25/09 gsm 
!!    ia_b  print ascii or binary files
!!       0 for ascii file 
!!       1 for binary file   
      ia_b = 0  
      read (101, *, iostat=eof) ia_b

!!    read code to turn on output.wqr output file
!!      ihumus = 0 (do not print file)
!!      ihumus = 1 (print formerly watqual.out - now output.wql)
      read(101,*,iostat=eof) ihumus


!!   flag for output files named tempvel.out and tempdep.out
!!   this flag will print both files 
!!   default is = 0; no print
      read (101,*,iostat=eof) itemp


!!    output by elevation band to (formerly 'snowband.out')
      read (101,*,iostat=eof) isnow
	if (isnow == 1) then
         open (115,file='output.snw')
         write (115,1010)
!!    output temperatures by elevation band (Dhiraj Raj - 07/22/2015)
         open (116,file='ebandtemp.out')
         write (116,1011)
      end if

      !!Set default output variables for REACH, SUBBASIN and HRU files if none
      !!were specified

      do ii = 1, 20
        if (ipdvar(ii) > 0) itotr = itotr + 1
      end do

      if (ipdvar(1) <= 0) then
 !! change 42 to 45 for output.rch file gsm 10/30/2011  
 !! change 46 to 56 for output.rch file salt - srini  
        do ii = 1, 58     !! salty dog
          ipdvar(ii) = ii
        end do
        itotr = 58        !! salty dog
      end if


      do ii = 1, 15
        if (ipdvab(ii) > 0) itotb = itotb + 1
      end do

      if (ipdvab(1) <= 0) then
        do ii = 1, msubo
          ipdvab(ii) = ii
        end do
        itotb = msubo
      end if


      do ii = 1, 20
        if (ipdvas(ii) > 0) itots = itots + 1
      end do

      if (ipdvas(1) <= 0) then
        do ii = 1, mhruo
          ipdvas(ii) = ii
        end do
        itots = mhruo
      end if


      do ii = 1, 20
        if (ipdhru(ii) > 0) itoth = itoth + 1
      end do
	
      if (ipdhru(1) <= 0) then
        do ii = 1, mhru
!       do ii = 1, mhruo
          ipdhru(ii) = ii
        end do
        itoth = mhru
!       itoth = mhruo
      end if

      !!Open output files
      open (24,file="input.std")
      open (26,file="output.std")

      open (28,file="output.hru",recl=1500)
      if (ia_b == 1) then 
        open (33333,file="outputb.hru",form='unformatted')
      end if
      open (30,file="output.pes",recl=600)
      open (31,file="output.sub",recl=600)
      if (ia_b == 1) then
        open (66666,file = "outputb.sub", form = 'unformatted')
      end if
      open (7,file="output.rch",recl=1500)
      open (8,file="output.rsv",recl=800)
      if (ia_b == 1) then
        open (77777,file = "outputb.rch", form = 'unformatted')
      end if
      
!!    sediment routing output file
      open (84,file="output.sed",recl=800)
      
!! write headings to output file (output.sed)
!      write (84,7000) prog, values(2), values(3), values(1), values(5), 
!     &               values(6), values(7)
!      write (84,7010) title
     
!! write headings to sediment outputfile (output.sed)
      write (84,1080)
1080  format (t10,'RCH',t17,'GIS',t23,'MON',t31,'AREAkm2',               
     &t40,'SED_INtons',t51,'SED_OUTtons',t63,'SAND_INtons',t74,         
     &'SAND_OUTtons',t87,'SILT_INtons',t98,'SILT_OUTtons',t111,         
     &'CLAY_INtons',t122,'CLAY_OUTtons',t135,'SMAG_INtons',t146,        
     &'SMAG_OUTtons',t160,'LAG_INtons',t171,'LAG_OUTtons',t184,         
     &'GRA_INtons',t195,'GRA_OUTtons',t208,'CH_BNKtons',t220,           
     &'CH_BEDtons',t232,'CH_DEPtons',t244,'FP_DEPtons',t259,'TSSmg/L')
     
      ! Jaehak, sedimentation-filtration output
      open (77778,file = "bmp-sedfil.out") !jaehak temp urban print out
      write(77778,'(a46)') 'Sed-Fil Basins Configuration'   
      write(77778,'(a46)') ''   ! 
       !retention-irrigation output
      open (77779,file = "bmp-ri.out") !jaehak temp urban print out
      write(77779,'(a46)') 'Retention-Irrigation Basins Configuration'
      write(77779,'(a46)') ''   ! 


!! srin output file from watqual.f  
      if (ihumus ==1) then
        open (82,file='output.wql')
        write (82,6000)
 6000   format (18x,'WTEMP(C)',' ALGAE_INppm','  ALGAE_Oppm',
     *  '  ORGN_INppm',' ORGN_OUTppm','   NH4_INppm','  NH4_OUTppm',
     *  '   NO2_INppm','  NO2_OUTppm','   NO3_INppm','  NO3_OUTppm',
     *  '  ORGP_INppm',' ORGP_OUTppm','  SOLP_INppm',' SOLP_OUTppm',
     *  '  CBOD_INppm',' CBOD_OUTppm','   SAT_OXppm',' DISOX_INppm',
     *  '  DISOX_Oppm',' H20VOLUMEm3',' TRVL_TIMEhr')
      end if

!! mauro/jerry whittaker hourly output file
      if (iphr > 0) then
        open (83,file='hourq.out')
        write (83,6001) 
6001    format (t29,'TOTAL',/,t27,'WATER YLD',/,
     *  t3,'YEAR',t10,'DAY',T15,'HOUR',t22,'HYD',t29,'(m**3)')
      endif
!! end hourly codes

!!darrell output files added for interface plotting
      open (11,file='rch.dat')
      open (12,file='hru.dat')
      open (13,file='sub.dat')
      open (14,file='rsv.dat')
!!darrell output files added for interface plotting
      open (11123,file='hyd.out')
      open (16,file='chan.deg')
!!    open (17,file='wbl.out')
      open (18,file='swat.qst')
!! output amount of water stored in the soil layer (formerly 'soilst.out')
      if (isto > 0) then
        open (129,file='output.swr')
        write (129,5001) 
5001    format (t20,'Soil Storage (mm)',/,t25,'Layer #',/,t3,'Day',t9,
     *  'HRU',t19,'GIS',t34,'1',t46,'2',t58,'3',t70,'4',t82,'5',t93,'6',
     *  t106,'7',t118,'8',t130,'9',t141,'10')
      end if


!! Output daily streamflow velocity for each channel (subbasin)
      if (itemp == 1) then
         open (141,file='output.vel')
         write (141,4999)
 4999     format(t17,'CH_VEL',/,t3,'Day',t7,'Year',t18,'(m/s)')
         open (142,file='output.dep')
          write (142,4998)
 4998    format(t17,'AVE WATER',/,t3,'Day',t7,'Year',t18,'DEPTH(m)')
      end if
      
!!!!!open Srin/Arun new output file from subbasin.f (monsoon_plt.out)      
      open (144, file="monsoon_plt.out", recl=600)
         write (144, 1999)
1999     format(2x,'Sub',2x,'Hru',2x,'Year',3x,'Mon',3x,'Day',
     *'   AREAkm2', 13x,'CPNM', 4x, "      PLANT", 
     *'   PRECIPD ', '  PET_DAY ', '      RTO', ' PRECIPSUM',
     *'   PET_SUM ',
     *'   SOL_SW',/,70x,'     (mm)  ','    (mm)  ',10x,'  (mm)    ',
     *'    (mm) ','     (mm)  ')
 !!!!!open Srin/Arun new output file from subbasin.f

!! Code for output.mgt file
!  0=no print 1=print
      read (101, *,iostat=eof) imgt
	if (imgt==1) then
         open (143, file="output.mgt", recl=600)
         write (143,999)
999      format(2x,'Sub',2x,'Hru',2x,'Year',3x,'Mon',3x,'Day',
     *'   AREAkm2', 3x,'crop/fert/pest', 4x,
     *'Operation',4x,'phubase',3x,'phuacc',4x,'sol_sw',4x,'bio_ms',3x,
     *'sol_rsd',7x,'sol',7x,'sol',5x,'yield',3x,'irr amt',
     *5x,'amt',5x,'mix eff',
     *5x,'strsn',
     *5x,'strsp',3x,'strstmp',5x,'strsw',5x,'strsa',2x,'irrsc',
     *2x,'irrno',5x,'grain',3x,'biomass',5x,'tuber',3x,'residue',7x,
     *'nit',6x,'phos',/,111x,
     *' sumno3',2x,' sumsolp',23x,'frt-kg',17x,' sum',6x,' sum',6x,
     *' sum',6x,' sum',6x,' sum',20x,'yld',6x,'yld',8x,'yld',6x,'yld',
     *9x,'yld',6x,'yld'/,85x,'mm', 6x,'kg/ha',5x,'kg/ha',5x,
     *'kg/ha', 5x,'kg/ha',5x, 'kg/ha',5x, 'mm',4x,'or dwfert',3x,
     *'frac',6x,'fertno3',7x,'nh3',6x,'orgn',6x,'solp',6x,'orgp',19x,
     *'kg/ha',4x,'kg/ha',6x,'kg/ha',4x,'kg/ha',7x,'kg/ha',5x,'kg/ha',
     */,'_______________________________________________________________
     *__________________________________________________________________
     *__________________________________________________________________
     *__________________________________________________________________
     *________________________________',/)
	end if     
      
!! Code for output.wtr and output.pot files
! 0 =no print  1 =print
      read (101,*,iostat=eof) iwtr
        if (iwtr == 1) then
          open (29,file="output.wtr",recl=800)
! write statement added for Aziz (06/25/09)
          open (125,file='output.pot')
          write (125, 1000) 
        end if
        
 1000  format (1x,'SUB',t6,'HRU',t12,'DAY',t17,'YEAR',t26,'VOL-I',t37,  
     &'SA-I',t46,'SPILLO', 
     &t56,'POTSEP',t66,'POTEV',t75,'SOL_SW',t85,'TILE-O',t96,'VOL-F',   
     &t106,'SA-F',/,t27,'(mm)',t37,'(ha)',t47,'(mm)',t57,'(mm)',t67,    
     &'(mm)',t77,'(mm)',t87,'(mm)',t97,'(mm)',t107,'(ha)')  
       
!     code for writing out calendar day or julian day to output.rch, .sub, .hru files
!     icalen = 0 (print julian day) 1 (print month/day/year)
!     icalen MUST be == zero  if IPRINT == 3 to print subdaily;
      read (101,*, iostat=eof) icalen
      if (icalen == 1) iprint = 1
      
      if (isproj == 1) then
        open (19,file="output2.std")
        open (20,file="output2.rch",recl=600)
        open (21,file="output2.hru",recl=800)
        open (22,file="output2.rsv",recl=800)
      end if

!! septic result  J.Jeong Feb2009
      open (173,file='septic.out')  
	write(173,5102) 'HRU','YEAR','DAY','Precip', 'PERC',        
     & 'sol_ul','sol_st','sol_fc','nh3bgn','nh3end',   
     & 'no3bgn','no3end', 'nitrN','denitrN',
     & 'solpbgn','solpend','solpconc'
      write(173,5102) '#','','','(mm)','(mm)','(mm)',    
     & '(mm)','(mm)','(kg/ha)','(kg/ha)',
     & '(kg/ha)','(kg/ha)','(kg/ha)','(kg/ha)',
     & '(kg/ha)','(kg/ha)','(mg/l)'

!!   charles ikenberry output file
!      open (2222,file='charles.out',recl=800)
!      write (2222,2222) 
!2222  format (3x,'yr',2x,'day',5x,'res_vol',7x,'res_no3',7x,'ressa',
!     & 6x,'conc_n',4x,'con_nirr',6x,'nsetlr',5x,'theta_n',7x,'tmpav',
!     & 6x,'nitrok',/,15x,'m3',12x,'kg',12x,'ha',9x,'kg/m3',5x,'kg/m3',
!     & 9x,'--',9x,'--',12x,'deg c',6x,'--')
      close (101)
      return

 1010 format (32x,'SNOW(mm) at ELEVATION BAND (1-10)',/,                
     &1x,'DAY','   YR',t14,'GISnum',t28,'1',t36,'2',t44,'3',t52,'4',    
     &t60,'5',t68,'6',t76,'7',t84,'8',t92,'9',t99,'10')
 1011 format (32x,'Average TEMP(deg C) at ELEVATION BAND (1-10)',/,
     &1x,'DAY','   YR',t14,'GISnum',t28,'1',t36,'2',t44,'3',t52,'4',    
     &t60,'5',t68,'6',t76,'7',t84,'8',t92,'9',t99,'10')
 5000 format (6a)
 5100 format (20a4)
 5101 format (a80)
 5102 format (3a5,30a15)
 
 !7000 format ('1',/t5,a80,t105,2(i2,'/'),i4,5x,2(i2,':'),i2)
 !7010 format (/(t5,20a4))
      end