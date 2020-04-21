      subroutine getallo2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   This subroutine calculates the number of HRUs, subbasins, etc. in the 
!!   simulation. These values are used to allocate array sizes.

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mapp        |none        |maximum number of applications
!!    mch         |none        |maximum number of channels
!!    mcr         |none        |maximum number of crops grown per year
!!    mcrdb       |none        |max number of lu/lc defined in crop.dat
!!    mcut        |none        |maximum number of cuttings per year
!!    mfdb        |none        |max number of fertilizers in fert.dat
!!    mgr         |none        |maximum number of grazings per year
!!    mhru        |none        |maximum number of HRUs in watershed
!!    mhyd        |none        |maximum number of hydrograph nodes
!!    mlyr        |none        |maximum number of soil layers 
!!    mnr         |none        |max number of years of rotation
!!    mpst        |none        |max number of pesticides used in wshed
!!    mpdb        |none        |max number of pesticides in pest.dat
!!    mrecc       |none        |maximum number of reccnst files
!!    mrecd       |none        |maximum number of recday files
!!    mrech       |none        |maximum number of rechour files
!!    mrecm       |none        |maximum number of recmon files
!!    mrecy       |none        |maximum number of recyear files
!!    mres        |none        |maximum number of reservoirs
!!    mrg         |none        |max number of rainfall/temp gages
!!    nstep       |none        |max number of time steps per day
!!    msub        |none        |maximum number of subbasins
!!    mtil        |none        |max number of tillage types in till.dat
!!    mudb        |none        |maximum number of urban land types in urban.dat
!!    myr         |none        |max number of years of simulation
!!    pstflg(:)   |none        |flag for types of pesticide used in watershed
!!                             |array location is pesticide ID number
!!                             |0: pesticide not used
!!                             |1: pesticide used
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    a           |NA          |comment flag
!!    plantdb     |NA          |name of LU/LC database input file (crop.dat)
!!    eof         |none        |end of file flag
!!    fertdb      |NA          |name of fertilizer database file (fert.dat)
!!    figfile     |NA          |name of watershed configuration file (.fig)
!!    i           |none        |counter
!!    ic          |none        |number of land cover in crop database
!!    icd         |none        |routing command code (.fig)
!!    ifnum       |none        |number of fertilizer type in database file
!!    iht         |none        |hydrograph storage location number (.fig)
!!    inm1        |none        |1st routing command variable (.fig)
!!    inm2        |none        |2nd routing command variable (.fig)
!!    inm3        |none        |3rd routing command variable (.fig)
!!                             |if icd=1, inm3=subbasin #
!!    ipnum       |none        |number of pesticide type in database file
!!    itnum       |none        |number of tillage implement in database file
!!    iunum       |none        |number of urban land type in database file
!!    j           |none        |counter
!!    nhtot       |none        |number of relative humidity records in file
!!    nrgage      |none        |number of raingage files
!!    nrgfil      |none        |number of rain gages per file
!!    nrtot       |none        |total number of rain gages
!!    nsave       |none        |number of save commands in .fig file
!!    nstot       |none        |number of solar radiation records in file
!!    ntgage      |none        |number of temperature gage files
!!    ntgfil      |none        |number of temperature gages per file
!!    nttot       |none        |total number of temperature gages
!!    numhru      |none        |number of HRUs listed in subbasin file
!!    nwtot       |none        |number of wind speed records in file
!!    pestdb      |NA          |name of pesticide database input file(pest.dat)
!!    subfile     |NA          |name of subbasin input file (.sub)
!!    tilldb      |NA          |name of tillage database input file(till.dat)
!!    title       |NA          |description lines in file.cio(1st 3 lines)
!!    titldum     |NA          |variable to read in data line
!!    urbandb     |NA          |name of urban land type database file 
!!                             |(urban.dat)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: caps

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm


      character (len=13) :: urbandb, plantdb, tilldb, pestdb, figfile,  &
     &                      fertdb, subfile, bsnfile
      character (len=1) ::  a
      character (len=80) ::  titldum
      integer :: icd, inm1, inm2, inm3, iht, eof, numhru, ic
      integer :: ipnum, ifnum, iunum, itnum, j

!!    initialize variables
      title = ""
      plantdb = ""
      tilldb = ""
      pestdb = ""
      fertdb = ""
      urbandb = ""
      figfile = ""
      bsnfile = ""
      nrgage = 0
      ntgage = 0
      nrtot = 0
      nttot = 0
      nrgfil = 0
      ntgfil = 0
      nstot = 0
      nhtot = 0
      nwtot = 0
      nstep = 0
      myr = 0

      open (23,file="file.cio")
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,5100) title
      read (23,6000) titldum
      read (23,5000) figfile
      read (23,*) myr
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,*) nstep
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,*) nrgage
      read (23,*) nrtot
      read (23,*) nrgfil
      read (23,6000) titldum
      read (23,*) ntgage
      read (23,*) nttot
      read (23,*) ntgfil
      read (23,6000) titldum
      read (23,*) nstot
      read (23,6000) titldum
      read (23,*) nhtot
      read (23,6000) titldum
      read (23,*) nwtot
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,6000) titldum
      read (23,5000) bsnfile
      read (23,6000) titldum
      read (23,5000) plantdb
      read (23,5000) tilldb
      read (23,5000) pestdb
      read (23,5000) fertdb
      read (23,5000) urbandb

!!    read title lines from septic database file
      do nlines = 1, 24
        read (23,6000,iostat=eof) titldum
	  if (eof < 0) exit
      end do

      read (23,5000,iostat=eof) septdb

!! calculate max number of years simulated, daily time increment
      myr = myr + 2
      if (nstep <= 0) then
        nstep = 1
      else
        nstep = 1440 / nstep
      end if
      nstep = nstep + 1
      
      call caps(plantdb)
      call caps(fertdb)
      call caps(pestdb)
      call caps(figfile)
      call caps(tilldb)
      call caps(urbandb)

!! open .bsn file to get ievent input
      open (103,file=bsnfile)
      call caps(bsnfile)
      do nlines = 1, 17
        read (103,6000,iostat=eof) titldum
        if (eof < 0) exit
      end do
        read (103,*,iostat=eof) ievent
      if (ievent == 1) nstep = 24


!!    open routing file
      open (27,file=figfile)

!!    opens database files
      open (29,file=plantdb)
      open (30,file=tilldb)
      open (31,file=pestdb)
      open (7,file=fertdb)
      open (8,file=urbandb)

!!    initialize variables
      a = ""
      icd = 1
      iht = 0
      inm1 = 0
      inm2 = 0
      inm3 = 0
      mhru = 0
      mch = 1
      msub = 1
      mhyd = 1
      mres = 0 
      mlyr = 0
      mpst = 0
      mcr = 0
      mapp = 0
      mgr = 0
      mcut = 0
      mnr = 0
      mapex = 0
      mrecc = 0
      mrecd = 0
      mrech = 0
      mrecm = 0
      mrecy = 0
      mtran = 0
      nsave = 0
 !!     nauto = 0


!! calculate number of records in plant growth database
      eof = 0
      mcrdb = 0
      do
        ic = 0
        read (29,*,iostat=eof) ic
        if (eof < 0) exit
        read (29,6000,iostat=eof) titldum
        if (eof < 0) exit
        read (29,6000,iostat=eof) titldum
        if (eof < 0) exit
        read (29,6000,iostat=eof) titldum
        if (eof < 0) exit
        read (29,6000,iostat=eof) titldum
        if (eof < 0) exit
        mcrdb = Max(mcrdb,ic)
      end do
      if (mcrdb <= 0) mcrdb = 1

!! calculate number of records in urban database
      eof = 0
      mudb = 0
      do
        iunum = 0
        read (8,6200,iostat=eof) iunum
        if (eof < 0) exit
        read (8,6000,iostat=eof) titldum
        if (eof < 0) exit
        mudb = Max(mudb,iunum)
      end do
      if (mudb <= 0) mudb = 1

!! calculate number of records in fertilizer database
      eof = 0
      mfdb = 0
      do
        ifnum = 0
        read (7,6300,iostat=eof) ifnum
        if (eof < 0) exit
        mfdb = Max(mfdb,ifnum)
      end do
      if (mfdb <= 0) mfdb = 1

!! calculate number of records in pesticide database
      eof = 0
      mpdb = 0
      do
        ipnum = 0
        read (31,6200,iostat=eof) ipnum
        if (eof < 0) exit
        mpdb = Max(mpdb,ipnum)
      end do
      if (mpdb <= 0) mpdb = 1

!! calculate number of records in tillage database
      eof = 0
      mtil = 0
      do
        itnum = 0
        read (30,6300,iostat=eof) itnum
        if (eof < 0) exit
        mtil = Max(mtil,itnum)
      end do
      if (mtil <= 0) mtil = 1


!! process .fig file
      pstflg = 0
      do while (icd > 0)
        read (27,5002) a
        if (a /= "*") then
          backspace 27

          read (27,5001) a, icd, iht, inm1, inm2, inm3

          select case (icd)
          case (1)                      !! icd = 1  SUBBASIN command
            msub = msub + 1             !! # subbasins
            !! calculate total number of HRUs in watershed
            subfile = ""
            numhru = 0
            read (27,6100) subfile
            call caps(subfile)
            open (25,file=subfile)
            do j = 1,52 
              read (25,6000) titldum
            end do
            read (25,*) numhru
            mhru = mhru + numhru
            call hruallo(numhru)
!! routing add 5/24/2010
	      read (25,6000,iostat=eof) titldum
!!	      if (eof >= 0) then
		  if (eof > 0) then	       
              read (25,5002,iostat=eof) irt
	        if (irt == 1) then
		      call ruallo 
	        end if
		  end if 
		  close (25)
          case (2)                      !! icd = 2  ROUTE command
            mch = mch + 1               !! # channels
            read (27,5002) a
          case (3)                      !! icd = 3  ROUTE RESERVOIR command
            mres = mres + 1
            read (27,5002) a
          case (4)                      !! icd = 4  TRANSFER command
            mtran = mtran + 1
          case (6)                      !! icd = 6  RECALL HOUR command
            read (27,5002) a
            mrech = mrech + 1
          case (7)                      !! icd = 7  RECALL MONTH command
            read (27,5002) a
            mrecm = mrecm + 1
          case (8)                      !! icd = 8  RECALL YEAR command
            read (27,5002) a
            mrecy = mrecy + 1
          case (9)                      !! icd = 9  SAVE command
            read (27,5002) a
            nsave = nsave + 1
          case (10)                     !! icd = 10 RECALL DAY command
            read (27,5002) a
            mrecd = mrecd + 1
          case (11)                     !! icd = 11 RECALL CONSTANT command
            read (27,5002) a
            mrecc = mrecc + 1
          case (13)                     !! icd = 13 APEX command           
            read (27,5002) a
            mapex = mapex + 1
          case (14)                     !! icd = 14 SAVECONC command
            read (27,5002) a
            nsave = nsave + 1
          end select

          mhyd = Max(mhyd,iht)

        end if
      end do
      if (mhru <= 0) mhru = 1
      if (msub <= 0) msub = 1
      if (mch <= 0) mch = 1
	  mch = Max(mch,msub+1)
      if (mrecc <= 0) mrecc = 1
      if (mrecd <= 0) mrecd = 1
      if (mrech <= 0) mrech = 1
      if (mrecm <= 0) mrecm = 1
      if (mrecy <= 0) mrecy = 1
      if (mres <= 0) mres = 1

 !!     mhyd = mhyd + nsave + nauto + mtran + 1
        mhyd = mhyd + nsave + mtran + 1
!! septic change 1-28-09 gsm
      mlyr = mlyr + 4
!! septic change 1-28-09 gsm

      mcr = mcr + 1
      mcr = Max(2,mcr)
      mapp = mapp + 1
      mgr = mgr + 1
      mcut = mcut + 1
      mnr = mnr + 1
      mpst = Sum(pstflg) + 1

!! calculate max number of climate gages
      mrg = 0
      mrg = Max(nrtot,nttot,nstot,nhtot,nwtot)
      if (mrg <= 0) mrg = 1

      close (23)
      close (27)
      close (29)
      close (30)
      close (31)
      close (103)
      close (7)
      close (8)
      return
 5000 format (6a)
 5001 format (a1,9x,5i6)
 5002 format(a)
 5100 format (20a4)
!$$$$$$  5200 format (10i4)
 6000 format (a80)
 6100 format (10x,a13)
 6200 format (i3)
 6300 format (i4)
      end

