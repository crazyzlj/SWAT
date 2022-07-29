      subroutine openwth

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine opens the precipitation, temperature, solar radiation,
!!    relative humidity and wind speed files for simulations using measured 
!!    weather data

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    nrgage      |none          |number of raingage files
!!    nrgfil      |none          |number of rain gage per file
!!    nrtot       |none          |total number of rain gages
!!    ntgage      |none          |number of temperature gage files
!!    ntgfil      |none          |number of temperature gages per file
!!    nttot       |none          |total number of temperature gages
!!    petfile     |NA            |potential ET file name (.pet)
!!    rfile(:)    |NA            |rainfall file name (.pcp)
!!    rhfile      |NA            |relative humidity file name (.hmd)
!!    slrfile     |NA            |solar radiation file name (.slr)
!!    tfile(:)    |NA            |temperature file name (.tmp)
!!    wndfile     |NA            |wind speed file name (.wnd)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    elevp(:)    |m             |elevation of precipitation gage station
!!    elevt(:)    |m             |elevation of temperature gage station
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none           |counter
!!    k           |none           |counter
!!    kk1         |none           |gage code for first dataset in weather file
!!    kk2         |none           |gage code for last dataset in weather file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, kk1, kk2, k
      character (len=80) :: titldum

!! open precip files and read elevation
      do j = 1, nrgage
        !! calculate gage id codes for first and last dataset in file
        kk1 = 0
        kk2 = 0
        kk1 = nrgfil * (j - 1) + 1
        if (j == nrgage) then
          kk2 = nrtot
        else
    
         kk2 = kk1 + (nrgfil - 1)
        end if
        if (rfile(j) /= '             ') then
          open (100+j,file=rfile(j),recl=1850)     !!!SWAT2012 main code
          read (100+j,5000) titldum
          read (100+j,5000) titldum
          read (100+j,5000) titldum
          if (ievent == 0) then   !daily records
            read (100+j,5001) (elevp(k), k = kk1, kk2)
          else                   !subdaily records
            read (100+j,5003) (elevp(k), k = kk1, kk2)
          endif
        end if
      end do

      do j = 1, ntgage
        !! calculate gage id codes for first and last dataset in file
        kk1 = 0
        kk2 = 0
        kk1 = ntgfil * (j - 1) + 1
        if (j == ntgage) then
          kk2 = nttot
        else
          kk2 = kk1 + (ntgfil - 1)
        end if
        if (tfile(j) /= '             ') then
          open (118+j,file=tfile(j),recl=20000)     !!! SWAT2012 CODE
          read (118+j,5000) titldum
          read (118+j,5000) titldum
          read (118+j,5000) titldum
          read (118+j,5002) (elevt(k), k = kk1, kk2)
        end if
      end do

        if (slrfile /= '             ') then
   !!       open (137,file=slrfile,recl=800)
          open (137,file=slrfile,recl=16000)
          read (137,5000) titldum
        end if

        if (rhfile /= '             ') then
  !!        open (138,file=rhfile,recl=800)
          open (138,file=rhfile,recl=16000)
          read (138,5000) titldum
        end if

        if (wndfile /= '             ') then
   !!       open (139,file=wndfile,recl=800)
          open (139,file=wndfile,recl=16000)
          read (139,5000) titldum
        end if

        if (petfile /= '             ') then
          open (140,file=petfile)
          read (140,5000) titldum
        end if

      return
 5000 format (a80)
 5001 format (7x,1800i5)
 5002 format (7x,1800i10)   
 5003 format (13x,1800i5)
      end