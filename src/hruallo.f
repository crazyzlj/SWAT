      subroutine hruallo(hru)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!   This subroutine calculates the number of management operation types, etc.
!!   used in the simulation. These values are used to allocate array sizes for
!!   processes occurring in the HRU.

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    mapp        |none        |max number of applications
!!    mcr         |none        |max number of crops grown per year
!!    mcut        |none        |max number of cuttings per year
!!    mgr         |none        |max number of grazings per year
!!    mlyr        |none        |max number of soil layers
!!    mnr         |none        |max number of years of rotation
!!    pstflg(:)   |none        |flag for types of pesticide used in watershed
!!                             |array location is pesticide ID number
!!                             |0: pesticide not used
!!                             |1: pesticide used
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ap_af       |none        |number of autofertilizer operations in mgt file
!!    ap_ai       |none        |number of autoirrigation operations in mgt file
!!    ap_cc       |none        |number of continuous cuuting operations in mgt
!!    ap_cf       |none        |number of continuous fertilization operations in mgt
!!    ap_ci       |none        |number of continuous irrigation operations in mgt
!!    ap_f        |none        |number of fertilizer operations in mgt file
!!    ap_i        |none        |number of irrigation operations in mgt file
!!    ap_p        |none        |number of pesticide operations in mgt file
!!    ap_r        |none        |number of release/impound operations in mgt file
!!    ap_s        |none        |number of sweep operations in mgt file
!!    ap_t        |none        |number of tillage operations in mgt file
!!    chmfile     |NA          |HRU soil chemical data file name (.chm)
!!    cut         |none        |number of harvest only operations in mgt file
!!    depth(:)    |mm          |depth to bottom of soil layer
!!    eof         |none        |end of file flag (=-1 if eof, else =0)
!!    grz         |none        |number of grazing operations in mgt file
!!    hkll        |none        |number of harvest/kill operations in mgt file
!!    hru         |none        |number of HRUs in subbasin
!!    hrufile     |NA          |name of HRU general data file name (.hru)
!!    ii          |none        |counter
!!    j           |none        |counter
!!    k           |none        |counter
!!    kll         |none        |number of kill operations in mgt file
!!    lyrtot      |none        |total number of layers in profile
!!    mgt_op      |none        |manangement operation code
!!    mgt1i       |none        |sixth parameter in mgt file operation line
!!    mgtfile     |NA          |HRU management data file name (.mgt)
!!    plt         |none        |number of plant operations in mgt file
!!    pstnum      |none        |pesticide ID number from database file
!!    rot         |none        |number of years in rotation used in HRU
!!    solfile     |NA          |HRU soil data file name (.sol)
!!    titldum     |NA          |input lines in .sub that are not processed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max
!!    SWAT: caps

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      integer, intent (in) :: hru
      character (len=13) :: hrufile, mgtfile, solfile, chmfile
      character (len=80) ::  titldum
      integer :: eof, j, k, lyrtot, rot, plt, ap_f, ap_p, ap_t, ap_i
      integer :: grz, cut, mgt1i, pstnum, ii, ap_r, ap_s, kll, hkll
      integer :: ap_ai, ap_af, mgt_op, ap_cf, ap_cc, ap_ci, jj
      real :: depth(25)

!! skip subbasin input data
      jj = 1
      read (25,6000) titldum
      do j = 1, 3
      read (25,6000) titldum
      mgtfile = ""
      solfile = ""
      chmfile = ""
      read (25,5300) hrufile, mgtfile, solfile, chmfile
        if (hrufile /= '             ') then
        call caps(mgtfile)
        call caps(solfile)
        call caps(chmfile)
        open (9,file=solfile,recl=350)
        !! calculate # of soil layers in profile
          depth = 0.
          lyrtot = 0
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6100) (depth(k), k = 1, 25)
          do k = 1, 25
            if (depth(k) <= 0.001) lyrtot = k - 1
            if (depth(k) <= 0.001) exit
          end do
          mlyr = Max(mlyr,lyrtot)
        open (10,file=mgtfile)
        !! calculate maximum number of years in a rotation
          rot = 0
          do k = 1, 28
          read (10,6000) titldum
          end do
          read (10,*) rot
          mnr = Max(mnr,rot)
          read (10,6000) titldum
        !! calculate maximum number of crops grown in a year
          nopp = 0
          
          do k = 1, rot
            do
            mgt_op = 0
            mgt1i = 0
            read (10,6300) mgt_op, mgt1i
	      if (mgt_op == 4 .and. mgt1i > 0) pstflg(mgt1i) = 1
            if (mgt_op == 0) exit
            
            nopp = nopp + 1
            
            
            end do
            mapp = Max(mapp,nopp)
            
          end do
        open (11,file=chmfile)
          eof = 0
          do 
            do k = 1, 11
              read (11,6000,iostat=eof) titldum
              if (eof < 0) exit
            end do
            if (eof < 0) exit
            do
              pstnum = 0
              read (11,*,iostat=eof) pstnum
              if (eof < 0) exit
              if (pstnum > 0) pstflg(pstnum) = 1
            end do
            if (eof < 0) exit
          end do
        close (11)
        close (10)
        close (9)
        jj = jj + 1
        end if
      end do

      read (25,6000) titldum
      do j = jj, hru
        mgtfile = ""
        solfile = ""
        chmfile = ""
        read (25,5300) hrufile, mgtfile, solfile, chmfile
        call caps(mgtfile)
        call caps(solfile)
        call caps(chmfile)
        open (9,file=solfile,recl=350)
        !! calculate # of soil layers in profile
          depth = 0.
          lyrtot = 0
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6000) titldum
          read (9,6100) (depth(k), k = 1, 25)
          do k = 1, 25
            if (depth(k) <= 0.001) lyrtot = k - 1
            if (depth(k) <= 0.001) exit
          end do
          mlyr = Max(mlyr,lyrtot)
        open (10,file=mgtfile)
        !! calculate maximum number of years in a rotation
          rot = 0
          do k = 1, 28
            read (10,6000) titldum
          end do
          read (10,*) rot
          mnr = Max(mnr,rot)
        !! calculate maximum number of crops grown in a year
          read (10,6000) titldum
          nopp = 0
          mcri = 0
          
!!          do k = 1, rot
            do
            mgt_op = 0
            mgt1i = 0
            read (10,6300,iostat=eof) mgt_op, mgt1i
            if (eof < 0) exit
	      if (mgt_op == 4 .and. mgt1i > 0) pstflg(mgt1i) = 1
            if (mgt_op == 1) then
              mcri = mcri + 1
            end if
            nopp = nopp + 1
            
            end do
            
            mcr = Max(mcr,mcri)
            mapp = Max(mapp,nopp)
            
!!          end do
        open (11,file=chmfile)
          eof = 0
          do 
            do k = 1, 11
              read (11,6000,iostat=eof) titldum
              if (eof < 0) exit
            end do
            if (eof < 0) exit
            do
              pstnum = 0
              read (11,*,iostat=eof) pstnum
              if (eof < 0) exit
              if (pstnum > 0) pstflg(pstnum) = 1
            end do
            if (eof < 0) exit
          end do
        close (11)
        close (10)
        close (9)
      end do

      return
 5000 format (6a)
 5001 format (a1,9x,5i6)
 5002 format(a)
 5100 format (20a4)
 5200 format (10i4)
 5300 format (6a13)
 6000 format (a80)
 6100 format (27x,25f12.2)
 6200 format (1x,i3)
 6300 format (16x,i2,1x,i4)
      end

