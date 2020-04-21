      subroutine ruallo

      use parm

      character (len=13) :: subfile, figfile
      character (len=1) ::  a
      character (len=80) ::  titldum
      integer :: icd, inm1, inm2, inm3, iht, eof, numhru, ic

      icd = 1

!! process .fig file
      do while (icd > 0)
        read (25,5002) a
        if (a /= "*") then
          backspace 25

          read (25,5001) a, icd, iht, inm1, inm2, inm3

          select case (icd)
          case (2)                      !! icd = 2  ROUTE command
            mch = mch + 1               !! # channels
            read (25,5002) a
          case (3)                      !! icd = 3  ROUTE RESERVOIR command
            mres = mres + 1
            read (25,5002) a
          case (4)
	      read (25,5002) a             !! icd = 4  TRANSFER command
            mtran = mtran + 1
          case (6)                      !! icd = 6  RECALL HOUR command
            read (25,5002) a
            mrech = mrech + 1
          case (7)                      !! icd = 7  RECALL MONTH command
            read (25,5002) a
            mrecm = mrecm + 1
          case (8)                      !! icd = 8  RECALL YEAR command
            read (25,5002) a
            mrecy = mrecy + 1
          case (9)                      !! icd = 9  SAVE command
            read (25,5002) a
            nsave = nsave + 1
          case (10)                     !! icd = 10 RECALL DAY command
            read (25,5002) a
            mrecd = mrecd + 1
          case (11)                     !! icd = 11 RECALL CONSTANT command
            read (25,5002) a
            mrecc = mrecc + 1
          case (13)                     !! icd = 13 APEX command
            read (25,5002) a
            mapex = mapex + 1
          case (14)                     !! icd = 14 SAVECONC command
            read (25,5002) a
            nsave = nsave + 1
          case (16)                     !! icd = 16 AUTOCAL command
            read (25,5002) a
            nauto = nauto + 1
          case (17)                     !! icd = 17 ROUTE UNIT command
            read (25,5002) a
            mru = mru + 1
          end select

          mhyd = Max(mhyd,iht)

        end if
      end do

	return
 5000 format (6a)
 5001 format (a1,9x,5i6)
 5002 format(a)
 5100 format (20a4)
 6000 format (a80)
 6100 format (10x,a13)
 6200 format (i3)
 6300 format (i4)

	end
