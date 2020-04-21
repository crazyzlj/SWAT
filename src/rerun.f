      subroutine rerun(xxo ,nopt, iiname, inrhru, iinr, iitel,imet,ipr)


!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reruns SWAT for autocalibration

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    xxo(:)       |none          |new values of the parameters 
!!    nopt        |none          |number of parameters to optimise
!!    iiname(:)    |none          |code refering to which parameter to change, and how
!!    iitel        |none          |number of simulation run
!!    iinr(:)      !              |number of HRUs to change for parameter (:)
!!    inrhru(:,:)  |none          |list of HRU numbers to change  

!!    date        |NA            |date simulation is performed where leftmost
!!                               |eight characters are set to a value of
!!                               |yyyymmdd, where yyyy is the year, mm is the 
!!                               |month and dd is the day
!!    time        |NA            |time simulation is performed where leftmost
!!                               |ten characters are set to a value of
!!                               |hhmmss.sss, where hh is the hour, mm is the 
!!                               |minutes and ss.sss is the seconds and
!!                               |milliseconds
!!    values(1)   |year          |year simulation is performed
!!    values(2)   |month         |month simulation is performed
!!    values(3)   |day           |day in month simulation is performed
!!    values(4)   |minutes       |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    values(5)   |hour          |hour simulation is performed
!!    values(6)   |minutes       |minute simulation is performed
!!    values(7)   |seconds       |second simulation is performed
!!    values(8)   |milliseconds  |millisecond simulation is performed
!!    zone        |NA            |time difference with respect to Coordinated
!!                               |Universal Time (ie Greenwich Mean Time)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    prog        |NA            |program name and version
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    i           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: date_and_time
!!    SWAT: getallo, readfile, readfig
!!    SWAT: readbsn, std1, readwwq, readinpt, std2, storeinitial
!!    SWAT: openwth, headout, simulate, finalbal, writeaa, pestw 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real*8 xxo(nopt)
      integer iiname(nopt), iinr(nopt), inrhru(nopt,2000)
	integer iitel, nopt, imet(nopt)
      integer*2 igior, imese,ianno
  
      character*80 titldum
      prog = "SWAT  May '02 VERSION2003"


!! process input

        write(*,*) 'simulation ',iitel
        do  ii = 1,2000 
        rewind(ii)
        end do
        do ii = 3000, 3500
        rewind(ii)
        end do

        close (8000)
      call getallo2
      call zero0
      call zero1
      call zero2
      call zeroini
      call readfile
!     call date_and_time (date, time, zone, values)

      call readbsn
      call readwwq
      call readplant             !! read in the landuse/landcover database
      call readtill              !! read in the tillage database
      call readpest              !! read in the pesticide database
      call readfert              !! read in the fertilizer/nutrient database
      call readurban             !! read in the urban land types database
      call readfig
      call changepar(xxo,nopt, iiname, inrhru,iinr, imet)
      call readinpt
      call std1
      call std2
      call openwth
      call headout


     

        !! simulate watershed processes

        call simulate
        !! perform summary calculations
	if (ipr.eq.1) then
        call finalbal
        call writeaa
        call pestw
	end if
      do i = 1, 9
        close (i)
      end do

      return
      end
