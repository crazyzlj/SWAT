      subroutine readfig

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the routing information from the watershed configuration
!!     input file (.fig) and calculates the number of subbasins, reaches, 
!!     and reservoirs
     
!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     hru_sub(:)   |none          |subbasin in which HRU is located
!!     icodes(:)    |none          |routing command code:
!!                                 |0 = finish       9 = save
!!                                 |1 = subbasin    10 = recday
!!                                 |2 = route       11 = reccnst
!!                                 |3 = routres     12 = structure
!!                                 |4 = transfer    13 = apex 
!!                                 |5 = add         14 = saveconc
!!                                 |6 = rechour     15 = 
!!                                 |7 = recmon    
!!                                 |8 = recyear    
!!     ihouts(:)    |none          |For ICODES equal to
!!                                 |0: not used
!!                                 |1,2,3,5,7,8,10,11: hydrograph storage 
!!                                 |                     location number
!!                                 |4: departure type
!!                                 |   (1=reach)
!!                                 |   (2=reservoir)
!!                                 |9: hydrograph storage location of data to 
!!                                 |   be printed to event file
!!                                 |14:hydrograph storage location of data to 
!!                                 |   be printed to saveconc file
!!     inum1s(:)    |none          |For ICODES equal to
!!                                 |0: not used
!!                                 |1: subbasin number
!!                                 |2: reach number
!!                                 |3: reservoir number
!!                                 |4: reach or res # flow is diverted from
!!                                 |5: hydrograph storage location of 1st 
!!                                 |   dataset to be added
!!                                 |7,8,9,10,11,14: file number
!!     inum2s(:)    |none          |For ICODES equal to
!!                                 |0,1,7,8,10,11: not used
!!                                 |2,3: inflow hydrograph storage location
!!                                 |4: destination type
!!                                 |   (1=reach)
!!                                 |   (2=reservoir)
!!                                 |5: hydrograph storage location of 2nd
!!                                 |   dataset to be added
!!                                 |9,14:print frequency
!!                                 |   (0=daily)
!!                                 |   (1=hourly)
!!     inum3s(:)    |none          |For ICODES equal to
!!                                 |0,1,2,3,5,7,8,10,11: not used
!!                                 |4: destination number. Reach or
!!                                 |   reservoir receiving water
!!                                 |9: print format
!!                                 |   (0=normal, fixed format)
!!                                 |   (1=txt format for AV interface,recday)
!!     inum4s(:)    |none          |For ICODES equal to
!!                                 |0,2,3,5,7,8,9,10,11: not used
!!                                 |1: GIS code printed to output file 
!!                                 |   (optional)
!!                                 |4: rule code governing transfer of water
!!                                 |   (1=fraction transferred out)
!!                                 |   (2=min volume or flow left)
!!                                 |   (3=exact amount transferred)
!!     nrch         |none          |number of reaches in watershed
!!     nres         |none          |number of reservoirs in watershed
!!     rnum1s(:)    |none          |For ICODES equal to
!!                                 |0,1,3,5,9: not used
!!                                 |2: Fraction of flow in channel
!!                                 |4: amount of water transferred (as
!!                                 |   defined by INUM4S)
!!                                 |7,8,10,11: drainage area in square kilometers
!!                                 |   associated with the record file
!!     subgis(:)    |none          |GIS code printed to output files(output.sub)
!!     subtot       |none          |number of subbasins in watershed
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     a            |NA            |comment flag in .fig file(*=comment)
!!     annual_in    |NA            |name of file containing average annual
!!                                 |loadings to reach (fig command 11)
!!     day_in       |NA            |name of file containing daily loadings 
!!                                 |to reach (fig command 10)
!!     eof          |none          |end of file flag (=-1 at end of file)
!!     hour_in      |NA            |name of file containing hourly loadings 
!!                                 |to reach (fig command 6)
!!     idum         |none          |counter
!!     ii           |none          |counter
!!     lwqfile      |NA            |reservoir water quality file names (.lwq)
!!     month_in     |NA            |name of file containing monthly loadings
!!                                 |to reach (fig command 7)
!!     resfile      |NA            |reservoir file names (.res)
!!     rtefile      |NA            |reach input file (.rte)
!!     swqfile      |NA            |stream water quality file (.swq)
!!     titldum      |NA            |description line
!!     year_in      |NA            |name of file containing annual loadings
!!                                 |to reach (fig command 8)
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!     Intrinsic: Max
!!     SWAT: caps

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
      character (len=1) ::  a
      character (len=13) :: month_in, day_in, annual_in, year_in
      character (len=13) :: apex_in
      character (len=13) :: hour_in, resfile, lwqfile, rtefile, swqfile
      character (len=13) :: subfile, auto_in , rufile
      integer :: ii, eof

      character (len=3), dimension (mhyd) :: char6, char7, char8
      char6 = "   "
      char7 = "   "
      char8 = "   "

!!    initialize variables
      mhyd_bsn = 0
      idum = 0
      eof = 0

      do 
        a = ""
        read (102,5002,iostat=eof) a
        if (eof < 0) exit
        if (a /= "*") then
          backspace 102
          idum = idum + 1

!!    CEAP project
        if (isproj == 2) then
          read (102,5003) a, icodes(idum), ihouts(idum), inum1s(idum),  
     &    inum2s(idum), inum3s(idum), rnum1s(idum), inum4s(idum)
	  else
          read (102,5000) a, icodes(idum), ihouts(idum), inum1s(idum),  
     &    inum2s(idum), inum3s(idum), rnum1s(idum), inum4s(idum),       
 !!    &    inum5s(idum), inum6s(idum), inum7s(idum), inum8s(idum)
     &    inum5s(idum), char6(idum), char7(idum), char8(idum)
	  end if
        mhyd_bsn = mhyd_bsn + 1 

!!!!!! inum6s, inum7s and inum8s (integer) read in as char6, char7, char8 (character) and 
!!!!!! converted back to integer due to "Subbasin:" included in the .fig file by ArcSWAT
        jjii = 1     !! inum6s/inum7s
        iijj = 0     !! inum8s
        
   !     if (char6(idum) == "Sub") inum6s = 0
   !     if (char7(idum) == "bas") inum7s = 0
   !     if (char8(idum) == "in:") inum8s = 0
        
        if (char6(idum) == "  1")  then
          xyz = 0.
           write (char6(idum), fmt=' (i3)') jjii
           inum6s(idum) = jjii  
        else
           inum6s(idum) = 0
        end if
        
        if (char7(idum) == "  1")  then
          xyz = 0.
           write (char7(idum), fmt=' (i3)') jjii
           inum7s(idum) = jjii  
        else
           inum7s(idum) = 0
        end if
        
        if (char8(idum) == "  0")  then
          xyz = 0.
           write (char8(idum), fmt=' (i3)') iijj
           inum8s(idum) = iijj  
        else
           inum8s(idum) = 1
        end if
!!!!!! end convert code         
            
          select case(icodes(idum))

            case (0)  !! icode = 0  FINISH command
             exit

            case (1)  !! icode = 1  SUBBASIN command
              subtot = subtot + 1
              subgis(inum1s(idum)) = inum4s(idum)
              subfile = ""
              read (102,5100) subfile
              call caps(subfile)
              i = 0
              i = inum1s(idum)
              subed(ihouts(idum)) = inum4s(idum)
              open (101,file=subfile)
              call readsub
              nhru = nhru + hrutot(i)

            case (2)  !! icode = 2  ROUTE CHANNEL command
              nrch = nrch + 1
!!            assume subbasin is the same number as the reach (if zero)              
              if (inum3s(idum) == 0) then
                inum3s(idum) = inum1s(idum)
              end if  
              rtefile = ""
              swqfile = ""
              read (102,5100) rtefile, swqfile
              call caps(rtefile)
              call caps(swqfile)
              irch = 0
              irch = inum1s(idum)
              open (103,file=rtefile)
              open (104,file=swqfile)
              call readrte
              call readswq

            case (3)  !! icode = 3  ROUTE RESERVOIR command
              nres = nres + 1
              resfile = ""
              lwqfile = ""
              read (102,5100) resfile, lwqfile
              call caps(resfile)
              call caps(lwqfile)
              i = 0
              i = inum1s(idum)
              open (105,file=resfile)
              call readres
              if (lwqfile /= '             ') then
                open (106,file=lwqfile)
                call readlwq
              end if
            !! lake water quality default values
              call lwqdef
 
            case (4)  !! icode = 4  TRANSFER command: read in beg/end month
              read (102,5004) mo_transb(inum5s(idum)),    
     &		    mo_transe(inum5s(idum)), ih_tran(inum5s(idum))


            case (6)  !! icode = 6  RECHOUR command: read in hourly values
                      !! with water in m^3 and rest in tons/kgs
              hour_in = ""
              read (102,5100) hour_in
              call caps(hour_in)
              open (200+inum1s(idum),file=hour_in,recl=350)
              do ii = 1, 6
                read (200+inum1s(idum),5200) titldum
              end do

            case (7)  !! icode = 7  RECMON command:
                      !!  read in monthly values
            month_in = ""
            read (102,5100) month_in
              recmonps(ihouts(idum)) = month_in(1:index(month_in,'.')-1)
            call caps(month_in)
            i = 0
            i = inum1s(idum)
            open (107,file=month_in,recl=350)
            call readmon
            
            case (8)  !! icode = 8  RECYEAR command: 
                      !! read in average daily loadings for each year
            year_in = ""
            read (102,5100) year_in
            
            call caps(year_in)
            i = 0
            i = inum1s(idum)
            open (108,file=year_in,recl=350)
            call readyr

            close (108)

            case (9)  !! icode = 9  SAVE command: saves daily reach
                      !! constituent masses from a hydrograph node on
                      !! the channel network to an output file 
              day_in = ""
              read (102,5100) day_in
              call caps(day_in)
              !if (inum1s(idum) <= 10 .and. inum1s(idum) > 0) then
                open (30000+inum1s(idum),file=day_in,recl=350)
                if (inum3s(idum) == 0) then
                  write (30000+inum1s(idum),5400) title
                  write (30000+inum1s(idum),5500)
                else
                  iida = 0
                  iida = idaf
                  call xmon
                  write (30000+inum1s(idum),5501) iyr, i_mo,               
     &                                              (iida - ndays(i_mo))
                end if
              !end if

            case (10) !! icode = 10  RECDAY command: read in daily values
                      !! with water in cms and rest in tons
              day_in = ""
              read (102,5100) day_in
              call caps(day_in)
              open (555+inum1s(idum),file=day_in,recl=350)
              do ii = 1,4
                read (555+inum1s(idum),5200) titldum
              end do
              read (555+inum1s(idum),5201) isalt
 5201         format (i4)
              read (555+inum1s(idum), 5200) titldum
              
            case (11) !! icode = 11  RECCNST command: read in average 
                      !! annual values with water in m^3, sed in t, and 
                      !! the nutrients in kg
            annual_in = ""
            read (102,5100) annual_in
            reccnstps(ihouts(idum))=annual_in(1:index(annual_in,'.')-1)
            call caps(annual_in)
            i = 0
            i = inum1s(idum)
            open (109,file=annual_in,recl=350)
            call readcnst

!! code to read from apex output file
            case (13) 
            apex_in = ""
            read (102,5100) apex_in
	      call caps(apex_in)
      !      i = 0
      !      i = inum1s(idum)
            open (400+inum1s(idum),file=apex_in,recl=350)
	      do ii = 1, 9 
              read (400+inum1s(idum),5200) titldum
            end do
!! code to read from apex output file
         

            case (14) !! icode = 14 SAVECONC command: saves hourly or 
                      !! daily reach constituent concentrations to an 
                      !! output file from a hydrograph node on the 
                      !! channel network
              day_in = ""
              read (102,5100) day_in
              call caps(day_in)
              if (inum1s(idum) <= 50 .and. inum1s(idum) > 0) then
                open (50+inum1s(idum),file=day_in,recl=350)
                write (50+inum1s(idum),5400) title
                write (50+inum1s(idum),5600)
              end if

            case (17)  !! icode = 17  ROUTING UNIT command
              rufile = ""
              read (102,5100) rufile
              call caps(rufile)
              iru = inum1s(idum)
              isub = inum2s(idum)
              daru_km(isub,iru) = rnum1s(idum)
              open (113,file=rufile)
              call readru
              close(113)
              
            case (18)  !! icode = 18  LANDSCAPE ROUTING command
              !!if (rnum1s(idum) < 1.e-9) rnum1s(idum) = 1.
              
          end select
          
          !! calculate upstream drainage area (km2) and impervious cover (km2)
          !! in the drainage arae at each subbasin outlet
          if (icodes(idum)==1) then      !subbasin
            subdr_km(ihouts(idum)) = sub_km(inum1s(idum))
          elseif (icodes(idum)==17) then !routing unit
            subdr_km(ihouts(idum)) = daru_km(inum2s(idum),inum1s(idum))
          elseif (icodes(idum)==5) then  !add
            subdr_km(ihouts(idum)) = subdr_km(inum1s(idum)) 
     &              + subdr_km(inum2s(idum)) 
            subdr_ickm(ihouts(idum)) = subdr_ickm(inum1s(idum))
     &              + subdr_ickm(inum2s(idum)) 
          elseif (icodes(idum)==2) then !route
            if(inum1s(idum)==inum2s(idum)) then
                subdr_km(ihouts(idum)) = subdr_km(inum1s(idum)) 
                subdr_ickm(ihouts(idum)) = subdr_ickm(inum1s(idum))
            else
                subdr_km(ihouts(idum)) = subdr_km(inum1s(idum)) 
     &              + subdr_km(inum2s(idum)) 
                subdr_ickm(ihouts(idum)) = subdr_ickm(inum1s(idum))
     &              + subdr_ickm(inum2s(idum)) 
            endif
          elseif (icodes(idum)==18) then  !routels
            subdr_km(ihouts(idum)) = subdr_km(inum2s(idum)) 
            ru_a(inum3s(idum),inum1s(idum)) = subdr_km(ihouts(idum)) *  
!     &                        daru_km(inum3s(idum),inum1s(idum))) /     
     &                     100. / ru_ovsl(inum3s(idum),inum1s(idum))
          end if

        end if
      end do

!! close .fig file
      close (102)


      return
!! isproj = 0
!! 5000 format (a1,9x,5i6,f6.3,i9,4i3)
 5000 format (a1,9x,5i6,f6.3,i9,i3,3a3)
 5001 format (7x,i3,4x,6f12.3)
 5002 format(a)
 5004 format (10x,3i4)
!! isproj = 2 (CEAP)
 5003 format (a1,9x,4i6,i5,f8.0,i8)
 5100 format (10x,2a13)
 5200 format (a80)
 5300 format (2i6)
 5400 format (20a4)
 5500 format (//," DAY YEAR HR ","  WATER m^3 ","   SED tons ",         
     &         "    ORGN kg ","    ORGP kg ","   NO3-N kg ",            
     &         "   NH3-N kg ","   NO2-N kg ","    MINP kg ",            
     &         "    CBOD kg ","   DISOX kg ","    CHLA kg ",            
     &         "  SOLPST mg ","  SORPST mg ","    BACTP # ",            
     &         "   BACTLP # ","CMETAL#1 kg ","CMETAL#2 kg ",            
     &         "CMETAL#3 kg ","  TEMP degC ")
 5501 format (i4,2i2,", Point Discharge File created with Save Comand")
 5600 format (//," Year  Day Step  FLOWm^3/s    SEDmg/L   ORGNmg/L",    
     &         "   ORGPmg/L    NO3mg/L    NH3mg/L    NO2mg/L",          
     &         "   MINPmg/L   CBODmg/L  DISOXmg/L   CHLAug/L",          
     &         " SOLPSTmg/L SORPSTmg/L  BACTPct/L BACTLPct/L",          
     &         "CMETAL1mg/LCMETAL2mg/LCMETAL3mg/L   TEMPdegC")
      end