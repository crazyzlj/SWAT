      subroutine command
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    for every day of simulation, this subroutine steps through the command
!!    lines in the watershed configuration (.fig) file. Depending on the 
!!    command code on the .fig file line, a command loop is accessed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    icodes(:)   |none          |routing command code:
!!                               |0 = finish       9 = save
!!                               |1 = subbasin    10 = recday
!!                               |2 = route       11 = reccnst
!!                               |3 = routres     12 = structure
!!                               |4 = transfer    13 = 
!!                               |5 = add         14 = saveconc
!!                               |6 = rechour     15 = 
!!                               |7 = recmon    
!!                               |8 = recyear     
!!    ihouts(:)   |none          |For ICODES equal to
!!                               |0: not used
!!                               |1,2,3,5,7,8,10,11: hydrograph storage
!!                               |                     location number
!!                               |4: water source type
!!                               |   (1=reach)
!!                               |   (2=reservoir)
!!                               |9: hydrograph storage location of data to
!!                               |   be printed to event file
!!                               |14:hydrograph storage location of data to
!!                               |   be printed to saveconc file
!!    inum1s(:)   |none          |For ICODES equal to
!!                               |0: not used
!!                               |1: subbasin number
!!                               |2: reach number
!!                               |3: reservoir number
!!                               |4: reach or res # flow is diverted from
!!                               |5: hydrograph storage location of 1st
!!                               |   dataset to be added
!!                               |7,8,9,10,11,14: file number
!!    inum2s(:)   |none          |For ICODES equal to
!!                               |0,1,7,8,10,11: not used
!!                               |2,3: inflow hydrograph storage location
!!                               |4: destination type
!!                               |   (1=reach)
!!                               |   (2=reservoir)
!!                               |5: hydrograph storage location of 2nd
!!                               |   dataset to be added
!!                               |9,14:print frequency
!!                               |   (0=daily)
!!                               |   (1=hourly)
!!    inum3s(:)   |none          |For ICODES equal to
!!                               |0,1,5,7,8,10,11: not used
!!                               |2,3: subbasin number
!!                               |4: destination number. Reach or
!!                               |   reservoir receiving water
!!                               |9: print format
!!                               |   (0=normal, fixed format)
!!                               |   (1=txt format for AV interface,recday)
!!    inum4s(:)   |none          |For ICODES equal to
!!                               |0,2,3,5,7,8,9,10,11: not used
!!                               |1: GIS code printed to output file
!!                               |   (optional)
!!                               |4: rule code governing transfer of water
!!                               |   (1=fraction transferred out)
!!                               |   (2=min volume or flow left)
!!                               |   (3=exact amount transferred)
!!    mhyd        |none          |maximum number of hydrographs
!!    rnum1s(:)   |none          |For ICODES equal to
!!                               |0,1,3,5,9: not used
!!                               |2: fraction of overland flow
!!                               |4: amount of water transferred (as
!!                               |   defined by INUM4S)
!!                               |7,8,10,11: drainage area in square kilometers
!!                               |   associated with the record file
!!                               |12: rearation coefficient
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    icode       |none          |variable to hold value for icodes(:)
!!    ihout       |none          |variable to hold value for ihouts(:)
!!    inum1       |none          |variable to hold value for inum1s(:)
!!    inum2       |none          |variable to hold value for inum2s(:)
!!    inum3       |none          |variable to hold value for inum3s(:)
!!    inum4       |none          |variable to hold value for inum4s(:)
!!    rnum1       |none          |variable to hold value for rnum1s(:)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    idum        |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: subbasin, route, routres, transfer, addh, recmon
!!    SWAT: recepic, save, recday, recyear

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      idum = 0

      do ii = 1, mhyd_bsn
        icode = 0
        ihout = 0
        inum1 = 0
        inum2 = 0
        inum3 = 0
        rnum1 = 0.
        inum4 = 0
        idum = idum + 1
        icode = icodes(idum)
        ihout = ihouts(idum)
        inum1 = inum1s(idum)
        inum2 = inum2s(idum)
        inum3 = inum3s(idum)
        rnum1 = rnum1s(idum)
        inum4 = inum4s(idum)
        inum5 = inum5s(idum)
        inum6 = inum6s(idum)
        inum7 = inum7s(idum)
        inum8 = inum8s(idum)

        select case (icode)
          case (0)
            return
          case (1)
            if (isub_sav == 0) then
              call subbasin
              call print_hyd
            end if
          case (2) 
            call route
            if (dtp_onoff(inum1)==1) call bmp_det_pond  !route detention pond J.Jeong feb 2010
            if (wtp_onoff(inum1)==1) call bmp_wet_pond  !route wetention pond J.Jeong june 2010
            call sumhyd
            call print_hyd
          case (3) 
            call routres
            call sumhyd
          case (4) 
            call transfer
          case (5) 
            call addh
            call sumhyd
            call print_hyd
          case (6) 
            call rechour
            call sumhyd
            call print_hyd
          case (7) 
            call recmon
            call sumhyd
          case (8) 
            call recyear
            call sumhyd
          case (9) 
            call save
          case (10) 
            call recday
            call sumhyd
          case (11) 
            call reccnst
            call sumhyd
          case (12)
            call structure
          case (13) 
            call apex_day
          case (14)
            if (curyr > nyskip) call saveconc
          case (17)
            call routeunit
            call sumhyd
          case (18)
            iru_sub = inum1   !!routing unit number
            inum8 = 1
            call routels(iru_sub)
            call sumhyd
        end select

      end do

      return
      end