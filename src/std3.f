      subroutine std3

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes the annual table header to the standard output 
!!    file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    curyr         |none        |current year in simulation
!!    isproj        |none        |special project code:
!!                               |1 test rewind (run simulation twice)
!!    prog          |NA          |program name and version
!!    title         |NA          |title lines from file.cio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
 
!!    standard output file
      if (iscen == 1) then
        write (26,1000) prog
        write (26,1100) title
        write (26,1200) curyr
        write (26,1300)
      else if (isproj == 1) then
        write (19,1000) prog
        write (19,1100) title
        write (19,1200) curyr
        write (19,1300)
      endif

      return
 1000 format ('1',/t5,a80,t105,2(a2,'/'),a2,5x,2(i2,':'),i2)
 1100 format (/(t5,20a4))
 1200 format ('Annual Summary for Watershed in year ',i4,               &
     &        ' of simulation',/)
 1300 format (' UNIT',t36,'PERCO',t44,'TILE',t71,'WATER',t80,'SED',t87, &
!     &    'NO3',t94,'NO3',t101,'NO3',t108,'NO3',t119,'N',t127,'P',t135, &
     &    'NO3',t94,'NO3',t101,'NO3',t108,'NO3',t119,'N',t127,          &
     &    'P       P',
!     &    'P',/,' TIME',t9,'PREC',t16,'SURQ',t23,'LATQ',t31,'GWQ',t37,  &
     &    /,' TIME',t9,'PREC',t16,'SURQ',t23,'LATQ',t31,'GWQ',t37,      &
     &    'LATE',t47,'Q',t52,'SW',t59,'ET',t66,'PET',t71,'YIELD',t78,   &
     &    'YIELD',t86,'SURQ',t93,'LATQ',t100,'PERC',t107,'CROP',t113,   &
     &    'ORGANIC',t121,'SOLUBLE',t129,'ORGANIC',t139,'TILENO3'/,      &
     &    t9,'(mm)',t16,'(mm)'                                          &
!     &    'ORGANIC',t121,'SOLUBLE',t129,'ORGANIC',/,t9,'(mm)',t16,'(mm)'&
     &    ,t23,'(mm)',t30,'(mm)',t37,'(mm)',t44,'(mm)',t51,'(mm)',t58,  &
     &    '(mm)',t65,'(mm)',t72,'(mm)',t77,'(t/ha)',t85,                &
     &    '-----------------(kg nutrient/ha)------------------',        &
     &    t139,'(kg/ha)')
      end
