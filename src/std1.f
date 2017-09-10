      subroutine std1

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes general information to the standard output file
!!    and header lines to miscellaneous output files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_km       |km**2         |area of the watershed in square kilometers
!!    icrk        |none          |crack flow code
!!                               |1: compute flow in cracks
!!    ideg        |none          |channel degredation code
!!                               |1: compute channel degredation (downcutting
!!                               |   and widening)
!!    idg(:)      |none          |array location of random generator seed
!!    idt         |minutes       |length of time step used to report
!!                               |precipitation data for sub-daily modeling
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    igen        |none          |random number generator seed code
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    iwq         |none          |stream water quality code
!!                               |0 do not model stream water quality
!!                               |1 model stream water quality (QUAL2E)
!!    nbyr        |none          |number of calendar years simulated
!!    pcpsim      |none          |rainfall input code
!!    prog        |NA            |program name and version
!!    rndseed(:,:)|none          |random number seeds 
!!    tmpsim      |none          |temperature input code
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

!!    input summary file
      write (24,1000) prog,values(2),values(3),values(1),values(5),   
     &values(6),values(7)
      write (24,1010) title
      write (24,1020) nbyr, da_km
      if (igen == 0) then
        write (24,1030) 
      else
        write (24,1040) igen
      end if
      write (24,1050) rndseed(idg(1),1)
      write (24,1051) rndseed(idg(2),1)
      write (24,1052) rndseed(idg(3),1)
      write (24,1053) rndseed(idg(4),1)
      write (24,1054) rndseed(idg(5),1)
      write (24,1055) rndseed(idg(6),1)
      write (24,1056) rndseed(idg(7),1)
      write (24,1057) rndseed(idg(8),1)
      write (24,1058) rndseed(idg(9),1)

      write (24,1060)
      select case (pcpsim)
        case (1)
          write (24,1061)
          if (ievent > 0) then
            write (24,1062) idt
          else
            write (24,1063)
          end if
        case (2)
          write (24,1064)
      end select
      write (24,1070)
      select case (tmpsim)
        case (1)
          write (24,1071)
        case (2)
          write (24,1072)
      end select

      select case (ipet)
        case (0)
          write (24,1080)
        case (1)
          write (24,1081)
        case (2)
          write (24,1082)
        case (3)
          write (24,1083)
      end select

      write (24,1090)
      select case (ievent)
        case (0)
          write (24,1091)
        case (1)
          write (24,1094)
      end select
      select case (irte)
        case (0)
          write (24,1095)
        case (1)
          write (24,1096)
      end select
      select case (ideg)
        case (0)
          write (24,1097)
        case (1)
          write (24,1098)
      end select
      select case (isubwq)
        case (0)
          write (24,1101)
        case (1)
          write (24,1102)
      end select
      select case (iwq)
        case (0)
          write (24,1099)
        case (1)
          write (24,1100)
      end select

      if (icrk == 1) write (24,1110)

!!    standard output file
      write (26,1000) prog,values(2),values(3),values(1),values(5),     
     &values(6),values(7)
      write (26,1010) title
      write (26,1020) nbyr, da_km
      if (isproj == 1) then
        write (19,1000) prog,values(2),values(3),values(1),values(5),   
     &  values(6),values(7)
        write (19,1010) title
        write (19,1020) nbyr, da_km
      end if
 
!!    hyd.out file
      write (11123,5000)     

!!    chan.deg file
      write (16,7000)

      return
 1000 format ('1',/t5,a80,t105,2(i2,'/'),i4,5x,2(i2,':'),i2)
 1010 format (/(t5,20a4))
 1020 format (t10,'Number of years in run: ',i4/t10,                    
     &       'Area of watershed: ',f12.3,' km2')
 1030 format (t10,'Random number generator cycles: 0, use default number
     &s')
 1040 format (t10,'Random number generator cycles: ',i4)
 1050 format (/t10,'Initial random number seed: wet/dry day prob  ',1x, 
     &i14)
 1051 format (t10,'Initial random number seed: radiation         ',1x,  
     &i14)
 1052 format (t10,'Initial random number seed: precipitation     ',1x,  
     &i14)
 1053 format (t10,'Initial random number seed: 0.5 hr rainfall   ',1x,  
     &i14)
 1054 format (t10,'Initial random number seed: wind speed        ',1x,  
     &i14)
 1055 format (t10,'Initial random number seed: irrigation        ',1x,  
     &i14)
 1056 format (t10,'Initial random number seed: relative humidity ',1x,  
     &i14)
 1057 format (t10,'Initial random number seed: max temperature   ',1x,  
     &i14)
 1058 format (t10,'Initial random number seed: min temperature   ',1x,  
     &i14)
 1060 format (/t10,'Precipitation data used in run:')
 1061 format (t11,'Multiple gages read for watershed')
 1062 format (t14,'Subdaily rainfall data used, summarized every ',i3,  
     &       'min')
 1063 format (t14,'Daily rainfall data used')
 1064 format (t11,'Multiple gages simulated for watershed')
 1070 format (/t10,'Temperature data used in run:')
 1071 format (t11,'Multiple gages read for watershed')
 1072 format (t11,'Multiple gages simulated for watershed')
 1080 format (/t10,'PET method used: Priestley-Taylor')
 1081 format (/t10,'PET method used: Penman-Monteith')
 1082 format (/t10,'PET method used: Hargreaves')
 1083 format (/t10,'PET method used: read in values')
 1090 format (/t10,'Rainfall/Runoff/Routing Option:')
 1091 format (t11,'Daily rainfall data',/t11,'Runoff estimated with ',  
     &        'curve number method',/t11,'Daily stream routing')
 1094 format (t11,'Subdaily rainfall data',/t11,'Runoff estimated with',
     &        ' Green & Ampt method',/t11,'Hourly stream routing')
 1095 format (t12,'Variable Storage routing method')
 1096 format (t12,'Muskingum routing method')
 1097 format (t12,'Channel dimensions remain constant')
 1098 format (t12,'Channel dimensions change due to deposition/degrad', 
     &        'ation')
 1099 format (t12,'In-stream nutrient transformations not modeled')
 1100 format (t12,'In-stream nutrient transformations modeled using',   
     &        ' QUAL2E equations')
 1101 format (t12,'Subbasin algae/CBOD loadings not modeled')
 1102 format (t12,'Subbasin algae/CBOD loadings modeled')
 1110 format (/t10,'Crack flow modeled')
 5000 format ('  icode',t11,'ic',t14,'inum1',t20,'inum2',t26,'inum3',   
     & t34,'subed',t41,'recmonps',t50,'reccnstps',t61,'flow(m^3)',      
     & t73,'sed(t)',t85,'orgn(kg)',t97,'orgp(kg)',t109,'nitrate(kg)',   
     & t121,'sol.p(kg)',t133,'sol.pst(mg)',t145,'sor.pst(mg)')
 7000 format (/,' Initial Dimen',' Channel Dimensions ',/,' Reach',     
     &  '    Depth (m)','  Width (m)','  Slope (m/m)')
      end