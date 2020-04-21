      subroutine readmetfiles(iobj,calw, icalpar,isenspar,sensw,
     *      isens)
!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
      real*8 calw(iobj), sensw(isens)
      integer isens
      integer iobj
      integer icalpar(4,iobj), isenspar(4,isens)
      character*10 icalpar1(22), icalpar2(8), isenspar2(5)
      character*5 icalpar3a(22),icalpar3b(22)
      common /iopar/ in,iprii
      data icalpar1/'flow      ','sediment  ','organic N ','organic P ',
     *'nitrate   ','ammonia   ','nitrite   ','mineral P ','CBOD      ',                  
     *'oxygen    ','chl-a     ','sol pest  ','sor pest  ','perc bact ',                  
     *'see manual','see manual','metal#2   ','metal#3   ','temperat  ',                  
     *'kjel N    ','total N   ','total P   '/                  

      data icalpar2/'SSQ       ','SSQ-log   ','SSQ-sqrt  ','          ',
     *'SSQR      ','          ','          ','BIAS      '/

      data isenspar2/'aver.','     ','          ','          ',
     *'          '/

      data icalpar3a/'m3/s ','mg/L ','mg/L ','mg/L ','mg/L ','mg/L '
     *,'mg/L ','mg/L ','mg/L ','mg/L ','ug/L ','mg/L ','mg/L '                  
     *,'ct/L ','**** ','**** ','mg/L ','mg/L ','degC ','mg/L '                  
     *,'mg/L ','mg/L '/
      data icalpar3b/'m3/s ','T/d  ','T/d  ','T/d  ','T/d  ','T/d  '
     *,'T/d  ','T/d  ','T/d  ','T/d  ','kg/d ','T/d  ','T/d  '                  
     *,'Gct/d','*****','*****','T/d  ','T/d  ','heat ','T/d  '                  
     *,'T/d  ','T/d  '/



c
c  READ THE OBJECTIVE FUNCTIONS CONTROL PARAMETERS

        do mm=1,iobj
!     !  !         !         !         !         !         !         ! !
      read(18016,997, end=996) icalpar(1,mm),icalpar(2,mm),icalpar(3,mm)
     *   ,icalpar(4,mm),calw(mm) 
        if (calw(mm).le.0.) calw(mm)=1.
        if (icalpar(1,mm).le.0) go to 996
        end do
996     continue
       
        write(iprii,*) "Objective functions are (objmet.dat): "
        do mm = 1,iobj
      write(iprii,305) mm
305      format('OBJECTIVE FUNCTION', i3,' =')

      if (icalpar(1,mm).eq.1) then
            write(iprii,301) icalpar2(icalpar(2,mm)),
     *      icalpar1(icalpar(1,mm)),
     *   icalpar(4,mm)
      else
            if (icalpar(3,mm).eq.0) then
            write(iprii,302) icalpar2(icalpar(2,mm)),
     *      icalpar1(icalpar(1,mm)),
     *      icalpar3a(icalpar(1,mm)),
     *    icalpar(4,mm)
            else
            write(iprii,303) icalpar2(icalpar(2,mm)),
     *      icalpar1(icalpar(1,mm)),
     *      icalpar3b(icalpar(1,mm)),
     *    icalpar(4,mm)
            end if      
      end if
      end do


c
c
c  READ THE CONTROL PARAMETERS
      write (*,*) ' READING RESPONSE CONTROL PARAMETERS'

        do mm=1,isens
!     !  !         !         !         !         !         !         ! !
      read(18017,997, end=998) isenspar(1,mm),isenspar(2,mm),
     *   isenspar(3,mm),isenspar(4,mm),sensw(mm) 
        if (isenspar(1,mm).le.0) go to 998
        end do
998     continue
        write(iprii,*) "Response functions are (responsmet.dat): "
        do mm = 1,isens
      write(iprii,300) mm
300      format('OUTPUT ',i3,' =')

      if (isenspar(1,mm).eq.1) then
            write(iprii,301) isenspar2(isenspar(2,mm)),
     *      icalpar1(isenspar(1,mm)),isenspar(4,mm)
301   format(a5,' for ',a10,'       (m^3/s)',' at location',i3)
      else
            if (isenspar(3,mm).eq.0) then
            write(iprii,302) isenspar2(isenspar(2,mm)),
     *      icalpar1(isenspar(1,mm)),
     *      icalpar3a(isenspar(1,mm)),
     *   isenspar(4,mm)
302   format(a5,' for ',a10,' conc.',' (',a5,')',' at location',i3)
            else
            write(iprii,303) isenspar2(isenspar(2,mm)),
     *      icalpar1(isenspar(1,mm)),
     *      icalpar3b(isenspar(1,mm)),
     *   isenspar(4,mm)
303   format(a5,' for ',a10,' load ',' (',a5,')',' at location',i3)
            end if      
      end if
       end do


997   format(4i3, f8.3)      



      return
      end
