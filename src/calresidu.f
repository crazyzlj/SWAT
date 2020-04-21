      subroutine calresidu(iobj, icalpar, calw,itel)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    creating a file with the residues for VAO5A

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iobj         |none          |number of objective functions
!!    obj(:)       |none          |values of the objective functions
!!    icalpar(:,:) |none          |objective functions codes of CALMET.DAT
!!    icalw        |none          |weight given for the objectives as given in CALMET.DAT
!!    iitel        |none          |number of simulation run
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~


       INTEGER yout, dagout, ydat, dagdat, daysout, daysdat, hdat, hout
       integer itel,status,iobj,icalpar(4,100)
       real*8 obj(40),calw(100),calouttot(14), caldattot(14)
    

c
c::::::::::::::
c  calredidu.for: calculates residuals 
c::::::::::::::

c
c mean sqare function
c  
!     !  !         !         !         !         !         !         ! !
       itel=0
       do mm=1,iobj
       ii=icalpar(1,mm)
       jj=icalpar(2,mm)
       kk=icalpar(3,mm)
       ll=icalpar(4,mm)
       close(18022)	
       open(18022,file='vaores.dat',status='unknown')	
       rewind(8000+ll)
       rewind(9000+ll)
      read(8000+ll,5000)yout, dagout, hout,(calouttot(ki), ki=1,14)
      read(9000+ll,5000) ydat, dagdat, hdat,(caldattot(ki), ki=1,14)
47    continue
      daysout = yout*100000 + dagout*100+hout
      daysdat = ydat*100000 +dagdat*100+hdat
      if (daysout-daysdat) 44,45,46
44    read (8000+ll,5000, end=60) yout, dagout, hout,
     *  (calouttot(ki), ki=1,14)
      go to 47

45    continue		
      if (caldattot(ii).lt.0.or.calouttot(ii).lt.0.) go to 49
       itel = itel+1


       cal1=caldattot(ii)
       cal2=calouttot(ii)

       residu=calw(mm)*(cal1-cal2)
       write(18022,100)itel, residu, iobj
100    format(i6,1x,f10.3,i4)
49     continue
       read (8000+ll,5000, end=60) yout, dagout, hout,
     *(calouttot(ki), ki=1,14)
       read (9000+ll,5000, end=60) ydat, dagdat, hdat,
     *(caldattot(ki), ki=1,14)
       go to 47

46     read (9000+ll,5000, end=60) ydat, dagdat,hdat,
     *(caldattot(ki), ki=1,14)
       go to 47
       end do
60     continue
5000  format(1x,i4,2x,i3,1x,i2,14(1x,e10.3))
8012  format(i5, 40e10.3)
      return
      end
c==================================================================
 
