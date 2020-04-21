       subroutine writeswatfile

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    write swatoutputs to files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iobj         |none          |number of objective functions
!!    nobs(:)       |none         |number of observations
!!    icalpar(:,:) |none          |objective functions codes of OBJk_annaMET.DAT
!!    iitel        |none          |number of simulation run
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    obj(:)       |none          |values of the objective functions
!!    ffz(:)       |none          |transformed objectif function
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

		implicit real*8 (a-h,o-z)
        INTEGER yout, dagout, ydat, dagdat, daysout, daysdat, hdat, hout
        real*8 calouttot(22), xout(14)

c  mean sqare function
c  
	rewind(8001)
	rewind(8002)
	rewind(8003)
	rewind(8004)
	rewind(8005)

47	continue
      read(8001,5000, end=60)yout, dagout, hout,(calouttot(ki), ki=1,22)
	  xout(1)=calouttot(1)
	  xout(2)=calouttot(2)
c	  xout(3)=calouttot(21)
c	  xout(4)=calouttot(22)
c      read(8002,5000, end=60)yout, dagout, hout,(calouttot(ki), ki=1,22)
c	  xout(5)=calouttot(1)
c	  xout(6)=calouttot(2)
c	  xout(7)=calouttot(21)
c	  xout(8)=calouttot(22)
c      read(8003,5000, end=60)yout, dagout, hout,(calouttot(ki), ki=1,22)
c	  xout(9)=calouttot(1)
c	  xout(10)=calouttot(2)
c	  xout(11)=calouttot(21)
c	  xout(12)=calouttot(22)
c     read(8004,5000, end=60)yout, dagout, hout,(calouttot(ki), ki=1,22)
c	  xout(13)=calouttot(1)
c      read(8005,5000, end=60)yout, dagout, hout,(calouttot(ki), ki=1,22)
c	  xout(14)=calouttot(1)


	write(18019,5001) yout, dagout,(xout(ik),ik=1,2)
	  go to 47

60	continue
5000  format(1x,i4,2x,i3,1x,i2,22e10.3)
5001  format(1x,i4,2x,i3,22e10.3)
        return
        end
