       subroutine response(sensw, isens, isenspar, iitel)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    calculating the objective functions for SCEUA

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iobj         |none          |number of objective functions
!!    obj(:)       |none          |values of the objective functions
!!    icalpar(:,:) |none          |objective functions codes of CALMET.DAT
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


        real*8   sens(isens)
        INTEGER yout, dagout,  hout, itelt
        integer itel,status, iitel, isenspar(4,isens), isens
        real*8  outmodel(22), sensw(isens), tot, outval

c
c::::::::::::::
c  response.for
c::::::::::::::

c
c  mean sqare function
c  

        do mm=1,isens
	  sens(mm)=0.
	if (sensw(mm) < 1.e-6) sensw(mm) = 0.
	  val=sensw(mm)
        ii=isenspar(1,mm)
        jj=isenspar(2,mm)
        kk=isenspar(3,mm)
        ll=isenspar(4,mm)
      rewind(8000+ll)
        itel=0
        itelt=0
	  tot=0.
5	continue
      read(8000+ll,5000, end=4)yout, dagout,hout,(outmodel(ki), ki=1,22)
5000  format(1x,i4,2x,i3,1x,i2,22e11.3)
	
       itel=itel+1

	outval=outmodel(ii)
	if (kk.eq.1.and.ii.ne.1) outval=(3600.*24./1000000.)*outval*
     *	outmodel(1)
	


        select case (jj)
        case (1) 
         tot=tot+outval
        case (2)
	  if (outval.lt.val) itelt=itelt+1
        end select
	go to 5
4	continue	
        select case (jj)
        case (1) 
         sens(mm)=tot/itel
        case (2)
	   sens(mm)=itelt*100./itel
        end select
        end do

        write(18014,8012) iitel, (sens(mm), mm=1,isens)
8012    format(i5, 40e12.5)
         return
      end
