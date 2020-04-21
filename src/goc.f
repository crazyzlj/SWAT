       subroutine goc(iobj,obj,omean,ologstd,
     *ologmean,ostd,omin, omax,o5,ffz,calw,wgoc)
	use parm

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    calculating the GOC for several OF's

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iobj         |none          |number of objective functions
!!    obj(:)       |none          |values of the objective functions
!!    omean(:)     |none          |means for the objective functions
!!    ostd(:)      |none          |standard deviations for the objective functions
!!    ologmean(:)  |none          |log means for the objective functions
!!    ologstd(:)   |none          |log standard deviations for the objective functions
!!    omax(:)      |none          |maximum for the objective functions
!!    omin(:)      |none          |minimum for the objective functions
!!    o5(:)        |none          |5 percentile for the objective functions
!!    iclb         |none          |method to form GOC out of objective functions
!!    icalw        |none          |weight given for the objectives
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wgoc         |none          |calculated GOC
!!    ffz(:)       |none          |transformed objectif function
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~


       real*8  omean(40), ologstd(40), obj(40),ffz(40),wgoc,
     *ologmean(40),ostd(40),omin(40), omax(40),o5(40),calw(100)
      integer iobj
c      transformation for goc
       wgoc=0.
       y=0.
       ffw=0.
        do mm=1,iobj
         select case (iclb)
          case(1)
           fz=obj(mm)
          case(2) 
           fz=obj(mm)/omean(mm)
          case(3)
           z = (obj(mm)-omean(mm))/ostd(mm)
           fz1=0.196854*abs(z)
           fz2=0.115194*z**2
           fz3=0.000344*abs(z)**3
           fz4=0.019527*z**4
           fzz=(1+fz1+fz2+fz3+fz4)**4
           fz= 0.5/fzz
             if (z.gt.0.)fz=1-fz
          y=fz 
         case(4)
          z = (log(obj(mm))-ologmean(mm))/ologstd(mm)
          fz1=0.196854*abs(z)
          fz2=0.115194*z**2
          fz3=0.000344*abs(z)**3
          fz4=0.019527*z**4
          fzz=(1+fz1+fz2+fz3+fz4)**4
          fz= 0.5/fzz
          if (z.gt.0.)fz=1-fz
          case(5)
           fz=(obj(mm)/omin(mm))**2
          case(6)
           fz=(obj(mm)/o5(mm))**2
          end select
          ffz(mm)=fz*calw(mm)
          wgoc=wgoc+ffz(mm)
          end do
         return
      end
c==================================================================
