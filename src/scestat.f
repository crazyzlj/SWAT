       subroutine scestat(npt1,iobj,nopt,xobjf,omean,ologstd,
     *ologmean,ostd,omin, omax,o5)
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    calculating the statistical characteristics of the OF to be used for calculating the GOC

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    npt1         |none          |number of random sampled runs in loop 0
!!    iobj         |none          |number of objective functions
!!    nopt         |none          |number parameters to optimise
!!    xobj(:,:)    |none          |values of the objective functions for the npt1 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    omean(:)     |none          |means for the objective functions
!!    ostd(:)      |none          |standard deviations for the objective functions
!!    ologmean(:)  |none          |log means for the objective functions
!!    ologstd(:)   |none          |log standard deviations for the objective functions
!!    omax(:)      |none          |maximum for the objective functions
!!    omin(:)      |none          |minimum for the objective functions
!!    o5(:)        |none          |5 percentile for the objective functions!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

      real*8, dimension (:), allocatable :: ranker
      real*8  omean(40), ologstd(40), x(2000,100),xobjf(2000,40),       & 
     *ologmean(40),ostd(40),omin(40), omax(40),o5(40),y(2000,1)
      integer npt1, iobj,nopt
c     statistics on objective functions
      allocate (ranker(npt1))

      do mm=1,iobj
       omean(mm)=0.
       ologmean(mm)=0.
       ostd(mm)=0.
       ologstd(mm)=0.

       do i=1,npt1
        omin(mm)=min(omin(mm),xobjf(i,mm))
        omax(mm)=max(omax(mm),xobjf(i,mm))
        ranker(i)=xobjf(i,mm)
       end do

       call sorteer2(ranker,npt1)
       newpt1=npt1*0.95
       n5i=npt1*0.05
       n5=max(n5i,1)
       do i=1,newpt1
        ologmean(mm) = ologmean(mm)+log(ranker(i))
        omean(mm) = omean(mm)+ranker(i)
       end do
       o5(mm)=ranker(n5)
       omean(mm) = omean(mm)/newpt1
       ologmean(mm) = ologmean(mm)/newpt1

       std=0.
       logstd=0
       do i=1,newpt1
       ologst = ologst+(log(ranker(i))-ologmean(mm))**2
       std = std+(ranker(i)-omean(mm))**2
       end do
       if (ologst.gt.0.) then
       ologstd(mm)=(ologst/(newpt1-1))**0.5
       else
       ologstd(mm)=0.
       end if
       if (std.gt.0.) then
       ostd(mm)=(std/(newpt1-1))**0.5
       else
       ostd(mm)=0.
       end if
       end do
       write (18015,*) 'statistics on the objective functions'
       write (18015,*) 'mean values'
       write (18015,1813) (omean(mm), mm=1,iobj) 
1813   format(100e10.3)
       write (18015,*) 'standard deviation values'
       write (18015,1813) (ostd(mm), mm=1,iobj) 
       write (18015,*) 'logmean values'
       write (18015,1813) (ologmean(mm), mm=1,iobj) 
       write (18015,*) 'logn standard deviation values'
       write (18015,1813) (ologstd(mm), mm=1,iobj) 
       write (18015,*) 'minimum values'
       write (18015,1813) (omin(mm), mm=1,iobj) 
       write (18015,*) 'maximum values'
       write (18015,1813) (omax(mm), mm=1,iobj) 
       write (18015,*) 'interval 5%'
       write (18015,1813) (o5(mm), mm=1,iobj) 
       write (18015,*) '***********************************************'&
     &'******************************'

      return
      end
c==================================================================
