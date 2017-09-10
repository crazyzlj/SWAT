      subroutine operatn
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs all management operations             

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dayl(:)     |hours         |day length for current day
!!    daylmn(:)   |hours         |shortest daylength occurring during the
!!                               |year
!!    dormhr(:)   |hours         |time threshold used to define dormant
!!                               |period for plant (when daylength is within
!!                               |the time specified by dormhr from the minimum
!!                               |daylength for the area, the plant will go
!!                               |dormant)
!!    phubase(:)  |heat units    |base zero total heat units (used when no
!!                               |land cover is growing
!!    icr(:)      |none          |sequence number of crop grown within a year
!!    iida        |julian date   |day being simulated (current julian date)
!!    idc(:)      |none          |crop/landcover category:
!!                               |1 warm season annual legume
!!                               |2 cold season annual legume
!!                               |3 perennial legume
!!                               |4 warm season annual
!!                               |5 cold season annual
!!                               |6 perennial
!!                               |7 trees
!!    idplt(:)    |none          |land cover code from crop.dat
!!    igro(:)     |none          |land cover status code:
!!                               |0 no land cover currently growing
!!                               |1 land cover growing
!!    ihru        |none          |HRU number
!!    iop(:,:,:)  |julian date   |date of tillage operation
!!    ncut(:)     |none          |sequence number of harvest operation within
!!                               |current year
!!    nro(:)      |none          |sequence number of year in rotation
!!    ntil(:)     |none          |sequence number of tillage operation within
!!                               |current year
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    phut(:,:,:) |none          |fraction of heat units (base zero or plant)
!!                               |at which tillage occurs
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aphu        |heat units    |fraction of total heat units accumulated
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: plantop, dormant, harvkillop, harvestop, killop, tillmix

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
   
      integer :: j
      real :: aphu, tillphu

      j = 0
      j = ihru


!! operations performed only when no land cover growing
        
        do while(idop(nop(j),j) > 0 .and. iida == idop(nop(j),j))
          call sched_mgt
          if (mgtop(nop(j),j) == 17) then
	      call sched_mgt
	    end if
          if (yr_skip(j) == 1) exit
        end do
        
        if (igro(j) == 0) then
          aphu = phubase(j)
        else
          aphu = phuacc(j)
        end if 
        if (dorm_flag == 1) aphu = 999.
        do while (phu_op(nop(j),j) > 0. .and. aphu > phu_op(nop(j),j))
          call sched_mgt
          if (igro(j) == 0) then
            aphu = phubase(j)
          else
            aphu = phuacc(j)
          end if 
          if (dorm_flag == 1) aphu = 999.
          if (mgtop(nop(j),j) == 17) then
	      call sched_mgt
          end if
          if (yr_skip(j) == 1) exit
        end do
         
      return
1000  format (4i10,a10)
      end