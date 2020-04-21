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
!!    idplt(:,:,:)|none          |land cover code from crop.dat
!!    igro(:)     |none          |land cover status code:
!!                               |0 no land cover currently growing
!!                               |1 land cover growing
!!    ikill(:,:,:)|julian date   |date of kill operation
!!    ihru        |none          |HRU number
!!    ihv(:,:,:)  |julian date   |date of harvest and kill operation
!!    ihvo(:,:,:) |julian date   |date of harvest operation
!!    iop(:,:,:)  |julian date   |date of tillage operation
!!    iplant(:,:,:)|julian date   |date of planting/beginning of growing
!!                               |season
!!    ncut(:)     |none          |sequence number of harvest operation within
!!                               |current year
!!    nro(:)      |none          |sequence number of year in rotation
!!    ntil(:)     |none          |sequence number of tillage operation within
!!                               |current year
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    phuh(:,:,:) |none          |fraction of plant heat units at which
!!                               |harvest and kill operation occurs
!!    phuho(:,:,:)|none          |fraction of plant heat units at which 
!!                               |harvest operation occurs
!!    phuk(:,:,:) |none          |fraction of plant heat units at which
!!                               |kill operation occurs
!!    phup(:,:,:) |none          |fraction of base zero heat units at which
!!                               |planting occurs
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
      if (igro(j) == 0) then

        !! plant operation
        if (iplant(nro(j),icr(j)+1,j) > 0) then
          if (iida == iplant(nro(j),icr(j)+1,j)) then
            call plantop
	        if (imgt == 1) then
                write (143,1000) j, iyr, i_mo, iida, "     PLANT"
	        endif
          end if
        else if (phup(nro(j),icr(j)+1,j) > 0.) then
          if (phubase(j) > phup(nro(j),icr(j)+1,j))  then
            call plantop
	        if (imgt == 1) then
                write (143,1000) j, iyr, i_mo, iida, "     PLANT"
	        end if
          end if
        end if

      end if


!! operations performed only when land cover is growing
      if (igro(j) == 1) then

        !! check if plant going into or coming out of dormancy
        call dormant

        !! check if end of annual growing season
        if (dayl(j)-dormhr(j) < daylmn(hru_sub(j)) .and.                &
     &                                           phuacc(j) > 0.75)  then
          select case (idc(idplt(nro(j),icr(j),j)))
            case (1, 4, 5)
              call harvkillop
	          if (imgt == 1) then
                  write (143,1000) j, iyr, i_mo, iida, "  END GROW"
	          end if
          end select
        end if

        !! harvest and kill operation
        if (ihv(nro(j),icr(j),j) > 0) then
          if (iida == ihv(nro(j),icr(j),j)) then
            call harvkillop
	        if (imgt == 1) then
                write (143,1000) j, iyr, i_mo, iida, " HARV&KILL"
	        end if
          end if
        else
          if (phuacc(j) > phuh(nro(j),icr(j),j)) then
            call harvkillop
	        if (imgt == 1) then
                write (143,1000) j, iyr, i_mo, iida, " HARV&KILL"
	        end if
          end if
        end if

        !! harvest operation (no kill)
        do 
        if (ihvo(nro(j),ncut(j),j) > 0) then
          if (iida == ihvo(nro(j),ncut(j),j)) then
            if (ihv_gbm(nro(j),ncut(j),j) == 0) then
              call harvestop
	          if (imgt == 1) then
	            write (143,1000) j, iyr, i_mo, iida, "   HARVEST"
	          end if
            else 
              call harvgrainop
            end if 
          else
            exit
          end if
          else
            if (phuacc(j) > phuho(nro(j),ncut(j),j)) then
              if (ihv_gbm(nro(j),ncut(j),j) == 0) then
                call harvestop
	            if (imgt == 1) then
		          write (143,1000) j, iyr, i_mo, iida, "   HARVEST"
	            end if
              else 
                call harvgrainop
              end if 
            else
              exit
            end if
	    end if 
        end do 
	  

        !! kill operation
        if (ikill(nro(j),icr(j),j) > 0) then
          if (iida == ikill(nro(j),icr(j),j)) then
            call killop
	        if (imgt == 1) then
                write (143,1000) j, iyr, i_mo, iida, "      KILL"
	        end if
          end if
        else
          if (phuacc(j) > phuk(nro(j),icr(j),j)) then
            call killop
	        if (imgt == 1) then
                write (143,1000) j, iyr, i_mo, iida, "      KILL"
	        end if
          end if 
        end if
      end if


!! operations performed at any time
      !! multiple tillage operation may be scheduled on same day

      !! scheduling by date
      do while (iida == iop(nro(j),ntil(j),j))
        call newtillmix(j,0.)
	    if (imgt == 1) then
            write (143,1000) j, iyr, i_mo, iida, "   TILLAGE"
	    end if
      end do

      !! scheduling by heat unit
      aphu = 0.
      if (igro(j) == 0) then
        aphu = phubase(j)
        tillphu = phut_nocrop(nro(j),ntil(j),j)
      else
        aphu = phuacc(j)
        tillphu = phut(nro(j),ntil(j),j)
      end if

      do while (tillphu > 0. .and. aphu > tillphu)   
        call newtillmix(j,0.)
	    if (imgt == 1) then
            write (143,1000) j, iyr, i_mo, iida, "   TILLAGE"
	    end if
        if (igro(j) == 0) then
          aphu = phubase(j)
          tillphu = phut_nocrop(nro(j),ntil(j),j)
        else
          aphu = phuacc(j)
          tillphu = phut(nro(j),ntil(j),j)
        end if
      end do
          
 1000 format(4i10,a10)
      return
      end
