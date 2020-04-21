      subroutine autoirr

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the auto-irrigation operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)        |mm H2O        |amount of water applied to HRU on current
!!                                  |day
!!    auto_wstr(:)   |none or mm    |water stress threshold that triggers irrigation
!!    deepst(:)      |mm H2O        |depth of water in deep aquifer
!!    hru_sub(:)     |none          |subbasin in which HRU is located
!!    wstrs_id(:)    |none          |water stress identifier:
!!                                  |1 plant water demand
!!                                  |2 soil water deficit
!!    ihru           |none          |HRU number
!!    irrno(:)       |none          |irrigation source location
!!                                  |if IRR=1, IRRNO is the number of the
!!                                  |          reach
!!                                  |if IRR=2, IRRNO is the number of the
!!                                  |          reservoir
!!                                  |if IRR=3, IRRNO is the number of the
!!                                  |          subbasin
!!                                  |if IRR=4, IRRNO is the number of the
!!                                  |          subbasin
!!                                  |if IRR=5, not used
!!    irrsc(:)       |none          |irrigation source code:
!!                                  |1 divert water from reach
!!                                  |2 divert water from reservoir
!!                                  |3 divert water from shallow aquifer
!!                                  |4 divert water from deep aquifer
!!                                  |5 divert water from source outside
!!                                  |  watershed
!!    nhru           |none          |number of HRUs in watershed
!!    shallst(:)     |mm H2O        |depth of water in shallow aquifer
!!    sol_sumfc(:)   |mm H2O        |amount of water held in the soil profile
!!                                  |at field capacity
!!    sol_sw(:)      |mm H2O        |amount of water stored in soil profile on any
!!                                  |given day
!!    strsw(:)       |none          |fraction of potential plant growth achieved
!!                                  |on the day where the reduction is caused by
!!                                  |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepirr(:)  |mm H2O        |amount of water removed from deep aquifer
!!                               |for irrigation
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    shallirr(:) |mm H2O        |amount of water removed from shallow aquifer
!!                               |for irrigation
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    vmm         |mm H2O        |maximum amount of water to be applied
!!    vmma        |mm H2O        |amount of water in source
!!    vmmd        |m^3 H2O       |total amount of water in subbasin deep
!!                               |aquifer
!!    vmms        |m^3 H2O       |total amount of water in subbasin shallow 
!!                               |aquifer
!!    vol         |mm H2O        |volume of water applied to HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: irrigate

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, k
      real :: vmma, vmm, cnv, vol, vmms, vmmd

      j = 0
      j = ihru
  
!!!! Srin's irrigation source by each application changes
      irrsc(j) = irr_sca(j)
      irrno(j) = irr_noa(j)
!!!! Srin's irrigation source by each application changes

      if ((wstrs_id(j) == 1 .and. strsw(j) < auto_wstr(j) .or.          &
     & (wstrs_id(j)==2.and.sol_sumfc(j)-sol_sw(j)>auto_wstr(j)))) then  &
        !! determine available amount of water in source
        !! ie upper limit on water removal on day
        vmma = 0.
        vmms = 0.
        vmmd = 0.
        vmm = 0.
        select case (irrsc(j))
          case (3)   !! shallow aquifer source
            do k = 1, nhru
              if (hru_sub(k) == irrno(j)) then
                cnv = 0.
                cnv = hru_ha(k) * 10.
                vmma = vmma + shallst(k) * cnv
              end if
            end do
            vmms = vmma
            cnv = 0.
            cnv = hru_ha(j) * 10.
            vmma = vmma / cnv
            vmm = Min(sol_sumfc(j), vmma)

          case (4)   !! deep aquifer source
            do k = 1, nhru
              if (hru_sub(k) == irrno(j)) then
                cnv = 0.
                cnv = hru_ha(k) * 10.
                vmma = vmma + deepst(k) * cnv
              end if
            end do
            vmmd = vmma
            cnv = 0.
            cnv = hru_ha(j) * 10.
            vmma = vmma / cnv
            vmm = Min(sol_sumfc(j), vmma)

          case (5)   !! unlimited source
            vmm = sol_sumfc(j)
        end select


      !! if water available from source, proceed with irrigation
        if (vmm > 0.) then
          vmm = Min(vmm,irr_mx(j))
          sq_rto = irr_asq(j)
          call irrigate(j,vmm)

        !! subtract irrigation from shallow or deep aquifer
          vol = 0.
          cnv = 0.
          cnv = hru_ha(j) * 10.
          vol = aird(j) * cnv
          select case (irrsc(j))
            case (3)   !! shallow aquifer source
              do k = 1, nhru
                if (hru_sub(k) == irrno(j)) then
                  cnv = 0.
                  vmma = 0.
                  cnv = hru_ha(k) * 10.
                  if (vmms > 1.e-4) then
                    vmma = vol * (shallst(k) * cnv / vmms)
                  end if
                  vmma = vmma / cnv
                  vmma = vmma / irr_eff(k)
                  shallst(k) = shallst(k) - vmma
                  if (shallst(k) < 0.) then
                    vmma = vmma + shallst(k)
                    shallst(k) = 0.
                  end if
                  shallirr(k) = shallirr(k) + vmma
                end if
              end do

            case (4)   !! deep aquifer source
              do k = 1, nhru
                if (hru_sub(k) == irrno(j)) then
                  cnv = 0.
                  vmma = 0.
                  cnv = hru_ha(k) * 10.
                  if (vmmd>1.e-4) vmma = vol * (deepst(k) * cnv / vmmd)
                  vmma = vmma / cnv
                  vmma = vmma / irr_eff(k)
                  deepst(k) = deepst(k) - vmma
                  if (deepst(k) < 0.) then
                    vmma = vmma + deepst(k)
                    deepst(k) = 0.
                  end if
                  deepirr(k) = deepirr(k) + vmma
                end if
              end do
            end select

          if (imgt == 1) then
            write (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, 
     *       "         ",  " AUTOIRR", phubase(j), phuacc(j)
     *       , sol_sw(j),bio_ms(j), sol_rsd(1,j),sol_sumno3(j)
     *         ,sol_sumsolp(j), aird(j)
     *          ,irrsc(j), irrno(j)
          end if
       
        endif
      end if 
        
!! changed format below     
!!1000  format (a5,1x,a7,3i6,2a15,7f10.2,10x,f10.2,70x,f10.2) 
1000  format (a5,1x,a4,3i6,2a15,7f10.2,10x,f10.2,70x,i10,10x,i10) 

      return
      end
