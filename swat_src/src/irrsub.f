      subroutine irrsub

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the irrigation operation when the source is 
!!    the shallow or deep aquifer or a source outside the watershed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aird(:)        |mm H2O        |amount of water applied to HRU on current
!!                                  |day
!!    deepst(:)      |mm H2O        |depth of water in deep aquifer
!!    hru_sub(:)     |none          |subbasin in which HRU is located
!!    ihru           |none          |HRU number
!!    ipot(:)        |none          |number of HRU (in subbasin) that is ponding
!!                                  |water--the HRU that the surface runoff from
!!                                  |current HRU drains into. This variable is
!!                                  |used only for rice paddys or closed
!!                                  |depressional areas
!!    irramt(:)      |mm H2O        |depth of irrigation water applied to
!!                                  |HRU
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
!!    nirr(:)        |none          |sequence number of irrigation application
!!                                  |within the year
!!    pot_vol(:)     |m**3 H2O      |current volume of water stored in the
!!                                  |depression/impounded area
!!    shallst(:)     |mm H2O        |depth of water in shallow aquifer
!!    sol_sumfc(:)   |mm H2O        |amount of water held in the soil profile
!!                                  |at field capacity
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    deepirr(:)  |mm H2O        |amount of water removed from deep aquifer
!!                               |for irrigation
!!    deepst(:)   |mm H2O        |depth of water in deep aquifer
!!    nirr(:)     |none          |sequence number of irrigation application
!!                               |within the year
!!    pot_vol(:)  |m**3 H2O      |current volume of water stored in the
!!                               |depression/impounded area
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
!!    vmmd        |m^3 H2O       |amount of water in deep aquifer
!!    vmms        |m^3 H2O       |amount of water in shallow aquifer
!!    vmxi        |mm H2O        |amount of water specified in irrigation
!!                               |operation
!!    vol         |m^3 H2O       |volume of water to be applied in irrigation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min
!!    SWAT: irrigate

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, k, flag
      real :: vmma, vmm, cnv, vmxi, vol, vmms, vmmd

      j = 0
      j = ihru

!! determine available amount of water in source
!! ie upper limit on water removal on day
      vmma = 0.
      vmms = 0.
      vmmd = 0.
      vmm = 0.
      irrsc(j) = irr_sc(j)
      irrno(j) = irr_no(j)

      select case (irrsc(j))
        case (3)   !! shallow aquifer source
          do k = 1, nhru
            if (hru_sub(k) == irrno(j)) then
              cnv = 0.
              cnv = hru_ha(k) * 10.
	        if (shallst(k) < 1.e-6) shallst(k) = 0.0
              vmma = vmma + shallst(k) * cnv * irrefm(j)
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
              vmma = vmma + deepst(k) * cnv * irrefm(j)
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
        cnv = 0.
        cnv = hru_ha(j) * 10.

        vmxi = 0.
        vmxi = irramt(j)
        if (vmxi < 1.e-6) vmxi = sol_sumfc(j)
        if (vmm > vmxi) vmm = vmxi

        vol = 0.
        vol = vmm * cnv

        if (pot_fr(j) > 1.e-6) then
          pot_vol(j) = pot_vol(j) + vol / (10. * potsa(j))
          aird(j) = vmm                 !!added rice irrigation 11/10/11
        else
!! get correct SQ_RTO is this manual or auto
          sq_rto = irrsq(j) 
            if (auto_wstr(j) > 0.)  then
				sq_rto = irr_asq(j)
		  end if
        call irrigate(j,vmm)
        end if

        !! subtract irrigation from shallow or deep aquifer
        if (pot_fr(j) > 1.e-6) then
          vol = 0.
          vol = aird(j) * cnv * irrefm(j)
        end if
        select case (irrsc(j))
          case (3)   !! shallow aquifer source
            do k = 1, nhru
              if (hru_sub(k) == irrno(j)) then
                cnv = 0.
                vmma = 0.
                cnv = hru_ha(k) * 10.
                if (vmms > 0.01) vmma = vol * (shallst(k) * cnv / vmms)
                vmma = vmma / cnv
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
                if (vmmd > 0.01) vmma = vol * (deepst(k) * cnv / vmmd)
                vmma = vmma / cnv
                deepst(k) = deepst(k) - vmma
                if (deepst(k) < 0.) then
                 vmma = vmma + deepst(k)
                 deepst(k) = 0.
                end if
                deepirr(k) = deepirr(k) + vmma
              end if
            end do
        end select

        !! advance irrigate operation number
        nirr(j) = nirr(j) + 1

      endif

      return
      end