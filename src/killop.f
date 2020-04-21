      subroutine killop

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the kill operation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    curyr        |none          |current year of simulation
!!    hrupest(:)  |none           |pesticide use flag:
!!                                | 0: no pesticides used in HRU
!!                                | 1: pesticides used in HRU
!!    icr(:)       |none          |sequence number of crop grown within the
!!                                |current year
!!    ihru         |none          |HRU number
!!    ncrops(:,:,:)|
!!    npmx        |none           |number of different pesticides used in
!!                                |the simulation
!!    nro(:)       |none          |sequence number for year in rotation
!!    nyskip       |none          |number of years to skip output printing/
!!                                |summarization
!!    plt_pst(:,:)|kg/ha          |pesticide on plant foliage
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bio_ms(:)    |kg/ha         |land cover/crop biomass (dry weight)
!!    idorm(:)     |none          |dormancy status code:
!!                                |0 land cover growing (not dormant)
!!                                |1 land cover dormant
!!    igro(:)      |none          |land cover status code:
!!                                |0 no land cover currently growing
!!                                |1 land cover growing
!!    laiday(:)    |m**2/m**2     |leaf area index
!!    ncrops(:,:,:)|
!!    phuacc(:)    |none          |fraction of plant heat units accumulated
!!    plantn(:)    |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)    |kg P/ha       |amount of phosphorus in plant biomass
!!    plt_pst(:,:) |kg/ha         |pesticide on plant foliage
!!    sol_fon(:,:) |kg N/ha       |amount of nitrogen stored in the fresh
!!                                |organic (residue) pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_pst(:,:,1)|kg/ha        |pesticide in first layer of soil
!!    sol_rsd(:,:) |kg/ha         |amount of organic matter in the soil
!!                                |classified as residue
!!    strsw(:)     |none          |fraction of potential plant growth achieved
!!                                |on the day where the reduction is caused by
!!                                |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    resnew      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
  
      integer :: j, k
      real :: resnew

      j = 0
      j = ihru

!      if (curyr > nyskip) then
!       ncrops(icr(j),j) = ncrops(icr(j),j) + 1
!      endif

	!! 22 January 2008	
	resnew = 0.
	rtresnew = 0.
      resnew = bio_ms(j) * (1. - rwt(j))
	rtresnew = bio_ms(j) * rwt(j)
	call rootfr

	!! update residue, N, P on soil surface
      sol_rsd(1,j) = resnew + sol_rsd(1,j)
      sol_fon(1,j) = plantn(j) * (1. - rwt(j)) + sol_fon(1,j)
      sol_fop(1,j) = plantp(j) * (1. - rwt(j)) + sol_fop(1,j)
      sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
	sol_fon(1,j) = Max(sol_fon(1,j),0.)
	sol_fop(1,j) = Max(sol_fop(1,j),0.)

	!! allocate dead roots, N, P to soil layers
	do l=1, sol_nly(j)
	 sol_rsd(l,j) = sol_rsd(l,j) + rtfr(l) * rtresnew
	 sol_fon(l,j) = sol_fon(l,j) + rtfr(l) * plantn(j) * rwt(j)
	 sol_fop(l,j) = sol_fop(l,j) + rtfr(l) * plantp(j) * rwt(j)
	end do

      if (hrupest(j) == 1) then
        do k = 1, npmx
          sol_pst(k,j,1) = sol_pst(k,j,1) + plt_pst(k,j)
          plt_pst(k,j) = 0.
        end do
      end if

      bio_hv(icr(j),j) = bio_ms(j) + bio_hv(icr(j),j)
      bio_yrms(j) = bio_yrms(j) + bio_ms(j) / 1000.

	!! reset variables
      igro(j) = 0
      idorm(j) = 0
      bio_ms(j) = 0.
      rwt(j) = 0.
	plantn(j) = 0.
      plantp(j) = 0.
      strsw(j) = 1.
      laiday(j) = 0.
      hvstiadj(j) = 0.
      phuacc(j) = 0.
!      phubase(j) = 0.
	rtfr = 0. ! Resetting roots fraction per layer array
	 
      return
      end
