      subroutine solp
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of phosphorus lost from the soil
!!    profile in runoff and the movement of soluble phosphorus from the first
!!    to the second layer via percolation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conv_wt(:,:)  |none         |factor which converts kg/kg soil to kg/ha
!!    curyr         |none         |current year of simulation
!!    hru_dafr(:)   |none         |fraction of watershed area located in HRU
!!    ihru          |none         |HRU number
!!    nyskip        |none         |number of years to skip output summarization
!!                                |and printing
!!    phoskd        |none         |Phosphorus soil partitioning coefficient
!!                                |Ratio of phosphorus attached to sediment to
!!                                |phosphorus dissolved in soil water
!!    pperco        |none         |phosphorus percolation coefficient (0-1)
!!                                |0:concentration of soluble P in surface
!!                                |  runoff is zero
!!                                |1:percolate has same concentration of soluble
!!                                |  P as surface runoff
!!    sol_bd(:,:)   |Mg/m**3      |bulk density of the soil
!!    sol_nly(:)    |none         |number of layers in soil profile
!!    sol_prk(:,:)  |mm H2O       |percolation from soil layer on current day
!!    sol_solp(:,:) |kg P/ha      |amount of phosohorus stored in solution
!!    sol_z(:,:)    |mm           |depth to bottom of soil layer
!!    surfq(:)      |mm H2O       |surface runoff generated on day in HRU
!!    wshd_plch     |kg P/ha      |average annual amount of phosphorus leached
!!                                |into second soil layer 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    wshd_plch     |kg P/ha       |average annual amount of phosphorus leached
!!                                 |into second soil layer 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    vap         |kg P/ha       |amount of P leached from soil layer
!!    xx          |none          |variable to hold intermediate calculation
!!                               |result
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real*8 :: xx, vap

      j = 0
      j = ihru
      vap_tile = 0

!! compute soluble P lost in surface runoff
      xx = 0.
      xx = sol_bd(1,j) * sol_z(1,j) * phoskd(j)
      surqsolp(j) = sol_solp(1,j) * surfq(j) / xx 
        !!units ==> surqsolp = [kg/ha * mm] / [t/m^3 * mm * m^3/t] = kg/ha
!     if (surfq(j) > 0.001) then
!     write (17,77) i, iyr, sol_bd(1,j), sol_z(1,j), phoskd(j), surfq(j),  &
!    &              sol_solp(1,j), surqsolp(j)
!     end if
! 77  format(2i6,6f10.3)
      surqsolp(j) = Min(surqsolp(j), sol_solp(1,j))
      surqsolp(j) = Max(surqsolp(j), 0.)
      sol_solp(1,j) = sol_solp(1,j) - surqsolp(j)

!! compute soluble P leaching
      vap = 0.
      vap = sol_solp(1,j) * sol_prk(1,j) / ((conv_wt(1,j) / 1000.)      
     &                                            * pperco_sub(1,j))
      vap = Min(vap, .5 * sol_solp(1,j))
      sol_solp(1,j) = sol_solp(1,j) - vap
      
      !! estimate soluble p in tiles due to crack flow
      if (ldrain(j) > 0) then
        xx = Min(1., sol_crk(j) / 3.0)
        vap_tile = xx * vap
        vap = vap - vap_tile
      end if

      if (qtile < 1.e-6) vap_tile = 0.
      
      if (sol_nly(j) >= 2) then
        sol_solp(2,j) = sol_solp(2,j) + vap
      end if
   
      do ii = 2, sol_nly(j)
        vap = 0.
	 if (ii /= i_sep(j)) then
         vap = sol_solp(ii,j) * sol_prk(ii,j) / ((conv_wt(ii,j) 
     &	   / 1000.) * pperco_sub(ii,j))
	   vap = Min(vap, .2 * sol_solp(ii,j))
	   sol_solp(ii,j) = sol_solp(ii,j) - vap
	   if (ii == sol_nly(j)) then
           sol_solp(ii+1,j) = sol_solp(ii+1,j) + vap
         end if
!         if (ii == ldrain(j)) then
!           vap = sol_solp(ii,j) * qtile / (conv_wt(ii,j) / 1000.
!     *                                         * pperco_sub(ii,j))
!           sol_solp(ii,j) = sol_solp(ii,j) - vap
!           tilep = vap
!         endif
	 endif
	end do
	percp(j) = vap
	tilep(j) = vap_tile
      
      !! summary calculation
      if (curyr > nyskip) then
        wshd_plch = wshd_plch + vap * hru_dafr(j)
        wshd_ptile = wshd_ptile + vap_tile * hru_dafr(j)
      end if

      return
      end