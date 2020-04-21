       subroutine rtover
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes overland flow

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha        |ha           |area of watershed in hectares
!!    hru_sub(:)   |none         |subbasin in which HRU/reach is located
!!    inum1        |none         |reach number
!!    nhru         |none         |number of HRUs in watershed
!!    rnum1        |none         |fraction of overland flow
!!    sol_nh3(1,:) |kg N/ha      |amount of nitrogen stored in the ammonium
!!                               |pool in first soil layer
!!    sol_no3(1,:) |kg N/ha      |amount of nitrogen stored in the
!!                               |nitrate pool of first soil layer
!!    sol_orgn(1,:)|kg N/ha      |amount of nitrogen stored in the stable
!!                               |organic N pool of first soil layer
!!    sol_orgp(1,:)|kg P/ha      |amount of phosphorus stored in the organic
!!                               |P pool of first soil layer
!!    sol_solp(1,:)|kg P/ha      |amount of phosohorus stored in solution
!!                               |of first soil layer
!!    sub_fr(:)    |none         |fraction of watershed area in HRU
!!    varoute(2,:) |m^3 H2O      |water
!!    varoute(3,:) |metric tons  |sediment or suspended solid load
!!    varoute(4,:) |kg N         |organic nitrogen
!!    varoute(5,:) |kg P         |organic phosphorus
!!    varoute(6,:) |kg N         |nitrate
!!    varoute(7,:) |kg P         |soluble phosphorus
!!    varoute(11,:)|mg pst       |pesticide in solution
!!    varoute(12,:)|mg pst       |pesticide sorbed to sediment
!!    varoute(13,:)|kg           |chlorophyll-a
!!    varoute(14,:)|kg N         |ammonium
!!    varoute(15,:)|kg N         |nitrite
!!    varoute(16,:)|kg           |carbonaceous biological oxygen demand
!!    varoute(17,:)|kg           |dissolved oxygen
!!    varoute(18,:)|# cfu/100ml  |persistent bacteria
!!    varoute(19,:)|# cfu/100ml  |less persistent bacteria
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ovrlnd(:)    |mm H2O       |amount of overland flow onto HRU on day
!!    sol_nh3(1,:) |kg N/ha      |amount of nitrogen stored in the ammonium
!!                               |pool in first soil layer
!!    sol_no3(1,:) |kg N/ha      |amount of nitrogen stored in the
!!                               |nitrate pool of first soil layer
!!    sol_orgn(1,:)|kg N/ha      |amount of nitrogen stored in the stable
!!                               |organic N pool of first soil layer
!!    sol_orgp(1,:)|kg P/ha      |amount of phosphorus stored in the organic
!!                               |P pool of first soil layer
!!    sol_solp(1,:)|kg P/ha      |amount of phosohorus stored in solution
!!                               |of first soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    jrch        |none          |reach number
!!    k           |none          |counter
!!    nh3         |kg N/ha       |ammonia in overland flow
!!    no3         |kg N/ha       |nitrate in overland flow
!!    orgn        |kg N/ha       |organic nitrogen in overland flow
!!    orgp        |kg P/ha       |organic phosphorus in overland flow
!!    slbp        |kg P/ha       |soluble phosphorus in overland flow
!!    sub_ha      |ha            |area of subbasin in hectares
!!    wtrmm       |mm H2O        |depth of water in overland flow
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jrch, k
      real :: sub_ha, wtrmm, orgn, orgp, no3, slbp, nh3

      jrch = 0
      jrch = inum1

!! add water and chemicals from overland flow to 1st layer of soil
      if (rnum1 > 1.e-4) then
        sub_ha = 0.
        sub_ha = da_ha * sub_fr(jrch)

        wtrmm = 0.
        orgn = 0.
        orgp = 0.
        no3 = 0.
        slbp = 0.
        nh3 = 0.
        !! water
        wtrmm = varoute(2,inum2) * rnum1 / (sub_ha * 10.)
        !! sediment not routed
        !! organic nitrogen
        orgn = varoute(4,inum2) * rnum1 / sub_ha
        !! organic phosphorus
        orgp = varoute(5,inum2) * rnum1 / sub_ha
        !! nitrate (& nitrite)
        no3 = (varoute(6,inum2) + varoute(15,inum2)) * rnum1 / sub_ha
        !! soluble phosphorus
        slbp = varoute(7,inum2) * rnum1 / sub_ha
        !! soluble pesticide not routed
        !! sorbed pesticide not routed
        !! chlorophyll-a not routed
        !! ammonium
        nh3 = varoute(14,inum2) * rnum1 / sub_ha
        !! CBOD not routed
        !! dissolved oxygen not routed
        !! persistent bacteria not routed
        !! less persistent bacteria not routed
        if (ievent>2) then
          do k = 1, nhru; do ii=1,nstep
            ovrlnd_dt(k,ii) = hhvaroute(2,inum2,ii) * rnum1 
     &        / (sub_ha * 10.)
          end do; end do
        endif

        do k = 1, nhru
          if (hru_sub(k) == jrch) then
            ovrlnd(k) = wtrmm
           if (cswat == 0) then
			sol_orgn(1,k) = sol_orgn(1,k) + orgn
	     else
			sol_n(1,k) = sol_n(1,k) + orgn
	     end if
            sol_orgp(1,k) = sol_orgp(1,k) + orgp
            sol_no3(1,k) = sol_no3(1,k) + no3
            sol_solp(1,k) = sol_solp(1,k) + slbp
            sol_nh3(1,k) = sol_nh3(1,k) + nh3
          end if
        end do
      end if

      return
      end
