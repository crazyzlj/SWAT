      subroutine psed(iwave)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the amount of organic and mineral phosphorus
!!    attached to sediment in surface runoff

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha         |ha           |area of watershed in hectares
!!    enratio       |none         |enrichment ratio calculated for day in HRU
!!    erorgp(:)     |none         |organic P enrichment ratio, if left blank
!!                                |the model will calculate for every event
!!    hru_dafr(:)   |none         |fraction of watershed area in HRU
!!    ihru          |none         |HRU number
!!    inum1         |none         |subbasin number
!!    iwave         |none         |flag to differentiate calculation of HRU and
!!                                |subbasin sediment calculation
!!                                |iwave = 0 for HRU
!!                                |iwave = subbasin # for subbasin
!!    sedyld(:)     |metric tons  |daily soil loss caused by water erosion in
!!                                |HRU
!!    sol_actp(:,:) |kg P/ha      |amount of phosphorus stored in the
!!                                |active mineral phosphorus pool
!!    sol_bd(:,:)   |Mg/m**3      |bulk density of the soil
!!    sol_fop(:,:)  |kg P/ha      |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_orgp(:,:) |kg P/ha      |amount of phosphorus stored in the organic
!!                                |P pool
!!    sol_stap(:,:)|kg P/ha       |amount of phosphorus in the soil layer
!!                                |stored in the stable mineral phosphorus pool
!!    sol_z(:,:)    |mm           |depth to bottom of soil layer
!!    sub_fr(:)     |none         |fraction of watershed area in subbasin
!!    sub_orgp(:)   |kg P/ha      |amount of phosphorus stored in all organic
!!                                |pools
!!    sub_minpa(:)  |kg P/ha      |amount of phosphorus stored in active mineral
!!                                |pools sorbed to sediment
!!    sub_minps(:)  |kg P/ha      |amount of phosphorus stored in stable mineral
!!                                |pools sorbed to sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedminpa(:)  |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedminps(:)  |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                                |sediment in surface runoff in HRU for day
!!    sedorgp(:)   |kg P/ha       |amount of organic phosphorus in surface
!!                                |runoff in HRU for the day
!!    sol_actp(:,:)|kg P/ha       |amount of phosphorus stored in the
!!                                |active mineral phosphorus pool
!!    sol_fop(:,:) |kg P/ha       |amount of phosphorus stored in the fresh
!!                                |organic (residue) pool
!!    sol_orgp(:,:)|kg P/ha       |amount of phosphorus stored in the organic
!!                                |P pool
!!    sol_stap(:,:)|kg P/ha       |amount of phosphorus in the soil layer
!!                                |stored in the stable mineral phosphorus pool
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    conc        |              |concentration of P in soil
!!    er          |none          |enrichment ratio
!!    j           |none          |HRU number
!!    porgg       |kg P/ha       |total amount of P in organic pools prior to
!!                               |sediment removal
!!    psedd       |kg P/ha       |total amount of P in mineral sediment pools
!!                               |prior to sediment removal
!!    sedp        |kg P/ha       |total amount of P removed in sediment erosion
!!    sb          |none          |subbasin number
!!    wt1         |none          |conversion factor (mg/kg => kg/ha)
!!    xx          |kg P/ha       |amount of phosphorus attached to sediment 
!!                               |in soil
!!    xxa         |kg P/ha       |fraction of active mineral phosphorus in soil
!!    xxo         |kg P/ha       |fraction of organic phosphorus in soil
!!    xxs         |kg P/ha       |fraction of stable mineral phosphorus in soil
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: iwave
      integer :: j, sb
      real*8 :: xx, wt1, er, conc, xxo, sedp, psedd, porgg, xxa, xxs

      j = 0
      j = ihru

      sb = 0
      sb = inum1

      xx = 0.
      xxo = 0.
      xxa = 0.
      xxs = 0.
      if (iwave <= 0) then
        !! HRU sediment calculations
        xx = sol_orgp(1,j) + sol_fop(1,j) + sol_mp(1,j) +   
     &                                    sol_actp(1,j) + sol_stap(1,j)
        if (xx > 1.e-3) then
           xxo = (sol_orgp(1,j) + sol_fop(1,j)+ sol_mp(1,j)) / xx
           xxa = sol_actp(1,j) / xx
           xxs = sol_stap(1,j) / xx
        end if
        !! sum for subbasin sediment calculations
        sub_orgp(sb) = sub_orgp(sb) + (sol_orgp(1,j) + sol_fop(1,j)
     &      + sol_mp(1,j)) * hru_dafr(j)
        sub_minpa(sb) = sub_minpa(sb) + sol_actp(1,j) * hru_fr(j)
        sub_minps(sb) = sub_minps(sb) + sol_stap(1,j) * hru_fr(j)
      else
        !! subbasin sediment calculations
        xx = sub_orgp(iwave) + sub_minpa(iwave) + sub_minps(iwave)
        if (xx > 1.e-3) then
          ! notice no soil_fop or soil_mp calculations here Armen March 2009
          xxo = sub_orgp(iwave) / xx
          xxa = sub_minpa(iwave) / xx
          xxs = sub_minps(iwave) / xx
        end if
      end if

      wt1 = 0.
      if (iwave <= 0) then
        !! HRU sediment calculations
        wt1 = sol_bd(1,j) * sol_z(1,j) / 100.
      else
        !! subbasin sediment calculations
        wt1 = sub_bd(iwave) * sol_z(1,j) / 100.
      end if

      er = 0.
      if (iwave <= 0) then
        !! HRU sediment calculations
        if (erorgp(j) > .001) then
         er = erorgp(j)
        else
         er = enratio
        end if
      else
        !! subbasin sediment calculations
        er = enratio
      end if

      conc = 0.
      conc = xx * er / wt1

      sedp = 0.
      if (iwave <= 0) then
        !! HRU sediment calculations
        sedp = .001 * conc * sedyld(j) / hru_ha(j)
        sedorgp(j) = sedp * xxo
        sedminpa(j) = sedp * xxa
        sedminps(j) = sedp * xxs
      else
        !! subbasin sediment calculations
        sedp = .001 * conc * sedyld(j) / (da_ha * sub_fr(iwave))
        sedorgp(j) = sedp * xxo
        sedminpa(j) = sedp * xxa
        sedminps(j) = sedp * xxs
      end if


!! modify phosphorus pools only for HRU calculations
      if (iwave <= 0) then
        psedd = 0.
        porgg = 0.
        psedd = sol_actp(1,j) + sol_stap(1,j)
        porgg = sol_orgp(1,j) + sol_fop(1,j)
        if (porgg > 1.e-3) then
        sol_orgp(1,j) = sol_orgp(1,j) - sedorgp(j) * (sol_orgp(1,j) /
     &                                                            porgg)
        sol_fop(1,j) = sol_fop(1,j) - sedorgp(j) * (sol_fop(1,j) /
     &                                                            porgg)
        sol_mp(1,j) = sol_mp(1,j) - sedorgp(j) * (sol_mp(1,j) /
     &                                                            porgg)
        end if
        sol_actp(1,j) = sol_actp(1,j) - sedminpa(j)
        sol_stap(1,j) = sol_stap(1,j) - sedminps(j)

        !! Not sure how can this happen but I reapeated 
        !! the check for sol_mp(1,j) - Armen March 2009
        if (sol_orgp(1,j) < 0.) then
          sedorgp(j) = sedorgp(j) + sol_orgp(1,j)
          sol_orgp(1,j) = 0.
        end if

        if (sol_fop(1,j) < 0.) then
          sedorgp(j) = sedorgp(j) + sol_fop(1,j)
          sol_fop(1,j) = 0.
        end if

        if (sol_mp(1,j) < 0.) then
          sedorgp(j) = sedorgp(j) + sol_mp(1,j)
          sol_mp(1,j) = 0.
        end if

        if (sol_actp(1,j) < 0.) then
          sedminpa(j) = sedminpa(j) + sol_actp(1,j)
          sol_actp(1,j) = 0.
        end if

        if (sol_stap(1,j) < 0.) then
          sedminps(j) = sedminps(j) + sol_stap(1,j)
          sol_stap(1,j) = 0.
        end if
      end if

      return
      end