      subroutine hrupond
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine routes water and sediment through ponds in the HRUs

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bp1(:)      |none          |1st shape parameter for pond surface area
!!                               |equation
!!    bp2(:)      |none          |2nd shape parameter for the pond surface area
!!                               |equation
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    ihru        |none          |HRU number
!!    latno3(:)   |kg N/ha       |amount of NO3-N in lateral flow in HRU for
!!                               |the day
!!    minpgw(:)   |kg P/ha       |soluble P loading to reach in groundwater
!!    no3gw(:)    |kg N/ha       |nitrate loading to reach in groundwater
!!    pnd_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into ponds
!!    pnd_no3(:)  |kg N          |amount of nitrate originating from surface
!!                               |runoff in pond at beginning of day
!!    pnd_no3g(:) |kg N          |amount of nitrate originating from
!!                               |groundwater in pond at beginning of day
!!    pnd_no3s(:) |kg N          |amount of nitrate originating from lateral
!!                               |flow in pond at beginning of day
!!    pnd_orgn(:) |kg N          |amount of organic N originating from
!!                               |surface runoff in pond at beginning of day
!!    pnd_orgp(:) |kg P          |amount of organic P originating from
!!                               |surface runoff in pond at beginning of day
!!    pnd_psed(:) |kg P          |amount of mineral P attached to sediment
!!                               |originating from surface runoff in pond at
!!                               |beginning of day
!!    pnd_solp(:) |kg P          |amount of soluble P originating from surface
!!                               |runoff in pond at beginning of day
!!    pnd_solpg(:)|kg P          |amount of soluble P originating from
!!                               |groundwater in pond at beginning of day
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond
!!    pndflwo     |m^3 H2O       |volume of water flowing out of pond on day
!!    pndsedo     |metric tons   |sediment leaving pond during day
!!    pndsep      |m^3 H2O       |seepage from pond on day
!!    qdr(:)      |mm H2O        |net water loading from HRU to main channel
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    sedorgp(:)  |kg P/ha       |amount of organic phosphorus in surface runoff
!!                               |in HRU for the day
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    surqsolp(:) |kg P/ha       |amount of soluble phosphorus in surface runoff
!!                               |in HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latno3(:)   |kg N/ha       |amount of NO3-N in lateral flow in HRU for the
!!                               |day
!!    minpgw(:)   |kg P/ha       |soluble P loading to reach in groundwater
!!    no3gw(:)    |kg N/ha       |nitrate loading to reach in groundwater
!!    pndflwi     |m^3 H2O       |volume of water flowing into pond on day
!!    pndsedin    |metric tons   |sediment entering pond during day
!!    qdr(:)      |mm H2O        |net water loading from HRU to main channel
!!    sedminpa(:) |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps(:) |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn(:)  |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    sedorgp(:)  |kg P/ha       |amount of organic phosphorus in surface runoff
!!                               |in HRU for the day
!!    sedyld(:)   |metric tons   |daily soil loss caused by water erosion in HRU
!!    shallst(:)  |mm H2O        |depth of water in shallow aquifer
!!    surqno3(:)  |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    surqsolp(:) |kg P/ha       |amount of soluble phosphorus in surface runoff
!!                               |in HRU for the day
!!    twlpnd      |mm H2O        |water lost through seepage from ponds on
!!                               |day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!!    j           |none          |HRU number
!!    pndsa       |ha            |surface area of pond on current day
!!    xx          |none          |fraction of HRU not draining into ponds
!!    yy          |none          |fraction of water leaving pond on day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: pond

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real :: cnv, pndsa, xx, yy, qdayi, latqi

      j = 0
      j = ihru

      if (pnd_fr(j) > 0.01) then
        cnv = 0.
        cnv = hru_ha(j) * 10.

        !! calculate area of HRU covered by pond
        pndsa = 0.
        pndsa = hru_fr(j) * bp1(j) * pnd_vol(j) ** bp2(j)

        !! calculate water flowing into pond for day
        pndflwi = qday + latq(j)
        pndflwi = pndflwi * 10. * hru_ha(j) * pnd_fr(j)
        qdayi = qday
        latqi = latq(j)
        qday = qday * (1. - pnd_fr(j))
        latq(j) = latq(j) * (1. - pnd_fr(j))
        pndloss = qdayi - qday
        lpndloss = latqi - latq(j)
        qdr(j) = qdr(j) - pndloss - lpndloss
!       qdr(j) = qdr(j) - qdr(j) * pnd_fr(j)

        !! calculate sediment loading to pond for day
        pndsedin = sedyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
        pndsanin = sanyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
        pndsilin = silyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
        pndclain = clayld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
        pndsagin = sagyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))
        pndlagin = lagyld(j) * (pnd_fr(j) - pndsa / hru_ha(j))

        sedyld(j) = sedyld(j) - sedyld(j) * pnd_fr(j)
        sanyld(j) = sanyld(j) - sanyld(j) * pnd_fr(j)
        silyld(j) = silyld(j) - silyld(j) * pnd_fr(j)
        clayld(j) = clayld(j) - clayld(j) * pnd_fr(j)
        sagyld(j) = sagyld(j) - sagyld(j) * pnd_fr(j)
        lagyld(j) = lagyld(j) - lagyld(j) * pnd_fr(j)

        !! compute nitrogen and phosphorus levels in pond at beginning
        !! of day: equation 29.1.1 in SWAT manual
        if (pnd_solp(j) < 1.e-6) pnd_solp(j) = 0.0
        if (pnd_psed(j) < 1.e-6) pnd_psed(j) = 0.0
        if (pnd_orgp(j) < 1.e-6) pnd_orgp(j) = 0.0
        if (pnd_solpg(j) < 1.e-6) pnd_solpg(j) = 0.0
        if (pnd_orgn(j) < 1.e-6) pnd_orgn(j) = 0.0
        if (pnd_no3(j) < 1.e-6) pnd_no3(j) = 0.0
        if (pnd_no3s(j) < 1.e-6) pnd_no3s(j) = 0.0
        if (pnd_no3g(j) < 1.e-6) pnd_no3g(j) = 0.0
        xx = 0.
        xx = pnd_fr(j) * hru_ha(j)
        pnd_solp(j) = pnd_solp(j) + (surqsolp(j) + sedminpa(j)) * xx
        pnd_psed(j) = pnd_psed(j) + sedminps(j) * xx
        pnd_orgp(j) = pnd_orgp(j) + sedorgp(j) * xx
        pnd_solpg(j) = pnd_solpg(j) + minpgw(j) * xx
        pnd_orgn(j) = pnd_orgn(j) + sedorgn(j) * xx
        pnd_no3(j) = pnd_no3(j) + surqno3(j) * xx
        pnd_no3s(j) = pnd_no3s(j) + latno3(j) * xx
        pnd_no3g(j) = pnd_no3g(j) + no3gw(j) * xx

        !! compute amount of nutrients not passing through ponds
        xx = 0.
        xx = 1. - pnd_fr(j)
        sedorgn(j) = sedorgn(j) * xx
        surqno3(j) = surqno3(j) * xx
        latno3(j) = latno3(j) * xx
        no3gw(j) = no3gw(j) * xx
        sedorgp(j) = sedorgp(j) * xx
        sedminpa(j) = sedminpa(j) * xx
        sedminps(j) = sedminps(j) * xx
        surqsolp(j) = surqsolp(j) * xx
        minpgw(j) = minpgw(j) * xx

        call pond(j)

        !! compute water leaving pond
!        qday = qday + pndflwo / cnv
        qdr(j) = qdr(j) + pndflwo / cnv

        !! compute sediment leaving pond
        sedyld(j) = sedyld(j) + pndsedo
        sanyld(j) = sanyld(j) + pndsano
        silyld(j) = silyld(j) + pndsilo
        clayld(j) = clayld(j) + pndclao
        sagyld(j) = sagyld(j) + pndsago
        lagyld(j) = lagyld(j) + pndlago

        !! compute nutrients leaving pond
        if (pndflwo > 1.e-5) then
          yy = 0.
          yy = pndflwo / (pnd_vol(j) + pndflwo)
          sedorgn(j) = sedorgn(j) + pnd_orgn(j) * yy / hru_ha(j)
          surqno3(j) = surqno3(j) + pnd_no3(j) * yy / hru_ha(j)
          latno3(j) = latno3(j) + pnd_no3s(j) * yy / hru_ha(j)
          no3gw(j) = no3gw(j) + pnd_no3g(j) * yy / hru_ha(j)
          sedorgp(j) = sedorgp(j) + pnd_orgp(j) * yy / hru_ha(j)
          sedminps(j) = sedminps(j) + pnd_psed(j) * yy / hru_ha(j)
          surqsolp(j) = surqsolp(j) + pnd_solp(j) * yy / hru_ha(j)
          minpgw(j) = minpgw(j) + pnd_solpg(j) * yy / hru_ha(j)

          !! adjust nutrient content in pond
          pnd_orgn(j) = pnd_orgn(j) * (1. - yy)
          pnd_no3(j) = pnd_no3(j) * (1. - yy)
          pnd_no3g(j) = pnd_no3g(j) * (1. - yy)
          pnd_no3s(j) = pnd_no3s(j) * (1. - yy)
          pnd_orgp(j) = pnd_orgp(j) * (1. - yy)
          pnd_psed(j) = pnd_psed(j) * (1. - yy)
          pnd_solp(j) = pnd_solp(j) * (1. - yy)
          pnd_solpg(j) = pnd_solpg(j) * (1. - yy)
          pnd_chla(j) = pnd_chla(j) * (1. - yy)
        end if

        
        !! add pond seepage to shallow aquifer convert from m^3 to mm
        shallst(j) = shallst(j) + pndsep / cnv

        !! compute seepage depth for HRU water balance
        twlpnd = pndsep / cnv

      end if

      if (qdr(j) < 0.) qdr(j) = 0.

      if (sedyld(j) < 0.) then
	    sedyld(j) = 0.0
        sanyld(j) = 0.0
        silyld(j) = 0.0
        clayld(j) = 0.0
        sagyld(j) = 0.0
        lagyld(j) = 0.0
	  end if

      return
      end