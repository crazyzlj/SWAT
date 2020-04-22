      subroutine finalbal
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates final water balance for watershed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    aairr(:)    |mm H2O        |average annual amount of irrigation water
!!                               |applied to HRU
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    irrsc(:)    |none          |irrigation source code:
!!                               |1 divert water from reach
!!                               |2 divert water from reservoir
!!                               |3 divert water from shallow aquifer
!!                               |4 divert water from deep aquifer
!!                               |5 divert water from source outside
!!                               |  watershed
!!    nhru        |none          |number of HRUs in watershed
!!    nres        |none          |number of reservoirs in watershed
!!    pnd_sed(:)  |kg/L          |ratio of sediment to water in pond
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond
!!    res_sed(:)  |kg/L (ton/m^3)|amount of sediment in reservoir
!!    res_vol(:)  |m^3 H2O       |reservoir volume
!!    resouta(3,:)|metric tons   |sediment entering reservoir during simulation
!!    resouta(4,:)|metric tons   |sediment leaving reservoir during simulation
!!    resouta(17,:)|m^3 H2O       |evaporation from reservoir during simulation
!!    resouta(18,:)|m^3 H2O       |seepage from reservoir during simulation
!!    resouta(19,:)|m^3 H2O       |precipitation on reservoir during simulation
!!    resouta(20,:)|m^3 H2O       |water entering reservoir during simulation
!!    resouta(21,:)|m^3 H2O       |water leaving reservoir during simulation
!!    sno_hru(:)  |mm H2O        |amount of water stored as snow
!!    wshd_pndha  |ha            |watershed area in hectares which drains into
!!                               |ponds
!!    wshd_pndsed |metric tons   |total amount of suspended sediment in ponds
!!                               |in the watershed
!!    wshd_pndv   |m^3           |total volume of water in ponds in the 
!!                               |watershed
!!    wshd_resha  |ha            |watershed area in hectares which drains into
!!                               |reservoirs
!!    wshd_ressed |metric tons   |total amount of suspended sediment in
!!                               |reservoirs in the watershed
!!    wshd_resv   |m**3          |total volume of water in all reservoirs in
!!                               |the watershed
!!    wshdaao(13) |metric tons   |sediment loading to ponds in watershed 
!!    wshdaao(14) |metric tons   |sediment loading from ponds in watershed
!!    wshdaao(15) |metric tons   |net change in sediment level in ponds in
!!                               |watershed
!!    wshdaao(19) |m^3 H2O       |evaporation from ponds in watershed
!!    wshdaao(20) |m^3 H2O       |seepage from ponds in watershed
!!    wshdaao(21) |m^3 H2O       |precipitation on ponds in watershed
!!    wshdaao(22) |m^3 H2O       |volume of water entering ponds in watershed
!!    wshdaao(23) |m^3 H2O       |volume of water leaving ponds in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wshd_pndsed |metric tons/ha|mass balance discrepancy for pond
!!                               |sediment expressed as loading per unit
!!                               |hectare of drainage area
!!    wshd_pndv   |mm H2O        |mass balance discrepancy for pond water
!!                               |volume expressed as depth over drainage
!!                               |area
!!    wshd_ressed |metric tons/ha|mass balance discrepancy for reservoir
!!                               |sediment expressed as loading per unit
!!                               |hectare of drainage area
!!    wshd_resv   |mm H2O        |mass balance discrepancy for reservoir water
!!                               |volume expressed as depth over drainage
!!                               |area
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |counter
!!    res3        |metric tons   |sediment entering all reservoirs
!!    res4        |metric tons   |sediment leaving all reservoirs
!!    res17       |m^3 H2O       |evaporation from all reservoirs
!!    res18       |m^3 H2O       |seepage from all reservoirs
!!    res19       |m^3 H2O       |precipitation on all reservoirs
!!    res20       |m^3 H2O       |water entering all reservoirs
!!    res21       |m^3 H2O       |water leaving all reservoirs
!!    sedout      |metric tons   |total sediment in ponds at end of
!!                               |simulation
!!    tir         |mm H2O        |average annual amount of irrigation
!!                               |water applied to watershed
!!    volout      |m^3 H2O       |total volume of water in ponds at end
!!                               |of simulation
!!    wshd_snoe   |mm H2O        |amount of water in snow in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: swbl, vbl

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real*8 :: tir, wshd_snoe, volout, sedout, res3, res4, res17
      real*8 :: res18, res19, res20, res21

!! compute amount of irrigation water applied in watershed
      tir = 0.
      do j = 1, nhru
        if (irrsc(j) > 0) then
          tir = tir + aairr(j) * hru_dafr(j)
        end if
      end do

!! compute amount of water stored in snow at end of simulation in watershed
      wshd_snoe = 0.
      do j = 1, nhru
        wshd_snoe = wshd_snoe + sno_hru(j) * hru_dafr(j)
      end do

!! check final soil water balance
      call swbl(wshd_snoe,tir)

!! check pond water and sediment balance
      if (wshd_pndha > 1.e-4) then
        volout = 0.
        sedout = 0.
        do j = 1, nhru
          volout = volout + pnd_vol(j)
          sedout = sedout + pnd_vol(j) * pnd_sed(j)
        end do
        call vbl(wshdaao(19),wshdaao(20),wshdaao(21),wshdaao(22),       
     &           wshdaao(23),wshd_pndv,wshd_pndsed,wshdaao(13),         
     &           wshdaao(14),wshdaao(15),volout,sedout,wshd_pndha)
      end if

!! check reservoir water and sediment balance
      if (wshd_resha > 1.e-4) then
        res3 = 0.
        res4 = 0.
        res17 = 0.
        res18 = 0.
        res19 = 0.
        res20 = 0.
        res21 = 0.
        volout = 0.
        sedout = 0.
        do j = 1, nres
          res3 = res3 + resouta(3,j)
          res4 = res4 + resouta(4,j)
          res17 = res17 + resouta(17,j)
          res18 = res18 + resouta(18,j)
          res19 = res19 + resouta(19,j)
          res20 = res20 + resouta(20,j)
          res21 = res21 + resouta(21,j)
          volout = volout + res_vol(j)
          sedout = sedout + res_vol(j) * res_sed(j)
        end do
        call vbl(res17,res18,res19,res20,res21,wshd_resv,wshd_ressed, 
     &        res3,res4,0.0D+00,volout,sedout,wshd_resha)
      end if

      return
      end