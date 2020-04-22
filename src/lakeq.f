      subroutine lakeq

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes the lake hydrologic pesticide balance.
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    inum1         |none          |reservoir number
!!    lkpst_conc(:) |mg/m^3        |pesticide concentration in lake water
!!    lkpst_koc(:)  |m**3/g        |pesticide partition coefficient between
!!                                 |water and sediment in lake water
!!    lkpst_mix(:)  |m/day         |mixing velocity (diffusion/dispersion) in
!!                                 |lake water for pesticide
!!    lkpst_rea(:)  |1/day         |pesticide reaction coefficient in lake water
!!    lkpst_rsp(:)  |m/day         |resuspension velocity in lake water for
!!                                 |pesticide sorbed to sediment
!!    lkpst_stl(:)  |m/day         |settling velocity in lake water for
!!                                 |pesticide sorbed to sediment
!!    lkpst_vol(:)  |m/day         |pesticide volatilization coefficient in lake
!!                                 |water
!!    lkspst_act(:) |m             |depth of active sediment layer in lake for
!!                                 |for pesticide
!!    lkspst_bry(:) |m/day         |pesticide burial velocity in lake bed
!!                                 |sediment
!!    lkspst_conc(:)|mg/m^3        |pesticide concentration in lake bed sediment
!!    lkspst_rea(:) |1/day         |pesticide reaction coefficient in lake bed
!!                                 |sediment
!!    res_sed(:)    |kg/L (ton/m^3)|amount of sediment in reservoir
!!    res_vol(:)    |m^3 H2O       |reservoir volume
!!    resflwo       |m^3 H2O       |water leaving reservoir on day
!!    ressa         |ha            |surface area of reservoir on day
!!    ressedo       |metric tons   |sediment leaving reservoir during time step
!!    solpesti      |mg pst        |soluble pesticide entering reservoir
!!    sorpesti      |mg pst        |sorbed pesticide entering reservoir
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury          |mg pst        |loss of pesticide from active sediment layer
!!                                 |by burial
!!    difus         |mg pst        |diffusion of pesticide from sediment to lake
!!                                 |water
!!    lkpst_conc(:) |mg/m^3        |pesticide concentration in lake water
!!    lkspst_conc(:)|mg/m^3        |pesticide concentration in lake bed sediment
!!    reactw        |mg pst        |amount of pesticide in lake water lost
!!                                 |through reactions
!!    reactb        |mg pst        |amount of pesticide in sediment that is lost
!!                                 |through reactions
!!    respesti      |mg pst        |pesticide entering reservoir on day
!!    resuspst      |mg pst        |amount of pesticide moving from sediment to
!!                                 |lake water due to resuspension
!!    setlpst       |mg pst        |amount of pesticide moving from water to
!!                                 |sediment due to settling
!!    solpesto      |mg pst        |soluble pesticide in outflow on day
!!    sorpesto      |mg pst        |sorbed pesticide in outflow on day
!!    volatpst      |mg pst        |amount of pesticide lost from lake water
!!                                 |by volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dlake       |m             |depth of water in reservoir
!!    fd1         |none          |fraction of pesticide in water that is soluble
!!    fd2         |none          |fraction of pesticide in sediment that is
!!                               |soluble
!!    fp1         |none          |fraction of pesticide in water that is sorbed
!!    fp2         |none          |fraction of pesticide in sediment that is 
!!                               |sorbed
!!    jres        |none          |reservoir number
!!    tpest1      |mg pst        |amount of pesticide in lake water
!!    tpest2      |mg pst        |amount of pesticide in lake sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: jres
      real*8 :: tpest1, tpest2, fd1, fp1, fd2, dlake, fp2

      jres = 0
      jres = inum1

      tpest1 = 0.
      tpest2 = 0.
      tpest1 = lkpst_mass(jres)
      tpest2 = lkspst_mass(jres)

      if (res_vol(jres) > 1.) then
        !! calculate depth of lake
        dlake = 0.
        dlake = res_vol(jres) / (ressa * 10000.)

        fd1 = 0.
        fp1 = 0.
        fd2 = 0.
        fp2 = 0.
        fd1 = 1. / (1. + lkpst_koc(jres) * res_sed(jres) * 1.e6)
        fp1 = 1. - fd1
        !! ASSUME POR=0.8; DENSITY=2.6E6, then concsed = 5.2e5; KD2=KD1
        fd2 = 1. / (.8 + 5.2e5 * lkpst_koc(jres))
        fp2 = 1. - fd2

        !! add incoming pesticide to pesticide in water layer
        respesti = solpesti + sorpesti
        tpest1 = tpest1 + respesti

        !! determine pesticide lost through reactions in water layer
        reactw = lkpst_rea(jres) * tpest1
        tpest1 = tpest1 - reactw

        !! determine pesticide lost through volatilization
        volatpst = lkpst_vol(jres) * fd1 * tpest1 / dlake
        if (volatpst > tpest1) then
          volatpst = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - volatpst
        end if

        !! determine amount of pesticide settling to sediment layer
        setlpst = lkpst_stl(jres) * fp1 * tpest1 / dlake
        if (setlpst > tpest1) then
          setlpst = tpest1
          tpest1 = 0.
          tpest2 = tpest2 + setlpst
        else
          tpest1 = tpest1 - setlpst
          tpest2 = tpest2 + setlpst
        end if

        !! determine pesticide resuspended into lake water
        resuspst = lkpst_rsp(jres) * tpest2 / lkspst_act(jres)
        if (resuspst > tpest2) then
          resuspst = tpest2
          tpest2 = 0.
          tpest1 = tpest1 + resuspst
        else
          tpest2 = tpest2 - resuspst
          tpest1 = tpest1 + resuspst
        end if

        !! determine pesticide diffusing from sediment to water
        difus = lkpst_mix(jres) *                                       
     &          (fd2 * tpest2 / lkspst_act(jres) - fd1 * tpest1 / dlake)
        if (difus > 0.) then
          if (difus > tpest2) then
            difus = tpest2
            tpest2 = 0.
          else
            tpest2 = tpest2 - abs(difus)
          end if
          tpest1 = tpest1 + abs(difus)
        else
          if (abs(difus) > tpest1) then
            difus = -tpest1
            tpest1 = 0.
          else
            tpest1 = tpest1 - abs(difus)
          end if
          tpest2 = tpest2 + abs(difus)
        end if

        !! determine pesticide lost from sediment by reactions
        reactb = lkspst_rea(jres) * tpest2
        if (reactb > tpest2) then
          reactb = tpest2
          tpest2 = 0.
        else
          tpest2 = tpest2 - reactb
        end if

        !! determine pesticide lost from sediment by burial
        bury = lkspst_bry(jres) * tpest2 / lkspst_act(jres)
        if (bury > tpest2) then
          bury = tpest2
          tpest2 = 0.
        else
          tpest2 = tpest2 - bury
        end if

        !! calculate soluble pesticide transported out of reservoir
        solpesto = resflwo * fd1 * tpest1 / res_vol(jres)
        if (solpesto > tpest1) then
          solpesto = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - solpesto
        end if

        !! calculate sorbed pesticide transported out of reservoir
        sorpesto = resflwo * fp1 * tpest1 / res_vol(jres)
        if (sorpesto > tpest1) then
          sorpesto = tpest1
          tpest1 = 0.
        else
          tpest1 = tpest1 - sorpesto
        end if

        !! update concentration of pesticide in lake water and sediment
          if (tpest1 < 1.e-10) tpest1 = 0.0
          if (tpest2 < 1.e-10) tpest2 = 0.0
          lkpst_mass(jres) = tpest1
          lkspst_mass(jres) = tpest2
        lkpst_conc(jres) = tpest1 / res_vol(jres)
        lkspst_conc(jres) = tpest2 /                                    
     &                          (lkspst_act(jres) * ressa * 10000. + 1.)
      else
        solpesto = 0.
        sorpesto = 0.
      end if


      return
      end