      subroutine soil_par

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU/subbasin soil properties file 
!!    (.sol). This file contains data related to soil physical properties and
!!    general chemical properties.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    i             |none          |HRU number
!!    mlyr          |none          |maximum number of soil layers
!!    idplt(:)      |none          |land cover/crop identification code for
!!                                 |first crop grown in HRU (the only crop if
!!                                 |there is no rotation)
!!    rdmx(:)       |m             |maximum root depth of plant
!!    rsdin(:)      |kg/ha         |initial residue cover
!!    sol_no3(:,:)  |mg N/kg       |concentration of nitrate in soil layer
!!    sol_orgn(1,:) |mg N/kg soil  |organic N concentration in top soil layer
!!    sol_orgp(1,:) |mg P/kg soil  |organic P concentration in top soil layer
!!    sol_solp(1,:) |mg P/kg soil  |soluble P concentration in top soil layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    anion_excl(:) |none          |fraction of porosity from which anions
!!                                 |are excluded
!!    sol_clay(:,:) |%             |percent clay content in soil material
!!    snam(:)       |NA            |soil series name
!!    sol_alb(:)    |none          |albedo when soil is moist
!!    sol_awc(:,:)  |mm H20/mm soil|available water capacity of soil layer
!!    sol_bd(:,:)   |Mg/m**3       |bulk density of the soil
!!    sol_cbn(:,:)  |%             |percent organic carbon in soil layer
!!    sol_crk(:)    |none          |crack volume potential of soil
!!    sol_k(:,:)    |mm/hr         |saturated hydraulic conductivity of soil 
!!                                 |layer
!!    sol_nly(:)    |none          |number of soil layers 
!!    sol_no3(:,:)  |mg N/kg       |concentration of nitrate in soil layer
!!    sol_orgn(1,:) |mg N/kg soil  |organic N concentration in top soil layer
!!    sol_orgp(1,:) |mg P/kg soil  |organic P concentration in top soil layer
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil layer
!!                                 |classified as residue
!!    sol_solp(1,:) |mg P/kg soil  |soluble P concentration in top soil layer
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus 
!!                                 |pool
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    sol_zmx(:)    |mm            |maximum rooting depth
!!    usle_k(:)     |none          |USLE equation soil erodibility (K) factor
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    flag        |none          |flag to exit do loop
!!    j           |none          |counter
!!    jj          |none          |dummy variable to hold value
!!    n           |none          |counter
!!    nly         |none          |number of soil layers
!!    plt_zmx     |mm            |rooting depth of plant
!!    sand        |%             |percent sand content of soil material
!!    titldum     |NA            |title line/skipped line in .sol file
!!    xx          |none          |variable to hold value
!!    yy          |none          |variable to hold value
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Exp, Abs

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
      integer :: j, nly, n, jj, flag
      real ::  xx, plt_zmx, yy

      nly=sol_nly(i)
!!    add 10mm layer at surface of soil
      if (sol_z(1,i) > 10.1) then
        sol_nly(i) = sol_nly(i) + 1
        nly = nly + 1
        do j = nly, 2, -1
          sol_z(j,i) = sol_z(j-1,i)
          sol_bd(j,i) = sol_bd(j-1,i)
          sol_awc(j,i) = sol_awc(j-1,i)
          sol_k(j,i) = sol_k(j-1,i)
          sol_cbn(j,i) = sol_cbn(j-1,i)
          sol_clay(j,i) = sol_clay(j-1,i)
          sol_no3(j,i) = sol_no3(j-1,i)
          sol_orgn(j,i) = sol_orgn(j-1,i)
          sol_orgp(j,i) = sol_orgp(j-1,i)
          sol_solp(j,i) = sol_solp(j-1,i)
        end do
        sol_z(1,i) = 10.
      endif


!!    compare maximum rooting depth in soil to maximum rooting depth of
!!    plant
      if (sol_zmx(i) <= 0.001) sol_zmx(i) = sol_z(nly,i)
      plt_zmx = 1000. * rdmx(idplt(i))
      if (sol_zmx(i) > 1. .and. plt_zmx > 1.) then
         sol_zmx(i) = Min(sol_zmx(i),plt_zmx)
      else
         !! if one value is missing it will set to the one available
         sol_zmx(i) = Max(sol_zmx(i),plt_zmx)
      end if


!!    create a layer boundary at maximum rooting depth (sol_zmx)
      if (sol_zmx(i) > 0.001) then
        flag = 0
        do j = 1, nly - 1
          xx = 0.
          yy = 0.
          xx = Abs(sol_zmx(i)-sol_z(j,i))
          yy = Abs(sol_zmx(i)-sol_z(j+1,i))
          !! if values are within 51 mm of one another, reset boundary
          if (xx < 51. .and. yy > 51.) then
            sol_z(j,i) = sol_zmx(i)
            exit
          end if

          !! set a soil layer at sol_zmx and adjust all lower layers
          if (sol_z(j,i) > sol_zmx(i)) then
            flag = 1
            sol_nly(i) = sol_nly(i) + 1
            nly = nly + 1
            jj = 0
            jj = j + 1
            do n = nly, jj, -1
              sol_z(n,i) = sol_z(n-1,i)
              sol_bd(n,i) = sol_bd(n-1,i)
              sol_awc(n,i) = sol_awc(n-1,i)
              sol_k(n,i) = sol_k(n-1,i)
              sol_cbn(n,i) = sol_cbn(n-1,i)
              sol_clay(n,i) = sol_clay(n-1,i)
              sol_no3(n,i) = sol_no3(n-1,i)
              sol_orgn(n,i) = sol_orgn(n-1,i)
              sol_orgp(n,i) = sol_orgp(n-1,i)
              sol_solp(n,i) = sol_solp(n-1,i)
            end do
            sol_z(j,i) = sol_zmx(i)
          end if
          if (flag == 1) exit
        end do
      end if
              


!!    set default values/initialize variables
      if (sol_alb(i) < 0.1) sol_alb(i) = 0.1
      if (anion_excl(i) <= 0.) anion_excl(i) = 0.5
      if (anion_excl(i) >= 1.) anion_excl(i) = 0.99
      if (rsdin(i) > 0.) sol_rsd(1,i) = rsdin(i)
      do j = 1, nly
        if (sol_bd(j,i) <= 1.e-6) sol_bd(j,i) = 1.3
        if (sol_bd(j,i) > 2.) sol_bd(j,i) = 2.0
        if (sol_awc(j,i) <= 0.) sol_awc(j,i) = .005
      end do

      return
 5000 format (27x,10f12.2)
 5100 format (12x,a16)
 5200 format (24x,a1)
 5300 format (28x,f12.2)
 5400 format (51x,f5.3)
 5500 format (a80)
 5600 format (33x,f5.3)
      end
