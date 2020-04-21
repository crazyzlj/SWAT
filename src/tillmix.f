      subroutine tillmix(jj,bmix)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine mixes residue and nutrients during tillage and 
!!    biological mixing

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil 
!!                                 |particles
!!    cnop(:,:,:)   |none          |SCS runoff curve number for moisture
!!                                 |condition II
!!    curyr         |none          |current year of simulation
!!    deptil(:)     |mm            |depth of mixing caused by tillage
!!                                 |operation
!!    effmix(:)     |none          |mixing efficiency of tillage operation
!!    mlyr          |none          |maximum number of soil layers
!!    npmx          |none          |number of different pesticides used in
!!                                 |the simulation
!!    nro(:)        |none          |sequence number of year in rotation
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    nyskip        |none          |number of years to skip output printing/
!!                                 |summarization
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_nly(:)    |none          |number of soil layers
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool.
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool
!!    sol_pst(:,:,:)|kg/ha         |amount of pesticide in layer
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil
!!                                 |classified as residue
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus pool
!!    sol_z(:,:)    |mm            |depth to bottom of soil layer
!!    sumix(:)      |none          |sum of mixing efficiencies in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactlpq(:)    |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)    |# colonies/ha |less persistent bacteria attached to soil
!!                                 |particles
!!    bactpq(:)     |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)     |# colonies/ha |persistent bacteria attached to soil 
!!                                 |particles
!!    ntil(:)       |none          |sequence number of tillage operation within
!!                                 |current year
!!    sol_actp(:,:) |kg P/ha       |amount of phosphorus stored in the
!!                                 |active mineral phosphorus pool
!!    sol_aorgn(:,:)|kg N/ha       |amount of nitrogen stored in the active
!!                                 |organic (humic) nitrogen pool
!!    sol_fon(:,:)  |kg N/ha       |amount of nitrogen stored in the fresh
!!                                 |organic (residue) pool
!!    sol_fop(:,:)  |kg P/ha       |amount of phosphorus stored in the fresh
!!                                 |organic (residue) pool
!!    sol_nh3(:,:)  |kg N/ha       |amount of nitrogen stored in the ammonium
!!                                 |pool in soil layer
!!    sol_no3(:,:)  |kg N/ha       |amount of nitrogen stored in the
!!                                 |nitrate pool.
!!    sol_orgn(:,:) |kg N/ha       |amount of nitrogen stored in the stable
!!                                 |organic N pool
!!    sol_orgp(:,:) |kg P/ha       |amount of phosphorus stored in the organic
!!                                 |P pool
!!    sol_rsd(:,:)  |kg/ha         |amount of organic matter in the soil
!!                                 |classified as residue
!!    sol_solp(:,:) |kg P/ha       |amount of phosohorus stored in solution
!!    sol_stap(:,:) |kg P/ha       |amount of phosphorus in the soil layer
!!                                 |stored in the stable mineral phosphorus pool
!!    sumix(:)      |none          |sum of mixing efficiencies in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bmix        |none          |biological mixing efficiency: this 
!!                               |number is zero for tillage operations
!!    dg          |mm            |depth of soil layer
!!    dtil        |mm            |depth of mixing
!!    emix        |none          |mixing efficiency
!!    jj          |none          |HRU number
!!    k           |none          |counter
!!    l           |none          |counter
!!    nl          |none          |number of layers being mixed
!!    smix(:)     |varies        |amount of substance in soil profile
!!                               |that is being redistributed between 
!!                               |mixed layers
!!    thtill(:)   |none          |fraction of soil layer that is mixed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Min, Max
!!    SWAT: curno 

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: jj
      real, intent (in) :: bmix
      integer :: l, k, nl
      real :: emix, dtil
      real :: thtill(mlyr), smix(11+npmx)

      emix = 0.
      dtil = 0.
      if (bmix > 1.e-6) then
        !! biological mixing
        emix = bmix
        dtil = Min(sol_z(sol_nly(jj),jj), 300.)
      else 
        !! tillage operation
        emix = effmix(idtill(nro(jj),ntil(jj),jj))
        dtil = deptil(idtill(nro(jj),ntil(jj),jj))
      endif


      if (dtil > 10.) then

        !! incorporate bacteria - no mixing - lost from transport
        bactpq(jj) = bactpq(jj) * (1. - emix)
        bactps(jj) = bactps(jj) * (1. - emix)
        bactlpq(jj) = bactlpq(jj) * (1. - emix)
        bactlps(jj) = bactlps(jj) * (1. - emix)

        thtill = 0.
        smix = 0.
        nl = 0
        thtill(1) = 1.
        do l = 1, sol_nly(jj)
          if (l /= 1) then
            if (sol_z(l,jj) > dtil) then
              if (sol_z(l-1,jj) < dtil) then
                thtill(l) = (dtil - sol_z(l-1,jj)) /                    &
     &                                     (sol_z(l,jj) - sol_z(l-1,jj))
                nl = l
              endif
            else
              thtill(l) = 1.
              nl = l
            endif
          endif
 
          !! calculate amount of each substance in the profile being
          !! redistributed between layers   
          if (thtill(l) > 0.) then
            smix(1) = smix(1) + thtill(l) * emix * sol_no3(l,jj)
            smix(2) = smix(2) + thtill(l) * emix * sol_orgn(l,jj)
            smix(3) = smix(3) + thtill(l) * emix * sol_nh3(l,jj)
            smix(4) = smix(4) + thtill(l) * emix * sol_solp(l,jj)
            smix(5) = smix(5) + thtill(l) * emix * sol_orgp(l,jj)
            smix(6) = smix(6) + thtill(l) * emix * sol_aorgn(l,jj)
            smix(7) = smix(7) + thtill(l) * emix * sol_actp(l,jj)
            smix(8) = smix(8) + thtill(l) * emix * sol_fon(l,jj)
            smix(9) = smix(9) + thtill(l) * emix * sol_fop(l,jj)
            smix(10) = smix(10) + thtill(l) * emix * sol_stap(l,jj)
            smix(11) = smix(11) + thtill(l) * emix * sol_rsd(l,jj)
            do k = 1, npmx
            smix(11+k) = smix(11+k) + thtill(l) * emix * sol_pst(k,jj,l)
            end do
          end if
        end do

        do l = 1, nl

          dg = 0.
          if (l == 1) then
            dg = sol_z(1,jj)
          else
            dg = sol_z(l,jj) - sol_z(l-1,jj)
          endif

          !! calculate new amount of each substance in each layer
          !! undergoing mixing
          sol_no3(l,jj) = sol_no3(l,jj) * (1. - thtill(l)) +            &
     &                    sol_no3(l,jj) * thtill(l) * (1. - emix) +     &
     &                    smix(1) * thtill(l) * dg / dtil

          sol_orgn(l,jj) = sol_orgn(l,jj) * (1. - thtill(l)) +          &
     &                    sol_orgn(l,jj) * thtill(l) * (1. - emix) +    &
     &                    smix(2) * thtill(l) * dg / dtil

          sol_nh3(l,jj) = sol_nh3(l,jj) * (1. - thtill(l)) +            &
     &                    sol_nh3(l,jj) * thtill(l) * (1. - emix) +     &
     &                    smix(3) * thtill(l) * dg / dtil

          sol_solp(l,jj) = sol_solp(l,jj) * (1. - thtill(l)) +          &
     &                    sol_solp(l,jj) * thtill(l) * (1. - emix) +    &
     &                    smix(4) * thtill(l) * dg / dtil

          sol_orgp(l,jj) = sol_orgp(l,jj) * (1. - thtill(l)) +          &
     &                    sol_orgp(l,jj) * thtill(l) * (1. - emix) +    &
     &                    smix(5) * thtill(l) * dg / dtil

          sol_aorgn(l,jj) = sol_aorgn(l,jj) * (1. - thtill(l)) +        &
     &                    sol_aorgn(l,jj) * thtill(l) * (1. - emix) +   &
     &                    smix(6) * thtill(l) * dg / dtil

          sol_actp(l,jj) = sol_actp(l,jj) * (1. - thtill(l)) +          &
     &                    sol_actp(l,jj) * thtill(l) * (1. - emix) +    &
     &                    smix(7) * thtill(l) * dg / dtil

          sol_fon(l,jj) = sol_fon(l,jj) * (1. - thtill(l)) +            &
     &                    sol_fon(l,jj) * thtill(l) * (1. - emix) +     &
     &                    smix(8) * thtill(l) * dg / dtil

          sol_fop(l,jj) = sol_fop(l,jj) * (1. - thtill(l)) +            &
     &                    sol_fop(l,jj) * thtill(l) * (1. - emix) +     &
     &                    smix(9) * thtill(l) * dg / dtil

          sol_stap(l,jj) = sol_stap(l,jj) * (1. - thtill(l)) +          &
     &                    sol_stap(l,jj) * thtill(l) * (1. - emix) +    &
     &                    smix(10) * thtill(l) * dg / dtil

          sol_rsd(l,jj) = sol_rsd(l,jj) * (1. - thtill(l)) +            &
     &                    sol_rsd(l,jj) * thtill(l) * (1. - emix) +     &
     &                    smix(11) * thtill(l) * dg / dtil
          sol_rsd(l,jj) = Max(sol_rsd(l,jj),0.)

          if (hrupest(jj) > 0) then
          do k = 1, npmx
            sol_pst(k,jj,l) = sol_pst(k,jj,l) * (1. - thtill(l)) +      &
     &                    sol_pst(k,jj,l) * thtill(l) * (1. - emix) +   &
     &                    smix(11+k) * thtill(l) * dg / dtil
          end do
          end if
        end do

        !! remove all residue from soil surface if mixing with moldboard
        !! plow (emix = 0.95 in default tillage database)
        if (emix > 0.9) then
          sol_rsd(2,jj) = sol_rsd(2,jj) + sol_rsd(1,jj)
          sol_rsd(1,jj) = 0.
        end if

        !! summary calculations
        if (curyr > nyskip) then
          sumix(jj) = sumix(jj) + emix
        end if

      end if

      !! perform final calculations for tillage operation
      if (cnop(nro(jj),ntil(jj),jj) > 1.e-4) then
        call curno(cnop(nro(jj),ntil(jj),jj),jj)
      end if
      ntil(jj) = ntil(jj) + 1

      return
      end
