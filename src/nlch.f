      subroutine nlch
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine simulates the loss of nitrate via surface runoff, 
!!    lateral flow, tile flow, and percolation out of the profile

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    anion_excl(:)|none         |fraction of porosity from which anions
!!                               |are excluded
!!    flat(:,:)   |mm H2O        |lateral flow in soil layer on current day
!!    ihru        |none          |HRU number
!!    nperco      |none          |nitrate percolation coefficient (0-1)
!!                               |0:concentration of nitrate in surface runoff
!!                               |  is zero
!!                               |1:surface runoff has same concentration of
!!                               |  nitrate as percolate
!!    sol_nly(:)  |none          |number of layers in soil profile
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in soil layer
!!    sol_prk(:,:)|mm H2O        |percolation from soil layer on current day
!!    sol_ul(:,:) |mm H2O        |amount of water held in the soil layer at
!!                               |saturation
!!    surfq(:)    |mm H2O        |surface runoff generated on day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    latno3(:)   |kg N/ha       |amount of nitrate transported with lateral
!!                               |flow
!!    percn(:)    |kg N/ha       |amount of nitrate percolating past bottom
!!                               |of soil profile
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the nitrate pool
!!                               |in soil layer
!!    surqno3(:)  |kg N/ha       |amount of nitrate transported with surface 
!!                               |runoff
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    co          |kg N/mm       |concentration of nitrate in solution
!!    cosurf      |kg N/mm       |concentration of nitrate in surface runoff
!!    j           |none          |HRU number
!!    jj          |none          |counter (soil layers)
!!    percnlyr    |kg N/ha       |nitrate leached to next lower layer with
!!                               |percolation
!!    sro         |mm H2O        |surface runoff
!!    ssfnlyr     |kg N/ha       |nitrate transported in lateral flow from layer
!!    vno3        |
!!    vv          |mm H2O        |water mixing with nutrient in layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Max, Min

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, jj
      real :: sro, ssfnlyr, percnlyr, vv, vno3, co
      real :: cosurf, nloss

      j = 0
      j = ihru

      percnlyr = 0.

      tno3 = 0.
      do jj = 1, sol_nly(j)
        tno3 = tno3 + sol_no3(jj,j)
      end do
          
      do jj = 1, sol_nly(j)

        !! add nitrate leached from layer above
        sol_no3(jj,j) = sol_no3(jj,j) + percnlyr
	  if (sol_no3(jj,j) < 1.e-6) sol_no3(jj,j) = 0.0

        !! determine concentration of nitrate in mobile water
        sro = 0.
        vv = 0.
        vno3 = 0.
        co = 0.
        if (jj == 1) then
          sro = surfq(j)
        else
          sro = 0.
        end if
        vv = sol_prk(jj,j) + sro + flat(jj,j) + 1.e-10
        if (ldrain(j) == jj) vv = vv + qtile
        ww = -vv / ((1. - anion_excl(j)) * sol_ul(jj,j))
        vno3 = sol_no3(jj,j) * (1. - Exp(ww))
        if (vv > 1.e-10)  co = Max(vno3 / vv, 0.)

        !! calculate nitrate in surface runoff
        cosurf = 0.
        if (isep_opt(j)==2) then
            cosurf = 1.0 * co ! N percolation does not apply to failing septic HRUs 
        else
            cosurf = nperco(j) * co
        end if
        if (jj == 1) then
          surqno3(j) = surfq(j) * cosurf
          surqno3(j) = Min(surqno3(j), sol_no3(jj,j))
          !! bmp adjustment
          surqno3(j) = surqno3(j) * bmp_sn(j)
          sol_no3(jj,j) = sol_no3(jj,j) - surqno3(j)
        endif
        !! Daniel 1/2012    
        !! calculate nitrate in tile flow 
        if (ldrain(j) == jj) then
          alph_e(j) = Exp(-1./(n_lag(j) + 1.e-6))
          ww1 = -1./ ((1. - anion_excl(j)) * sol_ul(jj,j))
          vno3_c = sol_no3(jj,j) * (1. - Exp(ww1))
          if (tno3 > 1.001) then
            tno3ln = n_lnco(j) * (Log(tno3)) ** n_ln(j)
          else
            tno3ln = 0.
          end if
          vno3_c = tno3ln * (1. - Exp(ww1))
          co_p(j) = co_p(j) * (1. - alph_e(j)) + vno3_c * alph_e(j)
          tileno3(j) = co * qtile     !Daniel 1/2012
          tileno3(j) = Min(tileno3(j), sol_no3(jj,j))
          !! bmp adjustment
          tileno3(j) = tileno3(j) * bmp_snt(j)
          sol_no3(jj,j) = sol_no3(jj,j) - tileno3(j)          
        end if
        !Daniel 1/2012                  

        !! calculate nitrate in lateral flow
        ssfnlyr = 0.
        if (jj == 1) then
          ssfnlyr = cosurf * flat(jj,j)
        else
          ssfnlyr = co * flat(jj,j)
        end if
        ssfnlyr = Min(ssfnlyr, sol_no3(jj,j))
        latno3(j) = latno3(j) + ssfnlyr
        !! bmp adjustment
        latno3(j) = latno3(j) * bmp_sns(j)
        sol_no3(jj,j) = sol_no3(jj,j) - ssfnlyr

        !! calculate nitrate in percolate
        percnlyr = 0.
        percnlyr = co * sol_prk(jj,j)
        percnlyr = Min(percnlyr, sol_no3(jj,j))
        sol_no3(jj,j) = sol_no3(jj,j) - percnlyr
      end do

      !! calculate nitrate leaching from soil profile
      percn(j) = percnlyr


      nloss = (2.18 * dis_stream(j) - 8.63) / 100.
      nloss = amax1(0.,nloss)
      nloss = Amin1(1.,nloss)
      latno3(j) = (1. - nloss) * latno3(j)

      return
      end