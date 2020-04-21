      subroutine urb_bmp
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name       |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

	j = 0
	j = ihru

!! convert to ppm -> (kg/ha)*100./mm = ppm
      if (qdr(j) > 0.1) then
	  xx = 100. / qdr(j)
        sedppm = 1000. * xx * sedyld(j) / hru_ha(j)
        solnppm = xx * (surqno3(j) + latno3(j) + no3gw(j))
        solpppm = xx * (surqsolp(j) + minpgw(j))
        sednppm = xx * sedorgn(j)
        sedpppm = xx * (sedorgp(j) + sedminpa(j) + sedminps(j))
      
        if (sedppm > sed_con (j)) then
          sedyld(j) = sedyld(j) * ((1. - pot_fr(j)) + pot_fr(j) *
     *        sed_con(j) * hru_ha(j) / xx / 1000.)
        endif

        if (solnppm > soln_con(j)) then
          rto = (soln_con(j) / xx) / (surqno3(j) + latno3(j) + no3gw(j))
	    surqno3(j) = surqno3(j) * (rto * pot_fr(j) + (1.-pot_fr(j)))
          latno3(j) = latno3(j) * (rto * pot_fr(j) + (1.-pot_fr(j)))
	    no3gw(j) = no3gw(j) * (rto * pot_fr(j) + (1.-pot_fr(j)))
        endif

        if (solpppm > solp_con(j)) then
          rto = (solp_con(j) / xx) / (surqsolp(j) + minpgw(j))
          surqsolp(j) = surqsolp(j) * (rto * pot_fr(j) + (1.-pot_fr(j)))
          minpgw(j) = minpgw(j) * (rto * pot_fr(j) + (1. - pot_fr(j)))
        endif

        if (sednppm > orgn_con(j)) then
          sedorgn(j) = sedorgn(j) * (pot_fr(j) * orgn_con(j) / xx +
     *          (1. - pot_fr(j)))
        endif

        if (sedpppm > orgp_con(j)) then 
          rto = (orgp_con(j) / xx) / (sedorgp(j) + sedminpa(j) +
     *        sedminps(j))
	    sedorgn(j)=sedorgp(j) * (rto * pot_fr(j) + (1. - pot_fr(j)))
          sedminpa(j)=sedminpa(j) * (rto * pot_fr(j) + (1. - pot_fr(j)))
	    sedminps(j)=sedminps(j) * (rto * pot_fr(j) + (1. - pot_fr(j)))
        endif

	endif

      return
      end
