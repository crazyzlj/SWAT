      subroutine storeinitial

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine saves initial values for variables that must be reset to 
!!    rerun the simulation for different real time weather scenarios
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

      use parm

!!weather
      ivar_orig(1) = iyr
      ivar_orig(2) = pcpsim
      ivar_orig(3) = tmpsim
      ivar_orig(4) = rhsim
      ivar_orig(5) = slrsim
      ivar_orig(6) = wndsim
      otmpmx = tmpmx
      otmpmn = tmpmn
      otmpstdmx = tmpstdmx
      otmpstdmn = tmpstdmn
      opcp_stat = pcp_stat
      opr_w = pr_w
      ogen = igen


      rvar_orig(1) = wshd_sw
      rvar_orig(2) = wshd_pndv
      rvar_orig(3) = wshd_resv
      rvar_orig(4) = wshd_ressed

!! arrays
      orig_snoeb = snoeb
      orig_snohru = sno_hru

      orig_igro = igro
      orig_alai = laiday
      orig_bioms = bio_ms
      orig_phu = phu_plt
      orig_phuacc = phuacc
  !!    orig_tnylda = tnylda

      orig_sumix = sumix

      orig_pltpst = plt_pst
      orig_solpst = sol_pst
      orig_solno3 = sol_no3
      orig_solorgn = sol_orgn
      orig_solorgp = sol_orgp
      orig_solsolp = sol_solp
      orig_soltmp = sol_tmp
      orig_solrsd = sol_rsd
      orig_solcov = sol_cov
      orig_solfop = sol_fop
      orig_solfon = sol_fon
      orig_solaorgn = sol_aorgn
      orig_solactp = sol_actp
      orig_solstap = sol_stap
      orig_solst = sol_st
      orig_solsw = sol_sw
      orig_volcr = volcr

      orig_shallst = shallst
      orig_deepst = deepst

      orig_potvol = pot_vol
      orig_potsed = pot_sed
      orig_potno3 = pot_no3
      orig_pndvol = pnd_vol
      orig_pndsed = pnd_sed
      orig_pndno3 = pnd_no3
      orig_pndsolp = pnd_solp
      orig_pndorgp = pnd_orgp
      orig_pndorgn = pnd_orgn
      orig_wetvol = wet_vol
      orig_wetsed = wet_sed
      orig_wetno3 = wet_no3
      orig_wetorgn = wet_orgn
      orig_wetorgp = wet_orgp
      orig_wetsolp = wet_solp


      orig_sedpstconc = sedpst_conc

      orig_resvol = res_vol
      orig_ressed = res_sed
      orig_lkpstconc = lkpst_conc
      orig_lkspstconc = lkspst_conc
      orig_ressolp = res_solp
      orig_resorgp = res_orgp
      orig_resno3 = res_no3
      orig_resno2 = res_no2
      orig_resnh3 = res_nh3
      orig_resorgn = res_orgn

      return
      end