      subroutine sched_mgt
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    auto_eff(:) |none          |fertilizer application efficiency calculated
!!                               |as the amount of N applied divided by the
!!                               |amount of N removed at harvest
!!    icpst                      |icpst = 0 do not apply = 1 application period
!!    ipst_freq   |days          |number of days between applications
!!    iday_pest   |day           |current day between applications
!!    ndcpst      |day           |current day within the application period
!!    irramt(:)   |mm H20        |depth of irrigation water applied to HRU
!!    irrsalt(:)  |mg/kg         |concentration of salt in irrigation water
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      j = ihru
      
      select case (mgtop(nop(j),j))

          case (1)  !! plant operation
            igro(j) = 1
            lai_init = mgt5op(nop(j),j)
            bio_init = mgt6op(nop(j),j)
            hi_targ(j) = mgt7op(nop(j),j)
            bio_targ(j) = mgt8op(nop(j),j) * 1000.
            cnop = mgt9op(nop(j),j)
            curyr_mat(j) = mgt3iop(nop(j),j)
            if (curyr_mat(j) == 0) igrotree(j) = 1
            
            idplt(j) = mgt1iop(nop(j),j)
         
            if (mgt4op(nop(j),j) < 700.) mgt4op(nop(j),j) = 1700.
!            if (mgt4op(nop(j),j) > 5000.) mgt4op(nop(j),j) = 5000.
            phu_plt(j) = mgt4op(nop(j),j)

            call plantop 

            if (imgt == 1) then
              write (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida,   
     &        hru_km(j),cpnm(idplt(j))," PLANT", phubase(j), phuacc(j), 
     &        sol_sw(j),bio_ms(j), sol_rsd(1,j),sol_sumno3(j),
     &        sol_sumsolp(j)
            end if

 
          case (2)  !! irrigation operation
            irr_sc(ihru) = mgt2iop(nop(j),j)     !!NUBZ
            irr_no(ihru) = mgt10iop(nop(j),j)
            irramt(ihru) = mgt4op(nop(j),j)
            irrsalt(ihru) = mgt5op(nop(j),j)
            irrefm(ihru) = mgt6op(nop(j),j)
            irrsq(ihru) = mgt7op(nop(j),j)
	      irr_flag(ihru) = 1
            
            if (irrefm(ihru) < 1.e-6) irrefm(ihru)=1.0
            if (irr_sc(j) <= 0) irr_sc(j) = irrsc(j)
            if (irr_no(j) <= 0) irr_no(j) = irrno(j)
            if (irr_no(j) <= 0) irr_no(j) = hru_sub(j)
            if (irr_sc(ihru) > 2) then    !! reach and res flag ??
              call irrsub
            endif
            
            if (imgt ==1) then
              write (143, 1002) subnum(j), hruno(j), iyr, i_mo, 
     *        iida, hru_km(j), "        ",
     *        "IRRIGATE", phubase(j), phuacc(j), sol_sw(j),bio_ms(j), 
     *        sol_rsd(1,j), sol_sumno3(j),sol_sumsolp(j),irramt(j),
     *        irr_sc(j), irr_no(j)
1002  format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,7f10.2,10x,f10.2,70x,2i7)
             
            end if
            
          
          case (3)   !! fertilizer operation
            ifrttyp = mgt1iop(nop(j),j)
            frt_kg = mgt4op(nop(j),j)
            frt_surface = mgt5op(nop(j),j)
            if (frt_surface <= 1.e-6) frt_surface = 0.2
            
            call fert
            
            if (imgt ==1) then
              write (143, 1004) subnum(j), hruno(j), iyr, i_mo, iida, 
     *        hru_km(j), fertnm(ifrttyp),
     *        "   FERT", phubase(j), phuacc(j), sol_sw(j),bio_ms(j), 
     *        sol_rsd(1,j), sol_sumno3(j),sol_sumsolp(j),frt_kg,
     *        fertno3, fertnh3, fertorgn, fertsolp, fertorgp
1004  format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,7f10.2,20x,f10.2,10x,5f10.2)
            endif
            
   
          case (4)   !! pesticide operation
            hrupest(ihru) = 1
            ipest = mgt1iop(nop(j),j)
            pst_kg = mgt4op(nop(j),j)
            pst_dep = mgt5op(nop(j),j)
            
            call apply
            
            if (imgt ==1) then
              write (143, 1004) subnum(j), hruno(j), iyr, i_mo, iida, 
     *        hru_km(j), pname(ipest),
     *        "   PEST", phubase(j), phuacc(j), sol_sw(j),bio_ms(j), 
     *        sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j),pst_kg
            endif
     
          case (5)   !! harvest and kill operation
            cnop = mgt4op(nop(j),j)
            hi_ovr = mgt5op(nop(j),j)
            frac_harvk = mgt6op(nop(j),j)
            biomass = bio_ms(j)
            
            call harvkillop       
            
            if (imgt ==1) then
              write (143, 1001) subnum(j), hruno(j), iyr, i_mo, iida, 
     *        hru_km(j), cpnm(idplt(j)),
     *        "HARV/KILL", phubase(j), phuacc(j), sol_sw(j),biomass, 
     *        sol_rsd(1,j), sol_sumno3(j),sol_sumsolp(j),yield,
     *        strsn_sum(j), strsp_sum(j), strstmp_sum(j), strsw_sum(j),
     *        strsa_sum(j)
!!1001  format (a5,1x,a4,3i6,2a15,8f10.2,30x,11f10.2)
1001    format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,8f10.2,30x,5f10.2,14x,
     *     6f10.2)
            end if 
            
            phubase(j) = 0.
            phuacc(j) = 0.
            
          case (6)   !! tillage operation
            idtill = mgt1iop(nop(j),j)
            cnop = mgt4op(nop(j),j)
            
            call newtillmix(j,0.)
            
            if (imgt ==1) then
              write (143, 1003) subnum(j), hruno(j),iyr, i_mo, iida, 
     *        hru_km(j), tillnm(idtill),
     *        "TILLAGE", phubase(j), phuacc(j), sol_sw(j),bio_ms(j), 
     *        sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j), effmix(idtill)
1003  format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,7f10.2,30x,f10.2)
            end if
            
          case (7)  !! harvest only operation
            hi_bms = mgt5op(nop(j),j)
            hi_rsd = mgt6op(nop(j),j)
            harveff = mgt4op(nop(j),j)
            if (harveff <= 0.) harveff = 1.0 
            call harvestop

            if (imgt == 1) then
              write (143, 1001) subnum(j), hruno(j), iyr, i_mo, iida, 
     *        hru_km(j), cpnm(idplt(j)),
     *        "HARVEST ONLY", phubase(j), phuacc(j),sol_sw(j),bio_ms(j),
     *        sol_rsd(1,j), sol_sumno3(j), sol_sumsolp(j), yield, 
     *        strsn_sum(j), strsp_sum(j), strstmp_sum(j), strsw_sum(j), 
     *        strsa_sum(j), yieldgrn, yieldbms, yieldtbr, yieldrsd, 
     *        yieldn, yieldp
            end if
          
          case (8)   !! kill operation
            call killop
  
            if (imgt == 1) then 
              write (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida,
     *        hru_km(j), "         ",
     *        "    KILL", phubase(j), phuacc(j), sol_sw(j),bio_ms(j), 
     *        sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j)
            end if
            
            phubase(j) = 0.
            phuacc(j) = 0.
            
          case (9)    !! grazing operation
            manure_id(j) = mgt2iop(nop(j),j) 
            grz_days(j) = mgt1iop(nop(j),j)
            bio_eat(j) = mgt4op(nop(j),j)
            bio_trmp(j) = mgt5op(nop(j),j)
            manure_kg(j) = mgt6op(nop(j),j)
            ndeat(j) = 0
            igrz(j) = 1
          
            if (manure_kg(j) < = 0.) then 
              manure_kg(j) = 0.95 * mgt4op(nop(j),j)
            end if
            call graze

            if (imgt == 1) then
              write (143, 1005) subnum(j), hruno(j), iyr, i_mo, iida,
     *        hru_km(j), "         ",
     *        "   GRAZE", phubase(j), phuacc(j), sol_sw(j),bio_ms(j), 
     *        sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j),manure_kg(j)
1005  format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,7f10.2,20x,f10.2)
            end if
          
          case (10)   !! auto irrigation operation 
            wstrs_id(j) = mgt1iop(nop(j),j)
            auto_wstr(j) = mgt4op(nop(j),j)
            irr_eff(j) = mgt5op(nop(j),j)
            irr_mx(j) = mgt6op(nop(j),j)
            irr_asq(j) = mgt7op(nop(j),j)
            irr_sca(j) = mgt2iop(nop(j),j)
            irr_noa(j) = mgt10iop(nop(j),j)
            if (irr_noa(j) <= 0) irr_noa(j) = irrno(j)
            if (irr_noa(j) <= 0) irr_noa(j) = hru_sub(j)
            if (wstrs_id(j) <= 0) wstrs_id(j) = 1     
            if (irr_eff(j) > 1.) irr_eff(j) = 0.
            if (irr_eff(j) == 0.) irr_eff(j) = 1.
            if (irr_mx(j) < 1.e-6) irr_mx(j) = 25.4
            if (irr_sca(j) <= 0) irr_sca(j) = irrsc(j)
            irra_flag(ihru) = 1
          if (imgt ==1) then
            write (143, 1010) subnum(j), hruno(j), iyr, i_mo, 
     *      iida, hru_km(j), "        ",
     *      "SCHED AUTORR", phubase(j), phuacc(j), sol_sw(j), bio_ms(j),
     *      sol_rsd(1,j), sol_sumno3(j),sol_sumsolp(j)
1010  format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,7f10.2)
          end if
          
            
          case (11)   !! auto fertilizer operation
            iafrttyp(j) = mgt1iop(nop(j),j)
            nstress(j) = mgt2iop(nop(j),j)
            auto_nstrs(j) = mgt4op(nop(j),j)
            auto_napp(j) = mgt5op(nop(j),j)
            if (auto_napp(j) < 1.e-6) auto_napp(j) = 250.
            auto_nyr(j) = mgt6op(nop(j),j)
            if (auto_nyr(j) < 1.e-6) auto_nyr(j) = 350.
            auto_eff(j) = mgt7op(nop(j),j)
            if (auto_eff(j) <= 0.) auto_eff(j) = 1.3
            afrt_surface(j) = mgt8op(nop(j),j)
            if (afrt_surface(j) <= 1.e-6) afrt_surface(j) = .8
            !! calculate tnylda for autofertilization
            ncrp = idplt(j)
            if (tnylda(j) < 1.e-6)tnylda(j)=150.*cnyld(ncrp)*bio_e(ncrp)
      !      if (tnylda(j) < 1.e-6)tnylda(j)=350.*cnyld(ncrp)*bio_e(ncrp)
      !         tnylda(j) = 350. * cnyld(ncrp) * bio_e(ncrp)
      !        tnylda(j) = 350. * cnyld(ncrp) * bio_e(ncrp)
      !       else
      !         tnylda(j) = 1000. * cnyld(ncrp) * bio_e(ncrp)
      !    endif
          
          case (12)   !! street sweeping (only if iurban=2)

            if (husc > 0.) then
              if (igrow == 1) then
                phusw(ihru) = husc
              else
                phusw_nocrop(ihru) = husc
              endif        
            endif   
            sweepeff = mgt4op(nop(j),j)
            fr_curb = mgt5op(nop(j),j)
            
            if (imgt == 1) then
              write (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, 
     *        hru_km(j), "         ",
     *        "STREET SWEEP",phubase(j), phuacc(j), sol_sw(j),bio_ms(j),
     *        sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j)
            end if
          
          case (13)    !! release/impound water in rice fields
            imp_trig(j) = mgt1iop(nop(j),j)
          
            if (imgt == 1) then
              write (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, 
     *        hru_km(j), "         ","RELEASE/IMPOUND", phubase(j),
     *        phuacc(j),sol_sw(j),bio_ms(j),sol_rsd(1,j),sol_sumno3(j),
     *        sol_sumsolp(j)
            end if
          
          case (14)    !! continuous fertilization operation
            fert_days(j) = mgt1iop(nop(j),j)
            cfrt_id(j) = mgt2iop(nop(j),j)
            ifrt_freq(j) = mgt3iop(nop(j),j)
            cfrt_kg(j) = mgt4op(nop(j),j)
            icfrt(j) = 1
            ndcfrt(j) = 1
            iday_fert(j) = ifrt_freq(j)

          case (15)    !! continuous pesticide operation
            cpst_id(j) = mgt1iop(nop(j),j)
            pest_days(j) = mgt2iop(nop(j),j)
            ipst_freq(j) = mgt3iop(nop(j),j)
            cpst_kg(j) = mgt4op(nop(j),j)
            icpst(j) = 1
            ndcpst(j) = 0
            iday_pest(j) = ipst_freq(j)

          case (16)   !! burning
            burn_frlb = mgt4op(nop(j),j)
            call burnop
            if (imgt == 1) then
              write (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, 
     *        hru_km(j), "         ",
     *        "      BURN", phubase(j), phuacc(j), sol_sw(j),bio_ms(j), 
     *        sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j)
            end if

          case (17)    !! skip a year
            yr_skip(j) = 1
          
      end select
      
      if (mgtop(nop(j),j) /= 17) then 
        nop(j) = nop(j) + 1
      end if
      
      if (nop(j) > nopmx(j)) then
        nop(j) = 1
      end if
      
1000  format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,19f10.2)    
      return

      end