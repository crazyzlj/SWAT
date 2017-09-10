      subroutine std2

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine writes general information to the standard output file
!!    and to miscellaneous output files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ch_d(:)       |m           |average depth of main channel
!!    ch_s(2,:)     |m/m         |average slope of main channel
!!    ch_w(2,:)     |m           |average width of main channel
!!    hru_fr(:)     |none        |fraction of subbasin area in HRU
!!    hru_ha(:)     |ha          |area of HRU 
!!    hrupest(:)    |none        |pesticide use flag:
!!                               | 0: no pesticides used in HRU
!!                               | 1: pesticides used in HRU
!!    irte          |none        |water routing method:
!!                               |0 variable storage method
!!                               |1 Muskingum method
!!    kirr(:)       |NA          |irrigation in HRU
!!    lkpst_conc(:) |mg/m**3     |pesticide concentration in lake water
!!    lkpst_koc(:)  |m**3/g      |pesticide partition coefficient between water
!!                               |and sediment in lake water
!!    lkpst_mix(:)  |m/day       |mixing velocity (diffusion/dispersion) in
!!                               |lake water for pesticide
!!    lkpst_rea(:)  |1/day       |pesticide reaction coefficient in lake water
!!    lkpst_rsp(:)  |m/day       |resuspension velocity in lake water for
!!                               |pesticide sorbed to sediment
!!    lkpst_stl(:)  |m/day       |settling velocity in lake water for pesticide
!!                               |sorbed to sediment
!!    lkpst_vol(:)  |m/day       |pesticide volatilization coefficient in lake
!!                               |water
!!    lkspst_act(:) |m           |depth of active sediment layer in lake for
!!                               |for pesticide
!!    lkspst_bry(:) |m/day       |pesticide burial velocity in lake bed
!!                               |sediment
!!    lkspst_conc(:)|mm/m**3     |pesticide concentration in lake bed sediment
!!    lkspst_rea(:) |1/day       |pesticide reaction coefficient in lake bed
!!                               |sediment
!!    msk_co1       |none        |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at bankfull depth (phi(10,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_co2       |none        |calibration coefficient to control impact
!!                               |of the storage time constant for the
!!                               |reach at 0.1 bankfull depth (phi(13,:) upon
!!                               |the storage time constant for the reach
!!                               |used in the Muskingum flow method
!!    msk_x         |none        |weighting factor controlling relative
!!                               |importance of inflow rate and outflow rate
!!                               |in determining storage on reach
!!    nhru          |none        |number of HRUs in watershed
!!    nres          |none        |number of reservoirs in watershed
!!    phi(10,:)     |hr          |storage time constant for reach at
!!                               |bankfull depth (ratio of storage to
!!                               |discharge)
!!    phi(13,:)     |hr          |storage time constant for reach at
!!                               |0.1 bankfull depth (low flow) (ratio
!!                               |of storage to discharge)
!!    prog          |NA          |program name and version
!!    res_esa(:)    |ha          |reservoir surface area when reservoir is
!!                               |filled to emergency spillway
!!    res_evol(:)   |m**3        |volume of water needed to fill the reservoir
!!                               |to the emergency spillway
!!    res_k(:)      |mm/hr       |hydraulic conductivity of the reservoir bottom
!!    res_nsed(:)   |kg/kg       |normal amount of sediment in reservoir
!!    res_psa(:)    |ha          |reservoir surface area when reservoir is
!!                               |filled to principal spillway
!!    res_pvol(:)   |m**3        |volume of water needed to fill the reservoir
!!                               |to the principal spillway
!!    res_rr(:)     |m**3/day    |average daily principal spillway release
!!                               |volume
!!    res_sed(:)    |kg/kg       |amount of sediment in reservoir
!!    res_vol(:)    |m**3        |reservoir volume
!!    resvo(:)      |NA          |reservoir file names (.res)
!!    subtot        |none        |number of subbasins in watershed
!!    title         |NA          |description lines in file.cio(1st 3 lines)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL VARIABLES ~ ~ ~
!!    name          |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j             |none        |counter
!!    kbnd          |NA          |elevation bands in subbasin
!!    kdrn          |NA          |drainage tiles in HRU
!!    kpnd          |NA          |pond in subbasin
!!    kpot          |NA          |pothole in HRU
!!    kpst          |NA          |pesticide in HRU
!!    kubn          |NA          |urban area simulated in HRU
!!    kwet          |NA          |wetlands in subbasin
!!    sumarea       |none        |sum of all hru_fr
!!    xch_l1        |km          |length of tributary channel
!!    xkm           |hr          |storage time constant for the reach on
!!                               |current day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm
 
      integer :: j
      real :: sumarea, xkm, xch_l1
      character (len=1) :: kpnd, kbnd, kwet, kubn, kpot, kdrn, kpst,ksep

!!    input summary file
        !!write subbasin info
        write (24,1000)
        do j = 1, subtot
          kpnd = ""
          kbnd = ""
          kwet = ""
          if (pnd_fr(hru1(j)) > 0.) then
            kpnd = "x"
          else
            kpnd = " "
          end if
          if (elevb_fr(1,j) > 0.) then
            kbnd = "x"
          else
            kbnd = " "
          end if
          if (wet_fr(hru1(j)) > 0.) then
            kwet = "x"
          else
            kwet = " "
          end if
          write (24,1001)j, sub_lat(hru_sub(j)), sub_elev(j), hrutot(j),
     *         kpnd, kbnd, kwet

        end do

        !! write HRU info
        write (24,1020)
        do j = 1, nhru
          kubn = " "
          kdrn = " "
          kpot = " "
          if (iurban(j) > 0) kubn = "x"
     !!     if (ipot(j) == j) kpot = "x"
          if (pot_fr(j) > 0.) kpot = "x"    
          if (ddrain(j) > 0.) kdrn = "x"
          write (24,1021)hru_sub(j),j,hru_ha(j),hru_slp(j),slsubbsn(j), 
     &          ov_n(j),cn2(j),tconc(j),esco(j),epco(j)
        end do
        
        write (24,1028)
        do j = 1, nhru
	if (idplt(j) > 0) then
       write (24,1029) hru_sub(j), j, hru_ha(j),cpnm(idplt(j)),snam(j), 
     &  cn1(j), cn2(j), cn3(j), sol_sumwp(j),sol_sumfc(j), sol_sumul(j)
      else
        write (24,1030)hru_sub(j),j,hru_ha(j), snam(j), cn1(j), cn2(j), 
     &   cn3(j), sol_sumwp(j), sol_sumfc(j), sol_sumul(j)
	end if
        end do

        write (24,1022)
        do j = 1, nhru
          write (24,1023) hru_sub(j),j,hru_ha(j),snam(j),hydgrp(j),     
     &          sol_zmx(j),sol_alb(j),usle_k(j),usle_p(j),usle_ls(j),   
     &          sol_sumfc(j),sol_sw(j)
        end do
        write (24,1024)
        do j = 1, nhru
          kubn = " "
          kdrn = " "
          kpot = " "
          kpst = " "
          ksep = "   "
          if (iurban(j) > 0) kubn = "x"
       !!   if (ipot(j) == j) kpot = "x"
          if (pot_fr(j) > 0.) kpot = "x"
          if (ddrain(j) > 0.) kdrn = "x"
          if (hrupest(j) > 0.) kpst = "x"
          if (isep_hru(j) == 1) ksep = "x"

          write (24,1025) hru_sub(j),j,hru_ha(j),kubn,kirr(j),kdrn,kpot,
     &          kpst,biomix(j),ksep
        end do
        write (24,1026)
        do j = 1, nhru
          write (24,1027) hru_sub(j),j,hru_ha(j),delay(j),alpha_bf(j),  
     &          gwqmn(j),gw_revap(j),revapmn(j),rchrg_dp(j),gwno3(j),   
     &          gwminp(j)
        end do

      !! write reach characteristics
      write (24,1010)
      if (irte == 1) then
        write (24,1013)
        do j = 1, subtot
          xmk = 0.
          xkm = phi(10,j) * msk_co1 + phi(13,j) * msk_co2
          xch_l1 = 0.
          xch_l1 = ch_l1(hru1(j)) / hru_fr(hru1(j))
          write (24,1014) j,xch_l1,ch_s(1,j),ch_w(1,j),ch_k(1,j)        
     &          ,ch_n(1,j),ch_l2(j),ch_s(2,j),ch_w(2,j),ch_d(j),        
     &          ch_k(2,j),ch_n(2,j),xkm
        end do
      else
        write (24,1011)
        do j = 1, subtot
          xch_l1 = 0.
          xch_l1 = ch_l1(hru1(j)) / hru_fr(hru1(j))
          write (24,1012) j,xch_l1,ch_s(1,j),ch_w(1,j),ch_k(1,j)        
     &          ,ch_n(1,j),ch_l2(j),ch_s(2,j),ch_w(2,j),ch_d(j),        
     &          ch_k(2,j),ch_n(2,j)
        end do
      end if

!!    standard output file
      sumarea = 0.
      sumarea = Sum(hru_fr)
      !if (sumarea < .9999) write (26,2000) sumarea
 
!!    chan.deg file
      do j = 1, nrch
        write (16,3000) j, ch_d(j), ch_w(2,j), ch_s(2,j)
      end do

      return
 1000 format (/t10,'Subbasin Input Summary:',/t10,                      
     &       ' Sub Latitude  Elev(m) #HRUs Ponds Elevbnds Wetlnd')
 1001 format (t10,i4,2f9.2,i6,3x,a1,6x,a1,7x,a1,2x)
 1010 format (/t10,'Tributary/Main Channel Characteristics')
 1011 format (t10,'     |--------------Tributary--------------------|', 
     &          '---------------------Main---------------------------|',
     &       /t10,' Sub  Length(km) Slope Width(m) Cond(mm/hr)     N',  
     &       1x,'Length(km) Slope Width(m) Depth(m) Cond(mm/hr)     N')
 1012 format (t10,i4,2x,f10.2,f6.3,f9.2,f12.4,1x,f5.3,1x,f10.2,f6.3,    
     &        2f9.2,f12.4,1x,f5.3)
 1013 format (t10,'     |--------------Tributary--------------------|', 
     &          '---------------------Main---------------------------', 
     &          '----------|',                                    
     &       /t10,' Sub  Length(km) Slope Width(m) Cond(mm/hr)     N',  
     &       1x,'Length(km) Slope Width(m) Depth(m) Cond(mm/hr)     N', 
     &       '      MSK_K')
 1014 format (t10,i6,2x,f10.2,f6.3,f9.2,f12.4,1x,f5.3,1x,f10.2,f6.3,    
     &        2f9.2,f12.4,1x,f5.3,1x,f8.2)
 1020 format (/t10,'HRU Input Summary Table 1:',/t10,                   
     &       ' Sub   HRU   Area(ha) Slope SlpLgth(m) Ovrlnd_N CondII_CN'
     &       ,' TimeConc(hr) ESCO EPCO')
 1021 format (t10,i4,1x,i6,f11.2,f6.3,f11.2,f9.3,f10.2,f13.3,2f5.2)
 1022 format (/t10,'HRU Input Summary Table 2:',/t10,                   
     &       ' Sub   HRU   Area(ha)         SoilName Hydgrp MaxRtDpth', 
     &       '(mm) Albedo USLE_K USLE_P USLE_LS ProfileAWC(mm) IniSoil',
     &       'H2O(mm)')
 1023 format (t10,i4,1x,i6,f11.2,1x,a16,3x,a1,3x,f14.2,3f7.2,f8.2,f11.3,
     &       f15.3)
 1024 format (/t10,'HRU Input Summary Table 3:',/t10,                   
     &       ' Sub   HRU   Area(ha) Urban Irrig DrainTiles Pothole',    
     &       ' Pstcide Biomix Septic')
 1025 format (t10,i4,1x,i6,f11.2,3x,a1,5x,a1,8x,a1,8x,a1,6x,a1,5x,f6.2, 
     & 1x,a1)
 1026 format (/t10,'HRU Input Summary Table 4 (Groundwater):',/t10,     
     &       ' Sub   HRU   Area(ha) GWdelay(days), GWalpha(days)',      
     &       ' GWQmin(mm) GWrevap Revapmin(mm) Deepfr NO3(ppm)',        
     &       ' SolP(ppm)')
 1027 format (t10,i4,1x,i6,f11.2,2f14.3,f11.3,f8.3,f13.3,f7.3,f9.3,     
     &       f10.3)
 1028 format (/t10,'HRU CN Input Summary Table:',/t10,                  
     &       ' Sub   HRU   Area(ha)  LULC      Soil    CN1    CN2    ', 
     &       'CN3  Wilting Point (mm H2O)  Field Capacity (mm H2O)',    
     &       '  Saturation (mm H2O)')
 1029 format (t10,i4,1x,i6,f11.2,2x,a4,2x,a8,3f7.1,18x,f6.1,19x,f6.1,   
     &       15x,f6.1)
 1030 format (t10,i4,1x,i6,f11.2,2x,'BARR',2x,a8,3f7.1,18x,f6.1,19x,f6.1
     &       ,15x,f6.1)
 2000 format (//,' AREA DOES NOT SUM TO ONE   ', f8.6,//)
 3000 format (i4,3f12.4)
 5000 format (/t5,a80,t105,2(a2,'/'),a2,5x,2(i2,':'),i2)
      end