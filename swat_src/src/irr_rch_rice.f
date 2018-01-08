      subroutine irr_rch_rice

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine performs the irrigation operation for paddy rice,
!!    the watersource is pond and reach

      use parm

      integer :: jrch, j
      real :: cnv, vmm, vminmm, vol, wtrin
      ! total amount of water in ponds of the subbasin (m3)
      real :: pnd_vol_tot
      ! amount of available irrigation water in the reach (m3)
      real :: rch_vol_avail
      ! total irrigation water demand in the subbasin(m3)
      real :: irr_vol_demand

      ! local variable
      ! amount of irrigation water demand in each hru (m3)
      real :: irr_vol_demand_hru
      ! total area of paddy rice in the subbasin (ha)
      real :: rice_area_tot
      ! average water depth in rice field that cannot be satisfied by irrigation (mm)
      real :: depth_deficit
      ! total amount of irragation (mm)
      real :: irri_vol_act
      ! temporal variable for actural irrigation
      real :: xx, irr_from_pnd, irr_from_rch, irr_from_rt, fr_irr_pnd

      jrch = 0
      jrch = inum1

      !! calculate the total amount of water in ponds within the subbasin, by Junzhi Liu, 2017-12-04
      pnd_vol_tot = 0.0
      do j = hru1(jrch), hru1(jrch) + hrutot(jrch) - 1
          pnd_vol_tot = pnd_vol_tot + pnd_vol(j)
      end do
      !! end the calculation

      ! the total amount of water in reach before flowout
      wtrin = 0.
      wtrin = rtwtr + rchstor(jrch)
      !! compute maximum amount of water available for irrigation
      !! from reach
      rch_vol_avail = wtrin - flowmin(jrch) * 86400.

      !! loop to calculate the total amount of irrigation water requirement in paddy rice
      irr_vol_demand = 0.0
      rice_area_tot = 0.0
      do j = hru1(jrch), hru1(jrch) + hrutot(jrch) - 1
          ! only for impound paddy rice
          if (idplt(j) == 33 .and. imp_trig(j) == 0) then
              irr_vol_demand_hru = 0.0
               !! auto irragation
              if ( pot_vol(j) < prfit_min(j) ) then
                  irr_vol_demand_hru = ( prfit_max(j) - pot_vol(j) ) * hru_ha(j) * 10.
                  irr_vol_demand = irr_vol_demand + irr_vol_demand_hru
                  rice_area_tot = rice_area_tot + hru_ha(j)
              end if
          end if
      end do

      if (irr_vol_demand <= 0) return

      ! if the irrigation water demand cannot be satisfied,
      ! calculate the average water depth in rice field that cannot be satisfied by irrigation (mm)
      depth_deficit = 0.0
      irri_vol_act = irr_vol_demand
      if (irr_vol_demand > pnd_vol_tot + rch_vol_avail) then
          depth_deficit = (irr_vol_demand - pnd_vol_tot - rch_vol_avail) / (rice_area_tot * 10.0)
          irri_vol_act = pnd_vol_tot + rch_vol_avail
      end if
      ! raise the depth of water layer to prfit_max
      do j = hru1(jrch), hru1(jrch) + hrutot(jrch) - 1
          if (idplt(j) == 33 .and. imp_trig(j) == 0) then
              pot_vol(j) = prfit_max(j) - depth_deficit
          end if
      end do

      ! substract water first from pond
      xx = irri_vol_act
      irr_from_pnd = 0.0
      if (xx < pnd_vol_tot) then
          irr_from_pnd = xx
          xx = 0.0
      else
          irr_from_pnd = pnd_vol_tot
          xx = xx - irr_from_pnd
      end if

      if (pnd_vol_tot > 0)  then
          fr_irr_pnd = irr_from_pnd * 1.0 / pnd_vol_tot
          ! subtract water in each pond according to its volume
          do j = hru1(jrch), hru1(jrch) + hrutot(jrch) - 1
              pnd_vol(j) = pnd_vol(j) * (1 - fr_irr_pnd)
          end do
       end if

      ! from reach
      ! if remaining demand still larger than reach storage
      if (xx > rchstor(jrch)) then
           xx = vol - rchstor(jrch)
           rchstor(jrch) = 0.
      else
           rchstor(jrch) = rchstor(jrch) - xx
           xx = 0.
      end if

      ! if there are still remaining demand, take from flowout
      if (xx > 0.) then
           rtwtr = rtwtr - xx
           rtwtr = amax1(0., rtwtr)
      end if

      if (wtrin /= rtwtr .and. wtrin > 0.01) then
        sedrch = sedrch * rtwtr / wtrin

        rch_san = rch_san * rtwtr / wtrin
        rch_sil = rch_sil * rtwtr / wtrin
        rch_cla = rch_cla * rtwtr / wtrin
        rch_sag = rch_sag * rtwtr / wtrin
        rch_lag = rch_lag * rtwtr / wtrin
        rch_gra = rch_gra * rtwtr / wtrin

        if (sedrch  < 1.e-6) then
        sedrch = 0.
        rch_san = 0.
          rch_sil = 0.
          rch_cla = 0.
          rch_sag = 0.
          rch_lag = 0.
          rch_gra = 0.
      end if

        if (ievent > 0) then
          do ii = 1, nstep
            hsedyld(ii) = hsedyld(ii) * rtwtr / wtrin
          end do
        end if
      end if

      return
      end