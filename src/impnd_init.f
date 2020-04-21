      subroutine impnd_init


!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables related to impoundments (ponds,
!!    wetlands, reservoirs and potholes)


!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    da_ha       |ha            |drainage area of watershed in hectares
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    hru_fr(:)   |none          |fraction of subbasin area in HRU
!!    nhru        |none          |number of HRUs in watershed
!!    pnd_esa(:)  |ha            |surface area of ponds when filled to the 
!!                               |emergency spillway
!!    pnd_evol(:) |10^4 m^3 H2O  |runoff volume from catchment area needed to
!!                               |fill the ponds to the emergency spillway
!!    pnd_fr(:)   |none          |fraction of HRU/subbasin that drains into
!!                               |pond
!!    pnd_nsed(:) |mg/kg         |normal sediment concentration in pond water
!!    pnd_psa(:)  |ha            |surface area of ponds when filled to 
!!                               |principal spillway
!!    pnd_pvol(:) |10^4 m^3 H2O  |runoff volume from catchment area needed to
!!                               |fill the ponds to the principal spillway
!!    pnd_sed(:)  |mg/kg         |sediment concentration in pond water
!!    pnd_vol(:)  |10^4 m^3 H2O  |volume of water in pond at any given time
!!    sol_silt(:,:)|%             |percent silt content in soil material
!!    sol_clay(:,:)|%             |percent clay content in soil material
!!    sub_fr(:)   |none          |fraction of watershed area in subbasin
!!    wet_fr(:)   |none          |fraction of HRU/subbasin area that drains
!!                               |into wetlands
!!    wet_mxsa(:) |ha            |surface area of wetlands at maximum water
!!                               |level
!!    wet_mxvol(:)|10^4 m^3 H2O  |runoff volume from catchment area needed to 
!!                               |fill wetlands to maximum water level
!!    wet_nsa(:)  |ha            |surface area of wetlands at normal water
!!                               |level
!!    wet_nsed(:) |mg/kg         |normal sediment concentration in wetland water
!!    wet_nvol(:) |10^4 m^3 H2O  |runoff volume from catchment area needed to
!!                               |fill wetlands to normal water level
!!    wet_sed(:)  |mg/L          |sediment concentration in wetland water
!!    wet_vol(:)  |10^4 m^3 H2O  |volume of water in wetlands
!!    wshd_resfr  |none          |fraction of watershed area that drains into
!!                               |reservoirs
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bp1(:)      |none          |1st shape parameter for pond surface area
!!                               |equation
!!    bp2(:)      |none          |2nd shape parameter for the pond surface area
!!                               |equation
!!    bw1(:)      |none          |1st shape parameter for wetland surface area
!!                               |equation
!!    bw2(:)      |none          |2nd shape parameter for the wetland surface
!!                               |area equation
!!    pnd_evol(:) |m^3 H2O       |runoff volume from catchment area needed to
!!                               |fill the ponds to the emergency spillway
!!                               |(UNIT CHANGE!)
!!    pnd_nsed(:) |kg/kg         |normal ratio of sediment to water in pond
!!                               |(UNIT CHANGE!)
!!    pnd_pvol(:) |m^3 H2O       |runoff volume from catchment area needed to
!!                               |fill the ponds to the principal spillway
!!                               |(UNIT CHANGE!)
!!    pnd_sed(:)  |kg/kg         |ratio of sediment to water in pond
!!                               |(UNIT CHANGE!)
!!    pnd_vol(:)  |m^3 H2O       |volume of water in pond at any given time
!!                               |(UNIT CHANGE!)
!!    sed_stl(:)  |kg/kg         |fraction of sediment remaining suspended in
!!                               |impoundment after settling for one day 
!!    sol_silt(:) |none          |fraction silt content in soil material
!!                               |(UNIT CHANGE!)
!!    sol_clay(:,:)|none          |fraction clay content in soil material
!!                               |(UNIT CHANGE!)
!!    wet_mxvol(:)|m^3 H2O       |runoff volume from catchment area needed to 
!!                               |fill wetlands to maximum water level
!!                               |(UNIT CHANGE!)
!!    wet_nsed(:) |kg/kg         |ratio of sediment to water in wetland 
!!                               |(UNIT CHANGE!)
!!    wet_nvol(:) |m^3 H2O       |runoff volume from catchment area needed to
!!                               |fill wetlands to normal water level
!!                               |(UNIT CHANGE!)
!!    wet_sed(:)  |kg/L          |ratio of sediment to water in wetland
!!                               |(UNIT CHANGE!)
!!    wet_vol(:)  |m^3 H2O       |volume of water in wetlands (UNIT CHANGE!)
!!    wshd_pndfr  |none          |fraction of watershed area which drains into
!!                               |ponds
!!    wshd_pndha  |ha            |watershed area in hectares which drains into
!!                               |ponds
!!    wshd_pndsed |metric tons   |total amount of suspended sediment in ponds
!!                               |in the watershed
!!    wshd_pndv   |m^3           |total volume of water in ponds in the 
!!                               |watershed
!!    wshd_resha  |ha            |watershed area in hectares which drains into
!!                               |reservoirs
!!    wshd_wetfr  |none          |fraction of watershed area which drains into
!!                               |wetlands
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    cl          |none          |variable to hold calculation result
!!    j           |none          |counter
!!    mnpsz       |none          |mean particle size
!!    lnvol       |none          |variable to hold denominator value
!!    pe_sa       |ha            |local variable to hold value for pnd_esa(:)
!!    pe_vo       |m^3           |local variable to hold value for pnd_evol(:)
!!    pp_sa       |ha            |local variable to hold value for pnd_psa(:)
!!    pp_vo       |m^3           |local variable to hold value for pnd_pvol(:)
!!    sol_sand(:,:) |none          |fraction of sand in soil material
!!    si          |none          |variable to hold calculation result
!!    targ        |10^4 m^3 H2O  |target pond volume
!!    wetdif      |m^3           |difference between maximum and normal amounts
!!                               |of water held in wetlands
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Exp, Log10
!!
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j
      real :: cl, si, mnpsz, targ, lnvol
      real :: pe_sa, pp_sa, pe_vo, pp_vo, wetdif


      do j = 1, nhru

        !! calculate the sediment settling rate
        cl = 0.
        si = 0.
        sa = 0.
        mnpsz = 0.
        cl = 0.4100 * sol_clay(1,j) / 100.
        si = 2.7100 * sol_silt(1,j) / 100.
        sa = 5.7000 * sol_sand(1,j) / 100.
        mnpsz = Exp(cl + si + sa)
        sed_stl(j) = Exp(-res_stlr_co * mnpsz)


!!      set initial pond/wetland parameters
        if (pnd_fr(j) /= 0.) then

          !! fill in missing parameters
         
          if (pnd_evol(j) + pnd_pvol(j) > 0.) then
            if (pnd_evol(j) <= 0.) pnd_evol(j) = 1.11 * pnd_pvol(j)
            if (pnd_pvol(j) <= 0.) pnd_pvol(j) = .9 * pnd_evol(j)
            if (pnd_psa(j) <= 0.) pnd_psa(j) = 0.08 * pnd_pvol(j)
            if (pnd_esa(j) <= 0.) pnd_esa(j) = 1.5 * pnd_psa(j)
            targ = 0
            targ = pnd_pvol(j) + .1 * (pnd_evol(j) - pnd_pvol(j))
            if (pnd_vol(j) > targ) pnd_vol(j) = targ
            
            !! convert to new units
            pnd_sed(j) = pnd_sed(j) * 1.e-6     !! mg/L => tons/m^3

            pnd_san(j) = pnd_sed(j) * 1.e-6 * 0.      !! mg/L => tons/m^3
            pnd_sil(j) = pnd_sed(j) * 1.e-6 * 1.      !! mg/L => tons/m^3
            pnd_cla(j) = pnd_sed(j) * 1.e-6 * 0.      !! mg/L => tons/m^3
            pnd_sag(j) = pnd_sed(j) * 1.e-6 * 0.      !! mg/L => tons/m^3
            pnd_lag(j) = pnd_sed(j) * 1.e-6 * 0.      !! mg/L => tons/m^3

            pnd_nsed(j) = pnd_nsed(j) * 1.e-6   !! mg/L => tons/m^3
            pnd_pvol(j) = 10000. * pnd_pvol(j)  !! 10^4 m^3 => m^3
            pnd_evol(j) = 10000. * pnd_evol(j)  !! 10^4 m^3 => m^3
            pnd_vol(j) = 10000. * pnd_vol(j)    !! 10^4 m^3 => m^3

            !! calculate watershed pond values
            wshd_pndfr = wshd_pndfr + (hru_dafr(j) * pnd_fr(j))
            wshd_pndv = wshd_pndv + pnd_vol(j)
            wshd_pndsed = wshd_pndsed + pnd_vol(j) * pnd_sed(j)
            
            !! calculate shape parameters for surface area equation
            pe_sa = 0.
            pp_sa = 0.
            pp_vo = 0.
            pe_vo = 0.
            pp_vo = pnd_pvol(j)
            pe_vo = pnd_evol(j)
            pe_sa = pnd_esa(j)
            pp_sa = pnd_psa(j)
            if ((pe_sa - pp_sa) > 0. .and. (pe_vo - pp_vo) > 0.) then
              lnvol = 0.        
              lnvol = Log10(pe_vo) - Log10(pp_vo)
              if (lnvol > 1.e-4) then
                bp2(j) = (Log10(pe_sa) - Log10(pp_sa)) / lnvol
              else
                bp2(j) = (Log10(pe_sa) - Log10(pp_sa)) / 0.001
              end if
              if (bp2(j) > .9) then
                bp2(j) = .9
                bp1(j) = (pnd_psa(j) / pnd_pvol(j)) ** .9
              else
                bp1(j) = (pnd_esa(j) / pnd_evol(j)) ** bp2(j)
              endif
            else
              bp2(j) = .9
              bp1(j) = (pnd_psa(j) / pnd_pvol(j)) ** .9
            end if

          else
            pnd_fr(j) = 0.
          end if
 
          !! partition pond volume/surface areas between HRUS in subbasin
          pnd_psa(j) = pnd_psa(j) * hru_fr(j) 
          pnd_esa(j) = pnd_esa(j) * hru_fr(j) 
          pnd_pvol(j) = pnd_pvol(j) * hru_fr(j)
          pnd_evol(j) = pnd_evol(j) * hru_fr(j)
          pnd_vol(j) = pnd_vol(j) * hru_fr(j)

        else
          pnd_vol(j) = 0.
        end if



!!      set initial wetland parameters
        if (wet_fr(j) /= 0.) then

          !! fill in missing parameters
          if (wet_mxvol(j) + wet_nvol(j) > 0.) then
            if (wet_mxvol(j) <= 0.) wet_mxvol(j) = 1.11 * wet_nvol(j)
            if (wet_nvol(j) <= 0.) wet_nvol(j) = .9 * wet_mxvol(j)
            if (wet_nsa(j) <= 0.) wet_nsa(j) = .08 * wet_nvol(j)
            if (wet_mxsa(j) <= 0.) wet_mxsa(j) = 1.5 * wet_nsa(j)
            if (wet_vol(j) <= 0.) wet_vol(j) = wet_nvol(j)
            if (wet_vol(j) > wet_mxvol(j)) wet_vol(j) = wet_mxvol(j)

            !! unit conversions
            wet_sed(j) = wet_sed(j) * 1.e-6        !! mg/L => kg/L

            wet_san(j) = wet_sed(j) * 1.e-6 * 0.         !! mg/L => kg/L
            wet_sil(j) = wet_sed(j) * 1.e-6 * 1.         !! mg/L => kg/L
            wet_cla(j) = wet_sed(j) * 1.e-6 * 0.         !! mg/L => kg/L
            wet_sag(j) = wet_sed(j) * 1.e-6 * 0.         !! mg/L => kg/L
            wet_lag(j) = wet_sed(j) * 1.e-6 * 0.         !! mg/L => kg/L

            wet_nsed(j) = wet_nsed(j) * 1.e-6      !! mg/L => kg/L
            wet_nvol(j) = 10000. * wet_nvol(j)     !! 10^4 m^3 => m^3
            wet_mxvol(j) = 10000. * wet_mxvol(j)   !! 10^4 m^3 => m^3
            wet_vol(j) = 10000. * wet_vol(j)       !! 10^4 m^3 => m^3

            !! calculate watershed variables
            wshd_wetfr = wshd_wetfr + (hru_dafr(j) * wet_fr(j))

            !! calculate shape parameters for surface area equation
            wetdif = 0.
            wetdif = wet_mxvol(j) - wet_nvol(j)
            if ((wet_mxsa(j) - wet_nsa(j)) > 0. .and. wetdif > 0.) then
              lnvol = 0.
              lnvol = Log10(wet_mxvol(j)) - Log10(wet_nvol(j))
              if (lnvol > 1.e-4) then
               bw2(j) = (Log10(wet_mxsa(j)) - Log10(wet_nsa(j))) / lnvol
              else
               bw2(j) = (Log10(wet_mxsa(j)) - Log10(wet_nsa(j))) / 0.001
              end if
              if (bw2(j) > 0.9) bw2(j) = .9
              bw1(j) = (wet_mxsa(j) / wet_mxvol(j)) ** bw2(j)
            else
              bw2(j) = .9
              bw1(j) = (wet_nsa(j) / wet_nvol(j)) ** .9
            end if
          else
            wet_fr(j) = 0.
          end if


          !! partition wetland volume/surface areas between HRUS in subbasin
          wet_nsa(j) = wet_nsa(j) * hru_fr(j)
          wet_mxsa(j) = wet_mxsa(j) * hru_fr(j)
          wet_nvol(j) = wet_nvol(j) * hru_fr(j)
          wet_mxvol(j) = wet_mxvol(j) * hru_fr(j)
          wet_vol(j) = wet_vol(j) * hru_fr(j)

        else
          wet_vol(j) = 0.
        end if

      end do

!!    initialize watershed variables
      wshd_resha = wshd_resfr * da_ha
      wshd_pndha = wshd_pndfr * da_ha

      return
      end
