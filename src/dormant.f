      subroutine dormant

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the dormant status of the different plant types

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    alai_min(:)    |m**2/m**2     |minimum LAI during winter dormant period
!!    bio_leaf(:)    |none          |fraction of biomass that drops during
!!                                  |dormancy (for trees only)
!!    bio_ms(:)      |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_yrms(:)    |metric tons/ha|annual biomass (dry weight) in the HRU
!!    dayl(:)        |hours         |day length for current day
!!    daylmn(:)      |hours         |shortest daylength occurring during the
!!                                  |year
!!    dormhr(:)      |hour          |time threshold used to define dormant
!!                                  |period for plant (when daylength is within
!!                                  |the time specified by dormhr from the minimum
!!                                  |daylength for the area, the plant will go
!!                                  |dormant)
!!    icr(:)         |none          |sequence number of crop grown within the
!!                                  |current year
!!    idc(:)         |none          |crop/landcover category:
!!                                  |1 warm season annual legume
!!                                  |2 cold season annual legume
!!                                  |3 perennial legume
!!                                  |4 warm season annual
!!                                  |5 cold season annual
!!                                  |6 perennial
!!                                  |7 trees
!!    idorm(:)       |none          |dormancy status code:
!!                                  |0 land cover growing
!!                                  |1 land cover dormant
!!    idplt(:)       |none          |land cover code from crop.dat
!!    ihru           |none          |HRU number
!!    nro(:)         |none          |sequence number for year in rotation
!!    phuacc(:)      |none          |fraction of plant heat units accumulated
!!    plantn(:)      |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)      |kg P/ha       |amount of phosphorus in plant biomass
!!    pltfr_n(:)     |none          |fraction of plant biomass that is nitrogen
!!    pltfr_p(:)     |none          |fraction of plant biomass that is phosphorus
!!    sol_fon(:,:)   |kg N/ha       |amount of nitrogen stored in the fresh
!!                                  |organic (residue) pool
!!    sol_fop(:,:)   |kg P/ha       |amount of phosphorus stored in the fresh
!!                                  |organic (residue) pool
!!    sol_rsd(:,:)   |kg/ha         |amount of organic matter in the soil
!!                                  |classified as residue
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    laiday(:)   |m**2/m**2     |leaf area index
!!    bio_ms(:)   |kg/ha         |land cover/crop biomass (dry weight)
!!    bio_yrms(:) |metric tons/ha|annual biomass (dry weight) in the HRU
!!    idorm(:)    |none          |dormancy status code:
!!                               |0 land cover growing
!!                               |1 land cover dormant
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
!!    plantp(:)   |kg P/ha       |amount of phosphorus in plant biomass
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool
!!    sol_rsd(:,:)|kg/ha         |amount of organic matter in the soil
!!                               |classified as residue
!!    strsw(:)    |none          |fraction of potential plant growth achieved
!!                               |on the day where the reduction is caused by
!!                               |water stress
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    resnew      |
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Max

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real :: resnew
      integer :: j

      j = 0
      j = ihru


!! check for beginning of dormant season
      if (idorm(j) == 0 .and. dayl(j)-dormhr(j) < daylmn(hru_sub(j)))   &
     &                                                              then

        select case (idc(idplt(j)))
        
          !! make sure all operations are scheduled during growing season of warm season annual
          case (1,4)
            dorm_flag = 1
            call operatn
            dorm_flag = 0

          !! beginning of forest dormant period
          case (7)
            idorm(j) = 1
            resnew = 0.
            resnew = bio_ms(j) * bio_leaf(idplt(j))
            sol_rsd(1,j) = sol_rsd(1,j) + resnew
            sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
            sol_fon(1,j) = resnew * pltfr_n(j) + sol_fon(1,j)
            sol_fop(1,j) = resnew * pltfr_p(j) + sol_fop(1,j)
            bio_hv(icr(j),j) = bio_ms(j) + bio_hv(icr(j),j)
            bio_yrms(j) = bio_yrms(j) + bio_ms(j) / 1000.
            bio_ms(j) = bio_ms(j) * (1. - bio_leaf(idplt(j)))
            plantn(j) = plantn(j) - resnew * pltfr_n(j)
            plantp(j) = plantp(j) - resnew * pltfr_p(j)
            strsw(j) = 1.
            laiday(j) = alai_min(idplt(j))
            phuacc(j) = 0.
            laimxfr(j) = 0.        !Sue White - dormancy
            ncrops(icr(j),j) = ncrops(icr(j),j) + 1

          !! beginning of perennial (pasture/alfalfa) dormant period
          case (3, 6)
            idorm(j) = 1
            resnew = 0.
            resnew = bm_dieoff(idplt(j)) * bio_ms(j)
            sol_rsd(1,j) = sol_rsd(1,j) + resnew
            sol_rsd(1,j) = Max(sol_rsd(1,j),0.)
            sol_fon(1,j) = sol_fon(1,j) +                               &
     &         bm_dieoff(idplt(j)) * plantn(j)
            sol_fop(1,j) = sol_fop(1,j) +                               &
     &         bm_dieoff(idplt(j)) * plantp(j)
            bio_hv(icr(j),j) = bio_ms(j) *                              & 
     &        bm_dieoff(idplt(j)) +                                     &
     &	    bio_hv(icr(j),j)
            bio_yrms(j) = bio_yrms(j) + bio_ms(j) *                     &
     &         bm_dieoff(idplt(j)) / 1000.
            bio_ms(j) = (1. - bm_dieoff(idplt(j))) *                    &
     &         bio_ms(j)
            plantn(j) = (1. - bm_dieoff(idplt(j))) *                    &
     &         plantn(j)
            plantp(j) = (1. - bm_dieoff(idplt(j))) *                    &
     &         plantp(j)
            strsw(j) = 1.
            laiday(j) = alai_min(idplt(j))
            phuacc(j) = 0.
            ncrops(icr(j),j) = ncrops(icr(j),j) + 1

          !! beginning of cool season annual dormant period
          case (2, 5)
            if (phuacc(j) < 0.75) then
              idorm(j) = 1
              strsw(j) = 1.
            end if

          end select
          if (imgt == 1) then
           write (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, 
     *     cpnm(idplt(j)),"START-DORM", phubase(j), phuacc(j), 
     *     sol_sw(j),bio_ms(j), sol_rsd(1,j), sol_sumno3(j),
     *     sol_sumsolp(j)
          endif
      end if


!! check if end of dormant period
        if (idorm(j) == 1 .and. dayl(j)-dormhr(j) >= daylmn(hru_sub(j)))&
     &                                                              then

          select case (idc(idplt(j)))
          
            !! end of perennial dormant period
            case (3, 6, 7)
              idorm(j) = 0

            !! end of cool season annual dormant period
            case (2, 5)
              idorm(j) = 0

            end select
            
            if (imgt == 1) then
                 write (143,1000) subnum(j), hruno(j), iyr, i_mo, iida, 
     *       cpnm(idplt(j)), "END-DORM", phubase(j), phuacc(j), 
     *       sol_sw(j), bio_ms(j), sol_rsd(1,j), sol_sumno3(j),
     *       sol_sumsolp(j)
           end if

        end if

1000  format (a5,1x,a7,3i6,2a15,7f10.2)
      return
      end
