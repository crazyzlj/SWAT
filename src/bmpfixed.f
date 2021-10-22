      subroutine bmpfixed
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine applies fixed removal eff. from the .ops to upland loads 
                  
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name          |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru          |none          |HRU number
!!    bmp_sed(:)    |%             | Sediment removal by BMP
!!    bmp_pp(:)     |%             | Particulate P removal by BMP
!!    bmp_sp(:)     |%             | Soluble P removal by BMP  
!!    bmp_pn(:)     |%             | Particulate N removal by BMP  
!!    bmp_sn(:)     |%             | Soluble N removal by BMP  
!!    bmp_bac(:)    |%             | Bacteria removal by BMP  
!!    sedyld(:)     |metric tons   |daily soil loss caused by water erosion
!!    minpgw(:)     |kg P/ha       |soluble P loading to reach in groundwater
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedorgp(:)    |kg P/ha       |amount of organic phosphorus in surface
!!                                 |runoff in HRU for the day
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for
!!    surqno3(:)    |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                                 |the day
!!    sedorgn(:)   |kg N/ha        |amount of organic nitrogen in surface runoff
!!    no3gw(:)     |kg N/ha        |nitrate loading to reach in groundwater
!!                                 |in HRU for the day
!!    bactrolp      |# cfu/m^2     |less persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactrop       |# cfu/m^2     |persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactsedlp     |# cfu/m^2     |less persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bactsedp      |# cfu/m^2     |persistent bacteria transported with
!!                                 |sediment in surface runoff

!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    sedyld(:)     |metric tons   |daily soil loss caused by water erosion
!!    minpgw(:)     |kg P/ha       |soluble P loading to reach in groundwater
!!    sedminpa(:)   |kg P/ha       |amount of active mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedminps(:)   |kg P/ha       |amount of stable mineral phosphorus sorbed
!!                                 |to sediment in surface runoff in HRU for day
!!    sedorgp(:)    |kg P/ha       |amount of organic phosphorus in surface
!!                                 |runoff in HRU for the day
!!    surqsolp(:)   |kg P/ha       |amount of soluble phosphorus in surface
!!                                 |runoff in HRU for the day
!!    latno3(:)     |kg N/ha       |amount of NO3-N in lateral flow in HRU for
!!    surqno3(:)    |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                                 |the day
!!    sedorgn(:)   |kg N/ha        |amount of organic nitrogen in surface runoff
!!    no3gw(:)     |kg N/ha        |nitrate loading to reach in groundwater
!!                                 |in HRU for the day
!!    bactrolp      |# cfu/m^2     |less persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactrop       |# cfu/m^2     |persistent bacteria transported to main
!!                                 |channel with surface runoff
!!    bactsedlp     |# cfu/m^2     |less persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    bactsedp      |# cfu/m^2     |persistent bacteria transported with
!!                                 |sediment in surface runoff
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

!!	set variables
      j = ihru


!! Subtract reductions from sediment, nutrients, bacteria, NOT SURFACE RUNOFF to protect water balance
!! Sediment
	sedyld(j) = sedyld(j) * bmp_sed(j)

!! Phosphorus
      !! Particulate
	sedminpa(j) = sedminpa(j) * bmp_pp(j)
	sedminps(j) = sedminps(j) * bmp_pp(j)
	sedorgp(j) = sedorgp(j) * bmp_pp(j)
      !! Soluble
	surqsolp(j) = surqsolp(j) * bmp_sp(j)
	minpgw(j)= minpgw(j) * bmp_sps(j)

!! Nitrogen
	!! Particulate
	sedorgn(j) = sedorgn(j) * bmp_pn(j)
      !! Soluble
      surqno3(j) = surqno3(j) * bmp_sn(j)
	latno3(j) = latno3(j) * bmp_sns(j)
	no3gw(j) = no3gw(j) * bmp_sns(j)

!! Bacteria 
      bactrop = bactrop * bmp_bac(j)
      bactrolp = bactrolp * bmp_bac(j)
      bactsedp = bactsedp * bmp_bac(j)
      bactsedlp = bactsedlp * bmp_bac(j)

      return
	end
	
