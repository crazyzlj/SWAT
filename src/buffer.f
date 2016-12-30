      subroutine buffer
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calculates the reduction of nitrates through a riparian 
!!    buffer system - developed for Sushama at NC State

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactrolp    |# colonies/ha |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# colonies/ha |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# colonies/ha |less persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    bactsedp    |# colonies/ha |persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    curyr       |none          |current year of simulation
!!    fsred(:)    |none          |reduction in bacteria loading from filter
!!                               |strip
!!    hru_fr(:)   |none          |fraction of watershed area in HRU
!!    hrupest(:)  |none          |pesticide use flag:
!!                               | 0: no pesticides used in HRU
!!                               | 1: pesticides used in HRU
!!    ihru        |none          |HRU number
!!    npmx        |none          |number of different pesticides used in
!!                               |the simulation
!!    nyskip      |none          |number of years to skip output summarization
!!                               |and printing
!!    pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto
!!                               |sediment
!!    pst_surq(:,:)|kg/ha        |amount of pesticide type lost in surface
!!                               |runoff on current day in HRU
!!    sbactrolp   |# colonies/ha |average annual number of less persistent 
!!                               |bacteria transported to main channel
!!                               |with surface runoff in solution
!!    sbactrop    |# colonies/ha |average annual number of persistent bacteria
!!                               |transported to main channel with surface
!!                               |runoff in solution
!!    sbactsedlp  |# colonies/ha |average annual number of less persistent
!!                               |bacteria transported with sediment in
!!                               |surface runoff
!!    sbactsedp   |# colonies/ha |average annual number of persistent bacteria   
!!                               |transported with sediment in surface runoff
!!    sedminpa    |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps    |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn     |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    sedorgp     |kg P/ha       |amount of organic phosphorus in surface runoff
!!                               |in HRU for the day
!!    sedyld      |metric tons   |daily soil loss caused by water erosion
!!    surqno3     |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    surqsolp    |kg P/ha       |amount of soluble phosphorus in surface runoff
!!                               |in HRU for the day
!!    trapeff(:)  |none          |filter strip trapping efficiency (used for
!!                               |everything but bacteria)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bactrolp    |# colonies/ha |less persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactrop     |# colonies/ha |persistent bacteria transported to main
!!                               |channel with surface runoff
!!    bactsedlp   |# colonies/ha |less persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    bactsedp    |# colonies/ha |persistent bacteria transported with 
!!                               |sediment in surface runoff
!!    pst_sed(:,:)|kg/ha         |pesticide loading from HRU sorbed onto
!!                               |sediment
!!    pst_surq(:,:)|kg/ha        |amount of pesticide type lost in surface
!!                               |runoff on current day in HRU
!!    sbactrolp   |# colonies/ha |average annual number of less persistent 
!!                               |bacteria transported to main channel
!!                               |with surface runoff in solution
!!    sbactrop    |# colonies/ha |average annual number of persistent bacteria
!!                               |transported to main channel with surface 
!!                               |runoff in solution
!!    sbactsedlp  |# colonies/ha |average annual number of less persistent
!!                               |bacteria transported with sediment in
!!                               |surface runoff
!!    sbactsedp   |# colonies/ha |average annual number of persistent bacteria
!!                               |transported with sediment in surface runoff
!!    sedminpa    |kg P/ha       |amount of active mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedminps    |kg P/ha       |amount of stable mineral phosphorus sorbed to
!!                               |sediment in surface runoff in HRU for day
!!    sedorgn     |kg N/ha       |amount of organic nitrogen in surface runoff
!!                               |in HRU for the day
!!    sedorgp     |kg P/ha       |amount of organic phosphorus in surface runoff
!!                               |in HRU for the day
!!    sedyld      |metric tons   |daily soil loss caused by water erosion
!!    surqno3     |kg N/ha       |amount of NO3-N in surface runoff in HRU for
!!                               |the day
!!    surqsolp    |kg P/ha       |amount of soluble phosphorus in surface runoff
!!                               |in HRU for the day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    k           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, k

      j = 0
      j = ihru

!! compute nitrate reduction as a function of distance to stream
      reduc = 2.1661 * filterw(j) - 5.1302
      if (reduc < 0.) reduc = 0.
      latno3(j) = latno3(j) * (1. - reduc / 100.)
      no3gw(j) = no3gw(j) * (1. - reduc / 100.)

      return
      end