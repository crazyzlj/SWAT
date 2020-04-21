      subroutine swbl(snow,irrg)

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine checks the soil water balance at the end of the
!!    simulation     

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wshd_sw     |mm H2O        |water in soil at beginning of simulation
!!    wshdaao(1)  |mm H2O        |precipitation in watershed
!!    wshdaao(3)  |mm H2O        |surface runoff loading to main 
!!                               |channel in watershed
!!    wshdaao(4)  |mm H2O        |lateral flow loading to main channel
!!                               |in watershed
!!    wshdaao(5)  |mm H2O        |percolation of water out of root zone
!!                               |in watershed
!!    wshdaao(7)  |mm H2O        |actual evapotranspiration in watershed
!!    wshdaao(38) |mm H2O        |transmission losses in watershed
!!    wshddayo(35)|mm H2O        |water stored in soil profile in watershed
!!                               |at end of day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wshd_sw     |mm H2O        |difference between mass balance calculated
!!                               |from watershed averages and actual
!!                               |value for water in soil at end of 
!!                               |simulation (goal is to have wshd_sw = 0.)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    irrg        |mm H2O        |irrigation water applied to watershed
!!    snow        |mm H2O        |snow in watershed at end of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

!!this calculation ignores water stored in lag terms, also I think
!!snow sublimation needs to be taken into account
!!ie this equation needs work

      use parm

      real, intent (in) :: snow, irrg

      wshd_sw = wshd_sw  + wshd_snob - snow + wshdaao(1) - wshdaao(3) - &
     &          wshdaao(4) - wshdaao(7) - wshdaao(5) - wshddayo(35) +   &
     &          wshdaao(38) + irrg

      return
      end
