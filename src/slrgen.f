      subroutine slrgen(j)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates solar radiation

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    hru_rmx(:)  |MJ/m^2        |maximum possible radiation for the day in HRU
!!    j           |none          |HRU number
!!    i_mo        |none          |month being simulated
!!    pr_w(3,:,:) |none          |proportion of wet days in a month
!!    solarav(:,:)|MJ/m^2/day    |average daily solar radiation for the month
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    wgncur(3,:) |none          |parameter which predicts impact of precip on
!!                               |daily solar radiation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_ra(:)   |MJ/m^2        |solar radiation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rav         |MJ/m^2        |modified monthly average solar radiation
!!    rx          |none          |variable to hold intermediate calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: j
 
      real :: rx, rav


      rav = 0.
      rav = solarav(i_mo,hru_sub(j)) /                                  
     &                              (1. - 0.5 * pr_w(3,i_mo,hru_sub(j)))
      if (subp(j) > 0.0) rav = 0.5 * rav
      rx = 0.
      rx = hru_rmx(j) - rav
      hru_ra(j) = rav + wgncur(3,j) * rx / 4.
      if (hru_ra(j) <= 0.) hru_ra(j) = .05 * hru_rmx(j)

      return
      end