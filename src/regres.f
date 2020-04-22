      real*8 function regres(k) result (r_regres)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this function calculates constituent loadings to the main channel using
!!    USGS regression equations

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fimp(:)      |fraction      |fraction of HRU area that is
!!                                |impervious (both directly and
!!                                |indirectly connected)
!!    hru_km(:)    |km^2          |area of HRU in square kilometers
!!    ihru         |none          |HRU number
!!    ireg(:)      |none          |precipitation category:
!!                                |  1 precipitation <= 508 mm/yr
!!                                |  2 precipitation > 508 and <= 1016 mm/yr
!!                                |  3 precipitation > 1016 mm/yr
!!    k            |none          |identification code for regression data
!!                                |  1 carbonaceous oxygen demand
!!                                |  2 suspended solid load
!!                                |  3 total nitrogen
!!                                |  4 total phosphorus
!!    precipday    |mm H2O        |precipitation for the day in HRU
!!    urblu(:)     |none          |urban land type identification number from
!!                                |urban database
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    regres       |kg            |amount of constituent removed in surface
!!                                |runoff
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name         |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bcod(:,:)    |none          |regression coefficients for calculating
!!                                |carbonaceous oxygen demand of urban runoff
!!    bsus(:,:)    |none          |regression coefficients for calculating
!!                                |suspended solid load of urban runoff
!!    btn(:,:)     |none          |regression coefficients for calculating
!!                                |total nitrogen in urban runoff
!!    btp(:,:)     |none          |regression coefficients for calculating
!!                                |total phosphorus in urban runoff
!!    ii           |none          |precipitation category
!!    j            |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm, except_this_one => regres

      integer, intent (in) :: k
      real*8, dimension (5,3) :: beta
      integer :: j, ii
      real*8, dimension(5,3) :: bcod =                                    
     &      reshape ((/407.0, 0.626, 0.710, 0.379, 1.518,               
     &                 151.0, 0.823, 0.726, 0.564, 1.451,               
     &                 102.0, 0.851, 0.601, 0.528, 1.978/), (/5,3/))
      real*8, dimension(5,3) :: bsus =                                    
     &      reshape ((/1778.0, 0.867, 0.728, 0.157, 2.367,              
     &                  812.0, 1.236, 0.436, 0.202, 1.938,              
     &                   97.7, 1.002, 1.009, 0.837, 2.818/), (/5,3/))
      real*8, dimension(5,3) :: btn =                                     
     &      reshape ((/20.2, 0.825, 1.070, 0.479, 1.258,                
     &                 4.04, 0.936, 0.937, 0.692, 1.373,                
     &                 1.66, 0.703, 0.465, 0.521, 1.845/), (/5,3/))
      real*8, dimension(5,3) :: btp =                                     
     &      reshape ((/1.725, 0.884, 0.826, 0.467, 2.130,               
     &                 0.697, 1.008, 0.628, 0.469, 1.790,               
     &                 1.618, 0.954, 0.789, 0.289, 2.247/), (/5,3/))

      j = 0
      j = ihru

      ii = 0
      ii = ireg(hru_sub(j))

      beta = 0.
      if (k==1) beta = bcod
      if (k==2) beta = bsus
      if (k==3) beta = btn
      if (k==4) beta = btp

      r_regres = 0.
      r_regres = beta(1,ii) * (precipday / 25.4) ** beta(2,ii) *          
     &         (hru_km(j) * fimp(urblu(j)) / 2.589) ** beta(3,ii) *     
     &         (fimp(urblu(j)) * 100. + 1.) ** beta(4,ii) * beta(5,ii)


      r_regres = r_regres / 2.205      !! convert from lbs to kg

      return
      end