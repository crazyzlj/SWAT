      subroutine ttcoef_wway     
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine computes travel time coefficients for routing
!!    along the main channel - grassed waterways

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    grwat_d(:)  |m             |average depth of main channel
!!    grwat_l(:)  |km            |length of main channel
!!    grwat_n(:)|none          |Manning's "n" value for the main channel
!!    grwat_s(:)|m/m           |average slope of main channel
!!    grwat_w(:)  |m             |average width of main channel
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    chsslope(:)   |none          |change in horizontal distance per unit
!!                               |change in vertical distance on channel side
!!                               |slopes; always set to 2 (slope=1/2)
!!    wat_phi(1,:)    |m^2       |cross-sectional area of flow at bankfull
!!                               |depth
!!    wat_phi(2,:)    |none      |
!!    wat_phi(3,:)    |none      |
!!    wat_phi(4,:)    |none      |
!!    wat_phi(5,:)    |m^3/s     |flow rate when reach is at bankfull depth
!!    wat_phi(6,:)    |m         |bottom width of main channel
!!    wat_phi(7,:)    |m         |depth of water when reach is at bankfull
!!    wat_phi(8,:)    |m/s       |average velocity when reach is at 
!!                               |bankfull depth
!!    wat_phi(9,:)    |m/s       |wave celerity when reach is at
!!                               |bankfull depth
!!    wat_phi(10,:)   |hr        |storage time constant for reach at
!!                               |bankfull depth (ratio of storage to
!!                               |discharge)
!!    wat_phi(11,:)   |m/s       |average velocity when reach is at
!!                               |0.1 bankfull depth (low flow)
!!    wat_phi(12,:)   |m/s       |wave celerity when reach is at
!!                               |0.1 bankfull depth (low flow)
!!    wat_phi(13,:)   |hr        |storage time constant for reach at
!!                               |0.1 bankfull depth (low flow) (ratio
!!                               |of storage to discharge)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    aa          |none          |area/area=1 (used to calculate velocity with
!!                               |Manning's equation)
!!    a           |m^2           |cross-sectional area of channel
!!    b           |m             |bottom width of channel
!!    d           |m             |depth of flow 
!!    fps         |none          |change in horizontal distance per unit
!!                               |change in vertical distance on floodplain side
!!                               |slopes; always set to 4 (slope=1/4)
!!    jj          |none          |counter
!!    k           |none          |dummy argument (HRU number)
!!    p           |m             |wetting perimeter
!!    qq1         |m^3/s         |flow rate for a specified depth
!!    rh          |m             |hydraulic radius of channel
!!    tt1         |km s/m        |time coefficient for specified depth
!!    tt2         |km s/m        |time coefficient for bankfull depth
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Sqrt
!!    SWAT: Qman

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
      use parm
 

      integer :: jj, k
      real :: fps, d, b, p, a, qq1, rh, tt1, tt2, aa
      
      k = ihru
      
      do jj = 1, 13
        wat_phi(jj,k) = 0.
      end do

      aa = 1.
      b = 0.
      d = 0.
      
!!    If side slope is not set in .rte file then assume this default
!!    If it is main reach default side slope to 2:1 if it is a waterway default to 8:1
      if (chside(k) <= 1.e-6) then
         chsslope = 8.
      else
         chsslope = chside(k)
      end if

      fps = 4.
      d = grwat_d(k)
      b = grwat_w(k) - 2. * d * chsslope


!!    check if bottom width (b) is < 0
      if (b <= 0.) then
        b = 0.
        chsslope = 0.
        b = .5 * grwat_w(k)
        chsslope = (grwat_w(k) - b) / (2. * d)
      end if
      wat_phi(6,k) = b
      wat_phi(7,k) = d

!!    compute flow and travel time at bankfull depth
      p = 0.
      a = 0.
      rh = 0.
      tt2 = 0.
      p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
      a = b * d + chsslope * d * d
      rh = a / p
      wat_phi(1,k) = a
      wat_phi(5,k) = Qman(a, rh, grwat_n(k), grwat_s(k))
      wat_phi(8,k) = Qman(aa, rh, grwat_n(k), grwat_s(k))
      wat_phi(9,k) = wat_phi(8,k) * 5. / 3.
      wat_phi(10,k) = grwat_l(k) / wat_phi(9,k) / 3.6
      tt2 = grwat_l(k) * a / wat_phi(5,k)

!!    compute flow and travel time at 1.2 bankfull depth
      d = 0.
      rh = 0.
      qq1 = 0.
      tt1 = 0.
      d = 1.2 * grwat_d(k)
      a = a + (grwat_w(k) * grwat_d(k) + fps * (d - grwat_d(k)) ** 2)
      p=p + 4.*grwat_w(k) + (0.4 * grwat_d(k) * Sqrt(fps * fps + 1.))
      rh = a / p
      qq1 = Qman(a, rh, grwat_n(k), grwat_s(k))
      tt1 = grwat_l(k) * a / qq1

!!    compute flow and travel time at 0.1 bankfull depth
      a = 0.
      d = 0.
      p = 0.
      rh = 0.
      qq1 = 0.
      tt1 = 0.
      d = 0.1 * grwat_d(k)
      p = b + 2. * d * Sqrt(chsslope * chsslope + 1.)
      a = b * d + chsslope * d * d
      rh = a / p
      qq1 = Qman(a, rh, grwat_n(k), grwat_s(k))
      tt1 = grwat_l(k) * a / qq1
      wat_phi(11,k) = Qman(aa, rh, grwat_n(k), grwat_s(k))
      wat_phi(12,k) = wat_phi(11,k) * 5. / 3.
      wat_phi(13,k) = grwat_l(k) / wat_phi(12,k) / 3.6

      return
      end
