      subroutine resinit

!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine initializes variables for the daily simulation of the
!!    channel routing command loop

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name         |units        |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihout        |none         |outflow hydrograph storage location number
!!    inum1        |none         |reservoir number
!!    inum2        |none         |inflow hydrograph storage location number
!!    mvaro        |none         |max number of variables routed through the
!!                               |reach
!!    res_sub(:)   |none         |number of subbasin reservoir is in
!!    sub_pet(:)   |mm H2O       |potential evapotranspiration for day in
!!                               |subbasin
!!    varoute(2,:) |m^3 H2O      |water
!!    varoute(3,:) |metric tons  |sediment or suspended solid load
!!    varoute(11,:)|mg pst       |pesticide in solution
!!    varoute(12,:)|mg pst       |pesticide sorbed to sediment
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    bury        |mg pst        |loss of pesticide from active sediment layer
!!                               |by burial
!!    difus       |mg pst        |diffusion of pesticide from sediment to lake 
!!                               |water
!!    pet_day     |mm H2O        |potential evapotranspiration on day
!!    reactb      |mg pst        |amount of pesticide in sediment that is lost
!!                               |through reactions
!!    reactw      |mg pst        |amount of pesticide in lake water lost through
!!                               |reactions
!!    reschlao    |kg chl-a      |chlorophyll-a leaving reservoir on day
!!    resev       |m^3 H2O       |evaporation from reservoir on day
!!    resflwi     |m^3 H2O       |water entering reservoir on day
!!    resflwo     |m^3 H2O       |water leaving reservoir on day
!!    resnh3o     |kg N          |ammonia leaving reservoir on day
!!    resno2o     |kg N          |nitrite leaving reservoir on day
!!    resno3o     |kg N          |nitrate leaving reservoir on day
!!    resorgno    |kg N          |organic N leaving reservoir on day
!!    resorgpo    |kg P          |orgainc P leaving reservoir on day
!!    respcp      |m^3 H2O       |precipitation on reservoir for day
!!    respesti    |mg pst        |pesticide entering reservoir on day
!!    ressa       |ha            |surface area of reservoir on day
!!    ressedc     |metric tons   |net change in sediment in reservoir during day
!!    ressedi     |metric tons   |sediment entering reservoir during time step
!!    ressedo     |metric tons   |sediment leaving reservoir during time step
!!    ressep      |m^3 H2O       |seepage from reservoir on day
!!    ressolpo    |kg P          |soluble P leaving reservoir on day
!!    resuspst    |mg pst        |amount of pesticide moving from sediment to
!!                               |lake water due to resuspension
!!    setlpst     |mg pst        |amount of pesticide moving from water to
!!                               |sediment due to settling
!!    solpesti    |mg pst        |soluble pesticide entering reservoir
!!    solpesto    |mg pst        |soluble pesticide in outflow on day
!!    sorpesti    |mg pst        |sorbed pesticide entering reservoir
!!    sorpesto    |mg pst        |sorbed pesticide in outflow on day
!!    varoute(:,:)|varies        |routing storage array
!!    volatpst    |mg pst        |amount of pesticide lost from lake water
!!                               |by volatilization
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ii          |none          |counter
!!    jres        |none          |reservoir number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: ii, jres
   
      jres = 0
      jres = inum1
      
!! add transfer amounts
      do ii = 2, mvaro
        varoute(ii,inum2) = varoute(ii,inum2) + vartran(ii,jres)
        vartran(ii,jres) = 0.
      end do
      

!! zero flow out variables
      do ii = 1, mvaro
        varoute(ii,ihout) = 0.
      end do

!! initialize daily variables
      bury = 0.
      difus = 0.
      pet_day = 0.
      pet_day = sub_pet(res_sub(jres))
      reactb = 0.
      reactw = 0.
      reschlao = 0.
      resev = 0.
      resflwi = 0.
      if(ievent == 0) then		!!urban modeling by J.Jeong
	  resflwi = varoute(2,inum2)
	else
	  resflwi = hhvaroute(2,inum2,1)
	endif
 !!     resflwi = varoute(2,inum2)
      resflwo = 0.
      respcp = 0.
      resnh3o = 0.
      resno2o = 0.
      resno3o = 0.
      resorgno = 0.
      resorgpo = 0.
      respesti = 0.
      ressa = 0.
      ressedc = 0.
      ressedi = 0.
      if (varoute(3,inum2) < 1.e-6) varoute(3,inum2) = 0.0
      ressedi = varoute(3,inum2)
	  ressani = varoute(23,inum2)
	  ressili = varoute(24,inum2)
	  resclai = varoute(25,inum2)
	  ressagi = varoute(26,inum2)
	  reslagi = varoute(27,inum2)
	  resgrai = varoute(28,inum2)

      if (varoute(3,inum2) < 1.e-6) varoute(3,inum2) = 0.0
      if(ievent == 0) then		!!urban modeling by J.Jeong
	  ressedi = varoute(3,inum2)
	else
	  ressedi = hhvaroute(3,inum2,1)
	endif
      ressedo = 0.

      ressano = 0.
      ressilo = 0.
      resclao = 0.
      ressago = 0.
      reslago = 0.
	  resgrao = 0.

      ressep = 0.
      ressolpo = 0.
      resuspst = 0.
      setlpst = 0.
      solpesti = 0.
      solpesti = varoute(11,inum2)
      solpesto = 0.
      sorpesti = 0.
      sorpesti = varoute(12,inum2)
      sorpesto = 0.
      volatpst = 0.

      return
      end