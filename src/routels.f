      subroutine routels
      
!!    ~ ~ ~ PURPOSE ~ ~ ~

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    icodes(:)   |none          |routing command code:
!!                               |0 = finish       9 = save
!!                               |1 = subbasin    10 = recday
!!                               |2 = route       11 = reccnst
!!                               |3 = routres     12 = structure
!!                               |4 = transfer    13 = 
!!                               |5 = add         14 = saveconc
!!                               |6 = rechour     15 = 
!!                               |7 = recmon      16 = autocal
!!                               |8 = recyear     17 = routing unit  
!!    ihouts(:)   |none          |outflow hydrograph storage location 
!!    inum1s(:)   |none          |routing unit number - runon
!!    inum2s(:)   |none          |subbasin number
!!    inum3s(:)   |none          |not used in this command
!!    inum4s(:)   |none          |not used in this command
!!    rnum1s(:)   |none          |drainage area of routing unit in km2
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!
      use parm


!!    compute infiltration from surface runon to next landscape unit

      dstor = rnum1
      surfqrunon = 0.
      latqrunon = 0.
      gwqrunon = 0.
      surfqout = 0.
      latqout = 0.
      gwqout = 0.
      if (varoute(29,inum2) > 1.e-6) then
        do kk = 1, hrutot(inum2)
          if (hru_rufr(inum1,kk) < 1.e-9) exit
          xx = hru_rufr(inum1,kk)* daru_km(inum1) * 100.     !!km2*100 = ha
	    surfqrunon = varoute(29,inum2) / (10. * xx)
	    jj= hru1(inum2) + kk - 1
!!        add surface runon to soil layers - use percmain like rainfall infiltration
          qs = surfqrunon / 24.
          vs = (qs ** .4) * (hru_slp(jj) ** .3) / (ov_n(jj) ** .6)
          trt = slsubbsn(jj) / (3600. * vs)
          inflpcp = sol_k(1,jj) * trt + dstor
          inflpcp = Min (inflpcp, surfqrunon)
          surfqout = surfqout + (surfqrunon - inflpcp) * 10. * xx
          ihru = jj
          latq(jj) = 0.
          sepbtm(jj) = 0.
          qtile = 0.
          call percmain
          latqout = latqout + latq(jj) * 10. * xx
          gwqout = gwqout + sepbtm(jj) * 10. * xx
        end do
        varoute(29,ihout) = varoute(29,ihout) + surfqout
        varoute(30,ihout) = varoute(30,ihout) + latqout
        varoute(32,ihout) = varoute(32,ihout) + gwqout
      end if

!!    compute lateral flow to next landscape unit
      latqout = 0.
      if (varoute(30,inum2) > 1.e-6) then
        do kk = 1, hrutot(inum2)
          xx = hru_rufr(inum1,kk)* daru_km(inum1) * 100.     !!km2*100 = ha
	    latqrunon = varoute(30,inum2) / (10. * xx)
	    jj= hru1(inum2) + kk - 1
!!        put in soil layers - weighted by depth of soil layer
          dep = 0.
          xslat = 0.
          do lyr = 1, sol_nly(jj)
            latqlyr = ((sol_z(lyr,jj)-dep) / sol_z(sol_nly(jj),jj))
     &                                                  * latqrunon
            dep = sol_z(lyr,jj)
            sol_st(lyr,jj) = sol_st(lyr,jj) + latqlyr
            if (sol_st(lyr,jj) > sol_ul(lyr,jj)) then
              xslat = xslat + (sol_st(lyr,jj) - sol_ul(lyr,jj))
              sol_st(lyr,jj) = sol_ul(lyr,jj)
            end if
          end do
!!        add excess water to next landscape unit
          latqout = latqout + xslat * 10. * xx
        end do
        varoute(30,ihout) = varoute(30,ihout) + latqout
      end if

!!    compute groundwater flow to next landscape unit -
!!    used the next day in gwmod - routed with recharge
      if (varoute(32,inum2) > 1.e-6) then
        gwqrunon = varoute(32,inum2) / (10. * xx)
        gwqout = 0.
        do kk = 1, hrutot(inum2)
          if (hru_rufr(inum1,kk) > 1.e-6) then
	      xx = hru_rufr(inum1,kk)* daru_km(inum1) * 100.   !!km2*100 = ha
	      jj= hru1(inum2) + kk - 1
	      gwq_ru(jj) = gwqrunon
	    end if
        end do
      end if
		 
      return
      end
