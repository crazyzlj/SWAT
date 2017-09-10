      subroutine routeunit
      
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
!!    inum1s(:)   |none          |routing unit number
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
!        inum2 = 1
        varoute(:,ihout) = 0.
        sumc = 0.
        sumeiq = 0.
        do kk = 1, hrutot(inum2)
	    xx = hru_rufr(inum1,kk)* daru_km(inum2,inum1) * 100.      !!km2*100 = ha
	    if (xx > 1.e-9) then
	    jj= hru1(inum2) + kk - 1
          sumc = sumc + usle_cfac(jj) * hru_rufr(inum1,kk)
          sumeiq = sumeiq + usle_eifac(jj) * qdayout(jj) *            
     &                                              hru_rufr(inum1,kk)
          varoute(1,ihout) = 5.0 + 0.75 * tmpav(jj)
          varoute(2,ihout) = varoute(2,ihout) + qdr(jj) * xx * 10.     !! mm*ha*10 = m3
          varoute(3,ihout) = varoute(3,ihout) + sedyld(jj)             !! t
          varoute(4,ihout) = varoute(4,ihout) + sedorgn(jj) * xx
          varoute(5,ihout) = varoute(5,ihout) + sedorgp(jj) * xx
          varoute(6,ihout) = varoute(6,ihout) + (latno3(jj) + no3gw(jj) 
     &            + surqno3(jj)) * xx 
          varoute(7,ihout) = varoute(7,ihout) + (surqsolp(jj) + 
     &            minpgw(jj)) * xx
          varoute(8,ihout) = 0.
          varoute(9,ihout) = 0.
          varoute(10,ihout) = 0.
!!          varoute(11,ihout) = varoute(11,ihout) + (hrupstd(irtpest,1,jj)
!!     &	        + hrupstd(irtpest,4,jj)) * xx
!!          varoute(12,ihout) = varoute(12,ihout) + (hrupstd(irtpest,2,jj)
!!     &            ) * xx
          varoute(11,ihout) = 0.
		  varoute(12,ihout) = 0.
          varoute(13,ihout) = 0.
          varoute(14,ihout) = 0.
          varoute(15,ihout) = 0.
          varoute(16,ihout) = 0.
          varoute(17,ihout) = 0.
          varoute(20,ihout) = 0.
          varoute(21,ihout) = 0.
          varoute(22,ihout) = 0.
          varoute(29,ihout) = varoute(29,ihout) + qdayout(jj) * xx * 10.
          varoute(30,ihout) = varoute(30,ihout) + latq(jj) * xx * 10.
          varoute(31,ihout) = varoute(31,ihout) + tileq(jj) * xx * 10.
          varoute(32,ihout) = varoute(32,ihout) + gw_q(jj) * xx * 10.
          end if
        end do
      
        ru_c(inum2,inum1) = sumc
        ru_eiq(inum2,inum1) = sumeiq
	return
      end