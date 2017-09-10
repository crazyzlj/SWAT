      subroutine schedule_ops
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine controls the simulation of the land phase of the 
!!    hydrologic cycle

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name           |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    iida           |julian date   |day being simulated (current julian date)
!!    inum1          |none          |subbasin number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ihru        |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |HRU number
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~


!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, plant_no , zz
	real:: b
	j = 0
	j = ihru


        iops = ioper(ihru)

        do while (iida == iopday(iops,ihru).and.iyr == iopyr(iops,ihru)) 

        select case(mgt_ops(iops,ihru))

        case (0)
     
        case (1)
         xm = 0.6 * (1. - Exp(-35.835 * hru_slp(ihru)))    
         sin_sl = Sin(Atan(hru_slp(ihru)))
         usle_ls(ihru) = (terr_sl(iops,ihru)/22.128) ** xm * (65.41 *   
     &      sin_sl * sin_sl + 4.56 * sin_sl + .065)
         usle_mult(ihru) = sol_rock(1,ihru) * usle_k(ihru) *            
     &   terr_p(iops,ihru) * usle_ls(ihru) * 11.8
        if (terr_cn(iops,ihru) > 1.e-6) then
           call curno(terr_cn(iops,ihru),ihru)
        end if

         case (2)
         ddrain(ihru) = drain_d(iops,ihru)
         gdrain(ihru) = drain_g(iops,ihru)
         tdrain(ihru) = drain_t(iops,ihru)
         dep_imp(ihru) = drain_idep(iops,ihru)

	!!    define soil layer that the drainage tile is in
      if (ddrain(ihru) > 0) then
        do jj = 1, sol_nly(ihru)
          if (ddrain(ihru) < sol_z(jj,ihru)) ldrain(ihru) = jj
          if (ddrain(ihru) < sol_z(jj,ihru)) exit
        end do
      else
        ldrain(ihru) = 0
      endif
!!    setting tile lage time
	 if (ldrain(j) > 0 .and. gdrain(j) > 0.01) then
            tile_ttime(j) = 1. - Exp(-24. / gdrain(j))
        else
            tile_ttime(j) = 0.
       end if

         case (3)
         usle_mult(ihru) = usle_mult(ihru) * cont_p(iops,ihru) /        
     &      usle_p(ihru)
         call curno(cont_cn(iops,ihru),ihru)

         case (4) !! filter strip
		vfsi(ihru) = filter_i(iops,ihru) !! on off flag
		vfsratio(ihru) = filter_ratio(iops,ihru)
		vfscon(ihru) = filter_con(iops,ihru)
		vfsch(ihru) = filter_ch(iops,ihru) 

!! Set some defaults if needed
		if (vfsratio(ihru) <= 0.) vfsratio(ihru) = 0.
!! minimum value for vfsratio is 0 max is 300
		if (vfsratio(ihru) <= 0.) vfsratio(ihru) = 0.
		if (vfsratio(ihru) > 300) vfsratio(ihru) = 300
!! minimum value for vfscon is 0.1 default is 0.5 max is 0.95
		if (vfscon(ihru) <= 0) vfscon(ihru) = 0.5
		if (vfscon(ihru) <= 0.1) vfscon(ihru) = 0.1
		if (vfscon(ihru) > 0.95) vfscon(ihru) = 0.95
!! minimum value for vfsch is 0 max is .95
		if (vfsch(ihru) <= 0.) vfsch(ihru) = 0.
		if (vfsch(ihru) > .95) vfsch(ihru) = .95


         case (5)
         call curno(strip_cn(iops,ihru),ihru)
         usle_mult(ihru) = usle_mult(ihru) * strip_p(iops,ihru) /       
     &      usle_p(ihru)
         tover = .0556 * (slsubbsn(j) * strip_n(iops,ihru)) ** .6 /     
     &      hru_slp(j) ** .3  

         tconc(ihru) = tconc(ihru) + tover - t_ov(ihru)

!       ioper(ihru) = ioper(ihru) + 1
!       iops = ioper(ihru)

        case (6)
	   call curno (fire_cn(iops,ihru),ihru)

        case (7)
	if (ngrwat(ihru) < 0)  ngrwat(ihru) = 0
           ngrwat(ihru) = ngrwat(ihru) + 1
           grwat_n(ihru) = gwatn(iops,ihru)
           grwat_i(ihru) = gwati(iops,ihru)
           grwat_l(ihru) = gwatl(iops,ihru)
           grwat_w(ihru) = gwatw(iops,ihru)
           grwat_d(ihru) = gwatd(iops,ihru)
           grwat_s(ihru) = gwats(iops,ihru)
           grwat_spcon(ihru) = gwatspcon(iops,ihru)
!! Calculate time of concentration for waterway similar to hydroinit.f
           tch = .62 * grwat_l(ihru) * grwat_n(ihru) ** .6 / 
     *      (hru_km(ihru) ** .125 * grwat_s(ihru) ** .375)
           tc_gwat(ihru) = tch + t_ov(ihru)
!! Set counter
		 k = mhru + ngrwat(ihru)
!!Check the channel to make sure the enter width and depth will work with 8:1 trap channel, assume width is correct
		b = (grwat_w(ihru) - 2.) * grwat_d(ihru) * 8
!! Depth and Width not possible with 8:1 sideslope and trapazoidal channel assume b =.25*width
	    if (b <= 0.) grwat_d(ihru) = 3. / 64. * grwat_w(ihru)
           call ttcoef_wway
		      
        case (8)
           plant_no = cropno_upd(iops,ihru)
           blai(plant_no) = laimx_upd(iops,ihru)
           hvsti(plant_no) = hi_upd(iops,ihru)

	 case (9)
	!! Implement Residue Management MJW
		if (so_res_flag(iops,ihru) == 1)  then
		  min_res(ihru) = so_res(iops,ihru)
	    else
	      min_res(ihru) = 0.
	    end if

	case (10) !! User defined Upland CP removal MJW
		if (ro_bmp_flag (iops,ihru) == 1) then
		  bmp_flag(ihru) = 1
            !! surface
            bmp_flo(ihru) = (1. - ro_bmp_flo(iops,ihru) / 100.)    !! Surface Flow
		  bmp_sed(ihru) = (1. - ro_bmp_sed(iops,ihru) / 100.)    !! Sediment
		  bmp_pp(ihru) = (1. - ro_bmp_pp(iops,ihru) / 100.)      !! Particulate P
		  bmp_sp(ihru) = (1. - ro_bmp_sp(iops,ihru) / 100.)      !! Soluble P
		  bmp_pn(ihru) =  (1. - ro_bmp_pn(iops,ihru) / 100.)     !! Particulate N
		  bmp_sn(ihru) = (1. - ro_bmp_sn(iops,ihru) / 100.)      !! Soluble N
		  bmp_bac(ihru) = (1. - ro_bmp_bac(iops,ihru) / 100.)    !! Bacteria
            !! subsurface
            bmp_flos(ihru) = (1. - ro_bmp_flos(iops,ihru) / 100.)    !! Subsurface Flow
		  bmp_seds(ihru) = (1. - ro_bmp_seds(iops,ihru) / 100.)    !! Sediment
		  bmp_pps(ihru) = (1. - ro_bmp_pps(iops,ihru) / 100.)      !! Particulate P
		  bmp_sps(ihru) = (1. - ro_bmp_sps(iops,ihru) / 100.)      !! Soluble P
		  bmp_pns(ihru) =  (1. - ro_bmp_pns(iops,ihru) / 100.)     !! Particulate N
		  bmp_sns(ihru) = (1. - ro_bmp_sns(iops,ihru) / 100.)      !! Soluble N
		  bmp_bacs(ihru) = (1. - ro_bmp_bacs(iops,ihru) / 100.)    !! Bacteria
            !! tile
            bmp_flot(ihru) = (1. - ro_bmp_flot(iops,ihru) / 100.)    !! Tile Flow
		  bmp_sedt(ihru) = (1. - ro_bmp_sedt(iops,ihru) / 100.)    !! Sediment
		  bmp_ppt(ihru) = (1. - ro_bmp_ppt(iops,ihru) / 100.)      !! Particulate P
		  bmp_spt(ihru) = (1. - ro_bmp_spt(iops,ihru) / 100.)      !! Soluble P
		  bmp_pnt(ihru) =  (1. - ro_bmp_pnt(iops,ihru) / 100.)     !! Particulate N
		  bmp_snt(ihru) = (1. - ro_bmp_snt(iops,ihru) / 100.)      !! Soluble N
		  bmp_bact(ihru) = (1. - ro_bmp_bact(iops,ihru) / 100.)    !! Bacteria
		else
            bmp_flag(ihru) = 0
            !! surface
            bmp_flo(ihru) = 1.      !! Surface Flow
		  bmp_sed(ihru) = 1.      !! Sediment
		  bmp_pp(ihru) = 1.       !! Particulate P
		  bmp_sp(ihru) = 1.       !! Soluble P
		  bmp_pn(ihru) =  1.      !! Particulate N
		  bmp_sn(ihru) = 1.       !! Soluble N
		  bmp_bac(ihru) = 1.      !! Bacteria
            !! subsurface
            bmp_flos(ihru) = 1.      !! Subsurface Flow
		  bmp_seds(ihru) = 1.      !! Sediment
		  bmp_pps(ihru) = 1.       !! Particulate P
		  bmp_sps(ihru) = 1.       !! Soluble P
		  bmp_pns(ihru) =  1.      !! Particulate N
		  bmp_sns(ihru) = 1.       !! Soluble N
		  bmp_bacs(ihru) = 1.      !! Bacteria
            !! tile
            bmp_flot(ihru) = 1.      !! Tile Flow
		  bmp_sedt(ihru) = 1.      !! Sediment
		  bmp_ppt(ihru) = 1.       !! Particulate P
		  bmp_spt(ihru) = 1.       !! Soluble P
		  bmp_pnt(ihru) =  1.      !! Particulate N
		  bmp_snt(ihru) = 1.       !! Soluble N
		  bmp_bact(ihru) = 1.      !! Bacteria

	    end if

	   end select

           ioper(ihru) = ioper(ihru) + 1
           iops = ioper(ihru)

      end do

      return
      end