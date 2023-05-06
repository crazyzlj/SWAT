      subroutine readops
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads data from the HRU/subbasin management input file
!!    (.mgt). This file contains data related to management practices used in
!!    the HRU/subbasin.

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name       |units            |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                 |urban.dat
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name            |units          |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!                                    |daily
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    eof         |none          |end of file flag (=-1 if eof, else = 0)
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    day         |none          |day operation occurs
!!    mgt_op      |none          |operation code number
!!                               |0 end of rotation year
!!                               |1 plant/beginning of growing season
!!    mgt1i       |none          |first management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt2i       |none          |second management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt3i       |none          |third management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt4        |none          |fourth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt5        |none          |fifth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt6        |none          |sixth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt7        |none          |seventh management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt8        |none          |eighth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    mgt9        |none          |ninth management parameter out of .mgt
!!                               |file (definition changes depending on
!!                               |mgt_op)
!!    titldum     |NA            |title line from input dataset
!!    vfscon(:)   |none          |Fraction of the total runoff from the entire field
!!                               |entering the most concentrated 10% of the VFS.
!!    vfsratio(:) |none          |Field area/VFS area ratio
!!    vfsch(:)    |none          |Fraction of flow entering the most concentrated 10% of the VFS.
!!	                           |which is fully channelized
!!	grwat_n(:)      |none      |Mannings's n for grassed waterway
!!	grwat_i(:)      |none      |Flag for the simulation of grass waterways
!!                                 | gwat_i = 0 inactive
!!                                 |        = 1 active
!!	grwat_l(:)      |km	   |Length of grass Waterway
!!	grwat_w(:)      |m         |Average width of grassed waterway
!!	grwat_d(:)      |m         |Depth of grassed waterway from top of bank 
!!                                 |  to bottom
!!	grwat_s(:)      |m         |Average slope of grassed waterway channel
!!	grwat_spcon(:)  |none      |Linear parameter for calculating sediment 
!!                                 |  in grassed waterways
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: Jdt

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=80) :: titldum
	integer :: eof
	integer :: mon, day, mgt_op, mgt2i, mgt1i
	real*8 :: mgt3,mgt4,mgt5,mgt6,mgt7,mgt8,mgt9,mgt10,mgt11,mgt12,
     & mgt13,mgt14,mgt15,mgt16,mgt17,mgt18,mgt19,mgt20,mgt21,mgt22,mgt23
      
	
	
	read (111,5000,end=999) titldum

!!      read scheduled operations

          iops = 0

          do
          mon = 0
          day = 0
          iyear = 0.
          mgt_op = 0
          mgt1i = 0
          mgt2i = 0
          mgt3 = 0.
          mgt4 = 0.
          mgt5 = 0.
          mgt6 = 0
          mgt7 = 0.
          mgt8 = 0.
		mgt9 = 0.
		

          read (111,5200,iostat=eof) mon, day, iyear, mgt_op, mgt1i,    
     & mgt2i, mgt3, mgt4, mgt5, mgt6, mgt7, mgt8, mgt9, mgt10, mgt11,
     & mgt12, mgt13, mgt14, mgt15, mgt16, mgt17, mgt18, mgt19, mgt20,
     & mgt21, mgt22, mgt23 
	    if (eof < 0) exit
          iops = iops + 1
          iopday(iops,ihru) = Jdt (ndays,day,mon)
          iopyr(iops,ihru) = iyear                  
          mgt_ops(iops,ihru) = mgt_op                

          select case (mgt_op)

          case (0)  !! end of operations file

          case (1)  !! terracing        
             terr_p(iops,ihru) = mgt4
             terr_cn(iops,ihru) = mgt5
             terr_sl(iops,ihru) = mgt6

          case (2)  !! tile drainage            
             drain_d(iops,ihru) = mgt4
             drain_t(iops,ihru) = mgt5
             drain_g(iops,ihru) = mgt6
             if (mgt7 < 1.e-6) mgt7 = 6000.0
             drain_idep(iops,ihru) = mgt7
        
          case (3)  !! contouring           
             cont_cn(iops,ihru) = mgt4
             cont_p(iops,ihru) =  mgt5

          case (4)  !! filter
		     filter_i(iops,ihru) = mgt1i  !! on off flag
		     filter_ratio(iops,ihru) = mgt3
		     filter_con(iops,ihru) = mgt4
		     filter_ch(iops,ihru) = mgt5                  

          case (5)  !! strip cropping
             strip_n(iops,ihru) = mgt4
             strip_cn(iops,ihru) = mgt5
             strip_c(iops,ihru) = mgt6
             strip_p(iops,ihru) = mgt7

          case (6)  !! fire
             fire_cn(iops,ihru) = mgt4

          case (7)  !! grass waterways
             gwati(iops,ihru) = mgt1i 
             gwatn(iops,ihru) = mgt3
             gwatspcon(iops,ihru) = mgt4             
             gwatd(iops,ihru) = mgt5
             gwatw(iops,ihru) = mgt6
             gwatl(iops,ihru) = mgt7	
             gwats(iops,ihru) = mgt8
!! Set defaults
!! Mannings via Fiener, 2006
	if (gwatn(iops,ihru) <=0.) gwatn(iops,ihru) = 0.35 
!! length based on one side of a square HRU
	if (gwatl(iops,ihru) <=0.) gwatl(iops,ihru) = hru_km(ihru)**.5
!! default to a workable depth based on with and 8:1 sideslopes
	if (gwatd(iops,ihru) <= 0.) then
		gwatd(iops,ihru) = 3. / 64. * gwatw(iops,ihru)
	end if
!! Default to 3/4 HRU slope
	if (gwats(iops,ihru) <=0.) gwats(iops,ihru) = hru_slp(ihru)*.75
!! default sed coeff to 0.005
	if (gwatspcon(iops,ihru)<= 0.) gwatspcon(iops,ihru) = 0.005


          case (8) !! plant parameter update
             cropno_upd(iops,ihru) = mgt1i
             hi_upd(iops,ihru) = mgt4
             laimx_upd(iops,ihru) = mgt5


	!!	case (9) !! Residue Managment  mjw
	!! Force residue to a minimum value regardless of tillage.		mjw
	!!	so_res_flag(iops,ihru) = mgt1i !!mjw
	!!	so_res(iops,ihru) = mgt4 !!mjw

		case (10) !! Generic Conservation Practice  mjw
	!! Get user defined removal eff and use these		mjw
		ro_bmp_flag(iops,ihru) = mgt1i  !! Flag to turn on or off user BMP
 
      !! surface runoff removal efficiency
          ro_bmp_flo(iops,ihru) = mgt3    !! Flow
		ro_bmp_sed(iops,ihru) = mgt4    !! Sediment
		ro_bmp_pp(iops,ihru) = mgt5     !! Particulate P
		ro_bmp_sp(iops,ihru) = mgt6     !! Soluble P
		ro_bmp_pn(iops,ihru) = mgt7     !! Particulate N
		ro_bmp_sn(iops,ihru) = mgt8     !! Soluble N
		ro_bmp_bac(iops,ihru) = mgt9    !! Bacteria
      !! subsurface - lateral soil and groundwater         
          ro_bmp_flos(iops,ihru) = mgt10  !! Flow
		ro_bmp_seds(iops,ihru) = mgt11  !! Sediment
		ro_bmp_pps(iops,ihru) = mgt12   !! Particulate P
		ro_bmp_sps(iops,ihru) = mgt13    !! Soluble P
		ro_bmp_pns(iops,ihru) = mgt14   !! Particulate N
		ro_bmp_sns(iops,ihru) = mgt15   !! Soluble N
		ro_bmp_bacs(iops,ihru) = mgt16  !! Bacteria
      !! tile flow removal efficiency   
          ro_bmp_flot(iops,ihru) = mgt17  !! Flow
		ro_bmp_sedt(iops,ihru) = mgt18  !! Sediment
		ro_bmp_ppt(iops,ihru) = mgt19   !! Particulate P
		ro_bmp_spt(iops,ihru) = mgt20   !! Soluble P
		ro_bmp_pnt(iops,ihru) = mgt21   !! Particulate N
		ro_bmp_snt(iops,ihru) = mgt22   !! Soluble N
		ro_bmp_bact(iops,ihru) = mgt23  !! Bacteria
		
		case (11) !! Generic Salt concentration loading from HRU through various pathways Srini
	!! Get user defined salt loading -- Srini
		salt_flag(iops,ihru) = mgt1i  !! Flag to turn on or off user Salt modeling, specify # of salt constituents
		bmp_salt(ihru) = iops
 
      !! surface runoff salt concentrations
        sro_salt(iops,ihru,1) = mgt3    !! Salt conc constituent 1
		sro_salt(iops,ihru,2) = mgt4    !! Salt conc constituent 2
		sro_salt(iops,ihru,3) = mgt5     !! Salt conc constituent 3
		sro_salt(iops,ihru,4) = mgt6     !! Salt conc constituent 4
		sro_salt(iops,ihru,5) = mgt7     !! Salt conc constituent 5
		sro_salt(iops,ihru,6) = mgt8     !! Salt conc constituent 6
		sro_salt(iops,ihru,7) = mgt9    !! Salt conc constituent 7
		sro_salt(iops,ihru,8) = mgt10    !! Salt conc constituent 8
		sro_salt(iops,ihru,9) = mgt11    !! Salt conc constituent 9
		sro_salt(iops,ihru,10) = mgt12    !! Salt conc constituent 10
		
		read (111,5200,iostat=eof) mon, day, iyear, mgt_op, mgt1i,    
     & mgt2i, mgt3, mgt4, mgt5, mgt6, mgt7, mgt8, mgt9, mgt10, mgt11,
     & mgt12, mgt13, mgt14, mgt15, mgt16, mgt17, mgt18, mgt19, mgt20,
     & mgt21, mgt22, mgt23 
	    if (eof < 0) exit
		
      !! subsurface - lateral flow salt concentration         
        slt_salt(iops,ihru,1) = mgt3    !! Salt conc constituent 1
		slt_salt(iops,ihru,2) = mgt4    !! Salt conc constituent 2
		slt_salt(iops,ihru,3) = mgt5     !! Salt conc constituent 3
		slt_salt(iops,ihru,4) = mgt6     !! Salt conc constituent 4
		slt_salt(iops,ihru,5) = mgt7     !! Salt conc constituent 5
		slt_salt(iops,ihru,6) = mgt8     !! Salt conc constituent 6
		slt_salt(iops,ihru,7) = mgt9    !! Salt conc constituent 7
		slt_salt(iops,ihru,8) = mgt10    !! Salt conc constituent 8
		slt_salt(iops,ihru,9) = mgt11    !! Salt conc constituent 9
		slt_salt(iops,ihru,10) = mgt12    !! Salt conc constituent 10
		
		read (111,5200,iostat=eof) mon, day, iyear, mgt_op, mgt1i,    
     & mgt2i, mgt3, mgt4, mgt5, mgt6, mgt7, mgt8, mgt9, mgt10, mgt11,
     & mgt12, mgt13, mgt14, mgt15, mgt16, mgt17, mgt18, mgt19, mgt20,
     & mgt21, mgt22, mgt23 
	    if (eof < 0) exit
      
      !! gw - groundwater flow salt concentration       
        gw_salt(iops,ihru,1) = mgt3    !! Salt conc constituent 1
		gw_salt(iops,ihru,2) = mgt4    !! Salt conc constituent 2
		gw_salt(iops,ihru,3) = mgt5     !! Salt conc constituent 3
		gw_salt(iops,ihru,4) = mgt6     !! Salt conc constituent 4
		gw_salt(iops,ihru,5) = mgt7     !! Salt conc constituent 5
		gw_salt(iops,ihru,6) = mgt8     !! Salt conc constituent 6
		gw_salt(iops,ihru,7) = mgt9    !! Salt conc constituent 7
		gw_salt(iops,ihru,8) = mgt10    !! Salt conc constituent 8
		gw_salt(iops,ihru,9) = mgt11    !! Salt conc constituent 9
		gw_salt(iops,ihru,10) = mgt12    !! Salt conc constituent 10
		
		read (111,5200,iostat=eof) mon, day, iyear, mgt_op, mgt1i,    
     & mgt2i, mgt3, mgt4, mgt5, mgt6, mgt7, mgt8, mgt9, mgt10, mgt11,
     & mgt12, mgt13, mgt14, mgt15, mgt16, mgt17, mgt18, mgt19, mgt20,
     & mgt21, mgt22, mgt23 
	    if (eof < 0) exit
		
      !! tile flow salt concentration   
        tile_salt(iops,ihru,1) = mgt3    !! Salt conc constituent 1
		tile_salt(iops,ihru,2) = mgt4    !! Salt conc constituent 2
		tile_salt(iops,ihru,3) = mgt5     !! Salt conc constituent 3
		tile_salt(iops,ihru,4) = mgt6     !! Salt conc constituent 4
		tile_salt(iops,ihru,5) = mgt7     !! Salt conc constituent 5
		tile_salt(iops,ihru,6) = mgt8     !! Salt conc constituent 6
		tile_salt(iops,ihru,7) = mgt9    !! Salt conc constituent 7
		tile_salt(iops,ihru,8) = mgt10    !! Salt conc constituent 8
		tile_salt(iops,ihru,9) = mgt11    !! Salt conc constituent 9
		tile_salt(iops,ihru,10) = mgt12    !! Salt conc constituent 10
		

          
          end select
          end do

      close (111)
     
  999 return
 5000 format (a)
 5200 format (1x,i2,1x,i2,5x,i4,1x,i2,1x,i4,1x,i3,1x,f6.2,1x,f12.5,1x,
     &        f6.2,1x,f11.5,1x,f8.2,1x,f6.2,1x,16f5.2)
      end