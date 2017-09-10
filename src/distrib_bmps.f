      subroutine distributed_bmps
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine calls routines for urban BMPs in the subbasin

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hhqday(:)   |mm H2O        |surface runoff generated each hour of day
!!                               |in HRU
!!    hru_ha(:)   |ha            |area of HRU in hectares
!!    ihru        |none          |HRU number
!!    ubntss(:)    |metric tons    |TSS loading from urban impervious cover
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ubntss(:)    |metric tons    |TSS loading from urban impervious cover
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    cnv         |none          |conversion factor (mm/ha => m^3)
!
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT: sand_filter
!!    SWAT: sed_pond
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
      implicit none
      
      integer :: kk,sb,ii
      real :: sub_ha,bmpfr
      real, dimension(4,0:nstep) :: sf_totalflw,sf_totaltss,ri_totalflw
      real, dimension(4,0:nstep) :: ri_totaltss
      real, dimension(4,0:nstep) :: sfflw,sfsed,riflw,rised !dimensions: 1=inflow/outflow, 2=pond id, 3=time step
      real, dimension(4,0:nstep) :: spqm3,spsed,ftqm3,ftsed,riqm3
      sb = inum1
      sub_ha = da_ha * sub_fr(sb)
      sf_totalflw = 0.; sf_totaltss = 0.
      ri_totalflw = 0.; ri_totaltss = 0.
      sfflw = 0.; sfsed = 0.; riflw = 0.; rised = 0.;bmpfr=0.
      spqm3 = 0.; spsed=0.;ftqm3=0.;ftsed=0.;riqm3=0.
      
       !initialize daily recharge from distributed BMPs
       bmp_recharge(sb) = 0.
	   
 	!--------------------------------- 	   
 	! sedimentation-filtration basin
      if(num_sf(sb)>=1.and.hrnopcp(sb,nstep)<96) then
         
         do kk=1,num_sf(sb)
            !fraction urban runoff to the sed-fil
            sfflw(1,1:nstep) = sub_ubnrunoff(sb,1:nstep) * sf_fr(sb,kk) !mm
            sfsed(1,1:nstep) = sub_ubntss(sb,1:nstep) * sf_fr(sb,kk) !tons
            !total inflow to sedfils
            sf_totalflw(1,:) = sf_totalflw(1,:) + sfflw(1,:) !mm
            sf_totaltss(1,:) = sf_totaltss(1,:) + sfsed(1,:) !tons
            
            if (iyr>sf_iy(sb,kk) .or. 
     &      (iyr==sf_iy(sb,kk).and.i_mo>=sf_im(sb,kk))) then
               if(sf_typ(sb,kk)==2) then !partial scale 
                  call sand_filter(kk,sfflw,sfsed) 
                  spqm3(:,:) = 0.
                  spsed(:,:)=0.
                  ftqm3(:,:) = sfflw(:,:) * ((sub_ha - ft_sa(sb,kk)  !m3
     &                          / 10000.) *10.) 
                  ftsed(:,:) = sfsed(:,:) !tons
                  
                !total (aggregated) outflow from sedfils
                  sf_totalflw(2,:) = sf_totalflw(2,:) + sfflw(2,:) 
     &                               + sfflw(3,:) !mm
                  sf_totaltss(2,:) = sf_totaltss(2,:) + ftsed(2,:) 
     &                               + ftsed(3,:) !tons
               elseif(sf_typ(sb,kk)==1) then !full scale
                  !first route through sedimentation pond
                  call sed_pond(kk,sfflw,sfsed)      
                  
                  spqm3(:,:) = sfflw(:,:) * ((sub_ha - sp_sa(sb,kk) 
     &                          / 10000.) *10.)
                  spsed(:,:) = sfsed(:,:) 

                ! add bypass flow to total (aggregated) outflow 
                  sf_totalflw(2,:) = sf_totalflw(2,:) + sfflw(3,:) 
                  sf_totaltss(2,:) = sf_totaltss(2,:) + sfsed(3,:) 

                  ! outflow from the sedimentation basin goest to sand filter
                  sfflw(1,:) = sfflw(2,:)  
                  sfsed(1,:) = sfsed(2,:) 

                  ! then the outflow from sed pond goes to sand filter
                  call sand_filter(kk,sfflw,sfsed)
               
                  ftqm3(:,:) = sfflw(:,:) *  ((sub_ha - ft_sa(sb,kk) 
     &                          / 10000.) *10.) !m3
                  ftsed(:,:) = sfsed(:,:)  !tons
         
                !total (aggregated) outflow from sedfils
                  sf_totalflw(2,:) = sf_totalflw(2,:) + sfflw(3,:) 
     &                               + sfflw(2,:) !mm
                  sf_totaltss(2,:) = sf_totaltss(2,:) + sfsed(3,:) 
     &                               + sfsed(2,:) !tons

               else !sedimentation pond only
                  call sed_pond(kk,sfflw,sfsed)      

                  ftqm3(:,:) = 0.
                  ftsed(:,:)=0.
                  spqm3(:,:) = sfflw(:,:) * ((sub_ha - sp_sa(sb,kk) 
     &                          / 10000.) *10.)
                  spsed(:,:) = sfsed(:,:) 
                  !total (aggregated) outflow from sedfils
                  sf_totalflw(2,:) = sf_totalflw(2,:) + sfflw(3,:) 
     &                               + sfflw(2,:) 
                  sf_totaltss(2,:) = sf_totaltss(2,:) + sfsed(3,:) 
     &                               + sfsed(2,:)
             endif
 
             
            else
               ! skip bmp simulation before it's constructed
               !total (aggregated) outflow from sedfils
               sf_totalflw(2,:) = sf_totalflw(2,:) + sfflw(1,:) 
               sf_totaltss(2,:) = sf_totaltss(2,:) + sfsed(1,:) 
           endif

           !print out bmp result in bmp-sedfil.out 
      write(77778,'(5i6,30f12.3)') iyr,iida,ii,sb,kk,sum(spqm3(1,:)),
     & sum(spqm3(2,:)),sum(spqm3(3,:)),sum(spqm3(4,:)),
     & sum(spsed(1,:))*1000.,sum(spsed(2,:))*1000.,sum(spsed(3,:))*1000.
     & ,sum(ftqm3(1,:)),sum(ftqm3(2,:)),sum(ftqm3(3,:)),sum(ftqm3(4,:)),
     & sum(ftsed(1,:))*1000.,sum(ftsed(2,:))*1000.,sum(ftsed(3,:))*1000. 
            
         end do
      endif

 	   !--------------------------------- 	   
	   ! retention-irrigation (RI)
      if(num_ri(sb)>=1.and.hrnopcp(sb,nstep)<96) then !72 hours draw-down plus another day (24hrs) 
         
         do kk=1,num_ri(sb)
 
            ! skip the pond that has zero inflow
            if (ri_fr(sb,kk)==0) cycle
            
            !fraction urban runoff to the RI
            riflw(1,1:nstep) = sub_ubnrunoff(sb,1:nstep) * 
     &         ri_fr(sb,kk)
            rised(1,1:nstep) = sub_ubntss(sb,1:nstep) * ri_fr(sb,kk)
            
            ! total inflow to RIs
            ri_totalflw(1,1:nstep) = ri_totalflw(1,1:nstep) 
     &                                + riflw(1,1:nstep)
            ri_totaltss(1,1:nstep) = ri_totaltss(1,1:nstep) 
     &                                + rised(1,1:nstep)
         
           ! skip bmp simulation before it's constructed
            if (iyr>ri_iy(sb,kk) .or.
     &      (iyr==ri_iy(sb,kk).and.i_mo>=ri_im(sb,kk))) then
              
               call ri_pond(kk,riflw,rised) 
              
               riqm3(:,:) = riflw(:,:)* ((sub_ha - sp_sa(sb,kk) 
     &                          / 10000.) *10.)
            else
               riflw(2,:) = riflw(1,:)
               rised(2,:) = rised(1,:)
            end if
 
            do ii=1,nstep
               !aggregate individual pond effluents in subbasin 
               ri_totalflw(2,ii) = ri_totalflw(2,ii) + riflw(2,ii)
               ri_totaltss(2,ii) = ri_totaltss(2,ii) + rised(2,ii)
            end do
      write(77779,'(5i6,30f12.3)') iyr,iida,ii,sb,kk,sum(riqm3(1,:)),
     & sum(riqm3(2,:)),sum(riqm3(3,:)),sum(riqm3(4,:)),
     & sum(rised(1,:))*1000.,sum(rised(2,:))*1000.,sum(rised(3,:))*1000.

         end do
         
      endif
	
      ! allocate bmp inflow/outflow to subbasin surface runoff volume
      sub_ubnrunoff(sb,1:nstep) = sub_ubnrunoff(sb,1:nstep) - 
     &   sf_totalflw(1,1:nstep) - ri_totalflw(1,1:nstep) + 
     &   sf_totalflw(2,1:nstep) + ri_totalflw(2,1:nstep)
      sub_ubntss(sb,1:nstep) = sub_ubntss(sb,1:nstep) - 
     &   sf_totaltss(1,1:nstep) - ri_totaltss(1,1:nstep) +
     &   sf_totaltss(2,1:nstep) + ri_totaltss(2,1:nstep)
     
      return
      end