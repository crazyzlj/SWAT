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
      real, dimension(2,nstep) :: sf_totalflw,sf_totaltss,ri_totalflw
      real, dimension(2,nstep) :: ri_totaltss
      real, dimension(2,0:nstep) :: sfflw,sfsed,riflw,rised !dimensions: 1=inflow/outflow, 2=pond id, 3=time step
 
      sb = inum1
      sf_totalflw = 0.; sf_totaltss = 0.
      ri_totalflw = 0.; ri_totaltss = 0.
      sfflw = 0.; sfsed = 0.
      riflw = 0.; rised = 0.
      
 	   !--------------------------------- 	   
 	   ! sedimentation-filtration basin
      if(num_sf(sb)>=1.and.hrnopcp(sb,nstep)<96) then
         
         do kk=1,num_sf(sb)
            !fraction urban runoff to the sed-fil
            sfflw(1,:) = sub_ubnrunoff(sb,1:nstep) * sf_fr(sb,kk)
            sfsed(1,:) = sub_ubntss(sb,1:nstep) * sf_fr(sb,kk)
            
            if (iyr>sf_iy(sb,kk) .or. 
     &      (iyr==sf_iy(sb,kk).and.i_mo>=sf_im(sb,kk))) then
               if(sf_typ(sb,kk)==2) then !partial scale 
                  call sand_filter(kk,sfflw,sfsed) 
               elseif(sf_typ(sb,kk)==1) then !full scale
                  call sed_pond(kk,sfflw,sfsed)      
                  sfflw(1,:) = sfflw(2,:)
                  sfsed(1,:) = sfsed(2,:)
                  call sand_filter(kk,sfflw,sfsed)
               else !sedimentation pond only
                  call sed_pond(kk,sfflw,sfsed)      
               endif
               
            else
               ! skip bmp simulation before it's constructed
               sfflw(2,:) = sfflw(1,:)
               sfsed(2,:) = sfsed(1,:)
           endif

            !aggregate individual pond effluents in subbasin 
            do ii=1,nstep
               sf_totalflw(:,ii) = sf_totalflw(:,ii) + sfflw(:,ii)
               sf_totaltss(:,ii) = sf_totaltss(:,ii) + sfsed(:,ii)
            end do
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
            
            ! skip bmp simulation before it's constructed
            if (iyr>ri_iy(sb,kk) .or.
     &      (iyr==ri_iy(sb,kk).and.i_mo>=ri_im(sb,kk))) then
               call ri_pond(kk,riflw(1,:),riflw(2,:),
     &          rised(1,:),rised(2,:)) 
            else
               riflw(2,:) = riflw(1,:)
               rised(2,:) = rised(1,:)
            end if
 
            do ii=1,nstep
               !aggregate individual pond effluents in subbasin 
               ri_totalflw(:,ii) = ri_totalflw(:,ii) + riflw(:,ii)
               ri_totaltss(:,ii) = ri_totaltss(:,ii) + rised(:,ii)
            end do
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
