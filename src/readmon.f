      subroutine readmon

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the input data for the recmon command
     
!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     i            |none          |file number
!!     nbyr         |none          |number of years simulated
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name            |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     bactlpmon(:,:,:)|# bact/day    |average amount of less persistent bacteria
!!                                    |loaded to stream on a given day in the 
!!                                    |month
!!     bactpmon(:,:,:) |# bact/day    |average amount of persistent bacteria
!!                                    |loaded to stream on a given day in the 
!!                                    |month
!!     cbodmon(:,:,:)  |kg/day        |average daily loading of CBOD in month
!!     chlamon(:,:,:)  |kg/day        |average daily loading of chlorophyll-a in 
!!                                    |month
!!     cmtl1mon(:,:,:) |# bact/day    |average amount of conservative metal #1
!!                                    |loaded to stream on a given day in the 
!!                                    |month
!!     cmtl2mon(:,:,:) |# bact/day    |average amount of conservative metal #2
!!                                    |loaded to stream on a given day in the 
!!                                    |month
!!     cmtl3mon(:,:,:) |# bact/day    |average amount of conservative metal #3
!!                                    |loaded to stream on a given day in the 
!!                                    |month
!!     disoxmon(:,:,:) |kg/day        |average daily loading of dissolved O2 in
!!                                    |month
!!     flomon(:,:,:)   |m**3/d        |average daily water loading for month
!!     minpmon(:,:,:)  |kg P/day      |average daily mineral P loading for month
!!     nh3mon(:,:,:)   |kg N/day      |average amount of NH3-N loaded to
!!                                    |stream on a given day in the month
!!     no2mon(:,:,:)   |kg N/day      |average amount of NO2-N loaded to
!!                                    |stream on a given day in the month
!!     no3mon(:,:,:)   |kg N/day      |average daily NO3-N loading for month
!!     orgnmon(:,:,:)  |kg N/day      |average daily organic N loading for month
!!     orgpmon(:,:,:)  |kg P/day      |average daily organic P loading for month
!!     sedmon(:,:,:)   |metric tons/d |average daily sediment loading for month
!!     solpstmon(:,:,:)|mg pst/day    |average daily soluble pesticide loading
!!                                    |for month
!!     srbpstmon(:,:,:)|mg pst/day    |average daily sorbed pesticide loading
!!                                    |for month
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     eof          |none          |end of file flag (=-1 at end of file)
!!     ia1          |none          |dummy variable
!!     ia2          |none          |dummy variable
!!     ii           |none          |counter
!!     iya          |none          |counter
!!     mon          |none          |month counter
!!     titldum      |NA            |description line
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
      integer :: ii, iya, mon, eof, ia1, ia2, begmon


!!    initialize variables
      eof = 0

      do ii = 1, 6
        read (107,5000) titldum
      end do

!!    Read until the year is the beginning year of simulation
        iya = 1
        mon = 1
        do
          read (107,*,iostat=eof) ia1, ia2,                             
     &                            flomon(i,iya,mon),                    
     &                            sedmon(i,iya,mon),                    
     &                            orgnmon(i,iya,mon),                   
     &                            orgpmon(i,iya,mon),                   
     &                            no3mon(i,iya,mon),                    
     &                            nh3mon(i,iya,mon),                    
     &                            no2mon(i,iya,mon),                    
     &                            minpmon(i,iya,mon),                   
     &                            cbodmon(i,iya,mon),                   
     &                            disoxmon(i,iya,mon),                  
     &                            chlamon(i,iya,mon),                   
     &                            solpstmon(i,iya,mon),                 
     &                            srbpstmon(i,iya,mon),                 
     &                            bactpmon(i,iya,mon),                  
     &                            bactlpmon(i,iya,mon),                 
     &                            cmtl1mon(i,iya,mon),                  
     &                            cmtl2mon(i,iya,mon),                  
     &                            cmtl3mon(i,iya,mon)
	   

		  if (ia2 == iyr) exit
	      if (eof < 0) exit
        end do



      do iya = 1, nbyr+2  !2 extra for scenarios
          if (iya == 1) then
            begmon = 2
          else
            begmon = 1
          end if
        do mon = begmon, 12
          read (107,*,iostat=eof) ia1, ia2,                             
     &                            flomon(i,iya,mon),                    
     &                            sedmon(i,iya,mon),                    
     &                            orgnmon(i,iya,mon),                   
     &                            orgpmon(i,iya,mon),                   
     &                            no3mon(i,iya,mon),                    
     &                            nh3mon(i,iya,mon),                    
     &                            no2mon(i,iya,mon),                    
     &                            minpmon(i,iya,mon),                   
     &                            cbodmon(i,iya,mon),                   
     &                            disoxmon(i,iya,mon),                  
     &                            chlamon(i,iya,mon),                   
     &                            solpstmon(i,iya,mon),                 
     &                            srbpstmon(i,iya,mon),                 
     &                            bactpmon(i,iya,mon),                  
     &                            bactlpmon(i,iya,mon),                 
     &                            cmtl1mon(i,iya,mon),                  
     &                            cmtl2mon(i,iya,mon),                  
     &                            cmtl3mon(i,iya,mon)
          if (eof < 0) exit
        end do
        if (eof < 0) exit
      end do

      close (107)
         
      return
 5000 format (a80)
      end