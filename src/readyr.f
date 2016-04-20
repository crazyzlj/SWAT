      subroutine readyr

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     reads in the input data for the recyear command
     
!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     i            |none          |file number
!!     nbyr         |none          |number of years simulated
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     bactlpyr(:,:)|# bact/day    |average daily loading of less persistent
!!                                 |bacteria for year
!!     bactpyr(:,:) |# bact/day    |average daily loading of persistent bacteria
!!                                 |for year
!!     cbodyr(:,:)  |kg/day        |average daily loading of CBOD in year
!!     chlayr(:,:)  |kg/day        |average daily loading of chlorophyll-a in 
!!                                 |year
!!     cmtl1yr(:,:) |kg/day        |average daily loading of conservative metal
!!                                 |#1 for year
!!     cmtl2yr(:,:) |kg/day        |average daily loading of conservative metal
!!                                 |#2 for year
!!     cmtl3yr(:,:) |kg/day        |average daily loading of conservative metal
!!                                 |#3 for year
!!     disoxyr(:,:) |kg/day        |average daily loading of dissolved O2 in 
!!                                 |year
!!     floyr(:,:)   |m**3/d        |average daily water loading for year
!!     minpyr(:,:)  |kg P/day      |average daily mineral P loading for year
!!     nh3yr(:,:)   |kg N/day      |average daily NH3-N loading for year
!!     no2yr(:,:)   |kg N/day      |average daily NO2-N loading for year
!!     no3yr(:,:)   |kg N/day      |average daily NO3-N loading for year
!!     orgnyr(:,:)  |kg N/day      |average daily organic N loading for year
!!     orgpyr(:,:)  |kg P/day      |average daily organic P loading for year
!!     sedyr(:,:)   |metric tons/d |average daily sediment loading for year
!!     solpstyr(:,:)|mg pst/day    |average daily soluble pesticide loading
!!                                 |for year
!!     srbpstyr(:,:)|mg pst/day    |average daily sorbed pesticide loading
!!                                 |for year
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name         |units         |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     eof          |none          |end of file flag (=-1 at end of file)
!!     ii           |none          |counter
!!     iya          |none          |counter
!!     titldum      |NA            |description line
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~


      use parm

      character (len=80) :: titldum
      integer :: ii, iya, eof, ia1


!!    initialize variables
      eof = 0

      do ii = 1, 6 
        read (108,5000) titldum
      end do

!!    Read until the year is the beginning year of simulation
      iya = 1
        do
        read (108,*,iostat=eof) ia1, floyr(i,iya),                      
     &                       sedyr(i,iya),                              
     &                       orgnyr(i,iya),                             
     &                       orgpyr(i,iya),                             
     &                       no3yr(i,iya),                              
     &                       nh3yr(i,iya),                              
     &                       no2yr(i,iya),                              
     &                       minpyr(i,iya),                             
     &                       cbodyr(i,iya),                             
     &                       disoxyr(i,iya),                            
     &                       chlayr(i,iya),                             
     &                       solpstyr(i,iya),                           
     &                       srbpstyr(i,iya),                           
     &                       bactpyr(i,iya),                            
     &                       bactlpyr(i,iya),                           
     &                       cmtl1yr(i,iya),                            
     &                       cmtl2yr(i,iya),                            
     &                       cmtl3yr(i,iya)
            if (ia1 == iyr) exit
	      if (eof < 0) exit
        end do

      do iya = 2, nbyr+2  !2 extra for forecast scenarios
        read (108,*,iostat=eof) ia1, floyr(i,iya),                      
     &                       sedyr(i,iya),                              
     &                       orgnyr(i,iya),                             
     &                       orgpyr(i,iya),                             
     &                       no3yr(i,iya),                              
     &                       nh3yr(i,iya),                              
     &                       no2yr(i,iya),                              
     &                       minpyr(i,iya),                             
     &                       cbodyr(i,iya),                             
     &                       disoxyr(i,iya),                            
     &                       chlayr(i,iya),                             
     &                       solpstyr(i,iya),                           
     &                       srbpstyr(i,iya),                           
     &                       bactpyr(i,iya),                            
     &                       bactlpyr(i,iya),                           
     &                       cmtl1yr(i,iya),                            
     &                       cmtl2yr(i,iya),                            
     &                       cmtl3yr(i,iya)
        if (eof < 0) exit
      end do

      close (108)

      return
 5000 format (a80)
      end