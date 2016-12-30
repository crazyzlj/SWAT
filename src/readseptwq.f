      subroutine readseptwq

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine reads input parameters from the sept wq database
!!     (septwq.dat). Information is used when a hru has septic tank.
       
!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name             |units      |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     msdb             |none       |maximum number of sept wq data database
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name             |units      |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     sptqs(:)         |m3/d       |Flow rate of the septic tank effluent 
!!                      |           |per capita
!!     sptbodconcs(:)   |mg/l       |Biological Oxygen Demand of the septic 
!!                      |           |tank effluent  
!!     sptnames(:)      |           |name of septic system 
!!     spttssconcs(:)   |mg/l       |Concentration of total suspended solid in the 
!!                      |           |septic tank effluent
!!     spttnconcs(:)    |mg/l       |Concentration of total nitrogen 
!!                      |           |in the septic tank effluent
!!     sptnh4concs(:)   |mg/l       |Concentration of total phosphorus 
!!                      |           |of the septic tank effluent
!!     sptno3concs(:)   |mg/l       |Concentration of nitrate 
!!                      |           |in the septic tank effluent
!!     sptno2concs(:)   |mg/l       |Concentration of nitrite 
!!                      |           |in the septic tank effluent
!!     sptorgnconcs(:)  |mg/l       |Concentration of organic nitrogen in 
!!                      |           |the septic tank effluent
!!     spttpconcs(:)    |mg/l       |Concentration of total phosphorus in 
!!                      |           |the septic tank effluent  
!!     sptminps(:)      |mg/l       |Concentration of mineral phosphorus in
!!                      |           |the septic tank effluent
!!     sptorgps(:)      |mg/l       |concentration of organic phosphorus in the 
!!                      |           |septic tank effluent
!!     sptfcolis(:)     |cfu/100ml  |concentration of the facel caliform in the 
!!                      |           |septic tank effluent
!!
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!
!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name           |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     eof            |none         |end of file flag
!!     ist            |none         |counter which represents the array
!!                                  |storage number of the septic data
!!                                  |the array storage number is used by the
!!                                  |model to access data for a specific 
!!                                  |sept type
!!     isnum          |none         |number of septic system database  (reference
!!                                  |only)
!!     sptfulname (:)   |           | septic tank full name description 
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~
    
    
!!     This routine was developed by C. Santhi. Inputs for this routine is provided in septwq.dat 
!!     of septic documentation. Data were compiled from Siegrist et al, 2005 and McCray et al,2007. 
     
      use parm
      
      character* 4 sptnames(50)
      character*4 sptname
      character*80 titlesep
      character*70 sptfullname

      integer :: ist, isnum, eof

      real :: sptq,sptbodin,spttssconc,spttnconc,sptnh4conc,sptno3conc
      real :: sptno2conc,sptorgnconc,spttpconc,sptminp,sptorgp,sptfcoli

      isnum = 0
      eof = 0

!!    septic database filename present in file.cio
      if (septdb /= '             ') then
        open (171,file=septdb)
	

!!    read title lines
         do ii = 1, 4
          read (171,5999) titlesep
        end do
       
     
      do 
       
	  sptname = ""
	  idspttype = 0 
 	  sptq = 0.
	  sptbodin = 0.
	  spttssconc = 0.
	  spttnconc = 0.
	  sptnh4conc = 0.
	  sptno3conc = 0.
	  sptno2conc = 0.
	  sptorgnconc = 0.
	  spttpconc = 0.
	  sptminp = 0.
	  sptorgp = 0.
	  sptfcoli = 0.
     

        read (171,6000,iostat=eof)ist,sptname,sptfullname,idspttype,
     &  sptq,sptbodin,spttssconc,spttnconc,sptnh4conc,sptno3conc,
     &  sptno2conc,sptorgnconc,spttpconc,sptminp,sptorgp,sptfcoli

	  if (eof < 0) exit
        if (ist == 0) exit

	! Assign default values for missing data
	  if (sptnh4conc==0.and.sptno3conc==0) then
	    sptnh4conc = spttnconc * 0.8
	    sptno3conc = spttnconc * 0.2
	  endif
	  if (spttpconc==0.and.sptminp==0.and.sptorgp==0) then
	    sptminp = 5.1
	    sptorgp = 0.9
	  endif
	  
!	assign a flag to identify which septic system (1-26) should be selected

	  sptnames(ist) = sptname
 	  sptqs(ist) = sptq  
	  sptbodconcs(ist) = sptbodin
	  spttssconcs(ist) = spttssconc
	  spttnconcs(ist) = spttnconc
	  sptnh4concs(ist) = sptnh4conc
	  sptno3concs(ist) = sptno3conc
	  sptno2concs(ist) = sptno2conc
	  sptorgnconcs(ist) = sptorgnconc
	  spttpconcs(ist) = spttpconc
	  sptminps(ist) = sptminp
	  sptorgps(ist) = sptorgp
	  sptfcolis(ist) = sptfcoli
       
      end do
      else
      return
      endif

      close (171)
      return
 5999 format (a)
 6000 format (i3,1x,a4,1x,a70,i4/4x,10f8.3/4x,f8.3,f11.1)
 
      end