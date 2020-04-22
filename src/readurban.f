      subroutine readurban

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine reads input parameters from the urban database
!!     (urban.dat). Information from this database is used only if the 
!!     urban buildup/washoff routines are selected for the modeling of 
!!     urban areas.

!!     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!     name        |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     mudb        |none            |maximum number of urban landuses in
!!                                  |database
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!     name        |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     curbden(:)  |km/ha           |curb length density in HRU
!!     dirtmx(:)   |kg/curb km      |maximum amount of solids allowed to
!!                                  |build up on impervious surfaces
!!     fcimp(:)    |fraction        |fraction of HRU area that is classified
!!                                  |as directly connected impervious
!!     fimp(:)     |fraction        |fraction of HRU area that is 
!!                                  |impervious (both directly and 
!!                                  |indirectly connected)
!!     thalf(:)    |days            |time for the amount of solids on
!!                                  |impervious areas to build up to 1/2
!!                                  |the maximum level
!!     tnconc(:)   |mg N/kg sed     |concentration of total nitrogen in
!!                                  |suspended solid load from impervious
!!                                  |areas
!!     tno3conc(:) |mg NO3-N/kg sed |concentration of NO3-N in suspended
!!                                  |solid load from impervious areas
!!     tpconc(:)   |mg P/kg sed     |concentration of total phosphorus in
!!                                  |suspended solid load from impervious
!!                                  |areas
!!     urbcn2(:)   |none            |SCS curve number for moisture condition
!!                                  |II in impervious areas
!!     urbcoef(:)  |1/mm            |wash-off coefficient for removal of
!!                                  |constituents from an impervious surface
!!     urbname(:)  |NA              |name of urban land use
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!     name        |units           |definition
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!     eof         |none            |end of file flag
!!     iu          |none            |counter which represents the array
!!                                  |storage number of the urban data
!!                                  |the array storage number is used by the
!!                                  |model to access data for a specific 
!!                                  |urban land type
!!     iunum       |none            |number of urban land type (reference
!!                                  |only)
!!     ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!     ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
   
      integer :: iu, iunum, eof
      character (len=4) :: unam
      real*8 :: fimpu, fcimpu, crbdn, ucoef, dtmx, thlf, tncnc, tpcnc
      real*8 :: tno3cnc

      iunum = 0
      eof = 0

      do 
        crbdn = 0.
        dtmx = 0.
        fcimpu = 0.
        fimpu = 0.
        thlf = 0.
        tncnc = 0.
        tno3cnc = 0.
        tpcnc = 0.
        ucoef = 0.
        unam = ""

        read (108,5000,iostat=eof) iu, unam, fimpu, fcimpu, crbdn,      
     &     ucoef, dtmx, thlf, tncnc, tpcnc, tno3cnc, urbcn
        if (eof < 0) exit

        if (iu == 0) exit

        urbname(iu) = unam
        fimp(iu) = fimpu
        fcimp(iu) = fcimpu
        curbden(iu) = crbdn
        urbcoef(iu) = ucoef
        dirtmx(iu) = dtmx
        thalf(iu) = thlf
        tnconc(iu) = tncnc
        tpconc(iu) = tpcnc
        tno3conc(iu) = tno3cnc
        urbcn2(iu) = urbcn

        !! check values
        if (fimp(iu) <= 0.) fimp(iu) = 0.05
        if (fimp(iu) >= 1.) fimp(iu) = 0.99
        if (fcimp(iu) <= 0.) fcimp(iu) = 0.05
        if (fcimp(iu) >= 1.) fcimp(iu) = 0.99
        if (thalf(iu) <= 0.) thalf(iu) = 1.
        if (dirtmx(iu) <= 0.) dirtmx(iu) = 1000.
        if (urbcn2(iu) <= 0.) urbcn2(iu) = 98.
      end do

      close (108)
      return
 5000 format (i3,1x,a4,56x,2f8.3/4x,7f8.3,f6.1)
      end