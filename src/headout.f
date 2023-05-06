      subroutine headout

!!     ~ ~ ~ PURPOSE ~ ~ ~
!!     this subroutine writes the headings to the major output files

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hedb(:)     |NA            |column titles in subbasin output files
!!    hedr(:)     |NA            |column titles in reach output files
!!    hedrsv(:)   |NA            |column titles in reservoir output files
!!    heds(:)     |NA            |column titles in HRU output files
!!    hedwtr(:)   |NA            |column titles in HRU impoundment output
!!                               |file
!!    icolb(:)    |none          |space number for beginning of column in
!!                               |subbasin output file
!!    icolr(:)    |none          |space number for beginning of column in
!!                               |reach output file
!!    icolrsv(:)  |none          |space number for beginning of column in
!!                               |reservoir output file
!!    icols(:)    |none          |space number for beginning of column in
!!                               |HRU output file
!!    ipdvab(:)   |none          |output variable codes for output.sub file
!!    ipdvar(:)   |none          |output variable codes for .rch file
!!    ipdvas(:)   |none          |output variable codes for output.hru file
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    itotb       |none          |number of output variables printed (output.sub)
!!    itotr       |none          |number of output variables printed (.rch)
!!    itots       |none          |number of output variables printed (output.hru)
!!    msubo       |none          |maximum number of variables written to
!!                               |subbasin output file (output.sub)
!!    mhruo       |none          |maximum number of variables written to
!!                               |HRU output file (output.hru)
!!    mrcho       |none          |maximum number of variables written to
!!                               |reach output file (.rch)
!!    prog        |NA            |program name and version
!!    title       |NA            |title lines from file.cio
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ilen        |none          |width of data columns in output file
!!    j           |none          |counter
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    header

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: j, ilen

      call header

!! write headings to HRU output file (output.hru)
      write (28,1000) prog, values(2), values(3), values(1), values(5),
     &               values(6), values(7)
      write (28,1010) title


      if (ipdvas(1) > 0) then
        if (icalen == 0) write (28,1020) (heds(ipdvas(j)), j = 1, itots) !!custom printout
        if (icalen == 1) write (28,1021) (heds(ipdvas(j)), j = 1, itots) !!custom printout
	else
        if (icalen == 0) write (28,1020) (heds(j), j = 1, mhruo)         !!default printout
	  if (icalen == 1) write (28,1021) (heds(j), j = 1, mhruo)         !!default printout
      endif

!! write headings to HRU output file (output2.hru)
      if (isproj == 1) then
        write (21,1000)prog, values(2), values(3), values(1), values(5),
     &               values(6), values(7)
        write (21,1010) title
      if (ipdvas(1) > 0) then
        write (21,1020) (heds(ipdvas(j)), j = 1, itots) !!custom printout
      else
        write (21,1020) (heds(j), j = 1, mhruo)         !!default printout
      endif
      endif

!! write headings to subbasin output file (output.sub)
      write (31,1000) prog, values(2), values(3), values(1), values(5),
     &    values(6), values(7)
      write (31,1010) title


      if (ipdvab(1) > 0) then
        if (icalen == 0) write (31,1030) (hedb(ipdvab(j)), j = 1, itotb) !!custom printout
        if (icalen == 1) write (31,1031) (hedb(ipdvab(j)), j = 1, itotb) !! month/day/yr print
      else
        if (icalen == 0) write (31,1030) (hedb(j), j = 1, msubo)         !!default printout
        if (icalen == 1) write (31,1031) (hedb(j), j = 1, msubo)         !!month/day/yr print
1031  format (//6x,' SUB      GIS  MO DA  YR   AREAkm2',24(a10))
      endif

!! write headings to reach output file (output.rch)
      write (7,1000) prog, values(2), values(3), values(1), values(5),
     &               values(6), values(7)
      write (7,1010) title


      if (ipdvar(1) > 0) then
        if (iprint /= 3) then
         if (icalen == 0) write (7,1040) (hedr(ipdvar(j)), j = 1, itotr)  !! daily/monthly output - julian day
         if (icalen == 1) write (7,1042) (hedr(ipdvar(j)), j = 1, itotr)  !! daily output - calendar day
 1042 format (//7x,'RCH      GIS  MO DA   YR     AREAkm2',58a12)
        
	  else
	    write (7,1041) (hedr(ipdvar(j)), j = 1, itotr)  !! subdaily output
	  endif
      else     !! default printout
         if (iprint /= 3) then
           if (icalen == 0) write (7,1040) (hedr(j), j = 1, mrcho)       !! daily/monthly output - julian day
           if (icalen == 1) write (7,1042) (hedr(j), j = 1, mrcho)       !! daily output - calendar day
 	  else
            write (7,1041) (hedr(j), j = 1, mrcho)          !! subdaily output
 	  endif
      endif

!! write headings to reach output file (output2.rch)
      if (isproj == 1) then
        write (20,1000)prog, values(2), values(3), values(1), values(5),
     &               values(6), values(7)
        write (20,1010) title
      if (ipdvar(1) > 0) then
        write (20,1040) (hedr(ipdvar(j)), j = 1, itotr)  !! custom printout
      else
        write (20,1040) (hedr(j), j = 1, mrcho)          !! default printout
      endif
      endif

!! write headings to reservoir output file (output.rsv)
      write (8,1000) prog, values(2), values(3), values(1), values(5),
     &               values(6), values(7)
      write (8,1010) title
      write (8,1050) (hedrsv(j), j = 1, 41)
!! write headings to reservoir output file (output2.rsv)
      if (isproj == 1) then
      write (22,1000) prog, values(2), values(3), values(1), values(5),
     &               values(6), values(7)
      write (22,1010) title
      write (22,1050) (hedrsv(j), j = 1, 41)
      end if

!! write headings to HRU impoundment output file (output.wtr)
      if (iwtr == 1) then
        write (29,1000)prog, values(2), values(3), values(1), values(5),
     &                values(6), values(7)
        write (29,1010) title
        write (29,1020) (hedwtr(j), j = 1, 40)
      end if

!! write headings to pesticide output file (output.pes)
      if (iprp /= 0) then
        write (30,1000)prog, values(2), values(3), values(1), values(5),
     &                values(6), values(7)
        write (30,1010) title
        write (30,3000)
        write (30,3001) (npno(j),npno(j), j = 1, npmx)
        write (30,3002) (pname(npno(j)),pname(npno(j)), j = 1, npmx)
        write (30,3003) (("SOLUBLE mg       SORBED mg"), j = 1, npmx)
      end if
!! Jaehak subdaily bmp output header
!bmp-sedfil.out
      write(77778,'(a21)') 'SED-FIL Basins output'
      write(77778,'(a200)') '------------------------------   ----------
     &---------------- Sedimentation Pond ------------------------------
     &-----   ----------------------------------- Sand Filter ----------
     &-----------------------'
      write(77778,'(5a6,30a12)') 'year', 'day','sub','SFnum',
     & 'inflw(m3)','outflw(m3)','bypass(m3)','recharg(m3)','sedin(kg)',
     & 'sedout(kg)','sbypass(kg)','inflw(m3)','outflw(m3)','bypass(m3)',
     & 'recharg(m3)','sedin(kg)','sedout(kg)','sbypass(kg)'

!bmp-ri.out
      write(77779,'(a21)') 'Retention-Irrigation output'
      write(77779,'(5a6,30a12)') 'year', 'day','sub','RInum',
     & 'inflw(m3)','qbypass(m3)','pmpflw(m3)','pmpflw(m3)','sedin(kg)',
     & 'sbypass(kg)','pmpsed(kg)'

      if (iprp == 2) then
        open (2222,file='weather_day.out',recl=800)
        write (2222,2222)
	end if

      return

 2222	format (2x,'SUB',5x,'SUBGIS',1x,' DAY',3x,' YEAR',6x,
     &'SUB_KM',11x,' SLR',5x,'WND',4x,'RELH')

 1000 format ('1',/t5,a80,t105,2(i2,'/'),i4,5x,2(i2,':'),i2)
 1010 format (/(t5,20a4))
 1020 format (//'LULC  HRU       GIS  SUB  MGT  MON','   AREAkm2',
     & 79(a10))
 1021  format (//'LULC  HRU       GIS  SUB  MGT MO DA   YR',
     &'   AREAkm2', 79(a10))
 1030 format (//6x,' SUB      GIS  MON   AREAkm2',24(a10))
 1040 format (//7x,'RCH      GIS   MON     AREAkm2',58a12)
 1041 format (//7x,'RCH      GIS   DAY   DET     AREAkm2',56a12)
 1050 format (//6x,'     RES  MON',41a12)
 1060 format (//6x,'RCH GIS  MON',26a12)
 2000 format (a12,12x,i4,4x,i4)
 3000 format ("Pesticide loadings to main channel by HRU",/)
 3001 format ("Pesticide #",250(18x,i3,1x))
 3002 format ("Pesticide name:      ",250(a16,1x))
 3003 format (4x,'GISnum YEAR MON',7x,125(a26,8x))
      end
