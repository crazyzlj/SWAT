      subroutine pmeas
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine reads in precipitation data and assigns it to the 
!!    proper subbasins

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition  
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_sub(:)  |none          |number of subbasin in which HRU is located
!!    i           |julian date   |current day of simulation
!!    id1         |julian date   |first day of simulation in current year
!!    ievent      |none          |rainfall/runoff code
!!                               |0 daily rainfall/curve number technique
!!                               |1 sub-daily rainfall/Green&Ampt/hourly
!!                               |  routing
!!                               |3 sub-daily rainfall/Green&Ampt/hourly routing
!!    ifirstpcp(:)|none          |precipitation data search code
!!                               |0 first day of precipitation data located in 
!!                               |  file
!!                               |1 first day of precipitation data not located
!!                               |  in file
!!    irgage(:)   |none          |HRU rain gage data code (gage # for rainfall
!!                               |data used in HRU)
!!    iyr         |none          |beginning year of simulation
!!    mrg         |none          |maximum number of rainfall/temp gages
!!    nhru        |none          |number of HRUs in watershed
!!    nrgage      |none          |number of raingage files
!!    nrgfil      |none          |number of rain gage per file
!!    nrtot       |none          |total number of rain gages
!!    nstep       |none          |number of lines of rainfall data for each
!!                               |day
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    ifirstpcp(:)|none          |precipitation data search code
!!                               |0 first day of precipitation data located in 
!!                               |  file
!!                               |1 first day of precipitation data not located
!!                               |  in file
!!    rainsub(:,:)|mm H2O        |precipitation for the time step during the 
!!                               |day in HRU
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    a           |NA            |check for hour/min ":" on first time step of
!!                               |day
!!    flag        |none          |flag to locate first day of simulation
!!                               |in precipitation file
!!    hrmeas(:,:) |mm H2O        |precipitation falling in hour on day of
!!                               |simulation
!!    idap        |julian date   |julian date of measured weather data
!!    ihour       |none          |hour of measured weather data (0 - 23)
!!    ii          |none          |counter
!!    imin        |none          |minute of measured weather data within hour
!!    inum3sprev  |none          |subbasin number of previous HRU
!!    iyp         |none          |last 2 digits of year measured weather data
!!    k           |none          |counter
!!    kk1         |none          |gage code for first dataset in weather file
!!    kk2         |none          |gage code for last dataset in weather file
!!    l           |none          |counter
!!    rainsb(:,:) |mm H2O        |precipitation falling in time increment 
!!                               |defined by IDT
!!    rbsb        |mm H2O        |generated precipitation for subbasin
!!    rmeas(:)    |mm H2O        |precipitation read in from file
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    SWAT subroutines: pgen

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      character (len=1) :: a
      integer :: k, kk1, kk2, iyp, idap, l, inum3sprev, ii
      integer :: ihour, imin, flag
      real :: rbsb
      real, dimension (mrg) :: rmeas
      real, dimension (:,:), allocatable :: rainsb
!     real, dimension (:), allocatable :: rhrbsb, rstpbsb
      if (nstep > 0) then
        allocate (rainsb(mrg,nstep))
!       allocate (rstpbsb(nstep))
!        allocate (hrmeas(mrg,25))
!       allocate (rhrbsb(24))
      end if
      
      inum3sprev = 0

      !! initialize variables for the day
      rmeas = 0.
      if (nstep > 0) then
        rainsb = 0.
!       hrmeas = 0.
      end if
      

      select case (ievent)
         case (0)                       !!daily rainfall
    
          !! read precipitation data from files
          do k = 1, nrgage
            !! calculate gage id codes for first and last dataset in file
            kk1 = 0
            kk2 = 0
            kk1 = nrgfil * (k - 1) + 1
            if (k == nrgage) then
              kk2 = nrtot
            else
              kk2 = kk1 + (nrgfil - 1)
            end if

            !! read data from file
            if (ifirstpcp(k) == 0) then
              read (100+k,5000) (rmeas(l), l = kk1, kk2)
            else
              ifirstpcp(k) = 0
              do
                iyp = 0
                idap = 0
                read (100+k,5100) iyp, idap, (rmeas(l), l = kk1, kk2)
                if (iyp + idap <= 0) exit
                if (iyp == iyr .and. idap == id1) exit
              end do
            end if
          end do

          !! assign precipitation data to HRUsoutput.std
          
          inum3sprev = 0
          do k = 1, nhru
            subp(k) = rmeas(irgage(hru_sub(k)))
            !! generate data to replace missing values
            if (subp(k) < -97.) then
              !! use same generated data for all HRUs in a subbasin
              if (hru_sub(k) == inum3sprev .and. hru_sub(k) /= 0) then
                subp(k) = rbsb
                if (ievent == 1) then
                  do l = 1, nstep
                    rainsub(k,l) = rstpbsb(l)
                  end do
                end if
              else
                call pgen(k)
                !! set subbasin generated values
                inum3sprev = 0
                rbsb = 0.
                inum3sprev = hru_sub(k)
                rbsb = subp(k)
                if (ievent == 1) then
                  rstpbsb(:) = 0.
                  do l = 1, nstep
                    rstpbsb(l) = rainsub(k,l)
                  end do
                end if
              end if
            else
              if (ievent == 1 .and. subp(k) >= 0.01) call pgenhr(k)
            end if
          end do

        case (1)                   !! subdaily precipitation

          !! read precipitation data from files
          do k = 1, nrgage
            !! calculate gage id codes for first and last dataset in file
            kk1 = 0
            kk2 = 0
            kk1 = nrgfil * (k - 1) + 1
            if (k == nrgage) then
              kk2 = nrtot
            else
              kk2 = kk1 + (nrgfil - 1)
            end if

            !! read data from file
            if (ifirstpcp(k) == 0) then
              read (100+k,5300) a
              backspace (100+k)
              if (a /= " ") then               !subdaily precip on day
                do ii = 1, nstep
                  flag = 0
                  ihour = 0
                  imin = 0
                  a = ""
                  read (100+k,5200) iyp, idap, ihour, imin,             
     &                                      (rainsb(l,ii), l = kk1, kk2)
				   if (iyp /= iyr .or. idap /= i) flag = 1
                  if (flag == 1) then
                    write (24,5400) iyr, i
      !!              stop
                  end if
                  do l = kk1, kk2
                    if (rainsb(l,ii)<-97) then
                        call pgen(k)
                        rainsb(l,ii) = subp(k) / nstep
                    endif
                    rmeas(l) = rmeas(l) + rainsb(l,ii)
                  end do
                end do
              else                                 !no precip on day
                read (100+k,5201) iyp, idap, (rmeas(l), l = kk1, kk2)
                  if (iyp /= iyr .or. idap /= i) flag = 1
                  if (flag == 1) then
                    write (24,5400) iyr, i
     !!               stop
                  end if
                do l = kk1, kk2
                  do ii = 1, nstep
                    rainsb(l,ii) = 0.
                  end do
                  rmeas(l) = 0.
                end do
              end if
            else
              ifirstpcp(k) = 0
              flag = 0
              do
                iyp = 0
                idap = 0
                read (100+k,5202) iyp, idap, ihour, a, imin,            
     &                                       (rainsb(l,1), l = kk1, kk2)
                if (iyp == iyr .and. idap == i) flag = 1
                if (flag == 1) then
                  if (a /= " ") then
                    do l = kk1, kk2
                      if (rainsb(l,1)<-97) then
                         call pgen(k)
                         rainsb(l,1) = subp(k) / nstep
                      endif
                      rmeas(l) = rmeas(l) + rainsb(l,1)
                    end do
                    do ii = 2, nstep
                      ihour = 0
                      imin = 0
                      read (100+k,5200) iyp, idap, ihour, imin,         
     &                                      (rainsb(l,ii), l = kk1, kk2)
                      do l = kk1, kk2
                        if (rainsb(l,1)<-97) then
                           call pgen(k)
                           rainsb(l,1) = subp(k) / nstep
                        endif
                        rmeas(l) = rmeas(l) + rainsb(l,ii)
                      end do
                    end do
                  else
                    do l = kk1, kk2
                      rmeas(l) = rainsb(l,1)
                      do ii = 1, nstep
                        rainsb(l,ii) = 0.
                      end do
                    end do
                  end if
                end if
                if (flag == 1) exit
              end do
            end if
          end do

          !! assign precipitation data to HRUs
          !! missing precipitation data cannot be generated for 
          !! sub-daily simulation
          do k = 1, nhru
            subp(k) = rmeas(irgage(hru_sub(k)))
            do ii = 1, nstep
              rainsub(k,ii) = rainsb(irgage(hru_sub(k)),ii)
            end do
            !! generate data to replace missing values
            if (subp(k) < -97.) then
              !! use same generated data for all HRUs in a subbasin
              if (hru_sub(k) == inum3sprev .and. hru_sub(k) /= 0) then
                subp(k) = rbsb
                if (ievent == 1) then
                  do l = 1, nstep
                    rainsub(k,l) = rstpbsb(l)
                  end do
                end if
              else
                call pgen(k)
                !! set subbasin generated values
                inum3sprev = 0
                rbsb = 0.
                inum3sprev = hru_sub(k)
                rbsb = subp(k)
                rstpbsb(:) = 0.
                do l = 1, nstep
                  rstpbsb(l) = rainsub(k,l)
                end do
              end if
            end if
          end do

      end select

      if (nstep > 0) then
        deallocate (rainsb)
      end if


      return
 5000 format (7x,1800f5.1)
 5100 format (i4,i3,1800f5.1)
 5200 format (i4,i3,i2,1x,i2,300f6.2)
 5201 format (i4,i3,5x,300f5.1)
 5202 format (i4,i3,i2,a1,i2,300f6.2)
 5300 format (9x,a1)
 5400 format (10x,"ERROR: Precipitation data dates do not match for",   
     &       " simulation year: ",i4," and julian date: ",i3)
      end