!$$$$$$       subroutine parasolin(maxn,kstop,pcento,iseed,nopt,iobj,npt,
!$$$$$$      &  nintval,igoc,ngs,npg,nps,nspl, istat, iprob)
	subroutine parasolin(maxn,kstop,pcento,nopt,iobj,npt,
     &  nintval,igoc,ngs,npg,nps,nspl, istat, iprob)
	
c   THIS SUBROUTINE READS AND PRINTS THE INPUT VARIABLES FOR
c   PARASOL METHOD FOR GLOBAL OPTIMIZATION
c
c   WRITTEN BY QINGYUN DUAN - UNIVERSITY OF ARIZONA, APRIL 1992
c   Modified by Ann van Griensven - UNIVERSITY OF CALIFORNIA RIVERSIDE, APRIL 2003
c
c
      use parm
!     !  !         !         !         !         !         !         ! !
      implicit real*8 (a-h,o-z)
	integer nopt,npt,nintval,igoc,ngs,npg,nps,nspl,istat, iprob
      integer iseedval
!$$$$$$       character*10 pcntrl,deflt,usrsp,  pname
      character*10 pcntrl,deflt,usrsp
!$$$$$$       character*8 reduc,initl,ysflg,noflg
      character*8 ysflg,noflg
      data deflt/' DEFAULT  '/
      data usrsp/'USER SPEC.'/
      data ysflg/'YES '/
      data noflg/'NO  '/
      data pcntrl/'          '/
c
      write (*,*) ' ENTER THE SCEIN SUBROUTINE --- '
c

c
c  INITIALIZE I/O VARIABLES
c
      ierror = 0
      iwarn = 0


        write(18011,*) " sce control parameters "
	
!$$$$$$        ideflt = 0
       pcntrl = deflt
      read(18009,*) maxn
	read(18009,*) kstop
	read(18009,*) pcento
	read(18009,*) ngs
	read(18009,*) iseedval
      if (iseedval .eq. 0) then
        iseed(1) = 1969
      else
        iseed(1) = iseedval
      end if
	read(18009,*) 
	read(18009,*) 
	read(18009,*) nspl
	read(18009,*) istat
	read(18009,*) iprob
	read(18009,*) igoc
	if (igoc.eq.0) igoc=iobj

	read(18009,*) nintval


      noptmax=min(nopt,16)
        npg = 2*noptmax + 1
        nps = noptmax + 1
        nspl = npg



c  CHECK IF THE SCE CONTROL PARAMETERS ARE VALID
      if (ngs .lt. 1 .or. ngs .ge. 1320) then
        write(18011,900) ngs
  900   format(//,1x,'**ERROR** NUMBER OF COMPLEXES IN INITIAL ',
     *         ' POPULATION ',i5,' IS NOT A VALID CHOICE')
        ierror = ierror + 1
      end if
c
      if (kstop .lt. 0 .or. kstop .ge. 20) then
        write(18011,901) kstop
  901   format(//,1x,'**WARNING** THE NUMBER OF SHUFFLING LOOPS IN',
     *  ' WHICH THE CRITERION VALUE MUST CHANGE ',/,13x,'SHOULD BE',
     *  ' GREATER THAN 0 AND LESS THAN 10.  ','kstop = ',i2,
     *  ' WAS SPECIFIED.'/,13x,'BUT kstop = 5 WILL BE USED INSTEAD.')
        iwarn = iwarn + 1
        kstop=5
      end if


c
      if (nspl .lt. 1) then
        write(18011,905) nspl
  905   format(//,1x,'**WARNING** THE NUMBER OF EVOLUTION STEPS ',
     *         'TAKEN IN EACH COMPLEX BEFORE SHUFFLING ',I4,/,13x,
     *         'IS NOT A VALID CHOICE, SET IT TO DEFAULT')
        iwarn = iwarn + 1
        nspl = npg
      end if
c
c  COMPUTE THE TOTAL NUMBER OF POINTS IN INITIAL POPULATION

      npt = ngs * npg


c
c  PRINT OUT THE TOTAL NUMBER OF ERROR AND WARNING MESSAGES
      if (ierror .ge. 1) write(18011,907) ierror
  907 format(//,1x,'*** TOTAL NUMBER OF ERROR MESSAGES IS ',i2)
c
      if (iwarn .ge. 1) write(18011,908) iwarn
  908 format(//,1x,'*** TOTAL NUMBER OF WARNING MESSAGES IS ',i2)
c


c
c
c  PRINT SHUFFLED COMPLEX EVOLUTION OPTIMIZATION OPTIONS
!$$$$$$   104 write(18011,910)
!$$$$$$   910 format(//,2x,'SCE CONTROL',5x,'MAX TRIALS',5x,
!$$$$$$      &'REQUIRED IMPROVEMENT',5x,'RANDOM',/,3x,'PARAMETER',8x,
!$$$$$$      &'ALLOWED',6x,'PERCENT',4x,'NO. LOOPS',6x,'SEED',/,
!$$$$$$      &2x,11(1h-),5x,10(1H-),5x,7(1h-),4x,9(1h-),5x,6(1h-))
c
      pcenta=pcento*100.
      write(18011,912) pcntrl,maxn,pcenta,kstop,iseed(1)
  912 format(3x,a10,7x,i5,10x,f3.1,9x,i2,9x,i5)
      write(18011,914) ngs,npg,npt,nps,nspl
  914 format(//,18x,'SCE ALGORITHM CONTROL PARAMETERS',/,18x,32(1H=),
     &//,2x,'NUMBER OF',5x,'POINTS PER',5x,'POINTS IN',6x,'POINTS PER',
     &4x,'EVOL. STEPS',/,2x,'COMPLEXES',6X,'COMPLEX',6x,'INI. POPUL.',
     &5x,'SUB-COMPLX',4x,'PER COMPLEX',/,2x,9(1h-),5x,10(1h-),4x,
     &11(1h-),5x,10(1h-),4x,11(1h-),5x,/,2x,5(i5,10x))
      write(18011,916)
  916 format(//,8x,'INITIAL PARAMETER VALUES AND PARAMETER BOUNDS',/,
     &       8x,45(1h=),//,2x,'PARAMETER',5x,'INITIAL VALUE',5x,
     &       'LOWER BOUND',5x,'UPPER BOUND',/,2x,9(1h-),5x,13(1h-),5x,
     &       11(1h-),5x,11(1h-))
      if (ierror .ge. 1) then
      write(18011,922)
  922 format(//,'*** THE OPTIMIZATION SEARCH IS NOT CONDUCTED BECAUSE',
     &       ' OF INPUT DATA ERROR ***')
      end if

!     Initialize the random generator
      call random_seed
      call random_seed(size=n)
      call random_seed(put = iseed)
c
C  END OF SUBROUTINE SCEIN

      return
      end
