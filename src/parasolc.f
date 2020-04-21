!$$$$$$       subroutine parasolcce(nopt,nps,s,sf,sfz,bl,bu,xnstd,icalml,maxn,
!$$$$$$       *iseed,itel, iiname, inrhru, iinr,iobj,igoc, icalpar, isens, 
!$$$$$$      * isenspar,sensw,imet, varobj, omin,npt,is,bbound2,icd)
      subroutine parasolcce(nopt,nps,s,sf,sfz,bl,bu,xnstd,icalml,maxn,
     * itel, iiname, inrhru, iinr,iobj,igoc, icalpar, isens, 
     * isenspar,sensw,imet, varobj, omin,npt,is,bbound2,icd)

c  ALGORITHM GENERATE A NEW POINT(S) FROM A SUB-COMPLEX
c
c  SUB-COMPLEX VARIABLES


      implicit real*8 (a-h,o-z)
      real*8  sensw(isens),obj(iobj), ffz(iobj),varobj(iobj),vartot
      integer iiname(nopt), iinr(nopt),inrhru(nopt,2000),icd
      integer icalpar(4,iobj),isenspar(4,isens),isens, icalml
!$$$$$$       integer imet(nopt),igoc,nopt, nps,npt,iseed,itel,iobj
	integer imet(nopt),igoc,nopt, nps,npt,itel,iobj
      parameter (c1=0.8,c2=0.4)
      real*8 s(nps,nopt),sf(nps),bu(nopt),bl(nopt),xnstd(nopt)
      real*8 snew(nopt),omin(iobj),sfz(nps,igoc), bbound(2,nopt)
!$$$$$$       real*8 bound(nopt),criter(100)
	real*8 bbound2(2,nopt), bu2(nopt), bl2(nopt)
!$$$$$$       real*8 swq(nopt),sb(nopt),ce(nopt), a(nopt), unit(nopt)
      real*8 swq(nopt), sb(nopt), ce(nopt)
	integer is(50)
      real*8 fnew
c
c  LIST OF LOCAL VARIABLES
c    sb(.) = the best point of the simplex
c    swq(.) = the worst point of the simplex
c    fwqq = function value of the worst point
c    ce(.) = the centroid of the simplex excluding wo
c    snew(.) = new point generated from the simplex
c    iviol = flag indicating if constraints are violated
c          = 1 , yes
c          = 0 , no


	unit=1.
	do i=1,nopt
	!!!   code added by Nancy 04/07/2006 
!!!   bbound2(1,i) and bbound2(2,i) got too small for compiler
      if (bbound2(1,i) < 1.e-6) bbound2(1,i) = 0.0
	if (bbound2(2,i) < 1.e-6) bbound2(2,i) = 0.0
!!!   end code added
	bl2(i)=bbound2(1,i)
	bu2(i)=bbound2(2,i)
	bbound(1,i)=bl(i)
	bbound(2,i)=bu(i)
	end do
c
c  EQUIVALENCE OF VARIABLES FOR READABILTY OF CODE
	vartot=0.
	do mm=1,igoc
	vartot=vartot+varobj(mm)
	end do
      n = nps
      m = nopt
      alpha = 1.0
      beta = 0.2
c
c  IDENTIFY THE WORST POINT wo OF THE SUB-COMPLEX s
c  COMPUTE THE CENTROID ce OF THE REMAINING POINTS
c  COMPUTE step, THE VECTOR BETWEEN wo AND ce
c  IDENTIFY THE WORST FUNCTION VALUE fwqq
      do j = 1, m
        sb(j) = s(1,j)
        swq(j) = s(n,j)
        ce(j) = 0.0
        do i = 1, n-1
          ce(j) = ce(j) + s(i,j)
        end do
        ce(j) = ce(j)/dble(n-1)
      end do
      fwqq = sf(n)
c
c  COMPUTE THE NEW POINT snew
c
	do kk=1,2
c
c  CHECK IF snew SATISFIES ALL CONSTRAINTS
      call chkcst(nopt,snew,bl,bu,ibound)

c  FIRST TRY A REFLECTION STEP
      do j = 1, m
      snew(j) = ce(j) + alpha * (ce(j) - swq(j))/kk
      end do
c
c  CHECK IF snew SATISFIES ALL CONSTRAINTS
      call chkcst(nopt,snew,bl,bu,ibound)
c
c
c  snew IS OUTSIDE THE BOUND,
c  CHOOSE A POINT AT RANDOM WITHIN FEASIBLE REGION ACCORDING TO
c  A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
c  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD
!      if (ibound .ge. 1) call getpnt(nopt,2,iseed,snew,bl,bu,xnstd,
!     *sb)
!$$$$$$       if (ibound .ge. 1)call getpnt2(nopt,iseed,snew,bl,bu,bl2,bu2)
      if (ibound .ge. 1)call getpnt2(nopt,snew,bl,bu,bl2,bu2)
c
c
c  COMPUTE THE FUNCTION VALUE AT snew
      itel=itel+1
	icalml=icalml+1
	call functn(snew,nopt, iiname, inrhru, iinr, icalml,iobj,
     *	 icalpar, isens, isenspar, sensw,obj, imet,icd)
        fnew=0.
	  do mm=1,igoc
	  ffz(mm)=obj(mm)/(varobj(mm)*2)
	  fnew= fnew+ffz(mm)
	  omin(mm)=min(omin(mm),obj(mm))
        end do
	  fnew=fnew*vartot
	  write(18019,6666) icalml, (ffz(mm), mm=1,igoc), fnew

6666    format(i5, 41e12.5)	
c
c  COMPARE fnew WITH THE WORST FUNCTION VALUE fwqq
c
c  fnew IS LESS THAN fwqq, ACCEPT THE NEW POINT snew AND RETURN
      if (fnew .le. fwqq) go to 2000
      if (itel .ge. maxn) go to 3000
	end do
c
c
c  fnew IS GREATER THAN fwqq, SO TRY A CONTRACTION STEP
      do j = 1, m
        snew(j) = ce(j) - beta * (ce(j) - swq(j))
      end do
c
c  COMPUTE THE FUNCTION VALUE OF THE CONTRACTED POINT
      itel=itel+1
	icalml=icalml+1
	call functn(snew,nopt, iiname, inrhru, iinr, icalml,iobj,
     *	 icalpar, isens, isenspar, sensw,obj, imet,icd )
        fnew=0.
	  do mm=1,igoc
	  ffz(mm)=obj(mm)/(varobj(mm)*2)
	  fnew= fnew+ffz(mm)
	  omin(mm)=min(omin(mm),obj(mm))
        end do
	  fnew=fnew*vartot
	write(18019,6666) icalml, (ffz(mm), mm=1,igoc), fnew
c
c  COMPARE fnew TO THE WORST VALUE fwqq
c  IF fnew IS LESS THAN OR EQUAL TO fwqq, THEN ACCEPT THE POINT AND RETURN
      if (fnew .le. fwqq) go to 2000
      if (itel .ge. maxn) go to 3000
c
c
c  IF BOTH REFLECTION AND CONTRACTION FAIL, CHOOSE ANOTHER POINT
c  ACCORDING TO A NORMAL DISTRIBUTION WITH BEST POINT OF THE SUB-COMPLEX
c  AS MEAN AND STANDARD DEVIATION OF THE POPULATION AS STD

!$$$$$$  1000 if (ibound .ge. 1) call getpnt2(nopt,iseed,snew,bl,bu,bl2,bu2)
      if (ibound .ge. 1) call getpnt2(nopt,snew,bl,bu,bl2,bu2)
!     call getpnt(nopt,2,iseed,snew,bl,bu,xnstd,sb)
c
c  COMPUTE THE FUNCTION VALUE AT THE RANDOM POINT
      itel=itel+1
	icalml=icalml+1
	call functn(snew,nopt, iiname, inrhru, iinr, icalml,iobj,
     *	 icalpar, isens, isenspar, sensw,obj, imet,icd)
        fnew=0.
	  do mm=1,igoc
	  ffz(mm)=obj(mm)/(varobj(mm)*2)
	  fnew= fnew+ffz(mm)
	  omin(mm)=min(omin(mm),obj(mm))
        end do
	  fnew=fnew*vartot
	write(18019,6666) icalml, (ffz(mm), mm=1,igoc), fnew
c
c
c  REPLACE THE WORST POINT BY THE NEW POINT
 2000 continue

      do j = 1, m
        s(n,j) = snew(j)
      end do
      do j = 1, igoc
        sfz(n,j) = obj(j)
      end do
	sf(n) = fnew
	is(n)=icalml
 3000 continue

!$$$$$$  1812 format(i5,100e12.3)

c  END OF SUBROUTINE uncce
      return
      end
