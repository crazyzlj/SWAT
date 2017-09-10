      subroutine pgen(j)
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine generates precipitation data when the user chooses to 
!!    simulate or when data is missing for particular days in the weather file

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    idist       |none          |rainfall distribution code
!!                               |  0 for skewed distribution
!!                               |  1 for mixed exponential distribution
!!    j           |none          |HRU number
!!    i_mo        |none          |month being simulated
!!    npcp(:)     |none          |prior day category
!!                               |1 dry day
!!                               |2 wet day
!!    pcf(:,:)    |none          |normalization factor for precipitation
!!                               |generated from skewed distribution
!!    pcp_stat(:,1,:)|mm/day     |average amount of precipitation falling in
!!                               |one day for the month
!!    pcp_stat(:,2,:)|mm/day     |standard deviation for the average daily
!!                               |precipitation
!!    pcp_stat(:,3,:)|none       |skew coefficient for the average daily
!!                               |precipitation
!!    pr_w(1,:,:) |none          |probability of wet day after dry day in month
!!    pr_w(2,:,:) |none          |probability of wet day after wet day in month
!!    rcor        |none          |correction coefficient for generated rainfall
!!                               |to ensure that the annual means for generated
!!                               |and observed values are comparable. (needed
!!                               |only if IDIST=1)
!!    rexp        |none          |value of exponent for mixed exponential
!!                               |rainfall distribution (needed only if
!!                               |IDIST=1)
!!    rnd3(:)     |none          |random number between 0.0 and 1.0
!!    rndseed(:,:)|none          |random number seeds 
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    rnd3(:)     |none          |random number between 0.0 and 1.0
!!    subp(:)     |mm H2O        |precipitation for the day in HRU
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    pcpgen      |mm H2O        |generated precipitation value for the day
!!    r6          |none          |variable to hold intermediate calculation
!!    v8          |none          |random number between 0.0 and 1.0
!!    vv          |none          |random number between 0.0 and 1.0
!!    xlv         |none          |variable to hold intermediate calculation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~


!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Log
!!    SWAT: Aunif, Dstn1

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer, intent (in) :: j
 
      real :: vv, pcpgen, v8, r6, xlv

      pcpgen = 0.
      vv = 0.
      vv = Aunif(rndseed(idg(1),j))
      if (vv > pr_w(npcp(j),i_mo,hru_sub(j))) then
        pcpgen = 0.
      else
        v8 = 0.
        v8 = Aunif(rndseed(idg(3),j))
        if (idist == 0) then
          !!skewed rainfall distribution
          r6 = 0.
          xlv = 0.
          r6 = pcp_stat(i_mo,3,hru_sub(j)) / 6.
          xlv = (Dstn1(rnd3(j),v8) - r6) * r6 + 1.
          xlv = (xlv**3 - 1.) * 2. / pcp_stat(i_mo,3,hru_sub(j))
          rnd3(j) = v8
          pcpgen = xlv * pcp_stat(i_mo,2,hru_sub(j)) +                  
     &                                       pcp_stat(i_mo,1,hru_sub(j))
          pcpgen = pcpgen * pcf(i_mo,hru_sub(j))
        else
          !! mixed exponential rainfall distribution
          pcpgen = ((-Log(v8))**rexp) * pcp_stat(i_mo,1,hru_sub(j)) *   
     &                                                              rcor
        end if
        if (pcpgen < .1) pcpgen = .1
      end if

      subp(j) = pcpgen

      if (ievent > 0 .and. subp(j) >= 0.01 .and. pcpsim == 2) then
        call pgenhr(j)
      endif

      return
      end