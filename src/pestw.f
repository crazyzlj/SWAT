      subroutine pestw
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this suroutine writes summary information on pesticide fate in watershed

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    hru_dafr(:) |none          |fraction of watershed area in HRU
!!    isproj      |none          |special project code:
!!                               |1 test rewind (run simulation twice)
!!    nhru        |none          |number of HRUs in watershed
!!    npmx        |none          |number of different pesticides used in
!!                               |the simulation
!!    npno(:)     |none          |array of unique pesticides used in watershed
!!    plt_pst(:,:)|kg/ha         |pesticide on plant foliage
!!    sol_nly(:)  |none          |number of layers in soil profile
!!    sol_pst(:,:,:)|kg/ha       |pesticide in soil layer
!!    wpstaao(:,1)|mg pst/ha     |amount of pesticide type in surface runoff
!!                               |contribution to stream in watershed 
!!                               |(in solution) - average annual
!!    wpstaao(:,2)|mg pst/ha     |amount of pesticide type in surface runoff
!!                               |contribution to stream in watershed
!!                               |(sorbed to sediment) -average annual
!!    wpstaao(:,3)|kg pst/ha     |amount of pesticide type leached from soil
!!                               |profile in watershed - average annual
!!    wpstaao(:,4)|kg pst/ha     |amount of pesticide type in lateral flow
!!                               |contribution to stream in watershed -
!!                               |average annual
!!    wshd_pstap(:)|kg pst/ha     |average annual amount of pesticide type
!!                               |applied in watershed during simulation
!!    wshd_pstdg(:)|kg pst/ha     |average annual amount of pesticide lost
!!                               |through degradation in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    wpstaao(:,3)|mg pst/ha     |amount of pesticide type leached from soil
!!                               |profile in watershed on day
!!    wpstaao(:,4)|mg pst/ha     |amount of pesticide type in lateral flow
!!                               |contribution to stream in watershed on day
!!    wshd_pstap(:)|mg pst/ha     |average annual  amount of pesticide type
!!                               |applied in watershed during simulation
!!    wshd_pstdg(:)|mg pst/ha     |average annual amount of pesticide lost
!!                               |through degradation in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    j           |none          |counter (HRU number)
!!    k           |none          |counter
!!    kk          |none          |pesticide identification number from database
!!    l           |none          |counter (soil layer)
!!    pfp         |mg/ha         |pesticide on plants at end of simulation
!!    pfg         |mg/ha         |pesticide in soil at end of simulation
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    Intrinsic: Maxval

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      integer :: k, kk, j, l
      real*8 :: pfp, pfg

      do k = 1, npmx
        kk = 0
        kk = npno(k)

        !! change units from kg/ha to mg/ha
        wpstaao(k,3) = wpstaao(k,3) * 1.e6
        wpstaao(k,4) = wpstaao(k,4) * 1.e6
        wshd_pstap(k) = wshd_pstap(k) * 1.e6
        wshd_pstdg(k) = wshd_pstdg(k) * 1.e6

        if (iscen == 1) then
        write (26,5000) kk, pname(kk), wshd_pstap(k), wshd_pstdg(k),    
     &            wpstaao(k,1), wpstaao(k,2), wpstaao(k,3), wpstaao(k,4)
        else if (isproj == 1) then
        write (19,5000) kk, pname(kk), wshd_pstap(k), wshd_pstdg(k),    
     &            wpstaao(k,1), wpstaao(k,2), wpstaao(k,3), wpstaao(k,4)
        endif

        !!calculate pesticide levels on plants/in soil at end of simulation
        pfp = 0.
        pfg = 0.
        do j = 1, nhru
          if (hrupest(j) == 1) then
            pfp = pfp + plt_pst(k,j) * hru_dafr(j)
            do l = 1, sol_nly(j)
              pfg = pfg + sol_pst(k,j,l) * hru_dafr(j)
            end do
          end if
        end do
        !! change units from kg/ha to mg/ha
        pfp = pfp * 1.e6
        pfg = pfg * 1.e6
        if (iscen == 1) then
        write (26,5100) pfp, pfg
        else if (isproj == 1) then
        write (19,5100) pfp, pfg
        endif
      end do

      return
 5000 format (/,'AVERAGE ANNUAL PESTICIDE SUMMARY DATA, PESTICIDE #',i3,
     &    ': ',a16,/,t20,'APPLIED = ',f15.4,' mg/ha',/,t20,'DECAYED = ',
     &    f15.4,' mg/ha',/,t20,                                         
     &    'IN SURFACE RUNOFF ENTERING STREAM (DISSOLVED) = ',f15.4,     
     &    ' mg/ha',/,t20,'IN SURFACE RUNOFF ENTERING STREAM (SORBED) = '
     &    ,f15.4,' mg/ha',/,t20, 'LEACHED OUT OF SOIL PROFILE = ',f15.4,
     &    ' mg/ha',/,t20,'IN LATERAL FLOW ENTERING STREAM = ',f15.4,    
     &    ' mg/ha')
 5100 format (/,t20,'FINAL AMOUNT OF PESTICIDE ON PLANT = ',f15.4,      
     &    ' mg/ha',/,t20,'FINAL AMOUNT OF PESTICIDE IN GROUND = ',f15.4,
     &    ' mg/ha')
 5200 format (//,'PESTICIDE RISK BY ROUTING REACH',/,'REACH',t12,
     *    '4 DAY',t30,'21 DAY',t49,'60 DAY',t69,'90 DAY',/,t6,
     *    'DAY YEAR CON(PPB)',t24,'DAY YEAR CON(PPB)',t42,
     *    'DAY YEAR CON(PPB)',t60,'DAY YEAR CON(PPB)',t80,'% FLOW')
 5300 format (i4,4(2i4,e10.3),f8.2)
      end