      subroutine anfert
      
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    this subroutine automatically applies Nitrogen and Phosphorus when
!!    Nitrogen stress exceeds a user input threshhold.  

!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    fminn(:)    |kg minN/kg frt|fraction of fertilizer which is mineral
!!                               |nitrogen (NO3 + NH3)
!!    fminp(:)    |kg minP/kg frt|fraction of fertilizer which is mineral
!!                               |phosphorus
!!    fnh3n(:)    |kg NH3-N/kg N |fraction of mineral N content of
!!                               |fertilizer which is NH3
!!    forgn(:)    |kg orgN/kg frt|fraction of fertilizer which is organic
!!                               |nitrogen
!!    forgp(:)    |kg orgP/kg frt|fraction of fertilizer which is organic
!!                               |phosphorus
!!    afrt_surface(:) |none          |fraction of fertilizer which is applied
!!                               |to top 10 mm of soil (the remaining
!!                               |fraction is applied to first soil
!!                               |layer)
!!    auto_nyr(:) |kg NO3-N/ha   |maximum NO3-N content allowed to be
!!                               |applied in one year by auto-fertilization
!!    auto_napp(:)|kg NO3-N/ha   |maximum NO3-N content allowed in one
!!                               |fertilizer application
!!    auto_nstrs(:)|none          |nitrogen stress factor which triggers
!!                               |auto fertilization
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert
!!                               |application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert
!!                               |application
!!    bactkddb(:) |none          |fraction of bacteria in solution (the
!!                               |remaining fraction is sorbed to soil
!!                               |particles)
!!    bactlpdb(:) |# bact/kg frt |concentration of less persistent
!!                               |bacteria in fertilizer
!!    bactlpq(:)  |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)  |# colonies/ha |less persistent bacteria attached to soil
!!                               |particles
!!    bactpdb(:)  |# bact/kg frt |concentration of persistent bacteria in
!!                               |fertilizer
!!    bactpq(:)   |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)   |# colonies/ha |persistent bacteria attached to soil particles
!!    curyr       |none          |current year of simulation
!!    hru_dafr(:) |km**2/km**2   |fraction of watershed area in HRU
!!    icr(:)      |none          |sequence number of crop grown within the
!!                               |current year
!!    ihru        |none          |HRU number
!!    nro(:)      |none          |sequence number of year in rotation
!!    nyskip      |none          |number of years of output summarization
!!                               |and printing to skip
!!    phuacc(:)   |none          |fraction of plant heat units accumulated
!!    plantn(:)   |kg N/ha       |amount of nitrogen in plant biomass
!!    sol_aorgn(:,:)|kg N/ha     |amount of nitrogen stored in the active
!!                               |organic (humic) nitrogen pool in soil layer
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool in soil layer
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool in soil layer
!!    sol_nh3(:,:)|kg N/ha       |amount of nitrogen stored in the ammonium
!!                               |pool in soil layer
!!    sol_nly(:)  |none          |number of layers in soil profile
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the
!!                               |nitrate pool in soil layer
!!    sol_orgp(:,:)|kg P/ha      |amount of phosphorus stored in the organic
!!                               |P pool in soil layer
!!    sol_solp(:,:)|kg P/ha      |amount of phosohorus in solution
!!                               |in soil layer
!!    strsn(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |nitrogen stress
!!    strsp(:)    |none          |fraction of potential plant growth achieved on
!!                               |the day where the reduction is caused by
!!                               |phosphorus stress
!!    tnylda(:)   |kg N/kg yield |estimated/target nitrogen content of
!!                               |yield used in autofertilization
!!    wshd_fminp  |kg P/ha       |average annual amount of mineral P applied
!!                               |in watershed
!!    wshd_fnh3   |kg N/ha       |average annual amount of NH3-N applied in
!!                               |watershed
!!    wshd_fno3   |kg N/ha       |average annual amount of NO3-N applied in
!!                               |watershed
!!    wshd_forgn  |kg N/ha       |average annual amount of organic N applied
!!                               |in watershed
!!    wshd_forgp  |kg P/ha       |average annual amount of organic P applied
!!                               |in watershed
!!    wshd_ftotn  |kg N/ha       |average annual amount of N (mineral &
!!                               |organic) applied in watershed
!!    wshd_ftotp  |kg P/ha       |average annual amount of P (mineral &
!!                               |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    anano3(:)   |kg N/ha       |total amount of nitrogen applied during the
!!                               |year in auto-fertilization
!!    auton       |kg N/ha       |amount of nitrogen applied in auto-fert
!!                               |application
!!    autop       |kg P/ha       |amount of phosphorus applied in auto-fert
!!                               |application
!!    bactlpq(:)  |# colonies/ha |less persistent bacteria in soil solution
!!    bactlps(:)  |# colonies/ha |less persistent bacteria attached to soil
!!                               |particles
!!    bactpq(:)   |# colonies/ha |persistent bacteria in soil solution
!!    bactps(:)   |# colonies/ha |persistent bacteria attached to soil particles
!!    sol_aorgn(:,:)|kg N/ha     |amount of nitrogen stored in the active
!!                               |organic (humic) nitrogen pool in soil layer
!!    sol_fon(:,:)|kg N/ha       |amount of nitrogen stored in the fresh
!!                               |organic (residue) pool in soil layer
!!    sol_fop(:,:)|kg P/ha       |amount of phosphorus stored in the fresh
!!                               |organic (residue) pool in soil layer
!!    sol_nh3(:,:)|kg N/ha       |amount of nitrogen stored in the ammonium
!!                               |pool in soil layer
!!    sol_no3(:,:)|kg N/ha       |amount of nitrogen stored in the
!!                               |nitrate pool in soil layer
!!    sol_orgp(:,:)|kg P/ha      |amount of phosphorus stored in the organic
!!                               |P pool in soil layer
!!    sol_solp(:,:)|kg P/ha      |amount of phosohorus stored in solution
!!                               |in soil layer
!!    tauton(:)   |kg N/ha       |amount of N applied in autofert operation in
!!                               |year
!!    tautop(:)   |kg P/ha       |amount of P applied in autofert operation in
!!                               |year
!!    wshd_fminp  |kg P/ha       |average annual amount of mineral P applied
!!                               |in watershed
!!    wshd_fnh3   |kg N/ha       |average annual amount of NH3-N applied in
!!                               |watershed
!!    wshd_fno3   |kg N/ha       |average annual amount of NO3-N applied in
!!                               |watershed
!!    wshd_forgn  |kg N/ha       |average annual amount of organic N applied
!!                               |in watershed
!!    wshd_forgp  |kg P/ha       |average annual amount of organic P applied
!!                               |in watershed
!!    wshd_ftotn  |kg N/ha       |average annual amount of N (mineral &
!!                               |organic) applied in watershed
!!    wshd_ftotp  |kg P/ha       |average annual amount of P (mineral &
!!                               |organic) applied in watershed
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units         |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
!!    dwfert      |kg fert/ha    |amount of fertilizer to be applied to meet
!!                               |nitrogen requirement
!!    j           |none          |HRU number
!!    ly          |none          |counter (soil layers)
!!    nstress     |none          |code for approach used to determine amount
!!                               |of nitrogen to HRU
!!                               |0 nitrogen target approach
!!                               |1 annual max approach
!!    rtoaf       |none          |weighting factor used to partition the
!!                               |organic N & P content of the fertilizer
!!                               |between the fresh organic and the active
!!                               |organic pools
!!    targn       |kg N/ha       |target mineral N application
!!    tfp         |kg minP/kg frt|fraction of mineral P to be applied
!!    tpno3       |
!!    tsno3       |
!!    xx          |none          |fraction of total amount of fertilizer to
!!                               |be applied to layer
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm

      real, parameter :: rtoaf = 0.50
      integer :: j, ly, ifrt
      real :: tsno3, tpno3, dwfert, xx, targn, tfp

      j = 0
      j = ihru

      ifrt = 0
      ifrt = iafrttyp(j)

!! determine amount of mineral N to be applied
      if (strsn(j) < auto_nstrs(j)) then
        targn = 0.
        if (nstress(j) == 0) then                !! n target approach
         tsno3 = 0.
         tpno3 = 0.
         do ly = 1, sol_nly(j)
           tsno3 = tsno3 + sol_no3(ly,j) + sol_nh3(ly,j)
         end do
         tpno3 = plantn(j)

         targn = tnylda(j) - tsno3 - tpno3
         if (targn > auto_napp(j)) targn = auto_napp(j)
         if (targn < 0.) targn = 0.

         anano3(j) = anano3(j) + targn
         if (anano3(j) >= auto_nyr(j)) then
           targn = auto_nyr(j) - (anano3(j) - targn)
           if (targn < 0.) targn = 0.
           anano3(j) = auto_nyr(j)
         endif

        else                                  !! annual max approach
          targn = auto_napp(j) * (1. - phuacc(j))
          if (targn > auto_napp(j)) targn = auto_napp(j)

          anano3(j) = anano3(j) + targn
          if (anano3(j) >= auto_nyr(j)) then
            targn = auto_nyr(j) - (anano3(j) - targn)
            anano3(j) = auto_nyr(j)
          endif
        endif
        if (targn <= 1.e-6) return


!! add nutrients to soil based on nitrogen need
        dwfert = 0.
        if (fminn(ifrt) > 0.0001) then
          dwfert = targn / fminn(ifrt)
        else
!! Naresh (npai@stone-env.com) commented this line on 4/12/2016 
!! for cases where fmin(ifrt) = 0 (e.g. for elemental P fertilizer)
!! setting this to targn, further edits made around line 317
!!          dwfert = 0.
          dwfert = targn
        endif
 
        !! add bacteria to surface layer
        bactpq(j) = bactpq(j) + bactkddb(ifrt) * bactpdb(ifrt) * dwfert
        bactlpq(j) = bactlpq(j) + bactkddb(ifrt) * bactlpdb(ifrt) *     
     &                                                            dwfert
        bactps(j) = bactps(j) + (1. - bactkddb(ifrt)) * bactpdb(ifrt)*  
     &                                                           dwfert
        bactlps(j) = bactlps(j) + (1. - bactkddb(ifrt)) *bactlpdb(ifrt) 
     &                                                        * dwfert

        do ly = 1, 2
          xx = 0.
          if (ly == 1) then
            xx = afrt_surface(j)
          else
            xx = 1. - afrt_surface(j)
          endif
  
          sol_no3(ly,j) = sol_no3(ly,j) + xx * dwfert * fminn(ifrt) *   
     &                    (1. - fnh3n(ifrt))
          sol_nh3(ly,j) = sol_nh3(ly,j) + xx * dwfert * fminn(ifrt) *   
     &                    fnh3n(ifrt)

          if (cswat == 0) then
		sol_fon(ly,j) = sol_fon(ly,j) + rtoaf * xx * dwfert
     &                    * forgn(ifrt)
            sol_aorgn(ly,j) = sol_aorgn(ly,j) + (1. - rtoaf) * xx       
     &                    * dwfert * forgn(ifrt)
            sol_fop(ly,j) = sol_fop(ly,j) + rtoaf * xx * dwfert         
     &                    * forgp(ifrt)
            sol_orgp(ly,j) = sol_orgp(ly,j) + (1. - rtoaf) * xx *       
     &                    dwfert* forgp(ifrt)
	    end if
	    if (cswat == 1) then
            sol_mc(ly,j) = sol_mc(ly,j) + xx * dwfert * forgn(ifrt)*10.
            sol_mn(ly,j) = sol_mn(ly,j) + xx * dwfert * forgn(ifrt)
            sol_mp(ly,j) = sol_mp(ly,j) + xx * dwfert * forgp(ifrt)
	    end if

	    !! add by zhang
	    !!=================
	    if (cswat == 2) then
            sol_fop(ly,j) = sol_fop(ly,j) + rtoaf * xx * dwfert         
     &                    * forgp(ifrt)
            sol_orgp(ly,j) = sol_orgp(ly,j) + (1. - rtoaf) * xx *       
     &                    dwfert* forgp(ifrt)	    
            !!Allocate organic fertilizer to Slow (SWAT_active) N pool;
            sol_HSN(ly,j) = sol_HSN(ly,j) + (1. - rtoaf) * xx           
     &                    * dwfert * forgn(ifrt)
            sol_aorgn(ly,j) = sol_HSN(ly,j)

          !orgc_f is the fraction of organic carbon in fertilizer
          !for most fertilziers this value is set to 0.
              orgc_f = 0.0 
              
          !X1 is fertlizer applied to layer (kg/ha)
          !xx is fraction of fertilizer applied to layer
              X1 = xx * dwfert 
              X8 = X1 * orgc_f
              RLN = .175 *(orgc_f)/(fminn(ifrt) + forgn(ifrt) + 1.e-5)
              X10 = .85-.018*RLN
              if (X10<0.01) then
                X10 = 0.01
              else
                if (X10 > .7) then
                    X10 = .7
                end if
              end if
              XXX = X8 * X10
              sol_LMC(ly,j) = sol_LMC(ly,j) + XXX
              YY = X1 * X10
              sol_LM(ly,j) = sol_LM(ly,j) + YY
              
              ZZ = X1 *rtoaf *forgn(ifrt) * X10
              
              sol_LMN(ly,j) = sol_LMN(ly,j) + ZZ
              sol_LSN(ly,j) = sol_LSN(ly,j) + X1
     &                      *forgn(ifrt) -ZZ
              XZ = X1 *orgc_f-XXX
              sol_LSC(ly,j) = sol_LSC(ly,j) + XZ
              sol_LSLC(ly,j) = sol_LSLC(ly,j) + XZ * .175          
              sol_LSLNC(ly,j) = sol_LSLNC(ly,j) + XZ * (1.-.175) 
              YZ = X1 - YY
              sol_LS(ly,j) = sol_LS(ly,j) + YZ
              sol_LSL(ly,j) = sol_LSL(ly,j) + YZ*.175
              
              sol_fon(ly,j) = sol_LMN(ly,j) + sol_LSN(ly,j)
	    
	    end if
	    !! add by zhang
	    !!=================

          !! check for P stress
          tfp = 0.
!!       Naresh (npai@stone-env.com) edited on 4/12/2016 
!!       to handle fertilizers which have fminn(ifrt) = 0 (e.g. elemental P)
!!        if (strsp(j) <= 0.75) then
!!           tfp = fminn(ifrt) / 7.
!!        else
!!           tfp = fminp(ifrt)
!!        end if

          if (strsp(j) <= 0.75 .and. fminn(ifrt) > 0.0001) then
            tfp = fminn(ifrt) / 7. !! all other fertilizers
            autop = autop + dwfert *(tfp + forgp(ifrt))
          else if (strsp(j) <= 0.75 .and. fminn(ifrt) == 0) then
            tfp = 1/7. !! elemental P cases
            autop = autop + dwfert *(tfp + forgp(ifrt))
          else
            tfp = 0 !! no P stress, plant doesn't need any P
            autop=0
          end if
          sol_solp(ly,j) = sol_solp(ly,j) + xx * dwfert * tfp
        end do
        

!! summary calculations
          auton = auton + dwfert * (fminn(ifrt) + forgn(ifrt))
!! Naresh (npai@stone-env.com) commented this code on 4/12/2016 
!! and moved it above to handle elemental P auto-fertilization
!!        autop = autop + dwfert *(tfp + forgp(ifrt))
          tauton(j) = tauton(j) + auton
          tautop(j) = tautop(j) + autop
        if (curyr > nyskip) then
         wshd_ftotn = wshd_ftotn + dwfert * (fminn(ifrt) +              
     &               forgn(ifrt))* hru_dafr(j)
         wshd_forgn = wshd_forgn + dwfert * forgn(ifrt) * hru_dafr(j)   
         wshd_fno3 = wshd_fno3 + dwfert * fminn(ifrt) *                 
     &               (1. - fnh3n(ifrt)) * hru_dafr(j)
         wshd_fnh3 = wshd_fnh3 + dwfert * fminn(ifrt) * fnh3n(ifrt) *   
     &               hru_dafr(j)
         wshd_fminp = wshd_fminp + dwfert * tfp * hru_dafr(j)
         wshd_forgp = wshd_forgp + dwfert * forgp(ifrt) * hru_dafr(j)
        end if
        
        if (imgt == 1) then
              write (143, 1000) subnum(j), hruno(j), iyr, i_mo, iida, 
     *        hru_km(j), "         ",
     *        "AUTOFERT", phubase(j), phuacc(j), sol_sw(j),bio_ms(j), 
     *        sol_rsd(1,j),sol_sumno3(j),sol_sumsolp(j), dwfert,
     *        fertno3, fertnh3, fertorgn, fertsolp, fertorgp            
        end if
      
      endif
      

1000  format (a5,1x,a4,3i6,1x,e10.5,1x,2a15,7f10.2,20x,f10.2,10x,5f10.2)
      
      return
      end subroutine