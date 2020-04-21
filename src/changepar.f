      subroutine changepar(xxo,nopt, iiname, inrhru,iinr, imet)
        use parm
        real*8 xxo(nopt), parlim(2)
        integer nopt
        integer iiname(nopt), iinr(nopt), inrhru(nopt,2000) 
	  integer imet(nopt)
        character*1 tt




c     ~ ~ ~ PURPOSE ~ ~ ~
c     changing the parameters values for autocalibration
c     ~ ~ ~ COMMON BLOCKS ~ ~ ~
c
c     ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    nopt        |none          |number of parameters to optimise
!!    xxo(:)       |none          |new values of the parameters 
!!    iiname(:)    |none          |code refering to which parameter to change, and how
!!    iinr(:)      !              |number of HRUs to change for parameter (:)
!!    inrhru(:,:)  |none          |list of HRU numbers to change  

       write(*,*) 'changing parameters'
      do iopt= 1,nopt

	 iimet=imet(iopt)
	  parlim(1)=0.001
        parlim(2)=1.
cc general parameters
       if (iinr(iopt).eq.0) then
        
        select case ( iiname(iopt))
        case(1)
        call chngp(ai0, xxo(iopt), iimet, parlim)
        case(2)
        call chngp(ai1, xxo(iopt), iimet, parlim)
        case(3)
        call chngp(ai2, xxo(iopt), iimet, parlim)
        case(4)
        call chngp(ai3, xxo(iopt), iimet, parlim)
        case(5)
        call chngp(ai4, xxo(iopt), iimet, parlim)
        case(6)	
        call chngp(ai5, xxo(iopt), iimet, parlim)
        case(7)
        call chngp(ai6, xxo(iopt), iimet, parlim)
        case(8)
        call chngp(mumax, xxo(iopt), iimet, parlim)
        case(9)
        call chngp(rhoq, xxo(iopt), iimet, parlim)
        case(10)
        call chngp(tfac, xxo(iopt), iimet, parlim)
        case(11)
        call chngp(k_l, xxo(iopt), iimet, parlim)
        case(12)
        call chngp(k_n, xxo(iopt), iimet, parlim)
        case(13)
        call chngp(k_p, xxo(iopt), iimet, parlim)
        case(14)
        call chngp(lambda0, xxo(iopt), iimet, parlim)
        case(15)
        call chngp(lambda1, xxo(iopt), iimet, parlim)
        case(16)
        call chngp(lambda2, xxo(iopt), iimet, parlim)
        case(17)
        call chngp(p_n, xxo(iopt), iimet, parlim)
        case(26)

        case(27)


        case(30)
	  parlim(1)=0.
	  parlim(2)=0.01
        call chngp(spcon, xxo(iopt), iimet, parlim)
        case(31)
	  parlim(1)=1.
	  parlim(2)=1.5
        call chngp(spexp, xxo(iopt), iimet, parlim)
        case(32)

        case(33)
	  parlim(1)=0.
	  parlim(2)=4000.
        call chngp(surlag, xxo(iopt), iimet, parlim)
        case(34)
	  parlim(1)=0.
	  parlim(2)=10.
        call chngp(smfmx, xxo(iopt), iimet, parlim)
        case(35)
	  parlim(1)=0.
	  parlim(2)=10.
        call chngp(smfmn, xxo(iopt), iimet, parlim)
        case(36)
	  parlim(1)=0.
	  parlim(2)=5.
        call chngp(SFTMP, xxo(iopt), iimet, parlim)
        case(37)
	  parlim(1)=0.
	  parlim(2)=5.
        call chngp(SMTMP, xxo(iopt), iimet, parlim)
        case(38)
	  parlim(1)=0.01
	  parlim(2)=1.
        call chngp(TIMP, xxo(iopt), iimet, parlim)
        case(39)
        call chngp(SNOCOVMX, xxo(iopt), iimet, parlim)
        case(40)
        call chngp(SNO50COV, xxo(iopt), iimet, parlim)
        call ascrv(.5,.95,sno50cov,.95,snocov1,snocov2)
        case(41)
	  parlim(1)=0.0
	  parlim(2)=1.
	  call chngp(nperco, xxo(iopt), iimet, parlim)
        case(42)
	  parlim(1)=10.
	  parlim(2)=17.
	  call chngp(pperco, xxo(iopt), iimet, parlim)
        case(43)
	  parlim(1)=100.
	  parlim(2)=200.
	  call chngp(phoskd, xxo(iopt), iimet, parlim)

       case(44)
	  parlim(1)=0.0
	  parlim(2)=1.
	  call chngp(msk_co1, xxo(iopt), iimet, parlim)
	  msk_co2 = 1. - msk_co1

       case(45)
	  parlim(1)=0.0
	  parlim(2)=0.5
	  call chngp(msk_x, xxo(iopt), iimet, parlim)
        end select
        else


        do iii=1,iinr(iopt)
        if (iinr(iopt).le.2000)then
        ilu=inrhru(iopt,iii)
        else
        ilu=iii
	  
        if (ilu.gt.mhru.and.iiname(iopt).lt.30) go to 20
	  if (ilu.gt.msub.and.iiname(iopt).ge.30.and.iiname(iopt).lt.60) 
     *	  go to 20 
	  if(ilu.gt.mcrdb.and.iiname(iopt).ge.60) go to 20
        end if

        if (iiname(iopt).ge.100) go to 100

	select case (iiname(iopt))
cc      HRU parameters
        case(1)
        parlim(1)=0.001
        parlim(2)=1.
        call chngp(alpha_bf(ilu), xxo(iopt), iimet, parlim)
        alpha_bfe(ilu) = exp(-alpha_bf(ilu))
        case(2)
	  parlim(1)=0.001
	  parlim(2)=365.
        call chngp(delay(ilu), xxo(iopt), iimet, parlim)
	  if(delay(ilu) < .1) delay(ilu) = .1
        gw_delaye(ilu) = Exp(-1./(delay(ilu) + 1.e-6))
        case(3)
	  parlim(1)=0.02
	  parlim(2)=0.2
        call chngp(gw_revap(ilu), xxo(iopt), iimet, parlim)
        case(4)
	  parlim(1)=0.001
        parlim(2)=1.
        call chngp(rchrg_dp(ilu), xxo(iopt), iimet, parlim)
        case(5)
		  parlim(1)=0.001
	  parlim(2)=500.
        call chngp(revapmn(ilu), xxo(iopt), iimet, parlim)
        case(6)
	  parlim(1)=0.001
	  parlim(2)=5000.
	  call chngp(gwqmn(ilu), xxo(iopt), iimet, parlim)
        case(7)
        parlim(1)=0.001
	  parlim(2)=10.
        call chngp(canmx(ilu), xxo(iopt), iimet, parlim)
        case(8)
        parlim(1)=0.001
	  parlim(2)=100.
         call chngp(gwno3(ilu), xxo(iopt), iimet, parlim)
        case(9)
        case(10)
	  parlim(1)=30.
        parlim(2)=98.
	  call chngp(cn2(ilu), xxo(iopt), iimet, parlim)

        case(11)
 
        case(15)
        parlim(1)=0.001
	  parlim(2)=500.
        do ii=1,sol_nly(ilu)
	  call chngp(sol_k(ii, ilu), xxo(iopt), iimet, parlim)
        end do
        case(16)
	  parlim(1)=1.
	  parlim(2)=5000.
        do ii=1,sol_nly(ilu)
	    call chngp(sol_z(ii, ilu), xxo(iopt), iimet, parlim)
        end do

       case (17)
        parlim(1)=0.001
	  parlim(2)=1.
       do ii=1,sol_nly(ilu)
	   call chngp(sol_awc(ii, ilu), xxo(iopt), iimet, parlim)
       end do
       case (18)
	  parlim(1)=0.
	  parlim(2)=100.
	    do ii=1,sol_nly(ilu)
	      call chngp(sol_solp(ii, ilu), xxo(iopt), iimet, parlim)
        end do

       case (19)
	  parlim(1)=0.
	  parlim(2)=10000.
        do ii=1,sol_nly(ilu)

	 call chngp(sol_orgn(ii, ilu), xxo(iopt), iimet, parlim)
        end do
       case (20)
		  parlim(1)=0.
	  parlim(2)=4000.
	      do ii=1,sol_nly(ilu)
	  call chngp(sol_orgp(ii, ilu), xxo(iopt), iimet, parlim)
        end do
       case (21)
        parlim(1)=0.
	  parlim(2)=5.
	   do ii=1,sol_nly(ilu)
	  call chngp(sol_no3(ii, ilu), xxo(iopt), iimet, parlim)
        end do
	case(22)
	  parlim(1)=0.
	  parlim(2)=1.
	  call chngp(sol_alb(ilu), xxo(iopt), iimet, parlim)
	case(23)
	  parlim(1)=0.00001
	  parlim(2)=0.6
	  call chngp(hru_slp(ilu), xxo(iopt), iimet, parlim)
	  xm = .6 * (1. - Exp(-35.835 * hru_slp(ilu)))

        if (slsubbsn(ilu) <= 0.) slsubbsn(ilu) = 0.0
        sin_sl = Sin(Atan(hru_slp(ilu)))
        usle_ls(ilu) = (slsubbsn(ilu)/22.128)**xm * (65.41 * sin_sl *     
     *                sin_sl + 4.56 * sin_sl + .065)
	case(24)
	  parlim(1)=10.
	  parlim(2)=150.
	  call chngp(slsubbsn(ilu), xxo(iopt), iimet, parlim)
        sin_sl = Sin(Atan(hru_slp(ilu)))
	  usle_ls(ilu) = (slsubbsn(ilu)/22.128)**xm * (65.41 * sin_sl *     
     *                sin_sl + 4.56 * sin_sl + .065)
	
	case(25)
	  parlim(1)=0.
	  parlim(2)=1.
	  call chngp(biomix(ilu), xxo(iopt), iimet, parlim)

      case(26)
	  parlim(1)=0.1 
	  parlim(2)=1.
	  call chngp(usle_p(ilu), xxo(iopt), iimet, parlim)

	case(27)
	  parlim(1)=0. 
	  parlim(2)=1.
	  call chngp(esco(ilu), xxo(iopt), iimet, parlim)

	case(28)
	  parlim(1)=0.1 
	  parlim(2)=1.
	  call chngp(epco(ilu), xxo(iopt), iimet, parlim)

cc      subbasin parameters
       case(30)

       case(31)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rs1(ilu) , xxo(iopt), iimet, parlim)

       case(32)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rs2(ilu), xxo(iopt), iimet, parlim)

       case(33)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rs3(ilu) , xxo(iopt), iimet, parlim)

       case(34)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rs4(ilu) , xxo(iopt), iimet, parlim)

       case(35)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rs5(ilu), xxo(iopt), iimet, parlim)

       case(36)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rk1(ilu) , xxo(iopt), iimet, parlim)

       case(37)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rk2(ilu), xxo(iopt), iimet, parlim)

       case(38)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rk3(ilu), xxo(iopt), iimet, parlim)

       case(39)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rk4(ilu), xxo(iopt), iimet, parlim)

       case(40)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rk5(ilu), xxo(iopt), iimet, parlim)

        case(41)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(rk6(ilu), xxo(iopt), iimet, parlim)

        case(43)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(bc1(ilu), xxo(iopt), iimet, parlim)

       case(44)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(bc2(ilu), xxo(iopt), iimet, parlim)

      case(45)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(bc3(ilu), xxo(iopt), iimet, parlim)

      case(46)
	  parlim(1)=0.001 
	  parlim(2)=1.
	  call chngp(bc4(ilu), xxo(iopt), iimet, parlim)

      case(47)
      case(48)


      case(50)
	  parlim(1)=-0.05
	  parlim(2)=1.
	  call chngp(ch_cov1(ilu), xxo(iopt), iimet, parlim)

      case(51)
        parlim(1)=0.001 
	  parlim(2)= .5
	  call chngp(ch_n(2,ilu), xxo(iopt), iimet, parlim)

	case(52)
	  parlim(1)=0. 
	  parlim(2)= 50.
	  call chngp(tlaps(ilu), xxo(iopt), iimet, parlim)

	case(53)
	  parlim(1)=-0.001 
	  parlim(2)=1.
	  call chngp(ch_cov2(ilu), xxo(iopt), iimet, parlim)

	case(54)
	  parlim(1)=-0.01 
	  parlim(2)=150.
	  call chngp(ch_k(2,ilu), xxo(iopt), iimet, parlim)

	case(55)
	  parlim(1)=0.
	  parlim(2)=365.
	  call chngp(hlife_ngw, xxo(iopt), iimet, parlim)
        if (hlife_ngw <= 0.) hlife_ngw = 365. 
        gw_nloss(ilu) = Exp(-.693 / hlife_ngw)
cc      crop parameters

	case(60)
	  parlim(1)=0.001
	  parlim(2)=0.5
	  usle_c=10.**cvm(ilu)
	  call chngp(usle_c, xxo(iopt), iimet, parlim)
	  cvm(ilu)=log(usle_c)
	
	case(61)
	  parlim(1)=0.001 
	  parlim(2)=1.
        call chngp(blai(ilu), xxo(iopt), iimet, parlim)
cc	pesticide parameters
	
      case(62)
	  parlim(1)=0.
	  parlim(2)=1000.
        call chngp(skoc(ilu), xxo(iopt), iimet, parlim)
	case(63)
	  parlim(1)=0.
	  parlim(2)=1.
        call chngp(pst_wof(ilu), xxo(iopt), iimet, parlim)
	case(64)
	  parlim(1)=0.
	  parlim(2)=50.
        call chngp(hlife_f(ilu), xxo(iopt), iimet, parlim)
        if (hlife_f(ilu) > 0.) then
          decay_f(ilu) = Exp(-.693/hlife_f(ilu))
        else
          decay_f(ilu) = 0.
        endif
	case(65)
	  parlim(1)=0.
	  parlim(2)=1000.
        call chngp(hlife_s(ilu), xxo(iopt), iimet, parlim)
        if (hlife_s(ilu) > 0.) then
          decay_s(ilu) = Exp(-.693/hlife_s(ilu))
        else
          decay_s(ilu) = 0.
        endif
	case(66)
	  parlim(1)=0.
	  parlim(2)=1.
        call chngp(ap_ef(ilu), xxo(iopt), iimet, parlim)
	case(67)
	  parlim(1)=0.
	  parlim(2)=1000.
        call chngp(pst_wsol(ilu), xxo(iopt), iimet, parlim)
        if (ilu == irtpest) then
          pest_sol = pst_wsol(ilu) * 1000.
        end if
      case(68)
	  parlim(1)=0.
	  parlim(2)=1000.
        call chngp(prf, xxo(iopt), iimet, parlim)
      case(69)
	  parlim(1)=0.
	  parlim(2)=1000.
        call chngp(psp, xxo(iopt), iimet, parlim)
        do j = 1, sol_nly(ilu)
          sol_actp(j,ilu) = sol_solp(j,ilu) * (1. - psp) / psp
        enddo  
      end select

	


	go to 120
100   continue
      nlayer =iiname(iopt)/100
      ipar=iiname(iopt)-nlayer*100
      select case (ipar)

      case (1)
	  call chngp(sol_k(nlayer, ilu) , xxo(iopt), iimet, parlim)
      case (2)
	  call chngp(sol_z(nlayer, ilu), xxo(iopt), iimet, parlim)

      case (3)
	  call chngp(sol_awc(nlayer, ilu), xxo(iopt), iimet, parlim)

	  if (sol_awc(nlayer, ilu).gt.1.) sol_awc(nlayer, ilu)=1.
      end select
120   continue	
       end do
 20   continue
	 end if
cc   end subbasin parameters
c	other calculations
   
	end do


 6000  format(6x, a13,1x, a13,1x, a13,1x, a13)
 6001  format(6x, a13,1x, a13,1x, a13)
       return
      end


ccccccccccccccccccccccccccccc
	subroutine chngp(parval, change, iimet, parlim)
	real*8 change, parlim(2)

	select case (iimet)
	case (1)
      parval=change
	case (2)
      parval=parval+change
      case (3)
	parval=parval *(1+change/100.)
	end select
	parval=min(parval, parlim(2))
	parval=max(parval, parlim(1))
	return
	end
