      subroutine water_hru
            
!!    ~ ~ ~ PURPOSE ~ ~ ~
!!    
!!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 

!!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ LOCAL DEFINITIONS ~ ~ ~
!!    name        |units       |definition
!!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ 
!!    ~ ~ ~ SUBROUTINES/FUNCTIONS CALLED ~ ~ ~
!!    ~ ~ ~ ~ ~ ~ END SPECIFICATIONS ~ ~ ~ ~ ~ ~

      use parm
	j = ihru
!! if the HRU is water compute only pet and et
!! using Priestly-Taylor and a coefficient
        tmpk = 0.
        d = 0.
        gma = 0.
        ho = 0.
        albday = .08
        pet_alpha = 1.28
        tmpk = tmpav(j) + 273.15
        d = Exp(21.255 - 5304. / tmpk) * 5304. / tmpk ** 2
        gma = d / (d + .68)
        ho = hru_ra(j) * (1. - albday) / 2.44
        pet_day = pet_alpha * ho * gma
        etday = .7 * pet_day
      
      return
      end