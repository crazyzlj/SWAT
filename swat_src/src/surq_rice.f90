subroutine surq_rice

    !!    ~ ~ ~ INCOMING VARIABLES ~ ~ ~
    !!    name        |units         |definition
    !!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    !!    precipday   |mm H2O        |precipitation for the day in HRU

    !!    ~ ~ ~ OUTGOING VARIABLES ~ ~ ~
    !!    name           |units         |definition
    !!    ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    !!    potsedo        |metric tons   |sediment leaving pothole on day
    !!    pot_vol(:)     |mm            |current volume of water stored in the
    !!                                  |depression/impounded area

    pot_vol(j) = pot_vol(j) + precipday

    !       if overflow, then send the overflow to the HRU surface flow
    if (pot_vol(j) > pot_volxmm(j)) then
        qdr(j) = qdr(j) + (pot_vol(j)- pot_volxmm(j))
        !          qday = qday + (pot_vol(j)- pot_volxmm(j))
        spillo = pot_vol(j) - pot_volxmm(j)
        pot_vol(j) = pot_volxmm(j)
        xx = spillo / (spillo + pot_volxmm(j))
        potsedo = potsedo + pot_sed(j) * xx
        potsano = potsano + pot_san(j) * xx
        potsilo = potsilo + pot_sil(j) * xx
        potclao = potclao + pot_cla(j) * xx
        potsago = potsago + pot_sag(j) * xx
        potlago = potlago + pot_lag(j) * xx
        potno3o = potno3o + pot_no3(j) * xx
        potsolpo = potsolpo + pot_solp(j) * xx
        potorgno = potorgno + pot_orgn(j) * xx
        potorgpo = potorgpo + pot_orgp(j) * xx
        potmpso = potmpso + pot_mps(j) * xx
        potmpao = potmpao + pot_mpa(j) * xx

        pot_sed(j) = pot_sed(j) - potsedo
        pot_san(j) = pot_san(j) - potsano
        pot_sil(j) = pot_sil(j) - potsilo
        pot_cla(j) = pot_cla(j) - potclao
        pot_sag(j) = pot_sag(j) - potsago
        pot_lag(j) = pot_lag(j) - potlago

        pot_no3(j) = pot_no3(j) - potno3o
        pot_solp(j) = pot_solp(j) - potsolpo
        pot_orgn(j) = pot_orgn(j) - potorgno
        pot_orgp(j) = pot_orgp(j) - potorgpo
        pot_mps(j) = pot_mps(j) - potmpso
        pot_mpa(j) = pot_mpa(j) - potmpao

        sedyld(j) = sedyld(j) + potsedo
        sanyld(j) = sanyld(j) + potsano
        silyld(j) = silyld(j) + potsilo
        clayld(j) = clayld(j) + potclao
        ! pot_sag(j) = sagyld(j) + potsago ! fixed by lj
        sagyld(j) = sagyld(j) + potsago
        lagyld(j) = lagyld(j) + potlago

        surqno3(j) = surqno3(j) + potno3o
        surqsolp(j) = surqsolp(j) + potsolpo
        sedorgn(j) = sedorgn(j) + potorgno
        sedorgp(j) = sedorgp(j) + potorgpo
        sedminps(j) = sedminps(j) + potmpso
        sedminpa(j) = sedminpa(j) + potmpao
    end if       !! if overflow

    !      If no overflow, compute settling and losses, surface inlet tile
    !      flow, evap, seepage, and redistribute soil water
    if (pot_vol(j) > 1.e-6) then


    endif


end subroutine surq_rice