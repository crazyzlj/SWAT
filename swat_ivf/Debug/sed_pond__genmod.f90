        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 08 18:38:17 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SED_POND__genmod
          INTERFACE 
            SUBROUTINE SED_POND(KK,FLW,SED)
              USE PARM
              INTEGER(KIND=4) :: KK
              REAL(KIND=4), INTENT(INOUT) :: FLW(4,0:NSTEP)
              REAL(KIND=4), INTENT(INOUT) :: SED(4,0:NSTEP)
            END SUBROUTINE SED_POND
          END INTERFACE 
        END MODULE SED_POND__genmod
