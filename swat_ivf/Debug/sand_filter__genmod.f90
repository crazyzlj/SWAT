        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 08 18:38:26 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SAND_FILTER__genmod
          INTERFACE 
            SUBROUTINE SAND_FILTER(KK,FLW,SED)
              USE PARM
              INTEGER(KIND=4), INTENT(IN) :: KK
              REAL(KIND=4), INTENT(INOUT) :: FLW(4,0:NSTEP)
              REAL(KIND=4), INTENT(INOUT) :: SED(4,0:NSTEP)
            END SUBROUTINE SAND_FILTER
          END INTERFACE 
        END MODULE SAND_FILTER__genmod
