        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 08 18:37:56 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHKCST__genmod
          INTERFACE 
            SUBROUTINE CHKCST(NOPT,XI,BL,BU,IBOUND)
              INTEGER(KIND=4) :: NOPT
              REAL(KIND=8) :: XI(NOPT)
              REAL(KIND=8) :: BL(NOPT)
              REAL(KIND=8) :: BU(NOPT)
              INTEGER(KIND=4) :: IBOUND
            END SUBROUTINE CHKCST
          END INTERFACE 
        END MODULE CHKCST__genmod
