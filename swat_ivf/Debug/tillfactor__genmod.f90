        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 08 18:38:26 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TILLFACTOR__genmod
          INTERFACE 
            SUBROUTINE TILLFACTOR(JJ,BMIX,EMIX,DTIL,SOL_THICK)
              INTEGER(KIND=4), INTENT(IN) :: JJ
              REAL(KIND=4), INTENT(IN) :: BMIX
              REAL(KIND=4) :: EMIX
              REAL(KIND=4) :: DTIL
              REAL(KIND=4) :: SOL_THICK(SOL_NLY((JJ)))
            END SUBROUTINE TILLFACTOR
          END INTERFACE 
        END MODULE TILLFACTOR__genmod
