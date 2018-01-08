        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 08 18:38:06 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE VBL__genmod
          INTERFACE 
            SUBROUTINE VBL(EVX,SPX,PP,QIN,OX,VX1,VY,YI,YO,YSX,VF,VYF,AHA&
     &)
              REAL(KIND=4), INTENT(IN) :: EVX
              REAL(KIND=4), INTENT(IN) :: SPX
              REAL(KIND=4), INTENT(IN) :: PP
              REAL(KIND=4), INTENT(IN) :: QIN
              REAL(KIND=4), INTENT(IN) :: OX
              REAL(KIND=4), INTENT(INOUT) :: VX1
              REAL(KIND=4), INTENT(INOUT) :: VY
              REAL(KIND=4), INTENT(IN) :: YI
              REAL(KIND=4), INTENT(IN) :: YO
              REAL(KIND=4), INTENT(IN) :: YSX
              REAL(KIND=4), INTENT(IN) :: VF
              REAL(KIND=4), INTENT(IN) :: VYF
              REAL(KIND=4), INTENT(IN) :: AHA
            END SUBROUTINE VBL
          END INTERFACE 
        END MODULE VBL__genmod
