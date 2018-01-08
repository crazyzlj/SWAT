        !COMPILER-GENERATED INTERFACE MODULE: Mon Jan 08 18:38:35 2018
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RI_POND__genmod
          INTERFACE 
            SUBROUTINE RI_POND(KK,RIFLW,RISED)
              USE PARM
              INTEGER(KIND=4) :: KK
              REAL(KIND=4), INTENT(INOUT) :: RIFLW(4,0:NSTEP)
              REAL(KIND=4), INTENT(INOUT) :: RISED(4,0:NSTEP)
            END SUBROUTINE RI_POND
          END INTERFACE 
        END MODULE RI_POND__genmod
