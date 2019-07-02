
      SUBROUTINE FISHERZ (COVMAT,SIZE)
      INCLUDE 'PARAM.H'

      INTEGER G2,G3,SIZE,NODO
      REAL KSIGN
      REAL  COVMAT(MAXKLIQ,MAXKLIQ),ZABS
      DO 336 G2=1,SIZE
       DO 338 G3=1,SIZE

        IF (COVMAT(G2,G3) .GE. 0) THEN
         KSIGN=1
        ELSE 
         KSIGN=-1
        END IF

         NODO=0
        IF (COVMAT(G2,G3) .GT. 10) THEN 
         COVMAT(G2,G3) = 1
         NODO=1
        END IF
        IF (COVMAT(G2,G3) .LT. -10) THEN 
         COVMAT(G2,G3) = -1
         NODO=1
        END IF
        
        IF (NODO .EQ. 0) THEN
C        ZEXP(G2,G3)=EXP(COVMAT(G2,G3))
        ZABS=EXP(ABS(COVMAT(G2,G3)*2))
        COVMAT(G2,G3)=KSIGN*(ZABS-1)/(ZABS+1)
        END IF
C        IF (G2 .EQ. G3) THEN
C         ZMAT(G2,G3)=0.000
C        END IF
  338 CONTINUE
  336 CONTINUE
      RETURN
      END 
