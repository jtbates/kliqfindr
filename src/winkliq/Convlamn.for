       SUBROUTINE CONVLAMN(PL,M2X,MX2,TOTAL,
     C ALPHA,BETA,GAMMA,DELTA,XROOT,LE)

       REAL PL,ALPHA,BETA,GAMMA,DELTA,LE,
     C QA,QB,QC,INROOT,ROOT,TMX2,THETA1,THETA2,
     C LTHETA1,LTHETA2,DELTA1,DELTA2,GAMMA1,GAMMA2,
     C BETA1,BETA2,ALPHA1,ALPHA2,MAXD

       INTEGER M2X,MX2,TOTAL,MX1,XROOT,NEGO1,NEGO2

       IF ((M2X .GT. 0) .AND. (MX2 .GT. 0) .AND. (TOTAL .GT. 0)) THEN
       ROOT=-XROOT
       MX1=TOTAL-MX2
       LE=2.718281828**(2*PL)
       QA=LE - 1
       QB=-(LE*M2X+LE*MX2+MX1-M2X)
       QC=LE*M2X*MX2
       INROOT=QB**2-4*QA*QC

       IF ((QB .GT. -9999999) .AND. (QA .GT. -9999999). AND. 
     C (QC .GT. -9999999) .AND. (INROOT .GT. 0)) THEN
       DELTA1=(-QB+SQRT(INROOT))/(2.0000*QA)

       BETA1=MX2-DELTA1
       GAMMA1=M2X-DELTA1
       ALPHA1=MX1-GAMMA1
       THETA1=DELTA1*ALPHA1/(BETA1*GAMMA1)

       DELTA2=(-QB-SQRT(QB**2-4*QA*QC))/(2*QA)
       BETA2=MX2-DELTA2
       GAMMA2=M2X-DELTA2
       ALPHA2=MX1-GAMMA2
       THETA2=DELTA2*ALPHA2/(BETA2*GAMMA2)

       DELTA=DELTA1
       ALPHA=ALPHA1
       BETA=BETA1
       GAMMA=GAMMA1
       LTHETA1=-99999
       IF (THETA1 .GT. 0) THEN
       LTHETA1=LOG(THETA1)/2.00
       END IF
       LTHETA2=-99999

       IF (THETA2 .GT. 0.001) THEN
       LTHETA2=LOG(THETA2)/2.00
       END IF
       TMX2=MX2
       IF (TMX2 .GT. M2X) THEN
       TMX2=M2X
       END IF

       TMX2=TMX2-.05

       NEGO1=0
        IF ((DELTA .LT. 0) .OR. (ALPHA .LT. 0) .OR.
     C (BETA .LT. 0) .OR. (GAMMA .LT. 0)) THEN
       NEGO1=1
       END IF

       NEGO2=0
        IF ((DELTA2 .LT. 0) .OR. (ALPHA2 .LT. 0) .OR.
     C (BETA2 .LT. 0) .OR. (GAMMA2 .LT. 0)) THEN
       NEGO2=1
       END IF
       IF ((DELTA1 .GT. TMX2) .OR.
     C (ABS(LTHETA1-PL) .GT. ABS(LTHETA2-PL)) .OR.
     C ((NEGO1 .EQ. 1) .AND. (NEGO2 .EQ. 0))) THEN 
       DELTA=DELTA2
       ALPHA=ALPHA2
       BETA=BETA2
       GAMMA=GAMMA2
       END IF
        IF ((NEGO1 .EQ. 1) .AND. (NEGO2 .EQ. 1)) THEN 
        DELTA=TMX2-1.0
       BETA=MX2-DELTA
       GAMMA=M2X-DELTA
       ALPHA=MX1-GAMMA
       THETA=DELTA*ALPHA/(BETA*GAMMA)
       END IF
       END IF

C       INROOT=0

       IF ((INROOT .LE. 0) .OR. (DELTA .LT. 0)) THEN

       DELTA=MX2*M2X/TOTAL
       BETA=MX2-DELTA
       GAMMA=M2X-DELTA
       ALPHA=MX1-GAMMA
       END IF       


       END IF

       IF (DELTA .LT. 0) THEN 
         DELTA=0
       END IF
        MAXD=999999
       IF (DELTA .GT. MAXD) THEN 
         DELTA=MAXD
       END IF
       
       IF (ALPHA .LT. 0) THEN 
         ALPHA=0
       END IF

       IF (ALPHA .GT. MAXD) THEN 
         ALPHA=MAXD
       END IF

       IF (BETA .LT. 0) THEN 
         BETA=0
       END IF

       IF (BETA .GT. MAXD) THEN 
         BETA=MAXD
       END IF

       IF (GAMMA .LT. 0) THEN 
         GAMMA=0
       END IF
        MAXD=999999
       IF (GAMMA .GT. MAXD) THEN 
         GAMMA=MAXD
       END IF
       
       RETURN
       END
